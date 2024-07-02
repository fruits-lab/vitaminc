#include "llvm_ir_generator.hpp"

#include <llvm/ADT/ArrayRef.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/Support/Casting.h>

#include <cassert>
#include <cstddef>
#include <map>
#include <memory>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "ast.hpp"
#include "operator.hpp"
#include "type.hpp"

namespace {

/// @throw `std::runtime_error` if there's unrecognize binary operator
llvm::Instruction::BinaryOps GetBinaryOperator(BinaryOperator op) {
  switch (op) {
    case BinaryOperator::kAdd:
      return llvm::BinaryOperator::Add;
    case BinaryOperator::kSub:
      return llvm::BinaryOperator::Sub;
    case BinaryOperator::kMul:
      return llvm::BinaryOperator::Mul;
    case BinaryOperator::kDiv:
      return llvm::BinaryOperator::SDiv;
    case BinaryOperator::kMod:
      return llvm::BinaryOperator::SRem;
    case BinaryOperator::kAnd:
      return llvm::BinaryOperator::And;
    case BinaryOperator::kXor:
      return llvm::BinaryOperator::Xor;
    case BinaryOperator::kOr:
      return llvm::BinaryOperator::Or;
    case BinaryOperator::kShl:
      return llvm::BinaryOperator::Shl;
    // NOTE: Arithmetic shift right (AShr) is akin to dividing by a power of two
    // for non-negative numbers. For negatives, it's implementation-defined, so
    // we opt for arithmetic shifting.
    case BinaryOperator::kShr:
      return llvm::BinaryOperator::AShr;
    default:
      throw std::runtime_error{"unrecognize binary operator!"};
  }
}

/// @note Comparison operators are not categorized as operator in LLVM. Thus, we
/// have this helper function to get the predicate of our comparison operator.
llvm::CmpInst::Predicate GetCmpPredicate(BinaryOperator op) {
  switch (op) {
    case BinaryOperator::kGt:
      return llvm::CmpInst::Predicate::ICMP_SGT;
    case BinaryOperator::kGte:
      return llvm::CmpInst::Predicate::ICMP_SGE;
    case BinaryOperator::kLt:
      return llvm::CmpInst::Predicate::ICMP_SLT;
    case BinaryOperator::kLte:
      return llvm::CmpInst::Predicate::ICMP_SLE;
    case BinaryOperator::kEq:
      return llvm::CmpInst::Predicate::ICMP_EQ;
    case BinaryOperator::kNeq:
      return llvm::CmpInst::Predicate::ICMP_NE;
    default:
      return llvm::CmpInst::Predicate::BAD_ICMP_PREDICATE;
  }
}

bool IsCmpInst(BinaryOperator op) {
  switch (op) {
    case BinaryOperator::kGt:
    case BinaryOperator::kGte:
    case BinaryOperator::kLt:
    case BinaryOperator::kLte:
    case BinaryOperator::kEq:
    case BinaryOperator::kNeq:
      return true;
    default:
      return false;
  }
}

auto id_to_val  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables):
                // Accessible only within this translation unit; declaring as a
                // data member introduces unnecessary dependency.
    = std::map<std::string, llvm::Value*>{};

auto
    val_to_id_addr  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables):
                    // Accessible only within this translation unit; declaring
                    // as a data member introduces unnecessary dependency.
    = std::map<llvm::Value*, llvm::Value*>{};

/// @brief Every expression generates a LLVM object that is the subclass of
/// `llvm::Value`. This object is stored, so we can propagate to later use.
class PrevValueRecorder {
 public:
  void Record(llvm::Value* val) {
    prev_val_ = val;
  }

  llvm::Value* ValOfPrevExpr() {
    assert(prev_val_);
    return prev_val_;
  }

 private:
  llvm::Value* prev_val_ = nullptr;
};

auto
    val_recorder  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables):
                  // Accessible only within this translation unit; declaring as
                  // a data member introduces unnecessary dependency.
    = PrevValueRecorder{};

struct LabelViewInfo {
  llvm::BasicBlock* entry;
  llvm::BasicBlock* exit;
  /// @brief This vector stores every `case` and `default` basic blocks of
  /// a switch case.
  /// This first element of a pair is the expression value
  /// of a case statement.
  /// This second element of a pair is the label's basic block.
  std::vector<std::pair<llvm::Value*, llvm::BasicBlock*>> cases{};
};

/// @note Blocks that allows jumping within or out of it should add its labels
/// to this list.
auto
    label_views_of_jumpable_blocks  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)
    = std::vector<LabelViewInfo>{};

}  // namespace

void LLVMIRGenerator::Visit(const DeclStmtNode& decl_stmt) {
  for (const auto& decl : decl_stmt.decls) {
    decl->Accept(*this);
  }
}

void LLVMIRGenerator::Visit(const VarDeclNode& decl) {
  auto var_type = llvm_util_.GetLLVMType(*(decl.type));
  // For function pointer, we need to change from FunctionType to PointerType
  var_type = var_type->isFunctionTy() ? var_type->getPointerTo() : var_type;
  auto addr = builder_->CreateAlloca(var_type);
  if (decl.init) {
    decl.init->Accept(*this);
    auto val = val_recorder.ValOfPrevExpr();
    builder_->CreateStore(val, addr);
  }
  id_to_val[decl.id] = addr;
}

void LLVMIRGenerator::Visit(const ArrDeclNode& arr_decl) {
  auto arr_type = llvm_util_.GetLLVMType(*(arr_decl.type));
  auto base_addr = builder_->CreateAlloca(arr_type, nullptr);
  id_to_val[arr_decl.id] = base_addr;

  auto arr_decl_type = dynamic_cast<ArrType*>(arr_decl.type.get());
  for (auto i = std::size_t{0}, e = arr_decl_type->len(); i < e; ++i) {
    if (i < arr_decl.init_list.size()) {
      auto& arr_init = arr_decl.init_list.at(i);
      arr_init->Accept(*this);
    }

    auto res_addr =
        builder_->CreateConstInBoundsGEP2_32(arr_type, base_addr, 0, i);

    if (i < arr_decl.init_list.size()) {
      auto init_val = val_recorder.ValOfPrevExpr();
      builder_->CreateStore(init_val, res_addr);
    } else {
      // set remaining elements as 0
      auto zero = llvm::ConstantInt::get(llvm_util_.IntType(), 0, true);
      builder_->CreateStore(zero, res_addr);
    }
  }
}

void LLVMIRGenerator::Visit(const RecordDeclNode& record_decl) {
  /* Do nothing because this node only declares a type. */
}

void LLVMIRGenerator::Visit(const FieldNode& field) {
  /* Do nothing because this node only declares a member type in a record. */
}

void LLVMIRGenerator::Visit(const RecordVarDeclNode& record_var_decl) {
  auto* record_type = dynamic_cast<RecordType*>(record_var_decl.type.get());
  assert(record_type);
  auto type = llvm_util_.GetLLVMType(*(record_var_decl.type));
  auto base_addr = builder_->CreateAlloca(type, nullptr);
  id_to_val[record_var_decl.id] = base_addr;

  // NOTE: This predicate will make sure that we don't initialize members that
  // exceed the total number of members in a record. Also, it gurantees
  // that accessing element in the initializers will not go out of bound.
  for (auto i = std::size_t{0}, e = record_var_decl.inits.size(),
            slot_count = record_type->SlotCount();
       i < slot_count && i < e; ++i) {
    auto& init = record_var_decl.inits.at(i);
    init->Accept(*this);
    auto init_val = val_recorder.ValOfPrevExpr();

    auto res_addr = builder_->CreateStructGEP(type, base_addr, i);
    builder_->CreateStore(init_val, res_addr);
  }
}

void LLVMIRGenerator::Visit(const ParamNode& parameter) {
  /* Do nothing */
}

void LLVMIRGenerator::Visit(const FuncDefNode& func_def) {
  // Explicit cast to llvm::FunctionType to avoid compiler error.
  auto func_type = llvm::dyn_cast<llvm::FunctionType>(
      llvm_util_.GetLLVMType(*(func_def.type)));
  auto func = llvm::Function::Create(func_type,
                                     func_def.id == "main"
                                         ? llvm::Function::ExternalLinkage
                                         : llvm::Function::InternalLinkage,
                                     func_def.id, *module_);

  auto body = llvm::BasicBlock::Create(*context_, "body", func);
  builder_->SetInsertPoint(body);
  // Allocate space for parameters.
  auto args_iter = func->arg_begin();
  for (auto& parameter : func_def.parameters) {
    parameter->Accept(*this);
    args_iter->setName(parameter->id);

    llvm::Type* param_type = llvm_util_.GetLLVMType(*(parameter->type));
    // Update type from FunctionType to PointerType for function pointer.
    if (param_type->isFunctionTy()) {
      param_type = param_type->getPointerTo();
    }
    args_iter->mutateType(param_type);
    auto addr = builder_->CreateAlloca(param_type);
    builder_->CreateStore(args_iter, addr);
    id_to_val[parameter->id] = addr;
    ++args_iter;
  }

  func_def.body->Accept(*this);
}

void LLVMIRGenerator::Visit(const LoopInitNode& loop_init) {
  std::visit([this](auto&& clause) { clause->Accept(*this); },
             loop_init.clause);
}

void LLVMIRGenerator::Visit(const CompoundStmtNode& compound_stmt) {
  for (const auto& stmt : compound_stmt.stmts) {
    stmt->Accept(*this);
  }
}
void LLVMIRGenerator::Visit(const ExternDeclNode& extern_decl) {
  std::visit([this](auto&& extern_decl) { extern_decl->Accept(*this); },
             extern_decl.decl);
}

void LLVMIRGenerator::Visit(const TransUnitNode& trans_unit) {
  // Generate builtin print function.
  auto arg = llvm::ArrayRef<llvm::Type*>{llvm_util_.IntType()};
  auto builtin_print =
      llvm::FunctionType::get(llvm_util_.IntType(), arg, false);
  llvm::Function::Create(builtin_print, llvm::Function::ExternalLinkage,
                         "__builtin_print", *module_);

  // Generate printf function for LLVM interpreter.
  auto args = llvm::ArrayRef<llvm::Type*>{llvm_util_.IntPtrType(),
                                          llvm_util_.IntType()};
  auto printf = llvm::FunctionType::get(llvm_util_.IntType(), args, false);
  llvm::Function::Create(printf, llvm::Function::ExternalLinkage, "printf",
                         *module_);

  builder_->CreateGlobalString("%d\n", "__builtin_print_format", 0,
                               module_.get());

  for (const auto& extern_decl : trans_unit.extern_decls) {
    extern_decl->Accept(*this);
  }
}

void LLVMIRGenerator::Visit(const IfStmtNode& if_stmt) {
  if_stmt.predicate->Accept(*this);
  auto predicate_val = val_recorder.ValOfPrevExpr();
  auto func = llvm_util_.CurrFunc();
  auto then_bb = llvm::BasicBlock::Create(*context_, "if_then", func);
  auto else_bb = if_stmt.or_else != nullptr
                     ? llvm::BasicBlock::Create(*context_, "if_else", func)
                     : nullptr;
  auto end_bb = llvm::BasicBlock::Create(*context_, "if_end", func);

  auto zero = llvm::ConstantInt::get(predicate_val->getType(), 0, true);
  auto predicate = builder_->CreateICmpNE(predicate_val, zero);
  builder_->CreateCondBr(predicate, then_bb,
                         if_stmt.or_else != nullptr ? else_bb : end_bb);
  builder_->SetInsertPoint(then_bb);
  if_stmt.then->Accept(*this);
  llvm_util_.CreateBrIfNoBrBefore(end_bb);

  if (if_stmt.or_else) {
    builder_->SetInsertPoint(else_bb);
    if_stmt.or_else->Accept(*this);
    builder_->CreateBr(end_bb);
  }
  builder_->SetInsertPoint(end_bb);
}

void LLVMIRGenerator::Visit(const WhileStmtNode& while_stmt) {
  auto label_prefix = std::string{while_stmt.is_do_while ? "do_" : "while_"};
  auto func = llvm_util_.CurrFunc();
  auto body_bb =
      llvm::BasicBlock::Create(*context_, label_prefix + "body", func);
  auto pred_bb =
      llvm::BasicBlock::Create(*context_, label_prefix + "pred", func);
  auto end_bb = llvm::BasicBlock::Create(*context_, label_prefix + "end", func);

  // A while statement's predicate is evaluated "before" the body statement,
  // whereas a do-while statement's predicate is evaluated "after" the body
  // statement. In the generated code for a while statement, there is an
  // unconditional jump at the end of the body to jump back to the predicate.
  // For a do-while statement, it only needs one conditional jump.
  if (!while_stmt.is_do_while) {
    builder_->CreateBr(pred_bb);
    builder_->SetInsertPoint(pred_bb);
    while_stmt.predicate->Accept(*this);
    auto predicate = val_recorder.ValOfPrevExpr();
    builder_->CreateCondBr(predicate, body_bb, end_bb);
  }

  // Connect entry basic block to body basic block.
  if (while_stmt.is_do_while) {
    builder_->CreateBr(body_bb);
  }
  builder_->SetInsertPoint(body_bb);
  label_views_of_jumpable_blocks.push_back({.entry = pred_bb, .exit = end_bb});
  while_stmt.loop_body->Accept(*this);
  label_views_of_jumpable_blocks.pop_back();
  llvm_util_.CreateBrIfNoBrBefore(pred_bb);

  if (while_stmt.is_do_while) {
    builder_->SetInsertPoint(pred_bb);
    while_stmt.predicate->Accept(*this);
    auto predicate = val_recorder.ValOfPrevExpr();
    builder_->CreateCondBr(predicate, body_bb, end_bb);
  }
  builder_->SetInsertPoint(end_bb);
}

void LLVMIRGenerator::Visit(const ForStmtNode& for_stmt) {
  auto func = llvm_util_.CurrFunc();
  auto pred_bb = llvm::BasicBlock::Create(*context_, "for_pred", func);
  auto body_bb = llvm::BasicBlock::Create(*context_, "for_body", func);
  auto step_bb = llvm::BasicBlock::Create(*context_, "for_step", func);
  auto end_bb = llvm::BasicBlock::Create(*context_, "for_end", func);

  for_stmt.loop_init->Accept(*this);
  builder_->CreateBr(pred_bb);
  builder_->SetInsertPoint(pred_bb);
  for_stmt.predicate->Accept(*this);
  if (!dynamic_cast<NullExprNode*>((for_stmt.predicate).get())) {
    auto predicate = val_recorder.ValOfPrevExpr();
    builder_->CreateCondBr(predicate, body_bb, end_bb);
  }

  builder_->SetInsertPoint(body_bb);
  label_views_of_jumpable_blocks.push_back({.entry = step_bb, .exit = end_bb});
  for_stmt.loop_body->Accept(*this);
  label_views_of_jumpable_blocks.pop_back();
  llvm_util_.CreateBrIfNoBrBefore(step_bb);

  builder_->SetInsertPoint(step_bb);
  for_stmt.step->Accept(*this);
  builder_->CreateBr(pred_bb);
  builder_->SetInsertPoint(end_bb);
}

void LLVMIRGenerator::Visit(const ReturnStmtNode& ret_stmt) {
  ret_stmt.expr->Accept(*this);
  auto expr = val_recorder.ValOfPrevExpr();
  builder_->CreateRet(expr);
}

void LLVMIRGenerator::Visit(const GotoStmtNode& goto_stmt) {
  llvm::BasicBlock* target_bb = llvm_util_.FindBBWithNameOf(goto_stmt.label);

  if (target_bb) {
    builder_->CreateBr(target_bb);
  } else {
    auto label_bb = llvm::BasicBlock::Create(*context_, goto_stmt.label,
                                             llvm_util_.CurrFunc());
    builder_->CreateBr(label_bb);
  }
}

void LLVMIRGenerator::Visit(const BreakStmtNode& break_stmt) {
  assert(!label_views_of_jumpable_blocks.empty());
  llvm_util_.CreateBrIfNoBrBefore(label_views_of_jumpable_blocks.back().exit);
}

void LLVMIRGenerator::Visit(const ContinueStmtNode& continue_stmt) {
  assert(!label_views_of_jumpable_blocks.empty());
  llvm_util_.CreateBrIfNoBrBefore(label_views_of_jumpable_blocks.back().entry);
}

void LLVMIRGenerator::Visit(const SwitchStmtNode& switch_stmt) {
  switch_stmt.ctrl->Accept(*this);
  auto ctrl = val_recorder.ValOfPrevExpr();
  auto end_bb =
      llvm::BasicBlock::Create(*context_, "switch_end", llvm_util_.CurrFunc());
  auto sw = builder_->CreateSwitch(ctrl, nullptr);
  label_views_of_jumpable_blocks.push_back({.entry = end_bb, .exit = end_bb});
  switch_stmt.stmt->Accept(*this);
  // Update cases and default label.
  auto switch_infos = label_views_of_jumpable_blocks.back();
  for (auto i = std::size_t{0}, e = switch_infos.cases.size(); i < e; ++i) {
    auto case_info = switch_infos.cases.at(i);
    auto curr_bb = case_info.second;
    if (!case_info.first) {  // default case
      sw->setDefaultDest(curr_bb);
    } else {
      auto const_val = llvm::dyn_cast<llvm::ConstantInt>(case_info.first);
      assert(const_val);
      sw->addCase(const_val, curr_bb);
    }

    // NOTE: If BB has no terminator and has a next BB, add one branch to next
    // BB. If BB is the last BB, then branch to switch exit.
    if (i + 1 != e) {
      auto next_bb = switch_infos.cases.at(i + 1).second;
      llvm_util_.CurrBBFallThroughNextBB(curr_bb, next_bb);
    } else {
      llvm_util_.CurrBBFallThroughNextBB(curr_bb, switch_infos.exit);
    }
  }
  label_views_of_jumpable_blocks.pop_back();
  llvm_util_.CreateBrIfNoBrBefore(end_bb);
  builder_->SetInsertPoint(end_bb);
}

void LLVMIRGenerator::Visit(const IdLabeledStmtNode& id_labeled_stmt) {
  llvm::BasicBlock* target_bb =
      llvm_util_.FindBBWithNameOf(id_labeled_stmt.label);

  if (!target_bb) {
    auto label_bb = llvm::BasicBlock::Create(*context_, id_labeled_stmt.label,
                                             llvm_util_.CurrFunc());
    builder_->SetInsertPoint(label_bb);
  } else {
    builder_->SetInsertPoint(target_bb);
  }

  id_labeled_stmt.stmt->Accept(*this);
}

void LLVMIRGenerator::Visit(const CaseStmtNode& case_stmt) {
  case_stmt.expr->Accept(*this);
  auto val = val_recorder.ValOfPrevExpr();
  auto int_expr = dynamic_cast<IntConstExprNode*>(case_stmt.expr.get());
  assert(int_expr);

  auto case_bb = llvm::BasicBlock::Create(
      *context_, "case" + std::to_string(int_expr->val), llvm_util_.CurrFunc());

  builder_->SetInsertPoint(case_bb);
  case_stmt.stmt->Accept(*this);

  assert(!label_views_of_jumpable_blocks.empty());
  std::pair<llvm::Value*, llvm::BasicBlock*> p{val, case_bb};
  label_views_of_jumpable_blocks.back().cases.push_back(p);
}

void LLVMIRGenerator::Visit(const DefaultStmtNode& default_stmt) {
  auto default_bb =
      llvm::BasicBlock::Create(*context_, "default", llvm_util_.CurrFunc());
  builder_->SetInsertPoint(default_bb);
  default_stmt.stmt->Accept(*this);

  assert(!label_views_of_jumpable_blocks.empty());
  std::pair<llvm::Value*, llvm::BasicBlock*> p{nullptr, default_bb};
  label_views_of_jumpable_blocks.back().cases.push_back(p);
}

void LLVMIRGenerator::Visit(const ExprStmtNode& expr_stmt) {
  expr_stmt.expr->Accept(*this);
}

void LLVMIRGenerator::Visit(const InitExprNode& init_expr) {
  init_expr.expr->Accept(*this);
}

void LLVMIRGenerator::Visit(const ArrDesNode& arr_des) {}

void LLVMIRGenerator::Visit(const IdDesNode& id_des) {}

void LLVMIRGenerator::Visit(const NullExprNode& null_expr) {
  /* do nothing */
}

void LLVMIRGenerator::Visit(const IdExprNode& id_expr) {
  if (id_expr.type->IsFunc()) {
    auto* func = module_->getFunction(id_expr.id);
    assert(func);
    val_recorder.Record(func);
    return;
  }
  assert(id_to_val.count(id_expr.id) != 0);
  auto id_val = id_to_val.at(id_expr.id);

  if (id_expr.type->IsPtr() || id_expr.type->IsFunc()) {
    auto res = builder_->CreateLoad(llvm_util_.IntPtrType(), id_val);
    val_recorder.Record(res);
    val_to_id_addr[res] = id_val;
  } else {
    auto res =
        builder_->CreateLoad(llvm_util_.GetLLVMType(*(id_expr.type)), id_val);
    val_recorder.Record(res);
    val_to_id_addr[res] = id_val;
  }
}

void LLVMIRGenerator::Visit(const IntConstExprNode& int_expr) {
  // NOTE: LLVM Constant does not generate IR code, it can be used directly.
  auto val = llvm::ConstantInt::get(llvm_util_.IntType(), int_expr.val, true);
  val_recorder.Record(val);
}

// builtin_print call
void LLVMIRGenerator::Visit(const ArgExprNode& arg_expr) {
  arg_expr.arg->Accept(*this);
}

void LLVMIRGenerator::Visit(const ArrSubExprNode& arr_sub_expr) {
  arr_sub_expr.arr->Accept(*this);
  auto val = val_recorder.ValOfPrevExpr();
  auto base_addr = val_to_id_addr.at(val);
  auto arr_type = llvm_util_.GetLLVMType(*(arr_sub_expr.arr->type));
  arr_sub_expr.index->Accept(*this);
  auto index = dynamic_cast<IntConstExprNode*>(arr_sub_expr.index.get());
  assert(index);

  auto res_addr = builder_->CreateConstInBoundsGEP2_32(
      arr_type, base_addr, 0, (unsigned int)index->val);
  auto res_val =
      builder_->CreateLoad(arr_type->getArrayElementType(), res_addr);
  val_to_id_addr[res_val] = res_addr;
  val_recorder.Record(res_val);
}

void LLVMIRGenerator::Visit(const CondExprNode& cond_expr) {
  cond_expr.predicate->Accept(*this);
  auto predicate_val = val_recorder.ValOfPrevExpr();
  auto func = llvm_util_.CurrFunc();
  // The second operand is evaluated only if the first compares unequal to
  // 0; the third operand is evaluated only if the first compares equal to
  // 0; the result is the value of the second or third operand (whichever is
  // evaluated).
  auto second_bb = llvm::BasicBlock::Create(*context_, "cond_second", func);
  auto third_bb = llvm::BasicBlock::Create(*context_, "cond_third", func);
  auto end_bb = llvm::BasicBlock::Create(*context_, "cond_end", func);

  auto zero = llvm::ConstantInt::get(predicate_val->getType(), 0, true);
  auto predicate = builder_->CreateICmpNE(predicate_val, zero);
  builder_->CreateCondBr(predicate, second_bb, third_bb);

  builder_->SetInsertPoint(second_bb);
  cond_expr.then->Accept(*this);
  auto second_val = val_recorder.ValOfPrevExpr();
  builder_->CreateBr(end_bb);

  builder_->SetInsertPoint(third_bb);
  cond_expr.or_else->Accept(*this);
  auto third_val = val_recorder.ValOfPrevExpr();
  builder_->CreateBr(end_bb);

  builder_->SetInsertPoint(end_bb);
  // NOTE: Since we do not know which operand will be executed in runtime, we
  // create a Phi node to merge both values.
  auto phi_res = builder_->CreatePHI(llvm_util_.IntType(), 2);
  phi_res->addIncoming(second_val, second_bb);
  phi_res->addIncoming(third_val, third_bb);
  val_recorder.Record(phi_res);
}

void LLVMIRGenerator::Visit(const FuncCallExprNode& call_expr) {
  // Evaluate the arguments.
  std::vector<llvm::Value*> arg_vals{};
  for (const auto& arg : call_expr.args) {
    arg->Accept(*this);
    auto* arg_val = val_recorder.ValOfPrevExpr();
    arg_vals.push_back(arg_val);
  }

  call_expr.func_expr->Accept(*this);
  auto val = val_recorder.ValOfPrevExpr();
  if (auto func = llvm::dyn_cast<llvm::Function>(val)) {
    if (func->getName() == "__builtin_print") {
      // builtin_print call
      auto printf = module_->getFunction("printf");
      std::vector<llvm::Value*> print_args{};
      // NOTE: set AllowInternal true to get internal linkage global variable
      auto print_format =
          module_->getGlobalVariable("__builtin_print_format", true);
      assert(print_format);
      print_args.push_back(print_format);
      print_args.insert(print_args.end(), arg_vals.begin(), arg_vals.end());
      builder_->CreateCall(printf, print_args);
    } else {
      auto called_func = module_->getFunction(func->getName());
      auto return_res = builder_->CreateCall(called_func, arg_vals);
      val_recorder.Record(return_res);
    }
  } else if (val->getType()->isPointerTy()) {
    // function pointer
    auto type = llvm_util_.GetLLVMType(*(call_expr.func_expr->type));
    if (auto func_type = llvm::dyn_cast<llvm::FunctionType>(type)) {
      auto return_res = builder_->CreateCall(func_type, val, arg_vals);
      val_recorder.Record(return_res);
    } else {
      // TODO: unreachable
      assert(false);
    }
  }
}

void LLVMIRGenerator::Visit(const PostfixArithExprNode& postfix_expr) {
  postfix_expr.operand->Accept(*this);
  auto val = val_recorder.ValOfPrevExpr();
  val_recorder.Record(val);

  auto arith_op = postfix_expr.op == PostfixOperator::kIncr
                      ? llvm::BinaryOperator::Add
                      : llvm::BinaryOperator::Sub;

  auto one = llvm::ConstantInt::get(llvm_util_.IntType(), 1, true);
  auto res = builder_->CreateBinOp(arith_op, val, one);
  const auto* id_expr = dynamic_cast<IdExprNode*>((postfix_expr.operand).get());
  assert(id_expr);
  builder_->CreateStore(res, id_to_val.at(id_expr->id));
}

void LLVMIRGenerator::Visit(const RecordMemExprNode& mem_expr) {
  mem_expr.expr->Accept(*this);
  auto val = val_recorder.ValOfPrevExpr();
  auto base_addr = val_to_id_addr.at(val);
  auto struct_type = llvm_util_.GetLLVMType(*(mem_expr.expr->type));
  auto* record_type = dynamic_cast<RecordType*>(mem_expr.expr->type.get());
  assert(record_type);

  auto res_addr = builder_->CreateStructGEP(
      struct_type, base_addr, record_type->MemberIndex(mem_expr.id));
  auto res_val = builder_->CreateLoad(
      llvm_util_.GetLLVMType(*(record_type->MemberType(mem_expr.id))),
      res_addr);
  val_to_id_addr[res_val] = res_addr;
  val_recorder.Record(res_val);
}

void LLVMIRGenerator::Visit(const UnaryExprNode& unary_expr) {
  unary_expr.operand->Accept(*this);
  switch (unary_expr.op) {
    case UnaryOperator::kIncr:
    case UnaryOperator::kDecr: {
      // Equivalent to i += 1 or i -= 1.
      auto operand = val_recorder.ValOfPrevExpr();
      auto arith_op = unary_expr.op == UnaryOperator::kIncr
                          ? BinaryOperator::kAdd
                          : BinaryOperator::kSub;
      auto one = llvm::ConstantInt::get(operand->getType(), 1, true);
      auto res =
          builder_->CreateBinOp(GetBinaryOperator(arith_op), operand, one);
      builder_->CreateStore(res, val_to_id_addr.at(operand));
      val_recorder.Record(res);
    } break;
    case UnaryOperator::kPos: {
      /* Do nothing. */
    } break;
    case UnaryOperator::kNeg: {
      auto operand = val_recorder.ValOfPrevExpr();
      auto zero = llvm::ConstantInt::get(operand->getType(), 0, true);
      auto res = builder_->CreateSub(zero, operand);
      val_recorder.Record(res);
    } break;
    case UnaryOperator::kNot: {
      auto operand = val_recorder.ValOfPrevExpr();
      auto zero = llvm::ConstantInt::get(operand->getType(), 0, true);
      auto res = builder_->CreateICmpEQ(operand, zero);
      val_recorder.Record(res);
    } break;
    case UnaryOperator::kBitComp: {
      auto operand = val_recorder.ValOfPrevExpr();
      auto all_ones = llvm::ConstantInt::get(operand->getType(), -1, true);
      auto res = builder_->CreateXor(operand, all_ones);
      val_recorder.Record(res);
    } break;
    case UnaryOperator::kAddr: {
      if (unary_expr.operand->type->IsFunc()) {
        // No-op; the function itself already evaluates to the address.
        break;
      }
      auto operand = val_recorder.ValOfPrevExpr();
      auto operand_addr = val_to_id_addr.at(operand);
      val_recorder.Record(operand_addr);
    } break;
    case UnaryOperator::kDeref: {
      // Is function pointer.
      if (unary_expr.operand->type->IsPtr() &&
          dynamic_cast<PtrType*>((unary_expr.operand->type).get())
              ->base_type()
              .IsFunc()) {
        // No-op; the function itself also evaluates to the address.
        break;
      }

      auto operand = val_recorder.ValOfPrevExpr();
      auto res = builder_->CreateLoad(unary_expr.type->IsPtr()
                                          ? (llvm::Type*)llvm_util_.IntPtrType()
                                          : (llvm::Type*)llvm_util_.IntType(),
                                      operand);
      val_recorder.Record(res);
      val_to_id_addr[res] = operand;
    } break;
    default:
      break;
  }
}

void LLVMIRGenerator::Visit(const BinaryExprNode& bin_expr) {
  bin_expr.lhs->Accept(*this);
  auto lhs = val_recorder.ValOfPrevExpr();

  if (bin_expr.op == BinaryOperator::kComma) {
    // For the comma operator, the value of its left operand is not used, so
    // we passed right hand side operand to the upper level.
    bin_expr.rhs->Accept(*this);
    auto rhs = val_recorder.ValOfPrevExpr();
    val_recorder.Record(rhs);
    return;
  }
  if (bin_expr.op == BinaryOperator::kLand ||
      bin_expr.op == BinaryOperator::kLor) {
    auto func = llvm_util_.CurrFunc();
    auto rhs_bb = llvm::BasicBlock::Create(*context_, "logic_rhs", func);
    auto short_circuit_bb =
        llvm::BasicBlock::Create(*context_, "short_circuit", func);
    auto end_bb = llvm::BasicBlock::Create(*context_, "logic_end", func);
    auto zero = llvm::ConstantInt::get(lhs->getType(), 0, true);
    auto lhs_res = builder_->CreateCmp(bin_expr.op == BinaryOperator::kLand
                                           ? llvm::CmpInst::Predicate::ICMP_NE
                                           : llvm::CmpInst::Predicate::ICMP_EQ,
                                       lhs, zero);
    builder_->CreateCondBr(lhs_res, rhs_bb, short_circuit_bb);
    builder_->SetInsertPoint(rhs_bb);
    bin_expr.rhs->Accept(*this);
    auto res = val_recorder.ValOfPrevExpr();
    auto rhs_res = builder_->CreateICmpNE(res, zero);
    builder_->CreateBr(end_bb);
    builder_->SetInsertPoint(short_circuit_bb);
    auto false_val = llvm::ConstantInt::getFalse(*context_);
    auto true_val = llvm::ConstantInt::getTrue(*context_);
    auto short_circuit_res =
        bin_expr.op == BinaryOperator::kLand ? false_val : true_val;
    builder_->CreateBr(end_bb);
    builder_->SetInsertPoint(end_bb);
    // Merge results from rhs and short_circuit_res.
    auto phi_res = builder_->CreatePHI(builder_->getInt1Ty(), 2);
    phi_res->addIncoming(rhs_res, rhs_bb);
    phi_res->addIncoming(short_circuit_res, short_circuit_bb);
    val_recorder.Record(phi_res);
  } else if (IsCmpInst(bin_expr.op)) {
    bin_expr.rhs->Accept(*this);
    auto rhs = val_recorder.ValOfPrevExpr();
    auto res = builder_->CreateCmp(GetCmpPredicate(bin_expr.op), lhs, rhs);
    val_recorder.Record(res);
  } else {
    bin_expr.rhs->Accept(*this);
    auto rhs = val_recorder.ValOfPrevExpr();
    auto res = builder_->CreateBinOp(GetBinaryOperator(bin_expr.op), lhs, rhs);
    val_recorder.Record(res);
  }
}

void LLVMIRGenerator::Visit(const SimpleAssignmentExprNode& assign_expr) {
  assign_expr.lhs->Accept(*this);
  auto lhs = val_recorder.ValOfPrevExpr();
  assign_expr.rhs->Accept(*this);
  auto rhs = val_recorder.ValOfPrevExpr();
  builder_->CreateStore(rhs, val_to_id_addr.at(lhs));
  val_recorder.Record(rhs);
}
