#include "llvm_ir_generator.hpp"

#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>

#include <map>

#include "ast.hpp"

namespace {

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
    // NOTE: Arithmetic shift right (sar) is akin to dividing by a power of two
    // for non-negative numbers. For negatives, it's implementation-defined, so
    // we opt for arithmetic shifting.
    case BinaryOperator::kShr:
      return llvm::BinaryOperator::AShr;
    default:
      // TODO: unreachable
      return llvm::BinaryOperator::Xor;
  }
}

llvm::CmpInst::Predicate GetCmpOperator(BinaryOperator op) {
  switch (op) {
    case BinaryOperator::kGt: {
      return llvm::CmpInst::Predicate::ICMP_SGT;
    } break;
    case BinaryOperator::kGte: {
      return llvm::CmpInst::Predicate::ICMP_SGE;
    } break;
    case BinaryOperator::kLt: {
      return llvm::CmpInst::Predicate::ICMP_SLT;
    } break;
    case BinaryOperator::kLte: {
      return llvm::CmpInst::Predicate::ICMP_SLE;
    } break;
    case BinaryOperator::kEq: {
      return llvm::CmpInst::Predicate::ICMP_EQ;
    } break;
    case BinaryOperator::kNeq: {
      return llvm::CmpInst::Predicate::ICMP_NE;
    } break;
    default:
      return llvm::CmpInst::Predicate::BAD_ICMP_PREDICATE;
  }
}

bool isCmpInst(BinaryOperator op) {
  switch (op) {
    case BinaryOperator::kGt:
    case BinaryOperator::kGte:
    case BinaryOperator::kLt:
    case BinaryOperator::kLte:
    case BinaryOperator::kEq:
    case BinaryOperator::kNeq: {
      return true;
    } break;
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

/// @brief Store LLVM Value class at the bottom level of AST node. Upper level
/// AST node can use the information in Value directly.
class PrevValueRecorder {
 public:
  void Record(llvm::Value* val) {
    prev_val_ = val;
  }

  llvm::Value* ValOfPrevExpr() {
    return prev_val_;
  }

 private:
  llvm::Value* prev_val_;
};

auto
    val_recorder  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables):
                  // Accessible only within this translation unit; declaring as
                  // a data member introduces unnecessary dependency.
    = PrevValueRecorder{};

struct LabelViewPair {
  llvm::BasicBlock* entry;
  llvm::BasicBlock* exit;
};

/// @note Blocks that allows jumping within or out of it should add its labels
/// to this list
auto
    label_views_of_jumpable_blocks  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)
    = std::vector<LabelViewPair>{};

}  // namespace

void LLVMIRGenerator::Visit(const DeclStmtNode& decl_stmt) {
  for (const auto& decl : decl_stmt.decls) {
    decl->Accept(*this);
  }
}

void LLVMIRGenerator::Visit(const VarDeclNode& decl) {
  auto addr = builder_->CreateAlloca(decl.type->IsPtr() == true
                                         ? (llvm::Type*)util_.intPtrTy
                                         : (llvm::Type*)util_.intTy);
  if (decl.init) {
    decl.init->Accept(*this);
    auto val = val_recorder.ValOfPrevExpr();
    builder_->CreateStore(val, addr);
  }
  id_to_val[decl.id] = addr;
}

void LLVMIRGenerator::Visit(const ArrDeclNode& arr_decl) {}

void LLVMIRGenerator::Visit(const RecordDeclNode& struct_def) {}

void LLVMIRGenerator::Visit(const FieldNode& field) {}

void LLVMIRGenerator::Visit(const RecordVarDeclNode& struct_def) {}

void LLVMIRGenerator::Visit(const ParamNode& parameter) {}

void LLVMIRGenerator::Visit(const FuncDefNode& func_def) {
  auto prototype = llvm::FunctionType::get(util_.intTy, false);
  auto* fn = llvm::Function::Create(prototype,
                                    func_def.id == "main"
                                        ? llvm::Function::ExternalLinkage
                                        : llvm::Function::InternalLinkage,
                                    func_def.id, module_);
  auto* body = llvm::BasicBlock::Create(*context_, "body", fn);
  builder_->SetInsertPoint(body);
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
  auto arg = llvm::ArrayRef<llvm::Type*>{util_.intTy};
  auto builtin_print = llvm::FunctionType::get(util_.intTy, arg, false);
  llvm::Function::Create(builtin_print, llvm::Function::ExternalLinkage,
                         "__builtin_print", module_);

  auto ptrTy = builder_->getPtrTy();
  auto args = llvm::ArrayRef<llvm::Type*>{ptrTy, util_.intTy};
  auto printf = llvm::FunctionType::get(util_.intTy, args, false);
  llvm::Function::Create(printf, llvm::Function::ExternalLinkage, "printf",
                         module_);

  builder_->CreateGlobalString("%d\n", "__builtin_print_format", 0, module_);

  for (const auto& extern_decl : trans_unit.extern_decls) {
    extern_decl->Accept(*this);
  }
}

void LLVMIRGenerator::Visit(const IfStmtNode& if_stmt) {
  if_stmt.predicate->Accept(*this);
  auto predicate_val = val_recorder.ValOfPrevExpr();
  auto func = builder_->GetInsertBlock()->getParent();
  auto then_BB = llvm::BasicBlock::Create(*context_, "if_then", func);
  auto else_BB = if_stmt.or_else != nullptr
                     ? llvm::BasicBlock::Create(*context_, "if_else", func)
                     : nullptr;
  auto end_BB = llvm::BasicBlock::Create(*context_, "if_end", func);

  auto zero = llvm::ConstantInt::get(predicate_val->getType(), 0, true);
  auto predicate = builder_->CreateICmpNE(predicate_val, zero);
  builder_->CreateCondBr(predicate, then_BB,
                         if_stmt.or_else != nullptr ? else_BB : end_BB);
  builder_->SetInsertPoint(then_BB);
  if_stmt.then->Accept(*this);
  util_.CreateBrIfNoBrBefore(end_BB);

  if (if_stmt.or_else) {
    builder_->SetInsertPoint(else_BB);
    if_stmt.or_else->Accept(*this);
    builder_->CreateBr(end_BB);
  }
  builder_->SetInsertPoint(end_BB);
}

void LLVMIRGenerator::Visit(const WhileStmtNode& while_stmt) {
  auto label_prefix = std::string{while_stmt.is_do_while ? "do_" : "while_"};
  auto func = builder_->GetInsertBlock()->getParent();
  auto body_BB =
      llvm::BasicBlock::Create(*context_, label_prefix + "body", func);
  auto pred_BB =
      llvm::BasicBlock::Create(*context_, label_prefix + "pred", func);
  auto end_BB = llvm::BasicBlock::Create(*context_, label_prefix + "end", func);

  // A while statement's predicate is evaluated "before" the body statement,
  // whereas a do-while statement's predicate is evaluated "after" the body
  // statement. In the generated code for a while statement, there is an
  // unconditional jump at the end of the body to jump back to the predicate.
  // For a do-while statement, it only needs one conditional jump.
  if (!while_stmt.is_do_while) {
    builder_->CreateBr(pred_BB);
    builder_->SetInsertPoint(pred_BB);
    while_stmt.predicate->Accept(*this);
    auto predicate = val_recorder.ValOfPrevExpr();
    builder_->CreateCondBr(predicate, body_BB, end_BB);
  }

  // Connect entry basic block to body basic block.
  if (while_stmt.is_do_while) {
    builder_->CreateBr(body_BB);
  }
  builder_->SetInsertPoint(body_BB);
  label_views_of_jumpable_blocks.push_back({.entry = pred_BB, .exit = end_BB});
  while_stmt.loop_body->Accept(*this);
  label_views_of_jumpable_blocks.pop_back();
  builder_->CreateBr(pred_BB);

  if (while_stmt.is_do_while) {
    builder_->SetInsertPoint(pred_BB);
    while_stmt.predicate->Accept(*this);
    auto predicate = val_recorder.ValOfPrevExpr();
    builder_->CreateCondBr(predicate, body_BB, end_BB);
  }
  builder_->SetInsertPoint(end_BB);
}

void LLVMIRGenerator::Visit(const ForStmtNode& for_stmt) {
  auto func = builder_->GetInsertBlock()->getParent();
  auto pred_BB = llvm::BasicBlock::Create(*context_, "for_pred", func);
  auto body_BB = llvm::BasicBlock::Create(*context_, "for_body", func);
  auto step_BB = llvm::BasicBlock::Create(*context_, "for_step", func);
  auto end_BB = llvm::BasicBlock::Create(*context_, "for_end", func);

  for_stmt.loop_init->Accept(*this);
  builder_->CreateBr(pred_BB);
  builder_->SetInsertPoint(pred_BB);
  for_stmt.predicate->Accept(*this);
  if (!dynamic_cast<NullExprNode*>((for_stmt.predicate).get())) {
    auto predicate = val_recorder.ValOfPrevExpr();
    builder_->CreateCondBr(predicate, body_BB, end_BB);
  }

  builder_->SetInsertPoint(body_BB);
  // TODO: break, continue label
  label_views_of_jumpable_blocks.push_back({.entry = step_BB, .exit = end_BB});
  for_stmt.loop_body->Accept(*this);
  label_views_of_jumpable_blocks.pop_back();
  builder_->CreateBr(step_BB);

  builder_->SetInsertPoint(step_BB);
  for_stmt.step->Accept(*this);
  builder_->CreateBr(pred_BB);
  builder_->SetInsertPoint(end_BB);
}

void LLVMIRGenerator::Visit(const ReturnStmtNode& ret_stmt) {
  ret_stmt.expr->Accept(*this);
  auto expr = val_recorder.ValOfPrevExpr();
  builder_->CreateRet(expr);
}

void LLVMIRGenerator::Visit(const GotoStmtNode& goto_stmt) {}

void LLVMIRGenerator::Visit(const BreakStmtNode& break_stmt) {
  assert(!label_views_of_jumpable_blocks.empty());
  builder_->CreateBr(label_views_of_jumpable_blocks.back().exit);
}

void LLVMIRGenerator::Visit(const ContinueStmtNode& continue_stmt) {
  assert(!label_views_of_jumpable_blocks.empty());
  builder_->CreateBr(label_views_of_jumpable_blocks.back().entry);
}

void LLVMIRGenerator::Visit(const SwitchStmtNode& switch_stmt) {}

void LLVMIRGenerator::Visit(const IdLabeledStmtNode& id_labeled_stmt) {}

void LLVMIRGenerator::Visit(const CaseStmtNode& case_stmt) {}

void LLVMIRGenerator::Visit(const DefaultStmtNode& default_stmt) {}

void LLVMIRGenerator::Visit(const ExprStmtNode& expr_stmt) {
  expr_stmt.expr->Accept(*this);
}

void LLVMIRGenerator::Visit(const InitExprNode& init_expr) {}

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
    auto res = builder_->CreateLoad(util_.intPtrTy, id_val);
    val_recorder.Record(res);
    val_to_id_addr[res] = id_val;
  } else {
    auto res = builder_->CreateLoad(util_.intTy, id_val);
    val_recorder.Record(res);
    val_to_id_addr[res] = id_val;
  }
}

void LLVMIRGenerator::Visit(const IntConstExprNode& int_expr) {
  // NOTE: LLVM Constant does not generate IR code, it can be used directly.
  auto val = llvm::ConstantInt::get(util_.intTy, int_expr.val, true);
  val_recorder.Record(val);
}

// builtin_print call
void LLVMIRGenerator::Visit(const ArgExprNode& arg_expr) {
  arg_expr.arg->Accept(*this);
}

void LLVMIRGenerator::Visit(const ArrSubExprNode& arr_sub_expr) {}

void LLVMIRGenerator::Visit(const CondExprNode& cond_expr) {}

void LLVMIRGenerator::Visit(const FuncCallExprNode& call_expr) {
  call_expr.func_expr->Accept(*this);
  auto val = val_recorder.ValOfPrevExpr();
  auto func = llvm::dyn_cast<llvm::Function>(val);
  assert(func);

  // Evaluate the arguments.
  std::vector<llvm::Value*> arg_vals{};
  for (const auto& arg : call_expr.args) {
    arg->Accept(*this);
    auto* arg_val = val_recorder.ValOfPrevExpr();
    arg_vals.push_back(arg_val);
  }
  if (func->getName() == "__builtin_print") {
    // builtin_print call
    auto printf = module_->getFunction("printf");
    assert(printf);
    std::vector<llvm::Value*> print_args{};
    // NOTE: set AllowInternal as true to get internal linkage global variable
    auto print_format =
        module_->getGlobalVariable("__builtin_print_format", true);
    assert(print_format);
    print_args.push_back(print_format);
    print_args.insert(print_args.end(), arg_vals.begin(), arg_vals.end());
    builder_->CreateCall(printf, print_args);
  } else {
  }
}

void LLVMIRGenerator::Visit(const PostfixArithExprNode& postfix_expr) {
  postfix_expr.operand->Accept(*this);
  auto val = val_recorder.ValOfPrevExpr();
  val_recorder.Record(val);

  auto arith_op = postfix_expr.op == PostfixOperator::kIncr
                      ? llvm::BinaryOperator::Add
                      : llvm::BinaryOperator::Sub;

  auto one = llvm::ConstantInt::get(util_.intTy, 1, true);
  auto res = builder_->CreateBinOp(arith_op, val, one);
  const auto* id_expr = dynamic_cast<IdExprNode*>((postfix_expr.operand).get());
  assert(id_expr);
  builder_->CreateStore(res, id_to_val.at(id_expr->id));
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
      auto one = llvm::ConstantInt::get(util_.intTy, 1, true);
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
      auto zero = llvm::ConstantInt::get(util_.intTy, 0, true);
      auto res =
          builder_->CreateBinOp(llvm::BinaryOperator::Sub, zero, operand);
      val_recorder.Record(res);
    } break;
    case UnaryOperator::kNot: {
      auto operand = val_recorder.ValOfPrevExpr();
      auto zero = llvm::ConstantInt::get(util_.intTy, 0, true);
      auto res = builder_->CreateICmpEQ(operand, zero);
      val_recorder.Record(res);
    } break;
    case UnaryOperator::kBitComp: {
      auto operand = val_recorder.ValOfPrevExpr();
      auto all_ones = llvm::ConstantInt::get(util_.intTy, -1, true);
      auto res =
          builder_->CreateBinOp(llvm::BinaryOperator::Xor, operand, all_ones);
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
      auto res = builder_->CreateLoad(unary_expr.type->IsPtr() == true
                                          ? (llvm::Type*)util_.intPtrTy
                                          : (llvm::Type*)util_.intTy,
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
    // Get the current function we are in.
    auto func = builder_->GetInsertBlock()->getParent();
    auto rhs_BB = llvm::BasicBlock::Create(*context_, "logic_rhs", func);
    auto short_circuit_BB =
        llvm::BasicBlock::Create(*context_, "short_circuit", func);
    auto end_BB = llvm::BasicBlock::Create(*context_, "logic_end", func);
    auto zero = llvm::ConstantInt::get(lhs->getType(), 0, true);
    auto lhs_res = builder_->CreateCmp(bin_expr.op == BinaryOperator::kLand
                                           ? llvm::CmpInst::Predicate::ICMP_NE
                                           : llvm::CmpInst::Predicate::ICMP_EQ,
                                       lhs, zero);
    builder_->CreateCondBr(lhs_res, rhs_BB, short_circuit_BB);
    builder_->SetInsertPoint(rhs_BB);
    bin_expr.rhs->Accept(*this);
    auto res = val_recorder.ValOfPrevExpr();
    auto rhs_res = builder_->CreateICmpNE(res, zero);
    builder_->CreateBr(end_BB);
    builder_->SetInsertPoint(short_circuit_BB);
    auto false_val = llvm::ConstantInt::getFalse(*context_);
    auto true_val = llvm::ConstantInt::getTrue(*context_);
    auto short_circuit_res =
        bin_expr.op == BinaryOperator::kLand ? false_val : true_val;
    builder_->CreateBr(end_BB);
    builder_->SetInsertPoint(end_BB);
    // Merge results from rhs and short_circuit_res
    auto phi_res = builder_->CreatePHI(builder_->getInt1Ty(), 2);
    phi_res->addIncoming(rhs_res, rhs_BB);
    phi_res->addIncoming(short_circuit_res, short_circuit_BB);
    val_recorder.Record(phi_res);
  } else if (isCmpInst(bin_expr.op)) {
    bin_expr.rhs->Accept(*this);
    auto rhs = val_recorder.ValOfPrevExpr();
    auto res = builder_->CreateCmp(GetCmpOperator(bin_expr.op), lhs, rhs);
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
