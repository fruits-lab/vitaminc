#include "llvm_ir_generator.hpp"

#include <llvm/IR/IRBuilder.h>

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
    default:
      // TODO
      return llvm::BinaryOperator::Xor;
  }
}

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
}  // namespace

void LLVMIRGenerator::Visit(const DeclStmtNode& decl_stmt) {
  for (const auto& decl : decl_stmt.decls) {
    decl->Accept(*this);
  }
}

void LLVMIRGenerator::Visit(const VarDeclNode& decl) {}

void LLVMIRGenerator::Visit(const ArrDeclNode& arr_decl) {}

void LLVMIRGenerator::Visit(const RecordDeclNode& struct_def) {}

void LLVMIRGenerator::Visit(const FieldNode& field) {}

void LLVMIRGenerator::Visit(const RecordVarDeclNode& struct_def) {}

void LLVMIRGenerator::Visit(const ParamNode& parameter) {}

void LLVMIRGenerator::Visit(const FuncDefNode& func_def) {
  auto i32 = builder_.getInt32Ty();
  auto prototype = llvm::FunctionType::get(i32, false);
  auto* fn = llvm::Function::Create(prototype,
                                    func_def.id == "main"
                                        ? llvm::Function::ExternalLinkage
                                        : llvm::Function::InternalLinkage,
                                    func_def.id, module_.get());
  auto* body = llvm::BasicBlock::Create(*context_, "body", fn);
  builder_.SetInsertPoint(body);
  func_def.body->Accept(*this);
}

void LLVMIRGenerator::Visit(const LoopInitNode& loop_init) {}

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
  auto i32 = builder_.getInt32Ty();
  // pointer to char
  auto ptr = builder_.getPtrTy();
  std::vector<llvm::Type*> params;
  params.push_back(ptr);
  auto prototype = llvm::FunctionType::get(i32, params, true);
  llvm::Function::Create(prototype, llvm::Function::ExternalLinkage, "printf",
                         module_.get());

  for (const auto& extern_decl : trans_unit.extern_decls) {
    extern_decl->Accept(*this);
  }
}

void LLVMIRGenerator::Visit(const IfStmtNode& if_stmt) {}

void LLVMIRGenerator::Visit(const WhileStmtNode& while_stmt) {}

void LLVMIRGenerator::Visit(const ForStmtNode& for_stmt) {}

void LLVMIRGenerator::Visit(const ReturnStmtNode& ret_stmt) {
  ret_stmt.expr->Accept(*this);
  auto i32 = builder_.getInt32Ty();
  auto* expr = val_recorder.ValOfPrevExpr();
  auto* ret = builder_.CreateLoad(i32, expr);
  builder_.CreateRet(ret);
}

void LLVMIRGenerator::Visit(const GotoStmtNode& goto_stmt) {}

void LLVMIRGenerator::Visit(const BreakStmtNode& break_stmt) {}

void LLVMIRGenerator::Visit(const ContinueStmtNode& continue_stmt) {}

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

void LLVMIRGenerator::Visit(const NullExprNode& null_expr) {}

void LLVMIRGenerator::Visit(const IdExprNode& id_expr) {}

void LLVMIRGenerator::Visit(const IntConstExprNode& int_expr) {
  auto i32 = builder_.getInt32Ty();
  auto* addr = builder_.CreateAlloca(i32);
  auto* val = llvm::ConstantInt::get(i32, int_expr.val, true);
  builder_.CreateStore(val, addr);
  val_recorder.Record(addr);
}

void LLVMIRGenerator::Visit(const ArgExprNode& arg_expr) {
  arg_expr.arg->Accept(*this);
}

void LLVMIRGenerator::Visit(const ArrSubExprNode& arr_sub_expr) {}

void LLVMIRGenerator::Visit(const CondExprNode& cond_expr) {}

void LLVMIRGenerator::Visit(const FuncCallExprNode& call_expr) {
  call_expr.func_expr->Accept(*this);

  // Evaluate the arguments.
  std::vector<llvm::Value*> arg_vals{};
  for (const auto& arg : call_expr.args) {
    arg->Accept(*this);
    auto* arg_val = val_recorder.ValOfPrevExpr();
    arg_vals.push_back(arg_val);
  }
}

void LLVMIRGenerator::Visit(const PostfixArithExprNode& postfix_expr) {}

void LLVMIRGenerator::Visit(const UnaryExprNode& unary_expr) {}

void LLVMIRGenerator::Visit(const BinaryExprNode& bin_expr) {
  bin_expr.lhs->Accept(*this);
  auto* lhs = val_recorder.ValOfPrevExpr();

  if (bin_expr.op == BinaryOperator::kComma) {
    // For the comma operator, the value of its left operand is not used and can
    // be eliminated if it has no side effects or if its definition is
    // immediately dead. However, we leave these optimizations to QBE.
    bin_expr.rhs->Accept(*this);
    auto* rhs = val_recorder.ValOfPrevExpr();
    val_recorder.Record(rhs);
    return;
  }
  if (bin_expr.op == BinaryOperator::kLand ||
      bin_expr.op == BinaryOperator::kLor) {
    // TODO
  } else {
    auto* rhs = val_recorder.ValOfPrevExpr();
    builder_.CreateBinOp(GetBinaryOperator(bin_expr.op), lhs, rhs);
  }
}

void LLVMIRGenerator::Visit(const SimpleAssignmentExprNode& assign_expr) {}
