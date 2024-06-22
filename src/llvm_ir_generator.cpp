#include "llvm_ir_generator.hpp"

#include <llvm/IR/IRBuilder.h>

#include <map>

#include "ast.hpp"

namespace {

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

using namespace llvm;

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
  auto prototype = FunctionType::get(i32, false);
  auto* fn = Function::Create(prototype,
                              func_def.id == "main" ? Function::ExternalLinkage
                                                    : Function::InternalLinkage,
                              func_def.id, module_.get());
  auto* body = BasicBlock::Create(*context_, "body", fn);
  builder_.SetInsertPoint(body);
  func_def.body->Accept(*this);
}

void LLVMIRGenerator::Visit(const LoopInitNode& loop_init) {}

void LLVMIRGenerator::Visit(const CompoundStmtNode& compound_stmt) {
  for (const auto& stmt : compound_stmt.stmts) {
    stmt->Accept(*this);
  }
}

void LLVMIRGenerator::Visit(const ProgramNode& program) {
  // Generate builtin print function.
  auto i32 = builder_.getInt32Ty();
  // pointer to char
  auto ptr = builder_.getPtrTy();
  std::vector<llvm::Type*> params;
  params.push_back(ptr);
  auto prototype = FunctionType::get(i32, params, true);
  Function::Create(prototype, Function::ExternalLinkage, "printf",
                   module_.get());

  for (const auto& func_def : program.func_def_list) {
    func_def->Accept(*this);
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

void LLVMIRGenerator::Visit(const ExprStmtNode& expr_stmt) {}

void LLVMIRGenerator::Visit(const InitExprNode& init_expr) {}

void LLVMIRGenerator::Visit(const ArrDesNode& arr_des) {}

void LLVMIRGenerator::Visit(const IdDesNode& id_des) {}

void LLVMIRGenerator::Visit(const NullExprNode& null_expr) {}

void LLVMIRGenerator::Visit(const IdExprNode& id_expr) {}

void LLVMIRGenerator::Visit(const IntConstExprNode& int_expr) {
  auto i32 = builder_.getInt32Ty();
  auto* addr = builder_.CreateAlloca(i32);
  auto* val = ConstantInt::get(i32, int_expr.val, true);
  builder_.CreateStore(val, addr);
  val_recorder.Record(addr);
}

void LLVMIRGenerator::Visit(const ArgExprNode& arg_expr) {}

void LLVMIRGenerator::Visit(const ArrSubExprNode& arr_sub_expr) {}

void LLVMIRGenerator::Visit(const CondExprNode& cond_expr) {}

void LLVMIRGenerator::Visit(const FuncCallExprNode& call_expr) {}

void LLVMIRGenerator::Visit(const PostfixArithExprNode& postfix_expr) {}

void LLVMIRGenerator::Visit(const UnaryExprNode& unary_expr) {}

void LLVMIRGenerator::Visit(const BinaryExprNode& bin_expr) {}

void LLVMIRGenerator::Visit(const SimpleAssignmentExprNode& assign_expr) {}
