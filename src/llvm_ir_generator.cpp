#include "llvm_ir_generator.hpp"

#include <llvm/IR/IRBuilder.h>

#include "ast.hpp"

using namespace llvm;

void LLVMIRGenerator::Visit(const DeclStmtNode& decl_stmt) {}

void LLVMIRGenerator::Visit(const VarDeclNode& decl) {}

void LLVMIRGenerator::Visit(const ArrDeclNode& arr_decl) {}

void LLVMIRGenerator::Visit(const RecordDeclNode& struct_def) {}

void LLVMIRGenerator::Visit(const FieldNode& field) {}

void LLVMIRGenerator::Visit(const RecordVarDeclNode& struct_def) {}

void LLVMIRGenerator::Visit(const ParamNode& parameter) {}

void LLVMIRGenerator::Visit(const FuncDefNode& func_def) {
  auto i32 = builder_.getInt32Ty();
  auto prototype = FunctionType::get(i32, false);
  Function* fn = Function::Create(prototype, Function::ExternalLinkage,
                                  func_def.id, module_.get());
  BasicBlock* body = BasicBlock::Create(*context_, "body", fn);
  builder_.SetInsertPoint(body);
}

void LLVMIRGenerator::Visit(const LoopInitNode& loop_init) {}

void LLVMIRGenerator::Visit(const CompoundStmtNode& compound_stmt) {}

void LLVMIRGenerator::Visit(const ProgramNode& program) {
  // Generate builtin print function.
  auto i32 = builder_.getInt32Ty();
  auto prototype = FunctionType::get(i32, false);
  Function* main_fn = Function::Create(prototype, Function::ExternalLinkage,
                                       "__builtin_print__", module_.get());
  BasicBlock* body = BasicBlock::Create(*context_, "body", main_fn);
  builder_.SetInsertPoint(body);

  for (const auto& func_def : program.func_def_list) {
    func_def->Accept(*this);
  }
}

void LLVMIRGenerator::Visit(const IfStmtNode& if_stmt) {}

void LLVMIRGenerator::Visit(const WhileStmtNode& while_stmt) {}

void LLVMIRGenerator::Visit(const ForStmtNode& for_stmt) {}

void LLVMIRGenerator::Visit(const ReturnStmtNode& ret_stmt) {}

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

void LLVMIRGenerator::Visit(const IntConstExprNode& int_expr) {}

void LLVMIRGenerator::Visit(const ArgExprNode& arg_expr) {}

void LLVMIRGenerator::Visit(const ArrSubExprNode& arr_sub_expr) {}

void LLVMIRGenerator::Visit(const CondExprNode& cond_expr) {}

void LLVMIRGenerator::Visit(const FuncCallExprNode& call_expr) {}

void LLVMIRGenerator::Visit(const PostfixArithExprNode& postfix_expr) {}

void LLVMIRGenerator::Visit(const UnaryExprNode& unary_expr) {}

void LLVMIRGenerator::Visit(const BinaryExprNode& bin_expr) {}

void LLVMIRGenerator::Visit(const SimpleAssignmentExprNode& assign_expr) {}