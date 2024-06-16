#ifndef LLVM_IR_GENERATOR_HPP_
#define LLVM_IR_GENERATOR_HPP_

#include "ast.hpp"
#include "visitor.hpp"

class LLVMIRGenerator : public NonModifyingVisitor {
 public:
  void Visit(const DeclStmtNode&) override;
  void Visit(const LoopInitNode&) override;
  void Visit(const VarDeclNode&) override;
  void Visit(const ArrDeclNode&) override;
  void Visit(const RecordDeclNode&) override;
  void Visit(const FieldNode&) override;
  void Visit(const RecordVarDeclNode&) override;
  void Visit(const ParamNode&) override;
  void Visit(const FuncDefNode&) override;
  void Visit(const CompoundStmtNode&) override;
  void Visit(const ProgramNode&) override;
  void Visit(const IfStmtNode&) override;
  void Visit(const WhileStmtNode&) override;
  void Visit(const ForStmtNode&) override;
  void Visit(const ReturnStmtNode&) override;
  void Visit(const GotoStmtNode&) override;
  void Visit(const BreakStmtNode&) override;
  void Visit(const ContinueStmtNode&) override;
  void Visit(const SwitchStmtNode&) override;
  void Visit(const IdLabeledStmtNode&) override;
  void Visit(const CaseStmtNode&) override;
  void Visit(const DefaultStmtNode&) override;
  void Visit(const ExprStmtNode&) override;
  void Visit(const InitExprNode&) override;
  void Visit(const ArrDesNode&) override;
  void Visit(const IdDesNode&) override;
  void Visit(const NullExprNode&) override;
  void Visit(const IdExprNode&) override;
  void Visit(const IntConstExprNode&) override;
  void Visit(const ArgExprNode&) override;
  void Visit(const ArrSubExprNode&) override;
  void Visit(const CondExprNode&) override;
  void Visit(const FuncCallExprNode&) override;
  void Visit(const PostfixArithExprNode&) override;
  void Visit(const UnaryExprNode&) override;
  void Visit(const BinaryExprNode&) override;
  void Visit(const SimpleAssignmentExprNode&) override;

  LLVMIRGenerator(std::ostream& output) : output_{output} {}

 private:
  std::ostream& output_;
};

#endif  // LLVM_IR_GENERATOR_HPP_