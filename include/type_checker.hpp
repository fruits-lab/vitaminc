#ifndef TYPE_CHECKER_HPP_
#define TYPE_CHECKER_HPP_

#include "ast.hpp"
#include "scope.hpp"
#include "visitor.hpp"

/// @brief A modifying pass; resolves the type of expressions.
class TypeChecker : public ModifyingVisitor {
 public:
  TypeChecker(ScopeStack& env) : env_{env} {}

  void Visit(DeclStmtNode&) override;
  void Visit(LoopInitNode&) override;
  void Visit(VarDeclNode&) override;
  void Visit(ArrDeclNode&) override;
  void Visit(RecordDeclNode&) override;
  void Visit(FieldNode&) override;
  void Visit(RecordVarDeclNode&) override;
  void Visit(ParamNode&) override;
  void Visit(FuncDefNode&) override;
  void Visit(CompoundStmtNode&) override;
  void Visit(ProgramNode&) override;
  void Visit(IfStmtNode&) override;
  void Visit(WhileStmtNode&) override;
  void Visit(ForStmtNode&) override;
  void Visit(ReturnStmtNode&) override;
  void Visit(GotoStmtNode&) override;
  void Visit(BreakStmtNode&) override;
  void Visit(ContinueStmtNode&) override;
  void Visit(SwitchStmtNode&) override;
  void Visit(IdLabeledStmtNode&) override;
  void Visit(CaseStmtNode&) override;
  void Visit(DefaultStmtNode&) override;
  void Visit(ExprStmtNode&) override;
  void Visit(InitExprNode&) override;
  void Visit(ArrDesNode&) override;
  void Visit(IdDesNode&) override;
  void Visit(NullExprNode&) override;
  void Visit(IdExprNode&) override;
  void Visit(IntConstExprNode&) override;
  void Visit(ArgExprNode&) override;
  void Visit(ArrSubExprNode&) override;
  void Visit(CondExprNode&) override;
  void Visit(FuncCallExprNode&) override;
  void Visit(PostfixArithExprNode&) override;
  void Visit(RecordMemExprNode&) override;
  void Visit(UnaryExprNode&) override;
  void Visit(BinaryExprNode&) override;
  void Visit(SimpleAssignmentExprNode&) override;

 private:
  ScopeStack& env_;

  /// @brief Installs the built-in functions into the environment.
  void InstallBuiltins_(ScopeStack&);
};

#endif  // TYPE_CHECKER_HPP_
