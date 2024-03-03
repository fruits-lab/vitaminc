#ifndef TYPE_CHECKER_HPP_
#define TYPE_CHECKER_HPP_

#include "ast.hpp"
#include "scope.hpp"
#include "visitor.hpp"

/// @brief A modifying pass; resolves the type of expressions.
class TypeChecker : public ModifyingVisitor {
 public:
  TypeChecker(ScopeStack& env) : env_{env} {}

  void Visit(LoopInitNode&) override;
  void Visit(DeclNode&) override;
  void Visit(FuncDefNode&) override;
  void Visit(CompoundStmtNode&) override;
  void Visit(ProgramNode&) override;
  void Visit(IfStmtNode&) override;
  void Visit(WhileStmtNode&) override;
  void Visit(ForStmtNode&) override;
  void Visit(ReturnStmtNode&) override;
  void Visit(BreakStmtNode&) override;
  void Visit(ContinueStmtNode&) override;
  void Visit(SwitchStmtNode&) override;
  void Visit(CaseStmtNode&) override;
  void Visit(DefaultStmtNode&) override;
  void Visit(ExprStmtNode&) override;
  void Visit(NullExprNode&) override;
  void Visit(IdExprNode&) override;
  void Visit(IntConstExprNode&) override;
  void Visit(FunCallExprNode&) override;
  void Visit(UnaryExprNode&) override;
  void Visit(BinaryExprNode&) override;
  void Visit(SimpleAssignmentExprNode&) override;

 private:
  ScopeStack& env_;
};

#endif  // TYPE_CHECKER_HPP_
