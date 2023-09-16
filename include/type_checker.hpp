#ifndef TYPE_CHECKER_HPP_
#define TYPE_CHECKER_HPP_

#include "scope.hpp"
#include "visitor.hpp"

/// @brief A modifying pass; resolves the type of expressions.
class TypeChecker : public ModifyingVisitor {
 public:
  TypeChecker(ScopeStack& env) : env_{env} {}

  void Visit(DeclNode&) override;
  void Visit(BlockStmtNode&) override;
  void Visit(ProgramNode&) override;
  void Visit(NullStmtNode&) override;
  void Visit(ReturnStmtNode&) override;
  void Visit(ExprStmtNode&) override;
  void Visit(IdExprNode&) override;
  void Visit(IntConstExprNode&) override;
  void Visit(BinaryExprNode&) override;
  void Visit(SimpleAssignmentExprNode&) override;

 private:
  ScopeStack& env_;
};

#endif  // TYPE_CHECKER_HPP_
