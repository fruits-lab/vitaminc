#ifndef AST_DUMPER_HPP_
#define AST_DUMPER_HPP_

#include "util.hpp"
#include "visitor.hpp"

class AstDumper : public NonModifyingVisitor {
 public:
  void Visit(const DeclNode&) override;
  void Visit(const BlockStmtNode&) override;
  void Visit(const ProgramNode&) override;
  void Visit(const NullStmtNode&) override;
  void Visit(const IfStmtNode&) override;
  void Visit(const ReturnStmtNode&) override;
  void Visit(const ExprStmtNode&) override;
  void Visit(const IdExprNode&) override;
  void Visit(const IntConstExprNode&) override;
  void Visit(const BinaryExprNode&) override;
  void Visit(const PlusExprNode&) override;
  void Visit(const SubExprNode&) override;
  void Visit(const MulExprNode&) override;
  void Visit(const DivExprNode&) override;
  void Visit(const ModExprNode&) override;
  void Visit(const GreaterThanExprNode&) override;
  void Visit(const GreaterThanOrEqualToExprNode&) override;
  void Visit(const LessThanExprNode&) override;
  void Visit(const LessThanOrEqualToExprNode&) override;
  void Visit(const EqualToExprNode&) override;
  void Visit(const NotEqualToExprNode&) override;
  void Visit(const SimpleAssignmentExprNode&) override;

  AstDumper(Indenter& indenter) : indenter_{indenter} {}

 private:
  Indenter& indenter_;
};

#endif  // AST_DUMPER_HPP_
