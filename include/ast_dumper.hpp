#ifndef AST_DUMPER_HPP_
#define AST_DUMPER_HPP_

#include "ast.hpp"
#include "util.hpp"
#include "visitor.hpp"

class AstDumper : public NonModifyingVisitor {
 public:
  void Visit(const LoopInitNode&) override;
  void Visit(const DeclNode&) override;
  void Visit(const BlockStmtNode&) override;
  void Visit(const ProgramNode&) override;
  void Visit(const IfStmtNode&) override;
  void Visit(const WhileStmtNode&) override;
  void Visit(const ForStmtNode&) override;
  void Visit(const ReturnStmtNode&) override;
  void Visit(const BreakStmtNode&) override;
  void Visit(const ContinueStmtNode&) override;
  void Visit(const ExprStmtNode&) override;
  void Visit(const NullExprNode&) override;
  void Visit(const IdExprNode&) override;
  void Visit(const IntConstExprNode&) override;
  void Visit(const UnaryExprNode&) override;
  void Visit(const BinaryExprNode&) override;
  void Visit(const SimpleAssignmentExprNode&) override;

  AstDumper(Indenter& indenter) : indenter_{indenter} {}

 private:
  Indenter& indenter_;
};

#endif  // AST_DUMPER_HPP_
