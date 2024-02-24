#ifndef QBE_IR_GENERATOR_HPP_
#define QBE_IR_GENERATOR_HPP_

#include "visitor.hpp"

class QbeIrGenerator : public NonModifyingVisitor {
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
};

#endif  // QBE_IR_GENERATOR_HPP_
