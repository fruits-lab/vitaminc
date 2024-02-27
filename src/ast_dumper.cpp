#include "ast_dumper.hpp"

#include <iostream>
#include <memory>
#include <string>
#include <variant>

#include "ast.hpp"
#include "type.hpp"
#include "visitor.hpp"

namespace {

std::string GetBinaryOperator(BinaryOperator op) {
  switch (op) {
    case BinaryOperator::kAdd:
      return "+";
    case BinaryOperator::kSub:
      return "-";
    case BinaryOperator::kMul:
      return "*";
    case BinaryOperator::kDiv:
      return "/";
    case BinaryOperator::kMod:
      return "%";
    case BinaryOperator::kGt:
      return ">";
    case BinaryOperator::kGte:
      return ">=";
    case BinaryOperator::kLt:
      return "<";
    case BinaryOperator::kLte:
      return "<=";
    case BinaryOperator::kEq:
      return "==";
    case BinaryOperator::kNeq:
      return "!=";
    default:
      return "Unknown";
  }
}

std::string GetUnaryOperator(UnaryOperator op) {
  switch (op) {
    case UnaryOperator::kIncr:
      return "++";
    case UnaryOperator::kDecr:
      return "--";
    case UnaryOperator::kPos:
      return "+";
    case UnaryOperator::kNeg:
      return "-";
    case UnaryOperator::kNot:
      return "!";
    case UnaryOperator::kAddr:
      return "&";
    case UnaryOperator::kDeref:
      return "*";
    case UnaryOperator::kBitComp:
      return "~";
    default:
      return "Unknown";
  }
}

}  // namespace

void AstDumper::Visit(const DeclNode& decl) {
  std::cout << indenter_.Indent() << '(' << decl.id << ": "
            << ExprTypeToString(decl.type);
  if (decl.init) {
    std::cout << " =" << '\n';
    indenter_.IncreaseLevel();
    decl.init->Accept(*this);
    indenter_.DecreaseLevel();
  }
  std::cout << indenter_.Indent() << ')' << '\n';
}

void AstDumper::Visit(const LoopInitNode& loop_init) {
  if (std::holds_alternative<std::unique_ptr<DeclNode>>(loop_init.clause)) {
    std::get<std::unique_ptr<DeclNode>>(loop_init.clause)->Accept(*this);
  } else {
    std::get<std::unique_ptr<ExprNode>>(loop_init.clause)->Accept(*this);
  }
}

void AstDumper::Visit(const CompoundStmtNode& compound_stmt) {
  for (const auto& item : compound_stmt.items) {
    std::visit([this](auto&& item) { item->Accept(*this); }, item);
  }
}

void AstDumper::Visit(const ProgramNode& program) {
  program.body->Accept(*this);
}

void AstDumper::Visit(const IfStmtNode& if_stmt) {
  std::cout << indenter_.Indent() << "(if" << '\n';
  indenter_.IncreaseLevel();
  if_stmt.predicate->Accept(*this);
  if_stmt.then->Accept(*this);
  indenter_.DecreaseLevel();
  std::cout << indenter_.Indent() << ')' << '\n';
  if (if_stmt.or_else) {
    std::cout << indenter_.Indent() << "(else" << '\n';
    indenter_.IncreaseLevel();
    if_stmt.or_else->Accept(*this);
    indenter_.DecreaseLevel();
    std::cout << indenter_.Indent() << ')' << '\n';
  }
}

void AstDumper::Visit(const WhileStmtNode& while_stmt) {
  if (while_stmt.is_do_while) {
    std::cout << indenter_.Indent() << "(do" << '\n';
    indenter_.IncreaseLevel();
    while_stmt.loop_body->Accept(*this);
    indenter_.DecreaseLevel();
    std::cout << indenter_.Indent() << ')' << '\n';
  }
  std::cout << indenter_.Indent() << "(while" << '\n';
  indenter_.IncreaseLevel();
  while_stmt.predicate->Accept(*this);
  if (!while_stmt.is_do_while) {
    while_stmt.loop_body->Accept(*this);
  }
  indenter_.DecreaseLevel();
  std::cout << indenter_.Indent() << ')' << '\n';
}

void AstDumper::Visit(const ForStmtNode& for_stmt) {
  std::cout << indenter_.Indent() << "(for" << '\n';
  indenter_.IncreaseLevel();
  for_stmt.loop_init->Accept(*this);
  for_stmt.predicate->Accept(*this);
  for_stmt.step->Accept(*this);
  for_stmt.loop_body->Accept(*this);
  indenter_.DecreaseLevel();
  std::cout << indenter_.Indent() << ')' << '\n';
}

void AstDumper::Visit(const ReturnStmtNode& ret_stmt) {
  std::cout << indenter_.Indent() << "(ret" << '\n';
  indenter_.IncreaseLevel();
  ret_stmt.expr->Accept(*this);
  indenter_.DecreaseLevel();
  std::cout << indenter_.Indent() << ')' << '\n';
}

void AstDumper::Visit(const BreakStmtNode& break_stmt) {
  std::cout << indenter_.Indent() << "(break)" << '\n';
}

void AstDumper::Visit(const ContinueStmtNode& continue_stmt) {
  std::cout << indenter_.Indent() << "(continue)" << '\n';
}

void AstDumper::Visit(const ExprStmtNode& expr_stmt) {
  expr_stmt.expr->Accept(*this);
}

void AstDumper::Visit(const NullExprNode& null_expr) {
  std::cout << indenter_.Indent() << "()" << '\n';
}

void AstDumper::Visit(const IdExprNode& id_expr) {
  std::cout << indenter_.Indent() << id_expr.id << ": "
            << ExprTypeToString(id_expr.type) << '\n';
}

void AstDumper::Visit(const IntConstExprNode& int_expr) {
  std::cout << indenter_.Indent() << int_expr.val << ": "
            << ExprTypeToString(int_expr.type) << '\n';
}

void AstDumper::Visit(const UnaryExprNode& unary_expr) {
  std::cout << indenter_.Indent() << '(' << GetUnaryOperator(unary_expr.op)
            << '\n';
  indenter_.IncreaseLevel();
  unary_expr.operand->Accept(*this);
  indenter_.DecreaseLevel();
  std::cout << indenter_.Indent() << ')' << ": "
            << ExprTypeToString(unary_expr.type) << '\n';
}

void AstDumper::Visit(const BinaryExprNode& bin_expr) {
  std::cout << indenter_.Indent() << '(' << GetBinaryOperator(bin_expr.op)
            << '\n';
  indenter_.IncreaseLevel();
  bin_expr.lhs->Accept(*this);
  bin_expr.rhs->Accept(*this);
  indenter_.DecreaseLevel();
  std::cout << indenter_.Indent() << ')' << ": "
            << ExprTypeToString(bin_expr.type) << '\n';
}

void AstDumper::Visit(const SimpleAssignmentExprNode& assign_expr) {
  std::cout << indenter_.Indent() << '(' << '=' << '\n';
  indenter_.IncreaseLevel();
  std::cout << indenter_.Indent() << assign_expr.id << ": "
            << ExprTypeToString(assign_expr.type) << '\n';
  assign_expr.expr->Accept(*this);
  indenter_.DecreaseLevel();
  std::cout << indenter_.Indent() << ')' << ": "
            << ExprTypeToString(assign_expr.expr->type) << '\n';
}
