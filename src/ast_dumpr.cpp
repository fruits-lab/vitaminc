#include <iostream>

#include "ast.hpp"
#include "ast_dumper.hpp"
#include "type.hpp"
#include "util.hpp"

namespace {

auto indenter = Indenter{' ', 2, 80};

}  // namespace

void AstDumper::Visit(const DeclNode& decl) {
  std::cout << indenter.Indent() << '(' << decl.id_ << ": "
            << ExprTypeToCString(decl.type_);
  if (decl.init_) {
    std::cout << " =" << std::endl;
    indenter.IncreaseLevel();
    decl.init_->Accept(*this);
    indenter.DecreaseLevel();
  }
  std::cout << ')' << std::endl;
}

void AstDumper::Visit(const BlockStmtNode& block) {
  for (const auto& decl : block.decls_) {
    decl->Accept(*this);
  }
  for (const auto& stmt : block.stmts_) {
    stmt->Accept(*this);
  }
}

void AstDumper::Visit(const ProgramNode& program) {
  program.block_->Accept(*this);
}

void AstDumper::Visit(const NullStmtNode& stmt) {
  std::cout << indenter.Indent() << "()" << std::endl;
}

void AstDumper::Visit(const ReturnStmtNode& ret_stmt) {
  std::cout << indenter.Indent() << "(ret" << std::endl;
  indenter.IncreaseLevel();
  ret_stmt.expr_->Accept(*this);
  indenter.DecreaseLevel();
  std::cout << indenter.Indent() << ')' << std::endl;
}

void AstDumper::Visit(const ExprStmtNode& expr_stmt) {
  expr_stmt.expr_->Accept(*this);
}

void AstDumper::Visit(const IdExprNode& id_expr) {
  std::cout << indenter.Indent() << id_expr.id_ << ": "
            << ExprTypeToCString(id_expr.type) << std::endl;
}

void AstDumper::Visit(const IntConstExprNode& int_expr) {
  std::cout << indenter.Indent() << int_expr.val_ << ": "
            << ExprTypeToCString(int_expr.type) << std::endl;
}

void AstDumper::Visit(const BinaryExprNode& bin_expr) {
  std::cout << indenter.Indent() << '(' << bin_expr.Op_() << std::endl;
  indenter.IncreaseLevel();
  bin_expr.lhs_->Accept(*this);
  bin_expr.rhs_->Accept(*this);
  indenter.DecreaseLevel();
  std::cout << indenter.Indent() << ')' << ": "
            << ExprTypeToCString(bin_expr.type) << std::endl;
}

/// @brief Dispatch the concrete binary expressions to the parent
/// `BinaryExprNode`.
/// @param classname A subclass of `BinaryExprNode`.
#define DISPATCH_TO_VISIT_BINARY_EXPR(classname) \
  void AstDumper::Visit(const classname& expr) { \
    Visit(static_cast<const BinaryExprNode&>(expr)); \
  }

DISPATCH_TO_VISIT_BINARY_EXPR(PlusExprNode);
DISPATCH_TO_VISIT_BINARY_EXPR(SubExprNode);
DISPATCH_TO_VISIT_BINARY_EXPR(MulExprNode);
DISPATCH_TO_VISIT_BINARY_EXPR(DivExprNode);
DISPATCH_TO_VISIT_BINARY_EXPR(ModExprNode);
DISPATCH_TO_VISIT_BINARY_EXPR(GreaterThanExprNode);
DISPATCH_TO_VISIT_BINARY_EXPR(GreaterThanOrEqualToExprNode);
DISPATCH_TO_VISIT_BINARY_EXPR(LessThanExprNode);
DISPATCH_TO_VISIT_BINARY_EXPR(LessThanOrEqualToExprNode);
DISPATCH_TO_VISIT_BINARY_EXPR(EqualToExprNode);
DISPATCH_TO_VISIT_BINARY_EXPR(NotEqualToExprNode);

#undef DISPATCH_TO_VISIT_BINARY_EXPR

void AstDumper::Visit(const SimpleAssignmentExprNode& assign_expr) {
  std::cout << indenter.Indent() << '(' << '=' << std::endl;
  indenter.IncreaseLevel();
  std::cout << indenter.Indent() << assign_expr.id_ << ": "
            << ExprTypeToCString(assign_expr.type) << std::endl;
  assign_expr.expr_->Accept(*this);
  indenter.DecreaseLevel();
  std::cout << indenter.Indent() << ')' << ": "
            << ExprTypeToCString(assign_expr.expr_->type) << std::endl;
}
