#include "ast_dumper.hpp"

#include <iostream>
#include <memory>
#include <string>

#include "ast.hpp"
#include "type.hpp"
#include "visitor.hpp"

namespace {
class OpGetter {
 public:
  /// @return The mathematical symbol of the binary operator, e.g., `+`.
  std::string OpOf(const BinaryExprNode& bin_expr);

  OpGetter();

 private:
  /// @note An alternative approach would be to directly implement `OpGetter` as
  /// a `Visitor`, but this is intended to be used exclusively with binary
  /// expressions. Therefore, we encapsulate it to prevent unintended usage in
  /// other contexts. We also employ the Pimpl idiom, allowing deferred
  /// implementation details later in this file.
  class OpGetterImpl;
  std::unique_ptr<OpGetterImpl> impl_;
};

}  // namespace

void AstDumper::Visit(const DeclNode& decl) {
  std::cout << indenter_.Indent() << '(' << decl.id << ": "
            << ExprTypeToCString(decl.type);
  if (decl.init) {
    std::cout << " =" << std::endl;
    indenter_.IncreaseLevel();
    decl.init->Accept(*this);
    indenter_.DecreaseLevel();
  }
  std::cout << ')' << std::endl;
}

void AstDumper::Visit(const BlockStmtNode& block) {
  for (const auto& decl : block.decls) {
    decl->Accept(*this);
  }
  for (const auto& stmt : block.stmts) {
    stmt->Accept(*this);
  }
}

void AstDumper::Visit(const ProgramNode& program) {
  program.block->Accept(*this);
}

void AstDumper::Visit(const NullStmtNode& stmt) {
  std::cout << indenter_.Indent() << "()" << std::endl;
}

void AstDumper::Visit(const IfStmtNode& if_stmt) {
  std::cout << indenter_.Indent() << "(if" << std::endl;
  indenter_.IncreaseLevel();
  if_stmt.predicate->Accept(*this);
  if_stmt.then->Accept(*this);
  indenter_.DecreaseLevel();
  std::cout << indenter_.Indent() << ')' << std::endl;
  if (if_stmt.or_else) {
    std::cout << indenter_.Indent() << "(else" << std::endl;
    indenter_.IncreaseLevel();
    if_stmt.or_else->Accept(*this);
    indenter_.DecreaseLevel();
    std::cout << indenter_.Indent() << ')' << std::endl;
  }
}

void AstDumper::Visit(const ReturnStmtNode& ret_stmt) {
  std::cout << indenter_.Indent() << "(ret" << std::endl;
  indenter_.IncreaseLevel();
  ret_stmt.expr->Accept(*this);
  indenter_.DecreaseLevel();
  std::cout << indenter_.Indent() << ')' << std::endl;
}

void AstDumper::Visit(const ExprStmtNode& expr_stmt) {
  expr_stmt.expr->Accept(*this);
}

void AstDumper::Visit(const IdExprNode& id_expr) {
  std::cout << indenter_.Indent() << id_expr.id << ": "
            << ExprTypeToCString(id_expr.type) << std::endl;
}

void AstDumper::Visit(const IntConstExprNode& int_expr) {
  std::cout << indenter_.Indent() << int_expr.val << ": "
            << ExprTypeToCString(int_expr.type) << std::endl;
}

void AstDumper::Visit(const BinaryExprNode& bin_expr) {
  std::cout << indenter_.Indent() << '(' << OpGetter{}.OpOf(bin_expr)
            << std::endl;
  indenter_.IncreaseLevel();
  bin_expr.lhs->Accept(*this);
  bin_expr.rhs->Accept(*this);
  indenter_.DecreaseLevel();
  std::cout << indenter_.Indent() << ')' << ": "
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
  std::cout << indenter_.Indent() << '(' << '=' << std::endl;
  indenter_.IncreaseLevel();
  std::cout << indenter_.Indent() << assign_expr.id << ": "
            << ExprTypeToCString(assign_expr.type) << std::endl;
  assign_expr.expr->Accept(*this);
  indenter_.DecreaseLevel();
  std::cout << indenter_.Indent() << ')' << ": "
            << ExprTypeToCString(assign_expr.expr->type) << std::endl;
}

class OpGetter::OpGetterImpl : public NonModifyingVisitor {
 public:
  std::string Op() const {
    return op_;
  }

  void Visit(const PlusExprNode&) override {
    op_ = "+";
  }

  void Visit(const SubExprNode&) override {
    op_ = "-";
  }

  void Visit(const MulExprNode&) override {
    op_ = "*";
  }

  void Visit(const DivExprNode&) override {
    op_ = "/";
  }

  void Visit(const ModExprNode&) override {
    op_ = "%";
  }

  void Visit(const GreaterThanExprNode&) override {
    op_ = ">";
  }

  void Visit(const GreaterThanOrEqualToExprNode&) override {
    op_ = ">=";
  }

  void Visit(const LessThanExprNode&) override {
    op_ = "<";
  }

  void Visit(const LessThanOrEqualToExprNode&) override {
    op_ = "<=";
  }

  void Visit(const EqualToExprNode&) override {
    op_ = "==";
  }

  void Visit(const NotEqualToExprNode&) override {
    op_ = "!=";
  }

 private:
  std::string op_;
};

std::string OpGetter::OpOf(const BinaryExprNode& bin_expr) {
  bin_expr.Accept(*impl_);
  return impl_->Op();
}

OpGetter::OpGetter() : impl_{std::make_unique<OpGetterImpl>()} {}
