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
  /// @return The mathematical symbol of the binary or unary operator, e.g.,
  /// `+`, `&`.
  std::string OpOf(const ExprNode& bin_expr);

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
            << ExprTypeToString(decl.type);
  if (decl.init) {
    std::cout << " =" << std::endl;
    indenter_.IncreaseLevel();
    decl.init->Accept(*this);
    indenter_.DecreaseLevel();
  }
  std::cout << indenter_.Indent() << ')' << std::endl;
}

void AstDumper::Visit(const LoopInitNode& loop_init) {
  if (std::holds_alternative<std::unique_ptr<DeclNode>>(loop_init.clause)) {
    std::get<std::unique_ptr<DeclNode>>(loop_init.clause)->Accept(*this);
  } else {
    std::get<std::unique_ptr<ExprNode>>(loop_init.clause)->Accept(*this);
  }
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

void AstDumper::Visit(const WhileStmtNode& while_stmt) {
  if (while_stmt.is_do_while) {
    std::cout << indenter_.Indent() << "(do" << std::endl;
    indenter_.IncreaseLevel();
    while_stmt.loop_body->Accept(*this);
    indenter_.DecreaseLevel();
    std::cout << indenter_.Indent() << ')' << std::endl;
  }
  std::cout << indenter_.Indent() << "(while" << std::endl;
  indenter_.IncreaseLevel();
  while_stmt.predicate->Accept(*this);
  if (!while_stmt.is_do_while) {
    while_stmt.loop_body->Accept(*this);
  }
  indenter_.DecreaseLevel();
  std::cout << indenter_.Indent() << ')' << std::endl;
}

void AstDumper::Visit(const ForStmtNode& for_stmt) {
  std::cout << indenter_.Indent() << "(for" << std::endl;
  indenter_.IncreaseLevel();
  for_stmt.loop_init->Accept(*this);
  for_stmt.predicate->Accept(*this);
  for_stmt.step->Accept(*this);
  for_stmt.loop_body->Accept(*this);
  indenter_.DecreaseLevel();
  std::cout << indenter_.Indent() << ')' << std::endl;
}

void AstDumper::Visit(const ReturnStmtNode& ret_stmt) {
  std::cout << indenter_.Indent() << "(ret" << std::endl;
  indenter_.IncreaseLevel();
  ret_stmt.expr->Accept(*this);
  indenter_.DecreaseLevel();
  std::cout << indenter_.Indent() << ')' << std::endl;
}

void AstDumper::Visit(const BreakStmtNode& break_stmt) {
  std::cout << indenter_.Indent() << "(break)" << std::endl;
}

void AstDumper::Visit(const ContinueStmtNode& continue_stmt) {
  std::cout << indenter_.Indent() << "(continue)" << std::endl;
}

void AstDumper::Visit(const ExprStmtNode& expr_stmt) {
  expr_stmt.expr->Accept(*this);
}

void AstDumper::Visit(const NullExprNode& null_expr) {
  std::cout << indenter_.Indent() << "()" << std::endl;
}

void AstDumper::Visit(const IdExprNode& id_expr) {
  std::cout << indenter_.Indent() << id_expr.id << ": "
            << ExprTypeToString(id_expr.type) << std::endl;
}

void AstDumper::Visit(const IntConstExprNode& int_expr) {
  std::cout << indenter_.Indent() << int_expr.val << ": "
            << ExprTypeToString(int_expr.type) << std::endl;
}

void AstDumper::Visit(const UnaryExprNode& unary_expr) {
  std::cout << indenter_.Indent() << '(' << OpGetter{}.OpOf(unary_expr)
            << std::endl;
  indenter_.IncreaseLevel();
  unary_expr.operand->Accept(*this);
  indenter_.DecreaseLevel();
  std::cout << indenter_.Indent() << ')' << ": "
            << ExprTypeToString(unary_expr.type) << std::endl;
}

void AstDumper::Visit(const BinaryExprNode& bin_expr) {
  std::cout << indenter_.Indent() << '(' << OpGetter{}.OpOf(bin_expr)
            << std::endl;
  indenter_.IncreaseLevel();
  bin_expr.lhs->Accept(*this);
  bin_expr.rhs->Accept(*this);
  indenter_.DecreaseLevel();
  std::cout << indenter_.Indent() << ')' << ": "
            << ExprTypeToString(bin_expr.type) << std::endl;
}

/// @brief Dispatch the concrete binary or unary expressions to the parent
/// `BinaryExprNode` or `UnaryExprNode`.
/// @param classname A subclass of `BinaryExprNode` or `UnaryExprNode`.
#define DISPATCH_TO_VISIT_EXPR(parentname, classname) \
  void AstDumper::Visit(const classname& expr) { \
    Visit(static_cast<const parentname&>(expr)); \
  }

DISPATCH_TO_VISIT_EXPR(BinaryExprNode, PlusExprNode);
DISPATCH_TO_VISIT_EXPR(BinaryExprNode, SubExprNode);
DISPATCH_TO_VISIT_EXPR(BinaryExprNode, MulExprNode);
DISPATCH_TO_VISIT_EXPR(BinaryExprNode, DivExprNode);
DISPATCH_TO_VISIT_EXPR(BinaryExprNode, ModExprNode);
DISPATCH_TO_VISIT_EXPR(BinaryExprNode, GreaterThanExprNode);
DISPATCH_TO_VISIT_EXPR(BinaryExprNode, GreaterThanOrEqualToExprNode);
DISPATCH_TO_VISIT_EXPR(BinaryExprNode, LessThanExprNode);
DISPATCH_TO_VISIT_EXPR(BinaryExprNode, LessThanOrEqualToExprNode);
DISPATCH_TO_VISIT_EXPR(BinaryExprNode, EqualToExprNode);
DISPATCH_TO_VISIT_EXPR(BinaryExprNode, NotEqualToExprNode);

DISPATCH_TO_VISIT_EXPR(UnaryExprNode, IncrExprNode);
DISPATCH_TO_VISIT_EXPR(UnaryExprNode, DecrExprNode);
DISPATCH_TO_VISIT_EXPR(UnaryExprNode, NegExprNode);
DISPATCH_TO_VISIT_EXPR(UnaryExprNode, AddrExprNode);
DISPATCH_TO_VISIT_EXPR(UnaryExprNode, DereferExprNode);
DISPATCH_TO_VISIT_EXPR(UnaryExprNode, NotExprNode);
DISPATCH_TO_VISIT_EXPR(UnaryExprNode, BitCompExprNode);

#undef DISPATCH_TO_VISIT_EXPR

void AstDumper::Visit(const SimpleAssignmentExprNode& assign_expr) {
  std::cout << indenter_.Indent() << '(' << '=' << std::endl;
  indenter_.IncreaseLevel();
  std::cout << indenter_.Indent() << assign_expr.id << ": "
            << ExprTypeToString(assign_expr.type) << std::endl;
  assign_expr.expr->Accept(*this);
  indenter_.DecreaseLevel();
  std::cout << indenter_.Indent() << ')' << ": "
            << ExprTypeToString(assign_expr.expr->type) << std::endl;
}

class OpGetter::OpGetterImpl : public NonModifyingVisitor {
 public:
  std::string Op() const {
    return op_;
  }

  void Visit(const IncrExprNode&) override {
    op_ = "++";
  }

  void Visit(const DecrExprNode&) override {
    op_ = "--";
  }

  void Visit(const NegExprNode&) override {
    op_ = "-";
  }

  void Visit(const AddrExprNode&) override {
    op_ = "&";
  }

  void Visit(const DereferExprNode&) override {
    op_ = "*";
  }

  void Visit(const NotExprNode&) override {
    op_ = "!";
  }

  void Visit(const BitCompExprNode&) override {
    op_ = "~";
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

std::string OpGetter::OpOf(const ExprNode& expr) {
  expr.Accept(*impl_);
  return impl_->Op();
}

OpGetter::OpGetter() : impl_{std::make_unique<OpGetterImpl>()} {}
