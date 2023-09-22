#include <cstddef>
#include <iostream>
#include <string>

#include "ast.hpp"
#include "ast_dumper.hpp"
#include "type.hpp"

namespace {

class Indenter {
 public:
  /// @return `size_per_level` * `level` number of `symbol`s.
  std::string Indent() const {
    return std::string(size_per_level_ * level_, symbol_);
  }

  void IncreaseLevel() {
    if (level_ == 0 /* no limit */ || level_ < max_level_) {
      ++level_;
    }
  }

  void DecreaseLevel() {
    if (level_) {
      --level_;
    }
  }

  /// @param symbol The symbol used to indent with.
  /// @param size_per_level The indention size. For example, if the size is `2`,
  /// each indention level adds 2 `symbol`s.
  /// @param max_level If equals to `0`, there is no limit.
  Indenter(char symbol, std::size_t size_per_level, std::size_t max_level = 0)
      : symbol_{symbol},
        size_per_level_{size_per_level},
        max_level_{max_level} {}

 private:
  char symbol_;
  std::size_t size_per_level_;
  std::size_t max_level_;
  std::size_t level_{0};
};

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
