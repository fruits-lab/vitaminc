#include "type_checker.hpp"

#include "ast.hpp"

void TypeChecker::Visit(DeclNode& decl) {
  if (decl.init_) {
    decl.init_->Accept(*this);
    if (decl.init_->type != decl.type_) {
      // TODO: incompatible types when initializing type 'type_' using type
      // 'init_->type'
    }
  }

  if (env_.Probe(decl.id_)) {
    // TODO: redefinition of 'id_'
  } else {
    auto symbol = std::make_unique<SymbolEntry>(decl.id_);
    symbol->expr_type = decl.type_;
    env_.Add(std::move(symbol));
  }
}

void TypeChecker::Visit(BlockStmtNode& block) {
  env_.PushScope();
  for (auto& decl : block.decls_) {
    decl->Accept(*this);
  }
  for (auto& stmt : block.stmts_) {
    stmt->Accept(*this);
  }
  env_.PopScope();
}

void TypeChecker::Visit(ProgramNode& program) {
  program.block_->Accept(*this);
}

void TypeChecker::Visit(NullStmtNode&) {
  /* do nothing */
}

void TypeChecker::Visit(ReturnStmtNode& ret_stmt) {
  ret_stmt.expr_->Accept(*this);
  if (ret_stmt.expr_->type != ExprType::kInt) {
    // TODO: return value type does not match the function type
  }
}

void TypeChecker::Visit(ExprStmtNode& expr_stmt) {
  expr_stmt.expr_->Accept(*this);
}

void TypeChecker::Visit(IdExprNode& id_expr) {
  if (auto symbol = env_.LookUp(id_expr.id_)) {
    id_expr.type = symbol->expr_type;
  } else {
    // TODO: 'id_' undeclared
  }
}

void TypeChecker::Visit(IntConstExprNode& int_expr) {
  int_expr.type = ExprType::kInt;
}

void TypeChecker::Visit(BinaryExprNode& bin_expr) {
  bin_expr.lhs_->Accept(*this);
  bin_expr.rhs_->Accept(*this);
  if (bin_expr.lhs_->type != bin_expr.rhs_->type) {
    // TODO: invalid operands to binary +
  } else {
    bin_expr.type = bin_expr.lhs_->type;
  }
}

/// @brief Dispatch the concrete binary expressions to the parent
/// `BinaryExprNode`.
/// @param classname A subclass of `BinaryExprNode`.
#define DISPATCH_TO_VISIT_BINARY_EXPR(classname) \
  void TypeChecker::Visit(classname& expr) { \
    Visit(static_cast<BinaryExprNode&>(expr)); \
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

void TypeChecker::Visit(SimpleAssignmentExprNode& assign_expr) {
  assign_expr.expr_->Accept(*this);
  if (auto symbol = env_.LookUp(assign_expr.id_)) {
    if (assign_expr.expr_->type == symbol->expr_type) {
      // 6.5.16 Assignment operators
      // The type of an assignment expression is the type of the left
      // operand unless the left operand has qualified type, in which case it is
      // the unqualified version of the type of the left operand.
      assign_expr.type = symbol->expr_type;
    } else {
      // TODO: assigning to 'symbol->expr_type' from incompatible type
      // 'expr_->type'
    }
  } else {
    // TODO: 'id_' undeclared
  }
}
