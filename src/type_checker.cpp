#include "type_checker.hpp"

#include <memory>
#include <variant>

#include "ast.hpp"

void TypeChecker::Visit(DeclNode& decl) {
  if (decl.init) {
    decl.init->Accept(*this);
    if (decl.init->type != decl.type) {
      // TODO: incompatible types when initializing type 'type' using type
      // 'init->type'
    }
  }

  if (env_.Probe(decl.id)) {
    // TODO: redefinition of 'id'
  } else {
    auto symbol = std::make_unique<SymbolEntry>(decl.id);
    symbol->expr_type = decl.type;
    env_.Add(std::move(symbol));
  }
}

void TypeChecker::Visit(LoopInitNode& loop_init) {
  if (std::holds_alternative<std::unique_ptr<DeclNode>>(loop_init.clause)) {
    std::get<std::unique_ptr<DeclNode>>(loop_init.clause)->Accept(*this);
  } else {
    std::get<std::unique_ptr<ExprNode>>(loop_init.clause)->Accept(*this);
  }
}

void TypeChecker::Visit(BlockStmtNode& block) {
  env_.PushScope();
  for (auto& decl : block.decls) {
    decl->Accept(*this);
  }
  for (auto& stmt : block.stmts) {
    stmt->Accept(*this);
  }
  env_.PopScope();
}

void TypeChecker::Visit(ProgramNode& program) {
  program.block->Accept(*this);
}

void TypeChecker::Visit(IfStmtNode& if_stmt) {
  if_stmt.predicate->Accept(*this);
  if_stmt.then->Accept(*this);
  if (if_stmt.or_else) {
    if_stmt.or_else->Accept(*this);
  }
}

void TypeChecker::Visit(WhileStmtNode& while_stmt) {
  while_stmt.predicate->Accept(*this);
  while_stmt.loop_body->Accept(*this);
}

void TypeChecker::Visit(ForStmtNode& for_stmt) {
  for_stmt.loop_init->Accept(*this);
  for_stmt.predicate->Accept(*this);
  for_stmt.step->Accept(*this);
  for_stmt.loop_body->Accept(*this);
}

void TypeChecker::Visit(ReturnStmtNode& ret_stmt) {
  ret_stmt.expr->Accept(*this);
  if (ret_stmt.expr->type != ExprType::kInt) {
    // TODO: return value type does not match the function type
  }
}

void TypeChecker::Visit(BreakStmtNode& break_stmt) {
  // TODO: check if it is in a loop body or a switch body
}

void TypeChecker::Visit(ContinueStmtNode& continue_stmt) {
  // TODO: check if it is in a loop body
}

void TypeChecker::Visit(ExprStmtNode& expr_stmt) {
  expr_stmt.expr->Accept(*this);
}

void TypeChecker::Visit(NullExprNode&) {
  /* do nothing */
}

void TypeChecker::Visit(IdExprNode& id_expr) {
  if (auto symbol = env_.LookUp(id_expr.id)) {
    id_expr.type = symbol->expr_type;
  } else {
    // TODO: 'id' undeclared
  }
}

void TypeChecker::Visit(IntConstExprNode& int_expr) {
  int_expr.type = ExprType::kInt;
}

void TypeChecker::Visit(UnaryExprNode& unary_expr) {
  unary_expr.operand->Accept(*this);
  unary_expr.type = unary_expr.operand->type;
  // TODO: check operands type
}

void TypeChecker::Visit(BinaryExprNode& bin_expr) {
  bin_expr.lhs->Accept(*this);
  bin_expr.rhs->Accept(*this);
  if (bin_expr.lhs->type != bin_expr.rhs->type) {
    // TODO: invalid operands to binary +
  } else {
    bin_expr.type = bin_expr.lhs->type;
  }
}

void TypeChecker::Visit(SimpleAssignmentExprNode& assign_expr) {
  assign_expr.expr->Accept(*this);
  if (auto symbol = env_.LookUp(assign_expr.id)) {
    if (assign_expr.expr->type == symbol->expr_type) {
      // 6.5.16 Assignment operators
      // The type of an assignment expression is the type of the left
      // operand unless the left operand has qualified type, in which case it
      // is the unqualified version of the type of the left operand.
      assign_expr.type = symbol->expr_type;
    } else {
      // TODO: assigning to 'symbol->expr_type' from incompatible type
      // 'expr->type'
    }
  } else {
    // TODO: 'id' undeclared
  }
}
