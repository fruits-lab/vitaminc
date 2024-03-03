#include "type_checker.hpp"

#include <algorithm>
#include <cassert>
#include <cstdint>
#include <memory>
#include <utility>
#include <variant>
#include <vector>

#include "ast.hpp"
#include "symbol.hpp"
#include "type.hpp"

namespace {

/// @brief Some statements can only appear in body of certain constructs, namely
/// the return, break, and continue statements.
enum class BodyType : std::uint8_t {
  /// @brief No special semantics.
  kNone = 0,
  kLoop,
  kSwitch,
};

/// @note Constructs that enters a body (compound statement) should add their
/// body type to this list.
auto body_types  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)
    = std::vector<BodyType>{};

bool IsInBodyOf(BodyType type) {
  return std::any_of(body_types.cbegin(), body_types.cend(),
                     [type](auto&& t) { return t == type; });
}

}  // namespace

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

void TypeChecker::Visit(FuncDefNode& func_def) {
  if (env_.Probe(func_def.id)) {
    // TODO: redefinition of function id
  } else {
    auto symbol = std::make_unique<SymbolEntry>(func_def.id);
    symbol->expr_type = func_def.return_type;
    env_.Add(std::move(symbol));
  }

  func_def.body->Accept(*this);
  //  TODO: check body return type and function return type
}

void TypeChecker::Visit(LoopInitNode& loop_init) {
  std::visit([this](auto&& clause) { clause->Accept(*this); },
             loop_init.clause);
}

void TypeChecker::Visit(CompoundStmtNode& compound_stmt) {
  env_.PushScope();
  for (auto& item : compound_stmt.items) {
    std::visit([this](auto&& item) { item->Accept(*this); }, item);
  }
  env_.PopScope();
}

void TypeChecker::Visit(ProgramNode& program) {
  env_.PushScope();
  bool has_main_func = false;
  for (auto& func_def : program.func_def_list) {
    if (func_def->id == "main") {
      has_main_func = true;
    }
    func_def->Accept(*this);
  }

  if (!has_main_func) {
    // TODO: no main function
  }
  env_.PopScope();
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
  body_types.push_back(BodyType::kLoop);
  while_stmt.loop_body->Accept(*this);
  body_types.pop_back();
}

void TypeChecker::Visit(ForStmtNode& for_stmt) {
  for_stmt.loop_init->Accept(*this);
  for_stmt.predicate->Accept(*this);
  for_stmt.step->Accept(*this);
  body_types.push_back(BodyType::kLoop);
  for_stmt.loop_body->Accept(*this);
  body_types.pop_back();
}

void TypeChecker::Visit(ReturnStmtNode& ret_stmt) {
  ret_stmt.expr->Accept(*this);
  if (ret_stmt.expr->type != ExprType::kInt) {
    // TODO: return value type does not match the function type
  }
}

void TypeChecker::Visit(BreakStmtNode& break_stmt) {
  // TODO: or a switch body
  if (!IsInBodyOf(BodyType::kLoop)) {
    assert(false);
    // TODO: 'break' statement not in loop or switch statement
  }
}

void TypeChecker::Visit(ContinueStmtNode& continue_stmt) {
  if (!IsInBodyOf(BodyType::kLoop)) {
    assert(false);
    // TODO: 'continue' statement not in loop statement
  }
}

void TypeChecker::Visit(SwitchStmtNode& switch_stmt) {
  switch_stmt.ctrl->Accept(*this);
  if (switch_stmt.ctrl->type != ExprType::kInt) {
    // TODO: statement requires expression of integer type
  }
  body_types.push_back(BodyType::kSwitch);
  switch_stmt.stmt->Accept(*this);
  body_types.pop_back();
  // TODO: No two of the case constant expressions in the same switch statement
  // shall have the same value (we need constant expression support on this).
  // TODO: At most one default label in a switch statement.
}

void TypeChecker::Visit(CaseStmtNode& case_stmt) {
  if (!IsInBodyOf(BodyType::kSwitch)) {
    // TODO: 'case' statement not in switch statement
  }
  case_stmt.expr->Accept(*this);
  if (case_stmt.expr->type != ExprType::kInt) {
    // TODO: expression is not an integer constant expression
  }
  case_stmt.stmt->Accept(*this);
}

void TypeChecker::Visit(DefaultStmtNode& default_stmt) {
  if (!IsInBodyOf(BodyType::kSwitch)) {
    // TODO: 'default' statement not in switch statement
  }
  default_stmt.stmt->Accept(*this);
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

void TypeChecker::Visit(FunCallExprNode& call_expr) {
  call_expr.func_expr->Accept(*this);
  call_expr.type = call_expr.func_expr->type;
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
