#include "type_checker.hpp"

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <utility>
#include <variant>
#include <vector>

#include "ast.hpp"
#include "operator.hpp"
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
    auto symbol = std::make_unique<SymbolEntry>(decl.id, decl.type);
    env_.Add(std::move(symbol));
  }
}

void TypeChecker::Visit(ParamNode& parameter) {
  if (env_.Probe(parameter.id)) {
    // TODO: redefinition of 'id'
  } else {
    auto symbol = std::make_unique<SymbolEntry>(parameter.id, parameter.type);
    env_.Add(std::move(symbol));
  }
}

void TypeChecker::Visit(FuncDefNode& func_def) {
  if (env_.Probe(func_def.id)) {
    // TODO: redefinition of function id
  } else {
    auto symbol = std::make_unique<SymbolEntry>(func_def.id, func_def.type);
    for (auto& parameter : func_def.parameters) {
      parameter->Accept(*this);
      symbol->param_types.push_back(parameter->type);
    }

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

void TypeChecker::InstallBuiltins_(ScopeStack& env) {
  // The supported builtins are:
  // - int __builtin_print(int)

  auto symbol =
      std::make_unique<SymbolEntry>("__builtin_print", PrimitiveType::kInt);
  symbol->param_types.emplace_back(PrimitiveType::kInt);
  env.Add(std::move(symbol));
}

void TypeChecker::Visit(ProgramNode& program) {
  env_.PushScope();
  InstallBuiltins_(env_);
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
  if (ret_stmt.expr->type != PrimitiveType::kInt) {
    // TODO: return value type does not match the function type
  }
}

void TypeChecker::Visit(GotoStmtNode& goto_stmt) {
  // FIXME: To allow forward reference, we assume the label is declared.
}

void TypeChecker::Visit(BreakStmtNode& break_stmt) {
  if (!IsInBodyOf(BodyType::kLoop) && !IsInBodyOf(BodyType::kSwitch)) {
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

namespace {
/// @brief A shared state to convey the presence of a default label in a switch
/// statement.
/// @note To allow nested switch statements, the state is stacked.
auto
    switch_already_has_default  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)
    = std::vector<bool>{};
}  // namespace

void TypeChecker::Visit(SwitchStmtNode& switch_stmt) {
  switch_stmt.ctrl->Accept(*this);
  if (switch_stmt.ctrl->type != PrimitiveType::kInt) {
    // TODO: statement requires expression of integer type
  }
  body_types.push_back(BodyType::kSwitch);
  switch_already_has_default.push_back(false);
  switch_stmt.stmt->Accept(*this);
  switch_already_has_default.pop_back();
  body_types.pop_back();
  // TODO: No two of the case constant expressions in the same switch statement
  // shall have the same value (we need constant expression support on this).
}

void TypeChecker::Visit(IdLabeledStmtNode& id_labeled_stmt) {
  // TODO: To allow forward reference, the label has to be added in some other
  // way.
  // TODO: The label has function scope.
  // FIXME: Labels have error type.
  env_.Add(std::make_unique<SymbolEntry>(id_labeled_stmt.label,
                                         PrimitiveType::kUnknown));
  id_labeled_stmt.stmt->Accept(*this);
}

void TypeChecker::Visit(CaseStmtNode& case_stmt) {
  if (!IsInBodyOf(BodyType::kSwitch)) {
    // TODO: 'case' statement not in switch statement
  }
  case_stmt.expr->Accept(*this);
  if (case_stmt.expr->type != PrimitiveType::kInt) {
    // TODO: expression is not an integer constant expression
  }
  case_stmt.stmt->Accept(*this);
}

void TypeChecker::Visit(DefaultStmtNode& default_stmt) {
  if (!IsInBodyOf(BodyType::kSwitch)) {
    // TODO: 'default' statement not in switch statement
  }
  assert(!switch_already_has_default.empty());
  if (switch_already_has_default.back()) {
    // TODO: multiple default labels in one switch
  }
  switch_already_has_default.back() = true;
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
  int_expr.type = Type{PrimitiveType::kInt};
}

void TypeChecker::Visit(ArgExprNode& arg_expr) {
  arg_expr.arg->Accept(*this);
  arg_expr.type = arg_expr.arg->type;
}

void TypeChecker::Visit(FuncCallExprNode& call_expr) {
  call_expr.func_expr->Accept(*this);
  call_expr.type = call_expr.func_expr->type;

  const auto* id_expr = dynamic_cast<IdExprNode*>((call_expr.func_expr).get());
  assert(id_expr);
  auto func_def = env_.LookUp(id_expr->id);
  auto& param_types = func_def->param_types;
  auto& args = call_expr.args;
  if (param_types.size() != args.size()) {
    // TODO: argument size doesn't match
  }

  for (auto i = std::size_t{0}; i < args.size(); ++i) {
    args.at(i)->Accept(*this);
    if (args.at(i)->type != param_types.at(i)) {
      // TODO: unmatched argument type
    }
  }
}

void TypeChecker::Visit(UnaryExprNode& unary_expr) {
  unary_expr.operand->Accept(*this);
  unary_expr.type = unary_expr.operand->type;
  if (unary_expr.op == UnaryOperator::kAddr ||
      unary_expr.op == UnaryOperator::kDeref) {
    const auto* id_expr = dynamic_cast<IdExprNode*>((unary_expr.operand).get());
    assert(id_expr);
    auto operand = env_.LookUp(id_expr->id);
    if (operand == nullptr) {
      // TODO: lvalue required as unary '&' or '*' operand
    }

    // Return pointer type for reference, or return primitive type for
    // dereference.
    unary_expr.type.is_ptr = (unary_expr.op == UnaryOperator::kAddr);
  }
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
  assign_expr.lhs->Accept(*this);
  assign_expr.rhs->Accept(*this);
  if (assign_expr.lhs->type != assign_expr.rhs->type) {
    // TODO: unmatched assignment type
  } else {
    assign_expr.type = assign_expr.rhs->type;
  }
}
