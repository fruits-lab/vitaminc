#include "type_checker.hpp"

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <map>
#include <memory>
#include <string>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

#include "ast.hpp"
#include "operator.hpp"
#include "scope.hpp"
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

auto
    id_is_global  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables):
                  // Accessible only within this translation unit;
                  // declaring as a data member introduces unnecessary
                  // dependency.
    = std::map<std::string, bool>{};

}  // namespace

void TypeChecker::Visit(DeclStmtNode& decl_stmt) {
  for (auto& decl : decl_stmt.decls) {
    decl->Accept(*this);
  }
}

void TypeChecker::Visit(VarDeclNode& decl) {
  if (decl.init) {
    if (env_.CurrentScopeKind() == ScopeKind::kFile) {
      decl.init->is_global = true;
    }

    decl.init->Accept(*this);
    if (decl.init->type != decl.type) {
      // TODO: incompatible types when initializing type 'type' using type
      // 'init->type'
    }
  }

  if (env_.ProbeSymbol(decl.id)) {
    // TODO: redefinition of 'id'
  } else {
    auto symbol = std::make_unique<SymbolEntry>(decl.id, decl.type->Clone());
    env_.AddSymbol(std::move(symbol), env_.CurrentScopeKind());

    if (env_.CurrentScopeKind() == ScopeKind::kFile) {
      decl.is_global = true;
      id_is_global[decl.id] = true;
    }
  }
}

void TypeChecker::Visit(ArrDeclNode& arr_decl) {
  if (env_.ProbeSymbol(arr_decl.id)) {
    // TODO: redefinition of 'id'
  } else {
    auto symbol =
        std::make_unique<SymbolEntry>(arr_decl.id, arr_decl.type->Clone());

    for (auto& init : arr_decl.init_list) {
      init->Accept(*this);
      if (!init->type->IsEqual(*symbol->type)) {
        // TODO: element unmatches array element type
      }

      if (env_.CurrentScopeKind() == ScopeKind::kFile) {
        init->is_global = true;
      }
    }
    env_.AddSymbol(std::move(symbol), env_.CurrentScopeKind());

    if (env_.CurrentScopeKind() == ScopeKind::kFile) {
      arr_decl.is_global = true;
      id_is_global[arr_decl.id] = true;
    }
  }

  // TODO: Check initializer type
}

void TypeChecker::Visit(RecordDeclNode& record_decl) {
  if (env_.ProbeType(record_decl.id)) {
    // TODO: Check if 'id' is forward declaration (incomplete type).
    // If yes, then 'id' is used to declare its fields. For instance,
    // struct ss; // forward declaration
    // struct ss { // declaration of its fields
    //    int num;
    // };
    // If no, then it is the redefinition of 'id'.
  } else {
    auto decl_type =
        std::make_unique<TypeEntry>(record_decl.id, record_decl.type->Clone());

    env_.AddType(std::move(decl_type), env_.CurrentScopeKind());
  }
}

void TypeChecker::Visit(FieldNode& field) {
  // NOTE: Do nothing since record_decl 'Clone' already copies every field.
}

void TypeChecker::Visit(RecordVarDeclNode& record_var_decl) {
  if (env_.ProbeSymbol(record_var_decl.id)) {
    // TODO: redefinition of 'id'
  } else {
    // NOTE: record_var_decl doesn't know its own type, it needs to look up in
    // the type table to update its type.
    // struct birth { // RecordDeclNode -> stores type entry in type table
    //   int date;
    // };
    //
    // struct birth bd1 { .date = 1 }; // RecordVarDeclNode -> search type entry
    // to update its type.
    // record_type_id is "birth" in the above example.
    auto record_type_id = dynamic_cast<RecordType&>(*record_var_decl.type).id();
    auto record_type = env_.LookUpType(record_type_id);
    assert(record_type);
    auto symbol = std::make_unique<SymbolEntry>(record_var_decl.id,
                                                record_type->type->Clone());

    // TODO: type check between fields and initialized members.
    for (auto& init : record_var_decl.inits) {
      init->Accept(*this);
      if (env_.CurrentScopeKind() == ScopeKind::kFile) {
        init->is_global = true;
      }
    }
    env_.AddSymbol(std::move(symbol), env_.CurrentScopeKind());
    if (env_.CurrentScopeKind() == ScopeKind::kFile) {
      record_var_decl.is_global = true;
      id_is_global[record_var_decl.id] = true;
    }

    record_var_decl.type = record_type->type->Clone();
  }
}

void TypeChecker::Visit(EnumConstDeclNode& enum_const_decl) {
  // Since the enum constant doesn't know its type, it has the enum declaration
  // set its type.
  if (enum_const_decl.int_const) {
    enum_const_decl.int_const->Accept(*this);
  }
  if (env_.ProbeSymbol(enum_const_decl.id)) {
    // TODO: redefinition of 'id'
    assert(false);
  }
}

void TypeChecker::Visit(EnumDeclNode& enum_decl) {
  // NOTE: There's no forward declaration for enum.
  if (env_.ProbeType(enum_decl.id)) {
    // TODO: redefinition of 'id'
    assert(false);
  }
  // Do not add the enum type to the type table if it's an unnamed enum, so that
  // multiple unnamed enums can be declared in the same scope.
  if (!enum_decl.id.empty()) {
    auto decl_type =
        std::make_unique<TypeEntry>(enum_decl.id, enum_decl.type->Clone());
    env_.AddType(std::move(decl_type), env_.CurrentScopeKind());
  }

  for (auto& enum_const : enum_decl.enum_consts) {
    enum_const->Accept(*this);
    // The type of the enum constant is the enum type itself.
    auto symbol =
        std::make_unique<SymbolEntry>(enum_const->id, enum_decl.type->Clone());
    // The enum constant is introduced as a symbol in the current scope.
    env_.AddSymbol(std::move(symbol), env_.CurrentScopeKind());
  }
}

void TypeChecker::Visit(ParamNode& parameter) {
  if (env_.ProbeSymbol(parameter.id)) {
    // TODO: redefinition of 'id'
  } else {
    // NOTE: Any parameter of array or function type is adjusted to the
    // corresponding pointer type.
    if (parameter.type->IsArr()) {
      // Decay to simple pointer type.
      parameter.type = std::make_unique<PtrType>(
          dynamic_cast<ArrType&>(*parameter.type).element_type().Clone());
    } else if (parameter.type->IsFunc()) {
      // Decay to function pointer type.
      parameter.type = std::make_unique<PtrType>(parameter.type->Clone());
    }
    auto symbol =
        std::make_unique<SymbolEntry>(parameter.id, parameter.type->Clone());
    // TODO: May be parameter scope once we support function prototypes.
    env_.AddSymbol(std::move(symbol), ScopeKind::kBlock);
  }
}

namespace {

/// @brief Associate with a function scope. Keep track of the use and definition
/// of a label. This is essential for forward referencing. Each time a label is
/// used, add it to the map. Each time a label is defined, mark its
/// corresponding mapping as true. In case a label is defined before used, also
/// add it to the map.
std::unordered_map<std::string, bool>
    label_defined;  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)
}  // namespace

void TypeChecker::Visit(FuncDefNode& func_def) {
  if (env_.ProbeSymbol(func_def.id)) {
    // TODO: redefinition of function id
  }

  env_.PushScope(ScopeKind::kFunc);
  // NOTE: This block scope will be merged with the function body. Don't pop it.
  env_.PushScope(ScopeKind::kBlock);
  env_.MergeWithNextScope();
  for (auto& parameter : func_def.parameters) {
    parameter->Accept(*this);
  }
  // The type of some parameters may be decayed to pointer type.
  // The type of the function should be updated accordingly.
  auto decayed_param_types = std::vector<std::unique_ptr<Type>>{};
  for (auto& parameter : func_def.parameters) {
    decayed_param_types.push_back(parameter->type->Clone());
  }
  auto return_type =
      dynamic_cast<FuncType&>(*func_def.type).return_type().Clone();
  func_def.type = std::make_unique<FuncType>(std::move(return_type),
                                             std::move(decayed_param_types));
  auto symbol =
      std::make_unique<SymbolEntry>(func_def.id, func_def.type->Clone());
  env_.AddSymbol(std::move(symbol), ScopeKind::kFile);

  label_defined.clear();
  func_def.body->Accept(*this);
  for (auto& [label, defined] : label_defined) {
    if (!defined) {
      // TODO: use of undeclared label 'label'
    }
  }
  label_defined.clear();
  // Pops the function scope.
  env_.PopScope();
  //  TODO: check body return type and function return type
}

void TypeChecker::Visit(LoopInitNode& loop_init) {
  std::visit([this](auto&& clause) { clause->Accept(*this); },
             loop_init.clause);
}

void TypeChecker::Visit(CompoundStmtNode& compound_stmt) {
  env_.PushScope(ScopeKind::kBlock);
  for (auto& stmt : compound_stmt.stmts) {
    stmt->Accept(*this);
  }
  env_.PopScope();
}

void TypeChecker::InstallBuiltins_(ScopeStack& env) {
  // The supported builtins are:
  // - int __builtin_print(int)

  auto param_types = std::vector<std::unique_ptr<Type>>{};
  param_types.emplace_back(std::make_unique<PrimType>(PrimitiveType::kInt));
  auto symbol = std::make_unique<SymbolEntry>(
      "__builtin_print", std::make_unique<FuncType>(
                             std::make_unique<PrimType>(PrimitiveType::kInt),
                             std::move(param_types)));
  env.AddSymbol(std::move(symbol), ScopeKind::kFile);
}

void TypeChecker::Visit(ExternDeclNode& extern_decl) {
  std::visit([this](auto&& extern_decl) { extern_decl->Accept(*this); },
             extern_decl.decl);
}

void TypeChecker::Visit(TransUnitNode& trans_unit) {
  env_.PushScope(ScopeKind::kFile);
  InstallBuiltins_(env_);
  for (auto& extern_decl : trans_unit.extern_decls) {
    extern_decl->Accept(*this);
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
  if (!ret_stmt.expr->type->IsEqual(PrimitiveType::kInt)) {
    // TODO: return value type does not match the function type
  }
}

void TypeChecker::Visit(GotoStmtNode& goto_stmt) {
  // NOTE: We can know whether a label is defined until the function is about to
  // end. Thus, it's checked in the function definition.
  // Also the lookup from the environment is not necessary. In fact, labels are
  // not added to the environment.
  const bool is_not_defined =
      label_defined.find(goto_stmt.label) == label_defined.end() ||
      !label_defined.at(goto_stmt.label);
  if (is_not_defined) {
    label_defined[goto_stmt.label] = false;
  }
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
  if (!switch_stmt.ctrl->type->IsEqual(PrimitiveType::kInt)) {
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
  if (label_defined.find(id_labeled_stmt.label) != label_defined.end() &&
      label_defined.at(id_labeled_stmt.label)) {
    // TODO: redefinition of label 'label'
  }
  label_defined[id_labeled_stmt.label] = true;
  id_labeled_stmt.stmt->Accept(*this);
}

void TypeChecker::Visit(CaseStmtNode& case_stmt) {
  if (!IsInBodyOf(BodyType::kSwitch)) {
    // TODO: 'case' statement not in switch statement
  }
  case_stmt.expr->Accept(*this);
  if (!case_stmt.expr->type->IsEqual(PrimitiveType::kInt)) {
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

void TypeChecker::Visit(InitExprNode& init_expr) {
  for (const auto& des : init_expr.des) {
    des->Accept(*this);
  }
  init_expr.expr->Accept(*this);
  init_expr.type = init_expr.expr->type->Clone();
  if (env_.CurrentScopeKind() == ScopeKind::kFile) {
    init_expr.expr->is_global = true;
  }
}

void TypeChecker::Visit(ArrDesNode& arr_des) {
  /* ArrDesNode shall have array type and the expression shall be an integer
   * constant expression. */
  arr_des.index->Accept(*this);
}

void TypeChecker::Visit(IdDesNode& id_des) {
  /* IdDesNode does nothing and shall have structure or union type and the
   * identifier shall be the name of a member of that type. */
}

void TypeChecker::Visit(NullExprNode&) {
  /* do nothing */
}

void TypeChecker::Visit(IdExprNode& id_expr) {
  if (auto symbol = env_.LookUpSymbol(id_expr.id)) {
    id_expr.type = symbol->type->Clone();
    if (id_is_global.count(id_expr.id) != 0) {
      id_expr.is_global = true;
    }
    // This identifier is one of the following:
    // 1. An enumeration constant.
    // 2. A variable.
    if (id_expr.type->IsEnum()) {
      const auto& enum_type = dynamic_cast<EnumType&>(*id_expr.type);
      // 1. Hold the value of the constant if it is an enumeration constant.
      if (enum_type.IsEnumConst(id_expr.id)) {
        id_expr.const_expr = std::make_unique<IntConstExprNode>(
            id_expr.loc, enum_type.ValueOf(id_expr.id));
        id_expr.const_expr->Accept(*this);
      } else {
        // 2. Do nothing if is a variable. Do nothing.
      }
    }
  } else {
    // TODO: 'id' undeclared
    assert(false);
  }
}

void TypeChecker::Visit(IntConstExprNode& int_expr) {
  int_expr.type = std::make_unique<PrimType>(PrimitiveType::kInt);
}

void TypeChecker::Visit(ArgExprNode& arg_expr) {
  arg_expr.arg->Accept(*this);
  arg_expr.type = arg_expr.arg->type->Clone();
}

void TypeChecker::Visit(ArrSubExprNode& arr_sub_expr) {
  arr_sub_expr.arr->Accept(*this);
  arr_sub_expr.index->Accept(*this);
  const auto& arr_type = dynamic_cast<ArrType&>(*arr_sub_expr.arr->type);
  // arr_sub_expr should have the element type of the array.
  arr_sub_expr.type = arr_type.element_type().Clone();
  arr_sub_expr.is_global = arr_sub_expr.arr->is_global;
}

void TypeChecker::Visit(CondExprNode& cond_expr) {
  // TODO: support operand pointer types
  cond_expr.predicate->Accept(*this);
  cond_expr.then->Accept(*this);
  cond_expr.or_else->Accept(*this);
  if (!cond_expr.then->type->IsEqual(*cond_expr.or_else->type)) {
    // TODO: unmatched operand types
  } else {
    // If both the second and third operands have arithmetic type, the result
    // type that would be determined by the usual arithmetic conversions, were
    // they applied to those two operands, is the type of the result. If both
    // the operands have structure or union type, the result has that type. If
    // both operands have void type, the result has void type.
    cond_expr.type = cond_expr.then->type->Clone();
  }
}

void TypeChecker::Visit(FuncCallExprNode& call_expr) {
  call_expr.func_expr->Accept(*this);

  // The function expression should have a function type or a pointer to a
  // function type.
  // NOTE: Using shared pointer to avoid additional casting on raw pointer.
  auto func_type = std::shared_ptr<FuncType>{};
  if (call_expr.func_expr->type->IsFunc()) {
    func_type = std::dynamic_pointer_cast<FuncType>(
        std::shared_ptr<Type>{call_expr.func_expr->type->Clone()});
  } else if (const auto* ptr_type =
                 dynamic_cast<PtrType*>((call_expr.func_expr->type).get());
             ptr_type->base_type().IsFunc()) {
    func_type = std::dynamic_pointer_cast<FuncType>(
        std::shared_ptr<Type>{ptr_type->base_type().Clone()});
  } else {
    // TODO: called object type 'type' is not a function or function pointer
    assert(false);
  }
  call_expr.type = func_type->return_type().Clone();

  auto& param_types = func_type->param_types();
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

void TypeChecker::Visit(PostfixArithExprNode& postfix_expr) {
  postfix_expr.operand->Accept(*this);
  // NOTE: The operand of the postfix increment or decrement operator shall
  // have atomic, qualified, or unqualified real or pointer type, and shall
  // be a modifiable lvalue.
  const auto* id_expr = dynamic_cast<IdExprNode*>((postfix_expr.operand).get());
  if (!id_expr || !env_.LookUpSymbol(id_expr->id)) {
    // TODO: lvalue required for postfix increment
  }
  postfix_expr.type = postfix_expr.operand->type->Clone();
}

void TypeChecker::Visit(RecordMemExprNode& mem_expr) {
  mem_expr.expr->Accept(*this);
  if (auto* record_type =
          dynamic_cast<RecordType*>((mem_expr.expr->type).get())) {
    if (record_type->IsMember(mem_expr.id)) {
      mem_expr.type = record_type->MemberType(mem_expr.id).Clone();
    } else {
      assert(false);
      // TODO: Throw error if mem_expr.id is not a symbol's member.
    }
  } else {
    // TODO: Throw error since symbol is not struct nor union type.
  }
}

void TypeChecker::Visit(UnaryExprNode& unary_expr) {
  unary_expr.operand->Accept(*this);
  switch (unary_expr.op) {
    case UnaryOperator::kAddr: {
      const auto* id_expr =
          dynamic_cast<IdExprNode*>((unary_expr.operand).get());
      // NOTE: The operand of unary '&' must be an lvalue, and the only
      // supported lvalue is an identifier.
      if (!id_expr || !env_.LookUpSymbol(id_expr->id)) {
        // TODO: lvalue required as unary '&' operand
      }
      unary_expr.type =
          std::make_unique<PtrType>(unary_expr.operand->type->Clone());
    } break;
    case UnaryOperator::kDeref:
      if (!unary_expr.operand->type->IsPtr()) {
        // TODO: the operand of unary '*' shall have pointer type
      }
      unary_expr.type =
          dynamic_cast<PtrType&>(*unary_expr.operand->type).base_type().Clone();
      break;
    default:
      unary_expr.type = unary_expr.operand->type->Clone();
      break;
  }
  // TODO: check operands type
}

void TypeChecker::Visit(BinaryExprNode& bin_expr) {
  bin_expr.lhs->Accept(*this);
  bin_expr.rhs->Accept(*this);

  // NOTE: The left operand of a comma operator is evaluated as a void
  // expression; there is a sequence point after its evaluation. Then the right
  // operand is evaluated; the result has its type and value.
  if (bin_expr.op == BinaryOperator::kComma) {
    bin_expr.type = bin_expr.rhs->type->Clone();
    return;
  }

  if (!bin_expr.lhs->type->IsEqual(*bin_expr.rhs->type)) {
    // TODO: invalid operands to binary +
  } else {
    bin_expr.type = bin_expr.lhs->type->Clone();
  }
}

void TypeChecker::Visit(SimpleAssignmentExprNode& assign_expr) {
  assign_expr.lhs->Accept(*this);
  assign_expr.rhs->Accept(*this);
  if (!assign_expr.rhs->type->IsConvertibleTo(*assign_expr.lhs->type)) {
    // TODO: unmatched assignment type
    assert(false);
  } else {
    // The type of the assignment is the type of the left-hand side.
    assign_expr.type = assign_expr.lhs->type->Clone();
  }
}
