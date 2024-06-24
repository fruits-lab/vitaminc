#include "qbe_ir_generator.hpp"

#include <fmt/core.h>
#include <fmt/ostream.h>

#include <cassert>
#include <cstddef>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "ast.hpp"
#include "operator.hpp"
#include "qbe/sigil.hpp"
#include "type.hpp"

// Since compiler-generated sigils are used more frequently, we include them
// directly.
using namespace qbe::compiler_generated;
namespace user_defined = qbe::user_defined;

namespace {

/// @brief Returns the next local number and increment it by 1. The first number
/// will be 1.
int NextLocalNum() {
  /// @brief temporary index under a scope
  static int next_local_num = 1;
  return next_local_num++;
}

int NextLabelNum() {
  static int next_label_num = 1;
  return next_label_num++;
}

std::string GetBinaryOperator(BinaryOperator op) {
  switch (op) {
    case BinaryOperator::kAdd:
      return "add";
    case BinaryOperator::kSub:
      return "sub";
    case BinaryOperator::kMul:
      return "mul";
    case BinaryOperator::kDiv:
      return "div";
    case BinaryOperator::kMod:
      return "rem";
    // TODO: update comparison instructions with no data type, such as "gt".
    case BinaryOperator::kGt:
      return "csgtw";
    case BinaryOperator::kGte:
      return "csgew";
    case BinaryOperator::kLt:
      return "csltw";
    case BinaryOperator::kLte:
      return "cslew";
    case BinaryOperator::kEq:
      return "ceqw";
    case BinaryOperator::kNeq:
      return "cnew";
    case BinaryOperator::kAnd:
      return "and";
    case BinaryOperator::kXor:
      return "xor";
    case BinaryOperator::kOr:
      return "or";
    case BinaryOperator::kShl:
      return "shl";
    // NOTE: Arithmetic shift right (sar) is akin to dividing by a power of two
    // for non-negative numbers. For negatives, it's implementation-defined, so
    // we opt for arithmetic shifting.
    case BinaryOperator::kShr:
      return "sar";
    default:
      return "Unknown";
  }
}

auto id_to_num  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables):
                // Accessible only within this translation unit; declaring as a
                // data member introduces unnecessary dependency.
    = std::map<std::string, int>{};

auto
    reg_num_to_id_num  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables):
                       // Accessible only within this translation unit;
                       // declaring as a data member introduces unnecessary
                       // dependency.
    = std::map<int, int>{};

auto
    reg_num_to_id  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables):
                   // Accessible only within this translation unit; declaring as
                   // a data member introduces unnecessary dependency.
    = std::map<int, std::string>{};

/// @brief Every expression generates a temporary. The local number of such
/// temporary should be stored, so can propagate to later uses.
class PrevExprNumRecorder {
 public:
  void Record(int num) {
    num_of_prev_expr_ = num;
  }

  /// @note The local number can only be gotten once. This is to reduce the
  /// possibility of getting obsolete number.
  int NumOfPrevExpr() {
    assert(num_of_prev_expr_ != kNoRecord);
    int tmp = num_of_prev_expr_;
    num_of_prev_expr_ = kNoRecord;
    return tmp;
  }

 private:
  static constexpr int kNoRecord = -1;
  int num_of_prev_expr_ = kNoRecord;
};

auto
    num_recorder  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables):
                  // Accessible only within this translation unit; declaring as
                  // a data member introduces unnecessary dependency.
    = PrevExprNumRecorder{};

struct LabelViewPair {
  BlockLabel entry;
  BlockLabel exit;
};

/// @note Blocks that allows jumping within or out of it should add its labels
/// to this list.
auto
    label_views_of_jumpable_blocks  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)
    = std::vector<LabelViewPair>{};

union GlobalVarInitVal {
  int ival;
};

auto
    global_var_init_val  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)
    = std::vector<GlobalVarInitVal>{};

}  // namespace

void QbeIrGenerator::Visit(const DeclStmtNode& decl_stmt) {
  // TODO: code generation for global variables, VarDeclNode, ArrDeclNode,
  // RecordVarDeclNode
  for (const auto& decl : decl_stmt.decls) {
    decl->Accept(*this);
  }
}

void QbeIrGenerator::Visit(const VarDeclNode& decl) {
  if (decl.is_global) {
    // TODO: support different data types.
    if (decl.init) {
      global_var_init_val.clear();
      decl.init->Accept(*this);
      Write_("export data {} = align {} {{ w {} }}\n",
             user_defined::GlobalPointer{decl.id}, decl.type->size(),
             global_var_init_val.at(0).ival);
    } else {
      // `z` in QBE stands for allocating n bytes of memory space.
      Write_("export data {0} = align {1} {{ z {1} }}\n",
             user_defined::GlobalPointer{decl.id}, decl.type->size());
    }
  } else {
    int id_num = NextLocalNum();
    WriteInstr_("{} =l alloc{} {}", FuncScopeTemp{id_num}, decl.type->size(),
                decl.type->size());
    if (decl.init) {
      decl.init->Accept(*this);
      int init_num = num_recorder.NumOfPrevExpr();
      // A pointer declaration may have two options for its right hand side:
      if (decl.init->type->IsPtr() || decl.init->type->IsFunc()) {
        // 1. int* a = &b; rhs is a reference of integer. We need to store b's
        // address to a, where we need to map b's reg_num back to its id_num.
        if (dynamic_cast<UnaryExprNode*>((decl.init).get())) {
          WriteInstr_("storel {}, {}",
                      FuncScopeTemp{reg_num_to_id_num.at(init_num)},
                      FuncScopeTemp{id_num});
        } else {
          // 2. int* a = c; c itself stores the address of another integer. We
          // can directly use the address c currently holds.
          WriteInstr_("storel {}, {}", FuncScopeTemp{init_num},
                      FuncScopeTemp{id_num});
        }
      } else {
        WriteInstr_("storew {}, {}", FuncScopeTemp{init_num},
                    FuncScopeTemp{id_num});
      }
    }
    // Set up the number of the id so we know were to load it back.
    id_to_num[decl.id] = id_num;
  }
}

void QbeIrGenerator::Visit(const ArrDeclNode& arr_decl) {
  if (arr_decl.is_global) {
    const auto* arr_type = dynamic_cast<ArrType*>((arr_decl.type).get());
    assert(arr_type);
    if (arr_decl.init_list.size() != 0) {
      global_var_init_val.clear();
      Write_("export data {} = align {} {{ ",
             user_defined::GlobalPointer{arr_decl.id},
             arr_type->element_type().size());
      for (auto i = std::size_t{0}, e = arr_type->len(); i < e; ++i) {
        auto& arr_init = arr_decl.init_list.at(i);
        arr_init->Accept(*this);
        Write_("w {}", global_var_init_val.at(i).ival);
        if (i != e - 1) {
          Write_(", ");
        }
      }
      Write_(" }}\n");
    } else {
      Write_("export data {} = align {} {{ z {} }}\n",
             user_defined::GlobalPointer{arr_decl.id},
             arr_type->element_type().size(), arr_decl.type->size());
    }
  } else {
    int base_addr_num = NextLocalNum();
    assert(arr_decl.type->IsArr());
    const auto* arr_type = dynamic_cast<ArrType*>((arr_decl.type).get());
    auto element_size = arr_type->element_type().size();
    WriteInstr_("{} =l alloc{} {}", FuncScopeTemp{base_addr_num}, element_size,
                arr_decl.type->size());
    id_to_num[arr_decl.id] = base_addr_num;

    for (auto i = std::size_t{0}, e = arr_type->len(); i < e; ++i) {
      if (i < arr_decl.init_list.size()) {
        auto& arr_init = arr_decl.init_list.at(i);
        arr_init->Accept(*this);
      }

      const int offset = NextLocalNum();
      WriteInstr_("{} =l extsw {}", FuncScopeTemp{offset}, i * element_size);

      // res_addr = base_addr + offset
      const int res_addr_num = NextLocalNum();
      WriteInstr_("{} =l add {}, {}", FuncScopeTemp{res_addr_num},
                  FuncScopeTemp{base_addr_num}, FuncScopeTemp{offset});

      if (i < arr_decl.init_list.size()) {
        int init_val_num = num_recorder.NumOfPrevExpr();
        WriteInstr_("storew {}, {}", FuncScopeTemp{init_val_num},
                    FuncScopeTemp{res_addr_num});
      } else {
        // set remaining elements as 0
        WriteInstr_("storew 0, {}", FuncScopeTemp{res_addr_num});
      }
    }
  }
}

void QbeIrGenerator::Visit(const RecordDeclNode& record_decl) {
  /* Do nothing because this node only declares a type. */
}

void QbeIrGenerator::Visit(const FieldNode& field) {
  /* Do nothing because this node only declares a member type in a record. */
}

void QbeIrGenerator::Visit(const RecordVarDeclNode& record_var_decl) {
  const auto base_addr = NextLocalNum();
  // TODO: support different data types. We have `int` type for now.
  WriteInstr_("{} =l alloc4 {}", FuncScopeTemp{base_addr},
              record_var_decl.type->size());
  id_to_num[record_var_decl.id] = base_addr;

  auto* record_type = dynamic_cast<RecordType*>(record_var_decl.type.get());
  assert(record_type);
  // NOTE: This predicate will make sure that we don't initialize members that
  // exceed the total number of members in a record. Also, it gurantees
  // that accessing element in the initializers will not go out of bound.
  for (auto i = std::size_t{0}, e = record_var_decl.inits.size(),
            slot_count = record_type->SlotCount();
       i < slot_count && i < e; ++i) {
    const auto& init = record_var_decl.inits.at(i);
    init->Accept(*this);
    const auto init_num = num_recorder.NumOfPrevExpr();

    // res_addr = base_addr + offset
    const int res_addr_num = NextLocalNum();
    const auto offset = record_type->OffsetOf(i);
    WriteInstr_("{} =l add {}, {}", FuncScopeTemp{res_addr_num},
                FuncScopeTemp{base_addr}, offset);
    WriteInstr_("storew {}, {}", FuncScopeTemp{init_num},
                FuncScopeTemp{res_addr_num});
  }
}

void QbeIrGenerator::Visit(const ParamNode& parameter) {
  int id_num = NextLocalNum();
  // TODO: support different data types
  if (parameter.type->IsPtr()) {
    Write_("l %.{}", id_num);
  } else {
    Write_("w %.{}", id_num);
  }
  id_to_num[parameter.id] = id_num;
}

void QbeIrGenerator::AllocMemForParams_(
    const std::vector<std::unique_ptr<ParamNode>>& parameters) {
  for (const auto& parameter : parameters) {
    int id_num = id_to_num.at(parameter->id);
    int reg_num = NextLocalNum();
    WriteInstr_("{} =l alloc{} {}", FuncScopeTemp{reg_num},
                parameter->type->size(), parameter->type->size());
    if (parameter->type->IsPtr()) {
      WriteInstr_("storel {}, {}", FuncScopeTemp{id_num},
                  FuncScopeTemp{reg_num});
    } else {
      WriteInstr_("storew {}, {}", FuncScopeTemp{id_num},
                  FuncScopeTemp{reg_num});
    }
    // Update to store the new number.
    id_to_num[parameter->id] = reg_num;
  }
}

void QbeIrGenerator::Visit(const FuncDefNode& func_def) {
  int label_num = NextLabelNum();
  // Parameter allocations go after the start label and before the body.
  auto start_label = BlockLabel{"start", label_num};
  auto body_label = BlockLabel{"body", label_num};

  Write_("export\n");
  Write_("function w ${}(", func_def.id);
  for (const auto& parameter : func_def.parameters) {
    parameter->Accept(*this);
    if (parameter != func_def.parameters.back()) {
      Write_(", ");
    }
  }
  Write_(") {{\n");
  WriteLabel_(start_label);
  AllocMemForParams_(func_def.parameters);
  WriteLabel_(body_label);
  func_def.body->Accept(*this);
  Write_("}}\n");
}

void QbeIrGenerator::Visit(const LoopInitNode& loop_init) {
  std::visit([this](auto&& clause) { clause->Accept(*this); },
             loop_init.clause);
}

void QbeIrGenerator::Visit(const CompoundStmtNode& compound_stmt) {
  // Note: CompoundStmtNode cannot output the correct label to its own block
  // because it doesn't know whether it is a if statement body or a function.
  // Thus, by moving label creation to an upper level, each block can have its
  // correct starting label.
  for (const auto& stmt : compound_stmt.stmts) {
    stmt->Accept(*this);
  }
}

void QbeIrGenerator::Visit(const ExternDeclNode& extern_decl) {
  std::visit([this](auto&& extern_decl) { extern_decl->Accept(*this); },
             extern_decl.decl);
}

void QbeIrGenerator::Visit(const TransUnitNode& trans_unit) {
  // Generate the data of builtin functions.
  Write_("data {} = align 1 {{ b \"%d\\012\\000\", }}\n",
         user_defined::GlobalPointer{"__builtin_print_format"});

  for (const auto& extern_decl : trans_unit.extern_decls) {
    extern_decl->Accept(*this);
  }
}

void QbeIrGenerator::Visit(const IfStmtNode& if_stmt) {
  if_stmt.predicate->Accept(*this);
  int predicate_num = num_recorder.NumOfPrevExpr();
  int label_num = NextLabelNum();
  auto then_label = BlockLabel{"if_then", label_num};
  auto else_label = BlockLabel{"if_else", label_num};
  auto end_label = BlockLabel{"if_end", label_num};

  // Jumps to "then" if the predicate is true (non-zero), else jumps to "else".
  // If no "else" exists, falls through to "end".
  // If "else" exists, a second jump is needed after executing "then" to skip
  // it, as the generated code for "else" follows immediately after "then".
  WriteComment_("if");
  Write_("{}jnz {}, {}, ", kIndentStr, FuncScopeTemp{predicate_num},
         then_label);
  if (if_stmt.or_else) {
    Write_("{}\n", else_label);
  } else {
    Write_("{}\n", end_label);
  }

  WriteLabel_(then_label);
  if_stmt.then->Accept(*this);
  if (if_stmt.or_else) {
    // Skip the "else" part after executing "then".
    WriteInstr_("jmp {}\n", end_label);
    WriteLabel_(else_label);
    if_stmt.or_else->Accept(*this);
  }
  WriteLabel_(end_label);
}

void QbeIrGenerator::Visit(const WhileStmtNode& while_stmt) {
  int label_num = NextLabelNum();
  const auto label_prefix =
      std::string{while_stmt.is_do_while ? "do_" : "while_"};
  auto body_label = BlockLabel{label_prefix + "body", label_num};
  auto pred_label = BlockLabel{label_prefix + "pred", label_num};
  auto end_label = BlockLabel{label_prefix + "end", label_num};

  // A while statement's predicate is evaluated "before" the body statement,
  // whereas a do-while statement's predicate is evaluated "after" the body
  // statement. In the generated code for a while statement, there is an
  // unconditional jump at the end of the body to jump back to the predicate.
  // For a do-while statement, it only needs one conditional jump.
  if (!while_stmt.is_do_while) {
    WriteLabel_(pred_label);
    while_stmt.predicate->Accept(*this);
    int predicate_num = num_recorder.NumOfPrevExpr();
    WriteInstr_("jnz {}, {}, {}", FuncScopeTemp{predicate_num}, body_label,
                end_label);
  }
  WriteLabel_(body_label);
  label_views_of_jumpable_blocks.push_back(
      {.entry = pred_label, .exit = end_label});
  while_stmt.loop_body->Accept(*this);
  label_views_of_jumpable_blocks.pop_back();
  if (!while_stmt.is_do_while) {
    WriteInstr_("jmp {}", pred_label);
  } else {
    WriteLabel_(pred_label);
    while_stmt.predicate->Accept(*this);
    int predicate_num = num_recorder.NumOfPrevExpr();
    WriteInstr_("jnz {}, {}, {}", FuncScopeTemp{predicate_num}, body_label,
                end_label);
  }
  WriteLabel_(end_label);
}

void QbeIrGenerator::Visit(const ForStmtNode& for_stmt) {
  int label_num = NextLabelNum();

  // A for loop consists of three clauses: loop initialization, predicate, and a
  // step: for (init; pred; step) { body; }

  auto pred_label = BlockLabel{"for_pred", label_num};
  auto body_label = BlockLabel{"for_body", label_num};
  auto step_label = BlockLabel{"for_step", label_num};
  auto end_label = BlockLabel{"for_end", label_num};

  // A for statement's loop initialization is the first clause to execute,
  // whereas a for statement's predicate specifies evaluation made before each
  // iteration. A step is an operation that is performed after each iteration.
  // Skip predicate generation if it is a null expression.
  WriteComment_("loop init");
  for_stmt.loop_init->Accept(*this);
  WriteLabel_(pred_label);
  for_stmt.predicate->Accept(*this);
  if (!dynamic_cast<NullExprNode*>((for_stmt.predicate).get())) {
    int predicate_num = num_recorder.NumOfPrevExpr();
    WriteInstr_("jnz {}, {}, {}", FuncScopeTemp{predicate_num}, body_label,
                end_label);
  }
  WriteLabel_(body_label);
  label_views_of_jumpable_blocks.push_back(
      {.entry = step_label, .exit = end_label});
  for_stmt.loop_body->Accept(*this);
  label_views_of_jumpable_blocks.pop_back();
  WriteLabel_(step_label);
  for_stmt.step->Accept(*this);
  WriteInstr_("jmp {}", pred_label);
  WriteLabel_(end_label);
}

void QbeIrGenerator::Visit(const ReturnStmtNode& ret_stmt) {
  ret_stmt.expr->Accept(*this);
  int ret_num = num_recorder.NumOfPrevExpr();
  WriteInstr_("ret {}", FuncScopeTemp{ret_num});
}

void QbeIrGenerator::Visit(const GotoStmtNode& goto_stmt) {
  WriteInstr_("jmp {}", user_defined::BlockLabel{goto_stmt.label});
}

void QbeIrGenerator::Visit(const BreakStmtNode& break_stmt) {
  assert(!label_views_of_jumpable_blocks.empty());
  WriteInstr_("jmp {}", BlockLabel{label_views_of_jumpable_blocks.back().exit});
}

void QbeIrGenerator::Visit(const ContinueStmtNode& continue_stmt) {
  assert(!label_views_of_jumpable_blocks.empty());
  WriteInstr_("jmp {}",
              BlockLabel{label_views_of_jumpable_blocks.back().entry});
}

namespace {

struct CaseInfo {
  /// @note This is a non-owning pointer that points to the expression of the
  /// case.
  const ExprNode* expr = nullptr;
  BlockLabel label;
};

struct SwitchInfo {
  std::vector<CaseInfo> case_infos{};
  std::optional<BlockLabel> default_label;
  BlockLabel exit_label;

  explicit SwitchInfo(BlockLabel exit_label,
                      std::optional<BlockLabel> default_label = std::nullopt)
      : default_label{std::move(default_label)},
        exit_label{std::move(exit_label)} {}
};

/// @brief The shared states passed around during the generation of a switch.
/// @note To allow nested switch statements, the information is stacked.
auto switch_infos  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)
    = std::vector<std::shared_ptr<SwitchInfo>>{};

/// @brief The label of the next condition depends on whether we've already
/// handled the last one and whether there is a default label.
BlockLabel GetNextCondLabel(bool is_last_cond,
                            const std::optional<BlockLabel>& default_label) {
  if (is_last_cond && default_label) {
    return *default_label;
  }
  if (is_last_cond) {
    return switch_infos.back()->exit_label;
  }
  return {"switch_cond", NextLabelNum()};
}

}  // namespace

void QbeIrGenerator::Visit(const SwitchStmtNode& switch_stmt) {
  // The structure of a switch statement, including the labeled statements
  // inside, is represented in the following pseudo IR:
  //
  // %1 = ... ctrl ...
  //  jmp @cond.1
  // # { The enclosing curly brace of the switch statement.
  //  ... unreachable code ...
  // @case.2:
  //  ... body ...
  //  jmp @exit  # break
  // @case.3:
  //  ... body ...
  // @default.4:
  //  ... body ...
  // @case.5:
  //  ... body ...
  //  jmp @exit  # break
  // # } The enclosing curly brace of the switch statement.
  // # The jmp is intentionally placed, so that the conditions are never touch
  // # twice.
  // # A label to avoid invalid consecutive jumps.
  // @switch_bottom.6
  //  jmp @exit
  // @cond.1:
  //  %2 =w ... case expr ...
  //  %3 =w ceqw %1, %2
  //  jnz %3, @case.2, @cond.7
  // @cond.7:
  //  %4 =w ... case expr ...
  //  %5 =w ceqw %1, %4
  //  jnz %5, @case.3, @cond.8
  // @cond.8:
  //  %8 =w ... case expr ...
  //  %9 =w ceqw %1, %8
  // # If no case matches, jump to the default case.
  // # If no default case exists, jump to the exit label instead.
  //  jnz %9, @case.5, @default.4
  // @exit:
  // ... statements after the switch statement ...
  //
  // The first part contains the case labels and the default label, while the
  // second part contains the conditions matching the control expression
  // with the case expressions.
  // The switch statement first jumps to the second part, which then jumps back
  // to the first part. This ensures that each part is executed only once.
  // NOTE:
  // (1) Case labels are generated first to collect information needed for
  //     the conditions.
  // (2) Evaluation of case expressions is done in the condition part.

  WriteComment_("switch");
  switch_stmt.ctrl->Accept(*this);
  const auto ctrl_num = num_recorder.NumOfPrevExpr();
  auto cond_label = BlockLabel{"switch_cond", NextLabelNum()};
  WriteInstr_("jmp {}", cond_label);

  switch_infos.push_back(
      std::make_shared<SwitchInfo>(BlockLabel{"switch_exit", NextLabelNum()}));
  GenerateCases_(switch_stmt);
  GenerateConditions_(switch_stmt, cond_label, ctrl_num);

  WriteLabel_(switch_infos.back()->exit_label);
  switch_infos.pop_back();
}

void QbeIrGenerator::GenerateCases_(const SwitchStmtNode& switch_stmt) {
  auto this_switch_info = switch_infos.back();
  label_views_of_jumpable_blocks.push_back(
      {// FIXME: The break statement only jumps to the exit label; there's no
       // appropriate entry label to set here.
       .entry = this_switch_info->exit_label,
       .exit = this_switch_info->exit_label}

  );
  switch_stmt.stmt->Accept(*this);
  label_views_of_jumpable_blocks.pop_back();
  WriteLabel_(BlockLabel{"switch_bottom", NextLabelNum()});
  WriteInstr_("jmp {}", this_switch_info->exit_label);
}

void QbeIrGenerator::GenerateConditions_(const SwitchStmtNode& switch_stmt,
                                         const BlockLabel& first_cond_label,
                                         int ctrl_num) {
  auto this_switch_info = switch_infos.back();
  auto cond_label = first_cond_label;
  for (auto i = std::size_t{0}, e = this_switch_info->case_infos.size(); i < e;
       ++i) {
    WriteLabel_(cond_label);
    const auto& case_info = this_switch_info->case_infos.at(i);
    case_info.expr->Accept(*this);
    const auto expr_num = num_recorder.NumOfPrevExpr();
    const auto match_num = NextLocalNum();
    WriteInstr_("{} =w ceqw {}, {}", FuncScopeTemp{match_num},
                FuncScopeTemp{ctrl_num}, FuncScopeTemp{expr_num});
    const auto is_last_cond = i == e - 1;
    cond_label =
        GetNextCondLabel(is_last_cond, this_switch_info->default_label);
    WriteInstr_("jnz {}, {}, {}", FuncScopeTemp{match_num}, case_info.label,
                cond_label);
  }
}

void QbeIrGenerator::Visit(const IdLabeledStmtNode& id_labeled_stmt) {
  WriteLabel_(user_defined::BlockLabel{id_labeled_stmt.label});
  id_labeled_stmt.stmt->Accept(*this);
}

void QbeIrGenerator::Visit(const CaseStmtNode& case_stmt) {
  assert(!switch_infos.empty());
  // The evaluation of the case expression is done in the condition part.
  auto case_label = BlockLabel{"switch_case", NextLabelNum()};
  switch_infos.back()->case_infos.push_back(
      CaseInfo{case_stmt.expr.get(), case_label});
  auto& this_case_info = switch_infos.back()->case_infos.back();
  WriteLabel_(this_case_info.label);
  case_stmt.stmt->Accept(*this);
}

void QbeIrGenerator::Visit(const DefaultStmtNode& default_stmt) {
  assert(!switch_infos.empty());
  auto default_label = BlockLabel{"switch_default", NextLabelNum()};
  WriteLabel_(default_label);
  switch_infos.back()->default_label = default_label;
  default_stmt.stmt->Accept(*this);
}

void QbeIrGenerator::Visit(const ExprStmtNode& expr_stmt) {
  expr_stmt.expr->Accept(*this);
}

void QbeIrGenerator::Visit(const InitExprNode& init_expr) {
  init_expr.expr->Accept(*this);
}

void QbeIrGenerator::Visit(const ArrDesNode& arr_des) {}

void QbeIrGenerator::Visit(const IdDesNode& id_des) {}

void QbeIrGenerator::Visit(const NullExprNode& null_expr) {
  /* do nothing */
}

void QbeIrGenerator::Visit(const IdExprNode& id_expr) {
  // If the id is a function, the result is the address of the function.
  if (id_expr.type->IsFunc()) {
    int res_num = NextLocalNum();
    // The function name is already a function pointer.
    WriteInstr_("{} =l copy {}", FuncScopeTemp{res_num},
                user_defined::GlobalPointer{id_expr.id});
    num_recorder.Record(res_num);
    return;
  }
  if (id_expr.is_global) {
    int reg_num = NextLocalNum();
    WriteInstr_("{} =w loadw {}", FuncScopeTemp{reg_num},
                user_defined::GlobalPointer{id_expr.id});
    num_recorder.Record(reg_num);
    reg_num_to_id[reg_num] = id_expr.id;
  } else {
    assert(id_to_num.count(id_expr.id) != 0);
    /// @brief Plays the role of a "pointer". Its value has to be loaded to
    /// the register before use.
    int id_num = id_to_num.at(id_expr.id);
    int reg_num = NextLocalNum();
    if (id_expr.type->IsPtr() || id_expr.type->IsFunc()) {
      WriteInstr_("{} =l loadl {}", FuncScopeTemp{reg_num},
                  FuncScopeTemp{id_num});
    } else {
      WriteInstr_("{} =w loadw {}", FuncScopeTemp{reg_num},
                  FuncScopeTemp{id_num});
    }
    num_recorder.Record(reg_num);
    // Map the temporary reg_num to id_num, so that upper level nodes can store
    // value to id_num instead of reg_num.
    reg_num_to_id_num[reg_num] = id_num;
  }
}

void QbeIrGenerator::Visit(const IntConstExprNode& int_expr) {
  if (int_expr.is_global) {
    GlobalVarInitVal val = {.ival = int_expr.val};
    global_var_init_val.push_back(val);
  } else {
    int num = NextLocalNum();
    WriteInstr_("{} =w copy {}", FuncScopeTemp{num}, int_expr.val);
    num_recorder.Record(num);
  }
}

void QbeIrGenerator::Visit(const ArgExprNode& arg_expr) {
  arg_expr.arg->Accept(*this);
}

void QbeIrGenerator::Visit(const ArrSubExprNode& arr_sub_expr) {
  arr_sub_expr.arr->Accept(*this);
  const int reg_num = num_recorder.NumOfPrevExpr();
  arr_sub_expr.index->Accept(*this);
  const int index_num = num_recorder.NumOfPrevExpr();

  // extend word to long
  const int extended_num = NextLocalNum();
  WriteInstr_("{} =l extsw {}", FuncScopeTemp{extended_num},
              FuncScopeTemp{index_num});

  // offset = index number * element size
  // e.g. int a[3]
  // a[1]'s offset = 1 * 4 (int size)
  const int offset = NextLocalNum();
  const auto* arr_type = dynamic_cast<ArrType*>((arr_sub_expr.arr->type).get());
  assert(arr_type);
  WriteInstr_("{} =l mul {}, {}", FuncScopeTemp{offset},
              FuncScopeTemp{extended_num}, arr_type->element_type().size());

  // res_addr = base_addr + offset
  const int res_addr_num = NextLocalNum();
  if (arr_sub_expr.is_global) {
    const auto id = reg_num_to_id.at(reg_num);
    WriteInstr_("{} =l add {}, {}", FuncScopeTemp{res_addr_num},
                user_defined::GlobalPointer{id}, FuncScopeTemp{offset});
  } else {
    // address of the first element
    const int base_addr = reg_num_to_id_num.at(reg_num);
    WriteInstr_("{} =l add {}, {}", FuncScopeTemp{res_addr_num},
                FuncScopeTemp{base_addr}, FuncScopeTemp{offset});
  }

  // load value from res_addr
  const int res_num = NextLocalNum();
  WriteInstr_("{} =w loadw {}", FuncScopeTemp{res_num},
              FuncScopeTemp{res_addr_num});
  reg_num_to_id_num[res_num] = res_addr_num;
  num_recorder.Record(res_num);
}

void QbeIrGenerator::Visit(const CondExprNode& cond_expr) {
  cond_expr.predicate->Accept(*this);
  const int first_num = num_recorder.NumOfPrevExpr();
  // The second operand is evaluated only if the first compares unequal to
  // 0; the third operand is evaluated only if the first compares equal to
  // 0; the result is the value of the second or third operand (whichever is
  // evaluated).
  const int label_num = NextLabelNum();
  auto second_label = BlockLabel{"cond_second", label_num};
  auto third_label = BlockLabel{"cond_third", label_num};
  auto end_label = BlockLabel{"cond_end", label_num};
  const int first_res = NextLocalNum();
  WriteInstr_("{} =w {} {}, 0", FuncScopeTemp{first_res},
              GetBinaryOperator(BinaryOperator::kNeq),
              FuncScopeTemp{first_num});
  WriteInstr_("jnz {}, {}, {}", FuncScopeTemp{first_res}, second_label,
              third_label);
  const int res_num = NextLocalNum();
  WriteLabel_(second_label);
  cond_expr.then->Accept(*this);
  const int second_num = num_recorder.NumOfPrevExpr();
  WriteInstr_("{} =w copy {}", FuncScopeTemp{res_num},
              FuncScopeTemp{second_num});
  WriteInstr_("jmp {}", end_label);
  WriteLabel_(third_label);
  cond_expr.or_else->Accept(*this);
  const int third_num = num_recorder.NumOfPrevExpr();
  WriteInstr_("{} =w copy {}", FuncScopeTemp{res_num},
              FuncScopeTemp{third_num});
  WriteLabel_(end_label);
  num_recorder.Record(res_num);
}

void QbeIrGenerator::Visit(const FuncCallExprNode& call_expr) {
  call_expr.func_expr->Accept(*this);
  const int func_num = num_recorder.NumOfPrevExpr();

  // Evaluate the arguments.
  std::vector<int> arg_nums{};
  for (const auto& arg : call_expr.args) {
    arg->Accept(*this);
    const int arg_num = num_recorder.NumOfPrevExpr();
    arg_nums.push_back(arg_num);
  }

  const int res_num = NextLocalNum();
  Write_(kIndentStr);
  if (const auto* id_expr =
          dynamic_cast<IdExprNode*>(call_expr.func_expr.get());
      id_expr && id_expr->id == "__builtin_print") {
    Write_("{} =w call $printf(", FuncScopeTemp{res_num});
    Write_("l {}, ", user_defined::GlobalPointer{"__builtin_print_format"});
  } else {
    // Call the function through its address.
    Write_("{} =w call {}(", FuncScopeTemp{res_num}, FuncScopeTemp{func_num});
  }
  // Traverse the argument number along with the argument to get the type.
  for (auto i = size_t{0}, e = arg_nums.size(); i < e; ++i) {
    if (call_expr.args.at(i)->type->IsPtr()) {
      Write_("l %.{}", arg_nums.at(i));
    } else {
      Write_("w %.{}", arg_nums.at(i));
    }
    if (i != e - 1) {
      Write_(", ");
    }
  }
  Write_(")\n");
  num_recorder.Record(res_num);
}

void QbeIrGenerator::Visit(const PostfixArithExprNode& postfix_expr) {
  // The result of the postfix ++ operator is the value of the operand. As a
  // side effect, the value of the operand object is incremented ( that is, the
  // value 1 of the appropriate type is added to it).
  // The postfix -- operator is analogous to the postfix ++ operator, except
  // that the value of the operand is decremented (that is, the value 1 of the
  // appropriate type is subtracted from it).
  postfix_expr.operand->Accept(*this);
  const int expr_num = num_recorder.NumOfPrevExpr();
  num_recorder.Record(expr_num);

  const int res_num = NextLocalNum();
  const auto arith_op = postfix_expr.op == PostfixOperator::kIncr
                            ? BinaryOperator::kAdd
                            : BinaryOperator::kSub;

  // TODO: support pointer arithmetic
  WriteInstr_("{} =w {} {}, 1", FuncScopeTemp{res_num},
              GetBinaryOperator(arith_op), FuncScopeTemp{expr_num});
  const auto* id_expr = dynamic_cast<IdExprNode*>((postfix_expr.operand).get());
  assert(id_expr);
  WriteInstr_("storew {}, {}", FuncScopeTemp{res_num},
              FuncScopeTemp{id_to_num.at(id_expr->id)});
}

void QbeIrGenerator::Visit(const RecordMemExprNode& mem_expr) {
  mem_expr.expr->Accept(*this);
  const auto num = num_recorder.NumOfPrevExpr();
  const auto id_num = reg_num_to_id_num.at(num);
  auto* record_type = dynamic_cast<RecordType*>(mem_expr.expr->type.get());
  assert(record_type);

  const auto res_addr_num = NextLocalNum();
  WriteInstr_("{} =l add {}, {}", FuncScopeTemp{res_addr_num},
              FuncScopeTemp{id_num}, record_type->OffsetOf(mem_expr.id));

  const int res_num = NextLocalNum();
  WriteInstr_("{} =w loadw {}", FuncScopeTemp{res_num},
              FuncScopeTemp{res_addr_num});
  reg_num_to_id_num[res_num] = res_addr_num;
  num_recorder.Record(res_num);
}

void QbeIrGenerator::Visit(const UnaryExprNode& unary_expr) {
  unary_expr.operand->Accept(*this);
  switch (unary_expr.op) {
    case UnaryOperator::kIncr:
    case UnaryOperator::kDecr: {
      // Equivalent to i += 1 or i -= 1.
      const int expr_num = num_recorder.NumOfPrevExpr();
      const int res_num = NextLocalNum();
      const auto arith_op = unary_expr.op == UnaryOperator::kIncr
                                ? BinaryOperator::kAdd
                                : BinaryOperator::kSub;
      WriteInstr_("{} =w {} {}, 1", FuncScopeTemp{res_num},
                  GetBinaryOperator(arith_op), FuncScopeTemp{expr_num});
      const auto* id_expr =
          dynamic_cast<IdExprNode*>((unary_expr.operand).get());
      assert(id_expr);
      WriteInstr_("storew {}, {}", FuncScopeTemp{res_num},
                  FuncScopeTemp{id_to_num.at(id_expr->id)});
      num_recorder.Record(res_num);
    } break;
    case UnaryOperator::kPos:
      // Do nothing.
      break;
    case UnaryOperator::kNeg: {
      const int expr_num = num_recorder.NumOfPrevExpr();
      const int res_num = NextLocalNum();
      WriteInstr_("{} =w neg {}", FuncScopeTemp{res_num},
                  FuncScopeTemp{expr_num});
      num_recorder.Record(res_num);
    } break;
    case UnaryOperator::kNot: {
      // Is 0 if the value of its operand compares unequal to 0, 1 if the value
      // of its operand compares equal to 0.
      // The expression !E is equivalent to (0 == E).
      const int expr_num = num_recorder.NumOfPrevExpr();
      const int res_num = NextLocalNum();
      WriteInstr_("{} =w {} {}, 0", FuncScopeTemp{res_num},
                  GetBinaryOperator(BinaryOperator::kEq),
                  FuncScopeTemp{expr_num});
      num_recorder.Record(res_num);
    } break;
    case UnaryOperator::kBitComp: {
      const int expr_num = num_recorder.NumOfPrevExpr();
      const int res_num = NextLocalNum();
      // Exclusive or with all ones to flip the bits.
      WriteInstr_("{} =w xor {}, -1", FuncScopeTemp{res_num},
                  FuncScopeTemp{expr_num});
      num_recorder.Record(res_num);
    } break;
    case UnaryOperator::kAddr: {
      if (unary_expr.operand->type->IsFunc()) {
        // No-op; the function itself already evaluates to the address.
        break;
      }
      const auto* id_expr =
          dynamic_cast<IdExprNode*>((unary_expr.operand).get());
      // NOTE: The operand of the address-of operator must be an lvalue, and we
      // do not support arrays now, so it must have been backed by an id.
      assert(id_expr);
      // The address of the id is the id itself.
      const int reg_num = num_recorder.NumOfPrevExpr();
      const int id_num = reg_num_to_id_num.at(reg_num);
      // Since each expression has to generate a temporary, we need to copy the
      // id to a new temporary, and update the mapping.
      const int res_num = NextLocalNum();
      WriteInstr_("{} =l copy {}", FuncScopeTemp{res_num},
                  FuncScopeTemp{id_num});
      reg_num_to_id_num[res_num] = id_num;
      num_recorder.Record(res_num);
    } break;
    case UnaryOperator::kDeref: {
      // Is function pointer.
      if (unary_expr.operand->type->IsPtr() &&
          dynamic_cast<PtrType*>((unary_expr.operand->type).get())
              ->base_type()
              .IsFunc()) {
        // No-op; the function itself also evaluates to the address.
        break;
      }

      // Lhs can use res_num to map to the address, which reg_num currently
      // holds.
      const int reg_num = num_recorder.NumOfPrevExpr();
      const int res_num = NextLocalNum();
      // The result might yet be another pointer if the operand is a pointer to
      // a pointer.
      if (unary_expr.type->IsPtr()) {
        WriteInstr_("{} =l loadl {}", FuncScopeTemp{res_num},
                    FuncScopeTemp{reg_num});
      } else {
        WriteInstr_("{} =w loadw {}", FuncScopeTemp{res_num},
                    FuncScopeTemp{reg_num});
      }
      num_recorder.Record(res_num);
      reg_num_to_id_num[res_num] = reg_num;
    } break;
    default:
      break;
  }
}

void QbeIrGenerator::Visit(const BinaryExprNode& bin_expr) {
  bin_expr.lhs->Accept(*this);

  if (bin_expr.op == BinaryOperator::kComma) {
    // For the comma operator, the value of its left operand is not used and can
    // be eliminated if it has no side effects or if its definition is
    // immediately dead. However, we leave these optimizations to QBE.
    bin_expr.rhs->Accept(*this);
    const int right_num = num_recorder.NumOfPrevExpr();
    num_recorder.Record(right_num);
    return;
  }

  const int left_num = num_recorder.NumOfPrevExpr();
  // Due to the lack of direct support for logical operators in QBE, we
  // implement logical expressions using comparison and jump instructions.
  if (bin_expr.op == BinaryOperator::kLand ||
      bin_expr.op == BinaryOperator::kLor) {
    // The && operator shall yield 1 if both of its operands compare unequal to
    // 0; otherwise, it yields 0; The || operator shall yield 1 if either of its
    // operands compare unequal to 0; otherwise, it yields 0.
    const int label_num = NextLabelNum();
    auto rhs_label = BlockLabel{"logic_rhs", label_num};
    // Early exit after evaluating the first operand.
    auto short_circuit_label = BlockLabel{"short_circuit", label_num};
    auto end_label = BlockLabel{"logic_end", label_num};
    const int left_res = NextLocalNum();
    // NOTE: (&& operator) If the first operand compares equal to 0, the second
    // operand is not evaluated. (|| operator)  If the first operand compares
    // unequal to 0, the second operand is not evaluated.
    WriteInstr_("{} =w {} {}, 0", FuncScopeTemp{left_res},
                bin_expr.op == BinaryOperator::kLand
                    ? GetBinaryOperator(BinaryOperator::kNeq)
                    : GetBinaryOperator(BinaryOperator::kEq),
                FuncScopeTemp{left_num});
    WriteInstr_("jnz {}, {}, {}", FuncScopeTemp{left_res}, rhs_label,
                short_circuit_label);
    WriteLabel_(rhs_label);
    const int res_num = NextLocalNum();
    bin_expr.rhs->Accept(*this);
    const int right_num = num_recorder.NumOfPrevExpr();
    WriteInstr_("{} =w {} {}, 0", FuncScopeTemp{res_num},
                GetBinaryOperator(BinaryOperator::kNeq),
                FuncScopeTemp{right_num});
    WriteInstr_("jmp {}", end_label);
    WriteLabel_(short_circuit_label);
    WriteInstr_("{} =w copy {}", FuncScopeTemp{res_num},
                bin_expr.op == BinaryOperator::kLand ? 0 : 1);
    WriteLabel_(end_label);
    num_recorder.Record(res_num);
  } else {
    const int num = NextLocalNum();
    // TODO: use the correct instruction for specific data type:
    // 1. signed or unsigned: currently only supports signed integers.
    // 2. QBE base data type 'w' | 'l' | 's' | 'd': currently only supports
    // 'w'.
    bin_expr.rhs->Accept(*this);
    const int right_num = num_recorder.NumOfPrevExpr();
    WriteInstr_("{} =w {} {}, {}", FuncScopeTemp{num},
                GetBinaryOperator(bin_expr.op), FuncScopeTemp{left_num},
                FuncScopeTemp{right_num});
    num_recorder.Record(num);
  }
}

void QbeIrGenerator::Visit(const SimpleAssignmentExprNode& assign_expr) {
  assign_expr.lhs->Accept(*this);
  int lhs_num = num_recorder.NumOfPrevExpr();
  assign_expr.rhs->Accept(*this);
  int rhs_num = num_recorder.NumOfPrevExpr();
  if (assign_expr.lhs->type->IsPtr()) {
    // Assign pointer address to another pointer.
    WriteInstr_("storel {}, {}", FuncScopeTemp{rhs_num},
                FuncScopeTemp{reg_num_to_id_num.at(lhs_num)});
  } else {
    WriteInstr_("storew {}, {}", FuncScopeTemp{rhs_num},
                FuncScopeTemp{reg_num_to_id_num.at(lhs_num)});
  }
  num_recorder.Record(rhs_num);
}

void QbeIrGenerator::VWrite_(fmt::string_view format, fmt::format_args args) {
  fmt::vprint(output_, format, args);
}
