#include "qbe_ir_generator.hpp"

#include <fmt/core.h>
#include <fmt/ostream.h>

#include <cassert>
#include <cstddef>
#include <map>
#include <memory>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "ast.hpp"
#include "operator.hpp"
#include "qbe/sigil.hpp"

using namespace qbe;

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
    default:
      return "Unknown";
  }
}

auto id_to_num  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables):
                // Accessible only within this translation unit; declaring as a
                // data member introduces unnecessary dependency.
    = std::map<std::string, int>{};

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

/// @note Labels are stored as string_views since `BlockLabel` is not copyable.
/// The caller is responsible for ensuring that the lifetime of entry and exit
/// spans the lifetime of this object. Remember to reconstruct a `BlockLabel`
/// from the string_view before using it.
struct LabelViewPair {
  std::string_view entry;
  std::string_view exit;
};

/// @note Blocks that allows jumping within or out of it should add its labels
/// to this list.
auto
    label_views_of_jumpable_blocks  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)
    = std::vector<LabelViewPair>{};

}  // namespace

void QbeIrGenerator::Visit(const DeclNode& decl) {
  int id_num = NextLocalNum();
  WriteInstr_("{} =l alloc4 4", FuncScopeTemp{id_num});

  if (decl.init) {
    decl.init->Accept(*this);
    int init_num = num_recorder.NumOfPrevExpr();
    WriteInstr_("storew {}, {}", FuncScopeTemp{init_num},
                FuncScopeTemp{id_num});
  }
  // Set up the number of the id so we know were to load it back.
  id_to_num[decl.id] = id_num;
}

void QbeIrGenerator::Visit(const ParamNode& parameter) {
  int id_num = NextLocalNum();
  // TODO: support different data types
  Write_("w %.{}", id_num);
  id_to_num[parameter.id] = id_num;
}

void QbeIrGenerator::AllocMemForParams_(
    const std::vector<std::unique_ptr<ParamNode>>& parameters) {
  for (const auto& parameter : parameters) {
    int id_num = id_to_num.at(parameter->id);
    int reg_num = NextLocalNum();
    WriteInstr_("{} =l alloc4 4", FuncScopeTemp{reg_num});
    WriteInstr_("storew {}, {}", FuncScopeTemp{id_num}, FuncScopeTemp{reg_num});
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
  for (const auto& item : compound_stmt.items) {
    std::visit([this](auto&& item) { item->Accept(*this); }, item);
  }
}

void QbeIrGenerator::Visit(const ProgramNode& program) {
  // Generate the data of builtin functions.
  Write_("data {} = align 1 {{ b \"%d\\012\\000\", }}\n",
         GlobalPointer{"builtin_print_format"});

  for (const auto& func_def : program.func_def_list) {
    func_def->Accept(*this);
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
      {.entry = pred_label.name(), .exit = end_label.name()});
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
      {.entry = step_label.name(), .exit = end_label.name()});
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
  WriteInstr_("jmp @{}", goto_stmt.label);
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
  const ExprNode* expr;
  std::shared_ptr<BlockLabel> label;
};

struct SwitchInfo {
  std::vector<CaseInfo> case_infos{};
  /// @note `nullptr` if no default label exists.
  std::shared_ptr<BlockLabel> default_label = nullptr;
  std::shared_ptr<BlockLabel> exit_label;
};

/// @brief The shared states passed around during the generation of a switch.
/// @note To allow nested switch statements, the information is stacked.
auto switch_infos  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)
    = std::vector<std::shared_ptr<SwitchInfo>>{};

/// @brief The label of the next condition depends on whether we've already
/// handled the last one and whether there is a default label.
std::unique_ptr<BlockLabel> GetNextCondLabel(
    bool is_last_cond, const std::shared_ptr<BlockLabel>& default_label) {
  if (is_last_cond && default_label) {
    return std::make_unique<BlockLabel>(default_label->name());
  }
  if (is_last_cond) {
    return std::make_unique<BlockLabel>(
        switch_infos.back()->exit_label->name());
  }
  return std::make_unique<BlockLabel>("switch_cond", NextLabelNum());
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
  switch_infos.push_back(std::make_shared<SwitchInfo>());
  switch_stmt.ctrl->Accept(*this);
  const auto ctrl_num = num_recorder.NumOfPrevExpr();
  auto cond_label = BlockLabel{"switch_cond", NextLabelNum()};
  WriteInstr_("jmp {}", cond_label);

  GenerateCases_(switch_stmt);
  GenerateConditions_(switch_stmt, cond_label, ctrl_num);

  WriteLabel_(*switch_infos.back()->exit_label);
  switch_infos.pop_back();
}

void QbeIrGenerator::GenerateCases_(const SwitchStmtNode& switch_stmt) {
  auto this_switch_info = switch_infos.back();
  this_switch_info->exit_label =
      std::make_shared<BlockLabel>("switch_exit", NextLabelNum());
  label_views_of_jumpable_blocks.push_back(
      {// The break statement only jumps to the exit label; the entry label is
       // irrelevant.
       .entry = "",
       .exit = this_switch_info->exit_label->name()});
  switch_stmt.stmt->Accept(*this);
  label_views_of_jumpable_blocks.pop_back();
  WriteLabel_(BlockLabel{"switch_bottom", NextLabelNum()});
  WriteInstr_("jmp {}", *this_switch_info->exit_label);
}

void QbeIrGenerator::GenerateConditions_(const SwitchStmtNode& switch_stmt,
                                         const BlockLabel& first_cond_label,
                                         int ctrl_num) {
  auto this_switch_info = switch_infos.back();
  auto cond_label = std::make_unique<BlockLabel>(first_cond_label.name());
  for (auto i = std::size_t{0}, e = this_switch_info->case_infos.size(); i < e;
       ++i) {
    WriteLabel_(*cond_label);
    const auto& case_info = this_switch_info->case_infos.at(i);
    case_info.expr->Accept(*this);
    const auto expr_num = num_recorder.NumOfPrevExpr();
    const auto match_num = NextLocalNum();
    WriteInstr_("{} =w ceqw {}, {}", FuncScopeTemp{match_num},
                FuncScopeTemp{ctrl_num}, FuncScopeTemp{expr_num});
    const auto is_last_cond = i == e - 1;
    cond_label =
        GetNextCondLabel(is_last_cond, this_switch_info->default_label);
    WriteInstr_("jnz {}, {}, {}", FuncScopeTemp{match_num}, *case_info.label,
                *cond_label);
  }
}

void QbeIrGenerator::Visit(const IdLabeledStmtNode& id_labeled_stmt) {
  Write_("@{}\n", id_labeled_stmt.label);
  id_labeled_stmt.stmt->Accept(*this);
}

void QbeIrGenerator::Visit(const CaseStmtNode& case_stmt) {
  assert(!switch_infos.empty());
  // The evaluation of the case expression is done in the condition part.
  auto case_label = std::make_shared<BlockLabel>("switch_case", NextLabelNum());
  switch_infos.back()->case_infos.push_back(
      CaseInfo{case_stmt.expr.get(), case_label});
  auto& this_case_info = switch_infos.back()->case_infos.back();
  WriteLabel_(*this_case_info.label);
  case_stmt.stmt->Accept(*this);
}

void QbeIrGenerator::Visit(const DefaultStmtNode& default_stmt) {
  assert(!switch_infos.empty());
  auto this_switch_info = switch_infos.back();
  this_switch_info->default_label =
      std::make_shared<BlockLabel>("switch_default", NextLabelNum());
  WriteLabel_(*this_switch_info->default_label);
  default_stmt.stmt->Accept(*this);
}

void QbeIrGenerator::Visit(const ExprStmtNode& expr_stmt) {
  expr_stmt.expr->Accept(*this);
}

void QbeIrGenerator::Visit(const NullExprNode& null_expr) {
  /* do nothing */
}

void QbeIrGenerator::Visit(const IdExprNode& id_expr) {
  /// @brief Plays the role of a "pointer". Its value has to be loaded to
  /// the register before use.
  assert(id_to_num.count(id_expr.id) != 0);
  int id_num = id_to_num.at(id_expr.id);
  int reg_num = NextLocalNum();
  WriteInstr_("{} =w loadw {}", FuncScopeTemp{reg_num}, FuncScopeTemp{id_num});
  num_recorder.Record(reg_num);
}

void QbeIrGenerator::Visit(const IntConstExprNode& int_expr) {
  int num = NextLocalNum();
  WriteInstr_("{} =w copy {}", FuncScopeTemp{num}, int_expr.val);
  num_recorder.Record(num);
}

void QbeIrGenerator::Visit(const ArgExprNode& arg_expr) {
  arg_expr.arg->Accept(*this);
}

void QbeIrGenerator::Visit(const FuncCallExprNode& call_expr) {
  const auto* id_expr = dynamic_cast<IdExprNode*>((call_expr.func_expr).get());
  assert(id_expr);
  const int res_num = NextLocalNum();

  std::vector<int> arg_nums{};
  for (const auto& arg : call_expr.args) {
    arg->Accept(*this);
    const int arg_num = num_recorder.NumOfPrevExpr();
    arg_nums.push_back(arg_num);
  }

  Write_(kIndentStr);
  if (id_expr->id == "__builtin_print") {
    Write_("{} =w call $printf(", FuncScopeTemp{res_num});
    Write_("l {}, ", GlobalPointer{"builtin_print_format"});
  } else {
    Write_("{} =w call ${}(", FuncScopeTemp{res_num}, id_expr->id);
  }
  for (const auto& arg_num : arg_nums) {
    Write_("w %.{}", arg_num);
    if (arg_num != arg_nums.back()) {
      Write_(", ");
    }
  }
  Write_(")\n");
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
    default:
      break;
  }
}

void QbeIrGenerator::Visit(const BinaryExprNode& bin_expr) {
  bin_expr.lhs->Accept(*this);
  int left_num = num_recorder.NumOfPrevExpr();
  bin_expr.rhs->Accept(*this);
  int right_num = num_recorder.NumOfPrevExpr();
  int num = NextLocalNum();
  // TODO: use the correct instruction for specific data type:
  // 1. signed or unsigned: currently only supports signed integers.
  // 2. QBE base data type 'w' | 'l' | 's' | 'd': currently only supports 'w'.
  WriteInstr_("{} =w {} {}, {}", FuncScopeTemp{num},
              GetBinaryOperator(bin_expr.op), FuncScopeTemp{left_num},
              FuncScopeTemp{right_num});
  num_recorder.Record(num);
}

void QbeIrGenerator::Visit(const SimpleAssignmentExprNode& assign_expr) {
  assign_expr.expr->Accept(*this);
  int expr_num = num_recorder.NumOfPrevExpr();
  WriteInstr_("storew {}, {}", FuncScopeTemp{expr_num},
              FuncScopeTemp{id_to_num.at(assign_expr.id)});
  num_recorder.Record(expr_num);
}

void QbeIrGenerator::VWrite_(fmt::string_view format, fmt::format_args args) {
  fmt::vprint(output_, format, args);
}
