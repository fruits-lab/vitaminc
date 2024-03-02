#include "qbe_ir_generator.hpp"

#include <fmt/ostream.h>

#include <cassert>
#include <map>
#include <memory>
#include <string>
#include <variant>

#include "ast.hpp"
#include "operator.hpp"
#include "qbe/sigil.hpp"
#include "visitor.hpp"

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

void QbeIrGenerator::Visit(const FuncDefNode& func_def) {
  Write_(
      "export function w ${}() {{\n"
      "@start\n",
      func_def.id);
  func_def.body->Accept(*this);
  Write_("}}\n");
}

void QbeIrGenerator::Visit(const LoopInitNode& loop_init) {
  if (std::holds_alternative<std::unique_ptr<DeclNode>>(loop_init.clause)) {
    std::get<std::unique_ptr<DeclNode>>(loop_init.clause)->Accept(*this);
  } else {
    std::get<std::unique_ptr<ExprNode>>(loop_init.clause)->Accept(*this);
  }
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
  for (const auto& func_def : program.func_def_list) {
    func_def->Accept(*this);
  }

  Write_(
      "export function w $main() {{\n"
      "@start\n");
  program.body->Accept(*this);
  Write_("}}");
}

void QbeIrGenerator::Visit(const IfStmtNode& if_stmt) {
  if_stmt.predicate->Accept(*this);
  int predicate_num = num_recorder.NumOfPrevExpr();
  int label_num = NextLabelNum();
  auto then_label = BlockLabel{"then", label_num};
  auto else_label = BlockLabel{"else", label_num};
  auto end_label = BlockLabel{"end", label_num};

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
  auto body_label = BlockLabel{"loop_body", label_num};
  auto pred_label = BlockLabel{"pred", label_num};
  auto end_label = BlockLabel{"end", label_num};

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

  auto pred_label = BlockLabel{"pred", label_num};
  auto body_label = BlockLabel{"loop_body", label_num};
  auto step_label = BlockLabel{"step", label_num};
  auto end_label = BlockLabel{"end", label_num};

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

void QbeIrGenerator::Visit(const BreakStmtNode& break_stmt) {
  assert(!label_views_of_jumpable_blocks.empty());
  WriteInstr_("jmp {}", BlockLabel{label_views_of_jumpable_blocks.back().exit});
}

void QbeIrGenerator::Visit(const ContinueStmtNode& continue_stmt) {
  assert(!label_views_of_jumpable_blocks.empty());
  WriteInstr_("jmp {}",
              BlockLabel{label_views_of_jumpable_blocks.back().entry});
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

void QbeIrGenerator::Visit(const FunCallExprNode& call_expr) {
  const auto* id_expr = dynamic_cast<IdExprNode*>((call_expr.func_expr).get());
  assert(id_expr);
  const int res_num = NextLocalNum();
  WriteInstr_("{} =w call ${}()", FuncScopeTemp{res_num}, id_expr->id);
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
