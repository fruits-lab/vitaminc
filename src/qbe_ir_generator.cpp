#include "qbe_ir_generator.hpp"

#include <cassert>
#include <fstream>
#include <map>
#include <memory>
#include <string>

#include "ast.hpp"
#include "visitor.hpp"

/// @brief qbe intermediate file
extern std::ofstream output;

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

/// @brief Returns the function-scope temporary with sigil (`%`).
std::string PrefixSigil(int local_num) {
  return "%." + std::to_string(local_num);
}

/// @brief Returns a label with prefix (`@`).
std::string PrefixLabel(const std::string& name, int label_num) {
  return "@" + name + "." + std::to_string(label_num);
}

class OpNameGetter {
 public:
  /// @return The name of the operator used in the QBE IR, e.g., `add`.
  std::string OpNameOf(const BinaryExprNode& bin_expr);

  OpNameGetter();

 private:
  /// @note An alternative approach would be to directly implement
  /// `OpNameGetterImpl` as a `Visitor`, but this is intended to be used
  /// exclusively with binary expressions. Therefore, we encapsulate it to
  /// prevent unintended usage in other contexts. We also employ the Pimpl
  /// idiom, allowing deferred implementation details later in this file.
  class OpNameGetterImpl;
  std::unique_ptr<OpNameGetterImpl> impl_;
};

std::map<std::string, int> id_to_num{};

/// @brief Every expression generates a temporary. The local number of such
/// temporary should be stored, so can propagate to later uses.
class PrevExprNumRecorder {
 public:
  void Record(int num) {
    num_of_prev_expr = num;
  }

  /// @note The local number can only be gotten once. This is to reduce the
  /// possibility of getting obsolete number.
  int NumOfPrevExpr() {
    assert(num_of_prev_expr != kNoRecord);
    int tmp = num_of_prev_expr;
    num_of_prev_expr = kNoRecord;
    return tmp;
  }

 private:
  static constexpr int kNoRecord = -1;
  int num_of_prev_expr = kNoRecord;
};

auto num_recorder = PrevExprNumRecorder{};

}  // namespace

void QbeIrGenerator::Visit(const DeclNode& decl) {
  int id_num = NextLocalNum();
  output << PrefixSigil(id_num) << " =l alloc4 4" << std::endl;

  if (decl.init) {
    decl.init->Accept(*this);
    int init_num = num_recorder.NumOfPrevExpr();
    output << "storew " << PrefixSigil(init_num) << ", " << PrefixSigil(id_num)
           << std::endl;
  }
  // Set up the number of the id so we know were to load it back.
  id_to_num[decl.id] = id_num;
}

void QbeIrGenerator::Visit(const BlockStmtNode& block) {
  // Note: BlockStmtNode cannot output the correct label to its own block
  // because it doesn't know whether it is a if statement body or a function.
  // Thus, by moving label creation to an upper level, each block can have its
  // correct starting label.
  for (const auto& decl : block.decls) {
    decl->Accept(*this);
  }
  for (const auto& stmt : block.stmts) {
    stmt->Accept(*this);
  }
}

void QbeIrGenerator::Visit(const ProgramNode& program) {
  output << "export function w $main() {" << std::endl;
  output << "@start" << std::endl;
  program.block->Accept(*this);
  output << "}";
}

void QbeIrGenerator::Visit(const NullStmtNode&) {
  /* do nothing */
}

void QbeIrGenerator::Visit(const IfStmtNode& if_stmt) {
  if_stmt.predicate->Accept(*this);
  int predicate_num = num_recorder.NumOfPrevExpr();
  int label_num = NextLabelNum();
  std::string then_label = PrefixLabel("then", label_num);
  std::string else_label = PrefixLabel("else", label_num);
  std::string end_label = PrefixLabel("end", label_num);

  // Jumps to "then" if the predicate is true (non-zero), else jumps to "else".
  // If no "else" exists, falls through to "end".
  // If "else" exists, a second jump is needed after executing "then" to skip
  // it, as the generated code for "else" follows immediately after "then".
  output << "# if" << std::endl;
  output << "jnz " << PrefixSigil(predicate_num) << ", " << then_label << ", ";
  if (if_stmt.or_else) {
    output << else_label << std::endl;
  } else {
    output << end_label << std::endl;
  }

  output << then_label << std::endl;
  if_stmt.then->Accept(*this);
  if (if_stmt.or_else) {
    // Skip the "else" part after executing "then".
    output << "jmp " << end_label << std::endl;
    output << else_label << std::endl;
    if_stmt.or_else->Accept(*this);
  }
  output << end_label << std::endl;
}

void QbeIrGenerator::Visit(const WhileStmtNode& while_stmt) {
  int label_num = NextLabelNum();
  std::string body_label = PrefixLabel("loop_body", label_num);
  std::string pred_label = PrefixLabel("pred", label_num);
  std::string end_label = PrefixLabel("end", label_num);

  // A while statement's predicate is evaluated "before" the body statement,
  // whereas a do-while statement's predicate is evaluated "after" the body
  // statement. In the generated code for a while statement, there is an
  // unconditional jump at the end of the body to jump back to the predicate.
  // For a do-while statement, it only needs one conditional jump.
  if (!while_stmt.is_do_while) {
    output << pred_label << std::endl;
    while_stmt.predicate->Accept(*this);
    int predicate_num = num_recorder.NumOfPrevExpr();
    output << "jnz " << PrefixSigil(predicate_num) << ", " << body_label << ", "
           << end_label << std::endl;
  }
  output << body_label << std::endl;
  while_stmt.loop_body->Accept(*this);
  if (!while_stmt.is_do_while) {
    output << "jmp " << pred_label << std::endl;
  } else {
    while_stmt.predicate->Accept(*this);
    int predicate_num = num_recorder.NumOfPrevExpr();
    output << "jnz " << PrefixSigil(predicate_num) << ", " << body_label << ", "
           << end_label << std::endl;
  }
  output << end_label << std::endl;
}

void QbeIrGenerator::Visit(const ReturnStmtNode& ret_stmt) {
  ret_stmt.expr->Accept(*this);
  int ret_num = num_recorder.NumOfPrevExpr();
  output << " ret " << PrefixSigil(ret_num) << std::endl;
}

void QbeIrGenerator::Visit(const ExprStmtNode& expr_stmt) {
  expr_stmt.expr->Accept(*this);
}

void QbeIrGenerator::Visit(const IdExprNode& id_expr) {
  /// @brief Plays the role of a "pointer". Its value has to be loaded to
  /// the register before use.
  int id_num = id_to_num.at(id_expr.id);
  int reg_num = NextLocalNum();
  output << PrefixSigil(reg_num) << " =w loadw " << PrefixSigil(id_num)
         << std::endl;
  num_recorder.Record(reg_num);
}

void QbeIrGenerator::Visit(const IntConstExprNode& int_expr) {
  int num = NextLocalNum();
  output << PrefixSigil(num) << " =w copy " << int_expr.val << std::endl;
  num_recorder.Record(num);
}

void QbeIrGenerator::Visit(const BinaryExprNode& bin_expr) {
  bin_expr.lhs->Accept(*this);
  int left_num = num_recorder.NumOfPrevExpr();
  bin_expr.rhs->Accept(*this);
  int right_num = num_recorder.NumOfPrevExpr();
  int num = NextLocalNum();
  output << PrefixSigil(num) << " =w " << OpNameGetter{}.OpNameOf(bin_expr)
         << " " << PrefixSigil(left_num) << ", " << PrefixSigil(right_num)
         << std::endl;
  num_recorder.Record(num);
}

/// @brief Dispatch the concrete binary expressions to the parent
/// `BinaryExprNode`.
/// @param classname A subclass of `BinaryExprNode`.
#define DISPATCH_TO_VISIT_BINARY_EXPR(classname) \
  void QbeIrGenerator::Visit(const classname& expr) { \
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

void QbeIrGenerator::Visit(const SimpleAssignmentExprNode& assign_expr) {
  assign_expr.expr->Accept(*this);
  int expr_num = num_recorder.NumOfPrevExpr();
  output << "storew " << PrefixSigil(expr_num) << ", "
         << PrefixSigil(id_to_num.at(assign_expr.id)) << std::endl;
  num_recorder.Record(expr_num);
}

class OpNameGetter::OpNameGetterImpl : public NonModifyingVisitor {
 public:
  std::string OpName() const {
    return op_name_;
  }

  void Visit(const PlusExprNode&) override {
    op_name_ = "add";
  }

  void Visit(const SubExprNode&) override {
    op_name_ = "sub";
  }

  void Visit(const MulExprNode&) override {
    op_name_ = "mul";
  }

  void Visit(const DivExprNode&) override {
    op_name_ = "div";
  }

  void Visit(const ModExprNode&) override {
    op_name_ = "rem";
  }

  void Visit(const GreaterThanExprNode&) override {
    op_name_ = "csgtw";
  }

  void Visit(const GreaterThanOrEqualToExprNode&) override {
    op_name_ = "csgew";
  }

  void Visit(const LessThanExprNode&) override {
    op_name_ = "csltw";
  }

  void Visit(const LessThanOrEqualToExprNode&) override {
    op_name_ = "cslew";
  }

  void Visit(const EqualToExprNode&) override {
    op_name_ = "ceqw";
  }

  void Visit(const NotEqualToExprNode&) override {
    op_name_ = "cnew";
  }

 private:
  std::string op_name_;
};

std::string OpNameGetter::OpNameOf(const BinaryExprNode& bin_expr) {
  bin_expr.Accept(*impl_);
  return impl_->OpName();
}

OpNameGetter::OpNameGetter() : impl_{std::make_unique<OpNameGetterImpl>()} {}
