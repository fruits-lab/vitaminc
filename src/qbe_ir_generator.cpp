#include "qbe_ir_generator.hpp"

#include <fmt/ostream.h>

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

const int kIndentSize = 8;

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

void OutputNoIndent(const std::string& str) {
  fmt::print(output, "{}\n", str);
}

void OutputIndentComment(const std::string& comment) {
  fmt::print(output, "{:{}}", "", kIndentSize);
  fmt::print(output, "{}\n", comment);
}

void OutputIndentDeclare(const std::string& id, const std::string& data_type,
                         int alignment, int stack_size) {
  fmt::print(output, "{:{}}", "", kIndentSize);
  fmt::print(output, "{} ={} alloc{} {}\n", id, data_type, alignment,
             stack_size);
}

void OutputIndentStore(const std::string& data_type, const std::string& source,
                       const std::string& dest) {
  fmt::print(output, "{:{}}", "", kIndentSize);
  fmt::print(output, "store{} {}, {}\n", data_type, source, dest);
}

void OutputIndentLoad(const std::string& data_type, const std::string& source,
                      const std::string& dest) {
  fmt::print(output, "{:{}}", "", kIndentSize);
  fmt::print(output, "{} ={} load{} {}\n", dest, data_type, data_type, source);
}

void OutputIndentCopy(const std::string& data_type, int source,
                      const std::string& dest) {
  fmt::print(output, "{:{}}", "", kIndentSize);
  fmt::print(output, "{} ={} copy {}\n", dest, data_type, source);
}

void OutputIndentJumpConditional(const std::string& cond,
                                 const std::string& body_label,
                                 const std::string& end_label) {
  fmt::print(output, "{:{}}", "", kIndentSize);
  fmt::print(output, "jnz {}, {}, {}\n", cond, body_label, end_label);
}

void OutputIndentJumpUnconditional(const std::string& dest) {
  fmt::print(output, "{:{}}", "", kIndentSize);
  fmt::print(output, "jmp {}\n", dest);
}

void OutputIndentReturn(const std::string& val) {
  fmt::print(output, "{:{}}", "", kIndentSize);
  // TODO: return value is optional
  fmt::print(output, "ret {}\n", val);
}

void OutputIndentThreeAddr(const std::string& op, const std::string& data_type,
                           const std::string& dest,
                           const std::string& source_one,
                           const std::string& source_two) {
  fmt::print(output, "{:{}}", "", kIndentSize);
  fmt::print(output, "{} ={} {} {}, {}\n", dest, data_type, op, source_one,
             source_two);
}

class OpNameGetter {
 public:
  /// @return The name of the operator used in the QBE IR, e.g., `add`.
  std::string OpNameOf(const ExprNode& expr);

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
  // TODO: declare based on data types
  OutputIndentDeclare(PrefixSigil(id_num), "l", 4, 4);

  if (decl.init) {
    decl.init->Accept(*this);
    int init_num = num_recorder.NumOfPrevExpr();
    // TODO: store to memory based on identifier's data type
    OutputIndentStore("w", PrefixSigil(init_num), PrefixSigil(id_num));
  }
  // Set up the number of the id so we know were to load it back.
  id_to_num[decl.id] = id_num;
}

void QbeIrGenerator::Visit(const LoopInitNode& loop_init) {
  if (std::holds_alternative<std::unique_ptr<DeclNode>>(loop_init.clause)) {
    std::get<std::unique_ptr<DeclNode>>(loop_init.clause)->Accept(*this);
  } else {
    std::get<std::unique_ptr<ExprNode>>(loop_init.clause)->Accept(*this);
  }
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
  OutputNoIndent("export function w $main() {");
  OutputNoIndent("@start");
  program.block->Accept(*this);
  OutputNoIndent("}");
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
  OutputIndentComment("# if");
  std::string is_else = "";
  if (if_stmt.or_else) {
    is_else = else_label;
  } else {
    is_else = end_label;
  }
  OutputIndentJumpConditional(PrefixSigil(predicate_num), then_label, is_else);

  OutputNoIndent(then_label);
  if_stmt.then->Accept(*this);
  if (if_stmt.or_else) {
    // Skip the "else" part after executing "then".
    OutputIndentJumpUnconditional(end_label);
    OutputNoIndent(else_label);
    if_stmt.or_else->Accept(*this);
  }
  OutputNoIndent(end_label);
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
    OutputNoIndent(pred_label);
    while_stmt.predicate->Accept(*this);
    int predicate_num = num_recorder.NumOfPrevExpr();
    OutputIndentJumpConditional(PrefixSigil(predicate_num), body_label,
                                end_label);
  }
  OutputNoIndent(body_label);
  while_stmt.loop_body->Accept(*this);
  if (!while_stmt.is_do_while) {
    OutputIndentJumpUnconditional(pred_label);
  } else {
    while_stmt.predicate->Accept(*this);
    int predicate_num = num_recorder.NumOfPrevExpr();
    OutputIndentJumpConditional(PrefixSigil(predicate_num), body_label,
                                end_label);
  }
  OutputNoIndent(end_label);
}

void QbeIrGenerator::Visit(const ForStmtNode& for_stmt) {
  int label_num = NextLabelNum();
  std::string pred_label = PrefixLabel("pred", label_num);
  std::string body_label = PrefixLabel("loop_body", label_num);
  std::string end_label = PrefixLabel("end", label_num);

  // A for statement's loop initialization is the first clause to execute,
  // whereas a for statement's predicate specifies evaluation made before each
  // iteration. A step is an operation that is performed after each iteration.
  // Skip predicate generation if it is a null expression.
  OutputIndentComment("# loop init");
  for_stmt.loop_init->Accept(*this);
  OutputNoIndent(pred_label);
  for_stmt.predicate->Accept(*this);
  if (!dynamic_cast<NullExprNode*>((for_stmt.predicate).get())) {
    int predicate_num = num_recorder.NumOfPrevExpr();
    OutputIndentJumpConditional(PrefixSigil(predicate_num), body_label,
                                end_label);
  }
  OutputNoIndent(body_label);
  for_stmt.loop_body->Accept(*this);
  for_stmt.step->Accept(*this);
  OutputIndentJumpUnconditional(pred_label);
  OutputNoIndent(end_label);
}

void QbeIrGenerator::Visit(const ReturnStmtNode& ret_stmt) {
  ret_stmt.expr->Accept(*this);
  int ret_num = num_recorder.NumOfPrevExpr();
  OutputIndentReturn(PrefixSigil(ret_num));
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
  // TODO: load based on identifier's data type
  OutputIndentLoad("w", PrefixSigil(id_num), PrefixSigil(reg_num));
  num_recorder.Record(reg_num);
}

void QbeIrGenerator::Visit(const IntConstExprNode& int_expr) {
  int num = NextLocalNum();
  OutputIndentCopy("w", int_expr.val, PrefixSigil(num));
  num_recorder.Record(num);
}

void QbeIrGenerator::Visit(const UnaryExprNode& unary_expr) {
  unary_expr.operand->Accept(*this);
  // TODO: The evaluation of certain unary expressions are the same as simple
  // assignment. For instance, ++i is equivalant to i += 1. We need to handle
  // this case by case.
}

void QbeIrGenerator::Visit(const BinaryExprNode& bin_expr) {
  bin_expr.lhs->Accept(*this);
  int left_num = num_recorder.NumOfPrevExpr();
  bin_expr.rhs->Accept(*this);
  int right_num = num_recorder.NumOfPrevExpr();
  int num = NextLocalNum();
  // TODO: identify expression's data type
  OutputIndentThreeAddr(OpNameGetter{}.OpNameOf(bin_expr), "w",
                        PrefixSigil(num), PrefixSigil(left_num),
                        PrefixSigil(right_num));
  num_recorder.Record(num);
}

/// @brief Dispatch the concrete binary or unary expressions to the parent
/// `BinaryExprNode` or `UnaryExprNode`.
/// @param classname A subclass of `BinaryExprNode` or `UnaryExprNode`.
#define DISPATCH_TO_VISIT_EXPR(parentname, classname) \
  void QbeIrGenerator::Visit(const classname& expr) { \
    Visit(static_cast<const parentname&>(expr)); \
  }

DISPATCH_TO_VISIT_EXPR(BinaryExprNode, PlusExprNode);
DISPATCH_TO_VISIT_EXPR(BinaryExprNode, SubExprNode);
DISPATCH_TO_VISIT_EXPR(BinaryExprNode, MulExprNode);
DISPATCH_TO_VISIT_EXPR(BinaryExprNode, DivExprNode);
DISPATCH_TO_VISIT_EXPR(BinaryExprNode, ModExprNode);
DISPATCH_TO_VISIT_EXPR(BinaryExprNode, GreaterThanExprNode);
DISPATCH_TO_VISIT_EXPR(BinaryExprNode, GreaterThanOrEqualToExprNode);
DISPATCH_TO_VISIT_EXPR(BinaryExprNode, LessThanExprNode);
DISPATCH_TO_VISIT_EXPR(BinaryExprNode, LessThanOrEqualToExprNode);
DISPATCH_TO_VISIT_EXPR(BinaryExprNode, EqualToExprNode);
DISPATCH_TO_VISIT_EXPR(BinaryExprNode, NotEqualToExprNode);

DISPATCH_TO_VISIT_EXPR(UnaryExprNode, IncrExprNode);
DISPATCH_TO_VISIT_EXPR(UnaryExprNode, DecrExprNode);
DISPATCH_TO_VISIT_EXPR(UnaryExprNode, NegExprNode);
DISPATCH_TO_VISIT_EXPR(UnaryExprNode, AddrExprNode);
DISPATCH_TO_VISIT_EXPR(UnaryExprNode, DereferExprNode);
DISPATCH_TO_VISIT_EXPR(UnaryExprNode, NotExprNode);
DISPATCH_TO_VISIT_EXPR(UnaryExprNode, BitCompExprNode);

#undef DISPATCH_TO_VISIT_EXPR

void QbeIrGenerator::Visit(const SimpleAssignmentExprNode& assign_expr) {
  assign_expr.expr->Accept(*this);
  int expr_num = num_recorder.NumOfPrevExpr();
  // TODO: store to memory based on expression's data type
  OutputIndentStore("w", PrefixSigil(expr_num),
                    PrefixSigil(id_to_num.at(assign_expr.id)));
  num_recorder.Record(expr_num);
}

class OpNameGetter::OpNameGetterImpl : public NonModifyingVisitor {
 public:
  std::string OpName() const {
    return op_name_;
  }

  // TODO: Defer code generation implementation for unary expression since some
  // unary expressions may need more than one instruction to complete.
  void Visit(const IncrExprNode&) override {
    op_name_ = "";
  }

  void Visit(const DecrExprNode&) override {
    op_name_ = "";
  }

  void Visit(const NegExprNode&) override {
    op_name_ = "";
  }

  void Visit(const AddrExprNode&) override {
    op_name_ = "";
  }

  void Visit(const DereferExprNode&) override {
    op_name_ = "";
  }

  void Visit(const NotExprNode&) override {
    op_name_ = "";
  }

  void Visit(const BitCompExprNode&) override {
    op_name_ = "";
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

std::string OpNameGetter::OpNameOf(const ExprNode& expr) {
  expr.Accept(*impl_);
  return impl_->OpName();
}

OpNameGetter::OpNameGetter() : impl_{std::make_unique<OpNameGetterImpl>()} {}
