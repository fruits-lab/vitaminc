#include "code_generator.hpp"

#include <cassert>
#include <fstream>
#include <map>
#include <string>

#include "ast.hpp"

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

/// @brief Returns the function-scope temporary with sigil (`%`).
std::string PrefixSigil(int local_num) {
  return "%." + std::to_string(local_num);
}

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

void CodeGenerator::Visit(const DeclNode& decl) {
  int id_num = NextLocalNum();
  output << PrefixSigil(id_num) << " =l alloc4 4" << std::endl;

  if (decl.init_) {
    decl.init_->Accept(*this);
    int init_num = num_recorder.NumOfPrevExpr();
    output << "storew " << PrefixSigil(init_num) << ", " << PrefixSigil(id_num)
           << std::endl;
  }
  // Set up the number of the id so we know were to load it back.
  id_to_num[decl.id_] = id_num;
}

void CodeGenerator::Visit(const BlockStmtNode& block) {
  output << "@start" << std::endl;
  for (const auto& decl : block.decls_) {
    decl->Accept(*this);
  }
  for (const auto& stmt : block.stmts_) {
    stmt->Accept(*this);
  }
}

void CodeGenerator::Visit(const ProgramNode& program) {
  output << "export function w $main() {" << std::endl;
  program.block_->Accept(*this);
  output << "}";
}

void CodeGenerator::Visit(const NullStmtNode&) {
  /* do nothing */
}

void CodeGenerator::Visit(const ReturnStmtNode& ret_stmt) {
  ret_stmt.expr_->Accept(*this);
  int ret_num = num_recorder.NumOfPrevExpr();
  output << " ret " << PrefixSigil(ret_num) << std::endl;
}

void CodeGenerator::Visit(const ExprStmtNode& expr_stmt) {
  expr_stmt.expr_->Accept(*this);
}

void CodeGenerator::Visit(const IdExprNode& id_expr) {
  /// @brief Plays the role of a "pointer". Its value has to be loaded to
  /// the register before use.
  int id_num = id_to_num.at(id_expr.id_);
  int reg_num = NextLocalNum();
  output << PrefixSigil(reg_num) << " =w loadw " << PrefixSigil(id_num)
         << std::endl;
  num_recorder.Record(reg_num);
}

void CodeGenerator::Visit(const IntConstExprNode& int_expr) {
  int num = NextLocalNum();
  output << PrefixSigil(num) << " =w copy " << int_expr.val_ << std::endl;
  num_recorder.Record(num);
}

void CodeGenerator::Visit(const BinaryExprNode& bin_expr) {
  bin_expr.lhs_->Accept(*this);
  int left_num = num_recorder.NumOfPrevExpr();
  bin_expr.rhs_->Accept(*this);
  int right_num = num_recorder.NumOfPrevExpr();
  int num = NextLocalNum();
  output << PrefixSigil(num) << " =w " << bin_expr.OpName_() << " "
         << PrefixSigil(left_num) << ", " << PrefixSigil(right_num)
         << std::endl;
  num_recorder.Record(num);
}

/// @brief Dispatch the concrete binary expressions to the parent
/// `BinaryExprNode`.
/// @param classname A subclass of `BinaryExprNode`.
#define DISPATCH_TO_VISIT_BINARY_EXPR(classname) \
  void CodeGenerator::Visit(const classname& expr) { \
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

void CodeGenerator::Visit(const SimpleAssignmentExprNode& assign_expr) {
  assign_expr.expr_->Accept(*this);
  int expr_num = num_recorder.NumOfPrevExpr();
  output << "storew " << PrefixSigil(expr_num) << ", "
         << PrefixSigil(id_to_num.at(assign_expr.id_)) << std::endl;
  num_recorder.Record(expr_num);
}
