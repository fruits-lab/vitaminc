#include "ast.hpp"

#include <fstream>
#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "scope.hpp"
#include "symbol.hpp"
#include "type.hpp"

/// @brief qbe intermediate file
extern std::ofstream output;

namespace {

// clang-format off
// Not to format the padding to emphasize the actual length.

// 80 spaces for padding
//                     01234567890123456789012345678901234567890123456789012345678901234567890123456789
const char* padding = "                                                                                ";

// clang-format on

/// @param n The length of the padding, saturated on the boundary of [0, 80].
const char* Pad(int n);

/// @brief Returns the next local number and increment it by 1. The first number
/// will be 1.
int NextLocalNum() {
  /// @brief temporary index under a scope
  static int next_local_num = 1;
  return next_local_num++;
}

/// @note Use this as the return local number if the it's not expected to be
/// used, e.g., `StmtNode`.
const int kDummyLocalNum = -1;

/// @brief Returns the function-scope temporary with sigil (`%`).
std::string PrefixSigil(int local_num) {
  return "%." + std::to_string(local_num);
}

std::map<std::string, int> id_to_num{};

}  // namespace

int DeclNode::CodeGen() const {
  int id_num = NextLocalNum();
  output << PrefixSigil(id_num) << " =l alloc4 4" << std::endl;

  if (init_) {
    int init_num = init_->CodeGen();
    output << "storew " << PrefixSigil(init_num) << ", " << PrefixSigil(id_num)
           << std::endl;
  }
  // Set up the number of the id so we know were to load it back.
  id_to_num[id_] = id_num;
  return kDummyLocalNum;
}

void DeclNode::Dump(int pad) const {
  std::cout << Pad(pad) << '(' << id_ << ": " << ExprTypeToCString(type_);
  if (init_) {
    std::cout << " =" << std::endl;
    init_->Dump(pad + 2);
  }
  std::cout << ')' << std::endl;
}

void DeclNode::CheckType(ScopeStack& env) {
  if (init_) {
    init_->CheckType(env);
    if (init_->type != type_) {
      // TODO: incompatible types when initializing type 'type_' using type
      // 'init_->type'
    }
  }

  if (env.Probe(id_)) {
    // TODO: redefinition of 'id_'
  } else {
    auto symbol = std::make_unique<SymbolEntry>(id_);
    symbol->expr_type = type_;
    env.Add(std::move(symbol));
  }
}

int BlockStmtNode::CodeGen() const {
  output << "@start" << std::endl;
  for (const auto& decl : decls_) {
    decl->CodeGen();
  }
  for (const auto& stmt : stmts_) {
    stmt->CodeGen();
  }

  return kDummyLocalNum;
}

void BlockStmtNode::Dump(int pad) const {
  for (const auto& decl : decls_) {
    decl->Dump(pad);
  }
  for (const auto& stmt : stmts_) {
    stmt->Dump(pad);
  }
}

void BlockStmtNode::CheckType(ScopeStack& env) {
  env.PushScope();
  for (auto& decl : decls_) {
    decl->CheckType(env);
  }
  for (auto& stmt : stmts_) {
    stmt->CheckType(env);
  }
  env.PopScope();
}

/// @brief Root of the entire program.

int ProgramNode::CodeGen() const {
  output << "export function w $main() {" << std::endl;
  block_->CodeGen();
  output << "}";

  return kDummyLocalNum;
}

void ProgramNode::Dump(int pad) const {
  block_->Dump(pad);
}

void ProgramNode::CheckType(ScopeStack& env) {
  block_->CheckType(env);
}

int NullStmtNode::CodeGen() const {
  return kDummyLocalNum;
}

void NullStmtNode::Dump(int pad) const {
  std::cout << Pad(pad) << "()" << std::endl;
}

void NullStmtNode::CheckType(ScopeStack& env) {}

int ReturnStmtNode::CodeGen() const {
  int ret_num = expr_->CodeGen();
  output << " ret " << PrefixSigil(ret_num) << std::endl;
  return kDummyLocalNum;
}

void ReturnStmtNode::Dump(int pad) const {
  std::cout << Pad(pad) << "(ret" << std::endl;
  expr_->Dump(pad + 2);
  std::cout << Pad(pad) << ')' << std::endl;
}

void ReturnStmtNode::CheckType(ScopeStack& env) {
  expr_->CheckType(env);
  if (expr_->type != ExprType::kInt) {
    // TODO: return value type does not match the function type
  }
}

int ExprStmtNode::CodeGen() const {
  expr_->CodeGen();

  return kDummyLocalNum;
}

void ExprStmtNode::Dump(int pad) const {
  expr_->Dump(pad);
}

void ExprStmtNode::CheckType(ScopeStack& env) {
  expr_->CheckType(env);
}

int IdExprNode::CodeGen() const {
  /// @brief Plays the role of a "pointer". Its value has to be loaded to
  /// the register before use.
  int id_num = id_to_num.at(id_);
  int reg_num = NextLocalNum();
  output << PrefixSigil(reg_num) << " =w loadw " << PrefixSigil(id_num)
         << std::endl;
  return reg_num;
}

void IdExprNode::Dump(int pad) const {
  std::cout << Pad(pad) << id_ << ": " << ExprTypeToCString(type) << std::endl;
}

void IdExprNode::CheckType(ScopeStack& env) {
  if (auto symbol = env.LookUp(id_)) {
    type = symbol->expr_type;
  } else {
    // TODO: 'id_' undeclared
  }
}

int IntConstExprNode::CodeGen() const {
  int num = NextLocalNum();
  output << PrefixSigil(num) << " =w copy " << val_ << std::endl;
  return num;
}

void IntConstExprNode::Dump(int pad) const {
  std::cout << Pad(pad) << val_ << ": " << ExprTypeToCString(type) << std::endl;
}

void IntConstExprNode::CheckType(ScopeStack& env) {
  type = ExprType::kInt;
}

int BinaryExprNode::CodeGen() const {
  int left_num = lhs_->CodeGen();
  int right_num = rhs_->CodeGen();
  int num = NextLocalNum();
  output << PrefixSigil(num) << " =w " << OpName_() << " "
         << PrefixSigil(left_num) << ", " << PrefixSigil(right_num)
         << std::endl;

  return num;
}

void BinaryExprNode::Dump(int pad) const {
  std::cout << Pad(pad) << '(' << Op_() << std::endl;
  lhs_->Dump(pad + 2);
  rhs_->Dump(pad + 2);
  std::cout << Pad(pad) << ')' << ": " << ExprTypeToCString(type) << std::endl;
}

void BinaryExprNode::CheckType(ScopeStack& env) {
  lhs_->CheckType(env);
  rhs_->CheckType(env);
  if (lhs_->type != rhs_->type) {
    // TODO: invalid operands to binary +
  } else {
    type = lhs_->type;
  }
}

std::string PlusExprNode::OpName_() const {
  return "add";
}

std::string PlusExprNode::Op_() const {
  return "+";
}

std::string SubExprNode::OpName_() const {
  return "sub";
}

std::string SubExprNode::Op_() const {
  return "-";
}

std::string MulExprNode::OpName_() const {
  return "mul";
}

std::string MulExprNode::Op_() const {
  return "*";
}

std::string DivExprNode::OpName_() const {
  return "div";
}

std::string DivExprNode::Op_() const {
  return "/";
}

std::string ModExprNode::OpName_() const {}

std::string ModExprNode::Op_() const {}

std::string GreaterThanExprNode::OpName_() const {
  // signed
  return "sgt";
}

std::string GreaterThanExprNode::Op_() const {
  return ">";
}

std::string GreaterThanOrEqualToExprNode::OpName_() const {
  // signed
  return "sge";
}

std::string GreaterThanOrEqualToExprNode::Op_() const {
  return ">=";
}

std::string LessThanExprNode::OpName_() const {
  // signed
  return "slt";
}

std::string LessThanExprNode::Op_() const {
  return "<";
}

std::string LessThanOrEqualToExprNode::OpName_() const {
  // signed
  return "sle";
}

std::string LessThanOrEqualToExprNode::Op_() const {
  return "<=";
}

std::string EqualToExprNode::OpName_() const {
  return "eq";
}

std::string EqualToExprNode::Op_() const {
  return "==";
}

std::string NotEqualToExprNode::OpName_() const {
  return "ne";
}

std::string NotEqualToExprNode::Op_() const {
  return "!=";
}

int SimpleAssignmentExprNode::CodeGen() const {
  int expr_num = expr_->CodeGen();
  output << "storew " << PrefixSigil(expr_num) << ", "
         << PrefixSigil(id_to_num.at(id_)) << std::endl;
  return expr_num;
}

void SimpleAssignmentExprNode::Dump(int pad) const {
  std::cout << Pad(pad) << '(' << '=' << std::endl;
  std::cout << Pad(pad + 2) << id_ << ": " << ExprTypeToCString(type)
            << std::endl;
  expr_->Dump(pad + 2);
  std::cout << Pad(pad) << ')' << ": " << ExprTypeToCString(expr_->type)
            << std::endl;
}

void SimpleAssignmentExprNode::CheckType(ScopeStack& env) {
  expr_->CheckType(env);
  if (auto symbol = env.LookUp(id_)) {
    if (expr_->type == symbol->expr_type) {
      // 6.5.16 Assignment operators
      // The type of an assignment expression is the type of the left
      // operand unless the left operand has qualified type, in which case it is
      // the unqualified version of the type of the left operand.
      type = symbol->expr_type;
    } else {
      // TODO: assigning to 'symbol->expr_type' from incompatible type
      // 'expr_->type'
    }
  } else {
    // TODO: 'id_' undeclared
  }
}

namespace {

const char* Pad(int n) {
  if (n > 80) {
    n = 80;
  } else if (n < 0) {
    n = 0;
  }
  return padding + (80 - n);
}

}  // namespace
