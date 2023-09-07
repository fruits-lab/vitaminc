#ifndef AST_HPP_
#define AST_HPP_

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

// clang-format off
// Not to format the padding to emphasize the actual length.

// 80 spaces for padding      01234567890123456789012345678901234567890123456789012345678901234567890123456789
static const char* padding = "                                                                                ";

// clang-format on

/// @param n The length of the padding, saturated on the boundary of [0, 80].
static const char* Pad(int n);

/// @brief qbe intermediate file
extern std::ofstream output;

/// @brief Returns the next local number and increment it by 1. The first number
/// will be 1.
static int NextLocalNum() {
  /// @brief temporary index under a scope
  static int next_local_num = 1;
  return next_local_num++;
}

/// @brief Returns the for function-scope temporary with sigil (`%`).
static std::string PrefixSigil(int local_num) {
  return "%." + std::to_string(local_num);
}
/// @brief map
static std::map<std::string, int> id_to_num{};

/// @brief The most general base node of the Abstract Syntax Tree.
/// @note This is an abstract class.
class AstNode {
 public:
  virtual int CodeGen() const = 0;
  /// @param pad The length of the padding.
  virtual void Dump(int pad) const = 0;
  /// @brief A modifying pass; resolves the type of expressions.
  virtual void CheckType(ScopeStack&) = 0;
  virtual ~AstNode() = default;

 protected:
  /// @note Return this if the local number is not expected to be used, e.g.
  /// StmtNode.
  static const int kDummyLocalNum_ = -1;
};

/// @note This is an abstract class.
class StmtNode : public AstNode {};

/// @note This is an abstract class.
class ExprNode : public AstNode {
 public:
  ExprType type = ExprType::kUnknown;
};

class DeclNode : public AstNode {
 public:
  DeclNode(const std::string& id, ExprType decl_type,
           std::unique_ptr<ExprNode> init = {})
      : id_{id}, type_{decl_type}, init_{std::move(init)} {}

  int CodeGen() const override {
    int id_num = NextLocalNum();
    output << PrefixSigil(id_num) << " =l alloc4 4" << std::endl;

    if (init_) {
      int init_num = init_->CodeGen();
      output << "storew " << PrefixSigil(init_num) << ", "
             << PrefixSigil(id_num) << std::endl;
    }

    id_to_num[id_] = id_num;
    return kDummyLocalNum_;
  }

  void Dump(int pad) const override {
    std::cout << Pad(pad) << '(' << id_ << ": " << ExprTypeToCString(type_);
    if (init_) {
      std::cout << " =" << std::endl;
      init_->Dump(pad + 2);
    }
    std::cout << ')' << std::endl;
  }

  void CheckType(ScopeStack& env) override {
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

 protected:
  std::string id_;
  ExprType type_;
  std::unique_ptr<ExprNode> init_;
};

/// @brief A block is a set of declarations and statements.
class BlockStmtNode : public StmtNode {
 public:
  BlockStmtNode(std::vector<std::unique_ptr<DeclNode>>&& decls,
                std::vector<std::unique_ptr<StmtNode>>&& stmts)
      : decls_{std::move(decls)}, stmts_{std::move(stmts)} {}

  int CodeGen() const override {
    output << "@start" << std::endl;
    for (const auto& decl : decls_) {
      decl->CodeGen();
    }
    for (const auto& stmt : stmts_) {
      stmt->CodeGen();
    }

    return kDummyLocalNum_;
  }

  void Dump(int pad) const override {
    for (const auto& decl : decls_) {
      decl->Dump(pad);
    }
    for (const auto& stmt : stmts_) {
      stmt->Dump(pad);
    }
  }

  void CheckType(ScopeStack& env) override {
    env.PushScope();
    for (auto& decl : decls_) {
      decl->CheckType(env);
    }
    for (auto& stmt : stmts_) {
      stmt->CheckType(env);
    }
    env.PopScope();
  }

 protected:
  std::vector<std::unique_ptr<DeclNode>> decls_;
  std::vector<std::unique_ptr<StmtNode>> stmts_;
};

/// @brief Root of the entire program.
class ProgramNode : public AstNode {
 public:
  /// @note vector of move-only elements are move-only
  ProgramNode(std::unique_ptr<BlockStmtNode> block)
      : block_{std::move(block)} {}

  int CodeGen() const override {
    /* qbe main */
    output << "export function w $main() {" << std::endl;
    block_->CodeGen();
    output << "}";

    return kDummyLocalNum_;
  }

  void Dump(int pad) const override {
    block_->Dump(pad);
  }

  void CheckType(ScopeStack& env) override {
    block_->CheckType(env);
  }

 protected:
  std::unique_ptr<BlockStmtNode> block_;
};

class NullStmtNode : public StmtNode {
 public:
  NullStmtNode() = default;

  int CodeGen() const override {
    return kDummyLocalNum_;
  }

  void Dump(int pad) const override {
    std::cout << Pad(pad) << "()" << std::endl;
  }

  void CheckType(ScopeStack& env) override {}
};

class ReturnStmtNode : public StmtNode {
 public:
  ReturnStmtNode(std::unique_ptr<ExprNode> expr) : expr_{std::move(expr)} {}

  int CodeGen() const override {
    int ret_num = expr_->CodeGen();
    output << " ret " << PrefixSigil(ret_num) << std::endl;
    return kDummyLocalNum_;
  }

  void Dump(int pad) const override {
    std::cout << Pad(pad) << "(ret" << std::endl;
    expr_->Dump(pad + 2);
    std::cout << Pad(pad) << ')' << std::endl;
  }

  void CheckType(ScopeStack& env) override {
    expr_->CheckType(env);
    if (expr_->type != ExprType::kInt) {
      // TODO: return value type does not match the function type
    }
  }

 protected:
  std::unique_ptr<ExprNode> expr_;
};

/// @note Any expression can be turned into a statement by adding a semicolon
/// to the end of the expression.
class ExprStmtNode : public StmtNode {
 public:
  ExprStmtNode(std::unique_ptr<ExprNode> expr) : expr_{std::move(expr)} {}

  int CodeGen() const override {
    expr_->CodeGen();

    return kDummyLocalNum_;
  }

  void Dump(int pad) const override {
    expr_->Dump(pad);
  }

  void CheckType(ScopeStack& env) override {
    expr_->CheckType(env);
  }

 protected:
  std::unique_ptr<ExprNode> expr_;
};

class IdExprNode : public ExprNode {
 public:
  IdExprNode(const std::string& id) : id_{id} {}

  int CodeGen() const override {
    int id_num = id_to_num.at(id_);
    int reg_num = NextLocalNum();
    output << PrefixSigil(reg_num) << " =w loadw " << PrefixSigil(id_num)
           << std::endl;
    return reg_num;
  }

  void Dump(int pad) const override {
    std::cout << Pad(pad) << id_ << ": " << ExprTypeToCString(type)
              << std::endl;
  }

  void CheckType(ScopeStack& env) override {
    if (auto symbol = env.LookUp(id_)) {
      type = symbol->expr_type;
    } else {
      // TODO: 'id_' undeclared
    }
  }

 protected:
  std::string id_;
};

class IntConstExprNode : public ExprNode {
 public:
  IntConstExprNode(int val) : val_{val} {}

  int CodeGen() const override {
    int num = NextLocalNum();
    output << PrefixSigil(num) << " =w copy " << val_ << std::endl;
    return num;
  }

  void Dump(int pad) const override {
    std::cout << Pad(pad) << val_ << ": " << ExprTypeToCString(type)
              << std::endl;
  }

  void CheckType(ScopeStack& env) override {
    type = ExprType::kInt;
  }

 protected:
  int val_;
};

/// @note This is an abstract class.
class BinaryExprNode : public ExprNode {
 public:
  BinaryExprNode(std::unique_ptr<ExprNode> lhs, std::unique_ptr<ExprNode> rhs)
      : lhs_{std::move(lhs)}, rhs_{std::move(rhs)} {}

  int CodeGen() const override {
    int left_num = lhs_->CodeGen();
    int right_num = rhs_->CodeGen();
    int num = NextLocalNum();
    output << PrefixSigil(num) << " =w " << OpName_() << " "
           << PrefixSigil(left_num) << ", " << PrefixSigil(right_num)
           << std::endl;

    return num;
  }

  void Dump(int pad) const override {
    std::cout << Pad(pad) << '(' << Op_() << std::endl;
    lhs_->Dump(pad + 2);
    rhs_->Dump(pad + 2);
    std::cout << Pad(pad) << ')' << ": " << ExprTypeToCString(type)
              << std::endl;
  }

  void CheckType(ScopeStack& env) override {
    lhs_->CheckType(env);
    rhs_->CheckType(env);
    if (lhs_->type != rhs_->type) {
      // TODO: invalid operands to binary +
    } else {
      type = lhs_->type;
    }
  }

 protected:
  std::unique_ptr<ExprNode> lhs_;
  std::unique_ptr<ExprNode> rhs_;

  virtual std::string OpName_() const = 0;
  virtual char Op_() const = 0;
};

class PlusExprNode : public BinaryExprNode {
  using BinaryExprNode::BinaryExprNode;

 protected:
  std::string OpName_() const override {
    return "add";
  }

  char Op_() const override {
    return '+';
  }
};

class SubExprNode : public BinaryExprNode {
  using BinaryExprNode::BinaryExprNode;

 protected:
  std::string OpName_() const override {
    return "sub";
  }

  char Op_() const override {
    return '-';
  }
};

class MulExprNode : public BinaryExprNode {
  using BinaryExprNode::BinaryExprNode;

 protected:
  std::string OpName_() const override {
    return "mul";
  }

  char Op_() const override {
    return '*';
  }
};

class DivExprNode : public BinaryExprNode {
  using BinaryExprNode::BinaryExprNode;

 protected:
  std::string OpName_() const override {
    return "div";
  }

  char Op_() const override {
    return '/';
  }
};

static const char* Pad(int n) {
  if (n > 80) {
    n = 80;
  } else if (n < 0) {
    n = 0;
  }
  return padding + (80 - n);
}

#endif  // AST_HPP_
