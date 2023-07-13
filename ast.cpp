#include <iostream>
#include <memory>
#include <utility>
#include <vector>

/// @brief The most general base node of the Abstract Syntax Tree.
/// @note This is an abstract class.
class AstNode {
 public:
  virtual void Dump() const = 0;
  virtual ~AstNode() = default;
};

/// @note This is an abstract class.
class ExprNode : public AstNode {};

/// @brief Root of the entire program.
class ProgramNode : public AstNode {
 public:
  /// @note vector of move-only elements are move-only
  ProgramNode(std::vector<std::unique_ptr<ExprNode>>&& exprs)
      : exprs_{std::move(exprs)} {}

  void Dump() const override {
    for (const auto& expr : exprs_) {
      expr->Dump();
      std::cout << std::endl;
    }
  }

 protected:
  std::vector<std::unique_ptr<ExprNode>> exprs_;
};

class IntConstExprNode : public ExprNode {
 public:
  IntConstExprNode(int val) : val_{val} {}

  void Dump() const override {
    std::cout << val_;
  }

 protected:
  int val_;
};

/// @note This is an abstract class.
class BinaryExprNode : public ExprNode {
 public:
  BinaryExprNode(std::unique_ptr<ExprNode> lhs, std::unique_ptr<ExprNode> rhs)
      : lhs_{std::move(lhs)}, rhs_{std::move(rhs)} {}

  void Dump() const override {
    lhs_->Dump();
    std::cout << ' ' << Op_() << ' ';
    rhs_->Dump();
  }

 protected:
  std::unique_ptr<ExprNode> lhs_;
  std::unique_ptr<ExprNode> rhs_;

  virtual char Op_() const = 0;
};

class PlusExprNode : public BinaryExprNode {
  using BinaryExprNode::BinaryExprNode;

 protected:
  char Op_() const override {
    return '+';
  }
};

class SubExprNode : public BinaryExprNode {
  using BinaryExprNode::BinaryExprNode;

 protected:
  char Op_() const override {
    return '-';
  }
};

class MulExprNode : public BinaryExprNode {
  using BinaryExprNode::BinaryExprNode;

 protected:
  char Op_() const override {
    return '*';
  }
};

class DivExprNode : public BinaryExprNode {
  using BinaryExprNode::BinaryExprNode;

 protected:
  char Op_() const override {
    return '/';
  }
};
