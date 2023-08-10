#include <iostream>
#include <memory>
#include <utility>
#include <vector>

// 80 spaces for padding      01234567890123456789012345678901234567890123456789012345678901234567890123456789
static const char* padding = "                                                                                ";

/// @param n The length of the padding, saturated on the boundary of [0, 80].
static const char* Pad(int n);

/// @brief qbe intermediate file
extern std::ofstream output;

/// @brief The most general base node of the Abstract Syntax Tree.
/// @note This is an abstract class.
class AstNode {
 public:
  /// @param pad The length of the padding.
  virtual void Dump(int pad) const = 0;
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

  void Dump(int pad) const override {
    for (const auto& expr : exprs_) {
      expr->Dump(pad);
    }
  }

 protected:
  std::vector<std::unique_ptr<ExprNode>> exprs_;
};

class IntConstExprNode : public ExprNode {
 public:
  IntConstExprNode(int val) : val_{val} {}

  void Dump(int pad) const override {
    std::cout << Pad(pad) << val_ << std::endl;
  }

 protected:
  int val_;
};

/// @note This is an abstract class.
class BinaryExprNode : public ExprNode {
 public:
  BinaryExprNode(std::unique_ptr<ExprNode> lhs, std::unique_ptr<ExprNode> rhs)
      : lhs_{std::move(lhs)}, rhs_{std::move(rhs)} {}

  void Dump(int pad) const override {
    std::cout << Pad(pad) << '(' << Op_() << std::endl;
    lhs_->Dump(pad + 2);
    rhs_->Dump(pad + 2);
    std::cout << Pad(pad) << ')' << std::endl;
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

static const char* Pad(int n) {
  if (n > 80) {
    n = 80;
  } else if (n < 0) {
    n = 0;
  }
  return padding + (80 - n);
}
