#include <iostream>
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
  ProgramNode(const std::vector<ExprNode*>& exprs) : exprs_{exprs} {}  // lvalue
  ProgramNode(std::vector<ExprNode*>&& exprs)
      : exprs_{std::move(exprs)} {}  // rvalue

  void Dump() const override {
    for (auto* expr : exprs_) {
      expr->Dump();
      std::cout << std::endl;
    }
  }

  ~ProgramNode() {
    for (auto* expr : exprs_) {
      delete expr;
    }
  }

 protected:
  std::vector<ExprNode*> exprs_;
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
  BinaryExprNode(ExprNode* lhs, ExprNode* rhs) : lhs_{lhs}, rhs_{rhs} {}

  void Dump() const override {
    lhs_->Dump();
    std::cout << ' ' << Op_() << ' ';
    rhs_->Dump();
  }

  ~BinaryExprNode() {
    delete lhs_;
    delete rhs_;
  }

 protected:
  ExprNode* lhs_;
  ExprNode* rhs_;

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
