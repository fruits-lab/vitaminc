#include <fstream>
#include <iostream>
#include <memory>
#include <utility>
#include <vector>

// clang-format off
// Not to format the padding to emphasize the actual length.

// 80 spaces for padding      01234567890123456789012345678901234567890123456789012345678901234567890123456789
static const char* padding = "                                                                                ";

// clang-format on

/// @param n The length of the padding, saturated on the boundary of [0, 80].
static const char* Pad(int n);

/// @brief qbe intermediate file
extern std::ofstream output;

/// @brief The most general base node of the Abstract Syntax Tree.
/// @note This is an abstract class.
class AstNode {
 public:
  virtual void CodeGen() const = 0;
  /// @param pad The length of the padding.
  virtual void Dump(int pad) const = 0;
  virtual ~AstNode() = default;
};

/// @note This is an abstract class.
class DeclNode : public AstNode {};
/// @note This is an abstract class.
class StmtNode : public AstNode {};
/// @note This is an abstract class.
class ExprNode : public AstNode {};

/// @brief Root of the entire program.
class ProgramNode : public AstNode {
 public:
  /// @note vector of move-only elements are move-only
  ProgramNode(std::unique_ptr<StmtNode> block) : block_{std::move(block)} {}

  void CodeGen() const override {
    /* qbe main */
    output << "export function w $main() {" << std::endl;
    output << "@start" << std::endl;
    block_->CodeGen();
    output << "}";
  }

  void Dump(int pad) const override {
    block_->Dump(pad);
  }

 protected:
  std::unique_ptr<StmtNode> block_;
};

/// @brief A block is a set of declarations and statements.
class BlockStmtNode : public StmtNode {
 public:
  BlockStmtNode(std::vector<std::unique_ptr<DeclNode>>&& decls,
            std::vector<std::unique_ptr<StmtNode>>&& stmts)
      : decls_{std::move(decls)}, stmts_{std::move(stmts)} {}

  void CodeGen() const override {
    for (const auto& decl : decls_) {
      decl->CodeGen();
    }
    for (const auto& stmt : stmts_) {
      stmt->CodeGen();
    }
  }

  void Dump(int pad) const override {
    for (const auto& decl : decls_) {
      decl->Dump(pad);
    }
    for (const auto& stmt : stmts_) {
      stmt->Dump(pad);
    }
  }

 protected:
  std::vector<std::unique_ptr<DeclNode>> decls_;
  std::vector<std::unique_ptr<StmtNode>> stmts_;
};

class DeclNoInitNode : public DeclNode {
 public:
  DeclNoInitNode(const std::string& id) : id_{id} {}

  void CodeGen() const override {}
  void Dump(int pad) const override {
    std::cout << Pad(pad) << '(' << id_ << ')' << std::endl;
  }

 protected:
  std::string id_;
};

class DeclWithInitNode : public DeclNode {
 public:
  DeclWithInitNode(const std::string& id, std::unique_ptr<ExprNode> expr)
      : id_{id}, expr_{std::move(expr)} {}

  void CodeGen() const override {}
  void Dump(int pad) const override {
    std::cout << Pad(pad) << '(' << id_ << " =" << std::endl;
    expr_->Dump(pad + 2);
    std::cout << Pad(pad) << ')' << std::endl;
  }

 protected:
  std::string id_;
  std::unique_ptr<ExprNode> expr_;
};

class NullStmtNode : public StmtNode {
 public:
  NullStmtNode() = default;

  void CodeGen() const override {}
  void Dump(int pad) const override {
    std::cout << Pad(pad) << "()" << std::endl;
  }
};

class ReturnStmtNode : public StmtNode {
 public:
  ReturnStmtNode(std::unique_ptr<ExprNode> expr) : expr_{std::move(expr)} {}

  void CodeGen() const override {
    output << " ret ";
    expr_->CodeGen();
  }

  void Dump(int pad) const override {
    std::cout << Pad(pad) << "(ret" << std::endl;
    expr_->Dump(pad + 2);
    std::cout << Pad(pad) << ')' << std::endl;
  }

 protected:
  std::unique_ptr<ExprNode> expr_;
};

/// @note Any expression can be turned into a statement by adding a semicolon
/// to the end of the expression.
class ExprStmtNode : public StmtNode {
 public:
  ExprStmtNode(std::unique_ptr<ExprNode> expr) : expr_{std::move(expr)} {}

  void CodeGen() const override {}
  void Dump(int pad) const override {
    expr_->Dump(pad);
  }

 protected:
  std::unique_ptr<ExprNode> expr_;
};

class IdExprNode : public ExprNode {
 public:
  IdExprNode(const std::string& id) : id_{id} {}
  void CodeGen() const override {}
  void Dump(int pad) const override {
    std::cout << Pad(pad) << id_ << std::endl;
  }

 protected:
  std::string id_;
};

class IntConstExprNode : public ExprNode {
 public:
  IntConstExprNode(int val) : val_{val} {}

  void CodeGen() const override {
    output << val_ << std::endl;
  }

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

  void CodeGen() const override {}
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
