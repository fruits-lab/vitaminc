#ifndef AST_HPP_
#define AST_HPP_

#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "scope.hpp"
#include "type.hpp"

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

  int CodeGen() const override;

  void Dump(int pad) const override;

  void CheckType(ScopeStack& env) override;

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

  int CodeGen() const override;

  void Dump(int pad) const override;

  void CheckType(ScopeStack& env) override;

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

  int CodeGen() const override;

  void Dump(int pad) const override;

  void CheckType(ScopeStack& env) override;

 protected:
  std::unique_ptr<BlockStmtNode> block_;
};

class NullStmtNode : public StmtNode {
 public:
  int CodeGen() const override;

  void Dump(int pad) const override;

  void CheckType(ScopeStack& env) override;
};

class ReturnStmtNode : public StmtNode {
 public:
  ReturnStmtNode(std::unique_ptr<ExprNode> expr) : expr_{std::move(expr)} {}

  int CodeGen() const override;

  void Dump(int pad) const override;

  void CheckType(ScopeStack& env) override;

 protected:
  std::unique_ptr<ExprNode> expr_;
};

/// @note Any expression can be turned into a statement by adding a semicolon
/// to the end of the expression.
class ExprStmtNode : public StmtNode {
 public:
  ExprStmtNode(std::unique_ptr<ExprNode> expr) : expr_{std::move(expr)} {}

  int CodeGen() const override;

  void Dump(int pad) const override;

  void CheckType(ScopeStack& env) override;

 protected:
  std::unique_ptr<ExprNode> expr_;
};

class IdExprNode : public ExprNode {
 public:
  IdExprNode(const std::string& id) : id_{id} {}

  int CodeGen() const override;

  void Dump(int pad) const override;

  void CheckType(ScopeStack& env) override;

 protected:
  std::string id_;
};

class IntConstExprNode : public ExprNode {
 public:
  IntConstExprNode(int val) : val_{val} {}

  int CodeGen() const override;

  void Dump(int pad) const override;

  void CheckType(ScopeStack& env) override;

 protected:
  int val_;
};

/// @note This is an abstract class.
class BinaryExprNode : public ExprNode {
 public:
  BinaryExprNode(std::unique_ptr<ExprNode> lhs, std::unique_ptr<ExprNode> rhs)
      : lhs_{std::move(lhs)}, rhs_{std::move(rhs)} {}

  int CodeGen() const override;

  void Dump(int pad) const override;

  void CheckType(ScopeStack& env) override;

 protected:
  std::unique_ptr<ExprNode> lhs_;
  std::unique_ptr<ExprNode> rhs_;

  /// @brief The name of the operator used in the QBE IR, e.g., `add`.
  virtual std::string OpName_() const = 0;
  /// @brief The mathematical symbol of the operator, e.g., `+`.
  virtual std::string Op_() const = 0;
};

class PlusExprNode : public BinaryExprNode {
  using BinaryExprNode::BinaryExprNode;

 protected:
  std::string OpName_() const override;

  std::string Op_() const override;
};

class SubExprNode : public BinaryExprNode {
  using BinaryExprNode::BinaryExprNode;

 protected:
  std::string OpName_() const override;

  std::string Op_() const override;
};

class MulExprNode : public BinaryExprNode {
  using BinaryExprNode::BinaryExprNode;

 protected:
  std::string OpName_() const override;

  std::string Op_() const override;
};

class DivExprNode : public BinaryExprNode {
  using BinaryExprNode::BinaryExprNode;

 protected:
  std::string OpName_() const override;

  std::string Op_() const override;
};

class GreaterThanExprNode : public BinaryExprNode {
  using BinaryExprNode::BinaryExprNode;

 protected:
  std::string OpName_() const override;

  std::string Op_() const override;
};

class GreaterThanOrEqualToExprNode : public BinaryExprNode {
  using BinaryExprNode::BinaryExprNode;

 protected:
  std::string OpName_() const override;

  std::string Op_() const override;
};

class LessThanExprNode : public BinaryExprNode {
  using BinaryExprNode::BinaryExprNode;

 protected:
  std::string OpName_() const override;

  std::string Op_() const override;
};

class LessThanOrEqualToExprNode : public BinaryExprNode {
  using BinaryExprNode::BinaryExprNode;

 protected:
  std::string OpName_() const override;

  std::string Op_() const override;
};

class EqualToExprNode : public BinaryExprNode {
  using BinaryExprNode::BinaryExprNode;

 protected:
  std::string OpName_() const override;

  std::string Op_() const override;
};

class NotEqualToExprNode : public BinaryExprNode {
  using BinaryExprNode::BinaryExprNode;

 protected:
  std::string OpName_() const override;

  std::string Op_() const override;
};

#endif  // AST_HPP_
