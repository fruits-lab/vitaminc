#ifndef AST_HPP_
#define AST_HPP_

#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "type.hpp"
#include "visitor.hpp"

/// @brief The most general base node of the Abstract Syntax Tree.
/// @note This is an abstract class.
class AstNode {
 public:
  virtual void Accept(NonModifyingVisitor&) const;
  virtual void Accept(ModifyingVisitor&);

  /// @note To make the class abstract.
  virtual ~AstNode() = 0;
  AstNode() = default;

  // Delete copy/move operations to avoid slicing. [1]
  // And "You almost never want to copy or move polymorphic objects. They
  // generally live on the heap, and are accessed via (smart) pointers." [2]
  // [1]
  // https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines#Rc-copy-virtual
  // [2] https://stackoverflow.com/a/54792149

  AstNode(const AstNode&) = delete;
  AstNode& operator=(const AstNode&) = delete;
  AstNode(AstNode&&) = delete;
  AstNode& operator=(AstNode&&) = delete;
};

/// @note This is an abstract class.
class StmtNode : public AstNode {
 public:
  virtual void Accept(NonModifyingVisitor&) const override;
  virtual void Accept(ModifyingVisitor&) override;

  /// @note To make the class abstract.
  virtual ~StmtNode() = 0;
};

/// @note This is an abstract class.
class ExprNode : public AstNode {
 public:
  ExprType type = ExprType::kUnknown;
  virtual void Accept(NonModifyingVisitor&) const override;
  virtual void Accept(ModifyingVisitor&) override;

  /// @note To make the class abstract.
  virtual ~ExprNode() = 0;
};

class DeclNode : public AstNode {
 public:
  DeclNode(const std::string& id, ExprType decl_type,
           std::unique_ptr<ExprNode> init = {})
      : id{id}, type{decl_type}, init{std::move(init)} {}

  virtual void Accept(NonModifyingVisitor&) const override;
  virtual void Accept(ModifyingVisitor&) override;

  std::string id;
  ExprType type;
  std::unique_ptr<ExprNode> init;
};

/// @brief A block is a set of declarations and statements.
class BlockStmtNode : public StmtNode {
 public:
  BlockStmtNode(std::vector<std::unique_ptr<DeclNode>>&& decls,
                std::vector<std::unique_ptr<StmtNode>>&& stmts)
      : decls{std::move(decls)}, stmts{std::move(stmts)} {}

  virtual void Accept(NonModifyingVisitor&) const override;
  virtual void Accept(ModifyingVisitor&) override;

  std::vector<std::unique_ptr<DeclNode>> decls;
  std::vector<std::unique_ptr<StmtNode>> stmts;
};

/// @brief Root of the entire program.
class ProgramNode : public AstNode {
 public:
  /// @note vector of move-only elements are move-only
  ProgramNode(std::unique_ptr<BlockStmtNode> block) : block{std::move(block)} {}

  virtual void Accept(NonModifyingVisitor&) const override;
  virtual void Accept(ModifyingVisitor&) override;

  std::unique_ptr<BlockStmtNode> block;
};

class NullStmtNode : public StmtNode {
 public:
  virtual void Accept(NonModifyingVisitor&) const override;
  virtual void Accept(ModifyingVisitor&) override;
};

class ReturnStmtNode : public StmtNode {
 public:
  ReturnStmtNode(std::unique_ptr<ExprNode> expr) : expr{std::move(expr)} {}

  virtual void Accept(NonModifyingVisitor&) const override;
  virtual void Accept(ModifyingVisitor&) override;

  std::unique_ptr<ExprNode> expr;
};

/// @note Any expression can be turned into a statement by adding a semicolon
/// to the end of the expression.
class ExprStmtNode : public StmtNode {
 public:
  ExprStmtNode(std::unique_ptr<ExprNode> expr) : expr{std::move(expr)} {}

  virtual void Accept(NonModifyingVisitor&) const override;
  virtual void Accept(ModifyingVisitor&) override;

  std::unique_ptr<ExprNode> expr;
};

class IdExprNode : public ExprNode {
 public:
  IdExprNode(const std::string& id) : id{id} {}

  virtual void Accept(NonModifyingVisitor&) const override;
  virtual void Accept(ModifyingVisitor&) override;

  std::string id;
};

class IntConstExprNode : public ExprNode {
 public:
  IntConstExprNode(int val) : val{val} {}

  virtual void Accept(NonModifyingVisitor&) const override;
  virtual void Accept(ModifyingVisitor&) override;

  int val;
};

/// @note This is an abstract class.
class BinaryExprNode : public ExprNode {
 public:
  BinaryExprNode(std::unique_ptr<ExprNode> lhs, std::unique_ptr<ExprNode> rhs)
      : lhs{std::move(lhs)}, rhs{std::move(rhs)} {}

  virtual void Accept(NonModifyingVisitor&) const override;
  virtual void Accept(ModifyingVisitor&) override;

  /// @note To make the class abstract.
  virtual ~BinaryExprNode() = 0;

  std::unique_ptr<ExprNode> lhs;
  std::unique_ptr<ExprNode> rhs;
};

class PlusExprNode : public BinaryExprNode {
  using BinaryExprNode::BinaryExprNode;

 public:
  virtual void Accept(NonModifyingVisitor&) const override;
  virtual void Accept(ModifyingVisitor&) override;
};

class SubExprNode : public BinaryExprNode {
  using BinaryExprNode::BinaryExprNode;

 public:
  virtual void Accept(NonModifyingVisitor&) const override;
  virtual void Accept(ModifyingVisitor&) override;
};

class MulExprNode : public BinaryExprNode {
  using BinaryExprNode::BinaryExprNode;

 public:
  virtual void Accept(NonModifyingVisitor&) const override;
  virtual void Accept(ModifyingVisitor&) override;
};

class DivExprNode : public BinaryExprNode {
  using BinaryExprNode::BinaryExprNode;

 public:
  virtual void Accept(NonModifyingVisitor&) const override;
  virtual void Accept(ModifyingVisitor&) override;
};

class ModExprNode : public BinaryExprNode {
  using BinaryExprNode::BinaryExprNode;

 public:
  virtual void Accept(NonModifyingVisitor&) const override;
  virtual void Accept(ModifyingVisitor&) override;
};

class GreaterThanExprNode : public BinaryExprNode {
  using BinaryExprNode::BinaryExprNode;

 public:
  virtual void Accept(NonModifyingVisitor&) const override;
  virtual void Accept(ModifyingVisitor&) override;
};

class GreaterThanOrEqualToExprNode : public BinaryExprNode {
  using BinaryExprNode::BinaryExprNode;

 public:
  virtual void Accept(NonModifyingVisitor&) const override;
  virtual void Accept(ModifyingVisitor&) override;
};

class LessThanExprNode : public BinaryExprNode {
  using BinaryExprNode::BinaryExprNode;

 public:
  virtual void Accept(NonModifyingVisitor&) const override;
  virtual void Accept(ModifyingVisitor&) override;
};

class LessThanOrEqualToExprNode : public BinaryExprNode {
  using BinaryExprNode::BinaryExprNode;

 public:
  virtual void Accept(NonModifyingVisitor&) const override;
  virtual void Accept(ModifyingVisitor&) override;
};

class EqualToExprNode : public BinaryExprNode {
  using BinaryExprNode::BinaryExprNode;

 public:
  virtual void Accept(NonModifyingVisitor&) const override;
  virtual void Accept(ModifyingVisitor&) override;
};

class NotEqualToExprNode : public BinaryExprNode {
  using BinaryExprNode::BinaryExprNode;

 public:
  virtual void Accept(NonModifyingVisitor&) const override;
  virtual void Accept(ModifyingVisitor&) override;
};

/// @note This is an abstract class.
class AssignmentExprNode : public ExprNode {
 public:
  virtual void Accept(NonModifyingVisitor&) const override;
  virtual void Accept(ModifyingVisitor&) override;

  /// @note To make the class abstract.
  virtual ~AssignmentExprNode() = 0;
};

class SimpleAssignmentExprNode : public AssignmentExprNode {
 public:
  SimpleAssignmentExprNode(std::string id, std::unique_ptr<ExprNode> expr)
      : id{std::move(id)}, expr{std::move(expr)} {}

  virtual void Accept(NonModifyingVisitor&) const override;
  virtual void Accept(ModifyingVisitor&) override;

  std::string id;
  std::unique_ptr<ExprNode> expr;
};

#endif  // AST_HPP_
