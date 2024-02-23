#ifndef AST_HPP_
#define AST_HPP_

#include <memory>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "operator.hpp"
#include "type.hpp"
#include "visitor.hpp"

/// @brief The most general base node of the Abstract Syntax Tree.
/// @note This is an abstract class.
struct AstNode {
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
struct StmtNode : public AstNode {
  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  /// @note To make the class abstract.
  ~StmtNode() override = 0;
};

/// @note This is an abstract class.
struct ExprNode : public AstNode {
  ExprType type = ExprType::kUnknown;
  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  /// @note To make the class abstract.
  ~ExprNode() override = 0;
};

struct DeclNode : public AstNode {
  DeclNode(std::string id, ExprType decl_type,
           std::unique_ptr<ExprNode> init = {})
      : id{std::move(id)}, type{decl_type}, init{std::move(init)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  std::string id;
  ExprType type;
  std::unique_ptr<ExprNode> init;
};

/// @brief A loop initialization can be either a declaration or an expression.
struct LoopInitNode : public AstNode {
  LoopInitNode(
      std::variant<std::unique_ptr<DeclNode>, std::unique_ptr<ExprNode>> clause)
      : clause{std::move(clause)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  std::variant<std::unique_ptr<DeclNode>, std::unique_ptr<ExprNode>> clause;
};

/// @brief A block is a set of declarations and statements.
struct BlockStmtNode : public StmtNode {
  BlockStmtNode(std::vector<std::unique_ptr<DeclNode>>&& decls,
                std::vector<std::unique_ptr<StmtNode>>&& stmts)
      : decls{std::move(decls)}, stmts{std::move(stmts)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  std::vector<std::unique_ptr<DeclNode>> decls;
  std::vector<std::unique_ptr<StmtNode>> stmts;
};

/// @brief Root of the entire program.
struct ProgramNode : public AstNode {
  /// @note vector of move-only elements are move-only
  ProgramNode(std::unique_ptr<BlockStmtNode> block) : block{std::move(block)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  std::unique_ptr<BlockStmtNode> block;
};

struct IfStmtNode : public StmtNode {
  IfStmtNode(std::unique_ptr<ExprNode> expr, std::unique_ptr<StmtNode> then,
             std::unique_ptr<StmtNode> or_else = {})
      : predicate{std::move(expr)},
        then{std::move(then)},
        or_else{std::move(or_else)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  std::unique_ptr<ExprNode> predicate;
  std::unique_ptr<StmtNode> then;
  std::unique_ptr<StmtNode> or_else;
};

struct WhileStmtNode : public StmtNode {
  WhileStmtNode(std::unique_ptr<ExprNode> predicate,
                std::unique_ptr<StmtNode> loop_body, bool is_do_while = false)
      : predicate{std::move(predicate)},
        loop_body{std::move(loop_body)},
        is_do_while{is_do_while} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  std::unique_ptr<ExprNode> predicate;
  std::unique_ptr<StmtNode> loop_body;
  bool is_do_while;
};

struct ForStmtNode : public StmtNode {
  ForStmtNode(std::unique_ptr<LoopInitNode> loop_init,
              std::unique_ptr<ExprNode> predicate,
              std::unique_ptr<ExprNode> step,
              std::unique_ptr<StmtNode> loop_body)
      : loop_init{std::move(loop_init)},
        predicate{std::move(predicate)},
        step{std::move(step)},
        loop_body{std::move(loop_body)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  std::unique_ptr<LoopInitNode> loop_init;
  std::unique_ptr<ExprNode> predicate;
  std::unique_ptr<ExprNode> step;
  std::unique_ptr<StmtNode> loop_body;
};

struct ReturnStmtNode : public StmtNode {
  ReturnStmtNode(std::unique_ptr<ExprNode> expr) : expr{std::move(expr)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  std::unique_ptr<ExprNode> expr;
};

struct BreakStmtNode : public StmtNode {
  BreakStmtNode() {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;
};

struct ContinueStmtNode : public StmtNode {
  ContinueStmtNode() {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;
};

/// @note Any expression can be turned into a statement by adding a semicolon
/// to the end of the expression.
struct ExprStmtNode : public StmtNode {
  ExprStmtNode(std::unique_ptr<ExprNode> expr) : expr{std::move(expr)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  std::unique_ptr<ExprNode> expr;
};

/// @note Only appears in for statement's expressions and null statement.
struct NullExprNode : public ExprNode {
  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;
};

struct IdExprNode : public ExprNode {
  IdExprNode(std::string id) : id{std::move(id)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  std::string id;
};

struct IntConstExprNode : public ExprNode {
  IntConstExprNode(int val) : val{val} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  int val;
};

struct UnaryExprNode : public ExprNode {
  UnaryExprNode(UnaryOperator op, std::unique_ptr<ExprNode> operand)
      : op{op}, operand{std::move(operand)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  UnaryOperator op;
  std::unique_ptr<ExprNode> operand;
};

struct BinaryExprNode : public ExprNode {
  BinaryExprNode(BinaryOperator op, std::unique_ptr<ExprNode> lhs,
                 std::unique_ptr<ExprNode> rhs)
      : op{op}, lhs{std::move(lhs)}, rhs{std::move(rhs)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  BinaryOperator op;
  std::unique_ptr<ExprNode> lhs;
  std::unique_ptr<ExprNode> rhs;
};

/// @note This is an abstract class.
struct AssignmentExprNode : public ExprNode {
  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  /// @note To make the class abstract.
  ~AssignmentExprNode() override = 0;
};

struct SimpleAssignmentExprNode : public AssignmentExprNode {
  SimpleAssignmentExprNode(std::string id, std::unique_ptr<ExprNode> expr)
      : id{std::move(id)}, expr{std::move(expr)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  std::string id;
  std::unique_ptr<ExprNode> expr;
};

#endif  // AST_HPP_
