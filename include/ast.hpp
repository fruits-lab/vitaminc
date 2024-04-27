#ifndef AST_HPP_
#define AST_HPP_

#include <cstddef>
#include <memory>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "location.hpp"
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

  AstNode(Location loc) : loc{loc} {}

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

  Location loc;
};

// NOLINTBEGIN(cppcoreguidelines-special-member-functions):
// Since the base class `AstNode` doesn't have a pure virtual destructor,
// subclasses have to mark their destructors as pure virtual again to make the
// class abstract. The destructor is then defined out-of-class using `=
// default`; we do not actually define a custom destructor.

/// @note This is an abstract class.
struct StmtNode : public AstNode {
  using AstNode::AstNode;

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  /// @note To make the class abstract.
  ~StmtNode() override = 0;
};

// NOLINTEND(cppcoreguidelines-special-member-functions)

/// @note This is an abstract class.
struct DeclNode  // NOLINT(cppcoreguidelines-special-member-functions)
    : public AstNode {
  using AstNode::AstNode;

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  /// @note To make the class abstract.
  ~DeclNode() override = 0;
};

/// @note This is an abstract class.
struct ExprNode  // NOLINT(cppcoreguidelines-special-member-functions)
    : public AstNode {
  using AstNode::AstNode;

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  /// @note To make the class abstract.
  ~ExprNode() override = 0;

  std::unique_ptr<Type> type =
      std::make_unique<PrimType>(PrimitiveType::kUnknown);
};

struct DeclVarNode : public DeclNode {
  DeclVarNode(Location loc, std::string id, std::unique_ptr<Type> type,
              std::unique_ptr<ExprNode> init = {})
      : DeclNode{loc},
        id{std::move(id)},
        type{std::move(type)},
        init{std::move(init)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  std::string id;
  std::unique_ptr<Type> type;
  std::unique_ptr<ExprNode> init;
};

struct DeclArrNode : public DeclNode {
  DeclArrNode(Location loc, std::string id, std::unique_ptr<ArrType> type)
      : DeclNode{loc}, id{std::move(id)}, type{std::move(type)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  std::string id;
  std::unique_ptr<ArrType> type;
};

struct ParamNode : public AstNode {
  ParamNode(Location loc, std::string id, std::unique_ptr<Type> type)
      : AstNode{loc}, id{std::move(id)}, type{std::move(type)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  std::string id;
  std::unique_ptr<Type> type;
};

struct FuncDefNode : public AstNode {
  FuncDefNode(Location loc, std::string id,
              std::vector<std::unique_ptr<ParamNode>> parameters,
              std::unique_ptr<CompoundStmtNode> body,
              std::unique_ptr<Type> type)
      : AstNode{loc},
        id{std::move(id)},
        parameters{std::move(parameters)},
        body{std::move(body)},
        type{std::move(type)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  std::string id;
  std::vector<std::unique_ptr<ParamNode>> parameters;
  std::unique_ptr<CompoundStmtNode> body;
  std::unique_ptr<Type> type;
};

/// @brief A loop initialization can be either a declaration or an expression.
struct LoopInitNode : public AstNode {
  LoopInitNode(
      Location loc,
      std::variant<std::unique_ptr<DeclNode>, std::unique_ptr<ExprNode>> clause)
      : AstNode{loc}, clause{std::move(clause)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  std::variant<std::unique_ptr<DeclNode>, std::unique_ptr<ExprNode>> clause;
};

struct CompoundStmtNode : public StmtNode {
  using Item =
      std::variant<std::unique_ptr<DeclNode>, std::unique_ptr<StmtNode>>;
  CompoundStmtNode(Location loc, std::vector<Item> items)
      : StmtNode{loc}, items{std::move(items)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  std::vector<Item> items;
};

/// @brief Root of the entire program.
struct ProgramNode : public AstNode {
  /// @note vector of move-only elements are move-only
  ProgramNode(Location loc,
              std::vector<std::unique_ptr<FuncDefNode>> func_def_list)
      : AstNode{loc}, func_def_list{std::move(func_def_list)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  std::vector<std::unique_ptr<FuncDefNode>> func_def_list;
};

struct IfStmtNode : public StmtNode {
  IfStmtNode(Location loc, std::unique_ptr<ExprNode> expr,
             std::unique_ptr<StmtNode> then,
             std::unique_ptr<StmtNode> or_else = {})
      : StmtNode{loc},
        predicate{std::move(expr)},
        then{std::move(then)},
        or_else{std::move(or_else)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  std::unique_ptr<ExprNode> predicate;
  std::unique_ptr<StmtNode> then;
  std::unique_ptr<StmtNode> or_else;
};

struct WhileStmtNode : public StmtNode {
  WhileStmtNode(Location loc, std::unique_ptr<ExprNode> predicate,
                std::unique_ptr<StmtNode> loop_body, bool is_do_while = false)
      : StmtNode{loc},
        predicate{std::move(predicate)},
        loop_body{std::move(loop_body)},
        is_do_while{is_do_while} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  std::unique_ptr<ExprNode> predicate;
  std::unique_ptr<StmtNode> loop_body;
  bool is_do_while;
};

struct ForStmtNode : public StmtNode {
  ForStmtNode(Location loc, std::unique_ptr<LoopInitNode> loop_init,
              std::unique_ptr<ExprNode> predicate,
              std::unique_ptr<ExprNode> step,
              std::unique_ptr<StmtNode> loop_body)
      : StmtNode{loc},
        loop_init{std::move(loop_init)},
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
  ReturnStmtNode(Location loc, std::unique_ptr<ExprNode> expr)
      : StmtNode{loc}, expr{std::move(expr)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  std::unique_ptr<ExprNode> expr;
};

struct GotoStmtNode : public StmtNode {
  GotoStmtNode(Location loc, std::string label)
      : StmtNode{loc}, label{std::move(label)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  std::string label;
};

struct BreakStmtNode : public StmtNode {
  using StmtNode::StmtNode;

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;
};

struct ContinueStmtNode : public StmtNode {
  using StmtNode::StmtNode;

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;
};

struct SwitchStmtNode : public StmtNode {
  SwitchStmtNode(Location loc, std::unique_ptr<ExprNode> ctrl,
                 std::unique_ptr<StmtNode> stmt)
      : StmtNode{loc}, ctrl{std::move(ctrl)}, stmt{std::move(stmt)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  /// @brief The expression that controls which case to jump to.
  std::unique_ptr<ExprNode> ctrl;
  std::unique_ptr<StmtNode> stmt;
};

/// @brief This is an abstract class.
struct LabeledStmtNode  // NOLINT(cppcoreguidelines-special-member-functions)
    : public StmtNode {
  LabeledStmtNode(Location loc, std::unique_ptr<StmtNode> stmt)
      : StmtNode{loc}, stmt{std::move(stmt)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  /// @note To make the class abstract.
  ~LabeledStmtNode() override = 0;

  std::unique_ptr<StmtNode> stmt;
};

struct IdLabeledStmtNode : public LabeledStmtNode {
  IdLabeledStmtNode(Location loc, std::string label,
                    std::unique_ptr<StmtNode> stmt)
      : LabeledStmtNode{loc, std::move(stmt)}, label{std::move(label)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  std::string label;
};

/// @brief A specialized labeled statement with label `case`.
struct CaseStmtNode : public LabeledStmtNode {
  CaseStmtNode(Location loc, std::unique_ptr<ExprNode> expr,
               std::unique_ptr<StmtNode> stmt)
      : LabeledStmtNode{loc, std::move(stmt)}, expr{std::move(expr)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  std::unique_ptr<ExprNode> expr;
};

/// @brief A specialized labeled statement with label `default`.
struct DefaultStmtNode : public LabeledStmtNode {
  using LabeledStmtNode::LabeledStmtNode;

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;
};

/// @note Any expression can be turned into a statement by adding a semicolon
/// to the end of the expression.
struct ExprStmtNode : public StmtNode {
  ExprStmtNode(Location loc, std::unique_ptr<ExprNode> expr)
      : StmtNode{loc}, expr{std::move(expr)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  std::unique_ptr<ExprNode> expr;
};

/// @note Only appears in for statement's expressions and null statement.
struct NullExprNode : public ExprNode {
  using ExprNode::ExprNode;

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;
};

struct IdExprNode : public ExprNode {
  IdExprNode(Location loc, std::string id) : ExprNode{loc}, id{std::move(id)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  std::string id;
};

struct IntConstExprNode : public ExprNode {
  IntConstExprNode(Location loc, int val) : ExprNode{loc}, val{val} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  int val;
};

struct ArgExprNode : public ExprNode {
  ArgExprNode(Location loc, std::unique_ptr<ExprNode> arg)
      : ExprNode{loc}, arg{std::move(arg)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  std::unique_ptr<ExprNode> arg;
};

// @brief An array subscripting expression.
struct ArrSubExprNode : public ExprNode {
  ArrSubExprNode(Location loc, std::unique_ptr<ExprNode> postfix_expr,
                 std::unique_ptr<ExprNode> index)
      : ExprNode{loc},
        postfix_expr{std::move(postfix_expr)},
        index{std::move(index)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  std::unique_ptr<ExprNode> postfix_expr;
  std::unique_ptr<ExprNode> index;
};

struct FuncCallExprNode : public ExprNode {
  FuncCallExprNode(Location loc, std::unique_ptr<ExprNode> func_expr,
                   std::vector<std::unique_ptr<ArgExprNode>> args)
      : ExprNode{loc}, func_expr{std::move(func_expr)}, args{std::move(args)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  std::unique_ptr<ExprNode> func_expr;
  std::vector<std::unique_ptr<ArgExprNode>> args;
};

struct UnaryExprNode : public ExprNode {
  UnaryExprNode(Location loc, UnaryOperator op,
                std::unique_ptr<ExprNode> operand)
      : ExprNode{loc}, op{op}, operand{std::move(operand)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  UnaryOperator op;
  std::unique_ptr<ExprNode> operand;
};

struct BinaryExprNode : public ExprNode {
  BinaryExprNode(Location loc, BinaryOperator op, std::unique_ptr<ExprNode> lhs,
                 std::unique_ptr<ExprNode> rhs)
      : ExprNode{loc}, op{op}, lhs{std::move(lhs)}, rhs{std::move(rhs)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  BinaryOperator op;
  std::unique_ptr<ExprNode> lhs;
  std::unique_ptr<ExprNode> rhs;
};

/// @note This is an abstract class.
struct AssignmentExprNode  // NOLINT(cppcoreguidelines-special-member-functions)
    : public ExprNode {
  using ExprNode::ExprNode;

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  /// @note To make the class abstract.
  ~AssignmentExprNode() override = 0;
};

struct SimpleAssignmentExprNode : public AssignmentExprNode {
  SimpleAssignmentExprNode(Location loc, std::unique_ptr<ExprNode> lhs,
                           std::unique_ptr<ExprNode> rhs)
      : AssignmentExprNode{loc}, lhs{std::move(lhs)}, rhs{std::move(rhs)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  std::unique_ptr<ExprNode> lhs;
  std::unique_ptr<ExprNode> rhs;
};

#endif  // AST_HPP_
