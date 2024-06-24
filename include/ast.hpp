#ifndef AST_HPP_
#define AST_HPP_

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
  bool is_global{false};
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
  DeclNode(Location loc, std::string id, std::unique_ptr<Type> type)
      : AstNode{loc}, id{std::move(id)}, type{std::move(type)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  /// @note To make the class abstract.
  ~DeclNode() override = 0;

  std::string id;
  std::unique_ptr<Type> type;
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

/// @brief A designator node is used to explicitly reference a member for
/// initializer.
/// @note This is an abstract class.
struct DesNode  // NOLINT(cppcoreguidelines-special-member-functions)
    : public AstNode {
  using AstNode::AstNode;

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  /// @note To make the class abstract.
  ~DesNode() override = 0;

  std::unique_ptr<Type> type =
      std::make_unique<PrimType>(PrimitiveType::kUnknown);
};

/// @brief A declaration statement may declare multiple identifiers.
struct DeclStmtNode : public StmtNode {
  DeclStmtNode(Location loc, std::vector<std::unique_ptr<DeclNode>> decls)
      : StmtNode{loc}, decls{std::move(decls)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  std::vector<std::unique_ptr<DeclNode>> decls;
};

struct VarDeclNode : public DeclNode {
  VarDeclNode(Location loc, std::string id, std::unique_ptr<Type> type,
              std::unique_ptr<ExprNode> init = {})
      : DeclNode{loc, std::move(id), std::move(type)}, init{std::move(init)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  std::unique_ptr<ExprNode> init;
};

struct ArrDeclNode : public DeclNode {
  ArrDeclNode(Location loc, std::string id, std::unique_ptr<Type> type,
              std::vector<std::unique_ptr<InitExprNode>> init_list)
      : DeclNode{loc, std::move(id), std::move(type)},
        init_list{std::move(init_list)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  std::vector<std::unique_ptr<InitExprNode>> init_list;
};

/// @brief This holds the declaration of struct or union type.
struct RecordDeclNode : public DeclNode {
  RecordDeclNode(Location loc, std::string id, std::unique_ptr<Type> type,
                 std::vector<std::unique_ptr<FieldNode>> fields)
      : DeclNode{loc, std::move(id), std::move(type)},
        fields{std::move(fields)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  std::vector<std::unique_ptr<FieldNode>> fields;
};

/// @brief A field in a struct or an union.
struct FieldNode : public DeclNode {
  using DeclNode::DeclNode;

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;
};

/// @brief This holds the declaration of struct or union variable.
struct RecordVarDeclNode : public DeclNode {
  RecordVarDeclNode(Location loc, std::string id, std::unique_ptr<Type> type,
                    std::vector<std::unique_ptr<InitExprNode>> inits)
      : DeclNode{loc, std::move(id), std::move(type)},
        inits{std::move(inits)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  std::vector<std::unique_ptr<InitExprNode>> inits;
};

struct ParamNode : public DeclNode {
  using DeclNode::DeclNode;

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;
};

struct FuncDefNode : public DeclNode {
  FuncDefNode(Location loc, std::string id,
              std::vector<std::unique_ptr<ParamNode>> parameters,
              std::unique_ptr<CompoundStmtNode> body,
              std::unique_ptr<Type> type)
      : DeclNode{loc, std::move(id), std::move(type)},
        parameters{std::move(parameters)},
        body{std::move(body)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  std::vector<std::unique_ptr<ParamNode>> parameters;
  std::unique_ptr<CompoundStmtNode> body;
};

/// @brief A loop initialization can be either a declaration or an expression.
struct LoopInitNode : public AstNode {
  LoopInitNode(
      Location loc,
      std::variant<std::unique_ptr<DeclStmtNode>, std::unique_ptr<ExprNode>>
          clause)
      : AstNode{loc}, clause{std::move(clause)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  std::variant<std::unique_ptr<DeclStmtNode>, std::unique_ptr<ExprNode>> clause;
};

struct CompoundStmtNode : public StmtNode {
  CompoundStmtNode(Location loc, std::vector<std::unique_ptr<StmtNode>> stmts)
      : StmtNode{loc}, stmts{std::move(stmts)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  std::vector<std::unique_ptr<StmtNode>> stmts;
};

/// @brief An external declaration can be a definition of a function or an
/// object.
struct ExternDeclNode : public AstNode {
  ExternDeclNode(
      Location loc,
      std::variant<std::unique_ptr<FuncDefNode>, std::unique_ptr<DeclStmtNode>>
          decl)
      : AstNode{loc}, decl{std::move(decl)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  std::variant<std::unique_ptr<FuncDefNode>, std::unique_ptr<DeclStmtNode>>
      decl;
};

/// @brief A translation unit, which the compiler handles individually,
/// representing a high-level entity in the compilation process.
struct TransUnitNode : public AstNode {
  /// @note vector of move-only elements are move-only
  TransUnitNode(Location loc,
                std::vector<std::unique_ptr<ExternDeclNode>> extern_decls)
      : AstNode{loc}, extern_decls{std::move(extern_decls)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  std::vector<std::unique_ptr<ExternDeclNode>> extern_decls;
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

/// @brief An initializer initializes a member in array, struct or union.
struct InitExprNode : public ExprNode {
  InitExprNode(Location loc, std::vector<std::unique_ptr<DesNode>> des,
               std::unique_ptr<ExprNode> expr)
      : ExprNode{loc}, des{std::move(des)}, expr{std::move(expr)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  std::vector<std::unique_ptr<DesNode>> des;
  std::unique_ptr<ExprNode> expr;
};

/// @brief An array designator node can designate a member by using
/// array subscripting.
struct ArrDesNode : public DesNode {
  ArrDesNode(Location loc, std::unique_ptr<ExprNode> index)
      : DesNode{loc}, index{std::move(index)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  std::unique_ptr<ExprNode> index;
};

/// @brief An identifier designator node can designate a member by using
/// parameter "id".
struct IdDesNode : public DesNode {
  IdDesNode(Location loc, std::string id) : DesNode{loc}, id{std::move(id)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  std::string id;
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

/// @brief An array subscripting expression.
struct ArrSubExprNode : public ExprNode {
  /// @param arr An expression that evaluates to the target array.
  /// @param index An expression that evaluates to the subscription index.
  ArrSubExprNode(Location loc, std::unique_ptr<ExprNode> arr,
                 std::unique_ptr<ExprNode> index)
      : ExprNode{loc}, arr{std::move(arr)}, index{std::move(index)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  std::unique_ptr<ExprNode> arr;
  std::unique_ptr<ExprNode> index;
};

struct CondExprNode : public ExprNode {
  CondExprNode(Location loc, std::unique_ptr<ExprNode> predicate,
               std::unique_ptr<ExprNode> then,
               std::unique_ptr<ExprNode> or_else)
      : ExprNode{loc},
        predicate{std::move(predicate)},
        then{std::move(then)},
        or_else{std::move(or_else)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  std::unique_ptr<ExprNode> predicate;
  std::unique_ptr<ExprNode> then;
  std::unique_ptr<ExprNode> or_else;
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

/// @brief A postfix arithmetic expression.
struct PostfixArithExprNode : public ExprNode {
  PostfixArithExprNode(Location loc, PostfixOperator op,
                       std::unique_ptr<ExprNode> operand)
      : ExprNode{loc}, op{op}, operand{std::move(operand)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  PostfixOperator op;
  std::unique_ptr<ExprNode> operand;
};

/// @brief A postfix expression that designates a member of struct or union.
struct RecordMemExprNode : public ExprNode {
  RecordMemExprNode(Location loc, PostfixOperator op,
                    std::unique_ptr<ExprNode> expr, std::string id)
      : ExprNode{loc}, op{op}, expr{std::move(expr)}, id{std::move(id)} {}

  void Accept(NonModifyingVisitor&) const override;
  void Accept(ModifyingVisitor&) override;

  PostfixOperator op;
  std::unique_ptr<ExprNode> expr;
  std::string id;
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
