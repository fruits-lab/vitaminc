#ifndef VISITOR_HPP_
#define VISITOR_HPP_

#include <type_traits>

// Forward declarations to fix the acyclic problem with name only dependency.
// NOTE: update the list every time a new kind of node is introduced.

struct AstNode;
struct StmtNode;
struct ExprNode;
struct DeclNode;
struct DeclVarNode;
struct DeclArrNode;
struct ParamNode;
struct FuncDefNode;
struct LoopInitNode;
struct CompoundStmtNode;
struct ProgramNode;
struct IfStmtNode;
struct WhileStmtNode;
struct ForStmtNode;
struct ReturnStmtNode;
struct GotoStmtNode;
struct BreakStmtNode;
struct ContinueStmtNode;
struct SwitchStmtNode;
struct LabeledStmtNode;
struct IdLabeledStmtNode;
struct CaseStmtNode;
struct DefaultStmtNode;
struct ExprStmtNode;
struct NullExprNode;
struct IdExprNode;
struct IntConstExprNode;
struct ArgExprNode;
struct ArrSubExprNode;
struct FuncCallExprNode;
struct PostfixArithExprNode;
struct UnaryExprNode;
struct BinaryExprNode;
struct AssignmentExprNode;
struct SimpleAssignmentExprNode;

/// @tparam is_modifying If `true`, `Visit()` takes a non-const reference of the
/// visitable; otherwise, a const reference. Default to `false`.
/// @note This is an abstract class.
/// @note For concrete Visitors, define `Visit()` on the classes that you care
/// about, others will do nothing by default.
template <bool is_modifying = false>
class Visitor {
 public:
  /// @brief Conditionally mutable.
  template <typename Visitable>
  using CondMut = std::conditional_t<is_modifying, Visitable, const Visitable>;

  // Notice that the `Visit()` function is also defined for abstract classes.
  // This is to provide the fallback operation for all concrete classes derived
  // from such abstract class.

  virtual void Visit(CondMut<AstNode>&){};
  virtual void Visit(CondMut<StmtNode>&){};
  virtual void Visit(CondMut<ExprNode>&){};
  virtual void Visit(CondMut<DeclNode>&){};
  virtual void Visit(CondMut<DeclVarNode>&){};
  virtual void Visit(CondMut<DeclArrNode>&){};
  virtual void Visit(CondMut<ParamNode>&){};
  virtual void Visit(CondMut<FuncDefNode>&){};
  virtual void Visit(CondMut<LoopInitNode>&){};
  virtual void Visit(CondMut<CompoundStmtNode>&){};
  virtual void Visit(CondMut<ProgramNode>&){};
  virtual void Visit(CondMut<IfStmtNode>&){};
  virtual void Visit(CondMut<WhileStmtNode>&){};
  virtual void Visit(CondMut<ForStmtNode>&){};
  virtual void Visit(CondMut<ReturnStmtNode>&){};
  virtual void Visit(CondMut<GotoStmtNode>&){};
  virtual void Visit(CondMut<BreakStmtNode>&){};
  virtual void Visit(CondMut<ContinueStmtNode>&){};
  virtual void Visit(CondMut<SwitchStmtNode>&){};
  virtual void Visit(CondMut<LabeledStmtNode>&){};
  virtual void Visit(CondMut<IdLabeledStmtNode>&){};
  virtual void Visit(CondMut<CaseStmtNode>&){};
  virtual void Visit(CondMut<DefaultStmtNode>&){};
  virtual void Visit(CondMut<ExprStmtNode>&){};
  virtual void Visit(CondMut<NullExprNode>&){};
  virtual void Visit(CondMut<IdExprNode>&){};
  virtual void Visit(CondMut<IntConstExprNode>&){};
  virtual void Visit(CondMut<ArgExprNode>&){};
  virtual void Visit(CondMut<ArrSubExprNode>&){};
  virtual void Visit(CondMut<FuncCallExprNode>&){};
  virtual void Visit(CondMut<PostfixArithExprNode>&){};
  virtual void Visit(CondMut<UnaryExprNode>&){};
  virtual void Visit(CondMut<BinaryExprNode>&){};
  virtual void Visit(CondMut<AssignmentExprNode>&){};
  virtual void Visit(CondMut<SimpleAssignmentExprNode>&){};

  /// @note To make the class abstract. But still we have to provide an
  /// out-of-class definition for the destructor.
  virtual ~Visitor() = 0;
  Visitor() = default;

  // Delete copy/move operations to avoid slicing.

  Visitor(const Visitor&) = delete;
  Visitor& operator=(const Visitor&) = delete;
  Visitor(Visitor&&) = delete;
  Visitor& operator=(Visitor&&) = delete;
};

template <bool is_modifying>
Visitor<is_modifying>::~Visitor() = default;

// One can use these type aliases in cases where the meaning of the template
// parameter is not obvious.

using ModifyingVisitor = Visitor<true>;
using NonModifyingVisitor = Visitor<false>;

#endif  // VISITOR_HPP_
