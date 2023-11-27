#ifndef VISITOR_HPP_
#define VISITOR_HPP_

#include <type_traits>

// Forward declarations to fix the acyclic problem with name only dependency.
// NOTE: update the list every time a new kind of node is introduced.

struct AstNode;
struct StmtNode;
struct ExprNode;
struct DeclNode;
struct BlockStmtNode;
struct ProgramNode;
struct NullStmtNode;
struct IfStmtNode;
struct ReturnStmtNode;
struct ExprStmtNode;
struct IdExprNode;
struct IntConstExprNode;
struct BinaryExprNode;
struct PlusExprNode;
struct SubExprNode;
struct MulExprNode;
struct DivExprNode;
struct ModExprNode;
struct GreaterThanExprNode;
struct GreaterThanOrEqualToExprNode;
struct LessThanExprNode;
struct LessThanOrEqualToExprNode;
struct EqualToExprNode;
struct NotEqualToExprNode;
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
  virtual void Visit(CondMut<BlockStmtNode>&){};
  virtual void Visit(CondMut<ProgramNode>&){};
  virtual void Visit(CondMut<NullStmtNode>&){};
  virtual void Visit(CondMut<IfStmtNode>&){};
  virtual void Visit(CondMut<ReturnStmtNode>&){};
  virtual void Visit(CondMut<ExprStmtNode>&){};
  virtual void Visit(CondMut<IdExprNode>&){};
  virtual void Visit(CondMut<IntConstExprNode>&){};
  virtual void Visit(CondMut<BinaryExprNode>&){};
  virtual void Visit(CondMut<PlusExprNode>&){};
  virtual void Visit(CondMut<SubExprNode>&){};
  virtual void Visit(CondMut<MulExprNode>&){};
  virtual void Visit(CondMut<DivExprNode>&){};
  virtual void Visit(CondMut<ModExprNode>&){};
  virtual void Visit(CondMut<GreaterThanExprNode>&){};
  virtual void Visit(CondMut<GreaterThanOrEqualToExprNode>&){};
  virtual void Visit(CondMut<LessThanExprNode>&){};
  virtual void Visit(CondMut<LessThanOrEqualToExprNode>&){};
  virtual void Visit(CondMut<EqualToExprNode>&){};
  virtual void Visit(CondMut<NotEqualToExprNode>&){};
  virtual void Visit(CondMut<AssignmentExprNode>&){};
  virtual void Visit(CondMut<SimpleAssignmentExprNode>&){};

  /// @note To make the class abstract. But still we have to provide an
  /// out-of-class definition for the destructor.
  virtual ~Visitor() = 0;
};

template <bool is_modifying>
Visitor<is_modifying>::~Visitor() = default;

// One can use these type aliases in cases where the meaning of the template
// parameter is not obvious.

using ModifyingVisitor = Visitor<true>;
using NonModifyingVisitor = Visitor<false>;

#endif  // VISITOR_HPP_
