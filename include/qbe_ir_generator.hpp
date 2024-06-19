#ifndef QBE_IR_GENERATOR_HPP_
#define QBE_IR_GENERATOR_HPP_

#include <fmt/core.h>

#include <iosfwd>
#include <memory>
#include <utility>
#include <vector>

#include "ast.hpp"
#include "qbe/sigil.hpp"
#include "visitor.hpp"

class QbeIrGenerator : public NonModifyingVisitor {
 public:
  void Visit(const DeclStmtNode&) override;
  void Visit(const LoopInitNode&) override;
  void Visit(const VarDeclNode&) override;
  void Visit(const ArrDeclNode&) override;
  void Visit(const RecordDeclNode&) override;
  void Visit(const FieldNode&) override;
  void Visit(const RecordVarDeclNode&) override;
  void Visit(const ParamNode&) override;
  void Visit(const FuncDefNode&) override;
  void Visit(const CompoundStmtNode&) override;
  void Visit(const ExternDeclNode&) override;
  void Visit(const ProgramNode&) override;
  void Visit(const IfStmtNode&) override;
  void Visit(const WhileStmtNode&) override;
  void Visit(const ForStmtNode&) override;
  void Visit(const ReturnStmtNode&) override;
  void Visit(const GotoStmtNode&) override;
  void Visit(const BreakStmtNode&) override;
  void Visit(const ContinueStmtNode&) override;
  void Visit(const SwitchStmtNode&) override;
  void Visit(const IdLabeledStmtNode&) override;
  void Visit(const CaseStmtNode&) override;
  void Visit(const DefaultStmtNode&) override;
  void Visit(const ExprStmtNode&) override;
  void Visit(const InitExprNode&) override;
  void Visit(const ArrDesNode&) override;
  void Visit(const IdDesNode&) override;
  void Visit(const NullExprNode&) override;
  void Visit(const IdExprNode&) override;
  void Visit(const IntConstExprNode&) override;
  void Visit(const ArgExprNode&) override;
  void Visit(const ArrSubExprNode&) override;
  void Visit(const CondExprNode&) override;
  void Visit(const FuncCallExprNode&) override;
  void Visit(const PostfixArithExprNode&) override;
  void Visit(const RecordMemExprNode&) override;
  void Visit(const UnaryExprNode&) override;
  void Visit(const BinaryExprNode&) override;
  void Visit(const SimpleAssignmentExprNode&) override;

  QbeIrGenerator(std::ostream& output) : output_{output} {}

 private:
  std::ostream& output_;

  static constexpr auto kIndentStr = "\t";

  /// @brief Writes a single instruction with newline.
  /// @note The instruction is indented.
  template <typename... T>
  void WriteInstr_(fmt::format_string<T...> format, T&&... args) {
    Write_(kIndentStr);
    Write_(format, std::forward<T>(args)...);
    Write_("\n");
  }

  /// @brief Writes the definition of a label with newline.
  void WriteLabel_(const qbe::user_defined::BlockLabel& label) {
    Write_("{}\n", label);
  }
  /// @brief Writes the definition of a label with newline.
  void WriteLabel_(const qbe::compiler_generated::BlockLabel& label) {
    Write_("{}\n", label);
  }

  /// @brief Writes the `# ` comment with newline.
  template <typename... T>
  void WriteComment_(fmt::format_string<T...> format, T&&... args) {
    Write_("# ");
    Write_(format, std::forward<T>(args)...);
    Write_("\n");
  }

  /// @brief Writes the formatted string to `output`; can be used to write
  /// multiple instructions and labels at once. The format is fully specified by
  /// the user.
  /// @note This is a convenience function to avoid having to pass `output`
  /// everywhere.
  template <typename... T>
  void Write_(fmt::format_string<T...> format,
              T&&... args) {  // NOLINT(cppcoreguidelines-missing-std-forward):
                              // `make_format_args` takes rvalue references.
    VWrite_(format, fmt::make_format_args(args...));
  }

  /// @note This function is not meant to be used directly.
  void VWrite_(fmt::string_view format, fmt::format_args args);

  /// @brief Called by the code generation of `FuncDefNode` to allocate memory
  /// for the parameters. The value of the parameters are stored in their
  /// corresponding memory locations.
  void AllocMemForParams_(const std::vector<std::unique_ptr<ParamNode>>&);

  /// @brief Called by the code generation of `SwitchStmtNode` to generate the
  /// statement of its cases.
  void GenerateCases_(const SwitchStmtNode&);
  /// @brief Called by the code generation of `SwitchStmtNode` to generate the
  /// condition matching of the cases.
  void GenerateConditions_(
      const SwitchStmtNode&,
      const qbe::compiler_generated::BlockLabel& first_cond_label,
      int ctrl_num);
};

#endif  // QBE_IR_GENERATOR_HPP_
