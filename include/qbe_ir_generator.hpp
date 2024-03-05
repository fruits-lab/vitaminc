#ifndef QBE_IR_GENERATOR_HPP_
#define QBE_IR_GENERATOR_HPP_

#include <fmt/core.h>

#include <iosfwd>
#include <utility>

#include "ast.hpp"
#include "qbe/sigil.hpp"
#include "visitor.hpp"

class QbeIrGenerator : public NonModifyingVisitor {
 public:
  void Visit(const LoopInitNode&) override;
  void Visit(const DeclNode&) override;
  void Visit(const ParamNode&) override;
  void Visit(const FuncDefNode&) override;
  void Visit(const CompoundStmtNode&) override;
  void Visit(const ProgramNode&) override;
  void Visit(const IfStmtNode&) override;
  void Visit(const WhileStmtNode&) override;
  void Visit(const ForStmtNode&) override;
  void Visit(const ReturnStmtNode&) override;
  void Visit(const BreakStmtNode&) override;
  void Visit(const ContinueStmtNode&) override;
  void Visit(const ExprStmtNode&) override;
  void Visit(const NullExprNode&) override;
  void Visit(const IdExprNode&) override;
  void Visit(const IntConstExprNode&) override;
  void Visit(const FunCallExprNode&) override;
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
  void WriteLabel_(const qbe::BlockLabel& label) {
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
};

#endif  // QBE_IR_GENERATOR_HPP_
