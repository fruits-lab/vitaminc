#ifndef QBE_IR_GENERATOR_HPP_
#define QBE_IR_GENERATOR_HPP_

#include <fmt/core.h>

#include <iosfwd>

#include "ast.hpp"
#include "visitor.hpp"

class QbeIrGenerator : public NonModifyingVisitor {
 public:
  void Visit(const LoopInitNode&) override;
  void Visit(const DeclNode&) override;
  void Visit(const BlockStmtNode&) override;
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
  void Visit(const UnaryExprNode&) override;
  void Visit(const BinaryExprNode&) override;
  void Visit(const SimpleAssignmentExprNode&) override;

  QbeIrGenerator(std::ostream& output) : output_{output} {}

 private:
  std::ostream& output_;

  /// @brief Writes out the formatted string to `output`.
  /// @note This is a convenience function to avoid having to pass `output`
  /// everywhere.
  template <typename... T>
  void WriteOut_(
      fmt::format_string<T...> format,
      T&&... args) {  // NOLINT(cppcoreguidelines-missing-std-forward):
                      // `make_format_args` takes rvalue references.
    VWriteOut_(format, fmt::make_format_args(args...));
  }

  /// @note This function is not meant to be used directly.
  void VWriteOut_(fmt::string_view format, fmt::format_args args);
};

#endif  // QBE_IR_GENERATOR_HPP_
