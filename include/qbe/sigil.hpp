#ifndef QBE_SIGIL_HPP_
#define QBE_SIGIL_HPP_

#include <fmt/core.h>
#include <fmt/format.h>

#include <string>
#include <string_view>
#include <utility>
#include <variant>

namespace qbe {

/// @brief QBE makes heavy use of sigils, all user-defined names are prefixed
/// with a sigil. This is to avoid keyword conflicts, and also to quickly spot
/// the scope and nature of identifiers. This class is a strongly-typed wrapper
/// for names, adding the corresponding sigil as a prefix.
/// @note This is an abstract class.
class Sigil {
 public:
  /// @brief Returns the name.
  std::string_view name()  // NOLINT(readability-identifier-naming): Accessors
                           // may be named like variables.
      const noexcept {
    return name_;
  }

  /// @param name The name may be a meaningful string or an meaningless integer.
  Sigil(std::variant<std::string, int> name) noexcept
      : name_{std::holds_alternative<std::string>(name)
                  ? std::move(std::get<std::string>(name))
                  : std::to_string(std::get<int>(name))} {}

  /// @note To make the class abstract.
  virtual ~Sigil() = 0;

  // Delete copy/move operations to avoid slicing.

  Sigil(const Sigil&) = delete;
  Sigil(Sigil&&) = delete;
  Sigil& operator=(const Sigil&) = delete;
  Sigil& operator=(Sigil&&) = delete;

 private:
  std::string name_;
};

/// @brief Block labels.
class BlockLabel : public Sigil {
 public:
  using Sigil::Sigil;

  /// @brief For labels, we usually have a meaningful name followed by a number.
  /// They are separated by a dot.
  BlockLabel(std::string_view name, int number) noexcept
      : Sigil{fmt::format("{}.{}", name, number)} {}
};

/// @brief Globals (represented by a pointer).
class GlobalPointer : public Sigil {
 public:
  using Sigil::Sigil;
};

/// @brief function-scope temporaries.
class FuncScopeTemp : public Sigil {
 public:
  using Sigil::Sigil;
};

/// @brief User-defined Aggregate Types.
class AggregateType : public Sigil {
 public:
  using Sigil::Sigil;
};

}  // namespace qbe

template <>
struct fmt::formatter<qbe::FuncScopeTemp> : fmt::formatter<std::string_view> {
  auto format(const qbe::FuncScopeTemp& s, fmt::format_context& ctx) const
      -> decltype(ctx.out());
};

template <>
struct fmt::formatter<qbe::BlockLabel> : fmt::formatter<std::string_view> {
  auto format(const qbe::BlockLabel& s, fmt::format_context& ctx) const
      -> decltype(ctx.out());
};

template <>
struct fmt::formatter<qbe::GlobalPointer> : fmt::formatter<std::string_view> {
  auto format(const qbe::GlobalPointer& s, fmt::format_context& ctx) const
      -> decltype(ctx.out());
};

template <>
struct fmt::formatter<qbe::AggregateType> : fmt::formatter<std::string_view> {
  auto format(const qbe::AggregateType& s, fmt::format_context& ctx) const
      -> decltype(ctx.out());
};

#endif  // QBE_SIGIL_HPP_
