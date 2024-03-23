#ifndef QBE_SIGIL_HPP_
#define QBE_SIGIL_HPP_

#include <fmt/core.h>
#include <fmt/format.h>

#include <string>
#include <string_view>

namespace qbe {

// QBE makes heavy use of sigils, all user-defined (as well as
// compiler-generated) names are prefixed with a sigil. This is to avoid keyword
// conflicts, and also to quickly spot the scope and nature of identifiers.
// This class is a strongly-typed wrapper for names, adding the corresponding
// sigil as a prefix.

// NOTE: Inheritance is not used for the following reasons:
// 1. The kind of sigil is unlikely to be extended.
// 2. All sigils have the same interface and behavior, except for the prefix.
// 3. Polymorphic objects cannot be easily copied or moved.
// Thus, template specialization is used instead.

namespace user_defined {

/// @note This class is not meant to be used directly, use the aliases.
template <char prefix>
class Sigil {
 public:
  /// @note No unique number because the name should already be unique.
  Sigil(std::string_view name) : name_{name} {}

  std::string Repr() const {
    return fmt::format("{}{}", prefix, name_);
  }

 private:
  std::string name_;
};

/// @brief Block labels (user-defined).
using BlockLabel = Sigil<'@'>;
/// @brief Globals (represented by a pointer) (user-defined).
using GlobalPointer = Sigil<'$'>;
/// @brief function-scope temporaries (user-defined).
using FuncScopeTemp = Sigil<'%'>;
/// @brief User-defined Aggregate Types (user-defined).
using AggregateType = Sigil<':'>;

}  // namespace user_defined

namespace compiler_generated {

/// @note This class is not meant to be used directly, use the aliases.
template <char prefix>
class Sigil {
 public:
  Sigil(std::string_view name, int number)
      : name_{fmt::format("{}.{}", name, number)} {}
  Sigil(int number) : name_{std::to_string(number)} {}

  /// @note Add an additional `.` before the name.
  std::string Repr() const {
    return fmt::format("{}.{}", prefix, name_);
  }

 private:
  std::string name_;
};

/// @brief Block labels (compiler generated).
using BlockLabel = Sigil<'@'>;
/// @brief Globals (represented by a pointer) (compiler generated).
using GlobalPointer = Sigil<'$'>;
/// @brief function-scope temporaries (compiler generated).
using FuncScopeTemp = Sigil<'%'>;
/// @brief User-defined Aggregate Types (compiler generated).
using AggregateType = Sigil<':'>;

}  // namespace compiler_generated

}  // namespace qbe

template <char prefix>
struct fmt::formatter<qbe::user_defined::Sigil<prefix>>
    : fmt::formatter<std::string_view> {
  auto format(const qbe::user_defined::Sigil<prefix>& s,
              fmt::format_context& ctx) const -> decltype(ctx.out()) {
    return fmt::format_to(ctx.out(), "{}", s.Repr());
  }
};
template <char prefix>
struct fmt::formatter<qbe::compiler_generated::Sigil<prefix>>
    : fmt::formatter<std::string_view> {
  auto format(const qbe::compiler_generated::Sigil<prefix>& s,
              fmt::format_context& ctx) const -> decltype(ctx.out()) {
    return fmt::format_to(ctx.out(), "{}", s.Repr());
  }
};

#endif  // QBE_SIGIL_HPP_
