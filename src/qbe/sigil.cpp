#include "qbe/sigil.hpp"

#include <fmt/core.h>
#include <fmt/format.h>

#include <string_view>

namespace qbe {

Sigil::~Sigil() = default;

}  // namespace qbe

auto fmt::formatter<qbe::FuncScopeTemp>::format(const qbe::FuncScopeTemp& s,
                                                fmt::format_context& ctx) const
    -> decltype(ctx.out()) {
  return fmt::formatter<std::string_view>::format(
      fmt::format("{}.{}", '%', s.name()), ctx);
}

auto fmt::formatter<qbe::BlockLabel>::format(const qbe::BlockLabel& s,
                                             fmt::format_context& ctx) const
    -> decltype(ctx.out()) {
  return fmt::formatter<std::string_view>::format(
      fmt::format("{}.{}", '@', s.name()), ctx);
}

auto fmt::formatter<qbe::GlobalPointer>::format(const qbe::GlobalPointer& s,
                                                fmt::format_context& ctx) const
    -> decltype(ctx.out()) {
  return fmt::formatter<std::string_view>::format(
      fmt::format("{}.{}", '$', s.name()), ctx);
}

auto fmt::formatter<qbe::AggregateType>::format(const qbe::AggregateType& s,
                                                fmt::format_context& ctx) const
    -> decltype(ctx.out()) {
  return fmt::formatter<std::string_view>::format(
      fmt::format("{}.{}", ':', s.name()), ctx);
}
