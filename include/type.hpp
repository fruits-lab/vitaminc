#ifndef TYPE_HPP_
#define TYPE_HPP_

#include <cstdint>
#include <string>

/// @note C has a lots of primitive type. We might need to use classes to
/// implement type coercion rules.
enum class PrimitiveType : std::uint8_t {
  kUnknown = 0,  // HACK: default initialized to 0 -> unknown
  kInt,
};

struct Type {
  Type(PrimitiveType prim_type = PrimitiveType::kUnknown, bool is_ptr = false)
      : prim_type{prim_type}, is_ptr{is_ptr} {};

  bool operator==(const Type& that) const noexcept {
    return prim_type == that.prim_type && is_ptr == that.is_ptr;
  }
  bool operator!=(const Type& that) const noexcept {
    return !operator==(that);
  }

  PrimitiveType prim_type;
  bool is_ptr;
};

std::string TypeToString(Type type);

#endif  // TYPE_HPP_
