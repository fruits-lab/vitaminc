#ifndef TYPE_HPP_
#define TYPE_HPP_

/// @note C has a lots of primitive type. We might need to use classes to
/// implement type coercion rules.
enum class ExprType {
  KUnknown = 0,  // HACK: default initialized to 0 -> unknown
  KInt,
};

// FIXME: multiple definition error without inline
inline const char* ExprTypeToCString(ExprType type) {
  switch (type) {
    case ExprType::KInt:
      return "int";
    case ExprType::KUnknown:
    default:
      return "unknown";
  }
}
#endif  // TYPE_HPP_
