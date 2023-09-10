#ifndef TYPE_HPP_
#define TYPE_HPP_

/// @note C has a lots of primitive type. We might need to use classes to
/// implement type coercion rules.
enum class ExprType {
  kUnknown = 0,  // HACK: default initialized to 0 -> unknown
  kInt,
};

const char* ExprTypeToCString(ExprType type);

#endif  // TYPE_HPP_
