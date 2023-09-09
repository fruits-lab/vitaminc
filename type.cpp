#include "type.hpp"

const char* ExprTypeToCString(ExprType type) {
  switch (type) {
    case ExprType::kInt:
      return "int";
    case ExprType::kUnknown:
    default:
      return "unknown";
  }
}
