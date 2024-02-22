#include "type.hpp"

#include <string>

std::string ExprTypeToString(ExprType type) {
  switch (type) {
    case ExprType::kInt:
      return "int";
    case ExprType::kUnknown:
    default:
      return "unknown";
  }
}
