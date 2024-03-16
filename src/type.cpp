#include "type.hpp"

#include <string>

std::string TypeToString(Type type) {
  std::string out = "";
  switch (type.prim_type) {
    case PrimitiveType::kInt:
      out.append("int");
      break;
    case PrimitiveType::kUnknown:
    default:
      out.append("unknown");
  }

  if (type.is_ptr) {
    out.append("*");
  }

  return out;
}
