#include "type.hpp"

#include <cstddef>
#include <memory>
#include <string>

bool Type::IsEqual(PrimitiveType that) const noexcept {
  return IsEqual(PrimType{that});
}

bool PrimType::IsEqual(const Type& that) const noexcept {
  if (const auto* that_prim = dynamic_cast<const PrimType*>(&that)) {
    return that_prim->prim_type_ == prim_type_;
  }
  return false;
}

std::size_t PrimType::size() const {
  switch (prim_type_) {
    case PrimitiveType::kInt:
    default:
      return 4;
  }
}

std::string PrimType::ToString() const {
  switch (prim_type_) {
    case PrimitiveType::kInt:
      return "int";
    default:
      return "unknown";
  }
}

std::unique_ptr<Type> PrimType::Clone() const {
  return std::make_unique<PrimType>(prim_type_);
}

bool PtrType::IsEqual(const Type& that) const noexcept {
  if (const auto* that_ptr = dynamic_cast<const PtrType*>(&that)) {
    return that_ptr->base_type_->IsEqual(*base_type_);
  }
  return false;
}

std::size_t PtrType::size() const {
  return 8;  // NOLINT(cppcoreguidelines-avoid-magic-numbers)
}

std::string PtrType::ToString() const {
  return base_type_->ToString() + "*";
}

std::unique_ptr<Type> PtrType::Clone() const {
  return std::make_unique<PtrType>(base_type_->Clone());
}

bool ArrType::IsEqual(const Type& that) const noexcept {
  if (const auto* that_arr = dynamic_cast<const ArrType*>(&that)) {
    // For two array types to be compatible, both shall have compatible element
    // types, and if both size specifiers are present, and are integer constant
    // expressions, then both size specifiers shall have the same constant
    // value.
    return that_arr->element_type_->IsEqual(*element_type_) &&
           that_arr->len_ == len_;
  }
  return false;
}

/// @note The size of an array is (size of base type) * (number of elements).
std::size_t ArrType::size() const {
  return element_type_->size() * len_;
}

std::string ArrType::ToString() const {
  return element_type_->ToString() + "[" + std::to_string(len_) + "]";
}

std::unique_ptr<Type> ArrType::Clone() const {
  return std::make_unique<ArrType>(element_type_->Clone(), len_);
}

std::size_t ArrType::len() const {
  return len_;
}
