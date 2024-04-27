#include "type.hpp"

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

std::string PtrType::ToString() const {
  return base_type_->ToString() + "*";
}

std::unique_ptr<Type> PtrType::Clone() const {
  return std::make_unique<PtrType>(base_type_->Clone());
}

bool ArrType::IsEqual(const Type& that) const noexcept {
  if (const auto* that_arr = dynamic_cast<const ArrType*>(&that)) {
    return that_arr->base_type_->IsEqual(*base_type_);
  } else if (const auto* that_prim = dynamic_cast<const PrimType*>(&that)) {
    return that_prim->IsEqual(*base_type_);
  }
  return false;
}

std::string ArrType::ToString() const {
  return base_type_->ToString();
}

std::unique_ptr<Type> ArrType::Clone() const {
  return std::make_unique<ArrType>(base_type_->Clone(), len_);
}

std::size_t ArrType::GetLen() const {
  return len_;
}
