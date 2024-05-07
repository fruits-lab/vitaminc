#include "type.hpp"

#include <cstddef>
#include <memory>
#include <string>
#include <utility>
#include <vector>

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
  // For function pointer types, the '*' is placed between the return type and
  // the parameter list.
  if (const auto* base_func = dynamic_cast<const FuncType*>(base_type_.get())) {
    auto str = base_func->return_type().ToString() + " (*)(";
    for (auto i = std::size_t{0}, e = base_func->param_types().size(); i < e;
         ++i) {
      str += base_func->param_types().at(i)->ToString();
      if (i < e - 1) {
        str += ", ";
      }
    }
    str += ")";
    return str;
  }
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

bool FuncType::IsEqual(const Type& that) const noexcept {
  if (const auto* that_func = dynamic_cast<const FuncType*>(&that)) {
    if (that_func->param_types_.size() != param_types_.size()) {
      return false;
    }
    if (!that_func->return_type_->IsEqual(*return_type_)) {
      return false;
    }
    for (auto i = std::size_t{0}, e = param_types_.size(); i < e; ++i) {
      if (!that_func->param_types_.at(i)->IsEqual(*param_types_.at(i))) {
        return false;
      }
    }
    return true;
  }
  return false;
}

bool FuncType::ConvertibleHook_(const Type& that) const noexcept {
  // A function type can be implicitly converted to a pointer to the function.
  if (const auto* that_ptr = dynamic_cast<const PtrType*>(&that)) {
    return this->IsConvertibleTo(that_ptr->base_type());
  }
  return false;
}

std::size_t FuncType::size() const {
  const auto pointer_size = 8;
  return pointer_size;
}

std::string FuncType::ToString() const {
  auto str = return_type_->ToString() + " (";
  for (auto i = std::size_t{0}, e = param_types_.size(); i < e; ++i) {
    str += param_types_.at(i)->ToString();
    if (i < e - 1) {
      str += ", ";
    }
  }
  str += ")";
  return str;
}

std::unique_ptr<Type> FuncType::Clone() const {
  auto cloned_return_type = return_type_->Clone();
  auto cloned_param_types = std::vector<std::unique_ptr<Type>>{};
  for (const auto& param_type : param_types_) {
    cloned_param_types.push_back(param_type->Clone());
  }
  return std::make_unique<FuncType>(std::move(cloned_return_type),
                                    std::move(cloned_param_types));
}
