#include "llvm/util.hpp"

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Type.h>

#include <cassert>
#include <memory>
#include <stdexcept>
#include <string>
#include <vector>

#include "type.hpp"

using namespace util;

bool LLVMIRBuilderHelper::HasTerminator(llvm::BasicBlock* bb) {
  for (auto it = bb->begin(); it != bb->end(); ++it) {
    if (it->isTerminator()) {
      return true;
    }
  }
  return false;
}

void LLVMIRBuilderHelper::CreateBrIfNoBrBefore(llvm::BasicBlock* next_bb) {
  auto bb = builder_.GetInsertBlock();
  if (!HasTerminator(bb)) {
    builder_.CreateBr(next_bb);
  }
}

void LLVMIRBuilderHelper::CurrBBFallThroughNextBB(llvm::BasicBlock* curr_bb,
                                                  llvm::BasicBlock* next_bb) {
  if (!HasTerminator(curr_bb)) {
    builder_.SetInsertPoint(curr_bb);
    builder_.CreateBr(next_bb);
  }
}

llvm::BasicBlock* LLVMIRBuilderHelper::FindBBWithNameOf(const std::string& id) {
  auto func = builder_.GetInsertBlock()->getParent();
  for (auto& bb_iter : *func) {
    if (bb_iter.getName() == id) {
      return &(bb_iter);
    }
  }
  return nullptr;
}

llvm::Function* LLVMIRBuilderHelper::CurrFunc() {
  return builder_.GetInsertBlock()->getParent();
}

llvm::Type* LLVMIRBuilderHelper::GetLLVMType(const Type& type) {
  if (type.IsPrim()) {
    if (type.IsEqual(PrimitiveType::kInt)) {
      return builder_.getInt32Ty();
    }
    throw std::runtime_error{"unknown type in GetLLVMType!"};
  } else if (type.IsPtr()) {
    auto ptr_type = dynamic_cast<const PtrType*>(&type);
    auto base_type = ptr_type->base_type().Clone();
    auto llvm_base_type = GetLLVMType(*base_type);
    // Function pointers
    if (llvm_base_type->isFunctionTy()) {
      return llvm_base_type;
    }
    return builder_.getPtrTy();
  } else if (type.IsArr()) {
    auto arr_type = dynamic_cast<const ArrType*>(&type);
    return llvm::ArrayType::get(GetLLVMType(arr_type->element_type()),
                                arr_type->len());
  } else if (type.IsStruct() || type.IsUnion()) {
    auto record_type = dynamic_cast<const RecordType*>(&type);
    std::vector<llvm::Type*> field_types;
    for (auto& field : record_type->fields()) {
      field_types.push_back(GetLLVMType(*(field->type)));
    }
    // A prefix is needed to distinguish struct and union with the same name.
    std::string record_prefix = type.IsStruct() ? "struct_" : "union_";
    return llvm::StructType::create(builder_.getContext(), field_types,
                                    record_prefix + record_type->id());
  } else if (type.IsFunc()) {
    auto func_type = dynamic_cast<const FuncType*>(&type);
    auto return_type = GetLLVMType(func_type->return_type());
    std::vector<llvm::Type*> param_types;
    for (auto& param_type : func_type->param_types()) {
      param_types.push_back(GetLLVMType(*param_type));
    }
    return llvm::FunctionType::get(return_type, param_types, false);
  }

  assert(false);
  return nullptr;
}
