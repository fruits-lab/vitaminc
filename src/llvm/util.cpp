#include "llvm/util.hpp"

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Type.h>

#include "type.hpp"

using namespace util;

void LLVMIRUtil::CreateBrIfNoBrBefore(llvm::BasicBlock* next_BB) {
  auto BB = builder_->GetInsertBlock();
  bool has_terminator = false;
  for (auto it = BB->begin(); it != BB->end();) {
    if (it->isTerminator()) {
      has_terminator = true;
      break;
    } else {
      ++it;
    }
  }

  if (!has_terminator) {
    builder_->CreateBr(next_BB);
  }
}

void LLVMIRUtil::CurrBBFallThroughNextBB(llvm::BasicBlock* curr_BB,
                                         llvm::BasicBlock* next_BB) {
  auto BB = curr_BB;
  bool has_terminator = false;
  for (auto it = BB->begin(); it != BB->end();) {
    if (it->isTerminator()) {
      has_terminator = true;
      break;
    } else {
      ++it;
    }
  }

  if (!has_terminator) {
    builder_->SetInsertPoint(curr_BB);
    builder_->CreateBr(next_BB);
  }
}

llvm::BasicBlock* LLVMIRUtil::FindBBWithNameOf(const std::string& id) {
  auto func = builder_->GetInsertBlock()->getParent();
  for (auto BB_iter = func->begin(), BB_end = func->end(); BB_iter != BB_end;
       ++BB_iter) {
    if (BB_iter->getName() == id) {
      return &(*BB_iter);
    }
  }

  return nullptr;
}

llvm::Function* LLVMIRUtil::CurrFunc() {
  return builder_->GetInsertBlock()->getParent();
}

llvm::Type* LLVMIRUtil::GetLLVMType(const std::unique_ptr<Type>& type) {
  if (type->IsPtr()) {
    // TODO recursive
    return IntPtrType;
  } else if (type->IsArr()) {
    auto arr_type = dynamic_cast<ArrType*>(type.get());
    return llvm::ArrayType::get(IntType, arr_type->len());
  } else if (type->IsStruct() || type->IsUnion()) {
    std::string record_prefix = type->IsStruct() ? "struct_" : "union_";
    auto record_type = dynamic_cast<RecordType*>(type.get());
    std::vector<llvm::Type*> field_types;
    for (auto& field : record_type->fields()) {
      field_types.push_back(GetLLVMType(field->type));
    }

    return llvm::StructType::create(builder_->getContext(), field_types,
                                    record_prefix + record_type->id());
  } else if (type->IsFunc()) {
    auto func_type = dynamic_cast<FuncType*>(type.get());
    auto return_type = GetLLVMType(func_type->return_type());

    std::vector<llvm::Type*> param_types;
    for (auto& param_type : func_type->param_types()) {
      param_types.push_back(GetLLVMType(param_type));
    }

    return llvm::FunctionType::get(return_type, param_types, false);
  }

  return IntType;
}
