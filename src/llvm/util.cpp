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

llvm::Type* LLVMIRUtil::GetLLVMType(const std::unique_ptr<Type>& type) {
  if (type->IsPtr()) {
    return IntPtrType;
  }

  return IntType;
}