#ifndef LLVM_UTIL_HPP_
#define LLVM_UTIL_HPP_

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Type.h>

#include <memory>
#include <string>

#include "type.hpp"

namespace util {

/// @brief A collection of wrappers of LLVM types and functions.
class LLVMIRUtil {
 public:
  /// @brief LLVM Integer type
  llvm::IntegerType* IntType();
  /// @brief LLVM Pointer type
  llvm::PointerType* IntPtrType();

  /// @brief Every LLVM basic block can only have one terminator instruction.
  /// This function can check if there are terminator instructions before the
  /// current insert point. If no, then it will create an unconditional branch
  /// to the next basic block. If yes, then it will not create branch
  /// instruction.
  void CreateBrIfNoBrBefore(llvm::BasicBlock* next_bb);

  /// @brief Create a branch instruction to the next basic block.
  void CurrBBFallThroughNextBB(llvm::BasicBlock* curr_bb,
                               llvm::BasicBlock* next_bb);

  /// @brief Find the basic block with the same name as `id` within the current
  /// function.
  /// @param id The name of the target basic block.
  /// @return A pointer to basic block if found. `nullptr` if not found.
  llvm::BasicBlock* FindBBWithNameOf(const std::string& id);

  /// @brief Get the current function.
  /// @return A pointer to the current function.
  llvm::Function* CurrFunc();

  /// @brief Get the corresponding LLVM type from our type.
  /// @note For Function Pointers, even though it is a pointer type, we return
  /// `FunctionType` instead of `PointerType` because `FunctionType` is needed
  /// for creating LLVM IR function call.
  /// @throw `std::runtime_error` if the `type` is not unknown.
  llvm::Type* GetLLVMType(const Type& type);

  LLVMIRUtil(std::unique_ptr<llvm::IRBuilder<>>& builder) : builder_{builder} {}

 private:
  /// @brief Stores a reference from the original builder.
  std::unique_ptr<llvm::IRBuilder<>>&  // XXX: Ignore check until refactor.
      builder_;  // NOLINT(cppcoreguidelines-avoid-const-or-ref-data-members)
};

}  // namespace util

#endif  // LLVM_UTIL_HPP_
