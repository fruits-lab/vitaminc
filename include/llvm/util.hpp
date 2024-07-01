#ifndef LLVM_UTIL_HPP_
#define LLVM_UTIL_HPP_

#include <llvm/IR/IRBuilder.h>

namespace util {

/// @brief A collection of wrappers of LLVM types and functions.
class Util {
 public:
  /// @brief Integer type
  llvm::IntegerType* intTy;
  /// @brief Pointer type
  llvm::PointerType* intPtrTy;

  /// @brief Every LLVM basic block can only have one terminator instruction.
  /// This function can check if there are terminator instructions before the
  /// current insert point. If no, then it will create an unconditional branch
  /// to the next basic block. If yes, then it will not create branch
  /// instruction.
  void CreateBrIfNoBrBefore(llvm::BasicBlock* next_BB);

  /// @brief Create a branch instruction to the next basic block.
  void CurrBBFallThroughNextBB(llvm::BasicBlock* curr_BB,
                               llvm::BasicBlock* next_BB);

  Util(std::unique_ptr<llvm::IRBuilder<>>& builder) : builder_{builder} {
    intTy = builder_->getInt32Ty();
    intPtrTy = builder_->getPtrTy();
  }

 private:
  /// @brief Stores a reference from the original builder.
  std::unique_ptr<llvm::IRBuilder<>>& builder_;
};

}  // namespace util

#endif  // LLVM_UTIL_HPP_
