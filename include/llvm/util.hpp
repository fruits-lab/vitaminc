#ifndef LLVM_UTIL_HPP_
#define LLVM_UTIL_HPP_

#include <llvm/IR/IRBuilder.h>

namespace util {

/// @brief A collection of wrappers of LLVM types and functions.
class Util {
 public:
  /// @brief Integer type
  llvm::IntegerType* i32Ty;

  /// @brief NOT operation wrapper since LLVM doesn't provide NOT operator.
  /// @return
  llvm::Value* NotOperation(llvm::Value* val) {
    // Is 0 if the value of its operand compares unequal to 0, 1 if the value
    // of its operand compares equal to 0.
    auto zero = llvm::ConstantInt::get(val->getType(), 0, true);
    auto one = llvm::ConstantInt::get(val->getType(), 1, true);
    auto* isZero = builder_->CreateICmpEQ(val, zero, "isZero");
    return builder_->CreateSelect(isZero, one, zero, "notResult");
  };

  Util(std::unique_ptr<llvm::IRBuilder<>>& builder) : builder_{builder} {
    i32Ty = builder_->getInt32Ty();
  }

 private:
  /// @brief Stores a refernce from the original builder.
  std::unique_ptr<llvm::IRBuilder<>>& builder_;
};

}  // namespace util

#endif  // LLVM_UTIL_HPP_