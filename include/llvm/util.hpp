#ifndef LLVM_UTIL_HPP_
#define LLVM_UTIL_HPP_

#include <llvm/IR/IRBuilder.h>

namespace util {

/// @brief A collection of wrappers of LLVM types and functions.
class Util {
 public:
  /// @brief Integer type
  llvm::IntegerType* i32Ty;

  Util(std::unique_ptr<llvm::IRBuilder<>>& builder) : builder_{builder} {
    i32Ty = builder_->getInt32Ty();
  }

 private:
  /// @brief Stores a reference from the original builder.
  std::unique_ptr<llvm::IRBuilder<>>& builder_;
};

}  // namespace util

#endif  // LLVM_UTIL_HPP_