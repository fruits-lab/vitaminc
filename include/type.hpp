#ifndef TYPE_HPP_
#define TYPE_HPP_

#include <cstddef>
#include <cstdint>
#include <memory>
#include <string>
#include <utility>

/// @note C has a lots of primitive type. We might need to use classes to
/// implement type coercion rules.
enum class PrimitiveType : std::uint8_t {
  kUnknown = 0,  // HACK: default initialized to 0 -> unknown
  kInt,
};

/// @brief The abstract base class for all types.
class Type {
 public:
  virtual bool IsPtr() const noexcept {
    return false;
  }
  virtual bool IsPrim() const noexcept {
    return false;
  }
  virtual bool IsArr() const noexcept {
    return false;
  }

  virtual bool IsEqual(const Type& that) const noexcept = 0;
  /// @brief A convenience function to compare with a primitive type.
  bool IsEqual(PrimitiveType that) const noexcept;

  virtual std::size_t ToSize() const = 0;
  virtual std::string ToString() const = 0;
  virtual std::unique_ptr<Type> Clone() const = 0;

  virtual ~Type() = default;
  Type() = default;

  // Delete copy/move operations to avoid slicing.

  Type(const Type&) = delete;
  Type(Type&&) = delete;
  Type& operator=(const Type&) = delete;
  Type& operator=(Type&&) = delete;
};

/// @brief A primitive type wrapper.
class PrimType : public Type {
 public:
  /// @note Implicit conversion is intentional.
  PrimType(PrimitiveType prim_type) : prim_type_{prim_type} {}

  bool IsPrim() const noexcept override {
    return true;
  }

  bool IsEqual(const Type& that) const noexcept override;
  std::size_t ToSize() const override;
  std::string ToString() const override;
  std::unique_ptr<Type> Clone() const override;

 private:
  PrimitiveType prim_type_;
};

class PtrType : public Type {
 public:
  explicit PtrType(std::unique_ptr<Type> base_type)
      : base_type_{std::move(base_type)} {}

  const Type& base_type()  // NOLINT(readability-identifier-naming)
      const noexcept {
    return *base_type_;
  }

  bool IsPtr() const noexcept override {
    return true;
  }

  bool IsEqual(const Type& that) const noexcept override;
  std::size_t ToSize() const override;
  std::string ToString() const override;
  std::unique_ptr<Type> Clone() const override;

 private:
  std::unique_ptr<Type> base_type_;
  const std::size_t size_ = 8;
};

class ArrType : public Type {
 public:
  explicit ArrType(std::unique_ptr<Type> base_type, std::size_t len)
      : base_type_{std::move(base_type)}, len_{len} {}

  const Type& base_type()  // NOLINT(readability-identifier-naming)
      const noexcept {
    return *base_type_;
  }

  bool IsArr() const noexcept override {
    return true;
  }

  bool IsEqual(const Type& that) const noexcept override;
  std::size_t ToSize() const override;
  std::string ToString() const override;
  std::unique_ptr<Type> Clone() const override;

  std::size_t GetLen() const;

 private:
  std::unique_ptr<Type> base_type_;
  std::size_t len_;
};

#endif  // TYPE_HPP_
