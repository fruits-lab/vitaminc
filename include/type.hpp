#ifndef TYPE_HPP_
#define TYPE_HPP_

#include <cstddef>
#include <cstdint>
#include <memory>
#include <string>
#include <utility>
#include <vector>

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
  virtual bool IsFunc() const noexcept {
    return false;
  }

  virtual bool IsEqual(const Type& that) const noexcept = 0;
  /// @brief A convenience function to compare with a primitive type.
  bool IsEqual(PrimitiveType that) const noexcept;

  /// @note `that` is not necessarily convertible to `this`.
  /// @note Two types that are equal are always compatible.
  bool IsConvertibleTo(const Type& that) const noexcept {
    return IsEqual(that) || ConvertibleHook_(that);
  }

  virtual std::size_t size()  // NOLINT(readability-identifier-naming)
      const = 0;
  virtual std::string ToString() const = 0;
  virtual std::unique_ptr<Type> Clone() const = 0;

  virtual ~Type() = default;
  Type() = default;

  // Delete copy/move operations to avoid slicing.

  Type(const Type&) = delete;
  Type(Type&&) = delete;
  Type& operator=(const Type&) = delete;
  Type& operator=(Type&&) = delete;

 private:
  /// @note By default, types are only convertible to themselves. Derived types
  /// should override this hook to provide additional compatibility rules.
  virtual bool ConvertibleHook_(const Type& that) const noexcept {
    return false;
  }
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
  std::size_t size() const override;
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
  std::size_t size() const override;
  std::string ToString() const override;
  std::unique_ptr<Type> Clone() const override;

 private:
  std::unique_ptr<Type> base_type_;
};

class ArrType : public Type {
 public:
  /// @param element_type The type of a single element in the array.
  explicit ArrType(std::unique_ptr<Type> element_type, std::size_t len)
      : element_type_{std::move(element_type)}, len_{len} {}

  const Type& element_type()  // NOLINT(readability-identifier-naming)
      const noexcept {
    return *element_type_;
  }

  bool IsArr() const noexcept override {
    return true;
  }

  bool IsEqual(const Type& that) const noexcept override;
  std::size_t size() const override;
  std::string ToString() const override;
  std::unique_ptr<Type> Clone() const override;

  std::size_t len() const;  // NOLINT(readability-identifier-naming)

 private:
  std::unique_ptr<Type> element_type_;
  std::size_t len_;
};

class FuncType : public Type {
 public:
  FuncType(std::unique_ptr<Type> return_type,
           std::vector<std::unique_ptr<Type>> param_types)
      : return_type_{std::move(return_type)},
        param_types_{std::move(param_types)} {}

  const Type& return_type()  // NOLINT(readability-identifier-naming)
      const noexcept {
    return *return_type_;
  }

  // XXX: Consider exposing iterators for constness.
  const std::vector<std::unique_ptr<Type>>&
  param_types()  // NOLINT(readability-identifier-naming)
      const noexcept {
    return param_types_;
  }

  bool IsFunc() const noexcept override {
    return true;
  }

  bool IsEqual(const Type& that) const noexcept override;
  std::size_t size() const override;
  std::string ToString() const override;
  std::unique_ptr<Type> Clone() const override;

 private:
  std::unique_ptr<Type> return_type_;
  std::vector<std::unique_ptr<Type>> param_types_;

  bool ConvertibleHook_(const Type& that) const noexcept override;
};

#endif  // TYPE_HPP_
