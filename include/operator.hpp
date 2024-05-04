#ifndef OPERATOR_HPP_
#define OPERATOR_HPP_

#include <cstdint>

enum class BinaryOperator : std::uint8_t {
  kAdd,
  kSub,
  kMul,
  kDiv,
  kMod,
  kGt,
  kGte,
  kLt,
  kLte,
  kEq,
  kNeq,
};

enum class UnaryOperator : std::uint8_t {
  kIncr,
  kDecr,
  kPos,
  kNeg,
  kAddr,
  kDeref,
  kNot,
  kBitComp,
};

enum class PostfixOperator : std::uint8_t {
  kIncr,
  kDecr,
};

#endif
