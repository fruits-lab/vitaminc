#ifndef OPERATOR_HPP_
#define OPERATOR_HPP_

enum class BinaryOperator {
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

enum class UnaryOperator {
  kIncr,
  kDecr,
  kNeg,
  kAddr,
  kDeref,
  kNot,
  kBitComp,
};

#endif
