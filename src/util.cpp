#include "util.hpp"

#include <string>

std::string Indenter::Indent() const {
  return std::string(  // NOLINT(modernize-return-braced-init-list:
                       // False-positive on (size, value) constructor.
      size_per_level_ * level_, symbol_);
}

void Indenter::IncreaseLevel() {
  if (HasNoLevelLimit_() || level_ < max_level_) {
    ++level_;
  }
}

void Indenter::DecreaseLevel() {
  if (level_) {
    --level_;
  }
}

bool Indenter::HasNoLevelLimit_() const {
  return 0 == max_level_;
}
