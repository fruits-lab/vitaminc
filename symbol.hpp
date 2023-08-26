#ifndef SYMBOL_HPP_
#define SYMBOL_HPP_

#include <map>
#include <memory>
#include <string>
#include <utility>

#include "type.hpp"

struct SymbolEntry {
  std::string id;
  ExprType expr_type;
};

class SymbolTable {
 public:
  /// @brief Adds the `entry` to the table if the `id` of the `entry` isn't
  /// already in the table.
  /// @returns The added entry if the `id` of the `entry` isn't
  /// already in the table; otherwise, the original entry.
  std::shared_ptr<SymbolEntry> Add(std::unique_ptr<SymbolEntry> entry) {
    const std::string& id = entry->id;  // to reference id after moved
    if (!Probe(id)) {
      entries_.insert({id, std::shared_ptr<SymbolEntry>{std::move(entry)}});
    }
    return entries_.at(id);
  }

  std::shared_ptr<SymbolEntry> Probe(const std::string& id) const {
    if (entries_.count(id)) {
      return entries_.at(id);
    }
    return nullptr;
  }

 private:
  std::map<std::string, std::shared_ptr<SymbolEntry>> entries_{};
};

#endif  // SYMBOL_HPP_
