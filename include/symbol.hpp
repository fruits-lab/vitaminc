#ifndef SYMBOL_HPP_
#define SYMBOL_HPP_

#include <map>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "type.hpp"

struct SymbolEntry {
  std::string id;
  Type expr_type;
  std::vector<Type> param_types{};

  SymbolEntry(std::string id, Type expr_type)
      : id{std::move(id)}, expr_type{expr_type} {}
};

class SymbolTable {
 public:
  /// @brief Adds the `entry` to the table if the `id` of the `entry` isn't
  /// already in the table.
  /// @returns The added entry if the `id` of the `entry` isn't
  /// already in the table; otherwise, the original entry.
  std::shared_ptr<SymbolEntry> Add(std::unique_ptr<SymbolEntry> entry);

  std::shared_ptr<SymbolEntry> Probe(const std::string& id) const;

 private:
  std::map<std::string, std::shared_ptr<SymbolEntry>> entries_{};
};

#endif  // SYMBOL_HPP_
