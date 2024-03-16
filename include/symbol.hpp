#ifndef SYMBOL_HPP_
#define SYMBOL_HPP_

#include <map>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "type.hpp"

struct ParamType {
  ExprType type;
  bool is_pointer = false;

  ParamType(ExprType type, bool is_pointer = false)
      : type{type}, is_pointer{is_pointer} {}
};

struct SymbolEntry {
  std::string id;
  ExprType expr_type{ExprType::kUnknown};
  bool is_pointer = false;
  std::vector<std::unique_ptr<ParamType>> param_types{};

  SymbolEntry(std::string id, bool is_pointer = false)
      : id{std::move(id)}, is_pointer{is_pointer} {}
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
