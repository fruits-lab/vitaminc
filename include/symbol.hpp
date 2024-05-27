#ifndef SYMBOL_HPP_
#define SYMBOL_HPP_

#include <map>
#include <memory>
#include <string>
#include <utility>

#include "type.hpp"

struct SymbolEntry {
  std::string id;
  std::unique_ptr<Type> type;

  SymbolEntry(std::string id, std::unique_ptr<Type> expr_type)
      : id{std::move(id)}, type{std::move(expr_type)} {}
};

struct TypeEntry {
  std::string id;
  std::unique_ptr<Type> type;

  TypeEntry(std::string id, std::unique_ptr<Type> type)
      : id{std::move(id)}, type{std::move(type)} {}
};

template <typename Entry>
class TableTemplate {
 public:
  /// @brief Adds the `entry` to the table if the `id` of the `entry` isn't
  /// already in the table.
  /// @returns The added entry if the `id` of the `entry` isn't
  /// already in the table; otherwise, the original entry.
  std::shared_ptr<Entry> Add(std::unique_ptr<Entry> entry);
  std::shared_ptr<Entry> Probe(const std::string& id) const;

 private:
  std::map<std::string, std::shared_ptr<Entry>> entries_{};
};

using SymbolTable = TableTemplate<SymbolEntry>;
/// @brief Stores declared types, such as struct, union.
using TypeTable = TableTemplate<TypeEntry>;

#endif  // SYMBOL_HPP_
