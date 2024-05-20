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

struct DeclTypeEntry {
  std::string id;
  std::unique_ptr<Type> type;

  DeclTypeEntry(std::string id, std::unique_ptr<Type> type)
      : id{std::move(id)}, type{std::move(type)} {}
};

template <typename EntryType>
class TableTemplate {
 public:
  /// @brief Adds the `entry` to the table if the `id` of the `entry` isn't
  /// already in the table.
  /// @returns The added entry if the `id` of the `entry` isn't
  /// already in the table; otherwise, the original entry.
  std::shared_ptr<EntryType> Add(std::unique_ptr<EntryType> entry) {
    const std::string& id = entry->id;  // to reference id after moved
    if (!Probe(id)) {
      entries_.insert({id, std::shared_ptr<EntryType>{std::move(entry)}});
    }
    return entries_.at(id);
  }
  std::shared_ptr<EntryType> Probe(const std::string& id) const {
    if (entries_.count(id)) {
      return entries_.at(id);
    }
    return nullptr;
  }

 private:
  std::map<std::string, std::shared_ptr<EntryType>> entries_{};
};

/// @brief Stores identifiers.
using SymbolTable = TableTemplate<SymbolEntry>;
/// @brief Stores declared types, such as struct, union.
using DeclTypeTable = TableTemplate<DeclTypeEntry>;

#endif  // SYMBOL_HPP_
