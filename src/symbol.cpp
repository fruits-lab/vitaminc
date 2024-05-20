#include "symbol.hpp"

#include <map>
#include <memory>
#include <string>
#include <utility>

std::shared_ptr<SymbolEntry> SymbolTable::Add(
    std::unique_ptr<SymbolEntry> entry) {
  const std::string& id = entry->id;  // to reference id after moved
  if (!Probe(id)) {
    entries_.insert({id, std::shared_ptr<SymbolEntry>{std::move(entry)}});
  }
  return entries_.at(id);
}

std::shared_ptr<SymbolEntry> SymbolTable::Probe(const std::string& id) const {
  if (entries_.count(id)) {
    return entries_.at(id);
  }
  return nullptr;
}

std::shared_ptr<DeclTypeEntry> DeclTypeTable::Add(
    std::unique_ptr<DeclTypeEntry> entry) {
  const std::string& id = entry->id;  // to reference id after moved
  if (!Probe(id)) {
    entries_.insert({id, std::shared_ptr<DeclTypeEntry>{std::move(entry)}});
  }
  return entries_.at(id);
}

std::shared_ptr<DeclTypeEntry> DeclTypeTable::Probe(
    const std::string& id) const {
  if (entries_.count(id)) {
    return entries_.at(id);
  }
  return nullptr;
}