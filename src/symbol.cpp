#include "symbol.hpp"

#include <map>
#include <memory>
#include <string>
#include <utility>

template <typename Entry>
std::shared_ptr<Entry> TableTemplate<Entry>::Add(std::unique_ptr<Entry> entry) {
  const std::string& id = entry->id;  // to reference id after moved
  if (!Probe(id)) {
    entries_.insert({id, std::shared_ptr<Entry>{std::move(entry)}});
  }
  return entries_.at(id);
}

template <typename Entry>
std::shared_ptr<Entry> TableTemplate<Entry>::Probe(
    const std::string& id) const {
  if (entries_.count(id)) {
    return entries_.at(id);
  }
  return nullptr;
}

template class TableTemplate<SymbolEntry>;
template class TableTemplate<TypeEntry>;
