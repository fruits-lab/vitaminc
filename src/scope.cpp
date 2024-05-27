#include "scope.hpp"

#include <cstdint>
#include <memory>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

#include "symbol.hpp"

void ScopeStack::PushScope(ScopeKind kind) {
  if (should_merge_with_next_scope_) {
    if (scopes_.back().kind != kind) {
      throw ScopesOfDifferentKindIsNotMergeableError{""};
    }
    should_merge_with_next_scope_ = false;
    return;
  }
  scopes_.emplace_back(kind, std::make_unique<SymbolTable>(),
                       std::make_unique<TypeTable>());
}

void ScopeStack::PopScope() {
  ThrowIfNotInScope_();
  scopes_.pop_back();
}

void ScopeStack::MergeWithNextScope() {
  ThrowIfNotInScope_();
  should_merge_with_next_scope_ = true;
}

std::shared_ptr<SymbolEntry> ScopeStack::AddSymbol(
    std::unique_ptr<SymbolEntry> entry, ScopeKind kind) {
  return AddEntry<SymbolTable, SymbolEntry>(std::move(entry), kind,
                                            &Scope::symbol_table);
}

std::shared_ptr<SymbolEntry> ScopeStack::LookUpSymbol(
    const std::string& id) const {
  return LookUpEntry<SymbolTable, SymbolEntry>(id, &Scope::symbol_table);
}

std::shared_ptr<SymbolEntry> ScopeStack::ProbeSymbol(
    const std::string& id) const {
  return ProbeEntry<SymbolTable, SymbolEntry>(id, &Scope::symbol_table);
}

std::shared_ptr<TypeEntry> ScopeStack::AddType(std::unique_ptr<TypeEntry> entry,
                                               ScopeKind kind) {
  return AddEntry<TypeTable, TypeEntry>(std::move(entry), kind,
                                        &Scope::type_table);
}

std::shared_ptr<TypeEntry> ScopeStack::LookUpType(const std::string& id) const {
  return LookUpEntry<TypeTable, TypeEntry>(id, &Scope::type_table);
}

std::shared_ptr<TypeEntry> ScopeStack::ProbeType(const std::string& id) const {
  return ProbeEntry<TypeTable, TypeEntry>(id, &Scope::type_table);
}
