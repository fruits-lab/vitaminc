#include "scope.hpp"

#include <cstdint>
#include <memory>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

#include "symbol.hpp"

/// @throws `ScopesOfDifferentKindIsNotMergeableError` if the previous scope
/// had set to merge with the next (this) scope, which is of a different kind.
void ScopeStack::PushScope(ScopeKind kind) {
  if (should_merge_with_next_scope_) {
    if (scopes_.back().kind != kind) {
      throw ScopesOfDifferentKindIsNotMergeableError{""};
    }
    should_merge_with_next_scope_ = false;
    return;
  }
  scopes_.emplace_back(kind, std::make_unique<SymbolTable>(),
                       std::make_unique<DeclTypeTable>());
}

/// @throws `NotInScopeError`
void ScopeStack::PopScope() {
  ThrowIfNotInScope_();
  scopes_.pop_back();
}

/// @throws `NotInScopeError` if currently not in any scope.
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

std::shared_ptr<DeclTypeEntry> ScopeStack::AddType(
    std::unique_ptr<DeclTypeEntry> entry, ScopeKind kind) {
  return AddEntry<DeclTypeTable, DeclTypeEntry>(std::move(entry), kind,
                                                &Scope::type_table);
}

std::shared_ptr<DeclTypeEntry> ScopeStack::LookUpType(
    const std::string& id) const {
  return LookUpEntry<DeclTypeTable, DeclTypeEntry>(id, &Scope::type_table);
}

std::shared_ptr<DeclTypeEntry> ScopeStack::ProbeType(
    const std::string& id) const {
  return ProbeEntry<DeclTypeTable, DeclTypeEntry>(id, &Scope::type_table);
}
