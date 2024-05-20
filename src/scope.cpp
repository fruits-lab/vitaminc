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

/// @throws `NotInScopeError`
/// @throws `NotInSuchKindOfScopeError`
template <typename Table>
std::shared_ptr<typename Table::EntryType> ScopeStack::AddEntry(
    std::unique_ptr<typename Table::EntryType> entry, ScopeKind kind,
    std::unique_ptr<Table> Scope::*table) {
  ThrowIfNotInScope_();
  for (auto it = scopes_.rbegin(); it != scopes_.rend(); ++it) {
    if (it->kind == kind) {
      return ((*it).*table)->Add(std::move(entry));
    }
  }
  throw NotInSuchKindOfScopeError{""};
}

/// @throws `NotInScopeError`
template <typename Table>
std::shared_ptr<typename Table::EntryType> ScopeStack::LookUpEntry(
    const std::string& id, std::unique_ptr<Table> Scope::*table) const {
  ThrowIfNotInScope_();
  // Iterates backward since we're using the container as a stack.
  for (auto it = scopes_.crbegin(); it != scopes_.crend(); ++it) {
    if (auto entry = ((*it).*table)->Probe(id)) {
      return entry;
    }
  }
  return nullptr;
}

/// @throws `NotInScopeError`
template <typename Table>
std::shared_ptr<typename Table::EntryType> ScopeStack::ProbeEntry(
    const std::string& id, std::unique_ptr<Table> Scope::*table) const {
  ThrowIfNotInScope_();
  return ((*scopes_.rbegin()).*table)->Probe(id);
}

std::shared_ptr<SymbolEntry> ScopeStack::AddSymbol(
    std::unique_ptr<SymbolEntry> entry, ScopeKind kind) {
  return AddEntry<SymbolTable>(std::move(entry), kind, &Scope::symbol_table);
}

std::shared_ptr<SymbolEntry> ScopeStack::LookUpSymbol(
    const std::string& id) const {
  return LookUpEntry<SymbolTable>(id, &Scope::symbol_table);
}

std::shared_ptr<SymbolEntry> ScopeStack::ProbeSymbol(
    const std::string& id) const {
  return ProbeEntry<SymbolTable>(id, &Scope::symbol_table);
}

std::shared_ptr<DeclTypeEntry> ScopeStack::AddType(
    std::unique_ptr<DeclTypeEntry> entry, ScopeKind kind) {
  return AddEntry<DeclTypeTable>(std::move(entry), kind, &Scope::type_table);
}

std::shared_ptr<DeclTypeEntry> ScopeStack::LookUpType(
    const std::string& id) const {
  return LookUpEntry<DeclTypeTable>(id, &Scope::type_table);
}

std::shared_ptr<DeclTypeEntry> ScopeStack::ProbeType(
    const std::string& id) const {
  return ProbeEntry<DeclTypeTable>(id, &Scope::type_table);
}
