#include "scope.hpp"

#include <memory>
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

ScopeKind ScopeStack::CurrentScopeKind() {
  ThrowIfNotInScope_();
  return scopes_.back().kind;
}

void ScopeStack::MergeWithNextScope() {
  ThrowIfNotInScope_();
  should_merge_with_next_scope_ = true;
}

template <typename Entry>
std::shared_ptr<Entry> ScopeStack::AddEntry_(
    std::unique_ptr<Entry> entry, ScopeKind kind,
    std::unique_ptr<TableTemplate<Entry>> Scope::*table) {
  ThrowIfNotInScope_();
  for (auto it = scopes_.rbegin(); it != scopes_.rend(); ++it) {
    if (it->kind == kind) {
      return ((*it).*table)->Add(std::move(entry));
    }
  }
  throw NotInSuchKindOfScopeError{""};
}

template <typename Entry>
std::shared_ptr<Entry> ScopeStack::LookUpEntry_(
    const std::string& id,
    std::unique_ptr<TableTemplate<Entry>> Scope::*table) const {
  ThrowIfNotInScope_();
  // Iterates backward since we're using the container as a stack.
  for (auto it = scopes_.crbegin(); it != scopes_.crend(); ++it) {
    if (auto entry = ((*it).*table)->Probe(id)) {
      return entry;
    }
  }
  return nullptr;
}

template <typename Entry>
std::shared_ptr<Entry> ScopeStack::ProbeEntry_(
    const std::string& id,
    std::unique_ptr<TableTemplate<Entry>> Scope::*table) const {
  ThrowIfNotInScope_();
  return (scopes_.back().*table)->Probe(id);
}

std::shared_ptr<SymbolEntry> ScopeStack::AddSymbol(
    std::unique_ptr<SymbolEntry> entry, ScopeKind kind) {
  return AddEntry_<SymbolEntry>(std::move(entry), kind, &Scope::symbol_table);
}

std::shared_ptr<SymbolEntry> ScopeStack::LookUpSymbol(
    const std::string& id) const {
  return LookUpEntry_<SymbolEntry>(id, &Scope::symbol_table);
}

std::shared_ptr<SymbolEntry> ScopeStack::ProbeSymbol(
    const std::string& id) const {
  return ProbeEntry_<SymbolEntry>(id, &Scope::symbol_table);
}

std::shared_ptr<TypeEntry> ScopeStack::AddType(std::unique_ptr<TypeEntry> entry,
                                               ScopeKind kind) {
  return AddEntry_<TypeEntry>(std::move(entry), kind, &Scope::type_table);
}

std::shared_ptr<TypeEntry> ScopeStack::LookUpType(const std::string& id) const {
  return LookUpEntry_<TypeEntry>(id, &Scope::type_table);
}

std::shared_ptr<TypeEntry> ScopeStack::ProbeType(const std::string& id) const {
  return ProbeEntry_<TypeEntry>(id, &Scope::type_table);
}
