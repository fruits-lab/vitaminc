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

template <typename Table, typename Entry>
std::shared_ptr<Entry> ScopeStack::AddEntry_(
    std::unique_ptr<Entry> entry, ScopeKind kind,
    std::unique_ptr<Table> Scope::*table) {
  ThrowIfNotInScope_();
  for (auto it = scopes_.rbegin(); it != scopes_.rend(); ++it) {
    if (it->kind == kind) {
      return ((*it).*table)->Add(std::move(entry));
    }
  }
  throw NotInSuchKindOfScopeError{""};
}

template <typename Table, typename Entry>
std::shared_ptr<Entry> ScopeStack::LookUpEntry_(
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

template <typename Table, typename Entry>
std::shared_ptr<Entry> ScopeStack::ProbeEntry_(
    const std::string& id, std::unique_ptr<Table> Scope::*table) const {
  ThrowIfNotInScope_();
  return (scopes_.back().*table)->Probe(id);
}

std::shared_ptr<SymbolEntry> ScopeStack::AddSymbol(
    std::unique_ptr<SymbolEntry> entry, ScopeKind kind) {
  return AddEntry_<SymbolTable, SymbolEntry>(std::move(entry), kind,
                                             &Scope::symbol_table);
}

std::shared_ptr<SymbolEntry> ScopeStack::LookUpSymbol(
    const std::string& id) const {
  return LookUpEntry_<SymbolTable, SymbolEntry>(id, &Scope::symbol_table);
}

std::shared_ptr<SymbolEntry> ScopeStack::ProbeSymbol(
    const std::string& id) const {
  return ProbeEntry_<SymbolTable, SymbolEntry>(id, &Scope::symbol_table);
}

std::shared_ptr<TypeEntry> ScopeStack::AddType(std::unique_ptr<TypeEntry> entry,
                                               ScopeKind kind) {
  return AddEntry_<TypeTable, TypeEntry>(std::move(entry), kind,
                                         &Scope::type_table);
}

std::shared_ptr<TypeEntry> ScopeStack::LookUpType(const std::string& id) const {
  return LookUpEntry_<TypeTable, TypeEntry>(id, &Scope::type_table);
}

std::shared_ptr<TypeEntry> ScopeStack::ProbeType(const std::string& id) const {
  return ProbeEntry_<TypeTable, TypeEntry>(id, &Scope::type_table);
}

// Explicit template instantiation for the member functions used by ScopeStack
template std::shared_ptr<SymbolEntry> ScopeStack::AddEntry_<
    SymbolTable, SymbolEntry>(std::unique_ptr<SymbolEntry>, ScopeKind,
                              std::unique_ptr<SymbolTable> Scope::*);

template std::shared_ptr<TypeEntry> ScopeStack::AddEntry_<TypeTable, TypeEntry>(
    std::unique_ptr<TypeEntry>, ScopeKind, std::unique_ptr<TypeTable> Scope::*);

template std::shared_ptr<SymbolEntry>
ScopeStack::LookUpEntry_<SymbolTable, SymbolEntry>(
    const std::string&, std::unique_ptr<SymbolTable> Scope::*) const;

template std::shared_ptr<TypeEntry>
ScopeStack::LookUpEntry_<TypeTable, TypeEntry>(
    const std::string&, std::unique_ptr<TypeTable> Scope::*) const;

template std::shared_ptr<SymbolEntry>
ScopeStack::ProbeEntry_<SymbolTable, SymbolEntry>(
    const std::string&, std::unique_ptr<SymbolTable> Scope::*) const;

template std::shared_ptr<TypeEntry>
ScopeStack::ProbeEntry_<TypeTable, TypeEntry>(
    const std::string&, std::unique_ptr<TypeTable> Scope::*) const;
