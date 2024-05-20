#include "scope.hpp"

#include <cstdint>
#include <memory>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

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
  scopes_.emplace_back(kind, std::make_unique<SymbolTable>());
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
std::shared_ptr<SymbolEntry> ScopeStack::Add(std::unique_ptr<SymbolEntry> entry,
                                             ScopeKind kind) {
  ThrowIfNotInScope_();
  for (auto it = scopes_.rbegin(); it != scopes_.rend(); ++it) {
    if (it->kind == kind) {
      return it->table->Add(std::move(entry));
    }
  }
  throw NotInSuchKindOfScopeError{""};
}

/// @throws `NotInScopeError`
std::shared_ptr<SymbolEntry> ScopeStack::LookUp(const std::string& id) const {
  ThrowIfNotInScope_();
  // Iterates backward since we're using the container as a stack.
  for (auto it = scopes_.crbegin(); it != scopes_.crend(); ++it) {
    if (auto entry = it->table->Probe(id)) {
      return entry;
    }
  }
  return nullptr;
}

/// @throws `NotInScopeError`
std::shared_ptr<SymbolEntry> ScopeStack::Probe(const std::string& id) const {
  ThrowIfNotInScope_();
  return scopes_.back().table->Probe(id);
}