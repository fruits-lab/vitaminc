#ifndef SCOPE_HPP_
#define SCOPE_HPP_

#include <cstdint>
#include <memory>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

#include "symbol.hpp"

// 6.2.1 Scopes of identifiers
enum class ScopeKind : std::uint8_t {
  /// @note A label name is the only kind of identifier that has function scope.
  kFunc,
  /// @note Appears outside of any block or list of parameters.
  kFile,
  /// @note Appears inside a block or within the list of parameter declarations
  /// in a function definition.
  kBlock,
  /// @note Appears within the list of parameter declarations in a function
  /// prototype.
  kParam,
};

/// @brief A symbol table associated with a scope kind.
struct Scope {
  ScopeKind kind;
  std::unique_ptr<SymbolTable> table;

  Scope(ScopeKind kind, std::unique_ptr<SymbolTable> table)
      : kind{kind}, table{std::move(table)} {}
};

/// @brief Manages scopes and symbol tables.
class ScopeStack {
 public:
  /// @brief Pushes a new scope of the kind.
  /// @throws `ScopesOfDifferentKindIsNotMergeableError` if the previous scope
  /// had set to merge with the next (this) scope, which is of a different kind.
  void PushScope(ScopeKind kind) {
    if (should_merge_with_next_scope_) {
      if (scopes_.back().kind != kind) {
        throw ScopesOfDifferentKindIsNotMergeableError{""};
      }
      should_merge_with_next_scope_ = false;
      // No need to push a new scope.
      return;
    }
    scopes_.emplace_back(kind, std::make_unique<SymbolTable>());
  }

  /// @throws `NotInScopeError`
  void PopScope() {
    ThrowIfNotInScope_();
    scopes_.pop_back();
  }

  /// @brief Adds an entry to the top-most scope of the kind.
  /// @throws `NotInScopeError`
  /// @throws `NotInSuchKindOfScopeError`
  std::shared_ptr<SymbolEntry> Add(std::unique_ptr<SymbolEntry> entry,
                                   ScopeKind kind) {
    ThrowIfNotInScope_();
    for (auto it = scopes_.rbegin(); it != scopes_.rend(); ++it) {
      if (it->kind == kind) {
        return it->table->Add(std::move(entry));
      }
    }
    throw NotInSuchKindOfScopeError{""};
  }

  /// @brief Looks up the `id` from through all scopes.
  /// @throws `NotInScopeError`
  std::shared_ptr<SymbolEntry> LookUp(const std::string& id) const {
    ThrowIfNotInScope_();
    // Iterates backward since we're using the container as a stack.
    for (auto it = scopes_.crbegin(); it != scopes_.crend(); ++it) {
      if (auto entry = it->table->Probe(id)) {
        return entry;
      }
    }
    return nullptr;
  }

  /// @brief Probes the `id` from the top-most scope.
  /// @throws `NotInScopeError`
  std::shared_ptr<SymbolEntry> Probe(const std::string& id) const {
    ThrowIfNotInScope_();
    return scopes_.back().table->Probe(id);
  }

  /// @brief Merges the current scope with the next pushed scope.
  /// @throws `NotInScopeError` if currently not in any scope.
  void MergeWithNextScope() {
    ThrowIfNotInScope_();
    should_merge_with_next_scope_ = true;
  }

  using NotInSuchKindOfScopeError = std::runtime_error;
  using NotInScopeError = std::runtime_error;
  using ScopesOfDifferentKindIsNotMergeableError = std::runtime_error;

 private:
  std::vector<Scope> scopes_{};
  /// @brief If `true`, the current scope will be merged with the next scope.
  /// @note This is used specifically for the function parameters to be included
  /// in the scope of the function body.
  bool should_merge_with_next_scope_{false};

  /// @throws `NotInScopeError`
  void ThrowIfNotInScope_() const {
    if (scopes_.empty()) {
      throw NotInScopeError{""};
    }
  }
};

#endif  // SCOPE_HPP_
