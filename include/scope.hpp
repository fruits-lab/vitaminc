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

/// @brief Has a symbol table and a type table associating with the scope kind.
struct Scope {
  ScopeKind kind;
  std::unique_ptr<SymbolTable> symbol_table;
  std::unique_ptr<TypeTable> type_table;

  Scope(ScopeKind kind, std::unique_ptr<SymbolTable> symbol_table,
        std::unique_ptr<TypeTable> type_table)
      : kind{kind},
        symbol_table{std::move(symbol_table)},
        type_table{std::move(type_table)} {}
};

/// @brief Manages scopes and symbol tables.
class ScopeStack {
 public:
  /// @brief Pushes a new scope of the kind.
  /// @throws `ScopesOfDifferentKindIsNotMergeableError` if the previous scope
  /// had set to merge with the next (this) scope, which is of a different kind.
  void PushScope(ScopeKind kind);
  /// @brief Pops the top scope of the stack.
  /// @throws `NotInScopeError`
  void PopScope();
  /// @return Current scope kind.
  /// @throws `NotInScopeError`
  ScopeKind CurrentScopeKind();

  /// @brief Merges the current scope with the next pushed scope.
  /// @throws `NotInScopeError` if currently not in any scope.
  void MergeWithNextScope();

  using NotInSuchKindOfScopeError = std::runtime_error;
  using NotInScopeError = std::runtime_error;
  using ScopesOfDifferentKindIsNotMergeableError = std::runtime_error;

  //
  // Provides two sets of functions for symbols and types respectively.
  //

  /// @brief Adds the `entry` to the top-most scope of the `kind`.
  /// @return The added entry if the `id` of the `entry` isn't already in such
  /// scope; otherwise, the original entry.
  /// @throws `NotInScopeError`
  /// @throws `NotInSuchKindOfScopeError`
  std::shared_ptr<SymbolEntry> AddSymbol(std::unique_ptr<SymbolEntry> entry,
                                         ScopeKind kind);
  /// @brief Looks up the symbol with the `id` from through all scopes.
  /// @return The symbol with the `id` if it exists; otherwise, `nullptr`.
  /// @throws `NotInScopeError`
  std::shared_ptr<SymbolEntry> LookUpSymbol(const std::string& id) const;
  /// @brief Probes the symbol with the `id` from the top-most scope.
  /// @return The symbol with the `id` if it exists; otherwise, `nullptr`.
  /// @throws `NotInScopeError`
  std::shared_ptr<SymbolEntry> ProbeSymbol(const std::string& id) const;

  /// @brief Adds the `entry` to the top-most scope of the `kind`.
  /// @return The added entry if the `id` of the `entry` isn't already in such
  /// scope; otherwise, the original entry.
  /// @throws `NotInScopeError`
  /// @throws `NotInSuchKindOfScopeError`
  std::shared_ptr<TypeEntry> AddType(std::unique_ptr<TypeEntry> entry,
                                     ScopeKind kind);
  /// @brief Looks up the type with the `id` from through all scopes.
  /// @return The type with the `id` if it exists; otherwise, `nullptr`.
  /// @throws `NotInScopeError`
  std::shared_ptr<TypeEntry> LookUpType(const std::string& id) const;
  /// @brief Probes the type with the `id` from the top-most scope.
  /// @return The type with the `id` if it exists; otherwise, `nullptr`.
  /// @throws `NotInScopeError`
  std::shared_ptr<TypeEntry> ProbeType(const std::string& id) const;

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

  /// @brief Adds the `entry` to the top-most scope of the `kind`.
  /// @tparam Table The type of the table to add the entry.
  /// @tparam Entry The type of the entry to add.
  /// @param kind The kind of the scope to add the entry.
  /// @param table The class member pointer to the table to add the entry.
  /// @return The added entry if the `id` of the `entry` isn't already in such
  /// scope; otherwise, the original entry.
  template <typename Entry>
  std::shared_ptr<Entry> AddEntry_(
      std::unique_ptr<Entry> entry, ScopeKind kind,
      std::unique_ptr<TableTemplate<Entry>> Scope::*table);

  /// @brief Looks up the `id` from through all scopes.
  /// @tparam Table The type of the table to look up from the scope.
  /// @tparam Entry The type of the entry to look up.
  /// @param table The class member pointer to the table to look up from the
  /// scope.
  /// @return The entry with the `id` if it exists; otherwise, `nullptr`.
  /// @throws `NotInScopeError`
  template <typename Entry>
  std::shared_ptr<Entry> LookUpEntry_(
      const std::string& id,
      std::unique_ptr<TableTemplate<Entry>> Scope::*table) const;

  /// @brief Probes the `id` from the top-most scope.
  /// @tparam Table The type of the table to probe from the scope.
  /// @tparam Entry The type of the entry to probe.
  /// @param table The class member pointer to the table to probe from the
  /// scope.
  /// @return The entry with the `id` if it exists; otherwise, `nullptr`.
  /// @throws `NotInScopeError`
  template <typename Entry>
  std::shared_ptr<Entry> ProbeEntry_(
      const std::string& id,
      std::unique_ptr<TableTemplate<Entry>> Scope::*table) const;
};

#endif  // SCOPE_HPP_
