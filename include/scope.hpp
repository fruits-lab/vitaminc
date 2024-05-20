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
  std::unique_ptr<SymbolTable> symbol_table;
  std::unique_ptr<DeclTypeTable> type_table;

  Scope(ScopeKind kind, std::unique_ptr<SymbolTable> symbol_table,
        std::unique_ptr<DeclTypeTable> type_table)
      : kind{kind},
        symbol_table{std::move(symbol_table)},
        type_table{std::move(type_table)} {}
};

/// @brief Manages scopes and symbol tables.
class ScopeStack {
 public:
  /// @brief Pushes a new scope of the kind.
  void PushScope(ScopeKind kind);
  /// @brief Pops the top scope of the stack.
  void PopScope();

  /// @brief Adds an entry to the top-most scope of the kind.
  template <typename Table>
  std::shared_ptr<typename Table::EntryType> AddEntry(
      std::unique_ptr<typename Table::EntryType> entry, ScopeKind kind,
      std::unique_ptr<Table> Scope::*table);

  /// @brief Looks up the `id` from through all scopes.
  template <typename Table>
  std::shared_ptr<typename Table::EntryType> LookUpEntry(
      const std::string& id, std::unique_ptr<Table> Scope::*table) const;

  /// @brief Probes the `id` from the top-most scope.
  template <typename Table>
  std::shared_ptr<typename Table::EntryType> ProbeEntry(
      const std::string& id, std::unique_ptr<Table> Scope::*table) const;

  /// @brief Merges the current scope with the next pushed scope.
  void MergeWithNextScope();

  using NotInSuchKindOfScopeError = std::runtime_error;
  using NotInScopeError = std::runtime_error;
  using ScopesOfDifferentKindIsNotMergeableError = std::runtime_error;

  std::shared_ptr<SymbolEntry> AddSymbol(std::unique_ptr<SymbolEntry> entry,
                                         ScopeKind kind);
  std::shared_ptr<SymbolEntry> LookUpSymbol(const std::string& id) const;
  std::shared_ptr<SymbolEntry> ProbeSymbol(const std::string& id) const;

  std::shared_ptr<DeclTypeEntry> AddType(std::unique_ptr<DeclTypeEntry> entry,
                                         ScopeKind kind);
  std::shared_ptr<DeclTypeEntry> LookUpType(const std::string& id) const;
  std::shared_ptr<DeclTypeEntry> ProbeType(const std::string& id) const;

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
