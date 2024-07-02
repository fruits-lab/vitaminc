#ifndef LLVM_IR_GENERATOR_HPP_
#define LLVM_IR_GENERATOR_HPP_

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/Support/raw_os_ostream.h>

#include <memory>
#include <ostream>
#include <string>

#include "ast.hpp"
#include "llvm/util.hpp"
#include "visitor.hpp"

class LLVMIRGenerator : public NonModifyingVisitor {
 public:
  void Visit(const DeclStmtNode&) override;
  void Visit(const LoopInitNode&) override;
  void Visit(const VarDeclNode&) override;
  void Visit(const ArrDeclNode&) override;
  void Visit(const RecordDeclNode&) override;
  void Visit(const FieldNode&) override;
  void Visit(const RecordVarDeclNode&) override;
  void Visit(const ParamNode&) override;
  void Visit(const FuncDefNode&) override;
  void Visit(const CompoundStmtNode&) override;
  void Visit(const TransUnitNode&) override;
  void Visit(const ExternDeclNode&) override;
  void Visit(const IfStmtNode&) override;
  void Visit(const WhileStmtNode&) override;
  void Visit(const ForStmtNode&) override;
  void Visit(const ReturnStmtNode&) override;
  void Visit(const GotoStmtNode&) override;
  void Visit(const BreakStmtNode&) override;
  void Visit(const ContinueStmtNode&) override;
  void Visit(const SwitchStmtNode&) override;
  void Visit(const IdLabeledStmtNode&) override;
  void Visit(const CaseStmtNode&) override;
  void Visit(const DefaultStmtNode&) override;
  void Visit(const ExprStmtNode&) override;
  void Visit(const InitExprNode&) override;
  void Visit(const ArrDesNode&) override;
  void Visit(const IdDesNode&) override;
  void Visit(const NullExprNode&) override;
  void Visit(const IdExprNode&) override;
  void Visit(const IntConstExprNode&) override;
  void Visit(const ArgExprNode&) override;
  void Visit(const ArrSubExprNode&) override;
  void Visit(const CondExprNode&) override;
  void Visit(const FuncCallExprNode&) override;
  void Visit(const RecordMemExprNode&) override;
  void Visit(const PostfixArithExprNode&) override;
  void Visit(const UnaryExprNode&) override;
  void Visit(const BinaryExprNode&) override;
  void Visit(const SimpleAssignmentExprNode&) override;

  LLVMIRGenerator(std::ostream& output, std::string& filename)
      : output_{output},
        context_{std::make_unique<llvm::LLVMContext>()},
        builder_{std::make_unique<llvm::IRBuilder<>>(*context_)},
        module_{std::make_unique<llvm::Module>(filename, *context_)},
        llvm_util_{util::LLVMIRUtil(builder_)} {}

  /// @brief Print LLVM IR to output_.
  void PrintIR() {
    module_->print(output_, nullptr);
  }

 private:
  /// @brief A LLVM ostream wrapper for writing to output.
  llvm::raw_os_ostream output_;
  /// @brief A LLVM object that includes core LLVM infrastructure.
  std::unique_ptr<llvm::LLVMContext> context_;
  /// @brief Provides LLVM Builder API for constructing IR. By default, Constant
  /// folding is enabled and we have more flexibility for inserting
  /// instructions.
  std::unique_ptr<llvm::IRBuilder<>> builder_;
  /// @brief Stores global variables, function lists, and the constructed IR.
  std::unique_ptr<llvm::Module> module_;
  /// @brief Handy LLVM types and functions for code generation.
  util::LLVMIRUtil llvm_util_;
};

#endif  // LLVM_IR_GENERATOR_HPP_
