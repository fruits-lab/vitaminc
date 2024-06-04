#include "ast.hpp"

#include "visitor.hpp"

void AstNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void AstNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

AstNode::~AstNode() = default;

void StmtNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void StmtNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

StmtNode::~StmtNode() = default;

void ExprNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void ExprNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

ExprNode::~ExprNode() = default;

void DeclNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void DeclNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

DeclNode::~DeclNode() = default;

void DesNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void DesNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

DesNode::~DesNode() = default;

void DeclStmtNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void DeclStmtNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void VarDeclNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void VarDeclNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void ArrDeclNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void ArrDeclNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void RecordDeclNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void RecordDeclNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void FieldNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void FieldNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void RecordVarDeclNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void RecordVarDeclNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void ParamNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void ParamNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void FuncDefNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void FuncDefNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void LoopInitNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void LoopInitNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void CompoundStmtNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void CompoundStmtNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void ProgramNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void ProgramNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void IfStmtNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void IfStmtNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void WhileStmtNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void WhileStmtNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void ForStmtNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void ForStmtNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void ReturnStmtNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void ReturnStmtNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void GotoStmtNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void GotoStmtNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void BreakStmtNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void BreakStmtNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void ContinueStmtNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void ContinueStmtNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void SwitchStmtNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void SwitchStmtNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void LabeledStmtNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void LabeledStmtNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

LabeledStmtNode::~LabeledStmtNode() = default;

void IdLabeledStmtNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void IdLabeledStmtNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void CaseStmtNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void CaseStmtNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void DefaultStmtNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void DefaultStmtNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void ExprStmtNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void ExprStmtNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void InitExprNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void InitExprNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void ArrDesNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void ArrDesNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void IdDesNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void IdDesNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void NullExprNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void NullExprNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void IdExprNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void IdExprNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void IntConstExprNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void IntConstExprNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void ArgExprNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void ArgExprNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void ArrSubExprNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void ArrSubExprNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void CondExprNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void CondExprNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void FuncCallExprNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void FuncCallExprNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void PostfixArithExprNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void PostfixArithExprNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void RecordMemExprNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void RecordMemExprNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void UnaryExprNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void UnaryExprNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void BinaryExprNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void BinaryExprNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void AssignmentExprNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void AssignmentExprNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

AssignmentExprNode::~AssignmentExprNode() = default;

void SimpleAssignmentExprNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void SimpleAssignmentExprNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}
