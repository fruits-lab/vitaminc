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

void BlockStmtNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void BlockStmtNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void ProgramNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void ProgramNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void NullStmtNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void NullStmtNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void ReturnStmtNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void ReturnStmtNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void ExprStmtNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void ExprStmtNode::Accept(ModifyingVisitor& v) {
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

void BinaryExprNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void BinaryExprNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

BinaryExprNode::~BinaryExprNode() = default;

void PlusExprNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void PlusExprNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void SubExprNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void SubExprNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void MulExprNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void MulExprNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void DivExprNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void DivExprNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void ModExprNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void ModExprNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void GreaterThanExprNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void GreaterThanExprNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void GreaterThanOrEqualToExprNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void GreaterThanOrEqualToExprNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void LessThanExprNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void LessThanExprNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void LessThanOrEqualToExprNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void LessThanOrEqualToExprNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void EqualToExprNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void EqualToExprNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void NotEqualToExprNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void NotEqualToExprNode::Accept(ModifyingVisitor& v) {
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
