#include "ast.hpp"

#include <string>

#include "visitor.hpp"

void AstNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void AstNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void StmtNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void StmtNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void ExprNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void ExprNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

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

void PlusExprNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void PlusExprNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

std::string PlusExprNode::OpName_() const {
  return "add";
}

void SubExprNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void SubExprNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

std::string SubExprNode::OpName_() const {
  return "sub";
}

void MulExprNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void MulExprNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

std::string MulExprNode::OpName_() const {
  return "mul";
}

void DivExprNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void DivExprNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

std::string DivExprNode::OpName_() const {
  return "div";
}

void ModExprNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void ModExprNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

std::string ModExprNode::OpName_() const {
  return "rem";
}

void GreaterThanExprNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void GreaterThanExprNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

std::string GreaterThanExprNode::OpName_() const {
  // signed
  return "sgt";
}

void GreaterThanOrEqualToExprNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void GreaterThanOrEqualToExprNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

std::string GreaterThanOrEqualToExprNode::OpName_() const {
  // signed
  return "sge";
}

void LessThanExprNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void LessThanExprNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

std::string LessThanExprNode::OpName_() const {
  // signed
  return "slt";
}

void LessThanOrEqualToExprNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void LessThanOrEqualToExprNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

std::string LessThanOrEqualToExprNode::OpName_() const {
  // signed
  return "sle";
}

void EqualToExprNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void EqualToExprNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

std::string EqualToExprNode::OpName_() const {
  return "eq";
}

void NotEqualToExprNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void NotEqualToExprNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

std::string NotEqualToExprNode::OpName_() const {
  return "ne";
}

void AssignmentExprNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void AssignmentExprNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}

void SimpleAssignmentExprNode::Accept(NonModifyingVisitor& v) const {
  v.Visit(*this);
}

void SimpleAssignmentExprNode::Accept(ModifyingVisitor& v) {
  v.Visit(*this);
}
