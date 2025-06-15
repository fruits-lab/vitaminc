#include "ast_dumper.hpp"

#include <iostream>
#include <memory>
#include <string>
#include <variant>

#include "ast.hpp"
#include "operator.hpp"
#include "type.hpp"

namespace {

std::string GetBinaryOperator(BinaryOperator op) {
  switch (op) {
    case BinaryOperator::kAdd:
      return "+";
    case BinaryOperator::kSub:
      return "-";
    case BinaryOperator::kMul:
      return "*";
    case BinaryOperator::kDiv:
      return "/";
    case BinaryOperator::kMod:
      return "%";
    case BinaryOperator::kGt:
      return ">";
    case BinaryOperator::kGte:
      return ">=";
    case BinaryOperator::kLt:
      return "<";
    case BinaryOperator::kLte:
      return "<=";
    case BinaryOperator::kEq:
      return "==";
    case BinaryOperator::kNeq:
      return "!=";
    case BinaryOperator::kAnd:
      return "&";
    case BinaryOperator::kXor:
      return "^";
    case BinaryOperator::kOr:
      return "|";
    case BinaryOperator::kShl:
      return "<<";
    case BinaryOperator::kShr:
      return ">>";
    case BinaryOperator::kLand:
      return "&&";
    case BinaryOperator::kLor:
      return "||";
    case BinaryOperator::kComma:
      return ",";
    default:
      return "Unknown";
  }
}

std::string GetUnaryOperator(UnaryOperator op) {
  switch (op) {
    case UnaryOperator::kIncr:
      return "++";
    case UnaryOperator::kDecr:
      return "--";
    case UnaryOperator::kPos:
      return "+";
    case UnaryOperator::kNeg:
      return "-";
    case UnaryOperator::kNot:
      return "!";
    case UnaryOperator::kAddr:
      return "&";
    case UnaryOperator::kDeref:
      return "*";
    case UnaryOperator::kBitComp:
      return "~";
    default:
      return "Unknown";
  }
}

std::string GetPostfixOperator(PostfixOperator op) {
  switch (op) {
    case PostfixOperator::kIncr:
      return "++";
    case PostfixOperator::kDecr:
      return "--";
    case PostfixOperator::kDot:
      return ".";
    case PostfixOperator::kArrow:
      return "->";
    default:
      return "Unknown";
  }
}

}  // namespace

void AstDumper::Visit(const DeclStmtNode& decl_stmt) {
  std::cout << indenter_.Indent() << "DeclStmtNode <" << decl_stmt.loc << ">\n";
  indenter_.IncreaseLevel();
  for (const auto& decl : decl_stmt.decls) {
    decl->Accept(*this);
  }
  indenter_.DecreaseLevel();
}

void AstDumper::Visit(const VarDeclNode& decl) {
  std::cout << indenter_.Indent() << "VarDeclNode <" << decl.loc << "> "
            << decl.id << ": " << decl.type->ToString() << '\n';

  if (decl.init) {
    indenter_.IncreaseLevel();
    decl.init->Accept(*this);
    indenter_.DecreaseLevel();
  }
}

void AstDumper::Visit(const ArrDeclNode& arr_decl) {
  std::cout << indenter_.Indent() << "ArrDeclNode <" << arr_decl.loc << "> "
            << arr_decl.id << ": " << arr_decl.type->ToString() << '\n';

  indenter_.IncreaseLevel();
  for (const auto& arr_init : arr_decl.init_list) {
    arr_init->Accept(*this);
  }
  indenter_.DecreaseLevel();
}

void AstDumper::Visit(const RecordDeclNode& record_decl) {
  std::cout << indenter_.Indent() << "RecordDeclNode <" << record_decl.loc
            << "> " << record_decl.type->ToString() << " definition\n";

  indenter_.IncreaseLevel();
  for (const auto& field : record_decl.fields) {
    field->Accept(*this);
  }
  indenter_.DecreaseLevel();
}

void AstDumper::Visit(const RecordVarDeclNode& record_decl) {
  std::cout << indenter_.Indent() << "RecordVarDeclNode <" << record_decl.loc
            << "> " << record_decl.id << ": " << record_decl.type->ToString()
            << '\n';

  indenter_.IncreaseLevel();
  for (const auto& init : record_decl.inits) {
    init->Accept(*this);
  }
  indenter_.DecreaseLevel();
}

void AstDumper::Visit(const FieldNode& field) {
  std::cout << indenter_.Indent() << "FieldNode <" << field.loc << "> "
            << field.id << ": " << field.type->ToString() << '\n';
}

void AstDumper::Visit(const EnumDeclNode& enum_decl) {
  std::cout << indenter_.Indent() << "EnumDeclNode <" << enum_decl.loc << "> "
            << enum_decl.id << '\n';

  indenter_.IncreaseLevel();
  for (const auto& enum_const_decl : enum_decl.enum_consts) {
    enum_const_decl->Accept(*this);
  }
  indenter_.DecreaseLevel();
}

void AstDumper::Visit(const EnumConstDeclNode& enum_const_decl) {
  std::cout << indenter_.Indent() << "EnumConstDeclNode <"
            << enum_const_decl.loc << "> " << enum_const_decl.id << ": "
            << enum_const_decl.type->ToString() << '\n';
  if (enum_const_decl.int_const) {
    indenter_.IncreaseLevel();
    enum_const_decl.int_const->Accept(*this);
    indenter_.DecreaseLevel();
  }
}

void AstDumper::Visit(const ParamNode& parameter) {
  std::cout << indenter_.Indent() << "ParamNode <" << parameter.loc << "> "
            << parameter.id << ": " << parameter.type->ToString() << '\n';
}

void AstDumper::Visit(const FuncDefNode& func_def) {
  std::cout << indenter_.Indent() << "FuncDefNode <" << func_def.loc << "> "
            << func_def.id << ": " << func_def.type->ToString() << '\n';

  indenter_.IncreaseLevel();
  for (const auto& parameter : func_def.parameters) {
    parameter->Accept(*this);
  }
  func_def.body->Accept(*this);
  indenter_.DecreaseLevel();
}

void AstDumper::Visit(const LoopInitNode& loop_init) {
  std::cout << indenter_.Indent() << "LoopInitNode <" << loop_init.loc << ">\n";
  indenter_.IncreaseLevel();
  std::visit([this](auto&& clause) { clause->Accept(*this); },
             loop_init.clause);
  indenter_.DecreaseLevel();
}

void AstDumper::Visit(const CompoundStmtNode& compound_stmt) {
  std::cout << indenter_.Indent() << "CompoundStmtNode <" << compound_stmt.loc
            << ">\n";
  indenter_.IncreaseLevel();
  for (const auto& stmt : compound_stmt.stmts) {
    stmt->Accept(*this);
  }
  indenter_.DecreaseLevel();
}

void AstDumper::Visit(const ExternDeclNode& extern_decl) {
  std::cout << indenter_.Indent() << "ExternDeclNode <" << extern_decl.loc
            << ">\n";
  indenter_.IncreaseLevel();
  std::visit([this](auto&& extern_decl) { extern_decl->Accept(*this); },
             extern_decl.decl);
  indenter_.DecreaseLevel();
}

void AstDumper::Visit(const TransUnitNode& trans_unit) {
  std::cout << indenter_.Indent() << "TransUnitNode <" << trans_unit.loc
            << ">\n";
  indenter_.IncreaseLevel();
  for (const auto& extern_decl : trans_unit.extern_decls) {
    extern_decl->Accept(*this);
  }
  indenter_.DecreaseLevel();
}

void AstDumper::Visit(const IfStmtNode& if_stmt) {
  std::cout << indenter_.Indent() << "IfStmtNode <" << if_stmt.loc << ">\n";
  indenter_.IncreaseLevel();
  if_stmt.predicate->Accept(*this);
  std::cout << indenter_.Indent() << "// Then\n";
  if_stmt.then->Accept(*this);
  indenter_.DecreaseLevel();
  if (if_stmt.or_else) {
    indenter_.IncreaseLevel();
    std::cout << indenter_.Indent() << "// Else\n";
    if_stmt.or_else->Accept(*this);
    indenter_.DecreaseLevel();
  }
}

void AstDumper::Visit(const WhileStmtNode& while_stmt) {
  std::cout << indenter_.Indent() << "WhileStmtNode <" << while_stmt.loc
            << ">\n";
  if (while_stmt.is_do_while) {
    indenter_.IncreaseLevel();
    std::cout << indenter_.Indent() << "// Do\n";
    while_stmt.loop_body->Accept(*this);
    indenter_.DecreaseLevel();
  }
  indenter_.IncreaseLevel();
  std::cout << indenter_.Indent() << "// While\n";
  while_stmt.predicate->Accept(*this);
  if (!while_stmt.is_do_while) {
    std::cout << indenter_.Indent() << "// Body\n";
    while_stmt.loop_body->Accept(*this);
  }
  indenter_.DecreaseLevel();
}

void AstDumper::Visit(const ForStmtNode& for_stmt) {
  std::cout << indenter_.Indent() << "ForStmtNode <" << for_stmt.loc << ">\n";
  indenter_.IncreaseLevel();
  for_stmt.loop_init->Accept(*this);
  for_stmt.predicate->Accept(*this);
  for_stmt.step->Accept(*this);
  for_stmt.loop_body->Accept(*this);
  indenter_.DecreaseLevel();
}

void AstDumper::Visit(const ReturnStmtNode& ret_stmt) {
  std::cout << indenter_.Indent() << "ReturnStmtNode <" << ret_stmt.loc
            << ">\n";
  indenter_.IncreaseLevel();
  ret_stmt.expr->Accept(*this);
  indenter_.DecreaseLevel();
}

void AstDumper::Visit(const GotoStmtNode& goto_stmt) {
  std::cout << indenter_.Indent() << "GotoStmtNode <" << goto_stmt.loc << "> "
            << goto_stmt.label << '\n';
}

void AstDumper::Visit(const BreakStmtNode& break_stmt) {
  std::cout << indenter_.Indent() << "BreakStmtNode <" << break_stmt.loc
            << ">\n";
}

void AstDumper::Visit(const ContinueStmtNode& continue_stmt) {
  std::cout << indenter_.Indent() << "ContinueStmtNode <" << continue_stmt.loc
            << ">\n";
}

void AstDumper::Visit(const SwitchStmtNode& switch_stmt) {
  std::cout << indenter_.Indent() << "SwitchStmtNode <" << switch_stmt.loc
            << ">\n";
  indenter_.IncreaseLevel();
  switch_stmt.ctrl->Accept(*this);
  switch_stmt.stmt->Accept(*this);
  indenter_.DecreaseLevel();
}

void AstDumper::Visit(const IdLabeledStmtNode& id_labeled_stmt) {
  std::cout << indenter_.Indent() << "IdLabeledStmtNode <"
            << id_labeled_stmt.loc << "> " << id_labeled_stmt.label << '\n';
  indenter_.IncreaseLevel();
  id_labeled_stmt.stmt->Accept(*this);
  indenter_.DecreaseLevel();
}

void AstDumper::Visit(const CaseStmtNode& case_stmt) {
  std::cout << indenter_.Indent() << "CaseStmtNode <" << case_stmt.loc << ">\n";
  indenter_.IncreaseLevel();
  case_stmt.expr->Accept(*this);
  case_stmt.stmt->Accept(*this);
  indenter_.DecreaseLevel();
}

void AstDumper::Visit(const DefaultStmtNode& default_stmt) {
  std::cout << indenter_.Indent() << "DefaultStmtNode <" << default_stmt.loc
            << ">\n";
  indenter_.IncreaseLevel();
  default_stmt.stmt->Accept(*this);
  indenter_.DecreaseLevel();
}

void AstDumper::Visit(const ExprStmtNode& expr_stmt) {
  std::cout << indenter_.Indent() << "ExprStmtNode <" << expr_stmt.loc << ">\n";
  indenter_.IncreaseLevel();
  expr_stmt.expr->Accept(*this);
  indenter_.DecreaseLevel();
}

void AstDumper::Visit(const InitExprNode& init_expr) {
  std::cout << indenter_.Indent() << "InitExprNode <" << init_expr.loc << "> "
            << init_expr.type->ToString() << "\n";
  indenter_.IncreaseLevel();
  for (const auto& des : init_expr.des) {
    des->Accept(*this);
  }
  init_expr.expr->Accept(*this);
  indenter_.DecreaseLevel();
}

void AstDumper::Visit(const ArrDesNode& arr_des) {
  std::cout << indenter_.Indent() << "ArrDesNode <" << arr_des.loc << ">\n";
  indenter_.IncreaseLevel();
  arr_des.index->Accept(*this);
  indenter_.DecreaseLevel();
}

void AstDumper::Visit(const IdDesNode& id_des) {
  std::cout << indenter_.Indent() << "IdDesNode <" << id_des.loc << "> "
            << id_des.id << "\n";
}

void AstDumper::Visit(const NullExprNode& null_expr) {
  std::cout << indenter_.Indent() << "NullStmtNode <" << null_expr.loc << ">\n";
}

void AstDumper::Visit(const IdExprNode& id_expr) {
  std::cout << indenter_.Indent() << "IdExprNode <" << id_expr.loc << "> "
            << id_expr.id << ": " << id_expr.type->ToString() << '\n';
  if (id_expr.const_expr) {
    indenter_.IncreaseLevel();
    id_expr.const_expr->Accept(*this);
    indenter_.DecreaseLevel();
  }
}

void AstDumper::Visit(const IntConstExprNode& int_expr) {
  std::cout << indenter_.Indent() << "IntConstExprNode <" << int_expr.loc
            << "> " << int_expr.val << ": " << int_expr.type->ToString()
            << '\n';
}

void AstDumper::Visit(const ArgExprNode& arg_expr) {
  std::cout << indenter_.Indent() << "ArgExprNode <" << arg_expr.loc << "> "
            << arg_expr.type->ToString() << '\n';
  indenter_.IncreaseLevel();
  arg_expr.arg->Accept(*this);
  indenter_.DecreaseLevel();
}

void AstDumper::Visit(const ArrSubExprNode& arr_sub_expr) {
  std::cout << indenter_.Indent() << "ArrSubExprNode <" << arr_sub_expr.loc
            << "> " << arr_sub_expr.type->ToString() << '\n';
  indenter_.IncreaseLevel();
  arr_sub_expr.arr->Accept(*this);
  arr_sub_expr.index->Accept(*this);
  indenter_.DecreaseLevel();
}

void AstDumper::Visit(const CondExprNode& cond_expr) {
  std::cout << indenter_.Indent() << "CondExprNode <" << cond_expr.loc << "> "
            << cond_expr.type->ToString() << '\n';
  indenter_.IncreaseLevel();
  cond_expr.predicate->Accept(*this);
  cond_expr.then->Accept(*this);
  cond_expr.or_else->Accept(*this);
  indenter_.DecreaseLevel();
}

void AstDumper::Visit(const FuncCallExprNode& call_expr) {
  std::cout << indenter_.Indent() << "FuncCallExprNode <" << call_expr.loc
            << "> " << call_expr.type->ToString() << '\n';
  indenter_.IncreaseLevel();
  call_expr.func_expr->Accept(*this);
  for (const auto& arg : call_expr.args) {
    arg->Accept(*this);
  }
  indenter_.DecreaseLevel();
}

void AstDumper::Visit(const PostfixArithExprNode& postfix_expr) {
  std::cout << indenter_.Indent() << "PostfixArithExprNode <"
            << postfix_expr.loc << "> " << postfix_expr.type->ToString() << " "
            << GetPostfixOperator(postfix_expr.op) << '\n';
  indenter_.IncreaseLevel();
  postfix_expr.operand->Accept(*this);
  indenter_.DecreaseLevel();
}

void AstDumper::Visit(const RecordMemExprNode& mem_expr) {
  std::cout << indenter_.Indent() << "RecordMemExprNode <" << mem_expr.loc
            << "> " << GetPostfixOperator(mem_expr.op) << mem_expr.id << ": "
            << mem_expr.type->ToString() << '\n';
  indenter_.IncreaseLevel();
  mem_expr.expr->Accept(*this);
  indenter_.DecreaseLevel();
}

void AstDumper::Visit(const UnaryExprNode& unary_expr) {
  std::cout << indenter_.Indent() << "UnaryExprNode <" << unary_expr.loc << "> "
            << unary_expr.type->ToString() << " "
            << GetUnaryOperator(unary_expr.op) << '\n';
  indenter_.IncreaseLevel();
  unary_expr.operand->Accept(*this);
  indenter_.DecreaseLevel();
}

void AstDumper::Visit(const BinaryExprNode& bin_expr) {
  std::cout << indenter_.Indent() << "BinaryExprNode <" << bin_expr.loc << "> "
            << bin_expr.type->ToString() << " "
            << GetBinaryOperator(bin_expr.op) << '\n';
  indenter_.IncreaseLevel();
  bin_expr.lhs->Accept(*this);
  bin_expr.rhs->Accept(*this);
  indenter_.DecreaseLevel();
}

void AstDumper::Visit(const SimpleAssignmentExprNode& assign_expr) {
  std::cout << indenter_.Indent() << "SimpleAssignmentExprNode <"
            << assign_expr.loc << "> " << assign_expr.type->ToString() << '\n';
  indenter_.IncreaseLevel();
  assign_expr.lhs->Accept(*this);
  assign_expr.rhs->Accept(*this);
  indenter_.DecreaseLevel();
}
