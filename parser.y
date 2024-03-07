%{

#include <iostream>
#include <memory>
#include <utility>
#include <vector>

#include "operator.hpp"
#include "ast.hpp"
#include "type.hpp"

%}

// Dependency code required for the value and location types;
// inserts verbatim to the header file.
%code requires {
  #include <memory>
  #include <string>
  #include <vector>

  #include "ast.hpp"
}

// Placed after the usual contents of the parser header file.
%code {
  extern yy::parser::symbol_type yylex();
}

%skeleton "lalr1.cc"
%require "3.2"
%language "c++"

%parse-param {std::unique_ptr<AstNode>& program}

// Use complete symbols (parser::symbol_type).
%define api.token.constructor
// Allow non-pointer-based rich types.
%define api.value.type variant
// Check whether symbols are constructed and destructed using RTTI.
%define parse.assert
// Copy may be expensive when using rich types, such as std::vector.
// Also with automove, smart pointers can be moved implicity without boilerplate std::move.
// NOTE: can no longer reference a $x twice since it's moved in the first place.
%define api.value.automove
// This guarantees that headers do not conflict when included together.
%define api.token.prefix {TOK_}
// Have messages report the unexpected token, and possibly the expected ones.
// Without this, the error message is always only "syntax error".
%define parse.error verbose
// Improve syntax error handling, as LALR parser might perform additional
// parser stack reductions before discovering the syntax error.
%define parse.lac full

%token <int> NUM
%token <std::string> ID
%token INT
%token IF ELSE
%token SWITCH CASE DEFAULT
%token EQ NE LE GE
%token DO WHILE FOR
%token CONTINUE BREAK RETURN
// increment (INCR: ++) and decrement (DECR: --)
%token INCR DECR
%token EOF 0

%nterm <ExprType> type_specifier
%nterm <std::unique_ptr<ExprNode>> expr
%nterm <std::unique_ptr<ExprNode>> expr_opt
%nterm <std::unique_ptr<ExprNode>> unary_expr
%nterm <std::unique_ptr<ExprNode>> postfix_expr
%nterm <std::unique_ptr<ExprNode>> primary_expr
%nterm <std::unique_ptr<DeclNode>> decl
%nterm <std::unique_ptr<ArgExprNode>> arg
%nterm <std::vector<std::unique_ptr<ArgExprNode>>> arg_list_opt arg_list
%nterm <std::unique_ptr<FuncDefNode>> func_def
%nterm <std::unique_ptr<ParamNode>> parameter
%nterm <std::vector<std::unique_ptr<ParamNode>>> parameter_list_opt parameter_list
%nterm <std::vector<std::unique_ptr<FuncDefNode>>> func_def_list_opt
%nterm <std::unique_ptr<LoopInitNode>> loop_init
%nterm <std::unique_ptr<StmtNode>> stmt jump_stmt selection_stmt labeled_stmt
%nterm <std::unique_ptr<CompoundStmtNode>> compound_stmt
%nterm <std::vector<CompoundStmtNode::Item>> block_item_list block_item_list_opt
%nterm <CompoundStmtNode::Item> block_item

%precedence '='
%left EQ NE
%left '<' '>' LE GE
%left '+' '-'
%left '*' '/' '%'

// Resolve the ambiguity in the "dangling-else" grammar.
// Example: IF '(' expr ')' IF '(' expr ')' stmt • ELSE stmt
// Yacc has two options to make, either shift or reduce:
// Shift derivation
//   stmt
//   ↳ 13: IF '(' expr ')' stmt
//                         ↳ 14: IF '(' expr ')' stmt • ELSE stmt
// Reduce derivation
//   stmt
//   ↳ 14: IF '(' expr ')' stmt                         ELSE stmt
//                         ↳ 13: IF '(' expr ')' stmt •
//
// Our goal is to find the closest IF for ELSE, so we tell Yacc to shift.
// Since the token "ELSE" has a higher precedence than the production rule
// "if without else", Yacc shifts to "ELSE" instead of reducing with the rule.
%precedence IF_WITHOUT_ELSE
%precedence ELSE

%start entry

%%
// TODO: support global variables
entry: func_def_list_opt {
    program = std::make_unique<ProgramNode>($1);
  }
  ;

func_def_list_opt: func_def_list_opt func_def {
    auto func_def_list_opt = $1;
    func_def_list_opt.push_back($2);
    $$ = std::move(func_def_list_opt);
  }
  | epsilon { $$ = std::vector<std::unique_ptr<FuncDefNode>>{}; }
  ;

func_def: type_specifier ID '(' parameter_list_opt ')' compound_stmt {
    $$ = std::make_unique<FuncDefNode>($2, $4, $6, $1);
  }
  ;

parameter_list_opt: parameter_list { $$ = $1; }
  | epsilon { $$ = std::vector<std::unique_ptr<ParamNode>>{}; }
  ;

parameter_list: parameter_list ',' parameter {
    auto parameter_list = $1;
    parameter_list.push_back($3);
    $$ = std::move(parameter_list);
  }
  | parameter {
    $$ = std::vector<std::unique_ptr<ParamNode>>{};
    $$.push_back($1);
  }
  ;

parameter: type_specifier ID {
    $$ = std::make_unique<ParamNode>($2, $1);
  }
  ;

  /* 6.8.2 Compound statement */
compound_stmt: '{' block_item_list_opt '}' {
    $$ = std::make_unique<CompoundStmtNode>($2);
  }
  ;

block_item_list_opt: block_item_list { $$ = $1; }
  | epsilon {
    $$ = std::vector<CompoundStmtNode::Item>{};
  }
  ;

block_item_list: block_item {
    $$ = std::vector<CompoundStmtNode::Item>{};
    $$.push_back($1);
  }
  | block_item_list block_item {
    auto block_item_list = $1;
    block_item_list.push_back($2);
    $$ = std::move(block_item_list);
  }
  ;

block_item: decl { $$ = $1; }
  | stmt { $$ = $1; }
  ;

decl: type_specifier ID ';' { $$ = std::make_unique<DeclNode>($2, $1); }
    | type_specifier ID '=' expr ';' { $$ = std::make_unique<DeclNode>($2, $1, $4); }
    ;

stmt: expr_opt ';' { $$ = std::make_unique<ExprStmtNode>($1); }
    | compound_stmt { $$ = $1; }
    | selection_stmt { $$ = $1; }
    | labeled_stmt { $$ = $1; }
    | WHILE '(' expr ')' stmt { $$ = std::make_unique<WhileStmtNode>($3, $5); }
    | DO stmt WHILE '(' expr ')' ';' { $$ = std::make_unique<WhileStmtNode>($5, $2, true); }
    | FOR '(' loop_init expr_opt ';' expr_opt ')' stmt { $$ = std::make_unique<ForStmtNode>($3, $4, $6, $8); }
    | jump_stmt { $$ = $1; }
    ;

/* 6.8.1 Labeled statements */
labeled_stmt: /* TODO: identifier label */
    /* TODO: constant expression */
    CASE expr ':' stmt { $$ = std::make_unique<CaseStmtNode>($2, $4); }
    | DEFAULT ':' stmt { $$ = std::make_unique<DefaultStmtNode>($3); }
    ;

/* 6.8.4 Selection statements */
selection_stmt: IF '(' expr ')' stmt %prec IF_WITHOUT_ELSE { $$ = std::make_unique<IfStmtNode>($3, $5); }
    | IF '(' expr ')' stmt ELSE stmt { $$ = std::make_unique<IfStmtNode>($3, $5, $7); }
    | SWITCH '(' expr ')' stmt { $$ = std::make_unique<SwitchStmtNode>($3, $5); }
    ;

/* 6.8.6 Jump statements */
jump_stmt: RETURN expr ';' { $$ = std::make_unique<ReturnStmtNode>($2); }
    | BREAK ';' { $$ = std::make_unique<BreakStmtNode>(); }
    | CONTINUE ';' { $$ = std::make_unique<ContinueStmtNode>(); }
    /* TODO: goto */
    ;

loop_init: decl { $$ = std::make_unique<LoopInitNode>($1); }
    | expr_opt ';' { $$ = std::make_unique<LoopInitNode>($1); }
    ;

expr_opt: expr { $$ = $1; }
    | epsilon { $$ = std::make_unique<NullExprNode>(); }
    ;

expr: unary_expr { $$ = $1; }
  /* additive 6.5.6 */
  | expr '+' expr { $$ = std::make_unique<BinaryExprNode>(BinaryOperator::kAdd, $1, $3); }
  | expr '-' expr { $$ = std::make_unique<BinaryExprNode>(BinaryOperator::kSub, $1, $3); }
  /* multiplicative 6.5.5 */
  | expr '*' expr { $$ = std::make_unique<BinaryExprNode>(BinaryOperator::kMul, $1, $3); }
  | expr '/' expr { $$ = std::make_unique<BinaryExprNode>(BinaryOperator::kDiv, $1, $3); }
  | expr '%' expr { $$ = std::make_unique<BinaryExprNode>(BinaryOperator::kMod, $1, $3); }
  /* relational 6.5.8 */
  | expr '>' expr { $$ = std::make_unique<BinaryExprNode>(BinaryOperator::kGt, $1, $3); }
  | expr '<' expr { $$ = std::make_unique<BinaryExprNode>(BinaryOperator::kLt, $1, $3); }
  | expr GE expr { $$ = std::make_unique<BinaryExprNode>(BinaryOperator::kGte, $1, $3); }
  | expr LE expr { $$ = std::make_unique<BinaryExprNode>(BinaryOperator::kLte, $1, $3); }
  /* equality 6.5.9 */
  | expr EQ expr { $$ = std::make_unique<BinaryExprNode>(BinaryOperator::kEq, $1, $3); }
  | expr NE expr { $$ = std::make_unique<BinaryExprNode>(BinaryOperator::kNeq, $1, $3); }
  /* assignment 6.5.16 */
  | ID '=' expr { $$ = std::make_unique<SimpleAssignmentExprNode>($1, $3); }
  ;

/* 6.5.3 Unary operators */
unary_expr: postfix_expr { $$ = $1; }
  | INCR unary_expr { $$ = std::make_unique<UnaryExprNode>(UnaryOperator::kIncr, $2); }
  | DECR unary_expr { $$ = std::make_unique<UnaryExprNode>(UnaryOperator::kDecr, $2); }
  | '+' unary_expr { $$ = std::make_unique<UnaryExprNode>(UnaryOperator::kPos, $2); }
  | '-' unary_expr { $$ = std::make_unique<UnaryExprNode>(UnaryOperator::kNeg, $2); }
  | '!' unary_expr { $$ = std::make_unique<UnaryExprNode>(UnaryOperator::kNot, $2); }
  /* TODO: implement pointer type */
  | '&' unary_expr { $$ = std::make_unique<UnaryExprNode>(UnaryOperator::kAddr, $2); }
  | '*' unary_expr { $$ = std::make_unique<UnaryExprNode>(UnaryOperator::kDeref, $2); }
  | '~' unary_expr { $$ = std::make_unique<UnaryExprNode>(UnaryOperator::kBitComp, $2); }
  /* TODO: sizeof */
  ;

/* 6.5.2 Postfix operators */
postfix_expr: primary_expr { $$ = $1; }
  /* TODO: support arguments */
  | postfix_expr '(' arg_list_opt ')' { $$ = std::make_unique<FunCallExprNode>($1, $3); }
  ;

arg_list_opt: arg_list { $$ = $1; }
  | epsilon { $$ = std::vector<std::unique_ptr<ArgExprNode>>{}; }
  ;

arg_list: arg_list ',' arg {
    auto arg_list = $1;
    arg_list.push_back($3);
    $$ = std::move(arg_list);
  }
  | arg {
    $$ = std::vector<std::unique_ptr<ArgExprNode>>{};
    $$.push_back($1);
  }
  ;

arg: expr {
    $$ = std::make_unique<ArgExprNode>($1);
  }
  ;

primary_expr: ID { $$ = std::make_unique<IdExprNode>($1); }
  | NUM { $$ = std::make_unique<IntConstExprNode>($1); }
  | '(' expr ')' { $$ = $2; }
  ;

/* 6.7.2 Type specifiers */
/* TODO: support multiple data types */
type_specifier: INT {
    $$ = ExprType::kInt;
  }
  ;

epsilon: %empty;
%%

void yy::parser::error(const std::string& err) {
  std::cerr << err << std::endl;
}
