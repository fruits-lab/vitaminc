%{

#include <iostream>
#include <memory>
#include <utility>
#include <vector>

#include "ast.hpp"
#include "lex.yy.cpp"
#include "type.hpp"

extern std::unique_ptr<AstNode> program;
%}

// Dependency code required for the value and location types;
// inserts verbatim to the header file.
%code requires {
  #include <memory>
  #include <string>
  #include <vector>

  #include "ast.hpp"
}

%skeleton "lalr1.cc"
%require "3.2"
%language "c++"

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
%token INT MAIN RETURN
%token IF ELSE
%token EQ NE LE GE
%token DO WHILE
%token EOF 0

%nterm <std::unique_ptr<ExprNode>> expr
%nterm <std::unique_ptr<DeclNode>> decl
%nterm <std::vector<std::unique_ptr<DeclNode>>> decls
%nterm <std::unique_ptr<StmtNode>> stmt
%nterm <std::vector<std::unique_ptr<StmtNode>>> stmts
%nterm <std::unique_ptr<BlockStmtNode>> block
%nterm <std::unique_ptr<BlockStmtNode>> main_func

%left '='
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
entry: main_func {
    program = std::make_unique<ProgramNode>($1);
  }
  ;

main_func: INT MAIN '(' ')' block {
    $$ = $5;
  }
  ;

  /* TODO: mix declarations and statements in compound statement */
block: '{' decls stmts '}' {
    $$ = std::make_unique<BlockStmtNode>($2, $3);
  }
  ;

decls: decls decl {
    auto decls = $1;
    decls.push_back($2);
    $$ = std::move(decls);
  }
  | epsilon { $$ = std::vector<std::unique_ptr<DeclNode>>{}; }
  ;

  /* TODO: parse multiple data types and id list */
decl: INT ID ';' { $$ = std::make_unique<DeclNode>($2, ExprType::kInt); }
    | INT ID '=' expr ';' { $$ = std::make_unique<DeclNode>($2, ExprType::kInt, $4); }
    ;

stmts: stmts stmt {
    auto stmts = $1;
    stmts.push_back($2);
    $$ = std::move(stmts);
  }
  | epsilon { $$ = std::vector<std::unique_ptr<StmtNode>>{}; }
  ;

stmt: ';' { $$ = std::make_unique<NullStmtNode>(); }
    | RETURN expr ';' { $$ = std::make_unique<ReturnStmtNode>($2); }
    | expr ';' { $$ = std::make_unique<ExprStmtNode>($1); }
    | block { $$ = $1; }
    | IF '(' expr ')' stmt %prec IF_WITHOUT_ELSE { $$ = std::make_unique<IfStmtNode>($3, $5); }
    | IF '(' expr ')' stmt ELSE stmt { $$ = std::make_unique<IfStmtNode>($3, $5, $7); }
    | WHILE '(' expr ')' stmt { $$ = std::make_unique<WhileStmtNode>($3, $5); }
    | DO stmt WHILE '(' expr ')' ';' { $$ = std::make_unique<WhileStmtNode>($5, $2, true); }
    ;

expr: ID { $$ = std::make_unique<IdExprNode>($1); }
  | NUM { $$ = std::make_unique<IntConstExprNode>($1); }
  /* additive 6.5.6 */
  | expr '+' expr { $$ = std::make_unique<PlusExprNode>($1, $3); }
  | expr '-' expr { $$ = std::make_unique<SubExprNode>($1, $3); }
  /* multiplicative 6.5.5 */
  | expr '*' expr { $$ = std::make_unique<MulExprNode>($1, $3); }
  | expr '/' expr { $$ = std::make_unique<DivExprNode>($1, $3); }
  | expr '%' expr { $$ = std::make_unique<ModExprNode>($1, $3); }
  /* relational 6.5.8 */
  | expr '>' expr { $$ = std::make_unique<GreaterThanExprNode>($1, $3); }
  | expr '<' expr { $$ = std::make_unique<LessThanExprNode>($1, $3); }
  | expr GE expr { $$ = std::make_unique<GreaterThanOrEqualToExprNode>($1, $3); }
  | expr LE expr { $$ = std::make_unique<LessThanOrEqualToExprNode>($1, $3); }
  /* equality 6.5.9 */
  | expr EQ expr { $$ = std::make_unique<EqualToExprNode>($1, $3); }
  | expr NE expr { $$ = std::make_unique<NotEqualToExprNode>($1, $3); }
  | '(' expr ')' { $$ = $2; }
  /* assignment 6.5.16 */
  | ID '=' expr { $$ = std::make_unique<SimpleAssignmentExprNode>($1, $3); }
  ;

epsilon: /* empty */ ;
%%

void yy::parser::error(const std::string& err) {
  std::cerr << err << std::endl;
}
