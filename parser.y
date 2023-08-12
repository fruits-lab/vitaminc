%{

#include <fstream>
#include <iostream>
#include <memory>
#include <string>
#include <utility>

#include "ast.cpp"
#include "lex.yy.c"

std::ofstream output;
%}

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

%token <int> NUM
%token <std::string> ID
%token
  INT
  MAIN
  RETURN
  EOF 0
;

%nterm <std::unique_ptr<ExprNode>> expr
%nterm <std::vector<std::unique_ptr<ExprNode>>> exprs
%nterm <std::unique_ptr<StmtNode>> stmt
%nterm <std::vector<std::unique_ptr<StmtNode>>> stmts
%nterm <std::vector<std::unique_ptr<StmtNode>>> main_func

%left '+' '-'
%left '*' '/'

%start entry

%%
entry: main_func {
    auto program = std::make_unique<ProgramNode>($1);
    program->Dump(0);
    program->CodeGen();
  }
  ;

main_func: INT MAIN '(' ')' '{' stmts '}' { $$ = $6; }
  ;

stmts: stmts stmt {
    auto stmts = $1;
    stmts.push_back($2);
    $$ = std::move(stmts);
  }
  | epsilon { $$ = std::vector<std::unique_ptr<StmtNode>>{}; }
  ;

stmt: ';' { $$ = std::make_unique<NullStmtNode>(); }
    | RETURN exprs ';' { $$ = std::make_unique<ReturnStmtNode>($2); }
    | exprs ';' { $$ = std::make_unique<ExprStmtNode>($1); }
    ;

exprs: exprs expr {
    auto exprs = $1;
    exprs.push_back($2);
    $$ = std::move(exprs);
  }
  | epsilon { $$ = std::vector<std::unique_ptr<ExprNode>>{}; }
  ;

expr: ID { $$ = std::make_unique<IdExprNode>($1); }
  | NUM { $$ = std::make_unique<IntConstExprNode>($1); }
  | expr '+' expr { $$ = std::make_unique<PlusExprNode>($1, $3); }
  | expr '-' expr { $$ = std::make_unique<SubExprNode>($1, $3); }
  | expr '*' expr { $$ = std::make_unique<MulExprNode>($1, $3); }
  | expr '/' expr { $$ = std::make_unique<DivExprNode>($1, $3); }
  ;

epsilon: /* empty */ ;
%%

void yy::parser::error(const std::string& err) {
  std::cerr << err << std::endl;
}

int main(int argc, char **argv) {
  /* TODO: read input parameter */
  output.open("test.ssa");
  yy::parser parser{};
  int ret = parser.parse();

  yylex_destroy();
  output.close();

  return ret;
}
