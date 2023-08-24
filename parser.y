%{

#include <fstream>
#include <iostream>
#include <utility>

#include "ast.hpp"
#include "lex.yy.c"

std::ofstream output;
%}

// Dependency code required for the value and location types;
// inserts verbatim to the header file.
%code requires {
  #include <memory>
  #include <string>
  #include <vector>

  #include "ast.cpp"
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

%token <int> NUM
%token <std::string> ID
%token
  INT
  MAIN
  RETURN
  EOF 0
;

%nterm <std::unique_ptr<ExprNode>> expr
%nterm <std::unique_ptr<DeclNode>> decl
%nterm <std::vector<std::unique_ptr<DeclNode>>> decls
%nterm <std::unique_ptr<StmtNode>> stmt
%nterm <std::vector<std::unique_ptr<StmtNode>>> stmts
%nterm <std::unique_ptr<BlockStmtNode>> main_func

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

  /* TODO: mix declarations and statements in compound statement */
main_func: INT MAIN '(' ')' '{' decls stmts '}' {
    $$ = std::make_unique<BlockStmtNode>($6, $7);
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
decl: INT ID ';' { $$ = std::make_unique<DeclNode>($2); }
    | INT ID '=' expr ';' { $$ = std::make_unique<DeclNode>($2, $4); }
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
