%{

#include <iostream>

#include "ast.cpp"
#include "lex.yy.c"

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

%token <int> NUM

%nterm <ExprNode*> expr
%nterm <std::vector<ExprNode*>*> exprs

%left '+' '-'
%left '*' '/'

%start entry

%%
entry: exprs {
    auto* program = new ProgramNode{$1};
    program->Dump();
    delete program;
  }
  ;

exprs: exprs expr {
    $1->push_back($2);
    $$ = $1;
  }
  | epsilon { $$ = new std::vector<ExprNode*>{}; }
  ;

expr: NUM { $$ = new IntConstExprNode{$1}; }
  | expr '+' expr { $$ = new PlusExprNode{$1, $3}; }
  | expr '-' expr { $$ = new SubExprNode{$1, $3}; }
  | expr '*' expr { $$ = new MulExprNode{$1, $3}; }
  | expr '/' expr { $$ = new DivExprNode{$1, $3}; }
  ;

epsilon: /* empty */ ;
%%

void yy::parser::error(const std::string& err) {
  std::cout << err << std::endl;
}

int main(int argc, char **argv) {
  yy::parser parser{};
  int ret = parser.parse();

  yylex_destroy();

  return ret;
}
