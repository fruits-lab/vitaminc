%{

#include <fstream>
#include <iostream>
#include <memory>
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

%token <int> NUM

%nterm <std::unique_ptr<ExprNode>> expr
%nterm <std::vector<std::unique_ptr<ExprNode>>> exprs

%left '+' '-'
%left '*' '/'

%start entry

%%
entry: exprs {
    auto program = std::make_unique<ProgramNode>($1);
    program->Dump(0);
  }
  ;

exprs: exprs expr {
    auto exprs = $1;
    exprs.push_back($2);
    $$ = std::move(exprs);
  }
  | epsilon { $$ = std::vector<std::unique_ptr<ExprNode>>{}; }
  ;

expr: NUM { $$ = std::make_unique<IntConstExprNode>($1); }
  | expr '+' expr { $$ = std::make_unique<PlusExprNode>($1, $3); }
  | expr '-' expr { $$ = std::make_unique<SubExprNode>($1, $3); }
  | expr '*' expr { $$ = std::make_unique<MulExprNode>($1, $3); }
  | expr '/' expr { $$ = std::make_unique<DivExprNode>($1, $3); }
  ;

epsilon: /* empty */ ;
%%

void yy::parser::error(const std::string& err) {
  std::cout << err << std::endl;
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
