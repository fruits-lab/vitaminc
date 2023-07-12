%{

#include <iostream>

#include "lex.yy.c"

extern int yylex();
extern int yylex_destroy(void);
void yyerror(const char *err);
%}

%union {
  int ival;
}

%token <ival> NUM

%start entry

%%
entry: exprs {}
     ;

exprs: exprs expr
     | epsilon
     ;

expr: NUM { std::cout << $1 << std::endl; }
    ;

epsilon: /* empty */ ;
%%

void yyerror(const char *err){
  std::cout << err << std::endl;
}

int main(int argc, char **argv){
  if (yyparse() == 1){
      yyerror("parsing error");
  }
  yylex_destroy();

  return 0;
}
