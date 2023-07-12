%{

#include <iostream>

#include "ast.cpp"
#include "lex.yy.c"

extern int yylex();
extern int yylex_destroy(void);
void yyerror(const char *err);
%}

%union {
  int ival;
  ExprNode* expr;
  std::vector<ExprNode*>* exprs;
}

%token <ival> NUM

%type <expr> expr
%type <exprs> exprs

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

void yyerror(const char *err) {
  std::cout << err << std::endl;
}

int main(int argc, char **argv) {
  if (yyparse() == 1) {
      yyerror("parsing error");
  }
  yylex_destroy();

  return 0;
}
