%{

// Give Flex the prototype of yylex we want.
# define YY_DECL \
  yy::parser::symbol_type yylex()

#include <cstdlib>
#include <iostream>

#include "y.tab.h"

%}

integer [0-9]+
/* minus sign (-) must be the first or last one since it has special meaning
   when it's in the middle */
operator [-+*/]

%%

{integer} {
    return yy::parser::make_NUM(std::atoi(yytext));
  }

{operator} { return yytext[0]; }

[ \t\r]   {}

. {
    std::cout << "Invalid input: " << yytext << std::endl;
    std::exit(-1);
  }

%%

int yywrap() {
  return(1);
}
