%{

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
            yylval.ival = std::atoi(yytext);
            return(NUM);
          }

{operator} { return yytext[0]; }

[ \t\r]   {}

.         {
            std::cout << "Invalid input: " << yytext << std::endl;
            std::exit(-1);
          }

%%

int yywrap() {
  return(1);
}
