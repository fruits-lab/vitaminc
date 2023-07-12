%{

#include <cstdlib>
#include <iostream>

#include "y.tab.h"

%}

integer [0-9]+

%%

{integer} {
            yylval.ival = std::atoi(yytext);
            return(NUM);
          }

[ \t\r]   {}

.         {
            std::cout << "Invalid input: " << yytext << std::endl;
            std::exit(-1);
          }

%%

int yywrap() {
  return(1);
}


