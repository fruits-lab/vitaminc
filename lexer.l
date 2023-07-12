%{

#include <iostream>
#include <cstdlib>

#include "y.tab.h"

%}

integer [0-9]+

%%

{integer} {
            yylval.ival = atoi(yytext);
            return(NUM);
          }

[ \t\r]   {}

.         {
            std::cout << "Invalid input: " << yytext << std::endl;
            exit(-1);
          }

%%

int yywrap()
{
  return(1);
}


