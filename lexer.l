/* terminates after a file reaches the end */
%option noyywrap
/* we're not using input & yyunput, don't generate code for them to avoid compiler warnings */
%option noinput
%option nounput
/* automatically track line number */
%option yylineno

%{

// Give Flex the prototype of yylex we want.
# define YY_DECL \
  yy::parser::symbol_type yylex()

#include <cstdlib>
#include <iostream>

#include "y.tab.h"

// Use LINENO for the actual line number in the source code
// yylineno has advanced to the next line when we reference it.
#define LINENO (yylineno - 1)
%}

/* You can include letters, decimal digits, and the underscore character '_'
   in identifiers. The first character of an identifier cannot be a digit.
   Reference:
   https://www.gnu.org/software/gnu-c-manual/gnu-c-manual.html#Identifiers */
identifier [a-zA-Z_][0-9a-zA-Z_]*
integer [0-9]+
/* minus sign (-) must be the first or last one since it has special meaning
   when it's in the middle */
operator [-+*/%=<>]
bracket [\(\)\{\}\[\]]

%x C_COMMENT CXX_COMMENT

%%

  /* delimiters */
";" { return yytext[0]; }

  /* operators */
{operator} { return yytext[0]; }
"==" { return yy::parser::make_EQ(); }
"!=" { return yy::parser::make_NE(); }
"<=" { return yy::parser::make_LE(); }
">=" { return yy::parser::make_GE(); }

  /* keywords */
int { return yy::parser::make_INT(); }
main { return yy::parser::make_MAIN(); }
return { return yy::parser::make_RETURN(); }

  /* brackets */
{bracket} { return yytext[0]; }

{identifier} {
    return yy::parser::make_ID(yytext);
  }

{integer} {
    return yy::parser::make_NUM(std::atoi(yytext));
  }

  /* comments */
"/*" {
    BEGIN(C_COMMENT);
  }
<C_COMMENT>"*/" {
    BEGIN(INITIAL);
  }
"//" {
    BEGIN(CXX_COMMENT);
  }
<CXX_COMMENT>\n {
    BEGIN(INITIAL);
  }
<C_COMMENT,CXX_COMMENT>.|\n { /* eat up */ }

<<EOF>> { return yy::parser::make_EOF(); }

[ \t\r] {}

\n {}

. {
    std::cerr << "Invalid input: " << yytext << std::endl;
    std::exit(-1);
  }

%%
