/* terminates after a file reaches the end */
%option noyywrap
/* we're not using input & yyunput, don't generate code for them to avoid compiler warnings */
%option noinput
%option nounput

%{

#include <cstdlib>
#include <iostream>

#include "y.tab.hpp"

// Give Flex the prototype of yylex we want.
# define YY_DECL \
  yy::parser::symbol_type yylex()

static auto yylloc = yy::location{};

// For more details, see https://stackoverflow.com/a/22125500.
#define YY_USER_ACTION \
  yylloc.begin.line = yylloc.end.line; \
  yylloc.begin.column = yylloc.end.column; \
  for (auto i = 0; i < yyleng; ++i) { \
    if (yytext[i] == '\n') { \
      yylloc.end.line++; \
      yylloc.end.column = 1; \
    } else { \
      yylloc.end.column++; \
    } \
  }

%}

/* You can include letters, decimal digits, and the underscore character '_'
   in identifiers. The first character of an identifier cannot be a digit.
   Reference:
   https://www.gnu.org/software/gnu-c-manual/gnu-c-manual.html#Identifiers */
identifier [a-zA-Z_][0-9a-zA-Z_]*
integer [0-9]+

%x C_COMMENT CXX_COMMENT

%%

  /* delimiters */
";" { return yy::parser::make_SEMICOLON(yylloc); }
":" { return yy::parser::make_COLON(yylloc); }

  /* commas */
"," { return yy::parser::make_COMMA(yylloc); }

  /* operators */
"-" { return yy::parser::make_MINUS(yylloc); }
"+" { return yy::parser::make_PLUS(yylloc); }
"*" { return yy::parser::make_STAR(yylloc); }
"/" { return yy::parser::make_DIV(yylloc); }
"%" { return yy::parser::make_MOD(yylloc); }
"&" { return yy::parser::make_AMPERSAND(yylloc); }
"!" { return yy::parser::make_EXCLAMATION(yylloc); }
"?" { return yy::parser::make_QUESTION(yylloc); }
"^" { return yy::parser::make_XOR(yylloc); }
"|" { return yy::parser::make_OR(yylloc); }
"||" { return yy::parser::make_LOGIC_OR(yylloc); }
"&&" { return yy::parser::make_LOGIC_AND(yylloc); }
"~" { return yy::parser::make_TILDE(yylloc); }
"=" { return yy::parser::make_ASSIGN(yylloc); }
"<<" { return yy::parser::make_SHIFT_LEFT(yylloc); }
">>" { return yy::parser::make_SHIFT_RIGHT(yylloc); }
"<" { return yy::parser::make_LT(yylloc); }
">" { return yy::parser::make_GT(yylloc); }
"--" { return yy::parser::make_DECR(yylloc); }
"++" { return yy::parser::make_INCR(yylloc); }
"==" { return yy::parser::make_EQ(yylloc); }
"!=" { return yy::parser::make_NE(yylloc); }
"<=" { return yy::parser::make_LE(yylloc); }
">=" { return yy::parser::make_GE(yylloc); }

  /* keywords */
break { return yy::parser::make_BREAK(yylloc); }
continue { return yy::parser::make_CONTINUE(yylloc); }
switch { return yy::parser::make_SWITCH(yylloc); }
case { return yy::parser::make_CASE(yylloc); }
default { return yy::parser::make_DEFAULT(yylloc); }
else { return yy::parser::make_ELSE(yylloc); }
for { return yy::parser::make_FOR(yylloc); }
do { return yy::parser::make_DO(yylloc); }
if { return yy::parser::make_IF(yylloc); }
int { return yy::parser::make_INT(yylloc); }
return { return yy::parser::make_RETURN(yylloc); }
while { return yy::parser::make_WHILE(yylloc); }
goto { return yy::parser::make_GOTO(yylloc); }
struct { return yy::parser::make_STRUCT(yylloc); }
union { return yy::parser::make_UNION(yylloc); }

  /* brackets */
"(" { return yy::parser::make_LEFT_PAREN(yylloc); }
")" { return yy::parser::make_RIGHT_PAREN(yylloc); }
"{" { return yy::parser::make_LEFT_CURLY(yylloc); }
"}" { return yy::parser::make_RIGHT_CURLY(yylloc); }
"[" { return yy::parser::make_LEFT_SQUARE(yylloc); }
"]" { return yy::parser::make_RIGHT_SQUARE(yylloc); }

{identifier} {
    return yy::parser::make_ID(yytext, yylloc);
  }

{integer} {
    return yy::parser::make_NUM(std::atoi(yytext), yylloc);
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

<<EOF>> { return yy::parser::make_EOF(yylloc); }

[ \t\r] {}

\n {}

. {
    std::cerr << "Invalid input: " << yytext << std::endl;
    std::exit(-1);
  }

%%
