#include <fstream>

#include "y.tab.h"

/// @brief Where the generated code goes.
std::ofstream output;

extern void yylex_destroy();

int main(int argc, char** argv) {
  /* TODO: read input parameter */
  output.open("test.ssa");
  yy::parser parser{};
  int ret = parser.parse();

  yylex_destroy();
  output.close();

  return ret;
}
