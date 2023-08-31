#include <fstream>
#include <memory>

#include "ast.hpp"
#include "scope.hpp"
#include "y.tab.h"

/// @brief Where the generated code goes.
std::ofstream output;
/// @brief The root node of the program.
auto program = std::unique_ptr<AstNode>{};

extern void yylex_destroy();

int main(int argc, char** argv) {
  /* TODO: read input parameter */
  output.open("test.ssa");
  yy::parser parser{};
  int ret = parser.parse();

  yylex_destroy();

  // 1 if parsing failed because of invalid input;
  // 2 if parsing failed due to memory exhaustion.
  if (ret) {
    return ret;
  }

  // perform analyses and transformations on the ast
  auto scopes = ScopeStack{};
  program->CheckType(scopes);
  program->Dump(0);
  program->CodeGen();

  output.close();

  return 0;
}
