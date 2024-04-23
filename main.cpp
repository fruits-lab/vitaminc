#include <fmt/core.h>

#include <cstdio>
#include <cstdlib>
#include <cxxopts.hpp>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <memory>

#include "ast.hpp"
#include "ast_dumper.hpp"
#include "qbe_ir_generator.hpp"
#include "scope.hpp"
#include "type_checker.hpp"
#include "util.hpp"
#include "y.tab.hpp"

extern FILE*
    yyin;  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables):
           // flex read from this file pointer.
extern void yylex_destroy();  // NOLINT(readability-identifier-naming): extern
                              // from flex generated code.

int main(  // NOLINT(bugprone-exception-escape): Using a big try-catch block to
           // catch all exceptions isn't reasonable.
    int argc, char** argv)

{
  auto cmd_options = cxxopts::Options{
      argv[0],  // NOLINT(cppcoreguidelines-pro-bounds-pointer-arithmetic):
                // std::span is available in C++20.
      "A simple C compiler."};
  // clang-format off
  cmd_options.custom_help("[options] file");
  cmd_options.add_options()
      ("o, output", "Write output to <file>", cxxopts::value<std::string>()->default_value("a.out"), "<file>")
      ("d, dump", "Dump the abstract syntax tree", cxxopts::value<bool>()->default_value("false"))
      // TODO: support LLVM IR
      ("t, target", "Specify target IR", cxxopts::value<std::string>()->default_value("qbe"), "[qbe]")
      ("h, help", "Display available options")
      ;
  // clang-format on

  auto opts = cmd_options.parse(argc, argv);
  if (opts.count("help")) {
    std::cerr << cmd_options.help() << '\n';
    std::exit(0);
  }

  auto args = opts.unmatched();
  if (args.size() == 0) {
    std::cerr << "no input files" << '\n';
    std::exit(0);
  }

  // TODO: support compiling multiple files
  if (args.size() > 1) {
    std::cerr << "cannot compile more than one input file" << '\n';
    std::exit(0);
  }

  auto input_path = std::filesystem::path(args.at(0));
  // NOLINTNEXTLINE(cppcoreguidelines-owning-memory)
  yyin = fopen(input_path.c_str(), "r");
  if (yyin == nullptr) {
    std::cerr << "cannot open input file" << '\n';
    std::exit(0);
  }

  /// @brief The root node of the program.
  auto program = std::unique_ptr<AstNode>{};
  yy::parser parser{program};
  int ret = parser.parse();

  // NOLINTNEXTLINE(cppcoreguidelines-owning-memory)
  fclose(yyin);
  yylex_destroy();

  // 0 on success, 1 otherwise
  if (ret) {
    return ret;
  }

  // perform analyses and transformations on the ast
  auto scopes = ScopeStack{};
  TypeChecker type_checker{scopes};
  program->Accept(type_checker);
  if (opts["dump"].as<bool>()) {
    const auto max_level = 80u;
    AstDumper ast_dumper{Indenter{' ', Indenter::SizePerLevel{2},
                                  Indenter::MaxLevel{max_level}}};
    program->Accept(ast_dumper);
  }

  // generate intermediate representation
  auto input_basename = input_path.stem().string();
  auto output_ir = std::ofstream{fmt::format("{}.ssa", input_basename)};
  QbeIrGenerator code_generator{output_ir};
  program->Accept(code_generator);

  output_ir.close();

  // generate assembly
  if (opts["target"].as<std::string>() == "qbe") {
    std::string qbe_command =
        fmt::format("qbe -o {0}.s {0}.ssa", input_basename);
    auto qbe_ret = std::system(qbe_command.c_str());
    // 0 on success, 1 otherwise
    if (qbe_ret) {
      return qbe_ret;
    }
  } else {
    std::cerr << "unknown target" << '\n';
    std::exit(0);
  }

  // generate executable
  auto output = opts["output"].as<std::string>();
  std::string cc_command = fmt::format("cc -o {} {}.s", output, input_basename);
  auto cc_ret = std::system(cc_command.c_str());
  // 0 on success, 1 otherwise
  if (cc_ret) {
    return cc_ret;
  }

  return 0;
}
