#include <fmt/core.h>

#include <cstdio>
#include <cstdlib>
#include <cxxopts.hpp>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <memory>
#include <string>
#include <utility>

#include "ast.hpp"
#include "ast_dumper.hpp"
#include "llvm_ir_generator.hpp"
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

int QbeBuilder(std::unique_ptr<AstNode> trans_unit, std::string& input_basename,
               std::string& output_name);

int LLVMBuilder(std::unique_ptr<AstNode> trans_unit,
                std::string& input_basename, std::string& output_name);

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
      ("t, target", "Specify target IR", cxxopts::value<std::string>()->default_value("qbe"), "[qbe|llvm]")
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

  /// @brief The root node of the trans_unit.
  auto trans_unit = std::unique_ptr<AstNode>{};
  yy::parser parser{trans_unit};
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
  trans_unit->Accept(type_checker);
  if (opts["dump"].as<bool>()) {
    const auto max_level = 80u;
    AstDumper ast_dumper{Indenter{' ', Indenter::SizePerLevel{2},
                                  Indenter::MaxLevel{max_level}}};
    trans_unit->Accept(ast_dumper);
  }

  auto input_basename = input_path.stem().string();
  auto output = opts["output"].as<std::string>();
  // generate intermediate representation based on target option
  if (opts["target"].as<std::string>() == "qbe") {
    return QbeBuilder(std::move(trans_unit), input_basename, output);
  } else if (opts["target"].as<std::string>() == "llvm") {
    return LLVMBuilder(std::move(trans_unit), input_basename, output);
  } else {
    std::cerr << "unknown target" << '\n';
    std::exit(0);
  }

  return 0;
}

int QbeBuilder(std::unique_ptr<AstNode> trans_unit, std::string& input_basename,
               std::string& output_name) {
  auto output_ir = std::ofstream{fmt::format("{}.ssa", input_basename)};
  QbeIrGenerator code_generator{output_ir};
  trans_unit->Accept(code_generator);

  output_ir.close();

  // generate assembly
  std::string qbe_command = fmt::format("qbe -o {0}.s {0}.ssa", input_basename);
  auto qbe_ret = std::system(qbe_command.c_str());
  // 0 on success, 1 otherwise
  if (qbe_ret) {
    return qbe_ret;
  }
  // generate executable
  std::string cc_command =
      fmt::format("cc -o {} {}.s", output_name, input_basename);
  auto cc_ret = std::system(cc_command.c_str());
  // 0 on success, 1 otherwise
  if (cc_ret) {
    return cc_ret;
  }

  return 0;
}

int LLVMBuilder(std::unique_ptr<AstNode> trans_unit,
                std::string& input_basename, std::string& output_name) {
  auto output_ir = std::ofstream{fmt::format("{}.ll", input_basename)};
  LLVMIRGenerator code_generator{output_ir, input_basename};
  trans_unit->Accept(code_generator);
  // TODO: Write to stdout based on cxxopts.
  // Write LLVM IR to output file "*.ll".
  code_generator.PrintIR();

  output_ir.close();
  return 0;
}
