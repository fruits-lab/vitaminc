# VitaminC

[![CI](https://github.com/fruits-lab/vitaminc/actions/workflows/ci.yml/badge.svg)](https://github.com/fruits-lab/vitaminc/actions/workflows/ci.yml)

VitaminC 🍋 is an educational C compiler frontend written in C++. As a frontend, it generates [intermediate representations](https://en.wikipedia.org/wiki/Intermediate_representation) (IR). The compilation phases—lexer, parser, type checker, and IR generator—are carefully designed as individual classes.

## Features

> [!WARNING]
> This project is still under development. Many features are not yet implemented. Currently, we do not support preprocessor directives, and only the `int` type and its pointer, as well as object types, are supported.

The main goal of this project is to demonstrate how a compiler frontend works with [the LLVM compiler infrastructure](https://llvm.org/) and generates LLVM IR. However, we are not yet there. Currently, we generate [QBE](https://c9x.me/compile/) IR manually.

We are not aiming to be a fully compliant C compiler, although we strive to be as compliant as possible with C89 and support common C99 features.

## Prerequisites

- A C++ compiler that supports C++17.
- [GNU Make](https://www.gnu.org/software/make/): for building the project.
- [QBE](https://c9x.me/compile/releases.html): for compiling the QBE IR down to assembly.
- [cxxopts](https://github.com/jarro2783/cxxopts): for command-line argument parsing.
- [fmt](https://fmt.dev/latest/index.html): for modern C++ formatting.
- (test-only) [turnt](https://github.com/cucapra/turnt): for snapshot testing.

> [!NOTE]
> We provide installation scripts under the `scripts/` directory to install some of the prerequisites. You can run the corresponding script to install the prerequisites or to see what versions we are using.

## Build

To build the project, run the following command:

```console
make
```

This will generate an executable named `vitaminc`.

If you have _turnt_ installed, you can run the tests with:

```console
make test
```

## Usage

```console
$ ./vitaminc --help
A simple C compiler.
Usage:
  ./vitaminc [options] file

  -o, --output <file>  Write output to <file> (default: a.out)
  -d, --dump           Dump the abstract syntax tree
  -t, --target [qbe]   Specify target IR (default: qbe)
  -h, --help           Display available options
```

## License

This project is licensed under the [MIT License](LICENSE).
