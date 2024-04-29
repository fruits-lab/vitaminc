%{

#include <iostream>
#include <memory>
#include <utility>
#include <vector>

#include "ast.hpp"
#include "location.hpp"
#include "operator.hpp"
#include "type.hpp"

%}

// Dependency code required for the value and location types;
// inserts verbatim to the header file.
%code requires {
  #include <memory>
  #include <string>
  #include <vector>

  #include "ast.hpp"
  #include "type.hpp"
}

// Placed after the usual contents of the parser header file.
%code {
  extern yy::parser::symbol_type yylex();

  /// @brief Converts the location information from Bison to our own location type.
  Location Loc(const yy::location& loc) {
    return Location{loc.begin.line, loc.begin.column};
  }
}

%skeleton "lalr1.cc"
%require "3.2"
%language "c++"
%locations

%parse-param {std::unique_ptr<AstNode>& program}

// Use complete symbols (parser::symbol_type).
%define api.token.constructor
// Allow non-pointer-based rich types.
%define api.value.type variant
// Check whether symbols are constructed and destructed using RTTI.
%define parse.assert
// Copy may be expensive when using rich types, such as std::vector.
// Also with automove, smart pointers can be moved implicity without boilerplate std::move.
// NOTE: can no longer reference a $x twice since it's moved in the first place.
%define api.value.automove
// This guarantees that headers do not conflict when included together.
%define api.token.prefix {TOK_}
// Have messages report the unexpected token, and possibly the expected ones.
// Without this, the error message is always only "syntax error".
%define parse.error verbose
// Improve syntax error handling, as LALR parser might perform additional
// parser stack reductions before discovering the syntax error.
%define parse.lac full
// Avoid creating header file since we don't use yy::location outside of the parser.
%define api.location.file none

%token MINUS PLUS STAR DIV MOD ASSIGN
%token EXCLAMATION TILDE AMPERSAND
%token COMMA SEMICOLON COLON
// (), {}, []
%token LEFT_PAREN RIGHT_PAREN LEFT_CURLY RIGHT_CURLY LEFT_SQUARE RIGHT_SQUARE

%token <int> NUM
%token <std::string> ID
%token INT
%token IF ELSE
%token SWITCH CASE DEFAULT
%token EQ LT GT NE LE GE
%token DO WHILE FOR
%token CONTINUE BREAK RETURN
%token GOTO
// increment (INCR: ++) and decrement (DECR: --)
%token INCR DECR
%token EOF 0

%nterm <std::unique_ptr<Type>> type_specifier pointer_type
%nterm <std::unique_ptr<ExprNode>> expr assign_expr expr_opt unary_expr postfix_expr primary_expr
%nterm <std::unique_ptr<DeclNode>> decl
%nterm <std::unique_ptr<DeclArrNode>> array_decl
%nterm <std::vector<std::unique_ptr<ExprNode>>> initializer_opt init_list_opt init_list
%nterm <std::unique_ptr<ArgExprNode>> arg
%nterm <std::vector<std::unique_ptr<ArgExprNode>>> arg_list_opt arg_list
%nterm <std::unique_ptr<FuncDefNode>> func_def
%nterm <std::unique_ptr<ParamNode>> parameter
%nterm <std::vector<std::unique_ptr<ParamNode>>> parameter_list_opt parameter_list
%nterm <std::vector<std::unique_ptr<FuncDefNode>>> func_def_list_opt
%nterm <std::unique_ptr<LoopInitNode>> loop_init
%nterm <std::unique_ptr<StmtNode>> stmt jump_stmt selection_stmt labeled_stmt
%nterm <std::unique_ptr<CompoundStmtNode>> compound_stmt
%nterm <std::vector<CompoundStmtNode::Item>> block_item_list block_item_list_opt
%nterm <CompoundStmtNode::Item> block_item

%precedence ASSIGN
%left EQ NE
%left LT GT LE GE
%left PLUS MINUS
%left STAR DIV MOD

// Resolve the ambiguity in the "dangling-else" grammar.
// Example: IF LEFT_PAREN expr RIGHT_PAREN IF LEFT_PAREN expr RIGHT_PAREN stmt • ELSE stmt
// Yacc has two options to make, either shift or reduce:
// Shift derivation
//   stmt
//   ↳ 13: IF LEFT_PAREN expr RIGHT_PAREN stmt
//                         ↳ 14: IF LEFT_PAREN expr RIGHT_PAREN stmt • ELSE stmt
// Reduce derivation
//   stmt
//   ↳ 14: IF LEFT_PAREN expr RIGHT_PAREN stmt                         ELSE stmt
//                         ↳ 13: IF LEFT_PAREN expr RIGHT_PAREN stmt •
//
// Our goal is to find the closest IF for ELSE, so we tell Yacc to shift.
// Since the token "ELSE" has a higher precedence than the production rule
// "if without else", Yacc shifts to "ELSE" instead of reducing with the rule.
%precedence IF_WITHOUT_ELSE
%precedence ELSE

%start entry

%%
// TODO: support global variables
entry: func_def_list_opt {
    program = std::make_unique<ProgramNode>(Loc(@1), $1);
  }
  ;

func_def_list_opt: func_def_list_opt func_def {
    auto func_def_list_opt = $1;
    func_def_list_opt.push_back($2);
    $$ = std::move(func_def_list_opt);
  }
  | epsilon { $$ = std::vector<std::unique_ptr<FuncDefNode>>{}; }
  ;

/* 6.9.1 Function definitions */
/* NOTE: The obsolete form of function definition is not supported,
   e.g., `int max(a, b) int a, b; { return a > b ? a : b; }`. */
func_def: declaration_specifiers declarator compound_stmt
  ;

/* 6.8.2 Compound statement */
compound_stmt: LEFT_CURLY block_item_list_opt RIGHT_CURLY {
    $$ = std::make_unique<CompoundStmtNode>(Loc(@1), $2);
  }
  ;

block_item_list_opt: block_item_list { $$ = $1; }
  | epsilon {
    $$ = std::vector<CompoundStmtNode::Item>{};
  }
  ;

block_item_list: block_item {
    $$ = std::vector<CompoundStmtNode::Item>{};
    $$.push_back($1);
  }
  | block_item_list block_item {
    auto block_item_list = $1;
    block_item_list.push_back($2);
    $$ = std::move(block_item_list);
  }
  ;

block_item: decl { $$ = $1; }
  | stmt { $$ = $1; }
  ;

stmt: expr_opt SEMICOLON { $$ = std::make_unique<ExprStmtNode>(Loc(@1), $1); }
    | compound_stmt { $$ = $1; }
    | selection_stmt { $$ = $1; }
    | labeled_stmt { $$ = $1; }
    | WHILE LEFT_PAREN expr RIGHT_PAREN stmt { $$ = std::make_unique<WhileStmtNode>(Loc(@1), $3, $5); }
    | DO stmt WHILE LEFT_PAREN expr RIGHT_PAREN SEMICOLON { $$ = std::make_unique<WhileStmtNode>(Loc(@1), $5, $2, true); }
    | FOR LEFT_PAREN loop_init expr_opt SEMICOLON expr_opt RIGHT_PAREN stmt { $$ = std::make_unique<ForStmtNode>(Loc(@1), $3, $4, $6, $8); }
    | jump_stmt { $$ = $1; }
    ;

/* 6.8.1 Labeled statements */
labeled_stmt: ID COLON stmt { $$ = std::make_unique<IdLabeledStmtNode>(Loc(@1), $1, $3); }
    /* TODO: constant expression */
    | CASE expr COLON stmt { $$ = std::make_unique<CaseStmtNode>(Loc(@1), $2, $4); }
    | DEFAULT COLON stmt { $$ = std::make_unique<DefaultStmtNode>(Loc(@1), $3); }
    ;

/* 6.8.4 Selection statements */
selection_stmt: IF LEFT_PAREN expr RIGHT_PAREN stmt %prec IF_WITHOUT_ELSE { $$ = std::make_unique<IfStmtNode>(Loc(@1), $3, $5); }
    | IF LEFT_PAREN expr RIGHT_PAREN stmt ELSE stmt { $$ = std::make_unique<IfStmtNode>(Loc(@1), $3, $5, $7); }
    | SWITCH LEFT_PAREN expr RIGHT_PAREN stmt { $$ = std::make_unique<SwitchStmtNode>(Loc(@1), $3, $5); }
    ;

/* 6.8.6 Jump statements */
jump_stmt: RETURN expr SEMICOLON { $$ = std::make_unique<ReturnStmtNode>(Loc(@1), $2); }
    | BREAK SEMICOLON { $$ = std::make_unique<BreakStmtNode>(Loc(@1)); }
    | CONTINUE SEMICOLON { $$ = std::make_unique<ContinueStmtNode>(Loc(@1)); }
    | GOTO ID SEMICOLON { $$ = std::make_unique<GotoStmtNode>(Loc(@1), $2); }
    ;

loop_init: decl { $$ = std::make_unique<LoopInitNode>(Loc(@1), $1); }
    | expr_opt SEMICOLON { $$ = std::make_unique<LoopInitNode>(Loc(@1), $1); }
    ;

expr_opt: expr { $$ = $1; }
    | epsilon { $$ = std::make_unique<NullExprNode>(Loc(@1)); }
    ;

expr: unary_expr { $$ = $1; }
  /* additive 6.5.6 */
  | expr PLUS expr { $$ = std::make_unique<BinaryExprNode>(Loc(@2), BinaryOperator::kAdd, $1, $3); }
  | expr MINUS expr { $$ = std::make_unique<BinaryExprNode>(Loc(@2), BinaryOperator::kSub, $1, $3); }
  /* multiplicative 6.5.5 */
  | expr STAR expr { $$ = std::make_unique<BinaryExprNode>(Loc(@2), BinaryOperator::kMul, $1, $3); }
  | expr DIV expr { $$ = std::make_unique<BinaryExprNode>(Loc(@2), BinaryOperator::kDiv, $1, $3); }
  | expr MOD expr { $$ = std::make_unique<BinaryExprNode>(Loc(@2), BinaryOperator::kMod, $1, $3); }
  /* relational 6.5.8 */
  | expr GT expr { $$ = std::make_unique<BinaryExprNode>(Loc(@2), BinaryOperator::kGt, $1, $3); }
  | expr LT expr { $$ = std::make_unique<BinaryExprNode>(Loc(@2), BinaryOperator::kLt, $1, $3); }
  | expr GE expr { $$ = std::make_unique<BinaryExprNode>(Loc(@2), BinaryOperator::kGte, $1, $3); }
  | expr LE expr { $$ = std::make_unique<BinaryExprNode>(Loc(@2), BinaryOperator::kLte, $1, $3); }
  /* equality 6.5.9 */
  | expr EQ expr { $$ = std::make_unique<BinaryExprNode>(Loc(@2), BinaryOperator::kEq, $1, $3); }
  | expr NE expr { $$ = std::make_unique<BinaryExprNode>(Loc(@2), BinaryOperator::kNeq, $1, $3); }
  | assign_expr { $$ = $1; }
  ;

/* assignment 6.5.16 */
/* TODO: support multiple assignment operators */
assign_expr: unary_expr ASSIGN expr {
    $$ = std::make_unique<SimpleAssignmentExprNode>(Loc(@2), $1, $3);
  }
  ;

/* 6.5.3 Unary operators */
unary_expr: postfix_expr { $$ = $1; }
  | INCR unary_expr { $$ = std::make_unique<UnaryExprNode>(Loc(@1), UnaryOperator::kIncr, $2); }
  | DECR unary_expr { $$ = std::make_unique<UnaryExprNode>(Loc(@1), UnaryOperator::kDecr, $2); }
  | PLUS unary_expr { $$ = std::make_unique<UnaryExprNode>(Loc(@1), UnaryOperator::kPos, $2); }
  | MINUS unary_expr { $$ = std::make_unique<UnaryExprNode>(Loc(@1), UnaryOperator::kNeg, $2); }
  | EXCLAMATION unary_expr { $$ = std::make_unique<UnaryExprNode>(Loc(@1), UnaryOperator::kNot, $2); }
  | AMPERSAND unary_expr { $$ = std::make_unique<UnaryExprNode>(Loc(@1), UnaryOperator::kAddr, $2); }
  | STAR unary_expr { $$ = std::make_unique<UnaryExprNode>(Loc(@1), UnaryOperator::kDeref, $2); }
  | TILDE unary_expr { $$ = std::make_unique<UnaryExprNode>(Loc(@1), UnaryOperator::kBitComp, $2); }
  /* TODO: sizeof */
  ;

/* 6.5.2 Postfix operators */
postfix_expr: primary_expr { $$ = $1; }
  | postfix_expr LEFT_PAREN arg_list_opt RIGHT_PAREN { $$ = std::make_unique<FuncCallExprNode>(Loc(@1), $1, $3); }
  | postfix_expr LEFT_SQUARE expr RIGHT_SQUARE { $$ = std::make_unique<ArrSubExprNode>(Loc(@1), $1, $3); }
  ;

arg_list_opt: arg_list { $$ = $1; }
  | epsilon { $$ = std::vector<std::unique_ptr<ArgExprNode>>{}; }
  ;

arg_list: arg_list COMMA arg {
    auto arg_list = $1;
    arg_list.push_back($3);
    $$ = std::move(arg_list);
  }
  | arg {
    $$ = std::vector<std::unique_ptr<ArgExprNode>>{};
    $$.push_back($1);
  }
  ;

arg: expr {
    $$ = std::make_unique<ArgExprNode>(Loc(@1), $1);
  }
  ;

primary_expr: ID { $$ = std::make_unique<IdExprNode>(Loc(@1), $1); }
  | NUM { $$ = std::make_unique<IntConstExprNode>(Loc(@1), $1); }
  | LEFT_PAREN expr RIGHT_PAREN { $$ = $2; }
  ;

/* 6.7 Declarations */
/* TODO: init declarator list */
decl: declaration_specifiers init_declarator_opt SEMICOLON {
    // Find the innermost type and replace it with the type specifier.
    auto decl = $2;
    // This is a handle to the type of the declarator.
    std::unique_ptr<Type>* type = &(decl->type);
    while (!(*type)->IsPrim()) {
      if ((*type)->IsPtr()) {
        type = &(static_cast<PtrType*>(type)->base_type);
      } else if ((*type)->IsArr()) {
        type = &(static_cast<ArrType*>(type)->element_type);
      }
    }
    *type = $1;
    $$ = decl;
  }
  ;

/* A declaration specifier declares part of the type of a declarator. */
/* TODO: storage class specifier, type qualifier, function specifier */
declaration_specifiers: type_specifier declaration_specifiers
  | type_specifier
  ;

init_declarator_opt: init_declarator
  | epsilon
  ;

/* A init declarator is a declarator with an optional initializer. */
init_declarator: declarator
  | declarator ASSIGN initializer
  ;


/* 6.7.2 Type specifiers */
/* TODO: support multiple data types */
type_specifier: INT
  /* TODO: struct or union specifier */
  /* TODO: enum specifier */
  /* TODO: typedef name */
  ;

/* 6.7.5 Declarators */
/* A declarator declares an identifier, and may be followed by a single
   dimension of an array, or the parameters of a function.
   Furthermore,
   (1) if the declared identifier is a function, it doesn't contain the return type;
   (2) if it's a pointer, it doesn't contain the base type;
   (3) if it's an array, it doesn't contain the element type;
   etc. */
declarator: pointer_opt direct_declarator
  ;

direct_declarator: ID
  | LEFT_PAREN declarator RIGHT_PAREN
  /* array */
  | direct_declarator LEFT_SQUARE NUM RIGHT_SQUARE
  /* function */
  | direct_declarator LEFT_PAREN parameter_type_list_opt RIGHT_PAREN
  /* TODO: identifier list */
  /* The identifier may be a type name. */
  /* TODO: direct declarator ( identifier list ) */
  ;

pointer_opt: pointer
  | epsilon
  ;

pointer: STAR
  | pointer STAR
  ;

parameter_type_list_opt: parameter_type_list
  | epsilon
  ;

parameter_type_list: parameter_list
  /* TODO: parameter list, ... */
  ;

parameter_list: parameter_declaration
  | parameter_list COMMA parameter_declaration
  ;

parameter_declaration: declaration_specifiers declarator
  /* Declare parameters without identifiers. */
  | declaration_specifiers abstract_declarator_opt
  ;

abstract_declarator_opt: abstract_declarator
  | epsilon
  ;

/* 6.7.6 Type names */
/* NOTE: abstract means the declarator does not have an identifier */
abstract_declarator: pointer
  | pointer_opt direct_abstract_declarator
  ;

direct_abstract_declarator: LEFT_PAREN abstract_declarator RIGHT_PAREN
  | direct_abstract_declarator_opt LEFT_SQUARE NUM RIGHT_SQUARE
  | direct_abstract_declarator_opt LEFT_PAREN parameter_type_list_opt RIGHT_PAREN
  ;

direct_abstract_declarator_opt: direct_abstract_declarator
  | epsilon
  ;

/* 6.7.8 Initialization */
/* The current object shall have array type and the expression shall be an integer constant expression. */
initializer: LEFT_CURLY initializer_list comma_opt RIGHT_CURLY
  | expr
  ;

/* TODO: the initializer may be nested */
initializer_list: expr
  | initializer_list COMMA expr
  ;

comma_opt: COMMA
  | epsilon
  ;

epsilon: %empty;
%%

void yy::parser::error(const yy::location& loc, const std::string& err) {
  std::cerr << loc << ": " << err << std::endl;
}
