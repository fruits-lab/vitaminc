%{

#include <cassert>
#include <iostream>
#include <memory>
#include <utility>
#include <vector>

#include "ast.hpp"
#include "location.hpp"
#include "operator.hpp"
#include "type.hpp"

namespace {
/// @brief Resolves `unknown_type` with `resolved_type`, forming a new type with
/// `resolved_type` as the inner type and `unknown_type` as the outer type,
/// wrapping `resolved_type`.
/// @param resolved_type The type usually already resolved, to be wrapped by
/// `unknown_type`.
/// @param unknown_type The type to be resolved. It's innermost type
/// must be the unknown type.
/// @example If `unknown_type` is `unknown* []` (outer) and `resolved_type` is
/// `int` (inner), the resolved type is `int* []` (inner outer).
std::unique_ptr<Type> ResolveType(std::unique_ptr<Type> resolved_type,
                                  std::unique_ptr<Type> unknown_type);
}

%}

// Dependency code required for the value and location types;
// inserts verbatim to the header file.
%code requires {
  #include <memory>
  #include <string>
  #include <variant>
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
%token EXCLAMATION TILDE AMPERSAND QUESTION
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
%token LOGIC_OR LOGIC_AND OR XOR
%token SHIFT_LEFT SHIFT_RIGHT
%token EOF 0

%nterm <std::unique_ptr<ExprNode>> expr assign_expr expr_opt unary_expr postfix_expr primary_expr
%nterm <std::unique_ptr<ExprNode>> const_expr cond_expr logic_or_expr logic_and_expr inclusive_or_expr exclusive_or_expr
%nterm <std::unique_ptr<ExprNode>> and_expr eq_expr relational_expr shift_expr add_expr mul_expr cast_expr
%nterm <std::unique_ptr<DeclNode>> decl
%nterm <std::unique_ptr<ParamNode>> parameter_declaration
%nterm <std::vector<std::unique_ptr<ParamNode>>> parameter_type_list_opt parameter_type_list parameter_list
// The followings also declare an identifier, however, their types are not yet fully resolved.
%nterm <std::unique_ptr<DeclNode>> declarator direct_declarator init_declarator
// The abstract declarator is a declarator without an identifier, which are actually types.
%nterm <std::unique_ptr<Type>> abstract_declarator_opt abstract_declarator direct_abstract_declarator_opt direct_abstract_declarator
%nterm <std::unique_ptr<Type>> type_specifier
// The followings also construct types, but they are not yet fully resolved.
%nterm <std::unique_ptr<Type>> declaration_specifiers
// The number of '*'s.
%nterm <int> pointer_opt pointer
// The initializer of a simple variable is an expression, whereas that of an array or complex object is a list of expressions.
%nterm <std::variant<std::unique_ptr<ExprNode>, std::vector<std::unique_ptr<ExprNode>>>> initializer
%nterm <std::vector<std::unique_ptr<ExprNode>>> initializer_list
%nterm <std::unique_ptr<ArgExprNode>> arg
%nterm <std::vector<std::unique_ptr<ArgExprNode>>> arg_list_opt arg_list
%nterm <std::unique_ptr<FuncDefNode>> func_def
%nterm <std::vector<std::unique_ptr<FuncDefNode>>> func_def_list_opt
%nterm <std::unique_ptr<LoopInitNode>> loop_init
%nterm <std::unique_ptr<StmtNode>> stmt jump_stmt selection_stmt labeled_stmt
%nterm <std::unique_ptr<CompoundStmtNode>> compound_stmt
%nterm <std::vector<CompoundStmtNode::Item>> block_item_list block_item_list_opt
%nterm <CompoundStmtNode::Item> block_item

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
func_def: declaration_specifiers declarator compound_stmt {
    // The declarator shall already be a function declarator.
    // We resolve its return type with the declaration specifiers and set the body.
    auto func_def = $2;
    assert(dynamic_cast<FuncDefNode*>(func_def.get()));
    assert(func_def->type->IsFunc());
    const auto* func_type = static_cast<FuncType*>(func_def->type.get());
    auto resolved_return_type = ResolveType($1, func_type->return_type().Clone());
    auto param_types = std::vector<std::unique_ptr<Type>>{};
    for (auto& param : func_type->param_types()) {
      param_types.push_back(param->Clone());
    }
    func_def->type = std::make_unique<FuncType>(std::move(resolved_return_type), std::move(param_types));
    static_cast<FuncDefNode*>(func_def.get())->body = $3;
    $$ = std::unique_ptr<FuncDefNode>(static_cast<FuncDefNode*>(func_def.release()));
  }
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
    | CASE const_expr COLON stmt { $$ = std::make_unique<CaseStmtNode>(Loc(@1), $2, $4); }
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

/* 6.5 Expressions */
/* TODO: implement "expr , assign_expr" */
expr: assign_expr { $$ = $1; }
    ;

/* 6.5.1 Primary expressions */
primary_expr: ID { $$ = std::make_unique<IdExprNode>(Loc(@1), $1); }
  | NUM { $$ = std::make_unique<IntConstExprNode>(Loc(@1), $1); }
  | LEFT_PAREN expr RIGHT_PAREN { $$ = $2; }
  ;

/* 6.5.2 Postfix operators */
postfix_expr: primary_expr { $$ = $1; }
  | postfix_expr LEFT_PAREN arg_list_opt RIGHT_PAREN { $$ = std::make_unique<FuncCallExprNode>(Loc(@1), $1, $3); }
  | postfix_expr LEFT_SQUARE expr RIGHT_SQUARE { $$ = std::make_unique<ArrSubExprNode>(Loc(@1), $1, $3); }
  /* 6.5.2.4 Postfix increment and decrement operators */
  | postfix_expr INCR { $$ = std::make_unique<PostfixArithExprNode>(Loc(@1), PostfixOperator::kIncr, $1); }
  | postfix_expr DECR { $$ = std::make_unique<PostfixArithExprNode>(Loc(@1), PostfixOperator::kDecr, $1); }
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

/* 6.5.4 Cast operators*/
/* TODO: cast type */
cast_expr: unary_expr { $$ = $1; }
  ;

/* 6.5.5 Multiplicative operators */
mul_expr: cast_expr { $$ = $1; }
  | mul_expr STAR cast_expr { $$ = std::make_unique<BinaryExprNode>(Loc(@2), BinaryOperator::kMul, $1, $3); }
  | mul_expr DIV cast_expr { $$ = std::make_unique<BinaryExprNode>(Loc(@2), BinaryOperator::kDiv, $1, $3); }
  | mul_expr MOD cast_expr { $$ = std::make_unique<BinaryExprNode>(Loc(@2), BinaryOperator::kMod, $1, $3); }
  ;

/* 6.5.6 Additive operators */
add_expr: mul_expr { $$ = $1; }
  | add_expr PLUS mul_expr { $$ = std::make_unique<BinaryExprNode>(Loc(@2), BinaryOperator::kAdd, $1, $3); }
  | add_expr MINUS mul_expr { $$ = std::make_unique<BinaryExprNode>(Loc(@2), BinaryOperator::kSub, $1, $3); }
  ;

/* 6.5.7 Bitwise shift operators */
shift_expr: add_expr { $$ = $1; }
  | shift_expr SHIFT_LEFT add_expr
  | shift_expr SHIFT_RIGHT add_expr
  ;

/* 6.5.8 Relational operators */
relational_expr: shift_expr { $$ = $1; }
  | relational_expr GT shift_expr { $$ = std::make_unique<BinaryExprNode>(Loc(@2), BinaryOperator::kGt, $1, $3); }
  | relational_expr LT shift_expr { $$ = std::make_unique<BinaryExprNode>(Loc(@2), BinaryOperator::kLt, $1, $3); }
  | relational_expr GE shift_expr { $$ = std::make_unique<BinaryExprNode>(Loc(@2), BinaryOperator::kGte, $1, $3); }
  | relational_expr LE shift_expr { $$ = std::make_unique<BinaryExprNode>(Loc(@2), BinaryOperator::kLte, $1, $3); }
  ;

/* 6.5.9 Equality operators */
eq_expr: relational_expr { $$ = $1; }
  | eq_expr EQ relational_expr { $$ = std::make_unique<BinaryExprNode>(Loc(@2), BinaryOperator::kEq, $1, $3); }
  | eq_expr NE relational_expr { $$ = std::make_unique<BinaryExprNode>(Loc(@2), BinaryOperator::kNeq, $1, $3); }
  ;

/* 6.5.10 Bitwise AND operators */
and_expr: eq_expr
  | and_expr AMPERSAND eq_expr
  ;

/* 6.5.11 Bitwise exclusive OR operators */
exclusive_or_expr: and_expr
  | exclusive_or_expr XOR and_expr
  ;

/* 6.5.12 Bitwise inclusive OR operators */
inclusive_or_expr: exclusive_or_expr
  | inclusive_or_expr OR exclusive_or_expr
  ;

/* 6.5.13 Logical AND operators */
logic_and_expr: inclusive_or_expr
  | logic_and_expr LOGIC_AND inclusive_or_expr
  ;

/* 6.5.14 Logical OR operators */
logic_or_expr: logic_and_expr
  | logic_or_expr LOGIC_OR logic_and_expr
  ;

/* 6.5.15 Conditional operators */
cond_expr: logic_or_expr
  | logic_or_expr QUESTION expr : cond_expr
  ;

/* 6.5.16 Assignment operators */
/* TODO: support multiple assignment operators */
assign_expr: cond_expr { $$ = $1; }
  | unary_expr ASSIGN expr { $$ = std::make_unique<SimpleAssignmentExprNode>(Loc(@2), $1, $3); }
  ;

/* 6.6 Constant Expressions*/
const_expr: cond_expr { $$ = $1; }
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

/* 6.7 Declarations */
/* TODO: init declarator list */
/* TODO: If the init declarator doesn't present, e.g., `int;`, the declaration is still valid. */
decl: declaration_specifiers init_declarator SEMICOLON {
    auto decl = $2;
    decl->type = ResolveType($1, std::move(decl->type));
    $$ = std::move(decl);
  }
  ;

/* A declaration specifier declares part of the type of a declarator. */
/* TODO: storage class specifier, type qualifier, function specifier */
declaration_specifiers: type_specifier declaration_specifiers {
    // Leave unimplemented; useless without support of other specifiers.
    $$ = nullptr;
  }
  | type_specifier { $$ = $1; }
  ;

/* A init declarator is a declarator with an optional initializer. */
init_declarator: declarator { $$ = $1; }
  | declarator ASSIGN initializer {
    // NOTE: The parser crashes when initializing a variable with a list of expressions.
    auto decl = $1;
    auto init = $3;
    if (std::holds_alternative<std::unique_ptr<ExprNode>>(init)) {
      auto* var_decl = dynamic_cast<DeclVarNode*>(decl.get());
      assert(var_decl);
      auto init_expr = std::move(std::get<std::unique_ptr<ExprNode>>(init));
      var_decl->init = std::move(init_expr);
    } else { // The initializer is a list of expressions.
      auto* arr_decl = dynamic_cast<DeclArrNode*>(decl.get());
      assert(arr_decl);
      auto init_expr_list = std::move(std::get<std::vector<std::unique_ptr<ExprNode>>>(init));
      arr_decl->init_list = std::move(init_expr_list);
    }
    $$ = std::move(decl);
  }
  ;


/* 6.7.2 Type specifiers */
/* TODO: support multiple data types */
type_specifier: INT { $$ = std::make_unique<PrimType>(PrimitiveType::kInt); }
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
declarator: pointer_opt direct_declarator {
    @$ = @2; // Set the location to the identifier.
    auto declarator = $2;
    for (int i = 0, e = $1; i < e; ++i) {
      auto unknown_ptr_type = std::make_unique<PtrType>(std::make_unique<PrimType>(PrimitiveType::kUnknown));
      declarator->type = ResolveType(std::move(unknown_ptr_type), std::move(declarator->type));
    }
    $$ = std::move(declarator);
  }
  ;

direct_declarator: ID {
    auto type = std::make_unique<PrimType>(PrimitiveType::kUnknown);
    $$ = std::make_unique<DeclVarNode>(Loc(@1), $1, std::move(type));
  }
  | LEFT_PAREN declarator RIGHT_PAREN {
    @$ = @2; // Set the location to the identifier.
    $$ = $2;
  }
  /* array */
  | direct_declarator LEFT_SQUARE NUM RIGHT_SQUARE {
    auto declarator = $1;
    auto type = std::make_unique<ArrType>(std::move(declarator->type), $3);
    if (!dynamic_cast<DeclArrNode*>(declarator.get())) {
      // If the declarator is not yet a array declarator, we need to construct one.
      $$ = std::make_unique<DeclArrNode>(Loc(@1), declarator->id, std::move(type), /* init list */ std::vector<std::unique_ptr<ExprNode>>{});
    } else {
      declarator->type = std::move(type);
      $$ = std::move(declarator);
    }
  }
  /* function */
  | direct_declarator LEFT_PAREN parameter_type_list_opt RIGHT_PAREN {
    auto decl = $1;
    auto params = $3;
    auto param_types = std::vector<std::unique_ptr<Type>>{};
    for (const auto& param : params) {
      param_types.push_back(param->type->Clone());
    }
    // The return type is unknown at this point.
    auto return_type = std::make_unique<PrimType>(PrimitiveType::kUnknown);
    auto type = std::make_unique<FuncType>(std::move(return_type), std::move(param_types));
    // If the direct declarator has a pointer type, this is a declaration of a function pointer, not a function.
    if (decl->type->IsPtr()) {
      decl->type = ResolveType(std::move(type), std::move(decl->type));
      $$ = std::move(decl);
    } else {
      $$ = std::make_unique<FuncDefNode>(Loc(@1), std::move(decl->id), std::move(params), /* body */ nullptr, std::move(type));
    }
  }
  /* TODO: identifier list */
  /* The identifier may be a type name. */
  /* TODO: direct declarator ( identifier list ) */
  ;

pointer_opt: pointer { $$ = $1; }
  | epsilon { $$ = 0; }
  ;

/* A pointer is a sequence of one or more '*'s. */
pointer: STAR { $$ = 1; }
  | pointer STAR { $$ = $1 + 1; }
  ;

parameter_type_list_opt: parameter_type_list { $$ = $1; }
  | epsilon { $$ = std::vector<std::unique_ptr<ParamNode>>{}; }
  ;

parameter_type_list: parameter_list { $$ = $1; }
  /* TODO: parameter list, ... */
  ;

parameter_list: parameter_declaration {
    $$ = std::vector<std::unique_ptr<ParamNode>>{};
    $$.push_back($1);
  }
  | parameter_list COMMA parameter_declaration {
    auto parameter_list = $1;
    parameter_list.push_back($3);
    $$ = std::move(parameter_list);
  }
  ;

parameter_declaration: declaration_specifiers declarator {
    auto decl = $2;
    auto resolved_type = ResolveType($1, std::move(decl->type));
    $$ = std::make_unique<ParamNode>(Loc(@2), std::move(decl->id), std::move(resolved_type));
  }
  /* Declare parameters without identifiers. */
  | declaration_specifiers abstract_declarator_opt {
    // XXX: The identifier is empty.
    $$ = std::make_unique<ParamNode>(Loc(@1), /* id */ "", ResolveType($1, $2));
  }
  ;

abstract_declarator_opt: abstract_declarator { $$ = $1; }
  | epsilon { $$ = std::make_unique<PrimType>(PrimitiveType::kUnknown); }
  ;

/* 6.7.6 Type names */
/* NOTE: abstract means the declarator does not have an identifier */
abstract_declarator: pointer {
    std::unique_ptr<Type> type = std::make_unique<PrimType>(PrimitiveType::kUnknown);
    for (int i = 0, e = $1; i < e; ++i) {
      type = std::make_unique<PtrType>(std::move(type)); 
    }     
    $$ = std::move(type);
  }
  | pointer_opt direct_abstract_declarator {
    @$ = @2; // Set the location to the identifier.
    auto type = $2;
    for (int i = 0, e = $1; i < e; ++i) {
      auto unknown_ptr_type = std::make_unique<PtrType>(std::move(type));
      type = ResolveType(std::move(unknown_ptr_type), std::make_unique<PrimType>(PrimitiveType::kUnknown));
    }
    $$ = std::move(type);
  }
  ;

direct_abstract_declarator: LEFT_PAREN abstract_declarator RIGHT_PAREN {
    @$ = @2;
    $$ = $2;
  }
  | direct_abstract_declarator_opt LEFT_SQUARE NUM RIGHT_SQUARE {
    $$ = std::make_unique<ArrType>($1, $3);
  }
  | direct_abstract_declarator_opt LEFT_PAREN parameter_type_list_opt RIGHT_PAREN {
    auto params = $3;
    auto param_types = std::vector<std::unique_ptr<Type>>{};
    for (const auto& param : params) {
      param_types.push_back(param->type->Clone());
    }
    $$ = std::make_unique<FuncType>($1, std::move(param_types));
  }
  ;

direct_abstract_declarator_opt: direct_abstract_declarator { $$ = $1; }
  | epsilon { $$ = std::make_unique<PrimType>(PrimitiveType::kUnknown); }
  ;

/* 6.7.8 Initialization */
/* The current object shall have array type and the expression shall be an integer constant expression. */
initializer: LEFT_CURLY initializer_list comma_opt RIGHT_CURLY { $$ = $2; }
  | expr { $$ = $1; }
  ;

/* TODO: the initializer may be nested */
initializer_list: expr {
    $$ = std::vector<std::unique_ptr<ExprNode>>{};
    $$.push_back($1);
  }
  | initializer_list COMMA expr {
    auto initializer_list = $1;
    initializer_list.push_back($3);
    $$ = std::move(initializer_list);
  }
  ;

comma_opt: COMMA
  | epsilon
  ;

epsilon: %empty;
%%

void yy::parser::error(const yy::location& loc, const std::string& err) {
  std::cerr << loc << ": " << err << std::endl;
}

namespace {

std::unique_ptr<Type> ResolveType(std::unique_ptr<Type> resolved_type,
                                  std::unique_ptr<Type> unknown_type) {
  // Base case: this type itself is the unknown type to resolve.
  if (unknown_type->IsPrim()) {
    assert(unknown_type->IsEqual(PrimitiveType::kUnknown));
    return resolved_type;
  }
  // Since we cannot change the internal state of a type, we construct a new one.
  if (unknown_type->IsPtr()) {
    auto ptr_type = static_cast<PtrType*>(unknown_type.get());
    resolved_type = ResolveType(std::move(resolved_type), ptr_type->base_type().Clone());
    return std::make_unique<PtrType>(std::move(resolved_type));
  }
  if (unknown_type->IsArr()) {
    auto arr_type = static_cast<ArrType*>(unknown_type.get());
    resolved_type = ResolveType(std::move(resolved_type), arr_type->element_type().Clone());
    return std::make_unique<ArrType>(std::move(resolved_type), arr_type->len());
  }
  if (unknown_type->IsFunc()) {
    // NOTE: Due to the structure of the grammar, the return type of a function is to be resolved.
    auto func_type = static_cast<FuncType*>(unknown_type.get());
    resolved_type = ResolveType(std::move(resolved_type), func_type->return_type().Clone());
    auto param_types = std::vector<std::unique_ptr<Type>>{};
    for (const auto& param : func_type->param_types()) {
      param_types.push_back(param->Clone());
    }
    return std::make_unique<FuncType>(std::move(resolved_type), std::move(param_types));
  }
  assert(false);
  return nullptr;
}

}
