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
%token COMMA DOT ARROW SEMICOLON COLON
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
%token STRUCT UNION
// increment (INCR: ++) and decrement (DECR: --)
%token INCR DECR
%token LOGIC_OR LOGIC_AND OR XOR
%token SHIFT_LEFT SHIFT_RIGHT
%token EOF 0

%nterm <std::unique_ptr<ExprNode>> expr assign_expr expr_opt unary_expr postfix_expr primary_expr
%nterm <std::unique_ptr<ExprNode>> const_expr cond_expr logic_or_expr logic_and_expr inclusive_or_expr exclusive_or_expr
%nterm <std::unique_ptr<ExprNode>> and_expr eq_expr relational_expr shift_expr add_expr mul_expr cast_expr
%nterm <std::unique_ptr<DeclNode>> id_opt
%nterm <std::unique_ptr<DeclStmtNode>> decl
%nterm <std::unique_ptr<ParamNode>> parameter_declaration
%nterm <std::vector<std::unique_ptr<ParamNode>>> parameter_type_list_opt parameter_type_list parameter_list
%nterm <std::unique_ptr<FieldNode>> struct_declaration struct_declarator struct_declarator_list
%nterm <std::vector<std::unique_ptr<FieldNode>>> struct_declaration_list
// The followings also declare an identifier, however, their types are not yet fully resolved.
%nterm <std::unique_ptr<DeclNode>> declarator direct_declarator init_declarator
%nterm <std::vector<std::unique_ptr<DeclNode>>> init_declarator_list_opt init_declarator_list
// The abstract declarator is a declarator without an identifier, which are actually types.
%nterm <std::unique_ptr<Type>> abstract_declarator_opt abstract_declarator direct_abstract_declarator_opt direct_abstract_declarator
%nterm <std::unique_ptr<Type>> struct_or_union specifier_qualifier_list
// Type specifier can be a primitive type (int) or an user defined type (struct, union)
// The followings also construct types, but they are not yet fully resolved.
%nterm <std::variant<std::unique_ptr<Type>, std::unique_ptr<DeclNode>>> type_specifier declaration_specifiers struct_or_union_specifier
// The number of '*'s.
%nterm <int> pointer_opt pointer
// The initializer of a simple variable is an expression, whereas that of an array or complex object is a list of expressions.
%nterm <std::variant<std::unique_ptr<InitExprNode>, std::vector<std::unique_ptr<InitExprNode>>>> initializer
%nterm <std::vector<std::unique_ptr<InitExprNode>>> initializer_list
%nterm <std::unique_ptr<DesNode>> designator
%nterm <std::vector<std::unique_ptr<DesNode>>> designator_list designation_opt
%nterm <std::unique_ptr<ArgExprNode>> arg
%nterm <std::vector<std::unique_ptr<ArgExprNode>>> arg_list_opt arg_list
%nterm <std::unique_ptr<FuncDefNode>> func_def
%nterm <std::vector<std::unique_ptr<FuncDefNode>>> func_def_list_opt
%nterm <std::unique_ptr<LoopInitNode>> loop_init
%nterm <std::unique_ptr<StmtNode>> stmt jump_stmt selection_stmt labeled_stmt block_item
%nterm <std::unique_ptr<CompoundStmtNode>> compound_stmt
%nterm <std::vector<std::unique_ptr<StmtNode>>> block_item_list block_item_list_opt

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
    auto type = std::get<std::unique_ptr<Type>>($1);
    auto resolved_return_type = ResolveType(std::move(type), func_type->return_type().Clone());
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
    $$ = std::vector<std::unique_ptr<StmtNode>>{};
  }
  ;

block_item_list: block_item {
    $$ = std::vector<std::unique_ptr<StmtNode>>{};
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
expr: assign_expr { $$ = $1; }
    | expr COMMA assign_expr { $$ = std::make_unique<BinaryExprNode>(Loc(@2), BinaryOperator::kComma, $1, $3); }
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
  /* 6.5.2.3 Structure and union members */
  | postfix_expr DOT ID { $$ = std::make_unique<RecordMemExprNode>(Loc(@1), PostfixOperator::kDot, $1, $3); }
  | postfix_expr ARROW ID { $$ = std::make_unique<RecordMemExprNode>(Loc(@1), PostfixOperator::kArrow, $1, $3); }
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
  | shift_expr SHIFT_LEFT add_expr { $$ = std::make_unique<BinaryExprNode>(Loc(@2), BinaryOperator::kShl, $1, $3); }
  | shift_expr SHIFT_RIGHT add_expr { $$ = std::make_unique<BinaryExprNode>(Loc(@2), BinaryOperator::kShr, $1, $3); }
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
and_expr: eq_expr { $$ = $1; }
  | and_expr AMPERSAND eq_expr { $$ = std::make_unique<BinaryExprNode>(Loc(@2), BinaryOperator::kAnd, $1, $3); }
  ;

/* 6.5.11 Bitwise exclusive OR operators */
exclusive_or_expr: and_expr { $$ = $1; }
  | exclusive_or_expr XOR and_expr { $$ = std::make_unique<BinaryExprNode>(Loc(@2), BinaryOperator::kXor, $1, $3); }
  ;

/* 6.5.12 Bitwise inclusive OR operators */
inclusive_or_expr: exclusive_or_expr { $$ = $1; }
  | inclusive_or_expr OR exclusive_or_expr { $$ = std::make_unique<BinaryExprNode>(Loc(@2), BinaryOperator::kOr, $1, $3); }
  ;

/* 6.5.13 Logical AND operators */
logic_and_expr: inclusive_or_expr { $$ = $1; }
  | logic_and_expr LOGIC_AND inclusive_or_expr { $$ = std::make_unique<BinaryExprNode>(Loc(@2), BinaryOperator::kLand, $1, $3); }
  ;

/* 6.5.14 Logical OR operators */
logic_or_expr: logic_and_expr { $$ = $1; }
  | logic_or_expr LOGIC_OR logic_and_expr { $$ = std::make_unique<BinaryExprNode>(Loc(@2), BinaryOperator::kLor, $1, $3); }
  ;

/* 6.5.15 Conditional operators */
cond_expr: logic_or_expr { $$ = $1; }
  | logic_or_expr QUESTION expr COLON cond_expr { $$ = std::make_unique<CondExprNode>(Loc(@2), $1, $3, $5); }
  ;

/* 6.5.16 Assignment operators */
/* TODO: support multiple assignment operators */
assign_expr: cond_expr { $$ = $1; }
  | unary_expr ASSIGN assign_expr { $$ = std::make_unique<SimpleAssignmentExprNode>(Loc(@2), $1, $3); }
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

arg: assign_expr {
    $$ = std::make_unique<ArgExprNode>(Loc(@1), $1);
  }
  ;

/* 6.7 Declarations */
/* Declaration specifiers can be either a 'type' or a 'declaration of type'. */
decl: declaration_specifiers init_declarator_list_opt SEMICOLON {
    auto decl_specifiers = $1;
    auto init_decl_list = $2;
    // A single declaration may declare multiple identifiers.
    auto decl_list = std::vector<std::unique_ptr<DeclNode>>{};
    if (std::holds_alternative<std::unique_ptr<Type>>(decl_specifiers)) {
      auto type = std::move(std::get<std::unique_ptr<Type>>(decl_specifiers));
      if (init_decl_list.empty()) {
        // A stand-alone type that doesn't declare any identifier, e.g., `int;`.
        decl_list.push_back(std::make_unique<VarDeclNode>(Loc(@1), "", type->Clone()));
      }

      for (auto& init_decl : init_decl_list) {
        if (init_decl) {
          init_decl->type = ResolveType(type->Clone(), std::move(init_decl->type));
        } else { // unnamed primitive type
          init_decl = std::make_unique<VarDeclNode>(Loc(@1), "", type->Clone());
        }
        decl_list.push_back(std::move(init_decl));
      }
    } else {
      auto decl = std::move(std::get<std::unique_ptr<DeclNode>>(decl_specifiers));
      // A record declaration that doesn't declare any identifier, e.g., `struct point {int x, int y};`.
      if (init_decl_list.empty()) {
        decl_list.push_back(std::move(decl));
      }

      auto* rec_decl = dynamic_cast<RecordDeclNode*>(decl.get());
      // Initialize record variable.
      for (auto& init_decl : init_decl_list) {
        if (init_decl) {
          init_decl->type = ResolveType(rec_decl->type->Clone(), std::move(init_decl->type));
        }
        decl_list.push_back(std::move(init_decl));
      }
    }
    $$ = std::make_unique<DeclStmtNode>(Loc(@1), std::move(decl_list));
  }
  ;

init_declarator_list_opt: init_declarator_list { $$ = $1; }
  | epsilon { $$ = std::vector<std::unique_ptr<DeclNode>>{}; }
  ;

init_declarator_list: init_declarator {
    $$ = std::vector<std::unique_ptr<DeclNode>>{};
    $$.push_back($1);
  }
  | init_declarator_list COMMA init_declarator {
    auto init_decl_list = $1;
    init_decl_list.push_back($3);
    $$ = std::move(init_decl_list);
  }
  ;

/* A declaration specifier declares part of the type of a declarator. */
/* TODO: storage class specifier, type qualifier, function specifier */
declaration_specifiers: type_specifier declaration_specifiers {
    // Leave unimplemented; useless without support of other specifiers.
    $$ = $1;
  }
  | type_specifier { $$ = $1; }
  ;

/* A init declarator is a declarator with an optional initializer. */
init_declarator: declarator { $$ = $1; }
  | declarator ASSIGN initializer {
    // NOTE: The parser crashes when initializing a variable with a list of expressions.
    auto decl = $1;
    auto init = $3;
    if (std::holds_alternative<std::unique_ptr<InitExprNode>>(init)) {
      auto* var_decl = dynamic_cast<VarDeclNode*>(decl.get());
      assert(var_decl);
      auto initializer = std::move(std::get<std::unique_ptr<InitExprNode>>(init));
      var_decl->init = std::move(initializer->expr);
    } else { // The initializer is a list of expressions.
      auto init_expr_list = std::move(std::get<std::vector<std::unique_ptr<InitExprNode>>>(init));
      if (auto* arr_decl = dynamic_cast<ArrDeclNode*>(decl.get())) {
        // Declares an array variable.
        arr_decl->init_list = std::move(init_expr_list);
      } else if (auto* var_decl = dynamic_cast<VarDeclNode*>(decl.get())) {
        // Declares a struct or union variable.
        decl = std::make_unique<RecordVarDeclNode>(Loc(@1),
                                         std::move(var_decl->id),
                                         std::move(var_decl->type),
                                         std::move(init_expr_list));
      }
    }
    $$ = std::move(decl);
  }
  ;


/* 6.7.2 Type specifiers */
/* TODO: support multiple data types */
type_specifier: INT { $$ = std::make_unique<PrimType>(PrimitiveType::kInt); }
  | struct_or_union_specifier { $$ = $1; }
  /* TODO: enum specifier */
  /* TODO: typedef name */
  ;

struct_or_union_specifier: struct_or_union id_opt LEFT_CURLY struct_declaration_list RIGHT_CURLY {
    // Field types for variable 'type' are unknown until now.
    auto type = $1;
    auto decl_id = $2;
    auto field_list = $4;
    auto field_types = std::vector<std::unique_ptr<Type>>{};
    for (const auto& field : field_list) {
      field_types.push_back(field->type->Clone());
    }

    auto type_id = decl_id ? decl_id->id : "";
    if (type->IsStruct()) {
      type = std::make_unique<StructType>(type_id, std::move(field_types));
    } else {
      type = std::make_unique<UnionType>(type_id, std::move(field_types));
    }

    $$ = std::make_unique<RecordDeclNode>(Loc(@2), std::move(type_id), std::move(type), std::move(field_list));
  }
  | struct_or_union ID {
    auto type = $1;
    auto decl_id = $2;
    auto field_list = std::vector<std::unique_ptr<FieldNode>>{};
    auto field_types = std::vector<std::unique_ptr<Type>>{};

    if (type->IsStruct()) {
      type = std::make_unique<StructType>(decl_id, std::move(field_types));
    } else {
      type = std::make_unique<UnionType>(decl_id, std::move(field_types));
    }

    $$ = std::make_unique<RecordDeclNode>(Loc(@2), std::move(decl_id), std::move(type), std::move(field_list));
  }
  ;

struct_declaration_list: struct_declaration {
    $$ = std::vector<std::unique_ptr<FieldNode>>{};
    $$.push_back($1);
  }
  | struct_declaration_list struct_declaration {
    $$ = $1;
    $$.push_back($2);
  }
  ;

/* TODO: struct_declarator_list_opt */
struct_declaration: specifier_qualifier_list struct_declarator_list SEMICOLON {
    auto type = $1;
    auto decl = $2;
    decl->type = ResolveType(std::move(type), std::move(decl->type));
    $$ = std::move(decl);
  }
  ;

struct_declarator_list: struct_declarator { $$ = $1; }
  | struct_declarator_list COMMA struct_declarator
  ;

/* TODO: declarator_opt COLON const_expr */
struct_declarator: declarator {
    auto decl = $1;
    $$ = std::make_unique<FieldNode>(Loc(@1), std::move(decl->id), std::move(decl->type));
  }
  ;

/* TODO: type_qualifier specifier_qualifier_list_opt */
specifier_qualifier_list: type_specifier {
    $$ = std::move(std::get<std::unique_ptr<Type>>($1));
  }
  ;

/* id_opt is used for struct, union, enum. */
id_opt: ID {
    auto type = std::make_unique<PrimType>(PrimitiveType::kUnknown);
    $$ = std::make_unique<VarDeclNode>(Loc(@1), $1, std::move(type));
  }
  | epsilon { $$ = nullptr; }
  ;

struct_or_union: STRUCT {
    auto field_types = std::vector<std::unique_ptr<Type>>{};
    $$ = std::make_unique<StructType>("", std::move(field_types));
  }
  | UNION {
    auto field_types = std::vector<std::unique_ptr<Type>>{};
    $$ = std::make_unique<UnionType>("", std::move(field_types));
  }
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
    $$ = std::make_unique<VarDeclNode>(Loc(@1), $1, std::move(type));
  }
  | LEFT_PAREN declarator RIGHT_PAREN {
    @$ = @2; // Set the location to the identifier.
    $$ = $2;
  }
  /* array */
  | direct_declarator LEFT_SQUARE NUM RIGHT_SQUARE {
    auto declarator = $1;
    auto type = std::make_unique<ArrType>(std::move(declarator->type), $3);
    if (!dynamic_cast<ArrDeclNode*>(declarator.get())) {
      // If the declarator is not yet a array declarator, we need to construct one.
      $$ = std::make_unique<ArrDeclNode>(Loc(@1), declarator->id, std::move(type), std::vector<std::unique_ptr<InitExprNode>>{});
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
    auto type = std::move(std::get<std::unique_ptr<Type>>($1));
    auto decl = $2;
    auto resolved_type = ResolveType(std::move(type), std::move(decl->type));
    $$ = std::make_unique<ParamNode>(Loc(@2), std::move(decl->id), std::move(resolved_type));
  }
  /* Declare parameters without identifiers. */
  | declaration_specifiers abstract_declarator_opt {
    // XXX: The identifier is empty.
    auto type = std::get<std::unique_ptr<Type>>($1);
    $$ = std::make_unique<ParamNode>(Loc(@1), /* id */ "", ResolveType(std::move(type), $2));
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
  /* e.g., (*)(int, int) */
  | direct_abstract_declarator_opt LEFT_PAREN parameter_type_list_opt RIGHT_PAREN {
    auto params = $3;
    auto param_types = std::vector<std::unique_ptr<Type>>{};
    for (const auto& param : params) {
      param_types.push_back(param->type->Clone());
    }
    auto func_type = std::make_unique<FuncType>(std::make_unique<PrimType>(PrimitiveType::kUnknown), std::move(param_types));
    $$ = ResolveType(std::move(func_type), $1);
  }
  ;

direct_abstract_declarator_opt: direct_abstract_declarator { $$ = $1; }
  | epsilon { $$ = std::make_unique<PrimType>(PrimitiveType::kUnknown); }
  ;

/* 6.7.8 Initialization */
initializer: LEFT_CURLY initializer_list comma_opt RIGHT_CURLY { $$ = $2; }
  | assign_expr { $$ = std::make_unique<InitExprNode>(Loc(@1), std::vector<std::unique_ptr<DesNode>>{}, $1); }
  ;

/* TODO: the initializer may be nested (change assign_expr to initializer) */
initializer_list: designation_opt assign_expr {
    auto init = std::make_unique<InitExprNode>(Loc(@1), $1, $2);
    $$ = std::vector<std::unique_ptr<InitExprNode>>{};
    $$.push_back(std::move(init));
  }
  | initializer_list COMMA designation_opt assign_expr {
    auto initializer_list = $1;
    auto init = std::make_unique<InitExprNode>(Loc(@1), $3, $4);
    initializer_list.push_back(std::move(init));
    $$ = std::move(initializer_list);
  }
  ;

designation_opt: designator_list ASSIGN { $$ = $1; }
  | epsilon { $$ = std::vector<std::unique_ptr<DesNode>>{}; }
  ;

designator_list: designator {
    auto designator_list = std::vector<std::unique_ptr<DesNode>>{};
    designator_list.push_back($1);
    $$ = std::move(designator_list);
  }
  | designator_list designator {
    $$ = $1;
    $$.push_back($2);
  }
  ;

designator: LEFT_SQUARE const_expr RIGHT_SQUARE { $$ = std::make_unique<ArrDesNode>(Loc(@2), $2); }
  | DOT ID { $$ = std::make_unique<IdDesNode>(Loc(@2), $2); }
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
