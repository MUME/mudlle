/*
 * Copyright (c) 1993-2012 David Gay and Gustav Hållberg
 * All rights reserved.
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose, without fee, and without written agreement is hereby granted,
 * provided that the above copyright notice and the following two paragraphs
 * appear in all copies of this software.
 *
 * IN NO EVENT SHALL DAVID GAY OR GUSTAV HALLBERG BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF DAVID GAY OR
 * GUSTAV HALLBERG HAVE BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * DAVID GAY AND GUSTAV HALLBERG SPECIFICALLY DISCLAIM ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
 * FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS ON AN
 * "AS IS" BASIS, AND DAVID GAY AND GUSTAV HALLBERG HAVE NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */

%{
#include <stdlib.h>
#include <string.h>

#include "alloc.h"
#include "calloc.h"
#include "compile.h"
#include "mudlle.h"
#include "mparser.h"
#include "print.h"
#include "strbuf.h"
#include "tree.h"
#include "types.h"
#include "utils.h"

#define YYERROR_VERBOSE
%}

%union {
  struct str_and_len string;
  char *symbol;
  long integer;
  enum builtin_op operator;
  double mudlle_float;
  struct bigint_const *bigint;
  struct constant *tconstant;
  struct block *tblock;
  struct function *tfunction;
  struct clist *tclist;
  struct vlist *tvlist;
  struct cstlist *tcstlist;
  struct cstpair *tcstpair;
  struct component *tcomponent;
  enum mudlle_type tmtype;
  unsigned tmtypeset;           /* bitset of 1 << type_xxx */
  struct {
    bool varargs;
    char *var;
    struct vlist *args;
  } tparameters;
  struct mfile *tfile;
  struct pattern *tpattern;
  struct pattern_list *tpatternlist;
  struct match_node *tmatchnode;
  struct match_node_list *tmatchnodelist;
  enum builtin_op bop;
  struct {
    enum file_class fclass;
    char *name;
    struct vlist *defines, *requires;
  } tmodule_head;
  struct {
    bool space_suffix;
  } tcomma;
}

%locations

%token END 0          "end of input"

%token FORCE_CONSTANT

%token EQ             "=="
%token GE             ">="
%token GT             ">"
%token LE             "<="
%token LT             "<"
%token NE             "!="

%token DECREMENT      "--"
%token INCREMENT      "++"
%token SC_AND         "&&"
%token SC_OR          "||"
%token XOR            "^^"

%token SHIFT_LEFT     "<<"
%token SHIFT_RIGHT    ">>"

%token PATTERN_MATCH  "=>"
%token ELLIPSIS       "..."
%token SINK           "_"

%token ASSIGN         "="
%token ASSIGN_ADD     "+="
%token ASSIGN_SUB     "-="
%token ASSIGN_MUL     "*="
%token ASSIGN_DIV     "/="
%token ASSIGN_REM     "%="
%token ASSIGN_BIT_XOR "^="
%token ASSIGN_BIT_AND "&="
%token ASSIGN_BIT_OR  "|="
%token ASSIGN_SC_AND  "&&="
%token ASSIGN_XOR     "^^="
%token ASSIGN_SC_OR   "||="
%token ASSIGN_SHR     ">>="
%token ASSIGN_SHL     "<<="

%token ELSE           "else"
%token FOR            "for"
%token FUNCTION       "fn"
%token IF             "if"
%token LOOP           "loop"
%token MATCH          "match"
%token LOOP_EXIT      "exit"
%token WHILE          "while"

%token DEFINES        "defines"
%token REQUIRES       "requires"
%token LIBRARY        "library"
%token MODULE         "module"
%token READS          "reads"
%token WRITES         "writes"
%token STATIC         "static"

%token BIGINT         "bigint constant"
%token FLOAT          "floating-point constant"
%token INTEGER        "integer constant"
%token QUOTE          "'"
%token COMMA          ","
%token STRING         "string constant"
%token GLOBAL_SYMBOL  ":"
%token SYMBOL         "variable name"


%expect 1                       /* if-else */

%right '.'
%left XOR
%left SC_OR
%left SC_AND
%left EQ NE LT LE GT GE
%left '|' '^'
%left '&'
%left SHIFT_LEFT SHIFT_RIGHT
%left '+' '-'
%left '*' '/' '%'
%left '!' '~'

%type <tclist> expression_list expression_list1 call_list call_list1
%type <tcomponent> expression e0 e1 e2 e3 primary_expr loop exit match for
%type <tcomponent> labeled_expression unary_expr assign_expr sc_and_expr
%type <tcomponent> control_expression opt_match_condition optional_expression
%type <tcomponent> if while optional_else
%type <tvlist> variable_list plist plist1 optional_variable_list
%type <tvlist> requires defines reads writes statics
%type <tparameters> parameters
%type <tmtype> type
%type <tmtypeset> opt_typeset typeset typelist
%type <string> optional_help help STRING
%type <symbol> variable label optional_label optional_symbol variable_name
%type <symbol> SYMBOL GLOBAL_SYMBOL
%type <integer> INTEGER
%type <mudlle_float> FLOAT
%type <bigint> BIGINT
%type <integer> table_class
%type <tconstant> constant unary_constant simple_constant force_constant
%type <tconstant> constant_expr constant_expr_tail
%type <tconstant> optional_constant_tail table_entry string_constant
%type <tblock> code_block optional_implicit_code_block
%type <tfunction> function_expression
%type <tcstlist> constant_list optional_constant_list table_entry_list
%type <tfile> file simple module
%type <tmodule_head> module_head;
%type <tpattern> pattern pattern_list pattern_array pattern_atom
%type <tpattern> opt_pattern_list_tail pattern_atom_expr pattern_symbol
%type <tpatternlist> opt_pattern_sequence pattern_sequence
%type <tmatchnode> match_node
%type <tmatchnodelist> match_list
%type <operator> incrementer
%type <bop> op_assign
%type <tcomma> COMMA

%{

#include "lexer.h"

static struct mfile *parsed_code;
static struct constant *parsed_constant;
struct alloc_block *parser_memory;

static void yyerror(const char *s)
{
  if (!erred)
    compile_error("%s", s);
}

static void check_constant_expr(void)
{
  if (!allow_comma_expression())
    yyerror("comma-prefixed expression not allowed");
}

static void check_space(bool warn, const char *where)
{
  if (warn && !erred)
    compile_error("comma must not be followed by whitespace in %s", where);
}

/* true if c is a string constant */
static bool is_string(struct component *c)
{
  return c->vclass == c_constant && c->u.cst->vclass == cst_string;
}

/* true if c is an addition of two strings */
static bool is_string_add(struct component *c)
{
  return (c->vclass == c_builtin && c->u.builtin.fn == b_add
          && is_string(c->u.builtin.args->c)
          && is_string(c->u.builtin.args->next->c));
}

/* collapse the string addition in c into a string constant */
static struct component *perform_string_add(struct component *c)
{
  assert(is_string_add(c));
  const struct str_and_len *a = &c->u.builtin.args->c->u.cst->u.string;
  const struct str_and_len *b = &c->u.builtin.args->next->c->u.cst->u.string;
  size_t len = a->len + b->len;
  struct str_and_len s = {
    .len = len,
    .str = allocate(parser_memory, len)
  };
  memcpy(s.str, a->str, a->len);
  memcpy(s.str + a->len, b->str, b->len);
  return new_component(parser_memory, c->u.builtin.args->c->lineno,
                       c_constant,
                       new_constant(parser_memory, cst_string, s));
}

static struct component *make_binary(unsigned int op, int lineno,
                             struct component *arg1, struct component *arg2)
{
  if (op == b_add)
    {
      /* check if this is a collapsible string addition */
      if (is_string_add(arg1))
        {
          if (is_string(arg2))
            arg1 = perform_string_add(arg1);
          else if (is_string_add(arg2))
            {
              arg1 = perform_string_add(arg1);
              arg2 = perform_string_add(arg2);
            }
        }
      else if (is_string(arg1) && is_string_add(arg2))
        arg2 = perform_string_add(arg2);
    }

  return new_binop_component(parser_memory, lineno, op, arg1, arg2);
}

static struct component *make_unary(unsigned int op, int lineno,
                                    struct component *arg)
{
  if (arg->vclass == c_constant)
    switch (arg->u.cst->vclass)
      {
      case cst_int:
        {
          long n = arg->u.cst->u.integer;
          switch (op)
            {
            case b_not:    return new_int_component(parser_memory, !n);
            case b_negate: return new_int_component(parser_memory, -n);
            case b_bitnot: return new_int_component(parser_memory, ~n);
            default:
              break;
            }
          break;
        }
      case cst_float:
        {
          double d = arg->u.cst->u.mudlle_float;
          if (op == b_negate)
            return new_component(parser_memory, lineno, c_constant,
                                 new_constant(parser_memory, cst_float, -d));
          break;
        }
      default:
        break;
      }
  return new_component(parser_memory, lineno, c_builtin, op, 1, arg);
}

int yyparse(void);

static bool do_parse(struct alloc_block *heap)
{
  parsed_code = NULL;
  parsed_constant = NULL;
  parser_memory = heap;
  erred = false;
  int result = yyparse();
  parser_memory = NULL;

  return result == 0 && !erred;
}

bool parse(struct alloc_block *heap, struct mfile **f)
{
  assert(allow_comma_expression());
  if (!do_parse(heap))
    return false;
  *f = parsed_code;
  return true;
}

bool parse_constant(struct alloc_block *heap, struct constant **c)
{
  assert(!allow_comma_expression());
  if (!do_parse(heap))
    return false;
  *c = parsed_constant;
  return true;
}

static bool find_type(const char *name, enum mudlle_type *type)
{
  if (strcasecmp(name, "int") == 0)
    {
      *type = type_integer;
      return true;
    }
  for (enum mudlle_type t = 0; t < last_synthetic_type; ++t)
    if (strcasecmp(name, mudlle_type_names[t]) == 0)
      {
        *type = t;
        return true;
      }

  compile_error("unknown type %s", name);
  return false;
}

%}

%%

start:
  file { parsed_code = $1; } |
  force_constant { parsed_constant = $1; } ;

file:
  simple |
  module ;

simple:
  expression_list {
    $$ = new_file(parser_memory, f_plain, NULL, NULL, NULL, NULL, NULL, NULL,
                new_codeblock(parser_memory, NULL, $1, lexer_filename,
                              lexer_nicename, -1),
                @1.first_line);
  } ;

module:
  module_head reads writes statics code_block optional_semi {
    $$ = new_file(parser_memory, $1.fclass, $1.name, $1.requires, $1.defines,
                  $2, $3, $4, $5, @5.first_line);
  } ;

module_head:
  MODULE optional_symbol requires {
    $$.fclass = f_module;
    $$.name = $2;
    $$.requires = $3;
    $$.defines = NULL;
  } |
  LIBRARY SYMBOL requires defines {
    $$.fclass = f_library;
    $$.name = $2;
    $$.requires = $3;
    $$.defines = $4;
  };

optional_symbol:
  SYMBOL |
  /* empty */ { $$ = NULL; } ;

requires:
  REQUIRES variable_list { $$ = $2; } |
  /* empty */ { $$ = NULL; } ;

defines:
  DEFINES variable_list { $$ = $2; } ;

reads:
  READS variable_list { $$ = $2; } |
  /* empty */ { $$ = NULL; } ;

writes:
  WRITES variable_list { $$ = $2; } |
  /* empty */ { $$ = NULL; } ;

statics:
  STATIC variable_list { $$ = $2; } |
  /* empty */ { $$ = NULL; } ;

expression_list:
  expression_list1 ';' optional_implicit_code_block {
    struct clist *cl = $1;
    if ($3 != NULL) {
      struct component *comp = new_component(parser_memory, @3.first_line,
                                             c_block, $3);
      cl = new_clist(parser_memory, comp, cl);
    }
    $$ = reverse_clist(cl);
  } |
  expression_list1 {
    $$ = reverse_clist($1);
  } ;

optional_implicit_code_block:
  /* empty */ { $$ = NULL; } |
  '|' variable_list '|' expression_list {
      $$ = new_codeblock(parser_memory, $2, $4, lexer_filename,
                         lexer_nicename, @1.first_line);
  } ;

expression_list1:
  expression_list1 ';' expression {
    $$ = new_clist(parser_memory, $3, $1);
  } |
  expression_list1 ';' ELSE {
    yyerror("there must be no semicolon before \"else\"");
    YYABORT;
  } |
  expression {
    $$ = new_clist(parser_memory, $1, NULL);
  } ;

optional_semi: /* empty */ | ';' ;

optional_expression:
  expression |
  /* empty */ { $$ = NULL; } ;

expression: labeled_expression | e0 ;

labeled_expression: label expression
  {
    $$ = new_component(parser_memory, @1.first_line, c_labeled, $1, $2);
  } ;

label: LT SYMBOL GT { $$ = $2; } ;

optional_label:
  label { $$ = $1; } |
  /* empty */ { $$ = NULL; } ;

e0:
  '@' pattern ASSIGN expression {
    $$ = new_pattern_component(parser_memory, $2, $4);
  } |
  control_expression |
  function_expression {
    $$ = new_component(parser_memory, @1.first_line, c_closure, $1);
  } |
  assign_expr |
  e1 ;

control_expression: if | while | loop | exit | match | for ;

if:
  IF '(' expression ')' expression optional_else
    {
      if ($6)
        $$ = new_component(parser_memory, @1.first_line, c_builtin,
                           b_ifelse, 3, $3, $5, $6);
      else
        $$ = new_binop_component(parser_memory, @1.first_line, b_if, $3, $5);
    } ;

optional_else:
  /* empty */ { $$ = NULL; } |
  ELSE expression { $$ = $2; } ;

while:
  WHILE '(' expression ')' expression
    {
      $$ = new_binop_component(parser_memory, @1.first_line, b_while, $3, $5);
    } ;

loop:
  LOOP expression
    {
      $$ = new_component(parser_memory, @1.first_line, c_builtin,
                         b_loop, 1, $2);
    } ;

for:
  FOR '(' optional_variable_list optional_expression ';'
      optional_expression ';' optional_expression ')' expression
    {
      $$ = new_for_component(parser_memory, $3, $4, $6, $8, $10,
                             lexer_filename, @1.first_line);
    } ;

match:
  MATCH '(' expression ')' '[' match_list optional_semi ']' {
    $$ = new_match_component(parser_memory, $3, $6);
  } ;

match_list:
  match_node { $$ = new_match_list(parser_memory, $1, NULL); } |
  match_list ';' match_node { $$ = new_match_list(parser_memory, $3, $1); } ;

opt_match_condition:
  /* nothing */ { $$ = NULL; } |
  SC_AND sc_and_expr { $$ = $2; } ;

match_node:
  pattern_atom opt_match_condition PATTERN_MATCH expression {
    $$ = new_match_node(parser_memory, $1, $2, $4, lexer_filename,
                        @1.first_line);
  } ;

exit:
  LOOP_EXIT optional_label e0
    {
      $$ = new_component(parser_memory, @1.first_line, c_exit, $2, $3);
    } ;

function_expression:
  opt_typeset FUNCTION optional_help parameters expression {
    if ($4.varargs)
      $$ = new_vfunction(parser_memory, $1, $3, $4.var, $5,
                         @2.first_line, lexer_filename, lexer_nicename);
    else
      $$ = new_function(parser_memory, $1, $3, $4.args, $5, @2.first_line,
                        lexer_filename, lexer_nicename);
  } ;

optional_help:
  /* empty */ { $$.str = NULL; $$.len = 0; } |
  help ;

help:
  help '+' STRING {
    $$.len = $1.len + $3.len;
    $$.str = allocate(parser_memory, $$.len);
    memcpy($$.str, $1.str, $1.len);
    memcpy($$.str + $1.len, $3.str, $3.len);
  } | STRING ;

parameters:
  '(' plist ')' { $$.varargs = false; $$.args = $2; } |
  variable_name {
    $$.varargs = true;
    $$.var = $1;
  } ;

plist:
  /* empty */ { $$ = NULL; } |
  plist1 ;

plist1:
  plist1 COMMA typeset variable_name {
    $$ = new_vlist(parser_memory, $4, $3, @4.first_line, $1);
  } |
  plist1 COMMA variable_name {
    $$ = new_vlist(parser_memory, $3, TYPESET_ANY, @3.first_line, $1);
  } |
  typeset variable_name {
    $$ = new_vlist(parser_memory, $2, $1, @2.first_line, NULL);
  } |
  variable_name {
    $$ = new_vlist(parser_memory, $1, TYPESET_ANY, @1.first_line, NULL);
  } ;

opt_typeset:
  typeset { $$ = $1; } |
  /* empty */ { $$ = TYPESET_ANY; } ;

typeset:
  type { $$ = type_typeset($1); } |
  '{' typelist '}' { $$ = $2; } ;

typelist:
  typelist COMMA type { $$ = $1 | type_typeset($3); } |
  type { $$ = type_typeset($1); } ;

type:
  SYMBOL { if (!find_type($1, &$$)) YYABORT; } ;

assign_expr:
  unary_expr ASSIGN expression {
    $$ = new_assign_expression(parser_memory, $1, b_invalid, $3, false,
                               @2.first_line);
    if ($$ == NULL)
      YYABORT;
  } |
  unary_expr op_assign expression {
    $$ = new_assign_expression(parser_memory, $1, $2, $3, false,
                               @2.first_line);
    if ($$ == NULL)
      YYABORT;
  } ;

op_assign:
  ASSIGN_ADD     { $$ = b_add; } |
  ASSIGN_SUB     { $$ = b_subtract; } |
  ASSIGN_MUL     { $$ = b_multiply; } |
  ASSIGN_DIV     { $$ = b_divide; } |
  ASSIGN_REM     { $$ = b_remainder; } |
  ASSIGN_BIT_XOR { $$ = b_bitxor; } |
  ASSIGN_BIT_AND { $$ = b_bitand; } |
  ASSIGN_BIT_OR  { $$ = b_bitor; } |
  ASSIGN_SC_AND  { $$ = b_sc_and; } |
  ASSIGN_XOR     { $$ = b_xor; } |
  ASSIGN_SC_OR   { $$ = b_sc_or; } |
  ASSIGN_SHR     { $$ = b_shift_right; } |
  ASSIGN_SHL     { $$ = b_shift_left; } ;

e1:
  e1 '.' e1 { $$ = make_binary(b_cons, @2.first_line, $1, $3); } |

  e1 XOR e1 { $$ = make_binary(b_xor, @2.first_line, $1, $3); } |
  e1 SC_OR e1 { $$ = make_binary(b_sc_or, @2.first_line, $1, $3); } |
  sc_and_expr ;

sc_and_expr:
  sc_and_expr SC_AND e2 {
    $$ = make_binary(b_sc_and, @2.first_line, $1, $3);
  } |
  e2 ;

e2:
  e2 EQ e2 { $$ = make_binary(b_eq, @2.first_line, $1, $3); } |
  e2 NE e2 { $$ = make_binary(b_ne, @2.first_line, $1, $3); } |
  e2 LT e2 { $$ = make_binary(b_lt, @2.first_line, $1, $3); } |
  e2 LE e2 { $$ = make_binary(b_le, @2.first_line, $1, $3); } |
  e2 GT e2 { $$ = make_binary(b_gt, @2.first_line, $1, $3); } |
  e2 GE e2 { $$ = make_binary(b_ge, @2.first_line, $1, $3); } |
  e2 '^' e2 { $$ = make_binary(b_bitxor, @2.first_line, $1, $3); } |
  e2 '|' e2 { $$ = make_binary(b_bitor, @2.first_line, $1, $3); } |
  e2 '&' e2 { $$ = make_binary(b_bitand, @2.first_line, $1, $3); } |
  e2 SHIFT_LEFT e2 { $$ = make_binary(b_shift_left, @2.first_line, $1, $3); } |
  e2 SHIFT_RIGHT e2 {
    $$ = make_binary(b_shift_right, @2.first_line, $1, $3);
  } |
  e2 '+' e2 { $$ = make_binary(b_add, @2.first_line, $1, $3); } |
  e2 '-' e2 { $$ = make_binary(b_subtract, @2.first_line, $1, $3); } |
  e2 '*' e2 { $$ = make_binary(b_multiply, @2.first_line, $1, $3); } |
  e2 '/' e2 { $$ = make_binary(b_divide, @2.first_line, $1, $3); } |
  e2 '%' e2 { $$ = make_binary(b_remainder, @2.first_line, $1, $3); } |
  unary_expr ;

unary_expr:
  '-' unary_expr { $$ = make_unary(b_negate, @1.first_line, $2); } |
  '!' unary_expr { $$ = make_unary(b_not, @1.first_line, $2); } |
  '~' unary_expr { $$ = make_unary(b_bitnot, @1.first_line, $2); } |
  '&' unary_expr {
    $$ = new_reference(parser_memory, @1.first_line, $2);
    if ($$ == NULL)
      YYABORT;
  } |
  '*' unary_expr { $$ = new_dereference(parser_memory, @1.first_line, $2); } |
  incrementer unary_expr {
    $$ = new_assign_expression(parser_memory, $2, $1,
                               new_int_component(parser_memory, 1),
                               false, @1.first_line);
    if ($$ == NULL)
      YYABORT;
  } |
  e3 ;

e3:
  e3 '(' call_list ')' {
    $$ = new_component(parser_memory, @2.first_line, c_execute,
                       new_clist(parser_memory, $1, $3));
    $$->lineno = @2.first_line;
  } |
  e3 '[' expression ']' {
    $$ = new_binop_component(parser_memory, @2.first_line, b_ref, $1, $3);
  } |
  e3 incrementer {
    $$ = new_assign_expression(parser_memory, $1, $2,
                               new_int_component(parser_memory, 1),
                               true, @2.first_line);
    if ($$ == NULL)
      YYABORT;
  } |
  primary_expr ;

incrementer:
  INCREMENT { $$ = b_add; } |
  DECREMENT { $$ = b_subtract; } ;

primary_expr:
  variable {
    $$ = new_component(parser_memory, @1.first_line, c_recall, $1);
  } |
  simple_constant {
    $$ = new_component(parser_memory, @1.first_line, c_constant, $1);
  } |
  QUOTE constant {
    $$ = new_component(parser_memory, @1.first_line, c_constant, $2);
  } |
  code_block {
    $$ = new_component(parser_memory, @1.first_line, c_block, $1);
  } |
  '(' expression ')' { $$ = $2; } ;

call_list:
  /* empty */ { $$ = NULL; } |
  call_list1 { $$ = reverse_clist($1); } ;

call_list1:
  call_list1 COMMA expression { $$ = new_clist(parser_memory, $3, $1); } |
  expression { $$ = new_clist(parser_memory, $1, NULL); } ;

unary_constant:
  '-' INTEGER { $$ = new_int_constant(parser_memory, -$2); } |
  '~' INTEGER { $$ = new_int_constant(parser_memory, ~$2); } |
  '!' INTEGER { $$ = new_int_constant(parser_memory, !$2); } |
  '-' FLOAT { $$ = new_constant(parser_memory, cst_float, -$2); } |
  simple_constant;

force_constant:
  FORCE_CONSTANT constant { $$ = $2; } ;

table_class:
  SYMBOL {
    if (strcasecmp($1, "c") != 0)
      {
        compile_error("unexpected symbol %s", $1);
        YYABORT;
      }
    $$ = cst_ctable;
  } |
  /* empty */ { $$ = cst_table; }

constant:
  unary_constant |
  '{' table_class table_entry_list '}' {
    if (cstlist_has_len($3, MAX_TABLE_ENTRIES + 1))
      {
        compile_error("constant table size exceeds %ld elements",
                      (long)MAX_TABLE_ENTRIES);
        YYABORT;
      }
    struct str_and_len *s0, *s1;
    if (cstlist_find_symbol_clash($3, $2 == cst_ctable, &s0, &s1))
      {
        struct strbuf sb0 = SBNULL, sb1 = SBNULL;
        sb_write_string(&sb0, s0->str, s0->len);
        sb_write_string(&sb1, s1->str, s1->len);
        compile_error("%stable entry %s conflicts with entry %s",
                      $2 == cst_ctable ? "c" : "",
                      sb_str(&sb0), sb_str(&sb1));
        sb_free(&sb0);
        sb_free(&sb1);
        YYABORT;
      }
    $$ = new_constant(parser_memory, $2, $3);
  } |
  '{' table_class '}' { $$ = new_constant(parser_memory, $2, NULL); } |
  '[' optional_constant_list ']' {
    if (cstlist_has_len($2, MAX_VECTOR_SIZE + 1))
      {
        compile_error("constant vector length exceeds %ld elements",
                      (long)MAX_VECTOR_SIZE);
        YYABORT;
      }
    $$ = new_constant(parser_memory, cst_array, $2);
  } |
  '(' constant_list optional_constant_tail ')' {
    $$ = new_constant(parser_memory, cst_list,
                      new_cstlist(parser_memory, $3, $2));
  } |
  '(' ')' { $$ = new_constant(parser_memory, cst_list, NULL); } |
  LT table_entry GT { $$ = $2; } |
  constant_expr ;

optional_constant_tail:
  /* empty */ { $$ = NULL; } |
  '.' constant { $$ = $2; } ;

simple_constant:
  string_constant |
  INTEGER { $$ = new_int_constant(parser_memory, $1); } |
  FLOAT { $$ = new_constant(parser_memory, cst_float, $1); } |
  BIGINT { $$ = new_constant(parser_memory, cst_bigint, $1); } ;

string_constant:
  STRING { $$ = new_constant(parser_memory, cst_string, $1); } ;

optional_constant_list:
  /* empty */ { $$ = NULL; } |
  constant_list;

constant_list:
  constant { $$ = new_cstlist(parser_memory, $1, NULL); } |
  constant_list constant { $$ = new_cstlist(parser_memory, $2, $1); } ;

constant_expr:
  COMMA { check_constant_expr(); check_space($1.space_suffix, "constants"); }
  constant_expr_tail { $$ = $3; } ;

constant_expr_tail:
  variable {
    $$ = new_constant(parser_memory, cst_expression,
                      new_component(parser_memory, @1.first_line,
                                    c_recall, $1));
  } |
  '(' expression ')' {
    $$ = new_constant(parser_memory, cst_expression, $2);
  } ;

table_entry_list:
  table_entry { $$ = new_cstlist(parser_memory, $1, NULL); } |
  table_entry_list table_entry { $$ = new_cstlist(parser_memory, $2, $1); } ;

table_entry:
  string_constant ASSIGN constant {
    $$ = new_constant(parser_memory, cst_symbol,
		      new_cstpair(parser_memory, $1, $3));
  } |
  constant_expr ASSIGN constant {
    $$ = new_constant(parser_memory, cst_symbol,
		      new_cstpair(parser_memory, $1, $3));
  } ;

pattern:
  pattern_list | pattern_array | pattern_symbol;

pattern_atom:
  pattern |
  SINK { $$ = new_pattern_sink(parser_memory); } |
  variable {
    $$ = new_pattern_variable(parser_memory, $1, stype_any, @1.first_line);
  } |
  unary_constant { $$ = new_pattern_constant(parser_memory, $1,
                                             @1.first_line); } |
  COMMA { check_space($1.space_suffix, "patterns"); } pattern_atom_expr {
    $$ = $3;
  } ;

pattern_atom_expr:
  variable {
    $$ = new_pattern_expression(parser_memory,
				new_component(parser_memory, @1.first_line,
                                              c_recall, $1));
  } |
  '(' expression ')' { $$ = new_pattern_expression(parser_memory, $2); } ;

pattern_symbol:
  LT pattern_atom ASSIGN pattern_atom GT {
    $$ = new_pattern_symbol(parser_memory, $2, $4, @1.first_line);
  } ;

pattern_sequence:
  pattern_atom { $$ = new_pattern_list(parser_memory, $1, NULL); } |
  pattern_sequence pattern_atom {
    $$ = new_pattern_list(parser_memory, $2, $1);
  } ;

opt_pattern_sequence:
  /**/ { $$ = NULL; } |
  pattern_sequence ;

opt_pattern_list_tail:
  /**/ { $$ = NULL; } |
  '.' pattern_atom { $$ = $2; } ;

pattern_list:
  '(' pattern_sequence opt_pattern_list_tail ')' {
    $$ = new_pattern_compound(parser_memory, pat_list,
			      new_pattern_list(parser_memory, $3, $2), false,
                              @1.first_line);
  } |
  '(' ')' { $$ = new_pattern_compound(parser_memory, pat_list, NULL, false,
                                      @1.first_line); } ;

pattern_array:
  '[' opt_pattern_sequence ELLIPSIS ']' {
    $$ = new_pattern_compound(parser_memory, pat_array, $2, true,
                              @1.first_line);
  } |
  '[' opt_pattern_sequence ']' {
    $$ = new_pattern_compound(parser_memory, pat_array, $2, false,
                              @1.first_line);
  } ;

variable:
  GLOBAL_SYMBOL |
  SYMBOL ;

variable_name:
  SYMBOL ;

code_block:
  '[' optional_variable_list expression_list ']' {
    $$ = new_codeblock(parser_memory, $2, $3, lexer_filename,
                       lexer_nicename, @1.first_line);
  } ;

optional_variable_list:
  /* empty */ { $$ = NULL; } |
  '|' variable_list '|' { $$ = $2; } ;

variable_list:
  variable_list COMMA variable_name {
    $$ = new_vlist(parser_memory, $3, TYPESET_ANY, @3.first_line, $1);
  } |
  variable_name {
    $$ = new_vlist(parser_memory, $1, TYPESET_ANY, @1.first_line, NULL);
  } ;

%%
