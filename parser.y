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
#include "print.h"
#include "strbuf.h"
#include "tree.h"
#include "types.h"
#include "utils.h"

#define YYERROR_VERBOSE
%}

%union {
  str_and_len_t string;
  char *symbol;
  int integer;
  enum builtin_op operator;
  double mudlle_float;
  char *bigint_str;
  constant tconstant;
  block tblock;
  function tfunction;
  clist tclist;
  vlist tvlist;
  cstlist tcstlist;
  cstpair tcstpair;
  component tcomponent;
  mtype tmtype;
  unsigned tmtypeset;           /* bitset of 1 << type_xxx */
  struct {
    bool varargs;
    char *var;
    vlist args;
  } tparameters;
  mfile tfile;
  pattern tpattern;
  patternlist tpatternlist;
  matchnode tmatchnode;
  matchnodelist tmatchnodelist;
  enum builtin_op bop;
  struct {
    enum file_class fclass;
    char *name;
    vlist defines, imports;
  } tmodule_head;
}

%locations

%token END 0          "end of input"

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
%token MEXIT          "exit"
%token WHILE          "while"

%token DEFINES        "defines"
%token IMPORTS        "requires"
%token LIBRARY        "library"
%token MODULE         "module"
%token READS          "reads"
%token WRITES         "writes"
%token STATIC         "static"

%token BIGINT         "bigint constant"
%token FLOAT          "floating-point constant"
%token INTEGER        "integer constant"
%token QUOTE          "'"
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
%type <tvlist> imports defines reads writes statics
%type <tparameters> parameters
%type <tmtype> type
%type <tmtypeset> opt_typeset typeset typelist
%type <string> optional_help help STRING
%type <symbol> variable label optional_label optional_symbol variable_name
%type <symbol> SYMBOL GLOBAL_SYMBOL
%type <integer> INTEGER
%type <mudlle_float> FLOAT
%type <bigint_str> BIGINT
%type <tconstant> constant unary_constant simple_constant constant_expr
%type <tconstant> optional_constant_tail table_entry string_constant
%type <tblock> code_block optional_implicit_code_block
%type <tfunction> function_expression
%type <tcstlist> constant_list optional_constant_list table_entry_list
%type <tfile> entry_types simple module
%type <tmodule_head> module_head;
%type <tpattern> pattern pattern_list pattern_array pattern_atom
%type <tpattern> opt_pattern_list_tail pattern_atom_expr
%type <tpatternlist> opt_pattern_sequence pattern_sequence
%type <tmatchnode> match_node
%type <tmatchnodelist> match_list
%type <operator> incrementer
%type <bop> op_assign

%{

#include "lexer.h"

static mfile parsed_code;
block_t parser_memory;

void yyerror(const char *s)
{
  if (!erred)
    compile_error("%s", s);
}

/* true if c is a string constant */
static bool is_string(component c)
{
  return c->vclass == c_constant && c->u.cst->vclass == cst_string;
}

/* true if c is an addition of two strings */
static bool is_string_add(component c)
{
  return (c->vclass == c_builtin && c->u.builtin.fn == b_add
          && is_string(c->u.builtin.args->c)
          && is_string(c->u.builtin.args->next->c));
}

/* collapse the string addition in c into a string constant */
static component perform_string_add(component c)
{
  assert(is_string_add(c));
  const str_and_len_t *a = &c->u.builtin.args->c->u.cst->u.string;
  const str_and_len_t *b = &c->u.builtin.args->next->c->u.cst->u.string;
  size_t len = a->len + b->len;
  str_and_len_t s = {
    .len = len,
    .str = allocate(parser_memory, len)
  };
  memcpy(s.str, a->str, a->len);
  memcpy(s.str + a->len, b->str, b->len);
  return new_component(parser_memory, c->u.builtin.args->c->lineno,
                       c_constant,
                       new_constant(parser_memory, cst_string, s));
}

static component make_binary(unsigned int op, int lineno,
                             component arg1, component arg2)
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

static component make_unary(unsigned int op, int lineno, component arg)
{
  if (arg->vclass == c_constant)
    switch (arg->u.cst->vclass)
      {
      case cst_int:
        {
          int n = arg->u.cst->u.integer;
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

mfile parse(block_t heap)
{
  parser_memory = heap;
  erred = false;
  int result = yyparse();
  parser_memory = NULL;

  return result == 0 && !erred ? parsed_code : NULL;
}

static bool find_type(const char *name, mtype *type)
{
  if (stricmp(name, "int") == 0)
    {
      *type = type_integer;
      return true;
    }
  for (mtype t = 0; t < last_synthetic_type; ++t)
    if (stricmp(name, mtypenames[t]) == 0)
      {
        *type = t;
        return true;
      }

  compile_error("unknown type %s", name);
  return false;
}

%}

%%

start : entry_types { parsed_code = $1; } ;

entry_types :
  simple |
  module ;

simple : expression_list
{ $$ = new_file(parser_memory, f_plain, NULL, NULL, NULL, NULL, NULL, NULL,
		 new_codeblock(parser_memory, NULL, $1, lexer_filename,
                               lexer_nicename, -1),
                 @1.first_line); } ;

module : module_head reads writes statics code_block optional_semi
  { $$ = new_file(parser_memory, $1.fclass, $1.name, $1.imports, $1.defines,
                  $2, $3, $4, $5, @5.first_line); } ;

module_head:
  MODULE optional_symbol imports {
    $$.fclass = f_module;
    $$.name = $2;
    $$.imports = $3;
    $$.defines = NULL;
  } |
  LIBRARY SYMBOL imports defines {
    $$.fclass = f_library;
    $$.name = $2;
    $$.imports = $3;
    $$.defines = $4;
  };

optional_symbol :
  SYMBOL |
  /* empty */ { $$ = NULL; } ;

imports :
  IMPORTS variable_list { $$ = $2; } |
  /* empty */ { $$ = NULL; } ;

defines :
  DEFINES variable_list { $$ = $2; } ;

reads :
  READS variable_list { $$ = $2; } |
  /* empty */ { $$ = NULL; } ;

writes :
  WRITES variable_list { $$ = $2; } |
  /* empty */ { $$ = NULL; } ;

statics :
  STATIC variable_list { $$ = $2; } |
  /* empty */ { $$ = NULL; } ;

expression_list :
  expression_list1 ';' optional_implicit_code_block {
    clist cl = $1;
    if ($3 != NULL) {
      component comp = new_component(parser_memory, @3.first_line,
                                     c_block, $3);
      cl = new_clist(parser_memory, comp, cl);
    }
    $$ = reverse_clist(cl);
  } |
  expression_list1 {
    $$ = reverse_clist($1);
  } ;

optional_implicit_code_block :
  /* empty */ { $$ = NULL; } |
  '|' variable_list '|' expression_list {
      $$ = new_codeblock(parser_memory, $2, $4, lexer_filename,
                         lexer_nicename, @1.first_line);
  } ;

expression_list1 :
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

optional_semi : /* empty */ | ';' ;

optional_expression :
  expression |
  /* empty */ { $$ = NULL; } ;

expression : labeled_expression | e0 ;

labeled_expression : label expression
  {
    $$ = new_component(parser_memory, @1.first_line, c_labeled, $1, $2);
  } ;

label : LT SYMBOL GT { $$ = $2; } ;

optional_label :
  label { $$ = $1; } |
  /* empty */ { $$ = NULL; } ;

e0 :
  '@' pattern ASSIGN expression {
    $$ = new_pattern_component(parser_memory, $2, $4);
  } |
  control_expression |
  function_expression {
    $$ = new_component(parser_memory, @1.first_line, c_closure, $1);
  } |
  assign_expr |
  e1 ;

control_expression : if | while | loop | exit | match | for ;

if :
  IF '(' expression ')' expression optional_else
    {
      if ($6)
        $$ = new_component(parser_memory, @1.first_line, c_builtin,
                           b_ifelse, 3, $3, $5, $6);
      else
        $$ = new_binop_component(parser_memory, @1.first_line, b_if, $3, $5);
    } ;

optional_else :
  /* empty */ { $$ = NULL; } |
  ELSE expression { $$ = $2; } ;

while :
  WHILE '(' expression ')' expression
    {
      $$ = new_binop_component(parser_memory, @1.first_line, b_while, $3, $5);
    } ;

loop :
  LOOP expression
    {
      $$ = new_component(parser_memory, @1.first_line, c_builtin,
                         b_loop, 1, $2);
    } ;

for :
  FOR '(' optional_variable_list optional_expression ';'
      optional_expression ';' optional_expression ')' expression
    {
      $$ = new_for_component(parser_memory, $3, $4, $6, $8, $10,
                             lexer_filename, @1.first_line);
    } ;

match :
  MATCH '(' expression ')' '[' match_list optional_semi ']' {
    $$ = new_match_component(parser_memory, $3, $6);
  } ;

match_list :
  match_node { $$ = new_match_list(parser_memory, $1, NULL); } |
  match_list ';' match_node { $$ = new_match_list(parser_memory, $3, $1); } ;

opt_match_condition :
  /* nothing */ { $$ = NULL; } |
  SC_AND sc_and_expr { $$ = $2; } ;

match_node :
  pattern_atom opt_match_condition PATTERN_MATCH expression {
    $$ = new_match_node(parser_memory, $1, $2, $4, lexer_filename,
                        @1.first_line);
  } ;

exit :
  MEXIT optional_label e0
    {
      $$ = new_component(parser_memory, @1.first_line, c_exit, $2, $3);
    } ;

function_expression :
  opt_typeset FUNCTION optional_help parameters expression {
    if ($4.varargs)
      $$ = new_vfunction(parser_memory, $1, $3, $4.var, $5,
                         @2.first_line, lexer_filename, lexer_nicename);
    else
      $$ = new_function(parser_memory, $1, $3, $4.args, $5, @2.first_line,
                        lexer_filename, lexer_nicename);
  } ;

optional_help :
  /* empty */ { $$.str = NULL; $$.len = 0; } |
  help ;

help :
  help '+' STRING {
    $$.len = $1.len + $3.len;
    $$.str = allocate(parser_memory, $$.len + 1);
    memcpy($$.str, $1.str, $1.len);
    memcpy($$.str + $1.len, $3.str, $3.len + 1);
  } | STRING ;

parameters :
  '(' plist ')' { $$.varargs = false; $$.args = $2; } |
  variable_name {
    $$.varargs = true;
    $$.var = $1;
  } ;

plist :
  /* empty */ { $$ = NULL; } |
  plist1 ;

plist1 :
  plist1 ',' typeset variable_name {
    $$ = new_vlist(parser_memory, $4, $3, @4.first_line, $1);
  } |
  plist1 ',' variable_name {
    $$ = new_vlist(parser_memory, $3, TYPESET_ANY, @3.first_line, $1);
  } |
  typeset variable_name {
    $$ = new_vlist(parser_memory, $2, $1, @2.first_line, NULL);
  } |
  variable_name {
    $$ = new_vlist(parser_memory, $1, TYPESET_ANY, @1.first_line, NULL);
  } ;

opt_typeset :
  typeset { $$ = $1; } |
  /* empty */ { $$ = TYPESET_ANY; } ;

typeset :
  type { $$ = type_typeset($1); } |
  '{' typelist '}' { $$ = $2; } ;

typelist :
  typelist ',' type { $$ = $1 | type_typeset($3); } |
  type { $$ = type_typeset($1); } ;

type :
  SYMBOL { if (!find_type($1, &$$)) YYABORT; } ;

assign_expr :
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

op_assign :
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

e1 :
  e1 '.' e1 { $$ = make_binary(b_cons, @2.first_line, $1, $3); } |

  e1 XOR e1 { $$ = make_binary(b_xor, @2.first_line, $1, $3); } |
  e1 SC_OR e1 { $$ = make_binary(b_sc_or, @2.first_line, $1, $3); } |
  sc_and_expr ;

sc_and_expr :
  sc_and_expr SC_AND e2 {
    $$ = make_binary(b_sc_and, @2.first_line, $1, $3);
  } |
  e2 ;

e2 :
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
  e2 SHIFT_RIGHT e2 { $$ = make_binary(b_shift_right, @2.first_line, $1, $3); } |
  e2 '+' e2 { $$ = make_binary(b_add, @2.first_line, $1, $3); } |
  e2 '-' e2 { $$ = make_binary(b_subtract, @2.first_line, $1, $3); } |
  e2 '*' e2 { $$ = make_binary(b_multiply, @2.first_line, $1, $3); } |
  e2 '/' e2 { $$ = make_binary(b_divide, @2.first_line, $1, $3); } |
  e2 '%' e2 { $$ = make_binary(b_remainder, @2.first_line, $1, $3); } |
  unary_expr ;

unary_expr :
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

e3 :
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

incrementer :
  INCREMENT { $$ = b_add; } |
  DECREMENT { $$ = b_subtract; } ;

primary_expr :
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

call_list :
  /* empty */ { $$ = NULL; } |
  call_list1 { $$ = reverse_clist($1); } ;

call_list1 :
  call_list1 ',' expression { $$ = new_clist(parser_memory, $3, $1); } |
  expression { $$ = new_clist(parser_memory, $1, NULL); } ;

unary_constant :
  '-' INTEGER { $$ = new_constant(parser_memory, cst_int, -$2); } |
  '~' INTEGER { $$ = new_constant(parser_memory, cst_int, ~$2); } |
  '!' INTEGER { $$ = new_constant(parser_memory, cst_int, !$2); } |
  '-' FLOAT { $$ = new_constant(parser_memory, cst_float, -$2); } |
  simple_constant;

constant :
  unary_constant |
  '{' table_entry_list '}' {
    $$ = new_constant(parser_memory, cst_table, $2); } |
  '{' '}' { $$ = new_constant(parser_memory, cst_table, NULL); } |
  '[' optional_constant_list ']' {
    $$ = new_constant(parser_memory, cst_array, $2); } |
  '(' constant_list optional_constant_tail ')' {
    $$ = new_constant(parser_memory, cst_list,
                      new_cstlist(parser_memory, $3, $2));
  } |
  '(' ')' { $$ = new_constant(parser_memory, cst_list, NULL); } |
  ',' constant_expr  { $$ = $2; } ;

optional_constant_tail :
  /* empty */ { $$ = NULL; } |
  '.' constant { $$ = $2; } ;

simple_constant :
  string_constant |
  INTEGER { $$ = new_constant(parser_memory, cst_int, $1); } |
  FLOAT { $$ = new_constant(parser_memory, cst_float, $1); } |
  BIGINT { $$ = new_constant(parser_memory, cst_bigint, $1); } ;

string_constant :
  STRING { $$ = new_constant(parser_memory, cst_string, $1); } ;

optional_constant_list :
  /* empty */ { $$ = NULL; } |
  constant_list;

constant_list :
  constant { $$ = new_cstlist(parser_memory, $1, NULL); } |
  constant_list constant { $$ = new_cstlist(parser_memory, $2, $1); } ;

constant_expr :
  variable {
    $$ = new_constant(parser_memory, cst_expression,
                      new_component(parser_memory, @1.first_line,
                                    c_recall, $1));
  } |
  '(' expression ')' {
    $$ = new_constant(parser_memory, cst_expression, $2);
  } ;

table_entry_list :
  table_entry { $$ = new_cstlist(parser_memory, $1, NULL); } |
  table_entry_list table_entry {
    if ($2->vclass != cst_expression)
      {
        str_and_len_t *sym = &$2->u.constpair->cst1->u.string;
        str_and_len_t *conflict = cstlist_find_symbol($1, *sym);
        if (conflict)
          {
            strbuf_t sb0 = SBNULL, sb1 = SBNULL;
            struct string *mstr = alloc_string_length(sym->str, sym->len);
            GCPRO1(mstr);
            struct oport *sport = make_strbuf_oport(&sb0);
            output_value(sport, prt_write, false, mstr);
            mstr = alloc_string_length(conflict->str, conflict->len);
            sport = make_strbuf_oport(&sb1);
            output_value(sport, prt_write, false, mstr);
            UNGCPRO();
            compile_error("table entry %s conflicts with entry %s",
                          sb_str(&sb0), sb_str(&sb1));
            sb_free(&sb0);
            sb_free(&sb1);
            YYABORT;
          }
      }
    $$ = new_cstlist(parser_memory, $2, $1);
  } ;

table_entry :
  string_constant ASSIGN constant {
    $$ = new_constant(parser_memory, cst_symbol,
		      new_cstpair(parser_memory, $1, $3));
  } |
  ',' constant_expr ASSIGN constant {
    $$ = new_constant(parser_memory, cst_symbol,
		      new_cstpair(parser_memory, $2, $4));
  } ;

pattern :
  pattern_list | pattern_array ;

pattern_atom :
  pattern |
  SINK { $$ = new_pattern_sink(parser_memory); } |
  variable {
    $$ = new_pattern_symbol(parser_memory, $1, stype_any, @1.first_line);
  } |
  unary_constant { $$ = new_pattern_constant(parser_memory, $1,
                                             @1.first_line); } |
  ',' pattern_atom_expr { $$ = $2; } ;

pattern_atom_expr :
  variable {
    $$ = new_pattern_expression(parser_memory,
				new_component(parser_memory, @1.first_line,
                                              c_recall, $1));
  } |
  '(' expression ')' { $$ = new_pattern_expression(parser_memory, $2); } ;

pattern_sequence :
  pattern_atom { $$ = new_pattern_list(parser_memory, $1, NULL); } |
  pattern_sequence pattern_atom { $$ = new_pattern_list(parser_memory, $2, $1); } ;

opt_pattern_sequence :
  /**/ { $$ = NULL; } |
  pattern_sequence ;

opt_pattern_list_tail :
  /**/ { $$ = NULL; } |
  '.' pattern_atom { $$ = $2; } ;

pattern_list :
  '(' pattern_sequence opt_pattern_list_tail ')' {
    $$ = new_pattern_compound(parser_memory, pat_list,
			      new_pattern_list(parser_memory, $3, $2), false,
                              @1.first_line);
  } |
  '(' ')' { $$ = new_pattern_compound(parser_memory, pat_list, NULL, false,
                                      @1.first_line); } ;

pattern_array :
  '[' opt_pattern_sequence ELLIPSIS ']' {
    $$ = new_pattern_compound(parser_memory, pat_array, $2, true,
                              @1.first_line);
  } |
  '[' opt_pattern_sequence ']' {
    $$ = new_pattern_compound(parser_memory, pat_array, $2, false,
                              @1.first_line);
  } ;

variable :
  GLOBAL_SYMBOL |
  SYMBOL ;

variable_name :
  SYMBOL ;

code_block :
  '[' optional_variable_list expression_list ']' {
    $$ = new_codeblock(parser_memory, $2, $3, lexer_filename,
                       lexer_nicename, @1.first_line);
  } ;

optional_variable_list :
  /* empty */ { $$ = NULL; } |
  '|' variable_list '|' { $$ = $2; } ;

variable_list :
  variable_list ',' variable_name {
    $$ = new_vlist(parser_memory, $3, stype_none, @3.first_line, $1);
  } |
  variable_name {
    $$ = new_vlist(parser_memory, $1, stype_none, @1.first_line, NULL);
  } ;

%%
