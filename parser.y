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
#include "mudlle-config.h"

#include <math.h>
#include <stdlib.h>
#include <string.h>

#include "alloc.h"
#include "calloc.h"
#include "compile.h"
#include "mparser.h"
#include "print.h"
#include "strbuf.h"
#include "tree.h"
#include "types.h"
#include "utils.h"

#define YYERROR_VERBOSE

#ifdef __clang__
/* generated code doesn't handle this  */
#pragma clang diagnostic ignored "-Wmissing-variable-declarations"
#endif

/* needed to copy the fname field */
#define YYLLOC_DEFAULT(cur, rhs, n) do {                        \
    (cur) = YYRHSLOC(rhs, n);                                   \
    if (n > 1)                                                  \
      {                                                         \
        (cur).first_line   = YYRHSLOC(rhs, 1).first_line;       \
        (cur).first_column = YYRHSLOC(rhs, 1).first_column;     \
      }                                                         \
  } while (0)

%}

%union {
  struct str_and_len string;
  const char *symbol;
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
  struct mfile *tfile;
  struct pattern *tpattern;
  struct pattern_list *tpatternlist;
  struct match_node *tmatchnode;
  struct match_node_list *tmatchnodelist;
  enum builtin_op bop;
  struct module_head {
    enum file_class fclass;
    const char *name;
    struct vlist *defines, *requires;
  } *tmodule_head;
  struct {
    bool space_suffix;
  } tcomma;
  bool force_match;
  enum arith_mode arith_mode;
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
%token KW_IF          "if"
%token LOOP           "loop"
%token MATCH          "match"
%token FORCE_MATCH    "match!"
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
%token INTEGER        "integer constant" /* warning: can be -MIN_TAGGED_INT */
%token QUOTE          "'"
%token COMMA          ","
%token STRING         "string constant"
%token GLOBAL_SYMBOL  ":"
%token SYMBOL         "variable name"

%token ARITH          "#arith"

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

%type <tclist> expr_list simple_expr_list opt_call_list call_list
%type <tcomponent> expr e0 e1 e2 e3 e4 e4_safe primary_expr loop exit match for
%type <tcomponent> labeled_expr unary_expr assign_expr sc_and_expr
%type <tcomponent> control_expr opt_expr
%type <tcomponent> if while opt_else
%type <tvlist> variable_list
%type <tvlist> opt_local_vars local_vars
%type <tvlist> requires defines reads writes statics
%type <tmtype> type
%type <tmtypeset> typeset typelist
%type <string> help STRING
%type <symbol> variable label opt_label opt_symbol variable_name
%type <symbol> SYMBOL GLOBAL_SYMBOL
%type <integer> INTEGER
%type <mudlle_float> FLOAT
%type <bigint> BIGINT
%type <integer> table_class
%type <force_match> match_type
%type <tconstant> constant unary_constant simple_constant force_constant
%type <tconstant> constant_expr constant_expr_tail special_float_constant
%type <tconstant> opt_constant_tail table_entry string_constant
%type <tblock> code_block opt_implicit_code_block
%type <tfunction> function_expr f0 f1 f2
%type <tcstlist> constant_list opt_constant_list table_entry_list
%type <tfile> file simple module
%type <tmodule_head> module_head;
%type <tpattern> pattern pattern_list pattern_array pattern_atom pattern_cdr
%type <tpattern> pattern_atom_expr pattern_symbol pattern_and
%type <tpattern> pattern_or opt_pattern_or
%type <tpatternlist> opt_arglist arglist arglist_tail
%type <tpatternlist> opt_pattern_sequence pattern_sequence pattern_list_tail
%type <tmatchnode> match_node
%type <tmatchnodelist> match_list
%type <operator> incrementer
%type <bop> op_assign ASSIGN_ADD ASSIGN_SUB ASSIGN_MUL ASSIGN_DIV ASSIGN_REM
%type <bop> ASSIGN_BIT_XOR ASSIGN_BIT_AND ASSIGN_BIT_OR ASSIGN_SC_AND
%type <bop> ASSIGN_XOR ASSIGN_SC_OR ASSIGN_SHR ASSIGN_SHL
%type <tcomma> COMMA
%type <arith_mode> save_arith_mode

%{

#include "lexer.h"

/* here to catch accidental increase of the size; it's not really harmful to
   change the size */
CASSERT_SIZEOF( YYSTYPE, sizeof (struct str_and_len));

#define BINOP(dst, op, loc, e0, e1) do {        \
  dst = make_binary(b_ ## op, &loc, e0, e1);    \
  if (dst == NULL)                              \
    YYABORT;                                    \
} while (0)

#define UNOP(dst, op, loc, e) do {              \
  dst = make_unary(b_ ## op, &loc, e);          \
  if (dst == NULL)                              \
    YYABORT;                                    \
} while (0)

static enum arith_mode cur_arith_mode;

static struct mfile *parsed_code;
static struct constant *parsed_constant;
struct alloc_block *parser_memory;

static const char *varlist_name;

static void maybe_compile_error(const YYLTYPE *loc, const char *s)
{
  if (!erred)
    compile_error(LLOC_LOC(*loc), "%s", s);
}

static void yyerror(const char *s)
{
  if (!erred)
    compile_error(lexer_location(), "%s", s);
}

static bool lookup_arith_mode(enum arith_mode *dst, const char *src,
                              const YYLTYPE *loc)
{
  static struct {
    const char *name;
    enum arith_mode mode;
  } modes[] = {
    { "bigint",  arith_bigint },
    { "default", arith_default },
    { "float",   arith_float },
    { "integer", arith_integer },
  };

  size_t slen = strlen(src);
  for (int i = 0; i < VLENGTH(modes); ++i)
    if (strncmp(modes[i].name, src, slen) == 0)
      {
        *dst = modes[i].mode;
        return true;
      }

  compile_error(LLOC_LOC(*loc), "unknown arithmetic mode");
  return false;
}

static void check_constant_expr(const YYLTYPE *loc)
{
  if (!allow_comma_expression())
    maybe_compile_error(loc, "comma-prefixed expression not allowed");
}

static void check_space(const YYLTYPE *loc, bool warn, const char *where)
{
  if (warn && !erred)
    compile_error(LLOC_LOC(*loc),
                      "comma must not be followed by whitespace in %s",
                      where);
}

static struct constant *parse_special_float(const YYLTYPE *loc,
                                            bool neg, const char *s)
{
  double d;
  if (strcasecmp(s, "inf") == 0 || strcasecmp(s, "infinity") == 0)
    d = neg ? -INFINITY : INFINITY;
  else if (!neg && strcasecmp(s, "nan") == 0)
    d = nan("");
  else
    {
      compile_error(LLOC_LOC(*loc), "syntax error, unexpected variable name");
      return NULL;
    }
  return new_float_constant(parser_memory, d);
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
  return new_const_component(parser_memory, &c->u.builtin.args->c->loc,
                             new_string_constant(parser_memory, &s));
}

static struct component *make_binary(unsigned int op,
                                     const YYLTYPE *loc,
                                     struct component *arg1,
                                     struct component *arg2)
{
  if (op == b_add && cur_arith_mode == arith_default)
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

  return new_binop_component(parser_memory, LLOC_LOC(*loc), cur_arith_mode,
                             op, arg1, arg2);
}

static bool check_int_range(long l, const struct loc *loc)
{
  if (l == -MIN_TAGGED_INT)
    {
      compile_error(loc, "integer constant out of range");
      return false;
    }
  return true;
}

static bool check_comp_int_range(struct component *c)
{
  if (c->vclass == c_constant && c->u.cst->vclass == cst_int)
    return check_int_range(c->u.cst->u.integer, &c->loc);

  return true;
}

static struct component *make_unary(enum builtin_op op,
                                    const YYLTYPE *loc,
                                    struct component *arg)
{
  if (arg->vclass == c_constant && cur_arith_mode == arith_default)
    switch (arg->u.cst->vclass)
      {
      case cst_int:
        {
          long n = arg->u.cst->u.integer;
          switch (op)
            {
            case b_not: n = !n; break;
            case b_negate:
              n = mudlle_sign_extend(-n); /* sign extend for MIN_TAGGED_INT */
              break;
            case b_bitnot: n = ~n; break;
            default:
              abort();
            }
          return new_int_component(parser_memory, n);
        }
      case cst_float:
        {
          double d = arg->u.cst->u.mudlle_float;
          if (op == b_negate)
            return new_const_component(parser_memory, LLOC_LOC(*loc),
                                       new_float_constant(parser_memory, -d));
          break;
        }
      default:
        break;
      }
  return new_unop_component(parser_memory, LLOC_LOC(*loc), cur_arith_mode,
                            op, arg);
}

int yyparse(void);

static bool do_parse(struct alloc_block *heap)
{
  cur_arith_mode = arith_default;

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

static bool find_type(const YYLTYPE *loc, const char *name,
                      enum mudlle_type *type)
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

  compile_error(LLOC_LOC(*loc), "unknown type %s", name);
  return false;
}

static struct module_head *make_module(
  enum file_class fclass, const char *name,
  struct vlist *requires, struct vlist *defines)
{
  struct module_head *m = allocate(parser_memory, sizeof *m);
  *m = (struct module_head){
    .fclass = fclass, .name = name, .defines = defines, .requires = requires
  };
  return m;
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
  opt_local_vars expr_list {
    const struct loc loc = *LLOC_LOC($1 ? @1 : @2);
    $$ = new_file(parser_memory, f_plain, NULL, NULL, NULL, NULL, NULL, NULL,
                  new_codeblock(parser_memory, $1, $2, &loc),
                  &loc);
  } ;

module:
  module_head reads writes statics code_block opt_semi {
    $$ = new_file(parser_memory, $1->fclass, $1->name, $1->requires,
                  $1->defines, $2, $3, $4, $5, LLOC_LOC(@5));
  } ;

module_head:
  MODULE opt_symbol requires {
    $$ = make_module(f_module, $2, $3, NULL);
  } |
  LIBRARY SYMBOL requires defines {
    $$ = make_module(f_library, $2, $3, $4);
  } ;

opt_symbol:
  SYMBOL |
  /* empty */ { $$ = NULL; } ;

requires:
  REQUIRES { varlist_name = "required module"; } variable_list { $$ = $3; } |
  /* empty */ { $$ = NULL; } ;

defines:
  DEFINES { varlist_name = "defined variable"; } variable_list { $$ = $3; } ;

reads:
  READS { varlist_name = "read variable"; } variable_list { $$ = $3; } |
  /* empty */ { $$ = NULL; } ;

writes:
  WRITES { varlist_name = "written variable"; } variable_list { $$ = $3; } |
  /* empty */ { $$ = NULL; } ;

statics:
  STATIC { varlist_name = "static variable"; } variable_list { $$ = $3; } |
  /* empty */ { $$ = NULL; } ;

expr_list:
  simple_expr_list ';' opt_implicit_code_block {
    struct clist *cl = $1;
    if ($3 != NULL) {
      struct component *comp = new_block_component(parser_memory, $3);
      cl = new_clist(parser_memory, comp, cl);
    }
    $$ = reverse_clist(cl);
  } |
  simple_expr_list {
    $$ = reverse_clist($1);
  } ;

opt_implicit_code_block:
  /* empty */ { $$ = NULL; } |
  local_vars expr_list {
      $$ = new_codeblock(parser_memory, $1, $2, LLOC_LOC(@1));
  } ;

simple_expr_list:
  simple_expr_list ';' expr {
    $$ = new_clist(parser_memory, $3, $1);
  } |
  simple_expr_list ';' ELSE {
    maybe_compile_error(&@2, "there must be no semicolon before \"else\"");
    YYABORT;
  } |
  expr {
    $$ = new_clist(parser_memory, $1, NULL);
  } ;

opt_semi: /* empty */ | ';' ;

opt_expr:
  expr |
  /* empty */ { $$ = NULL; } ;

expr: labeled_expr | e0 ;

labeled_expr: label expr
  {
    $$ = new_labeled_component(parser_memory, LLOC_LOC(@1), $1, $2);
  } ;

label: LT SYMBOL GT { $$ = $2; } ;

opt_label:
  label { $$ = $1; } |
  /* empty */ { $$ = NULL; } ;

e0:
  '@' pattern ASSIGN expr {
    $$ = new_pattern_component(parser_memory, $2, $4);
  } |
  control_expr |
  function_expr {
    $$ = new_closure_component(parser_memory, LLOC_LOC(@1), $1);
  } |
  assign_expr |
  e1 ;

control_expr: if | while | loop | exit | match | for ;

if:
  KW_IF '(' expr ')' expr opt_else {
    if ($6)
      $$ = new_ternop_component(parser_memory, LLOC_LOC(@1),
                                b_ifelse, $3, $5, $6);
    else
      $$ = new_binop_component(parser_memory, LLOC_LOC(@1), arith_default,
                               b_if, $3, $5);
  } ;

opt_else:
  /* empty */ { $$ = NULL; } |
  ELSE expr { $$ = $2; } ;

while:
  WHILE '(' expr ')' expr {
    $$ = new_binop_component(parser_memory, LLOC_LOC(@1), arith_default,
                             b_while, $3, $5);
  } ;

loop:
  LOOP expr {
    $$ = new_unop_component(parser_memory, LLOC_LOC(@1), arith_default,
                            b_loop, $2);
  } ;

for:
  FOR '(' opt_local_vars opt_expr ';' opt_expr ';' opt_expr ')' expr {
    $$ = new_for_component(parser_memory, $3, $4, $6, $8, $10,
                           LLOC_LOC(@1));
  } ;

match:
  match_type '(' expr ')' '[' match_list opt_semi ']' {
    $$ = new_match_component(parser_memory, $1, $3, $6, LLOC_LOC(@1));
  } ;

match_type:
  MATCH { $$ = false; } |
  FORCE_MATCH { $$ = true; } ;

match_list:
  match_node { $$ = new_match_list(parser_memory, $1, NULL); } |
  match_list ';' match_node { $$ = new_match_list(parser_memory, $3, $1); } ;

match_node:
  pattern_or PATTERN_MATCH expr {
    $$ = new_match_node(parser_memory, $1, $3, LLOC_LOC(@1));
  } ;

pattern_or:
  pattern_and SC_OR pattern_or {
    $$ = new_pattern_or(parser_memory, $1, $3, LLOC_LOC(@1));
  } |
  pattern_and ;

pattern_and:
  pattern_atom SC_AND sc_and_expr {
    $$ = new_pattern_and_expr(parser_memory, $1, $3);
  } |
  pattern_atom ;

exit:
  LOOP_EXIT opt_label e0 {
    $$ = new_exit_component(parser_memory, LLOC_LOC(@1), $2, $3);
  } ;

function_expr:
  typeset FUNCTION f0 {
    @$ = @2;
    $$ = $3;
    $$->loc = *LLOC_LOC(@2);
    $$->typeset = $1;
  } |
  FUNCTION f0 {
    $$ = $2;
    $$->loc = *LLOC_LOC(@1);
  } ;

f0:
  help f1 {
    $$ = $2;
    $$->help = $1;
  } |
  f1 ;

f1:
  '(' opt_arglist ')' f2 {
    $$ = $4;
    $$->varargs = false;
    if (!set_function_args(parser_memory, $$, $2))
      YYABORT;
  } |
  variable_name f2 {
    $$ = $2;
    $$->varargs = true;
    $$->args = new_vlist(parser_memory, $1, TYPESET_ANY, LLOC_LOC(@1), NULL);
  } ;

f2:
  expr {
    $$ = new_fn(parser_memory, $1, LLOC_LOC(@1));
  } ;

help:
  help '+' STRING {
    $$.len = $1.len + $3.len;
    $$.str = allocate(parser_memory, $$.len);
    memcpy($$.str, $1.str, $1.len);
    memcpy($$.str + $1.len, $3.str, $3.len);
  } | STRING ;

opt_arglist:
  /* empty */ { $$ = NULL; } |
  arglist ;

arglist:
  arglist_tail COMMA arglist {
    $1->next = $3;
    $$ = $1;
  } |
  arglist_tail ;

arglist_tail:
  typeset variable_name {
    $$ = new_pattern_list(
      parser_memory,
      new_pattern_variable(parser_memory, $2, $1, LLOC_LOC(@2)),
      NULL);
  } |
  variable_name {
    $$ = new_pattern_list(
      parser_memory,
      new_pattern_variable(parser_memory, $1, TYPESET_ANY, LLOC_LOC(@1)),
      NULL);
  } |
  '@' pattern_or { $$ = new_pattern_list(parser_memory, $2, NULL); } ;

typeset:
  type { $$ = type_typeset($1); } |
  '{' typelist '}' { $$ = $2; } ;

typelist:
  typelist COMMA type { $$ = $1 | type_typeset($3); } |
  type { $$ = type_typeset($1); } ;

type:
  SYMBOL { if (!find_type(&@1, $1, &$$)) YYABORT; } ;

assign_expr:
  unary_expr op_assign expr {
    $$ = new_assign_modify_expr(parser_memory, cur_arith_mode, $2, $1, $3,
                                false, true, LLOC_LOC(@2));
    if ($$ == NULL)
      YYABORT;
  } |
  unary_expr ASSIGN expr {
    $$ = new_assign_expression(parser_memory, $1, $3, LLOC_LOC(@2),
                               LLOC_LOC(@1));
    if ($$ == NULL)
      YYABORT;
  } ;

op_assign:
  ASSIGN_ADD |
  ASSIGN_SUB |
  ASSIGN_MUL |
  ASSIGN_DIV |
  ASSIGN_REM |
  ASSIGN_BIT_XOR |
  ASSIGN_BIT_AND |
  ASSIGN_BIT_OR |
  ASSIGN_SC_AND |
  ASSIGN_XOR |
  ASSIGN_SC_OR |
  ASSIGN_SHR |
  ASSIGN_SHL ;

e1:
  e1 '.' e1   { BINOP($$, cons,  @2, $1, $3); } |
  e1 XOR e1   { BINOP($$, xor,   @2, $1, $3); } |
  e1 SC_OR e1 { BINOP($$, sc_or, @2, $1, $3); } |
  sc_and_expr ;

sc_and_expr:
  sc_and_expr SC_AND e2 {
    BINOP($$, sc_and, @2, $1, $3);
  } |
  e2 ;

e2:
  e2 EQ e2          { BINOP($$, eq,          @2, $1, $3); } |
  e2 NE e2          { BINOP($$, ne,          @2, $1, $3); } |
  e2 LT e2          { BINOP($$, lt,          @2, $1, $3); } |
  e2 LE e2          { BINOP($$, le,          @2, $1, $3); } |
  e2 GT e2          { BINOP($$, gt,          @2, $1, $3); } |
  e2 GE e2          { BINOP($$, ge,          @2, $1, $3); } |
  e2 '^' e2         { BINOP($$, bitxor,      @2, $1, $3); } |
  e2 '|' e2         { BINOP($$, bitor,       @2, $1, $3); } |
  e2 '&' e2         { BINOP($$, bitand,      @2, $1, $3); } |
  e2 SHIFT_LEFT e2  { BINOP($$, shift_left,  @2, $1, $3); } |
  e2 SHIFT_RIGHT e2 { BINOP($$, shift_right, @2, $1, $3); } |
  e2 '+' e2         { BINOP($$, add,         @2, $1, $3); } |
  e2 '-' e2         { BINOP($$, subtract,    @2, $1, $3); } |
  e2 '*' e2         { BINOP($$, multiply,    @2, $1, $3); } |
  e2 '/' e2         { BINOP($$, divide,      @2, $1, $3); } |
  e2 '%' e2         { BINOP($$, remainder,   @2, $1, $3); } |
  unary_expr ;

unary_expr:
  e3 {
    if (!check_comp_int_range($1))
      YYABORT;
    $$ = $1;
  };

/* may return out-of-range integer; cf. unary_expr */
e3:
  '-' e3         { UNOP($$, negate, @1, $2); } |
  '!' unary_expr { UNOP($$, not,    @1, $2); } |
  '~' unary_expr { UNOP($$, bitnot, @1, $2); } |
  '&' unary_expr {
    $$ = new_reference(parser_memory, LLOC_LOC(@1), $2);
    if ($$ == NULL)
      YYABORT;
  } |
  '*' unary_expr { $$ = new_dereference(parser_memory, LLOC_LOC(@1), $2); } |
  incrementer unary_expr {
    $$ = new_assign_modify_expr(parser_memory, cur_arith_mode, $1, $2,
                                new_int_component(parser_memory, 1),
                                false, false, LLOC_LOC(@1));
    if ($$ == NULL)
      YYABORT;
  } |
  e4 ;

e4_safe:
  e4 {
    if (!check_comp_int_range($1))
      YYABORT;
    $$ = $1;
  };

/* may return out-of-range integer; cf. e4_safe */
e4:
  e4_safe '(' opt_call_list ')' {
    int nargs = 0;
    for (struct clist *l = $3; l; l = l->next)
      if (++nargs > MAX_FUNCTION_ARGS)
        {
          compile_error(
            &l->c->loc,
            "functions can be called with at most %d arguments",
            MAX_FUNCTION_ARGS);
          YYABORT;
        }
    $$ = new_execute_component(parser_memory, LLOC_LOC(@2),
                               new_clist(parser_memory, $1, $3));
  } |
  e4_safe '[' expr ']' {
    $$ = new_binop_component(parser_memory, LLOC_LOC(@2), arith_default,
                             b_ref, $1, $3);
  } |
  e4_safe incrementer {
    $$ = new_assign_modify_expr(parser_memory, cur_arith_mode, $2, $1,
                                new_int_component(parser_memory, 1),
                                true, false, LLOC_LOC(@2));
    if ($$ == NULL)
      YYABORT;
  } |
  primary_expr ;

incrementer:
  INCREMENT { $$ = b_add; } |
  DECREMENT { $$ = b_subtract; } ;

/* may return out-of-range integer */
primary_expr:
  variable {
    $$ = new_recall_component(parser_memory, LLOC_LOC(@1), $1);
  } |
  simple_constant {
    $$ = new_const_component(parser_memory, LLOC_LOC(@1), $1);
  } |
  QUOTE constant {
    $$ = new_const_component(parser_memory, LLOC_LOC(@1), $2);
  } |
  save_arith_mode opt_arith_mode code_block {
    cur_arith_mode = $1;
    $$ = new_block_component(parser_memory, $3);
  } |
  '(' expr ')' { $$ = $2; } ;

opt_call_list:
  /* empty */ { $$ = NULL; } |
  call_list { $$ = reverse_clist($1); } ;

call_list:
  call_list COMMA expr { $$ = new_clist(parser_memory, $3, $1); } |
  expr { $$ = new_clist(parser_memory, $1, NULL); } ;

unary_constant:
  '-' INTEGER {
    long l = mudlle_sign_extend(-$2); /* sign extend for MIN_TAGGED_INT */
    $$ = new_int_constant(parser_memory, l);
  } |
  '~' INTEGER {
    if (!check_int_range($2, LLOC_LOC(@2)))
      YYABORT;
    $$ = new_int_constant(parser_memory, ~$2);
  } |
  '!' INTEGER {
    if (!check_int_range($2, LLOC_LOC(@2)))
      YYABORT;
    $$ = new_int_constant(parser_memory, !$2);
  } |
  '-' FLOAT { $$ = new_float_constant(parser_memory, -$2); } |
  simple_constant;

force_constant:
  FORCE_CONSTANT constant { $$ = $2; } ;

table_class:
  SYMBOL {
    if (strcasecmp($1, "c") != 0)
      {
        compile_error(LLOC_LOC(@1), "unexpected symbol %s", $1);
        YYABORT;
      }
    $$ = cst_ctable;
  } |
  /* empty */ { $$ = cst_table; }

constant:
  unary_constant |
  special_float_constant |
  '{' table_class table_entry_list '}' {
    if (cstlist_has_len($3, MAX_TABLE_ENTRIES + 1))
      {
        compile_error(LLOC_LOC(@1),
                      "constant table size exceeds %ld elements",
                      (long)MAX_TABLE_ENTRIES);
        YYABORT;
      }
    struct str_and_len *s0, *s1;
    if (cstlist_find_symbol_clash($3, $2 == cst_ctable, &s0, &s1))
      {
        struct strbuf sb0 = SBNULL, sb1 = SBNULL;
        sb_write_string(&sb0, s0->str, s0->len);
        sb_write_string(&sb1, s1->str, s1->len);
        compile_error(LLOC_LOC(@1),
                      "%stable entry %s conflicts with entry %s",
                      $2 == cst_ctable ? "c" : "",
                      sb_str(&sb0), sb_str(&sb1));
        sb_free(&sb0);
        sb_free(&sb1);
        YYABORT;
      }
    $$ = new_table_constant(parser_memory, $2, $3);
  } |
  '{' table_class '}' { $$ = new_table_constant(parser_memory, $2, NULL); } |
  '[' opt_constant_list ']' {
    if (cstlist_has_len($2, MAX_VECTOR_SIZE + 1))
      {
        compile_error(LLOC_LOC(@1),
                      "constant vector length exceeds %ld elements",
                      (long)MAX_VECTOR_SIZE);
        YYABORT;
      }
    $$ = new_array_constant(parser_memory, $2);
  } |
  '(' constant_list opt_constant_tail ')' {
    $$ = new_list_constant(parser_memory, new_cstlist(parser_memory, $3, $2));
  } |
  '(' ')' { $$ = constant_null; } |
  LT table_entry GT { $$ = $2; } |
  constant_expr ;

opt_constant_tail:
  /* empty */ { $$ = NULL; } |
  '.' constant { $$ = $2; } ;

special_float_constant:
  SYMBOL {
    $$ = parse_special_float(&@1, false, $1);
    if ($$ == NULL)
      YYABORT;
  } |
  '-' SYMBOL {
    $$ = parse_special_float(&@2, true, $2);
    if ($$ == NULL)
      YYABORT;
  } ;

simple_constant:
  string_constant |
  INTEGER { $$ = new_int_constant(parser_memory, $1); } |
  FLOAT { $$ = new_float_constant(parser_memory, $1); } |
  BIGINT { $$ = new_bigint_constant(parser_memory, $1); } ;

string_constant:
  STRING { $$ = new_string_constant(parser_memory, &$1); } ;

opt_constant_list:
  /* empty */ { $$ = NULL; } |
  constant_list;

constant_list:
  constant { $$ = new_cstlist(parser_memory, $1, NULL); } |
  constant_list constant { $$ = new_cstlist(parser_memory, $2, $1); } ;

constant_expr:
  COMMA {
    check_constant_expr(&@1);
    check_space(&@1, $1.space_suffix, "constants");
  } constant_expr_tail { $$ = $3; } ;

constant_expr_tail:
  variable {
    $$ = new_expr_constant(
      parser_memory,
      new_recall_component(parser_memory, LLOC_LOC(@1), $1));
  } |
  '(' expr ')' {
    $$ = new_expr_constant(parser_memory, $2);
  } ;

table_entry_list:
  table_entry { $$ = new_cstlist(parser_memory, $1, NULL); } |
  table_entry_list table_entry { $$ = new_cstlist(parser_memory, $2, $1); } ;

table_entry:
  string_constant ASSIGN constant {
    $$ = new_symbol_constant(parser_memory,
                             new_cstpair(parser_memory, $1, $3));
  } |
  constant_expr ASSIGN constant {
    $$ = new_symbol_constant(parser_memory,
                             new_cstpair(parser_memory, $1, $3));
  } ;

pattern:
  pattern_list | pattern_array | pattern_symbol;

pattern_atom:
  pattern |
  SINK { $$ = new_pattern_sink(parser_memory); } |
  variable {
    $$ = new_pattern_variable(parser_memory, $1, TYPESET_ANY, LLOC_LOC(@1));
  } |
  unary_constant { $$ = new_pattern_constant(parser_memory, $1,
                                             LLOC_LOC(@1)); } |
  COMMA {
    check_space(&@1, $1.space_suffix, "patterns");
  } pattern_atom_expr {
    $$ = $3;
  } ;

pattern_atom_expr:
  variable {
    $$ = new_pattern_expression(
      parser_memory,
      new_recall_component(parser_memory, LLOC_LOC(@1), $1));
  } |
  '(' expr ')' { $$ = new_pattern_expression(parser_memory, $2); } ;

pattern_symbol:
  LT pattern_atom ASSIGN pattern_atom GT {
    $$ = new_pattern_symbol(parser_memory, $2, $4, LLOC_LOC(@1));
  } ;

pattern_sequence:
  pattern_atom { $$ = new_pattern_list(parser_memory, $1, NULL); } |
  pattern_sequence pattern_atom {
    $$ = new_pattern_list(parser_memory, $2, $1);
  } ;

opt_pattern_sequence:
  /**/ { $$ = NULL; } |
  pattern_sequence ;

opt_pattern_or:
  SC_OR pattern_or { $$ = $2; } |
  /* empty */ { $$ = NULL; } ;

/* Complicated grammar below to special-case (p0 || p1), (p0 && e),
   and (p0 && e || p1)*/
pattern_list:
  '(' pattern_atom SC_AND sc_and_expr opt_pattern_or ')' {
    $$ = new_pattern_and_expr(parser_memory, $2, $4);
    if ($5)
      $$ = new_pattern_or(parser_memory, $$, $5, LLOC_LOC(@1));
  } |
  '(' pattern_atom SC_OR pattern_or ')' {
    $$ = new_pattern_or(parser_memory, $2, $4, LLOC_LOC(@1));
  } |
  '(' pattern_atom pattern_list_tail ')' {
    struct pattern_list *pl = $3;
    while (pl->next != NULL)
      pl = pl->next;
    pl->next = new_pattern_list(parser_memory, $2, NULL);
    $$ = new_list_pattern(parser_memory, $3, LLOC_LOC(@1));
  } |
  '(' ')' {
    $$ = new_pattern_constant(parser_memory, constant_null, LLOC_LOC(@1));
  } ;

pattern_list_tail:
  pattern_sequence pattern_cdr {
    $$ = new_pattern_list(parser_memory, $2, $1);
  } |
  pattern_cdr {
    $$ = new_pattern_list(parser_memory, $1, NULL);
  } ;

pattern_cdr:
  /* empty */ { $$ = NULL; } |
  '.' pattern_atom { $$ = $2; } ;

pattern_array:
  '[' opt_pattern_sequence ELLIPSIS opt_pattern_sequence ']' {
    $$ = new_array_pattern(parser_memory, $2, true, $4, LLOC_LOC(@1));
  } |
  '[' opt_pattern_sequence ']' {
    $$ = new_array_pattern(parser_memory, $2, false, NULL, LLOC_LOC(@1));
  } ;

variable:
  GLOBAL_SYMBOL |
  SYMBOL ;

variable_name:
  SYMBOL ;

code_block:
  '[' opt_local_vars expr_list ']' {
    $$ = new_codeblock(parser_memory, $2, $3, LLOC_LOC(@1));
  } ;

save_arith_mode:
  /* empty */ { $$ = cur_arith_mode; } ;

opt_arith_mode:
  ARITH '(' SYMBOL ')' {
    if (!lookup_arith_mode(&cur_arith_mode, $3, &@3))
      YYABORT;
  } |
  /* empty */;

opt_local_vars:
  /* empty */ { $$ = NULL; } |
  local_vars ;

local_vars:
  '|' { varlist_name = "variable name"; } variable_list '|' { $$ = $3; } ;

/* requires varlist_name to be set */
variable_list:
  variable_list COMMA variable_name {
    if (vlist_length($1) >= MAX_LOCAL_VARS)
      {
        compile_error(LLOC_LOC(@3), "too many local variables");
        YYABORT;
      }
    if (vlist_find($1, $3))
      {
        compile_error(LLOC_LOC(@3), "duplicate %s %s", varlist_name, $3);
        YYABORT;
      }
    $$ = new_vlist(parser_memory, $3, TYPESET_ANY, LLOC_LOC(@3), $1);
  } |
  variable_name {
    $$ = new_vlist(parser_memory, $1, TYPESET_ANY, LLOC_LOC(@1), NULL);
  } ;

%%
