/*
 * Copyright (c) 1993-2006 David Gay and Gustav Hållberg
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
#include "mudlle.h"
#include "tree.h"
#include "utils.h"
#include "calloc.h"
#include "types.h"
#include "compile.h"
#include <stdlib.h>
#include <string.h>

#define YYERROR_VERBOSE

%}

%union {
  str_and_len_t string;
  char *symbol;
  int integer;
  struct {
    int op;
    int lineno;
  } operator;
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
  struct {
    int varargs;
    char *var;
    vlist args;
  } tparameters;
  mfile tfile;
  pattern tpattern;
  patternlist tpatternlist;
  matchnode tmatchnode;
  matchnodelist tmatchnodelist;
  int lineno;
}

%token FUNCTION IF ELSE WHILE ASSIGN QUOTE MEXIT LOOP FOR
%token INTEGER STRING SYMBOL GLOBAL_SYMBOL FLOAT BIGINT SINK MATCH PATTERN_MATCH 
%token ELLIPSIS INCREMENTER
%token MODULE LIBRARY IMPORTS DEFINES READS WRITES OP_ASSIGN

%expect 2              /* if-else and constant . */

%right '.'
%left XOR
%left SC_OR OR
%left SC_AND AND
%left EQ NE LT LE GT GE
%left '|' '^'
%left '&'
%left SHIFT_LEFT SHIFT_RIGHT
%left '+' '-'
%left '*' '/' '%'
%left NOT '~' UMINUS

%type <tclist> expression_list expression_list1 call_list call_list1
%type <tcomponent> expression e0 e1 e2 loop exit match for
%type <tcomponent> labeled_expression function_call array_ref 
%type <tcomponent> control_expression opt_match_condition optional_expression
%type <tcomponent> if while optional_else
%type <tvlist> variable_list plist plist1 optional_variable_list
%type <tvlist> imports defines reads writes
%type <tparameters> parameters
%type <tmtype> type optional_type
%type <string> optional_help help STRING
%type <symbol> variable SYMBOL label optional_label optional_symbol GLOBAL_SYMBOL
%type <symbol> variable_name
%type <integer> INTEGER
%type <mudlle_float> FLOAT
%type <bigint_str> BIGINT
%type <tconstant> constant simple_constant optional_constant_tail table_entry
%type <tconstant> string_constant
%type <tblock> code_block
%type <tfunction> function_expression
%type <tcstlist> constant_list optional_constant_list table_entry_list
%type <tfile> entry_types simple module library
%type <tpattern> pattern pattern_list pattern_array pattern_atom 
%type <tpattern> opt_pattern_list_tail pattern_atom_expr
%type <tpatternlist> opt_pattern_sequence pattern_sequence
%type <tmatchnode> match_node
%type <tmatchnodelist> match_list
%type <operator> OP_ASSIGN INCREMENTER
%type <lineno> save_lineno

%{

#include "lexer.h"

static mfile parsed_code;
block_t parser_memory;

void yyerror(const char *s)
{
  if (!erred)
    compile_error("%s", s);
}

static component make_binary(unsigned int op, component arg1, component arg2)
{
  if (op == b_xor)
    return new_xor_component(parser_memory, arg1, arg2);
  return new_component(parser_memory, c_builtin, op, 2, arg1, arg2);
}

static component make_unary(unsigned int op, component arg)
{
  return new_component(parser_memory, c_builtin, op, 1, arg);
}

static component make_ref_set_increment(component exp0, component exp1, 
					int op, int line, component exp2,
					int is_postfix)
{
    /* prefix:
     *  [
     *    | $exp, $ref |
     *    $exp = <exp0>; $ref = <exp1>;
     *    $exp[$ref] = $exp[$ref] <op> <exp2>;
     *  ]
     *
     * postfix:
     *  [
     *    | $exp, $ref, $val |
     *    $exp = <exp0>; $ref = <exp1>;
     *    $val = $exp[$ref];
     *    $exp[$ref] = $val <op> <exp2>;
     *    $val;
     *  ]
     */
    vlist vl = new_vlist(parser_memory, "$exp", stype_any,
			 new_vlist(parser_memory, "$ref", stype_any,
				   NULL));
    component val;
    clist cl;

    if (is_postfix)
      vl = new_vlist(parser_memory, "$val", stype_any,
		     vl);

    cl = new_clist(parser_memory,
		   new_component(parser_memory, c_assign, "$exp", exp0),
		   NULL);
    cl = new_clist(parser_memory,
		   new_component(parser_memory, c_assign, "$ref", exp1),
		   cl);

    val = new_component(parser_memory, c_builtin, b_ref, 2,
			new_component(parser_memory, c_recall, "$exp"),
			new_component(parser_memory, c_recall, "$ref"));

    if (is_postfix)
      {
	cl = new_clist(parser_memory, 
		       new_component(parser_memory, c_assign, "$val", val),
		       cl);
	val = new_component(parser_memory, c_recall, "$val");
      }

    cl = new_clist(parser_memory,
		   new_component(parser_memory, c_builtin, b_set, 3, 
				 new_component(parser_memory, c_recall, "$exp"),
				 new_component(parser_memory, c_recall, "$ref"),
				 make_binary(op, val, exp2)),
		   cl);
    cl->c->lineno = line;

    if (is_postfix)
      cl = new_clist(parser_memory,
		     new_component(parser_memory, c_recall, "$val"),
		     cl);

    val = new_component(parser_memory, c_block,
			new_codeblock(parser_memory,
				      vl,
				      reverse_clist(cl),
				      NULL, -1));
    val->lineno = line;

    return val;
}

void parser_init(void)
{
}

static block_t line_memory;

int yyparse();

mfile parse(block_t heap)
{
  int result;

  parser_memory = heap;
  line_memory = new_block();
  erred = FALSE;
  result = yyparse();
  free_block(line_memory);
  line_memory = parser_memory = NULL;

  return result == 0 && !erred ? parsed_code : NULL;
}

static struct lstack {
  struct lstack *next;
  int lineno;
  const char *filename;
} *lstack;

static void lpush(int alineno, const char *afilename)
{
  struct lstack *newp = allocate(line_memory, sizeof *newp);

  newp->next = lstack;
  lstack = newp;

  newp->lineno = alineno;
  newp->filename = afilename;
}

static void lpop(int *alineno, const char **afilename)
{
  *alineno = lstack->lineno;
  *afilename = lstack->filename;
  lstack = lstack->next;
}

struct mkeyword {
  const char *name;
  mtype value;
};

static const struct mkeyword types[] = {
  { "int",       type_integer },
  { "string",    type_string },
  { "vector",    type_vector },
  { "pair",      type_pair },
  { "symbol",    type_symbol },
  { "table",     type_table },
  { "object",    type_object },
  { "character", type_character },
  { "float",     type_float },
  { "bigint",    type_bigint },
  { "gone",      type_gone },
  { "function",  stype_function },
  { "list",      stype_list },
  { "none",      stype_none },
  { "any",       stype_any },
  { "null",      type_null }
};
#define NTYPES (sizeof types / sizeof(struct mkeyword))

static int find_type(const char *name, mtype *type)
{
  int i;

  for (i = 0; i < NTYPES; i++)
    if (!stricmp(name, types[i].name))
      {
	*type = types[i].value;
	return 1;
      }

  compile_error("unknown type %s", name);
  return 0;
}

%}

%expect 1

%%

start : { lstack = NULL; } entry_types { parsed_code = $2; } ;

entry_types :
  simple |
  library |
  module ;

simple : expression_list 
 { $$ = new_file(parser_memory, f_plain, NULL, NULL, NULL, NULL, NULL, 
		 new_codeblock(parser_memory, NULL, $1, NULL, -1)); } ;

module : MODULE optional_symbol imports reads writes code_block optional_semi
  { $$ = new_file(parser_memory, f_module, $2, $3, NULL, $4, $5, $6); } ;

library : LIBRARY SYMBOL imports defines reads writes code_block optional_semi
  { $$ = new_file(parser_memory, f_library, $2, $3, $4, $5, $6, $7); } ;

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

expression_list :
  expression_list1 optional_semi { $$ = reverse_clist($1); } ;

expression_list1 :
  expression_list1 ';' save_lineno expression { 
    $4->lineno = $3;
    $$ = new_clist(parser_memory, $4, $1);
  } |
  expression_list1 ';' ELSE {
    yyerror("there must be no semicolon before \"else\"");
    YYABORT;
  } |
  save_lineno expression { 
    $2->lineno = $1;
    $$ = new_clist(parser_memory, $2, NULL); 
  } ;

optional_semi : /* empty */ | ';' ;

optional_expression :
  expression |
  /* empty */ { $$ = NULL; } ;

expression : labeled_expression | save_lineno e0 { $$ = $2; $$->lineno = $1; };

labeled_expression : label save_lineno expression 
  { 
    $$ = new_component(parser_memory, c_labeled, $1, $3); 
    $$->lineno = $2;
  } ;

label : LT SYMBOL GT { $$ = $2; } ;

optional_label : label { $$ = $1; }
	       | /* empty */ { $$ = NULL; } ;

e0 :
  '@' pattern ASSIGN expression { 
    $$ = new_pattern_component(parser_memory, $2, $4);
  } |
  control_expression |
  function_expression { $$ = new_component(parser_memory, c_closure, $1); } |
  variable save_lineno ASSIGN expression { 
    $$ = new_component(parser_memory, c_assign, $1, $4);
    $$->lineno = $2;
  } |
  variable OP_ASSIGN expression {
      $$ = new_component(parser_memory, c_assign,
			 $1, make_binary($2.op,
					 new_component(parser_memory, c_recall, $1),
					 $3));
  } |
  e2 '[' expression ']' ASSIGN expression
    { $$ = new_component(parser_memory, c_builtin, b_set, 3, $1, $3, $6); } |
  e2 '[' expression ']' OP_ASSIGN expression {
    $$ = make_ref_set_increment($1, $3, $5.op, $5.lineno, $6, 0);
  } |
  e1 ;

control_expression : if | while | loop | exit | match | for ;

if : 
  IF '(' expression ')' expression optional_else 
    {
      if ($6)
        $$ = new_component(parser_memory, c_builtin, b_ifelse, 3, $3, $5, $6);
      else
        $$ = new_component(parser_memory, c_builtin, b_if, 2, $3, $5);
    } ;		

optional_else : 
  /* empty */ { $$ = NULL; } |
  ELSE expression { $$ = $2; } ;

while : 
  WHILE '(' expression ')' expression
    {
      $$ = new_component(parser_memory, c_builtin, b_while, 2, $3, $5);
    } ;

loop :
  LOOP expression
    {
      $$ = new_component(parser_memory, c_builtin, b_loop, 1, $2);
    } ;

for :
  FOR '(' optional_variable_list optional_expression ';' optional_expression ';' optional_expression ')' expression
    {
      $$ = new_for_component(parser_memory, $3, $4, $6, $8, $10);
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
  SC_AND expression { $$ = $2; } ;

match_node :
  pattern_atom opt_match_condition PATTERN_MATCH expression {
    
    $$ = new_match_node(parser_memory, $1, $2, $4); 
  } ;

exit :
  MEXIT optional_label e0
    {
      $$ = new_component(parser_memory, c_exit, $2, $3);
    } ;

function_expression :
  optional_type FUNCTION { lpush(lineno, filename); } optional_help parameters
  expression 
    {
      int l;
      const char *f;

      lpop(&l, &f);
      if ($5.varargs)
	$$ = new_vfunction(parser_memory, $1, $4, $5.var, $6, l, f);
      else
        $$ = new_function(parser_memory, $1, $4, $5.args, $6, l, f);
    }
  ;

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
  '(' plist ')' { $$.varargs = FALSE; $$.args = $2; } |
  variable_name { $$.varargs = TRUE; $$.var = $1; } ;

plist :
  /* empty */ { $$ = NULL; } |
  plist1 ;

plist1 :
  plist1 ',' type variable_name { $$ = new_vlist(parser_memory, $4, $3, $1); } |
  plist1 ',' variable_name { $$ = new_vlist(parser_memory, $3, stype_any, $1); } |
  type variable_name { $$ = new_vlist(parser_memory, $2, $1, NULL); } |
  variable_name { $$ = new_vlist(parser_memory, $1, stype_any, NULL); } ;

optional_type :
  /* empty */ { $$ = stype_any; } |
  type ;

type :
  SYMBOL { if (!find_type($1, &$$)) YYABORT; } ;

e1 :
  e1 '.' e1 { $$ = make_binary(b_cons, $1, $3); } |
  e1 XOR e1 { $$ = make_binary(b_xor, $1, $3); } |
  e1 SC_OR e1 { $$ = make_binary(b_sc_or, $1, $3); } |
  e1 OR e1 { $$ = make_binary(b_or, $1, $3); } |
  e1 SC_AND e1 { $$ = make_binary(b_sc_and, $1, $3); } |
  e1 AND e1 { $$ = make_binary(b_and, $1, $3); } |
  e1 EQ e1 { $$ = make_binary(b_eq, $1, $3); } |
  e1 NE e1 { $$ = make_binary(b_ne, $1, $3); } |
  e1 LT e1 { $$ = make_binary(b_lt, $1, $3); } |
  e1 LE e1 { $$ = make_binary(b_le, $1, $3); } |
  e1 GT e1 { $$ = make_binary(b_gt, $1, $3); } |
  e1 GE e1 { $$ = make_binary(b_ge, $1, $3); } |
  e1 '|' e1 { $$ = make_binary(b_bitor, $1, $3); } |
  e1 '^' e1 { $$ = make_binary(b_bitxor, $1, $3); } |
  e1 '&' e1 { $$ = make_binary(b_bitand, $1, $3); } |
  e1 SHIFT_LEFT e1 { $$ = make_binary(b_shift_left, $1, $3); } |
  e1 SHIFT_RIGHT e1 { $$ = make_binary(b_shift_right, $1, $3); } |
  e1 '+' e1 { $$ = make_binary(b_add, $1, $3); } |
  e1 '-' e1 { $$ = make_binary(b_subtract, $1, $3); } |
  e1 '*' e1 { $$ = make_binary(b_multiply, $1, $3); } |
  e1 '/' e1 { $$ = make_binary(b_divide, $1, $3); } |
  e1 '%' e1 { $$ = make_binary(b_remainder, $1, $3); } |
  '-' e1 %prec UMINUS { $$ = make_unary(b_negate, $2); } |
  NOT e1  { $$ = make_unary(b_not, $2); } |
  '~' e1  { $$ = make_unary(b_bitnot, $2); } |
  INCREMENTER variable {
    $$ = new_component
      (parser_memory, c_assign, 
       $2, 
       make_binary($1.op,
		   new_component(parser_memory, c_recall, $2),
		   new_component(parser_memory, c_constant,
				 new_constant(parser_memory, cst_int, 1))));
  } |
  variable INCREMENTER { 
    $$ = new_postfix_inc_component(parser_memory, $1, $2.op); } |
  INCREMENTER e2 '[' expression ']' {
    $$ = make_ref_set_increment
      ($2, $4, $1.op, $1.lineno, 
       new_component(parser_memory, c_constant,
		     new_constant(parser_memory, cst_int, 1)),
       0);
  } |
  e2 '[' expression ']' INCREMENTER {
    $$ = make_ref_set_increment
      ($1, $3, $5.op, $5.lineno,
       new_component(parser_memory, c_constant,
		     new_constant(parser_memory, cst_int, 1)),
       1);
  } |
  e2 ;

e2 :
  function_call |
  array_ref |
  variable { $$ = new_component(parser_memory, c_recall, $1); } |
  simple_constant { $$ = new_component(parser_memory, c_constant, $1); } |
  QUOTE constant { $$ = new_component(parser_memory, c_constant, $2); } |
  code_block { $$ = new_component(parser_memory, c_block, $1); } |
  '(' expression ')' { $$ = $2; } ;

array_ref :
  e2 '[' expression ']'
    { $$ = new_component(parser_memory, c_builtin, b_ref, 2, $1, $3); } ;

function_call :
  e2 save_lineno '(' call_list ')' {
    $$ = new_component(parser_memory, c_execute, new_clist(parser_memory, $1, $4));
    $$->lineno = $2;
  } ;

call_list :
  /* empty */ { $$ = NULL; } |
  call_list1 { $$ = reverse_clist($1); } ;

call_list1 :
  call_list1 ',' expression { $$ = new_clist(parser_memory, $3, $1); } |
  expression { $$ = new_clist(parser_memory, $1, NULL); } ;

constant :
  simple_constant |
  '{' table_entry_list '}' { $$ = new_constant(parser_memory, cst_table, $2); } |
  '{' '}' { $$ = new_constant(parser_memory, cst_table, NULL); } |
  '[' optional_constant_list ']' { $$ = new_constant(parser_memory, cst_array, $2); } |
  '(' constant_list optional_constant_tail ')' { 
    $$ = new_constant(parser_memory, cst_list, new_cstlist(parser_memory, $3, $2));
  } |
  '(' ')' { $$ = new_constant(parser_memory, cst_list, NULL); } ;

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
  constant_list constant { $$ = new_cstlist(parser_memory, $2, $1); } |
  constant ',' {
    yyerror("there should be no commas between constant elements");
    YYABORT;
  } ;

table_entry_list :
  table_entry { $$ = new_cstlist(parser_memory, $1, NULL); } |
  table_entry_list table_entry { 
    str_and_len_t *sym = &$2->u.constpair->cst1->u.string;
    str_and_len_t *conflict;
    if ((conflict = cstlist_has_symbol($1, *sym)))
      {
	compile_error("table entry '%s' conflicts with entry '%s'",
		      sym->str, conflict->str);
	YYABORT;
      }
    $$ = new_cstlist(parser_memory, $2, $1);
  } ;

table_entry :
  string_constant ASSIGN constant { 
    $$ = new_constant(parser_memory, cst_symbol, 
		      new_cstpair(parser_memory, $1, $3));
  } ;

pattern :
  save_lineno pattern_list { $$ = $2; $$->lineno = $1; } |
  save_lineno pattern_array { $$ = $2; $$->lineno = $1; } ;

pattern_atom :
  pattern |
  SINK { $$ = new_pattern_sink(parser_memory); } |
  save_lineno variable {
    $$ = new_pattern_symbol(parser_memory, $2, stype_any);
    $$->lineno = $1;
  } |
  simple_constant { $$ = new_pattern_constant(parser_memory, $1); } |
  ',' pattern_atom_expr { $$ = $2; } ;

pattern_atom_expr :
  variable { 
    $$ = new_pattern_expression(parser_memory, 
				new_component(parser_memory, c_recall, $1));
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
			      new_pattern_list(parser_memory, $3, $2), 0);
  } |
  '(' ')' { $$ = new_pattern_compound(parser_memory, pat_list, NULL, 0); } ;

pattern_array :
  '[' opt_pattern_sequence ELLIPSIS ']' { 
    $$ = new_pattern_compound(parser_memory, pat_array, $2, 1); 
  } |
  '[' opt_pattern_sequence ']' {
    $$ = new_pattern_compound(parser_memory, pat_array, $2, 0);
  } ;

variable :
  GLOBAL_SYMBOL |
  SYMBOL ;

variable_name :
  SYMBOL ;

code_block :
  '[' save_lineno optional_variable_list expression_list ']' { 
    $$ = new_codeblock(parser_memory, $3, $4, filename, $2);
  } ;

optional_variable_list :
  /* empty */ { $$ = NULL; } |
  '|' variable_list '|' { $$ = $2; } ;

variable_list :
  variable_list ',' variable_name { $$ = new_vlist(parser_memory, $3, stype_none, $1); } |
  variable_name { $$ = new_vlist(parser_memory, $1, stype_none, NULL); } ;

save_lineno:
  { if (yychar == YYEMPTY) yychar = YYLEX; $$ = lineno; } ;

%%
