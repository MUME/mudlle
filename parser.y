%{
#include "mudlle.h"
#include "tree.h"
#include "utils.h"
#include "calloc.h"
#include "types.h"
#include "compile.h"
#include <stdlib.h>
#include <alloca.h>
#include <string.h>
%}

%union {
  char *string;
  char *symbol;
  int integer;
  constant constant;
  block block;
  function function;
  clist clist;
  vlist vlist;
  cstlist cstlist;
  component component;
  mtype mtype;
  struct {
    int varargs;
    char *var;
    vlist args;
  } parameters;
  file file;
}

%token FUNCTION IF ELSE WHILE ASSIGN QUOTE MEXIT LOOP
%token INTEGER STRING SYMBOL
%token MODULE LIBRARY IMPORTS DEFINES READS WRITES

%right '.'
%left SC_OR OR
%left SC_AND AND
%left EQ NE LT LE GT GE
%left '|' '^'
%left '&'
%left SHIFT_LEFT SHIFT_RIGHT
%left '+' '-'
%left '*' '/' '%'
%left NOT '~' UMINUS

%type <clist> expression_list expression_list1 call_list call_list1
%type <component> expression e0 e1 e2 loop exit
%type <component> labeled_expression function_call array_ref control_expression
%type <component> if while optional_else
%type <vlist> variable_list variable_list1 plist plist1
%type <vlist> imports defines reads writes
%type <parameters> parameters
%type <mtype> type optional_type
%type <string> optional_help STRING
%type <symbol> variable SYMBOL label optional_label optional_symbol
%type <integer> INTEGER
%type <constant> constant simple_constant
%type <block> code_block
%type <function> function_expression
%type <cstlist> constant_list
%type <file> entry_types simple module library

%{

#include "lexer.h"

file parsed_code;

void yyerror(const char *s)
{
  error("%s around line %d of %s", s, lineno, filename);
}

component make_binary(unsigned int op, component arg1, component arg2)
{
  return new_component(c_builtin, op, 2, arg1, arg2);
}

component make_unary(unsigned int op, component arg)
{
  return new_component(c_builtin, op, 1, arg);
}

void parser_init(void)
{
}

struct lstack {
  struct lstack *next;
  int lineno;
  const char *filename;
} *lstack;

void lpush(int lineno, const char *filename)
{
  struct lstack *new = allocate(memory, sizeof *new);

  new->next = lstack;
  lstack = new;

  new->lineno = lineno;
  new->filename = filename;
}

void lpop(int *lineno, const char **filename)
{
  *lineno = lstack->lineno;
  *filename = lstack->filename;
  lstack = lstack->next;
}

struct keyword {
  char *name;
  int value;
};

static struct keyword types[] = {
  { "int", type_integer },
  { "string", type_string },
  { "vector", type_vector },
  { "pair", type_pair },
  { "symbol", type_symbol },
  { "table", type_table },
  { "object", type_object },
  { "character", type_character },
  { "gone", type_gone },
  { "function", stype_function },
  { "list", stype_list },
  { "none", stype_none },
  { "any", stype_any },
  { "null", type_null }
};
#define NTYPES (sizeof types / sizeof(struct keyword))

mtype find_type(char *name)
{
  int i;

  for (i = 0; i < NTYPES; i++)
    if (!stricmp(name, types[i].name))
      return types[i].value;

  error("unknown type %s", name);
  return stype_none;
}

%}

%%

start : { lstack = NULL; } entry_types { parsed_code = $2; } ;

entry_types :
  simple |
  library |
  module ;

simple : expression_list 
 { $$ = new_file(f_plain, NULL, NULL, NULL, NULL, NULL, new_codeblock(NULL, $1)); } ;

module : MODULE optional_symbol imports reads writes code_block optional_semi
  { $$ = new_file(f_module, $2, $3, NULL, $4, $5, $6); } ;

library : LIBRARY SYMBOL imports defines reads writes code_block optional_semi
  { $$ = new_file(f_library, $2, $3, $4, $5, $6, $7); } ;

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
  expression_list1 ';' expression { $$ = new_clist($3, $1); } |
  expression { $$ = new_clist($1, NULL); } ;

optional_semi : /* empty */ | ';' ;

expression : labeled_expression | e0 ;

labeled_expression : label expression 
  { 
    $$ = new_component(c_labeled, $1, $2); 
  } ;

label : LT SYMBOL GT { $$ = $2; } ;

optional_label : label { $$ = $1; }
	       | /* empty */ { $$ = NULL; } ;

e0 :
  control_expression |
  function_expression { $$ = new_component(c_closure, $1); } |
  variable ASSIGN expression { $$ = new_component(c_assign, $1, $3); } |
  e2 '[' expression ']' ASSIGN expression
    { $$ = new_component(c_builtin, b_set, 3, $1, $3, $6); } |
  e1 ;

control_expression : if | while | loop | exit ;

if : 
  IF '(' expression ')' expression optional_else 
    {
      if ($6)
        $$ = new_component(c_builtin, b_ifelse, 3, $3, $5, $6);
      else
        $$ = new_component(c_builtin, b_if, 2, $3, $5);
    } ;		

optional_else : 
  /* empty */ { $$ = NULL; } |
  ELSE expression { $$ = $2; } ;

while : 
  WHILE '(' expression ')' expression
    {
      $$ = new_component(c_builtin, b_while, 2, $3, $5);
    } ;

loop :
  LOOP expression
    {
      $$ = new_component(c_builtin, b_loop, 1, $2);
    } ;

exit :
  MEXIT optional_label e0
    {
      $$ = new_component(c_exit, $2, $3);
    } ;

function_expression :
  optional_type FUNCTION { lpush(lineno, filename); } optional_help parameters
  expression 
    {
      int l;
      const char *f;

      lpop(&l, &f);
      if ($5.varargs)
	$$ = new_vfunction($1, $4, $5.var, $6, l, f);
      else
        $$ = new_function($1, $4, $5.args, $6, l, f);
    }
  ;

optional_help :
  /* empty */ { $$ = NULL; } |
  STRING ;

parameters : 
  '(' plist ')' { $$.varargs = FALSE; $$.args = $2; } |
  variable { $$.varargs = TRUE; $$.var = $1; } ;

plist :
  /* empty */ { $$ = NULL; } |
  plist1 ;

plist1 :
  plist1 ',' type variable { $$ = new_vlist($4, $3, $1); } |
  plist1 ',' variable { $$ = new_vlist($3, stype_any, $1); } |
  type variable { $$ = new_vlist($2, $1, NULL); } |
  variable { $$ = new_vlist($1, stype_any, NULL); } ;

optional_type :
  /* empty */ { $$ = stype_any; } |
  type ;

type :
  SYMBOL { $$ = find_type($1); } ;

e1 :
  e1 '.' e1 { $$ = make_binary(b_cons, $1, $3); } |
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
  e2 ;

e2 :
  function_call |
  array_ref |
  variable { $$ = new_component(c_recall, $1); } |
  simple_constant { $$ = new_component(c_constant, $1); } |
  QUOTE constant { $$ = new_component(c_constant, $2); } |
  code_block { $$ = new_component(c_block, $1); } |
  '(' expression ')' { $$ = $2; } ;

array_ref :
  e2 '[' expression ']'
    { $$ = new_component(c_builtin, b_ref, 2, $1, $3); } ;

function_call :
  e2 '(' call_list ')'
    { $$ = new_component(c_execute, new_clist($1, $3)); } ;

call_list :
  /* empty */ { $$ = NULL; } |
  call_list1 { $$ = reverse_clist($1); } ;

call_list1 :
  call_list1 ',' expression { $$ = new_clist($3, $1); } |
  expression { $$ = new_clist($1, NULL); } ;

constant :
  simple_constant |
  '[' constant_list ']' { $$ = new_constant(cst_array, $2); } |
  '(' constant_list ')' { $$ = new_constant(cst_list, $2); } ;

simple_constant :
  INTEGER { $$ = new_constant(cst_int, $1); } |
  STRING { $$ = new_constant(cst_string, $1); } ;

constant_list :
  /* empty */ { $$ = NULL; } |
  constant_list constant { $$ = new_cstlist($2, $1); } ;

variable :
  SYMBOL ;

code_block : /* This junk to make grammar LALR(1) */
  '[' expression optional_semi ']'
    { $$ = new_codeblock(NULL, new_clist($2, NULL)); } |
  '[' expression ';' expression_list ']' 
    { $$ = new_codeblock(NULL, new_clist($2, $4)); } |
  '[' '|' variable_list '|' expression_list ']' 
    { $$ = new_codeblock($3, $5); } ;

variable_list :
  /* empty */ { $$ = NULL; } |
  variable_list1 ;

variable_list1 :
  variable_list1 ',' variable { $$ = new_vlist($3, stype_none, $1); } |
  variable { $$ = new_vlist($1, stype_none, NULL); } ;

%%
