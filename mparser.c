# line 2 "parser.y"
#include "mudlle.h"
#include "tree.h"
#include "utils.h"
#include "calloc.h"
#include "types.h"
#include "compile.h"
#include <stdlib.h>
#include <alloca.h>
#include <string.h>

# line 13 "parser.y"
typedef union  {
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
} YYSTYPE;
# define FUNCTION 257
# define IF 258
# define ELSE 259
# define WHILE 260
# define ASSIGN 261
# define QUOTE 262
# define MEXIT 263
# define LOOP 264
# define INTEGER 265
# define STRING 266
# define SYMBOL 267
# define MODULE 268
# define LIBRARY 269
# define IMPORTS 270
# define DEFINES 271
# define READS 272
# define WRITES 273
# define SC_OR 274
# define OR 275
# define SC_AND 276
# define AND 277
# define EQ 278
# define NE 279
# define LT 280
# define LE 281
# define GT 282
# define GE 283
# define SHIFT_LEFT 284
# define SHIFT_RIGHT 285
# define NOT 286
# define UMINUS 287

# line 66 "parser.y"

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

#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern int yyerrflag;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
YYSTYPE yylval, yyval;
# define YYERRCODE 256

# line 374 "parser.y"

int yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
-1, 26,
	257, 58,
	267, 58,
	-2, 104,
-1, 48,
	257, 56,
	-2, 22,
-1, 52,
	41, 93,
	-2, 56,
-1, 134,
	93, 22,
	-2, 56,
	};
# define YYNPROD 112
# define YYLAST 590
int yyact[]={

    73,    66,   124,    20,   161,    71,    69,   142,    70,    53,
    72,    73,    66,   139,    95,    84,    71,    69,    26,    70,
    74,    72,   166,    46,    44,   147,   163,    50,   184,    75,
    40,    73,    66,    47,   160,    31,    71,    69,    73,    70,
   141,    72,    32,    71,    69,   135,    70,    33,    72,     7,
    29,   156,   185,   171,   168,   154,    52,    65,    79,   150,
   143,    52,    43,    13,   126,    73,    66,   188,    65,    32,
    71,    69,   134,    70,    33,    72,    48,    94,   190,    32,
   157,    77,    79,    88,    33,    79,   149,    64,    65,   189,
   145,    73,    66,    43,   153,    32,    71,    69,    64,    70,
    33,    72,   152,   144,   128,    73,    32,   129,    14,    78,
    71,    33,    51,    87,   125,    72,     2,    76,    64,    73,
    43,     1,    65,    96,    71,    69,   133,    70,    35,    72,
    43,     5,     6,    78,     4,   148,    78,     3,    16,    45,
    90,   140,   146,    25,   127,   165,    43,   138,    91,   180,
   179,   136,    64,    93,   132,    35,   183,    43,    22,    21,
    15,    28,    27,    12,    24,    35,    23,   101,   100,    10,
     0,     0,     0,     0,     0,     0,     0,    77,    77,   158,
     0,    35,     0,     0,   155,   159,     0,     0,   162,     0,
     0,     0,    35,   174,     0,     0,   175,   181,     0,     0,
     0,     0,     0,     0,     0,     0,   172,   176,     0,   187,
   186,     0,     0,     0,     0,     0,     0,     0,     0,     0,
   193,   194,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,    54,    55,    56,
    57,    58,    59,    60,    61,    62,    63,    67,    68,    84,
    56,    57,    58,    59,    60,    61,    62,    63,    67,    68,
    36,     0,    37,     0,    30,    39,    38,    41,    42,    26,
     9,     8,    58,    59,    60,    61,    62,    63,    67,    68,
     0,     0,    20,    41,    42,    67,    68,    36,    34,    37,
     0,    30,    39,    38,    41,    42,    26,    36,     0,    37,
     0,    30,    39,    38,    41,    42,    26,    41,    42,    20,
    41,    42,    67,    68,     0,    34,     0,    30,     0,    20,
    41,    42,    84,     0,    36,    34,    37,     0,    30,    39,
    38,    41,    42,    26,    83,     0,     0,    17,    67,    68,
     0,    34,     0,     0,     0,     0,     0,     0,     0,    17,
     0,     0,    34,     0,     0,     0,     0,     0,     0,     0,
    82,     0,     0,    18,     0,     0,     0,    17,     0,     0,
     0,     0,     0,    17,     0,    18,     0,     0,    17,     0,
     0,     0,    11,    17,     0,    17,    17,    17,     0,     0,
     0,     0,     0,    18,     0,     0,     0,    49,     0,    18,
     0,     0,     0,     0,    18,     0,     0,     0,     0,    18,
     0,    18,    18,    18,     0,    80,     0,     0,     0,     0,
     0,    89,    17,    17,     0,    17,    92,     0,   137,     0,
   137,    97,     0,    98,    99,   102,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,    18,    18,
     0,    18,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,    17,     0,     0,     0,     0,    17,
   130,   131,     0,     0,   137,     0,     0,   137,     0,     0,
    17,   167,     0,     0,     0,     0,     0,    17,    17,     0,
    18,    17,   173,     0,     0,    18,   137,     0,    17,     0,
    17,   182,     0,     0,     0,     0,    18,     0,     0,     0,
     0,     0,   151,    18,    18,    19,   191,    18,     0,    17,
     0,     0,     0,     0,    18,   195,    18,     0,   164,   196,
     0,     0,     0,     0,     0,   169,   170,     0,     0,     0,
     0,     0,     0,     0,     0,    18,   177,     0,   178,    81,
    85,    86,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,   192,     0,   103,
   104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
   114,   115,   116,   117,   118,   119,   120,   121,   122,   123 };
int yypact[]={

 -1000, -1000,     2, -1000, -1000, -1000, -1000, -1000,  -243,  -244,
    17, -1000, -1000, -1000,    39, -1000, -1000,  -234,    21,   -37,
  -247, -1000, -1000, -1000, -1000,  -228, -1000, -1000, -1000, -1000,
    45, -1000,    39,    55,    55,    55,    73,    43,    39,  -277,
 -1000, -1000, -1000,    29,  -256,  -256, -1000, -1000,    39, -1000,
    39,    39,    39,    55,    55,    55,    55,    55,    55,    55,
    55,    55,    55,    55,    55,    55,    55,    55,    55,    55,
    55,    55,    55,    55,  -280, -1000, -1000, -1000, -1000, -1000,
    63, -1000,    16, -1000, -1000, -1000, -1000,    39,    39, -1000,
    66, -1000,    13,  -252,  -258,  -252,  -265, -1000, -1000,   -33,
    62,    46, -1000,   -37,   -26,   -26,    -6,    -6,    28,    28,
    28,    28,    28,    28,    54,    54,     1,    82,    82,    68,
    68, -1000, -1000, -1000, -1000,  -241,    42,    18, -1000,    39,
    61,    53, -1000,   -38,    39,   -73,    36, -1000,  -265,  -252,
 -1000,  -269,  -252,  -235, -1000,    39,   -18, -1000, -1000, -1000,
 -1000,   -39,    39,    39, -1000,   -40,    39,  -252,  -269, -1000,
   -29,  -252, -1000,    39, -1000,    39,  -249, -1000, -1000,  -231,
 -1000, -1000,   -41, -1000,   -29,     8, -1000, -1000, -1000,    48,
    34,  -252, -1000, -1000,    39, -1000,     8, -1000, -1000, -1000,
  -249, -1000, -1000, -1000,  -252, -1000, -1000 };
int yypgo[]={

     0,    49,   169,   168,   167,   382,    63,   515,   360,   166,
   164,   163,   162,   161,   160,   159,   158,   156,    45,   151,
   150,   149,    77,   147,    40,    34,   145,    30,   143,   142,
   334,   108,   140,   139,    86,    50,    35,   138,    64,   137,
   134,   132,   131,   121,   116,    33,   114 };
int yyr1[]={

     0,    44,    43,    39,    39,    39,    40,    41,    42,    33,
    33,    22,    22,    23,    24,    24,    25,    25,     1,     2,
     2,    45,    45,     5,     5,    11,    31,    32,    32,     6,
     6,     6,     6,     6,    14,    14,    14,    14,    15,    17,
    17,    16,     9,    10,    46,    37,    29,    29,    26,    26,
    20,    20,    21,    21,    21,    21,    28,    28,    27,     7,
     7,     7,     7,     7,     7,     7,     7,     7,     7,     7,
     7,     7,     7,     7,     7,     7,     7,     7,     7,     7,
     7,     7,     7,     7,     8,     8,     8,     8,     8,     8,
     8,    13,    12,     3,     3,     4,     4,    34,    34,    34,
    35,    35,    38,    38,    30,    36,    36,    36,    18,    18,
    19,    19 };
int yyr2[]={

     0,     1,     5,     2,     2,     2,     3,    15,    17,     2,
     1,     5,     1,     5,     5,     1,     5,     1,     5,     7,
     3,     0,     2,     2,     2,     5,     7,     3,     1,     2,
     3,     7,    13,     2,     2,     2,     2,     2,    13,     1,
     5,    11,     5,     7,     1,    13,     1,     2,     7,     3,
     1,     2,     9,     7,     5,     3,     1,     2,     3,     7,
     7,     7,     7,     7,     7,     7,     7,     7,     7,     7,
     7,     7,     7,     7,     7,     7,     7,     7,     7,     7,
     5,     5,     5,     2,     2,     2,     3,     3,     5,     3,
     7,     9,     9,     1,     3,     7,     3,     2,     7,     7,
     3,     3,     1,     5,     2,     9,    11,    13,     1,     2,
     7,     3 };
int yychk[]={

 -1000,   -43,   -44,   -39,   -40,   -42,   -41,    -1,   269,   268,
    -2,    -5,   -11,    -6,   -31,   -14,   -37,   -30,    -8,    -7,
   280,   -15,   -16,    -9,   -10,   -28,   267,   -12,   -13,   -35,
   262,   -36,    40,    45,   286,   126,   258,   260,   264,   263,
   -27,   265,   266,    91,   267,   -33,   267,   -45,    59,    -5,
   261,    91,    40,    46,   274,   275,   276,   277,   278,   279,
   280,   281,   282,   283,   124,    94,    38,   284,   285,    43,
    45,    42,    47,    37,   267,   257,   -34,   -35,    91,    40,
    -5,    -7,    -8,   -30,   267,    -7,    -7,    40,    40,    -5,
   -32,   -31,    -5,   124,   -22,   270,   -22,    -5,    -5,    -5,
    -3,    -4,    -5,    -7,    -7,    -7,    -7,    -7,    -7,    -7,
    -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,
    -7,    -7,    -7,    -7,   282,   -46,   -38,   -38,    41,    91,
    -5,    -5,    -6,   -45,    59,   -18,   -19,   -30,   -23,   271,
   -18,   -24,   272,    93,    41,    44,   -29,   266,    93,   -34,
    41,    -5,    41,    41,    93,    -1,   124,    44,   -24,   -18,
   -25,   273,   -18,   261,    -5,   -26,    40,   -30,    93,    -5,
    -5,    93,    -1,   -30,   -25,   -36,   -18,    -5,    -5,   -20,
   -21,   -27,   -30,   -17,   259,    93,   -36,   -45,    59,    41,
    44,   -30,    -5,   -45,   -27,   -30,   -30 };
int yydef[]={

     1,    -2,    56,     2,     3,     4,     5,     6,     0,    10,
    21,    20,    23,    24,    56,    29,    30,    86,    83,    33,
     0,    34,    35,    36,    37,     0,    -2,    84,    85,    87,
     0,    89,    56,     0,     0,     0,     0,     0,    56,    28,
    57,   100,   101,    56,    12,    12,     9,    18,    -2,    25,
    56,    56,    -2,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,    44,    88,    97,   102,   102,
     0,    80,    83,    86,   104,    81,    82,    56,    56,    42,
    56,    27,    21,   108,     0,   108,    15,    19,    31,     0,
     0,    94,    96,    59,    60,    61,    62,    63,    64,    65,
    66,    67,    68,    69,    70,    71,    72,    73,    74,    75,
    76,    77,    78,    79,    26,    46,     0,     0,    90,    56,
     0,     0,    43,     0,    -2,     0,   109,   111,    15,   108,
    11,    17,   108,    91,    92,    56,     0,    47,    98,   103,
    99,     0,    56,    56,   105,     0,    56,     0,    17,    13,
     0,   108,    14,    56,    95,    56,    50,    49,    91,    39,
    41,   106,     0,   110,     0,    21,    16,    32,    45,     0,
    51,     0,    55,    38,    56,   107,    21,     7,    22,    48,
     0,    54,    40,     8,     0,    53,    52 };
typedef struct { char *t_name; int t_val; } yytoktype;
#ifndef YYDEBUG
#	define YYDEBUG	1	/* allow debugging */
#endif

#if YYDEBUG

yytoktype yytoks[] =
{
	"FUNCTION",	257,
	"IF",	258,
	"ELSE",	259,
	"WHILE",	260,
	"ASSIGN",	261,
	"QUOTE",	262,
	"MEXIT",	263,
	"LOOP",	264,
	"INTEGER",	265,
	"STRING",	266,
	"SYMBOL",	267,
	"MODULE",	268,
	"LIBRARY",	269,
	"IMPORTS",	270,
	"DEFINES",	271,
	"READS",	272,
	"WRITES",	273,
	".",	46,
	"SC_OR",	274,
	"OR",	275,
	"SC_AND",	276,
	"AND",	277,
	"EQ",	278,
	"NE",	279,
	"LT",	280,
	"LE",	281,
	"GT",	282,
	"GE",	283,
	"|",	124,
	"^",	94,
	"&",	38,
	"SHIFT_LEFT",	284,
	"SHIFT_RIGHT",	285,
	"+",	43,
	"-",	45,
	"*",	42,
	"/",	47,
	"%",	37,
	"NOT",	286,
	"~",	126,
	"UMINUS",	287,
	"-unknown-",	-1	/* ends search */
};

char * yyreds[] =
{
	"-no such reduction-",
	"start : /* empty */",
	"start : entry_types",
	"entry_types : simple",
	"entry_types : library",
	"entry_types : module",
	"simple : expression_list",
	"module : MODULE optional_symbol imports reads writes code_block optional_semi",
	"library : LIBRARY SYMBOL imports defines reads writes code_block optional_semi",
	"optional_symbol : SYMBOL",
	"optional_symbol : /* empty */",
	"imports : IMPORTS variable_list",
	"imports : /* empty */",
	"defines : DEFINES variable_list",
	"reads : READS variable_list",
	"reads : /* empty */",
	"writes : WRITES variable_list",
	"writes : /* empty */",
	"expression_list : expression_list1 optional_semi",
	"expression_list1 : expression_list1 ';' expression",
	"expression_list1 : expression",
	"optional_semi : /* empty */",
	"optional_semi : ';'",
	"expression : labeled_expression",
	"expression : e0",
	"labeled_expression : label expression",
	"label : LT SYMBOL GT",
	"optional_label : label",
	"optional_label : /* empty */",
	"e0 : control_expression",
	"e0 : function_expression",
	"e0 : variable ASSIGN expression",
	"e0 : e2 '[' expression ']' ASSIGN expression",
	"e0 : e1",
	"control_expression : if",
	"control_expression : while",
	"control_expression : loop",
	"control_expression : exit",
	"if : IF '(' expression ')' expression optional_else",
	"optional_else : /* empty */",
	"optional_else : ELSE expression",
	"while : WHILE '(' expression ')' expression",
	"loop : LOOP expression",
	"exit : MEXIT optional_label e0",
	"function_expression : optional_type FUNCTION",
	"function_expression : optional_type FUNCTION optional_help parameters expression",
	"optional_help : /* empty */",
	"optional_help : STRING",
	"parameters : '(' plist ')'",
	"parameters : variable",
	"plist : /* empty */",
	"plist : plist1",
	"plist1 : plist1 ',' type variable",
	"plist1 : plist1 ',' variable",
	"plist1 : type variable",
	"plist1 : variable",
	"optional_type : /* empty */",
	"optional_type : type",
	"type : SYMBOL",
	"e1 : e1 '.' e1",
	"e1 : e1 SC_OR e1",
	"e1 : e1 OR e1",
	"e1 : e1 SC_AND e1",
	"e1 : e1 AND e1",
	"e1 : e1 EQ e1",
	"e1 : e1 NE e1",
	"e1 : e1 LT e1",
	"e1 : e1 LE e1",
	"e1 : e1 GT e1",
	"e1 : e1 GE e1",
	"e1 : e1 '|' e1",
	"e1 : e1 '^' e1",
	"e1 : e1 '&' e1",
	"e1 : e1 SHIFT_LEFT e1",
	"e1 : e1 SHIFT_RIGHT e1",
	"e1 : e1 '+' e1",
	"e1 : e1 '-' e1",
	"e1 : e1 '*' e1",
	"e1 : e1 '/' e1",
	"e1 : e1 '%' e1",
	"e1 : '-' e1",
	"e1 : NOT e1",
	"e1 : '~' e1",
	"e1 : e2",
	"e2 : function_call",
	"e2 : array_ref",
	"e2 : variable",
	"e2 : simple_constant",
	"e2 : QUOTE constant",
	"e2 : code_block",
	"e2 : '(' expression ')'",
	"array_ref : e2 '[' expression ']'",
	"function_call : e2 '(' call_list ')'",
	"call_list : /* empty */",
	"call_list : call_list1",
	"call_list1 : call_list1 ',' expression",
	"call_list1 : expression",
	"constant : simple_constant",
	"constant : '[' constant_list ']'",
	"constant : '(' constant_list ')'",
	"simple_constant : INTEGER",
	"simple_constant : STRING",
	"constant_list : /* empty */",
	"constant_list : constant_list constant",
	"variable : SYMBOL",
	"code_block : '[' expression optional_semi ']'",
	"code_block : '[' expression ';' expression_list ']'",
	"code_block : '[' '|' variable_list '|' expression_list ']'",
	"variable_list : /* empty */",
	"variable_list : variable_list1",
	"variable_list1 : variable_list1 ',' variable",
	"variable_list1 : variable",
};
#endif /* YYDEBUG */
#line 1 "/usr/lib/yaccpar"
/*	@(#)yaccpar 1.10 89/04/04 SMI; from S5R3 1.10	*/

/*
** Skeleton parser driver for yacc output
*/

/*
** yacc user known macros and defines
*/
#define YYERROR		goto yyerrlab
#define YYACCEPT	{ free(yys); free(yyv); return(0); }
#define YYABORT		{ free(yys); free(yyv); return(1); }
#define YYBACKUP( newtoken, newvalue )\
{\
	if ( yychar >= 0 || ( yyr2[ yytmp ] >> 1 ) != 1 )\
	{\
		yyerror( "syntax error - cannot backup" );\
		goto yyerrlab;\
	}\
	yychar = newtoken;\
	yystate = *yyps;\
	yylval = newvalue;\
	goto yynewstate;\
}
#define YYRECOVERING()	(!!yyerrflag)
#ifndef YYDEBUG
#	define YYDEBUG	1	/* make debugging available */
#endif

/*
** user known globals
*/
int yydebug;			/* set to 1 to get debugging */

/*
** driver internal defines
*/
#define YYFLAG		(-1000)

/*
** static variables used by the parser
*/
static YYSTYPE *yyv;			/* value stack */
static int *yys;			/* state stack */

static YYSTYPE *yypv;			/* top of value stack */
static int *yyps;			/* top of state stack */

static int yystate;			/* current state */
static int yytmp;			/* extra var (lasts between blocks) */

int yynerrs;			/* number of errors */

int yyerrflag;			/* error recovery flag */
int yychar;			/* current input token number */


/*
** yyparse - return 0 if worked, 1 if syntax error not recovered from
*/
int
yyparse()
{
	register YYSTYPE *yypvt;	/* top of value stack for $vars */
	unsigned yymaxdepth = YYMAXDEPTH;

	/*
	** Initialize externals - yyparse may be called more than once
	*/
	yyv = (YYSTYPE*)malloc(yymaxdepth*sizeof(YYSTYPE));
	yys = (int*)malloc(yymaxdepth*sizeof(int));
	if (!yyv || !yys)
	{
		yyerror( "out of memory" );
		return(1);
	}
	yypv = &yyv[-1];
	yyps = &yys[-1];
	yystate = 0;
	yytmp = 0;
	yynerrs = 0;
	yyerrflag = 0;
	yychar = -1;

	goto yystack;
	{
		register YYSTYPE *yy_pv;	/* top of value stack */
		register int *yy_ps;		/* top of state stack */
		register int yy_state;		/* current state */
		register int  yy_n;		/* internal state number info */

		/*
		** get globals into registers.
		** branch to here only if YYBACKUP was called.
		*/
	yynewstate:
		yy_pv = yypv;
		yy_ps = yyps;
		yy_state = yystate;
		goto yy_newstate;

		/*
		** get globals into registers.
		** either we just started, or we just finished a reduction
		*/
	yystack:
		yy_pv = yypv;
		yy_ps = yyps;
		yy_state = yystate;

		/*
		** top of for (;;) loop while no reductions done
		*/
	yy_stack:
		/*
		** put a state and value onto the stacks
		*/
#if YYDEBUG
		/*
		** if debugging, look up token value in list of value vs.
		** name pairs.  0 and negative (-1) are special values.
		** Note: linear search is used since time is not a real
		** consideration while debugging.
		*/
		if ( yydebug )
		{
			register int yy_i;

			(void)printf( "State %d, token ", yy_state );
			if ( yychar == 0 )
				(void)printf( "end-of-file\n" );
			else if ( yychar < 0 )
				(void)printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				(void)printf( "%s\n", yytoks[yy_i].t_name );
			}
		}
#endif /* YYDEBUG */
		if ( ++yy_ps >= &yys[ yymaxdepth ] )	/* room on stack? */
		{
			/*
			** reallocate and recover.  Note that pointers
			** have to be reset, or bad things will happen
			*/
			int yyps_index = (yy_ps - yys);
			int yypv_index = (yy_pv - yyv);
			int yypvt_index = (yypvt - yyv);
			yymaxdepth += YYMAXDEPTH;
			yyv = (YYSTYPE*)realloc((char*)yyv,
				yymaxdepth * sizeof(YYSTYPE));
			yys = (int*)realloc((char*)yys,
				yymaxdepth * sizeof(int));
			if (!yyv || !yys)
			{
				yyerror( "yacc stack overflow" );
				return(1);
			}
			yy_ps = yys + yyps_index;
			yy_pv = yyv + yypv_index;
			yypvt = yyv + yypvt_index;
		}
		*yy_ps = yy_state;
		*++yy_pv = yyval;

		/*
		** we have a new state - find out what to do
		*/
	yy_newstate:
		if ( ( yy_n = yypact[ yy_state ] ) <= YYFLAG )
			goto yydefault;		/* simple state */
#if YYDEBUG
		/*
		** if debugging, need to mark whether new token grabbed
		*/
		yytmp = yychar < 0;
#endif
		if ( ( yychar < 0 ) && ( ( yychar = yylex() ) < 0 ) )
			yychar = 0;		/* reached EOF */
#if YYDEBUG
		if ( yydebug && yytmp )
		{
			register int yy_i;

			(void)printf( "Received token " );
			if ( yychar == 0 )
				(void)printf( "end-of-file\n" );
			else if ( yychar < 0 )
				(void)printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				(void)printf( "%s\n", yytoks[yy_i].t_name );
			}
		}
#endif /* YYDEBUG */
		if ( ( ( yy_n += yychar ) < 0 ) || ( yy_n >= YYLAST ) )
			goto yydefault;
		if ( yychk[ yy_n = yyact[ yy_n ] ] == yychar )	/*valid shift*/
		{
			yychar = -1;
			yyval = yylval;
			yy_state = yy_n;
			if ( yyerrflag > 0 )
				yyerrflag--;
			goto yy_stack;
		}

	yydefault:
		if ( ( yy_n = yydef[ yy_state ] ) == -2 )
		{
#if YYDEBUG
			yytmp = yychar < 0;
#endif
			if ( ( yychar < 0 ) && ( ( yychar = yylex() ) < 0 ) )
				yychar = 0;		/* reached EOF */
#if YYDEBUG
			if ( yydebug && yytmp )
			{
				register int yy_i;

				(void)printf( "Received token " );
				if ( yychar == 0 )
					(void)printf( "end-of-file\n" );
				else if ( yychar < 0 )
					(void)printf( "-none-\n" );
				else
				{
					for ( yy_i = 0;
						yytoks[yy_i].t_val >= 0;
						yy_i++ )
					{
						if ( yytoks[yy_i].t_val
							== yychar )
						{
							break;
						}
					}
					(void)printf( "%s\n", yytoks[yy_i].t_name );
				}
			}
#endif /* YYDEBUG */
			/*
			** look through exception table
			*/
			{
				register int *yyxi = yyexca;

				while ( ( *yyxi != -1 ) ||
					( yyxi[1] != yy_state ) )
				{
					yyxi += 2;
				}
				while ( ( *(yyxi += 2) >= 0 ) &&
					( *yyxi != yychar ) )
					;
				if ( ( yy_n = yyxi[1] ) < 0 )
					YYACCEPT;
			}
		}

		/*
		** check for syntax error
		*/
		if ( yy_n == 0 )	/* have an error */
		{
			/* no worry about speed here! */
			switch ( yyerrflag )
			{
			case 0:		/* new error */
				yyerror( "syntax error" );
				goto skip_init;
			yyerrlab:
				/*
				** get globals into registers.
				** we have a user generated syntax type error
				*/
				yy_pv = yypv;
				yy_ps = yyps;
				yy_state = yystate;
				yynerrs++;
			skip_init:
			case 1:
			case 2:		/* incompletely recovered error */
					/* try again... */
				yyerrflag = 3;
				/*
				** find state where "error" is a legal
				** shift action
				*/
				while ( yy_ps >= yys )
				{
					yy_n = yypact[ *yy_ps ] + YYERRCODE;
					if ( yy_n >= 0 && yy_n < YYLAST &&
						yychk[yyact[yy_n]] == YYERRCODE)					{
						/*
						** simulate shift of "error"
						*/
						yy_state = yyact[ yy_n ];
						goto yy_stack;
					}
					/*
					** current state has no shift on
					** "error", pop stack
					*/
#if YYDEBUG
#	define _POP_ "Error recovery pops state %d, uncovers state %d\n"
					if ( yydebug )
						(void)printf( _POP_, *yy_ps,
							yy_ps[-1] );
#	undef _POP_
#endif
					yy_ps--;
					yy_pv--;
				}
				/*
				** there is no state on stack with "error" as
				** a valid shift.  give up.
				*/
				YYABORT;
			case 3:		/* no shift yet; eat a token */
#if YYDEBUG
				/*
				** if debugging, look up token in list of
				** pairs.  0 and negative shouldn't occur,
				** but since timing doesn't matter when
				** debugging, it doesn't hurt to leave the
				** tests here.
				*/
				if ( yydebug )
				{
					register int yy_i;

					(void)printf( "Error recovery discards " );
					if ( yychar == 0 )
						(void)printf( "token end-of-file\n" );
					else if ( yychar < 0 )
						(void)printf( "token -none-\n" );
					else
					{
						for ( yy_i = 0;
							yytoks[yy_i].t_val >= 0;
							yy_i++ )
						{
							if ( yytoks[yy_i].t_val
								== yychar )
							{
								break;
							}
						}
						(void)printf( "token %s\n",
							yytoks[yy_i].t_name );
					}
				}
#endif /* YYDEBUG */
				if ( yychar == 0 )	/* reached EOF. quit */
					YYABORT;
				yychar = -1;
				goto yy_newstate;
			}
		}/* end if ( yy_n == 0 ) */
		/*
		** reduction by production yy_n
		** put stack tops, etc. so things right after switch
		*/
#if YYDEBUG
		/*
		** if debugging, print the string that is the user's
		** specification of the reduction which is just about
		** to be done.
		*/
		if ( yydebug )
			(void)printf( "Reduce by (%d) \"%s\"\n",
				yy_n, yyreds[ yy_n ] );
#endif
		yytmp = yy_n;			/* value to switch over */
		yypvt = yy_pv;			/* $vars top of value stack */
		/*
		** Look in goto table for next state
		** Sorry about using yy_state here as temporary
		** register variable, but why not, if it works...
		** If yyr2[ yy_n ] doesn't have the low order bit
		** set, then there is no action to be done for
		** this reduction.  So, no saving & unsaving of
		** registers done.  The only difference between the
		** code just after the if and the body of the if is
		** the goto yy_stack in the body.  This way the test
		** can be made before the choice of what to do is needed.
		*/
		{
			/* length of production doubled with extra bit */
			register int yy_len = yyr2[ yy_n ];

			if ( !( yy_len & 01 ) )
			{
				yy_len >>= 1;
				yyval = ( yy_pv -= yy_len )[1];	/* $$ = $1 */
				yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
					*( yy_ps -= yy_len ) + 1;
				if ( yy_state >= YYLAST ||
					yychk[ yy_state =
					yyact[ yy_state ] ] != -yy_n )
				{
					yy_state = yyact[ yypgo[ yy_n ] ];
				}
				goto yy_stack;
			}
			yy_len >>= 1;
			yyval = ( yy_pv -= yy_len )[1];	/* $$ = $1 */
			yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
				*( yy_ps -= yy_len ) + 1;
			if ( yy_state >= YYLAST ||
				yychk[ yy_state = yyact[ yy_state ] ] != -yy_n )
			{
				yy_state = yyact[ yypgo[ yy_n ] ];
			}
		}
					/* save until reenter driver code */
		yystate = yy_state;
		yyps = yy_ps;
		yypv = yy_pv;
	}
	/*
	** code supplied by user is placed in this switch
	*/
	switch( yytmp )
	{
		
case 1:
# line 153 "parser.y"
{ lstack = NULL; } break;
case 2:
# line 153 "parser.y"
{ parsed_code = yypvt[-0].file; } break;
case 6:
# line 161 "parser.y"
{ yyval.file = new_file(f_plain, NULL, NULL, NULL, NULL, NULL, new_codeblock(NULL, yypvt[-0].clist)); } break;
case 7:
# line 164 "parser.y"
{ yyval.file = new_file(f_module, yypvt[-5].symbol, yypvt[-4].vlist, NULL, yypvt[-3].vlist, yypvt[-2].vlist, yypvt[-1].block); } break;
case 8:
# line 167 "parser.y"
{ yyval.file = new_file(f_library, yypvt[-6].symbol, yypvt[-5].vlist, yypvt[-4].vlist, yypvt[-3].vlist, yypvt[-2].vlist, yypvt[-1].block); } break;
case 10:
# line 171 "parser.y"
{ yyval.symbol = NULL; } break;
case 11:
# line 174 "parser.y"
{ yyval.vlist = yypvt[-0].vlist; } break;
case 12:
# line 175 "parser.y"
{ yyval.vlist = NULL; } break;
case 13:
# line 178 "parser.y"
{ yyval.vlist = yypvt[-0].vlist; } break;
case 14:
# line 181 "parser.y"
{ yyval.vlist = yypvt[-0].vlist; } break;
case 15:
# line 182 "parser.y"
{ yyval.vlist = NULL; } break;
case 16:
# line 185 "parser.y"
{ yyval.vlist = yypvt[-0].vlist; } break;
case 17:
# line 186 "parser.y"
{ yyval.vlist = NULL; } break;
case 18:
# line 189 "parser.y"
{ yyval.clist = reverse_clist(yypvt[-1].clist); } break;
case 19:
# line 192 "parser.y"
{ yyval.clist = new_clist(yypvt[-0].component, yypvt[-2].clist); } break;
case 20:
# line 193 "parser.y"
{ yyval.clist = new_clist(yypvt[-0].component, NULL); } break;
case 25:
# line 200 "parser.y"
{ 
    yyval.component = new_component(c_labeled, yypvt[-1].symbol, yypvt[-0].component); 
  } break;
case 26:
# line 204 "parser.y"
{ yyval.symbol = yypvt[-1].symbol; } break;
case 27:
# line 206 "parser.y"
{ yyval.symbol = yypvt[-0].symbol; } break;
case 28:
# line 207 "parser.y"
{ yyval.symbol = NULL; } break;
case 30:
# line 211 "parser.y"
{ yyval.component = new_component(c_closure, yypvt[-0].function); } break;
case 31:
# line 212 "parser.y"
{ yyval.component = new_component(c_assign, yypvt[-2].symbol, yypvt[-0].component); } break;
case 32:
# line 214 "parser.y"
{ yyval.component = new_component(c_builtin, b_set, 3, yypvt[-5].component, yypvt[-3].component, yypvt[-0].component); } break;
case 38:
# line 221 "parser.y"
{
      if (yypvt[-0].component)
        yyval.component = new_component(c_builtin, b_ifelse, 3, yypvt[-3].component, yypvt[-1].component, yypvt[-0].component);
      else
        yyval.component = new_component(c_builtin, b_if, 2, yypvt[-3].component, yypvt[-1].component);
    } break;
case 39:
# line 229 "parser.y"
{ yyval.component = NULL; } break;
case 40:
# line 230 "parser.y"
{ yyval.component = yypvt[-0].component; } break;
case 41:
# line 234 "parser.y"
{
      yyval.component = new_component(c_builtin, b_while, 2, yypvt[-2].component, yypvt[-0].component);
    } break;
case 42:
# line 240 "parser.y"
{
      yyval.component = new_component(c_builtin, b_loop, 1, yypvt[-0].component);
    } break;
case 43:
# line 246 "parser.y"
{
      yyval.component = new_component(c_exit, yypvt[-1].symbol, yypvt[-0].component);
    } break;
case 44:
# line 251 "parser.y"
{ lpush(lineno, filename); } break;
case 45:
# line 253 "parser.y"
{
      int l;
      const char *f;

      lpop(&l, &f);
      if (yypvt[-1].parameters.varargs)
	yyval.function = new_vfunction(yypvt[-5].mtype, yypvt[-2].string, yypvt[-1].parameters.var, yypvt[-0].component, l, f);
      else
        yyval.function = new_function(yypvt[-5].mtype, yypvt[-2].string, yypvt[-1].parameters.args, yypvt[-0].component, l, f);
    } break;
case 46:
# line 266 "parser.y"
{ yyval.string = NULL; } break;
case 48:
# line 270 "parser.y"
{ yyval.parameters.varargs = FALSE; yyval.parameters.args = yypvt[-1].vlist; } break;
case 49:
# line 271 "parser.y"
{ yyval.parameters.varargs = TRUE; yyval.parameters.var = yypvt[-0].symbol; } break;
case 50:
# line 274 "parser.y"
{ yyval.vlist = NULL; } break;
case 52:
# line 278 "parser.y"
{ yyval.vlist = new_vlist(yypvt[-0].symbol, yypvt[-1].mtype, yypvt[-3].vlist); } break;
case 53:
# line 279 "parser.y"
{ yyval.vlist = new_vlist(yypvt[-0].symbol, stype_any, yypvt[-2].vlist); } break;
case 54:
# line 280 "parser.y"
{ yyval.vlist = new_vlist(yypvt[-0].symbol, yypvt[-1].mtype, NULL); } break;
case 55:
# line 281 "parser.y"
{ yyval.vlist = new_vlist(yypvt[-0].symbol, stype_any, NULL); } break;
case 56:
# line 284 "parser.y"
{ yyval.mtype = stype_any; } break;
case 58:
# line 288 "parser.y"
{ yyval.mtype = find_type(yypvt[-0].symbol); } break;
case 59:
# line 291 "parser.y"
{ yyval.component = make_binary(b_cons, yypvt[-2].component, yypvt[-0].component); } break;
case 60:
# line 292 "parser.y"
{ yyval.component = make_binary(b_sc_or, yypvt[-2].component, yypvt[-0].component); } break;
case 61:
# line 293 "parser.y"
{ yyval.component = make_binary(b_or, yypvt[-2].component, yypvt[-0].component); } break;
case 62:
# line 294 "parser.y"
{ yyval.component = make_binary(b_sc_and, yypvt[-2].component, yypvt[-0].component); } break;
case 63:
# line 295 "parser.y"
{ yyval.component = make_binary(b_and, yypvt[-2].component, yypvt[-0].component); } break;
case 64:
# line 296 "parser.y"
{ yyval.component = make_binary(b_eq, yypvt[-2].component, yypvt[-0].component); } break;
case 65:
# line 297 "parser.y"
{ yyval.component = make_binary(b_ne, yypvt[-2].component, yypvt[-0].component); } break;
case 66:
# line 298 "parser.y"
{ yyval.component = make_binary(b_lt, yypvt[-2].component, yypvt[-0].component); } break;
case 67:
# line 299 "parser.y"
{ yyval.component = make_binary(b_le, yypvt[-2].component, yypvt[-0].component); } break;
case 68:
# line 300 "parser.y"
{ yyval.component = make_binary(b_gt, yypvt[-2].component, yypvt[-0].component); } break;
case 69:
# line 301 "parser.y"
{ yyval.component = make_binary(b_ge, yypvt[-2].component, yypvt[-0].component); } break;
case 70:
# line 302 "parser.y"
{ yyval.component = make_binary(b_bitor, yypvt[-2].component, yypvt[-0].component); } break;
case 71:
# line 303 "parser.y"
{ yyval.component = make_binary(b_bitxor, yypvt[-2].component, yypvt[-0].component); } break;
case 72:
# line 304 "parser.y"
{ yyval.component = make_binary(b_bitand, yypvt[-2].component, yypvt[-0].component); } break;
case 73:
# line 305 "parser.y"
{ yyval.component = make_binary(b_shift_left, yypvt[-2].component, yypvt[-0].component); } break;
case 74:
# line 306 "parser.y"
{ yyval.component = make_binary(b_shift_right, yypvt[-2].component, yypvt[-0].component); } break;
case 75:
# line 307 "parser.y"
{ yyval.component = make_binary(b_add, yypvt[-2].component, yypvt[-0].component); } break;
case 76:
# line 308 "parser.y"
{ yyval.component = make_binary(b_subtract, yypvt[-2].component, yypvt[-0].component); } break;
case 77:
# line 309 "parser.y"
{ yyval.component = make_binary(b_multiply, yypvt[-2].component, yypvt[-0].component); } break;
case 78:
# line 310 "parser.y"
{ yyval.component = make_binary(b_divide, yypvt[-2].component, yypvt[-0].component); } break;
case 79:
# line 311 "parser.y"
{ yyval.component = make_binary(b_remainder, yypvt[-2].component, yypvt[-0].component); } break;
case 80:
# line 312 "parser.y"
{ yyval.component = make_unary(b_negate, yypvt[-0].component); } break;
case 81:
# line 313 "parser.y"
{ yyval.component = make_unary(b_not, yypvt[-0].component); } break;
case 82:
# line 314 "parser.y"
{ yyval.component = make_unary(b_bitnot, yypvt[-0].component); } break;
case 86:
# line 320 "parser.y"
{ yyval.component = new_component(c_recall, yypvt[-0].symbol); } break;
case 87:
# line 321 "parser.y"
{ yyval.component = new_component(c_constant, yypvt[-0].constant); } break;
case 88:
# line 322 "parser.y"
{ yyval.component = new_component(c_constant, yypvt[-0].constant); } break;
case 89:
# line 323 "parser.y"
{ yyval.component = new_component(c_block, yypvt[-0].block); } break;
case 90:
# line 324 "parser.y"
{ yyval.component = yypvt[-1].component; } break;
case 91:
# line 328 "parser.y"
{ yyval.component = new_component(c_builtin, b_ref, 2, yypvt[-3].component, yypvt[-1].component); } break;
case 92:
# line 332 "parser.y"
{ yyval.component = new_component(c_execute, new_clist(yypvt[-3].component, yypvt[-1].clist)); } break;
case 93:
# line 335 "parser.y"
{ yyval.clist = NULL; } break;
case 94:
# line 336 "parser.y"
{ yyval.clist = reverse_clist(yypvt[-0].clist); } break;
case 95:
# line 339 "parser.y"
{ yyval.clist = new_clist(yypvt[-0].component, yypvt[-2].clist); } break;
case 96:
# line 340 "parser.y"
{ yyval.clist = new_clist(yypvt[-0].component, NULL); } break;
case 98:
# line 344 "parser.y"
{ yyval.constant = new_constant(cst_array, yypvt[-1].cstlist); } break;
case 99:
# line 345 "parser.y"
{ yyval.constant = new_constant(cst_list, yypvt[-1].cstlist); } break;
case 100:
# line 348 "parser.y"
{ yyval.constant = new_constant(cst_int, yypvt[-0].integer); } break;
case 101:
# line 349 "parser.y"
{ yyval.constant = new_constant(cst_string, yypvt[-0].string); } break;
case 102:
# line 352 "parser.y"
{ yyval.cstlist = NULL; } break;
case 103:
# line 353 "parser.y"
{ yyval.cstlist = new_cstlist(yypvt[-0].constant, yypvt[-1].cstlist); } break;
case 105:
# line 360 "parser.y"
{ yyval.block = new_codeblock(NULL, new_clist(yypvt[-2].component, NULL)); } break;
case 106:
# line 362 "parser.y"
{ yyval.block = new_codeblock(NULL, new_clist(yypvt[-3].component, yypvt[-1].clist)); } break;
case 107:
# line 364 "parser.y"
{ yyval.block = new_codeblock(yypvt[-3].vlist, yypvt[-1].clist); } break;
case 108:
# line 367 "parser.y"
{ yyval.vlist = NULL; } break;
case 110:
# line 371 "parser.y"
{ yyval.vlist = new_vlist(yypvt[-0].symbol, stype_none, yypvt[-2].vlist); } break;
case 111:
# line 372 "parser.y"
{ yyval.vlist = new_vlist(yypvt[-0].symbol, stype_none, NULL); } break;
	}
	goto yystack;		/* reset registers in driver code */
}
