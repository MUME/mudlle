
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
extern YYSTYPE yylval;
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
