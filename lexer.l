/*                                                                    -*- c -*-
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
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include "mudlle.h"
#include "utils.h"
#include "mparser.h"
#include "lexer.h"
#include "env.h"

/* We never wrap into another file */
#define YY_SKIP_YYWRAP
#define yywrap() (1)

#define YY_USE_PROTOS

#undef YY_INPUT
#define YY_INPUT(buf, result, max_size) do {            \
  if (!line_read)                                       \
    (result) = fread((buf), 1, (max_size), yyin);       \
  else                                                  \
    (result) = string_read((buf), (max_size));          \
} while (0)

static int string_read(char *buf, int max_size);

static YY_BUFFER_STATE mbuf;
static int line_read;
static const char *line;
static int length;
int lineno;
const char *filename;

struct lkeyword {
  const char *name;
  int value;
};

static const struct lkeyword keywords[] = {
 { "fn",       FUNCTION }, 
  { "if",       IF },       
  { "else",     ELSE },     
  { "while",    WHILE },    
  { "or",       OR },       
  { "and",      AND },      
  { "not",      NOT },      
  { "exit",     MEXIT },    
  { "loop",     LOOP },     
  { "for",      FOR },      
  { "match",    MATCH },    

  { "library",  LIBRARY },  
  { "module",   MODULE },   
  { "requires", IMPORTS },  
  { "reads",    READS },    
  { "writes",   WRITES },   
  { "defines",  DEFINES }
};
#define NKEYWORDS (sizeof keywords / sizeof(struct lkeyword))

#define MAKE_OP(oper, ret) do {			\
  yylval.operator.op = (oper);			\
  yylval.operator.lineno = lineno;		\
  return (ret);					\
} while (0)

%}

DIGIT           [0-9]
NONZERODIGIT	[1-9]
OCTDIGIT	[0-7]
EXP		[eE][+-]?{DIGIT}+
DECIM		\.{DIGIT}+
HEXDIGIT	[0-9a-fA-F]
GLOBAL_PREFIX   :
SYMBOL_NAME     [a-zA-Z][a-zA-Z0-9$_:?!]*

%%

\n { lineno++; }
[ \t\r]+  { }
\/\/.*\n  { lineno++; }
\/\*     { /* start of multi-line comment */
	   /* find end of comment */
	   int depth = 1, c, star = FALSE, slash = FALSE;

#ifndef __cplusplus
#define yyinput input
#endif
	   while (depth > 0 && (c = yyinput()) != EOF)
	     {
	       if (c == '\n') lineno++;
	       else if (c == '*' && slash) depth++;
	       else if (c == '/' && star) depth--;
	       star = c == '*';
	       slash = c == '/';
	     }
         }
"&&"	{ return SC_AND; }
"||"	{ return SC_OR; }
"^^"    { return XOR; }
"!"	{ return NOT; }
"["	{ return '['; }
"]"	{ return ']'; }
"|"	{ return '|'; }
";"	{ return ';'; }
"+"	{ return '+'; }
"-"	{ return '-'; }
"*"	{ return '*'; }
"/"	{ return '/'; }
"%"	{ return '%'; }
"("	{ return '('; }
")"	{ return ')'; }
"{"	{ return '{'; }
"}"	{ return '}'; }
"&"	{ return '&'; }
"~"	{ return '~'; }
"^"	{ return '^'; }
","     { return ','; }
"."	{ return '.'; }
"+="	{ MAKE_OP(b_add, OP_ASSIGN); }
"-="	{ MAKE_OP(b_subtract, OP_ASSIGN); }
"*="	{ MAKE_OP(b_multiply, OP_ASSIGN); }
"/="	{ MAKE_OP(b_divide, OP_ASSIGN); }
"%="	{ MAKE_OP(b_remainder, OP_ASSIGN); }
"^="	{ MAKE_OP(b_bitxor, OP_ASSIGN); }
"&="	{ MAKE_OP(b_bitand, OP_ASSIGN); }
"|="	{ MAKE_OP(b_bitor, OP_ASSIGN); }
"&&="	{ MAKE_OP(b_sc_and, OP_ASSIGN); }
"^^="	{ MAKE_OP(b_xor, OP_ASSIGN); }
"||="	{ MAKE_OP(b_sc_or, OP_ASSIGN); }
">>="	{ MAKE_OP(b_shift_right, OP_ASSIGN); }
"<<="	{ MAKE_OP(b_shift_left, OP_ASSIGN); }
"="	{ return ASSIGN; }
"=="	{ return EQ; }
"!="	{ return NE; }
"<"	{ return LT; }
"<="	{ return LE; }
">"	{ return GT; }
">="	{ return GE; }
"<<"	{ return SHIFT_LEFT; }
">>"	{ return SHIFT_RIGHT; }
"++"    { MAKE_OP(b_add, INCREMENTER); }
"--"    { MAKE_OP(b_subtract, INCREMENTER); }
"'"	{ return QUOTE; }
"@"	{ return '@'; }
"=>"    { return PATTERN_MATCH; }
"_"     { return SINK; }
"..."   { return ELLIPSIS; }

\?\\(x{HEXDIGIT}{HEXDIGIT}|{OCTDIGIT}{OCTDIGIT}?{OCTDIGIT}?|.) {
          char c;
          convert_escape_char(yytext + 2, &c);
          yylval.integer = (unsigned char)c;
          return INTEGER;
        }

\?.	{
	  yylval.integer = (unsigned char)yytext[1];
          return INTEGER;
	}

-?({DIGIT}+|0[xX]{HEXDIGIT}+) {
	  if (!mudlle_strtoint(yytext, &yylval.integer))
            { 
	      compile_error("integer constant out of bounds");
              yyterminate();
            }
	  else
            return INTEGER; 
	}

0[fF]{HEXDIGIT}+ { 
	  if (strlen(yytext) != 2 + 16)
	    compile_error("0f-floats must consist of exactly 16 hex digits");
	  else if (!mudlle_strtofloat(yytext, &yylval.mudlle_float))
	    compile_error("illegal floating point number");
	  else
	    return FLOAT; 
          yyterminate();
	}

[#][bB]-?(0{OCTDIGIT}*|{NONZERODIGIT}{DIGIT}*|0[xX]{HEXDIGIT}+)	{
          yylval.bigint_str = allocate(parser_memory, strlen(yytext) - 1);
          strcpy(yylval.bigint_str, yytext + 2);
	  return BIGINT;
        }

-?{DIGIT}+(({DECIM}{EXP}?)|{EXP}) {
          if (!mudlle_strtofloat(yytext, &yylval.mudlle_float))
            {
	      compile_error("illegal floating point number");
              yyterminate();
            }
	  else
            return FLOAT;
	}

\"([^\n\\\"]*(\\(.|\n))?)+\" {
          char *str = yylval.string.str =
            allocate(parser_memory, strlen(yytext));
          const char *text = yytext + 1;
          
          while (*text)
            if (*text == '\\')
              {
                char c;

                if (text[1] == '\n')
                  {
                    text += 2;
                    lineno++;
                  }
                else
                  {
                    text += convert_escape_char(text + 1, &c) + 1;
                    *str++ = c;
                  }
              }
            else
              *str++ = *text++;
          
          *--str = '\0';
          yylval.string.len = str - yylval.string.str;
          return STRING;
	}

{GLOBAL_PREFIX}?{SYMBOL_NAME} { 
          int i, global;
          
          for (i = 0; i < NKEYWORDS; i++)
            if (stricmp(yytext, keywords[i].name) == 0)
              return keywords[i].value;
          
          yylval.symbol = allocate(parser_memory,
                                   strlen(yytext) + 1);
          strlwr(strcpy(yylval.symbol, yytext));
          global = strncmp(yytext, GLOBAL_ENV_PREFIX,
                           strlen(GLOBAL_ENV_PREFIX)) == 0;
          return global ? GLOBAL_SYMBOL : SYMBOL;
        }

.	{ 
          int c = (unsigned char)yytext[0];
          if (isprint(c))
            compile_error("bad character %c (%#02x)", c, c);
          else
            compile_error("bad character %#02x", c);
          yyterminate();
        }
                      

%%

static int string_read(char *abuf, int max_size)
{
  int cnt;

  cnt = max_size;
  if (cnt > length) cnt = length;

  memcpy(abuf, line, cnt);
  line += cnt;
  length -= cnt;

  return cnt;
}


void read_from_file(FILE *f, const char *name)
{
  if (!mbuf)
    {
      mbuf = yy_create_buffer(f, YY_BUF_SIZE);
      yy_switch_to_buffer(mbuf);
    }
  else
    {
      yyrestart(f);
      
    }

  line_read = FALSE;
  lineno = 1;
  filename = name;
}

void read_from_string(const char *str, const char *name)
{
  if (!mbuf)
    {
      mbuf = yy_create_buffer(stdin, YY_BUF_SIZE);
      yy_switch_to_buffer(mbuf);
    }
  else
    yyrestart(stdin);

  yy_switch_to_buffer(mbuf);
  line_read = TRUE;
  line = str;
  length = strlen(line);
  lineno = 0;
  filename = name ? name : "<string>";
}

void dummy_function_lexer_l(void)
{
  /* removes warning about yyunput being unused */
  volatile void *x = yyunput;
  (void)x;
}

size_t convert_escape_char(const char *text, char *result)
{
  switch (text[0])
    {
    case 'a': *result = '\a'; break;
    case 'b': *result = '\b'; break;
    case 'f': *result = '\f'; break;
    case 'n': *result = '\n'; break;
    case 'r': *result = '\r'; break;
    case 't': *result = '\t'; break;
    case 'v': *result = '\v'; break;
    case '0': case '1': case '2': case '3':
    case '4': case '5': case '6': case '7': {
      int n = 0, i;
      for (i = 0; i < 3; ++i)
        {
          if (text[i] < '0' || text[i] > '7')
            break;
          n = n * 8 | (text[i] - '0');
        }
      *result = n;
      return i;
    }
    case 'x':
      if (isxdigit(text[1]) && isxdigit(text[2]))
        {
          int n, c = toupper(text[1]);
          if (isdigit(c))
            n = c - '0';
          else
            n = c - 'A' + 10;
          n *= 16;
          c = toupper(text[2]);
          if (isdigit(c))
            n += c - '0';
          else
            n += c - 'A' + 10;
          *result = n;
          return 3;
        }
      /* fallthrough */
    default:
      *result = text[0];
      break;
    }
  return 1;
}
