/*                                                                    -*- c -*-
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
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

#include "charset.h"
#include "env.h"
#include "lexer.h"
#include "mparser.h"
#include "mudlle.h"
#include "tokens.h"
#include "utils.h"

/* We never wrap into another file */
#define YY_SKIP_YYWRAP
#define yywrap() (1)

#define YY_USE_PROTOS

#define YY_USER_ACTION do {                             \
  yylloc.first_line = yylloc.last_line = yylineno;      \
} while (0);

#undef YY_INPUT
#define YY_INPUT(buf, result, max_size) do {            \
  if (!line_read)                                       \
    (result) = fread((buf), 1, (max_size), yyin);       \
  else                                                  \
    (result) = string_read((buf), (max_size));          \
} while (0)

static int string_read(char *buf, int max_size);
static bool convert_escape_char(const char **textp, char *result);

static YY_BUFFER_STATE mbuf;
static int line_read;
static const char *line;
static int length;
const char *lexer_filename;
const char *lexer_nicename;

struct lkeyword {
  const char *name;
  int value;
};

static const struct lkeyword keywords[] = {
  { "fn",       FUNCTION },
  { "if",       IF },
  { "else",     ELSE },
  { "while",    WHILE },
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
  yylval.operator.lineno = yylineno;            \
  return (ret);					\
} while (0)

%}

%option nounput
%option yylineno

DIGIT           [0-9]
NONZERODIGIT	[1-9]
OCTDIGIT	[0-7]
EXP		[eE][+-]?{DIGIT}+
DECIM		\.{DIGIT}+
HEXDIGIT	[0-9a-fA-F]
GLOBAL_PREFIX   :
SYMBOL_NAME     [a-zA-Z][a-zA-Z0-9$_:?!]*
CHAR_NAME       N\{[^\}\"\n]*\}?

%%

\n { }
[ \t\r]+  { }
\/\/.*\n  { }
\/\*     { /* start of multi-line comment */
	   /* find end of comment */
           int depth = 1;
           bool star = false, slash = false;
           int start_line = yylineno;

#ifndef __cplusplus
#define yyinput input
#endif
	   while (depth > 0)
	     {
               int c = yyinput();
               if (c == EOF)
                 {
                   yylineno = start_line;
                   compile_error("unterminated comment");
                   yyterminate();
                 }
	       else if (c == '*' && slash) depth++;
	       else if (c == '/' && star) depth--;
	       star = c == '*';
	       slash = c == '/';
	     }
         }
"&&"	{ return SC_AND; }
"||"	{ return SC_OR; }
"^^"    { return XOR; }
"!"	{ return '!'; }
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
"+="	{ return ASSIGN_ADD; }
"&="	{ return ASSIGN_BIT_AND; }
"|="	{ return ASSIGN_BIT_OR; }
"^="	{ return ASSIGN_BIT_XOR; }
"/="	{ return ASSIGN_DIV; }
"*="	{ return ASSIGN_MUL; }
"%="	{ return ASSIGN_REM; }
"&&="	{ return ASSIGN_SC_AND; }
"||="	{ return ASSIGN_SC_OR; }
"<<="	{ return ASSIGN_SHL; }
">>="	{ return ASSIGN_SHR; }
"-="	{ return ASSIGN_SUB; }
"^^="	{ return ASSIGN_XOR; }
"="	{ return ASSIGN; }
"=="	{ return EQ; }
">="	{ return GE; }
">"	{ return GT; }
"<="	{ return LE; }
"<"	{ return LT; }
"!="	{ return NE; }
"<<"	{ return SHIFT_LEFT; }
">>"	{ return SHIFT_RIGHT; }
"--"    { return DECREMENT; }
"++"    { return INCREMENT; }

"@"	{ return '@'; }
"'"	{ return QUOTE; }
"=>"    { return PATTERN_MATCH; }
"_"     { return SINK; }
"..."   { return ELLIPSIS; }

\?\\(x{HEXDIGIT}{HEXDIGIT}|{OCTDIGIT}{OCTDIGIT}?{OCTDIGIT}?|{CHAR_NAME}|.) {
          char c;
          const char *text = yytext + 2;
          if (!convert_escape_char(&text, &c))
            yyterminate();
          assert(*text == 0);
          yylval.integer = (unsigned char)c;
          return INTEGER;
        }

\?.	{
	  yylval.integer = (unsigned char)yytext[1];
          if (strchr("({[]})\"\\", yylval.integer) != NULL)
            {
              compile_error("'?%c' must be written '?\\%c'",
                            yylval.integer, yylval.integer);
              yyterminate();
            }
          return INTEGER;
	}

{DIGIT}+|0[xX]{HEXDIGIT}+ {
	  if (!mudlle_strtoint(yytext, &yylval.integer))
            {
	      compile_error("integer constant out of bounds");
              yyterminate();
            }
	  else
            return INTEGER;
	}

0[xX]({HEXDIGIT}+(\.{HEXDIGIT}*)?|\.{HEXDIGIT}+)[pP][-+]?{DIGIT}+ {
          if (!mudlle_strtofloat(yytext, &yylval.mudlle_float))
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

{DIGIT}+(({DECIM}{EXP}?)|{EXP}) {
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
                if (text[1] == '\n')
                  {
                    text += 2;
                  }
                else
                  {
                    ++text;
                    char c;
                    if (!convert_escape_char(&text, &c))
                      yyterminate();
                    *str++ = c;
                  }
              }
            else
              *str++ = *text++;

          *--str = '\0';
          yylval.string.len = str - yylval.string.str;
          yylloc.last_line = yylineno;
          return STRING;
	}

{GLOBAL_PREFIX}?{SYMBOL_NAME} {
          for (int i = 0; i < NKEYWORDS; i++)
            if (stricmp(yytext, keywords[i].name) == 0)
              return keywords[i].value;

          size_t size = strlen(yytext) + 1;
          yylval.symbol = allocate(parser_memory, size);
          memcpy(yylval.symbol, yytext, size);

          bool global = strncmp(yytext, GLOBAL_ENV_PREFIX,
                                strlen(GLOBAL_ENV_PREFIX)) == 0;
          return global ? GLOBAL_SYMBOL : SYMBOL;
        }

\"      {
          compile_error("unterminated string constant");
          yyterminate();
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


void read_from_file(FILE *f, const char *afilename, const char *anicename)
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

  line_read = false;
  yylineno = 1;
  lexer_filename = afilename;
  lexer_nicename = anicename;
}

void read_from_string(const char *str, const char *afilename,
                      const char *anicename)
{
  if (!mbuf)
    {
      mbuf = yy_create_buffer(stdin, YY_BUF_SIZE);
      yy_switch_to_buffer(mbuf);
    }
  else
    yyrestart(stdin);

  yy_switch_to_buffer(mbuf);
  line_read = true;
  line = str;
  length = strlen(line);
  yylineno = 0;
  lexer_filename = afilename ? afilename : "<string>";
  lexer_nicename = anicename ? anicename : "<string>";
}

static bool convert_escape_char(const char **textp, char *result)
{
  const char *text = *textp;
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
      *textp += i;
      return true;
    }
    case 'N':
      {
        if (text[1] != '{')
          {
            compile_error("\\N must be followed by left curly bracket ({)");
            return false;
          }
        text += 2;
        const char *end;
        for (end = text; *end != '}'; ++end)
          if (*end == 0 || *end == '\n')
            {
              compile_error("unterminated \\N character name");
              return false;
            }

        int c = lookup_named_character(text, end - text);
        if (c < 0)
          {
            compile_error("unknown named character");
            return false;
          }
        *result = c;
        *textp = end + 1;
        return true;
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
          *textp += 3;
          return true;
        }
      else
        {
          compile_error("invalid hexadecimal character constant");
          return false;
        }
    default:
      *result = text[0];
      break;
    }
  ++*textp;
  return true;
}
