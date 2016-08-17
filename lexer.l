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

#include "calloc.h"
#include "charset.h"
#include "global.h"
#include "lexer.h"
#include "mparser.h"
#include "mudlle.h"
#include "mvalues.h"
#include "strbuf.h"
#include "tree.h"
#include "utils.h"

#  include "parser.tab.h"

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

static const char *base_name(int base);
static int string_read(char *buf, int max_size);

static YY_BUFFER_STATE mbuf;
static bool line_read;
static const char *lexer_string;
static const char *const *lexer_strings;
static int lexer_string_length;

const char *lexer_filename;
const char *lexer_nicename;
static bool force_constant;

struct lkeyword {
  const char *name;
  int value;
};

static const struct lkeyword keywords[] = {
  { "fn",       FUNCTION },
  { "if",       IF },
  { "else",     ELSE },
  { "while",    WHILE },
  { "exit",     LOOP_EXIT },
  { "loop",     LOOP },
  { "for",      FOR },
  { "match",    MATCH },

  { "library",  LIBRARY },
  { "module",   MODULE },
  { "requires", REQUIRES },
  { "reads",    READS },
  { "writes",   WRITES },
  { "static",   STATIC },
  { "defines",  DEFINES }
};
#define NKEYWORDS (sizeof keywords / sizeof(struct lkeyword))

#define MAKE_OP(oper, ret) do {			\
  yylval.operator.op = (oper);			\
  yylval.operator.lineno = yylineno;            \
  return (ret);					\
} while (0)

#define UNTERMINATED_STRING() do {                                      \
  yylineno = cstring.first_line;                                        \
  compile_error("unterminated %s constant",                             \
                cstring.is_char_const ? "character" : "string");        \
  yyterminate();                                                        \
} while (0)

static struct {
  struct strbuf sb;
  int first_line;
  bool is_char_const;
} cstring = { .sb = SBNULL };

static struct {
  int start_line;
  int level;
} ccomment;

static inline int hexval(int c)
{
  if (isdigit(c))
    return c - '0';
  return toupper(c) - 'A' + 10;
}

#define END_ESC_CHAR() do {                                     \
  if (cstring.is_char_const)                                    \
    {                                                           \
      BEGIN(INITIAL);                                           \
      assert(sb_len(&cstring.sb) == 1);                         \
      yylval.integer = (unsigned char)sb_str(&cstring.sb)[0];   \
      return INTEGER;                                           \
    }                                                           \
  BEGIN(STRING_CONST);                                          \
} while (0)

#define END_FLOAT() do {                                        \
 if (!mudlle_strtofloat(yytext, yyleng, &yylval.mudlle_float))  \
   {                                                            \
     compile_error("illegal floating point number");            \
     yyterminate();                                             \
   }                                                            \
 BEGIN(INITIAL);                                                \
 return FLOAT;                                                  \
} while (0)

static bool check_int(const char **str, size_t len, int *base,
                      const char *type, bool *is_float);

%}

%option nounput
%option noinput
%option yylineno

DIGIT           [0-9]
NONZERO		[1-9]
OCTDIGIT	[0-7]
EXP		[eE][+-]?{DIGIT}+
DECIM		\.{DIGIT}+
HEXDIGIT	[0-9a-fA-F]
GLOBAL_PREFIX   :
SYMBOL_NAME     [a-zA-Z][a-zA-Z0-9$_:?!]{0,1024}
BIGINT          ({NONZERO}{DIGIT}*|0([xX]{HEXDIGIT}*|[bB][01]*|{OCTDIGIT}*))
INT             ({NONZERO}{DIGIT}*|0([xX]{HEXDIGIT}*|[bB][01]*|{DIGIT}*))

/* used by read_constant() */
%x FC_START

%x STRING_CONST
%x SC_ESC
%x SC_NAME
%x DEC_PERIOD
%x HEX_PERIOD
%x FLOAT_EXP

%x LINE_COMMENT
%x COMMENT

%%

<FC_START>"'" {
          /* emit special symbol for starting quote */
	  BEGIN(INITIAL);
	  return FORCE_CONSTANT;
	}

<FC_START><<eof>> { abort(); }  /* should never happen */
<FC_START>.|\n { abort(); }     /* should never happen */

\n { }
[ \t\r]+  { }
"/"[/\*]? {
          if (yyleng == 1) return '/';
          if (yytext[1] == '*')
            {
              ccomment.start_line = yylineno;
              ccomment.level = 1;
              BEGIN(COMMENT);
            }
          else
            BEGIN(LINE_COMMENT);
        }

<LINE_COMMENT><<eof>> { BEGIN(INITIAL); }
<LINE_COMMENT>\n { BEGIN(INITIAL); }
<LINE_COMMENT>.* { }

<COMMENT>[^/\*\n] { }
<COMMENT>\n { }
<COMMENT><<eof>> {
          yylineno = ccomment.start_line;
          compile_error("unterminated comment");
          yyterminate();
        }

<COMMENT>\*\/? {
          if (yyleng == 2 && --ccomment.level == 0)
            BEGIN(INITIAL);
        }
<COMMENT>\/\*? {
          if (yyleng == 2)
            ++ccomment.level;
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
"%"	{ return '%'; }
"("	{ return '('; }
")"	{ return ')'; }
"{"	{ return '{'; }
"}"	{ return '}'; }
"&"	{ return '&'; }
"~"	{ return '~'; }
"^"	{ return '^'; }
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
"."{1,3} {
          if (yyleng == 1) return '.';
          if (yyleng == 3) return ELLIPSIS;
          compile_error("invalid '..' operator");
          yyterminate();
        }

,[ \n\t\r]? {
          yylval.tcomma.space_suffix = (yytext[1] != 0);
          return COMMA;
        }

\?([0-9A-Za-z_][a-zA-Z0-9$_:?!]?|.)? {
          cstring.is_char_const = true;
          if (yyleng == 1)
            UNTERMINATED_STRING();

          if (yyleng == 3)
            {
              compile_error("invalid character constant");
              yyterminate();
            }

          unsigned char c = yytext[1];
          if (c == '\\')
            {
              sb_empty(&cstring.sb);
              BEGIN(SC_ESC);
            }
          else if (!IS_8PRINT(c))
            {
              int esc = 0;
              switch (c)
                {
                case '\a': esc = 'a'; break;
                case '\b': esc = 'b'; break;
                case '\f': esc = 'f'; break;
                case '\n': esc = 'n'; break;
                case '\r': esc = 'r'; break;
                case '\t': esc = 't'; break;
                case '\v': esc = 'v'; break;
                }
              if (esc)
                compile_error("invalid character constant:"
                              " use '?\\%c'", esc);
              else
                compile_error("invalid character constant:"
                              " use '?\\%03o'", c);
              yyterminate();
            }
          else if (IS_8SPACE(c) || strchr("(){}[]\"", c) != NULL)
            {
              compile_error("'?%c' must be written '?\\%c'", c, c);
              yyterminate();
            }
          else
            {
              yylval.integer = yytext[1];
              return INTEGER;
            }
        }

{INT}[0-9A-Za-z_.]? {
          const char *s = yytext;
          int base;
          bool is_float;
          if (!check_int(&s, yyleng, &base, "integer", &is_float))
            yyterminate();

          if (!is_float)
            {
              if (!mudlle_strtolong(s, yytext + yyleng - s, &yylval.integer,
                                    base))
                {
                  compile_error("%sinteger constant out of range",
                                base_name(base));
                  yyterminate();
                }
              return INTEGER;
            }

          yymore();
          int last = yytext[yyleng - 1];
          if (last == 'p' || last == 'P')
            {
              assert(base == 16);
              BEGIN(FLOAT_EXP);
            }
          else if (last == 'e' || last == 'E')
            {
              assert(base == 10);
              BEGIN(FLOAT_EXP);
            }
          else
            {
              assert(last == '.');
              if (base == 10)
                BEGIN(DEC_PERIOD);
              else if (base == 16)
                BEGIN(HEX_PERIOD);
              else
                abort();
            }
        }

<DEC_PERIOD>{DIGIT}+[A-Za-z_]? {
          int last = yytext[yyleng - 1];
          if (last == 'e' || last == 'E')
            {
              yymore();
              BEGIN(FLOAT_EXP);
            }
          else if (!isdigit(last))
            {
              compile_error("invalid suffix in floating-point constant: %c",
                            last);
              yyterminate();
            }
          else
            END_FLOAT();
        }

<DEC_PERIOD>[^0-9] {
          compile_error("expected digit after decimal period");
          yyterminate();
        }

<DEC_PERIOD><<eof>> {
          compile_error("expected digit after decimal period");
          yyterminate();
        }

<HEX_PERIOD>{HEXDIGIT}+[A-Za-z_]? {
          int last = yytext[yyleng - 1];
          if (last == 'p' || last == 'P')
            {
              yymore();
              BEGIN(FLOAT_EXP);
            }
          else
            {
              compile_error("expected exponent in hexadecimal"
                            " floating-point constant");
              yyterminate();
            }
        }

<HEX_PERIOD><<eof>> {
          compile_error("expected hexadecimal digit after period");
          yyterminate();
        }

<FLOAT_EXP>[+-]?{DIGIT}*[A-Za-z_]? {
          int last = yytext[yyleng - 1];
          if (last == '+' || last == '-')
            {
              compile_error("expected exponent in floating-point constant");
              yyterminate();
            }
          if (!isdigit(last))
            {
              compile_error("invalid suffix in floating-point constant: %c",
                            last);
              yyterminate();
            }
          END_FLOAT();
        }

<FLOAT_EXP><<eof>> {
          compile_error("expected exponent in floating-point constant");
          yyterminate();
        }

[#]([bB]-?{BIGINT}?[0-9A-Za-z_]?)? {
	  if (yyleng == 1)
            {
              compile_error("# must be followed by b to start a bigint"
                            " constant");
              yyterminate();
            }
          const char *s = yytext + 2;
          bool neg = *s == '-';
          if (neg) ++s;
          int base;
          if (!check_int(&s, yytext + yyleng - s, &base, "bigint", NULL))
            yyterminate();
          size_t sz = yytext + yyleng - s + 1;
          yylval.bigint = allocate(parser_memory, sizeof *yylval.bigint + sz);
          strcpy(yylval.bigint->str, s);
          yylval.bigint->base = base;
          yylval.bigint->neg = neg;
	  return BIGINT;
        }

\"	{
          sb_empty(&cstring.sb);
          cstring.first_line = yylineno;
          cstring.is_char_const = false;
          BEGIN(STRING_CONST);
        }

<STRING_CONST>\" {
          BEGIN(INITIAL);
          size_t len = sb_len(&cstring.sb);
          yylval.string.len = len;
          yylval.string.str = allocate(parser_memory, len);
          memcpy(yylval.string.str, sb_str(&cstring.sb), len);
          if (len > 512)
            sb_free(&cstring.sb);
          return STRING;
        }

<STRING_CONST>[^\n\\\"]+ {
          sb_addmem(&cstring.sb, yytext, yyleng);
          if (sb_len(&cstring.sb) > MAX_STRING_SIZE)
            {
              compile_error("string length exceeds %ld characters",
                            (long)MAX_STRING_SIZE);
              yyterminate();
            }
        }

<STRING_CONST,SC_NAME>\n { UNTERMINATED_STRING(); }

<STRING_CONST,SC_NAME,SC_ESC><<eof>> { UNTERMINATED_STRING(); }

<STRING_CONST>\\ { BEGIN(SC_ESC); }

<SC_ESC>\n {
          if (cstring.is_char_const)
            UNTERMINATED_STRING();
          END_ESC_CHAR();
        }

<SC_ESC>N\{? {
          if (yytext[1] != '{')
            {
              compile_error("\\N must be followed by left curly"
                            " bracket ({)");
              yyterminate();
            }
          BEGIN(SC_NAME);
        }

<SC_NAME>[- A-Z]*[^A\n]? {
          int last = yytext[yyleng - 1];
          if (last != '}')
            {
              if (isupper(last) || last == '-' || last == ' ')
                compile_error("unterminated character name");
              else if (isprint(last) && !isspace(last))
                compile_error("invalid character in character name: %c",
                              last);
              else
                compile_error("invalid%s character in character name:"
                              " \\x%02x",
                              isspace(last) ? " space" : "",
                              last);
              yyterminate();
            }
          int c = lookup_named_character(yytext, yyleng - 1);
          if (c < 0)
            {
              compile_error("unknown named character");
              yyterminate();
            }
          sb_addc(&cstring.sb, c);
          END_ESC_CHAR();
        }

<SC_ESC>{OCTDIGIT}{1,3}[a-zA-Z0-9$_:?!]? {
          int last = yytext[yyleng - 1];
          /* check for invalid next character for octal constants */
          if ((last < '0' || last > '7') || yyleng > 3)
            {
              if (cstring.is_char_const)
                {
                  compile_error("invalid character constant");
                  yyterminate();
                }
              --yyleng;
            }
          else
            last = 0;
          int val = 0;
          for (int i = 0; i < yyleng; ++i)
            val = val * 8 + yytext[i] - '0';
          if (val > 255)
            {
              compile_error("invalid octal character: \\%03o", val);
              yyterminate();
            }
          sb_addc(&cstring.sb, val);
          if (last)
            sb_addc(&cstring.sb, last);
          END_ESC_CHAR();
        }

<SC_ESC>x{HEXDIGIT}{0,2}[a-zA-Z0-9$_:?!] {
          if (yyleng < 3 || !isxdigit(yytext[2])
              || (yyleng == 4 && cstring.is_char_const))
            {
              compile_error("invalid hexadecimal character constant");
              yyterminate();
            }
          sb_addc(&cstring.sb, hexval(yytext[1]) * 16 + hexval(yytext[2]));
          if (yyleng == 4)
            sb_addc(&cstring.sb, yytext[3]);
          END_ESC_CHAR();
        }

<SC_ESC>([A-Za-z_][a-zA-Z0-9$_:?!]?|.) {
          if (yyleng == 2 && cstring.is_char_const)
            {
              compile_error("invalid character constant");
              yyterminate();
            }
          int c = yytext[0];
          switch (c)
            {
            case 'a': c = '\a'; break;
            case 'b': c = '\b'; break;
            case 'f': c = '\f'; break;
            case 'n': c = '\n'; break;
            case 'r': c = '\r'; break;
            case 't': c = '\t'; break;
            case 'v': c = '\v'; break;
            }
          sb_addc(&cstring.sb, c);
          if (yyleng == 2)
            sb_addc(&cstring.sb, yytext[1]);
          END_ESC_CHAR();
        }

{GLOBAL_PREFIX}?{SYMBOL_NAME} {
          for (int i = 0; i < NKEYWORDS; i++)
            if (strcasecmp(yytext, keywords[i].name) == 0)
              return keywords[i].value;

          size_t len = yyleng;
          bool is_global = strncmp(yytext, GLOBAL_ENV_PREFIX,
                                   strlen(GLOBAL_ENV_PREFIX)) == 0;
          if (is_global)
            len -= strlen(GLOBAL_ENV_PREFIX);

          if (len > MAX_VARIABLE_LENGTH)
            {
              compile_error("symbol name exceeds %d characters",
                            MAX_VARIABLE_LENGTH);
              yyterminate();
            }

          size_t size = yyleng + 1;
          yylval.symbol = allocate(parser_memory, size);
          memcpy(yylval.symbol, yytext, size);
          return is_global ? GLOBAL_SYMBOL : SYMBOL;
        }

.	{
          unsigned char c = yytext[0];
          if (isprint(c))
            compile_error("bad character %c (%#02x)", c, c);
          else
            compile_error("bad character %#02x", c);
          yyterminate();
        }


%%

static const char *base_name(int base)
{
  switch (base)
    {
    case 2: return "binary ";
    case 8: return "octal ";
    case 10: return "decimal ";
    case 16: return "hexadecimal ";
    }
  return "";
}

/* set *base to detected base; set *str to start of digit sequence;
   return true if valid */
static bool check_int(const char **str, size_t len, int *base,
                      const char *type, bool *is_float)
{
  if (is_float)
    *is_float = false;

  const char *s = *str, *end = s + len;

  int b = 0;
  int last;
  if (s < end && *s == '0')
    {
      ++s;
      if (s == end)
        {
          *base = 8;
          return true;
        }
      switch (*s)
        {
        case 'b': case 'B':
          *str = ++s;
          b = 2;
          break;
        case 'x': case 'X':
          *str = ++s;
          b = 16;
          if (end > s)
            {
              last = end[-1];
              if (last == '.' || last == 'p' || last == 'P')
                {
                  if (is_float == NULL)
                    goto invalid_last;
                  *base = 16;
                  *is_float = true;
                  return true;
                }
            }
          break;
        default:
          b = 8;
          int non_octal = 0;
          do
            {
              last = *s++;
              if (last == '8' || last == '9')
                {
                  /* handle decimal floating-point constants with leading
                     zero */
                  if (is_float == NULL)
                    goto invalid_last;
                  non_octal = last;
                  /* only decimal digits can occur before the last character,
                     so we skip to it right away */
                  s = end;
                  last = s[-1];
                }
            }
          while (s < end);
          if (last == '.' || last == 'e' || last == 'E')
            {
              if (is_float == NULL)
                goto invalid_last;
              assert(s == end);
              *base = 10;
              *is_float = true;
              return true;
            }
          if (non_octal)
            {
              last = non_octal;
              goto invalid_last;
            }
          if (last < '0' || last > '7')
            goto invalid_last;
          *base = 8;
          return true;
        }
    }

  if (s == end)
    goto incomplete;

  if (b == 0)
    {
      last = *s;
      if (!isdigit(last))
        goto invalid_last;
      b = 10;
    }

  *base = b;

  last = end[-1];
  if (b == 10 && (last == '.' || last == 'e' || last == 'E'))
    {
      if (is_float == NULL)
        goto invalid_last;
      *is_float = true;
      return true;
    }

  if (b == 16
      ? !isxdigit(last)
      : last < '0' || last >= '0' + b)
    goto invalid_last;

  return true;

 invalid_last:
  compile_error("invalid %s in %s%s constant: %c",
                isdigit(last)
                ? "digit"
                : b == 0 ? "leading character" : "character",
                base_name(b), type, last);
  return false;

 incomplete:
  compile_error("incomplete %s%s constant", base_name(b), type);
  return false;
}

static int string_read(char *abuf, int max_size)
{
  int total = 0;
  for (;;)
    {
      if (lexer_string == NULL)
        return total;

      int cnt = (lexer_string_length < max_size
                 ? lexer_string_length
                 : max_size);
      memcpy(abuf, lexer_string, cnt);
      max_size -= cnt;
      lexer_string += cnt;
      lexer_string_length -= cnt;
      total += cnt;
      if (max_size == 0)
        return total;
      abuf += cnt;

      lexer_string = *++lexer_strings;
      lexer_string_length = lexer_string ? strlen(lexer_string) : 0;
    }
}

bool allow_comma_expression(void)
{
  return !force_constant;
}

void save_reader_state(struct reader_state *state)
{
  *state = (struct reader_state){
    .filename       = lexer_filename,
    .nicename       = lexer_nicename,
    .force_constant = force_constant,
  };
  lexer_filename = lexer_nicename = NULL;
}

void restore_reader_state(const struct reader_state *state)
{
  lexer_filename = state->filename;
  lexer_nicename = state->nicename;
  force_constant = state->force_constant;
  yylineno = 0xdeadbeef;
  line_read = false;
  lexer_strings = NULL;
  lexer_string = NULL;
  lexer_string_length = 0;
}

void read_from_file(FILE *f, const char *afilename, const char *anicename)
{
  assert(lexer_filename == NULL);
  assert(lexer_nicename == NULL);

  if (!mbuf)
    {
      mbuf = yy_create_buffer(f, YY_BUF_SIZE);
      yy_switch_to_buffer(mbuf);
    }
  else
    {
      yyrestart(f);
    }

  force_constant = false;
  line_read = false;
  yylineno = 1;
  lexer_filename = afilename;
  lexer_nicename = anicename;
  BEGIN(INITIAL);
}

void read_from_strings(const char *const *strs, const char *afilename,
                       const char *anicename, bool force_const)
{
  assert(lexer_filename == NULL);
  assert(lexer_nicename == NULL);

  if (!mbuf)
    {
      mbuf = yy_create_buffer(NULL, YY_BUF_SIZE);
      yy_switch_to_buffer(mbuf);
    }
  else
    yyrestart(NULL);

  yy_switch_to_buffer(mbuf);
  line_read = true;
  lexer_strings = strs;
  lexer_string = *strs;
  lexer_string_length = strlen(lexer_string);
  yylineno = 1;
  lexer_filename = afilename ? afilename : "<string>";
  lexer_nicename = anicename ? anicename : "<string>";
  force_constant = force_const;
  if (force_const)
    BEGIN(FC_START);
  else
    BEGIN(INITIAL);
}
