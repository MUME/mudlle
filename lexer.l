%{
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include "mudlle.h"
#include "utils.h"
#include "mparser.h"
#include "lexer.h"

/* We never wrap into another file */
#undef yywrap
#define yywrap() (1)


#undef YY_INPUT
#define YY_INPUT(buf, result, max_size) \
  { if (!line_read) (result) = read(fileno(yyin), (buf), (max_size)); \
  else (result) = string_read((buf), (max_size)); }

static int string_read(char *buf, int max_size);

static YY_BUFFER_STATE buf;
static int line_read;
static const char *line;
static int length;
int lineno;
const char *filename;

struct keyword {
  const char *name;
  int value;
};

static struct keyword keywords[] = {
  { "fn", FUNCTION },
  { "if", IF },
  { "else", ELSE },
  { "while", WHILE },
  { "or", OR },
  { "and", AND },
  { "not", NOT },
  { "exit", MEXIT },
  { "loop", LOOP },

  { "library", LIBRARY },
  { "module", MODULE },
  { "requires", IMPORTS },
  { "reads", READS },
  { "writes", WRITES },
  { "defines", DEFINES }
};
#define NKEYWORDS (sizeof keywords / sizeof(struct keyword))

%}

%%

\n { lineno++; }
[ \t\r]+  { }
\/\/.*\n  { lineno++; }
\/\*     { /* start of multi-line comment */
	   /* find end of comment */
	   int depth = 1, c, star = FALSE, slash = FALSE;

	   while (depth > 0 && (c = input()) != EOF)
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
"&"	{ return '&'; }
"~"	{ return '~'; }
"^"	{ return '^'; }
","     { return ','; }
"."	{ return '.'; }
"="	{ return ASSIGN; }
"=="	{ return EQ; }
"!="	{ return NE; }
"<"	{ return LT; }
"<="	{ return LE; }
">"	{ return GT; }
">="	{ return GE; }
"<<"	{ return SHIFT_LEFT; }
">>"	{ return SHIFT_RIGHT; }
"'"	{ return QUOTE; }

\?\\.	{
	  switch (yytext[2])
	    {
	    case 'n': yylval.integer = '\n'; break;
	    case 'r': yylval.integer = '\r'; break;
	    case 't': yylval.integer = '\t'; break;
	    default: yylval.integer = yytext[2]; break;
	    }
	  return INTEGER;
	}
\?.	{ yylval.integer = yytext[1]; return INTEGER; }
-?[0-9]+ { yylval.integer = atoi(yytext); return INTEGER; }
\"([^\n\\"]*(\\(.|\n))?)+\" { char *str = yylval.string = allocate(memory,
							    strlen(yytext));
			      const char *text = yytext + 1;

			      while (*text)
				if (*text == '\\' && text[1])
				  {
				    switch (text[1])
				      {
				      case '\n': lineno++; break;
				      case 'n': *str++ = '\n'; break;
				      case 'r': *str++ = '\r'; break;
				      case 't': *str++ = '\t'; break;
				      default: *str++ = text[1]; break;
				      }
				    text += 2;
				  }
				else *str++ = *text++;

			      str[-1] = '\0';
			      return STRING;
			    }
[a-zA-Z_:][a-zA-Z0-9$_:?!]* { 
			      int i;

			      for (i = 0; i < NKEYWORDS; i++)
			        if (stricmp(yytext, keywords[i].name) == 0)
			          return keywords[i].value;

			      yylval.symbol = allocate(memory,
							  strlen(yytext) + 1);
			      strlwr(strcpy(yylval.symbol, yytext));
			      return SYMBOL; 
			    }

.	{ error("Bad character %s(%02x)\n", yytext, (unsigned char)yytext[0]); }

%%

static int string_read(char *buf, int max_size)
{
  int cnt;

  cnt = max_size;
  if (cnt > length) cnt = length;

  memcpy(buf, line, cnt);
  line += cnt;
  length -= cnt;

  return cnt;
}


void read_from_file(FILE *f, const char *name)
{
  if (!buf) buf = yy_create_buffer(f, YY_BUF_SIZE);
  else yy_init_buffer(buf, f);
  
  yy_current_buffer = 0;
  yy_switch_to_buffer(buf);
  line_read = FALSE;
  lineno = 1;
  filename = name;
}

void read_from_string(const char *str)
{
  if (!buf) buf = yy_create_buffer(stdin, YY_BUF_SIZE);
  else yy_init_buffer(buf, stdin);
  
  yy_current_buffer = 0;
  yy_switch_to_buffer(buf);
  line_read = TRUE;
  line = str;
  length = strlen(line);
  lineno = 0;
  filename = "<string>";
}
