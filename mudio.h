#ifndef MUDIO_H
#define MUDIO_H

#include "print.h"

/* Warning: mprint/mprintf/mputs may cause gc */

#ifdef MUME

typedef struct char_data *Mio;
typedef struct char_data *Muser;

#define mprintf cprintf
#define mputs cputs
#define mflush(x) 
#define EOL "\n\r"

#else

#include <stdio.h>

typedef FILE *Mio;
typedef int Muser;		/* Unused. */

#define mprintf fprintf
#define mputs fputs
#define mflush(x) fflush(x)
#define EOL "\n"

#endif

void mprint(Mio f, prt_level level, value v);

#include "context.h"

#define mudout session_context->_mudout
#define muduser session_context->_muduser
#define muderr session_context->_muderr

#endif
