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

/* A profiler for mudlle, based on the dump files */

#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <sys/stat.h>
#include <sys/types.h>

#include "alloc.h"
#include "types.h"

#undef calloc
#undef malloc
#undef exit
#undef realloc

ubyte *posgen0, *endgen0, *datagen0;
ubyte *startgen1, *endgen1, *datagen1;

#define MAXNAME 32

char (*primitives)[MAXNAME];
int nb_primitives;

void load_profile_data(void)
{
  struct stat sb;
  int fd;

  /* Read primitive names */
  fd = open("lib/mudlle-primitives", O_RDONLY);
  if (fstat(fd, &sb) != 0)
    {
      perror("primitives");
      exit(2);
    }
  primitives = (void *)malloc(sb.st_size);
  read(fd, primitives, sb.st_size);
  close(fd);
  nb_primitives = sb.st_size / MAXNAME;

  /* Read GC data */
  fd = open("lib/mudlle-memory.dump", O_RDONLY);

  if (read(fd, &startgen1, sizeof startgen1) != sizeof startgen1 ||
      read(fd, &endgen1, sizeof endgen1) != sizeof endgen1 ||
      startgen1 > endgen1)
    {
      perror("read data");
      exit(2);
    }
  datagen1 = malloc(endgen1 - startgen1);
  if (read(fd, datagen1, endgen1 - startgen1) != endgen1 - startgen1 ||
      read(fd, &posgen0, sizeof posgen0) != sizeof posgen0 ||
      read(fd, &endgen0, sizeof endgen0) != sizeof endgen0 ||
      posgen0 > endgen0)
    {
      perror("read data");
      exit(2);
    }
  datagen0 = malloc(endgen0 - posgen0);
  if (read(fd, datagen0, endgen0 - posgen0) != endgen0 - posgen0)
    {
      perror("read data");
      exit(2);
    }
  close(fd);
}

void *cp(void *ptr)
/* Returns: ptr converted to the current address of generations 0 & 1
*/
{
  ubyte *p = ptr;

  if (p >= posgen0 && p < endgen0) return datagen0 + (p - posgen0);
  else if (p >= startgen1 && p < endgen1) return datagen1 + (p - startgen1);
  else abort();
}

struct pp {
  char *name;
  ulong count;
} *info_primitives;

int order_primitives(const void *_p1, const void *_p2)
{
  return (int)(((struct pp *)_p2)->count - ((struct pp *)_p1)->count);
}

static ubyte *primitive_scan(ubyte *ptr, ubyte *end)
{
  struct obj *obj;
  struct primitive *op;

  while (ptr < end && *(ulong *)ptr == 0) ptr += sizeof(ulong);
  if (ptr == end) return ptr;
  obj = (struct obj *)ptr;

  ptr += MUDLLE_ALIGN(obj->size, sizeof(value));
  switch (obj->type)
    {
    case type_primitive: case type_secure: case type_varargs:
      op = (struct primitive *)obj;
      assert(op->nb < nb_primitives);
      info_primitives[op->nb].name = primitives[op->nb];
      info_primitives[op->nb].count = op->call_count;
      break;
    }
  return ptr;
}

void profile_primitives(int show_unused)
{
  ubyte *scan, *end0 = datagen0 + (endgen0 - posgen0),
    *end1 = datagen1 + (endgen1 - startgen1);
  int i;

  info_primitives = (void *)calloc(nb_primitives, sizeof(*info_primitives));
  for (scan = datagen0; scan < end0; ) scan = primitive_scan(scan, end0);
  for (scan = datagen1; scan < end1; ) scan = primitive_scan(scan, end1);

  qsort(info_primitives, nb_primitives, sizeof *info_primitives,
	order_primitives);

  for (i = 0; i < nb_primitives; i++)
    if (info_primitives[i].count > 0 || show_unused)
      printf("%-32s %9ld\n", info_primitives[i].name, info_primitives[i].count);
}

struct pm
{
  const char *varname;
  const char *filename;
  int lineno;
  ulong instructions;
  ulong calls;
  int ratio;
} *info_mudlle;

int info_mudlle_size, info_mudlle_used = -1;

void extend_info_mudlle(void)
{
  info_mudlle_used++;
  if (info_mudlle_used < info_mudlle_size) return;
  info_mudlle_size += 200;
  if (info_mudlle)
    info_mudlle = (void *)realloc(info_mudlle, info_mudlle_size * sizeof(struct pm));
  else
    info_mudlle = (void *)malloc(info_mudlle_size * sizeof(struct pm));
}

int order_mudlle_call(const void *_p1, const void *_p2)
{
  return (int)(((struct pm *)_p2)->calls - ((struct pm *)_p1)->calls);
}

int order_mudlle_ins(const void *_p1, const void *_p2)
{
  return (int)(((struct pm *)_p2)->instructions - ((struct pm *)_p1)->instructions);
}

int order_mudlle_ratio(const void *_p1, const void *_p2)
{
  return ((struct pm *)_p2)->ratio - ((struct pm *)_p1)->ratio;
}

static ubyte *mudlle_scan(ubyte *ptr, ubyte *end, int show_unused)
{
  struct obj *obj;

  while (ptr < end && *(ulong *)ptr == 0) ptr += sizeof(ulong);
  if (ptr == end) return ptr;
  obj = (struct obj *)ptr;

  ptr += MUDLLE_ALIGN(obj->size, sizeof(value));
  switch (obj->type)
    {
    case type_code: ;
      struct icode *code = (struct icode *)obj;
      if (code->call_count > 0 || show_unused)
	{
	  extend_info_mudlle();
	  if (code->code.varname)
	    info_mudlle[info_mudlle_used].varname =
	      ((struct string *)cp(code->code.varname))->str;
	  else
	    info_mudlle[info_mudlle_used].varname = "<fn>";
	  info_mudlle[info_mudlle_used].filename =
	    ((struct string *)cp(code->code.filename))->str;
	  info_mudlle[info_mudlle_used].lineno = code->code.lineno;
	  info_mudlle[info_mudlle_used].instructions = code->instruction_count;
	  info_mudlle[info_mudlle_used].calls = code->call_count;
	  if (code->call_count > 0)
	    info_mudlle[info_mudlle_used].ratio =
	      code->instruction_count / code->call_count;
	  else
	    info_mudlle[info_mudlle_used].ratio = 0;
	}
      break;
    }
  return ptr;
}

void profile_mudlle(int show_unused, int sort_method)
{
  ubyte *scan, *end0 = datagen0 + (endgen0 - posgen0),
    *end1 = datagen1 + (endgen1 - startgen1);
  int i;

  info_mudlle = NULL;
  for (scan = datagen0; scan < end0; ) scan = mudlle_scan(scan, end0, show_unused);
  for (scan = datagen1; scan < end1; ) scan = mudlle_scan(scan, end1, show_unused);

  if (info_mudlle_used >= 0)
    {
      qsort(info_mudlle, 1 + info_mudlle_used, sizeof *info_mudlle,
	    sort_method == 1 ? order_mudlle_ins :
	    sort_method == 2 ? order_mudlle_call :
	    order_mudlle_ratio);

      printf("%-41s %13s %9s %13s\n",
	     "name", "insns", "calls", "insn/call");
      printf("-------------------------------------------------------------------------------\n");
      for (i = 0; i <= info_mudlle_used; i++)
	{
	  char tmp[512];

	  sprintf(tmp, "%s[%s:%d]", info_mudlle[i].varname,
		  info_mudlle[i].filename, info_mudlle[i].lineno);
	  tmp[41] = '\0';
	  printf("%-41s %13ld %9ld %13d\n", tmp,
		 info_mudlle[i].instructions, info_mudlle[i].calls,
		 info_mudlle[i].ratio);
	}
    }
}

int main(int argc, char **argv)
{
  int prims = false, mudlle = false, unused = false, c;
  int sort_method = 1;

  while ((c = getopt(argc, argv, "pmu123")) != -1)
    switch (c)
      {
      case 'p':
	prims = true;
	break;
      case 'm':
	mudlle = true;
	break;
      case 'u':
	unused = true;
	break;
      case '1': case '2': case '3':
	sort_method = c - '0';
	break;
      case '?':
	fprintf(stderr, "Usage: %s [-pmu123]\n", argv[0]);
	return 2;
      }

  if (!prims && !mudlle) prims = mudlle = true;

  load_profile_data();

  if (prims) profile_primitives(unused);
  if (mudlle) profile_mudlle(unused, sort_method);
}
