#ifndef COMPILE_H
#define COMPILE_H

#include "tree.h"

value make_constant(constant c);
struct string *make_filename(const char *fname);
int interpret(value *result, int seclev, int reload);

void compile_init(void);

#endif
