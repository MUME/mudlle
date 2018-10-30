#ifndef DWARF_H
#define DWARF_H

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

struct ary;
struct code;
struct string;

/* register array of mcode objects as belonging to GC generation gen */
void register_dwarf_mcodes(unsigned gen, struct ary *a);
/* clear registrations of mcode objects in GC generation gen */
void reset_dwarf_mcodes(unsigned gen);

struct lni_state {
  uint32_t addr, line;
};

struct string *dwarf_line_number_info(const struct lni_state *lco,
                                      size_t nlco);

uint32_t dwarf_lookup_line_number(struct code *code, uint32_t addr);

#endif  /* DWARF_H */
