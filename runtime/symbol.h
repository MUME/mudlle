#ifndef SYMBOL_H
#define SYMBOL_H

/* Support for mume instance symbol tables */

struct table *mume_table(struct dynpro *table);
value mume_ref(struct dynpro *tab, struct string *s);
value mume_set(struct dynpro *tab, struct string *s, value x);
value code_table_ref(struct table *tab, struct string *s);
value code_table_set(struct table *tab, struct string *s, value c);

void symbol_init(void);

#endif
