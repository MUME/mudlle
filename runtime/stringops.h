#ifndef STRING_H
#define STRING_H

value string_append(struct string *s1, struct string *s2);
value code_string_ref(struct string *str, value i);
value code_string_set(struct string *str, value i, value c);

void string_init(void);

#endif
