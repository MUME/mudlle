#ifndef VECTOR_H
#define VECTOR_H

value code_vector_ref(struct vector *vec, value i);
value code_vector_set(struct vector *vec, value i, value c);

void vector_init(void);

#endif
