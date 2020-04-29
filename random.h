#ifndef RANDOM_H
#define RANDOM_H

#include "mudlle-config.h"

#include <stdint.h>

struct pcg32_random {
  uint64_t state, inc;
};

/* initialize 'rng' with a random seed */
void pcg32_srandom(struct pcg32_random *rng);
/* initialize 'rng' with the specified state */
void pcg32_srandom_r(struct pcg32_random *rng,
                     uint64_t initstate, uint64_t initseq);

/* returns a number in [0..range] */
uint32_t random_range32(uint32_t range);
uint64_t random_range64(uint64_t range);
uint32_t random_range32_r(struct pcg32_random *rng, uint32_t range);
uint64_t random_range64_r(struct pcg32_random *rng, uint64_t range);

/* returns a number in [0..1) */
double drandom(void);
double drandom_r(struct pcg32_random *rng);

void random_init(void);

#endif  /* RANDOM_H */
