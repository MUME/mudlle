#include "mudlle-config.h"

#  include <assert.h>
#include <float.h>
#include <math.h>
#include <stdio.h>
#include <time.h>

#include "random.h"

static uint64_t random64(struct pcg32_random *rng);

/*
 * All pcg32 code below is derived from and modified from its original form:
 *
 * PCG Random Number Generation for C.
 *
 * Copyright 2014 Melissa O'Neill <oneill@pcg-random.org>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * For additional information about the PCG random number generation scheme,
 * including its license and other licensing options, visit
 *
 *     http://www.pcg-random.org
 */

static struct pcg32_random pcg32_shared;

#define PCG_DEFAULT_MULTIPLIER_64 UINT64_C(6364136223846793005)

static void pcg32_step_r(struct pcg32_random *rng)
{
  rng->state = rng->state * PCG_DEFAULT_MULTIPLIER_64 + rng->inc;
}

void pcg32_srandom_r(struct pcg32_random *rng,
                     uint64_t initstate, uint64_t initseq)
{
  rng->state = 0;
  rng->inc = (initseq << 1) | 1;
  pcg32_step_r(rng);
  rng->state += initstate;
  pcg32_step_r(rng);
}

void pcg32_srandom(struct pcg32_random *rng)
{
  uint64_t initstate = random64(&pcg32_shared);
  uint64_t initseq = random64(&pcg32_shared);
  pcg32_srandom_r(rng, initstate, initseq);
}

static uint32_t pcg32_random_r(struct pcg32_random *rng)
{
  uint64_t oldstate = rng->state;
  pcg32_step_r(rng);
  uint32_t xorshifted = ((oldstate >> 18) ^ oldstate) >> 27;
  uint32_t rot = oldstate >> 59;
  return (xorshifted >> rot) | (xorshifted << ((-rot) & 31));
}

static uint64_t random64(struct pcg32_random *rng)
{
  /* This should use two separate random states for high and low parts instead.
     Changing it in a backwards-compatible way is challenging.
     Cf. https://github.com/imneme/pcg-c/blob/master/sample/pcg32x2-demo.c */
  uint64_t h = (uint64_t)pcg32_random_r(rng) << 32;
  return h | pcg32_random_r(rng);
}

struct uint128 {
  uint64_t l, h;
};

static void mul_u64(struct uint128 *dst, uint64_t a, uint64_t b)
{
#ifdef HAVE___UINT128_T
  __uint128_t r = (__uint128_t)a * b;
  dst->l = r;
  dst->h = r >> 64;
#else
  /* from Hacker's Delight by Henry S. Warren, Jr. */
  uint32_t al = a, ah = a >> 32;
  uint32_t bl = b, bh = b >> 32;
  uint64_t albl = (uint64_t)al * bl;
  uint64_t albh = (uint64_t)al * bh;
  uint64_t ahbl = (uint64_t)ah * bl;
  uint64_t ahbh = (uint64_t)ah * bh;

  uint64_t s = albh + (albl >> 32);
  uint64_t t = ahbl + (uint32_t)s;

  dst->l = (uint32_t)albl + (t << 32);
  dst->h = ahbh + (s >> 32) + (t >> 32);
#endif
}

/* random_rangeNN() are based on "Fast Random Integer Generation in an
 * Interval" by Daniel Lemire */

/* returns a number in [0..range] */
uint64_t random_range64_r(struct pcg32_random *rng, uint64_t range)
{
  if (rng == NULL)
    rng = &pcg32_shared;

  uint64_t x = random64(rng);

  if (range == UINT64_MAX)
    return x;
  ++range;

  struct uint128 m;
  mul_u64(&m, x, range);
  uint64_t l = m.l;
  if (l < range)
    {
      uint64_t t = -range % range;
      while (l < t)
        {
          x = random64(rng);
          mul_u64(&m, x, range);
          l = m.l;
        }
    }
  return m.h;
}

/* returns a number in [0..range] */
uint32_t random_range32_r(struct pcg32_random *rng, uint32_t range)
{
  if (rng == NULL)
    rng = &pcg32_shared;

  uint32_t x = pcg32_random_r(rng);

  if (range == UINT32_MAX)
    return x;
  ++range;

  uint64_t m = (uint64_t)x * (uint64_t)range;
  uint32_t l = m;
  if (l < range)
    {
      uint32_t t = -range % range;
      while (l < t)
        {
          x = pcg32_random_r(rng);
          m = (uint64_t)x * (uint64_t)range;
          l = m;
        }
    }
  return m >> 32;
}

uint64_t random_range64(uint64_t range)
{
  return random_range64_r(&pcg32_shared, range);
}

uint32_t random_range32(uint32_t range)
{
  return random_range32_r(&pcg32_shared, range);
}

#if DBL_MANT_DIG <= 0 || DBL_MANT_DIG > 64
  #error Unsupported floating-point environment
#endif

/* a bit complicated to avoid 64-bit overflow; cf. assert() in random_init() */
#define DBL_MAX_MANT (~UINT64_C(0) >> (64 - DBL_MANT_DIG))

/* returns a number in [0..1) */
double drandom_r(struct pcg32_random *rng)
{
  /* not very good; cf. http://mumble.net/~campbell/tmp/random_real.c */
  return scalbn(random_range64_r(rng, DBL_MAX_MANT), -DBL_MANT_DIG);
}

double drandom(void)
{
  return drandom_r(&pcg32_shared);
}

void random_init(void)
{
  double drandom_max = scalbn(DBL_MAX_MANT, -DBL_MANT_DIG);
  assert(drandom_max < 1.0);
  assert(nextafter(drandom_max, 1.0) == 1.0);

  struct {
    uint64_t initstate, initseq;
  } seed;

  FILE *f = fopen("/dev/urandom", "r");
  if (f == NULL || fread(&seed, sizeof seed, 1, f) != 1)
    {
      seed.initstate = time(NULL);
      seed.initseq = (uintptr_t)&seed;
    }
  if (f != NULL)
    fclose(f);

  pcg32_srandom_r(&pcg32_shared, seed.initstate, seed.initseq);
}
