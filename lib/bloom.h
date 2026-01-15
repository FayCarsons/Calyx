#ifndef BLOOM_H
#define BLOOM_H

#include "util.h"
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

// Utility
usize nextPowerOfTwo(usize n);

// Bit vector
typedef struct {
  u32 *mem;
  usize size;
} BitVec;

BitVec *BitVec_new(usize size, struct Allocator a);
void BitVec_free(BitVec *self, struct Allocator a);
bool BitVec_get(const BitVec *self, usize index);
void BitVec_set(BitVec *self, usize index);
void BitVec_clear(BitVec *self, usize index);

// Bloom filter using Kirsch-Mitzenmacher optimization
typedef struct {
  BitVec *vec;
  usize numHashes;
  usize count;
} BloomFilter;

BloomFilter *BloomFilter_new(usize size, usize numHashes, struct Allocator a);
void BloomFilter_free(BloomFilter *self, struct Allocator a);

void BloomFilter_put(BloomFilter *self, const void *data, usize len);
void BloomFilter_putStr(BloomFilter *self, const char *str);
void BloomFilter_putInt(BloomFilter *self, u64 key);

bool BloomFilter_test(const BloomFilter *self, const void *data, usize len);
bool BloomFilter_testStr(const BloomFilter *self, const char *str);
bool BloomFilter_testInt(const BloomFilter *self, u64 key);

#endif
