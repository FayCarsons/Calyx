#ifndef BLOOM_H
#define BLOOM_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

typedef uint64_t u64;
typedef uint32_t u32;
typedef uint8_t u8;
typedef size_t usize;

// Allocator interface
typedef struct {
  void *(*alloc)(usize);
  void *(*calloc)(usize, usize);
  void (*dealloc)(void *);
  void *(*realloc)(void *, usize);
} Allocator;

#define STDLIB_ALLOCATOR                                                       \
  ((Allocator){.alloc = malloc,                                                \
               .calloc = calloc,                                               \
               .dealloc = free,                                                \
               .realloc = realloc})

// Bit vector
typedef struct {
  u32 *mem;
  usize size;
} BitVec;

BitVec *BitVec_new(usize size, Allocator a);
void BitVec_free(BitVec *self, Allocator a);
bool BitVec_get(const BitVec *self, usize index);
void BitVec_set(BitVec *self, usize index);
void BitVec_clear(BitVec *self, usize index);

// Bloom filter using Kirsch-Mitzenmacher optimization
typedef struct {
  BitVec *vec;
  usize numHashes;
  usize count;
} BloomFilter;

BloomFilter *BloomFilter_new(usize size, usize numHashes, Allocator a);
void BloomFilter_free(BloomFilter *self, Allocator a);

void BloomFilter_put(BloomFilter *self, const void *data, usize len);
void BloomFilter_putStr(BloomFilter *self, const char *str);
void BloomFilter_putInt(BloomFilter *self, u64 key);

bool BloomFilter_test(const BloomFilter *self, const void *data, usize len);
bool BloomFilter_testStr(const BloomFilter *self, const char *str);
bool BloomFilter_testInt(const BloomFilter *self, u64 key);

#endif
