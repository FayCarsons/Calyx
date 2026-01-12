#include <__stdarg_va_list.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

// Integer types
typedef uint64_t u64;
typedef size_t usize;
typedef uint8_t u8;
typedef uint32_t u32;

// Function types
typedef u32 (*hash32f)(void *);
typedef void *(*allocate)(usize);
typedef void (*deallocate)(void *, usize);

u64 fnv1a(const void *data, usize len) {
  const u8 *p = data;
  u64 h = 0xcbf29ce484222325ULL;
  for (usize i = 0; i < len; i++)
    h = (h ^ p[i]) * 0x100000001b3ULL;
  return h;
}

struct BitVec {
  u32 *mem;
  usize size;
};

struct BloomFilter {
  struct BitVec *vec;
  hash32f *hash_functions;
  usize num_functions;
  usize num_items;
};

struct BloomFilter *bloom_filter_new(usize size, usize num_functions, ...) {
  va_list argp;

  struct BloomFilter *filter = malloc(sizeof(*filter));
}
