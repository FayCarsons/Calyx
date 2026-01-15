#include "bloom.h"
#include <string.h>

#define BITS_PER_U32 32
#define BITS_PER_U32_MASK (BITS_PER_U32 - 1)

usize nextPowerOfTwo(usize n) {
  if (n == 0)
    return 1;

  n--;

  n |= n >> 1;
  n |= n >> 2;
  n |= n >> 4;
  n |= n >> 8;
  n |= n >> 16;
  n |= n >> 32;
  return n + 1;
}

// wyhash - fast hash for short strings
// Public domain, by Wang Yi

#define WY0 0xa0761d6478bd642fULL
#define WY1 0xe7037ed1a0b428dbULL
#define WY2 0x8ebc6af09c88c6e3ULL

static inline u64 wyread64(const u8 *p) {
  u64 v;
  memcpy(&v, p, 8);
  return v;
}

static inline u64 wyread32(const u8 *p) {
  u32 v;
  memcpy(&v, p, 4);
  return v;
}

static inline u64 wymul(u64 a, u64 b) {
  __uint128_t r = (__uint128_t)a * b;
  return (u64)r ^ (u64)(r >> 64);
}

static inline u64 wyhash(const void *data, usize len) {
  const u8 *p = data;
  u64 seed = WY0, a, b;

  if (len <= 16) {
    if (len >= 4) {
      a = (wyread32(p) << 32) | wyread32(p + ((len >> 3) << 2));
      b = (wyread32(p + len - 4) << 32) |
          wyread32(p + len - 4 - ((len >> 3) << 2));
    } else if (len > 0) {
      a = ((u64)p[0] << 16) | ((u64)p[len >> 1] << 8) | p[len - 1];
      b = 0;
    } else {
      a = b = 0;
    }
  } else {
    usize i = len;
    if (i > 48) {
      u64 s1 = seed, s2 = seed;
      do {
        seed = wymul(wyread64(p) ^ WY0, wyread64(p + 8) ^ seed);
        s1 = wymul(wyread64(p + 16) ^ WY1, wyread64(p + 24) ^ s1);
        s2 = wymul(wyread64(p + 32) ^ WY2, wyread64(p + 40) ^ s2);
        p += 48;
        i -= 48;
      } while (i > 48);
      seed ^= s1 ^ s2;
    }
    while (i > 16) {
      seed = wymul(wyread64(p) ^ WY0, wyread64(p + 8) ^ seed);
      p += 16;
      i -= 16;
    }
    a = wyread64(p + i - 16);
    b = wyread64(p + i - 8);
  }

  return wymul(WY1 ^ len, wymul(a ^ WY0, b ^ seed));
}

static inline u64 wyhashInt(u64 x) { return wymul(x ^ WY0, WY1); }

// Split 64-bit hash into two 32-bit values for Kirsch-Mitzenmacher
static inline void hashSplit(u64 h, u32 *h1, u32 *h2) {
  *h1 = (u32)h;
  *h2 = (u32)(h >> 32);
}

// BitVec

BitVec *BitVec_new(usize size, struct Allocator a) {
  if (size == 0) {
    return NULL;
  }

  size = nextPowerOfTwo(size);

  BitVec *self = a.alloc(sizeof(*self));
  if (!self) {
    return NULL;
  }

  usize numWords = size / BITS_PER_U32;
  self->mem = a.calloc(numWords, sizeof(*self->mem));
  if (!self->mem) {
    a.dealloc(self);
    return NULL;
  }

  self->size = size;
  return self;
}

void BitVec_free(BitVec *self, struct Allocator a) {
  if (!self) {
    return;
  }
  a.dealloc(self->mem);
  a.dealloc(self);
}

bool BitVec_get(const BitVec *self, usize index) {
  usize word = index / BITS_PER_U32;
  usize bit = index & BITS_PER_U32_MASK;
  return (self->mem[word] >> bit) & 1;
}

void BitVec_set(BitVec *self, usize index) {
  usize word = index / BITS_PER_U32;
  usize bit = index & BITS_PER_U32_MASK;
  self->mem[word] |= (u32)1 << bit;
}

void BitVec_clear(BitVec *self, usize index) {
  usize word = index / BITS_PER_U32;
  usize bit = index & BITS_PER_U32_MASK;
  self->mem[word] &= ~((u32)1 << bit);
}

// BloomFilter

BloomFilter *BloomFilter_new(usize size, usize numHashes, struct Allocator a) {
  if (size == 0 || numHashes == 0) {
    return NULL;
  }

  BloomFilter *self = a.alloc(sizeof(*self));
  if (!self) {
    return NULL;
  }

  self->vec = BitVec_new(size, a);
  if (!self->vec) {
    a.dealloc(self);
    return NULL;
  }

  self->numHashes = numHashes;
  self->count = 0;
  return self;
}

void BloomFilter_free(BloomFilter *self, struct Allocator a) {
  if (!self) {
    return;
  }
  BitVec_free(self->vec, a);
  a.dealloc(self);
}

static inline void bloomInsert(BloomFilter *self, u32 h1, u32 h2) {
  usize mask = self->vec->size - 1;
  for (usize i = 0; i < self->numHashes; i++) {
    usize idx = (h1 + i * h2) & mask;
    BitVec_set(self->vec, idx);
  }
  self->count++;
}

static inline bool bloomQuery(const BloomFilter *self, u32 h1, u32 h2) {
  usize mask = self->vec->size - 1;
  for (usize i = 0; i < self->numHashes; i++) {
    usize idx = (h1 + i * h2) & mask;
    if (!BitVec_get(self->vec, idx)) {
      return false;
    }
  }
  return true;
}

// Generic data

void BloomFilter_put(BloomFilter *self, const void *data, usize len) {
  u32 h1, h2;
  hashSplit(wyhash(data, len), &h1, &h2);
  bloomInsert(self, h1, h2);
}

bool BloomFilter_test(const BloomFilter *self, const void *data, usize len) {
  u32 h1, h2;
  hashSplit(wyhash(data, len), &h1, &h2);
  return bloomQuery(self, h1, h2);
}

// Strings

void BloomFilter_putStr(BloomFilter *self, const char *str) {
  BloomFilter_put(self, str, strlen(str));
}

bool BloomFilter_testStr(const BloomFilter *self, const char *str) {
  return BloomFilter_test(self, str, strlen(str));
}

// Integers (fast path - no byte iteration)

void BloomFilter_putInt(BloomFilter *self, u64 key) {
  u32 h1, h2;
  hashSplit(wyhashInt(key), &h1, &h2);
  bloomInsert(self, h1, h2);
}

bool BloomFilter_testInt(const BloomFilter *self, u64 key) {
  u32 h1, h2;
  hashSplit(wyhashInt(key), &h1, &h2);
  return bloomQuery(self, h1, h2);
}
