#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <string.h>

#include "bloom.h"

// OCaml runtime doesn't have calloc, so we provide a wrapper
static void *caml_stat_calloc(usize n, usize size) {
  usize total = n * size;
  void *p = caml_stat_alloc(total);

  if (p) {
    memset(p, 0, total);
  }
  return p;
}

#define BloomFilter_val(v) (*((BloomFilter **)Data_custom_val(v)))

static void bloomFinalize(value v) {
  BloomFilter *bf = BloomFilter_val(v);
  if (bf) {
    BloomFilter_free(bf, OCAML_ALLOCATOR);
  }
}

static struct custom_operations bloomOps = {
    .identifier = "calyx.bloom_filter",
    .finalize = bloomFinalize,
};

CAMLprim value caml_bloom_create(value vSize, value vNumHashes) {
  CAMLparam2(vSize, vNumHashes);
  CAMLlocal1(v);

  usize size = Long_val(vSize);
  usize numHashes = Long_val(vNumHashes);

  BloomFilter *bf = BloomFilter_new(size, numHashes, OCAML_ALLOCATOR);
  if (!bf) {
    caml_failwith("BloomFilter_new: allocation failed");
  }

  // Calculate actual external memory usage for GC tracking
  usize actual_size = nextPowerOfTwo(size);
  usize mem_used =
      sizeof(BloomFilter) + sizeof(BitVec) + (actual_size / 32) * sizeof(u32);

  v = caml_alloc_custom_mem(&bloomOps, sizeof(BloomFilter *), mem_used);
  BloomFilter_val(v) = bf;

  CAMLreturn(v);
}

CAMLprim value caml_bloom_put(value vBf, value v) {
  BloomFilter *bf = BloomFilter_val(vBf);
  if (Is_long(v)) {
    BloomFilter_putInt(bf, Long_val(v));
  } else {
    BloomFilter_putStr(bf, String_val(v));
  }
  return Val_unit;
}

CAMLprim value caml_bloom_test(value vBf, value v) {
  BloomFilter *bf = BloomFilter_val(vBf);
  if (Is_long(v)) {
    return Val_bool(BloomFilter_testInt(bf, Long_val(v)));
  } else {
    return Val_bool(BloomFilter_testStr(bf, String_val(v)));
  }
}

CAMLprim value caml_bloom_count(value vBf) {
  return Val_long(BloomFilter_val(vBf)->count);
}

CAMLprim value caml_bloom_size(value vBf) {
  return Val_long(BloomFilter_val(vBf)->vec->size);
}
