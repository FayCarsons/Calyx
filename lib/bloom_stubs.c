#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include "bloom.h"

#define BloomFilter_val(v) (*((BloomFilter **)Data_custom_val(v)))

static void bloomFinalize(value v) {
  BloomFilter *bf = BloomFilter_val(v);
  if (bf) {
    BloomFilter_free(bf, STDLIB_ALLOCATOR);
  }
}

static struct custom_operations bloomOps = {
    .identifier = "fingerpaint.bloom_filter",
    .finalize = bloomFinalize,
    .compare = custom_compare_default,
    .hash = custom_hash_default,
    .serialize = custom_serialize_default,
    .deserialize = custom_deserialize_default,
};

CAMLprim value caml_bloom_create(value vSize, value vNumHashes) {
  CAMLparam2(vSize, vNumHashes);
  CAMLlocal1(v);

  usize size = Long_val(vSize);
  usize numHashes = Long_val(vNumHashes);

  BloomFilter *bf = BloomFilter_new(size, numHashes, STDLIB_ALLOCATOR);
  if (!bf) {
    caml_failwith("BloomFilter_new: allocation failed");
  }

  v = caml_alloc_custom(&bloomOps, sizeof(BloomFilter *), 0, 1);
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
