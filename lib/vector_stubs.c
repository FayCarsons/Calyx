#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <string.h>

#include "vector.h"

static void *caml_stat_calloc(usize n, usize size) {
  usize total = n * size;
  void *p = caml_stat_alloc(total);
  if (p) {
    memset(p, 0, total);
  }
  return p;
}

#define Vector_val(v) (*((struct Vector **)Data_custom_val(v)))

static void vectorFinalize(value v) {
  struct Vector *vec = Vector_val(v);
  if (vec) {
    Vector_free(vec, OCAML_ALLOCATOR);
  }
}

static struct custom_operations vectorOps = {
    .identifier = "calyx.vector",
    .finalize = vectorFinalize,
};

CAMLprim value caml_vector_create(value vCapacity) {
  CAMLparam1(vCapacity);
  CAMLlocal1(v);

  usize capacity = Long_val(vCapacity);

  struct Vector *vec = Vector_new(capacity, OCAML_ALLOCATOR);
  if (!vec) {
    caml_failwith("Vector.create: allocation failed");
  }

  v = caml_alloc_custom(&vectorOps, sizeof(struct Vector *), 0, 1);
  Vector_val(v) = vec;

  CAMLreturn(v);
}

CAMLprim value caml_vector_release_roots(value vVec) {
  Vector_releaseRoots(Vector_val(vVec));
  return Val_unit;
}

CAMLprim value caml_vector_push(value vVec, value vElt) {
  CAMLparam2(vVec, vElt);
  Vector_push(Vector_val(vVec), vElt, OCAML_ALLOCATOR);
  CAMLreturn(Val_unit);
}

// Helper to construct Some(v)
static inline value mkSome(value v) {
  CAMLparam1(v);
  CAMLlocal1(some);
  some = caml_alloc(1, 0);
  Store_field(some, 0, v);
  CAMLreturn(some);
}

CAMLprim value caml_vector_pop(value vVec) {
  CAMLparam1(vVec);
  CAMLlocal1(result);

  struct Vector *vec = Vector_val(vVec);
  if (vec->size == 0) {
    CAMLreturn(Val_none);
  }

  result = Vector_pop(vec);
  CAMLreturn(mkSome(result));
}

CAMLprim value caml_vector_get(value vVec, value vIdx) {
  CAMLparam2(vVec, vIdx);
  struct Vector *vec = Vector_val(vVec);
  usize idx = Long_val(vIdx);

  if (idx >= vec->size) {
    CAMLreturn(Val_none);
  }

  CAMLreturn(mkSome(Vector_get(vec, idx)));
}

CAMLprim value caml_vector_set(value vVec, value vIdx, value vElt) {
  CAMLparam3(vVec, vIdx, vElt);
  struct Vector *vec = Vector_val(vVec);
  usize idx = Long_val(vIdx);

  if (idx >= vec->size) {
    CAMLreturn(Val_false);
  }

  Vector_set(vec, idx, vElt);
  CAMLreturn(Val_true);
}

CAMLprim value caml_vector_length(value vVec) {
  return Val_long(Vector_val(vVec)->size);
}

CAMLprim value caml_vector_capacity(value vVec) {
  return Val_long(Vector_val(vVec)->capacity);
}

CAMLprim value caml_vector_unsafe_get(value vVec, value vIdx) {
  struct Vector *vec = Vector_val(vVec);
  usize idx = Long_val(vIdx);
  return vec->data[idx];
}

CAMLprim value caml_vector_unsafe_set(value vVec, value vIdx, value vElt) {
  struct Vector *vec = Vector_val(vVec);
  usize idx = Long_val(vIdx);
  Vector_set(vec, idx, vElt);
  return Val_unit;
}

CAMLprim value caml_vector_to_array(value vVec) {
  CAMLparam1(vVec);
  CAMLlocal1(arr);

  struct Vector *vec = Vector_val(vVec);
  usize len = vec->size;

  if (len == 0) {
    CAMLreturn(Atom(0));
  }

  arr = caml_alloc(len, 0);
  for (usize i = 0; i < len; i++) {
    Store_field(arr, i, vec->data[i]);
  }

  CAMLreturn(arr);
}

CAMLprim value caml_vector_of_array(value vArr) {
  CAMLparam1(vArr);
  CAMLlocal1(vVec);

  usize len = Wosize_val(vArr);

  struct Vector *vec = Vector_new(len > 0 ? len : 8, OCAML_ALLOCATOR);
  if (!vec) {
    caml_failwith("Vector.of_array: allocation failed");
  }

  for (usize i = 0; i < len; i++) {
    Vector_push(vec, Field(vArr, i), OCAML_ALLOCATOR);
  }

  vVec = caml_alloc_custom(&vectorOps, sizeof(struct Vector *), 0, 1);
  Vector_val(vVec) = vec;

  CAMLreturn(vVec);
}

CAMLprim value caml_vector_clear(value vVec) {
  struct Vector *vec = Vector_val(vVec);
  for (usize i = 0; i < vec->size; i++) {
    caml_remove_generational_global_root(&vec->data[i]);
  }
  vec->size = 0;
  return Val_unit;
}

CAMLprim value caml_vector_shrink_to_fit(value vVec) {
  CAMLparam1(vVec);
  struct Vector *vec = Vector_val(vVec);

  if (vec->size == vec->capacity || vec->size == 0) {
    CAMLreturn(Val_unit);
  }

  for (usize i = 0; i < vec->size; i++) {
    caml_remove_generational_global_root(&vec->data[i]);
  }

  usize newCap = vec->size;
  value *newData = caml_stat_resize(vec->data, newCap * sizeof(value));
  if (newData) {
    vec->data = newData;
    vec->capacity = newCap;
  }

  for (usize i = 0; i < vec->size; i++) {
    caml_register_generational_global_root(&vec->data[i]);
  }

  CAMLreturn(Val_unit);
}

CAMLprim value caml_vector_copy(value vVec) {
  CAMLparam1(vVec);
  CAMLlocal1(vResult);

  struct Vector *src = Vector_val(vVec);
  usize len = src->size;
  usize cap = len > 0 ? len : 8;

  struct Vector *dst = Vector_new(cap, OCAML_ALLOCATOR);
  if (!dst) {
    caml_failwith("Vector.copy: allocation failed");
  }

  for (usize i = 0; i < len; i++) {
    Vector_push(dst, src->data[i], OCAML_ALLOCATOR);
  }

  vResult = caml_alloc_custom(&vectorOps, sizeof(struct Vector *), 0, 1);
  Vector_val(vResult) = dst;

  CAMLreturn(vResult);
}

CAMLprim value caml_vector_append(value vDst, value vSrc) {
  CAMLparam2(vDst, vSrc);

  struct Vector *dst = Vector_val(vDst);
  struct Vector *src = Vector_val(vSrc);

  for (usize i = 0; i < src->size; i++) {
    Vector_push(dst, src->data[i], OCAML_ALLOCATOR);
  }

  CAMLreturn(Val_unit);
}

CAMLprim value caml_vector_concat(value vA, value vB) {
  CAMLparam2(vA, vB);
  CAMLlocal1(vResult);

  struct Vector *a = Vector_val(vA);
  struct Vector *b = Vector_val(vB);
  usize total = a->size + b->size;
  usize cap = total > 0 ? total : 8;

  struct Vector *result = Vector_new(cap, OCAML_ALLOCATOR);
  if (!result) {
    caml_failwith("Vector.concat: allocation failed");
  }

  for (usize i = 0; i < a->size; i++) {
    Vector_push(result, a->data[i], OCAML_ALLOCATOR);
  }
  for (usize i = 0; i < b->size; i++) {
    Vector_push(result, b->data[i], OCAML_ALLOCATOR);
  }

  vResult = caml_alloc_custom(&vectorOps, sizeof(struct Vector *), 0, 1);
  Vector_val(vResult) = result;

  CAMLreturn(vResult);
}

CAMLprim value caml_vector_rev(value vVec) {
  CAMLparam1(vVec);
  CAMLlocal1(vResult);

  struct Vector *src = Vector_val(vVec);
  usize len = src->size;
  usize cap = len > 0 ? len : 8;

  struct Vector *dst = Vector_new(cap, OCAML_ALLOCATOR);
  if (!dst) {
    caml_failwith("Vector.rev: allocation failed");
  }

  for (usize i = len; i > 0; i--) {
    Vector_push(dst, src->data[i - 1], OCAML_ALLOCATOR);
  }

  vResult = caml_alloc_custom(&vectorOps, sizeof(struct Vector *), 0, 1);
  Vector_val(vResult) = dst;

  CAMLreturn(vResult);
}

CAMLprim value caml_vector_rev_in_place(value vVec) {
  struct Vector *vec = Vector_val(vVec);
  usize len = vec->size;

  for (usize i = 0; i < len / 2; i++) {
    usize j = len - 1 - i;
    value tmp = vec->data[i];
    vec->data[i] = vec->data[j];
    vec->data[j] = tmp;
  }

  return Val_unit;
}

CAMLprim value caml_vector_blit(value vSrc, value vSrcOff, value vDst,
                                value vDstOff, value vLen) {
  CAMLparam5(vSrc, vSrcOff, vDst, vDstOff, vLen);

  struct Vector *src = Vector_val(vSrc);
  struct Vector *dst = Vector_val(vDst);
  usize srcOff = Long_val(vSrcOff);
  usize dstOff = Long_val(vDstOff);
  usize len = Long_val(vLen);

  if (srcOff + len > src->size || dstOff + len > dst->size) {
    caml_invalid_argument("Vector.blit: out of bounds");
  }

  for (usize i = 0; i < len; i++) {
    caml_modify_generational_global_root(&dst->data[dstOff + i],
                                         src->data[srcOff + i]);
  }

  CAMLreturn(Val_unit);
}
