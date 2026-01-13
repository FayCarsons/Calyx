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
