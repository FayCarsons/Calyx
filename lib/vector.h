#ifndef VECTOR_H
#define VECTOR_H

#include <caml/memory.h>
#include <caml/mlvalues.h>

#include "util.h"

struct Vector {
  value *data;
  usize size;
  usize capacity;
};

struct Vector *Vector_new(usize capacity, struct Allocator a);
void Vector_free(struct Vector *self, struct Allocator a);
void Vector_push(struct Vector *self, value v, struct Allocator a);
value Vector_pop(struct Vector *self);
value Vector_get(struct Vector *self, usize index);
void Vector_set(struct Vector *self, usize index, value v);
void Vector_releaseRoots(struct Vector *self);

#endif
