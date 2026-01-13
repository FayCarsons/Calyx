#include "vector.h"

#include <caml/memory.h>
#include <string.h>

static void *caml_stat_calloc(usize n, usize size) {
  usize total = n * size;
  void *p = caml_stat_alloc(total);
  if (p) {
    memset(p, 0, total);
  }
  return p;
}

struct Vector *Vector_new(usize capacity, struct Allocator a) {
  struct Vector *self = a.alloc(sizeof(*self));
  if (!self) {
    return NULL;
  }

  self->data = caml_stat_calloc(capacity, sizeof(value));
  if (!self->data) {
    a.dealloc(self);
    return NULL;
  }

  self->size = 0;
  self->capacity = capacity;

  return self;
}

void Vector_free(struct Vector *self, struct Allocator a) {
  if (!self) {
    return;
  }
  // Note: roots must be released before calling this (via Gc.finalise)
  a.dealloc(self->data);
  a.dealloc(self);
}

static void grow(struct Vector *self, struct Allocator a) {
  // Unregister all roots at old addresses
  for (usize i = 0; i < self->size; i++) {
    caml_remove_generational_global_root(&self->data[i]);
  }

  usize newCapacity = self->capacity * 2;
  value *newData = caml_stat_resize(self->data, newCapacity * sizeof(value));
  if (newData) {
    self->data = newData;
    self->capacity = newCapacity;
  }

  // Re-register all roots at new addresses
  for (usize i = 0; i < self->size; i++) {
    caml_register_generational_global_root(&self->data[i]);
  }
}

void Vector_push(struct Vector *self, value v, struct Allocator a) {
  if (self->size >= self->capacity) {
    grow(self, a);
  }

  usize idx = self->size++;
  self->data[idx] = v;
  caml_register_generational_global_root(&self->data[idx]);
}

value Vector_pop(struct Vector *self) {
  if (self->size == 0) {
    return Val_unit;
  }

  usize idx = --self->size;
  value v = self->data[idx];
  caml_remove_generational_global_root(&self->data[idx]);

  return v;
}

value Vector_get(struct Vector *self, usize index) {
  return self->data[index];
}

void Vector_set(struct Vector *self, usize index, value v) {
  caml_modify_generational_global_root(&self->data[index], v);
}

void Vector_releaseRoots(struct Vector *self) {
  if (!self || !self->data) {
    return;
  }
  for (usize i = 0; i < self->size; i++) {
    caml_remove_generational_global_root(&self->data[i]);
  }
}
