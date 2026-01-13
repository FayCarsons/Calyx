#ifndef UTIL_H
#define UTIL_H

#include <stddef.h>
#include <stdint.h>

typedef uint64_t u64;
typedef uint32_t u32;
typedef uint8_t u8;
typedef size_t usize;

// Allocator interface
struct Allocator {
  void *(*alloc)(usize);
  void *(*calloc)(usize, usize);
  void (*dealloc)(void *);
  void *(*realloc)(void *, usize);
};

#define STDLIB_ALLOCATOR                                                       \
  ((struct Allocator){                                                         \
      .alloc = malloc, .calloc = calloc, .dealloc = free, .realloc = realloc})

// OCaml runtime allocator (use in C stubs)
// Requires: #include <caml/memory.h>
#define OCAML_ALLOCATOR                                                        \
  ((struct Allocator){.alloc = caml_stat_alloc,                                \
                      .calloc = caml_stat_calloc,                              \
                      .dealloc = caml_stat_free,                               \
                      .realloc = caml_stat_resize})
#endif
