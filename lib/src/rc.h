#ifndef RC_H
#define RC_H

#include <stddef.h>
#include <stdbool.h>

// Allocates something on the heap with a reference count of 1
void* rcalloc(size_t size);

// Copies a pointer with a given size onto the heap with a reference count of 1.
void* rccopy(void* ptr, size_t len, size_t size);

// Increments the reference count.
void rcinc(void* ptr);

// Returns true if there is only one reference to the pointer.
bool has_one_reference(void* ptr);

// Decrement the reference count.
void rcfree(void* ptr);

#endif /* RC_H */
