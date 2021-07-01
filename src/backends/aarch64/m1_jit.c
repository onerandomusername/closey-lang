#include <pthread.h>
#include <sys/mman.h>
#include <stdbool.h>
#include <string.h>

#include "m1_jit.h"

#ifndef __APPLE__
void pthread_jit_write_protect_np(bool) { }

int MAP_JIT = 0;
#endif

