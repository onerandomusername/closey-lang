#include <pthread.h>
#include <sys/mman.h>
#include <stdbool.h>
#include <string.h>

#include "m1_jit.h"

#ifndef __APPLE__
void pthread_jit_write_protect_np(int) { }
#else
void ________is_not_used_but_is_here_to_supress_warnings________() {
}
#endif

