#include <pthread.h>
#include <sys/mman.h>
#include <stdbool.h>
#include <string.h>

#include "m1_jit.h"

#if !defined(__APPLE__) && !defined(__aarch64__)
void pthread_jit_write_protect_np(int _) { if (_) {  } }
#else
void ________is_not_used_but_is_here_to_suppress_warnings________() { }
#endif /* __APPLE__ */

