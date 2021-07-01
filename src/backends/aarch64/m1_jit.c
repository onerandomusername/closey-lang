#include <pthread.h>
#include <sys/mman.h>
#include <stdbool.h>
#include <string.h>

#include "m1_jit.h"

#ifndef __APPLE__
#ifndef __aarch64__
void pthread_jit_write_protect_np(int _) { if (_) {  } }
#else
void ________is_not_used_but_is_here_to_suppress_warnings________() {
}
#endif /* __aarch64__ */
void ________is_not_used_but_is_here_to_suppress_warnings_2______() {
}
#endif /* __APPLE__ */
