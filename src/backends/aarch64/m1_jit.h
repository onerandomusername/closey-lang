#ifndef M1_JIT_H
#define M1_JIT_H

void pthread_jit_write_protect_np(int _);

#ifndef __APPLE__
#ifndef __aarch64__
extern int MAP_JIT;
#endif /* __aarch64__ */
#endif /* __APPLE__ */

#endif /* M1_JIT_H */
