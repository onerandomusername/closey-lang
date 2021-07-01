#ifndef M1_JIT_H
#define M1_JIT_H

void pthread_jit_write_protect_np(bool);

#ifndef __APPLE__
extern int MAP_JIT;
#endif /* __APPLE__ */

#endif /* M1_JIT_H */
