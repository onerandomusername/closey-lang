#ifndef SYSCALLS_H
#define SYSCALLS_H

#include <stddef.h>

void* mmap(void* start, size_t length, int prot, int flags, int fd, size_t offset);

int munmap(void* start, size_t length);

void exit(int ecode);

#endif /* SYSCALLS_H */
