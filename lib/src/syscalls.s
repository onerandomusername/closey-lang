.intel_syntax noprefix
.global mmap
.global munmap
.global exit

# args passed into registers:
# rdi, rsi, rdx, rcx, r8, r9

# void* mmap(void* start, size_t length, int prot, int flags, int fd, size_t offset);
mmap:
    mov r10, rcx
    mov rax, 9
    syscall
    ret

# int munmap(void* start, size_t length);
munmap:
    mov rax, 11
    syscall
    ret

# void exit(int ecode);
exit:
    mov rax, 60
    syscall
    ret

