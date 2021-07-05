#include "rc.h"
#include "unknown_arity.h"

// Applies a function with the given closed values and passed in arguments.
void* apply_func(void* func, unsigned int* called_argc, unsigned int saved, void* closed[], unsigned int argc, void* args[]) {
    asm(".intel_syntax noprefix");
    asm("push rdi");
    asm("push rsi");
    asm("push rdx");
    asm("push rcx");
    asm("push r8");
    asm("push r9");

    {
        void* passed_args[argc];
        for (unsigned int i = 0; i < saved; i++) {
            passed_args[i] = closed[i];
        }
        for (unsigned int i = saved; i < argc; i++) {
            passed_args[i] = args[i - saved];
        }

        // rdi, rsi, rdx, rcx, r8, r9
        func = (void*) (((unsigned long long) func) + 0xf);
        asm("mov rax, %0" : "=r" (func));
        if (argc >= 1) {
            asm("pop rdi");

            if (argc >= 2) {
                asm("pop rsi");

                if (argc >= 3) {
                    asm("pop rdx");

                    if (argc >= 4) {
                        asm("pop rcx");

                        if (argc >= 5) {
                            asm("pop r8");

                            if (argc >= 6) {
                                asm("pop r9");
                            }
                        }
                    }
                }
            }
        }

        asm("call rax");

        if (argc >= 1) {
            asm("push rdi");

            if (argc >= 2) {
                asm("push rsi");

                if (argc >= 3) {
                    asm("push rdx");

                    if (argc >= 4) {
                        asm("push rcx");

                        if (argc >= 5) {
                            asm("push r8");

                            if (argc >= 6) {
                                asm("push r9");
                            }
                        }
                    }
                }
            }
        }
    }

    asm("pop r9");
    asm("pop r8");
    asm("pop rcx");
    asm("pop rdx");
    asm("pop rsi");
    asm("pop rdi");
    asm("mov %0, rax" : "=r" (func));
    *called_argc -= argc - saved;
    return func;
}

// Calls a function with unknown arity.
void* call_unknown_arity(void* func, unsigned int called_argc, void* args[]) {
    while (called_argc > 0) {
        if (((unsigned long long) func) & 1) {
            unsigned int argc = *((unsigned int*) func);

            if (argc <= called_argc) {
                func = apply_func(func, &called_argc, 0, NULL, argc, args);
            } else {
                void* array[called_argc + 1];
                array[0] = func;
                for (unsigned int i = 1; i < called_argc + 1; i++) {
                    array[i] = args[i];
                }
                return rccopy(array, (called_argc + 1) * 8, (argc + 1) * 8);
            }
        } else {
            void** closure = (void**) func;
            func = *closure;
            unsigned int argc = *((unsigned int*) func);
            unsigned int saved = 0;
            for (unsigned int i = 1; i < argc + 1; i++) {
                if (closure[i] == NULL) {
                    saved = i - 1;
                    break;
                }
            }

            if (argc <= called_argc + saved) {
                func = apply_func(func, &called_argc, saved, closure + 1, argc, args);
            } else {
                if (!has_one_reference(closure)) {
                    closure = rccopy(closure, (saved + 1) * 8, (argc + 1) * 8);
                }

                for (unsigned int i = saved + 1; i < saved + called_argc + 1; i++) {
                    closure[i] = args[i - saved - 1];
                    rcinc(closure[i]);
                }

                return closure;
            }
        }
    }

    return func;
}

