CC=gcc
CFLAGS=-c -fPIC -nostdlib -Wall -g -masm=intel -fno-stack-protector
AR=ar
AFLAGS=rcs

all: *.o
	mkdir -p bin/
	$(AR) $(AFLAGS) bin/libclosey.a $?

*.o: src/*.c src/*.s
	$(CC) $(CFLAGS) $?

clean:
	rm -r bin *.o

