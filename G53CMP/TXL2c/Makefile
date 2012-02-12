CC = gcc
CFLAGS = -Wall --save-temps

targets = test1 test2 test3
generated = test1.c test2.c test3.c

all: TXL2C ${targets} ${generated}

.PHONY:
clean:
	rm TXL2C ${targets} ${generated}

TXL2C: TXL2C.hs
	ghc -Wall --make TXL2C.hs

%.c: %.txl TXL2C
	./TXL2C $<
