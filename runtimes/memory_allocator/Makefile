
# Trey Rubino
# 07/13/2025

all: crumb

crumb: main.o
	clang -o crumb main.o

main.o: main.c
	clang -c main.c -o main.o

clean:
	rm -f crumb *.o

.PHONY: all clean