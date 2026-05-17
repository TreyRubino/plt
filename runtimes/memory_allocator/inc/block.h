#include <stdio.h>
#include <stdbool.h>

typedef struct Block {
    size_t size;
    bool free;
    struct Block *next;
} Block;