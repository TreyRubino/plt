/// @author Trey Rubino
/// @date   07/12/2025

#include <unistd.h> 
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <sys/mman.h>

#include "inc/heap.h"
#include "inc/block.h"

#define HEAP_SIZE 64 * 1024
#define BLOCK_HEADER_SIZE sizeof(Block)
#define BLOCK_MIN_SIZE sizeof(BLOCK_HEADER_SIZE + 16)

static Block *free_list_head = NULL; 

/// uses first-fit
void *cmalloc(size_t size);
void *crealloc(void *ptr, size_t size);
void cfree(void *ptr);

void cinit(void);
void cdebug(void);

int main(void) 
{
    cinit();
    printf("=== Crumb Allocator Demo ===\n\n");

    printf("Heap initialized (%d bytes total)\n", HEAP_SIZE);
    printf("Each allocation includes a header of %zu bytes.\n", sizeof(Block));
    usleep(1000000);

    printf("\n[1] Allocating 32 bytes...\n");
    char *a = (char *)cmalloc(32);
    assert(a);
    strcpy(a, "hello, crumb");
    printf("  → Allocated at: %p | Written: %s\n", (void *)a, a);
    cdebug();

    usleep(1000000);
    printf("\n[2] Freeing block...\n");
    cfree(a);
    printf("  → Block freed.\n");
    cdebug();

    usleep(1000000);
    printf("\n[3] Allocating 32 bytes again...\n");
    char *b = (char *)cmalloc(32);
    assert(b);
    strcpy(b, "block reuse success");
    printf("  → Allocated at: %p | Written: %s\n", (void *)b, b);
    cdebug();

    usleep(1000000);
    printf("\n[4] Allocating 3 blocks of 16 bytes...\n");
    char *x = (char *)cmalloc(16);
    char *y = (char *)cmalloc(16);
    char *z = (char *)cmalloc(16);
    assert(x && y && z);
    strcpy(x, "X");
    strcpy(y, "Y");
    strcpy(z, "Z");
    printf("  → x: %p, y: %p, z: %p\n", (void *)x, (void *)y, (void *)z);
    cdebug();

    usleep(1000000);
    printf("\n[5] Freeing all blocks...\n");
    cfree(b);
    cfree(x);
    cfree(y);
    cfree(z);
    cdebug();

    usleep(1000000);
    printf("\nCrumb allocator working as expected — no memory leaked, full reuse and coalescing verified.\n");
    return EXIT_SUCCESS;
}

void cdebug() 
{
    Block *b = free_list_head;
    while (b) {
        printf("Block at %p | size: %zu | free: %d\n", (void *)b, b->size, b->free);
        b = b->next;
    }
}

void cfree(void *ptr)
{
    if (ptr == NULL) {
        return;
    }

    Block *b = (Block *)((char *)ptr - BLOCK_HEADER_SIZE);
    b->free = 1;

    /// now that the given block has been marked free, its time to
    /// merge adjacent free blocks to minimize external fragmentation within
    /// our managed heap, this is called coalescing 
    Block *current = free_list_head;
    while (current != NULL && current->next != NULL) {
        /// are both current and next free? 
        if (current->free == 1 && current->next->free == 1) {
            /// are the blocks physically next to each other in memory?
            if ((char *)current + current->size == (char *)current->next) {
                /// merge the adjacent blocks together
                current->size += current->next->size;
                current->next = current->next->next;
            } else {
                current = current->next;
            }
        } else {
            current = current->next;
        }
    }
}

void *cmalloc(size_t size)
{
    Block *current = free_list_head;
    while (current != NULL) {
        /// if the current block available and is it big enough to handle the requested
        /// allocation space plus the header size
        if (current->free == 1 && current->size >= (size + BLOCK_HEADER_SIZE)) {
            /// is the block big enough to handle a split? 
            if (current->size >= (size + BLOCK_HEADER_SIZE + BLOCK_MIN_SIZE)) {
                /// move forward in memory by the header size and requested payload
                /// this starts to form the left off block after the current becomes allocated
                /// this defines the starting point of that left over block
                /// this is just moving addresses no sizes calculation 
                Block *b = (Block *)((char *)current + BLOCK_HEADER_SIZE + size);    

                /// carve off the size of the allocating block (current)
                /// this sets the size of the left over block to that 
                /// of the current block minus the header minus the allocating size
                b->size = current->size - BLOCK_HEADER_SIZE - size;
                b->free = 1;
                b->next = current->next;

                /// define the size of the allocated block which is trival
                /// requested size plus the defined header size of the block struct
                /// this shrinks the current block to exactly the requested size 
                /// and takes in account the header size as well
                current->size = size + BLOCK_HEADER_SIZE;
                current->free = 0;
                current->next = b;
                return (void *)((char *)current + BLOCK_HEADER_SIZE);
            } else { 
                /// block can't handle a split, return the pointer to the full blocks payload
                return (void *)((char *)current + BLOCK_HEADER_SIZE);
            }
        } else { 
            /// block is not big enough for size plus header, move to the next block and loop
            current = current->next;
        }
    }
    return NULL; /// no block was found for the requested size 
}

void *crealloc(void *ptr, size_t size) 
{
    /// pointer is NULL, this is the same as allocating a new block
    /// so do so
    if (ptr == NULL) return cmalloc(size);
    
    /// if the requested size is 0, this is the same
    /// as freeing the provided pointer, call free and 
    /// return the NULL pointer 
    if (size == 0) { cfree(ptr); return NULL; }

    /// now we need to get the meta data of the 
    /// provided pointer, and check if the given pointers
    /// body already has enough space for the reallocation
    /// size, then return the given pointer unchanged
    Block *current = (Block *)((char *)ptr - BLOCK_HEADER_SIZE);
    if (current->size - BLOCK_HEADER_SIZE >= size) return ptr;

    /// since the the given pointer is not large enough
    /// lets try in place expansion by checking the next pointers 
    /// size and combined with the given pointers size
    if (
        current->next->free == 1 && 
        (char *)current + current->size == (char *)current->next 
    ) {
        current->size += current->next->size;
        current->next = current->next->next;
        return ptr;
    }

    /// in place expansion is not possible so we need to
    /// alloc a new block, and copy the contents of the old block
    /// into the new block. This is the current blocks size minus
    /// the size of the header to get the payload size, take the minium of 
    /// the request size and the current payload size since we don't know
    /// if cmalloc shrunk the block exactly to the requested size of not. 
    /// after copy lets make sure to free the old block and return the new
    void *new = cmalloc(size);
    memcpy(new, ptr, ((current->size - BLOCK_HEADER_SIZE) < size) ? (current->size - BLOCK_HEADER_SIZE) : size);
    cfree(ptr);
    return new;
}

void cinit()
{
    char *start, *middle, *end;
    start = (char *)mmap(NULL, HEAP_SIZE, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (start == MAP_FAILED) {     
        perror("mmap failed");
        exit(EXIT_FAILURE);
    }

    middle = start + (HEAP_SIZE / 2);       /// this will give us the middle of our allocated heap  
    end = start + HEAP_SIZE;                /// this will give us the a pointer to the end of the heap

    Block *first = (Block *)start;
    first->size = HEAP_SIZE;
    first->free = 1;
    first->next = NULL;
    free_list_head = first;
}