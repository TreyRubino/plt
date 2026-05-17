# Crumb: Custom C Memory Allocator
### Author: Trey Rubino
Crumb is a minimal memory allocator written in C. It uses a linked list, supports first-fit allocation, and operates without malloc, using mmap to request memory from the OS