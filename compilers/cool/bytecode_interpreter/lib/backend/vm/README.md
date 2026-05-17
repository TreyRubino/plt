# The Virtual Machine  
Trey Rubino -  
CPSC 372 Independent Study -  
Dr. Schwesinger  

## Project Structure  
- `runtime.ml`: Defines the core runtime types used by the VM: the unboxed
  value type, the raw word-slab managed heap, the parallel string table, the
  call frame, and the virtual machine state.  
- `heap.ml`: Raw word-slab allocator for COOL runtime objects. Manages a
  `Bigarray.Array1` of `nativeint` words that lives outside OCaml's GC.
  Provides field encode/decode between the OCaml value type and the slab's
  tagged-word representation, and exposes the header manipulation primitives
  used by the collector.  
- `strings.ml`: Parallel string table for COOL runtime strings. String bytes
  live in an OCaml array managed here; the slab carries only an integer slot
  index per String object. `intern` deduplicates content via a hash table. The
  collector marks live slots; sweep reclaims unreachable ones.  
- `gc.ml`: Mark-and-sweep garbage collector for the raw word slab and the
  parallel string table. The mark phase uses an explicit worklist to avoid
  OCaml call-stack overflow on deep object graphs. The sweep phase scans the
  slab linearly, coalescing adjacent dead blocks into the free list.  
- `alloc.ml`: Provides allocation routines for COOL objects. All allocations
  flow through this module into the raw word slab. A collection is triggered
  proactively when the allocation counter reaches the configured interval.
  Integer and boolean results are unboxed values and never allocated here.  
- `stack.ml`: Implements the VM call stack and value stack, supporting frame
  creation, argument installation, local access, and stack-value operations.
  `self_ptr` is a raw slab word offset replacing the old direct OCaml object
  reference.  
- `builtin.ml`: Handles all built-in COOL methods such as `abort`, `copy`,
  `type_name`, I/O routines, and string operations. Dispatches these before
  normal bytecode execution. Integer and boolean return values are unboxed.
  String content is read from the parallel string table via the `StrIdx` stored
  in the String object's slab field.  
- `exec.ml`: Core execution engine that interprets bytecode instructions over
  the raw word slab. Integers and booleans are unboxed; no allocation occurs
  for arithmetic, comparison, or boolean operations. Field access, dispatch,
  and new-object instructions go through the slab allocator and heap accessors.  
- `vm.ml`: Entry point for constructing the VM, instantiating the main object,
  installing the entry method frame, and starting program execution. The Main
  object is allocated without pushing an init frame; `main()` invokes
  `__init_Main` itself via its lowered prologue.  

## Overview  
The Virtual Machine executes the intermediate representation produced by the
Code Generator, providing a concrete operational semantics for COOL programs.
Every class, attribute, method, bytecode instruction, and constructed object is
represented explicitly at runtime. The VM maintains a self-managed raw heap,
call frames, value stack, and object model. It interprets bytecode
step-by-step, performing dynamic dispatch, evaluating expressions, invoking
constructors, and carrying out all primitive and built-in COOL operations.
COOL object data lives entirely in a `Bigarray` word slab that is outside
OCaml's GC. Integers and booleans are unboxed and never touch the heap.

## Design

### Value Representation  
The operational value type is a lightweight OCaml variant carried on the call
stack, in frame locals, and on the operand stack:

- `VInt of int` — unboxed integer, never allocated on the heap  
- `VBool of bool` — unboxed boolean, never allocated on the heap  
- `VPtr of int` — word offset into the raw slab, references a heap object  
- `VVoid` — the null/void sentinel  

Only `VPtr` references slab memory. All arithmetic and boolean results remain
unboxed on the OCaml call stack, eliminating the allocation previously produced
by every boxing operation.

### Raw Heap Slab  
The heap is a `Bigarray.Array1` of `nativeint` words allocated with C layout.
OCaml's GC tracks only the container; the object data inside is entirely
unmanaged by OCaml. Each COOL object is encoded in the slab as a sequence of
tagged words:

```
object layout at word offset p:
  slab[p+0]           header  = (class_id lsl 2) lor mark_bit
  slab[p+1]           size    = total words including header (2 + n_fields)
  slab[p+2 .. p+1+n]  fields  encoded as tagged words

slab field word encoding:
  lsl : logic shift left

  0n              = VVoid
  (n lsl 3) | 1n  = VInt n
  (b lsl 3) | 2n  = VBool b
  (p lsl 3) | 3n  = VPtr p    (heap word offset)
  (i lsl 3) | 4n  = StrIdx i  (string table index, String objects only)
```

Allocation uses a bump pointer with a first-fit free list for reclaimed
blocks. The GC fires after 75% heap allocation; after each collection the
counter resets to zero so the trigger does not permanently refire after the
bump pointer's high-water mark first reaches capacity. The slab capacity
(1M words) is the hard out-of-memory boundary.

### Parallel String Table  
Strings are stored in a parallel `string array` since `Bigarray` cannot hold
variable-length OCaml strings directly. Each String slab object holds one
`StrIdx` field word carrying an integer index into this table. The table
deduplicates content via a hash map. The garbage collector marks live table
slots during the mark phase and the sweep reclaims unreachable ones. A slot is
only reclaimed when the table's own mapping confirms it as the canonical owner
of its content, preventing uninitialized slots from being incorrectly freed.

### Call Stack  
The operand stack, frame list, and local arrays remain in OCaml constructs
intentionally. These structures are ephemeral - they exist
only for the duration of a method call and unwind deterministically on return,
mirroring the role of the hardware stack in a native runtime. They carry no GC
pressure and require no collection. Each frame carries `self_ptr : int`, a word
offset into the slab, in place of the former direct OCaml object reference.

### Garbage Collector  
The mark-and-sweep collector operates entirely on slab word offsets and header
bits. The GC fires every 65 536 allocations and resets after each collection.
The mark phase uses an explicit integer worklist rather than OCaml recursion to
avoid call-stack overflow on deep object graphs. Roots are the operand stack,
all frame locals, all frame self pointers, and all constant strings from the IR
literal pool which are permanently live. The sweep phase scans the slab
linearly from offset zero to the bump pointer, coalescing adjacent dead blocks
into a single free node before returning them to the free list. The string
table is swept in the same collection pass.

### OCaml Boundary  
Two structures in this implementation have OCaml involvement, and both are
correct and standard.

The `Bigarray.Array1` slab uses `c_layout`, which means the backing memory is
allocated via `malloc` outside OCaml's heap entirely. OCaml's GC holds a thin
wrapper record with a pointer to that memory but does not scan the slab
contents for pointers. This is the fundamental design guarantee of Bigarray
with C layout — the GC sees a handle, not the data. All 1M words of COOL object
data are invisible to OCaml's GC.

The `string array` used by the parallel string table is a regular OCaml array.
OCaml's GC will keep any string in a live slot alive. However, the VM's
collector makes every lifecycle decision: when sweep determines a string slot is
dead it sets `data.(i) <- ""`, dropping the OCaml reference, after which
OCaml's GC may physically reclaim those string bytes at its next cycle. OCaml
never independently decides a live COOL string is dead. The only effect is a
brief delay between our sweep and OCaml's physical reclamation of the string
bytes — there is no correctness issue and no COOL object lifetime is influenced
by OCaml.

The call stack is in OCaml by design and is correct. Frame locals and the
operand stack are stack-disciplined and never need collected.

## Implementation  
Execution begins by constructing an initial VM state (allocating the slab and
string table), allocating an instance of the program's Main class on the slab
without pushing an init frame, then pushing the entry method (`main()`) frame
directly. The `main()` method's lowered prologue calls `__init_Main` itself,
ensuring the constructor fires exactly once. An earlier design pushed an init
frame from `vm.ml` via a function `allocate_and_init` and then pushed `main()` on top,
causing `__init_Main` to run twice. The fix was to use
`allocate_object` in `vm.ml` and let `main()` own the constructor call.

The interpreter loop fetches the next instruction, increments the program
counter, and performs the operation encoded by the opcode. Attribute access
reads or writes slab field words at the object's word offset plus the field
index, translating the bytecode's legacy `offset + 1` convention by
subtracting one at every `GET_ATTR` and `SET_ATTR` access site. Arithmetic
extracts `VInt` values directly with no unboxing step, computes the result as
an OCaml integer, and returns `VInt` with no allocation. Dispatch operations
read the receiver's class ID from the slab header word, index into the class
dispatch table to locate the correct method ID, and push a new frame with the
receiver's word offset as `self_ptr` and arguments installed into the local
array. Built-in routines are intercepted at `OP_RETURN` via
`Builtin.maybe_handle_builtin` before the general return path. The loop
continues until the last frame returns.

## Testing  
Testing for the VM was conducted through a combination of direct execution
against the COOL reference interpreter, targeted regression tests for
discovered bugs, and a dedicated GC stress test.

The `hs.cl` program exercises mutual class
initialization across four classes that inherit in a ring. Every class
allocates instances of related classes inside attribute initializers, requiring
the VM to handle deeply recursive construction without re-entrant looping. This
test exposed three bugs in sequence: the frame array out-of-bounds crash from
`n_locals = 0` in constructors, the infinite construction loop from parent
constructor chaining, and the `OP_IS_SUBTYPE` infinite loop from
`Object.parent_id = 0`. 

The GC stress test (`gc_test.cl`) validates the mark-and-sweep collector
directly. It allocates 100 000 `Box` objects in a loop, keeping only the most
recently allocated one live by reassigning a single `keeper` variable each
iteration. With `gc_interval = 65 536`, the collector fires once during the
loop, at which point approximately 65 535 dead objects must be swept and their
slab words returned to the free list while the single live `keeper` must
survive with its integer field intact. The expected output is `99999` followed
by a newline. If the mark phase incorrectly collects the live object the
program crashes or prints a wrong value; if the sweep phase fails to reclaim
dead objects the slab exhausts before the loop completes. The test passing
confirms both phases are correct.

## References  
[1] "The Cool Reference Manual," Alex Aiken (et al.), Stanford University, The COOL Language Project, Jan. 2011.  
[Online]. Available: https://theory.stanford.edu/~aiken/software/cool/cool-manual.pdf

[2] A. V. Aho, M. S. Lam, R. Sethi, and J. D. Ullman, Compilers: Principles, Techniques,
and Tools, 2nd ed., ch. 7, "Run-Time Environments," Pearson/Addison-Wesley, 2006.

[3] R. Jones, A. Hosking, and E. Moss, The Garbage Collection Handbook: The Art of 
Automatic Memory Management, 2nd ed., Chapman and Hall/CRC, Jul. 2023.

[4] "Slab allocation," Wikipedia, The Free Encyclopedia, Mar. 2026. [Online]. 
Available: https://en.wikipedia.org/wiki/Slab_allocation

[5] R. Nystrom, Crafting Interpreters, 1st ed. Genever Benning, 2021.

[6] "First Fit allocation in Operating Systems," GeeksforGeeks. [Online]. 
Available: https://www.geeksforgeeks.org/operating-systems/first-fit-allocation-in-operating-systems/

Note: Grammarly was used in conjunction to write this document.