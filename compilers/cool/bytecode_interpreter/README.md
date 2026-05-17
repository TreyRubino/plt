# COOL Bytecode Compiler & Virtual Machine  
Trey Rubino -  
CPSC 372 Independent Study -  
Dr. Schwesinger  

## Project Structure  
The repository is divided into a frontend and backend architecture, with core
definitions shared by all phases and a main driver that orchestrates the full
pipeline.

### Frontend  
- `lexer/`: Produces tokens from COOL source text, including identifiers,
  literals, keywords, operators, and error recovery for malformed input.  
- `parser/`: Implements the COOL grammar, constructs AST nodes with precise
  source locations, and reports syntax-level issues.  
- `checker/`: Performs semantic analysis including inheritance validation,
  environment construction, static type checking, and annotation of AST nodes
  with static type information.  

### Backend  
- `codegen/`: Lowers the typed COOL program into IR form, constructs class and
  method layouts, synthesizes flat constructors, resolves dispatch, and emits
  the final bytecode instruction stream.  
- `vm/`: Executes the generated IR using a stack-based virtual machine over a
  self-managed raw heap. Supports object allocation on a `Bigarray` word slab,
  dynamic and static dispatch, unboxed integer and boolean values, a parallel
  string table, and a mark-and-sweep garbage collector — all operating
  independently of OCaml's runtime.  

### Shared Modules  
- `core/`: The compiler-wide foundation, defining the COOL AST, shared static
  types, bytecode ISA, intermediate representation structures, error-handling
  framework, and semantic environment types.  

### Additional  
- `tests/`: Validation programs, IR dumps, and execution tests used during
  development, including a dedicated GC stress test.  
- `main.ml`: Entry point that wires together the frontend and backend, invokes
  each compilation stage, and runs the generated IR through the VM (in core/).  

## Overview  
This repository implements the full COOL toolchain — from raw source text to
executable bytecode running on a self-managed runtime. The pipeline follows a
traditional compiler structure: lexing and parsing produce the AST, semantic
analysis enforces correctness, code generation constructs a machine-executable
IR, and the VM interprets that IR deterministically. The VM maintains its own
heap as a raw word slab (`Bigarray.Array1` of `nativeint`) that is outside
OCaml's garbage collector; COOL object lifetimes are governed entirely by a
mark-and-sweep collector built into the VM. Integers and booleans are unboxed
values that never touch the heap. Each subsystem documents its own behavior
through a directory-level README; this file serves as a top-level guide.

## Design  
The system adopts a clean, modular architecture. The frontend enforces all
static requirements, guaranteeing that only well-formed and well-typed programs
reach the backend. The backend assumes semantic correctness and produces a
compact IR along with deterministic bytecode for runtime execution. The VM
runtime separates concerns cleanly: the call stack and frame locals remain in
OCaml constructs (mirroring the role of the hardware stack), while all
heap-allocated COOL objects reside in a raw word slab managed entirely by the
VM. The mark-and-sweep collector traces from operand stack, frame locals, and
frame self-pointers as roots and fires on a configurable allocation-count
interval. Shared core modules centralize definitions for AST nodes, IR layout,
bytecode instructions, and the exception system to ensure consistency across
all stages.

## Implementation  
Compilation proceeds through strictly ordered phases: lexical analysis, parsing,
semantic validation, IR generation, and bytecode lowering. The VM loads the IR,
allocates the Main object on the raw slab, pushes the entry method frame, and
executes instructions until the entry method completes. Each COOL object is
laid out in the slab as a tagged-word sequence: a header word encoding the
class ID and GC mark bit, a size word, and one tagged word per field. Field
words discriminate integers, booleans, heap pointers, string table indices, and
void using a three-bit tag in the low bits. The mark phase uses an explicit
integer worklist over slab word offsets to avoid call-stack overflow on deep
object graphs. The sweep phase scans the slab linearly, coalescing adjacent
dead blocks to reduce fragmentation. String content is interned in a parallel
table managed by the same collector. Each module interacts only through the
shared structures defined under `core/`, ensuring a consistent and maintainable
implementation.

## Pipeline Usage  
The system is managed via a central Makefile that orchestrates the compilation
of the frontend, backend, and core modules. Executing `make` produces the
`cooli` (COOL Interpreter) executable in the project root. Running `make clean`
removes the build directory, the executable, and any generated debug or delta
reports.

The `cooli` executable supports two primary modes of operation. In standard
mode, compiling and executing a raw COOL source file is done via
`./cooli <file.cl>`. The interpreter will lex, parse, and type-check the
source before lowering it to bytecode. In bootstrap mode, bypassing the
frontend to run the system using a pre-generated semantic analysis file is
achieved using the `-b` flag via `./cooli -b <file.cl-type>`.

For debugging and IR inspection, passing the `-d` flag via
`./cooli -d <file.cl>` generates a human-readable dump of the constant table,
class layouts, and disassembled bytecode, producing a `debug.txt` file of the
precise IR state used by the VM.

The project includes automated scripts for validation. Running
`make regression` executes the full suite of validation programs, while
`make delta` performs a differential analysis against reference outputs. A
convenience rule, `make run`, rebuilds the system and executes the standard
test case. 

Note: Grammarly was used in conjunction to write this document.