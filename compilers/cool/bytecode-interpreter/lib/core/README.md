# Core Definitions and Shared Types  
Trey Rubino -  
CPSC 372 Independent Study -  
Dr. Schwesinger  

## Project Structure  
- `ast.ml`: Defines the COOL abstract syntax tree including all expression
  forms, identifiers, class declarations, features, static types, and location
  metadata. Shared across the entire compiler pipeline.  
- `bytecode.ml`: Declares the full instruction set architecture (ISA) for the
  COOL VM, including opcodes, operands, instruction records, and program
  representations. Used by the code generator and VM.  
- `error.ml`: Provides the unified compiler and VM error-reporting system. Each
  compiler stage raises phase-specific exceptions with precise source locations
  and formatted diagnostic messages.  
- `ir.ml`: Defines the intermediate representation consumed by the VM,
  including literal constants, class layouts, dispatch tables, method
  descriptors, and the entry method index.  
- `semantics.ml`: Data structures for the semantic analysis environment
  including class maps, attribute and method implementations, parent maps,
  method bodies, and representation of type-annotated AST nodes.  

## Overview  
The core module set defines the foundational types and data structures used
uniformly across parsing, semantic analysis, code generation, and virtual
machine execution. These files form the compiler’s shared vocabulary: the COOL
AST, static typing annotations, the bytecode instruction set, the IR layout
used by the VM, and the environments that store class metadata and method
definitions. By centralizing these definitions, each compiler stage can operate
independently while maintaining consistent views of the COOL program’s
structure, types, and runtime representation.

## Design  
The AST captures COOL programs with explicit tagging of expression variants,
recursive structure, and source-location fields. Each node carries an optional
static type field populated during semantic analysis. The bytecode module
defines a compact, fixed instruction set paired with operand types, supporting
the stack-based execution model of the VM. The error module unifies the raising
and reporting of exceptions across all phases—parsing, type checking, code
generation, and runtime—ensuring consistent diagnostic formatting. The IR
module models the compiled program as literal pools, attribute layouts, method
tables, and dispatch vectors, reflecting all decisions made during code
generation. Finally, the semantic environment organizes class attributes,
methods, and inheritance relationships used during type checking and lowering.

## Implementation  
AST construction occurs in the parser and is reused through every stage of the
pipeline. Static types are assigned during semantic analysis using the optional
field within each expression node. The bytecode instruction set is encoded as
variant types with operand constructors, allowing the code generator to emit
compact program arrays. The error system is implemented using a single exception
type parameterized by the compiler phase and source line, enabling precise error
tracking. The IR structures mirror the final layout expected by the VM,
including offsets for attributes, dispatch-table slots, and method-local frame
sizes. The semantic environment uses hash tables for fast class and method
lookup and stores both user-defined and internal method bodies for later
lowering.

Note: Grammarly was used in conjunction to write this document.