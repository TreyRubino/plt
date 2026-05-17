# The Operational Semantics 
Trey Rubino - 
CPSC 425 -
Dr. Schwesinger

## Project Structure
- `main.ml`: Entry point, loads runtime environment and executes Main.main
- `runtime.ml`: Defines runtime objects, values, scopes, and helpers
- `eval.ml`: Core evaluation engine implementing COOL's operational semantics
- `reader.ml`: Deserializers typed AST and runtime maps from `.cl-type` file
- `ast.ml`: Shared type definitions for COOL programs and expressions (same as the checkers)
- `Makefile`: Build instructions for compiling with the OCaml toolchain  
- `build/`: Directory containing generated lexer artifacts and compiled output  
- `test/`: Test files displaying good and bad test cases

## Overview
The operational semantics phase extends the interpreter from syntax and type analysis (static) into executable (dynamic)
behavior. It defines how COOL expression and statements evaluate at runtime, mapping abstract syntax to concrete effects and
values. This phase supports object creation, attribute initialization, method dispatch, expression evaluation, and control
structures. Runtime errors detected here include division by zero, unbound identifiers, invalid dispatches, and type
incompatible operations.

## Design
The runtime environment stores class attributes, method implementations, and inheritance relations. Objects are
modeled with field tables and defaults for uninitialized attributes. A value domain is defined with `Int`, `Bool`,
`String`, objects, and `Void`. Scoping is handled lexically with activation records pushed and popped as blocks,
lets, and methods execute. The evaluator implements COOL's semantics recursively evaluating expressions, dispatching
methods (both user defined and internal), and applying runtime checks.

## Implementation
The `eval` function is the core engine, pattern matching on expression kinds and producing runtime values. Built in
methods for `Object`, `IO`, and `String` are provided internally, while user methods execute within new scopes with bound
formals. Attribute initialization is applied in ancestor to descendant order as per the COOL reference manual. Dynamic,
static, and self dispatch fully supported, with `SELF_TYPE` handled by resolving to the runtime class of `self`. On error, 
the system prints a location tagged runtime message and exits, ensuring deterministic and auditable behavior.

## Testing
Testing for the Operational Semantics phase focused on verifying the correctness of runtime behavior and the faithful implementation 
of COOL’s execution model. Unlike the Lexer and Parser, this phase required a distinct suite of test cases targeting dynamic evaluation 
rather than syntactic or static correctness. The tests concentrated on runtime-specific constructs, including object creation, 
inheritance-based attribute initialization, method dispatch (both static and dynamic), and evaluation of control structures such as 
conditionals, loops, and case expressions. Special attention was given to SELF_TYPE handling, object aliasing, and correct propagation 
of scope across nested blocks and method calls.

Additional test cases were designed to trigger well-defined runtime errors such as dispatch on void, division by zero, unbound identifiers, 
and unmatched case branches to ensure precise and consistent error reporting. Delta debugging was selectively applied to isolate complex 
interactions between heap management and dispatch resolution, particularly in programs with deeply nested inheritance or aliasing behavior. 
All tests operated on well-typed COOL programs to ensure that observed failures reflected true semantic violations rather than earlier lexical 
or syntactic errors. The successful execution of these tests confirmed that the interpreter faithfully models COOL’s operational semantics and
provides deterministic, auditable runtime behavior aligned with the language specification.

## References
[1] “The Cool Reference Manual,” Alex Aiken (et al.), Stanford University, The COOL Language Project, Jan. 2011. 
[Online]. Available: https://theory.stanford.edu/~aiken/software/cool/cool-manual.pdf

[2] “Video Guide - PA5” YouTube, uploaded by westleyweimer6512, 
[Online]. Available: https://www.youtube.com/watch?v=eQRRlZoVxSc

[3] “Video Guide - PA5c” YouTube, uploaded by westleyweimer6512, 
[Online]. Available: https://www.youtube.com/watch?v=i-nBIxE5Xcs

[4] “Video Guide - PA5t” YouTube, uploaded by westleyweimer6512, 
[Online]. Available: https://www.youtube.com/watch?v=dWpJQ0lrJhs



