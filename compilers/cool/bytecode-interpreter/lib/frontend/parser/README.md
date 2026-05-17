# The Parser
Trey Rubino - 
CPSC 372 -
Dr. Schwesinger

## Project Structure
- `main.mly`: OCamlyacc grammar specification for COOL
- `Makefile`: Build instructions for compiling and testing with the OCaml toolchain  
- `build/`: Directory containing generated parser artifacts and compiled output  
- `test/`: Test files displaying good and bad test cases
  
## Overview
The Parser phase takes the stream of tokens produced by the lexer and builds an abstract syntax tree (AST)
that captures the structure of COOL programs. Parsing enforces the language's grammar rules and ensures
that token sequences form syntactically valid programs. Errors detected here include unexpected or misplaced 
tokens, missing delimiters, and violation of COOL's grammar. This phase is critical for bridging lexical
analysis and later semantic checks.

## Design
The Parser is implemented with OCamlyacc and structured as a context free grammar (CFG) over COOL's syntax. It
defines productions for classes, features, formals, expression, and control structures, each mapped to a 
corresponding AST node. Operator precedence and associativity are specified to disambiguate expressions such
as arithmetic and comparisons. The grammar closely follows the COOL reference manual to preserve 
compatibility with the language definition.

## Implementation
The actual grammar is implemented in `131` lines, just above the reference compiler's `116` line implementation.
AST node types are defined in the parser specification, with OCaml constructors representing classes,
methods, attributes, and expressions. Grammar rules use line numbered tokens to attach location information
for error reporting. The parser integrates with the lexer output by deserializing token streams and
feeding them into OCamlyacc's parsing engine. On syntax errors, the parser reports the offending token and line number,
then exits cleanly. Successfully parsed programs are serialized as `.cl-ast` files for later semantic analysis.

## Testing
Testing for the Parser focused on ensuring the grammar correctly recognized valid COOL program structures and rejected malformed 
input with precise error reporting. The same suite of test cases used during the Lexer phase was extended for the Parser, allowing 
a consistent evaluation across both components. These tests verified that syntactically correct programs were successfully transformed 
into well formed abstract syntax trees, while invalid programs produced clear, line specific parser errors. Test scenarios included 
variations in class definitions, feature lists, nested expressions, operator precedence, and block structures to confirm that all grammar 
rules behaved as expected. Additional malformed inputs, such as missing semicolons, unmatched braces, or misplaced keywords, were 
introduced to confirm accurate detection of syntax violations. Overall, testing demonstrated that the parser reliably bridges the lexer’s 
token stream to the AST representation, maintaining conformity with the COOL grammar and producing output suitable for subsequent semantic analysis.

## References
[1] “The Cool Reference Manual,” Alex Aiken (et al.), Stanford University, The COOL Language Project, Jan. 2011. 
[Online]. Available: https://theory.stanford.edu/~aiken/software/cool/cool-manual.pdf

[2] “Video Guide - PA3 Parser - OCaml” YouTube, uploaded by westleyweimer6512, 
[Online]. Available: https://www.youtube.com/watch?v=3xCJMyawoxg

[3] A. V. Aho, J. D. Ullman, R. Sethi, and M. S. Lam, Compilers: Principles, Techniques, and Tools, 2nd ed., ch. 4, “Syntax Analysis,” Pearson/Addison-Wesley, 2006.