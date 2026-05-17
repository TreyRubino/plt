# The Lexer 
Trey Rubino - 
CPSC 425 -
Dr. Schwesinger

## Project Structure
- `main.mll`: OCamllex specification defining the lexer rules  
- `Makefile`: Build instructions for compiling with the OCaml toolchain  
- `build/`: Directory containing generated lexer artifacts and compiled output  
- `test/`: Test files displaying good and bad test cases

## Overview
The Lexer phase is responsible for scanning raw COOL source code and producing a stream of `tokens`. These
`tokens` form the smallest meaningful units of the language (or any programming language) such as identifiers,
keywords, operators, and literals. Lexical analysis is typically the first step toward parsing and ensures later 
phases receive structured input rather than raw text. Typical errors caught here include unknown characters, 
unterminated comments, overly long string constants, and unclosed strings.

## Design
The Lexer phase was implemented with OCaml (as well as the sequential phases), specifically the tool OCamllex. It
defines a `token` type that covers all COOL keywords, symbols, operators, and literal forms. Whitespace and comments
(both line and nested block comments) are skipped. Reserved words are matched in a case insensitive way, as per the COOL 
reference manual and COOLs lexical structure. Exceptions are raised for unknown characters, unclosed strings or comments, 
overly long strings, or EOF conditions for clean and centralized error handling within the `main` function.

## Implementation
The main rule set handles operators, delimiters, and keywords with explicit patterns. Identifiers and types are distinguished by
case convention (lowercase for identifiers, uppercase for types). Strings enforce a maximum length of 1024 characters. Nested
comments are supported through a recursive `comment` rule with depth tracking. The `main` function drives tokenization, serializes 
tokens with line numbers, and writes them to a `-lex` output file for later consumption by The Parser. Error messages are reported 
with line numbers to aid debugging and to satisfy the specifications provided.

## Testing
Testing for the Lexer focused on validating both correctness and robustness in token recognition, error handling, and line number tracking. 
A comprehensive suite of test files was developed under the `tests/` directory to cover valid and invalid COOL programs. Valid test cases ensured 
the lexer correctly recognized all keywords, operators, delimiters, and identifiers while accurately distinguishing between lowercase identifiers 
and uppercase types. These cases also verified proper whitespace normalization, correct handling of string and integer literals within bounds, 
and successful recognition and termination of nested block comments.

Invalid test cases were used to verify error detection and diagnostic accuracy. These included unterminated or improperly nested comments, 
unclosed string constants, string literals exceeding the 1024 character limit, and illegal or unknown characters. Each failure scenario was 
checked for the correct error message and corresponding line number to ensure consistency with the COOL reference behavior. Overall, the lexer 
successfully produced serialized token streams for syntactically correct input and precise specification compliant error reporting for malformed input.

## References
[1] “The Cool Reference Manual,” Alex Aiken (et al.), Stanford University, The COOL Language Project, Jan. 2011. 
[Online]. Available: https://theory.stanford.edu/~aiken/software/cool/cool-manual.pdf

[2] “Lexical analysis,” Wikipedia: The Free Encyclopedia, Sep. 2, 2025. 
[Online]. Available: https://en.wikipedia.org/wiki/Lexical_analysis

[3] “Video Guide - PA2 Lexer - OCaml” YouTube, uploaded by westleyweimer6512, 
[Online]. Available: https://www.youtube.com/watch?v=GpPIzjJSWls&t=194s

[4] A. V. Aho, J. D. Ullman, R. Sethi, and M. S. Lam, Compilers: Principles, Techniques, and Tools, 2nd ed., ch. 3, “Lexical Analysis,” Pearson/Addison-Wesley, 2006.