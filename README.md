ulang prototype
===============

Minimal un(i)typed functional programming language based on tagged values and pattern matching.

See `src/ulang` for some examples on the syntax.

## Features
- distinguish data constructors and identifiers by capitalizing the former
- flexible mixfix syntax including precedences
- pattern matching in function definitions, match expressions, let bindings, and lambdas
  with optional conditions
- list/tuple literals
- builtin unit test syntax
- a primitive interactive repl
- ad-hoc overloading of functions

## Library + Code
- primitive data types booleans, option, (unary) numerals, lists with useful functions
- a red-black tree implementation
- regular expression matching
- combinator parsing
- a simplifying prover for propositional logic (incomplete in the logical sense)

## Expression Syntax examples (see also `src/scala/ulang/syntax.scala`)

    f x y
    \x -> f x
    
    /* abstractions can pattern match and take several cases */
    \ Zero m -> Zero | (Succ n) m -> ...
    if p then a else b 
    
    // tuple, list literals
    (1, 2)
    [A, B, C]
    
    (<) // infix identifier (can be used as local variable) 
    
    match a with
      p -> e
    | s -> d
    
    
Currently broken

    // parallel let
    let x = A, y = B in e
    // destructuring let
    let (x,y) = unzip zs in ...
    
