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
- exceptions, optional lazy values
- builtin unit test syntax
- a primitive interactive repl
- ad-hoc overloading of functions (based on arity and checked disjoint patterns),
  note that this rules out partial function applications.
- embedded sublanguage definitions with (LL) grammars and interpreters

## Library + Code
- primitive data types booleans, option, (church) numerals, lists, streams with useful functions
- a red-black tree implementation
- regular expression matching
- combinator parsing
- a simplifying prover for propositional logic (incomplete in the logical sense)

## Expression Syntax examples (see also `src/scala/ulang/syntax.scala`)

    f x y
    \x -> f x
    
    /* abstractions can pattern match */
    \(x,y) if x <= y -> y - x | ...
    if p then a else b 
    
    // tuple, list literals
    (1, 2)
    [A, B, C]
    
    (<) // infix identifier (can be used as local variable) 
    
    // parallel let
    let x = A, y = B in e
    // destructuring let
    let (x,y) = unzip zs in ...
    
    // match many values at once
    match a, b, c with
      p, q, r -> e
    | s, t, u -> d
    
    raise x, y

    try e
    catch a, b -> c | ... // same syntax as match
    
    $ delayed
    
    // experimental: expression reification
    `e
    
