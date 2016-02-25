# Lambda-Interpreter

An Untyped Lambda Calculus Interpreter in Haskell

This project consists of a lambda calculus expression parser using Parsec,
and an eval-apply interpreter. The basis of the parser was implemented by
Ioannis V.

## Parser - Printer

Lambda expressions should abide to the following format.

The term
    λx.λy.xy
should be written as
    \\x.\\y.xy

Only one symbol variables are suppported.

Τhe parser and the printer offer an environment for easier interaction with the
interpreter. The user can write small lambda calculus programs without recalling
any lambda terms.

A simple program using the enviroment feature
    (\\x.@if@iszero(x)(@true)(@false))(5)

The same program in pure lambda calculus
    (\\x.(\\p.\\a.\\b.pab)(\\n.n(\\x.(\\a.\\b.b))(\\a.\\b.a))(x)(\\a.\\b.a)(\\a.\\b.b))(5)

The two main functions:

* **myparse** parses a given lambda expression string.

* **prettyprint** pretty prints a lambda expression.

## Interpreter

The evaluation strategy that is used is normal order. The leftmost, outermost
redex is always reduced first. That is, whenever possible the arguments are
substituted into the body of an abstraction before the arguments are reduced.

There are four main functions provided.

* **run** given a lambda expression string, applies consecutive beta reductions
and prints the result.

* **runp** given a lambda expression string, applies consecutive beta reductions
and prints every reduction of the evaluation process.

* **reduce** applies consecutive beta and eta reductions to the given lambda
term and returns a result of the final term, the intermidiate reductions, their
number and their type.

* **b_reduce** same as reduce but without eta reductions.
   
