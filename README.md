# Lambda-Interpreter

An Untyped Lambda Calculus Interpreter in Haskell

This project consists of a lambda calculus expression parser using Parsec,
and an eval-apply interpreter. The basis of the parser was implemented by
Ioannis V.

## Parser - Printer

Lambda expressions should abide to the following format.

The term
```
λx.λy.xy
```
should be written as
```
\\x.\\y.xy
```

Only one symbol variables are supported.

The two main functions:

* **myparse** parses a given lambda expression string.

* **prettyprint** pretty prints a lambda expression.

## Environment

Τhe parser and the printer offer an environment for easier interaction with the
interpreter. The user can write small lambda calculus programs without recalling
any lambda terms.

A simple program using the enviroment feature
```
(\\x.@if@iszero(x)(@true)(@false))(2)
```

The same program in pure lambda calculus
```
(\\x.(\\p.\\a.\\b.pab)(\\n.n(\\x.(\\a.\\b.b))(\\a.\\b.a))(x)(\\a.\\b.a)(\\a.\\b.b))(\\f.\\x.f(f(x)))
```

### Environment Commands

| Command | Description |
| --- | --- |
| @succ | Successor of a number |
| @pred | Predecessor of a number |
| @plus | Addition of two numbers |
| @sub  | Subtraction of two numbers |
| @mult | Multiplication of two numbers |
| @pow  | Exponentiation of two numbers |
| @true | Boolean value TRUE |
| @false | Boolean value FALSE |
| @and | Logic operator AND |
| @or | Logic operator OR |
| @not | logic operator NOT |
| @if | IF-ELSE statement |
| @iszero | Returns @true if the number is zero |
| @Y | Y combinator for recursion |

*Number* means *Church numeral*

## Interpreter

The evaluation strategy that is used is normal order. The leftmost, outermost
redex is always reduced first. That is, whenever possible the arguments are
substituted into the body of an abstraction before the arguments are reduced.

There are four main functions provided:

* **run** given a lambda expression string, applies consecutive beta reductions
and prints the result.

* **runp** given a lambda expression string, applies consecutive beta reductions
and prints every reduction of the evaluation process.

* **reduce** applies consecutive beta and eta reductions to the given lambda
term and returns a result of the final term, the intermidiate reductions, their
number and their type.

* **b_reduce** same as reduce but without eta reductions.

## Examples

#### Function that returns true if its parameter is zero.

Command
```
run "(\\x.@if@iszero(x)(@true)(@false))(2)"
```
Output
```
@false
```

Command
```
runp "(\\x.@if@iszero(x)(@true)(@false))(2)"
```
Output
```
beta	->	(\p.\a.\b.pab)(\n.n(\x.(@false))((@true)))(2)((@true))((@false))
beta	->	(\a.\b.(\n.n(\x.(@false))((@true)))ab)(2)((@true))((@false))
beta	->	(\b.(\n.n(\x.(@false))((@true)))(2)b)((@true))((@false))
beta	->	(\n.n(\x.(@false))((@true)))(2)((@true))((@false))
beta	->	(2)(\x.(@false))((@true))((@true))((@false))
beta	->	(\#.(\x.(@false))((\x.(@false))#))((@true))((@true))((@false))
beta	->	(\x.(@false))((\x.(@false))((@true)))((@true))((@false))
beta	->	((@false))((@true))((@false))
beta	->	(\$.$)((@false))
beta	->	(@false)
```

