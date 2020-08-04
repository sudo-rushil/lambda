# Lambda

An interpreter for the untyped lambda calculus.

## Overview

`lambda` is a straightforward REPL for lambda expressions. On top of the standard syntax of the lambda calculus, it also enables named bindings, while maintaining functional purity. This makes it a good way to build an understanding of the lambda calculus. Moreover, the source code is all straightforward, providing a readable example of (simple) language implementation using tools like Alex and Happy.

## Requirements
- [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

## Installation

#### Install from source

```sh
$ git clone https://github.com/sudo-rushil/lambda.git
$ cd lambda
$ stack build
$ stack install lambda
```

This will copy the `lambda` executable into `~/.local/bin`, which should be on your path.

## Example

```
$ lambda
 λ> (\x.x) y                     -- evaluation
 y

 λ> (\x.x x)(\x.x x)             -- no infinite recursion
 ((λx.(x x)) (λx.(x x)))

 λ> (\y.z)((\x.x x)(\x.x x))     -- normal order reduction
 z

 λ> (\p q.p q p)                 -- multiple binding syntax
 (\p.\q.p q p)

 λ> 0                            -- built-in
 (λf.(λx.x))

 λ> 1                            -- built-in
 (λf.(λx.(f x)))

 λ> succ                         -- built-in
 (λn.(λf.(λx.(f ((n f) x)))))

 λ> let 2 = succ 1               -- define new bindings

 λ> 2                            -- new bindings are visible
 (λf.(λx.(f (f x))))

```

## Usage

This assumes you have seen the basic grammar and reduction semantics of the untyped lambda calculus. A quick review is given below.

### Lambda Calculus Review

#### Grammar

The untyped lambda calculus has three kind of terms:

```
M, N ::= x                    -- variables
      |  λx.M                 -- abstractions
      |  M N                  -- applications
```

#### Reduction

The "evaluation" of a lambda expression is the successive reductions of the expression to normal form, in which no further reductions are possible. The key reduction operation, beta reduction, captures the idea of function application, and is defined as

```
(λx.E) F --> E[x := F]
```

Here, all instances of variable `x` within expression `E` are substituted by `F`. Under normal order reduction, which `lambda` uses, this substitution occurs *before* `F` is evaluated. Also note that this substitution is capture-avoiding, in that variables which are free in F but bound in E will be alpha-renamed such as to preserve semantic meaning.

### `Lambda`

In `lambda`, the only major difference between the formal lambda calculus grammar and the interpreter grammar is the use of `\` instead of the `λ` symbol. We encourage you to play around with writing abstractions, applications, and the like to get used to thinking in terms of the lambda calculus.

In addition, pass the `-d` flag to the `lambda` executable to get a debug printout of the AST your input parses into.

#### Syntactic Sugars

For convenience, we support the syntax of having multiple variables bound by a single lambda. Note that, because identifers in `Lambda` can be more than one character, separate variables need to be whitespace separated.

```
$ lambda -d
Running in debug mode
 λ> \xyz.x y z
Expr: Abs "xyz" (App (App (Var "x") (Var "y")) (Var "z"))
 (λxyz.((x y) z))

 λ> \x y z.x y z
Expr: Abs "x" (Abs "y" (Abs "z" (App (App (Var "x") (Var "y")) (Var "z"))))
 (λx.(λy.(λz.((x y) z))))

```

#### Bindings

In order to make `lambda` a bit more practical than the frugal pure lambda calculus, we allow for binding lambda expressions to variables using `let` statements.

```
 λ> let <var> = <expr>
```

The binding semantics act somewhat like macro expressions, and all variables bound by the environment are expanded before any evaluation takes place.

In addition, the following seven variables are bound by default for convenience.

```
0     := λf.λx.x
1     := λf.λx.f x
succ  := λn.λf.λx.f (n f x)
#t    := λt.λf.t
#f    := λt.λf.f
and   := λp.λq.p q p
or    := λp.λq.p p q
```

Any bindings you make with `let` last until the end of the session. If you feel like a certain binding is common enough that it should be a default, file an [issue](https://github.com/sudo-rushil/lambda/issues/new) or a pull request.


## TODO:
- Eta reductions
- Files and stdlib
- de Bruijn notation
