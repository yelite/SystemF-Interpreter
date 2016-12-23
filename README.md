This is a self-interpreter for System F. The idea is from the paper [Breaking Through the Normalization Barrier: A Self-Interpreter for F-omega](http://web.cs.ucla.edu/~palsberg/paper/popl16-full.pdf).

## How to play
With haskell tool stack,
```
stack ghci
```

## Example
```haskell
main :: IO ()
main = validate (App (TApp identity (typecheck unquote)) unquote)
```

```
The representation:
λid:(∀t. t -> t). id ((∀a. ((∀t. t -> t) -> a) -> a) -> (∀a. ((∀t. t -> t) -> a) -> a)) id (∀t. t -> t) (Λt. λx:t. x) (∀a. ((∀t. t -> t) -> a) -> a) (Λa. λq:(∀t. t -> t) -> a. id ((∀t. t -> t) -> a) q (Λt. λx:t. x))

The original type:
∀a. ((∀t. t -> t) -> a) -> a
The result type:
∀a. ((∀t. t -> t) -> a) -> a

The original normal form:
Λa. λq:(∀t. t -> t) -> a. q (Λt. λx:t. x)
The result normal form:
Λa. λq:(∀t. t -> t) -> a. q (Λt. λx:t. x)

The interpreted term is equivalent to the original one
```

## Description
There are function `reduce` for reduction, `typecheck` for type checking, `quote` for generating representation. The function `validate` goes through the whole process. It takes a term and check if the result from `unquote` has the same type and is equivalent to the original term.

There is no parser so the only way is writing AST directly. System F function `unquote` and `identity` are already here. You can combine them to get more complicated function, or you can write your own.
