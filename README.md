# ELSA

`elsa` is a tiny language designed to build
intuition about how the Lambda Calculus, or
more generally, _computation-by-substitution_ works.
Rather than the usual interpreter that grinds
lambda terms down to values, `elsa` aims to be
a light-weight _proof checker_ that determines
whether, under a given sequence of definitions,
a particular term _reduces to_ to another.

## Online Demo

You can try `elsa` online at [this link](https://elsa.goto.ucsd.edu/index.html)


## Install

You can locally build and run `elsa` by

1. Installing [stack](https://www.haskellstack.org)
2. Cloning this repo
3. Building `elsa` with `stack`.

That is, to say

```bash
$ curl -sSL https://get.haskellstack.org/ | sh
$ git clone https://github.com/ucsd-progsys/elsa.git
$ cd elsa
$ stack install
```
## Editor Plugins

- [VS Code extension](https://marketplace.visualstudio.com/items?itemName=akainth015.elsa-lang) with syntax highlighting and autocompletion support
  - [Source](https://github.com/akainth015/vscode-elsa-lang)
  - Contributed by [**@akainth015**](https://github.com/akainth015/), based on the [original version](https://github.com/mistzzt/vscode-elsa-lang) by [**@mistzzt**](https://github.com/mistzzt)
- [Vim](https://github.com/glapa-grossklag/elsa.vim)

## Overview

`elsa` programs look like:

```haskell
-- id_0.lc
let id   = \x -> x
let zero = \f x -> x

eval id_zero :
  id zero
  =d> (\x -> x) (\f x -> x)   -- expand definitions
  =a> (\z -> z) (\f x -> x)   -- alpha rename
  =b> (\f x -> x)             -- beta reduce
  =d> zero                    -- expand definitions

eval id_zero_tr :
  id zero  
  =*> zero                    -- transitive reductions
```

When you run `elsa` on the above, you should get the following output:

```bash
$ elsa ex1.lc

OK id_zero, id_zero_tr.
```

## Partial Evaluation

If instead you write a partial sequence of
reductions, i.e. where the _last_ term can
still be further reduced:

```haskell
-- succ_1_bad.lc
let one  = \f x -> f x
let two  = \f x -> f (f x)
let incr = \n f x -> f (n f x)

eval succ_one :
  incr one
  =d> (\n f x -> f (n f x)) (\f x -> f x)
  =b> \f x -> f ((\f x -> f x) f x)
  =b> \f x -> f ((\x -> f x) x)
```

Then `elsa` will complain that

```bash
$ elsa ex2.lc

ex2.lc:11:7-30: succ_one can be further reduced

  11  |   =b> \f x -> f ((\x -> f x) x)
              ^^^^^^^^^^^^^^^^^^^^^^^^^
```

You can _fix_ the error by completing the reduction

```haskell
-- succ_1.lc
let one  = \f x -> f x
let two  = \f x -> f (f x)
let incr = \n f x -> f (n f x)

eval succ_one :
  incr one
  =d> (\n f x -> f (n f x)) (\f x -> f x)
  =b> \f x -> f ((\f x -> f x) f x)
  =b> \f x -> f ((\x -> f x) x)
  =b> \f x -> f (f x)                 -- beta-reduce the above
  =d> two                             -- optional
```

Similarly, `elsa` rejects the following program,

```haskell
-- id_0_bad.lc
let id   = \x -> x
let zero = \f x -> x

eval id_zero :
  id zero
  =b> (\f x -> x)
  =d> zero
```

with the error

```bash
$ elsa ex4.lc

ex4.lc:7:5-20: id_zero has an invalid beta-reduction

   7  |   =b> (\f x -> x)
          ^^^^^^^^^^^^^^^
```

You can fix the error by inserting the appropriate
intermediate term as shown in `id_0.lc` above.

## Syntax of `elsa` Programs

An `elsa` program has the form

```haskell
-- definitions
[let <id> = <term>]+

-- reductions
[<reduction>]*
```

where the basic elements are lambda-calulus `term`s

```haskell
<term> ::=  <id>
          \ <id>+ -> <term>
            (<term> <term>)
```

and `id` are lower-case identifiers            

```
<id>   ::= x, y, z, ...
```

A `<reduction>` is a sequence of `term`s chained together
with a `<step>`

```haskell
<reduction> ::= eval <id> : <term> (<step> <term>)*

<step>      ::= =a>   -- alpha equivalence
                =b>   -- beta  equivalence
                =d>   -- def   equivalence
                =*>   -- trans equivalence
                =~>   -- normalizes to
```


## Semantics of `elsa` programs

A `reduction` of the form `t_1 s_1 t_2 s_2 ... t_n` is **valid** if

* Each `t_i s_i t_i+1` is **valid**, and
* `t_n` is in normal form (i.e. cannot be further beta-reduced.)

Furthermore, a `step` of the form  

* `t =a> t'` is valid if `t` and `t'` are equivalent up to **alpha-renaming**,
* `t =b> t'` is valid if `t` **beta-reduces** to `t'` in a single step,
* `t =d> t'` is valid if `t` and `t'` are identical after **let-expansion**.
* `t =*> t'` is valid if `t` and `t'` are in the reflexive, transitive closure
             of the union of the above three relations.
* `t =~> t'` is valid if `t` [normalizes to][normalform] `t'`.


(Due to Michael Borkowski)

The difference between `=*>` and `=~>` is as follows.

* `t =*> t'` is _any_ sequence of zero or more steps from `t` to `t'`. 
  So if you are working forwards from the start, backwards from the end, 
  or a combination of both, you could use `=*>` as a quick check to see 
  if you're on the right track. 

* `t =~> t'` says that `t` reduces to `t'` in zero or more steps **and** 
   that `t'` is in **normal form** (i.e. `t'` cannot be reduced further). 
   This means you can only place it as the *final step*. 

So `elsa` would accept these three

```
eval ex1:
  (\x y -> x y) (\x -> x) b 
  =*> b

eval ex2:
  (\x y -> x y) (\x -> x) b 
  =~> b

eval ex3:
  (\x y -> x y) (\x -> x) (\z -> z) 
  =*> (\x -> x) (\z -> z) 
  =b> (\z -> z)
```

but `elsa` would *not* accept 

```
eval ex3:
  (\x y -> x y) (\x -> x) (\z -> z) 
  =~> (\x -> x) (\z -> z) 
  =b> (\z -> z)
```

because the right hand side of `=~>` can still be reduced further.

[normalform]: http://dl.acm.org/citation.cfm?id=860276
[normalform-pdf]: http://www.cs.cornell.edu/courses/cs6110/2014sp/Handouts/Sestoft.pdf
