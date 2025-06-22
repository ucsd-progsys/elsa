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

## Operators and Normal Form Checking

Elsa supports several operators with optional normal form checking:

### Basic Operators
- `=a>` - alpha equivalence
- `=b>` - single beta reduction
- `=e>` - single eta reduction
- `=d>` - definition expansion
- `=n>` - normal order beta reduction
- `=p>` - applicative order beta reduction
- `=*>` - transitive closure of reductions

### Normal Form Extensions
All operators can be extended with normal form checks:
- `=op:s>` - check strong normal form after operation
- `=op:w>` - check weak normal form after operation
- `=op:h>` - check head normal form after operation

Examples:
```haskell
-- nf_0.lc
let id = \z -> z

-- Check beta reduction to weak normal form
conf example1:
  (\x y -> x (\w -> w w)) id
  =b:w> (\y -> id (\w -> w w))

-- Check beta reduction to head normal form
conf example2:
  ((\x -> x) Z) (\x y -> x (\w -> w w)) id
  =b:h> Z (\x y -> x (\w -> w w)) id

-- Normal order reduction to strong normal form
conf example3:
  (\x y -> x) id
  =n:s> \y -> id
```

### Strategy-Specific Transitive Reductions
- `=n*>` - normal order transitive reductions
- `=p*>` - applicative order transitive reductions

Example:
```haskell
-- sptr_0.lc

-- The numbers 0, 1, 3, 6 in church encoding
let c0 = \f x -> x
let c1 = \f x -> f x
let c3 = \f x -> f (f (f x))
let c6 = \f x -> f (f (f (f (f (f x)))))

-- Boolean functions
let true = \x y -> x
let false = \x y -> y

-- Number operations
let iszero = \n -> n (\x -> false) true
let pred = \n f x -> n (\g h -> h (g f)) (\u -> x) (\u -> u)
let mult = \m n f x -> m (n f) x

-- Fixed-point combinator and recursive function
let Y = \g -> (\x -> g (x x)) (\x -> g (x x))
let G = \f n -> iszero n c1 (mult n (f (pred n)))
let fact = Y G

eval factorial:
  fact c3
  -- The next line shows that we can show specific intermediate steps and leave out the rest
  =n*> iszero c3 c1 (mult c3 (((\x -> G (x x)) (\x -> G (x x))) (pred c3)))
  =n*> c6 --In this case, using =~> also works

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

Or you can change evaluation method, by changing
`eval` to `conf` (see also next section)

```haskell
-- succ_1_alt.lc
let one  = \f x -> f x
let two  = \f x -> f (f x)
let incr = \n f x -> f (n f x)

conf succ_one :
  incr one
  =d> (\n f x -> f (n f x)) (\f x -> f x)
  =b> \f x -> f ((\f x -> f x) f x)
  =b> \f x -> f ((\x -> f x) x)
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

## Confirmation Statements

The `conf` statement works like `eval` but
doesn't require the final term to be in normal
form. This is useful for infinite reductions
or intermediate proofs.

Example:
```haskell
-- om_0.lc
let omega = (\x -> x x) (\x -> x x)

conf omega_reduces_to_self:
  omega
  =d> (\x -> x x) (\x -> x x)
  =b> (\x -> x x) (\x -> x x)
  =d> omega
```

## Syntax of `elsa` Programs

An `elsa` program has the form

```haskell
-- definitions and evaluations can be mixed
  ([let <id> = <term>] | [<reduction>])*
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
<reduction> ::= (eval | conf) <id> : <term> (<step> <term>)*

<step> ::= =, <equivtype>, [:, <nfcheck>], >

<equivtype> ::= a   -- alpha equivalence
                b   -- beta  equivalence
                e   -- eta   equivalence
                d   -- def   equivalence
                *   -- trans equivalence
                n   -- normal order            beta equivalence
                p   -- applicative order       beta equivalence
                n*  -- normal order      trans beta equivalence
                p*  -- applicative order trans beta equivalence
                ~   -- normalizes to

<nfcheck> ::= s -- strong normal form check
              w -- weak normal form check
              h -- head normal form check
```


## Semantics of `elsa` programs

An `eval` `reduction` of the form `t_1 s_1 t_2 s_2 ... t_n` is **valid** if

* Each `t_i s_i t_i+1` is **valid**, and
* `t_n` is in normal form (i.e. cannot be further beta-reduced.)

Furthermore, a `step` of the form

* `t =a> t'` is valid if `t` and `t'` are equivalent up to **alpha-renaming**,
* `t =b> t'` is valid if `t` **beta-reduces** to `t'` in a single step,
* `t =d> t'` is valid if `t` and `t'` are identical after **let-expansion**.
* `t =*> t'` is valid if `t` and `t'` are in the reflexive, transitive closure
             of the union of the above three relations,
* `t =n> t'` is valid if `t` **beta-reduces** using normal order to `t'` in
             a single step,
* `t =p> t'` is valid if `t` **beta-reduces** using applicative order to `t'`
             in a single step,
* `t =n*> t'` is valid if `t` and `t'` are in the reflexive, transitive closure
             of the union of the `=a>`, `=d>` and `=n>` operator relations,
* `t =p*> t'` is valid if `t` and `t'` are in the reflexive, transitive closure
             of the union of the `=a>`, `=d>` and `=p>` operator relations,
* `t =~> t'` is valid if `t` [normalizes to][normalform] `t'`,
* `t =e> t'` is valid if `t` **eta-reduces** to `t'` in a single step.

A `conf` `reduction` of the form `t_1 s_1 t_2 s_2 ... t_n` is similar to the
`eval` `reduction` of the same form, except that `t_n` *does not* have to be
in normal form.

Each `reduction` supports an optional `nfcheck`, which specifically checks
whether the operator is in the requested normal form, in addition to checking
the functionality of the operator. For example, `t =b:w> t'` not only checks
whether `t` can be reduced to `t'` in a single step, but also whether the
result is in weak normal form.


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
