# ELSA

`elsa` is a tiny language designed to build
intuition about how the Lambda Calculus, or
more generally, _computation-by-substitution_ works.

Rather than the usual interpreter that grinds
lambda terms down to values, `elsa` aims to be
a light-weight _proof checker_ that determines
whether, under a given sequence of definitions,
a particular term _reduces to_ to another.

## Full Evaluation

For example, `elsa` programs look like:

```haskell
-- id_0.lc
id   = \x -> x
zero = \f x -> x

eval id_zero :
  id zero
  =d> (\x -> x) (\f x -> x)
  =b> (\f x -> x)
  =d> zero
```

When you run it, you should get the following output:

```bash
$ elsa ex1.lc

OK id_zero.
```

## Partial Evaluation

If instead you write a partial sequence of
reductions, i.e. where the _last_ term can
still be further reduced:

```haskell
-- succ_1_bad.lc
one  = \f x -> f x
two  = \f x -> f (f x)
incr = \n f x -> f (n f x)

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
one  = \f x -> f x
two  = \f x -> f (f x)
incr = \n f x -> f (n f x)

eval succ_one :
  incr one
  =d> (\n f x -> f (n f x)) (\f x -> f x)
  =b> \f x -> f ((\f x -> f x) f x)
  =b> \f x -> f ((\x -> f x) x)
  =b> \f x -> f (f x)
  =d> two
```

Similarly, `elsa` rejects the following program,

```haskell
-- id_0_bad.lc
id   = \x -> x
zero = \f x -> x

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
[<id> = <term>]+

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
```


## Semantics of `elsa` programs

A `reduction` of the form `t_1 s_1 t_2 s_2 ... t_n` is **valid** if

* Each `t_i s_i t_i+1` is **valid**, and
* `t_n` is in normal form (i.e. cannot be further beta-reduced.)

Furthermore, a `step` of the form  

* `t =a> t'` is valid if `t` and `t'` are equivalent up to alpha-renaming,
* `t =b> t'` is valid if `t` beta-reduces to `t'` in a single step,
* `t =d> t'` is valid if `t` and `t'` are identical after def expansion.
