# Changes

0.3.0.0 

- bump to GHC 9.8.4 by @ilanashapiro

- new evaluation steps and strategies by @JRB-Prod-UVA
    - A new operator =e> for the eta reduction has been added.
    - Definitions introduced with let and the evaluation or confirmation statements can now be used interchangeably. So after an evaluation or confirmation block a new let binding can be introduced.
    - Reduction and equivalence checking sequence that do not have to end in a strong normal form are now also supported, by replacing the keyword `eval` with `conf`
    - Different normal form checks on arbitrary reduction and equivalence proof checking results are now supported.
    - Support for two specific reduction strategies: normal order and applicative order were added. For this, we introduced two new operators (`=n>` and `=p>`).


0.2.2.0

- Faster (and correct!) implementation of Normalization by Mark Barbone (@mb64)
- Better parse error messages by Justin Yao Du (@justinyaodo)
- Updated to work with GHC 8.10.7 by Rose Kunkel (@rosekunkel)
