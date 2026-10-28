# Check a model's structure

Checks the structural rules derivation relies on (design section 4.5):
ids are unique, references resolve, a symbol is set by at most one rule
(or by reactions and not also by a rule), rules and events do not change
constants, and assignment rules do not form a cycle. This is not
libSBML's full rule set.

## Usage

``` r
sbml_check(m)
```

## Arguments

- m:

  An `sbml_model`.

## Value

A data frame with one row per finding: `rule` (`"duplicate_id"`,
`"unknown_reference"`, `"multiple_definitions"`, `"constant_target"` or
`"assignment_cycle"`), the `id` concerned, and a `message`. No rows when
the model passes. `sbml_check()` never raises.

## Examples

``` r
m <- sbml_read(system.file("extdata", "decay.xml", package = "zusbml"))
sbml_check(m)
#> [1] rule    id      message
#> <0 rows> (or 0-length row.names)
```
