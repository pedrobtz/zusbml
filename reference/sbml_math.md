# Show model math as R code

Model math is held as R language objects: SBML's MathML operators become
the matching R calls (`<power/>` is `^`, `<ln/>` is
[`log()`](https://rdrr.io/r/base/Log.html), `<piecewise>` is nested
`if`), the `time` csymbol is the name `time`, and lambdas are `function`
expressions. Operators without an exact base R equivalent keep their
SBML name: `quotient()`, `rem()`, `implies()`, `sec()` and the other
reciprocal trigonometric functions, `rateOf()` and `delay()`.

## Usage

``` r
sbml_math(x)
```

## Arguments

- x:

  An expression from a model, such as `sbml_rules(m)$math[[1]]`; a part
  holding one in `$math`, such as `sbml_reactions(m)$kinetic_law[[1]]`;
  or a list of either.

## Value

A character vector with one line of R code per expression, `NA` for
absent math.

## Examples

``` r
m <- sbml_read(system.file("extdata", "decay.xml", package = "zusbml"))
sbml_math(sbml_reactions(m)$kinetic_law[[1]])
#> [1] "C * k1 * S1"
```
