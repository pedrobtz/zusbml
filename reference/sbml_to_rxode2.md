# Write a model as rxode2 text

Prints the derived equations
([`sbml_odes()`](https://pedrobtz.github.io/zusbml/reference/sbml_odes.md))
in the model syntax of the rxode2 package: constants and assignments as
`x = ...;`, initial conditions as `x(0) = ...;` and right-hand sides as
`d/dt(x) = ...;`. The text is self-contained; edit the constants or pass
parameters to override them.

## Usage

``` r
sbml_to_rxode2(m, events = c("refuse", "drop"))
```

## Arguments

- m:

  An `sbml_model`.

- events:

  `"refuse"` (the default) raises `zusbml_unsupported` for a model with
  events; `"drop"` leaves them out.

## Value

A single string of rxode2 model code.

## Details

rxode2 text cannot express SBML events, which change states at triggered
times, so a model with events is refused unless `events = "drop"`, which
writes the continuous part only (design D10). Names rxode2 cannot take,
ids starting with `_` and those it reserves such as `time` or `param`,
are written with a `.u` prefix: `_J0` becomes `.u_J0`.

## Examples

``` r
m <- sbml_read(system.file("extdata", "decay.xml", package = "zusbml"))
cat(sbml_to_rxode2(m))
#> # decay, written by zusbml from SBML Level 3 Version 2
#> C = 1;
#> k1 = 1;
#> S1(0) = 1;
#> S2(0) = 0;
#> R1 = C * k1 * S1;
#> d/dt(S1) = -R1/C;
#> d/dt(S2) = R1/C;
```
