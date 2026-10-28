# Simulating SBML models

This article takes a model from an SBML file to a simulation: reading
it, looking at what it holds, deriving its equations, and running them
with deSolve, events included. It ends with how the package is checked
against the SBML Test Suite.

``` r

library(zusbml)
```

## Reading a model

[`sbml_read()`](https://pedrobtz.github.io/zusbml/reference/sbml_read.md)
accepts a file path, a `file://` or `http(s)://` URL, the XML as a
string, or a raw vector. It reads SBML Level 3 Versions 1 and 2, and
Level 2 Versions 4 and 5, which it maps to their Level 3 meaning.

``` r

m <- sbml_read(system.file("extdata", "dosing.xml", package = "zusbml"))
m
#> <sbml_model 'dosing'> SBML Level 3 Version 2
#>   compartments  2
#>   species       2
#>   parameters    3
#>   reactions     2
#>   events        1
```

Every element kind is a data frame, with the SBML attributes as columns
in snake_case. Attributes a document leaves out are `NA`: Level 3 has no
defaults for them.

``` r

sbml_parameters(m)[, c("id", "value", "constant")]
#>     id value constant
#> 1   ka   1.0     TRUE
#> 2   ke   0.2     TRUE
#> 3 dose 100.0     TRUE
sbml_compartments(m)[, c("id", "size", "constant")]
#>        id size constant
#> 1     gut    1     TRUE
#> 2 central   10     TRUE
```

Math is held as R expressions.
[`sbml_math()`](https://pedrobtz.github.io/zusbml/reference/sbml_math.md)
shows it as R code.

``` r

sbml_math(sbml_reactions(m)$kinetic_law)
#> [1] "ka * drug_gut"       "ke * drug * central"
```

A document that uses an SBML Level 3 package is refused by name, with a
`zusbml_unsupported` error; `ignore_packages = TRUE` reads its core
model.
[`sbml_check()`](https://pedrobtz.github.io/zusbml/reference/sbml_check.md)
reports structural problems as a data frame and never raises:

``` r

sbml_check(m)
#> [1] rule    id      message
#> <0 rows> (or 0-length row.names)
```

## Deriving the equations

[`sbml_odes()`](https://pedrobtz.github.io/zusbml/reference/sbml_odes.md)
follows the Level 3 Version 2 specification to turn reactions and rules
into states and right-hand sides:

- a species whose symbol is an amount changes by stoichiometry times
  rate; one whose symbol is a concentration changes by that divided by
  its compartment’s size;
- a reaction id stands for its rate, and is evaluated as an assignment;
- function definitions and local parameters are inlined.

``` r

eq <- sbml_odes(m)
eq$states
#>       name   symbol    kind
#> 1 drug_gut drug_gut species
#> 2     drug     drug species
eq$rhs
#> $drug_gut
#> -absorption
#> 
#> $drug
#> (absorption - elimination)/central
eq$assignments
#> $absorption
#> ka * drug_gut
#> 
#> $elimination
#> ke * drug * central
eq$constants
#>     gut central      ka      ke    dose 
#>     1.0    10.0     1.0     0.2   100.0
sbml_initial_state(m)
#> drug_gut     drug 
#>      100        0
```

Events are carried as data:

``` r

str(eq$events[[1]][c("id", "trigger", "assignments")])
#> List of 3
#>  $ id         : chr "second_dose"
#>  $ trigger    : language time >= 12
#>  $ assignments:List of 1
#>   ..$ drug_gut: language drug_gut + dose
```

## Simulating

[`sbml_derivs()`](https://pedrobtz.github.io/zusbml/reference/sbml_derivs.md)
returns the derivative function deSolve calls, with what else it needs
as attributes: `y0`, the states at time 0 after any events that fire
then; `parms`; and, for a model with events, `rootfunc` and `events`.
Use `lsodar`, the deSolve method that finds roots.

``` r

f <- sbml_derivs(m, outputs = "absorption")
out <- deSolve::ode(
  y = attr(f, "y0"),
  times = seq(0, 24, by = 0.25),
  func = f,
  parms = attr(f, "parms"),
  method = "lsodar",
  rootfunc = attr(f, "rootfunc"),
  events = attr(f, "events")
)
```

``` r

plot(out[, "time"], out[, "drug"], type = "l",
     xlab = "Time (hours)", ylab = "Concentration in central")
```

![Concentration of the drug in the central compartment over 24 hours: it
rises to a peak near hour 2, falls, and after the second dose at hour 12
peaks again near hour
14.](zusbml_files/figure-html/unnamed-chunk-10-1.png)

The event engine applies the SBML rules for events: a trigger fires when
it changes from false to true; `initialValue` says what it was before
time 0; a delay postpones execution, and a non-persistent event is
canceled if its trigger turns false meanwhile; events due at the same
time run in priority order, in random order when tied, so set a seed for
a reproducible run.

## Writing

[`sbml_write()`](https://pedrobtz.github.io/zusbml/reference/sbml_write.md)
writes SBML Level 3 Version 2, whatever level the model was read from,
and reading the result back gives the same object:

``` r

identical(sbml_read(sbml_write(m)), m)
#> [1] TRUE
```

[`sbml_to_rxode2()`](https://pedrobtz.github.io/zusbml/reference/sbml_to_rxode2.md)
writes the equations as rxode2 model text. rxode2 cannot express SBML
events, so `events = "drop"` is needed for this model:

``` r

cat(sbml_to_rxode2(m, events = "drop"))
#> # dosing, written by zusbml from SBML Level 3 Version 2
#> gut = 1;
#> central = 10;
#> ka = 1;
#> ke = 0.20000000000000001;
#> dose = 100;
#> drug_gut(0) = 100;
#> drug(0) = 0;
#> absorption = ka * drug_gut;
#> elimination = ke * drug * central;
#> d/dt(drug_gut) = -absorption;
#> d/dt(drug) = (absorption - elimination)/central;
```

## How it is checked

The package is tested against the [SBML Test
Suite](https://github.com/sbmlteam/sbml-test-suite) 3.5.0. Each of its
1,692 Level 3 Version 2 semantic cases is read, written and read back,
derived, simulated with deSolve, and compared with the case’s results
under its own absolute and relative tolerances. The 1,384 cases within
the supported set all pass; the 308 that need algebraic rules, the
`delay` csymbol, fast reactions or SBML packages are asserted to be
refused. The 1,028 supported cases that also have a Level 2 Version 5
file pass from that file too. Fifty curated BioModels are read,
round-tripped and derived in CI. Neither the suite nor the models are
shipped: the suite is LGPL, and both are downloaded by scripts in the
repository’s `tools/` directory.
