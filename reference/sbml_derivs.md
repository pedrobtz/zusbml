# A derivative function for deSolve

`sbml_derivs()` turns a model into a `function(t, y, parms)` for
[`deSolve::ode()`](https://rdrr.io/pkg/deSolve/man/ode.html). The
function carries what deSolve needs as attributes: `y0`, the states at
time 0 after any events that fire then; `parms`, the constants; and, for
a model with events, `rootfunc` and `events`, which run the events with
the full SBML semantics: delays, persistence, priorities,
`useValuesFromTriggerTime`, and events triggered by other events at the
same time.

## Usage

``` r
sbml_derivs(m, outputs = character(0), parms = NULL)
```

## Arguments

- m:

  An `sbml_model`.

- outputs:

  Model symbols to report as extra columns of the solver output, such as
  species defined by assignment rules or reaction rates.

- parms:

  A named numeric vector of constants to use instead of the model's
  values, or `NULL`.

## Value

A function with attributes `y0`, `parms`, and for models with events
`rootfunc` and `events`.

## Details

The function keeps the event queue between calls. Use it for one
integration starting at time 0; calling it again at time 0 restarts the
queue.

To change constants, pass them in `parms` here rather than editing the
`parms` attribute: initial assignments and events at time 0 that depend
on them are then reflected in `y0`. The function and its events always
use the `parms` the solver passes.

## Examples

``` r
m <- sbml_read(system.file("extdata", "decay.xml", package = "zusbml"))
f <- sbml_derivs(m)
deSolve::ode(attr(f, "y0"), seq(0, 2, by = 0.5), f, attr(f, "parms"))
#>   time        S1        S2
#> 1  0.0 1.0000000 0.0000000
#> 2  0.5 0.6065312 0.3934688
#> 3  1.0 0.3678794 0.6321206
#> 4  1.5 0.2231312 0.7768688
#> 5  2.0 0.1353360 0.8646640
```
