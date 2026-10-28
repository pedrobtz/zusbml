# Derive the equations of a model

`sbml_odes()` turns a model into what an ODE solver needs: the states,
the right-hand side of each, the assignments evaluated before the
right-hand sides at every time, and the constants.
`sbml_initial_state()` gives the states' values at time 0.

## Usage

``` r
sbml_odes(m)

sbml_initial_state(m)
```

## Arguments

- m:

  An `sbml_model`.

## Value

`sbml_odes()` returns a list:

- `states`: a data frame with the state's `name`, the model `symbol` it
  belongs to, and its `kind` (`"species"`, `"compartment"`, ...);

- `rhs`: a named list of expressions, the time derivative of each state;

- `assignments`: a named list of expressions in evaluation order;

- `constants`: a named numeric vector of the constants' values;

- `initial`: the states' values at time 0, as `sbml_initial_state()`;

- `species`: for each species, whether its symbol is an amount and its
  compartment, to convert between amounts and concentrations;

- `events`: one list per event with its `id`, `trigger`,
  `initial_value`, `persistent`, `priority` and `delay` expressions
  (`NULL` when absent), `use_values_from_trigger_time`, and
  `assignments`, a named list of expressions by target.

`sbml_initial_state()` returns a named numeric vector.

## Details

Species whose symbol is a concentration are integrated as
concentrations, except in a compartment whose size can change, where the
amount is integrated (as the state `.amount.<id>`) and the concentration
is an assignment. Reaction ids are assignments holding the reaction's
rate. Function definitions are inlined, local parameters become
constants named `.local.<reaction>.<id>`, and `rateOf()` is replaced by
the right-hand side it refers to.

## Errors

- `zusbml_unsupported` for algebraic rules (the model is then a DAE),
  the `delay` csymbol and fast reactions, each naming the element.

- `zusbml_derive_error` for an assignment cycle, a missing value, or a
  name the model does not define, naming the ids involved in its `ids`
  field.

## Examples

``` r
m <- sbml_read(system.file("extdata", "decay.xml", package = "zusbml"))
eq <- sbml_odes(m)
eq$rhs
#> $S1
#> -R1/C
#> 
#> $S2
#> R1/C
#> 
sbml_initial_state(m)
#> S1 S2 
#>  1  0 
```
