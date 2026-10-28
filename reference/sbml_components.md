# Model components

Each accessor returns one data frame of an `sbml_model`, one row per
element. Scalar attributes are columns, named in snake_case after the
SBML attribute (`initialAmount` is `initial_amount`); optional
attributes that are absent are `NA`. Math is a list column. Every table
also has `metaid`, `sbo_term`, and `notes` and `annotation` as XML text.

## Usage

``` r
sbml_compartments(m)

sbml_species(m)

sbml_parameters(m)

sbml_reactions(m)

sbml_rules(m)

sbml_events(m)

sbml_functions(m)

sbml_initial_assignments(m)

sbml_constraints(m)

sbml_unit_definitions(m)
```

## Arguments

- m:

  An `sbml_model`.

## Value

A data frame.

## Details

`sbml_reactions()` holds the species references (`reactants`,
`products`, `modifiers`) as data frames and `kinetic_law` as a list with
`math` and `local_parameters`. `sbml_events()` holds `trigger`,
`priority`, `delay` and `assignments`. `sbml_rules()` has a `type`
column: `"assignment"`, `"rate"` or `"algebraic"`. `sbml_functions()`
adds `fun`, each lambda as an R function; see
[`sbml_math()`](https://pedrobtz.github.io/zusbml/reference/sbml_math.md)
for how math is represented.

## Examples

``` r
m <- sbml_read(system.file("extdata", "decay.xml", package = "zusbml"))
sbml_species(m)
#>   id name compartment initial_amount initial_concentration substance_units
#> 1 S1 <NA>           C              1                    NA            <NA>
#> 2 S2 <NA>           C              0                    NA            <NA>
#>   has_only_substance_units boundary_condition constant conversion_factor metaid
#> 1                    FALSE              FALSE    FALSE              <NA>   <NA>
#> 2                    FALSE              FALSE    FALSE              <NA>   <NA>
#>   sbo_term notes annotation
#> 1     <NA>  <NA>       <NA>
#> 2     <NA>  <NA>       <NA>
sbml_reactions(m)$reactants[[1]]
#>     id name species stoichiometry constant metaid sbo_term notes annotation
#> 1 <NA> <NA>      S1             1     TRUE   <NA>     <NA>  <NA>       <NA>
```
