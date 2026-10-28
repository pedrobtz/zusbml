# Read an SBML document

Reads an SBML core document into an `sbml_model`: Level 3 Versions 1 and
2, and Level 2 Versions 4 and 5, which are mapped to their Level 3
meaning. Every element carries the attributes Level 3 defines; optional
values without a default read as `NA`.

## Usage

``` r
sbml_read(x, ignore_packages = FALSE)
```

## Arguments

- x:

  A file path, a `file://`, `http://` or `https://` URL, a string
  holding the XML itself, or a raw vector.

- ignore_packages:

  A document that uses SBML Level 3 packages (`comp`, `fbc`, ...) is
  refused with a `zusbml_unsupported` error naming them. With `TRUE`,
  the core model is read, the package content is dropped, and the
  dropped package names are kept in the model's `ignored_packages`.

## Value

An `sbml_model`.

## Errors

- `zusbml_parse_error` for a malformed document: an unknown element or
  attribute in the SBML namespace, a missing required attribute, or a
  value of the wrong type. Its `path` field names the element, for
  example `model/listOfSpecies/species[3]`, and `attribute` the
  attribute.

- `zusbml_unsupported` for SBML packages, Level 1, and Level 2 before
  Version 4.

- `zusbml_arg_error` for an argument of the wrong type, a missing file
  or a directory.

## Examples

``` r
path <- system.file("extdata", "decay.xml", package = "zusbml")
m <- sbml_read(path)
m
#> <sbml_model 'decay'> SBML Level 3 Version 2
#>   compartments  1
#>   species       2
#>   parameters    1
#>   reactions     1
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
```
