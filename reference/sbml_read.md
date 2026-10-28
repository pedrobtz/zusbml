# Read an SBML document

Reads an SBML Level 3 core document into an `sbml_model`. Every element
carries the attributes the specification defines for the document's
level and version; optional values without a Level 3 default read as
`NA`.

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

- `zusbml_unsupported` for SBML packages and for levels other than 3.

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
