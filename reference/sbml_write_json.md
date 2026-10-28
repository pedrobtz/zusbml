# Write a model as JSON

Writes the `sbml_model` object as JSON with the zujson package, for use
without XML: one array of records per element kind, math as R code (as
[`sbml_math()`](https://pedrobtz.github.io/zusbml/reference/sbml_math.md)
shows it), notes and annotation as XML text.

## Usage

``` r
sbml_write_json(m, path = NULL)
```

## Arguments

- m:

  An `sbml_model`.

- path:

  A file to write, or `NULL` to return the document as a string.

## Value

The path, invisibly, or the JSON as a string when `path` is `NULL`.

## Examples

``` r
m <- sbml_read(system.file("extdata", "decay.xml", package = "zusbml"))
cat(substr(sbml_write_json(m), 1, 200))
#> {
#>   "level": 3,
#>   "version": 2,
#>   "id": "decay",
#>   "name": "decay",
#>   "substance_units": null,
#>   "time_units": null,
#>   "volume_units": null,
#>   "area_units": null,
#>   "length_units": null,
#>   "extent_uni
```
