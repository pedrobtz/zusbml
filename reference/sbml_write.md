# Write a model as SBML

Writes an `sbml_model` as an SBML Level 3 Version 2 core document
(design D9), whatever level and version it was read from. Attributes
that are `NA` are left out; `notes`, `annotation` and constraint
messages are written as they were read; math is written as MathML.
Reading the result gives back an identical model for any Level 3 Version
2 input.

## Usage

``` r
sbml_write(m, path = NULL)
```

## Arguments

- m:

  An `sbml_model`.

- path:

  A file to write, or `NULL` to return the document as a string.

## Value

The path, invisibly, or the document as a string when `path` is `NULL`.

## Details

`zuxml` cannot build nodes, so the document is written as text (design
D14), escaping attribute values and names.

## Errors

`zusbml_write_error` for math with no MathML equivalent, naming the R
call.

## Examples

``` r
m <- sbml_read(system.file("extdata", "decay.xml", package = "zusbml"))
path <- tempfile(fileext = ".xml")
sbml_write(m, path)
identical(sbml_read(path), m)
#> [1] TRUE
```
