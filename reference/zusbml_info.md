# What this build of zusbml reads and writes

What this build of zusbml reads and writes

## Usage

``` r
zusbml_info()
```

## Value

A list: `version`, the package version; `read`, the SBML levels and
versions
[`sbml_read()`](https://pedrobtz.github.io/zusbml/reference/sbml_read.md)
accepts; `write`, the level and version
[`sbml_write()`](https://pedrobtz.github.io/zusbml/reference/sbml_write.md)
produces; and `packages`, the SBML Level 3 packages supported (none:
they are refused or, on request, ignored).

## Examples

``` r
zusbml_info()
#> $version
#> [1] "0.1.0"
#> 
#> $read
#> [1] "2.4" "2.5" "3.1" "3.2"
#> 
#> $write
#> [1] "3.2"
#> 
#> $packages
#> character(0)
#> 
```
