# zusbml 0.1.0 cran-comments

## Submission

This is a new submission.

zusbml reads, writes and derives equations from SBML (Systems Biology Markup
Language) models in pure R. Its only import is 'zuxml', on CRAN.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Test environments

The package is checked with `R CMD check --as-cran` by the
pedrobtz/r-actions continuous integration workflows on every change:

* macOS (latest), R release
* Windows (latest), R release
* Windows (latest), R devel
* Ubuntu (latest), R release
* Ubuntu (latest), R oldrel-1
* r-devel containers: clang23, ubuntu-clang, ubuntu-gcc16
* nosuggests (no suggested packages installed)
* nold (R built without long double)

## Method references

The package implements the SBML Level 3 Version 2 Core specification,
linked in the Description field
(<https://sbml.org/documents/specifications/>). It is checked against the
SBML Test Suite and curated models from BioModels; neither is included in
the package.
