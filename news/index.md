# Changelog

## zusbml 0.0.0.9000

- [`sbml_read()`](https://pedrobtz.github.io/zusbml/reference/sbml_read.md)
  reads SBML Level 3 Version 1 and 2 core documents from a file, a URL,
  XML text or a raw vector into an `sbml_model`, with one data frame per
  element kind
  ([`sbml_species()`](https://pedrobtz.github.io/zusbml/reference/sbml_components.md),
  [`sbml_reactions()`](https://pedrobtz.github.io/zusbml/reference/sbml_components.md),
  …). Malformed documents raise `zusbml_parse_error` naming the element
  path and attribute; SBML packages and other levels raise
  `zusbml_unsupported`, and `ignore_packages = TRUE` reads the core of a
  package document.

- Model math is R language objects: `<apply><power/>` is `^`,
  `<piecewise>` is nested `if`, lambdas are `function` expressions, and
  operators without a base R equivalent keep their SBML names
  (`quotient()`, `rateOf()`, …).
  [`sbml_math()`](https://pedrobtz.github.io/zusbml/reference/sbml_math.md)
  shows math as R code, and
  [`sbml_functions()`](https://pedrobtz.github.io/zusbml/reference/sbml_components.md)
  adds each function definition as a callable R function.

- [`sbml_odes()`](https://pedrobtz.github.io/zusbml/reference/sbml_odes.md)
  derives the states, right-hand sides, assignments and constants of a
  model without events, following the Level 3 Version 2 specification,
  and
  [`sbml_initial_state()`](https://pedrobtz.github.io/zusbml/reference/sbml_odes.md)
  gives the states at time 0. Algebraic rules, the `delay` csymbol and
  fast reactions are refused with `zusbml_unsupported`; assignment
  cycles and missing values are `zusbml_derive_error`. All 964
  event-free supported SBML Test Suite cases reproduce their reference
  results.

- [`zusbml_info()`](https://pedrobtz.github.io/zusbml/reference/zusbml_info.md)
  reports what the package reads and writes.
