# Changelog

## zusbml 0.1.0

- Initial CRAN submission.

- [`sbml_read()`](https://pedrobtz.github.io/zusbml/reference/sbml_read.md)
  reads SBML Level 3 Version 1 and 2 core documents, and Level 2
  Versions 4 and 5 mapped to their Level 3 meaning, from a file, a URL,
  XML text or a raw vector, into an `sbml_model` with one data frame per
  element kind
  ([`sbml_species()`](https://pedrobtz.github.io/zusbml/reference/sbml_components.md),
  [`sbml_reactions()`](https://pedrobtz.github.io/zusbml/reference/sbml_components.md),
  …). Malformed documents raise `zusbml_parse_error` naming the element
  path and attribute; SBML packages, Level 1 and Level 2 before Version
  4 raise `zusbml_unsupported`, and `ignore_packages = TRUE` reads the
  core of a package document.
  [`sbml_check()`](https://pedrobtz.github.io/zusbml/reference/sbml_check.md)
  reports structural problems as a data frame.

- Model math is R language objects: `<apply><power/>` is `^`,
  `<piecewise>` is nested `if`, lambdas are `function` expressions, and
  operators without a base R equivalent keep their SBML names
  (`quotient()`, `rateOf()`, …).
  [`sbml_math()`](https://pedrobtz.github.io/zusbml/reference/sbml_math.md)
  shows math as R code, and
  [`sbml_functions()`](https://pedrobtz.github.io/zusbml/reference/sbml_components.md)
  adds each function definition as an R function. Math from a document
  can only call its own function definitions, and is evaluated without
  access to the rest of R.

- [`sbml_odes()`](https://pedrobtz.github.io/zusbml/reference/sbml_odes.md)
  derives the states, right-hand sides, assignments, constants and
  events of a model, following the Level 3 Version 2 specification;
  [`sbml_initial_state()`](https://pedrobtz.github.io/zusbml/reference/sbml_odes.md)
  gives the states at time 0. Algebraic rules, the `delay` csymbol and
  fast reactions are refused with `zusbml_unsupported`. A symbol defined
  twice, a definition that uses itself, math that calls anything but a
  MathML operator or function definition, and function definitions that
  inline to more than 100,000 terms raise `zusbml_derive_error`.

- [`sbml_derivs()`](https://pedrobtz.github.io/zusbml/reference/sbml_derivs.md)
  returns a derivative function for deSolve with a root function and
  event handler that apply SBML’s event semantics (delays, persistence,
  priorities, values from trigger time, events at time 0), and takes
  `parms` to change constants. All 1,384 supported SBML Test Suite 3.5.0
  cases reproduce their reference results.

- [`sbml_write()`](https://pedrobtz.github.io/zusbml/reference/sbml_write.md)
  writes a model as SBML Level 3 Version 2; reading it back gives an
  identical model for every readable SBML Test Suite case.
  [`sbml_write_json()`](https://pedrobtz.github.io/zusbml/reference/sbml_write_json.md)
  writes the model object as JSON, and
  [`sbml_to_rxode2()`](https://pedrobtz.github.io/zusbml/reference/sbml_to_rxode2.md)
  writes the equations as rxode2 model text.
