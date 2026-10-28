# Changelog

## zusbml 0.0.0.9000

- [`sbml_read()`](https://pedrobtz.github.io/zusbml/reference/sbml_read.md)
  reads SBML Level 3 Version 1 and 2 core documents, and Level 2
  Versions 4 and 5 mapped to the Level 3 meaning, from a file, a URL,
  XML text or a raw vector into an `sbml_model`, with one data frame per
  element kind
  ([`sbml_species()`](https://pedrobtz.github.io/zusbml/reference/sbml_components.md),
  [`sbml_reactions()`](https://pedrobtz.github.io/zusbml/reference/sbml_components.md),
  …). Malformed documents raise `zusbml_parse_error` naming the element
  path and attribute; SBML packages, Level 1 and Level 2 before Version
  4 raise `zusbml_unsupported`, and `ignore_packages = TRUE` reads the
  core of a package document.

- Model math is R language objects: `<apply><power/>` is `^`,
  `<piecewise>` is nested `if`, lambdas are `function` expressions, and
  operators without a base R equivalent keep their SBML names
  (`quotient()`, `rateOf()`, …).
  [`sbml_math()`](https://pedrobtz.github.io/zusbml/reference/sbml_math.md)
  shows math as R code, and
  [`sbml_functions()`](https://pedrobtz.github.io/zusbml/reference/sbml_components.md)
  adds each function definition as a callable R function.

- [`sbml_odes()`](https://pedrobtz.github.io/zusbml/reference/sbml_odes.md)
  derives the states, right-hand sides, assignments, constants and
  events of a model, following the Level 3 Version 2 specification, and
  [`sbml_initial_state()`](https://pedrobtz.github.io/zusbml/reference/sbml_odes.md)
  gives the states at time 0. Algebraic rules, the `delay` csymbol and
  fast reactions are refused with `zusbml_unsupported`; assignment
  cycles and missing values are `zusbml_derive_error`.

- Events are derived and run:
  [`sbml_derivs()`](https://pedrobtz.github.io/zusbml/reference/sbml_derivs.md)
  returns a derivative function for deSolve with the root function and
  event handler that apply SBML’s event semantics (delays, persistence,
  priorities, values from trigger time, events at time 0). All 1,384
  supported SBML Test Suite cases reproduce their reference results.

- `sbml_derivs(parms =)` takes constants to use instead of the model’s
  values; initial assignments and events at time 0 follow them in `y0`,
  and the event engine uses the `parms` the solver passes.

- [`sbml_to_rxode2()`](https://pedrobtz.github.io/zusbml/reference/sbml_to_rxode2.md)
  writes the equations of a model as rxode2 model text; events are
  refused, or left out with `events = "drop"`.

- [`sbml_write()`](https://pedrobtz.github.io/zusbml/reference/sbml_write.md)
  writes a model as SBML Level 3 Version 2; reading it back gives an
  identical model for every readable SBML Test Suite case.
  [`sbml_write_json()`](https://pedrobtz.github.io/zusbml/reference/sbml_write_json.md)
  writes the model object as JSON with zujson.

- [`sbml_check()`](https://pedrobtz.github.io/zusbml/reference/sbml_check.md)
  reports structural problems (duplicate ids, unresolved references,
  symbols set twice, rules or events on constants, assignment cycles) as
  a data frame.

- A README, the article “Simulating SBML models”, and a scope section in
  [`?zusbml`](https://pedrobtz.github.io/zusbml/reference/zusbml-package.md);
  `inst/extdata/dosing.xml` is a bundled example model with an event.

- [`zusbml_info()`](https://pedrobtz.github.io/zusbml/reference/zusbml_info.md)
  reports what the package reads and writes.
