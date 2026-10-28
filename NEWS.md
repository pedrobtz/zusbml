# zusbml 0.0.0.9000

* `sbml_read()` reads SBML Level 3 Version 1 and 2 core documents from a
  file, a URL, XML text or a raw vector into an `sbml_model`, with one data
  frame per element kind (`sbml_species()`, `sbml_reactions()`, ...).
  Malformed documents raise `zusbml_parse_error` naming the element path
  and attribute; SBML packages and other levels raise `zusbml_unsupported`,
  and `ignore_packages = TRUE` reads the core of a package document.

* Model math is R language objects: `<apply><power/>` is `^`, `<piecewise>`
  is nested `if`, lambdas are `function` expressions, and operators without
  a base R equivalent keep their SBML names (`quotient()`, `rateOf()`,
  ...). `sbml_math()` shows math as R code, and `sbml_functions()` adds each
  function definition as a callable R function.

* `sbml_odes()` derives the states, right-hand sides, assignments,
  constants and events of a model, following the Level 3 Version 2
  specification, and `sbml_initial_state()` gives the states at time 0.
  Algebraic rules, the `delay` csymbol and fast reactions are refused with
  `zusbml_unsupported`; assignment cycles and missing values are
  `zusbml_derive_error`.

* Events are derived and run: `sbml_derivs()` returns a derivative function
  for deSolve with the root function and event handler that apply SBML's
  event semantics (delays, persistence, priorities, values from trigger
  time, events at time 0). All 1,384 supported SBML Test Suite cases
  reproduce their reference results.

* `sbml_to_rxode2()` writes the equations of a model as rxode2 model text;
  events are refused, or left out with `events = "drop"`.

* `sbml_write()` writes a model as SBML Level 3 Version 2; reading it back
  gives an identical model for every readable SBML Test Suite case.
  `sbml_write_json()` writes the model object as JSON with zujson.

* `sbml_check()` reports structural problems (duplicate ids, unresolved
  references, symbols set twice, rules or events on constants, assignment
  cycles) as a data frame.

* `zusbml_info()` reports what the package reads and writes.
