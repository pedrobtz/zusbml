# zusbml: Read, Write and Derive Equations from 'SBML' Models

Reads Systems Biology Markup Language ('SBML') core documents, Level 3
Versions 1 and 2 and Level 2 Versions 4 and 5, into one R object with a
data frame per element kind. Writes that object back as valid Level 3
Version 2. Derives the states, initial values, right-hand sides,
assignments and events that an ordinary differential equation solver
needs, following the specification at
<https://sbml.org/documents/specifications/>. Model math becomes R
expressions that are evaluated without access to the rest of R, and
constructs the package does not support are refused rather than
approximated.

## Scope

zusbml reads SBML Level 3 Versions 1 and 2 core, and Level 2 Versions 4
and 5 mapped to the Level 3 meaning, and writes Level 3 Version 2.
Derivation covers reactions, assignment and rate rules, initial
assignments, function definitions, `rateOf`, conversion factors,
compartments that change size, and events with delays, priorities and
persistence.

It refuses, with a `zusbml_unsupported` error naming the construct:

- SBML Level 3 packages (`comp`, `fbc`, `layout`, `qual`, ...), unless
  `sbml_read(ignore_packages = TRUE)`;

- Level 1 and Level 2 Versions 1 to 3;

- in derivation, algebraic rules (a DAE), the `delay` csymbol (a delay
  differential equation) and fast reactions;

- in
  [`sbml_to_rxode2()`](https://pedrobtz.github.io/zusbml/reference/sbml_to_rxode2.md),
  events, unless dropped on request.

It does not check units, validate against libSBML's full rule set, or
simulate on its own:
[`sbml_derivs()`](https://pedrobtz.github.io/zusbml/reference/sbml_derivs.md)
hands the equations to deSolve.

## See also

Useful links:

- <https://pedrobtz.github.io/zusbml/>

- <https://github.com/pedrobtz/zusbml>

- Report bugs at <https://github.com/pedrobtz/zusbml/issues>

## Author

**Maintainer**: Pedro Baltazar <pedrobtz@gmail.com> \[copyright holder\]

Authors:

- Pedro Baltazar <pedrobtz@gmail.com> \[copyright holder\]
