# Stage 2 plan: document reader and model object

Scratch plan for
[Stage 2](../roadmap.md#stage-2--document-reader-and-model-object).

- `R/conditions.R`: `zusbml_abort()` building classed conditions with
  fields (`path`, `attribute`, `section`, `construct`, `id`, `package`).
- `R/read.R`: `sbml_read(x, ignore_packages = FALSE)` for a path, a
  `file://` or `http(s)://` URL, XML text or a raw vector. One table of
  element specifications (attributes with type and requiredness, allowed
  children) drives the walk, so unknown core elements and attributes are
  parse errors and required booleans are enforced (§6.1). Level 3 Versions
  1 and 2 read; Level 2 and Level 1 are `zusbml_unsupported`. Package
  namespaces are refused by name, or dropped and recorded with
  `ignore_packages = TRUE` (D8). `zuxml` parse errors become
  `zusbml_parse_error`.
- `R/model.R`: `sbml_model` (one data frame per element kind, list columns
  for math and nested parts), `print()`, `summary()`, accessors.
- Math stays a `zuxml` `<math>` node until Stage 3. `notes` and
  `annotation` are serialized XML text, so a re-read is `identical()`.
- `zusbml_info()`.
- Tests: one file per element kind on small inline documents, error classes
  for every §8 parse error, and the suite floor raised to read 1,386 and
  refuse 157.
