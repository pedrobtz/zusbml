# zusbml 0.0.0.9000

* `sbml_read()` reads SBML Level 3 Version 1 and 2 core documents from a
  file, a URL, XML text or a raw vector into an `sbml_model`, with one data
  frame per element kind (`sbml_species()`, `sbml_reactions()`, ...).
  Malformed documents raise `zusbml_parse_error` naming the element path
  and attribute; SBML packages and other levels raise `zusbml_unsupported`,
  and `ignore_packages = TRUE` reads the core of a package document.

* `zusbml_info()` reports what the package reads and writes.
