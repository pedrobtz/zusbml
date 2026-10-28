#' @section Scope:
#' zusbml reads SBML Level 3 Versions 1 and 2 core, and Level 2 Versions 4
#' and 5 mapped to the Level 3 meaning, and writes Level 3 Version 2.
#' Derivation covers reactions, assignment and rate rules, initial
#' assignments, function definitions, `rateOf`, conversion factors,
#' compartments that change size, and events with delays, priorities and
#' persistence.
#'
#' It refuses, with a `zusbml_unsupported` error naming the construct:
#'
#' * SBML Level 3 packages (`comp`, `fbc`, `layout`, `qual`, ...), unless
#'   `sbml_read(ignore_packages = TRUE)`;
#' * Level 1 and Level 2 Versions 1 to 3;
#' * in derivation, algebraic rules (a DAE), the `delay` csymbol (a delay
#'   differential equation) and fast reactions;
#' * in [sbml_to_rxode2()], events, unless dropped on request.
#'
#' It does not check units, validate against libSBML's full rule set, or
#' simulate on its own: [sbml_derivs()] hands the equations to deSolve.
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

#' What this build of zusbml reads and writes
#'
#' @return A list: `version`, the package version; `read`, the SBML levels
#'   and versions `sbml_read()` accepts; `write`, the level and version
#'   `sbml_write()` produces; and `packages`, the SBML Level 3 packages
#'   supported (none: they are refused or, on request, ignored).
#' @export
#' @examples
#' zusbml_info()
zusbml_info <- function() {
  list(
    version = as.character(utils::packageVersion("zusbml")),
    read = c("2.4", "2.5", "3.1", "3.2"),
    write = "3.2",
    packages = character(0)
  )
}
