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
