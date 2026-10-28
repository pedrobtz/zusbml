# Classed conditions (design section 8). Every condition carries the fields
# a caller needs to act on it, so tests assert on class and fields, never on
# message text.
zusbml_abort <- function(class, message, ..., call = NULL) {
  cnd <- structure(
    class = c(class, "zusbml_error", "error", "condition"),
    list(message = message, call = call, ...)
  )
  stop(cnd)
}

abort_parse <- function(
  message,
  path = NA_character_,
  attribute = NA_character_,
  section = NA_character_,
  file = NA_character_
) {
  location <- if (!is.na(path)) paste0(" at ", path) else ""
  if (!is.na(attribute)) {
    location <- paste0(location, ", attribute '", attribute, "'")
  }
  reference <- if (!is.na(section)) paste0(" (SBML L3V2 ", section, ")") else ""
  zusbml_abort(
    "zusbml_parse_error",
    paste0(message, location, reference, "."),
    file = file,
    path = path,
    attribute = attribute,
    section = section
  )
}

abort_unsupported <- function(
  message,
  construct,
  id = NA_character_,
  package = NA_character_,
  section = NA_character_
) {
  zusbml_abort(
    "zusbml_unsupported",
    message,
    construct = construct,
    id = id,
    package = package,
    section = section
  )
}

# A bad argument to an exported function.
abort_arg <- function(message, argument) {
  zusbml_abort("zusbml_arg_error", message, argument = argument)
}
