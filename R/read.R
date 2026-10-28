#' Read an SBML document
#'
#' Reads an SBML Level 3 core document into an `sbml_model`. Every element
#' carries the attributes the specification defines for the document's level
#' and version; optional values without a Level 3 default read as `NA`.
#'
#' @param x A file path, a `file://`, `http://` or `https://` URL, a string
#'   holding the XML itself, or a raw vector.
#' @param ignore_packages A document that uses SBML Level 3 packages (`comp`,
#'   `fbc`, ...) is refused with a `zusbml_unsupported` error naming them.
#'   With `TRUE`, the core model is read, the package content is dropped, and
#'   the dropped package names are kept in the model's `ignored_packages`.
#' @return An `sbml_model`.
#' @section Errors:
#' * `zusbml_parse_error` for a malformed document: an unknown element or
#'   attribute in the SBML namespace, a missing required attribute, or a
#'   value of the wrong type. Its `path` field names the element, for
#'   example `model/listOfSpecies/species[3]`, and `attribute` the attribute.
#' * `zusbml_unsupported` for SBML packages and for levels other than 3.
#' @export
#' @examples
#' path <- system.file("extdata", "decay.xml", package = "zusbml")
#' m <- sbml_read(path)
#' m
#' sbml_species(m)
sbml_read <- function(x, ignore_packages = FALSE) {
  if (
    !is.logical(ignore_packages) ||
      length(ignore_packages) != 1 ||
      is.na(ignore_packages)
  ) {
    stop("`ignore_packages` must be TRUE or FALSE.", call. = FALSE)
  }
  input <- read_input(x)
  doc <- tryCatch(
    zuxml::xml_parse(input$content),
    zuxml_error = function(cnd) {
      abort_parse(
        paste0("Not well-formed XML: ", conditionMessage(cnd)),
        file = input$file
      )
    }
  )
  ctx <- new.env(parent = emptyenv())
  ctx$file <- input$file
  ctx$ignore_packages <- ignore_packages
  ctx$ignored <- character(0)

  root <- zuxml::xml_root(doc)
  read_level(root, ctx)
  sbml <- read_element(root, "sbml", "sbml", ctx)
  check_level_attributes(sbml, ctx)
  if (is.null(sbml$children$model)) {
    abort_parse(
      "Document has no <model>",
      path = "sbml",
      section = "4.1",
      file = ctx$file
    )
  }
  new_sbml_model(sbml, ctx)
}

read_input <- function(x) {
  if (is.raw(x)) {
    return(list(content = x, file = NA_character_))
  }
  if (!is.character(x) || length(x) != 1 || is.na(x)) {
    stop("`x` must be a path, a URL, XML text or a raw vector.", call. = FALSE)
  }
  if (grepl("^\\s*<", x)) {
    return(list(content = x, file = NA_character_))
  }
  if (grepl("^(https?|file)://", x)) {
    con <- url(x, open = "rb")
    on.exit(close(con))
    chunks <- list()
    repeat {
      chunk <- readBin(con, "raw", 1024 * 1024)
      if (length(chunk) == 0) {
        break
      }
      chunks[[length(chunks) + 1]] <- chunk
    }
    return(list(content = unlist(chunks), file = x))
  }
  if (!file.exists(x)) {
    stop("File '", x, "' does not exist.", call. = FALSE)
  }
  list(content = readBin(x, "raw", file.size(x)), file = x)
}

read_level <- function(root, ctx) {
  ns <- zuxml::xml_ns(root)
  if (zuxml::xml_local(root) != "sbml" || is.na(ns)) {
    abort_parse(
      "Root element is not <sbml> in an SBML namespace",
      path = zuxml::xml_local(root),
      section = "4.1",
      file = ctx$file
    )
  }
  if (ns %in% sbml_ns_other) {
    level <- names(sbml_ns_other)[match(ns, sbml_ns_other)]
    abort_unsupported(
      paste0(
        "SBML Level ",
        sub("\\..*$", "", level),
        " documents are not supported; zusbml reads Level 3."
      ),
      construct = paste0("SBML Level ", level)
    )
  }
  if (!ns %in% sbml_ns_core) {
    abort_parse(
      paste0("Unknown SBML namespace '", ns, "'"),
      path = "sbml",
      section = "4.1",
      file = ctx$file
    )
  }
  level <- names(sbml_ns_core)[match(ns, sbml_ns_core)]
  ctx$ns <- ns
  ctx$level <- 3L
  ctx$version <- as.integer(sub("^3\\.", "", level))

  # Every Level 3 package a document uses declares `prefix:required` on
  # <sbml>, so they can all be named at once.
  attrs <- zuxml::xml_attrs(root)[[1]]
  packages <- sub(
    ":.*$",
    "",
    grep("^[^:]+:required$", names(attrs), value = TRUE)
  )
  note_packages(packages, "sbml", ctx)
}

check_level_attributes <- function(sbml, ctx) {
  if (
    sbml$attributes$level != ctx$level ||
      sbml$attributes$version != ctx$version
  ) {
    abort_parse(
      "The level and version attributes disagree with the namespace",
      path = "sbml",
      attribute = "level",
      section = "4.1",
      file = ctx$file
    )
  }
}

note_packages <- function(packages, path, ctx) {
  packages <- unique(packages)
  if (length(packages) == 0) {
    return(invisible())
  }
  if (!ctx$ignore_packages) {
    abort_unsupported(
      paste0(
        "SBML Level 3 package",
        if (length(packages) > 1) "s",
        " ",
        paste0("'", packages, "'", collapse = ", "),
        " not supported (at ",
        path,
        "). ",
        "Use `ignore_packages = TRUE` to read the core model only."
      ),
      construct = "SBML package",
      package = packages
    )
  }
  ctx$ignored <- union(ctx$ignored, packages)
}

package_from_ns <- function(ns) {
  name <- sub("^.*/level3/version[0-9]+/([^/]+)/.*$", "\\1", ns)
  ifelse(name == ns, ns, name)
}

# One element, checked against its specification. Returns its typed
# attributes, its SBase fields, and its children: the R expression for
# <math> (wrapped in a list, since it may be NULL), serialized text for
# <message>, a list for listOf containers, and a parsed
# element for single children such as <kineticLaw>.
read_element <- function(node, kind, path, ctx) {
  spec <- element_spec(kind, ctx$version)
  attributes <- read_attributes(node, spec, path, ctx)

  out <- list(
    kind = kind,
    path = path,
    attributes = attributes,
    notes = NA_character_,
    annotation = NA_character_,
    children = list()
  )

  kids <- zuxml::xml_elements(node)
  locals <- zuxml::xml_local(kids)
  namespaces <- zuxml::xml_ns(kids)
  for (i in seq_along(locals)) {
    local <- locals[[i]]
    ns <- namespaces[[i]]
    child_path <- paste0(path, "/", local)
    if (
      !identical(ns, ctx$ns) && !(local == "math" && identical(ns, mathml_ns))
    ) {
      if (is.na(ns)) {
        abort_parse(
          paste0("Element <", local, "> is in no namespace"),
          path = child_path,
          section = spec$section,
          file = ctx$file
        )
      }
      note_packages(package_from_ns(ns), child_path, ctx)
      next
    }
    if (local %in% c("notes", "annotation")) {
      out[[local]] <- zuxml::xml_serialize(kids[[i]])
      next
    }
    if (local == "math" && !identical(ns, mathml_ns)) {
      abort_parse(
        "<math> is not in the MathML namespace",
        path = child_path,
        section = "3.4",
        file = ctx$file
      )
    }
    if (!local %in% spec$children) {
      abort_parse(
        paste0("Unknown element <", local, "> in <", kind, ">"),
        path = child_path,
        section = spec$section,
        file = ctx$file
      )
    }
    if (!is.null(out$children[[local]])) {
      abort_parse(
        paste0("Element <", local, "> appears more than once"),
        path = child_path,
        section = spec$section,
        file = ctx$file
      )
    }
    out$children[[local]] <- read_child(kids[[i]], local, child_path, ctx)
  }
  out
}

read_child <- function(node, local, path, ctx) {
  if (local == "math") {
    return(list(mathml_to_r(node, path, ctx$file)))
  }
  if (local == "message") {
    return(zuxml::xml_serialize(node))
  }
  if (local %in% names(sbml_lists)) {
    return(read_list(node, local, path, ctx))
  }
  read_element(node, local, path, ctx)
}

read_list <- function(node, list_kind, path, ctx) {
  allowed <- sbml_lists[[list_kind]]
  read_attributes(
    node,
    list(attributes = sbase_attributes, section = "3.1"),
    path,
    ctx
  )
  kids <- zuxml::xml_elements(node)
  locals <- zuxml::xml_local(kids)
  namespaces <- zuxml::xml_ns(kids)
  items <- list()
  counts <- list()
  for (i in seq_along(locals)) {
    local <- locals[[i]]
    ns <- namespaces[[i]]
    if (!identical(ns, ctx$ns)) {
      note_packages(package_from_ns(ns), paste0(path, "/", local), ctx)
      next
    }
    if (local %in% c("notes", "annotation")) {
      next
    }
    counts[[local]] <- (counts[[local]] %||% 0L) + 1L
    item_path <- paste0(path, "/", local, "[", counts[[local]], "]")
    if (!local %in% allowed) {
      abort_parse(
        paste0("Unknown element <", local, "> in <", list_kind, ">"),
        path = item_path,
        section = "3.1",
        file = ctx$file
      )
    }
    items[[length(items) + 1]] <- read_element(kids[[i]], local, item_path, ctx)
  }
  items
}

read_attributes <- function(node, spec, path, ctx) {
  raw <- zuxml::xml_attrs(node)[[1]]
  names_raw <- names(raw) %||% character(0)
  prefixed <- grepl(":", names_raw, fixed = TRUE)
  xml_reserved <- startsWith(names_raw, "xml:") | startsWith(names_raw, "xmlns")
  packages <- sub(":.*$", "", names_raw[prefixed & !xml_reserved])
  if (path != "sbml") {
    note_packages(packages, path, ctx)
  }
  raw <- raw[!prefixed]

  unknown <- setdiff(names(raw), names(spec$attributes))
  if (length(unknown) > 0) {
    abort_parse(
      paste0("Unknown attribute '", unknown[[1]], "'"),
      path = path,
      attribute = unknown[[1]],
      section = spec$section,
      file = ctx$file
    )
  }

  values <- list()
  for (name in names(spec$attributes)) {
    type <- spec$attributes[[name]][[1]]
    required <- spec$attributes[[name]][[2]]
    value <- raw[name]
    if (is.na(value)) {
      if (required) {
        abort_parse(
          "Missing required attribute",
          path = path,
          attribute = name,
          section = spec$section,
          file = ctx$file
        )
      }
      values[[name]] <- na_of(type)
      next
    }
    values[[name]] <- parse_value(value, type, path, name, spec$section, ctx)
  }
  values
}

na_of <- function(type) {
  switch(
    type,
    string = NA_character_,
    double = NA_real_,
    integer = NA_integer_,
    boolean = NA
  )
}

parse_value <- function(value, type, path, name, section, ctx) {
  value <- unname(value)
  trimmed <- trimws(value)
  parsed <- switch(
    type,
    string = value,
    double = parse_double(trimmed),
    integer = if (grepl("^[+-]?[0-9]+$", trimmed)) {
      suppressWarnings(as.integer(trimmed))
    } else {
      NA_integer_
    },
    boolean = switch(
      trimmed,
      "true" = ,
      "1" = TRUE,
      "false" = ,
      "0" = FALSE,
      NA
    )
  )
  if (type != "string" && is.na(parsed) && !identical(trimmed, "NaN")) {
    abort_parse(
      paste0("Invalid ", type, " value '", value, "'"),
      path = path,
      attribute = name,
      section = section,
      file = ctx$file
    )
  }
  parsed
}

# XML Schema doubles: decimal or scientific notation, INF, -INF and NaN.
parse_double <- function(x) {
  switch(
    x,
    "INF" = ,
    "+INF" = Inf,
    "-INF" = -Inf,
    "NaN" = NaN,
    if (grepl("^[+-]?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)([eE][+-]?[0-9]+)?$", x)) {
      as.numeric(x)
    } else {
      NA_real_
    }
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x
