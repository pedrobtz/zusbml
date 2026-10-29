#' Read an SBML document
#'
#' Reads an SBML core document into an `sbml_model`: Level 3 Versions 1 and
#' 2, and Level 2 Versions 4 and 5, which are mapped to their Level 3
#' meaning. Every element carries the attributes Level 3 defines; optional
#' values without a default read as `NA`.
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
#' * `zusbml_unsupported` for SBML packages, Level 1, and Level 2 before
#'   Version 4.
#' * `zusbml_arg_error` for an argument of the wrong type, a missing file or
#'   a directory.
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
    abort_arg("`ignore_packages` must be TRUE or FALSE.", "ignore_packages")
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
  ctx$dropped <- character(0)

  root <- zuxml::xml_root(doc)
  read_level(root, ctx)
  # The whole document as flat vectors, read in a few vectorized calls; the
  # reader walks it by index (see flat_tree()).
  ctx$t <- flat_tree(root)
  declared <- declared_ids(ctx$t, ctx$ns)
  ctx$ids <- declared$ids
  ctx$functions <- function_ids(declared$functions)
  # Every SId-valued attribute in the document, checked in one pass.
  values <- unlist(lapply(ctx$t$attrs, function(a) {
    a[names(a) %in% sid_attributes]
  }))
  ctx$bad_sids <- unique(values[!is_sid(values)])
  sbml <- read_element(1L, "sbml", "sbml", ctx)
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

# The ids of the document's SBML elements, so math can tell the time csymbol
# and <pi/> from model symbols of those names (design D16), and the ids of
# the model's function definitions. Notes, annotation and elements of other
# namespaces are not walked: an XHTML id="time" in <notes> declares nothing.
# One pass per depth over the flat tree.
declared_ids <- function(t, ns) {
  id_of <- function(i) {
    unlist(lapply(t$attrs[i], function(a) a[names(a) == "id"]))
  }
  ids <- id_of(1L)
  functions <- character(0)
  level <- 1L
  while (length(level) > 0) {
    kids <- unlist(t$kids[level])
    kids <- kids[
      t$ns[kids] %in% ns & !t$local[kids] %in% c("notes", "annotation")
    ]
    ids <- c(ids, id_of(kids))
    lists <- level[t$local[level] == "listOfFunctionDefinitions"]
    defs <- unlist(t$kids[lists])
    defs <- defs[t$ns[defs] %in% ns & t$local[defs] == "functionDefinition"]
    functions <- c(functions, id_of(defs))
    level <- kids
  }
  list(ids = unique(ids), functions = functions)
}

# The ids of the document's function definitions are the only names math
# may call (design D20). One that is also a built-in the math maps to, such
# as `exp`, cannot be kept apart from it in R and is refused.
function_ids <- function(ids) {
  builtin <- c(math_base_names, ls(math_env), "pi", "rateOf", "delay")
  clash <- intersect(ids, builtin)
  if (length(clash) > 0) {
    abort_unsupported(
      paste0(
        "Function definition '",
        clash[[1]],
        "' has the name of a built-in ",
        "function, which zusbml cannot tell apart from it."
      ),
      construct = "functionDefinition",
      id = clash[[1]],
      section = "4.3"
    )
  }
  ids
}

# What an element holds, as XML text: notes, annotation and a constraint's
# message are kept without their own tag, whose namespace is the source
# document's, and the writer wraps them in the tag of the document it
# writes (design D14). Each child carries the namespaces it uses.
xml_content <- function(node) {
  paste(zuxml::xml_serialize(zuxml::xml_children(node)), collapse = "")
}

read_input <- function(x) {
  if (is.raw(x)) {
    return(list(content = x, file = NA_character_))
  }
  if (!is.character(x) || length(x) != 1 || is.na(x)) {
    abort_arg("`x` must be a path, a URL, XML text or a raw vector.", "x")
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
    abort_arg(paste0("File '", x, "' does not exist."), "x")
  }
  if (dir.exists(x)) {
    abort_arg(paste0("'", x, "' is a directory, not a file."), "x")
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
  readable_l2 <- sbml_ns_other[c("2.4", "2.5")]
  if (ns %in% readable_l2) {
    level <- names(readable_l2)[match(ns, readable_l2)]
    ctx$ns <- ns
    ctx$level <- 2L
    ctx$version <- as.integer(sub("^2\\.", "", level))
    return(invisible())
  }
  if (ns %in% sbml_ns_other) {
    level <- names(sbml_ns_other)[match(ns, sbml_ns_other)]
    abort_unsupported(
      paste0(
        "SBML Level ",
        sub("\\.", " Version ", level),
        " documents are not supported; zusbml reads Level 3 and Level 2 ",
        "Versions 4 and 5."
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

# Element i of the document's flat tree (ctx$t), checked against its
# specification. Returns its typed attributes, its SBase fields, and its
# children: the R expression for <math> (wrapped in a list, since it may be
# NULL), XML text for <message>, a list for listOf containers, and a parsed
# element for single children such as <kineticLaw>.
read_element <- function(i, kind, path, ctx) {
  t <- ctx$t
  spec <- element_spec(kind, ctx$version, ctx$level)
  attributes <- read_attributes(NULL, spec, path, ctx, t$attrs[[i]])

  out <- list(
    kind = kind,
    path = path,
    attributes = attributes,
    notes = NA_character_,
    annotation = NA_character_,
    children = list()
  )

  for (k in t$kids[[i]]) {
    local <- t$local[[k]]
    ns <- t$ns[[k]]
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
      out[[local]] <- xml_content(t$nodes[[k]])
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
    out$children[[local]] <- read_child(k, local, child_path, ctx)
  }
  out
}

read_child <- function(k, local, path, ctx) {
  if (local == "math") {
    return(list(math_from_tree(
      ctx$t,
      k,
      path,
      ctx$file,
      ctx$ids,
      ctx$functions
    )))
  }
  if (local == "message") {
    return(xml_content(ctx$t$nodes[[k]]))
  }
  if (local %in% c(names(sbml_lists), names(level2_lists))) {
    return(read_list(k, local, path, ctx))
  }
  read_element(k, local, path, ctx)
}

read_list <- function(i, list_kind, path, ctx) {
  t <- ctx$t
  allowed <- sbml_lists[[list_kind]] %||% level2_lists[[list_kind]]
  read_attributes(
    NULL,
    list(attributes = sbase_attributes, section = "3.1"),
    path,
    ctx,
    t$attrs[[i]]
  )
  kids <- t$kids[[i]]
  items <- vector("list", length(kids))
  counts <- list()
  for (n in seq_along(kids)) {
    k <- kids[[n]]
    local <- t$local[[k]]
    ns <- t$ns[[k]]
    if (!identical(ns, ctx$ns)) {
      if (is.na(ns)) {
        abort_parse(
          paste0("Element <", local, "> is in no namespace"),
          path = paste0(path, "/", local),
          section = "3.1",
          file = ctx$file
        )
      }
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
    items[[n]] <- read_element(k, local, item_path, ctx)
  }
  items[!vapply(items, is.null, logical(1))]
}

# `raw` is the element's attributes when the caller already has them: a list
# reads all its items' attributes in one call.
read_attributes <- function(node, spec, path, ctx, raw = NULL) {
  if (is.null(raw)) {
    raw <- zuxml::xml_attrs(node)[[1]]
  }
  names_raw <- names(raw) %||% character(0)
  prefixed <- grepl(":", names_raw, fixed = TRUE)
  if (any(prefixed)) {
    xml_reserved <- startsWith(names_raw, "xml:") |
      startsWith(names_raw, "xmlns")
    packages <- sub(":.*$", "", names_raw[prefixed & !xml_reserved])
    if (path != "sbml") {
      note_packages(packages, path, ctx)
    }
    raw <- raw[!prefixed]
    names_raw <- names_raw[!prefixed]
  }
  if (ctx$level == 2L) {
    ctx$dropped <- union(ctx$dropped, intersect(names_raw, level2_dropped))
  }

  attrs <- spec$attributes
  unknown <- names_raw[!names_raw %in% names(attrs)]
  if (length(unknown) > 0) {
    abort_parse(
      paste0("Unknown attribute '", unknown[[1]], "'"),
      path = path,
      attribute = unknown[[1]],
      section = spec$section,
      file = ctx$file
    )
  }

  at <- match(names(attrs), names_raw)
  values <- vector("list", length(attrs))
  names(values) <- names(attrs)
  for (k in seq_along(attrs)) {
    a <- attrs[[k]]
    name <- names(attrs)[[k]]
    if (is.na(at[[k]])) {
      if (a[[2]]) {
        abort_parse(
          "Missing required attribute",
          path = path,
          attribute = name,
          section = spec$section,
          file = ctx$file
        )
      }
      values[k] <- list(a[3][[1]] %||% na_of(a[[1]]))
      next
    }
    values[[k]] <- parse_value(
      raw[[at[[k]]]],
      a[[1]],
      path,
      name,
      spec$section,
      ctx
    )
  }
  values
}

# trimws() costs a regular expression per call; most values have no
# surrounding whitespace, so look at the first and last character first.
fast_trim <- function(x) {
  space <- c(" ", "\t", "\n", "\r")
  n <- nchar(x)
  padded <- substr(x, 1, 1) %in% space | substr(x, n, n) %in% space
  if (any(padded)) {
    x[padded] <- trimws(x[padded])
  }
  x
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
  trimmed <- if (type == "string") value else fast_trim(value)
  if (type == "string" && name %in% sid_attributes && !known_sid(value, ctx)) {
    abort_parse(
      paste0("'", value, "' is not a valid SBML identifier"),
      path = path,
      attribute = name,
      section = "3.1.7",
      file = ctx$file
    )
  }
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

# Attributes whose value is an SId or UnitSId, and SBML's syntax for them
# (section 3.1.7). Derivation relies on it: internal names start with ".",
# which no SId can (design D16).
sid_attributes <- c(
  "id",
  "variable",
  "symbol",
  "species",
  "compartment",
  "conversionFactor",
  "units",
  "substanceUnits",
  "timeUnits",
  "volumeUnits",
  "areaUnits",
  "lengthUnits",
  "extentUnits",
  "outside",
  "compartmentType",
  "speciesType"
)

# sbml_read() checks every SId-valued attribute of the document at once and
# keeps the invalid values.
known_sid <- function(value, ctx) {
  !value %in% ctx$bad_sids
}

# \\z, not $: PCRE's $ also matches before a final newline.
is_sid <- function(x) grepl("^[A-Za-z_][A-Za-z0-9_]*\\z", x, perl = TRUE)

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
