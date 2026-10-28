#' Write a model as SBML
#'
#' Writes an `sbml_model` as an SBML Level 3 Version 2 core document (design
#' D9), whatever level and version it was read from. Attributes that are `NA`
#' are left out; `notes`, `annotation` and constraint messages are written as
#' they were read; math is written as MathML. Reading the result gives back
#' an identical model for any Level 3 Version 2 input.
#'
#' `zuxml` cannot build nodes, so the document is written as text (design
#' D14), escaping attribute values and names.
#'
#' @inheritParams sbml_odes
#' @param path A file to write, or `NULL` to return the document as a
#'   string.
#' @return The path, invisibly, or the document as a string when `path` is
#'   `NULL`.
#' @section Errors:
#' `zusbml_write_error` for math with no MathML equivalent, naming the R
#' call.
#' @export
#' @examples
#' m <- sbml_read(system.file("extdata", "decay.xml", package = "zusbml"))
#' path <- tempfile(fileext = ".xml")
#' sbml_write(m, path)
#' identical(sbml_read(path), m)
sbml_write <- function(m, path = NULL) {
  check_model(m)
  ids <- model_ids(m)
  text <- paste0(
    '<?xml version="1.0" encoding="UTF-8"?>\n',
    element(
      "sbml",
      c(
        xmlns = sbml_ns_core[["3.2"]],
        level = "3",
        version = "2",
        metaid = m$sbml$metaid,
        sboTerm = m$sbml$sbo_term
      ),
      sbase_children(m$sbml),
      write_model(m, ids)
    ),
    "\n"
  )
  if (is.null(path)) {
    return(text)
  }
  con <- file(path, open = "wb")
  on.exit(close(con))
  writeBin(charToRaw(enc2utf8(text)), con)
  invisible(path)
}

# ---- Text building ---------------------------------------------------------

# Attribute values escaped for a double-quoted attribute. Tab, newline and
# carriage return become character references, which attribute value
# normalisation would otherwise turn into spaces.
attr_escape <- function(x) {
  x <- xml_escape(x)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x <- gsub("\t", "&#9;", x)
  x <- gsub("\n", "&#10;", x)
  gsub("\r", "&#13;", x)
}

# Attribute values as text: NA is left out, logicals are true/false,
# doubles use the shortest text that reads back the same.
attr_text <- function(x) {
  if (length(x) == 0 || is.null(x) || is.na(x)) {
    if (!(is.double(x) && length(x) == 1 && is.nan(x))) {
      return(NA_character_)
    }
  }
  if (is.logical(x)) {
    return(if (x) "true" else "false")
  }
  if (is.integer(x)) {
    return(as.character(x))
  }
  if (is.double(x)) {
    if (is.nan(x)) {
      return("NaN")
    }
    if (is.infinite(x)) {
      return(if (x > 0) "INF" else "-INF")
    }
    return(format_double(x))
  }
  as.character(x)
}

element <- function(tag, attrs = character(0), ...) {
  attrs <- vapply(as.list(attrs), attr_text, character(1))
  attrs <- attrs[!is.na(attrs)]
  open <- paste0(
    "<",
    tag,
    paste0(" ", names(attrs), '="', attr_escape(attrs), '"', collapse = "")
  )
  if (length(attrs) == 0) {
    open <- paste0("<", tag)
  }
  body <- paste(unlist(list(...)), collapse = "")
  if (!nzchar(body)) {
    return(paste0(open, "/>"))
  }
  paste0(open, ">", body, "</", tag, ">")
}

list_of <- function(tag, items) {
  if (length(items) == 0) "" else element(tag, character(0), items)
}

sbase_attrs <- function(row) {
  c(metaid = row$metaid, sboTerm = row$sbo_term)
}

# notes and annotation are stored without their own tag (xml_content()).
sbase_children <- function(row) {
  c(
    wrap_content("notes", row$notes),
    wrap_content("annotation", row$annotation)
  )
}

# XML content kept as text (notes, annotation, a constraint's message),
# wrapped in its tag. It may have been edited, so it is checked to be a
# well-formed fragment before it goes into the document.
wrap_content <- function(tag, x) {
  if (is.null(x) || is.na(x)) {
    return(NULL)
  }
  well_formed <- tryCatch(
    {
      zuxml::xml_parse(paste0("<x>", x, "</x>"))
      TRUE
    },
    zuxml_error = function(cnd) FALSE
  )
  if (!well_formed) {
    abort_write(
      paste0("The ", tag, " text is not well-formed XML."),
      tag
    )
  }
  paste0("<", tag, ">", x, "</", tag, ">")
}

math_child <- function(expr, ids) {
  if (is.null(expr)) "" else r_to_mathml(expr, ids)
}

rows <- function(df) {
  lapply(seq_len(nrow(df)), function(i) {
    lapply(df, function(col) if (is.list(col)) col[[i]] else col[[i]])
  })
}

# ---- Elements --------------------------------------------------------------

write_model <- function(m, ids) {
  attrs <- c(
    id = m$id,
    name = m$name,
    metaid = m$metaid,
    sboTerm = m$sbo_term,
    substanceUnits = m$substance_units,
    timeUnits = m$time_units,
    volumeUnits = m$volume_units,
    areaUnits = m$area_units,
    lengthUnits = m$length_units,
    extentUnits = m$extent_units,
    conversionFactor = m$conversion_factor
  )
  element(
    "model",
    attrs,
    sbase_children(m),
    list_of(
      "listOfFunctionDefinitions",
      lapply(rows(m$functions), function(r) {
        element(
          "functionDefinition",
          c(id = r$id, name = r$name, sbase_attrs(r)),
          sbase_children(r),
          math_child(r$math, ids)
        )
      })
    ),
    list_of(
      "listOfUnitDefinitions",
      lapply(rows(m$unit_definitions), function(r) {
        units <- lapply(rows(r$units), function(u) {
          element(
            "unit",
            list(
              kind = u$kind,
              exponent = u$exponent,
              scale = u$scale,
              multiplier = u$multiplier,
              metaid = u$metaid,
              sboTerm = u$sbo_term
            ),
            sbase_children(u)
          )
        })
        element(
          "unitDefinition",
          c(id = r$id, name = r$name, sbase_attrs(r)),
          sbase_children(r),
          list_of("listOfUnits", units)
        )
      })
    ),
    list_of(
      "listOfCompartments",
      lapply(rows(m$compartments), function(r) {
        element(
          "compartment",
          list(
            id = r$id,
            name = r$name,
            metaid = r$metaid,
            sboTerm = r$sbo_term,
            spatialDimensions = r$spatial_dimensions,
            size = r$size,
            units = r$units,
            constant = r$constant
          ),
          sbase_children(r)
        )
      })
    ),
    list_of(
      "listOfSpecies",
      lapply(rows(m$species), function(r) {
        element(
          "species",
          list(
            id = r$id,
            name = r$name,
            metaid = r$metaid,
            sboTerm = r$sbo_term,
            compartment = r$compartment,
            initialAmount = r$initial_amount,
            initialConcentration = r$initial_concentration,
            substanceUnits = r$substance_units,
            hasOnlySubstanceUnits = r$has_only_substance_units,
            boundaryCondition = r$boundary_condition,
            constant = r$constant,
            conversionFactor = r$conversion_factor
          ),
          sbase_children(r)
        )
      })
    ),
    list_of(
      "listOfParameters",
      lapply(rows(m$parameters), function(r) {
        element(
          "parameter",
          list(
            id = r$id,
            name = r$name,
            metaid = r$metaid,
            sboTerm = r$sbo_term,
            value = r$value,
            units = r$units,
            constant = r$constant
          ),
          sbase_children(r)
        )
      })
    ),
    list_of(
      "listOfInitialAssignments",
      lapply(rows(m$initial_assignments), function(r) {
        element(
          "initialAssignment",
          list(
            symbol = r$symbol,
            id = r$id,
            name = r$name,
            metaid = r$metaid,
            sboTerm = r$sbo_term
          ),
          sbase_children(r),
          math_child(r$math, ids)
        )
      })
    ),
    list_of(
      "listOfRules",
      lapply(rows(m$rules), function(r) {
        element(
          paste0(r$type, "Rule"),
          list(
            variable = r$variable,
            id = r$id,
            name = r$name,
            metaid = r$metaid,
            sboTerm = r$sbo_term
          ),
          sbase_children(r),
          math_child(r$math, ids)
        )
      })
    ),
    list_of(
      "listOfConstraints",
      lapply(rows(m$constraints), function(r) {
        element(
          "constraint",
          list(
            id = r$id,
            name = r$name,
            metaid = r$metaid,
            sboTerm = r$sbo_term
          ),
          sbase_children(r),
          math_child(r$math, ids),
          wrap_content("message", r$message)
        )
      })
    ),
    list_of(
      "listOfReactions",
      lapply(rows(m$reactions), write_reaction, ids = ids)
    ),
    list_of("listOfEvents", lapply(rows(m$events), write_event, ids = ids))
  )
}

write_species_references <- function(tag, list_tag, refs) {
  list_of(
    list_tag,
    lapply(rows(refs), function(r) {
      attrs <- list(
        id = r$id,
        name = r$name,
        metaid = r$metaid,
        sboTerm = r$sbo_term,
        species = r$species
      )
      if (tag == "speciesReference") {
        attrs$stoichiometry <- r$stoichiometry
        attrs$constant <- r$constant
      }
      element(tag, attrs, sbase_children(r))
    })
  )
}

write_reaction <- function(r, ids) {
  law <- r$kinetic_law
  law_text <- if (!is.null(law)) {
    locals <- lapply(rows(law$local_parameters), function(p) {
      element(
        "localParameter",
        list(
          id = p$id,
          name = p$name,
          metaid = p$metaid,
          sboTerm = p$sbo_term,
          value = p$value,
          units = p$units
        ),
        sbase_children(p)
      )
    })
    element(
      "kineticLaw",
      sbase_attrs(law),
      sbase_children(law),
      math_child(law$math, ids),
      list_of("listOfLocalParameters", locals)
    )
  }
  element(
    "reaction",
    list(
      id = r$id,
      name = r$name,
      metaid = r$metaid,
      sboTerm = r$sbo_term,
      reversible = r$reversible,
      compartment = r$compartment
    ),
    sbase_children(r),
    write_species_references(
      "speciesReference",
      "listOfReactants",
      r$reactants
    ),
    write_species_references("speciesReference", "listOfProducts", r$products),
    write_species_references(
      "modifierSpeciesReference",
      "listOfModifiers",
      r$modifiers
    ),
    law_text
  )
}

write_event <- function(r, ids) {
  part <- function(tag, p, attrs = list()) {
    if (is.null(p)) {
      return("")
    }
    element(
      tag,
      c(attrs, sbase_attrs(p)),
      sbase_children(p),
      math_child(p$math, ids)
    )
  }
  trigger <- r$trigger
  assignments <- lapply(rows(r$assignments), function(a) {
    element(
      "eventAssignment",
      list(
        variable = a$variable,
        id = a$id,
        name = a$name,
        metaid = a$metaid,
        sboTerm = a$sbo_term
      ),
      sbase_children(a),
      math_child(a$math, ids)
    )
  })
  element(
    "event",
    list(
      id = r$id,
      name = r$name,
      metaid = r$metaid,
      sboTerm = r$sbo_term,
      useValuesFromTriggerTime = r$use_values_from_trigger_time
    ),
    sbase_children(r),
    part(
      "trigger",
      trigger,
      list(
        initialValue = trigger$initial_value,
        persistent = trigger$persistent
      )
    ),
    part("priority", r$priority),
    part("delay", r$delay),
    list_of("listOfEventAssignments", assignments)
  )
}

#' Write a model as JSON
#'
#' Writes the `sbml_model` object as JSON with the zujson package, for use
#' without XML: one array of records per element kind, math as R code (as
#' [sbml_math()] shows it), notes and annotation as XML text.
#'
#' @inheritParams sbml_write
#' @return The path, invisibly, or the JSON as a string when `path` is `NULL`.
#' @export
#' @examplesIf requireNamespace("zujson", quietly = TRUE)
#' m <- sbml_read(system.file("extdata", "decay.xml", package = "zusbml"))
#' cat(substr(sbml_write_json(m), 1, 200))
sbml_write_json <- function(m, path = NULL) {
  check_model(m)
  if (!requireNamespace("zujson", quietly = TRUE)) {
    stop("sbml_write_json() needs the zujson package.", call. = FALSE)
  }
  plain <- function(x) {
    if (is.language(x) || is.function(x)) {
      return(sbml_math(x))
    }
    if (is.data.frame(x)) {
      x[] <- lapply(x, function(col) {
        if (is.list(col)) lapply(col, plain) else col
      })
      return(x)
    }
    if (is.list(x)) {
      return(lapply(x, plain))
    }
    x
  }
  text <- zujson::json_write(plain(unclass(m)), pretty = TRUE)
  if (is.null(path)) {
    return(text)
  }
  writeLines(text, path, useBytes = TRUE)
  invisible(path)
}
