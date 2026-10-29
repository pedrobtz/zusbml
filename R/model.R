# The sbml_model object (design section 4.3): one data frame per element
# kind, scalar attributes as columns in snake_case, and list columns for math
# and for nested parts (species references, kinetic laws, triggers). Math is
# R language objects (design D6; see R/mathml.R). Every
# element also keeps its SBase fields: metaid, sbo_term, and notes and
# annotation as serialized XML text, so a re-read model is identical().

new_sbml_model <- function(sbml, ctx) {
  model <- sbml$children$model
  if (ctx$level == 2L) {
    model <- convert_level2(model, ctx)
  }
  a <- model$attributes
  lists <- model$children
  structure(
    list(
      level = ctx$level,
      version = ctx$version,
      id = a$id,
      name = a$name,
      substance_units = a$substanceUnits,
      time_units = a$timeUnits,
      volume_units = a$volumeUnits,
      area_units = a$areaUnits,
      length_units = a$lengthUnits,
      extent_units = a$extentUnits,
      conversion_factor = a$conversionFactor,
      metaid = a$metaid,
      sbo_term = a$sboTerm,
      notes = model$notes,
      annotation = model$annotation,
      sbml = list(
        metaid = sbml$attributes$metaid,
        sbo_term = sbml$attributes$sboTerm,
        notes = sbml$notes,
        annotation = sbml$annotation
      ),
      functions = function_table(lists$listOfFunctionDefinitions),
      unit_definitions = unit_definition_table(lists$listOfUnitDefinitions),
      compartments = compartment_table(lists$listOfCompartments),
      species = species_table(lists$listOfSpecies),
      parameters = parameter_table(lists$listOfParameters),
      initial_assignments = initial_assignment_table(
        lists$listOfInitialAssignments
      ),
      rules = rule_table(lists$listOfRules),
      constraints = constraint_table(lists$listOfConstraints),
      reactions = reaction_table(lists$listOfReactions),
      events = event_table(lists$listOfEvents),
      ignored_packages = ctx$ignored,
      dropped = ctx$dropped
    ),
    class = "sbml_model"
  )
}

# A data frame from a list of rows, one column per entry of `columns`: an
# atomic prototype gives a typed column (and the right type with no rows),
# list() gives a list column.
table_of <- function(rows, columns) {
  out <- lapply(names(columns), function(name) {
    proto <- columns[[name]]
    values <- lapply(rows, `[[`, name)
    if (is.list(proto)) {
      return(values)
    }
    missing <- vapply(values, is.null, logical(1))
    values[missing] <- list(proto[NA_integer_])
    unlist(values, use.names = FALSE) %||% proto
  })
  names(out) <- names(columns)
  # Built directly: data.frame() and column assignment cost more than the
  # rest of reading for the many small tables (a reaction's reactants).
  structure(out, class = "data.frame", row.names = .set_row_names(length(rows)))
}

sbase_columns <- list(
  metaid = character(),
  sbo_term = character(),
  notes = character(),
  annotation = character()
)

sbase_row <- function(el) {
  list(
    metaid = el$attributes$metaid,
    sbo_term = el$attributes$sboTerm,
    notes = el$notes,
    annotation = el$annotation
  )
}

is_lambda <- function(x) {
  is.call(x) && identical(x[[1]], as.name("function"))
}

# An element's math as an R expression, or NULL when it has none.
math_of <- function(el) {
  wrapped <- el$children$math
  if (is.null(wrapped)) NULL else wrapped[[1]]
}

# A nested single element (kineticLaw, trigger, priority, delay) as a list,
# or NULL when absent.
part <- function(el, ...) {
  if (is.null(el)) {
    return(NULL)
  }
  c(list(...), list(math = math_of(el)), sbase_row(el))
}

function_table <- function(items) {
  rows <- lapply(items, function(el) {
    math <- math_of(el)
    if (!is.null(math) && !is_lambda(math)) {
      abort_parse(
        "A function definition's math must be a <lambda>",
        path = el$path,
        section = "4.3"
      )
    }
    c(
      list(
        id = el$attributes$id,
        name = el$attributes$name,
        math = math_of(el)
      ),
      sbase_row(el)
    )
  })
  table_of(
    rows,
    c(list(id = character(), name = character(), math = list()), sbase_columns)
  )
}

unit_definition_table <- function(items) {
  rows <- lapply(items, function(el) {
    units <- lapply(el$children$listOfUnits, function(u) {
      c(
        list(
          kind = u$attributes$kind,
          exponent = u$attributes$exponent,
          scale = u$attributes$scale,
          multiplier = u$attributes$multiplier
        ),
        sbase_row(u)
      )
    })
    c(
      list(
        id = el$attributes$id,
        name = el$attributes$name,
        units = list(table_of(
          units,
          c(
            list(
              kind = character(),
              exponent = double(),
              scale = integer(),
              multiplier = double()
            ),
            sbase_columns
          )
        ))
      ),
      sbase_row(el)
    )
  })
  out <- table_of(
    rows,
    c(list(id = character(), name = character(), units = list()), sbase_columns)
  )
  out$units <- lapply(out$units, function(u) u[[1]])
  out
}

compartment_table <- function(items) {
  rows <- lapply(items, function(el) {
    a <- el$attributes
    c(
      list(
        id = a$id,
        name = a$name,
        spatial_dimensions = a$spatialDimensions,
        size = a$size,
        units = a$units,
        constant = a$constant
      ),
      sbase_row(el)
    )
  })
  table_of(
    rows,
    c(
      list(
        id = character(),
        name = character(),
        spatial_dimensions = double(),
        size = double(),
        units = character(),
        constant = logical()
      ),
      sbase_columns
    )
  )
}

species_table <- function(items) {
  rows <- lapply(items, function(el) {
    a <- el$attributes
    c(
      list(
        id = a$id,
        name = a$name,
        compartment = a$compartment,
        initial_amount = a$initialAmount,
        initial_concentration = a$initialConcentration,
        substance_units = a$substanceUnits,
        has_only_substance_units = a$hasOnlySubstanceUnits,
        boundary_condition = a$boundaryCondition,
        constant = a$constant,
        conversion_factor = a$conversionFactor
      ),
      sbase_row(el)
    )
  })
  table_of(
    rows,
    c(
      list(
        id = character(),
        name = character(),
        compartment = character(),
        initial_amount = double(),
        initial_concentration = double(),
        substance_units = character(),
        has_only_substance_units = logical(),
        boundary_condition = logical(),
        constant = logical(),
        conversion_factor = character()
      ),
      sbase_columns
    )
  )
}

parameter_table <- function(items) {
  rows <- lapply(items, function(el) {
    a <- el$attributes
    c(
      list(
        id = a$id,
        name = a$name,
        value = a$value,
        units = a$units,
        constant = a$constant
      ),
      sbase_row(el)
    )
  })
  table_of(
    rows,
    c(
      list(
        id = character(),
        name = character(),
        value = double(),
        units = character(),
        constant = logical()
      ),
      sbase_columns
    )
  )
}

initial_assignment_table <- function(items) {
  rows <- lapply(items, function(el) {
    a <- el$attributes
    c(
      list(
        symbol = a$symbol,
        id = a$id,
        name = a$name,
        math = math_of(el)
      ),
      sbase_row(el)
    )
  })
  table_of(
    rows,
    c(
      list(
        symbol = character(),
        id = character(),
        name = character(),
        math = list()
      ),
      sbase_columns
    )
  )
}

rule_table <- function(items) {
  rows <- lapply(items, function(el) {
    a <- el$attributes
    c(
      list(
        type = sub("Rule$", "", el$kind),
        variable = a$variable %||% NA_character_,
        id = a$id,
        name = a$name,
        math = math_of(el)
      ),
      sbase_row(el)
    )
  })
  table_of(
    rows,
    c(
      list(
        type = character(),
        variable = character(),
        id = character(),
        name = character(),
        math = list()
      ),
      sbase_columns
    )
  )
}

constraint_table <- function(items) {
  rows <- lapply(items, function(el) {
    a <- el$attributes
    c(
      list(
        id = a$id,
        name = a$name,
        math = math_of(el),
        message = el$children$message %||% NA_character_
      ),
      sbase_row(el)
    )
  })
  table_of(
    rows,
    c(
      list(
        id = character(),
        name = character(),
        math = list(),
        message = character()
      ),
      sbase_columns
    )
  )
}

species_reference_table <- function(items) {
  rows <- lapply(items, function(el) {
    a <- el$attributes
    c(
      list(
        id = a$id,
        name = a$name,
        species = a$species,
        stoichiometry = a$stoichiometry %||% NA_real_,
        constant = a$constant %||% NA
      ),
      sbase_row(el)
    )
  })
  table_of(
    rows,
    c(
      list(
        id = character(),
        name = character(),
        species = character(),
        stoichiometry = double(),
        constant = logical()
      ),
      sbase_columns
    )
  )
}

local_parameter_table <- function(items) {
  rows <- lapply(items, function(el) {
    a <- el$attributes
    c(
      list(id = a$id, name = a$name, value = a$value, units = a$units),
      sbase_row(el)
    )
  })
  table_of(
    rows,
    c(
      list(
        id = character(),
        name = character(),
        value = double(),
        units = character()
      ),
      sbase_columns
    )
  )
}

reaction_table <- function(items) {
  rows <- lapply(items, function(el) {
    a <- el$attributes
    k <- el$children
    law <- k$kineticLaw
    c(
      list(
        id = a$id,
        name = a$name,
        reversible = a$reversible,
        fast = a$fast %||% NA,
        compartment = a$compartment,
        reactants = species_reference_table(k$listOfReactants),
        products = species_reference_table(k$listOfProducts),
        modifiers = species_reference_table(k$listOfModifiers),
        kinetic_law = part(
          law,
          local_parameters = local_parameter_table(
            law$children$listOfLocalParameters
          )
        )
      ),
      sbase_row(el)
    )
  })
  wrap <- function(row) {
    for (col in c("reactants", "products", "modifiers", "kinetic_law")) {
      row[[col]] <- list(row[[col]])
    }
    row
  }
  out <- table_of(
    lapply(rows, wrap),
    c(
      list(
        id = character(),
        name = character(),
        reversible = logical(),
        fast = logical(),
        compartment = character(),
        reactants = list(),
        products = list(),
        modifiers = list(),
        kinetic_law = list()
      ),
      sbase_columns
    )
  )
  for (col in c("reactants", "products", "modifiers", "kinetic_law")) {
    out[[col]] <- lapply(out[[col]], function(x) x[[1]])
  }
  out
}

event_table <- function(items) {
  rows <- lapply(items, function(el) {
    a <- el$attributes
    k <- el$children
    assignments <- lapply(k$listOfEventAssignments, function(ea) {
      c(
        list(
          variable = ea$attributes$variable,
          id = ea$attributes$id,
          name = ea$attributes$name,
          math = math_of(ea)
        ),
        sbase_row(ea)
      )
    })
    trigger <- k$trigger
    c(
      list(
        id = a$id,
        name = a$name,
        use_values_from_trigger_time = a$useValuesFromTriggerTime,
        trigger = list(part(
          trigger,
          initial_value = trigger$attributes$initialValue,
          persistent = trigger$attributes$persistent
        )),
        priority = list(part(k$priority)),
        delay = list(part(k$delay)),
        assignments = list(table_of(
          assignments,
          c(
            list(
              variable = character(),
              id = character(),
              name = character(),
              math = list()
            ),
            sbase_columns
          )
        ))
      ),
      sbase_row(el)
    )
  })
  out <- table_of(
    rows,
    c(
      list(
        id = character(),
        name = character(),
        use_values_from_trigger_time = logical(),
        trigger = list(),
        priority = list(),
        delay = list(),
        assignments = list()
      ),
      sbase_columns
    )
  )
  for (col in c("trigger", "priority", "delay", "assignments")) {
    out[[col]] <- lapply(out[[col]], function(x) x[[1]])
  }
  out
}

#' @export
print.sbml_model <- function(x, ...) {
  label <- if (!is.na(x$id)) paste0(" '", x$id, "'") else ""
  cat(
    "<sbml_model",
    label,
    "> SBML Level ",
    x$level,
    " Version ",
    x$version,
    "\n",
    sep = ""
  )
  counts <- sbml_counts(x)
  shown <- counts[counts > 0]
  if (length(shown) == 0) {
    cat("  (empty model)\n")
  } else {
    width <- max(nchar(names(shown)))
    for (name in names(shown)) {
      cat(
        "  ",
        formatC(name, width = -width),
        "  ",
        shown[[name]],
        "\n",
        sep = ""
      )
    }
  }
  if (length(x$ignored_packages) > 0) {
    cat(
      "  ignored packages: ",
      paste(x$ignored_packages, collapse = ", "),
      "\n",
      sep = ""
    )
  }
  if (length(x$dropped) > 0) {
    cat(
      "  dropped from Level 2: ",
      paste(x$dropped, collapse = ", "),
      "\n",
      sep = ""
    )
  }
  invisible(x)
}

#' @export
summary.sbml_model <- function(object, ...) {
  structure(
    list(
      id = object$id,
      level = object$level,
      version = object$version,
      counts = sbml_counts(object),
      ignored_packages = object$ignored_packages
    ),
    class = "summary.sbml_model"
  )
}

#' @export
print.summary.sbml_model <- function(x, ...) {
  cat(
    "SBML Level ",
    x$level,
    " Version ",
    x$version,
    " model",
    if (!is.na(x$id)) paste0(" '", x$id, "'"),
    "\n",
    sep = ""
  )
  print(x$counts)
  invisible(x)
}

sbml_counts <- function(x) {
  kinds <- c(
    functions = "functions",
    unit_definitions = "unit_definitions",
    compartments = "compartments",
    species = "species",
    parameters = "parameters",
    initial_assignments = "initial_assignments",
    rules = "rules",
    constraints = "constraints",
    reactions = "reactions",
    events = "events"
  )
  vapply(kinds, function(k) nrow(x[[k]]), integer(1))
}

# Every id the model declares, for writing math with the same context it
# was read with (design D16).
model_ids <- function(m) {
  laws <- m$reactions$kinetic_law
  ids <- c(
    m$id,
    m$functions$id,
    m$unit_definitions$id,
    m$compartments$id,
    m$species$id,
    m$parameters$id,
    m$initial_assignments$id,
    m$rules$id,
    m$constraints$id,
    m$reactions$id,
    m$events$id,
    unlist(lapply(
      c(m$reactions$reactants, m$reactions$products, m$reactions$modifiers),
      function(r) r$id
    )),
    unlist(lapply(laws, function(k) k$local_parameters$id)),
    unlist(lapply(m$events$assignments, function(a) a$id))
  )
  unique(ids[!is.na(ids)])
}

check_model <- function(m) {
  if (!inherits(m, "sbml_model")) {
    abort_arg("`m` must be an sbml_model, as returned by sbml_read().", "m")
  }
}

#' Model components
#'
#' Each accessor returns one data frame of an `sbml_model`, one row per
#' element. Scalar attributes are columns, named in snake_case after the
#' SBML attribute (`initialAmount` is `initial_amount`); optional attributes
#' that are absent are `NA`. Math is a list column. Every table also has
#' `metaid`, `sbo_term`, and `notes` and `annotation` as XML text.
#'
#' `sbml_reactions()` holds the species references (`reactants`, `products`,
#' `modifiers`) as data frames and `kinetic_law` as a list with `math` and
#' `local_parameters`. `sbml_events()` holds `trigger`, `priority`, `delay`
#' and `assignments`. `sbml_rules()` has a `type` column: `"assignment"`,
#' `"rate"` or `"algebraic"`. `sbml_functions()` adds `fun`, each lambda
#' as an R function; see [sbml_math()] for how math is represented.
#'
#' @param m An `sbml_model`.
#' @return A data frame.
#' @name sbml_components
#' @examples
#' m <- sbml_read(system.file("extdata", "decay.xml", package = "zusbml"))
#' sbml_species(m)
#' sbml_reactions(m)$reactants[[1]]
NULL

#' @rdname sbml_components
#' @export
sbml_compartments <- function(m) {
  check_model(m)
  m$compartments
}

#' @rdname sbml_components
#' @export
sbml_species <- function(m) {
  check_model(m)
  m$species
}

#' @rdname sbml_components
#' @export
sbml_parameters <- function(m) {
  check_model(m)
  m$parameters
}

#' @rdname sbml_components
#' @export
sbml_reactions <- function(m) {
  check_model(m)
  m$reactions
}

#' @rdname sbml_components
#' @export
sbml_rules <- function(m) {
  check_model(m)
  m$rules
}

#' @rdname sbml_components
#' @export
sbml_events <- function(m) {
  check_model(m)
  m$events
}

#' @rdname sbml_components
#' @export
sbml_functions <- function(m) {
  check_model(m)
  out <- m$functions
  out$fun <- lapply(out$math, function(lambda) {
    if (is.call(lambda) && identical(lambda[[1]], as.name("function"))) {
      eval(lambda, math_env)
    }
  })
  out
}

#' @rdname sbml_components
#' @export
sbml_initial_assignments <- function(m) {
  check_model(m)
  m$initial_assignments
}

#' @rdname sbml_components
#' @export
sbml_constraints <- function(m) {
  check_model(m)
  m$constraints
}

#' @rdname sbml_components
#' @export
sbml_unit_definitions <- function(m) {
  check_model(m)
  m$unit_definitions
}
