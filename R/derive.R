# Derivation (design section 6.3): an sbml_model becomes the state
# variables, their right-hand sides, the assignments evaluated alongside,
# and the constants, following the Level 3 Version 2 Core specification,
# sections 4.9 (rules) and 4.11 (reactions). Constructs it cannot derive are
# refused, never approximated (D7).
#
# Every model symbol gets one role:
# - "constant": a value fixed after time 0 (constant=TRUE, local parameters);
# - "assignment": set by an assignment rule, a reaction's rate (a reaction id
#   in math means its rate), or a concentration defined from an amount;
# - "state": integrated, with a right-hand side (0 when only events change it).
# A species whose symbol is a concentration in a compartment that can change
# size is integrated as an amount, under the internal name `.amount.<id>`;
# its concentration is then an assignment. SBML ids cannot contain ".", so
# internal names never collide with model ids.

#' Derive the equations of a model
#'
#' `sbml_odes()` turns a model into what an ODE solver needs: the states,
#' the right-hand side of each, the assignments evaluated before the
#' right-hand sides at every time, and the constants.
#' `sbml_initial_state()` gives the states' values at time 0.
#'
#' Species whose symbol is a concentration are integrated as concentrations,
#' except in a compartment whose size can change, where the amount is
#' integrated (as the state `.amount.<id>`) and the concentration is an
#' assignment. Reaction ids are assignments holding the reaction's rate.
#' Function definitions are inlined, local parameters become constants named
#' `.local.<reaction>.<id>`, and `rateOf()` is replaced by the right-hand
#' side it refers to.
#'
#' @param m An `sbml_model`.
#' @return `sbml_odes()` returns a list:
#'   * `states`: a data frame with the state's `name`, the model `symbol` it
#'     belongs to, and its `kind` (`"species"`, `"compartment"`, ...);
#'   * `rhs`: a named list of expressions, the time derivative of each state;
#'   * `assignments`: a named list of expressions in evaluation order;
#'   * `constants`: a named numeric vector of the constants' values;
#'   * `initial`: the states' values at time 0, as `sbml_initial_state()`;
#'   * `species`: for each species, whether its symbol is an amount and its
#'     compartment, to convert between amounts and concentrations.
#'
#'   `sbml_initial_state()` returns a named numeric vector.
#' @section Errors:
#' * `zusbml_unsupported` for algebraic rules (the model is then a DAE),
#'   the `delay` csymbol, fast reactions, and events, each naming the
#'   element.
#' * `zusbml_derive_error` for an assignment cycle or a missing value,
#'   naming the ids involved in its `ids` field.
#' @export
#' @examples
#' m <- sbml_read(system.file("extdata", "decay.xml", package = "zusbml"))
#' eq <- sbml_odes(m)
#' eq$rhs
#' sbml_initial_state(m)
sbml_odes <- function(m) {
  check_model(m)
  refuse_underivable(m)
  # A Level 3 Version 2 rule without math has no effect (section 4.9).
  m$rules <- m$rules[!vapply(m$rules$math, is.null, logical(1)), ]
  m <- inline_functions(m)
  symbols <- symbol_table(m)
  roles <- symbol_roles(m, symbols)
  rhs <- state_rhs(m, symbols, roles)
  assignments <- runtime_assignments(m, symbols, roles)
  rhs <- lapply(
    rhs,
    replace_rate_of,
    rhs = rhs,
    roles = roles,
    assignments = assignments
  )
  assignments <- lapply(
    assignments,
    replace_rate_of,
    rhs = rhs,
    roles = roles,
    assignments = assignments
  )
  assignments <- sort_definitions(assignments, "assignment rules")

  values <- initial_values(m, symbols, roles, rhs, assignments)
  constant_ids <- roles$name[roles$role == "constant"]
  states <- roles[roles$role == "state", c("name", "symbol", "kind")]
  rownames(states) <- NULL
  list(
    states = states,
    rhs = rhs[states$name],
    assignments = assignments,
    constants = unlist(values[constant_ids]) %||% numeric(0),
    initial = unlist(values[states$name]) %||% numeric(0),
    species = species_info(m, symbols)
  )
}

#' @rdname sbml_odes
#' @export
sbml_initial_state <- function(m) {
  sbml_odes(m)$initial
}

abort_derive <- function(message, ids) {
  zusbml_abort("zusbml_derive_error", message, ids = ids)
}

# ---- Refusals --------------------------------------------------------------

refuse_underivable <- function(m) {
  algebraic <- m$rules[m$rules$type == "algebraic", ]
  if (nrow(algebraic) > 0) {
    abort_unsupported(
      "Algebraic rules make the model a DAE, which sbml_odes() does not derive.",
      construct = "algebraicRule",
      id = algebraic$id[[1]],
      section = "4.9.2"
    )
  }
  fast <- which(m$reactions$fast %in% TRUE)
  if (length(fast) > 0) {
    abort_unsupported(
      paste0(
        "Fast reaction '",
        m$reactions$id[[fast[[1]]]],
        "' is not supported."
      ),
      construct = "fast reaction",
      id = m$reactions$id[[fast[[1]]]],
      section = "4.11 (Level 3 Version 1)"
    )
  }
  if (nrow(m$events) > 0) {
    abort_unsupported(
      "Events are not derived yet.",
      construct = "event",
      id = m$events$id[[1]],
      section = "4.12"
    )
  }
  for (expr in model_math(m)) {
    if ("delay" %in% all.names(expr$math)) {
      abort_unsupported(
        paste0(
          "The delay csymbol (in ",
          expr$where,
          ") makes the model a ",
          "delay differential equation, which is not supported."
        ),
        construct = "delay",
        id = expr$where,
        section = "3.4.6"
      )
    }
  }
}

# Every expression in a model, with where it is.
model_math <- function(m) {
  out <- list()
  add <- function(math, where) {
    if (!is.null(math)) {
      out[[length(out) + 1]] <<- list(math = math, where = where)
    }
  }
  for (i in seq_len(nrow(m$functions))) {
    add(m$functions$math[[i]], m$functions$id[[i]])
  }
  for (i in seq_len(nrow(m$initial_assignments))) {
    add(m$initial_assignments$math[[i]], m$initial_assignments$symbol[[i]])
  }
  for (i in seq_len(nrow(m$rules))) {
    add(m$rules$math[[i]], m$rules$variable[[i]] %|% "algebraic rule")
  }
  for (i in seq_len(nrow(m$reactions))) {
    add(m$reactions$kinetic_law[[i]]$math, m$reactions$id[[i]])
  }
  for (i in seq_len(nrow(m$events))) {
    where <- m$events$id[[i]] %|% paste0("event ", i)
    add(m$events$trigger[[i]]$math, where)
    add(m$events$priority[[i]]$math, where)
    add(m$events$delay[[i]]$math, where)
    a <- m$events$assignments[[i]]
    for (j in seq_len(nrow(a))) {
      add(a$math[[j]], where)
    }
  }
  out
}

`%|%` <- function(x, y) if (is.na(x)) y else x

# ---- Function definitions --------------------------------------------------

# Replace every call to a function definition by its body, with the bound
# variables substituted by the arguments, until none is left.
inline_functions <- function(m) {
  if (nrow(m$functions) == 0) {
    return(m)
  }
  lambdas <- stats::setNames(m$functions$math, m$functions$id)
  expand <- function(expr, depth = 0) {
    if (depth > 100) {
      abort_derive(
        "Function definitions call each other in a cycle.",
        names(lambdas)
      )
    }
    if (!is.call(expr)) {
      return(expr)
    }
    expr[] <- lapply(expr, expand, depth = depth)
    fn <- expr[[1]]
    if (is.name(fn) && as.character(fn) %in% names(lambdas)) {
      lambda <- lambdas[[as.character(fn)]]
      formals <- names(lambda[[2]])
      args <- as.list(expr)[-1]
      if (length(args) != length(formals)) {
        abort_derive(
          paste0(
            "Function '",
            fn,
            "' takes ",
            length(formals),
            " arguments, not ",
            length(args),
            "."
          ),
          as.character(fn)
        )
      }
      body <- do.call(
        substitute,
        list(lambda[[3]], stats::setNames(args, formals))
      )
      return(expand(body, depth + 1))
    }
    expr
  }
  map_math(m, expand)
}

# Apply `f` to every expression in the model outside function definitions.
map_math <- function(m, f) {
  g <- function(x) if (is.null(x)) NULL else f(x)
  m$initial_assignments$math <- lapply(m$initial_assignments$math, g)
  m$rules$math <- lapply(m$rules$math, g)
  m$reactions$kinetic_law <- lapply(m$reactions$kinetic_law, function(k) {
    if (!is.null(k)) {
      k$math <- g(k$math)
    }
    k
  })
  for (part in c("trigger", "priority", "delay")) {
    m$events[[part]] <- lapply(m$events[[part]], function(p) {
      if (!is.null(p)) {
        p$math <- g(p$math)
      }
      p
    })
  }
  m$events$assignments <- lapply(m$events$assignments, function(a) {
    a$math <- lapply(a$math, g)
    a
  })
  m
}

# ---- Symbols and roles -----------------------------------------------------

species_reference_rows <- function(m) {
  rows <- list()
  for (i in seq_len(nrow(m$reactions))) {
    for (side in c("reactants", "products")) {
      refs <- m$reactions[[side]][[i]]
      for (j in seq_len(nrow(refs))) {
        rows[[length(rows) + 1]] <- data.frame(
          reaction = m$reactions$id[[i]],
          side = side,
          species = refs$species[[j]],
          id = refs$id[[j]],
          stoichiometry = refs$stoichiometry[[j]],
          constant = refs$constant[[j]],
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (length(rows) == 0) {
    return(data.frame(
      reaction = character(),
      side = character(),
      species = character(),
      id = character(),
      stoichiometry = double(),
      constant = logical()
    ))
  }
  do.call(rbind, rows)
}

# One row per model symbol that has a value: its kind, whether it is
# declared constant, and its default value at time 0 as an expression (a
# number, NA when it has none, or a conversion between amount and
# concentration).
symbol_table <- function(m) {
  comps <- m$compartments
  sp <- m$species
  par <- m$parameters
  refs <- species_reference_rows(m)
  refs <- refs[!is.na(refs$id), ]
  spatial <- stats::setNames(comps$spatial_dimensions, comps$id)

  species_default <- lapply(seq_len(nrow(sp)), function(i) {
    amount <- sp$initial_amount[[i]]
    conc <- sp$initial_concentration[[i]]
    size <- as.name(sp$compartment[[i]])
    if (species_is_amount(sp[i, ], spatial)) {
      if (!is.na(amount)) {
        amount
      } else if (!is.na(conc)) {
        call("*", conc, size)
      } else {
        NA_real_
      }
    } else {
      if (!is.na(conc)) {
        conc
      } else if (!is.na(amount)) {
        call("/", amount, size)
      } else {
        NA_real_
      }
    }
  })

  out <- data.frame(
    id = c(comps$id, sp$id, par$id, refs$id),
    kind = c(
      rep("compartment", nrow(comps)),
      rep("species", nrow(sp)),
      rep("parameter", nrow(par)),
      rep("speciesReference", nrow(refs))
    ),
    constant = c(comps$constant, sp$constant, par$constant, refs$constant),
    stringsAsFactors = FALSE
  )
  out$default <- c(
    as.list(comps$size),
    species_default,
    as.list(par$value),
    as.list(refs$stoichiometry)
  )
  out
}

species_is_amount <- function(species_row, spatial) {
  isTRUE(species_row$has_only_substance_units) ||
    identical(unname(spatial[species_row$compartment]), 0)
}

symbol_roles <- function(m, symbols) {
  rules <- m$rules
  assigned <- rules$variable[rules$type == "assignment"]
  rated <- rules$variable[rules$type == "rate"]
  comps <- m$compartments
  sp <- m$species
  spatial <- stats::setNames(comps$spatial_dimensions, comps$id)
  varying <- comps$id[!comps$constant]

  rows <- list()
  add <- function(name, symbol, kind, role) {
    rows[[length(rows) + 1]] <<- data.frame(
      name = name,
      symbol = symbol,
      kind = kind,
      role = role,
      stringsAsFactors = FALSE
    )
  }
  for (i in seq_len(nrow(symbols))) {
    id <- symbols$id[[i]]
    kind <- symbols$kind[[i]]
    if (id %in% assigned) {
      add(id, id, kind, "assignment")
    } else if (id %in% rated) {
      add(id, id, kind, "state")
    } else if (
      kind == "species" && species_by_amount(id, sp, spatial, varying)
    ) {
      # A concentration in a compartment that changes size: the amount is
      # what stays constant or is integrated (section 4.6.5).
      amount_role <- if (isTRUE(symbols$constant[[i]])) "constant" else "state"
      add(paste0(".amount.", id), id, kind, amount_role)
      add(id, id, kind, "assignment")
    } else if (isTRUE(symbols$constant[[i]])) {
      add(id, id, kind, "constant")
    } else {
      add(id, id, kind, "state")
    }
  }
  for (i in seq_len(nrow(m$reactions))) {
    r <- m$reactions[i, ]
    add(r$id, r$id, "reaction", "assignment")
    params <- r$kinetic_law[[1]]$local_parameters
    for (p in params$id %||% character(0)) {
      add(
        local_name(r$id, p),
        local_name(r$id, p),
        "localParameter",
        "constant"
      )
    }
  }
  if (length(rows) == 0) {
    return(data.frame(
      name = character(),
      symbol = character(),
      kind = character(),
      role = character()
    ))
  }
  do.call(rbind, rows)
}

species_by_amount <- function(id, sp, spatial, varying) {
  row <- sp[sp$id == id, ]
  !species_is_amount(row, spatial) && row$compartment %in% varying
}

local_name <- function(reaction, id) paste0(".local.", reaction, ".", id)

# ---- Right-hand sides ------------------------------------------------------

state_rhs <- function(m, symbols, roles) {
  states <- roles[roles$role == "state", ]
  rules <- m$rules[m$rules$type == "rate", ]
  rate_of <- stats::setNames(rules$math, rules$variable)
  sp <- m$species
  refs <- species_reference_rows(m)
  laws <- reaction_rates(m)
  model_cf <- m$conversion_factor

  rhs <- list()
  for (i in seq_len(nrow(states))) {
    name <- states$name[[i]]
    symbol <- states$symbol[[i]]
    if (symbol %in% names(rate_of)) {
      rhs[[name]] <- rate_of[[symbol]]
      next
    }
    if (states$kind[[i]] != "species") {
      rhs[[name]] <- 0
      next
    }
    row <- sp[sp$id == symbol, ]
    change <- if (isTRUE(row$boundary_condition)) {
      0
    } else {
      species_change(symbol, refs, laws)
    }
    cf <- row$conversion_factor %|% (model_cf %||% NA_character_)
    if (!is.na(cf) && !identical(change, 0)) {
      change <- call("*", change, as.name(cf))
    }
    is_amount_state <- startsWith(name, ".amount.") ||
      species_is_amount(
        row,
        stats::setNames(
          m$compartments$spatial_dimensions,
          m$compartments$id
        )
      )
    if (!is_amount_state && !identical(change, 0)) {
      change <- call("/", change, as.name(row$compartment))
    }
    rhs[[name]] <- change
  }
  rhs
}

reaction_rates <- function(m) {
  out <- list()
  for (i in seq_len(nrow(m$reactions))) {
    r <- m$reactions[i, ]
    law <- r$kinetic_law[[1]]
    if (is.null(law) || is.null(law$math)) {
      abort_derive(
        paste0("Reaction '", r$id, "' has no kinetic law to derive from."),
        r$id
      )
    }
    params <- law$local_parameters
    rename <- stats::setNames(
      lapply(params$id, function(p) as.name(local_name(r$id, p))),
      params$id
    )
    out[[r$id]] <- if (length(rename) > 0) {
      do.call(substitute, list(law$math, rename))
    } else {
      law$math
    }
  }
  out
}

# The sum over reactions of signed stoichiometry times rate for a species,
# in amount per time.
species_change <- function(species, refs, laws) {
  mine <- refs[refs$species == species, ]
  terms <- list()
  for (j in seq_len(nrow(mine))) {
    stoich <- if (!is.na(mine$id[[j]])) {
      as.name(mine$id[[j]])
    } else if (!is.na(mine$stoichiometry[[j]])) {
      mine$stoichiometry[[j]]
    } else {
      abort_derive(
        paste0(
          "A species reference to '",
          species,
          "' in reaction '",
          mine$reaction[[j]],
          "' has no stoichiometry."
        ),
        c(mine$reaction[[j]], species)
      )
    }
    rate <- as.name(mine$reaction[[j]])
    term <- if (identical(stoich, 1)) rate else call("*", stoich, rate)
    terms[[length(terms) + 1]] <- if (mine$side[[j]] == "reactants") {
      call("-", term)
    } else {
      term
    }
  }
  if (length(terms) == 0) {
    return(0)
  }
  Reduce(
    function(a, b) {
      if (is.call(b) && identical(b[[1]], as.name("-")) && length(b) == 2) {
        call("-", a, b[[2]])
      } else {
        call("+", a, b)
      }
    },
    terms
  )
}

# ---- Assignments -----------------------------------------------------------

runtime_assignments <- function(m, symbols, roles) {
  rules <- m$rules[m$rules$type == "assignment", ]
  out <- stats::setNames(rules$math, rules$variable)
  out <- c(out, reaction_rates(m))
  amounts <- roles$name[startsWith(roles$name, ".amount.")]
  for (name in amounts) {
    id <- sub("^\\.amount\\.", "", name)
    compartment <- m$species$compartment[m$species$id == id]
    out[[id]] <- call("/", as.name(name), as.name(compartment))
  }
  out
}

# Replace rateOf(x) by x's right-hand side (design section 6.2): 0 for a
# constant, the state's right-hand side, and for a concentration integrated
# as an amount the quotient rule.
replace_rate_of <- function(expr, rhs, roles, assignments, depth = 0) {
  if (!is.call(expr)) {
    return(expr)
  }
  if (depth > 50) {
    abort_derive("rateOf() refers back to itself.", character(0))
  }
  if (identical(expr[[1]], as.name("rateOf"))) {
    target <- expr[[2]]
    if (!is.name(target)) {
      abort_unsupported(
        "rateOf() of an expression is not supported.",
        construct = "rateOf",
        section = "3.4.6"
      )
    }
    id <- as.character(target)
    role <- roles$role[roles$name == id]
    amount_state <- paste0(".amount.", id)
    value <- if (amount_state %in% names(rhs)) {
      compartment <- as.character(assignments[[id]][[3]])
      dv <- replace_rate_of(
        call("rateOf", as.name(compartment)),
        rhs,
        roles,
        assignments,
        depth + 1
      )
      call(
        "/",
        call("-", rhs[[amount_state]], call("*", target, dv)),
        as.name(compartment)
      )
    } else if (length(role) == 0 || role == "constant") {
      0
    } else if (role == "state") {
      rhs[[id]]
    } else {
      abort_unsupported(
        paste0("rateOf(", id, ") of an assigned symbol is not supported."),
        construct = "rateOf",
        id = id,
        section = "3.4.6"
      )
    }
    return(replace_rate_of(value, rhs, roles, assignments, depth + 1))
  }
  expr[] <- lapply(
    expr,
    replace_rate_of,
    rhs = rhs,
    roles = roles,
    assignments = assignments,
    depth = depth
  )
  expr
}

# Order named definitions so that each comes after those it uses; a cycle is
# a derive error naming the ids in it.
sort_definitions <- function(defs, what) {
  if (length(defs) == 0) {
    return(defs)
  }
  names_all <- names(defs)
  deps <- lapply(defs, function(e) {
    setdiff(intersect(all.names(e), names_all), character(0))
  })
  done <- character(0)
  order <- character(0)
  remaining <- names_all
  while (length(remaining) > 0) {
    ready <- remaining[vapply(
      remaining,
      function(n) {
        all(setdiff(deps[[n]], n) %in% done) && !(n %in% deps[[n]])
      },
      logical(1)
    )]
    if (length(ready) == 0) {
      abort_derive(
        paste0(
          "The ",
          what,
          " for ",
          paste(remaining, collapse = ", "),
          " depend on each other in a cycle."
        ),
        remaining
      )
    }
    order <- c(order, ready)
    done <- c(done, ready)
    remaining <- setdiff(remaining, ready)
  }
  defs[order]
}

# ---- Time 0 ----------------------------------------------------------------

# Every symbol's value at time 0. Initial assignments and assignment rules
# hold at time 0 together (section 4.8), so they, the reaction rates and the
# defaults are solved as one system in dependency order.
initial_values <- function(m, symbols, roles, rhs, assignments) {
  defs <- stats::setNames(symbols$default, symbols$id)
  for (r in seq_len(nrow(m$reactions))) {
    params <- m$reactions$kinetic_law[[r]]$local_parameters
    for (k in seq_len(nrow(params %||% data.frame()))) {
      defs[[local_name(m$reactions$id[[r]], params$id[[k]])]] <-
        params$value[[k]]
    }
  }
  ia <- m$initial_assignments
  for (k in seq_len(nrow(ia))) {
    if (!is.null(ia$math[[k]])) {
      defs[[ia$symbol[[k]]]] <- replace_rate_of(
        ia$math[[k]],
        rhs,
        roles,
        assignments
      )
    }
  }
  # Assignment rules and reaction rates hold at time 0; a concentration
  # defined from its amount does not, since the amount starts from it.
  for (name in names(assignments)) {
    if (!paste0(".amount.", name) %in% roles$name) {
      defs[[name]] <- assignments[[name]]
    }
  }
  # Amount states start from their concentration.
  for (name in roles$name[startsWith(roles$name, ".amount.")]) {
    id <- sub("^\\.amount\\.", "", name)
    compartment <- m$species$compartment[m$species$id == id]
    defs[[name]] <- call("*", as.name(id), as.name(compartment))
  }

  defs <- sort_definitions(defs, "initial values")
  env <- new.env(parent = math_env)
  env$.time <- 0
  if (!"time" %in% roles$name) {
    env$time <- 0
  }
  values <- list()
  for (name in names(defs)) {
    expr <- defs[[name]]
    value <- if (
      is.numeric(expr) && length(expr) == 1 && is.na(expr) && !is.nan(expr)
    ) {
      NA_real_
    } else {
      eval(expr, env)
    }
    assign(name, value, envir = env)
    values[[name]] <- as.numeric(value)
  }

  needed <- roles$name[roles$role %in% c("state", "constant")]
  missing <- needed[vapply(
    needed,
    function(n) {
      v <- values[[n]]
      is.null(v) || (is.na(v) && !is.nan(v))
    },
    logical(1)
  )]
  used <- unique(unlist(lapply(c(rhs, assignments), all.names)))
  missing <- missing[missing %in% used | missing %in% names(rhs)]
  if (length(missing) > 0) {
    abort_derive(
      paste0(
        "No value at time 0 for ",
        paste(missing, collapse = ", "),
        ": give it a value or an initial assignment."
      ),
      missing
    )
  }
  values
}

species_info <- function(m, symbols) {
  comps <- m$compartments
  spatial <- stats::setNames(comps$spatial_dimensions, comps$id)
  sp <- m$species
  data.frame(
    id = sp$id,
    compartment = sp$compartment,
    is_amount = vapply(
      seq_len(nrow(sp)),
      function(i) {
        species_is_amount(sp[i, ], spatial)
      },
      logical(1)
    ),
    stringsAsFactors = FALSE
  )
}

# ---- Evaluation ------------------------------------------------------------

# A function(t, y, p) for deSolve built from sbml_odes() output: it sets the
# constants, states and assignments in one environment and returns the
# right-hand sides, plus the values of `outputs` (symbol names) as extra
# columns.
derivs_function <- function(eq, outputs = character(0)) {
  # Arguments no SBML id can shadow: ids cannot start with ".".
  body <- list(as.name("{"), quote(.time <- .t))
  model_names <- c(names(eq$constants), eq$states$name, names(eq$assignments))
  if (!"time" %in% model_names) {
    body[[length(body) + 1]] <- quote(time <- .t)
  }
  for (name in names(eq$constants)) {
    body[[length(body) + 1]] <- call(
      "<-",
      as.name(name),
      call("[[", quote(.p), name)
    )
  }
  # By position: deSolve does not always keep the names of y.
  for (i in seq_along(eq$states$name)) {
    body[[length(body) + 1]] <- call(
      "<-",
      as.name(eq$states$name[[i]]),
      call("[[", quote(.y), i)
    )
  }
  for (name in names(eq$assignments)) {
    body[[length(body) + 1]] <- call(
      "<-",
      as.name(name),
      eq$assignments[[name]]
    )
  }
  dy <- as.call(c(
    as.name("c"),
    lapply(eq$states$name, function(n) {
      call("as.numeric", eq$rhs[[n]])
    })
  ))
  out <- as.call(c(
    as.name("c"),
    stats::setNames(
      lapply(outputs, function(o) call("as.numeric", as.name(o))),
      outputs
    )
  ))
  body[[length(body) + 1]] <- call("list", dy, out)
  f <- function(.t, .y, .p) NULL
  body(f) <- as.call(body)
  environment(f) <- math_env
  f
}
