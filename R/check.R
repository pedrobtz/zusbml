#' Check a model's structure
#'
#' Checks the structural rules derivation relies on:
#' ids are unique, references resolve, a symbol is set by at most one rule
#' (or by reactions and not also by a rule), rules and events do not change
#' constants, and assignment rules do not form a cycle. This is not
#' libSBML's full rule set.
#'
#' @inheritParams sbml_odes
#' @return A data frame with one row per finding: `rule` (`"duplicate_id"`,
#'   `"unknown_reference"`, `"multiple_definitions"`, `"constant_target"` or
#'   `"assignment_cycle"`), the `id` concerned, and a `message`. No rows when
#'   the model passes. For an `sbml_model`, `sbml_check()` never raises; it
#'   raises `zusbml_arg_error` for anything else.
#' @export
#' @examples
#' m <- sbml_read(system.file("extdata", "decay.xml", package = "zusbml"))
#' sbml_check(m)
sbml_check <- function(m) {
  check_model(m)
  findings <- list()
  add <- function(rule, id, message) {
    findings[[length(findings) + 1]] <<- data.frame(
      rule = rule,
      id = id,
      message = message,
      stringsAsFactors = FALSE
    )
  }

  refs <- species_reference_rows(m)
  modifiers <- unlist(lapply(m$reactions$modifiers, function(x) x$species))

  # Ids share one namespace in Level 3, except unit definitions and local
  # parameters (section 3.3).
  global <- c(
    m$functions$id,
    m$compartments$id,
    m$species$id,
    m$parameters$id,
    m$reactions$id,
    refs$id,
    m$events$id,
    m$initial_assignments$id,
    m$rules$id,
    m$constraints$id
  )
  global <- global[!is.na(global)]
  for (id in unique(global[duplicated(global)])) {
    add("duplicate_id", id, paste0("The id '", id, "' is used more than once."))
  }

  valued <- c(
    m$compartments$id,
    m$species$id,
    m$parameters$id,
    refs$id[!is.na(refs$id)]
  )
  check_ref <- function(id, kinds, what, owner) {
    if (!is.na(id) && !id %in% kinds) {
      add(
        "unknown_reference",
        id,
        paste0(
          what,
          " of '",
          owner,
          "' refers to '",
          id,
          "', which is not defined."
        )
      )
    }
  }
  for (i in seq_len(nrow(m$species))) {
    check_ref(
      m$species$compartment[[i]],
      m$compartments$id,
      "The compartment",
      m$species$id[[i]]
    )
    check_ref(
      m$species$conversion_factor[[i]],
      m$parameters$id,
      "The conversion factor",
      m$species$id[[i]]
    )
  }
  check_ref(
    m$conversion_factor,
    m$parameters$id,
    "The conversion factor",
    "the model"
  )
  for (i in seq_len(nrow(refs))) {
    check_ref(
      refs$species[[i]],
      m$species$id,
      "A species reference",
      refs$reaction[[i]]
    )
  }
  for (s in modifiers) {
    check_ref(s, m$species$id, "A modifier", "a reaction")
  }
  for (v in m$rules$variable[!is.na(m$rules$variable)]) {
    check_ref(v, valued, "The variable", "a rule")
  }
  for (v in m$initial_assignments$symbol) {
    check_ref(v, valued, "The symbol", "an initial assignment")
  }
  for (a in m$events$assignments) {
    for (v in a$variable) {
      check_ref(v, valued, "The variable", "an event")
    }
  }

  # Names used in math must be model symbols, reaction ids (their rates),
  # local parameters of the same reaction, or a lambda's bound variables.
  known <- c(valued, m$reactions$id, "time", ".time", "pi")
  check_math <- function(expr, where, extra = character(0)) {
    if (is.null(expr)) {
      return()
    }
    if (is.call(expr) && identical(expr[[1]], as.name("function"))) {
      extra <- c(extra, names(expr[[2]]))
    }
    for (name in setdiff(all.vars(expr), c(known, extra))) {
      add(
        "unknown_reference",
        name,
        paste0(
          "The math of '",
          where,
          "' uses '",
          name,
          "', which is not defined."
        )
      )
    }
  }
  for (i in seq_len(nrow(m$functions))) {
    lambda <- m$functions$math[[i]]
    if (is.call(lambda) && identical(lambda[[1]], as.name("function"))) {
      unbound <- setdiff(
        all.vars(lambda[[3]]),
        c(names(lambda[[2]]), "time", ".time", "pi")
      )
      for (name in unbound) {
        add(
          "unknown_reference",
          name,
          paste0(
            "Function '",
            m$functions$id[[i]],
            "' uses '",
            name,
            "', which is not one of its arguments."
          )
        )
      }
    }
  }
  for (i in seq_len(nrow(m$rules))) {
    check_math(m$rules$math[[i]], m$rules$variable[[i]] %|% "a rule")
  }
  for (i in seq_len(nrow(m$initial_assignments))) {
    check_math(
      m$initial_assignments$math[[i]],
      m$initial_assignments$symbol[[i]]
    )
  }
  for (i in seq_len(nrow(m$reactions))) {
    law <- m$reactions$kinetic_law[[i]]
    check_math(law$math, m$reactions$id[[i]], law$local_parameters$id)
  }
  for (i in seq_len(nrow(m$events))) {
    where <- m$events$id[[i]] %|% paste0("event ", i)
    for (p in c("trigger", "priority", "delay")) {
      check_math(m$events[[p]][[i]]$math, where)
    }
    for (expr in m$events$assignments[[i]]$math) {
      check_math(expr, where)
    }
  }

  # A symbol is set by at most one rule; reactions and a rule cannot both
  # set a non-boundary species (section 4.9.5).
  ruled <- m$rules$variable[!is.na(m$rules$variable)]
  for (v in unique(ruled[duplicated(ruled)])) {
    add(
      "multiple_definitions",
      v,
      paste0("'", v, "' is the variable of more than one rule.")
    )
  }
  assigned <- m$rules$variable[m$rules$type == "assignment"]
  for (v in intersect(assigned, m$initial_assignments$symbol)) {
    add(
      "multiple_definitions",
      v,
      paste0(
        "'",
        v,
        "' has both an assignment rule and an initial ",
        "assignment."
      )
    )
  }
  changed <- m$species$id[!m$species$boundary_condition]
  for (v in intersect(intersect(ruled, changed), refs$species)) {
    add(
      "multiple_definitions",
      v,
      paste0(
        "Species '",
        v,
        "' is changed by reactions and set by a ",
        "rule; make it a boundary species."
      )
    )
  }

  constants <- c(
    m$compartments$id[m$compartments$constant],
    m$species$id[m$species$constant],
    m$parameters$id[m$parameters$constant],
    refs$id[!is.na(refs$id) & refs$constant]
  )
  event_targets <- unlist(lapply(m$events$assignments, function(a) a$variable))
  for (v in intersect(c(ruled, event_targets), constants)) {
    add(
      "constant_target",
      v,
      paste0("'", v, "' is constant but set by a rule or an event.")
    )
  }

  rules <- m$rules[
    m$rules$type == "assignment" &
      !vapply(m$rules$math, is.null, logical(1)),
  ]
  cycle <- tryCatch(
    {
      sort_definitions(
        stats::setNames(rules$math, rules$variable),
        "assignment rules"
      )
      NULL
    },
    zusbml_derive_error = function(cnd) cnd$ids
  )
  for (v in cycle) {
    add(
      "assignment_cycle",
      v,
      paste0("The assignment rule for '", v, "' is part of a cycle.")
    )
  }

  if (length(findings) == 0) {
    return(data.frame(
      rule = character(),
      id = character(),
      message = character(),
      stringsAsFactors = FALSE
    ))
  }
  out <- do.call(rbind, findings)
  rownames(out) <- NULL
  out
}
