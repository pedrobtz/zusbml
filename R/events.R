# Events (design section 6.3, D10; Level 3 Version 2 Core section 4.12).
# sbml_odes() carries each event as data; sbml_derivs() runs them through
# an engine attached to the derivative function as a deSolve root function
# and event handler.

# Each event as a list: its trigger, initial value, persistence, priority
# and delay expressions (NULL when absent), whether assignments use values
# from trigger time, and the assignments as a named list of expressions by
# target symbol. Targets must be states: a constant or an assigned symbol
# cannot be changed by an event.
derive_events <- function(m, roles, rhs, assignments) {
  ev <- m$events
  out <- vector("list", nrow(ev))
  for (i in seq_len(nrow(ev))) {
    id <- ev$id[[i]] %|% paste0("event ", i)
    fix <- function(expr) {
      if (is.null(expr)) {
        NULL
      } else {
        replace_rate_of(expr, rhs, roles, assignments)
      }
    }
    a <- ev$assignments[[i]]
    keep <- !vapply(a$math, is.null, logical(1))
    targets <- a$variable[keep]
    for (target in targets) {
      role <- roles$role[roles$name == target]
      amount <- paste0(".amount.", target)
      if (amount %in% roles$name[roles$role == "state"]) {
        next
      }
      if (length(role) == 0 || role != "state") {
        abort_derive(
          paste0(
            "Event '",
            id,
            "' assigns '",
            target,
            "', which is constant or set by a rule."
          ),
          c(id, target)
        )
      }
    }
    trigger <- ev$trigger[[i]]
    out[[i]] <- list(
      id = id,
      # An event without a trigger (allowed in Level 3 Version 2) never fires.
      trigger = fix(trigger$math) %||% FALSE,
      initial_value = trigger$initial_value %||% FALSE,
      persistent = trigger$persistent %||% TRUE,
      priority = fix(ev$priority[[i]]$math),
      delay = fix(ev$delay[[i]]$math),
      use_values_from_trigger_time = ev$use_values_from_trigger_time[[i]],
      assignments = stats::setNames(lapply(a$math[keep], fix), targets)
    )
  }
  out
}

#' A derivative function for deSolve
#'
#' `sbml_derivs()` turns a model into a `function(t, y, parms)` for
#' [deSolve::ode()]. The function carries what deSolve needs as attributes:
#' `y0`, the states at time 0 after any events that fire then; `parms`, the
#' constants; and, for a model with events, `rootfunc` and `events`, which
#' run the events with the full SBML semantics: delays, persistence,
#' priorities, `useValuesFromTriggerTime`, and events triggered by other
#' events at the same time.
#'
#' The function keeps the event queue between calls. Use it for one
#' integration starting at time 0; calling it again at time 0 restarts the
#' queue.
#'
#' To change constants, pass them in `parms` here rather than editing the
#' `parms` attribute: initial assignments and events at time 0 that depend
#' on them are then reflected in `y0`. The function and its events always
#' use the `parms` the solver passes.
#'
#' @inheritParams sbml_odes
#' @param outputs Model symbols to report as extra columns of the solver
#'   output, such as species defined by assignment rules or reaction rates.
#' @param parms A named numeric vector of constants to use instead of the
#'   model's values, or `NULL`.
#' @return A function with attributes `y0`, `parms`, and for models with
#'   events `rootfunc` and `events`.
#' @export
#' @examplesIf requireNamespace("deSolve", quietly = TRUE)
#' m <- sbml_read(system.file("extdata", "decay.xml", package = "zusbml"))
#' f <- sbml_derivs(m)
#' deSolve::ode(attr(f, "y0"), seq(0, 2, by = 0.5), f, attr(f, "parms"))
sbml_derivs <- function(m, outputs = character(0), parms = NULL) {
  check_model(m)
  if (!is.character(outputs) || anyNA(outputs)) {
    abort_arg("`outputs` must be a character vector without NA.", "outputs")
  }
  if (
    !is.null(parms) &&
      (!is.numeric(parms) || is.null(names(parms)) || anyNA(names(parms)))
  ) {
    abort_arg("`parms` must be a named numeric vector or NULL.", "parms")
  }
  eq <- derive_odes(m, parms)
  known <- c(names(eq$constants), eq$states$name, names(eq$assignments))
  unknown <- setdiff(outputs, known)
  if (length(unknown) > 0) {
    abort_arg(
      paste0(
        "`outputs` names ",
        paste0("'", unknown, "'", collapse = ", "),
        ", which the model does not define."
      ),
      "outputs"
    )
  }
  f <- derivs_function(eq, outputs)
  attr(f, "parms") <- eq$constants
  if (length(eq$events) == 0) {
    attr(f, "y0") <- eq$initial
    return(f)
  }
  engine <- event_engine(eq)
  y0 <- engine$start(eq$initial)
  inner <- f
  g <- function(.t, .y, .p) {
    if (.t == 0) {
      engine$restart()
    }
    inner(.t, .y, .p)
  }
  attributes(g) <- attributes(f)
  attr(g, "y0") <- y0
  attr(g, "rootfunc") <- engine$roots
  attr(g, "events") <- list(func = engine$handle, root = TRUE, maxroot = 1e5)
  g
}

# The values of every symbol at time t for state y, as an environment.
scope_function <- function(eq) {
  f <- derivs_function(eq)
  b <- body(f)
  b[[length(b)]] <- quote(environment())
  body(f) <- b
  f
}

# The values of `exprs` at time t for state y, compiled into one function
# like derivs_function(): the root function runs at every solver step, and
# evaluating each atom separately dominated the run time.
values_function <- function(eq, exprs) {
  f <- derivs_function(eq)
  b <- body(f)
  b[[length(b)]] <- as.call(c(
    as.name("c"),
    lapply(exprs, function(e) call("as.numeric", e))
  ))
  body(f) <- b
  f
}

# Relational atoms of a trigger as continuous functions (lhs - rhs): a
# trigger can only change value where one of them crosses zero, so they are
# the solver's root functions. A trigger with none gets a step root.
trigger_atoms <- function(expr) {
  relational <- c(">", "<", ">=", "<=", "==", "!=")
  atoms <- list()
  walk <- function(e) {
    if (!is.call(e)) {
      return()
    }
    fn <- as.character(e[[1]])
    if (fn %in% relational) {
      atoms[[length(atoms) + 1]] <<- call("-", e[[2]], e[[3]])
      return()
    }
    for (arg in as.list(e)[-1]) {
      walk(arg)
    }
  }
  walk(expr)
  if (length(atoms) == 0) {
    atoms <- list(call("-", call("as.numeric", expr), 0.5))
  }
  atoms
}

event_engine <- function(eq) {
  events <- eq$events
  scope <- scope_function(eq)
  derivs <- derivs_function(eq)
  states <- eq$states$name
  atoms <- unlist(
    lapply(events, function(e) trigger_atoms(e$trigger)),
    recursive = FALSE
  )
  atom_values <- values_function(eq, atoms)
  tol <- 1e-12

  state <- new.env(parent = emptyenv())
  # The constants in force: those of the time 0 solve, then whatever the
  # solver passes to the root function and the event handler.
  reset <- function() {
    state$parms <- eq$constants
    state$previous <- vapply(events, function(e) e$initial_value, logical(1))
    state$pending <- list()
    state$next_time <- Inf
    state$sequence <- 0
  }

  value_in <- function(expr, env) eval(expr, env)
  truth <- function(expr, env) isTRUE(as.logical(value_in(expr, env)))

  fire <- function(i, t, env) {
    e <- events[[i]]
    delay <- if (is.null(e$delay)) 0 else as.numeric(value_in(e$delay, env))
    if (!is.finite(delay) || delay < 0) {
      abort_derive(
        paste0("Event '", e$id, "' has delay ", delay, " at time ", t, "."),
        e$id
      )
    }
    values <- if (e$use_values_from_trigger_time) {
      lapply(e$assignments, function(x) as.numeric(value_in(x, env)))
    }
    state$sequence <- state$sequence + 1
    state$pending[[length(state$pending) + 1]] <- list(
      event = i,
      time = t + delay,
      values = values,
      sequence = state$sequence
    )
  }

  # Apply one event's assignments, all computed before any is set (section
  # 4.12.5). A concentration integrated as an amount converts with the
  # compartment size at execution, before this event resizes anything, even
  # when its value was taken at trigger time.
  execute <- function(item, t, y) {
    env <- scope(t, y, state$parms)
    values <- item$values %||%
      lapply(events[[item$event]]$assignments, function(x) {
        as.numeric(value_in(x, env))
      })
    for (target in names(values)) {
      if (target %in% states) {
        y[[match(target, states)]] <- values[[target]]
      } else {
        compartment <- eq$species$compartment[eq$species$id == target]
        y[[match(paste0(".amount.", target), states)]] <-
          values[[target]] * get(compartment, env)
      }
    }
    y
  }

  # Trigger values just after time t. The solver stops where a trigger's
  # relational atom is zero, where `S < 0.1` is not yet true; a forward
  # Euler step of relative size 1e-9 says which side the trajectory is
  # heading to.
  triggers_after <- function(t, y) {
    h <- 1e-9 * max(1, abs(t))
    ahead <- if (length(y) > 0) y + h * derivs(t, y, state$parms)[[1]] else y
    env <- scope(t + h, ahead, state$parms)
    vapply(events, function(e) truth(e$trigger, env), logical(1))
  }

  triggers_at <- function(t, y) {
    env <- scope(t, y, state$parms)
    vapply(events, function(e) truth(e$trigger, env), logical(1))
  }

  # Process time t: edges of every trigger, then due events one at a time
  # in priority order, re-reading triggers after each.
  process <- function(t, y, read = triggers_after) {
    repeat {
      env <- scope(t, y, state$parms)
      now <- read(t, y)
      for (i in seq_along(events)) {
        if (now[[i]] && !state$previous[[i]]) {
          fire(i, t, env)
        }
        if (!now[[i]] && state$previous[[i]] && !events[[i]]$persistent) {
          state$pending <- Filter(function(p) p$event != i, state$pending)
        }
      }
      state$previous <- now
      due <- which(vapply(
        state$pending,
        function(p) p$time <= t + tol,
        logical(1)
      ))
      if (length(due) == 0) {
        times <- vapply(state$pending, function(p) p$time, numeric(1))
        state$next_time <- if (length(times) > 0) min(times) else Inf
        return(y)
      }
      priority <- vapply(
        state$pending[due],
        function(p) {
          expr <- events[[p$event]]$priority
          value <- if (is.null(expr)) -Inf else as.numeric(value_in(expr, env))
          # A priority that is NaN ranks below every number.
          if (is.na(value)) -Inf else value
        },
        numeric(1)
      )
      # Events of equal priority, or without one, run in random order
      # (section 4.12.7); set a seed for reproducible runs.
      top <- due[priority == max(priority)]
      pick <- if (length(top) > 1) top[[sample.int(length(top), 1)]] else top
      item <- state$pending[[pick]]
      state$pending <- state$pending[-pick]
      y <- execute(item, t, y)
    }
  }

  snapshot <- NULL
  list(
    start = function(y0) {
      reset()
      y <- process(0, y0, read = triggers_at)
      y <- process(0, y)
      snapshot <<- as.list(state)
      y
    },
    restart = function() {
      for (name in names(snapshot)) {
        assign(name, snapshot[[name]], state)
      }
    },
    roots = function(.t, .y, .p) {
      state$parms <- .p
      values <- atom_values(.t, .y, state$parms)
      # LSODAR rejects a root function that is exactly zero where it starts
      # (time 0, after an event), and a trajectory can sit on a trigger's
      # boundary. Give such zeros the sign they have a step ahead, or 1.
      # An atom that is NaN cannot cross zero; keep it away from a root.
      values[is.na(values)] <- 1
      zero <- values == 0
      if (any(zero)) {
        h <- 1e-9 * max(1, abs(.t))
        ahead <- if (length(.y) > 0) {
          .y + h * derivs(.t, .y, state$parms)[[1]]
        } else {
          .y
        }
        values[zero] <- atom_values(.t + h, ahead, state$parms)[zero]
        values[is.na(values) | values == 0] <- 1
      }
      # The earliest pending execution, even one the solver has stepped
      # past: the sign change from it is what stops the solver there.
      c(values, if (length(state$pending) > 0) .t - state$next_time else 1)
    },
    handle = function(.t, .y, .p) {
      state$parms <- .p
      process(.t, .y)
    }
  )
}
