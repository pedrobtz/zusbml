#' Write a model as rxode2 text
#'
#' Prints the derived equations ([sbml_odes()]) in the model syntax of the
#' rxode2 package: constants and assignments as `x = ...;`, initial
#' conditions as `x(0) = ...;` and right-hand sides as `d/dt(x) = ...;`. The
#' text is self-contained. Initial conditions are numbers computed from the
#' model's constants, so an initial value that depends on a constant does
#' not follow a parameter you pass to rxode2; edit the text instead.
#'
#' rxode2 text cannot express SBML events, which change states at triggered
#' times, so a model with events is refused unless `events = "drop"`, which
#' writes the continuous part only. Names rxode2 cannot take,
#' ids starting with `_` and those it reserves such as `time` or `param`, are
#' written with a `.u` prefix: `_J0` becomes `.u_J0`.
#'
#' @inheritParams sbml_odes
#' @param events `"refuse"` (the default) raises `zusbml_unsupported` for a
#'   model with events; `"drop"` leaves them out.
#' @return A single string of rxode2 model code.
#' @export
#' @examples
#' m <- sbml_read(system.file("extdata", "decay.xml", package = "zusbml"))
#' cat(sbml_to_rxode2(m))
sbml_to_rxode2 <- function(m, events = c("refuse", "drop")) {
  events <- match.arg(events)
  check_model(m)
  if (nrow(m$events) > 0 && events == "refuse") {
    abort_unsupported(
      paste0(
        "rxode2 text cannot express SBML events; use `events = \"drop\"` to ",
        "write the model without them."
      ),
      construct = "event",
      id = m$events$id[[1]],
      section = "4.12"
    )
  }
  m$events <- m$events[0, ]
  eq <- sbml_odes(m)

  ids <- model_ids(m)
  line <- function(lhs, expr) {
    paste0(lhs, " = ", rxode2_expr(expr, ids), ";")
  }
  names(eq$constants) <- rxode2_name(names(eq$constants))
  names(eq$initial) <- rxode2_name(names(eq$initial))
  names(eq$assignments) <- rxode2_name(names(eq$assignments))
  names(eq$rhs) <- rxode2_name(names(eq$rhs))
  eq$states$name <- rxode2_name(eq$states$name)
  lines <- c(
    paste0(
      "# ",
      if (!is.na(m$id)) m$id else "model",
      ", written by zusbml from SBML Level ",
      m$level,
      " Version ",
      m$version
    ),
    vapply(
      names(eq$constants),
      function(n) {
        line(n, eq$constants[[n]])
      },
      character(1)
    ),
    vapply(
      eq$states$name,
      function(n) {
        line(paste0(n, "(0)"), eq$initial[[n]])
      },
      character(1)
    ),
    vapply(
      names(eq$assignments),
      function(n) {
        line(n, eq$assignments[[n]])
      },
      character(1)
    ),
    vapply(
      eq$states$name,
      function(n) {
        line(paste0("d/dt(", n, ")"), eq$rhs[[n]])
      },
      character(1)
    )
  )
  paste0(paste(unname(lines), collapse = "\n"), "\n")
}

# Names rxode2 cannot assign, found by parsing `<name> = 1;` with it.
rxode2_reserved <- c(
  "t",
  "time",
  "Time",
  "TIME",
  "tlast",
  "amt",
  "evid",
  "cmt",
  "ss",
  "ii",
  "rate",
  "dur",
  "addl",
  "id",
  "ID",
  "dvid",
  "Inf",
  "NaN",
  "NA",
  "param",
  "M_PI",
  "newind",
  "NEWIND",
  "printf",
  "if",
  "while",
  "break",
  "ifelse"
)

# rxode2 names cannot start with "_", which SBML ids often do (`_J0`), nor be
# one of rxode2_reserved; such names become `.u_J0`, `.uparam`. SBML ids
# cannot contain ".", so the alias never collides with a model id.
rxode2_name <- function(x) {
  if (length(x) == 0) {
    return(x)
  }
  ifelse(
    startsWith(x, "_") | x %in% rxode2_reserved,
    paste0(".u", x),
    x
  )
}

# An R expression as rxode2 code. The functions of math_env are inlined from
# their bodies first, so what they contain is rewritten too; then `if`
# becomes ifelse(), log(x, b) becomes log(x)/log(b), `!` gets parentheses
# (rxode2 binds it tighter than relations, unlike R, whose deparse writes
# !(a > b) as !a > b), the time csymbol becomes rxode2's `t`, and model ids
# go through rxode2_name(). `ids` are the model's ids: the csymbol is `time`
# unless the model has an id of that name, and `.time` then (design D16).
rxode2_expr <- function(expr, ids = character(0)) {
  csymbol_time <- time_name(ids)
  inline <- function(e) {
    if (!is.call(e)) {
      return(e)
    }
    e[] <- lapply(e, inline)
    fn <- e[[1]]
    if (
      is.name(fn) &&
        exists(as.character(fn), envir = math_env, inherits = FALSE)
    ) {
      f <- get(as.character(fn), envir = math_env)
      args <- stats::setNames(as.list(e)[-1], names(formals(f)))
      return(call("(", do.call(substitute, list(body(f), args))))
    }
    e
  }
  rewrite <- function(e) {
    if (is.name(e)) {
      if (identical(e, csymbol_time)) {
        return(quote(t))
      }
      return(as.name(rxode2_name(as.character(e))))
    }
    if (!is.call(e)) {
      return(e)
    }
    fn <- e[[1]]
    e[-1] <- lapply(as.list(e)[-1], rewrite)
    if (!is.name(fn)) {
      return(e)
    }
    name <- as.character(fn)
    if (
      name == "!" && is.call(e[[2]]) && !identical(e[[2]][[1]], as.name("("))
    ) {
      return(call("!", call("(", e[[2]])))
    }
    if (name == "if") {
      otherwise <- if (length(e) == 4) e[[4]] else NaN
      return(call("ifelse", e[[2]], e[[3]], otherwise))
    }
    if (name == "log" && length(e) == 3) {
      return(call("/", call("log", e[[2]]), call("log", e[[3]])))
    }
    e
  }
  text <- deparse(
    rewrite(inline(expr)),
    width.cutoff = 500L,
    control = c("keepNA", "niceNames", "digits17")
  )
  paste(text, collapse = " ")
}
