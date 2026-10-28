# The SBML Test Suite oracle (design section 7). The suite is downloaded by
# tools/fetch-test-suite.sh and found through ZUSBML_TEST_SUITE; it is never
# committed (D5). Cases are judged by the suite's own tolerance rule (D12).

suite_unsupported_tags <- c("AlgebraicRule", "CSymbolDelay", "FastReaction")

# The one-case suite written for these tests, tests/testthat/fixtures/suite.
fixture_suite <- function() test_path("fixtures", "suite")

suite_dir <- function() {
  dir <- Sys.getenv("ZUSBML_TEST_SUITE")
  if (nzchar(dir) && file.exists(file.path(dir, "VERSION"))) {
    return(dir)
  }
  if (identical(Sys.getenv("ZUSBML_REQUIRE_SUITE"), "true")) {
    stop(structure(
      class = c("zusbml_suite_missing_error", "error", "condition"),
      list(
        message = paste0(
          "SBML Test Suite required but not found at '",
          dir,
          "'. ",
          "Run tools/fetch-test-suite.sh and set ZUSBML_TEST_SUITE."
        ),
        call = NULL
      )
    ))
  }
  testthat::skip("SBML Test Suite not downloaded (ZUSBML_TEST_SUITE unset)")
}

# "key: value" lines; numbers become doubles, comma lists character vectors.
suite_read_settings <- function(path) {
  lines <- readLines(path, warn = FALSE)
  lines <- lines[grepl(":", lines, fixed = TRUE)]
  keys <- trimws(sub(":.*$", "", lines))
  values <- lapply(trimws(sub("^[^:]*:", "", lines)), function(value) {
    items <- trimws(strsplit(value, ",", fixed = TRUE)[[1]])
    items <- items[nzchar(items)]
    number <- suppressWarnings(as.numeric(items))
    if (length(items) == 1 && !is.na(number)) number else items
  })
  stats::setNames(values, keys)
}

suite_read_results <- function(path) {
  utils::read.csv(path, check.names = FALSE)
}

# The header comment of NNNNN-model.m. Read line by line so that an empty
# tag line never captures the next one.
suite_read_tags <- function(path) {
  lines <- readLines(path, warn = FALSE)
  field <- function(key) {
    line <- grep(paste0("^\\s*", key, ":"), lines, value = TRUE)
    if (length(line) == 0) {
      return(character(0))
    }
    items <- trimws(strsplit(sub("^[^:]*:", "", line[[1]]), ",")[[1]])
    items[nzchar(items)]
  }
  list(
    component = field("componentTags"),
    test = field("testTags"),
    type = field("testType"),
    packages = field("packagesPresent")
  )
}

# Q2: everything except algebraic rules, delay, fast reactions, SBML
# packages (by tag or by packagesPresent) and non-time-course tests.
suite_category <- function(tags) {
  all_tags <- c(tags$component, tags$test)
  unsupported <- any(all_tags %in% suite_unsupported_tags) ||
    any(grepl(":", all_tags, fixed = TRUE)) ||
    length(tags$packages) > 0 ||
    !identical(tags$type, "TimeCourse")
  if (unsupported) "unsupported" else "supported"
}

# |actual - expected| <= absolute + relative * |expected| for every column
# of expected; non-finite values must match exactly.
suite_compare <- function(actual, expected, absolute, relative) {
  if (
    nrow(actual) != nrow(expected) || !all(names(expected) %in% names(actual))
  ) {
    return(FALSE)
  }
  for (name in names(expected)) {
    a <- actual[[name]]
    e <- expected[[name]]
    finite <- is.finite(e)
    if (
      !identical(is.nan(a[!finite]), is.nan(e[!finite])) ||
        !isTRUE(all(a[!finite & !is.nan(e)] == e[!finite & !is.nan(e)]))
    ) {
      return(FALSE)
    }
    if (any(!is.finite(a[finite]))) {
      return(FALSE)
    }
    within <- abs(a[finite] - e[finite]) <= absolute + relative * abs(e[finite])
    if (!all(within)) {
      return(FALSE)
    }
  }
  TRUE
}

suite_case <- function(dir, id) {
  file <- function(suffix) file.path(dir, id, paste0(id, "-", suffix))
  tags <- suite_read_tags(file("model.m"))
  list(
    id = id,
    tags = tags,
    category = suite_category(tags),
    settings = suite_read_settings(file("settings.txt")),
    results = file("results.csv"),
    model = file("sbml-l3v2.xml")
  )
}

suite_has <- function(name) {
  name %in% getNamespaceExports("zusbml")
}

# One case through as much of the pipeline as exists: "pending" before the
# reader, "read" (once its math survives R -> MathML -> R and the model
# survives write and re-read) before derivation, then "pass" or "fail" for supported
# cases and "refused" or "fail" for unsupported ones. Any condition that is
# not a zusbml condition is "error".
suite_run_case <- function(case) {
  result <- function(status, message = "", level2 = "none") {
    list(
      id = case$id,
      category = case$category,
      status = status,
      message = message,
      level2 = level2
    )
  }
  if (!suite_has("sbml_read")) {
    return(result("pending"))
  }
  outcome <- tryCatch(
    {
      sbml_read <- getExportedValue("zusbml", "sbml_read")
      model <- sbml_read(case$model)
      "read"
    },
    zusbml_unsupported = function(cnd) cnd,
    error = function(cnd) cnd
  )
  if (inherits(outcome, "zusbml_unsupported")) {
    status <- if (case$category == "unsupported") "refused" else "fail"
    return(result(status, conditionMessage(outcome)))
  }
  if (inherits(outcome, "condition")) {
    status <- if (any(startsWith(class(outcome), "zusbml_"))) {
      "fail"
    } else {
      "error"
    }
    return(result(status, conditionMessage(outcome)))
  }
  drift <- suite_math_drift(model)
  if (length(drift) > 0) {
    return(result("fail", paste("MathML round trip changed:", drift[[1]])))
  }
  if (suite_has("sbml_write")) {
    written <- getExportedValue("zusbml", "sbml_write")(model)
    back <- tryCatch(sbml_read(written), error = function(cnd) cnd)
    if (inherits(back, "condition")) {
      return(result("fail", paste("written document:", conditionMessage(back))))
    }
    if (!identical(back, model)) {
      return(result("fail", "read, write, read changed the model"))
    }
  }
  if (!suite_has("sbml_odes")) {
    return(result(outcome))
  }
  eq <- tryCatch(
    getExportedValue("zusbml", "sbml_odes")(model),
    error = function(cnd) cnd
  )
  if (inherits(eq, "zusbml_unsupported")) {
    status <- if (case$category == "unsupported") "refused" else "fail"
    return(result(status, conditionMessage(eq)))
  }
  if (inherits(eq, "condition")) {
    status <- if (inherits(eq, "zusbml_error")) "fail" else "error"
    return(result(status, conditionMessage(eq)))
  }
  if (case$category == "unsupported") {
    return(result("fail", "derived a model the suite marks unsupported"))
  }
  # Solver failures are the case failing, not the harness erring.
  actual <- tryCatch(
    suite_simulate(model, eq, case$settings),
    error = function(cnd) cnd
  )
  if (inherits(actual, "condition")) {
    return(result("fail", paste("simulation:", conditionMessage(actual))))
  }
  expected <- suite_read_results(case$results)
  names(expected) <- trimws(names(expected))
  # The first column is time, whatever it is called ("time", "Time"); a
  # model may also have a variable named time.
  names(expected)[[1]] <- ".time"
  ok <- suite_compare(
    actual,
    expected,
    absolute = case$settings$absolute,
    relative = case$settings$relative
  )
  if (!ok) {
    return(result("fail", "outside the tolerances"))
  }
  result("pass", level2 = suite_level2(case, expected))
}

# The case's Level 2 Version 5 file, when it has one, must simulate to the
# same results as its Level 3 file (design section 6.4): "pass", "fail: ..."
# or "none".
suite_level2 <- function(case, expected) {
  path <- sub("-sbml-l3v2.xml$", "-sbml-l2v5.xml", case$model)
  if (!file.exists(path)) {
    return("none")
  }
  outcome <- tryCatch(
    {
      model <- sbml_read(path)
      eq <- getExportedValue("zusbml", "sbml_odes")(model)
      actual <- suite_simulate(model, eq, case$settings)
      suite_compare(
        actual,
        expected,
        absolute = case$settings$absolute,
        relative = case$settings$relative
      )
    },
    error = function(cnd) paste("fail:", conditionMessage(cnd))
  )
  if (isTRUE(outcome)) {
    "pass"
  } else if (isFALSE(outcome)) {
    "fail: tolerances"
  } else {
    outcome
  }
}

# Simulate derived equations on the case's time grid with deSolve, and
# report each variable as the amount or concentration the settings ask for.
suite_simulate <- function(model, eq, settings) {
  vars <- settings$variables
  species <- eq$species
  compartments <- species$compartment[species$id %in% vars]
  outputs <- unique(c(vars, compartments))
  f <- getExportedValue("zusbml", "sbml_derivs")(model, outputs)
  times <- seq(
    settings$start,
    settings$start + settings$duration,
    length.out = settings$steps + 1
  )
  if (nrow(eq$states) == 0) {
    values <- matrix(
      vapply(
        times,
        function(t) f(t, numeric(0), attr(f, "parms"))[[2]],
        numeric(length(outputs))
      ),
      ncol = length(outputs),
      byrow = TRUE,
      dimnames = list(NULL, outputs)
    )
  } else {
    sim <- deSolve::ode(
      y = attr(f, "y0"),
      times = times,
      func = f,
      parms = attr(f, "parms"),
      method = "lsodar",
      rtol = 1e-10,
      atol = 1e-12,
      rootfunc = attr(f, "rootfunc"),
      events = attr(f, "events")
    )
    if (nrow(sim) != length(times)) {
      stop("the solver stopped at time ", sim[nrow(sim), 1])
    }
    values <- sim[, outputs, drop = FALSE]
  }
  out <- data.frame(.time = times)
  for (v in vars) {
    value <- values[, v]
    row <- species[species$id == v, ]
    if (nrow(row) == 1) {
      size <- values[, row$compartment]
      if (v %in% settings$amount && !row$is_amount) {
        value <- value * size
      } else if (v %in% settings$concentration && row$is_amount) {
        value <- value / size
      }
    }
    out[[v]] <- value
  }
  out
}

# Every expression in a model.
suite_model_math <- function(m) {
  laws <- lapply(m$reactions$kinetic_law, function(k) k$math)
  parts <- unlist(
    lapply(c("trigger", "priority", "delay"), function(p) {
      lapply(m$events[[p]], function(x) x$math)
    }),
    recursive = FALSE
  )
  assignments <- unlist(
    lapply(m$events$assignments, function(a) a$math),
    recursive = FALSE
  )
  all <- c(
    m$functions$math,
    m$initial_assignments$math,
    m$rules$math,
    m$constraints$math,
    laws,
    parts,
    assignments
  )
  Filter(Negate(is.null), all)
}

# Expressions that do not survive R -> MathML -> R unchanged, deparsed.
suite_math_drift <- function(m) {
  r_to_mathml <- utils::getFromNamespace("r_to_mathml", "zusbml")
  mathml_to_r <- utils::getFromNamespace("mathml_to_r", "zusbml")
  model_ids <- utils::getFromNamespace("model_ids", "zusbml")
  ids <- model_ids(m)
  drift <- character(0)
  for (expr in suite_model_math(m)) {
    text <- r_to_mathml(expr, ids)
    back <- mathml_to_r(zuxml::xml_root(zuxml::xml_parse(text)), ids = ids)
    if (!identical(back, expr)) {
      drift <- c(drift, paste(deparse(expr), collapse = " "))
    }
  }
  drift
}

suite_cases <- function(dir) {
  ids <- list.files(dir, pattern = "^[0-9]{5}$")
  ids[file.exists(file.path(dir, ids, paste0(ids, "-sbml-l3v2.xml")))]
}

# Cases run in parallel on ZUSBML_SUITE_CORES cores (default 1, as
# R CMD check limits tests to two); the test-suite workflow sets it.
# Stochastic cases make a run vary only in their own rows.
suite_run <- function(dir) {
  run <- function(id) as.data.frame(suite_run_case(suite_case(dir, id)))
  ids <- suite_cases(dir)
  cores <- as.integer(Sys.getenv("ZUSBML_SUITE_CORES", "1"))
  rows <- if (cores > 1 && .Platform$OS.type == "unix") {
    parallel::mclapply(ids, run, mc.cores = cores)
  } else {
    lapply(ids, run)
  }
  do.call(rbind, rows)
}

# The committed minimum counts for the current stage; they only go up.
suite_floor <- function() {
  floor <- read.dcf(test_path("suite-floor.dcf"))[1, ]
  as.list(stats::setNames(as.integer(floor), names(floor)))
}
