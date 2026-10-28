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
# reader, "read" before derivation, then "pass" or "fail" for supported
# cases and "refused" or "fail" for unsupported ones. Any condition that is
# not a zusbml condition is "error".
suite_run_case <- function(case) {
  result <- function(status, message = "") {
    list(
      id = case$id,
      category = case$category,
      status = status,
      message = message
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
  result(outcome)
}

suite_cases <- function(dir) {
  ids <- list.files(dir, pattern = "^[0-9]{5}$")
  ids[file.exists(file.path(dir, ids, paste0(ids, "-sbml-l3v2.xml")))]
}

suite_run <- function(dir) {
  rows <- lapply(suite_cases(dir), function(id) {
    as.data.frame(suite_run_case(suite_case(dir, id)))
  })
  do.call(rbind, rows)
}

# The committed minimum counts for the current stage; they only go up.
suite_floor <- function() {
  floor <- read.dcf(test_path("suite-floor.dcf"))[1, ]
  as.list(stats::setNames(as.integer(floor), names(floor)))
}
