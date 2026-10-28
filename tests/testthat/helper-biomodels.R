# The fifty fixed BioModels (tools/biomodels.txt, fetched by
# tools/fetch-biomodels.sh). Like suite_dir(): skip when they are absent,
# unless ZUSBML_REQUIRE_BIOMODELS is "true", when absence is an error.
biomodels_dir <- function() {
  dir <- Sys.getenv("ZUSBML_BIOMODELS")
  if (nzchar(dir) && dir.exists(dir)) {
    return(dir)
  }
  biomodels_missing("BioModels not downloaded (ZUSBML_BIOMODELS unset)")
}

biomodels_ids <- function() {
  list <- test_path("..", "..", "tools", "biomodels.txt")
  if (!file.exists(list)) {
    biomodels_missing("tools/biomodels.txt is not with the tests")
  }
  grep("^#", readLines(list), value = TRUE, invert = TRUE)
}

biomodels_missing <- function(reason) {
  if (identical(Sys.getenv("ZUSBML_REQUIRE_BIOMODELS"), "true")) {
    stop(structure(
      class = c("zusbml_biomodels_missing_error", "error", "condition"),
      list(message = paste("BioModels required:", reason), call = NULL)
    ))
  }
  testthat::skip(reason)
}

# A model written as Level 3 Version 2 and read back, made comparable with
# the model as read: writing changes only the level and version, drops the
# record of what Level 2 conversion dropped, and leaves out `fast`, which
# Level 3 Version 2 does not have.
biomodels_comparable <- function(m) {
  back <- sbml_read(sbml_write(m))
  back$level <- m$level
  back$version <- m$version
  back$dropped <- m$dropped
  back$reactions$fast <- m$reactions$fast
  back
}
