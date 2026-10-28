# The fifty fixed BioModels (tools/biomodels.txt). Skips unless they are
# downloaded (tools/fetch-biomodels.sh); the biomodels CI job requires them.
test_that("the fixed BioModels read, round-trip and derive", {
  dir <- Sys.getenv("ZUSBML_BIOMODELS")
  required <- identical(Sys.getenv("ZUSBML_REQUIRE_BIOMODELS"), "true")
  if (!nzchar(dir) || !dir.exists(dir)) {
    if (required) {
      stop("BioModels required but ZUSBML_BIOMODELS is not set.")
    }
    skip("BioModels not downloaded (ZUSBML_BIOMODELS unset)")
  }
  list <- test_path("..", "..", "tools", "biomodels.txt")
  if (!file.exists(list)) {
    skip("tools/biomodels.txt is not in the installed tests")
  }
  ids <- grep("^#", readLines(list), value = TRUE, invert = TRUE)

  for (id in ids) {
    m <- sbml_read(file.path(dir, paste0(id, ".xml")))
    written <- sbml_read(sbml_write(m))
    expect_identical(sbml_read(sbml_write(written)), written, info = id)
    expect_type(sbml_odes(m)$rhs, "list")
    expect_equal(nrow(sbml_check(m)), 0, info = id)
  }
})
