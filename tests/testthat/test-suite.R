# Every Level 3 Version 2 case of the SBML Test Suite. Skips unless the suite
# is downloaded (tools/fetch-test-suite.sh); the test-suite workflow requires
# it. Counts may only go up: raise suite-floor.dcf as stages land.
test_that("the SBML Test Suite meets the committed floors", {
  dir <- suite_dir()
  report <- suite_run(dir)

  out <- Sys.getenv("ZUSBML_SUITE_REPORT")
  if (nzchar(out)) {
    utils::write.csv(report, out, row.names = FALSE)
  }

  errors <- report[report$status == "error", ]
  expect_equal(
    nrow(errors),
    0,
    info = paste(
      utils::head(paste(errors$id, errors$message), 10),
      collapse = "\n"
    )
  )

  floor <- suite_floor()
  expect_gte(sum(report$status %in% c("read", "pass", "fail")), floor$read)
  expect_gte(sum(report$status == "pass"), floor$pass)
  expect_gte(sum(report$status == "refused"), floor$refused)
})
