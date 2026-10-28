# Every Level 3 Version 2 case of the SBML Test Suite. Skips unless the suite
# is downloaded (tools/fetch-test-suite.sh); the test-suite workflow requires
# it, split into shards (ZUSBML_SUITE_SHARD). Every supported case passes and
# every other one is refused; a run over the whole suite also checks the
# counts in suite-floor.dcf, which may only go up.
test_that("every supported case passes and every other one is refused", {
  dir <- suite_dir()
  report <- suite_run(dir)

  out <- Sys.getenv("ZUSBML_SUITE_REPORT")
  if (nzchar(out)) {
    utils::write.csv(report, out, row.names = FALSE)
  }

  show <- function(rows) {
    paste(
      utils::head(paste(rows$id, rows$status, rows$message), 10),
      collapse = "\n"
    )
  }
  supported <- report[report$category == "supported", ]
  unsupported <- report[report$category == "unsupported", ]
  expect_true(
    all(supported$status == "pass"),
    info = show(supported[supported$status != "pass", ])
  )
  expect_true(
    all(unsupported$status == "refused"),
    info = show(unsupported[unsupported$status != "refused", ])
  )
  failing_level2 <- report[startsWith(report$level2, "fail"), ]
  expect_equal(nrow(failing_level2), 0, info = show(failing_level2))

  if (suite_shard()[[2]] == 1) {
    floor <- suite_floor()
    expect_gte(sum(report$status %in% c("read", "pass", "fail")), floor$read)
    expect_gte(sum(report$status == "pass"), floor$pass)
    expect_gte(sum(report$status == "refused"), floor$refused)
    expect_gte(sum(report$level2 == "pass"), floor$level2)
  }
})
