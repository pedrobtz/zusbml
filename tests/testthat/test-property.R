test_that("random reaction networks derive to their generator's equations", {
  withr::local_seed(20261028)

  for (i in seq_len(25)) {
    net <- random_network()
    m <- sbml_read(net$doc)
    written <- sbml_read(sbml_write(m))
    f <- sbml_derivs(written)

    expect_identical(written, m, info = paste("network", i))
    expect_setequal(names(attr(f, "y0")), net$species)
    expect_equal(attr(f, "y0")[net$species], net$y0, info = paste("network", i))
    for (trial in 1:3) {
      y <- stats::setNames(
        stats::runif(length(net$species), 0.1, 2),
        net$species
      )
      dy <- f(0, y[names(attr(f, "y0"))], attr(f, "parms"))[[1]]
      expect_equal(
        unname(dy),
        unname(net$rhs(y)[names(attr(f, "y0"))]),
        tolerance = 1e-12,
        info = paste("network", i, "trial", trial)
      )
    }
  }
})
