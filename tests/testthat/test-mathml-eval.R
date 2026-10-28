test_that("quotient truncates toward zero and rem takes the dividend's sign", {
  expect_equal(eval_math(quote(quotient(9, 2))), 4)
  expect_equal(eval_math(quote(quotient(-9, 2))), -4)
  expect_equal(eval_math(quote(rem(9, 2))), 1)
  expect_equal(eval_math(quote(rem(-9, 2))), -1)
})

test_that("implies and the reciprocal functions evaluate", {
  expect_false(eval_math(quote(implies(TRUE, FALSE))))
  expect_true(eval_math(quote(implies(FALSE, FALSE))))
  expect_equal(eval_math(quote(sec(x)), x = 0.3), 1 / cos(0.3))
  expect_equal(eval_math(quote(csc(x)), x = 0.3), 1 / sin(0.3))
  expect_equal(eval_math(quote(cot(x)), x = 0.3), 1 / tan(0.3))
  expect_equal(eval_math(quote(sech(x)), x = 0.3), 1 / cosh(0.3))
  expect_equal(eval_math(quote(csch(x)), x = 0.3), 1 / sinh(0.3))
  expect_equal(eval_math(quote(coth(x)), x = 0.3), 1 / tanh(0.3))
  expect_equal(eval_math(quote(arcsec(x)), x = 2), acos(0.5))
  expect_equal(eval_math(quote(arccsc(x)), x = 2), asin(0.5))
  expect_equal(eval_math(quote(arccot(x)), x = 2), atan(0.5))
  expect_equal(eval_math(quote(arcsech(x)), x = 0.5), acosh(2))
  expect_equal(eval_math(quote(arccsch(x)), x = 2), asinh(0.5))
  expect_equal(eval_math(quote(arccoth(x)), x = 2), atanh(0.5))
})

test_that("sbml_functions() turns lambdas into R functions", {
  m <- sbml_read(sbml_doc(paste0(
    '<listOfFunctionDefinitions><functionDefinition id="f">',
    '<math xmlns="http://www.w3.org/1998/Math/MathML"><lambda>',
    "<bvar><ci>x</ci></bvar><apply><quotient/><ci>x</ci><cn>2</cn></apply>",
    "</lambda></math></functionDefinition></listOfFunctionDefinitions>"
  )))

  f <- sbml_functions(m)$fun[[1]]

  expect_equal(f(9), 4)
})

test_that("sbml_math() shows expressions as R code", {
  m <- sbml_read(test_path("fixtures", "suite", "00000", "00000-sbml-l3v2.xml"))
  law <- sbml_reactions(m)$kinetic_law[[1]]

  expect_equal(sbml_math(law), "C * k1 * S1")
  expect_equal(sbml_math(law$math), "C * k1 * S1")
  expect_equal(sbml_math(list(law, NULL)), c("C * k1 * S1", NA))
})
