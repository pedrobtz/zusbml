test_that("every MathML operator maps to its expected R function", {
  ops <- expected_ops()
  math_ops <- utils::getFromNamespace("math_ops", "zusbml")
  unary <- c(
    "abs",
    "exp",
    "floor",
    "ceiling",
    "factorial",
    "not",
    "sin",
    "cos",
    "tan",
    "sec",
    "csc",
    "cot",
    "sinh",
    "cosh",
    "tanh",
    "sech",
    "csch",
    "coth",
    "arcsin",
    "arccos",
    "arctan",
    "arcsec",
    "arccsc",
    "arccot",
    "arcsinh",
    "arccosh",
    "arctanh",
    "arcsech",
    "arccsch",
    "arccoth"
  )

  expect_setequal(names(ops), math_ops$mathml)
  for (op in names(ops)) {
    args <- if (op %in% unary) "<ci>x</ci>" else "<ci>x</ci><ci>y</ci>"
    expr <- from_mathml(paste0("<apply><", op, "/>", args, "</apply>"))
    expect_identical(as.character(expr[[1]]), ops[[op]], info = op)
  }
})
