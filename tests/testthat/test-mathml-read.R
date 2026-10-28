test_that("arithmetic operators become R arithmetic", {
  expect_identical(
    from_mathml("<apply><plus/><ci>a</ci><ci>b</ci><ci>c</ci></apply>"),
    quote(a + b + c)
  )
  expect_identical(from_mathml("<apply><plus/><ci>a</ci></apply>"), quote(+a))
  expect_identical(from_mathml("<apply><plus/></apply>"), 0)
  expect_identical(
    from_mathml("<apply><times/><ci>a</ci><ci>b</ci><ci>c</ci></apply>"),
    quote(a * b * c)
  )
  expect_identical(from_mathml("<apply><times/></apply>"), 1)
  expect_identical(from_mathml("<apply><minus/><ci>a</ci></apply>"), quote(-a))
  expect_identical(
    from_mathml("<apply><minus/><ci>a</ci><ci>b</ci></apply>"),
    quote(a - b)
  )
  expect_identical(
    from_mathml("<apply><divide/><ci>a</ci><ci>b</ci></apply>"),
    quote(a / b)
  )
  expect_identical(
    from_mathml("<apply><power/><ci>a</ci><ci>b</ci></apply>"),
    quote(a^b)
  )
})

test_that("roots and logarithms honour degree and logbase", {
  expect_identical(
    from_mathml("<apply><root/><ci>x</ci></apply>"),
    quote(sqrt(x))
  )
  expect_identical(
    from_mathml("<apply><root/><degree><cn>2</cn></degree><ci>x</ci></apply>"),
    quote(sqrt(x))
  )
  expect_identical(
    from_mathml("<apply><root/><degree><cn>3</cn></degree><ci>x</ci></apply>"),
    call("^", quote(x), quote(1 / 3))
  )
  expect_identical(from_mathml("<apply><ln/><ci>x</ci></apply>"), quote(log(x)))
  expect_identical(
    from_mathml("<apply><log/><ci>x</ci></apply>"),
    quote(log10(x))
  )
  expect_identical(
    from_mathml("<apply><log/><logbase><cn>2</cn></logbase><ci>x</ci></apply>"),
    quote(log(x, 2))
  )
})

test_that("functions map to R or keep their SBML name", {
  one <- function(op) {
    from_mathml(paste0("<apply><", op, "/><ci>x</ci></apply>"))
  }

  expect_identical(one("abs"), quote(abs(x)))
  expect_identical(one("exp"), quote(exp(x)))
  expect_identical(one("floor"), quote(floor(x)))
  expect_identical(one("ceiling"), quote(ceiling(x)))
  expect_identical(one("factorial"), quote(factorial(x)))
  expect_identical(one("arcsin"), quote(asin(x)))
  expect_identical(one("arccosh"), quote(acosh(x)))
  expect_identical(one("sec"), quote(sec(x)))
  expect_identical(one("arccoth"), quote(arccoth(x)))
  expect_identical(one("not"), quote(!x))
  expect_identical(
    from_mathml("<apply><max/><ci>a</ci><ci>b</ci><ci>c</ci></apply>"),
    quote(max(a, b, c))
  )
  expect_identical(
    from_mathml("<apply><quotient/><ci>a</ci><ci>b</ci></apply>"),
    quote(quotient(a, b))
  )
  expect_identical(
    from_mathml("<apply><implies/><ci>a</ci><ci>b</ci></apply>"),
    quote(implies(a, b))
  )
})

test_that("relations chain and logical operators take any arity", {
  expect_identical(
    from_mathml("<apply><lt/><ci>a</ci><ci>b</ci><ci>c</ci></apply>"),
    quote(a < b & b < c)
  )
  expect_identical(
    from_mathml("<apply><neq/><ci>a</ci><ci>b</ci></apply>"),
    quote(a != b)
  )
  expect_identical(
    from_mathml("<apply><and/><ci>a</ci><ci>b</ci><ci>c</ci></apply>"),
    quote(a & b & c)
  )
  expect_identical(from_mathml("<apply><and/></apply>"), TRUE)
  expect_identical(from_mathml("<apply><or/></apply>"), FALSE)
  expect_identical(
    from_mathml("<apply><or/><ci>a</ci></apply>"),
    quote(a | FALSE)
  )
  expect_identical(
    from_mathml("<apply><and/><ci>a</ci></apply>"),
    quote(a & TRUE)
  )
  expect_identical(
    from_mathml("<apply><xor/><ci>a</ci></apply>"),
    quote(xor(a, FALSE))
  )
  expect_identical(
    from_mathml("<apply><times/><ci>a</ci></apply>"),
    quote(a * 1)
  )
  expect_identical(
    from_mathml("<apply><xor/><ci>a</ci><ci>b</ci><ci>c</ci></apply>"),
    quote(xor(xor(a, b), c))
  )
})

test_that("numbers honour their cn type", {
  expect_identical(from_mathml("<cn> 2.5 </cn>"), 2.5)
  expect_identical(from_mathml('<cn type="integer">3</cn>'), 3)
  expect_identical(
    from_mathml('<cn type="e-notation">1.5<sep/>-3</cn>'),
    1.5e-3
  )
  expect_identical(from_mathml('<cn type="rational">1<sep/>4</cn>'), 0.25)
  expect_identical(from_mathml("<cn>INF</cn>"), Inf)
})

test_that("constants and names read", {
  expect_identical(from_mathml("<ci> k1 </ci>"), quote(k1))
  expect_identical(from_mathml("<true/>"), TRUE)
  expect_identical(from_mathml("<false/>"), FALSE)
  expect_identical(from_mathml("<notanumber/>"), NaN)
  expect_identical(from_mathml("<infinity/>"), Inf)
  expect_identical(from_mathml("<pi/>"), quote(pi))
  expect_identical(from_mathml("<exponentiale/>"), quote(exp(1)))
})

test_that("csymbols become time, the Avogadro constant, rateOf and delay", {
  url <- function(x) {
    paste0('definitionURL="http://www.sbml.org/sbml/symbols/', x, '"')
  }

  expect_identical(
    from_mathml(paste0("<csymbol ", url("time"), ">t</csymbol>")),
    quote(time)
  )
  expect_identical(
    from_mathml(paste0("<csymbol ", url("avogadro"), ">N</csymbol>")),
    6.02214179e23
  )
  expect_identical(
    from_mathml(paste0(
      "<apply><csymbol ",
      url("rateOf"),
      ">r</csymbol><ci>x</ci></apply>"
    )),
    quote(rateOf(x))
  )
  expect_identical(
    from_mathml(paste0(
      "<apply><csymbol ",
      url("delay"),
      ">d</csymbol><ci>x</ci><cn>1</cn></apply>"
    )),
    quote(delay(x, 1))
  )
})

test_that("piecewise becomes nested if, with NaN when nothing matches", {
  expect_identical(
    from_mathml(paste0(
      "<piecewise><piece><cn>1</cn><ci>a</ci></piece>",
      "<piece><cn>2</cn><ci>b</ci></piece><otherwise><cn>3</cn></otherwise></piecewise>"
    )),
    str2lang("if (a) 1 else if (b) 2 else 3")
  )
  expect_identical(
    from_mathml("<piecewise><piece><cn>1</cn><ci>a</ci></piece></piecewise>"),
    quote(if (a) 1 else NaN)
  )
})

test_that("lambdas become function expressions and calls stay calls", {
  lambda <- from_mathml(paste0(
    "<lambda><bvar><ci>x</ci></bvar><bvar><ci>y</ci></bvar>",
    "<apply><times/><ci>x</ci><ci>y</ci></apply></lambda>"
  ))

  expect_identical(lambda, quote(function(x, y) x * y))
  expect_identical(
    from_mathml("<apply><ci>f</ci><ci>a</ci><cn>2</cn></apply>"),
    quote(f(a, 2))
  )
})

test_that("semantics reads its first child", {
  expect_identical(
    from_mathml("<semantics><ci>x</ci><annotation>y</annotation></semantics>"),
    quote(x)
  )
})

test_that("an empty math element is NULL", {
  expect_null(from_mathml(""))
})

test_that("MathML outside the SBML subset is a parse error", {
  expect_error(
    from_mathml("<apply><sum/><ci>x</ci></apply>"),
    class = "zusbml_parse_error"
  )
  expect_error(from_mathml("<matrix/>"), class = "zusbml_parse_error")
  expect_error(
    from_mathml('<csymbol definitionURL="urn:other">x</csymbol>'),
    class = "zusbml_parse_error"
  )
  expect_error(
    from_mathml('<cn type="complex">1<sep/>2</cn>'),
    class = "zusbml_parse_error"
  )
  expect_error(from_mathml("<cn>abc</cn>"), class = "zusbml_parse_error")
  expect_error(
    from_mathml('<cn base="16">FF</cn>'),
    class = "zusbml_parse_error"
  )
  expect_error(
    from_mathml("<ci>a</ci><ci>b</ci>"),
    class = "zusbml_parse_error"
  )
})

test_that("arity mistakes are parse errors", {
  expect_error(
    from_mathml("<apply><divide/><ci>a</ci></apply>"),
    class = "zusbml_parse_error"
  )
  expect_error(
    from_mathml("<apply><minus/></apply>"),
    class = "zusbml_parse_error"
  )
  expect_error(
    from_mathml("<apply><eq/><ci>a</ci></apply>"),
    class = "zusbml_parse_error"
  )
  expect_error(
    from_mathml("<apply><max/></apply>"),
    class = "zusbml_parse_error"
  )
  expect_error(
    from_mathml("<apply><ln/></apply>"),
    class = "zusbml_parse_error"
  )
  expect_error(
    from_mathml(
      '<apply><csymbol definitionURL="http://www.sbml.org/sbml/symbols/rateOf">r</csymbol></apply>'
    ),
    class = "zusbml_parse_error"
  )
  expect_error(
    from_mathml(
      '<csymbol definitionURL="http://www.sbml.org/sbml/symbols/delay">d</csymbol>'
    ),
    class = "zusbml_parse_error"
  )
  expect_error(
    from_mathml("<apply><abs/><degree><cn>2</cn></degree><ci>x</ci></apply>"),
    class = "zusbml_parse_error"
  )
})

test_that("a parse error in math names its path in the document", {
  doc <- sbml_doc(paste0(
    '<listOfRules><assignmentRule variable="a">',
    '<math xmlns="http://www.w3.org/1998/Math/MathML"><apply><sum/></apply></math>',
    "</assignmentRule></listOfRules>"
  ))

  err <- expect_error(sbml_read(doc), class = "zusbml_parse_error")
  expect_equal(err$path, "sbml/model/listOfRules/assignmentRule[1]/math/apply")
  expect_equal(err$section, "3.4")
})
