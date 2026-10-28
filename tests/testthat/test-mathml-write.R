test_that("every operator in the table survives a round trip", {
  exprs <- list(
    quote(a + b + c),
    quote(+a),
    quote(-a),
    quote(a - b),
    quote(a * b),
    quote(a / b),
    quote(a^b),
    quote(sqrt(x)),
    call("^", quote(x), quote(1 / 3)),
    quote(log(x)),
    quote(log10(x)),
    quote(log(x, 2)),
    quote(abs(x)),
    quote(exp(x)),
    quote(exp(1)),
    quote(floor(x)),
    quote(ceiling(x)),
    quote(factorial(x)),
    quote(max(a, b, c)),
    quote(min(a, b)),
    quote(quotient(a, b)),
    quote(rem(a, b)),
    quote(a == b),
    quote(a != b),
    quote(a > b),
    quote(a < b),
    quote(a >= b),
    quote(a <= b),
    quote(a & b),
    quote(a | b),
    quote(xor(a, b)),
    quote(!a),
    quote(implies(a, b)),
    quote(sin(x)),
    quote(sec(x)),
    quote(asin(x)),
    quote(arccoth(x)),
    quote(rateOf(x)),
    quote(delay(x, 2)),
    quote(time),
    quote(pi),
    quote(f(a, 2)),
    quote(function(x, y) x * y),
    str2lang("if (a) 1 else if (b) 2 else 3"),
    quote(if (a) 1 else NaN),
    TRUE,
    FALSE,
    NaN,
    Inf,
    0,
    -2.5
  )

  for (expr in exprs) {
    expect_identical(
      round_trip(expr),
      expr,
      info = paste(deparse(expr), collapse = "")
    )
  }
})

test_that("R-only spellings are written as their SBML equivalents", {
  expect_identical(round_trip(quote(a && b)), quote(a & b))
  expect_identical(round_trip(quote(a || b)), quote(a | b))
  expect_identical(
    round_trip(quote((a + b) * c)),
    call("*", quote(a + b), quote(c))
  )
  expect_identical(round_trip(quote(log2(x))), quote(log(x, 2)))
  expect_identical(round_trip(quote(if (a) 1)), quote(if (a) 1 else NaN))
})

test_that("-Inf is written as minus infinity and reads as that call", {
  expect_identical(round_trip(-Inf), quote(-Inf))
  expect_identical(eval(round_trip(-Inf)), -Inf)
})

test_that("numbers round-trip to the last bit", {
  numbers <- c(0.1, 1 / 3, 6.02214179e23, 1e-12, 123456.789, -7, 2^60)

  for (x in numbers) {
    expect_identical(round_trip(x), x, info = format(x, digits = 17))
  }
})

test_that("numbers at the extremes of the double range round-trip exactly", {
  # R's text-to-double conversion is correctly rounded at the extremes of
  # the range only with a long double wider than double (design D19): the
  # nold and macOS arm64 builds read .Machine$double.xmax back as Inf.
  skip_if(
    .Machine$sizeof.longdouble <= 8,
    "R parses numbers at the extremes inexactly without an 80-bit long double"
  )

  for (x in c(1e-300, .Machine$double.xmax, .Machine$double.xmin)) {
    expect_identical(round_trip(x), x, info = format(x, digits = 17))
  }
})

test_that("integers are written as integer cn and exponents as e-notation", {
  r_to_mathml <- utils::getFromNamespace("r_to_mathml", "zusbml")

  expect_match(r_to_mathml(3), '<cn type="integer">3</cn>', fixed = TRUE)
  expect_match(r_to_mathml(0.25), "<cn>0.25</cn>", fixed = TRUE)
  expect_match(
    r_to_mathml(1.5e-30),
    '<cn type="e-notation">1.5<sep/>-30</cn>',
    fixed = TRUE
  )
})

test_that("a call with no MathML equivalent is a write error", {
  r_to_mathml <- utils::getFromNamespace("r_to_mathml", "zusbml")

  err <- expect_error(
    r_to_mathml(quote(trunc(x))),
    class = "zusbml_write_error"
  )
  expect_equal(err$construct, "trunc")
  expect_error(
    r_to_mathml(quote((function(x) x)(1))),
    class = "zusbml_write_error"
  )
  expect_error(r_to_mathml("text"), class = "zusbml_write_error")
})

test_that("names are escaped in MathML text", {
  r_to_mathml <- utils::getFromNamespace("r_to_mathml", "zusbml")

  expect_match(r_to_mathml(as.name("a<b")), "<ci>a&lt;b</ci>", fixed = TRUE)
})
