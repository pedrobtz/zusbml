# MathML text to R, through the package's reader of <math> elements.
# `functions` are the function-definition ids calls may name.
from_mathml <- function(inner, functions = character(0)) {
  text <- paste0(
    '<math xmlns="http://www.w3.org/1998/Math/MathML">',
    inner,
    "</math>"
  )
  mathml_to_r <- utils::getFromNamespace("mathml_to_r", "zusbml")
  mathml_to_r(
    zuxml::xml_root(zuxml::xml_parse(text)),
    functions = functions
  )
}

# R expression to MathML text and back.
round_trip <- function(expr, functions = character(0)) {
  r_to_mathml <- utils::getFromNamespace("r_to_mathml", "zusbml")
  mathml_to_r <- utils::getFromNamespace("mathml_to_r", "zusbml")
  mathml_to_r(
    zuxml::xml_root(zuxml::xml_parse(r_to_mathml(expr))),
    functions = functions
  )
}

eval_math <- function(expr, ...) {
  env <- list2env(
    list(...),
    parent = utils::getFromNamespace("math_env", "zusbml")
  )
  eval(expr, env)
}

# The expected R name of every MathML operator, written out here rather than
# taken from math_ops: one table drives both directions, so a round trip
# cannot catch a wrong row.
expected_ops <- function() {
  c(
    plus = "+",
    times = "*",
    minus = "-",
    divide = "/",
    power = "^",
    abs = "abs",
    exp = "exp",
    floor = "floor",
    ceiling = "ceiling",
    factorial = "factorial",
    max = "max",
    min = "min",
    quotient = "quotient",
    rem = "rem",
    eq = "==",
    neq = "!=",
    gt = ">",
    lt = "<",
    geq = ">=",
    leq = "<=",
    and = "&",
    or = "|",
    xor = "xor",
    not = "!",
    implies = "implies",
    sin = "sin",
    cos = "cos",
    tan = "tan",
    sec = "sec",
    csc = "csc",
    cot = "cot",
    sinh = "sinh",
    cosh = "cosh",
    tanh = "tanh",
    sech = "sech",
    csch = "csch",
    coth = "coth",
    arcsin = "asin",
    arccos = "acos",
    arctan = "atan",
    arcsec = "arcsec",
    arccsc = "arccsc",
    arccot = "arccot",
    arcsinh = "asinh",
    arccosh = "acosh",
    arctanh = "atanh",
    arcsech = "arcsech",
    arccsch = "arccsch",
    arccoth = "arccoth"
  )
}
