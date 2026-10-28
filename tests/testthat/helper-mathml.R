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
