# MathML text to R, through the package's reader of <math> elements.
from_mathml <- function(inner) {
  text <- paste0(
    '<math xmlns="http://www.w3.org/1998/Math/MathML">',
    inner,
    "</math>"
  )
  mathml_to_r <- utils::getFromNamespace("mathml_to_r", "zusbml")
  mathml_to_r(zuxml::xml_root(zuxml::xml_parse(text)))
}

# R expression to MathML text and back.
round_trip <- function(expr) {
  r_to_mathml <- utils::getFromNamespace("r_to_mathml", "zusbml")
  mathml_to_r <- utils::getFromNamespace("mathml_to_r", "zusbml")
  mathml_to_r(zuxml::xml_root(zuxml::xml_parse(r_to_mathml(expr))))
}

eval_math <- function(expr, ...) {
  env <- list2env(
    list(...),
    parent = utils::getFromNamespace("math_env", "zusbml")
  )
  eval(expr, env)
}
