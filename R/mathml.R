# Content MathML to R language objects and back (design section 6.2, D6).
# One table drives both directions: each MathML operator maps to the R
# function its <apply> becomes, and the printer reads the table backwards.
# Operators without an exact base R equivalent keep their SBML name and are
# defined in math_env, the environment derived equations evaluate in.

csymbol_url <- c(
  time = "http://www.sbml.org/sbml/symbols/time",
  avogadro = "http://www.sbml.org/sbml/symbols/avogadro",
  delay = "http://www.sbml.org/sbml/symbols/delay",
  rateOf = "http://www.sbml.org/sbml/symbols/rateOf"
)

# The value SBML Level 3 fixes for the avogadro csymbol.
avogadro <- 6.02214179e23

# MathML operator -> R function name, and how many arguments it takes:
# "n" any number (folded into nested binary calls where R's is binary),
# a number exactly that many.
math_ops <- data.frame(
  mathml = c(
    "plus",
    "times",
    "minus",
    "divide",
    "power",
    "abs",
    "exp",
    "floor",
    "ceiling",
    "factorial",
    "max",
    "min",
    "quotient",
    "rem",
    "eq",
    "neq",
    "gt",
    "lt",
    "geq",
    "leq",
    "and",
    "or",
    "xor",
    "not",
    "implies",
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
  ),
  r = c(
    "+",
    "*",
    "-",
    "/",
    "^",
    "abs",
    "exp",
    "floor",
    "ceiling",
    "factorial",
    "max",
    "min",
    "quotient",
    "rem",
    "==",
    "!=",
    ">",
    "<",
    ">=",
    "<=",
    "&",
    "|",
    "xor",
    "!",
    "implies",
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
    "asin",
    "acos",
    "atan",
    "arcsec",
    "arccsc",
    "arccot",
    "asinh",
    "acosh",
    "atanh",
    "arcsech",
    "arccsch",
    "arccoth"
  ),
  arity = c(
    "n",
    "n",
    "minus",
    "2",
    "2",
    "1",
    "1",
    "1",
    "1",
    "1",
    "n",
    "n",
    "2",
    "2",
    "rel",
    "2",
    "rel",
    "rel",
    "rel",
    "rel",
    "n",
    "n",
    "n",
    "1",
    "2",
    rep("1", 24)
  ),
  stringsAsFactors = FALSE
)

# Definitions for the operators kept under their SBML names. quotient and
# rem follow MathML 2.0: the quotient truncates toward zero and the
# remainder takes the sign of the dividend.
math_env <- local({
  env <- new.env(parent = baseenv())
  env$quotient <- function(x, y) trunc(x / y)
  env$rem <- function(x, y) x - y * trunc(x / y)
  env$implies <- function(x, y) !x | y
  env$sec <- function(x) 1 / cos(x)
  env$csc <- function(x) 1 / sin(x)
  env$cot <- function(x) 1 / tan(x)
  env$sech <- function(x) 1 / cosh(x)
  env$csch <- function(x) 1 / sinh(x)
  env$coth <- function(x) 1 / tanh(x)
  env$arcsec <- function(x) acos(1 / x)
  env$arccsc <- function(x) asin(1 / x)
  env$arccot <- function(x) atan(1 / x)
  env$arcsech <- function(x) acosh(1 / x)
  env$arccsch <- function(x) asinh(1 / x)
  env$arccoth <- function(x) atanh(1 / x)
  env
})

# ---- MathML to R ---------------------------------------------------------

# `node` is a <math> element; the result is an R expression (a call, a name,
# a number or a logical), a `function` call for a lambda, or NULL for an
# empty <math>.
mathml_to_r <- function(node, path = "math", file = NA_character_) {
  kids <- zuxml::xml_elements(node)
  if (length(kids) == 0) {
    return(NULL)
  }
  if (length(kids) > 1) {
    math_error("<math> holds more than one expression", path, file)
  }
  mathml_node(kids[[1]], path, file)
}

math_error <- function(message, path, file) {
  abort_parse(message, path = path, section = "3.4", file = file)
}

mathml_node <- function(node, path, file) {
  local <- zuxml::xml_local(node)
  if (!identical(zuxml::xml_ns(node), mathml_ns)) {
    math_error(paste0("<", local, "> is not MathML"), path, file)
  }
  here <- paste0(path, "/", local)
  switch(
    local,
    cn = mathml_cn(node, here, file),
    ci = as.name(trimws(zuxml::xml_text(node))),
    csymbol = mathml_csymbol(node, here, file, applied = FALSE),
    true = TRUE,
    false = FALSE,
    notanumber = NaN,
    pi = quote(pi),
    infinity = Inf,
    exponentiale = quote(exp(1)),
    apply = mathml_apply(node, here, file),
    piecewise = mathml_piecewise(node, here, file),
    lambda = mathml_lambda(node, here, file),
    semantics = {
      first <- zuxml::xml_elements(node)
      if (length(first) == 0) {
        math_error("Empty <semantics>", here, file)
      }
      mathml_node(first[[1]], here, file)
    },
    math_error(
      paste0("MathML element <", local, "> is not allowed in SBML"),
      here,
      file
    )
  )
}

mathml_cn <- function(node, path, file) {
  attrs <- zuxml::xml_attrs(node)[[1]]
  type <- if ("type" %in% names(attrs)) attrs[["type"]] else "real"
  if ("base" %in% names(attrs) && attrs[["base"]] != "10") {
    math_error("Only base 10 numbers are supported", path, file)
  }
  parts <- zuxml::xml_children(node)
  texts <- trimws(zuxml::xml_text(parts[zuxml::xml_type(parts) == "text"]))
  texts <- texts[nzchar(texts)]
  number <- function(x) {
    value <- parse_double(x)
    if (is.na(value) && x != "NaN") {
      math_error(paste0("Invalid number '", x, "' in <cn>"), path, file)
    }
    value
  }
  switch(
    type,
    real = ,
    integer = ,
    double = {
      if (length(texts) != 1) {
        math_error("<cn> needs one number", path, file)
      }
      number(texts)
    },
    "e-notation" = {
      if (length(texts) != 2) {
        math_error("e-notation <cn> needs two numbers", path, file)
      }
      number(paste0(texts[[1]], "e", texts[[2]]))
    },
    rational = {
      if (length(texts) != 2) {
        math_error("rational <cn> needs two numbers", path, file)
      }
      number(texts[[1]]) / number(texts[[2]])
    },
    math_error(paste0("Unknown <cn> type '", type, "'"), path, file)
  )
}

csymbol_kind <- function(node, path, file) {
  url <- zuxml::xml_attr(node, "definitionURL")
  kind <- names(csymbol_url)[match(url, csymbol_url)]
  if (is.na(kind)) {
    math_error(paste0("Unknown csymbol '", url, "'"), path, file)
  }
  kind
}

mathml_csymbol <- function(node, path, file, applied) {
  kind <- csymbol_kind(node, path, file)
  if (kind %in% c("delay", "rateOf") && !applied) {
    math_error(paste0("csymbol ", kind, " must be applied"), path, file)
  }
  if (kind %in% c("time", "avogadro") && applied) {
    math_error(paste0("csymbol ", kind, " cannot be applied"), path, file)
  }
  switch(kind, time = quote(time), avogadro = avogadro, as.name(kind))
}

mathml_apply <- function(node, path, file) {
  kids <- zuxml::xml_elements(node)
  if (length(kids) == 0) {
    math_error("Empty <apply>", path, file)
  }
  head <- kids[[1]]
  head_name <- zuxml::xml_local(head)
  rest <- kids[-1]
  locals <- zuxml::xml_local(rest)
  qualifiers <- locals %in% c("logbase", "degree")
  args <- lapply(which(!qualifiers), function(i) {
    mathml_node(rest[[i]], paste0(path, "[", i + 1, "]"), file)
  })
  qualifier <- function(name) {
    q <- rest[locals == name]
    if (length(q) == 0) {
      return(NULL)
    }
    inner <- zuxml::xml_elements(q[[1]])
    if (length(inner) != 1) {
      math_error(paste0("<", name, "> needs one expression"), path, file)
    }
    mathml_node(inner[[1]], paste0(path, "/", name), file)
  }

  if (head_name == "ci") {
    return(as.call(c(as.name(trimws(zuxml::xml_text(head))), args)))
  }
  if (head_name == "csymbol") {
    fn <- mathml_csymbol(head, path, file, applied = TRUE)
    want <- if (identical(fn, quote(delay))) 2L else 1L
    if (length(args) != want) {
      math_error(
        paste0(as.character(fn), " takes ", want, " argument(s)"),
        path,
        file
      )
    }
    return(as.call(c(fn, args)))
  }
  if (!identical(zuxml::xml_ns(head), mathml_ns)) {
    math_error(paste0("<", head_name, "> is not MathML"), path, file)
  }
  if (head_name == "root") {
    one_arg(args, "root", path, file)
    degree <- qualifier("degree")
    if (is.null(degree) || identical(degree, 2)) {
      return(call("sqrt", args[[1]]))
    }
    return(call("^", args[[1]], call("/", 1, degree)))
  }
  if (head_name %in% c("ln", "log")) {
    one_arg(args, head_name, path, file)
    if (head_name == "ln") {
      return(call("log", args[[1]]))
    }
    base <- qualifier("logbase")
    if (is.null(base) || identical(base, 10)) {
      return(call("log10", args[[1]]))
    }
    return(call("log", args[[1]], base))
  }
  if (any(qualifiers)) {
    math_error(
      paste0(
        "<",
        locals[qualifiers][[1]],
        "> is not allowed with <",
        head_name,
        ">"
      ),
      path,
      file
    )
  }
  op <- math_ops[math_ops$mathml == head_name, ]
  if (nrow(op) == 0) {
    math_error(
      paste0("MathML operator <", head_name, "> is not allowed in SBML"),
      path,
      file
    )
  }
  apply_op(op, args, path, file)
}

one_arg <- function(args, name, path, file) {
  if (length(args) != 1) {
    math_error(paste0("<", name, "> takes one argument"), path, file)
  }
}

fold <- function(fn, args) {
  Reduce(function(a, b) call(fn, a, b), args)
}

apply_op <- function(op, args, path, file) {
  fn <- op$r
  n <- length(args)
  switch(
    op$arity,
    n = {
      if (n == 0) {
        identity_value <- switch(
          op$mathml,
          plus = 0,
          times = 1,
          and = TRUE,
          or = FALSE,
          xor = FALSE,
          math_error(paste0("<", op$mathml, "> needs arguments"), path, file)
        )
        return(identity_value)
      }
      if (n == 1) {
        if (op$mathml == "plus") {
          return(call("+", args[[1]]))
        }
        return(as.call(c(as.name(fn), args)))
      }
      if (op$mathml %in% c("max", "min")) {
        return(as.call(c(as.name(fn), args)))
      }
      fold(fn, args)
    },
    minus = {
      if (n == 1) {
        return(call("-", args[[1]]))
      }
      if (n != 2) {
        math_error("<minus> takes one or two arguments", path, file)
      }
      call("-", args[[1]], args[[2]])
    },
    rel = {
      if (n < 2) {
        math_error(
          paste0("<", op$mathml, "> needs two or more arguments"),
          path,
          file
        )
      }
      pairs <- lapply(seq_len(n - 1), function(i) {
        call(fn, args[[i]], args[[i + 1]])
      })
      fold("&", pairs)
    },
    {
      want <- as.integer(op$arity)
      if (n != want) {
        math_error(
          paste0("<", op$mathml, "> takes ", want, " argument(s)"),
          path,
          file
        )
      }
      as.call(c(as.name(fn), args))
    }
  )
}

mathml_piecewise <- function(node, path, file) {
  kids <- zuxml::xml_elements(node)
  locals <- zuxml::xml_local(kids)
  if (
    any(!locals %in% c("piece", "otherwise")) || sum(locals == "otherwise") > 1
  ) {
    math_error("<piecewise> holds only <piece> and one <otherwise>", path, file)
  }
  pieces <- lapply(which(locals == "piece"), function(i) {
    parts <- zuxml::xml_elements(kids[[i]])
    if (length(parts) != 2) {
      math_error("<piece> needs a value and a condition", path, file)
    }
    here <- paste0(path, "/piece[", i, "]")
    list(
      value = mathml_node(parts[[1]], here, file),
      condition = mathml_node(parts[[2]], here, file)
    )
  })
  result <- NaN
  otherwise <- which(locals == "otherwise")
  if (length(otherwise) == 1) {
    parts <- zuxml::xml_elements(kids[[otherwise]])
    if (length(parts) != 1) {
      math_error("<otherwise> needs one expression", path, file)
    }
    result <- mathml_node(parts[[1]], paste0(path, "/otherwise"), file)
  }
  for (piece in rev(pieces)) {
    result <- call("if", piece$condition, piece$value, result)
  }
  result
}

mathml_lambda <- function(node, path, file) {
  kids <- zuxml::xml_elements(node)
  locals <- zuxml::xml_local(kids)
  bvars <- which(locals == "bvar")
  body <- which(locals != "bvar")
  if (length(body) != 1 || any(bvars > body)) {
    math_error("<lambda> needs its <bvar>s and then one body", path, file)
  }
  names <- vapply(
    bvars,
    function(i) {
      ci <- zuxml::xml_elements(kids[[i]])
      if (length(ci) != 1 || zuxml::xml_local(ci) != "ci") {
        math_error("<bvar> holds one <ci>", path, file)
      }
      trimws(zuxml::xml_text(ci))
    },
    character(1)
  )
  formals <- rep(list(quote(expr = )), length(names))
  names(formals) <- names
  call(
    "function",
    as.pairlist(formals),
    mathml_node(kids[[body]], paste0(path, "/body"), file)
  )
}

# ---- R to MathML ---------------------------------------------------------

# The <math> element for an R expression, as text. A call outside the table
# is a zusbml_write_error naming it (design section 8).
r_to_mathml <- function(expr) {
  paste0('<math xmlns="', mathml_ns, '">', r_node(expr), "</math>")
}

abort_write <- function(message, construct) {
  zusbml_abort("zusbml_write_error", message, construct = construct)
}

tag <- function(name) paste0("<", name, "/>")

apply_tag <- function(op, args) {
  paste0("<apply>", tag(op), paste(args, collapse = ""), "</apply>")
}

r_node <- function(expr) {
  if (is.null(expr)) {
    return("")
  }
  if (is.logical(expr) && length(expr) == 1 && !is.na(expr)) {
    return(tag(if (expr) "true" else "false"))
  }
  if (is.numeric(expr) && length(expr) == 1) {
    return(r_number(expr))
  }
  if (is.name(expr)) {
    name <- as.character(expr)
    if (name == "pi") {
      return(tag("pi"))
    }
    if (name == "time") {
      return(paste0(
        '<csymbol encoding="text" definitionURL="',
        csymbol_url[["time"]],
        '">time</csymbol>'
      ))
    }
    return(paste0("<ci>", xml_escape(name), "</ci>"))
  }
  if (!is.call(expr)) {
    abort_write(
      paste0("Cannot write ", class(expr)[[1]], " values as MathML."),
      class(expr)[[1]]
    )
  }
  r_call(expr)
}

r_call <- function(expr) {
  fn <- expr[[1]]
  args <- as.list(expr)[-1]
  if (!is.name(fn)) {
    abort_write(
      "Cannot write a call to a computed function as MathML.",
      paste(deparse(fn), collapse = "")
    )
  }
  name <- as.character(fn)
  n <- length(args)
  nodes <- function() vapply(args, r_node, character(1))

  if (name == "(") {
    return(r_node(args[[1]]))
  }
  if (name == "exp" && n == 1 && identical(args[[1]], 1)) {
    return(tag("exponentiale"))
  }
  if (name == "sqrt" && n == 1) {
    return(apply_tag("root", nodes()))
  }
  if (name == "log" && n == 1) {
    return(apply_tag("ln", nodes()))
  }
  if (name == "log" && n == 2) {
    return(paste0(
      "<apply><log/><logbase>",
      r_node(args[[2]]),
      "</logbase>",
      r_node(args[[1]]),
      "</apply>"
    ))
  }
  if (name %in% c("log10", "log2") && n == 1) {
    base <- if (name == "log10") 10 else 2
    return(paste0(
      "<apply><log/><logbase>",
      r_number(base),
      "</logbase>",
      r_node(args[[1]]),
      "</apply>"
    ))
  }
  if (name %in% c("&&", "||")) {
    name <- substr(name, 1, 1)
  }
  if (name == "if") {
    return(r_piecewise(expr))
  }
  if (name == "function") {
    return(r_lambda(expr))
  }
  if (name %in% c("delay", "rateOf")) {
    return(paste0(
      "<apply><csymbol encoding=\"text\" definitionURL=\"",
      csymbol_url[[name]],
      "\">",
      name,
      "</csymbol>",
      paste(nodes(), collapse = ""),
      "</apply>"
    ))
  }
  op <- math_ops[math_ops$r == name, ]
  if (nrow(op) == 1) {
    return(apply_tag(op$mathml, nodes()))
  }
  if (name %in% names(base_only_functions())) {
    abort_write(
      paste0("The R function `", name, "` has no SBML MathML equivalent."),
      name
    )
  }
  # Anything else is a call to a function definition of the model.
  paste0(
    "<apply><ci>",
    xml_escape(name),
    "</ci>",
    paste(nodes(), collapse = ""),
    "</apply>"
  )
}

# Base R functions a model's function definition could not be named after
# in practice; a call to one of these is an R-side expression, not SBML.
base_only_functions <- function() {
  fns <- c(
    "trunc",
    "round",
    "signif",
    "cumsum",
    "sum",
    "prod",
    "mean",
    "ifelse",
    "atan2",
    "expm1",
    "log1p",
    "c",
    "list",
    "paste"
  )
  stats::setNames(fns, fns)
}

r_piecewise <- function(expr) {
  pieces <- character(0)
  repeat {
    pieces <- c(
      pieces,
      paste0("<piece>", r_node(expr[[3]]), r_node(expr[[2]]), "</piece>")
    )
    rest <- if (length(expr) == 4) expr[[4]] else NaN
    if (is.call(rest) && identical(rest[[1]], as.name("if"))) {
      expr <- rest
      next
    }
    break
  }
  otherwise <- if (identical(rest, NaN)) {
    ""
  } else {
    paste0("<otherwise>", r_node(rest), "</otherwise>")
  }
  paste0("<piecewise>", paste(pieces, collapse = ""), otherwise, "</piecewise>")
}

r_lambda <- function(expr) {
  bvars <- vapply(
    names(expr[[2]]),
    function(n) {
      paste0("<bvar><ci>", xml_escape(n), "</ci></bvar>")
    },
    character(1)
  )
  paste0(
    "<lambda>",
    paste(bvars, collapse = ""),
    r_node(expr[[3]]),
    "</lambda>"
  )
}

# Shortest decimal text that reads back as the same double, at most 17
# significant digits; exponents are written as e-notation.
r_number <- function(x) {
  if (is.nan(x)) {
    return(tag("notanumber"))
  }
  if (is.infinite(x)) {
    return(if (x > 0) tag("infinity") else apply_tag("minus", tag("infinity")))
  }
  text <- format_double(x)
  if (grepl("e", text, fixed = TRUE)) {
    parts <- strsplit(text, "e", fixed = TRUE)[[1]]
    return(paste0(
      '<cn type="e-notation">',
      parts[[1]],
      "<sep/>",
      as.integer(parts[[2]]),
      "</cn>"
    ))
  }
  type <- if (x == round(x) && abs(x) < 2^53) ' type="integer"' else ""
  paste0("<cn", type, ">", text, "</cn>")
}

format_double <- function(x) {
  for (digits in 15:17) {
    text <- sprintf(paste0("%.", digits, "g"), x)
    if (as.numeric(text) == x) {
      return(text)
    }
  }
  text
}

xml_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

#' Show model math as R code
#'
#' Model math is held as R language objects: SBML's MathML operators become
#' the matching R calls (`<power/>` is `^`, `<ln/>` is `log()`,
#' `<piecewise>` is nested `if`), the `time` csymbol is the name `time`,
#' and lambdas are `function` expressions. Operators without an exact base R
#' equivalent keep their SBML name: `quotient()`, `rem()`, `implies()`,
#' `sec()` and the other reciprocal trigonometric functions, `rateOf()` and
#' `delay()`.
#'
#' @param x An expression from a model, such as `sbml_rules(m)$math[[1]]`; a
#'   part holding one in `$math`, such as `sbml_reactions(m)$kinetic_law[[1]]`;
#'   or a list of either.
#' @return A character vector with one line of R code per expression, `NA`
#'   for absent math.
#' @export
#' @examples
#' m <- sbml_read(system.file("extdata", "decay.xml", package = "zusbml"))
#' sbml_math(sbml_reactions(m)$kinetic_law[[1]])
sbml_math <- function(x) {
  one <- function(e) {
    if (is.list(e) && !is.null(names(e)) && "math" %in% names(e)) {
      e <- e$math
    }
    if (is.null(e)) {
      return(NA_character_)
    }
    paste(deparse(e, width.cutoff = 500L), collapse = " ")
  }
  if (is.list(x) && (is.null(names(x)) || !"math" %in% names(x))) {
    return(vapply(x, one, character(1)))
  }
  one(x)
}
