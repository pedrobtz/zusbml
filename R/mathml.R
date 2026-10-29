# Content MathML to R language objects and back (design section 6.2, D6).
# One table drives both directions: each MathML operator maps to the R
# function its <apply> becomes, and the printer reads the table backwards.
# Operators without an exact base R equivalent keep their SBML name and are
# defined in math_env, the environment derived equations evaluate in.

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

csymbol_url <- c(
  time = "http://www.sbml.org/sbml/symbols/time",
  avogadro = "http://www.sbml.org/sbml/symbols/avogadro",
  delay = "http://www.sbml.org/sbml/symbols/delay",
  rateOf = "http://www.sbml.org/sbml/symbols/rateOf"
)

# The value SBML Level 3 fixes for the avogadro csymbol.
avogadro <- 6.02214179e23

# MathML operator -> R function name, and how many arguments it takes: "n"
# any number (folded into nested binary calls where R's is binary), "minus"
# one or two, "rel" two or more (chained), a number exactly that many.
# One row per operator, so each MathML name, its R function and its arity
# can be checked together: this table drives both directions of the
# mapping.
math_ops <- as.data.frame(
  do.call(
    rbind,
    list(
      c(mathml = "plus", r = "+", arity = "n"),
      c(mathml = "times", r = "*", arity = "n"),
      c(mathml = "minus", r = "-", arity = "minus"),
      c(mathml = "divide", r = "/", arity = "2"),
      c(mathml = "power", r = "^", arity = "2"),
      c(mathml = "abs", r = "abs", arity = "1"),
      c(mathml = "exp", r = "exp", arity = "1"),
      c(mathml = "floor", r = "floor", arity = "1"),
      c(mathml = "ceiling", r = "ceiling", arity = "1"),
      c(mathml = "factorial", r = "factorial", arity = "1"),
      c(mathml = "max", r = "max", arity = "n"),
      c(mathml = "min", r = "min", arity = "n"),
      c(mathml = "quotient", r = "quotient", arity = "2"),
      c(mathml = "rem", r = "rem", arity = "2"),
      c(mathml = "eq", r = "==", arity = "rel"),
      c(mathml = "neq", r = "!=", arity = "2"),
      c(mathml = "gt", r = ">", arity = "rel"),
      c(mathml = "lt", r = "<", arity = "rel"),
      c(mathml = "geq", r = ">=", arity = "rel"),
      c(mathml = "leq", r = "<=", arity = "rel"),
      c(mathml = "and", r = "&", arity = "n"),
      c(mathml = "or", r = "|", arity = "n"),
      c(mathml = "xor", r = "xor", arity = "n"),
      c(mathml = "not", r = "!", arity = "1"),
      c(mathml = "implies", r = "implies", arity = "2"),
      c(mathml = "sin", r = "sin", arity = "1"),
      c(mathml = "cos", r = "cos", arity = "1"),
      c(mathml = "tan", r = "tan", arity = "1"),
      c(mathml = "sec", r = "sec", arity = "1"),
      c(mathml = "csc", r = "csc", arity = "1"),
      c(mathml = "cot", r = "cot", arity = "1"),
      c(mathml = "sinh", r = "sinh", arity = "1"),
      c(mathml = "cosh", r = "cosh", arity = "1"),
      c(mathml = "tanh", r = "tanh", arity = "1"),
      c(mathml = "sech", r = "sech", arity = "1"),
      c(mathml = "csch", r = "csch", arity = "1"),
      c(mathml = "coth", r = "coth", arity = "1"),
      c(mathml = "arcsin", r = "asin", arity = "1"),
      c(mathml = "arccos", r = "acos", arity = "1"),
      c(mathml = "arctan", r = "atan", arity = "1"),
      c(mathml = "arcsec", r = "arcsec", arity = "1"),
      c(mathml = "arccsc", r = "arccsc", arity = "1"),
      c(mathml = "arccot", r = "arccot", arity = "1"),
      c(mathml = "arcsinh", r = "asinh", arity = "1"),
      c(mathml = "arccosh", r = "acosh", arity = "1"),
      c(mathml = "arctanh", r = "atanh", arity = "1"),
      c(mathml = "arcsech", r = "arcsech", arity = "1"),
      c(mathml = "arccsch", r = "arccsch", arity = "1"),
      c(mathml = "arccoth", r = "arccoth", arity = "1")
    )
  ),
  stringsAsFactors = FALSE
)

# The table as named vectors, for lookups that run once per MathML node.
math_op_r <- stats::setNames(math_ops$r, math_ops$mathml)
math_op_arity <- stats::setNames(math_ops$arity, math_ops$mathml)
math_op_mathml <- stats::setNames(math_ops$mathml, math_ops$r)

# The functions model math calls once function definitions are inlined: the
# table's and those the reader builds (piecewise, root, log, the csymbols).
math_heads <- c(math_ops$r, "if", "sqrt", "log", "log10", "rateOf", "delay")

# The only base R functions model math can reach: those the MathML table and
# the compiled derivative functions produce. math_base holds them, and its
# parent is the empty environment, so a document cannot call system(),
# file.remove() or anything else, and a name that is not a model symbol is an
# error rather than a base R object such as `T` (design D20).
math_base_names <- c(
  "+",
  "-",
  "*",
  "/",
  "^",
  "==",
  "!=",
  ">",
  "<",
  ">=",
  "<=",
  "&",
  "|",
  "!",
  "&&",
  "||",
  "xor",
  "if",
  "(",
  "{",
  "<-",
  "[[",
  "function",
  "c",
  "list",
  "as.numeric",
  "environment",
  "abs",
  "exp",
  "floor",
  "ceiling",
  "trunc",
  "factorial",
  "max",
  "min",
  "sqrt",
  "log",
  "log10",
  "log2",
  "sin",
  "cos",
  "tan",
  "sinh",
  "cosh",
  "tanh",
  "asin",
  "acos",
  "atan",
  "asinh",
  "acosh",
  "atanh"
)

math_base <- local({
  env <- new.env(parent = emptyenv())
  for (name in math_base_names) {
    assign(name, get(name, envir = baseenv()), envir = env)
  }
  env$pi <- pi
  env
})

# Definitions for the operators kept under their SBML names. quotient and
# rem follow MathML 2.0: the quotient truncates toward zero and the
# remainder takes the sign of the dividend.
math_env <- local({
  env <- new.env(parent = math_base)
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

# The ids of the document being read or written; see mathml_to_r().
math_context <- new.env(parent = emptyenv())
math_context$ids <- character(0)
math_context$functions <- character(0)
math_context$bvars <- character(0)
math_context$time <- quote(time)

time_name <- function(ids = math_context$ids) {
  if ("time" %in% ids) quote(.time) else quote(time)
}

# ---- MathML to R ---------------------------------------------------------

# `node` is a <math> element; the result is an R expression (a call, a name,
# a number or a logical), a `function` call for a lambda, or NULL for an
# empty <math>.
#
# `ids` are the ids the document declares. The time csymbol is the name
# `time` and <pi/> the name `pi`, unless the document declares an id of that
# name: then they are `.time` and the number pi, which no SBML id can shadow
# (ids cannot contain "."). See design D16.
mathml_to_r <- function(
  node,
  path = "math",
  file = NA_character_,
  ids = character(0),
  functions = character(0)
) {
  math_from_tree(flat_tree(node), 1L, path, file, ids, functions)
}

# The math element i of a flat tree as an R expression; the reader passes
# the whole document's tree.
math_from_tree <- function(t, i, path, file, ids, functions) {
  old <- as.list(math_context)
  math_context$ids <- ids
  math_context$functions <- functions
  math_context$bvars <- character(0)
  math_context$time <- time_name(ids)
  on.exit(list2env(old, math_context))
  kids <- t$kids[[i]]
  if (length(kids) == 0) {
    return(NULL)
  }
  if (length(kids) > 1) {
    math_error("<math> holds more than one expression", path, file)
  }
  mathml_node(t, kids[[1]], path, file)
}

# A subtree as flat vectors, read with a handful of vectorized zuxml calls:
# a call per node from R costs more than the conversion itself. Element i
# has name local[i], namespace ns[i], attributes attrs[[i]], child elements
# kids[[i]] (indices), and for <ci>, <cn> and <csymbol> its text text[i].
# Element 1 is the subtree's root; nodes[[i]] is the node itself, for the
# few cases that still need it.
flat_tree <- function(node) {
  nodes <- c(node, zuxml::xml_find(node))
  ids <- as.integer(nodes)
  n <- length(ids)
  parent <- match(as.integer(zuxml::xml_parent(nodes)), ids)
  local <- zuxml::xml_local(nodes)
  kids <- vector("list", n)
  if (n > 1) {
    by_parent <- split(seq_len(n)[-1], parent[-1])
    kids[as.integer(names(by_parent))] <- unname(by_parent)
  }
  kids[vapply(kids, is.null, logical(1))] <- list(integer(0))
  text <- rep(NA_character_, n)
  textual <- which(local %in% c("ci", "cn", "csymbol"))
  if (length(textual) > 0) {
    text[textual] <- zuxml::xml_text(nodes[textual])
  }
  # <ci> names trimmed and checked as SIds in one pass, not one per name.
  ci <- which(local == "ci")
  name <- rep(NA_character_, n)
  name[ci] <- fast_trim(text[ci])
  sid <- rep(FALSE, n)
  sid[ci] <- is_sid(name[ci])
  list(
    nodes = nodes,
    local = local,
    ns = zuxml::xml_ns(nodes),
    attrs = zuxml::xml_attrs(nodes),
    kids = kids,
    text = text,
    name = name,
    sid = sid
  )
}

math_error <- function(message, path, file) {
  abort_parse(message, path = path, section = "3.4", file = file)
}

mathml_node <- function(t, i, path, file) {
  local <- t$local[[i]]
  if (!identical(t$ns[[i]], mathml_ns)) {
    math_error(paste0("<", local, "> is not MathML"), path, file)
  }
  # Paths are only for error messages: each is passed as an unevaluated
  # argument, so it is built only if an error needs it.
  switch(
    local,
    cn = mathml_cn(t, i, paste0(path, "/", local), file),
    ci = as.name(ci_name(t, i, paste0(path, "/", local), file)),
    csymbol = mathml_csymbol(
      t,
      i,
      paste0(path, "/", local),
      file,
      applied = FALSE
    ),
    true = TRUE,
    false = FALSE,
    notanumber = NaN,
    pi = if ("pi" %in% math_context$ids) pi else quote(pi),
    infinity = Inf,
    exponentiale = quote(exp(1)),
    apply = mathml_apply(t, i, paste0(path, "/", local), file),
    piecewise = mathml_piecewise(t, i, paste0(path, "/", local), file),
    lambda = mathml_lambda(t, i, paste0(path, "/", local), file),
    semantics = {
      first <- t$kids[[i]]
      if (length(first) == 0) {
        math_error("Empty <semantics>", paste0(path, "/", local), file)
      }
      mathml_node(t, first[[1]], paste0(path, "/", local), file)
    },
    math_error(
      paste0("MathML element <", local, "> is not allowed in SBML"),
      paste0(path, "/", local),
      file
    )
  )
}

# The name a <ci> holds, which must be an SId (section 3.1.7): derivation
# relies on names never starting with "." (design D16). flat_tree() has
# trimmed and checked it.
ci_name <- function(t, i, path, file) {
  name <- t$name[[i]]
  if (!t$sid[[i]]) {
    math_error(
      paste0("<ci> name '", name, "' is not a valid SBML identifier"),
      path,
      file
    )
  }
  name
}

mathml_cn <- function(t, i, path, file) {
  attrs <- t$attrs[[i]]
  type <- if ("type" %in% names(attrs)) attrs[["type"]] else "real"
  if ("base" %in% names(attrs) && attrs[["base"]] != "10") {
    math_error("Only base 10 numbers are supported", path, file)
  }
  texts <- if (length(t$kids[[i]]) == 0) {
    fast_trim(t$text[[i]])
  } else {
    # Parts separated by <sep/>: the text nodes, read from the node itself.
    parts <- zuxml::xml_children(t$nodes[[i]])
    trimws(zuxml::xml_text(parts[zuxml::xml_type(parts) == "text"]))
  }
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

csymbol_kind <- function(t, i, path, file) {
  url <- unname(t$attrs[[i]]["definitionURL"])
  kind <- names(csymbol_url)[match(url, csymbol_url)]
  if (is.na(kind)) {
    math_error(paste0("Unknown csymbol '", url, "'"), path, file)
  }
  kind
}

mathml_csymbol <- function(t, i, path, file, applied) {
  kind <- csymbol_kind(t, i, path, file)
  if (kind %in% c("delay", "rateOf") && !applied) {
    math_error(paste0("csymbol ", kind, " must be applied"), path, file)
  }
  if (kind %in% c("time", "avogadro") && applied) {
    math_error(paste0("csymbol ", kind, " cannot be applied"), path, file)
  }
  switch(kind, time = math_context$time, avogadro = avogadro, as.name(kind))
}

mathml_apply <- function(t, i, path, file) {
  kids <- t$kids[[i]]
  if (length(kids) == 0) {
    math_error("Empty <apply>", path, file)
  }
  head <- kids[[1]]
  head_name <- t$local[[head]]
  rest <- kids[-1]
  locals <- t$local[rest]
  qualifiers <- locals %in% c("logbase", "degree")
  args <- lapply(which(!qualifiers), function(k) {
    mathml_node(t, rest[[k]], paste0(path, "[", k + 1, "]"), file)
  })
  qualifier <- function(name) {
    q <- rest[locals == name]
    if (length(q) == 0) {
      return(NULL)
    }
    inner <- t$kids[[q[[1]]]]
    if (length(inner) != 1) {
      math_error(paste0("<", name, "> needs one expression"), path, file)
    }
    mathml_node(t, inner[[1]], paste0(path, "/", name), file)
  }

  if (head_name == "ci") {
    name <- ci_name(t, head, path, file)
    # A bound variable shadows a function definition of the same name, and
    # inlining would put its argument in call position.
    if (!name %in% math_context$functions || name %in% math_context$bvars) {
      math_error(
        paste0(
          "<apply> calls '",
          name,
          "', which is not a function definition"
        ),
        path,
        file
      )
    }
    return(as.call(c(as.name(name), args)))
  }
  if (head_name == "csymbol") {
    fn <- mathml_csymbol(t, head, path, file, applied = TRUE)
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
  if (!identical(t$ns[[head]], mathml_ns)) {
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
  if (!head_name %in% names(math_op_r)) {
    math_error(
      paste0("MathML operator <", head_name, "> is not allowed in SBML"),
      path,
      file
    )
  }
  op <- list(
    mathml = head_name,
    r = math_op_r[[head_name]],
    arity = math_op_arity[[head_name]]
  )
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
        # R's operators are binary, so a lone argument is paired with the
        # operator's identity: times(a) is a * 1, and(a) is a & TRUE.
        return(switch(
          op$mathml,
          plus = call("+", args[[1]]),
          times = call("*", args[[1]], 1),
          and = call("&", args[[1]], TRUE),
          or = call("|", args[[1]], FALSE),
          xor = call("xor", args[[1]], FALSE),
          as.call(c(as.name(fn), args))
        ))
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

mathml_piecewise <- function(t, i, path, file) {
  kids <- t$kids[[i]]
  locals <- t$local[kids]
  if (
    any(!locals %in% c("piece", "otherwise")) || sum(locals == "otherwise") > 1
  ) {
    math_error("<piecewise> holds only <piece> and one <otherwise>", path, file)
  }
  pieces <- lapply(which(locals == "piece"), function(k) {
    parts <- t$kids[[kids[[k]]]]
    if (length(parts) != 2) {
      math_error("<piece> needs a value and a condition", path, file)
    }
    here <- paste0(path, "/piece[", k, "]")
    list(
      value = mathml_node(t, parts[[1]], here, file),
      condition = mathml_node(t, parts[[2]], here, file)
    )
  })
  result <- NaN
  otherwise <- which(locals == "otherwise")
  if (length(otherwise) == 1) {
    parts <- t$kids[[kids[[otherwise]]]]
    if (length(parts) != 1) {
      math_error("<otherwise> needs one expression", path, file)
    }
    result <- mathml_node(t, parts[[1]], paste0(path, "/otherwise"), file)
  }
  for (piece in rev(pieces)) {
    result <- call("if", piece$condition, piece$value, result)
  }
  result
}

mathml_lambda <- function(t, i, path, file) {
  kids <- t$kids[[i]]
  locals <- t$local[kids]
  bvars <- which(locals == "bvar")
  body <- which(locals != "bvar")
  if (length(body) != 1 || any(bvars > body)) {
    math_error("<lambda> needs its <bvar>s and then one body", path, file)
  }
  names <- vapply(
    bvars,
    function(k) {
      ci <- t$kids[[kids[[k]]]]
      if (length(ci) != 1 || t$local[[ci]] != "ci") {
        math_error("<bvar> holds one <ci>", path, file)
      }
      ci_name(t, ci, path, file)
    },
    character(1)
  )
  formals <- rep(list(quote(expr = )), length(names))
  names(formals) <- names
  math_context$bvars <- names
  on.exit(math_context$bvars <- character(0))
  call(
    "function",
    as.pairlist(formals),
    mathml_node(t, kids[[body]], paste0(path, "/body"), file)
  )
}

# ---- R to MathML ---------------------------------------------------------

# The <math> element for an R expression, as text. A call outside the table
# is a zusbml_write_error naming it (design section 8).
r_to_mathml <- function(expr, ids = character(0)) {
  old <- as.list(math_context)
  math_context$ids <- ids
  math_context$time <- time_name(ids)
  on.exit(list2env(old, math_context))
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
    if (name == "pi" && !"pi" %in% math_context$ids) {
      return(tag("pi"))
    }
    if (identical(expr, math_context$time)) {
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
  if (name %in% names(math_op_mathml)) {
    return(apply_tag(math_op_mathml[[name]], nodes()))
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
  if (is.na(x) && !is.nan(x)) {
    abort_write("NA has no MathML equivalent.", "NA")
  }
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

# The control characters XML 1.0 does not allow at all, as a character class
# body.
xml_control <- "\x01-\x08\x0B\x0C\x0E-\x1F"

# Text escaped for XML. Characters XML 1.0 does not allow at all (most
# control characters) cannot be written.
xml_escape <- function(x) {
  # Most text needs no escaping; one check finds what does.
  special <- grepl(paste0("[&<>", xml_control, "]"), x, perl = TRUE)
  if (!any(special)) {
    return(x)
  }
  if (any(grepl(paste0("[", xml_control, "]"), x[special], perl = TRUE))) {
    abort_write(
      "Text with a control character cannot be written as XML 1.0.",
      "text"
    )
  }
  y <- x[special]
  y <- gsub("&", "&amp;", y, fixed = TRUE)
  y <- gsub("<", "&lt;", y, fixed = TRUE)
  x[special] <- gsub(">", "&gt;", y, fixed = TRUE)
  x
}
