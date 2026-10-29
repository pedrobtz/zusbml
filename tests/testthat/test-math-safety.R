test_that("math cannot call a function the document does not define", {
  # A harmless stand-in for system(): the document must be refused on read.
  doc <- model_doc(
    parameters = par("p", constant = FALSE),
    initial = paste0(
      '<initialAssignment symbol="p"><math xmlns="http://www.w3.org/1998/Math/MathML">',
      "<apply><plus/><cn>1</cn><apply><ci>Sys.getenv</ci><cn>0</cn></apply></apply>",
      "</math></initialAssignment>"
    )
  )

  err <- expect_error(sbml_read(doc), class = "zusbml_parse_error")
  expect_equal(err$section, "3.4")
})

test_that("derived math is evaluated without access to base R", {
  math_env <- utils::getFromNamespace("math_env", "zusbml")
  env <- new.env(parent = math_env)

  expect_error(eval(quote(Sys.getenv("HOME")), env), "could not find function")
  expect_error(eval(quote(T), env), "not found")
  expect_equal(eval(quote(sqrt(4) + pi), env), 2 + pi)
})

test_that("a name the model does not define is a derive error", {
  m <- sbml_read(model_doc(
    species = sp("A", amount = 1, only_substance = TRUE),
    reactions = rxn("r", quote(k * T * A), reactants = "A"),
    parameters = par("k", 1)
  ))

  err <- expect_error(sbml_odes(m), class = "zusbml_derive_error")
  expect_equal(err$ids, "T")
})

test_that("a function definition named like a built-in is refused", {
  doc <- model_doc(
    functions = paste0(
      '<functionDefinition id="exp">',
      mml(quote(function(x) 2 * x)),
      "</functionDefinition>"
    )
  )

  err <- expect_error(sbml_read(doc), class = "zusbml_unsupported")
  expect_equal(err$id, "exp")
})

test_that("a function definition whose math is not a lambda is a parse error", {
  doc <- model_doc(
    functions = paste0(
      '<functionDefinition id="f">',
      mml(1),
      "</functionDefinition>"
    )
  )

  err <- expect_error(sbml_read(doc), class = "zusbml_parse_error")
  expect_equal(err$section, "4.3")
})

test_that("a function called without math is a derive error", {
  m <- sbml_read(model_doc(
    functions = '<functionDefinition id="f"/>',
    parameters = par("p", constant = FALSE),
    rules = paste0(
      '<assignmentRule variable="p"><math xmlns="http://www.w3.org/1998/Math/MathML">',
      "<apply><ci>f</ci><cn>1</cn></apply></math></assignmentRule>"
    )
  ))

  err <- expect_error(sbml_odes(m), class = "zusbml_derive_error")
  expect_equal(err$ids, "f")
})

test_that("inlining nested function definitions is bounded", {
  # f1(x) = x + x and f_k(x) = f_{k-1}(f_{k-1}(x)) double at each level:
  # 30 levels would be 2^30 terms from a document of a few kilobytes.
  defs <- paste0(
    '<functionDefinition id="f1">',
    mml(quote(function(x) x + x)),
    "</functionDefinition>"
  )
  for (k in 2:30) {
    body <- call(paste0("f", k - 1), call(paste0("f", k - 1), quote(x)))
    defs <- paste0(
      defs,
      '<functionDefinition id="f',
      k,
      '"><math xmlns="http://www.w3.org/1998/Math/MathML">',
      "<lambda><bvar><ci>x</ci></bvar>",
      "<apply><ci>f",
      k - 1,
      "</ci><apply><ci>f",
      k - 1,
      "</ci><ci>x</ci></apply></apply>",
      "</lambda></math></functionDefinition>"
    )
  }
  m <- sbml_read(model_doc(
    functions = defs,
    parameters = par("p", constant = FALSE),
    rules = paste0(
      '<assignmentRule variable="p"><math xmlns="http://www.w3.org/1998/Math/MathML">',
      "<apply><ci>f30</ci><cn>1</cn></apply></math></assignmentRule>"
    )
  ))

  err <- expect_error(sbml_odes(m), class = "zusbml_derive_error")
  expect_match(err$ids, "^f[0-9]+$")
})

test_that("a symbol named like a function it uses is not a self-cycle", {
  eq <- odes(
    parameters = paste0(par("max", constant = FALSE), par("a", 1), par("b", 2)),
    rules = rule("assignment", "max", quote(max(a, b)))
  )

  expect_identical(eq$assignments$max, quote(max(a, b)))
})

test_that("many calls together are held to one inlining budget", {
  defs <- paste0(
    '<functionDefinition id="f0">',
    mml(quote(function(x) x)),
    "</functionDefinition>"
  )
  for (k in 1:13) {
    g <- as.name(paste0("f", k - 1))
    defs <- paste0(
      defs,
      '<functionDefinition id="f',
      k,
      '">',
      mml(call(
        "function",
        as.pairlist(alist(x = )),
        call(
          "+",
          call(as.character(g), quote(x)),
          call(as.character(g), quote(x))
        )
      )),
      "</functionDefinition>"
    )
  }
  ids <- paste0("p", 1:20)
  m <- sbml_read(model_doc(
    functions = defs,
    parameters = paste0(
      par("k", 1),
      paste0(vapply(ids, par, "", constant = FALSE), collapse = "")
    ),
    rules = paste0(
      vapply(ids, function(id) rule("assignment", id, quote(f13(k))), ""),
      collapse = ""
    )
  ))

  expect_error(sbml_odes(m), class = "zusbml_derive_error")
})

test_that("a lambda cannot call its bound variable", {
  defs <- paste0(
    '<functionDefinition id="f">',
    mml(quote(function(x) x)),
    "</functionDefinition>",
    '<functionDefinition id="call2"><math xmlns="http://www.w3.org/1998/Math/MathML">',
    "<lambda><bvar><ci>f</ci></bvar><bvar><ci>x</ci></bvar>",
    "<apply><ci>f</ci><ci>x</ci></apply></lambda></math></functionDefinition>"
  )
  doc <- model_doc(functions = defs, parameters = par("k", 1))

  expect_error(sbml_read(doc), class = "zusbml_parse_error")
})

test_that("derivation refuses calls to anything but operators", {
  m <- sbml_read(model_doc(
    parameters = paste0(par("k", 1), par("p", constant = FALSE)),
    rules = rule("assignment", "p", quote(k))
  ))
  m$rules$math[[1]] <- quote(environment(k))

  err <- expect_error(sbml_odes(m), class = "zusbml_derive_error")
  expect_identical(err$ids, "p")
  found <- sbml_check(m)
  expect_identical(found$id[found$rule == "unknown_reference"], "environment")
})
