test_that("malformed MathML structures are parse errors", {
  snippets <- c(
    empty_apply = "<apply/>",
    empty_semantics = "<semantics/>",
    real_two_parts = "<cn>1<sep/>2</cn>",
    enotation_one_part = '<cn type="e-notation">1</cn>',
    rational_one_part = '<cn type="rational">1</cn>',
    applied_time = paste0(
      '<apply><csymbol definitionURL="http://www.sbml.org/sbml/symbols/time">t</csymbol>',
      "<cn>1</cn></apply>"
    ),
    applied_avogadro = paste0(
      '<apply><csymbol definitionURL="http://www.sbml.org/sbml/symbols/avogadro">a</csymbol>',
      "<cn>1</cn></apply>"
    ),
    piecewise_stray = "<piecewise><ci>x</ci></piecewise>",
    piecewise_two_otherwise = paste0(
      "<piecewise><otherwise><cn>1</cn></otherwise>",
      "<otherwise><cn>2</cn></otherwise></piecewise>"
    ),
    piece_one_part = "<piecewise><piece><cn>1</cn></piece></piecewise>",
    otherwise_two_parts = "<piecewise><otherwise><cn>1</cn><cn>2</cn></otherwise></piecewise>",
    lambda_no_body = "<lambda><bvar><ci>x</ci></bvar></lambda>",
    lambda_bvar_after_body = "<lambda><ci>x</ci><bvar><ci>x</ci></bvar></lambda>",
    bvar_without_ci = "<lambda><bvar><cn>1</cn></bvar><ci>x</ci></lambda>",
    root_two_args = "<apply><root/><ci>x</ci><ci>y</ci></apply>",
    degree_two_parts = "<apply><root/><degree><cn>1</cn><cn>2</cn></degree><ci>x</ci></apply>",
    foreign_head = '<apply><x:plus xmlns:x="urn:x"/><cn>1</cn></apply>'
  )

  for (name in names(snippets)) {
    expect_error(
      from_mathml(snippets[[name]]),
      class = "zusbml_parse_error",
      info = name
    )
  }
})
