# a parse error names the element, attribute and section

    Code
      sbml_read(doc)
    Condition
      Error:
      ! Missing required attribute at sbml/model/listOfParameters/parameter[1], attribute 'constant' (SBML L3V2 4.7).

# an unsupported package is named

    Code
      sbml_read(doc)
    Condition
      Error:
      ! SBML Level 3 package 'comp' not supported (at sbml). Use `ignore_packages = TRUE` to read the core model only.

# an algebraic rule is refused in derivation

    Code
      sbml_odes(m)
    Condition
      Error:
      ! Algebraic rules make the model a DAE, which sbml_odes() does not derive.

# an assignment cycle names its ids

    Code
      sbml_odes(m)
    Condition
      Error:
      ! The assignment rules for a, b depend on each other in a cycle.

# a write error names the R call

    Code
      r_to_mathml(quote(trunc(x)))
    Condition
      Error:
      ! The R function `trunc` has no SBML MathML equivalent.

# an argument error names the argument

    Code
      sbml_read(1)
    Condition
      Error:
      ! `x` must be a path, a URL, XML text or a raw vector.

---

    Code
      sbml_odes(list())
    Condition
      Error:
      ! `m` must be an sbml_model, as returned by sbml_read().

# a parse error in nested math names its path

    Code
      sbml_read(doc)
    Condition
      Error:
      ! <ci> name '.x' is not a valid SBML identifier at sbml/model/listOfRules/assignmentRule[1]/math/apply/ci (SBML L3V2 3.4).

