(*

category:      Test
synopsis:      Fixture written for zusbml: S1 decays into S2 at rate 1.
componentTags: Compartment, Species, Reaction, Parameter
testTags:      Amount, Concentration
testType:      TimeCourse
levels:        3.2
generatedBy:   Analytic

S1 -> S2 at rate k1 * S1, k1 = 1, S1(0) = 1, S2(0) = 0, in a unit
compartment, so S1(t) = exp(-t) and S2(t) = 1 - exp(-t).

*)
