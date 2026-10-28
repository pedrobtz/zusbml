# sbml_to_rxode2() writes constants, initial values, assignments and rates

    Code
      cat(sbml_to_rxode2(m))
    Output
      # decay, written by zusbml from SBML Level 3 Version 2
      C = 1;
      k1 = 1;
      S1(0) = 1;
      S2(0) = 0;
      R1 = C * k1 * S1;
      d/dt(S1) = -R1/C;
      d/dt(S2) = R1/C;

