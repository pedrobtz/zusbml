# print() names ignored packages and handles an empty model

    Code
      print(sbml_read(doc, ignore_packages = TRUE))
    Output
      <sbml_model> SBML Level 3 Version 1
        (empty model)
        ignored packages: fbc

# print() shows level, version and non-zero counts

    Code
      print(m)
    Output
      <sbml_model 'decay'> SBML Level 3 Version 2
        compartments  1
        species       2
        parameters    1
        reactions     1

# summary() returns the counts

    Code
      print(s)
    Output
      SBML Level 3 Version 2 model 'decay'
                functions    unit_definitions        compartments             species 
                        0                   0                   1                   2 
               parameters initial_assignments               rules         constraints 
                        1                   0                   0                   0 
                reactions              events 
                        1                   0 

