Usage for SATplan:
>>> import satplan
>>> argv=['bb','-o','domain.pddl','-f','problem.pddl','-l','5','-G','0','-C','2']
>>> satplan.run(argv)


Instructions about arguments:

OPTIONS   DESCRIPTIONS

-p  <str>    path for operator and fact file
-o  <str>    operator file name
-f  <str>    fact file name

-l  <num>    goal layer for CNF

-G <0 or 1>  (0) create CNF output or (1) build final solution
-b  <str>    CNF output file name
-t <0 or 1>  (1) CNF output includes only unary/binary clauses - others ignored
-S  <str>    Input Solution File Name (only when -G 1 is used)
-F  <str>    Final Output Solution File Name (only when -G 1 is used)
-V  <str>    Variables File Name - list all variables (only when -G 1 is used)
-C           CNF formula output (preset: -1); at layer <-l>
      0      none
      1      action-based
      2      gp-style action-based
      3      gp-based
      4      thin gp-based

