# Usage for GP-CSP
```python
>>> import gpcsp
>>> argv = ['gpcsp', '-o', 'domain.pddl', '-f', 'problem.pddl']
>>> gpcsp.run(argv)
```

## Instructions about arguments:

### OPTIONS   DESCRIPTIONS
```bash
          -h               for this list
          -o <op file>     to specify operator file
          -f <fact file>   to specify fact file
          -t <integer>     to specify a fixed number of time steps
          -i <info level>  to specify info level 1 or 2 (default is 0)
          -O <option list> to specify options you want
          -M <integer>     to specify alternative max nodes in a time step
                            (default is 256)
          -d               give default values to everything not specified
          -csp             to run the CSP solver
          -nopddl          to take graphplan type problem description
          -ns <integer>    to specify the maximum size of nogoods to learn
          -nr <integer>    to specify the relevance-k value
          -cutoff <long>   to specify the cutoff limit for CSP solver
          -ldc             to specify the use *ldc* var-order when solving the CSP
          -dlc             to specify the use of *dlc* var-order when solving the CSP
          -switch          switch to the other var-order when over the cutoff-limit
```
### EXAMPLES
```bash
  Example1: gpcsp -o -nopddl fixit_ops -f fixit_facts1 -O IL -d
            Run original graphplan with original input format (not pddl).
  Example2: gpcsp -csp -ldc -cutoff 50000 -o a_ops.pddl -f a_facts.pddl
            Run GP-CSP with *ldc* variable ordering with cutoff-limit of 50000.
  Example3: gpcsp -csp -ns 100 -nr 5 -o fixit_ops.pddl -f fixit_ops.pddl
            Run GP-CSP with default variable ordering (dcl), with the maximum
            nogood size of 100, and will prune all irrelevant nogoods of size>5.
  Example4: gpcsp -csp -switch -cutoff 10000 -o ......
            Run GP-CSP with the cutoff-limit of 10000. Then switch from default
            var-ordering to ldc var-ordering.
  Example5: gpcsp -csp -ldc -cutoff 100 -switch -o ....
            Similar to Example4, but switch from *ldc* var-ordering to default
            var-ordering
```