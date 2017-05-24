# Usage for SGPlan:
## First,run:
```python
   >>> import translate
   >>> argv = ['translate','domain.pddl','problem.pddl']
   >>> translate.main(argv)
```
```bash
   The translator will will write its result to a file called
   "output.sas", which serves as an input to the next phase, knowledge
   compilation. The translator also writes a file called
   "test.groups", which is some sort of translation key (see
   "sas-format.txt" in the documentation directory mentioned above).
   This second file is not needed by the planner, but might help you
   understand what the translated task looks like. It also writes a
   file called "all.groups" which is needed by the landmark heuristic.
```

## Second, run:
```python
   >>> import preprocess
   >>> argv = ['preprocess','< output.sas']
   >>> preprocess.oldmain(argv)
```
```bash
   This will run the knowledge compilation component, writing its
   output to the file aptly named "output".
```
## Finally, run:
```python
   >>> import search
   >>> argv = ['search','f','< output']
   >>> search.oldmain(argv)
```
```bash
   note 
   This runs the search component of the planner. On success, it will
   write a file called "sas_plan" containing the plan.

OPTIONS DESCRIPTIONS
   l:  Use the landmark heuristic.
   L:  Use preferred operators of the landmark heuristic.
   f:  Use the FF heuristic.
   F:  Use helpful actions ("preferred operators" of the FF
       heuristic).
```
