# Usage for HSP: 
```python
>>> import hsp
>>> argv = ['hsp', '-o', 'domain.pddl', '-f', 'problem.pddl']
>>> hsp.run(argv)
```

## Instructions about arguments:

### OPTIONS   DESCRIPTIONS
```bash
-o  <str>    operator file name
-f  <str>    fact file name
-r  <str>    output file name

-a  <str>    Algorithm, either 'bfs' or 'gbfs' (default: 'gbfs')
-d  <str>    Search direction, either 'forward' or 'backward' (default: 'backward')
-h  <str>    Heuristic function, one of 'h1plus', 'h1max', 'h2plus', 'h2max' (default: 'h1plus')

-v  <num>    Verbose level >= 0 (default: 1)
-w <float>   Float to weight the heuristic component of the cost (default: 5.0)
```
