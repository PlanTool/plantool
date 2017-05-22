Usage: 
>>> import hsp
>>> argv = ['hsp', '-o', 'domain.pddl', '-f', 'problem.pddl']
>>> hsp.run(argv)


Instructions about arguments:

OPTIONS   DESCRIPTIONS

-o  <str>    operator file name
-f  <str>    fact file name

-a  <str>    Algorithm, either 'bfs' or 'gbfs'.
-d  <str>    Search direction, either 'forward' or 'backward'.
-h  <str>    Heuristic function, one of 'h1plus', 'h1max', 'h2plus', 'h2max'.

-v  <num>    Verbose level >= 0 (default is 1).
-w <float>   Float to weight the heuristic component of the cost 
