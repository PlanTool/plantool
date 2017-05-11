// ********************************************************************
// * CPT, version 1.0 sep. 2004                                       *
// * file: static-conflicts.cl                                        *
// * Copyright (©) Vincent Vidal, 2003-2004                           *
// ********************************************************************

// ------------------  File Overview  ---------------------------------
// *   Part 1: Global variables                                       *
// *   Part 2: I/O utilities                                          *
// *   Part 3: Counters                                               *
// *   Part 4: Arrays of booleans                                     *
// *   Part 5: Session                                                *
// --------------------------------------------------------------------

// ********************************************************************
// *   Part 1: Global variables                                       *
// ********************************************************************

VERB :: true
MEMORY_VERIF :: false
PRETTY :: true

// Search strategies
SUPPORT_BRANCHING :: 1
CONFLICT_BRANCHING :: 2

resolution_strategies :: list(
  "support branching",
  "conflict/support branching")

// Distances
H0 :: 0
H1 :: 1
H2 :: 2

// Dichotomy
NO_DIC :: 0
MIN_DIC :: 1
DIFF_DIC :: 2
MAX_DIC :: 3

// Conflict type
ACTUAL_CONF :: 0
EXTENDED_CONF :: 1
POSSIBLE_CONF :: 2

// Verbosity levels
POP_WORLD:integer :: 1
PUSH_WORLD:integer :: 1
DECO:integer :: 1
SUPPORT_CHOICE:integer :: 1
PRODUCER_CHOICE:integer :: 1
CONFLICT_CHOICE:integer :: 1
PRODUCER_USE:integer :: 2
PRODUCER_EXCLUDE:integer :: 3
ACTION_PRECEDENCE:integer :: 4
CAUSAL_PRECEDENCE:integer :: 4
CONFLICT:integer :: 5
UPDATE_INF:integer :: 5
UPDATE_SUP:integer :: 5

[memoryVerification(x:integer, y:integer, s:string) =>
   #if MEMORY_VERIF (
     if (x >= y) (
       printf("\n\nMemory error : increase ~A.\n\n", s), 
       exit(1)))]


// ********************************************************************
// *   Part 2: I/O utilities                                          *
// ********************************************************************

BUFFER :: make_string(256, ' ')

claire/endl :: char!(10)
begl :: char!(13)

[fflush() : void -> flush(system.cout)]

[read_string(p:port) : string -> 
  let n := 0, c := getc(p) in (
      while (c = ' ' | c = endl) c := getc(p),
      while (c != endl) (BUFFER[n :+ 1] := c, c := getc(p)),
      substring(BUFFER, 1, n))]

[read_int(p:port) : integer -> 
  let n := 0, c := getc(p) in (
      while (c = ' ' | c = endl) c := getc(p),
      while (c != ' ' & c != endl) (n := n * 10 + (integer!(c) - 48), c := getc(p)),
      n)]

[write_int(n:integer, p:port) : void ->
  let nb := string!(n) in (
      for i in (1 .. length(nb)) putc(nb[i], p),
      putc(' ', p))]

[format_time(t:integer) : string => 
  let st := string!(t) in (
      print_in_string(),
      if (t < 10) princ("0.00")
      else (
	if (t < 1000) princ("0."),
	if (t < 100) princ("0"),
	for i in (1 .. length(st) - 1) (
	  princ(st[i]),
	  if (i = length(st) - 3) princ("."))),
      end_of_string())]

[beginMonitor(message:string) : void =>
  #if PRETTY (printf("~A~A ", message, make_string(30 - length(message), '.')), time_set(), fflush())]

[endMonitor() : void =>
  #if PRETTY (printf("done : ~A\n", format_time(time_get())), fflush())]

[restartMonitor(message:string) : void =>
  endMonitor(),
  beginMonitor(message)]

// ********************************************************************
// *   Part 3: Counters                                               *
// ********************************************************************

Counter <: Ephemeral(
  nb:integer = 0,
  old:integer = 0)

[creset(c:Counter) : void => #if PRETTY c.nb := 0]
[cfix(c:Counter) : void => #if PRETTY c.old := c.nb]
[cdiff(c:Counter) : integer => #if PRETTY c.nb - c.old else 0]
[cget(c:Counter) : integer => #if PRETTY c.nb else 0]
[cold(c:Counter) : integer => #if PRETTY c.old else 0]
[cset(c:Counter, n:integer) : void => #if PRETTY c.nb := n]
[cinc(c:Counter, n:integer) : void => #if PRETTY (c.nb :+ n)]
[cinc(c:Counter) : void => #if PRETTY cinc(c, 1)]


// ********************************************************************
// *   Part 4: Arrays of booleans                                     *
// ********************************************************************
/*
BoolArray <: ephemeral_object(chunks:list[integer])

[makeBoolArray(nb:integer) : BoolArray ->
  BoolArray(chunks = make_list(nb / 30 + 1, integer, 0))]

[setTrue(t:BoolArray, i:integer) : void =>
  let a := i / 30 + 1 in (
      t.chunks[a] := (t.chunks[a] or (1 << mod(i - 1, 30))))]

[isTrue(t:BoolArray, i:integer) : boolean =>
  not(t.chunks[i / 30 + 1][mod(i - 1, 30)])]
*/
BoolArray <: ephemeral_object(chunks:list[boolean])

[makeBoolArray(nb:integer) : BoolArray ->
  BoolArray(chunks = make_list(nb, boolean, true))]

[setTrue(t:BoolArray, i:integer) : void =>
  t.chunks[i] := false]

[isTrue(t:BoolArray, i:integer) : boolean =>
  t.chunks[i]]


// ********************************************************************
// *   Part 5: Session                                                *
// ********************************************************************

Session <: Ephemeral(
  canonicity:boolean = false,
  already_used_actions:boolean = false,
  conflicts:integer = EXTENDED_CONF,
  dichotomy:integer = MIN_DIC,
  distances:integer = H1,
  facts:string = "",
  init_heuristic:integer = H2,
  initial_bound:Rational = zero,
  instance_only:boolean = false,
  interactive:boolean = false,
  landmarks:boolean = false,
  memory:list[integer] = list<integer>(10000, 63, 200, 63),
  mutex_sets:boolean = true,
  operators:string = "",
  print_domains:boolean = false,
  print_actions:boolean = false,
  print_events:boolean = false,
  propagate_inactive_causals:boolean = false,
  propagate_inactive_threats:boolean = true,
  propagate_causals:boolean = true,
  propagate_mutex:boolean = true,
  read_distances:boolean = false,
  relevance:boolean  = true,
  strategy:integer = CONFLICT_BRANCHING,
  write_distances:boolean = false)
  
[makeSession(params:list[string]) : Session ->
  let s := Session(), i := 0, nb := length(params) in (
      while (i < nb) (
	i :+ 1,
	case params[i] (
	  {"-o", "--operators"} s.operators := params[i :+ 1],
	  {"-f", "--facts"} s.facts := params[i :+ 1],
	  {"-dh0", "--no-distances"} s.distances := H0,
	  {"-dh1", "--h1-distances"} s.distances := H1,
	  {"-dh2", "--h2-distances"} s.distances := H2,
	  {"-ih0", "--no-init-heuristic"} s.init_heuristic := H0,
	  {"-ih1", "--h1-init-heuristic"} s.init_heuristic := H1,
	  {"-ih2", "--h2-init-heuristic"} s.init_heuristic := H2,
	  {"-rd", "--read-distances"} s.read_distances := true,
	  {"-wd", "--write-distances"} s.write_distances := true,
	  {"-cb", "--conflict-branching"} s.strategy := CONFLICT_BRANCHING,
	  {"-sb", "--support-branching"} s.strategy := SUPPORT_BRANCHING,
	  {"-ac", "--actual-conflicts"} s.conflicts := ACTUAL_CONF,
	  {"-ec", "--extended-conflicts"} s.conflicts := EXTENDED_CONF,
	  {"-pc", "--possible-conflicts"} s.conflicts := POSSIBLE_CONF,
	  {"-au", "--already-used"} s.already_used_actions := true,
	  {"-b", "--initial-bound"} s.initial_bound := stringToRational(params[i :+ 1]),
	  {"-d0", "--no-dichotomy"} s.dichotomy := NO_DIC,
	  {"-d1", "--min-dichotomy"} s.dichotomy := MIN_DIC,
	  {"-d2", "--diff-dichotomy"} s.dichotomy := DIFF_DIC,
	  {"-d3", "--max-dichotomy"} s.dichotomy := MAX_DIC,
	  {"-nr", "--no-relevance"} s.relevance := false,
	  {"-ca", "--canonicity"} s.canonicity := true,
	  {"-nms", "--no-mutex-sets"} s.mutex_sets := false,
	  {"-m", "--memory"} (for j in (1 .. 4) (s.memory[j] := integer!(params[i :+ 1]))),
	  {"-npc", "--no-propagate-causals"} s.propagate_causals := false,
	  {"-npt", "--no-prop-inactive-threats"} s.propagate_inactive_threats := false,
	  {"-pic", "--prop-inactive-causals"} s.propagate_inactive_causals := true,
	  {"-npm", "--no-prop-mutex"} s.propagate_mutex := false,
	  {"-s"} i :+ 2,
	  {"-i", "--interactive"} s.interactive := true,
	  {"-q", "--instance-only"} s.instance_only := true,
	  {"-ul", "--use-landmarks"} s.landmarks := true,
	  {"-v", "--verbosity"} system.verbose := integer!(params[i :+ 1]),
	  {"-pd", "--print-domains"} s.print_domains := true,
	  {"-pa", "--print-actions"} s.print_actions := true,
	  {"-pe", "--print-events"} s.print_events := true,
	  {"-vl", "--verbosity-levels"} (
	    POP_WORLD := 100, PUSH_WORLD := 100, DECO := 100, SUPPORT_CHOICE := 100, 
	    PRODUCER_CHOICE := 100, CONFLICT_CHOICE := 100, PRODUCER_USE := 100, 
	    PRODUCER_EXCLUDE := 100, ACTION_PRECEDENCE := 100, CAUSAL_PRECEDENCE := 100,
	    UPDATE_INF := 100, UPDATE_SUP := 100,
	    while ((i :+ 1) <= nb & params[i][1] != '-') (
	      case (params[i]) (
		{"po", "pop_world"} POP_WORLD := verbose(),
		{"pu", "push_world"} PUSH_WORLD := verbose(),
		{"de", "deco"} DECO := verbose(),
		{"sc", "support_choice"} SUPPORT_CHOICE := verbose(),
		{"pc", "producer_choice"} PRODUCER_CHOICE := verbose(),
		{"cc", "conflict_choice"} CONFLICT_CHOICE := verbose(),
		{"pu", "producer_use"} PRODUCER_USE := verbose(),
		{"pe", "producer_exclude"} PRODUCER_EXCLUDE := verbose(),
		{"ap", "action_precedence"} ACTION_PRECEDENCE := verbose(),
		{"cp", "causal_precedence"} CAUSAL_PRECEDENCE := verbose(),
		{"co", "conflict"} CONFLICT := verbose(),
		{"ui", "update_inf"} UPDATE_INF := verbose(),
		{"us", "update_sup"} UPDATE_SUP := verbose(),
		any (printf("\nUnknown trace parameter: ~A\n", params[i]), usage(), exit(0)))),
	    i :- 1),
	  any (usage(), printf("Unknown option: ~A\n\n", params[i]), exit(0)))),
      if (s.read_distances | s.write_distances) s.distances := H2,
      if (s.operators = "" | s.facts = "") (usage(), exit(0)),
      s)]

[usage() ->
   printf("
Usage:    cpt [-s <a> <b>] -o <operator file> -f <problem file> [Options]
   
  operator and problem files         (typed/untyped) PDDL files

  -s <a> <b>                         sets memory values (default 7 11) for 
                                     claire, with:
                                       <a>: space for allocation (2^<a>)
                                       <b>: space for world stack (2^<b>)
Options:

Distances:
  -h0,  --no-distances               disables distances
  -h1,  --h1-distances               enables h1 distances (default)
  -h2,  --h1-distances               enables h2 distances
  -rd,  --read-distances             reads h2 distances from cache file named 
                                     '.<ops>.<pb>'
  -wd,  --write-distances            writes h2 distances to cache file named
                                     '.<ops>.<pb>'

Branching:
  -cb,  --conflict-branching         branches on conflicts, then supports, then
                                     mutexes (default)
  -sb,  --support-branching          branches on supports, then time points,
                                     then mutexes
  -ac,  --actual-conflicts           branches on actual conflicts
  -ec,  --extended-conflicts         branches on extended actual conflicts
                                      (default)
  -pc,  --possible-conflicts         branches on possible conflicts

Bounds:
  -b,   --initial-bound <x>          sets initial bound to (at least) x 
                                     (default x=0)
  -d0,  --no-dichotomy               no dichotomy for temporal planning
  -d1,  --min-dichotomy              dichotomy with increment equal to min 
                                     duration
  -d2,  --diff-dichotomy             dichotomy with increment equal to max
                                     duration minus min duration
  -d3,  --max-dichotomy             dichotomy with increment equal to max
                                     duration

Propagations:
  -npc, --no-propagate-causals       disables propagations on causal link 
                                     protection
  -npt, --no-prop-inactive-threats   disables propagations to threats in 
                                     undetermined state
  -npm, --no-prop-mutex              disables propagations on mutex orderings
  -pic, --prop-inactive-causals      enables propagations from causals in 
                                     undetermined state

Other:
  -nr,  --no-relevance               disables simple relevance computation
  -ca,  --canonicity                 searches optimal canonical plan
  -nms, --no-mutex-sets              disables the use of mutex sets
  -m,   --memory <a> <b> <c> <d>     sets memory values (default 10000 63 200 
                                     63), with:
                                       <a>: max constraints in the problem
                                       <b>: max constraints by variable
                                       <c>: max actions in the plan
                                       <d>: max ordered threats by causal link

")]
