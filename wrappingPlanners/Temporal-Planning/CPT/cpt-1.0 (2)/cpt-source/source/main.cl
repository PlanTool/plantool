// ********************************************************************
// * CPT, version 1.0 sep. 2004                                       *
// * file: static-conflicts.cl                                        *
// * Copyright (©) Vincent Vidal, 2003-2004                           *
// ********************************************************************

// ------------------  File Overview  ---------------------------------
// *   Part 1: Main                                                   *
// *   Part 2: Solver                                                 *
// *   Part 3: Relevance Computation                                  *
// *   Part 4: Plan print                                             *
// *   Part 5: Compiler optimisation                                  *
// --------------------------------------------------------------------

// ********************************************************************
// *   Part 1: Main                                                   *
// ********************************************************************

SESSION:any :: unknown

[main(params:list[string]) : void -> 
  system.verbose := 0,
  time_set(),
  SESSION := makeSession(params),
  if not(SESSION.interactive) runPlanner(SESSION)]

[rp() : void -> runPlanner(SESSION)]

[runPlanner(s:Session) : void -> 
  let pb := makePlanningProblem(s) in (
      if (s.distances = H1) (
	beginMonitor("Computing distances"),
  	computeH1Distances(pb),
	endMonitor())
      else if (s.distances = H2) (
	if s.read_distances readDistances(pb)
	else (
	  beginMonitor("Computing distances"),
	  computeH2Distances(pb),
	  endMonitor(),
	  if s.write_distances writeDistances(pb))),
      if not(s.instance_only) (
	solve(pb),
	exit(0)))]
  

// ********************************************************************
// *   Part 2: Solver                                                 *
// ********************************************************************

[makeInitialConstraints(pb:PlanningProblem) : void ->
  setVal(pb.startAction, 0),
  for a in pb.actions post(pb, actionConstraint(a)),
  for causal in pb.causalLinks post(pb, supportConstraint(causal)),
  use(pb.startAction),

  use(pb.endAction),
  for a in pb.events (
    setVal(a, a.tinitEvent),
    use(a))]

[makePlanningSolver(pb:PlanningProblem) : Solver ->
  let
    support_branching := makeAssignOrForbidBranching(makeProducerVarHeuristic(pb), makeBestProducerSelector()),
    init_branching  := makeAssignVarBranching(makeInitTimeHeuristic(pb), makeIncValIterator()),
    conflict_branching := makeSolveConflict(),
    mutex_branching := makeSolveMutex(),
    branching := (
      case pb.session.strategy (
	{SUPPORT_BRANCHING} list(support_branching, init_branching, mutex_branching),
	{CONFLICT_BRANCHING} list(makeConflictBranching(conflict_branching, support_branching), mutex_branching ; , init_branching
				 ))),
    solver := makeGlobalSearchSolver(false, branching)
  in (
      attach(solver, pb),
      setNodeLimit(solver, MAXINT),
      setBacktrackLimit(solver, MAXINT),
      setTimeLimit(solver, MAXINT),
      solver)]
  
[solve(pb:PlanningProblem) : void -> 
  let
    solver := makePlanningSolver(pb),
    bnd := 0, dicinf := 0, dicsup := 0, dic := false,
    increment := (case pb.session.dichotomy (
		    {NO_DIC} 1,
		    {MIN_DIC} min(list{a.duration | a in list{ a in pb.actions | a.duration != 0}}),
		    {DIFF_DIC} 1 + max(list{a.duration | a in pb.actions}) - min(list{a.duration | a in list{a in pb.actions | a.duration != 0}}),
		    {MAX_DIC} max(list{a.duration | a in pb.actions})))
  in (
      // Building the initial set of constraints
      beginMonitor("Constraints creation"),
      makeInitialConstraints(pb),
      endMonitor(),

      // Initial propagation
      beginMonitor("Initial propagations"),
      try propagate(pb) catch contradiction (printf("---> No solution at unbounded propagation?!\n\n"), exit(0)),
      endMonitor(),
 
      #if PRETTY printf("\nProblem : ~S actions, ~S fluents, ~S causals\n          ~S init facts, ~S goals\n          ~A\n\n",
			length(pb.actions), length(pb.fluents), length(pb.causalLinks), 
			length(pb.startAction.add), length(pb.endAction.prec), resolution_strategies[pb.session.strategy]),
      
      bnd := max(pb.session.initial_bound.num, firstStart(pb.endAction)),
      dicinf := bnd,

      // Main loop : search and increase bound until a solution is found
      time_set(),
      while not(getFeasibility(solver)) (
	#if PRETTY (
	  time_set(), printf("Bound : ~A  ---  ", convert(pb, bnd)), fflush(),
	  cfix(pb.cptChoiceSupport),
	  cfix(pb.cptChoiceConflict),
	  cfix(pb.cptChoiceMutex)),
	while (world?() > 0) popWorld(pb),
	pushWorld(pb),
	setVal(pb.endAction, bnd),
	computeRelevance(pb, firstStart(pb.endAction)),
	run(solver),
	#if PRETTY printf("Nodes : ~S  ---  Backtracks : ~S  ---  Iteration time : ~A\n",
			  solver.limits[1].nb, solver.limits[2].nb, format_time(time_get())),
	if not(dic) (
	  if not(getFeasibility(solver)) bnd :+ increment
	  else if (increment > 1 & bnd > dicinf + 1) (
	    pb.feasible := false, 
	    dic := true, 
	    dicinf := bnd - increment, 
	    dicsup := bnd,
	    bnd := (dicinf + dicsup + 1) / 2))
	else (
	  if (not(getFeasibility(solver)) | bnd > dicinf + 1) (
	    if getFeasibility(solver) dicsup := bnd
	    else dicinf := bnd,
	    pb.feasible := false,
	    bnd := (dicinf + dicsup + 1) / 2))),
      printPlan(pb))]



// ********************************************************************
// *   Part 3: Relevance Computation                                  *
// ********************************************************************

[computeRelevance(pb:PlanningProblem, bnd:integer) : void ->
      if (pb.session.relevance) (
	for a in pb.actions a.tend := 0,
	computeActionRelevance(pb.endAction, bnd),
	for a in list{a in pb.actions | not(a.isevent)} updateSupA(a, a.tend))
      else (
	for a in list{a in pb.actions | not(a.isevent)} (
	  updateSupA(a, bnd - getDistance(a, pb.endAction) - getDuration(a))))]

[computeActionRelevance(a:Action, bnd:integer) : void ->
  if (bnd > a.tend) (
    a.tend := bnd,
    for c in a.causals (
      for a' in getProducers(c) (
	let new_bnd := bnd - getDistance(a', a) - getDuration(a') in (
	    if (firstEnd(a') + getDistance(a', a) <= lastStart(a) & firstStart(a') <= new_bnd)
	      computeActionRelevance(a', new_bnd)))))]


// ********************************************************************
// *   Part 4: Plan output                                            *
// ********************************************************************

[printPlan(pb:PlanningProblem) : void ->
  let
    search_time := max(1, time_get()),
    solver := pb.globalSearchSolver,
    steps := sort(orderActionsPlan @ Action, pb.activeActions),
    leng := 0,
    l := (#if PRETTY max(list<integer>{length(convert(pb, firstStart(a)) /+ action_to_string(a) /+ convert(pb, a.duration)) | a in steps}) + 5 else 0)
 in (
      #if PRETTY printf("\n~A\n", make_string(l, '-'))
      else printf("; Time ~A\n; SearchTime ~A\n; Makespan ~A\n; NrActions ~S\n; Nodes ~S\n",
		  format_time(time_get()), format_time(search_time),
		  convert(pb, firstStart(pb.endAction)), length(steps) - 2,
		  solver.limits[1].totNb),
      for a in steps (
	if (pb.session.print_events | (a != pb.startAction & a != pb.endAction & not(a.isevent))) (
	  if (a != pb.startAction & a != pb.endAction & not(a.isevent)) leng :+ 1,
	  printf("~A: ~A [~A]\n", convert(pb, firstStart(a)), action_to_string(a), convert(pb, a.duration)))),
      #if PRETTY (
	printf("~A\n\n", make_string(l, '-')),
	printf("Makespan : ~A\nLength : ~S\nNodes : ~S\nBacktracks : ~S\n", 
	       convert(pb, firstStart(pb.endAction)), 
	       leng, solver.limits[1].totNb, solver.limits[2].totNb),
	printf("Support choices : ~S\nConflict choices : ~S\nMutex choices : ~S\n", 
	       cget(pb.cptChoiceSupport), cget(pb.cptChoiceConflict), cget(pb.cptChoiceMutex)),
      	printf("Nodes/sec : ~A\n", (if (solver.limits[1].totNb = 0) 0 else integer!(float!(solver.limits[1].totNb) / (float!(search_time) / 1000.0)))),
	printf("Search time : ~A\nTotal time : ~A\n\n", 
	       format_time(search_time),
	       format_time(time_get()))))]

[orderActionsPlan(a1:Action, a2:Action) : boolean ->
  if (firstStart(a1) < firstStart(a2)) true
  else if (firstStart(a1) > firstStart(a2)) false
  else if (firstEnd(a1) < firstEnd(a2)) true
  else if (firstEnd(a1) > firstEnd(a2)) false
  else (a1.name <= a2.name)]

[convert(pb:PlanningProblem, n:integer) : string ->
  print_in_string(),
  n := n * pb.instance.pgcd,
  princ(n / pb.instance.ppcm),
  n := n mod pb.instance.ppcm,
  if (n != 0) (
    princ("."),
    while (n mod pb.instance.ppcm != 0) (
      n :* 10,
      princ((n / pb.instance.ppcm) mod 10))),
  end_of_string()]


// ********************************************************************
// *   Part 5: Compiler optimisation                                  *
// ********************************************************************
    
(final(Rational))
(final(Addition))
(final(Substraction))
(final(Multiplication))
(final(Division))
(final(OperationTerms))
(final(Predicate))
(final(Constant))
(final(Variabl))
(final(TransitionRule))
(final(PositiveAtom))
(final(NegativeAtom))
(final(Operator))
(final(Domain))
(final(Instance))
(final(Session))
(final(CausalLink))
(final(Action))
(final(Fluent))
(final(PlanningProblem))
(final(SupportConstraint))
(final(ActionConstraint))
(final(PrecedenceConstraint))
(final(CausalPrecedenceConstraint))
(final(ProducerDomain))
(final(BestProducerHeuristic))
(final(ConflictBranching))

