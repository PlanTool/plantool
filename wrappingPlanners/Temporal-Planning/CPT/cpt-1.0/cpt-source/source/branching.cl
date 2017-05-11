// ********************************************************************
// * CPT, version 1.0 sep. 2004                                       *
// * file: static-conflicts.cl                                        *
// * Copyright (©) Vincent Vidal, 2003-2004                           *
// ********************************************************************

// ------------------  File Overview  ---------------------------------
// *   Part 1: Init time heuristic                                    *
// *   Part 2: Support heuristic                                      *
// *   Part 3: Causal link conflict branching and heuristics          *
// *   Part 4: Mutex branching and heuristics                         *
// --------------------------------------------------------------------


[quickPost(pb:PlanningProblem, c:AbstractConstraint) : void => 
  post(pb, c), 
  propagate(pb)]



// ********************************************************************
// *                                                                  *
// *   Part 1: Init time heuristic                                    *
// *                                                                  *
// ********************************************************************



InitTimeDomain <: VarSelector(problem:PlanningProblem)

[makeInitTimeHeuristic(pb:PlanningProblem) : InitTimeDomain ->
  InitTimeDomain(problem = pb)]

[selectVar(vs:InitTimeDomain) : (Action U {unknown}) ->
  some(a in vs.problem.actions | isUsed(a) & not(isInstantiated(a)))]



// ********************************************************************
// *                                                                  *
// *   Part 2: Support heuristic                                      *
// *                                                                  *
// ********************************************************************



// ****************************************
// *   Best variable (support) heuristic  *
// ****************************************

ProducerDomain <: VarSelector(problem:PlanningProblem)

[makeProducerVarHeuristic(pb:PlanningProblem) : ProducerDomain ->
  ProducerDomain(problem = pb)]

[selectVar(vs:ProducerDomain) : (CausalLink U {unknown}) -> 
  let 
    pb := vs.problem,
    c0:(CausalLink U {unknown}) := unknown, 
    start0 := -1,
    slack0 := MAXINT
  in (
      for i in (1 .. length(pb.activeCausals)) (
	let c := pb.activeCausals[length(pb.activeCausals) - i + 1] in (
	    if not(isInstantiated(c)) (
	      let 
		best1 := (bestAction(c) as Action), 
		slack1 := slack(best1, c.consumer)
	      in (
		  if (firstStart(best1) > start0 | (firstStart(best1) = start0 & slack1 < slack0))
		    (c0 := c, (c0 as CausalLink).bestValue := best1.num, start0 := firstStart(best1), slack0 := slack1))))),

      traceSupportChoice(pb.activeCausals, c0),
      c0)]


[bestAction(c:CausalLink) : (Action U {unknown}) ->
  let a0:(Action U {unknown}) := unknown in (
      for a in getProducers(c) (
	if (unknown?(a0)
	   | (if c.problem.session.already_used_actions (a.num = 1 | (not(isUsed(a0)) & isUsed(a)) | ((not(isUsed(a0)) | isUsed(a)) & firstStart(a0) > firstStart(a)))
	      else (firstStart(a0) > firstStart(a) | (firstStart(a0) = firstStart(a) & firstEnd(a0) > firstEnd(a)))))
	  a0 := a),
      a0)]
	     


// **************************************
// *   Best value (producer) heuristic  *
// **************************************

BestProducerHeuristic <: ValSelector()

[makeBestProducerSelector() : ValSelector -> BestProducerHeuristic()]

[getBestVal(vh:BestProducerHeuristic, c:IntVar) : integer ->
  cinc(c.problem.cptChoiceSupport),
  if (c.bestValue <= c.cmax) c.bestValue else c.bestValue - c.offset]



// ********************************************************************
// *                                                                  *
// *   Part 3: Causal link conflict branching and heuristics          *
// *                                                                  *
// ********************************************************************



// *************************
// *   Conflict branching  *
// *************************

SolveConflict <: BinBranching
ConflictBranching <: CompositeBranching()
Conflict <: Ephemeral(causal:CausalLink, threat:Action)

[makeConflictBranching(b1:SolveConflict, b2:AbstractBranching) : ConflictBranching =>
  ConflictBranching(alternatives = list<AbstractBranching>(b1, b2))]

[selectBranching(b:ConflictBranching) : AbstractBranching =>
  if selectConflict(b.alternatives[1]) b.alternatives[1]
  else b.alternatives[2]]

[selectConflict(b:SolveConflict) : boolean =>
  let 
    pb := b.manager.problem,
    conf := pb.session.conflicts,
    c0 := unknown,
    a0 := unknown,
    d0 := MAXINT,
    d := 0 
  in (
      for i in (1 .. length(pb.activeCausals)) (
	let c := pb.activeCausals[length(pb.activeCausals) - i + 1] in (
	    for a in list{a in getThreats(c) | isUsed(a) 
			    & not(known?(solvedThreats, c) & a % c.solvedThreats) 
			    & (if (conf = ACTUAL_CONF) not(c canPrecedeWeak a | a canPrecedeWeak c)
			       else if (conf = EXTENDED_CONF) not(c canPrecede a | a canPrecede c)
			       else not(c cannotPrecede a | a cannotPrecede c))} (
	      d := max(slack(a, c), slack(c, a)),
	      if (d0 >= d) (c0 := c, a0 := a, d0 := d)))),
      if known?(c0) (
	b.bestConflict := Conflict(causal = (c0 as CausalLink), threat = (a0 as Action)),
	if unknown?(solvedThreats, c0) make_simple_bklist(c0, solvedThreats, pb.nbSolvedThreats, Action, Action()),
	memoryVerification(length(c0.solvedThreats), pb.nbSolvedThreats, "NbSolvedThreats"),
	store(c0.solvedThreats, a0), 
	true)
      else false)]



// *************************
// *   Conflict heuristic  *
// *************************

SolveConflict <: BinBranching(bestConflict:Conflict)

[makeSolveConflict() : SolveConflict => SolveConflict()]

[selectBranchingObject(b:SolveConflict) : (Conflict U {unknown}) => 
  traceConflictChoice(b.bestConflict),
  cinc(b.manager.problem.cptChoiceConflict),
  b.bestConflict]

[goDownBranch(b:SolveConflict, conflict:any, first:integer) : void -> 
  let 
    pb := b.manager.problem,
    c := conflict.causal as CausalLink, 
    a := conflict.threat as Action 
  in (
      if (slack(a, c) >= slack(c, a)) (
	if (first = 1) quickPost(pb, (a << c))
	else quickPost(pb, (c.consumer << a)))
      else (
	if (first = 2) quickPost(pb, (a << c))
	else quickPost(pb, (c.consumer << a))))]

[traceDownBranch(b:SolveConflict, conflict:any, first:integer) : void -> nil]



// ********************************************************************
// *                                                                  *
// *   Part 4: Mutex branching and heuristics                         *
// *                                                                  *
// ********************************************************************



Mutex <: Ephemeral(action1:Action, action2:Action)

SolveMutex <: BinBranching(bestMutex:Mutex)

[makeSolveMutex() : SolveMutex => SolveMutex()]

[selectBranchingObject(b:SolveMutex) : (Mutex U {unknown}) => 
  let pb := b.manager.problem, conf := pb.session.conflicts, mutex:(Mutex U {unknown}) := unknown in (
      for a1 in pb.activeActions (
	when a2 := some(a2 in a1.actionsMutex | isUsed(a2) 
			  & not(known?(mutexSolved, a1) & a2 % a1.mutexSolved)
			  & (if (conf = ACTUAL_CONF) not(a1 canPrecedeWeak a2 | a2 canPrecedeWeak a1)
			     else if (conf = EXTENDED_CONF) not(a1 canPrecede a2 | a2 canPrecede a1)
			     else not(a1 alwaysPrecede a2 | a2 alwaysPrecede a1)))
	in (
	  if unknown?(mutexSolved, a1) make_simple_bklist(a1, mutexSolved, pb.nbSolvedThreats, Action, Action()),
	  if unknown?(mutexSolved, a2) make_simple_bklist(a2, mutexSolved, pb.nbSolvedThreats, Action, Action()),
	  memoryVerification(length(a1.mutexSolved), pb.nbSolvedThreats, "NbMutexSolved"),
	  memoryVerification(length(a2.mutexSolved), pb.nbSolvedThreats, "NbMutexSolved"),
	  store(a1.mutexSolved, a2),
	  store(a2.mutexSolved, a1),
	  mutex := Mutex(action1 = a1, action2 = a2),
	  cinc(b.manager.problem.cptChoiceMutex),
	  break())),
      mutex)]
									 
[goDownBranch(b:SolveMutex, mutex:any, first:integer) : void -> 
  let pb := b.manager.problem, a1 := mutex.action1 as Action, a2 := mutex.action2 as Action in (
      if (firstStart(a1) <= firstStart(a2)) (
	if (first = 1) quickPost(pb, (a1 << a2))
	else quickPost(pb, (a2 << a1)))
      else (
	if (first = 1) quickPost(pb, (a2 << a1))
	else quickPost(pb, (a1 << a2))))]

[traceDownBranch(b:SolveMutex, conflict:any, first:integer) : void -> nil]
