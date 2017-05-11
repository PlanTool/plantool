// ********************************************************************
// * CPT, version 1.0 sep. 2004                                       *
// * file: static-conflicts.cl                                        *
// * Copyright (©) Vincent Vidal, 2003-2004                           *
// ********************************************************************

[traceProducerUse(c:CausalLink, idx:integer) : void =>
  #if VERB (
    if (verbose() >= PRODUCER_USE) (
      if (idx = 1 & isInstantiated(c) & c.value != c.excl) (
	printf("    ~S == ~S\n", c, getProducer(c)))))]

[traceProducerExclude(c:CausalLink, val:integer) : void =>
  #if VERB (
    if (verbose() >= PRODUCER_EXCLUDE) (
      if (val != c.excl) 
	printf("    ~S != ~S\n", c, c.problem.actions[val])))]

[tracePrecedenceAwake(c:PrecedenceConstraint) : void =>
  #if VERB (
    trace(ACTION_PRECEDENCE, "        ~S\n", c))]

[tracePrecedenceAwake(c:CausalPrecedenceConstraint) : void =>
  #if VERB (
    trace(CAUSAL_PRECEDENCE, "        ~S\n", c))]

[tracePopWorld(pb:PlanningProblem) : void =>
  #if VERB (
    trace(POP_WORLD, "\n------------------------------------------- Backtrack to world : ~S --------------------------------------------\n\n", world?()))]

[tracePushWorld(pb:PlanningProblem) : void =>
  #if VERB (
    trace(PUSH_WORLD, "\n================================================ New world : ~S ================================================\n\n", world?()))]

[traceSupportChoice(causals:list<CausalLink>, c0:(CausalLink U {unknown})) : void =>
  #if VERB (
    if (verbose() >= SUPPORT_CHOICE) (
      if (c0 = unknown) printf("\n ALL SUPPORTS ARE INSTANTIATED !\n"),
      if (c0 != unknown) (
	printf("\n*** Support choice (~S) : ~S among\n    ~S\n", world?(), c0, list<CausalLink>{v in causals | not(isInstantiated(v)) & isRequired(v)}))),
    if (verbose() >= PRODUCER_CHOICE) (
      if (c0 != unknown) (
	printf("\n*** Producer choice (~S) : ~S for ~S among\n    ~S\n", world?(),
	       (if (c0.bestValue = 0) "unused" else c0.problem.actions[c0.bestValue]),
	       c0, 
	       (if canBeInstantiatedTo(c0, c0.excl) list("unused") else list()) /+ list{a | a in getProducers(c0)}))))]

[traceConflictChoice(conflict:Conflict) : void =>
  #if VERB (
    if (verbose() >= CONFLICT_CHOICE) (
      if (conflict != unknown) (
	let 
	  c := conflict.causal as CausalLink, 
	  a := conflict.threat as Action 
	in (
	    printf("\n*** Conflict choice : (~S << ~S) or (~S << ~S) \n\n", a, c, c.consumer, a)))))]

[traceActionUsed(a:Action) : void =>
  #if VERB (if (verbose() >= PRODUCER_USE) printf("use ~S~S\n", a, a.num))]

[traceActionExcluded(a:Action) : void =>
  #if VERB (if (verbose() >= PRODUCER_EXCLUDE) printf("exclude ~S\n", a))]

[traceUpdateInf(a:Action, val:integer) : void =>
  #if VERB (if (verbose() >= UPDATE_INF) (if (a.inf < val) printf("   update inf ~S~S : ~S\n", a, a.num, val)))]

[traceUpdateSup(a:Action, val:integer) : void =>
  #if VERB (if (verbose() >= UPDATE_SUP) (if (a.sup > val) printf("   update sup ~S~S : ~S\n", a, a.num, val)))]

    
