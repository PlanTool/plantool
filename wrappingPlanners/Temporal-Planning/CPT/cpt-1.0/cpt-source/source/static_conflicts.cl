// ********************************************************************
// * CPT, version 1.0 sep. 2004                                       *
// * file: static-conflicts.cl                                        *
// * Copyright (©) Vincent Vidal, 2003-2004                           *
// ********************************************************************

// ------------------  File Overview  ---------------------------------
// *   Part 1: Support constraint                                     *
// *   Part 2: Action constraint                                      *
// *   Part 3: Propagations                                           *
// *   Part 4: Mutex Sets                                             *
// --------------------------------------------------------------------

// ********************************************************************
// *                                                                  *
// *   Part 1: Support constraint                                     *
// *                                                                  *
// ********************************************************************



// This constraint acts as an event handler on propagations made on a causal
// link S(p,a)[p]a. It is awaken when the domain of:
// - S(p,a) is updated: an action has been removed from D[S(p,a)], or S(p,a)
//   is instantiated with action or the exclusion value.
// - T(p,a) is updated: the bounds of some possible supporters in D[S(p,a)] 
//   have been updated.

SupportConstraint <: BinIntConstraint(v1:CausalLink, v2:IntVar)

[supportConstraint(c:CausalLink) : SupportConstraint -> SupportConstraint(v1 = c, v2 = c.init)]

[awake(sc:SupportConstraint) : void -> nil]

[awakeOnInst(sc:SupportConstraint, idx:integer) : void -> 
  if isInstantiated(sc.v1) (
    traceProducerUse(sc.v1, idx), 
    propagate(sc))]

[awakeOnRem(sc:SupportConstraint, idx:integer, v:integer) : void -> 
  traceProducerExclude(sc.v1, v), 
  propagate(sc)]

// If the causal link is excluded that is D[S(p,a)]={} (ie. is S(p,a) is 
// instantiated to the excluding value of the causal link), we exclude the 
// consumer from all possible partial plans.
// Otherwise, we synchronize the causal link (ie. we made all the possible
// propagations from the causal link), and we protect it from possible threats.
[propagate(sc:SupportConstraint) : void ->
  let c := sc.v1 in (
      if isExcluded(c) exclude(c.consumer) 
      else (
	synchronizeCausal(c), 
	protectCausal(c),
	if c.problem.session.mutex_sets computeLocalMutexSets(c)))]


// ********************************************************************
// *                                                                  *
// *   Part 2: Action constraint                                      *
// *                                                                  *
// ********************************************************************



// This constraint acts as an event handler on propagations made on an action a.
// It is awaken when the domain of T(p,a) is updated. 

ActionConstraint <: UnIntConstraint(v1:Action)

[actionConstraint(a:Action) : ActionConstraint -> ActionConstraint(v1 = a)]

[awake(ac:ActionConstraint) : void -> nil]

// If the action a is not excluded, we protect all actions mutex with a, and all
// causal links threatened by a.
// When protection is done, we synchronize the causal links in relation with a:
// - producers of the preconditions of a
// - consumers of the add effects of a
[propagate(ac:ActionConstraint) : void ->
  let a := ac.v1 in (
      if not(isExcluded(a)) (
	protectAgainst(a),
	if not(isExcluded(a)) 
	  synchronizeAction(a)))]



// ********************************************************************
// *                                                                  *
// *   Part 3: Propagations                                           *
// *                                                                  *
// ********************************************************************



// ****************************
// *  Bounds synchronization  *
// ****************************

// Something has changed around a causal link S(p,a)[p]a : we do all possible
// propagations on variables connected to that causal link.
[synchronizeCausal(c:CausalLink) : void ->
  when prod := getProducer(c) in (
    if not(isUsed(prod) | c.problem.session.canonicity) (
      let clone := cloneAction(prod, c) in (
	  post(c.problem, actionConstraint(clone)),
	  for c in clone.causals post(c.problem, supportConstraint(c)))),
    updateInfA(prod, firstStart(c)),
    updateInfC(c, firstStart(prod)),
    updateInfA(c.consumer, firstEnd(prod) + getDistance(prod, c.consumer)),
    updateSupC(c, lastStart(prod)),
    let m := min(lastStart(c), lastStart(c.consumer) - getDuration(prod) - getDistance(prod, c.consumer)) in
      updateSupA(prod, m),
      updateSupC(c, lastStart(prod)),
    use(prod))
  else (
    let min_first_end := MAXINT, min_prod_init := MAXINT, max_prod_init := 0, min_dist := MAXINT, m := 0 in (
	for a in getProducers(c) (
	  if (lastStart(a) < firstStart(c) | firstStart(a) > lastStart(c) | a cannotPrecede c.consumer) remProducer(c, a)
	  else (
	    m := firstEnd(a) + getDistance(a, c.consumer),
	    if (m < min_first_end) min_first_end := m,
	    m := firstStart(a),
	    if (m < min_prod_init) min_prod_init := m,
	    m := lastStart(a),
	    if (m > max_prod_init) max_prod_init := m,
	    m := getDuration(a) + getDistance(a, c.consumer),
	    if (m < min_dist) min_dist := m)),
	updateInfA(c.consumer, min_first_end),
	updateInfC(c, min_prod_init),
	m := min(max_prod_init, lastStart(c.consumer) - min_dist),
	updateSupC(c, m)))]

(interface(synchronizeCausal))

// Something has changed around an action: we don't do the propagations right
// now on the causal links connected to that action (preconditions and adds),
// but we post events instead. The propagations will be made later, when the 
// support constraints are awaken.
[synchronizeAction(a:Action) : void =>
  for c in a.causals (
    postUpdateInf(c.problem.propagationEngine as ChocEngine, c, 0)),
  for f in a.add (
    for c in list{c in f.causals | canProduce(a, c)} (
      postUpdateInf(c.problem.propagationEngine as ChocEngine, c, 0)))]


// ********************************************
// *  Protection of mutexes and causal links  *
// ********************************************

// We protect actions mutex with a by trying to order the two actions.
// Most of these propagations are redundant, but they improve efficiency.
// Then, we protect active causal links threatened by the action.
[protectAgainst(a:Action) : void =>
  let s := a.problem.session in (
      if s.propagate_causals (
	if (s.propagate_mutex & isUsed(a)) (
	  for f in a.add (
	    for a' in list{a' in f.deleters | a' != a & isUsed(a')} (
	      makeOrder(a, a'))),
	  for f in a.edel (
	    for a' in list{a' in f.producers | a' != a & isUsed(a')} (
	      makeOrder(a, a')))),
	if (s.propagate_inactive_threats | isUsed(a)) (
	  for f in a.edel (
	    if s.propagate_inactive_causals (
	      for c in list{c in f.causals | a != c.consumer} (
		makeOrder(a, c)))
 	    else (
	      for c in list{c in f.activeCausals | a != c.consumer} (
		makeOrder(a, c)))))))]

// We protect a causal link from its possible threats. We also compute
// local mutex sets.
[protectCausal(c:CausalLink) : void =>
  let s := c.problem.session in (
      if s.propagate_causals (
	if (isRequired(c) | s.propagate_inactive_causals) (
	  if s.propagate_inactive_threats (
	    for a in getThreats(c) makeOrder(a, c))
	  else (
	    for a in list{a in getThreats(c) | isUsed(a)} (
	      makeOrder(a, c))))))]


// *********************
// *  Using an action  *
// *********************

// A new action is used. The causal links associated to its preconditions are
// made active, it is set used (and stored in the active actions), mutexes
// and causal links threatened by that action are protected, and we finish
// by computing global mutex sets.
[use(a:Action) : void =>
  if not(isUsed(a)) (
    traceActionUsed(a),
    for c in a.causals setRequired(c),
    setUsed(a),
    protectAgainst(a),
    if a.problem.session.mutex_sets (
      computeGlobalMutexSets(a)))]


// ***************************************
// *  Actions and causal links ordering  *
// ***************************************

// Makes ordered 2 actions or one action and one causal link, if one ordering 
// is impossible
[makeOrder(a:Action, c:(Action U CausalLink)) : void =>
  if (c cannotPrecede a) orderBefore(a, c),
  if (a cannotPrecede c) orderBefore(c, a)]

// Orders the first action before the second one
[orderBefore(a:Action, a':Action) : void =>
  updateInfA(a', firstEnd(a) + getDistance(a, a')),
  updateSupA(a, lastStart(a') - getDuration(a) - getDistance(a, a'))]

[orderBefore2(a:Action, a':Action) : void =>
  if isUsed(a) updateInfA(a', firstEnd(a) + getDistance(a, a')),
  if isUsed(a') updateSupA(a, lastStart(a') - getDuration(a) - getDistance(a, a'))]

// Orders a causal link before an action
[orderBefore(c:CausalLink, a:Action) : void =>
  if isRequired(c) updateInfA(a, firstEnd(c.consumer) + getDistance(c, a)),
  if isUsed(a) updateSupA(c.consumer, lastStart(a) - getDuration(c.consumer) - getDistance(c, a))]

// Orders an action before a causal link
[orderBefore(a:Action, c:CausalLink) : void =>
  if canProduce(a, c) (
    if isRequired(c) updateSupA(a, lastStart(c)),
    if isUsed(a) updateInfC(c, firstStart(a)))
  else (
    if isRequired(c) (updateSupA(a, lastStart(c) - getDuration(a) - getDistance(a, c))),
    if isUsed(a) (updateInfC(c, firstStart(a) + getDuration(a) + getDistance(a, c))))]


// ********************************************************************
// *                                                                  *
// *   Part 4: Mutex Sets                                             *
// *                                                                  *
// ********************************************************************



// **************************
// *  Distance computation  *
// **************************

[dist(a:Action, a':Action) : integer =>
  max(getDistance(a, a'), firstStart(a') - lastEnd(a))]

[minDistance(a:Action, a':Action) : integer =>
  if (a cannotPrecede a') dist(a', a)
  else if (a' cannotPrecede a) dist(a, a')
  else min(dist(a, a'), dist(a', a))]


// ***********************
// *  Global Mutex Sets  *
// ***********************

[computeGlobalMutexSets(a:Action) =>
   let pb := a.problem in
   let st := false in (
       if (a.num > 2) (
	 for ms in pb.mutexSets (
	   if forall(a' in ms | isMutex(a, a')) (store(ms, a), st := true)),
	 if not(st) (
	   store(pb.mutexSets, make_list(length(pb.actions), Action, a)),
	   shrink(last(pb.mutexSets), 1)),
	 makespan(pb)))]

[makespan(pb:PlanningProblem) : boolean ->
  let res := false in (
  for ms in list{ms in pb.mutexSets | length(ms) > 2} (
    let msp := 0, min_start := MAXINT, max_end := 0, maxd := 0, d := 0 in (
	for ac in ms  (
	  let a := ac as Action in (
	      if (firstStart(a) < min_start) min_start := firstStart(a),
	      if (lastEnd(a) > max_end) max_end := lastEnd(a),
	      d := mindist(a, ms),
	      if (d > maxd) maxd := d,
	      msp :+ getDuration(a) + d
	      )),
	if (firstStart(pb.endAction) < min_start + msp - maxd) res := true,
	updateInfA(pb.endAction, min_start + msp - maxd),
	if (max_end - min_start < msp - maxd) raiseContradiction(pb))),
    res)]


// **********************
// *  Local Mutex Sets  *
// **********************

[computeLocalMutexSets(c:CausalLink) =>
   let 
     before := list<Action>(),
     after := list<Action>(),
     middle := list<Action>()
   in (
       for a in getThreats(c) (
	 if isUsed(a) (
	   if (c cannotPrecede a) addMutexSet(before, a)
	   else if (a cannotPrecede c) addMutexSet(after, a)
	   else add(middle, a)
	 )),
       if after makespanAfter(c, after, middle),
       if before makespanBefore(c, before, middle))]

[addMutexSet(l:list<Action>, a:Action) : void =>
  if (a.num > 2 & forall(a' in l | a != a' & isMutex(a, a')))
    add(l, a)]

[mindist(a:Action, actions:list<Action>) : integer =>
  if (length(actions) = 1) 0
  else min(list{minDistance(a, a') | a' in list{a' in actions | a != a'}})]

[makespanAfter(c:CausalLink, after:list<Action>, middle:list<Action>) : void =>
  let msp := 0, max_end := 0, old_max_end := 0, msp2 := 0 in (
      for a in after (
	if (lastEnd(a) > max_end) max_end := lastEnd(a),
	msp :+ getDuration(a) + min(getDistance(c, a), mindist(a, after))),
      updateSupA(c.consumer, max_end - msp - getDuration(c.consumer)),
      old_max_end := max_end,
      for a in list{a in middle | forall(a' in after | isMutex(a, a'))} (
	max_end := max(old_max_end, lastEnd(a)),
	add(after, a),
	msp := 0,
	for a in after (
	  msp :+ getDuration(a) + min(getDistance(c, a), mindist(a, after))),
	if (firstEnd(c.consumer) + msp > max_end) orderBefore(a, c),
	shrink(after, length(after) - 1)))]

[makespanBefore(c:CausalLink, before:list<Action>, middle:list<Action>) : void =>
  let msp := 0, min_start := MAXINT, old_min_start := 0, maxd := 0 in (
      for a in before (
	if (firstStart(a) < min_start) min_start := firstStart(a),
	if (canProduce(a, c) & getDuration(a) > maxd) maxd := getDuration(a),
	msp :+ getDuration(a) + min(getDistance(a, c), mindist(a, before))),
      updateInfC(c, min_start + msp - maxd),
      old_min_start := min_start,
      for a in list{a in middle | forall(a' in before | isMutex(a, a'))} (
	min_start := min(old_min_start, firstStart(a)),
	add(before, a),
	msp := 0,
	maxd := 0,
	for a in before (
	if (canProduce(a, c) & getDuration(a) > maxd) maxd := getDuration(a),
	  msp :+ getDuration(a) + min(getDistance(a, c), mindist(a, before))),
	if (lastStart(c) - msp + maxd < min_start) orderBefore(c, a),
	shrink(before, length(before) - 1)))]


