// ********************************************************************
// * CPT, version 1.0 sep. 2004                                       *
// * file: static-conflicts.cl                                        *
// * Copyright (©) Vincent Vidal, 2003-2004                           *
// ********************************************************************

// ------------------  File Overview  ---------------------------------
// *   Part 1: Object hierarchy                                       *
// *   Part 2: Fluents                                                *
// *   Part 3: Actions                                                *
// *   Part 4: Causal Links                                           *
// *   Part 5: Temporal relations between actions and causal link     *
// *   Part 6: Distance                                               *
// *   Part 7: Planning problems                                      *
// --------------------------------------------------------------------

// ********************************************************************
// *                                                                  *
// *   Part 1: Object hierarchy                                       *
// *                                                                  *
// ********************************************************************


Symbol <: object
Predicate <: Symbol
Term <: Symbol
Variabl <: Term
Constant <: Term

Fluent <: Ephemeral
Action <: IntVar
CausalLink <: IntVar
PlanningProblem <: Problem



// ********************************************************************
// *                                                                  *
// *   Part 2: Fluents                                                *
// *                                                                  *
// ********************************************************************



// ******************
// *  Fluent Class  *
// ******************

// Fields :
// - num: unique for each fluent. Reflects the fluent place in the list of 
//        fluents of the problem
// - name: name of the fluent
// - producers: list of actions that add the fluent
// - consumers: list of actions that have the fluent as precondition
// - deleters: list of actions that "e-delete" the fluent
// - causals: list of all potential causal links involving the fluent
// - activeCausals: backtrackable list of active causal links involving the fluent
// - pairCost: h2 cost of each pair involving the fluent
// - reachable: boolean used for pre-processing, stating that the fluent can or
//              cannot be reached from the initial state

Fluent <: Ephemeral(
  num:integer, 
  tinit:integer,
  name:string = "",
  producers:list<Action>,
  consumers:list<Action>, 
  deleters:list<Action>,
  causals:list[CausalLink],
  activeCausals:list[CausalLink],
  pairCost:list[integer],
  enabled:boolean = true,
  reachable:boolean = false)


// *********************
// *  Pretty printing  *
// *********************

[self_print(f:Fluent) : void -> printf("~A", f.name)]

[complete_print(f:Fluent) : void -> 
  printf("\nFLUENT ~S : ~A\n  PRODUCERS -> ", f.num, f.name),
  for a in f.producers printf(" ~A(~A)", a.name, a.parameters), printf("\n  CONSUMERS -> "), 
  for a in f.consumers printf(" ~A(~A)", a.name, a.parameters), printf("\n  DELETERS -> "),
  for a in f.deleters printf(" ~A(~A)", a.name, a.parameters), printf("\n")]


// *****************************************
// *  Access methods to fluent properties  *
// *****************************************

// get and set (backtrackable) a fluent pair cost
[getPairCost(f1:Fluent, f2:Fluent) : integer => f1.pairCost[f2.num]]
[setPairCost(f1:Fluent, f2:Fluent, c:integer) : void => 
  store(f1.pairCost, f2.num, c), store(f2.pairCost, f1.num, c)]


// *********************
// *  Closing Fluents  *
// *********************

[closeFluentParser(fluents:list<Fluent>, f:Fluent) : void ->
   for a in f.consumers (setConsumes(a, f), for a' in f.deleters setMutex(a, a')), 
   for a in f.producers setProduces(a, f),
   for a in f.deleters (setDeletes(a, f), for a' in f.producers setMutex(a, a')),
   f.pairCost := make_list(length(fluents), integer, MAXINT)]

[closeFluent(pb:PlanningProblem, f:Fluent) : void ->
  let dels := f.deleters, prods := f.producers in (
      make_simple_bklist(f, producers, length(f.producers) + pb.nbActionsMore, Action, Action()),
      make_simple_bklist(f, deleters, length(f.deleters) + pb.nbActionsMore, Action, Action()),
      for a in prods add(f.producers, a),
      for a in dels add(f.deleters, a)),
   for a in f.consumers (setConsumes(a, f), for a' in f.deleters setMutex(a, a')),
   for a in f.producers (setProduces(a, f)),
   for a in f.deleters (setDeletes(a, f), for a' in f.producers setMutex(a, a')),
   make_simple_bklist(f, activeCausals, length(f.consumers) + pb.nbCausalsMore, CausalLink, CausalLink()),
   f.pairCost := make_list(length(pb.fluents), integer, MAXINT)]

(interface(complete_print), interface(getPairCost), interface(setPairCost), ;interface(closeFluentParser),
 interface(closeFluent))


// ********************************************************************
// *                                                                  *
// *   Part 3: Actions                                                *
// *                                                                  *
// ********************************************************************



// ******************
// *  Action Class  *
// ******************

// Fields :
// - num: unique for each action. Reflects the action place in the list of 
//        actions of the problem
// - parameters: parameters of the actions used for pretty-printing
// - prec: list of the preconditions of the action
// - add: list of the add effects of the action
// - del: list of the del effects of the action
// - edel: list of the e-del effects of the action
// - causals: list of causal links attached to the preconditions of the action
// - actionsMutex: actions that are mutex with the action
// - mutexSolved: actions mutex that have been ordered before or after
// - duration: duration of the action
// - tinit: initial starting time of the action. Used for pre-processing
// - tend: initial ending time of the action. Used for pre-processing
// - distance: list of distances between the action and all the other actions
// - properties: table for fast acces to know if an action consumes, adds or deletes a fluent
// - commutative: table for fast acces to know if an action is mutex with the action
// - used: true if the action is used in the current partial plan
// - excluded: true if the action is excluded from all partial plans
// - reachable: true if the action can be reached from the initial sate, used for pre-processing
// - reachedPrec: number of reachable precs, used for pre-processing
//
// An action inherits from IntVar, which encodes the possible starting time
// of the action. For an action a, it encodes T(a).

Action <: IntVar(
  num:integer,
  numinit:integer,
  isevent:boolean = false,
  parameters:list[Term],
  prec:list[Fluent], 
  add:list[Fluent], 
  del:list[Fluent],
  edel:list<Fluent>,
  causals: list[CausalLink],
  actionsMutex:set<Action>,
  mutexSolved:list[Action],
  durationRat:Rational = zero, 
  duration:integer = 0, 
  tinit:integer = MAXINT, 
  tinitEvent:integer = 0,
  tend:integer = 0,
  distance:list[integer],
  properties:list[integer],
  commutative:BoolArray,
  used:boolean = false,
  excluded:boolean = false,
  reachable:boolean = false,
  reachedPrec:integer = 0)


// *********************
// *  Pretty printing  *
// *********************

[self_print(a:Action) : void -> 
  printf("~A~A(~A)[~S,~S]", (if a.used "*" else ""), a.name, a.parameters, a.inf, a.sup)]

[action_to_string(a:Action) : string ->
  print_in_string(),
  printf("(~A", a.name),
  for p in a.parameters printf(" ~S", p),
  princ(")"),
  end_of_string()]

[complete_print(a:Action) : void -> 
  printf("\nACTION ~S : ~A\n  INIT -> ~A\n  DURATION -> ~S (~S)\n  PREC ->", a.num, action_to_string(a), a.tinit, a.durationRat, a.duration),
  for p in a.prec (printf(" ~S", p), fflush()), princ("\n  ADD ->"),
  for a' in a.add printf(" ~S", a'), princ("\n  DEL ->"),
  for d in a.edel printf(" ~S", d), princ("\n")]


// *****************************************
// *  Access methods to action properties  *
// *****************************************

// Checks and sets wether the action is used in a partial plan or excluded from
// all partial plans.
[setUsed(a:Action) : void => put_store(used, a, true, true), store(a.problem.activeActions, a)]
[setExcluded(a:Action) : void => put_store(excluded, a, true, true)]
[isUsed(a:Action) : boolean => a.used]
[isExcluded(a:Action) : boolean => a.excluded]

// Register and test that 2 actions are mutex
[setMutex(a:Action, a':Action) : void => setTrue(a.commutative, a'.num), setTrue(a'.commutative, a.num)]
[isMutex(a:Action, a':Action) : boolean => not(isTrue(a.commutative, a'.numinit))]

// Register that an action consumes, produces or deletes a fluent for fast access
[setConsumes(a:Action, f:Fluent) : void => a.properties[f.num] :+ 1]
[setProduces(a:Action, f:Fluent) : void => a.properties[f.num] :+ 2]
[setDeletes(a:Action, f:Fluent) : void => a.properties[f.num] :+ 4]
[consumes(a:Action, f:Fluent) : boolean => a.properties[f.num][0]]
[produces(a:Action, f:Fluent) : boolean => a.properties[f.num][1]]
[deletes(a:Action, f:Fluent) : boolean => a.properties[f.num][2]]

// Set and get distances between 2 actions, and between one action and one causal link
;[setDistance(a:Action, a':Action, d:integer) : void => if not(a'.isevent) a.distance[a'.num] := d]
[setDistance(a:Action, a':Action, d:integer) : void => if not(a.isevent | a'.isevent) a.distance[a'.num] := d]
[getDistance(a:Action, a':Action) : integer => a.distance[a'.numinit]]
[getDistance(c:CausalLink, a:Action) : integer => getDistance(c.consumer,a)]
[getDistance(a:Action, c:CausalLink) : integer => min(list<integer>{getDistance(a, a') | a' in getProducers(c)})]

// Temporal properties of actions:
// - firstStart(a) = Tmin(a)
// - lastStart(a) = Tmax(a)
// - firstEnd(a) = Tmin(a)+dur(a)
// - lastEnd(a) = Tmax(a)+dur(a)
// - getDuration(a) = dur(a)
[firstStart(a:Action) : integer => a.inf]
[lastStart(a:Action) : integer => a.sup]
[getDuration(a:Action) : integer => a.duration]
[firstEnd(a:Action) : integer => firstStart(a) + getDuration(a)]
[lastEnd(a:Action) : integer => lastStart(a) + getDuration(a)]


// ********************
// *  Action closing  *
// ********************

[closeActionParser(actions:list<Action>, fluents:list<Fluent>, a:Action) : void ->
  a.properties := make_list(length(fluents), integer, 0),
  a.commutative := makeBoolArray(length(actions))]

[closeAction(pb:PlanningProblem, a:Action) : void ->
  addIntVar(pb,a),
  closeIntVar(a, a.tinit, MAXINT, 1),
  a.numinit := a.num,
  a.properties := make_list(length(pb.fluents), integer, 0),
  a.distance := make_list(length(pb.actions), integer, 0),
  a.commutative := makeBoolArray(length(pb.actions)),
  a.causals := list<CausalLink>{makeCausalLink(pb, f, a) | f in a.prec},
;  make_simple_bklist(a, mutexSolved, pb.nbSolvedThreats, Action, Action()),
  for c in a.causals add(pb.causalLinks, c)]

     

[cloneAction(a:Action, causal:CausalLink) : Action ->
  let pb := a.problem, clone := Action(
    name = a.name,
    problem = a.problem, 
    num = length(pb.actions) + 1,
    numinit = a.numinit,
    parameters = a.parameters,
    prec = a.prec, 
    add = a.add, 
    edel = a.edel,
    actionsMutex = a.actionsMutex,
    duration = a.duration,
    distance = a.distance,
    commutative = a.commutative) 
  in (
      memoryVerification(length(pb.activeActions) - 2, pb.nbActionsMore, "NbActionsMore"),
      store(pb.actions, clone),
      closeIntVar(clone, a.inf, a.sup, 1),
      clone.causals := list<CausalLink>{cloneCausalLink(c, clone) | c in a.causals},
;      make_simple_bklist(clone, mutexSolved, pb.nbSolvedThreats, Action, Action()),
      for f in clone.add (
	store(f.producers, clone),
	for c in list{c in f.causals | c != causal & canProduce(a, c)} (
	  addDomainVal(c.bucket, clone.num - c.offset, c.sup),
	  c.value := UNKNOWNINT,
	  c.sup := (clone.num - c.offset))),
      for f in clone.edel store(f.deleters, clone),
      clone)]
(interface(cloneAction))

[addDomainVal(d:LinkedListIntDomain,x:integer,s:integer) : void ->
  let l := d.contents, i := x - d.offset, j := s - d.offset in (
      d.bucketSize :+ 1,
      store(l,j + 1, i, true),
      for k in (j + 2 .. i) l[k] := i,
      l[i + 1] := MAXINT)]
;      for k in (j + 1 .. i) store(l,k,i,true),
;      store(l, i + 1, MAXINT, true))]

(interface(cloneAction), interface(closeAction), interface(addDomainVal))

// ******************************
// *  Computing action mutexes  *
// ******************************

[computeActionsMutex(a:Action) : void ->
  for f in a.del (
    for a' in list{a' in f.producers | a != a' & forall(f' in a.prec | not(deletes(a',f'))) & forall(f' in a'.prec | not(deletes(a,f')))} (
      add(a.actionsMutex, a')))]


// **********************************
// *  Bounds propagation on T(a)    *
// **********************************

// We take the control on increasing and decreasing the bounds of T(a)
// in order to avoid inconsistency when Tmin(a) > Tmax(a). In that case,
// the action is excluded but no inconsistency is raised.

[updateInfA(a:Action, val:integer) : void =>
  traceUpdateInf(a, val),
  if (a.sup < val) exclude(a)
  else if (a.inf < val) (
    a.inf := val,
    postUpdateInf(a.problem.propagationEngine as ChocEngine, a, 0))]

[updateSupA(a:Action, val:integer) : void =>
  traceUpdateSup(a, val),
  if (a.inf > val) exclude(a)
  else if (a.sup > val) (
    a.sup := val,
    postUpdateSup(a.problem.propagationEngine as ChocEngine, a, 0))]


// ***************************************************
// *  Exclusion of an action from all partial plans  *
// ***************************************************

[exclude(a:Action) : void ->
  if not(isExcluded(a)) (
    traceActionExcluded(a),
    if isUsed(a) raiseContradiction(a),
    for f in a.add (
      for c in list{c in f.causals | isPossible(c)} (
	remProducer(c, a))),
    setExcluded(a),
    for c in a.causals (
      setExcluded(c)))]

(interface(exclude))

// ********************************************************************
// *                                                                  *
// *   Part 4: Causal Links                                           *
// *                                                                  *
// ********************************************************************



// ***********************
// *  Causal link Class  *
// ***********************

// Fields :
// - fluent: fluent that needs to be supported by an action
// - consumer: action that has the fluent as a precondition
// - init: for a fluent f and an action a, represents the variable T(p,a) which
//         encodes the possible starting times of the potential supporters of p.
// - excl: minimal value of the domain minus 1 : represents the exclusion of the 
//         causal link from all partial plans
// - solvedThreats: list of actions that threaten the causal link, that have been
//         ordered *before* or *after* the causal link (that is, before the possible
//         supporter of the causal, or after the consumer of the causal
// - bestValue: number of the action considered as the best choice for being the
//         supporter, when computing the heuristic (and used only in that case)
//
// A causal link inherits from IntVar, which encodes the possible supporters
// of the causal link. For a causal link [p]a , it encodes S(p,a). The domain
// D[S(p,a)] contains the numbers of the actions that can produce p, plus an integer
// equal to the minimal value of D[S(p,a)] minus 1, which represents the fact that 
// the causal link is excluded from all possible partial plans.

CausalLink <: IntVar(
  fluent:Fluent,
  consumer:Action, 
  init:IntVar,
  excl:integer,
  cmax:integer,
  offset:integer,
  solvedThreats:any,
  bestValue:integer)


// *********************
// *  Pretty printing  *
// *********************

[self_print(c:CausalLink) : void -> printf("S(~S, ~S)", c.fluent, c.consumer)]


// **********************************************
// *  Access methods to causal link properties  *
// **********************************************

// Temporal properties of S(p,a):
// - firstStart([p]a): Tmin(p,a)
// - lastStart([p]a): Tmax(p,a)
[firstStart(c:CausalLink) : integer => c.init.inf]
[lastStart(c:CausalLink) : integer => c.init.sup]

[getProducerNum(i:integer, c:CausalLink) : integer => if (i <= c.cmax) i else i + c.offset]
[getProducerNum(a:Action, c:CausalLink) : integer => if (a.num <= c.cmax) a.num else a.num - c.offset]
[getProducerNum(c:CausalLink) : integer => getProducerNum(c.value, c)]

// Instantiate S(p,b) to a
[setProducer(c:CausalLink, a:Action) : void => if (c.excl = a.num) raiseContradiction(c) else setVal(c, getProducerNum(a, c))]

// Removes a from D[S(p,b)]
[remProducer(c:CausalLink, a:Action) : void => if (c.excl != a.num) remVal(c, getProducerNum(a, c))]

// Tests wether a belongs to D[S(p,b)]
[canProduce(a:Action, c:CausalLink) : boolean => c.excl != a.num & canBeInstantiatedTo(c, getProducerNum(a, c))]

// Returns S(p,b) if S(p,b) is instantiated, unknown otherwise
[getProducer(c:CausalLink) : (Action U {unknown}) => if (c.value > c.excl) c.problem.actions[getProducerNum(c)] else unknown]

// Tests if S(p,b) is instantiated and equal to a
[isProducer(a:Action, c:CausalLink) : boolean => getProducerNum(c) = a.num]

// Returns all possible supporters of [p]b
[getProducers(c:CausalLink) : list<Action> => list<Action>{c.problem.actions[getProducerNum(p,c)] | p in list<integer>{p in domain(c) | p != c.excl}}]

// Returns all possible threats of [p]b
[getThreats(c:CausalLink) : list<Action> => list<Action>{d in c.fluent.deleters | not(isExcluded(d)) & d != c.consumer}]
;[getThreats(c:CausalLink) : list<Action> => list<Action>{d in (c.fluent.deleters /+ c.moreThreats) | not(isExcluded(d)) & d != c.consumer}]

// Tests and sets wether the causal link is required (must be used in the plan), is 
// excluded from all future partial plans, or is not yet required and not excluded
[setExcluded(c:CausalLink) : void => c.sup := c.excl]
[setRequired(c:CausalLink) : void => 
  remVal(c, c.excl),
  store(c.problem.activeCausals, c),
  store(c.fluent.activeCausals, c)]
[isRequired(c:CausalLink) : boolean => c.inf != c.excl]
[isExcluded(c:CausalLink) : boolean => c.sup = c.excl]
[isPossible(c:CausalLink) : boolean => c.sup != c.excl]

// Optimization for using getProducers in iterations.

[Iterate(x:getProducers[tuple(CausalLink)], v:Variable, e:any) : void =>
  let
     c := eval(nth(args(x),1)) as CausalLink,
     dom := c.bucket as LinkedListIntDomain,
     ac := c.problem.actions,
     n := c.inf
   in (
       if (n = c.excl) n := getNextValue(dom, n),
       while (n <= c.sup) (
	 let v := ac[getProducerNum(n,c)] as Action in
	   (e, n := getNextValue(dom, n))))]


// **************************
// *  Causal link creation  *
// **************************

// Creation of a causal link
[makeCausalLink(pb:PlanningProblem, f:Fluent, a:Action) : CausalLink ->
  let 
    c := CausalLink(
      fluent = f, 
      consumer = a,
      init = makeBoundIntVar(pb, "init", 0, MAXINT)), // creation of T(p,a)
    maxVal := 0,
    minVal := MAXINT
  in (
;      make_simple_bklist(c, solvedThreats, pb.nbSolvedThreats, Action, Action()),
      // computing the minimum and maximum values of D[S(p,a)]
      for a in f.producers (
	if (a.num > maxVal) maxVal := a.num,
	if (a.num < minVal) minVal := a.num),
      // exclusion value of the causal link
      c.excl := minVal - 1,
      c.cmax := maxVal,
      c.offset := length(pb.actions) - maxVal,
      // creation of S(p,a)
      addIntVar(pb,c),
      closeIntVar(c, c.excl, maxVal, 1),
      c.bucket := makeLinkedListIntDomain(c.excl, c.cmax + pb.nbActionsMore),
      add(f.causals, c),
      c)]

// Removes values from D[S(p,a)] that do not represent possible supporters
[closeCausalLink(pb:PlanningProblem, c:CausalLink) : void ->
  let next := c.cmax - c.bucket.offset, v := c.cmax - 1 in (
      for val in (c.inf + 2 .. c.cmax - 1) (
	if produces(pb.actions[v], c.fluent) next := v - c.bucket.offset
	else (c.bucket.contents[v - c.bucket.offset] := next, c.bucket.bucketSize :- 1),
	v :- 1)),
    for val in (c.cmax + 1 - c.bucket.offset .. c.cmax + pb.nbActionsMore - c.bucket.offset) (
      c.bucket.contents[val] := MAXINT)]

[cloneCausalLink(c:CausalLink, a:Action) : CausalLink ->
  let pb := c.problem, clone := CausalLink(
    name = c.name,
    problem = c.problem, 
    fluent = c.fluent,
     consumer = a,
     init = IntVar(problem = pb),
     excl = c.excl,
     cmax = c.cmax,
     offset = c.offset,
     bucket = LinkedListIntDomain(
       offset = c.bucket.offset,
       bucketSize = c.bucket.bucketSize,
       contents = copy(c.bucket.contents))) in (
      store(pb.causalLinks, clone),
;      make_simple_bklist(clone, solvedThreats, pb.nbSolvedThreats, Action, Action()),
      closeIntVar(clone.init, c.init.inf, c.init.sup, 1),
      closeIntVar(clone, c.excl, c.sup, 1),
      store(clone.fluent.causals, clone),
      clone)]

(interface(makeCausalLink), interface(closeCausalLink), interface(cloneCausalLink))

// **********************************
// *  Bounds propagation on T(p,a)  *
// **********************************

// We take the control on increasing and decreasing the bounds of T(p,a)
// in order to avoid inconsistency when Tmin(p,a) > Tmax(p,a). In that case,
// the causal link is excluded but no inconsistency is raised.

[updateInfC(c:CausalLink, val:integer) : void =>
  if (c.init.sup < val) exclude(c.consumer)
  else if (c.init.inf < val) (
    c.init.inf := val,
    if (c.init.sup = val) c.init.value := val, 
    postUpdateInf(c.problem.propagationEngine as ChocEngine, c.init, 0))]

[updateSupC(c:CausalLink, val:integer) : void =>
  if (c.init.inf > val) exclude(c.consumer)
  else if (c.init.sup > val) (
    c.init.sup := val,
    if (c.init.inf = val) c.init.value := val, 
    postUpdateSup(c.problem.propagationEngine as ChocEngine, c.init, 0))]

  


// ********************************************************************
// *                                                                  *
// *   Part 5: Temporal relations between actions and causal link     *
// *                                                                  *
// ********************************************************************

alwaysPrecede :: operation(precedence = precedence(/))
cannotPrecede :: operation(precedence = precedence(/))
canPrecede :: operation(precedence = precedence(/))
canPrecedeWeak :: operation(precedence = precedence(/))


// An action a always precede an action a' iff:
//     a'=End  or  Tmax(a)+dur(a)+dist(a,a') <= Tmin(a')
[alwaysPrecede(a:Action, a':Action) : boolean => a'.num = 2 | lastEnd(a) + getDistance(a, a') <= firstStart(a')]

// A causal link [p]b always precede an action a iff:
//     b always precede a
[alwaysPrecede(c:CausalLink, a:Action) : boolean => c.consumer alwaysPrecede a]

// An action a always precede a causal link [p]b iff:
//     S(p,b)=a  or  
//     ForAll a' in D[S(p,b)], [ a=a' or a always precede a' or Tmax(a)+dur(a)+dist(a,a') <= Tmin(p,b) ]
// There is some kind of redundancy in this rule, but it improves efficiency.
[alwaysPrecede(a:Action, c:CausalLink) : boolean =>
  isProducer(a, c) | forall(a' in getProducers(c) | (a = a' | a alwaysPrecede a' | lastEnd(a) + getDistance(a, a') <= firstStart(c)))]

// An action a cannot precede an action a' iff:
//     a=End  or  Tmin(a)+dur(a)+dist(a,a') > Tmax(a')
[cannotPrecede(a:Action, a':Action) : boolean => a.num = 2 | firstEnd(a) + getDistance(a, a') > lastStart(a')]

// A causal link [p]b cannot precede an action a iff:
//     b cannot precede a
[cannotPrecede(c:CausalLink, a:Action) : boolean => c.consumer cannotPrecede a]

// An action a cannot precede a causal link [p]b iff:
//     a does not belong to D[S(p,b)]  or  
//     ForAll a' in D[S(p,b)], [ a always precede a' or Tmin(a)+dur(a)+dist(a,a') > Tmax(p,b) ]
// There is some kind of redundancy in this rule, but it improves efficiency.
[cannotPrecede(a:Action, c:CausalLink) : boolean =>
  not(canProduce(a, c)) & forall(a' in getProducers(c) | (a cannotPrecede a' | firstEnd(a) + getDistance(a, a') > lastStart(c)))]

[canPrecede(a:Action, a':Action) : boolean => firstEnd(a) + getDistance(a, a') <= firstStart(a')]
[canPrecede(c:CausalLink, a:Action) : boolean => canPrecede(c.consumer, a)]
[canPrecede(a:Action, c:CausalLink) : boolean => forall(a' in getProducers(c) | (a = a' | firstEnd(a) + getDistance(a, c) <= firstStart(c)))]

[canPrecedeWeak(a:Action, a':Action) : boolean => firstEnd(a) <= firstStart(a')]
[canPrecedeWeak(c:CausalLink, a:Action) : boolean => canPrecedeWeak(c.consumer, a)]
[canPrecedeWeak(a:Action, c:CausalLink) : boolean => canProduce(a, c) | firstEnd(a) <= firstStart(c)]


[slack(a:Action, c:CausalLink) : integer => lastStart(c) - (firstEnd(a) + getDistance(a, c))]
[slack(c:CausalLink, a:Action) : integer => slack(c.consumer, a)]
[slack(a1:Action, a2:Action) : integer => lastStart(a2) - (firstEnd(a1) + getDistance(a1, a2))]


// ********************************************************************
// *                                                                  *
// *   Part 6: Distances computation                                  *
// *                                                                  *
// ********************************************************************



// ********************************************
// *  Computation of h1 between action pairs  *
// ********************************************

[computeH1Distances(pb:PlanningProblem) : void ->
  let actions := pb.actions, fluents := pb.fluents in (
  for i in (3 .. length(actions)) (
    let a := actions[i] in (
	for a in actions (
	  a.tinit := MAXINT,
	  a.reachable := (a.num != 1 &  length(a.prec) = 0)),
	for f in fluents (
	  f.tinit := MAXINT,
	  if (not(deletes(a, f)) | produces(a, f)) updateCost(f, 0)),
	computeH1Cost(actions, firstStart(a) + getDuration(a)),
	;for a' in actions printf("~S ~S ===> ~S\n", a, a', a'.tinit),
	for a' in actions setDistance(a, a', a'.tinit))))]

[computeH1Cost(actions:list<Action>, start:integer) : void ->
  let loop := true in (
      while loop (
	loop := false,
	for a in list{a in actions | a.reachable} (
	  a.reachable := false,
	  let cost := (if a.prec max(list{f.tinit | f in a.prec}) else (if a.isevent (a.tinitEvent - start) else 0)) in (
;	  let cost := (if a.prec max(list{f.tinit | f in a.prec}) else 0) in (
	      if (cost < a.tinit) (
		loop := true,
		a.tinit := cost,
		cost :+ a.duration,
		for f in a.add updateCost(f, cost))))))]

[updateCost(f:Fluent, cost:integer) : void =>
  if (cost < f.tinit) (
    f.tinit := cost,
    for a in f.consumers a.reachable := true)]


// ********************************************
// *  Computation of h2 between action pairs  *
// ********************************************

[computeH2Distances(pb:PlanningProblem) : void ->
  let actions := pb.actions, fluents := pb.fluents, nfluents := length(fluents) in (
  for i in (3 .. length(actions)) (
    let a := actions[i] in (
	#if PRETTY (printf("~S/~S~A", i, length(actions), begl), fflush()),
	world+(),
	for a in actions (a.reachable := (a.num != 1 &  length(a.prec) = 0)),
	for i in (1 .. nfluents) (
	  for j in (i .. nfluents) (
	    if ((not(deletes(a, fluents[i])) | produces(a, fluents[i])) &
		(not(deletes(a, fluents[j])) | produces(a, fluents[j]))) (
	      updateCost(fluents[i], fluents[j], 0)))),
	computeH2Cost(actions, fluents, firstStart(a) + getDuration(a)),
	for a' in actions setDistance(a, a', a'.tinit),
	#if PRETTY printf("Computing distances........... "),
	world-())))]


// **************************************************
// *  Computation of h2 for the initial state only  *
// **************************************************

[computeInitH0Cost(actions:list<Action>, fluents:list<Fluent>) : void ->
  for a in actions (a.tinit := 0),
  for f1 in fluents for f2 in fluents setPairCost(f1, f2, 0),
  actions[1].tinit := 0]

[computeInitH1Cost(actions:list<Action>, fluents:list<Fluent>) : void ->
  for a in actions (a.tinit := MAXINT, a.reachable := (a.num != 1 &  length(a.prec) = 0)),
  for f in fluents (f.tinit := MAXINT),
  for f in actions[1].add updateCost(f, 0),
  computeH1Cost(actions, 0),
  for f1 in fluents for f2 in fluents setPairCost(f1, f2, max(f1.tinit, f2.tinit)),
  actions[1].tinit := 0]

[computeInitH2Cost(actions:list<Action>, fluents:list<Fluent>) : void ->
  for a in actions (a.reachable := (a.num != 1 &  length(a.prec) = 0)),
  for i in (1 .. length(actions[1].add)) (
    for j in (i .. length(actions[1].add)) (
      updateCost(actions[1].add[i], actions[1].add[j], 0))),
  computeH2Cost(actions, fluents, 0),
  actions[1].tinit := 0]


// ******************************
// *  h2 computation procedure  *
// ******************************

[computeH2Cost(actions:list<Action>, fluents:list<Fluent>, start:integer) : void =>
  let loop := true in (
      while loop (
	loop := false,
	for a in list{a in actions | a.reachable} (
 	  let cost := 0, aprec := a.prec, aadd := a.add, aduration := a.duration in (
	      loop := true,
	      a.reachable := false,
	      for i in (1 .. length(aprec)) for j in (i .. length(aprec)) cost := max(cost, getPairCost(aprec[i], aprec[j])),
	      a.tinit := max(cost, a.tinitEvent - start),
	      cost :+ aduration,
	      for i in (1 .. length(aadd)) for j in (i .. length(aadd)) updateCost(aadd[i], aadd[j], cost),
	      for f1 in list{f1 in fluents | not(deletes(a, f1))} (
		cost := a.tinit,
		for f2 in aprec cost := max(cost, getPairCost(f1, f2)),
		cost :+ aduration,
		for f2 in aadd updateCost(f1, f2, cost),
		for a' in list{a' in f1.producers | not(isMutex(a, a'))} (
		  let a'prec := a'.prec in (
		      cost := max(a.tinit, a'.tinit),
		      for f2 in aprec for f3 in a'prec cost := max(cost, getPairCost(f2, f3)),
		      if (a'.duration <= aduration) cost := max(a.tinit + aduration, cost + a'.duration)
		      else cost := max(a'.tinit + a'.duration, cost + aduration),
		      for f2 in aadd updateCost(f1, f2, cost))))))))]

[updateCost(f1:Fluent, f2:Fluent, cost:integer) : void =>
  if (getPairCost(f1, f2) > cost) (
    setPairCost(f1, f2, cost),
    for a in (f1.consumers /+ f2.consumers) a.reachable := true)]


// ******************************************************
// *  Writing and reading distances to and from a file  *
// ******************************************************

[writeDistances(pb:PlanningProblem) : void ->
  let p := fopen("." /+ pb.session.operators /+ "." /+ pb.session.facts /+ ".dist", "w") in (
      for a in pb.actions (
	for a' in pb.actions (
	  write_int(getDistance(a, a'), p))),
      fclose(p))]

[readDistances(pb:PlanningProblem) : void ->
  let p := fopen("." /+ pb.session.operators /+ "." /+ pb.session.facts /+ ".dist", "r") in (
      for a in pb.actions (
	for a' in pb.actions (
	  setDistance(a, a', read_int(p)))),
      fclose(p))]


// ********************************************************************
// *                                                                  *
// *   Part 7: Planning problems                                      *
// *                                                                  *
// ********************************************************************



// Fields:
// - fluents: list of all grounded fluents of the planning problem
// - actions: list of all grounded actions of the planning problem
// - startAction: dummy action representing the initial state
// - endAction: dummy action representing the goal
// - causalLinks: list of all causal linksof the problem
// - activeCausals: backtrackable list of causal links used in the current partial plan
// - activeActions: backtrackable list of actions used in the current partial plan
// - mutexSets: list of mutex sets (greedily) built and registered
// - cpt....: counters for printing statistics
// - session: options of the problem, ops and facts files, ...

Instance <: object

PlanningProblem <: Problem(
  instance:Instance,
  fluents:list<Fluent>,
  actions:list[Action],
  events:list<Action>,
  startAction:Action,
  endAction:Action,
  causalLinks:list[CausalLink],
  activeCausals:list[CausalLink], 
  activeActions:list[Action], 
  mutexSets:list,
  nbActionsMore:integer,
  nbCausalsMore:integer,
  nbSolvedThreats:integer,
  cptChoiceSupport:Counter,
  cptChoiceConflict:Counter,
  cptChoiceMutex:Counter,
  session:Session)

[makePlanningProblem(s:Session) : PlanningProblem -> 
  time_set(),
  let 
    inst := readProblem(s),
    nb_actions := length(inst.actions),
    nb_causals := sum(list{length(a.prec) | a in inst.actions}),
    max_precs := max(list{length(a.prec) | a in inst.actions}),
    nb_var := (nb_actions * 3) + nb_causals * 2,
    pb := PlanningProblem(
      instance = inst,
      fluents = inst.fluents,
      events = inst.events,
      startAction = inst.actions[1], 
      endAction = inst.actions[2],
      nbConstProblem = s.memory[1],
      nbConstVar = s.memory[2],
      nbActionsMore = s.memory[3],
      nbCausalsMore = s.memory[3] * max_precs,
      nbSolvedThreats = s.memory[4],
      cptChoiceSupport = Counter(),
      cptChoiceConflict = Counter(),
      cptChoiceMutex = Counter(),
      session = s)
  in (
      make_simple_bklist(pb, causalLinks, nb_causals + pb.nbCausalsMore, CausalLink, CausalLink()), 
      make_simple_bklist(pb, activeCausals, nb_causals + pb.nbCausalsMore, CausalLink, CausalLink()), 
      make_simple_bklist(pb, activeActions, nb_actions + pb.nbActionsMore, Action, Action()),
      make_simple_bklist(pb, actions, nb_actions + pb.nbActionsMore, Action, Action()),
      for a in inst.actions store(pb.actions, a),
      make_simple_bklist(pb, constraints, pb.nbConstProblem, AbstractConstraint, AbstractConstraint()),
      // Taken from makeProblem (chocapi.cl)
      pb.globalSearchSolver := GlobalSearchSolver(),
      attachPropagationEngine(pb, makeChocEngine(nb_var + pb.nbActionsMore + pb.nbCausalsMore)),
      setActiveProblem(pb),
      //
      if s.mutex_sets make_simple_bklist(pb, mutexSets, nb_actions, Action, Action()),
      for fluent in pb.fluents make_simple_bklist(fluent, causals, length(fluent.consumers) + pb.nbCausalsMore, CausalLink, CausalLink()),
      for action in pb.actions closeAction(pb, action),
      for fluent in pb.fluents closeFluent(pb, fluent),
      for action in pb.actions computeActionsMutex(action),
      for c in pb.causalLinks closeCausalLink(pb, c), 
     pb)]

[propagate(pb:PlanningProblem) : void ->
  let r := true in (
  propagate@Problem(pb),
  while r (
    r := makespan(pb),
    if r propagate@Problem(pb)),
  nil)]

