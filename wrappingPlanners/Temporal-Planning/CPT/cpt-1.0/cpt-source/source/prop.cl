// ********************************************************************
// * CHOCO, version 0.99 feb. 25th 2001                               *
// * file: iprop.cl                                                   *
// *    propagation of events of integer valued variables             *
// * Copyright (©) F. Laburthe, 1999-2000, see readme.txt             *
// ********************************************************************

// ------------------  File Overview  ---------------------------------
// *   Part 1: Propagation events                                     *
// *   Part 2: Propagation event queues                               *
// *   Part 3: from event generation to event post (to a prop. engine)*
// *   Part 4: from event generation to event post (SetVars)          *
// *   Part 4: utils for the queue storing INCINF/DECSUP events       *
// *   Part 5: posting an event to the queue                          *
// *   Part 6: retrieving an event & wakening the constraint          *
// *   Part 7: global constraints layered propagation                 *
// --------------------------------------------------------------------

                 
// ********************************************************************
// *   Part 1: Propagation events                                     *
// ********************************************************************

// v0.91 propagation events
// abstract class for propagation events on either constraints or variables 
PropagationEvent <: Ephemeral()

// An abstract class for all events related to a domain variable
VarEvent <: PropagationEvent(
    modifiedVar:AbstractVar,             // each event concerns a unique variable
    nextConst:list[integer]) // VV // index pointers coding a chained sublist of v.constraints
                                               // those constraints that should be waken upon handling the event
Instantiation <: VarEvent(
    cause:integer = 0)  // index of the constraint that caused the event
;DUMMY_INSTANTIATION :: Instantiation()
[self_print(e:Instantiation) : void
 -> princ("DUMMY_INST")]

    
// instantiating an integer variable
InstInt <: Instantiation()

[self_print(e:InstInt) : void
 -> let v := e.modifiedVar in printf("INST(~S):~S[c:~S]",v,v.value,e.cause)]

// instantiating a set variable
// updates to domains bouns of a variabl (inf/sup for integers; kernel/enveloppe for sets)
BoundUpdate <: VarEvent(
    cause:integer = 0,      // index of the constraint that caused the event
    idxInQueue:integer = 0) // index in the queue of all pending events

[self_print(e:BoundUpdate) : void -> printf("DUMMY-BOUND")]

// --- Bound updates for integer variables
// increasing the integer lower bound
IncInf <: BoundUpdate()

[self_print(e:IncInf) : void
 -> let v := e.modifiedVar in printf("INF(~S):~S[c:~S][i:~S]",v,v.inf,e.cause,e.idxInQueue)]

// decreasing the integer upper bound
DecSup <: BoundUpdate()

[self_print(e:DecSup) : void
 -> let v := e.modifiedVar in printf("SUP(~S):~S[c:~S][i:~S]",v,v.sup,e.cause,e.idxInQueue)]

// --- Bound updates for set variables
// increasing the set lower bound
// --- Removing values from the domain of integer variables
ValueRemovals <: VarEvent(
;    modifiedVar:IntVar,
    maxVals:integer = 3,    // max number of stored REMVAL(x,v,i)
                                    // before translation into REMVALS(x,i)
    nbVals:integer = 0,     // v1.0: set 0 as default value
    many:boolean = false,   // boolean flag (true  : one REMVALS(x,i) is stored,
                                    //               false : nbVals REMVAL(x,v,i) are stored)
    valueStack:list[integer],
    causeStack:list[integer],
    idxInQueue:integer = 0) // index in the queue of all pending ValueRemoval events
                                    //    (0 in case the event is absent from the queue)
;DUMMY_VALUE_REMOVAL :: ValueRemovals()
[self_print(e:ValueRemovals) : void
 -> when v := get(modifiedVar,e) in
      (if e.many
          printf("MANYREMS(~S)[c:~S][i:~S]",v,e.causeStack[1],e.idxInQueue)
       else (printf("REM(~S)[",v),
             for i in (1 .. e.nbVals)
                (if (i > 1) princ(", "),
                 printf("~S[~S]",e.valueStack[i],e.causeStack[i])),
             printf("][i:~S]",e.idxInQueue)))
    else princ("DUMMY-REM") ]

ConstAwakeEvent <: PropagationEvent(
    touchedConst:AbstractConstraint,
    initialized:boolean = false,
    event_idx:integer = 0 // VV
    )

[self_print(e:ConstAwakeEvent) : void
 -> when c := get(touchedConst,e) in
       (if e.initialized printf("AWAKE(~S)",c)
        else printf("INIT(~S)",c))
    else princ("DUMMY-AWAKE") ]



// ********************************************************************
// *   Part 5: Bipartite sets                                         *
// ********************************************************************

BipartiteSet <: Util(
  objs:list[ConstAwakeEvent],
  nbLeft:integer = 0)

[makeBipartiteSet(nb:integer) : BipartiteSet
-> let bp := BipartiteSet(nbLeft = 0) in (
  make_simple_bklist(bp, objs, nb, ConstAwakeEvent, ConstAwakeEvent()),
  bp)]

[swap(b:BipartiteSet,idx1:integer,idx2:integer) : void
->
  if (idx1 != idx2) (
    let obj1 := b.objs[idx1], obj2 := b.objs[idx2] in (
	store(b.objs, idx1, obj2),
	store(b.objs, idx2, obj1),
	put_store(event_idx, obj1, idx2, true),
	put_store(event_idx, obj2, idx1, true)))]

[moveLeft(b:BipartiteSet,obj:ConstAwakeEvent) : void
-> 
  if (obj.event_idx > b.nbLeft) 
    (swap(b,obj.event_idx,b.nbLeft + 1), b.nbLeft :+ 1)]

[moveRight(b:BipartiteSet,obj:ConstAwakeEvent) : void 
->
  if (obj.event_idx <= b.nbLeft) //not already in the right part
             (swap(b,obj.event_idx,b.nbLeft), b.nbLeft :- 1)]

[moveAllLeft(b:BipartiteSet) : void  -> b.nbLeft := length(b.objs)]      
[moveAllRight(b:BipartiteSet) : void -> b.nbLeft := 0]

[addRight(b:BipartiteSet,obj:ConstAwakeEvent) : void 
-> 
  store(b.objs, obj),
  put_store(event_idx, obj, length(b.objs), true)]

[addLeft(b:BipartiteSet,obj:ConstAwakeEvent) : void 
-> addRight(b,obj), moveLeft(b,obj)]

[isLeft(b:BipartiteSet,obj:ConstAwakeEvent) : boolean
-> obj.event_idx <= b.nbLeft]

[isIn(b:BipartiteSet,obj:ConstAwakeEvent) : boolean
-> obj.event_idx != 0 ]

[getNbLeft(b:BipartiteSet)  : integer => b.nbLeft]
[getNbRight(b:BipartiteSet) : integer => length(b.objs) - b.nbLeft]
[getNbObjects(b:BipartiteSet)  : integer => length(b.objs)]
[getObject(b:BipartiteSet,i:integer) : ConstAwakeEvent
 => assert(i >= 0 & i <= length(b.objs)), b.objs[i]]


[leftPart(b:BipartiteSet) : list  => list{b.objs[i] | i in (1 .. b.nbLeft)}]

[rightPart(b:BipartiteSet) : list => list{b.objs[i] | i in (b.nbLeft + 1 .. length(b.objs))}]

[Iterate(x:leftPart[tuple(BipartiteSet)],v:Variable,e:any)
=> let b:BipartiteSet := (eval(nth(args(x),1)) as BipartiteSet) in
      (for i in (1 .. b.nbLeft) 
          let v := b.objs[i] in e)]

[Iterate(x:rightPart[tuple(BipartiteSet)],v:Variable,e:any)
=> let b:BipartiteSet := (eval(nth(args(x),1)) as BipartiteSet) in
      (for i in (b.nbLeft + 1 .. length(b.objs)) 
          let v := b.objs[i] in e)]                             


// ********************************************************************
// *   Part 2: Propagation event queues                               *
// ********************************************************************

// the generic abstract class for storing sets of events
EventCollection <: Ephemeral(
    engine:PropagationEngine,
    qsize:integer = 0)

// an abstract subclass for storing events in a (cyclic) fifo queue
// instances q of subclasses from this class have the following invariant
//   (q.qLastRead != q.qLastEnqueued)
//        ==> forall i in (q.qLastRead .. q.qLastEnqueued) q.variableQueue != unknown
EventQueue <: EventCollection(
    qLastRead:integer = 0,
    qLastEnqueued:integer = 0,
    isPopping:boolean = false)  // 0.9907 <naren> are we currently popping events from the queue ?
    
// contains triples (awakeOnInf/awakeOnSup, v, idx1)
// handling a triple (awakeOnInf, v, i) leads to calling
//   awakeOnIncInf(v, x) for all x in ((1 .. v.nbConstraints) but i)
BoundEventQueue <: EventQueue(
    eventQueue:list[BoundUpdate],
    redundantEvent:boolean = false)
  // v0.9906: the redundantEvent flag replaced the propagationCycle exception:
  // it is set to true in case the event being popped generated a new event similar to itself

// instances q of this class have the following invariants
//   forall i in (1 .. q.qsize)
//      q.eventQueue[i] = REMVALS ==> (q.nbVals[i] = 1 & q.valuesStack[i] = 0)
//      q.eventQueue[i] = REMVAL
//           ==> forall j1,j2 in (1 .. q.nbVals[i]), j1 != j2,
//                      q.valuesStack[i][j1] != q.valuesStack[i][j2]
RemovalEventQueue <: EventQueue(
    eventQueue:list[ValueRemovals],
    redundantEvent:boolean = false)
    // <thb> v1.02: like BoundEvents, RemovalEvent can sometimes be redundant
    //   (the event being popped may generated a new event similar to itself)

// unlike the two previous queues, instantiation will be trailed (backtracked)
InstantiationStack <: EventCollection(
    eventQueue:list[Instantiation],
    sLastRead:integer = 0,
    sLastPushed:integer = 0)
// TODO: could be storedInt instead of integers ?.....
(store(sLastRead,sLastPushed))

// v0.9906
ConstAwakeEventQueue <: EventCollection(
    partition:BipartiteSet)

ChocEngine <: PropagationEngine(
   removalEvtQueue:RemovalEventQueue,
   boundEvtQueue:BoundEventQueue,
   instEvtStack:InstantiationStack,
   delayedConst:ConstAwakeEventQueue,  // v0.9906
   nbPendingInitConstAwakeEvent:integer = 0, // v1.07 : # of pending events for constraint initialization
   nbPendingVarEvent:integer = 0)         // v1.013: # of pending events on IntVar domain updates

// ********************************************************************
// *   Part 4: utils for the queue storing INCINF/DECSUP events       *
// ********************************************************************

// may be enriched for debugging purposes

// v1.0: internal methods of the propagation engine for handling contradictions
// port to claire 3 add the void range for all three methods
[raiseContradiction(pe:PropagationEngine) : void
 -> pe.contradictionCause := unknown, 
    flushCurrentOpenEvents(pe),
    contradiction!()]
[raiseContradiction(pe:PropagationEngine, x:Ephemeral) : void
 -> pe.contradictionCause := x, 
    flushCurrentOpenEvents(pe),
    contradiction!()]
// retrieving the cause of the last contradiction
[getContradictionCause(pe:PropagationEngine) : (Ephemeral U {unknown}) // VV
 -> pe.contradictionCause]

// v1.0 public methods for generating a contradiction
// port to claire 3 add the void range for all three methods
// v1.010: use getActiveProblem()
[raiseContradiction(v:AbstractVar) : void
 -> raiseContradiction(getActiveProblem().propagationEngine,v)]
[raiseContradiction(c:AbstractConstraint) : void
 -> raiseContradiction(getProblem(c).propagationEngine,c)]
[raiseContradiction(pb:Problem) : void
 -> raiseContradiction(pb.propagationEngine)]

// v0.9907
// claire3 port: strongly typed lists
[makeChocEngine(n:integer) : ChocEngine
 -> let m := 2 * n + 2,
        pe := ChocEngine(maxSize = n) in
      (pe.boundEvtQueue := BoundEventQueue(
              qsize = m,
              engine = pe,
;              eventQueue = list<BoundUpdate>{BoundUpdate() | i in (1 .. m)},
              eventQueue = make_list(m, BoundUpdate, BoundUpdate()),
              qLastRead = m,     // this is the same as 0 (cyclic iteration), but with a test
              qLastEnqueued = m),// against overwhelming
       pe.removalEvtQueue := RemovalEventQueue(
              qsize = n + 1, // v0.9903
              engine = pe,
;              eventQueue = list<ValueRemovals>{ValueRemovals() | i in (1 .. m)},
              eventQueue = make_list(m, ValueRemovals, ValueRemovals()),
              qLastRead = n + 1,        // v0.9907 <naren>: must be the last spot
              qLastEnqueued = n + 1),   // v0.9907 <naren>: must be the last spot
       pe.instEvtStack := InstantiationStack(
              qsize = n,
              engine = pe,
;              eventQueue = list<Instantiation>{Instantiation() | i in (1 .. m)},
              eventQueue = make_list(m, Instantiation, Instantiation()),
              sLastRead = 0,
              sLastPushed = 0),
   // v0.9906, v1.06 add default value parameter, v1.07: fill the engine slot
       pe.delayedConst := ConstAwakeEventQueue(engine = pe), // VV
       pe
      )]

[attachPropagationEngine(pb:Problem, pe:PropagationEngine) : void
 -> pb.propagationEngine := pe,
    pe.delayedConst.partition := makeBipartiteSet(pb.nbConstProblem), // VV
    pe.problem := pb]

[isEmpty(q:EventQueue) : boolean
 => (q.qLastRead = q.qLastEnqueued)]
[isEmpty(s:InstantiationStack) : boolean
 => (s.sLastRead = s.sLastPushed)]

// v0.9907
[popNextEvent(q:BoundEventQueue) : BoundUpdate
 => let i := (if (q.qLastRead >= q.qsize) 1 else q.qLastRead + 1) in
      (q.qLastRead := i,
       q.engine.nbPendingVarEvent :- 1,  // v1.013
       //[PDEBUG] just popped ~S : ~S // i, q.eventQueue[i],
       q.eventQueue[i] as BoundUpdate)]
       
[popNextEvent(q:RemovalEventQueue) : ValueRemovals
 => let i := (if (q.qLastRead >= q.qsize) 1 else q.qLastRead + 1) in
      (q.qLastRead := i,
       q.engine.nbPendingVarEvent :- 1,  // v1.013
       //[PDEBUG] just popped ~S : ~S // i, q.eventQueue[i],
       q.eventQueue[i] as ValueRemovals)]

[popNextEvent(q:InstantiationStack) : Instantiation
 => let i := q.sLastRead + 1 in 
      (assert(i <= q.qsize),
       q.sLastRead := i,
       q.engine.nbPendingVarEvent :- 1,  // v1.013
       //[PDEBUG] just popped ~S : ~S // i, q.eventQueue[i],
       q.eventQueue[i] as Instantiation)]


// the size of this queue can be bounded by 2*length(pb.vars)+1 => overflows are errors
[nextEventPostIndex(q:BoundEventQueue) : integer
 => let i := (if (q.qLastEnqueued = q.qsize) 1
              else q.qLastEnqueued + 1) in
        (if (i = q.qLastRead)
            (error("bound event fifo queue is full"), -1)
         else (q.qLastEnqueued := i, i))]

raiseOverflowWarning :: property()
// the size of this queue can be bounded by length(pb.vars)+1 => overflows are errors
[nextEventPostIndex(q:RemovalEventQueue) : integer
 => let i := (if (q.qLastEnqueued = q.qsize) 1
              else q.qLastEnqueued + 1) in
        (if (i = q.qLastRead)
            (raiseOverflowWarning(q.engine), -1)
         else (q.qLastEnqueued := i, i))]
[raiseOverflowWarning(pe:PropagationEngine) : void
 -> if not(pe.propagationOverflow)
      (pe.propagationOverflow := true,
       error("an overflow of value removals happened: this is very strange")
      )]
// the size of this queue can be bounded by length(pb.vars) => overflows are errors
[nextEventPostIndex(s:InstantiationStack) : integer
 => if (s.sLastPushed >= s.qsize)
       (error("instantiation event stack is full (top:~S, size:~S)",s.sLastPushed,s.qsize), -1)
    else (s.sLastPushed :+ 1, s.sLastPushed)]

// removes all the data on vars related to event pending in the queue.
// All this needs to be cleaned upon backtracking
// Note: only the information stored in vars (idxDecSupInQueue, idxIncInfInQueue) is cleaned
//       the queue itself is not cleaned (the events are not removed)
//       only the qLastRead pointer is reset
// In interpreted mode, a bunch of assertions are checked
// 1. cleaning the queue of INCINF/DECSUP
[flushEventQueue(q:BoundEventQueue) : void
 -> let eq := q.eventQueue, i := q.qLastRead, j := q.qLastEnqueued in
    (//[PVIEW] flush the eventQueue: del events ~S-~S (popping:~S) // i,j,q.isPopping,
     if q.isPopping           // there was an open event (code -1)
        let evt := (eq[i] as BoundUpdate) in // v1.02 cast
            (assert(evt.idxInQueue = -1 | (evt = eq[evt.idxInQueue] & i < evt.idxInQueue)), // VV
             if (evt.idxInQueue = -1) evt.idxInQueue := 0, // VV
             q.isPopping := false,
             q.redundantEvent := false)     // v0.9906
     else assert(q.redundantEvent = false), // v0.9906
     if (i != j) // the case (i=j) corresponds either to "no event" (when popping=false)
                 // or to "one single (currently open) event, no further pending events" (popping = true)
        (i :+ 1, if (i > q.qsize) i := 1,
                 // skip it and go to next pending event
        if (i <= j)              // we did not circle while posting events: straight iteration
          for k in (i .. j)      // all other events in the queue are pending // NAREN
             let evt := (eq[k] as BoundUpdate) in // v1.02 cast
               (//[PDEBUG] ~S:erase ~S[idx:~S] from queue // k,evt,evt.idxInQueue,
                assert(evt.idxInQueue = k),
                evt.idxInQueue := 0)
        else for k:integer in ((i .. q.qsize) /+ (1 .. j))  // we did circle while posting events :
                let evt := eq[k] in
                  (//[PDEBUG] ~S:erase ~S[idx:~S] from queue // k,evt,evt.idxInQueue,
                   assert(evt.idxInQueue = k),
                   evt.idxInQueue := 0),
        q.qLastRead := q.qLastEnqueued, // update pointers for all events have been erased
        assert(forall(v in q.engine.problem.vars |
                        v.updtInfEvt.idxInQueue = 0 & v.updtSupEvt.idxInQueue = 0))
        ) ) ]

// 2. cleaning the queue of REMVAL/REMVALS
[flushEventQueue(q:RemovalEventQueue) : void
 -> let eq := q.eventQueue, i := q.qLastRead, j := q.qLastEnqueued in
   (// v1.02 <thb> in case i=j (the queue looks empty), we might be processing the last unique pending event
    // while a contradiction occured -> therefore, just like for the BoundUpdateEventQueue, we need to clean that very event
    if q.isPopping           // there was an open event (code -1)
       let evt := (eq[i] as ValueRemovals) in // v1.02 cast
            (assert(evt.idxInQueue = -1),
             evt.idxInQueue := 0,
             q.isPopping := false), // v0.9907 <naren>: there was an open event
    if (i != j)  // something to clean only in case the queue is not empty
       (i :+ 1, if (i > q.qsize) i := 1,
        if (i <= j)              // we did not circle while posting events: straight iteration
          for k in (i .. j)      // all other events in the queue are pending // NAREN
             let evt := (eq[k] as ValueRemovals) in  // v1.02 cast
               (//[PDEBUG] ~S:erase ~S[idx:~S] from queue // k,evt,evt.idxInQueue,
                assert(evt.idxInQueue = k),
                evt.idxInQueue := 0,
                // removing all events between cursors i and j in interpreted mode yields an easier to read queue (useful for debugging)
                #if not(compiler.active?)
                   (eq[i] := ValueRemovals(), ;DUMMY_VALUE_REMOVAL,
                    let nbRems := evt.nbVals in
                        for r in (1 .. nbRems)
                           (evt.causeStack[r] := 0,
                            evt.valueStack[r] := 0),
                    evt.nbVals := 0)
               )
        else for k:integer in ((i .. q.qsize) /+ (1 .. j))  // we did circle while posting events :
               let evt := (eq[k] as ValueRemovals) in  // v1.02 cast
                 (//[PDEBUG] ~S:erase ~S[idx:~S] from queue // k,evt,evt.idxInQueue,
                  assert(evt.idxInQueue = k),
                  evt.idxInQueue := 0,
                  // removing all events between cursors i and j in interpreted mode yields an easier to read queue (useful for debugging)
                  #if not(compiler.active?)
                     (eq[i] := ValueRemovals(), ;DUMMY_VALUE_REMOVAL,
                      let nbRems := evt.nbVals in
                          for r in (1 .. nbRems)
                             (evt.causeStack[r] := 0,
                              evt.valueStack[r] := 0),
                      evt.nbVals := 0)),
        q.qLastRead := q.qLastEnqueued),
    assert(forall(v in q.engine.problem.vars | v.remValEvt.idxInQueue = 0))   )]

// 3. cleaning the queue of INSTANTIATE
// [flushEventQueue(q:InstantiationStack) : void
// There is nothing to flush in the stack of instantiations since the two cursors (sLastRead, sLastEnqueued) are stored
// as backtrackable values.

// v1.05 <thb> flush constraint event queues
[flushEventQueue(q:ConstAwakeEventQueue) : void
-> moveAllRight(q.partition)]

// A useful tool for debugging in interpreted mode
// checking that all links from variables to events are cleaned.
// v1.07 check pe.nbPendingInitConstAwakeEvent
[checkCleanState(pe:ChocEngine)
 => #if compiler.active? nil
    else let pb := pe.problem, aproblem := false in
      (if (pe.nbPendingInitConstAwakeEvent != 0)
         (printf("there are ~S pending initial const. awake Event",pe.nbPendingInitConstAwakeEvent),
          aproblem := true),
       if (pe.nbPendingVarEvent != 0)
         (printf("there are ~S pending int var Event",pe.nbPendingVarEvent),
          aproblem := true),
       for v0 in list{v in pb.vars | v.updtInfEvt.idxInQueue != 0}
         (printf("INCINF ~S has index ~S in the queue\n",v0,v0.updtInfEvt.idxInQueue),
          aproblem := true),
       for v0 in list{v in pb.vars | v.updtSupEvt.idxInQueue != 0}
         (printf("DECSUP ~S has index ~S in the queue\n",v0,v0.updtSupEvt.idxInQueue),
          aproblem := true),
       for v0 in list{v in pb.vars | v.remValEvt.idxInQueue != 0}
         (printf("REMVAL ~S has index ~S in the queue\n",v0,v0.remValEvt.idxInQueue),
          aproblem := true),
       if not(isEmpty(pe.delayedConst))
       		(printf("delayedConst is not empty\n"),
             aproblem := true),
       if aproblem error("stop and look why the queue is not empty !"))]

// v0.9906
getNextActiveEventQueue :: property(range = (EventCollection U {unknown}))

// v0.36 <fl> two new API functions
// v1.02 check that there are no pending events before pushing a world.
[pushWorld(pb:Problem) : void
 => when q := getNextActiveEventQueue(pb.propagationEngine) in
         error("it is forbidden to push a new world (~S) while there are pending events in queue:~S",world?(),q) // v1.02
    else (world+(),
          //[SDEBUG] CHOICE: ~S // world?()
         )]

[popWorld(pb:Problem) : void
 => world-(),
    checkCleanState(pb.propagationEngine),  // v1.02
    //[SDEBUG] BACKTRACK: ~S // world?()
    ]

// claire3 port world= -> backtrack
[setWorld(pb:Problem,n:integer) : void
 => backtrack(n),
    //[SDEBUG] BACKTRACK down to world ~S // n
    ]

[commitWorld(pb:Problem) : void
 => world-!(),
    //[SDEBUG] COMMIT //
    ]

// v1.0 now called by the raiseContradiction methods
// v1.05 <thb> flush also constraint based events
// v1.07 <fl> reset pe.nbPendingInitConstAwakeEvent
[flushCurrentOpenEvents(pe:ChocEngine)
 => flushEventQueue(pe.boundEvtQueue),
    flushEventQueue(pe.removalEvtQueue),
    flushEventQueue(pe.delayedConst),
    pe.nbPendingInitConstAwakeEvent := 0,
    pe.nbPendingVarEvent := 0,
    checkCleanState(pe)]

// ********************************************************************
// *   Part 5: posting an event to the propagation engine             *
// *     ChocEngine: <=/>= events are posted to a fifo queue          *
// *                 =/!= events are posted to a stack                *
// ********************************************************************

// interface functions
[postInstInt(pe:PropagationEngine,v:IntVar, i:integer) : void
 -> error("impossible to post INSTINT(~S,~S) to ~S",v,i,pe)]
[postRemoveVal(pe:PropagationEngine,v:IntVar, x:integer, i:integer) : void
 -> error("impossible to post REM(~S,~S,~S) to ~S",v,x,i,pe)]
[postUpdateInf(pe:PropagationEngine,v:IntVar, i:integer) : void
 -> error("impossible to post INCINF(~S,~S,~S) to ~S",v,i,pe)]
[postUpdateSup(pe:PropagationEngine,v:IntVar, i:integer) : void
 -> error("impossible to post DECSUP(~S,~S,~S) to ~S",v,i,pe)]
[postConstAwake(pe:PropagationEngine,c:AbstractConstraint, init:boolean) : void
 -> error("impossible to post AWAKE(~S,init:~S) to ~S",c,init,pe)]

// generic framwork of the four following methods:
//   get the right queue q for the event
//   get an index idx in the queue by calling nextEventPostIndex(q)
//   write all components of the event at q[idx]:
//      - the variable
//      - if needed, the kind of event (INCINF/DECSUP or REMVAL/REMVALS)
//      - the cause (index of hte constraint causing the event)
//      - if needed, the removed value (for REMVAL)
//   store the location of the event in the queue (idx) in the variable
//   + some additional optimization for removing doubles

[postInstantiateEvt(pe:ChocEngine,e:Instantiation, i:integer) : void
 -> //[PDEBUG] try to post INST ~S [cause:~S] // e,i,
    let iq := pe.instEvtStack, idx := nextEventPostIndex(iq) in
       (e.cause := i,
        //[PTALK]   ++ posted ~S:~S // idx,e,
        pe.nbPendingVarEvent :+ 1,        // v1.013
        store(iq.eventQueue,idx,e,true) )]

[postInstInt(pe:ChocEngine,v:IntVar, i:integer) : void
 => postInstantiateEvt(pe, v.instantiateEvt, i)]

[postRemoveVal(pe:ChocEngine,v:IntVar, x:integer, i:integer) : void
 -> //[PDEBUG] try to post REMOVAL ~S <> ~S [cause:~S] // v,x,i,
    let rq := pe.removalEvtQueue, e := v.remValEvt, idxQ := e.idxInQueue in
      (if (idxQ = 0)   // there are no event (REMVAL or REMVALS v) pending in the queue
         let idx := nextEventPostIndex(rq) in
            (//[PTALK]   ++ posted ~S:~S // idx,e,
             pe.nbPendingVarEvent :+ 1,        // v1.013
             e.idxInQueue := idx,
             e.many := false, // this event removes only one value and not an set of them,
             e.valueStack[1] := x,
             e.nbVals := 1,
             e.causeStack[1] := i,
             rq.eventQueue[idx] := e)
       else (// there is already such a similar event in the queue
             if (idxQ != -1) assert(rq.eventQueue[idxQ] = e),     // <naren> v1.0
             // v1.02 <thb> redesigned what happened when we post a REMOVAL s.t. similar REMOVAL are already pending in the queue
             if (e.many = true) // when a MANYREM event present,
                (if (idxQ = -1) // <thb> 1.02: a new REMOVAL during the propagation of a MANYREM is redundant
                    let idx := nextEventPostIndex(rq) in
                        (assert(e.nbVals = 0), // nbVals was 1 (MANYREM) and has been decremented by propagateEvent@ValueRemovals
                         //[PTALK]   ++ posted ~S:~S // idx,e,
                         e.idxInQueue := idx,       // store the new index for the new MANYREM event
                         e.nbVals := 1,             // integrity: always set to 1 with MANYREM events
                         rq.eventQueue[idx] := e,   // actual storage into the queue
                         rq.redundantEvent := true) // raises a flag so that the current iteration of the pending event can be stopped
                 else assert(e.nbVals = 1),    // <thb> v1.02: standard case, the event is not the current one,
                                               // so the integrity constraint (nbVals=1 for MANYREM events) holds
                 // in any case, since there is already such an event in the queue,  we reset the cause
                 if (e.causeStack[1] != i)
                     e.causeStack[1] := 0)
             else let nbRems := e.nbVals + 1 in
                (if (nbRems <= e.maxVals) // this is a simple REMOVAL event
                    (//<thb> Even if it occurs during the propagation of removals, this new removal cannot be redundant
                     assert(nbRems > 0),
                     e.nbVals :+ 1,
                     e.valueStack[nbRems] := x,
                     e.causeStack[nbRems] := i)
                 else (assert(nbRems = e.maxVals + 1),
                       //[PTALK] event abstraction REMVAL(~S) ~S and ~S  -> REMVALS // v,e.valueStack,x,
                       e.many := true, 
                       e.nbVals := 1,
                       e.valueStack[1] := x,
                       e.causeStack[1] := i,                       
                       rq.engine.propagationOverflow := true,
                       if (idxQ = -1) // <thb> a MANYREMS is redudant with any REMOVAL event on the same var
                          let idx := nextEventPostIndex(rq) in
                             (//[PTALK]   ++ posted ~S:~S // idx,e,
                              e.idxInQueue := idx,
                              rq.eventQueue[idx] := e, 
                              rq.redundantEvent := true),                                                  
                       if exists(j in (2 .. nbRems - 1) | e.causeStack[j] != i)
                          e.causeStack[1] := 0,
                       #if not(compiler.active?)
                          (e.valueStack[1] := 0,
                           for i in (2 .. nbRems - 1)
                             (e.causeStack[i] := 0,
                              e.valueStack[i] := 0))
                       )))) ]

// Before posting an event, we test whether this one is already in the queue.
// If it is, but was pushed in by another constraint, we forget the constraint that pushed it in.
[private/postBoundEvent(pe:ChocEngine,e:BoundUpdate, i:integer) : void
  -> //[PDEBUG] try to post ~S ~S // e,i,
     let bq := pe.boundEvtQueue, idxQ := e.idxInQueue in
      (if (idxQ <= 0)       // the event (INCINF v) is not a pending event in the queue
         let idx := nextEventPostIndex(bq) in
            (e.cause := i,
             pe.nbPendingVarEvent :+ 1,        // v1.013
             //[PTALK]   ++ posted ~S:~S // idx,e,
             bq.eventQueue[idx] := e,
             e.idxInQueue := idx,
             if (idxQ = -1) bq.redundantEvent := true) // 0.9906
                 // we are trying to post an event similar (a stronger bound for the same variable)
                 // to the one being treated
       else (assert(bq.eventQueue[idxQ] = e),
             //[PTALK] ~S already in queue at index ~S: change cause from ~S to ~S // e,idxQ,e.cause,i,
             e.cause := i) )]
        // we are trying to post an event similar to a pending event
        // -> if both events have the same cause, then one of the posts is ignored
        //    if both events have different causes (i0 and i), the the i-th constraint of v
        //    (the cause of the strongest -ie most recent- event) must not be re-waken
        //    Therefore, we always keep the the cause of the most recent event (and forget the previous one)

[postUpdateInf(pe:ChocEngine,v:IntVar, i:integer) : void
 => postBoundEvent(pe,v.updtInfEvt, i)]

[postUpdateSup(pe:ChocEngine,v:IntVar, i:integer) : void
 => postBoundEvent(pe,v.updtSupEvt, i)]

// v1.0: simple util
[getQueue(pe:ChocEngine, evt:ConstAwakeEvent) : ConstAwakeEventQueue
 => pe.delayedConst]
    
// v0.9907
[registerEvent(pe:ChocEngine, evt:ConstAwakeEvent) : void
 -> let q := getQueue(pe,evt) in
     (if isIn(q.partition,evt) error("event ~S is already attached to engine ~S",evt,pe)
      else addRight(q.partition, evt))]
       
// v0.9907
[postConstAwake(pe:ChocEngine,c:AbstractConstraint, init:boolean) : boolean
 -> let evt := c.constAwakeEvent,
        q := getQueue(pe,evt) in
      (if not(isLeft(q.partition, evt))  // check that the event is not already pending (do not erase an init while posting a not(init))
         (evt.initialized := not(init),
          moveLeft(q.partition,evt),
          if init pe.nbPendingInitConstAwakeEvent :+ 1,  // v1.07
          true)
       else false)]

// v1.0: used for removing an event from the queue without propagating it
[remove(q:ConstAwakeEventQueue, evt:ConstAwakeEvent) : void
 -> let ptn := q.partition in
      (if isLeft(q.partition, evt) moveRight(ptn, evt))]

// All posts are generated by IntVars, except constevent posts
// API
[constAwake(c:AbstractConstraint, init:boolean) : void
 -> //[PDEBUG] awake ~S // c,
    postConstAwake(getProblem(c).propagationEngine,c,init)]
 
// ********************************************************************
// *   Part 6: retrieving an event & wakening the constraint          *
// ********************************************************************

// v0.9: this part was fully rewritten
propagateEvent :: property()

// 1. Most preemptive events: instantiations
// propagate all instantiations
[popSomeEvents(q:InstantiationStack) : void
 -> let evtq := q.eventQueue in
      (//[PVIEW]   oo propagate INST events on basic constraints // ,
       while not(isEmpty(q))
             propagateEvent(popNextEvent(q)) )]   // v0.9907
              
// v0.9906 propagateInstantiation recast into propagateEvent (and goes from private to choco)
// for all pending INST(v,idx), performs an iteration similar to
//       for k in ((1 .. idx - 1) /+ (idx + 1 .. n))
//             doAwakeOnInst(v.constraints[k], v.indices[k])
// but using the sub-cycle of active constraints from v.constraints, coded by nextConstOnInst
// v0.9906: propagateAllInstantiations recast into popSomeEvents
[propagateEvent(e:Instantiation) : void
 -> let v := e.modifiedVar, n := v.nbConstraints, lc := v.constraints, li := v.indices in
      (//[PTALK]   -- popped ~S // e,            
       if (n > 0)
          let lnext := e.nextConst, i1 := e.cause, prevk := i1 ,
              k := (if (i1 = 0) lnext[n] else lnext[i1]) in
            (if ( (k > 0) & (k != i1) )   // the constraint cycle is not empty nor reduced to i1
                  (while (k > prevk)  // first part of the iteration: till the end
                     (//[PDEBUG]      doAwakeOnInst(~S,~S) // lc[k], li[k],
                      doAwakeOnInst(lc[k], li[k]),
                      prevk := k, k := lnext[k]),
                   prevk := 0,
                   while (k > prevk & k < i1)
                     (//[PDEBUG]      doAwakeOnInst(~S,~S) // lc[k], li[k],
                      doAwakeOnInst(lc[k], li[k]),
                      prevk := k, k := lnext[k]))))]

// V0.9906 propagateRemoval recast into popSomeEvents
// pop value removals one by one
[popSomeEvents(q:RemovalEventQueue) : void
 => propagateEvent(popNextEvent(q),q)]  // v0.9907
      
// 2a. Removal events
// for all pending REMOVAL(v,x,idx), performs an iteration similar to
//       for k in ((1 .. idx - 1) /+ (idx + 1 .. n))
//             doAwakeOnRem(v.constraints[k],x,v.indices[k])
// but using the sub-cycle of active constraints from v.constraints, coded by nextConstOnRem
// returns a boolean indicating if there were some events to propagate
// v1.02 <thb> add the ability for a new incoming event to interrupt the loops (in case of redundant MANYREM events)
[propagateEvent(e:ValueRemovals,q:RemovalEventQueue) : void
 -> let v := e.modifiedVar, n := v.nbConstraints, lc := v.constraints, li := v.indices in
      (e.idxInQueue := -1, // <naren> 1.0 set index to -1 during propagation (like for BoundUpdates)
       if (n > 0)
          let lnext := e.nextConst in
             (//[PTALK]   -- popped ~S // e,
              while (e.nbVals > 0)
                let nbv := e.nbVals,
                    i1 := e.causeStack[nbv],
                    prevk := i1 ,
                    k := (if (i1 = 0) lnext[n] else lnext[i1]) in
                  (e.nbVals :- 1,
                   if  ( (k > 0) & (k != i1) )   // the constraint cycle is not empty nor reduced to i1
                      (q.isPopping := true,    // v0.9907 <naren>
                       if e.many
                         (assert(nbv = 1),
                          while (k > prevk & not(q.redundantEvent))  // first part of the iteration: till the end
                           (//[PDEBUG]      doAwakeOnVar(~S) // lc[k],
                            doAwakeOnVar(lc[k],li[k]),
                            prevk := k, k := lnext[k]),
                          prevk := 0,
                          while (k > prevk & k < i1 & not(q.redundantEvent))      // second part: from the beginning to i1
                           (//[PDEBUG]      doAwakeOnVar(~S) // lc[k],
                            doAwakeOnVar(lc[k],li[k]),
                            prevk := k, k := lnext[k]))
                       else let x := e.valueStack[nbv] in
                         ( // v0.9907 <naren>
                           // removed that test: if (x > v.inf & x < v.sup)
                           // we assume that it is the constraint job to compare x to the current domain
                           while (k > prevk & not(q.redundantEvent))  // first part of the iteration: till the end
                             (//[PDEBUG]      doAwakeOnRem(~S,~S,~S) // lc[k], li[k],x,
                              doAwakeOnRem(lc[k], li[k],x),
                              prevk := k, k := lnext[k]),
                           prevk := 0,
                           while (k > prevk & k < i1 & not(q.redundantEvent))      // second part: from the beginning to i1
                             (//[PDEBUG]      doAwakeOnRem(~S,~S,~S) // lc[k], li[k],x,
                              doAwakeOnRem(lc[k], li[k],x),
                              prevk := k, k := lnext[k])),
                       q.isPopping := false))),  // v0.9907 <naren>
       if q.redundantEvent
           (//[PVIEW] propagation cycle with ~S // e,
            q.redundantEvent := false)
       else e.idxInQueue := 0)] // in this branch of the "if": v is absent from the queue

// v0.9906 propagateBoundEvent recast into popSomeEvents
[popSomeEvents(q:BoundEventQueue) : void
 => propagateEvent(popNextEvent(q),q)] // v0.9907
       
// 2b. Bound events (INCINF/DECSUP)
// propagate pops triplets from the queue until it is empty
// before popping a quadruplet, the index corresponding to this event is reset to 0
// Note (FL 30.11.98): we avoid doubles between the part of the iteration from the current triplet
// that remains to be done and the following triplets, by raising the PropagationCycle exception.
// returns a boolean indicating if there were some events to propagate
// v0.9906 replaced the propagationCycle exception by the redundantEvent flag 
// 1.330: now contains INC/INF events on integers, but also KER/ENV event on set vars
[propagateEvent(e:BoundUpdate, q:BoundEventQueue) : void       
 -> let v := e.modifiedVar, n := v.nbConstraints, lc := v.constraints, li := v.indices in
     (q.isPopping := true,   // record that we start popping propagation events,
      if (n = 0)             // <michel> 0.15: empty list of constraints => do not evaluate i1,k,...
         e.idxInQueue := 0   // <naren>  0.25: it was ill-fixed in 0.15
      else (//[PTALK]   -- popped ~S // e,
            let lnext := e.nextConst, i1 := e.cause,
                prevk := i1 ,
                k := (if (i1 = 0) lnext[n] else lnext[i1]) in
              (if ((length(lc) = 0) | (k = 0) | (k = i1))    // empty list of constraints or solely lc[i1] // VV for length(lc)
                  e.idxInQueue := 0          // <naren>
               else (e.idxInQueue := -1,     // record that we are iterating (-1)
                     case e
                           (IncInf (while (k > prevk & not(q.redundantEvent))  // first part of the iteration: till the end
                                          (//[PDEBUG]      doAwakeOnInf(~S,~S) // lc[k],li[k],
                                           doAwakeOnInf(lc[k],li[k]),
                                           prevk := k, k := lnext[k]),
                                    prevk := 0,
                                    while (k > prevk & k < i1 & not(q.redundantEvent))      // second part: from the beginning to i1
                                          (//[PDEBUG]      doAwakeOnInf(~S,~S) // lc[k],li[k],
                                           doAwakeOnInf(lc[k],li[k]),
                                           prevk := k, k := lnext[k])),
                            DecSup (while (k > prevk & not(q.redundantEvent))  // first part of the iteration: till the end
                                          (//[PDEBUG]      doAwakeOnSup(~S,~S) // lc[k],li[k],
                                           doAwakeOnSup(lc[k],li[k]),
                                           prevk := k, k := lnext[k]),
                                    prevk := 0,
                                    while (k > prevk & k < i1 & not(q.redundantEvent))      // second part: from the beginning to i1
                                          (//[PDEBUG]      doAwakeOnSup(~S,~S) // lc[k],li[k],
                                           doAwakeOnSup(lc[k],li[k]),
                                           prevk := k, k := lnext[k]))),                                           
                     if q.redundantEvent
                       (//[PVIEW] propagation cycle with ~S // e,
                        q.redundantEvent := false)
                     else e.idxInQueue := 0 // in this branch of the "if": v is absent from the queue    
                     ))),
      q.isPopping := false )]

// v0.9906
[isEmpty(q:ConstAwakeEventQueue) : boolean
 => (getNbLeft(q.partition) = 0)]
[popSomeEvents(q:ConstAwakeEventQueue) : void
 => propagateEvent(popNextEvent(q))] // v0.9907
// v0.9907
[popNextEvent(q:ConstAwakeEventQueue) : ConstAwakeEvent
 => let bp := q.partition, idx := getNbLeft(bp), // idx := 1 + random(getNbLeft(bp)),  // v1.013 trying another strategy (lifo)
        e := (getObject(bp,idx) as ConstAwakeEvent) in
       (if not(e.initialized) (q.engine as ChocEngine).nbPendingInitConstAwakeEvent :- 1,  // v1.07
        moveRight(bp,e),e)]
[propagateEvent(e:ConstAwakeEvent) : void
 -> //[PTALK]   -- popped ~S // e,
    if e.initialized propagate(e.touchedConst)
    else doAwake(e.touchedConst)]


// v1.013: fast dispatch of generic event collection methods
(interface(popSomeEvents))
(interface(EventCollection,popSomeEvents))
[popSomeEvents(q:EventCollection) : void
 -> error("popSomeEvents not defined for abstract class EventCollection")]

// v0.9906
;(put(range,getNextActiveEventQueue,(EventCollection U {unknown})))

[getNextActiveEventQueue(pe:PropagationEngine) : (EventCollection U {unknown})
 -> error("getNextActiveEventQueue not defined for abstract propagation engine"), unknown]

// v1.07: new priority mechanism: ensure that all constraints have been initialized (initial propagation)
// before handling any variable-based event
[getNextActiveConstraintEventQueue(pe:ChocEngine) : (EventCollection U {unknown})
 => let cq1 := pe.delayedConst in
      (if not(isEmpty(cq1)) cq1
       else unknown)]

[getNextActiveVariableEventQueue(pe:ChocEngine) : (EventCollection U {unknown})
 => let iq := pe.instEvtStack in
      (if not(isEmpty(iq)) iq
       else let bq := pe.boundEvtQueue in 
        (if not(isEmpty(bq)) bq
         else let rq := pe.removalEvtQueue in
          (if not(isEmpty(rq)) rq
           else unknown)))]

// explicit handling of event priorities: returns an event queue that contains pending
// propagation events (either on variables or on constraints)
// v1.013 the pe.nbPendingVarEvent flag is used as a shortcut to check whether any of
// the VarEventQueues contains pending events
[getNextActiveEventQueue2(pe:ChocEngine) : (EventCollection U {unknown})
 -> if (pe.nbPendingInitConstAwakeEvent > 0)
       (when q := getNextActiveConstraintEventQueue(pe) in q
        else (error("~S pending init events and no active constraint event queue",pe.nbPendingInitConstAwakeEvent),
              unknown))
    else if (pe.nbPendingVarEvent > 0)
             (when q := getNextActiveVariableEventQueue(pe) in q
              else (error("~S pending var events but none in the queues",pe.nbPendingVarEvent),
                    unknown))
    else getNextActiveConstraintEventQueue(pe)]

[getNextActiveEventQueue(pe:ChocEngine) : (EventCollection U {unknown})
 => if (pe.nbPendingInitConstAwakeEvent > 0)
   getNextActiveConstraintEventQueue(pe) 
    else if (pe.nbPendingVarEvent > 0)
             getNextActiveVariableEventQueue(pe)
    else unknown]

(interface(getNextActiveEventQueue))
(interface(PropagationEngine,getNextActiveEventQueue))
          

