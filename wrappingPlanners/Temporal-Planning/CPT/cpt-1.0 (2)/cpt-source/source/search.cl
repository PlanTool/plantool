// ********************************************************************
// * CHOCO, version 0.99 feb. 25th 2001                               *
// * file: main.cl                                                    *
// *    solving                                                       *
// * Copyright (©) F. Laburthe, 1999-2000, see readme.txt             *
// ********************************************************************

// ------------------  File Overview  ---------------------------------
// * A. Global search                                                 *
// *   Part 0: object model                                           *
// *   Part 1: utils and solution management                          *
// *   Part 2: exploring  one level of branching                      *
// *   Part 3: Feeding the engine with a library of branching objects *
// *   Part 4: Using branching classes within globalSearchSolver's    *
// * B. Local search (generating assignments, improving them by LO)   *
// *   Part 5: utils and solution management                          *
// *   Part 6: Feeding the engine with a library of neighborhoods     *
// *   Part 7: Using neighborhood classes within LocalSearchSolver's  *
// --------------------------------------------------------------------

// ********************************************************************
// *   Part 0: object model                                           *
// ********************************************************************

;AbstractBranching <: Ephemeral(
;        manager:GlobalSearchSolver,
;        nextBranching:AbstractBranching)

// v1.013 the range of the abstract interface properties needs be specified
// v1.013 add a parameter to getNextBranch and finishedBranching (index of the previous branch),
//        introduce getFirstBranch
selectBranchingObject :: property(range = any)
goDownBranch :: property(range = void)
traceDownBranch :: property(range = void)
getFirstBranch :: property(range = integer)  // v1.013
getNextBranch :: property(range = integer)
goUpBranch :: property(range = void)
traceUpBranch :: property(range = void)
finishedBranching :: property(range = boolean)
branchOn :: property(range = boolean)

// v0.9906
[selectBranchingObject(b:AbstractBranching) : any
 -> error("selectBranchingObject not defined on the abstract class AbstractBranching"), unknown]
[goDownBranch(b:AbstractBranching,x:any,i:integer) : void
 -> error("goDownBranch not defined on the abstract class AbstractBranching (called with ~S,~S)",x,i)]
[traceDownBranch(b:AbstractBranching,x:any,i:integer) : void
 -> error("traceDownBranch not defined on the abstract class AbstractBranching (called with ~S,~S)",x,i)]
// v0.9906: <thb> definitions required in order to have dynamic dispatch of the above interface methods
[getFirstBranch(b:AbstractBranching,x:any) : integer          // v1.013
 -> error("getFirstBranch not defined on the abstract class AbstractBranching (called with ~S)",x), 0]
[getNextBranch(b:AbstractBranching,x:any,i:integer) : integer // v1.013
 -> error("getNextBranch not defined on the abstract class AbstractBranching (called with ~S)",x), 0]
[goUpBranch(b:AbstractBranching,x:any,i:integer) : void
 -> error("goUpBranch not defined on the abstract class AbstractBranching (called with ~S,~S)",x,i)]
[traceUpBranch(b:AbstractBranching,x:any,i:integer) : void
 -> error("traceUpBranch not defined on the abstract class AbstractBranching (called with ~S,~S)",x,i)]
[finishedBranching(b:AbstractBranching,x:any,i:integer) : boolean
 -> error("finishedBranching not defined on the abstract class AbstractBranching (called with ~S)",x), true]
[branchOn(b:AbstractBranching,v:any,n:integer) : boolean
 -> error("branchOn not defined on the abstract class AbstractBranching (called with ~S,~S)",v,n), false]
 
// v1.0
(interface(selectBranchingObject),
 interface(getFirstBranch),
 interface(getNextBranch),
 interface(goDownBranch),
 interface(traceDownBranch),
 interface(goUpBranch),
 interface(traceUpBranch),
 interface(finishedBranching),
 interface(branchOn))
 
LargeBranching <: AbstractBranching()
BinBranching <: AbstractBranching()

// v1.013 using a binary branching object as a large branching one
[goUpBranch(b:BinBranching,x:any,i:integer) : void -> nil]
[traceUpBranch(b:BinBranching,x:any,i:integer) : void -> nil]
[getFirstBranch(b:BinBranching,x:any) : integer -> 1]
[getNextBranch(b:BinBranching,x:any,i:integer) : integer -> 2]
[finishedBranching(b:BinBranching,x:any,i:integer) : boolean -> (i = 2)]

[goUpBranch(b:LargeBranching,x:any,i:integer) : void -> nil]
[traceUpBranch(b:LargeBranching,x:any,i:integer) : void -> nil]
[getFirstBranch(b:LargeBranching,x:any) : integer -> 1]
[getNextBranch(b:LargeBranching,x:any,i:integer) : integer -> 2]
[finishedBranching(b:LargeBranching,x:any,i:integer) : boolean -> (i = 2)]

// v1.013: a new class for branching objects
// that may alternatively branch from several sub-branching objects
CompositeBranching <: LargeBranching(
     alternatives:list<AbstractBranching>) // VV
// the only method that should be defined
[selectBranching(b:CompositeBranching) : AbstractBranching
 -> error("selectBranching not defined on the abstract class CompositeBranching"), b]
[selectBranchingObject(b:CompositeBranching) : any
 -> let alt := selectBranching(b) in // VV
   when x  := selectBranchingObject(alt) in tuple(alt,x) else unknown] // VV
[getFirstBranch(b:CompositeBranching,xpair:any) : integer
  -> let xp := (xpair as tuple(AbstractBranching,any)),
         alt := (xp[1] as AbstractBranching), x := xp[2] in 
      getFirstBranch(alt,x)]
[getNextBranch(b:CompositeBranching,xpair:any,i:integer) : integer
  -> let xp := (xpair as tuple(AbstractBranching,any)),
         alt := (xp[1] as AbstractBranching), x := xp[2] in 
      getNextBranch(alt,x,i)]
[goUpBranch(b:CompositeBranching,xpair:any,i:integer) : void
  -> let xp := (xpair as tuple(AbstractBranching,any)),
         alt := (xp[1] as AbstractBranching), x := xp[2] in 
      goUpBranch(alt,x,i)]
[traceUpBranch(b:CompositeBranching,xpair:any,i:integer) : void
  -> let xp := (xpair as tuple(AbstractBranching,any)),
         alt := (xp[1] as AbstractBranching), x := xp[2] in 
      traceUpBranch(alt,x,i)]
[goDownBranch(b:CompositeBranching,xpair:any,i:integer) : void
  -> let xp := (xpair as tuple(AbstractBranching,any)),
         alt := (xp[1] as AbstractBranching), x := xp[2] in 
      goDownBranch(alt,x,i)]
[traceDownBranch(b:CompositeBranching,xpair:any,i:integer) : void
  -> let xp := (xpair as tuple(AbstractBranching,any)),
         alt := (xp[1] as AbstractBranching), x := xp[2] in 
      traceDownBranch(alt,x,i)]
[finishedBranching(b:CompositeBranching,xpair:any,i:integer) : boolean
  -> let xp := (xpair as tuple(AbstractBranching,any)),
         alt := (xp[1] as AbstractBranching), x := xp[2] in 
      finishedBranching(alt,x,i)]

(abstract(selectBranching)) // VV

// v1.318: introducing heuristics
VarSelector <: Ephemeral(
      branching:AbstractBranching)
[selectVar(vs:VarSelector) : (IntVar U {unknown}) -> error("selectVar is not defined on ~S",vs), unknown]
(interface(selectVar))

MinDomain <: VarSelector(problem:Problem, vars:list<IntVar>)
[selectVar(vs:MinDomain) : (IntVar U {unknown})
 -> if (length(vs.vars) > 0) getSmallestDomainUnassignedVar(vs.vars)
    else getSmallestDomainUnassignedVar(getProblem(getSearchManager(vs.branching)))]

MaxDeg <: VarSelector(problem:Problem, vars:list<IntVar>)
[selectVar(vs:MaxDeg) : (IntVar U {unknown})
 -> if (length(vs.vars) > 0) getMostConstrainedUnassignedVar(vs.vars)
    else getMostConstrainedUnassignedVar(getProblem(getSearchManager(vs.branching)))]

DomDeg <: VarSelector(problem:Problem, vars:list<IntVar>)
[selectVar(vs:DomDeg) : (IntVar U {unknown})
 -> if (length(vs.vars) > 0) getDomOverDegBestUnassignedVar(vs.vars)
    else getDomOverDegBestUnassignedVar(getProblem(getSearchManager(vs.branching)))]

StaticVarOrder <: VarSelector(vars:list[IntVar])
[selectVar(vs:StaticVarOrder) : (IntVar U {unknown})
 -> some(v in vs.vars | not(isInstantiated(v)))]

ValIterator <: Ephemeral(
      branching:AbstractBranching)
[isOver(vi:ValIterator, x:IntVar, i:integer) : boolean -> error("isOver is not defined on ~S",vi), true]
[getFirstVal(vi:ValIterator, x:IntVar) : integer -> error("getFirstVal is not defined on ~S",vi), 0]
[getNextVal(vi:ValIterator, x:IntVar, i:integer) : integer -> error("getNextVal is not defined on ~S",vi), 0]
(interface(isOver),interface(getFirstVal),interface(getNextVal))

IncreasingDomain <: ValIterator()
[isOver(vi:IncreasingDomain, x:IntVar, i:integer) : boolean
 -> (i >= x.sup)]
[getFirstVal(vi:IncreasingDomain, x:IntVar) : integer
 -> x.inf]
[getNextVal(vi:IncreasingDomain, x:IntVar, i:integer) : integer
 -> getNextDomainValue(x,i)]

DecreasingDomain <: ValIterator()
[isOver(vi:DecreasingDomain, x:IntVar, i:integer) : boolean
 -> (i <= x.inf)]
[getFirstVal(vi:DecreasingDomain, x:IntVar) : integer
 -> x.sup]
[getNextVal(vi:DecreasingDomain, x:IntVar, i:integer) : integer
 -> getPrevDomainValue(x,i)]

ValSelector <: Ephemeral(
      branching:AbstractBranching)
[getBestVal(vh:ValSelector, x:IntVar) : integer -> error("getBestVal is not defined on ~S",vh), 0]
(interface(getBestVal))

MidValHeuristic <: ValSelector()
MinValHeuristic <: ValSelector()
MaxValHeuristic <: ValSelector()
[getBestVal(vh:MidValHeuristic, x:IntVar) : integer
 -> x.inf + (x.sup - x.inf) / 2]
[getBestVal(vh:MinValHeuristic, x:IntVar) : integer
 -> x.inf]
[getBestVal(vh:MaxValHeuristic, x:IntVar) : integer
 -> x.sup]

AssignVar <: LargeBranching(
     varHeuristic:VarSelector,
     valHeuristic:ValIterator)
SplitDomain <: BinBranching(
     varHeuristic:VarSelector,
     valHeuristic:ValSelector,
     dichotomyThreshold:integer = 5)
AssignOrForbid <: BinBranching(
     varHeuristic:VarSelector,
     valHeuristic:ValSelector)

Solve <: GlobalSearchSolver()

// v1.02: returns a boolean indicating whether the search found solutions or not
[getFeasibility(a:GlobalSearchSolver) : boolean
 -> a.problem.feasible]

// v1.325: change the interface of search limit objects
newNode :: property(range = boolean)
endNode :: property(range = void)
reset :: property(range = void)
// Basic limits implementation:
[newNode(lim:GlobalSearchLimit,a:GlobalSearchSolver) : boolean -> true] 
 // returns true if it allows to create the node  (side effect: increment an internal counter when returning true)
 // newNode is called by GlobalSearchSolver before each pushWorld, returning false cause backtrack (default: returns true)
[endNode(lim:GlobalSearchLimit,a:GlobalSearchSolver) : void -> nil]
 // increment a local counter if needed (default: does nothing)
 // endNode is called by  GlobalSearchSolver after each popWorld
[reset(lim:GlobalSearchLimit, start:boolean) : void -> nil]
 // reset the internal counter to its internal value and possibly accumulate its old value in another slot
 //  with start=true: called at the end of the tree search
(interface(newNode), interface(endNode), interface(reset))

// special case: units that are measured by accumulating a quantity
[reset(lim:CounterLimit, start:boolean) : void -> if start lim.nb := 0 else lim.totNb :+ lim.nb]
[newNode(lim:CounterLimit,a:GlobalSearchSolver) : boolean -> (lim.nb <= lim.maxNb)] 
[self_print(lim:CounterLimit) : void
-> if (lim.nb = 0) printf("tot=~S~A",lim.totNb,lim.unit) else printf("~S~A",lim.nb,lim.unit)]

// checks whether the creation of a new node is allowed. If so, register it in its own counters
[newNode(lim:NodeLimit,a:GlobalSearchSolver) : boolean 
-> lim.nb :+ 1, newNode@CounterLimit(lim,a)]
; -> if (lim.nb < lim.maxNb) (lim.nb :+ 1, true)  else false]
[endNode(lim:BacktrackLimit,a:GlobalSearchSolver) : void -> lim.nb :+ 1]

[newNode(lim:TimeLimit, algo:GlobalSearchSolver) : boolean -> (time_read() <= lim.maxNb)] 
[reset(lim:TimeLimit, start:boolean) : void
-> if start time_set() else lim.totNb :+ time_get()]
[self_print(lim:TimeLimit) : void 
-> let t := time_read() in 
    (if (t = 0) printf("tot=~S~A",lim.totNb,lim.unit) else printf("~S~A",t,lim.unit))]

// LDS limit 
// branchCounter accumulates accounts of 1 per left branch + 2 per second branch, ...
;DiscLimit <: GlobalSearchLimit(branchCounter:integer = 0,
;                               maxNbDisc:integer)
;(store(branchCounter))
[newNode(lim:DiscLimit, algo:GlobalSearchSolver) : boolean 
-> lim.branchCounter :+ 1,
   (lim.branchCounter - (world?() - algo.baseWorld + 1) <= lim.maxNbDisc)]
[reset(lim:DiscLimit, start:boolean) : void 
-> if start lim.branchCounter := 0]
[self_print(lim:DiscLimit) : void -> printf("~S~A",lim.branchCounter - (world?() - lim.searchSolver.baseWorld + 1),lim.unit)]

// v1.08 new name attachGlobalSearchSolver -> attach
[attach(newSolver:GlobalSearchSolver, pb:Problem) : void
 -> pb.globalSearchSolver := newSolver,
    newSolver.problem := pb]

// v1.0, v1.013: more precise return type
[composeGlobalSearch(algo:GlobalSearchSolver, l:list[AbstractBranching]) : type[algo] // GlobalSearchSolver
 -> let n := length(l), l2 := l in // v1.018: no longer a copy // list{copy(t) | t in l} in
     (algo.branchingComponents := l2,
      for b in l2 (b.rootBranching := b, case b (CompositeBranching (for b2 in b.alternatives (b2.manager := algo, b2.rootBranching := b)))), // VV
      for i in (1 .. n - 1) 
         (l2[i] as AbstractBranching).nextBranching := (l2[i + 1] as AbstractBranching),
      for comp in l2 comp.manager := algo,
      algo)]

// ********************************************************************
// *   Part 1: utils and solution management                          *
// ********************************************************************

[getSmallestDomainUnassignedVar(pb:Problem) : (IntVar U {unknown})
 -> getSmallestDomainUnassignedVar(pb.vars)]

// v1.02 inlined <thb>
[getSmallestDomainUnassignedVar(l:list[IntVar]) : (IntVar U {unknown})
 => let minsize := MAXINT, v0:(IntVar U {unknown}) := unknown in
        (for v in list{v in l | not(isInstantiated(v))}
           let dsize := getDomainSize(v) in
              (if (dsize < minsize)
                  (minsize := dsize, v0 := v)),
         v0)]

// v1.010: a new variable selection heuristic
[getDomOverDegBestUnassignedVar(pb:Problem) : (IntVar U {unknown})
 -> getDomOverDegBestUnassignedVar(pb.vars)]
[getDomOverDegBestUnassignedVar(l:list[IntVar]) : (IntVar U {unknown})
 => let minsize := MAXINT, maxdeg := 0, v0:(IntVar U {unknown}) := unknown in
        (for v in list{v in l | not(isInstantiated(v))}
           let dsize := getDomainSize(v), deg := getDegree(v) in
              (if (dsize * maxdeg < minsize * deg)
                  (minsize := dsize, maxdeg := deg,v0 := v)),
         v0)]

// v1.010: a new variable selection heuristic
[getMostConstrainedUnassignedVar(pb:Problem) : (IntVar U {unknown})
 -> getMostConstrainedUnassignedVar(pb.vars)]
[getMostConstrainedUnassignedVar(l:list[IntVar]) : (IntVar U {unknown})
 => let maxdeg := 0, v0:(IntVar U {unknown}) := unknown in
        (for v in list{v in l | not(isInstantiated(v))}
           let deg := getDegree(v) in
              (if (maxdeg < deg)
                  (maxdeg := deg, v0 := v)),
         v0)]


// ********************************************************************
// *   Part 2: exploring  one level of branching                      *
// ********************************************************************

// explore is a general method that applies on any new descendent or AbtractBranching
explore :: property()

// The heart of the search engine:
// exploring the branches of one choice point
[explore(b:AbstractBranching,n:integer) : boolean
 -> let algo := b.manager, pb := algo.problem in
     (when v := selectBranchingObject(b) in branchOn(b,v,n)
      else (when b2 := get(nextBranching,b) in
                    explore(b2,n)
            else (algo.problem.feasible := true,
                  algo.stopAtFirstSol)))]

[explore(b:CompositeBranching,n:integer) : boolean // VV
 -> let algo := b.manager, pb := algo.problem in // VV
     (when v := selectBranchingObject(b) in branchOn(v[1],v[2],n) // VV
      else (when b2 := get(nextBranching,b) in // VV
                    explore(b2,n) // VV
            else (algo.problem.feasible := true,
                  algo.stopAtFirstSol)))] // VV

// v1.010: <thb> new method responsible for expanding a node of the search tree
// once the branching object has been generated
[branchOn(b:LargeBranching,v:any,n:integer) : boolean
 -> let algo := b.manager, pb := algo.problem,
        nodeSuccess := false, nodeFinished := false, i := getFirstBranch(b,v) in
      (newTreeNode(algo),
       try
          (until (nodeSuccess | nodeFinished)
             let branchSuccess := false in
               (try (checkCleanState(pb.propagationEngine),
                     pushWorld(pb),
                     if (n <= algo.maxPrintDepth)
                        traceDownBranch(b,v,i),
                     goDownBranch(b,v,i),
                     if explore(b.rootBranching,n + 1) branchSuccess := true) // VV
                catch contradiction nil,
                if not(branchSuccess)
                  (popWorld(pb),
                   endTreeNode(algo),
                   if (n <= algo.maxPrintDepth)
                      traceUpBranch(b,v,i),
                   goUpBranch(b,v,i)),
                if branchSuccess nodeSuccess := true,
                if not(finishedBranching(b,v,i)) i := getNextBranch(b,v,i)
                else nodeFinished := true
                ))
       catch contradiction nodeSuccess := false,
       nodeSuccess)]

// v1.103: the branches of a BinBranching are no longer labelled with a boolean
// but with an integer (1/2) like LargeBranching
[branchOn(b:BinBranching,v:any,n:integer) : boolean
 -> let algo := b.manager, pb := algo.problem, success := false in
      (newTreeNode(algo),
       try (checkCleanState(pb.propagationEngine),
            pushWorld(pb),
            if (n <= algo.maxPrintDepth)
               traceDownBranch(b,v,1),
            goDownBranch(b,v,1),
            if explore(b.rootBranching,n + 1) success := true) // VV
       catch contradiction nil,
       if not(success)
         (popWorld(pb),
          endTreeNode(algo),
          try (
               if (n <= algo.maxPrintDepth)
                  traceDownBranch(b,v,2),
               goDownBranch(b,v,2),
               if explore(b.rootBranching,n + 1) success := true) // VV
          catch contradiction success := false),
       success)]

// ********************************************************************
// *   Part 3: Feeding the engine with a library of branching objects *
// ********************************************************************

// library of choice points (Branching classes)
// v1.010: now caching the middle of the domain (and not recomputing it)
;SplitDomain <: BinBranching(dichotomyThreshold:integer = 5)
[selectBranchingObject(b:SplitDomain) : any
 -> when x := selectVar(b.varHeuristic) in
       (if (getDomainSize(x) >= b.dichotomyThreshold)  // <ega> v0.9906: typo
           tuple(x, getBestVal(b.valHeuristic, x))
        else unknown)
    else unknown]

[goDownBranch(b:SplitDomain,x:any,first:integer) : void
 -> let xp := (x as tuple(IntVar, integer)),
        v := (xp[1] as IntVar), mid := (xp[2] as integer) in
     (if (first = 1) setMax(v,mid)
      else setMin(v,mid + 1),
      propagate(b.manager.problem) )]
[traceDownBranch(b:SplitDomain,x:any,first:integer) : void
 -> let xp := (x as tuple(IntVar, integer)),
        v := (xp[1] as IntVar), mid := (xp[2] as integer) in
     (if (first = 1) //[STALK] !!! branch ~S: ~S <= ~S // world?(),v,mid
      else //[STALK] !!! branch ~S: ~S > ~S // world?(),v,mid
     )]  // <ega> v0.9907 removed ugly call to propagate !!

;AssignOrForbid <: VarBinBranching()
// v1.010: now caching the best value in the domain (and not recomputing it)
[selectBranchingObject(b:AssignOrForbid) : any
 -> when x := selectVar(b.varHeuristic) in
         tuple(x, getBestVal(b.valHeuristic, x))
    else unknown]

[goDownBranch(b:AssignOrForbid,x:any,first:integer) : void
 -> let xp := (x as tuple(IntVar, integer)),
        v := (xp[1] as IntVar), best := (xp[2] as integer) in
     (if (first = 1) setVal(v,best)
      else remVal(v,best),
      propagate(b.manager.problem)) ]
[traceDownBranch(b:AssignOrForbid,x:any,first:integer) : void
 -> let xp := (x as tuple(IntVar, integer)),
        v := (xp[1] as IntVar), best := (xp[2] as integer) in
     (if (first = 1) //[STALK]  !! branch ~S: ~S == ~S // world?(),v,best
      else //[STALK]  !! branch ~S: ~S !== ~S // world?(),v,best
     )]

;AssignVar <: LargeBranching()
[selectBranchingObject(b:AssignVar) : any
 -> selectVar(b.varHeuristic)]

// v1.013
[finishedBranching(b:AssignVar,x:any,i:integer) : boolean
 -> let y := (x as IntVar) in 
      isOver(b.valHeuristic, y, i)]
[getFirstBranch(b:AssignVar,x:any) : integer
 -> let y := (x as IntVar) in 
      getFirstVal(b.valHeuristic, y)]
[getNextBranch(b:AssignVar,x:any,i:integer) : integer
 -> let y := (x as IntVar) in 
      getNextVal(b.valHeuristic, y, i)]
[goDownBranch(b:AssignVar,x:any,i:integer) : void
 -> let y := (x as IntVar) in 
      (setVal(y,i), propagate(b.manager.problem))]
[goUpBranch(b:AssignVar,x:any,i:integer) : void
 -> let y := (x as IntVar) in 
      (remVal(y,i), propagate(b.manager.problem))]
[traceDownBranch(b:AssignVar,x:any,i:integer) : void
 -> let y := (x as IntVar) in 
      //[STALK]  !! branch ~S: ~S == ~S // world?(),y,i
]      
[traceUpBranch(b:AssignVar,x:any,i:integer) : void
 -> let y := (x as IntVar) in 
      //[STALK]  !! branch ~S: ~S !== ~S // world?(),y,i
]      

// ********************************************************************
// *   Part 4: Using branching classes within globalSearchSolver's    *
// ********************************************************************

// The main method is run
//   it calls a few general submethods

// general definitions, v1.325: new use of limits
[newTreeSearch(a:GlobalSearchSolver) : void
 -> assert(a.problem.globalSearchSolver = a), // v1.011 <thb> consistency check
    for lim in a.limits reset(lim,true), 
    a.nbSol := 0, a.baseWorld := world?()]    
[endTreeSearch(algo:GlobalSearchSolver) : void
 -> for lim in algo.limits reset(lim,false), 
    trace(SVIEW,"solve => ~A solution ~S\n",(if not(algo.problem.feasible) "no" else string!(algo.nbSol)),algo.limits)]
                                
[newTreeNode(a:GlobalSearchSolver) : void
 -> for lim in a.limits 
       (if not(newNode(lim,a)) 
           raiseContradiction(a.problem.propagationEngine,lim))]
[endTreeNode(a:GlobalSearchSolver) : void 
-> for lim in a.limits endNode(lim,a)]

// generic method
[run(algo:GlobalSearchSolver) : void
 -> error("run is not implemented on ~S",algo)]

// searching for one solution
// v1.02: note: the initial propagation must be done before pushing any world level. It is therefore kept before restoring a solution
[run(algo:Solve) : void
 -> newTreeSearch(algo),
    let pb := algo.problem, feasibleRootState := true in
       (try propagate(pb)
        catch contradiction feasibleRootState := false,
        if feasibleRootState
          (try (pushWorld(pb),
                explore(algo.branchingComponents[1],1))
           catch contradiction popWorld(pb))),
    endTreeSearch(algo) ]  // v1.02: returns void

