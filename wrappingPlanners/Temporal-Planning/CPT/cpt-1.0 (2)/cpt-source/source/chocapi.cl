// ********************************************************************
// * CHOCO, version 0.99 feb. 25th 2001                               *
// * file: chocapi.cl                                                 *
// *    API: application programmer interface (public methods)        *
// * Copyright (©) F. Laburthe, 1999-2000, see readme.txt             *
// ********************************************************************

// ------------------  File Overview  ---------------------------------
// *   Part 1: problems                                               *
// *   Part 2: variables                                              *
// *   Part 3: Constraints as tuples of values                        *
// *   Part 4: Arithmetic constraints                                 *
// *   Part 5: Boolean connectors                                     *
// *   Part 6: global constraints                                     *
// *   Part 7: adding information                                     *
// *   Part 8: searching for solutions / (tree search) optimization   *
// *   Part 9: searching for solutions / (assign & repair) local opt. *
// --------------------------------------------------------------------

// ********************************************************************
// *   Part 1: problems                                               *
// ********************************************************************

[getProblem(v:IntVar) : Problem -> v.problem]

// v1.02 <thb> add the case when the constraint c involves only constant variables, considered as belonging to CURRENT_PB
[getProblem(c:AbstractConstraint) : Problem
-> let p:Problem := (CURRENT_PB as Problem), n := getNbVars(c) in
      (when i0 := some(i in (1 .. n) | known?(problem,getVar(c,i) as IntVar)) in
            p := (getVar(c,i0) as IntVar).problem,
       p)]

// v1.010 new API methods
[getProblem() : Problem => CURRENT_PB as Problem]
[getActiveProblem() : Problem => CURRENT_PB as Problem]
[setActiveProblem(pb:Problem) : void -> CURRENT_PB := pb]
[discardProblem(pb:Problem) : void
 -> when gs := get(globalSearchSolver, pb)  in
       (backtrack(gs.baseWorld),
        CURRENT_PB := unknown)]
;        CURRENT_PB := DUMMY_PROBLEM)]  // v1.013

// v1.013: accessing the propagation engine
[getPropagationEngine(p:Problem) : PropagationEngine
 -> p.propagationEngine]

// v1.013: a toggle: choosing to delay or no the propagation on linear constraints
;[setDelayedLinearConstraintPropagation(pe:ChocEngine,b:boolean) : void
; -> if b pe.delayLinCombThreshold := 0
;    else pe.delayLinCombThreshold := 100000]

// ********************************************************************
// *   Part 2: variables                                              *
// ********************************************************************

// public methods for creating variables on the fly and assiging them a name
//
// v0.37 added closeIntVar because of new backtrackable domain bounds
[makeBoundIntVar(p:Problem, s:string, i:integer, j:integer) : IntVar
 -> assert(i <= j),
    let v := IntVar(name = copy(s)) in
       (addIntVar(p,v), // VV : invert add and close
        closeIntVar(v,i,j,1),  // v0.9903: react to removals in one pass
        v)]
[makeBoundIntVar(p:Problem, i:integer, j:integer) : IntVar => makeBoundIntVar(p,"",i,j)]
[makeBoundIntVar(p:Problem, s:string) : IntVar => makeBoundIntVar(p,s,-100,100)]

// v0.37
[makeConstantIntVar(x:integer) : IntVar
 -> let v := IntVar(name = "'" /+ string!(x) /+ "'") in // <thb> v0.97
      (v.problem := getActiveProblem(), // VV
       closeIntVar(v,x,x,0), v)]   // v0.9903: there will never be a removal to react to

[makeIntVar(p:Problem, s:string, i:integer, j:integer) : IntVar
 -> assert(i <= j),
;    let v := makeBoundIntVar(p,s,i,j) in
     let v := IntVar(name = copy(s)) in
       (addIntVar(p,v),  // VV : invert add and close
        closeIntVar(v,i,j,min(3,j - i - 1)),  // v0.9903: react to (up to 5) removal one by one
        v.bucket := makeLinkedListIntDomain(i,j),
        v)]
[makeIntVar(p:Problem, i:integer, j:integer) : IntVar => makeIntVar(p,"",i,j)]
[makeIntVar(p:Problem, s:string) : IntVar => makeIntVar(p,s,-100,100)]

// v0.22 <fl> documented but not implemented ...
// v0.26 stronger typing of b
[makeIntVar(p:Problem, s:string, b:list[integer]) : IntVar
 -> let minVal := min(b), // v1.02 min vs. Min
        maxVal := max(b), // v1.02 max vs. Max
        v := makeIntVar(p,s,minVal,maxVal) in
      (for val in list{val2 in (minVal .. maxVal) | not(val2 % b)}
           removeDomainVal(v.bucket, val),
       v)]
// v0.26 stronger typing of b
[makeIntVar(p:Problem, b:list[integer]) : IntVar -> makeIntVar(p,"",b)]


// ********************************************************************
// *   Part 7: adding information                                     *
// ********************************************************************

// v0.9906: rewritten
// v0.9907: merged the definitions from IntConstraint and DelayedConstraint into AbstractConstraint
// v1.010: one single restriction for post@Problem

[post(p:Problem, c:AbstractConstraint) : void //v0.30 replaced GlobalConstraint by DelayedConstraint
 ->
       store(p.constraints, c), // VV
       connect(c),
       let pe := p.propagationEngine,
           e := ConstAwakeEvent(touchedConst = c, initialized = false) in
        (c.constAwakeEvent := e,
         registerEvent(pe,e),
         postConstAwake(pe,c,true))]

// further constraining a domain: adding information to the current state
// (restricting by hand the domain of a variable)
[setMin(v:IntVar, x:integer) : void => updateInf(v,x,0)]
[setMax(v:IntVar, x:integer) : void => updateSup(v,x,0)]
[setVal(v:IntVar, x:integer) : void => instantiate(v,x,0)]
[remVal(v:IntVar, x:integer) : void => removeVal(v,x,0)]

// v0.9906
[propagate(p:Problem) : void
 -> let pe := p.propagationEngine, someEvents := true in
       (while someEvents
           (when q := getNextActiveEventQueue(pe) in 
                 popSomeEvents(q)
            else someEvents := false))]

// ********************************************************************
// *   Part 8: searching for solutions / optimization                 *
// ********************************************************************

[getProblem(algo:Solver) : Problem -> algo.problem]
[getSearchManager(b:AbstractBranching) : GlobalSearchSolver -> b.manager]

// v1.016: new branching heuristic objects
[makeMinDomVarHeuristic() : MinDomain -> MinDomain()] // VV 
[makeDomDegVarHeuristic() : DomDeg -> DomDeg()] // VV
[makeMaxDegVarHeuristic() : MaxDeg -> MaxDeg()] // VV
[makeStaticVarHeuristic(l:list[IntVar])  : StaticVarOrder // VV
 -> StaticVarOrder(vars = list<IntVar>{v | v in l})]

// new API methods
[makeMinDomVarHeuristic(l:list[IntVar]) : MinDomain
 -> MinDomain(vars = list<IntVar>{v | v in l})]
[makeDomDegVarHeuristic(l:list[IntVar]) : DomDeg
 -> DomDeg(vars = list<IntVar>{v | v in l})]
[makeMaxDegVarHeuristic(l:list[IntVar]) : MaxDeg
 -> MaxDeg(vars = list<IntVar>{v | v in l})]

[makeIncValIterator() : ValIterator -> IncreasingDomain()] // VV
[makeDecValIterator() : ValIterator -> DecreasingDomain()] // VV

[makeMinValSelector() : ValSelector -> MinValHeuristic()] // VV
[makeMaxValSelector() : ValSelector -> MaxValHeuristic()] // VV
[makeMidValSelector() : ValSelector -> MidValHeuristic()] // VV

[makeAssignVarBranching(varh:VarSelector, valh:ValIterator) : AssignVar
 -> let av := AssignVar(varHeuristic = varh, valHeuristic = valh) in
      (varh.branching := av,
       valh.branching := av,
       av)]
[makeAssignVarBranching(varh:VarSelector) : AssignVar
 -> makeAssignVarBranching(varh, makeIncValIterator())]
[makeAssignVarBranching() : AssignVar
 -> makeAssignVarBranching(makeMinDomVarHeuristic(), makeIncValIterator())]

[makeSplitDomBranching(varh:VarSelector, valh:ValSelector, threshold:integer) : SplitDomain
 -> let sd := SplitDomain(varHeuristic = varh, valHeuristic = valh, dichotomyThreshold = threshold) in
      (varh.branching := sd,
       valh.branching := sd,
       sd)]
[makeSplitDomBranching(varh:VarSelector, threshold:integer) : SplitDomain
 -> makeSplitDomBranching(varh, makeMidValSelector(), threshold)]
[makeSplitDomBranching(varh:VarSelector) : SplitDomain
 -> makeSplitDomBranching(varh, makeMidValSelector(), 5)]
[makeSplitDomBranching() : SplitDomain
 -> makeSplitDomBranching(makeMinDomVarHeuristic(), makeMidValSelector(), 5)]

[makeAssignOrForbidBranching(varh:VarSelector, valh:ValSelector) : AssignOrForbid
 -> let af := AssignOrForbid(varHeuristic = varh, valHeuristic = valh) in
      (varh.branching := af,
       valh.branching := af,
       af)]

//  the default standard procedure in Choco:
//  first settle suspended disjunctions, then split domains, then instantiate
[makeDefaultBranchingList(pb:Problem) : list[AbstractBranching]
 => list<AbstractBranching>(
             makeSplitDomBranching(makeMinDomVarHeuristic(), makeMidValSelector(), 5),
             makeAssignVarBranching(makeMinDomVarHeuristic(), makeIncValIterator())
             )]

// v1.0 new API methods
[makeGlobalSearchSolver(allSolutions:boolean, l:list[AbstractBranching]) : Solve
 -> composeGlobalSearch(Solve(stopAtFirstSol = not(allSolutions)),l)]

// default methods
// v1.010: add the Problem argument
[makeGlobalSearchSolver(allSolutions:boolean) : Solve
 -> makeGlobalSearchSolver(allSolutions,makeDefaultBranchingList(getActiveProblem()))]

[setSearchLimit(algo:GlobalSearchSolver, lim:GlobalSearchLimit) : void 
 -> lim.searchSolver := algo, algo.limits :add lim]

[getNb(lim:CounterLimit) : integer -> lim.nb]
[getMaxNb(lim:CounterLimit) : integer -> lim.maxNb]

// New API functions
// v1.08 attachGlobalSearchSolver -> attach
[solve(pb:Problem,l:list[AbstractBranching],all:boolean) : boolean
 -> let algo := makeGlobalSearchSolver(all, l), 
        lims := makeDefaultLimits() in
       (attach(algo,pb), 
        for lim in lims setSearchLimit(algo,lim),
        run(algo), getFeasibility(algo))]  // v1.02: explicit access to feasibility

// controlling the search
// v1.08 these functions no longer apply to Problem, but to GlobalSearchSolver
[setMaxPrintDepth(algo:GlobalSearchSolver, n:integer) : void
 -> algo.maxPrintDepth := n]

// v1.013 new API function
[setMaxSolutionStorage(algo:GlobalSearchSolver, nb:integer) : void
 -> algo.maxSolutionStorage := nb]

[setVarsToShow(algo:GlobalSearchSolver, l:list[IntVar]) : void
 -> algo.varsToShow := copy(l)]

[getNbSol(algo:GlobalSearchSolver) : integer
 -> algo.nbSol]

[getProblem(b:AbstractBranching) : Problem
 -> b.manager.problem]

// v1.322 new API method
[getGlobalSearchSolver(p:Problem) : GlobalSearchSolver
 -> p.globalSearchSolver]
  
// -------- new from v1.020: introducing limit objects

[makeNodeLimit(n:integer) : NodeLimit -> NodeLimit(maxNb = n, unit = "nd")]
[makeBacktrackLimit(n:integer) : BacktrackLimit -> BacktrackLimit(maxNb = n, unit = "bk")]
[makeTimeLimit(n:integer) : TimeLimit -> TimeLimit(maxNb = n, unit = "ms")]
[makeDiscLimit(n:integer) : DiscLimit -> DiscLimit(maxNb = n, unit = "disc")]

[makeDefaultLimits() : list<GlobalSearchLimit>
 -> list<GlobalSearchLimit>(makeNodeLimit(1000000), makeBacktrackLimit(1000000), makeTimeLimit(3600000))]

[setNodeLimit(algo:GlobalSearchSolver, n:integer) : void
 -> setSearchLimit(algo, makeNodeLimit(n))]
[setTimeLimit(algo:GlobalSearchSolver, n:integer) : void
 -> setSearchLimit(algo, makeTimeLimit(n))]
[setBacktrackLimit(algo:GlobalSearchSolver, n:integer) : void
 -> setSearchLimit(algo, makeBacktrackLimit(n))]
[setDiscLimit(algo:GlobalSearchSolver,maxNbDiscepancies:integer) : void 
 -> setSearchLimit(algo, makeDiscLimit(maxNbDiscepancies))]
 
