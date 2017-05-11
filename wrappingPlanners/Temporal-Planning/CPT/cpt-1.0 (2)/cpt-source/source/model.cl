// ********************************************************************
// * CHOCO, version 0.99 feb. 25th 2001                               *
// * file: model.cl                                                   *
// *    object model & basic architecture                             *
// * Copyright (©) F. Laburthe, 1999-2000, see readme.txt             *
// ********************************************************************

// ------------------  File Overview  ---------------------------------
// *   Part 1: Global parameters                                      *
// *   Part 2: simple utilities (min,max,etc.)                        *
// *   Part 3: data structure utilities                               *
// *   Part 3: The generic object hierarchy                           *
// *   Part 4: Variables                                              *
// *   Part 5: Propagation events                                     *
// *   Part 6: Constraints (objects, creation, generic methods)       *
// *   Part 7: compiler optimization                                  *
// *   Part 8: Problems & constraint networks                         *
// --------------------------------------------------------------------

claire/CHOCO_RELEASE :: 13.026
[showChocoLicense() : void
 -> printf("Choco version ~A, Copyright (©) 1999-2002 F. Laburthe\n",CHOCO_RELEASE),
    printf("Choco comes with ABSOLUTELY NO WARRANTY; "),
    printf("for details read licence.txt\n"),
    printf("This is free software, and you are welcome to redistribute it\n"),
    printf("under certain conditions; read licence.txt for details.\n") ]
;(showChocoLicense())

// ********************************************************************
// *   Part 1: Global parameters                                      *
// ********************************************************************

claire/EXTENDABLE_CHOCO:boolean := false

// VERBOSITY PARAMETERS: we use the ADHOC methodology
//
//    (a) The integer value of verbose() may be
//          0: application mode  = silent  (trace <=> printf)
//          1: execution mode    = silent but for structure comments
//          2: trace mode        = report the execution flow step by step
//          3: debug mode        = report everything !

//    (b) each fragment X of the program may
//          - either use the standard integer values (0,1,2)
//          - introduce the flags: XTALK, XSHOW, XDEBUG
//            The flags support a flexible control of the code fragments,
//            for which tracing can be independently turned on and off
//               XVIEW: (default 1)   execution
//               XTALK: (defalt 2)    trace
//               XDEBUG: (default 3)  debug

// fragment D: coding domains
claire/DDEBUG:integer := 6     // debugging buckets (implementing domains)

// fragment P: propagation
claire/PVIEW:integer := 4      // general propagation info (layered fix-points)
claire/PTALK:integer := 5      // tracing propagation (event queues)
claire/PDEBUG:integer := 6     // debugging propagation (domain updates)

// fragment GP: propagation of global constraints
claire/GPVIEW:integer := 3      // general propagation info (layered fix-points)
claire/GPTALK:integer := 4      // tracing propagation (event queues)
claire/GPDEBUG:integer := 5     // debugging propagation (domain updates)

// fragment S: global search
claire/SVIEW:integer := 1      // general search info (solutions found)
claire/STALK:integer := 2      // tracing decisions in the search tree
claire/SDEBUG:integer := 3     // debugging search tree construction (variable selection heuristics)

// fragment I: invariants (for non-monotonic moves)
claire/IVIEW:integer := 1      // general info about conflict counts
claire/ITALK:integer := 2      // tracing improvement of conflict counts
claire/IDEBUG:integer := 3     // debugging evaluations of invariant maintenance

// fragment L: moves & local search
claire/LVIEW:integer := 1      // general info about iterations (good solutions found)
claire/LTALK:integer := 5      // tracing assignments & flips
claire/LDEBUG:integer := 6     // debugging local optimization (selection heuristics)

// fragment CHOCOBENCH: benchmark suite
claire/CHOCOBENCH_VIEW:integer := -1
// fragment CHOCOTEST: regression testbed
claire/CHOCOTEST_VIEW:integer := -1

// ********************************************************************
// *   Part 3: The object hierarchy                                   *
// ********************************************************************

// root class for all Choco objects
Ephemeral <: ephemeral_object()

// below, the whole hierarchy is reproduced
// (uncommented lines are mandatory forward declarations)
Problem <: Ephemeral

Solver <: Ephemeral
  LocalSearchSolver <: Solver
  GlobalSearchSolver <: Solver

AbstractConstraint <: Ephemeral
  IntConstraint <: AbstractConstraint
;    UnIntConstraint <: IntConstraint
;    BinIntConstraint <: IntConstraint
;    TernIntConstraint <: IntConstraint
;    LargeIntConstraint <: IntConstraint
;    Delayer <: IntConstraint
;  CompositeConstraint <: AbstractConstraint
;    BinCompositeConstraint <: CompositeConstraint
;      BinBoolConstraint <: BinCompositeConstraint
;        BinBoolConstraintWCounterOpp <: BinBoolConstraint  // v1.02 
;    LargeCompositeConstraint <: CompositeConstraint
;      LargeBoolConstraint <: LargeCompositeConstraint
;        LargeBoolConstraintWCounterOpp <: LargeBoolConstraint  // v1.02
;
AbstractVar <: Ephemeral
  IntVar <: AbstractVar
;
;AbstractDomain <: collection
;  AbstractIntDomain <: AbstractDomain
;    LinkedListIntDomain <: AbstractIntDomain
;  AbstractSetDomain <: AbstractDomain
;    BitSetDomain <: AbstractSetDomain
;    BitListSetDomain <: AbstractSetDomain
;
PropagationEvent <: Ephemeral
  ConstAwakeEvent <: PropagationEvent  // v0.9906
  VarEvent <: PropagationEvent
    Instantiation <: VarEvent
      InstInt <: Instantiation
    ValueRemovals <: VarEvent
    BoundUpdate <: VarEvent
      IncInf <: BoundUpdate
      DecSup <: BoundUpdate
;
;EventCollection <: Ephemeral
;  InstantiationStack <: EventCollection
;  EventQueue <: EventCollection
;    BoundEventQueue <: EventQueue
;    RemovalEventQueue <: EventQueue
;    ConstAwakeEventQueue <: EventQueue  // v0.9906

LogicEngine <: Ephemeral
  PropagationEngine <: LogicEngine
;    ChocEngine <: PropagationEngine
  InvariantEngine <: LogicEngine
;    ConflictCount <: InvariantEngine
;
;Solver <: Ephemeral
;  GlobalSearchSolver <: Solver
;    Solve <: GlobalSearchSolver
;    AbstractOptimize <: GlobalSearchSolver
;      BranchAndBound <: AbstractOptimize
;      OptimizeWithRestarts <: AbstractOptimize
;  LocalSearchSolver <: Solver
;    MultipleDescent <: LocalSearchSolver
;
AbstractBranching <: Ephemeral
;  BinBranching <: AbstractBranching
;    SplitDomain <: BinBranching
;    AssignOrForbid <: BinBranching
;    SettleBinDisjunction <: BinBranching
;  LargeBranching <: AbstractBranching
;    CompositeBranching <: LargeBranching
;    AssignVar <: LargeBranching
GlobalSearchLimit <: Ephemeral
;
ConstructiveHeuristic <: Ephemeral
;  AssignmentHeuristic <: ConstructiveHeuristic
MoveNeighborhood <: Ephemeral
;  FlipNeighborhood <: MoveNeighborhood
;
;Solution <: Ephemeral

// ********************************************************************
// *   Part 8: Problems                                               *
// ********************************************************************

// v0.25: a solution contains the list of values for a reference list of variables
// claire3 port: strongly typed lists
Solution <: Ephemeral(
   algo:Solver,
   time:integer = 0,
   nbBk:integer = 0,
   nbNd:integer = 0,
   objectiveValue:integer = MAXINT,
   lval:list<(integer U {unknown})>)

[makeSolution(a:Solver,nbVars:integer) : Solution
 -> Solution(algo = a, 
             lval = make_list(nbVars,(integer U {unknown}),unknown))]

claire/DummyConstraint <: AbstractConstraint() // VV
claire/dummy_constraint :: DummyConstraint() // VV

// A problem is a global structure containing variables bound by constraints
// as well as solutions or solver parameters
Problem <: Ephemeral(
     // public slots
   constraints:list[AbstractConstraint], // VV
   vars:list<IntVar>,
   name:string = "",
   feasible:boolean = false,
   solved:boolean = false,
   // there are two reasoning modes: feasible/infeasible (depends whether constraints may be violated or not)
   feasibleMode:boolean = true,
   // tools for feasible mode: a propagation engine and a solver (controlling global search)
   propagationEngine:PropagationEngine,
   globalSearchSolver:GlobalSearchSolver,
   // tools for infeasible mode: an invariant engine (counting inconsistencies) and a solver (controlling local search)
   invariantEngine:InvariantEngine,
   localSearchSolver:LocalSearchSolver,
   nbConstProblem:integer = 255, // VV
   nbConstVar:integer = 31 // VV
   )
(store(feasibleMode))
;DUMMY_PROBLEM :: Problem(name = "dummy empty problem")
;CURRENT_PB:Problem := DUMMY_PROBLEM
CURRENT_PB:any := unknown

[getIntVar(p:Problem, i:integer) : IntVar
 -> assert(0 < i & i <= length(p.vars)),
    p.vars[i] ]

    
// forward declarations // v1.010
setActiveProblem :: property()
getActiveProblem :: property()

// ********************************************************************
// *   Part 9: local and global solvers                               *
// ********************************************************************

// -----------Logic-------------------------------------
LogicEngine <: Ephemeral(
   problem:Problem)

PropagationEngine <: LogicEngine(
   maxSize:integer = 100,
   propagationOverflow:boolean = false,
   contradictionCause:(Ephemeral U {unknown}))  // v1.0 <fl> // VV
    
InvariantEngine <: LogicEngine()

ConflictCount <: InvariantEngine(
    // slots for handling infeasible assignments (no propagation)
   nbViolatedConstraints:integer = 0,
   minNbViolatedConstraints:integer = MAXINT,
   assignedThenUnassignedVars:list<IntVar>,     // v0.34 contains assigned variables up to index
   lastAssignedVar:integer = 0,                 //       lastAssignedVar, unassigned afterwards.
   conflictingVars:list<IntVar>)

// -----------Control-------------------------------------
// claire3 port: strongly typed lists
Solver <: Ephemeral(
   problem:Problem,
   solutions:list<Solution>,     // v1.013 no more unknowns
   varsToStore:list<IntVar>,     // those variables that need be stored when finding a solution
   varsToShow:set<IntVar>
   )

// v1.020: new objects for search limits
GlobalSearchLimit  <: Ephemeral(
   searchSolver:GlobalSearchSolver,  // v1.325
   unit:string = "")

// limits that are based on counter (shtg that accumulates)
CounterLimit <: GlobalSearchLimit(
   maxNb:integer = 100000,
   totNb:integer = 0,
   nb:integer = 0)

NodeLimit <: CounterLimit()
BacktrackLimit <: CounterLimit()
TimeLimit <: CounterLimit()

// limits on the shape of the search tree
TopologyLimit <: GlobalSearchLimit()
DiscLimit <: TopologyLimit(branchCounter:integer = 0,
                           maxNbDisc:integer)
(store(branchCounter))   

GlobalSearchSolver <: Solver(
    // slots for global search
   private/stopAtFirstSol:boolean = true,
   nbSol:integer = 0,        // nb of solutions found
   nbBk:integer = 0,         // nb of backtracks in one tree
   nbNd:integer = 0,         // nb of nodes (== nb calls to assign) expanded in one tree
   maxNbBk:integer = 100000,   // limit on the total number of backtracks
   maxPrintDepth:integer = 5,  // maximal depth of printing
    // slots for managing the solutions generated during the search
   maxSolutionStorage:integer = 0, // v1.013 maximal number of solutions that are stored
   branchingComponents:list[AbstractBranching],
   baseWorld:integer = 0,
   limits:list<GlobalSearchLimit>  // 1.320: new mechanism: replaces maxNbBk
   )

AbstractBranching <: Ephemeral(
   manager:GlobalSearchSolver,
   nextBranching:AbstractBranching,
   rootBranching:AbstractBranching // VV
)
