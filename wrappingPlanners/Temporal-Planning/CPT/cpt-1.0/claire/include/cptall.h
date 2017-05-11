// interface defination for module cptall, Mon Nov  8 16:37:06 2004
#ifndef CLAIREH_cptall
#define CLAIREH_cptall


class cptall_Util;
class StoredInt;
class cptall_Ephemeral;
class cptall_Problem;
class cptall_Solver;
class cptall_LocalSearchSolver;
class cptall_GlobalSearchSolver;
class cptall_AbstractConstraint;
class cptall_IntConstraint;
class cptall_AbstractVar;
class cptall_IntVar;
class cptall_PropagationEvent;
class cptall_ConstAwakeEvent;
class cptall_VarEvent;
class cptall_Instantiation;
class cptall_InstInt;
class cptall_ValueRemovals;
class cptall_BoundUpdate;
class cptall_IncInf;
class cptall_DecSup;
class cptall_LogicEngine;
class cptall_PropagationEngine;
class cptall_InvariantEngine;
class cptall_AbstractBranching;
class cptall_GlobalSearchLimit;
class cptall_ConstructiveHeuristic;
class cptall_MoveNeighborhood;
class cptall_Solution;
class DummyConstraint;
class cptall_ConflictCount;
class cptall_CounterLimit;
class cptall_NodeLimit;
class cptall_BacktrackLimit;
class cptall_TimeLimit;
class cptall_TopologyLimit;
class cptall_DiscLimit;
class cptall_AbstractDomain;
class cptall_AbstractIntDomain;
class cptall_AbstractSetDomain;
class cptall_LinkedListIntDomain;
class cptall_BipartiteSet;
class cptall_EventCollection;
class cptall_EventQueue;
class cptall_BoundEventQueue;
class cptall_RemovalEventQueue;
class cptall_InstantiationStack;
class cptall_ConstAwakeEventQueue;
class cptall_ChocEngine;
class cptall_UnIntConstraint;
class cptall_BinIntConstraint;
class cptall_TernIntConstraint;
class cptall_LargeIntConstraint;
class cptall_CompositeConstraint;
class cptall_BinCompositeConstraint;
class cptall_LargeCompositeConstraint;
class cptall_LargeBranching;
class cptall_BinBranching;
class cptall_CompositeBranching;
class cptall_VarSelector;
class cptall_MinDomain;
class cptall_MaxDeg;
class cptall_DomDeg;
class cptall_StaticVarOrder;
class cptall_ValIterator;
class cptall_IncreasingDomain;
class cptall_DecreasingDomain;
class cptall_ValSelector;
class cptall_MidValHeuristic;
class cptall_MinValHeuristic;
class cptall_MaxValHeuristic;
class cptall_AssignVar;
class cptall_SplitDomain;
class cptall_AssignOrForbid;
class cptall_Solve;
class cptall_Atom;
class cptall_Symbol;
class cptall_Term;
class cptall_Rational;
class cptall_OperationTerms;
class cptall_Addition;
class cptall_Substraction;
class cptall_Multiplication;
class cptall_Division;
class cptall_Counter;
class cptall_BoolArray;
class cptall_Session;
class cptall_Predicate;
class cptall_Variabl;
class cptall_Constant;
class cptall_Fluent;
class cptall_Action;
class cptall_CausalLink;
class cptall_PlanningProblem;
class cptall_Instance;
class cptall_TransitionRule;
class cptall_PositiveAtom;
class cptall_NegativeAtom;
class cptall_Operator;
class cptall_Domain;
class cptall_SupportConstraint;
class cptall_ActionConstraint;
class cptall_PrecedenceConstraint;
class cptall_CausalPrecedenceConstraint;
class cptall_InitTimeDomain;
class cptall_ProducerDomain;
class cptall_BestProducerHeuristic;
class cptall_SolveConflict;
class cptall_ConflictBranching;
class cptall_Conflict;
class cptall_Mutex;
class cptall_SolveMutex;

class cptall_Util: public ephemeral_object{ 
  public:} 
;

class StoredInt: public cptall_Util{ 
  public:
     int latestValue;
     int latestUpdate;} 
;

class cptall_Ephemeral: public ephemeral_object{ 
  public:} 
;

class cptall_Problem: public cptall_Ephemeral{ 
  public:
     list *constraints;
     list *vars;
     char *name;
     ClaireBoolean *feasible;
     ClaireBoolean *solved;
     ClaireBoolean *feasibleMode;
     cptall_PropagationEngine *propagationEngine;
     cptall_GlobalSearchSolver *globalSearchSolver;
     cptall_InvariantEngine *invariantEngine;
     cptall_LocalSearchSolver *localSearchSolver;
     int nbConstProblem;
     int nbConstVar;} 
;

class cptall_Solver: public cptall_Ephemeral{ 
  public:
     cptall_Problem *problem;
     list *solutions;
     list *varsToStore;
     set *varsToShow;} 
;

class cptall_LocalSearchSolver: public cptall_Solver{ 
  public:} 
;

class cptall_GlobalSearchSolver: public cptall_Solver{ 
  public:
     ClaireBoolean *stopAtFirstSol;
     int nbSol;
     int nbBk;
     int nbNd;
     int maxNbBk;
     int maxPrintDepth;
     int maxSolutionStorage;
     list *branchingComponents;
     int baseWorld;
     list *limits;} 
;

class cptall_AbstractConstraint: public cptall_Ephemeral{ 
  public:
     cptall_ConstAwakeEvent *constAwakeEvent;} 
;

class cptall_IntConstraint: public cptall_AbstractConstraint{ 
  public:
     int cste;} 
;

class cptall_AbstractVar: public cptall_Ephemeral{ 
  public:
     char *name;
     cptall_Problem *problem;
     int nbViolatedConstraints;
     int nbConstraints;
     list *constraints;
     list *indices;} 
;

class cptall_IntVar: public cptall_AbstractVar{ 
  public:
     StoredInt *inf;
     StoredInt *sup;
     int value;
     int savedAssignment;
     cptall_AbstractIntDomain *bucket;
     cptall_IncInf *updtInfEvt;
     cptall_DecSup *updtSupEvt;
     cptall_InstInt *instantiateEvt;
     cptall_ValueRemovals *remValEvt;
  ClaireBoolean * cptall_domainIncludedIn(list *sortedList);  
  ClaireBoolean * cptall_hasExactDomain();  
  int  cptall_getInf();  
  int  cptall_getSup();  } 
;

class cptall_PropagationEvent: public cptall_Ephemeral{ 
  public:} 
;

class cptall_ConstAwakeEvent: public cptall_PropagationEvent{ 
  public:
     cptall_AbstractConstraint *touchedConst;
     ClaireBoolean *initialized;
     int event_idx;} 
;

class cptall_VarEvent: public cptall_PropagationEvent{ 
  public:
     cptall_AbstractVar *modifiedVar;
     list *nextConst;} 
;

class cptall_Instantiation: public cptall_VarEvent{ 
  public:
     int cause;} 
;

class cptall_InstInt: public cptall_Instantiation{ 
  public:} 
;

class cptall_ValueRemovals: public cptall_VarEvent{ 
  public:
     int maxVals;
     int nbVals;
     ClaireBoolean *many;
     list *valueStack;
     list *causeStack;
     int idxInQueue;} 
;

class cptall_BoundUpdate: public cptall_VarEvent{ 
  public:
     int cause;
     int idxInQueue;} 
;

class cptall_IncInf: public cptall_BoundUpdate{ 
  public:} 
;

class cptall_DecSup: public cptall_BoundUpdate{ 
  public:} 
;

class cptall_LogicEngine: public cptall_Ephemeral{ 
  public:
     cptall_Problem *problem;} 
;

class cptall_PropagationEngine: public cptall_LogicEngine{ 
  public:
     int maxSize;
     ClaireBoolean *propagationOverflow;
     OID contradictionCause;
  OID  cptall_getNextActiveEventQueue();  } 
;

class cptall_InvariantEngine: public cptall_LogicEngine{ 
  public:} 
;

class cptall_AbstractBranching: public cptall_Ephemeral{ 
  public:
     cptall_GlobalSearchSolver *manager;
     cptall_AbstractBranching *nextBranching;
     cptall_AbstractBranching *rootBranching;} 
;

class cptall_GlobalSearchLimit: public cptall_Ephemeral{ 
  public:
     cptall_GlobalSearchSolver *searchSolver;
     char *unit;} 
;

class cptall_ConstructiveHeuristic: public cptall_Ephemeral{ 
  public:} 
;

class cptall_MoveNeighborhood: public cptall_Ephemeral{ 
  public:} 
;

class cptall_Solution: public cptall_Ephemeral{ 
  public:
     cptall_Solver *algo;
     int time;
     int nbBk;
     int nbNd;
     int objectiveValue;
     list *lval;} 
;

class DummyConstraint: public cptall_AbstractConstraint{ 
  public:} 
;

class cptall_ConflictCount: public cptall_InvariantEngine{ 
  public:
     int nbViolatedConstraints;
     int minNbViolatedConstraints;
     list *assignedThenUnassignedVars;
     int lastAssignedVar;
     list *conflictingVars;} 
;

class cptall_CounterLimit: public cptall_GlobalSearchLimit{ 
  public:
     int maxNb;
     int totNb;
     int nb;} 
;

class cptall_NodeLimit: public cptall_CounterLimit{ 
  public:} 
;

class cptall_BacktrackLimit: public cptall_CounterLimit{ 
  public:} 
;

class cptall_TimeLimit: public cptall_CounterLimit{ 
  public:} 
;

class cptall_TopologyLimit: public cptall_GlobalSearchLimit{ 
  public:} 
;

class cptall_DiscLimit: public cptall_TopologyLimit{ 
  public:
     int branchCounter;
     int maxNbDisc;} 
;

class cptall_AbstractDomain: public ClaireCollection{ 
  public:} 
;

class cptall_AbstractIntDomain: public cptall_AbstractDomain{ 
  public:} 
;

class cptall_AbstractSetDomain: public cptall_AbstractDomain{ 
  public:} 
;

class cptall_LinkedListIntDomain: public cptall_AbstractIntDomain{ 
  public:
     int offset;
     int bucketSize;
     list *contents;} 
;

class cptall_BipartiteSet: public cptall_Util{ 
  public:
     list *objs;
     int nbLeft;} 
;

class cptall_EventCollection: public cptall_Ephemeral{ 
  public:
     cptall_PropagationEngine *engine;
     int qsize;
  void  cptall_popSomeEvents();  } 
;

class cptall_EventQueue: public cptall_EventCollection{ 
  public:
     int qLastRead;
     int qLastEnqueued;
     ClaireBoolean *isPopping;} 
;

class cptall_BoundEventQueue: public cptall_EventQueue{ 
  public:
     list *eventQueue;
     ClaireBoolean *redundantEvent;} 
;

class cptall_RemovalEventQueue: public cptall_EventQueue{ 
  public:
     list *eventQueue;
     ClaireBoolean *redundantEvent;} 
;

class cptall_InstantiationStack: public cptall_EventCollection{ 
  public:
     list *eventQueue;
     int sLastRead;
     int sLastPushed;} 
;

class cptall_ConstAwakeEventQueue: public cptall_EventCollection{ 
  public:
     cptall_BipartiteSet *partition;} 
;

class cptall_ChocEngine: public cptall_PropagationEngine{ 
  public:
     cptall_RemovalEventQueue *removalEvtQueue;
     cptall_BoundEventQueue *boundEvtQueue;
     cptall_InstantiationStack *instEvtStack;
     cptall_ConstAwakeEventQueue *delayedConst;
     int nbPendingInitConstAwakeEvent;
     int nbPendingVarEvent;} 
;

class cptall_UnIntConstraint: public cptall_IntConstraint{ 
  public:
     cptall_IntVar *v1;
     int idx1;} 
;

class cptall_BinIntConstraint: public cptall_IntConstraint{ 
  public:
     cptall_IntVar *v1;
     int idx1;
     cptall_IntVar *v2;
     int idx2;} 
;

class cptall_TernIntConstraint: public cptall_IntConstraint{ 
  public:
     cptall_IntVar *v1;
     int idx1;
     cptall_IntVar *v2;
     int idx2;
     cptall_IntVar *v3;
     int idx3;} 
;

class cptall_LargeIntConstraint: public cptall_IntConstraint{ 
  public:
     list *vars;
     list *indices;
     int nbVars;} 
;

class cptall_CompositeConstraint: public cptall_AbstractConstraint{ 
  public:} 
;

class cptall_BinCompositeConstraint: public cptall_CompositeConstraint{ 
  public:
     cptall_AbstractConstraint *const1;
     cptall_AbstractConstraint *const2;
     int offset;} 
;

class cptall_LargeCompositeConstraint: public cptall_CompositeConstraint{ 
  public:
     list *lconst;
     list *loffset;
     int nbConst;
     list *additionalVars;
     list *additionalIndices;} 
;

class cptall_LargeBranching: public cptall_AbstractBranching{ 
  public:} 
;

class cptall_BinBranching: public cptall_AbstractBranching{ 
  public:} 
;

class cptall_CompositeBranching: public cptall_LargeBranching{ 
  public:
     list *alternatives;} 
;

class cptall_VarSelector: public cptall_Ephemeral{ 
  public:
     cptall_AbstractBranching *branching;} 
;

class cptall_MinDomain: public cptall_VarSelector{ 
  public:
     cptall_Problem *problem;
     list *vars;} 
;

class cptall_MaxDeg: public cptall_VarSelector{ 
  public:
     cptall_Problem *problem;
     list *vars;} 
;

class cptall_DomDeg: public cptall_VarSelector{ 
  public:
     cptall_Problem *problem;
     list *vars;} 
;

class cptall_StaticVarOrder: public cptall_VarSelector{ 
  public:
     list *vars;} 
;

class cptall_ValIterator: public cptall_Ephemeral{ 
  public:
     cptall_AbstractBranching *branching;} 
;

class cptall_IncreasingDomain: public cptall_ValIterator{ 
  public:} 
;

class cptall_DecreasingDomain: public cptall_ValIterator{ 
  public:} 
;

class cptall_ValSelector: public cptall_Ephemeral{ 
  public:
     cptall_AbstractBranching *branching;} 
;

class cptall_MidValHeuristic: public cptall_ValSelector{ 
  public:} 
;

class cptall_MinValHeuristic: public cptall_ValSelector{ 
  public:} 
;

class cptall_MaxValHeuristic: public cptall_ValSelector{ 
  public:} 
;

class cptall_AssignVar: public cptall_LargeBranching{ 
  public:
     cptall_VarSelector *varHeuristic;
     cptall_ValIterator *valHeuristic;} 
;

class cptall_SplitDomain: public cptall_BinBranching{ 
  public:
     cptall_VarSelector *varHeuristic;
     cptall_ValSelector *valHeuristic;
     int dichotomyThreshold;} 
;

class cptall_AssignOrForbid: public cptall_BinBranching{ 
  public:
     cptall_VarSelector *varHeuristic;
     cptall_ValSelector *valHeuristic;} 
;

class cptall_Solve: public cptall_GlobalSearchSolver{ 
  public:} 
;

class cptall_Atom: public ephemeral_object{ 
  public:
     cptall_Predicate *pred;
     list *terms;
     OID tinitRat;} 
;

class cptall_Symbol: public ClaireObject{ 
  public:
     char *name;} 
;

class cptall_Term: public cptall_Symbol{ 
  public:
     ClaireBoolean *isvar;} 
;

class cptall_Rational: public ephemeral_object{ 
  public:
     int num;
     int den;} 
;

class cptall_OperationTerms: public ephemeral_object{ 
  public:
     list *terms;} 
;

class cptall_Addition: public cptall_OperationTerms{ 
  public:} 
;

class cptall_Substraction: public cptall_OperationTerms{ 
  public:} 
;

class cptall_Multiplication: public cptall_OperationTerms{ 
  public:} 
;

class cptall_Division: public cptall_OperationTerms{ 
  public:} 
;

class cptall_Counter: public cptall_Ephemeral{ 
  public:
     int nb;
     int old;} 
;

class cptall_BoolArray: public ephemeral_object{ 
  public:
     list *chunks;} 
;

class cptall_Session: public cptall_Ephemeral{ 
  public:
     ClaireBoolean *canonicity;
     ClaireBoolean *already_used_actions;
     int conflicts;
     int dichotomy;
     int distances;
     char *facts;
     int init_heuristic;
     cptall_Rational *initial_bound;
     ClaireBoolean *instance_only;
     ClaireBoolean *interactive;
     ClaireBoolean *landmarks;
     list *memory;
     ClaireBoolean *mutex_sets;
     char *operators;
     ClaireBoolean *print_domains;
     ClaireBoolean *print_actions;
     ClaireBoolean *print_events;
     ClaireBoolean *propagate_inactive_causals;
     ClaireBoolean *propagate_inactive_threats;
     ClaireBoolean *propagate_causals;
     ClaireBoolean *propagate_mutex;
     ClaireBoolean *read_distances;
     ClaireBoolean *relevance;
     int strategy;
     ClaireBoolean *write_distances;} 
;

class cptall_Predicate: public cptall_Symbol{ 
  public:
     int arity;
     ClaireBoolean *typing;
     set *upTypes;} 
;

class cptall_Variabl: public cptall_Term{ 
  public:
     int index;
     list *inequalities;
     set *attPrec;
     set *attEffect;
     set *domaine;} 
;

class cptall_Constant: public cptall_Term{ 
  public:} 
;

class cptall_Fluent: public cptall_Ephemeral{ 
  public:
     int num;
     int tinit;
     char *name;
     list *producers;
     list *consumers;
     list *deleters;
     list *causals;
     list *activeCausals;
     list *pairCost;
     ClaireBoolean *enabled;
     ClaireBoolean *reachable;} 
;

class cptall_Action: public cptall_IntVar{ 
  public:
     int num;
     int numinit;
     ClaireBoolean *isevent;
     list *parameters;
     list *prec;
     list *add;
     list *del;
     list *edel;
     list *causals;
     set *actionsMutex;
     list *mutexSolved;
     cptall_Rational *durationRat;
     int duration;
     int tinit;
     int tinitEvent;
     int tend;
     list *distance;
     list *properties;
     cptall_BoolArray *commutative;
     ClaireBoolean *used;
     ClaireBoolean *excluded;
     ClaireBoolean *reachable;
     int reachedPrec;} 
;

class cptall_CausalLink: public cptall_IntVar{ 
  public:
     cptall_Fluent *fluent;
     cptall_Action *consumer;
     cptall_IntVar *init;
     int excl;
     int cmax;
     int offset;
     OID solvedThreats;
     int bestValue;} 
;

class cptall_PlanningProblem: public cptall_Problem{ 
  public:
     cptall_Instance *instance;
     list *fluents;
     list *actions;
     list *events;
     cptall_Action *startAction;
     cptall_Action *endAction;
     list *causalLinks;
     list *activeCausals;
     list *activeActions;
     list *mutexSets;
     int nbActionsMore;
     int nbCausalsMore;
     int nbSolvedThreats;
     cptall_Counter *cptChoiceSupport;
     cptall_Counter *cptChoiceConflict;
     cptall_Counter *cptChoiceMutex;
     cptall_Session *session;} 
;

class cptall_Instance: public ClaireObject{ 
  public:
     char *name;
     cptall_Domain *domain;
     list *initState;
     list *timedLitterals;
     list *actions;
     list *events;
     list *fluents;
     list *goal;
     int pgcd;
     int ppcm;} 
;

class cptall_TransitionRule: public ClaireObject{ 
  public:
     set *enablers;
     set *consequences;} 
;

class cptall_PositiveAtom: public cptall_Atom{ 
  public:} 
;

class cptall_NegativeAtom: public cptall_Atom{ 
  public:} 
;

class cptall_Operator: public ClaireObject{ 
  public:
     char *name;
     list *parameters;
     list *precondition;
     list *addEffect;
     list *delEffect;
     list *constraints;
     ephemeral_object *durationExpr;
     int duration;} 
;

class cptall_Domain: public ClaireObject{ 
  public:
     char *name;
     ClaireBoolean *durative;
     ClaireBoolean *equality;
     list *operators;} 
;

class cptall_SupportConstraint: public cptall_BinIntConstraint{ 
  public:} 
;

class cptall_ActionConstraint: public cptall_UnIntConstraint{ 
  public:} 
;

class cptall_PrecedenceConstraint: public cptall_BinIntConstraint{ 
  public:} 
;

class cptall_CausalPrecedenceConstraint: public cptall_TernIntConstraint{ 
  public:} 
;

class cptall_InitTimeDomain: public cptall_VarSelector{ 
  public:
     cptall_PlanningProblem *problem;} 
;

class cptall_ProducerDomain: public cptall_VarSelector{ 
  public:
     cptall_PlanningProblem *problem;} 
;

class cptall_BestProducerHeuristic: public cptall_ValSelector{ 
  public:} 
;

class cptall_SolveConflict: public cptall_BinBranching{ 
  public:
     cptall_Conflict *bestConflict;} 
;

class cptall_ConflictBranching: public cptall_CompositeBranching{ 
  public:} 
;

class cptall_Conflict: public cptall_Ephemeral{ 
  public:
     cptall_CausalLink *causal;
     cptall_Action *threat;} 
;

class cptall_Mutex: public cptall_Ephemeral{ 
  public:
     cptall_Action *action1;
     cptall_Action *action2;} 
;

class cptall_SolveMutex: public cptall_BinBranching{ 
  public:
     cptall_Mutex *bestMutex;} 
;
extern int  claire_max_integer2(int x,int y);
extern int  claire_min_integer2(int x,int y);
extern int  claire_max_bag(bag *x);
extern int  claire_min_bag(bag *x);
extern int  claire_sum_bag(bag *x);
extern int  claire_product_bag(bag *x);
extern int  claire_count_any(OID S);
extern OID  claire_random_list(list *l);
extern int  claire_random_Interval(Interval *I);
extern int  claire_random_integer2(int a,int b);
extern ClaireBoolean * cptall_knownInt_integer(int x);
extern void  claire_self_print_StoredInt_cptall(StoredInt *x);
extern void  claire_write_StoredInt(StoredInt *x,int y);
extern int  claire_read_StoredInt(StoredInt *x);
extern void  cptall_make_simple_bklist_any(OID x,OID bkl,int size,ClaireType *t,OID e);
extern void  cptall_showChocoLicense_void();
extern cptall_Solution * cptall_makeSolution_Solver(cptall_Solver *a,int nbVars);
extern cptall_IntVar * cptall_getIntVar_Problem(cptall_Problem *p,int i);
extern list * cptall_domainSequence_AbstractIntDomain(cptall_AbstractIntDomain *d);
extern set * cptall_domainSet_AbstractIntDomain(cptall_AbstractIntDomain *d);
extern int  cptall_getDomainInf_AbstractIntDomain(cptall_AbstractIntDomain *d);
extern int  cptall_getDomainSup_AbstractIntDomain(cptall_AbstractIntDomain *d);
extern int  cptall_updateDomainInf_AbstractIntDomain(cptall_AbstractIntDomain *d,int x);
extern int  cptall_updateDomainSup_AbstractIntDomain(cptall_AbstractIntDomain *d,int x);
extern ClaireBoolean * cptall_containsValInDomain_AbstractIntDomain(cptall_AbstractIntDomain *d,int x);
extern ClaireBoolean * cptall_removeDomainVal_AbstractIntDomain(cptall_AbstractIntDomain *d,int x);
extern void  cptall_restrict_AbstractIntDomain(cptall_AbstractIntDomain *d,int x);
extern int  cptall_getDomainCard_AbstractIntDomain(cptall_AbstractIntDomain *d);
extern int  cptall_getNextValue_AbstractIntDomain(cptall_AbstractIntDomain *d,int x);
extern int  cptall_getPrevValue_AbstractIntDomain(cptall_AbstractIntDomain *d,int x);
extern void  claire_self_print_LinkedListIntDomain_cptall(cptall_LinkedListIntDomain *x);
extern cptall_LinkedListIntDomain * cptall_makeLinkedListIntDomain_integer(int a,int b);
extern int  claire_random_LinkedListIntDomain(cptall_LinkedListIntDomain *d);
extern set * cptall_domainSet_LinkedListIntDomain(cptall_LinkedListIntDomain *d);
extern list * cptall_domainSequence_LinkedListIntDomain(cptall_LinkedListIntDomain *d);
extern int  cptall_getDomainInf_LinkedListIntDomain(cptall_LinkedListIntDomain *d);
extern int  cptall_getDomainSup_LinkedListIntDomain(cptall_LinkedListIntDomain *d);
extern ClaireBoolean * cptall_isIncludedIn_LinkedListIntDomain(cptall_LinkedListIntDomain *b,list *l);
extern int  cptall_getDomainCard_LinkedListIntDomain(cptall_LinkedListIntDomain *d);
extern ClaireBoolean * cptall_containsValInDomain_LinkedListIntDomain(cptall_LinkedListIntDomain *d,int x);
extern int  cptall_getNextValue_LinkedListIntDomain(cptall_LinkedListIntDomain *d,int x);
extern int  cptall_getPrevValue_LinkedListIntDomain(cptall_LinkedListIntDomain *d,int x);
extern ClaireBoolean * cptall_removeDomainVal_LinkedListIntDomain(cptall_LinkedListIntDomain *d,int x);
extern void  cptall_restrict_LinkedListIntDomain(cptall_LinkedListIntDomain *d,int x);
extern int  cptall_updateDomainInf_LinkedListIntDomain(cptall_LinkedListIntDomain *d,int x);
extern int  cptall_updateDomainSup_LinkedListIntDomain(cptall_LinkedListIntDomain *d,int x);
extern void  claire_self_print_Instantiation_cptall(cptall_Instantiation *e);
extern void  claire_self_print_InstInt_cptall(cptall_InstInt *e);
extern void  claire_self_print_BoundUpdate_cptall(cptall_BoundUpdate *e);
extern void  claire_self_print_IncInf_cptall(cptall_IncInf *e);
extern void  claire_self_print_DecSup_cptall(cptall_DecSup *e);
extern void  claire_self_print_ValueRemovals_cptall(cptall_ValueRemovals *e);
extern void  claire_self_print_ConstAwakeEvent_cptall(cptall_ConstAwakeEvent *e);
extern cptall_BipartiteSet * cptall_makeBipartiteSet_integer(int nb);
extern void  cptall_swap_BipartiteSet(cptall_BipartiteSet *b,int idx1,int idx2);
extern void  cptall_moveLeft_BipartiteSet(cptall_BipartiteSet *b,cptall_ConstAwakeEvent *obj);
extern void  cptall_moveRight_BipartiteSet(cptall_BipartiteSet *b,cptall_ConstAwakeEvent *obj);
extern void  cptall_moveAllLeft_BipartiteSet(cptall_BipartiteSet *b);
extern void  cptall_moveAllRight_BipartiteSet(cptall_BipartiteSet *b);
extern void  cptall_addRight_BipartiteSet(cptall_BipartiteSet *b,cptall_ConstAwakeEvent *obj);
extern void  cptall_addLeft_BipartiteSet(cptall_BipartiteSet *b,cptall_ConstAwakeEvent *obj);
extern ClaireBoolean * cptall_isLeft_BipartiteSet(cptall_BipartiteSet *b,cptall_ConstAwakeEvent *obj);
extern ClaireBoolean * cptall_isIn_BipartiteSet(cptall_BipartiteSet *b,cptall_ConstAwakeEvent *obj);
extern int  cptall_getNbLeft_BipartiteSet(cptall_BipartiteSet *b);
extern int  cptall_getNbRight_BipartiteSet(cptall_BipartiteSet *b);
extern int  cptall_getNbObjects_BipartiteSet(cptall_BipartiteSet *b);
extern cptall_ConstAwakeEvent * cptall_getObject_BipartiteSet(cptall_BipartiteSet *b,int i);
extern list * cptall_leftPart_BipartiteSet(cptall_BipartiteSet *b);
extern list * cptall_rightPart_BipartiteSet(cptall_BipartiteSet *b);
extern void  cptall_raiseContradiction_PropagationEngine1(cptall_PropagationEngine *pe);
extern void  cptall_raiseContradiction_PropagationEngine2(cptall_PropagationEngine *pe,cptall_Ephemeral *x);
extern OID  cptall_getContradictionCause_PropagationEngine(cptall_PropagationEngine *pe);
extern void  cptall_raiseContradiction_AbstractVar(cptall_AbstractVar *v);
extern void  cptall_raiseContradiction_AbstractConstraint(cptall_AbstractConstraint *c);
extern void  cptall_raiseContradiction_Problem(cptall_Problem *pb);
extern cptall_ChocEngine * cptall_makeChocEngine_integer(int n);
extern void  cptall_attachPropagationEngine_Problem(cptall_Problem *pb,cptall_PropagationEngine *pe);
extern ClaireBoolean * cptall_isEmpty_EventQueue(cptall_EventQueue *q);
extern ClaireBoolean * cptall_isEmpty_InstantiationStack(cptall_InstantiationStack *s);
extern cptall_BoundUpdate * cptall_popNextEvent_BoundEventQueue(cptall_BoundEventQueue *q);
extern cptall_ValueRemovals * cptall_popNextEvent_RemovalEventQueue(cptall_RemovalEventQueue *q);
extern cptall_Instantiation * cptall_popNextEvent_InstantiationStack(cptall_InstantiationStack *q);
extern int  cptall_nextEventPostIndex_BoundEventQueue(cptall_BoundEventQueue *q);
extern int  cptall_nextEventPostIndex_RemovalEventQueue(cptall_RemovalEventQueue *q);
extern void  cptall_raiseOverflowWarning_PropagationEngine(cptall_PropagationEngine *pe);
extern int  cptall_nextEventPostIndex_InstantiationStack(cptall_InstantiationStack *s);
extern void  cptall_flushEventQueue_BoundEventQueue(cptall_BoundEventQueue *q);
extern void  cptall_flushEventQueue_RemovalEventQueue(cptall_RemovalEventQueue *q);
extern void  cptall_flushEventQueue_ConstAwakeEventQueue(cptall_ConstAwakeEventQueue *q);
extern void  cptall_checkCleanState_ChocEngine(cptall_ChocEngine *pe);
extern void  cptall_pushWorld_Problem(cptall_Problem *pb);
extern void  cptall_popWorld_Problem(cptall_Problem *pb);
extern void  cptall_setWorld_Problem(cptall_Problem *pb,int n);
extern void  cptall_commitWorld_Problem(cptall_Problem *pb);
extern void  cptall_flushCurrentOpenEvents_ChocEngine(cptall_ChocEngine *pe);
extern void  cptall_postInstInt_PropagationEngine(cptall_PropagationEngine *pe,cptall_IntVar *v,int i);
extern void  cptall_postRemoveVal_PropagationEngine(cptall_PropagationEngine *pe,cptall_IntVar *v,int x,int i);
extern void  cptall_postUpdateInf_PropagationEngine(cptall_PropagationEngine *pe,cptall_IntVar *v,int i);
extern void  cptall_postUpdateSup_PropagationEngine(cptall_PropagationEngine *pe,cptall_IntVar *v,int i);
extern void  cptall_postConstAwake_PropagationEngine(cptall_PropagationEngine *pe,cptall_AbstractConstraint *c,ClaireBoolean *init);
extern void  cptall_postInstantiateEvt_ChocEngine(cptall_ChocEngine *pe,cptall_Instantiation *e,int i);
extern void  cptall_postInstInt_ChocEngine(cptall_ChocEngine *pe,cptall_IntVar *v,int i);
extern void  cptall_postRemoveVal_ChocEngine(cptall_ChocEngine *pe,cptall_IntVar *v,int x,int i);
extern void  cptall_postBoundEvent_ChocEngine(cptall_ChocEngine *pe,cptall_BoundUpdate *e,int i);
extern void  cptall_postUpdateInf_ChocEngine(cptall_ChocEngine *pe,cptall_IntVar *v,int i);
extern void  cptall_postUpdateSup_ChocEngine(cptall_ChocEngine *pe,cptall_IntVar *v,int i);
extern cptall_ConstAwakeEventQueue * cptall_getQueue_ChocEngine(cptall_ChocEngine *pe,cptall_ConstAwakeEvent *evt);
extern void  cptall_registerEvent_ChocEngine(cptall_ChocEngine *pe,cptall_ConstAwakeEvent *evt);
extern ClaireBoolean * cptall_postConstAwake_ChocEngine(cptall_ChocEngine *pe,cptall_AbstractConstraint *c,ClaireBoolean *init);
extern void  cptall_remove_ConstAwakeEventQueue(cptall_ConstAwakeEventQueue *q,cptall_ConstAwakeEvent *evt);
extern void  cptall_constAwake_AbstractConstraint(cptall_AbstractConstraint *c,ClaireBoolean *init);
extern void  cptall_popSomeEvents_InstantiationStack(cptall_InstantiationStack *q);
extern void  cptall_propagateEvent_Instantiation(cptall_Instantiation *e);
extern void  cptall_popSomeEvents_RemovalEventQueue(cptall_RemovalEventQueue *q);
extern void  cptall_propagateEvent_ValueRemovals(cptall_ValueRemovals *e,cptall_RemovalEventQueue *q);
extern void  cptall_popSomeEvents_BoundEventQueue(cptall_BoundEventQueue *q);
extern void  cptall_propagateEvent_BoundUpdate(cptall_BoundUpdate *e,cptall_BoundEventQueue *q);
extern ClaireBoolean * cptall_isEmpty_ConstAwakeEventQueue(cptall_ConstAwakeEventQueue *q);
extern void  cptall_popSomeEvents_ConstAwakeEventQueue(cptall_ConstAwakeEventQueue *q);
extern cptall_ConstAwakeEvent * cptall_popNextEvent_ConstAwakeEventQueue(cptall_ConstAwakeEventQueue *q);
extern void  cptall_propagateEvent_ConstAwakeEvent(cptall_ConstAwakeEvent *e);
extern void  cptall_popSomeEvents_EventCollection(cptall_EventCollection *q);
extern OID  cptall_getNextActiveEventQueue_PropagationEngine(cptall_PropagationEngine *pe);
extern OID  cptall_getNextActiveConstraintEventQueue_ChocEngine(cptall_ChocEngine *pe);
extern OID  cptall_getNextActiveVariableEventQueue_ChocEngine(cptall_ChocEngine *pe);
extern OID  cptall_getNextActiveEventQueue2_ChocEngine(cptall_ChocEngine *pe);
extern OID  cptall_getNextActiveEventQueue_ChocEngine(cptall_ChocEngine *pe);
extern ClaireBoolean * cptall_isInstantiated_AbstractVar(cptall_AbstractVar *v);
extern cptall_AbstractConstraint * cptall_getConstraint_AbstractVar(cptall_AbstractVar *v,int i);
extern int  cptall_getDegree_AbstractVar(cptall_AbstractVar *v);
extern void  claire_self_print_IntVar_cptall(cptall_IntVar *v);
extern void  cptall_closeIntVar_IntVar(cptall_IntVar *v,int i,int j,int p);
extern ClaireBoolean * cptall_updateInf_IntVar1(cptall_IntVar *v,int x);
extern ClaireBoolean * cptall_updateSup_IntVar1(cptall_IntVar *v,int x);
extern ClaireBoolean * cptall_instantiate_IntVar1(cptall_IntVar *v,int x);
extern ClaireBoolean * cptall_removeVal_IntVar1(cptall_IntVar *v,int x);
extern ClaireType * claire_domain_IntVar(cptall_IntVar *x);
extern ClaireBoolean * cptall_isInstantiatedTo_IntVar(cptall_IntVar *v,int x);
extern ClaireBoolean * cptall_canBeInstantiatedTo_IntVar(cptall_IntVar *v,int x);
extern ClaireBoolean * cptall_canBeEqualTo_IntVar(cptall_IntVar *v,cptall_IntVar *x);
extern ClaireBoolean * cptall_domainIncludedIn_IntVar(cptall_IntVar *v,list *sortedList);
extern ClaireBoolean * cptall_canBeInstantiatedIn_IntVar(cptall_IntVar *v,list *sortedList);
extern ClaireBoolean * cptall_hasExactDomain_IntVar(cptall_IntVar *v);
extern int  claire_random_IntVar(cptall_IntVar *v);
extern int  cptall_getNextDomainValue_IntVar(cptall_IntVar *v,int i);
extern int  cptall_getPrevDomainValue_IntVar(cptall_IntVar *v,int i);
extern int  cptall_getInf_IntVar(cptall_IntVar *v);
extern int  cptall_getSup_IntVar(cptall_IntVar *v);
extern ClaireBoolean * cptall_isInstantiated_IntVar(cptall_IntVar *v);
extern int  cptall_getValue_IntVar(cptall_IntVar *v);
extern int  cptall_getDomainSize_IntVar(cptall_IntVar *v);
extern ClaireBoolean * cptall_updateInf_IntVar2(cptall_IntVar *v,int x,int idx);
extern ClaireBoolean * cptall_updateSup_IntVar2(cptall_IntVar *v,int x,int idx);
extern ClaireBoolean * cptall_removeVal_IntVar2(cptall_IntVar *v,int x,int idx);
extern ClaireBoolean * cptall_instantiate_IntVar2(cptall_IntVar *v,int x,int idx);
extern ClaireBoolean * cptall_removeInterval_IntVar(cptall_IntVar *v,int a,int b,int idx);
extern void  cptall_restrictTo_IntVar(cptall_IntVar *v,list *sortedList,int idx);
extern void  cptall_closeLargeIntConstraint_LargeIntConstraint(cptall_LargeIntConstraint *c);
extern void  cptall_closeCompositeConstraint_BinCompositeConstraint(cptall_BinCompositeConstraint *c);
extern void  cptall_closeCompositeConstraint_LargeCompositeConstraint(cptall_LargeCompositeConstraint *c);
extern int  cptall_getNbVarsFromSubConst_LargeCompositeConstraint(cptall_LargeCompositeConstraint *c);
extern int  cptall_assignIndices_AbstractConstraint(cptall_AbstractConstraint *c1,cptall_CompositeConstraint *root,int i);
extern int  cptall_assignIndices_UnIntConstraint(cptall_UnIntConstraint *c1,cptall_CompositeConstraint *root,int i);
extern int  cptall_assignIndices_BinIntConstraint(cptall_BinIntConstraint *c1,cptall_CompositeConstraint *root,int i);
extern int  cptall_assignIndices_TernIntConstraint(cptall_TernIntConstraint *c1,cptall_CompositeConstraint *root,int i);
extern int  cptall_assignIndices_LargeIntConstraint(cptall_LargeIntConstraint *c1,cptall_CompositeConstraint *root,int i);
extern int  cptall_assignIndices_BinCompositeConstraint(cptall_BinCompositeConstraint *c1,cptall_CompositeConstraint *root,int i);
extern int  cptall_assignIndices_LargeCompositeConstraint(cptall_LargeCompositeConstraint *c1,cptall_CompositeConstraint *root,int i);
extern void  cptall_setConstraintIndex_AbstractConstraint(cptall_AbstractConstraint *c,int i,int val);
extern void  cptall_setConstraintIndex_UnIntConstraint(cptall_UnIntConstraint *c,int i,int val);
extern void  cptall_setConstraintIndex_BinIntConstraint(cptall_BinIntConstraint *c,int i,int val);
extern void  cptall_setConstraintIndex_TernIntConstraint(cptall_TernIntConstraint *c,int i,int val);
extern void  cptall_setConstraintIndex_LargeIntConstraint(cptall_LargeIntConstraint *c,int i,int val);
extern void  cptall_setConstraintIndex_BinCompositeConstraint(cptall_BinCompositeConstraint *c,int i,int val);
extern void  cptall_setConstraintIndex_LargeCompositeConstraint(cptall_LargeCompositeConstraint *c,int i,int val);
extern int  cptall_getConstraintIdx_AbstractConstraint(cptall_AbstractConstraint *c,int idx);
extern int  cptall_getConstraintIdx_UnIntConstraint(cptall_UnIntConstraint *c,int idx);
extern int  cptall_getConstraintIdx_BinIntConstraint(cptall_BinIntConstraint *c,int idx);
extern int  cptall_getConstraintIdx_TernIntConstraint(cptall_TernIntConstraint *c,int idx);
extern int  cptall_getConstraintIdx_LargeIntConstraint(cptall_LargeIntConstraint *c,int idx);
extern int  cptall_getConstraintIdx_BinCompositeConstraint(cptall_BinCompositeConstraint *c,int idx);
extern int  cptall_getConstraintIdx_LargeCompositeConstraint(cptall_LargeCompositeConstraint *c,int idx);
extern ClaireBoolean * cptall_testIfCompletelyInstantiated_IntConstraint(cptall_IntConstraint *c);
extern ClaireBoolean * cptall_testIfCompletelyInstantiated_UnIntConstraint(cptall_UnIntConstraint *c);
extern ClaireBoolean * cptall_testIfCompletelyInstantiated_BinIntConstraint(cptall_BinIntConstraint *c);
extern ClaireBoolean * cptall_testIfCompletelyInstantiated_TernIntConstraint(cptall_TernIntConstraint *c);
extern ClaireBoolean * cptall_testIfCompletelyInstantiated_LargeIntConstraint(cptall_LargeIntConstraint *c);
extern ClaireBoolean * cptall_testIfCompletelyInstantiated_BinCompositeConstraint(cptall_BinCompositeConstraint *c);
extern ClaireBoolean * cptall_testIfCompletelyInstantiated_LargeCompositeConstraint(cptall_LargeCompositeConstraint *c);
extern int  cptall_getNbVars_AbstractConstraint(cptall_AbstractConstraint *c);
extern int  cptall_getNbVars_UnIntConstraint(cptall_UnIntConstraint *c);
extern int  cptall_getNbVars_BinIntConstraint(cptall_BinIntConstraint *c);
extern int  cptall_getNbVars_TernIntConstraint(cptall_TernIntConstraint *c);
extern int  cptall_getNbVars_LargeIntConstraint(cptall_LargeIntConstraint *c);
extern int  cptall_getNbVars_LargeCompositeConstraint(cptall_LargeCompositeConstraint *c);
extern int  cptall_getNbVars_BinCompositeConstraint(cptall_BinCompositeConstraint *c);
extern cptall_AbstractVar * cptall_getVar_AbstractConstraint(cptall_AbstractConstraint *c,int i);
extern cptall_AbstractVar * cptall_getVar_UnIntConstraint(cptall_UnIntConstraint *c,int i);
extern cptall_AbstractVar * cptall_getVar_BinIntConstraint(cptall_BinIntConstraint *c,int i);
extern cptall_AbstractVar * cptall_getVar_TernIntConstraint(cptall_TernIntConstraint *c,int i);
extern cptall_AbstractVar * cptall_getVar_LargeIntConstraint(cptall_LargeIntConstraint *c,int i);
extern cptall_AbstractVar * cptall_getVar_LargeCompositeConstraint(cptall_LargeCompositeConstraint *c,int i);
extern cptall_AbstractVar * cptall_getVar_BinCompositeConstraint(cptall_BinCompositeConstraint *c,int i);
extern void  cptall_connect_AbstractConstraint(cptall_AbstractConstraint *c);
extern cptall_AbstractConstraint * cptall_opposite_AbstractConstraint(cptall_AbstractConstraint *c);
extern OID  cptall_askIfEntailed_AbstractConstraint(cptall_AbstractConstraint *c);
extern void  cptall_propagate_Ephemeral_cptall(cptall_Ephemeral *c);
extern void  cptall_awakeOnInf_AbstractConstraint_cptall(cptall_AbstractConstraint *c,int idx);
extern void  cptall_awakeOnSup_AbstractConstraint_cptall(cptall_AbstractConstraint *c,int idx);
extern void  cptall_awakeOnInst_AbstractConstraint_cptall(cptall_AbstractConstraint *c,int idx);
extern void  cptall_awakeOnRem_AbstractConstraint_cptall(cptall_AbstractConstraint *c,int idx,int x);
extern void  cptall_awakeOnVar_AbstractConstraint_cptall(cptall_AbstractConstraint *c,int idx);
extern void  cptall_awake_AbstractConstraint_cptall(cptall_AbstractConstraint *c);
extern ClaireBoolean * cptall_testIfSatisfied_AbstractConstraint(cptall_AbstractConstraint *c);
extern ClaireBoolean * cptall_testIfCompletelyInstantiated_AbstractConstraint(cptall_AbstractConstraint *c);
extern void  cptall_propagate_UnIntConstraint_cptall(cptall_UnIntConstraint *c);
extern void  cptall_propagate_BinIntConstraint_cptall(cptall_BinIntConstraint *c);
extern void  cptall_propagate_LargeIntConstraint_cptall(cptall_LargeIntConstraint *c);
extern void  cptall_doAwake_AbstractConstraint(cptall_AbstractConstraint *c);
extern void  cptall_doPropagate_AbstractConstraint(cptall_AbstractConstraint *c);
extern void  cptall_doAwakeOnInf_AbstractConstraint(cptall_AbstractConstraint *c,int idx);
extern void  cptall_doAwakeOnSup_AbstractConstraint(cptall_AbstractConstraint *c,int idx);
extern void  cptall_doAwakeOnInst_AbstractConstraint(cptall_AbstractConstraint *c,int idx);
extern void  cptall_doAwakeOnRem_AbstractConstraint(cptall_AbstractConstraint *c,int idx,int x);
extern void  cptall_doAwakeOnVar_AbstractConstraint(cptall_AbstractConstraint *c,int idx);
extern OID  cptall_askIfTrue_AbstractConstraint(cptall_AbstractConstraint *c);
extern ClaireBoolean * cptall_testIfTrue_AbstractConstraint(cptall_AbstractConstraint *c);
extern ClaireBoolean * cptall_testIfInstantiated_AbstractConstraint(cptall_AbstractConstraint *c);
extern void  cptall_addIntVar_Problem(cptall_Problem *p,cptall_IntVar *v);
extern void  cptall_connect_IntConstraint(cptall_IntConstraint *c);
extern void  cptall_connect_UnIntConstraint(cptall_UnIntConstraint *c);
extern void  cptall_connect_BinIntConstraint(cptall_BinIntConstraint *c);
extern void  cptall_connect_TernIntConstraint(cptall_TernIntConstraint *c);
extern void  cptall_connect_LargeIntConstraint(cptall_LargeIntConstraint *c);
extern void  cptall_connect_CompositeConstraint(cptall_CompositeConstraint *c);
extern void  cptall_connectIntVar_AbstractConstraint(cptall_AbstractConstraint *cont,cptall_IntVar *u,int i);
extern void  cptall_connectEvent_VarEvent(cptall_VarEvent *e,int nbconst);
extern OID  cptall_selectBranchingObject_AbstractBranching(cptall_AbstractBranching *b);
extern void  cptall_goDownBranch_AbstractBranching(cptall_AbstractBranching *b,OID x,int i);
extern void  cptall_traceDownBranch_AbstractBranching(cptall_AbstractBranching *b,OID x,int i);
extern int  cptall_getFirstBranch_AbstractBranching(cptall_AbstractBranching *b,OID x);
extern int  cptall_getNextBranch_AbstractBranching(cptall_AbstractBranching *b,OID x,int i);
extern void  cptall_goUpBranch_AbstractBranching(cptall_AbstractBranching *b,OID x,int i);
extern void  cptall_traceUpBranch_AbstractBranching(cptall_AbstractBranching *b,OID x,int i);
extern ClaireBoolean * cptall_finishedBranching_AbstractBranching(cptall_AbstractBranching *b,OID x,int i);
extern ClaireBoolean * cptall_branchOn_AbstractBranching(cptall_AbstractBranching *b,OID v,int n);
extern void  cptall_goUpBranch_BinBranching(cptall_BinBranching *b,OID x,int i);
extern void  cptall_traceUpBranch_BinBranching(cptall_BinBranching *b,OID x,int i);
extern int  cptall_getFirstBranch_BinBranching(cptall_BinBranching *b,OID x);
extern int  cptall_getNextBranch_BinBranching(cptall_BinBranching *b,OID x,int i);
extern ClaireBoolean * cptall_finishedBranching_BinBranching(cptall_BinBranching *b,OID x,int i);
extern void  cptall_goUpBranch_LargeBranching(cptall_LargeBranching *b,OID x,int i);
extern void  cptall_traceUpBranch_LargeBranching(cptall_LargeBranching *b,OID x,int i);
extern int  cptall_getFirstBranch_LargeBranching(cptall_LargeBranching *b,OID x);
extern int  cptall_getNextBranch_LargeBranching(cptall_LargeBranching *b,OID x,int i);
extern ClaireBoolean * cptall_finishedBranching_LargeBranching(cptall_LargeBranching *b,OID x,int i);
extern cptall_AbstractBranching * cptall_selectBranching_CompositeBranching_cptall(cptall_CompositeBranching *b);
extern OID  cptall_selectBranchingObject_CompositeBranching(cptall_CompositeBranching *b);
extern int  cptall_getFirstBranch_CompositeBranching(cptall_CompositeBranching *b,OID xpair);
extern int  cptall_getNextBranch_CompositeBranching(cptall_CompositeBranching *b,OID xpair,int i);
extern void  cptall_goUpBranch_CompositeBranching(cptall_CompositeBranching *b,OID xpair,int i);
extern void  cptall_traceUpBranch_CompositeBranching(cptall_CompositeBranching *b,OID xpair,int i);
extern void  cptall_goDownBranch_CompositeBranching(cptall_CompositeBranching *b,OID xpair,int i);
extern void  cptall_traceDownBranch_CompositeBranching(cptall_CompositeBranching *b,OID xpair,int i);
extern ClaireBoolean * cptall_finishedBranching_CompositeBranching(cptall_CompositeBranching *b,OID xpair,int i);
extern OID  cptall_selectVar_VarSelector(cptall_VarSelector *vs);
extern OID  cptall_selectVar_MinDomain(cptall_MinDomain *vs);
extern OID  cptall_selectVar_MaxDeg(cptall_MaxDeg *vs);
extern OID  cptall_selectVar_DomDeg(cptall_DomDeg *vs);
extern OID  cptall_selectVar_StaticVarOrder(cptall_StaticVarOrder *vs);
extern ClaireBoolean * cptall_isOver_ValIterator(cptall_ValIterator *vi,cptall_IntVar *x,int i);
extern int  cptall_getFirstVal_ValIterator(cptall_ValIterator *vi,cptall_IntVar *x);
extern int  cptall_getNextVal_ValIterator(cptall_ValIterator *vi,cptall_IntVar *x,int i);
extern ClaireBoolean * cptall_isOver_IncreasingDomain(cptall_IncreasingDomain *vi,cptall_IntVar *x,int i);
extern int  cptall_getFirstVal_IncreasingDomain(cptall_IncreasingDomain *vi,cptall_IntVar *x);
extern int  cptall_getNextVal_IncreasingDomain(cptall_IncreasingDomain *vi,cptall_IntVar *x,int i);
extern ClaireBoolean * cptall_isOver_DecreasingDomain(cptall_DecreasingDomain *vi,cptall_IntVar *x,int i);
extern int  cptall_getFirstVal_DecreasingDomain(cptall_DecreasingDomain *vi,cptall_IntVar *x);
extern int  cptall_getNextVal_DecreasingDomain(cptall_DecreasingDomain *vi,cptall_IntVar *x,int i);
extern int  cptall_getBestVal_ValSelector(cptall_ValSelector *vh,cptall_IntVar *x);
extern int  cptall_getBestVal_MidValHeuristic(cptall_MidValHeuristic *vh,cptall_IntVar *x);
extern int  cptall_getBestVal_MinValHeuristic(cptall_MinValHeuristic *vh,cptall_IntVar *x);
extern int  cptall_getBestVal_MaxValHeuristic(cptall_MaxValHeuristic *vh,cptall_IntVar *x);
extern ClaireBoolean * cptall_getFeasibility_GlobalSearchSolver(cptall_GlobalSearchSolver *a);
extern ClaireBoolean * cptall_newNode_GlobalSearchLimit(cptall_GlobalSearchLimit *lim,cptall_GlobalSearchSolver *a);
extern void  cptall_endNode_GlobalSearchLimit(cptall_GlobalSearchLimit *lim,cptall_GlobalSearchSolver *a);
extern void  cptall_reset_GlobalSearchLimit(cptall_GlobalSearchLimit *lim,ClaireBoolean *start);
extern void  cptall_reset_CounterLimit(cptall_CounterLimit *lim,ClaireBoolean *start);
extern ClaireBoolean * cptall_newNode_CounterLimit(cptall_CounterLimit *lim,cptall_GlobalSearchSolver *a);
extern void  claire_self_print_CounterLimit_cptall(cptall_CounterLimit *lim);
extern ClaireBoolean * cptall_newNode_NodeLimit(cptall_NodeLimit *lim,cptall_GlobalSearchSolver *a);
extern void  cptall_endNode_BacktrackLimit(cptall_BacktrackLimit *lim,cptall_GlobalSearchSolver *a);
extern ClaireBoolean * cptall_newNode_TimeLimit(cptall_TimeLimit *lim,cptall_GlobalSearchSolver *algo);
extern void  cptall_reset_TimeLimit(cptall_TimeLimit *lim,ClaireBoolean *start);
extern void  claire_self_print_TimeLimit_cptall(cptall_TimeLimit *lim);
extern ClaireBoolean * cptall_newNode_DiscLimit(cptall_DiscLimit *lim,cptall_GlobalSearchSolver *algo);
extern void  cptall_reset_DiscLimit(cptall_DiscLimit *lim,ClaireBoolean *start);
extern void  claire_self_print_DiscLimit_cptall(cptall_DiscLimit *lim);
extern void  cptall_attach_GlobalSearchSolver(cptall_GlobalSearchSolver *newSolver,cptall_Problem *pb);
extern cptall_GlobalSearchSolver * cptall_composeGlobalSearch_GlobalSearchSolver(cptall_GlobalSearchSolver *algo,list *l);
extern ClaireType * cptall_composeGlobalSearch_GlobalSearchSolver_type(ClaireType *algo,ClaireType *l);
extern OID  cptall_getSmallestDomainUnassignedVar_Problem(cptall_Problem *pb);
extern OID  cptall_getSmallestDomainUnassignedVar_list(list *l);
extern OID  cptall_getDomOverDegBestUnassignedVar_Problem(cptall_Problem *pb);
extern OID  cptall_getDomOverDegBestUnassignedVar_list(list *l);
extern OID  cptall_getMostConstrainedUnassignedVar_Problem(cptall_Problem *pb);
extern OID  cptall_getMostConstrainedUnassignedVar_list(list *l);
extern ClaireBoolean * cptall_explore_AbstractBranching(cptall_AbstractBranching *b,int n);
extern ClaireBoolean * cptall_explore_CompositeBranching(cptall_CompositeBranching *b,int n);
extern ClaireBoolean * cptall_branchOn_LargeBranching(cptall_LargeBranching *b,OID v,int n);
extern ClaireBoolean * cptall_branchOn_BinBranching(cptall_BinBranching *b,OID v,int n);
extern OID  cptall_selectBranchingObject_SplitDomain(cptall_SplitDomain *b);
extern void  cptall_goDownBranch_SplitDomain(cptall_SplitDomain *b,OID x,int first);
extern void  cptall_traceDownBranch_SplitDomain(cptall_SplitDomain *b,OID x,int first);
extern OID  cptall_selectBranchingObject_AssignOrForbid(cptall_AssignOrForbid *b);
extern void  cptall_goDownBranch_AssignOrForbid(cptall_AssignOrForbid *b,OID x,int first);
extern void  cptall_traceDownBranch_AssignOrForbid(cptall_AssignOrForbid *b,OID x,int first);
extern OID  cptall_selectBranchingObject_AssignVar(cptall_AssignVar *b);
extern ClaireBoolean * cptall_finishedBranching_AssignVar(cptall_AssignVar *b,OID x,int i);
extern int  cptall_getFirstBranch_AssignVar(cptall_AssignVar *b,OID x);
extern int  cptall_getNextBranch_AssignVar(cptall_AssignVar *b,OID x,int i);
extern void  cptall_goDownBranch_AssignVar(cptall_AssignVar *b,OID x,int i);
extern void  cptall_goUpBranch_AssignVar(cptall_AssignVar *b,OID x,int i);
extern void  cptall_traceDownBranch_AssignVar(cptall_AssignVar *b,OID x,int i);
extern void  cptall_traceUpBranch_AssignVar(cptall_AssignVar *b,OID x,int i);
extern void  cptall_newTreeSearch_GlobalSearchSolver(cptall_GlobalSearchSolver *a);
extern void  cptall_endTreeSearch_GlobalSearchSolver(cptall_GlobalSearchSolver *algo);
extern void  cptall_newTreeNode_GlobalSearchSolver(cptall_GlobalSearchSolver *a);
extern void  cptall_endTreeNode_GlobalSearchSolver(cptall_GlobalSearchSolver *a);
extern void  cptall_run_GlobalSearchSolver(cptall_GlobalSearchSolver *algo);
extern void  cptall_run_Solve(cptall_Solve *algo);
extern cptall_Problem * cptall_getProblem_IntVar(cptall_IntVar *v);
extern cptall_Problem * cptall_getProblem_AbstractConstraint(cptall_AbstractConstraint *c);
extern cptall_Problem * cptall_getProblem_void();
extern cptall_Problem * cptall_getActiveProblem_void();
extern void  cptall_setActiveProblem_Problem(cptall_Problem *pb);
extern void  cptall_discardProblem_Problem(cptall_Problem *pb);
extern cptall_PropagationEngine * cptall_getPropagationEngine_Problem(cptall_Problem *p);
extern cptall_IntVar * cptall_makeBoundIntVar_Problem1(cptall_Problem *p,char *s,int i,int j);
extern cptall_IntVar * cptall_makeBoundIntVar_Problem2(cptall_Problem *p,int i,int j);
extern cptall_IntVar * cptall_makeBoundIntVar_Problem3(cptall_Problem *p,char *s);
extern cptall_IntVar * cptall_makeConstantIntVar_integer(int x);
extern cptall_IntVar * cptall_makeIntVar_Problem1(cptall_Problem *p,char *s,int i,int j);
extern cptall_IntVar * cptall_makeIntVar_Problem2(cptall_Problem *p,int i,int j);
extern cptall_IntVar * cptall_makeIntVar_Problem3(cptall_Problem *p,char *s);
extern cptall_IntVar * cptall_makeIntVar_Problem4(cptall_Problem *p,char *s,list *b);
extern cptall_IntVar * cptall_makeIntVar_Problem5(cptall_Problem *p,list *b);
extern void  cptall_post_Problem(cptall_Problem *p,cptall_AbstractConstraint *c);
extern void  cptall_setMin_IntVar(cptall_IntVar *v,int x);
extern void  cptall_setMax_IntVar(cptall_IntVar *v,int x);
extern void  cptall_setVal_IntVar(cptall_IntVar *v,int x);
extern void  cptall_remVal_IntVar(cptall_IntVar *v,int x);
extern void  cptall_propagate_Problem_cptall(cptall_Problem *p);
extern cptall_Problem * cptall_getProblem_Solver(cptall_Solver *algo);
extern cptall_GlobalSearchSolver * cptall_getSearchManager_AbstractBranching(cptall_AbstractBranching *b);
extern cptall_MinDomain * cptall_makeMinDomVarHeuristic_void();
extern cptall_DomDeg * cptall_makeDomDegVarHeuristic_void();
extern cptall_MaxDeg * cptall_makeMaxDegVarHeuristic_void();
extern cptall_StaticVarOrder * cptall_makeStaticVarHeuristic_list(list *l);
extern cptall_MinDomain * cptall_makeMinDomVarHeuristic_list(list *l);
extern cptall_DomDeg * cptall_makeDomDegVarHeuristic_list(list *l);
extern cptall_MaxDeg * cptall_makeMaxDegVarHeuristic_list(list *l);
extern cptall_ValIterator * cptall_makeIncValIterator_void();
extern cptall_ValIterator * cptall_makeDecValIterator_void();
extern cptall_ValSelector * cptall_makeMinValSelector_void();
extern cptall_ValSelector * cptall_makeMaxValSelector_void();
extern cptall_ValSelector * cptall_makeMidValSelector_void();
extern cptall_AssignVar * cptall_makeAssignVarBranching_VarSelector1(cptall_VarSelector *varh,cptall_ValIterator *valh);
extern cptall_AssignVar * cptall_makeAssignVarBranching_VarSelector2(cptall_VarSelector *varh);
extern cptall_AssignVar * cptall_makeAssignVarBranching_void();
extern cptall_SplitDomain * cptall_makeSplitDomBranching_VarSelector1(cptall_VarSelector *varh,cptall_ValSelector *valh,int threshold);
extern cptall_SplitDomain * cptall_makeSplitDomBranching_VarSelector2(cptall_VarSelector *varh,int threshold);
extern cptall_SplitDomain * cptall_makeSplitDomBranching_VarSelector3(cptall_VarSelector *varh);
extern cptall_SplitDomain * cptall_makeSplitDomBranching_void();
extern cptall_AssignOrForbid * cptall_makeAssignOrForbidBranching_VarSelector(cptall_VarSelector *varh,cptall_ValSelector *valh);
extern list * cptall_makeDefaultBranchingList_Problem(cptall_Problem *pb);
extern cptall_Solve * cptall_makeGlobalSearchSolver_boolean1(ClaireBoolean *allSolutions,list *l);
extern cptall_Solve * cptall_makeGlobalSearchSolver_boolean2(ClaireBoolean *allSolutions);
extern void  cptall_setSearchLimit_GlobalSearchSolver(cptall_GlobalSearchSolver *algo,cptall_GlobalSearchLimit *lim);
extern int  cptall_getNb_CounterLimit(cptall_CounterLimit *lim);
extern int  cptall_getMaxNb_CounterLimit(cptall_CounterLimit *lim);
extern ClaireBoolean * cptall_solve_Problem(cptall_Problem *pb,list *l,ClaireBoolean *all);
extern void  cptall_setMaxPrintDepth_GlobalSearchSolver(cptall_GlobalSearchSolver *algo,int n);
extern void  cptall_setMaxSolutionStorage_GlobalSearchSolver(cptall_GlobalSearchSolver *algo,int nb);
extern void  cptall_setVarsToShow_GlobalSearchSolver(cptall_GlobalSearchSolver *algo,list *l);
extern int  cptall_getNbSol_GlobalSearchSolver(cptall_GlobalSearchSolver *algo);
extern cptall_Problem * cptall_getProblem_AbstractBranching(cptall_AbstractBranching *b);
extern cptall_GlobalSearchSolver * cptall_getGlobalSearchSolver_Problem(cptall_Problem *p);
extern cptall_NodeLimit * cptall_makeNodeLimit_integer(int n);
extern cptall_BacktrackLimit * cptall_makeBacktrackLimit_integer(int n);
extern cptall_TimeLimit * cptall_makeTimeLimit_integer(int n);
extern cptall_DiscLimit * cptall_makeDiscLimit_integer(int n);
extern list * cptall_makeDefaultLimits_void();
extern void  cptall_setNodeLimit_GlobalSearchSolver(cptall_GlobalSearchSolver *algo,int n);
extern void  cptall_setTimeLimit_GlobalSearchSolver(cptall_GlobalSearchSolver *algo,int n);
extern void  cptall_setBacktrackLimit_GlobalSearchSolver(cptall_GlobalSearchSolver *algo,int n);
extern void  cptall_setDiscLimit_GlobalSearchSolver(cptall_GlobalSearchSolver *algo,int maxNbDiscepancies);
extern void  claire_self_print_Rational_cptall(cptall_Rational *r);
extern void  claire_self_print_list2_cptall(list *terms);
extern void  claire_self_print_Addition_cptall(cptall_Addition *a);
extern void  claire_self_print_Substraction_cptall(cptall_Substraction *s);
extern void  claire_self_print_Multiplication_cptall(cptall_Multiplication *m);
extern void  claire_self_print_Division_cptall(cptall_Division *d);
extern list * cptall_evaluateTerms_OperationTerms(cptall_OperationTerms *o,list *p);
extern cptall_Rational * cptall_evaluateExpr_Rational(cptall_Rational *r,list *p);
extern cptall_Rational * cptall_evaluateExpr_any(OID a,list *p);
extern cptall_Rational * cptall_evaluateExpr_Addition(cptall_Addition *a,list *p);
extern cptall_Rational * cptall_evaluateExpr_Substraction(cptall_Substraction *s,list *p);
extern cptall_Rational * cptall_evaluateExpr_Multiplication(cptall_Multiplication *m,list *p);
extern cptall_Rational * cptall_evaluateExpr_Division(cptall_Division *d,list *p);
extern void  cptall_addRational_Rational(cptall_Rational *r,cptall_Rational *t,ClaireBoolean *prem);
extern void  cptall_subRational_Rational(cptall_Rational *r,cptall_Rational *t,ClaireBoolean *prem);
extern void  cptall_multRational_Rational(cptall_Rational *r,cptall_Rational *t,ClaireBoolean *prem);
extern void  cptall_divRational_Rational(cptall_Rational *r,cptall_Rational *t,ClaireBoolean *prem);
extern void  cptall_simplify_Rational(cptall_Rational *r);
extern ClaireBoolean * claire__equal_Rational(cptall_Rational *r1,cptall_Rational *r2);
extern ClaireBoolean * claire__I_equal_Rational(cptall_Rational *r1,cptall_Rational *r2);
extern int  cptall_pgcd_integer(int a,int b);
extern int  cptall_ppcm_integer(int a,int b);
extern void  cptall_memoryVerification_integer(int x,int y,char *s);
extern void  cptall_fflush_void();
extern char * cptall_read_string_port(ClairePort *p);
extern int  cptall_read_int_port(ClairePort *p);
extern void  cptall_write_int_integer(int n,ClairePort *p);
extern char * cptall_format_time_integer(int t);
extern void  cptall_beginMonitor_string(char *message);
extern void  cptall_endMonitor_void();
extern void  cptall_restartMonitor_string(char *message);
extern void  cptall_creset_Counter(cptall_Counter *c);
extern void  cptall_cfix_Counter(cptall_Counter *c);
extern int  cptall_cdiff_Counter(cptall_Counter *c);
extern int  cptall_cget_Counter(cptall_Counter *c);
extern int  cptall_cold_Counter(cptall_Counter *c);
extern void  cptall_cset_Counter(cptall_Counter *c,int n);
extern void  cptall_cinc_Counter1(cptall_Counter *c,int n);
extern void  cptall_cinc_Counter2(cptall_Counter *c);
extern cptall_BoolArray * cptall_makeBoolArray_integer(int nb);
extern void  cptall_setTrue_BoolArray(cptall_BoolArray *t,int i);
extern ClaireBoolean * cptall_isTrue_BoolArray(cptall_BoolArray *t,int i);
extern cptall_Session * cptall_makeSession_list(list *params);
extern void  cptall_usage_void();
extern void  claire_self_print_Fluent_cptall(cptall_Fluent *f);
extern void  cptall_complete_print_Fluent(cptall_Fluent *f);
extern int  cptall_getPairCost_Fluent(cptall_Fluent *f1,cptall_Fluent *f2);
extern void  cptall_setPairCost_Fluent(cptall_Fluent *f1,cptall_Fluent *f2,int c);
extern void  cptall_closeFluentParser_list(list *fluents,cptall_Fluent *f);
extern void  cptall_closeFluent_PlanningProblem(cptall_PlanningProblem *pb,cptall_Fluent *f);
extern void  claire_self_print_Action_cptall(cptall_Action *a);
extern char * cptall_action_to_string_Action(cptall_Action *a);
extern void  cptall_complete_print_Action(cptall_Action *a);
extern void  cptall_setUsed_Action(cptall_Action *a);
extern void  cptall_setExcluded_Action(cptall_Action *a);
extern ClaireBoolean * cptall_isUsed_Action(cptall_Action *a);
extern ClaireBoolean * cptall_isExcluded_Action(cptall_Action *a);
extern void  cptall_setMutex_Action(cptall_Action *a,cptall_Action *a_prime);
extern ClaireBoolean * cptall_isMutex_Action(cptall_Action *a,cptall_Action *a_prime);
extern void  cptall_setConsumes_Action(cptall_Action *a,cptall_Fluent *f);
extern void  cptall_setProduces_Action(cptall_Action *a,cptall_Fluent *f);
extern void  cptall_setDeletes_Action(cptall_Action *a,cptall_Fluent *f);
extern ClaireBoolean * cptall_consumes_Action(cptall_Action *a,cptall_Fluent *f);
extern ClaireBoolean * cptall_produces_Action(cptall_Action *a,cptall_Fluent *f);
extern ClaireBoolean * cptall_deletes_Action(cptall_Action *a,cptall_Fluent *f);
extern void  cptall_setDistance_Action(cptall_Action *a,cptall_Action *a_prime,int d);
extern int  cptall_getDistance_Action1(cptall_Action *a,cptall_Action *a_prime);
extern int  cptall_getDistance_CausalLink(cptall_CausalLink *c,cptall_Action *a);
extern int  cptall_getDistance_Action2(cptall_Action *a,cptall_CausalLink *c);
extern int  cptall_firstStart_Action(cptall_Action *a);
extern int  cptall_lastStart_Action(cptall_Action *a);
extern int  cptall_getDuration_Action(cptall_Action *a);
extern int  cptall_firstEnd_Action(cptall_Action *a);
extern int  cptall_lastEnd_Action(cptall_Action *a);
extern void  cptall_closeActionParser_list(list *actions,list *fluents,cptall_Action *a);
extern void  cptall_closeAction_PlanningProblem(cptall_PlanningProblem *pb,cptall_Action *a);
extern cptall_Action * cptall_cloneAction_Action(cptall_Action *a,cptall_CausalLink *causal);
extern void  cptall_addDomainVal_LinkedListIntDomain(cptall_LinkedListIntDomain *d,int x,int s);
extern void  cptall_computeActionsMutex_Action(cptall_Action *a);
extern void  cptall_updateInfA_Action(cptall_Action *a,int val);
extern void  cptall_updateSupA_Action(cptall_Action *a,int val);
extern void  cptall_exclude_Action(cptall_Action *a);
extern void  claire_self_print_CausalLink_cptall(cptall_CausalLink *c);
extern int  cptall_firstStart_CausalLink(cptall_CausalLink *c);
extern int  cptall_lastStart_CausalLink(cptall_CausalLink *c);
extern int  cptall_getProducerNum_integer(int i,cptall_CausalLink *c);
extern int  cptall_getProducerNum_Action(cptall_Action *a,cptall_CausalLink *c);
extern int  cptall_getProducerNum_CausalLink(cptall_CausalLink *c);
extern void  cptall_setProducer_CausalLink(cptall_CausalLink *c,cptall_Action *a);
extern void  cptall_remProducer_CausalLink(cptall_CausalLink *c,cptall_Action *a);
extern ClaireBoolean * cptall_canProduce_Action(cptall_Action *a,cptall_CausalLink *c);
extern OID  cptall_getProducer_CausalLink(cptall_CausalLink *c);
extern ClaireBoolean * cptall_isProducer_Action(cptall_Action *a,cptall_CausalLink *c);
extern list * cptall_getProducers_CausalLink(cptall_CausalLink *c);
extern list * cptall_getThreats_CausalLink(cptall_CausalLink *c);
extern void  cptall_setExcluded_CausalLink(cptall_CausalLink *c);
extern void  cptall_setRequired_CausalLink(cptall_CausalLink *c);
extern ClaireBoolean * cptall_isRequired_CausalLink(cptall_CausalLink *c);
extern ClaireBoolean * cptall_isExcluded_CausalLink(cptall_CausalLink *c);
extern ClaireBoolean * cptall_isPossible_CausalLink(cptall_CausalLink *c);
extern cptall_CausalLink * cptall_makeCausalLink_PlanningProblem(cptall_PlanningProblem *pb,cptall_Fluent *f,cptall_Action *a);
extern void  cptall_closeCausalLink_PlanningProblem(cptall_PlanningProblem *pb,cptall_CausalLink *c);
extern cptall_CausalLink * cptall_cloneCausalLink_CausalLink(cptall_CausalLink *c,cptall_Action *a);
extern void  cptall_updateInfC_CausalLink(cptall_CausalLink *c,int val);
extern void  cptall_updateSupC_CausalLink(cptall_CausalLink *c,int val);
extern ClaireBoolean * cptall_alwaysPrecede_Action1(cptall_Action *a,cptall_Action *a_prime);
extern ClaireBoolean * cptall_alwaysPrecede_CausalLink(cptall_CausalLink *c,cptall_Action *a);
extern ClaireBoolean * cptall_alwaysPrecede_Action2(cptall_Action *a,cptall_CausalLink *c);
extern ClaireBoolean * cptall_cannotPrecede_Action1(cptall_Action *a,cptall_Action *a_prime);
extern ClaireBoolean * cptall_cannotPrecede_CausalLink(cptall_CausalLink *c,cptall_Action *a);
extern ClaireBoolean * cptall_cannotPrecede_Action2(cptall_Action *a,cptall_CausalLink *c);
extern ClaireBoolean * cptall_canPrecede_Action1(cptall_Action *a,cptall_Action *a_prime);
extern ClaireBoolean * cptall_canPrecede_CausalLink(cptall_CausalLink *c,cptall_Action *a);
extern ClaireBoolean * cptall_canPrecede_Action2(cptall_Action *a,cptall_CausalLink *c);
extern ClaireBoolean * cptall_canPrecedeWeak_Action1(cptall_Action *a,cptall_Action *a_prime);
extern ClaireBoolean * cptall_canPrecedeWeak_CausalLink(cptall_CausalLink *c,cptall_Action *a);
extern ClaireBoolean * cptall_canPrecedeWeak_Action2(cptall_Action *a,cptall_CausalLink *c);
extern int  cptall_slack_Action1(cptall_Action *a,cptall_CausalLink *c);
extern int  cptall_slack_CausalLink(cptall_CausalLink *c,cptall_Action *a);
extern int  cptall_slack_Action2(cptall_Action *a1,cptall_Action *a2);
extern void  cptall_computeH1Distances_PlanningProblem(cptall_PlanningProblem *pb);
extern void  cptall_computeH1Cost_list(list *actions,int start);
extern void  cptall_updateCost_Fluent1(cptall_Fluent *f,int cost);
extern void  cptall_computeH2Distances_PlanningProblem(cptall_PlanningProblem *pb);
extern void  cptall_computeInitH0Cost_list(list *actions,list *fluents);
extern void  cptall_computeInitH1Cost_list(list *actions,list *fluents);
extern void  cptall_computeInitH2Cost_list(list *actions,list *fluents);
extern void  cptall_computeH2Cost_list(list *actions,list *fluents,int start);
extern void  cptall_updateCost_Fluent2(cptall_Fluent *f1,cptall_Fluent *f2,int cost);
extern void  cptall_writeDistances_PlanningProblem(cptall_PlanningProblem *pb);
extern void  cptall_readDistances_PlanningProblem(cptall_PlanningProblem *pb);
extern cptall_PlanningProblem * cptall_makePlanningProblem_Session(cptall_Session *s);
extern void  cptall_propagate_PlanningProblem_cptall(cptall_PlanningProblem *pb);
extern void  claire_self_print_Symbol_cptall(cptall_Symbol *s);
extern void  claire_self_print_tuple2_cptall(tuple *att);
extern void  claire_self_print_set2_cptall(set *s);
extern tuple * cptall_createAttributeSpace_Predicate(cptall_Predicate *p,int n);
extern tuple * cptall_createAttributeSpace_Predicate_(cptall_Predicate *g0566,int g0567);
extern void  claire_self_print_TransitionRule_cptall(cptall_TransitionRule *t);
extern void  cptall_addConstant_Predicate(cptall_Predicate *p,int n,cptall_Constant *c);
extern cptall_Predicate * cptall_createType_string(char *s);
extern cptall_Predicate * cptall_createPredicate_string1(char *s);
extern cptall_Predicate * cptall_createPredicate_string2(char *s,int i);
extern cptall_Variabl * cptall_createVariable_string(char *rs);
extern cptall_Constant * cptall_createConstant_string(char *s);
extern cptall_Term * cptall_createTerm_string(char *s);
extern void  claire_self_print_PositiveAtom_cptall(cptall_PositiveAtom *a);
extern void  claire_self_print_NegativeAtom_cptall(cptall_NegativeAtom *a);
extern cptall_Atom * cptall_createAtom_string(char *n);
extern cptall_Atom * cptall_createAtom_Predicate(cptall_Predicate *p,list *t,ClaireBoolean *sign,OID init);
extern void  claire_self_print_Operator_cptall(cptall_Operator *o);
extern cptall_Operator * cptall_createOperator_string(char *s);
extern void  claire_self_print_Domain_cptall(cptall_Domain *d);
extern void  claire_self_print_Instance_cptall(cptall_Instance *p);
extern void  cptall_openFile_string(char *s);
extern void  cptall_closeFile_void();
extern void  cptall_parseError_void();
extern void  cptall_readChar_void();
extern char * cptall_readNext_void();
extern char * cptall_currentWord_void();
extern char * cptall_nextWord_void();
extern void  cptall_checkWord_string(char *s,char *w);
extern void  cptall_checkNext_string(char *s);
extern ClaireBoolean * cptall_testNext_string(char *s);
extern void  cptall_readBegin_void();
extern void  cptall_readEnd_void();
extern ClaireBoolean * cptall_testBegin_void();
extern ClaireBoolean * cptall_testEnd_void();
extern cptall_Domain * cptall_readDomain_void();
extern cptall_Instance * cptall_readInstance_Domain(cptall_Domain *d);
extern void  cptall_readDomainInstance_void();
extern void  cptall_readObjects_void();
extern void  cptall_readInitState_Instance(cptall_Instance *pb);
extern void  cptall_readGoal_Instance(cptall_Instance *pb);
extern void  cptall_readRequirements_Domain(cptall_Domain *d);
extern void  cptall_readTypes_Domain(cptall_Domain *d);
extern void  cptall_readConstants_void();
extern void  cptall_readPredicates_void();
extern void  cptall_readFunctions_void();
extern cptall_Operator * cptall_readOperator_Domain(cptall_Domain *d);
extern void  cptall_readParameters_Operator(cptall_Operator *o);
extern void  cptall_readDuration_Operator(cptall_Operator *o);
extern void  cptall_readPrecondition_Domain(cptall_Domain *d,cptall_Operator *o);
extern void  cptall_readEffect_Domain(cptall_Domain *d,cptall_Operator *o);
extern tuple * cptall_readConjunction_boolean(ClaireBoolean *timed);
extern tuple * cptall_readConjunction_boolean_(ClaireBoolean *g0596);
extern tuple * cptall_readAtomList_boolean(ClaireBoolean *timed);
extern tuple * cptall_readAtomList_boolean_(ClaireBoolean *g0599);
extern void  cptall_addAtomToList_Atom(cptall_Atom *a,list *l);
extern void  cptall_addAtomToList_PositiveAtom(cptall_PositiveAtom *a,list *pos,list *neg);
extern void  cptall_addAtomToList_NegativeAtom(cptall_NegativeAtom *a,list *pos,list *neg);
extern OID  cptall_readAtom_boolean(ClaireBoolean *timed);
extern cptall_Rational * cptall_readRational_void();
extern cptall_Rational * cptall_stringToRational_string(char *s);
extern ephemeral_object * cptall_readExpression_void();
extern list * cptall_readOperationTerms_void();
extern list * cptall_readTermList_void();
extern void  cptall_createAttributeSpaces_Domain(cptall_Domain *d);
extern cptall_Fluent * cptall_createFluent_Instance1(cptall_Instance *pb,cptall_Atom *a);
extern cptall_Fluent * cptall_createFluent_Instance2(cptall_Instance *pb,cptall_Atom *a,list *c);
extern cptall_Fluent * cptall_createFluent_Instance3(cptall_Instance *pb,cptall_Predicate *p,list *t);
extern cptall_Fluent * cptall_createFluent_Predicate(cptall_Predicate *p,list *c);
extern void  cptall_instantiateOperator_Instance(cptall_Instance *pb,cptall_Operator *o,int i,list *params);
extern void  cptall_instantiateOperators_Instance(cptall_Instance *pb);
extern void  cptall_addEvent_Instance1(cptall_Instance *pb,cptall_NegativeAtom *a);
extern void  cptall_addEvent_Instance2(cptall_Instance *pb,cptall_PositiveAtom *a);
extern void  cptall_addEdels_Action(cptall_Action *a,cptall_Fluent *f,list *fluents);
extern void  cptall_computeReachability_Action(cptall_Action *a);
extern void  cptall_computeReachability_Fluent(cptall_Fluent *f);
extern void  cptall_computeDurations_Instance(cptall_Instance *pb);
extern cptall_Instance * cptall_readProblem_Session(cptall_Session *s);
extern cptall_SupportConstraint * cptall_supportConstraint_CausalLink(cptall_CausalLink *c);
extern void  cptall_awake_SupportConstraint_cptall(cptall_SupportConstraint *sc);
extern void  cptall_awakeOnInst_SupportConstraint_cptall(cptall_SupportConstraint *sc,int idx);
extern void  cptall_awakeOnRem_SupportConstraint_cptall(cptall_SupportConstraint *sc,int idx,int v);
extern void  cptall_propagate_SupportConstraint_cptall(cptall_SupportConstraint *sc);
extern cptall_ActionConstraint * cptall_actionConstraint_Action(cptall_Action *a);
extern void  cptall_awake_ActionConstraint_cptall(cptall_ActionConstraint *ac);
extern void  cptall_propagate_ActionConstraint_cptall(cptall_ActionConstraint *ac);
extern void  cptall_synchronizeCausal_CausalLink(cptall_CausalLink *c);
extern void  cptall_synchronizeAction_Action(cptall_Action *a);
extern void  cptall_protectAgainst_Action(cptall_Action *a);
extern void  cptall_protectCausal_CausalLink(cptall_CausalLink *c);
extern void  cptall_use_Action(cptall_Action *a);
extern void  cptall_makeOrder_Action(cptall_Action *a,cptall_IntVar *c);
extern void  cptall_orderBefore_Action1(cptall_Action *a,cptall_Action *a_prime);
extern void  cptall_orderBefore2_Action(cptall_Action *a,cptall_Action *a_prime);
extern void  cptall_orderBefore_CausalLink(cptall_CausalLink *c,cptall_Action *a);
extern void  cptall_orderBefore_Action2(cptall_Action *a,cptall_CausalLink *c);
extern int  cptall_dist_Action(cptall_Action *a,cptall_Action *a_prime);
extern int  cptall_minDistance_Action(cptall_Action *a,cptall_Action *a_prime);
extern void  cptall_computeGlobalMutexSets_Action(cptall_Action *a);
extern ClaireBoolean * cptall_makespan_PlanningProblem(cptall_PlanningProblem *pb);
extern void  cptall_computeLocalMutexSets_CausalLink(cptall_CausalLink *c);
extern void  cptall_addMutexSet_list(list *l,cptall_Action *a);
extern int  cptall_mindist_Action(cptall_Action *a,list *actions);
extern void  cptall_makespanAfter_CausalLink(cptall_CausalLink *c,list *after,list *middle);
extern void  cptall_makespanBefore_CausalLink(cptall_CausalLink *c,list *before,list *middle);
extern void  claire_self_print_PrecedenceConstraint_cptall(cptall_PrecedenceConstraint *c);
extern cptall_PrecedenceConstraint * claire__inf_inf_Action1(cptall_Action *a,cptall_Action *a_prime);
extern void  cptall_propagate_PrecedenceConstraint_cptall(cptall_PrecedenceConstraint *c);
extern void  cptall_awake_PrecedenceConstraint_cptall(cptall_PrecedenceConstraint *c);
extern void  cptall_awakeOnInf_PrecedenceConstraint_cptall(cptall_PrecedenceConstraint *c,int idx);
extern void  cptall_awakeOnSup_PrecedenceConstraint_cptall(cptall_PrecedenceConstraint *c,int idx);
extern void  cptall_awakeOnInst_PrecedenceConstraint_cptall(cptall_PrecedenceConstraint *c,int idx);
extern OID  cptall_askIfEntailed_PrecedenceConstraint(cptall_PrecedenceConstraint *c);
extern void  claire_self_print_CausalPrecedenceConstraint_cptall(cptall_CausalPrecedenceConstraint *c);
extern cptall_CausalPrecedenceConstraint * claire__inf_inf_Action2(cptall_Action *a,cptall_CausalLink *c);
extern void  cptall_awake_CausalPrecedenceConstraint_cptall(cptall_CausalPrecedenceConstraint *c);
extern void  cptall_awakeOnInf_CausalPrecedenceConstraint_cptall(cptall_CausalPrecedenceConstraint *c,int idx);
extern void  cptall_awakeOnSup_CausalPrecedenceConstraint_cptall(cptall_CausalPrecedenceConstraint *c,int idx);
extern void  cptall_awakeOnInst_CausalPrecedenceConstraint_cptall(cptall_CausalPrecedenceConstraint *c,int idx);
extern void  cptall_awakeOnRem_CausalPrecedenceConstraint_cptall(cptall_CausalPrecedenceConstraint *c,int idx,int v);
extern void  cptall_awakeOnVar_CausalPrecedenceConstraint_cptall(cptall_CausalPrecedenceConstraint *c,int idx);
extern OID  cptall_askIfEntailed_CausalPrecedenceConstraint(cptall_CausalPrecedenceConstraint *c);
extern void  cptall_quickPost_PlanningProblem(cptall_PlanningProblem *pb,cptall_AbstractConstraint *c);
extern cptall_InitTimeDomain * cptall_makeInitTimeHeuristic_PlanningProblem(cptall_PlanningProblem *pb);
extern OID  cptall_selectVar_InitTimeDomain(cptall_InitTimeDomain *vs);
extern cptall_ProducerDomain * cptall_makeProducerVarHeuristic_PlanningProblem(cptall_PlanningProblem *pb);
extern OID  cptall_selectVar_ProducerDomain(cptall_ProducerDomain *vs);
extern OID  cptall_bestAction_CausalLink(cptall_CausalLink *c);
extern cptall_ValSelector * cptall_makeBestProducerSelector_void();
extern int  cptall_getBestVal_BestProducerHeuristic(cptall_BestProducerHeuristic *vh,cptall_IntVar *c);
extern cptall_ConflictBranching * cptall_makeConflictBranching_SolveConflict(cptall_SolveConflict *b1,cptall_AbstractBranching *b2);
extern cptall_AbstractBranching * cptall_selectBranching_ConflictBranching_cptall(cptall_ConflictBranching *b);
extern ClaireBoolean * cptall_selectConflict_SolveConflict(cptall_SolveConflict *b);
extern cptall_SolveConflict * cptall_makeSolveConflict_void();
extern OID  cptall_selectBranchingObject_SolveConflict(cptall_SolveConflict *b);
extern void  cptall_goDownBranch_SolveConflict(cptall_SolveConflict *b,OID conflict,int first);
extern void  cptall_traceDownBranch_SolveConflict(cptall_SolveConflict *b,OID conflict,int first);
extern cptall_SolveMutex * cptall_makeSolveMutex_void();
extern OID  cptall_selectBranchingObject_SolveMutex(cptall_SolveMutex *b);
extern void  cptall_goDownBranch_SolveMutex(cptall_SolveMutex *b,OID mutex,int first);
extern void  cptall_traceDownBranch_SolveMutex(cptall_SolveMutex *b,OID conflict,int first);
extern void  cptall_traceProducerUse_CausalLink(cptall_CausalLink *c,int idx);
extern void  cptall_traceProducerExclude_CausalLink(cptall_CausalLink *c,int val);
extern void  cptall_tracePrecedenceAwake_PrecedenceConstraint(cptall_PrecedenceConstraint *c);
extern void  cptall_tracePrecedenceAwake_CausalPrecedenceConstraint(cptall_CausalPrecedenceConstraint *c);
extern void  cptall_tracePopWorld_PlanningProblem(cptall_PlanningProblem *pb);
extern void  cptall_tracePushWorld_PlanningProblem(cptall_PlanningProblem *pb);
extern void  cptall_traceSupportChoice_list(list *causals,OID c0);
extern void  cptall_traceConflictChoice_Conflict(cptall_Conflict *conflict);
extern void  cptall_traceActionUsed_Action(cptall_Action *a);
extern void  cptall_traceActionExcluded_Action(cptall_Action *a);
extern void  cptall_traceUpdateInf_Action(cptall_Action *a,int val);
extern void  cptall_traceUpdateSup_Action(cptall_Action *a,int val);
extern void  main_list(list *params);
extern void  cptall_rp_void();
extern void  cptall_runPlanner_Session(cptall_Session *s);
extern void  cptall_makeInitialConstraints_PlanningProblem(cptall_PlanningProblem *pb);
extern cptall_Solver * cptall_makePlanningSolver_PlanningProblem(cptall_PlanningProblem *pb);
extern void  cptall_solve_PlanningProblem(cptall_PlanningProblem *pb);
extern void  cptall_computeRelevance_PlanningProblem(cptall_PlanningProblem *pb,int bnd);
extern void  cptall_computeActionRelevance_Action(cptall_Action *a,int bnd);
extern void  cptall_printPlan_PlanningProblem(cptall_PlanningProblem *pb);
extern ClaireBoolean * cptall_orderActionsPlan_Action(cptall_Action *a1,cptall_Action *a2);
extern char * cptall_convert_PlanningProblem(cptall_PlanningProblem *pb,int n);

// namespace class for cptall 
class cptallClass: public NameSpace {
public:

global_variable * MAXINT;
global_variable * MININT;
global_variable * UNKNOWNINT;
ClaireClass * _Util;
ClaireClass * _StoredInt;
global_variable * CHOCO_RELEASE;
global_variable * EXTENDABLE_CHOCO;
global_variable * DDEBUG;
global_variable * PVIEW;
global_variable * PTALK;
global_variable * PDEBUG;
global_variable * GPVIEW;
global_variable * GPTALK;
global_variable * GPDEBUG;
global_variable * SVIEW;
global_variable * STALK;
global_variable * SDEBUG;
global_variable * IVIEW;
global_variable * ITALK;
global_variable * IDEBUG;
global_variable * LVIEW;
global_variable * LTALK;
global_variable * LDEBUG;
global_variable * CHOCOBENCH_VIEW;
global_variable * CHOCOTEST_VIEW;
ClaireClass * _Ephemeral;
ClaireClass * _Problem;
ClaireClass * _Solver;
ClaireClass * _LocalSearchSolver;
ClaireClass * _GlobalSearchSolver;
ClaireClass * _AbstractConstraint;
ClaireClass * _IntConstraint;
ClaireClass * _AbstractVar;
ClaireClass * _IntVar;
ClaireClass * _PropagationEvent;
ClaireClass * _ConstAwakeEvent;
ClaireClass * _VarEvent;
ClaireClass * _Instantiation;
ClaireClass * _InstInt;
ClaireClass * _ValueRemovals;
ClaireClass * _BoundUpdate;
ClaireClass * _IncInf;
ClaireClass * _DecSup;
ClaireClass * _LogicEngine;
ClaireClass * _PropagationEngine;
ClaireClass * _InvariantEngine;
ClaireClass * _AbstractBranching;
ClaireClass * _GlobalSearchLimit;
ClaireClass * _ConstructiveHeuristic;
ClaireClass * _MoveNeighborhood;
ClaireClass * _Solution;
ClaireClass * _DummyConstraint;
global_variable * dummy_constraint;
global_variable * CURRENT_PB;
property * setActiveProblem;
property * getActiveProblem;
ClaireClass * _ConflictCount;
ClaireClass * _CounterLimit;
ClaireClass * _NodeLimit;
ClaireClass * _BacktrackLimit;
ClaireClass * _TimeLimit;
ClaireClass * _TopologyLimit;
ClaireClass * _DiscLimit;
ClaireClass * _AbstractDomain;
ClaireClass * _AbstractIntDomain;
property * containsValInDomain;
property * remove;
property * restrict;
ClaireClass * _AbstractSetDomain;
property * getDomainCard;
property * getNextValue;
property * getPrevValue;
property * removeDomainVal;
property * domainSequence;
property * domainSet;
global_variable * DINF;
global_variable * DSUP;
ClaireClass * _LinkedListIntDomain;
ClaireClass * _BipartiteSet;
ClaireClass * _EventCollection;
ClaireClass * _EventQueue;
ClaireClass * _BoundEventQueue;
ClaireClass * _RemovalEventQueue;
ClaireClass * _InstantiationStack;
ClaireClass * _ConstAwakeEventQueue;
ClaireClass * _ChocEngine;
property * raiseOverflowWarning;
property * getNextActiveEventQueue;
property * propagateEvent;
property * isInstantiated;
property * getInf;
property * getSup;
operation * isInstantiatedTo;
operation * canBeInstantiatedTo;
operation * canBeEqualTo;
operation * domainIncludedIn;
operation * canBeInstantiatedIn;
ClaireClass * _UnIntConstraint;
ClaireClass * _BinIntConstraint;
ClaireClass * _TernIntConstraint;
ClaireClass * _LargeIntConstraint;
ClaireClass * _CompositeConstraint;
ClaireClass * _BinCompositeConstraint;
ClaireClass * _LargeCompositeConstraint;
property * getNbVars;
property * connect;
property * connectIntVar;
property * connectIntVarEvents;
property * setConstraintIndex;
property * doPropagate;
property * doAwake;
property * doAwakeOnVar;
property * askIfTrue;
property * doAwakeOnInf;
property * doAwakeOnSup;
property * doAwakeOnInst;
property * doAwakeOnRem;
property * testIfTrue;
property * testIfInstantiated;
property * getVar;
property * propagate;
property * awake;
property * awakeOnInf;
property * awakeOnSup;
property * awakeOnInst;
property * awakeOnRem;
property * awakeOnVar;
property * askIfEntailed;
property * testIfSatisfied;
property * testIfCompletelyInstantiated;
property * selectBranchingObject;
property * goDownBranch;
property * traceDownBranch;
property * getFirstBranch;
property * getNextBranch;
property * goUpBranch;
property * traceUpBranch;
property * finishedBranching;
property * branchOn;
ClaireClass * _LargeBranching;
ClaireClass * _BinBranching;
ClaireClass * _CompositeBranching;
ClaireClass * _VarSelector;
ClaireClass * _MinDomain;
ClaireClass * _MaxDeg;
ClaireClass * _DomDeg;
ClaireClass * _StaticVarOrder;
ClaireClass * _ValIterator;
ClaireClass * _IncreasingDomain;
ClaireClass * _DecreasingDomain;
ClaireClass * _ValSelector;
ClaireClass * _MidValHeuristic;
ClaireClass * _MinValHeuristic;
ClaireClass * _MaxValHeuristic;
ClaireClass * _AssignVar;
ClaireClass * _SplitDomain;
ClaireClass * _AssignOrForbid;
ClaireClass * _Solve;
property * newNode;
property * endNode;
property * reset;
property * explore;
ClaireClass * _Atom;
ClaireClass * _Symbol;
ClaireClass * _Term;
ClaireClass * _Rational;
cptall_Rational * zero;
ClaireClass * _OperationTerms;
ClaireClass * _Addition;
ClaireClass * _Substraction;
ClaireClass * _Multiplication;
ClaireClass * _Division;
Union * Expression;
ClaireBoolean * VERB;
ClaireBoolean * MEMORY_VERIF;
ClaireBoolean * PRETTY;
int  SUPPORT_BRANCHING;
int  CONFLICT_BRANCHING;
list * resolution_strategies;
int  H0;
int  H1;
int  H2;
int  NO_DIC;
int  MIN_DIC;
int  DIFF_DIC;
int  MAX_DIC;
int  ACTUAL_CONF;
int  EXTENDED_CONF;
int  POSSIBLE_CONF;
int  POP_WORLD;
int  PUSH_WORLD;
int  DECO;
int  SUPPORT_CHOICE;
int  PRODUCER_CHOICE;
int  CONFLICT_CHOICE;
int  PRODUCER_USE;
int  PRODUCER_EXCLUDE;
int  ACTION_PRECEDENCE;
int  CAUSAL_PRECEDENCE;
int  CONFLICT;
int  UPDATE_INF;
int  UPDATE_SUP;
char * BUFFER;
global_variable * endl;
ClaireChar * begl;
ClaireClass * _Counter;
ClaireClass * _BoolArray;
ClaireClass * _Session;
ClaireClass * _Predicate;
ClaireClass * _Variabl;
ClaireClass * _Constant;
ClaireClass * _Fluent;
ClaireClass * _Action;
ClaireClass * _CausalLink;
ClaireClass * _PlanningProblem;
operation * alwaysPrecede;
operation * cannotPrecede;
operation * canPrecede;
operation * canPrecedeWeak;
ClaireClass * _Instance;
ClaireClass * _TransitionRule;
tuple * AttributeSpace;
Param * AttributeSpaceSet;
global_variable * attribute_spaces;
table * attribute_spaces_table;
table * constants_table;
int  cpt_var;
table * predicate_table;
table * variable_table;
table * constant_table;
ClaireClass * _PositiveAtom;
ClaireClass * _NegativeAtom;
ClaireClass * _Operator;
global_variable * typed_constants;
ClaireClass * _Domain;
global_variable * buffer;
global_variable * current_char;
global_variable * current_word;
global_variable * next_word;
global_variable * parsed_file;
global_variable * endl2;
global_variable * lparen;
global_variable * rparen;
global_variable * comt;
global_variable * space;
global_variable * tabul;
global_variable * diff_upcase;
ClaireChar * eof;
table * functions_table;
table * fluent_table;
table * constraints_table;
ClaireClass * _SupportConstraint;
ClaireClass * _ActionConstraint;
ClaireClass * _PrecedenceConstraint;
ClaireClass * _CausalPrecedenceConstraint;
ClaireClass * _InitTimeDomain;
ClaireClass * _ProducerDomain;
ClaireClass * _BestProducerHeuristic;
ClaireClass * _SolveConflict;
ClaireClass * _ConflictBranching;
ClaireClass * _Conflict;
ClaireClass * _Mutex;
ClaireClass * _SolveMutex;
global_variable * SESSION;
property * sum;// claire/"sum"
property * product;// claire/"product"
property * count;// claire/"count"
property * knownInt;// cptall/"knownInt"
property * latestValue;// claire/"latestValue"
property * latestUpdate;// claire/"latestUpdate"
property * make_simple_bklist;// cptall/"make_simple_bklist"
property * showChocoLicense;// cptall/"showChocoLicense"
property * algo;// cptall/"algo"
property * time;// cptall/"time"
property * nbBk;// cptall/"nbBk"
property * nbNd;// cptall/"nbNd"
property * objectiveValue;// cptall/"objectiveValue"
property * lval;// cptall/"lval"
property * makeSolution;// cptall/"makeSolution"
property * constraints;// cptall/"constraints"
property * feasible;// cptall/"feasible"
property * solved;// cptall/"solved"
property * feasibleMode;// cptall/"feasibleMode"
property * propagationEngine;// cptall/"propagationEngine"
property * globalSearchSolver;// cptall/"globalSearchSolver"
property * invariantEngine;// cptall/"invariantEngine"
property * localSearchSolver;// cptall/"localSearchSolver"
property * nbConstProblem;// cptall/"nbConstProblem"
property * nbConstVar;// cptall/"nbConstVar"
property * getIntVar;// cptall/"getIntVar"
property * problem;// cptall/"problem"
property * maxSize;// cptall/"maxSize"
property * propagationOverflow;// cptall/"propagationOverflow"
property * contradictionCause;// cptall/"contradictionCause"
property * nbViolatedConstraints;// cptall/"nbViolatedConstraints"
property * minNbViolatedConstraints;// cptall/"minNbViolatedConstraints"
property * assignedThenUnassignedVars;// cptall/"assignedThenUnassignedVars"
property * lastAssignedVar;// cptall/"lastAssignedVar"
property * conflictingVars;// cptall/"conflictingVars"
property * solutions;// cptall/"solutions"
property * varsToStore;// cptall/"varsToStore"
property * varsToShow;// cptall/"varsToShow"
property * searchSolver;// cptall/"searchSolver"
property * unit;// cptall/"unit"
property * maxNb;// cptall/"maxNb"
property * totNb;// cptall/"totNb"
property * nb;// cptall/"nb"
property * branchCounter;// cptall/"branchCounter"
property * maxNbDisc;// cptall/"maxNbDisc"
property * stopAtFirstSol;// cptall/"stopAtFirstSol"
property * nbSol;// cptall/"nbSol"
property * maxNbBk;// cptall/"maxNbBk"
property * maxPrintDepth;// cptall/"maxPrintDepth"
property * maxSolutionStorage;// cptall/"maxSolutionStorage"
property * branchingComponents;// cptall/"branchingComponents"
property * baseWorld;// cptall/"baseWorld"
property * limits;// cptall/"limits"
property * manager;// cptall/"manager"
property * nextBranching;// cptall/"nextBranching"
property * rootBranching;// cptall/"rootBranching"
property * getDomainInf;// cptall/"getDomainInf"
property * getDomainSup;// cptall/"getDomainSup"
property * updateDomainInf;// cptall/"updateDomainInf"
property * updateDomainSup;// cptall/"updateDomainSup"
property * offset;// cptall/"offset"
property * bucketSize;// cptall/"bucketSize"
property * contents;// cptall/"contents"
property * makeLinkedListIntDomain;// cptall/"makeLinkedListIntDomain"
property * isIncludedIn;// cptall/"isIncludedIn"
property * modifiedVar;// cptall/"modifiedVar"
property * nextConst;// cptall/"nextConst"
property * cause;// cptall/"cause"
property * idxInQueue;// cptall/"idxInQueue"
property * inf;// cptall/"inf"
property * sup;// cptall/"sup"
property * maxVals;// cptall/"maxVals"
property * nbVals;// cptall/"nbVals"
property * many;// cptall/"many"
property * valueStack;// cptall/"valueStack"
property * causeStack;// cptall/"causeStack"
property * touchedConst;// cptall/"touchedConst"
property * initialized;// cptall/"initialized"
property * event_idx;// cptall/"event_idx"
property * objs;// cptall/"objs"
property * nbLeft;// cptall/"nbLeft"
property * makeBipartiteSet;// cptall/"makeBipartiteSet"
property * swap;// cptall/"swap"
property * moveLeft;// cptall/"moveLeft"
property * moveRight;// cptall/"moveRight"
property * moveAllLeft;// cptall/"moveAllLeft"
property * moveAllRight;// cptall/"moveAllRight"
property * addRight;// cptall/"addRight"
property * addLeft;// cptall/"addLeft"
property * isLeft;// cptall/"isLeft"
property * isIn;// cptall/"isIn"
property * getNbLeft;// cptall/"getNbLeft"
property * getNbRight;// cptall/"getNbRight"
property * getNbObjects;// cptall/"getNbObjects"
property * getObject;// cptall/"getObject"
property * leftPart;// cptall/"leftPart"
property * rightPart;// cptall/"rightPart"
property * engine;// cptall/"engine"
property * qsize;// cptall/"qsize"
property * qLastRead;// cptall/"qLastRead"
property * qLastEnqueued;// cptall/"qLastEnqueued"
property * isPopping;// cptall/"isPopping"
property * eventQueue;// cptall/"eventQueue"
property * redundantEvent;// cptall/"redundantEvent"
property * sLastRead;// cptall/"sLastRead"
property * sLastPushed;// cptall/"sLastPushed"
property * partition;// cptall/"partition"
property * removalEvtQueue;// cptall/"removalEvtQueue"
property * boundEvtQueue;// cptall/"boundEvtQueue"
property * instEvtStack;// cptall/"instEvtStack"
property * delayedConst;// cptall/"delayedConst"
property * nbPendingInitConstAwakeEvent;// cptall/"nbPendingInitConstAwakeEvent"
property * nbPendingVarEvent;// cptall/"nbPendingVarEvent"
property * raiseContradiction;// cptall/"raiseContradiction"
property * flushCurrentOpenEvents;// cptall/"flushCurrentOpenEvents"
property * getContradictionCause;// cptall/"getContradictionCause"
property * getProblem;// cptall/"getProblem"
property * makeChocEngine;// cptall/"makeChocEngine"
property * attachPropagationEngine;// cptall/"attachPropagationEngine"
property * isEmpty;// cptall/"isEmpty"
property * popNextEvent;// cptall/"popNextEvent"
property * nextEventPostIndex;// cptall/"nextEventPostIndex"
property * flushEventQueue;// cptall/"flushEventQueue"
property * updtInfEvt;// cptall/"updtInfEvt"
property * updtSupEvt;// cptall/"updtSupEvt"
property * remValEvt;// cptall/"remValEvt"
property * checkCleanState;// cptall/"checkCleanState"
property * pushWorld;// cptall/"pushWorld"
property * popWorld;// cptall/"popWorld"
property * setWorld;// cptall/"setWorld"
property * commitWorld;// cptall/"commitWorld"
property * postInstInt;// cptall/"postInstInt"
property * postRemoveVal;// cptall/"postRemoveVal"
property * postUpdateInf;// cptall/"postUpdateInf"
property * postUpdateSup;// cptall/"postUpdateSup"
property * postConstAwake;// cptall/"postConstAwake"
property * postInstantiateEvt;// cptall/"postInstantiateEvt"
property * instantiateEvt;// cptall/"instantiateEvt"
property * postBoundEvent;// cptall/"postBoundEvent"
property * getQueue;// cptall/"getQueue"
property * registerEvent;// cptall/"registerEvent"
property * constAwakeEvent;// cptall/"constAwakeEvent"
property * constAwake;// cptall/"constAwake"
property * popSomeEvents;// cptall/"popSomeEvents"
property * nbConstraints;// cptall/"nbConstraints"
property * indices;// cptall/"indices"
property * getNextActiveConstraintEventQueue;// cptall/"getNextActiveConstraintEventQueue"
property * getNextActiveVariableEventQueue;// cptall/"getNextActiveVariableEventQueue"
property * getNextActiveEventQueue2;// cptall/"getNextActiveEventQueue2"
property * getConstraint;// cptall/"getConstraint"
property * getDegree;// cptall/"getDegree"
property * savedAssignment;// cptall/"savedAssignment"
property * bucket;// cptall/"bucket"
property * closeIntVar;// cptall/"closeIntVar"
property * updateInf;// cptall/"updateInf"
property * updateSup;// cptall/"updateSup"
property * instantiate;// cptall/"instantiate"
property * removeVal;// cptall/"removeVal"
property * hasExactDomain;// cptall/"hasExactDomain"
property * getNextDomainValue;// cptall/"getNextDomainValue"
property * getPrevDomainValue;// cptall/"getPrevDomainValue"
property * getValue;// cptall/"getValue"
property * getDomainSize;// cptall/"getDomainSize"
property * removeInterval;// cptall/"removeInterval"
property * restrictTo;// cptall/"restrictTo"
property * cste;// cptall/"cste"
property * v1;// cptall/"v1"
property * idx1;// cptall/"idx1"
property * v2;// cptall/"v2"
property * idx2;// cptall/"idx2"
property * v3;// cptall/"v3"
property * idx3;// cptall/"idx3"
property * nbVars;// cptall/"nbVars"
property * closeLargeIntConstraint;// cptall/"closeLargeIntConstraint"
property * const1;// cptall/"const1"
property * const2;// cptall/"const2"
property * lconst;// cptall/"lconst"
property * loffset;// cptall/"loffset"
property * nbConst;// cptall/"nbConst"
property * additionalVars;// cptall/"additionalVars"
property * additionalIndices;// cptall/"additionalIndices"
property * closeCompositeConstraint;// cptall/"closeCompositeConstraint"
property * getNbVarsFromSubConst;// cptall/"getNbVarsFromSubConst"
property * assignIndices;// cptall/"assignIndices"
property * getConstraintIdx;// cptall/"getConstraintIdx"
property * opposite;// cptall/"opposite"
property * addIntVar;// cptall/"addIntVar"
property * connectEvent;// cptall/"connectEvent"
property * alternatives;// cptall/"alternatives"
property * selectBranching;// cptall/"selectBranching"
property * branching;// cptall/"branching"
property * selectVar;// cptall/"selectVar"
property * getSmallestDomainUnassignedVar;// cptall/"getSmallestDomainUnassignedVar"
property * getSearchManager;// cptall/"getSearchManager"
property * getMostConstrainedUnassignedVar;// cptall/"getMostConstrainedUnassignedVar"
property * getDomOverDegBestUnassignedVar;// cptall/"getDomOverDegBestUnassignedVar"
property * isOver;// cptall/"isOver"
property * getFirstVal;// cptall/"getFirstVal"
property * getNextVal;// cptall/"getNextVal"
property * getBestVal;// cptall/"getBestVal"
property * varHeuristic;// cptall/"varHeuristic"
property * valHeuristic;// cptall/"valHeuristic"
property * dichotomyThreshold;// cptall/"dichotomyThreshold"
property * getFeasibility;// cptall/"getFeasibility"
property * attach;// cptall/"attach"
property * composeGlobalSearch;// cptall/"composeGlobalSearch"
property * newTreeNode;// cptall/"newTreeNode"
property * endTreeNode;// cptall/"endTreeNode"
property * setMax;// cptall/"setMax"
property * setMin;// cptall/"setMin"
property * setVal;// cptall/"setVal"
property * remVal;// cptall/"remVal"
property * newTreeSearch;// cptall/"newTreeSearch"
property * endTreeSearch;// cptall/"endTreeSearch"
property * run;// cptall/"run"
property * discardProblem;// cptall/"discardProblem"
property * getPropagationEngine;// cptall/"getPropagationEngine"
property * makeBoundIntVar;// cptall/"makeBoundIntVar"
property * makeConstantIntVar;// cptall/"makeConstantIntVar"
property * makeIntVar;// cptall/"makeIntVar"
property * post;// cptall/"post"
property * makeMinDomVarHeuristic;// cptall/"makeMinDomVarHeuristic"
property * makeDomDegVarHeuristic;// cptall/"makeDomDegVarHeuristic"
property * makeMaxDegVarHeuristic;// cptall/"makeMaxDegVarHeuristic"
property * makeStaticVarHeuristic;// cptall/"makeStaticVarHeuristic"
property * makeIncValIterator;// cptall/"makeIncValIterator"
property * makeDecValIterator;// cptall/"makeDecValIterator"
property * makeMinValSelector;// cptall/"makeMinValSelector"
property * makeMaxValSelector;// cptall/"makeMaxValSelector"
property * makeMidValSelector;// cptall/"makeMidValSelector"
property * makeAssignVarBranching;// cptall/"makeAssignVarBranching"
property * makeSplitDomBranching;// cptall/"makeSplitDomBranching"
property * makeAssignOrForbidBranching;// cptall/"makeAssignOrForbidBranching"
property * makeDefaultBranchingList;// cptall/"makeDefaultBranchingList"
property * makeGlobalSearchSolver;// cptall/"makeGlobalSearchSolver"
property * setSearchLimit;// cptall/"setSearchLimit"
property * getNb;// cptall/"getNb"
property * getMaxNb;// cptall/"getMaxNb"
property * solve;// cptall/"solve"
property * makeDefaultLimits;// cptall/"makeDefaultLimits"
property * setMaxPrintDepth;// cptall/"setMaxPrintDepth"
property * setMaxSolutionStorage;// cptall/"setMaxSolutionStorage"
property * setVarsToShow;// cptall/"setVarsToShow"
property * getNbSol;// cptall/"getNbSol"
property * getGlobalSearchSolver;// cptall/"getGlobalSearchSolver"
property * makeNodeLimit;// cptall/"makeNodeLimit"
property * makeBacktrackLimit;// cptall/"makeBacktrackLimit"
property * makeTimeLimit;// cptall/"makeTimeLimit"
property * makeDiscLimit;// cptall/"makeDiscLimit"
property * setNodeLimit;// cptall/"setNodeLimit"
property * setTimeLimit;// cptall/"setTimeLimit"
property * setBacktrackLimit;// cptall/"setBacktrackLimit"
property * setDiscLimit;// cptall/"setDiscLimit"
property * num;// cptall/"num"
property * den;// cptall/"den"
property * terms;// cptall/"terms"
property * evaluateTerms;// cptall/"evaluateTerms"
property * evaluateExpr;// cptall/"evaluateExpr"
property * isvar;// cptall/"isvar"
property * index;// cptall/"index"
property * pred;// cptall/"pred"
property * addRational;// cptall/"addRational"
property * subRational;// cptall/"subRational"
property * multRational;// cptall/"multRational"
property * divRational;// cptall/"divRational"
property * simplify;// cptall/"simplify"
property * pgcd;// cptall/"pgcd"
property * ppcm;// cptall/"ppcm"
property * memoryVerification;// cptall/"memoryVerification"
property * fflush;// cptall/"fflush"
property * read_string;// cptall/"read_string"
property * read_int;// cptall/"read_int"
property * write_int;// cptall/"write_int"
property * format_time;// cptall/"format_time"
property * beginMonitor;// cptall/"beginMonitor"
property * endMonitor;// cptall/"endMonitor"
property * restartMonitor;// cptall/"restartMonitor"
property * old;// cptall/"old"
property * creset;// cptall/"creset"
property * cfix;// cptall/"cfix"
property * cdiff;// cptall/"cdiff"
property * cget;// cptall/"cget"
property * cold;// cptall/"cold"
property * cset;// cptall/"cset"
property * cinc;// cptall/"cinc"
property * chunks;// cptall/"chunks"
property * makeBoolArray;// cptall/"makeBoolArray"
property * setTrue;// cptall/"setTrue"
property * isTrue;// cptall/"isTrue"
property * canonicity;// cptall/"canonicity"
property * already_used_actions;// cptall/"already_used_actions"
property * conflicts;// cptall/"conflicts"
property * dichotomy;// cptall/"dichotomy"
property * distances;// cptall/"distances"
property * facts;// cptall/"facts"
property * init_heuristic;// cptall/"init_heuristic"
property * initial_bound;// cptall/"initial_bound"
property * instance_only;// cptall/"instance_only"
property * interactive;// cptall/"interactive"
property * landmarks;// cptall/"landmarks"
property * memory;// cptall/"memory"
property * mutex_sets;// cptall/"mutex_sets"
property * operators;// cptall/"operators"
property * print_domains;// cptall/"print_domains"
property * print_actions;// cptall/"print_actions"
property * print_events;// cptall/"print_events"
property * propagate_inactive_causals;// cptall/"propagate_inactive_causals"
property * propagate_inactive_threats;// cptall/"propagate_inactive_threats"
property * propagate_causals;// cptall/"propagate_causals"
property * propagate_mutex;// cptall/"propagate_mutex"
property * read_distances;// cptall/"read_distances"
property * relevance;// cptall/"relevance"
property * strategy;// cptall/"strategy"
property * write_distances;// cptall/"write_distances"
property * makeSession;// cptall/"makeSession"
property * stringToRational;// cptall/"stringToRational"
property * usage;// cptall/"usage"
property * tinit;// cptall/"tinit"
property * producers;// cptall/"producers"
property * consumers;// cptall/"consumers"
property * deleters;// cptall/"deleters"
property * causals;// cptall/"causals"
property * activeCausals;// cptall/"activeCausals"
property * pairCost;// cptall/"pairCost"
property * enabled;// cptall/"enabled"
property * reachable;// cptall/"reachable"
property * complete_print;// cptall/"complete_print"
property * parameters;// cptall/"parameters"
property * getPairCost;// cptall/"getPairCost"
property * setPairCost;// cptall/"setPairCost"
property * closeFluentParser;// cptall/"closeFluentParser"
property * setConsumes;// cptall/"setConsumes"
property * setMutex;// cptall/"setMutex"
property * setProduces;// cptall/"setProduces"
property * setDeletes;// cptall/"setDeletes"
property * closeFluent;// cptall/"closeFluent"
property * nbActionsMore;// cptall/"nbActionsMore"
property * nbCausalsMore;// cptall/"nbCausalsMore"
property * fluents;// cptall/"fluents"
property * numinit;// cptall/"numinit"
property * isevent;// cptall/"isevent"
property * prec;// cptall/"prec"
property * del;// cptall/"del"
property * edel;// cptall/"edel"
property * actionsMutex;// cptall/"actionsMutex"
property * mutexSolved;// cptall/"mutexSolved"
property * durationRat;// cptall/"durationRat"
property * duration;// cptall/"duration"
property * tinitEvent;// cptall/"tinitEvent"
property * tend;// cptall/"tend"
property * distance;// cptall/"distance"
property * properties;// cptall/"properties"
property * commutative;// cptall/"commutative"
property * used;// cptall/"used"
property * excluded;// cptall/"excluded"
property * reachedPrec;// cptall/"reachedPrec"
property * action_to_string;// cptall/"action_to_string"
property * setUsed;// cptall/"setUsed"
property * activeActions;// cptall/"activeActions"
property * setExcluded;// cptall/"setExcluded"
property * isUsed;// cptall/"isUsed"
property * isExcluded;// cptall/"isExcluded"
property * isMutex;// cptall/"isMutex"
property * consumes;// cptall/"consumes"
property * produces;// cptall/"produces"
property * deletes;// cptall/"deletes"
property * setDistance;// cptall/"setDistance"
property * getDistance;// cptall/"getDistance"
property * consumer;// cptall/"consumer"
property * getProducers;// cptall/"getProducers"
property * firstStart;// cptall/"firstStart"
property * lastStart;// cptall/"lastStart"
property * getDuration;// cptall/"getDuration"
property * firstEnd;// cptall/"firstEnd"
property * lastEnd;// cptall/"lastEnd"
property * closeActionParser;// cptall/"closeActionParser"
property * closeAction;// cptall/"closeAction"
property * actions;// cptall/"actions"
property * makeCausalLink;// cptall/"makeCausalLink"
property * causalLinks;// cptall/"causalLinks"
property * cloneAction;// cptall/"cloneAction"
property * cloneCausalLink;// cptall/"cloneCausalLink"
property * canProduce;// cptall/"canProduce"
property * addDomainVal;// cptall/"addDomainVal"
property * computeActionsMutex;// cptall/"computeActionsMutex"
property * updateInfA;// cptall/"updateInfA"
property * traceUpdateInf;// cptall/"traceUpdateInf"
property * exclude;// cptall/"exclude"
property * updateSupA;// cptall/"updateSupA"
property * traceUpdateSup;// cptall/"traceUpdateSup"
property * traceActionExcluded;// cptall/"traceActionExcluded"
property * isPossible;// cptall/"isPossible"
property * remProducer;// cptall/"remProducer"
property * fluent;// cptall/"fluent"
property * init;// cptall/"init"
property * excl;// cptall/"excl"
property * cmax;// cptall/"cmax"
property * solvedThreats;// cptall/"solvedThreats"
property * bestValue;// cptall/"bestValue"
property * getProducerNum;// cptall/"getProducerNum"
property * setProducer;// cptall/"setProducer"
property * getProducer;// cptall/"getProducer"
property * isProducer;// cptall/"isProducer"
property * getThreats;// cptall/"getThreats"
property * setRequired;// cptall/"setRequired"
property * isRequired;// cptall/"isRequired"
property * closeCausalLink;// cptall/"closeCausalLink"
property * updateInfC;// cptall/"updateInfC"
property * updateSupC;// cptall/"updateSupC"
property * slack;// cptall/"slack"
property * computeH1Distances;// cptall/"computeH1Distances"
property * updateCost;// cptall/"updateCost"
property * computeH1Cost;// cptall/"computeH1Cost"
property * computeH2Distances;// cptall/"computeH2Distances"
property * computeH2Cost;// cptall/"computeH2Cost"
property * computeInitH0Cost;// cptall/"computeInitH0Cost"
property * computeInitH1Cost;// cptall/"computeInitH1Cost"
property * computeInitH2Cost;// cptall/"computeInitH2Cost"
property * writeDistances;// cptall/"writeDistances"
property * session;// cptall/"session"
property * readDistances;// cptall/"readDistances"
property * instance;// cptall/"instance"
property * events;// cptall/"events"
property * startAction;// cptall/"startAction"
property * endAction;// cptall/"endAction"
property * mutexSets;// cptall/"mutexSets"
property * nbSolvedThreats;// cptall/"nbSolvedThreats"
property * cptChoiceSupport;// cptall/"cptChoiceSupport"
property * cptChoiceConflict;// cptall/"cptChoiceConflict"
property * cptChoiceMutex;// cptall/"cptChoiceMutex"
property * makePlanningProblem;// cptall/"makePlanningProblem"
property * readProblem;// cptall/"readProblem"
property * makespan;// cptall/"makespan"
property * arity;// cptall/"arity"
property * typing;// cptall/"typing"
property * upTypes;// cptall/"upTypes"
property * createAttributeSpace;// cptall/"createAttributeSpace"
property * enablers;// cptall/"enablers"
property * consequences;// cptall/"consequences"
property * addConstant;// cptall/"addConstant"
property * inequalities;// cptall/"inequalities"
property * attPrec;// cptall/"attPrec"
property * attEffect;// cptall/"attEffect"
property * domaine;// cptall/"domaine"
property * createType;// cptall/"createType"
property * createPredicate;// cptall/"createPredicate"
property * createVariable;// cptall/"createVariable"
property * createConstant;// cptall/"createConstant"
property * createTerm;// cptall/"createTerm"
property * tinitRat;// cptall/"tinitRat"
property * createAtom;// cptall/"createAtom"
property * precondition;// cptall/"precondition"
property * addEffect;// cptall/"addEffect"
property * delEffect;// cptall/"delEffect"
property * durationExpr;// cptall/"durationExpr"
property * createOperator;// cptall/"createOperator"
property * durative;// cptall/"durative"
property * equality;// cptall/"equality"
property * initState;// cptall/"initState"
property * timedLitterals;// cptall/"timedLitterals"
property * goal;// cptall/"goal"
property * openFile;// cptall/"openFile"
property * readNext;// cptall/"readNext"
property * closeFile;// cptall/"closeFile"
property * parseError;// cptall/"parseError"
property * readChar;// cptall/"readChar"
property * currentWord;// cptall/"currentWord"
property * nextWord;// cptall/"nextWord"
property * checkWord;// cptall/"checkWord"
property * checkNext;// cptall/"checkNext"
property * testNext;// cptall/"testNext"
property * readBegin;// cptall/"readBegin"
property * readEnd;// cptall/"readEnd"
property * testBegin;// cptall/"testBegin"
property * testEnd;// cptall/"testEnd"
property * readDomain;// cptall/"readDomain"
property * readRequirements;// cptall/"readRequirements"
property * readTypes;// cptall/"readTypes"
property * readConstants;// cptall/"readConstants"
property * readPredicates;// cptall/"readPredicates"
property * readFunctions;// cptall/"readFunctions"
property * readOperator;// cptall/"readOperator"
property * readInstance;// cptall/"readInstance"
property * readDomainInstance;// cptall/"readDomainInstance"
property * readObjects;// cptall/"readObjects"
property * readInitState;// cptall/"readInitState"
property * readGoal;// cptall/"readGoal"
property * readAtomList;// cptall/"readAtomList"
property * readConjunction;// cptall/"readConjunction"
property * readTermList;// cptall/"readTermList"
property * readParameters;// cptall/"readParameters"
property * readDuration;// cptall/"readDuration"
property * readPrecondition;// cptall/"readPrecondition"
property * readEffect;// cptall/"readEffect"
property * readExpression;// cptall/"readExpression"
property * readAtom;// cptall/"readAtom"
property * addAtomToList;// cptall/"addAtomToList"
property * readRational;// cptall/"readRational"
property * readOperationTerms;// cptall/"readOperationTerms"
property * createAttributeSpaces;// cptall/"createAttributeSpaces"
property * createFluent;// cptall/"createFluent"
property * instantiateOperator;// cptall/"instantiateOperator"
property * instantiateOperators;// cptall/"instantiateOperators"
property * addEvent;// cptall/"addEvent"
property * addEdels;// cptall/"addEdels"
property * computeReachability;// cptall/"computeReachability"
property * computeDurations;// cptall/"computeDurations"
property * supportConstraint;// cptall/"supportConstraint"
property * traceProducerUse;// cptall/"traceProducerUse"
property * traceProducerExclude;// cptall/"traceProducerExclude"
property * synchronizeCausal;// cptall/"synchronizeCausal"
property * protectCausal;// cptall/"protectCausal"
property * computeLocalMutexSets;// cptall/"computeLocalMutexSets"
property * actionConstraint;// cptall/"actionConstraint"
property * protectAgainst;// cptall/"protectAgainst"
property * synchronizeAction;// cptall/"synchronizeAction"
property * use;// cptall/"use"
property * makeOrder;// cptall/"makeOrder"
property * traceActionUsed;// cptall/"traceActionUsed"
property * computeGlobalMutexSets;// cptall/"computeGlobalMutexSets"
property * orderBefore;// cptall/"orderBefore"
property * orderBefore2;// cptall/"orderBefore2"
property * dist;// cptall/"dist"
property * minDistance;// cptall/"minDistance"
property * mindist;// cptall/"mindist"
property * addMutexSet;// cptall/"addMutexSet"
property * makespanAfter;// cptall/"makespanAfter"
property * makespanBefore;// cptall/"makespanBefore"
property * tracePrecedenceAwake;// cptall/"tracePrecedenceAwake"
property * quickPost;// cptall/"quickPost"
property * makeInitTimeHeuristic;// cptall/"makeInitTimeHeuristic"
property * makeProducerVarHeuristic;// cptall/"makeProducerVarHeuristic"
property * bestAction;// cptall/"bestAction"
property * traceSupportChoice;// cptall/"traceSupportChoice"
property * makeBestProducerSelector;// cptall/"makeBestProducerSelector"
property * causal;// cptall/"causal"
property * threat;// cptall/"threat"
property * makeConflictBranching;// cptall/"makeConflictBranching"
property * selectConflict;// cptall/"selectConflict"
property * bestConflict;// cptall/"bestConflict"
property * makeSolveConflict;// cptall/"makeSolveConflict"
property * traceConflictChoice;// cptall/"traceConflictChoice"
property * action1;// cptall/"action1"
property * action2;// cptall/"action2"
property * bestMutex;// cptall/"bestMutex"
property * makeSolveMutex;// cptall/"makeSolveMutex"
property * tracePopWorld;// cptall/"tracePopWorld"
property * tracePushWorld;// cptall/"tracePushWorld"
property * runPlanner;// cptall/"runPlanner"
property * rp;// cptall/"rp"
property * makeInitialConstraints;// cptall/"makeInitialConstraints"
property * makePlanningSolver;// cptall/"makePlanningSolver"
property * convert;// cptall/"convert"
property * computeRelevance;// cptall/"computeRelevance"
property * printPlan;// cptall/"printPlan"
property * computeActionRelevance;// cptall/"computeActionRelevance"
property * orderActionsPlan;// cptall/"orderActionsPlan"

// module definition 
 void metaLoad();};

extern cptallClass cptall;

#endif
