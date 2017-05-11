// interface defination for module cptshort, Wed Oct 20 12:37:57 2004
#ifndef CLAIREH_cptshort
#define CLAIREH_cptshort


class cptshort_SupportConstraint;
class cptshort_ActionConstraint;
class cptshort_PrecedenceConstraint;
class cptshort_CausalPrecedenceConstraint;
class cptshort_InitTimeDomain;
class cptshort_ProducerDomain;
class cptshort_BestProducerHeuristic;
class cptshort_SolveConflict;
class cptshort_ConflictBranching;
class cptshort_Conflict;
class cptshort_Mutex;
class cptshort_SolveMutex;

class cptshort_SupportConstraint: public choco_BinIntConstraint{ 
  public:} 
;

class cptshort_ActionConstraint: public choco_UnIntConstraint{ 
  public:} 
;

class cptshort_PrecedenceConstraint: public choco_BinIntConstraint{ 
  public:} 
;

class cptshort_CausalPrecedenceConstraint: public choco_TernIntConstraint{ 
  public:} 
;

class cptshort_InitTimeDomain: public choco_VarSelector{ 
  public:
     cptlib_PlanningProblem *problem;} 
;

class cptshort_ProducerDomain: public choco_VarSelector{ 
  public:
     cptlib_PlanningProblem *problem;} 
;

class cptshort_BestProducerHeuristic: public choco_ValSelector{ 
  public:} 
;

class cptshort_SolveConflict: public choco_BinBranching{ 
  public:
     cptshort_Conflict *bestConflict;} 
;

class cptshort_ConflictBranching: public choco_CompositeBranching{ 
  public:} 
;

class cptshort_Conflict: public choco_Ephemeral{ 
  public:
     cptlib_CausalLink *causal;
     cptlib_Action *threat;} 
;

class cptshort_Mutex: public choco_Ephemeral{ 
  public:
     cptlib_Action *action1;
     cptlib_Action *action2;} 
;

class cptshort_SolveMutex: public choco_BinBranching{ 
  public:
     cptshort_Mutex *bestMutex;} 
;
extern cptshort_SupportConstraint * cptshort_supportConstraint_CausalLink(cptlib_CausalLink *c);
extern void  choco_awake_SupportConstraint_cptshort(cptshort_SupportConstraint *sc);
extern void  choco_awakeOnInst_SupportConstraint_cptshort(cptshort_SupportConstraint *sc,int idx);
extern void  choco_awakeOnRem_SupportConstraint_cptshort(cptshort_SupportConstraint *sc,int idx,int v);
extern void  choco_propagate_SupportConstraint_cptshort(cptshort_SupportConstraint *sc);
extern cptshort_ActionConstraint * cptshort_actionConstraint_Action(cptlib_Action *a);
extern void  choco_awake_ActionConstraint_cptshort(cptshort_ActionConstraint *ac);
extern void  choco_propagate_ActionConstraint_cptshort(cptshort_ActionConstraint *ac);
extern void  cptshort_synchronizeCausal_CausalLink(cptlib_CausalLink *c);
extern void  cptshort_synchronizeAction_Action(cptlib_Action *a);
extern void  cptshort_protectAgainst_Action(cptlib_Action *a);
extern void  cptshort_protectCausal_CausalLink(cptlib_CausalLink *c);
extern void  cptshort_use_Action(cptlib_Action *a);
extern void  cptshort_makeOrder_Action(cptlib_Action *a,choco_IntVar *c);
extern void  cptshort_orderBefore_Action1(cptlib_Action *a,cptlib_Action *a_prime);
extern void  cptshort_orderBefore_CausalLink(cptlib_CausalLink *c,cptlib_Action *a);
extern void  cptshort_orderBefore_Action2(cptlib_Action *a,cptlib_CausalLink *c);
extern int  cptshort_dist_Action(cptlib_Action *a,cptlib_Action *a_prime);
extern int  cptshort_minDistance_Action(cptlib_Action *a,cptlib_Action *a_prime);
extern void  cptshort_computeGlobalMutexSets_Action(cptlib_Action *a);
extern ClaireBoolean * cptlib_makespan_PlanningProblem(cptlib_PlanningProblem *pb);
extern void  cptshort_computeLocalMutexSets_CausalLink(cptlib_CausalLink *c);
extern void  cptshort_addMutexSet_list(list *l,cptlib_Action *a);
extern int  cptshort_mindist_Action(cptlib_Action *a,list *actions);
extern void  cptshort_makespanAfter_CausalLink(cptlib_CausalLink *c,list *after,list *middle);
extern void  cptshort_makespanBefore_CausalLink(cptlib_CausalLink *c,list *before,list *middle);
extern void  claire_self_print_PrecedenceConstraint_cptshort(cptshort_PrecedenceConstraint *c);
extern cptshort_PrecedenceConstraint * claire__inf_inf_Action1(cptlib_Action *a,cptlib_Action *a_prime);
extern void  choco_propagate_PrecedenceConstraint_cptshort(cptshort_PrecedenceConstraint *c);
extern void  choco_awake_PrecedenceConstraint_cptshort(cptshort_PrecedenceConstraint *c);
extern void  choco_awakeOnInf_PrecedenceConstraint_cptshort(cptshort_PrecedenceConstraint *c,int idx);
extern void  choco_awakeOnSup_PrecedenceConstraint_cptshort(cptshort_PrecedenceConstraint *c,int idx);
extern void  choco_awakeOnInst_PrecedenceConstraint_cptshort(cptshort_PrecedenceConstraint *c,int idx);
extern OID  choco_askIfEntailed_PrecedenceConstraint(cptshort_PrecedenceConstraint *c);
extern void  claire_self_print_CausalPrecedenceConstraint_cptshort(cptshort_CausalPrecedenceConstraint *c);
extern cptshort_CausalPrecedenceConstraint * claire__inf_inf_Action2(cptlib_Action *a,cptlib_CausalLink *c);
extern void  choco_awake_CausalPrecedenceConstraint_cptshort(cptshort_CausalPrecedenceConstraint *c);
extern void  choco_awakeOnInf_CausalPrecedenceConstraint_cptshort(cptshort_CausalPrecedenceConstraint *c,int idx);
extern void  choco_awakeOnSup_CausalPrecedenceConstraint_cptshort(cptshort_CausalPrecedenceConstraint *c,int idx);
extern void  choco_awakeOnInst_CausalPrecedenceConstraint_cptshort(cptshort_CausalPrecedenceConstraint *c,int idx);
extern void  choco_awakeOnRem_CausalPrecedenceConstraint_cptshort(cptshort_CausalPrecedenceConstraint *c,int idx,int v);
extern void  choco_awakeOnVar_CausalPrecedenceConstraint_cptshort(cptshort_CausalPrecedenceConstraint *c,int idx);
extern OID  choco_askIfEntailed_CausalPrecedenceConstraint(cptshort_CausalPrecedenceConstraint *c);
extern void  cptshort_quickPost_PlanningProblem(cptlib_PlanningProblem *pb,choco_AbstractConstraint *c);
extern cptshort_InitTimeDomain * cptshort_makeInitTimeHeuristic_PlanningProblem(cptlib_PlanningProblem *pb);
extern OID  choco_selectVar_InitTimeDomain(cptshort_InitTimeDomain *vs);
extern cptshort_ProducerDomain * cptshort_makeProducerVarHeuristic_PlanningProblem(cptlib_PlanningProblem *pb);
extern OID  choco_selectVar_ProducerDomain(cptshort_ProducerDomain *vs);
extern OID  cptlib_bestAction_CausalLink(cptlib_CausalLink *c);
extern choco_ValSelector * cptshort_makeBestProducerSelector_void();
extern int  choco_getBestVal_BestProducerHeuristic(cptshort_BestProducerHeuristic *vh,choco_IntVar *c);
extern cptshort_ConflictBranching * cptshort_makeConflictBranching_SolveConflict(cptshort_SolveConflict *b1,choco_AbstractBranching *b2);
extern choco_AbstractBranching * choco_selectBranching_ConflictBranching_cptshort(cptshort_ConflictBranching *b);
extern ClaireBoolean * cptshort_selectConflict_SolveConflict(cptshort_SolveConflict *b);
extern cptshort_SolveConflict * cptshort_makeSolveConflict_void();
extern OID  choco_selectBranchingObject_SolveConflict(cptshort_SolveConflict *b);
extern void  choco_goDownBranch_SolveConflict(cptshort_SolveConflict *b,OID conflict,int first);
extern void  choco_traceDownBranch_SolveConflict(cptshort_SolveConflict *b,OID conflict,int first);
extern cptshort_SolveMutex * cptshort_makeSolveMutex_void();
extern OID  choco_selectBranchingObject_SolveMutex(cptshort_SolveMutex *b);
extern void  choco_goDownBranch_SolveMutex(cptshort_SolveMutex *b,OID mutex,int first);
extern void  choco_traceDownBranch_SolveMutex(cptshort_SolveMutex *b,OID conflict,int first);
extern void  cptshort_traceProducerUse_CausalLink(cptlib_CausalLink *c,int idx);
extern void  cptshort_traceProducerExclude_CausalLink(cptlib_CausalLink *c,int val);
extern void  cptshort_tracePrecedenceAwake_PrecedenceConstraint(cptshort_PrecedenceConstraint *c);
extern void  cptshort_tracePrecedenceAwake_CausalPrecedenceConstraint(cptshort_CausalPrecedenceConstraint *c);
extern void  cptshort_tracePopWorld_PlanningProblem(cptlib_PlanningProblem *pb);
extern void  cptshort_tracePushWorld_PlanningProblem(cptlib_PlanningProblem *pb);
extern void  cptshort_traceSupportChoice_list(list *causals,OID c0);
extern void  cptshort_traceConflictChoice_Conflict(cptshort_Conflict *conflict);
extern void  cptshort_traceActionUsed_Action(cptlib_Action *a);
extern void  cptlib_traceActionExcluded_Action(cptlib_Action *a);
extern void  cptlib_traceUpdateInf_Action(cptlib_Action *a,int val);
extern void  cptlib_traceUpdateSup_Action(cptlib_Action *a,int val);
extern void  main_list(list *params);
extern void  cptshort_rp_void();
extern void  cptshort_runPlanner_Session(cptlib_Session *s);
extern cptlib_PlanningProblem * cptshort_makeInstance_Session(cptlib_Session *s);
extern void  cptshort_makeInitialConstraints_PlanningProblem(cptlib_PlanningProblem *pb);
extern choco_Solver * cptshort_makePlanningSolver_PlanningProblem(cptlib_PlanningProblem *pb);
extern void  choco_solve_PlanningProblem(cptlib_PlanningProblem *pb);
extern void  cptshort_computeRelevance_PlanningProblem(cptlib_PlanningProblem *pb,int bnd);
extern void  cptshort_computeActionRelevance_Action(cptlib_Action *a,int bnd);
extern void  cptshort_printPlan_PlanningProblem(cptlib_PlanningProblem *pb);
extern ClaireBoolean * cptshort_orderActionsPlan_Action(cptlib_Action *a1,cptlib_Action *a2);
extern char * cptshort_action_to_string_Action(cptlib_Action *a);
extern char * cptshort_convert_PlanningProblem(cptlib_PlanningProblem *pb,int n);

// namespace class for cptshort 
class cptshortClass: public NameSpace {
public:

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
property * supportConstraint;// cptshort/"supportConstraint"
property * traceProducerUse;// cptshort/"traceProducerUse"
property * traceProducerExclude;// cptshort/"traceProducerExclude"
property * synchronizeCausal;// cptshort/"synchronizeCausal"
property * protectCausal;// cptshort/"protectCausal"
property * computeLocalMutexSets;// cptshort/"computeLocalMutexSets"
property * actionConstraint;// cptshort/"actionConstraint"
property * protectAgainst;// cptshort/"protectAgainst"
property * synchronizeAction;// cptshort/"synchronizeAction"
property * use;// cptshort/"use"
property * makeOrder;// cptshort/"makeOrder"
property * traceActionUsed;// cptshort/"traceActionUsed"
property * computeGlobalMutexSets;// cptshort/"computeGlobalMutexSets"
property * orderBefore;// cptshort/"orderBefore"
property * dist;// cptshort/"dist"
property * minDistance;// cptshort/"minDistance"
property * mindist;// cptshort/"mindist"
property * addMutexSet;// cptshort/"addMutexSet"
property * makespanAfter;// cptshort/"makespanAfter"
property * makespanBefore;// cptshort/"makespanBefore"
property * tracePrecedenceAwake;// cptshort/"tracePrecedenceAwake"
property * quickPost;// cptshort/"quickPost"
property * makeInitTimeHeuristic;// cptshort/"makeInitTimeHeuristic"
property * makeProducerVarHeuristic;// cptshort/"makeProducerVarHeuristic"
property * traceSupportChoice;// cptshort/"traceSupportChoice"
property * makeBestProducerSelector;// cptshort/"makeBestProducerSelector"
property * causal;// cptshort/"causal"
property * threat;// cptshort/"threat"
property * makeConflictBranching;// cptshort/"makeConflictBranching"
property * selectConflict;// cptshort/"selectConflict"
property * bestConflict;// cptshort/"bestConflict"
property * makeSolveConflict;// cptshort/"makeSolveConflict"
property * traceConflictChoice;// cptshort/"traceConflictChoice"
property * action1;// cptshort/"action1"
property * action2;// cptshort/"action2"
property * bestMutex;// cptshort/"bestMutex"
property * makeSolveMutex;// cptshort/"makeSolveMutex"
property * tracePopWorld;// cptshort/"tracePopWorld"
property * tracePushWorld;// cptshort/"tracePushWorld"
property * runPlanner;// cptshort/"runPlanner"
property * rp;// cptshort/"rp"
property * makeInstance;// cptshort/"makeInstance"
property * computeRelevance;// cptshort/"computeRelevance"
property * makeInitialConstraints;// cptshort/"makeInitialConstraints"
property * makePlanningSolver;// cptshort/"makePlanningSolver"
property * convert;// cptshort/"convert"
property * printPlan;// cptshort/"printPlan"
property * computeActionRelevance;// cptshort/"computeActionRelevance"
property * action_to_string;// cptshort/"action_to_string"
property * orderActionsPlan;// cptshort/"orderActionsPlan"

// module definition 
 void metaLoad();};

extern cptshortClass cptshort;

#endif
