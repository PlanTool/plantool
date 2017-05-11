// interface defination for module cpt, Mon Nov  8 15:47:01 2004
#ifndef CLAIREH_cpt
#define CLAIREH_cpt


class cpt_Atom;
class cpt_Symbol;
class cpt_Term;
class cpt_Rational;
class cpt_OperationTerms;
class cpt_Addition;
class cpt_Substraction;
class cpt_Multiplication;
class cpt_Division;
class cpt_Counter;
class cpt_BoolArray;
class cpt_Session;
class cpt_Predicate;
class cpt_Variabl;
class cpt_Constant;
class cpt_Fluent;
class cpt_Action;
class cpt_CausalLink;
class cpt_PlanningProblem;
class cpt_Instance;
class cpt_TransitionRule;
class cpt_PositiveAtom;
class cpt_NegativeAtom;
class cpt_Operator;
class cpt_Domain;
class cpt_SupportConstraint;
class cpt_ActionConstraint;
class cpt_PrecedenceConstraint;
class cpt_CausalPrecedenceConstraint;
class cpt_InitTimeDomain;
class cpt_ProducerDomain;
class cpt_BestProducerHeuristic;
class cpt_SolveConflict;
class cpt_ConflictBranching;
class cpt_Conflict;
class cpt_Mutex;
class cpt_SolveMutex;

class cpt_Atom: public ephemeral_object{ 
  public:
     cpt_Predicate *pred;
     list *terms;
     OID tinitRat;} 
;

class cpt_Symbol: public ClaireObject{ 
  public:
     char *name;} 
;

class cpt_Term: public cpt_Symbol{ 
  public:
     ClaireBoolean *isvar;} 
;

class cpt_Rational: public ephemeral_object{ 
  public:
     int num;
     int den;} 
;

class cpt_OperationTerms: public ephemeral_object{ 
  public:
     list *terms;} 
;

class cpt_Addition: public cpt_OperationTerms{ 
  public:} 
;

class cpt_Substraction: public cpt_OperationTerms{ 
  public:} 
;

class cpt_Multiplication: public cpt_OperationTerms{ 
  public:} 
;

class cpt_Division: public cpt_OperationTerms{ 
  public:} 
;

class cpt_Counter: public choco_Ephemeral{ 
  public:
     int nb;
     int old;} 
;

class cpt_BoolArray: public ephemeral_object{ 
  public:
     list *chunks;} 
;

class cpt_Session: public choco_Ephemeral{ 
  public:
     ClaireBoolean *canonicity;
     ClaireBoolean *already_used_actions;
     int conflicts;
     int dichotomy;
     int distances;
     char *facts;
     int init_heuristic;
     cpt_Rational *initial_bound;
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

class cpt_Predicate: public cpt_Symbol{ 
  public:
     int arity;
     ClaireBoolean *typing;
     set *upTypes;} 
;

class cpt_Variabl: public cpt_Term{ 
  public:
     int index;
     list *inequalities;
     set *attPrec;
     set *attEffect;
     set *domaine;} 
;

class cpt_Constant: public cpt_Term{ 
  public:} 
;

class cpt_Fluent: public choco_Ephemeral{ 
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

class cpt_Action: public choco_IntVar{ 
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
     cpt_Rational *durationRat;
     int duration;
     int tinit;
     int tinitEvent;
     int tend;
     list *distance;
     list *properties;
     cpt_BoolArray *commutative;
     ClaireBoolean *used;
     ClaireBoolean *excluded;
     ClaireBoolean *reachable;
     int reachedPrec;} 
;

class cpt_CausalLink: public choco_IntVar{ 
  public:
     cpt_Fluent *fluent;
     cpt_Action *consumer;
     choco_IntVar *init;
     int excl;
     int cmax;
     int offset;
     OID solvedThreats;
     int bestValue;} 
;

class cpt_PlanningProblem: public choco_Problem{ 
  public:
     cpt_Instance *instance;
     list *fluents;
     list *actions;
     list *events;
     cpt_Action *startAction;
     cpt_Action *endAction;
     list *causalLinks;
     list *activeCausals;
     list *activeActions;
     list *mutexSets;
     int nbActionsMore;
     int nbCausalsMore;
     int nbSolvedThreats;
     cpt_Counter *cptChoiceSupport;
     cpt_Counter *cptChoiceConflict;
     cpt_Counter *cptChoiceMutex;
     cpt_Session *session;} 
;

class cpt_Instance: public ClaireObject{ 
  public:
     char *name;
     cpt_Domain *domain;
     list *initState;
     list *timedLitterals;
     list *actions;
     list *events;
     list *fluents;
     list *goal;
     int pgcd;
     int ppcm;} 
;

class cpt_TransitionRule: public ClaireObject{ 
  public:
     set *enablers;
     set *consequences;} 
;

class cpt_PositiveAtom: public cpt_Atom{ 
  public:} 
;

class cpt_NegativeAtom: public cpt_Atom{ 
  public:} 
;

class cpt_Operator: public ClaireObject{ 
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

class cpt_Domain: public ClaireObject{ 
  public:
     char *name;
     ClaireBoolean *durative;
     ClaireBoolean *equality;
     list *operators;} 
;

class cpt_SupportConstraint: public choco_BinIntConstraint{ 
  public:} 
;

class cpt_ActionConstraint: public choco_UnIntConstraint{ 
  public:} 
;

class cpt_PrecedenceConstraint: public choco_BinIntConstraint{ 
  public:} 
;

class cpt_CausalPrecedenceConstraint: public choco_TernIntConstraint{ 
  public:} 
;

class cpt_InitTimeDomain: public choco_VarSelector{ 
  public:
     cpt_PlanningProblem *problem;} 
;

class cpt_ProducerDomain: public choco_VarSelector{ 
  public:
     cpt_PlanningProblem *problem;} 
;

class cpt_BestProducerHeuristic: public choco_ValSelector{ 
  public:} 
;

class cpt_SolveConflict: public choco_BinBranching{ 
  public:
     cpt_Conflict *bestConflict;} 
;

class cpt_ConflictBranching: public choco_CompositeBranching{ 
  public:} 
;

class cpt_Conflict: public choco_Ephemeral{ 
  public:
     cpt_CausalLink *causal;
     cpt_Action *threat;} 
;

class cpt_Mutex: public choco_Ephemeral{ 
  public:
     cpt_Action *action1;
     cpt_Action *action2;} 
;

class cpt_SolveMutex: public choco_BinBranching{ 
  public:
     cpt_Mutex *bestMutex;} 
;
extern void  claire_self_print_Rational_cpt(cpt_Rational *r);
extern void  claire_self_print_list2_cpt(list *terms);
extern void  claire_self_print_Addition_cpt(cpt_Addition *a);
extern void  claire_self_print_Substraction_cpt(cpt_Substraction *s);
extern void  claire_self_print_Multiplication_cpt(cpt_Multiplication *m);
extern void  claire_self_print_Division_cpt(cpt_Division *d);
extern list * cpt_evaluateTerms_OperationTerms(cpt_OperationTerms *o,list *p);
extern cpt_Rational * cpt_evaluateExpr_Rational(cpt_Rational *r,list *p);
extern cpt_Rational * cpt_evaluateExpr_any(OID a,list *p);
extern cpt_Rational * cpt_evaluateExpr_Addition(cpt_Addition *a,list *p);
extern cpt_Rational * cpt_evaluateExpr_Substraction(cpt_Substraction *s,list *p);
extern cpt_Rational * cpt_evaluateExpr_Multiplication(cpt_Multiplication *m,list *p);
extern cpt_Rational * cpt_evaluateExpr_Division(cpt_Division *d,list *p);
extern void  cpt_addRational_Rational(cpt_Rational *r,cpt_Rational *t,ClaireBoolean *prem);
extern void  cpt_subRational_Rational(cpt_Rational *r,cpt_Rational *t,ClaireBoolean *prem);
extern void  cpt_multRational_Rational(cpt_Rational *r,cpt_Rational *t,ClaireBoolean *prem);
extern void  cpt_divRational_Rational(cpt_Rational *r,cpt_Rational *t,ClaireBoolean *prem);
extern void  cpt_simplify_Rational(cpt_Rational *r);
extern ClaireBoolean * claire__equal_Rational(cpt_Rational *r1,cpt_Rational *r2);
extern ClaireBoolean * claire__I_equal_Rational(cpt_Rational *r1,cpt_Rational *r2);
extern int  cpt_pgcd_integer(int a,int b);
extern int  cpt_ppcm_integer(int a,int b);
extern void  cpt_memoryVerification_integer(int x,int y,char *s);
extern void  cpt_fflush_void();
extern char * cpt_read_string_port(ClairePort *p);
extern int  cpt_read_int_port(ClairePort *p);
extern void  cpt_write_int_integer(int n,ClairePort *p);
extern char * cpt_format_time_integer(int t);
extern void  cpt_beginMonitor_string(char *message);
extern void  cpt_endMonitor_void();
extern void  cpt_restartMonitor_string(char *message);
extern void  cpt_creset_Counter(cpt_Counter *c);
extern void  cpt_cfix_Counter(cpt_Counter *c);
extern int  cpt_cdiff_Counter(cpt_Counter *c);
extern int  cpt_cget_Counter(cpt_Counter *c);
extern int  cpt_cold_Counter(cpt_Counter *c);
extern void  cpt_cset_Counter(cpt_Counter *c,int n);
extern void  cpt_cinc_Counter1(cpt_Counter *c,int n);
extern void  cpt_cinc_Counter2(cpt_Counter *c);
extern cpt_BoolArray * cpt_makeBoolArray_integer(int nb);
extern void  cpt_setTrue_BoolArray(cpt_BoolArray *t,int i);
extern ClaireBoolean * cpt_isTrue_BoolArray(cpt_BoolArray *t,int i);
extern cpt_Session * cpt_makeSession_list(list *params);
extern void  cpt_usage_void();
extern void  claire_self_print_Fluent_cpt(cpt_Fluent *f);
extern void  cpt_complete_print_Fluent(cpt_Fluent *f);
extern int  cpt_getPairCost_Fluent(cpt_Fluent *f1,cpt_Fluent *f2);
extern void  cpt_setPairCost_Fluent(cpt_Fluent *f1,cpt_Fluent *f2,int c);
extern void  cpt_closeFluentParser_list(list *fluents,cpt_Fluent *f);
extern void  cpt_closeFluent_PlanningProblem(cpt_PlanningProblem *pb,cpt_Fluent *f);
extern void  claire_self_print_Action_cpt(cpt_Action *a);
extern char * cpt_action_to_string_Action(cpt_Action *a);
extern void  cpt_complete_print_Action(cpt_Action *a);
extern void  cpt_setUsed_Action(cpt_Action *a);
extern void  cpt_setExcluded_Action(cpt_Action *a);
extern ClaireBoolean * cpt_isUsed_Action(cpt_Action *a);
extern ClaireBoolean * cpt_isExcluded_Action(cpt_Action *a);
extern void  cpt_setMutex_Action(cpt_Action *a,cpt_Action *a_prime);
extern ClaireBoolean * cpt_isMutex_Action(cpt_Action *a,cpt_Action *a_prime);
extern void  cpt_setConsumes_Action(cpt_Action *a,cpt_Fluent *f);
extern void  cpt_setProduces_Action(cpt_Action *a,cpt_Fluent *f);
extern void  cpt_setDeletes_Action(cpt_Action *a,cpt_Fluent *f);
extern ClaireBoolean * cpt_consumes_Action(cpt_Action *a,cpt_Fluent *f);
extern ClaireBoolean * cpt_produces_Action(cpt_Action *a,cpt_Fluent *f);
extern ClaireBoolean * cpt_deletes_Action(cpt_Action *a,cpt_Fluent *f);
extern void  cpt_setDistance_Action(cpt_Action *a,cpt_Action *a_prime,int d);
extern int  cpt_getDistance_Action1(cpt_Action *a,cpt_Action *a_prime);
extern int  cpt_getDistance_CausalLink(cpt_CausalLink *c,cpt_Action *a);
extern int  cpt_getDistance_Action2(cpt_Action *a,cpt_CausalLink *c);
extern int  cpt_firstStart_Action(cpt_Action *a);
extern int  cpt_lastStart_Action(cpt_Action *a);
extern int  cpt_getDuration_Action(cpt_Action *a);
extern int  cpt_firstEnd_Action(cpt_Action *a);
extern int  cpt_lastEnd_Action(cpt_Action *a);
extern void  cpt_closeActionParser_list(list *actions,list *fluents,cpt_Action *a);
extern void  cpt_closeAction_PlanningProblem(cpt_PlanningProblem *pb,cpt_Action *a);
extern cpt_Action * cpt_cloneAction_Action(cpt_Action *a,cpt_CausalLink *causal);
extern void  cpt_addDomainVal_LinkedListIntDomain(choco_LinkedListIntDomain *d,int x,int s);
extern void  cpt_computeActionsMutex_Action(cpt_Action *a);
extern void  cpt_updateInfA_Action(cpt_Action *a,int val);
extern void  cpt_updateSupA_Action(cpt_Action *a,int val);
extern void  cpt_exclude_Action(cpt_Action *a);
extern void  claire_self_print_CausalLink_cpt(cpt_CausalLink *c);
extern int  cpt_firstStart_CausalLink(cpt_CausalLink *c);
extern int  cpt_lastStart_CausalLink(cpt_CausalLink *c);
extern int  cpt_getProducerNum_integer(int i,cpt_CausalLink *c);
extern int  cpt_getProducerNum_Action(cpt_Action *a,cpt_CausalLink *c);
extern int  cpt_getProducerNum_CausalLink(cpt_CausalLink *c);
extern void  cpt_setProducer_CausalLink(cpt_CausalLink *c,cpt_Action *a);
extern void  cpt_remProducer_CausalLink(cpt_CausalLink *c,cpt_Action *a);
extern ClaireBoolean * cpt_canProduce_Action(cpt_Action *a,cpt_CausalLink *c);
extern OID  cpt_getProducer_CausalLink(cpt_CausalLink *c);
extern ClaireBoolean * cpt_isProducer_Action(cpt_Action *a,cpt_CausalLink *c);
extern list * cpt_getProducers_CausalLink(cpt_CausalLink *c);
extern list * cpt_getThreats_CausalLink(cpt_CausalLink *c);
extern void  cpt_setExcluded_CausalLink(cpt_CausalLink *c);
extern void  cpt_setRequired_CausalLink(cpt_CausalLink *c);
extern ClaireBoolean * cpt_isRequired_CausalLink(cpt_CausalLink *c);
extern ClaireBoolean * cpt_isExcluded_CausalLink(cpt_CausalLink *c);
extern ClaireBoolean * cpt_isPossible_CausalLink(cpt_CausalLink *c);
extern cpt_CausalLink * cpt_makeCausalLink_PlanningProblem(cpt_PlanningProblem *pb,cpt_Fluent *f,cpt_Action *a);
extern void  cpt_closeCausalLink_PlanningProblem(cpt_PlanningProblem *pb,cpt_CausalLink *c);
extern cpt_CausalLink * cpt_cloneCausalLink_CausalLink(cpt_CausalLink *c,cpt_Action *a);
extern void  cpt_updateInfC_CausalLink(cpt_CausalLink *c,int val);
extern void  cpt_updateSupC_CausalLink(cpt_CausalLink *c,int val);
extern ClaireBoolean * cpt_alwaysPrecede_Action1(cpt_Action *a,cpt_Action *a_prime);
extern ClaireBoolean * cpt_alwaysPrecede_CausalLink(cpt_CausalLink *c,cpt_Action *a);
extern ClaireBoolean * cpt_alwaysPrecede_Action2(cpt_Action *a,cpt_CausalLink *c);
extern ClaireBoolean * cpt_cannotPrecede_Action1(cpt_Action *a,cpt_Action *a_prime);
extern ClaireBoolean * cpt_cannotPrecede_CausalLink(cpt_CausalLink *c,cpt_Action *a);
extern ClaireBoolean * cpt_cannotPrecede_Action2(cpt_Action *a,cpt_CausalLink *c);
extern ClaireBoolean * cpt_canPrecede_Action1(cpt_Action *a,cpt_Action *a_prime);
extern ClaireBoolean * cpt_canPrecede_CausalLink(cpt_CausalLink *c,cpt_Action *a);
extern ClaireBoolean * cpt_canPrecede_Action2(cpt_Action *a,cpt_CausalLink *c);
extern ClaireBoolean * cpt_canPrecedeWeak_Action1(cpt_Action *a,cpt_Action *a_prime);
extern ClaireBoolean * cpt_canPrecedeWeak_CausalLink(cpt_CausalLink *c,cpt_Action *a);
extern ClaireBoolean * cpt_canPrecedeWeak_Action2(cpt_Action *a,cpt_CausalLink *c);
extern int  cpt_slack_Action1(cpt_Action *a,cpt_CausalLink *c);
extern int  cpt_slack_CausalLink(cpt_CausalLink *c,cpt_Action *a);
extern int  cpt_slack_Action2(cpt_Action *a1,cpt_Action *a2);
extern void  cpt_computeH1Distances_PlanningProblem(cpt_PlanningProblem *pb);
extern void  cpt_computeH1Cost_list(list *actions,int start);
extern void  cpt_updateCost_Fluent1(cpt_Fluent *f,int cost);
extern void  cpt_computeH2Distances_PlanningProblem(cpt_PlanningProblem *pb);
extern void  cpt_computeInitH0Cost_list(list *actions,list *fluents);
extern void  cpt_computeInitH1Cost_list(list *actions,list *fluents);
extern void  cpt_computeInitH2Cost_list(list *actions,list *fluents);
extern void  cpt_computeH2Cost_list(list *actions,list *fluents,int start);
extern void  cpt_updateCost_Fluent2(cpt_Fluent *f1,cpt_Fluent *f2,int cost);
extern void  cpt_writeDistances_PlanningProblem(cpt_PlanningProblem *pb);
extern void  cpt_readDistances_PlanningProblem(cpt_PlanningProblem *pb);
extern cpt_PlanningProblem * cpt_makePlanningProblem_Session(cpt_Session *s);
extern void  choco_propagate_PlanningProblem_cpt(cpt_PlanningProblem *pb);
extern void  claire_self_print_Symbol_cpt(cpt_Symbol *s);
extern void  claire_self_print_tuple2_cpt(tuple *att);
extern void  claire_self_print_set2_cpt(set *s);
extern tuple * cpt_createAttributeSpace_Predicate(cpt_Predicate *p,int n);
extern tuple * cpt_createAttributeSpace_Predicate_(cpt_Predicate *g0334,int g0335);
extern void  claire_self_print_TransitionRule_cpt(cpt_TransitionRule *t);
extern void  cpt_addConstant_Predicate(cpt_Predicate *p,int n,cpt_Constant *c);
extern cpt_Predicate * cpt_createType_string(char *s);
extern cpt_Predicate * cpt_createPredicate_string1(char *s);
extern cpt_Predicate * cpt_createPredicate_string2(char *s,int i);
extern cpt_Variabl * cpt_createVariable_string(char *rs);
extern cpt_Constant * cpt_createConstant_string(char *s);
extern cpt_Term * cpt_createTerm_string(char *s);
extern void  claire_self_print_PositiveAtom_cpt(cpt_PositiveAtom *a);
extern void  claire_self_print_NegativeAtom_cpt(cpt_NegativeAtom *a);
extern cpt_Atom * cpt_createAtom_string(char *n);
extern cpt_Atom * cpt_createAtom_Predicate(cpt_Predicate *p,list *t,ClaireBoolean *sign,OID init);
extern void  claire_self_print_Operator_cpt(cpt_Operator *o);
extern cpt_Operator * cpt_createOperator_string(char *s);
extern void  claire_self_print_Domain_cpt(cpt_Domain *d);
extern void  claire_self_print_Instance_cpt(cpt_Instance *p);
extern void  cpt_openFile_string(char *s);
extern void  cpt_closeFile_void();
extern void  cpt_parseError_void();
extern void  cpt_readChar_void();
extern char * cpt_readNext_void();
extern char * cpt_currentWord_void();
extern char * cpt_nextWord_void();
extern void  cpt_checkWord_string(char *s,char *w);
extern void  cpt_checkNext_string(char *s);
extern ClaireBoolean * cpt_testNext_string(char *s);
extern void  cpt_readBegin_void();
extern void  cpt_readEnd_void();
extern ClaireBoolean * cpt_testBegin_void();
extern ClaireBoolean * cpt_testEnd_void();
extern cpt_Domain * cpt_readDomain_void();
extern cpt_Instance * cpt_readInstance_Domain(cpt_Domain *d);
extern void  cpt_readDomainInstance_void();
extern void  cpt_readObjects_void();
extern void  cpt_readInitState_Instance(cpt_Instance *pb);
extern void  cpt_readGoal_Instance(cpt_Instance *pb);
extern void  cpt_readRequirements_Domain(cpt_Domain *d);
extern void  cpt_readTypes_Domain(cpt_Domain *d);
extern void  cpt_readConstants_void();
extern void  cpt_readPredicates_void();
extern void  cpt_readFunctions_void();
extern cpt_Operator * cpt_readOperator_Domain(cpt_Domain *d);
extern void  cpt_readParameters_Operator(cpt_Operator *o);
extern void  cpt_readDuration_Operator(cpt_Operator *o);
extern void  cpt_readPrecondition_Domain(cpt_Domain *d,cpt_Operator *o);
extern void  cpt_readEffect_Domain(cpt_Domain *d,cpt_Operator *o);
extern tuple * cpt_readConjunction_boolean(ClaireBoolean *timed);
extern tuple * cpt_readConjunction_boolean_(ClaireBoolean *g0364);
extern tuple * cpt_readAtomList_boolean(ClaireBoolean *timed);
extern tuple * cpt_readAtomList_boolean_(ClaireBoolean *g0367);
extern void  cpt_addAtomToList_Atom(cpt_Atom *a,list *l);
extern void  cpt_addAtomToList_PositiveAtom(cpt_PositiveAtom *a,list *pos,list *neg);
extern void  cpt_addAtomToList_NegativeAtom(cpt_NegativeAtom *a,list *pos,list *neg);
extern OID  cpt_readAtom_boolean(ClaireBoolean *timed);
extern cpt_Rational * cpt_readRational_void();
extern cpt_Rational * cpt_stringToRational_string(char *s);
extern ephemeral_object * cpt_readExpression_void();
extern list * cpt_readOperationTerms_void();
extern list * cpt_readTermList_void();
extern void  cpt_createAttributeSpaces_Domain(cpt_Domain *d);
extern cpt_Fluent * cpt_createFluent_Instance1(cpt_Instance *pb,cpt_Atom *a);
extern cpt_Fluent * cpt_createFluent_Instance2(cpt_Instance *pb,cpt_Atom *a,list *c);
extern cpt_Fluent * cpt_createFluent_Instance3(cpt_Instance *pb,cpt_Predicate *p,list *t);
extern cpt_Fluent * cpt_createFluent_Predicate(cpt_Predicate *p,list *c);
extern void  cpt_instantiateOperator_Instance(cpt_Instance *pb,cpt_Operator *o,int i,list *params);
extern void  cpt_instantiateOperators_Instance(cpt_Instance *pb);
extern void  cpt_addEvent_Instance1(cpt_Instance *pb,cpt_NegativeAtom *a);
extern void  cpt_addEvent_Instance2(cpt_Instance *pb,cpt_PositiveAtom *a);
extern void  cpt_addEdels_Action(cpt_Action *a,cpt_Fluent *f,list *fluents);
extern void  cpt_computeReachability_Action(cpt_Action *a);
extern void  cpt_computeReachability_Fluent(cpt_Fluent *f);
extern void  cpt_computeDurations_Instance(cpt_Instance *pb);
extern cpt_Instance * cpt_readProblem_Session(cpt_Session *s);
extern cpt_SupportConstraint * cpt_supportConstraint_CausalLink(cpt_CausalLink *c);
extern void  choco_awake_SupportConstraint_cpt(cpt_SupportConstraint *sc);
extern void  choco_awakeOnInst_SupportConstraint_cpt(cpt_SupportConstraint *sc,int idx);
extern void  choco_awakeOnRem_SupportConstraint_cpt(cpt_SupportConstraint *sc,int idx,int v);
extern void  choco_propagate_SupportConstraint_cpt(cpt_SupportConstraint *sc);
extern cpt_ActionConstraint * cpt_actionConstraint_Action(cpt_Action *a);
extern void  choco_awake_ActionConstraint_cpt(cpt_ActionConstraint *ac);
extern void  choco_propagate_ActionConstraint_cpt(cpt_ActionConstraint *ac);
extern void  cpt_synchronizeCausal_CausalLink(cpt_CausalLink *c);
extern void  cpt_synchronizeAction_Action(cpt_Action *a);
extern void  cpt_protectAgainst_Action(cpt_Action *a);
extern void  cpt_protectCausal_CausalLink(cpt_CausalLink *c);
extern void  cpt_use_Action(cpt_Action *a);
extern void  cpt_makeOrder_Action(cpt_Action *a,choco_IntVar *c);
extern void  cpt_orderBefore_Action1(cpt_Action *a,cpt_Action *a_prime);
extern void  cpt_orderBefore2_Action(cpt_Action *a,cpt_Action *a_prime);
extern void  cpt_orderBefore_CausalLink(cpt_CausalLink *c,cpt_Action *a);
extern void  cpt_orderBefore_Action2(cpt_Action *a,cpt_CausalLink *c);
extern int  cpt_dist_Action(cpt_Action *a,cpt_Action *a_prime);
extern int  cpt_minDistance_Action(cpt_Action *a,cpt_Action *a_prime);
extern void  cpt_computeGlobalMutexSets_Action(cpt_Action *a);
extern ClaireBoolean * cpt_makespan_PlanningProblem(cpt_PlanningProblem *pb);
extern void  cpt_computeLocalMutexSets_CausalLink(cpt_CausalLink *c);
extern void  cpt_addMutexSet_list(list *l,cpt_Action *a);
extern int  cpt_mindist_Action(cpt_Action *a,list *actions);
extern void  cpt_makespanAfter_CausalLink(cpt_CausalLink *c,list *after,list *middle);
extern void  cpt_makespanBefore_CausalLink(cpt_CausalLink *c,list *before,list *middle);
extern void  claire_self_print_PrecedenceConstraint_cpt(cpt_PrecedenceConstraint *c);
extern cpt_PrecedenceConstraint * claire__inf_inf_Action1(cpt_Action *a,cpt_Action *a_prime);
extern void  choco_propagate_PrecedenceConstraint_cpt(cpt_PrecedenceConstraint *c);
extern void  choco_awake_PrecedenceConstraint_cpt(cpt_PrecedenceConstraint *c);
extern void  choco_awakeOnInf_PrecedenceConstraint_cpt(cpt_PrecedenceConstraint *c,int idx);
extern void  choco_awakeOnSup_PrecedenceConstraint_cpt(cpt_PrecedenceConstraint *c,int idx);
extern void  choco_awakeOnInst_PrecedenceConstraint_cpt(cpt_PrecedenceConstraint *c,int idx);
extern OID  choco_askIfEntailed_PrecedenceConstraint(cpt_PrecedenceConstraint *c);
extern void  claire_self_print_CausalPrecedenceConstraint_cpt(cpt_CausalPrecedenceConstraint *c);
extern cpt_CausalPrecedenceConstraint * claire__inf_inf_Action2(cpt_Action *a,cpt_CausalLink *c);
extern void  choco_awake_CausalPrecedenceConstraint_cpt(cpt_CausalPrecedenceConstraint *c);
extern void  choco_awakeOnInf_CausalPrecedenceConstraint_cpt(cpt_CausalPrecedenceConstraint *c,int idx);
extern void  choco_awakeOnSup_CausalPrecedenceConstraint_cpt(cpt_CausalPrecedenceConstraint *c,int idx);
extern void  choco_awakeOnInst_CausalPrecedenceConstraint_cpt(cpt_CausalPrecedenceConstraint *c,int idx);
extern void  choco_awakeOnRem_CausalPrecedenceConstraint_cpt(cpt_CausalPrecedenceConstraint *c,int idx,int v);
extern void  choco_awakeOnVar_CausalPrecedenceConstraint_cpt(cpt_CausalPrecedenceConstraint *c,int idx);
extern OID  choco_askIfEntailed_CausalPrecedenceConstraint(cpt_CausalPrecedenceConstraint *c);
extern void  cpt_quickPost_PlanningProblem(cpt_PlanningProblem *pb,choco_AbstractConstraint *c);
extern cpt_InitTimeDomain * cpt_makeInitTimeHeuristic_PlanningProblem(cpt_PlanningProblem *pb);
extern OID  choco_selectVar_InitTimeDomain(cpt_InitTimeDomain *vs);
extern cpt_ProducerDomain * cpt_makeProducerVarHeuristic_PlanningProblem(cpt_PlanningProblem *pb);
extern OID  choco_selectVar_ProducerDomain(cpt_ProducerDomain *vs);
extern OID  cpt_bestAction_CausalLink(cpt_CausalLink *c);
extern choco_ValSelector * cpt_makeBestProducerSelector_void();
extern int  choco_getBestVal_BestProducerHeuristic(cpt_BestProducerHeuristic *vh,choco_IntVar *c);
extern cpt_ConflictBranching * cpt_makeConflictBranching_SolveConflict(cpt_SolveConflict *b1,choco_AbstractBranching *b2);
extern choco_AbstractBranching * choco_selectBranching_ConflictBranching_cpt(cpt_ConflictBranching *b);
extern ClaireBoolean * cpt_selectConflict_SolveConflict(cpt_SolveConflict *b);
extern cpt_SolveConflict * cpt_makeSolveConflict_void();
extern OID  choco_selectBranchingObject_SolveConflict(cpt_SolveConflict *b);
extern void  choco_goDownBranch_SolveConflict(cpt_SolveConflict *b,OID conflict,int first);
extern void  choco_traceDownBranch_SolveConflict(cpt_SolveConflict *b,OID conflict,int first);
extern cpt_SolveMutex * cpt_makeSolveMutex_void();
extern OID  choco_selectBranchingObject_SolveMutex(cpt_SolveMutex *b);
extern void  choco_goDownBranch_SolveMutex(cpt_SolveMutex *b,OID mutex,int first);
extern void  choco_traceDownBranch_SolveMutex(cpt_SolveMutex *b,OID conflict,int first);
extern void  cpt_traceProducerUse_CausalLink(cpt_CausalLink *c,int idx);
extern void  cpt_traceProducerExclude_CausalLink(cpt_CausalLink *c,int val);
extern void  cpt_tracePrecedenceAwake_PrecedenceConstraint(cpt_PrecedenceConstraint *c);
extern void  cpt_tracePrecedenceAwake_CausalPrecedenceConstraint(cpt_CausalPrecedenceConstraint *c);
extern void  cpt_tracePopWorld_PlanningProblem(cpt_PlanningProblem *pb);
extern void  cpt_tracePushWorld_PlanningProblem(cpt_PlanningProblem *pb);
extern void  cpt_traceSupportChoice_list(list *causals,OID c0);
extern void  cpt_traceConflictChoice_Conflict(cpt_Conflict *conflict);
extern void  cpt_traceActionUsed_Action(cpt_Action *a);
extern void  cpt_traceActionExcluded_Action(cpt_Action *a);
extern void  cpt_traceUpdateInf_Action(cpt_Action *a,int val);
extern void  cpt_traceUpdateSup_Action(cpt_Action *a,int val);
extern void  main_list(list *params);
extern void  cpt_rp_void();
extern void  cpt_runPlanner_Session(cpt_Session *s);
extern void  cpt_makeInitialConstraints_PlanningProblem(cpt_PlanningProblem *pb);
extern choco_Solver * cpt_makePlanningSolver_PlanningProblem(cpt_PlanningProblem *pb);
extern void  choco_solve_PlanningProblem(cpt_PlanningProblem *pb);
extern void  cpt_computeRelevance_PlanningProblem(cpt_PlanningProblem *pb,int bnd);
extern void  cpt_computeActionRelevance_Action(cpt_Action *a,int bnd);
extern void  cpt_printPlan_PlanningProblem(cpt_PlanningProblem *pb);
extern ClaireBoolean * cpt_orderActionsPlan_Action(cpt_Action *a1,cpt_Action *a2);
extern char * cpt_convert_PlanningProblem(cpt_PlanningProblem *pb,int n);

// namespace class for cpt 
class cptClass: public NameSpace {
public:

ClaireClass * _Atom;
ClaireClass * _Symbol;
ClaireClass * _Term;
ClaireClass * _Rational;
global_variable * zero;
ClaireClass * _OperationTerms;
ClaireClass * _Addition;
ClaireClass * _Substraction;
ClaireClass * _Multiplication;
ClaireClass * _Division;
global_variable * Expression;
global_variable * VERB;
global_variable * MEMORY_VERIF;
global_variable * PRETTY;
global_variable * SUPPORT_BRANCHING;
global_variable * CONFLICT_BRANCHING;
global_variable * resolution_strategies;
global_variable * H0;
global_variable * H1;
global_variable * H2;
global_variable * NO_DIC;
global_variable * MIN_DIC;
global_variable * DIFF_DIC;
global_variable * MAX_DIC;
global_variable * ACTUAL_CONF;
global_variable * EXTENDED_CONF;
global_variable * POSSIBLE_CONF;
global_variable * POP_WORLD;
global_variable * PUSH_WORLD;
global_variable * DECO;
global_variable * SUPPORT_CHOICE;
global_variable * PRODUCER_CHOICE;
global_variable * CONFLICT_CHOICE;
global_variable * PRODUCER_USE;
global_variable * PRODUCER_EXCLUDE;
global_variable * ACTION_PRECEDENCE;
global_variable * CAUSAL_PRECEDENCE;
global_variable * CONFLICT;
global_variable * UPDATE_INF;
global_variable * UPDATE_SUP;
global_variable * BUFFER;
global_variable * endl;
global_variable * begl;
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
global_variable * AttributeSpace;
global_variable * AttributeSpaceSet;
global_variable * attribute_spaces;
table * attribute_spaces_table;
table * constants_table;
global_variable * cpt_var;
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
global_variable * eof;
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
property * num;// cpt/"num"
property * den;// cpt/"den"
property * terms;// cpt/"terms"
property * evaluateTerms;// cpt/"evaluateTerms"
property * evaluateExpr;// cpt/"evaluateExpr"
property * isvar;// cpt/"isvar"
property * index;// cpt/"index"
property * pred;// cpt/"pred"
property * addRational;// cpt/"addRational"
property * subRational;// cpt/"subRational"
property * multRational;// cpt/"multRational"
property * divRational;// cpt/"divRational"
property * simplify;// cpt/"simplify"
property * pgcd;// cpt/"pgcd"
property * ppcm;// cpt/"ppcm"
property * memoryVerification;// cpt/"memoryVerification"
property * fflush;// cpt/"fflush"
property * read_string;// cpt/"read_string"
property * read_int;// cpt/"read_int"
property * write_int;// cpt/"write_int"
property * format_time;// cpt/"format_time"
property * beginMonitor;// cpt/"beginMonitor"
property * endMonitor;// cpt/"endMonitor"
property * restartMonitor;// cpt/"restartMonitor"
property * old;// cpt/"old"
property * creset;// cpt/"creset"
property * cfix;// cpt/"cfix"
property * cdiff;// cpt/"cdiff"
property * cget;// cpt/"cget"
property * cold;// cpt/"cold"
property * cset;// cpt/"cset"
property * cinc;// cpt/"cinc"
property * chunks;// cpt/"chunks"
property * makeBoolArray;// cpt/"makeBoolArray"
property * setTrue;// cpt/"setTrue"
property * isTrue;// cpt/"isTrue"
property * canonicity;// cpt/"canonicity"
property * already_used_actions;// cpt/"already_used_actions"
property * conflicts;// cpt/"conflicts"
property * dichotomy;// cpt/"dichotomy"
property * distances;// cpt/"distances"
property * facts;// cpt/"facts"
property * init_heuristic;// cpt/"init_heuristic"
property * initial_bound;// cpt/"initial_bound"
property * instance_only;// cpt/"instance_only"
property * interactive;// cpt/"interactive"
property * landmarks;// cpt/"landmarks"
property * memory;// cpt/"memory"
property * mutex_sets;// cpt/"mutex_sets"
property * operators;// cpt/"operators"
property * print_domains;// cpt/"print_domains"
property * print_actions;// cpt/"print_actions"
property * print_events;// cpt/"print_events"
property * propagate_inactive_causals;// cpt/"propagate_inactive_causals"
property * propagate_inactive_threats;// cpt/"propagate_inactive_threats"
property * propagate_causals;// cpt/"propagate_causals"
property * propagate_mutex;// cpt/"propagate_mutex"
property * read_distances;// cpt/"read_distances"
property * relevance;// cpt/"relevance"
property * strategy;// cpt/"strategy"
property * write_distances;// cpt/"write_distances"
property * makeSession;// cpt/"makeSession"
property * stringToRational;// cpt/"stringToRational"
property * usage;// cpt/"usage"
property * tinit;// cpt/"tinit"
property * producers;// cpt/"producers"
property * consumers;// cpt/"consumers"
property * deleters;// cpt/"deleters"
property * causals;// cpt/"causals"
property * activeCausals;// cpt/"activeCausals"
property * pairCost;// cpt/"pairCost"
property * enabled;// cpt/"enabled"
property * reachable;// cpt/"reachable"
property * complete_print;// cpt/"complete_print"
property * parameters;// cpt/"parameters"
property * getPairCost;// cpt/"getPairCost"
property * setPairCost;// cpt/"setPairCost"
property * closeFluentParser;// cpt/"closeFluentParser"
property * setConsumes;// cpt/"setConsumes"
property * setMutex;// cpt/"setMutex"
property * setProduces;// cpt/"setProduces"
property * setDeletes;// cpt/"setDeletes"
property * closeFluent;// cpt/"closeFluent"
property * nbActionsMore;// cpt/"nbActionsMore"
property * nbCausalsMore;// cpt/"nbCausalsMore"
property * fluents;// cpt/"fluents"
property * numinit;// cpt/"numinit"
property * isevent;// cpt/"isevent"
property * prec;// cpt/"prec"
property * del;// cpt/"del"
property * edel;// cpt/"edel"
property * actionsMutex;// cpt/"actionsMutex"
property * mutexSolved;// cpt/"mutexSolved"
property * durationRat;// cpt/"durationRat"
property * duration;// cpt/"duration"
property * tinitEvent;// cpt/"tinitEvent"
property * tend;// cpt/"tend"
property * distance;// cpt/"distance"
property * properties;// cpt/"properties"
property * commutative;// cpt/"commutative"
property * used;// cpt/"used"
property * excluded;// cpt/"excluded"
property * reachedPrec;// cpt/"reachedPrec"
property * action_to_string;// cpt/"action_to_string"
property * setUsed;// cpt/"setUsed"
property * activeActions;// cpt/"activeActions"
property * setExcluded;// cpt/"setExcluded"
property * isUsed;// cpt/"isUsed"
property * isExcluded;// cpt/"isExcluded"
property * isMutex;// cpt/"isMutex"
property * consumes;// cpt/"consumes"
property * produces;// cpt/"produces"
property * deletes;// cpt/"deletes"
property * setDistance;// cpt/"setDistance"
property * getDistance;// cpt/"getDistance"
property * consumer;// cpt/"consumer"
property * getProducers;// cpt/"getProducers"
property * firstStart;// cpt/"firstStart"
property * lastStart;// cpt/"lastStart"
property * getDuration;// cpt/"getDuration"
property * firstEnd;// cpt/"firstEnd"
property * lastEnd;// cpt/"lastEnd"
property * closeActionParser;// cpt/"closeActionParser"
property * closeAction;// cpt/"closeAction"
property * actions;// cpt/"actions"
property * makeCausalLink;// cpt/"makeCausalLink"
property * causalLinks;// cpt/"causalLinks"
property * cloneAction;// cpt/"cloneAction"
property * cloneCausalLink;// cpt/"cloneCausalLink"
property * canProduce;// cpt/"canProduce"
property * addDomainVal;// cpt/"addDomainVal"
property * computeActionsMutex;// cpt/"computeActionsMutex"
property * updateInfA;// cpt/"updateInfA"
property * traceUpdateInf;// cpt/"traceUpdateInf"
property * exclude;// cpt/"exclude"
property * updateSupA;// cpt/"updateSupA"
property * traceUpdateSup;// cpt/"traceUpdateSup"
property * traceActionExcluded;// cpt/"traceActionExcluded"
property * isPossible;// cpt/"isPossible"
property * remProducer;// cpt/"remProducer"
property * fluent;// cpt/"fluent"
property * init;// cpt/"init"
property * excl;// cpt/"excl"
property * cmax;// cpt/"cmax"
property * solvedThreats;// cpt/"solvedThreats"
property * bestValue;// cpt/"bestValue"
property * getProducerNum;// cpt/"getProducerNum"
property * setProducer;// cpt/"setProducer"
property * getProducer;// cpt/"getProducer"
property * isProducer;// cpt/"isProducer"
property * getThreats;// cpt/"getThreats"
property * setRequired;// cpt/"setRequired"
property * isRequired;// cpt/"isRequired"
property * closeCausalLink;// cpt/"closeCausalLink"
property * updateInfC;// cpt/"updateInfC"
property * updateSupC;// cpt/"updateSupC"
property * slack;// cpt/"slack"
property * computeH1Distances;// cpt/"computeH1Distances"
property * updateCost;// cpt/"updateCost"
property * computeH1Cost;// cpt/"computeH1Cost"
property * computeH2Distances;// cpt/"computeH2Distances"
property * computeH2Cost;// cpt/"computeH2Cost"
property * computeInitH0Cost;// cpt/"computeInitH0Cost"
property * computeInitH1Cost;// cpt/"computeInitH1Cost"
property * computeInitH2Cost;// cpt/"computeInitH2Cost"
property * writeDistances;// cpt/"writeDistances"
property * session;// cpt/"session"
property * readDistances;// cpt/"readDistances"
property * instance;// cpt/"instance"
property * events;// cpt/"events"
property * startAction;// cpt/"startAction"
property * endAction;// cpt/"endAction"
property * mutexSets;// cpt/"mutexSets"
property * nbSolvedThreats;// cpt/"nbSolvedThreats"
property * cptChoiceSupport;// cpt/"cptChoiceSupport"
property * cptChoiceConflict;// cpt/"cptChoiceConflict"
property * cptChoiceMutex;// cpt/"cptChoiceMutex"
property * makePlanningProblem;// cpt/"makePlanningProblem"
property * readProblem;// cpt/"readProblem"
property * makespan;// cpt/"makespan"
property * arity;// cpt/"arity"
property * typing;// cpt/"typing"
property * upTypes;// cpt/"upTypes"
property * createAttributeSpace;// cpt/"createAttributeSpace"
property * enablers;// cpt/"enablers"
property * consequences;// cpt/"consequences"
property * addConstant;// cpt/"addConstant"
property * inequalities;// cpt/"inequalities"
property * attPrec;// cpt/"attPrec"
property * attEffect;// cpt/"attEffect"
property * domaine;// cpt/"domaine"
property * createType;// cpt/"createType"
property * createPredicate;// cpt/"createPredicate"
property * createVariable;// cpt/"createVariable"
property * createConstant;// cpt/"createConstant"
property * createTerm;// cpt/"createTerm"
property * tinitRat;// cpt/"tinitRat"
property * createAtom;// cpt/"createAtom"
property * precondition;// cpt/"precondition"
property * addEffect;// cpt/"addEffect"
property * delEffect;// cpt/"delEffect"
property * durationExpr;// cpt/"durationExpr"
property * createOperator;// cpt/"createOperator"
property * durative;// cpt/"durative"
property * equality;// cpt/"equality"
property * initState;// cpt/"initState"
property * timedLitterals;// cpt/"timedLitterals"
property * goal;// cpt/"goal"
property * openFile;// cpt/"openFile"
property * readNext;// cpt/"readNext"
property * closeFile;// cpt/"closeFile"
property * parseError;// cpt/"parseError"
property * readChar;// cpt/"readChar"
property * currentWord;// cpt/"currentWord"
property * nextWord;// cpt/"nextWord"
property * checkWord;// cpt/"checkWord"
property * checkNext;// cpt/"checkNext"
property * testNext;// cpt/"testNext"
property * readBegin;// cpt/"readBegin"
property * readEnd;// cpt/"readEnd"
property * testBegin;// cpt/"testBegin"
property * testEnd;// cpt/"testEnd"
property * readDomain;// cpt/"readDomain"
property * readRequirements;// cpt/"readRequirements"
property * readTypes;// cpt/"readTypes"
property * readConstants;// cpt/"readConstants"
property * readPredicates;// cpt/"readPredicates"
property * readFunctions;// cpt/"readFunctions"
property * readOperator;// cpt/"readOperator"
property * readInstance;// cpt/"readInstance"
property * readDomainInstance;// cpt/"readDomainInstance"
property * readObjects;// cpt/"readObjects"
property * readInitState;// cpt/"readInitState"
property * readGoal;// cpt/"readGoal"
property * readAtomList;// cpt/"readAtomList"
property * readConjunction;// cpt/"readConjunction"
property * readTermList;// cpt/"readTermList"
property * readParameters;// cpt/"readParameters"
property * readDuration;// cpt/"readDuration"
property * readPrecondition;// cpt/"readPrecondition"
property * readEffect;// cpt/"readEffect"
property * readExpression;// cpt/"readExpression"
property * readAtom;// cpt/"readAtom"
property * addAtomToList;// cpt/"addAtomToList"
property * readRational;// cpt/"readRational"
property * readOperationTerms;// cpt/"readOperationTerms"
property * createAttributeSpaces;// cpt/"createAttributeSpaces"
property * createFluent;// cpt/"createFluent"
property * instantiateOperator;// cpt/"instantiateOperator"
property * instantiateOperators;// cpt/"instantiateOperators"
property * addEvent;// cpt/"addEvent"
property * addEdels;// cpt/"addEdels"
property * computeReachability;// cpt/"computeReachability"
property * computeDurations;// cpt/"computeDurations"
property * supportConstraint;// cpt/"supportConstraint"
property * traceProducerUse;// cpt/"traceProducerUse"
property * traceProducerExclude;// cpt/"traceProducerExclude"
property * synchronizeCausal;// cpt/"synchronizeCausal"
property * protectCausal;// cpt/"protectCausal"
property * computeLocalMutexSets;// cpt/"computeLocalMutexSets"
property * actionConstraint;// cpt/"actionConstraint"
property * protectAgainst;// cpt/"protectAgainst"
property * synchronizeAction;// cpt/"synchronizeAction"
property * use;// cpt/"use"
property * makeOrder;// cpt/"makeOrder"
property * traceActionUsed;// cpt/"traceActionUsed"
property * computeGlobalMutexSets;// cpt/"computeGlobalMutexSets"
property * orderBefore;// cpt/"orderBefore"
property * orderBefore2;// cpt/"orderBefore2"
property * dist;// cpt/"dist"
property * minDistance;// cpt/"minDistance"
property * mindist;// cpt/"mindist"
property * addMutexSet;// cpt/"addMutexSet"
property * makespanAfter;// cpt/"makespanAfter"
property * makespanBefore;// cpt/"makespanBefore"
property * tracePrecedenceAwake;// cpt/"tracePrecedenceAwake"
property * quickPost;// cpt/"quickPost"
property * makeInitTimeHeuristic;// cpt/"makeInitTimeHeuristic"
property * makeProducerVarHeuristic;// cpt/"makeProducerVarHeuristic"
property * bestAction;// cpt/"bestAction"
property * traceSupportChoice;// cpt/"traceSupportChoice"
property * makeBestProducerSelector;// cpt/"makeBestProducerSelector"
property * causal;// cpt/"causal"
property * threat;// cpt/"threat"
property * makeConflictBranching;// cpt/"makeConflictBranching"
property * selectConflict;// cpt/"selectConflict"
property * bestConflict;// cpt/"bestConflict"
property * makeSolveConflict;// cpt/"makeSolveConflict"
property * traceConflictChoice;// cpt/"traceConflictChoice"
property * action1;// cpt/"action1"
property * action2;// cpt/"action2"
property * bestMutex;// cpt/"bestMutex"
property * makeSolveMutex;// cpt/"makeSolveMutex"
property * tracePopWorld;// cpt/"tracePopWorld"
property * tracePushWorld;// cpt/"tracePushWorld"
property * runPlanner;// cpt/"runPlanner"
property * rp;// cpt/"rp"
property * makeInitialConstraints;// cpt/"makeInitialConstraints"
property * makePlanningSolver;// cpt/"makePlanningSolver"
property * convert;// cpt/"convert"
property * computeRelevance;// cpt/"computeRelevance"
property * printPlan;// cpt/"printPlan"
property * computeActionRelevance;// cpt/"computeActionRelevance"
property * orderActionsPlan;// cpt/"orderActionsPlan"

// module definition 
 void metaLoad();};

extern cptClass cpt;

#endif
