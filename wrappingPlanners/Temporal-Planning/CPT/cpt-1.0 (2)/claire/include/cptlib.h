// interface defination for module cptlib, Wed Oct 20 12:37:53 2004
#ifndef CLAIREH_cptlib
#define CLAIREH_cptlib


class cptlib_Atom;
class cptlib_Symbol;
class cptlib_Term;
class cptlib_Rational;
class cptlib_OperationTerms;
class cptlib_Addition;
class cptlib_Substraction;
class cptlib_Multiplication;
class cptlib_Division;
class cptlib_Counter;
class cptlib_BoolArray;
class cptlib_Session;
class cptlib_Variabl;
class cptlib_Constant;
class cptlib_Fluent;
class cptlib_Action;
class cptlib_CausalLink;
class cptlib_PlanningProblem;
class cptlib_Predicate;
class cptlib_TransitionRule;
class cptlib_Operator;
class cptlib_Domain;
class cptlib_PDDLProblem;

class cptlib_Atom: public ClaireObject{ 
  public:
     cptlib_Predicate *pred;
     list *terms;} 
;

class cptlib_Symbol: public ClaireObject{ 
  public:
     char *name;} 
;

class cptlib_Term: public cptlib_Symbol{ 
  public:
     ClaireBoolean *isvar;} 
;

class cptlib_Rational: public ephemeral_object{ 
  public:
     int num;
     int den;} 
;

class cptlib_OperationTerms: public ephemeral_object{ 
  public:
     list *terms;} 
;

class cptlib_Addition: public cptlib_OperationTerms{ 
  public:} 
;

class cptlib_Substraction: public cptlib_OperationTerms{ 
  public:} 
;

class cptlib_Multiplication: public cptlib_OperationTerms{ 
  public:} 
;

class cptlib_Division: public cptlib_OperationTerms{ 
  public:} 
;

class cptlib_Counter: public choco_Ephemeral{ 
  public:
     int nb;
     int old;} 
;

class cptlib_BoolArray: public ephemeral_object{ 
  public:
     list *chunks;} 
;

class cptlib_Session: public choco_Ephemeral{ 
  public:
     ClaireBoolean *canonicity;
     ClaireBoolean *actual_conflicts;
     int dichotomy;
     int distances;
     char *facts;
     cptlib_Rational *initial_bound;
     ClaireBoolean *instance_only;
     ClaireBoolean *interactive;
     list *memory;
     ClaireBoolean *mutex_sets;
     char *operators;
     char *output;
     ClaireBoolean *propagate_inactive_causals;
     ClaireBoolean *propagate_inactive_threats;
     ClaireBoolean *propagate_causals;
     ClaireBoolean *read_distances;
     ClaireBoolean *relevance;
     int strategy;
     ClaireBoolean *write_distances;} 
;

class cptlib_Variabl: public cptlib_Term{ 
  public:
     int index;
     list *inequalities;
     set *att;
     set *attPrec;
     set *attEffect;
     cptlib_TransitionRule *tr;
     set *domaine;} 
;

class cptlib_Constant: public cptlib_Term{ 
  public:} 
;

class cptlib_Fluent: public choco_Ephemeral{ 
  public:
     int num;
     char *name;
     char *MSname;
     list *producers;
     list *consumers;
     list *deleters;
     list *realDeleters;
     list *causals;
     list *activeCausals;
     list *pairCost;
     ClaireBoolean *reachable;} 
;

class cptlib_Action: public choco_IntVar{ 
  public:
     int num;
     int numinit;
     list *parameters;
     list *prec;
     list *add;
     list *del;
     list *realDel;
     list *causals;
     set *actionsMutex;
     list *mutexSolved;
     cptlib_Rational *durationRat;
     int duration;
     int tinit;
     int tend;
     list *distance;
     list *properties;
     cptlib_BoolArray *commutative;
     ClaireBoolean *used;
     ClaireBoolean *excluded;
     ClaireBoolean *reachable;
     int reachedPrec;} 
;

class cptlib_CausalLink: public choco_IntVar{ 
  public:
     cptlib_Fluent *fluent;
     cptlib_Action *consumer;
     choco_IntVar *init;
     int excl;
     int cmax;
     int offset;
     list *solvedThreats;
     int bestValue;} 
;

class cptlib_PlanningProblem: public choco_Problem{ 
  public:
     list *fluents;
     list *actions;
     cptlib_Action *startAction;
     cptlib_Action *endAction;
     list *causalLinks;
     list *activeCausals;
     list *activeActions;
     list *mutexSets;
     int nbActionsMore;
     int nbCausalsMore;
     int nbSolvedThreats;
     int pgcdDurations;
     int ppcmDurations;
     cptlib_Counter *cptChoiceSupport;
     cptlib_Counter *cptChoiceConflict;
     cptlib_Session *session;} 
;

class cptlib_Predicate: public cptlib_Symbol{ 
  public:
     int arity;
     ClaireBoolean *typing;
     ClaireBoolean *first;
     set *upTypes;} 
;

class cptlib_TransitionRule: public ClaireObject{ 
  public:
     set *enablers;
     set *consequences;} 
;

class cptlib_Operator: public ClaireObject{ 
  public:
     char *name;
     list *parameters;
     list *precondition;
     list *addEffect;
     list *delEffect;
     list *constraints;
     ClaireObject *durationExpr;
     int duration;} 
;

class cptlib_Domain: public ClaireObject{ 
  public:
     char *name;
     ClaireBoolean *equality;
     list *operators;} 
;

class cptlib_PDDLProblem: public ClaireObject{ 
  public:
     char *name;
     list *initState;
     list *goal;} 
;
extern void  claire_self_print_Rational_cptlib(cptlib_Rational *r);
extern void  claire_self_print_Addition_cptlib(cptlib_Addition *a);
extern void  claire_self_print_Substraction_cptlib(cptlib_Substraction *s);
extern void  claire_self_print_Multiplication_cptlib(cptlib_Multiplication *m);
extern void  claire_self_print_Division_cptlib(cptlib_Division *d);
extern void  cptlib_operationTermsPrint_OperationTerms(cptlib_OperationTerms *o,char *ope);
extern list * cptlib_evaluateTerms_OperationTerms(cptlib_OperationTerms *o,list *p);
extern cptlib_Rational * cptlib_evaluateExpr_Rational(cptlib_Rational *r,list *p);
extern cptlib_Rational * cptlib_evaluateExpr_Atom(cptlib_Atom *a,list *p);
extern cptlib_Rational * cptlib_evaluateExpr_any(OID a,list *p);
extern cptlib_Rational * cptlib_evaluateExpr_Addition(cptlib_Addition *a,list *p);
extern cptlib_Rational * cptlib_evaluateExpr_Substraction(cptlib_Substraction *s,list *p);
extern cptlib_Rational * cptlib_evaluateExpr_Multiplication(cptlib_Multiplication *m,list *p);
extern cptlib_Rational * cptlib_evaluateExpr_Division(cptlib_Division *d,list *p);
extern void  cptlib_addRational_Rational(cptlib_Rational *r,cptlib_Rational *t);
extern void  cptlib_subRational_Rational(cptlib_Rational *r,cptlib_Rational *t,ClaireBoolean *prem);
extern void  cptlib_multRational_Rational(cptlib_Rational *r,cptlib_Rational *t);
extern void  cptlib_divRational_Rational(cptlib_Rational *r,cptlib_Rational *t,ClaireBoolean *prem);
extern void  cptlib_simplify_Rational(cptlib_Rational *r);
extern int  cptlib_pgcd_integer(int a,int b);
extern int  cptlib_ppcm_integer(int a,int b);
extern void  cptlib_memoryVerification_integer(int x,int y,char *s);
extern void  cptlib_fflush_void();
extern char * cptlib_read_string_port(ClairePort *p);
extern int  cptlib_read_int_port(ClairePort *p);
extern void  cptlib_write_int_integer(int n,ClairePort *p);
extern char * cptlib_format_time_integer(int t);
extern void  cptlib_beginMonitor_string(char *message);
extern void  cptlib_endMonitor_void();
extern void  cptlib_restartMonitor_string(char *message);
extern void  cptlib_creset_Counter(cptlib_Counter *c);
extern void  cptlib_cfix_Counter(cptlib_Counter *c);
extern int  cptlib_cdiff_Counter(cptlib_Counter *c);
extern int  cptlib_cget_Counter(cptlib_Counter *c);
extern int  cptlib_cold_Counter(cptlib_Counter *c);
extern void  cptlib_cset_Counter(cptlib_Counter *c,int n);
extern void  cptlib_cinc_Counter1(cptlib_Counter *c,int n);
extern void  cptlib_cinc_Counter2(cptlib_Counter *c);
extern cptlib_BoolArray * cptlib_makeBoolArray_integer(int nb);
extern void  cptlib_setTrue_BoolArray(cptlib_BoolArray *t,int i);
extern ClaireBoolean * cptlib_isTrue_BoolArray(cptlib_BoolArray *t,int i);
extern cptlib_Session * cptlib_makeSession_list(list *params);
extern void  cptlib_usage_void();
extern void  claire_self_print_Fluent_cptlib(cptlib_Fluent *f);
extern void  cptlib_complete_print_Fluent(cptlib_Fluent *f);
extern int  cptlib_getPairCost_Fluent(cptlib_Fluent *f1,cptlib_Fluent *f2);
extern void  cptlib_setPairCost_Fluent(cptlib_Fluent *f1,cptlib_Fluent *f2,int c);
extern void  cptlib_closeFluentParser_list(list *fluents,cptlib_Fluent *f);
extern void  cptlib_closeFluent_PlanningProblem(cptlib_PlanningProblem *pb,cptlib_Fluent *f);
extern void  claire_self_print_Action_cptlib(cptlib_Action *a);
extern void  cptlib_complete_print_Action(cptlib_Action *a);
extern void  cptlib_setUsed_Action(cptlib_Action *a);
extern void  cptlib_setExcluded_Action(cptlib_Action *a);
extern ClaireBoolean * cptlib_isUsed_Action(cptlib_Action *a);
extern ClaireBoolean * cptlib_isExcluded_Action(cptlib_Action *a);
extern void  cptlib_setMutex_Action(cptlib_Action *a,cptlib_Action *a_prime);
extern ClaireBoolean * cptlib_isMutex_Action(cptlib_Action *a,cptlib_Action *a_prime);
extern void  cptlib_setConsumes_Action(cptlib_Action *a,cptlib_Fluent *f);
extern void  cptlib_setProduces_Action(cptlib_Action *a,cptlib_Fluent *f);
extern void  cptlib_setDeletes_Action(cptlib_Action *a,cptlib_Fluent *f);
extern ClaireBoolean * cptlib_consumes_Action(cptlib_Action *a,cptlib_Fluent *f);
extern ClaireBoolean * cptlib_produces_Action(cptlib_Action *a,cptlib_Fluent *f);
extern ClaireBoolean * cptlib_deletes_Action(cptlib_Action *a,cptlib_Fluent *f);
extern void  cptlib_setDistance_Action(cptlib_Action *a,cptlib_Action *a_prime,int d);
extern int  cptlib_getDistance_Action1(cptlib_Action *a,cptlib_Action *a_prime);
extern int  cptlib_getDistance_CausalLink(cptlib_CausalLink *c,cptlib_Action *a);
extern int  cptlib_getDistance_Action2(cptlib_Action *a,cptlib_CausalLink *c);
extern int  cptlib_firstStart_Action(cptlib_Action *a);
extern int  cptlib_lastStart_Action(cptlib_Action *a);
extern int  cptlib_getDuration_Action(cptlib_Action *a);
extern int  cptlib_firstEnd_Action(cptlib_Action *a);
extern int  cptlib_lastEnd_Action(cptlib_Action *a);
extern void  cptlib_closeActionParser_list(list *actions,list *fluents,cptlib_Action *a);
extern void  cptlib_closeAction_PlanningProblem(cptlib_PlanningProblem *pb,cptlib_Action *a);
extern cptlib_Action * cptlib_cloneAction_Action(cptlib_Action *a,cptlib_CausalLink *causal);
extern void  cptlib_addDomainVal_LinkedListIntDomain(choco_LinkedListIntDomain *d,int x,int s);
extern void  cptlib_computeActionsMutex_Action(cptlib_Action *a);
extern void  cptlib_updateInfA_Action(cptlib_Action *a,int val);
extern void  cptlib_updateSupA_Action(cptlib_Action *a,int val);
extern void  cptlib_exclude_Action(cptlib_Action *a);
extern void  claire_self_print_CausalLink_cptlib(cptlib_CausalLink *c);
extern int  cptlib_firstStart_CausalLink(cptlib_CausalLink *c);
extern int  cptlib_lastStart_CausalLink(cptlib_CausalLink *c);
extern int  cptlib_getProducerNum_integer(int i,cptlib_CausalLink *c);
extern int  cptlib_getProducerNum_Action(cptlib_Action *a,cptlib_CausalLink *c);
extern int  cptlib_getProducerNum_CausalLink(cptlib_CausalLink *c);
extern void  cptlib_setProducer_CausalLink(cptlib_CausalLink *c,cptlib_Action *a);
extern void  cptlib_remProducer_CausalLink(cptlib_CausalLink *c,cptlib_Action *a);
extern ClaireBoolean * cptlib_canProduce_Action(cptlib_Action *a,cptlib_CausalLink *c);
extern OID  cptlib_getProducer_CausalLink(cptlib_CausalLink *c);
extern ClaireBoolean * cptlib_isProducer_Action(cptlib_Action *a,cptlib_CausalLink *c);
extern list * cptlib_getProducers_CausalLink(cptlib_CausalLink *c);
extern list * cptlib_getThreats_CausalLink(cptlib_CausalLink *c);
extern void  cptlib_setExcluded_CausalLink(cptlib_CausalLink *c);
extern void  cptlib_setRequired_CausalLink(cptlib_CausalLink *c);
extern ClaireBoolean * cptlib_isRequired_CausalLink(cptlib_CausalLink *c);
extern ClaireBoolean * cptlib_isExcluded_CausalLink(cptlib_CausalLink *c);
extern ClaireBoolean * cptlib_isPossible_CausalLink(cptlib_CausalLink *c);
extern cptlib_CausalLink * cptlib_makeCausalLink_PlanningProblem(cptlib_PlanningProblem *pb,cptlib_Fluent *f,cptlib_Action *a);
extern void  cptlib_closeCausalLink_PlanningProblem(cptlib_PlanningProblem *pb,cptlib_CausalLink *c);
extern cptlib_CausalLink * cptlib_cloneCausalLink_CausalLink(cptlib_CausalLink *c,cptlib_Action *a);
extern void  cptlib_updateInfC_CausalLink(cptlib_CausalLink *c,int val);
extern void  cptlib_updateSupC_CausalLink(cptlib_CausalLink *c,int val);
extern ClaireBoolean * cptlib_alwaysPrecede_Action1(cptlib_Action *a,cptlib_Action *a_prime);
extern ClaireBoolean * cptlib_alwaysPrecede_CausalLink(cptlib_CausalLink *c,cptlib_Action *a);
extern ClaireBoolean * cptlib_alwaysPrecede_Action2(cptlib_Action *a,cptlib_CausalLink *c);
extern ClaireBoolean * cptlib_cannotPrecede_Action1(cptlib_Action *a,cptlib_Action *a_prime);
extern ClaireBoolean * cptlib_cannotPrecede_CausalLink(cptlib_CausalLink *c,cptlib_Action *a);
extern ClaireBoolean * cptlib_cannotPrecede_Action2(cptlib_Action *a,cptlib_CausalLink *c);
extern ClaireBoolean * cptlib_canPrecede_Action(cptlib_Action *a,cptlib_Action *a_prime);
extern ClaireBoolean * cptlib_canPrecede_CausalLink(cptlib_CausalLink *c,cptlib_Action *a);
extern int  cptlib_slack_Action1(cptlib_Action *a,cptlib_CausalLink *c);
extern int  cptlib_slack_CausalLink(cptlib_CausalLink *c,cptlib_Action *a);
extern int  cptlib_slack_Action2(cptlib_Action *a1,cptlib_Action *a2);
extern void  cptlib_computeWeakDistances_PlanningProblem(cptlib_PlanningProblem *pb);
extern int  cptlib_computeWeakDistance_Action(cptlib_Action *a,cptlib_Action *a_prime);
extern void  cptlib_computeH2Distances_PlanningProblem(cptlib_PlanningProblem *pb);
extern void  cptlib_computeInitCost_list(list *actions,list *fluents);
extern void  cptlib_computeCost_list(list *actions,list *fluents);
extern void  cptlib_updateCost_Fluent(cptlib_Fluent *f1,cptlib_Fluent *f2,int cost);
extern void  cptlib_writeDistances_PlanningProblem(cptlib_PlanningProblem *pb);
extern void  cptlib_readDistances_PlanningProblem(cptlib_PlanningProblem *pb);
extern cptlib_PlanningProblem * cptlib_makePlanningProblem_Session(cptlib_Session *s);
extern void  choco_propagate_PlanningProblem_cptlib(cptlib_PlanningProblem *pb);
extern void  cptlib_pms_Action(cptlib_Action *a);
extern void  cptlib_pms_string1(char *s,cptlib_Action *a);
extern void  cptlib_pms_CausalLink1(cptlib_CausalLink *c);
extern void  cptlib_pms_CausalLink2(cptlib_CausalLink *c,cptlib_Action *a);
extern void  cptlib_pms_string2(char *s,cptlib_CausalLink *c,cptlib_Action *a);
extern void  cptlib_pms_string3(char *s,cptlib_CausalLink *c);
extern void  cptlib_makeMathSat_PlanningProblem(cptlib_PlanningProblem *pb);
extern ClaireBoolean * cptlib_alwaysPrecede2_Action(cptlib_Action *a,cptlib_CausalLink *c);
extern void  cptlib_makeMathSat2_PlanningProblem(cptlib_PlanningProblem *pb);
extern void  claire_self_print_Symbol_cptlib(cptlib_Symbol *s);
extern void  claire_self_print_tuple2_cptlib(tuple *att);
extern void  claire_self_print_set2_cptlib(set *s);
extern tuple * cptlib_createAttributeSpace_Predicate(cptlib_Predicate *p,int n);
extern tuple * cptlib_createAttributeSpace_Predicate_(cptlib_Predicate *g0500,int g0501);
extern void  claire_self_print_TransitionRule_cptlib(cptlib_TransitionRule *t);
extern cptlib_TransitionRule * cptlib_createTransitionRule_set(set *e,set *c);
extern void  cptlib_addConstant_Predicate(cptlib_Predicate *p,int n,cptlib_Constant *c);
extern cptlib_Predicate * cptlib_createPredicate_string(char *s);
extern cptlib_Variabl * cptlib_createVariable_string(char *rs);
extern cptlib_Constant * cptlib_createConstant_string(char *s);
extern cptlib_Term * cptlib_createTerm_string(char *s);
extern void  claire_self_print_Atom_cptlib(cptlib_Atom *a);
extern cptlib_Atom * cptlib_createAtom_Predicate(cptlib_Predicate *p,list *t);
extern void  claire_self_print_Operator_cptlib(cptlib_Operator *o);
extern cptlib_Operator * cptlib_createOperator_string(char *s);
extern void  claire_self_print_Domain_cptlib(cptlib_Domain *d);
extern cptlib_Domain * cptlib_createDomain_string(char *s);
extern void  cptlib_addOperator_Domain(cptlib_Domain *d,cptlib_Operator *o);
extern void  claire_self_print_PDDLProblem_cptlib(cptlib_PDDLProblem *p);
extern cptlib_PDDLProblem * cptlib_createPDDLProblem_string(char *s);
extern void  cptlib_addInit_PDDLProblem(cptlib_PDDLProblem *p,cptlib_Atom *a);
extern void  cptlib_addGoal_PDDLProblem(cptlib_PDDLProblem *p,cptlib_Atom *a);
extern void  cptlib_openFile_string(char *s);
extern void  cptlib_parseError_void();
extern void  cptlib_readChar_void();
extern char * cptlib_readNext_void();
extern char * cptlib_currentWord_void();
extern char * cptlib_nextWord_void();
extern void  cptlib_checkWord_string(char *s,char *w);
extern void  cptlib_checkNext_string(char *s);
extern ClaireBoolean * cptlib_testNext_string(char *s);
extern void  cptlib_readBegin_void();
extern void  cptlib_readEnd_void();
extern ClaireBoolean * cptlib_testBegin_void();
extern ClaireBoolean * cptlib_testEnd_void();
extern cptlib_Domain * cptlib_readDomain_void();
extern cptlib_PDDLProblem * cptlib_readPDDLProblem_void();
extern void  cptlib_readDomainPDDLProblem_void();
extern void  cptlib_readObjects_void();
extern list * cptlib_readInitState_void();
extern list * cptlib_readGoal_void();
extern void  cptlib_readRequirements_Domain(cptlib_Domain *d);
extern void  cptlib_readTypes_Domain(cptlib_Domain *d);
extern void  cptlib_readConstants_void();
extern void  cptlib_readPredicates_void();
extern void  cptlib_readFunctions_void();
extern cptlib_Operator * cptlib_readOperator_void();
extern void  cptlib_readParameters_Operator(cptlib_Operator *o);
extern void  cptlib_readDuration_Operator(cptlib_Operator *o);
extern void  cptlib_readPrecondition_Operator(cptlib_Operator *o);
extern void  cptlib_readEffect_Operator(cptlib_Operator *o);
extern tuple * cptlib_readConjunction_boolean(ClaireBoolean *timed);
extern tuple * cptlib_readConjunction_boolean_(ClaireBoolean *g0527);
extern tuple * cptlib_readAtomList_boolean(ClaireBoolean *timed);
extern tuple * cptlib_readAtomList_boolean_(ClaireBoolean *g0530);
extern tuple * cptlib_readAtom_boolean(ClaireBoolean *timed);
extern tuple * cptlib_readAtom_boolean_(ClaireBoolean *g0532);
extern cptlib_Rational * cptlib_readRational_void();
extern cptlib_Rational * cptlib_stringToRational_string(char *s);
extern ClaireObject * cptlib_readExpression_void();
extern list * cptlib_readOperationTerms_void();
extern list * cptlib_readTermList_void();
extern void  cptlib_createAttributeSpaces_Domain(cptlib_Domain *d);
extern cptlib_Fluent * cptlib_createFluent_Atom(cptlib_Atom *a,list *c,list *fluents);
extern cptlib_Fluent * cptlib_createFluent2_Predicate(cptlib_Predicate *p,list *t,list *fluents);
extern cptlib_Fluent * cptlib_makeFluent_Predicate(cptlib_Predicate *p,list *c);
extern void  cptlib_createConstraint_Atom(cptlib_Atom *a);
extern void  cptlib_instantiateOperator_Domain(cptlib_Domain *d,cptlib_Operator *o,int i,list *params,list *actions,list *fluents);
extern void  cptlib_instantiateOperators_Domain(cptlib_Domain *d,cptlib_PDDLProblem *p,list *actions,list *fluents);
extern void  cptlib_addEdels_Action(cptlib_Action *a,cptlib_Fluent *f,list *fluents);
extern void  cptlib_computeReachability_Action(cptlib_Action *a);
extern void  cptlib_computeReachability_Fluent(cptlib_Fluent *f);
extern tuple * cptlib_computeDurations_list(list *actions);
extern tuple * cptlib_computeDurations_list_(list *g0587);
extern tuple * cptlib_readProblem_Session(cptlib_Session *s);
extern tuple * cptlib_readProblem_Session_(cptlib_Session *g0597);

// namespace class for cptlib 
class cptlibClass: public NameSpace {
public:

ClaireClass * _Atom;
ClaireClass * _Symbol;
ClaireClass * _Term;
ClaireClass * _Rational;
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
global_variable * NO_DIST;
global_variable * WEAK_DIST;
global_variable * H2_DIST;
global_variable * NO_DIC;
global_variable * MIN_DIC;
global_variable * DIFF_DIC;
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
ClaireClass * _Variabl;
ClaireClass * _Constant;
ClaireClass * _Fluent;
ClaireClass * _Action;
ClaireClass * _CausalLink;
ClaireClass * _PlanningProblem;
ClaireClass * _Predicate;
ClaireClass * _TransitionRule;
global_variable * AttributeSpace;
global_variable * AttributeSpaceSet;
global_variable * attribute_spaces;
table * attribute_spaces_table;
table * constants_table;
global_variable * cpt_var;
global_variable * durative;
table * predicate_table;
table * variable_table;
table * constant_table;
ClaireClass * _Operator;
global_variable * typed_constants;
ClaireClass * _Domain;
ClaireClass * _PDDLProblem;
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
global_variable * dummy_rational;
table * functions_table;
global_variable * dummy_atom;
table * fluent_table;
table * constraints_table;
property * num;// cptlib/"num"
property * den;// cptlib/"den"
property * operationTermsPrint;// cptlib/"operationTermsPrint"
property * terms;// cptlib/"terms"
property * evaluateTerms;// cptlib/"evaluateTerms"
property * evaluateExpr;// cptlib/"evaluateExpr"
property * isvar;// cptlib/"isvar"
property * index;// cptlib/"index"
property * pred;// cptlib/"pred"
property * addRational;// cptlib/"addRational"
property * subRational;// cptlib/"subRational"
property * multRational;// cptlib/"multRational"
property * divRational;// cptlib/"divRational"
property * simplify;// cptlib/"simplify"
property * pgcd;// cptlib/"pgcd"
property * ppcm;// cptlib/"ppcm"
property * memoryVerification;// cptlib/"memoryVerification"
property * fflush;// cptlib/"fflush"
property * read_string;// cptlib/"read_string"
property * read_int;// cptlib/"read_int"
property * write_int;// cptlib/"write_int"
property * format_time;// cptlib/"format_time"
property * beginMonitor;// cptlib/"beginMonitor"
property * endMonitor;// cptlib/"endMonitor"
property * restartMonitor;// cptlib/"restartMonitor"
property * old;// cptlib/"old"
property * creset;// cptlib/"creset"
property * cfix;// cptlib/"cfix"
property * cdiff;// cptlib/"cdiff"
property * cget;// cptlib/"cget"
property * cold;// cptlib/"cold"
property * cset;// cptlib/"cset"
property * cinc;// cptlib/"cinc"
property * makeBoolArray;// cptlib/"makeBoolArray"
property * setTrue;// cptlib/"setTrue"
property * isTrue;// cptlib/"isTrue"
property * canonicity;// cptlib/"canonicity"
property * actual_conflicts;// cptlib/"actual_conflicts"
property * dichotomy;// cptlib/"dichotomy"
property * distances;// cptlib/"distances"
property * facts;// cptlib/"facts"
property * initial_bound;// cptlib/"initial_bound"
property * instance_only;// cptlib/"instance_only"
property * interactive;// cptlib/"interactive"
property * memory;// cptlib/"memory"
property * mutex_sets;// cptlib/"mutex_sets"
property * operators;// cptlib/"operators"
property * output;// cptlib/"output"
property * propagate_inactive_causals;// cptlib/"propagate_inactive_causals"
property * propagate_inactive_threats;// cptlib/"propagate_inactive_threats"
property * propagate_causals;// cptlib/"propagate_causals"
property * read_distances;// cptlib/"read_distances"
property * relevance;// cptlib/"relevance"
property * strategy;// cptlib/"strategy"
property * write_distances;// cptlib/"write_distances"
property * makeSession;// cptlib/"makeSession"
property * stringToRational;// cptlib/"stringToRational"
property * usage;// cptlib/"usage"
property * MSname;// cptlib/"MSname"
property * producers;// cptlib/"producers"
property * consumers;// cptlib/"consumers"
property * deleters;// cptlib/"deleters"
property * realDeleters;// cptlib/"realDeleters"
property * causals;// cptlib/"causals"
property * activeCausals;// cptlib/"activeCausals"
property * pairCost;// cptlib/"pairCost"
property * reachable;// cptlib/"reachable"
property * complete_print;// cptlib/"complete_print"
property * parameters;// cptlib/"parameters"
property * getPairCost;// cptlib/"getPairCost"
property * setPairCost;// cptlib/"setPairCost"
property * closeFluentParser;// cptlib/"closeFluentParser"
property * setMutex;// cptlib/"setMutex"
property * setDeletes;// cptlib/"setDeletes"
property * closeFluent;// cptlib/"closeFluent"
property * nbActionsMore;// cptlib/"nbActionsMore"
property * setConsumes;// cptlib/"setConsumes"
property * setProduces;// cptlib/"setProduces"
property * nbCausalsMore;// cptlib/"nbCausalsMore"
property * fluents;// cptlib/"fluents"
property * numinit;// cptlib/"numinit"
property * prec;// cptlib/"prec"
property * del;// cptlib/"del"
property * realDel;// cptlib/"realDel"
property * actionsMutex;// cptlib/"actionsMutex"
property * mutexSolved;// cptlib/"mutexSolved"
property * durationRat;// cptlib/"durationRat"
property * duration;// cptlib/"duration"
property * tinit;// cptlib/"tinit"
property * tend;// cptlib/"tend"
property * distance;// cptlib/"distance"
property * properties;// cptlib/"properties"
property * commutative;// cptlib/"commutative"
property * used;// cptlib/"used"
property * excluded;// cptlib/"excluded"
property * reachedPrec;// cptlib/"reachedPrec"
property * setUsed;// cptlib/"setUsed"
property * activeActions;// cptlib/"activeActions"
property * setExcluded;// cptlib/"setExcluded"
property * isUsed;// cptlib/"isUsed"
property * isExcluded;// cptlib/"isExcluded"
property * isMutex;// cptlib/"isMutex"
property * consumes;// cptlib/"consumes"
property * produces;// cptlib/"produces"
property * deletes;// cptlib/"deletes"
property * setDistance;// cptlib/"setDistance"
property * getDistance;// cptlib/"getDistance"
property * consumer;// cptlib/"consumer"
property * getProducers;// cptlib/"getProducers"
property * firstStart;// cptlib/"firstStart"
property * lastStart;// cptlib/"lastStart"
property * getDuration;// cptlib/"getDuration"
property * firstEnd;// cptlib/"firstEnd"
property * lastEnd;// cptlib/"lastEnd"
property * closeActionParser;// cptlib/"closeActionParser"
property * closeAction;// cptlib/"closeAction"
property * actions;// cptlib/"actions"
property * makeCausalLink;// cptlib/"makeCausalLink"
property * nbSolvedThreats;// cptlib/"nbSolvedThreats"
property * causalLinks;// cptlib/"causalLinks"
property * cloneAction;// cptlib/"cloneAction"
property * cloneCausalLink;// cptlib/"cloneCausalLink"
property * canProduce;// cptlib/"canProduce"
property * addDomainVal;// cptlib/"addDomainVal"
property * computeActionsMutex;// cptlib/"computeActionsMutex"
property * updateInfA;// cptlib/"updateInfA"
property * traceUpdateInf;// cptlib/"traceUpdateInf"
property * exclude;// cptlib/"exclude"
property * updateSupA;// cptlib/"updateSupA"
property * traceUpdateSup;// cptlib/"traceUpdateSup"
property * traceActionExcluded;// cptlib/"traceActionExcluded"
property * isPossible;// cptlib/"isPossible"
property * remProducer;// cptlib/"remProducer"
property * fluent;// cptlib/"fluent"
property * init;// cptlib/"init"
property * excl;// cptlib/"excl"
property * cmax;// cptlib/"cmax"
property * solvedThreats;// cptlib/"solvedThreats"
property * bestValue;// cptlib/"bestValue"
property * getProducerNum;// cptlib/"getProducerNum"
property * setProducer;// cptlib/"setProducer"
property * getProducer;// cptlib/"getProducer"
property * isProducer;// cptlib/"isProducer"
property * getThreats;// cptlib/"getThreats"
property * setRequired;// cptlib/"setRequired"
property * isRequired;// cptlib/"isRequired"
property * closeCausalLink;// cptlib/"closeCausalLink"
property * updateInfC;// cptlib/"updateInfC"
property * updateSupC;// cptlib/"updateSupC"
property * alwaysPrecede;// cptlib/"alwaysPrecede"
property * cannotPrecede;// cptlib/"cannotPrecede"
property * canPrecede;// cptlib/"canPrecede"
property * slack;// cptlib/"slack"
property * computeWeakDistances;// cptlib/"computeWeakDistances"
property * computeWeakDistance;// cptlib/"computeWeakDistance"
property * computeH2Distances;// cptlib/"computeH2Distances"
property * updateCost;// cptlib/"updateCost"
property * computeCost;// cptlib/"computeCost"
property * computeInitCost;// cptlib/"computeInitCost"
property * writeDistances;// cptlib/"writeDistances"
property * session;// cptlib/"session"
property * readDistances;// cptlib/"readDistances"
property * startAction;// cptlib/"startAction"
property * endAction;// cptlib/"endAction"
property * mutexSets;// cptlib/"mutexSets"
property * pgcdDurations;// cptlib/"pgcdDurations"
property * ppcmDurations;// cptlib/"ppcmDurations"
property * cptChoiceSupport;// cptlib/"cptChoiceSupport"
property * cptChoiceConflict;// cptlib/"cptChoiceConflict"
property * makePlanningProblem;// cptlib/"makePlanningProblem"
property * readProblem;// cptlib/"readProblem"
property * makespan;// cptlib/"makespan"
property * pms;// cptlib/"pms"
property * makeMathSat;// cptlib/"makeMathSat"
property * bestAction;// cptlib/"bestAction"
property * alwaysPrecede2;// cptlib/"alwaysPrecede2"
property * makeMathSat2;// cptlib/"makeMathSat2"
property * arity;// cptlib/"arity"
property * typing;// cptlib/"typing"
property * first;// cptlib/"first"
property * upTypes;// cptlib/"upTypes"
property * createAttributeSpace;// cptlib/"createAttributeSpace"
property * enablers;// cptlib/"enablers"
property * consequences;// cptlib/"consequences"
property * createTransitionRule;// cptlib/"createTransitionRule"
property * addConstant;// cptlib/"addConstant"
property * inequalities;// cptlib/"inequalities"
property * att;// cptlib/"att"
property * attPrec;// cptlib/"attPrec"
property * attEffect;// cptlib/"attEffect"
property * tr;// cptlib/"tr"
property * domaine;// cptlib/"domaine"
property * createPredicate;// cptlib/"createPredicate"
property * createVariable;// cptlib/"createVariable"
property * createConstant;// cptlib/"createConstant"
property * createTerm;// cptlib/"createTerm"
property * createAtom;// cptlib/"createAtom"
property * precondition;// cptlib/"precondition"
property * addEffect;// cptlib/"addEffect"
property * delEffect;// cptlib/"delEffect"
property * durationExpr;// cptlib/"durationExpr"
property * createOperator;// cptlib/"createOperator"
property * equality;// cptlib/"equality"
property * createDomain;// cptlib/"createDomain"
property * addOperator;// cptlib/"addOperator"
property * initState;// cptlib/"initState"
property * goal;// cptlib/"goal"
property * createPDDLProblem;// cptlib/"createPDDLProblem"
property * addInit;// cptlib/"addInit"
property * addGoal;// cptlib/"addGoal"
property * openFile;// cptlib/"openFile"
property * readNext;// cptlib/"readNext"
property * parseError;// cptlib/"parseError"
property * readChar;// cptlib/"readChar"
property * currentWord;// cptlib/"currentWord"
property * nextWord;// cptlib/"nextWord"
property * checkWord;// cptlib/"checkWord"
property * checkNext;// cptlib/"checkNext"
property * testNext;// cptlib/"testNext"
property * readBegin;// cptlib/"readBegin"
property * readEnd;// cptlib/"readEnd"
property * testBegin;// cptlib/"testBegin"
property * testEnd;// cptlib/"testEnd"
property * readDomain;// cptlib/"readDomain"
property * readRequirements;// cptlib/"readRequirements"
property * readTypes;// cptlib/"readTypes"
property * readConstants;// cptlib/"readConstants"
property * readPredicates;// cptlib/"readPredicates"
property * readFunctions;// cptlib/"readFunctions"
property * readOperator;// cptlib/"readOperator"
property * readPDDLProblem;// cptlib/"readPDDLProblem"
property * readDomainPDDLProblem;// cptlib/"readDomainPDDLProblem"
property * readObjects;// cptlib/"readObjects"
property * readInitState;// cptlib/"readInitState"
property * readGoal;// cptlib/"readGoal"
property * readAtomList;// cptlib/"readAtomList"
property * readConjunction;// cptlib/"readConjunction"
property * readTermList;// cptlib/"readTermList"
property * readParameters;// cptlib/"readParameters"
property * readDuration;// cptlib/"readDuration"
property * readPrecondition;// cptlib/"readPrecondition"
property * readEffect;// cptlib/"readEffect"
property * readExpression;// cptlib/"readExpression"
property * readAtom;// cptlib/"readAtom"
property * readRational;// cptlib/"readRational"
property * readOperationTerms;// cptlib/"readOperationTerms"
property * createAttributeSpaces;// cptlib/"createAttributeSpaces"
property * createFluent;// cptlib/"createFluent"
property * createFluent2;// cptlib/"createFluent2"
property * makeFluent;// cptlib/"makeFluent"
property * createConstraint;// cptlib/"createConstraint"
property * instantiateOperator;// cptlib/"instantiateOperator"
property * instantiateOperators;// cptlib/"instantiateOperators"
property * addEdels;// cptlib/"addEdels"
property * computeReachability;// cptlib/"computeReachability"
property * computeDurations;// cptlib/"computeDurations"

// module definition 
 void metaLoad();};

extern cptlibClass cptlib;

#endif
