/*******************************************************************************
 **
 **   Global Data from Parser
 **
 ******************************************************************************/


EXTERNC char*          _low_yyfile;
EXTERNC char*          _low_domainName;
EXTERNC char*          _low_problemName;
EXTERNC int            _low_requirements;
EXTERNC int**          _low_initialAtoms;
EXTERNC int            _low_goalAtoms[];
EXTERNC int            _low_copyGoalAtoms[];
EXTERNC int            _low_numberSchema;
EXTERNC schema_t*      _low_schemaTable[];
EXTERNC int            _low_groundingOperators;

EXTERNC char**         _low_schemaName;
EXTERNC char**         _low_objectName;
EXTERNC char**         _low_predicateName;

// for operator info extraction
EXTERNC operator_t     _low_operator;
EXTERNC int**          _low_vars;
EXTERNC int**          _low_values;

// initial and goal formulas
EXTERNC formula_t*     structural;
EXTERNC formula_t*     initialSituation;
EXTERNC formula_t*     goalSituation;

EXTERNC map<const char*,idList_t*,ltstr>   objects;


// procedures

extern map<const char*,formula_t*,ltstr> predicates;
extern map<const char*,formula_t*,ltstr> defined;


EXTERNC int            yyparse( void );
EXTERNC void           generateAtoms( void );
EXTERNC void           generateVarsAndValues( void );
EXTERNC void           rescue_later();

EXTERNC bool           operatorAtomsWhen( schema_t *schema, int *parameters, 
					  Act &act, vector<int>* r2a,
					  const std::set<int>& reachable_atoms,
					  const std::set<int>* static_positive_atoms = 0 );
EXTERNC bool           operatorAtoms( schema_t*, int*, std::set<int>&, std::set<int>&, std::set<int>&, 
				      std::set<int>* reachable_atoms = 0, 
				      std::set<int>* neg_prec = 0,
				      std::set<int>* conds = 0 );
//EXTERNC void           operatorAtoms( schema_t*, int*, std::set<int>& );
EXTERNC void           formulaAtoms( formula_t*, int*, std::set<int>& );
EXTERNC void           formulaAtomsPos( formula_t*, int*, int, std::set<int>& );
EXTERNC void           formulaLiterals( formula_t *formula, int *parameters, std::set<int> &atoms );
EXTERNC size_t         mapWhenEffects( formula_t*, int*, std::vector<formula_t*>& );
EXTERNC void           printSchema( FILE *file, schema_t *schema );
EXTERNC void           printSchemaPDDL( FILE *file, schema_t *schema );
EXTERNC void           printFormulaPDDL( ostream& out, formula_t *formula );
EXTERNC void           printInitialPDDL( ostream& out, formula_t *formula );
EXTERNC void           printFormulaPDDLTrans( ostream& out, formula_t *formula, string (*trans)(int) );
EXTERNC void           collectPlainFormulaCertainAtoms( formula_t* formula, std::set<int> &atoms );
EXTERNC void           collectPlainFormulaPosAtoms( formula_t* formula, std::set<int> &atoms );
EXTERNC bool           isComplexFormula( formula_t* formula );
EXTERNC bool           isComplexFormula2( formula_t* formula );
EXTERNC void           collectFormulaUncertainty( formula_t *formula, std::vector<formula_t*>& formulas );
EXTERNC void           collectAllAtoms( formula_t* formula, int* parameters, std::vector<int> &atoms );
EXTERNC bool           isCNF( formula_t *formula, bool accept_or = false );
EXTERNC void           getCNF( formula_t *formula, clauses & c_or, clauses&  c_oneof, 
			       set<int>& literals, bool accept_or = false );

