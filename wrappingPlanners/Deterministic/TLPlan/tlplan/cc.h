// cc.h -- Current Condition Algorithm

CELLP MakeCCTrueForm
(
	BOOL bType,							// type of formula to generate
	CELLP pcForm,						// temporal formula to convert
	BOOL *pbImmutable					// flag indicating whether returned cell can be modified
);
CELLP MakeCCFalseForm
(
	BOOL bType,							// type of formula to generate
	CELLP pcForm,						// temporal formula to convert
	BOOL *pbImmutable					// flag indicating whether returned cell can be modified
);
CELLP MakeCCAtomicForm
(
	BOOL bType,							// type of formula to generate
	CELLP pcForm,						// temporal formula to convert
	BOOL *pbImmutable					// flag indicating whether returned cell can be modified
);
CELLP MakeCCNotForm
(
	BOOL bType,							// type of formula to generate
	CELLP pcForm,						// temporal formula to convert
	BOOL *pbImmutable					// flag indicating whether returned cell can be modified
);
CELLP MakeCCAndForm
(
	BOOL bType,							// type of formula to generate
	CELLP pcForm,						// temporal formula to convert
	BOOL *pbImmutable					// flag indicating whether returned cell can be modified
);
CELLP MakeCCOrForm
(
	BOOL bType,							// type of formula to generate
	CELLP pcForm,						// temporal formula to convert
	BOOL *pbImmutable					// flag indicating whether returned cell can be modified
);
CELLP MakeCCXorForm
(
	BOOL bType,							// type of formula to generate
	CELLP pcForm,						// temporal formula to convert
	BOOL *pbImmutable					// flag indicating whether returned cell can be modified
);
CELLP MakeCCImpliesForm
(
	BOOL bType,							// type of formula to generate
	CELLP pcForm,						// temporal formula to convert
	BOOL *pbImmutable					// flag indicating whether returned cell can be modified
);
CELLP MakeCCIfThenElseForm
(
	BOOL bType,							// type of formula to generate
	CELLP pcForm,						// temporal formula to convert
	BOOL *pbImmutable					// flag indicating whether returned cell can be modified
);
CELLP MakeCCForAllForm
(
	BOOL bType,							// type of formula to generate
	CELLP pcForm,						// temporal formula to convert
	BOOL *pbImmutable					// flag indicating whether returned cell can be modified
);
CELLP MakeCCExistsForm
(
	BOOL bType,							// type of formula to generate
	CELLP pcForm,						// temporal formula to convert
	BOOL *pbImmutable					// flag indicating whether returned cell can be modified
);
CELLP MakeCCExistsXForm
(
	BOOL bType,							// type of formula to generate
	CELLP pcForm,						// temporal formula to convert
	BOOL *pbImmutable					// flag indicating whether returned cell can be modified
);
CELLP MakeCCBindingForm
(
	BOOL bType,							// type of formula to generate
	CELLP pcForm,						// temporal formula to convert
	BOOL *pbImmutable					// flag indicating whether returned cell can be modified
);
CELLP MakeCCNextForm
(
	BOOL bType,							// type of formula to generate
	CELLP pcForm,						// temporal formula to convert
	BOOL *pbImmutable					// flag indicating whether returned cell can be modified
);
CELLP MakeCCEventuallyForm
(
	BOOL bType,							// type of formula to generate
	CELLP pcForm,						// temporal formula to convert
	BOOL *pbImmutable					// flag indicating whether returned cell can be modified
);
CELLP MakeCCAlwaysForm
(
	BOOL bType,							// type of formula to generate
	CELLP pcForm,						// temporal formula to convert
	BOOL *pbImmutable					// flag indicating whether returned cell can be modified
);
CELLP MakeCCUntilForm
(
	BOOL bType,							// type of formula to generate
	CELLP pcForm,						// temporal formula to convert
	BOOL *pbImmutable					// flag indicating whether returned cell can be modified
);
