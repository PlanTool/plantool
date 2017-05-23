// crchash.h

// global function prototypes

int CRCHashCreate(void);
int CRCHashClear(void);
int CRCHashInsert
(
	LINEARPLANP plpWorld				/* world pointer */
);
LINEARPLANP CRCHashSearch
(
	LINEARPLANP plpWorld				/* world pointer */
);
int CRCHashDelete
(
	LINEARPLANP plpWorld				/* world pointer */
);
int CRCHashCount(void);
void MarkCRCHash(void);

