/* domain.h */

#ifndef __DOMAIN_H
#define __DOMAIN_H

/* global structure definitions */

/* Selectors */

#define SymbolInfoName(psi)					((psi)->psName)
#define SymbolInfoSymbolType(psi)			((psi)->nSymbolType)
#define SymbolInfoArity(psi)				((psi)->nArity)
#define SymbolInfoEvalType(psi)				((psi)->nEvalType)
#define SymbolInfoFunction(psi)				((psi)->pfFunction)
#define SymbolInfoFormula(psi)				((psi)->pfFormula)

/* Modifiers */

#define SetSymbolInfoName(psi,val)			((psi)->psName=(val))
#define SetSymbolInfoSymbolType(psi,val)	((psi)->nSymbolType=(val))
#define SetSymbolInfoArity(psi,val)			((psi)->nArity=(val))
#define SetSymbolInfoEvalType(psi,val)		((psi)->nEvalType=(val))
#define SetSymbolInfoFunction(psi,val)		((psi)->pfFunction=(val))
#define SetSymbolInfoFormula(psi,val)		((psi)->pfFormula=(val))

/* global data */

extern SYMBOLINFO asiWorldSymbols[MAXSYMBOLS];
extern BOOL abUnRelaxableSymbols[MAXSYMBOLS]; /* array of boolean indicating whether a symbol can be relaxed */
extern BOOL abHSPIgnoredSymbols[MAXSYMBOLS]; /* array of boolean indicating whether a symbol is ignored (cost=0 always) by an HSP heuristic */
extern BOOL abWatchedPreserve[MAXSYMBOLS];  /* array of boolean indicating whether a symbol should be watched because it could violate a heuristic-preserved property */
extern int nNumberOfWorldSymbols;
extern int nNumberOfDescribedSymbols;
extern int nNumberOfCheckedSymbols;		/* number of cycle checked described symbols */
extern BINDINGP pbGlobalVariables;		/* global variable bindings list */
extern CELLP pcSearchGlobalInit;		/* search-global initialization list */

/* global function prototypes */

int StringToSymbolType
(
	char *psString
);
void MarkSymbolInfo
(
	SYMBOLINFOP psi
);
BOOL PredicateQ
(
	SYMBOLINFOP psiSymbolInfo			/* symbol info pointer */
);
BOOL FunctionQ
(
	SYMBOLINFOP psiSymbolInfo			/* symbol info pointer */
);
BOOL ExternalQ
(
	SYMBOLINFOP psiSymbolInfo			/* symbol info pointer */
);
BOOL DescribedQ
(
	SYMBOLINFOP psiSymbolInfo			/* symbol info pointer */
);
BOOL DefinedQ
(
	SYMBOLINFOP psiSymbolInfo			/* symbol info pointer */
);
BOOL GeneratorQ
(
	SYMBOLINFOP psiSymbolInfo			/* symbol info pointer */
);

SYMBOLINFOP GetSymbolInfoPtr
(
	char *psName
);
char *GetSymbolName
(
	int nIndex
);
void InsertSymbolInfo
(
	char *psName,
	int nSymbolType,
	int nArity,
	int nEvalType,
	void (*pfFunction)(void),			/* function pointer (optional) */
	CELLP pcFormula,
	BOOL bRewritable					/* predicate rewritable flag (optional) */
);
CELLP GetDefParameters
(
	SYMBOLINFOP psiSymbolInfo
);
CELLP GetDefLocals
(
	SYMBOLINFOP psiSymbolInfo
);
CELLP GetDefFormula
(
	SYMBOLINFOP psiSymbolInfo
);

#endif /* __DOMAIN_H */
