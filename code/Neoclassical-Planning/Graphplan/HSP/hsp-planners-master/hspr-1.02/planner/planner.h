/*
**
** Universidad Simon Bolivar, 1999 (c)
** Blai Bonet and Hector Geffner. 1999.
**
*/


#define MAXPARAMETERS        20
#define MAXSCHEMA            256



struct iatom_s
{
  int  idx;
  char name[128];
  int  parameters[MAXPARAMETERS];
  struct iatom_s *next;
} iatom_s;
typedef struct iatom_s iatom_t;


struct atom_s 
{
  char val;
} atom_s;
typedef struct atom_s atom_t;


struct cost_s
{
  int level;
  int cost;
  int max;
} cost_s;
typedef struct cost_s cost_t;



extern int numberSchema;
extern int initialized;
extern int operatorPrec[], operatorPrecSize;
extern int operatorAdd[], operatorAddSize;
extern int operatorDel[], operatorDelSize;

extern int level;
extern int **values;
extern int **vars;

extern atom_t staticState[];
extern atom_t staticNewState[];
extern atom_t relevantAtom[];
extern int operatorDetection;

extern int (*preconditionHeuristicOperatorFunctionTable[])( register int * );
extern int (*heuristicOperatorFunctionTable[])( register int * );
extern int (*operatorFunctionTable[])( register int *, register atom_t *, register atom_t * );

extern iatom_t *readAtomHash( register int * );
extern void addParents( register int, register int * );
