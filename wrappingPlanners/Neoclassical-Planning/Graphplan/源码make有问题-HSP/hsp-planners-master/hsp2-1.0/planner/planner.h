/*
**
** Universidad Simon Bolivar, 1999, 2000 (c)
** Blai Bonet and Hector Geffner. 1999, 2000.
**
*/



/*********************************************************
**********************************************************
**
** Macros
**
**/

#define MIN(x,y)             ((x) > (y) ? (y) : (x))
#define MAX(x,y)             ((x) > (y) ? (x) : (y))
#if 0
#define MUTEX(x,y)           ((y) <= (x) ? _mutex[((y)+(((x)*((x)-1))>>1))-1] : _mutex[((x)+(((y)*((y)-1))>>1))-1])
#endif
#define MUTEX(x,y)           _mutex[(MIN(x,y)+((MAX(x,y)*(MAX(x,y)-1))>>1))-1]

#define fatal(code)          _fatal( (code), NULL, __FILE__, __LINE__ )
#define fatal1(code,s)       _fatal( (code), (s), __FILE__, __LINE__ )

#define PLUSSUM(x,y)         (((x) == INT_MAX) || ((y) == INT_MAX) ? INT_MAX : (x) + (y))
#if 0
#define PAIR(x,y)            ((x) <= (y) ? H2Cost[(x)][(y)-(x)] : H2Cost[(y)][(x)-(y)])
#endif
#define PAIR(x,y)            H2Cost[MIN(x,y)][MAX(x,y)-MIN(x,y)]

/* state bits packing */
#define asserted(s,b)        ((s)[(b)/ATOMSPERPACK].pack & (1<<((b)%ATOMSPERPACK)))
#define set(s,b)             ((s)[(b)/ATOMSPERPACK].pack |= (1<<((b)%ATOMSPERPACK)))
#define clear(s,b)           ((s)[(b)/ATOMSPERPACK].pack &= ~(1<<((b)%ATOMSPERPACK)))



/*********************************************************
**********************************************************
**
** Constants
**
**/

#define MAXPARAMETERS        20
#define ATOMSPERPACK         (8*sizeof( unsigned ))
#define MAXATOMPACKS         (1<<16)         /* enough for 4096 * 32 = 131072 atoms */
#define MAXSCHEMA            100000

#define MINBUCKETTABLESIZE   2048
#define NODEHASHSIZE         5119            /* prime nearest to 5K */
#define ATOMHASHSIZE         1021            /* prime nearest to 1K */
#define INCRATE              (1.5)           /* increase rate for dynamic sized structures */

#define SIZE_ATOMS           (1 + numberAtoms)
#define SIZE_PACKS           (1 + numberAtoms / ATOMSPERPACK)
#define NODESIZE             (sizeof( node_t ) + SIZE_PACKS * sizeof( atom_t ))
#define MEGABYTE             (1024*1024)

/* PDDL requirements */
#define REQ_STRIPS           1
#define REQ_EQUALITY         2
#define REQ_TYPING           4
#define REQ_ADL              8

/* search direction */
#define FORWARD              0
#define BACKWARD             1
#define numberDirections     2

/* search algorithm */
#define _BFS                 0
#define _GBFS                1
#define numberAlgorithms     2

/* heuristics */
#define H1PLUS               0
#define H1MAX                1
#define H2PLUS               2
#define H2MAX                3
#define H2MAXP               4
#define numberHeuristics     5

/* constraint types */
#define TIME                 1
#define MEMORY               2

/* exit codes */
#define noError              00
#define noMoreMemory         10
#define maxSchema            20
#define maxAtoms             30
#define noAlgorithm          40
#define noChildren           50
#define optionSyntax         60
#define searchParameter      70



/*********************************************************
**********************************************************
**
** Structures and Typedefs
**
**/

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
  unsigned pack;
} atom_s;
typedef struct atom_s atom_t;


struct node_s
{
  int cost;                               /* cost from root to this node */
  unsigned h1_plus;                       /* value of h^1-plus */
  unsigned h2_plus;                       /* value of h^2-plus */
  unsigned h1_max;                        /* value of h^1-max */
  unsigned h2_max;                        /* value of h^2-max */
  int fvalue;                             /* f-function value */
  int valid;                              /* valid state? */
  int operator;                           /* operator id that leads to this state */
  int bucket;                             /* bucket number of node */
  int open;                               /* true if node in OPEN list */

  struct node_s *father;                  /* link to father nodes */
  struct node_s *hashNext;                /* link into nodeHashTable */
  struct node_s *hashPrev;                /* link into nodeHashTable */
  struct node_s *bucketNext;              /* link into bucketTable */
  struct node_s *bucketPrev;              /* link into bucketTable */

  atom_t *state;                          /* current state (must be the last field)  */
} node_s;
typedef struct node_s node_t;


struct suboperator_s
{
  int precSize;
  int addSize;
  int delSize;
  int *prec;
  int *add;
  int *del;
  struct suboperator_s *next;
} suboperator_s;
typedef struct suboperator_s suboperator_t;


struct operator_s
{
  char *name;
  int valid;
  int father;
  int precSize;
  int addSize;
  int delSize;
  int *prec;
  int *add;
  int *del;
  suboperator_t *suboperators;
} operator_s;
typedef struct operator_s operator_t;


struct cost_s
{
  unsigned long plus;
  unsigned long max;
} cost_s;
typedef struct cost_s cost_t;


struct mutex_s
{
  int x;
  int y;
  struct mutex_s *next;
  struct mutex_s *prev;
} mutex_s;
typedef struct mutex_s mutex_t;


struct mutexSet_s
{
  int x;
  int size;
  struct mutex_s *set;
  struct mutexSet_s *next;
} mutexSet_s;
typedef struct mutexSet_s mutexSet_t;


struct procRegister_s
{
  char *   procedure;
  float    diffTime;
  struct procRegister_s *next;
} procRegister_s;
typedef struct procRegister_s procRegister_t;


struct schedule_s
{
  int constraintType;
  int searchAlgorithm;
  int searchDirection;
  int searchHeuristic;
  unsigned long memory;
  unsigned long time;
  struct schedule_s *next;
} schedule_s;
typedef struct schedule_s schedule_t;



/*********************************************************
**********************************************************
**
** Extern Variables
**
**/

extern int             numberSchema;
extern int             globalInitialized;
extern int             operatorPrec[];
extern int             operatorPrecSize;
extern int             operatorAdd[];
extern int             operatorAddSize;
extern int             operatorDel[];
extern int             operatorDelSize;
extern int **          values;
extern int **          vars;

extern iatom_t *       readAtomHash( register int * );
extern void            addParents( register int, register int * );

extern char *          _low_yyfile;
extern int             _low_requirements;
extern char *          _low_problemName;
extern char *          _low_domainName;
extern char **         _low_schemaName;
extern char **         _low_objectName;
extern char **         _low_predicateName;
extern int             _low_numberPredicates;
extern schema_t *      _low_schemaTable[];
extern int             _low_initialAtoms[];
extern int             _low_goalAtoms[];
extern int             _low_copyGoalAtoms[];
extern suboperator_t * _low_suboperators;
extern int             _low_groundingOperators;
extern int             _low_negatedAtom[];
