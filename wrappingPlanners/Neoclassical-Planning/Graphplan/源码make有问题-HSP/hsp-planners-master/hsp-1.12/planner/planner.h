/* define's */
#define MAXACTIONS           256
#define SIZE                 (MAXATOMS + 2)

/* exit codes */
#define noError              00
#define noMoreMemory         10
#define maxTimeReached       30
#define noRoomOffspring      40
#define noOperator           50
#define frontierOverfill     60
#define badUsage             70
#define maxOperators         80
#define maxAtoms             90
#define unreacheableGoal     100
#define unexpectedAtom       110
#define maxTime              120

/* hash tables */
#define HASHSIZE             5119   /* prime nearest to 5K */
#define ATOMHASHSIZE         1021   /* prime nearest to 1K */

/* critical parameters */
#define MAXPARAMETERS        20
#define MAXATOMS             5000

/* exits */
#define fatal(code)          _fatal( (code), NULL, __FILE__, __LINE__ )
#define fatal1(code,s)       _fatal( (code), (s), __FILE__, __LINE__ )


/* struct's and typedef's */
struct literal_s 
{
  int lit;
} literal_s;

struct cost_s
{
  int level;
  int cost;
} cost_s;

struct operator_s
{
  char name[128];
  int parameters[MAXPARAMETERS];
  int (*function)( register int *, register struct literal_s *, register struct literal_s * );
  int (*heuristicFunction)( register int *, register struct literal_s *, register struct literal_s * );
} operator_s;

struct heuristicOperator_s
{
  int id;
  char name[128];
  int prec[MAXPARAMETERS];
  int add[MAXPARAMETERS];
  int valid;
} heuristicOperator_s;

struct atomHashEntry_s 
{
  char name[128];
  int idx;
  int parameters[MAXPARAMETERS];
  struct atomHashEntry_s *next;
} atomHashEntry_s;


/* extern variables */
extern struct literal_s situation[SIZE];
extern struct literal_s newSituation[SIZE];
extern int curtime;

extern struct operator_s  *operators;

extern int (*actionTable[MAXACTIONS])( register int *, register struct literal_s *, register struct literal_s * );
extern int (*heuristicActionTable[MAXACTIONS])( register int * );
extern int numActions;
extern int **values, **vars;
extern int initialized;
extern int **parents, *parentsSize, *relevantAtoms;


/* extern functions */
extern void setInitialSituation( void );
extern int goalSituation( struct literal_s * );
extern void fillTable( void );
extern void setupHeuristic( void );
extern float heuristicCost( register struct literal_s * );
extern char *buildName( register int * );
extern struct atomHashEntry_s *readAtomHash( register int * );
extern struct atomHashEntry_s *insertAtomHash( register int * );
extern char *atomName( register int * );
extern void instantiateOperators( void (*)( register int *, register int ), int (*)( register int *, register int ) );
extern int stillPossible( register int *, register int );
extern int heuristicStillPossible( register int *, register int );
extern char *operatorName( register int *, register int );
extern void addParents( register int, register int * );
extern void identifyGoalParents( register int * );
extern void _fatal( register int, register char *, register char *, register int );

/*********************************************************/
/*Wrapping hsp*/
int oldmain(int argc, char ** argv);