#ifndef __SEARCHHSP
#define __SEARCHHSP

#define NODEHASHSIZE         5119            /* prime nearest to 5K */
#define ATOMHASHSIZE         1021            /* prime nearest to 1K */
#define BUCKETTABLESIZE      4*1024          //RS: Modified by RS

#define SIZE                 (numberAtoms+1)
#define NODESIZE             (sizeof( node_t ) + claimSize)
//#define NODESIZE             (sizeof( node_t ) + claimSize * sizeof( atom_t ))

#define GBFS    1
#define ASTAR     2
#define GBFS_COMBO 3  //Added by RS
#define GBFS_RELAX 4  //Added by RS

/* exit codes */
#define noError              00
#define noMoreMemory         10
#define maxSchema            20
#define maxAtoms             30
#define noAlgorithm          40
#define noChildren           50

struct node_s
{
	int cost;        
        int h_value;
	int valid;
	int operIndex;
        int bucket;
	int open;	
        int nelems;
        int relaxlevel;
        struct node_s *father;        
        struct node_s *hashNext;
        struct node_s *hashPrev;
        struct node_s *bucketNext;
        struct node_s *bucketPrev;
      	token_list state;
};
/*RS: End */

typedef struct node_s node_t, *node_p;

extern int claimSize;
extern int numberAtoms;


/*RS: Added by RS*/
/* ALTALT */
/* Functions added to process the list of goals and implement Limited */
/* Discrepancy Search in ALTALT */
extern token_list staticGoalState;
extern token_list staticState;
void process_goalState(token_list);
void printState(token_list);
node_t * ILDSntree(node_t *,int,int); //Improved LImited Discrepancy Search DIsabled..
int startILDS(int,token_list,token_list);
node_t * LDS(node_t *,int);
/*RS: End */


void freeLastNodes( register int);
void freeAllSpace( void );
void cleanNodeHash( void );
void cleanLists( void );
node_t * getNode( void );

/*RS: Modified by RS */
/* ALTALT */
/* Function to calculate the heuristic of a set of propositions */
/* and general other functionalities regarding to set of atoms: comparison, printing, initialization, etc */
int stateCmp(token_list,int,register node_t *);
void heuristics( register node_t * );
void printNode(register node_t * );
void initialize( void );
/*RS: End */

unsigned  nodeHashFunction(token_list);
node_t * readNodeHash(token_list,int);
void insertIntoNodeHash(register node_t *);
void removeNodeFromHash( register node_t *);

/*RS: Modified by RS*/
int nodeBucket( register node_t *);
/*RS End */

void setInitialOPEN( register node_t *);
node_t * getFirstOPEN( void );
void removeNodeFromOPEN( register node_t * );
node_t * removeLastFromOPEN( register node_t *);
node_t * removeNodeFromCLOSE( void );

/*RS: Modified by RS*/
/* ALTALT */
/* Core functions to search the regression space and expand nodes during search */
int nodeExpansion( register node_t *, register node_t ***);
int startGreedyBFS(int,token_list,token_list);
node_t * greedyBFS( void );
/*RS: End */

int fatal( register int);

#endif
