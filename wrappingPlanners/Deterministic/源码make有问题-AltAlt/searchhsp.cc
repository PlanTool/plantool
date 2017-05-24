//HSP-r code taken from the original mplementation
//by Blai Bonet and Hector Geffner 1999 (c).
//Extended for the YOCHAN student group in Arizona
//State University to handle complete integration with
//a Planning graph structure.
//YOCHAN group. Arizona State University, 2000.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/mman.h>
#include <limits.h>
#include <ulimit.h>
#include <unistd.h>
#include <assert.h>
#include "alldefs.h"
#include "globals.h"
#include "facts.h"
#include "actions.h"
#include "badgoals.h"
#include "candidates.h"
#include "construction.h"
#include "searchhsp.h"

//Global variables...

int claimSize;
int numberAtoms = 0;
int initialized=0;
int leveloff;
int nodesAvoided=0;

/* for node pool management */
//static int pageSize;

 static node_t *nodePool = NULL;
 static int currentNodePool = -1;
 static int nodePoolClaimSize = 0;
 static int *nodePoolSize;
 static char *nodePoolUsed;
 static int nodePoolTableSize = 0;
 static node_t **nodePoolTable;


/* node hash table */
static node_t *nodeHashTable[NODEHASHSIZE];
static int nodeHashDiameter[NODEHASHSIZE];
static int *nodeHashValues;
static int nodeHashNumElem = 0;

/* for Best-First Search */
static node_t *headOPEN = NULL;
static node_t *tailOPEN = NULL;
static node_t *CLOSE = NULL;
static node_t *firstNodeInBucket[BUCKETTABLESIZE];
static node_t *lastNodeInBucket[BUCKETTABLESIZE];

static int minBucket;
static int maxBucket;

/* problem data */
static int expandedNodes = 0;
static int generatedNodes = 0;


/* some global parameters */
static int verbose;
static int weight;
static int algorithm;


/* RS: added by RS */
/* ALTALT Data*/
token_list staticState;
token_list staticGoalState;
token_list staticInitialState;
int ildsflag=0;
/* RS: End */

/* BM: Added by BM */
/* ALTALT output files */
extern ofstream my_outfile;
extern char my_outfile_name[];
extern char my_buffer[];
/* BM: End */


/* RS: added by RS*/
/* ALTALT */
/* THese functions conform almost all the set up for the regression search of ALTALT */
/* Checking Goal State */
int goalState(token_list state)
{
  for(token_list elem=staticGoalState;elem>0;elem=elem->next)
    {
      if(!in_list(state,elem->info))
	return FALSE;
    }
  return TRUE;
}

/* Checking plan for valid solution, relative solutions are not allowed */
int
checkSolution( register node_t *node )
{
  token_list atom, atomadd, atomdel;
  token_list staticState2, tempstate;
  int nelems;
  /* set initial state */
  staticState2=staticInitialState;
  //cout<<"Checking Solution ..."<<endl;
  /* apply operators */
  while( (node != NULL) && (node->father != NULL) )
    {
      /* check preconditions */
      tempstate=0;
      nelems=0;
      for( atom = action_table[node->operIndex]->get_precs();atom>0;atom=atom->next)
	{
	  if(!in_list(staticState2,atom->info))
	    {
	      return FALSE;
	    }

	}
       
      for(atomdel=staticState2;atomdel>0;atomdel=atomdel->next)
	  if(!in_list(action_table[node->operIndex]->get_dels(),atomdel->info))
	    {
	      tempstate=new token_list_node(atomdel->info,tempstate);
	      nelems++;
	    }
                   
      for(atomadd=action_table[node->operIndex]->get_adds();atomadd>0;atomadd=atomadd->next)
          if(!(in_list(tempstate,atomadd->info)))
	    {
	      tempstate=new token_list_node(atomadd->info,tempstate);
	      nelems++;
	    }

      staticState2=tempstate;
      /* next operator */
      node = node->father;
    }

  /* check original goal state with respect to our static state*/
  return( goalState ( staticState2 ) );
}

/* Printing state, propositions and distance-based values */
void printState(token_list state)
{
	cout<<"State : \n";
	for(;state>0;state=state->next)
	  cout<<"Elem: "<<state->info->get_name()<<"   -Sum H. value:  "<<state->info->getSumH()<<"  -Level value:  "<<state->info->get_when()<<"   MaxValue:  "<<state->info->getMaxV()<<"\n";
}

/* Printing NOde: heuristic values, operator information and number of elements in the node */
void printNode(register node_t *node )
{
  //cout<<"Node Information:  [Hplus= "<<node->h_plus<<"]  [Hmax= "<< node->h_max <<"]  [Hcombo= "<<node->h_combo<<"] [Cost= "<<node->cost<<"]\n";
  cout<<"Node Information: [Hvalue = "<<node->h_value<<"]  [Cost= "<<node->cost<<"]  [Bucket= "<<node->bucket<<"]"<<endl;
  if(node->operIndex>0)
  cout<<"Operator that gives this node: "<<action_table[node->operIndex]->get_name()<<endl;    
  cout<<"Number of atoms in the node: "<<node->nelems<<endl;
    printState( node->state);
}


/* Partitioning a set of propositions such that we can order them using a recursive quick sort algorithm*/
void partition2(int arrayp[MAX_PROPS],int first, int last,int &pivot)
{
	int pivote=arrayp[first];
        int temp;
	int lastp1=first;
	int unknown=first+1;
	for(;unknown<=last;++unknown)
	{
	  	if(fact_table[arrayp[unknown]]->get_when() > fact_table[pivote]->get_when())
		{
			++lastp1;
			//Swap_nodes
                        temp=arrayp[unknown];
                        arrayp[unknown]=arrayp[lastp1];
			arrayp[lastp1]=temp;
		}
	}
        temp=arrayp[first];
        arrayp[first]=arrayp[lastp1];
        arrayp[lastp1]=temp;
	pivot=lastp1;
}

/* Set up for the quick sort algorithm */
void quicksort2(int arrayp[MAX_PROPS], int first, int last)
{
	int pivotindex;
	if(first<last)
	{
	        partition2(arrayp,first,last,pivotindex);
		quicksort2(arrayp,first,pivotindex-1);
		quicksort2(arrayp,pivotindex+1,last);
	}
}

/* Obtaining the relax heuristic for partial constructed planning graphs */
/* IN fact this function can be joined with the one used in complete graphs */
/* but, we think that this is an easier way of organizing things */
int relax_Heuristic2(int arrayp[MAX_PROPS], int numprops)
{
  int temp[MAX_PROPS];
  token_list adds, precs;
  action_list_node * sup_actions;
  
  if(fact_table[arrayp[0]]->get_when()==0) return 0;

  /*Gets any action that support current set of atoms, it does not consider NOOPs */
  for(sup_actions=fact_table[arrayp[0]]->get_achievers(fact_table[arrayp[0]]->get_when());sup_actions->act->is_noop();sup_actions=sup_actions->next);

  /*Gets adds of such action */
  adds=sup_actions->act->get_adds();
  
  int newnprops=0;
  int i;

  /*COnsider only those propositions that are not already present in the current state */
  for(i=0;i<numprops;i++)
    {
         if(!(in_list(adds,fact_table[arrayp[i]])))	   	    
    	   temp[newnprops++]=arrayp[i];
    }


  int found;
  int temptotal=newnprops;
  /*COnstruct new state for regression, relaxed plans do not consider DEL list*/
   for(precs=sup_actions->act->get_precs();precs>0;precs=precs->next)     
   {
     found=0;
     for(int j=0;j<temptotal;j++)
       {
	 if(temp[j]==precs->info->getnum())
	   {
	     found=1;
	     break;
	   }
       }
     if(!found)
       {
	 if(newnprops<MAX_PROPS)
       	  temp[newnprops++]=precs->info->getnum();
	 else
	   {
	     cout<<"Error, increase MAX_PROPS..."<<endl;
	     exit(0);
	   }
       }
     }
   /*Order heuristically the new set, try the best for relaxation*/
   quicksort2(temp,0,newnprops-1);
   /*Return total length of the relaxed plan*/
     return (1 +  relax_Heuristic2(temp,newnprops));            
}
/* RS: End */


/* Function that calculates the max degree of interaction of a set of atoms*/
/* Lazy function mode */
/* THis function is the core of the HPART2 Heuristic*/
int partitionH2(register node_t * node, int * maxdeg)
{
  token_list elem1;
  int arrayp[node->nelems], arrayset[node->nelems];
  int nelems=0, maxdegree=0, maxLevel_ij=0, levS, currentv, highest, result;

  *maxdeg=0;
  for(elem1=node->state;elem1>0;elem1=elem1->next)
    {
      arrayp[nelems]=elem1->info->getnum();
      arrayset[nelems++]=0;
    }
  
  result=0;
  for(int i=0;i<nelems;i++)
    {
      maxdegree=0;
      maxLevel_ij=0;
      levS=0;
      currentv=0;
      highest=i;
      /*After you have found a max degree for a pair of propositions, mark this pair */
      /*in order to avoid considering them again in the computation */
      for(int j=i+1;j<nelems;j++)
	{
	  if(!arrayset[i]&&!arrayset[j])
	    {
	      maxLevel_ij=max(fact_table[arrayp[i]]->get_when(),fact_table[arrayp[j]]->get_when());
	      levS=maxLevel_ij;
	      while(fact_table[arrayp[i]]->is_mutex(levS,fact_table[arrayp[j]]->expos(),fact_table[arrayp[j]]->exmask()))
		{
		  levS++;
		  if(levS==leveloff) 
		    {
		      levS=-1;//Infinite ... 
		      break;
		    }		  
		}
	      if(levS>0)
		{
		  /* Keeping track of the current max degree for the proposition */
		  currentv=levS-maxLevel_ij;
		  if(currentv>maxdegree)
		    {
		      maxdegree=currentv;
		      highest=j;
		    }
		}
	    }
	}
      /*Updates values, if one pair of propositions is found to have a higher degree */
      *maxdeg=max(*maxdeg,maxdegree);
      result+=maxdegree;
      arrayset[i]=1;
      arrayset[highest]=1;
    }
  return result;
}


/* Eager approach for calculation of the Max degree of interaction of a set of atoms */
/* Considers all combinations, keep the highest degree between pair of actions*/
/* It does not care about repetition of sets*/
int partitionH2complete(register node_t * node, int * maxdeg)
{
  token_list elem1;
  int arrayp[node->nelems];
  int nelems=0, maxdegree=0, maxLevel_ij=0, levS, currentv, highest, result;
  *maxdeg=0;

  for(elem1=node->state;elem1>0;elem1=elem1->next)
    {
      arrayp[nelems++]=elem1->info->getnum();
    }
  
  result=0;
  for(int i=0;i<nelems;i++)
    {
      maxdegree=0;
      maxLevel_ij=0;
      levS=0;
      currentv=0;
      highest=i;
      for(int j=0;j<nelems;j++)
	{
	  if(i!=j)
	    {
	      maxLevel_ij=max(fact_table[arrayp[i]]->get_when(),fact_table[arrayp[j]]->get_when());
	      levS=maxLevel_ij;
	      while(fact_table[arrayp[i]]->is_mutex(levS,fact_table[arrayp[j]]->expos(),fact_table[arrayp[j]]->exmask()))
		{
		  levS++;
		  if(levS==leveloff) 
		    {
		      levS=-1;//Infinite ... 
		      break;
		    }		  
		}
	      if(levS>0)
		{
		  currentv=levS-maxLevel_ij;
		  if(currentv>maxdegree)
		    {
		      maxdegree=currentv;
		      highest=j;
		    }
		}
	    }
	}
      *maxdeg=max(*maxdeg,maxdegree);
      result+=maxdegree;
    }
  return result;
}

/* Calculation of heuristic values for Partial COnstructed planning graphs */
/* Again we can put this calculation in only one function with respect to */
/* complete graphs */
void heuristics2( register node_t *node )
{
  
  /* RS: Added by RS */
  int sumN=0;
  int maxlevelp=0; //The max lev(p_n)
  int maxlevelS=0; //max lev(S) where S = {p_n}
  int maxvalue=0;
  int relaxvalue=0;
  token_list elem;
  int arrayp[MAX_PROPS];
  int numfacts=0;
  int sumlevp=0;
  /* RS: End */

  node->h_value=0;
  node->valid = 1;
  
  /* RS: Added by RS */
  /*Different combinations of heuristics types */
  if(HEURISTYPE!=HPART2&&HEURISTYPE!=HPART2C)
  for(elem=node->state;elem>0;elem=elem->next)
  {	
       	 if(HEURISTYPE==HPLUS||HEURISTYPE==HCOMBO||HEURISTYPE==HADJSUM1) sumN=sumN+elem->info->getSumH();  //This was the original...
         if(HEURISTYPE==HCOMBO||HEURISTYPE==HADJSUM1||HEURISTYPE==HADJSUM2||HEURISTYPE==HLEVEL||PACTION)
	   {               
               maxlevelp=max(maxlevelp,elem->info->get_when());
               maxvalue=max(maxvalue,elem->info->getMaxV());
	   }
	 if(HEURISTYPE==HPART1) sumlevp+=elem->info->get_when();
	  
         if(HEURISTYPE==HADJSUM2||HEURISTYPE==HADJSUM2M)
               arrayp[numfacts++]=elem->info->getnum();
  }
  
  if(HEURISTYPE==HADJSUM2||HEURISTYPE==HADJSUM2M)
    {
         quicksort2(arrayp,0,numfacts-1);
         relaxvalue=relax_Heuristic2(arrayp,numfacts);
    }
  
  if(HEURISTYPE==HCOMBO||HEURISTYPE==HADJSUM1||HEURISTYPE==HADJSUM2||HEURISTYPE==HLEVEL||PACTION)
    {
      maxlevelS=maxlevelp;
      while(!all_possible(maxlevelS,node->state))
	{
	  maxlevelS++;
	}
      node->relaxlevel=maxlevelS;
    }
  /*Updating heuristic value of node h(S)*/
  int maxdegree;
  if(HEURISTYPE==HPLUS) node->h_value=sumN;
  else if(HEURISTYPE==HCOMBO) node->h_value=sumN+maxlevelS; 
  else if(HEURISTYPE==HADJSUM1) node->h_value=sumN+maxlevelS-maxlevelp; 
  else if(HEURISTYPE==HADJSUM2) node->h_value=relaxvalue+maxlevelS-maxlevelp; 
  else if(HEURISTYPE==HLEVEL) node->h_value=maxlevelS;
  else if(HEURISTYPE==HPART1) node->h_value=sumlevp;
  else if(HEURISTYPE==HPART2) node->h_value=partitionH2(node,&maxdegree); 
  else if(HEURISTYPE==HPART2C) node->h_value=partitionH2complete(node,&maxdegree)-maxdegree;
  else if(HEURISTYPE==HADJSUM2M) {partitionH2complete(node,&maxdegree); node->h_value=relaxvalue+maxdegree;};

}

/* COmparison of two sets of atoms */
int stateCmp(token_list set1,int nelems, register node_t * node)
{
 	token_list elem1, elem2;
	assert(graphbuilt);
	int equal=0;
	if(nelems!=node->nelems) return TRUE;
	else
	  {
	    token_list set2=node->state;
	    for(elem1=set1;elem1>0;elem1=elem1->next)
	    {
	      equal=0;
	      for(elem2=set2;elem2>0;elem2=elem2->next)
		{
		  if(elem1->info->getnum()==elem2->info->getnum())
		    { 
		      equal=1;
		      break;
		    }
		}
	      if(!equal) return TRUE;
	     }
	  }
	return FALSE;
 };


/* COmparison of two lists of sets of atoms*/
int listCmp(token_list set1,token_list set2)
{
 	token_list elem1, elem2;
	assert(graphbuilt);
	int equal=0;
	int nelems1, nelems2;
	nelems1=0;
	nelems2=0;
	for(elem1=set1;elem1>0;elem1=elem1->next)
	  nelems1++;
        for(elem2=set2;elem2>0;elem2=elem2->next)
	  nelems2++;	    
	if(nelems1!=nelems2) return TRUE;
	else
	  {
	    for(elem1=set1;elem1>0;elem1=elem1->next)
	    {
	      equal=0;
	      for(elem2=set2;elem2>0;elem2=elem2->next)
		{
		  if(elem1->info->getnum()==elem2->info->getnum())
		    { 
		      equal=1;
		      break;
		    }
		}
	      if(!equal) return TRUE;
	     }
	  }
	return FALSE;
 };
/* END OF ALTALT CODE */



/* THIS CODE HAS BEEN ADAPTED FROM HSP-r*/
node_t *
getNode( void )
{
  register int oldSize;
  register node_t *result;
  register node_t **newNodePoolTable;
  register int *newNodePoolSize;
  register char *newNodePoolUsed;

  /* check if we have enough pool areas */
  if( currentNodePool + 1 >= nodePoolTableSize )
    {
      oldSize = nodePoolTableSize;
      nodePoolTableSize = (nodePoolTableSize == 0 ? 1024 : 2 * nodePoolTableSize);
      newNodePoolTable = (node_t **) realloc( nodePoolTable, nodePoolTableSize * sizeof( node_t * ) );
      newNodePoolSize = (int *) realloc( nodePoolSize, nodePoolTableSize * sizeof( int ) );
      newNodePoolUsed = (char *) realloc( nodePoolUsed, nodePoolTableSize * sizeof( char ) );
      if( !newNodePoolTable || !newNodePoolSize || !newNodePoolUsed )
	fatal( noMoreMemory );
	
	 /* prepare new pool areas */
      nodePoolTable = newNodePoolTable;
      nodePoolSize = newNodePoolSize;
      nodePoolUsed = newNodePoolUsed;
      memset( &nodePoolUsed[oldSize], 0, (nodePoolTableSize - oldSize) * sizeof( char ) );
    }

  /* check if we have enough space in pool area */
  if( (nodePool == NULL) || 
      ((char *)nodePool ==
       (char *)((unsigned)nodePoolTable[currentNodePool] + nodePoolSize[currentNodePool])) )
    {
      /* get new pool area */
      ++currentNodePool;
      if( nodePoolUsed[currentNodePool] == 0 )
	{
	  nodePoolClaimSize = (nodePoolClaimSize == 0 ? 1024 : (int) (1.5 * nodePoolClaimSize));
	  nodePoolSize[currentNodePool] = nodePoolClaimSize * NODESIZE;
	  fprintf( stderr, "HEAPMGMT: allocating memory for %d nodes (%d bytes)... ", 
		   nodePoolClaimSize, nodePoolSize[currentNodePool] );
	  fflush( stderr );
	  nodePoolTable[currentNodePool] = (node_t *) malloc( nodePoolSize[currentNodePool] );
	  if( !nodePoolTable[currentNodePool] )
	   fatal( noMoreMemory );
	else
	    {
	      fprintf( stderr, "done!\n" );
	      nodePoolUsed[currentNodePool] = 1;
	    }
	}
      nodePool = &nodePoolTable[currentNodePool][0];
    }

  /* claim node from current pool */
  result = nodePool;
  //Modified after new version...
  /*RS: Added by RS*/
  result->state = (token_list) ((unsigned)&result->state + sizeof( node_t * ));
  /*RS: End */  

  nodePool = (node_t *) ((unsigned)nodePool + NODESIZE);

  /* return */
   return( result );
}

void cleanNodeHash( void )
{
  freeAllSpace();
  memset( nodeHashDiameter, 0, NODEHASHSIZE * sizeof( int ) );
  memset( nodeHashTable, 0, NODEHASHSIZE * sizeof( node_t *) );
}


void
freeLastNodes( register int number )
{ 
  nodePool = (node_t *) ((unsigned)nodePool - (number * NODESIZE));
}


void
freeAllSpace( void )
{
  currentNodePool = -1;
  nodePool = NULL;
}

void
cleanLists( void )
{
  headOPEN = NULL;
  tailOPEN = NULL;
  CLOSE = NULL;
  memset( firstNodeInBucket, 0, BUCKETTABLESIZE * sizeof( node_t * ) );
  memset( lastNodeInBucket, 0, BUCKETTABLESIZE * sizeof( node_t * ) );
}


/*********************************************************
**********************************************************
**
** Node Expansion
**
**/

int nodeBucket( register node_t *node )
{
  register int result;
  if(algorithm==ASTAR)
     result = node->cost + node->h_value;
  else
     result = node->cost + (weight * node->h_value);
  return( result );
}

void setInitialOPEN( register node_t *node )
{
  register int bucket;

  bucket = nodeBucket( node );
  minBucket = maxBucket = bucket;
  node->bucket = bucket;
  node->bucketNext = NULL;
  node->bucketPrev = NULL;
  firstNodeInBucket[bucket] = node;
  lastNodeInBucket[bucket] = node;
  headOPEN = node;
  tailOPEN = node;
}

node_t * getFirstOPEN( void )
{
  return( headOPEN );
}


void removeNodeFromOPEN( register node_t * node )
{
  register int bucket;

  bucket = node->bucket;

  /* link update */
  if( node->bucketPrev != NULL )
    node->bucketPrev->bucketNext = node->bucketNext;
  if( node->bucketNext != NULL )
    node->bucketNext->bucketPrev = node->bucketPrev;

  /* bucket update */
  if( node == firstNodeInBucket[bucket] )
    {
      if( lastNodeInBucket[bucket] != node )
	firstNodeInBucket[bucket] = node->bucketNext;
      else
	{
	  firstNodeInBucket[bucket] = NULL;
	  lastNodeInBucket[bucket] = NULL;
	}
    }
  else if( node == lastNodeInBucket[bucket] )
    {
      lastNodeInBucket[bucket] = node->bucketPrev;
    }

  /* OPEN update */
  if( node == headOPEN )
    {
      headOPEN = node->bucketNext;
      if( headOPEN != NULL )
	minBucket = headOPEN->bucket;
      else
	{
	  minBucket = BUCKETTABLESIZE;
	  maxBucket = 0;
	}
    }
  if( node == tailOPEN )
    {
      tailOPEN = node->bucketPrev;
      if( tailOPEN != NULL )
	maxBucket = tailOPEN->bucket;
      else
	{
	  minBucket = BUCKETTABLESIZE;
	  maxBucket = 0;
	}
    }

  /* CLOSE update */
  node->open = 0;
  node->bucketNext = CLOSE;
  if( CLOSE != NULL )
    CLOSE->bucketPrev = node;
  CLOSE = node;
}


node_t * removeLastFromOPEN( register node_t * father )
{
  register int bucket;
  register node_t * node;

  for( node = tailOPEN; (node != NULL) && (node->father == father); node = node->bucketPrev );
  if( node == NULL )
    return( NULL );
  else
    bucket = node->bucket;

  /* link update */
  if( node->bucketPrev != NULL )
    node->bucketPrev->bucketNext = node->bucketNext;
  if( node->bucketNext != NULL )
    node->bucketNext->bucketPrev = node->bucketPrev;

  /* bucket update */
  if( node == firstNodeInBucket[bucket] )
    {
      if( lastNodeInBucket[bucket] != node )
	firstNodeInBucket[bucket] = node->bucketNext;
      else
	{
	  firstNodeInBucket[bucket] = NULL;
	  lastNodeInBucket[bucket] = NULL;
	}
    }
  else if( node == lastNodeInBucket[bucket] )
    {
      lastNodeInBucket[bucket] = node->bucketPrev;
    }

  /* OPEN update */
  if( node == headOPEN )
    {
      headOPEN = node->bucketNext;
      if( headOPEN != NULL )
	minBucket = headOPEN->bucket;
      else
	{
	  minBucket = BUCKETTABLESIZE;
	  maxBucket = 0;
	}
    }
  if( node == tailOPEN )
    {
      tailOPEN = node->bucketPrev;
      if( tailOPEN != NULL )
	maxBucket = tailOPEN->bucket;
      else
	{
	  minBucket = BUCKETTABLESIZE;
	  maxBucket = 0;
	}
    }

  /* hash update */
  removeNodeFromHash( node );

  /* return */
  return( node );
}


node_t * removeNodeFromCLOSE( void )
{
  register node_t *node;

  node = CLOSE;

  /* CLOSE update */
  CLOSE = CLOSE->bucketNext;
  if( CLOSE != NULL )
    CLOSE->bucketPrev = NULL;

  /* hash update */
  removeNodeFromHash( node );

  /* return */
  return( node );
}




/***********************************************************************
  Node Hash FUnctions...
  
  **************************************************************/

/*RS: Modified by RS*/
/* ALTALT */
unsigned nodeHashFunction(token_list state )
{
  register unsigned val = 0;
  token_list elem;

  assert(initialized);  //If initialized the array of nodeHashValues....
  for( elem =state; elem>0;elem=elem->next )
        val+=nodeHashValues[elem->info->getnum()];
  return( val % NODEHASHSIZE );
}


node_t * readNodeHash(token_list state, int nelems )
{
  register unsigned index;
  register node_t *node;
  index = nodeHashFunction( state );
  for( node = nodeHashTable[index]; node != NULL; node = node->hashNext )
    {
      if(!stateCmp( state,nelems,node))
      return( node );
    }
  return( NULL );
}


void insertIntoNodeHash( register node_t *node )
{
  register unsigned index;
  
  index = nodeHashFunction( node->state );
  node->hashNext = nodeHashTable[index];
  node->hashPrev = NULL;
  nodeHashTable[index] = node;
  if( node->hashNext != NULL )
    node->hashNext->hashPrev = node;
  ++nodeHashNumElem;
  ++(nodeHashDiameter[index]);
}



void removeNodeFromHash( register node_t *node )
{
  register unsigned index;

  index = nodeHashFunction( node->state );
  if( nodeHashTable[index] == node )
    {
      assert( node->hashPrev == NULL );
      nodeHashTable[index] = node->hashNext;
    }
  else
    {
      assert( node->hashPrev != NULL );
      node->hashPrev->hashNext = node->hashNext;
    }

  if( node->hashNext != NULL )
    node->hashNext->hashPrev = node->hashPrev;
}
/* END OF MODIFICATIONS FOR ALTALT */


/*RS: Added by RS*/
/* ALTALT */
/* Additional functions, not necessary (some pruning of states) */
/* Code to check equivalent states in history of the plan */
int in_history(token_list state,register node_t * currentNode)
{
  token_list elemnode, laststate, current_node;
  laststate=state;
  current_node=currentNode->state;
  
  if(currentNode->father==NULL) return FALSE;

  for(elemnode=current_node;elemnode>0;elemnode=elemnode->next)
  {              
     if(!(in_list(laststate,elemnode->info)))      
       return in_history(state,currentNode->father);
  } 
  return TRUE;
}

/* THis action has not been debugged, so it is not fully implemented */
int reverseActions(token_list addsA2, token_list delA2, register node_t * currentNode)
{
  token_list addsA1, delA1;
  if(currentNode->father==NULL) return FALSE;
  addsA1=action_table[currentNode->operIndex]->get_adds();
  delA1=action_table[currentNode->operIndex]->get_dels();
  if(!listCmp(addsA1,delA2))
    {
      if(!listCmp(delA1,addsA2))
	{
	  return TRUE;
	}
    }
  else return reverseActions(addsA2,delA2,currentNode->father);
  return FALSE;
}
 /*RS ends here...*/


/**********************************************************
 NODE EXPANSION.............
**********************************************************/

/*RS: Modified by RS*/
/* ALTALT */
/* Expansion of nodes given a full planning graph */
int nodeExpansion( register node_t *node, register node_t ***result )
{

  register int alpha, child;
  register node_t *tmp;
  static int initialized = 0;
  static node_t **buffer;
  int nelems=0;

/*RS: Added by RS*/  
token_list elemnode;
token_list precelem;
/*RS: End */

// cout<<"Node expansion begins ...with number of operators "<<finacts_at[0]<<"\n";
  if( !initialized )
    {
      initialized = 1;
      buffer = (node_t **) malloc(finacts_at[0] * sizeof( node_t * ) );
      if( !buffer )
	fatal( noMoreMemory );
    }

   ++expandedNodes;
   child = 0;
   buffer[child] = getNode();

   for( alpha = 0; alpha < finacts_at[0];++alpha )
      if( finalAcTable[alpha]->applicable_regression(node->state))
      {
         staticState=0;
	 nelems=0;
         for(elemnode=node->state;elemnode>0;elemnode=elemnode->next)
         {
              if(!(in_list(finalAcTable[alpha]->get_adds(),elemnode->info)))
		{
	          staticState=new token_list_node(elemnode->info,staticState);
		  nelems++;
		}
         } 
         
         for(precelem=finalAcTable[alpha]->get_precs();precelem>0;precelem=precelem->next)
	       {
		 if(!(in_list(staticState,precelem->info)))
		   {
		   staticState=new token_list_node(precelem->info,staticState);
		   nelems++;
		   }
	       }
	         
        	 buffer[child]->state=staticState;  
		 buffer[child]->nelems=nelems;

           //Check mutexes given the information from the graph..
		if((!all_possible(leveloff,buffer[child]->state))||(in_history(buffer[child]->state,node)))
		  {
		    nodesAvoided++;
		    //State is inconsistent so prune it from the search tree...
	     	    continue;
		  }
		//Disable
		//if(in_history(buffer[child]->state,node))
	        // {
		//   nodesAvoided++;
	        //   continue;
	        // }
	    
           //Set is mutex, so prune the node, do not consider it for search.   
	    tmp = readNodeHash( buffer[child]->state, buffer[child]->nelems );

	    //	    other cases 
	    if( (tmp == NULL) ||
		(node->cost + 1 < tmp->cost) )
	      {

		//	cout<<"If tmp ==NULL "<<endl;
                ++generatedNodes;

		// check for tmp in OPEN 
		if( tmp != NULL )
		  {
		    if( verbose >= 4 )
		      {
			if( tmp->open == 0 )
			  fprintf( stderr, "reopening node %p\n", tmp );
			else
			  fprintf( stderr, "updating OPEN node %p\n", tmp );
		      }

		    removeNodeFromHash( tmp );
		    if( tmp->open == 1 )
		      {
			removeNodeFromOPEN( tmp );
		      }
		  }

		buffer[child]->operIndex=finalAcTable[alpha]->get_entry();
		buffer[child]->father = node;
		buffer[child]->cost = node->cost + 1;
		buffer[child]->open = 1;

		// heuristic computation 
		if( tmp == NULL )
		  {
		    heuristics2( buffer[child] );
		  }
		else
		  {
		    buffer[child]->valid = 1;
                    buffer[child]->h_value = tmp->h_value; 
                  }
		//Disable
		if(REVCHECK)
		  {
		    if(reverseActions(finalAcTable[alpha]->get_adds(),finalAcTable[alpha]->get_dels(),node))
		      {
			buffer[child]->h_value+=2;
		      }
		  }
		//node caching and node space allocation 
		insertIntoNodeHash( buffer[child] );
		buffer[++child] = getNode();
	      }
	  }
      

//   free resources and return 
  freeLastNodes( 1 );
  *result = buffer;
//  cout<<"Node Expansion ends..."<<"\n";
  return( child );
}
/*RS: End */

/********************************************************************/

/*NOde expansion for partial constructed planning graphs*/
/*Note: we can again join both expansion functions in only one*/
/*This way is simple, but not pretty*/
int nodeExpansion2( register node_t *node, register node_t ***result )
{

  register int alpha, child;
  register node_t *tmp;
  static int initialized = 0;
  static node_t **buffer;
  int nelems=0;

/*RS: Added by RS*/  
token_list elemnode;
token_list precelem;
/*RS: End */

// cout<<"Node expansion begins ...with number of operators "<<finacts_at[0]<<"\n";
  if( !initialized )
    {
      initialized = 1;
      buffer = (node_t **) malloc(acts_at[node->relaxlevel] * sizeof( node_t * ) );
      if( !buffer )
	fatal( noMoreMemory );
    }

   ++expandedNodes;
   child = 0;
   buffer[child] = getNode();

   for( alpha = 0; alpha < acts_at[node->relaxlevel];++alpha )
   {
     if(!action_table[alpha]->is_noop())
       {
	 if( action_table[alpha]->applicable_regression(node->state))
	   {
	     staticState=0;
	     nelems=0;
	     for(elemnode=node->state;elemnode>0;elemnode=elemnode->next)
	       {
		 if(!(in_list(action_table[alpha]->get_adds(),elemnode->info)))
		   {
		     staticState=new token_list_node(elemnode->info,staticState);
		     nelems++;
		   }
	       } 
         
	     for(precelem=action_table[alpha]->get_precs();precelem>0;precelem=precelem->next)
	       {
		 if(!(in_list(staticState,precelem->info)))
		   {
		   staticState=new token_list_node(precelem->info,staticState);
		   nelems++;
		   }
	       }
	         
        	 buffer[child]->state=staticState;  
		 buffer[child]->nelems=nelems;

           //Check mutexes given the information from the graph..
		if((!all_possible(leveloff,buffer[child]->state))||(in_history(buffer[child]->state,node)))
		  {
		    //State is inconsistent so prune it from the search tree...
		    nodesAvoided++;
	     	    continue;
		  }
		//if(in_history(buffer[child]->state,node))
	        // {
		//   nodesAvoided++;
	        //   continue;
	        // }
	    
           //Set is mutex, so prune the node, do not consider it for search.   
	    tmp = readNodeHash( buffer[child]->state, buffer[child]->nelems );

	    //	    other cases 
	    if( (tmp == NULL) ||
		(node->cost + 1 < tmp->cost) )
	      {

		//	cout<<"If tmp ==NULL "<<endl;
                ++generatedNodes;

		// check for tmp in OPEN 
		if( tmp != NULL )
		  {
		    if( verbose >= 4 )
		      {
			if( tmp->open == 0 )
			  fprintf( stderr, "reopening node %p\n", tmp );
			else
			  fprintf( stderr, "updating OPEN node %p\n", tmp );
		      }

		    removeNodeFromHash( tmp );
		    if( tmp->open == 1 )
		      {
			removeNodeFromOPEN( tmp );
		      }
		  }

		buffer[child]->operIndex=action_table[alpha]->get_entry();
		buffer[child]->father = node;
		buffer[child]->cost = node->cost + 1;
		buffer[child]->open = 1;

		// heuristic computation 
		if( tmp == NULL )
		  {
		    heuristics2( buffer[child] );
		  }
		else
		  {
		    buffer[child]->valid = 1;
                    buffer[child]->h_value = tmp->h_value; 
                   }
		
                if(REVCHECK)
		  {
		    if(reverseActions(action_table[alpha]->get_adds(),action_table[alpha]->get_dels(),node))
		      {
			buffer[child]->h_value+=2;
		      }
		  }
		//node caching and node space allocation 
		insertIntoNodeHash( buffer[child] );
		buffer[++child] = getNode();
	      }
	   }
	  }
       }

//   free resources and return 
  freeLastNodes( 1 );
  *result = buffer;
//  cout<<"Node Expansion ends..."<<"\n";
  return( child );
}
/*RS: End */


/*********************************************************************
  NODE ORDERING...............
  *********************************************************************/
void
insertNodeIntoBucket( register node_t *node, register int bucket )
{
  node->bucket = bucket;
  node->bucketPrev = NULL;
  node->bucketNext = firstNodeInBucket[bucket];
  
  if( firstNodeInBucket[bucket] == NULL )
    lastNodeInBucket[bucket] = node;
  else
    firstNodeInBucket[bucket]->bucketPrev = node;

  firstNodeInBucket[bucket] = node;
}


	
/*RS: Modified by RS*/	
/* ALTALT */
/* COde modified */
void
nodeOrdering( register node_t **buffer, register int size )
{
  register int i, bucket;
  register node_t *invalidNodes, *lastNodeInPrevBucket;

  /* bucket sorting */
  invalidNodes = NULL;
  for( i = 0; i < size; ++i )
   if( buffer[i]->valid == 1 )
      {
	/* registry used buckets */
	bucket = nodeBucket( buffer[i] );
       	assert( (bucket >= 0) && (bucket < BUCKETTABLESIZE) );
       	minBucket = min( minBucket, bucket );
       	maxBucket = max( maxBucket, bucket );
       	insertNodeIntoBucket( buffer[i], bucket );
        //printNode(buffer[i]);
      }
    else
      {
	buffer[i]->bucketNext = invalidNodes;
	invalidNodes = buffer[i];
      }

  /* bucket threading */
  if( firstNodeInBucket[minBucket] != NULL )
    {
      for( bucket = minBucket, lastNodeInPrevBucket = NULL; bucket <= maxBucket; ++bucket )
	if( firstNodeInBucket[bucket] != NULL )
	  {
	    if( lastNodeInPrevBucket != NULL )
	      {
		lastNodeInPrevBucket->bucketNext = firstNodeInBucket[bucket];
		firstNodeInBucket[bucket]->bucketPrev = lastNodeInPrevBucket;
	      }
	    lastNodeInPrevBucket = lastNodeInBucket[bucket];
	  }
    }

  /* compute new openList head and tail */
  headOPEN = firstNodeInBucket[minBucket];
  
  tailOPEN = lastNodeInBucket[maxBucket];
}
/*RS: End */


/***************************************************************************
	SEARCH.....
	
	********************************************************/
	

/*RS: Modified by RS*/
/* ALTALT */
int
startGreedyBFS(int level,token_list initst, token_list goals )
{
  register int rv, bound;
  register node_t *result; 
  register node_t *node;

  int i;                   // BS
  char* mystr;             // BS
  char  local_buffer[200]; // BS
  
  //registerEntry( "startGreedyBFS( void )" );

   numberAtoms=facts_at [level];
   
   initialize();

   
   leveloff=level;  
 
  cleanNodeHash();
  cleanLists();
  
  
  cout<<"\n";
  
  /* initialization */
  node = getNode();
  memset( firstNodeInBucket, 0, BUCKETTABLESIZE * sizeof( node_t * ) );
  memset( lastNodeInBucket, 0, BUCKETTABLESIZE * sizeof( node_t * ) );
  
  /*RS: Added by RS*/ 
  memcpy( node->state, goals, SIZE * sizeof( token_list ) );
  staticInitialState=initst;
  staticGoalState=goals;
    /*RS: End */

  int gelems=0;
  for(token_list elem1=goals;elem1>0;elem1=elem1->next) {gelems++;};

  node->operIndex = -1;
  node->father = NULL;
  node->cost = 0;
  node->nelems=gelems;

 //heuristics( node );
  heuristics2(node);

  insertIntoNodeHash( node );
  setInitialOPEN(node);
  
  /* search */
  cout<<"Heuristic guessing ... "<<node->h_value<<endl;
 if( (result = greedyBFS()) == NULL )
    {
       rv = 0;
       fprintf( stderr, "SOLUTION: no solution found\n" );
       
  /* BM: Added by BM */
       my_outfile.open(my_outfile_name,ios::app | ios::out);
       if(!my_outfile)
	 {
	   cerr << "Can't open " << my_outfile_name << "(in searchhsp.cc) for results!\n";
	   exit(1);
	 };

       my_outfile << "0,";
       my_outfile.close();
       /* BM: End */


    }
  else
    {
     rv = 1;
      bound = result->cost;

      //printNode(result);

      fprintf( stderr, "SOLUTION: solution found (length = %d)\n", bound );
        /* BM: Added */
    // sprintf(my_buffer,"%d,", bound);
     sprintf(my_buffer,"%d,NG:%d,NE:%d",bound,generatedNodes,expandedNodes);
     // strcat(my_buffer, "<Action sequence: ");
     /* BM: End */
      if( verbose >= 1 )
	{
	  while( result != NULL )
	    {
	      if( result->father != NULL )
              {
	       //fprintf( stderr, "  %s\n", operatorTable[result->operator].name );
	       fprintf( stderr, "  %s\n", action_table[result->operIndex]->get_name() );
	       /* BMBS: Added */
	        int parenth=FALSE;
                // BS: had to add new LISP format of "(oper arg1 arg2 ...)
               
	       mystr = action_table[result->operIndex]->get_name();

	       i = 0; 
               local_buffer[i++] = '('; 
	       
               while((*mystr != '(')&&(*mystr!='\0')) {
		 local_buffer[i++] = *mystr;
		 mystr++;
	       }
	       
               if(*(--mystr)==')'){ parenth=TRUE;};

               mystr++; 
                
	       if(!parenth)
		 {	
		   mystr++;
		   local_buffer[i++] = ' ';
		   while(*mystr != ')') 
		   {
		     if(*mystr == ',') 
		   local_buffer[i++] = ' ';
		     else 
		       local_buffer[i++] = *mystr;
		     mystr++;
		   }
		   local_buffer[i++] = ')';
		   local_buffer[i] = '\0';

	       // strcat(my_buffer, action_table[result->operIndex]->get_name());
	       // BM: old format of "oper(arg1, arg2, ...)
	       strcat(my_buffer, local_buffer);
	       strcat(my_buffer, ",");
	       /* BM: End */
		 }
	       else
		 {
		   local_buffer[--i]='(';
		   local_buffer[i++]=')';
		   local_buffer[i]='\0';
		   strcat(my_buffer,local_buffer);
		   strcat(my_buffer,",");
		 }
	     }
	       
                 result = result->father;
	     }
	  gettimeofday(&hsp_tend,0);
	  totalhsp= (hsp_tend.tv_sec - hsp_tstart.tv_sec) * 1000 + (hsp_tend.tv_usec-hsp_tstart.tv_usec)/1000;
	  cout<<"Length of plan ["<<bound<<"], Nodes generated ["<<generatedNodes<<"], Nodes expanded ["<<expandedNodes<<"], Nodes Avoided ["<<nodesAvoided<<"]."<<endl;
	 }
     }
  /* return */
  return( rv );
  
  }
/*RS: End */


/*RS: Modified by RS*/
node_t * greedyBFS( void )
{
  register int i, children, probe;
  register node_t *currentNode, *nextNode;
  node_t **buffer;

  /* get first node */
  probe = 0;
  currentNode = getFirstOPEN();
 
  if(currentNode->h_value==0&&checkSolution(currentNode)) //and checkSolution ...
     return( currentNode );
  
   /* loop */
  while( currentNode != NULL )  
    {
                probe = 0;
          /* print current node */
       if( verbose >= 5 )
	    printNode(currentNode);
      /* expand node */
       /* Given either partial constructed graphs or full graphs*/
       if(PACTION)
        children = nodeExpansion2( currentNode, &buffer);
       else
        children = nodeExpansion( currentNode, &buffer );
      /* print child */
       if( verbose >= 6 )
	for( i = 0; i < children; ++i )
	  if( buffer[i]->valid == 1 )
	      printNode( buffer[i] );

      /* test for goal */
      for( i = 0; i < children; ++i )
       if( (buffer[i]->valid == 1) && (buffer[i]->h_value == 0) && checkSolution(buffer[i])) //and checkSolution...
           return( buffer[i] );

     
      /* delete currentNode from OPEN */
      removeNodeFromOPEN( currentNode );

      /* insert children into OPEN */
      nodeOrdering( buffer, children );

      /* next node: first try a probe */
      nextNode = NULL;

      // [March 9 - I change h_plus to h_combo - Long

      if( !probe )
        {
         //There is not much difference if I uncomment this region... The altalt0.4 version 
         //has not commented this region and it works better for schedule domain not 
         //good with respect to h_plus */
            for( i = 0; i < children; ++i )
              if( (buffer[i]->valid == 1) && (buffer[i]->h_value < currentNode->h_value) )
              {
                 nextNode = buffer[i];
                 break;
              }
        }


      /* next node: if nothing, select by BFS */
      if( nextNode == NULL )
        {
          nextNode = getFirstOPEN();
          probe = 0;
        }
      else
       {
          probe = 1;
        }

      /*
      if( algorithm != ASTAR )
	{
	  for( i = 0; i < children; ++i )
	    if( (buffer[i]->valid == 1) &&
		((nextNode == NULL) ||
		 (buffer[i]->h_plus < nextNode->h_plus) ||
		 ((buffer[i]->h_plus == nextNode->h_plus) && (buffer[i]->h_max < nextNode->h_max))) )
	      nextNode = buffer[i];

	  if( (nextNode != NULL) && 
	      ((nextNode->h_plus > currentNode->h_plus) ||
	      ((nextNode->h_plus == currentNode->h_plus) && (nextNode->h_max >= currentNode->h_max))) )
	    nextNode = NULL;
	}
      */

      /* select node */
      currentNode = nextNode;
      
    }
  return( NULL );
}

/*******************************************************************/



/******************************************************************/

int fatal( register int returnCode)
{
  switch( returnCode )
    {
    case noMoreMemory:
      fprintf( stderr, "ERROR: error :no more memory\n");
      break;
    case maxAtoms:
      fprintf( stderr, "ERROR: error in : MAXATOMS reached. Recompile\n" );
      break;
    case maxSchema:
      break;
    case noAlgorithm:
      fprintf( stderr, "ERROR: error:  No search algotihm specified\n");
      break;
    case noError:
      break;
    }
  fprintf( stderr, "ERROR: fatal error.\n" );
  //printStatistics();
  //flushAllRegisters();
  exit( returnCode );



}


/*RS: Modified by RS*/
/* ALTALT */
/* Regression search initialization */
void initialize( void )
{
  register int i;

  verbose = 1;

  weight = GWEIGHT;
  // algorithm = GBFS;
  algorithm = HEURISTYPE;
  	
    /* nodeHashTable initialization */
  if( !(nodeHashValues = (int *) malloc( SIZE * sizeof( int ) )) )
    fatal( noMoreMemory );
  for( i = 0; i < SIZE; ++i )
    nodeHashValues[i] = lrand48();
  memset( nodeHashDiameter, 0, NODEHASHSIZE * sizeof( int ) );
  memset( nodeHashTable, 0, NODEHASHSIZE * sizeof( node_t *) );
  claimSize = (numberAtoms + 1) + (4 - (numberAtoms + 1) % 4);
  initialized=1; 
 }
/*RS: End */
