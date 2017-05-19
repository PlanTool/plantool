/*********************************************************************
	    NOTE: This version only work for the binary CSP
********************************************************************/

// #define NOGOOD_SIZE 20
// #define NOGOOD_SIZE_LIMIT 50
// #define RELEVANCE_LIMIT  10
#define N_NOGOOD_LIMIT 500000
#define MAX_CSP_VAR 20000
#define MAX_CL 100000


/*
 * Structure to hold the nogood
 */
typedef struct alabel {
  int var;
  int value;
} alabel_s, *alabel_t;

typedef struct nogood {
  alabel_s *item;
  int size;   /* Size of this nogood */
  int irrel_count;
  int index;
} nogood_s, *nogood_t;


/*
 * Structure to hold the list of all nogood associated with a compound label. 
 * Each compound label will be represented by an unique integer value.
 */
typedef struct nlist {
  nogood_t ng;
  struct nlist *next;
} nlist_s, *nlist_t;


/* List of function that will be needed in the GAC-CBJ solver */
void initialize_ebl( CSP_type *, int );
void record_nogood( CSP_type *, int, int,  int* );
int check_nogood( int, int, int * );
void check_nogood_relevant( int, int );
void inc_nogood_relevant( int, int );
void free_ebl();
