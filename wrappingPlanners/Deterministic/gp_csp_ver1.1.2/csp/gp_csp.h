/*
 *  Variable types for  Graphplan. Current temporary setting is that
 *  ACTIVE is variables in the goal level. INACTIVE are variables at lower
 *  level.
 */

#define  BE_ACTIVE	0  
#define	INACTIVE	1

/*
 *  Value types for blocks world planning problems.
 */
#define  NOTHING	0
#define  A_NOOP		1
#define  IS_ACTION	2

/*
 *  Types of constraints for state-based approach.
 */
#define  MUTEX_CONSTRAINT	0
#define  ACTIVITY_CONSTRAINT	1
#define  DELETE_CONSTRAINT      2
#define  FACT_MUTEX_CONSTRAINT  3

#define MAX_ACTION_NODE 5000


typedef struct {
  int first;
} hash_item;


typedef struct item {
  int index;
  int first;
  struct item *next;
} item_s, *item_t;

/*
 * List of function
 */
int gp_csp(int );

void construct_hash(int );
void free_hash();
void put_in_hashtable(int, int );
int get_1st_value_from_hashtable(int );
