#include "gp_csp.h"
#include "../graphplan.h"

#include "../Share/global.h"
#include "../Share/csp.h"
#include "../Share/ebl.h"

#define CC_LIMIT  50000000000.0   /* limit on consistency checks */

int num_action;
int num_fact;
static int global_flag = 0;


extern vertex_t list_action[];
extern int num_goals;

extern int total_recorded;
extern int total_prunned;
extern int irrelevant_nogood;

/*
 * Functions to  assign csp_val for each fact, action for the remaining one.
 * This function is modified version of BB's bb_assign_prop() routine.
 */

void
assign_csp_table( layer, flag )
     hashtable_t layer;
     int flag;
{
  vertex_t vert;

  get_next( layer, 0 );
  while( (vert = get_next( layer,1 )) != NULL ) {
    if( vert->needed == 0 )
      continue;

    if( flag ) {
      vert->csp_val = num_fact++;
    } else {
      vert->csp_val = num_action++;
      list_action[vert->csp_val] = vert;
    }
  }
}

int
assign_csp_val( len )
     int len;
{
  int i;

  num_action = 1;
  num_fact = 1;

  for( i = len; i >= 1; i-- )
    assign_csp_table( fact_table[i], 1 );

  for( i = len - 1; i >= 0; i--)
    assign_csp_table( op_table[i], 0 );

  printf("** Number of facts numbered: %d. Number of action: %d**\n", 
	 num_fact - 1, num_action - 1);

  return (num_fact-1);
}

/*
 * initialize() will remove irrelevant facts and actions. Then set the csp_val for
 * all facts and actions. Return the number of relevant facts in the graph.
 */

int
initialize( len )
     int len;
{
  int num_vars;
  
  /* BB's function to remove unneeded vertice */
  remove_unneeded_vertices(len);

  /*
   * Function to assign csp_val for relevant vertice. Modified from bb_assign_prop
   * of Blackbox
   */
  num_vars = assign_csp_val( len );

  return num_vars;
}

/*
 * This function will take as the arguments tables (action and fact) created
 * by Graphplan algorithm, then call appropriate functions to create and solve 
 * and CSP formular.
 */

int
gp_csp(len)
     int len;
{
    CSP_type    *C;
    int         num_vars;
 
    set_limit( CC_LIMIT );
    start_timer();

    printf("Start initialize the problem.......<%d>\n", num_goals);
    num_vars = initialize(len );

    printf("Start generate the CSP formulation ......\n");
    C = generate( num_vars, len );

    printf("Start initialize the EBL stuffs ....... [%d]\n", global_flag);
    initialize_ebl( C, global_flag );
    global_flag = 1;

    printf("Start solving the CSP problem. Initialize time: %7.2f......... \n",
	   elapsed_time() );

    start_timer();
    solve( C );

    free_CSP( C );
    free_hash();

    printf( "checks: %10.0f, visits: %7.0f, solution: %1.0f, time: %7.2f\n",
            checks, visits, solution_count, elapsed_time() );
    printf("------------------------------------------------------------------\n");
    printf( "EBL:    Recorded: %d *** Prunned: %d *** Oversize: %d\n",
	    total_recorded, total_prunned, irrelevant_nogood);
    printf("------------------------------------------------------------------\n");
    // getchar();

    fflush( stdout );

    if( solution_count > 0 )
      free_ebl();

    /*
     * Solution_count == 0: There is no solution.
     * Solution_count != 0: There is some solution.
     */
    return( solution_count );
}
