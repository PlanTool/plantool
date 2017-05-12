#include "global.h"
#include "csp.h"
#include "ebl.h"


/********************** Data structure ************************/
nogood_t nogood_list[N_NOGOOD_LIMIT];
int n_nogood = 0;  /* Number of nogood */

nlist_t  *cl_list; /* Array of linked list of nogoods for each label */
int n_cl; /* Number of compound label */

int *cl_map; /* Array to map from a compound label to an unique index value */

int number_csp_var;

int irrelevant_nogood = 0;
int total_recorded = 0;
int total_prunned = 0;

extern int num_goals;
/********************** End of DS declaration ****************/

/********************** Extern variables from CSP solver ***************/
extern BOOL *instantiated;
extern BOOL **conflicts;
extern int *order;
extern int **checking;
/******************************** End **********************************/

/********************** Extern parameter from graphplan.c *************/
extern int max_nogood_size;
extern int relevance_k;
/**********************************************************************/

#define  map_clabel(x,v)  (cl_map[x] + v)

void record_a_link();
void record_nogood_checking();

/*
 * Function to initialize data structure for EBL
 */
void initialize_ebl( C, flag )
     CSP_type *C;
     int flag;
{
  int i, j;
  nlist_t temp;


  /* 
   * Set up the cl_map array. The compound label <x,v> will be mapped to
   * value :  cl_map[x] + v
   */
  if( flag == 0 ) {  /* First time searching */
    cl_map = (int *) new_calloc( C->n+2, sizeof(int) );
  } else {
    free(cl_map);
    cl_map = (int *) new_calloc( C->n+2, sizeof(int) );
  }

  j = 0;
  for(i = 1; i < C->n+1; i++) {
    cl_map[i] = j;
    j += C->domain_size[i];
  }
  cl_map[i] = j;  /* Upper bound on the number of compound label*/

  number_csp_var = C->n;

  /* Set up the array to hold nogoods list of each c_label */

  if( flag ) {
    /* Copy over the old cl_list 
    for(i = 0; i < n_cl; i++)
      cl_list[i] = NULL;
	*/

	  for( i = 0; i < n_cl; i++ )
   	 while(cl_list[i] != NULL) {
      	temp = cl_list[i];
     	 	cl_list[i] = cl_list[i]->next;
     	 	temp->ng = NULL;
     	 	temp->next = NULL;
     	 	free(temp);
    	}


    /* Create a new cl_list */
    n_cl = j;
    free(cl_list);
    cl_list = (nlist_t *) new_calloc( (n_cl+1), sizeof(nlist_t) );
    for( i = 0; i < n_cl+1; i++ )
      cl_list[i] = NULL;

    /* Free old nogood list */
    for( i = 0; i < n_nogood; i++ )
      if( nogood_list[i] != NULL ) {
	free(nogood_list[i]->item);
	free(nogood_list[i]);
	nogood_list[i] = NULL;
      }

    n_nogood = 0;
  } else {
    n_cl = j;
    cl_list = (nlist_t *) new_calloc( (n_cl+1), sizeof(nlist_t) );
  }

  /* Tracking number for nogood */
  total_recorded = 0;
  total_prunned = 0;
  irrelevant_nogood = 0;
}



/*
 * Set of function to help prune (irrelevance) nogoods
 */
void prune_nogood( cl_index, a_nogood )
     int cl_index;
     nogood_t a_nogood;
{
  nlist_t temp, edge;

  if( cl_list[cl_index]->ng == a_nogood ) {
    temp = cl_list[cl_index];
    cl_list[cl_index] = cl_list[cl_index]->next;
    temp->next = NULL;
    temp->ng = NULL;
    free(temp);

    return;
  }

  for( edge = cl_list[cl_index]; edge->next != NULL; edge = edge->next )
    if( edge->next->ng == a_nogood ) {
      temp = edge->next;
      edge->next = temp->next;
      temp->next = NULL;
      temp->ng = NULL;
      free(temp);
      
      return;
    }
}

/*
 * Function to check the relevance of nogoods and prune them.
 */
void check_nogood_relevant(var, value)
     int var, value;
{
  int label_index = map_clabel(var, value);
  int i, j, index = 0;
  nogood_t nogood, ng_array[10000];
  nlist_t edge;

  for( edge = cl_list[label_index]; edge != NULL; edge = edge->next) {
    nogood = edge->ng;
    nogood->irrel_count++;

    if( nogood->irrel_count > relevance_k ) {
      ng_array[index++] = nogood;
      if( index >= 10000 ) {
	printf("Stop here ...\n");
	getchar();
      }
    }
  }

  for(j = 0; j < index; j++ ) {
    nogood = ng_array[j];
    for( i = 0; i < nogood->size; i++ )
      prune_nogood( map_clabel(nogood->item[i].var, nogood->item[i].value), nogood);
    total_prunned++;

    /* Free the memory */
    i = nogood->index;
    nogood = NULL;
    free(nogood_list[i]->item);
    free(nogood_list[i]);
    nogood_list[i] = NULL;
  }
}

/*
 * Function to increase the relevant of nogood
 */
void inc_nogood_relevant( var, value )
     int var, value;
{
  int label_index = map_clabel(var, value);
  nogood_t nogood;
  nlist_t edge;

  for( edge = cl_list[label_index]; edge != NULL; edge = edge->next ) {
    nogood = edge->ng;
    nogood->irrel_count--;
  }
}


/*
 * Function to record a link between a compound label and a nogood
 * contains that label.
 */
void record_a_link( cl_index, a_nogood )
     int cl_index;
     nogood_t a_nogood;
{
  nlist_t temp;

  temp = (nlist_t) new_calloc( 1, sizeof(nlist_s) );
  temp->ng = a_nogood;
  temp->next = cl_list[cl_index];
  cl_list[cl_index] = temp;
}

/*
 * Function to record a new nogood.
 */
void record_nogood( C, var, level, solution )
     CSP_type *C;
     int var, level;
     int *solution;
{
  int i, k,  cl_index;
  int c_cl[1000], size = 0, delta = 0;
  nogood_t a_nogood;

  if( var <= num_goals )
    delta = 1;

  /* Make a nogood from a variable */
  for( i = 1; order[i] != var; i++ ) {
    if( conflicts[var][order[i]] ) {
      k = order[i];

      if( size > max_nogood_size ) {
	// printf("\n Try to record a nogood with size > NOGOOD_SIZE... Bypass it !!\n");
	irrelevant_nogood++;
	return;
      } else {
	c_cl[size++] = k;
      }
    }
  }

  if( size == 0 )
    return;

  a_nogood = (nogood_t) new_calloc(1, sizeof(nogood_s));
  a_nogood->size = size;
  a_nogood->irrel_count = 0;
  a_nogood->index = n_nogood;
  a_nogood->item = (alabel_t) new_calloc(size, sizeof(alabel_s));
  for( i = 0; i < size; i++ ) {
    a_nogood->item[i].var = c_cl[i];
    a_nogood->item[i].value = solution[c_cl[i]];
  }


  /* Put the newly created nogood into the nogood list */
  if(n_nogood >= N_NOGOOD_LIMIT) {
    printf("Reach the maximum number of nogood. Bypass this one !!!\n");
    return;
  }

  nogood_list[n_nogood++] = a_nogood;
  total_recorded++;

  for(i = 0; i < a_nogood->size; i++) {
    cl_index = map_clabel(a_nogood->item[i].var, a_nogood->item[i].value);
    record_a_link( cl_index, a_nogood );
  }
}


/*
 * Function to free the nogood list, when we found a solution.
 */
void free_ebl( )
{
  int i;
  nlist_t temp;

  for( i = 0; i < n_cl; i++ ) 
    while(cl_list[i] != NULL) {
      temp = cl_list[i];
      cl_list[i] = cl_list[i]->next;
      temp->ng = NULL;
      temp->next = NULL;
      free(temp);
    }
  free(cl_list);
  free(cl_map);

  for( i = 0; i < n_nogood; i++ ) 
	if(nogood_list[i] != NULL) {
    free(nogood_list[i]->item);
    free(nogood_list[i]);
  }
}


/*
 * Function to check if current partial solution is consistent with all the nogoods
 */
int check_nogood( var, level, solution )
     int var, level;
     int *solution;
{
  nlist_t edge;
  nogood_t nogood;
  int label = map_clabel(var, solution[var]), i, v1, v2, flag;

  for( edge = cl_list[label]; edge != NULL; edge = edge->next ) {
    nogood = edge->ng;

    flag = 1;
    for( i = 0; i < nogood->size; i++ ) {
      v1 = nogood->item[i].var;
      v2 = nogood->item[i].value;

      if( (instantiated[v1] == 0) || (solution[v1] != v2 ) ) {
	flag = 0;
	break;
      }
    }

    if( flag ) {
      /* Record a checking to show the reason of failure for this value */
      for( i = 0; i < nogood->size; i++ )
	if( (nogood->item[i].var != var) && (checking[nogood->item[i].var][var] == 0) ) {
	  conflicts[var][nogood->item[i].var] = 1;
	}
      return 1;
    }
  }
  
  return 0;
}
