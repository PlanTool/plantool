#ifndef _GRAPH_H_
#define _GRAPH_H_

#define MAX_TSP_VARS 128

#include "list_set.h"

/****************************
 * TSP struct(s?)           *
 ****************************/

typedef struct _TSP_Var {

  set actions;
  set atoms;

  int* atoms_reverse_map;
  int* actions_reverse_map;
  
  float **distance_matrix;
  int **num_actions_matrix;
  /* to make the indicated transition, we go through
   * the location indicated below first */
  int **next_fact;

} TSP_Var, *TSP_Var_pointer;

void initialize_TSP();
void constructTSPVars();
void floyd_warshall( TSP_Var_pointer );
float solve_one_tsp(set_pointer tsp_label, int start_fact, int *num_real_actions, int *first_real_goal);

void print_TSP_by_index(int);
void print_TSP_variable( TSP_Var_pointer );
void print_TSP_info();



#endif
