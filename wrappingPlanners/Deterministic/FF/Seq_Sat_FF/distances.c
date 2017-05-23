#include "distances.h"
#include "ff.h"
#include "utility.h"
#include "memory.h"
#include "preprocess.h"
#include "utility.h"
#include "output.h"

float *a_distance;
float **h2_from_init;

void compute_a_distances() {

  int i,j;

  a_distance = calloc(gnum_ef_conn, sizeof(float));
  State min_a_result;

  printf("Calculating h2 from initial state...");

  calculate_h2(&ginitial_state);

  printf("\tdone\n");

  h2_from_init = calloc(gnum_ft_conn, sizeof(float *));
  for(i = 0; i < gnum_ft_conn; i++) {
    h2_from_init[i] = calloc(gnum_ft_conn, sizeof(float));
    for(j = 0; j < gnum_ft_conn; j++) {
      h2_from_init[i][j] = get_pair_value(i, j);
    }
  }

  make_state( &min_a_result, gnum_ft_conn, gnum_fl_conn );

  for(i = 0; i < gnum_ef_conn; i++) {
    
    printf("making minimal state for operator ");
    print_op_name(gef_conn[i].op); printf("\n");
    
    make_minimal_state(&min_a_result, i);
    calculate_h2(&min_a_result);
    a_distance[i] = pairwise_eval(gflogic_goal, gnum_flogic_goal);
    printf("\tcalculated h2 from minimal_state: %f\n", a_distance[i]);
  }

  for(i = 0; i < gnum_ef_conn; i++) {
    printf("distance for action ");
    print_op_name(gef_conn[i].op);
    printf("= %f\n", a_distance[i]);
  }

  for(i = 0; i < gnum_ft_conn; i++) {
    free(h2_from_init[i]);
  }
  free(h2_from_init);
}

void make_minimal_state(State *S, int effect) {

  int i;
  
  S->num_F = 0;

  for(i = 0; i < gnum_ft_conn; i++) {
    if (! edeletes(effect, i)) {
      add_fact(S, i);
    }
    else {
      printf("\tnot including fact ");
      print_ft_name(i);
      printf(" in minimal state for action ");
      print_op_name(gef_conn[effect].op);
      printf("\n");
    }
  }
}

Bool edeletes(int effect, int fact) {

  int i;
  
  if(effect_deletes_fact(effect, fact)) {
    return TRUE;
  }
  
  for(i = 0; i < gef_conn[effect].num_A; i++) {
    if(h2_from_init[fact][gef_conn[effect].A[i]] == INFINITY) {
      return TRUE;
    }
  }

  for(i = 0; i < gef_conn[effect].num_PC; i++) {
    if(h2_from_init[fact][gef_conn[effect].PC[i]] == INFINITY) {
      if(! effect_adds_fact(effect, fact)) {
	return TRUE;
      }
    }
  }

  return FALSE;
}

float get_goal_distance(int effect) {
  return a_distance[effect];
}
