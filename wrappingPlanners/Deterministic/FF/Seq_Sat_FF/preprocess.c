#include "preprocess.h"
#include "output.h"
#include "utility.h"

static float **pair_costs;
/*static float *single_costs;*/

void print_h2_pair(int i, int j);

float get_pair_value(int i, int j) {
  return pair_costs[i][j];
}

float pairwise_eval(int *facts, int num_facts) {

  int i, j;
  float cost = 0;

  for(i = 0; i < num_facts; i++) {
    for(j = i; j < num_facts; j++) {
      cost = MAX(cost, get_pair_value(facts[i], facts[j]));
      if (cost == INFINITY) {
	/*	printf("returning INF as cost, because: "); print_h2_pair(facts[i], facts[j]); printf("\n");*/
	return cost;
      }
    }
  }
  return cost;
}

Bool update_pair(int i, int j, float new_cost) {

  if(LESS(new_cost, pair_costs[i][j])) {
    pair_costs[i][j] = new_cost;
    pair_costs[j][i] = new_cost;
    return TRUE;
  }
  return FALSE;
}

void calculate_h2(State *init) {

  int i, j, k, m, effect;
  float pre_cost, noop_pre_cost;

  Bool done = FALSE;

  static Bool first_call = TRUE;

  if(first_call) {
    /*    single_costs = calloc(gnum_ft_conn, sizeof(float));*/
    pair_costs = calloc(gnum_ft_conn, sizeof(float *));
    for(i = 0; i < gnum_ft_conn; i++) {
      pair_costs[i] = calloc(gnum_ft_conn, sizeof(float));
    }
    first_call = FALSE;
  }
  
  for(i = 0; i < gnum_ft_conn; i++) {
    /*    single_costs[i] = INFINITY;*/
    for(j = 0; j < gnum_ft_conn; j++) {
      pair_costs[i][j] = pair_costs[j][i] = INFINITY;
    }
  }
  
  for(i = 0; i < init->num_F; i++) {
    /*    single_costs[init->F[i]] = 0.0;*/
    for(j = 0; j < init->num_F; j++) {
      pair_costs[init->F[i]][init->F[j]] = pair_costs[init->F[j]][init->F[i]] = 0.0;
    }
  }
   
  while(!done) {
    
    done = TRUE;
    
    for(effect = 0; effect < gnum_ef_conn; effect++) {
      pre_cost = pairwise_eval(gef_conn[effect].PC, gef_conn[effect].num_PC);

      if(pre_cost == INFINITY) {
	continue;
      }
      
      for(j = 0; j < gef_conn[effect].num_A; j++) {
	
	if(update_pair(gef_conn[effect].A[j], 
		       gef_conn[effect].A[j], 
		       PLUS(pre_cost, get_cost(effect)))) {
	  done = FALSE;
	}
	
	for(k = j + 1; k < gef_conn[effect].num_A; k++) {
	  if(update_pair(gef_conn[effect].A[j], gef_conn[effect].A[k], PLUS(pre_cost, get_cost(effect)))) {
	    done = FALSE;
	  }
	}
	
	for(k = 0; k < gnum_ft_conn; k++) {
	  /* no-op situation */
	  if(! effect_deletes_fact(effect, k)) {
	    /* here need to consider cost of achieving k for noop */

	    noop_pre_cost = MAX(pre_cost, pair_costs[k][k]);
	    
	    for(m = 0; m < gef_conn[effect].num_PC; m++) {
	      noop_pre_cost = MAX(noop_pre_cost, get_pair_value(k, gef_conn[effect].PC[m]));
	    }
	    
	    if(update_pair(gef_conn[effect].A[j], k, PLUS(noop_pre_cost, get_cost(effect)))) {
	      done = FALSE;
	    }
	  }
	}
      }
    }
  }
  print_h2_info();
}


void print_h2_pairs(void) {

  int i,j;

  for(i = 0; i < gnum_ft_conn; i++) {
    for(j = 0; j < i; j++) {
      printf("("); print_ft_name(i); printf(", "); print_ft_name(j); printf("): %f\n", pair_costs[i][j]);
    }
  }
}

void print_h2_singles(void) {
  
  int i;

  for(i = 0; i < gnum_ft_conn; i++) {
    print_ft_name(i); printf(": %f\n", pair_costs[i][i]);
  }
}


void print_h2_pair(int i, int j) {
  printf("("); 
  print_ft_name(i); 
  printf(", "); 
  print_ft_name(j); 
  printf(")");

}

void print_h2_info() {
  print_h2_pairs(); printf("\n");
  print_h2_singles(); printf("\n");
}
