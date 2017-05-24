#include <assert.h>

#include "heuristics.h"
#include "utility.h"
#include "output.h"
#include "graph.h"

label *labels;
label **tsp_labels;
label *landmarks;

/* put these outside the function, as eval
 * will check to make sure values were assigned.
 * (we may have unreachable goal atoms)
 */

#define HADD_SUP_BY_INITIAL_STATE -2

static int *earliest_use = NULL;

/* Actions in this label indicate actions applicable
 * in starting state.
 */

static label relaxed_plan;
/* temp label for atoms to be supported */
static label hadd_tbs;
static label* tsp_relaxed_plans;

static int *hadd_supporters;

float *prev_atom_cost;
float *prev_action_cost;

int lnum_level_one_goals;
int *llevel_one_goals;

extern int gnum_tsp_vars;
extern TSP_Var_pointer gtsp_vars[];

int *tsp_start_positions;

/* forward decs */
void print_state( State *S );
void print_helpful();
void print_action_label(label_pointer lp);

void reset_label_info() {

  int i;
  
  gcost = 0.0;
  lnum_level_one_goals = 0;

  set_empty(&relaxed_plan);
 
  for(i = 0; i < gnum_ft_conn; i++) {
    earliest_use[i] = INFINITY;
    gft_conn[i].is_goal = FALSE;
  }

  for(i = 0; i < gnum_tsp_vars; i++) {
    set_empty(&(tsp_relaxed_plans[i]));
    tsp_start_positions[i] = INFINITY;
  }
}

float eval(State *S, int *num_acts, Bool collect_helpful) {

  static Bool first_call = TRUE;

  int i, j, effect, fact, num_real_tsp_actions, first_real_tsp_goal;
  void *ref;

  /*
  printf("-- start --");
  print_state(*S);
  printf("\n-- end --\n");
  print_state(*current_goals);
  */

  if(first_call) {
 
    init_set(&relaxed_plan);
    init_set(&hadd_tbs);
    tsp_relaxed_plans = calloc(gnum_tsp_vars, SET_SIZE);
   
    llevel_one_goals = calloc(gnum_ft_conn, sizeof(int));

    earliest_use = calloc(gnum_ft_conn, sizeof(int));

    labels = calloc(gnum_ft_conn, SET_SIZE);
    for(i = 0; i < gnum_ft_conn; i++) {
      init_set( labels + i );
    }

    tsp_start_positions = calloc(gnum_tsp_vars, sizeof(int));
    tsp_labels = calloc(gnum_tsp_vars, sizeof(set_pointer *));
    for(i = 0; i < gnum_tsp_vars; i++) {
      init_set(&tsp_relaxed_plans[i]);
      tsp_labels[i] = calloc(gnum_ft_conn, SET_SIZE);
      for(j = 0; j < gnum_ft_conn; j++) {
	init_set (&tsp_labels[i][j]);
      }
    }

    gevaluated_states = 0;

    first_call = FALSE;
  }

  /* (re)initialize stuff */

  gevaluated_states++;

  reset_label_info();

  if (gcmd_line.hadd) {
    /* make sure there aren't any helpful actions lying around */
    gnum_H = 0;
    compute_hadd(S);
    for(i = 0; i < gnum_flogic_goal; i++) {
      gcost = PLUS(gcost, prev_atom_cost[gflogic_goal[i]]);
    }
    *num_acts = 0;
    return gcost;
  }

  else if (gcmd_line.hadd_de) {

    compute_hadd(S);

    for(i = 0; i < gnum_flogic_goal; i++) {
      add_elem(&hadd_tbs, gflogic_goal[i]);
      
      if((gft_conn[gflogic_goal[i]].level == 1) && (!gft_conn[gflogic_goal[i]].is_goal)) {
	llevel_one_goals[lnum_level_one_goals++] = gflogic_goal[i];
      }    
      
      gft_conn[gflogic_goal[i]].is_goal = TRUE;

    }

    while(remove_one_and_return(&hadd_tbs, &fact)) {
      if(hadd_supporters[fact] == INFINITY) {
	*num_acts = INFINITY;
	return INFINITY;
      }
      if(hadd_supporters[fact] != HADD_SUP_BY_INITIAL_STATE) {
	add_elem(&relaxed_plan, hadd_supporters[fact]);
	for(i = 0; i < gef_conn[hadd_supporters[fact]].num_PC; i++) {
	  add_elem(&hadd_tbs, gef_conn[hadd_supporters[fact]].PC[i]);
	}
      }
    }

    *num_acts = num_elems(&relaxed_plan);
    
    init_iterator(&relaxed_plan, &ref);
    while(ref) {
      gcost += get_cost( get_val(&relaxed_plan, &ref));
    }

    /*print_action_label(&relaxed_plan);*/
  }       
  
  else if (gcmd_line.hsa) {

    compute_labels(S);
    
    /* and take union over goal atoms. */
    for(i = 0; i < gnum_flogic_goal; i++) {
      if(gft_conn[gflogic_goal[i]].level == INFINITY) {
	*num_acts = INFINITY;
	return INFINITY;
      }
      
      if((gft_conn[gflogic_goal[i]].level == 1) && (!gft_conn[gflogic_goal[i]].is_goal)) {
	llevel_one_goals[lnum_level_one_goals++] = gflogic_goal[i];
      }    
      
      gft_conn[gflogic_goal[i]].is_goal = TRUE;
      
      set_union(&relaxed_plan, &labels[ gflogic_goal[i] ]);
      
      for(j = 0; j < gnum_tsp_vars; j++) {
	set_union(&(tsp_relaxed_plans[j]), &(tsp_labels[j][gflogic_goal[i]]));
      }
    }
    
    *num_acts = num_elems(&relaxed_plan);
  
    init_iterator(&relaxed_plan, &ref);
    while(ref) {
      gcost += get_cost( get_val(&relaxed_plan, &ref));
    }

    for( i = 0; i < gnum_tsp_vars; i++ ) {
      if(num_elems(&(tsp_relaxed_plans[i])) > 0) {
	/* after this call, first_real_goal should have first real xatom in path. */
	gcost += solve_one_tsp(&(tsp_relaxed_plans[i]), tsp_start_positions[i], &num_real_tsp_actions, &first_real_tsp_goal);
	*num_acts += num_real_tsp_actions;
     
	if((first_real_tsp_goal != INFINITY) && !(gft_conn[first_real_tsp_goal].is_goal)) {

	  /* if this is not the case, there is an error in the TSP computation */
	  assert(gft_conn[first_real_tsp_goal].level == 1);

	  llevel_one_goals[lnum_level_one_goals++] = first_real_tsp_goal;
	  gft_conn[first_real_tsp_goal].is_goal = TRUE;
	}
      }
    }
  }

  if(collect_helpful) {    
   
    /* add all level 1 preconds of any action in rel. plan to goals */
    /* for(i = 0; i < num_elems(&relaxed_plan); i++) {*/

    init_iterator(&relaxed_plan, &ref);

    while(ref) {

      effect = get_val(&relaxed_plan, &ref);

      for(j = 0; j < gef_conn[effect].num_PC; j++) {
	fact = gef_conn[effect].PC[j];
	if(LESS(gef_conn[effect].level, earliest_use[fact])) {
	  earliest_use[fact] = gef_conn[effect].level;
	}

	if((gft_conn[fact].level == 1) && (!gft_conn[fact].is_goal)){
	  /* as long as we are not (in tsp mode AND variable is TSP variable) */
	  if (! ( gcmd_line.tsp && (gft_conn[fact].tsp_var != INFINITY)) ) {
	    llevel_one_goals[lnum_level_one_goals++] = fact;
	  }
	}
	gft_conn[fact].is_goal = TRUE;
      }
    }

    collect_H_info_label();
  }

  /* print_helpful();*/
  /* printf("\n\n\n\n");*/

  return gcost;
}

void compute_labels(State *S) {

  /* attempt action this iteration? */
  static Bool *aa = NULL;

  /* label to store preconds */
  static label full_preconds;
  static label *full_preconds_tsp;

  static Bool first_call = TRUE;

  static float *tsp_costs;
  static int *tsp_num_actions;

  int i, j, k, dummy, fact;
  float preconds_cost = 0;
  int preconds_num_actions = 0, tsp_num_acts_temp = 0;
  Bool done = FALSE;
  Bool p = FALSE;

  void *ref;
  
  if(first_call) {

    aa = calloc(gnum_ef_conn, sizeof(Bool));
    init_set(&full_preconds);

    full_preconds_tsp = calloc(gnum_tsp_vars, SET_SIZE);
    for(i = 0; i < gnum_tsp_vars; i++) {
      init_set(&(full_preconds_tsp[i]));
    }

    tsp_costs = calloc(gnum_tsp_vars, sizeof(float));
    tsp_num_actions = calloc(gnum_tsp_vars, sizeof(int));

    prev_atom_cost = calloc(gnum_ft_conn, sizeof(float));
    prev_action_cost = calloc(gnum_ef_conn, sizeof(float));

    gA = ( int * ) calloc( gnum_op_conn, sizeof( int ) );
    gnum_A = 0;
        
    first_call = FALSE;
  }

  /* get rid of old applicable actions */
  for ( i = 0; i < gnum_A; i++ ) {
    gop_conn[gA[i]].is_in_A = FALSE;
  }
  gnum_A = 0;

  /* all atom costs infinite,labels empty */
  for(i = 0; i < gnum_ft_conn; i++) {
    gft_conn[i].level = INFINITY;
    prev_atom_cost[i] = INFINITY;
    set_empty(&(labels[i]));

    /* set tsp labels empty */
    for(j = 0; j < gnum_tsp_vars; j++) {
      set_empty(&tsp_labels[j][i]);
    }
  }
    
  for(i = 0; i < gnum_ef_conn; i++) {
    aa[i] = FALSE;
    if(gef_conn[i].num_PC == 0) {
      /* always applicable */
      aa[i] = TRUE;
      if (! gop_conn[gef_conn[i].op].is_in_A ) {
	gop_conn[gef_conn[i].op].is_in_A = TRUE;
	gA[gnum_A++] = gef_conn[i].op;
      }
    }
    else {
      /* if all preconds are tsp vars, applicable */
      if( gcmd_line.tsp ) {
	for(j = 0; j < gef_conn[i].num_PC; j++) {
	  if(gft_conn[gef_conn[i].PC[j]].tsp_var != INFINITY) {
	    aa[i] = TRUE;
	    break;
	  }
	}
      }
    }
    gef_conn[i].level = INFINITY;
    prev_action_cost[i] = INFINITY;
  }

  /* start atom costs 0, labels already empty */
  for(i = 0; i < S->num_F; i++) {
    gft_conn[S->F[i]].level = 0;
    prev_atom_cost[S->F[i]] = 0;

    for(j = 0; j < gft_conn[S->F[i]].num_PC; j++) {
      aa[gft_conn[S->F[i]].PC[j]] = TRUE;
    }

    if ( gft_conn[S->F[i]].tsp_var != INFINITY) {
      tsp_start_positions[gft_conn[S->F[i]].tsp_var] = S->F[i];
    }
  }

  /* fill in level and prev_atom_cost for tsp_vars,
   * according to new start positions.
   */  
  for( i = 0; i < gnum_tsp_vars; i++) {
    for (j = 0; j < num_elems(&(gtsp_vars[i]->atoms)); j++) {

      fact = gtsp_vars[i]->atoms_reverse_map[j];

      if(tsp_start_positions[i] != fact) {
	add_elem(&(tsp_labels[i][fact]), fact);
      }
      
      prev_atom_cost[fact] = solve_one_tsp(&(tsp_labels[i][fact]),
				      tsp_start_positions[i], &tsp_num_acts_temp, &dummy);
    
      gft_conn[fact].level = tsp_num_acts_temp;
      /*     
      printf("set cost = %f, level = %d for atom ", prev_atom_cost[fact], gft_conn[fact].level);
      print_ft_name( fact );
      printf("\n");*/
    }
  }

  while(! done ) {

    done = TRUE;

    for(i = 0; i < gnum_ef_conn; i++) {
      if(aa[i]) {

	p = FALSE;
	aa[i] = FALSE;

	set_empty(&full_preconds);

	for( j = 0; j < gnum_tsp_vars; j++) {
	  set_empty(&(full_preconds_tsp[j]));
	}

	for( j = 0; j < gef_conn[i].num_PC; j++ ) {
	  if( gft_conn[gef_conn[i].PC[j]].level == INFINITY ) {
	    p = TRUE; 
	    break;
	  } 
	  else {
	    /*	    
	    printf("before: \n");
	    print_set(&full_preconds);
	    print_set(&(labels[gef_conn[i].PC[j]]));*/

	    set_union (&full_preconds, &(labels[gef_conn[i].PC[j]]));
	    /*	    printf("after: \n");
		    print_set(&full_preconds);*/



	    for(k = 0; k < gnum_tsp_vars; k++) {
	      set_union(&full_preconds_tsp[k], &tsp_labels[k][gef_conn[i].PC[j]]);
	    }
	  }
	}

	if(p) { continue; }

	preconds_cost = 0;
	preconds_num_actions = 0;

	/* add non-tsp costs */
	preconds_num_actions = num_elems(&full_preconds);
	/*	for(j = 0; j < num_elems(&full_preconds); j++) {*/

	init_iterator(&full_preconds, &ref);

	while(ref) {
	  preconds_cost += get_cost( get_val(&full_preconds, &ref));
	}

	/* add tsp costs */
	for(j = 0; j < gnum_tsp_vars; j++) {
	  tsp_costs[j] = solve_one_tsp(&(full_preconds_tsp[j]), tsp_start_positions[j], &tsp_num_acts_temp, &dummy);
	  tsp_num_actions[j] = tsp_num_acts_temp;
	  preconds_cost += tsp_costs[j];
	  preconds_num_actions += tsp_num_actions[j];
	}

	if(LESS(preconds_cost, prev_action_cost[i]))/* ||
						       ((preconds_cost == prev_action_cost[i]) && LESS(gef_conn[i].level, preconds_num_actions)))*/ {

	  /* this takes into account normal label + tsp real actions */
	  gef_conn[i].level = preconds_num_actions;

	  if( gef_conn[i].level == 0 ) {
	    if (! gop_conn[gef_conn[i].op].is_in_A ) {
	      gop_conn[gef_conn[i].op].is_in_A = TRUE;
	      gA[gnum_A++] = gef_conn[i].op;
	    }
	  }
	  
	  prev_action_cost[i] = preconds_cost;
	  
	  /* if the effect is part of a tsp var */
	  if(gcmd_line.tsp && (gef_conn[i].tsp_var != INFINITY)) {

	    add_elem(&(full_preconds_tsp[gef_conn[i].tsp_var]), gef_conn[i].A[0]);

	    /* just change the cost of the changed itinerary */
	    preconds_cost -= tsp_costs[gef_conn[i].tsp_var];
	    preconds_num_actions -= tsp_num_actions[gef_conn[i].tsp_var];
	    tsp_costs[gef_conn[i].tsp_var] = solve_one_tsp(&(full_preconds_tsp[gef_conn[i].tsp_var]),
							   tsp_start_positions[gef_conn[i].tsp_var],
							   &tsp_num_acts_temp, &dummy);

	    tsp_num_actions[gef_conn[i].tsp_var] = tsp_num_acts_temp;
	    preconds_cost += tsp_costs[gef_conn[i].tsp_var];
	    preconds_num_actions += tsp_num_actions[gef_conn[i].tsp_var];
	  }
	  else {
	    add_elem(&full_preconds, i);
	    preconds_cost += get_cost(i);
	    preconds_num_actions++;
	  }
	  
	  for(j = 0; j < gef_conn[i].num_A; j++) {
	    if(LESS(preconds_cost, prev_atom_cost[gef_conn[i].A[j]]))/* ||
	       ((preconds_cost == prev_atom_cost[gef_conn[i].A[j]]) 
	       && LESS(gft_conn[gef_conn[i].A[j]].level, preconds_num_actions)))*/ {

	      prev_atom_cost[gef_conn[i].A[j]] = preconds_cost;

	      copy_set(&full_preconds, &(labels[gef_conn[i].A[j]]));

	      for(k = 0; k < gnum_tsp_vars; k++) {
		copy_set(&(full_preconds_tsp[k]), &(tsp_labels[k][gef_conn[i].A[j]]));
	      }

	      gft_conn[gef_conn[i].A[j]].level = gef_conn[i].level + 1;
	      /* print_ft_name(gef_conn[i].A[j]); printf(" at level %d\n", gft_conn[gef_conn[i].A[j]].level);*/
	      
	      for(k = 0; k < gft_conn[gef_conn[i].A[j]].num_PC; k++){
		aa[gft_conn[gef_conn[i].A[j]].PC[k]] = TRUE;
	      }
	    }
	  }
	  done = FALSE;
	}
      }
    }
  }
}

void collect_H_info_label( void )

{

  static Bool first_call = TRUE;
  static int *H, num_H, *D, *eu_O, *sort_eu_O;
  int i, j, k, ft, ef, op, d;

  if ( first_call ) {
    gH = ( int * ) calloc( gnum_op_conn, sizeof( int ) );
    H = ( int * ) calloc( gnum_op_conn, sizeof( int ) );
    D = ( int * ) calloc( gnum_op_conn, sizeof( int ) );
    sort_eu_O = ( int * ) calloc( gnum_op_conn, sizeof( int ) );
    eu_O = ( int * ) calloc( gnum_op_conn, sizeof( int ) );

    gnum_H = 0;
    num_H = 0;
    first_call = FALSE;
  }
  /* get rid of the previous helping actions */
  for ( i = 0; i < gnum_op_conn; i++ ) {
    gop_conn[i].is_in_H = FALSE;
  }

  for(i = 0; i < gnum_op_conn; i++) {
    eu_O[i] = INFINITY;
  }

  num_H = 0;
  /* only interested in level 1 goals for helping actions */
  /*  for ( i = 0; i < lnum_goals_at[1]; i++ ) { */
  for ( i = 0; i < lnum_level_one_goals; i++ ) {
    /*  ft = lgoals_at[1][i]; */
    ft = llevel_one_goals[i];

    for ( j = 0; j < gft_conn[ft].num_A; j++ ) {
      ef = gft_conn[ft].A[j];

      if ( gef_conn[ef].level != 0 ) {
	continue;
      }
      op = gef_conn[ef].op;

      if ( gop_conn[op].is_in_H ) {
	continue;
      }

      if(LESS(earliest_use[ft], eu_O[op])) {
	eu_O[op] = earliest_use[ft];
      }
     
      if(gef_conn[ef].level == 0){
	/* actually include it as a helpful action */
	gop_conn[op].is_in_H = TRUE;
	H[num_H++] = op;
      }
    }
  }


  /* H collected; now order it
   * here: count number of goal- and subgoal facts that
   *       op deletes (with level 0 effects). order less deletes
   *       before more deletes.
   *       start from back of H, to prefer down under
   *       goals to upper goals.
   */
  gnum_H = 0;
  /* for each op */
  for ( i = num_H - 1; i > -1; i-- ) {
  
    d = 0;
    
    ef = gop_conn[H[i]].E[0];
    
    if ( gef_conn[ef].level != 0 ) continue;
    /* count the number of deletes of the op 
     * which are current goals 
     */
    for ( k = 0; k < gef_conn[ef].num_D; k++ ) {
      if ( gft_conn[gef_conn[ef].D[k]].is_goal ) d++;
    }
      
    /* next two loops do an insert sort, 
     * using #goals deleted as the criteria 
     * and now, hopefully, earliest use.
     */ 
    for ( j = 0; j < gnum_H; j++ ) {

      if(D[j] == d) {
	if(sort_eu_O[j] > eu_O[H[i]]) {
	  break;
	}
      }

      if ( D[j] > d ) break;
    }
    
    for ( k = gnum_H; k > j; k-- ) {
      gH[k] = gH[k-1];
      D[k] = D[k-1];
      sort_eu_O[k] = sort_eu_O[k-1];
    }
    gH[j] = H[i];
    D[j] = d;
    sort_eu_O[j] = eu_O[H[i]];
    gnum_H++;
  }

  /* display stuff */
  if ( gcmd_line.display_info == 124 ) {
    printf("\ncollected H: ");
    for ( i = 0; i < gnum_H; i++ ) {
      print_op_name( gH[i] );
      printf("\n              ");
    }
  }
}

void print_action_label(label_pointer lp) {
  int *blah, i;
  int num_acts = get_as_array(lp, &blah);

  for(i = 0; i < num_acts; i++) {
    print_op_name(gef_conn[blah[i]].op); 
    printf("(lev. %d)", gef_conn[blah[i]].level);
    printf(", ");
  }

  printf("\n\n");
  free(blah);
}

void print_landmark(int fact_index) {
  int *blah, i;
  int num_acts = get_as_array(&(landmarks[fact_index]), &blah);

  printf("Landmarks for "); print_ft_name(fact_index); printf(": ");

  for(i = 0; i < num_acts; i++) {
    print_op_name(gef_conn[blah[i]].op); 
    printf(", ");
  }
  printf("\n");
}

void print_all_landmarks() {

  int i;

  for(i = 0; i < gnum_ft_conn; i++) {
    print_landmark(i);
  }
}

void print_tsp_label(label_pointer lp) {
  int *blah, i;
  int num_acts = get_as_array(lp, &blah);

  for(i = 0; i < num_acts; i++) {
    print_ft_name(blah[i]); 
    printf("(lev. %d)", gft_conn[blah[i]].level);
    printf(", ");
  }
  printf("\n\n");
  free(blah);
}

void print_whole_label(int fact_index) {

  int i;
  
  print_action_label( & (labels[fact_index]) );

  for (i = 0; i < gnum_tsp_vars; i++) {
    print_tsp_label ( &(tsp_labels[i][fact_index] ));
  }
}

void print_helpful() {

  int i;

  printf("num helpful = %d\n", gnum_H);

  for(i = 0; i < gnum_H; i++) {
    print_op_name(gH[i]); printf("(lev. %d, cost %4f)", 
				 gef_conn[gop_conn[gH[i]].E[0]].level,
				 get_cost(gop_conn[gH[i]].E[0]));
    printf(", "); 
  }  

  printf("\n\n");
}

void compute_hadd(State *S) {

  /* attempt action this iteration? */
  static Bool *aa = NULL;

  static Bool first_call = TRUE;

  int i, j, k, max_level;
  float preconds_cost = 0;
  Bool done = FALSE;
  Bool p = FALSE;
  
  if(first_call) {

    aa = calloc(gnum_ef_conn, sizeof(Bool));

    prev_atom_cost = calloc(gnum_ft_conn, sizeof(float));
    prev_action_cost = calloc(gnum_ef_conn, sizeof(float));

    gA = ( int * ) calloc( gnum_op_conn, sizeof( int ) );
    gnum_A = 0;

    hadd_supporters = ( int * ) calloc( gnum_ft_conn, sizeof( int ) );
        
    first_call = FALSE;
  }
  
  for ( i = 0; i < gnum_A; i++ ) {
    gop_conn[gA[i]].is_in_A = FALSE;
  }

  gnum_A = 0;

  /* all atom costs infinite,labels empty */
  for(i = 0; i < gnum_ft_conn; i++) {
    prev_atom_cost[i] = INFINITY;
    gft_conn[i].level = INFINITY;
    hadd_supporters[i] = INFINITY;
  }
    
  for(i = 0; i < gnum_ef_conn; i++) {
    if(gef_conn[i].num_PC == 0) {
      aa[i] = TRUE;
      /* always applicable */
      if (! gop_conn[gef_conn[i].op].is_in_A ) {
	gop_conn[gef_conn[i].op].is_in_A = TRUE;
	gA[gnum_A++] = gef_conn[i].op;
      }
    }
    else {
      aa[i] = FALSE;
    }
    prev_action_cost[i] = INFINITY;
    gef_conn[i].level = INFINITY;
  }
  
  /* start atom costs 0, labels already empty */
  for(i = 0; i < S->num_F; i++) {
    prev_atom_cost[S->F[i]] = 0;
    gft_conn[S->F[i]].level = 0;
    hadd_supporters[ S->F[i] ] = HADD_SUP_BY_INITIAL_STATE;

    for(j = 0; j < gft_conn[S->F[i]].num_PC; j++) {
      aa[gft_conn[S->F[i]].PC[j]] = TRUE;
    }
  }

  while(! done ) {

    done = TRUE;

    for(i = 0; i < gnum_ef_conn; i++) {
      if(aa[i]) {

	p = FALSE;
	aa[i] = FALSE;

	preconds_cost = 0.0;
	max_level = 0;

	for( j = 0; j < gef_conn[i].num_PC; j++ ) {
	  if( gft_conn[gef_conn[i].PC[j]].level == INFINITY ) {
	    p = TRUE;
	    break;
	  } 
	  else {
	    preconds_cost = PLUS(prev_atom_cost[ gef_conn[i].PC[j] ], preconds_cost);
	    max_level = MAX(max_level, gft_conn[gef_conn[i].PC[j]].level);
	  }
	}
	
	if(p) { continue; }

	if(LESS(preconds_cost, prev_action_cost[i])) {

	  if( max_level == 0 ) {
	    if (! gop_conn[gef_conn[i].op].is_in_A ) {
	      gop_conn[gef_conn[i].op].is_in_A = TRUE;
	      gA[gnum_A++] = gef_conn[i].op;
	    }
	  }
	  
	  prev_action_cost[i] = preconds_cost;
	  gef_conn[i].level = max_level;
	  
	  preconds_cost += get_cost(i); 
	  	  
	  for(j = 0; j < gef_conn[i].num_A; j++) {
	    
	    if(LESS(preconds_cost, prev_atom_cost[gef_conn[i].A[j]] )) {

	      prev_atom_cost[gef_conn[i].A[j]] = preconds_cost;
	      gft_conn[ gef_conn[i].A[j] ].level = max_level + 1;

	      hadd_supporters[ gef_conn[i].A[j] ] = i;
	      
	      for(k = 0; k < gft_conn[gef_conn[i].A[j]].num_PC; k++){
		aa[gft_conn[gef_conn[i].A[j]].PC[k]] = TRUE;
	      }
	    }
	  }
	  done = FALSE;
	}
      }
    }
  }
}

void compute_landmarks(State *S) {

  static Bool first_call = TRUE;
  int i, j, k, prev_size;

  static Bool *aa = NULL;
  static Bool *processed;
  static label full_preconds;

  Bool applicable;

  if( first_call) {
    landmarks = calloc(gnum_ft_conn, SET_SIZE);
    for(i = 0; i < gnum_ft_conn; i++) {
      init_set( landmarks + i );
    }
    init_set(&full_preconds);
    aa = calloc(gnum_ef_conn, sizeof(Bool));
    processed = calloc(gnum_ft_conn, sizeof(Bool));
    first_call = FALSE;
  }

  for(i = 0; i < gnum_ef_conn; i++) {
    aa[i] = FALSE;
  }

  for(i = 0; i < gnum_ft_conn; i++) {
    processed[i] = FALSE;
    set_empty(&(landmarks[i]));
  }
  
  for(i = 0; i < S->num_F; i++) {
    processed[S->F[i]] = TRUE;
    for(j = 0; j < gft_conn[S->F[i]].num_PC; j++) {
      aa[gft_conn[S->F[i]].PC[j]] = TRUE;
    }
  }
  
  Bool done = FALSE;

  while(!done) {

    done = TRUE;

    for (i = 0; i < gnum_ef_conn; i++) if(aa[i]) {

      applicable = TRUE;
      aa[i] = FALSE;
      set_empty(&full_preconds);

      for( j = 0; j < gef_conn[i].num_PC; j++ ) {
	if( !processed[gef_conn[i].PC[j]]) {
	  applicable = FALSE; 
	  break;
	} 
	else {
	  set_union (&full_preconds, &(landmarks[gef_conn[i].PC[j]]));
	}
      }

      if(! applicable) continue;
      
      add_elem(&full_preconds, i);
      
      for(j = 0; j < gef_conn[i].num_A; j++) {
	
	if(! processed[gef_conn[i].A[j]]) {
	  
	  copy_set(&full_preconds, &landmarks[gef_conn[i].A[j]]);
	  processed[gef_conn[i].A[j]] = TRUE;
	  
	  for(k = 0; k < gft_conn[gef_conn[i].A[j]].num_PC; k++) {
	    /* indicate potential update */
	    aa[gft_conn[gef_conn[i].A[j]].PC[k]] = TRUE;
	  }
	  
	  done = FALSE;
	}
	else {
	  prev_size = num_elems(&landmarks[gef_conn[i].A[j]]);
	  set_intersection(&landmarks[gef_conn[i].A[j]], &full_preconds);
	  
	  /* if the landmark has changed (become smaller, necessarily) */
	  if(prev_size > num_elems(&(landmarks[gef_conn[i].A[j]]))) {
	    for(k = 0; k < gft_conn[gef_conn[i].A[j]].num_PC; k++) {
	      /* indicate potential update */
	      aa[gft_conn[gef_conn[i].A[j]].PC[k]] = TRUE;
	    }
	    done = FALSE;
	  }
	}
      }
    }
  }
}

