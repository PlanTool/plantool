 
#include "plan_reconstruction.h"


Bool reconstruct_plan_dijkstra(Bucket* b, State *goal, State *pred_goal, int pred_g, int pred_h){

  State *states = calloc(MAX_PLAN_LENGTH, sizeof(State));
  State *states_temp = calloc(MAX_PLAN_LENGTH, sizeof(State));
  int k;
  for (k=0; k<MAX_PLAN_LENGTH; k++){
    allocate_state(&states[k]);
    allocate_state(&states_temp[k]);
  }

  initialize_bucket(b, pred_g, pred_h, FALSE);
  printf("\nGoal found! reconstructing it ... \n");
  Bool level_succeeded;

  prune = cost(goal);
  State pred;
  allocate_state(&pred);

  source_to_dest(&states_temp[0], goal);
  source_to_dest(&pred, pred_goal); /* Search for this predecessor */

  int level;
  int j;
  printf("Reading Level: ");
  State *S;
  int place_in_array = 0;
  for (level=pred_g; level>=0; level--){
    level_succeeded = FALSE; 
    
    printf(" %d", level);
    fflush(stdout);
    initialize_bucket(b, level, 0, FALSE);
    while((S=get_next(b))!=0){
      if (same_state(S, &pred)){
	source_to_dest(&states_temp[place_in_array++],&pred );
	source_to_dest(&pred, get_pred(b));

	if (!b->completely_read)
	  closeFile(b->s_file);
	
	break;
      }
    }
  } 
  
  /* Inverse the array */
  place_in_array--;
  level = 0;
  for (j=place_in_array; j >=0 ; j--){
    source_to_dest(&states[level++], &states_temp[j]);
  }
  
  pred_g = place_in_array;

  if (!b->completely_read)
    closeFile(b->s_file);

  printf("  DONE!\n");

  gnum_plan_ops = 0;
  for ( j = 1; j <= pred_g +1; j++ ) {
    gplan_ops[gnum_plan_ops] = states[j].op;
    gop_conn[states[j].op].duration = states[j].stt  - states[j-1].stt;
    gop_conn[states[j].op].cost = states[j].cost  - states[j-1].cost;
    gnum_plan_ops++;
  }

  gop_conn[states[j-1].op].cost = goal->cost  - states[j-2].cost;

#ifdef FREEMEM
  for (k=0; k<MAX_PLAN_LENGTH; k++){
    free_state(&states[k]);
    free_state(&states_temp[k]);
  }
  free_state(&pred);
  free(states);
  free(states_temp);
 
#endif 
 
  return 1;
 
}

Bool reconstruct_plan(Bucket* b, State *goal, State *pred_goal, int pred_g, int pred_h){
  /*  int ops[MAX_PLAN_LENGTH], num_ops;*/
  
  State *states = calloc(MAX_PLAN_LENGTH, sizeof(State));
  int k;
  for (k=0; k<MAX_PLAN_LENGTH; k++){
    allocate_state(&states[k]);
  }

  initialize_bucket(b, pred_g, pred_h, FALSE);
  printf("\nGoal found! reconstructing it ... \n");
  Bool level_succeeded;

  State pred;
  allocate_state(&pred);

  prune = cost(goal);
  /*printf("Goal cost = %f\n", prune);*/
  goal->cost = prune;
  source_to_dest(&states[pred_g + 1], goal);
  source_to_dest(&pred, pred_goal); /* Search for this predecessor */
 
  /*printf("\n***** GOAL STATE *****\n");
    print_State(*goal);*/
  /*  printf("\n***** PREDCESSOR OF GOAL ******\n");
      print_State(*pred_goal);
  */
  int current_h  = pred_h;
  int level;
  int j;
  printf("\nReading Level: ");
  State *S;
  for (level=pred_g; level>=0; level--){
    level_succeeded = FALSE; 

    printf(" %d", level);
    fflush(stdout);
    /* For a generalized implmentation this should be made to zero */
    current_h = 0;

    while(current_h != MAX_H && !level_succeeded){
      initialize_bucket(b, level, current_h, FALSE);
      while((S=get_next(b))!=0){
	if (same_state(S, &pred))
	  {
	    /*printf("\tFound the state .. \n");*/
	    source_to_dest(&states[level],&pred );
	    source_to_dest(&pred, get_pred(b));
	    /*printf("STT till the %d-th action = %f\n", 
	      level, states[level].stt);*/
	    /* The following line is helpful only with EHC.
	       comment out current_h = 0 above for this.*/
	    /*current_h++;*/
	    level_succeeded = TRUE;
		  
	    if (!b->completely_read)
	      closeFile(b->s_file);
	    /* else the file is already closed by read_bucket*/
	    break;
	  }else{
	    level_succeeded = TRUE;
	    /*printf("State[%d] does not match.\n", j);
	      print_State(b->states[j]);*/
	  }
	      
	      
      }
      /* Arriving here means that the search for the pred in 
	 (i, current_h) did not succeed. So we should look 
	 into the next H bucket. */
      /* Technically this is not possible in EHC but then this 
	 implmentation would be consistent with any kind of 
	 solution reconstruction*/
      current_h ++ ;
    }

    /*
      if (!level_succeeded) {
      printf("\nError in Trail Reconstruction .. \n");
      printf("State could not be found in level %d", level);
      return 1;
      }*/
  }

  if (!b->completely_read)
    closeFile(b->s_file);
  printf("\n*-*-* PLAN Successfully Reconstructed *-*-*\n");


  gnum_plan_ops = 0;
  for ( j = 1; j <= pred_g +1; j++ ) {
    /*      printf("==== ");
	    print_op_name(states[j].op);
	    printf("====>\n\n ");
	    printf("State at level %d: ", j);
	    print_State(states[j]);
	    printf("\n\n");*/
    gplan_ops[gnum_plan_ops] = states[j].op;
    gop_conn[states[j].op].duration = states[j].stt  - states[j-1].stt;
    gop_conn[states[j].op].cost = states[j].cost  - states[j-1].cost;
    /*printf("cost at level j = %d = %f \n", j,  gop_conn[states[j].op].cost);*/
    gnum_plan_ops++;
    /*gplan_ops_durations[gnum_plan_ops++] = states[j].stt;*/
    /*printf("STT till the %d-th action = %f\n", j, gplan_ops_durations[gnum_plan_ops-1]);*/
    /*printf("op  =%d \n", states[j].op);*/
  }

  /*  printf("Operators saved!\n");*/
#ifdef FREEMEM
  for (k=0; k<MAX_PLAN_LENGTH; k++){
    free_state(&states[k]);
  }
  free_state(&pred);
  free(states);
  /*printf("Mem freed!\n");*/
#endif 
  /*print_plan();*/
  return 1;
 
}
