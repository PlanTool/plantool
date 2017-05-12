#include "external.h"
#include "relax.h"
#include "bucket.h"
#include "file.h"
#include "plan_reconstruction.h"
#include "duplicates_removal.h"
#include "grounded.h"
#include <limits.h>
#include "output.h"
#include "expressions.h"
#include "external_prio_queue.h"

/* The heuristic function. Actual value depends on whether 
 * the helpful action pruning is on or not. */
int (*heuristic)(State*) = NULL;         

/* #define TESTING_EX */

Bool lH=0; /* For helpful action prunning */
Bool sub_optimal_solutions = 1; /* 1 => last best solution can be found in mipsSolution.soln */

/*
 * Asks the user about resuming a previous run. 
 * returns the g-value of the bucket from which the run should be continued. 
 * -1, otherwise. 
 */
int resume_question(int MAX_DEPTH){

  int resume = 0;
  int resume_bucket_g = -1 ;
  /* printf("Do you want to resume the algorithm ? press 0 for No and 1 for yes: ");
     scanf("%d", &resume);*/
  if (!resume){
    /*printf("PLEASE DELETE OLD BUCKETS!!\n");*/
  }else{
    printf("        Enter the G-Value of the bucket to expand: ");
    int ignore = scanf("%d", &resume_bucket_g);
    if (resume_bucket_g >= MAX_DEPTH){
      printf("ERROR!! G-value > MAX_DEPTH = %d", MAX_DEPTH);
      exit(1);
    }
  }
   
   
  return resume_bucket_g;
}

void	print_statistics(int depth, int read_bucket_inserted, 
			 int write_bucket_inserted, int write_bucket_duplicates, 
			 int duplicates_within, int duplicates_toplayers, 
			 double expanded, double upper_bound, 
			 int goal_depth, int elements_written){

  printf("\n\t******************** Depth %d Generated ********************\n", depth);
  printf("\t* Read in depth %d    = %u\n", depth-1, read_bucket_inserted);
  printf("\t* Inserted            = %d\n", write_bucket_inserted);
  printf("\t* Duplicates:          \n");
  printf("\t*             Buffers = %d - \n", write_bucket_duplicates);
  printf("\t*      External Merge = %d - \n", duplicates_within);
  printf("\t*     Previous Layers = %d - \n", duplicates_toplayers);
  printf("\t*     ---------------------------\n");
  printf("\t*  D = %d  *Remaining = %u\n", depth, write_bucket_inserted - write_bucket_duplicates - duplicates_within - duplicates_toplayers);
  printf("\t*     Expanded So Far = %.0f \n", expanded);
  printf("\t*     Best goal cost  = %.2f at depth %d \n", upper_bound, goal_depth);
  
  if (elements_written != (  write_bucket_inserted - 
			     write_bucket_duplicates - 
			     duplicates_within - 
			     duplicates_toplayers)){
    printf("\t*       ERROR: Incorrect number of elements written: It shoud be %d \n", elements_written);
    printf("\t*   Possible cause: MAX_IO_BUFFERS is too low for the final merge!\n ");
    printf("\t*   There are more buffers than %d for final merge\n", MAX_IO_BUFFERS);
    
    /*exit(1);*/
  }
  
  printf("\t************************************************************\n");
}

int get_stt(State* S, int action_taken){
  return S->stt + 
    instanziate_LnfExpNode(S, gop_conn[action_taken].action->lnf_duration);
}


int early_merge(DR *dr, int successors, int g, int h, 
		int *duplicates_within, 
		int *duplicates_toplayers, 
		int depth, int locality){
  
		    
  printf("******************* EARLY MERGE ****************\n");
  printf("%d elements generated. Starting early merge on (%d, %d)\n",
	 successors, g, h);
  
  int elements_written =  
    mergeAndRemoveDuplicates(dr, g, h, 1, duplicates_within);
  
  elements_written = 
    subtractFilesOfAboveLayers(dr, g, h, locality, 
			       duplicates_toplayers, 0, depth);
  printf("************ END OF EARLY MERGE ****************\n");
  return elements_written;
}

int get_applicable_actions(int *actions){
  int act;
  int total_actions;
  if (!lH){
    for (act=0; act<gnum_A; act++)
      actions[act] = gA[act];
    total_actions = gnum_A;
  }else{
    for (act=0; act<gnum_H; act++)
      actions[act] = gH[act]; /* helpful action pruning on */
    total_actions = gnum_H;
  }
  return total_actions;

}
/* TODO: To be implemented properly later. The value should come from the problem */
short get_max_edge_cost (){
  return 4;
}

void remove_old_files(){

  char rm_command[80];
  sprintf(rm_command, "rm %s/*-*-*", PATH);
  int ignore = system(rm_command);
}

/* 
 * Assumptions: 1) Maximum edge cost is known
 *              2) No edge of zero cost -- this condition might not be true.
 */
Bool do_external_dijkstra(void){
  
  static Bool fc = TRUE;   /* fc = first call */
  static State S__;
  external_prio_queue pq;
  int MAX_DEPTH = 100;

  printf("\n**** Doing EXTERNAL DIJKSTRA on Path = %s **** \n", PATH);
  /*printf("\t\tLEGEND: D=Depth; R=Read; W=Write\n");*/

  int resume_g = resume_question(MAX_DEPTH);
  if (resume_g == -1){ remove_old_files(); }

  /* We need to allocate as many buckets as the maximum edge cost 
   *                             + 1 to handle the current bucket*/
  int max_num_of_buckets = get_max_edge_cost() + 1;
  
  int maximum_cost = 1000;
  allocate_pq(&pq, maximum_cost, max_num_of_buckets);

  /* If this is the first call to the function, make the initial state */
  if ( fc ) {
    make_state( &S__, gnum_ft_conn, gnum_fl_conn );
    fc = FALSE;
  }  

  if(lH){
    heuristic = &get_1P_and_H;
  }
  else
    heuristic = &get_1P_and_A;
  
#ifdef TESTING_EX
  if (is_goal(&ginitial_state)){
    printf("************** INITIAL STATE IS A GOAL STATE *************\n");
    printf("Cost of inital state = %f \n", cost(&ginitial_state));
  }
  else
    printf("************ INITIAL STATE IS NOT A GOAL STATE ***********\n");
#endif

  Bucket *plan_bucket = ( Bucket * ) calloc( 1, sizeof( Bucket ) );
  allocate_bucket(plan_bucket, PLAN_BUCKETSIZE);
    
  int action_taken  = -1;
  int *applicable_actions = ( int * ) calloc( gnum_op_conn, sizeof( int ) );
  int total_actions = 0;    
  int act;
  State *S;
  long expanded = 0;   long successors = 0;

  State goal, pred, succ;
  allocate_state(&goal);  allocate_state(&pred);  allocate_state(&succ);

  insert_in_pq(&pq, &ginitial_state, &ginitial_state, -1);
  while((S = del_min(&pq)) != NULL){
    if (is_goal(S)){
      source_to_dest(&goal, S);
      goal.op = action_taken;
      source_to_dest(&pred, get_pred(pq.buckets[pq.dial[(int)S->cost].index_bucket_pool]));
      reconstruct_plan_dijkstra(plan_bucket, &goal, &pred, (int)S->cost, 0);
      printf("Goal Written!!\n");
      return TRUE;
    }
    heuristic(S); /* collect the operators that are applicable */
    total_actions = get_applicable_actions(applicable_actions);
    if (total_actions > 0)
      expanded++;
    for (act = 0; act < total_actions; act++){
      if(result_to_dest(&succ, S, applicable_actions[act])){
	successors ++;
	action_taken = applicable_actions[act];
	succ.cost = cost(&succ);
	insert_in_pq(&pq, &succ, S, action_taken);   
      }
    }
  }
  
  free_pq(&pq);

  return FALSE;
}

typedef struct {
  int expanded;
  int duplicates_within;
  int duplicates_toplayers;
}Statistics;

Bool do_external_branch_and_bound(Bool stop_at_first_solution){

  static Bool fc = TRUE;   /* fc = first call */
  static State S__;
  
  const int MAX_DEPTH = 100000;
  const int h         = 0;

  int         depth = 0;    
  int    goal_depth = -1;
  float upper_bound = 10000000.0;

  
  State goal, pred, succ; 
  /* Statistics *statistics = (Statistics*)calloc(1, sizeof(Statistics));*/
 
  /* For statistics */
  double           expanded = 0.0;
  int            successors = 0;
  int      elements_written = 0;
  int    *duplicates_within = (int*)calloc(1,sizeof(int));
  int *duplicates_toplayers = (int*)calloc(1, sizeof(int));

  printf("\n**** Doing EXTERNAL Branch-and-Bound on Path = %s **** \n", PATH);

  /* If this is the first call to the function, make the initial state */
  if ( fc ) {
    make_state( &S__, gnum_ft_conn, gnum_fl_conn );
    fc = FALSE;
  }  

  int resume_g = resume_question(MAX_DEPTH);
  if (resume_g == -1){remove_old_files(); }
  else depth = resume_g;

  Bucket *read_bucket, *write_bucket, *plan_bucket;
  read_bucket = ( Bucket * ) calloc( 1, sizeof( Bucket ) );
  write_bucket = ( Bucket * ) calloc( 1, sizeof( Bucket ) );
  plan_bucket = ( Bucket * ) calloc( 1, sizeof( Bucket ) );
    

  allocate_bucket(read_bucket, BUCKETSIZE);
  allocate_bucket(write_bucket, BUCKETSIZE);
  allocate_bucket(plan_bucket, PLAN_BUCKETSIZE);

  initialize_bucket(read_bucket, depth, h, TRUE);
  initialize_bucket(write_bucket, depth+1, h, TRUE);


  allocate_state(&goal);
  allocate_state(&pred);
  allocate_state(&succ);

  DR *dr = malloc(sizeof(DR));
  init_DuplicatesRemoval(dr);

  if (resume_g == -1){
    ginitial_state.stt = 0 ;
    insert_in_bucket(read_bucket, &ginitial_state, &ginitial_state,-1);
    flush_bucket(read_bucket);
    read_bucket->inserted = 0;
  }

  if(lH){
    heuristic = &get_1P_and_H;
  }
  else
    heuristic = &get_1P_and_A;
  
  if (is_goal(&ginitial_state)){
      /*  print_State(ginitial_state); */
    upper_bound = cost(&ginitial_state) ;
    goal_depth = 0;
#ifdef TESTING_EX
    printf("************** INITIAL STATE IS A GOAL STATE *************\n");
    printf("Cost of inital state = %f \n", upper_bound);
#endif
    /* exit(1); */
  }
  else{
#ifdef TESTING_EX
    printf("************ INITIAL STATE IS NOT A GOAL STATE ***********\n");
#endif
  }
  int action_taken ;
  int i;
  int *applicable_actions = ( int * ) calloc( gnum_op_conn, sizeof( int ) );
  int total_actions = 0;    
  int act;
  State *S;

  while(depth<MAX_DEPTH){
    *duplicates_within = 0;
    *duplicates_toplayers = 0;
#ifdef TESTING_EX
    printf("\n D=%d:", depth);
#endif
    successors = 0;
    while((S=get_next(read_bucket))!=0){
      heuristic(S);
      for ( i = 0; i < gnum_relevant_fluents; i++ ) {
	  if (S->f_D[i]) { 
	      if (strncmp(grelevant_fluents_name[i],"IS_COST",7)==0) {
		  if (S->f_V[i] > upper_bound) break;
	      }
	  }
      }
      if (i != gnum_relevant_fluents) {
	  continue; 
      }

      total_actions = get_applicable_actions(applicable_actions);

      if (total_actions > 0)
	expanded++;

      for (act = 0; act < total_actions; act++){
	if(result_to_dest(&succ, S, applicable_actions[act])){
	  successors ++;
	  action_taken = applicable_actions[act];
	  succ.cost = cost(&succ);
/*
	  if (succ.cost > upper_bound){continue;}
	  if (succ.cost >= upper_bound && isDurative == TRUE) continue;
*/
	  if (is_goal(&succ) &&  (succ.cost < upper_bound)){
	    prune = upper_bound = succ.cost;
	    goal_depth = depth + 1;
	    printf("\n\t\tBetter GOAL FOUND\n");
	    printf("\t\t    ----------- New Goal Cost = %f\n",succ.cost);
	    source_to_dest(&goal, &succ);
	    goal.op = action_taken;
	    source_to_dest(&pred, S);
	    reconstruct_plan(plan_bucket, &goal, &pred, depth, 0);
	    printf("Goal Written!!\n");
	    if (stop_at_first_solution) return TRUE;
	    if (sub_optimal_solutions) print_plan();
	    if (succ.cost == 0) return TRUE;
	  }
	  insert_in_bucket(write_bucket, &succ, S, action_taken);   
	}
      }
    }
  
    flush_bucket(write_bucket);
    close_bucket(read_bucket);

    depth++;

#ifdef TESTING_EX
    printf("\n****************************\n");
    printf("Total Elements remaining in Layer-%d = %d\n", depth, 
	   write_bucket->inserted - write_bucket->duplicates);
    printf("****************************\n");
#endif
    elements_written = mergeAndRemoveDuplicates(dr, depth, 0, 1,
						duplicates_within);

    elements_written = subtractFilesOfAboveLayers(dr, depth, 0, LOCALITY, 
						  duplicates_toplayers, 0, depth);
#ifdef TESTING_EX
print_statistics(depth, read_bucket->inserted, 
		     write_bucket->inserted, write_bucket->duplicates, 
		     *duplicates_within, 
		     *duplicates_toplayers, expanded, upper_bound, 
		     goal_depth, elements_written);
#endif

    if (!(write_bucket->inserted - 
	  write_bucket->duplicates - 
	  *duplicates_within - *duplicates_toplayers)){
      printf("No Successors Generated! Terminating!\n");
      if (goal_depth >= 0)
	return TRUE;
      else
	return FALSE;
    }
    initialize_bucket(read_bucket, depth, 0, TRUE);
    initialize_bucket(write_bucket, depth+1, 0, TRUE);
    
  }

#ifdef FREEMEM
  free_DuplicatesRemoval(dr);
  free(dr);
  free_bucket(read_bucket);
  free_bucket(write_bucket);
  free(read_bucket);
  free(write_bucket);
  free_state(&goal);
  free_state(&pred);
  free_state(&succ);
  free(applicable_actions);
  free(duplicates_within);
#endif
  if (depth >= MAX_DEPTH && goal_depth >= 0)
    return TRUE;

  return FALSE;
}


void do_external_merge(void){

  int g, h, loc;
  printf("Doing External Merge .. \n");
  printf("Please enter the layer number: G = ");
  int ignore = scanf("%d", &g);
  printf("H = ");
  ignore = scanf("%d", &h);
  printf("Locality ? ");
  ignore = scanf("%d", &loc);

  printf("Merging (%d, %d) and subtracting from past %d layers !",g, h, loc);
  int duplicates_within;
  duplicates_within = 0; 
  int duplicates_toplayers;
  duplicates_toplayers = 0;
    
  DR *dr = malloc(sizeof(DR));
  init_DuplicatesRemoval(dr);
    
  mergeAndRemoveDuplicates(dr, g, h, 1, &duplicates_within);
    
  int elements_remaining = 
    subtractFilesOfAboveLayers(dr, g, h, loc, &duplicates_toplayers, 0, 0);

  free_DuplicatesRemoval(dr);
  free(dr);
    
  printf("Summary: Duplicates in external merge  = %d \n",duplicates_within);
  printf("         Duplicates in previous layers = %d \n",duplicates_toplayers);
  printf("         Elements Remaining            = %d \n",elements_remaining);
  exit(1);
}

Bool do_external_scan(void){
    
  printf("Doing External Scan on the file");
  Bucket *read_bucket = 0;

  allocate_bucket(read_bucket, BUCKETSIZE);    
  initialize_bucket(read_bucket, 22, 0, FALSE);
  /*get_next(read_bucket);*/
  insert_in_bucket(read_bucket, &ginitial_state, &ginitial_state,-1);
  insert_in_bucket(read_bucket, &ginitial_state, &ginitial_state,-1);
  insert_in_bucket(read_bucket, &ginitial_state, &ginitial_state,-1);
    
  /*jumpToMiddle(read_bucket->s_file, 6 ,1461840192);*/
  flush_bucket(read_bucket);
  printf("Writing done! Reading bucket\n");

  initialize_bucket(read_bucket, 22, 0, TRUE);
  get_next(read_bucket);
  jumpToMiddle(read_bucket->s_file, 6 ,1461840192);

  State *S;
  while((S=get_next(read_bucket))!=0){
	
  }

  printf("Bucket read correctly\n");
  exit(1);
}


/*********************************
 * External Breadth-First Search *
 ********************************/

Bool do_external_breadth_first_search(void){
   

  printf("\n**** Doing EXTERNAL Breadth First Search on Path = %s.... **** \n", PATH);
  printf("\t\tLEGEND: D=Depth; R=Read; W=Write\n");
  char command[80];
  sprintf(command, "rm %s/*-*-*", PATH);
    
  /* fc = first call */
  /* If this is the first call to the function, make the initial state */
  static Bool fc = TRUE;
  static State S__;
  if ( fc ) {
    make_state( &S__, gnum_ft_conn, gnum_fl_conn );
    fc = FALSE;
  }

  double expanded = 0.0;
  int successors = 0;
  int elements_written = 0;
  int *duplicates_within = (int*)malloc(sizeof(int));
  *duplicates_within = 0;
  int *duplicates_toplayers = (int*)calloc(1, sizeof(int));


  static int MAX_DEPTH = 40;
  int h = 0, depth = 0;
  lH = FALSE;


  Bucket *read_bucket, *write_bucket, *plan_bucket;

  read_bucket = ( Bucket * ) calloc( 1, sizeof( Bucket ) );
  write_bucket = ( Bucket * ) calloc( 1, sizeof( Bucket ) );
  plan_bucket = ( Bucket * ) calloc( 1, sizeof( Bucket ) );

  allocate_bucket(read_bucket, BUCKETSIZE);
  allocate_bucket(write_bucket, BUCKETSIZE);
  allocate_bucket(plan_bucket, PLAN_BUCKETSIZE);




  int resume_g = resume_question(MAX_DEPTH);
  if (resume_g == -1){ remove_old_files(); }
  else depth = resume_g;

  DR *dr = malloc(sizeof(DR));
  init_DuplicatesRemoval(dr);

  initialize_bucket(read_bucket, depth, h, TRUE);
  initialize_bucket(write_bucket, depth+1, h, TRUE);

  State goal, pred, succ;
  allocate_state(&goal);
  allocate_state(&pred);
  allocate_state(&succ);

  if (resume_g == -1) {
    int ignore = system("rm M-*-*");
    ginitial_state.stt = 0 ;
    insert_in_bucket(read_bucket, &ginitial_state, &ginitial_state,-1);
    flush_bucket(read_bucket);
  }

  float upper_bound = 10000000.0;
  
  if(lH){
    heuristic = &get_1P_and_H;
  }
  else
    heuristic = &get_1P_and_A;
   
  int goal_depth = -1;
    
  if (is_goal(&ginitial_state)){
    printf("************** INITIAL STATE IS A GOAL STATE *************\n");
    upper_bound = cost(&ginitial_state) ;
    goal_depth = 0;
    printf("Cost of inital state = %f \n", upper_bound);
  }
  else
    printf("************ INITIAL STATE IS NOT A GOAL STATE ***********\n");
    

  int action_taken;
  int *applicable_actions = ( int * ) calloc( gnum_op_conn, sizeof( int ) );
  int total_actions = 0;    
  int act;

  State *S;
   
  while(depth<50){
    *duplicates_within = 0;
    *duplicates_toplayers = 0;
    /*printf("\n D=%d:", depth);*/
    successors = 0;
    while((S=get_next(read_bucket))!=0){

      heuristic(S); /* ignore the return value */
      total_actions = get_applicable_actions(applicable_actions);
      if (total_actions > 0)
	expanded++;
      for (act = 0; act < total_actions; act++){
	if(result_to_dest(&succ, S, applicable_actions[act])){
	  successors ++;
	  action_taken = applicable_actions[act];
	  succ.stt = get_stt(S, action_taken);
	  succ.cost = cost(&succ);
	  if (succ.cost >= upper_bound && isDurative == TRUE)continue;

	  if (is_goal(&succ) &&  (succ.cost < upper_bound)){
	    upper_bound = succ.cost;
	    goal_depth = depth + 1;
	    printf("\n\t\tBetter GOAL FOUND\n");
	    printf("    ----------- New Goal Cost = %f\n",succ.cost);
	    source_to_dest(&goal, &succ);
	    goal.op = action_taken;
	    source_to_dest(&pred, S);
	    reconstruct_plan(plan_bucket, &goal, &pred, depth, 0);
	    printf("Goal Written!!\n");
	    print_plan();

	    if (succ.cost == 0) return TRUE;
	  }
	  if (succ.cost > upper_bound){
	    /*printf("x=%f, ", gcost);*/
	    continue;
	  }
	  insert_in_bucket(write_bucket, &succ, S, action_taken);    
	  if (successors % (EARLY_MERGE_BUFFERS * BUCKETSIZE) == 0){
	    flush_bucket(write_bucket);
	    elements_written = early_merge(dr, successors, 
					   write_bucket->g, write_bucket->h, 
					   duplicates_within, duplicates_toplayers,
					   depth, EARLY_MERGE_LOCALITY);
	    init_bucket_file_only(write_bucket);
	  }
	}  
      }   
    }

    flush_bucket(write_bucket);
    depth++;
    close_bucket(read_bucket);

    printf("\n****************************\n");
    printf("Total Elements remaining in Layer-%d = %d\n", depth, write_bucket->inserted - write_bucket->duplicates);
    printf("****************************\n");
    elements_written = mergeAndRemoveDuplicates(dr, depth, 0, 1,
						duplicates_within);

    elements_written = subtractFilesOfAboveLayers(dr, depth, 0, LOCALITY, 
						  duplicates_toplayers, 0, depth);

    print_statistics(depth, read_bucket->inserted, 
		     write_bucket->inserted, write_bucket->duplicates, 
		     *duplicates_within, 
		     *duplicates_toplayers, expanded, upper_bound, 
		     goal_depth, elements_written);

    if (!(write_bucket->inserted - 
	  write_bucket->duplicates - 
	  *duplicates_within - *duplicates_toplayers)){
      printf("No Successors Generated! Terminating!\n");
      if (goal_depth >= 0)
	return TRUE;
      else
	return FALSE;
    }
    initialize_bucket(read_bucket, depth, 0, TRUE);
    initialize_bucket(write_bucket, depth+1, 0, TRUE);
    
  }

#ifdef FREEMEM
  free_DuplicatesRemoval(dr);
  free(dr);
  free_bucket(read_bucket);
  free_bucket(write_bucket);
  free(read_bucket);
  free(write_bucket);
  free_state(&goal);
  free_state(&pred);
  free_state(&succ);
  free(applicable_actions);
  free(duplicates_within);
#endif
  if (depth >= MAX_DEPTH && goal_depth >= 0)
    return TRUE;

  return FALSE;
}


/***********************************
 * External Enforced Hill Climbing *
 **********************************/


Bool do_external_enforced_hill_climbing(void){
  /*do_best_first_search();*/
  int resume, resume_bucket_g, resume_bucket_h;
  printf("Do you want to resume the ehc algorithm ? press 0 for No and 1 for yes: ");
  int ignore = scanf("%d", &resume);

  if (!resume){
    printf("Deleting already existing buckets .. ");
    ignore = system("rm *-*-*");
    printf("done.\n");
  }else{

    printf("        Enter the G-Value of the bucket: ");
    ignore = scanf("%d", &resume_bucket_g);
    printf("        Enter the H-Value of the bucket: ");
    ignore = scanf("%d", &resume_bucket_h);
  }
  /*system("rm M-*-*");*/

  /* fc = first call */
  static Bool fc = TRUE;
  static State S__;
  static int MAX_DEPTH = 2000;
  int i, h = 0, op, depth = 0;
  lH = FALSE;
  Bool IMPROVED = FALSE;
  Bool SET = FALSE;

  if ( fc ) {
    make_state( &S__, gnum_ft_conn, gnum_fl_conn );
    fc = FALSE;
  }
  Bucket *read_bucket = 0;
  Bucket *write_bucket = 0;
  Bucket *temp_bucket = 0;;

  allocate_bucket(read_bucket, BUCKETSIZE);
  allocate_bucket(write_bucket, BUCKETSIZE);    
    
  if (SET){
    allocate_bucket(temp_bucket, BUCKETSIZE);
    initialize_bucket(temp_bucket, depth+1, h, TRUE);
  }

  if(lH){
    heuristic = &get_1P_and_H;
  }
  else
    heuristic = &get_1P_and_A;

    
  if (resume){

    h = resume_bucket_h;
    depth  = resume_bucket_g;
  }else{
    h = heuristic(&ginitial_state);
  }
	
  int hval_initial_state = h;
  /*h = get_1P_and_A( &ginitial_state );*/

  printf("\nH-Value of Initial State = %d\n", h);
  /*print_State(ginitial_state);*/
  /*exit(1);*/

  initialize_bucket(read_bucket, depth, h, TRUE);
  initialize_bucket(write_bucket, depth+1, h, TRUE);

  /*printf("********** INITIAL State *********\n");
    print_State(ginitial_state); */

  /* temp = get_1P_and_A( &ginitial_state );
     printf("2::temp = %d; gnum_A = %d\n", temp, gnum_A);*/
  /* insert the initial state */

  int minH = h;
  if (!resume){
    insert_in_bucket(read_bucket, &ginitial_state, &ginitial_state, -1);
    flush_bucket(read_bucket);
  }
  /*int temp = 0;*/

  State goal, pred;
  allocate_state(&goal);
  allocate_state(&pred);
    
  printf("\n************* Doing EXTERNAL Enforced Hill Climbing .... ********** \n");
  if (lH) printf("\t\tUSING Helpful Action Pruning\n");
  else printf("\tNOT USING Helpful Action Pruning\n");

  printf("\t\tLEGEND: D=Depth; R=Read; W=Write\n");

  /*printf("\n*-*-*  Doing EXTERNAL EHC Search *-*-*\n");*/
  printf("\n\t\t%4d cueing into ", minH);
  int local_depth = 1;
  int action_taken = 0;
  int *old_gA_gH = ( int * ) calloc( gnum_op_conn, sizeof( int ) );
  int old_gnum_A_H = 0;
  int act = 0;
  int pred_H = 0; /* Heuristic of the predicate, used for plan_reconstruction */

  int elements_written = 0;
  int *duplicates_within = (int*)calloc(1, sizeof(int));
  *duplicates_within = 0;
  int *duplicates_toplayers = (int*)calloc(1, sizeof(int));
  *duplicates_toplayers = 0;

  int max_depth = 0;
  DR *dr = malloc(sizeof(DR));
  init_DuplicatesRemoval(dr);
  State *S;
  while(minH > 0 && depth < MAX_DEPTH){
    printf(", D=%d:", local_depth);
       
    fflush(stdout);
    while((S=get_next(read_bucket))!=0){
	    
      /* Initializes the gA array and save it in old_gA */
      pred_H = heuristic(S); 
      if (!lH){
	for (act=0; act<gnum_A; act++)
	  old_gA_gH[act] = gA[act];
	old_gnum_A_H = gnum_A;
      }else{
	for (act=0; act<gnum_H; act++)
	  old_gA_gH[act] = gH[act];
	old_gnum_A_H = gnum_H;
      }
      /*printf("H-val of the state being expanded = %d\n", pred_H);*/
      for (op = 0; op < old_gnum_A_H; op++){
	
	/** E X P A N D **/
	if(result_to_dest(&S__, S, old_gA_gH[op])){
	  action_taken = old_gA_gH[op];
	  h = heuristic(&S__); /* Changes the gA array */
	  /*printf("\t H-val of the succ. = %d \n", h);*/
	  if (h == 0)
	    {
	      printf("\n\t\tGOAL FOUND\n");
	      source_to_dest(&goal, &S__);
	      goal.op = action_taken;
	      source_to_dest(&pred, S);
	      reconstruct_plan(read_bucket, &goal, &pred, depth, pred_H);
#ifdef FREEMEM
	      free_DuplicatesRemoval(dr);
	      free(dr);
	      free_bucket(read_bucket);
	      free_bucket(write_bucket);
	      free(read_bucket);
	      free(write_bucket);
	      free_state(&goal);
	      free_state(&pred);
	      free(old_gA_gH);
	      free(duplicates_within);
#endif
	      return TRUE;
	    }else if (!IMPROVED && h < minH)
	      {
		/* If we have not yet started to collect and we have a better node */
		minH = h;
		printf("\n\t\t%4d             ", minH);
		local_depth = 1;
		printf(", D=%d:", local_depth);

		/* If we are using set based EHC, we should not stop */
		if (SET) {
		  initialize_bucket(temp_bucket, depth+1, minH, TRUE);
		  insert_in_bucket(temp_bucket, &S__,S, action_taken);
		  IMPROVED = TRUE;
		} else{
		  depth++;
		  if (max_depth < depth)
		    max_depth = depth;
		  initialize_bucket(write_bucket, depth, minH, TRUE);
		  insert_in_bucket(write_bucket, &S__, S, action_taken);
		  flush_bucket(write_bucket);
		  close_bucket(read_bucket);
				 
		  initialize_bucket(read_bucket, depth, minH, TRUE);
		  initialize_bucket(write_bucket, depth+1, minH, TRUE);
		  i = BUCKETSIZE;
		  break;
		}
	      } /* We are collecting all the good nodes */
	  else if (h == minH && IMPROVED){
	    exit(1);
	    insert_in_bucket(temp_bucket, &S__, S, action_taken);
	  } /* We were collecting but found a much better node */
	  else if (h < minH && IMPROVED){
	    exit(1);
	    /*printf("Found a much better node with H = %d, ", h);*/
	    /*printf("But I am ignoring it .. \n");*/
	  }else if(h >= minH ){
	    /* These will be ignored else */
	    if(!IMPROVED) insert_in_bucket(write_bucket, &S__, S, action_taken); 
	  }
	}else{
	  /*printf("Operator failed\n");*/
	}
      }

    }
    flush_bucket(write_bucket);
    if (IMPROVED) flush_bucket(temp_bucket);
    IMPROVED = FALSE;
    depth++;
    if (max_depth < depth)
      max_depth = depth;
    /*printf("********* Working on Depth = %d *************\n", depth);*/
    close_bucket(read_bucket);
	
    elements_written = mergeAndRemoveDuplicates(dr, depth, minH, 1,
						duplicates_within);

    /*	printf("Calling DR again .. \n");
	elements_written = mergeAndRemoveDuplicates(dr, depth, minH, 1,
	duplicates_within);
    */
    subtractFilesOfAboveLayers(dr, depth, minH, LOCALITY, duplicates_toplayers, hval_initial_state, max_depth);

    initialize_bucket(read_bucket, depth, minH, TRUE);
    initialize_bucket(write_bucket, depth+1, minH, TRUE);
    local_depth++;
  } /* End of main while loop */

#ifdef FREEMEM
  free_DuplicatesRemoval(dr);
  free(dr);
  free_bucket(read_bucket);
  free_bucket(write_bucket);
  free(read_bucket);
  free(write_bucket);
  free_state(&goal);
  free_state(&pred);
  free(old_gA_gH);
#endif
  return FALSE;

}

 
float goal_cost(State *goal, State *pred, int depth){
  float prune = 0;
  int i;
  for ( i = 0; i < glnf_metric.num_pF; i++ ) {
    if ( glnf_metric.pF[i] == -2 ) {  
      if ( pred ) { 
	/* printf ("+ %f * %d",gtt,first->father->g+1); */
	prune += gtt * (depth );
      }  
    } else {
      if(!fl_isSame_to_isViolated(glnf_metric.pF[i] )) {
	/* prune += (glnf_metric.pC[i] * 
	   (first->S.f_V[glnf_metric.pF[i]] - ginitial_state.f_V[glnf_metric.pF[i]])); */
	/* printf ("+ %f * %f",glnf_metric.pC[i], 
	   first->S.f_V[glnf_metric.pF[i]]); */
	prune += glnf_metric.pC[i] * 
	  (goal->f_V[glnf_metric.pF[i]]);
      }
      else {
	/*		printf ("+ %f * %f",glnf_metric.pC[i], 
			first->S.f_V[glnf_metric.pF[i]]); */
	prune += (glnf_metric.pC[i] * goal->f_V[glnf_metric.pF[i]]);
      }   
    }
  }
  return prune;
}
 
void write_goal(State *goal, State *pred, int depth){

  float prune = goal_cost(goal, pred, depth);
    
  FILE *out1;
  printf("\t New upper bound (minimization assumed)\n");      
  printf("\t Add (< (metric) %.2f) to goal description\n",prune);
  if((out1 = fopen("newgoal.pddl", "w"))==NULL) {
    printf("canno't open a file\n");
    exit(1);
  }  

  fprintf(out1, "(< ");
  fprint_ParseExpNode(out1, gparse_metric);
  fprintf(out1, " %.2f )\n", prune);
  fclose(out1);
}

Bool is_goal(State *goal){

  int i, j;
  /*printf("gnum_flogic_goal = %d \n", gnum_flogic_goal);*/
     
  for ( j = 0; j < gnum_flogic_goal; j++ ) {
    for( i = 0; i< goal->num_F ; i++){
      if (gflogic_goal[j] == goal->F[i])
	break;
    }
    if (i == goal->num_F)
      return FALSE;
	 
  }
  /*return TRUE;*/
  if (isDurative == TRUE){
    for ( i = 0; i < gnum_fnumeric_goal; i++ ) {
	    
      if ( gfl_conn[gfnumeric_goal_fl[i]].def[0]) {
	return FALSE;
      }
      /*	print_fl_name(	   gfnumeric_goal_fl[i]);
		printf("gfnumeric_goal_c = %f\n", gfnumeric_goal_c[i]);*/
      float value = 0.0;
      int k;
      for (k = 0; k < gnum_relevant_fluents; k++){
	if (k == gfnumeric_goal_fl[i]){
	  /*print_fl_name(	k);*/
	  value = goal->f_V[k];
	  break;
	}
      }
      /*	printf("I have found %f\n", value);*/
      /*	printf(gfnumeric_goal_c[i] ) ) {*/
      /*	printf("gfnumeric_goal_c again = %f\n", gfnumeric_goal_c[i]);*/
      if ( !number_comparison_holds( gfnumeric_goal_comp[i],
				     value,
				     gfnumeric_goal_c[i] ) ) {
	return FALSE;
      }
    }
  }
  /*
    for ( i = 0; i < gnum_flogic_goal; i++ ) {
    if ( gft_conn[gflogic_goal[i]].level == INFINITY ) {
    gft_conn[gflogic_goal[i]].level = time;
    }
    }
  */
  return TRUE;
}
