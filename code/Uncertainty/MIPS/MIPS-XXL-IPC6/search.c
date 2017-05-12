


/*
 * THIS SOURCE CODE IS SUPPLIED  ``AS IS'' WITHOUT WARRANTY OF ANY KIND, 
 * AND ITS AUTHOR AND THE JOURNAL OF ARTIFICIAL INTELLIGENCE RESEARCH 
 * (JAIR) AND JAIR'S PUBLISHERS AND DISTRIBUTORS, DISCLAIM ANY AND ALL 
 * WARRANTIES, INCLUDING BUT NOT LIMITED TO ANY IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE, AND
 * ANY WARRANTIES OR NON INFRINGEMENT.  THE USER ASSUMES ALL LIABILITY AND
 * RESPONSIBILITY FOR USE OF THIS SOURCE CODE, AND NEITHER THE AUTHOR NOR
 * JAIR, NOR JAIR'S PUBLISHERS AND DISTRIBUTORS, WILL BE LIABLE FOR 
 * DAMAGES OF ANY KIND RESULTING FROM ITS USE.  Without limiting the 
 * generality of the foregoing, neither the author, nor JAIR, nor JAIR's
 * publishers and distributors, warrant that the Source Code will be 
 * error-free, will operate without interruption, or will meet the needs 
 * of the user.
 */











/*********************************************************************
 *
 * File: search.c
 *
 * Description: implementation of routines that search the state space
 *
 *              PDDL level 2 version.
 *
 *              here: basic best-first search, using helpful actions
 *
 *
 * Author: Joerg Hoffmann 2001
 *
 *********************************************************************/ 









#include "ff.h"

#include "output.h"
#include "memory.h"

#include "expressions.h"

#include "relax.h"
#include "search.h"

#include "parse.h"

#include "grounded.h"
#include "bucket.h"









/*****************
 * LOCAL GLOBALS *
 *****************/











/* search space for EHC
 */
EhcNode *lehc_space_head, *lehc_space_end, *lehc_current_start, *lehc_current_end;



/* memory (hash table) for states that are already members
 * of the breadth - first search space in EHC
 */
EhcHashEntry_pointer lehc_hash_entry[EHC_HASH_SIZE];
int lnum_ehc_hash_entry[EHC_HASH_SIZE];
int lchanged_ehc_entrys[EHC_HASH_SIZE];
int lnum_changed_ehc_entrys;
Bool lchanged_ehc_entry[EHC_HASH_SIZE];



/* memory (hash table) for states that are already 
 * encountered by current serial plan
 */
PlanHashEntry_pointer lplan_hash_entry[PLAN_HASH_SIZE];



/* search space
 */
BfsNode *lbfs_space_head, *lbfs_space_had;



/* memory (hash table) for states that are already members
 * of the best first search space
 */
BfsHashEntry_pointer lbfs_hash_entry[BFS_HASH_SIZE];


/* For Solution Files*/

FILE *pfp;
FILE *fp2;
OpConn opconn_array1[MAX_PLAN_LENGTH];
OpConn opconn_array[MAX_PLAN_LENGTH];

OpConn *schedulop_conn;
int num_schedulop_conn;

/********************************
 * EHC FUNCTION, CALLED BY MAIN *
 ********************************/





Bool lH;










Bool do_enforced_hill_climbing( void )

{

  State S, S_;
  int i, h, h_;

  make_state( &S, gnum_ft_conn, gnum_fl_conn );
  make_state( &S_, gnum_ft_conn, gnum_fl_conn );

  /* initialize plan hash table, search space, search hash table
   */
  for ( i = 0; i < PLAN_HASH_SIZE; i++ ) {
    lplan_hash_entry[i] = NULL;
  }
  hash_plan_state( &ginitial_state, 0 );
  
  lehc_space_head = new_EhcNode();
  lehc_space_end = lehc_space_head;
  
  for ( i = 0; i < EHC_HASH_SIZE; i++ ) {
    lehc_hash_entry[i] = NULL;
    lnum_ehc_hash_entry[i] = 0;
    lchanged_ehc_entry[i] = FALSE;
  }
  lnum_changed_ehc_entrys = 0;
  
  /* start enforced Hill-climbing
   */
  lH = TRUE;

  source_to_dest( &S, &ginitial_state );
  h = get_1P_and_H( &S );

  if ( h == INFINITY ) {
    return FALSE;
  }
  if ( h == 0 ) {
    return TRUE;
  }  
  if ( gcmd_line.display_info ) {
    printf("\n\nCueing down from goal distance: %4d into depth ", h);
  }

  while ( h != 0 ) {
    if ( !search_for_better_state( &S, h, &S_, &h_ ) ) {
      printf(" --- pruning stopped --- ");
      get_1P_and_A( &S );
      lH = FALSE;
      if ( !search_for_better_state( &S, h, &S_, &h_ ) ) {
	return FALSE;
      }
      lH = TRUE;
      get_1P_and_H( &S_ );/* to set up gH info for new start state */
    }
    source_to_dest( &S, &S_ );
    h = h_;
    if ( gcmd_line.display_info ) {
      printf("\n                                %4d            ", h);
    }
  }

  return TRUE;

}













/*************************************************
 * FUNCTIONS FOR BREADTH FIRST SEARCH IN H SPACE *
 *************************************************/















Bool search_for_better_state( State *S, int h, State *S_, int *h_ )

{

  static Bool fc = TRUE;
  static State S__;

  int i, h__, depth = 0;
  EhcNode *tmp;

  if ( fc ) {
    make_state( &S__, gnum_ft_conn, gnum_fl_conn );
    fc = FALSE;
  }

  /* don't hash states, but search nodes.
   * this way, don't need to keep states twice in memory
   */
  tmp = new_EhcNode();
  source_to_dest( &(tmp->S), S);
  hash_ehc_node( tmp );

  lehc_current_end = lehc_space_head->next;
  if ( lH ) {
    for ( i = 0; i < gnum_H; i++ ) {
      if ( result_to_dest( &S__, S, gH[i] ) ) {
	add_to_ehc_space( &S__, gH[i], NULL );
      }
    }
  } else {
    for ( i = 0; i < gnum_A; i++ ) {
      if ( result_to_dest( &S__, S, gA[i] ) ) {
	add_to_ehc_space( &S__, gA[i], NULL );
      }
    }
  }

  lehc_current_start = lehc_space_head->next;

  while ( TRUE ) {  
    if ( lehc_current_start == lehc_current_end ) {
      reset_ehc_hash_entrys();
      free( tmp );
      return FALSE;
    }
    if ( lehc_current_start->depth > depth ) {
      depth = lehc_current_start->depth;
      if ( depth > gmax_search_depth ) {
	gmax_search_depth = depth;
      }
      if ( gcmd_line.display_info ) {
	printf("[%d]", depth);
	fflush( stdout );
      }
    }
    h__ = expand_first_node( h );
    if ( LESS( h__, h ) ) {
      break;
    }
  }

  reset_ehc_hash_entrys();
  free( tmp );
  
  extract_plan_fragment( S );

  source_to_dest( S_, &(lehc_current_start->S) );
  *h_ = h__;

  return TRUE;

}



void add_to_ehc_space( State *S, int op, EhcNode *father )

{

  /* see if superior state (in terms of goal reachability)
   * is already a part of this search space
   */
  if ( superior_ehc_state_hashed( S ) ) {
    return;
  }

  if ( !lehc_current_end ) {
    lehc_current_end = new_EhcNode();
    lehc_space_end->next = lehc_current_end;
    lehc_space_end = lehc_current_end;
  }

  source_to_dest( &(lehc_current_end->S), S );
  lehc_current_end->op = op;
  lehc_current_end->father = father;
  if ( !father ) {
    lehc_current_end->depth = 1;
  } else {
    lehc_current_end->depth = father->depth + 1;
  }

  hash_ehc_node( lehc_current_end );

  lehc_current_end = lehc_current_end->next;

}



int expand_first_node( int h )

{

  static Bool fc = TRUE;
  static State S_;

  int h_, i;

  if ( fc ) {
    make_state( &S_, gnum_ft_conn, gnum_fl_conn );
    fc = FALSE;
  }

  if ( lH ) {
    h_ = get_1P_and_H( &(lehc_current_start->S) );
  } else {
    h_ = get_1P_and_A( &(lehc_current_start->S) );
  }   

  if ( h_ == INFINITY ) {
    lehc_current_start = lehc_current_start->next;
    return h_;
  }

  if ( h_ < h ) {
    return h_;
  }

  if ( lH ) {
    for ( i = 0; i < gnum_H; i++ ) {
      if ( result_to_dest( &S_, &(lehc_current_start->S), gH[i] ) ) {
	add_to_ehc_space( &S_, gH[i], lehc_current_start );
      }
    }
  } else {
    for ( i = 0; i < gnum_A; i++ ) {
      if ( result_to_dest( &S_, &(lehc_current_start->S), gA[i] ) ) {
	add_to_ehc_space( &S_, gA[i], lehc_current_start );
      }
    }
  }
    
  lehc_current_start = lehc_current_start->next;

  return h_;

}














/********************************************************
 * HASHING ALGORITHM FOR RECOGNIZING REPEATED STATES IN *
 * EHC BREADTH FIRST SEARCH                             *
 ********************************************************/














void hash_ehc_node( EhcNode *n )

{

  int i, sum, index;
  EhcHashEntry *h, *prev = NULL;

  sum = state_sum( &(n->S) );
  index = sum & EHC_HASH_BITS;

  h = lehc_hash_entry[index];
  if ( !h ) {
    h = new_EhcHashEntry();
    h->sum = sum;
    h->ehc_node = n;
    lehc_hash_entry[index] = h;
    lnum_ehc_hash_entry[index]++;
    if ( !lchanged_ehc_entry[index] ) {
      lchanged_ehc_entrys[lnum_changed_ehc_entrys++] = index;
      lchanged_ehc_entry[index] = TRUE;
    }
    return;
  }
  i = 0;
  while ( h ) {
    if ( i == lnum_ehc_hash_entry[index] ) {
      break;
    }
    i++;
    prev = h;
    h = h->next;
  }

  if ( h ) {
    /* current list end is still in allocated list of hash entrys
     */
    h->sum = sum;
    h->ehc_node = n;
    lnum_ehc_hash_entry[index]++;
    if ( !lchanged_ehc_entry[index] ) {
      lchanged_ehc_entrys[lnum_changed_ehc_entrys++] = index;
      lchanged_ehc_entry[index] = TRUE;
    }
    return;
  }
  /* allocated list ended; connect a new hash entry to it.
   */
  h = new_EhcHashEntry();
  h->sum = sum;
  h->ehc_node = n;
  prev->next = h;
  lnum_ehc_hash_entry[index]++;
  if ( !lchanged_ehc_entry[index] ) {
    lchanged_ehc_entrys[lnum_changed_ehc_entrys++] = index;
    lchanged_ehc_entry[index] = TRUE;
  }
  return;
      
}



Bool ehc_state_hashed( State *S )

{

  int i, sum, index;
  EhcHashEntry *h;

  sum = state_sum( S );
  index = sum & EHC_HASH_BITS;

  h = lehc_hash_entry[index];
  for ( i = 0; i < lnum_ehc_hash_entry[index]; i++ ) {
    if ( h->sum != sum ) {
      h = h->next;
      continue;
    }
    if ( same_state( &(h->ehc_node->S), S ) ) {
      return TRUE;
    }
    h = h->next;
  }

  return FALSE;

}



Bool superior_ehc_state_hashed( State *S )

{

  int i, sum, index;
  EhcHashEntry *h;

  sum = state_sum( S );
  index = sum & EHC_HASH_BITS;

  h = lehc_hash_entry[index];
  for ( i = 0; i < lnum_ehc_hash_entry[index]; i++ ) {
    if ( h->sum < sum ) {
      h = h->next;
      continue;
    }
    if ( superior_state( &(h->ehc_node->S), S ) ) {
      return TRUE;
    }
    h = h->next;
  }

  return FALSE;

}



Bool superior_state( State *S1, State *S2 ) 

{

  int i, j;

  if ( !gconditional_effects ) {
    for ( i = 0; i < S2->num_F; i++ ) {
      for ( j = 0; j < S1->num_F; j++ ) {
	if ( S1->F[j] == S2->F[i] ) {
	  break;
	}
      }
      if ( j == S1->num_F ) {
	return FALSE;
      }
    }
    
    /* check whether the fluent values are superior.
     * see JAIR article for explanation / justification
     */
    for ( i = 0; i < gnum_real_fl_conn; i++ ) {
      if ( !gfl_conn[i].relevant ) {
	continue;
      }
      
      if ( !S2->f_D[i] ) {
	continue;
      }
      
      if ( !S1->f_D[i] ||
	   S2->f_V[i] > S1->f_V[i] ) {
	return FALSE;
      }
    }
  } else {
    if ( S2->num_F != S1->num_F ) {
      return FALSE;
    }
    for ( i = 0; i < S2->num_F; i++ ) {
      for ( j = 0; j < S1->num_F; j++ ) {
	if ( S1->F[j] == S2->F[i] ) {
	  break;
	}
      }
      if ( j == S1->num_F ) {
	return FALSE;
      }
    }
    for ( i = 0; i < gnum_real_fl_conn; i++ ) {
      if ( !gfl_conn[i].relevant ) {
	continue;
      }      
      if ( S2->f_D[i] != S1->f_D[i]  ) {
	return FALSE;
      }
      if ( S2->f_V[i] != S1->f_V[i] ) {
	return FALSE;
      }
    }
  }

  return TRUE;

}



void reset_ehc_hash_entrys( void )

{

  int i;

  for ( i = 0; i < lnum_changed_ehc_entrys; i++ ) {
    lnum_ehc_hash_entry[lchanged_ehc_entrys[i]] = 0;
    lchanged_ehc_entry[lchanged_ehc_entrys[i]] = FALSE;
  }
  lnum_changed_ehc_entrys = 0;

}










/***************************************************
 * FUNCTIONS FOR UPDATING THE CURRENT SERIAL PLAN, *
 * BASED ON SEARCH SPACE INFORMATION .             *
 *                                                 *
 * EMPLOY SOMEWHAT TEDIOUS HASHING PROCEDURE TO    *
 * AVOID REPEATED STATES IN THE PLAN               *
 ***************************************************/












void extract_plan_fragment( State *S )

{

  EhcNode *i;
  int ops[MAX_PLAN_LENGTH], num_ops;
  State_pointer states[MAX_PLAN_LENGTH];
  int j;
  PlanHashEntry *start = NULL, *i_ph;

  num_ops = 0;
  for ( i = lehc_current_start; i; i = i->father ) {
    if ( (start = plan_state_hashed( &(i->S) )) != NULL ) {
      for ( i_ph = start->next_step; i_ph; i_ph = i_ph->next_step ) {
	i_ph->step = -1;
      }
      gnum_plan_ops = start->step;
      break;
    }
    if ( num_ops == MAX_PLAN_LENGTH ) {
      printf("\nincrease MAX_PLAN_LENGTH! currently %d\n\n",
	     MAX_PLAN_LENGTH);
      exit( 1 );
    }
    states[num_ops] = &(i->S);

    ops[num_ops++] = i->op;
  }
  if ( !start ) {
    start = plan_state_hashed( S );
    if ( !start ) {
      printf("\n\ncurrent start state not hashed! debug me!\n\n");
      exit( 1 );
    }
    if ( start->step == -1 ) {
      printf("\n\ncurrent start state marked removed from plan! debug me!\n\n");
      exit( 1 );
    }
  }

  for ( j = num_ops - 1; j > -1; j-- ) {
    if ( gnum_plan_ops == MAX_PLAN_LENGTH ) {
      printf("\nincrease MAX_PLAN_LENGTH! currently %d\n\n",
	     MAX_PLAN_LENGTH);
      exit( 1 );
    }
    start->next_step = hash_plan_state( states[j], gnum_plan_ops + 1 );
    start = start->next_step;
    copy_source_to_dest( &(gplan_states[gnum_plan_ops+1]), states[j] );


    /*+++++++++++duration calculation+++++++++++++++*/


    if(isDurative==TRUE)
      gop_conn[ops[j]].duration = instanziate_LnfExpNode(S,gop_conn[ops[j]].action->lnf_duration);


    gplan_ops[gnum_plan_ops++] = ops[j];    

  }

}



float instanziate_LnfExpNode( State *S, LnfExpNode *n )

{

  int i, k;
  float result = 0.00;
  float help = 0.00;
  if (!n) return 0.0;
    

  for ( i = 0; i < n->num_pF; i++ ) {

    for(k=0; k< gnum_relevant_fluents; k++){
   
      if(k==n->pF[i]){
	help= S->f_V[k];
	break;
      }          

    }
    result+= n->pC[i]*help;
  
  }
  
  result+=n->c;
 
  return result;

}




/*+++++++++++++++++++++++++++++++++++++++++++++++++*/


PlanHashEntry *hash_plan_state( State *S, int step )

{

  int sum, index;
  PlanHashEntry *h, *tmp;

  sum = state_sum( S );
  index = sum & PLAN_HASH_BITS;

  for ( h = lplan_hash_entry[index]; h; h = h->next ) {
    if ( h->sum != sum ) continue;
    if ( same_state( S, &(h->S) ) ) break;
  }

  if ( h ) {
    if ( h->step != -1 ) {
      printf("\n\nreencountering a state that is already in plan! debug me\n\n");
      exit( 1 );
    }
    h->step = step;
    return h;
  }

  for ( h = lplan_hash_entry[index]; h && h->next; h = h->next );

  tmp = new_PlanHashEntry();
  tmp->sum = sum;
  copy_source_to_dest( &(tmp->S), S );
  tmp->step = step;

  if ( h ) {
    h->next = tmp;
  } else {
    lplan_hash_entry[index] = tmp;
  }

  return tmp;

}
  

 
PlanHashEntry *plan_state_hashed( State *S )

{

  int sum, index;
  PlanHashEntry *h;

  sum = state_sum( S );
  index = sum & PLAN_HASH_BITS;

  for ( h = lplan_hash_entry[index]; h; h = h->next ) {
    if ( h->sum != sum ) continue;
    if ( same_state( S, &(h->S) ) ) break;
  }

  if ( h && h->step != -1 ) {
    return h;
  }

  return NULL;

}



Bool same_state( State *S1, State *S2 ) 

{

  int i, j;

  if ( S1->num_F != S2->num_F ) {
    return FALSE;
  }

  for ( i = 0; i < S2->num_F; i++ ) {
    for ( j = 0; j < S1->num_F; j++ ) {
      if ( S1->F[j] == S2->F[i] ) {
	break;
      }
    }
    if ( j == S1->num_F ) {
      return FALSE;
    }
  }

  for ( i = 0; i < gnum_fl_conn; i++ ) {
    if ( S2->f_D[i] != S1->f_D[i] ||
	 S2->f_V[i] != S1->f_V[i] ) {
      return FALSE;
    }
  }

  return TRUE;

}













/************************************
 * BEST FIRST SEARCH IMPLEMENTATION *
 ************************************/











Bool do_best_first_search( void )

{

  BfsNode *first, *final;
  State S;
  float alpha = 100000000.0;
  int i,  expanded = 0, min = INFINITY;
  Bool start = TRUE;
  FILE *out1;
  int l;



  make_state( &S, gnum_ft_conn, gnum_fl_conn );

  lbfs_space_head = new_BfsNode();
  lbfs_space_had = NULL;

  for ( i = 0; i < BFS_HASH_SIZE; i++ ) {
    lbfs_hash_entry[i] = NULL;
  }

  ginitial_state.stt = 0;
  ginitial_state.cost = 0;

  add_to_bfs_space( &ginitial_state, -1, NULL );

  while ( TRUE ) {

    expanded++;
    

    if ( (first = lbfs_space_head->next) == NULL ) {
      printf("\n\t complete exploration - no possible solution!\n"); 
      exit(1);  /* Added for anytime search */
      if ( gcmd_line.display_info ) {
	printf("\n\nbest first search space empty! problem proven unsolvable.\n\n");
      }
      return FALSE;
    }
    lbfs_space_head->next = first->next;
    if ( first->next ) {
      first->next->prev = lbfs_space_head;
    }

    if ( LESS( first->h, min ) ) {
      min = first->h;
      if ( start ) {
	if ( gcmd_line.display_info ) {
	  printf("\n\nadvancing to distance: %4d", min);
	  fflush(stdout);
	}
	start = FALSE;
      } else {
	if ( gcmd_line.display_info ) {
	  printf("\n                       %4d", min);
	  fflush(stdout);
	}
      }
    }

    if ( first->h == 0 ) {

      for ( l = 0; l < glnf_metric.num_pF; l++ ) {
	if ( glnf_metric.pF[l] != -2 ) {  
	  prune += (glnf_metric.pC[l] * first->S.f_V[glnf_metric.pF[l]]);   
	}
      }

      prune += glnf_metric.c;

      printf("\t New upper bound (minimization assumed)\n");      
      if(gparse_optimization){
	if ( strcmp( gparse_optimization, "MAXIMIZE" ) == SAME )
	  printf("\t Add (> (metric) %.2f) to goal description\n",prune-0.01);
	else
	  printf("\t Add (< (metric) %.2f) to goal description\n",prune - 0.01);
      }          

      if(prune != 0){
	if((out1 = fopen("newgoal.pddl", "w"))==NULL) {
	  printf("canno't open a file\n");
	  exit(1);
	}  
          
	if ( strcmp( gparse_optimization, "MAXIMIZE" ) == SAME ){
	  fprintf(out1, "(> ");
	  fprint_FinalExpNode(out1, gparse_metric);
	  fprintf(out1, " %.2f )\n", prune - 0.01);
	  fclose(out1);
	}
	else{
	  fprintf(out1, "(< ");
	  fprint_FinalExpNode(out1, gparse_metric);
	  fprintf(out1, " %.2f )\n", prune - 0.01);
	  fclose(out1);
	}
           
      }
          

      printf("\t iterate to improve sol. quality\n");
      alpha = prune;
      final = first;  	
      break;

    } 
	
    for ( i = 0; i < first->num_H; i++ ) {
      if ( result_to_dest( &S, &(first->S), first->H[i] ) ) { 
	/* we must include a check here whether the numerical 
	 * part of the action
	 * is entirely fulfilled; only those actions are applied.
	 */
	S.stt = first->S.stt + instanziate_LnfExpNode(&(first->S), gop_conn[first->H[i]].action->lnf_duration); 		    
	add_to_bfs_space( &S, first->H[i], first );

	/*		print_op_name(first->H[i]);
			printf("\n; Duration = %d\n", gop_conn[first->H[i]].action->lnf_duration);  */
      }
    }
   
    first->next = lbfs_space_had;
    lbfs_space_had = first;
  }



  /*  extract_plan( first ); */ 

  extract_plan( final );  

  /* for plan quality
   */
  printf("\n\nPlan-quality: %.2f\n", final->float_fn);
  
  return TRUE;

}









void extract_actuall_plan(  BfsNode *last, int op)

{

  BfsNode *i;
  int ops[MAX_PLAN_LENGTH], num_ops;
  int j;

  num_ops = 0;
  for ( i = last; i->op != -1; i = i->father ) {
    if ( num_ops == MAX_PLAN_LENGTH ) {
      printf("\nincrease MAX_PLAN_LENGTH! currently %d\n\n",
	     MAX_PLAN_LENGTH);
      exit( 1 );
    }
    if (isDurative==TRUE )
      gop_conn[i->op].duration = i->S.stt - i->father->S.stt;
    /*last->father->S.instanziate_LnfExpNode(&(last->father->S),gop_conn[ops[j]].action->lnf_duration);*/
    ops[num_ops++] = i->op;
  }

  gnum_plan_ops = 0;
  for ( j = num_ops - 1; j > -1; j-- ) { 
    if (isWithinAutomaton(gop_conn[ops[j]].action->name)== FALSE) {
      /*  printf("\n%s{%f,%f}",gop_conn[ops[j]].action->name,gop_conn[ops[j]].action->timeMin,gop_conn[ops[j]].action->timeMax); */
      gplan_ops[gnum_plan_ops++] = ops[j];
    }
  }


  if (isWithinAutomaton(gop_conn[op].action->name) == FALSE) {
    /*      printf("\n%s{%f,%f}",gop_conn[op].action->name,gop_conn[op].action->timeMin,gop_conn[op].action->timeMax); */
    gplan_ops[gnum_plan_ops++] = op;
  }



  
}






float costMetric_without_TotalTime(const State* s){
  float lprune = 0.0;
  int i;
  int loop = (glnf_metric.num_pF ) / 4;
  int rest = (glnf_metric.num_pF ) % 4;
	

  for ( i = 0; i < loop; i+=4 ) {
    if ( glnf_metric.pF[i] != -2 ) {  
      lprune += (glnf_metric.pC[i] * s->f_V[glnf_metric.pF[i]]);   
    }

    if ( glnf_metric.pF[i+1] != -2 ) {  
      lprune += (glnf_metric.pC[i+1] * s->f_V[glnf_metric.pF[i+1]]);   
    }

    if ( glnf_metric.pF[i+2] != -2 ) {  
      lprune += (glnf_metric.pC[i+2] * s->f_V[glnf_metric.pF[i+2]]);   
    }

    if ( glnf_metric.pF[i+3] != -2 ) {  
      lprune += (glnf_metric.pC[i+3] * s->f_V[glnf_metric.pF[i+3]]);   
    }
  }
  loop = loop*4;
  for (i=0; i < rest; i++){
    if ( glnf_metric.pF[loop+i] != -2 ) {  
      lprune += (glnf_metric.pC[loop+i] * s->f_V[glnf_metric.pF[loop+i]]);   
    }
  }
  return lprune;
}






void add_to_bfs_space( State *S, int op, BfsNode *father )

{

  BfsNode *new, *i;
  int j, h, intg, int_fn = 0;
  float cost = 0, floatg = 0, float_fn = 0;
  float pertg = 0;


  if (gcmd_line.optimize && goptimization_established ) { 
    if ( bfs_state_hashed( S ) ) {
      return;
    }
  } else {
    if ( superior_bfs_state_hashed( S ) ) {
      return;
    }
  }

  h = get_1P_and_A( S );

  if ( gcmd_line.optimize && goptimization_established ) {

    /* gtt is mulitplicator of TOTAL-TIME in final metric; if no
     * total-time part in metric, it is 0
     */
    cost = gcost;
    cost += h * gtt; 
  }

  if ( h == INFINITY ) {
    return;
  }


  if ( father ) {
    intg = father->g + 1;

    gnum_plan_ops=0;

    extract_actuall_plan(father,op);

    if (isTempTimeWindows==FALSE)
      pertg = schedulpert();
    else {
      float wert = 10000.00;
      float *makespann = &wert ; 
	  
      OpConn_list *head = make_seqplan_list();

      makespann = calculate_best_makespann1(head,makespann);
	  
      int p;
      for( p=0;p < gnum_plan_ops;p++)
	{
	  gop_conn[gplan_ops[p]] = opconn_array[p];	
	}    
      pertg = schedulpert();
    }
      
    /*
      printf("\n--------------- (%d) %f",father->g,pertg);  
    */
      
    if (pertg >= 100000) { printf(".");  return; } 

      
  } else {
    intg = 0;
    pertg = S->stt;
  }

  if (gcmd_line.optimize && goptimization_established ) { 

    /* not called in competition */

    floatg = (float) intg; 
    /* state_cost( S, father ); 
       float_fn =  (((float) gcmd_line.g_weight) * floatg) + (((float) gcmd_line.h_weight) * cost); */

    if (isTempTimeWindows==FALSE)
      float_fn = (float) (gcmd_line.g_weight * intg) + (gcmd_line.h_weight  * h);  
    else
      float_fn = (float) (gcmd_line.g_weight * pertg) + (0 * gcmd_line.h_weight  * h); 
    

    /*  float_fn = state_cost(S, father) + (0.001 * float_fn); 
     */
    for ( i = lbfs_space_head; i->next; i = i->next ) {
      if ( i->next->float_fn >= float_fn ) break;
    }
  } else {

    if (isTempTimeWindows==FALSE) 
      int_fn = (gcmd_line.g_weight * intg) + (gcmd_line.h_weight * h);
    else {
      /* int_fn = (gcmd_line.g_weight * intg);  */
      int_fn = (gcmd_line.g_weight * pertg); 
    }
    for ( i = lbfs_space_head; i->next; i = i->next ) {
      if ( i->next->int_fn >= int_fn ) break;
    }
  }

  new = new_BfsNode();
  copy_source_to_dest( &(new->S), S );
  new->op = op;
  new->h = h;
  if ( gcmd_line.optimize && goptimization_established ) {
    new->float_fn = float_fn;
  } else {
    new->int_fn = int_fn;
  }
  new->father = father;
  new->g = intg;

  new->H = ( int * ) calloc( gnum_A, sizeof( int ) );
  for ( j = 0; j < gnum_A; j++ ) {
    new->H[j] = gA[j];
  }
  new->num_H = gnum_A;

  new->next = i->next;
  new->prev = i;
  i->next = new;
  if ( new->next ) {
    new->next->prev = new;
  }
  
  hash_bfs_node( new );

}



float state_cost( State *S, BfsNode *father )

{

  float cost = 0;
  int i;

  for ( i = 0; i < glnf_metric.num_pF; i++ ) {
    if ( glnf_metric.pF[i] == -2 ) {
      /* cost is number of steps from I to S 
       */ 
      if ( father ) {
	cost += gtt * (father->g + 1);
      }/* no father, no steps, no cost */
    } else {

      if(!fl_isSame_to_isViolated(glnf_metric.pF[i] ))
	cost += (glnf_metric.pC[i] * 
		 (S->f_V[glnf_metric.pF[i]] - ginitial_state.f_V[glnf_metric.pF[i]]));
      else {
	cost += (glnf_metric.pC[i] * S->f_V[glnf_metric.pF[i]]);
      }   
    }
  }
  return cost;

}





void extract_plan( BfsNode *last )

{

  BfsNode *i;
  int ops[MAX_PLAN_LENGTH], num_ops;
  int j;

  num_ops = 0;
  for ( i = last; i->op != -1; i = i->father ) {
    if ( num_ops == MAX_PLAN_LENGTH ) {
      printf("\nincrease MAX_PLAN_LENGTH! currently %d\n\n",
	     MAX_PLAN_LENGTH);
      exit( 1 );
    }
    if(isDurative==TRUE )
      gop_conn[i->op].duration = i->S.stt - i->father->S.stt;
    /*last->father->S.instanziate_LnfExpNode(&(last->father->S),gop_conn[ops[j]].action->lnf_duration);*/
    ops[num_ops++] = i->op;

  }

  gnum_plan_ops = 0;
  for ( j = num_ops - 1; j > -1; j-- ) { 
    gplan_ops[gnum_plan_ops++] = ops[j];
  }
  
}








/************************************************************
 * HASHING ALGORITHM FOR RECOGNIZING REPEATED STATES IN BFS *
 ************************************************************/










void hash_bfs_node( BfsNode *n )

{

  int sum, index;
  BfsHashEntry *h, *tmp;

  sum = state_sum( &(n->S) );
  index = sum & BFS_HASH_BITS;

  h = lbfs_hash_entry[index];
  if ( !h ) { 
    h = new_BfsHashEntry();
    h->sum = sum;
    h->bfs_node = n;
    lbfs_hash_entry[index] = h;
    return;
  }
  for ( ; h->next; h = h->next );

  tmp = new_BfsHashEntry();
  tmp->sum = sum;
  tmp->bfs_node = n;
  h->next = tmp;
      
}



Bool bfs_state_hashed( State *S )

{

  int sum, index;
  BfsHashEntry *h;

  sum = state_sum( S );
  index = sum & BFS_HASH_BITS;

  h = lbfs_hash_entry[index];
  for ( h = lbfs_hash_entry[index]; h; h = h->next ) {
    if ( h->sum != sum ) {
      continue;
    }
    if ( same_state( &(h->bfs_node->S), S ) ) {
      return TRUE;
    }
  }

  return FALSE;

}



Bool superior_bfs_state_hashed( State *S )

{

  int sum, index;
  BfsHashEntry *h;

  sum = state_sum( S );
  index = sum & BFS_HASH_BITS;

  h = lbfs_hash_entry[index];
  for ( h = lbfs_hash_entry[index]; h; h = h->next ) {
    if ( h->sum < sum ) {
      continue;
    }
    if ( superior_state( &(h->bfs_node->S), S ) ) {
      return TRUE;
    }
  }

  return FALSE;

}



int state_sum( State *S )

{

  int i, sum = 0;

  for ( i = 0; i < S->num_F; i++ ) {
    sum += gft_conn[S->F[i]].rand;
  }

  for ( i = 0; i < gnum_real_fl_conn; i++ ) {
    if ( !gfl_conn[i].relevant ) {
      continue;
    }
    if ( !S->f_D[i] ) {
      continue;
    }
    sum += gfl_conn[i].rand * ( int ) S->f_V[i];
  }

  return sum;

}















/****************************
 * STATE HANDLING FUNCTIONS *
 ****************************/










/* state transition function; here, takes in an action whose
 * logical and numerical preconds are fulfilled, and returns TRUE,
 * putting the result into *dest, iff the action has at least one
 * appearing effect and is legal, i.e. if
 * no illegal numeric effects occur.
 */
Bool result_to_dest( State *dest, State *source, int op )

{

  static Bool first_call = TRUE;
  static Bool *in_source, *in_dest, *in_del, *true_ef, *assigned;
  static int *del, num_del;

  int i, j, ef, fl;
  float val, source_val;
  Comparator comp;

  Bool one_appeared = FALSE;
  
  if ( first_call ) {
    in_source = ( Bool * ) calloc( gnum_ft_conn, sizeof( Bool ) );
    in_dest = ( Bool * ) calloc( gnum_ft_conn, sizeof( Bool ) );
    in_del = ( Bool * ) calloc( gnum_ft_conn, sizeof( Bool ) );
    true_ef = ( Bool * ) calloc( gnum_ef_conn, sizeof( Bool ) );
    assigned = ( Bool * ) calloc( gnum_fl_conn, sizeof( Bool ) );
    del = ( int * ) calloc( gnum_ft_conn, sizeof( int ) );
    for ( i = 0; i < gnum_ft_conn; i++ ) {
      in_source[i] = FALSE;
      in_dest[i] = FALSE;
      in_del[i] = FALSE;
    }
    for ( i = 0; i < gnum_ef_conn; i++ ) {
      true_ef[i] = FALSE;
    }
    for ( i = 0; i < gnum_fl_conn; i++ ) {
      assigned[i] = FALSE;
    }
    first_call = FALSE;
  }

  /* setup true facts for effect cond evaluation
   */
  for ( i = 0; i < source->num_F; i++ ) {
    in_source[source->F[i]] = TRUE;
  }

  /* evaluate effect conditions and setup deleted facts
   */
  num_del = 0;
  for ( i = 0; i < gop_conn[op].num_E; i++ ) {
    ef = gop_conn[op].E[i];
    /* logic cond true?
     */
    for ( j = 0; j < gef_conn[ef].num_PC; j++ ) {
      if ( !in_source[gef_conn[ef].PC[j]] ) break;
    }
    if ( j < gef_conn[ef].num_PC ) continue;
    /* numeric cond true?
     */
    for ( j = 0; j < gef_conn[ef].num_f_PC; j++ ) {
      fl = gef_conn[ef].f_PC_fl[j];
      val = gef_conn[ef].f_PC_c[j];
      comp = gef_conn[ef].f_PC_comp[j];
      if ( !determine_source_val( source, fl, &source_val ) ) {
	/* condition access to an undefined fluent!
	 */
	for ( i = 0; i < num_del; i++ ) {
	  in_del[del[i]] = FALSE;
	}
	for ( i = 0; i < gop_conn[op].num_E; i++ ) {
	  true_ef[i] = FALSE;
	}
	for ( i = 0; i < source->num_F; i++ ) {
	  in_source[source->F[i]] = FALSE;
	}
	return FALSE;
      }
      if ( !number_comparison_holds( comp, source_val, val ) ) break;
    }
    if ( j < gef_conn[ef].num_f_PC ) continue;

    if ( gef_conn[ef].illegal ) {
      /* effect always affects an undefined fluent, as we found out
       * earlier
       */
      for ( i = 0; i < source->num_F; i++ ) {
	in_source[source->F[i]] = FALSE;
      }
      for ( i = 0; i < num_del; i++ ) {
	in_del[del[i]] = FALSE;
      }
      for ( i = 0; i < gop_conn[op].num_E; i++ ) {
	true_ef[i] = FALSE;
      }
      return FALSE;
    }
    true_ef[i] = TRUE;
    one_appeared = TRUE; 
    for ( j = 0; j < gef_conn[ef].num_D; j++ ) {
      if ( in_del[gef_conn[ef].D[j]] ) continue;
      in_del[gef_conn[ef].D[j]] = TRUE;
      del[num_del++] = gef_conn[ef].D[j];
    }
  }
  if ( !one_appeared ) {
    /* no effect appeared which means that the action is either useless
     * here or its preconds are even not fulfilled (the latter
     * shouldn't happen by get_A but well....)
     */
    for ( i = 0; i < source->num_F; i++ ) {
      in_source[source->F[i]] = FALSE;
    }
    for ( i = 0; i < num_del; i++ ) {
      in_del[del[i]] = FALSE;
    }
    for ( i = 0; i < gop_conn[op].num_E; i++ ) {
      true_ef[i] = FALSE;
    }
    return FALSE;
  }

  /* first, see about the numeric effects - those might render
   * the op illegal here. start by copying numeric info.
   */
  for ( i = 0; i < gnum_fl_conn; i++ ) {
    dest->f_D[i] = source->f_D[i];
    dest->f_V[i] = source->f_V[i];

  }

  /* illegal is an op if the result is not well-defined,
   * or if it affects an undefined fluent.
   */
  for ( i = 0; i < gop_conn[op].num_E; i++ ) {
    if ( !true_ef[i] ) continue;
    ef = gop_conn[op].E[i];
    for ( j = 0; j < gef_conn[ef].num_AS; j++ ) {
      fl = gef_conn[ef].AS_fl[j];
      if ( gef_conn[ef].AS_fl_[j] == -1 ) {
	val = gef_conn[ef].AS_c[j];
      } else {
	if ( !determine_source_val( source, gef_conn[ef].AS_fl_[j], &val ) ) {
	  /* effect rh makes use of undefined fluent!
	   */
	  for ( i = 0; i < gnum_fl_conn; i++ ) {
	    assigned[i] = FALSE;
	  }
	  for ( i = 0; i < source->num_F; i++ ) {
	    in_source[source->F[i]] = FALSE;
	  }
	  for ( i = 0; i < num_del; i++ ) {
	    in_del[del[i]] = FALSE;
	  }
	  for ( i = 0; i < gop_conn[op].num_E; i++ ) {
	    true_ef[i] = FALSE;
	  }
	  return FALSE;
	}
	val += gef_conn[ef].AS_c[j];
      }
      if ( assigned[fl] &&
	   val != dest->f_V[fl] ) {
	/* two different values assigned --> result not well-defined --> illegal!
	 */
	for ( i = 0; i < gnum_fl_conn; i++ ) {
	  assigned[i] = FALSE;
	}
	for ( i = 0; i < source->num_F; i++ ) {
	  in_source[source->F[i]] = FALSE;
	}
	for ( i = 0; i < num_del; i++ ) {
	  in_del[del[i]] = FALSE;
	}
	for ( i = 0; i < gop_conn[op].num_E; i++ ) {
	  true_ef[i] = FALSE;
	}
	return FALSE;
      }
      dest->f_D[fl] = TRUE;
      dest->f_V[fl] = val;
      assigned[fl] = TRUE;
    }
    for ( j = 0; j < gef_conn[ef].num_IN; j++ ) {
      fl = gef_conn[ef].IN_fl[j];
      if ( assigned[fl] || 
	   !source->f_D[fl]) {
	/* assign and increase --> result not well-defined --> illegal!
	 * affects an undefined fluent --> illegal!
	 */
	for ( i = 0; i < gnum_fl_conn; i++ ) {
	  assigned[i] = FALSE;
	}
	for ( i = 0; i < source->num_F; i++ ) {
	  in_source[source->F[i]] = FALSE;
	}
	for ( i = 0; i < num_del; i++ ) {
	  in_del[del[i]] = FALSE;
	}
	for ( i = 0; i < gop_conn[op].num_E; i++ ) {
	  true_ef[i] = FALSE;
	}
	return FALSE;
      }
      if ( gef_conn[ef].IN_fl_[j] == -1 ) {
	val = gef_conn[ef].IN_c[j];
      } else {
	if ( !determine_source_val( source, gef_conn[ef].IN_fl_[j], &val ) ) {
	  /* effect rh makes use of undefined fluent!
	   */
	  for ( i = 0; i < gnum_fl_conn; i++ ) {
	    assigned[i] = FALSE;
	  }
	  for ( i = 0; i < source->num_F; i++ ) {
	    in_source[source->F[i]] = FALSE;
	  }
	  for ( i = 0; i < num_del; i++ ) {
	    in_del[del[i]] = FALSE;
	  }
	  for ( i = 0; i < gop_conn[op].num_E; i++ ) {
	    true_ef[i] = FALSE;
	  }
	  return FALSE;
	}
	val += gef_conn[ef].IN_c[j];
      }
      dest->f_V[fl] += val;

    }
  }

  /* put all non-deleted facts from source into dest.
   * need not check for put-in facts here,
   * as initial state is made doubles-free, and invariant keeps
   * true through the transition procedure
   */
  dest->num_F = 0;
  for ( i = 0; i < source->num_F; i++ ) {
    if ( in_del[source->F[i]] ) {
      continue;
    }
    dest->F[dest->num_F++] = source->F[i];
    in_dest[source->F[i]] = TRUE;
  }

  /* now add all fullfilled effect adds to dest; each fact at most once!
   */
  for ( i = 0; i < gop_conn[op].num_E; i++ ) {
    if ( !true_ef[i] ) continue;
    ef = gop_conn[op].E[i];
    for ( j = 0; j < gef_conn[ef].num_A; j++ ) {
      if ( in_dest[gef_conn[ef].A[j]] ) {
	continue;
      }
      dest->F[dest->num_F++] = gef_conn[ef].A[j];
      in_dest[gef_conn[ef].A[j]] = TRUE;
    }
  }

  /* unset infos
   */
  for ( i = 0; i < source->num_F; i++ ) {
    in_source[source->F[i]] = FALSE;
  }
  for ( i = 0; i < dest->num_F; i++ ) {
    in_dest[dest->F[i]] = FALSE;
  }
  for ( i = 0; i < num_del; i++ ) {
    in_del[del[i]] = FALSE;
  }
  for ( i = 0; i < gop_conn[op].num_E; i++ ) {
    true_ef[i] = FALSE;
  }
  for ( i = 0; i < gnum_fl_conn; i++ ) {
    assigned[i] = FALSE;
  }

  return TRUE;

}



Bool determine_source_val( State *source, int fl, float *val )

{

  int i;

  if ( gfl_conn[fl].artificial ) {
    *val = 0;
    for ( i = 0; i < gfl_conn[fl].num_lnf; i++ ) {
      if ( !source->f_D[gfl_conn[fl].lnf_F[i]] ) {
	return FALSE;
      }
      *val += (gfl_conn[fl].lnf_C[i] * source->f_V[gfl_conn[fl].lnf_F[i]]);
    }
  } else {
    if ( !source->f_D[fl] ) {
      return FALSE;
    }
    *val = source->f_V[fl];
  }

  return TRUE;

}



void copy_source_to_dest( State *dest, State *source )

{

  int i;

  make_state( dest, source->num_F, gnum_fl_conn );

  for ( i = 0; i < source->num_F; i++ ) {
    dest->F[i] = source->F[i];
  }
  dest->num_F = source->num_F;

  for ( i = 0; i < gnum_fl_conn; i++ ) {
    dest->f_D[i] = source->f_D[i];
    dest->f_V[i] = source->f_V[i];
  }

  dest->op = source->op;
  dest->cost = source->cost;
  dest->stt = source->stt;
}



void source_to_dest( State *dest, State *source )

{

  int i;

  for ( i = 0; i < source->num_F; i++ ) {
    dest->F[i] = source->F[i];
  }
  dest->num_F = source->num_F;

  for ( i = 0; i < gnum_fl_conn; i++ ) {
    dest->f_D[i] = source->f_D[i];
    dest->f_V[i] = source->f_V[i];
  }
  /************/
  /* EXTERNAL */
  /************/
  dest->pred_index = source->pred_index;
  dest->op = source->op;
  dest->cost = source->cost;
  dest->stt = source->stt;
}



/*--------------temporal--functions-------------------*/


/*-------------BubbeleSort---function-----------------*/

void BubbleSort(float feld[],int feldop[], int anzahl)
{
  /* Lokale Hilfsvariablen*/
  int i, k, fertig;
  float hilf;
  int hilfop;

  fertig = 0;
  k = anzahl-1;

  while (k>0 && !fertig)
    {
      fertig = 1;
      for (i=0; i<k; i++)
	{
	  if ( feld[i] > feld[i+1] )
	    {
	      hilf = feld[i];
	      hilfop = feldop[i];
	      feld[i] = feld[i+1];
	      feldop[i] = feldop[i+1];
	      feld[i+1] = hilf;
	      feldop[i+1] = hilfop;
	      fertig = 0;
	    }
	}
      k--;
    }


  return;
}

/*------end--of-------BubbeleSort---Methode-------------*/



/*-------endend--startend--startstart--startover---functions-----*/

/*hhh*/
Bool startover(OpConn op1, OpConn op2) {

  int i, j, k;
  int l, m, p, q;

  for(l = 0; l < op1.num_E; l++){
    p = op1.E[l];
    for(m = 0; m < op2.num_E; m++){
      q = op2.E[m];
           
      for(i=0; i< gop_conn[gef_conn[p].op].action->effects->num_adds; i++)
	{
	  int x = gop_conn[gef_conn[p].op].action->effects->adds[i];
	
	  for(j=0;j< gop_conn[gef_conn[q].op].action->num_preconds; j++)
	    {

              int y = gop_conn[gef_conn[q].op].action->preconds[j];
         
              if (x == y) {

		int u = gop_conn[gef_conn[p].op].action->effects->adds_t[i];
		int v = gop_conn[gef_conn[q].op].action->preconds_t[j];
              
		if(u ==1 && v ==2)
		  return TRUE;
	      }
	    }
	}

      for(i=0; i< gop_conn[gef_conn[p].op].action->effects->num_adds; i++)
	{
	  int x = gop_conn[gef_conn[p].op].action->effects->adds[i];
	
	  for(j=0;j< gop_conn[gef_conn[q].op].action->effects->num_conditions; j++)
	    {

              int y = gop_conn[gef_conn[q].op].action->effects->conditions[j];
         
              if (x == y) {

		int u = gop_conn[gef_conn[p].op].action->effects->adds_t[i];
		int v = gop_conn[gef_conn[q].op].action->effects->conditions_t[j];
              
		if(u ==1 && v ==2)
		  return TRUE;
	      }
	    }
	}





      for(i=0;i< gop_conn[gef_conn[p].op].action->effects->num_dels ;i++)
	{
          int x = gop_conn[gef_conn[p].op].action->effects->dels[i];
     
	  for(j=0;j< gop_conn[gef_conn[q].op].action->num_preconds;j++)
	    {

              int y = gop_conn[gef_conn[q].op].action->preconds[j];
    
              if (x == y) {

		int u = gop_conn[gef_conn[p].op].action->effects->dels_t[i];
		int v = gop_conn[gef_conn[q].op].action->preconds_t[j];
            
		if(u==1 && v==2)
		  return TRUE;
	      }
	    }
	}

      for(i=0;i< gop_conn[gef_conn[p].op].action->effects->num_dels;i++)
	{
          int x = gop_conn[gef_conn[p].op].action->effects->dels[i];
     
	  for(j=0;j< gop_conn[gef_conn[q].op].action->effects->num_conditions;j++)
	    {

              int y = gop_conn[gef_conn[q].op].action->effects->conditions[j];
    
              if (x == y) {

		int u = gop_conn[gef_conn[p].op].action->effects->dels_t[i];
		int v = gop_conn[gef_conn[q].op].action->effects->conditions_t[j];
            
		if(u==1 && v==2)
		  return TRUE;
	      }
	    }
	}


      /*-----------Numeric-dependence-test-----------*/


      for(i=0; i< gop_conn[gef_conn[p].op].action->num_lnf_preconds; i++)
	{
          LnfExpNode *x = gop_conn[gef_conn[p].op].action->lnf_preconds_lh[i];
      
	  for(j=0;j< gop_conn[gef_conn[q].op].action->effects->num_lnf_effects; j++)
	    {

	      int y = gop_conn[gef_conn[q].op].action->effects->lnf_effects_fl[j];
       
	      for( k = 0; k < x->num_pF; k++ )
		{
     
		  if (x->pF[k]== y)
		    {
		      int u = gop_conn[gef_conn[p].op].action->lnf_preconds_lh_t[i];
		      int v = gop_conn[gef_conn[q].op].action->effects->lnf_effects_fl_t[j];
            
		      if(u==1 && v==2)	     
			return TRUE;
                
		    }
		}
	    }

	}


      for(i=0; i< gop_conn[gef_conn[p].op].action->effects->num_lnf_effects; i++)
	{
          int x = gop_conn[gef_conn[p].op].action->effects->lnf_effects_fl[i];
        
	  for(j=0;j< gop_conn[gef_conn[q].op].action->num_lnf_preconds; j++)
	    {

	      LnfExpNode *y = gop_conn[gef_conn[q].op].action->lnf_preconds_lh[j];
	
	      for( k = 0; k < y->num_pF; k++ )
		{             
	  
		  if (x == y->pF[k])
		    {
		      int u = gop_conn[gef_conn[p].op].action->effects->lnf_effects_fl_t[i];
		      int v = gop_conn[gef_conn[q].op].action->lnf_preconds_lh_t[j];
            
		      if(u==1 && v==2)	     
			return TRUE;

		    }
		}
	    }

	}


      for(i=0; i< gop_conn[gef_conn[p].op].action->effects->num_lnf_effects; i++)
	{
	  int x = gop_conn[gef_conn[p].op].action->effects->lnf_effects_fl[i];
       
        
	  for(j=0;j< gop_conn[gef_conn[q].op].action->effects->num_lnf_effects; j++)
	    {
	      int y = gop_conn[gef_conn[q].op].action->effects->lnf_effects_fl[j];                          
              if (x == y)
		{
		  int u = gop_conn[gef_conn[p].op].action->effects->lnf_effects_fl_t[i];
		  int v = gop_conn[gef_conn[q].op].action->effects->lnf_effects_fl_t[j];;
            
		  if(u==1 && v==2)	     
		    return TRUE;

		}
	    }       
	}

   
      for(i=0; i< gop_conn[gef_conn[p].op].action->effects->num_lnf_conditions; i++)
	{
          LnfExpNode *x = gop_conn[gef_conn[p].op].action->effects->lnf_conditions_lh[i];
     
	  for(j=0;j< gop_conn[gef_conn[q].op].action->effects->num_lnf_effects; j++)
	    {

	      int y = gop_conn[gef_conn[q].op].action->effects->lnf_effects_fl[j];          
	      for( k = 0; k < x->num_pF; k++ )
		{
	   
		  if (x->pF[k]== y)
		    {
		      int u = gop_conn[gef_conn[p].op].action->effects->lnf_conditions_lh_t[i];
		      int v = gop_conn[gef_conn[q].op].action->effects->lnf_effects_fl_t[j];;
            
		      if(u==1 && v==2)	     
			return TRUE;
                
		    }
		}
	    }
	}

   
      for(i=0; i< gop_conn[gef_conn[p].op].action->effects->num_lnf_effects; i++)
	{
          int x = gop_conn[gef_conn[p].op].action->effects->lnf_effects_fl[i];

        
	  for(j=0;j< gop_conn[gef_conn[q].op].action->effects->num_lnf_conditions; j++)
	    {

	      LnfExpNode *y = gop_conn[gef_conn[q].op].action->effects->lnf_conditions_lh[j];
	

	      for( k = 0; k < y->num_pF; k++ )
		{             
	     
		  if (x == y->pF[k])
		    {
		      int u = gop_conn[gef_conn[p].op].action->effects->lnf_effects_fl_t[i];;
		      int v = gop_conn[gef_conn[q].op].action->effects->lnf_conditions_lh_t[j];
            
		      if(u==1 && v==2)	     
			return TRUE;

		    }
		}
	    }
	}

       
    }
  }


  return FALSE;
}



Bool startstart(OpConn op1, OpConn op2) {

  int i, j, k;
  int l, m, p, q;

  for(l = 0; l < op1.num_E; l++){
    p = op1.E[l];
    for(m = 0; m < op2.num_E; m++){
      q = op2.E[m];

                 
           
      for(i=0; i< gop_conn[gef_conn[p].op].action->effects->num_adds; i++)
	{
	  int x = gop_conn[gef_conn[p].op].action->effects->adds[i];
	
	  for(j=0;j< gop_conn[gef_conn[q].op].action->num_preconds; j++)
	    {

              int y = gop_conn[gef_conn[q].op].action->preconds[j];
         
              if (x == y) {

		int u = gop_conn[gef_conn[p].op].action->effects->adds_t[i];
		int v = gop_conn[gef_conn[q].op].action->preconds_t[j];
              
		if(u ==1 && v ==1)
		  return TRUE;
	      }
	    }
	}

      for(i=0; i< gop_conn[gef_conn[p].op].action->effects->num_adds; i++)
	{
	  int x = gop_conn[gef_conn[p].op].action->effects->adds[i];
	
	  for(j=0;j< gop_conn[gef_conn[q].op].action->effects->num_conditions; j++)
	    {

              int y = gop_conn[gef_conn[q].op].action->effects->conditions[j];
         
              if (x == y) {

		int u = gop_conn[gef_conn[p].op].action->effects->adds_t[i];
		int v = gop_conn[gef_conn[q].op].action->effects->conditions_t[j];
              
		if(u ==1 && v ==1)
		  return TRUE;
	      }
	    }
	}


      for(i=0;i< gop_conn[gef_conn[p].op].action->effects->num_dels;i++)
	{
          int x = gop_conn[gef_conn[p].op].action->effects->dels[i];
     
	  for(j=0;j< gop_conn[gef_conn[q].op].action->num_preconds;j++)
	    {

              int y = gop_conn[gef_conn[q].op].action->preconds[j];
    
              if (x == y) {

		int u = gop_conn[gef_conn[p].op].action->effects->dels_t[i];
		int v = gop_conn[gef_conn[q].op].action->preconds_t[j];
            
		if(u==1 && v==1)
		  return TRUE;
	      }
	    }
	}

      for(i=0;i< gop_conn[gef_conn[p].op].action->effects->num_dels;i++)
	{
          int x = gop_conn[gef_conn[p].op].action->effects->dels[i];
     
	  for(j=0;j< gop_conn[gef_conn[q].op].action->effects->num_conditions;j++)
	    {

              int y = gop_conn[gef_conn[q].op].action->effects->conditions[j];
    
              if (x == y) {

		int u = gop_conn[gef_conn[p].op].action->effects->dels_t[i];
		int v = gop_conn[gef_conn[q].op].action->effects->conditions_t[j];
            
		if(u==1 && v==1)
		  return TRUE;
	      }
	    }
	}


      for(i=0;i< gop_conn[gef_conn[p].op].action->num_preconds;i++)
	{
	  int x = gop_conn[gef_conn[p].op].action->preconds[i];
	  for(j=0;j< gop_conn[gef_conn[q].op].action->effects->num_adds;j++)
	    {
	      int y = gop_conn[gef_conn[q].op].action->effects->adds[j];
	   
	      if (x == y) {

		int u = gop_conn[gef_conn[p].op].action->preconds_t[i];
		int v = gop_conn[gef_conn[q].op].action->effects->adds_t[j];
	    
		if(u==1 && v==1)
		  return TRUE;

	      }

	    }

	  for(j=0;j< gop_conn[gef_conn[q].op].action->effects->num_dels;j++)
	    {
	      int y = gop_conn[gef_conn[q].op].action->effects->dels[j];
	      if (x == y) {

		int u = gop_conn[gef_conn[p].op].action->preconds_t[i];
		int v = gop_conn[gef_conn[q].op].action->effects->dels_t[j];
      
		if(u==1 && v==1)
		  return TRUE;
	      }
	    }

	}


      for(i=0;i< gop_conn[gef_conn[p].op].action->effects->num_conditions;i++)
	{
	  int x = gop_conn[gef_conn[p].op].action->effects->conditions[i];
	  for(j=0;j< gop_conn[gef_conn[q].op].action->effects->num_adds;j++)
	    {
	      int y = gop_conn[gef_conn[q].op].action->effects->adds[j];
	   
	      if (x == y) {

		int u = gop_conn[gef_conn[p].op].action->effects->conditions_t[i];
		int v = gop_conn[gef_conn[q].op].action->effects->adds_t[j];
	    
		if(u==1 && v==1)
		  return TRUE;

	      }

	    }

	  for(j=0;j< gop_conn[gef_conn[q].op].action->effects->num_dels;j++)
	    {
	      int y = gop_conn[gef_conn[q].op].action->effects->dels[j];
	      if (x == y) {

		int u = gop_conn[gef_conn[p].op].action->preconds_t[i];
		int v = gop_conn[gef_conn[q].op].action->effects->dels_t[j];
      
		if(u==1 && v==1)
		  return TRUE;
	      }
	    }

	}

         
      /*-----------Numeric-dependence-test-----------*/


      for(i=0; i< gop_conn[gef_conn[p].op].action->num_lnf_preconds; i++)
	{
          LnfExpNode *x = gop_conn[gef_conn[p].op].action->lnf_preconds_lh[i];
      
	  for(j=0;j< gop_conn[gef_conn[q].op].action->effects->num_lnf_effects; j++)
	    {

	      int y = gop_conn[gef_conn[q].op].action->effects->lnf_effects_fl[j];
       
	      for( k = 0; k < x->num_pF; k++ )
		{
	 
		  if (x->pF[k]== y)
		    {
		      int u = gop_conn[gef_conn[p].op].action->lnf_preconds_lh_t[i];
		      int v = gop_conn[gef_conn[q].op].action->effects->lnf_effects_fl_t[j];
            
		      if(u==1 && v==1)	     
			return TRUE;

		    }

		}

	    }
	}


      for(i=0; i< gop_conn[gef_conn[p].op].action->effects->num_lnf_effects; i++)
	{
          int x = gop_conn[gef_conn[p].op].action->effects->lnf_effects_fl[i];
        
	  for(j=0;j< gop_conn[gef_conn[q].op].action->num_lnf_preconds; j++)
	    {

	      LnfExpNode *y = gop_conn[gef_conn[q].op].action->lnf_preconds_lh[j];
	

	      for( k = 0; k < y->num_pF; k++ )
		{             
		  if (x == y->pF[k])
		    {
		      int u = gop_conn[gef_conn[p].op].action->effects->lnf_effects_fl_t[i];
		      int v = gop_conn[gef_conn[q].op].action->lnf_preconds_lh_t[j];
            
		      if(u==1 && v==1)	     
			return TRUE;

		    }
		}
	    }

	}

   

      for(i=0; i< gop_conn[gef_conn[p].op].action->effects->num_lnf_effects; i++)
	{
	  int x = gop_conn[gef_conn[p].op].action->effects->lnf_effects_fl[i];
        
	  for(j=0;j< gop_conn[gef_conn[q].op].action->effects->num_lnf_effects; j++)
	    {

	      int y = gop_conn[gef_conn[q].op].action->effects->lnf_effects_fl[j];          
             
              if (x == y)
		{
		  int u = gop_conn[gef_conn[p].op].action->effects->lnf_effects_fl_t[i];
		  int v = gop_conn[gef_conn[q].op].action->effects->lnf_effects_fl_t[j];;
            
		  if(u==1 && v==1)	     
		    return TRUE;

		}

	    }

	}

    
      for(i=0; i< gop_conn[gef_conn[p].op].action->effects->num_lnf_conditions; i++)
	{
          LnfExpNode *x = gop_conn[gef_conn[p].op].action->effects->lnf_conditions_lh[i];
     
	  for(j=0;j< gop_conn[gef_conn[q].op].action->effects->num_lnf_effects; j++)
	    {

	      int y = gop_conn[gef_conn[q].op].action->effects->lnf_effects_fl[j];          
	      for( k = 0; k < x->num_pF; k++ )
		{
	   
		  if (x->pF[k]== y)
		    {
		      int u = gop_conn[gef_conn[p].op].action->effects->lnf_conditions_lh_t[i];
		      int v = gop_conn[gef_conn[q].op].action->effects->lnf_effects_fl_t[j];;
            
		      if(u==1 && v==1)	     
			return TRUE;
                
		    }
		}
	    }
	}

   
      for(i=0; i< gop_conn[gef_conn[p].op].action->effects->num_lnf_effects; i++)
	{
          int x = gop_conn[gef_conn[p].op].action->effects->lnf_effects_fl[i];

        
	  for(j=0;j< gop_conn[gef_conn[q].op].action->effects->num_lnf_conditions; j++)
	    {

	      LnfExpNode *y = gop_conn[gef_conn[q].op].action->effects->lnf_conditions_lh[j];
	

	      for( k = 0; k < y->num_pF; k++ )
		{             
	     
		  if (x == y->pF[k])
		    {
		      int u = gop_conn[gef_conn[p].op].action->effects->lnf_effects_fl_t[i];;
		      int v = gop_conn[gef_conn[q].op].action->effects->lnf_conditions_lh_t[j];
            
		      if(u==1 && v==1)	     
			return TRUE;

		    }
		}
	    }
	}


    }
  }


  return FALSE;
}



Bool endend(OpConn op1, OpConn op2) {

  int i, j, k;
  int l, m, p, q;

  for(l = 0; l < op1.num_E; l++){
    p = op1.E[l];
    for(m = 0; m < op2.num_E; m++){
      q = op2.E[m];

                 
           
      for(i=0; i< gop_conn[gef_conn[p].op].action->effects->num_adds; i++)
	{
	  int x = gop_conn[gef_conn[p].op].action->effects->adds[i];
	
	  for(j=0;j< gop_conn[gef_conn[q].op].action->num_preconds; j++)
	    {

              int y = gop_conn[gef_conn[q].op].action->preconds[j];
         
              if (x == y) {

		int u = gop_conn[gef_conn[p].op].action->effects->adds_t[i];
		int v = gop_conn[gef_conn[q].op].action->preconds_t[j];
              
		if(u ==3 && v ==3)
		  return TRUE;
	      }
	    }
	}

      for(i=0; i< gop_conn[gef_conn[p].op].action->effects->num_adds; i++)
	{
	  int x = gop_conn[gef_conn[p].op].action->effects->adds[i];
	
	  for(j=0;j< gop_conn[gef_conn[q].op].action->effects->num_conditions; j++)
	    {

              int y = gop_conn[gef_conn[q].op].action->effects->conditions[j];
         
              if (x == y) {

		int u = gop_conn[gef_conn[p].op].action->effects->adds_t[i];
		int v = gop_conn[gef_conn[q].op].action->effects->conditions_t[j];
              
		if(u ==3 && v ==3)
		  return TRUE;
	      }
	    }
	}


      for(i=0;i< gop_conn[gef_conn[p].op].action->effects->num_dels;i++)
	{
          int x = gop_conn[gef_conn[p].op].action->effects->dels[i];
     
	  for(j=0;j< gop_conn[gef_conn[q].op].action->num_preconds;j++)
	    {

              int y = gop_conn[gef_conn[q].op].action->preconds[j];
    
              if (x == y) {

		int u = gop_conn[gef_conn[p].op].action->effects->dels_t[i];
		int v = gop_conn[gef_conn[q].op].action->preconds_t[j];
            
		if(u==3 && v==3)
		  return TRUE;
	      }
	    }
	}

      for(i=0;i< gop_conn[gef_conn[p].op].action->effects->num_dels;i++)
	{
          int x = gop_conn[gef_conn[p].op].action->effects->dels[i];
     
	  for(j=0;j< gop_conn[gef_conn[q].op].action->effects->num_conditions;j++)
	    {

              int y = gop_conn[gef_conn[q].op].action->effects->conditions[j];
    
              if (x == y) {

		int u = gop_conn[gef_conn[p].op].action->effects->dels_t[i];
		int v = gop_conn[gef_conn[q].op].action->effects->conditions_t[j];
            
		if(u==3 && v==3)
		  return TRUE;
	      }
	    }
	}


      for(i=0;i< gop_conn[gef_conn[p].op].action->num_preconds;i++)
	{
	  int x = gop_conn[gef_conn[p].op].action->preconds[i];
	  for(j=0;j< gop_conn[gef_conn[q].op].action->effects->num_adds;j++)
	    {
	      int y = gop_conn[gef_conn[q].op].action->effects->adds[j];
	   
	      if (x == y) {

		int u = gop_conn[gef_conn[p].op].action->preconds_t[i];
		int v = gop_conn[gef_conn[q].op].action->effects->adds_t[j];
	    
		if(u==3 && v==3)
		  return TRUE;

	      }

	    }

	  for(j=0;j< gop_conn[gef_conn[q].op].action->effects->num_dels;j++)
	    {
	      int y = gop_conn[gef_conn[q].op].action->effects->dels[j];
	      if (x == y) {

		int u = gop_conn[gef_conn[p].op].action->preconds_t[i];
		int v = gop_conn[gef_conn[q].op].action->effects->dels_t[j];
      
		if(u==3 && v==3)
		  return TRUE;
	      }
	    }

	}


      for(i=0;i< gop_conn[gef_conn[p].op].action->effects->num_conditions;i++)
	{
	  int x = gop_conn[gef_conn[p].op].action->effects->conditions[i];
	  for(j=0;j< gop_conn[gef_conn[q].op].action->effects->num_adds;j++)
	    {
	      int y = gop_conn[gef_conn[q].op].action->effects->adds[j];
	   
	      if (x == y) {

		int u = gop_conn[gef_conn[p].op].action->effects->conditions_t[i];
		int v = gop_conn[gef_conn[q].op].action->effects->adds_t[j];
	    
		if(u==3 && v==3)
		  return TRUE;

	      }

	    }

	  for(j=0;j< gop_conn[gef_conn[q].op].action->effects->num_dels;j++)
	    {
	      int y = gop_conn[gef_conn[q].op].action->effects->dels[j];
	      if (x == y) {

		int u = gop_conn[gef_conn[p].op].action->preconds_t[i];
		int v = gop_conn[gef_conn[q].op].action->effects->dels_t[j];
      
		if(u==3 && v==3)
		  return TRUE;
	      }
	    }

	}

         
      /*-----------Numeric-dependence-test-----------*/


      for(i=0; i< gop_conn[gef_conn[p].op].action->num_lnf_preconds; i++)
	{
          LnfExpNode *x = gop_conn[gef_conn[p].op].action->lnf_preconds_lh[i];
      
	  for(j=0;j< gop_conn[gef_conn[q].op].action->effects->num_lnf_effects; j++)
	    {

	      int y = gop_conn[gef_conn[q].op].action->effects->lnf_effects_fl[j];
       
	      for( k = 0; k < x->num_pF; k++ )
		{
	 
		  if (x->pF[k]== y)
		    {
		      int u = gop_conn[gef_conn[p].op].action->lnf_preconds_lh_t[i];
		      int v = gop_conn[gef_conn[q].op].action->effects->lnf_effects_fl_t[j];
            
		      if(u==3 && v==3)	     
			return TRUE;

		    }

		}

	    }
	}


      for(i=0; i< gop_conn[gef_conn[p].op].action->effects->num_lnf_effects; i++)
	{
          int x = gop_conn[gef_conn[p].op].action->effects->lnf_effects_fl[i];
        
	  for(j=0;j< gop_conn[gef_conn[q].op].action->num_lnf_preconds; j++)
	    {

	      LnfExpNode *y = gop_conn[gef_conn[q].op].action->lnf_preconds_lh[j];
	

	      for( k = 0; k < y->num_pF; k++ )
		{             
		  if (x == y->pF[k])
		    {
		      int u = gop_conn[gef_conn[p].op].action->effects->lnf_effects_fl_t[i];
		      int v = gop_conn[gef_conn[q].op].action->lnf_preconds_lh_t[j];
            
		      if(u==3 && v==3)	     
			return TRUE;

		    }
		}
	    }

	}

   

      for(i=0; i< gop_conn[gef_conn[p].op].action->effects->num_lnf_effects; i++)
	{
	  int x = gop_conn[gef_conn[p].op].action->effects->lnf_effects_fl[i];
        
	  for(j=0;j< gop_conn[gef_conn[q].op].action->effects->num_lnf_effects; j++)
	    {

	      int y = gop_conn[gef_conn[q].op].action->effects->lnf_effects_fl[j];          
             
              if (x == y)
		{
		  int u = gop_conn[gef_conn[p].op].action->effects->lnf_effects_fl_t[i];
		  int v = gop_conn[gef_conn[q].op].action->effects->lnf_effects_fl_t[j];;
            
		  if(u==3 && v==3)	     
		    return TRUE;

		}

	    }

	}

    
      for(i=0; i< gop_conn[gef_conn[p].op].action->effects->num_lnf_conditions; i++)
	{
          LnfExpNode *x = gop_conn[gef_conn[p].op].action->effects->lnf_conditions_lh[i];
     
	  for(j=0;j< gop_conn[gef_conn[q].op].action->effects->num_lnf_effects; j++)
	    {

	      int y = gop_conn[gef_conn[q].op].action->effects->lnf_effects_fl[j];          
	      for( k = 0; k < x->num_pF; k++ )
		{
	   
		  if (x->pF[k]== y)
		    {
		      int u = gop_conn[gef_conn[p].op].action->effects->lnf_conditions_lh_t[i];
		      int v = gop_conn[gef_conn[q].op].action->effects->lnf_effects_fl_t[j];;
            
		      if(u==3 && v==3)	     
			return TRUE;
                
		    }
		}
	    }
	}

   
      for(i=0; i< gop_conn[gef_conn[p].op].action->effects->num_lnf_effects; i++)
	{
          int x = gop_conn[gef_conn[p].op].action->effects->lnf_effects_fl[i];

        
	  for(j=0;j< gop_conn[gef_conn[q].op].action->effects->num_lnf_conditions; j++)
	    {

	      LnfExpNode *y = gop_conn[gef_conn[q].op].action->effects->lnf_conditions_lh[j];
	

	      for( k = 0; k < y->num_pF; k++ )
		{             
	     
		  if (x == y->pF[k])
		    {
		      int u = gop_conn[gef_conn[p].op].action->effects->lnf_effects_fl_t[i];;
		      int v = gop_conn[gef_conn[q].op].action->effects->lnf_conditions_lh_t[j];
            
		      if(u==3 && v==3)	     
			return TRUE;

		    }
		}
	    }
	}


    }
  }
 

  return FALSE;
}



Bool endstart(OpConn op1, OpConn op2) {

  int i, j, k;
  int l, m, p, q;

  for(l = 0; l < op1.num_E; l++){
    p = op1.E[l];
    for(m = 0; m < op2.num_E; m++){
      q = op2.E[m];
           
      for(i=0; i< gop_conn[gef_conn[p].op].action->effects->num_adds; i++)
	{
	  int x = gop_conn[gef_conn[p].op].action->effects->adds[i];
	
	  for(j=0;j< gop_conn[gef_conn[q].op].action->num_preconds; j++)
	    {

              int y = gop_conn[gef_conn[q].op].action->preconds[j];
         
              if (x == y) {

		int u = gop_conn[gef_conn[p].op].action->effects->adds_t[i];
		int v = gop_conn[gef_conn[q].op].action->preconds_t[j];
              
		if(u ==3 && v ==1)
		  return TRUE;
	      }
	    }
	}

      for(i=0; i< gop_conn[gef_conn[p].op].action->effects->num_adds; i++)
	{
	  int x = gop_conn[gef_conn[p].op].action->effects->adds[i];
	
	  for(j=0;j< gop_conn[gef_conn[q].op].action->effects->num_conditions; j++)
	    {

              int y = gop_conn[gef_conn[q].op].action->effects->conditions[j];
         
              if (x == y) {

		int u = gop_conn[gef_conn[p].op].action->effects->adds_t[i];
		int v = gop_conn[gef_conn[q].op].action->effects->conditions_t[j];
              
		if(u ==3 && v ==1)
		  return TRUE;
	      }
	    }
	}





      for(i=0;i< gop_conn[gef_conn[p].op].action->effects->num_dels ;i++)
	{
          int x = gop_conn[gef_conn[p].op].action->effects->dels[i];
     
	  for(j=0;j< gop_conn[gef_conn[q].op].action->num_preconds;j++)
	    {

              int y = gop_conn[gef_conn[q].op].action->preconds[j];
    
              if (x == y) {

		int u = gop_conn[gef_conn[p].op].action->effects->dels_t[i];
		int v = gop_conn[gef_conn[q].op].action->preconds_t[j];
            
		if(u==3 && v==1)
		  return TRUE;
	      }
	    }
	}

      for(i=0;i< gop_conn[gef_conn[p].op].action->effects->num_dels;i++)
	{
          int x = gop_conn[gef_conn[p].op].action->effects->dels[i];
     
	  for(j=0;j< gop_conn[gef_conn[q].op].action->effects->num_conditions;j++)
	    {

              int y = gop_conn[gef_conn[q].op].action->effects->conditions[j];
    
              if (x == y) {

		int u = gop_conn[gef_conn[p].op].action->effects->dels_t[i];
		int v = gop_conn[gef_conn[q].op].action->effects->conditions_t[j];
            
		if(u==3 && v==1)
		  return TRUE;
	      }
	    }
	}


      /*-----------Numeric-dependence-test-----------*/


      for(i=0; i< gop_conn[gef_conn[p].op].action->num_lnf_preconds; i++)
	{
          LnfExpNode *x = gop_conn[gef_conn[p].op].action->lnf_preconds_lh[i];
      
	  for(j=0;j< gop_conn[gef_conn[q].op].action->effects->num_lnf_effects; j++)
	    {

	      int y = gop_conn[gef_conn[q].op].action->effects->lnf_effects_fl[j];
       
	      for( k = 0; k < x->num_pF; k++ )
		{
     
		  if (x->pF[k]== y)
		    {
		      int u = gop_conn[gef_conn[p].op].action->lnf_preconds_lh_t[i];
		      int v = gop_conn[gef_conn[q].op].action->effects->lnf_effects_fl_t[j];
            
		      if(u==3 && v==1)	     
			return TRUE;
                
		    }
		}
	    }

	}


      for(i=0; i< gop_conn[gef_conn[p].op].action->effects->num_lnf_effects; i++)
	{
          int x = gop_conn[gef_conn[p].op].action->effects->lnf_effects_fl[i];
        
	  for(j=0;j< gop_conn[gef_conn[q].op].action->num_lnf_preconds; j++)
	    {

	      LnfExpNode *y = gop_conn[gef_conn[q].op].action->lnf_preconds_lh[j];
	
	      for( k = 0; k < y->num_pF; k++ )
		{             
	  
		  if (x == y->pF[k])
		    {
		      int u = gop_conn[gef_conn[p].op].action->effects->lnf_effects_fl_t[i];
		      int v = gop_conn[gef_conn[q].op].action->lnf_preconds_lh_t[j];
            
		      if(u==3 && v==1)	     
			return TRUE;

		    }
		}
	    }

	}


      for(i=0; i< gop_conn[gef_conn[p].op].action->effects->num_lnf_effects; i++)
	{
	  int x = gop_conn[gef_conn[p].op].action->effects->lnf_effects_fl[i];
       
        
	  for(j=0;j< gop_conn[gef_conn[q].op].action->effects->num_lnf_effects; j++)
	    {
	      int y = gop_conn[gef_conn[q].op].action->effects->lnf_effects_fl[j];                          
              if (x == y)
		{
		  int u = gop_conn[gef_conn[p].op].action->effects->lnf_effects_fl_t[i];
		  int v = gop_conn[gef_conn[q].op].action->effects->lnf_effects_fl_t[j];;
            
		  if(u==3 && v==1)	     
		    return TRUE;

		}
	    }       
	}

   
      for(i=0; i< gop_conn[gef_conn[p].op].action->effects->num_lnf_conditions; i++)
	{
          LnfExpNode *x = gop_conn[gef_conn[p].op].action->effects->lnf_conditions_lh[i];
     
	  for(j=0;j< gop_conn[gef_conn[q].op].action->effects->num_lnf_effects; j++)
	    {

	      int y = gop_conn[gef_conn[q].op].action->effects->lnf_effects_fl[j];          
	      for( k = 0; k < x->num_pF; k++ )
		{
	   
		  if (x->pF[k]== y)
		    {
		      int u = gop_conn[gef_conn[p].op].action->effects->lnf_conditions_lh_t[i];
		      int v = gop_conn[gef_conn[q].op].action->effects->lnf_effects_fl_t[j];;
            
		      if(u==3 && v==1)	     
			return TRUE;
                
		    }
		}
	    }
	}

   
      for(i=0; i< gop_conn[gef_conn[p].op].action->effects->num_lnf_effects; i++)
	{
          int x = gop_conn[gef_conn[p].op].action->effects->lnf_effects_fl[i];

        
	  for(j=0;j< gop_conn[gef_conn[q].op].action->effects->num_lnf_conditions; j++)
	    {

	      LnfExpNode *y = gop_conn[gef_conn[q].op].action->effects->lnf_conditions_lh[j];
	

	      for( k = 0; k < y->num_pF; k++ )
		{             
	     
		  if (x == y->pF[k])
		    {
		      int u = gop_conn[gef_conn[p].op].action->effects->lnf_effects_fl_t[i];;
		      int v = gop_conn[gef_conn[q].op].action->effects->lnf_conditions_lh_t[j];
            
		      if(u==3 && v==1)	     
			return TRUE;

		    }
		}
	    }
	}

       
    }
  }

 

  return FALSE;
}

Bool endover(OpConn op1, OpConn op2) {

  int i, j, k;
  int l, m, p, q;

  for(l = 0; l < op1.num_E; l++){
    p = op1.E[l];
    for(m = 0; m < op2.num_E; m++){
      q = op2.E[m];
           
      for(i=0; i< gop_conn[gef_conn[p].op].action->effects->num_adds; i++)
	{
	  int x = gop_conn[gef_conn[p].op].action->effects->adds[i];
	
	  for(j=0;j< gop_conn[gef_conn[q].op].action->num_preconds; j++)
	    {

              int y = gop_conn[gef_conn[q].op].action->preconds[j];
         
              if (x == y) {

		int u = gop_conn[gef_conn[p].op].action->effects->adds_t[i];
		int v = gop_conn[gef_conn[q].op].action->preconds_t[j];
              
		if(u ==3 && v ==2)
		  return TRUE;
	      }
	    }
	}

      for(i=0; i< gop_conn[gef_conn[p].op].action->effects->num_adds; i++)
	{
	  int x = gop_conn[gef_conn[p].op].action->effects->adds[i];
	
	  for(j=0;j< gop_conn[gef_conn[q].op].action->effects->num_conditions; j++)
	    {

              int y = gop_conn[gef_conn[q].op].action->effects->conditions[j];
         
              if (x == y) {

		int u = gop_conn[gef_conn[p].op].action->effects->adds_t[i];
		int v = gop_conn[gef_conn[q].op].action->effects->conditions_t[j];
              
		if(u ==3 && v ==2)
		  return TRUE;
	      }
	    }
	}





      for(i=0;i< gop_conn[gef_conn[p].op].action->effects->num_dels ;i++)
	{
          int x = gop_conn[gef_conn[p].op].action->effects->dels[i];
     
	  for(j=0;j< gop_conn[gef_conn[q].op].action->num_preconds;j++)
	    {

              int y = gop_conn[gef_conn[q].op].action->preconds[j];
    
              if (x == y) {

		int u = gop_conn[gef_conn[p].op].action->effects->dels_t[i];
		int v = gop_conn[gef_conn[q].op].action->preconds_t[j];
            
		if(u==3 && v==2)
		  return TRUE;
	      }
	    }
	}

      for(i=0;i< gop_conn[gef_conn[p].op].action->effects->num_dels;i++)
	{
          int x = gop_conn[gef_conn[p].op].action->effects->dels[i];
     
	  for(j=0;j< gop_conn[gef_conn[q].op].action->effects->num_conditions;j++)
	    {

              int y = gop_conn[gef_conn[q].op].action->effects->conditions[j];
    
              if (x == y) {

		int u = gop_conn[gef_conn[p].op].action->effects->dels_t[i];
		int v = gop_conn[gef_conn[q].op].action->effects->conditions_t[j];
            
		if(u==3 && v==2)
		  return TRUE;
	      }
	    }
	}


      /*-----------Numeric-dependence-test-----------*/


      for(i=0; i< gop_conn[gef_conn[p].op].action->num_lnf_preconds; i++)
	{
          LnfExpNode *x = gop_conn[gef_conn[p].op].action->lnf_preconds_lh[i];
      
	  for(j=0;j< gop_conn[gef_conn[q].op].action->effects->num_lnf_effects; j++)
	    {

	      int y = gop_conn[gef_conn[q].op].action->effects->lnf_effects_fl[j];
       
	      for( k = 0; k < x->num_pF; k++ )
		{
     
		  if (x->pF[k]== y)
		    {
		      int u = gop_conn[gef_conn[p].op].action->lnf_preconds_lh_t[i];
		      int v = gop_conn[gef_conn[q].op].action->effects->lnf_effects_fl_t[j];
            
		      if(u==3 && v==2)	     
			return TRUE;
                
		    }
		}
	    }

	}


      for(i=0; i< gop_conn[gef_conn[p].op].action->effects->num_lnf_effects; i++)
	{
          int x = gop_conn[gef_conn[p].op].action->effects->lnf_effects_fl[i];
        
	  for(j=0;j< gop_conn[gef_conn[q].op].action->num_lnf_preconds; j++)
	    {

	      LnfExpNode *y = gop_conn[gef_conn[q].op].action->lnf_preconds_lh[j];
	
	      for( k = 0; k < y->num_pF; k++ )
		{             
	  
		  if (x == y->pF[k])
		    {
		      int u = gop_conn[gef_conn[p].op].action->effects->lnf_effects_fl_t[i];
		      int v = gop_conn[gef_conn[q].op].action->lnf_preconds_lh_t[j];
            
		      if(u==3 && v==2)	     
			return TRUE;

		    }
		}
	    }

	}


      for(i=0; i< gop_conn[gef_conn[p].op].action->effects->num_lnf_effects; i++)
	{
	  int x = gop_conn[gef_conn[p].op].action->effects->lnf_effects_fl[i];
       
        
	  for(j=0;j< gop_conn[gef_conn[q].op].action->effects->num_lnf_effects; j++)
	    {
	      int y = gop_conn[gef_conn[q].op].action->effects->lnf_effects_fl[j];                          
              if (x == y)
		{
		  int u = gop_conn[gef_conn[p].op].action->effects->lnf_effects_fl_t[i];
		  int v = gop_conn[gef_conn[q].op].action->effects->lnf_effects_fl_t[j];;
            
		  if(u==3 && v==2)	     
		    return TRUE;

		}
	    }       
	}

   
      for(i=0; i< gop_conn[gef_conn[p].op].action->effects->num_lnf_conditions; i++)
	{
          LnfExpNode *x = gop_conn[gef_conn[p].op].action->effects->lnf_conditions_lh[i];
     
	  for(j=0;j< gop_conn[gef_conn[q].op].action->effects->num_lnf_effects; j++)
	    {

	      int y = gop_conn[gef_conn[q].op].action->effects->lnf_effects_fl[j];          
	      for( k = 0; k < x->num_pF; k++ )
		{
	   
		  if (x->pF[k]== y)
		    {
		      int u = gop_conn[gef_conn[p].op].action->effects->lnf_conditions_lh_t[i];
		      int v = gop_conn[gef_conn[q].op].action->effects->lnf_effects_fl_t[j];;
            
		      if(u==3 && v==2)	     
			return TRUE;
                
		    }
		}
	    }
	}

   
      for(i=0; i< gop_conn[gef_conn[p].op].action->effects->num_lnf_effects; i++)
	{
          int x = gop_conn[gef_conn[p].op].action->effects->lnf_effects_fl[i];

        
	  for(j=0;j< gop_conn[gef_conn[q].op].action->effects->num_lnf_conditions; j++)
	    {

	      LnfExpNode *y = gop_conn[gef_conn[q].op].action->effects->lnf_conditions_lh[j];
	

	      for( k = 0; k < y->num_pF; k++ )
		{             
	     
		  if (x == y->pF[k])
		    {
		      int u = gop_conn[gef_conn[p].op].action->effects->lnf_effects_fl_t[i];;
		      int v = gop_conn[gef_conn[q].op].action->effects->lnf_conditions_lh_t[j];
            
		      if(u==3 && v==2)	     
			return TRUE;

		    }
		}
	    }
	}

       
    }
  }
  

  return FALSE;
}

/*------end---of---startstart---overstart--------functions-----*/
/*
 *--------durativPert-function-and-depend---------------
 */

/*
 *--------depend-function---numeric-and-strips------------------
 */
Bool depend(OpConn op1,OpConn op2){


  int i, j, k;
  int l, m, p, q;

  for(l = 0; l < op1.num_E; l++){
    p = op1.E[l];
    for(m = 0; m < op2.num_E; m++){
      q = op2.E[m];
           
      for(i=0; i< gef_conn[p].num_A; i++)
	{
	  for(j=0;j< gef_conn[q].num_PC; j++)
	    {
	      int x = gef_conn[p].A[i];
	      int y = gef_conn[q].PC[j];
	      if (x == y) {
		return TRUE;
	      }
	    }
	}

      for(i=0;i< gef_conn[p].num_D;i++)
	{
	  for(j=0;j< gef_conn[q].num_PC;j++)
	    {
	      int x = gef_conn[p].D[i];
	      int y = gef_conn[q].PC[j];
	      if (x == y) {
		return TRUE;
	      }
	    }
	}

      for(i=0;i< gef_conn[p].num_PC;i++)
	{
	  for(j=0;j< gef_conn[q].num_A;j++)
	    {
	      int x = gef_conn[p].PC[i];
	      int y = gef_conn[q].A[j];
	      if (x == y) {
		return TRUE;
	      }
	    }

	  for(j=0;j< gef_conn[q].num_D;j++)
	    {
	      int x = gef_conn[p].PC[i];
	      int y = gef_conn[q].D[j];
	      if (x == y) {
		return TRUE;

	      }
	    }
	}

      /*-----------Numeric-dependence-test-----------*/

   

      for(i=0; i< gop_conn[gef_conn[p].op].action->num_lnf_preconds; i++)
	{
          LnfExpNode *x = gop_conn[gef_conn[p].op].action->lnf_preconds_lh[i];
     
	  for(j=0;j< gop_conn[gef_conn[q].op].action->effects->num_lnf_effects; j++)
	    {

	      int y = gop_conn[gef_conn[q].op].action->effects->lnf_effects_fl[j];
          
       
	      for( k = 0; k < x->num_pF; k++ )
		{
	   
		  if (x->pF[k]== y)
		    {
		      return TRUE;
                
		    }
		}
	    }
	}

   
      for(i=0; i< gop_conn[gef_conn[p].op].action->effects->num_lnf_effects; i++)
	{
          int x = gop_conn[gef_conn[p].op].action->effects->lnf_effects_fl[i];

        
	  for(j=0;j< gop_conn[gef_conn[q].op].action->num_lnf_preconds; j++)
	    {

	      LnfExpNode *y = gop_conn[gef_conn[q].op].action->lnf_preconds_lh[j];
	

	      for( k = 0; k < y->num_pF; k++ )
		{             
	     
		  if (x == y->pF[k])
		    {
             
		      return TRUE;

		    }
		}
	    }
	}


      for(i=0; i< gop_conn[gef_conn[p].op].action->effects->num_lnf_effects; i++)
	{
	  int x = gop_conn[gef_conn[p].op].action->effects->lnf_effects_fl[i];

	  if (fl_isSame_to_isViolated( x )) continue; 
        
	  for(j=0;j< gop_conn[gef_conn[q].op].action->effects->num_lnf_effects; j++)
	    {

	      int y = gop_conn[gef_conn[q].op].action->effects->lnf_effects_fl[j];                      
             
	      if (fl_isSame_to_isViolated( y )) continue; 
              if (x == y)
		{
	      
		  return TRUE;

		}

	    }
	}


      for(i=0; i< gop_conn[gef_conn[p].op].action->effects->num_lnf_conditions; i++)
	{
          LnfExpNode *x = gop_conn[gef_conn[p].op].action->effects->lnf_conditions_lh[i];
     
	  for(j=0;j< gop_conn[gef_conn[q].op].action->effects->num_lnf_effects; j++)
	    {

	      int y = gop_conn[gef_conn[q].op].action->effects->lnf_effects_fl[j];
          
	      for( k = 0; k < x->num_pF; k++ )
		{
	   
		  if (x->pF[k]== y)
		    {
		      return TRUE;
                
		    }
		}
	    }
	}

   
      for(i=0; i< gop_conn[gef_conn[p].op].action->effects->num_lnf_effects; i++)
	{
          int x = gop_conn[gef_conn[p].op].action->effects->lnf_effects_fl[i];
        
	  for(j=0;j< gop_conn[gef_conn[q].op].action->effects->num_lnf_conditions; j++)
	    {

	      LnfExpNode *y = gop_conn[gef_conn[q].op].action->effects->lnf_conditions_lh[j];

	      for( k = 0; k < y->num_pF; k++ )
		{             
	     
		  if (x == y->pF[k])
		    {
             
		      return TRUE;

		    }
		}
	    }
	}

    }
  }

  return FALSE;

}

/*
 *--------End-of-depend--numeric-and-strips---------------------
 */



/*
 *----------durativPert-function-------------------
 */


float durativpert(void)
{
  float start[gnum_plan_ops];
  float erliest_start[gnum_plan_ops];
  float dur[gnum_plan_ops];

  
  int i;
  for( i=0;i < gnum_plan_ops;i++)
    {
  
      dur[i] = gop_conn[gplan_ops[i]].duration;

      start[i] = gop_conn[gplan_ops[i]].action->timeMin + dur[i];

      if (start[i] > gop_conn[gplan_ops[i]].action->timeMax) {
	/*
	  printf( "fail max time in PERT schedule %s\n",gop_conn[gplan_ops[i]].action->name );
	*/
 
	return 100000;
      }


      int j ;
      for(j=0;j < i;j++)
	{      
      
	  if (depend(gop_conn[gplan_ops[i]],gop_conn[gplan_ops[j]]))
	    {

	      if (start[j] + dur[i] + 0.01 > start[i])
		{
		  if(endstart(gop_conn[gplan_ops[j]],gop_conn[gplan_ops[i]]))
		    {

		      start[i] = start[j] + dur[i] + 0.01;
		    }
		  else
		    {
           
		      if(!endover(gop_conn[gplan_ops[j]],gop_conn[gplan_ops[i]]) && !startover(gop_conn[gplan_ops[i]],gop_conn[gplan_ops[j]]))             {


			Bool ss = startstart(gop_conn[gplan_ops[j]],gop_conn[gplan_ops[i]]);
			Bool ee = endend(gop_conn[gplan_ops[j]],gop_conn[gplan_ops[i]]) ;

			if (ss && ! ee) {
                                       

			  if (start[i] < start[j] - dur[j] + dur[i] + 0.01) {
			    start[i] = start[j] - dur[j] + dur[i] + 0.01;
			  }
			}
			if (!ss && ee) {

			  if (start[i] < start[j] + 0.01) {
			    start[i] = start[j] + 0.01;
			  }
			}
			if (ss && ee) {

			  if (dur[j] < dur[i]) {
			    if (start[i] < start[j] - dur[j] + dur[i] + 0.01) {
			      start[i] = start[j] - dur[j] + dur[i] + 0.01;
			    }
			  }
			  else { /* dur[i] >= dur[j]*/
			    if (start[i] < start[j] + 0.01) {
			      start[i] = start[j] + 0.01;
			    }
			  }
			}
			if (!ss && !ee) {/* start overall (or overall end)*/
                    		  
			  if (start[i] < start[j] - dur[j] + dur[i]) {
			    start[i] = start[j] - dur[j] + dur[i] ;
                             
                                         
			  }
                                                                       
			}

		      }
		      else {

			start[i] = start[j] + dur[i];
		      }

		    }


		}

                 

	      if (start[i] > gop_conn[gplan_ops[i]].action->timeMax) {
		/*
		  printf("fail maxtime in PERT schedule: %s\n",gop_conn[gplan_ops[i]].action->name);
		*/
		return 100000;
	      }
	    }
             
	}
    }
  
  /*-------------erliest_start-non-sortiert-show----------*/
  int r;
  for( r=0;r < gnum_plan_ops;r++)
    {
      erliest_start[r] = start[r]-dur[r];
     
    }
  /*--------end-of-----erliest_start-non-sortiert-show-----*/


  BubbleSort(erliest_start,gplan_ops,gnum_plan_ops );



  int maxi=0;double max = -1;
  int k;
  for ( k=0;k < gnum_plan_ops;k++)
    {
      if (start[k]> max) {
	max = start[k];
	maxi=k;
      }
      
    }


  /* For ParallelSolutin File */
  if((pfp = fopen("mipsPSolution.soln", "w"))==NULL) {
    printf("cannot open a file\n");
    exit(1);
  }
  /*fprintf(pfp, "; Time %.2f\n", 
    gtempl_time + greach_time + grelev_time + gLNF_time + gconn_time + gsearch_time );*/
  fprintf(pfp, "; ParsingTime\n");
  /*fprintf(pfp, "; ParsingTime %.2f\n", gtempl_time);*/
  fprintf(pfp, "; NrActions %i\n", gnum_plan_ops);
  fprintf(pfp, "; MakeSpan %.2f\n", start[maxi]);
  fprintf(pfp, "; MetricValue %.2f \n", prune);
  if ( gcmd_line.ehc )
    fprintf(pfp, "; PlaningTechnique ehc\n" );
  else if (gcmd_line.external)
    fprintf(pfp, "; PlaningTechnique External BFS\n" );
  else if (gcmd_line.dijkstra)
    fprintf(pfp, "; PlaningTechnique External Dijkstra\n" );
  else if (gcmd_line.branch_and_bound)
    fprintf(pfp, "; PlaningTechnique External Branch-and-Bound\n");
  else
    fprintf(pfp, "; PlaningTechnique Best-First\n" );

  /*printf("\n\nff: found legal parallel plan as follows\n");*/
  /*-------------test3----------------------*/

  float wert;
  int j=1;
  int s;

  for(s=0;s<gnum_plan_ops;s++)
    {
      wert=erliest_start[s];
      /*printf("\n");*/
      /*printf("Step[%i] \n",j);*/

      /*printf("wert[%i]=%.2f \n",s,erliest_start[s]);*/
      /*print_op_name( gplan_ops[s] );*/

      fprintf(pfp, "%.2f", erliest_start[s]);
      print_op_namePTimeSolutinoFile( gplan_ops[s] );

    
      while(erliest_start[s+1]==wert)
	{
	
	  /*printf("\n");*/

	  /*print_op_name( gplan_ops[s+1] );*/

	  fprintf(pfp, "%.2f", erliest_start[s]);
	  print_op_namePTimeSolutinoFile( gplan_ops[s+1] );

	  s++;           
       
       
	}
      j++;
    }

  fclose(pfp);

  /*--------end--of--test3-------------------*/

  return start[maxi];
}

/*
 *------End-of-durativPert-------------
 */


/*
 *------End-of-durativPert-function-and-depend---------------
 */

/*-------end--of-------temporal--functions--------------*/



/*
 *----------Pert-function---Numeric-and-strips----------------
 */
int pert(void)
{
  int start[MAX_PLAN_LENGTH];
  int i=0; 


  while ( i < gnum_plan_ops)
    {	 
      start[i] = 1;
	 
      int j = 0;
      while ( j < i)
	{

	  if (depend(gop_conn[gplan_ops[i]],gop_conn[gplan_ops[j]])) {
		 
	    if (start[j] + 1 > start[i]) {
		     
	      start[i] = start[j] + 1;
		     
	    }
	  }
	     
	  j++;	     
	     
	}
	 	 
      i++;
	 
    }
      
   
  int maxi = 0; double max = -1;
  i = 0;
  while ( i < gnum_plan_ops)
    {
      if (start[i] > max) {
	max = start[i];
	maxi = i;
      }
      i++;
    }

  /* For ParallelSolutin File */

  if((pfp = fopen("mipsPSolution.soln", "w"))==NULL) {
    printf("canno't open a file \n");
    exit(1);
  }
  /*fprintf(pfp, "; Time %.2f\n", 
    gtempl_time + greach_time + grelev_time + gLNF_time + gconn_time + gsearch_time );*/
  fprintf(pfp, "; ParsingTime\n");
  /*fprintf(pfp, "; ParsingTime %.2f\n", gtempl_time);*/
  fprintf(pfp, "; NrActions %i\n", gnum_plan_ops);
  fprintf(pfp, "; MakeSpan %d\n", start[maxi]);
  fprintf(pfp, "; MetricValue %.2f\n", prune );
  if ( gcmd_line.ehc )
    fprintf(pfp, "; PlaningTechique ehc\n" );
  else
    fprintf(pfp, "; PlaningTechique best-first\n" );


  int l;
  int r;
  for(l=0; l<max; l++)
    {

      for(r=0; r< gnum_plan_ops; r++)
	{

	  if(start[r]==l+1)
	    {
            
	      fprintf(pfp,"%i", l);
	      print_op_namePSolutinoFile( gplan_ops[r]);

	    }
	}

    }
  fclose(pfp);

  return start[maxi];
}

/*
 *------End-of-Pert-Numeric-and--strips------------
 */


/*
 *------User_durative_pert-------------
 */

 

float User_durative_pert(void)
{
  float start[gnum_plan_ops];
  float erliest_start[gnum_plan_ops];
  float dur[gnum_plan_ops];
  
  TokenListOperator *tok_op;
 

  make_array_for_instance_operators();
  make_array_of_xml_operators();
  

  /* make TokenList for depend_art;
   */


  /* for endstart TokenList
   */

  TokenList *endstart_TokenList = new_TokenList();
  endstart_TokenList->item = "ENDSTART";
   
     
   

  /* for non endstart TokenList
   */

  TokenList *non_endstart_TokenList = new_TokenList();
  non_endstart_TokenList->item = "NON";
  non_endstart_TokenList->next = new_TokenList();
  non_endstart_TokenList->next->item ="ENDSTART" ;
  

  /* for non endover and non startover TokenList
   */

  TokenList *non_endover_and_non_startover_TokenList = new_TokenList();
  non_endover_and_non_startover_TokenList->item = "NON";
  non_endover_and_non_startover_TokenList->next = new_TokenList();
  non_endover_and_non_startover_TokenList->next->item = "ENDOVER";
  non_endover_and_non_startover_TokenList->next->next = new_TokenList();
  non_endover_and_non_startover_TokenList->next->next->item = "AND";
  non_endover_and_non_startover_TokenList->next->next->next = new_TokenList();
  non_endover_and_non_startover_TokenList->next->next->next->item = "NON";
  non_endover_and_non_startover_TokenList->next->next->next->next = new_TokenList();
  non_endover_and_non_startover_TokenList->next->next->next->next->item = "STARTOVER";

      

  /* for startstart and non endend TokenList
   */

  TokenList *startstart_and_non_endend_TokenList = new_TokenList();
  startstart_and_non_endend_TokenList->item = "STARTSTART";
  startstart_and_non_endend_TokenList->next = new_TokenList();
  startstart_and_non_endend_TokenList->next->item = "AND";
  startstart_and_non_endend_TokenList->next->next = new_TokenList();
  startstart_and_non_endend_TokenList->next->next->item = "NON";
  startstart_and_non_endend_TokenList->next->next->next = new_TokenList();
  startstart_and_non_endend_TokenList->next->next->next->item = "ENDEND";



  /* for non startstart and endend TokenList
   */

   
  TokenList *non_startstart_and_endend_TokenList = new_TokenList();
  non_startstart_and_endend_TokenList->item = "NON";
  non_startstart_and_endend_TokenList->next = new_TokenList();
  non_startstart_and_endend_TokenList->next->item = "STARTSTART";
  non_startstart_and_endend_TokenList->next->next = new_TokenList();
  non_startstart_and_endend_TokenList->next->next->item = "AND";
  non_startstart_and_endend_TokenList->next->next->next = new_TokenList();
  non_startstart_and_endend_TokenList->next->next->next->item = "ENDEND";

    
  /* for startstart and endend TokenList
   */
    

  TokenList *startstart_and_endend_TokenList = new_TokenList();
  startstart_and_endend_TokenList->item = "STARTSTART";
  startstart_and_endend_TokenList->next = new_TokenList();
  startstart_and_endend_TokenList->next->item = "AND";
  startstart_and_endend_TokenList->next->next = new_TokenList();
  startstart_and_endend_TokenList->next->next->item = "ENDEND";
    
    


  /* for non startstart and non endend TokenList
   */
  TokenList *non_startstart_and_non_endend_TokenList= new_TokenList();
  non_startstart_and_non_endend_TokenList->item="NON";
  non_startstart_and_non_endend_TokenList->next=new_TokenList();
  non_startstart_and_non_endend_TokenList->next->item="STARTSTART";
  non_startstart_and_non_endend_TokenList->next->next=new_TokenList();
  non_startstart_and_non_endend_TokenList->next->next->item="AND";
  non_startstart_and_non_endend_TokenList->next->next->next=new_TokenList();
  non_startstart_and_non_endend_TokenList->next->next->next->item="NON";
  non_startstart_and_non_endend_TokenList->next->next->next->next=new_TokenList();
  non_startstart_and_non_endend_TokenList->next->next->next->next->item="ENDEND";

   
  /* for startover or endover TokenList
   */
  TokenList *startover_or_endover_TokenList = new_TokenList();
  startover_or_endover_TokenList->item="STARTOVER";
  startover_or_endover_TokenList->next=new_TokenList();
  startover_or_endover_TokenList->next->item="OR";
  startover_or_endover_TokenList->next->next=new_TokenList();
  startover_or_endover_TokenList->next->next->item="ENDOVER";

    

  /* end for make TokenList for depend_art;
   */

 
  int i;
  for( i=0;i < gnum_plan_ops;i++)
    {

    
      
      tok_op = search_for_instance_operator(instance_Operators, xml_operators[i]->name);
      dur[i] = tok_op->duration;          
      start[i] = tok_op->timemin + dur[i];
      /*start[i] = dur[i];*/


      if (start[i] > tok_op->timemax) { /*
					  printf( "fail max time in PERT schedule %s\n",gop_conn[gplan_ops[i]].action->name );
					*/
	return 100000;
      }



      int j ;
      for(j=0;j < i;j++)
	{
 

	  dur[j] = search_for_instance_operator(instance_Operators, xml_operators[j]->name)->duration;


	  if (isOp1DepentonOp2(xml_operators[i],xml_operators[j]))
	    {

           

	      if (start[j] + dur[i] > start[i])
		{   
                 
		     
		  if(getDependArtList(xml_operators[i], xml_operators[j])){ 
  


		    if(TestDependArt(getDependArtList(xml_operators[i], xml_operators[j]), endstart_TokenList))
		      {
                         

			start[i] = start[j] + dur[i]+ 0.01;
		      }
		    if(TestDependArt(getDependArtList(xml_operators[i], xml_operators[j]), non_endstart_TokenList))
		      {
                        
                         

			if(TestDependArt(getDependArtList(xml_operators[i], xml_operators[j]),non_endover_and_non_startover_TokenList))  {


			  if(TestDependArt(getDependArtList(xml_operators[i], xml_operators[j]),startstart_and_non_endend_TokenList)){


			    if (start[i] < start[j] - dur[j] + dur[i] + 0.01) {
			      start[i] = start[j] - dur[j] + dur[i] + 0.01;
			    }
			  }

			  if(TestDependArt(getDependArtList(xml_operators[i], xml_operators[j]),non_startstart_and_endend_TokenList)){                    

			    if (start[i] < start[j] + 0.01) {
			      start[i] = start[j] + 0.01;
			    }
			  }

			  if(TestDependArt(getDependArtList(xml_operators[i], xml_operators[j]),startstart_and_endend_TokenList)){


			    if (dur[j] < dur[i]) {
			      if (start[i] < start[j] - dur[j] + dur[i] + 0.01) {
				start[i] = start[j] - dur[j] + dur[i] + 0.01;
			      }
			    }
			    else { /* dur[i] >= dur[j]*/
			      if (start[i] < start[j] + 0.01) {
				start[i] = start[j] + 0.01;
			      }
			    }
			  }

			  if(TestDependArt(getDependArtList(xml_operators[i], xml_operators[j]),non_startstart_and_non_endend_TokenList)){                                  

			    if (start[i] < start[j] - dur[j] + dur[i]) {
			      start[i] = start[j] - dur[j] + dur[i];
			    }
			  }

			}

			if(TestDependArt(getDependArtList(xml_operators[i], xml_operators[j]),startover_or_endover_TokenList)){    

			  start[i] = start[j] + dur[i];
			}

		      }

		  }

		}

                 

	      if (start[i] > tok_op->timemax) {
		/*
		  printf("fail maxtime in PERT schedule: %s\n",gop_conn[gplan_ops[i]].action->name); */
		return 100000;
	      }
   

	    }
             
             
               
	}

  


      
    }

    
  /*-------------erliest_start-non-sortiert-show----------*/
  int r;
  for( r=0;r < gnum_plan_ops;r++)
    {
      erliest_start[r] = start[r]-dur[r];
     
    }
  /*--------end-of-----erliest_start-non-sortiert-show-----*/





  BubbleSort_for_Userp(erliest_start,xml_operators,gnum_plan_ops );

 

  int maxi=0;double max = -1;
  int k;
  for ( k=0;k < gnum_plan_ops;k++)
    {
      if (start[k]> max) {
	max = start[k];
	maxi=k;
      }
      
    }

  /* For ParallelSolutin File */
  if((pfp = fopen("mipsPSolution.soln", "w"))==NULL) {
    printf("canno't open a file\n");
    exit(1);
  }
  /*fprintf(pfp, "; Time %.2f\n", 
    gtempl_time + greach_time + grelev_time + gLNF_time + gconn_time + gsearch_time );*/
  fprintf(pfp, "; ParsingTime\n");
  /*fprintf(pfp, "; ParsingTime %.2f\n", gtempl_time);*/
  fprintf(pfp, "; NrActions %i\n", gnum_plan_ops);
  fprintf(pfp, "; MakeSpan %.2f\n", start[maxi]);
  fprintf(pfp, "; MetricValue %.2f\n", prune);
  if ( gcmd_line.ehc )
    fprintf(pfp, "; PlaningTechique ehc\n" );
  else
    fprintf(pfp, "; PlaningTechique best-first\n" );

  /*-------------test3----------------------*/

  float wert;
  int j=1;
  int s;

  for(s=0;s<gnum_plan_ops;s++)
    {
      wert=erliest_start[s];
      printf("\n\n");
      printf("Step[%i] \n",j);

      printf("wert[%i]=%.2f \n",s,erliest_start[s]);
      print_hidden_TokenList(xml_operators[s]->name, " " );

      fprintf(pfp, "%.2f:", erliest_start[s]);
      printTokenList_for_UPSolutinoFile(xml_operators[s]->name, " " );
      fprintf(pfp, ") [%.2f]\n",search_for_instance_operator(instance_Operators,xml_operators[s]->name )->duration);
       
      /* printf("\n");*/
      while(erliest_start[s+1]==wert)
	{
	  /* printf("wert[%i]=%f \n",s+1,erliest_start[s+1]);*/
	  printf("\n");

	  print_hidden_TokenList(xml_operators[s+1]->name, " " );

	  fprintf(pfp, "%.2f:", erliest_start[s]);
	  printTokenList_for_UPSolutinoFile(xml_operators[s+1]->name, " " );
	  fprintf(pfp, ") [%.2f]\n",search_for_instance_operator(instance_Operators,xml_operators[s+1]->name)->duration);          

	  s++;           
       
       
	}
      j++;
    }

  fclose(pfp);

  /*--------end--of--test3-------------------*/

  return start[maxi];
}




/*
 *------End-of-User_durative_pert-------------
 */


/*---Test for dependance between op1 and op2 ---------*/ 

Bool isOp1DepentonOp2(XMLOperator *op1,XMLOperator *op2)


{
  if(op1->depend_on_ops==NULL)
    return FALSE;
  else
    {
      XMLDependOnOp *Anker=NULL;
      for(Anker=op1->depend_on_ops;Anker!=NULL;Anker=Anker->next)
	{
	  if(CompareTwoPre(Anker->name,op2->name))
	    {
	      return TRUE;
	    }
	}
      return FALSE;
    }
}





/*liefert zeiger auf das dependsart falls die operator sind abhangig*/



TypedList* getDependArtList(XMLOperator *op1,XMLOperator *op2)


{
  if(op1->depend_on_ops==NULL)
    return NULL;
  else
    {
      XMLDependOnOp *Anker=NULL;
      for(Anker=op1->depend_on_ops;Anker!=NULL;Anker=Anker->next)
	{
	  if(CompareTwoPre(Anker->name,op2->name))
	    {
	      return Anker->depend_art;
	    }
	}
      return NULL;
    }
}


/*testet ob t also art der depend in  der liste vorhanden oder nicht*/


Bool TestDependArt(TypedList *tpl,TokenList *t )


{
  TypedList *Anker=NULL;
  for(Anker=tpl;Anker!=NULL;Anker=Anker->next)
    {
      if(CompareTwoPre(Anker->type,t))
	return TRUE;

    }
  return FALSE;
}


/* make array of instances operators with duration and timeintervalle
 */

void make_array_for_instance_operators(void)

{
  int i, j;
    
  TokenList *head, *neu_element, *current;
  TokenListOperator *tmp = NULL;
  Action *a;
  instance_Operators = ( TokenListOperator_pointer * ) calloc(gnum_plan_ops , sizeof( TokenListOperator_pointer ) );
   
  for ( i = 0; i < gnum_plan_ops; i++ ) {
       
    a = gop_conn[gplan_ops[i]].action;
         
    tmp = new_TokenListOperator();

    if(a->norm_operator || a->pseudo_action ){

      head = NULL;
      neu_element = ( TokenList * ) calloc( 1, sizeof(TokenList ) );
      neu_element->item = a->name;
      neu_element->next = head;
      head = neu_element;
      current = head;

      for(j = 0; j < a->num_name_vars;j++ ){

	while(current->next != NULL)
	  current = current->next;

	neu_element = ( TokenList * ) calloc( 1, sizeof(TokenList ) );             
	neu_element->item = gconstants[a->name_inst_table[j]];

	current->next = neu_element;
	neu_element->next = NULL;

      }
   
               

      tmp->name = head;
           

      tmp->duration = gop_conn[gplan_ops[i]].duration;
           
      tmp->timemin = a->timeMin;
      tmp->timemax = a->timeMax;

    }

    instance_Operators[i] = tmp;
           
  }

}


/* make of xml OperatorsListe array
 */

void make_array_of_xml_operators(void)

{

  int i = 0;
  XMLOperator *Anker1 =NULL;
  XMLOperator *tmp = NULL;
  xml_operators = ( XMLOperator_pointer * ) calloc(gnum_plan_ops , sizeof( XMLOperator_pointer ) );   
  for(Anker1=op_depend_op;Anker1!=NULL;Anker1=Anker1->next)
    {
      tmp  = new_XMLOperator();
      tmp->name = Anker1->name;
      tmp->depend_on_ops = Anker1->depend_on_ops;
     
      xml_operators[i++] = tmp;
	  
    }


}


/* for search in instance_Operators for durtion and timemin and timemax
 */

TokenListOperator * search_for_instance_operator(TokenListOperator_pointer *tok_op_p, TokenList *op_name)

{

  int i;
    
  for ( i = 0; i < gnum_plan_ops; i++ ) {

    if(CompareTwoPre(tok_op_p[i]->name,op_name))
      return tok_op_p[i];		
	    		    
  }
  return tok_op_p[i];
}


/*-------------BubbeleSort---function------------------------------*/
void BubbleSort_for_Userp(float feld[],XMLOperator_pointer *feldop, int anzahl)
{
  /* Lokale Hilfsvariablen*/
  int i, k, fertig;
  float hilf;
  XMLOperator_pointer *hilfop;

  fertig = 0;
  k = anzahl-1;

  while (k>0 && !fertig)
    {
      fertig = 1;
      for (i=0; i<k; i++)
	{
	  if ( feld[i] > feld[i+1] )
	    {
	      hilf = feld[i];
	      hilfop = &feldop[i];
	      feld[i] = feld[i+1];
	      feldop[i] = feldop[i+1];
	      feld[i+1] = hilf;
	      feldop[i+1] = *hilfop;
	      fertig = 0;
	    }
	}
      k--;
    }


  return;
}

/*------end--of-------BubbeleSort---Methode----------------------------*/

    
/*
 *------User_pert-------------
 */

 

float User_pert(void)
{
  float start[gnum_plan_ops];
  

  make_array_of_xml_operators();
  

 
  int i;
  for( i=0;i < gnum_plan_ops;i++)
    {

      start[i] = 1;

      int j ;
      for(j=0;j < i;j++)
	{
	  if (isOp1DepentonOp2(xml_operators[i],xml_operators[j]))
	    {
	      if (start[j] + 1 > start[i])
		{            
		  start[i] = start[j] + 1;

		}         
	    }
        }
                 
    }
             

  int maxi=0;double max = -1;
  int k;
  for ( k=0;k < gnum_plan_ops;k++)
    {
      if (start[k]> max) {
	max = start[k];
	maxi=k;
      }
      
    }

  /* For ParallelSolutin File */

  if((pfp = fopen("mipsPSolution.soln", "w"))==NULL) {
    printf("canno't open a file\n");
    exit(1);
  }
  /*fprintf(pfp, "; Time %.2f\n", 
    gtempl_time + greach_time + grelev_time + gLNF_time + gconn_time + gsearch_time );*/
  fprintf(pfp, "; ParsingTime\n");
  /*fprintf(pfp, "; ParsingTime %.2f\n", gtempl_time);*/
  fprintf(pfp, "; NrActions %i\n", gnum_plan_ops);
  fprintf(pfp, "; MakeSpan %.2f\n", start[maxi]);
  fprintf(pfp, "; MetricValue %.2f\n", prune);
  if ( gcmd_line.ehc )
    fprintf(pfp, "; PlaningTechique ehc\n" );
  else
    fprintf(pfp, "; PlaningTechique best-first\n" );

  /*-------------test3----------------------*/

 
  int l;
  int r;
  for(l=0; l<max; l++)
    {

      printf( "the %d operators step ", l);
      printf("\n");

      for(r=0; r< gnum_plan_ops; r++)
	{

	  if(start[r]==l+1)
	    {
            
	      fprintf(pfp,"%i :", l);

	      printf("\n");
	      print_hidden_TokenList(xml_operators[r]->name, " " );
                
	      printTokenList_for_UPSolutinoFile(xml_operators[r]->name, " " );                fprintf(pfp,")[1]\n");
	      printf("\n");
	    }
	}
      printf("\n");

    }


  fclose(pfp);

  /*--------end--of--test3-------------------*/

  return start[maxi];
}




/*
 *------End-of-User_pert-------------
 */



/*
 *------Routines for multiple interval time windows
 */

/* make list of sequentiel_plan operators
 */

OpConn_list *make_seqplan_list(){

  OpConn_list *head = NULL;
  OpConn_list *new = ( OpConn_list * ) calloc( 1, sizeof( OpConn_list ) );
  new->op1 = gop_conn[gplan_ops[0]];
  new->f = 0;    

  new->next = head;
  head = new;
  int i;
  OpConn_list *current = head;

  for(i = 1; i < gnum_plan_ops; i++){
     
    while(current->next!=NULL)
      current = current->next;

    new = ( OpConn_list * ) calloc( 1, sizeof( OpConn_list ) );
    new->op1 = gop_conn[gplan_ops[i]];
    new->f = i;
    current->next = new;
    new-> next = NULL;

  }
   
  
  return head;

}


/* calculate best makespann for all possibility
 */


void calculate_best_makespann(){ 

  float wert = 10000.00;
  float *makespann = &wert ; 
  OpConn_list *head = make_seqplan_list();

  /*
    opconn_array1 = ( OpConn *) calloc(gnum_plan_ops , sizeof( OpConn ) );
    opconn_array = ( OpConn * ) calloc(gnum_plan_ops , sizeof( OpConn ) );
  */
    
  makespann = calculate_best_makespann1(head,makespann);

  int p;
  for( p=0;p < gnum_plan_ops;p++)
    {
      gop_conn[gplan_ops[p]] = opconn_array[p];
    }    
    
  printf("\n %f\n", durativpert());

}


float *calculate_best_makespann1(OpConn_list *current,float *makespann ){

    
  int i,j;
  float b;
  float *m = &b;
  TimeWindows *time_windows_list2 = current->op1.action->time_windows_list;

  if(current==NULL){
    printf("sequentiel_plan is empty");
    exit(1);
  }   


  for(i = 0; i < current->op1.action->num_interval; i++){    

    current->op1.action->timeMin = time_windows_list2->mintime;
    current->op1.action->timeMax = time_windows_list2->maxtime;

    opconn_array1[current->f] = current->op1;

    time_windows_list2 = time_windows_list2->next;

    if(current->next!=NULL){
	  
            
      if(i==0)
	m = calculate_best_makespann1(current->next, makespann);
      if(m < makespann)
	makespann = m;

      else{
	       
	m = calculate_best_makespann1(current->next, makespann);
	if(m < makespann)
	  makespann = m;
      }
    }

    else{
	   
      b = durativ_pert_without_xml_dependence(opconn_array1);
      *m = b;
      if(makespann > m){
	makespann = m;
	for(j=0;j<MAX_PLAN_LENGTH;j++)
	  opconn_array[j] = opconn_array1[j];
      }
 
    }

  }
  
  
  return makespann;

}






/*
 *----------durativPert-function without xml dependence-------------------
 */


float durativ_pert_without_xml_dependence(OpConn *array)
{
  float start[gnum_plan_ops];
  float dur[gnum_plan_ops];

  int i;
  
  for( i=0;i < gnum_plan_ops;i++)
    {
      dur[i] = array[i].duration;

      start[i] =array[i].action->timeMin + dur[i];

      if (start[i] > array[i].action->timeMax) {
	/*
	  printf( "fail max time in PERT schedule %s\n",array[i].action->name );
	*/
	return 100000;
      }


      int j ;
      for(j=0;j < i;j++)
	{


	  if (depend(array[i],array[j]))
	    {

              
	      if (start[j] + dur[i] > start[i])
		{
		  if(endstart(array[j],array[i]))
		    {
                         
                          
		      start[i] = start[j] + dur[i]+ 0.01;
		    }
		  else
		    {

                         

		      if(!endover(array[j],array[i]) && !startover(array[i],array[j]))             {




			Bool ss = startstart(array[j],array[i]);
			Bool ee = endend(array[j],array[i]) ;

			if (ss && ! ee) {
                                       


			  if (start[i] < start[j] - dur[j] + dur[i] + 0.01) {
			    start[i] = start[j] - dur[j] + dur[i] + 0.01;
			  }
			}
			if (!ss && ee) {



			  if (start[i] < start[j] + 0.01) {
			    start[i] = start[j] + 0.01;
			  }
			}
			if (ss && ee) {


			  if (dur[j] < dur[i]) {
			    if (start[i] < start[j] - dur[j] + dur[i] + 0.01) {
			      start[i] = start[j] - dur[j] + dur[i] + 0.01;
			    }
			  }
			  else { /* dur[i] >= dur[j]*/
			    if (start[i] < start[j] + 0.01) {
			      start[i] = start[j] + 0.01;
			    }
			  }
			}
			if (!ss && !ee) {/* start overall (or overall end)*/
                    		  
			  if (start[i] < start[j] - dur[j] + dur[i]) {
			    start[i] = start[j] - dur[j] + dur[i];
                             
                                         
			  }
                                                            
            
			}

		      }
		      else {


			start[i] = start[j] + dur[i];
		      }

		    }


		}

                 

	      if (start[i] > array[i].action->timeMax) {
		/*
		  printf("fail maxtime in PERT schedule: %s\n",array[i].action->name);  */
		return 100000;
	      }
   
	    }
             
             
               
	}


    }


  int maxi=0;double max = -1;
  int k;
  for ( k=0;k < gnum_plan_ops;k++)
    {
      if (start[k]> max) {
	max = start[k];
	maxi=k;
      }
      
    }
 
  return start[maxi];
}

/*
 *------End-of-durativPert without xml dependence-------------
 */







/* schedulpert
   float schedulpert(OpConn *array, int index)
**/

float schedulpert()
{
  float start[gnum_plan_ops];
  float erliest_start[gnum_plan_ops];
  float dur[gnum_plan_ops];

  
  int i;
  for( i=0;i < gnum_plan_ops;i++)
    {
  
      dur[i] = gop_conn[gplan_ops[i]].duration;

      start[i] =gop_conn[gplan_ops[i]].action->timeMin + dur[i];

      if (start[i] > gop_conn[gplan_ops[i]].action->timeMax) {
	/*
	  printf( "fail max time in PERT schedule %s\n",gop_conn[gplan_ops[i]].action->name );
	*/
	return 100000;
      }


      int j ;
      for(j=0;j < i;j++)
	{      
      
	  if (depend(gop_conn[gplan_ops[i]],gop_conn[gplan_ops[j]]))
	    {

	      if (start[j] + dur[i] > start[i])
		{
		  if(endstart(gop_conn[gplan_ops[j]],gop_conn[gplan_ops[i]]))
		    {

		      start[i] = start[j] + dur[i]+ 0.01;
		    }
		  else
		    {
           
		      if(!endover(gop_conn[gplan_ops[j]],gop_conn[gplan_ops[i]]) && !startover(gop_conn[gplan_ops[i]],gop_conn[gplan_ops[j]]))             {


			Bool ss = startstart(gop_conn[gplan_ops[j]],gop_conn[gplan_ops[i]]);
			Bool ee = endend(gop_conn[gplan_ops[j]],gop_conn[gplan_ops[i]]) ;

			if (ss && ! ee) {
                                       

			  if (start[i] < start[j] - dur[j] + dur[i] + 0.01) {
			    start[i] = start[j] - dur[j] + dur[i] + 0.01;
			  }
			}
			if (!ss && ee) {

			  if (start[i] < start[j] + 0.01) {
			    start[i] = start[j] + 0.01;
			  }
			}
			if (ss && ee) {

			  if (dur[j] < dur[i]) {
			    if (start[i] < start[j] - dur[j] + dur[i] + 0.01) {
			      start[i] = start[j] - dur[j] + dur[i] + 0.01;
			    }
			  }
			  else { /* dur[i] >= dur[j]*/
			    if (start[i] < start[j] + 0.01) {
			      start[i] = start[j] + 0.01;
			    }
			  }
			}
			if (!ss && !ee) {/* start overall (or overall end)*/
                    		  
			  if (start[i] < start[j] - dur[j] + dur[i]) {
			    start[i] = start[j] - dur[j] + dur[i] ;
                             
                                         
			  }
                                                                       
			}

		      }
		      else {

			start[i] = start[j] + dur[i];
		      }

		    }


		}

                 

	      if (start[i] > gop_conn[gplan_ops[i]].action->timeMax) {
		/*
		  printf("fail maxtime in PERT schedule: %s\n",gop_conn[gplan_ops[i]].action->name);
		*/
		return 100000;
	      }
	    }
             
	}
    }
  
  /*-------------erliest_start-non-sortiert-show----------*/
  int r;
  for( r=0;r < gnum_plan_ops;r++)
    {
      erliest_start[r] = start[r]-dur[r];
     
    }
  /*--------end-of-----erliest_start-non-sortiert-show-----*/


  BubbleSort(erliest_start,gplan_ops,gnum_plan_ops );



  int maxi=0;double max = -1;
  int k;
  for ( k=0;k < gnum_plan_ops;k++)
    {
      if (start[k]> max) {
	max = start[k];
	maxi=k;
      }
      
    }
  return start[maxi];
}

/*
  {
  float start[index];
  float erliest_start[index];
  float dur[index];

  
  int i;
  for( i=0;i < index;i++)
  {
  
  dur[i] = array[i].duration;

  start[i] = array[i].action->timeMin + dur[i];

  if (start[i] > array[i].action->timeMax) {
  return 100000;
  }


 

  int j ;
  for(j=0;j < i;j++)
  {      
      
  if (depend(array[i],array[j]))
  {

  if (start[j] + dur[i] > start[i])
  {
  if(endstart(array[j],array[i]))
  {
                         

  start[i] = start[j] + dur[i]+ 0.01;
  }
  else
  {

           

  if(!endover(array[j],array[i]) && !startover(array[i],array[j]))             {


  Bool ss = startstart(array[j],array[i]);
  Bool ee = endend(array[j], array[i]) ;

  if (ss && ! ee) {

  if (start[i] < start[j] - dur[j] + dur[i] + 0.01) {
  start[i] = start[j] - dur[j] + dur[i] + 0.01;
  }
  }
  if (!ss && ee) {

  if (start[i] < start[j] + 0.01) {
  start[i] = start[j] + 0.01;
  }
  }
  if (ss && ee) {

  if (dur[j] < dur[i]) {
  if (start[i] < start[j] - dur[j] + dur[i] + 0.01) {
  start[i] = start[j] - dur[j] + dur[i] + 0.01;
  }
  }
  else {
  if (start[i] < start[j] + 0.01) {
  start[i] = start[j] + 0.01;
  }
  }
  }
  if (!ss && !ee) {
                    		  
  if (start[i] < start[j] - dur[j] + dur[i]) {
  start[i] = start[j] - dur[j] + dur[i] ;
                             
                                         
  }
                                                                       
  }

  }
  else {

  start[i] = start[j] + dur[i];
  }

  }


  }

                 

  if (start[i] > array[i].action->timeMax) {
  return 100000;
  }
  
  }
                            
  }

  }

  int r;
  for( r=0;r < index;r++)
  {
  erliest_start[r] = start[r]-dur[r];
     
  }



  BubbleSort2(erliest_start,array,index );



  int maxi=0;double max = -1;
  int k;
  for ( k=0;k < index;k++)
  {
  if (start[k]> max) {
  max = start[k];
  maxi=k;
  }
      
  }

 
  return start[maxi];
  }


  *------End-of-schedulpert-------------
  */


/* BubbeleSort function
 */

void BubbleSort2(float feld[],OpConn *feldop, int anzahl)
{
  /* Lokale Hilfsvariablen*/
  int i, k, fertig;
  float hilf;
  OpConn hilfop;

  fertig = 0;
  k = anzahl-1;

  while (k>0 && !fertig)
    {
      fertig = 1;
      for (i=0; i<k; i++)
	{
	  if ( feld[i] > feld[i+1] )
	    {
	      hilf = feld[i];
	      hilfop = feldop[i];
	      feld[i] = feld[i+1];
	      feldop[i] = feldop[i+1];
	      feld[i+1] = hilf;
	      feldop[i+1] = hilfop;
	      fertig = 0;
	    }
	}
      k--;
    }


  return;
}

/*------end--of-------BubbeleSort---Methode-------------*/

