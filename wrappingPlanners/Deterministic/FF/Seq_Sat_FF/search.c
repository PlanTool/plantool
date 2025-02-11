

/*********************************************************************
 * (C) Copyright 2002 Albert Ludwigs University Freiburg
 *     Institute of Computer Science
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 * 
 *********************************************************************/




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





#include <assert.h>


#include "ff.h"
#include "output.h"
#include "memory.h"
#include "expressions.h"
#include "search.h"
#include "heuristics.h"
#include "utility.h"

extern Bool artificial_gtt;



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










/********************************
 * EHC FUNCTION, CALLED BY MAIN *
 ********************************/





Bool lH;










Bool do_enforced_hill_climbing( void )

{

  State S, S_;
  int i, num_actions, num_actions_;

  float cost, cost_;

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

  /* check if we need artificial total-time metric
   *  for positive costs 
   */

  for ( i = 0; i < gnum_ef_conn; i++ ) {
    if(get_cost(i) == 0.0) {
      artificial_gtt = TRUE;
      gtt = ARTIFICIAL_GTT_VALUE;
      if ( gcmd_line.display_info ) {
	printf("\nUsing artificial total-time metric with multiplier %f\n\n", ARTIFICIAL_GTT_VALUE);
      }
      break;
    }
  }
    
  /* start enforced Hill-climbing
   */
  /* no helpful actions if hadd */
  lH = (TRUE && !gcmd_line.hadd);

  source_to_dest( &S, &ginitial_state );
  /* h = get_1P_and_H( &S ); */

  cost = get_cost_with_eval(&S, &num_actions, lH);
 
  if ( cost == INFINITY ) {
    return FALSE;
  }
  if (cost == 0) {
    return TRUE;
  }  
  if ( gcmd_line.display_info ) {
    printf("\n\nCueing down from goal distance: %6.2f into depth ", cost);
  }

   while ( cost != 0.0 ) {
    if ( !search_for_better_state( &S, cost, num_actions, &S_, &cost_, &num_actions_ ) ) {
      if ( gcmd_line.display_info ) {
	printf(" --- pruning stopped --- ");
      }

      lH = FALSE;

      cost = get_cost_with_eval(&S, &num_actions, lH);

      if ( !search_for_better_state( &S, cost, num_actions, &S_, &cost_, &num_actions_ ) ) {
	return FALSE;
      }
      /* no helpful actions if hadd */
      lH = !gcmd_line.hadd;
      cost = get_cost_with_eval(&S_, &num_actions, lH); /* to set up gH info for new start state */
    }

    source_to_dest( &S, &S_ );
    cost = cost_;
    num_actions = num_actions_;

    if ( gcmd_line.display_info ) {
      printf("\n                                %6.2f             ", cost);
      fflush( stdout );
    }
  }
  return TRUE;
}













/*************************************************
 * FUNCTIONS FOR BREADTH FIRST SEARCH IN H SPACE *
 *************************************************/













EhcNode *first_level_end;

Bool search_for_better_state( State *S, float cost_to_beat, int n_acts_to_beat, State *S_, float *new_cost, int *new_n_acts )

{

  static Bool fc = TRUE;
  static State S__;

  Bool found_possible = FALSE;
  float improvement_metric, max_improvement_metric = 0;

  int i, depth = 0, op, lnum_actions, min_num_actions;
  float lcost, min_cost = INFINITY;
  /* first_level_end points to last direct child of starting point.*/
  /* to mark the min_node for later use */
  EhcNode *tmp, *min_node = NULL, *current_node;

  Bool in_first_level, last_first_level_node;

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
  } 
  else {
    for ( i = 0; i < gnum_A; i++ ) {
      if ( result_to_dest( &S__, S, gA[i] ) ) {
	add_to_ehc_space( &S__, gA[i], NULL );
      }
    }
  }

  lehc_current_start = lehc_space_head->next;
  
  if(lehc_current_start == NULL) {
    return FALSE;
  }
  first_level_end = lehc_current_start;

  while( first_level_end->next && 
	 !first_level_end->next->father &&
	 (first_level_end->next != lehc_current_end) )
    {
      first_level_end = first_level_end->next;
    }

  in_first_level = TRUE;
  last_first_level_node = FALSE;

  min_node = NULL;
  min_cost = cost_to_beat;
  min_num_actions = n_acts_to_beat;

  while ( TRUE ) {  

    if ( lehc_current_start == first_level_end ) {
      last_first_level_node = TRUE;
    }

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

    lcost = expand_first_node( cost_to_beat, &lnum_actions, &op, &current_node);
  
    if ( LESS( lcost, cost_to_beat ) ) {
      
      if(! in_first_level ) {
	/* if we already checked all direct children, take any improvement */
	lehc_current_start = current_node;
	break;
      }

      else {
	/* if action_cost = previous_cost - new_cost holds, take improvement.
	 * as always, assume single effect ops.
	 */

	if (ROUGHLY_EQUAL((get_cost(gop_conn[op].E[0]) + (artificial_gtt ? 1 : 0) ), (cost_to_beat - lcost)) &&
	     (lnum_actions == (n_acts_to_beat - 1))) {
	  lehc_current_start = current_node;
	  break;
	}
	
	/* else, store, and take minimum at end. */
	else {
	  /*
	    printf("\ncost to beat: %f, lcost: %f, opcost: %f\n", cost_to_beat, lcost, get_cost( gop_conn[op].E[0] ));
	    printf("lnum_actions: %d, n_acts_to_beat: %d\n", lnum_actions, n_acts_to_beat);
	  */
	  
	  if(gcmd_line.gain || gcmd_line.ratio) {
	    
	    if(gcmd_line.gain) {
	      improvement_metric = ((cost_to_beat - lcost) - (get_cost(gop_conn[op].E[0])));
	    }
	    
	    else if (gcmd_line.ratio) {
	      improvement_metric = ((cost_to_beat - lcost) / (get_cost(gop_conn[op].E[0])));
	    }
	    
	    if(found_possible) {
	      if(improvement_metric > max_improvement_metric) {
		max_improvement_metric = improvement_metric;
		min_cost = lcost;
		min_num_actions = lnum_actions;
		min_node = current_node;
	      }
	    }
	    else {
	      found_possible = TRUE;
	      max_improvement_metric = improvement_metric;
	      min_cost = lcost;
	      min_num_actions = lnum_actions;
	      min_node = current_node;	      
	    }
	  }

	  else if (gcmd_line.logic) {
	    if(LESS(lnum_actions, min_num_actions) ||
	       ((lnum_actions == min_num_actions) && (LESS(lcost, min_cost)))) {
	      min_cost = lcost;
	      min_num_actions = lnum_actions;
	      min_node = current_node;
	    }
	  }

	  else if( LESS(lcost, min_cost) ) {
	    min_cost = lcost;
	    min_num_actions = lnum_actions;
	    min_node = current_node;
	  }
	}
      }
    }
    
    if(last_first_level_node) {
      
      /* didn't find any perfect actions, 
       * so if we found a minimum, take that.
       */

      /* no perfect acts */

      if(min_node) {
	lehc_current_start = min_node;
	break;
      }
      /* otherwise, just go on with regular EHC. */
      else {
	last_first_level_node = FALSE;
	in_first_level = FALSE;
      }
    }
  }

  extract_plan_fragment( S );

  source_to_dest( S_, &(lehc_current_start->S) );

  *new_cost = lehc_current_start->cost;
  *new_n_acts = lehc_current_start->num_actions;

  gnum_H = lehc_current_start->num_helpful_actions;
  memcpy(gH, lehc_current_start->helpful_actions,
	 lehc_current_start->num_helpful_actions * sizeof(int));

  reset_ehc_hash_entrys();
  free( tmp );

  return TRUE;

}



void add_to_ehc_space( State *S, int op, EhcNode *father )

{

  /* see if superior state (in terms of goal reachability)
   * is already a part of this search space
   */
  
  float cost = get_cost(gop_conn[op].E[0]);

  if(father) {
    cost += father->g;
  }


  if ( superior_ehc_state_hashed(S, cost)) {
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
    lehc_current_end->g = get_cost(gop_conn[op].E[0]);
  } else {
    lehc_current_end->depth = father->depth + 1;
    lehc_current_end->g = father->g + get_cost(gop_conn[op].E[0]);
  }

  hash_ehc_node( lehc_current_end );

  lehc_current_end = lehc_current_end->next;
  /*
  printf("\n\nAdded in add_to_ehc_space:\n");
  printf("\t"); print_state(S); printf("\n");
  printf("\t\treached through op "); print_op_name(op);
  */
}



float expand_first_node( float prev_cost, int *n_acts, int *op, EhcNode **current_node)

{

  static Bool fc = TRUE;
  static State S_;

  int i;
  float new_cost;

  if ( fc ) {
    make_state( &S_, gnum_ft_conn, gnum_fl_conn );
    fc = FALSE;
  }

  *op = lehc_current_start->op;
  *current_node = lehc_current_start;
  /*
  print_state(&(lehc_current_start->S));
  printf("--------------------------------------------------------------\n");
  */
  new_cost = get_cost_with_eval( &(lehc_current_start->S), n_acts, lH);

  lehc_current_start->cost = new_cost;
  lehc_current_start->num_actions = *n_acts;
  
  if(lH) {
    assert(EHC_HELPFUL_ACTION_STORAGE_CAPACITY >= gnum_H);
    memcpy(lehc_current_start->helpful_actions, gH, (gnum_H * sizeof(int)));
    lehc_current_start->num_helpful_actions = gnum_H;
  }
  
  if ( new_cost == INFINITY ) {
    lehc_current_start = lehc_current_start->next;
    return new_cost;
  }

  if ( new_cost < prev_cost ) {
    lehc_current_start = lehc_current_start->next;
    return new_cost;
  }
  
  if ( lH ) {
    for ( i = 0; i < gnum_H; i++ ) {
      if ( result_to_dest( &S_, &(lehc_current_start->S), gH[i] ) ) {
	add_to_ehc_space( &S_, gH[i], lehc_current_start );
      }
    }
  } 
  else {
    for ( i = 0; i < gnum_A; i++ ) {
      if ( result_to_dest( &S_, &(lehc_current_start->S), gA[i] ) ) {
	add_to_ehc_space( &S_, gA[i], lehc_current_start );
      }
    }
  }
    
  lehc_current_start = lehc_current_start->next;

  return new_cost;

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



Bool superior_ehc_state_hashed( State *S, float gS )

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
    if ( superior_state( &(h->ehc_node->S), h->ehc_node->g, S, gS) ) {
      return TRUE;
    }
    h = h->next;
  }

  return FALSE;

}


/* return true if S1 is a better state than S2.
 * need to modify this function to take cost 
 * into account. 
 */

Bool superior_state( State *S1, float gS1, State *S2, float gS2 ) 

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
  
  /* added by Emil: if state 2 was reached in a cheaper
   * way, don't throw it away.
   */

  if(gS2 < gS1) {
    return FALSE;
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
    gplan_ops[gnum_plan_ops++] = ops[j];
  }

}




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

  BfsNode *first;
  State S;
  int i;
  /*  int min = INFINITY;*/
  float min = INFINITY;
  Bool start = TRUE;

  lH = FALSE; /* helpful actions always disabled in BFS */

  make_state( &S, gnum_ft_conn, gnum_fl_conn );

  lbfs_space_head = new_BfsNode();
  lbfs_space_had = NULL;

  for ( i = 0; i < BFS_HASH_SIZE; i++ ) {
    lbfs_hash_entry[i] = NULL;
  }

  add_to_bfs_space( &ginitial_state, -1, NULL );

  while ( TRUE ) {
    if ( (first = lbfs_space_head->next) == NULL ) {
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
	  printf("\n\nadvancing to distance: %4f", min);
	  fflush(stdout);
	}
	start = FALSE;
      } else {
	if ( gcmd_line.display_info ) {
	  printf("\n                       %4f", min);
	  fflush(stdout);
	}
      }
    }

    if ( first->h == 0 ) {
      break;
    }

    for ( i = 0; i < first->num_H; i++ ) {
      if ( result_to_dest( &S, &(first->S), first->H[i] ) ) {
	/* we must include a check here whether the numerical part of the action
	 * is entirely fulfilled; only those actions are applied.
	 */
	add_to_bfs_space( &S, first->H[i], first );
      }
    }

    first->next = lbfs_space_had;
    lbfs_space_had = first;
  }

  extract_plan( first );
  return TRUE;

}



void add_to_bfs_space( State *S, int op, BfsNode *father )

{

  BfsNode *new, *i;
  int j, h, intg, int_fn = 0;
  float cost = 0, floatg = 0, float_fn = 0;

  if ( gcmd_line.optimize && goptimization_established ) {
    if ( bfs_state_hashed( S ) ) {
      return;
    }
  } else {
    if ( superior_bfs_state_hashed( S, state_cost(S, father))) {
      return;
    }
  }
  
  cost = get_cost_with_eval(S, &h, lH);

  if ( cost == INFINITY ) {
    return;
  }

  if ( father ) {
    intg = father->g + 1;
  } else {
    intg = 0;
  }

  if ( gcmd_line.optimize && goptimization_established ) {
    floatg = state_cost( S, father );
    float_fn = (((float) gcmd_line.g_weight) * floatg) + (((float) gcmd_line.h_weight) * cost);
    for ( i = lbfs_space_head; i->next; i = i->next ) {
      if ( i->next->float_fn >= float_fn ) break;
    }
  } else {
    /*    int_fn = (gcmd_line.g_weight * intg) + (gcmd_line.h_weight * h);*/
    int_fn = (gcmd_line.g_weight * intg) + (gcmd_line.h_weight * cost);
    for ( i = lbfs_space_head; i->next; i = i->next ) {
      if ( i->next->int_fn >= int_fn ) break;
    }
  }

  new = new_BfsNode();
  copy_source_to_dest( &(new->S), S );
  new->op = op;
  /*  new->h = h;*/
  new->h = cost;
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
    }
    else {
      cost += (glnf_metric.pC[i] * 
	       (S->f_V[glnf_metric.pF[i]] - ginitial_state.f_V[glnf_metric.pF[i]]));
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



Bool superior_bfs_state_hashed( State *S, float cost )

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
    if (
	superior_state( &(h->bfs_node->S), 
			state_cost(&(h->bfs_node->S), h->bfs_node->father),
			S,
			cost)
	 )
    {
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


float get_cost_with_eval(State *S, int *num_acts, Bool collect_helpful ) {

  float cost = eval(S, num_acts, collect_helpful);
  
  /* artificial gtt is true iff we have 0 cost actions (even after
   * total-time is added)
   */
  if(artificial_gtt && (*num_acts != INFINITY)) {
    cost += gtt * (*num_acts);
  }

  return cost;
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

}


