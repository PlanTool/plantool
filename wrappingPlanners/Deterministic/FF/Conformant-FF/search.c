


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
 *              ADL version, Goal Agenda driven
 *                           Enforced Hill-climbing
 *
 *                           and, alternatively, standard Best First Search
 *
 *
 * Author: Joerg Hoffmann 2002
 *
 *********************************************************************/ 









#include "ff.h"

#include "output.h"
#include "memory.h"

#include "relax.h"
#include "state_transitions.h"
#include "repeated_states.h"
#include "search.h"














/*****************
 * LOCAL GLOBALS *
 *****************/









/* in agenda driven algorithm, the current set of goals is this
 */
State lscurrent_goals;



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
 * encountered by current serial plan - DISACTIVATED IN CONFORMANT SETTING...
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











Bool do_enforced_hill_climbing( State *start, State *end )

{

  static Bool first_call = TRUE;
  static State S, S_;
  int i, h, h_;

  if ( first_call ) {
    /* on first call, initialize plan hash table, search space, search hash table
     */
    for ( i = 0; i < PLAN_HASH_SIZE; i++ ) {
      lplan_hash_entry[i] = NULL;
    }
    
    lehc_space_head = new_EhcNode();
    lehc_space_end = lehc_space_head;
    
    make_state( &S, gnum_ft_conn ); 
    make_state( &S_, gnum_ft_conn );

    make_state( &lscurrent_goals, gnum_ft_conn );

    first_call = FALSE;
  }
  
  /* start enforced Hill-climbing
   */

  source_to_dest( &lscurrent_goals, end );  

  source_to_dest( &S, start );
  h = get_1P_and_H( &S, &lscurrent_goals, NULL, NULL, -1 );

  if ( h == INFINITY ) {
    return FALSE;
  }
  if ( h == 0 ) {
    return TRUE;
  }  
  printf("\n\nCueing down from goal distance: %4d into depth ", h);

  while ( h != 0 ) {
    if ( !search_for_better_state( &S, h, &S_, &h_ ) ) {
      for ( i = 0; i < gnum_clauses; i++ ) {
	gclause_length[i] = 0;
      }
      gnum_fixed_clauses = 0;
      gnum_clauses = 0;
      for ( i = 1; i <= gnum_fixed_c; i++ ) {
	if ( gpos_c_in_clause_start[i] == NULL ||
	     gneg_c_in_clause_start[i] == NULL ) {
	  printf("\nin search method reset, var %d has no dummy nodes in mem list yet??\n\n",
		 i);
	  exit( 1 );
	}
	gcodes[gct[i]][gcf[i]] = -1;
	gpos_c_in_clause_end[i] = gpos_c_in_clause_start[i]->next;
	gneg_c_in_clause_end[i] = gneg_c_in_clause_start[i]->next;
	gpos_c_in_clause_fixed[i] = gpos_c_in_clause_start[i]->next;
	gneg_c_in_clause_fixed[i] = gneg_c_in_clause_start[i]->next;
      }
      gnum_fixed_c = 0;		   
      extend_fixed_clauses_base( 0, 0 );
      extend_fixed_clauses_base_encoding( 0 );
      return FALSE;
    }
    source_to_dest( &S, &S_ );
    h = h_;
    printf("\n                                %4d            ", h);
  }

  return TRUE;

}











/*************************************************
 * FUNCTIONS FOR BREADTH FIRST SEARCH IN H SPACE *
 *************************************************/













Bool search_for_better_state( State *S, int h, State *S_, int *h_ )

{

  static Bool first_call = TRUE;
  static State S__;

  int i, h__, depth = 0;
  EhcNode *tmp;

  if ( first_call ) {
    make_state( &S__, gnum_ft_conn );
    first_call = FALSE;
  }

  /* don't hash states, but search nodes.
   * this way, don't need to keep states twice in memory
   */
  tmp = new_EhcNode();
  copy_source_to_dest( &(tmp->S), S);
  hash_ehc_node( tmp );

  lehc_current_end = lehc_space_head->next;
  for ( i = 0; i < gnum_H; i++ ) {
      /* see result-to-dest params explanation at fn header
       */
    if ( !result_to_dest( &S__, tmp, NULL, gH[i] ) ) continue;
    add_to_ehc_space( &S__, gH[i], tmp );
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
      printf("[%d]", depth);
      fflush( stdout );
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

  /* see if state is already a part of this search space
   */
  if ( gcmd_line.dominating && ehc_state_hashed( S, father, op ) ) {
    return;
  }

  if ( !lehc_current_end ) {
    lehc_current_end = new_EhcNode();
    lehc_space_end->next = lehc_current_end;
    lehc_space_end = lehc_current_end;
  }

  copy_source_to_dest( &(lehc_current_end->S), S );
  lehc_current_end->op = op;
  lehc_current_end->father = father;
  if ( !father ) {
    lehc_current_end->depth = 1;
  } else {
    lehc_current_end->depth = father->depth + 1;
  }

  if ( gcmd_line.dominating ) {
    hash_ehc_node( lehc_current_end );
  }

  lehc_current_end = lehc_current_end->next;

}



int expand_first_node( int h )

{

  static Bool fc = TRUE;
  static State S_;

  int h_, i;

  if ( fc ) {
    make_state( &S_, gnum_ft_conn );
    fc = FALSE;
  }

  h_ = get_1P_and_H( &(lehc_current_start->S), &lscurrent_goals,
		     lehc_current_start->father, NULL,
		     lehc_current_start->op );
    
  if ( h_ == INFINITY ) {
    lehc_current_start = lehc_current_start->next;
    return h_;
  }

  if ( h_ < h ) {
    return h_;
  }

  for ( i = 0; i < gnum_H; i++ ) {
    if ( !result_to_dest( &S_, lehc_current_start, NULL, gH[i] ) ) continue;
    add_to_ehc_space( &S_, gH[i], lehc_current_start );
  }
    
  lehc_current_start = lehc_current_start->next;

  return h_;

}

















/***************************************************
 * FUNCTIONS FOR UPDATING THE CURRENT SERIAL PLAN, *
 * BASED ON SEARCH SPACE INFORMATION .             *
 ***************************************************/












void extract_plan_fragment( State *S )

{

  EhcNode *i;
  int ops[MAX_PLAN_LENGTH], num_ops;
  State_pointer states[MAX_PLAN_LENGTH];
  int j, mem, prev;

  num_ops = 0;
  for ( i = lehc_current_start; i->father; i = i->father ) {
    if ( num_ops == MAX_PLAN_LENGTH ) {
      printf("\nincrease MAX_PLAN_LENGTH! currently %d\n\n",
	     MAX_PLAN_LENGTH);
      exit( 1 );
    }
    states[num_ops] = &(i->S);
    ops[num_ops++] = i->op;
  }

  mem = gnum_plan_ops;
  for ( j = num_ops - 1; j > -1; j-- ) {
    if ( gnum_plan_ops == MAX_PLAN_LENGTH ) {
      printf("\nincrease MAX_PLAN_LENGTH! currently %d\n\n",
	     MAX_PLAN_LENGTH);
      exit( 1 );
    }
    source_to_dest( &(gplan_states[gnum_plan_ops+1]), states[j] );
    gplan_ops[gnum_plan_ops++] = ops[j];
    if ( gcmd_line.A && gcmd_line.debug ) {
      printf("\n------------EHC SELECTING: ");
      print_op_name( ops[j] );
    }
  }

  if ( gcmd_line.A && gcmd_line.debug ) {
    printf("\n\nnew op path is:");
    for ( j = 0; j < gnum_plan_ops; j++ ) {
      printf("\n");print_op_name( gplan_ops[j] );
    }
  }

  prev = gnum_fixed_clauses;
  extend_fixed_clauses_base( mem, gnum_plan_ops );
  extend_fixed_clauses_base_encoding( prev );

}

















/************************************
 * BEST FIRST SEARCH IMPLEMENTATION *
 ************************************/














Bool do_best_first_search( void )

{

  static Bool fc = TRUE;
  static State S;

  BfsNode *first;
  int i, min = INFINITY;
  Bool start = TRUE;

  if ( fc ) {
    make_state( &S, gnum_ft_conn );
    fc = FALSE;
  }

  lbfs_space_head = new_BfsNode();
  lbfs_space_had = NULL;

  add_to_bfs_space( &ginitial_state, -1, NULL );

  while ( TRUE ) {
    if ( (first = lbfs_space_head->next) == NULL ) {
      printf("\n\nbest first search space empty! problem proven unsolvable.\n\n");
      return FALSE;
    }

    lbfs_space_head->next = first->next;
    if ( first->next ) {
      first->next->prev = lbfs_space_head;
    }

    if ( LESS( first->h, min ) ) {
      min = first->h;
      if ( start ) {
	printf("\nadvancing to distance : %4d", min);
	start = FALSE;
	fflush(stdout);
      } else {
	printf("\n                        %4d", min);
	fflush(stdout);
      }
    }

    if ( first->h == 0 ) {
      break;
    }

    get_A( &(first->S) );
    for ( i = 0; i < gnum_A; i++ ) {
      if ( !result_to_dest( &S, NULL, first, gA[i] ) ) continue;
      add_to_bfs_space( &S, gA[i], first );
    }

    first->next = lbfs_space_had;
    lbfs_space_had = first;
  }

  extract_plan( first );
  return TRUE;

}



void add_to_bfs_space( State *S, int op, BfsNode *father )

{

  static int max_d = 0;

  BfsNode *tmp, *i;
  int h, num;

  /* see if state is already a part of this search space
   */
  if ( op >= 0 ) {/* intial state has op -1 */
    if ( gcmd_line.dominating && bfs_state_hashed( S, father, op ) ) {
      return;
    }
  }

  if ( gcmd_line.A && gcmd_line.debug ) {
    num = 0;
    for ( i = father; i; i = i->father ) {
      num++;
    }
    if ( num > max_d ) {
      max_d = num;
      printf("\ndepth: %d", num);
    }
  }

  h = get_1P( S, &ggoal_state, NULL, father, op );

  if ( h == INFINITY ) {
     return;
  }

  for ( i = lbfs_space_head; i->next; i = i->next ) {
    if ( gcmd_line.breadth_bfs ) {
      if ( i->next->h > h ) break;/* stop only when next one is strictly worse */
    } else {
      if ( i->next->h >= h ) break;/* stop already when next one is equally good */
    }
  }

  tmp = new_BfsNode();
  copy_source_to_dest( &(tmp->S), S );
  tmp->op = op;
  tmp->h = h;
  tmp->father = father;
  
  tmp->next = i->next;
  tmp->prev = i;
  i->next = tmp;
  if ( tmp->next ) {
    tmp->next->prev = tmp;
  }
  
  if ( gcmd_line.dominating ) {
    hash_bfs_node( tmp );
  }

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















/***********************************
 * SIMPLE STATE HANDLING FUNCTIONS *
 ***********************************/
















void source_to_dest( State *dest, State *source )

{

  int i;

  for ( i = 0; i < source->num_F; i++ ) {
    dest->F[i] = source->F[i];
  }
  dest->num_F = source->num_F;

  for ( i = 0; i < source->num_U; i++ ) {
    dest->U[i] = source->U[i];
  }
  dest->num_U = source->num_U;

  for ( i = 0; i < source->num_unknown_E; i++ ) {
    dest->unknown_E[i] = source->unknown_E[i];
  }
  dest->num_unknown_E = source->num_unknown_E;

}



void copy_source_to_dest( State *dest, State *source )

{

  int i, m;

  if ( dest->max_F < source->num_F ) {
    if ( dest->F ) {
      free( dest->F );
    }
    if ( source->num_F + 50 > gnum_ft_conn ) {
      m = gnum_ft_conn;
    } else {
      m = source->num_F + 50;
    }
    dest->F = ( int * ) calloc( m, sizeof( int ) );
    dest->max_F = m;
  }
  for ( i = 0; i < source->num_F; i++ ) {
    dest->F[i] = source->F[i];
  }
  dest->num_F = source->num_F;

  /* rest is static, simply copy over.
   */
  for ( i = 0; i < source->num_U; i++ ) {
    dest->U[i] = source->U[i];
  }
  dest->num_U = source->num_U;

  for ( i = 0; i < source->num_unknown_E; i++ ) {
    dest->unknown_E[i] = source->unknown_E[i];
  }
  dest->num_unknown_E = source->num_unknown_E;

}



void print_state( State S )

{

  int i;
  
  printf("\nF:");
  for ( i = 0; i < S.num_F; i++ ) {
    printf("\n");
    print_ft_name( S.F[i] );
  }
  printf("\nU:");
  for ( i = 0; i < S.num_U; i++ ) {
    printf("\n");
    print_ft_name( S.U[i] );
  }

}





















/****************************************
 * for debugging: manual search control *
 ****************************************/





















void manual_control( void )

{

  static Bool fc = TRUE;
  static State S;

  int i, j, h, choice;
  BfsNode *tmp, *curr;

  if ( fc ) {
    make_state( &S, gnum_ft_conn );
    fc = FALSE;
  }

  tmp = new_BfsNode();
  copy_source_to_dest( &(tmp->S), &ginitial_state );
  tmp->op = -1;
  curr = tmp;
  if ( gcmd_line.dominating ) {
    hash_bfs_node( curr );
  }

  while ( TRUE ) {
    if ( !curr ) break;
    h = get_1P_and_H( &(curr->S), &ggoal_state, NULL, curr->father, curr->op );
    get_A( &(curr->S) );
    
    while ( TRUE ) {
      printf("\n\n\n-------------state h = %d", h);
      if ( h > 0 ) printf(" (resp. %d actions)", h - 1);
      printf(", info level %d, %d applicable actions", gcmd_line.debug, gnum_A);
      if ( gcmd_line.debug >= 1 ) {
	print_state( curr->S );
      }
      if ( 1 ) {
	printf("\nH:"); 
	for ( i = 0; i < gnum_H; i++ ) {
	  printf(" ");
	  print_op_name( gH[i] );
	}
      }
      printf("\n"); 
      for ( i = 0; i < gnum_A; i++ ) {
	printf("\n%3d ", i); 
	for ( j = 0; j < gnum_H; j++ ) {
	  if ( gA[i] == gH[j] ) break;
	}
	if ( j < gnum_H ) {
	  printf("H: ");
	} else {
	  printf(" : ");
	}
	print_op_name( gA[i] );
      }
      printf("\n\n -1: retract last choice");
      printf("\n -2: set info level");
      printf("\n\nchoice: "); scanf("%d", &choice);
      if ( choice >= -2 && choice < gnum_A ) break;
    }
    
    if ( choice >= 0 ) {
      if ( !result_to_dest( &S, NULL, curr, gA[choice] ) ) {
	printf("\naction not applicable!");
	continue;
      }
      
      if ( gcmd_line.dominating && bfs_state_hashed( &S, curr, gA[choice] ) ) {
	printf("\nthis state is dominated!\n\n");
      }
      
      tmp = new_BfsNode();
      copy_source_to_dest( &(tmp->S), &S );
      tmp->father = curr;
      tmp->op = gA[choice];
      curr = tmp;
      
      if ( gcmd_line.dominating ) {
	hash_bfs_node( curr );
      }
      
      continue;
    }
    if ( choice == -1 ) {
      curr = curr->father;
      continue;
    }
    printf("\nlevel : "); scanf("%d", &gcmd_line.debug);
  }

}

