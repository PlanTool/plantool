


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
 * File: relax.c
 * Description: this file handles the relaxed planning problem, i.e.,
 *              the code is responsible for the heuristic evaluation
 *              of states during search.
 *
 *              --- THE HEART PEACE OF THE FF PLANNER ! ---
 *
 *              - fast (time critical) computation of the relaxed fixpoint
 *              - extraction of as short as possible plans, without search
 *
 *
 *
 *  ----- UPDATED VERSION TO HANDLE CONFORMANT SEMANTICS -----
 *
 *
 *
 * Author: Joerg Hoffmann 2002
 *
 *********************************************************************/ 









#include "ff.h"

#include "output.h"
#include "memory.h"

#include "relax.h"
#include "search.h"
#include "state_transitions.h"














/* local globals
 */








/* in agenda driven algorithm, the current set of goals is this
 */
State lcurrent_goals;



/* fixpoint
 */
int *lF;
int lnum_F;
int *lE;
int lnum_E;

int *lch_E;
int lnum_ch_E;

int *l0P_E;
int lnum_0P_E;

/* for get applicable actions
 */
int *lch_O;
int lnum_ch_O;
int *l0P_O;
int lnum_0P_O;

/* conformant part: the ``temporal implication graph''
 */
UftNode **lU;
int *lnum_U;
/* the U graph is built for the whole path to the state;
 * this int here is the number of the U layer that corresponds
 * to the state itself, ie to the first rplangraph layer.
 */
int lpath_U_length;


/* 1P extraction
 */
int **lgoals_at;
int *lnum_goals_at;

int *lch_F;
int lnum_ch_F;

int *lused_O;
int lnum_used_O;

int lh;


int lMAX_PATHNODES;






/* CNF time of vars in S -- for ini_ors == 5
 */
int lStime;






/* clauses for reasoning about initial formula implications
 */

TimedLiteral **lr_clauses;
int *lr_clause_length;
int lr_num_clauses;

/* we use our own codes in the CNF, to avoid confusion with this other shit I programmed
 * 2 years ago
 */
int *lr_codes;/* maps a fact at time 0 to its r-internal code */
int lr_num_c;/* nr. of codes */
int *lr_cf;/* maps a r-internal code to the resp. fact */

 

/* for naive DP implementation
 */
int *lr_assigned;


/* stores the current DP decisions including unit propagations.
 */
int *lr_decision_stack;
int lr_num_decision_stack;


/* for each possible ft code, a pointer to connected dynamic list
 * of member elements, ie the clauses in which it participates,
 * positive and negative.
 */
MemberList_pointer *lr_pos_c_in_clause_start;
MemberList_pointer *lr_pos_c_in_clause_end;
MemberList_pointer *lr_neg_c_in_clause_start;
MemberList_pointer *lr_neg_c_in_clause_end;













/*************************************
 * helper, for -1 == INFINITY method *
 *************************************/












Bool LESS( int a, int b )

{

  if ( a == INFINITY ) {
    return FALSE;
  }

  if ( b == INFINITY ) {
    return TRUE;
  }

  return ( a < b ? TRUE : FALSE );

}












/***********************************
 * FUNCTIONS ACCESSED FROM OUTSIDE *
 ***********************************/






Bool contains_goal( State *S, State *current_goals )

{

  int i, j;

  for ( i = 0; i < current_goals->num_F; i++ ) {
    for ( j = 0; j < S->num_F; j++ ) {
      if ( S->F[j] == current_goals->F[i] ) break;
    }
    if ( j == S->num_F ) {
      return FALSE;
    }
  }

  return TRUE;

}



void initialize_relax( void )

{

  int i, j, maxcl, m, ff, min, min_i;
  Bool *had;

  make_state( &lcurrent_goals, gnum_ft_conn );
  /* need to do this here as we do not call real gH setup fn
   */
  gH = ( int * ) calloc( gnum_op_conn, sizeof( int ) );
  gnum_H = 0;

  gA = ( int * ) calloc( gnum_op_conn, sizeof( int ) );
  gnum_A = 0;

  /* the temporal implication graph between
   * unknown facts.
   */
  lU = ( UftNode ** ) calloc( RELAXED_STEPS + MAX_PLAN_LENGTH, sizeof( UftNode * ) );
  lnum_U = ( int * ) calloc( RELAXED_STEPS + MAX_PLAN_LENGTH, sizeof( int ) );
  for ( i = 0; i < RELAXED_STEPS + MAX_PLAN_LENGTH; i++ ) {
    lU[i] = ( UftNode * ) calloc( gmax_U, sizeof( UftNode ) );
    lnum_U[i] = 0;
    for ( j = 0; j < gmax_U; j++ ) {
      lU[i][j].num_in = 0;
    }
  }
  lMAX_PATHNODES = (RELAXED_STEPS + MAX_PLAN_LENGTH) * gmax_U;

  /* rp stuff
   */
  lF = ( int * ) calloc( gnum_ft_conn, sizeof( int ) );
  lE = ( int * ) calloc( gnum_ef_conn, sizeof( int ) );
  lch_E = ( int * ) calloc( gnum_ef_conn, sizeof( int ) );
  l0P_E = ( int * ) calloc( gnum_ef_conn, sizeof( int ) );

  /* get A stuff
   */
  l0P_O = ( int * ) calloc( gnum_op_conn, sizeof( int ) );
  lch_O = ( int * ) calloc( gnum_ef_conn, sizeof( int ) );

  /* various initializations
   */
  lnum_0P_E = 0;
  for ( i = 0; i < gnum_ef_conn; i++ ) {      
    gef_conn[i].level = INFINITY;    
    gef_conn[i].in_E = FALSE;
    gef_conn[i].num_active_PCs = 0;
    gef_conn[i].ch = FALSE;
    
    if ( gef_conn[i].num_PC == 0 ) {
      l0P_E[lnum_0P_E++] = i;
    }
  }
  lnum_0P_O = 0;
  for ( i = 0; i < gnum_op_conn; i++ ) {      
    gop_conn[i].is_in_A = FALSE;
    gop_conn[i].is_in_H = FALSE;
    gop_conn[i].ch = FALSE;
    
    if ( gop_conn[i].num_P == 0 ) {
      l0P_O[lnum_0P_O++] = i;
    }
  }
  for ( i = 0; i < gnum_ft_conn; i++ ) {
    gft_conn[i].level = INFINITY;
    gft_conn[i].in_F = FALSE;
  }

  

  /* Mar04:
   * CREATE SAT FORMULA TO BE USED FOR INITIAL IMPLIES DISJUNCTION CHECKING!!
   * (if needed)
   *
   * (for h == 0 we do this just to be able to use the same pieces of code...
   *  can't be critical for performance...)
   */

  if ( gcmd_line.heuristic == 0 ||
       gcmd_line.heuristic == 1 ) {
    /* do it Baby
     */

    /* get the memory
     */
    maxcl = gnum_initial_or + gnum_ft_conn;
    lr_clauses = ( TimedLiteral ** ) calloc( maxcl, sizeof( TimedLiteral * ) );
    lr_clause_length = ( int * ) calloc( maxcl, sizeof( int ) );
    for ( i = 0; i < maxcl; i++ ) {
      lr_clauses[i] = ( TimedLiteral * ) calloc( gmax_literals, sizeof( TimedLiteral ) );
      lr_clause_length[i] = 0;
    }
    lr_num_clauses = 0;
    lr_codes = ( int * ) calloc( gnum_ft_conn, sizeof( int ) );
    lr_cf = ( int * ) calloc( gnum_ft_conn + 1, sizeof( int ) );
    lr_num_c = 0;
    for ( i = 0; i < gnum_ft_conn; i++ ) {
      lr_codes[i] = -1;
    }
    lr_assigned = ( int * ) calloc( gnum_ft_conn + 1, sizeof( int ) );
    for ( i = 0; i < gnum_ft_conn + 1; i++ ) {
      lr_assigned[i] = -1;
    }

    lr_decision_stack = ( int * ) calloc( gnum_ft_conn, sizeof( int ) );

    lr_pos_c_in_clause_start = ( MemberList_pointer * ) 
      calloc( gnum_ft_conn + 1, sizeof( MemberList_pointer ) );
    lr_pos_c_in_clause_end = ( MemberList_pointer * ) 
      calloc( gnum_ft_conn + 1, sizeof( MemberList_pointer ) );
    lr_neg_c_in_clause_start = ( MemberList_pointer * ) 
      calloc( gnum_ft_conn + 1, sizeof( MemberList_pointer ) );
    lr_neg_c_in_clause_end = ( MemberList_pointer * ) 
      calloc( gnum_ft_conn + 1, sizeof( MemberList_pointer ) );
    for ( i = 0; i <= gnum_ft_conn; i++ ) {
      lr_pos_c_in_clause_start[i] = NULL;
      lr_neg_c_in_clause_start[i] = NULL;
      lr_pos_c_in_clause_end[i] = NULL;
      lr_neg_c_in_clause_end[i] = NULL;
    }


    /* now insert the explicit literal / time encoding
     *
     * NOTE: sort the clauses by increasing length; this way when, later, we 
     *       proceed over the clauses from front to back to see if one of them is
     *       contained in a leaf disjunction, we'll try the shortest clauses
     *       first (see also below in rplan extraction)
     */
    had = ( Bool * ) calloc( gnum_initial_or, sizeof( Bool ) );
    for ( i = 0; i < gnum_initial_or; i++ ) {
      had[i] = FALSE;
    }
    if ( gcmd_line.R && gcmd_line.debug ) {
      for ( i = 0; i < gnum_initial_or; i++ ) {
	printf("\nr initial OR ");
	for ( j = 0; j < ginitial_or_length[i]; j++ ) {
	  print_ft_name( ginitial_or[i][j] ); printf(" ");
	}
	fflush(stdout);
      }
    }
    for ( i = 0; i < gnum_initial_or; i++ ) {
      min = -1; min_i = -1;
      for ( j = 0; j < gnum_initial_or; j++ ) {
	if ( had[j] ) continue;
	if ( min == -1 || ginitial_or_length[j] < min ) {
	  min = ginitial_or_length[j];
	  min_i = j;
	}
      }
      if ( min_i < 0 || min_i >= gnum_initial_or ) {
	printf("\nsomething wrong with the min_i!\n\n");
	exit( 1 );
      }
      had[min_i] = TRUE;
      if ( lr_num_clauses == maxcl ) {
	printf("\n\ntoo many r clauses? %d\n\n", lr_num_clauses);
	exit( 1 );
      }
      for ( j = 0; j < ginitial_or_length[min_i]; j++ ) {
	ff = ginitial_or[min_i][j];
	if ( gft_conn[ff].CNF ) {
	  m = 1;
	} else {
	  m = -1;
	  ff = gft_conn[ff].negation;
	}
	if ( lr_codes[ff] == -1 ) {
	  lr_num_c++;/* must be non-zero */
	  lr_codes[ff] = lr_num_c;
	  if ( lr_num_c == gnum_ft_conn ) {
	    printf("\n\ntoo many r codes? %d\n\n", lr_num_c);
	    exit( 1 );
	  }
	  lr_cf[lr_num_c] = ff;
	}
	lr_clauses[lr_num_clauses][lr_clause_length[lr_num_clauses]].literal = m * lr_codes[ff];
	lr_clauses[lr_num_clauses][lr_clause_length[lr_num_clauses]].time = -1;
	lr_clause_length[lr_num_clauses]++;
      }
      lr_num_clauses++;
      if ( gcmd_line.R && gcmd_line.debug ) {
	printf("\nr inserted OR clause"); print_r_clause( lr_num_clauses-1 ); fflush(stdout);
      }
    } /* for all initial ors */

    if ( gcmd_line.R && gcmd_line.debug >= 4 ) {
      printf("\n\n--------------------------------r initial CNF encoding");
      printf("\n-----codes %d:", lr_num_c);
      for ( i = 1; i <= lr_num_c; i++ ) {
	printf("\n"); print_ft_name( lr_cf[i] ); 
	printf(" ---> %d", lr_codes[lr_cf[i]]);
      }
      fflush(stdout);
    }

    /* now create the membership lists.
     */
    r_do_membership_lists();
  }/* if heuristic == 1 */


}



void r_do_membership_lists( void )

{

  int i, j, v;
  MemberList *i_ml;

  /* first, "empty" the lists.
   */
  for ( i = 1; i <= lr_num_c; i++ ) {
    /* for new vars, create the dummy elements. else, just set the
     * "end marker" directly behind start.
     */
    if ( lr_pos_c_in_clause_start[i] == NULL ) {
      lr_pos_c_in_clause_start[i] = new_MemberList();
      lr_pos_c_in_clause_start[i]->clause = -1;
      lr_pos_c_in_clause_end[i] = new_MemberList();
      lr_pos_c_in_clause_end[i]->clause = -1;
      lr_pos_c_in_clause_start[i]->next = lr_pos_c_in_clause_end[i];
    } else {
      printf("\nvar in r CNF mem lists is not new??\n\n");
      exit( 1 );
    }
    if ( lr_neg_c_in_clause_start[i] == NULL ) {
      lr_neg_c_in_clause_start[i] = new_MemberList();
      lr_neg_c_in_clause_start[i]->clause = -1;
      lr_neg_c_in_clause_end[i] = new_MemberList();
      lr_neg_c_in_clause_end[i]->clause = -1;
      lr_neg_c_in_clause_start[i]->next = lr_neg_c_in_clause_end[i];
    } else {
      printf("\nvar in r CNF mem lists is not new??\n\n");
      exit( 1 );
    }
  }

  /* insert clauses from back to front; dunno if that has any significant effect
   * but it's become a kind of tradition..
   */
  for ( i = lr_num_clauses - 1; i >= 0; i-- ) {
    for ( j = 0; j < lr_clause_length[i]; j++ ) {
      if ( lr_clauses[i][j].literal > 0 ) {
	/* positive, is the var.
	 */
	v = lr_clauses[i][j].literal;
	if ( lr_pos_c_in_clause_end[v]->clause == -1 ) {
	  /* we're at the end of the allocated list!
	   */
	  lr_pos_c_in_clause_end[v]->clause = i;
	  lr_pos_c_in_clause_end[v]->next = new_MemberList();
	  lr_pos_c_in_clause_end[v]->next->clause = -1;
	  lr_pos_c_in_clause_end[v] = lr_pos_c_in_clause_end[v]->next;
	} else {
	  /* we're still in the middle of the list.
	   */
	  lr_pos_c_in_clause_end[v]->clause = i;
	  lr_pos_c_in_clause_end[v] = lr_pos_c_in_clause_end[v]->next;
	} /* case distinction for end pointer of v list */
      } else { /* literal negative; do the same, basically */
	v = (-1) * lr_clauses[i][j].literal;
	if ( lr_neg_c_in_clause_end[v]->clause == -1 ) {
	  /* we're at the end of the allocated list!
	   */
	  lr_neg_c_in_clause_end[v]->clause = i;
	  lr_neg_c_in_clause_end[v]->next = new_MemberList();
	  lr_neg_c_in_clause_end[v]->next->clause = -1;
	  lr_neg_c_in_clause_end[v] = lr_neg_c_in_clause_end[v]->next;
	} else {
	  /* we're still in the middle of the list.
	   */
	  lr_neg_c_in_clause_end[v]->clause = i;
	  lr_neg_c_in_clause_end[v] = lr_neg_c_in_clause_end[v]->next;
	} /* case distinction for end pointer of v list */
      } /* literal sign distinction */
    } /* clause elements */
  } /* clauses */

  if ( gcmd_line.R && gcmd_line.debug >= 5 ) {
    printf("\nr membership lists:");
    for ( i = 1; i <= lr_num_c; i++ ) {
      printf("\nr var %d pos:", i);
      for ( i_ml = lr_pos_c_in_clause_start[i]->next; 
	    i_ml != lr_pos_c_in_clause_end[i]; i_ml = i_ml->next ) {
	printf(" %d", i_ml->clause);
      }
      printf("\nr var %d neg:", i);
      for ( i_ml = lr_neg_c_in_clause_start[i]->next; 
	    i_ml != lr_neg_c_in_clause_end[i]; i_ml = i_ml->next ) {
	printf(" %d", i_ml->clause);
      }
    }
    fflush(stdout);
  }

}



int get_1P_and_H( State *S, State *current_goals, 
		  EhcNode *ehc_father, BfsNode *bfs_father, int S_op )

{

  int h, max, i;
  Bool reach;
  int retval;

  if ( contains_goal( S, current_goals ) ) {
    return 0;
  }

  gevaluated_states++;

  times( &end );
  TIME( gsearch_time );
  times( &start );

  source_to_dest( &lcurrent_goals, current_goals );  

  initialize_fixpoint( S, ehc_father, bfs_father, S_op );
  reach = build_fixpoint( S, &max );
  if ( gcmd_line.display_info == 122 ||
       (gcmd_line.R && gcmd_line.debug) ) {
    print_fixpoint_result();
    fflush(stdout);
  }

  if ( reach ) {
    if ( gcmd_line.R && gcmd_line.debug ) {
      printf("\nnow extracting rplan!");
      fflush(stdout);
    }
    h = extract_1P( max, TRUE );
  } else {
    if ( gcmd_line.R && gcmd_line.debug ) {
      printf("\nrpg terminated unsuccessfully! no rplan!");
      fflush(stdout);
    }
    h = INFINITY;
  }

  reset_fixpoint( max );

  /* replace helpfuls with A if wanted
   */
  if ( !gcmd_line.help ) {
    for ( i = 0; i < gnum_H; i++ ) {
      gop_conn[gH[i]].is_in_H = FALSE;
    }
    get_A( S );
    for ( i = 0; i < gnum_A; i++ ) {
      gH[i] = gA[i];
      gop_conn[gH[i]].is_in_H = TRUE;
    }
    gnum_H = gnum_A;
  }

  times( &end );
  TIME( geval_time );
  times( &start );


  /* for heuristic 0, infinity doesn't mean the state is a dead end;
   *
   * for heuristics 0 and 1, 0 doesn't mean we've reached the goal,
   * so we need to take care that the return h value is 0 only if the goals
   * are true (which is checked above)
   */
  retval = h;
  if ( gcmd_line.heuristic == 0 ) {
    if ( retval == INFINITY ) {
      retval = BIG_H;
    } else {
      retval = h + 1;
    }
  }
  if ( gcmd_line.heuristic == 1 ) {
    if ( retval != INFINITY ) {
      retval = h + 1;
    }
  }

  return retval;

}



int get_1P( State *S, State *current_goals,
	    EhcNode *ehc_father, BfsNode *bfs_father, int S_op )

{

  int h, max;
  Bool reach;
  int retval;

  if ( contains_goal( S, current_goals ) ) {
    return 0;
  }

  gevaluated_states++;

  times( &end );
  TIME( gsearch_time );
  times( &start );

  source_to_dest( &lcurrent_goals, current_goals );  

  initialize_fixpoint( S, ehc_father, bfs_father, S_op );
  reach = build_fixpoint( S, &max );
  if ( gcmd_line.display_info == 122 ||
       (gcmd_line.R && gcmd_line.debug) ) {
    print_fixpoint_result();
    fflush(stdout);
  }

  if ( reach ) {
    if ( gcmd_line.R && gcmd_line.debug ) {
      printf("\nnow extracting rplan!");
      fflush(stdout);
    }
    h = extract_1P( max, FALSE );
  } else {
    if ( gcmd_line.R && gcmd_line.debug ) {
      printf("\nrpg terminated unsuccessfully! no rplan!");
      fflush(stdout);
    }
    h = INFINITY;
  }

  reset_fixpoint( max );

  times( &end );
  TIME( geval_time );
  times( &start );

  /* for heuristic 0, infinity doesn't mean the state is a dead end;
   *
   * for heuristics 0 and 1, 0 doesn't mean we've reached the goal,
   * so we need to take care that the return h value is 0 only if the goals
   * are true (which is checked above)
   */
  retval = h;
  if ( gcmd_line.heuristic == 0 ) {
    if ( retval == INFINITY ) {
      retval = BIG_H;
    } else {
      retval = h + 1;
    }
  }
  if ( gcmd_line.heuristic == 1 ) {
    if ( retval != INFINITY ) {
      retval = h + 1;
    }
  }

  return retval;

}



void get_A( State *S )

{

  int i, j, ft;

  /* unset prev.
   */
  for ( i = 0; i < gnum_A; i++ ) {
    gop_conn[gA[i]].is_in_A = FALSE;
  }
  gnum_A = 0;

  /* the 0p ops
   */
  for ( i = 0; i < lnum_0P_O; i++ ) {
    if ( gop_conn[l0P_O[i]].is_in_A ) {
      continue;
    }
    gop_conn[l0P_O[i]].is_in_A = TRUE;
    gA[gnum_A++] = l0P_O[i];
  }

  /* the ops activated by our facts.
   */
  lnum_ch_O = 0;
  for ( i = 0; i < S->num_F; i++ ) {
    ft = S->F[i];
    for ( j = 0; j < gft_conn[ft].num_P; j++ ) {
      gop_conn[gft_conn[ft].P[j]].num_active_Ps++;
      if ( !gop_conn[gft_conn[ft].P[j]].ch ) {
	gop_conn[gft_conn[ft].P[j]].ch = TRUE;
	lch_O[lnum_ch_O++] = gft_conn[ft].P[j];
      }
      if ( gop_conn[gft_conn[ft].P[j]].num_active_Ps >=
	   gop_conn[gft_conn[ft].P[j]].num_P ) {
	/* DEBUGGING, REMOVE LATER
	 */
	if ( gop_conn[gft_conn[ft].P[j]].is_in_A ) {
	  printf("\n\nactivated op already in??\n\n");
	  fflush(stdout);
	  exit( 1 );
	}
	gop_conn[gft_conn[ft].P[j]].is_in_A = TRUE;
	gA[gnum_A++] = gft_conn[ft].P[j];
      }
    }
  }

  for ( i = 0; i < lnum_ch_O; i++ ) {
    gop_conn[lch_O[i]].num_active_Ps = 0;
    gop_conn[lch_O[i]].ch = FALSE;
  }

}






























/*******************************
 * RELAXED FIXPOINT ON A STATE *
 *******************************/





























Bool build_fixpoint( State *S, int *max )

{

  int start_ft, stop_ft, start_ef, stop_ef, i, time = 0;

  Bool new_U;

  start_ft = 0;
  start_ef = 0;
  while ( TRUE ) {
    if ( all_goals_activated( time ) ) {
      break;
    }

    stop_ft = lnum_F;
    for ( i = start_ft; i < stop_ft; i++ ) {
      activate_ft( lF[i], time );
    }

    if ( time == 0 ) {
      for ( i = 0; i < lnum_0P_E; i++ ) {
	if ( gef_conn[l0P_E[i]].in_E ) {
	  continue;
	}
	new_ef( l0P_E[i] );
      }
    }

    stop_ef = lnum_E;
    for ( i = start_ef; i < stop_ef; i++ ) {
      activate_ef( lE[i], time );
    }

    new_U = append_new_U_layer( time );

    if ( stop_ft == lnum_F && !new_U ) {
      *max = time;
      return FALSE;
    }

    start_ft = stop_ft;
    start_ef = stop_ef;
    time++;
  }

  *max = time;
  return TRUE;

}    



void initialize_fixpoint( State *S, 
			  EhcNode *ehc_father, BfsNode *bfs_father, int S_op )

{

  int i;

  lnum_E = 0;
  lnum_ch_E = 0;

  lnum_F = 0;
  for ( i = 0; i < S->num_F; i++ ) {
    if ( gft_conn[S->F[i]].in_F ) {
      continue;
    }
    new_fact( S->F[i] );
  }

  /* create the U graph up to this state here.
   */
  insert_path_implications( S, ehc_father, bfs_father, S_op );

}
   


void activate_ft( int index, int time )

{

  int i;

  gft_conn[index].level = time;

  for ( i = 0; i < gft_conn[index].num_PC; i++ ) {
    gef_conn[gft_conn[index].PC[i]].num_active_PCs++;
    if ( !gef_conn[gft_conn[index].PC[i]].ch ) {
      gef_conn[gft_conn[index].PC[i]].ch = TRUE;
      lch_E[lnum_ch_E++] = gft_conn[index].PC[i];
    }
    if ( gef_conn[gft_conn[index].PC[i]].num_active_PCs ==
	 gef_conn[gft_conn[index].PC[i]].num_PC ) {
      new_ef( gft_conn[index].PC[i] );
    }
  }

}



void activate_ef( int index, int time )

{

  int i;

  gef_conn[index].level = time;

  for ( i = 0; i < gef_conn[index].num_A; i++ ) {
    if ( gft_conn[gef_conn[index].A[i]].in_F ) {
      continue;
    }
    if ( gef_conn[index].A_nondet[i] ) {
      continue;
    }
    new_fact( gef_conn[index].A[i] );
  }

}



void new_fact( int index )

{

  lF[lnum_F++] = index;
  gft_conn[index].in_F = TRUE;

}



void new_ef( int index )

{

  lE[lnum_E++] = index;
  gef_conn[index].in_E = TRUE;

}



/* different versions of path implications are implemented; see -h option.
 */
void insert_path_implications( State *S, EhcNode *ehc_father, BfsNode *bfs_father, int S_op )

{

  static Bool fc = TRUE;
  static State_pointer *path;
  static int *path_op;
  static Bool *Ut, *Utpp;
  
  int i, j, k, ef, num_path, t, c, conu, addu, time, S_num_path = -17;
  int Ucond, nowUft, notc, ft;
  EhcNode *iehc;
  BfsNode *ibfs;

  if ( fc ) {
    path = ( State_pointer * ) calloc( MAX_PLAN_LENGTH + 1, sizeof( State_pointer ) );
    path_op = ( int * ) calloc( MAX_PLAN_LENGTH + 1, sizeof( int ) );
    
    Ut = ( Bool * ) calloc( gnum_ft_conn, sizeof( Bool ) );
    Utpp = ( Bool * ) calloc( gnum_ft_conn, sizeof( Bool ) );
    for ( i = 0; i < gnum_ft_conn; i++ ) {
      Ut[i] = FALSE;
      Utpp[i] = FALSE;
    }
    
    fc = FALSE;
  }

  if ( gcmd_line.R && gcmd_line.debug ) {
    printf("\n\n-----------------------rp path");
    fflush(stdout);
  }
  num_path = MAX_PLAN_LENGTH;
/*   path[num_path--] = S; */
/*   path_op[num_path+1] = S_op; */
/*   S_num_path = num_path; */
/*   if ( gcmd_line.R && gcmd_line.debug ) { */
/*     printf("\n-----end state %d", num_path+1); print_state( *path[num_path+1] ); */
/*     fflush(stdout); */
/*   } */
  /* S_num_path stores the point on path at which non-fixed-clauses-part
   * begins. at least I hope so... :-|
   *
   * when building r_dynamic clauses base, the process will step through
   * t = S_num_path+1 ... MAX_PLAN_LENGTH-1, and build clauses from t to t+1
   */
  if ( ehc_father ) {
    if ( bfs_father ) {
      printf("\n\nehc father AND bfs father?!\n\n");
      exit( 1 );
    }
    if ( ehc_father->op == -1 ) {
      /* this here has been produced directly from start state.
       * must include whole path up to that start state.
       */
      path_op[num_path] = S_op;
      path[num_path--] = S;
      if ( gcmd_line.R && gcmd_line.debug ) {
	printf("\n--- op: "); print_op_name( S_op );
	printf("\n-----ehc op -1 end state %d", num_path+1); 
	print_state( *path[num_path+1] );
	fflush(stdout);
      }

      S_num_path = num_path - 1;
      for ( i = gnum_plan_ops; i >= 0; i-- ) {
	if ( num_path == 0 ) {
	  printf("\n\nincrease MAX_PLAN_LENGTH! currently %d\n\n",
		 MAX_PLAN_LENGTH);
	  fflush(stdout);
	  exit( 1 );
	}
	if ( i > 0 ) {
	  path_op[num_path] = gplan_ops[i-1];
	}
	path[num_path--] = &(gplan_states[i]);
	if ( gcmd_line.R && gcmd_line.debug ) {
	  if ( i > 0 ) {
	    printf("\n--- op: "); print_op_name( gplan_ops[i-1] );
	  }
	  printf("\n-----ehc op -1 then gplan state %d", num_path+1); 
	  print_state( *path[num_path+1] );
	  fflush(stdout);
	}
      }
    } /* ehc father->op == -1 */ else {
      /* this is a longer way from ehc iteration start state;
       * include the states on that way (without start), and then 
       * the states on the path to the start (including it)
       */
      path_op[num_path] = S_op;
      path[num_path--] = S;
      if ( gcmd_line.R && gcmd_line.debug ) {
	printf("\n--- op: "); print_op_name( S_op );
	printf("\n-----ehc op != -1 end state %d", num_path+1); 
	print_state( *path[num_path+1] );
	fflush(stdout);
      }
      for ( iehc = ehc_father; iehc->father; iehc = iehc->father ) {
	if ( num_path == 0 ) {
	  printf("\n\nincrease MAX_PLAN_LENGTH! currently %d\n\n",
		 MAX_PLAN_LENGTH);
	  fflush(stdout);
	  exit( 1 );
	}
	path_op[num_path] = iehc->op;
	path[num_path--] = &(iehc->S);
	if ( gcmd_line.R && gcmd_line.debug ) {
	  printf("\n--- op: "); print_op_name( iehc->op );
	  printf("\n-----ehc father state %d", num_path+1); print_state( *path[num_path+1] );
	  fflush(stdout);
	}
      }
      S_num_path = num_path - 1;
      if ( S_num_path < 0 ) {
	printf("\n\nS_num_path < 0 ?? \n\n"); fflush(stdout);
	exit( 1 );
      }
      for ( i = gnum_plan_ops; i >= 0; i-- ) {
	if ( num_path == 0 ) {
	  printf("\n\nincrease MAX_PLAN_LENGTH! currently %d\n\n",
		 MAX_PLAN_LENGTH);
	  fflush(stdout);
	  exit( 1 );
	}
	if ( i > 0 ) {
	  path_op[num_path] = gplan_ops[i-1];
	}
	path[num_path--] = &(gplan_states[i]);
	if ( gcmd_line.R && gcmd_line.debug ) {
	  if ( i > 0 ) {
	    printf("\n--- op: "); print_op_name( gplan_ops[i-1] );
	  }
	  printf("\n-----ehc father then gplan state %d", num_path+1); print_state( *path[num_path+1] );
	  fflush(stdout);
	}
      }
    } /* ehc father->op != -1 */
  } /* if ehc_father */
  if ( bfs_father ) {
    if ( ehc_father ) {
      printf("\n\nbfs father AND ehc father?!\n\n");
      exit( 1 );
    }
    path_op[num_path] = S_op;
    path[num_path--] = S;
    if ( gcmd_line.R && gcmd_line.debug ) {
      printf("\n--- op: "); print_op_name( S_op );
      printf("\n-----bfs end state %d", num_path+1); 
      print_state( *path[num_path+1] );
      fflush(stdout);
    }
    for ( ibfs = bfs_father; ibfs; ibfs = ibfs->father ) {
      if ( num_path == 0 ) {
	printf("\n\nincrease MAX_PLAN_LENGTH! currently %d\n\n",
	       MAX_PLAN_LENGTH);
	exit( 1 );
      }
      if ( ibfs->father ) {
	path_op[num_path] = ibfs->op;
      }
      path[num_path--] = &(ibfs->S);
      if ( gcmd_line.R && gcmd_line.debug ) {
	if ( ibfs->father ) {
	  printf("\n--- op: "); print_op_name( ibfs->op );
	}
	printf("\n-----bfs state %d", num_path+1); print_state( *path[num_path+1] );
	fflush(stdout);
      }
    }
    S_num_path = num_path;
  }
  if ( !ehc_father && !bfs_father ) {
    /* beginning of new ehc iteration: path is simply curr gplan; end state of that
     * we already got in S!!!
     */
    /* note: this might also be very first state in BFS or manual
     * -- in which case gnum_plan_ops == 0!
     */
    S_num_path = num_path - 1;
    for ( i = gnum_plan_ops; i >= 0; i-- ) {
      if ( num_path == 0 ) {
	printf("\n\nincrease MAX_PLAN_LENGTH! currently %d\n\n",
	       MAX_PLAN_LENGTH);
	fflush(stdout);
	exit( 1 );
      }
      if ( i > 0 ) {
	path_op[num_path] = gplan_ops[i-1];
      }
      path[num_path--] = &(gplan_states[i]);
      if ( gcmd_line.R && gcmd_line.debug ) {
	if ( i > 0 ) {
	  printf("\n--- op: "); print_op_name( gplan_ops[i-1] );
	}
	printf("\n-----not ehc not bfs state %d", num_path+1); print_state( *path[num_path+1] );
	fflush(stdout);
      }
    }
  }
  if ( S_num_path == -17 ) {
    printf("\n\nS_num_path wasn't set?\n\n");
    exit( 1 );
  }



  if ( gcmd_line.heuristic == 2 ) {
    /* we build impli graph only for the RPG, ie nothing much to build here!
     *
     * what we DO do here is to create the CNF encoding of the S semantics.
     */
    times( &end );
    TIME( geval_time );
    times( &start );    
    
    /* extend the current fixed clauses with the search path up to S.
     */
    lStime = r_extend_dynamic_clauses_base( path, S_num_path, path_op );

    times( &end );
    TIME( gr_cnf_time );
    times( &start );

    /* then extend the encoding, ie map the literal / time pairs
     * into integers.
     *
     * the translation code is in table gcodes for use below in the
     * single checks.
     */
    extend_dynamic_clauses_base_encoding( gnum_fixed_clauses, gnum_fixed_c );

    times( &end );
    TIME( gr_enc_time );
    times( &start );


    /* now build the U nodes for the initial RPG layer corresp. to S
     */
    lpath_U_length = 0; 
    t = MAX_PLAN_LENGTH;
    lnum_U[lpath_U_length] = 0;
    for ( i = 0; i < path[t]->num_U; i++ ) {
      if ( lnum_U[lpath_U_length] == gmax_U ) {
	printf("\n\ntoo many U facts in rpg?\n\n");
	fflush(stdout);
	exit( 1 );
      }
      /* DEBUGGING, REMOVE LATER
       */
      for ( j = 0; j < lnum_U[lpath_U_length]; j++ ) {
	if ( lU[lpath_U_length][j].ft == path[t]->U[i] ) {
	  printf("\n\npath state U contains same fact twice? ");
	  print_ft_name( path[t]->U[i] );
	  printf("\n\n");
	  exit( 1 );
	}
      }
      lU[lpath_U_length][lnum_U[lpath_U_length]].ft = path[t]->U[i];
      lU[lpath_U_length][lnum_U[lpath_U_length]].became_F = FALSE;
      lU[lpath_U_length][lnum_U[lpath_U_length]++].num_in = 0;
    }
    lpath_U_length++;

    return;
  } /* gcmd_line.heuristic == 2 */




  /* heuristic is 0 or 1 -- we need the path to s
   */

  /* first, build all the layers up to current state.
   */  
  lpath_U_length = 0;
  for ( t = num_path + 1; t <= MAX_PLAN_LENGTH; t++ ) {
    if ( lpath_U_length == RELAXED_STEPS + MAX_PLAN_LENGTH ) {
      printf("\n\nU graph too long! increase RELAXED_STEPS (now %d)\n\n",
	     RELAXED_STEPS );
      fflush(stdout);
      exit( 1 );
    }
    lnum_U[lpath_U_length] = 0;
    for ( i = 0; i < path[t]->num_U; i++ ) {
      if ( lnum_U[lpath_U_length] == gmax_U ) {
	printf("\n\ntoo many U facts in rpg?\n\n");
	fflush(stdout);
	exit( 1 );
      }
      /* DEBUGGING, REMOVE LATER
       */
      for ( j = 0; j < lnum_U[lpath_U_length]; j++ ) {
	if ( lU[lpath_U_length][j].ft == path[t]->U[i] ) {
	  printf("\n\npath state U contains same fact twice? ");
	  print_ft_name( path[t]->U[i] );
	  printf("\n\n");
	  exit( 1 );
	}
      }
      lU[lpath_U_length][lnum_U[lpath_U_length]].ft = path[t]->U[i];
      lU[lpath_U_length][lnum_U[lpath_U_length]].became_F = FALSE;
      lU[lpath_U_length][lnum_U[lpath_U_length]++].num_in = 0;
    }
    lpath_U_length++;
  }

  /* now, include the NOOPs
   */
  for ( t = num_path + 1; t < MAX_PLAN_LENGTH; t++ ) {
    time = t - (num_path + 1);/* the time index of the lower state */
    for ( i = 0; i < lnum_U[time]; i++ ) {
      for ( j = 0; j < lnum_U[time+1]; j++ ) {
	if ( lU[time][i].ft == lU[time+1][j].ft ) break;
      }
      if ( j == lnum_U[time+1] ) continue;
      /* insert noop
       */
      if ( lU[time+1][j].num_in == MAX_UEDGES ) {
	printf("\n\ntoo many U edges! increase MAX_UEDGES (now %d)\n\n",
	       MAX_UEDGES );
	fflush(stdout);
	exit( 1 );
      }
      lU[time+1][j].in_edges[lU[time+1][j].num_in] = &(lU[time][i]);
      lU[time+1][j].in_efs[lU[time+1][j].num_in] = -1;
      lU[time+1][j].num_in++;
      if ( gcmd_line.R && gcmd_line.debug ) {
	printf("\ninserted NOOP edge %d -> %d: ", time, time+1);
	print_ft_name( lU[time][i].ft );
	printf(" -> ");
	print_ft_name( lU[time+1][j].ft );
	fflush(stdout);
      }
    }

    /* now, NOOPs for facts p that are F at t and U at t+1
     * insert, for one eff that del p, (-c(t),p(t+1)) for all unknown
     * c of eff ==> is stronger than the real implication
     * (-c +..+ -c) * .. * (-c +..+ -c) (t) => p(t+1)
     */
    /* first, make fast accessible the (real!, ie not for_free) U info at t and t+1
     */
    for ( i = 0; i < path[t]->num_U; i++ ) {
      Ut[path[t]->U[i]] = TRUE;
    }
    for ( i = 0; i < path[t+1]->num_U; i++ ) {
      Utpp[path[t+1]->U[i]] = TRUE;
    }
    for ( i = 0; i < path[t]->num_F; i++ ) {
      ft =  path[t]->F[i];
      if ( !Utpp[ft] ) continue;
      /* find an unknown eff that del ft
       */
      ef = -1;
      for ( j = 0; j < path[t+1]->num_unknown_E; j++ ) {
	ef = path[t+1]->unknown_E[j];
	for ( k = 0; k < gef_conn[ef].num_D; k++ ) {
	  if ( ft == gef_conn[ef].D[k] ) break;
	}
	if ( k < gef_conn[ef].num_D ) break;
      }
      if ( j == path[t+1]->num_unknown_E ) {
	printf("\nft F at t-1, U at t, but no unknown del on it?\n\n");
	exit( 1 );
      }
      if ( ef < 0 ) {
	printf("\nef < 0 in rplan-nodel-noops?\n\n");
	exit( 1 );
      }
      for ( j = 0; j < gef_conn[ef].num_C; j++ ) {
	c = gef_conn[ef].C[j];
	if ( !Ut[c] ) continue;
	notc = gft_conn[c].negation;
	if ( notc == -1 ) {
	  printf("\neffcond -- in rplan-nodel-noops -- has no negation?\n\n");
	  exit( 1 );
	}
	if ( !Ut[notc] ) {
	  printf("\nnegated effcond -- in rplan-nodel-noops -- is not unknown?\n\n");
	  exit( 1 );
	}
	/* find lU nodes corresponding to notc(t) and ft(t+1)
	 */
	for ( Ucond = 0; Ucond < lnum_U[time]; Ucond++ ) {
	  if ( lU[time][Ucond].ft == notc ) break;
	}
	if ( Ucond == lnum_U[time] ) {
	  printf("can't find negated effcond -- in rplan-nodel-noops -- in lU?\n\n");
	  fflush(stdout);
	  exit( 1 );
	}
	for ( nowUft = 0; nowUft < lnum_U[time+1]; nowUft++ ) {
	  if ( lU[time+1][nowUft].ft == ft ) break;
	}
	if ( nowUft == lnum_U[time+1] ) {
	  printf("can't find ft(t) -- in rplan-nodel-noops -- in lU?\n\n");
	  fflush(stdout);
	  exit( 1 );
	}
	/* we're ready. insert the edge!
	 */
	lU[time+1][nowUft].in_edges[lU[time+1][nowUft].num_in] = &(lU[time][Ucond]);
	lU[time+1][nowUft].in_efs[lU[time+1][nowUft].num_in] = -1;
	lU[time+1][nowUft].num_in++;
	if ( gcmd_line.R && gcmd_line.debug ) {
	  printf("\ninserted nondel NOOP edge %d -> %d: ", time, time+1);
	  print_ft_name( notc );
	  printf(" -> ");
	  print_ft_name( ft );
	  fflush(stdout);
	}
      } /* for all conditions c of U eff that del ft */
    } /* for all i (ft) in F(t) */

    /* unset infos
     */
    for ( i = 0; i < path[t]->num_U; i++ ) {
      Ut[path[t]->U[i]] = FALSE;
    }
    for ( i = 0; i < path[t+1]->num_U; i++ ) {
      Utpp[path[t+1]->U[i]] = FALSE;
    }
  } /* NOOPs - t */


  /*************************************************************
   * now, the OPs
   */

  /* go along the path just like in clause generation, and
   * if an unknown add effect on an unknown fact l' in S
   * occurs then insert the edge  l -> l' where l is the first
   * found cond that is unknown.
   */
  for ( t = num_path + 1; t < MAX_PLAN_LENGTH; t++ ) {
    time = t - (num_path + 1);/* the time index of the lower state */

    /* first, make fast accessible the U info at t and t+1
     */
    for ( i = 0; i < path[t]->num_U; i++ ) {
      Ut[path[t]->U[i]] = TRUE;
    }
    for ( i = 0; i < path[t+1]->num_U; i++ ) {
      Utpp[path[t+1]->U[i]] = TRUE;
    }

    for ( i = 0; i < path[t+1]->num_unknown_E; i++ ) {
      ef = path[t+1]->unknown_E[i];
      for ( c = 0; c < gef_conn[ef].num_C; c++ ) {
	if ( Ut[gef_conn[ef].C[c]] ) break;
      }
      /* DEBUGGING; REMOVE LATER
       */
      if ( c == gef_conn[ef].num_C ) {
	printf("\n\nunknown ef has all conds known??\n\n");
	fflush(stdout);
	exit( 1 );
      }
      /* node in path U
       */
      for ( k = 0; k < lnum_U[time]; k++ ) {
	if ( lU[time][k].ft ==  gef_conn[ef].C[c] ) break;
      }
      if ( k == lnum_U[time] ) {
	printf("can't find con ft on rp path?\n\n");
	fflush(stdout);
	exit( 1 );
      }
      conu = k;
      /* now, walk through the adds.
       */
      for ( j = 0; j < gef_conn[ef].num_A; j++ ) {
	if ( !Utpp[gef_conn[ef].A[j]] ) continue;/* known in next step */
	if ( gef_conn[ef].A_nondet[j] ) {
	  /* no implication edges based on nondet effects!
	   */
	  continue;
	}
	/* we insert the edge gef_conn[ef].C[c] -> gef_conn[ef].A[j]
	 * where c is the first condition of ef we found above that is unknown both
	 * at execution point and in our state.
	 */
	/* edge U[time] -> U[time+1]; first find node.
	 */
	for ( k = 0; k < lnum_U[time+1]; k++ ) {
	  if ( lU[time+1][k].ft == gef_conn[ef].A[j] ) break;
	}
	/* DEBUGGING
	 */
	if ( k == lnum_U[time+1] ) {
	  printf("can't find added ft on rp path?\n\n");
	  fflush(stdout);
	  exit( 1 );
	}
	addu = k;
	if ( lU[time+1][addu].num_in == MAX_UEDGES ) {
	  printf("\n\ntoo many U edges! increase MAX_UEDGES (now %d)\n\n",
		 MAX_UEDGES );
	  fflush(stdout);
	  exit( 1 );
	}
	lU[time+1][addu].in_edges[lU[time+1][addu].num_in] = &(lU[time][conu]);
	lU[time+1][addu].in_efs[lU[time+1][addu].num_in] = ef;
	lU[time+1][addu].num_in++;
	if ( gcmd_line.R && gcmd_line.debug ) {
	  printf("\ninserted OP edge %d -> %d: ", time, time + 1);
	  print_ft_name( gef_conn[ef].C[0] );
	  printf(" -> ");
	  print_ft_name( gef_conn[ef].A[j] );
	  fflush(stdout);
	}
      }/* A of u-ef */
    }/* u-efs at t */

    /* unset infos
     */
    for ( i = 0; i < path[t]->num_U; i++ ) {
      Ut[path[t]->U[i]] = FALSE;
    }
    for ( i = 0; i < path[t+1]->num_U; i++ ) {
      Utpp[path[t+1]->U[i]] = FALSE;
    }
  }/* t on path */

}



/* returns TRUE if a new fact has become unknown
 * rather than false, or has become true rather than unknown, or
 * has become new facts in its backwards reachability tree.
 */
Bool append_new_U_layer( time )

{

  static Bool fc = TRUE;
  static int *Ups, *ce;

  int num_ce;
  int i, j, k, conu, addu, ft, ef, Utime, l;

  Bool new_U = FALSE;

  /* the state to be evaluated is in U layer
   * lpath_U_length - 1!!
   */
  Utime = time + lpath_U_length - 1;
  if ( Utime + 1 == RELAXED_STEPS + MAX_PLAN_LENGTH ) {
    printf("\n\nU graph is too long! increase RELAXED_STEPS (now %d)\n\n",
	   RELAXED_STEPS );
    fflush(stdout);
    exit( 1 );
  }

  if ( gcmd_line.R && gcmd_line.debug ) {
    printf("\nappend new U layer call, time %d, Utime %d", time, Utime);
  }

  if ( fc ) {
    Ups = ( int * ) calloc( gnum_ef_conn, sizeof( int ) );
    ce = ( int * ) calloc( gnum_ef_conn, sizeof( int ) );
    for ( i = 0; i < gnum_ef_conn; i++ ) {
      Ups[i] = 0;
    }

    fc = FALSE;
  }

  lnum_U[Utime+1] = 0;
  /* first, transfer U facts to new layer, if they're not yet added.
   */
  for ( i = 0; i < lnum_U[Utime]; i++ ) {
    if ( lU[Utime][i].became_F ) {
      /* ignore those that became true already
       */
      continue;
    }
    ft = lU[Utime][i].ft;
    if ( gft_conn[ft].in_F ) {
      /* this one's now added
       */
      continue;
    }
    if ( lnum_U[Utime+1] == gmax_U ) {
      printf("\n\ntoo many U facts in rpg?\n\n");
      fflush(stdout);
      exit( 1 );
    }
    lU[Utime+1][lnum_U[Utime+1]].ft = ft;
    lU[Utime+1][lnum_U[Utime+1]].became_F = FALSE;
    lU[Utime+1][lnum_U[Utime+1]].num_in = 0;
    /* the "noop" edge
     */
    if ( lU[Utime+1][lnum_U[Utime+1]].num_in == MAX_UEDGES ) {
      printf("\n\ntoo many U edges! increase MAX_UEDGES (now %d)\n\n",
	     MAX_UEDGES );
      fflush(stdout);
      exit( 1 );
    }
    lU[Utime+1][lnum_U[Utime+1]].in_edges[lU[Utime+1][lnum_U[Utime+1]].num_in] = 
      &(lU[Utime][i]);
    lU[Utime+1][lnum_U[Utime+1]].in_efs[lU[Utime+1][lnum_U[Utime+1]].num_in] = 
      -1;
    /* increment edges and U fact at Utime+1 counters.
     */
    lU[Utime+1][lnum_U[Utime+1]].num_in++;
    lnum_U[Utime+1]++;
  }

  if ( gcmd_line.R && gcmd_line.debug >= 3 ) {
    printf("\nentry U on efs propag time %d, Utime %d", time, Utime);
    fflush(stdout);
  }

  /* now, see which new facts become unknown; determine 
   * unknown efs by stepping through U facts here, incrementing
   * counters of supported effects.
   */ 
  num_ce = 0;
  for ( i = 0; i < lnum_U[Utime]; i++ ) {
    if ( gcmd_line.R && gcmd_line.debug >= 3 ) {
      printf("\npropag fact "); print_ft_name( lU[Utime][i].ft );
    }
    if ( lU[Utime][i].became_F ) {
      /* ignore those that became true already
       */
      continue;
    }
    ft = lU[Utime][i].ft;
    for ( j = 0; j < gft_conn[ft].num_C; j++ ) {
      /* NOTE: we only step through the *C* effects!!!
       * thereby, we only accept effects whose op is already full in,
       * but the effect antecedent is (partly) open... seems unclear how
       * to proceed otherwise, as several pre+ef conds might be open.
       *
       * we insert a stronger binary implication, ie  c -> a for all unknown a
       * where c is the first ef cond we meet that is unknown.
       *
       * DOES THIS AFFECT RELAXED COMPLETENESS? -- hm? March 2004 I don't 
       * understand this question... 2 weeks later I guess the question refers to
       * demanding that the op is already completely in... I don't *think* it's a problem,
       * because we always require op prec to be strictly fulfilled.
       */
      ef = gft_conn[ft].C[j];
      if ( Ups[ef] == 0 ) {
	ce[num_ce++] = ef;
      }
      Ups[ef]++;
      if ( gcmd_line.R && gcmd_line.debug >= 3 ) {
	printf("\nadded 1 to val of ef %d, now %d, fact ", ef, Ups[ef]); print_ft_name( ft );
	fflush(stdout);
      }
      if ( gef_conn[ef].num_active_PCs + Ups[ef] == gef_conn[ef].num_PC ) {
	/* first, find the unknown condition. (Mar04: we could also just use ft here?!)
	 */
	conu = -1;
	for ( k = 0; k < gef_conn[ef].num_C; k++ ) {
	  for ( conu = 0; conu < lnum_U[Utime]; conu++ ) {
	    if ( gef_conn[ef].C[k] == lU[Utime][conu].ft ) break;
	  }
	  if ( conu < lnum_U[Utime] ) break;
	}
	/* DEBUGGING, REMOVE LATER.
	 */
	if ( k == gef_conn[ef].num_C ) {
	  printf("\n\nsupported ef got no unknown C?\n\n");
	  fflush(stdout);
	  exit( 1 );
	}
	if ( conu == -1 ) {
	  printf("\n\ncon == -1??\n\n");
	  fflush(stdout);
	  exit( 1 );
	}
	for ( k = 0; k < gef_conn[ef].num_A; k++ ) {
	  if ( gft_conn[gef_conn[ef].A[k]].in_F ) {
	    /* this add effect is known to be true already
	     */
	    continue;
	  }
	  if ( gef_conn[ef].A_nondet[k] ) {
	    /* no edges for nondet effects!
	     */
	    continue;
	  }
	  for ( addu = 0; addu < lnum_U[Utime+1]; addu++ ) {
	    if ( gef_conn[ef].A[k] == lU[Utime+1][addu].ft ) break;
	  }
	  if ( addu == lnum_U[Utime+1] ) {
	    /* this added ft is not registered unknown yet
	     */
	    if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
	      printf("\nnew U fact");
	      print_ft_name( gef_conn[ef].A[k] );
	      fflush(stdout);
	    }
	    if ( lnum_U[Utime+1] == gmax_U ) {
	      printf("\n\ntoo many U facts in rpg?\n\n");
	      fflush(stdout);
	      exit( 1 );
	    }
	    lU[Utime+1][lnum_U[Utime+1]].ft = gef_conn[ef].A[k];
	    lU[Utime+1][lnum_U[Utime+1]].became_F = FALSE;
	    lU[Utime+1][lnum_U[Utime+1]].num_in = 0;
	    
	    if ( lU[Utime+1][lnum_U[Utime+1]].num_in == MAX_UEDGES ) {
	      printf("\n\ntoo many U edges! increase MAX_UEDGES (now %d)\n\n",
		     MAX_UEDGES );
	      fflush(stdout);
	      exit( 1 );
	    }
	    lU[Utime+1][lnum_U[Utime+1]].in_edges[lU[Utime+1][lnum_U[Utime+1]].num_in] = 
	      &(lU[Utime][conu]);
	    lU[Utime+1][lnum_U[Utime+1]].in_efs[lU[Utime+1][lnum_U[Utime+1]].num_in] = 
	      ef;
	    lU[Utime+1][lnum_U[Utime+1]].num_in++;
	    lnum_U[Utime+1]++;
	    /* hand this event info back. no graph fixpoint yet.
	     */
	    new_U = TRUE;
	  } else {
	    /* else only insert ef edge; 
	     *
	     * AND: ONLY INSERT THAT EDGE IF WE DON'T HAVE AN EDGE TO lU[Utime][conu]->ft
	     *      ie to the enabling fact, already!!
	     */
	    for ( l = 0; l < lU[Utime+1][addu].num_in; l++ ) {
	      if ( lU[Utime+1][addu].in_edges[l]->ft ==
		   lU[Utime][conu].ft ) break;
	    }
	    if ( l == lU[Utime+1][addu].num_in ) {
	      if ( lU[Utime+1][addu].num_in == MAX_UEDGES ) {
		printf("\n\ntoo many added U edges! increase MAX_UEDGES (now %d)\n\n",
		       MAX_UEDGES );
		printf("\nadded ft: "); print_ft_name( lU[Utime+1][addu].ft );
		for ( k = 0; k < lU[Utime+1][addu].num_in; k++ ) {
		  printf("\nedge to: "); print_ft_name( lU[Utime+1][addu].in_edges[k]->ft );
		  printf(" by "); 
		  print_op_name( gef_conn[lU[Utime+1][addu].in_efs[k]].op );
		}
		printf("\n\n");
		fflush(stdout);
		exit( 1 );
	      }
	      lU[Utime+1][addu].in_edges[lU[Utime+1][addu].num_in] = &(lU[Utime][conu]);
	      lU[Utime+1][addu].in_efs[lU[Utime+1][addu].num_in] = ef;
	      lU[Utime+1][addu].num_in++;
	    }
	  } /* else -- added ft not new */
	} /* all added facts of new ef */
      } /* if unknown ef comes in */
      /* DEBUGGING, REMOVE LATER
       */
      if ( gef_conn[ef].num_active_PCs + Ups[ef] > gef_conn[ef].num_PC ) {
	printf("\n\nat time %d Utime %d sum F + U %d %d ef PC higher than total %d? ",
	       time, Utime, gef_conn[ef].num_active_PCs, Ups[ef], gef_conn[ef].num_PC);
	print_op_name( gef_conn[ef].op );
	printf(" supp here by ");
	print_ft_name( ft );
	printf("\n\n");
	fflush(stdout);
	exit( 1 );
      }
    } /* all efconds the ft participates in */
  } /* all unknown fts at Utime */
  /* unset the Ups info
   */
  for ( i = 0; i < num_ce; i++ ) {
    if ( gcmd_line.R && gcmd_line.debug >= 3 ) {
      printf("\nsetting %d to 0", ce[i]);
      fflush(stdout);
    }
    Ups[ce[i]] = 0;
  }



  /* now infere new facts! --- those for which the impleafs are
   * implied by the formula (initial h0/1 or state)
   */
  for ( i = 0; i < lnum_U[Utime+1]; i++ ) {
    /* DEBUGGING, REMOVE LATER
     */
    if ( lU[Utime+1][i].became_F ) {
      printf("\n\njust created U ft at time + 1 became F??\n\n");
      fflush(stdout);
      exit( 1 );
    }

    /* if a fact has only NOOPs as incoming edges, then it can't possibly
     * become true!
     */
    if ( lU[Utime+1][i].num_in == 1 ) {
      if ( lU[Utime+1][i].in_efs[0] == -1 ) {
	continue;
      }
    }


    if ( Uleaf_disjunction_implied_by_formula( Utime, &(lU[Utime+1][i]) ) ) {
      /* hand result over to the rest of the fixpoint algorithm!
       */
      lU[Utime+1][i].became_F = TRUE;
      new_fact( lU[Utime+1][i].ft );
      
      new_U = TRUE;
    }
  }



  /* it might be that no new ft became U, and no new fact
   * became F, but new fts became reachable from a node in
   * comparison to before so changes are still made.
   */
  if ( !new_U ) {
    if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
      printf("\nno new Us or Fs at %d. checking reachable fts", time+1);
      fflush( stdout );
    }
    for ( i = 0; i < lnum_U[Utime+1]; i++ ) {
      for ( j = 0; j < lnum_U[Utime]; j++ ) {
	if ( lU[Utime][j].ft == lU[Utime+1][i].ft ) break;
      }
      /* DEBUGGING; REMOVE LATER
       */
      if ( j == lnum_U[Utime] ) {
	printf("\n\ntime + 1 U ft not in time U though no new-U??\n\n");
	fflush(stdout);
	exit( 1 );
      }
      if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
	printf("\n"); print_ft_name( lU[Utime+1][i].ft );
	fflush(stdout);
      }
      if ( more_reachable_facts( Utime, &(lU[Utime+1][i]), &(lU[Utime][j]) ) ) {
	new_U = TRUE;
	break;
      }
    }
  }

  return new_U;

}



Bool more_reachable_facts( int Utime, UftNode *npp, UftNode *n )

{

  static Bool fc = TRUE;
  static Bool *on_npath;
  static UftNode_pointer *p, *pp;

  int num_p, num_pp, prev_p, now_p;
  int i, j, t, ft, ft_, k;

  if ( fc ) {
    on_npath = ( Bool * ) calloc( gnum_ft_conn, sizeof( Bool ) );
    p = ( UftNode_pointer * ) calloc( lMAX_PATHNODES, sizeof( UftNode_pointer ) );
    pp = ( UftNode_pointer * ) calloc( lMAX_PATHNODES, sizeof( UftNode_pointer ) );
    for ( i = 0; i < gnum_ft_conn; i++ ) {
      on_npath[i] = FALSE;
    }

    fc = FALSE;
  }

  /* set up n paths info
   */
  ft = n->ft;
  on_npath[ft] = TRUE;
  now_p = 0;
  p[0] = n;
  num_p = 1;
  for ( t = Utime; t >= 0; t-- ) {
    prev_p = now_p;
    now_p = num_p;
    for ( i = prev_p; (t > 0) ? (i < now_p) : (i < num_p); i++ ) {
      for ( j = 0; j < p[i]->num_in; j++ ) {
	/* see if we got this node already: avoid cycles!
	 */
	for ( k = 0; k < num_p; k++ ) {
	  if ( p[k] == p[i]->in_edges[j] ) break;
	}
	if ( k < num_p ) {
	  /* had this. skip it.
	   */
	  continue;
	}
	ft_ = p[i]->in_edges[j]->ft;
	on_npath[ft_] = TRUE;
	if ( num_p >= lMAX_PATHNODES ) {
	  printf("\n\n1 -- too many nodes on paths! increase lMAX_PATHNODES (now %d)\n\n",
		 lMAX_PATHNODES);
	  fflush(stdout);
	  exit( 1 );
	}
	p[num_p++] = p[i]->in_edges[j];
      }
    }
  }

  /* now check the npp reachability
   */
  ft = npp->ft;
  now_p = 0;
  pp[0] = npp;
  num_pp = 1;
  for ( t = Utime + 1; t >= 0; t-- ) {
    prev_p = now_p;
    now_p = num_pp;
    for ( i = prev_p; (t > 0) ? (i < now_p) : (i < num_pp); i++ ) {
      for ( j = 0; j < pp[i]->num_in; j++ ) {
	/* see if we got this node already: avoid cycles!
	 */
	for ( k = 0; k < num_pp; k++ ) {
	  if ( pp[k] == pp[i]->in_edges[j] ) break;
	}
	if ( k < num_pp ) {
	  /* had this. skip it.
	   */
	  continue;
	}
	ft_ = pp[i]->in_edges[j]->ft;
	if ( !on_npath[ft_] ) {
	  for ( i = 0; i < num_p; i++ ) {
	    on_npath[p[i]->ft] = FALSE;
	  }
	  if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
	    printf(" --- new reachable "); print_ft_name( ft_ );
	    fflush(stdout);
	  }
	  return TRUE;
	}
	if ( num_pp >= lMAX_PATHNODES ) {
	  printf("\n\n2 -- too many nodes on paths! increase lMAX_PATHNODES (now %d)\n\n",
		 lMAX_PATHNODES);
	  fflush(stdout);
	  exit( 1 );
	}
	pp[num_pp++] = pp[i]->in_edges[j];
      }
    }
  }

  for ( i = 0; i < num_p; i++ ) {
    on_npath[p[i]->ft] = FALSE;
  }

  return FALSE;

}




/* for all heuristics, the backchaining to determine the leafs 
 * is exactly the same -- it's the different impligraph that makes
 * the difference.
 */
Bool Uleaf_disjunction_implied_by_formula( int Utime, UftNode *n ) 

{

  static Bool fc = TRUE;
  static int *leafs;
  static Bool *is_leaf;
  static UftNode_pointer *p;

  int num_p, prev_p, now_p, num_leafs;
  int i, j, k, t, ft, ft_;

  if ( fc ) {
    leafs = ( int * ) calloc( gnum_ft_conn, sizeof( int ) );
    is_leaf = ( Bool * ) calloc( gnum_ft_conn, sizeof( Bool ) );
    p = ( UftNode_pointer * ) calloc( lMAX_PATHNODES, sizeof( UftNode_pointer ) );

    for ( i = 0; i < gnum_ft_conn; i++ ) {
      is_leaf[i] = FALSE;
    }

    fc = FALSE;
  }

  ft = n->ft;
  p[0] = n;
  now_p = 0;
  num_p = 1;
  num_leafs = 0;
  if ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
    printf("\nr starting at Utime %d + 1", Utime); print_ft_name( ft );
    fflush(stdout);
  }
  for ( t = Utime + 1; t > 0; t-- ) {
    /* include all edges from t to t-1
     */
    prev_p = now_p;
    now_p = num_p;
    for ( i = prev_p; i < now_p; i++ ) {
      /* loop over all incoming edges of the node.
       */
      if ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
	printf("\nr i at t %d: ", t); print_ft_name( p[i]->ft );
	fflush(stdout);
      }
      for ( j = 0; j < p[i]->num_in; j++ ) {
	/* see if we got this node already: avoid duplicates!
	 */
	for ( k = 0; k < num_p; k++ ) {
	  if ( p[k] == p[i]->in_edges[j] ) break;
	}
	if ( k < num_p ) {
	  /* had this. skip it.
	   */
	  continue;
	}
	if ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
	  printf("\nr new p: "); print_ft_name( p[i]->in_edges[j]->ft );
	  printf(" (by "); 
	  if ( p[i]->in_efs[j] == -1 ) {
	    printf("NOOP ");
	  } else {
	    if ( p[i]->in_efs[j] == -2 ) {
	      printf("OR constraint??\n\n");
	      fflush(stdout);
	      exit( 1 );
	    } else {
	      print_op_name( gef_conn[p[i]->in_efs[j]].op );
	    }
	  }
	  printf(")");
	  fflush(stdout);
	}
	ft_ = p[i]->in_edges[j]->ft;
	if ( t == 1 && !is_leaf[ft_] ) {
	  is_leaf[ft_] = TRUE;
	  leafs[num_leafs++] = ft_;
	  if ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
	    printf("\nr set is-l "); print_ft_name( ft_ );
	    fflush(stdout);
	  }
	}
	if ( num_p >= lMAX_PATHNODES ) {
	  printf("\n\nr 3 -- too many nodes on paths! increase lMAX_PATHNODES (now %d)\n\n",
		 lMAX_PATHNODES);
	  fflush(stdout);
	  exit( 1 );
	}
	p[num_p++] = p[i]->in_edges[j];
      } /* in edges of node p[i] */
    } /* all p[i] in prev level */
  } /* all levels from top to bottom */

  if ( gcmd_line.R && gcmd_line.debug  >= 2 ) {
    printf("\nr fact "); print_ft_name( ft );
    printf(" has leafs:");
    for ( i = 0; i < num_leafs; i++ ) {
      printf(" ");print_ft_name( leafs[i] );
    }
    printf("\nr now checking for implication by initial/state formula.");
    fflush(stdout);
  }


  if ( gcmd_line.heuristic == 0 ||
       gcmd_line.heuristic == 1 ) {
    if ( disjunction_implied_by_initial_formula( leafs, num_leafs, is_leaf ) ) {
      for ( i = 0; i < num_leafs; i++ ) {
	is_leaf[leafs[i]] = FALSE;
      }
      return TRUE;
    }
  }
  if ( gcmd_line.heuristic == 2 ) {
    if ( disjunction_implied_by_S_formula( leafs, num_leafs, is_leaf ) ) {
      for ( i = 0; i < num_leafs; i++ ) {
	is_leaf[leafs[i]] = FALSE;
      }
      return TRUE;
    }
  }

  for ( i = 0; i < num_leafs; i++ ) {
    is_leaf[leafs[i]] = FALSE;
  }

  return FALSE;

}



Bool disjunction_implied_by_initial_formula( int *dis, int num_dis, Bool *is_dis )

{

  static int *cdis;
  static Bool fc = TRUE;

  int i, j, k, m;
  int num_cdis;
  Bool sat;

  if ( fc ) {
    cdis = ( int * ) calloc( gnum_ft_conn, sizeof( int ) );
    fc = FALSE;
  }


  /* first, see if two of the leafs are contradictory
   * --> disjunction trivially implied, step out
   */
  for ( i = 0; i < num_dis; i++ ) {
    if ( gft_conn[dis[i]].negation != -1 &&
	 is_dis[gft_conn[dis[i]].negation] ) {
      if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
	printf("\ncontradictory ft pair in backchain leafs disjunction: ");
	print_ft_name(dis[i]);
	print_ft_name(gft_conn[dis[i]].negation);
	fflush(stdout);
      }
      return TRUE;
    }
  }


  /* create array of codes of these facts; use implicit
   * negated facts, ie. make these codes for coded literals as in the 
   * CNF.
   */
  num_cdis = 0;
  for ( i = 0; i < num_dis; i++ ) {
    if ( gft_conn[dis[i]].CNF ) {
      m = 1;
      cdis[num_cdis] = dis[i];
    } else {
      m = -1;
      cdis[num_cdis] = gft_conn[dis[i]].negation;
    }
    if ( lr_codes[cdis[num_cdis]] == -1 ) {
      /* special case: this is not encoded and therefore does not occur
       * in our initial state CNF. thus it can't be implied and we can
       * skip it from the disjunction; we just need to check if there are two
       * contradictory literals that are thrown away, in which case the implication
       * is true trivially; this has been done above already.
       */
      if ( gcmd_line.R && gcmd_line.debug  >= 2 ) {
	printf("\nun-encoded ft/0 pair in backchain leafs disjunction: ");
	print_ft_name(cdis[num_cdis]);
	fflush(stdout);
      }
      continue;
    }
    /* we have the code, the literal occurs. write it into the disj. code array.
     */
    cdis[num_cdis] = m * lr_codes[cdis[num_cdis]];
    num_cdis++;
  }

  if ( num_cdis == 0 ) {
    if ( gcmd_line.R && gcmd_line.debug  >= 2 ) {
      printf("\nencoded disjunction empty, bailing out");
      fflush(stdout);
    }
    return FALSE;
  }

  /* another pre-check: is any ini or contained in dis?
   *
   * NOTE: clauses are ordered by increasing length so
   * this way we find a smallest appropriate clause.
   */
  for ( i = 0; i < lr_num_clauses; i++ ) {
    /* for each ini or
     */
    for ( j = 0; j < lr_clause_length[i]; j++ ) {
      /* for each literal in it
       */
      for ( k = 0; k < num_cdis; k++ ) {
	if ( lr_clauses[i][j].literal == cdis[k] ) break;
      }
      if ( k == num_cdis ) {
	/* this one's not here -> abort
	 */
	break;
      }
    }
    if ( j == lr_clause_length[i] ) {
      /* yes, this one is contained in leafs
       */
      if ( gcmd_line.R && gcmd_line.debug  >= 2 ) {
	printf("\ninitial or is contained in leafs disjunction: ");
	print_r_clause( i );
	fflush(stdout);
      }
      return TRUE;
    }
  }




  /* with h 0, don't do SAT reasoning and simply assume that there is no
   * implication if none of the pre-checks holds.
   */
  if ( gcmd_line.heuristic == 0 ) {
    if ( gcmd_line.R && gcmd_line.debug  >= 2 ) {
      printf("\nno pre-check applies. bailing out due to h == 0.");
      fflush(stdout);
    }
    return FALSE;
  }



  /* nada yet --> with h 1, now for the real sat test.
   *
   * extend the fixed ini state encoding with singleton clauses containing
   * the negations of the elements of the disjunction.
   */
  if ( gcmd_line.R && gcmd_line.debug  >= 2 ) {
    printf("\nno pre-check applies. doing full sat test now.");
    fflush(stdout);
  }

  lr_num_decision_stack = 0;
  for ( i = 0; i < num_cdis; i++ ) {
    lr_decision_stack[lr_num_decision_stack++] = (-1) * cdis[i];
  }

  times( &end );
  TIME( geval_time );
  times( &start );
  gr_sat_calls++;
  sat = r_dp_CNF();
  times( &end );
  TIME( gr_sat_time );
  times( &start );
  if ( sat ) {
    if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
      printf("\nr sat! no implication");
      fflush(stdout);
    }
  } else {
    if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
      printf("\nr unsat! implication");
      fflush(stdout);
    }
    return TRUE;
  }

  return FALSE;

}



Bool disjunction_implied_by_S_formula( int *dis, int num_dis, Bool *is_dis )

{

  static int *cdis;
  static Bool fc = TRUE;

  int i, m;
  int num_cdis;
  Bool sat;

  if ( fc ) {
    cdis = ( int * ) calloc( gnum_ft_conn, sizeof( int ) );
    fc = FALSE;
  }


  /* first, see if two of the leafs are contradictory
   * --> disjunction trivially implied, step out
   */
  for ( i = 0; i < num_dis; i++ ) {
    if ( gft_conn[dis[i]].negation != -1 &&
	 is_dis[gft_conn[dis[i]].negation] ) {
      if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
	printf("\ncontradictory ft pair in backchain leafs disjunction: ");
	print_ft_name(dis[i]);
	print_ft_name(gft_conn[dis[i]].negation);
	fflush(stdout);
      }
      return TRUE;
    }
  }

  if ( lStime < 0 || lStime > MAX_PLAN_LENGTH ) {
    printf("\n\nsomething wrong with lStime!\n\n"); fflush(stdout);
    exit( 1 );
  }


  /* create array of codes of these facts; use implicit
   * negated facts, ie. make these codes for coded literals as in the 
   * CNF.
   */
  num_cdis = 0;
  for ( i = 0; i < num_dis; i++ ) {
    if ( gft_conn[dis[i]].CNF ) {
      m = 1;
      cdis[num_cdis] = dis[i];
    } else {
      m = -1;
      cdis[num_cdis] = gft_conn[dis[i]].negation;
    }
    if ( gcodes[lStime][cdis[num_cdis]] == -1 ) {
      /* special case: this is not encoded and therefore does not occur
       * in our initial state CNF. thus it can't be implied and we can
       * skip it from the disjunction; we just need to check if there are two
       * contradictory literals that are thrown away, in which case the implication
       * is true trivially; this has been done above already.
       */
      if ( gcmd_line.R && gcmd_line.debug  >= 2 ) {
	printf("\nun-encoded ft/0 pair in backchain leafs disjunction: ");
	print_ft_name(cdis[num_cdis]);
	fflush(stdout);
      }
      continue;
    }
    /* we have the code, the literal occurs. write it into the disj. code array.
     */
    cdis[num_cdis] = m * gcodes[lStime][cdis[num_cdis]];
    num_cdis++;
  }

  if ( num_cdis == 0 ) {
    if ( gcmd_line.R && gcmd_line.debug  >= 2 ) {
      printf("\nencoded disjunction empty, bailing out");
      fflush(stdout);
    }
    return FALSE;
  }

  /* no pre-check if any ini or is contained in dis:
   * this is bound to fail anyway.
   *
   * extend the S CNF encoding with singleton clauses containing
   * the negations of the elements of the disjunction.
   */
  if ( gcmd_line.R && gcmd_line.debug  >= 2 ) {
    printf("\ndoing full sat test for implication now.");
    fflush(stdout);
  }

  gnum_decision_stack = 0;
  for ( i = 0; i < num_cdis; i++ ) {
    gdecision_stack[gnum_decision_stack++] = (-1) * cdis[i];
  }

  times( &end );
  TIME( geval_time );
  times( &start );
  gr_sat_calls++;
  sat = dp_CNF();
  times( &end );
  TIME( gr_sat_time );
  times( &start );

  if ( sat ) {
    if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
      printf("\nr sat! no implication");
      fflush(stdout);
    }
  } else {
    if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
      printf("\nr unsat! implication");
      fflush(stdout);
    }
    return TRUE;
  }

  return FALSE;

}



void reset_fixpoint( int max )

{

  int i;

  for ( i = 0; i < lnum_F; i++ ) {
    gft_conn[lF[i]].level = INFINITY;
    gft_conn[lF[i]].in_F = FALSE;
  }

  for ( i = 0; i < lnum_E; i++ ) {
    gef_conn[lE[i]].level = INFINITY;
    gef_conn[lE[i]].in_E = FALSE;
  }

  for ( i = 0; i < lnum_ch_E; i++ ) {
    gef_conn[lch_E[i]].num_active_PCs = 0;
    gef_conn[lch_E[i]].ch = FALSE;
  }


  if ( gcmd_line.heuristic == 2 ) {
    /* undo the clauses and encoding infos
     */
    for ( i = gnum_fixed_clauses; i < gnum_clauses; i++ ) {
      gclause_length[i] = 0;
    }
    gnum_clauses = gnum_fixed_clauses;
    for ( i = gnum_fixed_c + 1; i <= gnum_c; i++ ) {
      gcodes[gct[i]][gcf[i]] = -1;
    }
    /* here, UNSET the extended lit-in-clause pointers!!
     */
    for ( i = 1; i <= gnum_fixed_c; i++ ) {
      gpos_c_in_clause_end[i] = gpos_c_in_clause_fixed[i];
      gneg_c_in_clause_end[i] = gneg_c_in_clause_fixed[i];
    }
    for ( ; i <= gnum_c; i++ ) {
      if ( gpos_c_in_clause_start[i] == NULL ||
	   gneg_c_in_clause_start[i] == NULL ) {
	printf("\nin rpg reset, var %d has no dummy nodes in mem list yet??\n\n", i);
	exit( 1 );
      }
      if ( gpos_c_in_clause_fixed[i] != gpos_c_in_clause_start[i]->next ||
	   gneg_c_in_clause_fixed[i] != gneg_c_in_clause_start[i]->next ) {
	printf("\nin rpg reset, for new dyn. code %d fixed != start->next??\n\n", i);
	exit( 1 );
      }
      gpos_c_in_clause_end[i] = gpos_c_in_clause_start[i]->next;
      gneg_c_in_clause_end[i] = gneg_c_in_clause_start[i]->next;
    }
  }

}



Bool all_goals_activated( int time ) 

{

  int i;

  for ( i = 0; i < lcurrent_goals.num_F; i++ ) {
    if ( !gft_conn[lcurrent_goals.F[i]].in_F ) {
      return FALSE;
    }
  }

  for ( i = 0; i < lcurrent_goals.num_F; i++ ) {
    if ( gft_conn[lcurrent_goals.F[i]].level == INFINITY ) {
      gft_conn[lcurrent_goals.F[i]].level = time;
    }
  }

  return TRUE;

}



void print_fixpoint_result( void )

{

  int time, i, j, Utime;
  Bool hit, hit_F, hit_E, hit_U;

  printf("\n\n----------------------------relaxed plan graph");

  Utime = 0;
  while ( Utime < lpath_U_length - 1 ) {
    printf("\n\nU-LEVEL %d (ie Utime %d):", Utime - lpath_U_length + 1, Utime );
    for ( i = 0; i < lnum_U[Utime]; i++ ) {
      printf("\n");
      print_ft_name( lU[Utime][i].ft );
      if ( lU[Utime][i].became_F ) printf(" became F");
      printf(", edges to: ");
      for ( j = 0; j < lU[Utime][i].num_in; j++ ) {
	print_ft_name( lU[Utime][i].in_edges[j]->ft );
	printf(" (by "); 
	if ( lU[Utime][i].in_efs[j] == -1 ) {
	  printf("NOOP ");
	} else {
	  if ( lU[Utime][i].in_efs[j] == -2 ) {
	    printf("OR constraint");
	  } else {
	    print_op_name( gef_conn[lU[Utime][i].in_efs[j]].op );
	  }
	}
	printf("), ");
      }
    }
    Utime++;
  }

  time = 0;
  while ( 1 ) {
    hit = FALSE;
    hit_F = FALSE;
    hit_U = FALSE;
    hit_E = FALSE;
    for ( i = 0; i < gnum_ft_conn; i++ ) {
      if ( gft_conn[i].level == time ) {
	hit = TRUE;
	hit_F = TRUE;
	break;
      }
    }
    for ( i = 0; i < gnum_ef_conn; i++ ) {
      if ( gef_conn[i].level == time ) {
	hit = TRUE;
	hit_E = TRUE;
	break;
      }
    }
    if ( lnum_U[Utime] ) {
      hit_U = TRUE;
      hit = TRUE;
    }

    if ( !hit ) {
      break;
    }
 
    printf("\n\nLEVEL %d:", time);
    if ( hit_F ) {
      printf("\n\nFACTS:");
      for ( i = 0; i < gnum_ft_conn; i++ ) {
	if ( gft_conn[i].level == time ) {
	  printf("\n");
	  print_ft_name( i );
	}
      }
    }
    if ( hit_U ) {
      printf("\n\nUNKNOWN FACTS (Utime %d):", Utime);
      for ( i = 0; i < lnum_U[Utime]; i++ ) {
	printf("\n");
	print_ft_name( lU[Utime][i].ft );
	if ( lU[Utime][i].became_F ) printf(" became F");
	printf(", edges to: ");
	for ( j = 0; j < lU[Utime][i].num_in; j++ ) {
	  print_ft_name( lU[Utime][i].in_edges[j]->ft );
	  printf(" (by "); 
	  if ( lU[Utime][i].in_efs[j] == -1 ) {
	    printf("NOOP ");
	  } else {
	    if ( lU[Utime][i].in_efs[j] == -2 ) {
	      printf("OR constraint");
	    } else {
	      print_op_name( gef_conn[lU[Utime][i].in_efs[j]].op );
	    }
	  }
	  printf("), ");
	}
      }
    }
    if ( hit_E ) {
      printf("\n\nEFS:");
      for ( i = 0; i < gnum_ef_conn; i++ ) {
	if ( gef_conn[i].level == time ) {
	  printf("\neffect %d to ", i);
	  print_op_name( gef_conn[i].op );
	}
      }
    }

    time++;
    Utime++;
  }
  fflush( stdout );

}
    



































/**************************************
 * FIRST RELAXED PLAN (1P) EXTRACTION *
 **************************************/









































int extract_1P( int max, Bool H_info )

{

  static Bool first_call = TRUE;
  int i, j, max_goal_level, time;

  if ( first_call ) {
    for ( i = 0; i < gnum_ft_conn; i++ ) {
      for ( j = 0; j < RELAXED_STEPS; j++ ) {
	gft_conn[i].is_goal_at[j] = FALSE;
      }
      gft_conn[i].is_true = INFINITY;
      gft_conn[i].ch = FALSE;
    }
    for ( i = 0; i < gnum_op_conn; i++ ) {
      for ( j = 0; j < RELAXED_STEPS; j++ ) {
	gop_conn[i].is_used_at[j] = FALSE;
      }
      gop_conn[i].is_used = FALSE;
    }
    lch_F = ( int * ) calloc( gnum_ft_conn, sizeof( int ) );
    lnum_ch_F = 0;
    lused_O = ( int * ) calloc( gnum_op_conn, sizeof( int ) );
    lnum_used_O = 0;
    first_call = FALSE;
  }

  reset_search_info();

  if ( gcmd_line.display_info == 123  || (gcmd_line.R && gcmd_line.debug) ) {
    printf("\n\n----------------------------------------relaxed plan");
    fflush(stdout);
  }

  /* < infty due to previous checks
   */ 
  max_goal_level = initialize_goals( max );
  /* DEBUGGING, REMOVE LATER
   */
  if ( max_goal_level == INFINITY ) {
    printf("\n\ntrying to find rplan for infty goals?\n\n");
    fflush(stdout);
    exit( 1 );
  }

  /* part of the H info --- the path selected ops at time 0, namely ---
   * we are collecting along the way so we already initialze the stuff here.
   */
  for ( i = 0; i < gnum_H; i++ ) {
    gop_conn[gH[i]].is_in_H = FALSE;
  }
  gnum_H = 0;

  lh = 0;
  for ( time = max_goal_level; time > 0; time-- ) {
    achieve_goals( time );
  }
  if ( H_info ) {
    collect_H_info();
  }

  return lh;

}



int initialize_goals( int max )

{

  static Bool first_call = TRUE;
  static int highest_seen;

  int i, max_goal_level, ft;

  if ( first_call ) {
    lgoals_at = ( int ** ) calloc( RELAXED_STEPS_DEFAULT, sizeof( int * ) );
    lnum_goals_at = ( int * ) calloc( RELAXED_STEPS_DEFAULT, sizeof( int ) );
    for ( i = 0; i < RELAXED_STEPS_DEFAULT; i++ ) {
      lgoals_at[i] = ( int * ) calloc( gnum_ft_conn, sizeof( int ) );
    }
    highest_seen = RELAXED_STEPS_DEFAULT;
    first_call = FALSE;
  }

  if ( max + 1 > highest_seen ) {
    for ( i = 0; i < highest_seen; i++ ) {
      free( lgoals_at[i] );
    }
    free( lgoals_at );
    free( lnum_goals_at );
    highest_seen = max + 1;
    lgoals_at = ( int ** ) calloc( highest_seen, sizeof( int * ) );
    lnum_goals_at = ( int * ) calloc( highest_seen, sizeof( int ) );
    for ( i = 0; i < highest_seen; i++ ) {
      lgoals_at[i] = ( int * ) calloc( gnum_ft_conn, sizeof( int ) );
    }
  }

  for ( i = 0; i < max + 1; i++ ) {
    lnum_goals_at[i] = 0;
  }

  max_goal_level = 0;
  for ( i = 0; i < lcurrent_goals.num_F; i++ ) {
    ft = lcurrent_goals.F[i];
    if ( gft_conn[ft].level == INFINITY ) {
      return INFINITY;
    }
    if ( gft_conn[ft].level > max_goal_level ) {
      max_goal_level = gft_conn[ft].level;
    }
    lgoals_at[gft_conn[ft].level][lnum_goals_at[gft_conn[ft].level]++] = ft;
    if ( gft_conn[ft].level >= RELAXED_STEPS ) {
      printf("\n\nRELAXED_STEPS too small due to ft->is_goal_at array, time %d\n\n",
	     gft_conn[ft].level);
      fflush(stdout);
      exit( 1 );
    }
    gft_conn[ft].is_goal_at[gft_conn[ft].level] = TRUE;
    if ( !gft_conn[ft].ch ) {
      lch_F[lnum_ch_F++] = ft;
      gft_conn[ft].ch = TRUE;
    }
  }

  return max_goal_level;

}



void achieve_goals( int time )

{

  int i, j, k, ft, min_p, min_e, ef, p, op, Utime;

  Utime =  time + lpath_U_length - 1;

  for ( i = 0; i < lnum_goals_at[time]; i++ ) {
    ft = lgoals_at[time][i];

    if ( gcmd_line.display_info == 123  || (gcmd_line.R && gcmd_line.debug) ) {
      printf("\nworking on goal ");
      print_ft_name( ft );
      printf(" at time %d", time);
      fflush(stdout);
    }

    if ( gft_conn[ft].is_true != INFINITY &&
	 gft_conn[ft].is_true <= time  ) {
      /* fact already true here.
       */
      if ( gcmd_line.display_info == 123  || (gcmd_line.R && gcmd_line.debug) ) {
	printf(" -- TRUE already!");
	fflush(stdout);
      }
      continue;
    }

    min_p = INFINITY;
    min_e = -1;
    for ( j = 0; j < gft_conn[ft].num_A; j++ ) {
      if ( gft_conn[ft].A_nondet[j] ) {
	/* no nondet achievers!
	 */
	continue;
      }
      ef = gft_conn[ft].A[j];
      if ( gef_conn[ef].level != time - 1 ) continue; 
      p = 0;
      for ( k = 0; k < gef_conn[ef].num_PC; k++ ) {
	p += gft_conn[gef_conn[ef].PC[k]].level;
      }
      if ( LESS( p, min_p ) ) {
	min_p = p;
	min_e = ef;
      }
    }

    /* has fact become true due to complementary unknown paths?
     * if yes, then select the ops on these paths!
     */
    if ( min_e == -1 ) {
      if ( gcmd_line.display_info == 123  || (gcmd_line.R && gcmd_line.debug) ) {
	printf("\nno real achiever. going to paths.");
	fflush(stdout);
      }
      for ( j = 0; j < lnum_U[Utime]; j++ ) {
	if ( lU[Utime][j].ft == ft ) break;
      }
      /* DEBUGGING; REMOVE LATER
       */
      if ( j == lnum_U[Utime] ) {
	printf("\n\nno ef in for subg ft but not in U either at %d U %d\n\n", time, Utime);
	fflush(stdout);
	exit( 1 );
      }
      if (!lU[Utime][j].became_F ) {
	printf("\n\nno ef in for subg ft but not became F either at %d U %d\n\n", time, Utime);
	fflush(stdout);
	exit( 1 );
      }
      select_implied_incoming_paths( time, &(lU[Utime][j]) );

      continue;
    } /* min_e == -1 */

    if ( gcmd_line.display_info == 123  || (gcmd_line.R && gcmd_line.debug) ) {
      printf("\nnormal. going to efs.");
      fflush(stdout);
    }
    /* else, proceed as normal.
     */
    if ( time - 1 >= RELAXED_STEPS ) {
      printf("\n\nRELAXED_STEPS too small due to op->is_used_at array, time %d\n\n",
	     time - 1);
      fflush(stdout);
      exit( 1 );
    }
    ef = min_e;
    op = gef_conn[ef].op;
    if ( !gop_conn[op].is_used_at[time-1] ) {
      if ( !gop_conn[op].is_used ) {
	lused_O[lnum_used_O++] = op;
	gop_conn[op].is_used = TRUE;
      }
      gop_conn[op].is_used_at[time-1] = TRUE;
      lh++;
      if ( gcmd_line.display_info == 123  || (gcmd_line.R && gcmd_line.debug) ) {
	printf("\nnormal selecting at step %3d: ", time-1);
	print_op_name( op );
	fflush(stdout);
      }
    } else {
      if ( gcmd_line.display_info == 123  || (gcmd_line.R && gcmd_line.debug) ) {
	printf("\nop used already at this time step. do not select it the 2nd time.");
	fflush(stdout);
      }
    }

    introduce_ef_PC_and_A( time, ef );
  } /* for all goals at time */

}



void introduce_ef_PC_and_A( int time, int ef )

{

  int j, k, ft;

  for ( j = 0; j < gef_conn[ef].num_PC; j++ ) {
    ft = gef_conn[ef].PC[j];
    if ( gft_conn[ft].level >= RELAXED_STEPS ) {
      printf("\n\nRELAXED_STEPS too small due to ft->is_goal_at array, time %d\n\n",
	     gft_conn[ft].level);
      fflush(stdout);
      exit( 1 );
    }
    if ( gft_conn[ft].is_true != INFINITY &&
	 gft_conn[ft].is_true <= time - 1 ) {
      /* already true here.
       */
      continue;
    }
    if ( gft_conn[ft].is_goal_at[gft_conn[ft].level] ) {
      /* this fact already is a goal at the point where we want to make it a goal.
       */
      continue;
    }
    lgoals_at[gft_conn[ft].level][lnum_goals_at[gft_conn[ft].level]++] = ft;
    gft_conn[ft].is_goal_at[gft_conn[ft].level] = TRUE;
    if ( !gft_conn[ft].ch ) {
      lch_F[lnum_ch_F++] = ft;
      gft_conn[ft].ch = TRUE;
    }
  }
  
  for ( j = 0; j < gef_conn[ef].num_A; j++ ) {
    ft = gef_conn[ef].A[j];
    if ( gft_conn[ft].is_true == INFINITY ||
	 gft_conn[ft].is_true > time ) {
      gft_conn[ft].is_true = time;
    }
    if ( !gft_conn[ft].ch ) {
      lch_F[lnum_ch_F++] = ft;
      gft_conn[ft].ch = TRUE;
    }
  }
  for ( j = 0; j < gef_conn[ef].num_I; j++ ) {
    for ( k = 0; k < gef_conn[gef_conn[ef].I[j]].num_A; k++ ) {
      ft = gef_conn[gef_conn[ef].I[j]].A[k];
      if ( gft_conn[ft].is_true == INFINITY ||
	   gft_conn[ft].is_true > time - 1 ) {
	gft_conn[ft].is_true = time - 1;
      }
      if ( !gft_conn[ft].ch ) {
	lch_F[lnum_ch_F++] = ft;
	gft_conn[ft].ch = TRUE;
      }
    }
  }

}



void pathselect( int Utime, int ef )

{

  int op, time, i;

  time = Utime - lpath_U_length + 1;

  if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
    printf("\npath select call Utime %d (ie time %d) ef %d", Utime, time, ef);
    fflush(stdout);
  }
  if ( ef == -1 ) {
    if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
      printf("\nskipping path select of noop constr.");
      fflush(stdout);
    }
    return;
  }
  if ( ef == -2 ) {
    if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
      printf("\nskipping path select of OR constr.");
      fflush(stdout);
    }
    return;
  }
      
  if ( time <= 0 ) {
    if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
      printf("\nskipping path select at time %d", time - 1);
      print_op_name( gef_conn[ef].op );
      fflush(stdout);
    }
    return;
  }
  op = gef_conn[ef].op;
  if ( !gop_conn[op].is_used_at[time-1] ) {
    if ( !gop_conn[op].is_used ) {
      lused_O[lnum_used_O++] = op;
      gop_conn[op].is_used = TRUE;
    }
    gop_conn[op].is_used_at[time-1] = TRUE;
    lh++;
    if ( gcmd_line.display_info == 123  || (gcmd_line.R && gcmd_line.debug) ) {
      printf("\npath selecting at step %3d: ", time-1);
      print_op_name( op );
      fflush(stdout);
    }
    /* path ops are helpful -- if they are applicable!!
     */
    if ( !gop_conn[op].is_in_H ) {
      for ( i = 0; i < gop_conn[op].num_P; i++ ) {
	if ( gft_conn[gop_conn[op].P[i]].level != 0 ) break;
      }
      if ( i == gop_conn[op].num_P ) {
	gop_conn[op].is_in_H = TRUE;
	gH[gnum_H++] = op;
	if ( gcmd_line.display_info == 123  || (gcmd_line.R && gcmd_line.debug) ) {
	  printf("\nhelpful action from path time %d: ", time-1);
	  print_op_name( op );
	  fflush(stdout);
	}
      }
    }
  }

  introduce_path_ef_PC_and_A( time, ef );

}



void introduce_path_ef_PC_and_A( int time, int ef )

{

  int i, j, ft, Utime;

  Utime =  time + lpath_U_length - 1;

  for ( j = 0; j < gef_conn[ef].num_PC; j++ ) {
    ft = gef_conn[ef].PC[j];
    if ( gft_conn[ft].is_true != INFINITY &&
	 gft_conn[ft].is_true <= time - 1 ) {
      /* already true here.
       */
      continue;
    }
    if ( gft_conn[ft].level != INFINITY &&
	 gft_conn[ft].level < time ) {
      /* only enforce pathconds to be true if they can be enforced,
       * ie. if their level is below the current goal level.
       *
       * THIS MAINTAINS CORRECTNESS OF RPLAN?
       */
      if ( gcmd_line.display_info == 123  || (gcmd_line.R && gcmd_line.debug >= 2) ) {
	printf("\nintroducing path goal ");
	print_ft_name( ft );
	printf(" at time %d", gft_conn[ft].level);
	fflush(stdout);
      }
      if ( gft_conn[ft].level >= RELAXED_STEPS ) {
	printf("\n\nRELAXED_STEPS too small due to ft->is_goal_at array, time %d\n\n",
	       gft_conn[ft].level);
	fflush(stdout);
	exit( 1 );
      }
      if ( gft_conn[ft].is_goal_at[gft_conn[ft].level] ) {
	/* this fact already is a goal at the point where we want to make it a goal.
	 */
	continue;
      }
      lgoals_at[gft_conn[ft].level][lnum_goals_at[gft_conn[ft].level]++] = ft;
      gft_conn[ft].is_goal_at[gft_conn[ft].level] = TRUE;
      if ( !gft_conn[ft].ch ) {
	lch_F[lnum_ch_F++] = ft;
	gft_conn[ft].ch = TRUE;
      }
    } else {/* fact is not known at the effcond level */
      /* DEBUGGING! REMOVE THAT LATER
       */
      for ( i = 0; i < lnum_U[Utime-1]; i++ ) {
	if ( lU[Utime-1][i].ft == ft ) break;
      }
      if ( i == lnum_U[Utime-1] ) {
	printf("\n\nprec ft for path ef neither in nor U at time %d -1, U %d??\n\n", 
	       time, Utime);
	fflush(stdout);
	exit( 1 );
      }
    }
  }/* for all eff pre+conds */

  /* actually, this effect will only in part of our paths be executed (?!)
   * so we can and do not mark its add effects as true!
   */

}



void collect_H_info( void )

{

  static Bool first_call = TRUE;
  static int *H, num_H, *D;
  int i, j, k, ft, ef, op, d;

  if ( first_call ) {
    H = ( int * ) calloc( gnum_op_conn, sizeof( int ) );
    D = ( int * ) calloc( gnum_op_conn, sizeof( int ) );
    num_H = 0;
    first_call = FALSE;
  }

  /* gnum_H and the is in H info have been initialized at 
   * rplan start already!
   */

  num_H = 0;
  for ( i = 0; i < lnum_goals_at[1]; i++ ) {
    ft = lgoals_at[1][i];

    for ( j = 0; j < gft_conn[ft].num_A; j++ ) {
      if ( gft_conn[ft].A_nondet[j] ) {
	/* no nondet achievers!
	 */
	continue;
      }
      ef = gft_conn[ft].A[j];
      if ( gef_conn[ef].level != 0 ) {
	continue;
      }
      op = gef_conn[ef].op;

      if ( gop_conn[op].is_in_H ) {
	continue;
      }
      gop_conn[op].is_in_H = TRUE;
      H[num_H++] = op;
    }
  }

  /* H collected; now order it; just a stupid ad-hoc optimisation
   * that worked well in (classical) Logistics etc.
   *
   *       count number subgoal at 0 or 1 facts that
   *       op deletes (with level 0 effects). order less deletes
   *       before more deletes.
   *       start from back of H, to prefer down under
   *       goals to upper goals.
   */
  for ( i = num_H - 1; i > -1; i-- ) {
    d = 0;
    for ( j = 0; j < gop_conn[H[i]].num_E; j++ ) {
      ef = gop_conn[H[i]].E[j];
      if ( gef_conn[ef].level != 0 ) continue;
      for ( k = 0; k < gef_conn[ef].num_D; k++ ) {
	if ( gft_conn[gef_conn[ef].D[k]].is_goal_at[0] ||
	     gft_conn[gef_conn[ef].D[k]].is_goal_at[1] ) d++;
      }
    }
    for ( j = 0; j < gnum_H; j++ ) {
      if ( D[j] > d ) break;
    }
    for ( k = gnum_H; k > j; k-- ) {
      gH[k] = gH[k-1];
      D[k] = D[k-1];
    }
    gH[j] = H[i];
    D[j] = d;
    gnum_H++;
  }
  if ( gcmd_line.display_info == 124 || (gcmd_line.R && gcmd_line.debug) ) {
    printf("\n--------------------------------- collected H: ");
    for ( i = 0; i < gnum_H; i++ ) {
      print_op_name( gH[i] );
      printf("\n              ");
    }
    fflush(stdout);
  }

}



void reset_search_info( void )

{

  int i, j;

  for ( i = 0; i < lnum_ch_F; i++ ) {
    for ( j = 0; j < RELAXED_STEPS; j++ ) {
      gft_conn[lch_F[i]].is_goal_at[j] = FALSE;
    }
    gft_conn[lch_F[i]].is_true = INFINITY;
    gft_conn[lch_F[i]].ch = FALSE;
  }
  lnum_ch_F = 0;

  for ( i = 0; i < lnum_used_O; i++ ) {
    for ( j = 0; j < RELAXED_STEPS; j++ ) {
      gop_conn[lused_O[i]].is_used_at[j] = FALSE;
    }
    gop_conn[lused_O[i]].is_used = FALSE;
  }
  lnum_used_O = 0;
  
}



void select_implied_incoming_paths( int time, UftNode *n ) 

{

  static Bool fc = TRUE;
  static int *leafs, *min_leafs;
  static Bool *is_leaf;
  static UftNode_pointer *p;
  static int *pfather, *pef, *pt, *leafp;

  int num_p, prev_p, now_p, num_leafs, num_min_leafs;
  int i, j, k, t, ft, ft_;
  int Utime;

  if ( fc ) {
    leafs = ( int * ) calloc( gnum_ft_conn, sizeof( int ) );
    min_leafs = ( int * ) calloc( gnum_ft_conn, sizeof( int ) );
    is_leaf = ( Bool * ) calloc( gnum_ft_conn, sizeof( Bool ) );
    p = ( UftNode_pointer * ) calloc( lMAX_PATHNODES, sizeof( UftNode_pointer ) );
    pfather = ( int * ) calloc( lMAX_PATHNODES, sizeof( int ) );
    pef = ( int * ) calloc( lMAX_PATHNODES, sizeof( int ) );
    pt = ( int * ) calloc( lMAX_PATHNODES, sizeof( int ) );
    leafp = ( int * ) calloc( lMAX_PATHNODES, sizeof( int ) );

    for ( i = 0; i < gnum_ft_conn; i++ ) {
      is_leaf[i] = FALSE;
    }

    fc = FALSE;
  }

  Utime =  time + lpath_U_length - 1;

  ft = n->ft;
  p[0] = n;
  pfather[0] = -1;
  pef[0] = -1;
  pt[0] = Utime;
  now_p = 0;
  num_p = 1;
  num_leafs = 0;
  if ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
    printf("\nstarting "); print_ft_name( ft );
    fflush(stdout);
  }
  for ( t = Utime; t > 0; t-- ) {
    /* include all edges from t to t-1
     *
     * Mar04: NOTE -- HERE, WE GOT NO EDGES AT TIME 0 SO WE CAN SIMPLIFY THE
     * COMPUTATION
     */
    prev_p = now_p;
    now_p = num_p;
    for ( i = prev_p; i < now_p; i++ ) {
      /* loop over all incoming edges of the node.
       */
      if ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
	printf("\nrp i at t %d: ", t); print_ft_name( p[i]->ft );
	fflush(stdout);
      }
      /* NOTE: this is "NOOPs-first", as the NOOPs are the first edges inserted
       *       into this list by the functions that build the implication
       *       graph!
       *
       *       NOOPs-first is good: it makes the rplan extraction try to apply
       *       real actions as far down in the RPG/the impli graph as possible,
       *       in particular if actions already applied on the path to s can help,
       *       then they will be used!
       */
      for ( j = 0; j < p[i]->num_in; j++ ) {
	/* see if we got this node already: avoid duplicates!
	 */
	for ( k = 0; k < num_p; k++ ) {
	  if ( p[k] == p[i]->in_edges[j] ) break;
	}
	if ( k < num_p ) {
	  /* had this. skip it.
	   */
	  continue;
	}
	if ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
	  printf("\nrp new p: "); print_ft_name( p[i]->in_edges[j]->ft );
	  printf(" (by "); 
	  if ( p[i]->in_efs[j] == -1 ) {
	    printf("NOOP ");
	  } else {
	    if ( p[i]->in_efs[j] == -2 ) {
	      printf("OR constraint??\n\n");
	      fflush(stdout);
	      exit( 1 );
	    } else {
	      print_op_name( gef_conn[p[i]->in_efs[j]].op );
	    }
	  }
	  printf(")");
	  fflush(stdout);
	}
	if ( num_p >= lMAX_PATHNODES ) {
	  printf("\n\nrp 3 -- too many nodes on paths! increase lMAX_PATHNODES (now %d)\n\n",
		 lMAX_PATHNODES);
	  fflush(stdout);
	  exit( 1 );
	}
	p[num_p] = p[i]->in_edges[j];
	pfather[num_p] = i;
	pt[num_p] = pt[i] - 1;
	pef[num_p++] = p[i]->in_efs[j];
	ft_ = p[i]->in_edges[j]->ft;
	if ( t == 1 && !is_leaf[ft_] ) {
	  is_leaf[ft_] = TRUE;
	  leafs[num_leafs] = ft_;
	  leafp[num_leafs++] = num_p-1;
	  if ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
	    printf("\nrp set is-l "); print_ft_name( ft_ );
	    fflush(stdout);
	  }
	}
      }
    }
  }

  if ( gcmd_line.R && gcmd_line.debug  >= 2 ) {
    printf("\nrp fact "); print_ft_name( ft );
    printf(" has leafs:");
    for ( i = 0; i < num_leafs; i++ ) {
      printf(" ");print_ft_name( leafs[i] );
    }
    printf("\nrp now checking for implication by initial/state formula.");
    fflush(stdout);
  }

  if ( gcmd_line.heuristic == 0 ||
       gcmd_line.heuristic == 1 ) {
    get_minimal_disjunction_implied_by_initial_formula( leafs, num_leafs, is_leaf,
							&(min_leafs), &(num_min_leafs), ft );
  }
  if ( gcmd_line.heuristic == 2 ) {
    get_minimal_disjunction_implied_by_state_formula( leafs, num_leafs, is_leaf,
						      &(min_leafs), &(num_min_leafs), ft );
  }

  for ( i = 0; i < num_leafs; i++ ) {
    is_leaf[leafs[i]] = FALSE;
  }



  /* select the paths to the min_leafs
   */
  if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
    printf("\nrp selecting minimal implied disj.: ");
    for ( i = 0; i < num_min_leafs; i++ ) {
      print_ft_name( leafs[min_leafs[i]] ); printf(" ");
    }
    fflush(stdout);
  }
  for ( i = 0; i < num_min_leafs; i++ ) {
    if ( leafs[min_leafs[i]] == ft ) {
      /* to ft, we have a path of noops; but this path might not have been
       * selected by backwards reasoning; we simply skip ft, to the same effect
       * as if we'd selected the noop edges leading to it.
       */
      continue;
    }
    for ( j = leafp[min_leafs[i]]; pfather[j] != -1; j = pfather[j] ) {
      pathselect( pt[j] + 1, pef[j] );
    }
  }


}



/* **mindis and *num_min_dis are the outputs of the fn -- an array of ints and the nr
 * of its elements, which at fn end contains a minimal implied subdisjunction in the
 * form of its indices in the dis array.
 */
void get_minimal_disjunction_implied_by_initial_formula( int *dis, int num_dis, Bool *is_dis,
							 int **min_dis, int *num_min_dis, int ft )

{

  static int *cdis, *cdis_to_dis;
  /* cdis can become a subset of dis; we need to remember, in cdis_to_dis,
   * what this subset is.
   */
  static Bool fc = TRUE;

  int i, j, k, m;
  int num_cdis;
  Bool sat;
  int curr_test_index;

  if ( fc ) {
    cdis = ( int * ) calloc( gnum_ft_conn, sizeof( int ) );
    cdis_to_dis = ( int * ) calloc( gnum_ft_conn, sizeof( int ) );
    fc = FALSE;
  }


  /* first, see if two of the leafs are contradictory
   */
  for ( i = 0; i < num_dis; i++ ) {
    if ( gft_conn[dis[i]].negation != -1 &&
	 is_dis[gft_conn[dis[i]].negation] ) {
      if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
	printf("\nrp contradictory ft pair in backchain leafs disjunction: ");
	print_ft_name(dis[i]);
	print_ft_name(gft_conn[dis[i]].negation);
	fflush(stdout);
      }
      (*min_dis)[0] = i;
      for ( k = 0; k < num_dis; k++ ) {
	if ( gft_conn[dis[i]].negation == dis[k] ) break;
      }
      if ( k == num_dis || k == i ) {
	printf("\nnegated fact not where it ought to be\n\n"); fflush(stdout);
	exit( 1 );
      }
      (*min_dis)[1] = k;
      (*num_min_dis) = 2;
      return;
    }
  }


  /* create array of codes of these facts; use implicit
   * negated facts, ie. make these codes for coded literals as in the 
   * CNF.
   */
  num_cdis = 0;
  for ( i = 0; i < num_dis; i++ ) {
    if ( gft_conn[dis[i]].CNF ) {
      m = 1;
      cdis[num_cdis] = dis[i];
    } else {
      m = -1;
      cdis[num_cdis] = gft_conn[dis[i]].negation;
    }
    if ( lr_codes[cdis[num_cdis]] == -1 ) {
      /* special case: this is not encoded and therefore does not occur
       * in our initial state CNF. thus it can't be implied and we can
       * skip it from the disjunction; we just need to check if there are two
       * contradictory literals that are thrown away, in which case the implication
       * is true trivially; this has been done above already.
       */
      if ( gcmd_line.R && gcmd_line.debug  >= 2 ) {
	printf("\nrp un-encoded ft/0 pair in backchain leafs disjunction: ");
	print_ft_name(cdis[num_cdis]);
	fflush(stdout);
      }
      continue;
    }
    /* we have the code, the literal occurs. write it into the disj. code array.
     */
    cdis[num_cdis] = m * lr_codes[cdis[num_cdis]];
    cdis_to_dis[num_cdis] = i;
    num_cdis++;
  }

  if ( num_cdis == 0 ) {
    printf("\nrp encoded disjunction empty in rp extraction??\n\n");
    fflush(stdout);
    exit( 1 );
  }
  if ( gcmd_line.R && gcmd_line.debug  >= 2 ) {
    printf("\nrp encoded disjunction: ");
    for ( i = 0; i < num_cdis; i++ ) {
      if ( cdis[i] < 0 ) {
	printf("-"); print_ft_name( lr_cf[(-1)*cdis[i]] ); 
      } else {
	print_ft_name( lr_cf[cdis[i]] ); 
      }
    }
    fflush(stdout);
  }

  /* another pre-check: is any ini or contained in dis?
   *
   * NOTE: the clauses are sorted by increasing length, see start of this file.
   *       so we will find as short a clause as possible (if we find one).
   */
  for ( i = 0; i < lr_num_clauses; i++ ) {
    /* for each ini or
     */
    for ( j = 0; j < lr_clause_length[i]; j++ ) {
      /* for each literal in it
       */
      for ( k = 0; k < num_cdis; k++ ) {
	if ( lr_clauses[i][j].literal == cdis[k] ) break;
      }
      if ( k == num_cdis ) {
	/* this one's not here -> abort
	 */
	break;
      }
    }
    if ( j == lr_clause_length[i] ) {
      /* yes, this one is contained in leafs
       */
      if ( gcmd_line.R && gcmd_line.debug  >= 2 ) {
	printf("\nrp initial or is contained in leafs disjunction: ");
	print_r_clause( i );
	fflush(stdout);
      }
      /* take the clause as the minimal implied disjunction; ie, collect the
       * appropriate indices in the dis array.
       */
      (*num_min_dis) = 0; 
      for ( j = 0; j < lr_clause_length[i]; j++ ) {
	for ( k = 0; k < num_cdis; k++ ) {
	  if ( lr_clauses[i][j].literal == cdis[k] ) break;
	}
	(*min_dis)[(*num_min_dis)++] = cdis_to_dis[k];
      }
      return;
    }
  } /* all fixed clauses, ie ini ors */


  
  /* if h == 0, and we get here, then something is wrong!!
   */
  if ( gcmd_line.heuristic == 0 ) {
    printf("\n\nno pre-check applies in h==0 - inferred ft leafs implication check?!\n\n");
    fflush(stdout);
    exit(1);
  }


  /* nada yet --> now do the real sat test, ie skip variables until
   * a minimal set remains.
   *
   * obviously (???) it is hard to find a smallest implied sub-disjunction.
   * we therefore go with a greedy minimisation, removing vars in some given 
   * fixed order. yet no heuristics at all on the order, we just proceed 
   * arbitrarily, ie from front to back in our array.
   *
   * some debugging stuff first...
   */
  if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
    printf("\nrp no pre-check applies. doing full sat minimisation now.");
    fflush(stdout);
  }
  if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
    printf("\ndoing debugging sat test");
    lr_num_decision_stack = 0;
    for ( i = 0; i < num_cdis; i++ ) {
      lr_decision_stack[lr_num_decision_stack++] = (-1) * cdis[i];
    }
    print_r_clauses();
    times( &end );
    TIME( geval_time );
    sat = r_dp_CNF();
    if ( sat ) {
      printf("\ndisj in full sat minimisation is not implied??\n\n");
      fflush(stdout);
      exit( 1 );
    }
  }


  /* and now for the actual code...
   */
  if ( num_cdis > 1 ) {
    /* else, nothing to minimise
     */
    curr_test_index = 0;
    while ( curr_test_index < num_cdis ) {
      if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
	printf("\nr now testing ");
	if ( cdis[curr_test_index] < 0 ) {
	  printf("-"); print_ft_name( gcf[(-1)*cdis[curr_test_index]] ); 
	  printf("(%d)", lStime);
	} else {
	  print_ft_name( gcf[cdis[curr_test_index]] ); 
	  printf("(%d)", lStime);
	}
	fflush(stdout);
      }
      /* for the fact ft in question itself, we will select a noop path
       * anyway, see above, so we want to keep it in.
       */
      if ( cdis[curr_test_index] < 0 ) {
	if ( lr_cf[(-1)*cdis[curr_test_index]] == ft ) {
	  curr_test_index++;
	  if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
	    printf(" --- target, take NOOPs");
	    fflush(stdout);
	  }
	  continue;
	} 
      } else {
	if ( lr_cf[cdis[curr_test_index]] == ft ) {
	  curr_test_index++;
	  if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
	    printf(" --- target, take NOOPs");
	    fflush(stdout);
	  }
	  continue;
	} 
      }
      lr_num_decision_stack = 0;
      for ( i = 0; i < num_cdis; i++ ) {
	if ( i == curr_test_index ) {
	  /* skip this literal
	   */
	  continue;
	}
	lr_decision_stack[lr_num_decision_stack++] = (-1) * cdis[i];
      }
      /* test if this is still implied
       */
      times( &end );
      TIME( geval_time );
      times( &start );
      grp_sat_calls++;
      sat = r_dp_CNF();
      times( &end );
      TIME( grp_sat_time );
      times( &start );
      if ( sat ) {
	if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
	  printf(" rp sat! no implication, keeping literal");
	  fflush(stdout);
	}
	curr_test_index++;
      } else {
	if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
	  printf(" rp unsat! implication, removing literal");
	  fflush(stdout);
	}
	/* statistics
	 */
	gremoved_lits++;
	for ( i = curr_test_index; i < num_cdis - 1; i++ ) {
	  cdis[i] = cdis[i+1];
	  cdis_to_dis[i] = cdis_to_dis[i+1];/* keep arrays synchronised */
	}
	num_cdis--;
      }
    } /* while curr test index < num cdis */
  } /* if num cdis > 1 */


  /* now cdis_to_dis contains exactly the minimal implied disj.
   * that we want to have. copy over.
   */
  for ( i = 0; i < num_cdis; i++ ) {
    (*min_dis)[i] = cdis_to_dis[i];
  }
  (*num_min_dis) = num_cdis;

}



/* as above, but with S formula.
 */
void get_minimal_disjunction_implied_by_state_formula( int *dis, int num_dis, Bool *is_dis,
						       int **min_dis, int *num_min_dis, int ft )

{

  static int *cdis, *cdis_to_dis;
  /* cdis can become a subset of dis; we need to remember, in cdis_to_dis,
   * what this subset is.
   */
  static Bool fc = TRUE;

  int i, k, m;
  int num_cdis;
  Bool sat;
  int curr_test_index;

  if ( fc ) {
    cdis = ( int * ) calloc( gnum_ft_conn, sizeof( int ) );
    cdis_to_dis = ( int * ) calloc( gnum_ft_conn, sizeof( int ) );
    fc = FALSE;
  }


  /* first, see if two of the leafs are contradictory
   */
  for ( i = 0; i < num_dis; i++ ) {
    if ( gft_conn[dis[i]].negation != -1 &&
	 is_dis[gft_conn[dis[i]].negation] ) {
      if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
	printf("\nrp contradictory ft pair in backchain leafs disjunction: ");
	print_ft_name(dis[i]);
	print_ft_name(gft_conn[dis[i]].negation);
	fflush(stdout);
      }
      (*min_dis)[0] = i;
      for ( k = 0; k < num_dis; k++ ) {
	if ( gft_conn[dis[i]].negation == dis[k] ) break;
      }
      if ( k == num_dis || k == i ) {
	printf("\nnegated fact not where it ought to be\n\n"); fflush(stdout);
	exit( 1 );
      }
      (*min_dis)[1] = k;
      (*num_min_dis) = 2;
      return;
    }
  }



  if ( lStime < 0 || lStime > MAX_PLAN_LENGTH ) {
    printf("\n\nsomething wrong with lStime!\n\n"); fflush(stdout);
    exit( 1 );
  }


  /* create array of codes of these facts; use implicit
   * negated facts, ie. make these codes for coded literals as in the 
   * CNF.
   */
  num_cdis = 0;
  for ( i = 0; i < num_dis; i++ ) {
    if ( gft_conn[dis[i]].CNF ) {
      m = 1;
      cdis[num_cdis] = dis[i];
    } else {
      m = -1;
      cdis[num_cdis] = gft_conn[dis[i]].negation;
    }
    if ( gcodes[lStime][cdis[num_cdis]] == -1 ) {
      /* special case: this is not encoded and therefore does not occur
       * in our initial state CNF. thus it can't be implied and we can
       * skip it from the disjunction; we just need to check if there are two
       * contradictory literals that are thrown away, in which case the implication
       * is true trivially; this has been done above already.
       */
      if ( gcmd_line.R && gcmd_line.debug  >= 2 ) {
	printf("\nun-encoded ft/0 pair in backchain leafs disjunction: ");
	print_ft_name(cdis[num_cdis]);
	fflush(stdout);
      }
      continue;
    }
    /* we have the code, the literal occurs. write it into the disj. code array.
     */
    cdis[num_cdis] = m * gcodes[lStime][cdis[num_cdis]];
    cdis_to_dis[num_cdis] = i;
    num_cdis++;
  }

  if ( num_cdis == 0 ) {
    printf("\nrp encoded disjunction empty in rp extraction??\n\n");
    fflush(stdout);
    exit( 1 );
  }

  if ( gcmd_line.R && gcmd_line.debug  >= 2 ) {
    printf("\nrp encoded disjunction: ");
    for ( i = 0; i < num_cdis; i++ ) {
      if ( cdis[i] < 0 ) {
	printf("-"); print_ft_name( gcf[(-1)*cdis[i]] ); 
	printf("(%d)", lStime);
      } else {
	print_ft_name( gcf[cdis[i]] ); 
	printf("(%d)", lStime);
      }
    }
    fflush(stdout);
  }

  /* no pre-check if any ini or is contained in dis:
   * this is bound to fail anyway.
   *
   * extend the S CNF encoding with singleton clauses containing
   * the negations of the elements of the disjunction.
   */
  if ( gcmd_line.R && gcmd_line.debug  >= 2 ) {
    printf("\ndoing full sat test for implication now.");
    fflush(stdout);
  }

  if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
    printf("\ndoing debugging sat test");
    gnum_decision_stack = 0;
    for ( i = 0; i < num_cdis; i++ ) {
      gdecision_stack[gnum_decision_stack++] = (-1) * cdis[i];
    }
    sat = dp_CNF();
    if ( sat ) {
      printf("\ndisj in full sat minimisation is not implied??\n\n");
      fflush(stdout);
      exit( 1 );
    } else {
      printf(" --- ok.");
    }
  }


  /* and now for the actual code...
   */
  if ( num_cdis > 1 ) {
    /* else, nothing to minimise
     */
    curr_test_index = 0;
    while ( curr_test_index < num_cdis ) {
      if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
	printf("\nr now testing ");
	if ( cdis[curr_test_index] < 0 ) {
	  printf("-"); print_ft_name( gcf[(-1)*cdis[curr_test_index]] ); 
	  printf("(%d)", lStime);
	} else {
	  print_ft_name( gcf[cdis[curr_test_index]] ); 
	  printf("(%d)", lStime);
	}
	fflush(stdout);
      }
      /* for the fact ft in question itself, we will select a noop path
       * anyway, see above, so we want to keep it in.
       */
      if ( cdis[curr_test_index] < 0 ) {
	if ( gcf[(-1)*cdis[curr_test_index]] == ft ) {
	  curr_test_index++;
	  if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
	    printf(" --- target, take NOOPs");
	    fflush(stdout);
	  }
	  continue;
	} 
      } else {
	if ( gcf[cdis[curr_test_index]] == ft ) {
	  curr_test_index++;
	  if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
	    printf(" --- target, take NOOPs");
	    fflush(stdout);
	  }
	  continue;
	} 
      }
      gnum_decision_stack = 0;
      for ( i = 0; i < num_cdis; i++ ) {
	if ( i == curr_test_index ) {
	  /* skip this literal
	   */
	  continue;
	}
	gdecision_stack[gnum_decision_stack++] = (-1) * cdis[i];
      }
      /* test if this is still implied
       */
      times( &end );
      TIME( geval_time );
      times( &start );
      grp_sat_calls++;
      sat = dp_CNF();
      times( &end );
      TIME( grp_sat_time );
      times( &start );
      if ( sat ) {
	if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
	  printf(" rp sat! no implication, keeping literal");
	  fflush(stdout);
	}
	curr_test_index++;
      } else {
	if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
	  printf(" rp unsat! implication, removing literal");
	  fflush(stdout);
	}
	/* statistics
	 */
	gremoved_lits++;
	for ( i = curr_test_index; i < num_cdis - 1; i++ ) {
	  cdis[i] = cdis[i+1];
	  cdis_to_dis[i] = cdis_to_dis[i+1];/* keep arrays synchronised */
	}
	num_cdis--;
      }
    } /* while curr test index < num cdis */
  } /* if num cdis > 1 */
    

  /* now cdis_to_dis contains exactly the minimal implied disj.
   * that we want to have. copy over.
   */
  for ( i = 0; i < num_cdis; i++ ) {
    (*min_dis)[i] = cdis_to_dis[i];
  }
  (*num_min_dis) = num_cdis;

}





























/**************************************************************
 * SAT RELATED STUFF
 **************************************************************/















































/* top level control fn for (extremely) naive DP implementation...
 */

Bool r_dp_CNF( void )

{

  int i, j, v, sign, c, sum;
  Bool sat;
  MemberList *i_ml, *j_ml;

  gdp_calls++;

  if ( lr_num_decision_stack == 0 ) {
    printf("\nr no decision stack at entering DP??\n\n"); 
    exit( 1 );
  }

  if ( gcmd_line.debug ) {
    for ( i = 0; i < gmax_literals + 1; i++ ) {
      for ( j = lr_num_clauses - 1; j >= 0; j-- ) {
	if ( lr_clause_length[j] == i ) gsum_k_clauses[i]++;
	if ( lr_clause_length[j] > gmax_literals ) {
	  printf("\n\nr ???\n\n");
	}
      }
    }
    gsum_clauses += lr_num_clauses;
  }

  if ( gcmd_line.debug ) {
    sum = -1;
    for ( i = lr_num_clauses - 1; i >= 0; i-- ) {
      for ( j = 0; j < lr_clause_length[i]; j++ ) {
	v = (lr_clauses[i][j].literal > 0) ? 
	  lr_clauses[i][j].literal : (-1) * lr_clauses[i][j].literal;
	if ( sum == -1 || v > sum ) {
	  sum = v;
	}
      }
    }
    if ( sum != lr_num_c ) {
      printf("\nr lr_num_clauses: %d, counted lr_num_c: %d, old: %d", 
	     lr_num_clauses, sum, lr_num_c);
      exit( 1 );
    }
  }


  if ( gcmd_line.debug ) {
    /* Oh... fuck you all: double-2-2-check that memlists are correct!!!
     */
    for ( i = 1; i <= lr_num_c; i++ ) {
      for ( i_ml = lr_pos_c_in_clause_start[i]->next; 
	    i_ml != lr_pos_c_in_clause_end[i]; i_ml = i_ml->next ) {
	c = i_ml->clause;
	for ( j = 0; j < lr_clause_length[c]; j++ ) {
	  if ( lr_clauses[c][j].literal == i ) break;
	}
	if ( j == lr_clause_length[c] ) {
	  printf("\nr error 1\n\n");
	  exit( 1 );
	}
	for ( j_ml = i_ml->next; 
	      j_ml != lr_pos_c_in_clause_end[i]; j_ml = j_ml->next ) {
	  if ( j_ml->clause == i_ml->clause ) {
	    printf("\nr error 1b\n\n");
	    exit( 1 );
	  }
	}
      }
      for ( i_ml = lr_neg_c_in_clause_start[i]->next; 
	    i_ml != lr_neg_c_in_clause_end[i]; i_ml = i_ml->next ) {
	c = i_ml->clause;
	for ( j = 0; j < lr_clause_length[c]; j++ ) {
	  if ( lr_clauses[c][j].literal == (-1) * i ) break;
	}
	if ( j == lr_clause_length[c] ) {
	  printf("\nr error 2\n\n");
	  exit( 1 );
	}
	for ( j_ml = i_ml->next; 
	      j_ml != lr_neg_c_in_clause_end[i]; j_ml = j_ml->next ) {
	  if ( j_ml->clause == i_ml->clause ) {
	    printf("\nr error 2b\n\n");
	    exit( 1 );
	  }
	}
      }
    }
    for ( i = 0; i < lr_num_clauses; i++ ) {
      for ( j = 0; j < lr_clause_length[i]; j++ ) {
	if ( lr_clauses[i][j].literal > 0 ) {
	  v = lr_clauses[i][j].literal;
	  if ( lr_pos_c_in_clause_start[v] == NULL ) {
	    printf("\nr error 3a\n\n");
	    exit( 1 );
	  }
	  for ( i_ml = lr_pos_c_in_clause_start[v]->next; 
		i_ml != lr_pos_c_in_clause_end[v]; i_ml = i_ml->next ) {
	    if ( i_ml->clause == i ) break;
	  }
	  if ( i_ml == lr_pos_c_in_clause_end[v] ) {
	    printf("\nr error 3\n\n");
	    exit( 1 );
	  }
	} else {
	  v = (-1) * lr_clauses[i][j].literal;
	  if ( lr_neg_c_in_clause_start[v] == NULL ) {
	    printf("\nr error 4a\n\n");
	    exit( 1 );
	  }
	  for ( i_ml = lr_neg_c_in_clause_start[v]->next; 
		i_ml != lr_neg_c_in_clause_end[v]; i_ml = i_ml->next ) {
	    if ( i_ml->clause == i ) break;
	  }
	  if ( i_ml == lr_neg_c_in_clause_end[v] ) {
	    printf("\nr error 4\n\n");
	    exit( 1 );
	  }
	}
      }
    }
  }
    


  /* first, create assignments corresponding to decision stack;
   * check for contradictions.
   */
  i = 0;
  while ( i < lr_num_decision_stack ) {
    if ( lr_decision_stack[i] < 0 ) {
      v = (-1) * lr_decision_stack[i];
      sign = 0;
    } else {
      v = lr_decision_stack[i];
      sign = 1;
    }
    if ( lr_assigned[v] == sign ) {
      if ( gcmd_line.R && gcmd_line.debug >= 5 ) {
	printf("\nr skipping pre-insertion %d of var %d val %d, already set so",
	       i, v, sign);
      }
      for ( j = i; j < lr_num_decision_stack-1; j++ ) {
	lr_decision_stack[j] = lr_decision_stack[j+1];
      }
      lr_num_decision_stack--;
      continue;
    }
    if ( lr_assigned[v] != -1 ) {
      if ( gcmd_line.R && gcmd_line.debug >= 5 ) {
	printf("\nr contradictory pre-insertion %d of var %d val %d!",
	       i, v, sign);
      }
      /* unset info
       */
      for ( j = 0; j < i; j++ ) {
	if ( lr_decision_stack[j] < 0 ) {
	  v = (-1) * lr_decision_stack[j];
	  sign = 0;
	} else {
	  v = lr_decision_stack[j];
	  sign = 1;
	}
	lr_assigned[v] = -1;
      }
      return FALSE;
    }
    /* insert new assignment
     */
    if ( gcmd_line.R && gcmd_line.debug >= 5 ) {
      printf("\nr pre-inserting pos %d: var %d val %d",
	     i, v, sign);
    }
    lr_assigned[v] = sign;
    i++;
  }


  if ( lr_num_decision_stack == 0 ) {
    printf("\nr no decision stack after pre-inserting DP??\n\n"); 
    exit( 1 );
  }

  /* now do the corresponding unit props for all the pre-insertions
   * in sequence; is implicit because do_unit_props will move through
   * entire (growing) decision stack, starting from start index
   *
   * no contradiction --> do DP!
   */
  sat = FALSE;
  if ( r_do_unit_props( 0 ) ) {
    sat = r_dp();
  }

  /* undo assignments
   */
  for ( i = 0; i < lr_num_decision_stack; i++ ) {
    if ( lr_decision_stack[i] < 0 ) {
      v = (-1) * lr_decision_stack[i];
    } else {
      v = lr_decision_stack[i];
    }
    if ( gcmd_line.R && gcmd_line.debug >= 5 ) {
      printf("\nr top retracting dec stack %d, var %d val %d!",
	     i, v, lr_decision_stack[i] < 0 ? 0 : 1);
    }
    lr_assigned[v] = -1;
  }
  /* not strictly necessary, as full (..) initialisation done above 
   * before DP calls... looks cleaner this way...
   */
  lr_num_decision_stack = 0;

  return sat;

}



/* invariant: lr_num_decision_stack is same on entry and exit; same holds for
 * lr_assigned array.
 */
Bool r_dp( void )

{

  int entry_num_decision_stack;
  int i, v, dec_v;

  if ( gcmd_line.R && gcmd_line.debug >= 5 ) {
    printf("\nr entering naive dp");
  }

  /* store the entry nr. of decisions on the stack, for later
   * retraction of the new decisions.
   */
  entry_num_decision_stack = lr_num_decision_stack;


  /* choose split vars from "backside of plan"
   */
  for ( dec_v = lr_num_c; dec_v > 0; dec_v-- ) {
    if ( lr_assigned[dec_v] == -1 ) break;
  }
  if ( dec_v == 0 ) {
    /* all vars assigned and no contradiction --> sat!
     */
    if ( gcmd_line.R && gcmd_line.debug >= 5 ) {
      printf("\nr all vars assigned, no contradiction -> sat!");
    }
    return TRUE;
  }

  /* split on v.
   *
   * first, positive.
   */
  if ( lr_num_decision_stack == gnum_ft_conn ) {
    printf("\nr dec stack overflow??\n\n");
    exit( 1 );
  }
  lr_decision_stack[lr_num_decision_stack++] = dec_v;
  lr_assigned[dec_v] = 1;
  if ( gcmd_line.R && gcmd_line.debug >= 5 ) {
    printf("\nr decide %d true, doing unit props", dec_v);
  } 
  if ( r_do_unit_props( lr_num_decision_stack-1 ) ) {
    if ( r_dp() ) {
      /* found solution.
       */
      return TRUE;
    }
  }
  for ( i = entry_num_decision_stack; i < lr_num_decision_stack; i++ ) {
    if ( lr_decision_stack[i] < 0 ) {
      v = (-1) * lr_decision_stack[i];
    } else {
      v = lr_decision_stack[i];
    }
    if ( gcmd_line.R && gcmd_line.debug >= 5 ) {
      printf("\nr true tried retracting dec stack %d, var %d val %d!",
	     i, v, lr_decision_stack[i] < 0 ? 0 : 1);
    }
    lr_assigned[v] = -1;
  }
  lr_num_decision_stack = entry_num_decision_stack;


  /* now, negative.
   */
  lr_decision_stack[lr_num_decision_stack++] = (-1) * dec_v;
  lr_assigned[dec_v] = 0;
  if ( gcmd_line.R && gcmd_line.debug >= 5 ) {
    printf("\nr decide %d false, doing unit props", dec_v);
  } 
  if ( r_do_unit_props( lr_num_decision_stack-1 ) ) {
    if ( r_dp() ) {
      /* found solution.
       */
      return TRUE;
    }
  }
  for ( i = entry_num_decision_stack; i < lr_num_decision_stack; i++ ) {
    if ( lr_decision_stack[i] < 0 ) {
      v = (-1) * lr_decision_stack[i];
    } else {
      v = lr_decision_stack[i];
    }
    if ( gcmd_line.R && gcmd_line.debug >= 5 ) {
      printf("\nr false tried retracting dec stack %d, var %d val %d!",
	     i, v, lr_decision_stack[i] < 0 ? 0 : 1);
    }
    lr_assigned[v] = -1;
  }
  lr_num_decision_stack = entry_num_decision_stack;

  return FALSE;

}



Bool r_do_unit_props( int startidx )

{

  int idx, i, v, c, v_, numopenlits, lastopen;  
  MemberList *i_ml;

  gup_calls++;

  idx = startidx;
  while ( idx < lr_num_decision_stack ) {
    if ( gcmd_line.R && gcmd_line.debug >= 5 ) {
      printf("\nr propagating dec stack %d, %d!",
	     idx, lr_decision_stack[idx]);
      fflush(stdout);
    }
    if ( lr_decision_stack[idx] < 0 ) {
      v = (-1) * lr_decision_stack[idx];
      for ( i_ml = lr_pos_c_in_clause_start[v]->next; 
	    i_ml != lr_pos_c_in_clause_end[v]; i_ml = i_ml->next ) {
	c = i_ml->clause;
	numopenlits = 0;
	lastopen = 0;
	for ( i = 0; i < lr_clause_length[c]; i++ ) {
	  if ( numopenlits > 1 ) break;
	  if ( lr_clauses[c][i].literal > 0 ) {
	    v_ = lr_clauses[c][i].literal;
	    if ( lr_assigned[v_] == -1 ) {
	      numopenlits++;
	      lastopen = lr_clauses[c][i].literal;
	    }
	    if ( lr_assigned[v_] == 1 ) {
	      /* clause satisfied!
	       */
	      break;
	    }
	  } else {
	    v_ = (-1) * lr_clauses[c][i].literal;
	    if ( lr_assigned[v_] == -1 ) {
	      numopenlits++;
	      lastopen = lr_clauses[c][i].literal;
	    }
	    if ( lr_assigned[v_] == 0 ) {
	      /* clause satisfied!
	       */
	      break;
	    }
	  }
	}
	if ( i < lr_clause_length[c] ) {
	  /* clause sat or >1 open lit! ignore.
	   */
	  continue;
	}
	if ( numopenlits == 0 ) {
	  if ( gcmd_line.R && gcmd_line.debug >= 5 ) {
	    printf("\nr empty clause %d", c);
	  }
	  /* no fulfilled and no open lits --> contradiction!!
	   */
	  return FALSE;
	}
	if ( numopenlits == 1 ) {
	  if ( gcmd_line.R && gcmd_line.debug >= 5 ) {
	    printf("\nr unit clause %d", c);
	  }
	  if ( lr_num_decision_stack == gnum_ft_conn ) {
	    printf("\nr dec stack overflow??\n\n");
	    exit( 1 );
	  }
	  /* this one's unit. constrain the variable.
	   */
	  /* DEBUGGING. REMOVE LATER.
	   */
	  if ( lastopen == 0 ) {
	    printf("\n\nr lastopen 0?\n\n");
	    exit( 1 );
	  }
	  if ( lastopen > 0 ) {
	    if ( gcmd_line.R && gcmd_line.debug >= 5 ) {
	      printf("\nr UP constr %d true", lastopen);
	    }
	    lr_assigned[lastopen] = 1;
	  } else {
	    if ( gcmd_line.R && gcmd_line.debug >= 5 ) {
	      printf("\nr UP constr %d false", (-1) * lastopen);
	    }
	    lr_assigned[(-1) * lastopen] = 0;
	  }
	  lr_decision_stack[lr_num_decision_stack++] = lastopen;
	} /* numopenlits == 1 */
      } /* clauses in that negated v participates */
    } else { /* if decision here is positive */
      v = lr_decision_stack[idx];
      fflush(stdout);
      for ( i_ml = lr_neg_c_in_clause_start[v]->next; 
	    i_ml != lr_neg_c_in_clause_end[v]; i_ml = i_ml->next ) {
	c = i_ml->clause;
	numopenlits = 0;
	lastopen = 0;
	for ( i = 0; i < lr_clause_length[c]; i++ ) {
	  if ( numopenlits > 1 ) break;
	  if ( lr_clauses[c][i].literal > 0 ) {
	    v_ = lr_clauses[c][i].literal;
	    if ( lr_assigned[v_] == -1 ) {
	      numopenlits++;
	      lastopen = lr_clauses[c][i].literal;
	    }
	    if ( lr_assigned[v_] == 1 ) {
	      /* clause satisfied!
	       */
	      break;
	    }
	  } else {
	    v_ = (-1) * lr_clauses[c][i].literal;
	    if ( lr_assigned[v_] == -1 ) {
	      numopenlits++;
	      lastopen = lr_clauses[c][i].literal;
	    }
	    if ( lr_assigned[v_] == 0 ) {
	      /* clause satisfied!
	       */
	      break;
	    }
	  }
	}
	if ( i < lr_clause_length[c] ) {
	  /* clause sat or >1 open lit! ignore.
	   */
	  continue;
	}
	if ( numopenlits == 0 ) {
	  if ( gcmd_line.R && gcmd_line.debug >= 5 ) {
	    printf("\nr empty clause %d", c);
	  }
	  /* no fulfilled and no open lits --> contradiction!!
	   */
	  return FALSE;
	}
	if ( numopenlits == 1 ) {
	  if ( gcmd_line.R && gcmd_line.debug >= 5 ) {
	    printf("\nr unit clause %d", c);
	  }
	  if ( lr_num_decision_stack == gnum_ft_conn ) {
	    printf("\nr dec stack overflow??\n\n");
	    exit( 1 );
	  }
	  /* this one's unit. constrain the variable.
	   */
	  /* DEBUGGING. REMOVE LATER.
	   */
	  if ( lastopen == 0 ) {
	    printf("\n\nr lastopen 0?\n\n");
	    exit( 1 );
	  }
	  if ( lastopen > 0 ) {
	    if ( gcmd_line.R && gcmd_line.debug >= 5 ) {
	      printf("\nr UP constr %d true", lastopen);
	    }
	    lr_assigned[lastopen] = 1;
	  } else {
	    if ( gcmd_line.R && gcmd_line.debug >= 5 ) {
	      printf("\nr UP constr %d false", (-1) * lastopen);
	    }
	    lr_assigned[(-1) * lastopen] = 0;
	  }
	  lr_decision_stack[lr_num_decision_stack++] = lastopen;
	} /* numopenlits == 1 */
      } /* clauses in that positive v participates */
    } /* neg or pos */
    idx++;
  } /* while not all decisions on stack propagated */

  return TRUE;

}



void print_r_clauses( void )

{

  int i, j;
  
  for ( i = 0; i < lr_num_clauses; i++ ) {
    printf("\n%d: ", i);
    for ( j = 0; j < lr_clause_length[i]; j++ ) {
      if ( lr_clauses[i][j].time >= 0 ) {
	if ( lr_clauses[i][j].literal < 0 ) {
	  printf("-");
	  print_ft_name( (-1) * lr_clauses[i][j].literal - 1 );
	} else {
	  print_ft_name( lr_clauses[i][j].literal - 1 );
	}
	printf("(%d) ", lr_clauses[i][j].time);
      } else {
	/* this is a code!
	 */
	if ( lr_clauses[i][j].literal < 0 ) {
	  printf("-");
	  print_ft_name( lr_cf[(-1) * lr_clauses[i][j].literal] );
	} else {
	  print_ft_name( lr_cf[lr_clauses[i][j].literal] );
	}
      }
    }
  }

}



void print_r_clause( int i )

{

  int j;
  
  printf("\n");
  for ( j = 0; j < lr_clause_length[i]; j++ ) {
    if ( lr_clauses[i][j].time >= 0 ) {
      if ( lr_clauses[i][j].literal < 0 ) {
	printf("-");
	print_ft_name( (-1) * lr_clauses[i][j].literal - 1 );
      } else {
	print_ft_name( lr_clauses[i][j].literal - 1 );
      }
      printf("(%d) ", lr_clauses[i][j].time);
    } else {
      /* this is a code!
       */
      if ( lr_clauses[i][j].literal < 0 ) {
	printf("-");
	print_ft_name( lr_cf[(-1) * lr_clauses[i][j].literal] );
      } else {
	print_ft_name( lr_cf[lr_clauses[i][j].literal] );
      }
    }
  }

}



int r_extend_dynamic_clauses_base( State_pointer *path, int num_path, int *path_op )

{

  static Bool fc = TRUE;
  static Bool *Ft, *Ut, *Ftpp, *Utpp;
  
  int i, j, k, ef, t, time, ft, m, ff;
  
  if ( fc ) {
    Ft = ( Bool * ) calloc( gnum_ft_conn, sizeof( Bool ) );
    Ut = ( Bool * ) calloc( gnum_ft_conn, sizeof( Bool ) );
    Ftpp = ( Bool * ) calloc( gnum_ft_conn, sizeof( Bool ) );
    Utpp = ( Bool * ) calloc( gnum_ft_conn, sizeof( Bool ) );
    for ( i = 0; i < gnum_ft_conn; i++ ) {
      Ft[i] = FALSE;
      Ut[i] = FALSE;
      Ftpp[i] = FALSE;
      Utpp[i] = FALSE;
    }
    
    fc = FALSE;
  }
  
  
  
  /* the states leading from current end-fixed-path-state to S are in 
   * path[num_path + 1] .. path[MAX_PLAN_LENGTH].
   * path_op[t] contains the op that leads into state [t]. we need this 
   * to check for noops, ie the case of uncond nondet effects.
   * unknown efs leading to each state are stored in the state,
   * (also in dest already) so we
   * produce our CNF based on these.
   */
  if ( gcmd_line.R && gcmd_line.debug ) {
    printf("\nR dynamic CNF");
  }
  if ( gcmd_line.R && gcmd_line.debug ) {
    printf("\n\n-----------------------adding clauses from %d",
	   gnum_clauses);
  }
  if ( gnum_clauses != gnum_fixed_clauses ) {
    printf("\n\nat start R dyn. extend gnum_clauses %d != gnum_fixed_clauses %d??\n\n",
	   gnum_clauses, gnum_fixed_clauses);
    fflush(stdout);
    exit( 1 );
  }
  /* insert the t -> t+1 implications for all t on the path.
   *
   * the times here are t-(num_path+1) -> t-(num_path+1)+1 for t in:
   */
  time = gfixed_endtime - 1;
  for ( t = num_path + 1; t < MAX_PLAN_LENGTH; t++ ) {
    time = gfixed_endtime + t - (num_path + 1);
    
    /* first, make fast accessible the F and U info at t and t+1
     */
    for ( i = 0; i < path[t]->num_F; i++ ) {
      Ft[path[t]->F[i]] = TRUE;
    }
    for ( i = 0; i < path[t]->num_U; i++ ) {
      Ut[path[t]->U[i]] = TRUE;
    }
    for ( i = 0; i < path[t+1]->num_F; i++ ) {
      Ftpp[path[t+1]->F[i]] = TRUE;
    }
    for ( i = 0; i < path[t+1]->num_U; i++ ) {
      Utpp[path[t+1]->U[i]] = TRUE;
    }
    
    /* the unknown effect - implications; these efs are stored
     * at the state which the resp op leads to.
     * (for known efs these clauses are fulfilled anyway so we can skip them.)
     */
    for ( i = 0; i < path[t+1]->num_unknown_E; i++ ) {
      ef = path[t+1]->unknown_E[i];
      /* for all non-true adds ad:
       * -u1(t) vee .. -un(t) vee ad(t+1)
       * where u1 .. un are the unknown conds of the ef.
       */
      for ( j = 0; j < gef_conn[ef].num_A; j++ ) {
	if ( !gft_conn[gef_conn[ef].A[j]].CNF ) continue;
	if ( Ftpp[gef_conn[ef].A[j]] ) continue;
	if ( gef_conn[ef].A_nondet[j] ) continue;
	if ( gnum_clauses == gmax_clauses ) {
	  printf("\n\ntoo many clauses? %d\n\n", gnum_clauses);
	  exit( 1 );
	}
	for ( k = 0; k < gef_conn[ef].num_C; k++ ) {
	  if ( Ft[gef_conn[ef].C[k]] ) continue;
	  /* DEBUGGING; REMOVE LATER
	   */
	  if ( !Ut[gef_conn[ef].C[k]] ) {
	    printf("\nunknown ef cond neither F nor U in CNF gen\n\n");
	    exit(1);
	  }
	  ff = gef_conn[ef].C[k];
	  if ( gft_conn[ff].CNF ) {
	    m = 1;
	  } else {
	    m = -1;
	    ff = gft_conn[ff].negation;
	  }
	  gclauses[gnum_clauses][gclause_length[gnum_clauses]].literal =
	    m * (-1) * (1 + ff);
	  gclauses[gnum_clauses][gclause_length[gnum_clauses]++].time =
	    time;
	}
	ff = gef_conn[ef].A[j];
	m = 1;
	gclauses[gnum_clauses][gclause_length[gnum_clauses]].literal =
	  m * (1 + ff);
	gclauses[gnum_clauses][gclause_length[gnum_clauses++]++].time =
	  time + 1;
	if ( gcmd_line.R && gcmd_line.debug ) {
	  printf("\nunknown add eff clause"); print_clause( gnum_clauses-1 );
	}
      }
      /* for all non-false dels de:
       * -u1(t) vee .. -un(t) vee -de(t+1)
       * where u1 .. un are the unknown conds of the ef.
       */
      for ( j = 0; j < gef_conn[ef].num_D; j++ ) {
	if ( !gft_conn[gef_conn[ef].D[j]].CNF ) continue;
	if ( !Ftpp[gef_conn[ef].D[j]] && !Utpp[gef_conn[ef].D[j]] ) continue;
	if ( gef_conn[ef].D_nondet[j] ) continue;
	if ( gnum_clauses == gmax_clauses ) {
	  printf("\n\ntoo many clauses? %d\n\n", gnum_clauses);
	  exit( 1 );
	}
	for ( k = 0; k < gef_conn[ef].num_C; k++ ) {
	  if ( Ft[gef_conn[ef].C[k]] ) continue;
	  /* DEBUGGING; REMOVE LATER
	   */
	  if ( !Ut[gef_conn[ef].C[k]] ) {
	    printf("\nunknown ef cond neither F nor U in CNF gen\n\n");
	    exit(1);
	  }
	  ff = gef_conn[ef].C[k];
	  if ( gft_conn[ff].CNF ) {
	    m = 1;
	  } else {
	    m = -1;
	    ff = gft_conn[ff].negation;
	  }
	  gclauses[gnum_clauses][gclause_length[gnum_clauses]].literal =
	    m * (-1) * (1 + ff);
	  gclauses[gnum_clauses][gclause_length[gnum_clauses]++].time =
	    time;
	}
	ff = gef_conn[ef].D[j];
	m = 1;
	gclauses[gnum_clauses][gclause_length[gnum_clauses]].literal =
	  m * (-1) * (1 + ff);
	gclauses[gnum_clauses][gclause_length[gnum_clauses++]++].time =
	  time + 1;
	if ( gcmd_line.R && gcmd_line.debug ) {
	  printf("\nunknown del eff clause"); print_clause( gnum_clauses-1 );
	}
      }
    }
    
    /* now, the noop - implications; first, the positives then the negatives
     * QUESTION: do we need the negatives for our derivations to be correct?
     */
    for ( ft = 0; ft < gnum_ft_conn; ft++ ) {
      if ( !gft_conn[ft].CNF ) continue;
      if ( Ftpp[ft] ) continue;/* clause satisfied */
      if ( !Utpp[ft] ) {
	/* proved false; so prev step, or conds, are proved false
	 * and clause thus satisfied.
	 *
	 * note: this check is necessary for validity of clause;
	 * in partic the uncond dels are false so no clause is
	 * generated in this case.
	 */
	continue;
      }
      if ( !Ft[ft] && !Ut[ft] ) continue;/* clause satisfied */
      if ( r_unconditional_nondet_del_on( ft, Ft, path_op[t+1] ) ) {
	/* there's a nondet del effect with fulfilled cond. no NOOP.
	 */
	continue;
      }
      /* we deal with Ft || Ut -> Utpp;
       * clauses are all -ft(t) vee c1 .. cn vee ft(t+1)
       * where c1 .. cn are conds of unknown efs deleting ft
       */
      insert_posnoop_hitting_set_clauses( ft, time, 
					  path[t+1],
					  Ft, Ut );
    }
    /* negative noops
     */
    for ( ft = 0; ft < gnum_ft_conn; ft++ ) {
      if ( !gft_conn[ft].CNF ) continue;
      if ( !Ftpp[ft] && !Utpp[ft] ) continue;/* clause satisfied */
      if ( Ftpp[ft] ) {
	/* proved true; so prev step, or conds, are proved false
	 * and clause thus satisfied.
	 */
	continue;
      }
      if ( Ft[ft] ) continue;/* clause satisfied */
      if ( r_unconditional_nondet_add_on( ft, Ft, path_op[t+1] ) ) {
	/* there's a nondet del effect with fulfilled cond. no NOOP.
	 */
	continue;
      }
      /* we deal with Ft || Ut -> Utpp;
       * clauses are all -ft(t) vee c1 .. cn vee ft(t+1)
       * where c1 .. cn are conds of unknown efs deleting ft
       */
      insert_negnoop_hitting_set_clauses( ft, time, 
					  path[t+1],
					  Ft, Ut );
    }
    
    /* undo F U info.
     */
    for ( i = 0; i < path[t]->num_F; i++ ) {
      Ft[path[t]->F[i]] = FALSE;
    }
    for ( i = 0; i < path[t]->num_U; i++ ) {
      Ut[path[t]->U[i]] = FALSE;
    }
    for ( i = 0; i < path[t+1]->num_F; i++ ) {
      Ftpp[path[t+1]->F[i]] = FALSE;
    }
    for ( i = 0; i < path[t+1]->num_U; i++ ) {
      Utpp[path[t+1]->U[i]] = FALSE;
    }
  } /* end t loop */

  /* debugging: see what that looks like!
   */
  if ( gcmd_line.R && gcmd_line.debug >= 7 ) {
    if ( gcmd_line.R && gcmd_line.debug >= 9 ) {
      printf("\n\n-----------------------------R to execution path\n");
      printf("\n---------state:");
      print_state( *(path[num_path+1]) );
      for ( t = num_path + 2; t <= MAX_PLAN_LENGTH; t++ ) {
	printf("\n----------state:");
	print_state( *(path[t]) );
      }
    }
    printf("\n\n-----------------------------R I get the CNF\n");
    print_clauses();
  }
  
  if ( time < -1 ) {
    printf("\n\ntime < -1 in R dynamic extend??\n\n");
    exit( 1 );
  }
  
  return time + 1;

}



/* these two are exactly the same as in state_transitions.c
 * except for the debugging info...
 */
Bool r_unconditional_nondet_del_on( int ft, Bool *Ft, int op )

{

  int i, j, ef;
  
  if ( gcmd_line.R && gcmd_line.debug >= 5 ) {
    printf("\nchecking if ");
    print_op_name( op );
    printf(" uncond nondet del ");
    print_ft_name( ft );
  }

  for ( i = 0; i < gop_conn[op].num_E; i++ ) {
    ef = gop_conn[op].E[i];
    for ( j = 0; j < gef_conn[ef].num_C; j++ ) {
      if ( !Ft[gef_conn[ef].C[j]] ) break;
    }
    if ( j < gef_conn[ef].num_C ) continue;

    for ( j = 0; j < gef_conn[ef].num_D; j++ ) {
      if ( gef_conn[ef].D[j] == ft ) {
	if ( gef_conn[ef].D_nondet[j] ) {
	  if ( gcmd_line.R && gcmd_line.debug >= 5 ) {
	    printf(" -- yes it does!");
	  }
	  return TRUE;
	} else {
	  printf("\n\nuncond det del eff in noop check?!\n\n");
	  exit( 1 );
	}
      }
    }
  }

  if ( gcmd_line.R && gcmd_line.debug >= 5 ) {
    printf(" -- no it doesn't!");
  }
  return FALSE;

}



Bool r_unconditional_nondet_add_on( int ft, Bool *Ft, int op )

{

  int i, j, ef;
  
  if ( gcmd_line.R && gcmd_line.debug >= 5 ) {
    printf("\nchecking if ");
    print_op_name( op );
    printf(" uncond nondet add ");
    print_ft_name( ft );
  }

  for ( i = 0; i < gop_conn[op].num_E; i++ ) {
    ef = gop_conn[op].E[i];
    for ( j = 0; j < gef_conn[ef].num_C; j++ ) {
      if ( !Ft[gef_conn[ef].C[j]] ) break;
    }
    if ( j < gef_conn[ef].num_C ) continue;

    for ( j = 0; j < gef_conn[ef].num_A; j++ ) {
      if ( gef_conn[ef].A[j] == ft ) {
	if ( gef_conn[ef].A_nondet[j] ) {
	  if ( gcmd_line.R && gcmd_line.debug >= 5 ) {
	    printf(" -- yes it does!");
	  }
	  return TRUE;
	} else {
	  printf("\n\nuncond det add eff in noop check?!\n\n");
	  exit( 1 );
	}
      }
    }
  }

  if ( gcmd_line.R && gcmd_line.debug >= 5 ) {
    printf(" -- no it doesn't!");
  }
  return FALSE;

}
