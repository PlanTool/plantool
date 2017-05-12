


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








extern "C" {
#include "ff.h"

#include "output.h"
#include "memory.h"

#include "relax.h"
#include "search.h"
#include "state_transitions.h"
}






#include "Cachet-1.21-wmc/SAT.h"

// #include <set.h>
// #include <vector.h>










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






/* CNF time of vars in S 
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

/* july06: some of the codes will be artificial, and for those we got
 * to communicate the dynamically created weight, with this guy here.
 */
double *lr_cweight;

 

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









/* communicate via this flag from RPG to 1P if
 * goals were reached by probability check
 * -- is taken into account when deciding about the form of 1P extraction. 
 *
 * note: only a shortcut, could be easily found out by looking at some
 * of the built data structures.
 */
Bool lgoals_not_known;



/* the circumstances under which preselection in 1P is done are a bit involved;
 * communicate whether or not by this flag here.
 */
Bool l1P_preselection_done;






/* july06: store the p effs that have true conds.
 */
int *lpE;
int lnum_pE;


/* july06: this guy will store the actual goal likelyhood computed below
 * "goals activated"; needed for new termination criterion.
 */
double lthisgoalprob;



/* july06: this guy here stores lists of probnodes.
 * the lists are allocated as needed!
 * is allocated to arrays of pointers;
 * lprobU points to start element, lprobUend to the curent
 * end of the list.
 */
UftNode **lprobU, **lprobUend;
int *lnum_probU;





/* communication in paths weight
 */
double lpathsweight;





















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



/* this here used to be simple... before we went to probabilistic...
 */

Bool satisfies_goal( State *S, State *current_goals )

{

  int i, j, ft, m;
  double a, b, gprob;

  static Bool fc = TRUE;

/*   static int num = 0; */
  
  /* first, see if goal is subset of F
   */
  for ( i = 0; i < current_goals->num_F; i++ ) {
    for ( j = 0; j < S->num_F; j++ ) {
      if ( S->F[j] == current_goals->F[i] ) break;
    }
    if ( j == S->num_F ) {
      break;
    }
  }
  if ( i == current_goals->num_F ) {
    /* goal definitely satisfied!
     */
    if ( gcmd_line.R && gcmd_line.debug  >= 2 ) {
      printf("\nall goals known! no wmc test for goal satisfaction needed.");
      fflush(stdout);
    }
    return TRUE;
  }

  /* if we *want* the goal to be 100% true, back out right here.
   */
  if ( ggoal_percent == 100 ) {
    return FALSE;
  }


  /* now, see if goal is subset of F cup U
   */
  for ( i = 0; i < current_goals->num_F; i++ ) {
    for ( j = 0; j < S->num_F; j++ ) {
      if ( S->F[j] == current_goals->F[i] ) break;
    }
    if ( j == S->num_F ) {
      for ( j = 0; j < S->num_U; j++ ) {
	if ( S->U[j] == current_goals->F[i] ) break;
      }
      if ( j == S->num_U ) {
	if ( gcmd_line.R && gcmd_line.debug  >= 2 ) {
	  printf("\n>=1 goal known to be false! no wmc test for goal satisfaction needed: ");
	  print_ft_name( current_goals->F[i] );
	  fflush(stdout);
	}
	return FALSE;
      }
    }
  }


  /* YES: extend the S CNF encoding with singleton clauses containing
   * the unknown top level goals.
   */
  if ( gcmd_line.R && gcmd_line.debug  >= 2 ) {
    printf("\ndoing wmc test for goal satisfaction now.");
    fflush(stdout);
  }

  gnum_decision_stack = 0;
  for ( i = 0; i < current_goals->num_F; i++ ) {
    for ( j = 0; j < S->num_F; j++ ) {
      if ( S->F[j] == current_goals->F[i] ) break;
    }
    if ( j < S->num_F ) {
      if ( gcmd_line.R && gcmd_line.debug  >= 2 ) {
	printf("\n");
	print_ft_name( current_goals->F[i] );
	printf(" known to be true in S. skipping.");
	fflush(stdout);
      }
      continue;
    }
    
    /* got one in U.
     */
    ft = current_goals->F[i];
    if ( gcmd_line.R && gcmd_line.debug  >= 2 ) {
      printf("\n");
      print_ft_name( ft );
      printf(" unknown in S. adding it to state formula.");
      fflush(stdout);
    }
    if ( gft_conn[ft].CNF ) {
      m = 1;
    } else {
      m = -1;
      ft = gft_conn[ft].negation;
      if ( ft == -1 ) {
	printf("\n\nnon-CNF goal fact not negated?\n\n");
	exit( 1 );
      }
    }
    if ( gcodes[lStime][ft] == -1 ) {
      /* we have to make sure we got the codes. the unit clauses here
       * are important for weighted model counting even if the resp
       * literals are completely disconnected to the rest of
       * the CNF.
       *
       * so, if there's no code, let's just create it, for convenience
       * of the WMC procedure called below.
       */
      gnum_c++;/* must be non-zero */
      gcodes[lStime][ft] = gnum_c;
      if ( gnum_c == (1 + MAX_PLAN_LENGTH) * gmax_CNFU ) {
	printf("\n\ntoo many codes? %d\n\n", gnum_c);
	exit( 1 );
      }
      gcf[gnum_c] = ft;
      gct[gnum_c] = lStime; 
    }

    /* we have the code. write it into the disj. code array.
     */
    gdecision_stack[gnum_decision_stack++] = m * gcodes[lStime][ft];
  } /* loop insert goals into decision stack */

  times( &end );
  TIME( geval_time );
  times( &start );
  /* count models with these constraints
   */
/*   printf("\n %d", num++); */
  gwmc_calls++;
  wmc_CNF(&a, &b);
  /* we also need the count without the constraints.
   *
   * if BN is unconstrained, this will always be 1.
   * in any case, it will always be the same as for initial state,
   * so compute only upon first call of this fn.
   * (WHICH IS THE 1ST ONE THAT DOES THIS STUFF)
   */
  if ( gBNevidence ) {
    if ( fc ) {
      gnum_decision_stack = 0;
      gwmc_calls++;
      wmc_CNF(&gBNa, &gBNb);
      fc = FALSE;
    }
  } /* else: both are initialized to 1 in main.c */
  times( &end );
  TIME( gwmc_time );
  times( &start );

  /* a/b enough for goal? use this tool here.
   */
  gprob = get_gprob( a, b );


  if ( gprob >= ggoal_probability ) {
    if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
      printf("\ngoal state!");
      fflush(stdout);
    }
    return TRUE;
  }

  if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
    printf("\nno goal state!");
    fflush(stdout);
  }
  return FALSE;

}










/***********************************
 * INITIALIZATION AND CNF CREATION STUFF *
 ***********************************/











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
  lMAX_PATHNODES = (RELAXED_STEPS + MAX_PLAN_LENGTH) * (gmax_U+50);/* july06: hack for prob effs; is double-checked in code */

  /* rp stuff
   */
  lF = ( int * ) calloc( gnum_ft_conn, sizeof( int ) );
  lE = ( int * ) calloc( gnum_ef_conn, sizeof( int ) );
  lch_E = ( int * ) calloc( gnum_ef_conn, sizeof( int ) );
  l0P_E = ( int * ) calloc( gnum_ef_conn, sizeof( int ) );

  lpE = ( int * ) calloc( gnum_ef_conn, sizeof( int ) );


  /* get A stuff
   */
  l0P_O = ( int * ) calloc( gnum_op_conn, sizeof( int ) );
  lch_O = ( int * ) calloc( gnum_ef_conn, sizeof( int ) );


  lprobU = ( UftNode ** ) calloc(RELAXED_STEPS + MAX_PLAN_LENGTH, sizeof( UftNode *) );
  lprobUend = ( UftNode ** ) calloc(RELAXED_STEPS + MAX_PLAN_LENGTH, sizeof( UftNode *) );
  lnum_probU = ( int * ) calloc(RELAXED_STEPS + MAX_PLAN_LENGTH, sizeof( int ) );
  for ( i = 0; i < RELAXED_STEPS + MAX_PLAN_LENGTH; i++ ) {
    lprobU[i] = ( UftNode * ) calloc(1, sizeof( UftNode ) );
    lprobU[i]->next = NULL;
    lprobUend[i] = lprobU[i];
  }


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
   */

  if ( gcmd_line.heuristic == 1 ) {
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
    lr_cf = ( int * ) calloc( gnum_ft_conn + 1001, sizeof( int ) );/* july06: space for prob nodes */
    /* july06: weight of dynaically created chance nodes
     */
    lr_cweight = ( double * ) calloc( gnum_ft_conn + 1001, sizeof( double ) );

    lr_num_c = 0;
    for ( i = 0; i < gnum_ft_conn; i++ ) {
      lr_codes[i] = -1;
    }
    lr_assigned = ( int * ) calloc( gnum_ft_conn + 1001, sizeof( int ) );/* july06: space for prob nodes */
    for ( i = 0; i < gnum_ft_conn + 1001; i++ ) {
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
      if ( gcmd_line.R && gcmd_line.debug > 2 ) {
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



void r_insert_dynamic_membership( int v, Bool neg, int c )

{

  if ( lr_pos_c_in_clause_start[v] == NULL ) {
    if ( lr_neg_c_in_clause_start[v] != NULL ) {
      printf("\n%d pos start NULL, neg not??\n\n", v);
      exit( 1 );
    }
    lr_pos_c_in_clause_start[v] = new_MemberList();
    lr_pos_c_in_clause_start[v]->clause = -1;
    lr_pos_c_in_clause_end[v] = new_MemberList();
    lr_pos_c_in_clause_end[v]->clause = -1;
    lr_pos_c_in_clause_start[v]->next = lr_pos_c_in_clause_end[v];

    lr_neg_c_in_clause_start[v] = new_MemberList();
    lr_neg_c_in_clause_start[v]->clause = -1;
    lr_neg_c_in_clause_end[v] = new_MemberList();
    lr_neg_c_in_clause_end[v]->clause = -1;
    lr_neg_c_in_clause_start[v]->next = lr_neg_c_in_clause_end[v];
  }

  if ( c == -1 ) {
    /* no clause given, only to create list for this v
     */
    return;
  }

  if ( !neg ) {
    if ( lr_pos_c_in_clause_end[v]->clause == -1 ) {
      /* we're at the end of the allocated list!
       */
      lr_pos_c_in_clause_end[v]->clause = c;
      lr_pos_c_in_clause_end[v]->next = new_MemberList();
      lr_pos_c_in_clause_end[v]->next->clause = -1;
      lr_pos_c_in_clause_end[v] = lr_pos_c_in_clause_end[v]->next;
    } else {
      /* we're still in the middle of the list.
       */
      lr_pos_c_in_clause_end[v]->clause = c;
      lr_pos_c_in_clause_end[v] = lr_pos_c_in_clause_end[v]->next;
    } /* case distinction for end pointer of v list */
    return;
  } /* !neg? */

  if ( lr_neg_c_in_clause_end[v]->clause == -1 ) {
    /* we're at the end of the allocated list!
     */
    lr_neg_c_in_clause_end[v]->clause = c;
    lr_neg_c_in_clause_end[v]->next = new_MemberList();
    lr_neg_c_in_clause_end[v]->next->clause = -1;
    lr_neg_c_in_clause_end[v] = lr_neg_c_in_clause_end[v]->next;
  } else {
    /* we're still in the middle of the list.
     */
    lr_neg_c_in_clause_end[v]->clause = c;
    lr_neg_c_in_clause_end[v] = lr_neg_c_in_clause_end[v]->next;
  } /* case distinction for end pointer of v list */

}



void r_remove_member( int v, Bool neg, int c )

{

  MemberList *i_ml;

  if ( !neg ) {
    for ( i_ml = lr_pos_c_in_clause_start[v]->next; 
	  i_ml != lr_pos_c_in_clause_end[v]; i_ml = i_ml->next ) {
      if ( i_ml->clause == c ) {
	break;
      }
    }

    if ( i_ml == lr_pos_c_in_clause_end[v] ) {
      printf("\n\nr: pos var, %d member to be removed, not found!\n\n", c);
      exit( 1 );
    }

    /* simply move all the clause indices (contents), up to
     * one before end marker, one step forward, and
     * decrement end marker.
     */
    for ( ; i_ml->next != lr_pos_c_in_clause_end[v]; i_ml = i_ml->next ) {
      i_ml->clause = i_ml->next->clause;
    }
    lr_pos_c_in_clause_end[v] = i_ml;

    return;
  }


  for ( i_ml = lr_neg_c_in_clause_start[v]->next; 
	i_ml != lr_neg_c_in_clause_end[v]; i_ml = i_ml->next ) {
    if ( i_ml->clause == c ) {
      break;
    }
  }  
  if ( i_ml == lr_neg_c_in_clause_end[v] ) {
    printf("\n\nr: neg var, %d member to be removed, not found!\n\n", c);
    exit( 1 );
  }
  for ( ; i_ml->next != lr_neg_c_in_clause_end[v]; i_ml = i_ml->next ) {
    i_ml->clause = i_ml->next->clause;
  }
  lr_neg_c_in_clause_end[v] = i_ml;

}






















/***********************************
 * MAIN OVERLOOKING FUNCTIONS
 ***********************************/



















int get_1P_and_H( State *S, State *current_goals, 
		  EhcNode *ehc_father, BfsNode *bfs_father, int S_op )

{

  int h, max, i;
  Bool reach;
  int retval;

  gevaluated_states++;

  times( &end );
  TIME( gsearch_time );
  times( &start );

  source_to_dest( &lcurrent_goals, current_goals );  


/*   printf("\nstate: -----------------------------------------------------"); */
/*   print_state(*S); */


  /* ask goal satisfaction after initialization since
   * we need the state formula in order to count
   * the goal-models.
   *
   * NOTE: goal satisfaction here, means a likelyhood test!
   *
   * NOTE ALSO: doing this here, in the h fn, is somewhat wasteful
   * since it means that we have to re-construct the state formula
   * even if not using -h 2. we could check goal satisfaction
   * when generating the new states in result_to_dest.
   * keep it here for now, since it goes easier with the overall
   * code structure (that relies on h==0 to detect goal states). 
   */
  initialize_fixpoint( S, ehc_father, bfs_father, S_op );
  if ( satisfies_goal( S, current_goals ) ) {
    reset_fixpoint();
    return 0;
  }
  
  reach = build_fixpoint( S, &max );
  if ( gcmd_line.display_info == 122 ||
       (gcmd_line.R && gcmd_line.debug > 2) ) {
    print_fixpoint_result();
    fflush(stdout);
  }

/*   printf("\n\n"); */
/*   exit( 1 ); */


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

/*   printf("\n\n"); */
/*   exit( 1 ); */

  reset_fixpoint();

  /* replace helpfuls with A if wanted, or needed
   * since H is empty (can happen if rplan is already empty
   * but no goal state)
   */
//   printf("\nH: ");
//   for ( i = 0; i < gnum_H; i++ ) {
//     print_op_name(gH[i]);printf(" ");
//   }

  if ( !gcmd_line.help || gnum_H == 0 ) {
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

/*   printf("h val: %d\n\n", h); */

  /* for heuristic 1, 0 doesn't mean we've reached the goal,
   * so we need to take care that the return h value is 0 only if the goals
   * are true (which is checked above)
   */
  retval = h;
  if ( gcmd_line.heuristic == 1 ) {
    if ( retval != INFINITY ) {
      retval = h + 1;
    }
  }


/*   printf("\nreturn val: %d; max: %d\n\n", retval, max); */
/*   exit( 1 ); */


  return retval;

}



int get_1P( State *S, State *current_goals,
	    EhcNode *ehc_father, BfsNode *bfs_father, int S_op )

{

  int h, max;
  Bool reach;
  int retval;

  gevaluated_states++;

  times( &end );
  TIME( gsearch_time );
  times( &start );

  source_to_dest( &lcurrent_goals, current_goals );  

  /* ask goal satisfaction after initialization sinze
   * we need the state formula in order to count
   * the goal-models.
   *
   * NOTE: goal satisfaction here, means a likelyhood test!
   *
   * NOTE ALSO: doing this here, in the h fn, is somewhat wasteful
   * since it means that we have to re-construct the state formula
   * even if not using -h 2. we could check goal satisfaction
   * when generating the new states in result_to_dest.
   * keep it here for now, since it goes easier with the overall
   * code structure (that relies on h==0 to detect goal states). 
   */
  initialize_fixpoint( S, ehc_father, bfs_father, S_op );
  if ( satisfies_goal( S, current_goals ) ) {
    reset_fixpoint();
    return 0;
  }

  reach = build_fixpoint( S, &max );
  if ( gcmd_line.display_info == 122 ||
       (gcmd_line.R && gcmd_line.debug > 2) ) {
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

  reset_fixpoint();

  times( &end );
  TIME( geval_time );
  times( &start );

  /* for heuristic 1, 0 doesn't mean we've reached the goal,
   * so we need to take care that the return h value is 0 only if the goals
   * are true (which is checked above)
   */
  retval = h;
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

  /* july06: new termination criterion!
   * "nothing has changed on the deterministic side, and the goal
   * prob has not increased" -- the only new thing can be the repeated
   * application of the same prob eff. if that does not increase the goal prob
   * then we're done.
   */
  double lastgoalprob = 0.0;

  Bool new_U;

  start_ft = 0;
  start_ef = 0;
  while ( TRUE ) {

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

    start_ft = stop_ft;
    start_ef = stop_ef;
    time++;


    /* july06: this will have to set lthisgoalprob as a side effect.
     *
     * moved this to end of loop since asking this for time==0 is bullshit anyway:
     * goal satisfaction is tested beforehand.
     */
    if ( all_goals_activated( time ) ) {
      break;
    }
    
    if ( stop_ft == lnum_F && !new_U && lthisgoalprob <= lastgoalprob ) { 
      /* july06: I think max doesn't matter in this case... anyway...
       * say "-1" here so it's sychronized with what used to be returned when
       * the termination criterion was still in front of "time++"
       */
      *max = time-1;
      return FALSE;
    }
    lastgoalprob = lthisgoalprob;

  } /* endwhile main RPG loop */

  *max = time;
  return TRUE;

}    



void initialize_fixpoint( State *S, 
			  EhcNode *ehc_father, BfsNode *bfs_father, int S_op )

{

  int i;

  lnum_E = 0;
  lnum_ch_E = 0;

  lnum_pE = 0;

  lnum_F = 0;
  for ( i = 0; i < S->num_F; i++ ) {
    if ( gft_conn[S->F[i]].in_F ) {
      continue;
    }
    new_fact( S->F[i] );
  }

  lgoals_not_known = FALSE;
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

  /* july06: treat the common adds just like usual adds;
   * remember this effect for treatment in append U below.
   */
  if ( gef_conn[index].eff_p < 1 ) {
    for ( i = 0; i < gef_conn[index].num_SA; i++ ) {
      if ( gft_conn[gef_conn[index].SA[i]].in_F ) {
	continue;
      }
      new_fact( gef_conn[index].SA[i] );
    }

    lpE[lnum_pE++] = index;
    return;
  } 

  for ( i = 0; i < gef_conn[index].num_A; i++ ) {
    if ( gft_conn[gef_conn[index].A[i]].in_F ) {
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



void insert_path_implications( State *S, EhcNode *ehc_father, BfsNode *bfs_father, int S_op )

{

  static Bool fc = TRUE;
  static State_pointer *path;
  static int *path_op;
  static Bool *Ut, *Utpp;
  
  int i, j, k, l, ef, ef_, num_path, t, c, conu, addu, time, S_num_path = -17;
  int Ucond, nowUft, notc, ft;
  EhcNode *iehc;
  BfsNode *ibfs;
  UftNode *probU, *iprobU;



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


  if ( 0 || (gcmd_line.R && gcmd_line.debug > 2) ) {
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
      if ( gcmd_line.R && gcmd_line.debug > 2  ) {
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
	if ( gcmd_line.R && gcmd_line.debug > 2  ) {
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
      if ( gcmd_line.R && gcmd_line.debug > 2  ) {
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
	if ( gcmd_line.R && gcmd_line.debug > 2 ) {
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
	if ( gcmd_line.R && gcmd_line.debug > 2 ) {
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
    if ( gcmd_line.R && gcmd_line.debug > 2  ) {
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
      if ( gcmd_line.R && gcmd_line.debug > 2  ) {
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
      if ( gcmd_line.R && gcmd_line.debug > 2  ) {
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




  /* in any case, we need the state formula, to see whether or not the state
   * is a goal state. (enough goal likelyhood) somewhat wasteful, see above;
   * normally, we'd need the state formula only for -h 2.
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
  



  if ( gcmd_line.heuristic == 2 ) {
    /* we build impli graph only for the RPG, ie nothing much to build here!
     *
     * just build the U nodes for the initial RPG layer corresp. to S
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




  /* heuristic is 1 -- we need the path to s
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

    lnum_probU[time] = 0;
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
      if ( gcmd_line.R && gcmd_line.debug > 2 ) {
	printf("\npath inserted NOOP edge %d -> %d: ", time, time+1);
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
      ft = path[t]->F[i];
      if ( !Utpp[ft] ) continue;

      for ( nowUft = 0; nowUft < lnum_U[time+1]; nowUft++ ) {
	if ( lU[time+1][nowUft].ft == ft ) break;
      }
      if ( nowUft == lnum_U[time+1] ) {
	printf("can't find ft(t++) -- in rplan-nodel-noops -- in lU?\n\n");
	fflush(stdout);
	exit( 1 );
      }

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
      /* july06: if eff is prob, and it is *not* a common delete, then insert one
       * prob support node for every effect that does not delete the fact; else, switch
       * to previous treatment. 
       *
       * NOTE: we could probably make this somewhat more efficient by inserting just one node
       * with the weight set appropriately.. right now we don't have individual weights in the rest
       * of the code... skip that for now. wel maybe inserting such a node not corresponding
       * to an effect would cause trouble anyway.
       */
      if ( gef_conn[ef].eff_p < 1 ) {
	for ( k = 0; k < gef_conn[ef].num_SD; k++ ) {
	  if ( ft == gef_conn[ef].SD[k] ) break;
	}
	if ( k == gef_conn[ef].num_SD ) {
	  /* fact is not a common delete!
	   */
	  for ( j = 0; j < gef_conn[ef].num_S; j++ ) {
	    ef_ = gef_conn[ef].S[j];
	    for ( k = 0; k < gef_conn[ef_].num_D; k++ ) {
	      if ( ft == gef_conn[ef_].D[k] ) break;
	    }
	    if ( k == gef_conn[ef_].num_D ) {
	      iprobU = lprobU[time]->next;
	      for ( l = 0; l < lnum_probU[time]; l++ ) {
		if ( iprobU->ef == ef_ && iprobU->Utime == time ) break;
		iprobU = iprobU->next;
	      }
	      if ( l < lnum_probU[time] ) {
		probU = iprobU;
	      } else {
		if ( iprobU ) {
		  probU = iprobU;
		} else {
		  probU = ( UftNode * ) calloc(1, sizeof( UftNode ) );
		  probU->next = NULL;
		  lprobUend[time]->next = probU;
		  lprobUend[time] = probU;
		}

		lnum_probU[time]++;
		probU->ft = -1;
		probU->Utime = time;
		probU->ef = ef_;
		probU->num_in = 0;
		/* connect to the first unknown cond, if there is one!
		 */
		conu = -1;
		for ( c = 0; c < gef_conn[ef_].num_C; c++ ) {
		  if ( Ut[gef_conn[ef_].C[c]] ) break;
		}
		if ( c < gef_conn[ef].num_C ) {
		  for ( k = 0; k < lnum_U[time]; k++ ) {
		    if ( lU[time][k].ft ==  gef_conn[ef].C[c] ) break;
		  }
		  if ( k == lnum_U[time] ) {
		    printf("can't find con ft on rp path?\n\n");
		    fflush(stdout);
		    exit( 1 );
		  }
		  conu = k;
		  probU->in_edges[0] = &(lU[time][conu]);
		  probU->in_efs[0] = -1971;
		  probU->num_in = 1;
		  if ( 0 || (gcmd_line.R && gcmd_line.debug  > 2)) {
		    printf("\n1 path inserted condprop eff cond-to-p edge %d -> %d: ", time, time);
		    print_ft_name( lU[time][conu].ft );
		    printf(" -> ");
		    printf("ef %d of ", ef_);
		    print_op_name(gef_conn[ef_].op);
		    fflush(stdout);
		  }
		} /* endif there's an unknown condition */
	      } /* endelsif have to create the prob ef node */

	      lU[time+1][nowUft].in_edges[lU[time+1][nowUft].num_in] = probU;
	      lU[time+1][nowUft].in_efs[lU[time+1][nowUft].num_in] = -1971;/* this is our code for prob eff edge */
	      lU[time+1][nowUft].num_in++;

	      if ( 0 || (gcmd_line.R && gcmd_line.debug  > 2)) {
		printf("\npath inserted prob nondel NOOP edge %d -> %d: ", time, time+1);
		printf("ef %d of ", ef_);
		print_op_name(gef_conn[ef_].op);
		printf(" -> ");
		print_ft_name( ft );
		fflush(stdout);
	      }
	    }
	  } /* endfor j over the outcomes */

	  /* this skips to the next ft
	   */
	  continue;
	} /* endif we do the prob treatment */
      } /* endif eff p < 1 */ 

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
	/* we're ready. insert the edge!
	 */
	lU[time+1][nowUft].in_edges[lU[time+1][nowUft].num_in] = &(lU[time][Ucond]);
	lU[time+1][nowUft].in_efs[lU[time+1][nowUft].num_in] = -1;
	lU[time+1][nowUft].num_in++;
	if ( gcmd_line.R && gcmd_line.debug  > 2) {
	  printf("\npath inserted nondel NOOP edge %d -> %d: ", time, time+1);
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
   *
   * july06: if the effect is prob, then insert a prob node and edges instead.
   */
  for ( t = num_path + 1; t < MAX_PLAN_LENGTH; t++ ) {
    time = t - (num_path + 1);/* the time index of the lower state */
//     printf("\ntime %d -----------------------------", time);
//     lnum_probU[time] = 0;


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

//       printf("\nnum probU: %d -- ", lnum_probU[time]);
//       iprobU = lprobU[time]->next;
//       for ( l = 0; l < lnum_probU[time]; l++ ) {
// 	printf("\n%d: ef %d", l, iprobU->ef);
// 	iprobU = iprobU->next;
//       }

      
      if ( gef_conn[ef].eff_p < 1 ) {
	/* first, see if there's an unknown cond, and get it if it's there.
	 */
	conu = -1;
	for ( c = 0; c < gef_conn[ef].num_C; c++ ) {
	  if ( Ut[gef_conn[ef].C[c]] ) break;
	}
	if ( c < gef_conn[ef].num_C ) {
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
	}

	/* now, care about the adds.
	 * first, the non-common adds: real probabilistic.
	 */
	iprobU = lprobU[time]->next;
	for ( l = 0; l < lnum_probU[time]; l++ ) {
	  if ( iprobU->ef == ef && iprobU->Utime == time ) break;
	  iprobU = iprobU->next;
	}
	if ( l < lnum_probU[time] ) {
	  probU = iprobU;
	  if ( conu != -1 ) {
	    if ( probU->num_in != 1 ) {
	      printf("\ngot probU in != 1?\n\n");
	      exit( 1 );
	    }
	    if ( probU->in_edges[0] != &(lU[time][conu]) ) {
	      printf("\ngot probU different cond? numprobu %d time %d lpath_U_length %d, here ef %d cond ", 
		     lnum_probU[time], time, lpath_U_length, ef);
	      print_ft_name(gef_conn[ef].C[c]);print_ft_name(lU[time][conu].ft);
	      printf(", there ef %d cond ", probU->ef);
	      print_ft_name(probU->in_edges[0]->ft);
	      exit( 1 );
	    }
	  } else {
	    if ( probU->num_in != 0 ) {
	      printf("\ngot probU in != 0?\n\n");
	      exit( 1 );
	    }
	  }
	} else {
	  if ( iprobU ) {
	    probU = iprobU;
	  } else {
	    probU = ( UftNode * ) calloc(1, sizeof( UftNode ) );
	    probU->next = NULL;
	    lprobUend[time]->next = probU;
	    lprobUend[time] = probU;
	  }

	  lnum_probU[time]++;
	  probU->ft = -1;
	  probU->Utime = time;
	  probU->ef = ef;
	  probU->num_in = 0;

// 	  printf("\nnew probU ef %d, new num %d", ef, lnum_probU[time]);

// 	  printf("\nnow probU: %d -- ", lnum_probU[time]);
// 	  iprobU = lprobU[time]->next;
// 	  for ( l = 0; l < lnum_probU[time]; l++ ) {
// 	    printf("\n%d: ef %d", l, iprobU->ef);
// 	    iprobU = iprobU->next;
// 	  }

	  if ( conu != -1 ) {
	    probU->in_edges[0] = &(lU[time][conu]);
	    probU->in_efs[0] = -1971;
	    probU->num_in = 1;
	    if ( 0 || (gcmd_line.R && gcmd_line.debug  > 2)) {
	      printf("\n2 path inserted condprop eff cond-to-p edge %d -> %d: ", time, time);
	      print_ft_name( lU[time][conu].ft );
	      printf(" -> ");
	      printf("ef %d of ", ef);
	      print_op_name(gef_conn[ef].op);
	      fflush(stdout);
	    }
	  } /* endif there's an unknown condition */
	} /* endelsif have to create the prob ef node */

	for ( j = 0; j < gef_conn[ef].num_NSA; j++ ) {
	  if ( !Utpp[gef_conn[ef].NSA[j]] ) continue;/* known in next step */
	  /* edge U[time] -> U[time+1]; first find add node.
	   */
	  for ( k = 0; k < lnum_U[time+1]; k++ ) {
	    if ( lU[time+1][k].ft == gef_conn[ef].NSA[j] ) break;
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
	  lU[time+1][addu].in_edges[lU[time+1][addu].num_in] = probU;
	  lU[time+1][addu].in_efs[lU[time+1][addu].num_in] = -1971;/* this is our code for prob eff edge */
	  lU[time+1][addu].num_in++;
	  if ( 0 || (gcmd_line.R && gcmd_line.debug > 2)) {
	    printf("\npath inserted prob OP edge %d -> %d: ", time, time + 1);
	    printf("ef %d of ", ef);
	    print_op_name(gef_conn[ef].op);
	    printf(" -> ");
	    print_ft_name( gef_conn[ef].A[j] );
	    fflush(stdout);
	  }
	}/* endfor j over NSA of prob-ef */



	/* now, the common adds. those are treated exactly like the usual adds:
	 * in particular, if there is no unknown condition, then we expect 
	 * all of these guys to be known.
	 */
	if ( conu == -1 ) {
	  for ( j = 0; j < gef_conn[ef].num_SA; j++ ) {
	    if ( Utpp[gef_conn[ef].SA[j]] ) {
	      printf("\ncommon add of prob eff with no unknown cond is not known at time+1?\n\n");
	      exit( 1 );
	    }
	  }
	} else {
	  for ( j = 0; j < gef_conn[ef].num_SA; j++ ) {
	    if ( !Utpp[gef_conn[ef].SA[j]] ) continue;/* known in next step */
	    /* edge U[time] -> U[time+1]; first find add node.
	     */
	    for ( k = 0; k < lnum_U[time+1]; k++ ) {
	      if ( lU[time+1][k].ft == gef_conn[ef].SA[j] ) break;
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
	    if ( 0 || (gcmd_line.R && gcmd_line.debug > 2) ) {
	      printf("\npath inserted commonadd probeff OP edge %d -> %d: ", time, time + 1);
	      print_ft_name( lU[time][conu].ft );
	      printf(" -> ");
	      print_ft_name( lU[time+1][addu].ft );
	      fflush(stdout);
	    }
	  } /* endfor j over common effects */
	} /* endelseif there is an unknown cond, end treatment of common adds */


      } else {
	/* effect is non-prob, resort to previous treatment.
	 */
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
	  if ( 0 || (gcmd_line.R && gcmd_line.debug > 2) ) {
	    printf("\npath inserted OP edge %d -> %d: ", time, time + 1);
	    print_ft_name( lU[time][conu].ft );
	    printf(" -> ");
	    print_ft_name( lU[time+1][addu].ft );
	    fflush(stdout);
	  }
	}/* A of u-ef */
      } /* end elseif effect is non-prob */
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
Bool append_new_U_layer( int time )

{

  static Bool fc = TRUE;
  static int *Ups, *ce;

  int num_ce;
  int i, j, k, conu, addu, ft, ef, Utime, l;

  Bool new_U = FALSE;

  UftNode *probU, *iprobU;


  /* the state to be evaluated is in U layer
   * lpath_U_length - 1!!
   */
  Utime = time + lpath_U_length - 1;
  if ( Utime + 2 >= RELAXED_STEPS + MAX_PLAN_LENGTH ) {
    printf("\n\nU graph is too long! increase RELAXED_STEPS (now %d)\n\n",
	   RELAXED_STEPS );
    fflush(stdout);
    exit( 1 );
  }
  /* july06: this is cosmetics. just to make sure that there are no stray
   * rests of old RPGs behind the end of the current RPG.
   */
  lnum_U[Utime+2] = 0;

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
  lnum_probU[Utime] = 0;
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
    if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
      printf("\ncopied U fact");
      print_ft_name( ft );
      fflush(stdout);
    }
    lU[Utime+1][lnum_U[Utime+1]].ft = ft;
    lU[Utime+1][lnum_U[Utime+1]].became_F = FALSE;
    lU[Utime+1][lnum_U[Utime+1]].num_in = 0;
    /* the "noop" edge: added before the others, NOOPs first!
     * (important to have already excuted actions first in
     * impleaf backchaining, see below)
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





  /* july06: next, treat knownto-occur prob effects, as are now in lpE list.
   */
  if ( gcmd_line.R && gcmd_line.debug ) {
    printf("\nU entry knownprob effs:");
    for ( i = 0; i < lnum_pE; i++ ) {
      printf("\n%d of action ", lpE[i]);
      print_op_name(gef_conn[lpE[i]].op);
    }
    fflush(stdout);
  }
  /* for each such eff, create an U node that'll be in no lU layer;
   * for each of its non-common adds p, create an U node at t+1 if not already present;
   */
  for ( i = 0; i < lnum_pE; i++ ) {
    iprobU = lprobU[Utime]->next;
    for ( l = 0; l < lnum_probU[Utime]; l++ ) {
      if ( iprobU->ef == lpE[i] && iprobU->Utime == Utime ) break;
      iprobU = iprobU->next;
    }
    if ( l < lnum_probU[Utime] ) {
      printf("\nduplicate probnode in append U layer?\n\n");
      exit( 1 );
    }
    if ( iprobU ) {
      probU = iprobU;
    } else {
      probU = ( UftNode * ) calloc(1, sizeof( UftNode ) );
      probU->next = NULL;
      lprobUend[Utime]->next = probU;
      lprobUend[Utime] = probU;
    }
    
    lnum_probU[Utime]++;
    probU->ft = -1;
    probU->Utime = Utime;
    probU->ef = lpE[i];
    probU->num_in = 0;

    for ( j = 0; j < gef_conn[lpE[i]].num_NSA; j++ ) {
      ft = gef_conn[lpE[i]].NSA[j];
      if ( gft_conn[ft].in_F ) {
	continue;
      }
      for ( addu = 0; addu < lnum_U[Utime+1]; addu++ ) {
	if ( ft == lU[Utime+1][addu].ft ) break;
      }
      if ( addu == lnum_U[Utime+1] ) {
	/* this added ft is not registered unknown yet
	 */
	if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
	  printf("\nnew p U fact");
	  print_ft_name( ft );
	  fflush(stdout);
	}
	if ( lnum_U[Utime+1] == gmax_U ) {
	  printf("\n\ntoo many p U facts %d in rpg?\n\n", lnum_U[Utime+1]);
	  fflush(stdout);
	  exit( 1 );
	}
	lU[Utime+1][lnum_U[Utime+1]].ft = ft;
	lU[Utime+1][lnum_U[Utime+1]].became_F = FALSE;
	lU[Utime+1][lnum_U[Utime+1]].num_in = 0;
	lnum_U[Utime+1]++;
	new_U = TRUE;
      }
      if ( lU[Utime+1][addu].num_in == MAX_UEDGES ) {
	printf("\n\n1 too many added p U edges! increase MAX_UEDGES (now %d)\n\n",
	       MAX_UEDGES );
	exit( 1 );
      }
      lU[Utime+1][addu].in_edges[lU[Utime+1][addu].num_in] = probU;
      lU[Utime+1][addu].in_efs[lU[Utime+1][addu].num_in] = -1971;/* this is our code for prob eff edge */
      lU[Utime+1][addu].num_in++;
      if ( gcmd_line.R && gcmd_line.debug  > 2) {
	printf("\ninserted knownprop eff p edge %d -> %d: ", Utime, Utime+1);
	printf("ef %d of ", lpE[i]);
	print_op_name(gef_conn[lpE[i]].op);
	printf(" -> ");
	print_ft_name( ft );
	fflush(stdout);
      }
    } /* j over non-common adds */
  } /* i over lpE */




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
      if ( gcmd_line.R && gcmd_line.debug >= 3 ) {
	printf("\nbecame F, ignore");
      }
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
	/* july06: in case the eff is prob, is is IMPORTANT that all the outcomes use
	 * the same condition fact! we will rely on this! follows here implicitly since
	 * all the "C" arrays of the individual outcomes are identical -- have been *made*
	 * identical (ie all ordered in the same way) in inst_final.c .. !!
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

	/* july06: if ef is prob, insert (a fact node +) a prob edge; ...
	 */
	if ( gef_conn[ef].eff_p < 1 ) {
	  iprobU = lprobU[Utime]->next;
	  for ( l = 0; l < lnum_probU[Utime]; l++ ) {
	    if ( iprobU->ef == ef && iprobU->Utime == Utime ) break;
	    iprobU = iprobU->next;
	  }
	  if ( l < lnum_probU[Utime] ) {
	    printf("\nduplicate probnode in append U layer?\n\n");
	    exit( 1 );
	  }
	  if ( iprobU ) {
	    probU = iprobU;
	  } else {
	    probU = ( UftNode * ) calloc(1, sizeof( UftNode ) );
	    probU->next = NULL;
	    lprobUend[Utime]->next = probU;
	    lprobUend[Utime] = probU;
	  }
	  
	  lnum_probU[Utime]++;
	  probU->ft = -1;
	  probU->Utime = Utime;
	  probU->ef = ef;
	  probU->num_in = 0;

	  for ( k = 0; k < gef_conn[ef].num_NSA; k++ ) {
	    if ( gft_conn[gef_conn[ef].NSA[k]].in_F ) {
	      continue;
	    }
	    for ( addu = 0; addu < lnum_U[Utime+1]; addu++ ) {
	      if ( gef_conn[ef].NSA[k] == lU[Utime+1][addu].ft ) break;
	    }
	    if ( addu == lnum_U[Utime+1] ) {
	      /* this added ft is not registered unknown yet
	       */
	      if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
		printf("\nnew cond p U fact");
		print_ft_name( gef_conn[ef].NSA[k] );
		fflush(stdout);
	      }
	      if ( lnum_U[Utime+1] == gmax_U ) {
		printf("\n\ntoo many p U facts %d in rpg?\n\n", lnum_U[Utime+1]);
		fflush(stdout);
		exit( 1 );
	      }
	      lU[Utime+1][lnum_U[Utime+1]].ft = gef_conn[ef].NSA[k];
	      lU[Utime+1][lnum_U[Utime+1]].became_F = FALSE;
	      lU[Utime+1][lnum_U[Utime+1]].num_in = 0;
	      lnum_U[Utime+1]++;
	      new_U = TRUE;
	    }
	    if ( lU[Utime+1][addu].num_in == MAX_UEDGES ) {
	      printf("\n\n2 too many added p U edges! fact ");
	      print_ft_name(lU[Utime+1][addu].ft);
	      printf("at Utime %d increase MAX_UEDGES (now %d)\n\n", Utime+1, MAX_UEDGES );
	      exit( 1 );
	    }
	    lU[Utime+1][addu].in_edges[lU[Utime+1][addu].num_in] = probU;
	    lU[Utime+1][addu].in_efs[lU[Utime+1][addu].num_in] = -1971;/* this is our code for prob eff edge */
	    lU[Utime+1][addu].num_in++;
	    if ( gcmd_line.R && gcmd_line.debug  > 2) {
	      printf("\ninserted condprop eff p edge %d -> %d: ", Utime, Utime+1);
	      printf("ef %d of ", ef);
	      print_op_name(gef_conn[ef].op);
	      printf(" -> ");
	      print_ft_name( lU[Utime+1][addu].ft );
	      fflush(stdout);
	    }


	    /* ... plus an edge from the unknown cond, as below, to the prob node!
	     */
	    probU->in_edges[0] = &(lU[Utime][conu]);
	    probU->in_efs[0] = -1971;
	    probU->num_in = 1;
	    if ( gcmd_line.R && gcmd_line.debug  > 2) {
	      printf("\ninserted condprop eff cond-to-p edge %d -> %d: ", Utime, Utime);
	      print_ft_name( lU[Utime][conu].ft );
	      printf(" -> ");
	      printf("ef %d of ", ef);
	      print_op_name(gef_conn[ef].op);
	      fflush(stdout);
	    }
	  } /* k over non-common adds */

	  /* now take care of the *common* add effects
	   * connect those buggers to the unknown cond.
	   */
	  for ( k = 0; k < gef_conn[ef].num_SA; k++ ) {
	    if ( gft_conn[gef_conn[ef].SA[k]].in_F ) {
	      continue;
	    }
	    for ( addu = 0; addu < lnum_U[Utime+1]; addu++ ) {
	      if ( gef_conn[ef].SA[k] == lU[Utime+1][addu].ft ) break;
	    }
	    if ( addu == lnum_U[Utime+1] ) {
	      /* this added ft is not registered unknown yet
	       */
	      if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
		printf("\nnew cond p common add U fact");
		print_ft_name( gef_conn[ef].SA[k] );
		fflush(stdout);
	      }
	      if ( lnum_U[Utime+1] == gmax_U ) {
		printf("\n\ntoo many p U facts %d in rpg?\n\n", lnum_U[Utime+1]);
		fflush(stdout);
		exit( 1 );
	      }
	      lU[Utime+1][lnum_U[Utime+1]].ft = gef_conn[ef].SA[k];
	      lU[Utime+1][lnum_U[Utime+1]].became_F = FALSE;
	      lU[Utime+1][lnum_U[Utime+1]].num_in = 0;
	      lnum_U[Utime+1]++;
	      new_U = TRUE;
	    }

	    if ( lU[Utime+1][addu].num_in == MAX_UEDGES ) {
	      printf("\n\n3 too many added p U edges!");
	      print_ft_name(lU[Utime+1][addu].ft);
	      printf("increase MAX_UEDGES (now %d)\n\n",
		     MAX_UEDGES );
	      exit( 1 );
	    }
	    lU[Utime+1][addu].in_edges[lU[Utime+1][addu].num_in] = &(lU[Utime][conu]);
	    lU[Utime+1][addu].in_efs[lU[Utime+1][addu].num_in] = ef;
	    lU[Utime+1][addu].num_in++;
	    if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
	      printf("\nnew cond p common add U edge %d --> %d: ", Utime, Utime+1);
	      print_ft_name( lU[Utime][conu].ft );
	      printf(" --> ");
	      print_ft_name( lU[Utime+1][addu].ft );
	      fflush(stdout);
	    }
	  } /* k over non-common adds */

	  continue;
	} /* endif eff is prob */



	/* eff is non-prob.
	 */
	for ( k = 0; k < gef_conn[ef].num_A; k++ ) {
	  if ( gft_conn[gef_conn[ef].A[k]].in_F ) {
	    /* this add effect is known to be true already
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

    /* if a fact has only its NOOP as incoming edge, then it can't possibly
     * become true -- it would have become true earlier!
     */
    if ( lU[Utime+1][i].num_in == 1 ) {
      if ( lU[Utime+1][i].in_efs[0] == -1 ) {
	continue;
      }
    }


    /* july06: for a fact that does not appear in the goal nor in any
     * precondition/condition, we definitely don't want to
     * go to the trouble of checking if it becomes
     * true!! 
     *
     * NOTE: one could presumably also exploit this in other
     * places in here, to obtain (less strong, i suppose)
     * optimizations.
     */
    if ( !gft_conn[lU[Utime+1][i].ft].srelevant ) {
      continue;
    }



    if ( Uleaf_disjunction_implied_by_formula( Utime, &(lU[Utime+1][i]) ) ) {
      /* hand result over to the rest of the fixpoint algorithm!
       */
      lU[Utime+1][i].became_F = TRUE;
      new_fact( lU[Utime+1][i].ft );
      
      new_U = TRUE;

      if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
	printf("\n"); print_ft_name( lU[Utime+1][i].ft );
	printf(" became F!");
	fflush(stdout);
      }
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
    for ( i = prev_p; i < now_p; i++ ) {
      for ( j = 0; j < p[i]->num_in; j++ ) {
	/* see if we got this node already: avoid duplicates!
	 */
	for ( k = now_p; k < num_p; k++ ) {
	  if ( p[k] == p[i]->in_edges[j] ) break;
	}
	if ( k < num_p ) {
	  /* had this. skip it.
	   */
	  continue;
	}
	ft_ = p[i]->in_edges[j]->ft;
	/* july06: skip over prob nodes in here!
	 * go straight to son if there is one, or else skip.
	 */
	if ( ft_ == -1 ) {
	  if ( p[i]->in_edges[j]->num_in > 0 ) {
	    if ( p[i]->in_edges[j]->num_in != 1 ) {
	      printf("\nprob node with more than one son?\n\n");
	      exit( 1 );
	    }
	    for ( k = now_p; k < num_p; k++ ) {
	      if ( p[k] == p[i]->in_edges[j]->in_edges[0] ) break;
	    }
	    if ( k < num_p ) {
	      /* had this. skip it.
	       */
	      continue;
	    }
	    if ( num_p >= lMAX_PATHNODES ) {
	      printf("\n\nr 3 eins -- too many nodes on paths! increase lMAX_PATHNODES (now %d)\n\n",
		     lMAX_PATHNODES);
	      fflush(stdout);
	      exit( 1 );
	    }
	    p[num_p++] = p[i]->in_edges[j]->in_edges[0];
	    on_npath[p[i]->in_edges[j]->in_edges[0]->ft] = TRUE;
	  }
	  continue;
	} /* endif prob node */
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
    for ( i = prev_p; i < now_p; i++ ) {
      for ( j = 0; j < pp[i]->num_in; j++ ) {
	/* see if we got this node already: avoid duplicates!
	 */
	for ( k = now_p; k < num_pp; k++ ) {
	  if ( pp[k] == pp[i]->in_edges[j] ) break;
	}
	if ( k < num_pp ) {
	  /* had this. skip it.
	   */
	  continue;
	}
	ft_ = pp[i]->in_edges[j]->ft;
	/* july06: skip over prob nodes in here!
	 * go straight to son if there is one, or else skip.
	 */
	if ( ft_ == -1 ) {
	  if ( pp[i]->in_edges[j]->num_in > 0 ) {
	    if ( pp[i]->in_edges[j]->num_in != 1 ) {
	      printf("\nprob node with other than one son?\n\n");
	      exit( 1 );
	    }
	    if ( !on_npath[pp[i]->in_edges[j]->in_edges[0]->ft] ) {
	      if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
		printf(" --- new reachable %d", pp[i]->in_edges[j]->in_edges[0]->ft); 
		fflush(stdout);
		print_ft_name( pp[i]->in_edges[j]->in_edges[0]->ft );
		fflush(stdout);
	      }
	      for ( i = 0; i < num_p; i++ ) {
		on_npath[p[i]->ft] = FALSE;
	      }
	      return TRUE;
	    }
	    for ( k = now_p; k < num_pp; k++ ) {
	      if ( pp[k] == pp[i]->in_edges[j]->in_edges[0] ) break;
	    }
	    if ( k < num_pp ) {
	      /* had this. skip it.
	       */
	      continue;
	    }
	    if ( num_pp >= lMAX_PATHNODES ) {
	      printf("\n\nr 3 zwei -- too many nodes on paths! increase lMAX_PATHNODES (now %d)\n\n",
		     lMAX_PATHNODES);
	      fflush(stdout);
	      exit( 1 );
	    }
	    pp[num_pp++] = pp[i]->in_edges[j]->in_edges[0];
	  }
	  continue;
	} /* endif prob node */
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
  /* july06: these guys here list the prob eff outcomes 
   * that contribute to our fact; this will have an effect on
   * knwon truth of the fact iff for one time and effect, ALL
   * outcomes are leaves, in which case we know the fact is true
   * without further thinking. else, we just skip this info.
   */
  static int *probleafefs, *probleaftimes;
  /* july06: this guy here stores, in each iteration of the bwds breadth
   * first, which prob effs *with condition* we got here: this is needed to know
   * if or if not *all* outcomes of an effect are there to lead 
   * to a fact node in the next iteration. (see also below)
   */
  static int *current_condprobefs;
  static Bool *is_leaf;
  static UftNode_pointer *p;

  int num_p, prev_p, now_p, num_leafs;
  int i, j, k, l, t, ft, ft_;

  int num_probleafs, ef, ef_, num_current_condprobefs;

  if ( fc ) {
    leafs = ( int * ) calloc( gnum_ft_conn, sizeof( int ) );
    is_leaf = ( Bool * ) calloc( gnum_ft_conn, sizeof( Bool ) );
    p = ( UftNode_pointer * ) calloc( lMAX_PATHNODES, sizeof( UftNode_pointer ) );

    for ( i = 0; i < gnum_ft_conn; i++ ) {
      is_leaf[i] = FALSE;
    }

    /* july06: this is a hack! well I guess 1000 should be enough
     * most of the time.. ! double-check for overflows below.
     */
    probleafefs = ( int * ) calloc( 1000, sizeof( int ) );
    probleaftimes = ( int * ) calloc( 1000, sizeof( int ) );

    current_condprobefs = ( int * ) calloc( gnum_pef_conn, sizeof( int ) );

    fc = FALSE;
  }

  ft = n->ft;
  p[0] = n;
  now_p = 0;
  num_p = 1;
  num_leafs = 0;
  num_probleafs = 0;
  if ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
    printf("\nr starting at Utime %d + 1", Utime); print_ft_name( ft );
    fflush(stdout);
  }
  for ( t = Utime + 1; t > 0; t-- ) {
    /* include all edges from t to t-1
     */
    prev_p = now_p;
    now_p = num_p;
    num_current_condprobefs = 0;
    for ( i = prev_p; i < now_p; i++ ) {
      /* july06: the prob nodes are in this list; they are taken care of below;
       * don't expand them here.
       */
      if ( p[i]->ft == -1 ) {
	continue;
      }

      /* loop over all incoming edges of the node.
       */
      if ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
	printf("\nr i at t %d: ", t); print_ft_name( p[i]->ft );
	fflush(stdout);
      }
      for ( j = 0; j < p[i]->num_in; j++ ) {
	/* see if we got this node already: avoid duplicates!
	 */
	for ( k = now_p; k < num_p; k++ ) {
	  if ( p[k] == p[i]->in_edges[j] ) break;
	}
	if ( k < num_p ) {
	  /* had this. skip it.
	   */
	  continue;
	}
	if ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
	  /* july06: catch prob nodes.
	   */
	  if ( p[i]->in_edges[j]->ft >= 0 ) {
	    printf("\nr new p: "); print_ft_name( p[i]->in_edges[j]->ft );
	  } else {
	    printf("\nr new prob node, ef %d Utime %d of op ", 
		   p[i]->in_edges[j]->ef, p[i]->in_edges[j]->Utime);
	    print_op_name(gef_conn[p[i]->in_edges[j]->ef].op);
	  }
	  printf(" (by "); 
	  if ( p[i]->in_efs[j] == -1 ) {
	    printf("NOOP ");
	  } else {
	    if ( p[i]->in_efs[j] == -2 ) {
	      printf("OR constraint??\n\n");
	      fflush(stdout);
	      exit( 1 );
	    } else {
	      if ( p[i]->in_efs[j] == -1971 ) {
		printf("probnode edge");
	      } else {
		print_op_name( gef_conn[p[i]->in_efs[j]].op );
	      }
	    }
	  }
	  printf(")");
	  fflush(stdout);
	}

	if ( num_p >= lMAX_PATHNODES ) {
	  printf("\n\nr 3 drei -- too many nodes on paths! increase lMAX_PATHNODES (now %d)\n\n",
		 lMAX_PATHNODES);
	  fflush(stdout);
	  exit( 1 );
	}
	p[num_p++] = p[i]->in_edges[j];

	ft_ = p[i]->in_edges[j]->ft;

	if ( ft_ == -1 ) {
	  if ( p[i]->in_edges[j]->num_in > 0 ) {
	    /* july06: if this is a guy with a condition node, then
	     * ask if all other outcomes are already there; if yes, put that
	     * node into the list for propagation, if no, skip it
	     * completely ---> this effect will be considered to make
	     * the target fact true only if *all* its outcomes contribute!
	     */
	    if ( p[i]->in_edges[j]->num_in != 1 ) {
	      printf("\nprob node with more than one son?\n\n");
	      exit( 1 );
	    }

	    /* first see if the condition is already there anyway.
	     */
	    for ( k = now_p; k < num_p; k++ ) {
	      if ( p[k] == p[i]->in_edges[j]->in_edges[0] ) break;
	    }
	    if ( k < num_p ) {
	      /* had this. skip it.
	       */
	      continue;
	    }

	    ef = p[i]->in_edges[j]->ef;
	    if ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
	      printf("\nchecking colleagues of condprob eff %d at bwd t %d of ", ef, t);
	      print_op_name(gef_conn[ef].op);
	      printf("\nrecorded:");
	      for ( l = 0; l < num_current_condprobefs; l++ ) {
		printf("\n%d: ef %d of ", l, current_condprobefs[l]);
		print_op_name(gef_conn[current_condprobefs[l]].op);
	      }
	    }
	    for ( k = 0; k < gef_conn[ef].num_S; k++ ) {
	      if ( gef_conn[ef].S[k] == ef ) {
		continue;
	      }
	      for ( l = 0; l < num_current_condprobefs; l++ ) {
		if ( current_condprobefs[l] == gef_conn[ef].S[k] ) {
		  break;
		}
	      }
	      if ( l == num_current_condprobefs ) {
		/* found bad guy!
		 */
		break;
	      }
	    }
	    if ( k < gef_conn[ef].num_S ) {
	      /* not all outcomes there (yet?). just record this guy, and skip it for now.
	       */
	      if ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
		printf("\nbad guy: %d; recording condprob eff %d at bwd t %d of ", 
		       gef_conn[ef].S[k], ef, t);
		print_op_name(gef_conn[ef].op);
	      }
	      current_condprobefs[num_current_condprobefs++] = ef;
	      continue;
	    }

	    /* ok! this guy has all its colleagues present. propagate the condition 
	     * node!
	     */
	    if ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
	      printf("\nall colleagues of condprob eff %d at bwd t %d of ", ef, t);
	      print_op_name(gef_conn[ef].op);
	      printf(" are there! propagating to fact node ");
	      print_ft_name(p[i]->in_edges[j]->in_edges[0]->ft);
	    }
	    if ( num_p >= lMAX_PATHNODES ) {
	      printf("\n\nr 3 vier -- too many nodes on paths! increase lMAX_PATHNODES (now %d)\n\n",
		     lMAX_PATHNODES);
	      fflush(stdout);
	      exit( 1 );
	    }
	    p[num_p++] = p[i]->in_edges[j]->in_edges[0];

	    /* also, if we're already at bottom level, we gotta mark this
	     * as a leaf!
	     */
	    if ( t == 1 && !is_leaf[p[i]->in_edges[j]->in_edges[0]->ft] ) {
	      is_leaf[p[i]->in_edges[j]->in_edges[0]->ft] = TRUE;
	      leafs[num_leafs] = p[i]->in_edges[j]->in_edges[0]->ft;
	      if ( gcmd_line.R && gcmd_line.debug >= 7 ) {
		printf("\nr set is-l "); print_ft_name( p[i]->in_edges[j]->in_edges[0]->ft );
		fflush(stdout);
	      }
	    }

	    continue;
	  } /* endif has cond */

	  /* july06: else, collect into the probeff leafs.
	   */
	  if ( num_probleafs == 1000 ) {
	    printf("\ntoo many probleafs! increase ``1000'' in relax.cpp, Uleaf_disjunction_implied_by_formula\n\n");
	    exit( 1 );
	  }
	  probleafefs[num_probleafs] = p[i]->in_edges[j]->ef;
	  probleaftimes[num_probleafs] = p[i]->in_edges[j]->Utime;
	  num_probleafs++;
	  /* continue here so we don't get to the leafs handling below for this guy.
	   */
	  continue;
	}
	if ( t == 1 && !is_leaf[ft_] ) {
	  is_leaf[ft_] = TRUE;
	  leafs[num_leafs++] = ft_;
	  if ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
	    printf("\nr set is-l "); print_ft_name( ft_ );
	    fflush(stdout);
	  }
	}
      } /* in edges of node p[i] */
    } /* all p[i] in prev level */
  } /* all levels from top to bottom */

  if ( gcmd_line.R && gcmd_line.debug  >= 2 ) {
    printf("\nr fact "); print_ft_name( ft );
    printf(" has leafs:");
    for ( i = 0; i < num_leafs; i++ ) {
      printf(" ");print_ft_name( leafs[i] );
    }
    printf(" and prob leafs:");
    for ( i = 0; i < num_probleafs; i++ ) {
      printf(" (ef %d at Utime %d of action ", probleafefs[i], probleaftimes[i]);
      print_op_name(gef_conn[probleafefs[i]].op);
      printf(")");
    }
    printf("\nr now checking for implication by initial/state formula.");
    fflush(stdout);
  }



  /* july06: check if any effect/time is completely in here.
   *
   * this is rather naively implemented... perhaps try to speed up later..
   */
  for ( i = 0; i < num_probleafs; i++ ) {
    for ( j = 0; j < gef_conn[probleafefs[i]].num_S; j++ ) {
      ef_ = gef_conn[probleafefs[i]].S[j];
      for ( k = 0; k < num_probleafs; k++ ) {
	if ( probleafefs[k] == ef_ && probleaftimes[k] == probleaftimes[i] ) {
	  /* found good guy.
	   */
	  break;
	}
      }
      if ( k == num_probleafs ) {
	/* found no good guy
	 */
	break;
      }
    }
    if ( j == gef_conn[probleafefs[i]].num_S ) {
      /* found good guys for all those S
       */
      if ( gcmd_line.R && gcmd_line.debug  >= 2 ) {
	printf("\nr found op ");
	print_op_name(gef_conn[probleafefs[i]].op);
	printf(" with effect outcomes all leaves at %d. succeeed!", probleaftimes[i]);
      }
      for ( i = 0; i < num_leafs; i++ ) {
	is_leaf[leafs[i]] = FALSE;
      }
      return TRUE;

    }
  }



  /* july06: it might be that his guy comes exclusively out of prob effs,
   * except for its own NOOP, making itself its only leaf.
   * in that case we know that it is not implied! (since it is unknown in ini/the state)
   */
  if ( num_leafs == 1 && leafs[0] == ft ) {
    for ( i = 0; i < num_leafs; i++ ) {
      is_leaf[leafs[i]] = FALSE;
    }
    if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
      printf("\nr no complete probeff, is its only leaf --> bail out");
      fflush(stdout);
    }
    return FALSE;
  }



  if ( gcmd_line.heuristic == 1 ) {
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

  /* no pre-check if any "ini" or is contained in dis:
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
  gr_sat_calls++;
  sat = dp_CNF();
  gr_sat_time += gDP_time;
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



void reset_fixpoint( void )

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
  for ( i = 0; i < lnum_pE; i++ ) {
    gef_conn[lpE[i]].level = INFINITY;
  }

  for ( i = 0; i < lnum_ch_E; i++ ) {
    gef_conn[lch_E[i]].num_active_PCs = 0;
    gef_conn[lch_E[i]].ch = FALSE;
  }


  /* undo the clauses and encoding infos
   *
   * --- WHICH WE HAVE SET UP FOR THE GOAL SATISFACTION CHECK,
   *     AND/OR -h 2
   */
  for ( i = gnum_fixed_clauses; i < gnum_clauses; i++ ) {
    gclause_length[i] = 0;
  }
  gnum_clauses = gnum_fixed_clauses;
  for ( i = gnum_fixed_c + 1; i <= gnum_c; i++ ) {
    gcodes[gct[i]][gcf[i]] = -1;
  }

}




/* here's what we extend to probabilities for the RPG:
 * stop already if the goals are *likely* enough.
 */
Bool all_goals_activated( int time ) 

{

  int i;

  if ( gcmd_line.R && gcmd_line.debug >= 3 ) {
    printf("\nentry checking goals activation at %d", time);
    fflush(stdout);
  }

 if ( time == 0 ) {
   printf("\ntime == 0 in all-goals-activated??\n\n");
   exit( 1 );
 } 


  if ( ggoal_percent == 100 ) {
    /* at S level, goal plausibility was already tested.
     * proceed as previously.
     */
    for ( i = 0; i < lcurrent_goals.num_F; i++ ) {
      if ( !gft_conn[lcurrent_goals.F[i]].in_F ) {
	if ( gcmd_line.R && gcmd_line.debug >= 3 ) {
	  printf("\ntime %d, not all in F, bailing out", time);
	  fflush(stdout);
	}
	lthisgoalprob = 0.0;
	return FALSE;
      }
    }
    
    /* if goal fft has only just come in, it has its level,
     * as set in activate, not set yet. do that now.
     */
    for ( i = 0; i < lcurrent_goals.num_F; i++ ) {
      if ( gft_conn[lcurrent_goals.F[i]].level == INFINITY ) {
	gft_conn[lcurrent_goals.F[i]].level = time;
      }
    }

    if ( gcmd_line.R && gcmd_line.debug >= 3 ) {
      printf("\ntime %d, all in F, yes!", time);
      fflush(stdout);
    }
    
    lthisgoalprob = 1.0;
    return TRUE;
  }



  for ( i = 0; i < lcurrent_goals.num_F; i++ ) {
    if ( !gft_conn[lcurrent_goals.F[i]].in_F ) {
      break;
    }
  }
  if ( i == lcurrent_goals.num_F ) {
    for ( i = 0; i < lcurrent_goals.num_F; i++ ) {
      if ( gft_conn[lcurrent_goals.F[i]].level == INFINITY ) {
	gft_conn[lcurrent_goals.F[i]].level = time;
      }
    }
    if ( gcmd_line.R && gcmd_line.debug >= 3 ) {
      printf("\ntime %d, all in F, yes!", time);
      fflush(stdout);
    }
    lthisgoalprob = 1.0;
    return TRUE;
  }


  if ( gcmd_line.R && gcmd_line.debug >= 3 ) {
    printf("\ntime %d, not all in F, checking P", time);
    fflush(stdout);
  }

  return goals_likely_enough( time );

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
	/* july06: take the prob nodes.
	 */
	if ( lU[Utime][i].in_edges[j]->ft == -1 ) {
	  printf("(prob node, ef %d Utime %d of op ", 
		 lU[Utime][i].in_edges[j]->ef, lU[Utime][i].in_edges[j]->Utime);
	  print_op_name(gef_conn[lU[Utime][i].in_edges[j]->ef].op);
	  printf(")");
	} else {
	  print_ft_name( lU[Utime][i].in_edges[j]->ft );
	}
	printf(" (by "); 
	if ( lU[Utime][i].in_efs[j] == -1 ) {
	  printf("NOOP ");
	} else {
	  if ( lU[Utime][i].in_efs[j] == -2 ) {
	    printf("OR constraint");
	  } else {
	    if ( lU[Utime][i].in_efs[j] == -1971 ) {
	      printf("probnode edge");
	    } else {
	      print_op_name( gef_conn[lU[Utime][i].in_efs[j]].op );
	    }
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
	  /* july06: take the prob nodes.
	   */
	  if ( lU[Utime][i].in_edges[j]->ft == -1 ) {
	    printf("(prob node, ef %d Utime %d of op ", 
		   lU[Utime][i].in_edges[j]->ef, lU[Utime][i].in_edges[j]->Utime);
	    print_op_name(gef_conn[lU[Utime][i].in_edges[j]->ef].op);
	    printf(")");
	  } else {
	    print_ft_name( lU[Utime][i].in_edges[j]->ft );
	  }
	  printf(" (by "); 
	  if ( lU[Utime][i].in_efs[j] == -1 ) {
	    printf("NOOP ");
	  } else {
	    if ( lU[Utime][i].in_efs[j] == -2 ) {
	      printf("OR constraint");
	    } else {
	      if ( lU[Utime][i].in_efs[j] == -1971 ) {
		printf("probnode edge");
	      } else {
		print_op_name( gef_conn[lU[Utime][i].in_efs[j]].op );
	      }
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
  static int highest_seen;

  int i, j, time;

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

  /* part of the H info --- the path selected ops at time 0, namely ---
   * we are collecting along the way so we already initialze the stuff here.
   */
  for ( i = 0; i < gnum_H; i++ ) {
    gop_conn[gH[i]].is_in_H = FALSE;
  }
  gnum_H = 0;
  for ( i = 0; i < max + 1; i++ ) {
    lnum_goals_at[i] = 0;
  }
  reset_search_info();
  lh = 0;




  if ( gcmd_line.display_info == 123  || (gcmd_line.R && gcmd_line.debug) ) {
    printf("\n\n----------------------------------------relaxed plan");
    fflush(stdout);
  }



  /* set up goal arrays;
   * 
   * if required, this guy also invokes the P pre-selection!!!
   * put into the same fn because whether or not pre-selct is done
   * influences the form of the setup.
   */
  initialize_goals( max );

  if ( gcmd_line.display_info == 123  || (gcmd_line.R && gcmd_line.debug) ) {
    printf("\n\n----------------------------------------relaxed plan BACKCHAIN");
    fflush(stdout);
  }

  for ( time = max; time > 0; time-- ) {
    achieve_goals( time );
  }
  if ( H_info ) {
    collect_H_info( max );
  }

  return lh;

}



void initialize_goals( int max )

{

  int i, j, ft;
  int Utime = max + lpath_U_length - 1;

  Bool first;


  /* if probabilities matter, then do the pre-achievement of
   * unknown goal facts here, taking into 
   * account the probabilities.
   *
   * the rest of the machinery, below, is straight 100%
   * achieval.
   */

  /* this guy here will tell us if or if not
   * a pre-selection was done.
   *  
   * a priori, we assume that not.
   */
  l1P_preselection_done = FALSE;

  if ( ggoal_percent < 100 ) {
    if ( gcmd_line.R && gcmd_line.debug ) {
      printf("\n\nrequested goal P < 1, asking pre-select.");
    }
    preselect_P_actions( max );
  }


  first = TRUE;
  /* now do the actual goal preparation.
   */
  for ( i = 0; i < lcurrent_goals.num_F; i++ ) {
    ft = lcurrent_goals.F[i];

    /* if pre-select was done, then only insert the
     * known goal facts here -- the unknown ones
     * were dealt with before by preselection.
     */
    if ( l1P_preselection_done ) {
      for ( j = 0; j < lnum_U[Utime]; j++ ) {
	if ( lU[Utime][j].ft == ft ) break;
      }
      if ( j < lnum_U[Utime] ) {
	if ( gcmd_line.R && gcmd_line.debug >= 3 ) {
	  printf("\ninitialize goals, max %d, goal fact U: ", max);
	  print_ft_name( ft );
	  fflush(stdout);
	}
	continue;
      }
    }
    if ( gft_conn[ft].level == INFINITY ) {
      printf("\nngoal ft level infty at initialize?\n\n");
      exit( 1 );
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

}



void achieve_goals( int time )

{

  int i, j, k, ft, min_p, min_e, ef, p, op, Utime;
  static UftNode_pointer *tmpU;
  static Bool fc = TRUE;

  /* this stuff here is only to have the right input format for the
   * probabilistic support selection function.
   */
  if ( fc ) {
    tmpU = ( UftNode_pointer * ) calloc( 1, sizeof( UftNode_pointer ) );
    fc = FALSE;
  }


  Utime =  time + lpath_U_length - 1;

  if ( gcmd_line.display_info == 123  || (gcmd_line.R && gcmd_line.debug) ) {
    printf("\n--------------------------------------------achieve goals at %d", time);
    fflush(stdout);
  }

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
  Bool first;


  first = TRUE;
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
	printf("\n\nprec/cond ft ");
	print_ft_name(ft);
	printf(" for path ef %d neither in nor U at time %d -1, U %d??\n\n", 
	       ef, time, Utime);
	fflush(stdout);
	exit( 1 );
      }
    }
  }/* for all eff pre+conds */

  /* actually, this effect will only in part of our paths be executed (?!)
   * so we can and do not mark its add effects as true!
   */

}



void collect_H_info( int max )

{

  static Bool first_call = TRUE;
  static int *H, num_H, *D, *P, num_P;
  static double *valP;
  int i, j, k, ft, ef, op, d, ttt;
  double val;

  if ( first_call ) {
    H = ( int * ) calloc( gnum_op_conn, sizeof( int ) );
    P = ( int * ) calloc( gnum_op_conn, sizeof( int ) );
    valP = ( double * ) calloc( gnum_op_conn, sizeof( int ) );
    D = ( int * ) calloc( gnum_op_conn, sizeof( int ) );
    num_H = 0;
    num_P = 0;
    first_call = FALSE;
  }

  /* gnum_H and the is in H info have been initialized at 
   * rplan start already!
   */

  if ( gcmd_line.R && gcmd_line.debug ) {
    printf("\n--------------------------------- pre-collected H: ");
    for ( i = 0; i < gnum_H; i++ ) {
      print_op_name( gH[i] );
      printf("\n              ");
    }
    fflush(stdout);
  }

  /* first, remember the pre-H-selected ops.
   */  
  num_P = 0;
  for ( i = 0; i < gnum_H; i++ ) {
    P[num_P++] = gH[i];
  }
  /* don't update flags since all the ops will be out back in below
   * anyway; we do this only for the *order* of them.
   */
  gnum_H = 0;



  /* comment below is somewhat flawed... actually the Grid domain here was
   * flawed... still there the point is true: in this particular (even if pointless)
   * domain, the 1P is more optimistic than the RPG, which shouldn't happen from
   * a "clean" point of view... so let's do it...
   */


  /* modified definition!! in order to get applicable actions that are useful
   * ``further up'' in the relaxed plan!!
   *
   * thing is, since in RPG forward propagation we don't take account of
   * U facts with ``enough P weight'', an action may be in the relaxed plan
   * later than possible!
   * 
   * example: Grid; [k00, k01, k10]^{L0} [lock]^{L1} [goal k01]^{L2}
   * situation where k10 was picked up, unlock L1 tried, k10 exchanged for
   * k01: the probability of open(L1) is enough, still it appears only in the
   * fact layer 1 marked as "became_F"; 1P extraction gets it as goal at step 1,
   * and selects no action (ie, the unlock prior to the state at hand, [time-2]),
   * giving us empty helpful actions
   *
   * the adequate fix would be to integrate a P propagation/reasoning into the
   * forward propagation. Oh well, whatever... just do the simple thing right now...
   */


  /* now, collect the 1Pgoal helpfuls.
   */

  /* actually, for now, rather than introduce much more H actions,
   * just run the "risk" of missing something.
   *
   * july06: HACKY!! don't consider an action helpful because of an effect
   * that has only a 0.1 chance of succeeding.
   */
  num_H = 0;
  for ( ttt = 1; ttt <= 1; ttt++ ) {
    for ( i = 0; i < lnum_goals_at[ttt]; i++ ) {
      ft = lgoals_at[ttt][i];
      
      for ( j = 0; j < gft_conn[ft].num_A; j++ ) {
	ef = gft_conn[ft].A[j];
	if ( gef_conn[ef].level != 0 || gef_conn[ef].eff_p <= 0.1 ) {
	  continue;
	}
	op = gef_conn[ef].op;

	if ( gop_conn[op].is_in_H ) {
	  continue;
	}

	/* see if op is applicable
	 */
/* 	for ( k = 0; k < gop_conn[op].num_P; k++ ) { */
/* 	  if ( gft_conn[gop_conn[op].P[k]].level != 0 ) { */
/* 	    break; */
/* 	  } */
/* 	} */
/* 	if ( k < gop_conn[op].num_P ) { */
/* 	  continue; */
/* 	} */

	gop_conn[op].is_in_H = TRUE;
	H[num_H++] = op;
      }
    }
  }


  /* first put in the pre-selected ops, in an order reflecting
   * the parsed likelihood of the effect conditions.
   */  
  for ( i = 0; i < num_P; i++ ) {
    op = P[i];
    val = 0;
    ef = -1;
    for ( j = 0; j < gop_conn[op].num_E; j++ ) {
      ef = gop_conn[op].E[j];
      /* take max parsed_p of effect condition which is multivar in ini,
       * or 0
       */
      for ( k = 0; k < gef_conn[ef].num_C; k++ ) {
	ft = gef_conn[ef].C[k];
	if ( gft_conn[ft].parsed_weight > val ) {
	  val = gft_conn[ft].parsed_weight;
	}
      }
    }

    /* now simply insert by decreasing order of val.
     */
    for ( j = 0; j < gnum_H; j++ ) {
      if ( valP[j] < val ) break;
    }
    for ( k = gnum_H; k > j; k-- ) {
      gH[k] = gH[k-1];
      valP[k] = valP[k-1];
    }
    gH[j] = op;
    valP[j] = val;
    gnum_H++;
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
  /* july06: these guys here list the prob eff outcomes 
   * that contribute to our fact; this will have an effect on
   * knwon truth of the fact iff for one time and effect, ALL
   * outcomes are leaves, in which case we select the respective
   * paths. else, we just skip this info.
   */
  static int *probleafefs, *probleaftimes, *probleafps;
  /* july06: this guy here stores, in each iteration of the bwds breadth
   * first, which prob effs *with condition* we got here: this is needed to know
   * if or if not *all* outcomes of an effect are there to lead 
   * to a fact node in the next iteration. (see also below)
   */
  static int *current_condprobefs, *current_condprobps;
  static Bool *is_leaf;
  static UftNode_pointer *p;
  static int *selectp;
  /* a node may have several fathers! --> *every* father obtained
   * via a prob eff is taken: just one of those may not be enough to
   * obtain the desired relaxed plan.
   */
  static int **pfathers, *num_fathers;
  static int **pfatherefs, *pt, *leafp;

  int num_p, prev_p, now_p, num_leafs, num_min_leafs;
  int i, j, k, l, t, ft, ft_, jj, kk, ll;
  int Utime;

  int num_probleafs, ef, ef_, num_current_condprobefs, fathernodeindex, fatheref;
  double maxprob;
  int maxk, conditionpnode;



  if ( fc ) {
    leafs = ( int * ) calloc( gnum_ft_conn, sizeof( int ) );
    min_leafs = ( int * ) calloc( gnum_ft_conn, sizeof( int ) );
    is_leaf = ( Bool * ) calloc( gnum_ft_conn, sizeof( Bool ) );
    p = ( UftNode_pointer * ) calloc( lMAX_PATHNODES, sizeof( UftNode_pointer ) );
    selectp = ( int * ) calloc( lMAX_PATHNODES, sizeof( int ) );

    pfathers = ( int ** ) calloc( lMAX_PATHNODES, sizeof( int * ) );
    num_fathers = ( int * ) calloc( lMAX_PATHNODES, sizeof( int ) );
    pfatherefs = ( int ** ) calloc( lMAX_PATHNODES, sizeof( int * ) );
    pt = ( int * ) calloc( lMAX_PATHNODES, sizeof( int ) );
    leafp = ( int * ) calloc( lMAX_PATHNODES, sizeof( int ) );
    for ( j = 0; j < lMAX_PATHNODES; j++ ) {
      /* hack! double-check overflows below.
       */
      pfathers[j] = ( int * ) calloc( 50, sizeof( int ) );
      pfatherefs[j] = ( int * ) calloc( 50, sizeof( int ) );
    }

    for ( i = 0; i < gnum_ft_conn; i++ ) {
      is_leaf[i] = FALSE;
    }

    /* july06: this is a hack! well I guess 1000 should be enough
     * most of the time.. ! double-check for overflows below.
     */
    probleafefs = ( int * ) calloc( 1000, sizeof( int ) );
    probleaftimes = ( int * ) calloc( 1000, sizeof( int ) );
    probleafps = ( int * ) calloc( 1000, sizeof( int ) );

    current_condprobefs = ( int * ) calloc( gnum_pef_conn, sizeof( int ) );
    current_condprobps = ( int * ) calloc( gnum_pef_conn, sizeof( int ) );

    fc = FALSE;
  }

  Utime =  time + lpath_U_length - 1;

  ft = n->ft;
  p[0] = n;
  num_fathers[0] = 0;
  pt[0] = Utime;
  now_p = 0;
  num_p = 1;
  num_leafs = 0;
  num_probleafs = 0;
  if ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
    printf("\nstarting 1P select implied"); print_ft_name( ft );
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
    num_current_condprobefs = 0;
    for ( i = prev_p; i < now_p; i++ ) {
      /* july06: the prob nodes are in this list; they are taken care of below;
       * don't expand them here.
       */
      if ( p[i]->ft == -1 ) {
	continue;
      }

      /* loop over all incoming edges of the node.
       */
      if ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
	printf("\nrp i id %d at t %d: ", i, t); print_ft_name( p[i]->ft );
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
	for ( k = now_p; k < num_p; k++ ) {
	  if ( p[k] == p[i]->in_edges[j] ) break;
	}
	if ( k < num_p ) {
	  /* had this. skip it.
	   */
	  continue;
	}
	if ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
	  if ( p[i]->in_edges[j]->ft >= 0 ) {
	    printf("\nrp new p id %d: ", num_p); print_ft_name( p[i]->in_edges[j]->ft );
	  } else {
	    printf("\nrp new prob node id %d, ef %d Utime %d of op ", 
		   num_p, p[i]->in_edges[j]->ef, p[i]->in_edges[j]->Utime);
	    print_op_name(gef_conn[p[i]->in_edges[j]->ef].op);
	  }
	  printf(" (by "); 
	  if ( p[i]->in_efs[j] == -1 ) {
	    printf("NOOP ");
	  } else {
	    if ( p[i]->in_efs[j] == -2 ) {
	      printf("OR constraint??\n\n");
	      fflush(stdout);
	      exit( 1 );
	    } else {
	      if ( p[i]->in_efs[j] == -1971 ) {
		printf("probnode edge");
	      } else {
		print_op_name( gef_conn[p[i]->in_efs[j]].op );
	      }
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
	pfathers[num_p][0] = i;
	pfatherefs[num_p][0] = p[i]->in_efs[j];
	num_fathers[num_p] = 1;
	pt[num_p] = pt[i] - 1;
	num_p++;
	ft_ = p[i]->in_edges[j]->ft;
	/* july06: collect the probeffs.
	 */
	if ( ft_ == -1 ) {
	  if ( p[i]->in_edges[j]->num_in > 0 ) {
	    /* july06: if this is a guy with a condition node, then
	     * ask if all other outcomes are already there; if yes, put that
	     * node into the list for propagation, if no, skip it
	     * completely ---> this effect will be considered to make
	     * the target fact true only if *all* its outcomes contribute!
	     */
	    if ( p[i]->in_edges[j]->num_in != 1 ) {
	      printf("\nprob node with more than one son?\n\n");
	      exit( 1 );
	    }

	    /* first see if the condition is already there anyway.
	     */
	    for ( k = now_p; k < num_p; k++ ) {
	      if ( p[k] == p[i]->in_edges[j]->in_edges[0] ) break;
	    }
	    if ( k < num_p ) {
	      /* had this. skip it.
	       */
	      continue;
	    }

	    ef = p[i]->in_edges[j]->ef;
	    if ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
	      printf("\n1P: checking colleagues of condprob eff %d at bwd t %d of ", ef, t);
	      print_op_name(gef_conn[ef].op);
	      printf("\nrecorded:");
	      for ( l = 0; l < num_current_condprobefs; l++ ) {
		printf("\n%d: id %d, ef %d of ", l, current_condprobps[l], current_condprobefs[l]);
		print_op_name(gef_conn[current_condprobefs[l]].op);
	      }
	    }
	    for ( k = 0; k < gef_conn[ef].num_S; k++ ) {
	      if ( gef_conn[ef].S[k] == ef ) {
		continue;
	      }
	      for ( l = 0; l < num_current_condprobefs; l++ ) {
		if ( current_condprobefs[l] == gef_conn[ef].S[k] ) {
		  break;
		}
	      }
	      if ( l == num_current_condprobefs ) {
		/* found bad guy!
		 */
		break;
	      }
	    }
	    if ( k < gef_conn[ef].num_S ) {
	      /* not all outcomes there (yet?). just record this guy, and skip it for now.
	       */
	      if ( gcmd_line.R && gcmd_line.debug >= 7 ) {
		printf("\n1P: bad guy: %d; recording id %d condprob eff %d at bwd t %d of ", 
		       gef_conn[ef].S[k], num_p -1, ef, t);
		print_op_name(gef_conn[ef].op);
	      }
	      current_condprobefs[num_current_condprobefs] = ef;
	      current_condprobps[num_current_condprobefs] = num_p-1;
	      num_current_condprobefs++;
	      continue;
	    }

	    /* ok! this guy has all its colleagues present. propagate the condition 
	     * node! since this may be used in path selecting, we *replace*
	     * the created probeff node with its condition!
	     */



	    conditionpnode = -1;
	    if ( gcmd_line.alloutcomepaths ) {
	      /* give this condition node all the effect outcomes as fathers.
	       */

	      /* first, replace the last inserted probeff node with its son (the cond)
	       * arbitrary choice -- we could just as well insert a new node for
	       * the condition or replace any other of the prob eff nodes.
	       */
	      p[num_p-1] = p[i]->in_edges[j]->in_edges[0];
	      conditionpnode = num_p-1;

	      /* now set the fathers and fatherefs correctly.
	       */
	      num_fathers[num_p-1] = 0;
	      for ( k = 0; k < gef_conn[ef].num_S; k++ ) {
		/* setting the effect is easy.
		 */
		pfatherefs[num_p-1][num_fathers[num_p-1]] = gef_conn[ef].S[k];

		for ( l = 0; l < num_current_condprobefs; l++ ) {
		  if ( current_condprobefs[l] == gef_conn[ef].S[k] ) {
		    break;
		  }
		}
		if ( l < num_current_condprobefs ) {
		/* one of the previous guys. 
		 * the father is set to the father of the p node for the probeff!
		 */
		  if ( num_fathers[current_condprobps[l]] != 1 ) {
		    printf("\n\nprob eff node has %d != 1 fathers??\n\n", num_fathers[current_condprobps[l]]);
		    exit( 1 );
		  }
		  pfathers[num_p-1][num_fathers[num_p-1]] = pfathers[current_condprobps[l]][0];
		} else {
		  /* this is our current guy. the father is, simply, i
		   * note: we have in the meantime possible replaced the father information
		   * of this node so we cannot get it from there anymore... must
		   * do it this way.
		   */
		  pfathers[num_p-1][num_fathers[num_p-1]] = i;
		}
		num_fathers[num_p-1]++;
	      } /* endfor k over outcomes */
	      if ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
		printf("\n1P: replaced at t %d prob node id %d", t, num_p-1);
		printf(" with condition fact ");
		print_ft_name(p[i]->in_edges[j]->in_edges[0]->ft);
		printf(" and inserted all outcomes as fathers: ");
		for ( l = 0; l < num_fathers[num_p-1]; l++ ) {
		  printf("\nfather %d: ef %d of ", l, pfatherefs[num_p-1][l]);
		  print_op_name(gef_conn[pfatherefs[num_p-1][l]].op);
		  printf(" linked to pnode id %d, fact ", pfathers[num_p-1][l]);
		  print_ft_name(p[pfathers[num_p-1][l]]->ft);
		  fflush(stdout);
		}
	      }
	    } else {
	      /* all fathers not wanted. since this edge here will decide which effect is selected
	       * in support of the goal, select the outcome with the highest
	       * prob of his colleagues.
	       */
	      maxprob = -1;
	      maxk = -1;
	      for ( k = 0; k < gef_conn[ef].num_S; k++ ) {
		if ( maxk == -1 || gef_conn[gef_conn[ef].S[k]].eff_p > maxprob ) {
		  maxk = k;
		  maxprob = gef_conn[gef_conn[ef].S[k]].eff_p;
		}
	      }
	      for ( l = 0; l < num_current_condprobefs; l++ ) {
		if ( current_condprobefs[l] == gef_conn[ef].S[maxk] ) {
		  break;
		}
	      }
	      if ( l == num_current_condprobefs ) {
		/* the max guy is actually our current one...
		 */
		current_condprobps[l] = num_p-1;
	      }
	      /* replace the node of the max-p eff outcome with the condition.
	       * NOTE: all S effs here share the SAME condition!!
	       */
	      p[current_condprobps[l]] = p[i]->in_edges[j]->in_edges[0];
	      conditionpnode = current_condprobps[l];
	      /* pfather is correct ie remains the added fact of current_condprobps[l] */
	      /* insert the responsible effect, instead of the -1971 marking the probedge.
	       */
	      pfatherefs[current_condprobps[l]][0] = gef_conn[ef].S[maxk];
	      /* pt is also correct, already set to time(i)-1 */
	      if ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
		printf("\n1P: replaced at t %d prob node id %d ef %d of ", 
		       t, current_condprobps[l], gef_conn[ef].S[maxk]);
		print_op_name(gef_conn[gef_conn[ef].S[maxk]].op);
		printf(" with condition fact ");
		print_ft_name(p[i]->in_edges[j]->in_edges[0]->ft);
	      }
	    } /* endif want multiple fathers or not */




	    /* also, if we're already at bottom level, we gotta mark this
	     * as a leaf!
	     */
	    if ( t == 1 && !is_leaf[p[i]->in_edges[j]->in_edges[0]->ft] ) {
	      is_leaf[p[i]->in_edges[j]->in_edges[0]->ft] = TRUE;
	      leafs[num_leafs] = p[i]->in_edges[j]->in_edges[0]->ft;
	      if ( conditionpnode == -1 ) {
		printf("\n\nconditionpnode == -1?\n\n");
		exit( 1 );
	      }
	      leafp[num_leafs] = conditionpnode;
	      if ( gcmd_line.R && gcmd_line.debug >= 7 ) {
		printf("\nrp set is-l "); print_ft_name( p[i]->in_edges[j]->in_edges[0]->ft );
		fflush(stdout);
	      }
	    }

	    continue;
	  } /* endif has cond */

	  /* july06: else, collect into the probeff leafs.
	   */
	  if ( num_probleafs == 1000 ) {
	    printf("\ntoo many probleafs! increase ``1000'' in relax.cpp, Uleaf_disjunction_implied_by_formula\n\n");
	    exit( 1 );
	  }
	  probleafefs[num_probleafs] = p[i]->in_edges[j]->ef;
	  probleaftimes[num_probleafs] = p[i]->in_edges[j]->Utime;
	  probleafps[num_probleafs] = num_p-1;
	  num_probleafs++;
	  /* continue here so we don't get to the leafs handling below for this guy.
	   */
	  continue;
	}
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
    printf(" and prob leafs:");
    for ( i = 0; i < num_probleafs; i++ ) {
      printf(" (ef %d at Utime %d of action ", probleafefs[i], probleaftimes[i]);
      print_op_name(gef_conn[probleafefs[i]].op);
      printf(")");
    }
    printf("\nrp now checking for implication by initial/state formula.");
    fflush(stdout);
  }



  /* july06: check if any effect/time is completely in here.
   *
   * this is rather naively implemented... perhaps try to speed up later..
   */
  for ( i = 0; i < num_probleafs; i++ ) {
    for ( j = 0; j < gef_conn[probleafefs[i]].num_S; j++ ) {
      ef_ = gef_conn[probleafefs[i]].S[j];
      for ( k = 0; k < num_probleafs; k++ ) {
	if ( probleafefs[k] == ef_ && probleaftimes[k] == probleaftimes[i] ) {
	  /* found good guy.
	   */
	  break;
	}
      }
      if ( k == num_probleafs ) {
	/* found no good guy
	 */
	break;
      }
    }
    if ( j == gef_conn[probleafefs[i]].num_S ) {
      /* found good guys for all those S
       */
      if ( gcmd_line.R && gcmd_line.debug  >= 2 ) {
	printf("\nrp found op ");
	print_op_name(gef_conn[probleafefs[i]].op);
	printf(" with effect outcomes all leaves at %d. succeeed!", probleaftimes[i]);
      }

      /* now select the paths!
       */
      /* for each good guy...
       */
      for ( j = 0; j < gef_conn[probleafefs[i]].num_S; j++ ) {
	ef_ = gef_conn[probleafefs[i]].S[j];
	for ( k = 0; k < num_probleafs; k++ ) {
	  if ( probleafefs[k] == ef_ && probleaftimes[k] == probleaftimes[i] ) {
	    /* found good guy.
	     */
	    
	    /* ... select the guy itself.
	     */
	    pathselect( pt[probleafps[k]] + 1, ef_ );
	    /* ... and select the guys forming the paths to its father (a fact)
	     *
	     * just one guy here since, in this 100% bwd chaining, all facts
	     * must have "weight 1.0"
	     */
	    if ( num_fathers[probleafps[k]] != 1 ) {
	      printf("\nprob node with %d != 1 fathers??\n\n", num_fathers[probleafps[k]]);
	      exit( 1 );
	    }
	    fathernodeindex = pfathers[probleafps[k]][0];
	    selectp[0] = fathernodeindex;
	    now_p = 1;
	    jj = 0;
	    while ( jj < now_p ) {
	      for ( kk = 0; kk < num_fathers[selectp[jj]]; kk++ ) {
		fathernodeindex = pfathers[selectp[jj]][kk];
		fatheref = pfatherefs[selectp[jj]][kk];
		pathselect( pt[fathernodeindex], fatheref );
		for ( ll = 0; ll < now_p; ll++ ) {
		  if ( selectp[ll] == fathernodeindex ) break;
		}
		if ( ll < now_p ) continue;
		selectp[now_p] = fathernodeindex;
		now_p++;
	      }
	      jj++;
	    }
	    break;
	  }
	} /* k over probleafs */
      } /* j over outcomes of this guy */

      for ( i = 0; i < num_leafs; i++ ) {
	is_leaf[leafs[i]] = FALSE;
      }
      return;
    } /* found a complete peff */
  } /* end of look-for-peffs-loop */



  /* this is not implied by peffs! get the (minimized) impleafs.
   */
  if ( gcmd_line.heuristic == 1 ) {
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
  /* july06: also from prob eff outcomes, if their common condition was propagated,
   * we select just the father of the one outcome that finally triggered
   * the propagation; really, we would have to select many here...
   * well... this seems quite a blow to the code (?hm, just replace "father"
   * with an array..) anyway, probably not so important... usually one would expect
   * that the outcomes don't work together so much...?!
   */
  for ( i = 0; i < num_min_leafs; i++ ) {
    if ( leafs[min_leafs[i]] == ft ) {
      /* to ft, we have a path of noops; but this path might not have been
       * selected by backwards reasoning; we simply skip ft, to the same effect
       * as if we'd selected the noop edges leading to it.
       */
      continue;
    }

    selectp[0] = leafp[min_leafs[i]];
    now_p = 1;
    j = 0;
    while ( j < now_p ) {
      for ( k = 0; k < num_fathers[selectp[j]]; k++ ) {
	fathernodeindex = pfathers[selectp[j]][k];
	fatheref = pfatherefs[selectp[j]][k];
	pathselect( pt[fathernodeindex], fatheref );
	for ( l = 0; l < now_p; l++ ) {
	  if ( selectp[l] == fathernodeindex ) break;
	}
	if ( l < now_p ) continue;
	selectp[now_p] = fathernodeindex;
	now_p++;
      }
      j++;
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
      grp_sat_calls++;
      sat = dp_CNF();
      grp_sat_time += gDP_time;
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



/* july06: in these guys, don't need to look for probnodes since
 * these fns are (now) only called from places
 * where there aren't any probnodes inserted anyway.
 */
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
      printf("(%d)  ||  ", lr_clauses[i][j].time);
    } else {
      /* this is a code!
       */
      if ( lr_clauses[i][j].literal < 0 ) {
	printf("-");
	print_ft_name( lr_cf[(-1) * lr_clauses[i][j].literal] );
      } else {
	print_ft_name( lr_cf[lr_clauses[i][j].literal] );
      }
      printf("(0)  ||  ");
    }
  }

}



int r_extend_dynamic_clauses_base( State_pointer *path, int num_path, int *path_op )

{

  static Bool fc = TRUE;
  static Bool *Ft, *Ut, *Ftpp, *Utpp;
  /* july06: for each set of alternative e's, only 
   * 1 set of pefclauses. store for each effect if or if not
   * we already had those.
   */
  static Bool *have_pefclauses_for;
  
  int i, j, k, l, ef, t, time, ft, m, ff;
  int ef1, ef2;
  
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
    
    have_pefclauses_for = ( Bool * ) calloc( gnum_ef_conn, sizeof( Bool ) );
    
    fc = FALSE;
  }
  
  
  
  /* the states leading from current end-fixed-path-state to S are in 
   * path[num_path + 1] .. path[MAX_PLAN_LENGTH].
   * path_op[t] contains the op that leads into state [t]; we used to need this 
   * to check for noops, ie the case of uncond nondet effects;
   * leave in since you never know...
   * unknown efs leading to each state are stored in the state,
   * (also in dest already) so we
   * produce our CNF based on these.
   */
  if ( gcmd_line.R && gcmd_line.debug > 2 ) {
    printf("\nR dynamic CNF");
  }
  if ( gcmd_line.R && gcmd_line.debug > 2  ) {
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

    /* july06: initialize this memory.
     */
    for ( i = 0; i < path[t+1]->num_unknown_E; i++ ) {
      have_pefclauses_for[path[t+1]->unknown_E[i]] = FALSE;
    }

    for ( i = 0; i < path[t+1]->num_unknown_E; i++ ) {
      ef = path[t+1]->unknown_E[i];

      /* july06: if this is prob, and if not yet done, 
       * insert the pefclauses.
       *
       * I guess this might be superfluous sometimes, if there're
       * no actual relevant adds or deletes requiring these pef vars.
       * we could avoid by inserting this below only if needed.
       * don't bother right now.. it's cleaner like this. 
       */
      if ( gef_conn[ef].eff_p < 1 && !have_pefclauses_for[ef] ) {
	/* first, "at least one pef" (recall that S includes ef itself)
	 */
	if ( gnum_clauses == gmax_clauses ) {
	  printf("\n\ntoo many clauses? %d\n\n", gnum_clauses);
	  exit( 1 );
	}
	for ( k = 0; k < gef_conn[ef].num_S; k++ ) {
	  ef1 = gef_conn[ef].S[k];
	  
	  have_pefclauses_for[ef1] = TRUE;
	  
	  ff = gnum_ft_conn + gef_conn[ef1].pef_id;
	  gclauses[gnum_clauses][gclause_length[gnum_clauses]].literal = 1 + ff;
	  gclauses[gnum_clauses][gclause_length[gnum_clauses]++].time = time;
	}
	gnum_clauses++;
	if ( gcmd_line.R && gcmd_line.debug ) {
	  printf("\npef at least one clause"); print_clause( gnum_clauses-1 );
	}
	
	/* second, "at most one pef"
	 */
	for ( k = 0; k < gef_conn[ef].num_S-1; k++ ) {
	  ef1 = gef_conn[ef].S[k];
	  for ( l = k+1; l < gef_conn[ef].num_S; l++ ) {
	    ef2 = gef_conn[ef].S[l];
	    
	    if ( gnum_clauses == gmax_clauses ) {
	      printf("\n\ntoo many clauses? %d\n\n", gnum_clauses);
	      exit( 1 );
	    }
	    ff = gnum_ft_conn + gef_conn[ef1].pef_id;
	    gclauses[gnum_clauses][gclause_length[gnum_clauses]].literal = (-1) * (1 + ff);
	    gclauses[gnum_clauses][gclause_length[gnum_clauses]++].time = time;
	    ff = gnum_ft_conn + gef_conn[ef2].pef_id;
	    gclauses[gnum_clauses][gclause_length[gnum_clauses]].literal = (-1) * (1 + ff);
	    gclauses[gnum_clauses][gclause_length[gnum_clauses]++].time = time;
	    gnum_clauses++;
	    if ( gcmd_line.R && gcmd_line.debug ) {
	      printf("\npef at most one clause"); print_clause( gnum_clauses-1 );
	    }
	  }
	}
	
	/* third, "weight clauses"
	 */
	for ( k = 0; k < gef_conn[ef].num_S; k++ ) {
	  ef1 = gef_conn[ef].S[k];
	  
	  if ( gnum_clauses == gmax_clauses ) {
	    printf("\n\ntoo many clauses? %d\n\n", gnum_clauses);
	    exit( 1 );
	  }
	  
	  for ( l = 0; l < k; l++ ) {
	    ef2 = gef_conn[ef].S[l];
	    ff = gnum_ft_conn + gef_conn[ef2].pef_chance_id;
	    gclauses[gnum_clauses][gclause_length[gnum_clauses]].literal = 1 + ff;
	    gclauses[gnum_clauses][gclause_length[gnum_clauses]++].time = time;
	  }
	  if ( k < gef_conn[ef].num_S-1 ) {
	    ff = gnum_ft_conn + gef_conn[ef1].pef_chance_id;
	    gclauses[gnum_clauses][gclause_length[gnum_clauses]].literal = (-1) * (1 + ff);
	    gclauses[gnum_clauses][gclause_length[gnum_clauses]++].time = time;
	  }
	  ff = gnum_ft_conn + gef_conn[ef1].pef_id;
	  gclauses[gnum_clauses][gclause_length[gnum_clauses]].literal = 1 + ff;
	  gclauses[gnum_clauses][gclause_length[gnum_clauses]++].time = time;
	  gnum_clauses++;
	  if ( gcmd_line.R && gcmd_line.debug ) {
	    printf("\npef weight clause"); print_clause( gnum_clauses-1 );
	  }
	}
      } /* endif need to create pef clauses */
      
      /* for all non-true adds ad:
       * -u1(t) vee .. -un(t) vee ad(t+1)
       * where u1 .. un are the unknown conds of the ef.
       */
      for ( j = 0; j < gef_conn[ef].num_A; j++ ) {
	if ( !gft_conn[gef_conn[ef].A[j]].CNF ) continue;
	if ( Ftpp[gef_conn[ef].A[j]] ) continue;
	if ( gnum_clauses == gmax_clauses ) {
	  printf("\n\ntoo many clauses? %d\n\n", gnum_clauses);
	  exit( 1 );
	}
	/* july06: if this is prob, insert (neg pef var) as add. "condition".
	 */
	if ( gef_conn[ef].eff_p < 1 ) {
	  ff = gnum_ft_conn + gef_conn[ef].pef_id;
	  gclauses[gnum_clauses][gclause_length[gnum_clauses]].literal = (-1) * (1 + ff);
	  gclauses[gnum_clauses][gclause_length[gnum_clauses]++].time = time;
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
	if ( gcmd_line.R && gcmd_line.debug > 2  ) {
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
	if ( gnum_clauses == gmax_clauses ) {
	  printf("\n\ntoo many clauses? %d\n\n", gnum_clauses);
	  exit( 1 );
	}
	/* july06: if this is prob, insert (neg pef var) as add. "condition".
	 */
	if ( gef_conn[ef].eff_p < 1 ) {
	  ff = gnum_ft_conn + gef_conn[ef].pef_id;
	  gclauses[gnum_clauses][gclause_length[gnum_clauses]].literal = (-1) * (1 + ff);
	  gclauses[gnum_clauses][gclause_length[gnum_clauses]++].time = time;
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
	if ( gcmd_line.R && gcmd_line.debug > 2  ) {
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

































/**************************************************************
 * STUFF for probabilities in RPG, called from above.
 **************************************************************/


































/* this one is called at each RPG layer
 * time >= 1, to see if goal 
 * likelihood has reached the threshhold.
 */
Bool goals_likely_enough( int time )

{
  
  static Bool fc = TRUE;
  static UftNode_pointer *goalU;

  int i, j, goalft;
  int num_goalU;

  int Utime;

  if ( fc ) {
    goalU = ( UftNode_pointer * ) calloc( ggoal_state.num_F, sizeof( UftNode_pointer ) );
    fc = FALSE;
  }
    
  /* the time in terms of U nodes
   */
  Utime =  time + lpath_U_length - 1;


  if ( gcmd_line.R && gcmd_line.debug ) {
    printf("\ntime %d, entry checking P", time);
    fflush(stdout);
  }

  /* process the goals, and collect the Us; test if any is
   * known to be false.
   */
  num_goalU = 0;
  for ( i = 0; i < lcurrent_goals.num_F; i++ ) {
    goalft = lcurrent_goals.F[i];

    if ( gcmd_line.R && gcmd_line.debug ) {
      printf("\ntime %d, checking P, goal fact ", time);
      print_ft_name(goalft);
      fflush(stdout);
    }

    if ( gft_conn[goalft].in_F ) {
      /* true!
       */
      if ( gcmd_line.R && gcmd_line.debug >= 3 ) {
	printf("\ngoal ft true!");
	fflush(stdout);
      }
      if ( gft_conn[goalft].level == INFINITY ) {
	/* fucking dirty... have to set this here
	 * since it may be that a true fact has only just come in,
	 * and wasn't yet activated, which is when it gets its level
	 * set.
	 */
	gft_conn[goalft].level = time;
      }
      continue;
    }

    if ( gcmd_line.R && gcmd_line.debug >= 3 ) {
      printf("\ngoal ft not true!");
      fflush(stdout);
    }

    for ( j = 0; j < lnum_U[Utime]; j++ ) {
      if ( lU[Utime][j].ft == goalft ) break;
    }
    if ( j == lnum_U[Utime] ) {
      if ( gcmd_line.R && gcmd_line.debug ) {
	printf("\ntime %d, checking P, goal fact is false!", time);
	fflush(stdout);
      }
      lthisgoalprob = 0.0;
      return FALSE;
    }

    if ( gcmd_line.R && gcmd_line.debug ) {
      printf("\ngoal ft added into U!");
      fflush(stdout);
    }

    goalU[num_goalU++] = &(lU[Utime][j]);
  }

  if ( num_goalU == 0 ) {
    printf("\n\nno U goal facts selected in P check??\n\n");
    exit( 1 );
  }


  /* we got the U nodes. now collect all the implication leafs
   * and test them with initial / state formula.
   */

  if ( gcmd_line.R && gcmd_line.debug ) {
    printf("\ntime %d, checking P, all F or U. real P test!", time);
    fflush(stdout);
  }

  times( &end );
  TIME( geval_time );
  times( &start );

  if ( !Uleaf_disjunctions_supported_by_formula( Utime, goalU, num_goalU ) ) {
    if ( gcmd_line.R && gcmd_line.debug ) {
      printf("\ntime %d, P test failed!", time);
      fflush(stdout);
    }
    times( &end );
    TIME( gP_time );
    times( &start );
    return FALSE;
  }

  if ( gcmd_line.R && gcmd_line.debug ) {
    printf("\ntime %d, P test succeeded!", time);
    fflush(stdout);
  }
  lgoals_not_known = TRUE;
  times( &end );
  TIME( gP_time );
  times( &start );
  return TRUE;

}



Bool Uleaf_disjunctions_supported_by_formula( int Utime, UftNode_pointer *goalU, int num_goalU ) 

{

  static Bool fc = TRUE;
  static int **leafs;
  static int *num_leafs;
  static Bool **is_leaf;
  /* july06: this guy here contains the p list indices of the leafs.
   * needed to connect to the weight information.
   */
  static int **leafps;
  /* july06: these guys store the weight of the leafs from the 
   * implication graph propagation.
   *
   * we NEED that!!! the weigthts may change when the next goal is
   * propagated!!!!!!!!!!!
   */
  static double **leafweights;
  static double **probleafweights;
  /* july06: this guys here lists the indices of the uncond prob nodes
   * during our bwd traversal.
   */
  static int **probleafps;
  static int *num_probleafs;
  /* july06: this guy here stores, in each iteration of the bwds breadth
   * first, the indices of the prob effs *with condition* we got here: this is needed to know
   * if or if not *all* outcomes of an effect are there to lead 
   * to a fact node in the next iteration; it is also needed to implement
   * the weight propagation.
   */
  static int *current_condprobps;
  /* we'll need this guy to keep track of which eff leafs
   * we already took into account.
   */
  static Bool *haveweighted;
  /* july06: this here will store the aggregated "prob eff weight" for each goal:
   *         1 - \Pi_{probeff at time in leafs} (1-\sum_{outcome} p(outcome))
   */
  static double *aggregated_peff_weight;
  static UftNode_pointer **p;
  /* a node may have several fathers! 
   */
  static int ***pfathers, **num_fathers;
  static int ***pfatherefs;
  static int *multiID;
  
  int num_p, prev_p, now_p;
  int i, j, k, l, t, ft, ft_, gfti;

  /* stuff to see if every leaf set is a subset of one
   * multi var;
   * if so, and gindimulti is TRUE, then we can simply
   * read the likelihood off the parsed weights, and need no
   * WMC
   */
  double gprob = -1, curr_weight;
  int blaa;

  double weightsum;
  int ef, ef_, num_current_condprobs;

  UftNode *cpu;/* just a helper .. */
  Bool have_full_parents;

  double maxpeffweight;
  int maxpeffp;



  if ( fc ) {
    leafs = ( int ** ) calloc( ggoal_state.num_F, sizeof( int * ) );
    num_leafs = ( int * ) calloc( ggoal_state.num_F, sizeof( int ) );
    is_leaf = ( Bool ** ) calloc( gnum_ft_conn, sizeof( Bool * ) );
    leafps = ( int ** ) calloc( ggoal_state.num_F, sizeof( int * ) );
    leafweights = ( double ** ) calloc( ggoal_state.num_F, sizeof( double * ) );

    multiID = ( int * ) calloc( gnum_ft_conn, sizeof( Bool * ) );

    for ( i = 0; i < ggoal_state.num_F; i++ ) {
      leafs[i] = ( int * ) calloc( gnum_ft_conn, sizeof( int ) );
      is_leaf[i] = ( Bool * ) calloc( gnum_ft_conn, sizeof( Bool ) );
      for ( j = 0; j < gnum_ft_conn; j++ ) {
	is_leaf[i][j] = FALSE;
      }
      leafps[i] = ( int * ) calloc( gnum_ft_conn, sizeof( int ) );
      leafweights[i] = ( double * ) calloc( gnum_ft_conn, sizeof( double ) );
    }

    /* july06
     */
    probleafps = ( int ** ) calloc( ggoal_state.num_F, sizeof( int * ) );
    probleafweights = ( double ** ) calloc( ggoal_state.num_F, sizeof( double * ) );
    num_probleafs = ( int * ) calloc( ggoal_state.num_F, sizeof( int ) );
    for ( i = 0; i < ggoal_state.num_F; i++ ) {
      /* july06: this is a hack! well I guess 1000 should be enough
       * most of the time.. ! double-check for overflows below.
       */
      probleafps[i] = ( int * ) calloc( 1000, sizeof( int ) );
      probleafweights[i] = ( double * ) calloc( 1000, sizeof( double ) );
    }
    haveweighted = ( Bool * ) calloc( 1000, sizeof( Bool ) );

    aggregated_peff_weight = ( double * ) calloc( ggoal_state.num_F, sizeof( double ) );

    current_condprobps = ( int * ) calloc( gnum_pef_conn, sizeof( int ) );

    p = ( UftNode_pointer ** ) calloc( ggoal_state.num_F, sizeof( UftNode_pointer * ) );
    pfathers = ( int *** ) calloc( ggoal_state.num_F, sizeof( int ** ) );
    num_fathers = ( int ** ) calloc( ggoal_state.num_F, sizeof( int * ) );
    pfatherefs = ( int *** ) calloc( ggoal_state.num_F, sizeof( int ** ) );
    for ( i = 0; i < ggoal_state.num_F; i++ ) {
      p[i] = ( UftNode_pointer * ) calloc( lMAX_PATHNODES, sizeof( UftNode_pointer ) );
      pfathers[i] = ( int ** ) calloc( lMAX_PATHNODES, sizeof( int * ) );
      num_fathers[i] = ( int * ) calloc( lMAX_PATHNODES, sizeof( int ) );
      pfatherefs[i] = ( int ** ) calloc( lMAX_PATHNODES, sizeof( int * ) );
      for ( j = 0; j < lMAX_PATHNODES; j++ ) {
	/* hack! double-check overflows below.
	 */
	pfathers[i][j] = ( int * ) calloc( 50, sizeof( int ) );
	pfatherefs[i][j] = ( int * ) calloc( 50, sizeof( int ) );
      }
    }

    if ( gcmd_line.R && gcmd_line.debug >= 3 ) {
      printf("\nINITIALIZING frage P support in RPG");
      fflush(stdout);
    }

    fc = FALSE;
  }


  if ( gcmd_line.R && gcmd_line.debug >= 3 ) {
    printf("\nUtime %d, entry goal Uleaf disjunctions", Utime);
    fflush(stdout);
  }
  

  /* loop over all the given U goals.
   */
  for ( gfti = 0; gfti < num_goalU; gfti++ ) {
    ft = goalU[gfti]->ft;
    p[gfti][0] = goalU[gfti];
    num_fathers[gfti][0] = 0;
    now_p = 0;
    num_p = 1;
    num_leafs[gfti] = 0;
    num_probleafs[gfti] = 0;
    switch ( gcmd_line.weightprop ) {
    case 0:
      p[gfti][0]->weight = 1.0;
      break;
    case 1:
      /* july06: with independence assumption,
       * before fixing, the weights say how likewly it is that this guy does
       * NOT get to the goal.
       */
      p[gfti][0]->weight = 0.0;
      break;
    case 2:
      p[gfti][0]->weight = 1.0;
      break;
    }
    p[gfti][0]->has_full_parents = TRUE;/* marker for fixing the value: is allowed to be 1.0 */
    if ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
      printf("\nPr starting goal nr %d at Utime %d", gfti, Utime); print_ft_name( ft );
      fflush(stdout);
    }



    for ( t = Utime; t > 0; t-- ) {
      prev_p = now_p;
      now_p = num_p;
      num_current_condprobs = 0;



      for ( i = prev_p; i < now_p; i++ ) {

	if ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
	  if ( p[gfti][i]->ft != -1 ) {
	    printf("\nPr i at t %d: ", t); 
	    print_ft_name( p[gfti][i]->ft );
	  } else {
	    printf("\nef %d of ", p[gfti][i]->ef);
	    print_op_name(gef_conn[p[gfti][i]->ef].op);
	    printf(" at t %d", t);
	  }
	  fflush(stdout);
	}



	/* july06: the prob nodes are in this list; they are expanded below;
	 * don't expand them here.
	 */
	if ( p[gfti][i]->ft == -1 ) {
	  if ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
	    printf("\nskipping ef %d of ", p[gfti][i]->ef);
	    print_op_name(gef_conn[p[gfti][i]->ef].op);
	    printf(" at t %d", t);
	    fflush(stdout);
	  }
	  continue;
	}



	/* july06: fix the weight of this node, ie do down to
	 * its max capacity if needed, subtracting DELTA if it does 
	 * not have at least one full parent.
	 *
	 * "capacity": 1 for normal nodes, own eff weight for prob eff nodes.
	 * prob nodes are fixed in their own loop below, *before* they
	 * propagate condition nodes!
	 */
	if ( gcmd_line.weightprop == 1 ) { /* independence */
	  p[gfti][i]->weight = (1.0 - p[gfti][i]->weight);
	}
	if ( p[gfti][i]->weight >= 1.0 ) {
	  p[gfti][i]->weight = 1.0;
	  if ( !p[gfti][i]->has_full_parents ) {
	    p[gfti][i]->weight -= DELTA;
	  }
	}
	if ( gcmd_line.R && gcmd_line.debug  >= 6 ) {
	  printf("\nfixed weight of pnode %d ", i);
	  print_ft_name(p[gfti][i]->ft);
	  printf("at t %d to %lf", t, p[gfti][i]->weight);
	}



	for ( j = 0; j < p[gfti][i]->num_in; j++ ) {
	  ft_ = p[gfti][i]->in_edges[j]->ft;


	  /* SKIP THIS NODE IF ft_ APPEARS ON A PATH FROM p[i] TO THE GOAL NODE!
	   * otherwise we're trying to make ft_
	   * true through making it true in the 1st place!
	   * this makes only sense if the effect we're using for that is a NOOP.
	   * allow this case.
	   *
	   * (well I guess the real thing would be to test if ft_ appears on ALL paths
	   * from the ef to the goal... skip that for now...)  
	   *
	   * NOTE: this implementation is naive, chaining back from the ef node
	   * over his fathers. we could save time by book-keeping the sets of
	   * ancestor facts across the node. well, no big deal probably.
	   */
	  if ( gcmd_line.ancestorpruning ) {
	    if ( p[gfti][i]->in_efs[j] != -1 && 
		 ft_ != -1 && /* only do this with edges actually leading to a fact! */
		 is_ancestor(ft_, 
			     gfti, i,
			     pfathers, num_fathers, p) ) {	    
	      if ( gcmd_line.R && gcmd_line.debug >= 7 ) {
		printf("\nPr main loop ANCESTOR-SKIPPING father of fact %d ", ft_);
		fflush(stdout);
		print_ft_name(ft_);
		printf(" at t %d, father pnode would have been pnode %d, ", 
		       t, i);
		print_ft_name(p[gfti][i]->ft);
		fflush(stdout);
	      }
	      continue;
	    }
	  }



	  /* see if we got this node already: avoid duplicates and update weights!
	   */
	  for ( k = now_p; k < num_p; k++ ) {
	    if ( p[gfti][k] == p[gfti][i]->in_edges[j] ) break;
	  }
	  if ( k < num_p ) {
	    /* july06: had this. update the weight and fathers, and skip it.
	     */
	    if ( num_fathers[gfti][k] == 50 ) {
	      printf("\ntoo many fathers in wieght prop! increase 50 in Uleaf_disjunctions_supported_by_formula.\n\n");
	      exit( 1 );
	    }
	    pfathers[gfti][k][num_fathers[gfti][k]] = i;
	    pfatherefs[gfti][k][num_fathers[gfti][k]] = p[gfti][i]->in_efs[j];
	    num_fathers[gfti][k]++;
	    if ( gcmd_line.R && gcmd_line.debug >= 7 ) {
	      printf("\nPr main loop inserted father of pnode %d, ", k);
	      print_ft_name(p[gfti][k]->ft);
	      printf(" at t %d, father pnode is %d, ", t, i);
	      print_ft_name(p[gfti][i]->ft);
	    }



	    switch ( gcmd_line.weightprop ) {
	    case 0:
	      p[gfti][i]->in_edges[j]->weight += p[gfti][i]->weight;
	      break;
	    case 1:
	      p[gfti][i]->in_edges[j]->weight *= (1.0 - p[gfti][i]->weight);
	      break;
	    case 2:
	      if ( p[gfti][i]->weight > p[gfti][i]->in_edges[j]->weight ) {
		p[gfti][i]->in_edges[j]->weight = p[gfti][i]->weight;
	      }
	      break;
	    }
	    if ( p[gfti][i]->has_full_parents ) {
	      p[gfti][i]->in_edges[j]->has_full_parents = TRUE;
	    }
	    if ( gcmd_line.R && gcmd_line.debug >= 7 ) {
	      if ( ft_ != -1 ) {
		printf("\nupdated weight of pnode %d ", k);
		print_ft_name(ft_);
		printf("at t %d to %lf", t, p[gfti][i]->in_edges[j]->weight);
		if ( p[gfti][i]->has_full_parents ) {
		  if ( gcmd_line.R && gcmd_line.debug >= 7 ) {
		    printf("\nupdating, det: set full parents for pnode %d ", k);
		    print_ft_name(ft_);
		    printf("at t %d", t);
		  }
		}
	      } else {
		printf("\nupdating weight of pnode %d ef %d of ", k, p[gfti][i]->in_edges[j]->ef);
		print_op_name(gef_conn[p[gfti][i]->in_edges[j]->ef].op);
		printf("at t %d to %lf", t, p[gfti][i]->in_edges[j]->weight);
		if ( p[gfti][i]->has_full_parents ) {
		  p[gfti][i]->in_edges[j]->has_full_parents = TRUE;
		  if ( gcmd_line.R && gcmd_line.debug >= 7 ) {
		    printf("\nupdating, prob: set full parents for pnode %d ef %d of ", k, p[gfti][i]->in_edges[j]->ef);
		    print_op_name(gef_conn[p[gfti][i]->in_edges[j]->ef].op);
		    printf("at t %d", t);
		  }
		}
	      }
	    } /* endif debug printout */
	    continue;
	  } /* endif had this before */



	  /* now store the new node into our bwd list.
	   */
	  if ( num_p >= lMAX_PATHNODES ) {
	    printf("\n\nr P3 -- too many nodes on paths! increase lMAX_PATHNODES (now %d)\n\n",
		   lMAX_PATHNODES);
	    fflush(stdout);
	    exit( 1 );
	  }
	  p[gfti][num_p] = p[gfti][i]->in_edges[j];
	  pfathers[gfti][num_p][0] = i;
	  pfatherefs[gfti][num_p][0] = p[gfti][i]->in_efs[j];
	  num_fathers[gfti][num_p] = 1;
	  if ( gcmd_line.R && gcmd_line.debug >= 7 ) {
	    printf("\nPr main loop initialized fathers of pnode %d, ", num_p);
	    if ( p[gfti][num_p]->ft != -1 ) {
	      print_ft_name(p[gfti][num_p]->ft);
	    } else {
	      printf(" ef %d of ", p[gfti][num_p]->ef);
	      print_op_name(gef_conn[p[gfti][num_p]->ef].op);
	    }
	    printf(" at t %d, to father pnode %d, ", t, i);
	    print_ft_name(p[gfti][i]->ft);
	  }
	  num_p++;
	  /* initialize weight and parents flag.
	   */
	  switch ( gcmd_line.weightprop ) {
	  case 0:
	    p[gfti][i]->in_edges[j]->weight = p[gfti][i]->weight;
	    break;
	  case 1:
	    p[gfti][i]->in_edges[j]->weight = (1.0 - p[gfti][i]->weight);
	    break;
	  case 2:
	    p[gfti][i]->in_edges[j]->weight = p[gfti][i]->weight;
	    break;
	  }
	  p[gfti][i]->in_edges[j]->has_full_parents = p[gfti][i]->has_full_parents;



	  if ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
	    /* july06: catch prob nodes.
	     */
	    if ( ft_ >= 0 ) {
	      printf("\nPr new p: "); print_ft_name( ft_ );
	    } else {
	      printf("\nPr new prob pnode %d, ef %d Utime %d of op ", 
		     num_p-1, p[gfti][i]->in_edges[j]->ef, p[gfti][i]->in_edges[j]->Utime);
	      print_op_name(gef_conn[p[gfti][i]->in_edges[j]->ef].op);
	    }
	    printf(" (by "); 
	    if ( p[gfti][i]->in_efs[j] == -1 ) {
	      printf("NOOP ");
	    } else {
	      if ( p[gfti][i]->in_efs[j] == -2 ) {
		printf("OR constraint??\n\n");
		fflush(stdout);
		exit( 1 );
	      } else {
		if ( p[gfti][i]->in_efs[j] == -1971 ) {
		  printf("probnode edge");
		} else {
		  print_op_name( gef_conn[p[gfti][i]->in_efs[j]].op );
		}
	      }
	    }
	    printf(")");
	    fflush(stdout);
	  }
	  /* july06: more debug prints...
	   */
	  if ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
	    if ( ft_ != -1 ) {
	      printf("\ninitialized weight of pnode %d ", num_p-1);
	      print_ft_name(ft_);
	      printf("at t %d to %lf", t, p[gfti][i]->in_edges[j]->weight);
	      if ( p[gfti][i]->has_full_parents ) {
		printf("\ninitializing, det: set full parents for pnode %d ", num_p-1);
		print_ft_name(ft_);
		printf("at t %d", t);
	      }
	    } else {
	      ef = p[gfti][i]->in_edges[j]->ef;
	      printf("\ninitialized weight of pnode %d ef %d of ", num_p-1, ef);
	      print_op_name(gef_conn[ef].op);
	      printf("at t %d to %lf", t, p[gfti][i]->in_edges[j]->weight);
	      if ( p[gfti][i]->has_full_parents ) {
		if ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
		  printf("\ninitializing, prob: set full parents for pnode %d ef %d of ", num_p-1, ef);
		  print_op_name(gef_conn[ef].op);
		  printf("at t %d", t);
		}
	      }
	    }
	  } /* endif debug printout */



	  /* july06: treat the probeffs.
	   */
	  if ( ft_ == -1 ) {
	    ef = p[gfti][i]->in_edges[j]->ef;

	    /* for the cond prob effs, since they form an "intermediate layer",
	     * we'll have an extra loop after this iteration of t has finished.
	     * so just record these guys here.
	     */
	    if ( p[gfti][i]->in_edges[j]->num_in > 0 ) {
	      if ( p[gfti][i]->in_edges[j]->num_in != 1 ) {
		printf("\nprob node with more than one son?\n\n");
		exit( 1 );
	      }
	      current_condprobps[num_current_condprobs] = num_p-1;
	      num_current_condprobs++;
	      continue;
	    }

	    /* july06: if there are no conds then all we do is record this probleaf 
	     * for treatment below.
	     */
	    if ( num_probleafs[gfti] == 1000 ) {
	      printf("\ntoo many probleafs! increase ``1000'' in relax.cpp, Uleaf_disjunctions_supported_by_formul\n\n");
	      exit( 1 );
	    }
	    if  ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
	      printf("\ninclude into probleafs at t %d ef %d of ", t, ef);
	      print_op_name(gef_conn[ef].op);
	      printf("at t %d, pnode %d Utime %d", t, num_p-1, p[gfti][num_p-1]->Utime);
	      printf("\nIN pnode: include into probleafs at t %d ef %d of ", t, p[gfti][num_p-1]->ef);
	      print_op_name(gef_conn[p[gfti][num_p-1]->ef].op);
	      printf("at t %d, pnode %d Utime %d", t, num_p-1, p[gfti][num_p-1]->Utime);
	    }

	    probleafps[gfti][num_probleafs[gfti]] = num_p-1;
	    num_probleafs[gfti]++;

	    if ( p[gfti][num_p-1]->ft != -1 || p[gfti][num_p-1]->ef != ef ) {
	      printf("\nincorrect probleaf p reference?\n\n");
	      exit( 1 );
	    }

	    /* continue to not get to the leaf stuff below.
	     */
	    continue;
	  }



	  if ( t == 1 && !is_leaf[gfti][ft_] ) {
	    is_leaf[gfti][ft_] = TRUE;
	    leafs[gfti][num_leafs[gfti]] = ft_;
	    leafps[gfti][num_leafs[gfti]] = num_p-1;
	    num_leafs[gfti]++;
	    if ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
	      printf("\nPr set is-l "); print_ft_name( ft_ );
	      fflush(stdout);
	    }
	    /* july06: indimulti computation deferred to below, so
	     * we can take the (then finalized weights into account!!)
	     */
	  } /* endif ft_ is new leaf: end of leafs treatment */

	} /* endfor j over in edges of node p[gfti][i] */
      } /* endfor i over all p[gfti][i] in prev level */



      /* july06: after the fact nodes t have been expanded, we now expand the 
       * cond prob eff nodes that we have collected above.
       *
       * FIRST LOOP: FIX PROBNODE WEIGHTS AND INSERT THE NEW FACT NODES
       */
      for ( i = 0; i < num_current_condprobs; i++ ) {
	cpu = p[gfti][current_condprobps[i]];
	/* some debugging...
	 */
	if ( cpu->ft != -1 || cpu->num_in != 1 ) {
	  printf("\nft ");
	  print_ft_name(cpu->ft);
	  printf(" num in %d in condprob expansion?\n\n", cpu->num_in);
	  exit( 1 );
	}
	if ( gcmd_line.R && gcmd_line.debug >= 7 ) {
	  printf("\nPr 1st CPloop now doing pnode %d ef %d of ", current_condprobps[i], cpu->ef); 
	  print_op_name(gef_conn[cpu->ef].op);
	  fflush(stdout);
	}

	/* first, fix the weight of this bugger.
	 */
	if ( gcmd_line.weightprop == 1 ) { /* independence */
	  cpu->weight = (1.0 - cpu->weight);
	}
	cpu->weight *= gef_conn[cpu->ef].eff_p;
	if ( cpu->weight >= gef_conn[cpu->ef].eff_p ) {
	  cpu->weight = gef_conn[cpu->ef].eff_p;
	  if ( !cpu->has_full_parents ) {
	    cpu->weight -= DELTA;
	  }
	}
	if ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
	  printf("\n1st CPloop fixed weight of pnode %d, ef %d of ", current_condprobps[i], cpu->ef);
	  print_op_name(gef_conn[cpu->ef].op);
	  printf("at t %d to %lf", t, cpu->weight);
	}

	for ( k = now_p; k < num_p; k++ ) {
	  if ( p[gfti][k] == cpu->in_edges[0] ) break;
	}
	if ( k == num_p ) {
	  /* new guy!
	   * store the new node into our bwd list.
	   */
	  if ( num_p >= lMAX_PATHNODES ) {
	    printf("\n\nr P3 1st CPloop -- too many nodes on paths! increase lMAX_PATHNODES (now %d)\n\n",
		   lMAX_PATHNODES);
	    fflush(stdout);
	    exit( 1 );
	  }
	  p[gfti][num_p] = cpu->in_edges[0];
	  /* the fathers of this guy will be inserted in 2nd CPloop below!
	   */
	  num_fathers[gfti][num_p] = 0;
	  num_p++;

	  /* set weight to neutral element at this point;
	   * will be adapted in 2nd loop below.
	   */
	  switch ( gcmd_line.weightprop ) {
	  case 0:
	    cpu->in_edges[0]->weight = 0.0;
	    break;
	  case 1:
	    cpu->in_edges[0]->weight = 1.0;
	    break;
	  case 2:
	    cpu->in_edges[0]->weight = 0.0;
	    break;
	  }
	  /* this will be set to 1 iff for one effect all outcomes are
	   * fathers.
	   */
	  cpu->in_edges[0]->has_full_parents = FALSE;

	  if ( gcmd_line.R && gcmd_line.debug >= 7 ) {
	    printf("\nPr 1st CPloop new pnode %d p: ", num_p-1); 
	    print_ft_name( cpu->in_edges[0]->ft );
	    printf(" weight %lf coming from ef %d of ", cpu->in_edges[0]->weight, cpu->ef);
	    print_op_name(gef_conn[cpu->ef].op);
	    fflush(stdout);
	  }

	  if ( t == 1 && !is_leaf[gfti][cpu->in_edges[0]->ft] ) {
	    is_leaf[gfti][cpu->in_edges[0]->ft] = TRUE;
	    leafs[gfti][num_leafs[gfti]] = cpu->in_edges[0]->ft;
	    leafps[gfti][num_leafs[gfti]] = num_p-1;
	    num_leafs[gfti]++;
	    if ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
	      printf("\nPr 1st CPloop set is-l "); print_ft_name( cpu->in_edges[0]->ft );
	      fflush(stdout);
	    }
	  }

	} /* endif new guy */

      } /* endfor i over condprobs: end of 1st CPloop */



      /* 2nd LOOP: NOW LOOP OVER THE **FACT** NODES AND ADAPT
       * THEIR WEIGHT ACORDING TO THE PROBEFFS!!
       */
      for ( i = now_p; i < num_p; i++ ) {
	if ( p[gfti][i]->ft == -1 ) {
	  if ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
	    printf("\nPr 2nd CPloop skipping pnode %d, ef %d of ", i, p[gfti][i]->ef);
	    print_op_name(gef_conn[p[gfti][i]->ef].op);
	    printf(" at t %d", t);
	    fflush(stdout);
	  }
	  continue;
	}

	if ( gcmd_line.R && gcmd_line.debug >= 7 ) {
	  printf("\nPr 2nd CPloop now doing pnode %d fact ", i); 
	  print_ft_name(p[gfti][i]->ft);
	  fflush(stdout);
	}

	if ( gcmd_line.maxpeffather ) {
	  /* simply insert as only (additional) father the highest p_eff current condprob 
	   * leading into this fact!
	   */
	  maxpeffweight = -1;
	  maxpeffp = -1;
	  for ( j = 0; j < num_current_condprobs; j++ ) {
	    /* for explanation of the next 2 guys, see below...
	     */
	    if ( p[gfti][current_condprobps[j]]->in_edges[0] != p[gfti][i] ) {
	      continue;
	    }
	    if ( gcmd_line.ancestorpruning ) {
	      if ( is_ancestor(p[gfti][i]->ft, 
			       gfti, current_condprobps[j],
			       pfathers, num_fathers, p) ) {	    
		if ( gcmd_line.R && gcmd_line.debug >= 7 ) {
		  printf("\nPr 2nd CPloop ANCESTOR-SKIPPING father of pnode %d, ", i);
		  print_ft_name(p[gfti][i]->ft);
		  printf(" at t %d, father pnode would have been %d, ef %d of ", 
			 t, current_condprobps[j], p[gfti][current_condprobps[j]]->ef);
		  print_op_name(gef_conn[p[gfti][current_condprobps[j]]->ef].op);
		}
		continue;
	      }
	    }
	    
	    if ( maxpeffp == -1 || p[gfti][current_condprobps[j]]->weight > maxpeffweight ) {
	      maxpeffweight = p[gfti][current_condprobps[j]]->weight;
	      maxpeffp = current_condprobps[j];
	    }
	  } /* endfor j over current condprobs */
	  if ( maxpeffp == -1 ) {
	    continue;
	  }

	  pfathers[gfti][i][num_fathers[gfti][i]] = maxpeffp;
	  pfatherefs[gfti][i][num_fathers[gfti][i]] = p[gfti][maxpeffp]->ef;
	  num_fathers[gfti][i]++;
	  if ( gcmd_line.R && gcmd_line.debug >= 7 ) {
	    printf("\nPr 2nd CPloop inserted MAXPEF father of pnode %d, ", i);
	    print_ft_name(p[gfti][i]->ft);
	    printf(" at t %d, father pnode is %d, ef %d of ", 
		   t, maxpeffp, p[gfti][maxpeffp]->ef);
	    print_op_name(gef_conn[p[gfti][maxpeffp]->ef].op);
	  }

	  switch ( gcmd_line.weightprop ) {
	  case 0:
	    p[gfti][i]->weight += p[gfti][maxpeffp]->weight;
	    break;
	  case 1:
	    p[gfti][i]->weight *= (1.0 - p[gfti][maxpeffp]->weight);
	    break;
	  case 2:
	    if ( p[gfti][maxpeffp]->weight > p[gfti][i]->weight ) {
	      p[gfti][i]->weight = p[gfti][maxpeffp]->weight;
	    }
	    break;
	  }
	  if ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
	    printf("\n2nd CPloop MAXPEF updating weight of pnode %d ", i);
	    print_ft_name(p[gfti][i]->ft);
	    printf(" at t %d to %lf", t, p[gfti][i]->weight);
	  }

	  continue;
	} /* endif we want only the max eff_p pef father */


	/* do the full thing...
	 */
	if ( num_current_condprobs > 1000 ) {
	  printf("\ntoo many current condprobs. increase ``1000'' in relax.cpp, Uleaf_disjunctions_supported_by_formul");
	  exit( 1 );
	}
	for ( j = 0; j < num_current_condprobs; j++ ) {
	  haveweighted[j] = FALSE;
	}
	for ( j = 0; j < num_current_condprobs; j++ ) {
	  if ( haveweighted[j] ) {
	    /* we did this guy below already
	     */
	    continue;
	  }
	  if ( p[gfti][current_condprobps[j]]->in_edges[0] != p[gfti][i] ) {
	    /* this one does not connect to the cond we're currently
	     * considering
	     */
	    continue;
	  }

	  /* at this point, every father of this node will appear exactly once!
	   * insert as father --- BUT ONLY IF THE CONDITION NODE DOES NOT APPEAR
	   * ON A PATH OF THIS EF TO THE GOAL NODE! otherwise we're trying to make C
	   * true through making it true in the 1st place!
	   * (well I guess the real thing would be to test if C appears on ALL paths
	   * from the ef to the goal... skip that for now...)  
	   *
	   * NOTE: this implementation is naive, chaining back from the ef node
	   * over his fathers. we could sav time by book-keeping the sets of
	   * ancester facts across the node. well, no big deal probably.
	   */
	  if ( gcmd_line.ancestorpruning ) {
	    if ( is_ancestor(p[gfti][i]->ft, 
			     gfti, current_condprobps[j],
			     pfathers, num_fathers, p) ) {	    
	      if ( gcmd_line.R && gcmd_line.debug >= 7 ) {
		printf("\nPr 2nd CPloop ANCESTOR-SKIPPING father of pnode %d, ", i);
		print_ft_name(p[gfti][i]->ft);
		printf(" at t %d, father pnode would have been %d, ef %d of ", 
		       t, current_condprobps[j], p[gfti][current_condprobps[j]]->ef);
		print_op_name(gef_conn[p[gfti][current_condprobps[j]]->ef].op);
	      }
	      continue;
	    }
	  }

	  if ( num_fathers[gfti][i] == 50 ) {
	    printf("\ntoo many fathers in weight prop! increase 50 in Uleaf_disjunctions_supported_by_formula.\n\n");
	    exit( 1 );
	  }
	  pfathers[gfti][i][num_fathers[gfti][i]] = current_condprobps[j];
	  pfatherefs[gfti][i][num_fathers[gfti][i]] = p[gfti][current_condprobps[j]]->ef;
	  num_fathers[gfti][i]++;
	  if ( gcmd_line.R && gcmd_line.debug >= 7 ) {
	    printf("\nPr 2nd CPloop inserted father of pnode %d, ", i);
	    print_ft_name(p[gfti][i]->ft);
	    printf(" at t %d, father pnode is %d, ef %d of ", 
		   t, current_condprobps[j], p[gfti][current_condprobps[j]]->ef);
	    print_op_name(gef_conn[p[gfti][current_condprobps[j]]->ef].op);
	  }


	  weightsum = 0;
	  have_full_parents = TRUE;
	  for ( k = 0; k < gef_conn[p[gfti][current_condprobps[j]]->ef].num_S; k++ ) {
	    ef_ = gef_conn[p[gfti][current_condprobps[j]]->ef].S[k];
	    for ( l = 0; l < num_current_condprobs; l++ ) {
	      if ( p[gfti][current_condprobps[l]]->ef == ef_ ) {
		if ( p[gfti][current_condprobps[l]]->in_edges[0] != p[gfti][i] ) {
		  printf("\ncolleague does not connect to the same cond fact?? ef1 %d Utime %d time %d, cond ", 
			 p[gfti][current_condprobps[j]]->ef,
			 p[gfti][current_condprobps[j]]->Utime,
			 p[gfti][current_condprobps[j]]->Utime - lpath_U_length + 1);
		  print_ft_name(p[gfti][current_condprobps[l]]->in_edges[0]->ft);
		  printf(" ef2 %d Utime %d time %d, cond ", 
			 p[gfti][current_condprobps[l]]->ef,
			 p[gfti][current_condprobps[l]]->Utime,
			 p[gfti][current_condprobps[l]]->Utime - lpath_U_length + 1);
		  print_ft_name(p[gfti][i]->ft);
		  exit( 1 );
		}
		if ( p[gfti][current_condprobps[l]]->Utime != p[gfti][current_condprobps[j]]->Utime ) {
		  printf("\ncolleague is not at same Utime??\n\n");
		  exit( 1 );
		}
		/* found this guy!
		 */
		if ( gcmd_line.R && gcmd_line.debug >= 7 ) {
		  printf("\nPr 2nd CPloop pnode %d, ef %d of ", 
			 current_condprobps[l], ef_); 
		  print_op_name(gef_conn[ef_].op);
		  printf(" contributes %lf to weightsum", p[gfti][current_condprobps[l]]->weight);
		  fflush(stdout);
		}
		weightsum += p[gfti][current_condprobps[l]]->weight;
		haveweighted[l] = TRUE;/* in particular, this will weight "j" itself since that is in S */
		if ( !p[gfti][probleafps[gfti][l]]->has_full_parents ) {
		  have_full_parents = FALSE;
		}
		break;
	      }
	    } /* endfor l over the condprobs, trying to find this outcome */
	    if ( l == num_current_condprobs ) {
	      /* this outcome is not there! remember that.
	       */
	      have_full_parents = FALSE;
	    }
	  } /* endfor k over all the colleagues */

	  /* now weightsum contains the weight of all outcomes of
	   * this particular effect that are fathers here.
	   * adapt the weight.
	   */
	  switch ( gcmd_line.weightprop ) {
	  case 0:
	    p[gfti][i]->weight += weightsum;
	    break;
	  case 1:
	    p[gfti][i]->weight *= (1.0 - weightsum);
	    break;
	  case 2:
	    if ( weightsum > p[gfti][i]->weight ) {
	      p[gfti][i]->weight = weightsum;
	    }
	    break;
	  }
	  if ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
	    printf("\n2nd CPloop updating weight of pnode %d ", i);
	    print_ft_name(p[gfti][i]->ft);
	    printf(" at t %d to %lf", t, p[gfti][i]->weight);
	  }
	  if ( have_full_parents ) {
	    p[gfti][i]->has_full_parents = TRUE;
	    if ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
	      printf("\n2nd CPloop setting full parents of pnode %d ", i);
	      print_ft_name(p[gfti][i]->ft);
	      printf(" at t %d", t);
	    }
	  }
	} /* endfor j over the probeffs */

      } /* i over the new fact nodes; end of 2nd CPloop */

    } /* t over all levels from top to bottom */



    /* the weight of the fact guys in the last bwd layer have not been fixed yet!
     * do so now.
     */
    for ( i = now_p; i < num_p; i++ ) {
      if ( p[gfti][i]->ft != -1 ) {
	if ( gcmd_line.weightprop == 1 ) { /* independence */
	  p[gfti][i]->weight = (1.0 - p[gfti][i]->weight);
	}
	if ( p[gfti][i]->weight >= 1.0 ) {
	  p[gfti][i]->weight = 1.0;
	  if ( !p[gfti][i]->has_full_parents ) {
	    p[gfti][i]->weight -= DELTA;
	  }
	}
	if ( gcmd_line.R && gcmd_line.debug  >= 6 ) {
	  printf("\npost-fix-loop: fixed weight of pnode %d ", i);
	  print_ft_name(p[gfti][i]->ft);
	  printf("at t %d to %lf", t, p[gfti][i]->weight);
	}
      }
    } /* endfor i in post-fix-loop */



    /* next, fix the weight of the probleafs!
     * 
     * remember those weights!!! the weights stored in the p nodes
     * may change as the next goal is propagated!!!!!!!!!!!!!
     */
    for ( i = 0; i < num_probleafs[gfti]; i++ ) {
      if ( gcmd_line.weightprop == 1 ) { /* independence */
	p[gfti][probleafps[gfti][i]]->weight = (1.0 - p[gfti][probleafps[gfti][i]]->weight);
      }
      ef = p[gfti][probleafps[gfti][i]]->ef;
      p[gfti][probleafps[gfti][i]]->weight *= gef_conn[ef].eff_p;
      if ( p[gfti][probleafps[gfti][i]]->weight >= gef_conn[ef].eff_p ) {
	p[gfti][probleafps[gfti][i]]->weight = gef_conn[ef].eff_p;
	if ( !p[gfti][probleafps[gfti][i]]->has_full_parents ) {
	  p[gfti][probleafps[gfti][i]]->weight -= DELTA;
	}
      }
      if ( gcmd_line.R && gcmd_line.debug >= 7 ) {
	printf("\npost-fix-loop-probleafs: fixed weight of probleaf pnode %d ef %d of ", probleafps[gfti][i], ef);
	print_op_name(gef_conn[ef].op);
	printf(" to %lf", p[gfti][probleafps[gfti][i]]->weight);
      }
    }
//     if ( gcmd_line.maxpeffather && num_probleafs[gfti] > 0 ) {
//       /* the probleafs are "fathers" of our "final node!" so under this option,
//        * consider only one of them!!
//        */
//       maxpeffweight = -1;
//       maxpeffp = -1;
//       for ( i = 0; i < num_probleafs[gfti]; i++ ) {
// 	if ( maxpeffp == -1 || p[gfti][probleafps[gfti][i]]->weight > maxpeffweight ) {
// 	  maxpeffweight = p[gfti][probleafps[gfti][i]]->weight;
// 	  maxpeffp = probleafps[gfti][i];
// 	}
//       }
//       probleafps[gfti][0] = maxpeffp;
//       num_probleafs[gfti] = 1;
//     }


    /* remember the leaf weights!!!!!
     */
    for ( i = 0; i < num_leafs[gfti]; i++ ) {
      leafweights[gfti][i] = p[gfti][leafps[gfti][i]]->weight;
    }
    for ( i = 0; i < num_probleafs[gfti]; i++ ) {
      probleafweights[gfti][i] = p[gfti][probleafps[gfti][i]]->weight;
    }
  } /* gfti over all goal facts: bwd collect loop */




  /* takes exponential time in ,e.g., walk across grid
   */
//   /* if desired, compute the weights NOW --
//    * simply go over all paths from a pnode to the goal,
//    * taking the prob of each path and assuming the paths are all
//    * independent.
//    * do so for the leafs.
//    */
//   if ( gcmd_line.weightprop == 3 ) {
//     for ( gfti = 0; gfti < num_goalU; gfti++ ) {
//       for ( i = 0; i < num_probleafs[gfti]; i++ ) {
// 	get_pnode_pathsweight(gfti, probleafps[gfti][i], 
// 			      pfathers, pfatherefs, num_fathers, p);
//       }
//       for ( i = 0; i < num_leafs[gfti]; i++ ) {
// 	get_pnode_pathsweight(gfti, leafps[gfti][i], 
// 			      pfathers, pfatherefs, num_fathers, p);
//       }
//     }
//   }






  /* july06: next, compute the weight aggregated from
   * uncond prob effs, for each goal fact.
   */
  for ( gfti = 0; gfti < num_goalU; gfti++ ) {
    aggregated_peff_weight[gfti] = 1.0;
    for ( i = 0; i < num_probleafs[gfti]; i++ ) {
      haveweighted[i] = FALSE;
    }
    for ( i = 0; i < num_probleafs[gfti]; i++ ) {
      if ( haveweighted[i] ) {
	continue;
      }
      weightsum = 0;
      for ( j = 0; j < gef_conn[p[gfti][probleafps[gfti][i]]->ef].num_S; j++ ) {
	ef_ = gef_conn[p[gfti][probleafps[gfti][i]]->ef].S[j];
	for ( k = 0; k < num_probleafs[gfti]; k++ ) {
	  if ( p[gfti][probleafps[gfti][k]]->ef == ef_ && 
	       p[gfti][probleafps[gfti][k]]->Utime == p[gfti][probleafps[gfti][i]]->Utime ) {
	    /* found good guy.
	     */
	    weightsum += probleafweights[gfti][k];
	    haveweighted[k] = TRUE;/* in particular, this will weight "i" itself since that is in S */
	    break;
	  }
	}
      }

      aggregated_peff_weight[gfti] *= (1.0 - weightsum);
    }
    aggregated_peff_weight[gfti] = (1.0 - aggregated_peff_weight[gfti]);
    if ( aggregated_peff_weight[gfti] >= 1 ) {
      /* this shouldn't happen...
       */
//       aggregated_peff_weight[gfti] -= DELTA;
      printf("\nr aggregated weight for goal %d is %lf >= 1?\n\n", gfti, aggregated_peff_weight[gfti]);
      for ( i = 0; i < num_probleafs[gfti]; i++ ) {
	printf("\nleaf %d pnode %d: ef %d of ", i, probleafps[gfti][i], p[gfti][probleafps[gfti][i]]->ef);
	print_op_name(gef_conn[p[gfti][probleafps[gfti][i]]->ef].op);
	printf(" at Utime %d, weight %lf", p[gfti][probleafps[gfti][i]]->Utime, probleafweights[gfti][i]);
      }
      printf("\n\n");
      exit( 1 );
    }
  } /* gfti over all goal facts: compute aggregated noncond prob weight loop */










  /* july06: next, if indimulti stuff is wanted (cmd line) and possible (gindimulti, in particular h=1)
   * check if it applies here; if yes, compute the goal probability.
   */ 
  if ( gcmd_line.simulation_wmc && gindimulti ) {
    gprob = 1.0;
    for ( gfti = 0; gfti < num_goalU; gfti++ ) {
      multiID[gfti] = -1;
      curr_weight = 0.0;

      for ( i = 0; i < num_leafs[gfti]; i++ ) {
	if ( gft_conn[leafs[gfti][i]].parsed_weight < 0 ) {
	  /* bad guy! not part of a multivar.
	   */
	  break;
	}

	if ( multiID[gfti] == -1 ) {
	  multiID[gfti] = gft_conn[leafs[gfti][i]].multiID;
	  for ( j = 0; j < gfti; j++ ) {
	    if ( multiID[j] == multiID[gfti] ) {
	      /* bad guy! must be *different* multi vars for diff disjunctions!
	       */
	      break;
	    }
	  }
	  if ( j < gfti ) {
	    break;
	  }
	} /* endif see about multi ID */

	if ( gft_conn[leafs[gfti][i]].multiID != multiID[gfti] ) {
	  /* bad guy! all leafs must be from the same multi var.
	   */
	  break;
	}

	/* so far everything is groovy -- update the weight.
	 * take the parsed weight of this multivar value
	 * and directly multiply it with the implication graph weight.
	 */
	curr_weight += (gft_conn[leafs[gfti][i]].parsed_weight * leafweights[gfti][i]);
      } /* endfor i over the leafs of this goal fact */
      if ( i < num_leafs[gfti] ) {
	/* this leaf is a bad guy for some reason
	 */
	break;
      }

      /* looks good so far: update the gprob!
       */
      curr_weight = (1.0 - ((1.0 - curr_weight) * (1.0 - aggregated_peff_weight[gfti])));
      /* just a debugger...
       */
      blaa = ((int) (curr_weight * 1000000));
      if ( blaa > 1000000 ) {
	printf("\n\ncurr weight %f > 1.0??\n\n", curr_weight);
	exit( 1 );
      }
      gprob *= curr_weight;
    } /* gfti over all goal facts: indimulti loop */
    if ( gfti < num_goalU ) {
      gprob = -1;/* so we will know later that we could not compute the gprob... */
    }
  } /* endif do we want to, and can, do inidimulti? */






  /* final debug printout before decisions are taken...
   */
  if ( gcmd_line.R && gcmd_line.debug  >= 2 ) {
    for ( gfti = 0; gfti < num_goalU; gfti++ ) {
      printf("\nPr goal fact "); print_ft_name( goalU[gfti]->ft );
      printf(" has leafs:");
      for ( i = 0; i < num_leafs[gfti]; i++ ) {
	printf(" (");
	print_ft_name( leafs[gfti][i] );
	printf(" weight %lf)", leafweights[gfti][i]);
      }
      printf(" and noncond prob leafs:");
      for ( i = 0; i < num_probleafs[gfti]; i++ ) {
	printf(" (ef %d at Utime %d of action ", p[gfti][probleafps[gfti][i]]->ef, p[gfti][probleafps[gfti][i]]->Utime);
	print_op_name(gef_conn[p[gfti][probleafps[gfti][i]]->ef].op);
	printf(" weight %lf)", probleafweights[gfti][i]);
      }
      printf(" giving the aggregated weight %lf", aggregated_peff_weight[gfti]);
      printf("\nr now checking for support by initial/state formula.");
      fflush(stdout);
    }

    printf("\nindimulti wanted? %d possible? %d gprob: %lf", 
	   gcmd_line.simulation_wmc, gindimulti, gprob);
  }






  /* july06: if there are no leafs at all, just probleafs, then we can simply 
   * compute the outcome prob.
   *
   * Actually this is just a special case of simply collecting the factor
   * for every goal whose disj is empty. don't bother... (will be a unit clause for wmc, so..)
   */
  for ( gfti = 0; gfti < num_goalU; gfti++ ) {
    if ( num_leafs[gfti] > 0 ) {
      break;
    }
  }
  if ( gcmd_line.simulation_wmc && gfti == num_goalU ) {
    for ( gfti = 0; gfti < num_goalU; gfti++ ) {
      for ( i = 0; i < num_leafs[gfti]; i++ ) {
	is_leaf[gfti][leafs[gfti][i]] = FALSE;
      }
    }
    /* this is a special case of the above indimulti computation; 
     * however, that computation is only done if indimulti is possible
     * and applies, so repeat it here.
     */
    gprob = 1.0;
    for ( gfti = 0; gfti < num_goalU; gfti++ ) {
      gprob *= aggregated_peff_weight[gfti];
    }    
    if ( gcmd_line.R && gcmd_line.debug >= 1 ) {
      printf("\nr only-probleafs treatment applies, computed P %f", gprob);
    }
    gsim_wmc_calls++;
    lthisgoalprob = gprob;
    if ( gprob >= ggoal_probability ) {
      if ( gcmd_line.R && gcmd_line.debug >= 1 ) {
	printf(" -- high enough!");
      }
      return TRUE;
    } else {
      if ( gcmd_line.R && gcmd_line.debug >= 1 ) {
	printf(" -- too low!");
      }
      return FALSE;
    }
  }



  /* if we can (and this is wanted), just use the indimulti information
   * instead of WMC!!
   */
  if ( gcmd_line.simulation_wmc && gindimulti && gprob != -1 ) {
    if ( gcmd_line.R && gcmd_line.debug >= 1 ) {
      printf("\nr indi multi treatment applies, computed P %f", gprob);
    }
    gsim_wmc_calls++;
    for ( gfti = 0; gfti < num_goalU; gfti++ ) {
      for ( i = 0; i < num_leafs[gfti]; i++ ) {
	is_leaf[gfti][leafs[gfti][i]] = FALSE;
      }
    }	
    /* july06: remember the actual prob!!
     */
    lthisgoalprob = gprob;
    if ( gprob >= ggoal_probability ) {
      if ( gcmd_line.R && gcmd_line.debug >= 1 ) {
	printf(" -- high enough!");
      }
      return TRUE;
    } else {
      if ( gcmd_line.R && gcmd_line.debug >= 1 ) {
	printf(" -- too low!");
      }
      return FALSE;
    }
  }
  

  if ( gcmd_line.heuristic == 1 ) {
    if ( disjunctions_supported_by_initial_formula( leafs, num_leafs, is_leaf, leafweights, 
						    aggregated_peff_weight, num_goalU ) ) {
      for ( gfti = 0; gfti < num_goalU; gfti++ ) {
	for ( i = 0; i < num_leafs[gfti]; i++ ) {
	  is_leaf[gfti][leafs[gfti][i]] = FALSE;
	}
      }
      return TRUE;
    }
  }
  if ( gcmd_line.heuristic == 2 ) {
    if ( disjunctions_supported_by_S_formula( leafs, num_leafs, is_leaf, leafweights, 
					      aggregated_peff_weight, num_goalU ) ) {
      for ( gfti = 0; gfti < num_goalU; gfti++ ) {
	for ( i = 0; i < num_leafs[gfti]; i++ ) {
	  is_leaf[gfti][leafs[gfti][i]] = FALSE;
	}
      }
      return TRUE;
    }
  }

  for ( gfti = 0; gfti < num_goalU; gfti++ ) {
    for ( i = 0; i < num_leafs[gfti]; i++ ) {
      is_leaf[gfti][leafs[gfti][i]] = FALSE;
    }
  }

  return FALSE;

}



/* does fact ft appear on a path backwards from pnode,
 * in the graph given by gfti in pfathers?
 */
Bool is_ancestor( int ft, 
		  int gfti, int pnode, 
		  int ***pfathers, int **num_fathers, UftNode_pointer **p )

{

  static Bool fc = TRUE;
  static int *pathp;

  int i, j, k, fathernodeindex, now_p;

  if ( fc ) {
    pathp = ( int * ) calloc( lMAX_PATHNODES, sizeof( int ) );
    fc = FALSE;
  }

  for ( i = 0; i < num_fathers[gfti][pnode]; i++ ) {
    fathernodeindex = pfathers[gfti][pnode][i];
    pathp[i] = fathernodeindex;
  }
  now_p = num_fathers[gfti][pnode];
  i = 0;
  while ( i < now_p ) {
    if ( p[gfti][pathp[i]]->ft == ft ) {
      return TRUE;
    }
    for ( j = 0; j < num_fathers[gfti][pathp[i]]; j++ ) {
      fathernodeindex = pfathers[gfti][pathp[i]][j];
      for ( k = 0; k < now_p; k++ ) {
	if ( pathp[k] == fathernodeindex ) break;
      }
      if ( k < now_p ) continue;
      pathp[now_p] = fathernodeindex;
      now_p++;
    }
    i++;
  }

  return FALSE;

}



// /* recursively walk through all paths from pnode,
//  * and accumulate the weight.
//  */
// void get_pnode_pathsweight( int gfti, int pnode,
// 			    int ***pfathers, int ***pfatherefs, int **num_fathers, 
// 			    UftNode_pointer **p )

// {

//   lpathsweight = 1.0;

//   rec_get_pnode_pathsweight( 1.0, gfti, pnode, pfathers, pfatherefs, num_fathers );

//   p[gfti][pnode]->weight = (1.0 - lpathsweight);
//   if ( p[gfti][pnode]->ft == -1 ) {
//     p[gfti][pnode]->weight *= gef_conn[p[gfti][pnode]->ef].eff_p;
//   }

//   if ( gcmd_line.R && gcmd_line.debug >= 7 ) {
//     printf("\nRp PATHSWEIGHT: setting weight of pnode %d, ", pnode);
//     if ( p[gfti][pnode]->ft != -1 ) {
//       print_ft_name(p[gfti][pnode]->ft);
//     } else {
//       printf("ef %d of ", p[gfti][pnode]->ef);
//       print_op_name(gef_conn[p[gfti][pnode]->ef].op);
//     }
//     printf(" to %lf.", p[gfti][pnode]->weight);
//   }

// }



// void rec_get_pnode_pathsweight( double currentweight, 
// 				int gfti, int pnode,
// 				int ***pfathers, int ***pfatherefs, int **num_fathers )

// {

//   int i;

//   if ( num_fathers[gfti][pnode] == 0 ) {
//     if ( gcmd_line.R && gcmd_line.debug >= 7 ) {
//       printf("\naggregating pathsweight by %lf from %lf to %lf",
// 	     currentweight, lpathsweight, lpathsweight * (1.0 - currentweight));
//     }
//     lpathsweight *= (1.0 - currentweight);
//     return;
//   }

//   for ( i = 0; i < num_fathers[gfti][pnode]; i++ ) {
//     if ( gcmd_line.R && gcmd_line.debug >= 7 ) {
//       printf("\nrec call on ef %d", pfatherefs[gfti][pnode][i]);
//     }
//     rec_get_pnode_pathsweight( currentweight * ((pfatherefs[gfti][pnode][i] >= 0) ? 
// 						gef_conn[pfatherefs[gfti][pnode][i]].eff_p : 1.0),
// 			       gfti, pfathers[gfti][pnode][i],
// 			       pfathers, pfatherefs, num_fathers );
//   }

// }



Bool disjunctions_supported_by_initial_formula( int **dis, int *num_dis, 
						Bool **is_dis, 
						double **dis_weight,
						double *peff_weight,
						int num )

{

  static int **cdis;
  static int *num_cdis;
  static Bool fc = TRUE;

  int i, j, k, m, gfti;
  double a, b, gprob;

  int old_lr_num_c;
  int old_lr_num_clauses;




  if ( fc ) {
    cdis = ( int ** ) calloc( ggoal_state.num_F, sizeof( int * ) );
    num_cdis = ( int * ) calloc( ggoal_state.num_F, sizeof( int ) );
    for ( i = 0; i < ggoal_state.num_F; i++ ) {
      cdis[i] = ( int * ) calloc( gnum_ft_conn, sizeof( int ) );
    }
    fc = FALSE;
  }



  if ( gcmd_line.R && gcmd_line.debug >= 1 ) {
    printf("\nP h0/1: checking support by initial formula!");
    fflush(stdout);
  }





  /* loop over all the given U goals.
   */
  for ( gfti = 0; gfti < num; gfti++ ) {
    num_cdis[gfti] = 0;

    /* first, see if two of these leafs are contradictory
     * --> disjunction trivially implied, no need to add it
     */
    for ( i = 0; i < num_dis[gfti]; i++ ) {
      if ( gft_conn[dis[gfti][i]].negation != -1 &&
	   is_dis[gfti][gft_conn[dis[gfti][i]].negation] ) {
	if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
	  printf("\nP h0/1 contradictory ft pair in backchain leafs disjunction: ");
	  print_ft_name(dis[gfti][i]);
	  print_ft_name(gft_conn[dis[gfti][i]].negation);
	  fflush(stdout);
	}
	break;
      }
    }
    if ( i < num_dis[gfti] ) {
      continue;
    }




    /* create array of codes of these facts; use implicit
     * negated facts, ie. make these codes for coded literals as in the 
     * CNF.
     */
    num_cdis[gfti] = 0;
    for ( i = 0; i < num_dis[gfti]; i++ ) {
      /* this can happen if we're called from support
       * graph minimization!
       */
      if ( dis_weight[gfti][i] <= 0 ) {
	continue;
      }

      if ( gft_conn[dis[gfti][i]].CNF ) {
	m = 1;
	cdis[gfti][num_cdis[gfti]] = dis[gfti][i];
      } else {
	m = -1;
	cdis[gfti][num_cdis[gfti]] = gft_conn[dis[gfti][i]].negation;
      }
      if ( lr_codes[cdis[gfti][num_cdis[gfti]]] == -1 ) {
	/* we have to create the code.
	 */
	lr_num_c++;/* must be non-zero */
	lr_codes[cdis[gfti][num_cdis[gfti]]] = lr_num_c;
	if ( lr_num_c == gnum_ft_conn ) {
	  printf("\n\ntoo many r codes? %d\n\n", lr_num_c);
	  exit( 1 );
	}
	lr_cf[lr_num_c] = cdis[gfti][num_cdis[gfti]];
      }
      /* we have the code. write it into the code array.
       */
      cdis[gfti][num_cdis[gfti]] = m * lr_codes[cdis[gfti][num_cdis[gfti]]];
      num_cdis[gfti]++;
    }

    /* all these leafs have weight 0, and there are no probleafs!
     */
    if ( num_cdis[gfti] == 0 && peff_weight[gfti] <= 0 ) {
      return FALSE;
    }

  } /* endfor gfti over goal facts, first loop to get disjunction codes */





  /* second loop: now do the clauses!
   * have to make separate so we can distinguish
   * between the permanent new codes above, and
   * the non-permanent ones that will be added below!
   */
  old_lr_num_c = lr_num_c;
  old_lr_num_clauses = lr_num_clauses;

  for ( gfti = 0; gfti < num; gfti++ ) {
    /* another pre-check: is any ini or contained in dis?
     *
     * NOTE: clauses are ordered by increasing length so
     * this way we find a smallest appropriate clause.
     */
    for ( i = 0; i < lr_num_clauses; i++ ) {
      for ( j = 0; j < lr_clause_length[i]; j++ ) {
	for ( k = 0; k < num_cdis[gfti]; k++ ) {
	  if ( lr_clauses[i][j].literal == cdis[gfti][k] ) break;
	}
	if ( k == num_cdis[gfti] ) {
	  break;
	}
      }
      if ( j == lr_clause_length[i] ) {
	if ( gcmd_line.R && gcmd_line.debug  >= 2 ) {
	  printf("\nP h0/1 initial or is contained in leafs disjunction: ");
	  print_r_clause( i );
	  fflush(stdout);
	}
	break;
      }
    }
    if ( i < lr_num_clauses ) {
      continue;
    }


    /* Ok, nada. add this clause to the initial state formula.
     */ 

    if ( num_cdis[gfti] == gmax_literals ) {
      printf("\n\ntoo many literals %d in goal impleafs h0/1. increase gmax_literals\n\n",
	     num_cdis[gfti]);
      exit( 1 );
    }

    lr_clause_length[lr_num_clauses] = 0;
    for ( k = 0; k < num_cdis[gfti]; k++ ) {
      lr_clauses[lr_num_clauses][lr_clause_length[lr_num_clauses]].literal = cdis[gfti][k];
      lr_clauses[lr_num_clauses][lr_clause_length[lr_num_clauses]].time = -1;
      lr_clause_length[lr_num_clauses]++;
      /* july06: we don't need member lists for those clauses since they will now be used
       * only by extern cachet anyway, and will be deleted afterwards.
       */
    }


    /* july06: if needed, create an extra artificial node in this clause!
     */
    if ( 0 < peff_weight[gfti] ) {
      if ( gcmd_line.R && gcmd_line.debug >= 3 ) {
	printf("\nr P h1: insert probnode weight %lf for leafs of goal nr %d ", peff_weight[gfti], gfti);
	fflush(stdout);
      }

      lr_num_c++;
      if ( lr_num_c == gnum_ft_conn + 1001 ) {
	printf("\n\nr too many codes %d, with the new probnode code. increase 1001 in relax.cpp\n\n",
	       lr_num_c);
	exit( 1 );
      }
      lr_cf[lr_num_c] = -1971;/* this will identify chance nodes */
      lr_cweight[lr_num_c] = peff_weight[gfti];

      if ( lr_clause_length[lr_num_clauses] == gmax_literals ) {
	printf("\n\nr too many literals %d in goal impleafs h1 plus probnode. increase gmax_literals\n\n",
	     num_cdis[gfti]+1);
	exit( 1 );
      }
      lr_clauses[lr_num_clauses][lr_clause_length[lr_num_clauses]].literal = lr_num_c;
      lr_clauses[lr_num_clauses][lr_clause_length[lr_num_clauses]].time = -1;
      lr_clause_length[lr_num_clauses]++;
    }

    lr_num_clauses++;
  }




  /* insert the additional leaf weighting!!
   */
  for ( gfti = 0; gfti < num; gfti++ ) {
    for ( i = 0; i < num_dis[gfti]; i++ ) {
      if ( dis_weight[gfti][i] >= 1 ) {/* this here is certain, which just means it's not weighted */
	continue;
      }

      lr_num_c++;
      if ( lr_num_c == gnum_ft_conn + 1001 ) {
	printf("\n\nr too many codes %d, with the new probnode code. increase 1001 in relax.cpp\n\n",
	       lr_num_c);
	exit( 1 );
      }
      lr_cf[lr_num_c] = -1971;/* this will identify chance nodes */
      lr_cweight[lr_num_c] = dis_weight[gfti][i];

      /* either we do not set this leaf to 1...
       */
      lr_clauses[lr_num_clauses][0].literal = (-1) * cdis[gfti][i];
      lr_clauses[lr_num_clauses][0].time = -1;
      /* ... or we have to pay the weight price!
       */
      lr_clauses[lr_num_clauses][1].literal = lr_num_c;
      lr_clauses[lr_num_clauses][1].time = -1;
      lr_clause_length[lr_num_clauses] = 2;
      lr_num_clauses++;
    }
  }



  /* now do the weighted model counting.
   */


  times( &end );
  TIME( gP_time );
  times( &start );
  gwmc_calls++;
  r_wmc_CNF(&a, &b);
  times( &end );
  TIME( gwmc_time );
  times( &start );



  /* remove the additional clauses and codes.
   */
  for ( i = old_lr_num_clauses; i < lr_num_clauses; i++ ) {
    lr_clause_length[i] = 0;
  }
  lr_num_clauses = old_lr_num_clauses;
  for ( i = old_lr_num_c+1; i <= lr_num_c; i++ ) {
    /* I guess all of this is unnecessary -- it should suffice to reset lr_num_c -- 
     * but whatever.
     */
    lr_codes[lr_cf[i]] = -1;
    lr_cf[i] = 0;
    lr_cweight[i] = -1;
  }
  lr_num_c = old_lr_num_c;

  gprob = get_gprob( a, b );
  /* july06: remember the actual prob!!
   */
  lthisgoalprob = gprob;

  if ( gprob < ggoal_probability ) {
    if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
      printf("\nP h0/1: goal not supported! %lf", gprob);
      fflush(stdout);
    }
    return FALSE;
  }

  if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
    printf("\nP h0/1: goal supported! %lf", gprob);
    fflush(stdout);
  }
  return TRUE;

}



Bool disjunctions_supported_by_S_formula( int **dis, int *num_dis, 
					  Bool **is_dis, 
					  double **dis_weight,
					  double *peff_weight,
					  int num )

{

  static int **cdis;
  static int *num_cdis;
  static Bool fc = TRUE;

  int i, k, m, gfti;
  double a, b, gprob;

  int old_gnum_c;
  int old_gnum_clauses;



  if ( fc ) {
    cdis = ( int ** ) calloc( ggoal_state.num_F, sizeof( int * ) );
    num_cdis = ( int * ) calloc( ggoal_state.num_F, sizeof( int ) );
    for ( i = 0; i < ggoal_state.num_F; i++ ) {
      cdis[i] = ( int * ) calloc( gnum_ft_conn, sizeof( int ) );
    }
    fc = FALSE;

    if ( gcmd_line.R && gcmd_line.debug >= 3 ) {
      printf("\nINITIALIZING frage P support by state formula");
      fflush(stdout);
    }
  }


  if ( gcmd_line.R && gcmd_line.debug >= 1 ) {
    printf("\nP h2: checking support by state formula!");
    fflush(stdout);
  }





  /* loop over all the given U goals.
   */
  for ( gfti = 0; gfti < num; gfti++ ) {
    num_cdis[gfti] = 0;

    /* first, see if two of these leafs are contradictory
     * --> disjunction trivially implied, no need to add it
     */
    for ( i = 0; i < num_dis[gfti]; i++ ) {
      if ( gft_conn[dis[gfti][i]].negation != -1 &&
	   is_dis[gfti][gft_conn[dis[gfti][i]].negation] ) {
	if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
	  printf("\nP h2 contradictory ft pair in backchain leafs disjunction: %d neg %d",
		 dis[gfti][i], gft_conn[dis[gfti][i]].negation);
	  print_ft_name(dis[gfti][i]);
	  print_ft_name(gft_conn[dis[gfti][i]].negation);
	  fflush(stdout);
	}
	break;
      }
    }
    if ( i < num_dis[gfti] ) {
      continue;
    }


    if ( lStime < 0 || lStime > MAX_PLAN_LENGTH ) {
      printf("\n\nP h2 something wrong with lStime!\n\n"); fflush(stdout);
      exit( 1 );
    }



    /* create array of codes of these facts; use implicit
     * negated facts, ie. make these codes for coded literals as in the 
     * CNF.
     */
    num_cdis[gfti] = 0;
    for ( i = 0; i < num_dis[gfti]; i++ ) {
      /* this can happen if we're called from support
       * graph minimization!
       */
      if ( dis_weight[gfti][i] <= 0 ) {
	continue;
      }

      if ( gft_conn[dis[gfti][i]].CNF ) {
	m = 1;
	cdis[gfti][num_cdis[gfti]] = dis[gfti][i];
      } else {
	m = -1;
	cdis[gfti][num_cdis[gfti]] = gft_conn[dis[gfti][i]].negation;
      }
      if ( gcodes[lStime][cdis[gfti][num_cdis[gfti]]] == -1 ) {
	if ( gcmd_line.R && gcmd_line.debug >= 7 ) {
	  printf("\nP h2: no code for ");
	  print_ft_name( cdis[gfti][num_cdis[gfti]] );
	  printf(" at lStime %d. creating", lStime);
	  fflush(stdout);
	}
	/* we have to create the code.
	 */
	gnum_c++;/* must be non-zero */
	gcodes[lStime][cdis[gfti][num_cdis[gfti]]] = gnum_c;
	if ( gnum_c == (1 + MAX_PLAN_LENGTH) * gmax_CNFU ) {
	  printf("\n\nrP too many codes? %d\n\n", gnum_c);
	  exit( 1 );
	}
	gcf[gnum_c] = cdis[gfti][num_cdis[gfti]];
	gct[gnum_c] = lStime; 
      }
      /* we have the code. write it into the code array.
       */
      cdis[gfti][num_cdis[gfti]] = m * gcodes[lStime][cdis[gfti][num_cdis[gfti]]];
      num_cdis[gfti]++;
    }


    /* all these leafs have weight 0, and there are no probleafs!
     */
    if ( num_cdis[gfti] == 0 && peff_weight[gfti] <= 0 ) {
      return FALSE;
    }

  } /* endfor gfti over goal facts, first loop to get disjunction codes */
 





  /* second loop: now do the clauses!
   * have to make separate so we can distinguish
   * between the permanent new codes above, and
   * the non-permanent ones that will be added below!
   */
  old_gnum_c = gnum_c;
  old_gnum_clauses = gnum_clauses;

  for ( gfti = 0; gfti < num; gfti++ ) {
    /* no pre-check if any "ini" or is contained in dis:
     * this is bound to fail anyway.
     */

    if ( num_cdis[gfti] == gmax_literals ) {
      printf("\n\ntoo many literals %d in goal impleafs h2. increase gmax_literals\n\n",
	     num_cdis[gfti]);
      exit( 1 );
    }

    gclause_length[gnum_clauses] = 0;
    for ( k = 0; k < num_cdis[gfti]; k++ ) {
      gclauses[gnum_clauses][gclause_length[gnum_clauses]].literal = cdis[gfti][k];
      gclauses[gnum_clauses][gclause_length[gnum_clauses]].time = -1;
      gclause_length[gnum_clauses]++;
    }


    /* july06: if needed, create an extra artificial node in this clause!
     */
    if ( 0 < peff_weight[gfti] ) {
      if ( gcmd_line.R && gcmd_line.debug >= 3 ) {
	printf("\nP h2: insert probnode weight %lf for leafs of goal %d", peff_weight[gfti], gfti);
	fflush(stdout);
      }

      gnum_c++;
      if ( gnum_c == (1 + MAX_PLAN_LENGTH) * gmax_CNFU + 1001 ) {
	printf("\n\ntoo many codes %d, with the new c code. increase 1001 in state_transitions.cpp\n\n",
	       gnum_c);
	exit( 1 );
      }
      gcf[gnum_c] = -1971;/* this will identify chance nodes */
      gcweight[gnum_c] = peff_weight[gfti];

      if ( gclause_length[gnum_clauses] == gmax_literals ) {
	printf("\n\ntoo many literals %d in goal impleafs h2 plus probnode. increase gmax_literals\n\n",
	     num_cdis[gfti]+1);
	exit( 1 );
      }
      gclauses[gnum_clauses][gclause_length[gnum_clauses]].literal = gnum_c;
      gclauses[gnum_clauses][gclause_length[gnum_clauses]].time = -1;
      gclause_length[gnum_clauses]++;
    }

    gnum_clauses++;
  }




  /* insert the additional leaf weighting!!
   */
  for ( gfti = 0; gfti < num; gfti++ ) {
    for ( i = 0; i < num_dis[gfti]; i++ ) {
      if ( dis_weight[gfti][i] >= 1 ) {/* this here is certain, which just means it's not weighted */
	continue;
      }

      gnum_c++;
      if ( gnum_c == (1 + MAX_PLAN_LENGTH) * gmax_CNFU + 1001 ) {
	printf("\n\nr too many codes %d, with the new probnode code. increase 1001 in state_transitions.cpp\n\n",
	       gnum_c);
	exit( 1 );
      }
      gcf[gnum_c] = -1971;/* this will identify chance nodes */
      gcweight[gnum_c] = dis_weight[gfti][i];

      /* either we do not set this leaf to 1...
       */
      gclauses[gnum_clauses][0].literal = (-1) * cdis[gfti][i];
      gclauses[gnum_clauses][0].time = -1;
      /* ... or we have to pay the weight price!
       */
      gclauses[gnum_clauses][1].literal = gnum_c;
      gclauses[gnum_clauses][1].time = -1;
      gclause_length[gnum_clauses] = 2;
      gnum_clauses++;
    }
  }



  /* now do the weighted model counting.
   */


  times( &end );
  TIME( gP_time );
  times( &start );
  gwmc_calls++;
  gnum_decision_stack = 0;
  wmc_CNF(&a, &b);
  times( &end );
  TIME( gwmc_time );
  times( &start );



  /* remove the additional clauses and codes.
   */
  for ( i = old_gnum_clauses; i < gnum_clauses; i++ ) {
    gclause_length[i] = 0;
  }
  gnum_clauses = old_gnum_clauses;
  for ( i = old_gnum_c+1; i <= gnum_c; i++ ) {
    gcf[i] = 0;
    gcweight[i] = -1;/* unnecessary but whatever */
  }
  gnum_c = old_gnum_c;



  gprob = get_gprob( a, b );
  /* july06: remember the actual prob!!
   */
  lthisgoalprob = gprob;
  
  if ( gprob < ggoal_probability ) {
    if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
      printf("\nP h2: goal not supported! %lf", gprob);
      fflush(stdout);
    }
    return FALSE;
  }

  if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
    printf("\nP h2: goal supported! %lf", gprob);
    fflush(stdout);
  }
  return TRUE;

}















































/**************************************************************
 * STUFF for probabilities in 1P, called from above.
 **************************************************************/


































void preselect_P_actions( int time )

{


  static Bool fc = TRUE;
  static UftNode_pointer *goalU;

  int i, j, goalft;
  int num_goalU;

  int Utime;

  if ( fc ) {
    goalU = ( UftNode_pointer * ) calloc( ggoal_state.num_F, sizeof( UftNode_pointer ) );
    fc = FALSE;
  }
    
  /* the time in terms of U nodes
   */
  Utime =  time + lpath_U_length - 1;


  if ( gcmd_line.R && gcmd_line.debug >= 1 ) {
    printf("\nPRE time %d, entry preselect P", time);
    fflush(stdout);
  }

  /* process the goals, and collect the Us
   */
  num_goalU = 0;
  for ( i = 0; i < lcurrent_goals.num_F; i++ ) {
    goalft = lcurrent_goals.F[i];

    if ( gcmd_line.R && gcmd_line.debug >= 1 ) {
      printf("\nPRE time %d, preselect P, goal fact ", time);
      print_ft_name(goalft);
      fflush(stdout);
    }

    for ( j = 0; j < lnum_U[Utime]; j++ ) {
      if ( lU[Utime][j].ft == goalft ) break;
    }
    if ( j == lnum_U[Utime] ) {
      if ( !gft_conn[goalft].in_F ) {
	if ( gcmd_line.R && gcmd_line.debug >= 1 ) {
	  printf("\nPRE time %d, preselect P, goal fact is false!", time);
	  fflush(stdout);
	  exit( 1 );
	}
      }
      /* true fact; skip it.
       */
      if ( gcmd_line.R && gcmd_line.debug >= 1 ) {
	printf("\nPRE goal ft not U! skipping");
	fflush(stdout);
      }
      continue;
    }

    if ( !lU[Utime][j].became_F && gcmd_line.R && gcmd_line.debug >= 3 ) {
      printf("\nPRE: goal fact not became F.");
      fflush(stdout);
    }

    if ( gcmd_line.R && gcmd_line.debug >= 1 ) {
      printf("\nPRE: preselect: goal ft added into U!");
      fflush(stdout);
    }

    goalU[num_goalU++] = &(lU[Utime][j]);
  }

  if ( num_goalU == 0 ) {
    /* actually, they are all true, so nada to do.
     */
    return;
  }


  /* we got the U nodes. now collect the minimal sufficient implication leafs
   */

  if ( gcmd_line.R && gcmd_line.debug >= 1 ) {
    printf("\ntime %d, preselect P, now P test!", time);
    fflush(stdout);
  }

  times( &end );
  TIME( geval_time );
  times( &start );

  /* recall whether the pre-select was done and the U goal facts have thus been
   * supported already.
   */
  l1P_preselection_done = select_disjunctions_supported_by_formula( Utime, goalU, num_goalU,
								    ggoal_probability );

  times( &end );
  TIME( gP_time );
  times( &start );

}



Bool select_disjunctions_supported_by_formula( int Utime, UftNode_pointer *goalU, int num_goalU,
					       double requested_weight ) 

{

  static Bool fc = TRUE;
  static int **leafs;
  static int *num_leafs;
  static Bool **is_leaf;
  static UftNode_pointer **p;
  static int *selectp;
  /* july06: these guys here list the prob eff outcomes 
   * that contribute to each goal fact.
   */
  static int **probleafefs, **probleaftimes, **probleafps;
  static int *num_probleafs;
  /* a node may have several fathers! --> *every* father obtained
   * via a prob eff is taken: just one of those may not be enough to
   * obtain the desired relaxed plan.
   */
  static int ***pfathers, **num_fathers;
  static int ***pfatherefs, **pt, **leafps;
  /* july06: this guy here stores, in each iteration of the bwds breadth
   * first, the indices of the prob effs *with condition* we got here: this is needed to know
   * if or if not *all* outcomes of an effect are there to lead 
   * to a fact node in the next iteration; it is also needed to implement
   * the weight propagation.
   */
  static int *current_condprobps;
  /* july06: this guy here, finally, is the concise storage of the bwd 
   * graph, as the basis for support graph minimization.
   *
   * array [goalfacts][numberedges]
   */
  static UEdgeNode **supportgraph;
  static int *num_supportgraph;


  int num_p, prev_p, now_p, num_current_condprobs;
  int i, j, k, l, ll, t, ft, ft_, gfti;

  int conditionp;
  UftNode *cpu;/* just a helper .. */

//   double maxpeffweight;
//   int maxpeffp;



  if ( fc ) {
    leafs = ( int ** ) calloc( ggoal_state.num_F, sizeof( int * ) );
    num_leafs = ( int * ) calloc( ggoal_state.num_F, sizeof( int ) );
    is_leaf = ( Bool ** ) calloc( gnum_ft_conn, sizeof( Bool * ) );

    p = ( UftNode_pointer ** ) calloc( ggoal_state.num_F, sizeof( UftNode_pointer * ) );
    pfathers = ( int *** ) calloc( ggoal_state.num_F, sizeof( int ** ) );
    num_fathers = ( int ** ) calloc( ggoal_state.num_F, sizeof( int * ) );
    pfatherefs = ( int *** ) calloc( ggoal_state.num_F, sizeof( int ** ) );
    pt = ( int ** ) calloc( ggoal_state.num_F, sizeof( int * ) );
    leafps = ( int ** ) calloc( ggoal_state.num_F, sizeof( int * ) );

    for ( i = 0; i < ggoal_state.num_F; i++ ) {
      leafs[i] = ( int * ) calloc( gnum_ft_conn, sizeof( int ) );
      is_leaf[i] = ( Bool * ) calloc( gnum_ft_conn, sizeof( Bool ) );
      for ( j = 0; j < gnum_ft_conn; j++ ) {
	is_leaf[i][j] = FALSE;
      }

      p[i] = ( UftNode_pointer * ) calloc( lMAX_PATHNODES, sizeof( UftNode_pointer ) );
      pfathers[i] = ( int ** ) calloc( lMAX_PATHNODES, sizeof( int * ) );
      pfatherefs[i] = ( int ** ) calloc( lMAX_PATHNODES, sizeof( int * ) );
      for ( j = 0; j < lMAX_PATHNODES; j++ ) {
	/* hack! double-check overflows below.
	 */
	pfathers[i][j] = ( int * ) calloc( 50, sizeof( int ) );
	pfatherefs[i][j] = ( int * ) calloc( 50, sizeof( int ) );
      }
      num_fathers[i] = ( int * ) calloc( lMAX_PATHNODES, sizeof( int ) );
      pt[i] = ( int * ) calloc( lMAX_PATHNODES, sizeof( int ) );
      leafps[i] = ( int * ) calloc( lMAX_PATHNODES, sizeof( int ) );
    }

    /* july06
     */
    probleafefs = ( int ** ) calloc( ggoal_state.num_F, sizeof( int * ) );
    probleaftimes = ( int ** ) calloc( ggoal_state.num_F, sizeof( int * ) );
    probleafps = ( int ** ) calloc( ggoal_state.num_F, sizeof( int * ) );
    num_probleafs = ( int * ) calloc( ggoal_state.num_F, sizeof( int ) );
    for ( i = 0; i < ggoal_state.num_F; i++ ) {
      /* july06: this is a hack! well I guess 1000 should be enough
       * most of the time.. ! double-check for overflows below.
       */
      probleafefs[i] = ( int * ) calloc( 1000, sizeof( int ) );
      probleaftimes[i] = ( int * ) calloc( 1000, sizeof( int ) );
      probleafps[i] = ( int * ) calloc( 1000, sizeof( int ) );
    }

    selectp = ( int * ) calloc( lMAX_PATHNODES, sizeof( int ) );

    current_condprobps = ( int * ) calloc( gnum_pef_conn, sizeof( int ) );


    supportgraph = ( UEdgeNode ** ) calloc( ggoal_state.num_F, sizeof( UEdgeNode * ) );
    num_supportgraph = ( int * ) calloc( ggoal_state.num_F, sizeof( int ) );
    for ( i = 0; i < ggoal_state.num_F; i++ ) {
      /* hack! double check below.
       */
      supportgraph[i] = ( UEdgeNode * ) calloc(10000, sizeof( UEdgeNode ) );
    }

    fc = FALSE;
  }



  if ( gcmd_line.R && gcmd_line.debug >= 1 ) {
    printf("\nUtime %d, entry goal Uleaf disjunctions", Utime);
    fflush(stdout);
  }  
  /* if the U goals are became F anyway,
   * then we can actually use the cheaper standard DP-based minimization.
   * so just go back here, leaving the "preselect-done" flag set to FALSE.
   */ 
  if ( !lgoals_not_known ) {
    if ( gcmd_line.R && gcmd_line.debug >= 1 ) {
      printf("\ngoals became F --> bail out and leave to usual stuff");
      fflush(stdout);
    }
    return FALSE;
  } 




  /* loop over all the given U goals.
   */
  for ( gfti = 0; gfti < num_goalU; gfti++ ) {
    ft = goalU[gfti]->ft;
    p[gfti][0] = goalU[gfti];
    num_fathers[gfti][0] = 0;
    pt[gfti][0] = Utime;
    now_p = 0;
    num_p = 1;
    num_leafs[gfti] = 0;
    num_probleafs[gfti] = 0;

    if ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
      printf("\nstarting "); print_ft_name( ft );
      fflush(stdout);
    }
    for ( t = Utime; t > 0; t-- ) {
      prev_p = now_p;
      now_p = num_p;
      num_current_condprobs = 0;



      for ( i = prev_p; i < now_p; i++ ) {
	/* july06: the prob nodes are in this list; they are taken care of below;
	 * don't expand them here.
	 */
	if ( p[gfti][i]->ft == -1 ) {
	  continue;
	}
	if ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
	  printf("\nPrp i at t %d: ", t); print_ft_name( p[gfti][i]->ft );
	  fflush(stdout);
	}



	for ( j = 0; j < p[gfti][i]->num_in; j++ ) {
	  ft_ = p[gfti][i]->in_edges[j]->ft;



	  /* SKIP THIS NODE IF ft_ APPEARS ON A PATH FROM p[i] TO THE GOAL NODE!
	   * otherwise we're trying to make ft_
	   * true through making it true in the 1st place!
	   * this makes only sense if the effect we're using for that is a NOOP.
	   * allow this case.
	   *
	   * (well I guess the real thing would be to test if ft_ appears on ALL paths
	   * from the ef to the goal... skip that for now...)  
	   *
	   * NOTE: this implementation is naive, chaining back from the ef node
	   * over his fathers. we could save time by book-keeping the sets of
	   * ancestor facts across the node. well, no big deal probably.
	   */
	  if ( gcmd_line.ancestorpruning ) {
	    if ( p[gfti][i]->in_efs[j] != -1 && 
		 ft_ != -1 && /* only do this with edges actually leading to a fact! */
		 is_ancestor(ft_, 
			     gfti, i,
			     pfathers, num_fathers, p) ) {	    
	      if ( gcmd_line.R && gcmd_line.debug >= 7 ) {
		printf("\nPrp main loop ANCESTOR-SKIPPING father of fact %d ", ft_);
		fflush(stdout);
		print_ft_name(ft_);
		printf(" at t %d, father pnode would have been pnode %d, ", 
		       t, i);
		print_ft_name(p[gfti][i]->ft);
		fflush(stdout);
	      }
	      continue;
	    }
	  }



	  /* see if we got this node already: avoid duplicates!
	   */
	  for ( k = now_p; k < num_p; k++ ) {
	    if ( p[gfti][k] == p[gfti][i]->in_edges[j] ) break;
	  }
	  if ( k < num_p ) {
	    /* had this. update fathers, and skip.
	     */
	    if ( gcmd_line.alloutcomepaths ) {		
	      if ( num_fathers[gfti][k] == 50 ) {
		printf("\ntoo many Unode fathers! increase ``50'' in relax.cpp, select_disjunctions_supported_by_formula\n\n");
		exit( 1 );
	      }
	      for ( l = 0; l < num_fathers[gfti][k]; l++ ) {
		/* note: eff_p = 1 for non-prob effects.
		 */
		if ( gef_conn[p[gfti][i]->in_efs[j]].eff_p >=
		     gef_conn[pfatherefs[gfti][k][l]].eff_p ) {
		  break;
		}
	      }
	      for ( ll = num_fathers[gfti][k]; ll > l; ll-- ) {
		pfathers[gfti][k][ll] = pfathers[gfti][k][ll-1];
		pfatherefs[gfti][k][ll] = pfatherefs[gfti][k][ll-1];
	      }
	      pfathers[gfti][k][l] = i;
	      pfatherefs[gfti][k][l] = p[gfti][i]->in_efs[j];
	      num_fathers[gfti][k]++;
	      if ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
		printf("\nPrp selectdisj main loop added at t %d additional father of pnode %d ", 
		       t, k);
		print_ft_name(p[gfti][k]->ft);
		printf(", pnode %d ", i);
		print_ft_name(p[gfti][i]->ft);
	      }
	      continue;
	    } /* endif want multiple fathers */

	    if ( gef_conn[p[gfti][i]->in_efs[j]].eff_p >
		 gef_conn[pfatherefs[gfti][k][0]].eff_p ) {
	      pfathers[gfti][k][0] = i;
	      pfatherefs[gfti][k][0] = p[gfti][i]->in_efs[j];
	      if ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
		printf("\nPrp replaced at t %d father of pnode %d ", t, k);
		print_ft_name(p[gfti][k]->ft);
		printf(" with pnode %d, eff %d of ", i, p[gfti][i]->in_efs[j]);
		print_op_name(gef_conn[p[gfti][i]->in_efs[j]].op);
	      }
	    }

	    continue;
	  } /* endif we had this node already */



	  if ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
	    /* july06: catch prob nodes.
	     */
	    if ( p[gfti][i]->in_edges[j]->ft >= 0 ) {
	      printf("\nPrp new p: "); print_ft_name( p[gfti][i]->in_edges[j]->ft );
	    } else {
	      printf("\nPrp new prob node, ef %d Utime %d of op ", 
		     p[gfti][i]->in_edges[j]->ef, p[gfti][i]->in_edges[j]->Utime);
	      print_op_name(gef_conn[p[gfti][i]->in_edges[j]->ef].op);
	    }
	    printf(" (by "); 
	    if ( p[gfti][i]->in_efs[j] == -1 ) {
	      printf("NOOP ");
	    } else {
	      if ( p[gfti][i]->in_efs[j] == -2 ) {
		printf("OR constraint??\n\n");
		fflush(stdout);
		exit( 1 );
	      } else {
		if ( p[gfti][i]->in_efs[j] == -1971 ) {
		  printf("probnode edge");
		} else {
		  print_op_name( gef_conn[p[gfti][i]->in_efs[j]].op );
		}
	      }
	    }
	    printf(")");
	    fflush(stdout);
	  }



	  if ( num_p >= lMAX_PATHNODES ) {
	    printf("\n\nPrp 3 -- too many nodes on paths! increase lMAX_PATHNODES (now %d)\n\n", lMAX_PATHNODES);
	    fflush(stdout);
	    exit( 1 );
	  }
	  p[gfti][num_p] = p[gfti][i]->in_edges[j];
	  pfathers[gfti][num_p][0] = i;
	  pfatherefs[gfti][num_p][0] = p[gfti][i]->in_efs[j];
	  num_fathers[gfti][num_p] = 1;
	  pt[gfti][num_p] = pt[gfti][i] - 1;
	  num_p++;



	  if ( ft_ == -1 ) {

	    if ( p[gfti][i]->in_edges[j]->num_in > 0 ) {
	      /* we cannot propagate to conditions before we know
	       * if or if not each probnode has full parents.
	       * so just remember the guys here, and treat them in an extra
	       * loop below.
	       */
	      if ( p[gfti][i]->in_edges[j]->num_in != 1 ) {
		printf("\nprob node with more than one son?\n\n");
		exit( 1 );
	      }
	      current_condprobps[num_current_condprobs] = num_p-1;
	      num_current_condprobs++;
	      continue;
	    }


	    if ( num_probleafs[gfti] == 1000 ) {
	      printf("\ntoo many probleafs! increase ``1000'' in relax.cpp, Uleaf_disjunctions_supported_by_formul\n\n");
	      exit( 1 );
	    }
	    probleafefs[gfti][num_probleafs[gfti]] = p[gfti][i]->in_edges[j]->ef;
	    probleaftimes[gfti][num_probleafs[gfti]] = p[gfti][i]->in_edges[j]->Utime;
	    probleafps[gfti][num_probleafs[gfti]] = num_p-1;
	    num_probleafs[gfti]++;
	    /* continue here so we don't get to the leafs handling below for this guy.
	     */
	    continue;
	  }



	  if ( t == 1 && !is_leaf[gfti][ft_] ) {
	    is_leaf[gfti][ft_] = TRUE;
	    leafs[gfti][num_leafs[gfti]] = ft_;
	    leafps[gfti][num_leafs[gfti]] = num_p-1;
	    num_leafs[gfti]++;
	    if ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
	      printf("\nPrp set is-l "); print_ft_name( ft_ );
	      fflush(stdout);
	    }
	  } /* endif ft_ is new leaf */
	} /* j in edges of node p[gfti][i] */
      } /* all p[gfti][i] in prev level */




      /* july06: after the fact nodes t have been expanded, we now expand the 
       * cond prob eff nodes that we have collected above.
       */
      for ( i = 0; i < num_current_condprobs; i++ ) {
	cpu = p[gfti][current_condprobps[i]];
	/* some debugging...
	 */
	if ( cpu->ft != -1 || cpu->num_in != 1 ) {
	  printf("\nft ");
	  print_ft_name(cpu->ft);
	  printf(" num in %d in selectdisj condprob expansion?\n\n", cpu->num_in);
	  exit( 1 );
	}
	if ( gcmd_line.R && gcmd_line.debug >= 7 ) {
	  printf("\nPrp selectdisj CPloop now doing pnode %d ef %d of ", current_condprobps[i], cpu->ef); 
	  print_op_name(gef_conn[cpu->ef].op);
	  fflush(stdout);
	}



	/* CONSIDER THIS EDGE ONLY IF THE CONDITION NODE DOES NOT APPEAR
	 * ON A PATH OF THIS EF TO THE GOAL NODE! otherwise we're trying to make C
	 * true through making it true in the 1st place!
	 * (well I guess the real thing would be to test if C appears on ALL paths
	 * from the ef to the goal... skip that for now...)  
	 *
	 * NOTE: this implementation is naive, chaining back from the ef node
	 * over his fathers. we could sav time by book-keeping the sets of
	 * ancester facts across the node. well, no big deal probably.
	 */
	if ( gcmd_line.ancestorpruning ) {
	  if ( is_ancestor(cpu->in_edges[0]->ft, 
			   gfti, current_condprobps[i],
			   pfathers, num_fathers, p) ) {	    
	    if ( gcmd_line.R && gcmd_line.debug >= 7 ) {
	      printf("\nPrp CPloop ANCESTOR-SKIPPING father of ");
	      print_ft_name(cpu->in_edges[0]->ft);
	      printf(" at t %d, father pnode would have been %d, ef %d of ", 
		     t, current_condprobps[i], p[gfti][current_condprobps[i]]->ef);
	      print_op_name(gef_conn[p[gfti][current_condprobps[i]]->ef].op);
	    }
	    continue;
	  }
	}



	for ( k = now_p; k < num_p; k++ ) {
	  if ( p[gfti][k] == cpu->in_edges[0] ) break;
	}
	if ( k < num_p ) {
	  conditionp = k;



	  if ( !gcmd_line.alloutcomepaths ) {		
	    if ( gef_conn[cpu->ef].eff_p >
		 gef_conn[pfatherefs[gfti][conditionp][0]].eff_p ) {
	      pfathers[gfti][conditionp][0] = current_condprobps[i];
	      pfatherefs[gfti][conditionp][0] = cpu->ef;
	      if ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
		printf("\nPrp replaced at t %d father probeff of pnode %d condition fact ", t, conditionp);
		print_ft_name(p[gfti][conditionp]->ft);
		printf(" with pnode %d, eff %d of ", current_condprobps[i], cpu->ef);
		print_op_name(gef_conn[cpu->ef].op);
	      }
	    }
	    continue;
	  }

	  if ( num_fathers[gfti][conditionp] == 50 ) {
	    printf("\ntoo many Unode fathers! increase ``50'' in relax.cpp, select_disjunctions_supported_by_formula\n\n");
	    exit( 1 );
	  }
	  for ( l = 0; l < num_fathers[gfti][conditionp]; l++ ) {
	    /* note: eff_p = 1 for non-prob effects.
	     */
	    if ( gef_conn[cpu->ef].eff_p >=
		 gef_conn[pfatherefs[gfti][conditionp][l]].eff_p ) {
	      break;
	    }
	  }
	  for ( ll = num_fathers[gfti][conditionp]; ll > l; ll-- ) {
	    pfathers[gfti][conditionp][ll] = pfathers[gfti][conditionp][ll-1];
	    pfatherefs[gfti][conditionp][ll] = pfatherefs[gfti][conditionp][ll-1];
	  }
	  pfathers[gfti][conditionp][l] = current_condprobps[i];
	  pfatherefs[gfti][conditionp][l] = cpu->ef;
	  num_fathers[gfti][conditionp]++;
	  if ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
	    printf("\nPrp selectdisj CPloop added at t %d additional father probeff of pnode %d condition fact ", 
		   t, conditionp);
	    print_ft_name(p[gfti][conditionp]->ft);
	    printf(", pnode %d, eff %d of ", current_condprobps[i], cpu->ef);
	    print_op_name(gef_conn[cpu->ef].op);
	  }
	  
	  continue;
	} /* endif this cond is already there */



	/* this is a new guy.
	 * first, store the new node into our bwd list.
	 */
	ft_ = cpu->in_edges[0]->ft;
	if ( num_p >= lMAX_PATHNODES ) {
	  printf("\n\nr P3 selectdisj CPloop -- too many nodes on paths! increase lMAX_PATHNODES (now %d)\n\n",
		 lMAX_PATHNODES);
	  fflush(stdout);
	  exit( 1 );
	}
	p[gfti][num_p] = cpu->in_edges[0];
	pfathers[gfti][num_p][0] = current_condprobps[i];
	pfatherefs[gfti][num_p][0] = cpu->ef;
	num_fathers[gfti][num_p] = 1;
	/* prob nodes are at same time as their condition sons!
	 */
	pt[gfti][num_p] = pt[gfti][current_condprobps[i]];
	num_p++;
	if ( gcmd_line.R && gcmd_line.debug >= 7 ) {
	  printf("\nPrp selectdisj CPloop new pnode %d p: ", num_p-1); 
	  print_ft_name( ft_ );
	  printf(" with t %d, coming from ef %d of ", pt[gfti][num_p-1], cpu->ef);
	  print_op_name(gef_conn[cpu->ef].op);
	  fflush(stdout);
	}

	if ( t == 1 && !is_leaf[gfti][ft_] ) {
	  is_leaf[gfti][ft_] = TRUE;
	  leafs[gfti][num_leafs[gfti]] = ft_;
	  leafps[gfti][num_leafs[gfti]] = num_p-1;
	  num_leafs[gfti]++;
	  if ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
	    printf("\nPrp selectdisj CPloop set is-l "); print_ft_name( ft_ );
	    fflush(stdout);
	  }
	}

      } /* i over current condprobs: end of cond prob node expansion */

    } /* t over all levels from top to bottom */
  } /* gfti over all goal facts */



  if ( gcmd_line.R && gcmd_line.debug  >= 1 ) {
    for ( gfti = 0; gfti < num_goalU; gfti++ ) {
      printf("\nPrp fact "); print_ft_name( goalU[gfti]->ft );
      printf(" has leafs:");
      for ( i = 0; i < num_leafs[gfti]; i++ ) {
	printf(" ");print_ft_name( leafs[gfti][i] );
      }
      printf(" and prob leafs:");
      for ( i = 0; i < num_probleafs[gfti]; i++ ) {
	printf(" (ef %d at Utime %d of action ", probleafefs[gfti][i], probleaftimes[gfti][i]);
	print_op_name(gef_conn[probleafefs[gfti][i]].op);
	printf(")");
      }
      fflush(stdout);
    }
  }







  /* now collect the support graphs!
   */
  for ( gfti = 0; gfti < num_goalU; gfti++ ) {
    collect_support_graph(gfti, 
			  supportgraph, num_supportgraph,
			  pfathers, pfatherefs, num_fathers,
			  leafps, num_leafs,
			  probleafps, probleafefs, num_probleafs, 
			  p, pt);

    if ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
      printf("\ngoal %d has support graph:", gfti);
      for ( i = 0; i < num_supportgraph[gfti]; i++ ) {
	printf("\nedge %3d: active %d pnode %3d ", 
	       i, supportgraph[gfti][i].active, supportgraph[gfti][i].pnodefrom);
	if ( p[gfti][supportgraph[gfti][i].pnodefrom]->ft != -1 ) {
	  print_ft_name(p[gfti][supportgraph[gfti][i].pnodefrom]->ft);
	} else {
	  printf("ef %3d of ", p[gfti][supportgraph[gfti][i].pnodefrom]->ef);
	  print_op_name(gef_conn[p[gfti][supportgraph[gfti][i].pnodefrom]->ef].op);
	}
	printf(" ---> pnode %3d ", supportgraph[gfti][i].pnodeto);
	if ( p[gfti][supportgraph[gfti][i].pnodeto]->ft != -1 ) {
	  print_ft_name(p[gfti][supportgraph[gfti][i].pnodeto]->ft);
	} else {
	  printf("ef %3d of ", p[gfti][supportgraph[gfti][i].pnodeto]->ef);
	  print_op_name(gef_conn[p[gfti][supportgraph[gfti][i].pnodeto]->ef].op);
	}
	printf(" at t %3d real %3d. \nEffects P %6lf: ", 
	       supportgraph[gfti][i].t, supportgraph[gfti][i].realtime, supportgraph[gfti][i].P);
	for ( j = 0; j < supportgraph[gfti][i].num_E; j++ ) {
	  printf("ef %d of ", supportgraph[gfti][i].E[j]);
	  print_op_name(gef_conn[supportgraph[gfti][i].E[j]].op);
	}
      }
    }
  }



  /* initialize the entire support graphs to be active.
   */
  for ( gfti = 0; gfti < num_goalU; gfti++ ) {
    for ( i = 0; i < num_supportgraph[gfti]; i++ ) {
      supportgraph[gfti][i].active = TRUE;
    }


    /* at this point, if wanted, propagate the weights and prune the fathers
     * so that only the most weighty pef father remains!!
     */
    if ( gcmd_line.maxpeffather ) {
      for ( i = 0; i < num_probleafs[gfti]; i++ ) {
	p[gfti][probleafps[gfti][i]]->weight = 0.0;
      }
      propagate_support_graph(gfti, supportgraph, num_supportgraph, p, TRUE);

//       /* take the max weight probleaf only!
//        */
//       maxpeffweight = -1;
//       maxpeffp = -1;
//       for ( i = 0; i < num_probleafs[gfti]; i++ ) {
// 	if ( p[gfti][probleafps[gfti][i]]->weight <= 0.0 ) {
// 	  continue;
// 	}
// 	if ( maxpeffp == -1 || p[gfti][probleafps[gfti][i]]->weight > maxpeffweight ) {
// 	  maxpeffweight = p[gfti][probleafps[gfti][i]]->weight;
// 	  maxpeffp = probleafps[gfti][i];
// 	}
//       }
//       if ( maxpeffp > -1 ) {
// 	probleafps[gfti][0] = maxpeffp;
// 	num_probleafs[gfti] = 1;
//       } else {
// 	num_probleafs[gfti] = 0;
//       }

      if ( gcmd_line.R && gcmd_line.debug  >= 7 ) {
	printf("\ngoal %d has PRUNED support graph:", gfti);
	for ( i = 0; i < num_supportgraph[gfti]; i++ ) {
	  if ( !supportgraph[gfti][i].active ) {
	    continue;
	  }
	  printf("\nedge %3d: active %d pnode %3d ", 
		 i, supportgraph[gfti][i].active, supportgraph[gfti][i].pnodefrom);
	  if ( p[gfti][supportgraph[gfti][i].pnodefrom]->ft != -1 ) {
	    print_ft_name(p[gfti][supportgraph[gfti][i].pnodefrom]->ft);
	  } else {
	    printf("ef %3d of ", p[gfti][supportgraph[gfti][i].pnodefrom]->ef);
	    print_op_name(gef_conn[p[gfti][supportgraph[gfti][i].pnodefrom]->ef].op);
	  }
	  printf(" ---> pnode %3d ", supportgraph[gfti][i].pnodeto);
	  if ( p[gfti][supportgraph[gfti][i].pnodeto]->ft != -1 ) {
	    print_ft_name(p[gfti][supportgraph[gfti][i].pnodeto]->ft);
	  } else {
	    printf("ef %3d of ", p[gfti][supportgraph[gfti][i].pnodeto]->ef);
	    print_op_name(gef_conn[p[gfti][supportgraph[gfti][i].pnodeto]->ef].op);
	  }
	  printf(" at t %3d real %3d. \nEffects P %6lf: ", 
		 supportgraph[gfti][i].t, supportgraph[gfti][i].realtime, supportgraph[gfti][i].P);
	  for ( j = 0; j < supportgraph[gfti][i].num_E; j++ ) {
	    printf("ef %d of ", supportgraph[gfti][i].E[j]);
	    print_op_name(gef_conn[supportgraph[gfti][i].E[j]].op);
	  }
	}
      }

    } /* endif want probefmaxfather */

  } /* endfor gfti over goal facts, support graph initialization/prune loop */







  /* and now, if wanted: minmize the support graph!
   *
   * our current method for this is to look at all
   * actions on the path to the state, and remove them from
   * the relaxed plan if possible ie if the goal
   * is then still supported enough.
   */
  if ( gcmd_line.minimize ) {
    minimize_support_graph(goalU, num_goalU,
			   supportgraph, num_supportgraph,
			   leafs, leafps, num_leafs, is_leaf,
			   probleafps, num_probleafs, 
			   p);
  }
  





  /* now, simply select the active edges in the support graph!
   */ 
  if ( gcmd_line.R && gcmd_line.debug >= 1 ) {
    printf("\nselect support graph.");
  }
  for ( gfti = 0; gfti < num_goalU; gfti++ ) {
    if ( gcmd_line.R && gcmd_line.debug >= 1 ) {
      printf("\nPrp PRE-selecting for %d", gfti);
    }
    for ( i = 0; i < num_supportgraph[gfti]; i++ ) {
      if ( !supportgraph[gfti][i].active ) {
	continue;
      }
      for ( j = 0; j < supportgraph[gfti][i].num_E; j++ ) {
	pathselect( supportgraph[gfti][i].t, supportgraph[gfti][i].E[j] );
      }
    }
  }

  for ( gfti = 0; gfti < num_goalU; gfti++ ) {
    for ( i = 0; i < num_leafs[gfti]; i++ ) {
      is_leaf[gfti][leafs[gfti][i]] = FALSE;
    }
  }
  return TRUE;

}

  

void collect_support_graph( int gfti, 
			    UEdgeNode **supportgraph, int *num_supportgraph,
			    int ***pfathers, int ***pfatherefs, int **num_fathers,
			    int **leafps, int *num_leafs,
			    int **probleafps, int **probleafefs, int *num_probleafs,
			    UftNode_pointer **p, int **pt )

{

  static Bool fc = TRUE;
  static int *fwdp;


  int i, j, k, l, ll, lll;
  int num_fwdp, fathernodeindex, opanodeindex, fatheref;


  if ( fc ) {
    fwdp = ( int * ) calloc( lMAX_PATHNODES, sizeof( int ) );
    fc = FALSE;
  }



  num_supportgraph[gfti] = 0;



  /* first, the fact leafs.
   */
  for ( i = 0; i < num_leafs[gfti]; i++ ) {
    fwdp[i] = leafps[gfti][i];
  }
  num_fwdp = num_leafs[gfti];
  for ( i = 0; i < num_probleafs[gfti]; i++ ) {
    /* a little dirty: just replace the father information for this
     * node with this effect. the rest remains exactly the same.
     */
    for ( j = 0; j < num_fathers[gfti][probleafps[gfti][i]]; j++ ) {
      pfatherefs[gfti][probleafps[gfti][i]][j] = probleafefs[gfti][i];
    }
    fwdp[num_fwdp] = probleafps[gfti][i];
    num_fwdp++;
  }


  j = 0;
  while ( j < num_fwdp ) {
    
    for ( k = 0; k < num_fathers[gfti][fwdp[j]]; k++ ) {
      fathernodeindex = pfathers[gfti][fwdp[j]][k];
      fatheref = pfatherefs[gfti][fwdp[j]][k];
      
      /* this father is a fact node. plain insert!
       */
      if ( p[gfti][fathernodeindex]->ft != -1 ) {
	for ( l = 0; l < num_fwdp; l++ ) {
	  if ( fwdp[l] == fathernodeindex ) break;
	}
	if ( l == num_fwdp ) {
	  fwdp[num_fwdp] = fathernodeindex;
	  num_fwdp++;
	}
	
	for ( l = 0; l < num_supportgraph[gfti]; l++ ) {
	  if ( supportgraph[gfti][l].pnodefrom == fwdp[j] &&
	       supportgraph[gfti][l].pnodeto == fathernodeindex &&
	       supportgraph[gfti][l].num_E == 1 &&
	       supportgraph[gfti][l].E[0] == fatheref ) {
	    break;
	  }
	}
	if ( l < num_supportgraph[gfti] ) {
	  /* we already created this edge for some other leaf.
	   */
	  continue;
	}
	
	if ( num_supportgraph[gfti] >= 10000 ) {
	  printf("\ntoo many edegs in support graph! increase 10000 in select_disjunctions_supported_by_formula\n\n");
	  exit( 1 );
	}
	supportgraph[gfti][num_supportgraph[gfti]].pnodefrom = fwdp[j];
	supportgraph[gfti][num_supportgraph[gfti]].pnodeto = fathernodeindex;
	supportgraph[gfti][num_supportgraph[gfti]].t = pt[gfti][fathernodeindex];
	supportgraph[gfti][num_supportgraph[gfti]].realtime = 
	  pt[gfti][fathernodeindex] - lpath_U_length + 1;
	if ( fatheref >= 0 ) {
	  supportgraph[gfti][num_supportgraph[gfti]].E[0] = fatheref;
	  supportgraph[gfti][num_supportgraph[gfti]].num_E = 1;
	  supportgraph[gfti][num_supportgraph[gfti]].P = gef_conn[fatheref].eff_p;
	} else {
	  /* NOOP. ignore.
	   */
	  supportgraph[gfti][num_supportgraph[gfti]].num_E = 0;
	  supportgraph[gfti][num_supportgraph[gfti]].P = 1.0;
	}
	supportgraph[gfti][num_supportgraph[gfti]].active = FALSE;
	num_supportgraph[gfti]++;
	
	continue;
      } /* endif father is a fact node */
      
      
      /* father is a probnode! have to chain one step further to
       * the fathers of the father!
       */
      if ( fatheref < 0 ) {
	printf("\nprobnode fatheref < 0?\n\n");
	exit( 1 );
      }
      
      for ( l = 0; l < num_fathers[gfti][fathernodeindex]; l++ ) {
	opanodeindex = pfathers[gfti][fathernodeindex][l];
	
	/* first check if we already had this ef between these pnodes,
	 * or if we can add the ef to an edge with its colleagues.
	 */
	for ( ll = 0; ll < num_supportgraph[gfti]; ll++ ) {
	  if ( supportgraph[gfti][ll].pnodefrom != fwdp[j] ) continue;
	  if ( supportgraph[gfti][ll].pnodeto != opanodeindex ) continue;
	  for ( lll = 0; lll < supportgraph[gfti][ll].num_E; lll++ ) {
	    if ( supportgraph[gfti][ll].E[lll] == fatheref ) break;
	  }
	  if ( lll < supportgraph[gfti][ll].num_E ) break;

	  if ( supportgraph[gfti][ll].num_E == 0 ) continue;
	  for ( lll = 0; lll < gef_conn[fatheref].num_S; lll++ ) {
	    if ( gef_conn[fatheref].S[lll] == supportgraph[gfti][ll].E[0] ) break;
	  }
	  if ( lll < gef_conn[fatheref].num_S ) {
	    /* edge ll is a colleague going to the same opa! add this effect, and update P.
	     */
	    supportgraph[gfti][ll].E[supportgraph[gfti][ll].num_E] = fatheref;
	    supportgraph[gfti][ll].num_E++;
	    supportgraph[gfti][ll].P += gef_conn[fatheref].eff_p;
	    break;
	  }
	}
	if ( ll < num_supportgraph[gfti] ) {
	  /* we either had this already, or we have added the effect to its
	   * colleagues. proceed to next opa.
	   */
	  continue;
	}
	
	/* we don't have this guy yet. insert new fact node into open list; create a new edge.
	 */
	for ( l = 0; l < num_fwdp; l++ ) {
	  if ( fwdp[l] == opanodeindex ) break;
	}
	if ( l == num_fwdp ) {
	  fwdp[num_fwdp] = opanodeindex;
	  num_fwdp++;
	}
	
	if ( num_supportgraph[gfti] >= 10000 ) {
	  printf("\ntoo many edegs in support graph! increase 10000 in select_disjunctions_supported_by_formula\n\n");
	  exit( 1 );
	}
	supportgraph[gfti][num_supportgraph[gfti]].pnodefrom = fwdp[j];
	supportgraph[gfti][num_supportgraph[gfti]].pnodeto = opanodeindex;
	supportgraph[gfti][num_supportgraph[gfti]].t = pt[gfti][opanodeindex];
	supportgraph[gfti][num_supportgraph[gfti]].realtime = 
	  pt[gfti][opanodeindex] - lpath_U_length + 1;
	supportgraph[gfti][num_supportgraph[gfti]].E[0] = fatheref;
	supportgraph[gfti][num_supportgraph[gfti]].num_E = 1;
	supportgraph[gfti][num_supportgraph[gfti]].P = gef_conn[fatheref].eff_p;
	supportgraph[gfti][num_supportgraph[gfti]].active = FALSE;
	num_supportgraph[gfti]++;
      } /* endfor l over fathers of father probnode */
    } /* endfor k over fathers */
    j++;
  } /* endwhile j over fwd ps */

}



void propagate_support_graph( int gfti, 
			      UEdgeNode **supportgraph, int *num_supportgraph,
			      UftNode_pointer **p,
			      Bool prune_maxpeffather )

{

  static Bool fc = TRUE;
  static int *bwdp;
  /* storage for which outcomes of which different effects are fathers.
   * gather this stuff and then compute the weight of the node
   * when it gets expanded.
   */
  static int ***nondet_accumulated_efs;
  static double ***nondet_accumulated_weights;/* what has this guy got from his parents? */
  static Bool ***nondet_accumulated_fulls;/* does this guy have a full parent? */
  static int **num_nondet_accumulated_efs;
  static int *num_nondet_accumulated;

//   /* just for debugging...
//    */
//   static double *prevweight;



  int i, j, k, l;
  int num_bwdp, oldp, newp, ef, groupid;

  double weight;
  Bool have_full_parents;

  double maxpeffweight;
  int maxef;



  if ( fc ) {
    bwdp = ( int * ) calloc( lMAX_PATHNODES, sizeof( int ) );

    nondet_accumulated_efs = ( int *** ) calloc( lMAX_PATHNODES, sizeof( int ** ) );
    nondet_accumulated_weights = ( double *** ) calloc( lMAX_PATHNODES, sizeof( double ** ) );
    nondet_accumulated_fulls = ( Bool *** ) calloc( lMAX_PATHNODES, sizeof( Bool ** ) );
    num_nondet_accumulated_efs = ( int ** ) calloc( lMAX_PATHNODES, sizeof( int * ) );
    num_nondet_accumulated = ( int * ) calloc( lMAX_PATHNODES, sizeof( int ) );
    for ( i = 0; i < lMAX_PATHNODES; i++ ) {
      nondet_accumulated_efs[i] = ( int ** ) calloc( 5, sizeof( int * ) );
      nondet_accumulated_weights[i] = ( double ** ) calloc( 5, sizeof( double * ) );
      nondet_accumulated_fulls[i] = ( Bool ** ) calloc( 5, sizeof( Bool * ) );
      num_nondet_accumulated_efs[i] = ( int * ) calloc( 5, sizeof( int ) );
      for ( j = 0; j < 5; j++ ) {
	nondet_accumulated_efs[i][j] = ( int * ) calloc( MAX_OUTCOMES, sizeof( int ) );
	nondet_accumulated_weights[i][j] = ( double * ) calloc( MAX_OUTCOMES, sizeof( double ) );
	nondet_accumulated_fulls[i][j] = ( Bool * ) calloc( MAX_OUTCOMES, sizeof( Bool ) );
      }
    }

//     prevweight = ( double * ) calloc( lMAX_PATHNODES, sizeof( double ) );

    fc = FALSE;
  }



  bwdp[0] = 0;
  num_bwdp = 1;
//   prevweight[0] = p[gfti][0]->weight;

  switch ( gcmd_line.weightprop ) {
  case 0:
    p[gfti][0]->weight = 1.0;
    break;
  case 1:
    p[gfti][0]->weight = 0.0;
    break;
  case 2:
    p[gfti][0]->weight = 1.0;
    break;
  }
  p[gfti][0]->has_full_parents = TRUE;


  
  i = 0;
  while ( i < num_bwdp ) {
    oldp = bwdp[i];



    /* go over accumulated stuff;
     * if only single father is wanted, simply collect the maximum weight and
     * update below loop.
     */
    maxpeffweight = -1;
    maxef = -1;
    for ( j = 0; j < num_nondet_accumulated[oldp]; j++ ) {      

      weight = 0.0;
      have_full_parents = TRUE;

      ef = nondet_accumulated_efs[oldp][j][0];
      if ( gef_conn[ef].num_S > num_nondet_accumulated_efs[oldp][j] ) {
	have_full_parents = FALSE;
      }

      for ( k = 0; k < num_nondet_accumulated_efs[oldp][j]; k++ ) {
	ef = nondet_accumulated_efs[oldp][j][k];

	/* first, right here fix the weight of this guy!!
	 */
	if ( gcmd_line.weightprop == 1 ) { /* independence */
	  nondet_accumulated_weights[oldp][j][k] = 
	    (1.0 - nondet_accumulated_weights[oldp][j][k]);
	}
	nondet_accumulated_weights[oldp][j][k] *= gef_conn[ef].eff_p;
	if ( nondet_accumulated_weights[oldp][j][k] >= gef_conn[ef].eff_p ) {
	  nondet_accumulated_weights[oldp][j][k] = gef_conn[ef].eff_p;
	  if ( !nondet_accumulated_fulls[oldp][j][k] ) {
	    nondet_accumulated_weights[oldp][j][k] -= DELTA;
	  }
	}
	
	if ( prune_maxpeffather ) {
	  if ( maxef == -1 || nondet_accumulated_weights[oldp][j][k] > maxpeffweight ) {
	    maxpeffweight = nondet_accumulated_weights[oldp][j][k];
	    maxef = ef;
	  }
	} else {
	  /* then, add this into the accumulated weight...
	   */
	  weight += nondet_accumulated_weights[oldp][j][k];
	  if ( !nondet_accumulated_fulls[oldp][j][k] ) {
	    have_full_parents = FALSE;
	  }
	}
      }

      if ( !prune_maxpeffather ) {
	switch ( gcmd_line.weightprop ) {
	case 0:
	  p[gfti][oldp]->weight += weight;
	  break;
	case 1:
	  p[gfti][oldp]->weight *= (1.0 - weight);
	  break;
	case 2:
	  if ( weight > p[gfti][oldp]->weight ) {
	    p[gfti][oldp]->weight = weight;
	  }
	  break;
	}
	if ( have_full_parents ) {
	  p[gfti][oldp]->has_full_parents = TRUE;
	}
      }
 
    } /* endfor j over accumulated guys */

    if ( prune_maxpeffather && num_nondet_accumulated[oldp] > 0 ) {
      if ( maxef == -1 ) {
	printf("\nsupport graph maxpeffather prune accumulated > 0, no max taken?\n\n");
	exit( 1 );
      }
      if ( gcmd_line.R && gcmd_line.debug >= 7 ) {
	printf("\nsupport graph PRUNE: max pef father of pnode %d is ef %d of ", 
	       oldp, maxef);
	print_op_name(gef_conn[maxef].op);
      }

      switch ( gcmd_line.weightprop ) {
      case 0:
	p[gfti][oldp]->weight += maxpeffweight;
	break;
      case 1:
	p[gfti][oldp]->weight *= (1.0 - maxpeffweight);
	break;
      case 2:
	if ( maxpeffweight > p[gfti][oldp]->weight ) {
	  p[gfti][oldp]->weight = maxpeffweight;
	}
	break;
      }

      /* and now, prune! de-activate all edges starting from oldp that are constituted
       * by probeffes other than maxef; from the edges containing maxef,
       * remove all other efs!
       */
      for ( j = 0; j < num_supportgraph[gfti]; j++ ) {
	if ( !supportgraph[gfti][j].active ) {
	  continue;
	}
	if ( supportgraph[gfti][j].pnodefrom != oldp ) {
	  continue;
	}
	if ( supportgraph[gfti][j].num_E == 0 ||
	     gef_conn[supportgraph[gfti][j].E[0]].eff_p >= 1.0 ) {
	  continue;
	}

	k = 0;
	while ( k < supportgraph[gfti][j].num_E ) {
	  if ( supportgraph[gfti][j].E[k] == maxef ) {
	    k++;
	    continue;
	  }
	  if ( gcmd_line.R && gcmd_line.debug >= 7 ) {
	    printf("\nsupport graph PRUNE: removing ef %d of ", supportgraph[gfti][j].E[k]);
	    print_op_name(gef_conn[supportgraph[gfti][j].E[k]].op);
	    printf("from edge %d --> %d", supportgraph[gfti][j].pnodefrom, supportgraph[gfti][j].pnodeto);
	  }
	  for ( l = k; l < supportgraph[gfti][j].num_E-1; l++ ) {
	    supportgraph[gfti][j].E[l] = supportgraph[gfti][j].E[l+1];
	  }
	  supportgraph[gfti][j].num_E--;
	}

	if ( supportgraph[gfti][j].num_E == 0 ) {
	  supportgraph[gfti][j].active = FALSE;
	  if ( gcmd_line.R && gcmd_line.debug >= 7 ) {
	    printf("\nsupport graph PRUNE: removing edge %d --> %d", 
		   supportgraph[gfti][j].pnodefrom, supportgraph[gfti][j].pnodeto);
	  }
	}
      } /* endfor j over support graph prune loop */
    } /* endif if we're in prune mode and this guy has at least one probeff father */



    /* now fix the weight!
     */
    if ( gcmd_line.weightprop == 1 ) { /* independence */
      p[gfti][oldp]->weight = (1.0 - p[gfti][oldp]->weight);
    }
    if ( p[gfti][oldp]->weight >= 1.0 ) {
      p[gfti][oldp]->weight = 1.0;
      if ( !p[gfti][oldp]->has_full_parents ) {
	p[gfti][oldp]->weight -= DELTA;
      }
    }
    if ( gcmd_line.R && gcmd_line.debug >= 7 ) {
      printf("\npropagate support: fixed weight of pnode %d ", oldp);
      if ( p[gfti][oldp]->ft != -1 ) {
	print_ft_name(p[gfti][oldp]->ft);
      } else {
	printf("ef %d of ", p[gfti][oldp]->ef);
	print_op_name(gef_conn[p[gfti][oldp]->ef].op);
      }
      printf(" to %lf", p[gfti][oldp]->weight);
    }
//     if ( prevweight[oldp] > p[gfti][oldp]->weight ||
// 	 prevweight[oldp] < p[gfti][oldp]->weight ) {
//       printf("\nNEQ: %lf != %lf\n\n", 
// 	     prevweight[oldp], p[gfti][oldp]->weight);
//       exit( 1 );
//     }



    /* now expand! naive: just walk over all edges... optimize?
     */
    for ( j = 0; j < num_supportgraph[gfti]; j++ ) {
      if ( !supportgraph[gfti][j].active ) {
	continue;
      }
      if ( supportgraph[gfti][j].pnodeto != oldp ) {
	continue;
      }



      /* new node?
       */
      newp = supportgraph[gfti][j].pnodefrom;
      for ( k = 0; k < num_bwdp; k++ ) {
	if ( bwdp[k] == newp ) break;
      }
      if ( k == num_bwdp ) {
// 	prevweight[newp] = p[gfti][newp]->weight;
	bwdp[num_bwdp] = newp;
	num_bwdp++;

	/* initialize weight to neutral element.
	 */
	switch ( gcmd_line.weightprop ) {
	case 0:
	  p[gfti][newp]->weight = 0.0;
	  break;
	case 1:
	  p[gfti][newp]->weight = 1.0;
	  break;
	case 2:
	  p[gfti][newp]->weight = 0.0;
	  break;
	}
	p[gfti][newp]->has_full_parents = FALSE;

	num_nondet_accumulated[newp] = 0;
      }



      /* NOOP or det effect
       */
      if ( supportgraph[gfti][j].num_E == 0 ||
	   gef_conn[supportgraph[gfti][j].E[0]].eff_p >= 1.0 ) {

	switch ( gcmd_line.weightprop ) {
	case 0:
	  p[gfti][newp]->weight += p[gfti][oldp]->weight;
	  break;
	case 1:
	  p[gfti][newp]->weight *= (1.0 - p[gfti][oldp]->weight);
	  break;
	case 2:
	  if ( p[gfti][oldp]->weight > p[gfti][newp]->weight ) {
	    p[gfti][newp]->weight = p[gfti][oldp]->weight;
	  }
	  break;
	}
	if ( p[gfti][oldp]->has_full_parents ) {
	  p[gfti][newp]->has_full_parents = TRUE;
	}

	continue;
      }



      /* nondet eff! do the accumulation.
       * first see if one of the guys already there matches 
       * this guy 
       */
      ef = supportgraph[gfti][j].E[0];
      for ( k = 0; k < num_nondet_accumulated[newp]; k++ ) {
	for ( l = 0; l < gef_conn[ef].num_S; l++ ) {
	  if ( gef_conn[ef].S[l] == nondet_accumulated_efs[newp][k][0] ) {
	    break;
	  }
	}
	if ( l < gef_conn[ef].num_S ) {
	  break;
	}
      }
      if ( k < num_nondet_accumulated[newp] ) {
	/* such a guy is already here! merge the new guys
	 * into the old guys.
	 */
	groupid = k;
	for ( k = 0; k < supportgraph[gfti][j].num_E; k++ ) {
	  for ( l = 0; l < num_nondet_accumulated_efs[newp][groupid]; l++ ) {
	    if ( nondet_accumulated_efs[newp][groupid][l] ==
		 supportgraph[gfti][j].E[k] ) {
	      break;
	    }
	  }
	  if ( l < num_nondet_accumulated_efs[newp][groupid] ) {
	    /* have this guy already!
	     */
	    switch ( gcmd_line.weightprop ) {
	    case 0:
	      nondet_accumulated_weights[newp][groupid][l] += p[gfti][oldp]->weight;
	      break;
	    case 1:
	      nondet_accumulated_weights[newp][groupid][l] *= (1.0 - p[gfti][oldp]->weight);
	      break;
	    case 2:
	      if ( p[gfti][oldp]->weight > nondet_accumulated_weights[newp][groupid][l] ) {
		nondet_accumulated_weights[newp][groupid][l] = p[gfti][oldp]->weight;
	      }
	      break;
	    }
	    if ( p[gfti][oldp]->has_full_parents ) {
	      nondet_accumulated_fulls[newp][groupid][l] = TRUE;
	    }
	    continue;
	  }

	  /* this is a new ef in here.
	   */
	  nondet_accumulated_efs[newp][groupid][num_nondet_accumulated_efs[newp][groupid]] =
	    supportgraph[gfti][j].E[k];
	  switch ( gcmd_line.weightprop ) {
	  case 0:
	    nondet_accumulated_weights[newp][groupid][num_nondet_accumulated_efs[newp][groupid]] = 
	      p[gfti][oldp]->weight;
	    break;
	  case 1:
	    nondet_accumulated_weights[newp][groupid][num_nondet_accumulated_efs[newp][groupid]] = 
	      (1.0 - p[gfti][oldp]->weight);
	    break;
	  case 2:
	    nondet_accumulated_weights[newp][groupid][num_nondet_accumulated_efs[newp][groupid]] = 
	      p[gfti][oldp]->weight;
	    break;
	  }
	  nondet_accumulated_fulls[newp][groupid][num_nondet_accumulated_efs[newp][groupid]] =
	    p[gfti][oldp]->has_full_parents;
	  num_nondet_accumulated_efs[newp][groupid]++;
	} /* endfor k over the guys in here */

	continue;
      } /* endif we already have this group of outcomes */



      /* no such guy is here yet. open a new group.
       */
      if ( num_nondet_accumulated[newp] == 5 ) {
	printf("\ntoo many different groups of outcomes! increase 5 in propagate_support_graph\n\n");
	exit( 1 );
      }
      for ( k = 0; k < supportgraph[gfti][j].num_E; k++ ) {
	nondet_accumulated_efs[newp][num_nondet_accumulated[newp]][k] = 
	  supportgraph[gfti][j].E[k];
	switch ( gcmd_line.weightprop ) {
	case 0:
	  nondet_accumulated_weights[newp][num_nondet_accumulated[newp]][k] = 
	    p[gfti][oldp]->weight;
	  break;
	case 1:
	  nondet_accumulated_weights[newp][num_nondet_accumulated[newp]][k] = 
	    (1.0 - p[gfti][oldp]->weight);
	  break;
	case 2:
	  nondet_accumulated_weights[newp][num_nondet_accumulated[newp]][k] = 
	    p[gfti][oldp]->weight;
	  break;
	}
	nondet_accumulated_fulls[newp][num_nondet_accumulated[newp]][k] = 
	  p[gfti][oldp]->has_full_parents;
      }
      num_nondet_accumulated_efs[newp][num_nondet_accumulated[newp]] = 
	supportgraph[gfti][j].num_E;
      num_nondet_accumulated[newp]++;



    } /* endfor j over the support graph edges */

    i++;
  } /* endwhile i over the open list */

}

  

/* shit!!!??? :
 * to make this work for -h 2, we cannot collect the pathops
 * from the implication graph -- they won't be in there!
 */
void minimize_support_graph( UftNode_pointer *goalU, int num_goalU,
			     UEdgeNode **supportgraph, int *num_supportgraph,
			     int **leafs, int **leafps, int *num_leafs, Bool **is_leaf,
			     int **probleafps, int *num_probleafs,
			     UftNode_pointer **p )

{

  static Bool fc = TRUE;
  /* the ops along the path, array index (-1)*realtime
   */
  static int *pathops;
  /* the ops in the rplan, array index [realtime][nr]
   */
  static int **rplanops, *num_rplanops;

  int num_path, num_rplan;/* the nrs of time steps */
  int totalnumrplan;/* the nr of steps inthe rplan! */

  int gfti, i, j, op, optime;
  
  int pathtime, pathop, rplantime, rmopindex;
  Bool ok;
  

  if ( fc ) {
    pathops = ( int * ) calloc( MAX_PLAN_LENGTH+1, sizeof( int ) );
    rplanops = ( int ** ) calloc( RELAXED_STEPS, sizeof( int * ) );
    num_rplanops = ( int * ) calloc( RELAXED_STEPS, sizeof( int ) );
    for ( i = 0; i < RELAXED_STEPS; i++ ) {
      rplanops[i] = ( int * ) calloc( 100, sizeof( int ) );
    }

    fc = FALSE;
  }





  /* first, collect the ops in the path and in the rplan!!
   * (as ementioned in the support graphs...)
   */

  for ( i = 0; i < MAX_PLAN_LENGTH+1; i++ ) {
    pathops[i] = -1;/* just for debugging */
  }
  for ( i = 0; i < RELAXED_STEPS; i++ ) {
    num_rplanops[i] = 0;/* this is necessary! */
  }

  num_path = 0;
  num_rplan = 0;
  totalnumrplan = 0;
  for ( gfti = 0; gfti < num_goalU; gfti++ ) {
    for ( i = 0; i < num_supportgraph[gfti]; i++ ) {
      if ( supportgraph[gfti][i].num_E == 0 ) {
	continue;/* NOOP */
      }

      op = gef_conn[supportgraph[gfti][i].E[0]].op;
      optime = supportgraph[gfti][i].realtime - 1;/* realtime is the time at the end of the edge */

      if ( optime < 0 ) {
	/* pathop! simply insert.
	 */
	if ( pathops[(-1)*optime] != -1 && pathops[(-1)*optime] != op ) {
	  printf("\ndifferent ops on path??\n\n");
	  exit( 1 );
	}

	pathops[(-1)*optime] = op;

	if ( (-1)*optime > num_path ) {
	  num_path = (-1)*optime;
	}

	continue;
      }

      /* rplan op! set-insert into this time step.
       */
      for ( j = 0; j < num_rplanops[optime]; j++ ) {
	if ( rplanops[optime][j] == op ) break;
      }
      if ( j < num_rplanops[optime] ) continue;

      if ( num_rplanops[optime] == 100 ) {
	printf("\ntoo many ops in rplan time step. incrase 100 in minimize_support_graph.\n\n");
	exit( 1 );
      }
      rplanops[optime][num_rplanops[optime]] = op;
      num_rplanops[optime]++;
      totalnumrplan++;

      if ( optime > num_rplan ) {
	num_rplan = optime;
      }

    } /* endfor i over support graph edges for rplan collection */

  } /* endfor gfti over goals for rplan collection */


  if ( gcmd_line.R && gcmd_line.debug >= 3 ) {
    printf("\n\nsupport graph minimization collected path:");
    for ( i = num_path; i > 0; i-- ) {
      if ( pathops[i] == -1 ) {
	continue;
      }
      printf("\ntime %d: ", (-1)*i);
      print_op_name(pathops[i]);
    }
    printf("\nsupport graph minimization collected support graph rplan:");
    for ( i = 0; i <= num_rplan; i++ ) {
      printf("\ntime %d: ", i);
      for ( j = 0; j < num_rplanops[i]; j++ ) {
	print_op_name(rplanops[i][j]); printf(" ");
      }
    }
  }







  /* NOW: replace NOOPS!
   * i.e., for every NOOP edge p-->p in the graph, see if there is an op
   * in the relaxed plan with a prob eff ef that has p in its cond and deletes p.
   * if so, take the first such op and replace the NOOP with all colleagues
   * of ef that do NOT delete p.
   */
  if ( gcmd_line.replacenoops ) {
    replace_support_graph_NOOPs(pathops, num_path,
				rplanops, num_rplanops, num_rplan,
				goalU, num_goalU,
				supportgraph, num_supportgraph,
				p);
  }









  /* MINIMIZATION LOOP!!
   *
   * outer loop: go over all actions on the path, from start
   * to end.
   */
  for ( pathtime = num_path; pathtime > 0; pathtime-- ) {
    pathop = pathops[pathtime];
    if ( pathop == -1 ) {
      continue;
    }
    if ( gcmd_line.R && gcmd_line.debug >= 3 ) {
      printf("\nsupport graph minimization try remove ");
      print_op_name(pathop);
    }

    if ( totalnumrplan <= 1 ) {
      /* don't minimize the last action away!
       */
      break;
    }


    /* inner loop: go over all steps in the rplan, from top to
     * bottom
     */
    for ( rplantime = num_rplan; rplantime >= 0; rplantime-- ) {

      for ( i = 0; i < num_rplanops[rplantime]; i++ ) {
	if ( rplanops[rplantime][i] == pathop ) break;
      }
      if ( i == num_rplanops[rplantime] ) {
	/* this op is not in here.
	 */
	continue;
      }
      if ( gcmd_line.R && gcmd_line.debug >= 3 ) {
	printf("\nsupport graph minimization ");
	print_op_name(pathop);
	printf(" found in rplan step %d", rplantime);
      }



      /* this op IS in here! remove it.
       */
      for ( gfti = 0; gfti < num_goalU; gfti++ ) {
	for ( i = 0; i < num_supportgraph[gfti]; i++ ) {
	  if ( supportgraph[gfti][i].realtime != rplantime + 1) {
	    continue;
	  }
	  if ( supportgraph[gfti][i].num_E == 0 ) {
	    continue;
	  }
	  if ( gef_conn[supportgraph[gfti][i].E[0]].op != pathop ) {
	    continue;
	  }
	  /* got him!
	   */
	  supportgraph[gfti][i].active = FALSE;
	  if ( gcmd_line.R && gcmd_line.debug >= 3 ) {
	    printf("\nsupport graph minimization remove edge %d --> %d (realtime %d)",
		   supportgraph[gfti][i].pnodefrom, supportgraph[gfti][i].pnodeto,
		   supportgraph[gfti][i].realtime);
	  }
	}
      } /* endfor gtfi: de-activate these edges */
      


      /* now check the goal suppport!
       */
      ok = support_graph_supports_goal(goalU, num_goalU,
				       supportgraph, num_supportgraph,
				       leafs, leafps, num_leafs, is_leaf,
				       probleafps, num_probleafs, 
				       p);
      


      /* in any case, break; try only at top layer in which
       * action is contained.
       */

      if ( ok ) {
	/* yes! remove this op from rplan!
	 */
	if ( gcmd_line.R && gcmd_line.debug >= 3 ) {
	  printf("\nsupport graph minimization OK! remove ");
	  print_op_name(pathop);
	  printf(" from rplan step %d", rplantime);
	}
	
	for ( i = 0; i < num_rplanops[rplantime]; i++ ) {
	  if ( rplanops[rplantime][i] == pathop ) break;
	}
	if ( i == num_rplanops[rplantime] ) {
	  printf("\nson quatsch\n\n");
	  exit( 1 );
	}
	rmopindex = i;
	for ( i = rmopindex; i < num_rplanops[rplantime]-1; i++ ) {
	  rplanops[rplantime][i] = rplanops[rplantime][i+1];
	}
	num_rplanops[rplantime]--;
	totalnumrplan--;

	if ( gcmd_line.R && gcmd_line.debug >= 3 ) {
	  for ( i = 0; i <= num_rplan; i++ ) {
	    printf("\ntime %d: ", i);
	    for ( j = 0; j < num_rplanops[i]; j++ ) {
	      print_op_name(rplanops[i][j]); printf(" ");
	    }
	  }
	}

	break;
      }
      if ( gcmd_line.R && gcmd_line.debug >= 3 ) {
	printf("\nsupport graph minimization NOT OK! put back ");
	print_op_name(pathop);
	printf(" into rplan step %d", rplantime);
      }



      /* No! we have to undo the de-activation...
       */
      for ( gfti = 0; gfti < num_goalU; gfti++ ) {
	for ( i = 0; i < num_supportgraph[gfti]; i++ ) {
	  if ( supportgraph[gfti][i].realtime != rplantime + 1) {
	    continue;
	  }
	  if ( supportgraph[gfti][i].num_E == 0 ) {
	    continue;
	  }
	  if ( gef_conn[supportgraph[gfti][i].E[0]].op != pathop ) {
	    continue;
	  }
	  supportgraph[gfti][i].active = TRUE;
	  if ( gcmd_line.R && gcmd_line.debug >= 3 ) {
	    printf("\nsupport graph minimization put back edge %d --> %d (realtime %d)",
		   supportgraph[gfti][i].pnodefrom, supportgraph[gfti][i].pnodeto,
		   supportgraph[gfti][i].realtime);
	  }
	}
      } /* endfor gtfi: de-de-activate these edges */
      /* NOTE!!!???: various runtime optimizations are possible here regarding eg
       * multiple times the same pathop does not need be tried to remove
       * if it didn't work before.
       *
       * ALSO: maybe it won't work anyway if it doesn't work at the top??!!
       * ie is there a monotone behavior, removing further below can only
       * hurt more? it is so in this logistics example...
       *
       * NO! it might be that further below the op makes less of a difference
       * because not all of its edges are enable -- see dice fake example
       * where at step 0 the dice has not yet been faked.
       * in this example, it hurts to remove the step 0 throw op.
       * so just change to "try only at top layer" .. I guess this is just as
       * justified/not justified as the method trying at every layer...
       */

      break;
    } /* endfor rplantime over rplan time steps */

  } /* endfor pathtime over path op time steps */

  /* that's it!?!
   */

}


  

/* tells us if this activation status of the support graph still
 * sufficiently supports the goals!
 */
Bool support_graph_supports_goal( UftNode_pointer *goalU, int num_goalU,
				  UEdgeNode **supportgraph, int *num_supportgraph,
				  int **leafs, int **leafps, int *num_leafs, Bool **is_leaf,
				  int **probleafps, int *num_probleafs,
				  UftNode_pointer **p )

{

  static Bool fc = TRUE;

  static double **leafweights;
  static double **probleafweights;

  static double *aggregated_peff_weight;
  static Bool *haveweighted;

  static int *multiID;

  int gfti, i, j, k;

  double weightsum;
  int ef_;

  double gprob = -1, curr_weight;
  int blaa;



  if ( fc ) {
    leafweights = ( double ** ) calloc( ggoal_state.num_F, sizeof( double * ) );
    probleafweights = ( double ** ) calloc( ggoal_state.num_F, sizeof( double * ) );
    for ( i = 0; i < ggoal_state.num_F; i++ ) {
      leafweights[i] = ( double * ) calloc( gnum_ft_conn, sizeof( double ) );
      /* the 1000 is a hack; it is made sure above in
       * select disjunctions that there are no more probleafs than that.
       */
      probleafweights[i] = ( double * ) calloc( 1000, sizeof( double ) );
    }

    aggregated_peff_weight = ( double * ) calloc( ggoal_state.num_F, sizeof( double ) );
    haveweighted = ( Bool * ) calloc( 1000, sizeof( Bool ) );

    multiID = ( int * ) calloc( gnum_ft_conn, sizeof( Bool * ) );

    fc = FALSE;
  }






  /* go over all the goals, and compute their leaf weights.
   */
  for ( gfti = 0; gfti < num_goalU; gfti++ ) {

    /* first, initialize the leaf weights to 0 -- 
     * if they aren't reached at all then they won't
     * change their value.
     */
    for ( i = 0; i < num_leafs[gfti]; i++ ) {
      p[gfti][leafps[gfti][i]]->weight = 0.0;
    }
    for ( i = 0; i < num_probleafs[gfti]; i++ ) {
      p[gfti][probleafps[gfti][i]]->weight = 0.0;
    }

    /* now propagate the support!
     */
    propagate_support_graph(gfti, supportgraph, num_supportgraph, p, FALSE);

    /* ... and remember the leaf weights.
     */
    for ( i = 0; i < num_leafs[gfti]; i++ ) {
      leafweights[gfti][i] = p[gfti][leafps[gfti][i]]->weight;
    }
    for ( i = 0; i < num_probleafs[gfti]; i++ ) {
      probleafweights[gfti][i] = p[gfti][probleafps[gfti][i]]->weight;
    }
    

    if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
      for ( gfti = 0; gfti < num_goalU; gfti++ ) {
	printf("\nsupport graph support check: support graph propagated! goal nr. %d ", gfti); 
	print_ft_name( goalU[gfti]->ft );
	printf(" has leafs:");
	for ( i = 0; i < num_leafs[gfti]; i++ ) {
	  printf(" (weight %lf: ", leafweights[gfti][i]);
	  print_ft_name( p[gfti][leafps[gfti][i]]->ft );
	  printf(")");
	}
	printf(" and prob leafs:");
	for ( i = 0; i < num_probleafs[gfti]; i++ ) {
	  printf(" (weight %lf: ef %d of ", 
		 probleafweights[gfti][i], p[gfti][probleafps[gfti][i]]->ef);
	  print_op_name(gef_conn[p[gfti][probleafps[gfti][i]]->ef].op);
	  printf(")");
	}
	fflush(stdout);
      }
    }
  }








  /* july06: next, compute the weight aggregated from
   * uncond prob effs, for each goal fact.
   */
  for ( gfti = 0; gfti < num_goalU; gfti++ ) {
    aggregated_peff_weight[gfti] = 1.0;
    for ( i = 0; i < num_probleafs[gfti]; i++ ) {
      haveweighted[i] = FALSE;
    }
    for ( i = 0; i < num_probleafs[gfti]; i++ ) {
      if ( haveweighted[i] ) {
	continue;
      }
      weightsum = 0.0;
      for ( j = 0; j < gef_conn[p[gfti][probleafps[gfti][i]]->ef].num_S; j++ ) {
	ef_ = gef_conn[p[gfti][probleafps[gfti][i]]->ef].S[j];
	for ( k = 0; k < num_probleafs[gfti]; k++ ) {
	  if ( p[gfti][probleafps[gfti][k]]->ef == ef_ && 
	       p[gfti][probleafps[gfti][k]]->Utime == p[gfti][probleafps[gfti][i]]->Utime ) {
	    /* found good guy.
	     */
	    weightsum += probleafweights[gfti][k];
	    haveweighted[k] = TRUE;/* in particular, this will weight "i" itself since that is in S */
	    break;
	  }
	}
      }

      aggregated_peff_weight[gfti] *= (1.0 - weightsum);
    }
    aggregated_peff_weight[gfti] = (1.0 - aggregated_peff_weight[gfti]);
    if ( aggregated_peff_weight[gfti] >= 1 ) {
      /* this shouldn't happen...
       */
//       aggregated_peff_weight[gfti] -= DELTA;
      printf("\nr aggregated weight for goal %d is %lf >= 1?\n\n", gfti, aggregated_peff_weight[gfti]);
      for ( i = 0; i < num_probleafs[gfti]; i++ ) {
	printf("\nleaf %d pnode %d: ef %d of ", i, probleafps[gfti][i], p[gfti][probleafps[gfti][i]]->ef);
	print_op_name(gef_conn[p[gfti][probleafps[gfti][i]]->ef].op);
	printf(" at Utime %d, weight %lf", p[gfti][probleafps[gfti][i]]->Utime, probleafweights[gfti][i]);
      }
      printf("\n\n");
      exit( 1 );
    }
  } /* gfti over all goal facts: compute aggregated noncond prob weight loop */








  /* july06: next, if indimulti stuff is wanted (cmd line) and possible (gindimulti, in particular h=1)
   * check if it applies here; if yes, compute the goal probability.
   */ 
  if ( gcmd_line.simulation_wmc && gindimulti ) {
    gprob = 1.0;
    for ( gfti = 0; gfti < num_goalU; gfti++ ) {
      multiID[gfti] = -1;
      curr_weight = 0.0;

      for ( i = 0; i < num_leafs[gfti]; i++ ) {
	if ( gft_conn[leafs[gfti][i]].parsed_weight < 0 ) {
	  /* bad guy! not part of a multivar.
	   */
	  break;
	}

	if ( multiID[gfti] == -1 ) {
	  multiID[gfti] = gft_conn[leafs[gfti][i]].multiID;
	  for ( j = 0; j < gfti; j++ ) {
	    if ( multiID[j] == multiID[gfti] ) {
	      /* bad guy! must be *different* multi vars for diff disjunctions!
	       */
	      break;
	    }
	  }
	  if ( j < gfti ) {
	    break;
	  }
	} /* endif see about multi ID */

	if ( gft_conn[leafs[gfti][i]].multiID != multiID[gfti] ) {
	  /* bad guy! all leafs must be from the same multi var.
	   */
	  break;
	}

	/* so far everything is groovy -- update the weight.
	 * take the parsed weight of this multivar value
	 * and directly multiply it with the implication graph weight.
	 */
	curr_weight += (gft_conn[leafs[gfti][i]].parsed_weight * leafweights[gfti][i]);
      } /* endfor i over the leafs of this goal fact */
      if ( i < num_leafs[gfti] ) {
	/* this leaf is a bad guy for some reason
	 */
	break;
      }

      /* looks good so far: update the gprob!
       */
      curr_weight = (1.0 - ((1.0 - curr_weight) * (1.0 - aggregated_peff_weight[gfti])));
      /* just a debugger...
       */
      blaa = ((int) (curr_weight * 1000000));
      if ( blaa > 1000000 ) {
	printf("\n\ncurr weight %f > 1.0??\n\n", curr_weight);
	exit( 1 );
      }
      gprob *= curr_weight;
    } /* gfti over all goal facts: indimulti loop */
    if ( gfti < num_goalU ) {
      gprob = -1;/* so we will know later that we could not compute the gprob... */
    }
  } /* endif do we want to, and can, do inidimulti? */






  /* final debug printout before decisions are taken...
   */
  if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
    for ( gfti = 0; gfti < num_goalU; gfti++ ) {
      printf("\nsupport graph support check: goal fact "); print_ft_name( goalU[gfti]->ft );
      printf(" has leafs:");
      for ( i = 0; i < num_leafs[gfti]; i++ ) {
	printf(" (");
	print_ft_name( leafs[gfti][i] );
	printf(" weight %lf)", leafweights[gfti][i]);
      }
      printf(" and noncond prob leafs:");
      for ( i = 0; i < num_probleafs[gfti]; i++ ) {
	printf(" (ef %d at Utime %d of action ", p[gfti][probleafps[gfti][i]]->ef, p[gfti][probleafps[gfti][i]]->Utime);
	print_op_name(gef_conn[p[gfti][probleafps[gfti][i]]->ef].op);
	printf(" weight %lf)", probleafweights[gfti][i]);
      }
      printf(" giving the aggregated weight %lf", aggregated_peff_weight[gfti]);
      printf("\nr now checking for support by initial/state formula.");
      fflush(stdout);
    }

    printf("\nindimulti wanted? %d possible? %d gprob: %lf", 
	   gcmd_line.simulation_wmc, gindimulti, gprob);
  }








  /* july06: if there are no leafs at all, just probleafs, then we can simply 
   * compute the outcome prob.
   *
   * Actually this is just a special case of simply collecting the factor
   * for every goal whose disj is empty. don't bother... (will be a unit clause for wmc, so..)
   */
  for ( gfti = 0; gfti < num_goalU; gfti++ ) {
    if ( num_leafs[gfti] > 0 ) {
      break;
    }
  }
  if ( gcmd_line.simulation_wmc && gfti == num_goalU ) {
    /* this is a special case of the above indimulti computation; 
     * however, that computation is only done if indimulti is possible
     * and applies, so repeat it here.
     */
    gprob = 1.0;
    for ( gfti = 0; gfti < num_goalU; gfti++ ) {
      gprob *= aggregated_peff_weight[gfti];
    }    
    if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
      printf("\nr only-probleafs treatment applies, computed P %f", gprob);
    }
    gsim_wmc_calls++;
    lthisgoalprob = gprob;
    if ( gprob >= ggoal_probability ) {
      if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
	printf(" -- high enough!");
      }
      return TRUE;
    } else {
      if ( gcmd_line.R && gcmd_line.debug >= 2 ) {
	printf(" -- too low!");
      }
      return FALSE;
    }
  }







  /* if we can (and this is wanted), just use the indimulti information
   * instead of WMC!!
   */
  if ( gcmd_line.simulation_wmc && gindimulti && gprob != -1 ) {
    if ( gcmd_line.R && gcmd_line.debug >= 1 ) {
      printf("\nr indi multi treatment applies, computed P %f", gprob);
    }
    gsim_wmc_calls++;
    /* july06: remember the actual prob!!
     */
    lthisgoalprob = gprob;
    if ( gprob >= ggoal_probability ) {
      if ( gcmd_line.R && gcmd_line.debug >= 1 ) {
	printf(" -- high enough!");
      }
      return TRUE;
    } else {
      if ( gcmd_line.R && gcmd_line.debug >= 1 ) {
	printf(" -- too low!");
      }
      return FALSE;
    }
  }
  






  /* none of the simple methods applies. do the real test!
   */
  if ( gcmd_line.heuristic == 1 ) {
    if ( disjunctions_supported_by_initial_formula( leafs, num_leafs, is_leaf, leafweights, 
						    aggregated_peff_weight, num_goalU ) ) {
      return TRUE;
    }
  }
  if ( gcmd_line.heuristic == 2 ) {
    if ( disjunctions_supported_by_S_formula( leafs, num_leafs, is_leaf, leafweights, 
					      aggregated_peff_weight, num_goalU ) ) {
      return TRUE;
    }
  }



  return FALSE;


}



void replace_support_graph_NOOPs(int *pathops, int num_path, 
				 int **rplanops, int *num_rplanops, int num_rplan,
				 UftNode_pointer *goalU, int num_goalU,
				 UEdgeNode **supportgraph, int *num_supportgraph,
				 UftNode_pointer **p )

{

  int gfti, i, j, k, ft, optime;


  for ( gfti = 0; gfti < num_goalU; gfti++ ) {
    for ( i = 0; i < num_supportgraph[gfti]; i++ ) {
      if ( supportgraph[gfti][i].num_E != 0 ) {
	/* not a NOOP
	 */
	continue;
      }
      if ( !supportgraph[gfti][i].active ) {
	/* might have been deactivated by pruning!
	 */
	continue;
      }

      ft = p[gfti][supportgraph[gfti][i].pnodefrom]->ft;
      if ( p[gfti][supportgraph[gfti][i].pnodeto]->ft != ft ) {
	printf("\nNOOP with different from, to facts?\n\n");
	exit( 1 );
      }

      optime = supportgraph[gfti][i].realtime - 1;/* realtime is the time at the end of the edge */

      if ( optime < 0 ) {
	/* check the pathop.
	 */
	if ( (-1)*optime > num_path ) {
	  /* can happen when the earliest actions have naught to do
	   * with the support graph.
	   */
	  continue;
// 	  printf("\nNOOP replace optime %d > num path %d?\n\n", (-1)*optime, num_path);
// 	  exit( 1 );
	}

	if ( has_prob_delete_on(ft, pathops[(-1)*optime]) ) {
	  get_prob_nondelete_on(ft, pathops[(-1)*optime],
				supportgraph[gfti][i].E, &(supportgraph[gfti][i].num_E));
	  
	  if ( gcmd_line.R && gcmd_line.debug >= 3 ) {
	    printf("\nreplacing NOOP ");
	    print_ft_name(ft);
	    printf(" %d --> %d with nondelete prob effs of op ", 
		   supportgraph[gfti][i].t - 1,
		   supportgraph[gfti][i].t);
	    print_op_name(pathops[(-1)*optime]);
	    printf(", efs: ");
	    for ( k = 0; k < supportgraph[gfti][i].num_E; k++ ) {
	      printf("%d ", supportgraph[gfti][i].E[k]);
	    }
	  }
	}

	continue;/* next support graph edge */
      }

      /* check the rplanops.
       */
      if ( optime > num_rplan ) {
	/* can happen when the latest actions have naught to do
	 * with the support graph.
	 */
	continue;
// 	printf("\nNOOP replace optime > num rplan?\n\n");
// 	exit( 1 );
      }
      /* just take the first guy, if there is one...
       * I guess this is not admissible, but who gives a f.. about this,
       * at this stage...
       */
      for ( j = 0; j < num_rplanops[optime]; j++ ) {
	if ( has_prob_delete_on(ft, rplanops[optime][j]) ) {
	  get_prob_nondelete_on(ft, rplanops[optime][j],
				supportgraph[gfti][i].E, &(supportgraph[gfti][i].num_E));

	  if ( gcmd_line.R && gcmd_line.debug >= 3 ) {
	    printf("\nreplacing NOOP ");
	    print_ft_name(ft);
	    printf(" %d --> %d with nondelete prob effs of op ", 
		   supportgraph[gfti][i].t - 1,
		   supportgraph[gfti][i].t);
	    print_op_name(rplanops[optime][j]);
	    printf(", efs: ");
	    for ( k = 0; k < supportgraph[gfti][i].num_E; k++ ) {
	      printf("%d ", supportgraph[gfti][i].E[k]);
	    }
	  }

	  break;/* to get to next support graph edge */
	}
      }

    } /* endfor i over support graph edges for gtfi */

  } /* gfti over goals */

}



Bool has_prob_delete_on( int ft, int op )

{

  int i, j, ef;

  for ( i = 0; i < gop_conn[op].num_E; i++ ) {
    ef = gop_conn[op].E[i];

    /* only prob effs...
     */
    if ( gef_conn[ef].eff_p >= 1 ) {
      continue;
    }

    /* is ft in non-common deletes?
     */
    for ( j = 0; j < gef_conn[ef].num_NSD; j++ ) {
      if ( gef_conn[ef].NSD[j] == ft ) {
	return TRUE;
      }
    }
  }

  return FALSE;

}



void get_prob_nondelete_on( int ft, int op, int *E, int *num_E )

{

  int i, j, k, ef, delef;

  for ( i = 0; i < gop_conn[op].num_E; i++ ) {
    ef = gop_conn[op].E[i];

    /* only prob effs...
     */
    if ( gef_conn[ef].eff_p >= 1 ) {
      continue;
    }

    /* is ft in non-common deletes?
     */
    for ( j = 0; j < gef_conn[ef].num_NSD; j++ ) {
      if ( gef_conn[ef].NSD[j] == ft ) {
	break;
      }
    }
    if ( j == gef_conn[ef].num_NSD ) {
      continue;
    }

    /* found one! now take this guy's non-ft-deleting colleagues!
     */
    delef = ef;
    (*num_E) = 0;
    for ( j = 0; j < gef_conn[delef].num_S; j++ ) {
      ef = gef_conn[ef].S[j];

      for ( k = 0; k < gef_conn[ef].num_NSD; k++ ) {
	if ( gef_conn[ef].NSD[k] == ft ) {
	  break;
	}
      }
      if ( k < gef_conn[ef].num_NSD ) {
	continue;
      }
      E[*num_E] = ef;
      (*num_E)++;
    }
    if ( *num_E == 0 ) {
      printf("\nno non-deleting colleagues in NOOP replace??\n\n");
      exit( 1 );
    }

    return;
  }

}





















































/***********************************************************
 * THINGS KEEP GETTING BETTER:                             * 
 *   HERE COMES A WEIGHTED MODEL COUNTER                   *
 ***********************************************************/
































/* this here returns the weight in terms of fraction a/b
 */
void r_wmc_CNF( double *a, double *b )

{

  int i, j, v, sum;

  for ( v = lr_num_c; v > 0; v-- ) {
    if ( lr_assigned[v] != -1 ) {
      printf("\nR:WMC entry assigned %d = %d??\n\n", v, lr_assigned[v]);
      exit( 1 );
    }
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
      printf("\nR:lr_num_clauses: %d, counted lr_num_c: %d, old: %d", lr_num_clauses, sum, lr_num_c);
    }
  }

  r_external_cachet_CNF( a, b );
  /* not strictly necessary, as full (..) initialisation done above 
   * before WMC calls... looks cleaner this way...
   */
  lr_num_decision_stack = 0;

}



void r_external_cachet_CNF( double *a, double *b )

{

  FILE *A;

  int satresult;
  double result, rtime;

  int i, j;

  if ( gcmd_line.debug && gcmd_line.R ) {
    printf("\nR: ForECachet: p %d %d (stack: %d)", lr_num_c, 
	   lr_num_clauses+lr_num_decision_stack, lr_num_decision_stack);

    for ( i = 1; i <= lr_num_c; i++ ) {
      printf("\nR: ForECachet: code %d = ", i);
      if ( lr_cf[i] == -1971 ) {
	printf("(chance node) w %lf", lr_cweight[i]);
      } else {
	print_ft_name(lr_cf[i]);
	printf(" w %lf", ginitial_ft_weight[lr_cf[i]]);
      }
    }

    /* decision stack is empty...
     */
/*     for ( i = 0; i < lr_num_decision_stack; i++ ) { */
/*       printf("\nR: ForECachet: stack %d = %d", i, lr_decision_stack[i]); */
/*     } */
    
    for ( i = 0; i < lr_num_clauses; i++ ) {
      printf("\nR: ForECachet: clause %d = ", i);
      for ( j = 0; j < lr_clause_length[i]; j++ ) {
	printf("%d ", lr_clauses[i][j].literal);
      }
    }

    printf("\nR: nForECachet: DONE!\n\n");
  }

  fflush(stdout);

  /* don't count time for file creation!
   */
  times( &end );
  TIME( gwmc_time );

  /* july06: count it, but separately.
   */
  times( &start );
  r_print_wmc_CNF();
  times( &end );
  TIME( gWMC_filetime );

  if ( gcmd_line.debug && gcmd_line.R ) {
    system( "~/CACHET/cachet CNF -c 100000" );
  } else {
    system( "~/CACHET/cachet CNF -q -c 100000" );
  }
  if ( (A = fopen("A","r")) == NULL ) {
    printf("\n\nR: can't open Catchet's answer file\n\n");
    exit( 1 );
  }
  fscanf(A,"%d %lf %lf\n", &satresult, &result, &rtime);
  fclose( A );
  system( "rm CNF" );
  system( "rm A" );

  times( &start );

  if ( gcmd_line.debug && gcmd_line.R ) {
    printf("\nR: Cachet: satresult ");
    switch ( satresult ) {
    case SATISFIABLE:
      printf("SAT");
      break;
    case UNSATISFIABLE:
      printf("UNSAT");
      break;
    case TIME_OUT:
      printf("TIMEOUT");
      break;
    case MEM_OUT:
      printf("MEMOUT");
      break;
    default:
      printf("\n\nunknown Cachet SAT outcome");
      exit( 1 );
    }
    printf(", result %lf rtime %lf", result, rtime);
  }

  fflush(stdout);

  switch ( satresult ) {
  case SATISFIABLE:
    break;
  case UNSATISFIABLE:
    /* is given back also in sat formulas..???!!
     */
    break;
  case TIME_OUT:
    printf("\n\nR: time out in Cachet\n\n");
    exit( 1 );
    break;
  case MEM_OUT:
    printf("\n\nR: memory out in Cachet\n\n");
    exit( 1 );
    break;
  default:
    printf("\n\nR: unknown Cachet SAT outcome");
    exit( 1 );
  }

  *a = result;
  *b = 1;

  gwmc_time += rtime;

}



void r_print_wmc_CNF( void )

{

  FILE *CNF;
  
  int i, j;
  
  if ( (CNF = fopen("CNF","w")) == NULL ) {
    printf("\n\ncan not open CNF file.\n\n");
    exit(1);
  }
  
  fprintf(CNF, "c encoded conformant search state transition base clauses\n");
  fprintf(CNF, "p cnf %d %d\n", lr_num_c, lr_num_clauses+lr_num_decision_stack);
  if ( gcmd_line.debug && gcmd_line.R ) {
    printf("\n\nR: Cachet: c encoded conformant search state transition base clauses\n");
    printf("R: Cachet: p cnf %d %d\n", lr_num_c, lr_num_clauses);
  }
  /* communicate the weights.
   */
  for ( i = 1; i <= lr_num_c; i++ ) {
    if ( lr_cf[i] == -1971 ) {
      fprintf(CNF, "w %d %lf\n", i, lr_cweight[i]);
      if ( gcmd_line.debug && gcmd_line.R ) {
	printf("R: Cachet: w %d %lf\n", i, lr_cweight[i]);
	fflush(stdout);
      }
    } else {
      /* may not be std, due to imp graph wewight prop
       */
      fprintf(CNF, "w %d %lf\n", i, ginitial_ft_weight[lr_cf[i]]);
      if ( gcmd_line.debug && gcmd_line.R ) {
	printf("R: Cachet: w %d %lf\n", i, ginitial_ft_weight[lr_cf[i]]);
      }
    }
  }

  for ( i = 0; i < lr_num_clauses; i++ ) {
    for ( j = 0; j < lr_clause_length[i]; j++ ) {
      fprintf(CNF, "%d", lr_clauses[i][j].literal);
      if ( j < lr_clause_length[i] - 1 ) {
	fprintf(CNF, " ");
      }
    }
    fprintf(CNF, " 0\n");
  }
  
  fclose( CNF );

}



