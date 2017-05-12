


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
 * File: repeated_states.c
 *
 * Description: recognises seen states. quite complicated.
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
















/***********************************************************
 * LOCAL GLOBALS                                           *
 ***********************************************************/








/* memory (hash table) for states that are already members
 * of the breadth - first search space in EHC
 */
EhcHashEntry_pointer lehc_hash_entry[EHC_HASH_SIZE];
int lnum_ehc_hash_entry[EHC_HASH_SIZE];
int lchanged_ehc_entrys[EHC_HASH_SIZE];
int lnum_changed_ehc_entrys;
Bool lchanged_ehc_entry[EHC_HASH_SIZE];








/* memory (hash table) for states that are already members
 * of the best first search space
 */
BfsHashEntry_pointer lbfs_hash_entry[BFS_HASH_SIZE];





/* the clauses to be communicated to the SAT solver for
 * determining fact progress. indexed by nr (S1 | S2) / cl nr / lit nr
 */
TimedLiteral ***lrs_clauses;
int **lrs_clause_length;
int *lrs_num_clauses;
int *lrs_endtime;


/* array; maps 1|2 (ie 0|1) / ft / time triple to its number in CNF encoding.
 */
int ***lrs_codes;

/* inverse mapping, to undo changes in table.
 */
int *lrs_cn, *lrs_cf, *lrs_ct, lrs_num_c;



/* hitting set -- in CNF construction for noops in presence of non-unary antecedents.
 */
int *lrs_hitting_set;
int lrs_num_hitting_set;



/* for naive DP implementation
 */
int **lrs_dp_clauses;
int *lrs_dp_clause_length;
int lrs_dp_num_clauses;
int *lrs_assigned;



/* stores the current DP decisions including unit propagations.
 */
int *lrs_decision_stack;
int lrs_num_decision_stack;


/* for each possible ft code, a pointer to connected dynamic list
 * of member elements, ie the clauses in which it participates,
 * positive and negative.
 */
MemberList_pointer *lrs_pos_c_in_clause_start;
MemberList_pointer *lrs_pos_c_in_clause_end;
MemberList_pointer *lrs_neg_c_in_clause_start;
MemberList_pointer *lrs_neg_c_in_clause_end;
















void initialize_repeated_states( void )

{

  int i, j;

  for ( i = 0; i < EHC_HASH_SIZE; i++ ) {
    lehc_hash_entry[i] = NULL;
    lnum_ehc_hash_entry[i] = 0;
    lchanged_ehc_entry[i] = FALSE;
  }
  lnum_changed_ehc_entrys = 0;
  
  for ( i = 0; i < BFS_HASH_SIZE; i++ ) {
    lbfs_hash_entry[i] = NULL;
  }

  lrs_clauses = ( TimedLiteral *** ) calloc( 2, sizeof( TimedLiteral ** ) );
  lrs_clauses[0] = ( TimedLiteral ** ) calloc( gmax_rs_clauses, sizeof( TimedLiteral * ) );
  lrs_clauses[1] = ( TimedLiteral ** ) calloc( gmax_rs_clauses, sizeof( TimedLiteral * ) );
  lrs_clause_length = ( int ** ) calloc( 2, sizeof( int * ) );
  lrs_clause_length[0] = ( int * ) calloc( gmax_rs_clauses, sizeof( int ) );
  lrs_clause_length[1] = ( int * ) calloc( gmax_rs_clauses, sizeof( int ) );
  lrs_dp_clauses = ( int ** ) calloc( gmax_rs_clauses, sizeof( int * ) );
  lrs_dp_clause_length = ( int * ) calloc( gmax_rs_clauses, sizeof( int ) );
  for ( i = 0; i < gmax_rs_clauses; i++ ) {
    lrs_clauses[0][i] = ( TimedLiteral * ) calloc( gmax_literals, sizeof( TimedLiteral ) );
    lrs_clause_length[0][i] = 0;
    lrs_clauses[1][i] = ( TimedLiteral * ) calloc( gmax_literals, sizeof( TimedLiteral ) );
    lrs_clause_length[1][i] = 0;
    lrs_dp_clauses[i] = ( int * ) calloc( gmax_literals, sizeof( int ) );
    lrs_dp_clause_length[i] = 0;
  }
  lrs_num_clauses = ( int * ) calloc( 2, sizeof( int ) );
  lrs_num_clauses[0] = 0;
  lrs_num_clauses[1] = 0;
  lrs_dp_num_clauses = 0;
  
  lrs_endtime = ( int * ) calloc( 2, sizeof( int ) );
  lrs_endtime[0] = -1;
  lrs_endtime[1] = -1;
  
  lrs_codes = ( int *** ) calloc( 2, sizeof( int ** ) );
  lrs_codes[0] = ( int ** ) calloc( MAX_PLAN_LENGTH + 1, sizeof( int * ) );
  lrs_codes[1] = ( int ** ) calloc( MAX_PLAN_LENGTH + 1, sizeof( int * ) );
  for ( i = 0; i <= MAX_PLAN_LENGTH; i++ ) {
    lrs_codes[0][i] = ( int * ) calloc( gnum_ft_conn, sizeof( int ) );
    lrs_codes[1][i] = ( int * ) calloc( gnum_ft_conn, sizeof( int ) );
    for ( j = 0; j < gnum_ft_conn; j++ ) {
      lrs_codes[0][i][j] = -1;
      lrs_codes[1][i][j] = -1;
    }
  }
  
  lrs_cn = ( int * ) calloc( (1 + MAX_PLAN_LENGTH) * gmax_CNFU * 2 + 1, sizeof( int ) );
  lrs_cf = ( int * ) calloc( (1 + MAX_PLAN_LENGTH) * gmax_CNFU * 2 + 1, sizeof( int ) );
  lrs_ct = ( int * ) calloc( (1 + MAX_PLAN_LENGTH) * gmax_CNFU * 2 + 1, sizeof( int ) );
  
  lrs_hitting_set = ( int * ) calloc( gnum_ft_conn, sizeof( int ) );


  lrs_assigned = ( int * ) calloc( (1 + MAX_PLAN_LENGTH) * gmax_CNFU * 2 + 1, sizeof( int ) );
  for ( i = 0; i < (1 + MAX_PLAN_LENGTH) * gmax_CNFU * 2 + 1; i++ ) {
    lrs_assigned[i] = -1;
  }

  lrs_decision_stack = ( int * ) calloc( (1 + MAX_PLAN_LENGTH) * gmax_CNFU * 2, sizeof( int ) );

  lrs_pos_c_in_clause_start = ( MemberList_pointer * ) 
    calloc( (1 + MAX_PLAN_LENGTH) * gmax_CNFU * 2 + 1, sizeof( MemberList_pointer ) );
  lrs_pos_c_in_clause_end = ( MemberList_pointer * ) 
    calloc( (1 + MAX_PLAN_LENGTH) * gmax_CNFU * 2 + 1, sizeof( MemberList_pointer ) );
  lrs_neg_c_in_clause_start = ( MemberList_pointer * ) 
    calloc( (1 + MAX_PLAN_LENGTH) * gmax_CNFU * 2 + 1, sizeof( MemberList_pointer ) );
  lrs_neg_c_in_clause_end = ( MemberList_pointer * ) 
    calloc( (1 + MAX_PLAN_LENGTH) * gmax_CNFU * 2 + 1, sizeof( MemberList_pointer ) );
  for ( i = 0; i <= (1 + MAX_PLAN_LENGTH) * gmax_CNFU * 2; i++ ) {
    lrs_pos_c_in_clause_start[i] = NULL;
    lrs_neg_c_in_clause_start[i] = NULL;
    lrs_pos_c_in_clause_end[i] = NULL;
    lrs_neg_c_in_clause_end[i] = NULL;
  }

}




















/*********************************************************
 * HASHING ALGORITHM FOR RECOGNIZING DOMINATED STATES IN *
 * EHC BREADTH FIRST SEARCH                              *
 *********************************************************/





























void hash_ehc_node( EhcNode *n )

{

  int i, Fsum, Usum, index;
  EhcHashEntry *h, *prev = NULL;

  Fsum = state_Fsum( &(n->S) );
  Usum = state_Usum( &(n->S) );
  index = (Fsum + Usum) & EHC_HASH_BITS;

  h = lehc_hash_entry[index];
  if ( !h ) {
    h = new_EhcHashEntry();
    h->Fsum = Fsum;
    h->Usum = Usum;
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
    h->Fsum = Fsum;
    h->Usum = Usum;
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
  h->Fsum = Fsum;
  h->Usum = Usum;
  h->ehc_node = n;
  prev->next = h;
  lnum_ehc_hash_entry[index]++;
  if ( !lchanged_ehc_entry[index] ) {
    lchanged_ehc_entrys[lnum_changed_ehc_entrys++] = index;
    lchanged_ehc_entry[index] = TRUE;
  }
  return;
      
}



Bool ehc_state_hashed( State *S, EhcNode *father, int op )

{

  int i, Usum, Fsum, index;
  EhcHashEntry *h;

  times( &end );
  TIME( gsearch_time );
  times( &start );    

  Fsum = state_Fsum( S );
  Usum = state_Usum( S );
  index = (Fsum + Usum) & EHC_HASH_BITS;

  h = lehc_hash_entry[index];
  for ( i = 0; i < lnum_ehc_hash_entry[index]; i++ ) {
    if ( h->Fsum < Fsum ) {
      h = h->next;
      continue;
    }
    if ( h->Fsum + h->Usum < Fsum + Usum ) {
      h = h->next;
      continue;
    }
    if ( dominates_state( &(h->ehc_node->S), S, 
			  h->ehc_node, father,
			  NULL, NULL,
			  op ) ) {
      times( &end );
      TIME( grs_time );
      times( &start );
      
      if ( gcmd_line.P && gcmd_line.debug ) {
	printf("\n----------------------ehc domination hit!!!");
      }
      
      return TRUE;
    }
    h = h->next;
  }
  
  times( &end );
  TIME( grs_time );
  times( &start );

  return FALSE;

}



/* does S1 dominate S2?, resp. is S1 the same as S2?
 */
Bool dominates_state( State *S1, State *S2,
		      EhcNode *S1ehcnode, EhcNode *S2ehcfather,
		      BfsNode *S1bfsnode, BfsNode *S2bfsfather,
		      int op ) 

{

  int i, j;

  grs_comps++;

  if ( gcmd_line.P && gcmd_line.debug ) {
    printf("\n\n---------------------------------------------hash table hit S1");
    print_state( *S1 );
    printf("\n---------------------------------------------S2");
    print_state( *S2 );
  }

  if ( gdomination_valid ) {
    /* S2.F \subseteq S1.F?
     */
    if ( S2->num_F > S1->num_F ) {
      if ( gcmd_line.P && gcmd_line.debug ) {
	printf("\n-----------|S2.F| > |S1.F|");
      }
      return FALSE;
    }
    for ( i = 0; i < S2->num_F; i++ ) {
      for ( j = 0; j < S1->num_F; j++ ) {
	if ( S1->F[j] == S2->F[i] ) {
	  break;
	}
      }
      if ( j == S1->num_F ) {
	if ( gcmd_line.P && gcmd_line.debug ) {
	  printf("\n-----------S2.F !sub S1.F: ");
	  print_ft_name( S2->F[i] );
	}
	return FALSE;
      }
    }
    
    /* S2.U \subseteq S1.F \cup S1.U?
     */
    if ( S2->num_U > S1->num_F + S1->num_U ) {
      if ( gcmd_line.P && gcmd_line.debug ) {
	printf("\n-----------|S2.U| > |S1.F|+|S1.U|");
      }
      return FALSE;
    }
    for ( i = 0; i < S2->num_U; i++ ) {
      for ( j = 0; j < S1->num_F; j++ ) {
	if ( S1->F[j] == S2->U[i] ) {
	  break;
	}
      }
      if ( j == S1->num_F ) {
	for ( j = 0; j < S1->num_U; j++ ) {
	  if ( S1->U[j] == S2->U[i] ) {
	    break;
	  }
	}
	if ( j == S1->num_U ) {
	  if ( gcmd_line.P && gcmd_line.debug ) {
	    printf("\n-----------S2.U !sub S1.F cup S1.U: ");
	    print_ft_name( S2->U[i] );
	  }
	  return FALSE;
	}
      }
    }
    
    /* S2.U \subseteq S1.F? --> yes, don't need to check!
     */
    for ( i = 0; i < S2->num_U; i++ ) {
      for ( j = 0; j < S1->num_F; j++ ) {
	if ( S1->F[j] == S2->U[i] ) {
	  break;
	}
      }
      if ( j == S1->num_F ) {
	break;
      }
    }
    if ( i == S2->num_U ) {
      return TRUE;
    }
  } else {
    /* S2.F == S1.F?
     */
    if ( S2->num_F != S1->num_F ) {
      if ( gcmd_line.P && gcmd_line.debug ) {
	printf("\n-----------|S2.F| != |S1.F|");
      }
      return FALSE;
    }
    for ( i = 0; i < S2->num_F; i++ ) {
      for ( j = 0; j < S1->num_F; j++ ) {
	if ( S1->F[j] == S2->F[i] ) {
	  break;
	}
      }
      if ( j == S1->num_F ) {
	if ( gcmd_line.P && gcmd_line.debug ) {
	  printf("\n-----------S2.F != S1.F: ");
	  print_ft_name( S2->F[i] );
	}
	return FALSE;
      }
    }
  
    /* S2.U == S1.U?
     */
    if ( S2->num_U != S1->num_U ) {
      if ( gcmd_line.P && gcmd_line.debug ) {
	printf("\n-----------|S2.U| != |S1.U|");
      }
      return FALSE;
    }
    for ( i = 0; i < S2->num_U; i++ ) {
      for ( j = 0; j < S1->num_U; j++ ) {
	if ( S1->U[j] == S2->U[i] ) {
	  break;
	}
      }
      if ( j == S1->num_U ) {
	if ( gcmd_line.P && gcmd_line.debug ) {
	  printf("\n-----------S2.U != S1.U: ");
	  print_ft_name( S2->U[i] );
	}
	return FALSE;
      }
    }
  }

  grs_conf_comps++;
  return dominates_conformant( S1, S2, S1ehcnode, S2ehcfather, S1bfsnode, S2bfsfather, op );

}



int state_Fsum( State *S )

{

  int i, sum = 0;

  for ( i = 0; i < S->num_F; i++ ) {
    sum += gft_conn[S->F[i]].rand;
  }

  return sum;

}



int state_Usum( State *S )

{

  int i, sum = 0;

  for ( i = 0; i < S->num_U; i++ ) {
    sum += gft_conn[S->U[i]].rand;
  }

  return sum;

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



Bool dominates_conformant( State *S1, State *S2,
			   EhcNode *S1ehcnode, EhcNode *S2ehcfather,
			   BfsNode *S1bfsnode, BfsNode *S2bfsfather,
			   int op ) 

{

  int i, j, ft, m, ff;
  int ft_S1_code, ft_S2_code;
  Bool sat;
  Bool CNF_built = FALSE;
  
  if ( gcmd_line.P && gcmd_line.debug ) {
    printf("\n----------------------------------checking conformant domination");
  }
  
  /* for all facts in S2.U, check whether they are better off/true in the same cases as in S1
   */
  for ( i = 0; i < S2->num_U; i++ ) {
    ft = S2->U[i];
    if ( gcmd_line.P && gcmd_line.debug ) {
      printf("\n-------posibly progressed/different ft %d", ft);
      print_ft_name( ft );
    }
    
    if ( !gft_conn[ft].srelevant ) {
      if ( gcmd_line.P && gcmd_line.debug ) {
	printf(" is not solution relevant!");
      }
      continue;
    }	    

    if ( gdomination_valid ) {
      for ( j = 0; j < S1->num_U; j++ ) {
	if ( ft == S1->U[j] ) break;
      }
      if ( j == S1->num_U ) {
	/* with pre-check, we got ft in S1.F and thus no need to check it.
	 */
	if ( gcmd_line.P && gcmd_line.debug ) {
	  printf(" is true in S1.F!");
	}
	continue;
      }
    }

    if ( !CNF_built ) {
      if ( gcmd_line.P && gcmd_line.debug ) {
	printf("\n-----------------building domination CNF");fflush(stdout);
      }
      build_domination_CNF( S1, S2, S1ehcnode, S2ehcfather, S1bfsnode, S2bfsfather, op );
      CNF_built = TRUE;
    }

    ff = ft;
    if ( gft_conn[ff].CNF ) {
      m = 1;
    } else {
      m = -1;
      ff = gft_conn[ff].negation;
    }
    ft_S1_code = lrs_codes[0][lrs_endtime[0]][ff];
    ft_S2_code = lrs_codes[1][lrs_endtime[1]][ff];
    if ( gcmd_line.P && gcmd_line.debug >= 3 ) {
      printf("\nft has code %d in S1 at time %d", ft_S1_code, lrs_endtime[0]);
      printf("\nft has code %d in S2 at time %d", ft_S2_code, lrs_endtime[1]);fflush(stdout);
    }
    if ( ft_S2_code == -1 ) {
      if ( lrs_endtime[1] == 0 ) {
	printf("\nS2 endtime is 0??\n\n");
	exit( 1 );
      }
      printf("\n(S2) U ft not encoded at time %d??\n\n", lrs_endtime[1]);
      exit( 1 );
    }
    if ( ft_S1_code == -1 ) {
      if ( lrs_endtime[0] > 0 ) {
	printf("\n(S1) U ft not encoded at time %d??\n\n", lrs_endtime[0]);
	exit( 1 );
      }
      if ( gcmd_line.debug >= 2 ) {
	printf("\nwarning: ft ");
	print_ft_name( ft );
	printf(" not encoded at S1 time 0. assuming independency so no domination.");
      }
      /* this can (?) only happen if ft is neither encoded by S2 at time 0,
       * meaning (?) it does not participate even in its noop...
       *
       * inserting a clause wouldn't make sense, ie yield results "sat" anyway
       * so this here seems to be the right answer.
       */ 
      return FALSE;    
    }
    
    /* ok, here we go. add the new clauses and go for it.
     */
    lrs_num_decision_stack = 0;
    lrs_decision_stack[lrs_num_decision_stack++] = m * (-1) * ft_S1_code;
    lrs_decision_stack[lrs_num_decision_stack++] = m * ft_S2_code;
    if ( gcmd_line.P && gcmd_line.debug >= 4 ) {
      printf("\n----------clauses :\n");
      print_rs_encoded_clauses();
    }
    
    times( &end );
    TIME( grs_time );
    times( &start );
    grs_sat_calls++;
    sat = rs_dp_CNF();
    times( &end );
    TIME( grs_sat_time );
    times( &start );
    if ( sat ) {
      if ( gcmd_line.P && gcmd_line.debug ) {
	printf("\nsat 1! no domination!");
      }
      return FALSE;
    }
    if ( !gdomination_valid ) {
      /* second test to ensure equality
       */
      lrs_num_decision_stack = 0;
      lrs_decision_stack[lrs_num_decision_stack++] = m * ft_S1_code;
      lrs_decision_stack[lrs_num_decision_stack++] = m * (-1) * ft_S2_code;
      if ( gcmd_line.P && gcmd_line.debug >= 4 ) {
	printf("\n----------clauses :\n");
	print_rs_encoded_clauses();
      }
      times( &end );
      TIME( grs_time );
      times( &start );
      grs_sat_calls++;
      sat = rs_dp_CNF();
      times( &end );
      TIME( grs_sat_time );
      times( &start );
      if ( sat ) {
	if ( gcmd_line.P && gcmd_line.debug ) {
	  printf("\nsat 2! no domination!");
	}
	return FALSE;
      }
    }
    if ( gcmd_line.P && gcmd_line.debug ) {
      printf("\nunsat! no improvement on this ft!");
    }
  } /* S2-> U */
  
  grs_hits++;
  return TRUE;

}



void build_domination_CNF( State *S1, State *S2,
			   EhcNode *S1ehcnode, EhcNode *S2ehcfather,
			   BfsNode *S1bfsnode, BfsNode *S2bfsfather,
			   int op )

{

  build_one_part_of_domCNF( 0, S1, 
			    S1ehcnode ? S1ehcnode->father : NULL, 
			    S1bfsnode ? S1bfsnode->father : NULL, 
			    S1ehcnode ? S1ehcnode->op : S1bfsnode->op );
  build_one_part_of_domCNF( 1, S2, S2ehcfather, S2bfsfather, op );
  
  encode_domCNF();
  
}



void build_one_part_of_domCNF( int nr, State *S, EhcNode *ehcdad, BfsNode *bfsdad, int op )

{

  static Bool fc = TRUE;
  static State_pointer *path;
  static int *path_op;
  static Bool *Ft, *Ut, *Ftpp, *Utpp;
  
  int i, j, num_path, n, t, time, ef, k, ft, num_path_op, m, ff;
  EhcNode *iehc;
  BfsNode *ibfs;
  
  if ( fc ) {
    path = ( State_pointer * ) calloc( MAX_PLAN_LENGTH + 1, sizeof( State_pointer ) );
    path_op = ( int * ) calloc( MAX_PLAN_LENGTH + 1, sizeof( int ) );
    
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
  
  /* first, erase old CNF
   */
  for ( i = 0; i < lrs_num_clauses[nr]; i++ ) {
    lrs_clause_length[nr][i] = 0;
  }
  lrs_num_clauses[nr] = 0;

  /* now, collect the path to S
   */
  if ( gcmd_line.P && gcmd_line.debug >= 2 ) {
    printf("\n\n-------------------------------------rs path for nr %d", nr);fflush(stdout);
  }
  num_path = MAX_PLAN_LENGTH;
  path[num_path--] = S;
  if ( gcmd_line.P && gcmd_line.debug >= 3 ) {
    printf("\n-----state %d", num_path+1); print_state( *path[num_path+1] );fflush(stdout);
  }
  /* DEBUGGING
   */
  num_path_op = MAX_PLAN_LENGTH;
  if ( gcmd_line.P && gcmd_line.debug >= 2 ) {
    if ( op >= 0 ) {
      path_op[num_path_op--] = op;
      printf("\n-----final op (%d) %d", op, num_path_op+1); fflush(stdout);
      print_op_name( path_op[num_path_op+1] );fflush(stdout);
    }
  }
  if ( ehcdad ) {
    for ( iehc = ehcdad; iehc->father; iehc = iehc->father ) {
      if ( num_path == 0 ) {
	printf("\n\nincrease MAX_PLAN_LENGTH! currently %d\n\n",
	       MAX_PLAN_LENGTH);
	exit( 1 );
      }
      path[num_path--] = &(iehc->S);
      if ( gcmd_line.P && gcmd_line.debug >= 3 ) {
	printf("\n-----state %d", num_path+1); print_state( *path[num_path+1] );fflush(stdout);
      }
      if ( gcmd_line.P && gcmd_line.debug >= 2 ) {
	path_op[num_path_op--] = iehc->op;
	printf("\n-----op %d", num_path_op+1); print_op_name( path_op[num_path_op+1] );fflush(stdout);
      }
    }
    for ( i = gnum_plan_ops; i >= 0; i-- ) {
      if ( num_path == 0 ) {
	printf("\n\nincrease MAX_PLAN_LENGTH! currently %d\n\n",
	       MAX_PLAN_LENGTH);
	exit( 1 );
      }
      path[num_path--] = &(gplan_states[i]);
      if ( gcmd_line.P && gcmd_line.debug >= 3 ) {
	printf("\n-----state %d", num_path+1); print_state( *path[num_path+1] );fflush(stdout);
      }
      if ( gcmd_line.P && gcmd_line.debug >= 2 && i >= 1 ) {
	path_op[num_path_op--] = gplan_ops[i-1];
	printf("\n-----op %d", num_path_op+1); print_op_name( path_op[num_path_op+1] );fflush(stdout);
      }
    }
  } else {
    for ( ibfs = bfsdad; ibfs; ibfs = ibfs->father ) {
      if ( num_path == 0 ) {
	printf("\n\nincrease MAX_PLAN_LENGTH! currently %d\n\n",
	       MAX_PLAN_LENGTH);
	exit( 1 );
      }
      path[num_path--] = &(ibfs->S);
      if ( gcmd_line.P && gcmd_line.debug >= 3 ) {
	printf("\n-----state %d", num_path+1); print_state( *path[num_path+1] );fflush(stdout);
      }
      if ( gcmd_line.P && gcmd_line.debug >= 2 && ibfs->father ) {
	path_op[num_path_op--] = ibfs->op;
	printf("\n-----op %d", num_path_op+1); print_op_name( path_op[num_path_op+1] );fflush(stdout);
      }
    }
  }
  
  
  if ( gcmd_line.P && gcmd_line.debug >= 2 ) {
    printf("\n\n-------------------------------------adding clauses");fflush(stdout);
  }
  /* now compute the CNF; start with the initial clauses.
   *
   * we need this only in one of our two CNFs. let's say the 1st one.
   */
  if ( nr == 0 ) {
    for ( i = 0; i < gnum_initial_or; i++ ) {
      if ( lrs_num_clauses[nr] == gmax_rs_clauses ) {
	printf("\n\ntoo many rs clauses? %d\n\n", lrs_dp_num_clauses);
	exit( 1 );
      }
      for ( j = 0; j < ginitial_or_length[i]; j++ ) {
	ff = ginitial_or[i][j];
	if ( gft_conn[ff].CNF ) {
	  m = 1;
	} else {
	  m = -1;
	  ff = gft_conn[ff].negation;
	}
	lrs_clauses[nr][lrs_num_clauses[nr]][lrs_clause_length[nr][lrs_num_clauses[nr]]].literal = 
	  m * (1 + ff);
	lrs_clauses[nr][lrs_num_clauses[nr]][lrs_clause_length[nr][lrs_num_clauses[nr]]].time = 0;
	lrs_clause_length[nr][lrs_num_clauses[nr]]++;
      }
      lrs_num_clauses[nr]++;
      if ( gcmd_line.P && gcmd_line.debug  >= 3 ) {
	printf("\ninitial OR clause"); print_rs_clause( nr, lrs_num_clauses[nr]-1 ); 
	fflush(stdout);
      }
    }
  }
  
  /* now, the path implications (up to the end state, need no distinction
   * with last step here)
   */
  for ( t = num_path + 1; t < MAX_PLAN_LENGTH; t++ ) {
    time = t - (num_path + 1);
    
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
    
    for ( i = 0; i < path[t+1]->num_unknown_E; i++ ) {
      ef = path[t+1]->unknown_E[i];
      for ( j = 0; j < gef_conn[ef].num_A; j++ ) {
	if ( !gft_conn[gef_conn[ef].A[j]].CNF ) continue;
	if ( Ftpp[gef_conn[ef].A[j]] ) continue;
	if ( lrs_num_clauses[nr] == gmax_rs_clauses ) {
	  printf("\n\ntoo many rs clauses? %d\n\n", lrs_dp_num_clauses);
	  exit( 1 );
	}
	n = lrs_num_clauses[nr];
	for ( k = 0; k < gef_conn[ef].num_C; k++ ) {
	  if ( Ft[gef_conn[ef].C[k]] ) continue;
	  /* DEBUGGING; REMOVE LATER
	   */
	  if ( !Ut[gef_conn[ef].C[k]] ) {
	    printf("\nunknown ef cond neither F nor U in domCNF gen\n\n");
	    exit(1);
	  }
	  ff = gef_conn[ef].C[k];
	  if ( gft_conn[ff].CNF ) {
	    m = 1;
	  } else {
	    m = -1;
	    ff = gft_conn[ff].negation;
	  }
	  lrs_clauses[nr][n][lrs_clause_length[nr][n]].literal =
	    m * (-1) * (1 + ff);
	  lrs_clauses[nr][n][lrs_clause_length[nr][n]++].time =
	    time;
	}
	lrs_clauses[nr][n][lrs_clause_length[nr][n]].literal =
	  1 + gef_conn[ef].A[j];
	lrs_clauses[nr][n][lrs_clause_length[nr][n]++].time =
	  time + 1;
	lrs_num_clauses[nr]++;
	if ( gcmd_line.P && gcmd_line.debug >= 3 ) {
	  printf("\nunknown add eff clause"); print_rs_clause( nr, lrs_num_clauses[nr]-1 );
	}
      }
      for ( j = 0; j < gef_conn[ef].num_D; j++ ) {
	if ( !gft_conn[gef_conn[ef].D[j]].CNF ) continue;
	if ( !Ftpp[gef_conn[ef].D[j]] && !Utpp[gef_conn[ef].D[j]] ) continue;
	n = lrs_num_clauses[nr];
	if ( lrs_num_clauses[nr] == gmax_rs_clauses ) {
	  printf("\n\ntoo many rs clauses? %d\n\n", lrs_dp_num_clauses);
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
	  lrs_clauses[nr][n][lrs_clause_length[nr][n]].literal =
	    m * (-1) * (1 + ff);
	  lrs_clauses[nr][n][lrs_clause_length[nr][n]++].time =
	    time;
	}
	lrs_clauses[nr][n][lrs_clause_length[nr][n]].literal =
	  (-1) * (1 + gef_conn[ef].D[j]);
	lrs_clauses[nr][n][lrs_clause_length[nr][n]++].time =
	  time + 1;
	lrs_num_clauses[nr]++;
	if ( gcmd_line.P && gcmd_line.debug >= 3 ) {
	  printf("\nunknown del eff clause"); print_rs_clause( nr, lrs_num_clauses[nr]-1 );
	}
      }
    }/* end U effs */
    
    /* now, the noop implications
     */
    for ( ft = 0; ft < gnum_ft_conn; ft++ ) {
      if ( !gft_conn[ft].CNF ) continue;
      if ( Ftpp[ft] ) continue;/* clause satisfied */
      if ( !Utpp[ft] ) {
	continue;
      }
      if ( !Ft[ft] && !Ut[ft] ) continue;/* clause satisfied */
      insert_rs_posnoop_hitting_set_clauses( nr, ft, time, 
					     path[t+1],
					     Ft, Ut );
    }
    for ( ft = 0; ft < gnum_ft_conn; ft++ ) {
      if ( !gft_conn[ft].CNF ) continue;
      if ( !Ftpp[ft] && !Utpp[ft] ) continue;/* clause satisfied */
      if ( Ftpp[ft] ) {
	continue;
      }
      if ( Ft[ft] ) continue;/* clause satisfied */
      insert_rs_negnoop_hitting_set_clauses( nr, ft, time, 
					     path[t+1],
					     Ft, Ut );
    }
    
    
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
  }/* end path implications */
  lrs_endtime[nr] = t - (num_path + 1);
  
  if ( gcmd_line.P && gcmd_line.debug >= 2 ) {
    printf("\n\n--------------------------finished clauses up to endtime %d",
	   lrs_endtime[nr]);
  }

}



void encode_domCNF( void )

{

  int i, j, ft, time;
  Bool neg;
  
  /* first, erase old codes and CNF
   */
  for ( i = 1; i <= lrs_num_c; i++ ) {
    lrs_codes[lrs_cn[i]][lrs_ct[i]][lrs_cf[i]] = -1;
    if ( lrs_ct[i] == 0 ) {
      /* at time 0, also reset the other one.
       */
      lrs_codes[1-lrs_cn[i]][lrs_ct[i]][lrs_cf[i]] = -1;
    }
  }
  lrs_num_c = 0;
  for ( i = 0; i < lrs_dp_num_clauses; i++ ) {
    lrs_dp_clause_length[i] = 0;
  }
  lrs_dp_num_clauses = 0;
  
  /* then insert the nr == 0 CNF
   */
  for ( i = 0; i < lrs_num_clauses[0]; i++ ) {
    for ( j = 0; j < lrs_clause_length[0][i]; j++ ) {
      time = lrs_clauses[0][i][j].time;
      if ( lrs_clauses[0][i][j].literal < 0 ) {
	neg = TRUE;
	ft = ((-1) * lrs_clauses[0][i][j].literal) - 1;
      } else {
	neg = FALSE;
	ft = lrs_clauses[0][i][j].literal - 1;
      }
      if ( lrs_codes[0][time][ft] == -1 ) {
	lrs_num_c++;/* must be non-zero */
	lrs_codes[0][time][ft] = lrs_num_c;
	if ( lrs_num_c == (1 + MAX_PLAN_LENGTH) * gmax_CNFU * 2 + 1 ) {
	  printf("\n\ntoo many rs codes? %d\n\n", lrs_num_c);
	  exit( 1 );
	}
	lrs_cn[lrs_num_c] = 0;
	lrs_cf[lrs_num_c] = ft;
	lrs_ct[lrs_num_c] = time; 
	if ( time == 0 ) {
	  /* synchronization: initial lits have same codes!
	   *
	   * NOTE: ACTUALLY WE COULD DO THIS WITH ALL
	   * TIME STEPS UP TO THE POINT WHERE THE TWO
	   * PATHS GO APART!!
	   */
	  lrs_codes[1][time][ft] = lrs_num_c;
	}   
      }
      lrs_dp_clauses[i][j] = lrs_codes[0][time][ft];
      if ( neg ) {
	lrs_dp_clauses[i][j] *= (-1);
      }
    }
    lrs_dp_clause_length[i] = lrs_clause_length[0][i];
  }/* end [0] clauses */
  lrs_dp_num_clauses = lrs_num_clauses[0];
  
  /* and now, the second one.
   */
  for ( i = 0; i < lrs_num_clauses[1]; i++ ) {
    for ( j = 0; j < lrs_clause_length[1][i]; j++ ) {
      time = lrs_clauses[1][i][j].time;
      if ( lrs_clauses[1][i][j].literal < 0 ) {
	neg = TRUE;
	ft = ((-1) * lrs_clauses[1][i][j].literal) - 1;
      } else {
	neg = FALSE;
	ft = lrs_clauses[1][i][j].literal - 1;
      }
      if ( lrs_codes[1][time][ft] == -1 ) {
	lrs_num_c++;
	lrs_codes[1][time][ft] = lrs_num_c;
	if ( lrs_num_c == (1 + MAX_PLAN_LENGTH) * gmax_CNFU * 2 + 1 ) {
	  printf("\n\ntoo many rs codes? %d\n\n", lrs_num_c);
	  exit( 1 );
	}
	lrs_cn[lrs_num_c] = 1;
	lrs_cf[lrs_num_c] = ft;
	lrs_ct[lrs_num_c] = time; 
	if ( time == 0 ) {
	  /* synchronization: initial lits have same codes!
	   *
	   * NOTE: ACTUALLY WE COULD DO THIS WITH ALL
	   * TIME STEPS UP TO THE POINT WHERE THE TWO
	   * PATHS GO APART!!
	   */
	  lrs_codes[0][time][ft] = lrs_num_c;
	}   
      }
      lrs_dp_clauses[lrs_dp_num_clauses][j] = lrs_codes[1][time][ft];
      if ( neg ) {
	lrs_dp_clauses[lrs_dp_num_clauses][j] *= (-1);
      }
    }
    lrs_dp_clause_length[lrs_dp_num_clauses] = lrs_clause_length[1][i];
    lrs_dp_num_clauses++;
  }
  
  if ( gcmd_line.P && gcmd_line.debug  >= 4 ) {
    printf("\n\n--------------------------------joint CNF encoding");
    printf("\n-----codes %d up to:", lrs_num_c);
    for ( i = 1; i <= lrs_num_c; i++ ) {
      printf("\n%d --- [%d] ", i, lrs_cn[i]); print_ft_name( lrs_cf[i] ); 
      printf("(%d) ---> %d", 
	     lrs_ct[i], lrs_codes[lrs_cn[i]][lrs_ct[i]][lrs_cf[i]]);
      if ( lrs_ct[i] == 0 ) {
	printf("\n%d --- [%d] ", i, 1-lrs_cn[i]); print_ft_name( lrs_cf[i] ); 
	printf("(%d) ---> %d", 
	       lrs_ct[i], lrs_codes[1-lrs_cn[i]][lrs_ct[i]][lrs_cf[i]]);
      }
    }
    printf("\n-----clauses:\n");
    print_rs_encoded_clauses();
  }

  /* create lit-clause lists
   */
  rs_update_membership_lists();

}



void print_rs_clause( int nr, int i )

{

  int j;
  
  printf("\n[%d] --- ", nr);
  for ( j = 0; j < lrs_clause_length[nr][i]; j++ ) {
    if ( lrs_clauses[nr][i][j].time >= 0 ) {
      if ( lrs_clauses[nr][i][j].literal < 0 ) {
	printf("-");
	print_ft_name( (-1) * lrs_clauses[nr][i][j].literal - 1 );
      } else {
	print_ft_name( lrs_clauses[nr][i][j].literal - 1 );
      }
      printf("(%d) ", lrs_clauses[nr][i][j].time);
    } else {
      printf("\ntime in dom CNF < 0 ??\n\n");
      exit( 1 );
    }
  }
  
}



void print_rs_encoded_clauses( void )
     
{
  
  int i, j;
  
  for ( i = 0; i < lrs_dp_num_clauses; i++ ) {
    printf("\n%d: ", i);
    for ( j = 0; j < lrs_dp_clause_length[i]; j++ ) {
      if ( lrs_dp_clauses[i][j] < 0 ) {
	printf("[%d] ", lrs_cn[(-1) * lrs_dp_clauses[i][j]] );
	printf("-");
	print_ft_name( lrs_cf[(-1) * lrs_dp_clauses[i][j]] );
	printf("(%d) ", lrs_ct[(-1) * lrs_dp_clauses[i][j]] );
      } else {
	printf("[%d] ", lrs_cn[lrs_dp_clauses[i][j]] );
	print_ft_name( lrs_cf[lrs_dp_clauses[i][j]] );
	printf("(%d) ", lrs_ct[lrs_dp_clauses[i][j]] );
      }
    }
    printf("        ---------------   i.e: ");
    for ( j = 0; j < lrs_dp_clause_length[i]; j++ ) {
      printf("%d ", lrs_dp_clauses[i][j]);
    }
  }

}



void insert_rs_posnoop_hitting_set_clauses( int nr, int ft, int time, 
					    State *deststate,
					    Bool *Ft, Bool *Ut )

{

  lrs_num_hitting_set = 0;
  next_rs_posnoop_hitting_set_step( 0,
				    nr, ft, time,
				    deststate,
				    Ft, Ut );
  
}



void next_rs_posnoop_hitting_set_step( int Uindex,
				       int nr, int ft, int time,
				       State *deststate,
				       Bool *Ft, Bool *Ut )

{

  int i, j, k, ef = -1, n, ff, m;
  
  for ( i = Uindex; i < deststate->num_unknown_E; i++ ) {
    ef = deststate->unknown_E[i];
    for ( j = 0; j < gef_conn[ef].num_D; j++ ) {
      if ( gef_conn[ef].D[j] == ft ) break;
    }
    if ( j < gef_conn[ef].num_D ) break;
  }
  
  if ( i == deststate->num_unknown_E ) {
    n = lrs_num_clauses[nr];
    if ( lrs_num_clauses[nr] == gmax_rs_clauses ) {
      printf("\n\ntoo many rs clauses? %d\n\n", lrs_dp_num_clauses);
      exit( 1 );
    }
    if ( !Ft[ft] ) {
      lrs_clauses[nr][n][lrs_clause_length[nr][n]].literal =
	(-1) * (1 + ft);
      lrs_clauses[nr][n][lrs_clause_length[nr][n]++].time =
	time;
    }
    for ( j = 0; j < lrs_num_hitting_set; j++ ) {
      /* if c == ft then the clause is true
       */
      if ( lrs_hitting_set[j] == ft ) {
	lrs_clause_length[nr][n] = 0;
	return;
      }
      /* if c == not-ft then we can skip it. got that already.
       */
      if ( gft_conn[lrs_hitting_set[j]].negation == ft ) {
	continue;
      }
      /* now, similar checks for the previously inserted hitting 
       * set members.
       */
      for ( k = 0; k < j; k++ ) {
	if ( lrs_hitting_set[j] == lrs_hitting_set[k] ) {
	  break;
	}
	if ( gft_conn[lrs_hitting_set[j]].negation == lrs_hitting_set[k] ) {
	  lrs_clause_length[nr][n] = 0;
	  return;
	}
      }
      if ( k < j ) {
	/* found duplicate; skip this j
	 */
	continue;
      }
      ff = lrs_hitting_set[j];
      if ( gft_conn[ff].CNF ) {
	m = 1;
      } else {
	m = -1;
	ff = gft_conn[ff].negation;
      }
      lrs_clauses[nr][n][lrs_clause_length[nr][n]].literal =
	m * (1 + ff);
      lrs_clauses[nr][n][lrs_clause_length[nr][n]++].time =
	time;
    }
    lrs_clauses[nr][n][lrs_clause_length[nr][n]].literal =
      1 + ft;
    lrs_clauses[nr][n][lrs_clause_length[nr][n]++].time =
      time + 1;
    lrs_num_clauses[nr]++;
    if ( gcmd_line.P && gcmd_line.debug >= 3 ) {
      printf("\npos noop clause"); print_rs_clause( nr, lrs_num_clauses[nr]-1 );
    }
    
    return;
  }
  
  /* DEBUGGING, REMOVE LATER
   */
  if ( ef == -1 ) {
    printf("\n\nHae?\n\n");
    exit( 1 );
  }
  
  if ( gcmd_line.P && gcmd_line.debug >= 4 ) {
    printf("\nhitting set for ef %d", ef);
  }
  
  lrs_num_hitting_set++;
  for ( j = 0; j < gef_conn[ef].num_C; j++ ) {
    lrs_hitting_set[lrs_num_hitting_set-1] = gef_conn[ef].C[j];
    next_rs_posnoop_hitting_set_step( i + 1,
				      nr, ft, time,
				      deststate,
				      Ft, Ut );
  }
  lrs_num_hitting_set--;
  
}



void insert_rs_negnoop_hitting_set_clauses( int nr, int ft, int time, 
					    State *deststate,
					    Bool *Ft, Bool *Ut )
     
{
  
  lrs_num_hitting_set = 0;
  next_rs_negnoop_hitting_set_step( 0,
				    nr, ft, time,
				    deststate,
				    Ft, Ut );
  
}



void next_rs_negnoop_hitting_set_step( int Uindex,
				       int nr, int ft, int time,
				       State *deststate,
				       Bool *Ft, Bool *Ut )
     
{
  
  int i, j, k, ef = -1, n, m, ff;
  
  for ( i = Uindex; i < deststate->num_unknown_E; i++ ) {
    ef = deststate->unknown_E[i];
    for ( j = 0; j < gef_conn[ef].num_A; j++ ) {
      if ( gef_conn[ef].A[j] == ft ) break;
    }
    if ( j < gef_conn[ef].num_A ) break;
  }
  
  if ( i == deststate->num_unknown_E ) {
    n = lrs_num_clauses[nr];
    if ( lrs_num_clauses[nr] == gmax_rs_clauses ) {
      printf("\n\ntoo many rs clauses? %d\n\n", lrs_dp_num_clauses);
      exit( 1 );
    }
    if ( Ut[ft] ) {
      lrs_clauses[nr][n][lrs_clause_length[nr][n]].literal =
	1 + ft;
      lrs_clauses[nr][n][lrs_clause_length[nr][n]++].time =
	time;
    }
    for ( j = 0; j < lrs_num_hitting_set; j++ ) {
      /* if c == ft then skip it. got that already.
       */
      if ( lrs_hitting_set[j] == ft ) {
	continue;
      }
      /* if c == not-ft then the clause is true.
       */
      if ( gft_conn[lrs_hitting_set[j]].negation == ft ) {
	lrs_clause_length[nr][n] = 0;
	return;
      }
      /* now, similar checks for the previously inserted hitting 
       * set members.
       */
      for ( k = 0; k < j; k++ ) {
	if ( lrs_hitting_set[j] == lrs_hitting_set[k] ) {
	  break;
	}
	if ( gft_conn[lrs_hitting_set[j]].negation == lrs_hitting_set[k] ) {
	  lrs_clause_length[nr][n] = 0;
	  return;
	}
      }
      if ( k < j ) {
	/* found duplicate; skip this j
	 */
	continue;
      }
      ff = lrs_hitting_set[j];
      if ( gft_conn[ff].CNF ) {
	m = 1;
      } else {
	m = -1;
	ff = gft_conn[ff].negation;
      }
      lrs_clauses[nr][n][lrs_clause_length[nr][n]].literal =
	m * (1 + ff);
      lrs_clauses[nr][n][lrs_clause_length[nr][n]++].time =
	time;
    }
    lrs_clauses[nr][n][lrs_clause_length[nr][n]].literal =
      (-1) * (1 + ft);
    lrs_clauses[nr][n][lrs_clause_length[nr][n]++].time =
      time + 1;
    lrs_num_clauses[nr]++;
    if ( gcmd_line.P && gcmd_line.debug >= 3 ) {
      printf("\nneg noop clause"); print_rs_clause( nr, lrs_num_clauses[nr]-1 );
    }
    
    return;
  }
  
  /* DEBUGGING, REMOVE LATER
   */
  if ( ef == -1 ) {
    printf("\n\nHae?\n\n");
    exit( 1 );
  }
  
  lrs_num_hitting_set++;
  for ( j = 0; j < gef_conn[ef].num_C; j++ ) {
    lrs_hitting_set[lrs_num_hitting_set-1] = gef_conn[ef].C[j];
    next_rs_negnoop_hitting_set_step( i + 1,
				      nr, ft, time,
				      deststate,
				      Ft, Ut );
  }
  lrs_num_hitting_set--;

}



















































/*************************************************************
 * HASHING ALGORITHM FOR RECOGNIZING DOMINATED STATES IN BFS *
 *************************************************************/



































void hash_bfs_node( BfsNode *n )

{

  int Fsum, Usum, index;
  BfsHashEntry *h, *tmp;

  Fsum = state_Fsum( &(n->S) );
  Usum = state_Usum( &(n->S) );
  index = (Fsum + Usum) & EHC_HASH_BITS;

  h = lbfs_hash_entry[index];
  if ( !h ) {
      h = new_BfsHashEntry();
      h->Fsum = Fsum;
      h->Usum = Usum;
      h->bfs_node = n;
      lbfs_hash_entry[index] = h;
      return;
  }
  for ( ; h->next; h = h->next );

  tmp = new_BfsHashEntry();
  tmp->Fsum = Fsum;
  tmp->Usum = Usum;
  tmp->bfs_node = n;
  h->next = tmp;
      
}



Bool bfs_state_hashed( State *S, BfsNode *father, int op  )

{

  int Fsum, Usum, index;
  BfsHashEntry *h;
  
  times( &end );
  TIME( gsearch_time );
  times( &start );    
  
  Fsum = state_Fsum( S );
  Usum = state_Usum( S );
  index = (Fsum + Usum) & EHC_HASH_BITS;
  
  h = lbfs_hash_entry[index];
  for ( h = lbfs_hash_entry[index]; h; h = h->next ) {
    if ( h->Fsum < Fsum ) {
      continue;
    }
    if ( h->Fsum + h->Usum < Fsum + Usum ) {
      continue;
    }
    if ( dominates_state( &(h->bfs_node->S), S,
			  NULL,NULL,
			  h->bfs_node, father,
			  op ) ) {
      times( &end );
      TIME( grs_time );
      times( &start );
      
      if ( gcmd_line.P && gcmd_line.debug ) {
	printf("\n----------------------bfs domination hit!!!");
      }
      
      return TRUE;
    } else {
      if ( gcmd_line.P && gcmd_line.debug ) {
	printf("\n----------------------no hit!!!");
      }
    }
  }
  
  times( &end );
  TIME( grs_time );
  times( &start );
  
  return FALSE;
  
}

















































/*********************************************************
 * EASY PRELIMINARY TEST: FOR GIVEN SOURCE AND DEST,   
 * IS THERE A STAGNATION IE NO IMPROVEMENT OVER S?
 * EASY TO IMPLEMENT AS CNF ALREADY COMPUTED INTO gclauses
 *********************************************************/











































/* here we test S' against *all states on the path to it*!!
 * this way we can still use the same CNF;
 *
 * implement it simply by calling the classical S x S' fn
 * for S going back into the past on the path.
 */
Bool stagnates( State *dest, 
		EhcNode *ehc_source, BfsNode *bfs_source,
		int endtime )

{

  EhcNode *iehc;
  BfsNode *ibfs;
  int anc = 1;
  
  times( &end );
  TIME( gsearch_time );
  times( &start );    
  
  if ( ehc_source ) {
    for ( iehc = ehc_source; iehc; iehc = iehc->father ) {
      if ( single_stagnates( dest, 
			     iehc, NULL,
			     endtime, anc ) ) {
	times( &end );
	TIME( gss_time );
	times( &start );
	gss_hits++;
	return TRUE;
      }
      anc++;
    }
  } else {
    if ( !bfs_source ) {
      printf("\nno ehc source but no bfs either in path stagnation check?\n\n");
      exit( 1 );
    }
    for ( ibfs = bfs_source; ibfs; ibfs = ibfs->father ) {
      if ( single_stagnates( dest, 
			     NULL, ibfs,
			     endtime, anc ) ) {
	times( &end );
	TIME( gss_time );
	times( &start );
	gss_hits++;
	return TRUE;
      }
      anc++;
    }
  }
  
  times( &end );
  TIME( gss_time );
  times( &start );
  return FALSE;

}



Bool single_stagnates( State *dest, 
		       EhcNode *ehc_source, BfsNode *bfs_source,
		       int endtime, int ancestornr )

{

  int i, j, k, ft, m, ff;
  State *source;
  int ft_source_code, ft_dest_code;
  Bool sat;
  
  if ( ehc_source ) {
    source = &(ehc_source->S);
  } else {
    source = &(bfs_source->S);
  }
  
  if ( gcmd_line.P && gcmd_line.debug ) {
    printf("\n-------------------------------------checking stagnation, S anc %d",
	   ancestornr);
    print_state( *source );
    printf("\n-------------------------------------S'");
    print_state( *dest );
  }


  if ( gdomination_valid ) {
    for ( i = 0; i < dest->num_F; i++ ) {
      if ( !gft_conn[dest->F[i]].srelevant ) continue;
      for ( j = 0; j < source->num_F; j++ ) {
	if ( source->F[j] == dest->F[i] ) {
	  break;
	}
      }
      if ( j == source->num_F ) {
	if ( gcmd_line.P && gcmd_line.debug ) {
	  printf("\n---------------S.F !supseteq S'.F|r:");
	  print_ft_name( dest->F[i] );
	}
	return FALSE;
      }
    }

    for ( i = 0; i < dest->num_U; i++ ) {
      if ( !gft_conn[dest->U[i]].srelevant ) continue;
      for ( j = 0; j < source->num_F; j++ ) {
	if ( source->F[j] == dest->U[i] ) {
	  break;
	}
      }
      if ( j == source->num_F ) {
	for ( j = 0; j < source->num_U; j++ ) {
	  if ( source->U[j] == dest->U[i] ) {
	    break;
	  }
	}
	if ( j == source->num_U ) {
	  if ( gcmd_line.P && gcmd_line.debug ) {
	    printf("\n---------------S.F cup S.U !sup S'.U|r:");
	    print_ft_name( dest->U[i] );
	  }
	  return FALSE;
	}
      }
    }
  } else {/* domination not valid */
    if ( source->num_F != dest->num_F ) {
      if ( gcmd_line.P && gcmd_line.debug ) {
	printf("\n---------------|S.F| != |S'.F|");
      }  
      return FALSE;
    }
    for ( i = 0; i < dest->num_F; i++ ) {
      for ( j = 0; j < source->num_F; j++ ) {
	if ( source->F[j] == dest->F[i] ) {
	  break;
	}
      }
      if ( j == source->num_F ) {
	if ( gcmd_line.P && gcmd_line.debug ) {
	  printf("\n---------------S.F != S'.F:");
	  print_ft_name( dest->F[i] );
	}
	return FALSE;
      }
    }
    
    /* dest.U == source.U?
     */
    if ( dest->num_U != source->num_U ) {
      if ( gcmd_line.P && gcmd_line.debug ) {
	printf("\n---------------|S.U| != |S'.U|");
      }
      return FALSE;
    }
    for ( i = 0; i < dest->num_U; i++ ) {
      for ( j = 0; j < source->num_U; j++ ) {
	if ( source->U[j] == dest->U[i] ) {
	  break;
	}
      }
      if ( j == source->num_U ) {
	if ( gcmd_line.P && gcmd_line.debug ) {
	  printf("\n---------------S.U != S'.U:");
	  print_ft_name( dest->U[i] );
	}
	return FALSE;
      }
    }
  } /* end pre-checks */
  
  
  /* and now the real thing: for all U (in dest) fts f,
   * see whether f can be true in dest but not in S!
   */
  for ( i = 0; i < dest->num_U; i++ ) {
    ft = dest->U[i];
    if ( gcmd_line.P && gcmd_line.debug ) {
      printf("\n--------------potentially non-stagnating ft ");
      print_ft_name( ft );
    }
    
    if ( !gft_conn[ft].srelevant ) {
      if ( gcmd_line.P && gcmd_line.debug ) {
	printf(" is not solution relevant!");
      }
      continue;
    }

    if ( gdomination_valid ) {
      /* if that ft is true in S it stagnates.
       */
      for ( k = 0; k < source->num_F; k++ ) {
	if ( source->F[k] == ft ) {
	  break;
	}
      }
      if ( k < source->num_F ) {
	if ( gcmd_line.P && gcmd_line.debug ) {
	  printf(" in S.F!");
	}
	continue;
      }
    }

    /* we're getting close to SAT. get the codes and do some debugging.
     */
    if ( endtime < 1 ) {
      printf("\ndest ends at endtime %d?\n\n", endtime);
      exit( 1 );
    }
    if ( endtime - ancestornr < 0 ) {
      printf("\ndest ends at endtime %d, but ancestor is %d?\n\n", 
	     endtime, ancestornr);
      exit( 1 );
    }
    ff = ft;
    if ( gft_conn[ff].CNF ) {
      m = 1;
    } else {
      m = -1;
      ff = gft_conn[ff].negation;
    }
    ft_source_code = gcodes[endtime-ancestornr][ff];
    ft_dest_code = gcodes[endtime][ff];
    /* if there is no code for ft in S, which is U, then (see text in
     * state_transitions.cpp, conflict check fn) it should hold that
     * endtime - 1 = 0, ie S is initial state. debug test.
     */ 
    if ( ft_source_code == -1 ) {
      if ( endtime - ancestornr != 0 ) {
	printf("\nft S var not encoded, but U at time > 0??\n\n");
	exit( 1 );
      }
      if ( gcmd_line.P && gcmd_line.debug ) {
	printf(" not encoded at S, therefore unconstrained wrpt (transitive?) conds so these can trigger");
      }
      /* no code -> no clauses -> conds of eff can be true even if ft isn't 
       * in (inst of) S --- unless ft was one of these conds which is useless and
       * removed in inst_final.c; if further back ancestor, it's a bit strange;
       * on the other hand, adding the single clause -ft(Stime) would yield
       * no constraint propagation and thus the CNF would be solvable so we
       * can spare us the effort.
       */
      return FALSE;
    }
    if ( ft_dest_code == -1 ) {
      printf("\nft S' var not encoded??\n\n");
      exit( 1 );
    }
    if ( gcmd_line.P && gcmd_line.debug >= 3 ) {
      printf("\nft S code %d, ft S' code %d",
	     ft_source_code, ft_dest_code);
    }
    
    
    /* here we go. must test sat.
     */
    gnum_decision_stack = 0;
    gdecision_stack[gnum_decision_stack++] = m * (-1) * ft_source_code;
    gdecision_stack[gnum_decision_stack++] = m * ft_dest_code;
    if ( gcmd_line.P && gcmd_line.debug  >= 3 ) {
      printf("\nstagnation check 1 coded CNF\n");
      print_encoded_clauses();
    }
    /* here it comes..
     */
    gss_sat_calls++;
    times( &end );
    TIME( gss_time );
    sat = dp_CNF();
    gss_time += gDP_time;
    times( &start );
    if ( sat ) {
      if ( gcmd_line.P && gcmd_line.debug ) {
	printf("\nsat! can be false at S but true at S'!");
      }
      return FALSE;
    }  
    if ( !gdomination_valid ) {
      /* 2nd test to ensure equality
       */
      gnum_decision_stack = 0;
      gdecision_stack[gnum_decision_stack++] = m * ft_source_code;
      gdecision_stack[gnum_decision_stack++] = m * (-1) * ft_dest_code;
      if ( gcmd_line.P && gcmd_line.debug  >= 3 ) {
	printf("\nstagnation check 2 coded CNF\n");
	print_encoded_clauses();
      }
      /* here it comes..
       */
      gss_sat_calls++;
      times( &end );
      TIME( gss_time );
      sat = dp_CNF();
      gss_time += gDP_time;
      times( &start );
      if ( sat ) {
	if ( gcmd_line.P && gcmd_line.debug ) {
	  printf("\nsat! can be true at S but false at S'!");
	}
	return FALSE;
      }
    }
    if ( gcmd_line.P && gcmd_line.debug ) {
      printf("\ntests unsat! stagnates!");
    }
  } /* for all dest->U facts */
  
  return TRUE;

}








































/***********************************************************
 * WE ALL LIVE IN A YELLOW SUBROUTINE:                     * 
 *   HERE COMES A SAT SOLVER                               *
 ***********************************************************/











































/* top level control fn for (extremely) naive DP implementation...
 */

Bool rs_dp_CNF( void )

{

  int i, j, v, sign, c, sum;
  Bool sat;
  MemberList *i_ml, *j_ml;


  gdp_calls++;

  if ( lrs_num_decision_stack == 0 ) {
    printf("\nrs no decision stack at entering DP??\n\n"); 
    exit( 1 );
  }

  if ( gcmd_line.debug ) {
    for ( i = 0; i < gmax_literals + 1; i++ ) {
      for ( j = lrs_dp_num_clauses - 1; j >= 0; j-- ) {
	if ( lrs_dp_clause_length[j] == i ) gsum_k_clauses[i]++;
	if ( lrs_dp_clause_length[j] > gmax_literals ) {
	  printf("\n\nrs ???\n\n");
	}
      }
    }
    gsum_clauses += lrs_dp_num_clauses;
  }

  if ( gcmd_line.debug ) {
    sum = -1;
    for ( i = lrs_dp_num_clauses - 1; i >= 0; i-- ) {
      for ( j = 0; j < lrs_dp_clause_length[i]; j++ ) {
	v = (lrs_dp_clauses[i][j] > 0) ? 
	  lrs_dp_clauses[i][j] : (-1) * lrs_dp_clauses[i][j];
	if ( sum == -1 || v > sum ) {
	  sum = v;
	}
      }
    }
    if ( sum != lrs_num_c ) {
      printf("\nrs lrs_dp_num_clauses: %d, counted lrs_num_c: %d, old: %d", lrs_dp_num_clauses, sum, lrs_num_c);
      exit( 1 );
    }
  }


  if ( gcmd_line.debug ) {
    /* Oh... fuck you all: double-2-2-check that memlists are correct!!!
     */
    for ( i = 1; i <= lrs_num_c; i++ ) {
      for ( i_ml = lrs_pos_c_in_clause_start[i]->next; 
	    i_ml != lrs_pos_c_in_clause_end[i]; i_ml = i_ml->next ) {
	c = i_ml->clause;
	for ( j = 0; j < lrs_dp_clause_length[c]; j++ ) {
	  if ( lrs_dp_clauses[c][j] == i ) break;
	}
	if ( j == lrs_dp_clause_length[c] ) {
	  printf("\nrs error 1\n\n");
	  exit( 1 );
	}
	for ( j_ml = i_ml->next; 
	      j_ml != lrs_pos_c_in_clause_end[i]; j_ml = j_ml->next ) {
	  if ( j_ml->clause == i_ml->clause ) {
	    printf("\nrs error 1b\n\n");
	    exit( 1 );
	  }
	}
      }
      for ( i_ml = lrs_neg_c_in_clause_start[i]->next; 
	    i_ml != lrs_neg_c_in_clause_end[i]; i_ml = i_ml->next ) {
	c = i_ml->clause;
	for ( j = 0; j < lrs_dp_clause_length[c]; j++ ) {
	  if ( lrs_dp_clauses[c][j] == (-1) * i ) break;
	}
	if ( j == lrs_dp_clause_length[c] ) {
	  printf("\nrs error 2\n\n");
	  exit( 1 );
	}
	for ( j_ml = i_ml->next; 
	      j_ml != lrs_neg_c_in_clause_end[i]; j_ml = j_ml->next ) {
	  if ( j_ml->clause == i_ml->clause ) {
	    printf("\nrs error 2b\n\n");
	    exit( 1 );
	  }
	}
      }
    }
    for ( i = 0; i < lrs_dp_num_clauses; i++ ) {
      for ( j = 0; j < lrs_dp_clause_length[i]; j++ ) {
	if ( lrs_dp_clauses[i][j] > 0 ) {
	  v = lrs_dp_clauses[i][j];
	  if ( lrs_pos_c_in_clause_start[v] == NULL ) {
	    printf("\nrs error 3a\n\n");
	    exit( 1 );
	  }
	  for ( i_ml = lrs_pos_c_in_clause_start[v]->next; 
		i_ml != lrs_pos_c_in_clause_end[v]; i_ml = i_ml->next ) {
	    if ( i_ml->clause == i ) break;
	  }
	  if ( i_ml == lrs_pos_c_in_clause_end[v] ) {
	    printf("\nrs error 3\n\n");
	    exit( 1 );
	  }
	} else {
	  v = (-1) * lrs_dp_clauses[i][j];
	  if ( lrs_neg_c_in_clause_start[v] == NULL ) {
	    printf("\nrs error 4a\n\n");
	    exit( 1 );
	  }
	  for ( i_ml = lrs_neg_c_in_clause_start[v]->next; 
		i_ml != lrs_neg_c_in_clause_end[v]; i_ml = i_ml->next ) {
	    if ( i_ml->clause == i ) break;
	  }
	  if ( i_ml == lrs_neg_c_in_clause_end[v] ) {
	    printf("\nrs error 4\n\n");
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
  while ( i < lrs_num_decision_stack ) {
    if ( lrs_decision_stack[i] < 0 ) {
      v = (-1) * lrs_decision_stack[i];
      sign = 0;
    } else {
      v = lrs_decision_stack[i];
      sign = 1;
    }
    if ( lrs_assigned[v] == sign ) {
      if ( gcmd_line.P && gcmd_line.debug >= 5 ) {
	printf("\nrs skipping pre-insertion %d of var %d val %d, already set so",
	       i, v, sign);
      }
      for ( j = i; j < lrs_num_decision_stack-1; j++ ) {
	lrs_decision_stack[j] = lrs_decision_stack[j+1];
      }
      lrs_num_decision_stack--;
      continue;
    }
    if ( lrs_assigned[v] != -1 ) {
      if ( gcmd_line.P && gcmd_line.debug >= 5 ) {
	printf("\nrs contradictory pre-insertion %d of var %d val %d!",
	       i, v, sign);
      }
      /* unset info
       */
      for ( j = 0; j < i; j++ ) {
	if ( lrs_decision_stack[j] < 0 ) {
	  v = (-1) * lrs_decision_stack[j];
	  sign = 0;
	} else {
	  v = lrs_decision_stack[j];
	  sign = 1;
	}
	lrs_assigned[v] = -1;
      }
      return FALSE;
    }
    /* insert new assignment
     */
    if ( gcmd_line.P && gcmd_line.debug >= 5 ) {
      printf("\nrs pre-inserting pos %d: var %d val %d",
	     i, v, sign);
    }
    lrs_assigned[v] = sign;
    i++;
  }


  if ( lrs_num_decision_stack == 0 ) {
    printf("\nrs no decision stack after pre-inserting DP??\n\n"); 
    exit( 1 );
  }

  /* now do the corresponding unit props for all the pre-insertions
   * in sequence; is implicit because do_unit_props will move through
   * entire (growing) decision stack, starting from start index
   *
   * no contradiction --> do DP!
   */
  sat = FALSE;
  if ( rs_do_unit_props( 0 ) ) {
    sat = rs_dp();
  }

  /* undo assignments
   */
  for ( i = 0; i < lrs_num_decision_stack; i++ ) {
    if ( lrs_decision_stack[i] < 0 ) {
      v = (-1) * lrs_decision_stack[i];
    } else {
      v = lrs_decision_stack[i];
    }
    if ( gcmd_line.P && gcmd_line.debug >= 5 ) {
      printf("\nrs top retracting dec stack %d, var %d val %d!",
	     i, v, lrs_decision_stack[i] < 0 ? 0 : 1);
    }
    lrs_assigned[v] = -1;
  }
  /* not strictly necessary, as full (..) initialisation done above 
   * before DP calls... looks cleaner this way...
   */
  lrs_num_decision_stack = 0;

  return sat;

}



/* invariant: lrs_num_decision_stack is same on entry and exit; same holds for
 * lrs_assigned array.
 */
Bool rs_dp( void )

{

  int entry_num_decision_stack;
  int i, v, dec_v;

  if ( gcmd_line.P && gcmd_line.debug >= 5 ) {
    printf("\nrs entering naive dp");
  }

  /* store the entry nr. of decisions on the stack, for later
   * retraction of the new decisions.
   */
  entry_num_decision_stack = lrs_num_decision_stack;


  /* choose split vars from "backside of plan"
   */
  for ( dec_v = lrs_num_c; dec_v > 0; dec_v-- ) {
    if ( lrs_assigned[dec_v] == -1 ) break;
  }
  if ( dec_v == 0 ) {
    /* all vars assigned and no contradiction --> sat!
     */
    if ( gcmd_line.P && gcmd_line.debug >= 5 ) {
      printf("\nrs all vars assigned, no contradiction -> sat!");
    }
    return TRUE;
  }

  /* split on v.
   *
   * first, positive.
   */
  if ( lrs_num_decision_stack == (1 + MAX_PLAN_LENGTH) * gmax_CNFU * 2 ) {
    printf("\nrs dec stack overflow??\n\n");
    exit( 1 );
  }
  lrs_decision_stack[lrs_num_decision_stack++] = dec_v;
  lrs_assigned[dec_v] = 1;
  if ( gcmd_line.P && gcmd_line.debug >= 5 ) {
    printf("\nrs decide %d true, doing unit props", dec_v);
  } 
  if ( rs_do_unit_props( lrs_num_decision_stack-1 ) ) {
    if ( rs_dp() ) {
      /* found solution.
       */
      return TRUE;
    }
  }
  for ( i = entry_num_decision_stack; i < lrs_num_decision_stack; i++ ) {
    if ( lrs_decision_stack[i] < 0 ) {
      v = (-1) * lrs_decision_stack[i];
    } else {
      v = lrs_decision_stack[i];
    }
    if ( gcmd_line.P && gcmd_line.debug >= 5 ) {
      printf("\nrs true tried retracting dec stack %d, var %d val %d!",
	     i, v, lrs_decision_stack[i] < 0 ? 0 : 1);
    }
    lrs_assigned[v] = -1;
  }
  lrs_num_decision_stack = entry_num_decision_stack;


  /* now, negative.
   */
  lrs_decision_stack[lrs_num_decision_stack++] = (-1) * dec_v;
  lrs_assigned[dec_v] = 0;
  if ( gcmd_line.P && gcmd_line.debug >= 5 ) {
    printf("\nrs decide %d false, doing unit props", dec_v);
  } 
  if ( rs_do_unit_props( lrs_num_decision_stack-1 ) ) {
    if ( rs_dp() ) {
      /* found solution.
       */
      return TRUE;
    }
  }
  for ( i = entry_num_decision_stack; i < lrs_num_decision_stack; i++ ) {
    if ( lrs_decision_stack[i] < 0 ) {
      v = (-1) * lrs_decision_stack[i];
    } else {
      v = lrs_decision_stack[i];
    }
    if ( gcmd_line.P && gcmd_line.debug >= 5 ) {
      printf("\nrs false tried retracting dec stack %d, var %d val %d!",
	     i, v, lrs_decision_stack[i] < 0 ? 0 : 1);
    }
    lrs_assigned[v] = -1;
  }
  lrs_num_decision_stack = entry_num_decision_stack;

  return FALSE;

}



Bool rs_do_unit_props( int startidx )

{

  int idx, i, v, c, v_, numopenlits, lastopen;  
  MemberList *i_ml;

  gup_calls++;

  idx = startidx;
  while ( idx < lrs_num_decision_stack ) {
    if ( gcmd_line.P && gcmd_line.debug >= 5 ) {
      printf("\nrs propagating dec stack %d, %d!",
	     idx, lrs_decision_stack[idx]);
      fflush(stdout);
    }
    if ( lrs_decision_stack[idx] < 0 ) {
      v = (-1) * lrs_decision_stack[idx];
      for ( i_ml = lrs_pos_c_in_clause_start[v]->next; 
	    i_ml != lrs_pos_c_in_clause_end[v]; i_ml = i_ml->next ) {
	c = i_ml->clause;
	numopenlits = 0;
	lastopen = 0;
	for ( i = 0; i < lrs_dp_clause_length[c]; i++ ) {
	  if ( numopenlits > 1 ) break;
	  if ( lrs_dp_clauses[c][i] > 0 ) {
	    v_ = lrs_dp_clauses[c][i];
	    if ( lrs_assigned[v_] == -1 ) {
	      numopenlits++;
	      lastopen = lrs_dp_clauses[c][i];
	    }
	    if ( lrs_assigned[v_] == 1 ) {
	      /* clause satisfied!
	       */
	      break;
	    }
	  } else {
	    v_ = (-1) * lrs_dp_clauses[c][i];
	    if ( lrs_assigned[v_] == -1 ) {
	      numopenlits++;
	      lastopen = lrs_dp_clauses[c][i];
	    }
	    if ( lrs_assigned[v_] == 0 ) {
	      /* clause satisfied!
	       */
	      break;
	    }
	  }
	}
	if ( i < lrs_dp_clause_length[c] ) {
	  /* clause sat or >1 open lit! ignore.
	   */
	  continue;
	}
	if ( numopenlits == 0 ) {
	  if ( gcmd_line.P && gcmd_line.debug >= 5 ) {
	    printf("\nrs empty clause %d", c);
	  }
	  /* no fulfilled and no open lits --> contradiction!!
	   */
	  return FALSE;
	}
	if ( numopenlits == 1 ) {
	  if ( gcmd_line.P && gcmd_line.debug >= 5 ) {
	    printf("\nrs unit clause %d", c);
	  }
	  if ( lrs_num_decision_stack == (1 + MAX_PLAN_LENGTH) * gmax_CNFU * 2 ) {
	    printf("\nrs dec stack overflow??\n\n");
	    exit( 1 );
	  }
	  /* this one's unit. constrain the variable.
	   */
	  /* DEBUGGING. REMOVE LATER.
	   */
	  if ( lastopen == 0 ) {
	    printf("\n\nrs lastopen 0?\n\n");
	    exit( 1 );
	  }
	  if ( lastopen > 0 ) {
	    if ( gcmd_line.P && gcmd_line.debug >= 5 ) {
	      printf("\nrs UP constr %d true", lastopen);
	    }
	    lrs_assigned[lastopen] = 1;
	  } else {
	    if ( gcmd_line.P && gcmd_line.debug >= 5 ) {
	      printf("\nrs UP constr %d false", (-1) * lastopen);
	    }
	    lrs_assigned[(-1) * lastopen] = 0;
	  }
	  lrs_decision_stack[lrs_num_decision_stack++] = lastopen;
	} /* numopenlits == 1 */
      } /* clauses in that negated v participates */
    } else { /* if decision here is positive */
      v = lrs_decision_stack[idx];
      fflush(stdout);
      for ( i_ml = lrs_neg_c_in_clause_start[v]->next; 
	    i_ml != lrs_neg_c_in_clause_end[v]; i_ml = i_ml->next ) {
	c = i_ml->clause;
	numopenlits = 0;
	lastopen = 0;
	for ( i = 0; i < lrs_dp_clause_length[c]; i++ ) {
	  if ( numopenlits > 1 ) break;
	  if ( lrs_dp_clauses[c][i] > 0 ) {
	    v_ = lrs_dp_clauses[c][i];
	    if ( lrs_assigned[v_] == -1 ) {
	      numopenlits++;
	      lastopen = lrs_dp_clauses[c][i];
	    }
	    if ( lrs_assigned[v_] == 1 ) {
	      /* clause satisfied!
	       */
	      break;
	    }
	  } else {
	    v_ = (-1) * lrs_dp_clauses[c][i];
	    if ( lrs_assigned[v_] == -1 ) {
	      numopenlits++;
	      lastopen = lrs_dp_clauses[c][i];
	    }
	    if ( lrs_assigned[v_] == 0 ) {
	      /* clause satisfied!
	       */
	      break;
	    }
	  }
	}
	if ( i < lrs_dp_clause_length[c] ) {
	  /* clause sat or >1 open lit! ignore.
	   */
	  continue;
	}
	if ( numopenlits == 0 ) {
	  if ( gcmd_line.P && gcmd_line.debug >= 5 ) {
	    printf("\nrs empty clause %d", c);
	  }
	  /* no fulfilled and no open lits --> contradiction!!
	   */
	  return FALSE;
	}
	if ( numopenlits == 1 ) {
	  if ( gcmd_line.P && gcmd_line.debug >= 5 ) {
	    printf("\nrs unit clause %d", c);
	  }
	  if ( lrs_num_decision_stack == (1 + MAX_PLAN_LENGTH) * gmax_CNFU * 2 ) {
	    printf("\nrs dec stack overflow??\n\n");
	    exit( 1 );
	  }
	  /* this one's unit. constrain the variable.
	   */
	  /* DEBUGGING. REMOVE LATER.
	   */
	  if ( lastopen == 0 ) {
	    printf("\n\nrs lastopen 0?\n\n");
	    exit( 1 );
	  }
	  if ( lastopen > 0 ) {
	    if ( gcmd_line.P && gcmd_line.debug >= 5 ) {
	      printf("\nrs UP constr %d true", lastopen);
	    }
	    lrs_assigned[lastopen] = 1;
	  } else {
	    if ( gcmd_line.P && gcmd_line.debug >= 5 ) {
	      printf("\nrs UP constr %d false", (-1) * lastopen);
	    }
	    lrs_assigned[(-1) * lastopen] = 0;
	  }
	  lrs_decision_stack[lrs_num_decision_stack++] = lastopen;
	} /* numopenlits == 1 */
      } /* clauses in that positive v participates */
    } /* neg or pos */
    idx++;
  } /* while not all decisions on stack propagated */

  return TRUE;

}



void rs_update_membership_lists( void )

{

  struct tms s, e;

  int i, j, v;
  MemberList *i_ml;

  times( &s );

  /* first, "empty" the lists.
   */
  for ( i = 1; i <= lrs_num_c; i++ ) {
    /* for new vars, create the dummy elements. else, just set the
     * "end marker" directly behind start.
     */
    if ( lrs_pos_c_in_clause_start[i] == NULL ) {
      lrs_pos_c_in_clause_start[i] = new_MemberList();
      lrs_pos_c_in_clause_start[i]->clause = -1;
      lrs_pos_c_in_clause_end[i] = new_MemberList();
      lrs_pos_c_in_clause_end[i]->clause = -1;
      lrs_pos_c_in_clause_start[i]->next = lrs_pos_c_in_clause_end[i];
    } else {
      lrs_pos_c_in_clause_end[i] = lrs_pos_c_in_clause_start[i]->next;
    }
    if ( lrs_neg_c_in_clause_start[i] == NULL ) {
      lrs_neg_c_in_clause_start[i] = new_MemberList();
      lrs_neg_c_in_clause_start[i]->clause = -1;
      lrs_neg_c_in_clause_end[i] = new_MemberList();
      lrs_neg_c_in_clause_end[i]->clause = -1;
      lrs_neg_c_in_clause_start[i]->next = lrs_neg_c_in_clause_end[i];
    } else {
      lrs_neg_c_in_clause_end[i] = lrs_neg_c_in_clause_start[i]->next;
    }
  }

  /* insert clauses from back to front; dunno if that has any significant effect
   * but it's become a kind of tradition..
   */
  for ( i = lrs_dp_num_clauses - 1; i >= 0; i-- ) {
    for ( j = 0; j < lrs_dp_clause_length[i]; j++ ) {
      if ( lrs_dp_clauses[i][j] > 0 ) {
	/* positive, is the var.
	 */
	v = lrs_dp_clauses[i][j];
	if ( lrs_pos_c_in_clause_end[v]->clause == -1 ) {
	  /* we're at the end of the allocated list!
	   */
	  lrs_pos_c_in_clause_end[v]->clause = i;
	  lrs_pos_c_in_clause_end[v]->next = new_MemberList();
	  lrs_pos_c_in_clause_end[v]->next->clause = -1;
	  lrs_pos_c_in_clause_end[v] = lrs_pos_c_in_clause_end[v]->next;
	} else {
	  /* we're still in the middle of the list.
	   */
	  lrs_pos_c_in_clause_end[v]->clause = i;
	  lrs_pos_c_in_clause_end[v] = lrs_pos_c_in_clause_end[v]->next;
	} /* case distinction for end pointer of v list */
      } else { /* literal negative; do the same, basically */
	v = (-1) * lrs_dp_clauses[i][j];
	if ( lrs_neg_c_in_clause_end[v]->clause == -1 ) {
	  /* we're at the end of the allocated list!
	   */
	  lrs_neg_c_in_clause_end[v]->clause = i;
	  lrs_neg_c_in_clause_end[v]->next = new_MemberList();
	  lrs_neg_c_in_clause_end[v]->next->clause = -1;
	  lrs_neg_c_in_clause_end[v] = lrs_neg_c_in_clause_end[v]->next;
	} else {
	  /* we're still in the middle of the list.
	   */
	  lrs_neg_c_in_clause_end[v]->clause = i;
	  lrs_neg_c_in_clause_end[v] = lrs_neg_c_in_clause_end[v]->next;
	} /* case distinction for end pointer of v list */
      } /* literal sign distinction */
    } /* clause elements */
  } /* clauses */

  times( &e );    
  gmembership_time += ( float ) ( ( e.tms_utime - s.tms_utime + \
				    e.tms_stime - s.tms_stime  ) / 100.0 );
  if ( gcmd_line.P && gcmd_line.debug >= 5 ) {
    printf("\nrs membership lists:");
    for ( i = 1; i <= lrs_num_c; i++ ) {
      printf("\nrs var %d pos:", i);
      for ( i_ml = lrs_pos_c_in_clause_start[i]->next; 
	    i_ml != lrs_pos_c_in_clause_end[i]; i_ml = i_ml->next ) {
	printf(" %d", i_ml->clause);
      }
      printf("\nrs var %d neg:", i);
      for ( i_ml = lrs_neg_c_in_clause_start[i]->next; 
	    i_ml != lrs_neg_c_in_clause_end[i]; i_ml = i_ml->next ) {
	printf(" %d", i_ml->clause);
      }
    }
    fflush(stdout);
  }

}

