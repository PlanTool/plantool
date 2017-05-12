


/*
 * THIS SOURCE CODE IS SUPPLIED  ``AS IS'' WITHOUT WARRANTY OF ANY KIND, 
 * AND ITS AUTHOR AND THE JOURNAL OF ARTIFICIAL INTELLIGENCE RESEARCH 
 * (JAIR) AND JAIR'S PUBLISHERS AND DISTRIBUTORS, DISCLAIM ANY AND ALL 
 * WARRANTIES, INCLUDING BUT NOT LIMITED TO ANY IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE, AND
 * ANY WARRANTIES OR NON INFRINGEMENT.  THE USER ASSUMES ALL LIABILITY AND
 * RESPONSIBILITY FOR USE OF THIS SOURCE CODE, AND NEITHER THE AUTHOR NOR
 * JAIR, NOR JAIR'S PUBLISHERS AND DISTRIBUTORS, WILL BE LIABLE FOR 
 * DAMAGES OF ANY KIND RES.LTING FROM ITS USE.  Without limiting the 
 * generality of the foregoing, neither the author, nor JAIR, nor JAIR's
 * publishers and distributors, warrant that the Source Code will be 
 * error-free, will operate without interruption, or will meet the needs 
 * of the user.
 */











/*********************************************************************
 *
 * File: state_transitions.c
 *
 * Description: computes new states. quite complicated.
 *
 * Author: Joerg Hoffmann 2002
 *
 *********************************************************************/ 





extern "C" {
#include "ff.h"

#include "output.h"
#include "memory.h"

#include "relax.h"
#include "state_transitions.h"
#include "repeated_states.h"
#include "search.h"
}




#include "Cachet-1.21-wmc/SAT.h"

#include "Solver.h"

#include <set.h>
#include <vector.h>














/***********************************************************
 * LOCAL GLOBALS                                           *
 ***********************************************************/








/* hitting set -- in CNF construction for noops in presence of non-unary antecedents.
 */
int *lhitting_set;
int lnum_hitting_set;










/* make space for global arrays needed here
 */
void initialize_state_transitions( void )

{

  int i, j;

  gclauses = ( TimedLiteral ** ) calloc( gmax_clauses, sizeof( TimedLiteral * ) );
  gclause_length = ( int * ) calloc( gmax_clauses, sizeof( int ) );
  for ( i = 0; i < gmax_clauses; i++ ) {
    gclauses[i] = ( TimedLiteral * ) calloc( gmax_literals, sizeof( TimedLiteral ) );
    gclause_length[i] = 0;
  }
  gnum_fixed_clauses = 0;
  gnum_clauses = 0;

  gcodes = ( int ** ) calloc( MAX_PLAN_LENGTH + 1, sizeof( int * ) );
  /* july06: use artificial (ie new) decision vars
   * saying whether a prob outcome actually occurs, + weioghted chance vars saying woith what prob.
   * encode these as (gnum_ft_conn + gef_conn[ef].pef_id|pef_chance_id)
   */
  for ( i = 0; i < MAX_PLAN_LENGTH + 1; i++ ) {
    gcodes[i] = ( int * ) calloc( gnum_ft_conn + gnum_pef_conn, sizeof( int ) );
    for ( j = 0; j < gnum_ft_conn + gnum_pef_conn; j++ ) {
      gcodes[i][j] = -1;
    }
  }
  gnum_fixed_c = 0;

  /* july06: 1001 to make space for chance node codes.
   */
  gcf = ( int * ) calloc( (1 + MAX_PLAN_LENGTH) * gmax_CNFU + 1001, sizeof( int ) );
  gct = ( int * ) calloc( (1 + MAX_PLAN_LENGTH) * gmax_CNFU + 1001, sizeof( int ) );

  /* july06: weight of dynaically created chance nodes
   */
  gcweight = ( double * ) 
    calloc( (1 + MAX_PLAN_LENGTH) * gmax_CNFU + 1001, sizeof( double ) );
  
  lhitting_set = ( int * ) calloc( gnum_ft_conn + gnum_pef_conn, sizeof( int ) );

  gdecision_stack = ( int * ) calloc( (1 + MAX_PLAN_LENGTH) * gmax_CNFU, sizeof( int ) );


  /* statistics...
   */
  gsum_k_clauses = ( float * ) calloc( gmax_literals + 1, sizeof( float ) );
  for ( i = 0; i < gmax_literals + 1; i++ ) {
    gsum_k_clauses[i] = 0;
  }


  ginitial_ft_weight = ( double * ) calloc( gnum_ft_conn, sizeof( double ) );
  for ( i = 0; i < gnum_ft_conn; i++ ) {
    if ( !gft_conn[i].weighted ) {
      ginitial_ft_weight[i] = -1;
      continue;
    }
    ginitial_ft_weight[i] = gft_conn[i].weight_a / gft_conn[i].weight_b;
  }

}


















/***********************************************************
 * THIS HERE IS THE "MAIN" FN, CALLED DIRECTLY FROM SEARCH *
 ***********************************************************/
















/* function that computes state transition as induced by a
 * normalized ADL action under unknown literals.
 * (uses CNF decision mechanism for the semantics of the latter)
 *
 * must take in search states as path to the source state is
 * needed for computing inferred literals.
 * when called from ehc, bfs_source is NULL and vice versa.
 *
 * implementation is largely naive, look at that again if it's
 * runtime relevant --- which I doubt.
 */
Bool result_to_dest( State *dest, 
		     EhcNode *ehc_source, BfsNode *bfs_source,
		     int op )

{

  static Bool first_call = TRUE;
  static Bool *F, *U, *Npp, *adds, *dels, *nadds, *ndels, *uadds, *udels, *unadds, *undels;
  static int *check_pos, *check_neg;

  State *source;


  int num_cp, num_cn;
  int i, j, fcount, ucount, ft, ef, k, endtime, nextstart, nextcstart;

  Bool retval = TRUE;



  if ( ehc_source ) {
    source = &(ehc_source->S);
  } else {
    source = &(bfs_source->S);
  }

  if ( first_call ) {
    F = ( Bool * ) calloc( gnum_ft_conn, sizeof( Bool ) );
    U = ( Bool * ) calloc( gnum_ft_conn, sizeof( Bool ) );
    Npp = ( Bool * ) calloc( gnum_ft_conn, sizeof( Bool ) );
    adds = ( Bool * ) calloc( gnum_ft_conn, sizeof( Bool ) );
    dels = ( Bool * ) calloc( gnum_ft_conn, sizeof( Bool ) );
    nadds = ( Bool * ) calloc( gnum_ft_conn, sizeof( Bool ) );
    ndels = ( Bool * ) calloc( gnum_ft_conn, sizeof( Bool ) );
    uadds = ( Bool * ) calloc( gnum_ft_conn, sizeof( Bool ) );
    udels = ( Bool * ) calloc( gnum_ft_conn, sizeof( Bool ) );
    unadds = ( Bool * ) calloc( gnum_ft_conn, sizeof( Bool ) );
    undels = ( Bool * ) calloc( gnum_ft_conn, sizeof( Bool ) );
    check_pos = ( int * ) calloc( gnum_ft_conn, sizeof( int ) );
    check_neg = ( int * ) calloc( gnum_ft_conn, sizeof( int ) );
    for ( i = 0; i < gnum_ft_conn; i++ ) {
      F[i] = FALSE;
      U[i] = FALSE;
      Npp[i] = FALSE;
      adds[i] = FALSE;
      dels[i] = FALSE;
      nadds[i] = FALSE;
      ndels[i] = FALSE;
      uadds[i] = FALSE;
      udels[i] = FALSE;
      unadds[i] = FALSE;
      undels[i] = FALSE;
    }
    
    first_call = FALSE;
  }



  /* setup direct F and U info
   */
  for ( i = 0; i < source->num_F; i++ ) {
    F[source->F[i]] = TRUE;
  }
  for ( i = 0; i < source->num_U; i++ ) {
    U[source->U[i]] = TRUE;
  }
  /* setup known and unknown adds and dels.
   */
  dest->num_F = 0;
  dest->num_U = 0;
  dest->num_unknown_E = 0;
  for ( i = 0; i < gop_conn[op].num_E; i++ ) {
    ef = gop_conn[op].E[i];
    fcount = 0;
    ucount = 0;
    for ( j = 0; j < gef_conn[ef].num_C; j++ ) {
      if ( F[gef_conn[ef].C[j]] ) fcount++;
      if ( U[gef_conn[ef].C[j]] ) ucount++;
      /* DEBUGGING TEST. REMOVE LATER.
       */
      if ( F[gef_conn[ef].C[j]] && U[gef_conn[ef].C[j]] ) {
	printf("\nsource F and U intersec non-empty?\n\n");
	    exit(1);
      }
    }
    if ( fcount == gef_conn[ef].num_C ) {
      if ( gef_conn[ef].eff_p >= 1 ) {
	for ( j = 0; j < gef_conn[ef].num_A; j++ ) {
	  adds[gef_conn[ef].A[j]] = TRUE;
	}
	for ( j = 0; j < gef_conn[ef].num_D; j++ ) {
	  dels[gef_conn[ef].D[j]] = TRUE;
	}
      } else {
	/* july06: in any case, we need this guy in here
	 * so that the clauses will be done, for wmc.
	 */
	dest->unknown_E[dest->num_unknown_E++] = ef;
	/* july06: common nondet adds/dels behave just like normal ones.
	 */
	for ( j = 0; j < gef_conn[ef].num_SA; j++ ) {
	  adds[gef_conn[ef].SA[j]] = TRUE;
	}
	for ( j = 0; j < gef_conn[ef].num_SD; j++ ) {
	  dels[gef_conn[ef].SD[j]] = TRUE;
	}
	/* july06: non-common nondet adds/dels are special.
	 */
	for ( j = 0; j < gef_conn[ef].num_NSA; j++ ) {
	  nadds[gef_conn[ef].NSA[j]] = TRUE;
	}
	for ( j = 0; j < gef_conn[ef].num_NSD; j++ ) {
	  ndels[gef_conn[ef].NSD[j]] = TRUE;
	}
      }
      continue;
    }
    if ( fcount + ucount == gef_conn[ef].num_C ) {
      dest->unknown_E[dest->num_unknown_E++] = ef;
      if ( gef_conn[ef].eff_p >= 1 ) {
	for ( j = 0; j < gef_conn[ef].num_A; j++ ) {
	  uadds[gef_conn[ef].A[j]] = TRUE;
	}
	for ( j = 0; j < gef_conn[ef].num_D; j++ ) {
	  udels[gef_conn[ef].D[j]] = TRUE;
	}
      } else {
	for ( j = 0; j < gef_conn[ef].num_SA; j++ ) {
	  uadds[gef_conn[ef].SA[j]] = TRUE;
	}
	for ( j = 0; j < gef_conn[ef].num_SD; j++ ) {
	  udels[gef_conn[ef].SD[j]] = TRUE;
	}
	for ( j = 0; j < gef_conn[ef].num_NSA; j++ ) {
	  unadds[gef_conn[ef].NSA[j]] = TRUE;
	}
	for ( j = 0; j < gef_conn[ef].num_NSD; j++ ) {
	  undels[gef_conn[ef].NSD[j]] = TRUE;
	}
      }
    }
  }


  /* setup the CNF for the following SAT calls.
   */

  /* EFFICIENCY: HERE, AND BELOW, ONE COULD DO SIMPLE PRE-CHECKS TO SEE
   * WHETHER THERE'LL BE ANY SAT REASONING NEEDED::: ONLY IF EITHER
   * OP CAN CONTRADICT ITSELF OR WHEN THERE ARE POS/NEG CHECK FACTS!!
   */
 
  times( &end );
  TIME( gsearch_time );
  times( &start );    
  
  /* first, extend the current fixed clauses with the search path up to source.
   * will be the same for all literals to be checked (?)
   */
  endtime = extend_dynamic_clauses_base( dest,
					 ehc_source, bfs_source,
					 op, 
					 Npp,
					 TRUE );

  nextstart = gnum_clauses;

  times( &end );
  TIME( gcnf_time );
  times( &start );
  
  /* then extend the encoding, ie map the literal / time pairs
   * into integers.
   *
   * Mar'04 NOTE: this is wasteful and completely unnecessary - we could just as well 
   *              implement DP directly on the timed actions encoding.
   *              well... the int IDs help in indexing into the "assigned" array...
   *
   * the translation code is in table gcodes for use below in the
   * single checks.
   */
  extend_dynamic_clauses_base_encoding( gnum_fixed_clauses, gnum_fixed_c );
  nextcstart = gnum_c;

  times( &end );
  TIME( genc_time );
  times( &start );


  /* check for possible contradictions.
   */
  for ( ft = 0; ft < gnum_ft_conn; ft++ ) {
    if ( adds[ft] && dels[ft] ) {
      if ( gcmd_line.T && gcmd_line.debug ) {
	printf("\naction ");
	print_op_name( op );
	printf(" contradicts itself. skipping it.");
      }
      retval = FALSE;
    }
    if ( adds[ft] && (udels[ft] || ndels[ft] || undels[ft]) ) {
      if ( gcmd_line.T && gcmd_line.debug ) {
	printf("\naction ");
	print_op_name( op );
	printf(" can delete an add. skipping it.");
	print_ft_name( ft ); exit( 1 );
      }
      retval = FALSE;
    }
    if ( (uadds[ft] || nadds[ft] || unadds[ft]) && dels[ft] ) {
      if ( gcmd_line.T && gcmd_line.debug ) {
	printf("\naction ");
	print_op_name( op );
	printf(" can add a delete. skipping it.");
      }
      retval = FALSE;
    }
    /* july06: disabled. who gives a shit about this? it just takes time.
     */
//     if ( uadds[ft] && udels[ft] ) {
//       if ( gcmd_line.T && gcmd_line.debug ) {
// 	printf("\naction ");
// 	print_op_name( op );
// 	printf(" may contradict itself on fact ");
// 	print_ft_name( ft );
// 	printf(". checking that.");
//       }
//       if ( can_contradict_in( op, ft, 
// 			      dest, ehc_source, bfs_source, endtime - 1 ) ) {
// 	retval = FALSE;
//       }
//     }
  }



  if ( retval ) {
    /* now proceed over all facts and see whether they will be true,
     * false, or unknown for sure, and whether we must check if they
     * become true or false.
     */
    dest->num_F = 0;
    dest->num_U = 0;
    num_cp = 0;
    num_cn = 0;
    if ( gcmd_line.T && gcmd_line.debug ) {
      printf("\n\n---------------------------state transition, setups");
    }
    for ( ft = 0; ft < gnum_ft_conn; ft++ ) {
      if ( F[ft] ) {
	/* this was true; note that per assumption all adds and dels
	 * are cap empty
	 */
	if ( dels[ft] ) {
	  /* remember, for use in CNF, that we *know* this is false
	   */
	  Npp[ft] = TRUE;
	  continue;/* state-wise, false <-> do not include ft into F' or U' */
	}
	/* july06: this one gets deleted by some effects so it'll definitely
	 * be unknown at tpp.
	 */
	if ( ndels[ft] || undels[ft] ) {
	  if ( gcmd_line.T && gcmd_line.debug ) {
	    printf("\n---becomes U due to nondet del "); 
	    print_ft_name( ft );
	  }
	  dest->U[dest->num_U++] = ft;
	  continue;
	}
	/* note: order matters, ie the above are asked first!
	 */
	if ( udels[ft] ) {
	  /* might become negative; can not stay pos as then all del effs
	   * would need to provably stay out, and we'd know that
	   */
	  check_neg[num_cn++] = ft;
	  continue;
	}
	if ( gcmd_line.T && gcmd_line.debug ) {
	  printf("\n---remains true "); 
	  print_ft_name( ft );
	}
	dest->F[dest->num_F++] = ft;
	continue;
      }
      if ( U[ft] ) {
	/* this was unknown
	 */
	if ( adds[ft] ) {
	  dest->F[dest->num_F++] = ft;
	  continue;
	}
	if ( dels[ft] ) {
	  /* remember, for use in CNF, that we *know* this is false
	   */
	  Npp[ft] = TRUE;
	  continue;
	}
	if ( uadds[ft] ) {
	  check_pos[num_cp++] = ft;
	  continue;
	}	      
	if ( udels[ft] ) {
	  check_neg[num_cn++] = ft;
	  continue;
	}
	dest->U[dest->num_U++] = ft;
	continue;
      }
      /* this was false
       */ 
      if ( adds[ft] ) {
	dest->F[dest->num_F++] = ft;
	continue;
      }
      /* july06: this one gets added by some effects so it'll definitely
       * be unknown at tpp.
       */
      if ( nadds[ft] || unadds[ft] ) {
	if ( gcmd_line.T && gcmd_line.debug ) {
	  printf("\n---becomes U due to nondet add "); 
	  print_ft_name( ft );
	}
	dest->U[dest->num_U++] = ft;
	continue;
      }
      /* note: order matters, ie the above are asked first!
       */
      if ( uadds[ft] ) {
	/* similar to above this can not stay false, so if it not
	 * becomes pos then it becomes U.
	 */
	check_pos[num_cp++] = ft;
	continue;
      }
      /* if it's not added then we do nothing, leaving it false.
       *
       * we remember, though, for use in CNF, that we *know* this is false
       */
      if ( gcmd_line.T && gcmd_line.debug ) {
	printf("\n---remains false "); 
	print_ft_name( ft );
      }
      Npp[ft] = TRUE;
    }
    
    times( &end );
    TIME( gsearch_time );
    times( &start );    
    
    /* now, extend the current clauses with the step source -> dest.
     * will be the same for all literals to be checked (?)
     */
    endtime = extend_dynamic_clauses_base( dest,
					   ehc_source, bfs_source,
					   op, 
					   Npp,
					   FALSE );
    times( &end );
    TIME( gcnf_time );
    times( &start );
    /* goes over fixed until num, so some of these have been encoded above;
     * don't matter as in these cases coded remain the same !?
     */
    extend_dynamic_clauses_base_encoding( nextstart, nextcstart );
    gcnfs++;
    times( &end );
    TIME( genc_time );
    times( &start );   
    
    /* hand the to-be-checked facts over to a SAT solver
     */
    handle_inferred_literals( dest, ehc_source, bfs_source, op,
			      check_pos, num_cp,
			      check_neg, num_cn,
			      Npp, endtime );
    
    
    
    /* DEBUGGING TEST. REMOVE LATER.
     * (unknown E s computed at start already can not interfere with
     *  F' and U' values, can they?)
     */
    if ( gcmd_line.T && gcmd_line.debug ) {
      for ( i = 0; i < dest->num_unknown_E; i++ ) {
	ef = dest->unknown_E[i];
	/* can the effect del an F' fact?
	 */
	for ( j = 0; j < gef_conn[ef].num_D; j++ ) {
	  for ( k = 0; k < dest->num_F; k++ ) {
	    if ( gef_conn[ef].D[j] == dest->F[k] ) {
	      printf("\nunknown effect deletes F'?");
	    }
	  }
	}
	/* can the effect add an N' fact?
	 */
	for ( j = 0; j < gef_conn[ef].num_A; j++ ) {
	  for ( k = 0; k < dest->num_F; k++ ) {
	    if ( gef_conn[ef].A[j] == dest->F[k] ) {
	      break;
	    }
	  }
	  if ( k < dest->num_F ) continue;
	  for ( k = 0; k < dest->num_U; k++ ) {
	    if ( gef_conn[ef].A[j] == dest->U[k] ) {
	      break;
	    }
	  }
	  if ( k < dest->num_U ) continue;
	  printf("\nunknown effect adds N'?");
	}
      }
    }
  
    /* finally, check whether this new state stagnates, ie whether
     * it is not the case that one fact can be true in it that isn't
     * in source. stagnating states are cut out by making the action
     * inapplicable.
     *
     * (NOTE: stagnation test easy to implement and set up here as 
     *  CNF already present in gclauses!!)
     */
    if ( gcmd_line.stagnating && stagnates( dest, ehc_source, bfs_source, endtime ) ) {
      if ( gcmd_line.T && gcmd_line.debug ) {
	printf("\nsuccessor state stagnates! skipping action!");
      }
      retval = FALSE;
    }
  } /* if retval */

  /* undo the clauses and encoding infos
   */
  for ( i = gnum_fixed_clauses; i < gnum_clauses; i++ ) {
    gclause_length[i] = 0;
  }
  gnum_clauses = gnum_fixed_clauses;
  for ( i = gnum_fixed_c + 1; i <= gnum_c; i++ ) {
    gcodes[gct[i]][gcf[i]] = -1;
  }

  /* unset direct infos
   */
  for ( i = 0; i < gnum_ft_conn; i++ ) {
    F[i] = FALSE;
    U[i] = FALSE;
    Npp[i] = FALSE;
    adds[i] = FALSE;
    dels[i] = FALSE;
    nadds[i] = FALSE;
    ndels[i] = FALSE;
    uadds[i] = FALSE;
    udels[i] = FALSE;
    unadds[i] = FALSE;
    undels[i] = FALSE;
  }

  return retval;

}





















/**************************************************************
 * CONFORMANT SEMANTICS (CALLED BY STATE TRANSITION FUNCTION) *
 **************************************************************/
























/* about the paths: if we get a bfs node then this is simply the sequence of
 * nodes reachable via backchaining over ->father
 * in an ehc node we either have a father, then we backchain til father
 * is empty then continue with gplan_states;
 * or we have no father but an op in which case the sequence is gplanstates
 * plus our states
 * or we have neither father nor op (==-1) in which case the node is
 * the current iteration starting state and the sequence is exactly
 * gplan_states.
 */





/* implementation largely naive. if critical, reconsider.
 */
void handle_inferred_literals( State *dest, 
			       EhcNode *ehc_source, BfsNode *bfs_source, 
			       int op,/* only for debugging .. */
			       int *check_pos, int num_cp,
			       int *check_neg, int num_cn,
			       Bool *Npp, int endtime )

{

  static Bool fc = TRUE;
  
  int i, j, m, ff;
  State *source;
  Bool sat;
  
  if ( ehc_source ) {
    source = &(ehc_source->S);
  } else {
    source = &(bfs_source->S);
  }
  
  if ( fc ) {
    fc = FALSE;
  }
  
  if ( num_cp + num_cn == 0 ) {
    /* naught to check --> get back.
     */
    return;
  }
  
  /* proceed over the queries, check, and update dest info.
   *
   * NOTE: the to-be-checked facts all appear in unknown effects
   * so they appear in the clauses and thus in partic in the gcodes
   * table! make sure that it is so with a little debugging test.
   */
  if ( gcmd_line.T && gcmd_line.debug ) {
    printf("\n\n-------------------------checks, entry state");
    print_state( *dest );
  }
  for ( i = 0; i < num_cp; i++ ) {
    if ( gcmd_line.T && gcmd_line.debug ) {
      printf("\nchecking positive: "); 
      print_ft_name( check_pos[i] );
      printf("(%d)", endtime);
    }
    ff = check_pos[i];
    if ( gft_conn[ff].CNF ) {
      m = 1;
    } else {
      m = -1;
      ff = gft_conn[ff].negation;
    }
    /* DEBUGGING, REMOVE LATER
     */
    if ( gcodes[endtime][ff] == -1 ) {
      printf("\n\npos to-be-checked var not encoded?\n\n");
      exit(1);
    }
    /* NEW: we don't actually insert unit clauses, but push the respective
     * decisions onto the stack used in DP. that is, we initialise these 
     * values here!!
     */
    gnum_decision_stack = 0;
    gdecision_stack[gnum_decision_stack++] = m * (-1) * gcodes[endtime][ff];
    if ( gcmd_line.T && gcmd_line.debug  >= 4 ) {
      print_encoded_clauses();
    }
    /* here goes the SAT solver!
     *
     * curr CNF unsolvable --> fact proved true at endtime
     */
    /* do not count time for communicating clauses to Chaff.
     */
    times( &end );
    TIME( gsearch_time );
    gsat_calls++;
    sat = dp_CNF();
    gsat_time += gDP_time;
    times( &start );
    if ( sat ) {
      if ( gcmd_line.T && gcmd_line.debug ) {
	printf("\nsat! unknown "); print_ft_name( check_pos[i] );
      }
      dest->U[dest->num_U++] = check_pos[i];
    } else {
      if ( gcmd_line.T && gcmd_line.debug ) {
	printf("\nunsat! proved true");
      }
      dest->F[dest->num_F++] = check_pos[i];
    }
  } /* endfor i over cp */
  /* here we could also check whether the negation (ie the artificial
   * inverse) is proved true or false already --> then insert inversely
   * without thinking... at the time, keep it full to see if it works.
   *
   *
   * old note: (??) july06
   * NOTE: ONE COULD ALSO COMPLETEY LEAVE THE ARTIFICIAL NEG VARS
   *       COMPLETELY OUT OF THE CLAUSES, AND USE -(ORGVAR) INSTEAD!!!
   *       KEEP ON TO-DO-LIST FOR LATER!! err... already done?!
   */
  for ( i = 0; i < num_cn; i++ ) {
    if ( gcmd_line.T && gcmd_line.debug ) {
      printf("\nchecking negative: "); 
      print_ft_name( check_neg[i] );
      printf("(%d)", endtime);
    }
    if ( gft_conn[check_neg[i]].negation != -1 ) {
      for ( j = 0; j < dest->num_F; j++ ) {
	if ( dest->F[j] == gft_conn[check_neg[i]].negation ) break;
      }
      if ( j < dest->num_F ) {
	if ( gcmd_line.T && gcmd_line.debug ) {
	  printf("\nnegation proved true!");
	}
	continue;
      }
      for ( j = 0; j < dest->num_U; j++ ) {
	if ( dest->U[j] == gft_conn[check_neg[i]].negation ) break;
      }
      if ( j < dest->num_U ) {
	if ( gcmd_line.T && gcmd_line.debug ) {
	  printf("\nnegation unknown!");
	}
	dest->U[dest->num_U++] = check_neg[i];
	continue;
      }
      printf("\nnegation of neg check there, but neither F nor U yet??\n\n");
      exit( 1 );
    }
    ff = check_neg[i];
    if ( gft_conn[ff].CNF ) {
      m = 1;
    } else {
      m = -1;
      ff = gft_conn[ff].negation;
    }
    if ( gcodes[endtime][ff] == -1 ) {
      printf("\n\nneg to-be-checked var not encoded?\n\n");
      exit(1);
    }
    /* NEW: we don't actually insert unit clauses, but push the respective
     * decisions onto the stack used in DP. that is, we initialise these 
     * values here!!
     */
    gnum_decision_stack = 0;
    gdecision_stack[gnum_decision_stack++] = m * gcodes[endtime][ff];
    if ( gcmd_line.T && gcmd_line.debug  >= 4 ) {
      printf("\n");
      print_encoded_clauses();
    }
    /* here goes the SAT solver!
     * curr CNF unsolvable --> fact proved false at endtime
     */
    times( &end );
    TIME( gsearch_time );
    gsat_calls++;
    sat = dp_CNF();
    gsat_time += gDP_time;
    times( &start );
    if ( sat ) {
      if ( gcmd_line.T && gcmd_line.debug ) {
	printf("\nsat! unknown");
      }
      dest->U[dest->num_U++] = check_neg[i];
    } else {
      /* false <-> noeither U nor F
       */
      if ( gcmd_line.T && gcmd_line.debug ) {
	printf("\nunsat! proved false");
      }
    }
  } /* endfor i over cn */

  if ( gcmd_line.T && gcmd_line.debug ) {
    printf("\n\n-------------------------checks, exit state");
    print_state( *dest );
  }

}



void extend_fixed_clauses_base( int low_state, int high_state )

{

  static Bool fc = TRUE;
  static State_pointer *path;
  static int *path_op;
  static Bool *Ft, *Ut, *Ftpp, *Utpp;
  /* july06: for each set of alternative e's, only 
   * 1 set of pefclauses. store for each effect if or if not
   * we already had those.
   */
  static Bool *have_pefclauses_for;
  
  int i, j, k, l, ef, num_path, t, time, ft, ff, m;
  int ef1, ef2;



  
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
    
    have_pefclauses_for = ( Bool * ) calloc( gnum_ef_conn, sizeof( Bool ) );
    
    fc = FALSE;
  }
  
  gfixed_endtime = high_state;
  
  if ( gcmd_line.T && gcmd_line.debug >= 2 ) {
    printf("\n\n-----------------------fixed path");
  }
  num_path = 0;
  for ( i = low_state; i <= high_state; i++ ) {
    path_op[num_path] = gplan_ops[i];
    path[num_path++] = &(gplan_states[i]);
    if ( gcmd_line.T && gcmd_line.debug >= 3 ) {
      printf("\n-----state %d", num_path - 1); print_state( *path[num_path - 1] );
    }
    if ( gcmd_line.T && gcmd_line.debug >= 2 && i < high_state ) {
      printf("\n-----op "); print_op_name( gplan_ops[i] );
    }
  }
  
  /* now the states leading to (including) source (and I) are in 
   * path[num_path + 1] .. path[MAX_PLAN_LENGTH-1].
   * unknown efs leading to each state are stored in the state,
   * (also in dest already) so we
   * produce our CNF based on these, 
   * the unknowns in source-dest (separate treatment as here
   * being not in F neither in U does not mean being provably
   * false),
   * as well as the initial implications.
   *
   * start with the initial implications.
   */
  if ( gcmd_line.T && gcmd_line.debug ) {
    printf("\n\n-----------------------adding clauses from %d",
	   gnum_fixed_clauses);
  }
  gnum_clauses = gnum_fixed_clauses;
  if ( low_state == 0 && high_state == 0 ) {
    /* insert the initially valid ORs given by user
     */
    for ( i = 0; i < gnum_initial_or; i++ ) {
      if ( gnum_clauses == gmax_clauses ) {
	printf("\n\ntoo many clauses? %d\n\n", gnum_clauses);
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
	gclauses[gnum_clauses][gclause_length[gnum_clauses]].literal = m * (1 + ff);
	gclauses[gnum_clauses][gclause_length[gnum_clauses]].time = 0;
	gclause_length[gnum_clauses]++;
      }
      gnum_clauses++;
      if ( gcmd_line.T && gcmd_line.debug ) {
	printf("\ninitial OR clause"); print_clause( gnum_clauses-1 ); fflush(stdout);
      }
    }
  }

  /* insert the t -> t+1 implications for all t on the path.
   *
   * the times here are t-(num_path+1) -> t-(num_path+1)+1 for t in:
   */
  for ( t = 0; t < num_path - 1; t++ ) {
    time = t + low_state;
    
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
	if ( gcmd_line.T && gcmd_line.debug ) {
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
	    if ( gcmd_line.T && gcmd_line.debug ) {
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
	  if ( gcmd_line.T && gcmd_line.debug ) {
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
	if ( gcmd_line.T && gcmd_line.debug ) {
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
	if ( gcmd_line.T && gcmd_line.debug ) {
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
  }
  gnum_fixed_clauses = gnum_clauses;
 
  /* debugging: see what that looks like!
   */
  if ( gcmd_line.T && gcmd_line.debug >= 7 ) {
    if ( gcmd_line.T && gcmd_line.debug >= 8 ) {
      printf("\n\n----------------------------- to fixed execution path\n");
      if ( gcmd_line.T && gcmd_line.debug >= 9 ) {
	printf("\n---------state:");
	print_state( *(path[0]) );
      }
      for ( t = 1; t < num_path; t++ ) {
	printf("\n-----------op ");
	print_op_name( gplan_ops[low_state + t - 1] );
	if ( gcmd_line.T && gcmd_line.debug >= 9 ) {
	  printf("\n----------state:");
	  print_state( *(path[t]) );
	}
      }
    }
    printf("\n\n----------------------------- I get the CNF (fixed up to %d)\n", 
	   gnum_fixed_clauses);
    print_clauses();
  }

}



/* collection of hitting sets largely naive. reconsider if critical.
 */
void insert_posnoop_hitting_set_clauses( int ft, int time, 
					 State *deststate,
					 Bool *Ft, Bool *Ut )

{

  lnum_hitting_set = 0;
  next_posnoop_hitting_set_step( 0,
				 ft, time,
				 deststate,
				 Ft, Ut );
 
}



void next_posnoop_hitting_set_step( int Uindex,
				    int ft, int time,
				    State *deststate,
				    Bool *Ft, Bool *Ut )

{

  int i, j, k, ef = -1, ff, m;

  for ( i = Uindex; i < deststate->num_unknown_E; i++ ) {
    ef = deststate->unknown_E[i];
    for ( j = 0; j < gef_conn[ef].num_D; j++ ) {
      if ( gef_conn[ef].D[j] == ft ) break;
    }
    if ( j < gef_conn[ef].num_D ) break;
  }

  if ( i == deststate->num_unknown_E ) {
    /* we got the hitting set. create the clause.
     *
     * apply several checks to avoid redundancies.
     */
    if ( gnum_clauses == gmax_clauses ) {
      printf("\n\ntoo many clauses? %d\n\n", gnum_clauses);
      exit( 1 );
    }
    if ( !Ft[ft] ) {
      ff = ft;
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
    for ( j = 0; j < lnum_hitting_set; j++ ) {
      /* if c == ft then the clause is true
       */
      if ( lhitting_set[j] == ft ) {
	gclause_length[gnum_clauses] = 0;
	return;
      }
      /* if c == not-ft then we can skip it. got that already.
       *
       * NOTE: THIS ALREADY IS AN OPTIMIZATION UTILIZING 
       * NEG TRANSLATION STRUCTURES!!
       */
      if ( lhitting_set[j] < gnum_ft_conn && gft_conn[lhitting_set[j]].negation == ft ) {
	continue;
      }
      /* july06: oops. found a bug, or at least ommission, in the previous code:
       * if c is known to be true in t, then the clause is satisfied and we can skip it.
       */
      if ( lhitting_set[j] < gnum_ft_conn && Ft[lhitting_set[j]] ) {
	gclause_length[gnum_clauses] = 0;
	return;
      }
      /* july06: oops. found a bug, or at least ommission, in the previous code:
       * if c is known to be false in t, then the literal is false and we can skip it.
       */
      if ( lhitting_set[j] < gnum_ft_conn && 
	   !Ft[lhitting_set[j]] && !Ut[lhitting_set[j]] ) {
	continue;
      }

      /* now, checks for the previously inserted hitting 
       * set members.
       *
       * NOTE: the member k might not have been inserted, in 1st check?
       *       but then the reason must be another identical member.
       */
      for ( k = 0; k < j; k++ ) {
	if ( lhitting_set[j] == lhitting_set[k] ) {
	  /* had this.
	   */
	  break;
	}
	if ( lhitting_set[j] < gnum_ft_conn && gft_conn[lhitting_set[j]].negation == lhitting_set[k] ) {
	  /* clause true.
	   */
	  gclause_length[gnum_clauses] = 0;
	  return;
	}
      }
      if ( k < j ) {
	/* found duplicate; skip this j
	 */
	continue;
      }      
      ff = lhitting_set[j];
      if ( ff < gnum_ft_conn ) {
	if ( gft_conn[ff].CNF ) {
	  m = 1;
	} else {
	  m = -1;
	  ff = gft_conn[ff].negation;
	}
      } else {
	/* july06: this is not a fact, ergo a pef id.
	 */
	m = 1;
      }
      gclauses[gnum_clauses][gclause_length[gnum_clauses]].literal =
	m * (1 + ff);
      gclauses[gnum_clauses][gclause_length[gnum_clauses]++].time =
	time;
    }
    ff = ft;
    if ( gft_conn[ff].CNF ) {
      m = 1;
    } else {
      m = -1;
      ff = gft_conn[ff].negation;
    }
    gclauses[gnum_clauses][gclause_length[gnum_clauses]].literal =
      m * (1 + ff);
    gclauses[gnum_clauses][gclause_length[gnum_clauses++]++].time =
      time + 1;
    if ( gcmd_line.T && gcmd_line.debug ) {
      printf("\npos noop clause"); print_clause( gnum_clauses-1 );
    }

    return;
  }

  /* DEBUGGING, REMOVE LATER
   */
  if ( ef == -1 ) {
    printf("\n\nHae?\n\n");
    exit( 1 );
  }

  if ( gcmd_line.T && gcmd_line.debug >= 2 ) {
    printf("\nhitting set for ef %d", ef);
  }



  /* july06: if this guy is probabilistic, also give him a chance
   * to "prevent himself"
   */
  if ( gef_conn[ef].eff_p < 1 ) {
    lnum_hitting_set++;
    lhitting_set[lnum_hitting_set-1] = gnum_ft_conn + gef_conn[ef].pef_id;
    next_posnoop_hitting_set_step( i + 1,
				   ft, time,
				   deststate,
				   Ft, Ut );
    lnum_hitting_set--;
  }
  

  lnum_hitting_set++;
  for ( j = 0; j < gef_conn[ef].num_C; j++ ) {
    /* remember the hitting set fact
     */
    lhitting_set[lnum_hitting_set-1] = gef_conn[ef].C[j];
    /* now, recursively, selct the possible completions of the set, and create the clauses.
     */
    next_posnoop_hitting_set_step( i + 1,
				   ft, time,
				   deststate,
				   Ft, Ut );
  }
  lnum_hitting_set--;

}



void insert_negnoop_hitting_set_clauses( int ft, int time, 
					 State *deststate,
					 Bool *Ft, Bool *Ut )

{

  lnum_hitting_set = 0;
  next_negnoop_hitting_set_step( 0,
				 ft, time,
				 deststate,
				 Ft, Ut );
 
}



void next_negnoop_hitting_set_step( int Uindex,
				    int ft, int time,
				    State *deststate,
				    Bool *Ft, Bool *Ut )

{

  int i, j, k, ef = -1, m, ff;

  for ( i = Uindex; i < deststate->num_unknown_E; i++ ) {
    ef = deststate->unknown_E[i];
    for ( j = 0; j < gef_conn[ef].num_A; j++ ) {
      if ( gef_conn[ef].A[j] == ft ) break;
    }
    if ( j < gef_conn[ef].num_A ) break;
  }

  if ( i == deststate->num_unknown_E ) {
    /* we got the hitting set. create the clause.
     *
     * apply several checks to avoid redundancies.
     */
    if ( gnum_clauses == gmax_clauses ) {
      printf("\n\ntoo many clauses? %d\n\n", gnum_clauses);
      exit( 1 );
    }
    if ( Ut[ft] ) {
      ff = ft;
      if ( gft_conn[ff].CNF ) {
	m = 1;
      } else {
	m = -1;
	ff = gft_conn[ff].negation;
      }
      gclauses[gnum_clauses][gclause_length[gnum_clauses]].literal =
	m * (1 + ff);
      gclauses[gnum_clauses][gclause_length[gnum_clauses]++].time =
	time;
    }
    for ( j = 0; j < lnum_hitting_set; j++ ) {
      /* if c == ft then skip it. got that already.
       */
      if ( lhitting_set[j] == ft ) {
	continue;
      }
      /* if c == not-ft then the clause is true.
       *
       * NOTE: THIS ALREADY IS AN OPTIMIZATION UTILIZING 
       * NEG TRANSLATION STRUCTURES!!
       */
      if ( lhitting_set[j] < gnum_ft_conn && gft_conn[lhitting_set[j]].negation == ft ) {
	gclause_length[gnum_clauses] = 0;
	return;
      }
      /* july06: oops. found a bug, or at least ommission, in the previous code:
       * if c is known to be true in t, then the clause is satisfied and we can skip it.
       */
      if ( lhitting_set[j] < gnum_ft_conn && Ft[lhitting_set[j]] ) {
	gclause_length[gnum_clauses] = 0;
	return;
      }
      /* july06: oops. found a bug, or at least ommission, in the previous code:
       * if c is known to be false in t, then the literal is false and we can skip it.
       */
      if ( lhitting_set[j] < gnum_ft_conn && 
	   !Ft[lhitting_set[j]] && !Ut[lhitting_set[j]] ) {
	continue;
      }

      /* now, checks for the previously inserted hitting 
       * set members.
       *
       * NOTE: the member k might not have been inserted, in 1st check?
       *       but then the reason must be another identical member.
       */
      for ( k = 0; k < j; k++ ) {
	if ( lhitting_set[j] == lhitting_set[k] ) {
	  /* had this.
	   */
	  break;
	}
	if ( lhitting_set[j] < gnum_ft_conn && gft_conn[lhitting_set[j]].negation == lhitting_set[k] ) {
	  /* clause true.
	   */
	  gclause_length[gnum_clauses] = 0;
	  return;
	}
      }
      if ( k < j ) {
	/* found duplicate; skip this j
	 */
	continue;
      }
      ff = lhitting_set[j];
      if ( ff < gnum_ft_conn ) {
	if ( gft_conn[ff].CNF ) {
	  m = 1;
	} else {
	  m = -1;
	  ff = gft_conn[ff].negation;
	}
      } else {
	/* july06: this is not a fact, ergo a pef id.
	 */
	m = 1;
      }
      gclauses[gnum_clauses][gclause_length[gnum_clauses]].literal =
	m * (1 + ff);
      gclauses[gnum_clauses][gclause_length[gnum_clauses]++].time =
	time;
    }
    ff = ft;
    if ( gft_conn[ff].CNF ) {
      m = 1;
    } else {
      m = -1;
      ff = gft_conn[ff].negation;
    }
    gclauses[gnum_clauses][gclause_length[gnum_clauses]].literal =
      m * (-1) * (1 + ff);
    gclauses[gnum_clauses][gclause_length[gnum_clauses++]++].time =
      time + 1;
    if ( gcmd_line.T && gcmd_line.debug ) {
      printf("\nneg noop clause"); print_clause( gnum_clauses-1 );
    }

    return;
  }

  /* DEBUGGING, REMOVE LATER
   */
  if ( ef == -1 ) {
    printf("\n\nHae?\n\n");
    exit( 1 );
  }



  /* july06: if this guy is probabilistic, also give him a chance
   * to "prevent himself"
   */
  if ( gef_conn[ef].eff_p < 1 ) {
    lnum_hitting_set++;
    lhitting_set[lnum_hitting_set-1] = gnum_ft_conn + gef_conn[ef].pef_id;
    next_negnoop_hitting_set_step( i + 1,
				   ft, time,
				   deststate,
				   Ft, Ut );
    lnum_hitting_set--;
  }


  lnum_hitting_set++;
  for ( j = 0; j < gef_conn[ef].num_C; j++ ) {
    /* remember the hitting set fact
     */
    lhitting_set[lnum_hitting_set-1] = gef_conn[ef].C[j];
    /* now, recursively, selct the possible completions of the set, and create the clauses.
     */
    next_negnoop_hitting_set_step( i + 1,
				   ft, time,
				   deststate,
				   Ft, Ut );
  }
  lnum_hitting_set--;

}



int extend_dynamic_clauses_base( State *dest, 
				 EhcNode *ehc_source, BfsNode *bfs_source, 
				 int op,
				 Bool *Npp,
				 Bool firstpart )

{

  static Bool fc = TRUE;
  static State_pointer *path;
  static int *path_op;
  static Bool *Ft, *Ut, *Ftpp, *Utpp;
  /* july06: for each set of alternative e's, only 
   * 1 set of pefclauses. store for each effect if or if not
   * we already had those.
   */
  static Bool *have_pefclauses_for;
  
  int i, j, k, l, ef, num_path, t, time, ft, ff, m;
  int ef1, ef2;

  State *source;
  EhcNode *iehc;
  BfsNode *ibfs;
  
  if ( ehc_source ) {
    source = &(ehc_source->S);
  } else {
    source = &(bfs_source->S);
  }
  
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
    
    have_pefclauses_for = ( Bool * ) calloc( gnum_ef_conn, sizeof( Bool ) );
    
    fc = FALSE;
  }
  
  
  
  if ( gcmd_line.T && gcmd_line.debug >= 2 ) {
    printf("\n\n-----------------------dynamic exct path");
  }
  num_path = MAX_PLAN_LENGTH;
  path[num_path--] = dest;
  path_op[num_path+1] = op;
  if ( gcmd_line.T && gcmd_line.debug >= 3 ) {
    printf("\n-----state %d", num_path+1); print_state( *path[num_path+1] );
  }
  if ( gcmd_line.T && gcmd_line.debug >= 2 ) {
    printf("\n-----op "); print_op_name( path_op[num_path+1] );
  }
  if ( ehc_source ) {
    for ( iehc = ehc_source; iehc->father; iehc = iehc->father ) {
      if ( num_path == 0 ) {
	printf("\n\nincrease MAX_PLAN_LENGTH! currently %d\n\n",
	       MAX_PLAN_LENGTH);
	exit( 1 );
      }
      path[num_path--] = &(iehc->S);
      path_op[num_path+1] = iehc->op;
      if ( gcmd_line.T && gcmd_line.debug >= 3 ) {
	printf("\n-----state %d", num_path+1); print_state( *path[num_path+1] );
      }
      if ( gcmd_line.T && gcmd_line.debug >= 2 ) {
	printf("\n-----op "); print_op_name( path_op[num_path+1] );
      }
    }
    if ( num_path == 0 ) {
      printf("\n\nincrease MAX_PLAN_LENGTH! currently %d\n\n",
	     MAX_PLAN_LENGTH);
      exit( 1 );
    }
    path[num_path--] = &(gplan_states[gnum_plan_ops]);
    if ( gcmd_line.T && gcmd_line.debug >= 3 ) {
      printf("\n-----state %d", num_path+1); print_state( *path[num_path+1] );
    }
  } else {
    for ( ibfs = bfs_source; ibfs; ibfs = ibfs->father ) {
      if ( num_path == 0 ) {
	printf("\n\nincrease MAX_PLAN_LENGTH! currently %d\n\n",
	       MAX_PLAN_LENGTH);
	exit( 1 );
      }
      path[num_path--] = &(ibfs->S);
      if ( gcmd_line.T && gcmd_line.debug >= 3 ) {
	printf("\n-----state %d", num_path+1); print_state( *path[num_path+1] );
      }
      if ( ibfs->father ) {
	path_op[num_path+1] = ibfs->op;
	if ( gcmd_line.T && gcmd_line.debug >= 2 ) {
	  printf("\n-----op "); print_op_name( path_op[num_path+1] );
	}
      }
    }
  }
  
  
  /* the states leading from current end-fixed-path-state to dest are in 
   * path[num_path + 1] .. path[MAX_PLAN_LENGTH].
   * unknown efs leading to each state are stored in the state,
   * (also in dest already) so we
   * produce our CNF based on these.
   */
  if ( firstpart ) {
    if ( gcmd_line.T && gcmd_line.debug ) {
      printf("\nfirst part dynamic CNF");
    }
    if ( gnum_clauses != gnum_fixed_clauses ) {
      printf("\n\nat start dyn. extend gnum_clauses != gnum_fixed_clauses??\n\n");
      fflush(stdout);
      exit( 1 );
    }
  }
  if ( gcmd_line.T && gcmd_line.debug ) {
    printf("\n\n-----------------------adding clauses from %d",
	   gnum_clauses);
  }
  if ( firstpart ) {
    /* insert the t -> t+1 implications for all t on the path.
     *
     * the times here are t-(num_path+1) -> t-(num_path+1)+1 for t in:
     */
    for ( t = num_path + 1; t < MAX_PLAN_LENGTH-1; t++ ) {
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
	  if ( gcmd_line.T && gcmd_line.debug ) {
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
	      if ( gcmd_line.T && gcmd_line.debug ) {
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
	    if ( gcmd_line.T && gcmd_line.debug ) {
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
	  if ( gcmd_line.T && gcmd_line.debug ) {
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
	  if ( gcmd_line.T && gcmd_line.debug ) {
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
    }/* end path clauses loop */
  }/* end 1st part */

  /* in any case, get the appropriate (end)time
   */
  t = MAX_PLAN_LENGTH-1;
  time = gfixed_endtime + t - (num_path + 1);

  if ( !firstpart ) {
    if ( gcmd_line.T && gcmd_line.debug ) {
      printf("\nsecond part dynamic CNF");
    }
    
    /* now we do pretty much the same, except that the proved false info is
     * now given by   Npp, and proved completely unknown by Upp
     *
     * ACTUALLY IT SEEMS THAT IN THE LAST STEP WE'D ONLY NEED
     * THOSE IMPLICATIONS RELEVANT FOR THE RESPECTIVE LITERAL
     * TO BE CHECKED?????
     */
    t = MAX_PLAN_LENGTH-1;
    time = gfixed_endtime + t - (num_path + 1);
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
	if ( gcmd_line.T && gcmd_line.debug ) {
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
	    if ( gcmd_line.T && gcmd_line.debug ) {
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
	  if ( gcmd_line.T && gcmd_line.debug ) {
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
	if ( gcmd_line.T && gcmd_line.debug ) {
	  printf("\nunknown add eff clause, final"); print_clause( gnum_clauses-1 );
	}
      }
      /* for all non-false dels de:
       * -u1(t) vee .. -un(t) vee -de(t+1)
       * where u1 .. un are the unknown conds of the ef.
       */
      for ( j = 0; j < gef_conn[ef].num_D; j++ ) {
	if ( !gft_conn[gef_conn[ef].D[j]].CNF ) continue;
	if ( Npp[gef_conn[ef].D[j]] ) continue;
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
	if ( gcmd_line.T && gcmd_line.debug ) {
	  printf("\nunknown del eff clause, final"); print_clause( gnum_clauses-1 );
	}
      }
    }
    
    /* now, the noop - implications; first, the positives then the negatives
     * QUESTION: do we need the negatives for our derivations to be correct?
     */
    for ( ft = 0; ft < gnum_ft_conn; ft++ ) {
      if ( gcmd_line.T && gcmd_line.debug >= 3 ) {
	printf("\npos noops: ");
	print_ft_name(ft);
	printf(" Ft %d, Ut %d, Ftpp %d, Utpp %d, Npp %d", 
	       Ft[ft], Ut[ft], Ftpp[ft], Utpp[ft], Npp[ft]);
	fflush(stdout);
      }


      if ( !gft_conn[ft].CNF ) continue;
      if ( Ftpp[ft] ) continue;/* clause satisfied */
      if ( Npp[ft] ) {
	/* note: this check is necessary for validity of code below;
	 * *un*conditional, or satisfied, dels are not taken into account there.
	 */
	continue;
      }
      if ( !Ft[ft] && !Ut[ft] ) continue;/* clause satisfied */
      /* we deal with Ft || Ut -> Utpp;
       * clauses are all -ft(t) vee c1 .. cn vee ft(t+1)
       * where c1 .. cn are conds of unknown efs deleting ft
       */
      if ( gcmd_line.T && gcmd_line.debug >= 3 ) {
	printf("\npos noops: doing that");fflush(stdout);
      }
      insert_posnoop_hitting_set_clauses( ft, time, 
					  path[t+1],
					  Ft, Ut );
    }
    /* negative noops
     */
    for ( ft = 0; ft < gnum_ft_conn; ft++ ) {
      if ( gcmd_line.T && gcmd_line.debug >= 3 ) {
	printf("\nneg noops: ");
	print_ft_name(ft);
	printf(" Ft %d, Ut %d, Ftpp %d, Utpp %d, Npp %d", 
	       Ft[ft], Ut[ft], Ftpp[ft], Utpp[ft], Npp[ft]);
	fflush(stdout);
      }

      if ( !gft_conn[ft].CNF ) continue;
      if ( Npp[ft] ) continue;/* clause satisfied */
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
      if ( gcmd_line.T && gcmd_line.debug >= 3 ) {
	printf("\nneg noops: doing that");fflush(stdout);
      }
      insert_negnoop_hitting_set_clauses( ft, time, 
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
  }
  
  /* debugging: see what that looks like!
   */
  if ( gcmd_line.T && gcmd_line.debug >= 7 ) {
    if ( gcmd_line.T && gcmd_line.debug >= 8 ) {
      printf("\n\n----------------------------- to execution path\n");
      if ( gcmd_line.T && gcmd_line.debug >= 9 ) {
	printf("\n---------state:");
	print_state( *(path[num_path+1]) );
      }
      for ( t = num_path + 2; t <= MAX_PLAN_LENGTH; t++ ) {
	printf("\n-----------op ");
	print_op_name( path_op[t] );
	if ( gcmd_line.T && gcmd_line.debug >= 9 ) {
	  printf("\n----------state:");
	  print_state( *(path[t]) );
	}
      }
    }
    printf("\n\n----------------------------- I get the CNF\n");
    print_clauses();
  }
  
  if ( time == -1 ) {
    printf("\n\ntime == -1 in dynamic extend??\n\n");
    exit( 1 );
  }
  
  return time + 1;

}



/* july06: in this fn, we will never have probnodes.
 */
void print_clauses( void )

{

  int i, j;
  
  for ( i = 0; i < gnum_clauses; i++ ) {
    printf("\n");
    for ( j = 0; j < gclause_length[i]; j++ ) {
      if ( gclauses[i][j].time >= 0 ) {
	if ( gclauses[i][j].literal < 0 ) {
	  printf("-");
	  if ( (-1) * gclauses[i][j].literal - 1 >= gnum_ft_conn  ) {
	    /* july06: pef code!
	     */
	    printf("(pef %d)", (-1) * gclauses[i][j].literal - 1 - gnum_ft_conn);
	  } else {
	    print_ft_name( (-1) * gclauses[i][j].literal - 1 );
	  }
	} else {
	  if ( gclauses[i][j].literal - 1 >= gnum_ft_conn ) {
	    /* july06: pef code!
	     */
	    printf("(pef %d)", gclauses[i][j].literal - 1 - gnum_ft_conn);
	  } else {
	    print_ft_name( gclauses[i][j].literal - 1 );
	  }
	}
	printf("(%d) ", gclauses[i][j].time);
      } else {
	/* this is a code!
	 */
	if ( gclauses[i][j].literal < 0 ) {
	  printf("-");
	  if ( gcf[(-1) * gclauses[i][j].literal] >= gnum_ft_conn ) {
	    /* july06: pef code!
	     */
	    printf("(pef %d)", gcf[(-1) * gclauses[i][j].literal] - gnum_ft_conn);
	  } else {
	    print_ft_name( gcf[(-1) * gclauses[i][j].literal] );
	  }
	  printf("(%d) ", gct[(-1) * gclauses[i][j].literal] );
	} else {
	  if ( gcf[gclauses[i][j].literal] >= gnum_ft_conn ) {
	    /* july06: pef code!
	     */
	    printf("(pef %d)", gcf[gclauses[i][j].literal] - gnum_ft_conn);
	  } else {
	    print_ft_name( gcf[gclauses[i][j].literal] );
	  }
	  printf("(%d) ", gct[gclauses[i][j].literal] );
	}
      }
    }
  }

}



/* july06: in this fn, we will never have probnodes.
 */
void print_clause( int i )

{

  int j;
  
  printf("\n");
  for ( j = 0; j < gclause_length[i]; j++ ) {
    if ( gclauses[i][j].time >= 0 ) {
      if ( gclauses[i][j].literal < 0 ) {
	printf("-");
	if ( (-1) * gclauses[i][j].literal - 1 >= gnum_ft_conn  ) {
	  /* july06: pef code!
	   */
	  printf("(pef %d)", (-1) * gclauses[i][j].literal - 1 - gnum_ft_conn);
	} else {
	  print_ft_name( (-1) * gclauses[i][j].literal - 1 );
	}
      } else {
	if ( gclauses[i][j].literal - 1 >= gnum_ft_conn ) {
	  /* july06: pef code!
	   */
	  printf("(pef %d)", gclauses[i][j].literal - 1 - gnum_ft_conn);
	} else {
	  print_ft_name( gclauses[i][j].literal - 1 );
	}
      }
      printf("(%d) ", gclauses[i][j].time);
    } else {
      /* this is a code!
       */
      if ( gclauses[i][j].literal < 0 ) {
	printf("-");
	if ( gcf[(-1) * gclauses[i][j].literal] >= gnum_ft_conn ) {
	  /* july06: pef code!
	   */
	  printf("(pef %d)", gcf[(-1) * gclauses[i][j].literal] - gnum_ft_conn);
	} else {
	  print_ft_name( gcf[(-1) * gclauses[i][j].literal] );
	}
	printf("(%d) ", gct[(-1) * gclauses[i][j].literal] );
      } else {
	if ( gcf[gclauses[i][j].literal] >= gnum_ft_conn ) {
	  /* july06: pef code!
	   */
	  printf("(pef %d)", gcf[gclauses[i][j].literal] - gnum_ft_conn);
	} else {
	  print_ft_name( gcf[gclauses[i][j].literal] );
	}
	printf("(%d) ", gct[gclauses[i][j].literal] );
      }
    }
  }

}



void extend_fixed_clauses_base_encoding( int prev )

{

  int i, j, ft, time;
  Bool neg;
  
  /* july06: herein, "ft" might now also be a pef code.
   *         this does not at all affect the way to deal with it,
   *         however. (??!!!)
   */
  for ( i = prev; i < gnum_fixed_clauses; i++ ) {
    for ( j = 0; j < gclause_length[i]; j++ ) {
      time = gclauses[i][j].time;
      if ( gclauses[i][j].literal < 0 ) {
	neg = TRUE;
	ft = ((-1) * gclauses[i][j].literal) - 1;
      } else {
	neg = FALSE;
	ft = gclauses[i][j].literal - 1;
      }
      if ( gcodes[time][ft] == -1 ) {
	gnum_fixed_c++;/* must be non-zero */
	gcodes[time][ft] = gnum_fixed_c;
	if ( gnum_fixed_c == (1 + MAX_PLAN_LENGTH) * gmax_CNFU + 1) {
	  printf("\n\ntoo many codes? %d\n\n", gnum_fixed_c);
	  exit( 1 );
	}
	gcf[gnum_fixed_c] = ft;
	gct[gnum_fixed_c] = time;
      }
      gclauses[i][j].literal = gcodes[time][ft];
      gclauses[i][j].time = -1;/* for knowing (in print) that this is encoded */
      if ( neg ) {
	gclauses[i][j].literal *= (-1);
      }
    }
  }

  if ( gcmd_line.T && gcmd_line.debug  >= 4 ) {
    printf("\n\n--------------------------------fixed CNF encoding");
    printf("\n-----codes up to %d:", gnum_fixed_c);
    for ( i = 1; i <= gnum_fixed_c; i++ ) {
      if ( gcf[i] >= gnum_ft_conn ) {
	printf("\n(pef %d)", gcf[i] - gnum_ft_conn);
      } else {
	printf("\n"); print_ft_name( gcf[i] ); 
      }
      printf("(%d) ---> %d", gct[i], gcodes[gct[i]][gcf[i]]);
    }
    printf("\n-----clauses:\n");
    print_encoded_clauses();
  }

}



void extend_dynamic_clauses_base_encoding( int startclauses, int startcodes )

{

  int i, j, ft, time;
  Bool neg;
  
  /* july06: herein, "ft" might now also be a pef code.
   *         this does not at all affect the way to deal with it,
   *         however. (??!!!)
   */  
  gnum_c = startcodes;
  for ( i = startclauses; i < gnum_clauses; i++ ) {
    for ( j = 0; j < gclause_length[i]; j++ ) {
      time = gclauses[i][j].time;
      if ( gclauses[i][j].literal < 0 ) {
	neg = TRUE;
	ft = ((-1) * gclauses[i][j].literal) - 1;
      } else {
	neg = FALSE;
	ft = gclauses[i][j].literal - 1;
      }
//       if ( time < 0 || time > MAX_PLAN_LENGTH ||
// 	   ft < 0 || ft >= gnum_ft_conn ) {
// 	printf("\n\ntime %d, ft %d (of %d) in dyn clause encod??",
// 	       time, ft, gnum_ft_conn);fflush(stdout);
// 	exit( 1 );
//       }
      /* july06: only change is in the debug question.
       */
      if ( time < 0 || time > MAX_PLAN_LENGTH ||
	   ft < 0 || ft >= gnum_ft_conn + gnum_pef_conn ) {
	printf("\n\ntime %d, ft/pef %d (of %d) in dyn clause encod??",
	       time, ft, gnum_ft_conn + gnum_pef_conn);fflush(stdout);
	exit( 1 );
      }
      if ( gcodes[time][ft] == -1 ) {
	gnum_c++;/* must be non-zero */
	gcodes[time][ft] = gnum_c;
	if ( gnum_c == (1 + MAX_PLAN_LENGTH) * gmax_CNFU ) {
	  printf("\n\ntoo many codes? %d\n\n", gnum_c);
	  exit( 1 );
	}
	gcf[gnum_c] = ft;
	gct[gnum_c] = time; 
      }
      gclauses[i][j].literal = gcodes[time][ft];
      gclauses[i][j].time = -1;/* for knowing (in print) that this is encoded */
      if ( neg ) {
	gclauses[i][j].literal *= (-1);
      }
    }
  }
  
  if ( gcmd_line.T && gcmd_line.debug  >= 4 ) {
    printf("\n\n--------------------------------CNF encoding");
    printf("\n-----codes up to: %d", gnum_c);
    for ( i = 1; i <= gnum_c; i++ ) {
      if ( gcf[i] >= gnum_ft_conn ) {
	printf("\n(pef %d)", gcf[i] - gnum_ft_conn);
      } else {
	printf("\n"); print_ft_name( gcf[i] ); 
      }
      printf("(%d) ---> %d", gct[i], gcodes[gct[i]][gcf[i]]);
    }
    printf("\n-----clauses:\n");
    print_encoded_clauses();
  }

}



void print_encoded_clauses( void )

{

    int i, j;

    printf("\nc encoded conformant search state transition base clauses");
    printf("\np cnf %d %d\n", gnum_c, gnum_clauses);
    for ( i = 0; i < gnum_clauses; i++ ) {
	printf("%d: ", i);
	for ( j = 0; j < gclause_length[i]; j++ ) {
	    printf("%d", gclauses[i][j].literal);
	    if ( j < gclause_length[i] - 1 ) {
		printf(" ");
	    }
	}
	printf("\n");
    }

}

























/***********************************************************
 * WE ALL LIVE IN A YELLOW SUBROUTINE:                     * 
 *   HERE COMES A SAT SOLVER                               *
 ***********************************************************/






























/* top level control fn for (extremely) naive DP implementation...
 */

Bool dp_CNF( void )

{

  int i, j, v, sum;
  Bool sat;

  gdp_calls++;

  if ( gnum_decision_stack == 0 ) {
    printf("\nno decision stack at entering DP??\n\n"); 
    exit( 1 );
  }

  if ( gcmd_line.debug ) {
    for ( i = 0; i < gmax_literals + 1; i++ ) {
      for ( j = gnum_clauses - 1; j >= 0; j-- ) {
	if ( gclause_length[j] == i ) gsum_k_clauses[i]++;
	if ( gclause_length[j] > gmax_literals ) {
	  printf("???\n\n");
	}
      }
    }
    gsum_clauses += gnum_clauses;

    sum = -1;
    for ( i = gnum_clauses - 1; i >= 0; i-- ) {
      for ( j = 0; j < gclause_length[i]; j++ ) {
	v = (gclauses[i][j].literal > 0) ? 
	  gclauses[i][j].literal : (-1) * gclauses[i][j].literal;
	if ( sum == -1 || v > sum ) {
	  sum = v;
	}
      }
    }
    if ( sum != gnum_c ) {
      printf("\ngnum_clauses: %d, counted gnum_c: %d, old: %d", gnum_clauses, sum, gnum_c);
    }
  }



  /* july06: hook Minisat up here! rest deleted.
   * 
   *         this automatically hooks the SAT solver in for all DP
   *         calls except those for full dominated states checking, and
   *         those for RPG h=1 implication against ini state formula.
   */
  sat = internal_minisat_CNF();

  /* not strictly necessary, as full (..) initialisation done above 
   * before WMC calls... looks cleaner this way...
   */
  gnum_decision_stack = 0;

  return sat;

}



/* july06
 */
Bool internal_minisat_CNF( void )

{

  int i, j, var;
  int result;

  Solver S;
  vec<Lit> lits;

  times( &start );
  for ( i = 0; i < gnum_c; i++ ) {
    S.newVar();
  }

  i = 0;
  while ( i < gnum_decision_stack ) {
    lits.clear();
    var = abs(gdecision_stack[i])-1;
    lits.push( (gdecision_stack[i] > 0) ? Lit(var) : ~Lit(var) );
    S.addClause(lits);
    i++;
  }
  for ( i = 0; i < gnum_clauses; i++ ) {
    lits.clear();
    for ( j = 0; j < gclause_length[i]; j++ ) {
      var = abs(gclauses[i][j].literal)-1;
      lits.push( (gclauses[i][j].literal > 0) ? Lit(var) : ~Lit(var) );
    }
    S.addClause(lits);
  }
  times( &end );
  TIME( gDP_parsetime );
  gDP_time = 0;
  times( &start );

  if (!S.okay()) {
    /* trivial instance
     */
    times( &end );
    TIME( gDP_time );
    return FALSE;
  }

  S.solve();

  result = S.okay();

  times( &end );
  TIME( gDP_time );
  return result;

}






















/***********************************************************
 * THINGS KEEP GETTING BETTER:                             * 
 *   HERE COMES A WEIGHTED MODEL COUNTER                   *
 ***********************************************************/
































/* this here returns the weight in terms of fraction a/b
 */
void wmc_CNF( double *a, double *b )

{

  int i, j, v, sum;

  if ( gcmd_line.debug ) {
    for ( i = 0; i < gmax_literals + 1; i++ ) {
      for ( j = gnum_clauses - 1; j >= 0; j-- ) {
	if ( gclause_length[j] == i ) gsum_k_clauses[i]++;
	if ( gclause_length[j] > gmax_literals ) {
	  printf("???\n\n");
	}
      }
    }
    gsum_clauses += gnum_clauses;

    sum = -1;
    for ( i = gnum_clauses - 1; i >= 0; i-- ) {
      for ( j = 0; j < gclause_length[i]; j++ ) {
	v = (gclauses[i][j].literal > 0) ? 
	  gclauses[i][j].literal : (-1) * gclauses[i][j].literal;
	if ( sum == -1 || v > sum ) {
	  sum = v;
	}
      }
    }
    if ( sum != gnum_c ) {
      printf("\ngnum_clauses: %d, counted gnum_c: %d, old: %d", gnum_clauses, sum, gnum_c);
    }
  }

  /* july06: just use external cachet.
   */
  external_cachet_CNF( a, b );
  /* not strictly necessary, as full (..) initialisation done above 
   * before WMC calls... looks cleaner this way...
   */
  gnum_decision_stack = 0;

}



/* divide a and b by their greatest common divisor.
 */
void simplify_ratio( double *a, double *b )

{

  double m = *a, n = *b, r;

  if ( gcmd_line.T && gcmd_line.debug >= 7 ) {
    printf("\nentry simplify ratio: a %f, b %f", *a, *b); fflush(stdout);
  }

  (*a) = (*a) / (*b);
  (*b) = 1;
  return;

  if ( *a < 0 || *b < 0 ) {
    printf("\nweight computation overflow before simplification! aborting.\n\n");
    exit( 1 );
  }

  if ( (*a) == 0 ) {
    (*b) = 1;
    return;
  }

  if ( *a == 1 || *b ==1 ) {
    /* nothing to simplify
     */
    return;
  }

  /* Euclidian algorithm
   */
  while ( TRUE ) {
    if ( m < n ) {
      r = m;
      m = n;
      n = r;
    }
    r = m - n;
    m = n;
    n = r;
    if ( r == 0 ) {
      break;
    }
  }

  if ( gcmd_line.debug ) {
    if ( (*a)/m != ((long) ((*a)/m)) ) {
      printf("\n\nGGT no int division?\n\n");
      exit( 1 );
    }
    if ( (*b)/m != ((long) ((*b)/m)) ) {
      printf("\n\nGGT no int division?\n\n");
      exit( 1 );
    }
  }

  (*a) /= m;
  (*b) /= m;

  if ( *a < 0 || *b < 0 ) {
    printf("\nweight computation overflow after simplification! aborting.\n\n");
    exit( 1 );
  }

  if ( gcmd_line.T && gcmd_line.debug >= 7 ) {
    printf("\nexit simplify ratio: a %f, b %f", *a, *b); fflush(stdout);
  }

}



/* normalize the ratios to smallest common multiple?
 * (zu deutsch: KGV)
 */
void synchronize_ratios( double *a1, double *b1, double *a2, double *b2 )

{

  double m = *b1, n = *b2, r;
  
  if ( *a1 < 0 || *b1 < 0 || *a2 < 0 || *b2 < 0 ) {
    printf("\nweight computation overflow before synchronization! aborting.\n\n");
    exit( 1 );
  }

  (*a1) = (*a1) / (*b1);
  (*b1) = 1;
  (*a2) = (*a2) / (*b2);
  (*b2) = 1;
  return;

  if ( gcmd_line.T && gcmd_line.debug >= 7 ) {
    printf("\nentry synchronize ratios: (1) %f/%f, (2) %f/%f", 
	   *a1, *b1, *a2, *b2); fflush(stdout);
  }

  if ( *b1 == *b2 ) {
    /* nothing to do.
     */
    return;
  }

  /* Euclidian algorithm
   */
  while ( TRUE ) {
    if ( m < n ) {
      r = m;
      m = n;
      n = r;
    }
    r = m - n;
    m = n;
    n = r;
    if ( r == 0 ) {
      break;
    }
  }

  /* m is now the greatest common divisor of b1 and b2.
   * compute n, from that, as KGV.
   */
  n = (*b1) / m;
  n *= (*b2);


  (*a1) = (*a1) * (n/(*b1));
  (*b1) = n;

  (*a2) = (*a2) * (n/(*b2));
  (*b2) = n;

  if ( *a1 < 0 || *b1 < 0 || *a2 < 0 || *b2 < 0 ) {
    printf("\nweight computation overflow after synchronization! aborting.\n\n");
    exit( 1 );
  }

  if ( gcmd_line.T && gcmd_line.debug >= 7 ) {
    printf("\nsynchronize ratios: (1) %f/%f, (2) %f/%f", 
	   *a1, *b1, *a2, *b2); fflush(stdout);
  }

}



/* what fraction
 * is a/b of BN fraction?
 *
 * --> used to answer enough P? questions.
 */
double get_gprob( double a, double b )

{

  double BNa = gBNa, BNb = gBNb, aa = a, bb = b;
  float res;

  /* try to avoid overflows as much as possible.
   */
  if ( BNa != 1 ) {
    simplify_ratio( &aa, &BNa );
  }
  if ( BNb != 1 ) {
    simplify_ratio( &BNb, &bb );
  }

  res = (aa / bb);
  res *= (BNb / BNa);

  return res;

}



void external_cachet_CNF( double *a, double *b )

{

  FILE *A;

  int satresult;
  double result, rtime;

  int i, j;

  if ( gcmd_line.debug && gcmd_line.T ) {
    printf("\nForECachet: p %d %d (stack: %d)", gnum_c, 
	   gnum_clauses+gnum_decision_stack, gnum_decision_stack);

    for ( i = 1; i <= gnum_c; i++ ) {
      printf("\nForECachet: code %d = ", i);
      /* july06: distinguish ft codes/pef codes/chance node codes.
       */
      if ( gcf[i] == -1971 ) {
	printf("(chance node)");
      } else {
	if ( gcf[i] < gnum_ft_conn ) {
	  print_ft_name(gcf[i]);
	} else {
	  printf("(pef %d)", gcf[i]-gnum_ft_conn);
	}
	printf(" at %d", gct[i]);
      }
      if ( gcf[i] == -1971 ) {
	printf(" w %lf", gcweight[i]);
      } else {
	if ( gcf[i] < gnum_ft_conn ) {
	  if ( gct[i] != 0 ) {
	    printf(" w -1");
	    continue;
	  }
	  printf(" w %lf", ginitial_ft_weight[gcf[i]]);
	} else {
	  if ( gpef_conn_weight_a[gcf[i]-gnum_ft_conn] > -1 ) {
	    printf(" w %lf", gpef_conn_weight_a[gcf[i]-gnum_ft_conn]/gpef_conn_weight_b[gcf[i]-gnum_ft_conn]);
	  } else {
	    printf(" w -1");
	  }
	}
      }
    }
    
    for ( i = 0; i < gnum_decision_stack; i++ ) {
      printf("\nForECachet: stack %d = %d", i, gdecision_stack[i]);
    }
    
    for ( i = 0; i < gnum_clauses; i++ ) {
      printf("\nForECachet: clause %d = ", i);
      for ( j = 0; j < gclause_length[i]; j++ ) {
	printf("%d ", gclauses[i][j].literal);
      }
    }

    printf("\nForECachet: DONE!\n\n");
  }

  fflush(stdout);

  /* don't count time for file creation!
   */
  times( &end );
  TIME( gwmc_time );

  /* july06: count it, but separately.
   */
  times( &start );
  print_wmc_CNF();
  times( &end );
  TIME( gWMC_filetime );

  if ( gcmd_line.debug && gcmd_line.T ) {
    system( "~/CACHET/cachet CNF -c 100000" );
  } else {
    system( "~/CACHET/cachet CNF -q -c 100000" );
  }
  if ( (A = fopen("A","r")) == NULL ) {
    printf("\n\ncan't open Catchet's answer file\n\n");
    exit( 1 );
  }
  fscanf(A,"%d %lf %lf\n", &satresult, &result, &rtime);
  fclose( A );
  system( "rm CNF" );
  system( "rm A" );

  times( &start );

  if ( gcmd_line.debug && gcmd_line.T ) {
    printf("\nCachet: satresult ");
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
    printf("\n\ntime out in Cachet\n\n");
    exit( 1 );
    break;
  case MEM_OUT:
    printf("\n\nmemory out in Cachet\n\n");
    exit( 1 );
    break;
  default:
    printf("\n\nunknown Cachet SAT outcome");
    exit( 1 );
  }

  *a = result;
  *b = 1;

  gwmc_time += rtime;

}



void print_wmc_CNF( void )

{

  FILE *CNF;
  
  int i, j, pef;
  
  if ( (CNF = fopen("CNF","w")) == NULL ) {
    printf("\n\ncan not open CNF file.\n\n");
    exit(1);
  }
  
  fprintf(CNF, "c encoded conformant search state transition base clauses\n");
  fprintf(CNF, "p cnf %d %d\n", gnum_c, gnum_clauses+gnum_decision_stack);
  if ( gcmd_line.debug && gcmd_line.T ) {
    printf("\n\nCachet: c encoded conformant search state transition base clauses\n");
    printf("Cachet: p cnf %d %d\n", gnum_c, gnum_clauses+gnum_decision_stack);
  }

  /* communicate the weights.
   */
  for ( i = 1; i <= gnum_c; i++ ) {
    /* july06: distinguish ft codes/pef codes/chance node codes.
     */
    if ( gcf[i] == -1971 ) {
      fprintf(CNF, "w %d %lf\n", i, gcweight[i]);
      if ( gcmd_line.debug && gcmd_line.T ) {
	printf("Cachet: w %d %lf\n", i, gcweight[i]);
	fflush(stdout);
      }
    } else {
      if ( gcf[i] < gnum_ft_conn ) {
	if ( gct[i] != 0 ) {
	  /* except at ini, everything's unweighted anyway
	   */
	  fprintf(CNF, "w %d -1\n", i);
	  if ( gcmd_line.debug && gcmd_line.T ) {
	    printf("Cachet: w %d -1\n", i);
	  }
	  continue;
	}
	/* may not be std, when called for h=2 from RPG
	 */
	fprintf(CNF, "w %d %lf\n", i, ginitial_ft_weight[gcf[i]]);
	if ( gcmd_line.debug && gcmd_line.T ) {
	  printf("Cachet: w %d %lf\n", i, ginitial_ft_weight[gcf[i]]);
	}
      } else {
	pef = gcf[i] - gnum_ft_conn;
	if ( gpef_conn_weight_a[pef] > -1 ) {
	  fprintf(CNF, "w %d %lf\n", i, gpef_conn_weight_a[pef]/gpef_conn_weight_b[pef]);
	  if ( gcmd_line.debug && gcmd_line.T ) {
	    printf("Cachet: w %d %lf\n", i, gpef_conn_weight_a[pef]/gpef_conn_weight_b[pef]);
	  }
	} else {
	  fprintf(CNF, "w %d -1\n", i);
	  if ( gcmd_line.debug && gcmd_line.T ) {
	    printf("Cachet: w %d -1\n", i);
	  }
	}
      }
    }
  }


  i = 0;
  while ( i < gnum_decision_stack ) {
    fprintf(CNF, "%d 0\n", gdecision_stack[i]);
    i++;
  }
  
  for ( i = 0; i < gnum_clauses; i++ ) {
    for ( j = 0; j < gclause_length[i]; j++ ) {
      fprintf(CNF, "%d", gclauses[i][j].literal);
      if ( j < gclause_length[i] - 1 ) {
	fprintf(CNF, " ");
      }
    }
    fprintf(CNF, " 0\n");
  }
  
  fclose( CNF );

}

