
/*********************************************************************
 * (C) Copyright 2002 Albert Ludwigs University Freiburg
 *     Institute of Computer Science
 *
 * All rights reserved. Use of this software is permitted for 
 * non-commercial research purposes, and it may be copied only 
 * for that use.  All copies must include this copyright message.
 * This software is made available AS IS, and neither the authors
 * nor the  Albert Ludwigs University Freiburg make any warranty
 * about the software or its performance. 
 *********************************************************************/





/*********************************************************************
 * File: graph.c
 * Description: (initial build of) the planning graph
 *
 * Author: Joerg Hoffmann 2002
 *
 *********************************************************************/ 















#include "bb.h"

#include "output.h"
#include "memory.h"

#include "instantiateI.h"
#include "instantiateII.h"

#include "graph.h"































Bool build_graph( int *min_time )

{

  int time = 0, i, ft;
  IntList *tmp;
  Bool reached_goals = FALSE, first = TRUE;

  /* put all ops into a list so that we can directly access
   * those that are not yet in the graph
   */
  for ( i = 0; i < gnum_op_conn; i++ ) {
    tmp = new_IntList( i );
    if ( gout_ops ) {
      gout_ops->prev = tmp;
    }
    tmp->next = gout_ops;
    gout_ops = tmp;
  }

  /* for ini fts, say that they appear, put them into graph fts list,
   * insert their bits
   */
  for ( i = 0; i < ginitial_state.num_F; i++ ) {
    ft = ginitial_state.F[i];
    gin_ft_count++;
    gft_conn[ft].first_appearance = 0;
    gft_conn[ft].info_at[0] = new_FtLevelInfo();
    tmp = new_IntList( ft );
    tmp->next = gin_fts;
    gin_fts = tmp;
  }
  gin_fts_at[0] = gin_fts;

  if ( gcmd_line.display_info ) {
    printf("\ntime: %3d, %5d facts and %7d exclusive pairs",
	     time, gin_ft_count, gin_ft_exclusion_count);
    fflush(stdout);
  }

  for( ; time < *min_time; time++ ) {
    if ( first ) {
      reached_goals = are_there_non_exclusive( time, gbit_goal_state,
					       ggoal_state.F, ggoal_state.num_F );
      if ( reached_goals ) {
	if ( gcmd_line.display_info ) {
	  printf("\ngoals first reachable in %d time steps\n\n", time);
          first = FALSE;
	  fflush(stdout);
	}
      }
    }
    if ( gsame_as_prev_flag ) break;
    build_graph_evolution_step();
  }
  
  for( ; time < MAX_GRAPH; time++ ) {
    reached_goals = are_there_non_exclusive( time, gbit_goal_state,
					     ggoal_state.F, ggoal_state.num_F );
    if ( reached_goals ) {
      if ( gcmd_line.display_info && first ) {
        printf("\ngoals first reachable in %d time steps\n\n", time);
	fflush(stdout);
      }
      break;
    }
    if ( gsame_as_prev_flag ) break;
    build_graph_evolution_step();
  }

  *min_time = time;
  return reached_goals;

}



void build_graph_evolution_step( void )

{

  static int time = 0;
  int in_ft_count = gin_ft_count, in_ft_exclusion_count = gin_ft_exclusion_count;
  IntList *il, *tmp, *il2;

  if ( time > MAX_GRAPH ) {
    printf("\nEXIT: max graph size exceeded!\n\n");
    exit( 1 );
  }

  if ( gsame_as_prev_flag ) {
    copy_graph_layer( time );
    if ( gcmd_line.display_info ) {
      printf("\n           %5d ops   and %7d exclusive pairs", gin_op_count, gin_op_exclusion_count);
      printf("\ntime: %3d, %5d facts and %7d exclusive pairs", time+1, gin_ft_count, gin_ft_exclusion_count);
      fflush(stdout);
    }
    time++;
    return;
  }

  /* extend op level infos (for already in ops)
   */
  for ( il = gin_ops; il; il = il->next ) {
    gop_conn[il->i1].info_at[time] = new_OpLevelInfo();
  }
  /* likewise for facts
   */
  for ( il = gin_fts; il; il = il->next ) {
    gft_conn[il->i1].info_at[time+1] = new_FtLevelInfo();
    /* we also need to transfer the adders we already have to the new level.
     *
     * attention: like this, order is kept constant (seems cleaner...)
     * (in particular, the NOOP remains first in list)
     */
    for ( il2 = gft_conn[il->i1].info_at[time]->A; il2; il2 = il2->next ) {
      if ( !gft_conn[il->i1].info_at[time+1]->A ) {
	gft_conn[il->i1].info_at[time+1]->A = new_IntList( il2->i1 );
	gft_conn[il->i1].info_at[time+1]->end_A = gft_conn[il->i1].info_at[time+1]->A;
/*  	gft_conn[il->i1].info_at[time+1]->num_A = 1; */
      } else {
	gft_conn[il->i1].info_at[time+1]->end_A->next = new_IntList( il2->i1 );
	gft_conn[il->i1].info_at[time+1]->end_A = gft_conn[il->i1].info_at[time+1]->end_A->next;
/*  	gft_conn[il->i1].info_at[time+1]->num_A++; */
      }
    }
    /* ... and the same for the ops we are precond of, if we've been there before
     */
    if ( gft_conn[il->i1].first_appearance >= time ) continue;
    for ( il2 = gft_conn[il->i1].info_at[time-1]->P; il2; il2 = il2->next ) {
      tmp = new_IntList( il2->i1 );
      tmp->next = gft_conn[il->i1].info_at[time]->P;
      gft_conn[il->i1].info_at[time]->P = tmp;
    }
  }

  /* update the pointers as to which ops/fts we had already previously
   */
  gin_prev_ops = gin_ops;
  gin_prev_fts = gin_fts;

  /* now try to apply all ops that are still out --- as given by resp. list
   * --- and remove them from the list, if they are applicable.
   *
   * side effects are that the new ops have their uid blocks and masks set,
   * and that the new facts are declared (in particular, gin_ft_count
   * is incremented in that case), and that the resp. new add *edges* in the ft level 
   * infos are inserted --- not the resp. bits; these are updated below,
   * after allocating bit strings of appropriate size.
   */
  il = gout_ops;
  while ( il ) {
    if ( apply_operator( time, il->i1 ) ) {
      gnum_ops++;
      if ( gop_conn[il->i1].uid_block > gmax_block ) {
	gmax_block = gop_conn[il->i1].uid_block;
      }
      if ( il->prev ) {
	il->prev->next = il->next;
      } else {
	gout_ops = il->next;
      }
      if ( il->next ) {
	il->next->prev = il->prev;
      }
      tmp = il;
      il = il->next;
      free( tmp );
    } else {
      il = il->next;
    }
  }

  /* for access later, remember what the ops n fts are at this point.
   */
  gin_ops_at[time] = gin_ops;
  gin_fts_at[time+1] = gin_fts;

  /* now determine op string length, allocate the op bit strings, 
   * and insert the bit_A information in the ft level infos!
   *
   * IMPLEMENT MORE EFFICIENTLY! if 1st action happens to be last in opconn,
   * then we got all the bitstrings here already!
   */
  gnum_op_bit_at[time] = gmax_block + 1;

  for ( il = gin_ops; il; il = il->next ) {
    gop_conn[il->i1].info_at[time]->bit_exclusives = 
      new_BitVector( gnum_op_bit_at[time] );
  }
  /* allocate adders and adders-excl space, setup adders simply by
   * setting the respective bits in the list. 
   *
   * other option would be, see dir 2nd, to copy adders over for
   * previously present facts.
   * favor this option cause it's conceptually simpler and probably
   * doesn't make much difference anyway.
   */
  for ( il = gin_fts; il; il = il->next ) {
    gft_conn[il->i1].info_at[time+1]->bit_A = 
      new_BitVector( gnum_op_bit_at[time] );
    for ( il2 = gft_conn[il->i1].info_at[time+1]->A; il2; il2 = il2->next ) {
      gft_conn[il->i1].info_at[time+1]->bit_A[gop_conn[il2->i1].uid_block] |=
	gop_conn[il2->i1].uid_mask;
    }
    gft_conn[il->i1].info_at[time+1]->bit_A_exclusives = 
      new_BitVector( gnum_op_bit_at[time] );
  }

  /* now do the mutex reasoning for the new graph layer
   */
  find_mutex_ops( time );
  /* DIFFERENCE TO IPP where it's time + 1;
   * supposed to improve readability of the code, by sticking to
   * opslevel time, ft level time+1 convention.
   */
  find_mutex_fts( time );

  if ( in_ft_count == gin_ft_count && in_ft_exclusion_count == gin_ft_exclusion_count ) {
    gsame_as_prev_flag = TRUE;
    if ( gcmd_line.display_info ) {
      printf("\ngraph has leveled off at time step %d\n\n", time+1);
      fflush(stdout);
    }
    gfirst_full_time = time;
  }

  if ( gcmd_line.display_info ) {
    printf("\n           %5d ops   and %7d exclusive pairs", gin_op_count, gin_op_exclusion_count);
    printf("\ntime: %3d, %5d facts and %7d exclusive pairs", time+1, gin_ft_count, gin_ft_exclusion_count);
    fflush(stdout);
  }

  time++;

}



Bool apply_operator( int time, int op )

{

  static Bool fc = TRUE;
  static BitVector *bit_P_exclusives;
  int i, j, ft;
  IntList *tmp;

  if ( fc ) {
    bit_P_exclusives = new_BitVector( gnum_ft_bit );
    fc = FALSE;
  } else {
    for ( i = 0; i < gnum_ft_bit; i++ ){
      bit_P_exclusives[i] = 0;
    }
  }

  /* check if precs are there, and at same time get the 
   * vector repres. of their exclusives
   */
  if ( !get_them_non_exclusive( time, gop_conn[op].bit_P,
				gop_conn[op].P, gop_conn[op].num_P,
				&bit_P_exclusives ) ) {
    return FALSE;
  }

  /* "insert" that op at this point into the graph
   */
  gop_conn[op].uid_block = ( int ) gin_op_count / gcword_size;
  gop_conn[op].uid_mask = 1 << ( gin_op_count % gcword_size );  
  gin_op_count++;
  gop_conn[op].first_appearance = time;
  gop_conn[op].info_at[time] = new_OpLevelInfo();
  for ( i = 0; i < time; i++ ) {
    /* probably unnecessary... just to be nice and clean
     */
    gop_conn[op].info_at[i] = NULL;
  }
  tmp = new_IntList( op );
  tmp->next = gin_ops;
  gin_ops = tmp;
  /* some more semantical stuff here:
   *
   * insert the prec. exclusives in the resp. level info
   */
  for ( i = 0; i < gnum_ft_bit; i++ ){
    gop_conn[op].info_at[time]->bit_P_exclusives[i] = bit_P_exclusives[i];
  }
  /* insert the op into the list of ops that the Ps are P of at this point
   */
  for ( i = 0; i < gop_conn[op].num_P; i++ ) {
    ft = gop_conn[op].P[i];
    tmp = new_IntList( op );
    tmp->next = gft_conn[ft].info_at[time]->P;
    gft_conn[ft].info_at[time]->P = tmp;
  }
  /* now declare the existence of the new incoming add effects
   */
  for ( i = 0; i < gop_conn[op].num_A; i++ ) {
    ft = gop_conn[op].A[i];
    if ( gft_conn[ft].first_appearance == -1 ) {
      /* new fact!
       */
      gin_ft_count++;
      gft_conn[ft].first_appearance = time + 1;
      gft_conn[ft].info_at[time+1] = new_FtLevelInfo();
      for ( j = 0; j < time + 1; j++ ) {
	/* again, probably unnecessary... just to be nice and clean
	 */
	gft_conn[ft].info_at[j] = NULL;
      }
      tmp = new_IntList( ft );
      tmp->next = gin_fts;
      gin_fts = tmp;
    }
    /* now insert the new (possibly first) adder!
     * (remember that for alreay existing fts the previous adders have already
     * been inserted above when starting graph evolution step)
     * 
     * if adder is "real", insert at end of list, if it is the NOOP, then insert
     * it at start --> the NOOPs are directly accessible as the first list 
     * element.
     */
    if ( gft_conn[ft].info_at[time+1]->A ) {
      if ( gop_conn[op].noop_for != -1 ) {
	tmp = new_IntList( op );
	tmp->next = gft_conn[ft].info_at[time+1]->A;
	gft_conn[ft].info_at[time+1]->A = tmp;
/*  	gft_conn[ft].info_at[time+1]->num_A++; */
      } else {
	gft_conn[ft].info_at[time+1]->end_A->next = new_IntList( op );
	gft_conn[ft].info_at[time+1]->end_A = gft_conn[ft].info_at[time+1]->end_A->next;
/*  	gft_conn[ft].info_at[time+1]->num_A++; */
      }
    } else {
      gft_conn[ft].info_at[time+1]->A = new_IntList( op );
      gft_conn[ft].info_at[time+1]->end_A = gft_conn[ft].info_at[time+1]->A;
/*        gft_conn[ft].info_at[time+1]->num_A = 1; */
    }
  }

  return TRUE;

}



Bool are_there_non_exclusive( int time, BitVector *bits, int *F, int num_F )

{

  BitVector *a;
  int i, j;

  for ( i = 0; i < num_F; i++ ) {
    if ( gcmd_line.debug == 1 ) {
      printf("\nchecking appearance of goal fact at %d: ", time);
      print_ft_name( F[i] );
    }
    if ( gft_conn[F[i]].first_appearance == -1 ||
	 gft_conn[F[i]].first_appearance > time ) {
      return FALSE;
    }
  }

  /* geht das EFFIZIENTER durch zusammen OR-en der exclusives??
   */
  for ( i = 0; i < num_F; i++ ) {
    a = gft_conn[F[i]].info_at[time]->bit_exclusives;
    for ( j = 0; j < gnum_ft_bit; j++ ) {
      if ( bits[j] & a[j] ) {
	return FALSE;
      }
    }
  }
    
  return TRUE;

}



Bool get_them_non_exclusive( int time, BitVector *bits, int *F, int num_F, BitVector **bit_P_exclusives )

{

  int i, j;

  for ( i = 0; i < num_F; i++ ) {
    if ( gcmd_line.debug == 2 ) {
      printf("\nchecking appearance of prec fact at %d: ", time);
      print_ft_name( F[i] );
    }
    if ( gft_conn[F[i]].first_appearance == -1 ||
	 gft_conn[F[i]].first_appearance > time ) {
      return FALSE;
    }
  }

  /* determine the vector of all facts that the elements of F are exclusive of
   * --> this will be stored in the op's level specific info (prec exclusives)
   * 
   * if one of these is contained in F, then fail.
   */
  for ( i = 0; i < num_F; i++ ) {
    for ( j = 0; j < gnum_ft_bit; j++ ) {
      (*bit_P_exclusives)[j] |= gft_conn[F[i]].info_at[time]->bit_exclusives[j];
      if ( bits[j] & (*bit_P_exclusives)[j] ) {
	return FALSE;
      }
    }
  }
    
  return TRUE;

}



/* we must build the graph beyond the fixpoint as the 
 * constraint system is dynamic beyond.
 */
void copy_graph_layer( int time )

{

  IntList *il, *tmp, *il2;
  int i;

  gin_ops_at[time] = gin_ops_at[time-1];
  gin_fts_at[time+1] = gin_fts_at[time];
  gnum_op_bit_at[time] = gnum_op_bit_at[time-1];

  for ( il = gin_ops; il; il = il->next ) {
    gop_conn[il->i1].info_at[time] = new_OpLevelInfo();
    gop_conn[il->i1].info_at[time]->bit_exclusives = new_BitVector( gnum_op_bit_at[time] );
    for ( i = 0; i < gnum_op_bit_at[time]; i++ ) {
      gop_conn[il->i1].info_at[time]->bit_exclusives[i] = 
	gop_conn[il->i1].info_at[time-1]->bit_exclusives[i];
    }
    /* P excl vec already allocated
     */
    for ( i = 0; i < gnum_ft_bit; i++ ) {
      gop_conn[il->i1].info_at[time]->bit_P_exclusives[i] = 
	gop_conn[il->i1].info_at[time-1]->bit_P_exclusives[i];
    }
  }
  /* likewise for facts
   */
  for ( il = gin_fts; il; il = il->next ) {
    gft_conn[il->i1].info_at[time+1] = new_FtLevelInfo();
    gft_conn[il->i1].info_at[time+1]->bit_A = new_BitVector( gnum_op_bit_at[time] );
    for ( i = 0; i < gnum_op_bit_at[time]; i++ ) {
      gft_conn[il->i1].info_at[time+1]->bit_A[i] = 
	gft_conn[il->i1].info_at[time]->bit_A[i];
    }
    gft_conn[il->i1].info_at[time+1]->bit_A_exclusives = new_BitVector( gnum_op_bit_at[time] );
    for ( i = 0; i < gnum_op_bit_at[time]; i++ ) {
      gft_conn[il->i1].info_at[time+1]->bit_A_exclusives[i] = 
	gft_conn[il->i1].info_at[time]->bit_A_exclusives[i];
    }
    for ( i = 0; i < gnum_ft_bit; i++ ) {
      gft_conn[il->i1].info_at[time+1]->bit_exclusives[i] = 
	gft_conn[il->i1].info_at[time]->bit_exclusives[i];
    }
    /* we also need to transfer the adders
     *
     * attention: like this, order is REVERSED (does it matter at all?)
     */
    for ( il2 = gft_conn[il->i1].info_at[time]->A; il2; il2 = il2->next ) {
      if ( !gft_conn[il->i1].info_at[time+1]->A ) {
	gft_conn[il->i1].info_at[time+1]->A = new_IntList( il2->i1 );
	gft_conn[il->i1].info_at[time+1]->end_A = gft_conn[il->i1].info_at[time+1]->A;
	gft_conn[il->i1].info_at[time+1]->num_A = 1;
      } else {
	gft_conn[il->i1].info_at[time+1]->end_A->next = new_IntList( il2->i1 );
	gft_conn[il->i1].info_at[time+1]->end_A = gft_conn[il->i1].info_at[time+1]->end_A->next;
	gft_conn[il->i1].info_at[time+1]->num_A++;
      }
    }
    /* ... and the same for the ops we are precond of
     *
     * note: ft has appeared at least 2 steps before
     */
    for ( il2 = gft_conn[il->i1].info_at[time-1]->P; il2; il2 = il2->next ) {
      tmp = new_IntList( il2->i1 );
      tmp->next = gft_conn[il->i1].info_at[time]->P;
      gft_conn[il->i1].info_at[time]->P = tmp;
    }
  }

}











































/*******************************************************
 *         EXCLUSIONS                         **********
 *******************************************************/






















/* ops
 */


















void find_mutex_ops( int time )

{

  IntList *il, *il2;
  IntPair *ip, *tmp;
  int i, j, ft;

  /* first, if not in initial state,
   * copy over all exclusions that we had before
   * (between ops already there, por supuesto)
   *
   * also, (re-)compute te new bit_P_exlusive vectors, i.e. the union of the
   * exclusives of all preconds; this seems difficult to compute in a
   * more efficient copy-and-update style (??!); in IPP it is done "on demand"
   * in the facts-are-exclusive fn which is the only point where the info is
   * needed; this here is somewhat cleaner; to be thought about later on...
   */
  if ( time > 0 ) {
    for ( il = gin_prev_ops; il; il = il->next ) {
      for ( i = 0; i < gnum_op_bit_at[time-1]; i++ ) {
	gop_conn[il->i1].info_at[time]->bit_exclusives[i] =
	  gop_conn[il->i1].info_at[time-1]->bit_exclusives[i];
      }
      for ( i = 0; i < gop_conn[il->i1].num_P; i++ ) {
	ft = gop_conn[il->i1].P[i];
	for ( j = 0; j < gnum_ft_bit; j++ ) {
	  gop_conn[il->i1].info_at[time]->bit_P_exclusives[j] |=
	    gft_conn[ft].info_at[time]->bit_exclusives[j];
	}
      }
    }
  }

  /* now examine all non-static (competing needs) of these relations;
   * if needs are no longer competing, remove the just set bit,
   * and remove that relation from the open list
   * (the list itself is built by code below)
   */
  ip = gin_op_mutex_pairs;
  while ( ip ) {
    if ( !competing_needs( time, ip->i1, ip->i2 ) ) {
      MAKE_OPS_UNEXCLUSIVE( time, ip->i1, ip->i2 );
      /* printf("\nremoving op excl comp needs"); */
      gin_op_exclusion_count--;
      if ( ip->prev ) {
	ip->prev->next = ip->next;
      } else {
	gin_op_mutex_pairs = ip->next;
      }
      if ( ip->next ) {
	ip->next->prev = ip->prev;
      }
      tmp = ip;
      ip = ip->next;
      free( tmp );
    } else {
      ip = ip->next;
    }
  }
  
  /* compare all pairs new op / new op resp. new op / old op;
   * if interfere, make mutex, if competing needs, make mutex and
   * put resp. pair into open list (see above)
   */
  for ( il = gin_ops; il != gin_prev_ops; il = il->next ) {
    for ( il2 = il->next; il2; il2 = il2->next ) {
      if ( interfere( il->i1, il2->i1 ) ) {
	MAKE_OPS_EXCLUSIVE( time, il->i1, il2->i1 );
	gin_op_exclusion_count++;
	continue;
      }
      if ( !competing_needs( time, il->i1, il2->i1 ) ) {
	continue;
      }
      MAKE_OPS_EXCLUSIVE( time, il->i1, il2->i1 );
      gin_op_exclusion_count++;
      tmp = new_IntPair( il->i1, il2->i1 );
      if ( gin_op_mutex_pairs ) {
	gin_op_mutex_pairs->prev = tmp;
      }
      tmp->next = gin_op_mutex_pairs;
      gin_op_mutex_pairs = tmp;
    }
  }

}



/* NOTE: of course it's a waste of time to do this here over again, but
 * it seems that storing all this info, i.e. an op X op table, requires to 
 * much memory.
 *
 * OTHER IDEAS??? -- perhaps hashing what has been done so far; on the other hand,
 * these comparisons are made at least between all pairs of actions that appear in the
 * graph, and these can be many already...
 */
Bool interfere( int op1, int op2 )

{

  int i;

  for ( i = 0; i < gnum_ft_bit; i++ ) {
    if ( ((gop_conn[op1].bit_P[i] | gop_conn[op1].bit_A[i]) & gop_conn[op2].bit_D[i]) ||
	 ((gop_conn[op2].bit_P[i] | gop_conn[op2].bit_A[i]) & gop_conn[op1].bit_D[i]) ) {
      return TRUE;
    }
  }

  return FALSE;

}



Bool competing_needs( int time, int op1, int op2 )

{

  int i;

  for ( i = 0; i < gnum_ft_bit; i++ ) {
    if ( gop_conn[op1].info_at[time]->bit_P_exclusives[i] &
	 gop_conn[op2].bit_P[i] ) {
      return TRUE;
    }
  }

  return FALSE;

}













/* fts
 */




















void find_mutex_fts( int time )

{

  IntList *il, *il2;
  IntPair *ip, *tmp;
  int i;

  /* pre-run: now that the ops excl. relations are there at time,
   * setup the bit_A_exclusives information, ie for all facts the ORed exclusives of
   * all possible achievers at time
   *
   * (note that these vectors have been allocated above in graph evolution step
   * already)
   *
   * in IPP, this info is computed in a more "on demand" style: the vectors are allocated
   * within the function call i.e. non-computed vectors are recognized as they are NULL.
   * This here is cleaner in style, vamos a ver whether one or the other option is more
   * effective -- depends on the specific needs of dynamic graph updates
   */
  for ( il = gin_fts; il; il = il->next ) {
    il2 = gft_conn[il->i1].info_at[time+1]->A;
    for ( i = 0; i < gnum_op_bit_at[time]; i++ ) {
      gft_conn[il->i1].info_at[time+1]->bit_A_exclusives[i] =
	gop_conn[il2->i1].info_at[time]->bit_exclusives[i];
    }
    for ( il2 = il2->next; il2; il2 = il2->next ) {
      for ( i = 0; i < gnum_op_bit_at[time]; i++ ) {
	gft_conn[il->i1].info_at[time+1]->bit_A_exclusives[i] &=
	  gop_conn[il2->i1].info_at[time]->bit_exclusives[i];
      }
    }
  }
      
  /* now copy over old exlusions info for those facts that were there
   * before
   */
  for ( il = gin_prev_fts; il; il = il->next ) {
    for ( i = 0; i < gnum_ft_bit; i++ ) {
      gft_conn[il->i1].info_at[time+1]->bit_exclusives[i] =
	gft_conn[il->i1].info_at[time]->bit_exclusives[i];
    }
  }

  /* similar to ops, check all mutexes in the open list and remove those 
   * that are not any longer valid.
   */
  ip = gin_ft_mutex_pairs;
  while ( ip ) {
    if ( !facts_are_exclusive( time, ip->i1, ip->i2 ) ) {
      /* in the trivial helper fns, we stick to the sound of the fn call
       * instead of to the ops-fts-schema
       */
      MAKE_FTS_UNEXCLUSIVE( time+1, ip->i1, ip->i2 );
      /* printf("\nremoving ft excl"); */
      gin_ft_exclusion_count--;
      if ( ip->prev ) {
	ip->prev->next = ip->next;
      } else {
	gin_ft_mutex_pairs = ip->next;
      }
      if ( ip->next ) {
	ip->next->prev = ip->prev;
      }
      tmp = ip;
      ip = ip->next;
      free( tmp );
    } else {
      ip = ip->next;
    }
  }

  /* now, again similar to ops above, check all pairs new ft / new ft resp.
   * new ft / old ft, and see whether they are mutex. if so,
   * mark them as being so and put the resp. pair into the open list.
   *
   * here we got no provably static mutexes (?!) so there's one special
   * case less than in the ops context.
   */
  for ( il = gin_fts; il != gin_prev_fts; il = il->next ) {
    for ( il2 = il->next; il2; il2 = il2->next ) {
      if ( !facts_are_exclusive( time, il->i1, il2->i1 ) ) {
	continue;
      }
      /* s.a.: in the trivial helper fns, we stick to the sound of the fn call
       * instead of to the ops-fts-schema
       */
      MAKE_FTS_EXCLUSIVE( time+1, il->i1, il2->i1 );
      gin_ft_exclusion_count++;
      tmp = new_IntPair( il->i1, il2->i1 );
      if ( gin_ft_mutex_pairs ) {
	gin_ft_mutex_pairs->prev = tmp;
      }
      tmp->next = gin_ft_mutex_pairs;
      gin_ft_mutex_pairs = tmp;
    }
  }

}



Bool facts_are_exclusive( int time, int ft1, int ft2 )

{

  int i;

  for ( i = 0; i < gnum_op_bit_at[time]; i++ ) {
    if ( gft_conn[ft1].info_at[time+1]->bit_A[i] !=
	 (gft_conn[ft1].info_at[time+1]->bit_A[i] & 
	  gft_conn[ft2].info_at[time+1]->bit_A_exclusives[i]) ) {
      return FALSE;
    }
  }

  return TRUE;

}






















/* trivial helpers
 */

















void MAKE_OPS_EXCLUSIVE( int time, int op1, int op2 ) 

{

  gop_conn[op1].info_at[time]->bit_exclusives[gop_conn[op2].uid_block] |=
    gop_conn[op2].uid_mask;

  gop_conn[op2].info_at[time]->bit_exclusives[gop_conn[op1].uid_block] |=
    gop_conn[op1].uid_mask;

}


							
void MAKE_OPS_UNEXCLUSIVE( int time, int op1, int op2 )

{

  gop_conn[op1].info_at[time]->bit_exclusives[gop_conn[op2].uid_block] &=
    ~(gop_conn[op2].uid_mask);

  gop_conn[op2].info_at[time]->bit_exclusives[gop_conn[op1].uid_block] &=
    ~(gop_conn[op1].uid_mask);

}



void MAKE_FTS_EXCLUSIVE( int time, int ft1, int ft2 ) 

{

  gft_conn[ft1].info_at[time]->bit_exclusives[gft_conn[ft2].uid_block] |=
    gft_conn[ft2].uid_mask;

  gft_conn[ft2].info_at[time]->bit_exclusives[gft_conn[ft1].uid_block] |=
    gft_conn[ft1].uid_mask;

}


							
void MAKE_FTS_UNEXCLUSIVE( int time, int ft1, int ft2 )

{

  gft_conn[ft1].info_at[time]->bit_exclusives[gft_conn[ft2].uid_block] &=
    ~(gft_conn[ft2].uid_mask);

  gft_conn[ft2].info_at[time]->bit_exclusives[gft_conn[ft1].uid_block] &=
    ~(gft_conn[ft1].uid_mask);

}
