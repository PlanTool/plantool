


/*********************************************************************
 * File: wave_front.c
 * Description: implementation of wave front - idea from STAN paper
 *
 * Author: Joerg Hoffmann 1998
 *
 *********************************************************************/ 
/*********************************************************************
 * (C) Copyright 1998 Albert Ludwigs University Freiburg
 *     Institute of Computer Science
 *
 * All rights reserved. Use of this software is permitted for 
 * non-commercial research purposes, and it may be copied only 
 * for that use.  All copies must include this copyright message.
 * This software is made available AS IS, and neither the authors
 * nor the  Albert Ludwigs University Freiburg make any warranty
 * about the software or its performance. 
 *********************************************************************/




#include "ipp.h"

#include "output.h"
#include "utilities.h"
#include "memory.h"

#include "wave_front.h"
#include "memoize.h"
#include "search_plan.h"






int search_wave_front( void )

{

  Candidate *start = new_candidate(), *current, *p, *next;
  int i = 0, max_depth = 0, r;
  Integers *j;

  gwave_front_head = new_candidate();
  gwave_front_tail = new_candidate();

  for ( j = gbit_goal_state->positive->indices; j; j = j->next ) {
    start->fts[i++] = gft_table[j->index];
  }
  for ( j = gbit_goal_state->negative->indices; j; j = j->next ) {
    start->fts[i++] = gft_table[NEG_ADR( j->index )];
  }
  start->fts[i] = NULL;
  start->ops[0] = NULL;
  start->father = NULL;
  start->depth = 0;
  start->prev = gwave_front_head;
  start->next = gwave_front_tail;

  gwave_front_head->father = NULL;
  gwave_front_head->depth = 0;
  gwave_front_head->prev = NULL;
  gwave_front_head->next = start;
  gwave_front_tail->father = NULL;
  gwave_front_tail->depth = 0;
  gwave_front_tail->prev = start;
  gwave_front_tail->next = NULL;

  expand( gfirst_full_time + 1 );

  current = gwave_front_head->next;
  while ( current != gwave_front_tail ) {

    if ( current->depth > max_depth ) {
      if ( gcmd_line.display_info ) {
	fprintf( OUT, "\nexpanding wave front to level %4d", 
		 gfirst_full_time + 1 + current->depth );
      }
      max_depth = current->depth;
    }
    
    if ( expand_candidate( current ) ) {
      break;
    }

    current->prev->next = current->next;
    current->next->prev = current->prev;

    /* to avoid memory leak:
     * link disconnected candidate into global trash list
     *
     * (keep track of next candidate to expand)
     */
    next = current->next;
    current->next = gwave_front_trash;
    gwave_front_trash = current;

    current = next;
  }

  if ( current == gwave_front_tail ) {
    if ( gcmd_line.display_info ) {
      fprintf( OUT, "\n\n\nproblem proved unsolvable: wave front is empty!\n\n" );
    }
    return gfirst_full_time + 1;
  }

  if ( gcmd_line.display_info ) {
    fprintf( OUT, "\n\n" );
    print_plan( gfirst_full_time + 1 );
    print_incremental_plan( current );    
  }

  gplan_start = current;
  r = gfirst_full_time;
  for ( p = current; p; p = p->father ) {
    r++;
  }
  return r;

}



Bool expand_candidate( Candidate *current )

{

  int time = gfirst_full_time + 1, i;
  Bool result;

  gnum_goals_at[time] = 0;
  for ( i = 0; current->fts[i] != NULL; i++ ) {
    DO_FT( time, current->fts[i], NULL );
  }

  if ( memoized( time ) ) {
    return FALSE;
  }

  gnum_ops_at[time-1] = 0;
  gnum_goals_at[time-1] = 0;


  result = search( time, gnum_goals_at[time] - 1);


  for ( i=gnum_goals_at[time]-1; i>-1; i-- ) {
    UNDO_FT( time, ggoals_at[time][i] );
  }
 

  if ( !result ) {
    memoize( time );
  }

  return result;

}




void add_candidate( int time )

{

  Candidate *new = new_candidate();
  int i;

  for ( i=0; i<gnum_goals_at[time]; i++ ) {
    new->fts[i] = ggoals_at[time][i];
  }
  new->fts[i] = NULL;

  for ( i=0; i<gnum_ops_at[time]; i++ ) {
    new->ops[i] = gops_at[time][i];
  }
  new->ops[i] = NULL;
  
  new->father = gwave_front_head->next;
  new->depth = new->father->depth + 1;
  new->prev = gwave_front_tail->prev;
  new->next = gwave_front_tail;
  gwave_front_tail->prev = new;
  new->prev->next = new;

}
  


