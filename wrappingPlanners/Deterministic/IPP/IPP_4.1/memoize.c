


/*********************************************************************
 * File: memoize.c
 * Description: code for realizing UBTree subset memoization strategy
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

#include "memoize.h"








/*************************************
 * SIMPLE HELPERS, SHOULD BE MACROS. *
 *************************************/







int MIN( int a, int b )

{

  return ( a < b ) ? a : b;

}







/***********************************************
 * THE TWO MAIN FUNCIONS, CALLED DURING SEARCH *        
 ***********************************************/










void memoize( int time )

{ 

  static IntArray double_indizes;

  int i, way = gnum_goals_at[time];
  MemoNode *node;
  FtNode *ft;

  if ( gnum_goals_at[time] == 0 ) {
    return;
  }

  for ( i=0; i<gnum_goals_at[time]; i++ ) {
    if ( ggoals_at[time][i]->positive ) {
      double_indizes[i] = ggoals_at[time][i]->index;
    } else {
      double_indizes[i] = NEG_ADR( ggoals_at[time][i]->index );
    }
  }

  quicksort( &double_indizes, 0, gnum_goals_at[time] - 1 );

  ft = gft_table[double_indizes[0]];
  node = ft->info_at[time]->memo_start;

  if ( !node ) {
    node = new_memo_node( double_indizes[0], --way );
    ft->info_at[time]->memo_start = node;
  } else {
    node->min_way = MIN( node->min_way, --way );
  }

  for ( i=1; i<gnum_goals_at[time]; i++ ) {
    if ( !node->sons ) {
      node->sons = new_memo_node_table();
    }
    node = insert_memo_node( node->sons, double_indizes[i] );
    node->min_way = MIN( node->min_way, --way );
  }

  /*  free_sub_tree( node ); */

}




Bool memoized( int time )

{

  static IntArray double_indizes;

  int i;
  MemoNode *node;
  FtNode *ft;

  if ( gnum_goals_at[time] == 0 ) {
    return FALSE;
  } 

  for ( i=0; i<gnum_goals_at[time]; i++ ) {
    if ( ggoals_at[time][i]->positive ) {
      double_indizes[i] = ggoals_at[time][i]->index;
    } else {
      double_indizes[i] = NEG_ADR( ggoals_at[time][i]->index );
    }
  }

  quicksort( &double_indizes, 0, gnum_goals_at[time] - 1 );

  if ( straight_memoized( time, double_indizes, 0 ) ) {
    gsimple_hits++;
    return TRUE;
  }

  if ( !gcmd_line.do_subset ) {
    return FALSE;
  }

  for ( i=1; i<gnum_goals_at[time]; i++ ) {
    if ( straight_memoized( time, double_indizes, i ) ) {
      gpartial_hits++;
      return TRUE;
    }
  }

  for ( i=0; i<gnum_goals_at[time]; i++ ) {
    ft = gft_table[double_indizes[i]];
    node = ft->info_at[time]->memo_start;
    if ( !node ) continue;
    if ( node->min_way > gnum_goals_at[time] - i - 1 ) continue;
    if ( subset_memoized( time, double_indizes, i + 1, node ) ) {
      gsubset_hits++;
      return TRUE;
    }
  }

  return FALSE;

}








/*************************************************************
 * FUNCTIONS FOR THE PROCESS OVERSEEN IN MEMOIZED - QUERY FN *
 *************************************************************/










Bool straight_memoized( int time, IntArray double_indizes, int start )

{ 

  int way = gnum_goals_at[time] - start, i;
  FtNode *ft;
  MemoNode *node;

  ft = gft_table[double_indizes[start]];
  node = ft->info_at[time]->memo_start;

  if ( !node || node->min_way > --way ) return FALSE;

  for ( i=start+1; i<gnum_goals_at[time]; i++ ) {
    if ( !node->sons ) {
      return FALSE;
    }
    node = lookup_memo_node( node->sons, double_indizes[i] );
    if ( !node || node->min_way > --way ) {
      return FALSE;
    }
    if ( node->min_way == 0 ) {
      return TRUE;
    }
  }

  return FALSE;

}



Bool subset_memoized( int time,
		      IntArray double_indizes, int curr_index,
		      MemoNode *node )

{

  int way = gnum_goals_at[time] - curr_index, i;
  MemoNode *new_node;

  if ( node->min_way == 0 ) {
    return TRUE;
  }

  if ( !node->sons || node->min_way > way ) {
    return FALSE;
  }

  for ( i=curr_index; i<gnum_goals_at[time]; i++ ) {
    new_node = lookup_memo_node( node->sons, double_indizes[i] );
    if ( !new_node ) {
      continue;
    }
    if ( subset_memoized( time, double_indizes, i + 1, new_node ) ) {
      return TRUE;
    }
  }
  
  return FALSE;

}








/******************************
 * SIMPLE HELPERS FOR HASHING *
 ******************************/









MemoNode *insert_memo_node( MemoNode_table *sons, int double_index )

{

  int position;
  MemoNode *node;

  if ( (node = lookup_memo_node( sons, double_index)) != NULL ) {
    return node;
  }

  position = ( int ) ( double_index & MEMO_HASH );

  node = new_memo_node( double_index, ARRAY_SIZE + 1 );

  /* here, one could sort the nodes, i.e., insert the node in 
   * increasing order of double_index
   */
  node->next = (*sons)[position];
  (*sons)[position] = node;

  return node;

}


MemoNode *lookup_memo_node( MemoNode_table *sons, int double_index )

{

  int position;
  MemoNode *node;

  position = ( int ) ( double_index & MEMO_HASH );

  for ( node = (*sons)[position]; node; node = node->next ) {
    if ( node->double_index != double_index ) {
      /* if we had the nodes sorted, we could break here at  > 
       */
      continue;
    }
    return node;
  }

  return NULL;

}











/********************
 * GENERAL HELPERS. *
 ********************/










/* you should know about that one
 *
 * if you don't: look it up in
 *   T. Ottmann / P.Widmayer 
 *   Algorithmen und Datenstrukturen, 3. Auflage
 *   page 85/86, Quicksort-Varianten, median of three quicksort
 */
void quicksort( IntArray *a, int l, int r )

{

  int v, i, j, m, t;

  if ( r > l ) {
    m = ( int ) ( r + l ) / 2;
    if ( (*a)[l] > (*a)[r] ) {
      t = (*a)[l];
      (*a)[l] = (*a)[r];
      (*a)[r] = t;
    }
    if ( (*a)[l] > (*a)[m] ) {
      t = (*a)[l];
      (*a)[l] = (*a)[m];
      (*a)[m] = t;
    }
    if ( (*a)[r] > (*a)[m] ) {
      t = (*a)[r];
      (*a)[r] = (*a)[m];
      (*a)[m] = t;
    }
    i = l - 1;
    j = r;
    v = (*a)[r];
    while( i < j ) {
      do i++; while ( (*a)[i] < v );
      do j--; while ( j >= 0 && (*a)[j] > v );
      if ( i < j ) {
        t = (*a)[i];
        (*a)[i] = (*a)[j];
        (*a)[j] = t;
      }
    }
    t = (*a)[i];
    (*a)[i] = (*a)[r];
    (*a)[r] = t;
    quicksort( a, l, i - 1 );
    quicksort( a, i + 1, r );
  }

}
