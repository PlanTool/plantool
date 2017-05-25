


/*********************************************************************
 * File: memoize.c
 * Description: function headers for UBTree and simple memoization...
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




#ifndef _MEMOIZE_H
#define _MEMOIZE_H






void memoize( int time );
Bool memoized( int time );


Bool straight_memoized( int time, IntArray double_indizes, int start );
Bool subset_memoized( int time,
		      IntArray double_indizes, int curr_index,
		      MemoNode *node );


MemoNode *insert_memo_node( MemoNode_table *sons, int double_index );
MemoNode *lookup_memo_node( MemoNode_table *sons, int double_index );


void quicksort( IntArray *a, int l, int r );


#endif /* _MEMOIZE_H */

