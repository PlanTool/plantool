


/*********************************************************************
 * File: exclusions.h
 * Description: headers of routines for calculating exclusion relations
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




#ifndef _EXCLUSIONS_H
#define _EXCLUSIONS_H







Bool ARE_MUTEX_OPS( int time, OpNode *o1, OpNode *o2 );
Bool ARE_MUTEX_FTS( int time, FtNode *f1, FtNode *f2 );
void SET_ADDERS( int time, FtNode *ft ) ;


void find_mutex_ops( int time );
void find_mutex_facts( int time );


Bool competing_needs( int time, OpNode *o1, OpNode *o2 );
Bool interfere( OpNode *i1, OpNode *i2 );
Bool noop_exclusive( OpNode *i1, OpNode *i2 );
Bool facts_are_exclusive( int time, FtNode *f1, FtNode *f2 );


#endif /* _EXCLUSIONS_H */
