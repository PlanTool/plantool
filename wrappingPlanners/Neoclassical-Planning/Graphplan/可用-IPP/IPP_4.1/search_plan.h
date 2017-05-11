


/*********************************************************************
 * File: search_plan.h
 * Description: headers of routines forsearching the graph
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




#ifndef _SEARCH_PLAN_H
#define _SEARCH_PLAN_H







void DO_OP( int time, OpNode *op );
void UNDO_OP( int time, OpNode *op );
void DO_FT( int time, FtNode *ft, OpNode *op );
void UNDO_FT( int time, FtNode *ft );
FtNode *NEG_FT( FtNode *ft );




Bool search_plan( int max_time );



Bool search( int time, int curr_index );
Bool complete_goals_and_search( int time );



Bool action_set_is_minimal( int time );
Bool goals_still_possible( int time, int n, OpNode *op );



Bool try_noop( int time, FtNode *ft, int curr_index );
Bool try_ef( int time, EfNode *ef, int curr_index );
void untry_noop( int time, FtNode *ft );
void untry_ef( int time, EfNode *ef );



Bool cant_do_ft( int time, FtNode *ft );
Bool cant_do_critic( int time, FtNode *ft, int start_index );
Bool is_deleted( int time, FtNode *ft, OpNode *op );
Bool cant_do_op( int time, OpNode *op );
Bool cant_do_preconds( int time, EfNode *ef );
Bool cant_do_conditions( int time, EfNode *ef );



void expand( int max_time );



#endif /* _SEARCH_PLAN_H */
