


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
 * File: search.h
 *
 * Description: headers of routines that search the state space
 *
 *              ADL version, Enforced Hill-climbing enhanced with
 *                           Goal-adders deletion heuristic
 *
 * Author: Joerg Hoffmann 2000
 *
 *********************************************************************/ 






#ifndef _SEARCH_H
#define _SEARCH_H



Bool do_enforced_hill_climbing( State *start, State *end );



Bool search_for_better_state( State *S, int h, State *S_, int *h_ );
void add_to_ehc_space( State *S, int op, EhcNode *father );
int expand_first_node( int h );



void extract_plan_fragment( State *S );



Bool do_best_first_search( void );
void add_to_bfs_space( State *S, int op, BfsNode *father );
void extract_plan( BfsNode *last );



void source_to_dest( State *dest, State *source );
void copy_source_to_dest( State *dest, State *source );
void print_state( State S );



void manual_control( void );



#endif /* _SEARCH_H */
