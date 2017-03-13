





/*********************************************************************
 * File: instantiateIV.h
 *
 * Description: routines that perform the final instantiation 
 *
 *                        - collect relevant facts and adjust ATOMs to them
 *
 *                        - create BitMap representation of domain
 *
 *
 * Author: Joerg Hoffmann 1999
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






#ifndef _INSTIV_H
#define _INSTIV_H



void collect_relevant_facts( void );
void set_relevants_in_condition( CodeNode **n );
void set_relevants_in_effect( CodeNode **n );
int pos_neg_use_index_adress( int predicate, ArgArray arguments );
void make_pos_neg_use_index_entries( CodeNode *n );




void generate_bitmap_representation( void );
void generate_ini_goal_bitmap_representation( void );
int get_bit( BitVector *vec, int vec_len, int pos );
void generate_BitOperators( CodeOperator *op );
void make_entry_in_FactInfo( FactInfo **f, int index );
void make_effect_entries( Effect **e, CodeNode *n );



#endif /* _INSTIV_H */


 
