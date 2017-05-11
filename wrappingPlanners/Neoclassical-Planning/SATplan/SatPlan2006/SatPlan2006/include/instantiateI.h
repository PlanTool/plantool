

/*********************************************************************
 * (C) Copyright 1999 Albert Ludwigs University Freiburg
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
 * File: instantiateI.h
 * Description: headers for instantiating operators, first part.
 *              - transform domain into integers
 *              - domain cleanup functions
 *              - inertia preprocessing:
 *                  - collect inertia info
 *                  - split initial state in special arrays
 *                  - encode unary inertia as types
 *
 * Author: Joerg Hoffmann 1999
 *
 *********************************************************************/ 







#ifndef _INSTANTIATEI_H
#define _INSTANTIATEI_H



void encode_domain_in_integers( void );
void collect_all_strings( void );
int position_in_types_table( char *str );
int position_in_constants_table( char *str );
int position_in_predicates_table( char *str );
void create_integer_representation( void );
void make_Fact( Fact *f, PlNode *n, Operator *o );
Bool is_subtype( int t1, int t2 );
void cleanup_domain( void );
void replace_var_entries( Operator *o, int p0, int p1 );
void remove_identical_preconds_and_effects( Operator *o );
Bool identical_fact( Fact *f1, Fact *f2 );
void remove_unused_parameters( void );
void decrement_var_entries( Operator *o, int start );



void do_inertia_preprocessing( void );
void collect_inertia_information( void );
void split_initial_state( void );
void encode_unary_inertia_as_types( void );
int find_intersected_type( TypeArray T, int num_T );
void remove_ops_with_empty_parameter_types( void );



#endif /* _INSTANTIATEI_H */

