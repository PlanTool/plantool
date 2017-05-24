



/*********************************************************************
 * File: instantiateI.h
 * Description: headers for instantiating, first part
 *
 * contains code for most of the stuff that needs to be
 * done up front:
 *                       - collect all strings of the domain
 *                       - perform basic syntax checks on input
 *                       - translate PlNode s into tighter CodeNode s
 *                       - build the implicit tuple tables for
 *                         fast access to the initial state info (TR 122)
 *                       - encode unary inertia into types and thus
 *                         receive easier instantiable operators (TR 122)
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


#ifndef _INSTANTIATEI_H
#define _INSTANTIATEI_H




void collect_all_strings( void );
int position_in_types_table( char *str );
int position_in_constants_table( char *str );
int position_in_predicates_table( char *str );



void transform_PlNodes_to_CodeNodes( void );
Bool normalize_initial( PlNode **n );
Bool is_legal_condition( PlNode *n );
Bool normalize_effects( PlNode **n );
Bool normalize_effect( PlNode **n );
CodeOperator *transform_PlOp_to_CodeOp( PlOperator *op );
CodeNode *transform_Pl_to_Code( PlNode *p, int num_vars );
Bool is_subtype( int t1, int t2 );



void build_predicate_tables( void );
void setup_added_deleted_info( void );
void extract_added_deleted_info( CodeNode *n );
void increment_tuples( CodeNode *n );
Bool possibly_positive( int predicate, ArgArray arguments );
Bool possibly_negative( int predicate, ArgArray arguments );
int a_to_the_power_of_b( int a, int b );
int increment_set( int n, ArgArray *set );
int inner_table_adr_sethelper( int n, ArgArray set, ArgArray args );
int inner_table_adr( int n, ArgArray args );
int set_number( int n, ArgArray args );
int max_unifying_tuples( int p, ArgArray args );
void free_predicate_tables( void );



void encode_unary_inertia_in_types( void );
void encode_unaries_under_CodeNode( CodeNode **n );
int var_used_in_unary_under( int var, CodeNode *n );
void replace_unary_var_occurences( short int unary, int var, 
				   Connective val, CodeNode **n );
void collect_all_effect_prefix_paths( CodeNode *n );
void recursive_collect_all_effect_prefix_paths( CodeNode *n, 
						CodeNode *history );



#endif /* _INSTANTIATEI_H */

