

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
 * File: instantiateII.h
 * Description: headers for instantiating operators, second part.
 *
 *              - unify inertia in preconds with initial state
 *              - multiply remaining uninstantiated parameters
 *              - perform reachability analysis
 *              - collect relevant facts and perform final action cleanup
 *
 *
 * Author: Joerg Hoffmann 1999
 *
 *********************************************************************/ 







#ifndef _INSTANTIATEII_H
#define _INSTANTIATEII_H





void build_action_templates( void );
void unify_inertia_preconds( int curr_inertia );
void multiply_parameters( int curr_parameter );



void perform_reachability_analysis( void );
int fact_adress( void );



void collect_relevant_facts( void );



void build_connectivity_graph( void );



void insert_bit_vectors( void );



#endif /* _INSTANTIATEII_H */

