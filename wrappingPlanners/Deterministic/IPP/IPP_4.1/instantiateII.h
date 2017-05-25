



/*********************************************************************
 * File: instantiateII.h
 * Description: headers for instantiating, second part
 *
 * code for handling transformations on CodeNodes that can occur
 * during (almost) all stages of instantiation:
 *
 *                        - detect tautologies in PLI conditions
 *
 *                        - simplify a PLI condition
 *                        - detect trivial or inconsistent or
 *                          never applicable effects
 *                        - simplify an effect list according to that
 *                        - detect unused variables, remove them
 *                        - clean up operators
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


#ifndef _INSTII_H
#define _INSTII_H


void detect_all_tautologies( Bool warnings );
void detect_inst_CodeOperator_tautologies( void );
void detect_tautologies_in_condition_CodeNode( CodeNode **n, Bool warnings );
Bool are_identical_CodeNodes( CodeNode *n1, CodeNode *n2 );





void simplify_all_CodeNodes( Bool warnings );
void simplify_all_CodeOperators( Bool warnings );
void simplify_all_inst_CodeOperators( void );
void simplify_condition_CodeNode( CodeNode **n, Bool warnings, Bool condition );
void clean_up_effects( CodeNode **n, Bool warnings );
Bool impossible_effect( CodeNode *n, Bool warnings );
Bool no_transition_effect( CodeNode *n, Bool warnings );
Bool has_contradicting_unconditional( CodeNode *n );
Bool contradicting_unconditional_effect( CodeNode *n );
Bool var_used_under_CodeNode( int var_num, CodeNode *n );
void remove_unused_vars_under_CodeNode( CodeNode **n, Bool warnings  );
void rec_remove_unused_vars_under_CodeNode( CodeNode **n, Bool warnings );
void rename_vars_under_CodeNode( CodeNode **n, int num_vars );
void replace_var_with_newvar_under_CodeNode( int var, int new_var, 
					     CodeNode **n );



#endif /* _INSTII_H */

