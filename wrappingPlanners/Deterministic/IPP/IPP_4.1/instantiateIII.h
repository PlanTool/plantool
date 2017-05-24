



/*********************************************************************
 * File: instantiateIII.h
 * Description: headers for instantiating, third part
 *
 *                        - expand all quantifiers into con/dis junctions
 *                        - multiply params of (conditional) effects
 *                        - multiply params of operators
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


#ifndef _INSTIII_H
#define _INSTIII_H



void multiply_params_and_quantifiers( void );
Bool expand_preconditions_of_CodeOperator( CodeOperator **op );
Bool expand_conditions_of_effect( CodeNode **ef );
void multiply_params_of_effect( CodeNode *pars, CodeNode *when_sons );
void multiply_params_of_CodeOperator( CodeOperator *op, int curr,
				      CodeNode *precond );
void expand_quantifiers_under_CodeNode( CodeNode **n, 
					short int var, short int constant );
void make_inst_version_of_effect( CodeNode **n,
				  short int var, short int constant );
void make_inst_version_of_condition( CodeNode **n,
				     short int var, short int constant );
void merge_multiple_ANDs_and_ORs( CodeNode **n );



#endif /* _INSTIII_H */

