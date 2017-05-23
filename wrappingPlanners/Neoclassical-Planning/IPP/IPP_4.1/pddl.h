




/*********************************************************************
 * File: pddl.h
 * Description: Headerfile for the pddl dependend part of
 *              the parser and the preprocessing.
 *
 * Author: Frank Rittinger 1998
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

#ifndef _PDDL_H
#define _PDDL_H

#include "ipp.h"
#include "output.h"

/********************************************************************* 
 * function prototypes from pddl.c 
 *********************************************************************/

int supported(char * str);

void pddl_preprocessing ( PlOperator * plops );

Bool contains_when ( PlNode * plnode );

void combine_when ( PlNode * plnode );

void ands_to_top ( PlNode * plnode );

void pdnf ( PlNode * plnode ); 

void dnf ( CodeNode * codenode ); 

void cnf ( CodeNode * codenode ); 

void distribute ( PlNode * plnode ); 

void quantifier_to_top ( PlNode * plnode , 
			     Connective outer , 
			     Connective inner); 

void nots_down_quantifiers_up_pl (PlNode * plnode);

void combine_ands_pl ( PlNode * plnode ); 

void combine_ands_code ( CodeNode * codenode ); 

void rename_all_quantifiers_ ( PlNode * plnode ); 

Bool rename_quantifier ( PlNode * plnode , char * token, Bool cont); 

void distribute_when (PlNode * plnode , const Bool lowest);

void distribute_unary_pl (PlNode * plnode, void ( * rec_fn)(PlNode * plnode));
void all_the_quantifiers_top_pl (PlNode * plnode);

void pddl_inner (void);

void pddl_outer (void);

#endif /* _PDDL_H */


