/*********************************************************************

 * (C) Copyright 2008, University of Illinois, Urbana-Champaign

 *

 * All rights reserved. Use of this software is permitted ONLY for

 * non-commercial research purposes, and it may be copied only

 * for that use only. All copies must include this copyright message.

 * This software is made available AS IS, and neither the authors

 * nor the University of Illinois, make any warranty about the

 * software or its performance.

 *

 *********************************************************************/
/********************************************************************

 * File: output.h 
 
 * Authors:  Chih-wei Hsu and Benjamin W. Wah

 *

 *********************************************************************/
/*
# Copyright (C) 2004, Board of Trustees of the University of Illinois.
#
# The program is copyrighted by the University of Illinois, and should
# not be distributed without prior approval.  Commercialization of this 
# product requires prior licensing from the University of Illinois.
# Commercialization includes the integration of this code in part or 
# whole into a product for resale. 
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Author: Y. X. Chen, C. W. Hsu, and B. W. Wah, University of Illinois
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
*/
/*********************************************************************
 * (C) Copyright 2000 Albert Ludwigs University Freiburg
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
 * File: output.h
 * Description: print headers
 *
 * Author: Joerg Hoffmann 1999
 *
 *********************************************************************/ 





#ifndef _OUTPUT_H
#define _OUTPUT_H



void print_FactList( FactList *list, char *sepf, char *sept );
void print_hidden_TokenList( TokenList *list, char *sep );
void print_indent( int indent );
void print_PlNode( PlNode *plnode, int indent );
void print_Wff( WffNode *n, int indent );
void print_plops( PlOperator *plop );
void print_Operator( Operator *o );
void print_NormOperator( NormOperator *o );
void print_MixedOperator( MixedOperator *o );
void print_PseudoAction( PseudoAction *o );
void print_Action( Action *a );
void print_type( int t );
void print_Fact( Fact *f ); 
void print_ft_name( int index );
void print_op_name( int index );
void print_plan( void );
/* PDDL 3 */
void print_ConNode( ConNode *plnode, int indent );
void print_PrefNode( PrefNode *plnode );


#endif /* _OUTPUT_H */
