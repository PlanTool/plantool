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

 * File: dis_output.h 

 * Description: modified from output.h in Metric-FF

 *

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
 * File: output.h
 * Description: print headers
 *
 * Author: Joerg Hoffmann 1999
 *
 *********************************************************************/ 





#ifndef _dis_OUTPUT_H
#define _dis_OUTPUT_H



void dis_print_dis_FactList( dis_FactList *list, char *sepf, char *sept );
void dis_print_hidden_dis_TokenList( dis_TokenList *list, char *sep );
void dis_print_indent( int indent );
void dis_print_dis_Parsedis_ExpNode( dis_Parsedis_ExpNode *n );
void dis_print_dis_PlNode( dis_PlNode *plnode, int indent );
void dis_print_dis_ExpNode( dis_ExpNode *n );
void dis_print_Wff( dis_WffNode *n, int indent );
void dis_print_plops( dis_Pldis_Operator *plop );
void dis_print_dis_Operator( dis_Operator *o );
void dis_print_Normdis_Operator( Normdis_Operator *o );
void dis_print_Mixeddis_Operator( Mixeddis_Operator *o );
void dis_print_dis_Pseudodis_Action( dis_Pseudodis_Action *o );
void dis_print_dis_Action( dis_Action *a );
void dis_print_dis_Action_name( dis_Action *a );
void dis_print_lnf_dis_Action( dis_Action *a );
void dis_print_type( int t );
void dis_print_dis_Fact( dis_Fact *f ); 
void dis_print_dis_Fluent( dis_Fluent *f );
void dis_print_ft_name( int index );
void dis_print_ft_name_string( int index, char* string );
void dis_print_op_name( int index );
void dis_print_fl_name( int index );
void dis_print_Lnfdis_ExpNode( Lnfdis_ExpNode *n );
void dis_print_dis_State( dis_State S );
void dis_print_dis_Fact_string( dis_Fact *f, char *str);


void dis_print_plan( void );



#endif /* _OUTPUT_H */
