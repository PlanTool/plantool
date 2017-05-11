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

 * File: dis_parse.h 

 * Description: modified from parse.h in Metric-FF

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
 * File: parse.h
 * Description: Functions for the pddl parser
 *
 * Author: Frank Rittinger 1998 / Joerg Hoffmann 1999
 *
 *********************************************************************/ 





#ifndef _dis_PARSE_H
#define _dis_PARSE_H



char *dis_copy_dis_Token( char *s );
dis_TokenList *dis_copy_dis_TokenList( dis_TokenList *source );
void dis_strupcase( char *from );
char *dis_rmdash( char *s );



void dis_build_orig_constant_list( void );
void dis_collect_type_names_in_pl( dis_PlNode *n );
int dis_get_type( char *str );
void dis_make_either_ty( dis_TypedList *tyl );
void dis_make_either_ty_in_pl( dis_PlNode *n );
void dis_normalize_tyl_in_pl( dis_PlNode **n );



dis_Bool dis_make_adl_domain( void );
dis_Bool dis_make_conjunction_of_atoms( dis_PlNode **n );
dis_Bool dis_is_wff( dis_PlNode *n );
dis_Bool dis_make_effects( dis_PlNode **n );
dis_Bool dis_is_eff_literal( dis_PlNode *n );
dis_Bool dis_make_conjunction_of_literals( dis_PlNode **n );

void dis_add_types( void );

#endif /* PARSE */
