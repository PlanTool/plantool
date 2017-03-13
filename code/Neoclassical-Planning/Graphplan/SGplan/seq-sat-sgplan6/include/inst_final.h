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

 * File: inst_final.h

 * Description: modified from inst_final.h in LPG

 *

 * Authors:  Chih-wei Hsu and Benjamin W. Wah

 *

 *********************************************************************/
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


/**
   The following parts of this file have been modified by the 
   LPG developers (DEA - University of Brescia):

   - Added prototypes:
   - set_relevants_in_wff
   - fact_adress1
**/










/*********************************************************************
 * File: inst_final.h
 * Description: headers for final domain representation functions
 *
 *
 * Author: Joerg Hoffmann 2000
 *
 *********************************************************************/ 







#ifndef _INST_FINAL_H
#define _INST_FINAL_H



void perform_reachability_analysis( void );
int fact_adress( void );
void make_name_inst_table_from_NormOperator( Action *a, NormOperator *o, EasyTemplate *t );
void make_name_inst_table_from_PseudoAction( Action *a, PseudoAction *pa );



void collect_relevant_facts( void );
void create_final_goal_state( void );

/*
 * DEA - University of Brescia
 */

int set_relevants_in_wff( WffNode **w );

/*
 * End of DEA
 */

void create_final_initial_state( void );
void create_final_actions( void );


void set_DPop_flag();
Bool compartment_ops(int pos, int neg);
void build_connectivity_graph( void );
int ft_argncmp(int f, int g, int n, int m);
int op_argncmp(int f, int g, int n, int m);
int ft_predlen(int f);
int ef_predlen(int f);
int ft_arity(int f);
int ef_arity(int f);
int tok_ft_argncmp(int f, int n, char *tok);
int tok_ef_argncmp(int f, int n, char *tok);

/*
 * DEA - University of Brescia
 */

int fact_adress1 (void);

/*
 * End of DEA
 */


#endif /* _INST_FINAL_H */
