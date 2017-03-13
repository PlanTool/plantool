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




#ifndef _SUBSPACE_H
#define _SUBSPACE_H

#define VSIZE 30000

typedef struct _vState {
      int F[VSIZE];  
      int num_F;
} vState;

int build_subspace(State * start_state,  int* rsa_facts, int* rsa_actions);
int rip_facts_subspace(State * start_state,State *gs,int* rsa_facts,int* rsa_actions) ;
int block_fact_subspace(State * start_state, int ripped, int* rsa_facts, int* rsa_actions, State * gs, int *decr_g);        
void pure_numerical_actions();

#endif /* _SUBSPACE_H */
