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

 * File: subfluent.h

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
 *
 *********************************************************************/ 




#ifndef _SUBFLUENT_H
#define _SUBFLUENT_H


#define MAX_FL_NODE 50
#define MAX_FL_RES 10
#define MAX_FL_ROUND 50
#define MAX_FL_FIELD 10

typedef struct _RelaxFlNode {
          char *s;
          int field[MAX_FL_FIELD];
} RelaxFlNode;
    
typedef struct _FluentHash {
    char* key;
    float value;
} FluentHash;

typedef struct _IntHash{
    int key;
    float value;
} IntHash;

void dis_collect_fluent();
void relax_1_easy_resource(State *S);
void relax_1_easy_predicate(State* S);
void relax_2_easy_resource(State *S);
void relax_1_hard_resource(State *S);
void relax_2_hard_resource(State *S);
void relax_3_hard_resource(State *S);
Bool fl_is_exceeding();

void collect_for_shortest();
void short_analyze_subpath();
void short_prune_subgoals();
void short_tif();
void short_process_sofar(int g);
int short_connect(int goal);
int short_min_sofar(); 
int short_given_connect(int goal, char* sat, char *inst);
void dis_delete_gplan_ops(int i);
 
#endif /* _SUBFLUENT_H */
