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

 * File: casual_graph.h

 * Authors:  Chih-wei Hsu and Benjamin W. Wah

 *********************************************************************/
/*
# Copyright (C) 2006, Board of Trustees of the University of Illinois.
#
# The program is copyrighted by the University of Illinois, and should
# not be distributed without prior approval.  Commercialization of this
# product requires prior licensing from the University of Illinois.
# Commercialization includes the integration of this code in part or
# whole into a product for resale.
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Author: C. W. Hsu, B. W. Wah, R. Y. Huang, and Y. X. Chen  
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
*/
/***********************************************************************************************************************

                                  					INVARIANTS

***********************************************************************************************************************/

typedef struct _dis_InvariantElement{
	int predicate;
		
	int focus;  
	dis_Fact* fact; 
} dis_InvariantElement;

typedef struct _dis_Invariant{
	int focusType; 
	dis_InvariantElement *obj[200];
	int size;
	int deleted;
	int id;
} dis_Invariant;

/*
	THE APPROACH TO FIND OUT ACCOMPANY FACTS;
	Initially, the the fact
	Suppose two fact is AT XX ZZ, AND IN XX YY.  We have no idea which one is master and which one is slave;

	#1. Find out an action ACTION which has BOTH AT XX ZZ and IN XX YY as a add effect. (I think add effect is enough);
	#2. FIND a predicate  PPP in ACTION's PRECONDITION, which INCLUDE YY and ZZ in, we call this as a *RELATION* predicate.
	#3. Get all the grounded facts of "PPP YY ZZ". For these facts, if for first param, there are duplicated objects, but param2 do not.
		we can consider param1 YY which included in predicate "IN XX YY" as the master fact. And param2's fact as the slave fact.
	
	#4. 
	
	#1. Find out the same object type *XX* in AT and *IN*,  consider it is of type XX.
	#2. Pick out first object of the type XX, which is OBJ_X. Then Generate all the facts for *AT OBJ_X ZZ* and *IN OBJ_X YY*.
				AT OBJ_X OZ1;AT OBJ_X OZ2;AT OBJ_X OZ3;
				IN OBJ_X OY1;IN OBJ_X OY2;IN OBJ_X OY3;IN OBJ_X OY4;
 */
typedef struct _dis_AccompanyGroup{
	dis_Fact *master_fact;
	dis_Fact *slave_fact;
	dis_Fact *relation_fact;
		
	int master_object_type;
	int slave_object_type;
	
	int same_param_master;
	int same_param_slave;
	
	int master_object[50];
	int master_object_num;
	int master_object_available[50];
	int *slave_object[50];
	int slave_object_num[50];
	int *slave_object_available[50];
	
	int valid;
}dis_AccompanyGroup;

extern dis_Invariant invariantGroup[200];
extern int num_invariantGroup;
extern dis_AccompanyGroup accompanyGroup[50];
extern int num_accompanyGroup;

/***********************************************************************************************************************
                                  				TRANSITIVE GRPAH 
***********************************************************************************************************************/

typedef struct _dis_TransitiveEdge{
	//int fromFact,toFact;
	int fromIndex,toIndex;
	int *action;
	int actionSize;
	
	int* commonPreconditionList;
	int  commonPreconditionListSize;
} dis_TransitiveEdge;

typedef struct _dis_transitiveNode{
	int *distance;
	int *weight;
	int *prev;
} dis_TransitiveNode;

typedef struct _dis_TranstiveGraph{
	int		order;
	int 	object;
	int* 	element;
	int 	elementSize;
	dis_TransitiveNode *node;
	
	int *dependencyList;
	int dependencyListSize;
	int *dependOnThisList;
	int dependOnThisListSize;
	
	dis_TransitiveEdge** edge;//This is a 2D array, you can access them by "edge[A][B]"	
	dis_TransitiveEdge** edgeList;//Set of all these edges
	int edgeListSize;
	
	int *distance;
	int  invariantId;
	int *reachable;
} dis_TransitiveGraph;

extern dis_TransitiveGraph transitiveGraph[3000];
extern int num_transitiveGraph;

extern int levelObject[200];
extern int levelObjectSize;
extern dis_State tg_init_state_backup;

/********************************************************************************************************************
											TRANSITIVE GRAPH DEFINITION END
********************************************************************************************************************/


void calc_invariants(void);
void build_transitive_graph(void);
int icmp( const void *p, const void *q );
int is_sub_invariant( dis_Invariant* invar1, dis_Invariant* invar2 );
void print_out_transitive_graph_set(void);
void print_out_invariant_result(void);
void print_out_invariant(dis_Invariant* group);
int get_state_distance(dis_State* evaluateState);
void dis_collect_A_cg_info( dis_State *source );
void dis_collect_H_cg_info( dis_State *source, int h );
int compare_level_object( int a, int b );
int get_level_index( int levelFact );
int get_acc_master_object_index_by_slave( dis_AccompanyGroup *group, int slave_object_id );
int get_acc_master_object_index_by_id( dis_AccompanyGroup *group, int master_object_id );
int acc_get_slave_object_avail( dis_AccompanyGroup *group, int slave_object_id);
int acc_init_object_availability( dis_AccompanyGroup *group );
void acc_mark_unavail_slave_object( dis_AccompanyGroup *group, int slave_object_id );
int get_single_graph_distance(int focus_object, dis_AccompanyGroup *acg, int init_prop_object, int goal_prop_object);
int get_valid_predicate_index(int predicateId);