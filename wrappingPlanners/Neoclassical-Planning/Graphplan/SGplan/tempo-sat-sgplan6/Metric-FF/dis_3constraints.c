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

 * File: dis_3constraints.c

 * Authors:  Chih-wei Hsu and Benjamin W. Wah

 *

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
#include "dis_ff.h"
#include "lpg.h"
#include "dis_parse.h"
#include "dis_memory.h"
#include "dis_output.h"
#include "dis_search.h"
#include "dis_expressions.h"
#include "dis_constraints.h"
#include "casual_graph.h"
#include "subspace.h"
#define MAX_CONSTRAINTS 10000
#define MAX_TEMPLATES 200000
#define MAXLEN_OF_GROUPLIST 10



typedef struct _GroupList
{
  int n, i[MAXLEN_OF_GROUPLIST];
} GroupList;
GroupList gl;
int **gtemplates;
int gnum_templates;

extern ConConn *dis_gcon_conn;
extern int dis_gnum_con_conn;

int traj_n;
int traj_plan[5000];
float traj_time[5000];

extern dis_State S0;
extern Bool subgoal_inc_planning(int, int);
extern void reduce_master_fact_in_state(int *, int *);
extern void construct_traj();
int depot_available[100];


float calculate_violation_storage(int *goal, int goal_num)
{

  int i, j, k, f, g;
  float v = 0;
  char temp[128], temp1[128];
     int one_cond_matched = 0;
     
  for (i=0;i<dis_gnum_con_conn;i++)
  {
    switch(dis_gcon_conn[i].connective)
    {
    case dis_SOMETIME_BEFORE_c:
    case dis_SOMETIME_AFTER_c:
		//printf("dis_SOMETIME_BEFORE_c or dis_SOMETIME_AFTER_c, ERROR HERE.\n");
    	break;
    case dis_AT_END_c:
    case dis_ALWAYS_c:
    for (j=0;j<dis_gcon_conn[i].num;j++)
    {
	one_cond_matched = 0;
      for (k=0;k<dis_gef_conn[dis_gcon_conn[i].cond[j]].num_PC;k++){
      	
        f = dis_gef_conn[dis_gcon_conn[i].cond[j]].PC[k];
        dis_print_ft_name_string(f, temp);
        if (strncmp(temp, "(dis_NOT-", strlen("(dis_NOT-")) == 0){
			for (g=0;g<goal_num;g++){
				if(goal == NULL ) printf("NULL!!!!!!!!!!!!!!!!!!!!!1\n");
				if (goal[g]==-1) continue;
	            dis_print_ft_name_string(goal[g], temp1);
				if (strcmp(temp+strlen("(dis_NOT-"), temp1+1) == 0)
					break;
			}
			if( g == goal_num ) break;
		}
		else
		{
			for (g=0;g<goal_num;g++)
            	if ( goal[g] == f )	break;
			if (g == goal_num) break;
        }
      }
      if (k == dis_gef_conn[dis_gcon_conn[i].cond[j]].num_PC)
        break;
    }
    if (j == dis_gcon_conn[i].num)
      v += dis_gcon_conn[i].weight;
    break;
    default:
    break;
    }
  }
  
  //printf("opt = %f: ", v);
 /*
  for (j=0;j<goal_num;j++)
      dis_print_ft_name(goal[j]);
  printf("\n");
  */
  return v;  
}


//If there is any element in a[0]... a[index-1],  exists in the same graph with a[index] then return true;
int check_overlap_graph(int *a, int index, int graphId){
	//Actually, this array *a* has elements of number index;
	int i,j,k;
	
	int graph_list[100];
	graph_list[0] = graphId;
	int graph_list_size = 1;
	for(i=0;i<num_transitiveGraph;i++){
		if ( i == graphId ) continue;
		dis_TransitiveGraph *graph = &(transitiveGraph[i]);
		for(j=0;j<graph->elementSize;j++)
			if(graph->element[j] == a[index]){
				graph_list[ graph_list_size++ ] = i;
				break;
			}
	}
	
	for(i=0;i<index;i++){
		for(j=0;j<graph_list_size;j++){
			dis_TransitiveGraph *graph = &(transitiveGraph[graph_list[j]]);
			for(k=0;k<graph->elementSize;k++)
				if ( a[i] == graph->element[k] )
					return 1;
		}
	}
	
	return 0;
}

void add_a_optimal_config(int *goal,int *goal_num, int *option, int option_num){
	int i;
	float optimal_value = 999999999;
	int optimal_index = 0;
	int gn = *goal_num;

	for(i=0;i<option_num;i++){
		goal[gn] = option[i];
		int v = calculate_violation_storage( goal, gn );
		if ( v<optimal_value ){
			optimal_index = i;
			optimal_value = v;
		}
	}
	if(optimal_index ==-1 ) printf("ERROR");
		else goal[gn++] = option[optimal_index];
	
	*goal_num = gn;
}


int get_object_list_index(int type_id, int object_id){
	int i;
	for(i=0;i<dis_gtype_size[type_id];i++)
		if(dis_gtype_consts[type_id][i]==object_id) return i; 
	return -1;
}


void get_optimal_config_1003sp(int *goal, int *goal_num){
	
	int i,j,k,m;
	//Assign the position of hoists.
	int transit_area_available[100];
	int depot_available[100];
	int container_available[100];
	int store_area_available[200];
	int TRANSIT_AREA_TYPE = 4;
	int STORE_AREA_TYPE = 11;
	int DEPOT_TYPE = 6;
	int CONTAINER_TYPE = 7;
	int option[200];
	int option_num = 0;
	
	for( i=0 ; i<100 ; i++) { transit_area_available[i] = 1;store_area_available[i]=1; }
	
	//NOW PUT THOSE CRATE IN DEPOT, or .... ... ;
	//BEGIN OF INITILIZATION;
	for(i=0;i<accompanyGroup[0].master_object_num;i++){
		int object_index = accompanyGroup[0].master_object[i];
		
		for( j=0 ; j<dis_gtype_size[DEPOT_TYPE] ; j++ )
			if( object_index == dis_gtype_consts[DEPOT_TYPE][j] )
				break;
			
		if ( j!=dis_gtype_size[DEPOT_TYPE] ){
			depot_available[j] = accompanyGroup[0].slave_object_num[i];	
			continue;
		}
		
		for(j=0;j<dis_gtype_size[CONTAINER_TYPE];j++)
			if( object_index == dis_gtype_consts[CONTAINER_TYPE][j] )
				break;
				
		if ( j!=dis_gtype_size[DEPOT_TYPE] ){
			container_available[j] = accompanyGroup[0].slave_object_num[i];	
			continue;
		}
	}//END OF AVAILABILITY INITILIZATION;
	
	//PUT CRATE INTO DEPOT, IF DEPOT ARE FULL, THEN CONTAINER;
	for(i=0;i<num_transitiveGraph;i++){
		option_num = 0;
		dis_TransitiveGraph *graph = &(transitiveGraph[i]);
		if ( graph->invariantId!= 4 ) continue;// If it is not about group AT HOIST1 XXXXX;
		
		for(j=0;j<graph->elementSize;j++){
			dis_Fact *fact = &(dis_grelevant_facts[graph->element[j]]);
			int area_object_id = fact->args[1];
			
			for(k=0;k<dis_gtype_size[DEPOT_TYPE];k++)
				if( area_object_id == dis_gtype_consts[DEPOT_TYPE][k] ) 
					break;
			
			if( k != dis_gtype_size[DEPOT_TYPE] && depot_available[k]>0 ){
				option[option_num++] = graph->element[j];
				depot_available[k]--;
				break;
			}
			
			for(k=0;k<dis_gtype_size[CONTAINER_TYPE];k++)
				if( area_object_id == dis_gtype_consts[CONTAINER_TYPE][k] ) 
					break;
			
			if( k!=dis_gtype_size[CONTAINER_TYPE] && container_available[k]>0 ){
				option[option_num++] = graph->element[j];
				container_available[k]--;
			}
		}
		
		assert(option_num > 0);
		add_a_optimal_config( goal , goal_num  , option , option_num );		
	}
	
	//First assign the location of hoist
	//Creteria: First assign them to the TRANSIT_AREA.
	/*
	for(i=0;i<num_transitiveGraph;i++){
		option_num = 0;
		dis_TransitiveGraph *graph = &(transitiveGraph[i]);
		if ( graph->invariantId!= 0 ) continue;// If it is not about group AT HOIST1 XXXXX;

		for( j=0 ; j<graph->elementSize ; j++ ){
			
			dis_Fact *fact = &(dis_grelevant_facts[graph->element[j]]);
			int area_object_id = fact->args[1];
			
			for( k=0 ; k<dis_gtype_size[TRANSIT_AREA_TYPE] ; k++ )
				if( area_object_id == dis_gtype_consts[TRANSIT_AREA_TYPE][k] )
					break;

			if( k!=dis_gtype_size[TRANSIT_AREA_TYPE] && transit_area_available[k] ){
				option[option_num++] = graph->element[j];
				transit_area_available[k] = 0;
				break;
			}

			//IF ALL THE TRANSIT POSITION are used up, then we have to try to locate hoist in store_area
			for( k=0 ; k<dis_gtype_size[STORE_AREA_TYPE] ; k++ )
				if( area_object_id == dis_gtype_consts[STORE_AREA_TYPE][k] ) 
					break;

			if( k != dis_gtype_size[STORE_AREA_TYPE] && store_area_available[k] ){
				option[option_num++] = graph->element[j];
				store_area_available[k] = 0;
				continue;
			}
		}
		add_a_optimal_config( goal , goal_num  , option , option_num );
	}
	*/	
}



void get_optimal_config_1003cp(int *goal, int *goal_num_ptr){
	
	int i,j,k,m;
	//Assign the position of hoists.
	int transit_area_available[100];
	int depot_available[100];
	int container_available[100];
	int store_area_available[200];
	int TRANSIT_AREA_TYPE = 4;
	int STORE_AREA_TYPE = 11;
	int DEPOT_TYPE = 6;
	int CONTAINER_TYPE = 7;
	int option[200];
	int option_num = 0;
	int goal_num = *goal_num_ptr;
	
	for( i=0 ; i<100 ; i++) { transit_area_available[i] = 1;store_area_available[i]=1; }
	
	//NOW PUT THOSE CRATE IN DEPOT, or .... ... ;
	//BEGIN OF INITILIZATION;
	for(i=0;i<accompanyGroup[0].master_object_num;i++){
		int object_index = accompanyGroup[0].master_object[i];
		
		for( j=0 ; j<dis_gtype_size[DEPOT_TYPE] ; j++ )
			if( object_index == dis_gtype_consts[DEPOT_TYPE][j] )
				break;
			
		if ( j!=dis_gtype_size[DEPOT_TYPE] ){
			depot_available[j] = accompanyGroup[0].slave_object_num[i];	
			continue;
		}
		
		for(j=0;j<dis_gtype_size[CONTAINER_TYPE];j++)
			if( object_index == dis_gtype_consts[CONTAINER_TYPE][j] )
				break;
				
		if ( j!=dis_gtype_size[DEPOT_TYPE] ){
			container_available[j] = accompanyGroup[0].slave_object_num[i];	
			continue;
		}
	}//END OF AVAILABILITY INITILIZATION;
	
	//PUT CRATE INTO DEPOT, IF DEPOT ARE FULL, THEN CONTAINER;
	int next_choice_id = -1;
	for(i=0;i<num_transitiveGraph;i++){
		option_num = 0;
		dis_TransitiveGraph *graph = &(transitiveGraph[i]);
		if ( graph->invariantId!= 4 ) continue;// If it is not about group AT HOIST1 XXXXX;
		
		for(j=0;j<graph->elementSize;j++){
			dis_Fact *fact = &(dis_grelevant_facts[graph->element[j]]);
			int area_object_id = fact->args[1];
			
			for( k=0 ; k<dis_gtype_size[DEPOT_TYPE] ; k++ )
				if( area_object_id == dis_gtype_consts[DEPOT_TYPE][k] )
					break;
			
			if( k != dis_gtype_size[DEPOT_TYPE] && depot_available[k]>0 ){
				if ( next_choice_id == -1 ){
					next_choice_id = k;	
				}
				if ( k != next_choice_id ) continue;
				//next_choice_id =  (next_choice_id + 1) % dis_gtype_size[DEPOT_TYPE] ;
				depot_available[k]--;
				goal[ goal_num++ ] = graph->element[j];
				break;
			}
				
			next_choice_id =  (next_choice_id + 1) % dis_gtype_size[DEPOT_TYPE] ;
			/*
			for(k=0;k<dis_gtype_size[CONTAINER_TYPE];k++)
				if( area_object_id == dis_gtype_consts[CONTAINER_TYPE][k] ) 
					break;
			
			if( k!=dis_gtype_size[CONTAINER_TYPE] && container_available[k]>0 ){
				option[option_num++] = graph->element[j];
				container_available[k]--;
			}
			*/
		}
		//add_a_optimal_config( goal , goal_num  , option , option_num );		
	}
	
	//First assign the location of hoist
	//Creteria: First assign them to the TRANSIT_AREA.
	/*
	for(i=0;i<num_transitiveGraph;i++){
		option_num = 0;
		dis_TransitiveGraph *graph = &(transitiveGraph[i]);
		if ( graph->invariantId!= 0 ) continue;// If it is not about group AT HOIST1 XXXXX;

		for( j=0 ; j<graph->elementSize ; j++ ){
			
			dis_Fact *fact = &(dis_grelevant_facts[graph->element[j]]);
			int area_object_id = fact->args[1];
			
			for( k=0 ; k<dis_gtype_size[TRANSIT_AREA_TYPE] ; k++ )
				if( area_object_id == dis_gtype_consts[TRANSIT_AREA_TYPE][k] )
					break;

			if( k!=dis_gtype_size[TRANSIT_AREA_TYPE] && transit_area_available[k] ){
				option[option_num++] = graph->element[j];
				transit_area_available[k] = 0;
				break;
			}

			//IF ALL THE TRANSIT POSITION are used up, then we have to try to locate hoist in store_area
			for( k=0 ; k<dis_gtype_size[STORE_AREA_TYPE] ; k++ )
				if( area_object_id == dis_gtype_consts[STORE_AREA_TYPE][k] ) 
					break;

			if( k != dis_gtype_size[STORE_AREA_TYPE] && store_area_available[k] ){
				option[option_num++] = graph->element[j];
				store_area_available[k] = 0;
				continue;
			}
		}
		add_a_optimal_config( goal , goal_num  , option , option_num );
	}
	*/
	*goal_num_ptr = goal_num;
}



void get_optimal_config_1003tc(int *goal, int *goal_num_ptr){
	
	int i,j,k,m;
	//Assign the position of hoists.
	int transit_area_available[100];
	int depot_available[100];
	int container_available[100];
	int store_area_available[200];
	int TRANSIT_AREA_TYPE = 11;
	int STORE_AREA_TYPE = accompanyGroup[0].slave_object_type;
	int DEPOT_TYPE = accompanyGroup[0].master_object_type;
	//printf("STORE_AREA_TYPE%d, DEPOT_TYPE%d\n", STORE_AREA_TYPE, DEPOT_TYPE);
	int CONTAINER_TYPE = 4;
	int option[200];
	int option_num = 0;
	int goal_num = *goal_num_ptr;
	
	for( i=0 ; i<100 ; i++) { transit_area_available[i] = 1;store_area_available[i]=1; }
	
	int hoist_goal[100];
	int hoist_goal_num;
	
	//NOW PUT THOSE CRATE IN DEPOT, or .... ... ;
	//BEGIN OF INITILIZATION;
	for(i=0;i<accompanyGroup[0].master_object_num;i++){
		int object_index = accompanyGroup[0].master_object[i];
		
		for( j=0 ; j<dis_gtype_size[DEPOT_TYPE] ; j++ )
			if( object_index == dis_gtype_consts[DEPOT_TYPE][j] )
				break;
			
		if ( j!=dis_gtype_size[DEPOT_TYPE] ){
			depot_available[j] = accompanyGroup[0].slave_object_num[i];	
			continue;
		}
		
		for(j=0;j<dis_gtype_size[CONTAINER_TYPE];j++)
			if( object_index == dis_gtype_consts[CONTAINER_TYPE][j] )
				break;
				
		if ( j!=dis_gtype_size[CONTAINER_TYPE] ){
			container_available[j] = accompanyGroup[0].slave_object_num[i];	
			continue;
		}
	}//END OF AVAILABILITY INITILIZATION;
	
	//printf("###[");for(i=0;i<dis_gtype_size[DEPOT_TYPE];i++)printf("%s-%d;",dis_gconstants[dis_gtype_consts[DEPOT_TYPE][i]],depot_available[i]);printf("]\n");
	
	//First assign the location of hoist
	for(i=0;i<num_transitiveGraph;i++){
		dis_TransitiveGraph *graph = &(transitiveGraph[i]);
		if ( graph->invariantId!= 4 ) continue;// If it is not about group AT HOIST1 XXXXX;

		for( j=0 ; j<graph->elementSize ; j++ ){
			dis_Fact *fact = &(dis_grelevant_facts[graph->element[j]]);
			int area_object_id = fact->args[1];
			
			for( k=0 ; k<dis_gtype_size[STORE_AREA_TYPE] ; k++ )
				if( area_object_id == dis_gtype_consts[STORE_AREA_TYPE][k] ) 
					break;

			if( k!=dis_gtype_size[STORE_AREA_TYPE] && store_area_available[k] ){
				hoist_goal[hoist_goal_num++] = graph->element[j];
				store_area_available[k] = 0;
				acc_mark_unavail_slave_object(&(accompanyGroup[0]),area_object_id);
				
				int depot_object_index = get_acc_master_object_index_by_slave(&(accompanyGroup[0]), area_object_id );
				int depot_object_id = accompanyGroup[0].master_object[depot_object_index];
				if (depot_object_id == -1 ) printf("critical error here. Can not find any depot as master object.\n");
				
				for(m=0;m<dis_gtype_size[DEPOT_TYPE];m++)
					if( dis_gtype_consts[DEPOT_TYPE][m] == depot_object_id )
						break;
				depot_available[m]--;
				break;
			}
		}
		
		if ( j != graph->elementSize) continue;
	}
	
	//printf("***###[");for(i=0;i<dis_gtype_size[DEPOT_TYPE];i++)printf("%s-%d;",dis_gconstants[dis_gtype_consts[DEPOT_TYPE][i]],depot_available[i]);printf("]\n");
	//printf("DEPOT SIZE:%d\n",dis_gtype_size[DEPOT_TYPE]);
	//printf("::");for(i=0;i<dis_gtype_size[DEPOT_TYPE];i++)printf("%d,",depot_available[i]);printf("]\n");
	//printf("::");for(i=0;i<dis_gtype_size[CONTAINER_TYPE];i++)printf("%d,",container_available[i]);printf("]\n");
	//PUT CRATE INTO DEPOT, IF DEPOT ARE FULL, THEN CONTAINER;
	for(i=0;i<num_transitiveGraph;i++){
		dis_TransitiveGraph *graph = &(transitiveGraph[i]);
		if ( graph->invariantId!= 4 ) continue;// If it is not about group IN CRATEXX DEPOTXX;
		
		for( j=0 ; j<graph->elementSize ; j++ ){
			dis_Fact *fact = &(dis_grelevant_facts[graph->element[j]]);
			
			int area_object_id = fact->args[1];

			for(k=0;k<dis_gtype_size[DEPOT_TYPE];k++)
				if( area_object_id == dis_gtype_consts[DEPOT_TYPE][k] )
					break;
			
			if( k != dis_gtype_size[DEPOT_TYPE] && depot_available[k]>0 ){
				goal[goal_num++] = graph->element[j];
				depot_available[k]--;
				break;
			}
		}
		
		if ( j != graph->elementSize ) continue;
		for(j=0;j<graph->elementSize;j++){
			dis_Fact *fact = &(dis_grelevant_facts[graph->element[j]]);
			int area_object_id = fact->args[1];
			
			for(k=0;k<dis_gtype_size[CONTAINER_TYPE];k++)
				if( area_object_id == dis_gtype_consts[CONTAINER_TYPE][k] ) 
					break;
			
			if( k!=dis_gtype_size[CONTAINER_TYPE] && container_available[k]>0 ){
				goal[goal_num++] = graph->element[j];
				container_available[k]--;
				break;
			}
		}
	}
	
	for(i=0;i<hoist_goal_num;i++)
		goal[goal_num++] = hoist_goal[i];
	*goal_num_ptr = goal_num;
}


void solve1003sp()
{
  int i, j;
  int *goal = (int *) malloc( num_transitiveGraph * 10 * sizeof(int) );
  int goal_num = 0;
  //mark_unreachable_elements();
  //printf("finish unreachable elements\n");
  get_optimal_config_1003sp(goal,&goal_num);
  saved_dis_gflogic_goal = (int *) realloc(saved_dis_gflogic_goal, dis_gnum_ft_conn*sizeof(int));
  /*
  for (i=0;i<num_transitiveGraph;i++)
    if (goal[i] != -1)
    {
      for (j=0;j<saved_dis_gnum_flogic_goal;j++)
        if (saved_dis_gflogic_goal[j] == goal[i])
          break;
      saved_dis_gflogic_goal[saved_dis_gnum_flogic_goal++] = goal[i];
    }
	*/
  	for(i=0;i<goal_num;i++)
  		saved_dis_gflogic_goal[saved_dis_gnum_flogic_goal++] = goal[i];
	//dis_gflogic_goal = (int *) realloc( dis_gflogic_goal, dis_gnum_ft_conn*sizeof(int));


	//printf("BEFORE TRNASFORMED:%d",saved_dis_gnum_flogic_goal);for( i=0;i<saved_dis_gnum_flogic_goal;i++ )dis_print_ft_name( saved_dis_gflogic_goal[i] );printf("\n");
    reduce_master_fact_in_state(saved_dis_gflogic_goal,&saved_dis_gnum_flogic_goal);
	//printf("TRNASFORMED STATE:");for( i=0;i<saved_dis_gnum_flogic_goal;i++)dis_print_ft_name(saved_dis_gflogic_goal[i]);printf("\n");
	//printf("PENALTY_B:%f\n",calculate_violation_storage(saved_dis_gflogic_goal,saved_dis_gnum_flogic_goal));

	bridge_option = 0;
	GpG.num_esp_sol = 0;
	subgoal_inc_planning(10000, 0);
	free(goal);
}


void solve1003cp()
{
  int i, j;
  int *goal = (int *) malloc( num_transitiveGraph * 10 * sizeof(int) );
  int goal_num = 0;
  get_optimal_config_1003cp( goal , &goal_num );

	saved_dis_gnum_flogic_goal=0;
	for(i=0;i<goal_num;i++)
		saved_dis_gflogic_goal[saved_dis_gnum_flogic_goal++] = goal[i];
  	saved_dis_gflogic_goal = (int *) realloc(saved_dis_gflogic_goal, dis_gnum_ft_conn*sizeof(int));		

	//printf("BEFORE TRNASFORMED:%d",saved_dis_gnum_flogic_goal);for( i=0;i<saved_dis_gnum_flogic_goal;i++)dis_print_ft_name(saved_dis_gflogic_goal[i]);printf("\n");
    reduce_master_fact_in_state(saved_dis_gflogic_goal,&saved_dis_gnum_flogic_goal);
	//printf("TRNASFORMED STATE:");for( i=0;i<saved_dis_gnum_flogic_goal;i++)dis_print_ft_name(saved_dis_gflogic_goal[i]);printf("\n");

	bridge_option = 0;
	GpG.num_esp_sol = 0;
	subgoal_inc_planning(10000, 100000);
	free(goal);
}

void solve1003qp()
{
  int i, j;
  int *goal = (int *) malloc( num_transitiveGraph * 10 * sizeof(int) );
  int goal_num = 0;
  get_optimal_config_1003cp( goal , &goal_num );

	for(i=0;i<goal_num;i++)
		saved_dis_gflogic_goal[saved_dis_gnum_flogic_goal++] = goal[i];
  	saved_dis_gflogic_goal = (int *) realloc(saved_dis_gflogic_goal, dis_gnum_ft_conn*sizeof(int));
    reduce_master_fact_in_state(saved_dis_gflogic_goal,&saved_dis_gnum_flogic_goal);

	bridge_option = 0;
	GpG.num_esp_sol = 0;
	subgoal_inc_planning(10000, 100000);
	free(goal);
}

void generate_intermidate_goal(int *goal, int *goal_num_ptr){

	int i,j,goal_num = *goal_num_ptr;
	int TYPE_ID = -1;
	int PREDICATE_ID = -1;
	int flag[1000];
	for(i=0;i<1000;i++)flag[i] = 1;
	dis_PrefNode *iter = NULL;
	dis_TypedList *type_list = NULL;
	dis_TokenList *token_list = NULL;
	
	for( iter=dis_gloaded_preferences ;iter;iter=iter->next){
		
		dis_ConNode *body = iter->body;
		if ( body->connective != dis_SOMETIME_c ) continue;

		type_list = iter->args;
		token_list = type_list->type;
		for( i=0 ; i<dis_gnum_types ; i++ )
			if ( strcmp( token_list->item , dis_gtype_names[i]) == 0  ){
				TYPE_ID = i;
				break;
			}

		dis_PlNode *node = body->sons;
		if ( node->connective != dis_EX ) continue;
		node = node->sons;
		for(i=0;i<dis_gnum_predicates;i++)
		if ( strcmp( node->atom->item, dis_gpredicates[i] ) == 0 ){
			PREDICATE_ID = i;
			break;
		}

		if ( PREDICATE_ID == -1 ) continue;
	}
	
	for(i=0;i<num_transitiveGraph;i++){
		dis_TransitiveGraph *graph = &(transitiveGraph[i]);	
		
		for(j=0;j<dis_gtype_size[TYPE_ID];j++)
			if( graph->object == dis_gtype_consts[TYPE_ID][j] )break;
		
		if ( j == dis_gtype_size[TYPE_ID] ) continue;
		
		for( j=0 ; j<graph->elementSize ; j++ ){
			dis_Fact *fact = &(dis_grelevant_facts[ graph->element[j] ]);
			int object = fact->args[1];
			
			if ( PREDICATE_ID == fact->predicate && flag[object] ){
				goal[goal_num++] = graph->element[j];
				flag[object] = 0;
				break;
			}
		}
	}
	*goal_num_ptr = goal_num;
	
	//printf("GENERATED GOAL:");
	//for(i=0;i<goal_num;i++)dis_print_ft_name(goal[i]);
	//printf("#\n");
}


void solve1003tc(){
	int i;
	int *goal = (int *) malloc( num_transitiveGraph * 10 * sizeof(int) );
	int goal_num = 0;
	int solu_len = 0;
	dis_State backup_state;
	backup_state.F = malloc( dis_ginitial_state.num_F*sizeof(int));
	backup_state.num_F = 0;
	for(i=0;i<dis_ginitial_state.num_F;i++){
		dis_Fact *fact	= &(dis_grelevant_facts[dis_ginitial_state.F[i]]);
		char temp[200];
		dis_print_dis_Fact_string(fact,temp);
		if ( strncmp(temp,"(_",strlen("(_")) == 0 ) continue;
		if ( strncmp(temp,"(dis_NOT",strlen("(dis_NOT"))==0 ) continue;
		backup_state.F[backup_state.num_F++] = dis_ginitial_state.F[i];
	}

	int solution[1000];

	//USE ALL THE HOIST

	generate_intermidate_goal(goal,&goal_num);
	saved_dis_gnum_flogic_goal = 0;	
  	for(i=0;i<goal_num;i++) saved_dis_gflogic_goal[saved_dis_gnum_flogic_goal++] = goal[i];
  	saved_dis_gflogic_goal = (int *) realloc(saved_dis_gflogic_goal, dis_gnum_ft_conn*sizeof(int));
	bridge_option = 0;
	GpG.num_esp_sol = 0;
	subgoal_inc_planning(10000, 100000);
	memcpy( solution, GpG.esp_solution, GpG.num_esp_sol * sizeof(int) );
	solu_len = GpG.num_esp_sol;	
	//print_initial_state();

	//PUT BACK ALL THE HOIST
	saved_dis_gnum_flogic_goal = 0;
	for(i=0;i<backup_state.num_F;i++) saved_dis_gflogic_goal[saved_dis_gnum_flogic_goal++] = backup_state.F[i];
  	saved_dis_gflogic_goal = (int *) realloc(saved_dis_gflogic_goal, dis_gnum_ft_conn*sizeof(int));	
  	//print_goal_state( saved_dis_gflogic_goal , saved_dis_gnum_flogic_goal );
	bridge_option = 0;
	GpG.num_esp_sol = 0;
	subgoal_inc_planning(10000, 100000);
	memcpy( solution+solu_len, GpG.esp_solution, GpG.num_esp_sol*sizeof(int) );
	solu_len += GpG.num_esp_sol;
	//print_initial_state();


	//START THE FORMAL PLANNING;
	goal_num = 0;
	saved_dis_gnum_flogic_goal = 0;		
	get_optimal_config_1003tc( goal , &goal_num );
	//saved_dis_gflogic_goal = (int *) realloc( saved_dis_gflogic_goal, dis_gnum_ft_conn*sizeof(int) );
  	for(i=0;i<goal_num;i++) saved_dis_gflogic_goal[saved_dis_gnum_flogic_goal++] = goal[i];
  	saved_dis_gflogic_goal = (int *) realloc(saved_dis_gflogic_goal, dis_gnum_ft_conn*sizeof(int));
    reduce_master_fact_in_state(saved_dis_gflogic_goal,&saved_dis_gnum_flogic_goal);
	bridge_option = 0;
	GpG.num_esp_sol = 0;
	subgoal_inc_planning(10000, 100000);
	memcpy( solution+solu_len, GpG.esp_solution, GpG.num_esp_sol*sizeof(int));
	solu_len += GpG.num_esp_sol;


	//FINALIZE THE PLANNING SEQUENCE;
	GpG.num_esp_sol = solu_len;
	memcpy( GpG.esp_solution, solution, solu_len*sizeof(int));
	free(goal);
}

int solve1003pro(){
	int i;
	//printf("IN SOLVE 10003PRO\n");
	saved_dis_gnum_flogic_goal = 0;
	for(i=0;i<dis_gnum_flogic_goal;i++) saved_dis_gflogic_goal[saved_dis_gnum_flogic_goal++] = dis_gflogic_goal[i];
	//printf("BEFORE TRNASFORMED:%d",saved_dis_gnum_flogic_goal);for( i=0;i<saved_dis_gnum_flogic_goal;i++)dis_print_ft_name(saved_dis_gflogic_goal[i]);printf("\n");	
	reduce_master_fact_in_state(saved_dis_gflogic_goal,&saved_dis_gnum_flogic_goal);
	//printf("TRNASFORMED STATE:");for( i=0;i<saved_dis_gnum_flogic_goal;i++)dis_print_ft_name(saved_dis_gflogic_goal[i]);printf("\n");	
	bridge_option = 0;
	GpG.num_esp_sol = 0;
	subgoal_inc_planning(10000, 100000);
}

int solve1003time(){
	int i;
	saved_dis_gnum_flogic_goal = 0;
	for(i=0;i<dis_gnum_flogic_goal;i++) saved_dis_gflogic_goal[saved_dis_gnum_flogic_goal++] = dis_gflogic_goal[i];
	//printf("BEFORE TRNASFORMED:%d",saved_dis_gnum_flogic_goal);for( i=0;i<saved_dis_gnum_flogic_goal;i++)dis_print_ft_name(saved_dis_gflogic_goal[i]);printf("\n");	
	reduce_master_fact_in_state(saved_dis_gflogic_goal,&saved_dis_gnum_flogic_goal);
	//printf("TRNASFORMED STATE:");for( i=0;i<saved_dis_gnum_flogic_goal;i++)dis_print_ft_name(saved_dis_gflogic_goal[i]);printf("\n");	
	bridge_option = 0;
	GpG.num_esp_sol = 0;
	subgoal_inc_planning(10000, 100000);
}

int model1003()
{
	int i;
	
	if (num_invariantGroup==0) calc_invariants();
	
	if (GpG.is_preferences || GpG.is_constraints)
	{
    	traj_n = 0;
    	dis_copy_dis_source_to_dest(&S0, &dis_ginitial_state);
    	dis_check_constraints(&dis_ginitial_state);
	}
  
  //printf("GpG.is_preferences%d, GpG.is_constraints%d, GpG.is_durative%d \n",GpG.is_preferences,GpG.is_constraints, GpG.is_durative);
  
	if ( GpG.is_preferences &&  GpG.is_constraints && GpG.is_durative){
  		solve1003cp();
  	}
  	else if (GpG.is_preferences &&  GpG.is_constraints && !GpG.is_durative){
  		solve1003qp();
  	}
  	else if ( GpG.is_preferences && !GpG.is_constraints &&  !GpG.is_durative){
  		solve1003sp();
  	}else if ( !GpG.is_preferences &&  GpG.is_constraints && GpG.is_durative){
		solve1003tc();
	}else if( !GpG.is_preferences && !GpG.is_constraints && !GpG.is_durative && !GpG.is_fluents ){
		solve1003pro();
	}else
		solve1003time();
	
	
     if (GpG.is_durative)
      construct_traj();
    else
    {
      traj_n = GpG.num_esp_sol;
      memcpy(traj_plan, GpG.esp_solution, traj_n*sizeof(int));
    }
    if (!dis_check_constraints(&dis_mff_sol))
    {
//      dis_print_dis_State(dis_mff_sol);
//      printf("\nINFEASIBLE FINAL STATE\n");
      exit(0);
    }   
//      dis_print_dis_State(dis_mff_sol);
//      printf("\nFINAL STATE\n");
 return 1;
}