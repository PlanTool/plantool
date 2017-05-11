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

 * File: dis_6constraints.c

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

#include <values.h>
#include <math.h>
#include "lpg.h"
#include "ff.h"
#include "LpgTime.h"
#include "check.h"
#include "numeric.h"
#include "ActionSubgraph.h"
#include "H_relaxed.h"
#include "H_max.h"
#include "utilities.h"
#include "LpgOutput.h"
#include "output.h"
#include <unistd.h>
#include <sys/types.h>
#include "esp.h" 
#include "subspace.h"
#include "LocalSearch.h"
#include "orderings.h"
#include "subfluent.h"
#include "dis_ff.h"
#include "dis_output.h"
#include "stripsff.h"
#include "search.h"
#include "dis_search.h"
#include "dis_inst_final.h"
#include "esp1.h"
#include "DistributeSearch.h"
#include "dis_constraints.h"
#include "casual_graph.h"

int **gtemplates;
int gnum_templates;

extern ConConn *dis_gcon_conn;
extern int dis_gnum_con_conn;
extern void reduce_master_fact_in_state(int *, int *);
extern void construct_traj();

int combined_fgoal_weight[1000];
int violation_facts[1000];
int violation_facts_num;

int violated_fact[100];
int violated_fact_num;

int first_time_evaluation = 1;
int *map = NULL;
int *pipe_size = NULL;
int area_num;
int pipe_num;
int TYPE_CO = -1;
int TYPE_AR = -1;
int TYPE_PI = -1;
int TYPE_FI = -1;
int TYPE_LA = -1;
int TYPE_FO = -1;
int TYPE_O = -1;
int final_goal[100];
int final_goal_num;

Bool subgoal_inc_planning_ruoyun(int h, int b)
{
   dis_Bool found_plan = TRUE;
    int j, k;
    
    max_hc_iter = h; 
    max_bfs_iter = b; 
   
    for (j = 0; j < saved_dis_gnum_flogic_goal; j++)
    {
        dis_gflogic_goal[j] = saved_dis_gflogic_goal[j]; 
        dis_gnum_flogic_goal = j+1;
        
        // R.Huang
        for(k=0;k<saved_dis_gnum_flogic_goal;k++){
        	if( k==j ) combined_fgoal_weight[k]= 1000;
        		else combined_fgoal_weight[k] = 1;
        }
        //R. Huang End
        
        // get known_iga_list
        dis_known_iga_list.num_F = 0;
        for(k=j+1; k<saved_dis_gnum_flogic_goal; k++) {
            dis_known_iga_list.F[dis_known_iga_list.num_F++] 
                = saved_dis_gflogic_goal[k];
        }
   
        bridge_option = 0;
        dis_mff_bridge();
        mff_record_esp_sol();
       
        if(dis_gnum_plan_ops>0) { 
            dis_source_to_dest(&dis_ginitial_state, &dis_mff_sol);
        }
        else
          if (j == saved_dis_gnum_flogic_goal - 1)
            return FALSE;
    }                                   
       
    //printf("\nnumeric part\n\n");
             
    for (j = 0; j < saved_dis_gnum_fnumeric_goal; j++)
    {
         
        dis_gnum_fnumeric_goal = j+1;
        dis_gfnumeric_goal_comp[j] = saved_dis_gfnumeric_goal_comp[j];
        dis_gfnumeric_goal_fl[j] = saved_dis_gfnumeric_goal_fl[j];
        dis_gfnumeric_goal_c[j] = saved_dis_gfnumeric_goal_c[j];
                                    
        dis_known_iga_list.num_F = 0;
        
        dis_mff_bridge();
        mff_record_esp_sol();
   
        if(dis_gnum_plan_ops>0)
            dis_source_to_dest(&dis_ginitial_state, &dis_mff_sol);
        else
          if (j == saved_dis_gnum_fnumeric_goal - 1)
            return FALSE;
    }
    return found_plan;
}

int get_object_index(int object_id, int type_id){
    int i;
    for( i=0 ; i<dis_gtype_size[type_id] ; i++ )
    if (dis_gtype_consts[type_id][i] == object_id )
        return i;
    return -1;
}

void build_map(){
    TYPE_CO = -1;
    TYPE_AR = -1;
    TYPE_PI = -1;
    int i,j;
    
    for(i=0;i<dis_gnum_types;i++){
        if( dis_gtype_names[i] == NULL ) continue;
        if( strncmp(dis_gtype_names[i],"AR",strlen("AR"))==0 )TYPE_AR = i;
        else if( strncmp(dis_gtype_names[i],"PI",strlen("PI"))==0 ) TYPE_PI = i;
        
    }
    
    for(i=0;i<dis_gnum_predicates;i++){
        if( dis_gpredicates[i]==NULL )continue; 
        if( strncmp(dis_gpredicates[i],"C",strlen("C")) == 0 ) TYPE_CO = i;
        else if ( strncmp(dis_gpredicates[i],"FI",strlen("FI")) == 0 ) TYPE_FI = i;
        else if ( strncmp(dis_gpredicates[i],"L",strlen("L")) == 0 ) TYPE_LA = i;
        else if ( strncmp(dis_gpredicates[i],"FO",strlen("FO")) == 0 ) TYPE_FO = i;
        else if ( strncmp(dis_gpredicates[i],"O",strlen("O")) == 0 ) TYPE_O = i;
    }
    
    //printf("TYPE_CO%d,FIRST%d,LAST%d,FOLLOW%d,ON%d\n",TYPE_CO,TYPE_FI,TYPE_LA,TYPE_FO,TYPE_O);    
    
    if( TYPE_CO == -1 || TYPE_AR == -1 || TYPE_PI== -1 ) {printf("CRITICAL ERROR"); return;}
    area_num = dis_gtype_size[TYPE_AR];
    pipe_num = dis_gtype_size[TYPE_PI];
    map = malloc( sizeof(int) * area_num * area_num );
    
    for(i=0;i<area_num;i++)
    for(j=0;j<area_num;j++){
        map[i*area_num+j] = -1;
        map[j*area_num+i] = -1;
    }
    
    for( i=0 ; i<dis_gnum_initial_predicate[TYPE_CO] ; i++ ){
        dis_Fact *fact = &(dis_ginitial_predicate[TYPE_CO][i]);
        
        int index1 = get_object_index(fact->args[0],TYPE_AR);
        int index2 = get_object_index(fact->args[1],TYPE_AR);
        map[index1*area_num+index2] = fact->args[2];
        map[index2*area_num+index1] = fact->args[2];
    }
}


//Find size of PIPE in the initial state;
int get_pipe_size(int pipe_id){
    int i;
    int first,last;

    for(i=0;i<dis_ginitial_state.num_F;i++){
        dis_Fact *fact = &(dis_grelevant_facts[dis_ginitial_state.F[i]]);
        if( fact->predicate == TYPE_FI && fact->args[1] == pipe_id ) first = fact->args[0];
        else if( fact->predicate==TYPE_LA && fact->args[1]== pipe_id ) last = fact->args[0];
    }
        
    int start_object = first;
    int size = 1;
    //printf("START CALCING:");
    while( start_object!=last ){
        //printf( "[%s]" , dis_gconstants[start_object] );
        for(i=0;i<dis_ginitial_state.num_F;i++){
            dis_Fact *fact = &(dis_grelevant_facts[dis_ginitial_state.F[i]]);
            if( fact->predicate == TYPE_FO && fact->args[1] == start_object ){
                start_object = fact->args[0];
                break;
            }
        }
        if ( i!= dis_ginitial_state.num_F ) size++;
            else break;
    }
    
    return size;
}

int get_location_object_count(int area_id, dis_State *state){
    int i;
    int count = 0;
    for(i=0;i<state->num_F;i++){
        dis_Fact *fact = &(dis_grelevant_facts[i]);
        if( fact->predicate == TYPE_O && fact->args[1]== area_id )
            count++;
    }
    return count;
}

//THE FACT IS OF TYPE_FO
//RETURN VALUE IS OF TYPE_P;
int get_hidden_location( dis_State *state , int f  ){
    int i;
    dis_Fact *fact = &(dis_grelevant_facts[f]);
    if( fact->predicate==TYPE_FI || fact->predicate==TYPE_LA )
        return fact->args[1];
    if( fact->predicate != TYPE_FO ) {printf("ERROR");return -1;}

    int next_object = fact->args[1];
    while(1){
        for(i=0;i<state->num_F;i++){
            dis_Fact *state_fact = &(dis_grelevant_facts[state->F[i]]); 
            if( state_fact->predicate==TYPE_FI && state_fact->args[0] == next_object ){
                return state_fact->args[1]; 
            }else if ( state_fact->predicate == TYPE_FO && state_fact->args[0] == next_object )
                next_object = state_fact->args[1];
        }
    }
    return -1;
}

void get_path_1006(int f1, int f2, int *path, int *path_len, dis_State *state){
    int len = 0,i,j;

    dis_Fact *fact1 = &(dis_grelevant_facts[f1]);
    dis_Fact *fact2 = &(dis_grelevant_facts[f2]);
    int from_index = -1;
    int to_index = -1;
    int is_o_flag = 0;
    int visited[500];
    int expand_list[500];
    int pre[500];   
    int expand_list_size = -1;
    
    if( fact1->predicate == TYPE_O ){
        //printf("IS_ON\n");
        from_index = get_object_index(fact1->args[1],TYPE_AR);
        to_index = get_object_index(fact2->args[1],TYPE_AR);
        //printf("from_index%d,to_index %d\n",from_index,to_index);
        is_o_flag = 1;
        expand_list[0] = from_index;
        expand_list_size = 1;
    }else{
        int location = get_hidden_location(state, f1);
        //printf("HIDDEN LOCATION%s",dis_gconstants[location]); 
        from_index = -1;
        to_index = get_object_index(fact2->args[1],TYPE_AR);
        
        for(i=0;i<dis_gnum_initial_predicate[TYPE_CO];i++){
            dis_Fact *fact = &(dis_ginitial_predicate[TYPE_CO][i]);
            if(fact->args[2]==location){
            //  printf("CHOSEN FACT:");dis_print_dis_Fact(fact);printf("\n");
                expand_list[0] = get_object_index(fact->args[0],TYPE_AR);
                expand_list[1] = get_object_index(fact->args[1],TYPE_AR);
                break;
            }
        }
        
        expand_list_size = 2;
        is_o_flag = 0;
    }

    for( i=0;i<area_num;i++){
        pre[i] = -1;
        visited[i] = 0;
    }
    do{
        for( i=0 ; i< expand_list_size ; i++)
            visited[ expand_list[i] ] = 1;

        int tmp_list[500];
        int tmp_list_size = 0;
        
        for( i=0 ; i<expand_list_size; i++)
        for( j=0 ; j<area_num ; j++ ){
                int v = map[expand_list[i]*area_num + j ];
                if ( v!= -1 &&  !visited[j]) {
                    //printf("SETTING VALUE FOR PRE[%d]\n",j);
                    pre[j] = expand_list[i];
                    tmp_list[tmp_list_size++] = j;
                }
        }

        expand_list_size = 0;
        for(i=0;i<tmp_list_size;i++)
            expand_list[ expand_list_size++ ] = tmp_list[i];
        
        
    }while(expand_list_size);
    
    int tmp[500];
    int tmp_size = 0;
    int start = to_index;
    
    while( start!= from_index && start!=-1 ){
        tmp[tmp_size++] = dis_gtype_consts[TYPE_AR][start];
        start = pre[start];
    }
    if(start!=-1)
        tmp[tmp_size++] = dis_gtype_consts[TYPE_AR][start];
    
    //In the tmp list, there are all objects;
    if( !is_o_flag ) path[len++] = f1;
    for(i=tmp_size-1;i>=0;i--){
        int object = tmp[i];
        for(j=0;j<dis_gnum_relevant_facts;j++){
            dis_Fact *fact = &(dis_grelevant_facts[j]);
            
            if ( fact->predicate == TYPE_O && fact->args[0] == fact2->args[0] && fact->args[1] == object)
                path[len++] = j;
        }
    }
    
    *path_len = len;
}

float get_pipe_cost(int pipe_id){
    int speed = 1;
    //return pipe_size[get_object_index(pipe_id,TYPE_PI)] / speed ;
    return pipe_size[get_object_index(pipe_id,TYPE_PI)];
}

void init(){
    build_map();
    int i;  
    pipe_size = malloc(sizeof(int) * pipe_num);
    for(i=0;i<pipe_num;i++)
        pipe_size[i] = get_pipe_size(dis_gtype_consts[TYPE_PI][i]);

    final_goal_num = 0;
    for(i=0;i<dis_gnum_flogic_goal;i++){
        dis_Fact *fact = &(dis_grelevant_facts[saved_dis_gflogic_goal[i]]);
        
        if ( fact->predicate == TYPE_O )
            final_goal[final_goal_num++] = saved_dis_gflogic_goal[i];
    }
        
    /*
    printf("##FINAL GOAL SET:");
    for(i=0;i<final_goal_num;i++){
        dis_print_ft_name(final_goal[i]);
    }printf("####\n");
    */
}

int get_mutex_fact(dis_State *state, int goal){
    
    int i;
    dis_Fact *gf = &(dis_grelevant_facts[goal]);
    for(i=0;i<state->num_F;i++){
        dis_Fact *fact = &(dis_grelevant_facts[state->F[i]]);
        if (  ( fact->predicate == TYPE_FO && (fact->args[0]==gf->args[0] || fact->args[1]==gf->args[0]) ) ||
               (fact->predicate == TYPE_O && fact->args[0] == gf->args[0] ) ||
               (fact->predicate == TYPE_LA && fact->args[0] == gf->args[0] ) ){
            return state->F[i];
            break;
        }
    }
    printf("CRITICAL ERROR. [O:110] \n");
    return -1;
}

int get_sub_goal(dis_State *state, int goal){
    int path[1000];
    int path_len=0;
    int i,j;
    
    dis_Fact *gf = &(dis_grelevant_facts[goal]);
    
    int start = get_mutex_fact( state , goal );
    
    //MAYBE START IS NOT A VALID START FACT;
    //printf("CALCING FROM:");dis_print_ft_name(start);printf(" to ");dis_print_ft_name(goal);printf("\n");
    
    get_path_1006( start, goal, path, &path_len, state );
    //printf("FOR GOAL:");dis_print_ft_name(goal);printf("\n");
    //printf("##PATH GOT:");for( i=0 ; i < path_len ; i++ ){printf("##");dis_print_ft_name(path[i]);}printf("\n");
    if(path_len>1)return path[1];
        
    printf("ERROR HERE.109\n");
    return -1;//INDICATES ERROR.
}

int get_exist_object_count(dis_State *state, int location){
    int i;
    int num = 0 ;
    for(i=0;i<state->num_F;i++){
        dis_Fact *fact = &(dis_grelevant_facts[state->F[i]]);
        if( fact->predicate == TYPE_O && fact->args[1] == location )
            num++;
    }
    return num;
}

int get_sub_distance( dis_State *state, int from_f, int to_f ){
    dis_Fact *from_fact = &(dis_grelevant_facts[from_f] );
    dis_Fact *to_Fact = &(dis_grelevant_facts[to_f] );
    
    int queue_start = -1;
    int queue_end = -1,i;
    int queue_forward = -1;
    int queue_backward = -1;
    for(i=0;i<state->num_F;i++){
        dis_Fact *fact = &(dis_grelevant_facts[state->F[i]]);
        if( fact->predicate == TYPE_FI )
            queue_start = state->F[i];
        else if (fact->predicate == TYPE_FI )
            queue_end = state->F[i];
        else if ( fact->predicate == TYPE_FO && fact->args[1] == to_Fact->args[0] )
            queue_forward = state->F[i];
        else if ( fact->predicate == TYPE_FO && fact->args[0] == to_Fact->args[0] )
            queue_backward = state->F[i];
    }
    
    if( queue_start!=-1 || queue_end != -1 ) return 1;
    
    int forward_count = 1, backward_count = 1;
    if( queue_forward == -1 || queue_backward == -1 ) {printf("STRANGE ERROR. E111.\n");return 0; }
    int loop_object = dis_grelevant_facts[queue_forward].args[1];
    
    while(1){
        for(i=0;i<state->num_F;i++){
            dis_Fact *fact = &(dis_grelevant_facts[state->F[i]]);
            if( fact->predicate == TYPE_FO && fact->args[0] == loop_object) {
                loop_object = fact->args[1];
                forward_count++;
                break;
            }
        }
        
        for(i=0;i<state->num_F;i++){
            dis_Fact *fact = &(dis_grelevant_facts[state->F[i]]);   
            if( fact->predicate == TYPE_FI && fact->args[0] == loop_object) break;
        }
        if( forward_count >1000 ){printf("CRITICAL ERROR. E112.\n");return 0;}      
        if( i != state->num_F ) break;
    }
    
    loop_object = dis_grelevant_facts[queue_backward].args[0];
    while(1){

        for(i=0;i<state->num_F;i++){
            dis_Fact *fact = &(dis_grelevant_facts[state->F[i]]);
            if( fact->predicate == TYPE_FO && fact->args[1] == loop_object ){
                loop_object = fact->args[0];
                backward_count++;
                break;  
            }
        }
        
        for(i=0;i<state->num_F;i++){
            dis_Fact *fact = &(dis_grelevant_facts[state->F[i]]);   
            if( fact->predicate == TYPE_LA && fact->args[0] == loop_object) break;
        }
        
        if( i != state->num_F ) break;
        if( backward_count >1000 ){printf("CRITICAL ERROR. E112.\n");return 0;}
    }
    
    int count = forward_count>backward_count ? backward_count : forward_count;

    
    if( from_fact->predicate != TYPE_O ){
        int hidden_location = get_hidden_location(state, from_f );
        int from_location = -1;
        for(i=0;i<dis_gnum_initial_predicate[TYPE_CO];i++){
            dis_Fact *fact = &(dis_ginitial_predicate[TYPE_CO][i]);
            if( fact->args[1] == to_Fact->args[1] && fact->args[2] == hidden_location ){
                from_location = fact->args[0];
                break;
            } 
        }
        
        int exist_num = get_exist_object_count( state, from_location );
        if( exist_num < count ) count += count - exist_num;
    }
    
    return count;
}

float evaluate_goal(dis_State *state, int goal){
    int path[1000];
    int path_len=0,i,j;
    for(i=0;i<state->num_F;i++)
        if( state->F[i]==goal ) return 0;   
    
    int start_fact = get_mutex_fact(state,goal);
    if( start_fact == goal ) return 0;
    get_path_1006( start_fact , goal , path , &path_len, state );
    //printf("##PATH GOT:");for( i=0 ; i < path_len ; i++ ){printf("##");dis_print_ft_name(path[i]);}
        
    float cost = 0;
    int start_location = 0;
    dis_Fact *start_ptr = &( dis_grelevant_facts[start_fact] );
    if( start_ptr->predicate != TYPE_O ){
        start_location = 1;
        cost += get_sub_distance(state, start_fact, path[1]);
        //printf("ADDING SUB..%d;",get_sub_distance(state, start_fact, path[1]));
    }
    
    for(i=start_location;i<path_len-1;i++)
    for(j=0;j<dis_gnum_initial_predicate[TYPE_CO];j++){
        dis_Fact *fact = &(dis_ginitial_predicate[TYPE_CO][j]);
        int from_loc = dis_grelevant_facts[path[i]].args[1];
        int to_loc = dis_grelevant_facts[path[i+1]].args[1];
        if( fact->args[0] == from_loc && fact->args[1] == to_loc )
            cost += get_pipe_cost(fact->args[2]) + 1;
            
            int task_size = get_pipe_size(fact->args[2]);
            int exist_num = get_exist_object_count( state, from_loc );
            if( exist_num < task_size ) cost += task_size - exist_num;
        }
    return cost;
}

/*
 *  
    state is the current state;
    For current_sub_goal, retrieve it from saved_glogic_goal[LAST_ONE];
    For All the finall_goal set, retrieve it from final_goal;
 *
 */
int evaluate_goal_set(dis_State *state){
    printf("*");
    if(first_time_evaluation){
        init();
        first_time_evaluation = 0;
    }
    
    int i;
    float result = 0;
    int curr_sub_goal = dis_gflogic_goal[dis_gnum_flogic_goal-1];
    //printf("[SG:");dis_print_ft_name(curr_sub_goal);printf("]");
    
    /*
    printf("##");printf("CURR_STAT:");
    for(i = 0;i<state->num_F;i++){
        dis_Fact *fact = &(dis_grelevant_facts[state->F[i]]);
        if( fact->predicate==TYPE_O || fact->predicate==TYPE_FI || fact->predicate==TYPE_LA )
            dis_print_ft_name(state->F[i]);
    }
    printf("##");
    */
    
    for(i=0;i<final_goal_num;i++){
        if(final_goal[i]==curr_sub_goal) result += evaluate_goal( state , curr_sub_goal ) * 1000;
            else result += evaluate_goal(state,final_goal[i]);
        //printf("[AFTER EVALUATE GOAL:");dis_print_ft_name(final_goal[i]);printf(",%f]\n",result);     
    }
    //printf("[COST%d]",(int)result);
    return (int)result;
}

int all_goal_achieved(dis_State *state){
    int i,j;
    
    if(state==NULL || state->num_F == 0 ) return 0;
    for(i=0;i<final_goal_num;i++){
    //printf("GOAL");dis_print_ft_name(state->F[i]);printf("#\n");
        for( j=0 ; j<state->num_F; j++){
            if( final_goal[i] == state->F[j] )
            break;
        }
        if ( j == state->num_F ) return 0;
    }
    //printf("CHECKING ALL GOALS(ALL ACHIEVED).\n");
    return 1;
}


void dis_collect_H_ps_info( dis_State *source){
    
  static dis_Bool first_call = dis_TRUE;
    
  int i,j;
  int* value = ( int * ) calloc( dis_gnum_op_conn+5, sizeof( int ) );
  int* ff_value = ( int * ) calloc( dis_gnum_op_conn+5, sizeof( int ) );
  if ( first_call ) {
    dis_gH = ( int * ) calloc( dis_gnum_op_conn+5, sizeof( int ) );
    dis_gnum_H = 0;
    first_call = dis_FALSE;
  }

  for ( i = 0; i < dis_gnum_H; i++ ) {
    dis_gop_conn[dis_gH[i]].is_in_H = dis_FALSE;
  }
  
  dis_gnum_H = 0;
    
    dis_State dest;
    dis_make_state( &dest, dis_gnum_ft_conn, dis_gnum_fl_conn );
    //dest.cg_value=-1;

    for (i=0;i<dis_gnum_op_conn;i++){
        
        dis_Bool result = dis_result_to_dest( &dest, source, i, -1 );
        if (result){
            dis_gH[dis_gnum_H] = i;
            //int cg_value = get_state_distance(&dest); 
            int cg_value = evaluate_goal_set(&dest);
            value[dis_gnum_H] = cg_value;
            //ff_value[dis_gnum_H] = dis_get_1P(&dest);
            dis_gnum_H++;
            //if ( h>cg_value ) break;
        }
    }
    
    for( i = dis_gnum_H-1 ; i>=0 ;i-- )
    for(j = 0; j < i; j++)
        if( value[j] > value[j+1] ){
            int tmp = value[j];
            value[j] = value[j+1];
            value[j+1] = tmp;
            
            tmp = dis_gH[j];
            dis_gH[j] = dis_gH[j+1];
            dis_gH[j+1] = tmp;
            
        }
    
    /*
    for(i=0;i<dis_gnum_H;i++){
        printf("%d;",value[i]);
    }
    printf("]\n");
    */
    free(value);
    free(ff_value);
}

extern int combined_fgoal_weight[1000];


int evaluate_combine_ff_schedule(dis_State *S){
    int i;
    int backup_goal_num = dis_gnum_flogic_goal;
    int *backup_goal = malloc( sizeof(int)*backup_goal_num );
    for(i=0;i<backup_goal_num;i++) backup_goal[i] = dis_gflogic_goal[i];
    
    dis_gnum_flogic_goal = 1;
    int h = 0;
    
    for (i=0;i<final_goal_num;i++){
        if( combined_fgoal_weight[i] == 1 ) continue;
        dis_gflogic_goal[0] = final_goal[i];
        int v = dis_get_1P(S);
        h += 1000 * v;          //This weight is initilized in inc_planning
    }
    
    free(dis_gflogic_goal);
    dis_gflogic_goal = malloc( sizeof(int) * (final_goal_num - 1));
    dis_gnum_flogic_goal = 0;
    for (i=0;i<final_goal_num;i++){
        if( combined_fgoal_weight[i] != 1 ) continue;
        dis_gflogic_goal[dis_gnum_flogic_goal++] = final_goal[i];
    }

    h+=dis_get_1P(S);
    dis_gnum_flogic_goal = backup_goal_num;
    free(dis_gflogic_goal);
    dis_gflogic_goal = malloc( sizeof(int) * dis_gnum_flogic_goal );
    for(i=0;i<dis_gnum_flogic_goal;i++) dis_gflogic_goal[i] = backup_goal[i];
    free(backup_goal);
    if ( h >= 1000 || dis_gnum_flogic_goal == final_goal_num ) return h;
        else return 0;
}

int evaluate_combine_ff(dis_State *S){
	
    int i,j;
    /*
    for(i=0;i<S->num_F;i++){
    	for(j=0;j<violation_facts_num;j++)
    		if( S->F[i] == violation_facts[j]) break;	
    	if(j<violation_facts_num) return 655359;
    }  */
    
    int backup_goal_num = dis_gnum_flogic_goal;
    int *backup_goal = malloc( sizeof(int)*backup_goal_num );
    for(i=0;i<backup_goal_num;i++) backup_goal[i] = dis_gflogic_goal[i];
    
    dis_gnum_flogic_goal = 0;
    dis_gflogic_goal = malloc( sizeof(int) * saved_dis_gnum_flogic_goal);    
    int h = 0;
    
    for (i=0;i<saved_dis_gnum_flogic_goal;i++){
        dis_gflogic_goal[dis_gnum_flogic_goal++] = saved_dis_gflogic_goal[i];
        if( combined_fgoal_weight[i] == 1000 ) break;;
    }
	int v = dis_get_1P(S);
	h += 1000 * v;          	//This weight is initilized in inc_planning
    
    dis_gnum_flogic_goal = 0;
    for (i=saved_dis_gnum_flogic_goal-1;i>=0;i--){
		if( combined_fgoal_weight[i] == 1000 ) break;;
        dis_gflogic_goal[dis_gnum_flogic_goal++] = saved_dis_gflogic_goal[i];
    }
	v = dis_get_1P(S);
	h+= v;
	
    dis_gnum_flogic_goal = backup_goal_num;
    free(dis_gflogic_goal);
    dis_gflogic_goal = malloc( sizeof(int) * dis_gnum_flogic_goal );
    for(i=0;i<dis_gnum_flogic_goal;i++) dis_gflogic_goal[i] = backup_goal[i];
    free(backup_goal);
    if ( h >= 1000 || dis_gnum_flogic_goal == final_goal_num ) return h;
        else return 0;
}

int dis_collect_cf_H_info(dis_State *source , int h){
  static dis_Bool first_call = dis_TRUE;
  
  int i,j;
  int* value = ( int * ) calloc( dis_gnum_op_conn+5, sizeof( int ) );
  int* ff_value = ( int * ) calloc( dis_gnum_op_conn+5, sizeof( int ) );
  if ( first_call ) {
    dis_gH = ( int * ) calloc( dis_gnum_op_conn+5, sizeof( int ) );
    dis_gnum_H = 0;
    first_call = dis_FALSE;
  }

  for ( i = 0; i < dis_gnum_H; i++ ) {
    dis_gop_conn[dis_gH[i]].is_in_H = dis_FALSE;
  }
  
  dis_gnum_H = 0;
    
    dis_State dest;
    dis_make_state( &dest, dis_gnum_ft_conn, dis_gnum_fl_conn );
    //dest.cg_value=-1;

    for (i=0;i<dis_gnum_op_conn;i++){
        
        dis_Bool result = dis_result_to_dest( &dest, source, i, -1 );
        if (result){
            dis_gH[dis_gnum_H] = i; 
            int cg_value = evaluate_combine_ff(&dest);
            value[dis_gnum_H] = cg_value;
            dis_gnum_H++;
            //if (  (h -cg_value)> 999) break;
        }
    }
    
    for( i = dis_gnum_H-1 ; i>=0 ;i-- )
    for(j = 0; j < i; j++)
        if( value[j] > value[j+1] ){
            int tmp = value[j];
            value[j] = value[j+1];
            value[j+1] = tmp;
            
            tmp = dis_gH[j];
            dis_gH[j] = dis_gH[j+1];
            dis_gH[j+1] = tmp;
            
        }
    
    /*
    for(i=0;i<dis_gnum_H;i++){
        printf("%d;",value[i]);
        //if( value[i] == 65540 ) {
            //ssssprintf("\n##");
            //print_out_state(source);
            //dis_print_op_name(dis_gH[i]);printf("##\n");}
    }
    printf("]\n");
    */
    free(value);
    free(ff_value);
}

/*
void solve3pro_mff(){
    int i,j;
    int value[1000];

    int backup_goal_num = dis_gnum_flogic_goal;
    int *backup_goal = malloc( sizeof(int)*backup_goal_num );
    for(i=0;i<backup_goal_num;i++)backup_goal[i] = dis_gflogic_goal[i];
    
    dis_gnum_flogic_goal = 1;
    for(i=0;i<saved_dis_gnum_flogic_goal;i++){
        
        for(j=0;j<saved_dis_gnum_flogic_goal;j++)
            if( i == j) combined_fgoal_weight[j] = 1000;
            else combined_fgoal_weight[j] = 1;
        
        dis_gflogic_goal[0] = saved_dis_gflogic_goal[i];
        int v = evaluate_combine_ff( &(dis_ginitial_state) );
        value[i] = v;
    }
    
    for( i = saved_dis_gnum_flogic_goal-1 ; i>0 ;i-- )
    for(j = 0; j < i - 1; j++)
        if( value[j] < value[j+1] ){
            int tmp = value[j];
            value[j] = value[j+1];
            value[j+1] = tmp;
            
            tmp = saved_dis_gflogic_goal[j];
            saved_dis_gflogic_goal[j] = saved_dis_gflogic_goal[j+1];
            saved_dis_gflogic_goal[j+1] = tmp;
            
        }
    
    free(dis_gflogic_goal);
    dis_gnum_flogic_goal = backup_goal_num;
    dis_gflogic_goal = malloc( sizeof(int) * dis_gnum_flogic_goal );
    for(i=0;i<dis_gnum_flogic_goal;i++) dis_gflogic_goal[i] = backup_goal[i];
    
    bridge_option = 0;
    GpG.num_esp_sol = 0;
    subgoal_inc_planning_ruoyun(10000, 100000);
    free(backup_goal);
}  
*/


void solve3pro_mff(){
    printf("mff\n");
    int value[1000],i,j,achieved[1000];
    int sol_len = 0, solution[1000];
    init();
    
    final_goal_num = saved_dis_gnum_flogic_goal;
    for(i=0;i<final_goal_num;i++){
        final_goal[i] = saved_dis_gflogic_goal[i];
        achieved[i] = 0;
    }
    
    saved_dis_gnum_flogic_goal = 0;
    while(!all_goal_achieved(&dis_mff_sol)){
        //printf("New iteration.\n");
        int backup_goal_num = dis_gnum_flogic_goal;
        int *backup_goal = malloc( sizeof(int)*backup_goal_num );
        for(i=0;i<backup_goal_num;i++)backup_goal[i] = dis_gflogic_goal[i];
        
        dis_gnum_flogic_goal = 1;
        for(i=0;i<final_goal_num;i++){
            
            for(j=0;j<final_goal_num;j++)
                if( i == j) combined_fgoal_weight[j] = 1000;
                else combined_fgoal_weight[j] = 1;
            
            dis_gflogic_goal[0] = final_goal[i];
            int v = evaluate_combine_ff_schedule( &(dis_mff_sol) );
            value[i] = v;
        }
    
        int max_value = 0;
        int max_index = 0;
        for(i=0;i<final_goal_num;i++){
            if(achieved[i]) continue;
            if( value[i] > max_value ){
                max_value = value[i];
                max_index = i;  
            }
        }
        if(max_value ==0 ) {break;}
        
        free(dis_gflogic_goal);
        dis_gnum_flogic_goal = backup_goal_num;
        dis_gflogic_goal = malloc( sizeof(int) * (dis_gnum_flogic_goal + 1));
        for(i=0;i<dis_gnum_flogic_goal;i++) dis_gflogic_goal[i] = backup_goal[i];
        
        int sub_goal = get_sub_goal(&dis_mff_sol,final_goal[max_index]);
        //printf("GENERATED SUB GOAL:");dis_print_ft_name(sub_goal);printf("\n");
        if( sub_goal == final_goal[max_index] ){ 
            //printf("FINAL_SUBGOAL_ACHIEVED.\n");
            achieved[max_index] = 1; 
        }
        
        dis_Fact *sub_goal_fact = &(dis_grelevant_facts[sub_goal]);
        for(i=0;i<saved_dis_gnum_flogic_goal;i++){
            dis_Fact *gfact = &(dis_grelevant_facts[saved_dis_gflogic_goal[i]]);
            if ( gfact->args[0] == sub_goal_fact->args[0]  )
            break; 
        }
        
        saved_dis_gflogic_goal[i] = sub_goal;
        
        if(i==saved_dis_gnum_flogic_goal) saved_dis_gnum_flogic_goal++;
        else{
            int tmp = saved_dis_gflogic_goal[i];
            saved_dis_gflogic_goal[i] = saved_dis_gflogic_goal[saved_dis_gnum_flogic_goal-1];
            saved_dis_gflogic_goal[saved_dis_gnum_flogic_goal-1] = tmp;         
        }
        
        bridge_option = 1;
        GpG.num_esp_sol = 0;
        //printf("\n####");for(i=0;i<saved_dis_gnum_flogic_goal;i++)dis_print_ft_name(saved_dis_gflogic_goal[i]); printf("####\n");
        subgoal_inc_planning_ruoyun(10000, 100000);
        //printf("\n####");for(i=0;i<dis_mff_sol.num_F;i++)dis_print_ft_name(dis_mff_sol.F[i]); printf("####\n");       
        memcpy(solution+sol_len, GpG.esp_solution, GpG.num_esp_sol*sizeof(int));
        sol_len += GpG.num_esp_sol;     
    }
    
    GpG.num_esp_sol = sol_len;
    memcpy(GpG.esp_solution, solution, sol_len*sizeof(int));
}





/*
void solve1006tc(){
    printf("5Constraints.c-->solve1006tc\n");
    int i,j;
    
    int goal_evaluation[100];
    int solution[100];
    int sol_len = 0;
    float deadline[1000];
    violation_facts_num = 0;
    
   printf("7@");for(i=0;i<saved_dis_gnum_flogic_goal;i++) dis_print_ft_name(saved_dis_gflogic_goal[i]);printf("#\n");
    //Save all the ALWAYS_WITHIN facts into the violation list
	for ( i=0 ; i<dis_gnum_con_conn ; i++ ){
		int k = dis_gcon_conn[i].cond[0];
		printf("  #:");dis_print_ft_name(dis_gef_conn[k].PC[0]);printf("\n");           
		if (dis_gcon_conn[i].connective == dis_ALWAYS_WITHIN_c && dis_gef_conn[k].num_PC==1){
			for(j=0;j<violation_facts_num;j++)if( violation_facts[j] == dis_gef_conn[k].PC[0]) break;
			if ( j == violation_facts_num ) violation_facts[violation_facts_num++] = dis_gef_conn[k].PC[0];
		}
	}    
    for(i=0;i<violation_facts_num;i++)dis_print_ft_name(violation_facts[i]);printf("\n");
    	printf("6@");for(i=0;i<saved_dis_gnum_flogic_goal;i++) dis_print_ft_name(saved_dis_gflogic_goal[i]);printf("#\n");

    for(j=0;j<saved_dis_gnum_flogic_goal;j++){
        int f_id = saved_dis_gflogic_goal[j];
        for ( i=0 ; i<dis_gnum_con_conn ; i++ ){
            int k = dis_gcon_conn[i].cond[0];
            if (dis_gcon_conn[i].connective == dis_WITHIN_c && dis_gef_conn[k].num_PC==1 && dis_gef_conn[k].PC[0] == f_id)
                deadline[j] = dis_gcon_conn[i].time1;
        }
    }
    
    printf("3@");for(i=0;i<saved_dis_gnum_flogic_goal;i++) dis_print_ft_name(saved_dis_gflogic_goal[i]);printf("#\n");        
    for( i = saved_dis_gnum_flogic_goal-1 ; i>=0 ;i-- )
    for(j = 0; j < i - 1; j++)
        if( deadline[j] < deadline[j+1] ){
            int tmp = deadline[j];
            deadline[j] = deadline[j+1];
            deadline[j+1] = tmp;
            
            tmp = dis_gflogic_goal[j];
            saved_dis_gflogic_goal[j] = saved_dis_gflogic_goal[j+1];
            saved_dis_gflogic_goal[j+1] = tmp;
        }
    
	printf("2@");for(i=0;i<saved_dis_gnum_flogic_goal;i++) dis_print_ft_name(saved_dis_gflogic_goal[i]);printf("#\n");        
    //Save all the goals in another array, because saved_dis_gflogic is useful while doing the plan.
    final_goal_num = 0;
    for(i=0;i<saved_dis_gnum_flogic_goal;i++) final_goal[final_goal_num++] = saved_dis_gflogic_goal[i];
    
	dis_State saved_dis_ginitial_state;
	dis_copy_dis_source_to_dest(&saved_dis_ginitial_state, &dis_ginitial_state);
	
	printf("1@");for(i=0;i<saved_dis_gnum_flogic_goal;i++) dis_print_ft_name(saved_dis_gflogic_goal[i]);printf("#\n");
	int count = 0;
    while(1){
    	count++;
    	//printf("ITERATION%d\n",count);
//    	printf("@");for(i=0;i<saved_dis_gnum_flogic_goal;i++) dis_print_ft_name(saved_dis_gflogic_goal[i]);printf("#\n");
    	
    	dis_copy_dis_source_to_dest(&dis_ginitial_state, &saved_dis_ginitial_state);
		bridge_option = 0;
		GpG.num_esp_sol = 0;
		subgoal_inc_planning_ruoyun(10000, 100000);
		
		if (GpG.is_durative)
			construct_traj();
		else
		{
			traj_n = GpG.num_esp_sol;
			memcpy(traj_plan, GpG.esp_solution, traj_n*sizeof(int));
		}
    	if (!dis_check_constraints(&dis_mff_sol)){
    		printf("QQ%d\n",violated_fact_num);
    		for(i=0;i<saved_dis_gnum_flogic_goal;i++)
    			if( violated_fact[violated_fact_num-1] == saved_dis_gflogic_goal[i] ) break;	
    		if( i == 0 ) {printf("IMPOSSIBLE.");break;}
    		printf("i%d,saved_dis_gnum_flogic_goal%d",i,saved_dis_gnum_flogic_goal);
    		if( i < saved_dis_gnum_flogic_goal ){
				int tmp = saved_dis_gflogic_goal[i];
				saved_dis_gflogic_goal[i] = saved_dis_gflogic_goal[i-1];
				saved_dis_gflogic_goal[i-1] = tmp;
				continue;
    		}
    	}
    }
    
    // Do the planning
    saved_dis_gnum_flogic_goal = 0;
    for(i=0;i<final_goal_num;i++){
    	int current_goal = final_goal[i];
    	
    	
    }
    
    bridge_option = 0;
    mff_subgoal();
}
*/

void solve1006tc(){
	int i,j;		
	for ( i=0 ; i<dis_gnum_con_conn ; i++ ){
		int k = dis_gcon_conn[i].cond[0];
		if (dis_gcon_conn[i].connective == dis_ALWAYS_WITHIN_c && dis_gef_conn[k].num_PC==1){
			for(j=0;j<violation_facts_num;j++)if( violation_facts[j] == dis_gef_conn[k].PC[0]) break;
			if ( j == violation_facts_num ) violation_facts[violation_facts_num++] = dis_gef_conn[k].PC[0];
		}
	}
			
	bridge_option = 1;
	GpG.num_esp_sol = 0;
	mff_subgoal();
//	subgoal_inc_planning_ruoyun(10000, 100000);
}


int model3(State * start_state, State * end_state, PlanAction ** plan_actions){
    
	bridge_option = 0;
	GpG.num_esp_sol = 0;
	mff_subgoal();
	//printf("ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ\n");
	return 1;
    /*
    if( !GpG.is_preferences && !GpG.is_constraints && !GpG.is_durative && !GpG.is_fluents ){
        solve3pro_mff();
    }else
        solve3time();
*/
}


void solve1006cp(){
    int i,j;
    
    int goal_evaluation[100];
    int solution[100];
    int sol_len = 0;
    violation_facts_num = 0;
    
    int goal[100];
    int goal_num = 0;
    float deadline[1000];
    for(j=0;j<dis_gnum_flogic_goal;j++) deadline[j] = 65535;
    
    for(j=0;j<saved_dis_gnum_flogic_goal;j++){
        int f_id = saved_dis_gflogic_goal[j];
        for ( i=0 ; i<dis_gnum_con_conn ; i++ ){
            int k = dis_gcon_conn[i].cond[0];
            if (dis_gcon_conn[i].connective == dis_WITHIN_c && dis_gef_conn[k].num_PC==1 && dis_gef_conn[k].PC[0] == f_id)
                deadline[j] = dis_gcon_conn[i].time1;
        }
    }
    
    //printf("@");for(i=0;i<saved_dis_gnum_flogic_goal;i++) dis_print_ft_name(saved_dis_gflogic_goal[i]);printf("#\n");
    for( i = saved_dis_gnum_flogic_goal-1 ; i>=0 ;i-- )
    for(j = 0; j < i; j++)
        if( deadline[j] > deadline[j+1] ){
            int tmp = deadline[j];
            deadline[j] = deadline[j+1];
            deadline[j+1] = tmp;
            
            tmp = saved_dis_gflogic_goal[j];
            saved_dis_gflogic_goal[j] = saved_dis_gflogic_goal[j+1];
            saved_dis_gflogic_goal[j+1] = tmp;
        }
	//printf("@");for(i=0;i<saved_dis_gnum_flogic_goal;i++) dis_print_ft_name(saved_dis_gflogic_goal[i]);printf("#\n");
	
	bridge_option = 0;
	GpG.num_esp_sol = 0;
	subgoal_inc_planning_ruoyun(10000, 100000);
}



int model1006()
{
  int nr_g, goal[100];
  
  if (GpG.is_preferences || GpG.is_constraints)
  {
    traj_n = 0;
    dis_copy_dis_source_to_dest(&S0, &dis_ginitial_state);
    dis_check_constraints(&dis_ginitial_state);
//  dis_print_dis_State(dis_ginitial_state);
//  printf("\nINITIAL STATE\n");
  }

  GpG.num_esp_sol = 0;
  dis_source_to_dest(&dis_mff_sol, &dis_ginitial_state);
    
      
    if (GpG.is_preferences && GpG.is_constraints && GpG.is_durative ){
        solve1006cp();		//cp
    }else if ( !GpG.is_preferences &&  GpG.is_constraints && GpG.is_durative ){
        solve1006tc();		//tc
    }
    
  if (GpG.is_preferences || GpG.is_constraints)
  {
    if (GpG.is_durative)
      construct_traj();
    else
    {
      traj_n = GpG.num_esp_sol;
      memcpy(traj_plan, GpG.esp_solution, traj_n*sizeof(int));
    }
    if (!dis_check_constraints(&dis_mff_sol))
    {
//		dis_print_dis_State(dis_mff_sol);
//		printf("\nINFEASIBLE FINAL STATE\n");
		exit(0);
    }
      //dis_print_dis_State(dis_mff_sol);
      //printf("\nFINAL STATE\n");
  }

  return 1;
}
