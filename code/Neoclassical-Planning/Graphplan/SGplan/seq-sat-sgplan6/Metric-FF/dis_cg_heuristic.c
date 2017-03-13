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

 * File: dis_cg_heuristic.c

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
#include "dis_memory.h"
#include "casual_graph.h"
#include "dis_output.h"

const int  INF = 65535;
int CG_HEURISTIC_CUTOFF_UPPER_VALUE = 30;
int CG_HEURISTIC_CUTOFF_BUTTOM_VALUE = 15;

dis_State *localInitState = NULL;
int *recurMark;
int stateOperationCount = 0;
int applyOperationCount = 0;
int depth = 0;


int heuristicLevel = 0;
int majorObject;

//For Debug
int searchedNodes[10];
int failCount[10];
int mallocStateCount=0;

//END OF FOR DEBUG

typedef struct _dis_ArraySet{
	int cell[200];
	int arraySize;
} dis_ArraySet;

void init_set( dis_ArraySet *set, int arraySize){
	int i;
	set->arraySize = arraySize;
	for( i = 0 ; i < arraySize ; i++) set->cell[i] = 0;
}

void add_to_set( dis_ArraySet *set,int value){
	set->cell[value] = 1;
}

void print_set(dis_ArraySet *Q){
	int i;
	for(i=0;i<Q->arraySize;i++) 
		if( Q->cell[i]==1 ) printf("%d,",i);
	printf("\n");	
}

void print_out_state(dis_State* s){
	int i;
	printf(" STATE:");
	if (s == NULL ) {
		printf("NULL State!!\n");
		return;
	}else if( s->F == NULL ){
		printf("NO FACT IN STATE; INVALID STATE!\n");
		return;	
	}
	
	for( i = 0 ; i < s->num_F ; i++ ){
		printf("%d",s->F[i]);
		dis_print_ft_name(s->F[i]);
		printf(";");
	}
	printf("\n");
}

int get_set_size( dis_ArraySet *set){	
	int size = 0,i;
	for( i = 0 ; i < set->arraySize ; i++ )
		if( set->cell[i]==1 ) size++; 
	return size;
}


/*
	Check whether a **fact** is in the given group or not.
	If in then return the pos in the array[0..n-1];
		else if *not* then return -1;
*/
int get_node_index(int factId, int groupId){
	int *pos = bsearch(&factId, transitiveGraph[groupId].element , transitiveGraph[groupId].elementSize, sizeof(int), icmp );
	if( pos!= NULL) return (pos-transitiveGraph[groupId].element);
		else return -1;
}


void print_space(){
	int i;
	for( i = 0; i<depth ; i++ ) printf("   ");
}

void print_out_route(int *prev, int *weight, int goal, int groupId){
	
	dis_TransitiveGraph *graph = &(transitiveGraph[groupId]);
	int *sequence = malloc( sizeof(int) * graph->elementSize );
	int sequenceSize = 0;
	
	while(goal!=-1){
		sequence[sequenceSize] = goal;
		sequenceSize++;
		goal  = prev[goal];
	}	
	
	int i;
	print_space();
	printf("SEQUENCE:");
	for( i = 0 ; i<sequenceSize ; i++ ) {
		dis_print_ft_name(transitiveGraph[groupId].element[sequence[i]]);
		printf("<--%d--",weight[i]);
	}
	free(sequence);
	printf("\n");
}

void print_out_edge(dis_TransitiveEdge *edge, dis_TransitiveGraph *graph, int weight){
	printf("[EDGE:");
	dis_print_ft_name( graph->element[ edge->fromIndex ]);
	printf("  ->  ");
	dis_print_ft_name( graph->element[ edge->toIndex ] );
	printf("] is of weight%d\n",weight);
}

int calc_fact_distance(int fromIndex, int toIndex, int groupId );

dis_State *clone_state(dis_State *original){
	
	stateOperationCount++; 
	mallocStateCount++;
	dis_State *result = malloc( sizeof(dis_State) );
	int k;
	result->num_F = original->num_F;
	result->F = malloc( sizeof(int)* result->num_F );
	for(k=0;k<result->num_F;k++)
		result->F[k] = original->F[k];
	return result;
}

void free_state(dis_State *state){
	if(state==NULL) return;
	mallocStateCount--;	
	if ( state->F != NULL ) free(state->F);
	state->F = NULL;
	if ( state!=NULL ) free(state);
	state = NULL;
}

int get_fact_distance_from_init(int factId, dis_TransitiveGraph* graph){
	int i,j;
	int min = INF;
	
	//if (depth<3){ print_space();printf("ENTERING GET_FACT_DISTANCE_FROM_INIT\n"); }
	for( i = 0 ; i < graph->dependencyListSize; i++ ){
		int dependedGroup = graph->dependencyList[i];
		for( j = 0 ; j < localInitState->num_F ; j++ ){
			int toIndex = get_node_index( factId , dependedGroup );
			if ( toIndex == -1 ) continue;
			
			int fromIndex = get_node_index( localInitState->F[j], dependedGroup);
			if( fromIndex == -1 ) continue;
			if ( fromIndex==toIndex ) return 0;

			int distance = calc_fact_distance( fromIndex , toIndex , dependedGroup);
			//if( distance < INF ) if (depth<3) { print_space(); printf("LEAVING GET_FACT_DISTANCE_FROM_INIT\n");}
			if( distance < INF ) return distance;   //***IMPORTANT*** Here I has a assumption, and I suppose only one matches between factId and all the fact in localInitState
			//if ( min > distance ) min = distance;
		}
	}
	//if (depth<3){ print_space(); printf("LEAVING GET_FACT_DISTANCE_FROM_INIT\n");}
	return min;
}

//check whether groupId1 depends on groupId2 or not;
int is_depend_on(int groupId1,int groupId2){
	int *pos = bsearch(&groupId2, transitiveGraph[groupId1].dependencyList , transitiveGraph[groupId1].dependencyListSize, sizeof(int), icmp );
	if ( pos!= NULL ) return 1;
		else return 0;
}

void get_ordered_conditions( int *array, dis_EfConn* efConn){
	int i,j;
	for( i = 0 ; i< efConn->num_PC ;i++ )
		array[i] = efConn->PC[i];
	
	for( i = efConn->num_PC-1 ; i>0 ;i-- )
   	for(j = 0; j < i; j++){
		if( is_depend_on(array[j+1],array[j]) ){
			int tmp = array[j];
			array[j] = array[i];
			array[i] = tmp;
		}
   	}
}

int merge_array(int *array1, int array1Size, int *array2, int array2Size, int *array){
  int i=0,j=0;
  int len = 0;
  while( i<array1Size && j<array2Size ){
  	if (array1[i]<0){ i++;continue;}
  	if (array2[j]<0){ j++;continue;}
  	
    if( array1[i] < array2[j])
      array[len++] =array1[i++];
    else if (array1[i]>array2[j]) array[len++]=array2[j++];
    else j++;
}
	
  while(i<array1Size){
  	if(array1[i]<0){ i++;continue;}
    array[len++]=array1[i++];
  }
  while(j<array2Size){
  	if(array2[j]<0) {j++;continue;}
    array[len++]=array2[j++];
  }
  return len;
}

int check_array_order(int *array, int arraySize){
	int i;
	for( i=1;i<arraySize;i++)
		if (array[i-1]>array[i]) return 0;
	return 1;
}


/*
 *
 */
int get_major_object(int actionId){
	int majorType = 7,i,j;
	
	dis_Action *a = dis_gop_conn[actionId].action;
	for ( i = 0; i < a->num_name_vars; i++ ) {
		int object = a->name_inst_table[i];
		
		int flag = 0;
		for ( j = 0; j < dis_gtype_size[majorType]; j++ ) 
		if ( dis_gtype_consts[majorType][j] == object ){
			flag = 1;
			break;	
		}
		
		if(flag)
			return object;
    }
    return -1;
}


/*****************************************************************************************
 ******						is_valid_action()                                      *******
 *****************************************************************************************/
int is_invalid_action(int actionId, int object ){
	if ( majorObject==-1 || object== -1 ) return 0;
	
	if ( object == majorObject ) return 0;
		else return 1;
}

int get_level_id(dis_Fact *precond){
	int i;
	for( i=0 ; i<localInitState->num_F ; i++ ){
		dis_Fact* fact = &(dis_grelevant_facts[localInitState->F[i]]);
		if( fact->predicate!= precond->predicate ) continue;

		if ( precond->predicate==4 || precond->predicate==6 || precond->predicate==7 ){
			if( (fact->args)[0]==(precond->args)[0] && (fact->args)[1]==(precond->args)[1] ) return (fact->args)[2];
		}else if ( precond->predicate==5 ){
			if( (fact->args)[0]==(precond->args)[0] ) return (fact->args[1]);
		}
	}
	return -1;
}

/*
 * Three kinds of fact: ON-SALE, READY-TO-LOAD, LOADED;
 * For these three kinds of facts, there are LEVEL property in it, it should NEVER be larger than which is in current state
   One kind of fact:STORED
   It should never be less than current state
 */
int is_invalid_precondition(int factId, dis_TransitiveEdge *edge , dis_TransitiveGraph *graph){
	int *pos = bsearch(&factId, localInitState->F , localInitState->num_F, sizeof(int), icmp );
	if ( pos!=NULL ) return 0;
	//printf("FACT:");dis_print_ft_name(factId);printf("UPPEDGE TO FACT:");dis_print_ft_name(graph->element[edge->toIndex]);printf("\n");
	dis_Fact* fact = &(dis_grelevant_facts[factId]);
	int level,goods,market=-1,i;
	
	//7 --> LOADED: GOODS TRUCK LEVEL	
	if ( fact->predicate==7 ){
		//print_out_state( localInitState );
		int stateLevel = get_level_id(fact);
		int onsaleCount = 0;
		//CALC COUNT OF ON-SALE of all the markets:
		for( i=0 ; i<localInitState->num_F ; i++ ){
			dis_Fact* onSaleFact = &(dis_grelevant_facts[localInitState->F[i]]);
			if( onSaleFact->predicate!= 4 ) continue;
			if ( (fact->args)[0] == (onSaleFact->args)[0] ){
				int onsaleLevel = (onSaleFact->args)[2];
				onsaleCount+=get_level_index(onsaleLevel);
			}
		}
		//printf("Sale Count for %s,",dis_gconstants[(fact->args)[0]]);printf(" is ");printf("%d.\n",onsaleCount);
				
		int readyLoadCount = 0;
		for( i=0 ; i<localInitState->num_F ; i++ ){
			dis_Fact* readyLoadFact = &(dis_grelevant_facts[localInitState->F[i]]);
			if( readyLoadFact->predicate!= 6 ) continue;
			if ( (fact->args)[0] == (readyLoadFact->args)[0] ){
				int readyLoadLevel = (readyLoadFact->args)[2];
				readyLoadCount += get_level_index( readyLoadLevel );
			}
		}
		
		int otherLoaded = 0;
		for( i=0 ; i<localInitState->num_F ; i++ ){
			dis_Fact* other = &(dis_grelevant_facts[localInitState->F[i]]);
			if( other->predicate!= 7 ) continue;
			if( (fact->args)[0] == (other->args)[0] && (fact->args)[1] == (other->args)[1] )
				continue;
			if( (fact->args)[0] == (other->args)[0] ){
				int loadLevel = (other->args)[2];
				otherLoaded += get_level_index( loadLevel );					
			}
		}
		
		level = (fact->args)[2];
		//printf("Precon:");dis_print_ft_name(factId);printf("[%s:%s]",dis_gconstants[stateLevel],dis_gconstants[level]);printf(":%d",compare_level_object(stateLevel,level));printf("\n");
		if ( onsaleCount + readyLoadCount + otherLoaded < get_level_index(level) )
			return 1;
			
		//If only need STORED XXX LEVEL1,  we really do not need to check like LOADED XXX XXX XXX LEVEL2
		dis_Fact *goalLevel = &(dis_grelevant_facts[graph->element[edge->toIndex]]);
		//printf("UPPER GOAL:%d;CHECKING GOAL%d\n",(goalLevel->args)[1],(fact->args)[2]);
		if (  get_level_index((goalLevel->args)[1]) < get_level_index((fact->args)[2]) ) return 1;
	}
	
	//6 --> READY-TO-LOAD: GOODS MARKET LEVEL 
	if ( fact->predicate==6 ){
		int stateLevel = get_level_id(fact);
		int onsaleLevel = 0;
		
		for( i=0 ; i<localInitState->num_F ; i++ ){
			dis_Fact* onSaleFact = &(dis_grelevant_facts[localInitState->F[i]]);
			if( onSaleFact->predicate!= 4 ) continue;
			if ( (fact->args)[0] == (onSaleFact->args)[0] && (fact->args)[1] == (onSaleFact->args)[1] ){
				onsaleLevel = (onSaleFact->args)[2];
			}
		}
		
		level = (fact->args)[2];
		if( get_level_index(stateLevel) + get_level_index(onsaleLevel) < get_level_index(level) ) 
			return 1;
		
		//dis_Fact *goalLevel = &(dis_grelevant_facts[graph->element[edge->toIndex]]);		
		//printf("UPPER GOAL:%d;CHECKING GOAL%d\n",(goalLevel->args)[2],(fact->args)[2]);
		//if (get_level_index((goalLevel->args)[2])<get_level_index((fact->args)[2])  ) return 1;
	}
	
  	//5 --> STORED: GOODS LEVEL 	
	if ( fact->predicate == 5  ){
		int stateLevel = get_level_id(fact);
		level = (fact->args)[1];
		if ( compare_level_object(level,stateLevel)!=0 ){
			return 1;
		}
	}

  	//4 --> ON-SALE: GOODS MARKET LEVEL 	
	if ( fact->predicate == 4  ){
		int stateLevel = get_level_id(fact);
		level = (fact->args)[2];
		if ( compare_level_object(level,stateLevel)!=0 ){
			return 1;
		}
	}
	
	return 0;
}

int apply_action_effect(dis_State* state, int op){
	int i,j;
	if( op == -1 ) printf("ERROR:No appliable op found.");
	//print_space();printf("%d++Applying op:",op);dis_print_op_name(op);print_out_state(state);
	
	applyOperationCount++;
	int effectIndex1 = 	dis_gop_conn[op].E[0];
	int *deleteList[100];
	int deleteListSize = 0;
	for( i = 0; i < dis_gef_conn[effectIndex1].num_D; i++){
		int flag = 1;
		
		int value = dis_gef_conn[effectIndex1].D[i];
		int *pos = bsearch(&value, localInitState->F, localInitState->num_F, sizeof(int), icmp );
		if (pos!=NULL){ 
			flag = 0;
			deleteList[deleteListSize++] = pos;
		}
				
		if ( flag ) {
			//printf("There might be errors, applying action failed in CG heuristic.");dis_print_op_name(op);printf("\n");
			return 0;
		}
	}
	
	for(i=0;i<deleteListSize;i++) *(deleteList[i])=-1;	
	int factListSize = state->num_F + dis_gef_conn[effectIndex1].num_A - dis_gef_conn[effectIndex1].num_D + 5;
	int* factList = malloc( sizeof(int) * factListSize );

	int pos = 0;
	qsort(dis_gef_conn[effectIndex1].A,dis_gef_conn[effectIndex1].num_A,sizeof(int),icmp);
	pos = merge_array( state->F , state->num_F , dis_gef_conn[effectIndex1].A , dis_gef_conn[effectIndex1].num_A , factList );
	
	free(state->F);
	state->F = NULL;
	state->num_F = pos;
	state->F = factList;
	return 1;
}


int find_truck_location_in_current_state(int truckId){
	int k;
	for( k = 0 ; k < localInitState->num_F ; k++ ){
		dis_Fact *fact= &(dis_grelevant_facts[localInitState->F[k]]);
		if ( (fact->args)[0] == truckId )
			return (fact->args)[1];
	}
	
	printf("WHILE FINDING TRUCK'S LOCATION, CRITITCAL ERROR.");
	return -1;
}

void exempt_sym_obj_action(dis_TransitiveEdge *edge , dis_TransitiveGraph *graph, int *actionArray, int *actionArraySize){
	int deleteFlag[300];
	int i,j;
	int TRUCK_TYPE_INT = 7;
	for(i=0;i<edge->actionSize;i++) deleteFlag[i]=0;
	
	/*This block is to prune multi-major-object searching*/	
	for(i=0;i<edge->actionSize;i++){
		int object = get_major_object(edge->action[i]);
		if ( is_invalid_action(edge->action[i],object) ) deleteFlag[i] = 1;
	}
	
	/* This block is to prune those sym-object, eg, TRUCKS*/
	for ( i = 0; i < dis_gtype_size[TRUCK_TYPE_INT]; i++ ){ 
		
		int loc1 = find_truck_location_in_current_state(dis_gtype_consts[TRUCK_TYPE_INT][i]);
		
		for ( j = i+1; j < dis_gtype_size[TRUCK_TYPE_INT]; j++ ){
			int loc2 = find_truck_location_in_current_state(dis_gtype_consts[TRUCK_TYPE_INT][j]);
			
			//IF equal, then delete all the actions about loc2(truck[j]);
			if( loc1 == loc2 ){
				int truckId = dis_gtype_consts[TRUCK_TYPE_INT][j];
				int k,l;
				for( k = 0; k<edge->actionSize;k++){
					if(deleteFlag[k]) continue;
					dis_OpConn *op = &(dis_gop_conn[edge->action[k]]);
					if ( strcmp( op->action->name, "UNLOAD")!=0 ) continue;
					
					int flag = 0;
				    for ( l = 0; l < op->action->num_name_vars; l++ ) 
						if ( truckId == op->action->name_inst_table[l] )
							flag = 1;
					if ( flag ) deleteFlag[k] = 1;
				}
			}
		}
    }
    
	for(i=0;i<edge->actionSize;i++)
		if(deleteFlag[i]==0) actionArray[(*actionArraySize)++] = edge->action[i];
}


/*
 * Calulate weight of one given edge, which might have many possible transition actions. So pick the one which has the smallest weight.
 */
 
int get_edge_weight(dis_TransitiveEdge *edge , dis_TransitiveGraph *graph){
	int i,j,k;
	int weight  = 1, min = INF;
	dis_State *localInitStateBackup = NULL;
	dis_State *reservedState= NULL;
	int majorObjectSettingFlag = 0;
	int minEdgePrecond = INF;
	int chosenActionId;
		
	//If all the preconditions are included in current state, then we can pick this action immediately.
	//if( depth<3) { print_space();printf("Geting_Edge_Weight:");printf(" FROM ");dis_print_ft_name( graph->element[edge->fromIndex]);printf("to "); dis_print_ft_name( graph->element[edge->toIndex]); printf("\n");}
	for( i=0;i<edge->actionSize;i++){
		int effectIndex1 = 	dis_gop_conn[edge->action[i]].E[0];
		
		int allMatch = 1;
		for( j = 0 ; j < dis_gef_conn[effectIndex1].num_PC ; j++){
			int found = 0;

			int value = dis_gef_conn[effectIndex1].PC[j];
			int *pos = bsearch(&value, localInitState->F, localInitState->num_F, sizeof(int), icmp );

			if ( pos==NULL ){
				allMatch = 0;
				break;
			}
		}
		
		if( allMatch ){			
			int object = get_major_object( edge->action[i] );
			if( is_invalid_action(edge->action[i],object) ) continue;
			apply_action_effect( localInitState, edge->action[i] );
			//if ( depth<3 ) { print_space();printf("Return By (All Match) get_edge_weight: "); print_out_edge( edge, graph, 1 ); }
			return 1;
		}
	}
	
	int conditionArray[100];
	int conditionArraySize = 0;
	
	//***** DOMAIN SPECIFIED
	int actionArray[100];
	int actionArraySize=0;
	exempt_sym_obj_action(edge,graph,actionArray,&actionArraySize);
	//*****END OF DOMAIN SPECIFIED	
	
	localInitStateBackup = localInitState;
	localInitState = NULL;
	
	for( i = 0 ; i<actionArraySize ; i++){
		//if(depth<3) {print_space();printf("ACTION:");dis_print_op_name(actionArray[i]);printf("\n");}
		int effectIndex1 = dis_gop_conn[actionArray[i]].E[0];		
		//print_space();printf("+Calcing min action weight of edge:");dis_print_op_name(edge->action[i]);printf(\n);
		 
		int edgeWeight = 0 ;
		localInitState = clone_state( localInitStateBackup );
		conditionArraySize = dis_gef_conn[effectIndex1].num_PC;
		get_ordered_conditions( conditionArray, &(dis_gef_conn[effectIndex1]) );
		
		int flag = 0;
		for( j=0;j<dis_gef_conn[effectIndex1].num_PC;j++)
			if ( is_invalid_precondition(conditionArray[j], edge , graph) ){
				flag = 1;
				break;
			}
		if (flag){
			free_state( localInitState );
			localInitState = NULL;
			continue;
		}
		
		int object = get_major_object(actionArray[i]);				
		if( object!=-1 && majorObject==-1 ) { majorObject = object; majorObjectSettingFlag=1; }
		for( j = 0 ; j < dis_gef_conn[effectIndex1].num_PC ; j++){
			if( conditionArray[j] == graph->element[ edge->fromIndex ]  ) continue;
			
			//print_space();printf("+++Start Checking Precondition#%d", conditionArray[j] ); dis_print_ft_name( conditionArray[j] ); printf("\n");
			int value = get_fact_distance_from_init( conditionArray[j] , graph );
			//print_space();printf("+++Finish Checked Precondition#%d", conditionArray[j] ); dis_print_ft_name( conditionArray[j] ); printf(":::%d\n",value);
			
			edgeWeight += value;
			if(edgeWeight>INF){
				edgeWeight = INF;
			 	break;
			}
		}
		if ( object!=-1 && majorObject!=-1 && majorObjectSettingFlag==1 ) majorObject = -1;

		if( minEdgePrecond > edgeWeight ){
			minEdgePrecond = edgeWeight;
			free_state(reservedState);
			reservedState = NULL;
			reservedState = localInitState;
			localInitState = NULL;
			chosenActionId = actionArray[i];
		}else{
			free_state(localInitState);
			localInitState = NULL;
		}
		
		
		if( minEdgePrecond < INF && depth > heuristicLevel ){
			//print_space();printf( "Breaking out the Loop --> minEdgePrecond%d:" , minEdgePrecond);dis_print_op_name(chosenActionId);printf( "\n" );			
			break;
		}
		/*		
		if( edgeWeight == INF ){
			//print_space();print_out_state(localInitStateBackup);
			//print_space();printf("  Failed Action:");dis_print_op_name(edge->action[i]);printf("\n");
			failCount[depth]++;
		}
		*/
	}
	
	free_state(localInitState);
	if ( minEdgePrecond < INF && chosenActionId!=-1 ){
		weight += minEdgePrecond;
		localInitState = reservedState;
		reservedState = NULL;
		int applyResult = apply_action_effect( localInitState, chosenActionId );
		if ( !applyResult ) return INF;
		//print_space();printf("Applying action:");dis_print_op_name(chosenActionId);printf("minEdgePrecond:%d\n",minEdgePrecond);//printf("--Seted state to ");print_out_state( reservedState );
		free_state(localInitStateBackup);localInitStateBackup = NULL;
	}else{
		weight = INF;
		free_state(reservedState);reservedState= NULL;
		localInitState = localInitStateBackup;
		localInitStateBackup = NULL;
	}

	//if(depth<3){print_space();printf("**Return By get_edge_weight: "); print_out_edge( edge, graph, weight);}
	return weight;
}


/*
 * This functions is for dijkstra computation.
 * searches for the vertex u in the vertex set Q that has the least d[u] value
 */
int extract_min_vertex(int *D, dis_ArraySet *Q){

	int minValue = INF;
	int minIndex = -1 , i;
	
	for( i = 0 ; i < Q->arraySize ; i++ ){
		if( Q->cell[i] == 1 && D[i]<minValue ){
			minValue = D[i];
			minIndex = i;
		}
	}
	
	if( minIndex != -1 )
		Q->cell[minIndex] = 0;
	return minIndex;
}

void calc_dijkstra(dis_TransitiveGraph *graph, int fromNode, int toNode, int *D, int *prev, int *w){
	int i, greedy_dijkstra;
	dis_State **state = malloc( sizeof(dis_State*) * graph->elementSize );
	
	for( i = 0 ; i < graph->elementSize ; i++ ){
		D[i] = INF;
		prev[i] = -1;
		state[i] = NULL;
	}
	
	D[fromNode] = 0;
	state[ fromNode ] = localInitState ;
	localInitState = NULL;
	
	dis_ArraySet S;
	dis_ArraySet Q;
	int arraySize = graph->elementSize;
	init_set( &S , arraySize );
	init_set( &Q , arraySize );
	for( i=0 ; i<arraySize ; i++ ) add_to_set( &Q , i );
	
	while( get_set_size( &Q )!=0 ){
		
		int u = extract_min_vertex( D, &Q );
		if( u == -1 ) break;
		
		add_to_set( &S, u );
		int found = 0;
		for( i = 0; i < graph->edgeListSize ; i++ ){
			dis_TransitiveEdge *edge = graph->edgeList[i];
			if( edge->fromIndex != u || edge->toIndex == u || edge->toIndex==fromNode) continue;
			
			localInitState = clone_state(state[u]);
			int weight = get_edge_weight( edge , graph );

			if( D[edge->toIndex] > D[u]  + weight ){
				D[edge->toIndex] = D[u] + weight;
				prev[edge->toIndex] = u;
				w[edge->toIndex] = weight;
				free_state( state[edge->toIndex] );
				state[ edge->toIndex ] = localInitState;
				localInitState = NULL;
			} else {
				free_state(localInitState); localInitState = NULL;
			}
			
		}
		
	}
	
	//Because we can not garentee the graph is strongly connected.	
	for( i = 0 ; i < Q.arraySize ; i++ )
		if( Q.cell[i] == 1 ){
			D[i] = INF;
			free_state(state[i]);
			state[i] = NULL;
		}

	free_state(localInitState);
	if ( state[toNode]!=NULL ) localInitState = clone_state(state[toNode]);
	else
		localInitState = clone_state(state[fromNode]);
	
	for( i=0 ; i<graph->elementSize ; i++ ){
		free_state(state[i]);
		state[i] = NULL;
	}
	
	free(state);
}

void calc_dijkstra_optimal(dis_TransitiveGraph *graph, int fromNode, int toNode, int *D, int *prev, int *w){
	int i, greedy_dijkstra;
	dis_State **state = malloc( sizeof(dis_State*) * graph->elementSize );
	
	for( i = 0 ; i < graph->elementSize ; i++ ){
		D[i] = INF;
		prev[i] = -1;
		state[i] = NULL;
	}
	
	D[fromNode] = 0;
	state[ fromNode ] = localInitState ;
	localInitState = NULL;
	
	dis_ArraySet S;
	dis_ArraySet Q;
	int arraySize = graph->elementSize;
	init_set( &S , arraySize );
	init_set( &Q , arraySize );
	for( i=0 ; i<arraySize ; i++ ) add_to_set( &Q , i );
	
	while( get_set_size( &Q )!=0 ){
		
		int u = extract_min_vertex( D, &Q );
		if( u == -1 ) break;
		
		add_to_set( &S, u );
		int found = 0;
		for( i = 0; i < graph->edgeListSize ; i++ ){
			dis_TransitiveEdge *edge = graph->edgeList[i];
			if( edge->fromIndex != u || edge->toIndex == u || edge->toIndex==fromNode) continue;
			
			localInitState = clone_state(state[u]);
			int weight = get_edge_weight( edge , graph );

			if( D[edge->toIndex] > D[u]  + weight ){
				D[edge->toIndex] = D[u] + weight;
				prev[edge->toIndex] = u;
				w[edge->toIndex] = weight;
				free_state( state[edge->toIndex] );
				state[ edge->toIndex ] = localInitState;
				localInitState = NULL;
			} else {
				free_state(localInitState); localInitState = NULL;
			}
			
			if( edge->toIndex == toNode ){
				found=1;
				break;
			}
		}
		
		if(found) break;
	}
	
	//Because we can not garentee the graph is strongly connected.	
	for( i = 0 ; i < Q.arraySize ; i++ )
		if( Q.cell[i] == 1 && i != toNode ){
			D[i] = INF;
			free_state(state[i]);
			state[i] = NULL;
		}

	free_state(localInitState);
	if ( state[toNode]!=NULL ) localInitState = clone_state(state[toNode]);
	else
		localInitState = clone_state(state[fromNode]);
	
	for( i=0 ; i<graph->elementSize ; i++ ){
		free_state(state[i]);
		state[i] = NULL;
	}
	
	free(state);
}

/*
 * For a array which has only one element not ordered, reorder it in ascending order.
 */
void reorder_array(int *array, int arraySize, int pos){
  int j;
  int k;
  if( array[0] > array[pos] )
    k = 0;
  else if( array[arraySize-1]<array[pos])
      k = arraySize;
  else
    for( k=1;k<arraySize;k++){
      if( array[k-1]<=array[pos] && array[k]>=array[pos] && k!=pos) break;
    }

  int from,to;
  if (k<pos){
    from = k;
    to = pos;

    int tmp = array[to];
    for(k=to;k>from;k--)
      array[k]= array[k-1];
    array[from] = tmp;

  }else{
    from = pos;
    to = k-1;

    int tmp = array[from];
    for(k=from;k<to;k++)
      array[k]= array[k+1];
    array[to] = tmp;
  }
}

/**
	get distance between two vertices, but we do not sure whether there are connected or not.
*/ 
int calc_fact_distance(int fromIndex, int toIndex, int groupId ){
	searchedNodes[depth]++;
	
	int i;
	if( fromIndex == toIndex ) return 0;
	int elementSize = transitiveGraph[groupId].elementSize;
	
	//For some lowest level graph, we just return the value in it, without any computation;	
	if( transitiveGraph[groupId].node!=NULL && transitiveGraph[groupId].node[fromIndex].distance!=NULL){
		//Of course, we have to change the state of localState, which is exactly the same with the below one.
		for( i = 0 ; i < localInitState->num_F ; i++ )
			if( localInitState->F[i] == transitiveGraph[groupId].element[fromIndex] ){
				localInitState->F[i] = transitiveGraph[groupId].element[toIndex];
				reorder_array( localInitState->F, localInitState->num_F, i );
				break;
			}
		return transitiveGraph[groupId].node[fromIndex].distance[toIndex];
	}
	
	//We suppose *every invariant* can only be access once in a top-down search
	if ( recurMark[transitiveGraph[groupId].invariantId] == 1 ) return 1;
		else recurMark[transitiveGraph[groupId].invariantId] = 1;
						
	if ( transitiveGraph[groupId].node == NULL ) {
		transitiveGraph[groupId].node = malloc( sizeof( dis_TransitiveNode ) * elementSize );
		for( i = 0 ; i < transitiveGraph[groupId].elementSize ; i++ )
			transitiveGraph[groupId].node[i].distance = NULL;
	}

	depth++;
	int *distance = malloc( sizeof(int) * elementSize);
	int *prev = malloc( sizeof(int) * elementSize );
	int *weight = malloc( sizeof(int) * elementSize );
	for( i =0 ;i < elementSize; i++ ){
		distance[i] = 0;
		prev[i] = 0;
		weight[i] = 0;
	}
	
//	if(depth<3) { print_space();printf("->Calcing[Level%d]:", depth);dis_print_ft_name( transitiveGraph[groupId].element[fromIndex] );printf(" to ");dis_print_ft_name( transitiveGraph[groupId].element[toIndex] );printf("\n");	}
	if (transitiveGraph[groupId].dependencyListSize==0) 
		calc_dijkstra(&(transitiveGraph[groupId]), fromIndex, toIndex, distance, prev, weight);
	else calc_dijkstra_optimal(&(transitiveGraph[groupId]), fromIndex, toIndex, distance, prev, weight);
//	if(depth<3) {print_space();printf("->Calced [Level%d]: Distance from " , depth);dis_print_ft_name( transitiveGraph[groupId].element[fromIndex] );printf(" to ");dis_print_ft_name( transitiveGraph[groupId].element[toIndex] );printf( " is %d. " , distance[toIndex] );printf("\n");/*print_out_route( prev , weight, toIndex , groupId );*/}
		
	int result = distance[toIndex];
	if ( transitiveGraph[groupId].dependencyListSize==0){
		transitiveGraph[groupId].node[fromIndex].distance = distance;
		transitiveGraph[groupId].node[fromIndex].weight = weight;
		transitiveGraph[groupId].node[fromIndex].prev = prev;
	}else{
		free(weight);
		free(prev);
		free(distance);
	}
	
	depth--;
	recurMark[transitiveGraph[groupId].invariantId] = 0;
	return result;
}


int get_state_distance(dis_State* evaluateState){
	//if (evaluateState->cg_value!=-1) return evaluateState->cg_value;
	int i,j,k,l,count = 0;
	
	depth = 0;
	majorObject = -1;
	recurMark = malloc( sizeof(int) * num_invariantGroup );
	localInitState = malloc( sizeof(dis_State) );
	localInitState->num_F = evaluateState->num_F;
	localInitState->F = malloc( sizeof(int) * localInitState->num_F );
	heuristicLevel	 = 4;
	mallocStateCount = 0;
	
	for(i=0;i<10;i++){
		searchedNodes[i] = 0;
		failCount[i] = 0;
	}
//	printf("*");
	
	for(i=0;i<dis_gnum_flogic_goal;i++){
		int num = 0 ;
		double sum = 0;
		//printf("Checking..");dis_print_ft_name( dis_gflogic_goal[i] );printf("\n");
		for( j=0;j<num_transitiveGraph;j++){
			int toIndex = get_node_index( dis_gflogic_goal[i], j );		
			if(  toIndex == -1  ) continue;
			
			for( k = 0 ; k < localInitState->num_F ; k++ )
				localInitState->F[k] = evaluateState->F[k];
			qsort(localInitState->F,localInitState->num_F,sizeof(int),icmp);				
			for( l = 0; l<num_invariantGroup; l++ ) recurMark[l] = 0;

			for( k=0 ; k<localInitState->num_F ; k++ ){				
				if ( localInitState->F[k]== dis_gflogic_goal[i] ) continue;
				
				int fromIndex = get_node_index( localInitState->F[k] , j );
				if( fromIndex == -1 ) continue;
				
				int dist = calc_fact_distance( fromIndex, toIndex, j );

				//printf("UPPER MOST LEVEL: FROM -----");dis_print_ft_name( transitiveGraph[j].element[fromIndex] );printf("  to  ");dis_print_ft_name( transitiveGraph[j].element[toIndex] );printf( " is of distance%d\n",dist );				
				if( dist!=0 ){
					sum += dist;
					num++;
				}
			}
		}
		
		if( num!=0 ){
			int average = ceil(sum/(double)num);
			count += average;
		}
	}

	free(recurMark);
	free_state(localInitState);
		
	//printf("Recursive Call Count:");for( i=0 ; i<10 ; i++ ) printf( "[%d]" , searchedNodes[i] ); printf("\n");
	//printf("Failed edge_weight Count:");for(i=0;i<10;i++) printf("[%d]",failCount[i]);printf("\n");
	//printf("evaluateStateSize%d;stateOperationCount:%d\n",evaluateState->num_F,stateOperationCount);
	
	//if(mallocStateCount>0) printf("Malloc State:%d\n",mallocStateCount);
	//if( count>=65535 ) print_out_state( evaluateState );printf("\n");
	//evaluateState->cg_value = count;
	return count;
}

void dis_cg_make_state( dis_State *pointer, int ft, int fl ) 
{

  int i;

  pointer->F = ( int * ) calloc( ft, sizeof( int ) ); 
  pointer->f_D = ( dis_Bool * ) calloc( fl, sizeof( dis_Bool ) ); 
  pointer->f_V = ( float * ) calloc( fl, sizeof( float ) );

  for ( i = 0; i < fl; i++ ) {
    pointer->f_D[i] = dis_FALSE;
  }

}


void dis_collect_H_cg_info( dis_State *source, int h ){
	
  static dis_Bool first_call = dis_TRUE;
    
  int i,j;
  int* value = ( int * ) calloc( dis_gnum_op_conn+5, sizeof( int ) );
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
		
		dis_Bool result = dis_result_to_dest( &dest, source, i, -1);
		if (result){
			dis_gH[dis_gnum_H] = i;
			int cg_value = get_state_distance(&dest);	
			value[dis_gnum_H] = cg_value; 
			dis_gnum_H++;
			if (cg_value<h) break;
		}
	}
    
    for( i = dis_gnum_H-1 ; i>0 ;i-- )
   	for(j = 0; j < i; j++)
   		if( value[j] > value[j+1] ){
   			int tmp = value[j];
   			value[j] = value[j+1];
   			value[j+1] = tmp;
   			
   			tmp = dis_gH[j];
   			dis_gH[j] = dis_gH[j+1];
   			dis_gH[j+1] = tmp;
   		}
   	
   	
/*   	printf("[");
   	for(i=0;i<dis_gnum_H;i++){
   		printf("%d,",value[i]);	
   	}
   	printf("]\n");*/
   
   	
   	//printf("Prepareing Search Space: value of first 4 elements in dis_gH %d,%d,%d,%d",value[0],value[1],value[2],value[3]);
   	/*
   	printf("\nFirst State in dis_gH: ");
   	dis_result_to_dest(&dest,source,i,-1);
   	print_out_state( &dest );
   	printf("\n");
   	*/
   	
   	free(value);
}