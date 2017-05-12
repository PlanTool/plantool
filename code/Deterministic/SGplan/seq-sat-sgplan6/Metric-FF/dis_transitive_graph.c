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

 * File: dis_transitive_graph.c

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
#include "lpg.h"

dis_TransitiveGraph transitiveGraph[3000];
int num_transitiveGraph;

int levelObject[200];
int levelObjectSize;
int levelFact[200];
int levelFactSize;

int* fact_distance[200];

//SEE BEGINNING OF THIS FILE, THERE ARE DEFINITION OF fact_distance;
void calc_distance_fact(dis_TransitiveGraph *graph, int *distance_array,  int fromIndex){
	int visited[500];
	int expand_list[500];
	expand_list[0] = fromIndex;
	int expand_list_size = 1;
	int distance = 0,i,j;
	
	for( i=0 ; i<graph->elementSize ; i++ ){
		visited[i] = 0;
		distance_array[i] = 65535;
	}
	distance_array[fromIndex] = 0;
	
	do{
		for( i=0 ; i< expand_list_size ; i++)
			visited[ expand_list[i] ] = 1;
		
		distance++;
		int tmp_list[500];
		int tmp_list_size = 0;
		
		printf("##########%d::::",distance);
		for(i=0;i<expand_list_size;i++)
			printf("%d;",expand_list[i]);
		printf("\n");
				
		for( i=0 ; i<expand_list_size; i++)
		for( j=0 ; j<graph->elementSize ; j++ ){
			dis_TransitiveEdge *edge = graph->edge[ expand_list[i] * graph->elementSize + j ];
			if ( edge != NULL && edge->actionSize && !visited[j] ) {
				distance_array[ j ] = distance;
				tmp_list[tmp_list_size++] = j;				
			}
		}

		expand_list_size = 0;
		for(i=0;i<tmp_list_size;i++)
			expand_list[ expand_list_size++ ] = tmp_list[i];
	}while( expand_list_size );
}

void calc_distance_bound(){
	
	int i,j;
	for(i=0;i<num_transitiveGraph;i++){
		dis_TransitiveGraph *graph = &( transitiveGraph[i] );
		fact_distance[i] = malloc( graph->elementSize * graph->elementSize * sizeof(int) );
		
		for( j=0 ; j<graph->elementSize ; j++ )
			calc_distance_fact( graph , fact_distance[i]+j*graph->elementSize , j );
	}
}


void init_level_list(){
	levelObjectSize = 0;
	levelFactSize = 0;
	int levelType = 2;
	int levelRelationPredicate = 3;
	int i;

	for ( i = 0; i < dis_gtype_size[levelType]; i++ ) 
		levelObject[levelObjectSize++] = dis_gtype_consts[levelType][i];

	/*
	for(i=0;i<levelObjectSize;i++)
		printf("%s,",dis_gconstants[levelObject[i]]);
	*/

	int flag = 1;
	while(flag){
		flag = 0;
		for(i=0;i<dis_gnum_initial_predicate[levelRelationPredicate];i++){
			dis_Fact *fact = &(dis_ginitial_predicate[levelRelationPredicate][i]);
			int a = (fact->args)[0];
			int b = (fact->args)[1];
			
			int indexA;
			for( indexA = 0 ; indexA < levelObjectSize ; indexA++ )
				if( levelObject[indexA] == a ) break;
				
			int indexB;
			for( indexB=0 ; indexB < levelObjectSize ; indexB++ )
				if( levelObject[indexB]==b ) break;
			
			if ( indexA < indexB ){
				int tmp = levelObject[indexA];
				levelObject[indexA] = levelObject[indexB];
				levelObject[indexB] = tmp;
				flag = 1;
			}
		}
	}

	/*
	printf("\nOBJECT:");
	for(i=0;i<levelObjectSize;i++)
		printf("%s,",dis_gconstants[levelObject[i]]);
	printf("\n");*/
}

int compare_level_object( int a, int b){
	int indexA,result;
	for( indexA = 0 ; indexA < levelObjectSize ; indexA++ )
		if( levelObject[indexA]==a ) break;
				
	int indexB;
	for( indexB=0 ; indexB< levelObjectSize ; indexB++ )
		if( levelObject[indexB]==b ) break;
	
	if( indexA < indexB ) result = 1;
	else if( indexA>indexB ) result =  -1;
	else result = 0;
	//printf(" COMPARING %s and %s, result:%d\n ",dis_gconstants[a],dis_gconstants[b],result );	
	return result; 
}

int get_level_index(int levelFact){
	int i;
	for(i=0;i<levelObjectSize;i++)
		if(levelObject[i]==levelFact) return i;
	return -1;	
}

int is_valid_atom(dis_Invariant* invar, dis_Fact *fact, int obj){

	int i,j;
	for(i=0;i<invar->size;i++){

		if( fact->predicate==invar->obj[i]->fact->predicate ){
	
			int focus = invar->obj[i]->focus;
			if( focus <= -1 ) return 1;						//Changed "==" to "<=" here, why what is the cause, still need to study into it.
			
			int flag = 0;
			for(j=0;j<dis_gtype_size[invar->focusType];j++){
				if(  dis_gtype_consts[invar->focusType][j] == fact->args[focus] ){
					flag = 1;
				}
			}
			if(!flag) return 0;
			
			if( fact->args[invar->obj[i]->focus] != obj ) return 0;
			
			return 1;
		}
	}
	return 0;
}

/*
	two invariant 
		#1.{[AT CRATE PLACE]   ,[IN CRATE TRUCK],[LIFTING CRATE]}
		#2.{[ON CRATE SURFACE] ,[IN CRATE TRUCK],[LIFTING CRATE]}
	We want to delete one of them, because CLEAR always accompany AT clause, we do it just by examine goal state. 
	According to goal state, the second one should not be taken into consideration.
	
	Test it as following:
		1. Pick one invariant
		2. exempt the predicates which are not included in the goal state. [CLEAR] CRATE WAS DELETED
		3. check the new invariant [IN CRATE TRUCK] [LIFTING CRATE] 
		4. If this new invariant is actually a subset of another invariant then we mark this invariant with "DELETED"

*/
void delete_duplicate_invariant(){
	int i,j,k;
	
	for(i=0;i<num_invariantGroup;i++){

		dis_Invariant *invar = &(invariantGroup[i]);

		dis_Invariant newInvar;
		newInvar.size = 0;
		newInvar.focusType = invar->focusType;
		
		for( j=0 ; j<invar->size ; j++ ){
			//invar->obj[i]->predicate;

			int flag = 0;
			for(k=0;k<dis_gnum_flogic_goal;k++)
				if( invar->obj[j]->fact->predicate == dis_grelevant_facts[dis_gflogic_goal[k]].predicate ) {
					flag = 1;
					break;	
				}

			if( flag == 0 ){
				newInvar.obj[newInvar.size] = invar->obj[j];
				newInvar.size++;
			}
		}
		
		//printf("[%d,%d]",newInvar.size,num_invariantGroup);
		for( j=0 ; j<num_invariantGroup ; j++ ){
			if( i!=j && is_sub_invariant( &newInvar , &(invariantGroup[j]) ) ){
				invar->deleted = 1;
				break;
			}
		}
	}
}

int get_weight(dis_Invariant *invar, int *fact, int obj){
	int i;
	int count = 0;
	if( invar->obj[0]->focus!=-1 ){
		//Obj must be of focusType	
		for(i=0;i<dis_gtype_size[invar->focusType];i++){
			if( dis_gtype_consts[invar->focusType][i] == obj ) count = 1;		
		}
		if( !count ) return 0;
	}
	
	count = 0;
	for ( i = 0; i < dis_ginitial_state.num_F; i++ ) {			
		int valid = is_valid_atom( invar, &(dis_grelevant_facts[dis_ginitial_state.F[i]]), obj );
		if( valid ){
			count++;
			*fact = dis_ginitial_state.F[i];
		}
	}

	return count;
}

int exist_in_tg(dis_TransitiveGraph *graph, int fact1 , int fact2){
	int i;
	int value = fact1;
	int *pos = bsearch(&value, graph->element , graph->elementSize, sizeof(int), icmp );
	if ( pos == NULL ) return 0;
	value = fact2;
	pos = bsearch(&value, graph->element , graph->elementSize, sizeof(int), icmp );
	if (pos == NULL) return 0;
	return 1;
}

int is_valid_action_tg(int actionId){
	
	int i,k,j;
	int effectId = dis_gop_conn[actionId].E[0];
	for(k=0;k<dis_gef_conn[effectId].num_PC;k++)
	for(j=0;j<dis_gef_conn[effectId].num_PC;j++){
  		if ( j>=k ) continue;
  		 
  		for(i=0;i<num_transitiveGraph;i++){
  			if ( dis_gef_conn[effectId].PC[j] == dis_gef_conn[effectId].PC[k]) continue;
  			if(exist_in_tg(&(transitiveGraph[i]),dis_gef_conn[effectId].PC[j],dis_gef_conn[effectId].PC[k])){
  				//printf("VIOLATE:");dis_print_ft_name(dis_gef_conn[effectId].PC[j]);printf(" and ");
  				//dis_print_ft_name(dis_gef_conn[effectId].PC[k]);printf(" is in same group.\n");
  				break;
  			}
  		}
  		if (i<num_transitiveGraph)
  			return 0;
	}
	
	
	/*
	dis_Action *a = dis_gop_conn[actionId].action;
	if ( a->num_name_vars>4 && a->name_inst_table[1] == a->name_inst_table[4] ) return 0;
	*/
	return 1;
}

void build_edges(dis_TransitiveGraph *graph){
	int i,j,k,l,m,effect;
	int elementSize = graph->elementSize;
	graph->edge = malloc( elementSize * elementSize * sizeof(dis_TransitiveEdge*) );
	for(i=0;i<elementSize;i++)
	for(j=0;j<elementSize;j++)
		graph->edge[i*elementSize+j] = NULL;
	
	int actionList[10000];
	int actionListSize = 0;
	for(i=0;i<elementSize;i++){
		for(j=0;j<elementSize;j++){
			actionListSize = 0;
			for(k=0;k<dis_gft_conn[graph->element[i]].num_D;k++){
				int actionIndex = dis_gef_conn[dis_gft_conn[graph->element[i]].D[k]].op;
				dis_Action *action = dis_gop_conn[actionIndex].action;

				//if ( !is_valid_action_tg(actionIndex) ) continue;
				
				for(effect = 0;effect<action->num_effects;effect++)
				for(l=0;l<action->effects[effect].num_adds;l++){
					int add = action->effects[effect].adds[l];
					
					if(add == graph->element[j] ){
						actionList[actionListSize++] = actionIndex;
						break;
					}
				}
			}

			if(actionListSize!=0){
				dis_TransitiveEdge *edge = malloc(sizeof(dis_TransitiveEdge));
				graph->edge[i*elementSize+j] = edge;
				//edge->fromFact = graph->element[i];
				//edge->toFact = graph->element[j];
				edge->fromIndex = i;
				edge->toIndex = j;
				
				edge->action = malloc(sizeof(int)*actionListSize);
				edge->actionSize = actionListSize;
				for(k=0;k<actionListSize;k++)
					edge->action[k] = actionList[k];
					
				
				if( actionListSize == 0 ) continue;
				//Calc Common Precondition
				int effectIndex1 = dis_gop_conn[actionList[0]].E[0];
				if( actionListSize==1 ){
					
					edge->commonPreconditionList = malloc( dis_gef_conn[effectIndex1].num_PC * sizeof(int) );
					edge->commonPreconditionListSize = 0;
					for( l=0 ; l<dis_gef_conn[effectIndex1].num_PC ; l++ ){
						int condition = dis_gef_conn[effectIndex1].PC[l];
						if( condition == graph->element[edge->fromIndex]) continue;

						edge->commonPreconditionList[edge->commonPreconditionListSize++] = condition;
					}
					continue;
				}
				
				int* tmp = malloc( dis_gef_conn[effectIndex1].num_PC * sizeof(int) );
				int tmpSize = 0;
				for( l = 0 ; l < dis_gef_conn[effectIndex1].num_PC ; l++ ){
					int condition = dis_gef_conn[effectIndex1].PC[l];
					if( condition == graph->element[edge->fromIndex] ) continue;
					
					int flag = 1;	
					for( k = 1 ; k < actionListSize ; k++ ){
						int effectIndex = dis_gop_conn[actionList[k]].E[0];
						
						int found = 0;
						for( m=0 ; m<dis_gef_conn[effectIndex].num_PC ; m++ )
						if( condition == dis_gef_conn[effectIndex].PC[m] ){
							found = 1;
							break;
						}
						
						if( found == 0 ){
							flag = 0;
							break;
						}
					}
					
					if( flag )
						tmp[tmpSize++] = condition;
				}
				
				edge->commonPreconditionList = malloc( tmpSize * sizeof(int) );
				for( k = 0 ; k < tmpSize ; k++ )
					edge->commonPreconditionList[k] = tmp[k];
				edge->commonPreconditionListSize = tmpSize;
				free(tmp);
			}
		}
	}
	
	//Add edges to EdgeList, this is just for easing Dijkstra algorithm
	int edgeCount = 0;
	for( i=0 ; i<elementSize;i++)
	for( j=0 ; j<elementSize;j++)
		if( graph->edge[i*elementSize+j] != NULL ) edgeCount++;
	
	graph->edgeList = malloc( sizeof(dis_TransitiveEdge*) * edgeCount );
	graph->edgeListSize = 0;
	for( i=0;i<elementSize;i++)
	for( j=0;j<elementSize;j++)
	if( graph->edge[i*elementSize+j]!=NULL ){
		graph->edgeList[graph->edgeListSize] = graph->edge[i*elementSize+j];	
		graph->edgeListSize++;
	}
}

int is_exist_group(int *factList, int factListSize){
	
	int i,j;
	for(i=0;i<num_transitiveGraph;i++){
		
		dis_TransitiveGraph *graph = &(transitiveGraph[i]);
		if( factListSize!=graph->elementSize ) continue;
		
		int flag = 0;
		for(j=0;j<factListSize;j++){
			if(graph->element[j] != factList[j] ) {
				flag = 1;
				break;
			}
		}
		if(flag==0) return 1;
	}
	return 0;
}

void new_transitive_group(dis_Invariant *invar, int fact, int obj){
	
	int i,j,k,l;
	int factList[dis_MAX_RELEVANT_FACTS];
	int calcPos = 0;
	int effect;
	factList[0] = fact;
	int factListSize = 1;	
	
	int flag;
	do{
		flag = 0;
		for( i = calcPos ; i < factListSize ; i++ ){
      		for ( j = 0; j < dis_gft_conn[factList[i]].num_D; j++ ){

				int actionIndex = dis_gef_conn[dis_gft_conn[factList[i]].D[j]].op;
				dis_Action * action = dis_gop_conn[actionIndex].action;
				
				for(effect=0;effect<action->num_effects;effect++)
				for(k=0;k<action->effects[effect].num_adds;k++ ){

					int add = action->effects[effect].adds[k];

					int exists = 0;
					for(l=0;l<factListSize;l++)
						if( factList[l] == add ){
							exists = 1;
							break;
						}
					if ( exists ) continue;
					
					if( is_valid_atom( invar , &(dis_grelevant_facts[add]) , obj ) ) {
						flag = 1;
						factList[factListSize] = add;
						factListSize++;
					}
				}
      		}
		}
	}while( flag==1 );
	
	qsort(factList,factListSize,sizeof(int),icmp);
	dis_TransitiveGraph *graph = &(transitiveGraph[num_transitiveGraph]);
	if( is_exist_group(factList,factListSize) ) return;

	graph->element = malloc( sizeof(int) * factListSize );
	graph->elementSize = factListSize;
	for(i = 0;i<factListSize;i++)
		graph->element[i] = factList[i];
	num_transitiveGraph++;
	graph->object = obj;
	graph->invariantId = invar->id;
}

void print_out_transitive_graph(dis_TransitiveGraph *graph){
	int i,j,edgeCount=0;

	for(i=0;i<graph->elementSize;i++)
	for(j=0;j<graph->elementSize;j++){
		dis_TransitiveEdge* edge = graph->edge[ i*graph->elementSize+j ];	
		if( edge!=0 && edge->actionSize!=0 ) edgeCount++;
	}
	
	printf("Graph Size:%d; ",graph->elementSize);
	printf("Build from invar:%d;",graph->invariantId);
	if(graph->object==-1) printf("Object: NULL\n");
		else printf("Object:%s;" , dis_gconstants[graph->object]);
	printf("Edge Count:%d\n",edgeCount);
	printf("Group elements:");
	for(i=0;i<graph->elementSize;i++){
		//printf("[%d]",graph->element[i]);
		dis_print_ft_name( graph->element[i] );
	}
}

void print_out_transitive_graph_set(){
	int i,j,k,l,m,effect;
	printf(" PRINTING OUT TRANSITIVE GRAPH\n" );
	for(i=0;i<num_transitiveGraph;i++){
		dis_TransitiveGraph* graph = &(transitiveGraph[i]); 
		printf("#%d:",i);
		print_out_transitive_graph( graph );
		printf("\n");
		int elementSize = transitiveGraph[i].elementSize;
		
		for(j=0;j<elementSize;j++)
		for(k=0;k<elementSize;k++){
			dis_TransitiveEdge* edge = graph->edge[j*elementSize+k];
			
			if( graph->edge[j*elementSize+k] != NULL){
				dis_print_ft_name(graph->element[j]);
				printf(" -->> ");
				dis_print_ft_name(graph->element[k]);
				printf(" --- COMMON PRE:");
				for( l=0 ; l<edge->commonPreconditionListSize ; l++ ){
					dis_print_ft_name( edge->commonPreconditionList[l] );
					printf(";");
				}
				printf("\n");
				/*
				for( l=0 ; l<edge->actionSize ; l++ ) {
					printf("   [");
					dis_print_op_name(edge->action[l]);printf("[PRE:%d]",dis_gef_conn[dis_gop_conn[edge->action[l]].E[0]].num_PC);
					printf("] {");
					
					//Print all the precondition:
					dis_Action * action = dis_gop_conn[edge->action[l]].action;
					for(m=0;m<dis_gef_conn[dis_gop_conn[edge->action[l]].E[0]].num_PC;m++){
						int condition = dis_gef_conn[dis_gop_conn[edge->action[l]].E[0]].PC[m];
						if(condition==graph->element[j]) continue;
						dis_print_ft_name( condition );
						printf(";");
					}
					
					printf("}\n");
				}
				*/
			}
		}
				
		printf("Depends on:");
		for(j=0;j<graph->dependencyListSize;j++){
			int depend =  graph->dependencyList[j];
			printf(" Group#%d(%s) ", depend ,dis_gconstants[ transitiveGraph[depend].object ]);
		}
		printf("\nDepended(%d) by:",graph->dependOnThisListSize);
		for(j=0;j<graph->dependOnThisListSize;j++){
			int depend = graph->dependOnThisList[j];
			printf(" Group#%d(%s)",depend, dis_gconstants[ transitiveGraph[depend].object] );	
		}
		printf("\n\n");
	}
}


void print_out_transitive_graph_set_simple(){
	int i,j,k;

	//calc_distance_bound();
	
	for(i=0;i<num_transitiveGraph;i++){
		printf("[");
		dis_TransitiveGraph* graph = &(transitiveGraph[i]);
		for(j=0;j<graph->elementSize;j++){
			printf(" ");
			dis_print_ft_name( graph->element[j] );
		}			
		printf(" ]\n");
		
		printf("%d\n" , graph->edgeListSize);
		for(j=0;j<graph->elementSize;j++)
		for(k=0;k<graph->elementSize;k++)
		if ( graph->edge[ j*graph->elementSize + k ] != NULL ){
			dis_print_ft_name( graph->element[j] );
			printf(" ");
			dis_print_ft_name( graph->element[k] );
			printf("\n");
		}
		
		/*
		printf("#####################\n");
		for(j=0;j<graph->elementSize;j++)
		for(k=0;k<graph->elementSize;k++){
			dis_print_ft_name( graph->element[j] );
			printf(" ");
			dis_print_ft_name( graph->element[k] );
			int *distance = fact_distance[i];
			printf("  ::: DISTANCE %d\n",distance[j*graph->elementSize+k]);
		}
		*/
	}
	
}

/*
	A transitive graph (A) depends on another (B) follows this creteria:
		#For all Edges in Graph A:
			#For all action in the edge
				#for all precondition of the action
					#There is a precondition(fact) included in B's element set.
*/
void analyze_dependency_relationship(dis_TransitiveGraph *graph){
	
	int facts[dis_MAX_RELEVANT_FACTS];
	int i,j,k,l,m;
	for( i=0 ; i<dis_MAX_RELEVANT_FACTS ; i++ ) facts[i] = -1;

	int elementSize = graph->elementSize;	
	for(j=0;j<elementSize;j++)
		for(k=0;k<elementSize;k++){
			dis_TransitiveEdge* edge = graph->edge[j*elementSize+k];
		
			if( graph->edge[j*elementSize+k] != NULL){

				for( l=0 ; l<edge->actionSize ; l++ ) {
					for(m=0;m<dis_gef_conn[dis_gop_conn[edge->action[l]].E[0]].num_PC;m++){

						int condition = dis_gef_conn[dis_gop_conn[edge->action[l]].E[0]].PC[m];
						facts[condition] = 1;
					}
				}
			}
		}

	int* list = malloc( sizeof(int) * num_transitiveGraph );
	int listSize;
	
	listSize = 0;
	//While the two group is the same object focused, How to deal with it requires refinement.~~~~~~~~~~~~~~~~~~~~
	for(i=0;i<num_transitiveGraph;i++){		
		if(i==graph->order) continue;

		for(j=0;j<transitiveGraph[i].elementSize;j++)
			if( facts[ transitiveGraph[i].element[j] ] == 1 ){
				list[listSize] = i;
				listSize++;
				break;
			}
	}

	graph->dependencyListSize = listSize;
	graph->dependencyList = malloc( sizeof(int) * listSize );
	for( i=0 ; i<listSize ; i++ ) graph->dependencyList[i] = list[i];
	free(list);
}


void generate_accompany_group(int index){
	
	//printf("GENERATE ACCOMPANY GROUP\n");
	dis_AccompanyGroup *acc_group = &(accompanyGroup[index]);	
	dis_Fact *fact1 = acc_group->master_fact;
	dis_Fact *fact2 = acc_group->slave_fact;
	//dis_print_dis_Fact(fact1);dis_print_dis_Fact(fact1);printf("\n");
	
	int i,j,k;
	int NUM_SLAVE_EACH_MASTER = 50;
	int same_type_pos1, same_type_pos2;
	int another_type_id1 , another_type_id2;
	int same_type_id;
	dis_Fact *relation_predicate = NULL;
	dis_Operator *relation_operator = NULL;
	
	// SAME_TYPE_POS means where the pos of the param those facts have 
	if( fact1->args[0] == fact2->args[0] ){
		same_type_pos1 = 0;
		same_type_pos2 = 0;
		same_type_id = fact1->args[0];
		another_type_id1 = fact1->args[1];
		another_type_id2 = fact2->args[1];
	}else if ( fact1->args[1] == fact2->args[1] ){
		same_type_pos1 = 1;
		same_type_pos2 = 1;
		same_type_id = fact1->args[1];
		another_type_id1 = fact1->args[0];
		another_type_id2 = fact2->args[0];
	}else if ( fact1->args[0]==fact2->args[1] ){
		same_type_pos1 = 0;
		same_type_pos2 = 1;	
		same_type_id = fact1->args[0];
		another_type_id1 = fact1->args[1];
		another_type_id2 = fact2->args[0];
	}else if (fact1->args[1]==fact2->args[0] ){
		same_type_pos1 = 1;
		same_type_pos2 = 0;
		same_type_id = fact1->args[1];
		another_type_id1 = fact1->args[0];
		another_type_id2 = fact2->args[1];
	}

	//FIND OUT THE RELATION PREDICATE	
	for(j=0;j<dis_gnum_operators;j++){
		dis_Operator* action = dis_goperators[j];
		
		int count = 0;
		dis_Effect *e = NULL;
		dis_Literal *l = NULL;
		for( e=action->effects ; e ; e=e->next)
		for( l=e->effects; l ; l=l->next){
			if (l->negated) continue;
			
			if( l->fact.predicate == fact1->predicate || l->fact.predicate == fact2->predicate )
				count++;
			
			if( l->fact.predicate == fact1->predicate )
				fact1 = &(l->fact);
			else if( l->fact.predicate == fact2->predicate)
				fact2 = &(l->fact);
		}
		if(count==2) break;
	}
	
	//printf("#################AA%d,%d\n",j,dis_gnum_operators);
	if( j == dis_gnum_operators ) return;
	
	//THE ACTION WHICH CONTAINS BOTH fact1 and fact2 is found out.
	//Then get the RELATION fact from it.
	
	dis_WffNode *precond = NULL;
	relation_predicate = NULL;
	for( precond = dis_goperators[j]->preconds->sons ; precond ; precond = precond->next ){
		if ( precond==NULL || precond->fact == NULL ) continue;
		if ( (precond->fact->args[0]== another_type_id1 && precond->fact->args[1] == another_type_id2 )
			|| ( precond->fact->args[1] == another_type_id1 && precond->fact->args[0] == another_type_id2 ) ){
			relation_predicate = precond->fact;
			relation_operator = dis_goperators[j];
			break;
		}
	}
	if ( relation_predicate == NULL ) return;
	
	dis_Fact *relation_fact[500];
	int relation_fact_size = 0;
	for(i=0;i<500;i++) relation_fact[i] = NULL;
	
	//find out and save all the fact of RELATION in inital_facts	
	for(i=0;i<dis_gnum_initial_predicate[relation_predicate->predicate];i++){
		dis_Fact *init_fact = &(dis_ginitial_predicate[relation_predicate->predicate][i]);
		
		int fail = 0;
		for(j=0;j<2;j++){
			int object = init_fact->args[j];
			int type = relation_operator->var_types[dis_DECODE_VAR(relation_predicate->args[j])];
			int *pos = bsearch(&object, dis_gtype_consts[type] , dis_gtype_size[type], sizeof(int), icmp );
			if (pos==NULL){
				fail = 1;
				break;	
			}
		}
		
		if (fail) continue;
		relation_fact[relation_fact_size++] = init_fact;
	}
	
	//for( i=0;i<relation_fact_size;i++ ) {printf("PPPP");dis_print_dis_Fact( relation_fact[i] );}
	
	//FIND which one is the master;
	int tmp_list[500];
	int tmp_list_size = 0;
	int is_param_multi[2];	
	for( k = 0; k < 2 ; k++ ){
		is_param_multi[k]=0;
		tmp_list_size = 0;
		for(i=0;i<relation_fact_size;i++){
			int object = relation_fact[i]->args[k];
			
			int found_same = 0;
			for( j=0 ; j<tmp_list_size ; j++ )
			if( tmp_list[j] == object ){
				found_same = 1;
				break;
			}
				
			if(found_same){
				is_param_multi[k] = 1;	
				break;
			}
			tmp_list[tmp_list_size++] = object;
		}
	}
	
	//ONLY one can be MASTER_OBJECT; otherwise, return;
	if( (is_param_multi[0]+is_param_multi[1])!=1 ) return;
	
	if( is_param_multi[0]==1 ){
		acc_group->master_object_type = relation_operator->var_types[dis_DECODE_VAR(relation_predicate->args[0])];
		acc_group->slave_object_type = relation_operator->var_types[dis_DECODE_VAR(relation_predicate->args[1])];
	}else if( is_param_multi[1]==1 ){
		acc_group->master_object_type = relation_operator->var_types[dis_DECODE_VAR(relation_predicate->args[1])];
		acc_group->slave_object_type = relation_operator->var_types[dis_DECODE_VAR(relation_predicate->args[0])];		
	}
	
	int diff_pos1,diff_pos2;
	diff_pos1 = same_type_pos1==0 ? 1 : 0;
	diff_pos2 = same_type_pos2==0 ? 1 : 0;
	if( relation_operator->var_types[dis_DECODE_VAR(fact1->args[diff_pos1])] == acc_group->master_object_type ){
		
		acc_group->master_fact = fact1;
		acc_group->slave_fact = fact2;
		acc_group->same_param_master = same_type_pos1;
		acc_group->same_param_slave = same_type_pos2;
	}else if ( relation_operator->var_types[dis_DECODE_VAR(fact2->args[diff_pos2])] == acc_group->master_object_type ){
		
		acc_group->master_fact = fact2;
		acc_group->slave_fact = fact1;
		acc_group->same_param_master = same_type_pos2;
		acc_group->same_param_slave = same_type_pos1;		
	}
	
	/*
	 *Now we have MASTER_OBJECT_TYPE, SLAVE_OBJECT_TYPE, RELATION_FACT
		#1.Generate a list of master object.
		#2.For each master object, generate a list of slave object
	 */
	int master_param = -1, slave_param = -1;
	if ( relation_operator->var_types[dis_DECODE_VAR(relation_predicate->args[0])]==acc_group->master_object_type ){
		master_param = 0;
		slave_param = 1;
	}
	else if ( relation_operator->var_types[dis_DECODE_VAR(relation_predicate->args[1])]==acc_group->master_object_type ){
		master_param = 1;
		slave_param = 0;
	}
	if( master_param==-1 ) return;
	
	acc_group->master_object_num = 0;
	for( i = 0 ; i <  relation_fact_size; i++ ){
		int found = 0;
		for( j=0;j<acc_group->master_object_num;j++)
		if ( acc_group->master_object[j] == relation_fact[i]->args[master_param]){
			found = 1;
			break;	
		}
		
		if(found) continue;		
		acc_group->master_object[acc_group->master_object_num++] = relation_fact[i]->args[master_param];
	}
	
	//init slave_object_list
	for(i=0;i<acc_group->master_object_num;i++){
		acc_group->slave_object_num[i] = 0;
		acc_group->slave_object[i] = malloc( sizeof(int) * NUM_SLAVE_EACH_MASTER );
		acc_group->slave_object_available[i] = malloc( sizeof(int)* NUM_SLAVE_EACH_MASTER ); //THIS SHOULD BE INIT AGAIN AFTER FINISHED
		for(j=0;j<NUM_SLAVE_EACH_MASTER;j++) acc_group->slave_object_available[i][j] = 1;
	}
	
	for( i=0 ; i<relation_fact_size ; i++){
		dis_Fact *fact = relation_fact[i];
		
		int found = 0;
		for( j=0; j<acc_group->master_object_num; j++ )
			if ( acc_group->master_object[j] == fact->args[master_param] ){
				found = 1;
				break;
			}
		
		if ( !found ) { printf("OOPS, SHIT!strang error here.\n"); return ; }
		
		found = 0;

		for(k=0;k<acc_group->slave_object_num[j];k++)
		if( acc_group->slave_object[j][k]==fact->args[slave_param] ){
			found = 1;
			break;
		}
		
		if(!found){
			int p = acc_group->slave_object_num[j];
			acc_group->slave_object[j][p] = fact->args[slave_param];
			acc_group->slave_object_num[j]++;
		}
	}
	
	if(acc_group->master_object_num==0 || acc_group->master_object_type == acc_group->slave_object_type ) acc_group->valid = 0;
	
	for(i=0;i<acc_group->master_object_num;i++){
		acc_group->master_object_available[i] = acc_group->slave_object_num[i];
	}
}

/*
 * This function search the shortest path in a single transitive graph(without any dependency.)
 * 
 * EXAMPLE:  focus_object = HOIST1;  init_prop_object = area1 ; goal_prop_object = area2;
 *
 */

int get_single_graph_distance(int focus_object, dis_AccompanyGroup *acg, int init_prop_object, int goal_prop_object){
	
	int i,j,k;
	int from_index=-1, to_index = -1 ;
	dis_TransitiveGraph *tg = NULL;
	//printf("###%d,%d,%d",focus_object, init_prop_object, goal_prop_object);
	for(i=0;i<num_transitiveGraph;i++){
		dis_TransitiveGraph *t = &(transitiveGraph[i]);
		if (t->invariantId!=0) continue;
		for(j=0;j<t->elementSize;j++){
			dis_Fact *fact = &(dis_grelevant_facts[t->element[j]]);
			//if( fact->args[acg->same_param_slave] != focus_object) continue;

			if ( fact->args[!acg->same_param_slave] == init_prop_object )
				from_index = j;
			else if ( fact->args[!acg->same_param_slave] == goal_prop_object )
				to_index = j;

			if(from_index!=-1 && to_index!=-1){
				tg = t;
				break;
			}
		}
		if ( tg!= NULL ) break;    // SHALL WE INTERATE ALL THE DOMAIN??
	}
		
	if (tg == NULL ) return 65535;
	if( from_index == to_index ) return 0;

	
	int visited[500];
	int expand_list[500];
	expand_list[0] = from_index;
	int expand_list_size = 1;
	int distance = 0;
	
	for( i=0;i<tg->elementSize;i++) visited[i] = 0;
	
	do{
		for( i=0 ; i< expand_list_size ; i++)
			visited[ expand_list[i] ] = 1;
/*  FOR DEBUG		
		printf("##########%d::::",distance);
		for(i=0;i<expand_list_size;i++)
			printf("%d;",expand_list[i]);
		printf("\n");
*/
		distance++;
		int tmp_list[500];
		int tmp_list_size = 0;
		
		for( i=0 ; i<expand_list_size; i++)
		for( j=0 ; j<tg->elementSize ; j++ ){
				dis_TransitiveEdge *edge = tg->edge[expand_list[i]*tg->elementSize+j];
				if ( edge!=NULL && edge->actionSize ) {		
					if( j == to_index ) return distance;
					if ( !visited[j] ) tmp_list[tmp_list_size++] = j;
				}
		}

		expand_list_size = 0;
		for(i=0;i<tmp_list_size;i++)
			expand_list[ expand_list_size++ ] = tmp_list[i];			
	}while(expand_list_size);
	
	return 65535;
}

/*
 * There are several slave object in each master object, 
 	INPUT:Given ptr to a accompanyGroup, and a slave object ID
 	OUTPUT: return the master_object_index(If exists);
 */
int get_acc_master_object_index_by_slave(dis_AccompanyGroup *group, int slave_object_id){
	int i,j;
	for(i=0;i<group->master_object_num;i++){
		for(j=0;j<group->slave_object_num[i];j++)
			if(group->slave_object[i][j]==slave_object_id)
				break;
		if ( j!=group->slave_object_num[i] )
			return i;
	}
	return -1;
}

int get_acc_master_object_index_by_id(dis_AccompanyGroup *group, int master_object_id){
	int i;
	for(i=0;i<group->master_object_num;i++)
		if ( group->master_object[i] == master_object_id )
			return i;
	return 	-1;
}

void acc_mark_unavail_slave_object( dis_AccompanyGroup *group, int slave_object_id ){
	int i,j;
	for(i=0;i<group->master_object_num;i++)
	for(j=0;j<group->slave_object_num[i];j++)
		if(group->slave_object[i][j]==slave_object_id){
			//printf("###########%d,%d\n",group->slave_object[i][j],group->slave_object_available[i][j]);
			group->slave_object_available[i][j] = 0;	
			return;
		}
}

int acc_get_slave_object_avail( dis_AccompanyGroup *group, int slave_object_id){
	int i,j;
	for(i=0;i<group->master_object_num;i++)
	for(j=0;j<group->slave_object_num;j++)
	if(group->slave_object[i][j]==slave_object_id)
		return group->slave_object_available[i][j];
}

int acc_init_object_availability( dis_AccompanyGroup *group ){
	int i,j;
	for( i=0 ; i<group->master_object_num ; i++ ){
		group->master_object_available[i] = group->slave_object_num[i];
		for( j=0 ; j<group->slave_object_num ; j++ ){
			group->slave_object_available[i][j] = 1;
		}
	}
}

/*
 * In this goal array, all the goal must be the same predicate. 
 * 
 * return the fact_id of transformed sub_goal.
 */
int transform_goal_into_slave_fact(int goal_fact, int *goal, int goal_num){
	
	int i,j,k;
	dis_AccompanyGroup *acc_group = &(accompanyGroup[0]);
	dis_Fact *goal_fact_ptr = &(dis_grelevant_facts[goal_fact]);
	
	int objectIndex = -1;
	int diff_param_master = !acc_group->same_param_master;
	int diff_param_slave = !acc_group->same_param_slave;
	
	if ( goal_fact_ptr->predicate != acc_group->master_fact->predicate ) return goal_fact;
	
	for( i = 0 ; i < acc_group->master_object_num;i++){
		if( goal_fact_ptr->args[diff_param_master] == acc_group->master_object[i] ){
			objectIndex = i;
			break;
		}
	}
	if( objectIndex==-1 ) return goal_fact;
	
	//printf("###");dis_print_dis_Fact( goal_fact_ptr ); printf("\n");
	
	int init_object_state = -1;  //This one is the object like: storearea1, which is located in current state;
	for(i=0;i<dis_ginitial_state.num_F;i++){
		dis_Fact *fact = &(dis_grelevant_facts[dis_ginitial_state.F[i]]);
		
		if( fact->predicate != acc_group->slave_fact->predicate ) continue;
		if( fact->args[acc_group->same_param_slave] == goal_fact_ptr->args[acc_group->same_param_master] )
			init_object_state  = fact->args[diff_param_slave];
	}
	if ( init_object_state == -1) { printf("IMPOSSIBLE ERROR. MAYBE ERROR HERE."); return goal_fact; }

	//int max_distance = 0;
	//int max_distance_index = -1;
	//int max_distance_object = -1;
	int min_distance = 99999999;
	int min_distance_index = -1;
	int min_distance_object = -1;
	for(i=0;i<acc_group->slave_object_num[objectIndex];i++){

		if ( acc_group->slave_object_available[objectIndex][i] ){

			int distance = get_single_graph_distance( goal_fact_ptr->args[acc_group->same_param_master], acc_group, init_object_state, acc_group->slave_object[objectIndex][i] );
			//printf("AFTER_GRAPH_DISTANCE,%d\n",distance);
			if( distance < min_distance ){
			//if( distance < 65535 && distance > max_distance ){
				min_distance = distance;
				min_distance_object = acc_group->slave_object[objectIndex][i];
				min_distance_index = i;
				//max_distance = distance;
				//max_distance_object = acc_group->slave_object[objectIndex][i];
				//max_distance_index = i;				
			}
		}
	}
	//printf("min_distance_object%d,%s\n",max_distance_object,dis_gconstants[max_distance_object]);
	
	if( min_distance_object == -1 ) return goal_fact;
	//if ( max_distance_object == -1 ) return goal_fact;
	
	for( i=0;i<dis_gnum_relevant_facts;i++){
		dis_Fact *fact = &(dis_grelevant_facts[i]);
		
		if( fact->predicate != acc_group->slave_fact->predicate ) continue;
		if( fact->args[diff_param_slave] == min_distance_object &&
			fact->args[acc_group->same_param_slave] == goal_fact_ptr->args[acc_group->same_param_master]){
			acc_group->slave_object_available[objectIndex][min_distance_index] = 0;
			return i;		
		}
	}

	printf("NOT ENOUGH FOR A slave_goal schedule. Maybe something wrong here.\n");
	return goal_fact;
}


void reduce_master_fact_in_state(int *goal, int *goal_num){
	int i,j;
	int do_replacement = 1;
	int new_goal[100];
	int new_goal_num = 0;
	
	if(num_accompanyGroup==0) return;
	dis_AccompanyGroup *acc_group = &( accompanyGroup[0] );  /// IMPORTANT, TO REDUCE THE COMPLEXITY, I JUST CONSIDER ONLY ONE HERE.
	
	//If we consider many acc_group, then have to use the array here, otherwise, just one is enough

	//int *master_fact_set[50];
	//int master_fact_set_size[50];
	//for(i=0;i<acc_group->master_object_num;i++)master_fact_set[i]=malloc(sizeof(int)*50);
	
	/*We consider a fact is a MASTER_FACT and should be reduced while:
	 *#1. The predicate name is same with ACC_GROUP->master_fact->predicate
	  #2. THE state_fact->args[master_fact_param] is included in ACC_GROUP->master_object;
	      Then we enclose this fact into the "TO_BE_REDUCED_LIST"(Which are MASTER_FACTs);
	 */
	for( i=(*goal_num)-1 ; i>=0 ; i-- ){
	//for( i=0 ; i<*goal_num ; i++ ){
		assert(goal[i] >= 0);
		dis_Fact *state_fact = &(dis_grelevant_facts[goal[i]]);
		if( acc_group->master_fact->predicate != state_fact->predicate ) continue;
		
		int v = transform_goal_into_slave_fact( goal[i] , goal, *goal_num );
		if (do_replacement)
			goal[i] = v;
		else{
			new_goal[new_goal_num++] = goal[i];
			goal[i] = v; 
		}
	}
	
	if ( !do_replacement ){
		for(i=0;i<new_goal_num;i++)
			goal[(*goal_num)++] = new_goal[i];
	}
}

void print_out_accompany_group_set(){
	int i,j,k;
	printf("\n*** ACCOMPANY GROUP SET ***\n");
	for(i=0;i<num_accompanyGroup;i++){
		dis_AccompanyGroup *acg = &(accompanyGroup[i]);
		if (!acg->valid) continue;
		
		printf("Set #%d:",i);
		printf(" MASTER_FACT:");
		dis_print_dis_Fact( acg->master_fact );
		printf(" SLAVE_FACT:");
		dis_print_dis_Fact( acg->slave_fact );
		printf(" MASTER_OBJECT_TYPE:%s;",dis_gtype_names[acg->master_object_type]);
		printf(" SLAVE_OBJECT_TYPE:%s;",dis_gtype_names[acg->slave_object_type]);
		printf("\n");
				
		for( j=0; j<acg->master_object_num; j++ ){
			
			printf("[%s]:",dis_gconstants[acg->master_object[j]]);
			for( k=0;k<acg->slave_object_num[j];k++)
				printf(" [%d]%s;",acg->slave_object[j][k],dis_gconstants[acg->slave_object[j][k]]);
			printf("\n");
		}
		printf("\n");
	}
	printf("*** END of ACCOMPANY GROUP SET ***\n");
}


void generate_accompany_group_set(){
	int i;
	for(i=0;i<num_accompanyGroup;i++)
		generate_accompany_group(i);
//	print_out_accompany_group_set();
}

void build_transitive_graph(){
	
	if( GpG.SearchModal == 3 ){
		invariantGroup[1].deleted = 1;
		invariantGroup[3].deleted = 1;
	}

	int i,j,k;
	num_transitiveGraph = 0;
	//delete_duplicate_invariant();
	//printf("START BUILDING TRANSITIVE GRAPH\n");
	int *groundedDomainMap = NULL;
	for(i=0;i<num_invariantGroup;i++){
		if( invariantGroup[i].deleted == 1 || invariantGroup[i].size==0 ) continue;
		
		//if( GpG.SearchModal == 3 && i == 0 ) continue;
		int object;
		
		int *candidate_fact = malloc( sizeof(int)* (dis_ginitial_state.num_F ) );
		int candidate_fact_num = 0;
		for(j=0;j<dis_ginitial_state.num_F;j++) candidate_fact[candidate_fact_num++] = dis_ginitial_state.F[j];
		//for(j=0;j<dis_gnum_flogic_goal;j++) candidate_fact[candidate_fact_num++] = dis_gflogic_goal[j];
	
		//End of Building Group for a simplified(Pregrounded domain.)//		
		if( invariantGroup[i].obj[0]->focus == -1 ){
			object = -1;
			/*
			weight= get_weight(&(invariantGroup[i]),&fact,object);						
			if (weight==1) new_transitive_group(&(invariantGroup[i]),fact,object);
			*/
			for ( k = 0; k < candidate_fact_num; k++ ) {
				dis_Fact *fact = &(dis_grelevant_facts[candidate_fact[k]]);
				if( get_valid_predicate_index( fact->predicate ) == -1 ) continue;
				dis_Invariant* invar = 	&(invariantGroup[i]);
				int valid = is_valid_atom( invar, &(dis_grelevant_facts[candidate_fact[k]]), object );
				if( valid ){
					int fact = dis_ginitial_state.F[k];
					new_transitive_group( invar , fact , object );
				}
			}

		} else{
			for(j=0;j<dis_gtype_size[invariantGroup[i].focusType];j++){
				int object = dis_gtype_consts[invariantGroup[i].focusType][j];

				/* 
				 *This code is for generating group with invariant of weight 1.*  DEPRECATED!!
				 *weight = get_weight( &(invariantGroup[i]) , &fact , object);
				 *printf("%dth invar,obj:%s, WEIGHT is%d\n",i,dis_gconstants[object], weight);
				 *if(weight==1) new_transitive_group(&(invariantGroup[i]),fact,object);
				 */

				for ( k = 0; k < candidate_fact_num; k++ ){
					dis_Fact* fact_ptr = &(dis_grelevant_facts[candidate_fact[k]]);
					if ( get_valid_predicate_index( fact_ptr->predicate ) == -1 ) continue;
					dis_Invariant* invar = 	&(invariantGroup[i]);
					int valid = is_valid_atom( invar, fact_ptr, object );

					if( valid ){
						int fact = dis_ginitial_state.F[k];
						new_transitive_group( invar , fact , object );
					}
				}
			}
		}
		free(candidate_fact);
	}

	if( groundedDomainMap != NULL ) free(groundedDomainMap);
	
	for(i=0;i<num_transitiveGraph;i++){
		transitiveGraph[i].order = i;
		build_edges(&(transitiveGraph[i]));
		//analyze_dependency_relationship(&(transitiveGraph[i]));
	}

	for(i=0;i<num_transitiveGraph;i++){
		analyze_dependency_relationship(&(transitiveGraph[i]));
	}
	//Build the list of those groups which depends on this one.	
	for(i=0;i<num_transitiveGraph;i++){
		int *list = malloc( sizeof(int) * num_transitiveGraph);
		int listSize = 0;
		
		for(j=0;j<num_transitiveGraph;j++){
			if (i==j) continue;
			int flag = 0;
			for( k=0; k<transitiveGraph[j].dependencyListSize; k++ )
			if ( transitiveGraph[j].dependencyList[k] == i ){
				flag = 1;
				break;
			}

			if ( flag==1 ) list[listSize++] = j;
		}
		
		transitiveGraph[i].dependOnThisList = malloc( sizeof(int) * listSize );
		transitiveGraph[i].dependOnThisListSize = listSize;
		for( j = 0 ; j<listSize ; j++ )
			transitiveGraph[i].dependOnThisList[j] = list[j];
		list = NULL;
	}
	generate_accompany_group_set();
//	print_out_accompany_group_set();
	//printf("TRANSITIVE GRAPH BUILD FINISHED.\n");
	/*
	if( GpG.SearchModal == 3 ){
		for(i=0;i<num_transitiveGraph;i++)
			if ( transitiveGraph[i].invariantId == 0 )
				transitiveGraph[i].dependencyListSize = 0;
	}
	*/
	//print_out_transitive_graph_set_simple();
	//printf("BUILD transitive graph finished.\n");
}
