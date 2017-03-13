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

 * File: dis_invariant.c

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

const int DEBUG = 1;

const int INVARIANT_BALANCE = -1;
const int INVARIANT_NO_IMPACT = -2;
const int INVARIANT_UNBALANCE = -3;
const int INVARIANT_FINDING_TYPE = 1;  // 1 or 0



typedef struct _dis_BalanceInfo{
	int type; 	//BalanceType; NO_IMPACT, UNBALANCE, BALANCE; -3, -2, -1
	int predicate;
	int focus;  //[-1..n]
	dis_Fact* fact; //Where the unbalance occur
	dis_Operator* action;
} dis_BalanceInfo;

int valid_predicate[100];
int valid_predicate_num = 0;
int valid_operator[100];
int valid_operator_num = 0;

dis_Invariant invariantGroup[200];
int num_invariantGroup = 0;
//dis_BalanceInfo balanceMap[dis_MAX_PREDICATES][dis_MAX_OPERATdis_ORS];
dis_BalanceInfo balanceMap[200][200];

dis_AccompanyGroup accompanyGroup[50];
int num_accompanyGroup;

int icmp(const void *p, const void *q){ 
    const int *m = p, *n = q; 
    return *m > *n ? 1 : (*m == *n ? 0 : -1); 
} 

//check balance of one effect  
int has_same_param(dis_Fact* fact1,  dis_Fact* fact2, int* p1, int* p2){
	int i,j;
	if( dis_garity[fact1->predicate]==0 || dis_garity[fact2->predicate]==0) return 1;
	
	for(i=0;i<dis_garity[fact1->predicate];i++)
	for(j=0;j<dis_garity[fact2->predicate];j++){
		if(fact1->args[i]==fact2->args[j]){
			*p1 = i;
			*p2 = j;
			return 1;
		}
	}
	return 0;
}

void print_out_invariant_group(dis_Invariant* group, int groupSize){
	int i,j,k;
	printf("*******************INVARIANT GROUPS *****************%d**\n",groupSize);
	for(i = 0 ; i<groupSize;i++){
		printf("Invariant #%d:(%d)(%d){",group[i].id,group[i].deleted,group[i].size);
		int size = group[i].size;
		//printf("###########focustType%d\n",group[i].focusType);
		if(group[i].obj[0]->focus==-1) printf("[NONE] | ");
			else printf("[%s] | ",dis_gtype_names[group[i].focusType]);
		
		for( j=0; j<size; j++){
			dis_InvariantElement* obj = group[i].obj[j];
			printf("(%s ",dis_gpredicates[obj->fact->predicate]);
			
			for(k=0;k<dis_garity[obj->fact->predicate];k++){
				if( obj->focus==k ) printf("%s,",dis_gtype_names[group[i].focusType]);
					else printf("#,");
			}
			printf("\b) ");
		}
		printf("\b}\n");
	}
}

void print_out_invariant(dis_Invariant* group){
		int j,k;
		printf("Invariant #%d:(%d)(%d){", group->id , group->deleted , group->size );
		int size = group->size;

		if(group->obj[0]->focus==-1) printf("[NONE] | ");
			else printf("[%s] | ",dis_gtype_names[group->focusType]);
		
		for( j=0; j<size; j++){
			dis_InvariantElement* obj = group->obj[j];
			printf("(%s ",dis_gpredicates[obj->fact->predicate]);
			
			for(k=0;k<dis_garity[obj->fact->predicate];k++){
				if( obj->focus==k ) printf("%s,",dis_gtype_names[ group->focusType ]);
					else printf("#,");
			}
			printf("\b) ");
		}
		printf("\b}\n");
}

void print_out_invariant_result(){
	print_out_invariant_group(invariantGroup,num_invariantGroup);
}

//Is the two predicate might occur together in a same action's add List, return 0
//else return 1
int is_mutex(dis_InvariantElement* element1, dis_InvariantElement* element2){

	int i, j;
	int b1, b2;

	b1 = b2 = 0;	
	for (i=0;i<dis_gnum_full_initial;i++)
	{
		if (dis_gfull_initial[i].predicate == element1->fact->predicate)
		{
			for ( j=0; j<dis_garity[element1->fact->predicate]; j++ ) 
				if ( element1->fact->args[j] != dis_gfull_initial[i].args[j])
					break;
			if (j == dis_garity[element1->fact->predicate])
				b1 = 1;
		}
		if (dis_gfull_initial[i].predicate == element2->fact->predicate)
		{
			for ( j=0; j<dis_garity[element2->fact->predicate]; j++ ) 
				if ( element2->fact->args[j] != dis_gfull_initial[i].args[j])
					break;
			if (j == dis_garity[element2->fact->predicate])
				b2 = 1;
		}
	}
	if (b1 == 1 && b2 == 1)
		return 0;
	
	for(i=0;i<valid_operator_num;i++){
		
		dis_Effect* e;
		dis_Literal* l;
		for( e = dis_goperators[valid_operator[i]]->effects ; e ; e=e->next ){
			b1 = b2 = 0;
			for( l=e->effects ; l ; l=l->next ){
				if(l->negated == INVARIANT_FINDING_TYPE ) continue;    //Need to check both not and add list;
				if(l->fact.predicate == element1->fact->predicate)  b1 = 1 ; 
				if(l->fact.predicate == element2->fact->predicate)  b2 = 1 ;
			}
			if(b1==1&& b2==1) return 0;
			
			b1 = b2 = 0;
			for( l=e->effects ; l ; l=l->next ){
				if(l->negated != INVARIANT_FINDING_TYPE ) continue;    //Need to check both not and add list;
				if(l->fact.predicate == element1->fact->predicate)  b1 = 1; 
				if(l->fact.predicate == element2->fact->predicate)  b2 = 1;
			}
			if( b1==1 && b2==1 ) return 0;
		}
	}
	return 1;
}


//Is invar1 sub set of invar2?
int is_sub_invariant(dis_Invariant* invar1, dis_Invariant* invar2){

	int i,j;
	if(invar1->size>invar2->size)	return 0;
	if( invar1->focusType!=0 && invar1->focusType != invar2->focusType) return 0;
	
	int flag = 0;
	for(i=0;i<invar1->size;i++){
		flag = 0;
		for(j=0;j<invar2->size;j++){	
			if( invar1->obj[i]->fact->predicate==invar2->obj[j]->fact->predicate 
				&& invar1->obj[i]->focus == invar2->obj[j]->focus){
				
				flag = 1;
				break;		
			}
		}
		if(flag==0) return 0;
	}
	return 1;
}

/* 
 *  If element belong to invar, return the pos of the same element in invar 
    or return -1;
 */
int is_belong_to(dis_InvariantElement *element, dis_Invariant* invar){
	int i;
	
	for(i=0;i<invar->size;i++){
		dis_InvariantElement* e = invar->obj[i];
		if( e->focus == element->focus && e->fact->predicate == element->fact->predicate )
			return i;
	}
	
	return -1;
}

void add_accompany_pair(dis_Fact *fact1, dis_Fact *fact2){
	
	int i,j,k;
	//printf("[[%d::%d]]" , fact1->predicate , fact2->predicate);
	if(fact1->predicate == fact2->predicate ) return;
	
	//If pair of (fact1,fact2) already exsit, just return
	for(i=0;i<num_accompanyGroup;i++){
		dis_AccompanyGroup *ag = &(accompanyGroup[i]);
		if ( (fact1->predicate == ag->master_fact->predicate && fact2->predicate==ag->slave_fact->predicate ) || 
				( fact1->predicate==ag->slave_fact->predicate && fact2->predicate==ag->master_fact->predicate ) )
			return;
	}
	
	accompanyGroup[num_accompanyGroup].valid = 1;
	accompanyGroup[num_accompanyGroup].master_fact = fact1;
	accompanyGroup[num_accompanyGroup].slave_fact = fact2;
	num_accompanyGroup++;
}

//Merge invar1 into invar2
int merge_invariant_element(dis_Invariant* invar1, dis_Invariant* invar2){
	int i;
	if( invar1->size!=2 ) return 0;
	
	int num = is_belong_to(invar1->obj[0],invar2);

	if( num!=-1 ){

		for(i=0;i<invar2->size;i++){
			if(i==num) continue;
			if( is_belong_to(invar1->obj[1],invar2) !=-1 ) continue;
						
			if( is_mutex(invar1->obj[1],invar2->obj[i])){
				invar2->obj[invar2->size] = invar1->obj[1];
				invar2->size++;
				return 1;				
			}

			if( invar2->size==2 ){
				//printf("****%s AND %s***",dis_gpredicates[invar1->obj[1]->fact->predicate],dis_gpredicates[invar2->obj[i]->fact->predicate]);	
				add_accompany_pair(invar1->obj[1]->fact,invar2->obj[i]->fact);
			}
		}
	}
	
	
	num = is_belong_to(invar1->obj[1],invar2);
	if(num!=-1){
		for(i=0;i<invar2->size;i++){
			if(i==num) continue;	
			if( is_belong_to(invar1->obj[0],invar2) !=-1 ) continue;
			

			if( is_mutex(invar1->obj[0],invar2->obj[i])){
				invar2->obj[invar2->size] = invar1->obj[0];
				invar2->size++;
				return 1;
			}
			
			if( invar2->size==2 ){
				//printf("****%s AND %s***",dis_gpredicates[invar1->obj[0]->fact->predicate],dis_gpredicates[invar2->obj[i]->fact->predicate]);	
				add_accompany_pair(invar1->obj[0]->fact,invar2->obj[i]->fact);
			}
		}
	}
		
	return 0;
}

void remove_invariant_subset(dis_Invariant* group, int* size){
	int i,j,k;
	int delMark[200];
	int delMarkSize = 0;
		
	for(i=0;i<*size;i++){
		dis_Invariant* invar1 = &group[i];
		

		for(j=0;j<*size;j++){
			if(i==j) continue;
			int flag = 0;
			for(k=0;k<delMarkSize;k++){
				if( delMark[k]==j ) {
					flag = 1;
					break;
				}
			}
			if( flag ==1 ) continue;
			
			dis_Invariant* invar2 = &group[j];
			if(is_sub_invariant(invar1,invar2)){ 
				delMark[delMarkSize] = i;
				delMarkSize++;
				break;
			}
		}
	}	
	
	int p = *size - delMarkSize;
	int pos = 0;
	for(i=0;i<p;i++){
		int r = i + pos;
		while( r == delMark[pos] && pos<delMarkSize ){
			pos ++;
			r++;
		}
		
		if(i==r) continue;
		group[i] = group[r];
	}
	*size = p;
}

//remove sub sets, and merge
void refine_invariant_group(dis_Invariant* group, int* size){
	int i,j,k, p, pos;
	int delMark[200];
	int delMarkSize = 0;
	
	remove_invariant_subset(group,size);
	
	delMarkSize = 0;
	//Starting from here, ******Merge***:
	for(i=0;i<*size;i++){
		dis_Invariant* invar1 = &group[i];
		//print_out_invariant(invar1);		
		int found = 1 ;
		while(found){
			found = 0;
			for( j=0 ; j<*size ; j++ ){
				if( i == j ) continue;
				dis_Invariant* invar2 = &group[j];
				
				//printf("--->");
				//print_out_invariant(invar2);
				if( merge_invariant_element(invar1, invar2) && delMark[delMarkSize-1] != i ){
					delMark[delMarkSize] = i;
					delMarkSize++;
					found = 1;
					break;
				}
			}
		}
	}
	
	//printf("Del Mark Size:%d\n",delMarkSize);
	//for(i=0;i<delMarkSize;i++) printf("%d;",delMark[i]);
	//printf("\n");
	
	p = *size - delMarkSize;
	pos = 0;
	for(i=0;i<p;i++){
		int r = i + pos;
		while( r == delMark[pos] && pos<delMarkSize ){
			pos ++;
			r++;
		}

		if(i==r) continue;
		group[i] = group[r];
	}
	*size = p;
	
	remove_invariant_subset(group,size);
	
	delMarkSize = 0;
	for(i=0;i<*size;i++)
	{
		dis_Invariant* invar1 = &group[i];
		for (j=0;j<invar1->size;j++)
		{
			for (k=j+1;k<invar1->size;k++)
				if (is_mutex(invar1->obj[j], invar1->obj[k]) == 0)
					break;
			if (k < invar1->size)
				break;
		}
		if (j < invar1->size)
		{
			delMark[delMarkSize++] = i;
			continue;
		}
	}

	p = *size - delMarkSize;
	pos = 0;
	for(i=0;i<p;i++){
		int r = i + pos;
		while( r == delMark[pos] && pos<delMarkSize ){
			pos ++;
			r++;
		}

		if(i==r) continue;
		group[i] = group[r];
	}
	*size = p;
	remove_invariant_subset(group,size);

	for(i=0;i<num_invariantGroup;i++){
		dis_Invariant *invar = &(invariantGroup[i]);
		invar->id = i;
		
		//If there is any obj has no param, then the focusType must be none.  I am not sure whether it is right.
		for( j=0 ; j<invar->size ; j++ )
			if( dis_garity[invar->obj[j]->fact->predicate]==0) break;		
		if ( j != invar->size ){
			invar->focusType = -1;
			for( j=0 ; j<invar->size ; j++ )
				invar->obj[j]->focus = -1;
		}
	}
}


void add_to_invariant_groupset(dis_Invariant *invar, dis_Fact *fact, int focus1 , int focus2){

	int i;

	for(i=0;i<invar->size;i++){
		if( invar->obj[i]->fact->predicate == fact->predicate ) return;	
	}

	dis_InvariantElement *element = malloc(sizeof(dis_InvariantElement));
	element->fact = fact;
	
	if(invar->size==0){	
		//nothing to do
	}else if(invar->size==1){
		invar->obj[0]->focus = focus1;
		element->focus = focus2;
	}else if(invar->size>1){
		element->focus = focus2;
	}else printf("EXCEPTION while add_to_invariant_groupset.\n");
	
	invar->obj[invar->size] = element;
	invar->size++;
}

int get_valid_predicate_index(int predicateId){
	int i;
	for(i=0;i<valid_predicate_num;i++)
		if( valid_predicate[i] == predicateId ) return i;	
	return -1;
}

int get_valid_operator_index( int operatorId ){
	int i;
	for( i=0 ; i < valid_operator_num ; i++ )
		if( valid_operator[i] == operatorId ) return i;
	return -1;
}

//repaire unbalance invariants and add them to the Invariant_GROUP_SET
void repair_unbalance_invariants(){
	int i,j,k;
	
	for( i=0 ; i<valid_predicate_num; i++ ){
		//printf("*************************CHECKING PREDICATE %s*************************\n",dis_gpredicates[i]);
		dis_Invariant invarSet[100];
		int invarSetSize = 0;
		for(j=0;j<100;j++){
			invarSet[j].size = 0;
			invarSet[j].deleted = 0;
		}
		
		for(j=0;j<valid_operator_num;j++){	
			//printf("Checking Action(%s) BalanceType(%d)---------------------------\n",dis_goperators[j]->name,balanceMap[i][j].type);
			dis_BalanceInfo *info = &(balanceMap[i][j]);
			if(info->type!=INVARIANT_UNBALANCE) continue;
			
			dis_Operator *action = dis_goperators[valid_operator[j]];
			dis_Effect *e;
			dis_Literal *l;
			dis_Fact* f = info->fact;

			//int flag = 0;
			for( e=action->effects ; e ; e=e->next )
			for( l=e->effects ; l ; l=l->next ){
				int pos1, pos2;
				//printf("    -%s(%d)\n",dis_gpredicates[l->fact.predicate],l->negated);
				if( l->negated != INVARIANT_FINDING_TYPE ) continue;
				//if( balanceMap[l->fact.predicate][j].type == INVARIANT_BALANCE ) continue; //this line is commented out, but I am not sure right or not.

				if( has_same_param( f, &(l->fact), &pos1, &pos2 ) == 0 ) continue;
				//printf("     -Chosen and Processing (%s)\n",dis_gpredicates[l->fact.predicate]);
								
				//test the combination of fact and l->fact
				int testSuccess = 1;
				for(k=0;k<valid_operator_num;k++){
					if(balanceMap[get_valid_predicate_index(f->predicate)][k].type == INVARIANT_UNBALANCE &&
						balanceMap[get_valid_predicate_index(l->fact.predicate)][k].type == INVARIANT_UNBALANCE ){
							testSuccess = 0;
					}
				}
				if( testSuccess==0 ) continue;

				dis_Invariant* invar = &invarSet[invarSetSize];
				invar->size = 0;
				invarSetSize++;
				add_to_invariant_groupset( invar, f, 0, 0);

				//if test passed, add to the [Invariant]
				add_to_invariant_groupset( invar , &(l->fact) , pos1 , pos2);
				/* 
				   Notice!!! Here becuase the grounded fact and predicate use the same data structure, 
				   so for DOMAINS like airport which is actually grounded may get different pos,pos2 value, i.e, negative number, indicates that it
				   is actually ground fact.
				*/

				if( invar->obj[0]->focus == -1 || pos1<0 || pos2<0 )  invar->focusType = -1;
				else 
					invar->focusType = f->args[pos1]>=0? f->args[pos1] :action->var_types[dis_DECODE_VAR(f->args[pos1])];
				
				//The invariant we got here is supposed to be of size 2, so if not, discard it.
				if( invar->size != 2 ) invarSetSize--;
			}
		}
		
		refine_invariant_group(invarSet,&invarSetSize);
		
		//add the temporary list for one given predicate to global [Invariant group set];
		for(j=0;j<invarSetSize;j++)
		if( invarSet[j].size>1 ){
			invariantGroup[num_invariantGroup] = invarSet[j];
			num_invariantGroup++;
		}
	}
}

void generate_balance_invariants(){
	int i,j,k;
	
	for(i=0;i<valid_predicate_num; i++){
		
		for(j=0;j<valid_operator_num;j++){
			dis_Operator* action = dis_goperators[j];
			if(balanceMap[i][j].type == INVARIANT_BALANCE ){
				
				//Dectec whether this invariant is already exist in the group or not.
				int flag = 1;
				for(k=0;k<num_invariantGroup;k++){
					dis_InvariantElement *tmp = invariantGroup[k].obj[0];					
					if( tmp->fact->predicate ==i && tmp->focus==balanceMap[i][j].focus ){
						flag = 0;
						break;
					}
				}
				if (flag==0) continue;
				
				dis_BalanceInfo *info1 = &balanceMap[i][j];
				for(k=0;k<valid_operator_num;k++){
					
					if( balanceMap[i][k].type != INVARIANT_UNBALANCE ) continue;
					
					//Whether it is balance or not, if some predicate is unbalance in another action, then it can not be a invariant
					//But some times like (at plane place) is balance but (at crate palace) is not, so we have to check the whether XXX in at( XXX,#) is the same type.
					dis_BalanceInfo *info2 = &balanceMap[i][k];
					int type1 = 0, type2 = 0;
					
					if( info1->focus!= -1  && info1->fact->predicate == info2->fact->predicate ){
						type1 = info1->action->var_types[dis_DECODE_VAR(info1->fact->args[info1->focus])];
						type2 = info2->action->var_types[dis_DECODE_VAR(info2->fact->args[info1->focus])];
					}
					
					if( type1 == type2 ){
						flag = 0;
						break;
					}
				}
				if(flag==0) continue;
				
				dis_InvariantElement *invar = malloc(sizeof(dis_InvariantElement));
				invar->focus = balanceMap[i][j].focus;
				invar->fact = balanceMap[i][j].fact;
				invariantGroup[num_invariantGroup].obj[0] = invar;
				invariantGroup[num_invariantGroup].size = 1;
				invariantGroup[num_invariantGroup].deleted = 0;
				if( invariantGroup[num_invariantGroup].obj[0]->focus == -1 )  invariantGroup[num_invariantGroup].focusType = -1;
					else invariantGroup[num_invariantGroup].focusType = 
						invar->fact->args[invar->focus]>-1? invar->fact->args[invar->focus] :action->var_types[dis_DECODE_VAR(invar->fact->args[invar->focus])];
				num_invariantGroup++;
			}	
		}
	}
}

void print_out_balance_map(){
	int i,j;
	
	for(i=0;i<valid_predicate_num; i++){
		printf("For Predicate:%s\n",dis_gpredicates[ valid_predicate[i] ]);
		for(j=0;j<valid_operator_num;j++){
			printf("   Action:(%s) ",dis_goperators[valid_operator[j]]->name);
			dis_BalanceInfo *info = &(balanceMap[i][j]);
			if( info->type==-3){
				printf("%d,%d,UNBALANCE",i,j);
			}else if( info->type==-2){				
				printf("NO IMPACT");	
			}else if( info->type==-1){
				printf("BALANCE");
			}else printf("%d",info->type);
			printf("\n");
		}
	}
}

void filter_valid_definition(){
	int i;
	valid_predicate_num = valid_operator_num = 0;
	for(i=0;i<dis_gnum_predicates;i++)
		if ( strncmp(dis_gpredicates[i] , "_" , strlen("_") ) != 0 )
			valid_predicate[valid_predicate_num++] = i;
	
	for(i=0;i<dis_gnum_operators;i++)
		if ( strncmp(dis_goperators[i]->name,"_",strlen("_")) != 0 )
			valid_operator[valid_operator_num++] = i;
}
 
/*  
    对每个Action判断这个Predicate是不是Balance
    如果对于每个Action都是返回结果INVARIANT_NO_IMPACT，那么这个Predicate 不是INVARIANT;
    只要有一个是UNBALANCED,那么这个INVARIANT就是UNBALANCED了，需要修补
    只要没有UNBALANCED存在，那么每找出来的一对{PREDICATE,PARAM_ORDER,TYPE}就是全都是一个独立的INVARIANT。
*/
void build_balance_relation_map(){
	
	int order,j,k;
	dis_Literal *l;
	dis_Effect *e;
	dis_Fact *addPos = NULL;
	dis_Fact *notPos = NULL;
		
	filter_valid_definition();

	//printf("PREDICATE:%d,%d\n",valid_predicate_index,dis_gnum_predicates);
	//printf("PREDICATE:%d,%d\n",valid_operator_index,dis_gnum_operators);
	//if( valid_operator_index == dis_gnum_operators || valid_predicate_index == dis_gnum_predicates ) printf("ERROR>No valid operator|predicate.\n");

	for( order=0; order< valid_predicate_num ; order++ ) 
	for( j=0 ; j<valid_operator_num; j++ ){
		
		dis_Operator *action = dis_goperators[valid_operator[j]];
		for( e = action->effects ; e ; e = e->next){
			
			addPos = NULL;
			notPos = NULL;
			for( l=e->effects; l ; l=l->next){
				dis_Fact *f = &(l->fact);
				//printf("       --Checking%d,%s\n",f->predicate,dis_gpredicates[f->predicate]);
				if( f->predicate==order){
					if( l->negated == INVARIANT_FINDING_TYPE ) notPos = f;
						else addPos = f;
				}
			}
			
			dis_BalanceInfo *info = &(balanceMap[order][j]);
			info->action = action;
			
			if( addPos==NULL ) info->type = INVARIANT_NO_IMPACT;
			else if( notPos==NULL){
				info->fact = addPos;
				info->type = INVARIANT_UNBALANCE;
			}
			else{
				info->type = INVARIANT_BALANCE;
				info->fact = addPos;
				
				int pos = -1;
				for( k = 0; k<dis_garity[addPos->predicate]; k++ ){
					if( addPos->args[k] == notPos->args[k] ){
						pos = k;
						break;
					}
				}
				info->focus = pos;
			}
		}
	}
}


void calc_invariants(){
	int i;
	num_accompanyGroup = 0;
	//INVARIANT_FINDING_TYPE = 1;
	build_balance_relation_map();
	//print_out_balance_map();
	generate_balance_invariants();
	repair_unbalance_invariants();
	refine_invariant_group(invariantGroup,&num_invariantGroup);
	
	//print_out_invariant_result();	
}
