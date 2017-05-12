


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
 *
 * File: search.h
 *
 * Description: headers of routines that search the state space
 *
 *              ADL version, Enforced Hill-climbing enhanced with
 *                           Goal-adders deletion heuristic
 *
 * Author: Joerg Hoffmann 2000
 *
 *********************************************************************/ 






#ifndef _SEARCH_H
#define _SEARCH_H



Bool do_enforced_hill_climbing( void );



Bool search_for_better_state( State *S, int h, State *S_, int *h_ );
void add_to_ehc_space( State *S, int op, EhcNode *father );
int expand_first_node( int h );



void hash_ehc_node( EhcNode *n );
Bool ehc_state_hashed( State *S );
Bool superior_ehc_state_hashed( State *S );
Bool superior_state( State *S1, State *S2 );
void reset_ehc_hash_entrys( void );



void extract_plan_fragment( State *S );
PlanHashEntry *hash_plan_state( State *S, int step );
PlanHashEntry *plan_state_hashed( State *S );
Bool same_state( State *S1, State *S2 );



Bool do_best_first_search( void );
void add_to_bfs_space( State *S, int op, BfsNode *father );
float state_cost( State *S, BfsNode *father );
void extract_plan( BfsNode *last );



void hash_bfs_node( BfsNode *n );
Bool bfs_state_hashed( State *S );
Bool superior_bfs_state_hashed( State *S );
int state_sum( State *S );



Bool result_to_dest( State *dest, State *source, int op );
Bool determine_source_val( State *source, int fl, float *val );
void copy_source_to_dest( State *dest, State *source );
void source_to_dest( State *dest, State *source );


void BubbleSort(float feld[],int feldop[], int anzahl);
Bool startover(OpConn op1,OpConn op2);
Bool startstart(OpConn op1,OpConn op2);
Bool endend(OpConn op1,OpConn op2);
Bool endstart(OpConn op1,OpConn op2);
Bool endover(OpConn op1,OpConn op2);
Bool depend(OpConn op1,OpConn op2);
float durativpert(void);

int pert(void);

Bool TestDependArt(TypedList *tpl,TokenList *t );
TypedList* getDependArtList(XMLOperator *op1,XMLOperator *op2);
Bool isOp1DepentonOp2(XMLOperator *op1,XMLOperator *op2);
void make_array_for_instance_operators(void);
float User_durative_pert(void);
void make_array_of_xml_operators(void);

TokenListOperator *search_for_instance_operator(TokenListOperator_pointer *tok_op_p, TokenList *op_name);
void BubbleSort_for_Userp(float feld[],XMLOperator_pointer *feldop, int anzahl); float User_pert(void);

float instanziate_LnfExpNode( State *S, LnfExpNode *n );

OpConn_list *make_seqplan_list();
void calculate_best_makespann();
float *calculate_best_makespann1(OpConn_list *current,float *makespann );
float durativ_pert_without_xml_dependence(OpConn *array);

float costMetric_without_TotalTime(const State* s);

void BubbleSort2(float feld[],OpConn *feldop, int anzahl);
/*float schedulpert(OpConn *array, int index); */
  float schedulpert();



#endif /* _SEARCH_H */
