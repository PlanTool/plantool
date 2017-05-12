/*********************************************************************
 * File: grounded.h
 * Description: headers for create grounded domain and problem.
 *
 *
 * 
 *
 *********************************************************************/ 


#ifndef _GROUNDED_H
#define _GROUNDED_H


void output_grounded_STRIPS_task( void );
void output_grounded_STRIPS_Fluents_temporal_task( void );
void fprint_Fact( FILE *out, Fact *f );
void fprint_ft_name( FILE *out, int index );
void fprint_op_name( FILE *out, Action *a );
void fprint_hidden_TokenList(FILE *out, TokenList *list, char *sep );
void fprint_fl_name( FILE *out, int index );
void fprint_ExpNode( FILE *out, ExpNode *n );
void fprint_ExpNode2( FILE *out, ExpNode *n );
void fprint_Fluent( FILE *out, Fluent *f );
void fprint_Fluent2( FILE *out, Fluent *f );
void fprint_Action(FILE *ops, Action *a);
void fprint_LnfExpNode( FILE *out, LnfExpNode *n );
void fprint_ParseExpNode( FILE *out, ParseExpNode *n );
void fprint_FinalExpNode( FILE *out, ParseExpNode *n );
void fprint_Wff(FILE *out, WffNode *n );

TokenList *int_to_TokenList(int index);
Bool fl_isSame_to_isViolated( int index );
Bool same_Fact(Fact *fact1, Fact *fact2);
Bool preference_exp_satisfy( WffNode *n);
Bool preference_exp_satisfy2(WffNode *n);
Bool same_Fluent(Fluent *fl1, Fluent *fl2);
void fprint_Wff_before_action_application1(FILE *out, WffNode *n, float value);
void fprint_Wff_before_action_application2(FILE *out, WffNode *n, float value);
Bool is_State_meet_preference_expression(WffNode *current );
Bool is_effect_meet_preference(ActionEffect *e, float *value, WffNode *current, int *l);
float is_effect_meet_numeric_preference(Fluent *fl, ActionEffect *e, int *l);
Bool fprint_Wff_for_or_node(FILE *out, ActionEffect *e, WffNode *n);
Bool fprint_Wff_for_and_node(FILE *out, ActionEffect *e, WffNode *n);
Bool fprint_Wff_for_or_expnode(FILE *out, ActionEffect *e, WffNode *n, float *value, Bool bol);
Bool fprint_Wff_for_and_expnode(FILE *out, ActionEffect *e, WffNode *n, float *value, Bool bol);
int same_pref_name(char *s, char *item );
void fluent_have_new_name(ParseExpNode *n, ParseExpNode *n1, ParseExpNode *n2, ParseExpNode *father);
void change_ParseExpNode( ParseExpNode *n, ParseExpNode *father );
void output_grounded_SIMPLEADL_task( void );
void output_grounded_ADL_task( void );
Bool is_State_meet_preference_preconds(WffNode *current, Action *a );
void fprint_ADL_Action(FILE *ops, Action *a);
Bool isAutomaton(char *s);
Bool isWithinAutomaton(char *s);

void  generate_gpref_predicate( WffNode *n);
void generate_Fact_for_gpref_predicate(  Fact *f);
void remove_not_found_IsViolated_from_ParseExpNode( ParseExpNode *n , ParseExpNode *father);
void print_FinalExpNode( ParseExpNode *n );
Bool found_violated_or_not(WffNode *n, float value );


#endif /* _GROUNDED_H */
