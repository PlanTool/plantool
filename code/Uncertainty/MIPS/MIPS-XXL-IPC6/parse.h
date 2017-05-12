

/*********************************************************************
 * (C) Copyright 2002 Albert Ludwigs University Freiburg
 *     Institute of Computer Science
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 * 
 *********************************************************************/




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
 * File: parse.h
 * Description: Functions for the pddl parser
 *
 * Author: Frank Rittinger 1998 / Joerg Hoffmann 1999
 *
 *********************************************************************/ 





#ifndef _PARSE_H
#define _PARSE_H



char *copy_Token( char *s );
TokenList *copy_TokenList( TokenList *source );
void strupcase( char *from );
char *rmdash( char *s );



void build_orig_constant_list( void );
void collect_type_names_in_pl( PlNode *n );
int get_type( char *str );
void make_either_ty( TypedList *tyl );
void make_either_ty_in_pl( PlNode *n );
void normalize_tyl_in_pl( PlNode **n );
void extract_Timed_Initial_Literals(void);
void split_Preconds_inPlOperator( PlOperator *plop );




Bool make_adl_domain( void );
Bool make_conjunction_of_atoms( PlNode **n );
Bool is_wff( PlNode *n );
Bool make_effects( PlNode **n );
Bool is_eff_literal( PlNode *n );
Bool make_conjunction_of_literals( PlNode **n );
Bool CompareTwoPre(TokenList *t1, TokenList *t2);
Time_Ini_Literal *SearchPinListPre(TokenList *p);
Bool Search_Init_Time_Literals_in_P_Pre(char *s);
void setComplementTIL( void );

PlNode *extract_preference_from_goal(PlNode *p);
void constraints_to_ltl(WffNode *n, PlNode *m);
void run_constraints_to_ltl( WffNode *n );
void run_constraints_to_ltl2( WffNode *n, int j, Bool changed, FILE *in );
PlNode *extract_constraints_for_timed_initial_literal( PlNode *n );
PlNode *extract_constraints_for_within(PlNode *n);
int read_gtil_constraints(WffNode *n, FILE *out);
void read_gtil_constraints2(WffNode *n,int j, FILE *out);
void add_til_and_within_constraints_pref_fact_to_predicates(WffNode *n,char s[40], FILE *out);
void add_til_and_within_constraints_pref_fact_to_predicates2(WffNode *n,int j,char s[40], FILE *out);
void add_is_violated_til_constraints_pref_to_function(WffNode *n, FILE *out);
void add_is_violated_til_constraints_pref_to_function2(WffNode *n,int j, FILE *out);
int read_gwithin_constraints(WffNode *n,char s[40], FILE *out);
void read_gwithin_constraints2(WffNode *n, int j,char s[40], FILE *out, Bool changed, FILE *in2);
void preference_simple_node(WffNode *n, PlNode *m);
void preference(WffNode *n);
int create_operator_for_gtil_constraints(WffNode *n,char s[40], FILE *out);
void create_operator_for_gtil_constraints2(WffNode *n, int j,char s[40], FILE *out);
char* exchange(char* str);
char* exchange2(char* str);
void change_within_exp_to_ltl(WffNode *n, char *str, FILE *out);

Bool  mark_preference_in_goal(PlNode *p);
PlNode *make_PlNode( PlNode *p );
Bool mark_at_end_in_constraints( PlNode *p);
PlNode *extract_at_end_from_constraints( PlNode *p);
void add_at_end_constraints_to_goal(PlNode *p1, PlNode *p2);
void cleanup_all_wff(WffNode *w);
void cleanup_all_wff_pref(WffNode *w);

ParseExpNode *copy_ParseExpNode( ParseExpNode *source );

void splitt_ltl_formel_for_constraints(WffNode *n, PlNode *m);
void splitt_ltl_formel(WffNode *n);



#endif /* PARSE */
