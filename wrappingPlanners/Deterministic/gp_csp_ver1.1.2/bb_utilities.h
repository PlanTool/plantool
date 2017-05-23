/*
 * BlackBox 
 */

#ifndef __UTILITIES_H_
#define __UTILITIES_H_

#include "graphplan.h"
char* lookup_instantiation (instantiation_list insts, char *name);
void free_instantiation (instantiation_list insts);
fact_list fact_list_append(fact_list f1, fact_list f2);
fact_list token2fact (token_list tlist);
token_list token_list_append(token_list f1, token_list f2);
token_list str2token (char *str);
token_list strdup2token (char *str);
token_list dup_token_list (token_list tlist);
char* bbstrdup (char* yytext);
void print_token_list (token_list t);
void print_fact_list (fact_list f);
void print_actions (op_list op);

#endif
