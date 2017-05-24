/*
* $Revision: 3.1 $
* $Date: 1999/01/06 10:15:43 $



search.h */

#ifndef __SEARCH
#define __SEARCH

#include "facts.h"

extern int maxtried[MAX_PLAN];
extern int tried[MAX_PLAN];
void only_mark_dominations(int,token_list);
int search_plan(int,token_list);
int find_plan(token_list,token_list);
void retrieveValidActions(int ln);

#endif
