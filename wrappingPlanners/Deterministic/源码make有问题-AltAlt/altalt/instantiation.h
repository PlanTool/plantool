/* 
* $Revision: 3.1 $
* $Date: 1999/01/06 10:15:43 $



instantiation.h */

#ifndef __INSTANTIATION
#define __INSTANTIATION

#include "alldefs.h"

void build_all_actions(oplistptr,objectlist);
char * catstrs(propptr);
char * catstrsPDDL(propptr);
int appRate(char *,opptr);
void filter_actions();

#endif
