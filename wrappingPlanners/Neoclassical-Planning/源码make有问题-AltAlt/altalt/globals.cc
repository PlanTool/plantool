/* 
* $Revision: 3.1 $
* $Date: 1999/01/06 10:15:43 $


globals.cc -- All the main global definitions. */

#include "globals.h"

#ifdef CANDSTATS
int lowestdepth = MAX_PLAN;
int parenttotal;
int Currentbest;
#define newlowest(x) lowestdepth = lowestdepth<x?lowestdepth:x
heritage * parent;
#endif

#ifdef HASHDEBUG
int counthits = 0;
#endif

#ifdef FINALSTATS
int final_level_count;
int initial_length;
#endif

/*RS: Added by RS*/
/*These are the different available options for ALtALt*/
int graphbuilt=0;
int level_out = 0;
int WHICHSEARCH=STANBASED;
int HEURISTYPE=HCOMBO;
int SERIAL=FALSE;
int PACTION=FALSE;
int NOLEVOFF=FALSE;
int REVCHECK=FALSE;
int GWEIGHT=5;
int GLEVEL=0; 
struct timeval pg_tstart, pg_tend, hsp_tstart, hsp_tend;
long totalgp, totalhsp;
/* RS End */


int exvecsizea;
int exvecsizef;
int no_facts;
int no_acts;
int hits;
int oldhits;

int * fixedreslim;
int * fixedres;
int * apprate;
int * appres;
char * * resnms;
int frCount;
int rCount;
int rStart;

char buff[20];

action * finalAcTable[MAX_ACTIONS];
int finacts_at[MAX_PLAN];

int facts_at[MAX_PLAN];
int acts_at[MAX_PLAN];
fact * fact_table[MAX_FACTS];
action * action_table[MAX_ACTIONS];
action_list available_acts;
int mutex_at[MAX_PLAN];
token_list goals_at[MAX_PLAN];
action_list committed_acts_at[MAX_PLAN];

#ifdef BADSTATS
int calls_at[MAX_PLAN];
int fail2s_at[MAX_PLAN];
#endif

#ifdef CHECKSTATS
int checks[MAX_PLAN][MAX_ACTIONS];
#endif

int acnt;

int stcs = 100;

#ifdef COUNTCANDS
int numCs = 0;
#endif

int top_level;

int checked = 0;

#ifdef BADSTATS
int badstats[MAX_PLAN];
#endif

fact * oGoals[MAX_GOALS];
int gdval[MAX_GOALS];
int chlen[MAX_GOALS];
int num_goals;
int gcs[EXVECSIZE];

#ifdef SYMMETRY
class object;
int * brokenSym;
int numAllSymObs;
object ** symDomobs;
int symSizes[MAX_ACTIONS];
#endif
