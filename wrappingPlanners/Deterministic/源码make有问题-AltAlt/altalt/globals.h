/* 
* $Revision: 3.1 $
* $Date: 1999/01/06 10:15:43 $


globals.h -- All the main global definitions and stuff. */

#ifndef __GLOBALS
#define __GLOBALS

#include "alldefs.h"
#include <stream.h>
#include <stdio.h>
#include <fstream.h>
#include <new.h>
#include <sys/time.h>
#include "switches.h"


extern "C" domptr yyparse();
extern "C" void printresults();
extern "C" domptr topdom;
extern "C" predlist allpreds;
extern "C" char * pname,dname;
extern "C" FILE * yyin;
extern "C" void printop(opptr);
extern char * catstrs(propptr);
extern char * catstrsPDDL(propptr);
extern void retrieveValidActions(int ln);

#define MAX_HASH 50 //Increase to 100
#define MAX_FACTS 5122 //10244 For some domains this constant needs to be increased
#define MAX_GOALS 200
#define MAX_ACTIONS 5122 //10244 
#define MAX_PLAN 1024
#define EXVECSIZE 160 //Increase to 320 
#define BIGPRIME 8000977
#define TRUE 1
#define FALSE 0


/* RS Added */
#define MAX_PROPS 100 //It was 50 //It should be more...
#define HSPBASED 100
#define STANBASED 200
#define HPLUS 101
#define HLEVEL 102
#define HPART1 103
#define HPART2 104
#define HPART2C 105
#define HCOMBO 106
#define HADJSUM1 107
#define HADJSUM2 108
#define HADJSUM2M 109

#define STOP -100
#define MAXATOMS 10*1024

#define min(x,y) x<y?x:y;
#define max(x,y) x>y?x:y;

/* RS End */


/* A vast array of compiling options. Not all of these can be switched
freely - the debug options will usually be OK, and produce more output
from a variety of places, some useful and some less so. */

/* Reporting and debugging options */

#define HSPSEARCH //ADDED BY RS...
//#define DEBUG
//#define DEBUG1
//#define DEBUG2
#define VERBOSE
//#define APPRATE
//#define HASHDEBUG
//#define BADDEBUG
//#define SEARCHDEBUG
//#define FINALSTATS
//#define TRYDEBUG
//#define HITDEBUG
//#define MUTEXFACTSDEBUG
//#define FIXEDRES
//#define BADSTATS
//#define CHECKSTATS
//#define REPORTACTS
//#define EXCLUSIONS
//#define DOMINATION
//#define CANDIDATESOUTPUT

/* Some compilation options */

/* There are several key switches in switches.h including:

#define SYMMETRY
#define SYMMETRYOUT

*/

/* WFOFF will switch the wave-front off and DOMOFF turns off some
simple goal-ordering machinery. Some of the machinery is active regardless
of this switch - the switch only determines whether the orderings are applied.
Some recent work has improved this, but it will not be available until the
next release. */

#define DOMOFF
//#define WFOFF

/* Turning DDB on at the wavefront can give dramatic performance boosts, 
but at a cost of loss of completeness (optimal plans lost in some cases).
Some recent work  */

#define DDBOFFATWF

/* This switches EBL off at the wave front. It now seems better left on! */

//#define EBLOFFATWF

/* Heuristic - turned off because it affects completeness. The inversions
stuff is much weaker, but tries to do something similar without affecting
completeness. */

//#define NOREUSE


/* This code could be useful: the test for action_set_minimal is used to check
that at least one non-noop action is used at each layer (which otherwise 
leads to a badgoal set test), and whether any actions are redundant. In the
latter case we would reject the plan as sub-optimal. At the moment this is
turned off because of interactions with EBL/DDB stuff which we have yet to 
resolve properly. */

//#define ACTIONSETMINIMAL

/* This switch will allow candidates to be counted. */

//#define COUNTCANDS

/* This should be switchable, but there is a minor change to some code
which means it will not compile without this set at the moment - not
much point in switching it, anyway, since it just makes STAN use our
original domain syntax, which requires an older parser and stuff. */

#define PDDL

/* Makes STAN take an extra argument for output file and write results
to that. Also switches to less pretty output. BUT it does turn on the
more graceful exits from memory allocation failures. */

//#define PDDLOUTPUT

#ifdef PDDLOUTPUT
void memerr();
#endif


#ifdef HASHDEBUG
extern int counthits;
#endif

#ifdef FINALSTATS
extern int final_level_count;
extern int initial_length;
#endif

/*RS: Added by RS*/
extern int level_out;
extern int graphbuilt;
extern int WHICHSEARCH;
extern int HEURISTYPE;
extern int SERIAL;
extern int GWEIGHT;
extern int GLEVEL;
extern int PACTION;
extern int NOLEVOFF;
extern int REVCHECK;
extern struct timeval pg_tstart, pg_tend, hsp_tstart, hsp_tend;
extern long totalgp, totalhsp;

/*RS End*/

extern int exvecsizea;
extern int exvecsizef;
extern int no_facts;
extern int no_acts;
extern int hits;
extern int oldhits;

extern int * fixedreslim;
extern int * fixedres;
extern int * apprate;
extern int * appres;
extern char * * resnms;
extern int frCount;
extern int rCount;
extern int rStart;

extern char buff[20];

class fact_level_info;
class action;
class action_list_node;
typedef action_list_node * action_list;
class token_list_node;
typedef token_list_node * token_list;
class hash_entry;
typedef hash_entry fact;

extern  action * finalAcTable[MAX_ACTIONS];
extern int finacts_at[MAX_PLAN]; //Added ...

extern int facts_at[MAX_PLAN];
extern int acts_at[MAX_PLAN];
extern fact * fact_table[MAX_FACTS];
extern action * action_table[MAX_ACTIONS];
extern action_list available_acts;
extern int mutex_at[MAX_PLAN];
extern token_list goals_at[MAX_PLAN];
extern action_list committed_acts_at[MAX_PLAN];

#ifdef BADSTATS
extern int calls_at[MAX_PLAN];
extern int fail2s_at[MAX_PLAN];
#endif

#ifdef CHECKSTATS
extern int checks[MAX_PLAN][MAX_ACTIONS];
#endif

extern int acnt;
extern int stcs;

#ifdef COUNTCANDS
extern int numCs;
#endif

extern int top_level;

extern int checked;

#ifdef BADSTATS
extern int badstats[MAX_PLAN];
#endif

extern fact * oGoals[MAX_GOALS];
extern int gdval[MAX_GOALS];
extern int chlen[MAX_GOALS];
extern int num_goals;
extern int gcs[EXVECSIZE];

#ifdef SYMMETRY
class object;
extern int * brokenSym;
extern int numAllSymObs;
extern object ** symDomobs;
extern int symSizes[MAX_ACTIONS];
#endif

#endif
