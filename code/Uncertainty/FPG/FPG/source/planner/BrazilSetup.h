/*
 * $Id: BrazilState.cpp 80 2006-06-14 05:18:04Z daa $ 
 * 
 * This file is part of the Brazil Planner. Copyright NICTA 2006.
 *
 * This file is commercial in confidence. You should no be looking at
 * it unless you are an employee of NICTA, a student who has signed
 * their IP to NICTA, or you have signed an NDA covering the Brazil
 * source code. If you are not one of these people we will poke out
 * your eyes with a gerkhin while forcing you to sing the Brazilian
 * national anthem.
 * 
 */

/** 
 * @file Includes, macros, and global vars needed for
 * the planning part of Brazil only. The name of the file
 * is poorly chosen because it's just for the planner, not
 * all of Brazil.
 */

///// Standard includes that don't change often

#include<exception>
#include<set>
#include<map>
#include<list>
#include<vector>
#include<iostream>

#ifndef NYI
// Not Yet Implemented
#define NYI { cerr<<__FUNCTION__<<" is not yet implemented in "<<__FILE__<<":"<<__LINE__<<endl; abort(); }
#endif

#ifndef BOUT
// Just the normal start to debugging messages for the planner
#define BOUT cout<<"<><>"<<__FUNCTION__<<" "
#endif

// Global variable for debug level
extern  int  brazilPlannerDebugLevel;

// Global variable for debug module
extern unsigned int brazilDebugModule;

// What are the separate entities for debug output, set 
// to individual bits. Corresponds closely to files.
enum DebugModules { 
    DBG_EVENTS = 1,
    DBG_STATE = 2,
    DBG_HELPERS = 4,
    DBG_UPDATERS = 8,
    DBG_SIMULATOR = 16,
    DBG_PLANNER = 32,
    DBG_SAMPLER = 64
};

#define DBG_MODULE(debugModule) static unsigned int fileDebugModule = debugModule;

// Default debug test
#define DBG(lvl) (((brazilPlannerDebugLevel >= lvl) && (brazilDebugModule & fileDebugModule))?true:false)

/** 
 * Debug test with overriding of local module name so that this debug test
 * can be triggered in multiple debug modes.
 */
#define DBGM(lvl, module) (((brazilPlannerDebugLevel >= lvl) && (brazilDebugModule & (module)))?true:false)




