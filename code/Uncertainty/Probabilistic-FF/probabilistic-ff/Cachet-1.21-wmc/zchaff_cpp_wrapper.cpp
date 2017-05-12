/*********************************************************************
 Copyright 2000-2003, Princeton University.  All rights reserved. 
 By using this software the USER indicates that he or she has read, 
 understood and will comply with the following:

 --- Princeton University hereby grants USER nonexclusive permission 
 to use, copy and/or modify this software for internal, noncommercial,
 research purposes only. Any distribution, including commercial sale 
 or license, of this software, copies of the software, its associated 
 documentation and/or modifications of either is strictly prohibited 
 without the prior consent of Princeton University.  Title to copyright
 to this software and its associated documentation shall at all times 
 remain with Princeton University.  Appropriate copyright notice shall 
 be placed on all software copies, and a complete copy of this notice 
 shall be included in all copies of the associated documentation.  
 No right is  granted to use in advertising, publicity or otherwise 
 any trademark,  service mark, or the name of Princeton University. 


 --- This software and any associated documentation is provided "as is" 

 PRINCETON UNIVERSITY MAKES NO REPRESENTATIONS OR WARRANTIES, EXPRESS 
 OR IMPLIED, INCLUDING THOSE OF MERCHANTABILITY OR FITNESS FOR A 
 PARTICULAR PURPOSE, OR THAT  USE OF THE SOFTWARE, MODIFICATIONS, OR 
 ASSOCIATED DOCUMENTATION WILL NOT INFRINGE ANY PATENTS, COPYRIGHTS, 
 TRADEMARKS OR OTHER INTELLECTUAL PROPERTY RIGHTS OF A THIRD PARTY.  

 Princeton University shall not be liable under any circumstances for 
 any direct, indirect, special, incidental, or consequential damages 
 with respect to any claim by USER or any third party on account of 
 or arising from the use, or inability to use, this software or its 
 associated documentation, even if Princeton University has been advised
 of the possibility of those damages.
*********************************************************************/

#include "zchaff_solver.h"
#include "zchaff_clsgen.h"
#include <fstream>

#ifndef SAT_Manager
#define SAT_Manager void *
#endif

//#define GEN_CALL_TRACE

#ifdef GEN_CALL_TRACE
#define TRACE(x)	x
ofstream trace_os("sat_call_trace");
#else
#define TRACE(x)	
#endif

/*=====================================================================
Following are wrapper functions for C/C++ callers. 

 ====================================================================*/

 SAT_Manager SAT_InitManager(void)
{
    CSolver * solver = new CSolver;
    TRACE(trace_os << "SAT_InitManager\t" << solver << endl;);
    return (SAT_Manager)solver;
}

//void SAT_SetVarWeight(SAT_Manager mng, vector <double> & var_weight)
void SAT_SetVarWeight(SAT_Manager mng, void * weight)
{
	CSolver * solver = (CSolver*) mng;
	vector<double> * var_weight = (vector<double> *) weight;
	solver->set_var_weight(var_weight);
}

void SAT_SetCacheSize(SAT_Manager mng, unsigned cache_size)
{
	CSolver * solver = (CSolver*) mng;
	solver ->set_cache_size(cache_size);
	return;
}


void SAT_SetBNNodes(SAT_Manager mng, int original)
{
	CSolver * solver = (CSolver*) mng;
	solver->set_BN_node(original);
}

void SAT_SetVGO(SAT_Manager mng, vector< set<int> > vgo)
{
	CSolver * solver = (CSolver*) mng;
	solver ->set_vgo(vgo);
	return;
}

void SAT_SetStaticHeuristic(SAT_Manager mng, bool static_heuristic)
{
	CSolver * solver = (CSolver*) mng;
	solver ->set_static_heuristic(static_heuristic);
	return;
}


void SAT_SetFarBacktrackFlag(SAT_Manager mng, bool far)
{
	CSolver * solver = (CSolver*) mng;
	solver ->set_far_back_track_flag(far);
	return;
}



void SAT_SetMaxEntry(SAT_Manager mng, unsigned entry)
{
	CSolver * solver = (CSolver*) mng;
	solver ->set_max_entry(entry);
	return;
}

void SAT_SetMaxDistance(SAT_Manager	mng, unsigned dist)
{
	CSolver * solver = (CSolver*) mng;
	solver -> set_max_distance(dist);
	return;
}

void SAT_SetDynamicHeuristic(SAT_Manager mng, int dynamic)
{
	CSolver * solver = (CSolver*) mng;
	solver ->set_dynamic_heuristic(dynamic);
	return;
}

void SAT_SetBacktrackFactor(SAT_Manager mng, double backtrack_factor)
{
	CSolver * solver = (CSolver*) mng;
	solver ->set_backtrack_factor(backtrack_factor);
	return;
}

//void SAT_SetApproximation(SAT_Manager mng, bool app)
//{
//	CSolver * solver = (CSolver*) mng;
//	solver ->set_approximation(app);
//	return;
//}

void SAT_SetMaxNumLearnedClause(SAT_Manager mng, unsigned max_num)
{
	CSolver * solver = (CSolver*) mng;
	solver ->set_max_num_learned_clause(max_num);
	return;
}

void SAT_SetOldestEntry(SAT_Manager mng, unsigned oldest)
{
	CSolver * solver = (CSolver*) mng;
	solver ->set_oldest_entry(oldest);
	return;
}

void SAT_SetCleanLimit(SAT_Manager mng, unsigned limit)
{
	CSolver * solver = (CSolver*) mng;
	solver ->set_clean_limit(limit);
	return;
}

void SAT_SetCrossFlag(SAT_Manager mng, bool cross)
{
	CSolver * solver = (CSolver*) mng;
	solver ->set_cross_flag(cross);
	return;
}

void SAT_SetAdjustFlag(SAT_Manager mng, bool adjust)
{
	CSolver * solver = (CSolver*) mng;
	solver ->set_adjust_flag(adjust);
	return;
}


long double SAT_SatProb(SAT_Manager mng)
{
	CSolver * solver = (CSolver*) mng;
	return solver->satisfying_prob();
}


long double SAT_NumSolutions(SAT_Manager mng)
{
	CSolver * solver = (CSolver*) mng;
	return solver->num_solutions();
}


bool SAT_GMP(SAT_Manager mng)
{
	CSolver * solver = (CSolver*) mng;
	return solver->gmp();
}


void SAT_SetQuietFlag(SAT_Manager mng, bool quiet)
{
	CSolver * solver = (CSolver*) mng;
	solver ->set_quiet_flag(quiet);
}


 char * SAT_Version(SAT_Manager mng)
{
    CSolver * solver = (CSolver*) mng;
    TRACE(trace_os << "SAT_Version\t" << mng << "\t" << solver->version() << endl;);
    return solver->version();
}

 void SAT_SetNumVariables(SAT_Manager mng, 
				    int n_var)
{
    CSolver * solver = (CSolver*) mng;
    TRACE(trace_os << "SAT_SetNumVariables\t" << mng << " " << n_var << endl; );
    solver->set_variable_number(n_var);
}

 void SAT_ReleaseManager(SAT_Manager mng)
{
    CSolver * solver = (CSolver*) mng;
    TRACE(trace_os << "SAT_ReleaseManager\t" << mng << endl;);
    delete solver;
}

 int SAT_AddVariable	(SAT_Manager 	mng)
{
    CSolver * solver = (CSolver*) mng;
    int vid = solver->add_variable();
    TRACE(trace_os << "SAT_AddVariable\t" << mng << "\t" << vid <<endl;);
    return vid;
}

 void  SAT_EnableVarBranch(SAT_Manager mng, int vid)
{
    CSolver * solver = (CSolver*) mng;
    TRACE(trace_os << "SAT_EnableVarBranch\t" << mng << " " << vid << endl;);
    solver->mark_var_branchable(vid);
}

 void SAT_DisableVarBranch(SAT_Manager mng, int vid)
{
    CSolver * solver = (CSolver*) mng;
    TRACE(trace_os << "SAT_DisableVarBranch\t" << mng << " " << vid << endl;);
    solver->mark_var_unbranchable(vid);
}

 void SAT_SetTimeLimit	(SAT_Manager 	mng ,  
				 float 		runtime)
{
    CSolver * solver = (CSolver*) mng;
    TRACE(trace_os << "SAT_SetTimeLimit\t" << mng << " " << runtime << endl;);
    solver->set_time_limit(runtime);
}

 void SAT_SetMemLimit	(SAT_Manager 	mng, 
				 int 		mem_limit)
{
    CSolver * solver = (CSolver*) mng;
    TRACE(trace_os << "SAT_SetMemLimit\t" << mng << " " << mem_limit << endl;);
    solver->set_mem_limit(mem_limit);
}

 void SAT_AddClause	(SAT_Manager 	mng, 
				 int * 		clause_lits,
				 int 		num_lits, 
				 int		gid = 0)
{
    CSolver * solver = (CSolver*) mng;
    TRACE(trace_os << "SAT_AddClause\t" << mng << " ";
	  for (int i=0; i< num_lits; ++i) {  trace_os << clause_lits[i] << " "; }
	  trace_os << num_lits << " " << gid << endl; );
    solver->add_orig_clause(clause_lits, num_lits, gid);
}

 void SAT_DeleteClauseGroup (SAT_Manager mng,
			    int		gid)
{
    CSolver * solver = (CSolver*) mng;
    TRACE(trace_os << "SAT_DeleteClauseGroup\t" << mng << " " << gid << endl;);
    solver->delete_clause_group(gid);
}

 void SAT_Reset	(SAT_Manager mng)
{
    CSolver * solver = (CSolver*) mng;
    TRACE(trace_os << "SAT_Reset\t" << mng << endl;);
    solver->reset();
}

 int SAT_MergeClauseGroup	(SAT_Manager mng,
				 int	gid1,
				 int 	gid2)
{
    CSolver * solver = (CSolver*) mng;
    int g = solver->merge_clause_group(gid1, gid2); 
    TRACE( trace_os << "SAT_MergeClauseGroup\t" << mng 
	   << " " << gid1 << " " << gid2 << "\t" << g;);
    return g;
}

 int SAT_AllocClauseGroupID (SAT_Manager mng) 
{
    CSolver * solver = (CSolver*) mng;
    int gid = solver->alloc_gid();
    TRACE ( trace_os << "SAT_AllocClauseGroupID\t" << mng << "\t" << gid << endl;);
    return gid;
}

 int SAT_GetGlobalGroupID (SAT_Manager mng) 
{
    TRACE ( trace_os << "SAT_GetGlobalGroupID\t" << mng << "\t" << 0 << endl;);
    return 0;
}

 int SAT_GetVolatileGroupID (SAT_Manager mng) 
{
    TRACE ( trace_os << "SAT_GetVolatileGroupID\t" << mng << "\t" << -1 << endl;);
    return -1;
}

 int SAT_Solve 	(SAT_Manager 	mng)
{
    CSolver * solver = (CSolver*) mng;
    int result = solver->solve();
    TRACE ( trace_os << "SAT_Solve\t" << mng << "\t" << result << endl;);
    return result;
}

 void SAT_AddHookFun 	(SAT_Manager	mng, 
				 void(*fun)(void *),
				 int 		interval)
{
    CSolver * solver = (CSolver*) mng;	
    TRACE ( trace_os << "SAT_AddHookFun\t" << mng 
	    << " " << fun << " " << interval << endl;);
    solver->add_hook(fun, interval);
}	

 void SAT_MakeDecision	(SAT_Manager	mng,
				int		vid,
				int		sign)
{
    CSolver * solver = (CSolver*) mng;	
    TRACE ( trace_os << "SAT_MakeDecision\t" << mng 
	    << " " << vid << " " << sign << endl;);
    solver->make_decision(vid+vid+sign);
}



 void SAT_SetRandomness	(SAT_Manager	mng,
				 int 		n)
{
    CSolver * solver = (CSolver*) mng;	
    TRACE ( trace_os << "SAT_SetRandomness\t" << mng<< " " << n << endl;);
    solver->set_randomness(n);
}

 void SAT_SetRandSeed	(SAT_Manager	mng,
				 int		seed)
{
    CSolver * solver = (CSolver*) mng;	
    TRACE ( trace_os << "SAT_SetRandSeed\t" << mng<< " " << seed << endl;);
    solver->set_random_seed(seed);
}

 int SAT_GetVarAsgnment	(SAT_Manager	mng,
					 int 		v_idx)
{
    CSolver * solver = (CSolver*) mng;
    assert (v_idx > 0 && v_idx < (int) solver->variables().size());
    int v = solver->variable(v_idx).value();	
    TRACE ( trace_os << "SAT_GetVarAsgnment\t" << mng<< " " 
	    << v_idx << "\t" << v << endl;);
    return v; 
}	

 int SAT_EstimateMemUsage(SAT_Manager mng)
{
    CSolver * solver = (CSolver*) mng;
    int usage = solver->estimate_mem_usage();
    TRACE (trace_os << "SAT_EstimateMemUsage\t" << mng << "\t" << usage << endl;);
    return usage;
}

 float SAT_GetElapsedCPUTime(SAT_Manager	mng)
{
    CSolver * solver = (CSolver*) mng;	
    float time = solver->elapsed_cpu_time();
    TRACE (trace_os << "SAT_GetElapsedCPUTime\t" << mng << "\t" << time << endl;);
    return time;
}

 double SAT_GetCurrentCPUTime(SAT_Manager	mng)
{
    CSolver * solver = (CSolver*) mng;	
    float time = get_cpu_time()/1000.0;
    TRACE (trace_os << "SAT_GetCurrentCPUTime\t" << mng << "\t" << time << endl;);
    return time;
}

 double SAT_GetCPUTime	(SAT_Manager	mng)
{
    CSolver * solver = (CSolver*) mng;	
    double time = solver->cpu_run_time();
    TRACE (trace_os << "SAT_GetCPUTime\t" << mng << "\t" << time << endl;);
    return time;
}

 int SAT_NumLiterals	(SAT_Manager mng)
{
    CSolver * solver = (CSolver*) mng;	
    int n = solver->num_literals();
    TRACE (trace_os << "SAT_NumLiterals\t" << mng << "\t" << n << endl;);
    return n;
}

 int SAT_NumClauses	(SAT_Manager mng)
{
    CSolver * solver = (CSolver*) mng;	
    int n = solver->num_clauses();
    TRACE (trace_os << "SAT_NumClauses\t" << mng << "\t" << n << endl;);
    return n;
}

 int SAT_NumVariables	(SAT_Manager mng)
{
    CSolver * solver = (CSolver*) mng;	
    int n = solver->num_variables();
    TRACE (trace_os << "SAT_NumVariables\t" << mng << "\t" << n << endl;);
    return n;
}

 int SAT_InitNumLiterals(SAT_Manager mng)
{
    CSolver * solver = (CSolver*) mng;	
    int n = solver->init_num_literals();
    TRACE (trace_os << "SAT_InitNumLiterals\t" << mng << "\t" << n << endl;);
    return n;
}

 int SAT_InitNumClauses(SAT_Manager mng)
{
    CSolver * solver = (CSolver*) mng;	
    int n = solver->init_num_clauses();
    TRACE (trace_os << "SAT_InitNumClauses\t" << mng << "\t" << n << endl;);
    return n;
}

 long64 SAT_NumAddedLiterals(SAT_Manager mng)
{
    CSolver * solver = (CSolver*) mng;	
    long64 n = solver->num_added_literals();
    TRACE (trace_os << "SAT_NumAddedLiterals\t" << mng << "\t" << n << endl;);
    return n;
}

 int SAT_NumAddedClauses	(SAT_Manager mng)
{
    CSolver * solver = (CSolver*) mng;	
    int  n =  solver->num_added_clauses();
    TRACE (trace_os << "SAT_NumAddedClauses\t" << mng << "\t" << n << endl;);
    return n;
}

 int SAT_NumDeletedClauses	(SAT_Manager mng)
{
    CSolver * solver = (CSolver*) mng;	
    int n = solver->num_deleted_clauses();
    TRACE (trace_os << "SAT_NumDeletedClauses\t" << mng << "\t" << n << endl;);
    return n;
}

 long64 SAT_NumDeletedLiterals	(SAT_Manager mng)
{
    CSolver * solver = (CSolver*) mng;	
    long64 n = solver->num_deleted_literals();
    TRACE (trace_os << "SAT_NumDeletedLiterals\t" << mng << "\t" << n << endl;);
    return n;
}

 int SAT_NumDecisions	(SAT_Manager mng)
{
    CSolver * solver = (CSolver*) mng;	
    int n = solver->num_decisions();
    TRACE (trace_os << "SAT_NumDecisions\t" << mng << "\t" << n << endl;);
    return n;
}

 long64 SAT_NumImplications	(SAT_Manager mng)
{
    CSolver * solver = (CSolver*) mng;	
    long64 n = solver->num_implications();
    TRACE (trace_os << "SAT_NumImplications\t" << mng << "\t" << n << endl;);
    return n;
}

 int SAT_MaxDLevel	(SAT_Manager mng)
{
    CSolver * solver = (CSolver*) mng;	
    int n = solver->max_dlevel();
    TRACE (trace_os << "SAT_MaxDLevel\t" << mng << "\t" << n << endl;);
    return n;
}

 float SAT_AverageBubbleMove	(SAT_Manager mng)
{
    CSolver * solver = (CSolver*) mng;	
    float n = ((float) solver->total_bubble_move()) / 
	(solver->num_added_literals() - solver->init_num_literals());
    TRACE (trace_os << "SAT_AverageBubbleMove\t" << mng << "\t" << n << endl;);
    return n;
}


 int SAT_GetFirstClause(SAT_Manager mng)
{
    CSolver * solver = (CSolver*) mng;	
    TRACE (trace_os << "SAT_GetFirstClause\t" << mng << "\t";);
    for (unsigned i=0; i< solver->clauses().size(); ++i)
	if ( solver->clause(i).status() != DELETED_CL) {
	    TRACE ( trace_os << i << endl; );
	    return i;
	}
    TRACE ( trace_os << -1 << endl; );
    return -1;
}

 int SAT_GetClauseType (SAT_Manager mng, int cl_idx)
{
    CSolver * solver = (CSolver*) mng;	
    int type = solver->clause(cl_idx).status();
    TRACE (trace_os << "SAT_GetClauseType\t" << mng << "\t" << type << endl;);
    return type;
}


 int SAT_IsSetClauseGroupID( SAT_Manager mng, int cl_idx, int id)
{
    CSolver * solver = (CSolver*) mng;	
    int r = solver->clause(cl_idx).gid(id);
    TRACE (trace_os << "SAT_IsSetClauseGroupID\t" << mng
	   << " " << cl_idx << " " << id << "\t" << r << endl;);
    return r;
}

 void SAT_ClearClauseGroupID( SAT_Manager mng, int cl_idx, int id)
{
    CSolver * solver = (CSolver*) mng;	
    TRACE (trace_os << "SAT_ClearClauseGroupID\t" 
	   << mng << " " << cl_idx << " " << id << endl;);
    solver->clause(cl_idx).clear_gid(id);
}

 void SAT_SetClauseGroupID( SAT_Manager mng, int cl_idx, int id)
{
    CSolver * solver = (CSolver*) mng;	
    TRACE (trace_os << "SAT_SetClauseGroupID\t" << mng 
	   << " " << cl_idx << " " << id << endl;);
    solver->clause(cl_idx).set_gid(id);
}

 int SAT_GetNextClause	(SAT_Manager mng, int cl_idx)
{
    CSolver * solver = (CSolver*) mng;	
    TRACE ( trace_os << "SAT_GetNextClause\t" << mng << " " << cl_idx <<"\t";);
    for (unsigned i= cl_idx + 1; i< solver->clauses().size(); ++i)
	if ( solver->clause(i).status() != DELETED_CL) {
	    TRACE (trace_os << i << endl;);
	    return i;
	}
    TRACE (trace_os << -1 << endl;);
    return -1;
}

 int SAT_GetClauseNumLits(SAT_Manager mng, int cl_idx)
{
    CSolver * solver = (CSolver*) mng;	
    int n = solver->clause(cl_idx).num_lits();
    TRACE ( trace_os << "SAT_GetClauseNumLits\t" << mng 
	    << " " << cl_idx <<"\t" << n << endl;);
    return n;
}

 void SAT_GetClauseLits(SAT_Manager mng, int cl_idx,  int * lits)
{
    CSolver * solver = (CSolver*) mng;	
    TRACE ( trace_os << "SAT_GetClauseLits\t" << mng 
	    << " " << cl_idx; );
    for (unsigned i=0; i< solver->clause(cl_idx).num_lits(); ++i) {
	lits[i] = solver->clause(cl_idx).literal(i).s_var();
	TRACE( trace_os << " " << lits[i];);
    }
    TRACE (trace_os << endl;);
}

 void SAT_EnableConfClsDeletion(SAT_Manager mng)
{
    CSolver * solver = (CSolver*) mng;	
    TRACE ( trace_os << "SAT_EnableConfClsDeletion\t" << mng << endl;);
    solver->enable_cls_deletion(true);
}

 void SAT_DisableConfClsDeletion(SAT_Manager mng)
{
    CSolver * solver = (CSolver*) mng;	
    TRACE ( trace_os << "SAT_DisableConfClsDeletion\t" << mng << endl;);
    solver->enable_cls_deletion(false);
}

 void SAT_SetClsDeletionInterval(SAT_Manager mng, int n)
{
    CSolver * solver = (CSolver*) mng;	
    TRACE ( trace_os << "SAT_SetClsDeletionInterval\t" << mng <<" " << n << endl;);
    solver->set_cls_del_interval(n);
}

// old version functions for clause deletion
 /*void SAT_SetMaxUnrelevance(SAT_Manager mng, int n)
{
    CSolver * solver = (CSolver*) mng;	
    TRACE ( trace_os << "SAT_SetMaxUnrelevance\t" << mng <<" " << n << endl;);
    solver->set_max_unrelevance(n);
}

 void SAT_SetMinClsLenForDelete(SAT_Manager mng, int n)
{
    CSolver * solver = (CSolver*) mng;	
    TRACE ( trace_os << "SAT_SetMinClsLenForDelete\t" << mng <<" " << n << endl;);
    solver->set_min_num_clause_lits_for_delete(n);
}

 void SAT_SetMaxConfClsLenAllowed(SAT_Manager mng, int n)
{
    CSolver * solver = (CSolver*) mng;	
    TRACE ( trace_os << "SAT_SetMaxConfClsLenAllowed\t" << mng <<" " << n << endl;);
    solver->set_max_conflict_clause_length(n);
}*/

//   void SAT_SetLitPoolCompactRatio(SAT_Manager mng, float ratio);
//   void SAT_SetLitPoolExpantionRatio(SAT_Manager mng, float ration);

 void SAT_CleanUpDatabase (SAT_Manager mng)
{
    CSolver * solver = (CSolver*) mng;	
    TRACE ( trace_os << "SAT_CleanUpDatabase\t" << mng<< endl; );
    solver->clean_up_dbase();    
}


 void SAT_GenClsAnd2	(SAT_Manager mng, 
				 int a,
				 int b,
				 int o,
				 int gid = 0 )
{
    CSolver * solver = (CSolver*) mng;	
    CClause_Gen cls_gen;
    TRACE ( trace_os << "SAT_GenClsAnd2\t" << mng
	    <<" " << a << " " << b << " " << o << " " << gid << endl; );
    cls_gen.and2 (*solver, a, b, o, gid);
}

 void SAT_GenClsAndN	(SAT_Manager mng, 
				 int * inputs,
				 int num_inputs,
				 int o,
				 int gid = 0) 
{
    CSolver * solver = (CSolver*) mng;	
    CClause_Gen cls_gen;
    TRACE ( trace_os << "SAT_GenClsAndN\t" << mng << " ";
	    for (int i=0; i< num_inputs; ++i) 
	    trace_os << inputs[i] << " " ; 
	    trace_os << num_inputs << " " << o << " " << gid << endl;);
    cls_gen.and_n (*solver, inputs, num_inputs, o, gid);

}

 void SAT_GenClsOr2	(SAT_Manager mng, 
			 int a,
			 int b,
			 int o,
				 int gid = 0) 
{
    CSolver * solver = (CSolver*) mng;	
    CClause_Gen cls_gen;
    TRACE ( trace_os << "SAT_GenClsOr2\t" << mng
	    <<" " << a << " " << b << " " << o << " " << gid << endl; );
    cls_gen.or2 (*solver, a, b, o, gid);
}

 void SAT_GenClsOrN	(SAT_Manager mng, 
				 int * inputs,
				 int num_inputs,
				 int o, 
				 int gid = 0 )
{
    CSolver * solver = (CSolver*) mng;	
    CClause_Gen cls_gen;
    TRACE ( trace_os << "SAT_GenClsOrN\t" << mng << " ";
	    for (int i=0; i< num_inputs; ++i) 
	    trace_os << inputs[i] << " " ; 
	    trace_os << num_inputs << " " << o << " " << gid << endl;);
    cls_gen.or_n (*solver, inputs, num_inputs, o, gid);
}

 void SAT_GenClsNand2	(SAT_Manager mng, 
				 int a,
				 int b,
				 int o,
				 int gid = 0)
{
    CSolver * solver = (CSolver*) mng;	
    CClause_Gen cls_gen;
    TRACE ( trace_os << "SAT_GenClsNand2\t" << mng
	    <<" " << a << " " << b << " " << o << " " << gid << endl; );
    cls_gen.nand2 (*solver, a, b, o, gid);
}


 void SAT_GenClsNandN	(SAT_Manager mng, 
				 int * inputs,
				 int num_inputs,
				 int o, 
				 int gid = 0 )
{
    CSolver * solver = (CSolver*) mng;	
    CClause_Gen cls_gen;
    TRACE ( trace_os << "SAT_GenClsNandN\t" << mng << " ";
	    for (int i=0; i< num_inputs; ++i) 
	    trace_os << inputs[i] << " " ; 
	    trace_os << num_inputs << " " << o << " " << gid << endl;);
    cls_gen.nand_n (*solver, inputs, num_inputs, o, gid);
}


 void SAT_GenClsNor2	(SAT_Manager mng, 
				 int a,
				 int b,	
				 int o, 
				 int gid = 0 )
{
    CSolver * solver = (CSolver*) mng;	
    CClause_Gen cls_gen;
    TRACE ( trace_os << "SAT_GenClsNor2\t" << mng
	    <<" " << a << " " << b << " " << o << " " << gid << endl; );
    cls_gen.nor2 (*solver, a, b, o, gid);
}


 void SAT_GenClsNorN	(SAT_Manager mng, 
				 int * inputs,
				 int num_inputs,
				 int o, 
				 int gid = 0 )
{
    CSolver * solver = (CSolver*) mng;	
    CClause_Gen cls_gen;
    TRACE ( trace_os << "SAT_GenClsNorN\t" << mng << " ";
	    for (int i=0; i< num_inputs; ++i) 
	    trace_os << inputs[i] << " " ; 
	    trace_os << num_inputs << " " << o << " " << gid << endl;);
    cls_gen.nor_n (*solver, inputs, num_inputs, o, gid);
}

 void SAT_GenClsXor	(SAT_Manager mng, 
				 int a,
				 int b,	
				 int o,
				 int gid = 0)
{
    CSolver * solver = (CSolver*) mng;	
    CClause_Gen cls_gen;
    TRACE ( trace_os << "SAT_GenClsXor2\t" << mng
	    <<" " << a << " " << b << " " << o << " " << gid << endl; );
    cls_gen.xor2 (*solver, a, b, o, gid);
}

 void SAT_GenClsNot	(SAT_Manager mng, 
				 int a,
				 int o,
				 int gid = 0)
{
    CSolver * solver = (CSolver*) mng;	
    CClause_Gen cls_gen;
    TRACE ( trace_os << "SAT_GenClsNot\t" << mng
	    <<" " << a << " " << o << " " << gid << endl; );
    cls_gen.not1 (*solver, a, o, gid);
}


























