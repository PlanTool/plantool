/* 
* $Revision: 3.2 $
* $Date: 1999/07/09 21:35:39 $

actions.h - This file contains the data structure prototypes for the
actions structures. */

#ifndef __ACTIONS
#define __ACTIONS

#include "switches.h"

class action;

class action_list_node;

extern action_list_node * freelist;
extern action_list_node * freep;
extern action_list_node * freelim;

class action_list_node {
public:	action * act;
	action_list_node * next;

	action_list_node(action * a,action_list_node * n) {act = a; next = n;};


	void * operator new(size_t sz)
	{
		if(freelist)
		{
			void * t = freelist;
			freelist = freelist -> next;
			return t;
		};
		if(freep < freelim)
		{
			void * t = freep;
			freep += 1;
			return t;
		};
		char * new_chunk = new char[sz * 1024];
		freep = (action_list_node *) new_chunk;
		freelim = freep+1024;
		void * t = freep;
		++freep;
		return t;
	};

	void operator delete(void * d)
	{
		((action_list_node *) d)->next = freelist;
		freelist = (action_list_node *) d;
	};

};

typedef action_list_node * action_list;

#include "globals.h"
#include "facts.h" 
#include "alldefs.h"

typedef int * pint;

class op_level_info {
private:
	int exvec[EXVECSIZE];
	int checkVec[EXVECSIZE];

	int used;
	int excluded;
	fact * what_for;

	action_list excludeds;
	action_list permexcludeds;

	action_list inversions;

	int nexcs;
	int npexcs;
	int ninvs;
	pint * excs;

public:	void set_up();
	void start_layer(op_level_info &);
	inline action_list get_excludeds(){return excludeds;};
	inline action_list get_pexcludeds(){return permexcludeds;};
	inline action_list get_inversions(){return inversions;};
	inline int * get_excluded_marker(){return &excluded;};
	inline void reset_mutex(action *);
	inline void reset_excludeds(){excludeds = 0;};
	inline void add_exclude(action *);
	inline void link_exclude(action_list_node *);
	inline void set_mutex(action *);
	inline void set_perm_mutex(action *);
	int * excludevec(){return exvec;};
	inline fact * already_excluded(){return excluded?what_for:0;};
	inline void mark_excluded(){excluded++;};
	inline void unmark_excluded(){excluded--;};
	void copy(op_level_info &);
	int * checkvec() {return checkVec;};
	void setvec(token_list,int);
	void copyvec(int * chkvec)
	{
		for(int i = 0;i<EXVECSIZE;i++)
			checkVec[i] = chkvec[i];
	};
	void mark_all_exclusives(int ae,fact * the_goal)
	{	
		for(int * * t = excs+nexcs-1;t>=excs;t--)
			if (!(*(*t)))
			 {(*(*t)) = ae+1;
			  *((fact * *)((*t)+1)) = the_goal;};
	};
	void unmark_all_exclusives(int aeinc)
	{
		for(int * * t = excs+nexcs-1;t>=excs;t--)
			if ((*(*t)) == aeinc)
				(*(*t)) = 0;
	};
	void set_excs(int);
	inline int is_used(){return used;};
	inline void mark_used(){used++;};
	inline void unmark_used(){used--;};
	inline void set_inversion(action *);
	void set_excludeds(action_list oldexcs,op_level_info & pl)
	{
		excludeds = oldexcs;
		nexcs = pl.nexcs-npexcs-ninvs;
	};
};


extern op_level_info * level[MAX_PLAN];

#ifdef SYMMETRY

extern int numSymGps[MAX_PLAN];
extern int totalSymGps;
extern int matchedSymGps;
extern char ** triedGps[MAX_PLAN];
extern char * foundSymGps[MAX_ACTIONS];

#endif

class action {
private:
	char * name;
	int noop;
	token_list preconditions;
	token_list add_list;
	token_list del_list;

	int precvec[EXVECSIZE]; //Spike vectors.....
	int addvec[EXVECSIZE];
	int delvec[EXVECSIZE];


	int exvec_pos;
	unsigned int exvec_mask;

	int act_entry;

        /* ALTALT */
	/* Distance-based value for actions */
	int levelact;
	/* RS */

	action_list tmp_excludeds;

#ifdef SYMMETRY
	int * symParams;
	int numSymParams;
	char * symGp;
	int symLabel;
	int * indexes;
#endif

public:	action(opptr);
	int applicable(int);

	/* ALTALT */
	/* Setting the distance-based values for actions */
	void setlevel(int ln){ levelact=ln;};
	int getlevel(){ return levelact;}; 	
        /* Determining if an action is applicable for regression given a set of atoms*/
        int applicable_regression(token_list);
	/* Returning the adds and dels of the action */
	token_list get_adds(){return add_list;};
	token_list get_dels(){return del_list;};
        /* RS: End */

	void enact(int);
	action(int,int,fact *);
	inline void set_up_layer(int);
	int get_entry(){return act_entry;};
	action_list get_tmp_excludeds(){return tmp_excludeds;};
	action_list get_excludeds(int);
	inline void move_tmp_excludeds(int);
	int expos(){return exvec_pos;};
	unsigned int exmask(){return exvec_mask;};
	inline int * checkvec(int);
	int check_mutx_vec(int * checkvec)
	{
		for(int i = 0;i<exvecsizef;i++)
			if(precvec[i] & checkvec[i]) return TRUE;
		return FALSE;
	};
	inline void add_exclude(int,action *);
	inline void link_exclude(int,action_list_node *);
	void set_excludeds(int ln)
	{
		level[ln][act_entry].set_excludeds(tmp_excludeds,level[ln-1][act_entry]);
	};
	void reset_mutex(int ln,action * b)
	{
		level[ln][act_entry].reset_mutex(b);
	};
	void set_mutex(int ln,action * a)
	{
		level[ln][act_entry].set_mutex(a);
	};
	void set_perm_mutex(int ln,action * a)
	{
		level[ln][act_entry].set_perm_mutex(a);
	};
	int * adds_vec(){return addvec;};
	int * dels_vec(){return delvec;};
	int * precs_vec(){return precvec;};
	inline token_list get_precs(){return preconditions;};
	inline void add_del_info(int,unsigned int);
	inline int * excludeds(int);
	inline fact * already_excluded(int);
	inline void mark_excluded(int ln) {level[ln][act_entry].mark_excluded();};
	inline void unmark_excluded(int);
	fact * tryop(int,token_list,fact *);
	void try_noop(int,fact *);
	void untry_noop(int,fact *);
	void untry(int,int);
	int all_precs_achievable();
	inline void begin_layer(int);
	char * get_name(){return name;};
	int is_noop(){return noop;};
	inline void copy(int);
	inline int * get_excluded_marker(int);
	inline void set_excs(int);
	int checkPAD(action * b)
	{
		int * ba = b->addvec;
		int * bp = b->precvec;
		int * bd = b->delvec;

		for(int i=0;i<exvecsizef;i++)
			if(((addvec[i] | precvec[i]) & bd[i]) ||
			   ((ba[i] | bp[i]) & delvec[i]))
				return TRUE;
		return FALSE;
	};
	void set_inversion(int ln,action * a)
	{
		level[ln][act_entry].set_inversion(a);
	};
	void check_still_mutex_pres(int ln,action_list b)
	{

		action * bb = b->act;
		
		if(!bb->check_mutx_vec(checkvec(ln)))
		{
			reset_mutex(ln,bb);
			bb->reset_mutex(ln,this);
			delete b;
		}
		else
		{
			link_exclude(ln,b);
			bb->add_exclude(ln,this);
		};
	};
#ifdef SYMMETRY
	int unusable(int ln)
	{
		if(!symLabel) return 0;
		int running_total = 0;
		int isbroken = 1;
		for(int i = 0;i<numSymParams;i++)
		{	isbroken = isbroken && brokenSym[symParams[i]];
			running_total += brokenSym[symParams[i]]?indexes[i]:0;
		};

		if(isbroken) return 0;
#ifdef SYMMETRYOUT
		if(triedGps[ln][symLabel][running_total] == '1')
			cout << "Not trying symmetric operator " << name << " at level " << ln << "\n";
#endif


		return triedGps[ln][symLabel][running_total] == '1';
	};
#endif
		
};

inline void op_level_info::set_mutex(action * a)
	{
		exvec[a->expos()] |= a->exmask();
		excludeds = new action_list_node(a,excludeds);
		nexcs++;
	};

inline void
op_level_info::add_exclude(action * a)
{
	excludeds = new action_list_node(a,excludeds);
	nexcs++;
};

inline void
op_level_info::link_exclude(action_list b)
{
	b->next = excludeds;
	excludeds = b;
	nexcs++;
};

inline void
action::add_exclude(int ln,action * a)
{
	level[ln][act_entry].add_exclude(a);
};

inline void
action::link_exclude(int ln,action_list b)
{
	level[ln][act_entry].link_exclude(b);
};

inline int * 
action::checkvec(int ln)
{
	return level[ln][act_entry].checkvec();
};

inline void
op_level_info::reset_mutex(action * b)
{
	exvec[b->expos()] ^= b->exmask();
};

inline void 
op_level_info::set_perm_mutex(action * a)
{
	permexcludeds = new action_list_node(a,permexcludeds);
	exvec[a->expos()] |= a->exmask();
	npexcs++;
};


inline void
action::set_excs(int ln)
{
	level[ln][act_entry].set_excs(ln);
};

extern int dummycell;

inline int *
action::get_excluded_marker(int ln)
{
	if(act_entry < acts_at[ln])
		return level[ln][act_entry].get_excluded_marker();

	return &dummycell;
};

inline void 
action::unmark_excluded(int ln)
{
	level[ln][act_entry].unmark_excluded();
};

inline void
action::copy(int ln)
{
	level[ln][act_entry].copy(level[ln-1][act_entry]);
	level[ln][act_entry].set_excs(ln);
};

inline fact *
action::already_excluded(int ln)
{
	return level[ln][act_entry].already_excluded();
};

inline int *
action::excludeds(int ln)
{
	return level[ln][act_entry].excludevec();
};

inline action_list
action::get_excludeds(int ln)
{
	return level[ln][act_entry].get_excludeds();
};

inline void 
action::move_tmp_excludeds(int ln)
{
	tmp_excludeds = level[ln][act_entry].get_excludeds();
};

inline void
action::set_up_layer(int ln)
{

	level[ln][act_entry].start_layer(level[ln-1][act_entry]);
	level[ln][act_entry].setvec(preconditions,ln);
	move_tmp_excludeds(ln-1);
};

inline void
action::begin_layer(int ln)
{
	level[ln][act_entry].set_up();
	level[ln][act_entry].setvec(preconditions,ln);
};

inline void 
action::add_del_info(int ep,unsigned int em)
{
	delvec[ep] |= em;
};

inline void
op_level_info::set_inversion(action * a)
{
	inversions = new action_list_node(a,inversions);
	ninvs++;
};

void setvec();
void create_action_layer(int,int,int);

int in_list(token_list,fact *);

#endif
