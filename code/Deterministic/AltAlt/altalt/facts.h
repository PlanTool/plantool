/* 
* $Revision: 3.1 $
* $Date: 1999/01/06 10:16:12 $


fact.h - This file defines the main data structures associated with the
maintenance of the facts within the plan graph. These are the hash_entry (fact)
and the fact_level_info */

#include "globals.h"
#include "actions.h"
#include "stream.h"
#include "stdio.h"

#ifndef __FACTS
#define __FACTS


/* This structure is just a simple list for facts. There are several
other similar ones - a template would probably be better coding style.
It is lazy, but simpler, to leave everything public. This means it behaves
like a struct. */

class token_list_node {
public:
	hash_entry * info;
	token_list_node * next;

	token_list_node(hash_entry * f,token_list_node * n) { info = f; next = n;};
};

typedef token_list_node * token_list;

class fact_level_info {
private:
	int exvec[EXVECSIZE];
	int used;
	fact * what_for;
	int excluded;
	int achieved;

	action_list achievers;
	action_list consumers;

	int domination[EXVECSIZE];
	int dominated;
	int dominates;

public:
	inline int is_mutex(int,unsigned int);
	inline void add_consumer(action *);
	inline void add_achiever(action *);
	void set_up(fact_level_info &);
	void initialise(action_list_node *);
	int * get_vec(){return exvec;};
	action_list get_achievers(){return achievers;};
	inline fact * getwhat_for()
		{return what_for;};
	inline void set_mutex(hash_entry *,int,unsigned int);
	int is_already_done() {return achieved;};
	void mark_achieved() {achieved++;};
	int is_used() {return used;};
	void mark_used(fact * the_goal) 
		{if (!used)
			what_for = the_goal;
		 used++;};
	void unmark_achieved() {achieved--;};
	void unmark_used() {used--;};
	void copy(fact_level_info &);
	void showexvec()
	 {for(int i=0;i<exvecsizef;i++) cout << exvec[i] << " "; cout << "\n";};
	void mark_excluded() {excluded++;};
	void unmark_excluded() {excluded--;};
	int is_excluded() {return excluded;};
	action_list get_consumers(){return consumers;};

	inline void add_dominate(hash_entry *);
	int * get_submissives(){return domination;};
	void mark_dominated(){dominated = 1;};
	void mark_dominates(){dominates = 1;};
	int is_dominated(){return dominated;};
	void no_domination(){dominated = 0;dominates = 0;};
	int is_in_domination(){return (dominates || dominated);};

};

extern fact_level_info * flevel[MAX_PLAN];

class hash_entry {
private:
	char * name;
	int achieved;
	int fact_entry;
	hash_entry * next;
	
	/*RS: Added by RS*/
	/* ALTALT */
	/*These are the distance-based values for individual propositions*/
	int sumH;
        int maxV;
        /*RS: End */

	int achievable;
	int modifiable;

	int hashval;
	
	int hvalue; //added...

	int exvec_pos;
	unsigned int exvec_mask;

	action_list_node * deleters;

	int achievec[EXVECSIZE];
	int consumers[EXVECSIZE];

	int mynoop;

	int frnum;
	int resval;

	char * mySym;

	action_list_node * allachievers;


// This symbol will be stored in a common structure - the symmetric propositions list
// described below.

//	int * mySyms;

public:	

	/* RS: Added by RS*/
	/* ALTALT */
	/* Retrieving the distance-based values for an individual fact*/
	void setSumH(int sum){sumH=sum;};	
	int getSumH(){return sumH;};
	void setMaxV(int maxv){maxV=maxv;};
        int getMaxV(){return maxV;};
	/*RS: End */
			
	hash_entry(char *,int,hash_entry *);
	void initialise(int,int);
	void add_consumer(action *);
	int * get_consumers(){return consumers;};
	void add_achiever(int,action *);
	int is_mutex(int ln,int ep,unsigned int em)
		{return flevel[ln][fact_entry].is_mutex(ep,em);};
	int expos() {return exvec_pos;};
	unsigned int exmask() {return exvec_mask;};
	inline fact * getwhat_for(int ln){return flevel[ln][fact_entry].getwhat_for();};
	hash_entry * getnext() {return next;};
	int has_name(int h,char * nm) {return hashval = h && !strcmp(name,nm);};
	int is_achieved(int ln) {return achieved <= ln;};
	void set_up_layer(int ln) 
		{flevel[ln][fact_entry].set_up(flevel[ln-1][fact_entry]);};
	int * get_vec(int ln) {return flevel[ln][fact_entry].get_vec();};
	action_list_node * achievers(int ln)
		{return flevel[ln][fact_entry].get_achievers();};
	inline int excluded_by(int *);
	void set_mutex(hash_entry * f,int ln,int ep,unsigned int em)
		{flevel[ln][fact_entry].set_mutex(f,ep,em);};
	int is_done(int ln)
		{return flevel[ln][fact_entry].is_already_done();};
	inline void mark_goal(int,fact *);
	void mark_achieved(int ln)
		{flevel[ln][fact_entry].mark_achieved();};
	inline void unmark_goal(int);
	void unmark_achieved(int ln) {flevel[ln][fact_entry].unmark_achieved();};
	char * get_name(){return name;};
	void show_achievec() 
	  {for(int i=0;i<exvecsizea;i++) cout << achievec[i];cout  << "\n";};
	void showexvec(int ln)
		{flevel[ln][fact_entry].showexvec();};
	void set_achievable(){achievable = TRUE;};
	void set_modifiable(){modifiable = TRUE;};
	int is_achievable(){return achievable;};
	int is_modifiable(){return modifiable;};
	inline void add_deleter(action *);
	void copy(int ln)
		{flevel[ln][fact_entry].copy(flevel[ln-1][fact_entry]);};
	token_list_node * excludeds_at(int);
	void mark_excluded(int ln) {flevel[ln][fact_entry].mark_excluded();};
	void unmark_excluded(int ln) {flevel[ln][fact_entry].unmark_excluded();};
	int is_excluded(int ln)
		{return flevel[ln][fact_entry].is_excluded();};
	int is_used(int ln) {return flevel[ln][fact_entry].is_used();};
	int still_achievable(int);
	void set_noop(int,action *);
	int has_noop(int ln) {return mynoop < acts_at[ln];};
	void mark_used() {flevel[0][fact_entry].mark_used(this);};
	inline action * get_noop();
	int where_noop(){return mynoop;};
	int getfrnum(){return frnum;};
	int getresval(){return resval;};
	int get_when(){return achieved;};
	int ordered_by(int *);
	action_list_node * get_consumers(int ln)
		{return flevel[ln][fact_entry].get_consumers();};
	action_list_node * get_achievers(int ln) 
		{return flevel[ln][fact_entry].get_achievers();};

#ifdef NOREUSE
	void please_exclude(int);
	void dont_exclude(int);
#endif

	void add_dominate(int ln,hash_entry * h)
		{flevel[ln][fact_entry].add_dominate(h);};

	void no_domination(int ln)
		{flevel[ln][fact_entry].no_domination();};
	void mark_dominated(int ln)
		{flevel[ln][fact_entry].mark_dominated();};
	void mark_dominates(int ln)
		{flevel[ln][fact_entry].mark_dominates();};
	int is_in_domination(int ln)
		{return flevel[ln][fact_entry].is_in_domination();};
	int is_dominated(int ln)
		{return flevel[ln][fact_entry].is_dominated();};
	int dominates(int,hash_entry *);

	int getnum(){return fact_entry;};

	int domval;
	int precount;	


	action_list_node * get_allachievers(){return allachievers;};

};


typedef hash_entry fact;


class hashtable {
private:
	hash_entry * hasht[MAX_HASH];

	inline int hash(char *);
public:
	hash_entry * insert(char *);

	hashtable();
};



extern hashtable all_facts;


inline int hashtable::hash(char * nm)
{
	int h;
	for(h = 0;*nm != '\0';nm++) h = (h*256+(*nm)) % BIGPRIME;
	return h;
};

/* Housekeeping stuff for setting up the graph... */


inline void 
hash_entry::add_deleter(action * a)
{
	deleters = new action_list_node(a,deleters);
};

inline action *
hash_entry::get_noop()
{
	return action_table[mynoop];
};

inline void
hash_entry::mark_goal(int ln,fact * the_goal)
{
	flevel[ln][fact_entry].mark_used(the_goal);

};

inline void
hash_entry::unmark_goal(int ln)
{
	flevel[ln][fact_entry].unmark_used();

};


inline void
fact_level_info::add_dominate(hash_entry * h)
{
	domination[h->expos()] |= h->exmask();
};

inline void
fact_level_info::set_mutex(fact * f,int ep,unsigned int em)
{
	exvec[ep] |= em;
};

inline void
fact_level_info::add_consumer(action * a)
{
	consumers = new action_list_node(a,consumers);
};

inline void 
fact_level_info::add_achiever(action * a)
{
	achievers = new action_list_node(a,achievers);
};

inline int
fact_level_info::is_mutex(int ep,unsigned int em)
{
	return (exvec[ep] & em) > 0;
};

inline int
hash_entry::excluded_by(int * acexvec)
{
	for(int i = 0;i<exvecsizea;i++)
		if(((achievec[i] & acexvec[i]) ^ achievec[i]) != 0) 
			return FALSE;
	return TRUE;
};

extern void create_fact_layer(int,int);

#endif
