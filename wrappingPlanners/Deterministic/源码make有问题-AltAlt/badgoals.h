/*
* $Revision: 3.1 $
* $Date: 1999/01/06 10:15:43 $

badgoals.h */

#include "globals.h"


#ifndef __BADGOALS
#define __BADGOALS


class goalcell;

class bgTrie {
private:
	goalcell * root;
public:
	int insert(int,token_list);
	int insertall(token_list);
	int find(token_list,int);
	int findit(token_list,int);
	bgTrie() : root(0) {};
	void show();
	~bgTrie();
	void reset(token_list);
};


class goalcell {
private:
	int val;
	bgTrie branch;
	bgTrie chain;
	int length;
public:
	goalcell(){val = 0;};
	goalcell(int v) : val(v) {};
	bgTrie & getchain(){return chain;};
	bgTrie & getbranch(){return branch;};
	int setlength(int a){length = length<=a+1?length:a+1;
			      return length;};
	int setnewlength(int a){length = a+1;return a+1;};
	int getval(){return val;};
	int getlength(){return length;};	
	
};

int marked_bad_goal_set(int,token_list);
int is_bad_goal_set(int,token_list);
void mark_bad_goal_set(int,token_list);
void reset(token_list);
 
extern bgTrie already_seen;

#endif
