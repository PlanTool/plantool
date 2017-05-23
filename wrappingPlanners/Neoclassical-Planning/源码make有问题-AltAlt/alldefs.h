/*
* $Revision: 3.4 $
* $Date: 1999/07/09 21:35:39 $
*/


#ifndef _ALLDEFS
#define _ALLDEFS

typedef struct proplist * plptr;
typedef struct propn * propptr;
typedef struct argslist * aptr;
typedef struct pair * pptr;
typedef struct operator_ * opptr;
typedef struct type_decls * tpptr;
typedef struct operator_set * oplistptr;
typedef struct domain * domptr;
typedef struct uneqlistnode * uneqlist;
typedef struct predcell * predlist;
typedef struct efflist * effptr;

struct efflist {
	plptr adds;
	plptr dels;
};

struct uneqlistnode {
	tpptr uneq;
	uneqlist next;
};

struct pair {
	char * varname;
	char * type_name;
	int type[2];
	int used;		/* Used in build
				   to track 
				   whether a var
				   is already 
				   instantiated

		 In constants we'll use it to
		 indicate whether a constant 
		 appears in an operator or not. */

	int sgnum;
	int sindex;

/* New field added to distinguish precondition paramters. */
	int isInPre;

};

struct type_decls {
	tpptr tps;
	pptr type;
	uneqlist uneqs;


};

struct operator_set {
	opptr op;
	oplistptr ops;
};

struct operator_ {
	tpptr vars;
	tpptr vars_shadow;
	plptr precs;
	plptr statics;
	plptr adds;
	plptr dels;
	propptr thename;
	};

struct propn {
	pptr nm;
	propptr as;
	};

struct proplist {
	plptr rest;
	propptr prop;
	};

struct domain {
	oplistptr ops;
	plptr initial_state;
	plptr goal_state;
	tpptr obs;
	plptr statics;
	};

struct predcell {
	char * name;
	int numargs;
	int fr;
	int ar;
	opptr op;
	plptr initially;
	predlist next;
	};
#endif
