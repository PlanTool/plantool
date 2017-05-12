/*********************** Graphplan **************************************
  (C) Copyright 1995 Avrim Blum and Merrick Furst

  All rights reserved. Use of this software is permitted for non-commercial
  research purposes, and it may be copied only for that use.  All copies must
  include this copyright message.  This software is made available AS IS, and
  neither the authors nor CMU make any warranty about the software or its 
  performance. 
*************************************************************************/
#ifndef GRAPHPLAN__H
#define GRAPHPLAN__H

#include<stdlib.h>
#include<stdio.h>
#include<ctype.h>
#include<strings.h>

typedef struct VERTEX vertex_s, *vertex_t;
typedef struct TOKENLIST *string_list;

typedef struct EDGE
{
  vertex_t endpt;
  struct EDGE *next;
} edgelist_s, *edgelist_t;

/* HERE ARE SOME ARBITRARY VALUES */

#define MAXMAXNODES (16*1024)  /* max number of nodes per layer of graph */
#define NUMINTS  (MAXMAXNODES/32) /* MAXMAXNODES / 32 */ 

#define DEFAULT_MAXNODES (4*1024)

#define HSIZE 50   /* For simplicity, fix hash table to have size HSIZE */
#define MAX_TOKENS 15 /* max number of tokens in a tokenlist */
#define MAXGOALS 250   /* max size of a goal set */
#define max_auto 200  /* arbitrary max #time steps for making graph */


/* a vertex in the graph */
struct VERTEX 
{
  char *name;
  int hashval;         /* make easier to find in table */
  edgelist_t in_edges;
  edgelist_t out_edges;
  string_list del_list; /* names of things you delete (that aren't preconds) */
  edgelist_t del_edges; /* pointers (one way) to things you delete */
  edgelist_t exclusive;
  int exclusive_vect[NUMINTS];
  edgelist_t excl_in_this_step; /* can't create any pair of these right now */
  int excl_in_this_step_vect[NUMINTS];
  int used;  /* if used, 1+index in array (add 1 to make sure not zero) */
  int is_true;        /* is it made true */
  int cant_do;        /* marked as exclusive of something being done */
  int needed;
  int uid_block; 	  /* For planning: use uid_mask and uid_block  */
  unsigned int uid_mask;  /* -- unique for time                        */
  vertex_t prev_time;  /* for facts: the fact at prev time step */
  vertex_t next_time;  /* and next time step */
  long rand1;    /* random values associated with fact */
  int junk;  /* use for whatever one wants */
  int is_noop;         /* is it a NOOP? */
  struct VERTEX *next;

  /* BM: Added value for store the unique csp value */
  int csp_val;
};

typedef struct PAIR{ int first; int second; } pair;

typedef vertex_t goal_arr[MAXGOALS];
typedef vertex_t hashtable_t[HSIZE];
typedef vertex_s element_s, *element_t;
typedef char * token;

typedef struct TOKENLIST {
  token item;
  struct TOKENLIST *next;
} token_list_elt, *token_list;

typedef struct FACTLIST {
  token_list item;
  struct FACTLIST *body;  /* BS: PDDL added for handling exists */
  struct FACTLIST *next;
} fact_list_elt, *fact_list, *precond_list, *effect_list, *param_list;

typedef struct INSTLIST {
  char *const_part;
  char *var_part;
  struct INSTLIST *next;
} instantiation_s, *instantiation_list;

typedef struct OPER {
  char *name;
  param_list params;
  precond_list preconds;
  effect_list effects;
/*   int num_preconds, num_effects, num_params; NOT NEEDED ANY MORE */
  instantiation_list insts;  /* use to store the variables */
  struct OPER *next;
} op_s, *op_list, *op_ptr;


/****defines*****/
#define STDMSG stdout
#define STDDATA stdout

#define SAME 0
#define DIFFERENT (-1)
#define PARAM "params"
#define PRECOND "preconds"
#define EFFECTS "effects"
#define OPR "operator"
#define DELETE "del"
#define LISPEXT ".lisp"
#define is_const(x) (*(x) != '<' && *(x) != '?')
#define is_var(x) (*(x) == '<' || *(x) == '?' )
#define OK 1
#define LEFT_PAREN 2
#define RIGHT_PAREN 3
#define max(x,y) ((x) > (y) ? (x) : (y))
#define min(x,y) ((x) > (y) ? (y) : (x))
#define equal_tokens(x,y) (!strcmp((x),(y)))
#define YES 1
#define NO  0
#define NEW_INSTS 2
#define CONNECTOR '_'
#define TRUE 1
#define NOOP "noop"
#define IS_NOOP(x) ((x)->is_noop)

/***********function prototypes: graphplan.c***************/
int create_graph(op_list olist, fact_list flist, int totaltime);
void create_graph_layer(op_list olist);
op_list load_ops(FILE *fp);
int load_fact_list(FILE *fp, fact_list *fptr);
void make_graph_piece(op_ptr op, int time);
void do_operator(hashtable_t htable, fact_list current_facts, op_ptr op, precond_list p, int time);
void remove_unneeded_vertices(int time);
int can_stop(int time);
void make_copy(int time);
op_list load_prodigy_ops(FILE *fp);
fact_list useful_facts(op_list ops, fact_list f);
void print_cant_do(int time);
fact_list load_types(FILE *fp);
void do_final_viewing(void);

/***********function prototypes: hash.c********************/
element_s * lookup_from_table(hashtable_t htable, char *key);
element_t insert_token_list(hashtable_t htable, token_list t);
element_t insert_into_table(hashtable_t htable, char *key);
void delete_unneeded(hashtable_t htable);
void delete_from_table(hashtable_t htable, char *key, vertex_t to_kill);
edgelist_t insert_edge(edgelist_t e, vertex_t v);
element_t get_next(hashtable_t h, int flag);
/***********function prototypes: utilities.c****************/
char *make_noop_string(char *str);
fact_list make_list_from_htable(hashtable_t h);
instantiation_list insert_inst(char *v, char *c, instantiation_list i);
int instantiate_into_string(token_list t, 
			    instantiation_list inst, char str[], int failflag);
int read_item(FILE *fp, char *str);
void make_op_string(op_ptr op, char *str);
void do_error(char *s);
int equal_facts(token_list f1, token_list f2);
pair find_mutex_facts(hashtable_t harr[], int time);
void find_currently_mutex_facts(void);
int are_facts_exclusive(vertex_t p, vertex_t q);
void find_all_mutex_ops(hashtable_t harr[], int time);
void find_mutex_ops(vertex_t v, int time);
int avoidable(vertex_t op);
int are_mutex(vertex_t v1, vertex_t v2);
void print_graph(hashtable_t *fact_arr, hashtable_t *op_arr, int len);
void read_initial_comments(FILE *fp);
void my_free(void * p);
void * my_alloc(int size);
void print_alloc(void);
void completely_free_fact_list(fact_list l);
void free_token_list(token_list l);
int compare_and_instantiate(token_list, token_list,instantiation_list);
token_list token_list_from_string(char str[]);
void set_uid(vertex_t v, int id);
// BS: for PDDL -- start
char* lookup_instantiation (instantiation_list insts, char *name);
void free_instantiation (instantiation_list insts);
fact_list fact_list_append(fact_list f1, fact_list f2);
fact_list token2fact (token_list tlist);
token_list token_list_append(token_list f1, token_list f2);
token_list str2token (char *str);
token_list strdup2token (char *str);
token_list dup_token_list (token_list tlist);
char* bbstrdup (char* yytext);
void print_token_list (token_list t);
void print_fact_list (fact_list f);
void print_actions (op_list op);
// BS: for PDDL -- end


/***********function prototypes: planner.c*****************/
int do_plan(int maxtime);
void print_plan(int maxtime);
void print_info(int len);
int try_facts(vertex_t f1, vertex_t f2, int time);
/***********others (viewing)**************/
void setup_viewer(void);
void wait_until_left(void);
void reset_viewer(int max_time);
void draw_fact(vertex_t v, int time, int flag);
void draw_op(vertex_t v, int time, int flag, int thick);

/**************vbles**************/
extern int num_deleted;
extern int num_created;
extern int DEBUG_FLAG;
extern int MAXNODES;            /* MAXNODES is the maximum number of nodes
				   allowed per level. */

/* BM: Extern variable for csp solver */
extern int csp_solver;
extern hashtable_t *fact_table;
extern hashtable_t *op_table;

/* for pddl parser */
enum { DOMAIN_INPUT, PROBLEM_INPUT, CONTROL_INPUT };

#endif
