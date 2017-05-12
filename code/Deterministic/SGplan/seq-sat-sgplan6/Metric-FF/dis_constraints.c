/*********************************************************************

 * (C) Copyright 2008, University of Illinois, Urbana-Champaign

 *

 * All rights reserved. Use of this software is permitted ONLY for

 * non-commercial research purposes, and it may be copied only

 * for that use only. All copies must include this copyright message.

 * This software is made available AS IS, and neither the authors

 * nor the University of Illinois, make any warranty about the

 * software or its performance.

 *

 *********************************************************************/
/********************************************************************

 * File: dis_constraints.c

 * Authors:  Chih-wei Hsu and Benjamin W. Wah

 *

 *********************************************************************/
/*
# Copyright (C) 2006, Board of Trustees of the University of Illinois.
#
# The program is copyrighted by the University of Illinois, and should
# not be distributed without prior approval.  Commercialization of this
# product requires prior licensing from the University of Illinois.
# Commercialization includes the integration of this code in part or
# whole into a product for resale.
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Author: C. W. Hsu, B. W. Wah, R. Y. Huang, and Y. X. Chen  
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
*/
#include "string.h"
#include "dis_ff.h"
#include "dis_parse.h"
#include "dis_memory.h"
#include "dis_output.h"
#include "dis_search.h"
#include "dis_expressions.h"
#include "casual_graph.h"
#include "subspace.h"
#include "lpg.h"
#include "dis_producible.h"

#define MAX_CONSTRAINTS 200000
#define MAX_TEMPLATES 200000
#define MAXLEN_OF_GROUPLIST 10
#define MAX_EVAL_NODE 10000

// numeric conds/effects are not implemented
typedef struct _TimeSpecConn
{
  TimeSpec *PC, *A, *D;
} TimeSpecConn;

TimeSpecConn *TS;
int duration_fl = -1;

typedef struct _ConConn
{   
  /*
    connective: 
    weight: 100000000 means hard constraint 
    time1, time2: time values
    cond: condition of the constraint
    num: length of cond
    num1: the divider of cond for those constraints with two conditions
    fact: corresponding fact  
    fv: corresponding function
  */
  dis_Connective_Con connective;
  float weight, time1, time2;
  int *cond, num1, num, fact, fv;
} ConConn;

typedef struct _GroupList
{
  int n, i[MAXLEN_OF_GROUPLIST];
} GroupList;
GroupList gl;
int **gtemplates;
int gnum_templates;

ConConn *dis_gcon_conn;
int dis_gnum_con_conn;

int traj_n;
int traj_plan[dis_MAX_PLAN_LENGTH];
float traj_time[dis_MAX_PLAN_LENGTH];
dis_State S0;
int priority_threshold, *priority;
float g_weight = 0.01, constraint_vio;
float compatible_weight = 0, compatible_violation = 0;

extern dis_TypedListList *dis_gparse_predicates, *dis_gparse_functions;
extern dis_PlNode *dis_gorig_initial_facts;
extern dis_Pldis_Operator *dis_gloaded_ops;
extern dis_PrefNode *dis_gloaded_preferences;
extern dis_Facts *dis_ginitial;
extern DuraTab *table;
void print_fl_goals();
extern void mff_record_esp_sol();
void dis_mff_bridge();
void dis_print_ft_name(int);
int dis_get_1P(dis_State *);
int myPERT(int, int, int *, float *);
dis_Bool dis_depend_efs(int, int);
Bool dis_num_depend_efs(int, int);
int dis_find_ef_tif(int ef, float *begin, float *end);
void init_timed_initial();
int dis_is_subtype(int, int);

void my_printplan(int n, int *op, float *time)
{
  int i;
   
  if (time)
  {
    for (i=0;i<n;i++)
    {
      printf("%.5f:\t", time[i]);
      dis_print_op_name(op[i]);
      printf("\n");
    }
    printf("------------------------------\n");
  }
  else
  {
    for (i=0;i<n;i++)
    {
      printf("%d:\t", i);
      dis_print_op_name(op[i]);
      printf("\n");
    }
    printf("------------------------------\n");
  }
}       

float dis_metric_value( dis_State *S )
{
 
  float cost = dis_glnf_metric.c;
  int i;

  for ( i = 0; i < dis_glnf_metric.num_pF; i++ ) 
      cost += dis_glnf_metric.pC[i]*S->f_V[dis_glnf_metric.pF[i]];

  /* not general */  
  if (GpG.SearchModal == -1003 && (GpG.is_constraints || GpG.is_preferences)) 
  {
    cost += compatible_violation;
  }
  if (strcmp( dis_gparse_optimization, "MAXIMIZE" ) == dis_SAME)
    cost = -cost;
  return cost;
}

int dis_contain_fact_state(int f, dis_State *S)
{
  int i;
  
  for (i=0;i<S->num_F;i++)
    if (f == S->F[i])
      return i;
  return -1;    
}

int is_applicable(dis_State *source, int ef)
{
  dis_Bool *in_source;
  int i, j, fl, ret = 0;
  float val, source_val;
  dis_Comparator comp;

  in_source = ( dis_Bool * ) calloc( dis_gnum_ft_conn, sizeof( dis_Bool ) );
  for ( i = 0; i < dis_gnum_ft_conn; i++ ) 
    in_source[i] = dis_FALSE;
  /* setup true facts for effect cond evaluation
   */
  for ( i = 0; i < source->num_F; i++ )
    in_source[source->F[i]] = dis_TRUE;

  /* evaluate effect conditions and setup deleted facts
   */
  for ( j = 0; j < dis_gef_conn[ef].num_PC; j++ ) 
    if ( !in_source[dis_gef_conn[ef].PC[j]] ) 
      break;
  if ( j < dis_gef_conn[ef].num_PC ) 
    goto end;

    /* numeric cond true?
     */
  for ( j = 0; j < dis_gef_conn[ef].num_f_PC; j++ ) 
  {
    fl = dis_gef_conn[ef].f_PC_fl[j];
    val = dis_gef_conn[ef].f_PC_c[j];
    comp = dis_gef_conn[ef].f_PC_comp[j];
    if ( !dis_determine_source_val( source, fl, &source_val ) ) 
        /* condition access to an undefined fluent!
	 */
      goto end;
    if ( !dis_number_comparison_holds( comp, source_val, val ) ) 
      break;
  }
  if ( j < dis_gef_conn[ef].num_f_PC ) 
    goto end;

  ret = 1;
end:
  free(in_source);
  return ret;
}

void dis_add_predicate(char *name, dis_TypedList *args)
{
  dis_TypedListList *tll;

  tll = dis_new_dis_TypedListList();
  tll->next = dis_gparse_predicates;
  
  dis_gparse_predicates = tll;
  tll->predicate = dis_copy_dis_Token( name );
  tll->args = copy_dis_TypedList(args);
}

void dis_add_function(char *name)
{
  char temp[128];
  dis_TypedListList *tll;

  sprintf(temp, "_%s_VIO", name);
  for (tll=dis_gparse_functions;tll;tll=tll->next)
    if (strcmp(tll->predicate, temp) == 0)
      return;
  tll = dis_new_dis_TypedListList();
  tll->next = dis_gparse_functions;
  dis_gparse_functions = tll;
  tll->predicate = dis_copy_dis_Token(temp);
}

void dis_add_initial_fact(char *name)
{
  dis_PlNode *q;
  
  q = dis_new_dis_PlNode(dis_ATOM);
  q->atom = dis_new_dis_TokenList();
  q->atom->item = dis_copy_dis_Token(name);
  q->next = dis_gorig_initial_facts->sons;
  dis_gorig_initial_facts->sons = q; 
}

void dis_add_initial_fv(char *name, char *v)
{
  char temp[128];
  dis_PlNode *fv;
  
  sprintf(temp, "_%s_VIO", name);
  for (fv=dis_gorig_initial_facts->sons;fv;fv=fv->next)
    if (fv->comp == EQ && fv->lh->connective == FHEAD)
      if (strcmp(temp, fv->lh->atom->item) == 0)
        return;
        
  fv = dis_new_dis_PlNode( dis_COMP );
  fv->comp = EQ;
  fv->lh = dis_new_dis_Parsedis_ExpNode( FHEAD );
  fv->lh->atom = dis_new_dis_TokenList();
  fv->lh->atom->item = dis_copy_dis_Token(temp);
  fv->rh = dis_new_dis_Parsedis_ExpNode( NUMBER );
  fv->rh->atom = dis_new_dis_TokenList();
  fv->rh->atom->item = dis_copy_dis_Token(v);
  
  fv->next = dis_gorig_initial_facts->sons;
  dis_gorig_initial_facts->sons = fv; 
}

void dis_add_constraint_1cond(char *name, char *pname, dis_TypedList *args, dis_PlNode *cond, char *type)
{
  dis_Pldis_Operator *scur_op;
  char temp[128];
  dis_TokenList *p;
  dis_TypedList *tl;
  dis_PlNode *q;
  
  sprintf(temp, "%s_%s", name, type);
  scur_op = dis_new_dis_Pldis_Operator( temp );
  scur_op->parse_params = copy_dis_TypedList(args);
  scur_op->preconds = copy_dis_PlNode(cond);

  scur_op->effects = dis_new_dis_PlNode(dis_AND);
  q = dis_new_dis_PlNode(dis_ATOM);
  q->atom = dis_new_dis_TokenList();
  q->atom->item = dis_copy_dis_Token(name);
  p = q->atom;
  scur_op->number_of_real_params = 0;
  for (tl=args;tl;tl=tl->next)
  {
    scur_op->number_of_real_params++;
    p->next = dis_new_dis_TokenList();
    p->next->item = dis_copy_dis_Token(tl->name);
    p = p->next;
  }

  scur_op->effects->sons = q;
  
  dis_add_predicate(name, args);
  dis_add_function(pname);
  dis_add_initial_fv(pname, "1");
  scur_op->next = dis_gloaded_ops;
  dis_gloaded_ops = scur_op;
}


void print_dis_ConNode( dis_ConNode *plnode, int indent )
{
//  dis_PlNode *i_son;
  dis_ConNode *i_son_c;

  if ( !plnode ) {
    printf("none\n");
    return;
  }
  
  switch (plnode->connective) {
  case dis_ALL_c_c: 
    printf("ALL ");
    dis_print_indent(indent);
    printf("(   ");
    print_dis_ConNode(plnode->sons_c,indent+4);
    dis_print_indent(indent);
    printf(")\n");
    break;
  case dis_AND_c_c: 
    printf("A(  ");
    print_dis_ConNode(plnode->sons_c, indent+4);
    if ( plnode->sons ) {
      for ( i_son_c = plnode->sons_c->next; i_son_c!=NULL; i_son_c = i_son_c->next ) {
	dis_print_indent(indent);
	printf("AND ");
	print_dis_ConNode(i_son_c,indent+4);
      }
    }
    dis_print_indent(indent);      
    printf(")\n");
    break;
  case dis_ATOM_c:
    printf("(");
    dis_print_dis_PlNode(plnode->sons, indent+4);
    printf(")\n");
    break;
  case dis_AT_END_c:
    printf ("(AT_END ");
    dis_print_dis_PlNode (plnode->sons, indent + 4);
    dis_print_indent (indent + 3);
    printf (")\n");
    break;

  default:
    printf("\n***** ERROR ****");
    printf("\nprint_plnode: %d > Wrong Node specifier\n", plnode->connective);
    exit(1);
  }     

  if (plnode->next)
  {
    printf("------------------------------\n");
    print_dis_ConNode(plnode->next, indent);
  }

} 

void print_dis_PrefNode( dis_PrefNode *plnode)
{
  printf("%s %s %s\n", plnode->name, plnode->opname, plnode->pname);
  print_dis_ConNode(plnode->body, 2);  
  printf("\n");
  if (plnode->next)
  {
    printf("------------------------------\n");
    print_dis_PrefNode(plnode->next);
  }
}

dis_ConNode *new_dis_ConNode( dis_Connective_Con c )

{

  dis_ConNode *result = ( dis_ConNode * ) calloc( 1, sizeof( dis_ConNode ) );
  dis_CHECK_PTR(result);

  result->connective = c;
  result->number = result->number2 = 0;
  result->sons = NULL;
  result->sons2 = NULL;
  result->sons_c = NULL;
  result->next = NULL;

  return result;

}

dis_PrefNode *new_dis_PrefNode( char *name, char *opname, char *pname)

{

  dis_PrefNode *result = ( dis_PrefNode * ) calloc( 1, sizeof( dis_PrefNode ) );
  dis_CHECK_PTR(result);
  
  result->name = (char *) malloc(strlen(name)+2);
  result->opname = (char *) malloc(strlen(opname)+1);
  result->pname = (char *) malloc(strlen(pname)+1);
  strcpy(result->name+1, name);
  result->name[0] = '_';
  strcpy(result->pname, pname);
  strcpy(result->opname, opname);
  result->body = NULL;
  result->next = NULL;

  return result;

}

void dis_add_constraints()
{
  dis_PrefNode *p;
  dis_TypedList *t;
  int c, i;
  char temp[16];
  
  for (p=dis_gloaded_preferences;p;p=p->next)
  {
    /*not general, to ignore preferences*/
    if (GpG.SearchModal == -1003)
    {
      c = 0;  
      for (t=p->args;t;t=t->next)
        c++;
      if (c == 4)
      {
//        printf("Ignore the following constraint\n");
//        print_dis_ConNode(p->body, 0);
        for (i=1;i<strlen(p->pname)-1;i++)
          temp[i-1] = p->pname[i];
        temp[i-1] = 0;
        compatible_weight = atof(temp);
        if (compatible_weight == 0)
          compatible_weight = 100000000;
        continue;
      }
    }
  
    if (strcmp(p->opname, "GOALS") == 0)
    {
      dis_add_constraint_1cond(p->name, p->pname, p->args, p->body->sons, "ATEND");
      continue;
    }

    /* Preference on operatior definitions */
    dis_add_predicate(p->name, p->args);
    dis_add_function(p->pname);
    dis_add_initial_fv(p->pname, "0");
    dis_add_initial_fact(p->name);
  }

/*    printf("\npreferences:\n");
    if (dis_gloaded_preferences)
      print_dis_PrefNode(dis_gloaded_preferences);
    printf("\nconstraints:\n");
    if (dis_gloaded_constraints)
      print_dis_ConNode(dis_gloaded_constraints, 0);
    printf("\nover\n");*/
}

void print_all_grounded_constraints()
{
  int i, j, k, l;

  printf("\n*******************************\n");
  fflush(stdout);
  for (k=0;k<dis_gnum_con_conn;k++)
  {
    dis_print_ft_name(dis_gcon_conn[k].fact);
    printf(": weight = %f %f %f\n", dis_gcon_conn[k].weight, dis_gcon_conn[k].time1, dis_gcon_conn[k].time2);
    for (l=0;l<dis_gcon_conn[k].num;l++)
    {
      if (l == dis_gcon_conn[k].num1)
        printf("+++++++++++++++++++++++++++++++++++++++++++\n");
      i = dis_gcon_conn[k].cond[l];
      dis_print_op_name( dis_gef_conn[i].op );
      if ( dis_gef_conn[i].illegal ) printf(" ******ILLEGAL************************");
      if ( dis_gef_conn[i].removed ) printf(" ******REMOVED************************");
      printf("\n----------PCS:");
      for ( j = 0; j < dis_gef_conn[i].num_PC; j++ ) {
	printf("\n");
	dis_print_ft_name( dis_gef_conn[i].PC[j] );
      }
      printf("\n----------f_PCS:");
      for ( j = 0; j < dis_gef_conn[i].num_f_PC; j++ ) {
	printf("\n");
	dis_print_fl_name( dis_gef_conn[i].f_PC_fl[j] );
	if ( dis_gef_conn[i].f_PC_comp[j] == GEQ ) {
	  printf(" >= ");
	} else {
	  printf(" > ");
	}
	printf("%f", dis_gef_conn[i].f_PC_c[j]);
      }
      printf("\nDIRECT: ");
      for ( j = 0; j < dis_gnum_fl_conn; j++ ) {
	if ( dis_gef_conn[i].f_PC_direct_comp[j] == IGUAL ) {
	  printf("IGUAL  |  ");
	}
	if ( dis_gef_conn[i].f_PC_direct_comp[j] == GEQ ) {
	  printf(">= %f  |  ", dis_gef_conn[i].f_PC_direct_c[j]);
	}
	if ( dis_gef_conn[i].f_PC_direct_comp[j] == GE ) {
	  printf("> %f  |  ", dis_gef_conn[i].f_PC_direct_c[j]);
	}
      }
    }
    printf("\n-----------------------------------------\n");
  }
}

void dis_setup_grounded_constraints()
{
  int i, j, k, n = 0, op, temp[MAX_CONSTRAINTS];
  char temp1[128];
  dis_Fact *f;
  dis_PrefNode *pref;
  
  for (i=0;i<dis_gnum_ft_conn;i++)
  {
    f = &(dis_grelevant_facts[i]);
    // is it an artificial predicate for constraint?
    if (f->predicate < 0 || dis_gpredicates[f->predicate][0] != '_')
      continue;
    // is it a precondition preference 
    if (dis_gft_conn[i].num_A == 0)
      continue;
    if (n < MAX_CONSTRAINTS)
      temp[n++] = i;
    else
    {
      fprintf(stderr, "Too many constraints! increase MAX_CONSTRAINTS (currently %d)\n", MAX_CONSTRAINTS);
      exit(1);
    }
  }
  dis_gcon_conn = (ConConn *) malloc(sizeof(ConConn)*n);
  dis_gnum_con_conn = n;

  for (i=0;i<n;i++)
  {
    for (pref=dis_gloaded_preferences;pref;pref=pref->next)
      if (strcmp(pref->name, dis_gpredicates[(dis_grelevant_facts[temp[i]]).predicate]) == 0)
        break;
    if (!pref)    
    {
      fprintf(stderr, "Unexpected error: cannot find %s in dis_gloaded_preference\n", pref->name);
      exit(1);
    }
    if (pref->body->connective == dis_ATOM_c)
      dis_gcon_conn[i].connective = dis_AT_END_c;
    else
      dis_gcon_conn[i].connective = pref->body->connective;
    dis_gcon_conn[i].fact = temp[i];
    dis_gcon_conn[i].num = dis_gft_conn[temp[i]].num_A;
    dis_gcon_conn[i].cond = (int *) malloc(sizeof(int)*dis_gcon_conn[i].num);

    switch(dis_gcon_conn[i].connective)
    {
    case dis_HOLD_DURING_c:
      dis_gcon_conn[i].time2 = pref->body->number2;
    case dis_WITHIN_c:
    case dis_HOLD_AFTER_c:
      dis_gcon_conn[i].time1 = pref->body->number;
    case dis_ATOM_c:
    case dis_AT_END_c:
    case dis_ALWAYS_c:
    case dis_SOMETIME_c:
    case dis_AT_MOST_ONCE_c:
      memcpy(dis_gcon_conn[i].cond, dis_gft_conn[temp[i]].A, sizeof(int)*dis_gcon_conn[i].num);
      dis_gcon_conn[i].num1 = dis_gcon_conn[i].num;
      break;
    case dis_ALWAYS_WITHIN_c:
      dis_gcon_conn[i].time1 = pref->body->number;
    case dis_SOMETIME_AFTER_c:
    case dis_SOMETIME_BEFORE_c:
      dis_gcon_conn[i].num1 = 0;
      k = dis_gft_conn[temp[i]].num_A;
      for (j=0;j<dis_gft_conn[temp[i]].num_A;j++)
      {
        op = dis_gef_conn[dis_gft_conn[temp[i]].A[j]].op;
        sprintf(temp1, "%s_1_", pref->name);
        if (strncmp(dis_gop_conn[op].action->name, temp1, strlen(temp1)) == 0)
          dis_gcon_conn[i].cond[dis_gcon_conn[i].num1++] = dis_gft_conn[temp[i]].A[j];
        else
          dis_gcon_conn[i].cond[--k] = dis_gft_conn[temp[i]].A[j];
      }
      break;
    case dis_AND_c_c:
    case dis_ALL_c_c:
        fprintf(stderr, "Unexpected error: dis_gcon_conn[i].connective is AND or FORALL\n");
        exit(1);
    }
    
    switch(dis_gcon_conn[i].connective)
    {
    case dis_ALWAYS_WITHIN_c:
      break;
    case dis_HOLD_DURING_c:
      break;
    case dis_HOLD_AFTER_c:
      break;
    case dis_ATOM_c:
    case dis_AT_END_c:
    case dis_SOMETIME_c:
    case dis_WITHIN_c:
      break;
    case dis_AT_MOST_ONCE_c:
    case dis_ALWAYS_c:
      for (j=0;j<dis_gcon_conn[i].num;j++)
        dis_gef_conn[dis_gcon_conn[i].cond[j]].num_A = 0;
      break;
    case dis_SOMETIME_BEFORE_c:
      for (j=0;j<dis_gcon_conn[i].num1;j++)
        dis_gef_conn[dis_gcon_conn[i].cond[j]].num_A = 0;
      break;
    case dis_SOMETIME_AFTER_c:
      for (j=dis_gcon_conn[i].num1;j<dis_gcon_conn[i].num;j++)
        dis_gef_conn[dis_gcon_conn[i].cond[j]].num_A = 0;
      break;
    case dis_AND_c_c:
    case dis_ALL_c_c:
      break;
    }

    if (strncmp(pref->name, "_CONSTRAINTS", strlen("_CONSTRAINTS")) == 0)
    {
      dis_gcon_conn[i].weight = 100000000;
      dis_gcon_conn[i].fv = -1;
      continue;
    }
    sprintf(temp1, "_%s_VIO", pref->pname);
    for (j=0;j<dis_glnf_metric.num_pF;j++)
    {
      if (strncmp(dis_grelevant_fluents_name[dis_glnf_metric.pF[j]], temp1, strlen(temp1)) == 0)
      {
        dis_gcon_conn[i].fv = dis_glnf_metric.pF[j];
        dis_gcon_conn[i].weight = dis_glnf_metric.pC[j];
        break;
      }        
    }
  }
//  print_all_grounded_constraints();
  priority_threshold = 0;
  priority = (int *) calloc(dis_gnum_op_conn, sizeof(int));
  
  for (i=0;i<dis_gnum_op_conn;i++)
  {
    priority[i] = 0;  
    if (!dis_gop_conn[i].action->name)
      continue;
// skip artificial actions and derived predicates
    if (dis_gop_conn[i].action->name[0] == '_' || 
    strncmp(dis_gop_conn[i].action->name, "deripred", 8) == 0)
      priority[i] = -1000;
    else
      if (dis_gop_conn[i].action->name[0] == '+')
        priority[i] = -1;
  }
}

int check_condition(dis_State *S, int i, int s, int e)
{
  int j;

  for (j=s;j<e;j++)
    if (is_applicable(S, dis_gcon_conn[i].cond[j]))
      return 1;
  return 0;
}


// conditional effects are partially implemented
void setup_conds_effects_timespec()
{
  int i, j, k, l, ef, m, num_PC, num_A, num_D;
  dis_Action *a;
  dis_Actiondis_Effect *e;
  
  TS = (TimeSpecConn *) malloc(dis_gnum_ef_conn*sizeof(TimeSpecConn));
  for (i=0;i<dis_gnum_ef_conn;i++)
    TS[i].PC = TS[i].A = TS[i].D = NULL;
  for (i=0;i<dis_gnum_op_conn;i++)
  {
    if (priority[i] < priority_threshold)
      continue;
    a = dis_gop_conn[i].action;
    for (k=0;k<dis_gop_conn[i].num_E;k++)
    {
      ef = dis_gop_conn[i].E[k];
      e = &(a->effects[k]);
      num_PC = dis_gef_conn[ef].num_PC;
      if (num_PC)
      {
        TS[ef].PC = (TimeSpec *) calloc(num_PC, sizeof(TimeSpec));
        for (j=0;j<num_PC;j++)
          TS[ef].PC[j] = AT_START_TIME;
      }
      num_A = e->num_adds;
      if (num_A)
      {
        TS[ef].A = (TimeSpec *) calloc(num_A, sizeof(TimeSpec));
        for (j=0;j<num_A;j++)
          TS[ef].A[j] = AT_END_TIME;
      }
      num_D = e->num_dels;
      if (num_D)
      {
        TS[ef].D = (TimeSpec *) calloc(num_D, sizeof(TimeSpec));
        for (j=0;j<num_D;j++)
          TS[ef].D[j] = AT_START_TIME;
      }

      // not general
      for (j=0;j<num_D;j++)
      {
        for (l=0;l<num_A;l++)
          if (e->adds[l] == e->dels[j])
            break;
        if (l < num_A)
        {
          for (m=0;m<dis_gef_conn[ef].num_PC;m++)
            if (dis_gef_conn[ef].PC[m] == e->dels[j])
              break;
          if (l == dis_gef_conn[ef].num_PC)
          {
            TS[ef].D[j] = AT_END_TIME;
            TS[ef].A[l] = AT_START_TIME;
          }
        }
      }
    }
  }

  if (GpG.is_durative)
  {
    for (duration_fl=0;duration_fl<dis_gnum_fl_conn;duration_fl++)
      if (strncmp(dis_grelevant_fluents_name[duration_fl], "DURATIONFUNCTION", strlen("DURATIONFUNCTION")) == 0)
        break;
    if (duration_fl == dis_gnum_fl_conn)
    {
      fprintf(stderr, "Missing duration information\n");
    }
  }
}

float Fmax(float a, float b)
{
  if (a > b)
    return a;
  return b;
}

float dependent_efs(int a, int b, float duration)
{
  int i, j;
  float v = -1000000;
  
  for(i=0; i<dis_gef_conn[a].num_PC; i++) 
  {
    for(j=0; j<dis_gef_conn[b].num_A; j++) 
      if(dis_gef_conn[a].PC[i] == dis_gef_conn[b].A[j])
      { 
        if (TS[b].A[j] == AT_START_TIME)
          v = Fmax(v, 0);
        else
          v = Fmax(v, duration);
      }
    for(j=0; j<dis_gef_conn[b].num_D; j++)
      if(dis_gef_conn[a].PC[i] == dis_gef_conn[b].D[j])
      { 
        if (TS[b].D[j] == AT_START_TIME)
          v = Fmax(v, 0);
        else
          v = Fmax(v, duration);
      }
  }
  for(i=0; i<dis_gef_conn[b].num_PC; i++)
  {
      for(j=0; j<dis_gef_conn[a].num_A; j++) 
        if(dis_gef_conn[b].PC[i] == dis_gef_conn[a].A[j])
          v = Fmax(v, duration);
      for(j=0; j<dis_gef_conn[a].num_D; j++)
        if(dis_gef_conn[b].PC[i] == dis_gef_conn[a].D[j])
          v = Fmax(v, duration);
  } 
  return v;
}

dis_Bool inconsistent_efs(int a, int b)
{
  int i, j, k, op;
  dis_Actiondis_Effect *e;
  
    for (i=0;i<dis_gef_conn[a].num_PC;i++)
    {
      op = dis_gef_conn[a].op;
      for (j=0;j<dis_gop_conn[op].num_E;j++)
        if (dis_gop_conn[op].E[j] == a)
          break;
      e = &(dis_gop_conn[op].action->effects[j]);
      for (k=0;k<e->num_dels;k++)
        if (dis_gef_conn[a].PC[i] == e->dels[k])
          break;
      if (k == e->num_dels)
        continue;
      for (j=0;j<dis_gef_conn[b].num_PC;j++)
        if (dis_gef_conn[a].PC[i] == dis_gef_conn[b].PC[j])
          return dis_TRUE;
    }
  return dis_FALSE;
}

int numeric_dependent(int a, int b)
{
  int i;

  if (!dis_gfl_conn[a].artificial && !dis_gfl_conn[b].artificial)
    return a == b;
  if (dis_gfl_conn[a].artificial)
  {
    for (i=0;i<dis_gfl_conn[a].num_lnf;i++)
      if (numeric_dependent(dis_gfl_conn[a].lnf_F[i], b))
        return 1;
    return 0;
  }
  if (dis_gfl_conn[b].artificial)
  {
    for (i=0;i<dis_gfl_conn[b].num_lnf;i++)
      if (numeric_dependent(dis_gfl_conn[b].lnf_F[i], a))
        return 1;
    return 0;
  }
}

float numeric_dependent_efs(int a, int b, float duration) 
{
  int i, j, k, fl_;
  float v = -1000000;

  for(i=0; i<dis_gef_conn[a].num_f_PC; i++)
    for(j=0; j<dis_gef_conn[b].num_f_PC; j++)
      if (numeric_dependent(dis_gef_conn[a].f_PC_fl[i], dis_gef_conn[b].f_PC_fl[j]))
        return Fmax(v, duration);
  for(i=0; i<dis_gef_conn[a].num_f_PC; i++)
    for(j=0; j<dis_gef_conn[b].num_IN; j++)
      if (numeric_dependent(dis_gef_conn[a].f_PC_fl[i], dis_gef_conn[b].IN_fl[j]))
        if (dis_gef_conn[b].IN_c[j] > 0)
          return Fmax(v, duration);
  for(j=0; j<dis_gef_conn[b].num_AS; j++)
    if (dis_gef_conn[b].AS_fl[j] == duration_fl)
    {
      fl_ = dis_gef_conn[b].AS_fl_[j];
      if (fl_ < 0)
        continue;
      for (i=0; i<dis_gef_conn[a].num_IN; i++)
        if (numeric_dependent(fl_, dis_gef_conn[a].IN_fl[i]))
          return Fmax(v, duration);
      for (i=0; i<dis_gef_conn[a].num_AS; i++)
        if (numeric_dependent(fl_, dis_gef_conn[a].AS_fl[i]))
          return Fmax(v, duration);
      if (!dis_gfl_conn[fl_].artificial ) 
        continue;
    }
  for(i=0; i<dis_gef_conn[a].num_AS; i++)
    if (dis_gef_conn[a].AS_fl[i] == duration_fl)
    {
      fl_ = dis_gef_conn[a].AS_fl_[i];
      if (fl_ < 0)
        continue;
      for (j=0; j<dis_gef_conn[b].num_IN; j++)
        if (numeric_dependent(fl_, dis_gef_conn[b].IN_fl[j]))
          return Fmax(v, duration);
      for (j=0; j<dis_gef_conn[b].num_AS; j++)
        if (numeric_dependent(fl_, dis_gef_conn[b].AS_fl[j]))
          return Fmax(v, duration);
    }
  return v; 
}

void delete_GpG_esp_solution(int i)
{
  int j;
  
  if (i < 0 || i >= GpG.num_esp_sol)
  {
    fprintf(stderr, "Delete a nonexistent action\n");
    exit(1);
  }
  for(j=i+1;j<GpG.num_esp_sol;j++) 
  {
    GpG.esp_solution[j-1] = GpG.esp_solution[j];
    GpG.pert_endtime[j-1] = GpG.pert_endtime[j];
    GpG.duration[j-1] = GpG.duration[j];
  }
  GpG.num_esp_sol--;
}

int verify_plan()
{
  int i, op, a;
  char *s;
  
  dis_source_to_dest(&dis_ginitial_state, &S0);
  if (GpG.is_til)
  for (i=0;i<GpG.num_esp_sol;i++)
  {
    op = GpG.esp_solution[i];
    s = dis_gop_conn[op].action->name;
    if (s && strncmp(s, "timed-initial-literals", 21) == 0)
      if (dis_gef_conn[dis_gop_conn[op].E[0]].num_A > 0 &&
      dis_gef_conn[dis_gop_conn[op].E[0]].num_D == 0)
      {
        dis_result_to_dest(&dis_mff_sol, &dis_ginitial_state, op, -1);
        dis_source_to_dest(&dis_ginitial_state, &dis_mff_sol);
      }
  }
  if (GpG.is_deripred)
  {
    dis_result_to_dest(&dis_mff_sol, &dis_ginitial_state, -1, -1);
    dis_source_to_dest(&dis_ginitial_state, &dis_mff_sol);
  }

  for (i=0;i<GpG.num_esp_sol;i++)
  {
    if (GpG.is_durative)
      dis_ginitial_state.f_V[duration_fl] = 0;
    if (dis_result_to_dest(&dis_mff_sol, &dis_ginitial_state, GpG.esp_solution[i], -1))
    {
      dis_source_to_dest(&dis_ginitial_state, &dis_mff_sol);
      if (GpG.is_durative)
        GpG.duration[i] = dis_ginitial_state.f_V[duration_fl];
    }
    else
      return -i;
  }
  for (i = 0; i < saved_dis_gnum_fnumeric_goal; i++)
  {
    dis_gnum_fnumeric_goal = i+1;
    dis_gfnumeric_goal_comp[i] = saved_dis_gfnumeric_goal_comp[i];
    dis_gfnumeric_goal_fl[i] = saved_dis_gfnumeric_goal_fl[i];
    dis_gfnumeric_goal_c[i] = saved_dis_gfnumeric_goal_c[i];
  }
  for (i = 0; i < saved_dis_gnum_flogic_goal; i++)
  {
    dis_gflogic_goal[i] = saved_dis_gflogic_goal[i]; 
    dis_gnum_flogic_goal = i+1;
  }
  if (dis_get_1P(&dis_mff_sol) > 0)
    return -GpG.num_esp_sol;

  for (i=0;i<GpG.num_esp_sol;i++)
  {
    a = GpG.esp_solution[i];
    s = dis_gop_conn[a].action->name;
    if(!s || strncmp(s, "deripred", 8) == 0 || s[0] == '_' ||
    strncmp(s, "timed-initial-literals", 21) == 0)
    {
      delete_GpG_esp_solution(i);
      i--;
    }    
  }
  
  return 1;
}

int schedule_actions(int n, int *plan, float *time, float *dur0)
{
  int i, j, a, b, res;
  float begin, end, *dur;
 
  if (dur0)
    dur = dur0;
  else
  {
    dur = (float *) malloc(sizeof(float)*n);
    for (i=0;i<n;i++)
    {
      a = dis_gop_conn[plan[i]].E[0];
      for (j=0;j<dis_gef_conn[a].num_AS;j++)
        if (dis_gef_conn[a].AS_fl[j] == duration_fl)
          dur[i] = dis_gef_conn[a].AS_c[j];
    }
  }  
 
  for (i=0;i<n;i++)
  {
    a = dis_gop_conn[plan[i]].E[0];
    time[i] = 0; 
    for(j=0; j<i; j++) 
    {
      b = dis_gop_conn[plan[j]].E[0];
      begin = Fmax(dependent_efs(a, b, dur[j]), 
      numeric_dependent_efs(a, b, dur[j])); 
      if (time[j] + begin > time[i]) 
        time[i]= time[j] + begin;
    }

    for (j=0;j<i;j++)
    {
      b = dis_gop_conn[plan[j]].E[0];
      if (inconsistent_efs(a, b) && 
      time[i] < time[j] + dur[j] &&
      time[i] + dur[i] > time[j])
        time[i] = time[j] + dur[j];
    }

    if (GpG.is_til)
    {
      res = dis_find_ef_tif(a, &begin, &end);
      if (res >= 0) 
      {
        if (time[i] < begin) 
          time[i] = begin;
        if(time[i] > end) 
        {
//          printf("Cannot solve.\n");
          if (!dur0)
            free(dur);
          return 0;
        }
      }
    }

    /* sort the actions */    
    for (j=0;j<i;j++)
      if (time[i] < time[j])
        break;
    begin = dur[i];
    end = time[i];
    for (b=i-1;b>=j;b--)	
    {
      plan[b+1] = plan[b];
      dur[b+1] = dur[b];
      time[b+1] = time[b];
    }
    plan[j] = a;
    time[j] = end;
    dur[j] = begin;
  }
  
  begin = 0.001;
  while (time[n-1]/begin > 1000000)
    begin *= 2;
  for (i=0;i<n;i++)
    time[i] += begin*i + 0.001;
  if (!dur0)
    free(dur);
  return 1;
}

/* for ignored preferences*/
int calculate_compatible_violation()
{
  int i, j, k, l, ret = 1, conn, comp;
  int nr_cr = 0, on[50], loc[50];
  dis_State dummy, dummy1;
  dis_Fact *f;
  
  dis_make_state(&dummy, dis_gnum_ft_conn, dis_gnum_fl_conn);
  dis_make_state(&dummy1, dis_gnum_ft_conn, dis_gnum_fl_conn);
  dis_source_to_dest(&dummy, &S0);
  for (k=0;k<traj_n;k++)
    if (dis_result_to_dest0(&dummy1, &dummy, traj_plan[k], -1))
      dis_source_to_dest(&dummy, &dummy1);
    else
    {
      fprintf(stderr, "Unexpected error during ALWAYS: infeasible action\n");
      exit(1);
    }
    
  for (i=0;i<dummy.num_F;i++)
  {
    f = &(dis_grelevant_facts[dummy.F[i]]);
    if (dis_gpredicates[f->predicate][0] == 'O' &&
    dis_gpredicates[f->predicate][1] == 'N')
    {
      on[nr_cr] = f->args[0];
      loc[nr_cr++] = f->args[1];
    }
  }

  for (i=0;i<dis_gnum_predicates;i++)
    if (dis_garity[i] == 2 && !dis_gis_added[i] && !dis_gis_added[i])
    {
      if (dis_gpredicates[i][2] == 'N')
        conn = i;
      else
        comp = i;
    }

  compatible_violation = 0;
  for (i=0;i<nr_cr;i++)
    for (j=0;j<nr_cr;j++)
      if (i != j)
      {
        for (k=0;k<dis_gnum_initial_predicate[conn];k++)
        {
          f = &(dis_ginitial_predicate[conn][k]);
          if (f->args[0] == loc[i] && f->args[1] == loc[j])
            break;
        }
        if (k == dis_gnum_initial_predicate[conn])
          continue;
          
        for (l=0;l<dis_gnum_initial_predicate[comp];l++)
        {
          f = &(dis_ginitial_predicate[comp][l]);
          if (f->args[0] == on[i] && f->args[1] == on[j])
            break;
        }
        if (l == dis_gnum_initial_predicate[comp])
        {
          if (compatible_weight < 1000000)
            compatible_violation += compatible_weight;
          else
            ret = 0;
        }
      }

  free(dummy.F);
  free(dummy.f_D);
  free(dummy.f_V);
  free(dummy1.F);
  free(dummy1.f_D);
  free(dummy1.f_V);
  return ret;
}

int dis_check_constraints(dis_State *S)
{
  char temp[128];
  int i, j, k, l, ret;
  dis_State dummy, dummy1, dummy2;
  char *f = (char *) calloc(dis_gnum_ft_conn, sizeof(char));
  float tl, tk;

  dis_make_state(&dummy, dis_gnum_ft_conn, dis_gnum_fl_conn);
  dis_make_state(&dummy1, dis_gnum_ft_conn, dis_gnum_fl_conn);
  dis_make_state(&dummy2, dis_gnum_ft_conn, dis_gnum_fl_conn);
  for (i=0;i<S->num_F;i++)
    f[S->F[i]] = 1;
  ret = 1;
  constraint_vio = 0;

  for (i=0;i<dis_gnum_con_conn;i++)
  {
    S->f_V[dis_gcon_conn[i].fv] = 0;
    switch(dis_gcon_conn[i].connective)
    {
    case dis_AT_END_c:
      if (check_condition(S, i, 0, dis_gcon_conn[i].num))
        f[dis_gcon_conn[i].fact] = 1;
      else
        f[dis_gcon_conn[i].fact] = 0;
      break;

    case dis_ATOM_c:
    case dis_AND_c_c:
    case dis_ALL_c_c:
      fprintf(stderr, "Unexpected error: p->body->connective is ATOM, AND, or FORALL\n");
      exit(1);
      break;
    default:
      break;
    }
  }

  for (i=0;i<dis_gnum_con_conn;i++)
    if (f[dis_gcon_conn[i].fact] == 0)
    {
      if (dis_gcon_conn[i].weight == 100000000)
        ret = 0;
      else
        S->f_V[dis_gcon_conn[i].fv]++;
    }
  S->num_F = 0;
  for (i=0;i<dis_gnum_ft_conn;i++)
    if (f[i])
      S->F[S->num_F++] = i;      

  if (GpG.SearchModal == -1003 && (GpG.is_constraints || GpG.is_preferences))
    ret = ret && calculate_compatible_violation();
  free(f);
  free(dummy.F);
  free(dummy.f_D);
  free(dummy.f_V);
  free(dummy1.F);
  free(dummy1.f_D);
  free(dummy1.f_V);
  free(dummy2.F);
  free(dummy2.f_D);
  free(dummy2.f_V);
  return ret;  
}

float dis_check_constraints1(dis_State *S)
{
  int i, j, k, l, ret;
  dis_State dummy, dummy1, dummy2;
  char *f = (char *) calloc(dis_gnum_ft_conn, sizeof(char));
  float tl, tk;

  dis_make_state(&dummy, dis_gnum_ft_conn, dis_gnum_fl_conn);
  dis_make_state(&dummy1, dis_gnum_ft_conn, dis_gnum_fl_conn);
  dis_make_state(&dummy2, dis_gnum_ft_conn, dis_gnum_fl_conn);
  for (i=0;i<S->num_F;i++)
    f[S->F[i]] = 1;
  ret = 1;
  constraint_vio = 0;
  
  for (i=0;i<dis_gnum_con_conn;i++)
  {
    S->f_V[dis_gcon_conn[i].fv] = 0;
    switch(dis_gcon_conn[i].connective)
    {
    case dis_AT_END_c:
      if (check_condition(S, i, 0, dis_gcon_conn[i].num))
        f[dis_gcon_conn[i].fact] = 1;
      else
        f[dis_gcon_conn[i].fact] = 0;
      break;

    case dis_ATOM_c:
    case dis_AND_c_c:
    case dis_ALL_c_c:
      fprintf(stderr, "Unexpected error: p->body->connective is ATOM, AND, or FORALL\n");
      exit(1);
      break;
    default:
      break;
    }
  }

  for (i=0;i<dis_gnum_con_conn;i++)
    if (f[dis_gcon_conn[i].fact] == 0)
      S->f_V[dis_gcon_conn[i].fv]++;
  S->num_F = 0;
  for (i=0;i<dis_gnum_ft_conn;i++)
    if (f[i])
      S->F[S->num_F++] = i;      

  free(f);
  free(dummy.F);
  free(dummy.f_D);
  free(dummy.f_V);
  free(dummy1.F);
  free(dummy1.f_D);
  free(dummy1.f_V);
  free(dummy2.F);
  free(dummy2.f_D);
  free(dummy2.f_V);
  return ret;
}

void output_solution( char *fact_file_name )
{
  int i, j, index;
  char cNameFile[256];
  FILE *fp;
  float time, makespan;
  dis_Action *a;
  
  makespan = 0;
  if (GpG.is_durative)
    for (i=0;i<GpG.num_esp_sol;i++)
    {
      index = dis_gop_conn[GpG.esp_solution[i]].E[0];
      if (GpG.duration[i] + GpG.pert_endtime[i] > makespan)
        makespan = GpG.duration[i] + GpG.pert_endtime[i];
    }
  else
    makespan = GpG.num_esp_sol;
  
  fprintf(stderr, "\nSolution found.\n");
  if (gcmd_line.out_file_name[0] == 0)
  { 
    for (i=strlen(fact_file_name)-1;i>=0;i--)
      if (fact_file_name[i] == '/' || fact_file_name[i] == '\\')
	break;
    strcpy(cNameFile, fact_file_name+i+1);
    strcat(cNameFile, ".soln");
  }
  else
    strcpy(cNameFile, gcmd_line.out_file_name);

    // Hoffmann
  if (gcmd_line.out_file_name[0] == 0)   
    fp = stdout;
  else
  if ((fp = fopen (cNameFile, "w")) == NULL)
  { 
    fprintf (stderr, "\n\n\nError opening output file: %s", cNameFile);
    MSG_ERROR (WAR_OPEN_FILE);
    return;
  }
    
  times (&glob_end_time);
  time = (float) ((glob_end_time.tms_utime -
                 glob_start_time.tms_utime +
                 glob_end_time.tms_stime -  
                 glob_start_time.tms_stime) / 100.0);
                 
  fprintf (fp, "\n; Time %.2f", time + GpG.glob_dist_time); 
  fprintf (fp, "\n; ParsingTime %.2f", gparsing_time);
  if (dis_gmetric)
  {
      j = 0;
      for (i=0;i<GpG.num_esp_sol;i++)
      {
        index = GpG.esp_solution[i] ; 
        a = dis_gop_conn[index].action;
        if ( a->norm_operator || a->pseudo_action ) 
          j++;
      }

    fprintf (fp, "\n; NrActions %d", j);
    fprintf (fp, "\n; MakeSpan");
    fprintf (fp, "\n; MetricValue %.3f", dis_metric_value(&dis_mff_sol) + dis_gtt*makespan);
  }
  else
    if (GpG.is_durative)
    {
      j = 0;
      for (i=0;i<GpG.num_esp_sol;i++)
      {
        index = GpG.esp_solution[i] ; 
        a = dis_gop_conn[index].action;
        if ( a->norm_operator || a->pseudo_action ) 
          j++;
      }

      fprintf (fp, "\n; NrActions %d", j);
      fprintf (fp, "\n; MakeSpan");
      fprintf (fp, "\n; MetricValue %.3f", makespan);
    }
    else
    {
      j = 0;
      for (i=0;i<GpG.num_esp_sol;i++)
      {
        index = GpG.esp_solution[i] ; 
        a = dis_gop_conn[index].action;
        if ( a->norm_operator || a->pseudo_action ) 
          j++;
      }
      
      fprintf (fp, "\n; NrActions %d", j);
      fprintf (fp, "\n; MakeSpan");
      fprintf (fp, "\n; MetricValue");
    }
  if (GpG.subsolver == 1)
    fprintf (fp, "\n; PlanningTechnique Modified-FF(best-first search) as the subplanner");
  else if (GpG.subsolver == 2)
    fprintf (fp, "\n; PlanningTechnique Downward as the subplanner");
  else
    fprintf (fp, "\n; PlanningTechnique Modified-FF(enforced hill-climbing search) as the subplanner");
  fprintf (fp, "\n\n");
                 
  if (GpG.is_durative)
  for (i = 0; i < GpG.num_esp_sol; i++)
  {
    index = GpG.esp_solution[i] ; 
    a = dis_gop_conn[index].action;
    if ( a->norm_operator || a->pseudo_action ) 
    {
      fprintf (fp, "%.3f: ", GpG.pert_endtime[i]);
      if (a->name[0] != '+')
        fprintf(fp, "(%s", a->name );
      else
        for (j=strlen(a->name);j>=0;j--)
          if (a->name[j] == '_')
          {
            fprintf(fp, "(%s", a->name + j + 1);
            break;
          }
      
      for ( j = 0; j < a->num_name_vars; j++ ) 
        fprintf(fp, " %s", dis_gconstants[a->name_inst_table[j]]);
      fprintf(fp, ") [%.4f]\n", GpG.duration[i]);
    } 
  } 
  else
  for (i = 0; i < GpG.num_esp_sol; i++)
  {
    index = GpG.esp_solution[i] ; 
    a = dis_gop_conn[index].action;
    if ( a->norm_operator || a->pseudo_action ) 
    {
      fprintf (fp, "%.3f: ", 1.001*i+0.001);
      fprintf(fp, "(%s", a->name );
      for ( j = 0; j < a->num_name_vars; j++ ) 
        fprintf(fp, " %s", dis_gconstants[a->name_inst_table[j]]);
      fprintf(fp, ") [1]\n");
    } 
  } 
  if (fp != stdout)
  {
    fclose (fp);
    fprintf(stderr, "Answer file is %s\n\n", cNameFile);
  }
} 

int group_id(int fact)
{
  int i, j;
  char temp[128], temp1[128];
  dis_TransitiveGraph *graph;

  dis_print_ft_name_string(fact, temp);
  if (strncmp("(dis_NOT-", temp, strlen("(dis_NOT-")) == 0)
  {
    for (i=0;i<num_transitiveGraph;i++)
    {
      graph = &(transitiveGraph[i]);
      for(j=0;j<graph->elementSize;j++)
      {
        dis_print_ft_name_string(graph->element[j], temp1);
        if (strcmp(temp+strlen("(dis_NOT-"), temp1+1) == 0)
          return i;
      }
    }
    return -1;
  }

  for(i=0;i<num_transitiveGraph;i++)
  {
    graph = &(transitiveGraph[i]);
    for(j=0;j<graph->elementSize;j++)
      if(graph->element[j] == fact) 
        return i;
  }
  return -1;
}

void multiply_group_elements(int n, int *a)
{
  int i, *aa;
  dis_TransitiveGraph *graph;
  
  if (n == gl.n)
  {
    if (gnum_templates == MAX_TEMPLATES)
    {
      fprintf(stderr, "MAX_TEMPLATES=%d is too small\n", MAX_TEMPLATES);
      exit(1);
    }
    gtemplates[gnum_templates] = (int *) malloc(n*sizeof(int));
    memcpy(gtemplates[gnum_templates++], a, n*sizeof(int));
  }
  else
  {
    if (a[n] == -1)
    {
      aa = (int *) malloc(gl.n*sizeof(int));
      memcpy(aa, a, gl.n*sizeof(int));
      graph = &(transitiveGraph[gl.i[n]]);
      for (i=0;i<graph->elementSize;i++)
        if (graph->reachable[i])
        {
          a[n] = graph->element[i];
          multiply_group_elements(n+1, a);
          memcpy(a, aa, gl.n*sizeof(int));
        }
      free(aa);
    }
    else
      multiply_group_elements(n+1, a);
  }
}

int is_landmark_element(int f, int gf)
{
  int g, j, k;
  
  g = group_id(gf);
  if (transitiveGraph[g].edgeListSize == transitiveGraph[g].elementSize - 1)
  {
    for (k=0;k<transitiveGraph[g].elementSize;k++)
      if (dis_contain_fact_state(transitiveGraph[g].element[k], &dis_ginitial_state) != -1)
        break;
    if (k == f)
      return 1;
    if (k == gf)
      return 0;

    while (1)
    {
      for (j=0;j<transitiveGraph[g].elementSize;j++)
        if (transitiveGraph[g].reachable[j] == 1 && transitiveGraph[g].edge[k*transitiveGraph[g].elementSize+j]) 
          break;
      if (j == transitiveGraph[g].elementSize)
        break;
      k = j;
      if (k == f)
        return 1;
      if (k == gf)
        return 0;
    }
    return 0;
  }
  else
    if (gf == f || dis_contain_fact_state(f, &dis_ginitial_state) != -1)
      return 1;
  return 0;
}

float calculate_violation(int *assignment)
{
  int i, j, k, f, g;
  float v = 0;
  char temp[128], temp1[128];
 
  for (i=0;i<dis_gnum_con_conn;i++)
  {
    switch(dis_gcon_conn[i].connective)
    {
    case dis_SOMETIME_BEFORE_c:
    case dis_SOMETIME_AFTER_c:
    for (j=0;j<dis_gcon_conn[i].num1;j++)
    {
      for (k=0;k<dis_gef_conn[dis_gcon_conn[i].cond[j]].num_PC;k++)
      {
        f = dis_gef_conn[dis_gcon_conn[i].cond[j]].PC[k];
        for (g=0;g<gl.n;g++)
          if (is_landmark_element(f, assignment[g]))
            break;
        if (g == gl.n)
            break;
      }
      if (k == dis_gef_conn[dis_gcon_conn[i].cond[j]].num_PC)
        break;
    }
    if (j == dis_gcon_conn[i].num1)
      break;
    for (j=dis_gcon_conn[i].num1;j<dis_gcon_conn[i].num;j++)
    {
      for (k=0;k<dis_gef_conn[dis_gcon_conn[i].cond[j]].num_PC;k++)
      {
        f = dis_gef_conn[dis_gcon_conn[i].cond[j]].PC[k];
        for (g=0;g<gl.n;g++)
          if (is_landmark_element(f, assignment[g]))
              break;
          if (g == gl.n)
            break;
      }
      if (k == dis_gef_conn[dis_gcon_conn[i].cond[j]].num_PC)
        break;
    }
    if (j == dis_gcon_conn[i].num)
      v += dis_gcon_conn[i].weight;
    break;
    case dis_AT_END_c:
    case dis_ALWAYS_c:
    for (j=0;j<dis_gcon_conn[i].num;j++)
    {
      for (k=0;k<dis_gef_conn[dis_gcon_conn[i].cond[j]].num_PC;k++)
      {
        f = dis_gef_conn[dis_gcon_conn[i].cond[j]].PC[k];
        dis_print_ft_name_string(f, temp);
        if (strncmp(temp, "(dis_NOT-", strlen("(dis_NOT-")) == 0)
        {
          for (g=0;g<gl.n;g++)
          {
            dis_print_ft_name_string(assignment[g], temp1);
            if (strcmp(temp+strlen("(dis_NOT-"), temp1+1) == 0)
              break;
          }
          if (g < gl.n)
            break;
        }
        else
        {
          for (g=0;g<gl.n;g++)
            if (assignment[g] != f && group_id(f) == group_id(assignment[g]))
              break;
          if (g < gl.n)
            break;
        }   
      }
      if (k == dis_gef_conn[dis_gcon_conn[i].cond[j]].num_PC)
        break;
    }
    if (j == dis_gcon_conn[i].num)
      v += dis_gcon_conn[i].weight;
    break;
    default:
    break;
    }
  }
/*  printf("opt = %f: ", v);
  for (j=0;j<gl.n;j++)
      dis_print_ft_name(assignment[j]);
  printf("\n");*/
  return v;  
}

int require_enumeration(int g)
{
  int i, j, k;
  
  for (i=0;i<dis_gnum_con_conn;i++)
  {
    // not general
    if (GpG.SearchModal == -1000)
    if (dis_contain_fact_state(dis_gcon_conn[i].fact, &dis_ginitial_state) != -1)
      continue;
    switch(dis_gcon_conn[i].connective)
    {
    case dis_AT_END_c:
    case dis_ALWAYS_c:
    case dis_SOMETIME_BEFORE_c:
    case dis_SOMETIME_AFTER_c:
      for (j=0;j<dis_gcon_conn[i].num;j++)
        for (k=0;k<dis_gef_conn[dis_gcon_conn[i].cond[j]].num_PC;k++)
          if (group_id(dis_gef_conn[dis_gcon_conn[i].cond[j]].PC[k]) == g)
            return 1;
      break;	
    default:
      break;
    }
  }
  return 0;
}

void get_optimal_assignment(int *goal)
{
  int i, j, k, f, g;
  int *flag, *array, a[MAXLEN_OF_GROUPLIST];
  float opt, opt0, v;
  
  gtemplates = (int **) malloc(MAX_TEMPLATES*sizeof(int *));
  array = (int *) calloc(num_transitiveGraph*num_transitiveGraph, sizeof(int));
  for (i=0;i<num_transitiveGraph;i++)
    goal[i] = -1;
  for (i=0;i<saved_dis_gnum_flogic_goal;i++)
    if ((j = group_id(saved_dis_gflogic_goal[i])) >= 0)
    {
      if (goal[j] == -1)
        goal[j] = saved_dis_gflogic_goal[i];
      else
      {
        fprintf(stderr, "Goal can be simplified to FALSE. No plan will solve it\n");
        exit(0);
      }
    }
    else
    {
      printf("Warning: cannot find corresponding state group for fact ");
      dis_print_ft_name(saved_dis_gflogic_goal[i]);
      printf("\n");
    }

  for (i=0;i<dis_gnum_con_conn;i++)
  {
    if (dis_gcon_conn[i].connective != dis_AT_END_c && 
    dis_gcon_conn[i].connective != dis_ALWAYS_c &&
    dis_gcon_conn[i].connective != dis_SOMETIME_AFTER_c &&
    dis_gcon_conn[i].connective != dis_SOMETIME_BEFORE_c)
      continue;
    flag = (int *) calloc(num_transitiveGraph, sizeof(int));
    for (j=0;j<dis_gcon_conn[i].num;j++)
    {
      for (k=0;k<dis_gef_conn[dis_gcon_conn[i].cond[j]].num_PC;k++)
      {
        f = dis_gef_conn[dis_gcon_conn[i].cond[j]].PC[k];
        if ((g = group_id(f)) >= 0)
          flag[g] = 1;
      }
    }
    f = 0;
    for (k=0;k<num_transitiveGraph;k++)
      if (flag[k])
      {
        f = 1;
        for (j=0;j<num_transitiveGraph;j++)
          if (flag[j])
            array[k*num_transitiveGraph+j] = 1;
      }
    free(flag);
    if (f == 0 && dis_gcon_conn[i].connective == dis_AT_END_c || dis_gcon_conn[i].connective == dis_ALWAYS_c)
      saved_dis_gflogic_goal[saved_dis_gnum_flogic_goal++] = dis_gcon_conn[i].fact;
  }

  // transitive closure or connective component?
  for (k=0;k<num_transitiveGraph;k++)
    for (i=0;i<num_transitiveGraph;i++)
      for (j=0;j<num_transitiveGraph;j++)
        if (array[i*num_transitiveGraph+k] == 1)
        if (array[j*num_transitiveGraph+k] == 1)
          array[i*num_transitiveGraph+j] = 1;

  opt = 0;
  for (i=0;i<num_transitiveGraph;i++)
  {
    if (!require_enumeration(i))
      continue;
    f = transitiveGraph[i].element[0];
    if (goal[i] == -1)
    {
      gl.n = 0;
      for (j=0;j<num_transitiveGraph;j++)
        if (array[i*num_transitiveGraph+j])
        {
          a[gl.n] = goal[j];
          gl.i[gl.n++] = j;
        }
      gnum_templates = 0;
      multiply_group_elements(0, a);
      
      k = 0;
      opt0 = calculate_violation(gtemplates[k]);
      for (j=1;j<gnum_templates;j++)
        if ((v = calculate_violation(gtemplates[j])) < opt0)
        {
          k = j;
          opt0 = v;
        }
      for (j=0;j<gl.n;j++)
        goal[gl.i[j]] = gtemplates[k][j];
      for (j=0;j<gnum_templates;j++)
        free(gtemplates[j]);
      opt += opt0;
    }
  }

/*  printf("opt = %f", opt);
  for (j=0;j<num_transitiveGraph;j++)
    if (goal[j] != -1)
      dis_print_ft_name(goal[j]);
  printf("\n");*/
  free(gtemplates);
  free(array);  
}

void mark_unreachable_elements(int iter)
{
  int i, j, k;
  dis_State saved_dis_ginitial_state;
  
  dis_copy_dis_source_to_dest(&saved_dis_ginitial_state, &dis_ginitial_state);
  dis_gnum_flogic_goal = 1; 
  for (i=0;i<num_transitiveGraph;i++)
  {
    transitiveGraph[i].reachable = (int *) malloc(transitiveGraph[i].elementSize*sizeof(int));
    if (transitiveGraph[i].edgeListSize == transitiveGraph[i].elementSize -1 && require_enumeration(i))
    {
      dis_source_to_dest(&dis_ginitial_state, &saved_dis_ginitial_state);
      for (j=0;j<transitiveGraph[i].elementSize;j++)
      {
        transitiveGraph[i].reachable[j] = 0;
        if (dis_contain_fact_state(transitiveGraph[i].element[j], &dis_ginitial_state) != -1)
          k = j;
      }
      transitiveGraph[i].reachable[k] = 1;

      while (1)
      {
        for (j=0;j<transitiveGraph[i].elementSize;j++)
          if (transitiveGraph[i].reachable[j] == 0 && transitiveGraph[i].edge[k*transitiveGraph[i].elementSize+j])
            break;
        if (j == transitiveGraph[i].elementSize)
          break;

        k = j;
        dis_gflogic_goal[0] = transitiveGraph[i].element[k];
        if (dis_get_1P(&dis_ginitial_state) == dis_INFINITY)
          break;
        transitiveGraph[i].reachable[k] = 1;
        bridge_option = 0;
        max_hc_iter = iter;
        dis_mff_bridge();
        if (dis_gnum_plan_ops)
          dis_source_to_dest(&dis_ginitial_state, &dis_mff_sol);
        else
          break;
      }
    }
    else
      for (j=0;j<transitiveGraph[i].elementSize;j++)
      {
        dis_gflogic_goal[0] = transitiveGraph[i].element[j];
        if (dis_get_1P(&dis_ginitial_state) != dis_INFINITY)
          transitiveGraph[i].reachable[j] = 1;
        else
          transitiveGraph[i].reachable[j] = 0;
      }
  }
  dis_source_to_dest(&dis_ginitial_state, &saved_dis_ginitial_state);
}

Bool subgoal_inc_planning(int h, int b)
{
   dis_Bool found_plan = TRUE;
    int j, k;
    
    max_hc_iter = h; 
    max_bfs_iter = b; 
   
    for (j = 0; j < saved_dis_gnum_flogic_goal; j++)
    {
        dis_gflogic_goal[j] = saved_dis_gflogic_goal[j]; 
        dis_gnum_flogic_goal = j+1;
        
        // get known_iga_list
        dis_known_iga_list.num_F = 0;
        for(k=j+1; k<saved_dis_gnum_flogic_goal; k++) {
            dis_known_iga_list.F[dis_known_iga_list.num_F++] 
                = saved_dis_gflogic_goal[k];
        }
   
        bridge_option = 0;
        dis_mff_bridge();
        mff_record_esp_sol();
       
        if(dis_gnum_plan_ops>0) { 
            dis_source_to_dest(&dis_ginitial_state, &dis_mff_sol);
        }
        else
          if (j == saved_dis_gnum_flogic_goal - 1)
            return FALSE;
    }                                   
       
    //printf("\nnumeric part\n\n");
             
    for (j = 0; j < saved_dis_gnum_fnumeric_goal; j++)
    {
         
        dis_gnum_fnumeric_goal = j+1;
        dis_gfnumeric_goal_comp[j] = saved_dis_gfnumeric_goal_comp[j];
        dis_gfnumeric_goal_fl[j] = saved_dis_gfnumeric_goal_fl[j];
        dis_gfnumeric_goal_c[j] = saved_dis_gfnumeric_goal_c[j];
                                    
        dis_known_iga_list.num_F = 0;
        
        dis_mff_bridge();
        mff_record_esp_sol();
   
        if(dis_gnum_plan_ops>0) { 
            dis_source_to_dest(&dis_ginitial_state, &dis_mff_sol);
        }
        else
          if (j == saved_dis_gnum_fnumeric_goal - 1)
            return FALSE;
    }
    return found_plan;
}

int no_part(int b, int h)
{
  int i;
  
  dis_gnum_flogic_goal = dis_gnum_fnumeric_goal = 0;
  for (i=0;i<saved_dis_gnum_flogic_goal;i++)
    dis_gflogic_goal[i] = saved_dis_gflogic_goal[i];
  dis_gnum_flogic_goal = i;
  for (i=0;i<saved_dis_gnum_fnumeric_goal;i++)
  {
    dis_gfnumeric_goal_comp[i] = saved_dis_gfnumeric_goal_comp[i];
    dis_gfnumeric_goal_fl[i] = saved_dis_gfnumeric_goal_fl[i];
    dis_gfnumeric_goal_c[i] = saved_dis_gfnumeric_goal_c[i];
  }
  dis_gnum_fnumeric_goal = i;

  if (b > 0)
  {
    GpG.subsolver = 1;
    bridge_option = 1;
  }
  else
    bridge_option = 0;
  max_bfs_iter = b;
  max_hc_iter = h;
  dis_mff_bridge();
  mff_record_esp_sol();
  if (dis_gnum_plan_ops > 0)
  {
    dis_source_to_dest(&dis_ginitial_state, &dis_mff_sol);
    return 1;
  }
  else
    return 0;
}

//int is_condition(dis_WffNode *, dis_Fact *);

int is_condition( dis_WffNode *n, dis_Fact *f)
{
  dis_WffNode *i;
  int v;

  if ( !n ) {
    return 0;
  }
  
  switch (n->connective) {
  case dis_ALL: 
  case dis_EX:
  case dis_NOT:
    v = is_condition(n->son, f);
    return v;
    break;
  case dis_AND:
  case dis_OR: 
    v = 0;
    for (i=n->sons;i;i=i->next)
      v = v || is_condition(i, f);
    return v;
    break;
  case dis_ATOM:
    if (n->fact->predicate == f->predicate)
      return 1;
    if (n->dis_NOT_p != -1 && strstr(dis_gpredicates[n->fact->predicate], 
    dis_gpredicates[f->predicate]))
      return 1;
    return 0;
    break;
  case dis_TRU:
  case dis_FAL:
  case dis_COMP:
    return 0;
    break;   
  default:
    printf("\n***** ERROR ****");
    printf("\nis_condition: %d > Wrong Node specifier\n", n->connective);
    exit(1);
  }
  return 0;     
} 

extern void print_out_transitive_graph(dis_TransitiveGraph *graph);
int num_bottleneck_var = 0, *bottleneck_var = NULL, *bvar_init = NULL;
void find_bottleneck_variables()
{
  int i, j, k, n;
  
  num_bottleneck_var = 0;
  for ( i = 0; i < dis_gnum_types; i++ ) 
  {
    for (j=0;j<dis_gnum_types;j++)
      if (i != j && dis_gtype_names[j])
        if (dis_is_subtype(j, i))
          break;
    if (j < dis_gnum_types)
      continue;
    for (j=0;j<dis_gnum_operators;j++)
    {
      if (!dis_goperators[j]->name || is_producible_operator[j] || dis_goperators[j]->name[0] == '_')
        continue;
      for (k=0;k<dis_goperators[j]->num_vars;k++)
        if (dis_goperators[j]->var_types[k] == i)
          break;
      if (k == dis_goperators[j]->num_vars)
        break;
    }
    if (j < dis_gnum_operators)
      continue;
      
    for (j=0;j<num_invariantGroup;j++)
    {
      if (!dis_is_subtype(i, invariantGroup[j].focusType))
        continue;
      for (k=0;k<dis_gnum_operators;k++)
        if (dis_goperators[k]->name && !is_producible_operator[k] && dis_goperators[k]->name[0] != '_')
        {
          for (n=0;n<invariantGroup[j].size;n++)
            if (is_condition(dis_goperators[k]->preconds, invariantGroup[j].obj[n]->fact))
              break;
          if (n == invariantGroup[j].size)
            break;
        }
      if (k == dis_gnum_operators)
        break;
    }
    if (j == num_invariantGroup)
      continue;

    if (num_bottleneck_var == 0 || dis_gtype_size[i] < num_bottleneck_var)
    {
      num_bottleneck_var = dis_gtype_size[i]; 
      bottleneck_var = (int *) realloc(bottleneck_var, sizeof(int)*num_bottleneck_var);
      bvar_init = (int *) realloc(bvar_init, sizeof(int)*num_bottleneck_var);
      n = 0;
      for (k=0;k<num_transitiveGraph;k++)
        if (transitiveGraph[k].invariantId == j)
          bottleneck_var[n++] = k;
      if (n != num_bottleneck_var)
      {
//        fprintf(stderr, "Inconsistent number of bottleneck variables %d\n", n);
        num_bottleneck_var = 0;
        xfree(bottleneck_var);
        xfree(bvar_init);
        return;
      }
    }
  } 
 
  for (j=0;j<num_invariantGroup;j++)
  {
    if (invariantGroup[j].focusType != -1)
      continue;
    for (k=0;k<dis_gnum_operators;k++)
      if (dis_goperators[k]->name && !is_producible_operator[k] && dis_goperators[k]->name[0] != '_')
      {
          for (n=0;n<invariantGroup[j].size;n++)
            if (is_condition(dis_goperators[k]->preconds, invariantGroup[j].obj[n]->fact))
              break;
          if (n == invariantGroup[j].size)
            break;
      }
    if (k == dis_gnum_operators)
      break;
  }
  if (j < num_invariantGroup)
  {
    if (num_bottleneck_var == 0 || 1 < num_bottleneck_var)
    {
      num_bottleneck_var = 1; 
      bottleneck_var = (int *) realloc(bottleneck_var, sizeof(int)*num_bottleneck_var);
      bvar_init = (int *) realloc(bvar_init, sizeof(int)*num_bottleneck_var);
      n = 0;
      for (k=0;k<num_transitiveGraph;k++)
        if (transitiveGraph[k].invariantId == j)
          bottleneck_var[n++] = k;
      if (n != num_bottleneck_var)
      {
//        fprintf(stderr, "Inconsistent number of bottleneck variables %d\n", n);
        num_bottleneck_var = 0;
        xfree(bottleneck_var);
        xfree(bvar_init);
        return;
      }
    }
  }
    
/*  printf("%d bottleneck variables\n", num_bottleneck_var);
  for (i=0;i<num_bottleneck_var;i++)
  {
    print_out_transitive_graph(&(transitiveGraph[bottleneck_var[i]]));
    printf("\n");
    fflush(stdout);
  }*/
}

void backup_saved_goals(int *nr_g, int *goal, int *nr_gf, int *goal_f, 
float *goal_c, dis_Comparator *goal_comp)
{
  *nr_g = saved_dis_gnum_flogic_goal;
  memcpy(goal, saved_dis_gflogic_goal, *nr_g*sizeof(int));
  *nr_gf = saved_dis_gnum_fnumeric_goal;
  memcpy(goal_f, saved_dis_gfnumeric_goal_fl, *nr_gf*sizeof(int));
  memcpy(goal_c, saved_dis_gfnumeric_goal_c, *nr_gf*sizeof(float));
  memcpy(goal_comp, saved_dis_gfnumeric_goal_comp, *nr_gf*sizeof(dis_Comparator));
}

int model0()
{
  int i, j, k, g, NP, *goal, granu, granuf;
  int *goal0, nr_g0, *goalf0, nr_gf0, *goal_f, nr_g, nr_gf;
  float *goalc0, *goal_c;
  dis_Comparator *goalcomp0, *goal_comp;

  dis_copy_dis_source_to_dest(&S0, &dis_ginitial_state);
  goal0 = (int *) malloc(sizeof(int)*saved_dis_gnum_flogic_goal);
  goalf0 = (int *) malloc(sizeof(int)*saved_dis_gnum_fnumeric_goal);
  goalc0 = (float *) malloc(sizeof(float)*saved_dis_gnum_fnumeric_goal);
  goalcomp0 = (dis_Comparator *) malloc(sizeof(dis_Comparator)*saved_dis_gnum_fnumeric_goal);
  backup_saved_goals(&nr_g0, goal0, &nr_gf0, goalf0, goalc0, goalcomp0);
  saved_dis_gflogic_goal = (int *) realloc(saved_dis_gflogic_goal, dis_gnum_ft_conn*sizeof(int));
  
  if (GpG.is_durative)
  {
    setup_conds_effects_timespec();
    if (GpG.is_til)
    {
      init_timed_initial();
      num_dur = -1;
    }
  }
  if (GpG.is_preferences || GpG.is_constraints)
  {
    traj_n = 0;
    dis_check_constraints(&dis_ginitial_state);
/*  dis_print_dis_State(dis_ginitial_state);
    printf("\nINITIAL STATE\n");*/
  }
  
//  find_bottleneck_variables();
  if (GpG.is_preferences || GpG.is_constraints)
  {
    goal = (int *) malloc(num_transitiveGraph*sizeof(int));
    mark_unreachable_elements(500);
    get_optimal_assignment(goal);
    for (i=0;i<num_transitiveGraph;i++)
      if (goal[i] != -1)
      {
        for (j=0;j<saved_dis_gnum_flogic_goal;j++)
          if (saved_dis_gflogic_goal[j] == goal[i])
            break;
        if (j == saved_dis_gnum_flogic_goal)
          saved_dis_gflogic_goal[saved_dis_gnum_flogic_goal++] = goal[i];
      }
    if (saved_dis_gnum_flogic_goal + saved_dis_gnum_fnumeric_goal == 0)
    {
      for (i=0;i<dis_gnum_con_conn;i++)
        if (dis_gcon_conn[i].connective == dis_AT_END_c)
        {
          for (j=0;j<dis_gef_conn[dis_gcon_conn[i].cond[0]].num_PC;j++)
          {
            for (g=0;g<saved_dis_gnum_flogic_goal;g++)
              if (saved_dis_gflogic_goal[g] == dis_gef_conn[dis_gcon_conn[i].cond[0]].PC[j])
                break;
            if (g == saved_dis_gnum_flogic_goal)
              saved_dis_gflogic_goal[saved_dis_gnum_flogic_goal++] = 
              dis_gef_conn[dis_gcon_conn[i].cond[0]].PC[j];
          }
        }
    }    
  }

  for (i=0;i<dis_glnf_metric.num_pF;i++)
    if (dis_glnf_metric.pF[i] < 0 ||
    strcmp(dis_grelevant_fluents_name[i], "TOTAL-COST"))
      break;
  GpG.num_esp_sol = 0;
  if (i == dis_glnf_metric.num_pF && GpG.is_preferences && !GpG.is_constraints)
    subgoal_inc_planning(10000, 100000);
  else
  {
    goal = (int *) malloc(sizeof(int)*saved_dis_gnum_flogic_goal);
    goal_f = (int *) malloc(sizeof(int)*saved_dis_gnum_fnumeric_goal);
    goal_c = (float *) malloc(sizeof(float)*saved_dis_gnum_fnumeric_goal);
    goal_comp = (dis_Comparator *) malloc(sizeof(dis_Comparator)*saved_dis_gnum_fnumeric_goal);
    backup_saved_goals(&nr_g, goal, &nr_gf, goal_f, goal_c, goal_comp);

    if (GpG.is_til)
      NP = 1;
    else
    {
    if (num_bottleneck_var > 0 && nr_g + nr_gf > num_bottleneck_var)
    {
      NP = num_bottleneck_var;
      for (j=0;j<NP;j++)
      {
        for (k=0;k<dis_ginitial_state.num_F;k++)
          if (j == group_id(dis_ginitial_state.F[k]))
            break;
        if (k < dis_ginitial_state.num_F)
        {
          bvar_init[j] = dis_ginitial_state.F[k];
          dis_ginitial_state.F[k] = dis_ginitial_state.F[--dis_ginitial_state.num_F];
        }
        else
        {
          fprintf(stderr, "State group is not present in the initial state\n"); 
          exit(1);
        }
      }
    }
    else
      NP = (nr_g + nr_gf + 4)/5;
    }
    granu = nr_g%NP>0 ? nr_g/NP+1 : nr_g/NP;
    granuf = nr_gf%NP>0 ? nr_gf/NP+1 : nr_gf/NP;
      
    g = i = 0;
    saved_dis_gnum_flogic_goal = saved_dis_gnum_fnumeric_goal = 0;
    for (j=0;j<NP;j++)
    {
      if (num_bottleneck_var > 0 && num_bottleneck_var < nr_g + nr_gf && !GpG.is_til)
        dis_ginitial_state.F[dis_ginitial_state.num_F++] = bvar_init[j];

      if (i < nr_g && granu > 0)
        saved_dis_gflogic_goal[saved_dis_gnum_flogic_goal++] = goal[i++];
      for (k=1;k<granu&&i<nr_g;k++)
        saved_dis_gflogic_goal[saved_dis_gnum_flogic_goal++] = goal[i++];

      if (g < nr_gf && granuf > 0) 
      {       
        saved_dis_gfnumeric_goal_comp[saved_dis_gnum_fnumeric_goal] = goal_comp[g];
        saved_dis_gfnumeric_goal_fl[saved_dis_gnum_fnumeric_goal] = goal_f[g];
        saved_dis_gfnumeric_goal_c[saved_dis_gnum_fnumeric_goal++] = goal_c[g++];
      }
      for (k=1;k<granuf&&g<nr_gf;k++)
      {
        saved_dis_gfnumeric_goal_comp[saved_dis_gnum_fnumeric_goal] = goal_comp[g];
        saved_dis_gfnumeric_goal_fl[saved_dis_gnum_fnumeric_goal] = goal_f[g];
        saved_dis_gfnumeric_goal_c[saved_dis_gnum_fnumeric_goal++] = goal_c[g++];
      }
      
      if (GpG.is_til)
        no_part(1000000, 0);
      else
      {
        if (!no_part(0, 10000))
          if (!no_part(10000, 0))
          subgoal_inc_planning(10000, 100000);
      }
      
      if (num_bottleneck_var > 0 && num_bottleneck_var < nr_g + nr_gf && !GpG.is_til)
      {
        for (k=0;k<dis_ginitial_state.num_F;k++)
          if (j == group_id(dis_ginitial_state.F[k]))
            break;
        if (k < dis_ginitial_state.num_F)
        {
          bvar_init[j] = dis_ginitial_state.F[k];
          dis_ginitial_state.F[k] = dis_ginitial_state.F[--dis_ginitial_state.num_F];
        }
      }
    }
  }
  
  if (verify_plan() != 1)
  {
    dis_source_to_dest(&dis_ginitial_state, &S0);
    GpG.num_esp_sol = 0;
    subgoal_inc_planning(10000, 100000);
    if (verify_plan() != 1)
      exit(0);
  }
  if (GpG.is_durative)
    if (!schedule_actions(GpG.num_esp_sol, GpG.esp_solution, GpG.pert_endtime, GpG.duration))
      exit(0);
  if (GpG.is_preferences || GpG.is_constraints)
  {
    traj_n = GpG.num_esp_sol;
    memcpy(traj_plan, GpG.esp_solution, sizeof(int)*traj_n);
    if (GpG.is_durative)
      memcpy(traj_time, GpG.pert_endtime, sizeof(float)*traj_n);
    if (!dis_check_constraints(&dis_mff_sol))
    {
//      dis_print_dis_State(dis_mff_sol);
//      printf("\nINFEASIBLE FINAL STATE\n");
      exit(0);
    }
  }
  
  saved_dis_gnum_flogic_goal = nr_g0;
  memcpy(saved_dis_gflogic_goal, goal0, nr_g0*sizeof(int));
  saved_dis_gnum_fnumeric_goal = nr_gf0;
  memcpy(saved_dis_gfnumeric_goal_fl, goalf0, nr_gf0*sizeof(int));
  memcpy(saved_dis_gfnumeric_goal_c, goalc0, nr_gf0*sizeof(float));
  memcpy(saved_dis_gfnumeric_goal_comp, goalcomp0, nr_gf0*sizeof(dis_Comparator));
//    dis_print_dis_State(dis_mff_sol);
//    printf("\nFINAL STATE\n");
  return 1;
}

void PERT1000(int n, int *op, float *time, float *duration)
{
  int i, j, k, l, ef1, ef2;
  dis_State S1, S;
  dis_Fluent *fl;
  dis_Action *a;
	
  dis_make_state(&S1, dis_gnum_ft_conn, dis_gnum_fl_conn);
  dis_make_state(&S, dis_gnum_ft_conn, dis_gnum_fl_conn);
  dis_source_to_dest(&S, &S0);
  for (i=0;i<n;i++)
  {
    ef1 = dis_gop_conn[op[i]].E[0];
    a = dis_gop_conn[op[i]].action;
    if (dis_gef_conn[ef1].duration > 0)
      duration[i] = dis_gef_conn[ef1].duration;
    else
    {
      k = (int) -dis_gef_conn[ef1].duration;
      for (j=0;j<dis_gnum_fl_conn;j++)
      {
        fl = &dis_grelevant_fluents[j];
        if (fl->function == table[k].fluent.function)
        {
          for (l=0;l<dis_gf_arity[fl->function];l++)
            if (fl->args[l] != a->name_inst_table[table[k].fluent.args[l]])
              break;
          if (l == dis_gf_arity[fl->function])
          {
            duration[i] = S.f_V[j];
            break;
          }
        }
      }
    }
    dis_result_to_dest(&S1, &S, op[i], -1);
    dis_source_to_dest(&S, &S1);
  }

  if (!GpG.is_preferences || GpG.is_constraints)
  {
    time[0] = 0.001;
    for (i=1;i<n;i++)
      time[i] = time[i-1] + duration[i-1] + 0.1;
  }
  else
  {
    for (i=0;i<n;i++)
    {
      ef1 = dis_gop_conn[op[i]].E[0];
      time[i] = 0.001;	
      for (j=0;j<i;j++)
      {
        ef2 = dis_gop_conn[op[j]].E[0];
	  if (dis_depend_efs(ef1, ef2))
            if (time[i] < time[j] + duration[j])
              time[i] = time[j] + duration[j];
      }
    }
    for (i=0;i<n;i++)
      time[i] += 0.1*i;
  }  
	
  free(S.F);
  free(S.f_D);
  free(S.f_V);
  free(S1.F);
  free(S1.f_D);
  free(S1.f_V);
}

void PERT1005(int n, int *op, float *time, float *duration)
{
  int i, j, k, l, ef1, ef2;
  dis_State S1, S;
  dis_Fluent *fl;
  dis_Action *a;
//  double time[5000], duration[5000];
	
  dis_make_state(&S1, dis_gnum_ft_conn, dis_gnum_fl_conn);
  dis_make_state(&S, dis_gnum_ft_conn, dis_gnum_fl_conn);
  dis_source_to_dest(&S, &S0);
  for (k=0;k<dis_gnum_functions;k++)
    if (dis_gis_changed[k] == 0 && dis_gf_arity[k] == 1)
      break;
  
  for (i=0;i<n;i++)
  {
    ef1 = dis_gop_conn[op[i]].E[0];
    a = dis_gop_conn[op[i]].action;
    if (dis_gef_conn[ef1].duration > 0.00001)
      duration[i] = dis_gef_conn[ef1].duration;
    else
    {
      a = dis_gop_conn[op[i]].action;
      for (l=0;l<dis_gnum_fl_conn;l++)
      {
        fl = &(dis_grelevant_fluents[l]);
        if (fl->args[0] == a->name_inst_table[0] && 
        strncmp(dis_grelevant_fluents_name[l], "MINUS", 5))
          break;
      }
      
      for (j=0;j<dis_gnum_initial_function[k];j++)
      {
        fl = &(dis_ginitial_function[k][j].fluent);
        if (fl->args[0] == a->name_inst_table[0])
        {
          duration[i] = (double) (80 - S.f_V[l])/dis_ginitial_function[k][j].value;
          break;
        }
      }
    }
    dis_result_to_dest(&S1, &S, op[i], -1);
    dis_source_to_dest(&S, &S1);
  }

  for (i=0;i<n;i++)
  {
    ef1 = dis_gop_conn[op[i]].E[0];
    time[i] = 0.001;	
    for (j=0;j<i;j++)
    {
      ef2 = dis_gop_conn[op[j]].E[0];
      if (dis_depend_efs(ef1, ef2) || dis_num_depend_efs(ef1, ef2))
        if (time[i] < time[j] + duration[j])
          time[i] = time[j] + duration[j];
    }
  }
  for (i=0;i<n;i++)
    time[i] += 0.02*i;
/*  for (i=0;i<n;i++)
  {
    time0[i] = time[i];
    duration0[i] = duration[i];
  }*/
	
  free(S.F);
  free(S.f_D);
  free(S.f_V);
  free(S1.F);
  free(S1.f_D);
  free(S1.f_V);
}

typedef struct _Event
{
  int op;
  double time;
} Event;
Event *efqueue;
int queue_size;

void add_event(Event *t)
{
  int i;
 
  queue_size++;
  for (i=queue_size-1;i>0;i--)
  {
    if (efqueue[i-1].time <= t->time)
      break;
    efqueue[i] = efqueue[i-1];
  }
  efqueue[i] = *t;
}

int apply_action1004(dis_State *S1, dis_State *S, int op, int opt)
{
  int IN_fl[10];
  int IN_fl_[10];
  float IN_c[10];
  int i, ret = 0, ef = dis_gop_conn[op].E[0];   
  dis_EfConn temp;
  
  if (opt == 0)
  {
    if (dis_gef_conn[ef].duration > 0)
    {
      temp = dis_gef_conn[ef];
      memcpy(IN_fl, temp.IN_fl, temp.num_IN*sizeof(int));
      memcpy(IN_fl_, temp.IN_fl_, temp.num_IN*sizeof(int));
      memcpy(IN_c, temp.IN_c, temp.num_IN*sizeof(float));
      dis_gef_conn[ef].num_IN = 0;
      for (i=0;i<temp.num_IN;i++)
        if (IN_c[i] < 0)
        {
          dis_gef_conn[ef].IN_fl[dis_gef_conn[ef].num_IN] = IN_fl[i];
          dis_gef_conn[ef].IN_fl_[dis_gef_conn[ef].num_IN] = IN_fl_[i];
          dis_gef_conn[ef].IN_c[dis_gef_conn[ef].num_IN++] = IN_c[i];
        }
      
      ret = 1;
      dis_result_to_dest(S1, S, op, -1);
      memcpy(temp.IN_fl, IN_fl, temp.num_IN*sizeof(int));
      memcpy(temp.IN_fl_, IN_fl_, temp.num_IN*sizeof(int));
      memcpy(temp.IN_c, IN_c, temp.num_IN*sizeof(float));
      dis_gef_conn[ef] = temp;
    }
    else
      dis_result_to_dest(S1, S, op, -1);
  }
  else
  {
    temp = dis_gef_conn[ef];
    dis_gef_conn[ef].num_PC = 0;
    dis_gef_conn[ef].num_f_PC = 0;
    memcpy(IN_fl, temp.IN_fl, temp.num_IN*sizeof(int));
    memcpy(IN_fl_, temp.IN_fl_, temp.num_IN*sizeof(int));
    memcpy(IN_c, temp.IN_c, temp.num_IN*sizeof(float));
    
    dis_gef_conn[ef].num_IN = 0;
    for (i=0;i<temp.num_IN;i++)
      if (IN_c[i] > 0)
      {
        dis_gef_conn[ef].IN_fl[dis_gef_conn[ef].num_IN] = IN_fl[i];
        dis_gef_conn[ef].IN_fl_[dis_gef_conn[ef].num_IN] = IN_fl_[i];
        dis_gef_conn[ef].IN_c[dis_gef_conn[ef].num_IN++] = IN_c[i];
      }          
                
    dis_result_to_dest(S1, S, op, -1);
    memcpy(temp.IN_fl, IN_fl, temp.num_IN*sizeof(int));
    memcpy(temp.IN_fl_, IN_fl_, temp.num_IN*sizeof(int));
    memcpy(temp.IN_c, IN_c, temp.num_IN*sizeof(float));
    dis_gef_conn[ef] = temp;
  }
  
  return ret;
}

void PERT1004(int n, int *op, float *start)
{
  int i, j, ef1, ef2;
  
  for (i=0;i<n;i++)
  {
    ef1 = dis_gop_conn[op[i]].E[0];
    start[i] = 0.001;	
    for (j=0;j<i;j++)
    {
      ef2 = dis_gop_conn[op[j]].E[0];
        if (dis_depend_efs(ef1, ef2) || dis_num_depend_efs(ef1, ef2))
          if (start[i] < start[j] + dis_gef_conn[op[j]].duration)
            start[i] = start[j] + dis_gef_conn[op[j]].duration;
    }
  }
  for (i=0;i<n;i++)
    start[i] += 0.003*i;
/*  for (i=0;i<n;i++)
  {
    start[i] = t;
    t += (dis_gef_conn[op[i]].duration + 0.002);
  }*/
}

void xPERT1004(int n, int *op, float *start)
{
  Event temp;
  dis_State S, S1;
  float dummy;
  int i, j, k, a;
  int *done = (int *) calloc(n, sizeof(int));

  dis_make_state(&S, dis_gnum_ft_conn, dis_gnum_fl_conn);  
  dis_make_state(&S1, dis_gnum_ft_conn, dis_gnum_fl_conn);
  dis_source_to_dest(&S, &S0);
  efqueue = (Event *) malloc(2*n*sizeof(Event));
  for (i=0;i<n;i++)
    start[i] = 100000;
  
  k = 0;
  dummy = 0;
  for (i=0;;i++)
  {
    a = 0;
    for (j=0;j<n;j++)
    {
      if (done[j])
        continue;
      if (is_applicable(&S, dis_gop_conn[op[j]].E[0]))
      {
        if (apply_action1004(&S1, &S, op[j], 0))
        {
          temp.op = op[j];
          temp.time = dummy + dis_gef_conn[dis_gop_conn[op[j]].E[0]].duration;
          add_event(&temp);
        }
        dis_source_to_dest(&S, &S1);
        start[j] = dummy + 0.002*k;
        k++;
        a = 1;
        done[j] = 1;
      }
    }
    if (i < queue_size)
    {
      dummy = efqueue[i].time;
      apply_action1004(&S1, &S, efqueue[i].op, 1);
      dis_source_to_dest(&S, &S1);
      a = 1;
    }
    if (a == 0)
      break;
  }
  
  free(S.F);
  free(S.f_D);
  free(S.f_V);
  free(S1.F);
  free(S1.f_D);
  free(S1.f_V);
  free(efqueue);
  free(done);
}

void construct_traj()
{
  int i, j, k;
  
  j = 0;
  if (GpG.SearchModal == -1000)
    PERT1000(GpG.num_esp_sol, GpG.esp_solution, GpG.pert_endtime, GpG.duration);
  else
    if (GpG.SearchModal == -1004)
      PERT1004(GpG.num_esp_sol, GpG.esp_solution, GpG.pert_endtime);
    else
      myPERT(GpG.num_esp_sol, 0, GpG.esp_solution, GpG.pert_endtime);
  for (i=0;i<GpG.num_esp_sol;i++)
  {
    k = 0;
    for (j=0;j<GpG.num_esp_sol;j++)
      if (GpG.pert_endtime[j] < GpG.pert_endtime[k])
        k = j;
    traj_plan[i] = GpG.esp_solution[k];
    traj_time[i] = GpG.pert_endtime[k];
    GpG.pert_endtime[k] = 100000000;
  }
  traj_n = GpG.num_esp_sol;
  memcpy(GpG.esp_solution, traj_plan, traj_n*sizeof(int));
}

/*
int solve1000mt()
{
  int i, k, g, nr_g, granu, at[10], nr_truck = 0;
  int goal_fl[100];
  dis_Comparator goal_comp[100];
  float goal_c[100];
  int solution[1000], sol_len;
  dis_Fact *f;
  dis_State S;

  dis_make_state(&S, dis_gnum_ft_conn, dis_gnum_fl_conn);
  nr_g = saved_dis_gnum_fnumeric_goal;
  for (i=nr_g-1;i>=0;i--)
  {
    goal_fl[i] = saved_dis_gfnumeric_goal_fl[i];
    goal_c[i] = saved_dis_gfnumeric_goal_c[i];
    goal_comp[i] = saved_dis_gfnumeric_goal_comp[i];
  }
  GpG.num_esp_sol = 0;
  dis_known_iga_list.num_F = 0;
  dis_red_space = 1;

  for (i=0;i<dis_ginitial_state.num_F;i++)
  {
    f = &(dis_grelevant_facts[dis_ginitial_state.F[i]]);
    if (dis_gpredicates[f->predicate][0] == 'A' &&
    dis_gpredicates[f->predicate][1] == 'T')
    {
      nr_truck++;
      at[atoi(&dis_gconstants[f->args[0]][5])-1] = dis_ginitial_state.F[i];
      dis_ginitial_state.F[i] = dis_ginitial_state.F[--(dis_ginitial_state.num_F)];
      i--;
    }
  }

  k = 0;
  granu = nr_g%nr_truck>0 ? nr_g/nr_truck+1 : nr_g/nr_truck;
  for (g=0;g<nr_g;)
  {
    dis_ginitial_state.F[dis_ginitial_state.num_F++] = at[k];
    dis_source_to_dest(&S, &dis_ginitial_state);
    saved_dis_gfnumeric_goal_comp[0] = goal_comp[g];
    saved_dis_gfnumeric_goal_fl[0] = goal_fl[g];
    saved_dis_gfnumeric_goal_c[0] = goal_c[g++];
    saved_dis_gnum_fnumeric_goal = 1;
    for (i=1;i<granu&&g<nr_g;i++)
    {
      saved_dis_gfnumeric_goal_comp[saved_dis_gnum_fnumeric_goal] = goal_comp[g];
      saved_dis_gfnumeric_goal_fl[saved_dis_gnum_fnumeric_goal] = goal_fl[g];
      saved_dis_gfnumeric_goal_c[saved_dis_gnum_fnumeric_goal++] = goal_c[g++];
    }

    if (!no_part(0, 10000))
    {
      sol_len = GpG.num_esp_sol;
      memcpy(solution, GpG.esp_solution, sol_len*sizeof(int));
      GpG.num_esp_sol = 0;
      saved_dis_gnum_flogic_goal = 0;
      saved_dis_gnum_fnumeric_goal = 0;
      for (i=0;i<nr_g;i++)
      {
        saved_dis_gfnumeric_goal_comp[saved_dis_gnum_fnumeric_goal] = goal_comp[i];
        saved_dis_gfnumeric_goal_fl[saved_dis_gnum_fnumeric_goal] = goal_fl[i];
        saved_dis_gfnumeric_goal_c[saved_dis_gnum_fnumeric_goal++] = goal_c[i];
      }

      if (!subgoal_inc_planning(10000, 0))
        exit(1);
      memcpy(solution+sol_len, GpG.esp_solution, GpG.num_esp_sol*sizeof(int)); 
      sol_len += GpG.num_esp_sol;
      memcpy(GpG.esp_solution, solution, sol_len*sizeof(int));
      GpG.num_esp_sol = sol_len;
      break;
    }

    for (i=0;i<dis_ginitial_state.num_F;i++)
      if (group_id(dis_ginitial_state.F[i]) == group_id(at[k]))
      {
        f = &(dis_grelevant_facts[dis_ginitial_state.F[i]]);
        if (dis_gpredicates[f->predicate][0] == 'A' &&
        dis_gpredicates[f->predicate][1] == 'T')
        {
          at[k] = dis_ginitial_state.F[i];
          dis_ginitial_state.F[i] = dis_ginitial_state.F[--dis_ginitial_state.num_F];
          break;
        }
      }
    k++;
  }

  return 1;
}*/

int model1000()
{

    if (!GpG.is_durative && !GpG.is_fluents)
//      solve1000pro();
      psearch();
    else
      if (!GpG.is_durative)
//        solve1000metric();
        psearch();
      else
      {
        dis_copy_dis_source_to_dest(&S0, &dis_ginitial_state);
//        solve1000mt();
        model0();
      }

  return 1;
}

// for openstacks
typedef struct _Global1001
{
  int Nproduct, Norder;
  char included[100][100];
  int Nsolution, h, Mh;
  int seq[100], upperbound;
} Global1001;
Global1001 g1001;

typedef struct _Global1001Time
{
  float pt[50], ot[50];
} Global1001Time;
Global1001Time g1001t;

int make_product(int p, char *made, int *orderstatus, int *m, int n)
{
  int i;
  
  if (!made[p])
  {
    for (i=0;i<g1001.Norder;i++)
      if (g1001.included[i][p] && orderstatus[i] == -1)
      {
        if (++n > *m)
          *m = n;
        orderstatus[i] = 0;
      }
    made[p] = 1;
  }
  return n;
}

int ship_order(int o, char *made, int *orderstatus, int *m, int n)
{
  int j;
  
  if (orderstatus[o] != 1)
  {
    for (j=0;j<g1001.Nproduct;j++)
      if (g1001.included[o][j] && !made[j])
        n = make_product(j, made, orderstatus, m, n);
    orderstatus[o] = 1;
    return n-1;
  }
  return n;
}

int ship_two_order_h(int o1, char *made, int *orderstatus, int *m, int n)
{
  int i, status0[100], status1[100], n0, m0, n1, m1;
  char made0[100], made1[100];
  
  memcpy(status0, orderstatus, sizeof(int)*g1001.Norder);  
  memcpy(made0, made, g1001.Nproduct);  
  n = ship_order(o1, made0, status0, m, n);
  m1 = n1 = g1001.Norder;
  
  for (i=0;i<g1001.Norder;i++)
    if (orderstatus[i] != 1)
    {
      memcpy(status1, status0, sizeof(int)*g1001.Norder);  
      memcpy(made1, made0, g1001.Nproduct);
      m0 = *m;
      n0 = ship_order(i, made1, status1, &m0, n);
      if (m0 <= m1)
        if (n0 < n1)
        {
          m1 = m0;
          n1 = n0;
        }
    }

  *m = m1;  
  g1001.h++;
  return n1;
}

void constructg1001()
{
  int i, j, k, n;
  dis_Fact *f;
  
  for (i=0;i<dis_gnum_types;i++)
  {
    if (strlen(dis_gtype_names[i]) == 5 && strncmp(dis_gtype_names[i], "OR", 2) == 0)
      g1001.Norder = dis_gtype_size[i];
    if (strlen(dis_gtype_names[i]) == 7 && dis_gtype_names[i][0] == 'P' && dis_gtype_names[i][2] == 'O')
      g1001.Nproduct = dis_gtype_size[i];
  }
  for (i=0;i<g1001.Norder;i++)
    for (j=0;j<g1001.Nproduct;j++)
      g1001.included[i][j] = 0;
  
  for (n=0;n<dis_gnum_predicates;n++)
    if (dis_gpredicates[n][0] == 'I' && strlen(dis_gpredicates[n]) == 8)
      break;
  for (k=0;k<dis_gnum_initial_predicate[n];k++)
  {
    f = &(dis_ginitial_predicate[n][k]);
    i = atoi(&dis_gconstants[f->args[0]][1]);      
    j = atoi(&dis_gconstants[f->args[1]][1]);      
    g1001.included[i-1][j-1] = 1;
  }

  for (n=0;n<dis_gnum_predicates;n++)
    if (dis_gpredicates[n][0] == 'S' && strlen(dis_gpredicates[n]) == 12)
      break;
  if (n < dis_gnum_predicates)
  {
    f = &(dis_ginitial_predicate[n][0]);
    g1001.upperbound = atoi(&dis_gconstants[f->args[0]][1]);
    if (g1001.upperbound == 0)
      g1001.upperbound = g1001.Norder;
    g1001.upperbound++;
  }
  else
    g1001.upperbound = -1;
}

void solve1001pro(int c, int *seq, char *made0, int *status0, int m0, int n0)
{
  int i, j, m, n;
  int hv[100], minh;
  int status[100]; 
  char made[100];

  if (c == g1001.Norder)
  {
    g1001.Nsolution++;
    g1001.upperbound = m0;
    memcpy(g1001.seq, seq, sizeof(int)*g1001.Norder);
  }
  else
  {
    if (g1001.h >= g1001.Mh)
      return;
    for (i=0;i<g1001.Norder;i++)
      if (status0[i] != 1)
      {
        hv[i] = m0;
        ship_two_order_h(i, made0, status0, &hv[i], n0);
      }
      else
        hv[i] = 1000;
        
    while (1)
    {
      j = -1;
      minh = g1001.upperbound;
      for (i=0;i<g1001.Norder;i++)
        if (hv[i] < minh)
        {
          j = i;
          minh = hv[j];
        }
      if (j == -1)
        return;
      
      memcpy(made, made0, g1001.Nproduct);
      memcpy(status, status0, g1001.Norder*sizeof(int));
      m = m0;
      n = n0;
      n = ship_order(j, made, status, &m, n);
      seq[c] = j;
      hv[j] = 1000;
      solve1001pro(c+1, seq, made, status, m, n);
    }
  }
}

void solve1001pro_wrapper()
{
  int i, orderstatus[100], seq[100];
  char made[100];

  g1001.Nsolution = 0;
  g1001.h = 0;
  g1001.Mh = 100000;
  for (i=0;i<g1001.Norder;i++)
    orderstatus[i] = -1;
  for (i=0;i<g1001.Nproduct;i++)
    made[i] = 0;

  solve1001pro(0, seq, made, orderstatus, 0, 0);
  memcpy(dis_gflogic_goal, saved_dis_gflogic_goal, sizeof(int)*saved_dis_gnum_flogic_goal);
  for (i=0;i<g1001.Norder;i++)
    saved_dis_gflogic_goal[i] = dis_gflogic_goal[g1001.Norder-1-g1001.seq[i]];
  subgoal_inc_planning(10000, 100000);
}

float calculate_violation_not(int g)
{
  int i, j, k;
  float v = 0;
  
  for (i=0;i<dis_gnum_con_conn;i++)
    switch(dis_gcon_conn[i].connective)
    {
    case dis_AT_END_c:
    case dis_ALWAYS_c:
    for (j=0;j<dis_gcon_conn[i].num;j++)
    {
      for (k=0;k<dis_gef_conn[dis_gcon_conn[i].cond[j]].num_PC;k++)
        if (g == dis_gef_conn[dis_gcon_conn[i].cond[j]].PC[k])
          break;
      if (k == dis_gef_conn[dis_gcon_conn[i].cond[j]].num_PC)
        break;
    }
    if (j == dis_gcon_conn[i].num)
      v += dis_gcon_conn[i].weight;
    break;
    default:
    break;
    }
  return v;  
}

void METIS_PartGraphRecursive(int *, int *, int *, int *, int *, int *, int *, int *, int *, int *, int *); 
void METIS_WPartGraphRecursive(int *, int *, int *, int *, int *, int *, int *, int *, float *, int *, int *, int *); 
int g1001weight[100][100];

int graph_partitioning(int n, int *order, int *product)
{
  int i, j;
  int Nnode, options1[1] = {1}, options[1] = {0}, mincut, Nedge;
  int xadj[201], *adjncy, *adjwgt, part[200];
  
  Nnode = g1001.Norder + g1001.Nproduct;
  adjncy = (int *) malloc(Nnode*Nnode*sizeof(int));
  adjwgt = (int *) malloc(Nnode*Nnode*sizeof(int));
  Nedge = 0;
  for (i=0;i<g1001.Norder;i++)
  {
    xadj[i] = Nedge;
    for (j=0;j<g1001.Nproduct;j++)
      if (g1001.included[i][j])
      {
        adjwgt[Nedge] = g1001weight[i][j];
        adjncy[Nedge++] = j + g1001.Norder;
      }
  }
  for (j=0;j<g1001.Nproduct;j++)
  {
    xadj[j+g1001.Norder] = Nedge;
    for (i=0;i<g1001.Norder;i++)
      if (g1001.included[i][j])
      {
        adjwgt[Nedge] = g1001weight[i][j];
        adjncy[Nedge++] = i;
      }
  }
  xadj[Nnode] = Nedge;
  adjncy = (int *) realloc(adjncy, Nedge*sizeof(int));
  adjwgt = (int *) realloc(adjwgt, Nedge*sizeof(int));

  if (n < g1001.Norder)
  {
    METIS_PartGraphRecursive(&Nnode, xadj, adjncy, NULL, adjwgt, options1, options, &n, options, &mincut, part);
    for (i=0;i<g1001.Norder;i++)
      order[i] = part[i];
    for (j=0;j<g1001.Nproduct;j++)
      product[j] = part[j+g1001.Norder];
  }
  else
  {
    for (i=0;i<g1001.Norder;i++)
      order[i] = i;
    for (j=0;j<g1001.Nproduct;j++)
      for (i=0;i<g1001.Norder;i++)
        if (g1001.included[i][j])
        {
          product[j] = i;
          break;
        }
  }

  free(adjncy);
  free(adjwgt);
  return mincut;
}

int is_allorders_started(int g, int *opart, int *status)
{
  int i;
  
  for (i=0;i<g1001.Norder;i++)
    if (opart[i] == g && status[i] != 2)
      return 0;
  return 1;
}

int solve1001sp(int n, int *solution)
{
  int i, j, k, p, mincut = 0;
  int opart[100], ppart[100], status[100], seq[100], sol[300];
  char made[100];
  
  g1001.Nsolution = 0;
  g1001.h = 0;
  g1001.Mh = 100000;
  for (i=0;i<g1001.Norder;i++)
    status[i] = -1;
  for (i=0;i<g1001.Nproduct;i++)
    made[i] = 0;
  solve1001pro(0, seq, made, status, g1001.upperbound-1, 0);

  if (g1001.Nsolution == 0)
  {
    mincut = graph_partitioning(2*n, opart, ppart);
    for (i=0;i<g1001.Norder;i++)
      for (j=0;j<g1001.Nproduct;j++)
        if (g1001.included[i][j] && opart[i] != ppart[j])
          g1001.included[i][j] = 0;
    mincut += solve1001sp(2*n, solution);

    if (solution[0] == -1)
    {
    for (i=0;i<g1001.Norder;i++)
      status[i] = -1;
    for (i=0;i<g1001.Nproduct;i++)
      made[i] = 0;
    
    p = 0;
    for (i=0;i<g1001.Norder;i++)
    {
      for (j=0;j<g1001.Nproduct;j++)
        if (g1001.included[g1001.seq[i]][j] && !made[j])
        {
          for (k=0;k<g1001.Norder;k++)
            if (g1001.included[k][j] && status[k] == -1)
            {
              sol[p++] = k + 200;
              status[k] = 0;
            }
          sol[p++] = j + 100;
          made[j] = 1;
        }
      sol[p++] = g1001.seq[i];
      status[g1001.seq[i]] = 1;
    }
    sol[p] = -1;
    
    k = 0;
    for (i=0;i<p;i++)
      if (sol[i] >= 0)
      {
        if (sol[i] >= 100)
        {
          if (sol[i] >= 200)
            status[sol[i] - 200] = 2;
          solution[k++] = sol[i];
          sol[i] = -1;
        }
        else
        {
          solution[k++] = sol[i];
          sol[i] = -1;
          if (is_allorders_started(opart[solution[k-1]], opart, status))
          {
            for (j=i+1;j<p;j++)
              if (sol[j] >= 200)
                break;
            if (j < p)
            {
              solution[k++] = sol[j];
              status[sol[j] - 200] = 2;
              sol[j] = -1;
              for (j++;sol[j]==-1||(sol[j]>=100&&sol[j]<200);j++)
                if (sol[j] != -1)
                {
                  solution[k++] = sol[j];
                  sol[j] = -1;
                }
            }
          }
        }
      }
      solution[k] = -1;
    }
  }
  else if (n == 1)
  {
    for (i=0;i<g1001.Norder;i++)
      status[i] = -1;
    for (i=0;i<g1001.Nproduct;i++)
      made[i] = 0;

    p = 0;
    for (i=0;i<g1001.Norder;i++)
    {
      for (j=0;j<g1001.Nproduct;j++)
        if (g1001.included[g1001.seq[i]][j] && !made[j])
        {
          for (k=0;k<g1001.Norder;k++)
            if (g1001.included[k][j] && status[k] == -1)
            {
              solution[p++] = k + 200;
              status[k] = 0;
            }
          solution[p++] = j + 100;
          made[j] = 1;
        }
      solution[p++] = g1001.seq[i];
      status[g1001.seq[i]] = 1;
    }
    solution[p] = -1;
  }
  return mincut;
}

void plan1001(int p, int *solution)
{
  int i, j;
  dis_Fact *f;

  dis_gnum_flogic_goal = 1;
  GpG.num_esp_sol = 0;
  for (i=0;i<p;i++)
  {
    if (solution[i] == -1)
      break;
    if (solution[i] < 100)
    {
      for (j=0;j<dis_gnum_ft_conn;j++)
      {
        f = &(dis_grelevant_facts[j]);
        if (strlen(dis_gpredicates[f->predicate]) == 7 && 
        dis_gpredicates[f->predicate][1] == 'H' &&
        dis_gpredicates[f->predicate][3] == 'P')
          if (atoi(&dis_gconstants[f->args[0]][1])-1 == solution[i])
            break;
      }
    }
    else
    if (solution[i] < 200)
    {
      for (j=0;j<dis_gnum_ft_conn;j++)
      {
        f = &(dis_grelevant_facts[j]);
        if (strlen(dis_gpredicates[f->predicate]) == 4 && 
        dis_gpredicates[f->predicate][1] == 'A' &&
        dis_gpredicates[f->predicate][3] == 'E')
          if (atoi(&dis_gconstants[f->args[0]][1])-1 == solution[i]-100)
            break;
      }
    }
    else
    {
      for (j=0;j<dis_gnum_ft_conn;j++)
      {
        f = &(dis_grelevant_facts[j]);
        if (strlen(dis_gpredicates[f->predicate]) == 7 && 
        dis_gpredicates[f->predicate][1] == 'T' &&
        dis_gpredicates[f->predicate][3] == 'R')
          if (atoi(&dis_gconstants[f->args[0]][1])-1 == solution[i] - 200)
            break;
      }
    }
    
    dis_gflogic_goal[0] = j;
    dis_mff_bridge();
    mff_record_esp_sol();
    if (dis_gnum_plan_ops > 0)
      dis_source_to_dest(&dis_ginitial_state, &dis_mff_sol);
  }
}

int solve1001sp_wrapper()
{
  int i, solution[300], cost, n;
  int j, k, p, status[100], sol[300];
  char made[100];
  float mv1, mv2;
  Global1001 saved_g1001 = g1001;
  dis_State saved_dis_ginitial_state, dis_mff_sol1;
  
  dis_copy_dis_source_to_dest(&saved_dis_ginitial_state, &dis_ginitial_state);
  solution[0] = -1;
  cost = solve1001sp(1, solution);
  bridge_option = 0;
  plan1001(g1001.Norder*2 + g1001.Nproduct, solution);

  traj_n = GpG.num_esp_sol;
  memcpy(traj_plan, GpG.esp_solution, traj_n*sizeof(int));
  dis_check_constraints(&dis_mff_sol);
  mv1 = dis_metric_value(&dis_mff_sol);
  memcpy(solution, GpG.esp_solution, GpG.num_esp_sol*sizeof(int));
  dis_copy_dis_source_to_dest(&dis_mff_sol1, &dis_mff_sol);
  n = GpG.num_esp_sol;

  for (i=0;i<g1001.Norder;i++)
    status[i] = -1;
  for (i=0;i<g1001.Nproduct;i++)
    made[i] = 0;
    
  p = 0;
  for (i=0;i<g1001.Norder;i++)
  {
    for (j=0;j<g1001.Nproduct;j++)
      if (saved_g1001.included[g1001.seq[i]][j] && !made[j])
      {
        for (k=0;k<g1001.Norder;k++)
          if (saved_g1001.included[k][j] && status[k] == -1)
          {
            sol[p++] = k + 200;
            status[k] = 0;
          }
        sol[p++] = j + 100;
        made[j] = 1;
      }
    sol[p++] = g1001.seq[i];
    status[g1001.seq[i]] = 1;
  }
  sol[p] = -1;
  
  dis_source_to_dest(&dis_ginitial_state, &saved_dis_ginitial_state);
  bridge_option = 0;
  plan1001(p, sol);
  traj_n = GpG.num_esp_sol;
  memcpy(traj_plan, GpG.esp_solution, traj_n*sizeof(int));
  dis_check_constraints(&dis_mff_sol);
  mv2 = dis_metric_value(&dis_mff_sol);

  if (mv1 < mv2)
  {
    GpG.num_esp_sol = n;
    memcpy(GpG.esp_solution, solution, n*sizeof(int));
    dis_source_to_dest(&dis_mff_sol, &dis_mff_sol1);
  }
  g1001 = saved_g1001;
//  fprintf(stderr, "\ncost = %d %f\n", cost, dis_metric_value(&dis_mff_sol));
  return cost;
}

void constructg1001t()
{
  int i, k, n;

  for (n=0;n<dis_gnum_functions;n++)
    if (dis_gfunctions[n][0] == 'M' && strlen(dis_gfunctions[n]) == 9)
      break;
  for (k=0;k<dis_gnum_initial_function[n];k++)
  {
    i = atoi(&dis_gconstants[dis_ginitial_function[n][k].fluent.args[0]][1])-1;
    g1001t.pt[i] = dis_ginitial_function[n][k].value;
  }

  for (i=0;i<g1001.Norder;i++)
  {
    g1001t.ot[i] = 0;
    for (k=0;k<g1001.Nproduct;k++)
      if (g1001.included[i][k])
        if (g1001t.ot[i] < g1001t.pt[k])
          g1001t.ot[i] = g1001t.pt[k];
    g1001t.ot[i] += 2;
  }
}

int solve1001t()
{
  int i, m;
  int orderstatus[100], seq[100];
  char made[100];
  dis_State saved_dis_ginitial_state;

  dis_copy_dis_source_to_dest(&saved_dis_ginitial_state, &dis_ginitial_state);
//  constructg1001t();
  g1001.Nsolution = 0;
  g1001.h = 0;
  g1001.Mh = 100000;
  for (i=0;i<g1001.Norder;i++)
    orderstatus[i] = -1;
  for (i=0;i<g1001.Nproduct;i++)
    made[i] = 0;

  m = g1001.upperbound - 1;        
  dis_red_space = 1;
  dis_gcmd_line.optimize = 1;
  solve1001pro(0, seq, made, orderstatus, m, 0);
  if (g1001.Nsolution == 0)
    return 0;
  memcpy(dis_gflogic_goal, saved_dis_gflogic_goal, sizeof(int)*saved_dis_gnum_flogic_goal);
  for (i=0;i<g1001.Norder;i++)
    saved_dis_gflogic_goal[i] = dis_gflogic_goal[g1001.Norder-1-g1001.seq[i]];
  GpG.num_esp_sol = 0;
  subgoal_inc_planning(0, 100000);
    
  m = 0;
  for (i=0;i<GpG.num_esp_sol;i++)
    if (dis_gop_conn[GpG.esp_solution[i]].action->name[0] == 'M' &&
    strlen(dis_gop_conn[GpG.esp_solution[i]].action->name) == 12)
      seq[m++] = dis_gef_conn[dis_gop_conn[GpG.esp_solution[i]].E[0]].A[0];
    
  dis_source_to_dest(&dis_ginitial_state, &saved_dis_ginitial_state);
  dis_gnum_flogic_goal = 1;
  bridge_option = 1;
  GpG.num_esp_sol = 0;

  for (i=0;i<m;i++)
  {
    dis_gflogic_goal[0] = seq[i];
    dis_mff_bridge();
    mff_record_esp_sol();
    if (dis_gnum_plan_ops > 0)
      dis_source_to_dest(&dis_ginitial_state, &dis_mff_sol);
  }
  subgoal_inc_planning(0, 100000);

  free(saved_dis_ginitial_state.F);
  free(saved_dis_ginitial_state.f_D);
  free(saved_dis_ginitial_state.f_V);
  return 1;
}

void plan1001mt(int p, int *solution)
{
  int i, j, k = 0, m = 0;
  dis_Action *a;

  dis_gnum_flogic_goal = 1;
  GpG.num_esp_sol = 0;
  for (i=0;i<p;i++)
  {
    if (solution[i] == -1)
      break;
    if (solution[i] < 100)
    {
      for (j=0;j<dis_gnum_op_conn;j++)
      {
        a = dis_gop_conn[j].action;
        if (strlen(a->name) == 10)
          if (atoi(&dis_gconstants[a->name_inst_table[0]][1])-1 == solution[i])
            break;
      }
      k--;
    }
    else
    if (solution[i] < 200)
    {
      for (j=0;j<dis_gnum_op_conn;j++)
      {
        a = dis_gop_conn[j].action;
        if (strlen(a->name) == 12)
          if (atoi(&dis_gconstants[a->name_inst_table[0]][1])-1 == solution[i]-100)
            break;
      }
    }
    else
    {
      for (j=0;j<dis_gnum_ft_conn;j++)
      {
        a = dis_gop_conn[j].action;
        if (strlen(a->name) == 13)
          if ((k < m && a->name[12] == '1') || (k >= m && a->name[12] == '2'))
            if (atoi(&dis_gconstants[a->name_inst_table[0]][1])-1 == solution[i] - 200)
              break;
      }
      k++;
      if (m < k)
        m = k;
    }
    
    GpG.esp_solution[GpG.num_esp_sol++] = j;
    dis_result_to_dest(&dis_mff_sol, &dis_ginitial_state, j, -1);
    dis_source_to_dest(&dis_ginitial_state, &dis_mff_sol);
  }
}
void solve1001mt()
{
  int i, j, k, mink, index, m;
  int orderstatus[100], seq[100];
  char made[100];
  int optimal_plan_length, optimal_plan[1000], p, solution[1000];
  float min_metric_value = 100000000, makespan;
  dis_State saved_dis_ginitial_state, min_final_state;
  
  dis_make_state(&min_final_state, dis_gnum_ft_conn, dis_gnum_fl_conn);
  dis_copy_dis_source_to_dest(&saved_dis_ginitial_state, &dis_ginitial_state);
  dis_red_space = 1;
  dis_gcmd_line.optimize = 1;

  for (k=1;k<=g1001.Norder;k++)
  {
    dis_source_to_dest(&dis_ginitial_state, &saved_dis_ginitial_state);
    g1001.upperbound = k + 1;
    g1001.Nsolution = 0;
    g1001.h = 0;
    g1001.Mh = 100000;
    for (i=0;i<g1001.Norder;i++)
      orderstatus[i] = -1;
    for (i=0;i<g1001.Nproduct;i++)
      made[i] = 0;

    solve1001pro(0, seq, made, orderstatus, k, 0);
    if (g1001.Nsolution == 0)
      continue;

    for (i=0;i<g1001.Norder;i++)
      orderstatus[i] = -1;
    for (i=0;i<g1001.Nproduct;i++)
      made[i] = 0;

    p = 0;
    for (i=0;i<g1001.Norder;i++)
    {
      for (j=0;j<g1001.Nproduct;j++)
        if (g1001.included[g1001.seq[i]][j] && !made[j])
        {
          for (m=0;m<g1001.Norder;m++)
            if (g1001.included[m][j] && orderstatus[m] == -1)
            {
              solution[p++] = m + 200;
              orderstatus[m] = 0;
            }
          solution[p++] = j + 100;
          made[j] = 1;
        }
      solution[p++] = g1001.seq[i];
      orderstatus[g1001.seq[i]] = 1;
    }
    solution[p] = -1;

    plan1001mt(p, solution);
/*    memcpy(dis_gflogic_goal, saved_dis_gflogic_goal, sizeof(int)*saved_dis_gnum_flogic_goal);
    for (i=0;i<g1001.Norder;i++)
      saved_dis_gflogic_goal[i] = dis_gflogic_goal[g1001.Norder-1-g1001.seq[i]];
    GpG.num_esp_sol = 0;
    subgoal_inc_planning(0, 100000);

    m = 0;
    for (i=0;i<GpG.num_esp_sol;i++)
      if (dis_gop_conn[GpG.esp_solution[i]].action->name[0] == 'M' &&
      strlen(dis_gop_conn[GpG.esp_solution[i]].action->name) == 12)
        seq[m++] = dis_gef_conn[dis_gop_conn[GpG.esp_solution[i]].E[0]].A[0];
    
    dis_source_to_dest(&dis_ginitial_state, &saved_dis_ginitial_state);
    dis_gnum_flogic_goal = 1;
    bridge_option = 1;
    GpG.num_esp_sol = 0;

    for (i=0;i<m;i++)
    {
      dis_gflogic_goal[0] = seq[i];
      dis_mff_bridge();
      mff_record_esp_sol();
      my_printplan(dis_gnum_plan_ops, dis_gplan_ops, NULL);
      if (dis_gnum_plan_ops > 0)
        dis_source_to_dest(&dis_ginitial_state, &dis_mff_sol);
    }
    subgoal_inc_planning(0, 100000);*/
    
    PERT1004(GpG.num_esp_sol, GpG.esp_solution, GpG.pert_endtime);
    makespan = 0;
    for (i=0;i<GpG.num_esp_sol;i++)
    {
      index = dis_gop_conn[GpG.esp_solution[i]].E[0];
      if (dis_gef_conn[index].duration + GpG.pert_endtime[i] > makespan)
        makespan = dis_gef_conn[index].duration + GpG.pert_endtime[i];
    }

//    fprintf(stderr, "%d %f %f\n", k, makespan, dis_metric_value(&dis_mff_sol) + dis_gtt*makespan);
    if (min_metric_value > dis_metric_value(&dis_mff_sol) + dis_gtt*makespan)
    {
      mink = k;
      dis_source_to_dest(&min_final_state, &dis_mff_sol);
      min_metric_value = dis_metric_value(&dis_mff_sol) + dis_gtt*makespan;
      memcpy(optimal_plan, GpG.esp_solution, GpG.num_esp_sol*sizeof(int));
      optimal_plan_length = GpG.num_esp_sol;
    }
  }
  
  dis_source_to_dest(&dis_mff_sol, &min_final_state);
  GpG.num_esp_sol = optimal_plan_length;
  memcpy(GpG.esp_solution, optimal_plan, GpG.num_esp_sol*sizeof(int));
}

int model1001()
{
  int i, j, k;
  dis_Fact *f;

  constructg1001();
  GpG.subsolver = 1;
  if (GpG.is_preferences)
  {
    for (i=0;i<g1001.Norder;i++)
      for (j=0;j<g1001.Nproduct;j++)
        if (g1001.included[i][j])
        {
          for (k=0;i<dis_gnum_ft_conn;k++)
          {
            f = &(dis_grelevant_facts[k]);
            if (strlen(dis_gpredicates[f->predicate]) == 9 &&
            dis_gpredicates[f->predicate][0] == 'D' &&
            atoi(&dis_gconstants[f->args[0]][1])-1 == i &&
            atoi(&dis_gconstants[f->args[1]][1])-1 == j)
              break;
          }
          g1001weight[i][j] = (int) calculate_violation_not(k);
        }
        else
          g1001weight[i][j] = 0;

    traj_n = 0;
    dis_copy_dis_source_to_dest(&S0, &dis_ginitial_state);
    dis_check_constraints(&dis_ginitial_state);
  }
  
    if (GpG.is_preferences)
      solve1001sp_wrapper();
    else
      if (GpG.is_durative)
      {
        if (g1001.upperbound != -1)
          solve1001t();
        else
          solve1001mt();
      }
      else
      {
        if (GpG.is_fluents)
          solve1001mt();
        else
          solve1001pro_wrapper();
      }

  if (GpG.is_preferences || GpG.is_constraints)
  {
    if (GpG.is_durative)
      construct_traj();
    else
    {
      traj_n = GpG.num_esp_sol;
      memcpy(traj_plan, GpG.esp_solution, traj_n*sizeof(int));
    }
    if (!dis_check_constraints(&dis_mff_sol))
    {
//      dis_print_dis_State(dis_mff_sol);
//      printf("\nINFEASIBLE FINAL STATE\n");
      exit(0);
    }
//      dis_print_dis_State(dis_mff_sol);
//      printf("\nFINAL STATE\n");
  }
  return 1;    
}  

// for trucks
typedef struct _Global1002
{
  int Ntruck, Npackage, Nlocation, Narea;
  int source[50], dest[50], trucksource[10], dead[50];
  float drive_time[10][10], deadline[50];
} Global1002;
Global1002 g1002;

void constructg1002()
{
  int i, j, k, n;
  dis_Fact *f;
  
  for (i=0;i<dis_gnum_types;i++)
  {
    if (strlen(dis_gtype_names[i]) == 5 && dis_gtype_names[i][0] == 'T' && dis_gtype_names[i][4] == 'K')
      g1002.Ntruck = dis_gtype_size[i];
    if (strlen(dis_gtype_names[i]) == 7 && dis_gtype_names[i][0] == 'P' && dis_gtype_names[i][3] == 'K')
      g1002.Npackage = dis_gtype_size[i];
    if (strlen(dis_gtype_names[i]) == 8 && dis_gtype_names[i][0] == 'L' && dis_gtype_names[i][1] == 'O')
      g1002.Nlocation = dis_gtype_size[i];
    if (strlen(dis_gtype_names[i]) == 9 && dis_gtype_names[i][0] == 'T' && dis_gtype_names[i][2] == 'U')
      g1002.Narea = dis_gtype_size[i];
  }
  
  for (i=0;i<g1002.Npackage;i++)
    g1002.source[i] = g1002.dest[i] = g1002.dead[i] = -1;
  if (GpG.is_durative)
  {
    for (i=0;i<g1002.Nlocation;i++)
      for (j=0;j<g1002.Nlocation;j++)
        g1002.drive_time[i][j] = 100000000;
    for (i=0;i<g1002.Npackage;i++)
      g1002.deadline[i] = 100000000;
  }
  else
  {
    for (i=0;i<dis_gnum_types;i++)
      if (strlen(dis_gtype_names[i]) == 4 && dis_gtype_names[i][0] == 'T' && dis_gtype_names[i][3] == 'E')
        n = dis_gtype_size[i];
    for (i=0;i<g1002.Npackage;i++)
      g1002.deadline[i] = n - 1;
  }
    
  for (i=0;i<dis_ginitial_state.num_F;i++)
  {
    f = &(dis_grelevant_facts[dis_ginitial_state.F[i]]);
    if (dis_gpredicates[f->predicate][0] == 'A' &&
    dis_gpredicates[f->predicate][1] == 'T')
    {
      k = atoi(&dis_gconstants[f->args[1]][1]) - 1;
      if (dis_gconstants[f->args[0]][0] == 'T')
      {
        j = atoi(&dis_gconstants[f->args[0]][5]) - 1;      
        g1002.trucksource[j] = k;
      }
      else
      {
        j = atoi(&dis_gconstants[f->args[0]][7]) - 1;      
        g1002.source[j] = k;
      }
    }
  }              
        
  if (GpG.is_durative)
  {
    for (n=0;n<dis_gnum_functions;n++)
      if (dis_gfunctions[n][0] == 'D' && strlen(dis_gfunctions[n]) == 10)
        break;
    for (k=0;k<dis_gnum_initial_function[n];k++)
    {
      i = atoi(&dis_gconstants[dis_ginitial_function[n][k].fluent.args[0]][1]) - 1;
      j = atoi(&dis_gconstants[dis_ginitial_function[n][k].fluent.args[1]][1]) - 1;
      g1002.drive_time[i][j] = dis_ginitial_function[n][k].value;
    }
    for (i=0;i<g1002.Nlocation;i++)
      g1002.drive_time[i][i] = 0;

    if (GpG.is_preferences || GpG.is_constraints)
      for (i=0;i<dis_gnum_con_conn;i++)
        if (dis_gcon_conn[i].connective == dis_WITHIN_c)
        {
          f = &(dis_grelevant_facts[dis_gef_conn[dis_gcon_conn[i].cond[0]].PC[0]]);
          n = atoi(&dis_gconstants[f->args[0]][7]) - 1;
          k = atoi(&dis_gconstants[f->args[1]][1]) - 1;
          g1002.dest[n] = k;
          g1002.deadline[n] = dis_gcon_conn[i].time1;
          g1002.dead[n] = i;
        }
        
    for (i=0;i<saved_dis_gnum_flogic_goal;i++)
    {
      f = &(dis_grelevant_facts[saved_dis_gflogic_goal[i]]);
      if (strlen(dis_gpredicates[f->predicate]) == 9 && dis_gpredicates[f->predicate][0] == 'D')
      {
        n = atoi(&dis_gconstants[f->args[0]][7]) - 1;
        k = atoi(&dis_gconstants[f->args[1]][1]) - 1;
        g1002.dest[n] = k;
      }
    }
  }
  else
  {
    for (i=0;i<g1002.Nlocation;i++)
      for (j=0;j<g1002.Nlocation;j++)
        if (i == j)
          g1002.drive_time[i][j] = 0;
        else
          g1002.drive_time[i][j] = 1;
    if (GpG.is_preferences || GpG.is_constraints)
      for (i=0;i<dis_gnum_con_conn;i++)
        if (dis_gcon_conn[i].connective == dis_AT_END_c)
        {
          f = &(dis_grelevant_facts[dis_gef_conn[dis_gcon_conn[i].cond[0]].PC[0]]);
          n = atoi(&dis_gconstants[f->args[0]][7]) - 1;
          k = atoi(&dis_gconstants[f->args[1]][1]) - 1;
          j = atoi(&dis_gconstants[f->args[2]][1]) - 1;
          g1002.dest[n] = k;
          g1002.deadline[n] = j;
          g1002.dead[n] = i;
        }
        
    for (i=0;i<saved_dis_gnum_flogic_goal;i++)
    {
      f = &(dis_grelevant_facts[saved_dis_gflogic_goal[i]]);
      if (strlen(dis_gpredicates[f->predicate]) == 14 && dis_gpredicates[f->predicate][2] == '-')
      {
        n = atoi(&dis_gconstants[f->args[0]][7]) - 1;
        k = atoi(&dis_gconstants[f->args[1]][1]) - 1;
        g1002.dest[n] = k;
      }
      if (strlen(dis_gpredicates[f->predicate]) == 9 && dis_gpredicates[f->predicate][0] == 'D')
      {
        n = atoi(&dis_gconstants[f->args[0]][7]) - 1;
        k = atoi(&dis_gconstants[f->args[1]][1]) - 1;
        j = atoi(&dis_gconstants[f->args[2]][1]);
        g1002.dest[n] = k;
        g1002.deadline[n] = j;
      }
    }
  }

/*for (i=0;i<g1002.Npackage;i++)
  {
    if (g1002.source[i] == -1)
      fprintf(stderr, "source of package %d is undefined\n", i+1);
    if (g1002.dest[i] == -1)
      fprintf(stderr, "dest of package %d is undefined\n", i+1);
    if (GpG.is_durative)
    {
      if (g1002.deadline[i] == 100000000)
        fprintf(stderr, "deadline of package %d is undefined\n", i+1);
    }
    else
      if (g1002.deadline[i] == 100000000)
        fprintf(stderr, "deadline of package %d is undefined\n", i+1);
  }
  for (i=0;i<g1002.Nlocation-1;i++)
    for (j=i+1;j<g1002.Nlocation;j++)
      if (g1002.drive_time[i][j] == 100000000)
        fprintf(stderr, "drive_time %d %d is undefined\n", i+1, j+1);*/
}

float distance_metric(int p, int t)
{
  float distance;
  
  distance = 3*g1002.drive_time[g1002.source[p]][g1002.trucksource[t]];
  distance += g1002.drive_time[g1002.dest[p]][g1002.trucksource[t]];
  return distance;
}

void cluster(int *part)
{
  int i, j, k, m;
  float dist[50][10];

  for (i=0;i<g1002.Npackage;i++)
  {
    part[i] = -1;
    for (j=0;j<g1002.Ntruck;j++)
      dist[i][j] = distance_metric(i, j); 
  }
  
  i = 0;
  for (k=0;k<g1002.Npackage;k++)
  {
    m = 0;
    for (j=1;j<g1002.Npackage;j++)  
      if (dist[j][i] < dist[m][i])
        m = j;
    part[m] = i;
    for (j=0;j<g1002.Ntruck;j++)
      dist[m][j] = 100000000;
    i = (i+1)%g1002.Ntruck;
  }
}

int solve1002t(int *part)
{
  int i, k, goal0[100], nr_g, at[10], atp[50];
  dis_Fact *f;
  
  dis_known_iga_list.num_F = 0;
  num_dur = -1;
  memcpy(goal0, saved_dis_gflogic_goal, saved_dis_gnum_flogic_goal*sizeof(int));
  nr_g = saved_dis_gnum_flogic_goal;

  for (i=0;i<dis_ginitial_state.num_F;i++)
  {
    f = &(dis_grelevant_facts[dis_ginitial_state.F[i]]);
    if (dis_gpredicates[f->predicate][0] == 'A' &&
    dis_gpredicates[f->predicate][1] == 'T' && strlen(dis_gpredicates[f->predicate]) == 2)
    {
      if (dis_gconstants[f->args[0]][0] == 'T')
      {
        at[atoi(&dis_gconstants[f->args[0]][5])-1] = dis_ginitial_state.F[i];
        dis_ginitial_state.F[i] = dis_ginitial_state.F[--(dis_ginitial_state.num_F)];
        i--;
      }
      if (dis_gconstants[f->args[0]][0] == 'P')
      {
        atp[atoi(&dis_gconstants[f->args[0]][7])-1] = dis_ginitial_state.F[i];
        dis_ginitial_state.F[i] = dis_ginitial_state.F[--(dis_ginitial_state.num_F)];
        i--;
      }
    }
  }

  for (k=0;k<g1002.Ntruck;k++)
  {
    dis_ginitial_state.F[dis_ginitial_state.num_F++] = at[k];
    for (i=0;i<g1002.Npackage;i++)
      if (part[i] == k)
        dis_ginitial_state.F[dis_ginitial_state.num_F++] = atp[i];

    saved_dis_gnum_flogic_goal = 0;
    for (i=0;i<nr_g;i++)
    {
      f = &(dis_grelevant_facts[goal0[i]]);
      if (part[atoi(&dis_gconstants[f->args[0]][7])-1] == k)
        saved_dis_gflogic_goal[saved_dis_gnum_flogic_goal++] = goal0[i];
    }

    if (saved_dis_gnum_flogic_goal)
    {
      g_weight = 0.02/saved_dis_gnum_flogic_goal;
      if (!no_part(100000, 10000))
        exit(0);
    }

    for (i=0;i<dis_ginitial_state.num_F;i++)
      if (group_id(dis_ginitial_state.F[i]) == group_id(at[k]))
      {
        dis_ginitial_state.F[i] = dis_ginitial_state.F[--dis_ginitial_state.num_F];
        break;
      }
  }
  
  return 1;
}

int partition(int *part)
{
  int i, j, k, order[50];
  
  for (i=0;i<g1002.Npackage;i++)
    order[i] = i;
  for (i=0;i<g1002.Npackage;i++)
  {
    k = i;
    for (j=i+1;j<g1002.Npackage;j++)
      if (g1002.deadline[order[j]] < g1002.deadline[order[k]])
        k = j;
    j = order[i];
    order[i] = order[k];
    order[k] = j;
  }
  
  for (i=0;i<g1002.Npackage;i++)
    if (i == 0)
      part[order[i]] = 0;
    else
    {
      if (g1002.deadline[order[i]] == g1002.deadline[order[i-1]])
        part[order[i]] = part[order[i-1]];
      else
        part[order[i]] = part[order[i-1]]+1;
    }
    
  return part[order[g1002.Npackage-1]]+1;
}

int dead;

int solve1002pro(int n, int *part)
{
  int i, j, k, goal0[100], nr_g, atp[50], at[10];
  dis_Fact *f;
  
// need to check 
  dis_known_iga_list.num_F = 0;
  memcpy(goal0, saved_dis_gflogic_goal, saved_dis_gnum_flogic_goal*sizeof(int));
  nr_g = saved_dis_gnum_flogic_goal;

  for (i=0;i<dis_ginitial_state.num_F;i++)
  {
    f = &(dis_grelevant_facts[dis_ginitial_state.F[i]]);
    if (dis_gpredicates[f->predicate][0] == 'A' &&
    dis_gpredicates[f->predicate][1] == 'T' && strlen(dis_gpredicates[f->predicate]) == 2)
    {
      if (dis_gconstants[f->args[0]][0] == 'T')
          for (j=0;j<transitiveGraph[group_id(dis_ginitial_state.F[i])].elementSize;j++)
            at[j] = transitiveGraph[group_id(dis_ginitial_state.F[i])].element[j];          
      if (dis_gconstants[f->args[0]][0] == 'P')
      {
        atp[atoi(&dis_gconstants[f->args[0]][7])-1] = dis_ginitial_state.F[i];
        dis_ginitial_state.F[i] = dis_ginitial_state.F[--(dis_ginitial_state.num_F)];
        i--;
      }
    }
  }
  
  j = 0;
  for (k=0;k<n;k++)
  {
    saved_dis_gnum_flogic_goal = 0;
add_more_goals:
    for (i=0;i<nr_g;i++)
    {
      f = &(dis_grelevant_facts[goal0[i]]);
      if (part[atoi(&dis_gconstants[f->args[0]][7])-1] == k)
        saved_dis_gflogic_goal[saved_dis_gnum_flogic_goal++] = goal0[i];
    }

    for (i=0;i<g1002.Npackage;i++)
      if (part[i] == k)
      {
        dis_ginitial_state.F[dis_ginitial_state.num_F++] = atp[i];
        dead = g1002.deadline[i];
      }
    dead -= j;

    if (saved_dis_gnum_flogic_goal < 3)
      if (k < n - 1)
      {
        k++;
        goto add_more_goals;
      }

    g_weight = 10;
    if (!no_part(100000, 10000))
    {
      g_weight = 0;
      if (!no_part(200000, 10000))
        exit(0);
    }
    else
      for (i=0;i<dis_gnum_plan_ops;i++)
        if (strlen(dis_gop_conn[dis_gplan_ops[i]].action->name) == 5)
          j++;
  }
  
  return 1;
}

int solve1002sp(int n, int *part)
{
  int i, j, k, goal0[100], nr_g, atp[50], at[10], farea[10];
  dis_Fact *f;
  dis_Action *a;
  
// need to check 
  dis_known_iga_list.num_F = 0;
  for (i=0;i<dis_gnum_op_conn;i++)
  {
    a = dis_gop_conn[i].action;
    if (strlen(a->name) == 7)
      if (g1002.deadline[atoi(&dis_gconstants[a->name_inst_table[0]][7])-1] == 0)
        if (a->name_inst_table[2] != a->name_inst_table[3])
          dis_gop_conn[i].num_E = 0;
  }

  memcpy(goal0, saved_dis_gflogic_goal, saved_dis_gnum_flogic_goal*sizeof(int));
  nr_g = saved_dis_gnum_flogic_goal;
  for (k=0;k<g1002.Npackage;k++)
    if (g1002.dead[k] >= 0)
      for (i=0;i<nr_g;i++)
      {
        f = &(dis_grelevant_facts[goal0[i]]);
        if (atoi(&dis_gconstants[f->args[0]][7])-1 == k)
        {
          if (g1002.deadline[k] > 0)
            goal0[i] = dis_gef_conn[dis_gcon_conn[g1002.dead[k]].cond[0]].PC[0];
          break;
        }
      }

  for (i=0;i<dis_ginitial_state.num_F;i++)
  {
    f = &(dis_grelevant_facts[dis_ginitial_state.F[i]]);
    if (dis_gpredicates[f->predicate][0] == 'A' &&
    dis_gpredicates[f->predicate][1] == 'T' && strlen(dis_gpredicates[f->predicate]) == 2)
    {
      if (dis_gconstants[f->args[0]][0] == 'T')
          for (j=0;j<transitiveGraph[group_id(dis_ginitial_state.F[i])].elementSize;j++)
            at[j] = transitiveGraph[group_id(dis_ginitial_state.F[i])].element[j];          
      if (dis_gconstants[f->args[0]][0] == 'P')
      {
        atp[atoi(&dis_gconstants[f->args[0]][7])-1] = dis_ginitial_state.F[i];
        dis_ginitial_state.F[i] = dis_ginitial_state.F[--(dis_ginitial_state.num_F)];
        i--;
      }
    }
    if (dis_gpredicates[f->predicate][0] == 'F' &&
    strlen(dis_gpredicates[f->predicate]) == 4)
    {
      farea[atoi(&dis_gconstants[f->args[0]][1])-1] = dis_ginitial_state.F[i];
      dis_ginitial_state.F[i] = dis_ginitial_state.F[--(dis_ginitial_state.num_F)];
      i--;
    }
  }
  
  j = 0;
  for (k=0;k<n;k++)
  {
    saved_dis_gnum_flogic_goal = 0;
    for (i=0;i<nr_g;i++)
    {
      f = &(dis_grelevant_facts[goal0[i]]);
      if (part[atoi(&dis_gconstants[f->args[0]][7])-1] == k)
        saved_dis_gflogic_goal[saved_dis_gnum_flogic_goal++] = goal0[i];
    }

    for (i=0;i<g1002.Npackage;i++)
      if (part[i] == k)
      {
        dis_ginitial_state.F[dis_ginitial_state.num_F++] = atp[i];
        dead = g1002.deadline[i];
      }
    if (dead > 0)
      dead -= j;
    else
      dead = 1000;

    if (k < n-1 || dead < 2*saved_dis_gnum_flogic_goal)
    {
      for (i=0;i<saved_dis_gnum_flogic_goal&&i<g1002.Narea;i++)
        dis_ginitial_state.F[dis_ginitial_state.num_F++] = farea[i];
    
      g_weight = 1;
      if (!no_part(100000, 10000))
      {
        g_weight = 0;
        if (!no_part(200000, 10000))
          exit(0);
      }
      else
      {
        for (i=0;i<dis_gnum_plan_ops;i++)
          if (strlen(dis_gop_conn[dis_gplan_ops[i]].action->name) == 5)
            j++;

        for (i=0;i<dis_ginitial_state.num_F;i++)
        {
          f = &(dis_grelevant_facts[dis_ginitial_state.F[i]]);
          if (dis_gpredicates[f->predicate][0] == 'F' &&
          strlen(dis_gpredicates[f->predicate]) == 4)
          {
            dis_ginitial_state.F[i] = dis_ginitial_state.F[--(dis_ginitial_state.num_F)];
            i--;
          }
        }
      }
    }
    else
    {
      dis_ginitial_state.F[dis_ginitial_state.num_F++] = farea[0];
      subgoal_inc_planning(10000, 100000);
    }
  }
  
  return 1;
}

int model1002()
{
  int n, part[50], i;
  
  if (GpG.is_preferences || GpG.is_constraints)
  {
    traj_n = 0;
    dis_copy_dis_source_to_dest(&S0, &dis_ginitial_state);
    dis_check_constraints(&dis_ginitial_state);
/*  dis_print_dis_State(dis_ginitial_state);
    printf("\nINITIAL STATE\n");*/
  }

  constructg1002();
  if (GpG.is_durative)
  {
    {
      cluster(part);
        solve1002t(part);
    }
  }
  else
  {
    n = partition(part);
    if (!GpG.is_preferences)
      solve1002pro(n, part);
    else
        solve1002sp(n, part);
  }

  if (GpG.is_preferences || GpG.is_constraints)
  {
    if (GpG.is_durative)
      construct_traj();
    else
    {
      traj_n = GpG.num_esp_sol;
      memcpy(traj_plan, GpG.esp_solution, traj_n*sizeof(int));
    }
    if (!dis_check_constraints(&dis_mff_sol))
    {
//      dis_print_dis_State(dis_mff_sol);
//      printf("\nINFEASIBLE FINAL STATE\n");
      exit(0);
    }
//      dis_print_dis_State(dis_mff_sol);
//      printf("\nFINAL STATE\n");
  }
  return 1;
}

int solve1004pro()
{
  int g, nr_g = 0;
  int goal1[1000];
  dis_State saved_dis_ginitial_state;

  dis_copy_dis_source_to_dest(&saved_dis_ginitial_state, &dis_ginitial_state);
  nr_g = saved_dis_gnum_flogic_goal;
  memcpy(goal1, saved_dis_gflogic_goal, saved_dis_gnum_flogic_goal*sizeof(int));
  GpG.num_esp_sol = 0;
  dis_red_space = 1;

  saved_dis_gnum_flogic_goal = 1;  
  for (g=0;g<nr_g;g++)
  {
    saved_dis_gflogic_goal[0] = goal1[g];
    if (!no_part(10000, 10000))
    {
      dis_red_space = 0;
      GpG.num_esp_sol = 0;
      dis_source_to_dest(&dis_ginitial_state, &saved_dis_ginitial_state);
      saved_dis_gnum_flogic_goal = nr_g;
      memcpy(saved_dis_gflogic_goal, goal1, nr_g*sizeof(int));
      subgoal_inc_planning(10000, 100000);
    }  
  }

  return 1;
}

extern int *relax_lused_O, relax_lnum_used_O;
typedef struct _Global1004
{
  int nr_g, goal[100], nr_sub, sub[100], sub0[100], maxsub;
  float upperbound, ub;
  int Nsolution, Maxsolution, timestamp, sol[100], sol0[100];
} Global1004;
Global1004 g1004;

void compute_benefit1(dis_State *src, float *v1, int *seq)
{
  int i, j, k, g, h, ef;
  dis_State S;
  char *temp;

  dis_make_state(&S, dis_gnum_ft_conn, dis_gnum_fl_conn);
  dis_source_to_dest(&S, src);
  g = 0;
  for (i=0;i<g1004.nr_sub;i++)
    if (seq[i] == -1)
    {
      S.F[S.num_F++] = g1004.sub0[i];
      v1[i] = 0;
    }
    else
    {
      g++;
      v1[i] = 100000000;
    }

  dis_gnum_flogic_goal = 1;
  for (i=0;i<g1004.nr_g;i++)
  {
    for (j=0;j<g1004.nr_sub;j++)
      if (v1[i] < 100000000)
        v1[j] += dis_gcon_conn[g1004.goal[i]].weight;
    dis_gflogic_goal[0] = dis_gcon_conn[g1004.goal[i]].fact;
    h = dis_get_1P(&S);
    if (h != dis_INFINITY)
    {
      k = 0;
      for (j=0;j<relax_lnum_used_O;j++)
      {
        temp = dis_gop_conn[relax_lused_O[j]].action->name;
        if (strlen(temp) == 6 && temp[0] == 'C')
          k++;
      }
      
      if (g + k <= g1004.maxsub)
      for (j=0;j<relax_lnum_used_O;j++)
      {
        temp = dis_gop_conn[relax_lused_O[j]].action->name;
        if (strlen(temp) == 6 && temp[0] == 'C')
        {
          ef = dis_gop_conn[relax_lused_O[j]].E[0];
          for (k=0;k<g1004.nr_sub;k++)
            if (dis_gef_conn[ef].A[0] == g1004.sub[k])
            {
              v1[k] -= dis_gcon_conn[g1004.goal[i]].weight;
              break;
            }
        }
      }    
    }
  }    
  
  free(S.F);
  free(S.f_D);
  free(S.f_V);
}

float compute_benefit(dis_State *src, int sub)
{
  int i;
  dis_State S;
  float v;
  
  dis_make_state(&S, dis_gnum_ft_conn, dis_gnum_fl_conn);
  dis_source_to_dest(&S, src);
  v = 0;

  dis_gnum_flogic_goal = 1;
  S.F[S.num_F++] = sub;
  for (i=0;i<g1004.nr_g;i++)
  {
    dis_gflogic_goal[0] = dis_gcon_conn[g1004.goal[i]].fact;
    if (dis_get_1P(&S) == dis_INFINITY)
      v += dis_gcon_conn[g1004.goal[i]].weight;
  }
  
  free(S.F);
  free(S.f_D);
  free(S.f_V);
  return v;
}

void solve1004sp0()
{
  int i, j, k, l, ef;
  char *temp;
  dis_State saved_dis_ginitial_state;
  float v = 0;

  dis_copy_dis_source_to_dest(&saved_dis_ginitial_state, &dis_ginitial_state);
  for (i=0;i<g1004.nr_sub;i++)
    g1004.sol0[i] = -1;
  max_bfs_iter = 0;
  max_hc_iter = 10000;
  bridge_option = 0;

  l = 0;
  dis_gnum_flogic_goal = 1;
  for (i=0;i<g1004.nr_g;i++)
  {
    dis_gflogic_goal[0] = dis_gcon_conn[g1004.goal[i]].fact;
    dis_gnum_plan_ops = 0;
    if (dis_get_1P(&dis_ginitial_state) != dis_INFINITY)
      dis_mff_bridge();
    if (dis_gnum_plan_ops > 0)
    {
      for (j=0;j<dis_gnum_plan_ops;j++)
      {
        temp = dis_gop_conn[dis_gplan_ops[j]].action->name;
        if (strlen(temp) == 6 && temp[0] == 'C')
        {
          ef = dis_gop_conn[dis_gplan_ops[j]].E[0];
          for (k=0;k<g1004.nr_sub;k++)
            if (dis_gef_conn[ef].A[0] == g1004.sub[k])
            {
              g1004.sol0[k] = l++;
              break;
            }
        }
      }
      dis_source_to_dest(&dis_ginitial_state, &dis_mff_sol);
    }
    else
      v += dis_gcon_conn[g1004.goal[i]].weight;
  }  

  g1004.ub = l + v;
  g1004.Nsolution = 1;
  g1004.timestamp = 0;
  g1004.upperbound = 1000000;
  dis_source_to_dest(&dis_ginitial_state, &saved_dis_ginitial_state);
  free(saved_dis_ginitial_state.F);
  free(saved_dis_ginitial_state.f_D);
  free(saved_dis_ginitial_state.f_V);
}

void solve1004sp(dis_State *src, int nsub, int *seq, float v0)
{
  int i, k;
  float minv, minv1, v[100], v1[100], t;
  dis_State S;

  if (nsub + v0 < g1004.upperbound)
  {
    g1004.timestamp = g1004.Nsolution;
    memcpy(g1004.sol, seq, g1004.nr_sub*sizeof(int));
    g1004.upperbound = nsub + v0;
  }
//  if (g1004.Nsolution >= g1004.Maxsolution)
//    fprintf(stderr, "Incomplete search\n");

  if (++g1004.Nsolution < g1004.Maxsolution && nsub < g1004.nr_sub)
  {
    for (i=0;i<g1004.nr_sub;i++)
      if (seq[i] == -1)
        v[i] = compute_benefit(src, g1004.sub[i]);
      else
        v[i] = 100000000;
    compute_benefit1(src, v1, seq);
    dis_make_state(&S, dis_gnum_ft_conn, dis_gnum_fl_conn);

    while (1)
    {        
      minv = 1000000;
      k = -1;
      for (i=0;i<g1004.nr_sub;i++)
        if (v[i] < minv || (v[i] == minv && v1[i] < minv1))
          if (nsub + 1 + v1[i] < g1004.upperbound)
          {
            k = i;
            minv = v[i];
            minv1 = v1[i];
          }

      if (k == -1)
      {
        free(S.F);
        free(S.f_D);
        free(S.f_V);
        return;
      }

      t = v[k];
      v[k] = 100000000;
      dis_gnum_flogic_goal = 1;
      dis_gnum_fnumeric_goal = 0;
      dis_gflogic_goal[0] = g1004.sub[k];
      dis_source_to_dest(&dis_ginitial_state, src);
      dis_ginitial_state.F[dis_ginitial_state.num_F++] = g1004.sub0[k];
      dis_mff_bridge();
      dis_source_to_dest(&S, &dis_mff_sol);
      seq[k] = nsub;
      solve1004sp(&S, nsub+1, seq, t);
      seq[k] = -1;
    }    
  }
}

int solve1004sp_wrapper()
{
  int i, j, seq[100];
  char temp[128], temp1[128];
  dis_State saved_dis_ginitial_state, S;
  dis_Fact *f;
  float v;
  
  dis_copy_dis_source_to_dest(&saved_dis_ginitial_state, &dis_ginitial_state);
  dis_make_state(&S, dis_gnum_ft_conn, dis_gnum_fl_conn);
  g1004.nr_g = 0;
  GpG.num_esp_sol = 0;
  for (j=0;j<dis_gnum_con_conn;j++)
  {
    dis_print_ft_name_string(dis_gcon_conn[j].fact, temp);
    if (strncmp(temp, "(_P0", 4) == 0)
      g1004.goal[g1004.nr_g++] = j;
  }

  g1004.nr_sub = 0;
  for (i=0;i<dis_ginitial_state.num_F;i++)
  {
    f = &(dis_grelevant_facts[dis_ginitial_state.F[i]]);
    dis_print_ft_name_string(dis_ginitial_state.F[i], temp);
    if (dis_garity[f->predicate] == 1 && strlen(dis_gpredicates[f->predicate]) == 14)
    {
      g1004.sub0[g1004.nr_sub] = dis_ginitial_state.F[i];
      dis_ginitial_state.F[i--] = dis_ginitial_state.F[--dis_ginitial_state.num_F];
      for (j=0;j<dis_gnum_ft_conn;j++)
      {
        dis_print_ft_name_string(j, temp1);
        if (strcmp(temp+9, temp1+1) == 0)
        {
          g1004.sub[g1004.nr_sub++] = j;
          break;
        }
      }
    }
  }
  
  dis_source_to_dest(&S, &dis_ginitial_state);
  dis_source_to_dest(&dis_ginitial_state, &saved_dis_ginitial_state);
  solve1004sp0();
  g1004.upperbound = 10000;
  g1004.Maxsolution = 1000;
  g1004.maxsub = dis_gnum_con_conn - g1004.nr_g;
  for (i=0;i<g1004.nr_sub;i++)
    seq[i] = -1;

  v = 0;
  for (i=0;i<g1004.nr_g;i++)
    v += dis_gcon_conn[g1004.goal[i]].weight;
  solve1004sp(&S, 0, seq, v);
//  fprintf(stderr, "Done: %d %d %f\n", g1004.Nsolution, g1004.timestamp, g1004.upperbound);
  if (g1004.upperbound > g1004.ub)
    memcpy(g1004.sol, g1004.sol0, g1004.nr_sub*sizeof(int));
  bridge_option = 0;
  max_hc_iter = 10000;
  max_bfs_iter = 100000;

  dis_gnum_flogic_goal = 0;
  dis_source_to_dest(&dis_ginitial_state, &saved_dis_ginitial_state);
  for (i=0;i<g1004.nr_sub;i++)
    if (g1004.sol[i] != -1)
      dis_gflogic_goal[dis_gnum_flogic_goal++] = g1004.sub[i];
  dis_mff_bridge();  
  mff_record_esp_sol();
  dis_source_to_dest(&dis_ginitial_state, &dis_mff_sol);
  
  for (i=0;i<dis_ginitial_state.num_F;i++)
  {
    f = &(dis_grelevant_facts[dis_ginitial_state.F[i]]);
    dis_print_ft_name_string(dis_ginitial_state.F[i], temp);
    if (dis_garity[f->predicate] == 1 && strlen(dis_gpredicates[f->predicate]) == 14)
      dis_ginitial_state.F[i--] = dis_ginitial_state.F[--dis_ginitial_state.num_F];
  }

  saved_dis_gnum_flogic_goal = 0;
  dis_gnum_flogic_goal = 1;
  for (i=0;i<g1004.nr_g;i++)
  {
    dis_gflogic_goal[0] = dis_gcon_conn[g1004.goal[i]].fact;
    if (dis_get_1P(&dis_ginitial_state) != dis_INFINITY)
      saved_dis_gflogic_goal[saved_dis_gnum_flogic_goal++] = dis_gflogic_goal[0];
  }
  subgoal_inc_planning(10000, 10000);
  
  free(S.F);
  free(S.f_D);
  free(S.f_V);
  free(saved_dis_ginitial_state.F);
  free(saved_dis_ginitial_state.f_D);
  free(saved_dis_ginitial_state.f_V);
  return 1;
}


int model1004()
{
  if (GpG.is_preferences || GpG.is_constraints)
  {
    traj_n = 0;
    dis_copy_dis_source_to_dest(&S0, &dis_ginitial_state);
    dis_check_constraints(&dis_ginitial_state);
/*  dis_print_dis_State(dis_ginitial_state);
    printf("\nINITIAL STATE\n");*/
  }

  if (GpG.is_durative)
  {
    {
      dis_copy_dis_source_to_dest(&S0, &dis_ginitial_state);
      producible_main();
    }
  }
  else
  {
    if (!GpG.is_preferences)
      solve1004pro();
    else
      solve1004sp_wrapper();
  }

  if (GpG.is_preferences || GpG.is_constraints)
  {
    if (GpG.is_durative)
      construct_traj();
    else
    {
      traj_n = GpG.num_esp_sol;
      memcpy(traj_plan, GpG.esp_solution, traj_n*sizeof(int));
    }
    if (!dis_check_constraints(&dis_mff_sol))
    {
//      dis_print_dis_State(dis_mff_sol);
//      printf("\nINFEASIBLE FINAL STATE\n");
      exit(0);
    }
//      dis_print_dis_State(dis_mff_sol);
//      printf("\nFINAL STATE\n");
  }
  return 1;
}

int solve1005pro()
{
  int g, nr_g = 0;
  int goal1[1000];
  dis_State saved_dis_ginitial_state;

  dis_copy_dis_source_to_dest(&saved_dis_ginitial_state, &dis_ginitial_state);
  nr_g = saved_dis_gnum_flogic_goal;
  memcpy(goal1, saved_dis_gflogic_goal, saved_dis_gnum_flogic_goal*sizeof(int));
  GpG.num_esp_sol = 0;
  dis_red_space = 1;

  saved_dis_gnum_flogic_goal = 0;  
  for (g=0;g<nr_g;g++)
  {
    saved_dis_gflogic_goal[saved_dis_gnum_flogic_goal++] = goal1[g];
    if (!no_part(10000, 10000))
    {
      saved_dis_gnum_flogic_goal = nr_g;
      memcpy(saved_dis_gflogic_goal, goal1, nr_g*sizeof(int));
      subgoal_inc_planning(10000, 100000);
    }  
  }

  return 1;
}

typedef struct _Global1005
{
  int goal[50], nr_g;
  float total, weight[50], upperbound;
  int Nsolution, Maxsolution, timestamp, sol[50], sollen;
} Global1005;
Global1005 g1005;

float compute_traverse_cost(dis_State *S, dis_State *src, int i)
{
  num_dur = 0;
  GpG.num_esp_sol = 0;
  dis_red_space = 1;
  dis_gcmd_line.optimize = 1;
  dis_source_to_dest(&dis_ginitial_state, src);
  saved_dis_gnum_flogic_goal = 1;
  saved_dis_gflogic_goal[0] = g1005.goal[i];  
  
  if (!no_part(0, 10000))
  {
    dis_source_to_dest(S, &dis_ginitial_state);
    return 100000000;
  }
  dis_source_to_dest(S, &dis_mff_sol);
  return S->f_V[0];
}

void solve1005sp(dis_State *src, int n, int *seq)
{
  int i, k;
  float minv, cost[50], v;
  dis_State *S[50], work_S;

  if (g1005.Nsolution > g1005.Maxsolution)
    return;
  for (i=0;i<50;i++)
    S[i] = NULL;
  if (n == g1005.nr_g-1)
  traj_n = 0;
    
  if (n < g1005.nr_g)
  {
    dis_make_state(&work_S, dis_gnum_ft_conn, dis_gnum_fl_conn);
    for (i=0;i<g1005.nr_g;i++)
      if (seq[i] == -1)
      {
        cost[i] = compute_traverse_cost(&work_S, src, i);
        dis_check_constraints(&work_S);
        v = dis_metric_value(&work_S);
        g1005.Nsolution++;
        if (v < g1005.upperbound)
        {
          g1005.upperbound = v;
          g1005.timestamp = g1005.Nsolution;
          memcpy(g1005.sol, seq, g1005.nr_g*sizeof(int));
          g1005.sol[i] = n;
          g1005.sollen = n + 1;
        }
        S[i] = (dis_State *) malloc(sizeof(dis_State));
        dis_copy_dis_source_to_dest(S[i], &work_S);
      }
      else
        cost[i] = 100000000;
    free(work_S.F);
    free(work_S.f_D);
    free(work_S.f_V);
    
    while (1)
    {        
      minv = g1005.upperbound;
      k = -1;
      for (i=0;i<g1005.nr_g;i++)
        if (cost[i] < minv)
        {
          k = i;
          minv = cost[i];
        }
      if (k == -1)
        break;

      seq[k] = n;
      solve1005sp(S[k], n+1, seq);
      seq[k] = -1;
      cost[k] = 100000000;
    }    

    for (i=0;i<g1005.nr_g;i++)  
      if (seq[i] == -1)
      {
        free(S[i]->F);
        free(S[i]->f_D);
        free(S[i]->f_V);
        free(S[i]);
      }
  }
}

void solve1005sp0()
{
  int g;
  
  saved_dis_gnum_flogic_goal = 0;
  dis_red_space = 1;
  dis_gcmd_line.optimize = 1;
  GpG.num_esp_sol = 0;
  for (g=0;g<g1005.nr_g;g++)
  {
    saved_dis_gflogic_goal[saved_dis_gnum_flogic_goal++] = g1005.goal[g];
    if (!no_part(10000, 10000))
      no_part(0, 10000);
  }
}

int solve1005sp_wrapper()
{
  int g, i, j, seq[50];
  dis_State S, saved_dis_ginitial_state;

  dis_copy_dis_source_to_dest(&S, &dis_ginitial_state);
  dis_copy_dis_source_to_dest(&saved_dis_ginitial_state, &dis_ginitial_state);
  for (g=0;g<100;g++)
  {
    g1005.goal[g] = -1;
    g1005.weight[g] = 0;
  }
  g1005.nr_g = 0;
  g1005.Maxsolution = 8000;
  g1005.total = 0;
  
  for (i=0;i<dis_gnum_con_conn;i++)
    if (dis_gcon_conn[i].connective == dis_AT_END_c)
    {
      g1005.total += dis_gcon_conn[i].weight;
      for (j=0;j<dis_gef_conn[dis_gcon_conn[i].cond[0]].num_PC;j++)
      {
        for (g=0;g<g1005.nr_g;g++)
          if (g1005.goal[g] == dis_gef_conn[dis_gcon_conn[i].cond[0]].PC[j])
            break;
        g1005.weight[g] += dis_gcon_conn[i].weight;
        if (g1005.goal[g] == -1)
          g1005.goal[g1005.nr_g++] = dis_gef_conn[dis_gcon_conn[i].cond[0]].PC[j];
      }
    }
    
  g1005.Nsolution = 1;
  g1005.timestamp = 0;
  g1005.sollen = 0;
  g1005.upperbound = g1005.total;
  for (g=0;g<g1005.nr_g;g++)
    g1005.sol[g] = seq[g] = -1;
  solve1005sp(&S, 0, seq);
//  fprintf(stderr, "%d %d %d %f\n", g1005.Nsolution, g1005.timestamp, g1005.sollen, g1005.upperbound);
  
  dis_red_space = 1;
  dis_gcmd_line.optimize = 1;
  dis_source_to_dest(&dis_ginitial_state, &saved_dis_ginitial_state);
  dis_source_to_dest(&dis_mff_sol, &saved_dis_ginitial_state);
  GpG.num_esp_sol = 0;
  saved_dis_gnum_flogic_goal = 0;
  for (i=0;i<g1005.sollen;i++)
  {
    for (g=0;g<g1005.nr_g;g++)
      if (g1005.sol[g] == i)
        break;
    saved_dis_gflogic_goal[saved_dis_gnum_flogic_goal++] = g1005.goal[g];
    if (!no_part(10000, 10000))
      no_part(0, 10000);
  }  

  free(S.F);
  free(S.f_D);
  free(S.f_V);
  free(saved_dis_ginitial_state.F);
  free(saved_dis_ginitial_state.f_D);
  free(saved_dis_ginitial_state.f_V);
  return 1;
}

typedef struct _MT1005
{
  int n, m, in_sun[100], dead[15][100], active, powersource[15][100];
  int dist[15][100][100], next[15][100][100], can_traverse[15][100][100];
} MT1005;
MT1005 mt1005;

int path(int r, int u, int v)
{
  int k = mt1005.can_traverse[r][u][v];
  
  if (k == -1)
    return v;
  else
    return path(r, u, k);
}

void constructmt1005()
{
  int i, j, k, l;
  int ins, cant;
  dis_Fact *f;
    
//  mt1005.active = 1;
  for (i=0;i<dis_gnum_types;i++)
    if (strlen(dis_gtype_names[i]) == 8 && dis_gtype_names[i][0] == 'W')
    {
      mt1005.n = dis_gtype_size[i];
      break;
    }
  for (i=0;i<dis_gnum_types;i++)
    if (strlen(dis_gtype_names[i]) == 5 && dis_gtype_names[i][0] == 'R')
    {
      mt1005.m = dis_gtype_size[i];
      break;
    }
  for (i=0;i<mt1005.n;i++)
  {
    mt1005.in_sun[i] = 0;
    for (k=0;k<mt1005.m;k++)
    {
      mt1005.dead[k][i] = 1000000;
      for (j=0;j<mt1005.n;j++)
      {
        mt1005.can_traverse[k][i][j] = -1;
        mt1005.dist[k][i][j] = 1000000;
        mt1005.next[k][i][j] = -1;
      }
      mt1005.dist[k][i][i] = 0;
      mt1005.next[k][i][i] = i;
    }
  }  
  
  for (i=0;i<dis_gnum_predicates;i++)
  {
    if (strlen(dis_gpredicates[i]) == 6 && dis_gpredicates[i][3] == 'S')
      ins = i;
    if (strlen(dis_gpredicates[i]) == 12 && dis_gpredicates[i][4] == 'T')
      cant = i;
  }

  for (i=0;i<dis_gnum_initial_predicate[ins];i++)
  {	
    f = &(dis_ginitial_predicate[ins][i]);
    j = atoi(&dis_gconstants[f->args[0]][8]);  
    mt1005.in_sun[j] = 1;
  }
  for (i=0;i<dis_gnum_initial_predicate[cant];i++)
  {	
    f = &(dis_ginitial_predicate[cant][i]);
    j = atoi(&dis_gconstants[f->args[0]][5]);  
    k = atoi(&dis_gconstants[f->args[1]][8]);  
    l = atoi(&dis_gconstants[f->args[2]][8]);  
    mt1005.dist[j][k][l] = 8;
    mt1005.can_traverse[j][k][l] = -1;
  }
  
  for (l=0;l<mt1005.m;l++)
  for (k=0;k<mt1005.n;k++)
    for (i=0;i<mt1005.n;i++)
      for (j=0;j<mt1005.n;j++)
        if (mt1005.dist[l][i][k] + mt1005.dist[l][k][j] < mt1005.dist[l][i][j])
        {
          mt1005.dist[l][i][j] = mt1005.dist[l][i][k] + mt1005.dist[l][k][j];
          mt1005.can_traverse[l][i][j] = k;
        }

  for (l=0;l<mt1005.m;l++)
    for (i=0;i<mt1005.n;i++)
      for (j=0;j<mt1005.n;j++)
        mt1005.next[l][i][j] = path(l, i, j);

  for (k=0;k<mt1005.m;k++)
  {
//    fprintf(stderr, "\nRover %d: ", k);
  for (i=0;i<mt1005.n;i++)
  {
    mt1005.powersource[k][i] = -1;
    for (j=0;j<mt1005.n;j++)
      if (mt1005.in_sun[j])
        if (mt1005.dead[k][i] > mt1005.dist[k][i][j])
        {
          mt1005.dead[k][i] = mt1005.dist[k][i][j];
          mt1005.powersource[k][i] = j;
        }
    mt1005.dead[k][i] += 0.5;
//    fprintf(stderr, "%d %d ", i, mt1005.dead[k][i]);
  }
  }
}

int check_power(dis_State *S, float *v)
{
  int i, j, k;
  dis_Fact *f;
  
  *v = 0;
  if (GpG.SearchModal != -1005 || !GpG.is_durative || !mt1005.active)
    return 1;

  for (i=0;i<S->num_F;i++)
  {
    f = &(dis_grelevant_facts[S->F[i]]);  
    if (strlen(dis_gpredicates[f->predicate]) == 2 && dis_gpredicates[f->predicate][0] == 'A')
    {
      j = atoi(&dis_gconstants[f->args[0]][5]);  
      k = atoi(&dis_gconstants[f->args[1]][8]);  
      if (S->f_V[j] < mt1005.dead[j][k])
      {
//        fprintf(stderr, "rover%d %d %f %d\n", j, k, S->f_V[j], mt1005.dead[j][k]);
        return 0;
      }
      (*v) += mt1005.dead[j][k];
    }
  }
  return 1;
}

int current_r;
int solve1005mt1()
{
  int nr_rover = 0, goal1[100], nr_g = 0;
  int i, j, r, w, w1, w0;
  int len, sol[100], loc[100];
  float value[100], energy[100];
  int g, e, h;
  dis_Fact *f, *f1, *fr;
  char n1, n2;
  dis_State saved_dis_ginitial_state;
  dis_Action *a;
  
  dis_copy_dis_source_to_dest(&saved_dis_ginitial_state, &dis_ginitial_state);
  nr_g = saved_dis_gnum_flogic_goal;
  memcpy(goal1, saved_dis_gflogic_goal, saved_dis_gnum_flogic_goal*sizeof(int));
  max_hc_iter = 10000;
  max_bfs_iter = 100000;
  bridge_option = 1;
  GpG.subsolver = 1;

  GpG.num_esp_sol = 0;
  dis_gnum_flogic_goal = 1;
  for (g=0;g<nr_g;g++)
  {
    for (i=0;i<2*nr_rover;i++)
      value[i] = dis_ginitial_state.f_V[i];
    for (i=0;i<nr_rover;i++)
    {
      dis_ginitial_state.f_V[i] = 10000;
      dis_ginitial_state.f_V[nr_rover+i] = -10000;
    }
    dis_gflogic_goal[0] = goal1[g];
    bridge_option = 0;
//    fprintf(stderr, "y");
    dis_mff_bridge();

    if (dis_gnum_plan_ops > 0)
    {
//      fprintf(stderr, "x");
      a = dis_gop_conn[dis_gplan_ops[dis_gnum_plan_ops-1]].action;
      r = atoi(&(dis_gconstants[a->name_inst_table[0]][5]));
      len = dis_gnum_plan_ops;
      memcpy(sol, dis_gplan_ops, len*sizeof(int));
      for (i=0;i<2*nr_rover;i++)
        dis_ginitial_state.f_V[i] = value[i];

      for (i=0;i<len;i++)
      {
        for (j=0;j<dis_ginitial_state.num_F;j++)
        {
          f = &(dis_grelevant_facts[dis_ginitial_state.F[j]]);
          if (dis_gpredicates[f->predicate][1] == 'T' 
          && strlen(dis_gpredicates[f->predicate]) == 2)
            if (atoi(&(dis_gconstants[f->args[0]][5])) == r)
            {
              w0 = atoi(&(dis_gconstants[f->args[1]][8]));
              break;
            }
        }
        
//        fprintf(stderr, "%f ", dis_ginitial_state.f_V[r]);
        if (dis_ginitial_state.f_V[r] >= mt1005.dead[r][w0] && 80 - mt1005.dead[r][w0] > dis_ginitial_state.f_V[r])
        {
          w = w0;
          while(mt1005.dead[r][w] > 0)
          {
            w1 = mt1005.next[r][w][mt1005.powersource[r][w]];
            for (j=0;j<dis_gnum_op_conn;j++)
            {
              a = dis_gop_conn[j].action;
              if (a->name[0] == 'N')
                if (atoi(&(dis_gconstants[a->name_inst_table[0]][5])) == r &&
                atoi(&(dis_gconstants[a->name_inst_table[1]][8])) == w &&
                atoi(&(dis_gconstants[a->name_inst_table[2]][8])) == w1)
                {
                  dis_result_to_dest(&dis_mff_sol, &dis_ginitial_state, j, -1);
                  dis_source_to_dest(&dis_ginitial_state, &dis_mff_sol);
                  GpG.esp_solution[GpG.num_esp_sol++] = j;
                  break;
                }
            }
            if (j == dis_gnum_op_conn)
              exit(0);
            w = w1;
          }

          for (j=0;j<dis_gnum_op_conn;j++)
          {
            a = dis_gop_conn[j].action;
            if (a->name[0] == 'R')
              if (atoi(&(dis_gconstants[a->name_inst_table[0]][5])) == r &&
              atoi(&(dis_gconstants[a->name_inst_table[1]][8])) == w)
              {
                dis_result_to_dest(&dis_mff_sol, &dis_ginitial_state, j, -1);
                dis_source_to_dest(&dis_ginitial_state, &dis_mff_sol);
                GpG.esp_solution[GpG.num_esp_sol++] = j;
                break;
              }
          }
          if (j == dis_gnum_op_conn)
            exit(0);

          while (w != w0)
          {
            w1 = mt1005.next[r][w][w0];
            for (j=0;j<dis_gnum_op_conn;j++)
            {
              a = dis_gop_conn[j].action;
              if (a->name[0] == 'N')
              if (atoi(&(dis_gconstants[a->name_inst_table[0]][5])) == r &&
              atoi(&(dis_gconstants[a->name_inst_table[1]][8])) == w &&
              atoi(&(dis_gconstants[a->name_inst_table[2]][8])) == w1)
              {
                dis_result_to_dest(&dis_mff_sol, &dis_ginitial_state, j, -1);
                dis_source_to_dest(&dis_ginitial_state, &dis_mff_sol);
                GpG.esp_solution[GpG.num_esp_sol++] = j;
                break;
              }
            }
            if (j == dis_gnum_op_conn)
              exit(0);
            w = w1;
          }
        }

        a = dis_gop_conn[sol[i]].action;
        if (a->name[0] == 'R')
          continue;
        if (dis_result_to_dest(&dis_mff_sol, &dis_ginitial_state, sol[i], -1))
        {
          dis_source_to_dest(&dis_ginitial_state, &dis_mff_sol);
          GpG.esp_solution[GpG.num_esp_sol++] = sol[i];
        }
        else
          exit(0);
          
//        fprintf(stderr, "%f\n", dis_ginitial_state.f_V[r]);
      }
    }
    else
      exit(0);
  }

  return 1;
}

int model1005()
{
  int nr_g, goal[100];

  if (GpG.is_preferences || GpG.is_constraints)
  {
    traj_n = 0;
    dis_copy_dis_source_to_dest(&S0, &dis_ginitial_state);
    dis_check_constraints(&dis_ginitial_state);
/*  dis_print_dis_State(dis_ginitial_state);
    printf("\nINITIAL STATE\n");*/
  }

  if (GpG.is_durative && GpG.SecondaryModal)
  {
    dis_copy_dis_source_to_dest(&S0, &dis_ginitial_state);
    constructmt1005();
    nr_g = saved_dis_gnum_flogic_goal;
    memcpy(goal, saved_dis_gflogic_goal, sizeof(int)*nr_g);
    solve1005mt1();

  }
  else
  {
    if (!GpG.is_preferences)
      solve1005pro();
    else
      if (!GpG.is_constraints)
        solve1005sp_wrapper();
  }
  
  if (GpG.is_preferences || GpG.is_constraints)
  {
    if (GpG.is_durative)
      construct_traj();
    else
    {
      traj_n = GpG.num_esp_sol;
      memcpy(traj_plan, GpG.esp_solution, traj_n*sizeof(int));
    }
    if (!dis_check_constraints(&dis_mff_sol))
    {
//      dis_print_dis_State(dis_mff_sol);
//      printf("\nINFEASIBLE FINAL STATE\n");
      exit(0);
    }
//      dis_print_dis_State(dis_mff_sol);
//      printf("\nFINAL STATE\n");
  }
  return 1;
}

