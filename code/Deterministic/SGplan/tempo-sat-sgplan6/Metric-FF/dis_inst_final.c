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

 * File: dis_inst_final.c 

 * Description: modified from inst_final.c in Metric-FF

 *

 * Authors:  Chih-wei Hsu and Benjamin W. Wah

 *

 *********************************************************************/
/*
# Copyright (C) 2004, Board of Trustees of the University of Illinois.
#
# The program is copyrighted by the University of Illinois, and should
# not be distributed without prior approval.  Commercialization of this 
# product requires prior licensing from the University of Illinois.
# Commercialization includes the integration of this code in part or 
# whole into a product for resale. 
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Author: Y. X. Chen, C. W. Hsu, and B. W. Wah, University of Illinois
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
*/



/*
 * THIS SOURCE CODE IS SUPPLIED  ``AS IS'' WITHOUT WARRANTY OF ANY KIND, 
 * AND ITS AUTHOR AND THE JOURNAL OF ARTIFICIAL INTELLIGENCE RESEARCH 
 * (JAIR) AND JAIR'S PUBLISHERS AND DISTRIBUTORS, DISCLAIM ANY AND ALL 
 * WARRANTIES, INCLUDING BUT NOT LIMITED TO ANY IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE, AND
 * ANY WARRANTIES OR NON INFRINGEMENT.  THE USER ASSUMES ALL LIABILITY AND
 * RESPONSIBILITY FOR USE OF THIS SOURCE CODE, AND NEITHER THE AUTHOR NOR
 * JAIR, NOR JAIR'S PUBLISHERS AND DISTRIBUTORS, WILL BE LIABLE FOR 
 * DAMAGES OF ANY KIND RESULTING FROM ITS USE.  Without limiting the 
 * generality of the foregoing, neither the author, nor JAIR, nor JAIR's
 * publishers and distributors, warrant that the Source Code will be 
 * error-free, will operate without interruption, or will meet the needs 
 * of the user.
 */








/*********************************************************************
 * File: inst_final.c
 * Description: final domain representation functions
 *
 *
 * Author: Joerg Hoffmann 2000
 *
 *********************************************************************/ 








#include "dis_ff.h"

#include "dis_output.h"
#include "dis_memory.h"

#include "dis_expressions.h"
#include "dis_inst_pre.h"
#include "dis_inst_final.h"
#include "dis_producible.h"
#include "dis_constraints.h"

#include "lpg.h"











/********************************
 * POSSIBLY dis_TRUE FACTS ANALYSIS *
 ********************************/

extern void build_transitive_graph();
extern void print_out_transitive_graph_set();
extern int num_invariantGroup;



/* local globals for this part
 */

/* Chih-Wei */
dis_Bool **dis_lpos;
dis_Bool **dis_lneg;
dis_Bool **dis_luse;
int **dis_lindex;

/*
dis_int_pointer dis_lpos[dis_MAX_PREDICATES];
dis_int_pointer dis_lneg[dis_MAX_PREDICATES];
dis_int_pointer dis_luse[dis_MAX_PREDICATES];
dis_int_pointer dis_lindex[dis_MAX_PREDICATES];
*/

int lp;
int dis_largs[dis_MAX_VARS];



/* for collecting poss. defined fluents
 */
dis_Bool **lf_def;
int **lf_index;

/*
dis_int_pointer lf_def[dis_MAX_FUNCTIONS];
dis_int_pointer lf_index[dis_MAX_FUNCTIONS];
*/

int lf;
int lf_args[dis_MAX_VARS];






void dis_perform_reachability_analysis( void )

{

  int size, i, j, k, adr, num;
  dis_Bool fixpoint;
  dis_Facts *f;
  Normdis_Operator *no;
  dis_EasyTemplate *t1, *t2;
  Normdis_Effect *ne;
  dis_Action *tmp, *a;
  dis_Bool *had_hard_template;
  dis_Pseudodis_Action *pa;
  dis_Pseudodis_Actiondis_Effect *pae;

  dis_gactions = NULL;
  dis_gnum_actions = 0;

   /* Chih-Wei */
  dis_grelevant_facts = (dis_Fact *) calloc(dis_MAX_RELEVANT_FACTS, sizeof(dis_Fact));
  dis_grelevant_fluents = (dis_Fluent *) calloc(dis_MAX_RELEVANT_FLUENTS, sizeof(dis_Fluent));
  dis_grelevant_fluents_name = (dis_Token *) calloc(dis_MAX_RELEVANT_FLUENTS, sizeof(dis_Token));
  dis_grelevant_fluents_lnf = (Lnfdis_ExpNode_pointer *) calloc(dis_MAX_RELEVANT_FLUENTS, 
          sizeof(Lnfdis_ExpNode_pointer));
  dis_lpos = (dis_Bool **) calloc(dis_gnum_predicates, sizeof(dis_Bool *));
  dis_lneg = (dis_Bool **) calloc(dis_gnum_predicates, sizeof(dis_Bool *));
  dis_luse = (dis_Bool **) calloc(dis_gnum_predicates, sizeof(dis_Bool *));
  dis_lindex = (int **) calloc(dis_gnum_predicates, sizeof(int *));
  
  for ( i = 0; i < dis_gnum_predicates; i++ ) {
    size =  1;

    if (dis_gconditional_effects == dis_TRUE || dis_gis_added[i] || dis_gis_deleted[i])
    {
    for ( j = 0; j < dis_garity[i]; j++ ) {
        size *= dis_gtype_size[dis_gpredicates_args_type[i][j]];
        //size *= dis_gnum_constants;
    }
    }

     dis_lpos[i] = ( dis_Bool * ) calloc( size, sizeof( dis_Bool ) );
     dis_lneg[i] = ( dis_Bool * ) calloc( size, sizeof( dis_Bool ) );
     dis_luse[i] = ( dis_Bool * ) calloc( size, sizeof( dis_Bool ) );
             
     /*
    dis_lpos[i] = ( dis_int_pointer ) calloc( size, sizeof( int ) );
    dis_lneg[i] = ( dis_int_pointer ) calloc( size, sizeof( int ) );
    dis_luse[i] = ( dis_int_pointer ) calloc( size, sizeof( int ) );
    */
    dis_lindex[i] = ( dis_int_pointer ) calloc( size, sizeof( int ) );

    for ( j = 0; j < size; j++ ) {
      dis_lpos[i][j] = 0;
      dis_lneg[i][j] = 1;/* all facts but initials are poss. negative */
      dis_luse[i][j] = 0;
      dis_lindex[i][j] = -1;
    }
  }

  had_hard_template = ( dis_Bool * ) calloc( dis_gnum_hard_templates, sizeof( dis_Bool ) );
  for ( i = 0; i < dis_gnum_hard_templates; i++ ) {
    had_hard_template[i] = dis_FALSE;
  }

  /* mark initial facts as possibly positive, not poss. negative
   */
  for ( i = 0; i < dis_gnum_predicates; i++ ) {
    if (dis_gconditional_effects == dis_FALSE && !dis_gis_added[i] && !dis_gis_deleted[i])
      continue;
    lp = i;
    for ( j = 0; j < dis_gnum_initial_predicate[i]; j++ ) {
      for ( k = 0; k < dis_garity[i]; k++ ) {
	dis_largs[k] = dis_ginitial_predicate[i][j].args[k];
      }
      adr = dis_fact_adress();
      dis_lpos[lp][adr] = 1;
      dis_lneg[lp][adr] = 0;
    }
  }

  /* compute fixpoint
   */
  fixpoint = dis_FALSE;
  while ( !fixpoint ) {
    fixpoint = dis_TRUE;

    /* assign next layer of easy templates to possibly positive fixpoint
     */
    t1 = dis_geasy_templates;
    while ( t1 ) {
      no = t1->op;
      for ( i = 0; i < no->num_preconds; i++ ) {
	lp = no->preconds[i].predicate;
	for ( j = 0; j < dis_garity[lp]; j++ ) {
	  dis_largs[j] = ( no->preconds[i].args[j] >= 0 ) ?
	    no->preconds[i].args[j] : t1->inst_table[dis_DECODE_VAR( no->preconds[i].args[j] )];
	}
	if ( !dis_lpos[lp][dis_fact_adress()] ) {
	  break;
	}
      }

      if ( i < no->num_preconds ) {
	t1 = t1->next;
	continue;
      }

      num = 0;
      for ( ne = no->effects; ne; ne = ne->next ) {
	num++;
	/* currently, simply ignore effect conditions and assume
	 * they will all be made true eventually.
	 */
	for ( i = 0; i < ne->num_adds; i++ ) {
	  lp = ne->adds[i].predicate;
	  for ( j = 0; j < dis_garity[lp]; j++ ) {
	    dis_largs[j] = ( ne->adds[i].args[j] >= 0 ) ?
	      ne->adds[i].args[j] : t1->inst_table[dis_DECODE_VAR( ne->adds[i].args[j] )];
	  }
	  adr = dis_fact_adress();
	  if ( !dis_lpos[lp][adr] ) {
	    /* new relevant fact! (added non initial)
	     */
	    dis_lpos[lp][adr] = 1;
	    dis_lneg[lp][adr] = 1;
	    dis_luse[lp][adr] = 1;
	    if ( dis_gnum_relevant_facts == dis_MAX_RELEVANT_FACTS ) {
	      printf("\ntoo many relevant facts! increase dis_MAX_RELEVANT_FACTS (currently %d)\n\n",
		     dis_MAX_RELEVANT_FACTS);
	      exit( 1 );
	    }
	    dis_grelevant_facts[dis_gnum_relevant_facts].predicate = lp;
	    for ( j = 0; j < dis_garity[lp]; j++ ) {
	      dis_grelevant_facts[dis_gnum_relevant_facts].args[j] = dis_largs[j];
	    }
	    dis_lindex[lp][adr] = dis_gnum_relevant_facts;
	    dis_gnum_relevant_facts++;
	    fixpoint = dis_FALSE;
	  }
	}
      }

      tmp = dis_new_dis_Action();
      tmp->norm_operator = no;
      for ( i = 0; i < no->num_vars; i++ ) {
	tmp->inst_table[i] = t1->inst_table[i];
      }
      tmp->name = no->operator->name;
      tmp->num_name_vars = no->operator->number_of_real_params;
      dis_make_name_inst_table_from_Normdis_Operator( tmp, no, t1 );
      tmp->next = dis_gactions;
      tmp->num_effects = num;
      dis_gactions = tmp;
      dis_gnum_actions++;

      t2 = t1->next;
      if ( t1->next ) {
	t1->next->prev = t1->prev;
      }
      if ( t1->prev ) {
	t1->prev->next = t1->next;
      } else {
	dis_geasy_templates = t1->next;
      }
      dis_free_single_dis_EasyTemplate( t1 );
      t1 = t2;
    }

    /* now assign all hard templates that have not been transformed
     * to actions yet.
     */
    for ( i = 0; i < dis_gnum_hard_templates; i++ ) {
      if ( had_hard_template[i] ) {
	continue;
      }
      pa = dis_ghard_templates[i];

      for ( j = 0; j < pa->num_preconds; j++ ) {
	lp = pa->preconds[j].predicate;
	for ( k = 0; k < dis_garity[lp]; k++ ) {
	  dis_largs[k] = pa->preconds[j].args[k];
	}
	if ( !dis_lpos[lp][dis_fact_adress()] ) {
	  break;
	}
      }

      if ( j < pa->num_preconds ) {
	continue;
      }

      for ( pae = pa->effects; pae; pae = pae->next ) {
	/* currently, simply ignore effect conditions and assume
	 * they will all be made true eventually.
	 */
	for ( j = 0; j < pae->num_adds; j++ ) {
	  lp = pae->adds[j].predicate;
	  for ( k = 0; k < dis_garity[lp]; k++ ) {
	    dis_largs[k] = pae->adds[j].args[k];
	  }
	  adr = dis_fact_adress();
	  if ( !dis_lpos[lp][adr] ) {
	    /* new relevant fact! (added non initial)
	     */
	    dis_lpos[lp][adr] = 1;
	    dis_lneg[lp][adr] = 1;
	    dis_luse[lp][adr] = 1;
	    if ( dis_gnum_relevant_facts == dis_MAX_RELEVANT_FACTS ) {
	      printf("\ntoo many relevant facts! increase dis_MAX_RELEVANT_FACTS (currently %d)\n\n",
		     dis_MAX_RELEVANT_FACTS);
	      exit( 1 );
	    }
	    dis_grelevant_facts[dis_gnum_relevant_facts].predicate = lp;
	    for ( k = 0; k < dis_garity[lp]; k++ ) {
	      dis_grelevant_facts[dis_gnum_relevant_facts].args[k] = dis_largs[k];
	    }
	    dis_lindex[lp][adr] = dis_gnum_relevant_facts;
	    dis_gnum_relevant_facts++;
	    fixpoint = dis_FALSE;
	  }
	}
      }

      tmp = dis_new_dis_Action();
      tmp->pseudo_action = pa;
      for ( j = 0; j < pa->operator->num_vars; j++ ) {
	tmp->inst_table[j] = pa->inst_table[j];
      }
      tmp->name = pa->operator->name;
      tmp->num_name_vars = pa->operator->number_of_real_params;
      dis_make_name_inst_table_from_dis_Pseudodis_Action( tmp, pa );
      tmp->next = dis_gactions;
      tmp->num_effects = pa->num_effects;
      dis_gactions = tmp;
      dis_gnum_actions++;

      had_hard_template[i] = dis_TRUE;
    }
  }

  xfree( had_hard_template );

  dis_gnum_pp_facts = dis_gnum_initial + dis_gnum_relevant_facts;

  if ( dis_gcmd_line.display_info == 118 ) {
    printf("\nreachability analysys came up with:");

    printf("\n\npossibly positive facts:");
    for ( f = dis_ginitial; f; f = f->next ) {
      printf("\n");
      dis_print_dis_Fact( f->fact );
    }
    for ( i = 0; i < dis_gnum_relevant_facts; i++ ) {
      printf("\n");
      dis_print_dis_Fact( &(dis_grelevant_facts[i]) );
    }

    printf("\n\nthis yields these %d action templates:", dis_gnum_actions);
    for ( i = 0; i < dis_gnum_operators; i++ ) {
      printf("\n\noperator %s:", dis_goperators[i]->name);
      for ( a = dis_gactions; a; a = a->next ) {
	if ( ( a->norm_operator && 
	       a->norm_operator->operator !=  dis_goperators[i] ) ||
	     ( a->pseudo_action &&
	       a->pseudo_action->operator !=  dis_goperators[i] ) ) {
	  continue;
	}
	printf("\ntemplate: ");
	for ( j = 0; j < dis_goperators[i]->number_of_real_params; j++ ) {
	  printf("%s", dis_gconstants[a->name_inst_table[j]]);
	  if ( j < dis_goperators[i]->num_vars-1 ) {
	    printf(" ");
	  }
	}
      }
    }
    printf("\n\n");
  }

}

int dis_tok_ft_argncmp(int f, int n, char *tok)
{       
    if(dis_garity[dis_grelevant_facts[f].predicate] <= n) return -1;
    return strcmp( (char *)dis_gconstants[dis_grelevant_facts[f].args[n]],tok);
}       
              
int dis_tok_ef_argncmp(int f, int n, char *tok)
{       
    if((dis_gop_conn[f].action)->num_name_vars <= n) return -1;
    return strcmp( (char *)dis_gconstants[(dis_gop_conn[f].action)->name_inst_table[n]]
            ,tok);
}       
int dis_ft_argncmp(int f, int g, int n, int m)
{   
    if(dis_garity[dis_grelevant_facts[f].predicate] <= n) return -1;
    if(dis_garity[dis_grelevant_facts[g].predicate] <= m) return -1;
    return strcmp( (char *)dis_gconstants[dis_grelevant_facts[f].args[n]],
                    (char *)dis_gconstants[dis_grelevant_facts[g].args[m]]);
}      
        
int dis_op_argncmp(int f, int g, int n, int m)
{                         
    if((dis_gop_conn[f].action)->num_name_vars <= n) return -1;
    if((dis_gop_conn[g].action)->num_name_vars <= m) return -1;
    return strcmp( (char*)dis_gconstants[(dis_gop_conn[f].action)->name_inst_table[n]],
           (char *)dis_gconstants[(dis_gop_conn[g].action)->name_inst_table[m]]);
}
    
int dis_ft_arity(int f)
{              
    return dis_garity[dis_grelevant_facts[f].predicate];
}   

int dis_ef_arity(int f) 
{           
    return (dis_gop_conn[f].action)->num_name_vars;
}       

char* dis_ft_argn(int f, int n)
{
    return (char *)dis_gconstants[dis_grelevant_facts[f].args[n]];
}

char* dis_ef_argn(int f, int n)
{
    return (char*)dis_gconstants[(dis_gop_conn[f].action)->name_inst_table[n]];
}

int dis_ef_predlen(int f)
{
    return strlen((char *)((dis_gop_conn[f].action)->name));
}   
        
int dis_ft_predlen(int f)
{                   
    return strlen((char *)dis_gpredicates[dis_grelevant_facts[f].predicate]);
}                           

int dis_fl_predlen(int f)
{                   
    return strlen((char *)dis_grelevant_fluents_name[f]);
}                           



int dis_fact_adress( void )

{

  int r = 0, b = 1, i;

    /* Chih-Wei */
  for ( i = dis_garity[lp] - 1; i > -1; i-- ) {
      //r += b * const_index[dis_largs[i]];
      r += b * const_index[dis_largs[i]][dis_gpredicates_args_type[lp][i]];
      b *= dis_gtype_size[dis_gpredicates_args_type[lp][i]];
  }
    
  
  /*
  for ( i = dis_garity[lp] - 1; i > -1; i-- ) {
    r += b * dis_largs[i];
    b *= dis_gnum_constants;
  }*/

  return r;

}



int fluent_adress( void )

{

  int r = 0, b = 1, i;

    
  /* Chih-Wei */
  for ( i = dis_gf_arity[lf] - 1; i > -1; i-- ) {
     // r += b * const_index[lf_args[i]]; 
      r += b * const_index[lf_args[i]][dis_gfunctions_args_type[lf][i]];
      b *= dis_gtype_size[dis_gfunctions_args_type[lf][i]];
  } 
    
  
  
  /*
  for ( i = dis_gf_arity[lf] - 1; i > -1; i-- ) {
    r += b * lf_args[i];
    b *= dis_gnum_constants;
  }*/

  return r;

}



void dis_make_name_inst_table_from_Normdis_Operator( dis_Action *a, Normdis_Operator *o, dis_EasyTemplate *t )

{

  int i, r = 0, m = 0;

  for ( i = 0; i < o->operator->number_of_real_params; i++ ) {
    if ( o->num_removed_vars > r &&
	 o->removed_vars[r] == i ) {
      /* this var has been removed in NormOp;
       * insert type constraint constant
       *
       * at least one there, as empty typed pars ops are removed
       */
      a->name_inst_table[i] = dis_gtype_consts[o->type_removed_vars[r]][0];
      r++;
    } else {
      /* this par corresponds to par m  in NormOp
       */
      a->name_inst_table[i] = t->inst_table[m];
      m++;
    }
  }

}



void dis_make_name_inst_table_from_dis_Pseudodis_Action( dis_Action *a, dis_Pseudodis_Action *pa )

{

  int i;

  for ( i = 0; i < pa->operator->number_of_real_params; i++ ) {
    a->name_inst_table[i] = pa->inst_table[i];
  }

}


















/***********************************************************
 * RELEVANCE ANALYSIS dis_AND FINAL DOMAIN dis_AND PROBLEM CLEANUP *
 ***********************************************************/









/* counts effects for later allocation
 */
int lnum_effects;









void dis_collect_relevant_facts_and_fluents( void )

{

  dis_Action *a;
  Normdis_Operator *no;
  Normdis_Effect *ne;
  int i, j, adr, size;
  dis_Pseudodis_Action *pa;
  dis_Pseudodis_Actiondis_Effect *pae;
  dis_FluentValues *fvs;

  
    /* facts: mark all deleted facts; such facts, that are also pos, are relevant.
   */
  for ( a = dis_gactions; a; a = a->next ) {
    if ( a->norm_operator ) {
      no = a->norm_operator;

      for ( ne = no->effects; ne; ne = ne->next ) {
	for ( i = 0; i < ne->num_dels; i++ ) {
	  lp = ne->dels[i].predicate;
	  for ( j = 0; j < dis_garity[lp]; j++ ) {
	    dis_largs[j] = ( ne->dels[i].args[j] >= 0 ) ?
	      ne->dels[i].args[j] : a->inst_table[dis_DECODE_VAR( ne->dels[i].args[j] )];
	  }
	  adr = dis_fact_adress();

	  dis_lneg[lp][adr] = 1;
	  if ( dis_lpos[lp][adr] &&
	       !dis_luse[lp][adr] ) {
	    dis_luse[lp][adr] = 1;
	    dis_lindex[lp][adr] = dis_gnum_relevant_facts;
	    if ( dis_gnum_relevant_facts == dis_MAX_RELEVANT_FACTS ) {
	      printf("\nincrease dis_MAX_RELEVANT_FACTS! (current value: %d)\n\n",
		     dis_MAX_RELEVANT_FACTS);
	      exit( 1 );
	    }
	    dis_grelevant_facts[dis_gnum_relevant_facts].predicate = lp;
	    for ( j = 0; j < dis_garity[lp]; j++ ) {
	      dis_grelevant_facts[dis_gnum_relevant_facts].args[j] = dis_largs[j];
	    }
	    dis_lindex[lp][adr] = dis_gnum_relevant_facts;
	    dis_gnum_relevant_facts++;
	  }
	}
      }
    } else {
      pa = a->pseudo_action;

      for ( pae = pa->effects; pae; pae = pae->next ) {
	for ( i = 0; i < pae->num_dels; i++ ) {
	  lp = pae->dels[i].predicate;
	  for ( j = 0; j < dis_garity[lp]; j++ ) {
	    dis_largs[j] = pae->dels[i].args[j];
	  }
	  adr = dis_fact_adress();

	  dis_lneg[lp][adr] = 1;
	  if ( dis_lpos[lp][adr] &&
	       !dis_luse[lp][adr] ) {
	    dis_luse[lp][adr] = 1;
	    dis_lindex[lp][adr] = dis_gnum_relevant_facts;
	    if ( dis_gnum_relevant_facts == dis_MAX_RELEVANT_FACTS ) {
	      printf("\nincrease dis_MAX_RELEVANT_FACTS! (current value: %d)\n\n",
		     dis_MAX_RELEVANT_FACTS);
	      exit( 1 );
	    }
	    dis_grelevant_facts[dis_gnum_relevant_facts].predicate = lp;
	    for ( j = 0; j < dis_garity[lp]; j++ ) {
	      dis_grelevant_facts[dis_gnum_relevant_facts].args[j] = dis_largs[j];
	    }
	    dis_lindex[lp][adr] = dis_gnum_relevant_facts;
	    dis_gnum_relevant_facts++;
	  }
	}
      }
    }
  }
  /* fluents: collect all that are defined in initial state, plus
   * all that are assigned to by an effect of an action
   * (i.e. preconds poss. pos. due to reachability)
   *
   * first initialise fast access structures
   */
   /* Chih-Wei */
  lf_def = (dis_Bool **) calloc(dis_gnum_functions, sizeof(dis_Bool *)); 
  lf_index = (int **) calloc(dis_gnum_functions, sizeof(int *));
          
  
  for ( i = 0; i < dis_gnum_functions; i++ ) {
    size =  1;
    for ( j = 0; j < dis_gf_arity[i]; j++ ) {
      //size *= dis_gnum_constants;
        size *= dis_gtype_size[dis_gfunctions_args_type[i][j]];
    }
// Chih-Wei PDDL3 TOTAL-TIME
    if (!dis_gis_changed[i] && size > 1000000)
      size = 1;
    //lf_def[i] = ( dis_int_pointer ) calloc( size, sizeof( int ) );
    lf_def[i] = ( dis_Bool * ) calloc( size, sizeof( dis_Bool ) );
    lf_index[i] = ( dis_int_pointer ) calloc( size, sizeof( int ) );
    for ( j = 0; j < size; j++ ) {
      lf_def[i][j] = 0;
      lf_index[i][j] = -1;
    }
  }
  /* from initial state, only those that are not static.
   */
  for ( fvs = dis_gf_initial; fvs; fvs = fvs->next ) {
    lf = fvs->fluent.function;
    for ( j = 0; j < dis_gf_arity[lf]; j++ ) {
      lf_args[j] = fvs->fluent.args[j];
    }
    adr = fluent_adress();
    if ( !lf_def[lf][adr] ) {
      lf_def[lf][adr] = 1;
      if ( dis_gnum_relevant_fluents == dis_MAX_RELEVANT_FLUENTS ) {
	printf("\ntoo many relevant fluents! increase dis_MAX_RELEVANT_FLUENTS (currently %d)\n\n",
	       dis_MAX_RELEVANT_FLUENTS);
	exit( 1 );
      }
      dis_grelevant_fluents[dis_gnum_relevant_fluents].function = lf;
      size = strlen(dis_gfunctions[lf]) + 1;
      for (j=0;j<dis_gf_arity[lf];j++)
        size += (1+ strlen(dis_gconstants[lf_args[j]]));
      dis_grelevant_fluents_name[dis_gnum_relevant_fluents] = 
	( char * ) calloc( size, sizeof( char ) );
      strcpy( dis_grelevant_fluents_name[dis_gnum_relevant_fluents], dis_gfunctions[lf] );
      for ( j = 0; j < dis_gf_arity[lf]; j++ ) {
	dis_grelevant_fluents[dis_gnum_relevant_fluents].args[j] = lf_args[j];
	strcat( dis_grelevant_fluents_name[dis_gnum_relevant_fluents], "_" );
	strcat( dis_grelevant_fluents_name[dis_gnum_relevant_fluents], dis_gconstants[lf_args[j]] );
      }
      lf_index[lf][adr] = dis_gnum_relevant_fluents;
      dis_gnum_relevant_fluents++;
    } else {
      printf("\n\nfluent ");
      dis_print_dis_Fluent( &(fvs->fluent) );
      printf(" defined twice in initial state! check input files\n\n");
      exit( 1 );
    }
  }
  /* from actions, all assigns (are non-static anyway)
   */
  for ( a = dis_gactions; a; a = a->next ) {
    if ( a->norm_operator ) {
      no = a->norm_operator;
      for ( ne = no->effects; ne; ne = ne->next ) {
	for ( i = 0; i < ne->num_numeric_effects; i++ ) {
	  if ( ne->numeric_effects_neft[i] != ASSIGN ) continue;
	  lf = ne->numeric_effects_fluent[i].function;
	  for ( j = 0; j < dis_gf_arity[lf]; j++ ) {
	    lf_args[j] = ( ne->numeric_effects_fluent[i].args[j] >= 0 ) ?
	      ne->numeric_effects_fluent[i].args[j] : 
	      a->inst_table[dis_DECODE_VAR( ne->numeric_effects_fluent[i].args[j] )];
	  }
	  adr = fluent_adress();
	  if ( !lf_def[lf][adr] ) {
	    lf_def[lf][adr] = 1;
	    if ( dis_gnum_relevant_fluents == dis_MAX_RELEVANT_FLUENTS ) {
	      printf("\ntoo many relevant fluents! increase dis_MAX_RELEVANT_FLUENTS (currently %d)\n\n",
		     dis_MAX_RELEVANT_FLUENTS);
	      exit( 1 );
	    }
	    dis_grelevant_fluents[dis_gnum_relevant_fluents].function = lf;
            size = strlen(dis_gfunctions[lf]) + 1;
            for (j=0;j<dis_gf_arity[lf];j++)
              size += (1+ strlen(dis_gconstants[lf_args[j]]));
	    dis_grelevant_fluents_name[dis_gnum_relevant_fluents] = 
	      ( char * ) calloc( size, sizeof( char ) );
	    strcpy( dis_grelevant_fluents_name[dis_gnum_relevant_fluents], dis_gfunctions[lf] );
	    for ( j = 0; j < dis_gf_arity[lf]; j++ ) {
	      dis_grelevant_fluents[dis_gnum_relevant_fluents].args[j] = lf_args[j];
	      strcat( dis_grelevant_fluents_name[dis_gnum_relevant_fluents], "_" );
	      strcat( dis_grelevant_fluents_name[dis_gnum_relevant_fluents], dis_gconstants[lf_args[j]] );
	    }
	    lf_index[lf][adr] = dis_gnum_relevant_fluents;
	    dis_gnum_relevant_fluents++;
	  }
	}
      }
    } else {
      pa = a->pseudo_action;
      for ( pae = pa->effects; pae; pae = pae->next ) {
	for ( i = 0; i < pae->num_numeric_effects; i++ ) {
	  if ( pae->numeric_effects_neft[i] != ASSIGN ) continue;
	  lf = pae->numeric_effects_fluent[i].function;
	  for ( j = 0; j < dis_gf_arity[lf]; j++ ) {
	    lf_args[j] = ( pae->numeric_effects_fluent[i].args[j] >= 0 ) ?
	      pae->numeric_effects_fluent[i].args[j] : 
	      a->inst_table[dis_DECODE_VAR( pae->numeric_effects_fluent[i].args[j] )];
	  }
	  adr = fluent_adress();
	  if ( !lf_def[lf][adr] ) {
	    lf_def[lf][adr] = 1;
	    if ( dis_gnum_relevant_fluents == dis_MAX_RELEVANT_FLUENTS ) {
	      printf("\ntoo many relevant fluents! increase dis_MAX_RELEVANT_FLUENTS (currently %d)\n\n",
		     dis_MAX_RELEVANT_FLUENTS);
	      exit( 1 );
	    }
	    dis_grelevant_fluents[dis_gnum_relevant_fluents].function = lf;
            size = strlen(dis_gfunctions[lf]) + 1;
            for (j=0;j<dis_gf_arity[lf];j++)
              size += (1+ strlen(dis_gconstants[lf_args[j]]));
	    dis_grelevant_fluents_name[dis_gnum_relevant_fluents] = 
	      ( char * ) calloc( size, sizeof( char ) );
	    strcpy( dis_grelevant_fluents_name[dis_gnum_relevant_fluents], dis_gfunctions[lf] );
	    for ( j = 0; j < dis_gf_arity[lf]; j++ ) {
	      dis_grelevant_fluents[dis_gnum_relevant_fluents].args[j] = lf_args[j];
	      strcat( dis_grelevant_fluents_name[dis_gnum_relevant_fluents], "_" );
	      strcat( dis_grelevant_fluents_name[dis_gnum_relevant_fluents], dis_gconstants[lf_args[j]] );
	    }
	    lf_index[lf][adr] = dis_gnum_relevant_fluents;
	    dis_gnum_relevant_fluents++;
	  }
	}
      }
    }
  }

  if ( dis_gcmd_line.display_info == 119 ) {
    printf("\n\nfacts selected as relevant:");
    for ( i = 0; i < dis_gnum_relevant_facts; i++ ) {
      printf("\n%d: ", i);
      dis_print_dis_Fact( &(dis_grelevant_facts[i]) );
    }
    printf("\n\nfluents selected as relevant:");
    for ( i = 0; i < dis_gnum_relevant_fluents; i++ ) {
      printf("\n%d: ", i);
      dis_print_dis_Fluent( &(dis_grelevant_fluents[i]) );
    }    
    printf("\n\n");
  }

  lnum_effects = 0;

  dis_create_final_goal_state();
  dis_create_final_initial_state();
  dis_create_final_actions();

  if ( dis_gmetric != NULL ) {
    if ( !dis_set_relevants_in_exp( &dis_gmetric ) ) {
      if ( dis_gcmd_line.display_info ) {
	printf("\nwarning: undefined fluent used in optimization expression. defaulting to plan length");
      }
      dis_free_dis_ExpNode( dis_gmetric );
      dis_gmetric = NULL;      
    }
  }

  if ( dis_gcmd_line.display_info == 120 ) {
    printf("\n\nfinal domain representation is:\n\n");  

    for ( i = 0; i < dis_gnum_operators; i++ ) {
      printf("\n\n------------------operator %s-----------\n\n", dis_goperators[i]->name);
      for ( a = dis_gactions; a; a = a->next ) {
	if ( ( !a->norm_operator &&
	       !a->pseudo_action ) ||
	     ( a->norm_operator && 
	       a->norm_operator->operator != dis_goperators[i] ) ||
	     ( a->pseudo_action &&
	       a->pseudo_action->operator != dis_goperators[i] ) ) {
	  continue;
	}
	dis_print_dis_Action( a );
      }
    }
    printf("\n\n--------------------GOAL REACHED ops-----------\n\n");
    for ( a = dis_gactions; a; a = a->next ) {
      if ( !a->norm_operator &&
	   !a->pseudo_action ) {
	dis_print_dis_Action( a );
      }
    }

    printf("\n\nfinal initial state is:\n\n");
    dis_print_dis_State( dis_ginitial_state );

    printf("\n\nfinal goal is:\n\n");
    for ( i = 0; i < dis_gnum_logic_goal; i++ ) {
      dis_print_ft_name( dis_glogic_goal[i] );
      printf("\n");
    }
    for ( i = 0; i < dis_gnum_numeric_goal; i++ ) {
      switch ( dis_gnumeric_goal_comp[i] ) {
      case LE:
	printf("(< ");
	break;
      case LEQ:
	printf("(<= ");
	break;
      case EQ:
	printf("(= ");
	break;
      case GEQ:
	printf("(>= ");
	break;
      case GE:
	printf("(> ");
	break;
      default:
	printf("\nwrong comparator in gnumeric_goal %d\n\n", dis_gnumeric_goal_comp[i]);
	exit( 1 );
      }
      dis_print_dis_ExpNode( dis_gnumeric_goal_lh[i] );
      dis_print_dis_ExpNode( dis_gnumeric_goal_rh[i] );
      printf(")\n");
    }

    if ( dis_gmetric ) {
      printf("\n\nmetric is (minimize):\n");
      dis_print_dis_ExpNode( dis_gmetric );
    } else {
      printf("\n\nmetric: none, i.e. plan length\n");
    }
  }

  /* Chih-Wei */
  for (i=0;i<dis_gnum_predicates;i++)
  {
      xfree(dis_lpos[i]);
      xfree(dis_lneg[i]);
      xfree(dis_luse[i]);
      xfree(dis_lindex[i]);
  }
  xfree(dis_lpos);
  xfree(dis_lneg);
  xfree(dis_luse);
  xfree(dis_lindex);
  for ( i = 0; i < dis_gnum_functions; i++ )
  {
      xfree(lf_def[i]);
      xfree(lf_index[i]);
  }
  xfree(lf_def);
  xfree(lf_index);

}



void dis_create_final_goal_state( void )

{

  dis_WffNode *w, *ww;
  int m, mn, i, adr;
  dis_Action *tmp;

  if ( !dis_set_relevants_in_wff( &dis_ggoal ) ) {
    printf("\n\nff: goal accesses a fluent that will never have a defined value. Problem unsolvable.\n\n");
    exit( 1 );
  }
  dis_cleanup_wff( &dis_ggoal );

// Chih-Wei PDDL3
/*  if ( dis_ggoal->connective == dis_TRU ) {
    printf("\nff: goal can be simplified to dis_TRUE. The empty plan solves it\n\n");
    dis_gnum_plan_ops = 0;
    dis_print_official_result();
    exit( 1 );
  }*/
  if ( dis_ggoal->connective == dis_FAL ) {
    printf("\ncreate ff: goal can be simplified to dis_FALSE. No plan will solve it\n\n");
    exit( 1 );
  }

  switch ( dis_ggoal->connective ) {
// Chih-Wei PDDL3
  case dis_TRU:
    break;
  case dis_OR:
    if ( dis_gnum_relevant_facts == dis_MAX_RELEVANT_FACTS ) {
      printf("\nincrease dis_MAX_RELEVANT_FACTS! (current value: %d)\n\n",
	     dis_MAX_RELEVANT_FACTS);
      exit( 1 );
    }
    dis_grelevant_facts[dis_gnum_relevant_facts].predicate = -3;
    dis_gnum_relevant_facts++;
    for ( w = dis_ggoal->sons; w; w = w->next ) {
      tmp = dis_new_dis_Action();
      if ( w->connective == dis_AND ) {
	m = 0; mn = 0;
	for ( ww = w->sons; ww; ww = ww->next ) {
	  if ( ww->connective == dis_ATOM ) m++;
	  if ( ww->connective == dis_COMP ) mn++;
	}
	tmp->preconds = ( int * ) calloc( m, sizeof( int ) );
	tmp->numeric_preconds_comp = ( dis_Comparator * ) calloc( mn, sizeof( dis_Comparator ) );
	tmp->numeric_preconds_lh = ( dis_ExpNode_pointer * ) calloc( mn, sizeof( dis_ExpNode_pointer ) );
	tmp->numeric_preconds_rh = ( dis_ExpNode_pointer * ) calloc( mn, sizeof( dis_ExpNode_pointer ) );
	tmp->num_preconds = m;
	tmp->num_numeric_preconds = mn;
	m = 0; mn = 0;
	for ( ww = w->sons; ww; ww = ww->next ) {
	  if ( ww->connective == dis_ATOM ) {
	    lp = ww->fact->predicate;
	    for ( i = 0; i < dis_garity[lp]; i++ ) {
	      dis_largs[i] = ww->fact->args[i];
	    }
	    adr = dis_fact_adress();
	    tmp->preconds[m] = dis_lindex[lp][adr];
	    m++;
	  }
	  if ( ww->connective == dis_COMP ) {
	    tmp->numeric_preconds_comp[mn] = ww->comp;
	    tmp->numeric_preconds_lh[mn] = dis_copy_Exp( ww->lh );
	    tmp->numeric_preconds_rh[mn] = dis_copy_Exp( ww->rh );	    
	    mn++;
	  }
	}
      } else {
	if ( w->connective == dis_ATOM ) {
	  tmp->preconds = ( int * ) calloc( 1, sizeof( int ) );
	  tmp->num_preconds = 1;
	  lp = w->fact->predicate;
	  for ( i = 0; i < dis_garity[lp]; i++ ) {
	    dis_largs[i] = w->fact->args[i];
	  }
	  adr = dis_fact_adress();
	  tmp->preconds[0] = dis_lindex[lp][adr];
	}
	if ( w->connective == dis_COMP ) {
	  tmp->numeric_preconds_comp = ( dis_Comparator * ) calloc( 1, sizeof( dis_Comparator ) );
	  tmp->numeric_preconds_lh = ( dis_ExpNode_pointer * ) calloc( 1, sizeof( dis_ExpNode_pointer ) );
	  tmp->numeric_preconds_rh = ( dis_ExpNode_pointer * ) calloc( 1, sizeof( dis_ExpNode_pointer ) );
	  tmp->numeric_preconds_comp[0] = w->comp;
	  tmp->numeric_preconds_lh[0] = dis_copy_Exp( w->lh );
	  tmp->numeric_preconds_rh[0] = dis_copy_Exp( w->rh );
	  tmp->num_numeric_preconds = 1;
	}
      }
      tmp->effects = ( dis_Actiondis_Effect * ) calloc( 1, sizeof( dis_Actiondis_Effect ) );
      tmp->num_effects = 1;
      tmp->effects[0].conditions = NULL;
      tmp->effects[0].num_conditions = 0;
      tmp->effects[0].dels = NULL;
      tmp->effects[0].num_dels = 0;
      tmp->effects[0].adds = ( int * ) calloc( 1, sizeof( int ) );
      tmp->effects[0].adds[0] = dis_gnum_relevant_facts - 1;
      tmp->effects[0].num_adds = 1;
      tmp->effects[0].numeric_conditions_comp = NULL;
      tmp->effects[0].numeric_conditions_lh = NULL;
      tmp->effects[0].numeric_conditions_rh = NULL;
      tmp->effects[0].num_numeric_conditions = 0;
      tmp->effects[0].numeric_effects_neft = NULL;
      tmp->effects[0].numeric_effects_fl = NULL;
      tmp->effects[0].numeric_effects_rh = NULL;
      tmp->effects[0].num_numeric_effects = 0;

      tmp->next = dis_gactions;
      dis_gactions = tmp;
      dis_gnum_actions++;
      lnum_effects++;
    }
    dis_glogic_goal = ( int * ) calloc( 1, sizeof( int ) );
    dis_glogic_goal[0] = dis_gnum_relevant_facts - 1;
    dis_gnum_logic_goal = 1;
    break;
  case dis_AND:
    m = 0; mn = 0;
    for ( w = dis_ggoal->sons; w; w = w->next ) {
      if ( w->connective == dis_ATOM ) m++;
      if ( w->connective == dis_COMP ) mn++;
    }
    dis_glogic_goal = ( int * ) calloc( m, sizeof( int ) );
    dis_gnumeric_goal_comp = ( dis_Comparator * ) calloc( mn, sizeof( dis_Comparator ) );
    dis_gnumeric_goal_lh = ( dis_ExpNode_pointer * ) calloc( mn, sizeof( dis_ExpNode_pointer ) );
    dis_gnumeric_goal_rh = ( dis_ExpNode_pointer * ) calloc( mn, sizeof( dis_ExpNode_pointer ) );
    dis_gnum_logic_goal = m;
    dis_gnum_numeric_goal = mn;
    m = 0; mn = 0;
    for ( w = dis_ggoal->sons; w; w = w->next ) {
      if ( w->connective == dis_ATOM ) {
	lp = w->fact->predicate;
	for ( i = 0; i < dis_garity[lp]; i++ ) {
	  dis_largs[i] = w->fact->args[i];
	}
	adr = dis_fact_adress();
	dis_glogic_goal[m] = dis_lindex[lp][adr];
	m++;
      }
      if ( w->connective == dis_COMP ) {
	dis_gnumeric_goal_comp[mn] = w->comp;
	dis_gnumeric_goal_lh[mn] = dis_copy_Exp( w->lh ); 
	dis_gnumeric_goal_rh[mn] = dis_copy_Exp( w->rh );
	mn++;
      }
    }
    break;
  case dis_ATOM:
    dis_glogic_goal = ( int * ) calloc( 1, sizeof( int ) );
    dis_gnum_logic_goal = 1;
    lp = dis_ggoal->fact->predicate;
    for ( i = 0; i < dis_garity[lp]; i++ ) {
      dis_largs[i] = dis_ggoal->fact->args[i];
    }
    adr = dis_fact_adress();
    dis_glogic_goal[0] = dis_lindex[lp][adr];
    break;
  case dis_COMP:
    dis_gnumeric_goal_comp = ( dis_Comparator * ) calloc( 1, sizeof( dis_Comparator ) );
    dis_gnumeric_goal_lh = ( dis_ExpNode_pointer * ) calloc( 1, sizeof( dis_ExpNode_pointer ) );
    dis_gnumeric_goal_rh = ( dis_ExpNode_pointer * ) calloc( 1, sizeof( dis_ExpNode_pointer ) );
    dis_gnum_numeric_goal = 1;
    dis_gnumeric_goal_comp[0] = dis_ggoal->comp;
    dis_gnumeric_goal_lh[0] = dis_copy_Exp( dis_ggoal->lh ); 
    dis_gnumeric_goal_rh[0] = dis_copy_Exp( dis_ggoal->rh );
    break;
  default:
    printf("\n\nwon't get here: non dis_COMP,dis_ATOM,dis_AND,dis_OR in fully simplified goal\n\n");
    exit( 1 );
  }

}



dis_Bool dis_set_relevants_in_wff( dis_WffNode **w )

{

  dis_WffNode *i;
  int j, adr;

  switch ( (*w)->connective ) {
// Chih-Wei PDDL3
  case dis_TRU:
    break;
  case dis_AND:
  case dis_OR:
    for ( i = (*w)->sons; i; i = i->next ) {
      if ( !dis_set_relevants_in_wff( &i ) ) {
	return dis_FALSE;
      }
    }
    break;
  case dis_ATOM:
    /* no equalities, as fully instantiated
     */
    lp = (*w)->fact->predicate;
    for ( j = 0; j < dis_garity[lp]; j++ ) {
      dis_largs[j] = (*w)->fact->args[j];
    }
    adr = dis_fact_adress();

    if ( !dis_lneg[lp][adr] ) {
      (*w)->connective = dis_TRU;
      xfree( (*w)->fact );
      (*w)->fact = NULL;
      break;
    }
    if ( !dis_lpos[lp][adr] ) {
      (*w)->connective = dis_FAL;
      xfree( (*w)->fact );
      (*w)->fact = NULL;
      break;
    }
    break;
  case dis_COMP:
    if ( !dis_set_relevants_in_exp( &((*w)->lh) ) ||
	 !dis_set_relevants_in_exp( &((*w)->rh) ) ) {
      return dis_FALSE;
    }
    break;
  default:
    printf("\n\nwon't get here: non dis_ATOM,dis_OR,dis_AND in goal set relevants\n\n");
    exit( 1 );
  }

  return dis_TRUE;

}



dis_Bool dis_set_relevants_in_exp( dis_ExpNode **n )

{

  int j, adr;

  /* can probably (for sure) forget about the simplification
   * stuff here because it's been done before.
   *
   * igual....
   */
  switch ( (*n)->connective ) {
  case AD:
    if ( !dis_set_relevants_in_exp( &((*n)->leftson) ) ) return dis_FALSE;
    if ( !dis_set_relevants_in_exp( &((*n)->rightson) ) ) return dis_FALSE;
    if ( (*n)->leftson->connective != NUMBER ||
	 (*n)->rightson->connective != NUMBER ) {
      break;
    }
    (*n)->connective = NUMBER;
    (*n)->value = (*n)->leftson->value + (*n)->rightson->value;
    dis_free_dis_ExpNode( (*n)->leftson );
    (*n)->leftson = NULL;
    dis_free_dis_ExpNode( (*n)->rightson );
    (*n)->rightson = NULL;
    break;
  case SU:
    if ( !dis_set_relevants_in_exp( &((*n)->leftson) ) ) return dis_FALSE;
    if ( !dis_set_relevants_in_exp( &((*n)->rightson) ) ) return dis_FALSE;
    if ( (*n)->leftson->connective != NUMBER ||
	 (*n)->rightson->connective != NUMBER ) {
      break;
    }
    (*n)->connective = NUMBER;
    (*n)->value = (*n)->leftson->value - (*n)->rightson->value;
    dis_free_dis_ExpNode( (*n)->leftson );
    (*n)->leftson = NULL;
    dis_free_dis_ExpNode( (*n)->rightson );
    (*n)->rightson = NULL;
    break;
  case MU:
    if ( !dis_set_relevants_in_exp( &((*n)->leftson) ) ) return dis_FALSE;
    if ( !dis_set_relevants_in_exp( &((*n)->rightson) ) ) return dis_FALSE;
    if ( (*n)->leftson->connective != NUMBER ||
	 (*n)->rightson->connective != NUMBER ) {
      break;
    }
    (*n)->connective = NUMBER;
    (*n)->value = (*n)->leftson->value * (*n)->rightson->value;
    dis_free_dis_ExpNode( (*n)->leftson );
    (*n)->leftson = NULL;
    dis_free_dis_ExpNode( (*n)->rightson );
    (*n)->rightson = NULL;
    break;
  case DI:
    if ( !dis_set_relevants_in_exp( &((*n)->leftson) ) ) return dis_FALSE;
    if ( !dis_set_relevants_in_exp( &((*n)->rightson) ) ) return dis_FALSE;
    if ( (*n)->leftson->connective != NUMBER ||
	 (*n)->rightson->connective != NUMBER ) {
      break;
    }
    if ( (*n)->rightson->value == 0 ) {
      /* kind of unclean: simply leave that in here;
       * we will later determine the right thing 
       * to do with it.
       */
      break;
    }
    (*n)->connective = NUMBER;
    (*n)->value = (*n)->leftson->value / (*n)->rightson->value;
    dis_free_dis_ExpNode( (*n)->leftson );
    (*n)->leftson = NULL;
    dis_free_dis_ExpNode( (*n)->rightson );
    (*n)->rightson = NULL;
    break;
  case MINUS:
    if ( !dis_set_relevants_in_exp( &((*n)->son) ) ) return dis_FALSE;
    if ( (*n)->son->connective != NUMBER ) break;
    (*n)->connective = NUMBER;
    (*n)->value = ((float) (-1)) * (*n)->son->value;
    dis_free_dis_ExpNode( (*n)->son );
    (*n)->son = NULL;
    break;    
  case NUMBER:
    break;
  case FHEAD:
    lf = (*n)->fluent->function;
    for ( j = 0; j < dis_gf_arity[lf]; j++ ) {
      lf_args[j] = (*n)->fluent->args[j];
    }
    adr = fluent_adress();
    (*n)->fl = lf_index[lf][adr];
    xfree( (*n)->fluent );
    (*n)->fluent = NULL;
    if ( lf_index[lf][adr] == -1 ) {
      if ( lf == 0 ) {
	/* ATTENTION!! FUNCTION 0 IS TOTAL-dis_TIME WHICH IS *ONLY* USED
	 * IN OPTIMIZATION dis_EXPRESSION. GETS A SPECIAL TREATMENT
	 * IN THE RESPECTIVE FUNCTION IN SEARCH.C!!!!
	 *
	 * we remember it as fluent -2!!
	 */
	(*n)->fl = -2;
      } else {
	return dis_FALSE;
      }
    }
    break;
  default:
    printf("\n\nset relevants in expnode: wrong specifier %d",
	   (*n)->connective);
    exit( 1 );
  }

  return dis_TRUE;

}



void dis_create_final_initial_state( void )

{

  dis_Facts *f;
  int i, adr, fl;
  dis_FluentValues *fvs;

  i = 0;
  for ( f = dis_ginitial; f; f = f->next ) i++;
  /* we need space for transformation fluents to come!
   */
  //dis_make_state( &dis_ginitial_state, i, dis_MAX_RELEVANT_FLUENTS );
  dis_make_state( &dis_ginitial_state, dis_gnum_relevant_facts, 
          dis_MAX_RELEVANT_FLUENTS );
                                        
  
  for ( f = dis_ginitial; f; f = f->next ) {
    lp = f->fact->predicate;
    for ( i = 0; i < dis_garity[lp]; i++ ) {
      dis_largs[i] = f->fact->args[i];
    }
    adr = dis_fact_adress();
    if ( !dis_lneg[lp][adr]) {/* non deleted ini */
      continue;
    }
    dis_ginitial_state.F[dis_ginitial_state.num_F++] = dis_lindex[lp][adr];
  }

  for ( fvs = dis_gf_initial; fvs; fvs = fvs->next ) {
    lf = fvs->fluent.function;
    for ( i = 0; i < dis_gf_arity[lf]; i++ ) {
      lf_args[i] = fvs->fluent.args[i];
    }
    adr = fluent_adress();
    fl = lf_index[lf][adr];
    dis_ginitial_state.f_D[fl] = dis_TRUE;
    dis_ginitial_state.f_V[fl] = fvs->value;
  }

}



void dis_create_final_actions( void )

{

  dis_Action *a, *p, *t;
  Normdis_Operator *no;
  Normdis_Effect *ne;
  int i, j, adr;
  dis_Pseudodis_Action *pa;
  dis_Pseudodis_Actiondis_Effect *pae;
  dis_Actiondis_Effect *aa;
  dis_Bool false_cond;

  a = dis_gactions; p = NULL;
  while ( a ) {
    if ( a->norm_operator ) {
      /* action comes from an easy template NormOp
       */
      no = a->norm_operator;

      if ( no->num_preconds > 0 ) {
	a->preconds = ( int * ) calloc( no->num_preconds, sizeof( int ) );
      }
      a->num_preconds = 0;
      for ( i = 0; i < no->num_preconds; i++ ) {
	lp = no->preconds[i].predicate;
	for ( j = 0; j < dis_garity[lp]; j++ ) {
	  dis_largs[j] = ( no->preconds[i].args[j] >= 0 ) ?
	    no->preconds[i].args[j] : a->inst_table[dis_DECODE_VAR( no->preconds[i].args[j] )];
	}
	adr = dis_fact_adress();	
	/* preconds are dis_lpos in all cases due to reachability analysis
	 */
	if ( !dis_lneg[lp][adr] ) {
	  continue;
	}
	a->preconds[a->num_preconds++] = dis_lindex[lp][adr];
      }

      /**************************NUMERIC PRECOND*************************/
      if ( no->num_numeric_preconds > 0 ) {
	a->numeric_preconds_comp = ( dis_Comparator * ) 
	  calloc( no->num_numeric_preconds, sizeof( dis_Comparator ) );
	a->numeric_preconds_lh = ( dis_ExpNode_pointer * )
	  calloc( no->num_numeric_preconds, sizeof( dis_ExpNode_pointer ) );
	a->numeric_preconds_rh = ( dis_ExpNode_pointer * )
	  calloc( no->num_numeric_preconds, sizeof( dis_ExpNode_pointer ) );
	a->num_numeric_preconds = 0;
      }
      for ( i = 0; i < no->num_numeric_preconds; i++ ) {
	a->numeric_preconds_comp[a->num_numeric_preconds] = no->numeric_preconds_comp[i];
	a->numeric_preconds_lh[a->num_numeric_preconds] = dis_copy_Exp( no->numeric_preconds_lh[i] );
	dis_instantiate_exp_by_action( &(a->numeric_preconds_lh[a->num_numeric_preconds]), a );
	if ( !dis_set_relevants_in_exp( &(a->numeric_preconds_lh[a->num_numeric_preconds]) ) ) break;
	a->numeric_preconds_rh[a->num_numeric_preconds] = dis_copy_Exp( no->numeric_preconds_rh[i] );
	dis_instantiate_exp_by_action( &(a->numeric_preconds_rh[a->num_numeric_preconds]), a );
	if ( !dis_set_relevants_in_exp( &(a->numeric_preconds_rh[a->num_numeric_preconds]) ) ) break;
	if ( a->numeric_preconds_lh[a->num_numeric_preconds]->connective == NUMBER &&
	     a->numeric_preconds_rh[a->num_numeric_preconds]->connective == NUMBER ) {
	  /* trivial numeric precond
	   */
	  if ( dis_number_comparison_holds( a->numeric_preconds_comp[a->num_numeric_preconds],
					a->numeric_preconds_lh[a->num_numeric_preconds]->value,
					a->numeric_preconds_rh[a->num_numeric_preconds]->value ) ) {
	    /* true precond -> throw precond away. by not incrementing number of such.
	     */
	    dis_free_dis_ExpNode( a->numeric_preconds_lh[a->num_numeric_preconds] );
	    dis_free_dis_ExpNode( a->numeric_preconds_rh[a->num_numeric_preconds] );
	    continue;
	  } else {
	    /* false precond -> throw action away.
	     */
	    break;
	  }
	}
	a->num_numeric_preconds++;
      }
      if ( i < no->num_numeric_preconds ) {
	/* a precond accesses an undefined fluent, or is false -> remove action!
	 */
	dis_gnum_actions--;
	if ( p ) {
	  p->next = a->next;
	  t = a;
	  a = a->next;
	  t->next = dis_gtrash_actions;
	  dis_gtrash_actions = t;
	} else {
	  dis_gactions = a->next;
	  t = a;
	  a = a->next;
	  t->next = dis_gtrash_actions;
	  dis_gtrash_actions = t;
	}
	continue;
      }
      /**************************NUMERIC PRECOND-END*************************/

      /* and now for the effects
       */
      if ( a->num_effects > 0 ) {
	a->effects = ( dis_Actiondis_Effect * ) calloc( a->num_effects, sizeof( dis_Actiondis_Effect ) );
	for ( i = 0; i < a->num_effects; i++ ) {
	  a->effects[i].illegal = dis_FALSE;
	  a->effects[i].removed = dis_FALSE;
	}
      }
      a->num_effects = 0;
      for ( ne = no->effects; ne; ne = ne->next ) {
	aa = &(a->effects[a->num_effects]);

	if ( ne->num_conditions > 0 ) {
	  aa->conditions = ( int * ) calloc( ne->num_conditions, sizeof( int ) );
	}
	aa->num_conditions = 0;
	for ( i = 0; i < ne->num_conditions; i++ ) {
	  lp = ne->conditions[i].predicate;
	  for ( j = 0; j < dis_garity[lp]; j++ ) {
	    dis_largs[j] = ( ne->conditions[i].args[j] >= 0 ) ?
	      ne->conditions[i].args[j] : a->inst_table[dis_DECODE_VAR( ne->conditions[i].args[j] )];
	  }
	  adr = dis_fact_adress();
	  if ( !dis_lpos[lp][adr] ) {/* condition not reachable: skip effect */
	    break;
	  }
	  if ( !dis_lneg[lp][adr] ) {/* condition always true: skip it */
	    continue;
	  }
	  aa->conditions[aa->num_conditions++] = dis_lindex[lp][adr];
	}
	if ( i < ne->num_conditions ) {/* found unreachable condition: free condition space */
	  xfree( aa->conditions );
	  continue;
	}

	/**************************NUMERIC COND*************************/
	if ( ne->num_numeric_conditions > 0 ) {
	  aa->numeric_conditions_comp = ( dis_Comparator * ) 
	    calloc( ne->num_numeric_conditions, sizeof( dis_Comparator ) );
	  aa->numeric_conditions_lh = ( dis_ExpNode_pointer * )
	    calloc( ne->num_numeric_conditions, sizeof( dis_ExpNode_pointer ) );
	  aa->numeric_conditions_rh = ( dis_ExpNode_pointer * )
	    calloc( ne->num_numeric_conditions, sizeof( dis_ExpNode_pointer ) );
	  for ( i = 0; i < ne->num_numeric_conditions; i++ ) {
	    aa->numeric_conditions_lh[i] = NULL;
	    aa->numeric_conditions_rh[i] = NULL;
	  }
	  aa->num_numeric_conditions = 0;
	}
	false_cond = dis_FALSE;
	for ( i = 0; i < ne->num_numeric_conditions; i++ ) {
	  aa->numeric_conditions_comp[aa->num_numeric_conditions] = ne->numeric_conditions_comp[i];
	  aa->numeric_conditions_lh[aa->num_numeric_conditions] = dis_copy_Exp( ne->numeric_conditions_lh[i] );
	  dis_instantiate_exp_by_action( &(aa->numeric_conditions_lh[aa->num_numeric_conditions]), a );
	  if ( !dis_set_relevants_in_exp( &(aa->numeric_conditions_lh[aa->num_numeric_conditions]) ) ) break;
	  aa->numeric_conditions_rh[aa->num_numeric_conditions] = dis_copy_Exp( ne->numeric_conditions_rh[i] );
	  dis_instantiate_exp_by_action( &(aa->numeric_conditions_rh[aa->num_numeric_conditions]), a );
	  if ( !dis_set_relevants_in_exp( &(aa->numeric_conditions_rh[aa->num_numeric_conditions]) ) ) break;
	  if ( aa->numeric_conditions_lh[aa->num_numeric_conditions]->connective == NUMBER &&
	       aa->numeric_conditions_rh[aa->num_numeric_conditions]->connective == NUMBER ) {
	    /* trivial numeric condition
	     */
	    if ( dis_number_comparison_holds( aa->numeric_conditions_comp[aa->num_numeric_conditions],
					  aa->numeric_conditions_lh[aa->num_numeric_conditions]->value,
					  aa->numeric_conditions_rh[aa->num_numeric_conditions]->value ) ) {
	      /* true cond -> throw cond away. by not incrementing number of such.
	       */
	      dis_free_dis_ExpNode( aa->numeric_conditions_lh[aa->num_numeric_conditions] );
	      dis_free_dis_ExpNode( aa->numeric_conditions_rh[aa->num_numeric_conditions] );
	      aa->numeric_conditions_lh[aa->num_numeric_conditions] = NULL;
	      aa->numeric_conditions_rh[aa->num_numeric_conditions] = NULL;
	      continue;
	    } else {
	      /* false cond -> throw effect away.
	       */
	      false_cond = dis_TRUE;
	      break;
	    }
	  }
	  aa->num_numeric_conditions++;
	}
	if ( i < ne->num_numeric_conditions ) {
	  if ( false_cond ) {
	    /* false numeric cond: free what's been done so far, and skip effect
	     */
	    for ( i = 0; i <= aa->num_numeric_conditions; i++ ) {
	      dis_free_dis_ExpNode( aa->numeric_conditions_lh[i] );
	      dis_free_dis_ExpNode( aa->numeric_conditions_rh[i] );
	    }
	    xfree( aa->numeric_conditions_comp );
	    xfree( aa->numeric_conditions_lh );
	    xfree( aa->numeric_conditions_rh );
	    continue;/* next effect, without incrementing action counter */
	  } else {
	    /* numeric effect uses undefined fluent in condition -->
	     * THROW WHOLE ACTION AWAY! done by breaking out of the 
	     * effects loop, which will be catched below overall
	     * effect handling.
	     */
	    break;
	  }
	}
	/**************************NUMERIC COND - END*************************/

	/* now create the add and del effects.
	 */
	if ( ne->num_adds > 0 ) {
	  aa->adds = ( int * ) calloc( ne->num_adds, sizeof( int ) );
	}
	aa->num_adds = 0;
	for ( i = 0; i < ne->num_adds; i++ ) {
	  lp = ne->adds[i].predicate;
	  for ( j = 0; j < dis_garity[lp]; j++ ) {
	    dis_largs[j] = ( ne->adds[i].args[j] >= 0 ) ?
	      ne->adds[i].args[j] : a->inst_table[dis_DECODE_VAR( ne->adds[i].args[j] )];
	  }
	  adr = dis_fact_adress();
	  if ( !dis_lneg[lp][adr] ) {/* effect always true: skip it */
	    continue;
	  }
	  aa->adds[aa->num_adds++] = dis_lindex[lp][adr];
	}

	if ( ne->num_dels > 0 ) {
	  aa->dels = ( int * ) calloc( ne->num_dels, sizeof( int ) );
	}
	aa->num_dels = 0;
	for ( i = 0; i < ne->num_dels; i++ ) {
	  lp = ne->dels[i].predicate;
	  for ( j = 0; j < dis_garity[lp]; j++ ) {
	    dis_largs[j] = ( ne->dels[i].args[j] >= 0 ) ?
	      ne->dels[i].args[j] : a->inst_table[dis_DECODE_VAR( ne->dels[i].args[j] )];
	  }
	  adr = dis_fact_adress();
	  if ( !dis_lpos[lp][adr] ) {/* effect always false: skip it */
	    continue;
	  }
	  /* NO CHECK Fdis_OR ADD \CAP DEL!!!!! -> dis_ALLOWED BY SEMANTICS!!!
	   */
	  aa->dels[aa->num_dels++] = dis_lindex[lp][adr];
	}
	if ( i < ne->num_dels ) break;

	/**************************NUMERIC EFFECTS*************************/
	if ( ne->num_numeric_effects > 0 ) {
	  aa->numeric_effects_neft = ( dis_Numericdis_EffectType * ) 
	    calloc( ne->num_numeric_effects, sizeof( dis_Numericdis_EffectType ) );
	  aa->numeric_effects_fl = ( int * )
	    calloc( ne->num_numeric_effects, sizeof( int ) );
	  aa->numeric_effects_rh = ( dis_ExpNode_pointer * )
	    calloc( ne->num_numeric_effects, sizeof( dis_ExpNode_pointer ) );
	  aa->num_numeric_effects = 0;
	}
	for ( i = 0; i < ne->num_numeric_effects; i++ ) {
	  aa->numeric_effects_neft[aa->num_numeric_effects] = ne->numeric_effects_neft[i];
	  lf = ne->numeric_effects_fluent[i].function;
	  for ( j = 0; j < dis_gf_arity[lf]; j++ ) {
	    lf_args[j] = ( ne->numeric_effects_fluent[i].args[j] >= 0 ) ?
	      ne->numeric_effects_fluent[i].args[j] : 
	      a->inst_table[dis_DECODE_VAR( ne->numeric_effects_fluent[i].args[j] )];
	  }
	  adr = fluent_adress();
	  /* if it's -1, simply let it in --- if that effect appears, then 
	   * action is illegal, otherwise not.
	   */
	  aa->numeric_effects_fl[i] = lf_index[lf][adr];
	  if ( lf_index[lf][adr] == -1 ) aa->illegal = dis_TRUE;
	  aa->numeric_effects_rh[aa->num_numeric_effects] = dis_copy_Exp( ne->numeric_effects_rh[i] );
	  dis_instantiate_exp_by_action( &(aa->numeric_effects_rh[aa->num_numeric_effects]), a );
	  if ( !dis_set_relevants_in_exp( &(aa->numeric_effects_rh[aa->num_numeric_effects]) ) ) {
	    aa->illegal = dis_TRUE;
	  }
	  if ( aa->illegal &&
	       aa->num_conditions == 0 &&
	       aa->num_numeric_conditions == 0 ) {
	    break;
	  }
	  /* that's it ???????????????? - !!
	   */
	  aa->num_numeric_effects++;
	}
	if ( i < ne->num_numeric_effects ) {
	  /* an unconditional illegal effekt
	   */
	  break;
	}
	/**************************NUMERIC EFFECTS - END*************************/

	/* this effect is OK. go to next one in NormOp.
	 */
	a->num_effects++;
	lnum_effects++;
      }
      if ( ne ) {
	/* we get here if one effect was faulty
	 */
	dis_gnum_actions--;
	if ( p ) {
	  p->next = a->next;
	  t = a;
	  a = a->next;
	  t->next = dis_gtrash_actions;
	  dis_gtrash_actions = t;
	} else {
	  dis_gactions = a->next;
	  t = a;
	  a = a->next;
	  t->next = dis_gtrash_actions;
	  dis_gtrash_actions = t;
	}
      } else {
	p = a;
	a = a->next;
      }
      continue;
    }
    /**********************************second half: hard operators --> pseudo actions******************/
    if ( a->pseudo_action ) {
      /* action is result of a dis_Pseudodis_Action
       */
      pa = a->pseudo_action;
      if ( pa->num_preconds > 0 ) {
	a->preconds = ( int * ) calloc( pa->num_preconds, sizeof( int ) );
      }
      a->num_preconds = 0;
      for ( i = 0; i < pa->num_preconds; i++ ) {
	lp = pa->preconds[i].predicate;
	for ( j = 0; j < dis_garity[lp]; j++ ) {
	  dis_largs[j] = pa->preconds[i].args[j];
	}
	adr = dis_fact_adress();
	/* preconds are dis_lpos in all cases due to reachability analysis
	 */
	if ( !dis_lneg[lp][adr] ) {
	  continue;
	}	
	a->preconds[a->num_preconds++] = dis_lindex[lp][adr];
      }

      /**************************NUMERIC PRECOND*************************/
      if ( pa->num_numeric_preconds > 0 ) {
	a->numeric_preconds_comp = ( dis_Comparator * ) 
	  calloc( pa->num_numeric_preconds, sizeof( dis_Comparator ) );
	a->numeric_preconds_lh = ( dis_ExpNode_pointer * )
	  calloc( pa->num_numeric_preconds, sizeof( dis_ExpNode_pointer ) );
	a->numeric_preconds_rh = ( dis_ExpNode_pointer * )
	  calloc( pa->num_numeric_preconds, sizeof( dis_ExpNode_pointer ) );
	a->num_numeric_preconds = 0;
      }
      for ( i = 0; i < pa->num_numeric_preconds; i++ ) {
	a->numeric_preconds_comp[a->num_numeric_preconds] = pa->numeric_preconds_comp[i];
	a->numeric_preconds_lh[a->num_numeric_preconds] = dis_copy_Exp( pa->numeric_preconds_lh[i] );
	if ( !dis_set_relevants_in_exp( &(a->numeric_preconds_lh[a->num_numeric_preconds]) ) ) break;
	a->numeric_preconds_rh[a->num_numeric_preconds] = dis_copy_Exp( pa->numeric_preconds_rh[i] );
	if ( !dis_set_relevants_in_exp( &(a->numeric_preconds_rh[a->num_numeric_preconds]) ) ) break;
	a->num_numeric_preconds++;
      }
      if ( i < pa->num_numeric_preconds ) {
	/* a precond accesses an undefined fluent -> remove action!
	 */
	dis_gnum_actions--;
	if ( p ) {
	  p->next = a->next;
	  t = a;
	  a = a->next;
	  t->next = dis_gtrash_actions;
	  dis_gtrash_actions = t;
	} else {
	  dis_gactions = a->next;
	  t = a;
	  a = a->next;
	  t->next = dis_gtrash_actions;
	  dis_gtrash_actions = t;
	}
	continue;
      }
      /**************************NUMERIC PRECOND-END*************************/

      /* and now for the effects
       */
      if ( a->num_effects > 0 ) {
	a->effects = ( dis_Actiondis_Effect * ) calloc( a->num_effects, sizeof( dis_Actiondis_Effect ) );
	for ( i = 0; i < a->num_effects; i++ ) {
	  a->effects[i].illegal = dis_FALSE;
	  a->effects[i].removed = dis_FALSE;
	}
      }
      a->num_effects = 0;
      for ( pae = pa->effects; pae; pae = pae->next ) {
	aa = &(a->effects[a->num_effects]);

	if ( pae->num_conditions > 0 ) {
	  aa->conditions = ( int * ) calloc( pae->num_conditions, sizeof( int ) );
	}
	aa->num_conditions = 0;
	for ( i = 0; i < pae->num_conditions; i++ ) {
	  lp = pae->conditions[i].predicate;
	  for ( j = 0; j < dis_garity[lp]; j++ ) {
	    dis_largs[j] = pae->conditions[i].args[j];
	  }
	  adr = dis_fact_adress();
	  if ( !dis_lpos[lp][adr] ) {/* condition not reachable: skip effect */
	    break;
	  }
	  if ( !dis_lneg[lp][adr] ) {/* condition always true: skip it */
	    continue;
	  }
	  aa->conditions[aa->num_conditions++] = dis_lindex[lp][adr];
	}
	if ( i < pae->num_conditions ) {/* found unreachable condition: free condition space */
	  xfree( aa->conditions );
	  continue;
	}

	/**************************NUMERIC COND*************************/
	if ( pae->num_numeric_conditions > 0 ) {
	  aa->numeric_conditions_comp = ( dis_Comparator * ) 
	    calloc( pae->num_numeric_conditions, sizeof( dis_Comparator ) );
	  aa->numeric_conditions_lh = ( dis_ExpNode_pointer * )
	    calloc( pae->num_numeric_conditions, sizeof( dis_ExpNode_pointer ) );
	  aa->numeric_conditions_rh = ( dis_ExpNode_pointer * )
	    calloc( pae->num_numeric_conditions, sizeof( dis_ExpNode_pointer ) );
	  for ( i = 0; i < pae->num_numeric_conditions; i++ ) {
	    aa->numeric_conditions_lh[i] = NULL;
	    aa->numeric_conditions_rh[i] = NULL;
	  }
	  aa->num_numeric_conditions = 0;
	}
	for ( i = 0; i < pae->num_numeric_conditions; i++ ) {
	  aa->numeric_conditions_comp[aa->num_numeric_conditions] = pae->numeric_conditions_comp[i];
	  aa->numeric_conditions_lh[aa->num_numeric_conditions] = dis_copy_Exp( pae->numeric_conditions_lh[i] );
	  if ( !dis_set_relevants_in_exp( &(aa->numeric_conditions_lh[aa->num_numeric_conditions]) ) ) break;
	  aa->numeric_conditions_rh[aa->num_numeric_conditions] = dis_copy_Exp( pae->numeric_conditions_rh[i] );
	  if ( !dis_set_relevants_in_exp( &(aa->numeric_conditions_rh[aa->num_numeric_conditions]) ) ) break;
	  aa->num_numeric_conditions++;
	}
	if ( i < pae->num_numeric_conditions ) {
	  /* numeric effect uses undefined fluent in condition -->
	   * THROW WHOLE ACTION AWAY! done by breaking out of the 
	   * effects loop, which will be catched below overall
	   * effect handling.
	   */
	  break;
	}
	/**************************NUMERIC COND - END*************************/

	/* now create the add and del effects.
	 */
	if ( pae->num_adds > 0 ) {
	  aa->adds = ( int * ) calloc( pae->num_adds, sizeof( int ) );
	}
	aa->num_adds = 0;
	for ( i = 0; i < pae->num_adds; i++ ) {
	  lp = pae->adds[i].predicate;
	  for ( j = 0; j < dis_garity[lp]; j++ ) {
	    dis_largs[j] = pae->adds[i].args[j];
	  }
	  adr = dis_fact_adress();
	  if ( !dis_lneg[lp][adr] ) {/* effect always true: skip it */
	    continue;
	  }
	  aa->adds[aa->num_adds++] = dis_lindex[lp][adr];
	}

	if ( pae->num_dels > 0 ) {
	  aa->dels = ( int * ) calloc( pae->num_dels, sizeof( int ) );
	}
	aa->num_dels = 0;
	for ( i = 0; i < pae->num_dels; i++ ) {
	  lp = pae->dels[i].predicate;
	  for ( j = 0; j < dis_garity[lp]; j++ ) {
	    dis_largs[j] = pae->dels[i].args[j];
	  }
	  adr = dis_fact_adress();
	  if ( !dis_lpos[lp][adr] ) {/* effect always false: skip it */
	    continue;
	  }
	  aa->dels[aa->num_dels++] = dis_lindex[lp][adr];
	}
	if ( i < pae->num_dels ) break;

	/**************************NUMERIC EFFECTS*************************/
	if ( pae->num_numeric_effects > 0 ) {
	  aa->numeric_effects_neft = ( dis_Numericdis_EffectType * ) 
	    calloc( pae->num_numeric_effects, sizeof( dis_Numericdis_EffectType ) );
	  aa->numeric_effects_fl = ( int * )
	    calloc( pae->num_numeric_effects, sizeof( int ) );
	  aa->numeric_effects_rh = ( dis_ExpNode_pointer * )
	    calloc( pae->num_numeric_effects, sizeof( dis_ExpNode_pointer ) );
	  aa->num_numeric_effects = 0;
	}
	for ( i = 0; i < pae->num_numeric_effects; i++ ) {
	  aa->numeric_effects_neft[aa->num_numeric_effects] = pae->numeric_effects_neft[i];
	  lf = pae->numeric_effects_fluent[i].function;
	  for ( j = 0; j < dis_gf_arity[lf]; j++ ) {
	    lf_args[j] = pae->numeric_effects_fluent[i].args[j];
	    if ( lf_args[j] < 0 ) {
	      printf("\n\nuninstantiated affected fluent in final actions! debug me.\n\n");
	      exit( 1 );
	    }
	  }
	  adr = fluent_adress();
	  /* if it's -1, simply let it in --- if that effect appears, then 
	   * action is illegal, otherwise not.
	   */
	  aa->numeric_effects_fl[i] = lf_index[lf][adr];
	  if ( lf_index[lf][adr] == -1 ) aa->illegal = dis_TRUE;
	  aa->numeric_effects_rh[aa->num_numeric_effects] = dis_copy_Exp( pae->numeric_effects_rh[i] );
	  if ( !dis_set_relevants_in_exp( &(aa->numeric_effects_rh[aa->num_numeric_effects]) ) ) {
	    aa->illegal = dis_TRUE;
	  }
	  if ( aa->illegal &&
	       aa->num_conditions == 0 &&
	       aa->num_numeric_conditions == 0 ) {
	    break;
	  }
	  /* that's it ???????????????? - !!
	   */
	  aa->num_numeric_effects++;
	}
	if ( i < pae->num_numeric_effects ) {
	  /* an unconditional illegal effekt
	   */
	  break;
	}
	/**************************NUMERIC EFFECTS - END*************************/

	/* this effect is OK. go to next one in dis_Pseudodis_Action.
	 */
	a->num_effects++;
	lnum_effects++;
      }
      if ( pae ) {
	/* we get here if one effect was faulty
	 */
	dis_gnum_actions--;
	if ( p ) {
	  p->next = a->next;
	  t = a;
	  a = a->next;
	  t->next = dis_gtrash_actions;
	  dis_gtrash_actions = t;
	} else {
	  dis_gactions = a->next;
	  t = a;
	  a = a->next;
	  t->next = dis_gtrash_actions;
	  dis_gtrash_actions = t;
	}
      } else {
	p = a;
	a = a->next;
      }
      continue;
    }/* end of if clause for dis_Pseudodis_Action */
    /* if action was neither normop, nor pseudo action determined,
     * then it is an artificial action due to disjunctive goal
     * conditions.
     *
     * these are already in final form.
     */
    p = a;
    a = a->next;
  }/* endfor all actions ! */

}



void dis_instantiate_exp_by_action( dis_ExpNode **n, dis_Action *a )

{

  int j, f, k, h;
  dis_Bool ok;

  switch ( (*n)->connective ) {
  case AD:
    dis_instantiate_exp_by_action( &((*n)->leftson), a );
    dis_instantiate_exp_by_action( &((*n)->rightson), a );
    if ( (*n)->leftson->connective != NUMBER ||
	 (*n)->rightson->connective != NUMBER ) {
      break;
    }
    (*n)->connective = NUMBER;
    (*n)->value = (*n)->leftson->value + (*n)->rightson->value;
    dis_free_dis_ExpNode( (*n)->leftson );
    (*n)->leftson = NULL;
    dis_free_dis_ExpNode( (*n)->rightson );
    (*n)->rightson = NULL;
    break;
  case SU:
    dis_instantiate_exp_by_action( &((*n)->leftson), a );
    dis_instantiate_exp_by_action( &((*n)->rightson), a );
    if ( (*n)->leftson->connective != NUMBER ||
	 (*n)->rightson->connective != NUMBER ) {
      break;
    }
    (*n)->connective = NUMBER;
    (*n)->value = (*n)->leftson->value - (*n)->rightson->value;
    dis_free_dis_ExpNode( (*n)->leftson );
    (*n)->leftson = NULL;
    dis_free_dis_ExpNode( (*n)->rightson );
    (*n)->rightson = NULL;
    break;
  case MU:
    dis_instantiate_exp_by_action( &((*n)->leftson), a );
    dis_instantiate_exp_by_action( &((*n)->rightson), a );
    if ( (*n)->leftson->connective != NUMBER ||
	 (*n)->rightson->connective != NUMBER ) {
      break;
    }
    (*n)->connective = NUMBER;
    (*n)->value = (*n)->leftson->value * (*n)->rightson->value;
    dis_free_dis_ExpNode( (*n)->leftson );
    (*n)->leftson = NULL;
    dis_free_dis_ExpNode( (*n)->rightson );
    (*n)->rightson = NULL;
    break;
  case DI:
    dis_instantiate_exp_by_action( &((*n)->leftson), a );
    dis_instantiate_exp_by_action( &((*n)->rightson), a );
    if ( (*n)->leftson->connective != NUMBER ||
	 (*n)->rightson->connective != NUMBER ) {
      break;
    }
    if ( (*n)->rightson->value == 0 ) {
      /* kind of unclean: simply leave that in here;
       * we will later determine the right thing 
       * to do with it.
       */
      break;
    }
    (*n)->connective = NUMBER;
    (*n)->value = (*n)->leftson->value / (*n)->rightson->value;
    dis_free_dis_ExpNode( (*n)->leftson );
    (*n)->leftson = NULL;
    dis_free_dis_ExpNode( (*n)->rightson );
    (*n)->rightson = NULL;
    break;
  case MINUS:
    dis_instantiate_exp_by_action( &((*n)->son), a );
    if ( (*n)->son->connective != NUMBER ) break;
    (*n)->connective = NUMBER;
    (*n)->value = ((float) (-1)) * (*n)->son->value;
    dis_free_dis_ExpNode( (*n)->son );
    (*n)->son = NULL;
    break;    
  case NUMBER:
    break;
  case FHEAD:
    f = (*n)->fluent->function;
    ok = dis_TRUE;
    for ( j = 0; j < dis_gf_arity[f]; j++ ) {
      h = ( (*n)->fluent->args[j] < 0 ) ?
	a->inst_table[dis_DECODE_VAR( (*n)->fluent->args[j] )] : (*n)->fluent->args[j];
      if ( h < 0 ) {
	ok = dis_FALSE;
      } else {
	(*n)->fluent->args[j] = h;
      }
    }
    if ( !ok ) {
      printf("\n\nnon-instantiated fluent in final actiona! debug me!!\n\n");
      exit( 1 );
    }
    if ( dis_gis_changed[f] ) break;
    for ( j = 0; j < dis_gnum_initial_function[f]; j++ ) {
      for ( k = 0; k < dis_gf_arity[f]; k++ ) {
	if ( dis_ginitial_function[f][j].fluent.args[k] !=
	     (*n)->fluent->args[k] ) break;
      }
      if ( k < dis_gf_arity[f] ) continue;
      (*n)->connective = NUMBER;
      (*n)->value = dis_ginitial_function[f][j].value;
      break;
    }
    break;
  default:
    printf("\n\ninst. exp by action: wrong specifier %d",
	   (*n)->connective);
    exit( 1 );
  }

}




















/**************************************************
 * CONNECTIVITY GRAPH. ULTRA CLEAN REPRESENTATION *
 **************************************************/




















void dis_build_connectivity_graph( void )

{

  int i, j, k, l, n_op, n_ef, fl, ef, ef_, m/*, fl_*/;
  float val;
//  dis_Comparator comp;
  dis_Action *a;
  dis_Actiondis_Effect *e;

  struct timeb tp;

  ftime( &tp );
  srandom( tp.millitm );

  /* Chih-Wei */
  dis_grelevant_facts = (dis_Fact *) realloc(dis_grelevant_facts, dis_gnum_relevant_facts*sizeof(dis_Fact));
  dis_grelevant_fluents = (dis_Fluent *) realloc(dis_grelevant_fluents, 
                  dis_gnum_relevant_fluents*sizeof(dis_Fluent));
  dis_grelevant_fluents_name = (dis_Token *) realloc(dis_grelevant_fluents_name, dis_gnum_relevant_fluents*sizeof(dis_Token));
  dis_grelevant_fluents_lnf = (Lnfdis_ExpNode_pointer *) realloc(dis_grelevant_fluents_lnf, 
                          dis_gnum_relevant_fluents*sizeof(Lnfdis_ExpNode_pointer));
                  
  
  dis_gnum_ft_conn = dis_gnum_relevant_facts;
  dis_gnum_fl_conn = dis_gnum_relevant_fluents;
  dis_gnum_op_conn = dis_gnum_actions;
  dis_gft_conn = ( dis_FtConn * ) calloc( dis_gnum_ft_conn, sizeof( dis_FtConn ) );
  dis_gfl_conn = ( dis_FlConn * ) calloc( dis_gnum_fl_conn, sizeof( dis_FlConn ) );
  dis_gop_conn = ( dis_OpConn * ) calloc( dis_gnum_op_conn, sizeof( dis_OpConn ) );
  dis_gef_conn = ( dis_EfConn * ) calloc( lnum_effects, sizeof( dis_EfConn ) );
  dis_gnum_ef_conn = 0;

  for ( i = 0; i < dis_gnum_ft_conn; i++ ) {
    dis_gft_conn[i].num_PC = 0;
    dis_gft_conn[i].num_A = 0;
    dis_gft_conn[i].num_D = 0;

    dis_gft_conn[i].rand = random() % dis_BIG_INT;
  }

  dis_gnum_real_fl_conn = 0;
  for ( i = 0; i < dis_gnum_fl_conn; i++ ) {
    dis_gfl_conn[i].num_PC = 0;
    dis_gfl_conn[i].num_IN = 0;
    dis_gfl_conn[i].num_AS = 0;

    if ( dis_grelevant_fluents_lnf[i] == NULL ) {
      dis_gfl_conn[i].artificial = dis_FALSE;
      dis_gfl_conn[i].rand = random() % dis_BIG_INT;
      dis_gnum_real_fl_conn++;
    } else {
      /* once we're in here we'll stay as all artificial 
       * fluents are appended to the end.
       */
      dis_gfl_conn[i].artificial = dis_TRUE;
      dis_gfl_conn[i].lnf_F = ( int * ) 
	calloc( dis_grelevant_fluents_lnf[i]->num_pF, sizeof( int ) );
      dis_gfl_conn[i].lnf_C = ( float * ) 
	calloc( dis_grelevant_fluents_lnf[i]->num_pF, sizeof( float ) );
      for ( j = 0; j < dis_grelevant_fluents_lnf[i]->num_pF; j++ ) {
	dis_gfl_conn[i].lnf_F[j] = dis_grelevant_fluents_lnf[i]->pF[j];
	dis_gfl_conn[i].lnf_C[j] = dis_grelevant_fluents_lnf[i]->pC[j];
      }
      dis_gfl_conn[i].num_lnf = dis_grelevant_fluents_lnf[i]->num_pF;
    }

/*    dis_gfl_conn[i].max_needed_comp = IGUAL;
    dis_gfl_conn[i].is_rh_needed = dis_FALSE;*/
  }

  /* why not do this here?
   */
  dis_gmneed_start_D = ( dis_Bool * ) calloc( dis_gnum_real_fl_conn, sizeof( dis_Bool ) );
  dis_gmneed_start_V = ( float * ) calloc( dis_gnum_real_fl_conn, sizeof( float ) );

  for ( i = 0; i < dis_gnum_op_conn; i++ ) {
    dis_gop_conn[i].num_E = 0;
  }

  for ( i = 0; i < lnum_effects; i++ ) {
    dis_gef_conn[i].num_PC = 0;
    dis_gef_conn[i].num_f_PC = 0;
    dis_gef_conn[i].num_A = 0;
    dis_gef_conn[i].num_D = 0;
    dis_gef_conn[i].num_I = 0;
    dis_gef_conn[i].num_IN = 0;
    dis_gef_conn[i].num_AS = 0;

    dis_gef_conn[i].illegal = dis_FALSE;
    dis_gef_conn[i].removed = dis_FALSE;
  }

  /* determine if there are conditional effects.
   */
  dis_gconditional_effects = dis_FALSE;
  for ( a = dis_gactions; a; a = a->next ) {
    for ( i = 0; i < a->num_effects; i++ ) {
      e = &(a->effects[i]);
      if ( e->num_conditions > 0 ) {
        break;
      }
      if ( e->num_lnf_conditions > 0 ) {
        break;
      }
    }  
    if ( i < a->num_effects ) break;
  }
  if ( a ) {
    printf("\n\ntask contains conditional effects. turning off state domination.\n\n");
    dis_gconditional_effects = dis_TRUE;
  }

  n_op = 0;
  n_ef = 0;
  for ( a = dis_gactions; a; a = a->next ) {
//    dis_free_Normdis_Operator(a->norm_operator);
    dis_gop_conn[n_op].action = a;
    if ( a->num_effects == 0 ) {
      continue;
    }

    dis_gop_conn[n_op].E = ( int * ) calloc( a->num_effects, sizeof( int ) );
    for ( i = 0; i < a->num_effects; i++ ) {
      e = &(a->effects[i]);
      dis_gef_conn[n_ef].cost = e->cost;
      if ( e->removed ) {
	/* this one disappeared through summarization
	 */
	continue;
      }
      dis_gop_conn[n_op].E[dis_gop_conn[n_op].num_E++] = n_ef;
      dis_gef_conn[n_ef].op = n_op;
      if ( e->illegal ) {
	dis_gef_conn[n_ef].illegal = dis_TRUE;
      }

      /*****************************CONDS********************************/
      dis_gef_conn[n_ef].PC = ( int * ) 
	calloc( e->num_conditions + a->num_preconds, sizeof( int ) );
      for ( j = 0; j < a->num_preconds; j++ ) {
	for ( k = 0; k < dis_gef_conn[n_ef].num_PC; k++ ) {
	  if ( dis_gef_conn[n_ef].PC[k] == a->preconds[j] ) break;
	}
	if ( k < dis_gef_conn[n_ef].num_PC ) continue;
	dis_gef_conn[n_ef].PC[dis_gef_conn[n_ef].num_PC++] = a->preconds[j];
      }
      for ( j = 0; j < e->num_conditions; j++ ) {
	for ( k = 0; k < dis_gef_conn[n_ef].num_PC; k++ ) {
	  if ( dis_gef_conn[n_ef].PC[k] == e->conditions[j] ) break;
	}
	if ( k < dis_gef_conn[n_ef].num_PC ) continue;
	dis_gef_conn[n_ef].PC[dis_gef_conn[n_ef].num_PC++] = e->conditions[j];
      }
      /* similar thing for numeric conditions.
       */
      dis_gef_conn[n_ef].f_PC_comp = ( dis_Comparator * ) 
	calloc( e->num_lnf_conditions + a->num_lnf_preconds, sizeof( dis_Comparator ) );
      dis_gef_conn[n_ef].f_PC_fl = ( int * ) 
	calloc( e->num_lnf_conditions + a->num_lnf_preconds, sizeof( int ) );
      dis_gef_conn[n_ef].f_PC_c = ( float * ) 
	calloc( e->num_lnf_conditions + a->num_lnf_preconds, sizeof( float ) );
      dis_gef_conn[n_ef].f_PC_direct_comp = ( dis_Comparator * ) calloc( dis_gnum_fl_conn, sizeof( dis_Comparator ) );
      for ( j = 0; j < dis_gnum_fl_conn; j++ ) {
	dis_gef_conn[n_ef].f_PC_direct_comp[j] = IGUAL;
      }
      dis_gef_conn[n_ef].f_PC_direct_c = ( float * ) calloc( dis_gnum_fl_conn, sizeof( float ) );
      for ( j = 0; j < a->num_lnf_preconds; j++ ) {
	if ( a->lnf_preconds_lh[j]->num_pF != 1 ) {
	  printf("\n\nnon 1 card. in comp lh final pre copyover.\n\n");
	  exit( 1 );
	}
	for ( k = 0; k < dis_gef_conn[n_ef].num_f_PC; k++ ) {
	  if ( dis_gef_conn[n_ef].f_PC_fl[k] == a->lnf_preconds_lh[j]->pF[0] ) break;
	}
	if ( k < dis_gef_conn[n_ef].num_f_PC ) {
	  if ( a->lnf_preconds_rh[j] < dis_gef_conn[n_ef].f_PC_c[k] ) {
	    /* weaker cond
	     */
	    continue;
	  }
	  if ( a->lnf_preconds_rh[j] > dis_gef_conn[n_ef].f_PC_c[k] ) {
	    /* stronger cond
	     */
	    dis_gef_conn[n_ef].f_PC_c[k] = a->lnf_preconds_rh[j];
	    dis_gef_conn[n_ef].f_PC_comp[k] = a->lnf_preconds_comp[j];
	    continue;
	  }
	  if ( a->lnf_preconds_comp[j] == GE ) {
	    /* we might need to strengthen our comp
	     */
	    dis_gef_conn[n_ef].f_PC_comp[k] = GE;
	  }
	} else {
	  dis_gef_conn[n_ef].f_PC_comp[dis_gef_conn[n_ef].num_f_PC] = a->lnf_preconds_comp[j];
	  dis_gef_conn[n_ef].f_PC_fl[dis_gef_conn[n_ef].num_f_PC] = a->lnf_preconds_lh[j]->pF[0];
	  dis_gef_conn[n_ef].f_PC_c[dis_gef_conn[n_ef].num_f_PC++] = a->lnf_preconds_rh[j];
	}
      }
      for ( j = 0; j < e->num_lnf_conditions; j++ ) {
	if ( e->lnf_conditions_lh[j]->num_pF != 1 ) {
	  printf("\n\nnon 1 card. in comp lh final cond copyover.\n\n");
	  exit( 1 );
	}
	for ( k = 0; k < dis_gef_conn[n_ef].num_f_PC; k++ ) {
	  if ( dis_gef_conn[n_ef].f_PC_fl[k] == e->lnf_conditions_lh[j]->pF[0] ) break;
	}
	if ( k < dis_gef_conn[n_ef].num_f_PC ) {
	  if ( e->lnf_conditions_rh[j] < dis_gef_conn[n_ef].f_PC_c[k] ) {
	    continue;
	  }
	  if ( e->lnf_conditions_rh[j] > dis_gef_conn[n_ef].f_PC_c[k] ) {
	    dis_gef_conn[n_ef].f_PC_c[k] = e->lnf_conditions_rh[j];
	    dis_gef_conn[n_ef].f_PC_comp[k] = e->lnf_conditions_comp[j];
	    continue;
	  }
	  if ( e->lnf_conditions_comp[j] == GE ) {
	    dis_gef_conn[n_ef].f_PC_comp[k] = GE;
	  }
	} else {
	  dis_gef_conn[n_ef].f_PC_comp[dis_gef_conn[n_ef].num_f_PC] = e->lnf_conditions_comp[j];
	  dis_gef_conn[n_ef].f_PC_fl[dis_gef_conn[n_ef].num_f_PC] = e->lnf_conditions_lh[j]->pF[0];
	  dis_gef_conn[n_ef].f_PC_c[dis_gef_conn[n_ef].num_f_PC++] = e->lnf_conditions_rh[j];
	}
      }
      /* now arrange the direct access structures from that.
       */
      for ( j = 0; j < dis_gef_conn[n_ef].num_f_PC; j++ ) {
	dis_gef_conn[n_ef].f_PC_direct_comp[dis_gef_conn[n_ef].f_PC_fl[j]] = dis_gef_conn[n_ef].f_PC_comp[j]; 
	dis_gef_conn[n_ef].f_PC_direct_c[dis_gef_conn[n_ef].f_PC_fl[j]] = dis_gef_conn[n_ef].f_PC_c[j]; 
      }
     /*****************************CONDS - END********************************/


      if ( e->illegal ) {
	/* we don't care about the effects if they're illegal -
	 * all we care about is whether the condition is true or not.
	 */
	n_ef++;
	dis_gnum_ef_conn++;
	continue;
      }
      /*****************************EFFECTS********************************/
      dis_gef_conn[n_ef].A = ( int * ) calloc( e->num_adds, sizeof( int ) );
      dis_gef_conn[n_ef].D = ( int * ) calloc( e->num_dels, sizeof( int ) );
      dis_gef_conn[n_ef].IN_fl = ( int * ) calloc( e->num_lnf_effects, sizeof( int ) );
      dis_gef_conn[n_ef].IN_fl_ = ( int * ) calloc( e->num_lnf_effects, sizeof( int ) );
      dis_gef_conn[n_ef].IN_c = ( float * ) calloc( e->num_lnf_effects, sizeof( float ) );
      dis_gef_conn[n_ef].AS_fl = ( int * ) calloc( e->num_lnf_effects, sizeof( int ) );
      dis_gef_conn[n_ef].AS_fl_ = ( int * ) calloc( e->num_lnf_effects, sizeof( int ) );
      dis_gef_conn[n_ef].AS_c = ( float * ) calloc( e->num_lnf_effects, sizeof( float ) );

      /* duplicates removed in summarize already.
       *
       * but don't include adds that are in the conds.
       * --- those are true anyway.
       *
       * and don't include dels that are in the adds
       * --- those will be re-added anyway.
       *
       * dis_NOTE: it is important that we use the *original* add list
       *       not the already reduced one, for the delete check!
       *       otherwise it may be that a delete that's in the add
       *       and also in the cond stays in!
       *
       *       IT IS ALSO IMPdis_ORTANT THAT WE DO BOTH!!!, i.e. if we do 
       *       the ads reduction then we *must* also do the dels 
       *       reduction to avoid that things are deleted that 
       *       would otherwise have been re-added.       
       */
      for ( j = 0; j < e->num_adds; j++ ) {
      if (!GpG.is_durative)
      {
	for ( k = 0; k < dis_gef_conn[n_ef].num_PC; k++ ) {
	  if ( dis_gef_conn[n_ef].PC[k] == e->adds[j] ) break;
	}
	if ( k < dis_gef_conn[n_ef].num_PC) continue;
      }
	dis_gef_conn[n_ef].A[dis_gef_conn[n_ef].num_A++] = e->adds[j];
      }
      for ( j = 0; j < e->num_dels; j++ ) {
	for ( k = 0; k < e->num_adds; k++ ) {
	  if ( e->adds[k] == e->dels[j] ) break;
	}
	if ( k < e->num_adds) continue;
	dis_gef_conn[n_ef].D[dis_gef_conn[n_ef].num_D++] = e->dels[j];
      }

      /* numeric part
       */
      for ( j = 0; j < e->num_lnf_effects; j++ ) {
	if ( e->lnf_effects_neft[j] != INCREASE ) continue;
	dis_gef_conn[n_ef].IN_fl[dis_gef_conn[n_ef].num_IN] = e->lnf_effects_fl[j];
	if ( e->lnf_effects_rh[j]->num_pF == 1 ) {
	  if ( e->lnf_effects_rh[j]->pF[0] < 0 ) {
	    printf("\n\nnon-relevant fluent in final copying to conn.\n\n");
	    exit( 1 );
	  }
	  dis_gef_conn[n_ef].IN_fl_[dis_gef_conn[n_ef].num_IN] = e->lnf_effects_rh[j]->pF[0];
	} else {
	  if ( e->lnf_effects_rh[j]->num_pF != 0 ) {
	    printf("\n\nnon-1 or 0 number of fl_ in copying to conn\n\n");
	    exit( 1 );
	  }
	  dis_gef_conn[n_ef].IN_fl_[dis_gef_conn[n_ef].num_IN] = -1;
	}
	dis_gef_conn[n_ef].IN_c[dis_gef_conn[n_ef].num_IN++] = e->lnf_effects_rh[j]->c;
      }
      /* now remove increasers by nothing.
       */
      j = 0;
      while ( j < dis_gef_conn[n_ef].num_IN ) {
	if ( dis_gef_conn[n_ef].IN_fl_[j] != -1 ||
	     dis_gef_conn[n_ef].IN_c[j] != 0 ) {
	  j++;
	  continue;
	}
	for ( k = j; k < dis_gef_conn[n_ef].num_IN - 1; k++ ) {
	  dis_gef_conn[n_ef].IN_fl[k] = dis_gef_conn[n_ef].IN_fl[k+1];
	  dis_gef_conn[n_ef].IN_fl_[k] = dis_gef_conn[n_ef].IN_fl_[k+1];
	  dis_gef_conn[n_ef].IN_c[k] = dis_gef_conn[n_ef].IN_c[k+1];
	}
	dis_gef_conn[n_ef].num_IN--;
      }
      /* now: the assigners...
       */
      for ( j = 0; j < e->num_lnf_effects; j++ ) {
	if ( e->lnf_effects_neft[j] != ASSIGN ) continue;
	dis_gef_conn[n_ef].AS_fl[dis_gef_conn[n_ef].num_AS] = e->lnf_effects_fl[j];
	if ( e->lnf_effects_rh[j]->num_pF == 1 ) {
	  if ( e->lnf_effects_rh[j]->pF[0] < 0 ) {
	    printf("\n\nnon-relevant fluent in final copying to conn.\n\n");
	    exit( 1 );
	  }
	  dis_gef_conn[n_ef].AS_fl_[dis_gef_conn[n_ef].num_AS] = e->lnf_effects_rh[j]->pF[0];
	} else {
	  if ( e->lnf_effects_rh[j]->num_pF != 0 ) {
	    printf("\n\nnon-1 or 0 number of fl_ in copying to conn\n\n");
	    exit( 1 );
	  }
	  dis_gef_conn[n_ef].AS_fl_[dis_gef_conn[n_ef].num_AS] = -1;
	}
	dis_gef_conn[n_ef].AS_c[dis_gef_conn[n_ef].num_AS++] = e->lnf_effects_rh[j]->c;
      }
      /*****************************EFFECTS - END********************************/
      
      n_ef++;
      dis_gnum_ef_conn++;
    }/* end all a->effects */


    /*****************************EMPTY EFFECTS********************************/
    if ( dis_gop_conn[n_op].num_E >= 1 ) {
      /* CHECK EMPTY EFFECTS!
       *
       * two step process --- first, remove all effects that are entirely empty.
       *                      second, check if all remaining effects are illegal
       *                      or only delete:
       *                      in that case, the op will never do any good so we 
       *                      remove all its effects.
       */
      i = 0;
      while ( i < dis_gop_conn[n_op].num_E ) {
	/* illegal effects *must* stay in!!!
	 */
	if ( dis_gef_conn[dis_gop_conn[n_op].E[i]].illegal ) {
	  i++;
	  continue;
	}
	if ( dis_gef_conn[dis_gop_conn[n_op].E[i]].num_A != 0 ||
	     dis_gef_conn[dis_gop_conn[n_op].E[i]].num_D != 0 ||
	     dis_gef_conn[dis_gop_conn[n_op].E[i]].num_IN != 0 ||
	     dis_gef_conn[dis_gop_conn[n_op].E[i]].num_AS != 0 ) {
	  i++;
	  continue;
	}
	/* we keep it in the dis_gef_conn (seems easier), 
	 * but mark it as removed, which will exclude it from everything.
	 */
	dis_gef_conn[dis_gop_conn[n_op].E[i]].removed = dis_TRUE;
	for ( j = i; j < dis_gop_conn[n_op].num_E - 1; j++ ) {
	  dis_gop_conn[n_op].E[j] = dis_gop_conn[n_op].E[j+1];
	}
	dis_gop_conn[n_op].num_E--;
      }

      m = 0;
      for ( i = 0; i < dis_gop_conn[n_op].num_E; i++ ) {
	if ( dis_gef_conn[dis_gop_conn[n_op].E[i]].illegal ) {
	  m++;
	  continue;
	}
	if ( dis_gef_conn[dis_gop_conn[n_op].E[i]].num_A == 0 &&
	     dis_gef_conn[dis_gop_conn[n_op].E[i]].num_IN == 0 &&
	     dis_gef_conn[dis_gop_conn[n_op].E[i]].num_AS == 0 ) {
	  m++;
	}
      }
      if ( m == dis_gop_conn[n_op].num_E ) {
	/* all remaining effects illegal or solely-deleters.
	 */
	for ( i = 0; i < dis_gop_conn[n_op].num_E; i++ ) {
	  dis_gef_conn[dis_gop_conn[n_op].E[i]].removed = dis_TRUE;
	}
	dis_gop_conn[n_op].num_E = 0;
      }
    }
    /*****************************EMPTY EFFECTS - END********************************/


    /*****************************IMPLIED EFFECTS********************************/
    if ( dis_gop_conn[n_op].num_E > 1 ) {
      for ( i = 0; i < dis_gop_conn[n_op].num_E; i++ ) {
	ef = dis_gop_conn[n_op].E[i];
	dis_gef_conn[ef].I = ( int * ) calloc( dis_gop_conn[n_op].num_E, sizeof( int ) );
	dis_gef_conn[ef].num_I = 0;
      }    
      for ( i = 0; i < dis_gop_conn[n_op].num_E - 1; i++ ) {
	ef = dis_gop_conn[n_op].E[i];
	for ( j = i+1; j < dis_gop_conn[n_op].num_E; j++ ) {
	  ef_ = dis_gop_conn[n_op].E[j];
	  /* ef ==> ef_ ? */
	  for ( k = 0; k < dis_gef_conn[ef_].num_PC; k++ ) {
	    for ( l = 0; l < dis_gef_conn[ef].num_PC; l++ ) {
	      if ( dis_gef_conn[ef].PC[l] == dis_gef_conn[ef_].PC[k] ) break;
	    }
	    if ( l == dis_gef_conn[ef].num_PC ) break;
	  }
	  if ( k == dis_gef_conn[ef_].num_PC ) {
	    for ( k = 0; k < dis_gnum_fl_conn; k++ ) {
	      if ( dis_gef_conn[ef_].f_PC_direct_comp[k] == IGUAL ) continue;
	      if ( dis_gef_conn[ef].f_PC_direct_comp[k] == IGUAL ||
		   dis_gef_conn[ef].f_PC_direct_c[k] < dis_gef_conn[ef_].f_PC_direct_c[k] ||
		   ( dis_gef_conn[ef].f_PC_direct_c[k] == dis_gef_conn[ef_].f_PC_direct_c[k] &&
		     dis_gef_conn[ef].f_PC_direct_comp[k] == GEQ && 
		     dis_gef_conn[ef_].f_PC_direct_comp[k] == GE ) ) break;
	    }
	    if ( k == dis_gnum_fl_conn ) {
	      dis_gef_conn[ef].I[dis_gef_conn[ef].num_I++] = ef_;
	    }
	  }
	  /* ef_ ==> ef ? */
	  for ( k = 0; k < dis_gef_conn[ef].num_PC; k++ ) {
	    for ( l = 0; l < dis_gef_conn[ef_].num_PC; l++ ) {
	      if ( dis_gef_conn[ef_].PC[l] == dis_gef_conn[ef].PC[k] ) break;
	    }
	    if ( l == dis_gef_conn[ef_].num_PC ) break;
	  }
	  if ( k == dis_gef_conn[ef].num_PC ) {
	    for ( k = 0; k < dis_gnum_fl_conn; k++ ) {
	      if ( dis_gef_conn[ef].f_PC_direct_comp[k] == IGUAL ) continue;
	      if ( dis_gef_conn[ef_].f_PC_direct_comp[k] == IGUAL ||
		   dis_gef_conn[ef_].f_PC_direct_c[k] < dis_gef_conn[ef].f_PC_direct_c[k] ||
		   ( dis_gef_conn[ef_].f_PC_direct_c[k] == dis_gef_conn[ef].f_PC_direct_c[k] &&
		     dis_gef_conn[ef_].f_PC_direct_comp[k] == GEQ && 
		     dis_gef_conn[ef].f_PC_direct_comp[k] == GE ) ) break;
	    }
	    if ( k == dis_gnum_fl_conn ) {
	      dis_gef_conn[ef_].I[dis_gef_conn[ef_].num_I++] = ef;
	    }
	  }
	}
      }
    }
    /*****************************IMPLIED EFFECTS - END********************************/

    /* first sweep: only count the space we need for the fact arrays !
     */
    if ( dis_gop_conn[n_op].num_E > 0 ) {
      for ( i = 0; i < dis_gop_conn[n_op].num_E; i++ ) {
	ef = dis_gop_conn[n_op].E[i];
	for ( j = 0; j < dis_gef_conn[ef].num_PC; j++ ) {
	  dis_gft_conn[dis_gef_conn[ef].PC[j]].num_PC++;
	}
 	for ( j = 0; j < dis_gef_conn[ef].num_A; j++ ) {
	  dis_gft_conn[dis_gef_conn[ef].A[j]].num_A++;
	}
	for ( j = 0; j < dis_gef_conn[ef].num_D; j++ ) {
	  dis_gft_conn[dis_gef_conn[ef].D[j]].num_D++;
	}
	/* similar increments for flconn
	 */
	for ( j = 0; j < dis_gef_conn[ef].num_f_PC; j++ ) {
	  dis_gfl_conn[dis_gef_conn[ef].f_PC_fl[j]].num_PC++;
	}
 	for ( j = 0; j < dis_gef_conn[ef].num_IN; j++ ) {
	  dis_gfl_conn[dis_gef_conn[ef].IN_fl[j]].num_IN++;
	}
 	for ( j = 0; j < dis_gef_conn[ef].num_AS; j++ ) {
	  dis_gfl_conn[dis_gef_conn[ef].AS_fl[j]].num_AS++;
	}
      }
    }

    n_op++;
  }

  /*****************************FLCONN********************************/
  for ( i = 0; i < dis_gnum_ft_conn; i++ ) {
    if ( dis_gft_conn[i].num_PC > 0 ) {
      dis_gft_conn[i].PC = ( int * ) calloc( dis_gft_conn[i].num_PC, sizeof( int ) );
    }
    dis_gft_conn[i].num_PC = 0;
    if ( dis_gft_conn[i].num_A > 0 ) {
      dis_gft_conn[i].A = ( int * ) calloc( dis_gft_conn[i].num_A, sizeof( int ) );
    }
    dis_gft_conn[i].num_A = 0;
    if ( dis_gft_conn[i].num_D > 0 ) {
      dis_gft_conn[i].D = ( int * ) calloc( dis_gft_conn[i].num_D, sizeof( int ) );
    }
    dis_gft_conn[i].num_D = 0;
  }
  for ( i = 0; i < dis_gnum_ef_conn; i++ ) {
    if ( dis_gef_conn[i].removed ) continue;
    for ( j = 0; j < dis_gef_conn[i].num_PC; j++ ) {
      dis_gft_conn[dis_gef_conn[i].PC[j]].PC[dis_gft_conn[dis_gef_conn[i].PC[j]].num_PC++] = i;
    }
    for ( j = 0; j < dis_gef_conn[i].num_A; j++ ) {
      dis_gft_conn[dis_gef_conn[i].A[j]].A[dis_gft_conn[dis_gef_conn[i].A[j]].num_A++] = i;
    }
    for ( j = 0; j < dis_gef_conn[i].num_D; j++ ) {
      dis_gft_conn[dis_gef_conn[i].D[j]].D[dis_gft_conn[dis_gef_conn[i].D[j]].num_D++] = i;
    }
  }
  /*****************************FTCONN - END********************************/


  /*****************************FLCONN********************************/
  /* similar thing for flconn
   */
  for ( i = 0; i < dis_gnum_fl_conn; i++ ) {
    if ( dis_gfl_conn[i].num_PC > 0 ) {
      dis_gfl_conn[i].PC = ( int * ) calloc( dis_gfl_conn[i].num_PC, sizeof( int ) );
    }
    dis_gfl_conn[i].num_PC = 0;
    if ( dis_gfl_conn[i].num_IN > 0 ) {
      dis_gfl_conn[i].IN = ( int * ) calloc( dis_gfl_conn[i].num_IN, sizeof( int ) );
      dis_gfl_conn[i].IN_fl_ = ( int * ) calloc( dis_gfl_conn[i].num_IN, sizeof( int ) );
      dis_gfl_conn[i].IN_c = ( float * ) calloc( dis_gfl_conn[i].num_IN, sizeof( float ) );
    }
    dis_gfl_conn[i].num_IN = 0;
    if ( dis_gfl_conn[i].num_AS > 0 ) {
      dis_gfl_conn[i].AS = ( int * ) calloc( dis_gfl_conn[i].num_AS, sizeof( int ) );
      dis_gfl_conn[i].AS_fl_ = ( int * ) calloc( dis_gfl_conn[i].num_AS, sizeof( int ) );
      dis_gfl_conn[i].AS_c = ( float * ) calloc( dis_gfl_conn[i].num_AS, sizeof( float ) );
    }
    dis_gfl_conn[i].num_AS = 0;
  }
  for ( i = 0; i < dis_gnum_ef_conn; i++ ) {
    if ( dis_gef_conn[i].removed ) continue;
    /* record the strongest requirement
     */
    for ( j = 0; j < dis_gef_conn[i].num_f_PC; j++ ) {
      fl = dis_gef_conn[i].f_PC_fl[j];
/*      val = dis_gef_conn[i].f_PC_c[j];
      comp = dis_gef_conn[i].f_PC_comp[j];*/
      dis_gfl_conn[fl].PC[dis_gfl_conn[fl].num_PC++] = i;
/*      if ( dis_gfl_conn[fl].max_needed_comp == IGUAL ||
	   val > dis_gfl_conn[fl].max_needed_c ) {
	dis_gfl_conn[fl].max_needed_comp = comp;
	dis_gfl_conn[fl].max_needed_c = val;
	continue;
      }
      if ( comp == GE &&
	   val == dis_gfl_conn[fl].max_needed_c ) {
	dis_gfl_conn[fl].max_needed_comp = GE;
      }*/
    }
    /* insert increasers by decreasing amount --> 
     * "best" - at least for constant part - are first!
     */
    for ( j = 0; j < dis_gef_conn[i].num_IN; j++ ) {
      fl = dis_gef_conn[i].IN_fl[j];
      val = dis_gef_conn[i].IN_c[j];
      for ( k = 0; k < dis_gfl_conn[fl].num_IN; k++ ) {
	if ( dis_gfl_conn[fl].IN_c[k] < val ) break;
      }
      for ( l = dis_gfl_conn[fl].num_IN; l > k; l-- ) {
	dis_gfl_conn[fl].IN[l] = dis_gfl_conn[fl].IN[l-1];
	dis_gfl_conn[fl].IN_fl_[l] = dis_gfl_conn[fl].IN_fl_[l-1];
	dis_gfl_conn[fl].IN_c[l] = dis_gfl_conn[fl].IN_c[l-1];
      }
      dis_gfl_conn[fl].IN[k] = i;
      dis_gfl_conn[fl].IN_fl_[k] = dis_gef_conn[i].IN_fl_[j];/* the rh fluent */
      dis_gfl_conn[fl].IN_c[k] = val;
      dis_gfl_conn[fl].num_IN++;
    }
    /* insert assigners by decreasing amount --> 
     * "best" - at least for constant part - are first!
     */
    for ( j = 0; j < dis_gef_conn[i].num_AS; j++ ) {
      fl = dis_gef_conn[i].AS_fl[j];
      val = dis_gef_conn[i].AS_c[j];
      for ( k = 0; k < dis_gfl_conn[fl].num_AS; k++ ) {
	if ( dis_gfl_conn[fl].AS_c[k] < val ) break;
      }
      for ( l = dis_gfl_conn[fl].num_AS; l > k; l-- ) {
	dis_gfl_conn[fl].AS[l] = dis_gfl_conn[fl].AS[l-1];
	dis_gfl_conn[fl].AS_fl_[l] = dis_gfl_conn[fl].AS_fl_[l-1];
	dis_gfl_conn[fl].AS_c[l] = dis_gfl_conn[fl].AS_c[l-1];
      }
      dis_gfl_conn[fl].AS[k] = i;
      dis_gfl_conn[fl].AS_fl_[k] = dis_gef_conn[i].AS_fl_[j];/* the rh fluent */
      dis_gfl_conn[fl].AS_c[k] = val;
      dis_gfl_conn[fl].num_AS++;
    }
  }
  /*****************************FLCONN - END********************************/


  /*****************************GOAL********************************/
// Chih-Wei PDDL3
  dis_gflogic_goal = ( int * ) calloc( dis_gnum_ft_conn, sizeof( int ) );
  for ( j = 0; j < dis_gnum_logic_goal; j++ ) {
    for ( k = 0; k < dis_gnum_flogic_goal; k++ ) {
      if ( dis_gflogic_goal[k] == dis_glogic_goal[j] ) break;
    }
    if ( k < dis_gnum_flogic_goal ) continue;
    dis_gflogic_goal[dis_gnum_flogic_goal++] = dis_glogic_goal[j];
  }
  /* numeric part
   */
  
  // Y. Chen
  
  /*
  dis_gfnumeric_goal_comp = ( dis_Comparator * ) calloc( dis_gnum_lnf_goal, sizeof( dis_Comparator ) );
  dis_gfnumeric_goal_fl = ( int * ) calloc( dis_gnum_lnf_goal, sizeof( int ) );
  dis_gfnumeric_goal_c = ( float * ) calloc( dis_gnum_lnf_goal, sizeof( float ) );
  */
  
  dis_gfnumeric_goal_comp = ( dis_Comparator * ) calloc( dis_gnum_fl_conn, sizeof( dis_Comparator ) );
  dis_gfnumeric_goal_fl = ( int * ) calloc( dis_gnum_fl_conn, sizeof( int ) );
  dis_gfnumeric_goal_c = ( float * ) calloc( dis_gnum_fl_conn, sizeof( float ) );
  
  for ( j = 0; j < dis_gnum_lnf_goal; j++ ) {
    if ( dis_glnf_goal_lh[j]->num_pF != 1 ) {
      printf("\n\nnon 1 card. in comp lh final goal copyover.\n\n");
      exit( 1 );
    }
    for ( k = 0; k < dis_gnum_fnumeric_goal; k++ ) {
      if ( dis_gfnumeric_goal_fl[k] == dis_glnf_goal_lh[j]->pF[0] ) break;
    }
    if ( k < dis_gnum_fnumeric_goal ) {
      if ( dis_glnf_goal_rh[j] < dis_gfnumeric_goal_c[k] ) continue;
      if ( dis_glnf_goal_rh[j] > dis_gfnumeric_goal_c[k] ) {
	dis_gfnumeric_goal_comp[k] = dis_glnf_goal_comp[j];
	dis_gfnumeric_goal_c[k] = dis_glnf_goal_rh[j];
	continue;
      }
      if ( dis_glnf_goal_comp[j] == GE ) {
	dis_gfnumeric_goal_comp[k] = GE;
      }
    } else {
      dis_gfnumeric_goal_comp[dis_gnum_fnumeric_goal] = dis_glnf_goal_comp[j];
      dis_gfnumeric_goal_fl[dis_gnum_fnumeric_goal] = dis_glnf_goal_lh[j]->pF[0];
      dis_gfnumeric_goal_c[dis_gnum_fnumeric_goal++] = dis_glnf_goal_rh[j];
    }
  }
  dis_gfnumeric_goal_direct_comp = ( dis_Comparator * ) calloc( dis_gnum_fl_conn, sizeof( dis_Comparator ) );
  for ( j = 0; j < dis_gnum_fl_conn; j++ ) {
    dis_gfnumeric_goal_direct_comp[j] = IGUAL;
  }
  dis_gfnumeric_goal_direct_c = ( float * ) calloc( dis_gnum_fl_conn, sizeof( float ) );
  for ( k = 0; k < dis_gnum_fnumeric_goal; k++ ) {
    dis_gfnumeric_goal_direct_comp[dis_gfnumeric_goal_fl[k]] = dis_gfnumeric_goal_comp[k];
    dis_gfnumeric_goal_direct_c[dis_gfnumeric_goal_fl[k]] = dis_gfnumeric_goal_c[k];
  }
  /*****************************GOAL - END********************************/

  /* update of the max needed values
   */
/*  for ( k = 0; k < dis_gnum_fnumeric_goal; k++ ) {
    fl = dis_gfnumeric_goal_fl[k];
    val = dis_gfnumeric_goal_c[k];
    comp = dis_gfnumeric_goal_comp[k];
    if ( dis_gfl_conn[fl].max_needed_comp == IGUAL ||
	 val > dis_gfl_conn[fl].max_needed_c ) {
      dis_gfl_conn[fl].max_needed_comp = comp;
      dis_gfl_conn[fl].max_needed_c = val;
      continue;
    }
    if ( comp == GE &&
	 val == dis_gfl_conn[fl].max_needed_c ) {
      dis_gfl_conn[fl].max_needed_comp = GE;
    }
  }*/
  /* now, finally, determine the max_rh_needed values!!
   * for efficiency, do that by stepping over effects and updating
   * all respective rh fluent values.
   */
/*  for ( i = 0; i < dis_gnum_ef_conn; i++ ) {
    if ( dis_gef_conn[i].removed ) continue;*/
    /* increasing effs; update to requ (>) -c
     */
/*    for ( j = 0; j < dis_gef_conn[i].num_IN; j++ ) {
      fl = dis_gef_conn[i].IN_fl[j];
      fl_ = dis_gef_conn[i].IN_fl_[j];
      val = dis_gef_conn[i].IN_c[j];
      if ( fl_ == -1 || 
	   val > 0 || 
	   dis_gfl_conn[fl].max_needed_comp == IGUAL ) continue;
      if ( !dis_gfl_conn[fl_].is_rh_needed ) {
	dis_gfl_conn[fl_].is_rh_needed = dis_TRUE;
	dis_gfl_conn[fl_].max_rh_needed = val * (-1);
	continue;
      }
      if ( dis_gfl_conn[fl_].max_rh_needed < (val * (-1)) ) {
	dis_gfl_conn[fl_].max_rh_needed = val * (-1);
      }
    }*/
    /* assigning effs; update requ (>) needed(fl) - c
     */
/*    for ( j = 0; j < dis_gef_conn[i].num_AS; j++ ) {
      fl = dis_gef_conn[i].AS_fl[j];
      fl_ = dis_gef_conn[i].AS_fl_[j];
      val = dis_gef_conn[i].AS_c[j];
      if ( fl_ == -1 || 
	   dis_gfl_conn[fl].max_needed_comp == IGUAL ) continue;
      if ( !dis_gfl_conn[fl_].is_rh_needed ) {
	dis_gfl_conn[fl_].is_rh_needed = dis_TRUE;
	dis_gfl_conn[fl_].max_rh_needed = dis_gfl_conn[fl].max_needed_c - val;
	continue;
      }
      if ( dis_gfl_conn[fl_].max_rh_needed < (dis_gfl_conn[fl].max_needed_c - val) ) {
	dis_gfl_conn[fl_].max_rh_needed = dis_gfl_conn[fl].max_needed_c - val;
      }
    }
  }*/

  for (i=0;i<dis_gnum_predicates;i++)
    if (dis_gis_added[i] || dis_gis_deleted[i])
      if (dis_garity[i] > 0)
        break;

// Ruoyun
  if (i < dis_gnum_predicates && num_invariantGroup > 0)
  //|| GpG.SearchModal == -1000 || GpG.SearchModal == -1002 || GpG.SearchModal == -1003 || GpG.SearchModal == -1005 || GpG.SearchModal < 0)
 {
    build_transitive_graph();
//    print_out_transitive_graph_set();
  }

  /* Chih-Wei durative actions */
  if (GpG.is_durative)
  {
    for (i=0;i<dis_gnum_ef_conn;i++)
    {
        //a = dis_gop_conn[i].action;
        a = dis_gop_conn[dis_gef_conn[i].op].action;
        
        if (!a->name || strncmp(a->name, "timed-initial-literal",
        strlen("timed-initial-literal")) == dis_SAME)
        {
            dis_gef_conn[i].removed = dis_FALSE;
            dis_gef_conn[i].duration = 0;
            continue;
        }
        if (dis_gef_conn[i].DPop || a->name[0] == '_')
          dis_gef_conn[i].removed = dis_FALSE;
        for (l=0;l<gnum_das&&strcmp(table[l].name, a->name)!=dis_SAME;l++)
            ;
            dis_gef_conn[i].duration = dis_BIG_INT;
        if (l == gnum_das) {
            continue;
        }
        if (table[l].fluent.function == -1)
            memcpy(&(dis_gef_conn[i].duration), table[l].fluent.args, sizeof(float));
        else 
        if (table[l].fluent.function >= 0)
        {
            fl = table[l].fluent.function;
            for ( j = 0; j < dis_gnum_initial_function[fl]; j++ )
            {
                for ( k = 0; k < dis_gf_arity[fl]; k++ )
                    if ( dis_ginitial_function[fl][j].fluent.args[k] !=
                       a->name_inst_table[table[l].fluent.args[k]])
                       break;
                if ( k < dis_gf_arity[fl] )
                    continue;
                if (table[l].flag)
                dis_gef_conn[i].duration = -l;
                else
                {
                  if (GpG.SearchModal == -1006)
                  {
                    if (strstr(a->name, "IT"))
                      dis_gef_conn[i].duration = 2/dis_ginitial_function[fl][j].value;
                    else
                      dis_gef_conn[i].duration = 1/dis_ginitial_function[fl][j].value;
                  }
                  else
                    dis_gef_conn[i].duration = dis_ginitial_function[fl][j].value;
                }
                break;
            }
        }
    }
  }
  else
    for (i=0;i<dis_gnum_ef_conn;i++)
    {
      a = dis_gop_conn[dis_gef_conn[i].op].action;
      if (!a->name)
      {
        dis_gef_conn[i].removed = dis_FALSE;
        dis_gef_conn[i].duration = 0;
        continue;
      }
      if (dis_gef_conn[i].DPop || a->name[0] == '_')
      {
        dis_gef_conn[i].removed = dis_FALSE;
        dis_gef_conn[i].duration = 0.001;
      }
      else
        dis_gef_conn[i].duration = 1;
    }

/*  fprintf(stderr, "is_negative %d\n", GpG.is_negative);
  fprintf(stderr, "is_numeric_fluents %d\n", GpG.is_numeric_fluents);
  fprintf(stderr, "is_durative %d\n", GpG.is_durative);
  fprintf(stderr, "is_deripred %d\n", GpG.is_deripred);
  fprintf(stderr, "is_til %d\n", GpG.is_til);
  fprintf(stderr, "is_constraints %d\n", GpG.is_constraints);
  fprintf(stderr, "is_preferences %d\n", GpG.is_preferences);
  fprintf(stderr, "is_goal_utilities %d\n", GpG.is_goal_utilities);
  fprintf(stderr, "is_action_costs %d\n", GpG.is_action_costs);
  fprintf(stderr, "dis_gconditional_effects %d\n", dis_gconditional_effects);
  fprintf(stderr, "dis_goptimization_established  %d\n", dis_goptimization_established);
  fprintf(stderr, "SearchModal = %d %d\n", GpG.SearchModal, GpG.SecondaryModal);*/
  detect_producible_variables();
  find_bottleneck_variables();
  if ( dis_gcmd_line.display_info == 125 ) {
    printf("\n\ncreated connectivity graph as follows:");

    printf("\n\n------------------OP ARRAY:-----------------------");
    for ( i = 0; i < dis_gnum_op_conn; i++ ) {
      printf("\n\nOP: ");
      dis_print_op_name( i );
      printf("\n----------EFFS:");
      for ( j = 0; j < dis_gop_conn[i].num_E; j++ ) {
	printf("\neffect %d", dis_gop_conn[i].E[j]);
      }
    }
    
    printf("\n\n-------------------EFFECT ARRAY:----------------------");
    for ( i = 0; i < dis_gnum_ef_conn; i++ ) {
      printf("\n\neffect %d of op %d cost %f duration %f: ", i, dis_gef_conn[i].op, dis_gef_conn[i].cost, dis_gef_conn[i].duration);
        
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
      if ( dis_gef_conn[i].illegal ) continue;
      printf("\n----------ADDS:");
      for ( j = 0; j < dis_gef_conn[i].num_A; j++ ) {
	printf("\n");
	dis_print_ft_name( dis_gef_conn[i].A[j] );
      }
      printf("\n----------DELS:");
      for ( j = 0; j < dis_gef_conn[i].num_D; j++ ) {
	printf("\n");
	dis_print_ft_name( dis_gef_conn[i].D[j] );
      }
      printf("\n----------INCREASE:");
      for ( j = 0; j < dis_gef_conn[i].num_IN; j++ ) {
	printf("\n");
	dis_print_fl_name( dis_gef_conn[i].IN_fl[j] );
	printf(" by ");
	if ( dis_gef_conn[i].IN_fl_[j] >= 0 ) {
	  dis_print_fl_name( dis_gef_conn[i].IN_fl_[j] );
	  printf(" + %f", dis_gef_conn[i].IN_c[j]);
	} else {
	  printf("%f", dis_gef_conn[i].IN_c[j]);
	}
      }
      printf("\n----------ASSIGN:");
      for ( j = 0; j < dis_gef_conn[i].num_AS; j++ ) {
	printf("\n");
	dis_print_fl_name( dis_gef_conn[i].AS_fl[j] );
	printf(" to ");
	if ( dis_gef_conn[i].AS_fl_[j] >= 0 ) {
	  dis_print_fl_name( dis_gef_conn[i].AS_fl_[j] );
	  printf(" + %f", dis_gef_conn[i].AS_c[j]);
	} else {
	  printf("%f", dis_gef_conn[i].AS_c[j]);
	}
      }
      printf("\n----------IMPLIEDS:");
      for ( j = 0; j < dis_gef_conn[i].num_I; j++ ) {
	printf("\nimplied effect %d of op %d: ", 
	       dis_gef_conn[i].I[j], dis_gef_conn[dis_gef_conn[i].I[j]].op);
	dis_print_op_name( dis_gef_conn[dis_gef_conn[i].I[j]].op );
      }
    }
    
    printf("\n\n----------------------FT ARRAY:-----------------------------");
    for ( i = 0; i < dis_gnum_ft_conn; i++ ) {
      printf("\n\nFT: ");
      dis_print_ft_name( i );
      printf(" rand: %d", dis_gft_conn[i].rand);
      printf("\n----------PRE COND OF:");
      for ( j = 0; j < dis_gft_conn[i].num_PC; j++ ) {
	printf("\neffect %d", dis_gft_conn[i].PC[j]);
	printf(" - op "); dis_print_op_name( dis_gef_conn[dis_gft_conn[i].PC[j]].op );
      }
      printf("\n----------ADD BY:");
      for ( j = 0; j < dis_gft_conn[i].num_A; j++ ) {
	printf("\neffect %d", dis_gft_conn[i].A[j]);
	printf(" - op "); dis_print_op_name( dis_gef_conn[dis_gft_conn[i].A[j]].op );
      }
      printf("\n----------DEL BY:");
      for ( j = 0; j < dis_gft_conn[i].num_D; j++ ) {
	printf("\neffect %d", dis_gft_conn[i].D[j]);
	printf(" - op "); dis_print_op_name( dis_gef_conn[dis_gft_conn[i].D[j]].op );
      }
    }
    
    printf("\n\n----------------------FLUENT ARRAY:-----------------------------");
    for ( i = 0; i < dis_gnum_fl_conn; i++ ) {
      printf("\n\nFL: ");
      dis_print_fl_name( i );
      printf("\n----------PRE COND OF:");
      for ( j = 0; j < dis_gfl_conn[i].num_PC; j++ ) {
	printf("\neffect %d", dis_gfl_conn[i].PC[j]);
	printf(" - op "); dis_print_op_name( dis_gef_conn[dis_gfl_conn[i].PC[j]].op );
      }
      printf("\n----------INCREASED BY:");
      for ( j = 0; j < dis_gfl_conn[i].num_IN; j++ ) {
	if ( dis_gfl_conn[i].IN_fl_[j] == -1 ) {
	  printf("\neffect %d  ---  %f", dis_gfl_conn[i].IN[j], dis_gfl_conn[i].IN_c[j]);
	  printf("  ---  op "); dis_print_op_name( dis_gef_conn[dis_gfl_conn[i].IN[j]].op );
	} else {
	  printf("\neffect %d  ---  ", dis_gfl_conn[i].IN[j]);
	  dis_print_fl_name( dis_gfl_conn[i].IN_fl_[j] );
	  printf(" + %f", dis_gfl_conn[i].IN_c[j]);
	  printf("  ---  op "); dis_print_op_name( dis_gef_conn[dis_gfl_conn[i].IN[j]].op );
	}
      }
      printf("\n----------ASSIGNED BY:");
      for ( j = 0; j < dis_gfl_conn[i].num_AS; j++ ) {
	if ( dis_gfl_conn[i].AS_fl_[j] == -1 ) {
	  printf("\neffect %d  ---  %f", dis_gfl_conn[i].AS[j], dis_gfl_conn[i].AS_c[j]);
	  printf("  ---  op "); dis_print_op_name( dis_gef_conn[dis_gfl_conn[i].AS[j]].op );
	} else {
	  printf("\neffect %d  ---  ", dis_gfl_conn[i].AS[j]);
	  dis_print_fl_name( dis_gfl_conn[i].AS_fl_[j] );
	  printf(" + %f", dis_gfl_conn[i].AS_c[j]);
	  printf("  ---  op "); dis_print_op_name( dis_gef_conn[dis_gfl_conn[i].AS[j]].op );
	}
      }
      if ( dis_gfl_conn[i].artificial ) {
	printf("\n----------ARTIFICIAL Fdis_OR:");
	for ( j = 0; j < dis_gfl_conn[i].num_lnf; j++ ) {
	  printf(" %f*", dis_gfl_conn[i].lnf_C[j]);
	  dis_print_fl_name( dis_gfl_conn[i].lnf_F[j] );
	  if ( j < dis_gfl_conn[i].num_lnf - 1 ) {
	    printf(" +");
	  }
	}
      } else {
	printf("\n----------REAL");
      }
/*      printf("\n----------MAX NEEDED:");
      if ( dis_gfl_conn[i].max_needed_comp == IGUAL ) {
	printf(" IGUAL");
      }
      if ( dis_gfl_conn[i].max_needed_comp == GEQ ) {
	printf(" >= %f", dis_gfl_conn[i].max_needed_c);
      }
      if ( dis_gfl_conn[i].max_needed_comp == GE ) {
	printf(" > %f", dis_gfl_conn[i].max_needed_c);
      }      
      printf("\n----------RH NEEDED:");
      if ( !dis_gfl_conn[i].is_rh_needed ) {
	printf(" NO");
      } else {
	printf(" > %f", dis_gfl_conn[i].max_rh_needed);
      }*/
    }

    printf("\n\n----------------------GOAL:-----------------------------");
    for ( j = 0; j < dis_gnum_flogic_goal; j++ ) {
      printf("\n");
      dis_print_ft_name( dis_gflogic_goal[j] );
    }
    for ( j = 0; j < dis_gnum_fnumeric_goal; j++ ) {
      printf("\n");
      dis_print_fl_name( dis_gfnumeric_goal_fl[j] );
      if ( dis_gfnumeric_goal_comp[j] == GEQ ) {
	printf(" >= ");
      } else {
	printf(" > ");
      }
      printf("%f", dis_gfnumeric_goal_c[j]);
    }
    printf("\nDIRECT: ");
    for ( j = 0; j < dis_gnum_fl_conn; j++ ) {
      if ( dis_gfnumeric_goal_direct_comp[j] == IGUAL ) {
	printf("IGUAL  |  ");
      }
      if ( dis_gfnumeric_goal_direct_comp[j] == GEQ ) {
	printf(">= %f  |  ", dis_gfnumeric_goal_direct_c[j]);
      }
      if ( dis_gfnumeric_goal_direct_comp[j] == GE ) {
	printf("> %f  |  ", dis_gfnumeric_goal_direct_c[j]);
      }
    }
    
    printf("\n\n");
  }
  
}



