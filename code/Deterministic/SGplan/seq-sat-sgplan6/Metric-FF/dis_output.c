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

 * File: dis_output.c 

 * Description: modified from output.c in Metric-FF

 *

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
 * File: output.c
 * Description: printing info out
 *
 * Author: Joerg Hoffmann
 *
 *********************************************************************/ 





#include "dis_ff.h"

#include "dis_output.h"







/* parsing
 */







void dis_print_dis_FactList( dis_FactList *list, char *sepf, char *sept )

{

  dis_FactList *i_list;
  dis_TokenList *i_tl;
    
  if ( list ) {
    i_tl = list->item;
    if (NULL == i_tl || NULL == i_tl->item) {
      printf("empty");
    } else {
      printf("%s", i_tl->item);
      i_tl = i_tl->next;
    }
    
    while (NULL != i_tl) {
      if (NULL != i_tl->item) {
	printf("%s%s", sept, i_tl->item);
      }
      i_tl = i_tl->next;
    }
    
    for ( i_list = list->next; i_list; i_list = i_list->next ) {
      printf("%s", sepf);
      i_tl = i_list->item;
      if (NULL == i_tl || NULL == i_tl->item) {
	printf("empty");
      } else {
	printf("%s", i_tl->item);
	i_tl = i_tl->next;
      }
      
      while (NULL != i_tl) {
	if (NULL != i_tl->item) {
	  printf("%s%s", sept, i_tl->item);
	}
	i_tl = i_tl->next;
      }
    }
  }

}



void dis_print_hidden_dis_TokenList( dis_TokenList *list, char *sep )

{

  dis_TokenList *i_tl;

  i_tl = list;
  if (NULL!=i_tl) {
    printf("%s", i_tl->item);
    i_tl = i_tl->next;
  } else {
    printf("empty");
  }
  
  while (NULL != i_tl) {
    printf("%s%s", sep, i_tl->item);
    i_tl = i_tl->next;
  }
  
}



void dis_print_indent( int indent )

{

  int i;
  for (i=0;i<indent;i++) {
    printf(" ");
  }

}



void dis_print_dis_Parsedis_ExpNode( dis_Parsedis_ExpNode *n )

{

  if ( !n ) return;

  switch ( n->connective) {
  case AD:
    printf("(+ ");
    dis_print_dis_Parsedis_ExpNode( n->leftson );
    dis_print_dis_Parsedis_ExpNode( n->rightson );
    printf(")");
    break;
  case SU:
    printf("(- ");
    dis_print_dis_Parsedis_ExpNode( n->leftson );
    dis_print_dis_Parsedis_ExpNode( n->rightson );
    printf(")");
    break;
  case MU:
    printf("(* ");
    dis_print_dis_Parsedis_ExpNode( n->leftson );
    dis_print_dis_Parsedis_ExpNode( n->rightson );
    printf(")");
    break;
  case DI:
    printf("(/ ");
    dis_print_dis_Parsedis_ExpNode( n->leftson );
    dis_print_dis_Parsedis_ExpNode( n->rightson );
    printf(")");
    break;
  case MINUS:
    printf("(- ");
    dis_print_dis_Parsedis_ExpNode( n->leftson );
    printf(")");
    break;
  case NUMBER:
    printf("%s", n->atom->item);
    break;
  case FHEAD:
    printf("(");
    dis_print_hidden_dis_TokenList(n->atom, " ");
    printf(")");
    break;
  case VIO:
    printf(" (VIO ");
    dis_print_hidden_dis_TokenList(n->atom, " ");
    printf(")");
    break;
  default:
    printf("\n\nprint Parseexpnode: wrong specifier %d",
	   n->connective);
  }

}



void dis_print_dis_PlNode( dis_PlNode *plnode, int indent )

{

  dis_PlNode *i_son;

  if ( !plnode ) {
    printf("none\n");
    return;
  }
  
  switch (plnode->connective) {
  case dis_ALL: 
//    printf("dis_ALL %s : %s\n", plnode->atom->item,
//	    plnode->atom->next->item);
//    dis_print_indent(indent);
    printf("dis_ALL (   ");
    dis_print_dis_PlNode(plnode->sons,indent+4);
    dis_print_indent(indent);
    printf(")\n");
    break;
  case dis_EX:
//    printf("dis_EX  %s : %s\n", plnode->atom->item,
//	    plnode->atom->next->item);
//    dis_print_indent(indent);
    printf("dis_EX (   ");
    dis_print_dis_PlNode(plnode->sons,indent+4);
    dis_print_indent(indent);
    printf(")\n");
    break;
  case dis_AND: 
    printf("A(  ");
    dis_print_dis_PlNode(plnode->sons, indent+4);
    if ( plnode->sons ) {
      for ( i_son = plnode->sons->next; i_son!=NULL; i_son = i_son->next ) {
	dis_print_indent(indent);
	printf("dis_AND ");
	dis_print_dis_PlNode(i_son,indent+4);
      }
    }
    dis_print_indent(indent);      
    printf(")\n");
    break;
  case dis_OR:  
    printf("O(  ");
    dis_print_dis_PlNode(plnode->sons, indent+4);
    for ( i_son = plnode->sons->next; i_son!=NULL; i_son = i_son->next ) {
      dis_print_indent(indent);
      printf("dis_OR ");
      dis_print_dis_PlNode(i_son,indent+4);
    }
    dis_print_indent(indent);      
    printf(")\n");
    break;
  case dis_WHEN:
    printf("IF   ");
    dis_print_dis_PlNode(plnode->sons,indent+5);
    dis_print_indent(indent);
    printf("THEN ");
    dis_print_dis_PlNode(plnode->sons->next,indent+5);
    dis_print_indent(indent);
    printf("ENDIF\n");
    break;
  case dis_NOT:
    if (dis_ATOM==plnode->sons->connective) {
      printf("dis_NOT ");
      dis_print_dis_PlNode(plnode->sons,indent+4);
    } else {
      printf("dis_NOT(");
      dis_print_dis_PlNode(plnode->sons,indent+4);
      dis_print_indent(indent+3);
      printf(")\n");
    }
    break;
  case dis_ATOM:
    printf("(");
    dis_print_hidden_dis_TokenList(plnode->atom, " ");
    printf(")\n");
    break;
  case dis_TRU:
     printf("(dis_TRUE)\n");
     break;
  case dis_FAL:
     printf("(dis_FALSE)\n");
     break;   
  case dis_COMP:
    switch (plnode->comp) {
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
      printf("\n\nillegal comp in parse tree!\n\n");
      exit( 1 );
    }
    dis_print_dis_Parsedis_ExpNode( plnode->lh );
    dis_print_dis_Parsedis_ExpNode( plnode->rh );
    printf(")\n");
    break;
  case dis_NEF:
    switch (plnode->neft) {
    case ASSIGN:
      printf("(assign ");
      break;
    case SCALE_UP:
      printf("(scale-up ");
      break;
    case SCALE_DOWN:
      printf("(scale-down ");
      break;
    case INCREASE:
      printf("(increase ");
      break;
    case DECREASE:
      printf("(decrease ");
      break;
    }
    dis_print_dis_Parsedis_ExpNode( plnode->lh );
    dis_print_dis_Parsedis_ExpNode( plnode->rh );
    printf(")\n");
    break;
  default:
    printf("\n***** ERRdis_OR ****");
    printf("\nprint_plnode: %d > Wrong Node specifier\n", plnode->connective);
    exit(1);
  }     

} 



void dis_print_plops( dis_Pldis_Operator *plop )

{

  dis_Pldis_Operator *i_plop;
  int count = 0;

  if ( !plop ) {
    printf("none\n");
  }

  for ( i_plop = plop; i_plop!=NULL; i_plop = i_plop->next ) {
    printf("\nOPERATdis_OR ");
    printf("%s", i_plop->name);
    printf("\nparameters: (%d real)\n", i_plop->number_of_real_params);
    dis_print_dis_FactList ( i_plop->params, "\n", " : ");
    printf("\n\npreconditions:\n");
    dis_print_dis_PlNode(i_plop->preconds, 0);
    printf("effects:\n");
    dis_print_dis_PlNode(i_plop->effects, 0);
    printf("\n-----\n");
    count++;
  }
  printf("\nAnzahl der dis_Operatoren: %d\n", count);

}



void dis_print_dis_ExpNode( dis_ExpNode *n )

{

  if ( !n ) return;

  switch ( n->connective) {
  case AD:
    printf("(+ ");
    dis_print_dis_ExpNode( n->leftson );
    dis_print_dis_ExpNode( n->rightson );
    printf(")");
    break;
  case SU:
    printf("(- ");
    dis_print_dis_ExpNode( n->leftson );
    dis_print_dis_ExpNode( n->rightson );
    printf(")");
    break;
  case MU:
    printf("(* ");
    dis_print_dis_ExpNode( n->leftson );
    dis_print_dis_ExpNode( n->rightson );
    printf(")");
    break;
  case DI:
    printf("(/ ");
    dis_print_dis_ExpNode( n->leftson );
    dis_print_dis_ExpNode( n->rightson );
    printf(")");
    break;
  case MINUS:
    printf("(- ");
    dis_print_dis_ExpNode( n->son );
    printf(")");
    break;
  case NUMBER:
    printf("%.2f", n->value);
    break;
  case FHEAD:
    if ( n->fluent ) {
      dis_print_dis_Fluent( n->fluent );
    } else {
      if ( n->fl >= 0 ) {
	printf(" %.2f*", n->c);
	dis_print_fl_name( n->fl );
      } else {
	printf("[UNDEF]");
      }
    }
    break;
  default:
    printf("\n\nprint Expnode: wrong specifier %d",
	   n->connective);
  }

}



void dis_print_Wff( dis_WffNode *n, int indent )

{

  dis_WffNode *i;

  if ( !n ) {
    printf("none\n");
    return;
  }
  
  switch (n->connective) {
  case dis_ALL: 
    printf("dis_ALL x%d (%s): %s\n", n->var, n->var_name,
	    dis_gtype_names[n->var_type]);
    dis_print_indent(indent);
    printf("(   ");
    dis_print_Wff(n->son,indent+4);
    dis_print_indent(indent);
    printf(")\n");
    break;
  case dis_EX:
    printf("dis_EX  x%d (%s) : %s\n",  n->var, n->var_name,
	    dis_gtype_names[n->var_type]);
    dis_print_indent(indent);
    printf("(   ");
    dis_print_Wff(n->son,indent+4);
    dis_print_indent(indent);
    printf(")\n");
    break;
  case dis_AND: 
    printf("A(  ");
    dis_print_Wff(n->sons, indent+4);
    if ( n->sons ) {
      for ( i = n->sons->next; i!=NULL; i = i->next ) {
	if ( !i->prev ) {
	  printf("\nprev in dis_AND not correctly set!\n\n");
	  exit( 1 );
	}
	dis_print_indent(indent);
	printf("dis_AND ");
	dis_print_Wff(i,indent+4);
      }
    }
    dis_print_indent(indent);      
    printf(")\n");
    break;
  case dis_OR:  
    printf("O(  ");
    dis_print_Wff(n->sons, indent+4);
    for ( i = n->sons->next; i!=NULL; i = i->next ) {
      dis_print_indent(indent);
      printf("dis_OR ");
      dis_print_Wff(i,indent+4);
    }
    dis_print_indent(indent);      
    printf(")\n");
    break;
  case dis_NOT:
    if (dis_ATOM==n->son->connective) {
      printf("dis_NOT ");
      dis_print_Wff(n->son,indent+4);
    } else {
      printf("dis_NOT(");
      dis_print_Wff(n->son,indent+4);
      dis_print_indent(indent+3);
      printf(")\n");
    }
    break;
  case dis_ATOM:
    dis_print_dis_Fact(n->fact);
    if ( n->dis_NOT_p != -1 ) printf(" - translation dis_NOT");
    printf("\n");
    break;
  case dis_TRU:
     printf("(dis_TRUE)\n");
     break;
  case dis_FAL:
     printf("(dis_FALSE)\n");
     break;   
  case dis_COMP:
    switch (n->comp) {
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
      printf("\nwrong comparator of Expnodes in WFF %d\n\n", n->comp);
      exit( 1 );
    }
    dis_print_dis_ExpNode( n->lh );
    dis_print_dis_ExpNode( n->rh );
    printf(")\n");
    break;
  default:
    printf("\n***** ERRdis_OR ****");
    printf("\ndis_print_Wff: %d > Wrong Node specifier\n", n->connective);
    exit(1);
  }     

} 



void dis_print_dis_Operator( dis_Operator *o )

{

  dis_Effect *e;
  dis_Literal *l;
  dis_Numericdis_Effect *ne;
  int i, m = 0;

  printf("\n\n----------------dis_Operator %s, translated form, step 1--------------\n", o->name);

  for ( i = 0; i < o->num_vars; i++ ) {
    printf("\nx%d (%s) of type %s, removed ? %s",
	   i, o->var_names[i], dis_gtype_names[o->var_types[i]],
	   o->removed[i] ? "YES" : "NO");
  }
  printf("\ntotal params %d, real params %d\n", 
	 o->num_vars, o->number_of_real_params);

  printf("\nPreconds:\n");
  dis_print_Wff( o->preconds, 0 );

  printf("\n\ndis_Effects:");
  for ( e = o->effects; e; e = e->next ) {
    printf("\n\neffect %d, parameters %d", m++, e->num_vars);

    for ( i = 0; i < e->num_vars; i++ ) {
      printf("\nx%d (%s) of type %s",
	     o->num_vars + i, e->var_names[i], dis_gtype_names[e->var_types[i]]);
    }
    printf("\nConditions\n");
    dis_print_Wff( e->conditions, 0 );
    printf("\ndis_Effect dis_Literals");
    for ( l = e->effects; l; l = l->next ) {
      if ( l->negated ) {
	printf("\ndis_NOT ");
      } else {
	printf("\n");
      }
      dis_print_dis_Fact( &(l->fact) );
    }
    printf("\nNumeric dis_Effects");
    for ( ne = e->numeric_effects; ne; ne = ne->next ) {
      switch ( ne->neft ) {
      case ASSIGN:
	printf("\nassign ");
	break;
      case SCALE_UP:
	printf("\nscale-up ");
	break;
      case SCALE_DOWN:
	printf("\nscale-down ");
	break;
      case INCREASE:
	printf("\nincrease ");
	break;
      case DECREASE:
	printf("\ndecrease ");
	break;
      default:
	printf("\n\nprint effect: illegal neft %d\n\n", ne->neft);
	exit( 1 );
      }
      dis_print_dis_Fluent( &(ne->fluent) );
      dis_print_dis_ExpNode( ne->rh );
    }
  }

}



void dis_print_Normdis_Operator( Normdis_Operator *o )

{

  Normdis_Effect *e;
  int i, m;

  printf("\n\n----------------dis_Operator %s, normalized form--------------\n", 
	 o->operator->name);

  for ( i = 0; i < o->num_vars; i++ ) {
    printf("\nx%d of type ", i);
    dis_print_type( o->var_types[i] );
  }
  printf("\n\n%d vars removed from original operator:",
	 o->num_removed_vars);
  for ( i = 0; i < o->num_removed_vars; i++ ) {
    m = o->removed_vars[i];
    printf("\nx%d (%s) of type %s, type constraint ", m, o->operator->var_names[m], 
	   dis_gtype_names[o->operator->var_types[m]]);
    dis_print_type( o->type_removed_vars[i] );
  }

  printf("\nPreconds:\n");
  for ( i = 0; i < o->num_preconds; i++ ) {
    dis_print_dis_Fact( &(o->preconds[i]) );
    printf("\n");
  }
  for ( i = 0; i < o->num_numeric_preconds; i++ ) {
    switch ( o->numeric_preconds_comp[i] ) {
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
      printf("\nwrong comparator of Expnodes in normpre %d\n\n", 
	     o->numeric_preconds_comp[i]);
      exit( 1 );
    }
    dis_print_dis_ExpNode( o->numeric_preconds_lh[i] );
    dis_print_dis_ExpNode( o->numeric_preconds_rh[i] );
    printf(")\n");
  }

  m = 0;
  printf("\n\ndis_Effects:");
  for ( e = o->effects; e; e = e->next ) {
    printf("\n\neffect %d, parameters %d", m++, e->num_vars);

    for ( i = 0; i < e->num_vars; i++ ) {
      printf("\nx%d of type ", o->num_vars + i);
      dis_print_type( e->var_types[i] );
    }
    printf("\nConditions\n");
    for ( i = 0; i < e->num_conditions; i++ ) {
      dis_print_dis_Fact( &(e->conditions[i]) );
      printf("\n");
    }
    for ( i = 0; i < e->num_numeric_conditions; i++ ) {
      switch ( e->numeric_conditions_comp[i] ) {
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
	printf("\nwrong comparator of Expnodes in normeff %d\n\n", 
	       e->numeric_conditions_comp[i]);
	exit( 1 );
      }
      dis_print_dis_ExpNode( e->numeric_conditions_lh[i] );
      dis_print_dis_ExpNode( e->numeric_conditions_rh[i] );
      printf(")\n");
    }

    printf("\nAdds\n");
    for ( i = 0; i < e->num_adds; i++ ) {
      dis_print_dis_Fact( &(e->adds[i]) );
      printf("\n");
    }
    printf("\nDels\n");
    for ( i = 0; i < e->num_dels; i++ ) {
      dis_print_dis_Fact( &(e->dels[i]) );
      printf("\n");
    }
    for ( i = 0; i < e->num_numeric_effects; i++ ) {
      switch ( e->numeric_effects_neft[i] ) {
      case ASSIGN:
	printf("\nassign ");
	break;
      case SCALE_UP:
	printf("\nscale-up ");
	break;
      case SCALE_DOWN:
	printf("\nscale-down ");
	break;
      case INCREASE:
	printf("\nincrease ");
	break;
      case DECREASE:
	printf("\ndecrease ");
	break;
      default:
	printf("\n\nprint normop: illegal neft %d\n\n", 
	       e->numeric_effects_neft[i]);
	exit( 1 );
      }
      dis_print_dis_Fluent( &(e->numeric_effects_fluent[i]) );
      dis_print_dis_ExpNode( e->numeric_effects_rh[i] );
    }
  }

}



void dis_print_Mixeddis_Operator( Mixeddis_Operator *o )

{

  int i, m;
  dis_Effect *e;
  dis_Numericdis_Effect *ne;
  dis_Literal *l;

  printf("\n\n----------------dis_Operator %s, mixed form--------------\n", 
	 o->operator->name);
 
  for ( i = 0; i < o->operator->num_vars; i++ ) {
    printf("\nx%d = %s of type ", i, dis_gconstants[o->inst_table[i]]);
    dis_print_type( o->operator->var_types[i] );
  }

  printf("\nPreconds:\n");
  for ( i = 0; i < o->num_preconds; i++ ) {
    dis_print_dis_Fact( &(o->preconds[i]) );
    printf("\n");
  }
  for ( i = 0; i < o->num_numeric_preconds; i++ ) {
    switch ( o->numeric_preconds_comp[i] ) {
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
      printf("\nwrong comparator of Expnodes in mixedpre %d\n\n", 
	     o->numeric_preconds_comp[i]);
      exit( 1 );
    }
    dis_print_dis_ExpNode( o->numeric_preconds_lh[i] );
    dis_print_dis_ExpNode( o->numeric_preconds_rh[i] );
    printf(")\n");
  }

  m = 0;
  printf("\n\ndis_Effects:");
  for ( e = o->effects; e; e = e->next ) {
    printf("\n\neffect %d, parameters %d", m++, e->num_vars);

    for ( i = 0; i < e->num_vars; i++ ) {
      printf("\nx%d of type %s",
	     o->operator->num_vars + i, dis_gtype_names[e->var_types[i]]);
    }
    printf("\nConditions\n");
    dis_print_Wff( e->conditions, 0 );
    printf("\ndis_Effect dis_Literals");
    for ( l = e->effects; l; l = l->next ) {
      if ( l->negated ) {
	printf("\ndis_NOT ");
      } else {
	printf("\n");
      }
      dis_print_dis_Fact( &(l->fact) );
    }
    printf("\nNumeric dis_Effects");
    for ( ne = e->numeric_effects; ne; ne = ne->next ) {
      switch ( ne->neft ) {
      case ASSIGN:
	printf("\nassign ");
	break;
      case SCALE_UP:
	printf("\nscale-up ");
	break;
      case SCALE_DOWN:
	printf("\nscale-down ");
	break;
      case INCREASE:
	printf("\nincrease ");
	break;
      case DECREASE:
	printf("\ndecrease ");
	break;
      default:
	printf("\n\nprint effect: illegal neft %d\n\n", ne->neft);
	exit( 1 );
      }
      dis_print_dis_Fluent( &(ne->fluent) );
      dis_print_dis_ExpNode( ne->rh );
    }
  }

}



void dis_print_dis_Pseudodis_Action( dis_Pseudodis_Action *o )

{

  dis_Pseudodis_Actiondis_Effect *e;
  int i, m;

  printf("\n\n----------------Pseudo dis_Action %s--------------\n", 
	 o->operator->name);

  for ( i = 0; i < o->operator->num_vars; i++ ) {
    printf("\nx%d = %s of type ", i, dis_gconstants[o->inst_table[i]]);
    dis_print_type( o->operator->var_types[i] );
  }

  printf("\nPreconds:\n");
  for ( i = 0; i < o->num_preconds; i++ ) {
    dis_print_dis_Fact( &(o->preconds[i]) );
    printf("\n");
  }
  for ( i = 0; i < o->num_numeric_preconds; i++ ) {
    switch ( o->numeric_preconds_comp[i] ) {
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
      printf("\nwrong comparator of Expnodes in mixedpre %d\n\n", 
	     o->numeric_preconds_comp[i]);
      exit( 1 );
    }
    dis_print_dis_ExpNode( o->numeric_preconds_lh[i] );
    dis_print_dis_ExpNode( o->numeric_preconds_rh[i] );
    printf(")\n");
  }

  m = 0;
  printf("\n\ndis_Effects:");
  for ( e = o->effects; e; e = e->next ) {
    printf("\n\neffect %d", m++);
    printf("\n\nConditions\n");
    for ( i = 0; i < e->num_conditions; i++ ) {
      dis_print_dis_Fact( &(e->conditions[i]) );
      printf("\n");
    }
    for ( i = 0; i < e->num_numeric_conditions; i++ ) {
      switch ( e->numeric_conditions_comp[i] ) {
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
	printf("\nwrong comparator of Expnodes in normeff %d\n\n", 
	       e->numeric_conditions_comp[i]);
	exit( 1 );
      }
      dis_print_dis_ExpNode( e->numeric_conditions_lh[i] );
      dis_print_dis_ExpNode( e->numeric_conditions_rh[i] );
      printf(")\n");
    }

    printf("\nAdds\n");
    for ( i = 0; i < e->num_adds; i++ ) {
      dis_print_dis_Fact( &(e->adds[i]) );
      printf("\n");
    }
    printf("\nDels\n");
    for ( i = 0; i < e->num_dels; i++ ) {
      dis_print_dis_Fact( &(e->dels[i]) );
      printf("\n");
    }
    for ( i = 0; i < e->num_numeric_effects; i++ ) {
      switch ( e->numeric_effects_neft[i] ) {
      case ASSIGN:
	printf("\nassign ");
	break;
      case SCALE_UP:
	printf("\nscale-up ");
	break;
      case SCALE_DOWN:
	printf("\nscale-down ");
	break;
      case INCREASE:
	printf("\nincrease ");
	break;
      case DECREASE:
	printf("\ndecrease ");
	break;
      default:
	printf("\n\nprint normop: illegal neft %d\n\n", 
	       e->numeric_effects_neft[i]);
	exit( 1 );
      }
      dis_print_dis_Fluent( &(e->numeric_effects_fluent[i]) );
      dis_print_dis_ExpNode( e->numeric_effects_rh[i] );
    }
  }

}



void dis_print_dis_Action( dis_Action *a )

{

  dis_Actiondis_Effect *e;
  int i, j;

  if ( !a->norm_operator &&
       !a->pseudo_action ) {
    printf("\n\ndis_Action REACH-GOAL");
  } else {
    printf("\n\ndis_Action %s", a->name ); 
    for ( i = 0; i < a->num_name_vars; i++ ) {
      printf(" %s", dis_gconstants[a->name_inst_table[i]]);
    }
  }

  printf("\n\nPreconds:\n");
  for ( i = 0; i < a->num_preconds; i++ ) {
    dis_print_ft_name( a->preconds[i] );
    printf("\n");
  }
  for ( i = 0; i < a->num_numeric_preconds; i++ ) {
    switch ( a->numeric_preconds_comp[i] ) {
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
      printf("\nwrong comparator of Expnodes in actionpre %d\n\n", 
	     a->numeric_preconds_comp[i]);
      exit( 1 );
    }
    dis_print_dis_ExpNode( a->numeric_preconds_lh[i] );
    dis_print_dis_ExpNode( a->numeric_preconds_rh[i] );
    printf(")\n");
  }

  printf("\n\ndis_Effects:");
  for ( j = 0; j < a->num_effects; j++ ) {
    printf("\n\neffect %d", j);
    e = &(a->effects[j]);
    if ( e->illegal ) printf(" ILLEGAL EFFECT!");
    printf("\n\nConditions\n");
    for ( i = 0; i < e->num_conditions; i++ ) {
      dis_print_ft_name( e->conditions[i] );
      printf("\n");
    }
    for ( i = 0; i < e->num_numeric_conditions; i++ ) {
      switch ( e->numeric_conditions_comp[i] ) {
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
	printf("\nwrong comparator of Expnodes in normeff %d\n\n", 
	       e->numeric_conditions_comp[i]);
	exit( 1 );
      }
      dis_print_dis_ExpNode( e->numeric_conditions_lh[i] );
      dis_print_dis_ExpNode( e->numeric_conditions_rh[i] );
      printf(")\n");
    }
    printf("\nAdds\n");
    for ( i = 0; i < e->num_adds; i++ ) {
      dis_print_ft_name( e->adds[i] );
      printf("\n");
    }
    printf("\nDels\n");
    for ( i = 0; i < e->num_dels; i++ ) {
      dis_print_ft_name( e->dels[i] );
      printf("\n");
    }
    for ( i = 0; i < e->num_numeric_effects; i++ ) {
      switch ( e->numeric_effects_neft[i] ) {
      case ASSIGN:
	printf("\nassign ");
	break;
      case SCALE_UP:
	printf("\nscale-up ");
	break;
      case SCALE_DOWN:
	printf("\nscale-down ");
	break;
      case INCREASE:
	printf("\nincrease ");
	break;
      case DECREASE:
	printf("\ndecrease ");
	break;
      default:
	printf("\n\nprint normop: illegal neft %d\n\n", 
	       e->numeric_effects_neft[i]);
	exit( 1 );
      }
      if ( e->numeric_effects_fl[i] >= 0 ) {
	dis_print_fl_name( e->numeric_effects_fl[i] );
      } else {
	printf("[UNDEF]");
      }
      dis_print_dis_ExpNode( e->numeric_effects_rh[i] );
    }
  }

}



void dis_print_dis_Action_name( dis_Action *a )

{

  int i;

  if ( !a->norm_operator &&
       !a->pseudo_action ) {
    printf("REACH-GOAL");
  } else {
    printf("%s", a->name ); 
    for ( i = 0; i < a->num_name_vars; i++ ) {
      printf(" %s", dis_gconstants[a->name_inst_table[i]]);
    }
  }

}



void dis_print_lnf_dis_Action( dis_Action *a )

{

  dis_Actiondis_Effect *e;
  int i, j;

  if ( !a->norm_operator &&
       !a->pseudo_action ) {
    printf("\n\ndis_Action REACH-GOAL");
  } else {
    printf("\n\ndis_Action %s", a->name ); 
    for ( i = 0; i < a->num_name_vars; i++ ) {
      printf(" %s", dis_gconstants[a->name_inst_table[i]]);
    }
  }

  printf("\n\nPreconds:\n");
  for ( i = 0; i < a->num_preconds; i++ ) {
    dis_print_ft_name( a->preconds[i] );
    printf("\n");
  }
  for ( i = 0; i < a->num_lnf_preconds; i++ ) {
    switch ( a->lnf_preconds_comp[i] ) {
    case GEQ:
      printf("(>= ");
      break;
    case GE:
      printf("(> ");
      break;
    default:
      printf("\nwrong comparator of Expnodes in lnf actionpre %d\n\n", 
	     a->lnf_preconds_comp[i]);
      exit( 1 );
    }
    dis_print_Lnfdis_ExpNode( a->lnf_preconds_lh[i] );
    printf(" %.2f)\n", a->lnf_preconds_rh[i]);
  }

  printf("\n\ndis_Effects:");
  for ( j = 0; j < a->num_effects; j++ ) {
    printf("\n\neffect %d COST %f", j, a->effects[j].cost);
    e = &(a->effects[j]);
    if ( e->illegal ) printf(" ILLEGAL EFFECT!");
    if ( e->removed ) printf(" REMOVED!!!");
    printf("\n\nConditions\n");
    for ( i = 0; i < e->num_conditions; i++ ) {
      dis_print_ft_name( e->conditions[i] );
      printf("\n");
    }
    for ( i = 0; i < e->num_lnf_conditions; i++ ) {
      switch ( e->lnf_conditions_comp[i] ) {
      case GEQ:
	printf("(>= ");
	break;
      case GE:
	printf("(> ");
	break;
      default:
	printf("\nwrong comparator of Expnodes in lnf normeff %d\n\n", 
	       e->lnf_conditions_comp[i]);
	exit( 1 );
      }
      dis_print_Lnfdis_ExpNode( e->lnf_conditions_lh[i] );
      printf(" %.2f)\n", e->lnf_conditions_rh[i] );
    }
    printf("\nAdds\n");
    for ( i = 0; i < e->num_adds; i++ ) {
      dis_print_ft_name( e->adds[i] );
      printf("\n");
    }
    printf("\nDels\n");
    for ( i = 0; i < e->num_dels; i++ ) {
      dis_print_ft_name( e->dels[i] );
      printf("\n");
    }
    for ( i = 0; i < e->num_lnf_effects; i++ ) {
      switch ( e->lnf_effects_neft[i] ) {
      case ASSIGN:
	printf("\nassign ");
	break;
      case INCREASE:
	printf("\nincrease ");
	break;
      default:
	printf("\n\nprint lnf normop: illegal neft %d\n\n", 
	       e->lnf_effects_neft[i]);
	exit( 1 );
      }
      if ( e->lnf_effects_fl[i] >= 0 ) {
	dis_print_fl_name( e->lnf_effects_fl[i] );
      } else {
	printf("[UNDEF]");
      }
      dis_print_Lnfdis_ExpNode( e->lnf_effects_rh[i] );
    }
  }

}



void dis_print_type( int t )

{

  int j;

  if ( dis_gpredicate_to_type[t] == -1 ) {
    if ( dis_gnum_intersected_types[t] == -1 ) {
      printf("%s", dis_gtype_names[t]);
    } else {
      printf("INTERSECTED TYPE (");
      for ( j = 0; j < dis_gnum_intersected_types[t]; j++ ) {
	if ( dis_gpredicate_to_type[dis_gintersected_types[t][j]] == -1 ) {
	  printf("%s", dis_gtype_names[dis_gintersected_types[t][j]]);
	} else {
	  printf("UNARY INERTIA TYPE (%s)", 
		 dis_gpredicates[dis_gpredicate_to_type[dis_gintersected_types[t][j]]]);
	}
	if ( j < dis_gnum_intersected_types[t] - 1 ) {
	  printf(" and ");
	}
      }
      printf(")");
    }
  } else {
    printf("UNARY INERTIA TYPE (%s)", dis_gpredicates[dis_gpredicate_to_type[t]]);
  }

}

void dis_print_dis_Fact_string( dis_Fact *f, char *str)
{
  int j;
  char temp[dis_MAX_LENGTH];

  if ( f->predicate == -3 ) {
    sprintf(str,"GOAL-REACHED");
    return;
  } 

  if ( f->predicate == -1 ) {
    sprintf(str,"(=");
    for ( j=0; j<2; j++ ) {
      strcat(str," ");
      if ( f->args[j] >= 0 ) {
    strcat(str, dis_gconstants[(f->args)[j]]);
      } else {
	sprintf(temp,"x%d", dis_DECODE_VAR( f->args[j] ));
      strcat(str, temp);
      }
    }
    sprintf(temp,")");
    strcat(str, temp);
    return;
  }

  if ( f->predicate == -2 ) {
    sprintf(str,"(!=");
    for ( j=0; j<2; j++ ) {
      sprintf(temp," ");
      strcat(str, temp);
      if ( f->args[j] >= 0 ) {
	sprintf(temp,"%s", dis_gconstants[(f->args)[j]]);
    strcat(str, temp);
      } else {
	sprintf(temp, "x%d", dis_DECODE_VAR( f->args[j] ));
    strcat(str, temp);
    }
    sprintf(temp,")");
    strcat(str, temp);
    return;
    }
  }
    
  sprintf(str,"(%s", dis_gpredicates[f->predicate]);
  for ( j=0; j<dis_garity[f->predicate]; j++ ) {
    sprintf(temp," ");
    strcat(str, temp);
    if ( f->args[j] >= 0 ) {
      sprintf(temp,"%s", dis_gconstants[(f->args)[j]]);
    strcat(str, temp);
    } else {
      sprintf(temp,"x%d", dis_DECODE_VAR( f->args[j] ));
    strcat(str, temp);
    }
  }
  sprintf(temp,")");
    strcat(str, temp);
}


void dis_print_dis_Fact( dis_Fact *f )

{

  int j;

  if ( f->predicate == -3 ) {
    printf("GOAL-REACHED");
    return;
  }

  if ( f->predicate == -1 ) {
    printf("(=");
    for ( j=0; j<2; j++ ) {
      printf(" ");
      if ( f->args[j] >= 0 ) {
	printf("%s", dis_gconstants[(f->args)[j]]);
      } else {
	printf("x%d", dis_DECODE_VAR( f->args[j] ));
      }
    }
    printf(")");
    return;
  }

  if ( f->predicate == -2 ) {
    printf("(!=");
    for ( j=0; j<2; j++ ) {
      printf(" ");
      if ( f->args[j] >= 0 ) {
	printf("%s", dis_gconstants[(f->args)[j]]);
      } else {
	printf("x%d", dis_DECODE_VAR( f->args[j] ));
      }
    }
    printf(")");
    return;
  }
    
  printf("(%s", dis_gpredicates[f->predicate]);
  for ( j=0; j<dis_garity[f->predicate]; j++ ) {
    printf(" ");
    if ( f->args[j] >= 0 ) {
      printf("%s", dis_gconstants[(f->args)[j]]);
    } else {
      printf("x%d", dis_DECODE_VAR( f->args[j] ));
    }
  }
  printf(")");

}



void dis_print_dis_Fluent( dis_Fluent *f )

{

  int j, ff = f->function;

  printf("(%s", dis_gfunctions[ff]);
  for ( j=0; j<dis_gf_arity[ff]; j++ ) {
    printf(" ");
    if ( f->args[j] >= 0 ) {
      printf("%s", dis_gconstants[(f->args)[j]]);
    } else {
      printf("x%d", dis_DECODE_VAR( f->args[j] ));
    }
  }
  printf(")");

}


void dis_print_ft_name_string( int index, char *string )

{

  dis_print_dis_Fact_string( &(dis_grelevant_facts[index]), string );
                    
}

void dis_print_ft_name( int index )

{

  dis_print_dis_Fact( &(dis_grelevant_facts[index]) );

}



void dis_print_fl_name( int index )

{

  int i;

  if ( index < 0 ) {
    if ( index != -2 ) {
      printf("[UNDEF]");
    } else {
      printf("[TOTAL-TIME]");
    }
    return;
  }

  if ( dis_grelevant_fluents_lnf[index] == NULL ) {
    /* this is a non-artificial "atomic" one
     * (or the mirrored version of one)
     */
    printf("[RF%d](%s)", index, dis_grelevant_fluents_name[index]);
  } else {
    /* this only summarizes a LNF requirement
     */
    printf("[artRF%d]", index);
    for ( i = 0; i < dis_grelevant_fluents_lnf[index]->num_pF; i++ ) {
      printf("%.2f*", dis_grelevant_fluents_lnf[index]->pC[i] );
      dis_print_fl_name( dis_grelevant_fluents_lnf[index]->pF[i] );
      if ( i < dis_grelevant_fluents_lnf[index]->num_pF - 1 ) {
	printf(" + ");
      }
    }
  }

}



void dis_print_Lnfdis_ExpNode( Lnfdis_ExpNode *n )

{

  int i;

  printf("((");
  for ( i = 0; i < n->num_pF; i++ ) {
    printf("%.2f*", n->pC[i]);
    dis_print_fl_name( n->pF[i] );
  }
  printf(") - (");
  for ( i = 0; i < n->num_nF; i++ ) {
    printf("%.2f*", n->nC[i]);
    dis_print_fl_name( n->nF[i] );
  }
  printf(") + %.2f)", n->c);

}



void dis_print_op_name( int index )

{

  int i;
  dis_Action *a = dis_gop_conn[index].action;

  if ( !a->norm_operator &&
       !a->pseudo_action ) {
    printf("REACH-GOAL");
  } else {
    printf("%s", a->name ); 
    for ( i = 0; i < a->num_name_vars; i++ ) {
      printf(" %s", dis_gconstants[a->name_inst_table[i]]);
    }
  }

}



void dis_print_dis_State( dis_State S )

{

  int i;
  
  for ( i = 0; i < S.num_F; i++ ) {
    printf("%d ", S.F[i] );
    dis_print_ft_name( S.F[i] );
    printf("\n" );
  } 
  for ( i = 0; i < dis_gnum_relevant_fluents; i++ ) {
    printf("\n");
    dis_print_fl_name( i );
    printf(": ");
    if ( S.f_D[i] ) {
      printf("%.2f", S.f_V[i]);
    } else {
      printf("UNDEF");
    }
  }

}








/*
 * program output routines
 */

void dis_print_plan( void )

{  

  int i;

  printf("\n\nff: found legal plan as follows");
  printf("\n\nstep ");
  for ( i = 0; i < dis_gnum_plan_ops; i++ ) {
    printf("%4d: ", i); 
    printf("%d\n", dis_gplan_ops[i]);
    //dis_print_op_name( dis_gplan_ops[i] );
    //printf("\n     ");
  }

}
