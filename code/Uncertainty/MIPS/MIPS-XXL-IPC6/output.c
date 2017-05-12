

/*********************************************************************
 * (C) Copyright 2002 Albert Ludwigs University Freiburg
 *     Institute of Computer Science
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 * 
 *********************************************************************/



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





#include "ff.h"

#include "output.h"
#include "parse.h"






/* parsing
 */


/* for parallelSolution and Solution Files */

extern FILE *pfp;

FILE *fp;
extern FILE *fp2;

float starttime;




void print_FactList( FactList *list, char *sepf, char *sept )

{

  FactList *i_list;
  TokenList *i_tl;
    
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



void print_hidden_TokenList( TokenList *list, char *sep )

{

  TokenList *i_tl;

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



void print_indent( int indent )

{

  int i;
  for (i=0;i<indent;i++) {
    printf(" ");
  }

}



void print_ParseExpNode( ParseExpNode *n )

{

  if ( !n ) return;

  switch ( n->connective) {
  case AD:
    printf("(+ ");
    print_ParseExpNode( n->leftson );
    print_ParseExpNode( n->rightson );
    printf(")");
    break;
  case SU:
    printf("(- ");
    print_ParseExpNode( n->leftson );
    print_ParseExpNode( n->rightson );
    printf(")");
    break;
  case MU:
    printf("(* ");
    print_ParseExpNode( n->leftson );
    print_ParseExpNode( n->rightson );
    printf(")");
    break;
  case DI:
    printf("(/ ");
    print_ParseExpNode( n->leftson );
    print_ParseExpNode( n->rightson );
    printf(")");
    break;
  case MINUS:
    printf("(- ");
    print_ParseExpNode( n->leftson );
    printf(")");
    break;
  case NUMBER:
    printf("%s", n->atom->item);
    break;
  case FHEAD:
    printf("(");
    print_hidden_TokenList(n->atom, " ");
    printf(")");
    break;
  default:
    printf("\n\nprint Parseexpnode: wrong specifier %d",
	   n->connective);
  }

}



void print_PlNode( PlNode *plnode, int indent )

{

  PlNode *i_son;

  if ( !plnode ) {
    printf("none\n");
    return;
  }

  if(isPreference || isConstraintsPreference)
    if(plnode->name)
      printf("\nPreference  %s: ", plnode->name);  

  if(isDurative)
    printf("atom_t= %i ",  plnode->atom_t);
  switch (plnode->connective) {
  case ttw_AT :
    printf("(AT    ");
    print_PlNode(plnode->sons,indent+4);
    break;	
  case ALL: 
    printf("ALL (");
    if(plnode->atom)
      print_hidden_TokenList(plnode->atom, " ");
    else
      print_TypedList(plnode->parse_vars);
    print_indent(indent);
    printf("(   ");
    print_PlNode(plnode->sons,indent+4);
    print_indent(indent);
    printf(")\n");
    break;
  case EX:
    printf("EX ");
    if(plnode->atom)
      print_hidden_TokenList(plnode->atom, " ");
    else
      print_TypedList(plnode->parse_vars);

    print_indent(indent);
    printf("(   ");
    print_PlNode(plnode->sons,indent+4);
    print_indent(indent);
    printf(")\n");
    break;
  case AND: 
    printf("A(  ");
    print_PlNode(plnode->sons, indent+4);
    if ( plnode->sons ) {
      for ( i_son = plnode->sons->next; i_son!=NULL; i_son = i_son->next ) {
	print_indent(indent);
	printf("AND ");
	print_PlNode(i_son,indent+4);
      }
    }
    print_indent(indent);      
    printf(")\n");
    break;
  case OR:  
    printf("O(  ");
    print_PlNode(plnode->sons, indent+4);
    for ( i_son = plnode->sons->next; i_son!=NULL; i_son = i_son->next ) {
      print_indent(indent);
      printf("OR ");
      print_PlNode(i_son,indent+4);
    }
    print_indent(indent);      
    printf(")\n");
    break;
  case WHEN:
    printf("IF   ");
    print_PlNode(plnode->sons,indent+5);
    print_indent(indent);
    printf("THEN ");
    print_PlNode(plnode->sons->next,indent+5);
    print_indent(indent);
    printf("ENDIF\n");
    break;
  case pref:
    printf(" Prefnode ");
    print_PlNode(plnode->sons, indent+4);
    break;
  case NOT:
    if (ATOM==plnode->sons->connective) {
      printf("NOT  ");
      print_PlNode(plnode->sons,indent+4);
    } else {
      printf("NOT (");
      print_PlNode(plnode->sons,indent+4);
      print_indent(indent+3);
      printf(")\n");
    }
    break;
  case ATOM:
    printf("(");
    print_hidden_TokenList(plnode->atom, " ");
    printf(")\n");
    break;
  case TRU:
    printf("(TRUE)\n");
    break;
  case FAL:
    printf("(FALSE)\n");
    break;   
  case COMP:
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
    print_ParseExpNode( plnode->lh );
    print_ParseExpNode( plnode->rh );
    printf(")\n");
    break;
  case NEF:
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
    print_ParseExpNode( plnode->lh );
    print_ParseExpNode( plnode->rh );
    printf(")\n");
    break;
  case TEMPORALOP:
    switch (plnode->temporal_op) {
    case ALWAYS:
      printf("(always ");
      print_PlNode( plnode->sons, indent+4);
      break;
    case SOMETIME:
      printf("(sometime ");
      print_PlNode( plnode->sons, indent+4);
      break;
    case AEND:
      printf("(at end ");
      print_PlNode( plnode->sons, indent+4);
      break;
    case WITHIN:
      printf("(within ");
      print_ParseExpNode( plnode->lh);
      printf(" ");
      print_PlNode( plnode->sons, indent+4);
      printf(" ");
      break;
    case AT_MOST_ONCE:
      printf("(at-most-once ");
      print_PlNode( plnode->sons, indent+4);
      break;
    case SOMETIME_AFTER:
      printf("(sometime-after ");
      print_PlNode( plnode->sons, indent+4);
      printf(" ");
      print_PlNode( plnode->sons->next, indent+4);
      break;
    case SOMETIME_BEFORE:
      printf("(sometime-before ");
      print_PlNode( plnode->sons, indent+4);
      printf(" ");
      print_PlNode( plnode->sons->next, indent+4);
      break;
    case ALWAYS_WITHIN:
      printf("(always-within " );
      print_ParseExpNode( plnode->lh);
      printf(" ");
      print_PlNode( plnode->sons, indent+4);
      printf(" ");
      print_PlNode( plnode->sons->next, indent+4);
      break;
    case HOLD_DURING:
      printf("(hold-during ");
      print_ParseExpNode( plnode->lh);
      printf(" ");
      print_ParseExpNode( plnode->rh);
      printf(" ");
      print_PlNode( plnode->sons, indent+4);
      break;
    case HOLD_AFTER:
      printf("(hold-after ");
      print_ParseExpNode( plnode->lh);
      printf(" ");
      print_PlNode( plnode->sons, indent+4);
      break;
    default:
      printf("\n\nillegal temporal operator: %d  in parse tree!\n\n", plnode->temporal_op);
      exit( 1 );
    }
    printf(")\n");
    break;
  default:
    printf("\n***** ERROR ****");
    printf("\nprint_plnode: %d > Wrong Node specifier\n", plnode->connective);
    exit(1);
  }     

} 






void print_plops( PlOperator *plop )

{

  PlOperator *i_plop;
  int count = 0;

  if ( !plop ) {
    printf("none\n");
  }

  for ( i_plop = plop; i_plop!=NULL; i_plop = i_plop->next ) {
    printf("\nOPERATOR ");
    printf("%s", i_plop->name);
    printf("\nparameters: (%d real)\n", i_plop->number_of_real_params);
    print_FactList ( i_plop->params, "\n", " : ");
    printf("\n\npreconditions:\n");
    print_PlNode(i_plop->preconds, 0);
    printf("effects:\n");
    print_PlNode(i_plop->effects, 0);

    if(isDurative)
      {
	printf("ttw_preconds:\n");
	print_PlNode(i_plop->ttw_preconds, 0);
      }
    printf("\n-----\n");
    count++;
  }
  printf("\nAnzahl der Operatoren: %d\n", count);

}



void print_ExpNode( ExpNode *n )

{

  if ( !n ) return;

  switch ( n->connective) {
  case AD:
    printf("(+ ");
    print_ExpNode( n->leftson );
    print_ExpNode( n->rightson );
    printf(")");
    break;
  case SU:
    printf("(- ");
    print_ExpNode( n->leftson );
    print_ExpNode( n->rightson );
    printf(")");
    break;
  case MU:
    printf("(* ");
    print_ExpNode( n->leftson );
    print_ExpNode( n->rightson );
    printf(")");
    break;
  case DI:
    printf("(/ ");
    print_ExpNode( n->leftson );
    print_ExpNode( n->rightson );
    printf(")");
    break;
  case MINUS:
    printf("(- ");
    print_ExpNode( n->son );
    printf(")");
    break;
  case NUMBER:
    printf("%.2f", n->value);
    break;
  case FHEAD:
    if ( n->fluent ) {
      print_Fluent( n->fluent );
    } else {
      if ( n->fl >= 0 ) {
	printf(" %.2f*", n->c);
	print_fl_name( n->fl );
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



void print_Wff( WffNode *n, int indent )

{

  WffNode *i;

  if ( !n ) {
    printf("none\n");
    return;
  }

  /* if(isDurative)
     printf(" fact_t: %i\n",n->fact_t);  */

  
  if(isPreference || isConstraintsPreference)
    if(n->name)
      printf("\nPreference  %s: ", n->name);  

  switch (n->connective) {
  case ALL: 
    printf("ALL x%d (%s): %s\n", n->var, n->var_name,
	   gtype_names[n->var_type]);
    print_indent(indent);
    printf("(   ");
    print_Wff(n->son,indent+4);
    print_indent(indent);
    printf(")\n");
    break;
  case EX:
    printf("EX  x%d (%s) : %s\n",  n->var, n->var_name,
	   gtype_names[n->var_type]);
    print_indent(indent);
    printf("(   ");
    print_Wff(n->son,indent+4);
    print_indent(indent);
    printf(")\n");
    break;
  case AND: 
    printf("A(  ");
    print_Wff(n->sons, indent+4);
    if ( n->sons ) {
      for ( i = n->sons->next; i!=NULL; i = i->next ) {
	if ( !i->prev ) {
	  printf("\nprev in AND not correctly set!\n\n");
	  exit( 1 );
	}
	print_indent(indent);
	printf("AND ");
	print_Wff(i,indent+4);
      }
    }
    print_indent(indent);      
    printf(")\n");
    break;
  case OR:  
    printf("O(  ");
    print_Wff(n->sons, indent+4);
    for ( i = n->sons->next; i!=NULL; i = i->next ) {
      print_indent(indent);
      printf("OR ");
      print_Wff(i,indent+4);
    }
    print_indent(indent);      
    printf(")\n");
    break;
  case NOT:
    if (ATOM==n->son->connective) {
      printf("NOT  ");
      print_Wff(n->son,indent+4);
    } else {
      printf("NOT (");
      print_Wff(n->son,indent+4);
      print_indent(indent+3);
      printf(")\n");
    }
    break;
  case ATOM:
    print_Fact(n->fact);
    if ( n->NOT_p != -1 ) printf(" - translation NOT");
    printf("\n");
    break;
  case TRU:
    printf("(TRUE)\n");
    break;
  case FAL:
    printf("(FALSE)\n");
    break;   
  case COMP:
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
    print_ExpNode( n->lh );
    print_ExpNode( n->rh );
    printf(")\n");
    break;
  case TEMPORALOP:
    switch (n->temporal_op) {
    case ALWAYS:
      printf("(always ");
      print_Wff( n->son, indent+4);
      break;
    case SOMETIME:
      printf("(sometime ");
      print_Wff( n->son, indent+4);
      break;
    case AEND:
      printf("(at end ");
      print_Wff( n->son, indent+4);
      break;
    case WITHIN:
      printf("(within ");
      print_ExpNode( n->lh );
      printf(" ");
      print_Wff( n->son, indent+4);
      break;
    case AT_MOST_ONCE:
      printf("(at-most-once ");
      print_Wff( n->son, indent+4);
      break;
    case SOMETIME_AFTER:
      printf("(sometime-after ");
      print_Wff( n->son, indent+4);
      print_Wff( n->son->next, indent+4);
      break;
    case SOMETIME_BEFORE:
      printf("(sometime-before ");
      print_Wff( n->son, indent+4);
      print_Wff( n->son->next, indent+4);
      break;
    case ALWAYS_WITHIN:
      printf("(always-within ");
      print_ExpNode( n->lh );
      printf(" ");
      print_Wff( n->son, indent+4);
      print_Wff( n->son->next, indent+4);
      break;
    case HOLD_DURING:
      printf("(hold-during ");
      print_ExpNode( n->lh );
      printf(" ");
      print_ExpNode( n->rh );
      printf(" ");
      print_Wff( n->son, indent+4);
      break;
    case HOLD_AFTER:
      printf("(hold-after ");
      print_ExpNode( n->lh );
      printf(" ");
      print_Wff( n->son, indent+4);
      break;
    default:
      printf("\n\nillegal temporal operator in parse tree %d!\n\n", n->temporal_op);
      exit( 1 );
    }
    printf(")\n");
    break;
  default:
    printf("\n***** ERROR ****");
    printf("\nprint_Wff: %d > Wrong Node specifier\n", n->connective);
    exit(1);
  }     

} 



void print_Operator( Operator *o )

{

  Effect *e;
  Literal *l;
  NumericEffect *ne;
  int i, m = 0;

  printf("\n\n----Operator %s, translated form, step 1-------\n", o->name);

  for ( i = 0; i < o->num_vars; i++ ) {
    printf("\nx%d (%s) of type %s, removed ? %s",
	   i, o->var_names[i], gtype_names[o->var_types[i]],
	   o->removed[i] ? "YES" : "NO");
  }
  printf("\ntotal params %d, real params %d\n", 
	 o->num_vars, o->number_of_real_params);

  printf("\nPreconds:\n");
  print_Wff( o->preconds, 0 );

  if(o->pref_preconds){
    printf("\npref Preconds:\n");
    print_Wff( o->pref_preconds, 0 );
  }

  printf("\n\nEffects:");
  for ( e = o->effects; e; e = e->next ) {
    printf("\n\neffect %d, parameters %d", m++, e->num_vars);

    for ( i = 0; i < e->num_vars; i++ ) {
      printf("\nx%d (%s) of type %s",
	     o->num_vars + i, e->var_names[i], gtype_names[e->var_types[i]]);
    }
    printf("\nConditions\n");
    print_Wff( e->conditions, 0 );
    printf("\nEffect Literals");
    for ( l = e->effects; l; l = l->next ) {
      if ( l->negated ) {
	printf("\nNOT ");
      } else {
	printf("\n");
      }
      print_Fact( &(l->fact) );

      if(isDurative)
	printf(" fact_t: %i\n",l->fact_t);     

    }
    printf("\nNumeric Effects");
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
      print_Fluent( &(ne->fluent) );

      if(isDurative)
	printf(" fluent_t: %i\n",ne->fluent_t);

      print_ExpNode( ne->rh );
    }
  }

}



void print_NormOperator( NormOperator *o )

{

  NormEffect *e;
  int i, m;

  printf("\n\n----------------Operator %s, normalized form--------------\n", 
	 o->operator->name);

  for ( i = 0; i < o->num_vars; i++ ) {
    printf("\nx%d of type ", i);
    print_type( o->var_types[i] );
  }
  printf("\n\n%d vars removed from original operator:",
	 o->num_removed_vars);
  for ( i = 0; i < o->num_removed_vars; i++ ) {
    m = o->removed_vars[i];
    printf("\nx%d (%s) of type %s, type constraint ", m, o->operator->var_names[m], 
	   gtype_names[o->operator->var_types[m]]);
    print_type( o->type_removed_vars[i] );
  }

  printf("\nPreconds:\n");
  for ( i = 0; i < o->num_preconds; i++ ) {
    print_Fact( &(o->preconds[i]) );

    if(isDurative)
      printf(" predicate_t: %i\n",o->preconds[i].predicate_t);
    printf("\n");
  }

  printf("\npref Preconds:\n");
  for ( i = 0; i < o->num_pref_preconds; i++ ) {
    print_Fact( &(o->pref_preconds[i]) );

    if(isDurative)
      printf(" predicate_t: %i\n",o->pref_preconds[i].predicate_t);
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
    print_ExpNode( o->numeric_preconds_lh[i] );

    if(isDurative)
      printf(" lh_t: %i\n",o->numeric_preconds_lh[i]->lh_t);


    print_ExpNode( o->numeric_preconds_rh[i] );
    printf(")\n");
  }

  m = 0;
  printf("\n\nEffects:");
  for ( e = o->effects; e; e = e->next ) {
    printf("\n\neffect %d, parameters %d", m++, e->num_vars);

    for ( i = 0; i < e->num_vars; i++ ) {
      printf("\nx%d of type ", o->num_vars + i);
      print_type( e->var_types[i] );
    }
    printf("\nConditions\n");
    for ( i = 0; i < e->num_conditions; i++ ) {
      print_Fact( &(e->conditions[i]) );
      printf("\n");

      if(isDurative)
	printf(" predicate_t: %i\n",e->conditions[i].predicate_t);
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
      print_ExpNode( e->numeric_conditions_lh[i] );

      if(isDurative)
        printf(" lh_t: %i\n",e->numeric_conditions_lh[i]->lh_t);

      print_ExpNode( e->numeric_conditions_rh[i] );
      printf(")\n");
    }

    printf("\nAdds\n");
    for ( i = 0; i < e->num_adds; i++ ) {
      print_Fact( &(e->adds[i]) );
      if(isDurative)
	printf(" prednorm_t: %i\n",e->adds[i].prednorm_t);     

      printf("\n");
    }
    printf("\nDels\n");
    for ( i = 0; i < e->num_dels; i++ ) {
      print_Fact( &(e->dels[i]) );
      if(isDurative)
	printf(" prednorm_t: %i\n",e->dels[i].prednorm_t);

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
      print_Fluent( &(e->numeric_effects_fluent[i]) );
      if(isDurative)
	printf(" function_t: %i\n",e->numeric_effects_fluent[i].function_t);

      print_ExpNode( e->numeric_effects_rh[i] );
    }
  }

}



void print_MixedOperator( MixedOperator *o )

{

  int i, m;
  Effect *e;
  NumericEffect *ne;
  Literal *l;

  printf("\n\n----------------Operator %s, mixed form--------------\n", 
	 o->operator->name);
 
  for ( i = 0; i < o->operator->num_vars; i++ ) {
    printf("\nx%d = %s of type ", i, gconstants[o->inst_table[i]]);
    print_type( o->operator->var_types[i] );
  }

  printf("\nPreconds:\n");
  for ( i = 0; i < o->num_preconds; i++ ) {
    print_Fact( &(o->preconds[i]) );
    printf("\n");

    if(isDurative)
      printf(" predicate_t: %i\n",o->preconds[i].predicate_t);
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
    print_ExpNode( o->numeric_preconds_lh[i] );

    if(isDurative)
      printf(" lh_t: %i\n",o->numeric_preconds_lh[i]->lh_t);


    print_ExpNode( o->numeric_preconds_rh[i] );
    printf(")\n");
  }

  m = 0;
  printf("\n\nEffects:");
  for ( e = o->effects; e; e = e->next ) {
    printf("\n\neffect %d, parameters %d", m++, e->num_vars);

    for ( i = 0; i < e->num_vars; i++ ) {
      printf("\nx%d of type %s",
	     o->operator->num_vars + i, gtype_names[e->var_types[i]]);
    }
    printf("\nConditions\n");
    print_Wff( e->conditions, 0 );
    printf("\nEffect Literals");
    for ( l = e->effects; l; l = l->next ) {
      if ( l->negated ) {
	printf("\nNOT ");
      } else {
	printf("\n");
      }
      print_Fact( &(l->fact) );
      if(isDurative)
	printf(" fact_t: %i\n",l->fact.prednorm_t);   
    }
    printf("\nNumeric Effects");
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
      print_Fluent( &(ne->fluent) );
      if(isDurative)
	printf(" fluent_t: %i\n",ne->fluent.function_t);

      print_ExpNode( ne->rh );
    }
  }

}



void print_PseudoAction( PseudoAction *o )

{

  PseudoActionEffect *e;
  int i, m;

  printf("\n\n----------------Pseudo Action %s--------------\n", 
	 o->operator->name);

  for ( i = 0; i < o->operator->num_vars; i++ ) {
    printf("\nx%d = %s of type ", i, gconstants[o->inst_table[i]]);
    print_type( o->operator->var_types[i] );
  }

  printf("\nPreconds:\n");
  for ( i = 0; i < o->num_preconds; i++ ) {
    print_Fact( &(o->preconds[i]) );
    printf("\n");

    if(isDurative)
      printf(" predicate_t: %i\n",o->preconds[i].predicate_t);
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
    print_ExpNode( o->numeric_preconds_lh[i] );

   
    if(isDurative)
      printf(" lh_t: %i\n",o->numeric_preconds_lh[i]->lh_t);    

    print_ExpNode( o->numeric_preconds_rh[i] );
    printf(")\n");
  }

  m = 0;
  printf("\n\nEffects:");
  for ( e = o->effects; e; e = e->next ) {
    printf("\n\neffect %d", m++);
    printf("\n\nConditions\n");
    for ( i = 0; i < e->num_conditions; i++ ) {
      print_Fact( &(e->conditions[i]) );
      printf("\n");

      if(isDurative)
	printf(" predicate_t: %i\n",e->conditions[i].predicate_t);
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
      print_ExpNode( e->numeric_conditions_lh[i] );

      if(isDurative)
        printf(" lh_t: %i\n",e->numeric_conditions_lh[i]->lh_t);

      print_ExpNode( e->numeric_conditions_rh[i] );
      printf(")\n");
    }

    printf("\nAdds\n");
    for ( i = 0; i < e->num_adds; i++ ) {
      print_Fact( &(e->adds[i]) );

      if(isDurative)
	printf(" prednorm_t: %i\n",e->adds[i].prednorm_t);     


      printf("\n");
    }
    printf("\nDels\n");
    for ( i = 0; i < e->num_dels; i++ ) {
      print_Fact( &(e->dels[i]) );

      if(isDurative)
	printf(" prednorm_t: %i\n",e->dels[i].prednorm_t);     

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
      print_Fluent( &(e->numeric_effects_fluent[i]) );

      if(isDurative)
	printf(" function_t: %i\n",e->numeric_effects_fluent[i].function_t);

      print_ExpNode( e->numeric_effects_rh[i] );
    }
  }

}



void print_Action( Action *a )

{

  ActionEffect *e;
  int i, j;

  if ( !a->norm_operator &&
       !a->pseudo_action ) {
    printf("\n\nAction REACH-GOAL");
  } else {
    printf("\n\nAction %s", a->name ); 
    for ( i = 0; i < a->num_name_vars; i++ ) {
      printf(" %s", gconstants[a->name_inst_table[i]]);
    }
  }

  printf("\n\nDuration:\n");
  print_ExpNode( a->action_duration);  

  printf("\n\nPreconds:\n");
  for ( i = 0; i < a->num_preconds; i++ ) {
    print_ft_name( a->preconds[i] );
    if(isDurative)
      printf("\na.preconds_t: %d", a->preconds_t[i] );
    printf("\n");
  }

  if(a->num_pref_preconds > 0){
    printf("\n\npref_Preconds:\n");
    for ( i = 0; i < a->num_pref_preconds; i++ ) {
      print_ft_name( a->pref_preconds[i] );
      printf("\n");
    }
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
    print_ExpNode( a->numeric_preconds_lh[i] );
    print_ExpNode( a->numeric_preconds_rh[i] );

    if(isDurative)
      printf("\na.numeric.preconds_t: %d\n", a->numeric_preconds_lh_t[i] );

    printf(")\n");
   
  }

  printf("\n\nEffects:");
  for ( j = 0; j < a->num_effects; j++ ) {
    printf("\n\neffect %d", j);
    e = &(a->effects[j]);
    if ( e->illegal ) printf(" ILLEGAL EFFECT!");
    printf("\n\nConditions\n");
    for ( i = 0; i < e->num_conditions; i++ ) {
      print_ft_name( e->conditions[i] );

      if(isDurative)
	printf("\ne.conditions_t: %d", e->conditions_t[i]);

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
      print_ExpNode( e->numeric_conditions_lh[i] );
      print_ExpNode( e->numeric_conditions_rh[i] );

      if(isDurative)
	printf("\ne.numeric.conditions_lh_t: %d\n", e->numeric_conditions_lh_t[i] );
      printf(")\n");
    }
    printf("\nAdds\n");
    for ( i = 0; i < e->num_adds; i++ ) {
      print_ft_name( e->adds[i] );
      if(isDurative)
	printf("\ne.adds_t: %d", e->adds_t[i]);
      printf("\n");
    }
    printf("\nDels\n");
    for ( i = 0; i < e->num_dels; i++ ) {
      print_ft_name( e->dels[i] );
      if(isDurative)
	printf("\ne.dels_t: %d", e->dels_t[i]);
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
	print_fl_name( e->numeric_effects_fl[i] );
      } else {
	printf("[UNDEF]");
      }
      print_ExpNode( e->numeric_effects_rh[i] );
      if(isDurative)
	printf("\ne.numeric.effects_fl_t: %d", e->numeric_effects_fl_t[i] );
    }
  }

}



void print_Action_name( Action *a )

{

  int i;

  if ( !a->norm_operator &&
       !a->pseudo_action ) {
    printf("REACH-GOAL");
  } else {
    printf("%s", a->name ); 
    for ( i = 0; i < a->num_name_vars; i++ ) {
      printf(" %s", gconstants[a->name_inst_table[i]]);
    }
  }

}



void print_lnf_Action( Action *a )

{

  ActionEffect *e;
  int i, j;

  if ( !a->norm_operator &&
       !a->pseudo_action ) {
    printf("\n\nAction REACH-GOAL");
  } else {
    printf("\n\nAction %s", a->name ); 
    for ( i = 0; i < a->num_name_vars; i++ ) {
      printf(" %s", gconstants[a->name_inst_table[i]]);
    }
  }

  printf("\n\nPreconds:\n");
  for ( i = 0; i < a->num_preconds; i++ ) {
    print_ft_name( a->preconds[i] );

    if(isDurative)
      printf("\na.preconds_t: %d", a->preconds_t[i] );

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
    print_LnfExpNode( a->lnf_preconds_lh[i] );
   
    printf(" %.2f)\n", a->lnf_preconds_rh[i]);
    if(isDurative)
      printf("\na->lnf->preconds_lh_t: %d", a->lnf_preconds_lh_t[i] );
  }

  printf("\n\nEffects:");
  for ( j = 0; j < a->num_effects; j++ ) {
    printf("\n\neffect %d COST %f", j, a->effects[j].cost);
    e = &(a->effects[j]);
    if ( e->illegal ) printf(" ILLEGAL EFFECT!");
    if ( e->removed ) printf(" REMOVED!!!");
    printf("\n\nConditions\n");
    for ( i = 0; i < e->num_conditions; i++ ) {
      print_ft_name( e->conditions[i] );

      if(isDurative)
	printf("\ne.conditions_t: %d", e->conditions_t[i]);

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
      print_LnfExpNode( e->lnf_conditions_lh[i] );
      printf(" %.2f)\n", e->lnf_conditions_rh[i] );

      if(isDurative)
	printf("\ne->lnf_conditions_lh_t: %d", e->lnf_conditions_lh_t[i] );

    }
    printf("\nAdds\n");
    for ( i = 0; i < e->num_adds; i++ ) {
      print_ft_name( e->adds[i] );

      if(isDurative)
	printf("\ne.adds_t: %d", e->adds_t[i]);

      printf("\n");
    }
    printf("\nDels\n");
    for ( i = 0; i < e->num_dels; i++ ) {
      print_ft_name( e->dels[i] );

      if(isDurative)
	printf("\ne.dels_t: %d", e->dels_t[i]);

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
	print_fl_name( e->lnf_effects_fl[i] );

	if(isDurative)
	  printf("\ne->lnf->effects_fl_t: %d", e->lnf_effects_fl_t[i] );
     
      } else {
	printf("[UNDEF]");
      }
      print_LnfExpNode( e->lnf_effects_rh[i] );
    }
  }

}



void print_type( int t )

{

  int j;

  if ( gpredicate_to_type[t] == -1 ) {
    if ( gnum_intersected_types[t] == -1 ) {
      printf("%s", gtype_names[t]);
    } else {
      printf("INTERSECTED TYPE (");
      for ( j = 0; j < gnum_intersected_types[t]; j++ ) {
	if ( gpredicate_to_type[gintersected_types[t][j]] == -1 ) {
	  printf("%s", gtype_names[gintersected_types[t][j]]);
	} else {
	  printf("UNARY INERTIA TYPE (%s)", 
		 gpredicates[gpredicate_to_type[gintersected_types[t][j]]]);
	}
	if ( j < gnum_intersected_types[t] - 1 ) {
	  printf(" and ");
	}
      }
      printf(")");
    }
  } else {
    printf("UNARY INERTIA TYPE (%s)", gpredicates[gpredicate_to_type[t]]);
  }

}



void print_Fact( Fact *f )

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
	printf("%s", gconstants[(f->args)[j]]);
      } else {
	printf("x%d", DECODE_VAR( f->args[j] ));
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
	printf("%s", gconstants[(f->args)[j]]);
      } else {
	printf("x%d", DECODE_VAR( f->args[j] ));
      }
    }
    printf(")");
    return;
  }
    
  printf("(%s", gpredicates[f->predicate]);
  for ( j=0; j<garity[f->predicate]; j++ ) {
    printf(" ");
    if ( f->args[j] >= 0 ) {
      printf("%s", gconstants[(f->args)[j]]);
    } else {
      printf("x%d", DECODE_VAR( f->args[j] ));
    }
  }
  printf(")");

}



void print_Fluent( Fluent *f )

{

  int j, ff = f->function;

  printf("(%s", gfunctions[ff]);
  for ( j=0; j<gf_arity[ff]; j++ ) {
    printf(" ");
    if ( f->args[j] >= 0 ) {
      printf("%s", gconstants[(f->args)[j]]);
    } else {
      printf("x%d", DECODE_VAR( f->args[j] ));
    }
  }
  printf(")");

}



void print_ft_name( int index )

{

  print_Fact( &(grelevant_facts[index]) );

}



void print_fl_name( int index )

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

  if ( grelevant_fluents_lnf[index] == NULL ) {
    /* this is a non-artificial "atomic" one
     * (or the mirrored version of one)
     */
    printf("[RF%d](%s)", index, grelevant_fluents_name[index]);
  } else {
    /* this only summarizes a LNF requirement
     */
    printf("[artRF%d]", index);
    for ( i = 0; i < grelevant_fluents_lnf[index]->num_pF; i++ ) {
      printf("%.2f*", grelevant_fluents_lnf[index]->pC[i] );
      print_fl_name( grelevant_fluents_lnf[index]->pF[i] );
      if ( i < grelevant_fluents_lnf[index]->num_pF - 1 ) {
	printf(" + ");
      }
    }
  }

}



void print_LnfExpNode( LnfExpNode *n )

{

  int i;

  printf("((");
  for ( i = 0; i < n->num_pF; i++ ) {
    printf("%.2f*", n->pC[i]);
    print_fl_name( n->pF[i] );
  }
  printf(") - (");
  for ( i = 0; i < n->num_nF; i++ ) {
    printf("%.2f*", n->nC[i]);
    print_fl_name( n->nF[i] );
  }
  printf(") + %.2f)", n->c);

}



void print_op_name( int index )

{

  int i;
  Action *a = gop_conn[index].action;
  char *str = exchange2(a->name);

  if ( !a->norm_operator &&
       !a->pseudo_action ) {
    printf("REACH-GOAL");
  } else {
    /*printf("(%s ", a->name);*/
    printf("(%s ", str);
    for ( i = 0; i < a->num_name_vars; i++ ) {
      printf("%s ", gconstants[a->name_inst_table[i]]);
    }
      
    /*------duration-show---------*/
    if(isDurative==TRUE){
	  
      /*	    printf("\nduration: %.2f",gop_conn[index].duration);*/
      printf(") [%.2f]",gop_conn[index].duration);    
	  
    }
/*
   else if (isTotalCost==TRUE){
     printf(") [%.0f]", gop_conn[index].cost);
   }else{
     printf(") [%i]", 1);
   }
*/
    printf(")");


	
    /*---end-of---duration-show-----*/    
      
      
  }
  
  

  
  /*    printf("%s", a->name ); 
	for ( i = 0; i < a->num_name_vars; i++ ) {
	printf(" %s", gconstants[a->name_inst_table[i]]);
	}
  */
  

  
  
}



void print_State( State S )

{

  int i;
  
  for ( i = 0; i < S.num_F; i++ ) {
    printf("\n");
    print_ft_name( S.F[i] );
  }
  for ( i = 0; i < gnum_relevant_fluents; i++ ) {
    printf("\n");
    print_fl_name( i );
    printf(": ");
    if ( S.f_D[i] ) {
      printf("%.2f", S.f_V[i]);
    } else {
      printf("UNDEF");
    }
  }

}


/* for Parallel Solution File user ddurative pert
 */

void printTokenList_for_UPSolutinoFile( TokenList *list, char *sep )

{      

  TokenList *i_tl;

  i_tl = list;
  if (NULL!=i_tl) {
    fprintf(pfp,"(%s", i_tl->item);
    i_tl = i_tl->next;
  } else {
    fprintf(pfp,"empty");
  }
  
  while (NULL != i_tl) {
    fprintf(pfp, "%s%s", sep, i_tl->item);
    i_tl = i_tl->next;
  }   
 
   

}





void print_op_namePTimeSolutinoFile( int index )

{

  int i;
  Action *a = gop_conn[index].action;
  char *str = exchange2(a->name);

  if ( !a->norm_operator &&
       !a->pseudo_action ) {
    
  } else {

    /* fprintf(pfp,": (%s ", a->name);*/

    fprintf(pfp,": (%s ", str);

    for ( i = 0; i < a->num_name_vars; i++ ) {
      
      fprintf(pfp, "%s ", gconstants[a->name_inst_table[i]]);

    }
    
   
    fprintf(pfp, ") [%.2f]\n", gop_conn[index].duration); 

  }

}


/*+++++++++++++++++++++++++*/

void print_op_nameInDependFile( int index )

{
  
  int i;
  
  Action *a = gop_conn[index].action;

    
  fprintf(fp2,"(%s", a->name); 
 

  for ( i = 0; i < a->num_name_vars; i++ ) {
    fprintf(fp2," %s", gconstants[a->name_inst_table[i]]);
  }
  fprintf(fp2, ")");
  

}


/*+++++++++++++++++++++++++*/

void print_op_namePSolutinoFile( int index )

{

  int i;
  Action *a = gop_conn[index].action;
  char *str = exchange2(a->name);

  if ( !a->norm_operator &&
       !a->pseudo_action ) {
    
  } else {

    /* fprintf(pfp,": (%s ", a->name);*/

    fprintf(pfp,": (%s ", str);

    for ( i = 0; i < a->num_name_vars; i++ ) {
      
      fprintf(pfp, "%s ", gconstants[a->name_inst_table[i]]);

    }
    

    fprintf(pfp, ") [%i]\n", 1);  

  }

}





void print_op_nameSolutionFile( int index )

{

  int i;
  Action *a = gop_conn[index].action;
  char *str = exchange2(a->name);

  if ( !a->norm_operator &&
       !a->pseudo_action ) {
    
  } else {
    

    /*fprintf(fp,": (%s ", a->name);*/
 
    fprintf(fp,": (%s ", str);

    for ( i = 0; i < a->num_name_vars; i++ ) {

      fprintf(fp, "%s ", gconstants[a->name_inst_table[i]]);
    
    }
/*
    if (isTotalCost == TRUE){
      fprintf(fp, ") [%.0f]\n", gop_conn[index].cost);  
    }else{
      fprintf(fp, ") [%i]\n", 1);  
    }
*/
    fprintf(fp,")\n");
  }
}




void print_op_nameTimeSolutionFile( int index )

{

  int i;
  Action *a = gop_conn[index].action;
  char *str;

  if ( !a->norm_operator &&
       !a->pseudo_action ) {
    
  } else {
 
      
    fprintf(fp, "%.2f", starttime);
     
    str = exchange2(a->name);

    /* fprintf(fp,": (%s ", a->name);*/

    fprintf(fp,": (%s ", str);

    printf( "%.2f", starttime);

    /* printf(": (%s ", a->name);*/
    printf(": (%s ", str);
 

    for ( i = 0; i < a->num_name_vars; i++ ) {

      fprintf(fp, "%s ", gconstants[a->name_inst_table[i]]);   
      printf("%s ", gconstants[a->name_inst_table[i]]);   

    }
    fprintf(fp, ") [%.2f]\n", gop_conn[index].duration);
    printf(") [%.2f]", gop_conn[index].duration);
 
    starttime= starttime+ gop_conn[index].duration +0.01;
     
  }

}





/*
 * program output routines
 */




void print_plan( void )

{  
  
  int i;
  float MaxTime = 0.00;

  starttime = 0.00;

  printf("\n\nMIPS-XXL: found legal plan as follows");
  printf("\n\n");

  if((fp = fopen("mipsSolution.soln", "w"))==NULL) {
    printf("canno' t open a file\n");
    exit(1);
  }

  /* fprintf(fp, "; Time %.2f\n", 
     gtempl_time + greach_time + grelev_time + gLNF_time + gconn_time + gsearch_time );
  fprintf(fp, "; ParsingTime\n"); */

  /*fprintf(fp, "; ParsingTime %.2f\n", gtempl_time);*/
  fprintf(fp, "; NrActions %d\n", gnum_plan_ops);
  if(!isDurative)
    fprintf(fp, "; MakeSpan %d\n", gnum_plan_ops);
  else{
    for(i = 0; i < gnum_plan_ops; i++ )
      MaxTime = MaxTime + gop_conn[ gplan_ops[i]].duration + 0.01;
    fprintf(fp, "; MakeSpan %.2f\n", MaxTime );
  }
      
  fprintf(fp, "; MetricValue %.2f\n", prune);
  if ( gcmd_line.ehc )
    fprintf(fp, "; PlaningTechnique ehc\n" );
  else if (gcmd_line.external)
    fprintf(fp, "; PlaningTechnique External BFS\n");
  else if (gcmd_line.branch_and_bound)
    fprintf(fp, "; PlaningTechnique External Branch-and-Bound\n");
  else if (gcmd_line.dijkstra)
    fprintf(fp, "; PlaningTechnique External Dijkstra\n" );
  else
    fprintf(fp, "; PlaningTechnique best-first\n" );


  for ( i = 0; i < gnum_plan_ops; i++ ) {

    /*printf("output. printing %d action \n", i);*/
    
    if(isDurative==FALSE){
      /*printf("isDurative == FALSE\n");*/ 
      printf("%d: ", i); 
      fprintf(fp, "%i", i);

      print_op_name( gplan_ops[i] );
	
      print_op_nameSolutionFile( gplan_ops[i] );
	 
      printf("\n");
    }

    else {

      print_op_nameTimeSolutionFile( gplan_ops[i] );
      printf("\n");
   
    }
  }

  fclose(fp);

}








void print_TypedList(TypedList *tpl)
{
  TypedList *Anker=NULL;
  for(Anker=tpl;Anker!=NULL;Anker=Anker->next)
    {
      printf("\n %s ",Anker->name);
      print_hidden_TokenList(Anker->type," "); /* 4);*/
    }
	
}



void print_Timed_Initial_Literals(void)
{
  Time_Ini_Literal *lauf = NULL;
  if(Timed_Initial_Literals==NULL)
    {
      printf("empty Timed_Initial_Literals");
    } 
  else
    {
      printf("\n Ausdruck der intial literal");
      for(lauf=Timed_Initial_Literals;lauf!=NULL;lauf=lauf->next)
	{ 
	  printf("\n"); 
	  print_hidden_TokenList(lauf->pre," ");
	  printf("\n");
	  printf("Anzahl der Intervall %i",lauf->numofint);
	  printf("\n");
	  TimeWindows *lauf0=lauf->tw;
	  while(lauf0)
	    {
	      printf("MINTIME= %f und MAXTIME = %f",lauf0->mintime,lauf0->maxtime);
	      printf("\n");
	      lauf0=lauf0->next;
	    }
	}
    }
}

/*+++++++++++++++++++++++++++++++++++*/

void print_XML_File(void)

{ 

  XMLDependOnOp *Anker2=NULL;
  XMLOperator *Anker1 =NULL;

  for(Anker1=op_depend_op;Anker1!=NULL;Anker1=Anker1->next)
    {

      printf("*****************Operator s name***************** \n");
      print_hidden_TokenList(Anker1->name," ");
      printf("\n --------depend on------------- \n"); 

      for(Anker2=Anker1->depend_on_ops; Anker2 !=NULL;Anker2=Anker2->next)
	{

	  print_hidden_TokenList(Anker2->name," \n");
          TypedList *Anker3=NULL;
          for(Anker3=Anker2->depend_art;Anker3!=NULL;Anker3=Anker3->next)

            {

	      printf("\n depend art is");
	      print_hidden_TokenList(Anker3->type," ");

	    }
	}
    }
}

