



/*********************************************************************
 * File: pddl.c
 * Description: Functions for the pddl dependend part of
 *              the parser and the preprocessing.
 *
 * Authors: Frank Rittinger, Andreas Schoen 1998
 *
 *********************************************************************/ 
/*********************************************************************
 * (C) Copyright 1998 Albert Ludwigs University Freiburg
 *     Institute of Computer Science
 *
 * All rights reserved. Use of this software is permitted for 
 * non-commercial research purposes, and it may be copied only 
 * for that use.  All copies must include this copyright message.
 * This software is made available AS IS, and neither the authors
 * nor the  Albert Ludwigs University Freiburg make any warranty
 * about the software or its performance. 
 *********************************************************************/
/*********************************************************************
 * INTRODUCTION TO THIS FILE
 *
 * All(?) functions are programmed recursively and not dynamically 
 * but this should not affect the efficiency, as these functions are called
 * only once in the beginning, and in most domains the recusion 
 * depth is not high.
 * Exceptions are dnf and their dependent(?help/sub-) functions
 * distribute_and_ors and append_following. They might be called often
 * during the instantiation
 * 
 * All function, that have pl-nodes as both input and output 
 * keep the input node fixed!  (for convenience in the calling function)
 *
 * All functions assume they are allowed to free nodes the plnode tree doesn't 
 *   need anymore. 
 *   
 *
 * i_pl,j_pl,k_pl,l_pl always are Laufvars(?l)
 *********************************************************************/
/*********************************************************************
 *
 * Dependencies of functions:
 * (1)
 * pddl(16)-+----->rename_all_quantifiers*(13)----->rename_quantifier*(14)
 * (2)      |
 *          +----->axioms_to_operators->...
 * (3)      |                                                         
 *          +-->ands_to_top*(3)--->combine_whens*(2)-->contains_when*(1)
 *          |      |                     |   ^            ^                 
 *          |      |                     v   |            |
 *          |      |         distribute_whens,2nd branch--+
 *          |      v    
 *          | distribute_whens,1st branch ;distribute_unary;combine_ands
 *          |                                                         
 * (4)      |                                                         
 *          |                                                         
 *          |                                                         
 *          +-------+->nots_down_qunatifiers_up*(11)<----+
 *          |       |                            |             
 *          |       +-->nots_down(10)------------+
 *          |       |       v                    |             
 *          |       |     distibute_unary        |             
 *          |       |                            |             
 *          |       +->all_the quantifiers_up(9)-+
 *          |                                    |            
 *                                               v            
 *                                    move_quantifier(8)
 * (5) ,trafo (6)                                                
 * pddl_inner(16)----->dnf*(6)<->distribute_and_ors(5)
 *                         |      |               |           
 *                         |      v               v  
 *                         +-combine_ands     append_following(5)          
 * (7)
 *
 * fn_name*(position in file) ,* means function calls itself
 *
 *
 *
 * Used basic functions:
 *
 * distribute_unary(6)
 * combine_ands(12)
 * 
 *********************************************************************/

#include "ipp.h"
#include "pddl.h"
#include "memory.h"
#include "utilities.h"
#include "output.h"

#define LOG_Q_NUMBER 8  /* so there can be 10^(6-3)=1000 quantifiers */
		

/* #include "/home/keim/pur4.1/purify-4.1-solaris2/purify.h" */

/* Laufv?l for number of quantifiers, needed for quantifier renaming */
static int snr_quantifiers;

int dep=0;

/*  void copy_contents_of_CodeNode( CodeNode **dest, CodeNode *source ); */
/* in instantiate.c */


void 
begin_of_function_pl ( char * name , PlNode * plnode)
{
  if (0 && gcmd_line.display_info>=20)
    { 
      dep++;
      /* if (purify_new_leaks()) */
/* 	    { */
/* 	      printf("New leaks\n"); */
/* 	    }  */
/*       else  */
/* 	{ */
/* 	  printf("No New leaks\n"); */
/* 	} */
      printf("\n%u%s\n",dep,name);
      print_plnode( plnode ,0);
    }
}
void 
end_of_function_pl ( char * name , PlNode * plnode)
{
  if (0 && gcmd_line.display_info>=20)
    { 
      /* if (purify_new_leaks()) */
/* 	    { */
/* 	      printf("New leaks\n"); */
/* 	    }  */
/*       else  */
/* 	{ */
/* 	  printf("No New leaks\n"); */
/* 	} */
      printf("\n%uEnd %s\n",dep,name);
      print_plnode( plnode ,0);
      dep--;
    }
}
void 
begin_of_function_code ( char * name , CodeNode * codenode)
{
  if (0 && gcmd_line.display_info>=20)
    { 
      dep++;
      /* if (purify_new_leaks()) */
/* 	    { */
/* 	      printf("New leaks\n"); */
/* 	    }  */
/*       else  */
/* 	{ */
/* 	  printf("No New leaks\n"); */
/* 	} */
      printf("\n%u%s\n",dep,name);
      print_CodeNode( codenode ,0);
    }
}
void 
end_of_function_code ( char * name , CodeNode * codenode)
{
  if (0 && gcmd_line.display_info>=20)
    { 
      /* if (purify_new_leaks()) */
/* 	    { */
/* 	      printf("New leaks\n"); */
/* 	    }  */
/*       else  */
/* 	{ */
/* 	  printf("No New leaks\n"); */
/* 	} */
      printf("\n%uEnd %s\n",dep,name);
      print_CodeNode( codenode ,0);
      dep--;
    }
}

/******************** quantifiers_renaming  ***************************/
/*********************************************************************
 * INPUT  plnode: any pl-tree
 * OUTPUT plnode: equivalent tree with all the quantified variables renamed 
 * to ??x , x being a counter
 * USING  
 *********************************************************************/
void
rename_all_quantifiers(PlNode * plnode)
{
  Bool contained;
  PlNode * i_pl;
  begin_of_function_pl("Rename_all_quantifiers",plnode);
  switch (plnode->connective)
    {    
    case EX:
    case ALL:
      rename_all_quantifiers(plnode->sons);
      contained=
	rename_quantifier(plnode,plnode->atom->item,FALSE);
      snr_quantifiers++;
      if (!contained)
	{
	  printf("Quantor wegschmeissen noch nicht implementiert.");
	}
      break;
    case AND:
    case OR:
    case WHEN:
      {
	i_pl=plnode->sons;    
	while (i_pl!=NULL)
	  {
	    rename_all_quantifiers(i_pl);
	    i_pl=i_pl->next;
	  }
      }
      break;
    case NOT:
      rename_all_quantifiers(plnode->sons);
    case ATOM:
    case TRU:
      break;
    default:
      spec_error("Rename_all_quantifiers");
    }
  end_of_function_pl("Rename_all_quantifiers",plnode);
}

/*********************************************************************
 * This function renames all occurences of the variable token to
 *  ??x, where x is the current value of the static counter snr_quantifiers
 * INPUT  plnode: pl-tree
 *        token: the name of the variable to be renamed
 *        cont:TRUE if the variable occurred so far 
 *                 (just for stupid domain definitions)
 * OUTPUT Bool: TRUE if the variable occurred so far or now
 * USING  
 *********************************************************************/
Bool
rename_quantifier(PlNode * plnode,char * token,Bool cont)
{
  PlNode * i_pl;
  TokenList * i_tl;
  char * i_token;
  begin_of_function_pl("Rename_quantifier",plnode);
  i_token=new_token(strlen(token)+1);
  strcpy(i_token,token);
  switch (plnode->connective)
    {
    case ALL:
    case EX:
      if (SAME==strcmp(plnode->atom->item,i_token))
	{
	  free(plnode->atom->item);
	  plnode->atom->item = new_token(LOG_Q_NUMBER);
	  sprintf(plnode->atom->item,"??%d",snr_quantifiers);

	  cont=TRUE;
	}
      cont=rename_quantifier(plnode->sons,i_token,cont);
      break;
    case NOT:
      cont=rename_quantifier(plnode->sons,i_token,cont);
      break;
    case AND:
    case OR:
    case WHEN:
      i_pl=plnode->sons;    
      while (i_pl!=NULL)
	{
	  cont|=rename_quantifier(i_pl,i_token,cont);
	  i_pl=i_pl->next;
	}
      break;
    case TRU:
      break;
    case ATOM:
      i_tl=plnode->atom;
      while (i_tl!=NULL)
	{
	  if (SAME==strcmp(i_tl->item,i_token))
	    {
	      free(i_tl->item);
	      i_tl->item = new_token(LOG_Q_NUMBER);
	      sprintf(i_tl->item,"??%d",snr_quantifiers);
	      cont=TRUE;
	    }
	  i_tl=i_tl->next;
	}
      break;
    default:
      spec_error("Rename_quantifier");
    }
  free(i_token);
  end_of_function_pl("Rename_quantifier",plnode);
  return cont;
}

/******************** pddl_outer ***************************/

/*********************************************************************
 * INPUT  plnode: AND or OR node 
 * OUTPUT plnode: (same) AND/OR node with all nodes with the same connective 
 *   directly below combined to one single AND node
 * USING  
 *********************************************************************/
void
combine_ands_pl ( PlNode * plnode )
{
  PlNode * i_pl,*j_pl,*this_pl;
  Connective con=plnode->connective;
  
  begin_of_function_pl("Combine ands",plnode);
  if (plnode->connective!=AND && plnode->connective!=OR) 
    {
      spec_error("Combine ands");
    }
  i_pl=new_pl_node(DUMMY);
  this_pl=plnode->sons;
  i_pl->next=this_pl;
  plnode->sons=i_pl;
  while (this_pl !=NULL)
    {
      if (this_pl->connective==con)
	{
	  combine_ands_pl(this_pl);
	  i_pl->next=this_pl->sons;
	  j_pl=this_pl->next;
	  free(this_pl);
	  this_pl=j_pl;
	  while (NULL!=i_pl->next)
	    {
	      i_pl=i_pl->next;
	    }
	}
      else /*  !=AND  while  */
	{
	  i_pl->next=this_pl;
	  i_pl=this_pl;
	  this_pl=this_pl->next;
	}
    } /*  while  */
  i_pl=plnode->sons->next;
  free(plnode->sons);/* DUMMY */
  plnode->sons=i_pl;
  end_of_function_pl("Combine ands",plnode);
} 

/*********************************************************************
 * INPUT  plnode: pl-tree
 * OUTPUT Bool: true iff the tree below (including plnode) contains a when node
 * USING  
 *********************************************************************/
Bool 
contains_when ( PlNode * plnode )
{
  PlNode * i_pl;
  
  switch (plnode->connective)
    {
    case AND:
      for (i_pl=plnode->sons;i_pl!=NULL;i_pl=i_pl->next)
	{
	  if (contains_when(i_pl))
	    {
	      return TRUE;
	    }
	}
      return FALSE;
    case ALL:
      return contains_when(plnode->sons);
    case WHEN:
      return TRUE;
    case NOT:
      return FALSE;
    case ATOM:
      return FALSE;
    default:
      spec_error("Contains_when");
      return TRUE;
    }
  
}

/*********************************************************************
 * INPUT  plnode: WHEN node with another when node below
 * OUTPUT plnode: a logically equivalent tree without exactly one WHEN node
 * USING  
 *********************************************************************/
void
combine_whens ( PlNode * plnode )
{
  PlNode * new_pl;
  PlNode * son=plnode->sons->next; /* effect */

  begin_of_function_pl("Combine_whens",plnode);
  switch (son->connective)
    {
    case AND:
      distribute_when(plnode,FALSE);
      break;
    case ALL:
      /* puts quantifier above when node */
      plnode->sons->next=son->sons;
      plnode->atom=son->atom;
      son->atom=NULL;
      son->sons=plnode->sons;
      son->connective=WHEN;
      plnode->connective=ALL;
      plnode->sons=son;
      combine_whens(son);
      break;
    case WHEN:
      if (contains_when(son->sons->next))
	{
	  combine_whens(son);
	}
      if (WHEN==son->connective)  /* melts directly following whens */
	{
	  if (TRU==son->sons->connective)
	    {
	      plnode->sons->next=son->sons->next;
	      free(son->sons);
	      free(son);
	    }
	  else
	    {
	      new_pl=new_pl_node(AND);
	      new_pl->sons=plnode->sons;
	      new_pl->sons->next=son->sons;
	      new_pl->next=son ->sons->next;
	      free(son);
	      plnode->sons=new_pl;
	      new_pl->sons->next->next=NULL;
	    }
	}
      else
	{
	  combine_whens(plnode);
	}
      break;
    default:
      fprintf(stdout,"wrong connective in combine_whens");/* spaeter weg */
    }
  end_of_function_pl("Combine_whens",plnode);
}

/*********************************************************************
 * This is the analogue function to distribute_unary.
 * It looks quite different due to the structure of WHEN plnodes
 * Instead of rec_fn both possible calling case are integrated into this 
 * function (I call it: inner functions, 1st/2nd branch
 *
 * INPUT  plnode: WHEN node
 * OUTPUT plnode: copies the WHEN node to above all the sons of the son,
 *   calls inner function applied to all these copied nodes afterwards,
 *  the original WHEN is deleted (resp. replaced by its son)
 * USING  
 *********************************************************************/
void
distribute_when (PlNode * plnode,const Bool lowest)
{ 
  PlNode * i_pl, * new_pl, * k_pl;  
  i_pl=new_pl_node(DUMMY);
  i_pl->next=plnode->sons->next->sons;
  plnode->sons->next->sons=i_pl;
  for (;i_pl->next!=NULL;i_pl=i_pl->next)
    {
      new_pl=new_pl_node(WHEN);
      new_pl->sons=copy_pl_node(plnode->sons);
      new_pl->sons->sons=deep_copy_tree(plnode->sons->sons);
      new_pl->sons->next=i_pl->next;
      i_pl->next=new_pl;
      new_pl->next=new_pl->sons->next->next;
      new_pl->sons->next->next=NULL;
      /* inner functions */
      if (lowest)
	{
	  /* 1st branch */
	  
	  /* quantifiers above WHEN */
	  if (ALL==new_pl->sons->next->connective)
	    {
	      k_pl=new_pl->sons->next;
	      while (k_pl->sons->connective!=ALL)
		{
		  k_pl=k_pl->sons;
		}
	      i_pl->next=new_pl->sons->next;
	      i_pl->next->next=new_pl->next;
	      new_pl->sons->next=k_pl->sons->sons->next;
	      free(k_pl->sons->sons);/*  WHEN */
	      free(k_pl->sons);/* TRU */
	      k_pl->sons=new_pl;
	      new_pl->next=NULL;
	    }
	}
      else
	{
	  /* 2nd branch */
	  if (contains_when(i_pl->next->sons->next)) 
	    {
	      combine_whens(i_pl->next);
	    }	
	}
    }
  i_pl=plnode->sons->next->sons->next;
  free(plnode->sons->next->sons);/* DUMMY */
  plnode->connective=plnode->sons->next->connective;
  plnode->atom=plnode->sons->next->atom;
  free_pl_node(plnode->sons->next);
  free_tree(plnode->sons->sons);
  free_pl_node(plnode->sons);
  plnode->sons=i_pl;
}

/*********************************************************************
 * INPUT  plnode: effect tree, multiple WHENs
 * OUTPUT plnode: effect tree,
 *            1st single AND (already melted together)
 *            2nd :(ALL) quantifiers, 
 *            3rd single WHENs (melted together using combine_whens)
 *                   on paths without WHEN, Literal is replaced by 
 *                   (WHEN TRU Literal) 
 * USING  
 *********************************************************************/
void
ands_to_top ( PlNode * plnode )
{
  PlNode * new_pl,*new_pl2,*i_pl,*j_pl;
  TokenList *t1, *t2;

  begin_of_function_pl("Ands to top",plnode);
  if (FALSE==contains_when(plnode))
    {
      nots_down_quantifiers_up_pl(plnode);/* quants_up reicht */
      /* state jetzt:ALL AND Lit */
      /*   jetzt IF TRU zwischen Quantoren und ANDs haengen */
      if (ALL==plnode->connective)
	{
	  /* step through quantifiers */
	  i_pl=plnode;
	  while (ALL==i_pl->sons->connective )
	    {
	      i_pl=i_pl->sons;
	    }
	  new_pl=new_pl_node(TRU);
	  new_pl->next=i_pl->sons;
	  new_pl2=new_pl_node(WHEN);
	  new_pl2->sons=new_pl;
	  i_pl->sons=new_pl2;
	}
      else
	{
	  new_pl=new_pl_node(TRU);
	  new_pl->next=copy_pl_node(plnode);
	  new_pl->next->sons=plnode->sons;
	  plnode->connective=WHEN;
	  plnode->sons=new_pl;
	  t1 = plnode->atom;
	  while ( t1 ) {
	    t2 = t1->next;
	    if ( '?' == *(t1->item) ) {
	      free( t1->item );
	    }
	    free( t1 );
	    t1 = t2;
	  }
	}
    }
  if (0 && gcmd_line.display_info>=20)
    { 
      printf("\n%u MITTEN in andstotop\n",dep);
      print_plnode( plnode ,0);
    }
  switch (plnode->connective)
    {
    case AND:
      for(i_pl=plnode->sons;NULL!=i_pl;i_pl=i_pl->next)
	    {
	      ands_to_top(i_pl);
	    }
      combine_ands_pl(plnode);
      break;
    case ALL:
      ands_to_top(plnode->sons);
      if (plnode->sons->connective!=AND)
	{
	  spec_error("Ands_to_top1");
	}
      distribute_unary_pl(plnode,NULL);
      break;
    case WHEN: 
      if (TRUE==contains_when(plnode->sons->next))
	{
	  combine_whens(plnode);
	  ands_to_top(plnode);
	}
      else
	{ 
	  /* pop_pl(AND,plnode)=schiebe an Stelle von plnode neuen AND */
	  new_pl=new_pl_node(WHEN); 
	  new_pl->sons=plnode->sons; 
	  plnode->connective=AND;
	  i_pl=plnode->sons->next;
	  if (ALL==i_pl->connective )
	    {
	      j_pl=i_pl;
	      while (ALL==i_pl->sons->connective )
		{
		  i_pl=i_pl->sons;
		}
	      plnode->sons=j_pl;
	      new_pl->sons->next=i_pl->sons;
	      i_pl->sons=new_pl;
	      if (new_pl->sons->next->connective!=NOT && 
		  new_pl->sons->next->connective!=ATOM)
		{
		  combine_ands_pl(new_pl->sons->next);
		}
	    }
	  else
	    {
	      plnode->sons=new_pl;
	      if (i_pl->connective!=NOT && i_pl->connective!=ATOM)
		{
		  combine_ands_pl(i_pl);
		}
	    }
	}
      break;
    default:
      spec_error("Ands to top");
    }
  end_of_function_pl("Ands to top",plnode);
}  

/*********************************************************************
 * INPUT  plnode: NOT, ALL, EX node
 * OUTPUT plnode: copies the unary node to above all the sons of the son,
 *   calls rec_fn appled to all these copied nodes afterwards, the original
 * plnode is deleted (resp. replaced by its son)
 * eg. NOT                      AND
 *      |                        |
 *     AND            becomes   NOT----NOT
 *      |                        |      |
 *     node 1--node 2           node 1 node 2
 *  and rec_fn ( NOT ) and rec_fn( NOT ) are called
 *                |                 |
 *              node 1            node2
 * USING  
 *********************************************************************/
void
distribute_unary_pl (PlNode * plnode,void ( * rec_fn)(PlNode * plnode))
{

  PlNode * i_pl, * new_pl;  
  TokenList *t1, *t2;

  i_pl=new_pl_node(DUMMY);
  i_pl->next=plnode->sons->sons;
  plnode->sons->sons=i_pl;
  for (;i_pl->next!=NULL;i_pl=i_pl->next)
    {
      new_pl=copy_pl_node(plnode);
      new_pl->sons=i_pl->next;
      i_pl->next=new_pl;
      new_pl->next=new_pl->sons->next;
      new_pl->sons->next=NULL;
      if (NULL!=rec_fn)
	{
	  ( * rec_fn)(new_pl);
	}
    }
  i_pl=plnode->sons->sons->next;
  free(plnode->sons->sons);/* DUMMY */
  plnode->connective=plnode->sons->connective;

  t1 = plnode->atom;
  while( t1 ) {
    t2 = t1->next;
    if ( '?' == *(t1->item) ) {
      free( t1->item );
    }
    free( t1 );
    t1 = t2;
  }

  plnode->atom=plnode->sons->atom;
  free(plnode->sons);
  plnode->sons=i_pl;
}


/*********************************************************************
 * INPUT  plnode: condition tree (no WHENs)
 * OUTPUT plnode: equivalent precondition tree,
 *            1st layer at the top: quantifiers
 *            2nd : ANDs/ORs (not melted together yet)
 *            3rd : Literals (NOTs and ATOMs)
 * USING  
 *********************************************************************/
void
nots_down_quantifiers_up_pl (PlNode * plnode)
{
  PlNode * i_pl;
  begin_of_function_pl("Nots_down_quantifiers_up_pl",plnode);
  switch (plnode->connective)
    {
    case NOT:
      nots_down_quantifiers_up_pl(plnode->sons);
      switch (plnode->sons->connective)
	{
	case NOT:
	  /* delete both NOT nodes */
	  i_pl=plnode->sons;
	  plnode->atom=plnode->sons->sons->atom;
	  plnode->connective=plnode->sons->sons->connective;
	  plnode->sons=plnode->sons->sons->sons;
	  free(i_pl->sons);
	  free(i_pl);
	  break;
	case AND:
	  distribute_unary_pl(plnode,nots_down_quantifiers_up_pl);
	  plnode->connective=OR;
	  break;
	case OR:
	  distribute_unary_pl(plnode,nots_down_quantifiers_up_pl);
	  plnode->connective=AND;
	  break;
	case ALL:
	  plnode->atom=plnode->sons->atom;
	  plnode->connective=EX;
	  plnode->sons->atom=NULL;
	  plnode->sons->connective=NOT;
	  nots_down_quantifiers_up_pl(plnode);
	  break;
	case EX:
	  plnode->atom=plnode->sons->atom;
	  plnode->connective=ALL;
	  plnode->sons->atom=NULL;
	  plnode->sons->connective=NOT;
	  nots_down_quantifiers_up_pl(plnode);
	  break;
	case ATOM:
	  break;
	default:
	  spec_error("Nots_down_pl");
	}
      break;
    case AND:
    case OR:
      all_the_quantifiers_top_pl(plnode);
      break;
    case ALL:
    case EX:
      nots_down_quantifiers_up_pl(plnode->sons);
      break;
    case ATOM:
    case TRU:
      break;
    default:
      spec_error("Nots_down_quantifiers_up_pl");
    }
  end_of_function_pl("Nots_down_quantifiers_up_pl",plnode);
}
/*********************************************************************
 * INPUT  plnode: condition tree (no WHENs)
 * OUTPUT plnode: equivalent precondition tree,
 *            1st layer at the top: quantifiers
 *            2nd : ANDs/ORs (not melted together yet)
 *            3rd : Literals (NOTs and ATOMs)
 * USING  
 *********************************************************************/
void
nots_down (PlNode * plnode)
{
  PlNode * i_pl;
  begin_of_function_pl("Nots_down",plnode);
  switch (plnode->connective)
    {
    case NOT:
      nots_down(plnode->sons);
      switch (plnode->sons->connective)
	{
	case NOT:
	  /* delete both NOT nodes */
	  i_pl=plnode->sons;
	  plnode->atom=plnode->sons->sons->atom;
	  plnode->connective=plnode->sons->sons->connective;
	  plnode->sons=plnode->sons->sons->sons;
	  free(i_pl->sons);
	  free(i_pl);
	  break;
	case AND:
	  distribute_unary_pl(plnode,nots_down_quantifiers_up_pl);
	  plnode->connective=OR;
	  break;
	case OR:
	  distribute_unary_pl(plnode,nots_down_quantifiers_up_pl);
	  plnode->connective=AND;
	  break;
	case ALL:
	  plnode->atom=plnode->sons->atom;
	  plnode->connective=EX;
	  plnode->sons->atom=NULL;
	  plnode->sons->connective=NOT;
	  nots_down_quantifiers_up_pl(plnode);
	  break;
	case EX:
	  plnode->atom=plnode->sons->atom;
	  plnode->connective=ALL;
	  plnode->sons->atom=NULL;
	  plnode->sons->connective=NOT;
	  nots_down_quantifiers_up_pl(plnode);
	  break;
	case ATOM:
	  break;
	default:
	  spec_error("Nots_down");
	}
      break;
    case AND:
    case OR:
      i_pl=plnode->sons;
      while (i_pl!=NULL)
	{
	  nots_down(i_pl);
	  i_pl=i_pl->next;
	}
      break;
    case ALL:
    case EX:
      nots_down(plnode->sons);
      break;
    case ATOM:
    case TRU:
      break;
    default:
      spec_error("Nots_down");
    }
  end_of_function_pl("Nots_down",plnode);
}



/*********************************************************************
 * This function removes vertical quantifier list at bef_from_pl->next
 *  and copies it between bef_to_pl and bef_to_pl->next, returns last 
 *  quantifier
 * INPUT  plnode
 * OUTPUT plnode: the  last element of the new quantifier list
 * USING  
 *********************************************************************/
PlNode *
move_quantifier_pl (PlNode * bef_from_pl ,PlNode * bef_to_pl)  
{
  PlNode *i_pl,*from_pl,*to_pl;
  from_pl=bef_from_pl->next;
  to_pl=bef_to_pl->sons;
      /*step through quantifiers */
  i_pl=from_pl;
  while (ALL==i_pl->sons->connective || EX==i_pl->sons->connective)
    {
      i_pl=i_pl->sons;
    }
  bef_from_pl->next=i_pl->sons;
  i_pl->sons->next=from_pl->next;
  i_pl->sons=bef_to_pl->sons;
  bef_to_pl->sons=from_pl;
  return i_pl;
}

/*********************************************************************
 * This function does the same as (is just the AND/OR branch of) 
 *   nots_down_qunatifiers_up_pl
 * INPUT  plnode: AND or OR node 
 * OUTPUT plnode: see nots_down_qunatifiers_up_pl
 * USING  
 *********************************************************************/
void
all_the_quantifiers_top_pl (PlNode * plnode)
{
  PlNode  * i_pl , * j_pl,*k_pl,* l_pl ,*new_pl,*m_pl,*dummy_quantifier_list;
  begin_of_function_pl("All_the_quantifiers_top_pl",plnode);
  l_pl=new_pl_node(DUMMY);
  l_pl->next=plnode->sons;
  plnode->sons=l_pl;
  j_pl=l_pl;
  i_pl=l_pl->next;
  dummy_quantifier_list=new_pl_node(DUMMY);
  k_pl=dummy_quantifier_list;
  while (i_pl!=NULL)
    {
      nots_down_quantifiers_up_pl(i_pl);
      switch (i_pl->connective)
	{  
	case NOT:
	case ATOM:
	case AND:
	case OR:/* will be distributed later */
	  j_pl=j_pl->next;
	  i_pl=j_pl->next;
	  break;
	case ALL:
	  k_pl=move_quantifier_pl(j_pl,k_pl);
	  i_pl=j_pl->next;
	  break;
	case EX:
	  k_pl=move_quantifier_pl(j_pl,k_pl);
	  i_pl=j_pl->next;
	  break;
	default:
	  spec_error("All_the_quantifiers_top_pl");
	}
    }
  /* remove dummy 1 */
  m_pl=plnode->sons;
  plnode->sons=m_pl->next;
  free(m_pl);
  /* remove dummy 2&3 */
  if (NULL!=dummy_quantifier_list->sons)
    {
      new_pl=copy_pl_node(plnode);
      new_pl->sons=plnode->sons;
      k_pl->sons=new_pl;
      plnode->sons=dummy_quantifier_list->sons->sons;
      plnode->connective=dummy_quantifier_list->sons->connective;
      plnode->atom=dummy_quantifier_list->sons->atom;
      free(dummy_quantifier_list->sons);
      free(dummy_quantifier_list);
    }
  else
    {
      free(dummy_quantifier_list);
    }
  end_of_function_pl("All_the_quantifiers_top_pl",plnode);
}

/******************** pddl_inner ***************************/



/*********************************************************************
 * This function steps through next list and appends (real) copies 
 *  of to_copy_code after the last element of the son list; 
 *  all must have sons ;!j_code is changed, returns last of parents list
 * INPUT  j_code: to all son lists of this node a real copy of 
 *           (the tree below/including) to_copy_code
 *        to_copy_code: the node/tree to be copied
 * OUTPUT codenode: the last element of the parents list 
 *                        (the next successors of j_code)
 * USING  
 *********************************************************************/
CodeNode *
append_following(CodeNode *j_code,CodeNode *to_copy_code)
{
  CodeNode * i_code;
    do
      {
	if (AND!=j_code->connective || NULL==j_code->sons)  
	  {
	    spec_error("Append_following");
	  }
	for (i_code=j_code->sons ; NULL!=i_code->next ; i_code = i_code->next )
	  {
	    ;
	  }
	i_code->next=copy_CodeNode(to_copy_code);
	i_code->next->next=NULL;
	i_code->next->sons=deep_copy_CodeTree(to_copy_code->sons);
	if (NULL!=j_code->next)
	  {
	    j_code=j_code->next;
	  }
	else
	  {
	    return j_code;
	  }
      }
    while (TRUE);
}

/*********************************************************************
 * INPUT  codenode: AND node, below: ORs, ANDs, Lit
 * OUTPUT codenode: 
 *            [1st layer at the top: OR node if necessary]
 *            [2nd :AND nodes if necessary]
 *            3rd : Literals
 * USING  
 *********************************************************************/
void
distribute_ands_ors (CodeNode * codenode)
{
  CodeNode * i_code,*l_code,*m_code;/* Laufvars?l */
  CodeNode *tmplist,*tmp;
  begin_of_function_code("Distribute_ands_ors",codenode);
  combine_ands_code(codenode);
  codenode->connective=OR;

  i_code=codenode->sons;
  codenode->sons=new_CodeNode(AND);
  /* dummy */
  codenode->sons->sons=new_CodeNode(DUMMY);
  while (i_code!=NULL)
    {
      switch (i_code->connective)
	{
	case OR:
	  dnf(i_code);
          /* ?c gehe zunaechst davon aus, dass oben OR bleibt, spaeter soll 
	     auch TRU moeglich sein */
	  /* ?c muesste egal sein, ob AND oder nicht */
	  tmplist=new_CodeNode(DUMMY);/* dummy */
	  m_code=tmplist;
	  for (l_code=i_code->sons;NULL!=l_code;l_code=l_code->next)
	    {
	      if (NULL!=l_code->next)
		{
		  m_code->next=deep_copy_CodeTree(codenode->sons);/* copylist */
		}
	      else
		{
		  m_code->next=codenode->sons;
		  codenode->sons=tmplist->next;
		  free(tmplist);
		}
	      m_code=append_following(m_code->next,l_code);
	    }
	  break;
	case ATOM:
	case NOT: 
	  append_following(codenode->sons,i_code);
	  /* ?c spaeter hier remove p AND not p, im Notfall FAL ergenzen */
	  break;
	default:
	  spec_error("Distribute_ands_ors");
	}
      tmp=i_code;
      i_code=i_code->next;
      if (NULL!=tmp->sons) 
	{
	  free_CodeNode(tmp->sons);
	}
      free(tmp);
    }
  for (i_code=codenode->sons;NULL!=i_code;i_code=i_code->next)
    {
      tmp=i_code->sons;
      i_code->sons=i_code->sons->next;
      free(tmp);/*DUMMY */
      combine_ands_code(i_code);
    }
  if ((OR==codenode->connective)&&(NULL==codenode->sons->next))
    {
      i_code=codenode->sons;
      codenode->sons=codenode->sons->sons;
      codenode->connective=AND;
      free(i_code);
    }
 end_of_function_code("Distribute_ands_ors",codenode);
}

/*********************************************************************
 * INPUT  codenode: AND node, below: ORs, ANDs, Lit
 * OUTPUT codenode: 
 *            [1st layer at the top: OR node if necessary]
 *            [2nd :AND nodes if necessary]
 *            3rd : Literals
 * USING  
 *********************************************************************/
void
distribute_ors_ands (CodeNode * codenode)
{
  CodeNode * i_code,*l_code,*m_code;/* Laufvars?l */
  CodeNode *tmplist,*tmp;
  begin_of_function_code("Distribute_ors_ands",codenode);
  combine_ands_code(codenode);
  codenode->connective=AND;

  i_code=codenode->sons;
  codenode->sons=new_CodeNode(OR);
  /* dummy */
  codenode->sons->sons=new_CodeNode(DUMMY);
  while (i_code!=NULL)
    {
      switch (i_code->connective)
	{
	case AND:
	  dnf(i_code);
          /* ?c gehe zunaechst davon aus, dass oben OR bleibt, spaeter soll 
	     auch TRU moeglich sein */
	  /* ?c muesste egal sein, ob AND oder nicht */
	  tmplist=new_CodeNode(DUMMY);/* dummy */
	  m_code=tmplist;
	  for (l_code=i_code->sons;NULL!=l_code;l_code=l_code->next)
	    {
	      if (NULL!=l_code->next)
		{
		  m_code->next=deep_copy_CodeTree(codenode->sons);/* copylist */
		}
	      else
		{
		  m_code->next=codenode->sons;
		  codenode->sons=tmplist->next;
		  free(tmplist);
		}
	      m_code=append_following(m_code->next,l_code);
	    }
	  break;
	case ATOM:
	case NOT: 
	  append_following(codenode->sons,i_code);
	  break;
	default:
	  spec_error("Distribute_ors_and");
	}
      tmp=i_code;
      i_code=i_code->next;
      if (NULL!=tmp->sons) 
	{
	  free_CodeNode(tmp->sons);
	}
      free(tmp);
    }
  for (i_code=codenode->sons;NULL!=i_code;i_code=i_code->next)
    {
      tmp=i_code->sons;
      i_code->sons=i_code->sons->next;
      free(tmp);/*DUMMY */
      combine_ands_code(i_code);
    }
  if ((AND==codenode->connective)&&(NULL==codenode->sons->next))
    {
      i_code=codenode->sons;
      codenode->sons=codenode->sons->sons;
      codenode->connective=OR;
      free(i_code);
    }
 end_of_function_code("Distribute_ors_ands",codenode);
}

/*********************************************************************
 * This is the crucial function of the (precondition) pdnf,
 *   it could be called quite often, and should therefore be fast 
 *   ( contrary to the other functions of this file )
 * INPUT  codenode
 * OUTPUT codenode: max 1 OR, max 1 layer of ands, Literals
 * USING  
 *********************************************************************/
void
dnf (CodeNode * codenode)
{
  CodeNode * i_code;
  begin_of_function_code("Ands below ors",codenode);
  switch (codenode->connective)
    {
    case AND:
      distribute_ands_ors(codenode);
      break;
    case OR:
      for (i_code=codenode->sons;NULL!=i_code;i_code=i_code->next)
	{
	  dnf(i_code);
	  /* ?c spaeter: wegmachen p or not p in tru, if irgendwo tru ret tru */
	}
      combine_ands_code(codenode);/* ors */
      break;
    case ATOM:
    case NOT:
      break;
    default:
      spec_error("Wrong node in dnf");
      printf("\nconnective: %d", codenode->connective);
    }
  end_of_function_code("Ands below ors",codenode);
}

/*********************************************************************
 * This is the crucial function of the (precondition) pdnf,
 *   it could be called quite often, and should therefore be fast 
 *   ( contrary to the other functions of this file )
 * INPUT  codenode
 * OUTPUT codenode: max 1 OR, max 1 layer of ands, Literals
 * USING  
 *********************************************************************/
void
cnf (CodeNode * codenode)
{
  CodeNode * i_code;
  begin_of_function_code("cnf",codenode);
  switch (codenode->connective)
    {
    case AND:
      distribute_ors_ands(codenode);
      break;
    case OR:
      for (i_code=codenode->sons;NULL!=i_code;i_code=i_code->next)
	{
	  cnf(i_code);
	  /* ?c spaeter: wegmachen p or not p in tru, if irgendwo tru ret tru */
	}
      combine_ands_code(codenode);/* ands */
      break;
    case ATOM:
    case NOT:
      break;
    default:
      spec_error("Wrong node in cnf");
      printf("\nconnective: %d", codenode->connective);
    }
  end_of_function_code("cnf",codenode);
}

			     
/*********************************************************************
 * INPUT  codenode: AND or OR node 
 * OUTPUT codenode: (same) AND/OR node with all nodes with the same connective 
 *   directly below combined to one single AND node
 * USING  
 *********************************************************************/
void
combine_ands_code ( CodeNode * codenode )
{
  CodeNode * i_code,*j_code,*this_code;
  Connective con=codenode->connective;
  
  begin_of_function_code("Combine ands",codenode);
  i_code=new_CodeNode(DUMMY);
  this_code=codenode->sons;
  i_code->next=this_code;
  codenode->sons=i_code;
  while (this_code !=NULL)
    {
      if (this_code->connective==con)
	{
	  combine_ands_code(this_code);
	  i_code->next=this_code->sons;
	  j_code=this_code->next;
	  free(this_code);
	  this_code=j_code;
	  while (NULL!=i_code->next)
	    {
	      i_code=i_code->next;
	    }
	}
      else /*  !=AND  while  */
	{
	  i_code->next=this_code;
	  i_code=this_code;
	  this_code=this_code->next;
	}
    } /*  while  */
  i_code=codenode->sons->next;
  free(codenode->sons);/* DUMMY */
  codenode->sons=i_code;
  end_of_function_code("Combine ands",codenode);
} 

			     
 
/******************** axioms  ***************************/
Bool
single_predicate_contained(PlNode * e,PlNode * p)
/* searches the first tree e for the predicate p*/
{
  PlNode * i_pl;
  switch (e->connective)
    {
    case AND:
    case OR:
    case WHEN:
      for (i_pl=e->sons;i_pl!=NULL;i_pl=i_pl->next)
	{
	  if (single_predicate_contained(i_pl,p))
	    {
	      return TRUE;
	    }
	}
      return FALSE;
    case ALL:
    case EX:
    case NOT:
      return single_predicate_contained(e->sons,p);
    case TRU:
      return FALSE;
    case ATOM:
      /* printf("? vergl%s%s\n",e->atom->item,p->atom->item); */
      if (SAME == strcmp(e->atom->item,p->atom->item))
	{
	  return TRUE;
	}
      return FALSE;
    default:
      spec_error("single_predicate_contained");
      return TRUE;
    }
  
}  

Bool
any_predicate_contained(PlNode * e,PlNode * p)
/* searches the latter tree p for any of the effects in the first tree e */
{
  PlNode * i_pl;
  switch (p->connective)
    {
    case AND:
    case OR:
    case WHEN:
      for (i_pl=p->sons;i_pl!=NULL;i_pl=i_pl->next)
	{
	  if (any_predicate_contained(e,i_pl))
	    {
	      return TRUE;
	    }
	}
      return FALSE;
    case ALL:
    case EX:
    case NOT:
      return any_predicate_contained(e,p->sons);
    case TRU:
      return FALSE;
    case ATOM:
  /* printf("? Ist im Effekt\n"); */
/*   print_plnode( e ,0); */
/*   print_plnode( p ,0); */
      if (single_predicate_contained(e,p))
	{
	  /* printf("TRUE"); */
	  return TRUE;
	} 
      /* printf("FALSE"); */
      return FALSE;
    default:
      spec_error("any_predicate_contained");
      return TRUE;
    }
  
}  

char *
get_type(char *var, PlNode * p)
{
  PlNode * i_pl;
  char *str;
  /* printf("looking for %s in :",var); */
/*   print_plnode(p,0); */
  switch (p->connective)
    {
    case AND:
    case OR:
    case WHEN:
      for (i_pl=p->sons;i_pl!=NULL;i_pl=i_pl->next)
	{
	  str=get_type(var,i_pl);
	  if (!(NULL==str))
	    {
	      return str;
	    }
	}
      return NULL;
    case ALL:
    case EX:
      if (SAME==strcmp(var,p->atom->item))
	{
	  return p->atom->next->item;
	}
      return get_type(var,p->sons);
    case NOT:
      return get_type(var,p->sons);
    case TRU:
    case ATOM:
      return NULL;
    default:
      spec_error("get_type");
      return NULL;
    }
}

void
axioms_to_operators(void)
{
PlNode * atom_pl, * i_pl, * j_pl, * new_pl;
PlOperator * op_list, * i_axiom, * i_plop, * tmp_op;
TokenList * i_tl;
FactList * i_fl;
char * str_type, * var_;
op_list=gloaded_ops;
for (i_axiom=gloaded_axioms;i_axiom!=NULL;  )
  {
    if (0 && gcmd_line.display_info>=15)
      { 
	fprintf(stdout, "\nipp: transforming axiom to operator: "); 
	print_tokenlist(i_axiom->name, " ");
	fprintf(stdout, "\n");
      }
    for (i_plop=op_list; i_plop!=NULL; i_plop = i_plop->next )
      {
	if (TRUE==any_predicate_contained(i_plop->effects,i_axiom->preconds))
	  {
	    printf("\nNOw\n");
	    print_plnode( i_plop->effects ,0);
	    printf("\nis contained in \n");
	    print_plnode( i_axiom->preconds ,0);
	    atom_pl=copy_pl_node(i_axiom->effects);
	    j_pl=new_pl_node(NOT);
	    j_pl->sons=atom_pl;
	    for (i_tl=atom_pl->atom->next;i_tl!=NULL;
		 i_tl = i_tl->next )
	      {
		var_=i_tl->item;
		/* free(i_tl->item); */
		i_tl->item=new_token(LOG_Q_NUMBER);
		sprintf(i_tl->item,"??%d",snr_quantifiers);
		i_pl=new_pl_node(ALL);
		i_pl->sons=j_pl;
		i_pl->atom=new_token_list();
		i_pl->atom->item=new_token(LOG_Q_NUMBER);
		sprintf(i_pl->atom->item,"??%d",snr_quantifiers);
		/* get type */
		str_type="";
		/* get type in var list */
		for (i_fl=i_axiom->params; i_fl!=NULL; i_fl = i_fl->next )
		  {
		    if (SAME==strcmp(i_fl->item->item,var_))
		      {
			str_type=i_fl->item->next->item;
		      }
		  }
		if (SAME==strcmp(str_type,""))
		  {
		    /* get type , search in quantifiers */
		    printf("scheisse");
		    exit(1);
		    str_type=get_type(var_,i_plop->effects);
		  }
		if (NULL==str_type)
		  {
		    printf("scheisse");
		    exit(1);
		  }
		printf("%s",str_type);
		i_pl->atom->next=new_token_list();
		i_pl->atom->next->item=(char * )calloc(strlen(str_type)+1,sizeof(char));
		sprintf(i_pl->atom->next->item,"%s",str_type);
		
		snr_quantifiers++;
		j_pl=i_pl;
	      }
	    /* add this list */
	    new_pl=new_pl_node(AND);
	    new_pl->sons=i_plop->effects;
	    new_pl->sons->next=j_pl;
	    i_plop->effects=new_pl;
	  }
      }
    /* add axiom to op_list */
    tmp_op = i_axiom->next;
    i_axiom->next=op_list;
    op_list=i_axiom;
    i_axiom=tmp_op;
    gloaded_ops=op_list;
    if (0 && gcmd_line.display_info>=15)
      { 
	fprintf(stdout, "\n... done\n");
      }
  }
}

/******************** main  ***************************/
/*********************************************************************
 * Main function of the pddl-tree simplification
 * USING  
 *********************************************************************/
void 
pddl_outer(void)
{
  PlOperator * i_plop;
  PlNode *i_pl;
  /************* Operator output ************/
  if (0 && gcmd_line.display_info>=15)
    { 
      fprintf(stdout,"\n----------1--------------------------------\n");
    fprintf(stdout, "\nLOADED OPERATORS:\n");
      print_plops(gloaded_ops);
      fprintf(stdout, "\nLOADED AXIOMS:\n");
      print_plops(gloaded_axioms);
      fprintf(stdout,"\n------------------------------------------\n");
      fprintf(stdout,"\nipp: merging axioms into operators...\n");
    }
/*       purify_new_leaks(); */
/*       purify_printf("1 "); */
  /************* axiom quantifier renaming ********/
  snr_quantifiers=1;
  for ( i_plop = gloaded_axioms; i_plop!=NULL; i_plop = i_plop->next )
    {
      if (0 && gcmd_line.display_info>=15)
	{ 
	  fprintf(stdout,"\nOpName: %s",i_plop->name->item);
	  fprintf(stdout,"\npreconditions:\n");
	}
      if (NULL!=i_plop->preconds)
	{
	  rename_all_quantifiers(i_plop->preconds);/* ***   */
	}
    }
  for ( i_plop = gloaded_ops; i_plop!=NULL; i_plop = i_plop->next )
    {
      if (0 && gcmd_line.display_info>=15)
	{ 
	  fprintf(stdout,"\nOpName: %s",i_plop->name->item);
	  fprintf(stdout,"\npreconditions:\n");
	}
      if (NULL!=i_plop->preconds)
	{
	  rename_all_quantifiers(i_plop->preconds);/* ***   */
	}

      if (0 && gcmd_line.display_info>=15)
	{ 
	  fprintf(stdout,"\neffects:\n");
	}
      if (NULL!=i_plop->effects)
	{
	  rename_all_quantifiers(i_plop->effects);/* ***   */
	}
    }
  /************* Operator output ************/
  if (0 && gcmd_line.display_info>=5)
    { 
      fprintf(stdout,"\n-----------2-------------------------------\n");
    fprintf(stdout, "\nLOADED OPERATORS:\n");
      print_plops(gloaded_ops);
      fprintf(stdout, "\nLOADED AXIOMS:\n");
      print_plops(gloaded_axioms);
      fprintf(stdout,"\n------------------------------------------\n");
      fprintf(stdout,"\nipp: merging axioms into operators...\n");
    }
  /************* axioms_to_operators ************/
  axioms_to_operators();/* ***   */
  /************* Operator output ************/
  if (0 && gcmd_line.display_info>=5)
    { 
      fprintf(stdout,"...done\n");
      fprintf(stdout,"\n------------3------------------------------\n");
      fprintf(stdout, "\nOPERATORS BEFORE pddl_outer:\n");
      print_plops(gloaded_ops);
      fprintf(stdout,"\n------------------------------------------\n");
      fprintf(stdout,"\nipp: simplifying operators (pddl_outer)...\n");
    }
  /************* melting multiple whens etc. ********/
  for ( i_plop = gloaded_ops; i_plop!=NULL; i_plop = i_plop->next )
    {
      if (0 && gcmd_line.display_info>=15)
	{ 
	  fprintf(stdout,"\nOpName: %s",i_plop->name->item);
	  fprintf(stdout,"\npreconditions:\n");
	}
      if (NULL!=i_plop->preconds)
	{
	  rename_all_quantifiers(i_plop->preconds);/* ***   */
	}

      if (0 && gcmd_line.display_info>=15)
	{ 
	  fprintf(stdout,"\neffects:\n");
	}
      if (NULL!=i_plop->effects)
	{
	  ands_to_top(i_plop->effects);/* ***   */
	}
    }
  if (0 && gcmd_line.display_info>=5)
    { 
      fprintf(stdout,"\n------------4------------------------------\n");
    }
  for ( i_plop = gloaded_ops; i_plop!=NULL; i_plop = i_plop->next )
    {
      /************* quantifier movement of operators' precondition ********/
      if (0 && gcmd_line.display_info>=15)
	{ 
	  fprintf(stdout,"\nOpName: %s",i_plop->name->item);
	  fprintf(stdout,"\npreconditions:\n");
	}
      if (NULL!=i_plop->preconds)
	{
	  nots_down(i_plop->preconds);/* ***   */
	}
      /********* quantifier movement of ************/
      /********(both conditional and unconditional) effects' precondition ***/
      if (0 && gcmd_line.display_info>=15)
	{ 
	  fprintf(stdout,"\neffects:\n");
	}
      if (NULL!=i_plop->effects)
	{
	  if (AND!=i_plop->effects->connective) 
	    {  
	      spec_error("No AND node");
	    }
	  for(i_pl=i_plop->effects->sons;NULL!=i_pl;i_pl=i_pl->next)
	    {
	      if (WHEN!=i_pl->connective) 
		{  		  
		  /* step through quantifiers */
		  while (ALL==i_pl->connective )
		    {  		  
		      i_pl=i_pl->sons;
		    }
		}
	      if (WHEN!=i_pl->connective) 
		{  		  
		  spec_error("No WHEN node");
		}
	      nots_down(i_pl->sons);/* ***   */ 
	    }
	}
    }
  /************* quantifier movement of goals ********/
  if (NULL!=gorig_goal_facts)
	{
	  nots_down(gorig_goal_facts);/* ***   */
	}
  /************* Operator output ************/
  if (0 && gcmd_line.display_info>=5)
    { 
      fprintf(stdout,"...done\n");
      fprintf(stdout,"\n-----------------5------------------------\n");
      fprintf(stdout, "\nOPERATORS AFTER pddl_outer(pl):\n");
      print_plops(gloaded_ops);
      fprintf(stdout,"\n------------------------------------------\n");
      fprintf(stdout,"\nipp: transformation to code_nodes...\n");
    }
}

void
pddl_inner(void)
{
  CodeOperator * i_codeop;
  CodeNode * i_code,*j_code;
  /************* Operator output ************/
  if (0 && gcmd_line.display_info>=15)
    { 
      fprintf(stdout,"\n...done\n");
      fprintf(stdout,"\n------------------6-----------------------\n");
      fprintf(stdout, "\nOPERATOREN BEFORE pddl_inner(code):\n"); 
      for ( i_codeop =gcode_operators; i_codeop!=NULL; i_codeop = i_codeop->next ) 
	{
	  print_CodeOperator(i_codeop);
	}
      fprintf(stdout,"\n-----------------------------------------\n");
    }
  for ( i_codeop = gcode_operators; i_codeop!=NULL; i_codeop = i_codeop->next )
    {
      /************* dnf of operators' precondition ************/
      if (0 && gcmd_line.display_info>=15)
	{ 
	  fprintf(stdout,"\nOpName: %s",i_codeop->name);
	  fprintf(stdout,"\npreconditions:\n");
	}
      if (NULL!=i_codeop->preconds)
	{
	  if (TRU!=i_codeop->preconds->connective)
	    {
	      /* step through quantifiers */
	      i_code=i_codeop->preconds;
	      while ((ALL==i_code->connective)||
		     (EX==i_code->connective))
		{  		  
		  i_code=i_code->sons;
		}
	      dnf(i_code);/* ***   */
	    }
	}
      /************* dnf of conditional effects' precondition ************/
      if (0 && gcmd_line.display_info>=15)
	{ 
	  fprintf(stdout,"\nconditional effects:\n");
	}
      if (NULL!=i_codeop->conditionals)
	{
	  if (AND!=i_codeop->conditionals->connective) 
	    {  
	      spec_error("No AND node");
	    }
	  for(i_code=i_codeop->conditionals->sons;NULL!=i_code;i_code=i_code->next)
	    {
	      j_code=i_code;
	      if (WHEN!=j_code->connective)
		{  		  
		  /* step through quantifiers */
		  while ((ALL==j_code->connective)||(EX==j_code->connective))
		    {  		  
		      j_code=j_code->sons;
		    }
		}
	      if (WHEN!=j_code->connective) 
		{  		  
		  spec_error("No WHEN node");
		}
	      if (TRU!=j_code->sons->connective)
		{
		  /* step through quantifiers */
		  while ((ALL==j_code->sons->connective)||
			 (EX==j_code->sons->connective))
		    {  		  
		      j_code=j_code->sons;
		    }
		  dnf(j_code->sons);/* ***   */ 
		}
	    }
	}
    }
  /************* output of goals ************/
  if (0 && gcmd_line.display_info>=15)
    { 
      fprintf(stdout,"\n-----------------------------------------\n");
      fprintf(stdout, "\nGOALS:\n"); 
      print_CodeNode(gcode_goal_state,0);
      fprintf(stdout,"\n-----------------------------------------\n");
    }
  /************* dnf of goals ************/
  if (0 && gcmd_line.display_info>=15)
    { 
      fprintf(stdout,"\ngoals:\n");
    }
  j_code=gcode_goal_state;
  /* step through quantifiers */
  if (NULL!=j_code->sons)
    {
      while ((ALL==j_code->sons->connective)||
	     (EX==j_code->sons->connective))
	{  		  
	  j_code=j_code->sons;
	}
    dnf(j_code->sons);/* ***   */ 
  }
  else
    dnf(j_code);/* ***   */ 
  /************* Operator output ************/
  if (0 && gcmd_line.display_info>=15)
    { 
      fprintf(stdout,"\n...done\n");
      fprintf(stdout,"\n------------------7----------------------\n");
      fprintf(stdout, "\nOPERATOREN AFTER ALL:\n"); 
      for ( i_codeop =gcode_operators; i_codeop!=NULL; i_codeop = i_codeop->next ) 
	{
	  print_CodeOperator(i_codeop);
	}
      fprintf(stdout,"\n-----------------------------------------\n");
    }
}



