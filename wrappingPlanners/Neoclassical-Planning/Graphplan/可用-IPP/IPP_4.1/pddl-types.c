



/*********************************************************************
 * File: pddl-types.c
 * Description: Functions for the pddl dependend part of
 *              the parser and the preprocessing.
 *
 * Author: Frank Rittinger 1998
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



#include "ipp.h"
#include "pddl.h"
#include "utilities.h"
#include "memory.h"




TokenList *
type_already_known(char * name, FactList * types)
{
  FactList * f;

  for ( f = types; f; f = f->next )
    if ( strcmp( f->item->item, name ) == SAME )
      return f->item->next;
  return NULL;
}

type_tree 
main_type_tree()
{
  type_tree_list ttl;

  for ( ttl = gglobal_type_tree_list; ttl; ttl = ttl->next )
    if ( strcmp( ttl->item->name, STANDARD_TYPE ) == SAME )
      return ttl->item;
  return NULL;
}

type_tree 
new_type_tree(char * name)
{
  type_tree act_type;
  
  if (!name)
    return NULL;
  act_type = ( type_tree ) calloc( 1, sizeof( type_tree_elt ) );
  CHECK_PTR(act_type);
  act_type->name = new_token( strlen( name ) + 1 );
  strcpy( act_type->name, name );
  act_type->sub_types = NULL;
  return act_type;
}

type_tree_list 
new_type_tree_list(char * name)
{
  type_tree_list act_type_list;
  
  act_type_list = ( type_tree_list ) calloc( 1, sizeof( type_tree_list_elt ) );
  CHECK_PTR(act_type_list);
  if ( name )
    act_type_list->item = new_type_tree( name );
  else
    act_type_list->item = NULL;
  act_type_list->next = NULL;
  
  return act_type_list;
}

/* steps recursively through type tree and searches for name */
type_tree 
find_branch(char * name, type_tree root )
{
  type_tree p;
  type_tree_list ttl;

  if ( !root )
    return NULL;
  if ( strcmp( root->name, name ) == SAME )
    return root;
  if ( !root->sub_types )
    return NULL;
  for ( ttl=root->sub_types; ttl; ttl=ttl->next )
    if ((p = find_branch( name, ttl->item )))
      return p;
  return NULL; 
}

/* calls itself recursively to get all objects that are of the types and
   subtypes of ttl */
FactList *
build_object_list_from_ttl(type_tree_list ttl, FactList * types_done )
{
  FactList * f;
  FactList * td;
  FactList * std;
  TokenList * t;
  TokenList * tl_dummy;
  TokenList * st;
  type_tree_list sttl;

  if ( !ttl )
    return types_done;
  
  types_done = build_object_list_from_ttl( ttl->next, types_done );

  if ((t = type_already_known( ttl->item->name, types_done)))
    return types_done;

  td = new_fact_list();
  t = td->item = new_token_list();
  /* begin with the name of the type... */
  t->item = copy_string( ttl->item->name );
  for ( f = gorig_constant_list; f; f = f->next )
    {
      /* ...followed by objects of that type. */
      if ( strcmp( f->item->next->item, ttl->item->name ) == SAME )
	{
	  t->next = new_token_list();
	  t = t->next;
	  t->item = copy_string( f->item->item );
	}
    }
  /* now append the objects of the subtypes */
  std =  build_object_list_from_ttl( ttl->item->sub_types, types_done );
  /* now we can be sure that for each subtype a list with all
     objects of that type is somewhere in std. We simply take these
     lists and copy all of them into a new one for the supertype */
  for ( sttl = ttl->item->sub_types; sttl; sttl = sttl->next )
    {
      st = type_already_known( sttl->item->name, std );
      if ( st )
	{
	  t->next = copy_complete_token_list( st, &tl_dummy );
	  t = tl_dummy;
	}
    }
  td->next = std;
  return td;
}

void 
build_orig_constant_list()
{
  FactList * f;
  FactList * nextf;
  FactList * end = NULL;
  TokenList * t;
  Bool do_count;
  int objects_count = 0;

  gtypes = build_object_list_from_ttl( gglobal_type_tree_list, 
						 NULL );
  free_complete_fact_list( gorig_constant_list );
  gorig_constant_list = NULL;
  for ( f = gtypes; f; f = nextf )
    {
      nextf = f->next;
      if ( strcmp( f->item->item, STANDARD_TYPE ) == SAME )
	do_count = TRUE;
      else
	do_count = FALSE;
      for ( t = f->item->next; t; t = t->next )
	{
	  if ( !gorig_constant_list )
	    gorig_constant_list = end = new_fact_list();
	  else
	    {
	      end->next = new_fact_list();
	      end = end->next;
	    }
	  end->item = new_token_list();
	  end->item->item = copy_string( t->item );
	  end->item->next = new_token_list();
	  end->item->next->item = copy_string( f->item->item );
	  if ( do_count )
	    {
	      /* count objects for RIFO meta strategy */
	      objects_count++;
	    }
	}
    }
}

void
add_to_type_tree(FactList * t_list, type_tree tree )
{
  type_tree branch = tree;
  type_tree_list new_branch;
  char * this_type; 
  char * super_type;

  /* step through list and build a hierarchy of types */
  for( ; t_list; t_list=t_list->next )
    {
      this_type = t_list->item->item;
      if ( !t_list->item->next ) 
	{
	  fprintf(stderr, "\n%s: error at '%s'.\n", gact_filename, this_type );
	  exit( PARSE_ERROR_CODE );
	}
      super_type = t_list->item->next->item;
      if ( strcmp( branch->name, super_type ) != SAME )
	branch = find_branch( super_type, tree );
      if ( !branch ) 
	{
	  fprintf(stderr, "\n%s: unknown type '%s'.\n", 
		  gact_filename, super_type );
	  exit( PARSE_ERROR_CODE );
	}
      /* now the type is a subtype of the one currently looked at 
	 in the type tree */
      new_branch = new_type_tree_list( this_type );
      new_branch->next = branch->sub_types;
      branch->sub_types = new_branch;
    }
}

void 
print_type_tree_list( type_tree_list root, int indent )
{
  int i;
  for(i=0;i<2*indent; i++ ) 
    {
      fprintf(stdout, " " );
    }
  if ( root ) 
    {
      if ( !root->item || !root->item->name )
	{
	  fprintf(stderr, "\n%s: internal error: no type name specified.\n", 
		  gact_filename );
	  exit( INTERNAL_ERROR_CODE );
	}
      else
	fprintf(stdout, "%s\n", root->item->name );
      if ( root->item->sub_types )
	print_type_tree_list( root->item->sub_types, indent+1 );
      if ( root->next )
	print_type_tree_list( root->next, indent );
    }
}

/********************************************************************* 
 * Insert this subtree into the preconditions. Take care whether there
 * are 0, 1 or >1 preconditions and 0, 1, >1 eq-predicates. Prepend a 
 * AND node if necessary.
 *
 * PlOperator * op: The current operator. 
 * PlNode * root: The root of the new subtree that is inserted.
 *
 *********************************************************************/
void 
insert_not_eq(PlOperator * op, PlNode * root)
{
    PlNode * tmp;

    if (NULL == op->preconds) {
	op->preconds = root;
    } else {
	if (AND == op->preconds->connective) {
	    /* There is already a AND node, simply append the tree. */
	    tmp = op->preconds->sons;
	    op->preconds->sons = root;
	    root->next = tmp;
	} else {
	    /* There is no AND. First create a new AND, prepend it to
	       the preconds and append the subtree. */
	    tmp = new_pl_node(AND);
	    tmp->sons = root;
	    root->next = op->preconds;
	    op->preconds = tmp;
	}
    }
}

/*********************************************************************
 * This function goes over all operators and inserts a "not="
 * predicate for every two parameters in one operator that have the
 * same types.
 *
 * PlOperator * ops: The global operator list.
 *
 *********************************************************************/
void 
adjust_equal_types(PlOperator * ops)
{
    PlOperator * tmp_op;
    FactList * tmp_out;
    FactList * tmp_in;
    PlNode * tmp_node;

    for (tmp_op = ops; tmp_op; tmp_op = tmp_op->next) {
	for (tmp_out = tmp_op->params; tmp_out; tmp_out = tmp_out->next) {
	    for (tmp_in = tmp_out->next; tmp_in; tmp_in = tmp_in->next) {
		/* If the types of two parameters are the same, 
		   create a new "not=" predicate and insert it
		   appropriately (AND or no AND). */
		if (tmp_out->item->next->item ==
		    tmp_in->item->next->item || SAME ==
		    strcmp(tmp_out->item->next->item,
			   tmp_in->item->next->item)) {
#ifdef MYDEBUG
		    fprintf(stdout, "\n>>> OP: %s \nSAME: %s == %s",
			    tmp_op->name->item, tmp_out->item->item,
			    tmp_in->item->item); 
#endif		    
		    /* Create the new subtree for the "not=". */
		    tmp_node = new_pl_node(NOT);
		    tmp_node->sons = new_pl_node(ATOM);
		    tmp_node->sons->atom = new_token_list();
		    tmp_node->sons->atom->item = EQ_STR;
		    tmp_node->sons->atom->next = new_token_list();
		    tmp_node->sons->atom->next->item =
			new_token(strlen(tmp_out->item->item) + 1);
		    strcpy(tmp_node->sons->atom->next->item,
			   tmp_out->item->item);
		    tmp_node->sons->atom->next->next = new_token_list();
		    tmp_node->sons->atom->next->next->item =
			new_token(strlen(tmp_in->item->item) + 1);
		    strcpy(tmp_node->sons->atom->next->next->item,
			   tmp_in->item->item);
#ifdef MYDEBUG
		    fprintf(stdout, " --> Creating new negated Atom: ");
		    print_tokenlist(tmp_node->sons->atom, " ");
#endif
		    /* Insert this subtree into the preconditions.
		       Take care whether there are 0, 1 or >1
		       preconditions and 0, 1, >1
		       eq-predicates. Prepend a AND node if
		       necessary. */
		    insert_not_eq(tmp_op, tmp_node);
		}
	    }
	}
    }
}

