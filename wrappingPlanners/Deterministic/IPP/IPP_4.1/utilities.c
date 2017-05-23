



/*********************************************************************
 * File: utilities.c
 * Description: 
 *
 * Author: Joerg Hoffmann / Frank Rittinger
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





/**********************************************************************
 * Make a true copy of string.
 *
 * char * s: The original string.
 * RETURNS the memory location of the new string.
 *********************************************************************/
char * 
copy_string(char * s)
{
  char * d = new_token( strlen( s ) + 1 );
  strcpy(d, s);
  return d;
}

/**********************************************************************
 * Copy a TokenList to a newly created TokenList, makes a true copy.
 *********************************************************************/
TokenList *
copy_complete_token_list(TokenList * source, TokenList ** end)
{
  TokenList * temp;

  if ( !source )
    {
      temp = NULL;
    }
  else
    {
      temp = new_token_list();
      if ( source->item )
	{
	  temp->item = new_token( strlen( source->item ) + 1 );
	  strcpy( temp->item, source->item );
	}
      temp->next = copy_complete_token_list( source->next, end );
      if ( !temp->next )
	*end = temp;
    }
  return temp;
}

/**********************************************************************
 * Checks whether a string is contained in a token list.
 * 
 * char * s: The string we search.
 * TokenList * tl: The token list in which we search.
 *
 * RETURNS the position of the element in the list, starting with 1
 *         or 0 if s is not contained in the list.
 *********************************************************************/
int 
is_tl_element(char * s, TokenList * tl)
{
  TokenList * tmp;
  int i = 0;

  for (tmp = tl; tmp; tmp = tmp->next)
    {
      i++;
      if ((tmp->item == s) || (SAME == strcmp(tmp->item, s)))
	{
	  return i;
	}
    }
  return 0;
}

/**********************************************************************
 * Copy a string to its all upcase aquivalent. 
 * Makes a true copy.
 **********************************************************************/
void
strupcase(char * from)
{
  char tmp;

  tmp = *from;
  while ('\0' != tmp)
    {
      *from = (char) toupper((int) tmp);
      tmp = *++from;
    }
}

/**********************************************************************
 * Jump '-', ' ', '\t' in input parsing and return only the strings
 * without any leading symbols.
 *********************************************************************/
char * 
rmdash( char* s )
{
  s++; /* first letter is '-', skip it! */
  for( ; (*s == ' ') || (*s == '\t'); s++ )
    ;
  return s;
}

/**********************************************************************
 * Compare two TokenLists whether they are equal (the strings are 
 * pairwise equal), i.e two strings point to the same memory location 
 * or two strings are compared equal by strcmp().
 * 
 * TokenList * fst:
 * TokenList * snd:
 *
 * RETURNS TRUE iff all strings are pairwise equal.
 *********************************************************************/
Bool
token_lists_equal(TokenList * fst, TokenList * snd)
{
  while (fst && snd)
    {
      if ((fst->item == snd->item) 
	  || (SAME == strcmp(fst->item, snd->item)))
	{
	  fst = fst->next;
	  snd = snd->next;
	}
      else
	{
	  return FALSE;
	}
    }
  /* If one TokenList has more elements than the other, return FALSE */
  return (!(fst || snd));
}

/*********************************************************************
 * Copy a TokenList, but do not copy predicates and constants, only
 * make deep copies of variables, i.e. strings that start with a '?'.
 * NOTE The order of the tokenlist is preserved.
 *
 * TokenList * in_tl: The TokenList that is copied.
 *
 * RETURNS a deep copy of all variables (starting with a '?') and
 *         a shallow copy of all predicates and constants.
 ********************************************************************/
TokenList * 
copy_token_list(TokenList * in_tl)
{
    TokenList * tl;
    TokenList * tl_ret;
    TokenList * tl_tmp;
    
    /* This is a dummy node at the beginning of the list. */
    tl_ret = new_token_list();
    tl_tmp = tl_ret;
    
    for (tl = in_tl; tl; tl = tl->next) {
	tl_ret->next = new_token_list();
	/* Make deep copies of variables. */
	if ('?' == *(tl->item)) {
	    register int l = strlen(tl->item);
	    tl_ret->next->item = new_token(1 + l);
	    strncpy(tl_ret->next->item, tl->item, l);
	} else {
	    /* Make shallow copies of predicates and constants. */
	    tl_ret->next->item = tl->item;
	}
	/* Do not change the order of the tokens! */
	tl_ret = tl_ret->next;
    }
    
    /* The first node was a dummy, remove it. */
    tl = tl_tmp->next;
    free(tl_tmp);
    
    return tl;
}

/*********************************************************************
 * Copy a FactList, but do not copy predicates and constants.
 * NOTE The order of the tokenlist is preserved.
 *
 * FactList * in_fl: The FactList that is copied.
 *
 * RETURNS a deep copy of all variables (starting with a '?') and
 *         a shallow copy of all predicates and constants.
 ********************************************************************/
FactList * 
copy_fact_list(FactList * in_fl)
{
    FactList * fl_ret;
    FactList * fl_tmp;
    FactList * fl;
    
    /* This is a dummy at the beginning of the list. */
    fl_ret = new_fact_list();
    fl_tmp = fl_ret;
    
    for (fl = in_fl; fl; fl = fl->next) {
	fl_ret->next = new_fact_list();
	fl_ret->next->item = copy_token_list(fl->item);
	fl_ret = fl_ret->next;
    }

    /* The first node was a dummy, remove it. */
    fl_ret = fl_tmp->next;
    free(fl_tmp);
    
    return fl_ret;
}


/**********************************************************************
 * 
 *********************************************************************/
Bool 
tl_is_fl_element(TokenList * tl, FactList * fl)
{
    FactList * tmp;

    for (tmp = fl; tmp; tmp = tmp->next) {
	if (token_lists_equal(tl, tmp->item)) {
	    return TRUE;
	}
    }
    return FALSE;
}

/*********************************************************************
 * Make a copy of the node.
 *
 * NOTE Shallow copy of predicates.                  NOTE
 * NOTE NO copy at all of the next and sons pointer. NOTE
 *
 * PlNode * node: The node that will be copied.
 *
 * RETURNS a real copy of the input node (son copied, next NOT copied).
 *********************************************************************/
PlNode * 
copy_pl_node(PlNode * node)
{
    PlNode * tmp;
    
    if (NULL == node) {
	return NULL;
    }
    
    tmp = new_pl_node(node->connective);

    if (NULL != node->atom) {
	tmp->atom = copy_token_list(node->atom);
    }
    
    return tmp;
}




void copy_contents_of_CodeNode( CodeNode **dest, CodeNode *source ) 

{

  int i;

  if ( !source ) {
    (*dest) = NULL;
    return;
  }

  (*dest)->connective = source->connective;
  (*dest)->var = source->var;
  (*dest)->var_type = source->var_type;
  (*dest)->predicate = source->predicate;
  for ( i=0; i<MAX_ARITY; i++ ) (*dest)->arguments[i] = source->arguments[i];

}


CodeNode *copy_CodeNode( CodeNode *source ) 

{

  CodeNode *r = new_CodeNode( source->connective );
  int i;

  r->var = source->var;
  r->var_type = source->var_type;
  r->predicate = source->predicate;
  for ( i=0; i<MAX_ARITY; i++ ) r->arguments[i] = source->arguments[i];
  
  r->next = source->next;
  r->sons = source->sons;

  return r;

}


CodeNode * 
deep_copy_CodeTree(CodeNode * node)
{
    /* helpers */
    CodeNode * tmp = NULL;
    if (node==NULL) {
      return NULL;
    }
    
    tmp = copy_CodeNode(node);
    if (NULL != tmp) {
	tmp->next = deep_copy_CodeTree(node->next);
	tmp->sons = deep_copy_CodeTree(node->sons);
    }
    return tmp;
}


/*********************************************************************
 * Make a deep copy of the tree below node.
 *
 * NOTE Shallow copies of predicates and constants!
 *
 * PlNode * node: The root of the tree that will be copied.
 *
 * RETURNS a real copy of the input tree.
 *********************************************************************/
PlNode * 
deep_copy_tree(PlNode * node)
{
    /* helpers */
    PlNode * tmp = NULL;
    
    tmp = copy_pl_node(node);
    if (NULL != tmp) {
	tmp->next = deep_copy_tree(node->next);
	tmp->sons = deep_copy_tree(node->sons);
    }
    return tmp;
}

/**********************************************************************
 * Make a deep copy of the operator.
 *
 * SHALLOW: all constants and predicate names
 * DEEP: everything else, in particular all structures
 *
 * PlOperator * op: The old operator.
 *
 * RETURNS the new operator, a true copy.
 *********************************************************************/
PlOperator * 
copy_operator(PlOperator * op)
{
    PlOperator * new_op;

    new_op = new_pl_operator(NULL);
    /* Make a true copy of the structures, but not the strings. 
       CAUTION The order must be conserved. */
    new_op->name = copy_token_list(op->name);
    
    new_op->params = copy_fact_list(op->params);
    new_op->preconds = deep_copy_tree(op->preconds);
    new_op->effects = deep_copy_tree(op->effects);
    new_op->number_of_real_params = op->number_of_real_params;
    
    return new_op;
}




/**********************************************************************
 * Make a copy of a list of operator.
 *
 * SHALLOW: operator name, all constants and predicate names
 * DEEP: everything else, in particular all structures
 *
 * PlOperator * old_op: The old operator list.
 *
 * RETURNS the new list of operator, true copies.
 *********************************************************************/
PlOperator * 
copy_operator_list(PlOperator * old_op)
{
    PlOperator * new_op = NULL;
    PlOperator * tmp_op;
    PlOperator * opl;
    
    for (opl = old_op; opl; opl = opl->next) {
	tmp_op = copy_operator(opl);
	/* append to new list */
	tmp_op->next = new_op;
	new_op = tmp_op;
    }
    
    return new_op;
}

/*
 * Remove operators that have empty effects.
 */
PlOperator * 
remove_empty_ops(PlOperator * ops)
{
  PlOperator * next;

  if (NULL == ops) {
    return NULL;
  }

  next = remove_empty_ops(ops->next);

  if (NULL == ops->effects) {
    /* This operator can be removed. */
    ops->next = NULL;
    free_pl_op(ops);
    return next;
  } else if (AND == ops->effects->connective && NULL == ops->effects->sons) {
    ops->next = NULL;
    free_pl_op(ops);
    return next;
  } else {
    ops->next = next;
    return ops;
  }
  return NULL;
}



Effect *copy_effects( Effect *e )

{

  Effect *r;

  if ( !e ) {
    return NULL;
  }

  r = new_Effect();
  r->next = copy_effects( e->next );

  copy_contents_of_FactInfo( &(r->p_conds), e->p_conds );
  copy_contents_of_FactInfo( &(r->n_conds), e->n_conds );
  copy_contents_of_FactInfo( &(r->p_effects), e->p_effects );
  copy_contents_of_FactInfo( &(r->n_effects), e->n_effects );

  return r;

}



void copy_contents_of_FactInfo( FactInfo **dst, FactInfo *src )

{

  int i;

  for ( i=0; i<gft_vector_length; i++ ) {
    (*dst)->vector[i] = src->vector[i];
  }

  (*dst)->indices = copy_Integers( src->indices );

}


Integers *copy_Integers( Integers *i )

{

  Integers *r;

  if ( !i ) {
    return NULL;
  }

  r = new_integers( i->index );
  r->next = copy_Integers( i->next );

  return r;

}



BitVector *copy_bit_vector( BitVector *vec, int vec_len )

{

  BitVector * result = new_bit_vector(vec_len);
  memcpy(result, vec, vec_len * sizeof(int));

  return result;

}
