



/*********************************************************************
 * File: output.c
 * Description: Functions for printing the data structures or 
 *              other general output.
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
#include "output.h"
#include "utilities.h"






void print_factlist( FactList *list, char *sepf, char* sept )

{
    FactList * i_list;
    TokenList * i_tl;
    
    if (NULL != list) {
	/* Extra handling of first element for pretty printing! */
	i_tl = list->item;
	if (NULL == i_tl || NULL == i_tl->item) {
	    fprintf(stdout, "empty");
	} else {
	    fprintf(stdout, "%s", i_tl->item);
	    i_tl = i_tl->next;
	}
	
	while (NULL != i_tl) {
	    if (NULL != i_tl->item) {
		fprintf(stdout, "%s%s", sept, i_tl->item);
	    }
	    i_tl = i_tl->next;
	}
	
	/* Print the rest. */
	for ( i_list = list->next; i_list; i_list = i_list->next ) {
	    fprintf(stdout, "%s", sepf);
	    i_tl = i_list->item;
	    if (NULL == i_tl || NULL == i_tl->item) {
		fprintf(stdout, "empty");
	    } else {
		fprintf(stdout, "%s", i_tl->item);
		i_tl = i_tl->next;
	    }
	    
	    while (NULL != i_tl) {
		if (NULL != i_tl->item) {
		    fprintf(stdout, "%s%s", sept, i_tl->item);
		}
		i_tl = i_tl->next;
	    }
	}
    }
}

/**********************************************************************
 * Prints a TokenList to stdout. But does not print tokens
 * that start with a '#'.
 *
 * TokenList * list: The TokenList to print.
 * char * sep: A separator for the different TokenLists.
 *********************************************************************/
void
print_tokenlist(TokenList * list, char * sep)
{
    TokenList * i_tl;
    
    /* this construction makes the print out nicer, since
       there is no sep at before the first item and after the
       last item */
    i_tl = list;
    if (NULL!=i_tl) {
	/* Only print items that do do not start with '#'. */
	if ('#' != *i_tl->item) {
	    fprintf(stdout, "%s", i_tl->item);
	}
	i_tl = i_tl->next;
    } else {
	fprintf(stdout,"empty");
    }
  
    while (NULL != i_tl) {
	/* Only print items that do do not start with '#'. */
	if ('#' != *i_tl->item) {
	    fprintf(stdout, "%s%s", sep, i_tl->item);
	}
	i_tl = i_tl->next;
    }
}


/**********************************************************************
 * Prints a TokenList to stdout.
 *
 * TokenList * list: The TokenList to print.
 * char * sep: A separator for the different TokenLists.
 *********************************************************************/
void
print_hidden_tokenlist(TokenList * list, char * sep)
{
    TokenList * i_tl;
    
    /* this construction makes the print out nicer, since
       there is no sep at before the first item and after the
       last item */
    i_tl = list;
    if (NULL!=i_tl) {
	/* Only print items that do do not start with '#'. */
	fprintf(stdout, "%s", i_tl->item);
	i_tl = i_tl->next;
    } else {
	fprintf(stdout,"empty");
    }
    
    while (NULL != i_tl) {
	/* Only print items that do do not start with '#'. */
	fprintf(stdout, "%s%s", sep, i_tl->item);
	i_tl = i_tl->next;
    }

}

/**********************************************************************
 *
 *********************************************************************/
void
print_indent(int indent)
{
  int i;
  for (i=0;i<indent;i++)
    {
      fprintf(stdout," ");
    }
}

/****** TEMPORAL FUNCTION ******/
void
spec_error(char * s)
{
  fprintf(stdout, "\nSpecification error at: %s\n", s);
  exit(EXIT_FAILURE);
}

/**********************************************************************
 * at function call the cursor is meant to be intended
 * at function exit at the beginning of a new line
 **********************************************************************/
void
print_plnode(PlNode * plnode, int indent)
{
  PlNode * i_son;

  if (NULL == plnode)
    {
      fprintf(stdout, "none\n");
      return;
    }
  
  switch (plnode->connective)
    {
    case ALL: 
      fprintf(stdout, "ALL %s : %s\n", plnode->atom->item,
	      plnode->atom->next->item);
      print_indent(indent);
      fprintf(stdout,"(   ");
      print_plnode(plnode->sons,indent+4);
      if (NULL!=plnode->sons->next) spec_error("ALL");
      print_indent(indent);
      fprintf(stdout,")\n");
      break;
    case EX:
      fprintf(stdout, "EX  %s : %s\n", plnode->atom->item,
	      plnode->atom->next->item);
      print_indent(indent);
      fprintf(stdout,"(   ");
      print_plnode(plnode->sons,indent+4);
      if (NULL!=plnode->sons->next) spec_error("EX");
      print_indent(indent);
      fprintf(stdout,")\n");
      break;
    case AND: 
      fprintf(stdout,"(   ");
      print_plnode(plnode->sons, indent+4);
      if (NULL != plnode->sons) {
	  for (i_son = plnode->sons->next; i_son!=NULL; i_son = i_son->next)
	    {
		print_indent(indent);
		fprintf(stdout,"AND ");
		print_plnode(i_son,indent+4);
	    }
      }
      print_indent(indent);      
      fprintf(stdout,")\n");
      break;
    case OR:  
      fprintf(stdout,"(   "); 
      print_plnode(plnode->sons, indent+4);
      for (i_son = plnode->sons->next; i_son!=NULL; i_son = i_son->next)
	{
	  print_indent(indent);
	  fprintf(stdout,"OR ");
	  print_plnode(i_son,indent+4);
	}
      print_indent(indent);      
      fprintf(stdout,")\n");
      break;
    case WHEN:
      fprintf(stdout,"IF   ");
      print_plnode(plnode->sons,indent+5);
      print_indent(indent);
      fprintf(stdout,"THEN ");
      print_plnode(plnode->sons->next,indent+5);
      if (NULL!=plnode->sons->next->next) spec_error("WHEN");
      print_indent(indent);
      fprintf(stdout,"ENDIF\n");
      break;
    case NOT:
      if (ATOM==plnode->sons->connective)
	{
	  fprintf(stdout,"NOT ");
	  print_plnode(plnode->sons,indent+4);
	}
      else
	{
	  fprintf(stdout,"NOT(");
	  print_plnode(plnode->sons,indent+4);
	  print_indent(indent+3);
	  fprintf(stdout,")\n");
	}
      if (NULL!=plnode->sons->next) spec_error("NOT");
      break;
    case TRU:
      fprintf(stdout,"TRUE\n");
      if (NULL!=plnode->sons) spec_error("TRU");
      break;
    case FAL:
      fprintf(stdout,"FALSE\n");
      if (NULL!=plnode->sons) spec_error("FAL");
      break;   
    case EMPTY:
      fprintf(stdout,"EMPTY\n");
      if (NULL!=plnode->sons) spec_error("EMPTY");
      break;   
    case ATOM:
      fprintf(stdout,"(");
      print_hidden_tokenlist(plnode->atom, " ");
      fprintf(stdout,")\n");
      if (NULL!=plnode->sons) spec_error("ATOM");
      break;
    default:
      fprintf(stdout, "\n***** ERROR ****");
      fprintf(stdout, "\nprint_plnode: %d > Wrong Node specifier\n", plnode->connective);
/*       fprintf(stdout, "\nprint_plnode: %s > Wrong Node specifier\n", gconnectives[plnode->connective]); */
      exit(1);
      ;
    }     
}  




void print_CodeNode( CodeNode *node, int indent )

{

  CodeNode *i_son;
  int i, n;
  Integers *in;

  if ( !node ) {
    fprintf(stdout, "none\n");
    return;
  }
  
  switch ( node->connective ) {
  case ALL: 
    printf( "ALL x%d : %s", node->var, gtypes_table[node->var_type].name );
    if ( gtypes_table[node->var_type].name == gnew_types_name ) {
      printf("( ");
      for ( in = gtypes_table[node->var_type].integers;
	    in; in = in->next ) {
	printf("%s ", gconstants_table[in->index]);
      }
      printf(")");
    }
    printf("\n");
    print_indent( indent );
    printf( "(   " );
    print_CodeNode( node->sons, indent+4 );
    if ( node->sons->next ) spec_error( "ALL" );
    print_indent( indent );
    printf(")\n");
    break;
  case EX:
    printf( "EX  x%d : %s", node->var, gtypes_table[node->var_type].name );
    if ( gtypes_table[node->var_type].name == gnew_types_name ) {
      printf("( ");
      for ( in = gtypes_table[node->var_type].integers;
	    in; in = in->next ) {
	printf("%s ", gconstants_table[in->index]);
      }
      printf(")");
    }
    printf("\n");
    print_indent( indent );
    printf( "(   " );
    print_CodeNode( node->sons, indent+4 );
    if ( node->sons->next ) spec_error( "ALL" );
    print_indent( indent );
    printf(")\n");
    break;
  case AND: 
    printf("(  ");
    print_CodeNode( node->sons, indent+4 );
    if ( node->sons ) {
      for (i_son = node->sons->next; i_son!=NULL; i_son = i_son->next) {
	print_indent(indent);
	printf("AND ");
	print_CodeNode( i_son, indent+4 );
      }
    }
    print_indent(indent);      
    printf(")\n");
    break;
  case OR:  
    printf("(  ");
    print_CodeNode( node->sons, indent+4 );
    if ( node->sons ) {
      for (i_son = node->sons->next; i_son!=NULL; i_son = i_son->next) {
	print_indent(indent);
	fprintf(stdout,"OR ");
	print_CodeNode( i_son, indent+4 );
      }
    }
    print_indent(indent);      
    printf(")\n");
    break;
  case WHEN:
    printf("IF   ");
    print_CodeNode( node->sons, indent+5 );
    print_indent(indent);
    printf("THEN ");
    print_CodeNode( node->sons->next, indent+5 );
    if ( node->sons->next &&
	 node->sons->next->next ) spec_error("WHEN");
    print_indent( indent );
    printf("ENDIF\n");
    break;
  case NOT:
    if ( ATOM == node->sons->connective) {
      printf("NOT ");
      print_CodeNode( node->sons, indent+4 );
    } else {
      printf("NOT(");
      print_CodeNode( node->sons, indent+4 );
      print_indent(indent+3);
      printf(")\n");
    }
    if ( node->sons->next ) spec_error("NOT");
    break;
  case TRU:
    printf("TRUE\n");
    if ( node->sons ) spec_error("TRU");
    break;
  case FAL:
    printf("FALSE\n");
    if ( node->sons ) spec_error("FAL");
    break;   
  case EMPTY:
    printf("EMPTY\n");
    if ( node->sons ) spec_error("EMPTY");
    break;   
  case ATOM:
    printf("( %s ",
	   node->predicate == -1 ?
	   "EQ" : gpredicates_table[node->predicate]);
    n = node->predicate == -1 ? 2 : garity[node->predicate];
    for ( i=0; i<n; i++ ) {
      if ( node->arguments[i] < 0 ) {
	printf("x%d ", ((-1)*node->arguments[i])-1);
      } else {
	printf("%s ", gconstants_table[node->arguments[i]]);
      }
    }
    printf(")\n");
    if ( node->sons ) spec_error("ATOM");
    break;
  default:
    printf("\n***** ERROR ****");
    printf("\nprint_CodeNode: %d > Wrong Node specifier\n", node->connective);
    exit(1);
  }

} 






/**********************************************************************
 *
 *********************************************************************/
void
print_plops(PlOperator * plop)
{
  PlOperator * i_plop;
  int count = 0;

  if (NULL == plop) 
    {
      fprintf(stdout,"none\n");
    }

  for ( i_plop = plop; i_plop!=NULL; i_plop = i_plop->next )
    {
      fprintf(stdout,"\nOPERATOR ");
      print_hidden_tokenlist(i_plop->name, " ");
      fprintf(stdout,"\nparameters:\n");
      print_factlist ( i_plop->params, "\n", " : ");
      fprintf(stdout,"\n\npreconditions:\n");
      print_plnode(i_plop->preconds, 0);
      fprintf(stdout,"effects:\n");
      print_plnode(i_plop->effects, 0);
      fprintf(stdout,"\n-----\n");
      count++;
    }
  fprintf(stdout, "\nAnzahl der Operatoren: %d\n", count);
}


/**********************************************************************
 *
 *********************************************************************/
void
print_plop(PlOperator * plop)
{
  if (NULL == plop) {
      fprintf(stdout,"\nEmpty Op\n");
  }

      fprintf(stdout,"\nOPERATOR ");
      print_hidden_tokenlist(plop->name, " ");
      fprintf(stdout,"\nparameters:\n");
      print_factlist ( plop->params, "\n", " : ");
      fprintf(stdout,"\n\npreconditions:\n");
      print_plnode(plop->preconds, 0);
      fprintf(stdout,"effects:\n");
      print_plnode(plop->effects, 0);
      fprintf(stdout,"\n-----\n");
}




void print_CodeOperator( CodeOperator *op )

{

  int i;
  Integers *in;

  if ( !op) {
    printf("\nEmpty Op\n");
    return;
  }

  printf("\nCoded Operator %s, parameters:", op->name);
  for ( i=0; i<op->num_vars; i++ ) {
    printf("\nx%d: %s", i, gtypes_table[op->var_types[i]].name);
    if ( gtypes_table[op->var_types[i]].name == gnew_types_name ) {
      printf("( ");
      for ( in = gtypes_table[op->var_types[i]].integers;
	    in; in = in->next ) {
	printf("%s ", gconstants_table[in->index]);
      }
      printf(")");
    }
  }
  printf("\n");

  printf("\nInstantiation:");
  for ( i=0; i<op->num_vars; i++ ) {
    printf("\nx%d: %s", i, 
	   op->inst_table[i] >=0 ? gconstants_table[op->inst_table[i]] : "?");
  }
  printf("\n");

  printf("\npreconds:\n");
  print_CodeNode( op->preconds, 0 );

  printf("conditionals:\n");
  print_CodeNode( op->conditionals, 0);

  printf("\n");

}







/*
 *
 */
void print_vector( BitVector *vec, int vec_len )

{

  int i, uid_block;
  unsigned int uid_mask;

  printf("\n");
  for (i=0; i < vec_len * gcword_size; i++) {
    if ( i % 8 == 0 && i != 0 ) {
      printf(":");
    }
    uid_block = i / gcword_size;
    uid_mask = 1 << ( i % gcword_size );
    if ( vec[uid_block] & uid_mask ) {
      printf("1");
    } else {
      printf("0");
    }
  }

}

/*
 *
 */
void 
print_fact_info(FactInfo * f_info, int vec_len)
{
  Integers * i;

  print_vector(f_info->vector, vec_len);
  
  if (NULL != f_info->indices)
    {
      fprintf(stdout, "%d", f_info->indices->index);

      for (i = f_info->indices->next; i; i = i->next)
	{
	  fprintf(stdout, ", %d", i->index);
	}
    }
  fprintf(stdout, "\n");
}



void print_plan( int time ) 

{

  OpNode *op;

  int i, j;
  Bool first;

  fprintf( OUT, "\nfound plan as follows:\n\n" );

  for ( i=0; i<time; i++ ) {
    fprintf( OUT, "time step %4d: ", i);
    first = TRUE;
    for ( j=0; j<gnum_ops_at[i]; j++ ) {
      op = gops_at[i][j];
      if ( op->is_noop ) continue;
      if ( first ) {
	print_op_name( op );
	fprintf( OUT, "\n" );
	first = FALSE;
      } else {
	fprintf( OUT, "                " );
	print_op_name( op );
	fprintf( OUT, "\n" );	
      }
    }
  }

}



void print_incremental_plan( Candidate *c )

{

  int time = gfirst_full_time + 1, i;
  Candidate *p;
  OpNode *op;
  Bool first;

  for ( p = c; p->father; p = p->father ) {   
    fprintf(OUT, "time step %4d: ", time++);
    first = TRUE;
    for ( i=0; p->ops[i] != NULL; i++ ) {
      op = p->ops[i];
      if ( op->is_noop ) continue;
      if ( first ) {
	print_op_name( op );
	fprintf( OUT, "\n" );
	first = FALSE;
      } else {
	fprintf( OUT, "                " );
	print_op_name( op );
	fprintf( OUT, "\n" );	
      }
    }
  }

  printf("\n");

}



void print_BitOperator( BitOperator *o )

{

  Effect *i;

  if ( !o ) {
    printf("\nempty BitOperator");
    return;
  }

  printf("\n");
  printf("\npos preconds:");
  print_FactInfo( o->p_preconds );
  printf("\nneg preconds:");
  print_FactInfo( o->n_preconds );

  printf("\nunconditional:");
  print_Effect( o->unconditional );

  printf("\nconditionals:");
  for ( i = o->conditionals; i; i = i->next ) {
    print_Effect( i );
  }

}



void print_Effect( Effect *e )

{

  if ( !e ) {
    printf("\nempty Effect");
    return;
  }

  printf("\npos conds");
  print_FactInfo( e->p_conds );
  printf("\nneg conds:");
  print_FactInfo( e->n_conds );
  printf("\npos effects");
  print_FactInfo( e->p_effects );
  printf("\nneg effects:");
  print_FactInfo( e->n_effects );


}



void print_FactInfo( FactInfo *f )

{

  Integers *i;
  RelevantFact *ff;

  if ( !f ) {
    printf("\nempty FactInfo");
    return;
  }

  printf("\nvector:");
  print_BitVector( f->vector, gft_vector_length );
  printf("\nthis is short for:");
  for ( i = f->indices; i; i = i->next ) {
    ff = grelevant_facts[i->index];
    printf("\n%d: ", i->index);
    print_fact( ff->predicate, ff->arguments );
  }

}



void print_BitVector( BitVector *vec, int vec_len )

{

  int i, uid_block;
  unsigned int uid_mask;

  printf("\n");
  for (i=0; i < vec_len * gcword_size; i++) {
    if ( i % 8 == 0 && i != 0 ) {
      printf(":");
    }
    uid_block = i / gcword_size;
    uid_mask = 1 << ( i % gcword_size );
    if ( vec[uid_block] & uid_mask ) {
      printf("1");
    } else {
      printf("0");
    }
  }

}




void print_fact( short int predicate, ArgArray arguments )

{

  int j;

  printf("%s(", gpredicates_table[predicate]);
  for ( j=0; j<garity[predicate]; j++ ) {
    printf("%s", gconstants_table[arguments[j]]);
    if ( j < garity[predicate] - 1 ) {
      printf(" ");
    }
  }
  printf(")");

}


void print_op_name( OpNode *op )

{

  int i;

  fprintf(OUT, "%s", op->name);
  for ( i=0; i<op->num_vars; i++ ) {
    fprintf(OUT, " %s", gconstants_table[op->inst_table[i]]);
  }

}


void print_ft_name( FtNode *ft )

{

  if ( !ft->positive ) {
    printf("NOT ");
  }
  print_fact( grelevant_facts[ft->index]->predicate,
	      grelevant_facts[ft->index]->arguments );

}
