

/*********************************************************************
 * (C) Copyright 1999 Albert Ludwigs University Freiburg
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
 * File: output.c
 * Description: printing info out
 *
 * Author: Joerg Hoffmann
 *
 *********************************************************************/ 





#include "bb.h"

#include "output.h"







/* parsing
 */







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



void print_PlNode( PlNode *plnode, int indent )

{

  PlNode *i_son;

  if ( !plnode ) {
    printf("none\n");
    return;
  }
  
  switch (plnode->connective) {
  case ALL: 
    printf("ALL %s : %s\n", plnode->atom->item,
	    plnode->atom->next->item);
    print_indent(indent);
    printf("(   ");
    print_PlNode(plnode->sons,indent+4);
    print_indent(indent);
    printf(")\n");
    break;
  case EX:
    printf("EX  %s : %s\n", plnode->atom->item,
	    plnode->atom->next->item);
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
  case NOT:
    if (ATOM==plnode->sons->connective) {
      printf("NOT ");
      print_PlNode(plnode->sons,indent+4);
    } else {
      printf("NOT(");
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
  default:
    printf("\n***** ERROR ****");
    printf("\nprint_plnode: %d > Wrong Node specifier\n", plnode->connective);
    printf("\nEXIT: Node Error (output.c)");
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
    printf("\nparameters:\n");
    print_FactList ( i_plop->params, "\n", " : ");
    printf("\n\npreconditions:\n");
    print_PlNode(i_plop->preconds, 0);
    printf("effects:\n");
    print_PlNode(i_plop->effects, 0);
    printf("\n-----\n");
    count++;
  }
  printf("\nAnzahl der Operatoren: %d\n", count);

}







/* facts and actions
 */








void print_Fact( Fact *f )

{

  int j;

  if ( f->predicate == -1 ) {
    printf("=(");
    for ( j=0; j<2; j++ ) {
      if ( f->args[j] >= 0 ) {
	printf("%s", gconstants[(f->args)[j]]);
      } else {
	printf("x%d", DECODE_VAR( f->args[j] ));
      }
      if ( j < 1) {
	printf(" ");
      }
    }
    printf(")");
    return;
  }
    
  printf("%s(", gpredicates[f->predicate]);
  for ( j=0; j<garity[f->predicate]; j++ ) {
    if ( f->args[j] >= 0 ) {
      printf("%s", gconstants[(f->args)[j]]);
    } else {
      printf("x%d", DECODE_VAR( f->args[j] ));
    }
    if ( j < garity[f->predicate] - 1 ) {
      printf(" ");
    }
  }
  printf(")");

}

void print_FactToFile( int index, FILE* fp )

{
	
  Fact* f = &(grelevant_facts[index]);

  int j;

  if ( f->predicate == -1 ) {
    fprintf(fp, "=(");
    for ( j=0; j<2; j++ ) {
      if ( f->args[j] >= 0 ) {
	fprintf(fp, "%s", gconstants[(f->args)[j]]);
      } else {
	fprintf(fp, "x%d", DECODE_VAR( f->args[j] ));
      }
      if ( j < 1) {
	fprintf(fp, " ");
      }
    }
    fprintf(fp, ")");
    return;
  }
    
  fprintf(fp, "%s(", gpredicates[f->predicate]);
  for ( j=0; j<garity[f->predicate]; j++ ) {
    if ( f->args[j] >= 0 ) {
      fprintf(fp, "%s", gconstants[(f->args)[j]]);
    } else {
      fprintf(fp, "x%d", DECODE_VAR( f->args[j] ));
    }
    if ( j < garity[f->predicate] - 1 ) {
      fprintf(fp, " ");
    }
  }
  fprintf(fp, ")");

}



void print_ft_name( int index )

{

  print_Fact( &(grelevant_facts[index]) );

}



void print_op_name( int index )

{

  int i;

  if ( gop_conn[index].noop_for == -1 ) {
    if ( goperators[gop_conn[index].op]->name ) {
      printf("%s", goperators[gop_conn[index].op]->name);
    }
    for ( i=0; i<goperators[gop_conn[index].op]->num_vars; i++ ) {
      printf(" %s", gconstants[gop_conn[index].inst_table[i]]);
    }
  } else {
    printf("NOOP-");
    print_ft_name( gop_conn[index].noop_for );
  }

}

void print_op_nameToFile( int index, FILE* fp ) {
	int i;

	if ( gop_conn[index].noop_for == -1 ) {
            if ( goperators[gop_conn[index].op]->name ) {
               fprintf(fp, "%s", goperators[gop_conn[index].op]->name);
	    }
	    for ( i=0; i<goperators[gop_conn[index].op]->num_vars; i++ ) {
	      fprintf(fp, " %s", gconstants[gop_conn[index].inst_table[i]]);
	    }
	} else {
		fprintf(fp, "NOOP-");
		print_FactToFile( index, fp );
	}
}



void print_Operator( Operator *o )

{

  int i, j, t;

  printf("\nCoded Operator %s, parameters:", o->name);

  for ( i=0; i<o->num_vars; i++ ) {
    printf("\nx%d: ", i);
    t = o->var_types[i];
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
  printf("\n");

  printf("\ninstantiation:");
  for ( i=0; i<o->num_vars; i++ ) {
    printf("\nx%d = %s", i, 
	   ( o->inst_table[i] >= 0 ) ? gconstants[o->inst_table[i]] : "?");
  }
  printf("\n");

  printf("\npreconditions:");
  for ( i = 0; i < o->num_preconds; i++ ) {
    printf("\n");
    print_Fact( &(o->preconds[i]) );
  }
  printf("\n");

  printf("\nadds:");
  for ( i = 0; i < o->num_adds; i++ ) {
    printf("\n");
    print_Fact( &(o->adds[i]) );
  }
  printf("\n");

  printf("\ndels:");
  for ( i = 0; i < o->num_dels; i++ ) {
    printf("\n");
    print_Fact( &(o->dels[i]) );
  }
  printf("\n");

}



void print_Action( Action *a )

{

  int i;
  Operator *o = goperators[a->op];

  printf("\nCoded Action %s ", o->name);
  for ( i = 0; i < o->num_vars; i++ ) {
    printf("%s ", gconstants[a->inst_table[i]]);
  }
  printf("\n");

  printf("\npreconditions:");
  for ( i = 0; i < a->num_preconds; i++ ) {
    printf("\n");
    print_ft_name( a->preconds[i] );
  }
  printf("\n");

  printf("\nadds:");
  for ( i = 0; i < a->num_adds; i++ ) {
    printf("\n");
    print_ft_name( a->adds[i] );
  }
  printf("\n");

  printf("\ndels:");
  for ( i = 0; i < a->num_dels; i++ ) {
    printf("\n");
    print_ft_name( a->dels[i] );
  }
  printf("\n");

}








/* graph-related output
 */












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







/* non trivial output routines
 */




