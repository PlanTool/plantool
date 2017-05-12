


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
 * File: parse.c
 * Description: Functions for the pddl parser
 *
 * Author: Frank Rittinger 1998 / Joerg Hoffmann 1999
 *
 *********************************************************************/ 






#include "bb.h"

#include "memory.h"
#include "output.h"

#include "parse.h"
#include <string.h>







/* used for type hirarchy during parsing
 */







type_tree main_type_tree( void )

{

  type_tree_list ttl;

  for ( ttl = gglobal_type_tree_list; ttl; ttl = ttl->next ) {
    if ( strcmp( ttl->item->name, STANDARD_TYPE ) == SAME ) {
      return ttl->item;
    }
  }

  return NULL;

}



/* steps recursively through type tree and searches for name 
 */
type_tree find_branch( char *name, type_tree root )

{

  type_tree p;
  type_tree_list ttl;

  if ( !root ) {
    return NULL;
  }
  if ( strcmp( root->name, name ) == SAME ) {
    return root;
  }
  if ( !root->sub_types ) {
    return NULL;
  }
  for ( ttl=root->sub_types; ttl; ttl=ttl->next ) {
    if ((p = find_branch( name, ttl->item ))) {
      return p;
    }
  }

  return NULL; 

}



void add_to_type_tree( FactList *t_list, type_tree tree )

{

  type_tree branch = tree;
  type_tree_list new_branch;
  char *this_type; 
  char *super_type;

  /* step through list and build a hierarchy of types 
   */
  for( ; t_list; t_list=t_list->next ) {
    this_type = t_list->item->item;
    if ( !t_list->item->next ) {
      fprintf(stderr, "\n%s: error at '%s'.\n", gact_filename, this_type );
      exit( 1 );
    }
    super_type = t_list->item->next->item;
    if ( strcmp( branch->name, super_type ) != SAME ) {
      branch = find_branch( super_type, tree );
    }
    if ( !branch ) {
      fprintf(stderr, "\n%s: unknown type '%s'.\n", 
	      gact_filename, super_type );
      exit( 1 );
    }
    /* now the type is a subtype of the one currently looked at 
     * in the type tree 
     */
    new_branch = new_type_tree_list( this_type );
    new_branch->next = branch->sub_types;
    branch->sub_types = new_branch;
  }

}









/* descendants of build_orig_constant_list
 */









void build_orig_constant_list( void )

{

  FactList *f, *nextf, *end = NULL;
  TokenList *t;
  Bool do_count;
  int objects_count = 0;

  gtypes = build_object_list_from_ttl( gglobal_type_tree_list, NULL );
  free_FactList( gorig_constant_list );
  gorig_constant_list = NULL;

  for ( f = gtypes; f; f = nextf ) {
    nextf = f->next;
    if ( strcmp( f->item->item, STANDARD_TYPE ) == SAME ) {
      do_count = TRUE;
    } else {
      do_count = FALSE;
    }
    for ( t = f->item->next; t; t = t->next ) {
      if ( !gorig_constant_list ) {
	end = new_FactList();
	gorig_constant_list = end;
      } else {
	end->next = new_FactList();
	end = end->next;
      }
      end->item = new_TokenList();
      end->item->item = copy_Token( t->item );
      end->item->next = new_TokenList();
      end->item->next->item = copy_Token( f->item->item );
      if ( do_count ) {
	/* count objects for RIFO meta strategy 
	 */
	objects_count++;
      }
    }
  }

}



/* calls itself recursively to get all objects that are of the types and
 * subtypes of ttl 
 */
FactList *build_object_list_from_ttl( type_tree_list ttl, FactList *types_done )

{

  FactList *f;
  FactList *td;
  FactList *std;
  TokenList *t;
  TokenList *tl_dummy;
  TokenList *st;
  type_tree_list sttl;

  if ( !ttl ) {
    return types_done;
  }
  
  types_done = build_object_list_from_ttl( ttl->next, types_done );

  if ((t = type_already_known( ttl->item->name, types_done))) {
    return types_done;
  }

  td = new_FactList();
  t = td->item = new_TokenList();
  /* begin with the name of the type... 
   */
  t->item = copy_Token( ttl->item->name );
  for ( f = gorig_constant_list; f; f = f->next ) {
    /* ...followed by objects of that type. 
     */
    if ( strcmp( f->item->next->item, ttl->item->name ) == SAME ) {
      t->next = new_TokenList();
      t = t->next;
      t->item = copy_Token( f->item->item );
    }
  }
  /* now append the objects of the subtypes
   */
  std =  build_object_list_from_ttl( ttl->item->sub_types, types_done );
  /* now we can be sure that for each subtype a list with all
   * objects of that type is somewhere in std. We simply take these
   * lists and copy all of them into a new one for the supertype 
   */
  for ( sttl = ttl->item->sub_types; sttl; sttl = sttl->next ) {
    st = type_already_known( sttl->item->name, std );
    if ( st ) {
      t->next = copy_complete_TokenList( st, &tl_dummy );
      t = tl_dummy;
    }
  }
  td->next = std;
  
  return td;

}



TokenList *type_already_known( char *name, FactList *types )

{

  FactList *f;

  for ( f = types; f; f = f->next ) {
    if ( strcmp( f->item->item, name ) == SAME ) {
      return f->item->next;
    }
  }

  return NULL;

}









/* simple parse helpers
 */







char *copy_Token( char *s )

{

  char *d = new_Token( strlen( s ) + 1 );
  strcpy(d, s);

  return d;

}



TokenList *copy_complete_TokenList( TokenList *source, 
				    TokenList **end )

{

  TokenList *temp;

  if ( !source ) {
    temp = NULL;
  } else {
    temp = new_TokenList();
    if ( source->item ) {
      temp->item = new_Token( strlen( source->item ) + 1 );
      strcpy( temp->item, source->item );
    }
    temp->next = copy_complete_TokenList( source->next, end );
    if ( !temp->next ) {
      *end = temp;
    }
  }

  return temp;

}



void strupcase( char *from )

{

  char tmp;

  tmp = *from;
  while ('\0' != tmp) {
    *from = (char) toupper((int) tmp);
    tmp = *++from;
  }

}



char *rmdash( char *s )

{

  s++;

  for( ; (*s == ' ') || (*s == '\t'); s++ );

  return s;

}







/* STRIPS syntax test
 */








Bool make_strips_domain( void )

{

  PlOperator *i;
  FactList *ff;

  if ( !make_conjunction_of_atoms( &gorig_initial_facts ) ) {
    printf("\nillegal initial state");
    return FALSE;
  }

  if ( !make_conjunction_of_atoms( &gorig_goal_facts ) ) {
    printf("\nillegal goal state");
    return FALSE;
  }

  for ( i = gloaded_ops; i; i = i->next ) {
    if ( !make_conjunction_of_atoms( &(i->preconds) ) ) {
      printf("\nop %s has illegal precondition", i->name);
      return FALSE;
    }
    if ( !make_conjunction_of_literals( &(i->effects) ) ) {
      printf("\nop %s has illegal effects", i->name);
      return FALSE;
    }
  }

  if ( gcmd_line.display_info == 101 ) {
    printf("\nfinal STRIPS representation is:\n");
    printf("\nobjects:");
    for ( ff = gorig_constant_list; ff; ff = ff->next ) {
      printf("\n%s : %s", ff->item->item, ff->item->next->item);
    }
    printf("\n\ninitial state:\n");
    print_PlNode( gorig_initial_facts, 0 );
    printf("\n\ngoal state:\n");
    print_PlNode( gorig_goal_facts, 0 );
    printf("\n\nops:");
    print_plops( gloaded_ops );
  }

  return TRUE;
      
}



Bool make_conjunction_of_atoms( PlNode **n )

{

  PlNode *tmp, *i;

  if ( !(*n) ) {
    return TRUE;
  }

  if ( (*n)->connective != AND ) {
    if ( (*n)->connective != ATOM ) {
      return FALSE;
    }
    tmp = new_PlNode( ATOM );
    tmp->atom = (*n)->atom;
    (*n)->atom = NULL;
    (*n)->connective = AND;
    (*n)->sons = tmp;
    return TRUE;
  }

  for ( i = (*n)->sons; i; i = i->next ) {
    if ( i->connective != ATOM ) {
      return FALSE;
    }
  }

  return TRUE;

}



Bool make_conjunction_of_literals( PlNode **n )

{

  PlNode *tmp, *i;

  if ( !(*n) ) {
    return TRUE;
  }

  if ( (*n)->connective != AND ) {
    if ( (*n)->connective == NOT ) {
      if ( !((*n)->sons) ||
	   (*n)->sons->connective != ATOM ) {
	return FALSE;
      }
      tmp = new_PlNode( NOT );
      tmp->sons = (*n)->sons;
      (*n)->connective = AND;
      (*n)->sons = tmp;
      return TRUE;
    }
    if ( (*n)->connective != ATOM ) {
      return FALSE;
    }
    tmp = new_PlNode( ATOM );
    tmp->atom = (*n)->atom;
    (*n)->atom = NULL;
    (*n)->connective = AND;
    (*n)->sons = tmp;
    return TRUE;
  }

  for ( i = (*n)->sons; i; i = i->next ) {
    if ( i->connective == NOT ) {
      if ( !(i->sons) ||
	   i->sons->connective != ATOM ) {
	return FALSE;
      }
      continue;
    }
    if ( i->connective != ATOM ) {
      return FALSE;
    }
  }

  return TRUE;

}
