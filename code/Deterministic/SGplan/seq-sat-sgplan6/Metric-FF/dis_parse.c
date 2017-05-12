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

 * File: dis_parse.c 

 * Description: modified from parse.c in Metric-FF

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
 * File: parse.c
 * Description: Functions for the pddl parser
 *
 * Author: Joerg Hoffmann 2000
 *
 *********************************************************************/ 






#include "dis_ff.h"

#include "dis_memory.h"
#include "dis_output.h"
#include "dis_inst_pre.h"
#include "dis_parse.h"











/* simple parse helpers
 */







char *dis_copy_dis_Token( char *s )

{

  char *d = dis_new_dis_Token( strlen( s ) + 1 );
  strcpy(d, s);

  return d;

}



dis_TokenList *dis_copy_dis_TokenList( dis_TokenList *source )

{

  dis_TokenList *temp;

  if ( !source ) {
    temp = NULL;
  } else {
    temp = dis_new_dis_TokenList();
    if ( source->item ) {
      temp->item = dis_new_dis_Token( strlen( source->item ) + 1 );
      strcpy( temp->item, source->item );
    }
    temp->next = dis_copy_dis_TokenList( source->next );
  }

  return temp;

}



void dis_strupcase( char *from )

{

  char tmp;

  tmp = *from;
  while ('\0' != tmp) {
    *from = (char) toupper((int) tmp);
    tmp = *++from;
  }

}



char *dis_rmdash( char *s )

{

  s++;

  for( ; (*s == ' ') || (*s == '\t'); s++ );

  return s;

}










/* typed-list-of   preprocessing
 */







dis_Token ltype_names[dis_MAX_TYPES];
int lnum_types;


int leither_ty[dis_MAX_TYPES][dis_MAX_TYPES];
int lnum_either_ty[dis_MAX_TYPES];

void dis_add_types( void )
{
  int i, type_num;
  
  for (i=0;i<lnum_types;i++)
  {
    if ((type_num = dis_position_in_types_table(ltype_names[i])) == -1)
    {
      if ( dis_gnum_types == dis_MAX_TYPES ) {
        printf("\ntoo many types! increase dis_MAX_TYPES (currently %d)\n\n",
               dis_MAX_TYPES);
        exit( 1 );
      }
      dis_gtype_names[dis_gnum_types] = dis_copy_dis_Token( ltype_names[i] );
      dis_gtype_size[dis_gnum_types] = 0;
      for ( i = 0; i < dis_MAX_CONSTANTS; i++ ) {
        dis_gis_member[i][dis_gnum_types] = dis_FALSE;
      }
      type_num = dis_gnum_types++;
    }
  }
}

void dis_build_orig_constant_list( void )

{

  char *tmp = NULL;
  dis_TypedList *tyl;
  dis_TypedListList *tyll;
  dis_TokenList *tl, *p_tl, *tmp_tl;
  dis_Pldis_Operator *po;

  int i, j, k, n, std;

  dis_Bool m[dis_MAX_TYPES][dis_MAX_TYPES];

  dis_FactList *fl, *p_fl;

  lnum_types = 0;
  for ( tyl = dis_gparse_types; tyl; tyl = tyl->next ) {
    if ( dis_get_type( tyl->name ) == -1 ) {
      ltype_names[lnum_types++] = dis_copy_dis_Token( tyl->name );
    }
    if ( tyl->type->next ) {
      n = 1 + strlen(dis_EITHER_STR);
      for (tl=tyl->type;tl;tl=tl->next)
      {
        n += strlen(dis_CONNECTdis_OR);
        n += strlen(tl->item);
      }
      tmp = dis_new_dis_Token( n );
      strcpy( tmp, dis_EITHER_STR );
      for ( tl = tyl->type; tl; tl = tl->next ) {
	strcat( tmp, dis_CONNECTdis_OR );
	strcat( tmp, tl->item );
      }
    } else {
      tmp = dis_copy_dis_Token( tyl->type->item );
    }
    if ( (n = dis_get_type( tmp )) == -1 ) {
      tyl->n = lnum_types;
      ltype_names[lnum_types++] = dis_copy_dis_Token( tmp );
    } else {
      tyl->n = n;
    }
    xfree( tmp );
    tmp = NULL;
  }
     
  for ( tyl = dis_gparse_constants; tyl; tyl = tyl->next ) {
    if ( tyl->type->next ) {
      n = 1 + strlen(dis_EITHER_STR);
      for (tl=tyl->type;tl;tl=tl->next)
      {
        n += strlen(dis_CONNECTdis_OR);
        n += strlen(tl->item);
      }
      tmp = dis_new_dis_Token( n );
      strcpy( tmp, dis_EITHER_STR );
      for ( tl = tyl->type; tl; tl = tl->next ) {
	strcat( tmp, dis_CONNECTdis_OR );
	strcat( tmp, tl->item );
      }
    } else {
      tmp = dis_copy_dis_Token( tyl->type->item );
    }
    if ( (n = dis_get_type( tmp )) == -1 ) {
      tyl->n = lnum_types;
      ltype_names[lnum_types++] = dis_copy_dis_Token( tmp );
    } else {
      tyl->n = n;
    }
    xfree( tmp );
    tmp = NULL;
  }
  
  for ( tyl = dis_gparse_objects; tyl; tyl = tyl->next ) {
    if ( tyl->type->next ) {
      n = 1 + strlen(dis_EITHER_STR);
      for (tl=tyl->type;tl;tl=tl->next)
      {
        n += strlen(dis_CONNECTdis_OR);
        n += strlen(tl->item);
      }
      tmp = dis_new_dis_Token( n );
      strcpy( tmp, dis_EITHER_STR );
      for ( tl = tyl->type; tl; tl = tl->next ) {
	strcat( tmp, dis_CONNECTdis_OR );
	strcat( tmp, tl->item );
      }
    } else {
      tmp = dis_copy_dis_Token( tyl->type->item );
    }
    if ( (n = dis_get_type( tmp )) == -1 ) {
      tyl->n = lnum_types;
      ltype_names[lnum_types++] = dis_copy_dis_Token( tmp );
    } else {
      tyl->n = n;
    }
    xfree( tmp );
    tmp = NULL;
  }

  for ( tyll = dis_gparse_predicates; tyll; tyll = tyll->next ) {
    for ( tyl = tyll->args; tyl; tyl = tyl->next ) {
      if ( tyl->type->next ) {
        n = 1 + strlen(dis_EITHER_STR);
        for (tl=tyl->type;tl;tl=tl->next)
        {
          n += strlen(dis_CONNECTdis_OR);
          n += strlen(tl->item);
        }
        tmp = dis_new_dis_Token( n );
	strcpy( tmp, dis_EITHER_STR );
	for ( tl = tyl->type; tl; tl = tl->next ) {
	  strcat( tmp, dis_CONNECTdis_OR );
	  strcat( tmp, tl->item );
	}
      } else {
	tmp = dis_copy_dis_Token( tyl->type->item );
      }
      if ( (n = dis_get_type( tmp )) == -1 ) {
	tyl->n = lnum_types;
	ltype_names[lnum_types++] = dis_copy_dis_Token( tmp );
      } else {
	tyl->n = n;
      }
      xfree( tmp );
      tmp = NULL;
    }
  }
    
  for ( tyll = dis_gparse_functions; tyll; tyll = tyll->next ) {
    for ( tyl = tyll->args; tyl; tyl = tyl->next ) {
      if ( tyl->type->next ) {
        n = 1 + strlen(dis_EITHER_STR);
        for (tl=tyl->type;tl;tl=tl->next)
        {
          n += strlen(dis_CONNECTdis_OR);
          n += strlen(tl->item);
        }
        tmp = dis_new_dis_Token( n );
	strcpy( tmp, dis_EITHER_STR );
	for ( tl = tyl->type; tl; tl = tl->next ) {
	  strcat( tmp, dis_CONNECTdis_OR );
	  strcat( tmp, tl->item );
	}
      } else {
	tmp = dis_copy_dis_Token( tyl->type->item );
      }
      if ( (n = dis_get_type( tmp )) == -1 ) {
	tyl->n = lnum_types;
	ltype_names[lnum_types++] = dis_copy_dis_Token( tmp );
      } else {
	tyl->n = n;
      }
      xfree( tmp );
      tmp = NULL;
    }
  }
    
  dis_collect_type_names_in_pl( dis_gorig_goal_facts );

  for ( po = dis_gloaded_ops; po; po = po->next ) {
    dis_collect_type_names_in_pl( po->preconds );
    dis_collect_type_names_in_pl( po->effects );
    for ( tyl = po->parse_params; tyl; tyl = tyl->next ) {
      if ( tyl->type->next ) {
        n = 1 + strlen(dis_EITHER_STR);
        for (tl=tyl->type;tl;tl=tl->next)
        {
          n += strlen(dis_CONNECTdis_OR);
          n += strlen(tl->item);
        }
        tmp = dis_new_dis_Token( n );
	strcpy( tmp, dis_EITHER_STR );
	for ( tl = tyl->type; tl; tl = tl->next ) {
	  strcat( tmp, dis_CONNECTdis_OR );
	  strcat( tmp, tl->item );
	}
      } else {
	tmp = dis_copy_dis_Token( tyl->type->item );
      }
      if ( (n = dis_get_type( tmp )) == -1 ) {
	tyl->n = lnum_types;
	ltype_names[lnum_types++] = dis_copy_dis_Token( tmp );
      } else {
	tyl->n = n;
      }
      xfree( tmp );
      tmp = NULL;
    }
  }


  /* now get the numbers of all composed either types
   */
  for ( i = 0; i < lnum_types; i++ ) {
    lnum_either_ty[i] = 0;
  }
  for ( tyl = dis_gparse_types; tyl; tyl = tyl->next ) {
    dis_make_either_ty( tyl );
  }
  for ( tyl = dis_gparse_constants; tyl; tyl = tyl->next ) {
    dis_make_either_ty( tyl );
  }
  for ( tyl = dis_gparse_objects; tyl; tyl = tyl->next ) {
    dis_make_either_ty( tyl );
  }
  for ( tyll = dis_gparse_predicates; tyll; tyll = tyll->next ) {
    for ( tyl = tyll->args; tyl; tyl = tyl->next ) {
      dis_make_either_ty( tyl );
    }
  }
  for ( tyll = dis_gparse_functions; tyll; tyll = tyll->next ) {
    for ( tyl = tyll->args; tyl; tyl = tyl->next ) {
      dis_make_either_ty( tyl );
    }
  }
  dis_make_either_ty_in_pl( dis_gorig_goal_facts );
  for ( po = dis_gloaded_ops; po; po = po->next ) {
    dis_make_either_ty_in_pl( po->preconds );
    dis_make_either_ty_in_pl( po->effects );
    for ( tyl = po->parse_params; tyl; tyl = tyl->next ) {
      dis_make_either_ty( tyl );
    }
  }


  /* now, compute the transitive closure of all type inclusions.
   * first initialize the matrix.
   */
  for ( i = 0; i < lnum_types; i++ ) {
    for ( j = 0; j < lnum_types; j++ ) {
      m[i][j] = ( i == j ? dis_TRUE : dis_FALSE );
    }
  }
  std = -1;
  for ( i = 0; i < lnum_types; i++ ) {
    if ( strcmp( ltype_names[i], dis_STdis_ANDARD_TYPE ) == dis_SAME ) {
      std = i;
      break;
    }
  }
  for ( i = 0; i < lnum_types; i++ ) {
    m[i][std] = dis_TRUE;/* all types are subtypes of OBJECT */
  }
  for ( tyl = dis_gparse_types; tyl; tyl = tyl->next ) {
    /* all inclusions as are defined in domain file
     */
    m[dis_get_type( tyl->name )][tyl->n] = dis_TRUE;
  }
  /* compute transitive closure on inclusions matrix
   */
  for ( j = 0; j < lnum_types; j++ ) {
    for ( i = 0; i < lnum_types; i++ ) {
      if ( m[i][j] ) {
	for ( k = 0; k < lnum_types; k++ ) {
	  if ( m[j][k] ) {
	    m[i][k] = dis_TRUE;
	  }
	}
      }
    }
  }
  /* union types are subsets of all those types that contain all
   * their components, and 
   * all component types are subsets of the either type !
   */
  for ( i = 0; i < lnum_types; i++ ) {
    if ( lnum_either_ty[i] < 2 ) continue;
    for ( j = 0; j < lnum_types; j++ ) {
      if ( j == i ) continue;
      /* get supertypes of all component types
       */
      for ( k = 0; k < lnum_either_ty[i]; k++ ) {
	if ( !m[leither_ty[i][k]][j] ) break;
      }
      if ( k < lnum_either_ty[i] ) continue;
      m[i][j] = dis_TRUE;
      /* make components subtypes of either type
       */
      for ( k = 0; k < lnum_either_ty[i]; k++ ) {
	m[leither_ty[i][k]][i] = dis_TRUE;
      }
    }
  }
  /* and again, compute transitive closure on inclusions matrix.
   * I guess, this won't change anything (?), but it also won't need
   * any remarkable computation time, so why should one think about it ?
   */
  for ( j = 0; j < lnum_types; j++ ) {
    for ( i = 0; i < lnum_types; i++ ) {
      if ( m[i][j] ) {
	for ( k = 0; k < lnum_types; k++ ) {
	  if ( m[j][k] ) {
	    m[i][k] = dis_TRUE;
	  }
	}
      }
    }
  }
  

  /* now build dis_FactList of dis_ALL  constant -> type   pairs.
   * for each constant / object, let it appear separately
   * for each type it is a member of; compute type
   * membership based on propagating constants / objects
   * through inclusions matrix.
   *
   * this might make the same pair appear doubly, if an object
   * is declared in type T as well as in some supertype T'.
   * such cases will be filtered out in string collection.
   */
  for ( tyl = dis_gparse_constants; tyl; tyl = tyl->next ) {
    fl = dis_new_dis_FactList();
    fl->item = dis_new_dis_TokenList();
    fl->item->next = dis_new_dis_TokenList();
    fl->item->item = dis_copy_dis_Token( tyl->name );
    if ( tyl->type->next ) {
      n = 1 + strlen(dis_EITHER_STR);
      for (tl=tyl->type;tl;tl=tl->next)
      {
        n += strlen(dis_CONNECTdis_OR);
        n += strlen(tl->item);
      }
      fl->item->next->item = dis_new_dis_Token( n );
      strcpy( fl->item->next->item, dis_EITHER_STR );
      for ( tl = tyl->type; tl; tl = tl->next ) {
	strcat( fl->item->next->item, dis_CONNECTdis_OR );
	strcat( fl->item->next->item, tl->item );
      }
    } else {
      fl->item->next->item = dis_copy_dis_Token( tyl->type->item );
    }
    fl->next = dis_gorig_constant_list;
    dis_gorig_constant_list = fl;
    /* now add constant to all supertypes
     */
    n = dis_get_type( fl->item->next->item );
    for ( i = 0; i < lnum_types; i++ ) {
      if ( i == n ||
	   !m[n][i] ) continue;
      fl = dis_new_dis_FactList();
      fl->item = dis_new_dis_TokenList();
      fl->item->next = dis_new_dis_TokenList();
      fl->item->item = dis_copy_dis_Token( tyl->name );
      fl->item->next->item = dis_copy_dis_Token( ltype_names[i] );
      fl->next = dis_gorig_constant_list;
      dis_gorig_constant_list = fl;
    }
  }
  for ( tyl = dis_gparse_objects; tyl; tyl = tyl->next ) {
    fl = dis_new_dis_FactList();
    fl->item = dis_new_dis_TokenList();
    fl->item->next = dis_new_dis_TokenList();
    fl->item->item = dis_copy_dis_Token( tyl->name );
    if ( tyl->type->next ) {
      n = 1 + strlen(dis_EITHER_STR);
      for (tl=tyl->type;tl;tl=tl->next)
      {
        n += strlen(dis_CONNECTdis_OR);
        n += strlen(tl->item);
      }
      fl->item->next->item = dis_new_dis_Token( n );
      strcpy( fl->item->next->item, dis_EITHER_STR );
      for ( tl = tyl->type; tl; tl = tl->next ) {
	strcat( fl->item->next->item, dis_CONNECTdis_OR );
	strcat( fl->item->next->item, tl->item );
      }
    } else {
      fl->item->next->item = dis_copy_dis_Token( tyl->type->item );
    }
    fl->next = dis_gorig_constant_list;
    dis_gorig_constant_list = fl;
    /* now add constant to all supertypes
     */
    n = dis_get_type( fl->item->next->item );
    for ( i = 0; i < lnum_types; i++ ) {
      if ( i == n ||
	   !m[n][i] ) continue;
      fl = dis_new_dis_FactList();
      fl->item = dis_new_dis_TokenList();
      fl->item->next = dis_new_dis_TokenList();
      fl->item->item = dis_copy_dis_Token( tyl->name );
      fl->item->next->item = dis_copy_dis_Token( ltype_names[i] );
      fl->next = dis_gorig_constant_list;
      dis_gorig_constant_list = fl;
    }
  }


  /* now, normalize all typed-list-of  s in domain and problem def,
   * i.e., in all dis_PlNode quantifiers and in op parameters
   *
   * at the same time, remove typed-listof structures in these defs
   */
  dis_normalize_tyl_in_pl( &dis_gorig_goal_facts );
  for ( po = dis_gloaded_ops; po; po = po->next ) {
    dis_normalize_tyl_in_pl( &po->preconds );
    dis_normalize_tyl_in_pl( &po->effects );
    /* be careful to maintain parameter ordering !
     */
    if ( !po->parse_params ) {
      continue;/* no params at all */
    }
    fl = dis_new_dis_FactList();
    fl->item = dis_new_dis_TokenList();
    fl->item->next = dis_new_dis_TokenList();
    fl->item->item = dis_copy_dis_Token( po->parse_params->name );
    if ( po->parse_params->type->next ) {
      n = 1 + strlen(dis_EITHER_STR);
      for (tl=po->parse_params->type;tl;tl=tl->next)
      {
        n += strlen(dis_CONNECTdis_OR);
        n += strlen(tl->item);
      }
      fl->item->next->item = dis_new_dis_Token( n );
      strcpy( fl->item->next->item, dis_EITHER_STR );
      for ( tl = po->parse_params->type; tl; tl = tl->next ) {
	strcat( fl->item->next->item, dis_CONNECTdis_OR );
	strcat( fl->item->next->item, tl->item );
      }
    } else {
      fl->item->next->item = dis_copy_dis_Token( po->parse_params->type->item );
    }
    po->params = fl;
    p_fl = fl;
    for ( tyl = po->parse_params->next; tyl; tyl = tyl->next ) {
      fl = dis_new_dis_FactList();
      fl->item = dis_new_dis_TokenList();
      fl->item->next = dis_new_dis_TokenList();
      fl->item->item = dis_copy_dis_Token( tyl->name );
      if ( tyl->type->next ) {
        n = 1 + strlen(dis_EITHER_STR);
        for (tl=tyl->type;tl;tl=tl->next)
        {
          n += strlen(dis_CONNECTdis_OR);
          n += strlen(tl->item);
        }
        fl->item->next->item = dis_new_dis_Token( n );
	strcpy( fl->item->next->item, dis_EITHER_STR );
	for ( tl = tyl->type; tl; tl = tl->next ) {
	  strcat( fl->item->next->item, dis_CONNECTdis_OR );
	  strcat( fl->item->next->item, tl->item );
	}
      } else {
	fl->item->next->item = dis_copy_dis_Token( tyl->type->item );
      }
      p_fl->next = fl;
      p_fl = fl;
    }
    dis_free_dis_TypedList( po->parse_params );
    po->parse_params = NULL;
  }


  /* finally, build  dis_gpredicates_and_types  by chaining predicate names 
   * together with the names of their args' types.
   */
  for ( tyll = dis_gparse_predicates; tyll; tyll = tyll->next ) {
    fl = dis_new_dis_FactList();
    fl->item = dis_new_dis_TokenList();
    fl->item->item = dis_copy_dis_Token( tyll->predicate );
    fl->next = dis_gpredicates_and_types;
    dis_gpredicates_and_types = fl;
    if ( !tyll->args ) continue;
    /* add arg types; MAINTAIN dis_ORDERING !
     */
    fl->item->next = dis_new_dis_TokenList();
    if ( tyll->args->type->next ) {
      n = 1 + strlen(dis_EITHER_STR);
      for (tl=tyll->args->type;tl;tl=tl->next)
      {
        n += strlen(dis_CONNECTdis_OR);
        n += strlen(tl->item);
      }
      fl->item->next->item = dis_new_dis_Token( n );
      strcpy( fl->item->next->item, dis_EITHER_STR );
      for ( tl = tyll->args->type; tl; tl = tl->next ) {
	strcat( fl->item->next->item, dis_CONNECTdis_OR );
	strcat( fl->item->next->item, tl->item );
      }
    } else {
      fl->item->next->item = dis_copy_dis_Token( tyll->args->type->item );
    }
    p_tl = fl->item->next;
    for ( tyl = tyll->args->next; tyl; tyl = tyl->next ) {
      tmp_tl = dis_new_dis_TokenList();
      if ( tyl->type->next ) {
        n = 1 + strlen(dis_EITHER_STR);
        for (tl=tyl->type;tl;tl=tl->next)
        {
          n += strlen(dis_CONNECTdis_OR);
          n += strlen(tl->item);
        }
        tmp_tl->item = dis_new_dis_Token( n );
	strcpy( tmp_tl->item, dis_EITHER_STR );
	for ( tl = tyl->type; tl; tl = tl->next ) {
	  strcat( tmp_tl->item, dis_CONNECTdis_OR );
	  strcat( tmp_tl->item, tl->item );
	}
      } else {
	tmp_tl->item = dis_copy_dis_Token( tyl->type->item );
      }
      p_tl->next = tmp_tl;
      p_tl = tmp_tl;
    }
  }

  for ( tyll = dis_gparse_functions; tyll; tyll = tyll->next ) {
    fl = dis_new_dis_FactList();
    fl->item = dis_new_dis_TokenList();
    fl->item->item = dis_copy_dis_Token( tyll->predicate );
    fl->next = dis_gfunctions_and_types;
    dis_gfunctions_and_types = fl;
    if ( !tyll->args ) continue;
    /* add arg types; MAINTAIN ORDERING !
     */
    fl->item->next = dis_new_dis_TokenList();
    if ( tyll->args->type->next ) {
      n = 1 + strlen(dis_EITHER_STR);
      for (tl=tyll->args->type;tl;tl=tl->next)
      {
        n += strlen(dis_CONNECTdis_OR);
        n += strlen(tl->item);
      }
      fl->item->next->item = dis_new_dis_Token( n );
      strcpy( fl->item->next->item, dis_EITHER_STR );
      for ( tl = tyll->args->type; tl; tl = tl->next ) {
	strcat( fl->item->next->item, dis_CONNECTdis_OR );
	strcat( fl->item->next->item, tl->item );
      }
    } else {
      fl->item->next->item = dis_copy_dis_Token( tyll->args->type->item );
    }
    p_tl = fl->item->next;
    for ( tyl = tyll->args->next; tyl; tyl = tyl->next ) {
      tmp_tl = dis_new_dis_TokenList();
      if ( tyl->type->next ) {
        n = 1 + strlen(dis_EITHER_STR);
        for (tl=tyl->type;tl;tl=tl->next)
        {
          n += strlen(dis_CONNECTdis_OR);
          n += strlen(tl->item);
        }
        tmp_tl->item = dis_new_dis_Token( n );
	strcpy( tmp_tl->item, dis_EITHER_STR );
	for ( tl = tyl->type; tl; tl = tl->next ) {
	  strcat( tmp_tl->item, dis_CONNECTdis_OR );
	  strcat( tmp_tl->item, tl->item );
	}
      } else {
	tmp_tl->item = dis_copy_dis_Token( tyl->type->item );
      }
      p_tl->next = tmp_tl;
      p_tl = tmp_tl;
    }
  }

  /* now get rid of remaining typed-list-of parsing structures
   */
  dis_free_dis_TypedList( dis_gparse_types );
  dis_gparse_types = NULL;
  dis_free_dis_TypedList( dis_gparse_constants );
  dis_gparse_constants = NULL;
  dis_free_dis_TypedList( dis_gparse_objects );
  dis_gparse_objects = NULL;
  dis_free_dis_TypedListList( dis_gparse_predicates );
  dis_gparse_predicates = NULL;
  dis_free_dis_TypedListList( dis_gparse_functions );
  dis_gparse_functions = NULL;

}



void dis_collect_type_names_in_pl( dis_PlNode *n )

{

  dis_PlNode *i;
  dis_TypedList *tyl;
  dis_TokenList *tl;
  char *tmp = NULL;
  int nn;

  if ( !n ) {
    return;
  }

  switch( n->connective ) {
  case dis_ALL:
  case dis_EX:
    for ( tyl = n->parse_vars; tyl; tyl = tyl->next ) {
      if ( tyl->type->next ) {
        nn = 1 + strlen(dis_EITHER_STR);
        for (tl=tyl->type;tl;tl=tl->next)
        {
          nn += strlen(dis_CONNECTdis_OR);
          nn += strlen(tl->item);
        }
        tmp = dis_new_dis_Token( nn );
	strcpy( tmp, dis_EITHER_STR );
	for ( tl = tyl->type; tl; tl = tl->next ) {
	  strcat( tmp, dis_CONNECTdis_OR );
	  strcat( tmp, tl->item );
	}
      } else {
	tmp = dis_copy_dis_Token( tyl->type->item );
      }
      if ( (nn = dis_get_type( tmp )) == -1 ) {
	tyl->n = lnum_types;
	ltype_names[lnum_types++] = dis_copy_dis_Token( tmp );
      } else {
	tyl->n = nn;
      }
      xfree( tmp );
      tmp = NULL;
    }
    dis_collect_type_names_in_pl( n->sons );
    break;
  case dis_AND:
  case dis_OR:
    for ( i = n->sons; i; i = i->next ) {
      dis_collect_type_names_in_pl( i );
    }
    break;
  case dis_NOT:
    dis_collect_type_names_in_pl( n->sons );
    break;
  case dis_ATOM:
  case dis_TRU:
  case dis_FAL:
    break;
  case dis_WHEN:
    dis_collect_type_names_in_pl( n->sons );
    dis_collect_type_names_in_pl( n->sons->next );
    break;
  default:
    break;
  }

}



int dis_get_type( char *str )

{

  int i;

  for ( i = 0; i < lnum_types; i++ ) {
    if ( strcmp( str, ltype_names[i] ) == dis_SAME ) return i;
  }

  return -1;

}



void dis_make_either_ty( dis_TypedList *tyl )

{

  dis_TokenList *i;

  if ( lnum_either_ty[tyl->n] > 0 ) {
    return;
  }

  for ( i = tyl->type; i; i = i->next ) {
    leither_ty[tyl->n][lnum_either_ty[tyl->n]++] = dis_get_type( i->item );
  }

}



void dis_make_either_ty_in_pl( dis_PlNode *n )

{

  dis_PlNode *i;
  dis_TypedList *tyl;

  if ( !n ) {
    return;
  }

  switch( n->connective ) {
  case dis_ALL:
  case dis_EX:
    for ( tyl = n->parse_vars; tyl; tyl = tyl->next ) {
      dis_make_either_ty( tyl );
    }
    dis_make_either_ty_in_pl( n->sons );
    break;
  case dis_AND:
  case dis_OR:
    for ( i = n->sons; i; i = i->next ) {
      dis_make_either_ty_in_pl( i );
    }
    break;
  case dis_NOT:
    dis_make_either_ty_in_pl( n->sons );
    break;
  case dis_ATOM:
  case dis_TRU:
  case dis_FAL:
    break;
  case dis_WHEN:
    dis_make_either_ty_in_pl( n->sons );
    dis_make_either_ty_in_pl( n->sons->next );
    break;
  default:
    break;
  }

}



void dis_normalize_tyl_in_pl( dis_PlNode **n )

{

  dis_PlNode *i;
  dis_TypedList *tyl;
  dis_PlNode *tmp_pl = NULL, *sons, *p_pl;
  dis_TokenList *tmp_tl, *tl;
  int nn;


  if ( !(*n) ) {
    return;
  }

  switch( (*n)->connective ) {
  case dis_ALL:
  case dis_EX:
    /* we need to make a sequence of quantifiers ( ->sons ...)
     * out of the given sequence of dis_TypedList  elements,
     * with connected type names, var - name in dis_TokenList
     * and KEEPING THE dis_SAME dis_ORDERING !!
     */
    if ( !(*n)->parse_vars ) {
      printf("\n\nquantifier without argument !! check input files.\n\n");
      exit( 1 );
    }
    tmp_tl = dis_new_dis_TokenList();
    tmp_tl->next = dis_new_dis_TokenList();
    tmp_tl->item = dis_copy_dis_Token( (*n)->parse_vars->name );
    if ( (*n)->parse_vars->type->next ) {
      nn = 1 + strlen(dis_EITHER_STR);
      for (tl=(*n)->parse_vars->type;tl;tl=tl->next)
      {
        nn += strlen(dis_CONNECTdis_OR);
        nn += strlen(tl->item);
      }
      tmp_tl->next->item = dis_new_dis_Token( nn );
      strcpy( tmp_tl->next->item, dis_EITHER_STR );
      for ( tl = (*n)->parse_vars->type; tl; tl = tl->next ) {
	strcat( tmp_tl->next->item, dis_CONNECTdis_OR );
	strcat( tmp_tl->next->item, tl->item );
      }
    } else {
      tmp_tl->next->item = dis_copy_dis_Token( (*n)->parse_vars->type->item );
    }
    (*n)->atom = tmp_tl;
    /* now add list of sons
     */
    sons = (*n)->sons;
    p_pl = *n;
    for ( tyl = (*n)->parse_vars->next; tyl; tyl = tyl->next ) {
      tmp_tl = dis_new_dis_TokenList();
      tmp_tl->next = dis_new_dis_TokenList();
      tmp_tl->item = dis_copy_dis_Token( tyl->name );
      if ( tyl->type->next ) {
        nn = 1 + strlen(dis_EITHER_STR);
        for (tl=tyl->type;tl;tl=tl->next)
        {
          nn += strlen(dis_CONNECTdis_OR);
          nn += strlen(tl->item);
        }
        tmp_tl->next->item = dis_new_dis_Token( nn );
	strcpy( tmp_tl->next->item, dis_EITHER_STR );
	for ( tl = tyl->type; tl; tl = tl->next ) {
	  strcat( tmp_tl->next->item, dis_CONNECTdis_OR );
	  strcat( tmp_tl->next->item, tl->item );
	}
      } else {
	tmp_tl->next->item = dis_copy_dis_Token( tyl->type->item );
      }
      tmp_pl = dis_new_dis_PlNode( (*n)->connective );
      tmp_pl->atom = tmp_tl;
      p_pl->sons = tmp_pl;
      p_pl = tmp_pl;
    }
    /* remove typed-list-of info
     */
    dis_free_dis_TypedList( (*n)->parse_vars );
    (*n)->parse_vars = NULL;
    /* the last son in list takes over ->sons
     */
    p_pl->sons = sons;
    /* normalize this sons and get out
     */
    dis_normalize_tyl_in_pl( &(p_pl->sons) );
    break;
  case dis_AND:
  case dis_OR:
    for ( i = (*n)->sons; i; i = i->next ) {
      dis_normalize_tyl_in_pl( &i );
    }
    break;
  case dis_NOT:
    dis_normalize_tyl_in_pl( &((*n)->sons) );
    break;
  case dis_ATOM:
  case dis_TRU:
  case dis_FAL:
    break;
  case dis_WHEN:
    dis_normalize_tyl_in_pl( &((*n)->sons) );
    dis_normalize_tyl_in_pl( &((*n)->sons->next) );
    break;
  default:
    break;
  }

}












/* ADL syntax test - and normalization (dis_AND s etc.)
 */












dis_Bool dis_make_adl_domain( void )

{

  dis_Pldis_Operator *i;
  dis_FactList *ff;

  if ( dis_gcmd_line.display_info == 101 ) {
    printf("\noriginal problem parsing is:\n");
    printf("\nobjects:");
    for ( ff = dis_gorig_constant_list; ff; ff = ff->next ) {
      printf("\n%s : %s", ff->item->item, ff->item->next->item);
    }
    printf("\n\ninitial state:\n");
    dis_print_dis_PlNode( dis_gorig_initial_facts, 0 );
    printf("\n\ngoal state:\n");
    dis_print_dis_PlNode( dis_gorig_goal_facts, 0 );
    printf("\n\nops:");
    dis_print_plops( dis_gloaded_ops );
  }

  if ( !dis_make_conjunction_of_atoms( &dis_gorig_initial_facts ) ) {
    printf("\nillegal initial state");
    return dis_FALSE;
  }

  if ( !dis_gorig_goal_facts ) {
    dis_gorig_goal_facts = dis_new_dis_PlNode( dis_TRU );
  }

  if ( !dis_is_wff( dis_gorig_goal_facts ) ) {
    printf("\nillegal goal formula");
    dis_print_dis_PlNode( dis_gorig_goal_facts, 0 );
    return dis_FALSE;
  }

  for ( i = dis_gloaded_ops; i; i = i->next ) {
    if ( !i->preconds ) {
      i->preconds = dis_new_dis_PlNode( dis_TRU );
    }
    if ( !dis_is_wff( i->preconds ) ) {
      printf("\nop %s has illegal precondition", i->name);
      return dis_FALSE;
    }
    if ( !dis_make_effects( &(i->effects) ) ) {
      printf("\nop %s has illegal effects", i->name);
      return dis_FALSE;
    }
  }

  if ( dis_gcmd_line.display_info == 102 ) {
    printf("\nfinal ADL representation is:\n");
    printf("\nobjects:");
    for ( ff = dis_gorig_constant_list; ff; ff = ff->next ) {
      printf("\n%s : %s", ff->item->item, ff->item->next->item);
    }
    printf("\n\ninitial state:\n");
    dis_print_dis_PlNode( dis_gorig_initial_facts, 0 );
    printf("\n\ngoal formula:\n");
    dis_print_dis_PlNode( dis_gorig_goal_facts, 0 );
    printf("\n\nops:");
    dis_print_plops( dis_gloaded_ops );
  }

  return dis_TRUE;
      
}



dis_Bool dis_make_conjunction_of_atoms( dis_PlNode **n )

{

  dis_PlNode *tmp, *i, *p, *m;

  if ( !(*n) ) {
    return dis_TRUE;
  }

  if ( (*n)->connective != dis_AND ) {
    switch ( (*n)->connective ) {
    case dis_ATOM:
      tmp = dis_new_dis_PlNode( dis_ATOM );
      tmp->atom = (*n)->atom;
      (*n)->atom = NULL;
      (*n)->connective = dis_AND;
      (*n)->sons = tmp;
      return dis_TRUE;
    case dis_COMP:
      tmp = dis_new_dis_PlNode( dis_COMP );
      tmp->comp = (*n)->comp;
      tmp->lh = (*n)->lh;
      tmp->rh = (*n)->rh;
      (*n)->lh = NULL;
      (*n)->rh = NULL;
      (*n)->comp = -1;
      (*n)->connective = dis_AND;
      (*n)->sons = tmp;
      return dis_TRUE;
    case dis_NOT:
      dis_free_dis_PlNode( *n );
      (*n) = NULL;
      return dis_TRUE; 
    default:
      return dis_FALSE;
    }
  }

  p = NULL;
  for ( i = (*n)->sons; i; i = i->next ) {
    switch ( i->connective ) {
    /* timed initial literals */
    case dis_TRU:
    	break;
    case dis_ATOM:
      break;
    case dis_COMP:
      break;
    case dis_NOT:
      if ( p ) {
	p->next = i->next;
      } else {
	(*n)->sons = i->next;
      }
      m = i->next;
      i->next = NULL;
      dis_free_dis_PlNode( i );
      i = m;
      break;
    default:
      return dis_FALSE;
    }
    p = i;
  }

  return dis_TRUE;

}



dis_Bool dis_is_wff( dis_PlNode *n )

{

  dis_PlNode *i;

  if ( !n ) {
    return dis_FALSE;
  }

  switch( n->connective ) {
  case dis_ALL:
  case dis_EX:
    if ( !(n->atom) ||
	 !(n->atom->next ) ||
	 n->atom->next->next != NULL ) {
      return dis_FALSE;
    }
    return dis_is_wff( n->sons );
  case dis_AND:
  case dis_OR:
    for ( i = n->sons; i; i = i->next ) {
      if ( !dis_is_wff( i ) ) {
	return dis_FALSE;
      }
    }
    return dis_TRUE;
  case dis_NOT:
  /* durative actions */
  case dis_AT_START_CONN:
  case dis_AT_END_CONN:
  case dis_OVER_ALL_CONN:
    return dis_is_wff( n->sons );
  case dis_ATOM:
    if ( !(n->atom) ||
	 n->sons != NULL ) {
      return dis_FALSE;
    }
    return dis_TRUE;
  case dis_TRU:
  case dis_FAL:
    if ( n->sons != NULL ) {
      return dis_FALSE;
    }
    return dis_TRUE;
  case dis_COMP:
    if ( n->sons != NULL ||
	 n->atom != NULL ||
	 n->lh == NULL ||
	 n->rh == NULL ||
	 n->comp < 0 ) {
      return dis_FALSE;
    }
    return dis_TRUE;
  default:
    return dis_FALSE;
  }

}



dis_Bool dis_make_effects( dis_PlNode **n )

{

  dis_PlNode *tmp, *i, *literals, *j, *k, *next;
  int m = 0;

  if ( (*n)->connective != dis_AND ) {
    if ( !dis_is_eff_literal( *n ) &&
	 (*n)->connective != dis_ALL &&
	 (*n)->connective != dis_WHEN ) {
      return dis_FALSE;
    }
    tmp = dis_new_dis_PlNode( (*n)->connective );
    tmp->atom = (*n)->atom;
    tmp->sons = (*n)->sons;
    tmp->neft = (*n)->neft;
    tmp->lh = (*n)->lh;
    tmp->rh = (*n)->rh;
    (*n)->connective = dis_AND;
    (*n)->sons = tmp;
    (*n)->lh = NULL;
    (*n)->rh = NULL;
    (*n)->neft = -1;
  }

  for ( i = (*n)->sons; i; i = i->next ) {
    if ( dis_is_eff_literal( i ) ) {
      m++;
      continue;
    }
    if ( i->connective == dis_AND ) {
      for ( j = i->sons; j; j = j->next ) {
	if ( !dis_is_eff_literal( j ) ) {
	  return dis_FALSE;
	}
	m++;
      }
      continue;
    }
    if ( i->connective == dis_ALL ) {
      for ( j = i->sons; j && j->connective == dis_ALL; j = j->sons ) {
	if ( !j->atom ||
	     !j->atom->next ||
	     j->atom->next->next != NULL ) {
	  return dis_FALSE;
	}
      }
      if ( !j ) {
	return dis_FALSE;
      }
      if ( dis_is_eff_literal( j ) ) {
	tmp = dis_new_dis_PlNode( dis_AND );
	for ( k = i; k->sons->connective == dis_ALL; k = k->sons );
	k->sons = tmp;
	tmp->sons = j;
	j = tmp;
      }
      if ( j->connective == dis_AND ) {
	for ( k = j->sons; k; k = k->next ) {
	  if ( !dis_is_eff_literal( k ) ) {
	    return dis_FALSE;
	  }
	}
	tmp = dis_new_dis_PlNode( dis_WHEN );
	for ( k = i; k->sons->connective == dis_ALL; k = k->sons );
	k->sons = tmp;
	tmp->sons = dis_new_dis_PlNode( dis_TRU );
	tmp->sons->next = j;
	continue;
      }
      if ( j->connective != dis_WHEN ) {
	return dis_FALSE;
      }
      if ( !(j->sons) ) {
	j->sons = dis_new_dis_PlNode( dis_TRU );
      }
      if ( !dis_is_wff( j->sons ) ) {
	return dis_FALSE;
      }
      if ( !dis_make_conjunction_of_literals( &(j->sons->next) ) ) {
	return dis_FALSE;
      }
      continue;
    }
    if ( i->connective != dis_WHEN ) {
      return dis_FALSE;
    }
    if ( !(i->sons) ) {
      i->sons = dis_new_dis_PlNode( dis_TRU );
    }
    if ( !dis_is_wff( i->sons ) ) {
      return dis_FALSE;
    }
    if ( !dis_make_conjunction_of_literals( &(i->sons->next) ) ) {
      return dis_FALSE;
    }
  }

  if ( m == 0 ) {
    return dis_TRUE;
  }

  tmp = dis_new_dis_PlNode( dis_WHEN );
  tmp->sons = dis_new_dis_PlNode( dis_TRU );
  literals = dis_new_dis_PlNode( dis_AND );
  tmp->sons->next = literals;
  tmp->next = (*n)->sons;
  (*n)->sons = tmp;
  i = (*n)->sons;
  while ( i->next ) {
    if ( dis_is_eff_literal( i->next ) ) {
      next = i->next->next;
      i->next->next = literals->sons;
      literals->sons = i->next;
      i->next = next;
      continue;
    }
    if ( i->next->connective == dis_AND ) {
      next = i->next->next;
      for ( j = i->next->sons; j && j->next; j = j->next );
      if ( j ) {
	j->next = literals->sons;
	literals->sons = i->next->sons;
      }
      i->next = next;
      continue;
    }
    i = i->next;
  }
  return dis_TRUE;

}



dis_Bool dis_is_eff_literal( dis_PlNode *n )

{

  if ( !n ) {
    return dis_FALSE;
  }

  if ( n->connective == dis_NOT ) {
    if ( !n->sons ||
	 n->sons->connective != dis_ATOM ||
	 !n->sons->atom ) {
      return dis_FALSE;
    }
    return dis_TRUE;
  }

  if ( n->connective == dis_ATOM ) {
    if ( !n->atom ) {
      return dis_FALSE;
    }
    return dis_TRUE;
  }

  if ( n->connective == dis_NEF ) {
    if ( !n->lh || 
	 !n->rh ||
	 n->neft < 0 ) {
      return dis_FALSE;
    }
    return dis_TRUE;
  }

  return dis_FALSE;

}



dis_Bool dis_make_conjunction_of_literals( dis_PlNode **n )

{

  dis_PlNode *tmp, *i;

  if ( !(*n) ) {
    return dis_FALSE;
  }

  if ( (*n)->connective != dis_AND ) {
    if ( (*n)->connective == dis_NOT ) {
      if ( !((*n)->sons) ||
	   (*n)->sons->connective != dis_ATOM ) {
	return dis_FALSE;
      }
      tmp = dis_new_dis_PlNode( dis_NOT );
      tmp->sons = (*n)->sons;
      (*n)->connective = dis_AND;
      (*n)->sons = tmp;
      return dis_TRUE;
    }
    if ( (*n)->connective == dis_NEF ) {
      tmp = dis_new_dis_PlNode( dis_NEF );
      tmp->neft = (*n)->neft;
      tmp->lh = (*n)->lh;
      tmp->rh = (*n)->rh;
      (*n)->lh = NULL;
      (*n)->rh = NULL;
      (*n)->neft = -1;
      (*n)->connective = dis_AND;
      (*n)->sons = tmp;
      return dis_TRUE;
    }
    if ( (*n)->connective != dis_ATOM ) {
      return dis_FALSE;
    }
    tmp = dis_new_dis_PlNode( dis_ATOM );
    tmp->atom = (*n)->atom;
    (*n)->atom = NULL;
    (*n)->connective = dis_AND;
    (*n)->sons = tmp;
    return dis_TRUE;
  }

  for ( i = (*n)->sons; i; i = i->next ) {
    if ( !dis_is_eff_literal( i ) ) {
      return dis_FALSE;
    }
  }

  return dis_TRUE;

}


