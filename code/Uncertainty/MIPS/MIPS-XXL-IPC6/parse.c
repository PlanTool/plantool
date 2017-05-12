

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
 * File: parse.c
 * Description: Functions for the pddl parser
 *
 * Author: Joerg Hoffmann 2000
 *
 *********************************************************************/ 






#include "ff.h"

#include "memory.h"
#include "output.h"

#include "parse.h"

#include "inst_pre.h"



#include "grounded.h"
#include <string.h>




/* simple parse helpers
 */







char *copy_Token( char *s )

{

    char *d = new_Token( strlen( s ) + 1 );
    strcpy(d, s);

    return d;

}



TokenList *copy_TokenList( TokenList *source )

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
	temp->next = copy_TokenList( source->next );
    }

    return temp;

}



ParseExpNode *copy_ParseExpNode( ParseExpNode *source )

{

    ParseExpNode *temp;

    if ( !source ) {
	temp = NULL;
    } else {
	temp = new_ParseExpNode(source->connective);
	if ( source->atom ) {
	    temp->atom = copy_TokenList(source->atom );
	}
	temp->leftson = copy_ParseExpNode( source->leftson );
        temp->rightson = copy_ParseExpNode( source->rightson );
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










/* typed-list-of   preprocessing
 */







Token ltype_names[MAX_TYPES];
int lnum_types;


int leither_ty[MAX_TYPES][MAX_TYPES];
int lnum_either_ty[MAX_TYPES];





void build_orig_constant_list( void )

{

    char *tmp = NULL;
    TypedList *tyl;
    TypedListList *tyll;
    TokenList *tl, *p_tl, *tmp_tl;
    PlOperator *po;

    int i, j, k, n, std;

    Bool m[MAX_TYPES][MAX_TYPES];

    FactList *fl, *p_fl;

    lnum_types = 0;
    for ( tyl = gparse_types; tyl; tyl = tyl->next ) {
	if ( get_type( tyl->name ) == -1 ) {
	    ltype_names[lnum_types++] = copy_Token( tyl->name );
	}
	if ( tyl->type->next ) {
	    tmp = new_Token( MAX_LENGTH );
	    strcpy( tmp, EITHER_STR );
	    for ( tl = tyl->type; tl; tl = tl->next ) {
		strcat( tmp, CONNECTOR );
		strcat( tmp, tl->item );
	    }
	} else {
	    tmp = copy_Token( tyl->type->item );
	}
	if ( (n = get_type( tmp )) == -1 ) {
	    tyl->n = lnum_types;
	    ltype_names[lnum_types++] = copy_Token( tmp );
	} else {
	    tyl->n = n;
	}
	free( tmp );
	tmp = NULL;
    }
     
    for ( tyl = gparse_constants; tyl; tyl = tyl->next ) {
	if ( tyl->type->next ) {
	    tmp = new_Token( MAX_LENGTH );
	    strcpy( tmp, EITHER_STR );
	    for ( tl = tyl->type; tl; tl = tl->next ) {
		strcat( tmp, CONNECTOR );
		strcat( tmp, tl->item );
	    }
	} else {
	    tmp = copy_Token( tyl->type->item );
	}
	if ( (n = get_type( tmp )) == -1 ) {
	    tyl->n = lnum_types;
	    ltype_names[lnum_types++] = copy_Token( tmp );
	} else {
	    tyl->n = n;
	}
	free( tmp );
	tmp = NULL;
    }
  
    for ( tyl = gparse_objects; tyl; tyl = tyl->next ) {
	if ( tyl->type->next ) {
	    tmp = new_Token( MAX_LENGTH );
	    strcpy( tmp, EITHER_STR );
	    for ( tl = tyl->type; tl; tl = tl->next ) {
		strcat( tmp, CONNECTOR );
		strcat( tmp, tl->item );
	    }
	} else {
	    tmp = copy_Token( tyl->type->item );
	}
	if ( (n = get_type( tmp )) == -1 ) {
	    tyl->n = lnum_types;
	    ltype_names[lnum_types++] = copy_Token( tmp );
	} else {
	    tyl->n = n;
	}
	free( tmp );
	tmp = NULL;
    }

    for ( tyll = gparse_predicates; tyll; tyll = tyll->next ) {
	for ( tyl = tyll->args; tyl; tyl = tyl->next ) {
	    if ( tyl->type->next ) {
		tmp = new_Token( MAX_LENGTH );
		strcpy( tmp, EITHER_STR );
		for ( tl = tyl->type; tl; tl = tl->next ) {
		    strcat( tmp, CONNECTOR );
		    strcat( tmp, tl->item );
		}
	    } else {
		tmp = copy_Token( tyl->type->item );
	    }
	    if ( (n = get_type( tmp )) == -1 ) {
		tyl->n = lnum_types;
		ltype_names[lnum_types++] = copy_Token( tmp );
	    } else {
		tyl->n = n;
	    }
	    free( tmp );
	    tmp = NULL;
	}
    }
    
    collect_type_names_in_pl( gorig_goal_facts );


    /*for preferences
    **/
    if(isPreference){
	collect_type_names_in_pl( pref_head );
    }

    /*for constraints
    **/
     
    if(isConstraints){
	collect_type_names_in_pl( gorig_constraints_facts );
	collect_type_names_in_pl( til_constraints );
	collect_type_names_in_pl( within_constraints );

	if(isConstraintsPreference){
	    collect_type_names_in_pl( pref_constraints_head );
	    collect_type_names_in_pl( til_constraints_pref );
	    collect_type_names_in_pl( within_constraints_pref );
	}

    } 
    

    for ( po = gloaded_ops; po; po = po->next ) {
	collect_type_names_in_pl( po->preconds );
	collect_type_names_in_pl( po->effects );
	for ( tyl = po->parse_params; tyl; tyl = tyl->next ) {
	    if ( tyl->type->next ) {
		tmp = new_Token( MAX_LENGTH );
		strcpy( tmp, EITHER_STR );
		for ( tl = tyl->type; tl; tl = tl->next ) {
		    strcat( tmp, CONNECTOR );
		    strcat( tmp, tl->item );
		}
	    } else {
		tmp = copy_Token( tyl->type->item );
	    }
	    if ( (n = get_type( tmp )) == -1 ) {
		tyl->n = lnum_types;
		ltype_names[lnum_types++] = copy_Token( tmp );
	    } else {
		tyl->n = n;
	    }
	    free( tmp );
	    tmp = NULL;
	}
    }


    /* now get the numbers of all composed either types
     */
    for ( i = 0; i < lnum_types; i++ ) {
	lnum_either_ty[i] = 0;
    }
    for ( tyl = gparse_types; tyl; tyl = tyl->next ) {
	make_either_ty( tyl );
    }
    for ( tyl = gparse_constants; tyl; tyl = tyl->next ) {
	make_either_ty( tyl );
    }
    for ( tyl = gparse_objects; tyl; tyl = tyl->next ) {
	make_either_ty( tyl );
    }
    for ( tyll = gparse_predicates; tyll; tyll = tyll->next ) {
	for ( tyl = tyll->args; tyl; tyl = tyl->next ) {
	    make_either_ty( tyl );
	}
    }
    make_either_ty_in_pl( gorig_goal_facts );

    /*for preference
    **/
    if(isPreference){
	make_either_ty_in_pl( pref_head );
    }

    /*for constraints
    **/
    if(isConstraints){
	make_either_ty_in_pl( gorig_constraints_facts );
	make_either_ty_in_pl( til_constraints );
        make_either_ty_in_pl( within_constraints );

	if(isConstraintsPreference){
	    make_either_ty_in_pl( pref_constraints_head );
	    make_either_ty_in_pl( til_constraints_pref );
            make_either_ty_in_pl( within_constraints_pref );
	}
    }


    for ( po = gloaded_ops; po; po = po->next ) {
	make_either_ty_in_pl( po->preconds );
	make_either_ty_in_pl( po->effects );
	for ( tyl = po->parse_params; tyl; tyl = tyl->next ) {
	    make_either_ty( tyl );
	}
    }


    /* now, compute the transitive closure of all type inclusions.
     * first initialize the matrix.
     */
    for ( i = 0; i < lnum_types; i++ ) {
	for ( j = 0; j < lnum_types; j++ ) {
	    m[i][j] = ( i == j ? TRUE : FALSE );
	}
    }
    std = -1;
    for ( i = 0; i < lnum_types; i++ ) {
	if ( strcmp( ltype_names[i], STANDARD_TYPE ) == SAME ) {
	    std = i;
	    break;
	}
    }
    for ( i = 0; i < lnum_types; i++ ) {
	m[i][std] = TRUE;/* all types are subtypes of OBJECT */
    }
    for ( tyl = gparse_types; tyl; tyl = tyl->next ) {
	/* all inclusions as are defined in domain file
	 */
	m[get_type( tyl->name )][tyl->n] = TRUE;
    }
    /* compute transitive closure on inclusions matrix
     */
    for ( j = 0; j < lnum_types; j++ ) {
	for ( i = 0; i < lnum_types; i++ ) {
	    if ( m[i][j] ) {
		for ( k = 0; k < lnum_types; k++ ) {
		    if ( m[j][k] ) {
			m[i][k] = TRUE;
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
	    m[i][j] = TRUE;
	    /* make components subtypes of either type
	     */
	    for ( k = 0; k < lnum_either_ty[i]; k++ ) {
		m[leither_ty[i][k]][i] = TRUE;
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
			m[i][k] = TRUE;
		    }
		}
	    }
	}
    }
  

    /* now build FactList of ALL  constant -> type   pairs.
     * for each constant / object, let it appear separately
     * for each type it is a member of; compute type
     * membership based on propagating constants / objects
     * through inclusions matrix.
     *
     * this might make the same pair appear doubly, if an object
     * is declared in type T as well as in some supertype T'.
     * such cases will be filtered out in string collection.
     */
    for ( tyl = gparse_constants; tyl; tyl = tyl->next ) {
	fl = new_FactList();
	fl->item = new_TokenList();
	fl->item->next = new_TokenList();
	fl->item->item = copy_Token( tyl->name );
	if ( tyl->type->next ) {
	    fl->item->next->item = new_Token( MAX_LENGTH );
	    strcpy( fl->item->next->item, EITHER_STR );
	    for ( tl = tyl->type; tl; tl = tl->next ) {
		strcat( fl->item->next->item, CONNECTOR );
		strcat( fl->item->next->item, tl->item );
	    }
	} else {
	    fl->item->next->item = copy_Token( tyl->type->item );
	}
	fl->next = gorig_constant_list;
	gorig_constant_list = fl;
	/* now add constant to all supertypes
	 */
	n = get_type( fl->item->next->item );
	for ( i = 0; i < lnum_types; i++ ) {
	    if ( i == n ||
		 !m[n][i] ) continue;
	    fl = new_FactList();
	    fl->item = new_TokenList();
	    fl->item->next = new_TokenList();
	    fl->item->item = copy_Token( tyl->name );
	    fl->item->next->item = copy_Token( ltype_names[i] );
	    fl->next = gorig_constant_list;
	    gorig_constant_list = fl;
	}
    }
    for ( tyl = gparse_objects; tyl; tyl = tyl->next ) {
	fl = new_FactList();
	fl->item = new_TokenList();
	fl->item->next = new_TokenList();
	fl->item->item = copy_Token( tyl->name );
	if ( tyl->type->next ) {
	    fl->item->next->item = new_Token( MAX_LENGTH );
	    strcpy( fl->item->next->item, EITHER_STR );
	    for ( tl = tyl->type; tl; tl = tl->next ) {
		strcat( fl->item->next->item, CONNECTOR );
		strcat( fl->item->next->item, tl->item );
	    }
	} else {
	    fl->item->next->item = copy_Token( tyl->type->item );
	}
	fl->next = gorig_constant_list;
	gorig_constant_list = fl;
	/* now add constant to all supertypes
	 */
	n = get_type( fl->item->next->item );
	for ( i = 0; i < lnum_types; i++ ) {
	    if ( i == n ||
		 !m[n][i] ) continue;
	    fl = new_FactList();
	    fl->item = new_TokenList();
	    fl->item->next = new_TokenList();
	    fl->item->item = copy_Token( tyl->name );
	    fl->item->next->item = copy_Token( ltype_names[i] );
	    fl->next = gorig_constant_list;
	    gorig_constant_list = fl;
	}
    }


    /* now, normalize all typed-list-of  s in domain and problem def,
     * i.e., in all PlNode quantifiers and in op parameters
     *
     * at the same time, remove typed-listof structures in these defs
     */
     normalize_tyl_in_pl( &gorig_goal_facts );

     /*for preference
     **/
     if(isPreference){
	 normalize_tyl_in_pl(&pref_head );
     }

     /*for constraints
     **/
     if(isConstraints){
	 normalize_tyl_in_pl( &gorig_constraints_facts );

	 normalize_tyl_in_pl( &til_constraints );
         normalize_tyl_in_pl( &within_constraints );

	if(isConstraintsPreference){
	    normalize_tyl_in_pl( &pref_constraints_head );
	    normalize_tyl_in_pl( &til_constraints_pref );
            normalize_tyl_in_pl( &within_constraints_pref );
	}
    }

     for ( po = gloaded_ops; po; po = po->next ) {
	normalize_tyl_in_pl( &po->preconds);
	normalize_tyl_in_pl( &po->effects );
	/* be careful to maintain parameter ordering !
	 */
	if ( !po->parse_params ) {
	    continue;/* no params at all */
	}
	fl = new_FactList();
	fl->item = new_TokenList();
	fl->item->next = new_TokenList();
	fl->item->item = copy_Token( po->parse_params->name );
	if ( po->parse_params->type->next ) {
	    fl->item->next->item = new_Token( MAX_LENGTH );
	    strcpy( fl->item->next->item, EITHER_STR );
	    for ( tl = po->parse_params->type; tl; tl = tl->next ) {
		strcat( fl->item->next->item, CONNECTOR );
		strcat( fl->item->next->item, tl->item );
	    }
	} else {
	    fl->item->next->item = copy_Token( po->parse_params->type->item );
	}
	po->params = fl;
	p_fl = fl;
	for ( tyl = po->parse_params->next; tyl; tyl = tyl->next ) {
	    fl = new_FactList();
	    fl->item = new_TokenList();
	    fl->item->next = new_TokenList();
	    fl->item->item = copy_Token( tyl->name );
	    if ( tyl->type->next ) {
		fl->item->next->item = new_Token( MAX_LENGTH );
		strcpy( fl->item->next->item, EITHER_STR );
		for ( tl = tyl->type; tl; tl = tl->next ) {
		    strcat( fl->item->next->item, CONNECTOR );
		    strcat( fl->item->next->item, tl->item );
		}
	    } else {
		fl->item->next->item = copy_Token( tyl->type->item );
	    }
	    p_fl->next = fl;
	    p_fl = fl;
	}
	free_TypedList( po->parse_params );
	po->parse_params = NULL;
    }


    /* finally, build  gpredicates_and_types  by chaining predicate names 
     * together with the names of their args' types.
     */
    for ( tyll = gparse_predicates; tyll; tyll = tyll->next ) {
	fl = new_FactList();
	fl->item = new_TokenList();
	fl->item->item = copy_Token( tyll->predicate );
	fl->next = gpredicates_and_types;
	gpredicates_and_types = fl;
	if ( !tyll->args ) continue;
	/* add arg types; MAINTAIN ORDERING !
	 */
	fl->item->next = new_TokenList();
	if ( tyll->args->type->next ) {
	    fl->item->next->item = new_Token( MAX_LENGTH );
	    strcpy( fl->item->next->item, EITHER_STR );
	    for ( tl = tyll->args->type; tl; tl = tl->next ) {
		strcat( fl->item->next->item, CONNECTOR );
		strcat( fl->item->next->item, tl->item );
	    }
	} else {
	    fl->item->next->item = copy_Token( tyll->args->type->item );
	}
	p_tl = fl->item->next;
	for ( tyl = tyll->args->next; tyl; tyl = tyl->next ) {
	    tmp_tl = new_TokenList();
	    if ( tyl->type->next ) {
		tmp_tl->item = new_Token( MAX_LENGTH );
		strcpy( tmp_tl->item, EITHER_STR );
		for ( tl = tyl->type; tl; tl = tl->next ) {
		    strcat( tmp_tl->item, CONNECTOR );
		    strcat( tmp_tl->item, tl->item );
		}
	    } else {
		tmp_tl->item = copy_Token( tyl->type->item );
	    }
	    p_tl->next = tmp_tl;
	    p_tl = tmp_tl;
	}
    }

    for ( tyll = gparse_functions; tyll; tyll = tyll->next ) {
	fl = new_FactList();
	fl->item = new_TokenList();
	fl->item->item = copy_Token( tyll->predicate );
	fl->next = gfunctions_and_types;
	gfunctions_and_types = fl;
	if ( !tyll->args ) continue;
	/* add arg types; MAINTAIN ORDERING !
	 */
	fl->item->next = new_TokenList();
	if ( tyll->args->type->next ) {
	    fl->item->next->item = new_Token( MAX_LENGTH );
	    strcpy( fl->item->next->item, EITHER_STR );
	    for ( tl = tyll->args->type; tl; tl = tl->next ) {
		strcat( fl->item->next->item, CONNECTOR );
		strcat( fl->item->next->item, tl->item );
	    }
	} else {
	    fl->item->next->item = copy_Token( tyll->args->type->item );
	}
	p_tl = fl->item->next;
	for ( tyl = tyll->args->next; tyl; tyl = tyl->next ) {
	    tmp_tl = new_TokenList();
	    if ( tyl->type->next ) {
		tmp_tl->item = new_Token( MAX_LENGTH );
		strcpy( tmp_tl->item, EITHER_STR );
		for ( tl = tyl->type; tl; tl = tl->next ) {
		    strcat( tmp_tl->item, CONNECTOR );
		    strcat( tmp_tl->item, tl->item );
		}
	    } else {
		tmp_tl->item = copy_Token( tyl->type->item );
	    }
	    p_tl->next = tmp_tl;
	    p_tl = tmp_tl;
	}
    }

    /* now get rid of remaining typed-list-of parsing structures
     */
    free_TypedList( gparse_types );
    gparse_types = NULL;
    free_TypedList( gparse_constants );
    gparse_constants = NULL;
    free_TypedList( gparse_objects );
    gparse_objects = NULL;
    free_TypedListList( gparse_predicates );
    gparse_predicates = NULL;
    free_TypedListList( gparse_functions );
    gparse_functions = NULL;

}



void collect_type_names_in_pl( PlNode *n )

{

    PlNode *i;
    TypedList *tyl;
    TokenList *tl;
    char *tmp = NULL;
    int nn;

    if ( !n ) {
	return;
    }



    switch( n->connective ) {
	case ALL:
	case EX:
	    for ( tyl = n->parse_vars; tyl; tyl = tyl->next ) {
		if ( tyl->type->next ) {
        
		    tmp = new_Token( MAX_LENGTH );
		    strcpy( tmp, EITHER_STR );
		    for ( tl = tyl->type; tl; tl = tl->next ) {
			strcat( tmp, CONNECTOR );
			strcat( tmp, tl->item );
		    }
		} else {
		    tmp = copy_Token( tyl->type->item );
		}
		if ( (nn = get_type( tmp )) == -1 ) {
		    tyl->n = lnum_types;
		    ltype_names[lnum_types++] = copy_Token( tmp );
		} else {
		    tyl->n = nn;
		}
		free( tmp );
		tmp = NULL;
	    }
	    collect_type_names_in_pl( n->sons );
	    break;
	case AND:
	case OR:
	    for ( i = n->sons; i; i = i->next ) {
		collect_type_names_in_pl( i );
	    }
	    break;
	case NOT:
	    collect_type_names_in_pl( n->sons );
	    break;
	case ATOM:
	case TRU:
	case FAL:
	    break;
	case TEMPORALOP:
            collect_type_names_in_pl( n->sons );
            if((n->temporal_op==SOMETIME_AFTER) || (n->temporal_op==SOMETIME_BEFORE) || (n->temporal_op==ALWAYS_WITHIN))
            collect_type_names_in_pl ( n->sons->next );
           break;
	case WHEN:
	    collect_type_names_in_pl( n->sons );
	    collect_type_names_in_pl( n->sons->next );
	    break;
	default:
	    break;
    }

}




int get_type( char *str )

{

    int i;

    for ( i = 0; i < lnum_types; i++ ) {
	if ( strcmp( str, ltype_names[i] ) == SAME ) return i;
    }

    return -1;

}



void make_either_ty( TypedList *tyl )

{

    TokenList *i;

    if ( lnum_either_ty[tyl->n] > 0 ) {
	return;
    }

    for ( i = tyl->type; i; i = i->next ) {
	leither_ty[tyl->n][lnum_either_ty[tyl->n]++] = get_type( i->item );
    }

}



void make_either_ty_in_pl( PlNode *n )

{

    PlNode *i;
    TypedList *tyl;

    if ( !n ) {
	return;
    }

    switch( n->connective ) {
	case ALL:
	case EX:
	    for ( tyl = n->parse_vars; tyl; tyl = tyl->next ) {
		make_either_ty( tyl );
	    }
	    make_either_ty_in_pl( n->sons );
	    break;
	case AND:
	case OR:
	    for ( i = n->sons; i; i = i->next ) {
		make_either_ty_in_pl( i );
	    }
	    break;
	case NOT:
	    make_either_ty_in_pl( n->sons );
	    break;
	case ATOM:
	case TRU:
	case FAL:
	    break;
        case TEMPORALOP:
            make_either_ty_in_pl( n->sons );
            if((n->temporal_op==SOMETIME_AFTER) || (n->temporal_op==SOMETIME_BEFORE) || (n->temporal_op==ALWAYS_WITHIN))
            make_either_ty_in_pl ( n->sons->next);
            break;
	case WHEN:
	    make_either_ty_in_pl( n->sons );
	    make_either_ty_in_pl( n->sons->next );
	    break;
	default:
	    break;
    }

}



void normalize_tyl_in_pl( PlNode **n )

{

    PlNode *i;
    TypedList *tyl;
    PlNode *tmp_pl = NULL, *sons, *p_pl;
    TokenList *tmp_tl, *tl;


    if ( !(*n) ) {
	return;
    }


    switch( (*n)->connective ) {
	case ALL:
	case EX:
	   
	    /* we need to make a sequence of quantifiers ( ->sons ...)
	     * out of the given sequence of TypedList  elements,
	     * with connected type names, var - name in TokenList
	     * and KEEPING THE SAME ORDERING !!
	     */
	    if ( !(*n)->parse_vars ) {
		printf("\n\nquantifier without argument !! check input files.\n\n");
		exit( 1 );
	    }
	    tmp_tl = new_TokenList();
	    tmp_tl->next = new_TokenList();
	    tmp_tl->item = copy_Token( (*n)->parse_vars->name );
	    if ( (*n)->parse_vars->type->next ) {
		tmp_tl->next->item = new_Token( MAX_LENGTH );
		strcpy( tmp_tl->next->item, EITHER_STR );
		for ( tl = (*n)->parse_vars->type; tl; tl = tl->next ) {
		    strcat( tmp_tl->next->item, CONNECTOR );
		    strcat( tmp_tl->next->item, tl->item );
		}
	    } else {
		tmp_tl->next->item = copy_Token( (*n)->parse_vars->type->item );
	    }
	    (*n)->atom = tmp_tl;

           
	    /* now add list of sons
	     */
	    sons = (*n)->sons;
	    p_pl = *n;

	    for ( tyl = (*n)->parse_vars->next; tyl; tyl = tyl->next ) {
		tmp_tl = new_TokenList();
		tmp_tl->next = new_TokenList();
		tmp_tl->item = copy_Token( tyl->name );
		if ( tyl->type->next ) {
                     
		    tmp_tl->next->item = new_Token( MAX_LENGTH );
		    strcpy( tmp_tl->next->item, EITHER_STR );
		    for ( tl = tyl->type; tl; tl = tl->next ) {
			strcat( tmp_tl->next->item, CONNECTOR );
			strcat( tmp_tl->next->item, tl->item );
		    }
		} else {                    

		    tmp_tl->next->item = copy_Token( tyl->type->item );
		}
		tmp_pl = new_PlNode( (*n)->connective );
		tmp_pl->atom = tmp_tl;

                /* for preference
		**/
                if((*n)->name){
                    tmp_pl->name = ( char * ) calloc( 20, sizeof( char ) );
	            strcpy(tmp_pl->name, (*n)->name);          
                 }

		p_pl->sons = tmp_pl;
		p_pl = tmp_pl;
	    }
	    /* remove typed-list-of info
	     */
	    free_TypedList( (*n)->parse_vars );
	    (*n)->parse_vars = NULL;

	    /* the last son in list takes over ->sons
	     */
	    p_pl->sons = sons;
	    /* normalize this sons and get out
	     */
	    normalize_tyl_in_pl( &(p_pl->sons) );
	    break;
	case AND:
	case OR:
	    for ( i = (*n)->sons; i; i = i->next ) {
		normalize_tyl_in_pl( &i );
	    }
	    break;
	case NOT:
	    normalize_tyl_in_pl( &((*n)->sons) );
	    break;
	case ATOM:
	case TRU:
	case FAL:
	    break;
        case TEMPORALOP:
            normalize_tyl_in_pl( &((*n)->sons) );
            if(((*n)->temporal_op==SOMETIME_AFTER) || ((*n)->temporal_op==SOMETIME_BEFORE) || ((*n)->temporal_op==ALWAYS_WITHIN))
            normalize_tyl_in_pl ( &((*n)->sons->next) );
            break;
	case WHEN:
	    normalize_tyl_in_pl( &((*n)->sons) );
	    normalize_tyl_in_pl( &((*n)->sons->next) );
	    break;
	default:
	    break;
    }

}












/* ADL syntax test - and normalization (AND s etc.)
 */












Bool make_adl_domain( void )

{

    PlOperator *i;
    FactList *ff;

    if ( gcmd_line.display_info == 101 ) {
	printf("\noriginal problem parsing is:\n");
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

 
    
    if ( !make_conjunction_of_atoms( &gorig_initial_facts ) ) {
	printf("\nillegal initial state");
	return FALSE;
    }

    if ( !gorig_goal_facts ) {
	gorig_goal_facts = new_PlNode( TRU );
    }

 
    if ( !is_wff( gorig_goal_facts ) ) {
	printf("\nillegal goal formula");
	print_PlNode( gorig_goal_facts, 0 );
	return FALSE;
    }

    for ( i = gloaded_ops; i; i = i->next ) {
	if ( !i->preconds ) {
	    i->preconds = new_PlNode( TRU );
	}
	if ( !is_wff( i->preconds ) ) {
	    printf("\nop %s has illegal precondition", i->name);
	    return FALSE;
	}
	if ( !make_effects( &(i->effects) ) ) {
	    printf("\nop %s has illegal effects", i->name);
	    return FALSE;
	}
    }

    if ( gcmd_line.display_info == 102 ) {
	printf("\nfinal ADL representation is:\n");
	printf("\nobjects:");
	for ( ff = gorig_constant_list; ff; ff = ff->next ) {
	    printf("\n%s : %s", ff->item->item, ff->item->next->item);
	}
	printf("\n\ninitial state:\n");
	print_PlNode( gorig_initial_facts, 0 );
	printf("\n\ngoal formula:\n");
	print_PlNode( gorig_goal_facts, 0 );
	printf("\n\nops:");
	print_plops( gloaded_ops );
    }

    return TRUE;
      
}



Bool make_conjunction_of_atoms( PlNode **n )

{

    PlNode *tmp, *i, *p, *m;

    if ( !(*n) ) {
	return TRUE;
    }

    if ( (*n)->connective != AND ) {
	switch ( (*n)->connective ) {
	    case ATOM:
		tmp = new_PlNode( ATOM );
		tmp->atom = (*n)->atom;
		(*n)->atom = NULL;
		(*n)->connective = AND;
		(*n)->sons = tmp;
		return TRUE;
	    case COMP:
		tmp = new_PlNode( COMP );
		tmp->comp = (*n)->comp;
		tmp->lh = (*n)->lh;
		tmp->rh = (*n)->rh;
		(*n)->lh = NULL;
		(*n)->rh = NULL;
		(*n)->comp = -1;
		(*n)->connective = AND;
		(*n)->sons = tmp;
		return TRUE;
	    case NOT:
		free_PlNode( *n );
		(*n) = NULL;
		return TRUE; 
	    default:
		return FALSE;
	}
    }

    p = NULL;
    i = (*n)->sons;
    while ( i ) {
	switch ( i->connective ) {
	    case ATOM:
		break;
	    case COMP:
		break;
	    case NOT:
		if ( p ) {
		    p->next = i->next;
		} else {
		    (*n)->sons = i->next;
		}
		m = i->next;
		i->next = NULL;
		free_PlNode( i );
		i = m;
		break;
	    default:
		return FALSE;
	}
	if ( i->connective != NOT ) {
	    p = i;
	    i = i->next;
	}
    }

    return TRUE;

}



Bool is_wff( PlNode *n )

{

    PlNode *i;

    if ( !n ) {
	return FALSE;
    }

    switch( n->connective ) {
	case ALL:
	case EX:
	     if ((!n->parse_vars ) && ( !n->atom)) {
		return FALSE;
		}
	     /* if ( !(n->atom) ||
	       !(n->atom->next ) ||
	       n->atom->next->next != NULL || !(n->parse_vars) ) {
	       return FALSE;
	       }*/
	    return is_wff( n->sons );
	case AND:
	case OR:
	    for ( i = n->sons; i; i = i->next ) {
		if ( !is_wff( i ) ) {
		    return FALSE;
		}
	    }
	    return TRUE;
	case NOT:
	    return is_wff( n->sons );
	case pref:
	    return is_wff( n->sons );
	case ATOM:
	    if ( !(n->atom) ||
		 n->sons != NULL ) {
		return FALSE;
	    }
	    return TRUE;
	case TRU:
	case FAL:
	    if ( n->sons != NULL ) {
		return FALSE;
	    }
	    return TRUE;
	case COMP:
             
	    if ( n->sons != NULL ||
		 n->atom != NULL ||
		 n->lh == NULL ||
		 n->rh == NULL ||
		 n->comp < 0 ) {
		return FALSE;
	    }
	    return TRUE;
	default:
	    return FALSE;
    }

}



Bool make_effects( PlNode **n )

{

    PlNode *tmp, *i, *literals, *j, *k, *next;
    int m = 0;

    if ( (*n)->connective != AND ) {
	if ( !is_eff_literal( *n ) &&
	     (*n)->connective != ALL &&
	     (*n)->connective != WHEN ) {
	    return FALSE;
	}
	tmp = new_PlNode( (*n)->connective );
	tmp->atom = (*n)->atom;
	tmp->sons = (*n)->sons;
	tmp->neft = (*n)->neft;
	tmp->lh = (*n)->lh;
	tmp->rh = (*n)->rh;
	(*n)->connective = AND;
	(*n)->sons = tmp;
	(*n)->lh = NULL;
	(*n)->rh = NULL;
	(*n)->neft = -1;
    }

    for ( i = (*n)->sons; i; i = i->next ) {
	if ( is_eff_literal( i ) ) {
	    m++;
	    continue;
	}
	if ( i->connective == AND ) {
	    for ( j = i->sons; j; j = j->next ) {
		if ( !is_eff_literal( j ) ) {
		    return FALSE;
		}
		m++;
	    }
	    continue;
	}
	if ( i->connective == ALL ) {
	    for ( j = i->sons; j && j->connective == ALL; j = j->sons ) {
		if ( !j->atom ||
		     !j->atom->next ||
		     j->atom->next->next != NULL ) {
		    return FALSE;
		}
	    }
	    if ( !j ) {
		return FALSE;
	    }
	    if ( is_eff_literal( j ) ) {
		tmp = new_PlNode( AND );
		for ( k = i; k->sons->connective == ALL; k = k->sons );
		k->sons = tmp;
		tmp->sons = j;
		j = tmp;
	    }
	    if ( j->connective == AND ) {
		for ( k = j->sons; k; k = k->next ) {
		    if ( !is_eff_literal( k ) ) {
			return FALSE;
		    }
		}
		tmp = new_PlNode( WHEN );
		for ( k = i; k->sons->connective == ALL; k = k->sons );
		k->sons = tmp;
		tmp->sons = new_PlNode( TRU );
		tmp->sons->next = j;
		continue;
	    }
	    if ( j->connective != WHEN ) {
		return FALSE;
	    }
	    if ( !(j->sons) ) {
		j->sons = new_PlNode( TRU );
	    }
	    if ( !is_wff( j->sons ) ) {
		return FALSE;
	    }
	    if ( !make_conjunction_of_literals( &(j->sons->next) ) ) {
		return FALSE;
	    }
	    continue;
	}
	if ( i->connective != WHEN ) {
	    return FALSE;
	}
	if ( !(i->sons) ) {
	    i->sons = new_PlNode( TRU );
	}
	if ( !is_wff( i->sons ) ) {
	    return FALSE;
	}
	if ( !make_conjunction_of_literals( &(i->sons->next) ) ) {
	    return FALSE;
	}
    }

    if ( m == 0 ) {
	return TRUE;
    }

    tmp = new_PlNode( WHEN );
    tmp->sons = new_PlNode( TRU );
    literals = new_PlNode( AND );
    tmp->sons->next = literals;
    tmp->next = (*n)->sons;
    (*n)->sons = tmp;
    i = (*n)->sons;
    while ( i->next ) {
	if ( is_eff_literal( i->next ) ) {
	    next = i->next->next;
	    i->next->next = literals->sons;
	    literals->sons = i->next;
	    i->next = next;
	    continue;
	}
	if ( i->next->connective == AND ) {
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
    return TRUE;

}



Bool is_eff_literal( PlNode *n )

{

    if ( !n ) {
	return FALSE;
    }

    if ( n->connective == NOT ) {
	if ( !n->sons ||
	     n->sons->connective != ATOM ||
	     !n->sons->atom ) {
	    return FALSE;
	}
	return TRUE;
    }

    if ( n->connective == ATOM ) {
	if ( !n->atom ) {
	    return FALSE;
	}
	return TRUE;
    }

    if ( n->connective == NEF ) {
	if ( !n->lh || 
	     !n->rh ||
	     n->neft < 0 ) {
	    return FALSE;
	}
	return TRUE;
    }

    return FALSE;

}



Bool make_conjunction_of_literals( PlNode **n )

{

    PlNode *tmp, *i;

    if ( !(*n) ) {
	return FALSE;
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
	if ( (*n)->connective == NEF ) {
	    tmp = new_PlNode( NEF );
	    tmp->neft = (*n)->neft;
	    tmp->lh = (*n)->lh;
	    tmp->rh = (*n)->rh;
	    (*n)->lh = NULL;
	    (*n)->rh = NULL;
	    (*n)->neft = -1;
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
	if ( !is_eff_literal( i ) ) {
	    return FALSE;
	}
    }

    return TRUE;

}







/*for timed initial literals
**/


	                 
void extract_Timed_Initial_Literals( void )

{
    Bool first=TRUE;
    Bool first1=TRUE;
    PlNode *lauf = NULL;
    Time_Ini_Literal *lauf1 = NULL;
  
    if(gorig_initial_facts==NULL || gorig_initial_facts->sons==NULL)
    {
	/*printf(" Timed Initial Literals is empty \n");*/
    }	
    else
    {
	int i=0;

	for(lauf=gorig_initial_facts; (lauf!=NULL)&&((lauf->sons !=NULL && first==TRUE)||(lauf->next!=NULL)) ;i++)
        { 
 	  
	    if((first==TRUE) && (first1==TRUE))
	    {
        	          	
        	             
		if(lauf->sons->connective==AT)
		{
  
        	               	
        	               	      
		    Timed_Initial_Literals=new_Time_Ini_Literal();
		    lauf1=Timed_Initial_Literals;
        	                  	 
		    if(strcmp( lauf->sons->sons->atom->next->item,"not") == SAME)
		    {
			lauf1->tw=new_TimeWindows();
			lauf1->tw->maxtime=atof(lauf->sons->sons->atom->item);
			lauf1->pre=lauf->sons->sons->atom->next->next;
			lauf->sons->sons->atom=NULL;
			lauf->sons=lauf->sons->next;
			first1=FALSE;
			lauf1->num++;
        	                                
        	                          
		    }
		    else
		    {
                                	 	
			lauf1->num++;
			lauf1->pre=lauf->sons->sons->atom->next;
			lauf1->tw=new_TimeWindows();
			lauf1->tw->mintime=atof(lauf->sons->sons->atom->item);
        	                  	       
			lauf->sons->sons->atom=NULL;
        	                            
			lauf->sons=lauf->sons->next;
			first1=FALSE;
										 
        	                               
        	                               
		    }	   
        	                     
        	                    	                    
        	                      
		}
		else
		{
        	               
		    lauf=lauf->sons;
		    first=FALSE;
		}	
	    }    	  
	    else
	    {
        	          	
		if(first==TRUE )
		{
        	                      		
        	                      	
		    if(lauf->sons->connective==AT)
		    {
        	                               	                           
			if(strcmp( lauf->sons->sons->atom->next->item,"not") == SAME)
			{
                                          	        
			    Time_Ini_Literal * Anker=SearchPinListPre(lauf->sons->sons->atom->next->next);
			    if(Anker)
			    {
																
				if(	((Anker->num)%2)==1)
				{
                                                                   
				    TimeWindows  *Anker0=Anker->tw;
				    TimeWindows  *Anker1=NULL;
				    while(Anker0->next)
				    {
					Anker1=Anker0;
					Anker0=Anker0->next;
				    }
				    Anker0->maxtime=atof(lauf->sons->sons->atom->item);
				    if(Anker1)
				    {
																	   
					Anker1->next=NULL;
					TimeWindows  *Anker2=Anker->tw;
                                                                      
					if(Anker2->mintime > Anker0->mintime)
					{
					    Anker0->next=Anker->tw;
					    Anker->tw=Anker0;
					}
					else
					{
					    if(Anker2->next==NULL)
					    {
						Anker2->next=Anker0;
					    }
					    else
					    {
						while( (Anker2->next!=NULL) && (Anker2->next->mintime < Anker0->mintime))
						{
						    Anker2=Anker2->next;
						}
						Anker0->next=Anker2->next;
						Anker2->next=Anker0;

					    }
																	 
					}
				    }
																  
				    lauf->sons->sons->atom=NULL;
				    lauf->sons=lauf->sons->next;
				    Anker->num=(Anker->num)+1;
				}
				else
				{    
				    TimeWindows  *Anker0=Anker->tw;
				    while(Anker0->next)
				    {
					Anker0=Anker0->next;
				    }
				    Anker0->next=new_TimeWindows();
				    Anker0->next->maxtime=atof(lauf->sons->sons->atom->item);
				    lauf->sons->sons->atom=NULL;
				    lauf->sons=lauf->sons->next;
				    Anker->num=(Anker->num)+1;
																   
				}  	     
                                          	                 
                                          		                    
			    }
			    else
			    {
																    
				lauf1->next=new_Time_Ini_Literal();
				lauf1->next->pre=lauf->sons->sons->atom->next->next;
				lauf1->next->tw=new_TimeWindows();
				lauf1->next->tw->maxtime=atof(lauf->sons->sons->atom->item);
				lauf1->next->num++;
                                          		                    
				lauf->sons->sons->atom=NULL;
				lauf->sons=lauf->sons->next;
				lauf1=lauf1->next;
																	
			    }	       
                                          	                
			}
			else
			{
                                	          
			    Time_Ini_Literal * Anker=SearchPinListPre(lauf->sons->sons->atom->next);
			    if(Anker)                        
			    {
																
				if(((Anker->num)%2)==1)
				{
																 
				    TimeWindows  *Anker0=Anker->tw;
				    TimeWindows  *Anker1=NULL;
				    while(Anker0->next)
				    {
					Anker1=Anker0;
					Anker0=Anker0->next;
				    }
				    Anker0->mintime=atof(lauf->sons->sons->atom->item);
															
				    /*print_Timed_Initial_Literals();*/
				    /***************************************************/
				    if(Anker1)
				    {
																	   
					Anker1->next=NULL;
																	  																   
					TimeWindows  *Anker2=Anker->tw;
                                                                      
					if(Anker2->mintime > Anker0->mintime)
					{
																		  
					    Anker0->next=Anker->tw;
					    Anker->tw=Anker0;
					}
					else
					{
																		  
					    if(Anker2->next==NULL)
					    {
						Anker2->next=Anker0;
					    }
					    else
					    {
						while( (Anker2->next!=NULL) && (Anker2->next->mintime < Anker0->mintime))
						{
						    Anker2=Anker2->next;
						}
						Anker0->next=Anker2->next;
						Anker2->next=Anker0;

					    }
																	 
					}
				    }
				    /***************************************************/
				    lauf->sons->sons->atom=NULL;
				    lauf->sons=lauf->sons->next;
				    Anker->num++;
				}
				else
				{
				    TimeWindows  *Anker0=Anker->tw;
				    while(Anker0->next)
				    {
					Anker0=Anker0->next;
				    }
				    Anker0->next=new_TimeWindows();
				    Anker0->next->mintime=atof(lauf->sons->sons->atom->item);
				    lauf->sons->sons->atom=NULL;
				    lauf->sons=lauf->sons->next;
				    Anker->num++;
				}
                                          	                 
                                          		                    
			    }
			    else
			    {
				lauf1->next=new_Time_Ini_Literal();
                                          	                	   
				lauf1->next->pre=lauf->sons->sons->atom->next;
				lauf1->next->tw=new_TimeWindows();
				lauf1->next->tw->mintime=atof(lauf->sons->sons->atom->item);
                                          		                    
                                          		                    
				lauf->sons->sons->atom=NULL;
                                          		                    
				lauf->sons=lauf->sons->next;
				lauf1=lauf1->next;
				lauf1->num++;
			    }	     
			}           	        
        	                            
        	                           
		    }
		    else
		    {
			lauf=lauf->sons;
			first=FALSE;
		    }
        	                           
		}
		else
		{  
        	                       
		    if(first1)
		    {
                                           	
                                          	    	
			if(lauf->next->connective==AT)
			{
                                	                 	
			    Timed_Initial_Literals=new_Time_Ini_Literal();
			    lauf1=Timed_Initial_Literals;
			    if(strcmp( lauf->next->sons->atom->next->item,"not") == SAME)
			    {
                                	               	      
				lauf1->pre=lauf->next->sons->atom->next->next;
				lauf1->tw=new_TimeWindows();
				lauf1->tw->maxtime=atof(lauf->next->sons->atom->item);
				lauf->next->atom=NULL;
				lauf->next=lauf->next->next;
				lauf1->num++;
			    }
			    else
			    {
                                	                    	                             	                    	
				lauf1->pre=lauf->next->sons->atom->next;
				lauf1->tw=new_TimeWindows();
				lauf1->tw->mintime=atof(lauf->next->sons->atom->item);                                	                    	                           	                    	
				lauf->next->atom=NULL;
				lauf->next=lauf->next->next;
				lauf1->num++;
			    }	      	    	
                                	                   	
                                	              	
                                	               
                                	               
			    first1=FALSE;
			}
			else
			{
			    lauf=lauf->next;
			}
                                    
                                	
                                  
		    }  
		    else
		    {
                                          	
                                          	
                                          	
			if(lauf->next->connective==AT)
			{                                    		
			    if(strcmp( lauf->next->sons->atom->next->item,"not") == SAME)
			    {
                                          		            
				Time_Ini_Literal *Anker=SearchPinListPre(lauf->next->sons->atom->next->next);
				if(Anker)
				{
				    if(((Anker->num)%2)==1)
				    {
					TimeWindows  *Anker0=Anker->tw;
					TimeWindows  *Anker1=NULL;
					Anker->num++;
					while(Anker0->next)
					{
					    Anker1=Anker0;
					    Anker0=Anker0->next;
					}
					Anker0->maxtime=atof(lauf->next->sons->atom->item);
					/******************************************/
					if(Anker1)
					{
																	   
					    Anker1->next=NULL;
					    TimeWindows  *Anker2=Anker->tw;
                                                                       
					    if(Anker2->mintime > Anker0->mintime)
					    {
						Anker0->next=Anker->tw;
						Anker->tw=Anker0;
					    }
					    else
					    {
						if(Anker2->next==NULL)
						{
						    Anker2->next=Anker0;
						}
						else
						{
						    while( (Anker2->next!=NULL) && (Anker2->next->mintime < Anker0->mintime))
						    {
							Anker2=Anker2->next;
						    }
						    Anker0->next=Anker2->next;
						    Anker2->next=Anker0;

						}
																	 
					    }
					}
					/********************************************/
					lauf->next->atom=NULL;
					lauf->next=lauf->next->next;
																	
				    }
				    else
				    {
					TimeWindows  *Anker0=Anker->tw;
					while(Anker0->next)
					{
					    Anker0=Anker0->next;
					}
					Anker0->next=new_TimeWindows();
					Anker0->next->maxtime=atof(lauf->next->sons->atom->item);
					lauf->next->atom=NULL;
					lauf->next=lauf->next->next;
					Anker->num++;
				    }
                                                               
                                          		                    
                                          		                    
				}
				else
				{	
				    lauf1->next=new_Time_Ini_Literal();
				    lauf1->next->pre=lauf->next->sons->atom->next->next;
				    lauf1->next->tw=new_TimeWindows();
				    lauf1->next->tw->maxtime=atof(lauf->next->sons->atom->item);
				    lauf->next->atom=NULL;
				    lauf->next=lauf->next->next;
				    lauf1=lauf1->next;
				    lauf1->num++;
				}
			    }
			    else
			    {
                                	               	    	    
				Time_Ini_Literal *Anker=SearchPinListPre(lauf->next->sons->atom->next);
				if(Anker)
				{   
				    if(Anker->num%2==1)
				    {
					TimeWindows  *Anker0=Anker->tw;
					while(Anker0->next)
					{
					    Anker0=Anker0->next;
					}
																																		 
					Anker0->mintime=atof(lauf->next->sons->atom->item);
					lauf->next->atom=NULL;
					lauf->next=lauf->next->next;
					Anker->num++;
				    }
				    else
				    {  
					TimeWindows  *Anker0=Anker->tw;
					while(Anker0->next)
					{
					    Anker0=Anker0->next;
					}
					Anker0->next=new_TimeWindows();
					Anker0->next->mintime=atof(lauf->next->sons->atom->item);
					lauf->next->atom=NULL;
					lauf->next=lauf->next->next;
					Anker->num++;
				    }
                                	               	    	             
                                            		                   
				}
				else
				{ 
				    lauf1->next=new_Time_Ini_Literal();
				    lauf1->next->pre=lauf->next->sons->atom->next;
				    lauf1->next->tw=new_TimeWindows();
				    lauf1->next->tw->mintime=atof(lauf->next->sons->atom->item);
				    lauf->next->atom=NULL;
				    lauf->next=lauf->next->next;
				    lauf1=lauf1->next;
				    lauf1->num++;
				}
			    }
                                          	 
                                          	     
			}
			else
			{
                                          		
			    lauf=lauf->next;
			}	
		    }  
        	          
	    	    
		}
	    }
        }
    }
}



/*+++++++++++++++++++++++++++++++++++++++++*/
      
void setComplementTIL( void )
{
	
    Time_Ini_Literal *lauf = NULL;
    if(Timed_Initial_Literals==NULL)
    {
    /* printf("empty Timed_Initial_Literals");*/
    }		    
    else
    {
	/*printf("\n Ausdruck der intial literal");*/
	for(lauf=Timed_Initial_Literals;lauf!=NULL;lauf=lauf->next)
	{  
				
	    /*print_hidden_TokenList(lauf->pre," ");*/
			
	    TimeWindows *lauf0=lauf->tw;
			
	    if(lauf0->mintime > lauf0->maxtime)
	    { 
		if(lauf0->maxtime > 0)
		{
                     
		    TimeWindows *newObj=new_TimeWindows();
		    newObj->mintime=0;
		    newObj->maxtime=lauf0->maxtime;
		    newObj->next=lauf->tw;
		    lauf->tw=newObj;
		    while(lauf0->next)
		    {
			lauf0->maxtime=lauf0->next->maxtime;
			lauf0=lauf0->next;
		    }
		    lauf0->maxtime=MAX_TIME;
		    lauf->num=lauf->num+2;
					 
		}
		else
		{
		    while(lauf0->next)
		    {
			lauf0->maxtime=lauf0->next->maxtime;
			lauf0=lauf0->next;
		    }
		    lauf0->maxtime=MAX_TIME;
		}
	    }
	    /*printf("MINTIME=    %f und MAXTIME =   %f",lauf0->mintime,lauf0->maxtime);
	      printf("\n");*/
			
	    lauf->numofint=lauf->num/2;
	}
    }
}


/*+++++++++++++++++++++++++++++++++++++++*/

Bool CompareTwoPre(TokenList *t1, TokenList *t2)
{
    TokenList *lauf1=t1;
    TokenList *lauf2=t2;
    if(!(lauf1) || !(lauf2))
	return FALSE;
    if(lauf1 !=NULL && lauf2==NULL)
	return FALSE;
    if(lauf2 !=NULL && lauf1==NULL)
	return FALSE;  
	
	
    while(lauf1!=NULL && lauf2!=NULL)
    {
	if ( strcmp( lauf1->item , lauf2->item) == SAME ) 
	{
	    lauf1=lauf1->next;
	    lauf2=lauf2->next;
	}
	else
	    return FALSE;
	if(lauf1==NULL && lauf2 ==NULL)
	    return TRUE;
    }   

    return FALSE;              
	    
}
Time_Ini_Literal *SearchPinListPre(TokenList *p)
{
    Time_Ini_Literal *lauf=NULL;
    if(Timed_Initial_Literals==NULL)
    {
	/*printf("kein Pointer ist schon gefunden");*/
	return NULL;
    }   
    else
    {
	for(lauf = Timed_Initial_Literals;lauf!=NULL;lauf=lauf->next)
	{
	    if(CompareTwoPre(lauf->pre,p))
	    {
	      
		return lauf;
	    }
	 		
	}
	
	return NULL;
    } 
}

Bool Search_Init_Time_Literals_in_P_Pre(char *s)
{
	
	
    if(Timed_Initial_Literals==NULL)
    {
	/*printf("no Pointer found");*/
	return FALSE;
    }   
    else
    {
	Time_Ini_Literal *lauf=NULL;
	
	for(lauf=Timed_Initial_Literals;lauf!=NULL;lauf=lauf->next)
	{
	    /*printf("\n Die wert von lauf->pre->item = %s ",lauf->pre->item);*/
	    if(strcmp( lauf->pre->item , s) == SAME)
	    {
		return TRUE;
	    }
	 		
	}
	 	
	return FALSE;
    }
}

/***************************************/
/*Bool Search_Init_Time_Literals_in_P_Pre(char *s)

{

Time_Ini_Literal *lauf=NULL;

for(lauf=Timed_Initial_Literals;lauf!=NULL;lauf=lauf->next) {
if ( strcmp( s, lauf->pre->item) == SAME ) return TRUE;
}

return FALSE;

}*/
/******************************************/
void split_Preconds_inPlOperator( PlOperator *plop )
{
    PlOperator *i_plop=NULL;
  

    if ( !plop ) {
	/*printf("none\n");*/
    }
    else
    {
	for ( i_plop = plop; i_plop!=NULL; i_plop = i_plop->next ) 
	{
	    /* printf("\n Name des operator =%s \n",i_plop->name);*/
     	           
	    if(i_plop->preconds)
	    {
		Bool first=TRUE;
		Bool first1=TRUE;
		PlNode *Anker = NULL;
		PlNode *Anker1 = NULL;
				      
	                       
		if(i_plop->preconds->sons==NULL)
		{
		    /* printf(" Preconds is empty \n");*/
		}	
		else
		{
		    int i=0;
		    for(Anker=i_plop->preconds; (Anker!=NULL)&&((Anker->sons !=NULL && first==TRUE)||(Anker->next!=NULL)) ;i++)
		    { 
			/*printf("laufNumemr=%d",i);*/
			if((first==TRUE) && (first1==TRUE))
			{
                                              	
			    if(Anker->sons->connective==ATOM)
			    {
        	                                                      	
        	                                                      	
				if(Search_Init_Time_Literals_in_P_Pre(Anker->sons->atom->item))
				{
				    if(i_plop->ttw_preconds==NULL)
				    {
					i_plop->ttw_preconds=new_PlNode(AND);
					Anker1=i_plop->ttw_preconds;
					Anker1->sons=Anker->sons;
					Anker->sons=Anker->sons->next;
					Anker1->sons->next=NULL;
					Anker1=Anker1->sons;
					first1=FALSE;
        	                                                      	        	        
				    }
        	                                                      	        	
				}
				else
				{
				    /*printf("*****************111111111******************");*/
				    Anker=Anker->sons;
				    first=FALSE;
				}	
        	                                                      	        
        	                                                      	       
        	                                                      	
			    }
			    else
			    {
				Anker=Anker->sons;
				first=FALSE;
			    }	
			}
			else
			{
			    if(first==TRUE )
			    {
				if(Anker->sons->connective==ATOM)
				{
				    if(Search_Init_Time_Literals_in_P_Pre(Anker->sons->atom->item))
				    {
					Anker1->next=Anker->sons;
					Anker->sons=Anker->sons->next;  
					Anker1->next->next=NULL;
					Anker1=Anker1->next;
				    }
				    else
				    {
					Anker=Anker->sons;
					first=FALSE;
				    }
				}
				else
				{
				    Anker=Anker->sons;
				    first=FALSE;
				}      	
			    }
			    else
			    {
				if(first1)
				{
				    if(Anker->next->connective==ATOM)
				    {  
					if(Search_Init_Time_Literals_in_P_Pre(Anker->next->atom->item))
					{
		          	 	                                                       	
					    if(i_plop->ttw_preconds==NULL)
					    {
						/*printf("********************22222222222222**************");*/
						i_plop->ttw_preconds=new_PlNode(AND);
						Anker1=i_plop->ttw_preconds;
						Anker1->sons=Anker->next;
						Anker->next=Anker->next->next;
						Anker1->sons->next=NULL;
						Anker1=Anker1->sons;
						first1=FALSE;
													          	 		                
					    }	
					}
					else
					{
		          	 	                                                       	
					    Anker=Anker->next;
					}		
                                                                         	
				    }
				    else
				    {
                                                                         	
					Anker=Anker->next;
				    }	
                                                                         
				}
				else
				{
				    /*printf("\n coooooooooooooontrol \n");*/
				    if(Anker->next->connective==ATOM)
				    {              
					if(Search_Init_Time_Literals_in_P_Pre(Anker->next->atom->item))
					{
                                                                            	         	
					    Anker1->next=Anker->next;
					    Anker->next=Anker->next->next;
					    Anker1->next->next=NULL;
					    Anker1=Anker1->next;
					}
					else
					{
					    Anker=Anker->next;
					}
				    }
				    else
				    {
					Anker=Anker->next;
				    }
				}		   
                                                          	
			    } 	       	  
			}
                                              
		    }
                                          
		    /*******************************/
		    /*printf("\n test ttt_preconds \n");
		      print_PlNode(i_plop->ttw_preconds,4);
		      printf("\n test preconds \n");
		      print_PlNode(i_plop->preconds, 4);*/
		    /*****************************/
		}
	    }    
	} 
    }
}     




/*********************************************************************
 * extract preferences from goal
 *********************************************************************/ 




PlNode *extract_preference_from_goal( PlNode *p)

{
     
    PlNode *current = NULL;
    PlNode *current2 = NULL;
   
    PlNode *pref_current = NULL;
    PlNode *i_son, *prefnode;
    PlNode *head = NULL;
    Bool changed = FALSE;

    if(p==NULL)
    {
	printf(" goal is empty \n");
    }	
    else
    {
	current=p;

        if(current->connective==AND)
        {
	    head = new_PlNode(AND);
         
	    current2 = current->sons;

	    /*first node
	    **/
	spring:
            if(current2){
		if(current2->name) {   
		    changed = TRUE;
		    pref_current = make_PlNode(current2);

		    prefnode = head;
		    if ( prefnode->sons ) 
			for ( i_son = prefnode->sons; i_son->next!=NULL; i_son = i_son->next );

		    if( !prefnode->sons ){
			prefnode->sons = pref_current;
			pref_current->next = NULL;
		    }
		    else{
                        i_son->next = pref_current;
                        pref_current->next = NULL;
          
		    }

		    current->sons = current2->next;
		    current2 = current->sons;
		}
		else{
		    changed = FALSE;
		    current=current2;
		    current2=current->next;                  
		}
	    }

            /*end for first node */

	    while(current2!=NULL)
	    {
		
		if(!changed){
		    if(current2->name) {               
                
			pref_current = make_PlNode(current2);

			prefnode = head;
			if ( prefnode->sons ) 
			    for ( i_son = prefnode->sons; i_son->next!=NULL; i_son = i_son->next );

			if( !prefnode->sons ){
                      
			    prefnode->sons = pref_current;
			    pref_current->next = NULL;
			}
			else{
			    i_son->next = pref_current;
			    /*pref_current->next = NULL;*/
          
			}

			current->next = current2->next; 
			current2 = current->next;
	     
		    }
		    else{                         		 
			current=current2;
			current2=current->next;
		
		    }
		}
		else{
		    goto spring;
		}

	    }

	}

        else
	{
	    if(current->name) {   
		pref_current = current;
               
		pref_current->next = head;
		head = pref_current;

		free_PlNode(p);
		p = NULL;
	    }
          
        }
    } 

    /* print_PlNode(pref_head, 0);*/

    return head;     

}






PlNode *make_PlNode( PlNode *p )

{

    PlNode *result = new_PlNode(p->connective);
  
    result->temporal_op = p->temporal_op;
    result->atom = p->atom;
    result->parse_vars = p->parse_vars;

    result->comp = p->comp;
    result->neft = p->neft;
    result->lh = p->lh;
    result->rh = p->rh;

    result->sons = p->sons;
    result->next = NULL;
    result->atom_t= p->atom_t;
    result->name = p->name;

    return result;

}



/*********************************************************************
 * mark preference in goal and in constraints node
 *********************************************************************/ 





Bool mark_preference_in_goal( PlNode *p)

{
     
    PlNode *current = NULL;
    PlNode *current2 = NULL;
    PlNode *current3, *current4, *current5, *current6;
    PlNode *i_son, *i_son1;
    Bool found = FALSE;

    if(p==NULL)
    {
	printf("node is empty \n");
    }	
    else
    {
	current=p;

        if(current->connective==AND)
        {
         
	    current2 = current->sons;

	    /*first node
	    **/
            if(current2){
		switch(current2->connective) {   
		 
		    case pref:
                        found = TRUE;
			current3 = current2->next;
			current->sons = current2->sons; 
			current->sons->next = current3;
			break;
		    case EX:
		    case ALL:
			current4 = current2->sons;
			current5 = current2;
			while(current4){
			    if(current4->connective == pref){
				found = TRUE;
			
				for(i_son1 = current2; i_son1 != current4; i_son1 = i_son1->sons){
				    i_son1->name = current4->sons->name;
				}
				current5->sons = current4->sons;
                                break;
			    }
                            else{
                                current5 = current4;
				current4 = current4->sons;
			    }
			}  
			break;
		    default:
			printf("\nmark_preference_in_goal!\n");
		}
	    }

	    /*end for first node */

	    current6 = current->sons;
	    for(i_son = current->sons->next; i_son != NULL; i_son = i_son->next){
             
		switch(i_son->connective) {   
		    case pref:
                        found = TRUE;
			current3 = i_son->next;
			current6->next = i_son->sons; 
			current6 = i_son->sons;
			i_son->sons->next = current3;
			break;
		    case EX:
		    case ALL:
			current4 = i_son->sons;
			current5 = i_son;
			current6 = i_son;
			while(current4){
                    
			    if(current4->connective == pref){
				found = TRUE;
                       
				for(i_son1 = i_son; i_son1 != current4; i_son1 = i_son1->sons){
                            
				    i_son1->name = current4->sons->name;
			
				}
				current5->sons = current4->sons;
                                break;
			    }
                            else{
                                current5 = current4;
				current4 = current4->sons;
			    }
			}  
			break;
		    default:
			current6 = i_son;
	             
		}
	     
	    }
	}

    } 
    return found;
   
}





/*********************************************************************
 * mark at end in constraints node and preference constraints
 *********************************************************************/ 





Bool mark_at_end_in_constraints( PlNode *p)

{
    
    
    PlNode *current = NULL;
    PlNode *current2 = NULL;
    PlNode *current3, *current4, *current5, *current6;
    PlNode *i_son;
    Bool found = FALSE;

    if(p==NULL)
    {
	printf("node is empty \n");
    }	
    else
    {
	current=p;

        if(current->connective==AND)
        {
         
	    current2 = current->sons;

	    /*first node
	    **/
            if(current2){
		if(current2->temporal_op == AEND) {   
                    found = TRUE;
		    current3 = current2->next;
		    current->sons = current2->sons;
		    current->sons->at_end_found = TRUE;
		    if(current2->name)
			current->sons->name = current2->name;
		    current->sons->next = current3;
		}
		else{
		    if(current2->connective == ALL || current2->connective == EX){
			current4 = current2->sons;
			current5 = current2;
			while(current4){
			    if(current4->temporal_op == AEND){
                                found = TRUE;
				current2->at_end_found = TRUE;
				current5->sons = current4->sons;
				if(current4->name)
				    current5->sons->name = current4->name;
                                break;
			    }
                            else{
                                current5 = current4;
				current4 = current4->sons;
			    }
			}  
		    }
		}
	    }

	    /*end for first node */

	    current6 = current->sons;
	    for(i_son = current->sons->next; i_son != NULL; i_son = i_son->next){
             
		if(i_son->temporal_op == AEND) {   
                    found = TRUE;
		    current3 = i_son->next;
		    i_son->sons->at_end_found = TRUE; 
		    if(i_son->name)
			i_son->sons->name = i_son->name;
		    current6->next = i_son->sons;
		    current6 = i_son->sons;
		    i_son->sons->next = current3;
		}
		else{
		    if(i_son->connective == ALL || i_son->connective == EX){
			current4 = i_son->sons;
			current5 = i_son;
			current6 = i_son;
			while(current4){
                    
			    if(current4->temporal_op == AEND){
                                found = TRUE;
				i_son->at_end_found = TRUE;
				current5->sons = current4->sons;
				if(current4->name)
				    current5->sons->name = current4->name;
                                
				break;
			    }
                            else{
                                current5 = current4;
				current4 = current4->sons;
			    }
			}  
		    }
		    else{
			current6 = i_son;
		    }
	             
		}
	     
	    }
	}

    } 
   	
    return found;
   
}





/*********************************************************************
 * extract at end from constraints
 *********************************************************************/ 





PlNode *extract_at_end_from_constraints( PlNode *p)

{
     
    PlNode *current = NULL;
    PlNode *current2 = NULL;
   
    PlNode *pref_current = NULL;
    PlNode *i_son, *prefnode;
    PlNode *head = NULL;
    Bool changed = FALSE;

    if(p==NULL)
    {
	printf(" goal is empty \n");
    }	
    else
    {
	current=p;

        if(current->connective==AND)
        {
	    head = new_PlNode(AND);
         
	    current2 = current->sons;

	    /*first node
	    **/
	spring:
            if(current2){
		if(current2->at_end_found) {   
		    changed = TRUE;
		    pref_current = make_PlNode(current2);

		    prefnode = head;
		    if ( prefnode->sons ) 
			for ( i_son = prefnode->sons; i_son->next!=NULL; i_son = i_son->next );

		    if( !prefnode->sons ){
			prefnode->sons = pref_current;
			pref_current->next = NULL;
		    }
		    else{
                        i_son->next = pref_current;
                        pref_current->next = NULL;
          
		    }

		    current->sons = current2->next;
		    current2 = current->sons;
		}
		else{
		    changed = FALSE;
		    current=current2;
		    current2=current->next;                  
		}
	    }

            /*end for first node */

	    while(current2!=NULL)
	    {
		
		if(!changed){
		    if(current2->at_end_found) {               
                
			pref_current = make_PlNode(current2);

			prefnode = head;
			if ( prefnode->sons ) 
			    for ( i_son = prefnode->sons; i_son->next!=NULL; i_son = i_son->next );

			if( !prefnode->sons ){
                      
			    prefnode->sons = pref_current;
			    pref_current->next = NULL;
			}
			else{
			    i_son->next = pref_current;
			    /*pref_current->next = NULL;*/
          
			}

			current->next = current2->next; 
			current2 = current->next;
	     
		    }
		    else{                         		 
			current=current2;
			current2=current->next;
		
		    }
		}
		else{
		    goto spring;
		}

	    }

	}

        else
	{
	    if(current->at_end_found) {   
		pref_current = current;
               
		pref_current->next = head;
		head = pref_current;

		free_PlNode(p);
		p = NULL;
	    }
          
        }
    } 


    return head;     

}





/*********************************************************************
 * extract at end from constraints
 *********************************************************************/ 


void add_at_end_constraints_to_goal(PlNode *p1, PlNode *p2)

{

    PlNode *i_son;
    
    

    if(p2){
	if(p1){
	    if(p1->connective == AND){
		if(p1->sons){
		    for(i_son = p1->sons; i_son->next!= NULL; i_son = i_son->next);
		    if(p2->connective == AND){
			if(p2->sons)
			    i_son->next = p2->sons;
		    }
		}
		else{
		    p1->sons = p2->sons;

		}

		p2 = NULL;
		free_PlNode(p2);

	    }
	}
      
    }


}


/*********************************************************************
 * extract constraints and constraints preference with hold-after
 * and hold-during. 
 *********************************************************************/ 





PlNode *extract_constraints_for_timed_initial_literal(PlNode *n)

{
     
    PlNode *current = NULL;
    PlNode *current2 = NULL;
   
    PlNode *til_current = NULL;
    PlNode *i_son, *tilnode;
    PlNode *til_constraints_head = NULL;
    Bool changed = FALSE;

    if(n==NULL)
    {
	printf(" constraints is empty \n");
    }	
    else
    {
	current=n;

        if(current->connective==AND)
        {
	    til_constraints_head = new_PlNode(AND);
         
	    current2 = current->sons;

	    /*first node
	    **/

	spring:

	    if(current2){
		if((current2->temporal_op==HOLD_DURING) || (current2->temporal_op==HOLD_AFTER) ) {  /* timed init literal  */	    
		    changed = TRUE;
                   
		    til_current = new_PlNode(TEMPORALOP);
		    til_current->temporal_op = current2->temporal_op;
		    if(current2->name)
			til_current->name = current2->name;

		    if(current2->temporal_op==HOLD_DURING){
			til_current->lh = current2->lh;
			til_current->rh = current2->rh;
		    }
		    else
			til_current->lh = current2->lh;

		    til_current->sons = current2->sons;

		    tilnode = til_constraints_head;
		    if ( tilnode->sons ) 
			for ( i_son = tilnode->sons; i_son->next!=NULL; i_son = i_son->next );

		    if( !tilnode->sons ){
			tilnode->sons = til_current;
			til_current->next = NULL;
		    }
		    else{
                        i_son->next = til_current;
                        til_current->next = NULL;
          
		    }

		    current->sons = current2->next; 
		    current2 = current->sons;
		 
		}
		else{ 
		    /* test, whether current2 is alquantor */
	      
		    if((current2->connective==ALL) || (current2->connective==EX)){
			if((current2->sons->temporal_op==HOLD_DURING) || (current2->sons->temporal_op==HOLD_AFTER) ) {  /* timed init literal  */	  
	      
			    changed = TRUE;
                   
			    til_current = new_PlNode(current2->connective);
			    til_current->parse_vars = current2->parse_vars;
                          

			    til_current->sons = new_PlNode(TEMPORALOP);
			    til_current->sons->temporal_op = current2->sons->temporal_op;
			    if(current2->name){
				til_current->name = current2->name;
			    }
			    if(current2->sons->name){
				til_current->sons->name = current2->sons->name;
			    }


			    if(current2->sons->temporal_op==HOLD_DURING){
				til_current->sons->lh = current2->sons->lh;
				til_current->sons->rh = current2->sons->rh;
			    }
			    else
				til_current->sons->lh = current2->sons->lh;                   

			    til_current->sons->sons = current2->sons->sons;

			    tilnode = til_constraints_head;
			    if ( tilnode->sons ) 
				for ( i_son = tilnode->sons; i_son->next!=NULL; i_son = i_son->next );

			    if( !tilnode->sons ){
				tilnode->sons = til_current;
				til_current->next = NULL;
			    }
			    else{
				i_son->next = til_current;
				til_current->next = NULL;
          
			    }

			    current->sons = current2->next; 
			    current2 = current->sons;
		 
			}

                  
			else{
			    changed = FALSE;
			    current=current2;
			    current2=current->next;
			}
		    } 
		    else{
			changed = FALSE;
			current=current2;
			current2=current->next;
		    }


		}

	    }

            /*end for first node */
          

	    while(current2!=NULL)
	    {
		if(!changed){
     
		    if((current2->temporal_op==HOLD_DURING) || (current2->temporal_op==HOLD_AFTER) ) {  /* timed init literal  */  	  
     
			til_current = new_PlNode(TEMPORALOP);
			til_current->temporal_op = current2->temporal_op;
			if(current2->name)
			    til_current->name = current2->name;

			if(current2->temporal_op==HOLD_DURING){
			    til_current->lh = current2->lh;
			    til_current->rh = current2->rh;
			}
			else
			    til_current->lh = current2->lh;

			til_current->sons = current2->sons;

			tilnode = til_constraints_head;
			if ( tilnode->sons ) 
			    for ( i_son = tilnode->sons; i_son->next!=NULL; i_son = i_son->next );

			if( !tilnode->sons ){
			    tilnode->sons = til_current;
			    til_current->next = NULL;
			}
			else{
			    i_son->next = til_current;
			    til_current->next = NULL;
          
			}
			current->next = current2->next;
			current2 = current->next;
		 
		    }
		    else{       
		   
			if((current2->connective==ALL) || (current2->connective==EX)){
			    if((current2->sons->temporal_op==HOLD_DURING) || (current2->sons->temporal_op==HOLD_AFTER) ) {  /* timed init literal  */  	  
       
				til_current = new_PlNode(current2->connective);
				til_current->parse_vars = current2->parse_vars;
                               

				til_current->sons = new_PlNode(TEMPORALOP);
				til_current->sons->temporal_op = current2->sons->temporal_op;
				if(current2->name){
				    til_current->name = current2->name;
				}
				if(current2->sons->name){
				    til_current->sons->name = current2->sons->name;
				}


				if(current2->sons->temporal_op==HOLD_DURING){
				    til_current->sons->lh = current2->sons->lh;
				    til_current->sons->rh = current2->sons->rh;
				}
				else
				    til_current->sons->lh = current2->sons->lh;                   
				til_current->sons->sons = current2->sons->sons;

				tilnode = til_constraints_head;
				if ( tilnode->sons ) 
				    for ( i_son = tilnode->sons; i_son->next!=NULL; i_son = i_son->next );

				if( !tilnode->sons ){
				    tilnode->sons = til_current;
				    til_current->next = NULL;
				}
				else{
				    i_son->next = til_current;
				    til_current->next = NULL;
          
				}
				current->next = current2->next;
				current2 = current->next;
		 
			    }

			    else{
                  
				current=current2;
				current2=current->next;
                   
			    }
			}
			else
			{
                  
			    current=current2;
			    current2=current->next;
                   
			}


		    }
		}
		else{
		    goto spring;
		}

	    }

	}

        else
	{

	    if((current->temporal_op==HOLD_DURING) || (current->temporal_op==HOLD_AFTER) ) {  /* timed init literal  */

		til_current = current;

		til_current->next = til_constraints_head;
		til_constraints_head = til_current;

		free_PlNode(n);
		n = NULL;

	    }
	    else{
		if((current->connective==ALL) || (current->connective==EX)){
		    if((current->sons->temporal_op==HOLD_DURING) || (current->sons->temporal_op==HOLD_AFTER) ) {  /* timed init literal  */


			til_current = current;

			til_current->next = til_constraints_head;
			til_constraints_head = til_current;

			free_PlNode(n);
			n = NULL;

		    }

		}

	    }
        
	}        

    }

    /* print_PlNode(til_constraints_head, 0);*/
     
    return til_constraints_head;

}





/*********************************************************************
 * extract constraints and constraints preference with within and  
 * always-within. 
 *********************************************************************/ 





PlNode *extract_constraints_for_within(PlNode *n)

{
     
    PlNode *current = NULL;
    PlNode *current2 = NULL;
   
    PlNode *til_current = NULL;
    PlNode *i_son, *tilnode;
    PlNode *til_constraints_head = NULL;
    Bool changed = FALSE;

    if(n==NULL)
    {
	printf(" constraints is empty \n");
    }	
    else
    {
	current=n;

        if(current->connective==AND)
        {
	    til_constraints_head = new_PlNode(AND);
         
	    current2 = current->sons;

	    /*first node
	    **/

	spring:

	    if(current2){
		if((current2->temporal_op==WITHIN)|| (current2->temporal_op==ALWAYS_WITHIN) ) {  	    
		    changed = TRUE;
                   
		    til_current = new_PlNode(TEMPORALOP);
		    til_current->temporal_op = current2->temporal_op;
		    if(current2->name)
			til_current->name = current2->name;

		    til_current->lh = current2->lh;

		    til_current->sons = current2->sons;

		    tilnode = til_constraints_head;
		    if ( tilnode->sons ) 
			for ( i_son = tilnode->sons; i_son->next!=NULL; i_son = i_son->next );

		    if( !tilnode->sons ){
			tilnode->sons = til_current;
			til_current->next = NULL;
		    }
		    else{
                        i_son->next = til_current;
                        til_current->next = NULL;
          
		    }

		    current->sons = current2->next; 
		    current2 = current->sons;
		 
		}
		else{ 
		    /* test, whether current2 is alquantor */
	      
		    if((current2->connective==ALL) || (current2->connective==EX)){
			if((current2->sons->temporal_op==WITHIN) || (current2->sons->temporal_op==ALWAYS_WITHIN)) { 	  
	      
			    changed = TRUE;
                   
			    til_current = new_PlNode(current2->connective);
			    til_current->parse_vars = current2->parse_vars;
                          

			    til_current->sons = new_PlNode(TEMPORALOP);
			    til_current->sons->temporal_op = current2->sons->temporal_op;
			    if(current2->name){
				til_current->name = current2->name;
			    }
			    if(current2->sons->name){
				til_current->sons->name = current2->sons->name;
			    }

			    til_current->sons->lh = current2->sons->lh;                   

			    til_current->sons->sons = current2->sons->sons;

			    tilnode = til_constraints_head;
			    if ( tilnode->sons ) 
				for ( i_son = tilnode->sons; i_son->next!=NULL; i_son = i_son->next );

			    if( !tilnode->sons ){
				tilnode->sons = til_current;
				til_current->next = NULL;
			    }
			    else{
				i_son->next = til_current;
				til_current->next = NULL;
          
			    }

			    current->sons = current2->next; 
			    current2 = current->sons;
		 
			}

                  
			else{
			    changed = FALSE;
			    current=current2;
			    current2=current->next;
			}
		    } 
		    else{
			changed = FALSE;
			current=current2;
			current2=current->next;
		    }


		}

	    }

            /*end for first node */
          

	    while(current2!=NULL)
	    {
		if(!changed){
     
		    if((current2->temporal_op==WITHIN) || (current2->temporal_op==ALWAYS_WITHIN)) {   	  
     
			til_current = new_PlNode(TEMPORALOP);
			til_current->temporal_op = current2->temporal_op;
			if(current2->name)
			    til_current->name = current2->name;

		        til_current->lh = current2->lh;

			til_current->sons = current2->sons;

			tilnode = til_constraints_head;
			if ( tilnode->sons ) 
			    for ( i_son = tilnode->sons; i_son->next!=NULL; i_son = i_son->next );

			if( !tilnode->sons ){
			    tilnode->sons = til_current;
			    til_current->next = NULL;
			}
			else{
			    i_son->next = til_current;
			    til_current->next = NULL;
          
			}
			current->next = current2->next;
			current2 = current->next;
		 
		    }
		    else{       
		   
			if((current2->connective==ALL) || (current2->connective==EX)){
			    if((current2->sons->temporal_op==WITHIN) || (current2->sons->temporal_op==ALWAYS_WITHIN)) {   	  
       
				til_current = new_PlNode(current2->connective);
				til_current->parse_vars = current2->parse_vars;
                              

				til_current->sons = new_PlNode(TEMPORALOP);
				til_current->sons->temporal_op = current2->sons->temporal_op;
				if(current2->name){
				    til_current->name = current2->name;
				}
				if(current2->sons->name){
				    til_current->sons->name = current2->sons->name;
				}

				til_current->sons->lh = current2->sons->lh;                   
				til_current->sons->sons = current2->sons->sons;

				tilnode = til_constraints_head;
				if ( tilnode->sons ) 
				    for ( i_son = tilnode->sons; i_son->next!=NULL; i_son = i_son->next );

				if( !tilnode->sons ){
				    tilnode->sons = til_current;
				    til_current->next = NULL;
				}
				else{
				    i_son->next = til_current;
				    til_current->next = NULL;
          
				}
				current->next = current2->next;
				current2 = current->next;
		 
			    }

			    else{
                  
				current=current2;
				current2=current->next;
                   
			    }
			}
			else
			{
                  
			    current=current2;
			    current2=current->next;
                   
			}


		    }
		}
		else{
		    goto spring;
		}

	    }

	}

        else
	{

	    if((current->temporal_op==WITHIN) || (current2->temporal_op==ALWAYS_WITHIN)) {  

		til_current = current;

		til_current->next = til_constraints_head;
		til_constraints_head = til_current;

		free_PlNode(n);
		n = NULL;

	    }
	    else{
		if((current->connective==ALL) || (current->connective==EX)){
		    if((current->sons->temporal_op==WITHIN) || (current2->sons->temporal_op==ALWAYS_WITHIN)) { 


			til_current = current;

			til_current->next = til_constraints_head;
			til_constraints_head = til_current;

			free_PlNode(n);
			n = NULL;

		    }

		}

	    }
        
	}        

    }

    /* print_PlNode(til_constraints_head, 0);*/
     
    return til_constraints_head;

}








/*********************************************************************
 * constraints_to_ltl: make from goal constraints a ltl formule
 * and make from preference constraints a ltl formule
 *********************************************************************/ 







void constraints_to_ltl(WffNode *n, PlNode *m)

{


    if ( !n ) {
	printf("none\n");
	return;
    }

    if(m->connective==AND)
	run_constraints_to_ltl(n);
    else
	run_constraints_to_ltl2(n, 0, TRUE, NULL);
  
  
}






void  run_constraints_to_ltl(WffNode *n)

{

  

    WffNode *i;
    int j = 0;

    switch (n->connective) {
	case AND: 
	    run_constraints_to_ltl2(n->sons, j++, TRUE, NULL);
	    if ( n->sons ) {
		for ( i = n->sons->next; i!=NULL; i = i->next ) {
		    if ( !i->prev ) {
			printf("\nprev in AND not correctly set!\n\n");
			exit( 1 );
		    }

		    run_constraints_to_ltl2(i, j++, TRUE, NULL);
	 
		}
	    }  
	    break;
	case OR:  
	    run_constraints_to_ltl2(n->sons, j++, TRUE, NULL);
	    for ( i = n->sons->next; i!=NULL; i = i->next ) {
		run_constraints_to_ltl2(i, j++, TRUE, NULL);
       
	    }     
	    break;
	default:
	    printf("\n***** ERROR ****");
	    printf("\nconstraints: %d > Wrong Node specifier\n", n->connective);
	    exit(1);
    }     


}  



void run_constraints_to_ltl2(WffNode *n, int j, Bool changed, FILE *in)

{

    WffNode *i;
    FILE *out = 0 ;
    char s[20], s1[30], s2[30];
    int k = 0;

    if ( !n ) {
	printf("none\n");
	return;
    }


    if(changed){
	changed = FALSE;
	if(n->name){
	    /* printf("****preferences******** %s\n",n->name);
	       exit(1); */
	    strcpy(s1, n->name);
	    sprintf(s,"%s%s_%d","p-", s1, j);
	    if(strcmp(s, automata_name) < 0)
	        strcpy(automata_name, s);
          
	    sprintf(s2,"%s_%d", s1, j);
	    strcpy(n->new_name, s2);  
	}
	else{
	    /* printf("****no-name******** empty \n");
	       exit(1); */
	    sprintf(s,"%s%d","a-", j);
	    if(strcmp(s, automata_name) < 0)
	        strcpy(automata_name, s);
	    
	}
/*	if(n->connective != TRU)*/
	    if ( (out = fopen( s, "w")) == NULL ) {
		printf("\n\nCannot open file .\n\n");
		exit( 1 );
	    }

    }
    else{
	out = in ;
    }
  
    switch (n->connective) {
	case AND: 
	    run_constraints_to_ltl2(n->sons,j, changed, out);
	    if ( n->sons ) {
		for ( i = n->sons->next; i!=NULL; i = i->next ) {
		    if ( !i->prev ) {
			printf("\nprev in AND not correctly set!\n\n");
			exit( 1 );
		    }
		    fprintf( out, " && ");
		    run_constraints_to_ltl2(i,j, changed, out);
		}
	    }  
	    break;
	case OR:  
	    run_constraints_to_ltl2(n->sons,j, changed, out);
            k++;
	    for ( i = n->sons->next; i!=NULL; i = i->next ) {
		fprintf(out, "|| ");
		run_constraints_to_ltl2(i,j, changed, out);
                k++;
                if(k > 30){

					printf("^^WARNING!!! LTL Formula too big. Truncated!! Quality of solution not guaranteed!\n");
					break;

				}
	    }     
	    break;
	case NOT:
	    if (ATOM==n->son->connective) {
		fprintf(out, " !  ");
		run_constraints_to_ltl2(n->son,j, changed, out);
	    } else {
		fprintf(out, " ! (");
		run_constraints_to_ltl2(n->son,j, changed, out);
		fprintf(out, ") ");
	    }
	    break;
	case ATOM:
	    fprint_Fact(out, n->fact);
	    if ( n->NOT_p != -1 ) printf(" - translation NOT");
	    break;
	case COMP:
	    switch (n->comp) {
		case LE:
		    fprintf(out, "less__");
		    break;
		case LEQ:
		    fprintf(out, "less_equal__");
		    break;
		case EQ:
		    fprintf(out, "equal__");
		    break;
		case GEQ:
		    fprintf(out, "great_equal__");
		    break;
		case GE:
		    fprintf(out, "great__");
		    break;
		default:
		    printf("\nwrong comparator of Expnodes in WFF %d\n\n", n->comp);
		    exit( 1 );
	    }
	    fprint_ExpNode2( out, n->lh );
	    fprintf(out, "__");
	    fprint_ExpNode2( out, n->rh );
	    break;
	case TEMPORALOP:
	    switch (n->temporal_op) {
		case ALWAYS:
		    fprintf(out, " [] ");
		    fprintf(out, "(");
		    run_constraints_to_ltl2( n->son, j, changed, out);
		    fprintf(out, ")");
		    break;
		case SOMETIME:
		    fprintf(out, " <> ");
		    fprintf(out, "(");
		    run_constraints_to_ltl2( n->son, j, changed, out);
		    fprintf(out, ")");
		    break;
		case AT_MOST_ONCE:
		    fprintf(out, " [] ( ");
		    run_constraints_to_ltl2( n->son, j, changed, out);
		    fprintf(out, " -> ( ");
		    run_constraints_to_ltl2( n->son, j, changed, out);
		    fprintf(out, " U ([] ! ( ");
		    run_constraints_to_ltl2( n->son, j, changed, out);
		    fprintf(out, " )))) ");
        
		    break;
		case SOMETIME_AFTER:
		    fprintf(out, " [] ( ");
		    run_constraints_to_ltl2( n->son, j, changed, out);
		    fprintf(out, " -> ( <> ");
		    run_constraints_to_ltl2( n->son->next, j, changed, out);
		    fprintf(out, " )) ");
		    break;
		case SOMETIME_BEFORE:
		    fprintf(out, "( !( ");
		    run_constraints_to_ltl2( n->son, j, changed, out);
		    fprintf(out, " ) && ");
		    fprintf(out, " !( ");
		    run_constraints_to_ltl2( n->son->next, j, changed, out);
		    fprintf(out, " )) ");
		    fprintf(out, " U ( ");
		    fprintf(out, "( !( ");
		    run_constraints_to_ltl2( n->son, j, changed, out);
		    fprintf(out, " ) && ");
		    fprintf(out, " ( ");
		    run_constraints_to_ltl2( n->son->next, j, changed, out);
		    fprintf(out, " )) ");
		    fprintf(out, " || ([]  ");
		    fprintf(out, "( !( ");
		    run_constraints_to_ltl2( n->son, j, changed, out);
		    fprintf(out, " ) && ");
		    fprintf(out, " !( ");
		    run_constraints_to_ltl2( n->son->next, j, changed, out);
		    fprintf(out, " )) ");
		    fprintf(out, ")) ");
		    break;
		default:
		    printf("\n\nillegal temporal operator: %d in tree !\n\n", n->temporal_op);
		    exit( 1 );
	    }
	    break;
	case TRU:
            fprintf(out, "true");
	    break;
       case FAL:
            fprintf(out, "false");
	    break;
	default:
	    printf("\n***** ERROR ****");
	    printf("\nconstraints2: %d > Wrong Node specifier\n", n->connective);
	    exit(1);
    }     


}  



/*********************************************************************
 * create simple preference and preference constraints node
 *********************************************************************/ 






void preference_simple_node(WffNode *n, PlNode *m)

{


    printf(" preference simple node \n");  

    if ( !n ) {
	printf("none\n");
	return;
    }


    if(m->connective==AND)
	preference(n);
    else
	printf("\n preference_simple_node: %d wrong node specifier!\n", m->connective);

}





void preference(WffNode *n)

{

    WffNode *i, *i1;
    WffNode *node1 = NULL;
    WffNode *father = NULL;

    switch (n->connective) {
	case AND:
	case OR: 
	    if(n->sons){
		if(n->sons->connective == AND || n->sons->connective == OR)
		    preference(n->sons);
		if(n->sons->sons){
		    if(n->sons->sons->name){
			node1 = n->sons;
			n->sons = n->sons->sons;
     

			if ( n->sons ) {
			    for ( i1 = n->sons; i1->next!=NULL; i1 = i1->next );
			    i1->next = node1->next;
			    if(i1->next)
				i1->next->prev = i1;
			    father = i1;
			}
		    }
		    else{
			father = n->sons;
			i1 = n->sons;
		    }
		}
		else{
		    father = n->sons;
		    i1 = n->sons;           
		}
	    }

	    if ( n->sons ) {
		for ( i = n->sons->next; i!=NULL; i = i->next ) {
		    if(i->connective == AND || i->connective == OR)
			preference(i);
		    if(i->sons){
			if(i->sons->name){
			    node1 = i;
			    father->next = node1->sons;
			    father->next->prev = father;

          
			    if ( father->next ) {
				for ( i1 = father->next; i1->next!=NULL; i1 = i1->next );
				i1->next = node1->next;
				if(i1->next)
				    i1->next->prev = i1;
				father = i1;
			    }
   
			}     
			else{
			    father = i;
			    i1 = i;
			}         
		    }
		    else{
			father = i;
			i1 = i;
		    }
		}
	    }  
	    break;
	default:
	    printf("\n***** ERROR ****");
	    printf("\n preference: %d > Wrong Node specifier\n", n->connective);
	    exit(1);
    }     


}  





/*+++++++++++++++++++++++++++++++++++++++++++++*/



void splitt_ltl_formel_for_constraints(WffNode *n, PlNode *m)

{


    printf(" splitt ltl formel \n");  

    if ( !n ) {
	printf("none\n");
	return;
    }


    if(m->connective==AND)
	splitt_ltl_formel(n);
    else
	printf("\n splitt_ltl_formel_for_constraints: %d wrong node specifier!\n", m->connective);

}




void splitt_ltl_formel(WffNode *n)

{

    WffNode *i, *i1;
    WffNode *node1 = NULL;
    WffNode *father = NULL;

    switch (n->connective) {
	case AND:
	case OR: 
	    if(n->sons){
		if(n->sons->connective == AND || n->sons->connective == OR){
		  if(n->sons->sons){
			node1 = n->sons;
			n->sons = n->sons->sons;
     

			if ( n->sons ) {
			    for ( i1 = n->sons; i1->next!=NULL; i1 = i1->next );
			    i1->next = node1->next;
			    if(i1->next)
				i1->next->prev = i1;
			    father = i1;
			}
		  }
		  else{
		      father = n->sons;
		      i1 = n->sons;           
		  }
		}
                else{
		    father = n->sons;
		    i1 = n->sons;           
	        }
	    }

	    if ( n->sons ) {
		for ( i = n->sons->next; i!=NULL; i = i->next ) {
		    if(i->connective == AND || i->connective == OR){

		      if(i->sons){
			      node1 = i;
			      father->next = node1->sons;
			      father->next->prev = father;

          
			      if ( father->next ) {
				  for ( i1 = father->next; i1->next!=NULL; i1 = i1->next );
				  i1->next = node1->next;
				  if(i1->next)
				      i1->next->prev = i1;
				  father = i1;
			       }
   
                	   }     
			   else{
			      father = i;
			      i1 = i;
			   }         
		       }
		       else{
			   father = i;
			   i1 = i;
		       }
		}
	    }  
	    break;
	default:
	    printf("\n***** ERROR ****");
	    printf("\n splitt_ltl_formel: %d > Wrong Node specifier\n", n->connective);
	    exit(1);
    }     

}  






/*********************************************************************
 * read_gtil_constraints: read gtil_constraints and write them as timed
 * initial literal  
 *********************************************************************/ 




int read_gtil_constraints(WffNode *n, FILE *out)

{

    WffNode *i;
    int j = 0;
 
  
    switch (n->connective) {
	case AND: 
	    read_gtil_constraints2(n->sons,j++, out);
	    if ( n->sons ) {
		for ( i = n->sons->next; i!=NULL; i = i->next ) {
		    if ( !i->prev ) {
			printf("\nprev in AND not correctly set!\n\n");
			exit( 1 );
		    }
		    read_gtil_constraints2(i,j++,out);
		}
	    }  
	    break;
	case OR:  
	    read_gtil_constraints2(n->sons,j++, out);
	    for ( i = n->sons->next; i!=NULL; i = i->next ) {
		read_gtil_constraints2( i,j++, out);
	    }     
	    break;
	default:
	    printf("\n***** ERROR ****");
	    printf("\ngtil_constraints: %d > Wrong Node specifier\n", n->connective);
	    exit(1);
    }     

    return j;

}  





void read_gtil_constraints2(WffNode *n, int j, FILE *out)

{

    WffNode *i;
    char s1[40]; 
    char s2[40];
    char s3[40];

    if(n->name){
	sprintf(s1,"%s-fact-%d",n->name, j);
	sprintf(s2,"IS_VIOLATED_%s_%d",n->name, j);

	sprintf(s3,"%s_%d",n->name, j);
	  strcpy(n->new_name, s3);
    }
    else
	sprintf(s1,"fact-%d", j);

    if ( !n ) {
	printf("none\n");
	return;
    }

  
    switch (n->connective) {
	case AND: 
	    read_gtil_constraints2(n->sons, j, out);
	    if ( n->sons ) {
		for ( i = n->sons->next; i!=NULL; i = i->next ) {
		    if ( !i->prev ) {
			printf("\nprev in AND not correctly set!\n\n");
			exit( 1 );
		    }
		    read_gtil_constraints2(i,j,out);
		}
	    }  
	    break;
	case OR:  
	    read_gtil_constraints2(n->sons,j, out);
	    for ( i = n->sons->next; i!=NULL; i = i->next ) {
		read_gtil_constraints2(i,j, out);
	    }     
	    break;
	case TEMPORALOP:
	    switch (n->temporal_op) {
		case HOLD_DURING:
		    fprintf(out, "(at ");
		    fprint_ExpNode(out,n->lh);
		    fprintf(out, " (");
		    fprintf(out, "%s", s1);
		    fprintf(out, "))\n");
		    fprintf(out, "(at ");
		    fprint_ExpNode(out,n->rh);
		    fprintf(out, " (not (");
		    fprintf(out, "%s", s1);
		    fprintf(out, ")))\n"); 
		    if(n->name){
			fprintf(out,"(=(");
			fprintf(out, "%s) 1)\n",s2);
		    }
		    break;
		case HOLD_AFTER:
		    fprintf(out, "(at ");
		    fprint_ExpNode(out,n->lh);
		    fprintf(out, " (");
		    fprintf(out, "%s", s1);
		    fprintf(out, "))\n");

		    if(n->name){
			fprintf(out,"(=(");
			fprintf(out, "%s) 1)\n",s2);
		    }
		    break;
		default:
		    printf("\n\nillegal temporal operator in tree !\n\n");
		    exit( 1 );
	    }
	    break;
	default:
	    printf("\n***** ERROR ****");
	    printf("\ngtil_constraints: %d > Wrong Node specifier\n", n->connective);
	    exit(1);
    }     


}  




/*********************************************************************
 * add_til_and_within_constraints_pref_to_predicates:
 *********************************************************************/ 




void add_til_and_within_constraints_pref_fact_to_predicates(WffNode *n,char s[40], FILE *out)

{

    WffNode *i;
    int j = 0;
 
    switch (n->connective) {
	case AND: 
	    add_til_and_within_constraints_pref_fact_to_predicates2(n->sons,j++,s, out);
	    if ( n->sons ) {
		for ( i = n->sons->next; i!=NULL; i = i->next ) {
		    if ( !i->prev ) {
			printf("\nprev in AND not correctly set!\n\n");
			exit( 1 );
		    }
		    add_til_and_within_constraints_pref_fact_to_predicates2(i,j++,s, out);
		}
	    }  
	    break;
	case OR:  
	    add_til_and_within_constraints_pref_fact_to_predicates2(n->sons,j++,s, out);
	    for ( i = n->sons->next; i!=NULL; i = i->next ) {
		add_til_and_within_constraints_pref_fact_to_predicates2( i,j++,s, out);
	    }     
	    break;
	default:
	    printf("\n***** ERROR ****");
	    printf("\ngtil_constraints: %d > Wrong Node specifier\n", n->connective);
	    exit(1);
    }     


}  





void add_til_and_within_constraints_pref_fact_to_predicates2(WffNode *n,int j,char s[40], FILE *out)

{

    WffNode *i;
    char s1[40]; 

  
    if(n->name){
	sprintf(s1,"%s-%s-%d",n->name,s, j);
    }
 

    if ( !n ) {
	printf("none\n");
	return;
    }

  
    switch (n->connective) {
	case AND: 
	    add_til_and_within_constraints_pref_fact_to_predicates2(n->sons,j,s, out);
	    if ( n->sons ) {
		for ( i = n->sons->next; i!=NULL; i = i->next ) {
		    if ( !i->prev ) {
			printf("\nprev in AND not correctly set!\n\n");
			exit( 1 );
		    }
		    add_til_and_within_constraints_pref_fact_to_predicates2(i,j,s, out);
		}
	    }  
	    break;
	case OR:  
	    add_til_and_within_constraints_pref_fact_to_predicates2(n->sons,j,s, out);
	    for ( i = n->sons->next; i!=NULL; i = i->next ) {
		add_til_and_within_constraints_pref_fact_to_predicates2(i,j,s, out);
	    }     
	    break;
	case TEMPORALOP:
	    switch (n->temporal_op) {
		case HOLD_DURING:
		    fprintf(out, "(%s)\n", s1);
		    break;
		case HOLD_AFTER:
		    fprintf(out, "(%s)\n", s1);
		    break;
		case WITHIN:
		    fprintf(out, "(%s)\n", s1);
		    break;
		case ALWAYS_WITHIN:
		    fprintf(out, "(%s)\n", s1);
		    break;
		default:
		    printf("\n\nillegal temporal operator in tree !\n\n");
		    exit( 1 );
	    }
	    break;
	default:
	    printf("\n***** ERROR ****");
	    printf("\ngtil_constraints: %d > Wrong Node specifier\n", n->connective);
	    exit(1);
    }     


}  





/*********************************************************************
 * add is-violated-pref for timed initial literal constraints preference
 * in function:
 *********************************************************************/ 




void add_is_violated_til_constraints_pref_to_function(WffNode *n, FILE *out)

{

    WffNode *i;
    int j = 0;

    switch (n->connective) {
	case AND: 
	    add_is_violated_til_constraints_pref_to_function2(n->sons,j++, out);
	    if ( n->sons ) {
		for ( i = n->sons->next; i!=NULL; i = i->next ) {
		    if ( !i->prev ) {
			printf("\nprev in AND not correctly set!\n\n");
			exit( 1 );
		    }
		    add_is_violated_til_constraints_pref_to_function2(i,j++, out);
		}
	    }  
	    break;
	case OR:  
	    add_is_violated_til_constraints_pref_to_function2(n->sons,j++, out);
	    for ( i = n->sons->next; i!=NULL; i = i->next ) {
		add_is_violated_til_constraints_pref_to_function2( i,j++, out);
	    }     
	    break;
	default:
	    printf("\n***** ERROR ****");
	    printf("\ngtil_constraints: %d > Wrong Node specifier\n", n->connective);
	    exit(1);
    }     


}  





void add_is_violated_til_constraints_pref_to_function2(WffNode *n,int j, FILE *out)

{

    WffNode *i;
    char s1[40]; 
  
    if(n->name){
	sprintf(s1,"IS_VIOLATED_%s_%d",n->name, j);
    }
 

    if ( !n ) {
	printf("none\n");
	return;
    }

  
    switch (n->connective) {
	case AND: 
	    add_is_violated_til_constraints_pref_to_function2(n->sons,j, out);
	    if ( n->sons ) {
		for ( i = n->sons->next; i!=NULL; i = i->next ) {
		    if ( !i->prev ) {
			printf("\nprev in AND not correctly set!\n\n");
			exit( 1 );
		    }
		    add_is_violated_til_constraints_pref_to_function2(i,j, out);
		}
	    }  
	    break;
	case OR:  
	    add_is_violated_til_constraints_pref_to_function2(n->sons,j, out);
	    for ( i = n->sons->next; i!=NULL; i = i->next ) {
		add_is_violated_til_constraints_pref_to_function2(i,j, out);
	    }     
	    break;
	case TEMPORALOP:
	    switch (n->temporal_op) {
		case HOLD_DURING:
		    fprintf(out, "(%s)\n", s1);
		    break;
		case HOLD_AFTER:
		    fprintf(out, "(%s)\n", s1);
		    break;
		default:
		    printf("\n\nillegal temporal operator in tree !\n\n");
		    exit( 1 );
	    }
	    break;
	default:
	    printf("\n***** ERROR ****");
	    printf("\ngtil_constraints: %d > Wrong Node specifier\n", n->connective);
	    exit(1);
    }     


}  






/*********************************************************************
 * read_gwithin_constraints: read gwithin_constraints, write them 
 * as timed initial literal and create ltl-formule
 *********************************************************************/ 




int read_gwithin_constraints(WffNode *n,char s[40], FILE *out)

{

    WffNode *i;
    int j = 0;
 
  
    switch (n->connective) {
	case AND: 
	    read_gwithin_constraints2(n->sons,j++,s, out,TRUE, NULL);
	    if ( n->sons ) {
		for ( i = n->sons->next; i!=NULL; i = i->next ) {
		    if ( !i->prev ) {
			printf("\nprev in AND not correctly set!\n\n");
			exit( 1 );
		    }
		    read_gwithin_constraints2(i,j++,s, out,TRUE, NULL);
		}
	    }  
	    break;
	case OR:  
	    read_gwithin_constraints2(n->sons,j++,s, out, TRUE, NULL);
	    for ( i = n->sons->next; i!=NULL; i = i->next ) {
		read_gwithin_constraints2( i,j++,s, out, TRUE, NULL);
	    }     
	    break;
	default:
	    printf("\n***** ERROR ****");
	    printf("\ngtil_constraints: %d > Wrong Node specifier\n", n->connective);
	    exit(1);
    }     

    return j;

}  




void read_gwithin_constraints2(WffNode *n, int j,char s[40], FILE *out, Bool changed, FILE *in2)

{
    FILE *out2;
    WffNode *i;
    char s1[40]; 
    char s2[40];

    if ( !n ) {
	printf("none\n");
	return;
    }

    if(changed){
	changed = FALSE;
	if(n->name){
	    sprintf(s1,"p-%s-%s-%d",n->name,s, j);
	    sprintf(s2,"%s-fact-%d",n->name,j);
	    printf("****preferences******** %s s1 %s s2 %s\n",n->name,s1,s2);

	    if(strcmp(s1, automata_name) < 0)
	        strcpy(automata_name, s1);
       

	 /* sprintf(s3,"%s-%d",n->name,j);
	    strcpy(n->new_name, s3); */

	}
	else{
	    sprintf(s1,"a-%s%s%d",s,"-", j);
	    sprintf(s2, "wfact-%d",j);

	    printf("****no-name ******** s1 %s s2 %s\n",s1,s2);

	    if(strcmp(s1, automata_name) < 0)
	        strcpy(automata_name, s1);

	}
	if ( (out2 = fopen( s1, "w")) == NULL ) {
	    printf("\n\nCannot open file.\n\n");
	    exit( 1 );
	}

    }
    else{
	strcpy(s2, s);
	out2 = in2 ;
    }
  

    switch (n->connective) {
	case AND: 
	    read_gwithin_constraints2(n->sons, j,s2, out, changed, out2);
	    if ( n->sons ) {
		for ( i = n->sons->next; i!=NULL; i = i->next ) {
		    if ( !i->prev ) {
			printf("\nprev in AND not correctly set!\n\n");
			exit( 1 );
		    }
		    fprintf( out2, " && ");
		    read_gwithin_constraints2(i,j,s2, out, changed, out2);
		}
	    }  
	    break;
	case OR:  
	    read_gwithin_constraints2(n->sons,j,s, out, changed, out2);
	    for ( i = n->sons->next; i!=NULL; i = i->next ) {
		fprintf( out, " || ");
		read_gwithin_constraints2(i,j,s, out, changed, out2);
              
	    }     
	    break;
	case TEMPORALOP:
	    switch (n->temporal_op) {
		case WITHIN:
		    fprintf(out, "(at  0.00 (");
		    fprintf(out, "%s", s2);
		    fprintf(out, "))\n");
		    fprintf(out, "(at ");
		    fprint_ExpNode(out,n->lh);
		    fprintf(out, " (not (");
		    fprintf(out, "%s", s2);
		    fprintf(out, ")))\n");
		    change_within_exp_to_ltl( n, s1, out2);
		    break;
		case ALWAYS_WITHIN:
		    fprintf(out, "(at  0.00 (");
		    fprintf(out, "%s", s2);
		    fprintf(out, "))\n");
		    fprintf(out, "(at ");
		    fprint_ExpNode(out,n->lh);
		    fprintf(out, " (not (");
		    fprintf(out, "%s", s2);
		    fprintf(out, ")))\n");
		    change_within_exp_to_ltl( n, s1,out2);
		    break;
		default:
		    printf("\n\nillegal temporal operator in tree !\n\n");
		    exit( 1 );
	    }
	    break;
	default:
	    printf("\n***** ERROR ****");
	    printf("\ngtil_constraints: %d > Wrong Node specifier\n", n->connective);
	    exit(1);
    }     

}  





void change_within_exp_to_ltl(WffNode *n, char *str, FILE *out)

{
    WffNode *i = NULL;
    int k = 0;

    if ( !n ) {
	printf("none\n");
	return;
    }
 
  
    switch (n->connective) {
	case NOT:
	    if (ATOM==n->son->connective) {
		fprintf(out, " !  ");
		change_within_exp_to_ltl(n->son,str, out);
	    } else {
		fprintf(out, " ! (");
		change_within_exp_to_ltl(n->son,str, out);
		fprintf(out, ") ");
	    }
	    break;
	case ATOM:
	    fprint_Fact(out, n->fact);
	    if ( n->NOT_p != -1 ) printf(" - translation NOT");
	    break;
	case COMP:
	    switch (n->comp) {
		case LE:
		    fprintf(out, "less__");
		    break;
		case LEQ:
		    fprintf(out, "less_equal__");
		    break;
		case EQ:
		    fprintf(out, "equal__");
		    break;
		case GEQ:
		    fprintf(out, "great_equal__");
		    break;
		case GE:
		    fprintf(out, "great__");
		    break;
		default:
		    printf("\nwrong comparator of Expnodes in WFF %d\n\n", n->comp);
		    exit( 1 );
	    }
	    fprint_ExpNode2( out, n->lh );
	    fprintf(out, "__");
	    fprint_ExpNode2( out, n->rh );
	    break;
        case AND:
            fprintf(out, "(");
            change_within_exp_to_ltl(n->sons, str, out);
	    for ( i = n->sons->next; i!=NULL; i = i->next ) {
		fprintf( out, " && ");
		change_within_exp_to_ltl(i, str, out);
	    } 
              fprintf(out, ")");
            break;
        case OR:
            fprintf(out, "(");
            change_within_exp_to_ltl(n->sons, str, out);
            k++;
	    for ( i = n->sons->next; i!=NULL; i = i->next ) {
		fprintf( out, " || ");
		change_within_exp_to_ltl(i, str, out);
                  k++;

				  if(k > 30){
					  printf("^^WARNING!!! LTL Formula too big. Truncated!! Quality of solution not guaranteed!\n");
					  break;
				  }
	    }
            fprintf(out, ")"); 
	    break;
	case TEMPORALOP:
	    switch (n->temporal_op) {
		case WITHIN:
		    fprintf(out, " <> ");
		    fprintf(out, "(");
		    change_within_exp_to_ltl( n->son, str, out);
		    fprintf(out, ")");
		    break;
		case ALWAYS_WITHIN:
		    fprintf(out, " [] ( ");
		    change_within_exp_to_ltl( n->son, str, out);
		    fprintf(out, " -> (<> ");
		    change_within_exp_to_ltl( n->son->next, str, out);
		    fprintf(out, " )) ");
		    break;
		default:
		    printf("\n\nillegal temporal operator in tree !\n\n");
		    exit( 1 );
	    }
	    break;
        case TRU:
            fprintf(out, "true");
	    break;
        case FAL:
            fprintf(out, "false");
	    break;
	default:
	    printf("\n***** ERROR ****");
	    printf("\ngwithin_constraints: %d > Wrong Node specifier\n", n->connective);
	    exit(1);
    }     


}  






/*********************************************************************
 * create supplementar operators for gtil_constraints 
 *********************************************************************/ 






int create_operator_for_gtil_constraints(WffNode *n,char s[40], FILE *out)

{

    WffNode *i;
    int j = 0;
 
  
    switch (n->connective) {
	case AND: 
	    create_operator_for_gtil_constraints2(n->sons,j++,s, out);
	    if ( n->sons ) {
		for ( i = n->sons->next; i!=NULL; i = i->next ) {
		    if ( !i->prev ) {
			printf("\nprev in AND not correctly set!\n\n");
			exit( 1 );
		    }
		    create_operator_for_gtil_constraints2(i,j++,s, out);
		}
	    }  
	    break;
	case OR:  
	    create_operator_for_gtil_constraints2(n->sons,j++,s, out);
	    for ( i = n->sons->next; i!=NULL; i = i->next ) {
		create_operator_for_gtil_constraints2( i,j++,s, out);
	    }     
	    break;
	default:
	    printf("\n***** ERROR ****");
	    printf("\ngtil_constraints: %d > Wrong Node specifier\n", n->connective);
	    exit(1);
    }     

    return j;

}  







void create_operator_for_gtil_constraints2(WffNode *n, int j,char s[40], FILE *out)

{

    WffNode *i;
    char s1[40]; 
    char s2[20];

    if(n->name){
	sprintf(s1,"%s-%s-%d",n->name,s, j);
	sprintf(s2,"%d", j);
    }

    else{
	sprintf(s1,"%s-%d",s, j);  
	sprintf(s2,"%d",j);
    }

    if ( !n ) {
	printf("none\n");
	return;
    }

  
    switch (n->connective) {
	case AND: 
	    create_operator_for_gtil_constraints2(n->sons, j,s, out);
	    if ( n->sons ) {
		for ( i = n->sons->next; i!=NULL; i = i->next ) {
		    if ( !i->prev ) {
			printf("\nprev in AND not correctly set!\n\n");
			exit( 1 );
		    }
		    create_operator_for_gtil_constraints2(i,j,s, out);
		}
	    }  
	    break;
	case OR:  
	    create_operator_for_gtil_constraints2(n->sons,j,s, out);
	    for ( i = n->sons->next; i!=NULL; i = i->next ) {
		create_operator_for_gtil_constraints2(i,j,s, out);
	    }     
	    break;
	case ATOM:
	    fprintf(out, "(");
	    fprint_Fact(out, n->fact);
	    fprintf(out, ")");
	    if ( n->NOT_p != -1 ) printf(" - translation NOT");
	    break;
	case COMP:
	    switch (n->comp) {
		case LE:
		    fprintf(out, "(< ");
		    break;
		case LEQ:
		    fprintf(out, "(<= ");
		    break;
		case EQ:
		    fprintf(out, "(= ");
		    break;
		case GEQ:
		    fprintf(out, "(>= ");
		    break;
		case GE:
		    fprintf(out, "(> ");
		    break;
		default:
		    printf("\nwrong comparator of Expnodes in WFF %d\n\n", n->comp);
		    exit( 1 );
	    }
	    fprintf(out, " ");
	    fprint_ExpNode( out, n->lh );
	    fprintf(out, " ");
	    fprint_ExpNode( out, n->rh );
	    fprintf(out, " )\n");
	    break;
	case TEMPORALOP:
	    switch (n->temporal_op) {
		case HOLD_DURING:
		    if(isDurative)
			fprintf(out, "(:durative-action supplem-%s\n", s1);  
		    else 
			fprintf(out, "(:action supplem-%s\n", s1);      
		    if(isDurative){
			fprintf(out, ":duration (= ?duration 0");
			fprintf(out, ")\n");
		    }
		    if(isDurative)
			fprintf(out, ":condition\n");
		    else
			fprintf(out, ":precondition\n");
		    fprintf(out, "(and\n");
		    if(isDurative){
			fprintf(out, "(over all ");
		    }
		    create_operator_for_gtil_constraints2(n->son, j, s, out);
		    if(isDurative){
			fprintf(out, ")\n");
		    }
		    if(gconstraints || gconstraints_pref || gwithin_constraints || gwithin_constraints_pref){
			if(isDurative){
			    fprintf(out, "(over all ");
			}
			fprintf(out, "(sync-ordinary)");
			if(isDurative){
			    fprintf(out, ")\n");
			}
		    }
		    if(isDurative){
			fprintf(out, "(over all ");
		    }
		    fprintf(out, "(%s) ", s1);
		    if(isDurative){
			fprintf(out, ")");
		    }
		    fprintf(out, ")\n");
          
		    fprintf(out, ":effect\n");
		    fprintf(out, "(and\n");
		    if(isDurative){
			fprintf(out, "(at end");
		    }

		    if(n->name){
			fprintf(out, "(assign ");
			fprintf(out, "( IS_VIOLATED_%s_%s",n->name, s2);
			fprintf(out, ")  %i)", 0);
		    }
		    else
			fprintf(out, " (done-%s) ", s2);

		    if(isDurative){
			fprintf(out, ")\n");
		    }
		    if(gconstraints || gconstraints_pref || gwithin_constraints || gwithin_constraints_pref){
			if(isDurative){
			    fprintf(out, "(over all ");
			}
			fprintf(out, "(not (sync-ordinary))");
			if(isDurative){
			    fprintf(out, ")\n");
			}
			if(isDurative)
			    fprintf(out, "(at start ");
			fprintf(out, "(sync-automaton-%s)", automata_name);
			if(isDurative)
			    fprintf(out, ")");
			fprintf(out, "\n");
		    }
		    fprintf(out, "))\n");

		    break;
		case HOLD_AFTER:
		    if(isDurative)
			fprintf(out, "(:durative-action supplem-%s\n", s1);  
		    else 
			fprintf(out, "(:action supplem-%s\n", s1);      
		    if(isDurative){
			fprintf(out, ":duration (= ?duration 0");
			fprintf(out, ")\n");
		    }
		    if(isDurative)
			fprintf(out, ":condition\n");
		    else
			fprintf(out, ":precondition\n");
		    fprintf(out, "(and\n");
		    if(isDurative){
			fprintf(out, "(over all ");
		    }
		    create_operator_for_gtil_constraints2(n->son, j, s, out);
		    if(isDurative){
			fprintf(out, ")\n");
		    }
		    if(isDurative){
			fprintf(out, "(over all ");
		    }
		    if(gconstraints || gconstraints_pref || gwithin_constraints || gwithin_constraints_pref){
			fprintf(out, "(sync-ordinary)");
			if(isDurative){
			    fprintf(out, ")\n");
			}
		    }
		    if(isDurative){
			fprintf(out, "(over all ");
		    }
		    fprintf(out, "(%s) ", s1);
		    if(isDurative){
			fprintf(out, ")");
		    }
		    fprintf(out, ")\n");

		    fprintf(out, ":effect\n");
		    fprintf(out, "(and\n");
		    if(isDurative){
			fprintf(out, "(at end");
		    }

		    if(n->name){
			fprintf(out, "(assign ");
			fprintf(out, "( IS_VIOLATED_%s_%s",n->name, s2);
			fprintf(out, ")  %i)", 0);
		    }
		    else
			fprintf(out, " (done-%s) ", s2);          

		    if(isDurative){
			fprintf(out, ")\n");
		    }
		    if(gconstraints || gconstraints_pref || gwithin_constraints || gwithin_constraints_pref){
			if(isDurative){
			    fprintf(out, "(over all ");
			}
			fprintf(out, "(not (sync-ordinary))");
			if(isDurative){
			    fprintf(out, ")\n");
			}
			if(isDurative)
			    fprintf(out, "(at start ");
			fprintf(out, "(sync-automaton-%s)", automata_name);
			if(isDurative)
			    fprintf(out, ")");
			fprintf(out, "\n");
		    }
		    fprintf(out, "))\n");
		    break;
		default:
		    printf("\n\nillegal temporal operator in tree !\n\n");
		    exit( 1 );
	    }
	    break;
	default:
	    printf("\n***** ERROR ****");
	    printf("\ngtil_constraints: %d > Wrong Node specifier\n", n->connective);
	    exit(1);
    }     

}  




char* exchange(char* str) {
    int i = 0;
    while (str[i] != '\0') {
	if (str[i] == '-') 
	    str[i] = '_';
	i++;
    }
    return str;
}



char* exchange2(char* str) {
    int i = 0;
    char *t = copy_Token(str);
    while (t[i] != '\0') {
	if (str[i] == '_') 
	    t[i] = '-';
           if (t[i] == '.')  {
	      t[i] = ' ';
           }

	i++;
    }

    return t;
}



void cleanup_all_wff(WffNode *w)

{
     
    WffNode *current = NULL;
    WffNode *current2 = NULL;
    Bool changed = FALSE;

    if(w==NULL)
    {
	printf(" w is empty \n");
    }	
    else
    {
	current=w;

        if(current->connective==AND)
        {
         
	    current2 = current->sons;

	    /*first node
	    **/
	spring:
            if(current2){
		if(current2->connective == TRU) {   
		    changed = TRUE;
		    current->sons = current2->next;
		    current2 = current->sons;
		}
		else{
                    if(current2->connective == FAL) {   
			w = new_WffNode(current2->connective);
                        return;
		    }
		    changed = FALSE;
		    current=current2;
		    current2=current->next;                  
		}
	    }

            /*end for first node */

	    while(current2!=NULL)
	    {
		
		if(!changed){
		    if(current2->connective == TRU) {               
			current->next = current2->next; 
			current2 = current->next;
		    }
		    else{
                       if(current2->connective == FAL) {   
			  w = new_WffNode(current2->connective);
                          return;
		       }                         		 
			current=current2;
			current2=current->next;
		
		    }
		}
		else{
		    goto spring;
		}

	    }

	}

        else
	{
	    if(current->connective == TRU) {   
		free_WffNode(w);
		w = NULL;
	    }
          
        }
    } 

}




void cleanup_all_wff_pref(WffNode *w)

{
     
    WffNode *current = NULL;
    WffNode *current2 = NULL;
    Bool changed = FALSE;

    if(w==NULL)
    {
	printf(" w is empty \n");
    }	
    else
    {
	current=w;

        if(current->connective==AND)
        {
         
	    current2 = current->sons;

	    /*first node
	    **/
	spring:
            if(current2){
		if(current2->connective == TRU) {   
		    changed = TRUE;
		    current->sons = current2->next;
		    current2 = current->sons;
		}
		else{
		    changed = FALSE;
		    current=current2;
		    current2=current->next;                  
		}
	    }

            /*end for first node */

	    while(current2!=NULL)
	    {
		
		if(!changed){
		    if(current2->connective == TRU) {               
			current->next = current2->next; 
			current2 = current->next;
		    }
		    else{                         		 
			current=current2;
			current2=current->next;
		
		    }
		}
		else{
		    goto spring;
		}

	    }

	}

        else
	{
	    if(current->connective == TRU) {   
		free_WffNode(w);
		w = NULL;
	    }
          
        }
    } 

}









/*

void  reduce_constraints(WffNode *n)

{

  

    WffNode *i;
    int j = 0;

    switch (n->connective) {
	case AND: 
	    break;
	case OR:  
            j++;
	    break;
	default:
	    printf("\n***** ERROR ****");
	    printf("\nreduce_constraints: %d > Wrong Node specifier\n", n->connective);
	    exit(1);
    }     


}  

*/
