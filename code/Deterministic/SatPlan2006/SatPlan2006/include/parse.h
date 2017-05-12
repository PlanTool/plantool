


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
 * File: parse.h
 * Description: Functions for the pddl parser
 *
 * Author: Frank Rittinger 1998 / Joerg Hoffmann 1999
 *
 *********************************************************************/ 





#ifndef _PARSE_H
#define _PARSE_H





type_tree main_type_tree( void );
type_tree find_branch( char *name, type_tree root );
void add_to_type_tree( FactList *t_list, type_tree tree );



void build_orig_constant_list( void );
FactList * build_object_list_from_ttl(type_tree_list ttl, 
				      FactList * types_done );
TokenList *type_already_known( char *name, FactList * types );



char *copy_Token( char *s );
TokenList *copy_complete_TokenList( TokenList *source, 
				    TokenList **end );
void strupcase( char *from );
char *rmdash( char *s );



Bool make_strips_domain( void );
Bool make_conjunction_of_atoms( PlNode **n );
Bool make_conjunction_of_literals( PlNode **n );





#endif /* PARSE */
