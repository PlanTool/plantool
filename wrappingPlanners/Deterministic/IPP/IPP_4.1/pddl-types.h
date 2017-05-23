



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



#ifndef _PDDL_TYPES_H
#define _PDDL_TYPES_H



#include "ipp.h"
#include "pddl.h"




type_tree main_type_tree();

type_tree new_type_tree(char * name);

type_tree_list new_type_tree_list(char * name);

/* steps recursively through type tree and searches for name */
type_tree find_branch(char * name, type_tree root );

/* calls itself recursively to get all objects that are of the types and
   subtypes of ttl */
FactList * build_object_list_from_ttl(type_tree_list ttl, 
				      FactList * types_done );

TokenList * type_already_known(char * name, FactList * types);

void build_orig_constant_list();

void add_to_type_tree(FactList * t_list, type_tree tree );

void print_type_tree_list( type_tree_list root, int indent );

void adjust_equal_types(PlOperator * ops);

#endif /* _PDDL_TYPES_H */
