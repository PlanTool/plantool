*** Metric-FF/parse.c	2005-03-11 14:36:08.000000000 +0100
--- Metric-FF-FTK/parse.c	2011-11-02 14:28:46.189201906 +0100
***************
*** 281,286 ****
--- 281,309 ----
        tmp = NULL;
      }
    }
+   
+   for ( tyll = gparse_functions; tyll; tyll = tyll->next ) {
+     for ( tyl = tyll->args; tyl; tyl = tyl->next ) {
+       if ( tyl->type->next ) {
+ 	tmp = new_Token( MAX_LENGTH );
+ 	strcpy( tmp, EITHER_STR );
+ 	for ( tl = tyl->type; tl; tl = tl->next ) {
+ 	  strcat( tmp, CONNECTOR );
+ 	  strcat( tmp, tl->item );
+ 	}
+       } else {
+ 	tmp = copy_Token( tyl->type->item );
+       }
+       if ( (n = get_type( tmp )) == -1 ) {
+ 	tyl->n = lnum_types;
+ 	ltype_names[lnum_types++] = copy_Token( tmp );
+       } else {
+ 	tyl->n = n;
+       }
+       free( tmp );
+       tmp = NULL;
+     }
+   }
      
    collect_type_names_in_pl( gorig_goal_facts );
  
***************
*** 328,333 ****
--- 351,361 ----
      for ( tyl = tyll->args; tyl; tyl = tyl->next ) {
        make_either_ty( tyl );
      }
+   }
+   for ( tyll = gparse_functions; tyll; tyll = tyll->next ) {
+     for ( tyl = tyll->args; tyl; tyl = tyl->next ) {
+       make_either_ty( tyl );
+     }
    }
    make_either_ty_in_pl( gorig_goal_facts );
    for ( po = gloaded_ops; po; po = po->next ) {
