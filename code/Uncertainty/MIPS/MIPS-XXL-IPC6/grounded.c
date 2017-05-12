
/*********************************************************************
 * File: grounded.c
 * Description: functions for create grounded domain and problem.
 *
 *
 * 
 *
 *********************************************************************/ 





#include "ff.h"

#include "output.h"
#include "memory.h"
#include "parse.h"
#include "expressions.h"

#include "inst_pre.h"
#include "inst_final.h"

#include "grounded.h"












/**************************************************
 * grounded strips task
 **************************************************/











void output_grounded_STRIPS_task( void )

{

  FILE *fts, *ops;
  int i, j, k;
  Action *a;

  if ( (fts = fopen( "groundedproblem.pddl", "w")) == NULL ) {
    printf("\n\nCannot open file groundedproblem.pddl.\n\n");
    exit( 1 );
  }
  fprintf(fts, "(define (problem grounded-STRIPS-%s)\n", gproblem_name);
  fprintf(fts, "(:domain grounded-STRIPS-%s)\n", gdomain_name);
  if ( ginitial_state.num_F > 0 ) {
    fprintf(fts, "(:init\n");
    for ( i = 0; i < ginitial_state.num_F; i++ ) {
      fprintf(fts, "(");
      fprint_ft_name(fts, ginitial_state.F[i]);
      fprintf(fts, ")\n");
    }


    /*for preferences goal
    **/

     if(isPreference || gpref_head ){
        WffNode *current1 = gpref_head;
        WffNode *i_son;
        WffNode *current2 = NULL;
        Bool all = TRUE;

    /* test, if initial  contains preference goal
     *  first preference
     */
       if ( current1->sons ) {
	 current2 = current1->sons;

          all = is_State_meet_preference_expression(current2);

          fprintf(fts,"(=(");
                 fprintf(fts, "IS_VIOLATED_%s)",current2->name);

              	if(all){
                   fprintf(fts, " %i", 0);
                   fprintf(fts, ")\n");
		}
                else{
                   fprintf(fts, " %i", 1);
                   fprintf(fts, ")\n");
		}

          fprintf(fts,"(=(");
                 fprintf(fts, "IS_SATISFIED_%s)",current2->name);

              	if(all){
                   fprintf(fts, " %i", 1);
                   fprintf(fts, ")\n");
		}
                else{
                   fprintf(fts, " %i", 0);
                   fprintf(fts, ")\n");
		}

           /*second one and so on preferences
	   **/

      for ( i_son = current1->sons->next; i_son!=NULL; i_son = i_son->next ) {

          /* test, if initial  contains preference goal
	  **/
          all = is_State_meet_preference_expression(i_son);
               fprintf(fts,"(=(");
               fprintf(fts, "IS_VIOLATED_%s)",i_son->name);
		if(all){
                   fprintf(fts, " %i", 0);
                   fprintf(fts, ")\n");
		}
                else{
                   fprintf(fts, " %i", 1);
                   fprintf(fts, ")\n");
		}

               fprintf(fts,"(=(");
               fprintf(fts, "IS_SATISFIED_%s)",i_son->name);
		if(all){
                   fprintf(fts, " %i", 1);
                   fprintf(fts, ")\n");
		}
                else{
                   fprintf(fts, " %i", 0);
                   fprintf(fts, ")\n");
		}

               
              
      }
    }          
  }


  /* value of pref preconds is-violated in initial
  **/
  if(isDomainPreference){
      for ( i = 0; i < gnum_operators; i++ ) {
         WffNode *i_son = NULL;

         if(goperators[i]->pref_preconds){
            if(goperators[i]->pref_preconds->name){
	       /* one pref_preconds*/
               fprintf(fts, "(=(IS_VIOLATED_%s) 0)\n",goperators[i]->pref_preconds->name);
	     }
         else{
	      for (i_son = goperators[i]->pref_preconds->sons; i_son != NULL; i_son = i_son->next ){
                 fprintf(fts, "(=(IS_VIOLATED_%s) 0)\n", i_son->name);

	      }
           }
        }
       }
    }
   


    /* for sync-ordinary
    **/
    if(gconstraints || gconstraints_pref || gwithin_constraints || gwithin_constraints_pref)
               fprintf(fts, "(sync-ordinary)\n");
    /* end for sync-ordinary*/


    fprintf(fts, ")\n");
  }
 

  fprintf(fts, "(:goal\n");
  fprintf(fts, "(and\n");
  for ( i = 0; i < gnum_logic_goal; i++ ) {
    fprintf(fts, "(");
    fprint_ft_name(fts,glogic_goal[i] );
    fprintf(fts, ")\n");
  }

  fprintf(fts, ")\n");
  fprintf(fts, ")\n");



   if ( gparse_metric ) {
      if ( strcmp( gparse_optimization, "MAXIMIZE" ) == SAME )
            fprintf(fts, "(:metric maximize ");
      else
            fprintf(fts, "(:metric minimize ");
      change_ParseExpNode(gparse_metric, NULL);
      remove_not_found_IsViolated_from_ParseExpNode(gparse_metric, NULL);
      fprint_ParseExpNode(fts, gparse_metric);
      fprintf(fts,")\n");
    } 


  fprintf(fts, ")\n");
  fclose( fts);

  if ( (ops = fopen( "groundeddomain.pddl", "w")) == NULL ) {
    printf("\n\nCannot open file groundeddomain.pddl.\n\n");
    exit( 1 );
  }
  fprintf(ops, "(define (domain grounded-STRIPS-%s)\n", gdomain_name);
  fprintf(ops, "(:requirements\n");
  fprintf(ops, ":strips \n");

  if(isPreference)
    fprintf(ops,":fluents\n");

  if(isDomainPreference)
    fprintf(ops,":preferences\n");  

  fprintf(ops, ")\n");

  fprintf(ops, "(:predicates\n");
  
    for ( i = 0; i < gnum_relevant_facts; i++ ) {
      
    fprintf(ops, "(");
    fprint_Fact( ops, &(grelevant_facts[i]) );
    fprintf(ops, ")\n");
  }

    /* supplementar relevant fact
    **/

   relevant_gpref_predicate = (Fact_pointer *) calloc(MAX_PREDICATES, sizeof(Fact_pointer));
    gnum_gpref_predicate = 0;
 /* predicates for gpref_head
    **/
    if(gpref_head)
        generate_gpref_predicate(gpref_head);  

   /* predicates for gconstraints_pref
   **/
    if(gconstraints_pref)
        generate_gpref_predicate(gconstraints_pref);

    /* predicates for gwithin_constraints_pref
    **/
    if(gwithin_constraints_pref)
    generate_gpref_predicate(gwithin_constraints_pref);

    /* predicates for gtil_constraints_pref
    **/
    if(gtil_constraints_pref)
        generate_gpref_predicate(gtil_constraints_pref);
    
     for(i = 0; i < gnum_gpref_predicate; i++){
        fprintf(ops, "(");
        fprint_Fact(ops,relevant_gpref_predicate[i] );
	    fprintf(ops, ")\n");
     }    

    /* predicates for pref preconds
    **/
    if(isDomainPreference){
        Bool found1 = FALSE;
        Bool found2 = FALSE;
        for(i = 0; i < gnum_pref_preconds_predicate; i++){
	   /*++++++++++++++++++++*/
             for ( j = 0; j < gnum_relevant_facts; j++ ) {
                 if(same_Fact(&(grelevant_facts[j]),&(grelevant_facts[relevant_pref_preconds_predicate[i]]) )){
                     found1 = TRUE;
                     break;
		 }
	     }
	     if(!found1){
               for(k = 0; k < gnum_gpref_predicate; k++){
		  if(same_Fact(relevant_gpref_predicate[k],&(grelevant_facts[relevant_pref_preconds_predicate[i]]))){
		      found2 = TRUE;
                      break;
		  }
	       }
	     }
	       if((!found1) && (!found2)){
		     fprintf(ops, "(");
                     fprint_Fact(ops,&(grelevant_facts[relevant_pref_preconds_predicate[i]]) );
	             fprintf(ops, ")\n");
		      		
	     }     
           /*++++++++++++++++++++*/
        }  
       }

  /* end of supplementar relevant fact  */



  fprintf(ops, ")\n");

   if( isPreference || isConstraintsPreference || isDomainPreference){
    fprintf(ops, "(:functions\n");
    if(gpref_head){
     WffNode *current = gpref_head;
     WffNode *i_son;

      if ( current->sons ) {
           fprintf(ops, "(");
           fprintf(ops, "IS_VIOLATED_%s",current->sons->name);
           fprintf(ops, ")\n");
           fprintf(ops, "(");
           fprintf(ops, "IS_SATISFIED_%s",current->sons->name);
           fprintf(ops, ")\n");
	   for ( i_son = current->sons->next; i_son!=NULL; i_son = i_son->next ) {
	       fprintf(ops, "(");
	       fprintf(ops, "IS_VIOLATED_%s",i_son->name);
	       fprintf(ops, ")\n");
               fprintf(ops, "(");
	       fprintf(ops, "IS_SATISFIED_%s",i_son->name);
	       fprintf(ops, ")\n");
	   }
      }
    }

   /* add is_violated preference name from pref_preconds
    **/
      if(isDomainPreference){
       for ( i = 0; i < gnum_operators; i++ ) {
         WffNode *i_son = NULL;

         if(goperators[i]->pref_preconds){
             if(goperators[i]->pref_preconds->name){
	         /* one pref_preconds*/
                  fprintf(ops, "(IS_VIOLATED_%s)\n",goperators[i]->pref_preconds->name);
	     }
         else{
	     for (i_son = goperators[i]->pref_preconds->sons; i_son != NULL; i_son = i_son->next ){
                fprintf(ops, "(IS_VIOLATED_%s)\n", i_son->name);

	      }
           }
         }
       }
   }

     fprintf(ops, ")\n");
     }


 
   /* actions
   */
   for ( a = gactions; a; a = a->next ) {
       if (a->norm_operator || a->pseudo_action ){
            fprint_Action( ops, a );
       }
   }
 
  
  fprintf(ops, ")\n");
  fclose( ops);

}








/**************************************************
 * gorunded strips fluents temporal task*
 **************************************************/









void output_grounded_STRIPS_Fluents_temporal_task( void )

{
  FILE *fts, *ops;
  int i, j, k;
  Action *a;
  char s1[30];
  char s2[30];
  int k1 = 0;
  int k2 = 0;
  int k3 = 0;
  int k4 = 0;

  strcpy(s1, "fact");
  strcpy(s2, "within_fact");

  if ( (fts = fopen( "groundedproblem.pddl", "w")) == NULL ) {
    printf("\n\nCannot open file groundedproblem.pddl.\n\n");
    exit( 1 );
  }
  fprintf(fts, "(define (problem grounded-%s)\n", gproblem_name);
  fprintf(fts, "(:domain grounded-%s)\n", gdomain_name);
  if ( ginitial_state.num_F > 0 ) {
    fprintf(fts, "(:init\n");
    for ( i = 0; i < ginitial_state.num_F; i++ ) {
      fprintf(fts, "(");
      fprint_ft_name(fts, ginitial_state.F[i]);
      fprintf(fts, ")\n");
    }

  
    for ( i = 0; i < gnum_relevant_fluents; i++ ) {
    fprintf(fts,"(=(");
    fprint_fl_name(fts,  i );
    fprintf(fts, ") ");
    if ( ginitial_state.f_D[i] ) {
      fprintf(fts, "%.2f", ginitial_state.f_V[i]);
    } 
    fprintf(fts, ")\n");

  }


     /*for preferences goal
     **/

     if(isPreference || gpref_head){
        WffNode *current1 = gpref_head;
        WffNode *i_son;
        WffNode *current2 = NULL;
        Bool all = TRUE;

    /* test, if initial  contains preference goal
     *  first preference
     */
       if ( current1->sons ) {
	 current2 = current1->sons;

          all = is_State_meet_preference_expression(current2);
            fprintf(fts,"(=(");
                 fprintf(fts, "IS_VIOLATED_%s)",current2->name);

              	if(all){
                   fprintf(fts, " %i", 0);
                   fprintf(fts, ")\n");
		}
                else{
                   fprintf(fts, " %i", 1);
                   fprintf(fts, ")\n");
		}

            fprintf(fts,"(=(");
                 fprintf(fts, "IS_SATISFIED_%s)",current2->name);

              	if(all){
                   fprintf(fts, " %i", 1);
                   fprintf(fts, ")\n");
		}
                else{
                   fprintf(fts, " %i", 0);
                   fprintf(fts, ")\n");
		}
   
           /*second one and so on preferences
	   **/

      for ( i_son = current1->sons->next; i_son!=NULL; i_son = i_son->next ) {

          /* test, if initial  contains preference goal
	  **/
          all = is_State_meet_preference_expression(i_son);
               fprintf(fts,"(=(");
               fprintf(fts, "IS_VIOLATED_%s)",i_son->name);
		if(all){
                   fprintf(fts, " %i", 0);
                   fprintf(fts, ")\n");
		}
                else{
                   fprintf(fts, " %i", 1);
                   fprintf(fts, ")\n");
		}

               fprintf(fts,"(=(");
               fprintf(fts, "IS_SATISFIED_%s)",i_son->name);
		if(all){
                   fprintf(fts, " %i", 1);
                   fprintf(fts, ")\n");
		}
                else{
                   fprintf(fts, " %i", 0);
                   fprintf(fts, ")\n");
		}
              
      }
              
  }
 }


  /* value of pref preconds is-violated in initial
  **/
   if(isDomainPreference){
     for ( i = 0; i < gnum_operators; i++ ) {
        WffNode *i_son = NULL;

        if(goperators[i]->pref_preconds){
            if(goperators[i]->pref_preconds->name){
	       /* one pref_preconds*/
               fprintf(fts, "(=(IS_VIOLATED_%s) 0)\n",goperators[i]->pref_preconds->name);
           }
       else{
	   for (i_son = goperators[i]->pref_preconds->sons; i_son != NULL; i_son = i_son->next ){
              fprintf(fts, "(=(IS_VIOLATED_%s) 0)\n", i_son->name);

	   }
         }
      }
    }
   }




    if(isTempTimeWindows){

    Time_Ini_Literal *lauf = NULL;
    if(Timed_Initial_Literals)
     {
           
      for(lauf=Timed_Initial_Literals;lauf!=NULL;lauf=lauf->next)
       { 
         

         TimeWindows *lauf0=lauf->tw;
         while(lauf0)
         {
          fprintf(fts, "(at "); 
          fprintf(fts, " %.2f ",lauf0->mintime);
          fprintf(fts, "(");
          fprint_hidden_TokenList(fts, lauf->pre,"-");
          fprintf(fts, "))\n");

          fprintf(fts, "(at "); 
          fprintf(fts, " %.2f ",lauf0->maxtime);
          fprintf(fts, "(not (");
          fprint_hidden_TokenList(fts, lauf->pre,"-");
          fprintf(fts, ")))\n");
         
          lauf0=lauf0->next;
        }
              
      }
     }
    }

    /*for timed initial literal  Constraints
    **/
    if(isConstraints){

        if(gtil_constraints){
          WffNode *n = gtil_constraints;
          PlNode *m = til_constraints;
          if(m->connective==AND){
	      k1 = read_gtil_constraints(n, fts);
            }         
          else{
               read_gtil_constraints2(n,0, fts);
            }
	}
       }

    /*for timed initial literal Constraints preference
    **/
    if(isConstraintsPreference){

        if(gtil_constraints_pref){
          WffNode *n = gtil_constraints_pref;
          PlNode *m = til_constraints_pref;
          if(m->connective==AND){
	      k2 = read_gtil_constraints(n, fts);
            }         
          else{
               read_gtil_constraints2(n,0, fts);
            }
	}
        
       }

    /*end for timed initial literal constraints and constraints preference*/

    /*create ltl and til-fact for within constraints
    **/
    if(isConstraints){
        if(gwithin_constraints){
          printf("\n...Reading within constraints...\n");
          WffNode *n = gwithin_constraints;
          PlNode *m = within_constraints;
          if(m->connective==AND){
	      k3 = read_gwithin_constraints(n,s2, fts);
            }         
          else{
               read_gwithin_constraints2(n,0,s2, fts, TRUE, NULL);
            }
	}
       }

    /*create ltl and til-fact for within constraints preference
    **/
    if(isConstraintsPreference){
        printf("\n...Reading within constraints preference...\n");
        if(gwithin_constraints_pref){
          WffNode *n = gwithin_constraints_pref;
          PlNode *m = within_constraints_pref;

          if(m->connective==AND){
	      k4 = read_gwithin_constraints(n,s2, fts);
            }         
          else{
               read_gwithin_constraints2(n,0,s2, fts, TRUE, NULL);
            }
	}
        
       }
  

    /* for sync-ordinary
    **/
    if(gconstraints || gconstraints_pref || gwithin_constraints || gwithin_constraints_pref)
               fprintf(fts, "(sync-ordinary)\n");
    /* end for sync-ordinary*/

    fprintf(fts, ")\n");
  }
  fprintf(fts, "(:goal\n");
  fprintf(fts, "(and\n");
  for ( i = 0; i < gnum_logic_goal; i++ ) {
    fprintf(fts, "(");
    fprint_ft_name(fts,glogic_goal[i] );
    fprintf(fts, ")\n");
  }


    /*supplementar goal for timed initial literal from Constraints
    **/

    if(isConstraints){
        char s[40];
        if(gtil_constraints){
              for ( i = 0; i < k1; i++ ) {
                sprintf(s,"%d", i);
                fprintf(fts, "(done-%s)\n", s);
               }
             }
      }
   
     /* end for timed initial literal constraints */
   




   for ( i = 0; i < gnum_numeric_goal; i++ ) {
      switch ( gnumeric_goal_comp[i] ) {
      case LE:
	fprintf(fts, "(< ");
	break;
      case LEQ:
	fprintf(fts, "(<= ");
	break;
      case EQ:
	fprintf(fts, "(= ");
	break;
      case GEQ:
	fprintf(fts, "(>= ");
	break;
      case GE:
	fprintf(fts, "(> ");
	break;
      default:
	printf("\nwrong comparator in gnumeric_goal %d\n\n", gnumeric_goal_comp[i]);
	exit( 1 );
      }
      fprint_ExpNode(fts,  gnumeric_goal_lh[i] );
      fprint_ExpNode(fts,  gnumeric_goal_rh[i] );
      fprintf(fts, ")\n");
    }

  fprintf(fts, ")\n");
  fprintf(fts, ")\n");

  
   if ( gparse_metric ) {
      if ( strcmp( gparse_optimization, "MAXIMIZE" ) == SAME )
            fprintf(fts, "(:metric maximize ");
      else
            fprintf(fts, "(:metric minimize ");
      change_ParseExpNode(gparse_metric, NULL);
      remove_not_found_IsViolated_from_ParseExpNode(gparse_metric, NULL);
      fprint_ParseExpNode(fts, gparse_metric);
      fprintf(fts,")\n");
    } 



  fprintf(fts, ")\n");
  fclose( fts);

  if ( (ops = fopen( "groundeddomain.pddl", "w")) == NULL ) {
    printf("\n\nCannot open file groundeddomain.pddl.\n\n");
    exit( 1 );
  }
  fprintf(ops, "(define (domain grounded-%s)\n", gdomain_name);
  fprintf(ops, "(:requirements\n");
  fprintf(ops, ":typing\n");
  if(isfluents)
    fprintf(ops,":fluents\n");
  if(isDurative)
    fprintf(ops, ":durative-actions\n");
  if(isTempTimeWindows)
    fprintf(ops, ":timed-initial-literals\n");
  fprintf(ops, ")\n");

  fprintf(ops, "(:predicates\n");
  
    for ( i = 0; i < gnum_relevant_facts; i++ ) {
      
    fprintf(ops, "(");
    fprint_Fact( ops, &(grelevant_facts[i]) );
    fprintf(ops, ")\n");
  }

    /* supplementar relevant fact
    **/

   relevant_gpref_predicate = (Fact_pointer *) calloc(MAX_PREDICATES, sizeof(Fact_pointer));
    gnum_gpref_predicate = 0;
 /* predicates for gpref_head
    **/
    if(gpref_head)
        generate_gpref_predicate(gpref_head);  

   /* predicates for gconstraints_pref
   **/
    if(gconstraints_pref)
        generate_gpref_predicate(gconstraints_pref);

    /* predicates for gwithin_constraints_pref
    **/
    if(gwithin_constraints_pref)
    generate_gpref_predicate(gwithin_constraints_pref);

    /* predicates for gtil_constraints_pref
    **/
    if(gtil_constraints_pref)
        generate_gpref_predicate(gtil_constraints_pref);
    
     for(i = 0; i < gnum_gpref_predicate; i++){
        fprintf(ops, "(");
        fprint_Fact(ops,relevant_gpref_predicate[i] );
	    fprintf(ops, ")\n");
     }    

    /* predicates for pref preconds
    **/
    if(isDomainPreference){
        Bool found1 = FALSE;
        Bool found2 = FALSE;
        for(i = 0; i < gnum_pref_preconds_predicate; i++){
	   /*++++++++++++++++++++*/
             for ( j = 0; j < gnum_relevant_facts; j++ ) {
                 if(same_Fact(&(grelevant_facts[j]),&(grelevant_facts[relevant_pref_preconds_predicate[i]]) )){
                     found1 = TRUE;
                     break;
		 }
	     }
	     if(!found1){
               for(k = 0; k < gnum_gpref_predicate; k++){
		  if(same_Fact(relevant_gpref_predicate[k],&(grelevant_facts[relevant_pref_preconds_predicate[i]]))){
		      found2 = TRUE;
                      break;
		  }
	       }
	     }
	       if((!found1) && (!found2)){
		     fprintf(ops, "(");
                     fprint_Fact(ops,&(grelevant_facts[relevant_pref_preconds_predicate[i]]) );
	             fprintf(ops, ")\n");
		      		
	     }     
           /*++++++++++++++++++++*/
        }  
       }
    /* end of supplementar relevant fact  */


  
    /*predicates for timed initial literal from Constraints
    **/

    if(isConstraints){
        char s[40];
        if(gtil_constraints){
              for ( i = 0; i < k1; i++ ) {
                sprintf(s,"%d", i);
                fprintf(ops, "(fact-%s)\n", s);
                fprintf(ops, "(done-%s)\n", s);
               }
             }
      }

    /*predicates for timed initial literal Constraints preference
    **/
    if(isConstraintsPreference){
        if(gtil_constraints_pref){
           WffNode *n = gtil_constraints_pref;
           PlNode *m = til_constraints_pref;
          if(m->connective==AND){
	       add_til_and_within_constraints_pref_fact_to_predicates(n,s1,ops);
            }         
          else{
               add_til_and_within_constraints_pref_fact_to_predicates2(n,0,s1,ops);
            }    
	}
       }

   
     /* end for timed initial literal constraints and constraints preference */

    /*predicates for within Constraints
    **/
    if(isConstraints){
        char s[40];
        if(gwithin_constraints){
              for ( i = 0; i < k3; i++ ) {
                sprintf(s,"wfact-%d", i);
                fprintf(ops, "(%s)\n", s);
               }
             }
      }

    /*predicates for within  Constraints preference
    **/
    if(isConstraintsPreference){
        if(gwithin_constraints_pref){
           WffNode *n = gwithin_constraints_pref;
           PlNode *m = within_constraints_pref;
          if(m->connective==AND){
	       add_til_and_within_constraints_pref_fact_to_predicates(n,s1,ops);
            }         
          else{
               add_til_and_within_constraints_pref_fact_to_predicates2(n,0,s1,ops);
            }    
	}
       }     
    /* end for within constraints and constraints preference */


    
    if(isTempTimeWindows){

	int m = 0;
        while(twpredicate[m]!=NULL){  
        fprintf(ops, "(");
	fprint_hidden_TokenList(ops, twpredicate[m], "-");
        fprintf(ops, ")\n");
        m++;

	}     
     }


  fprintf(ops, ")\n");

  if(gnum_relevant_fluents || isDomainPreference || isPreference || isConstraintsPreference){
    fprintf(ops, "(:functions\n");
  
    for ( i = 0; i < gnum_relevant_fluents; i++ ) {
       
    fprintf(ops, "(");
    fprint_Fluent( ops, &(grelevant_fluents[i]) );
    fprintf(ops, ")\n");
  }

   /* add is_violated preference name from pref_preconds
   **/
   int i;
   for ( i = 0; i < gnum_operators; i++ ) {
    WffNode *i_son = NULL;

    if(goperators[i]->pref_preconds){
        if(goperators[i]->pref_preconds->name){
	    /* one pref_preconds*/
            fprintf(ops, "(IS_VIOLATED_%s)\n",goperators[i]->pref_preconds->name);
	}
    else{
	for (i_son = goperators[i]->pref_preconds->sons; i_son != NULL; i_son = i_son->next ){
            fprintf(ops, "(IS_VIOLATED_%s)\n", i_son->name);

	}
      }
    }
   }


  /*for preferences from goal
  **/
  if(isPreference || gpref_head){
    WffNode *current = gpref_head;
    WffNode *i_son;

      if ( current->sons ) {
           fprintf(ops, "(");
           fprintf(ops, "IS_VIOLATED_%s",current->sons->name);
           fprintf(ops, ")\n");
           fprintf(ops, "(");
           fprintf(ops, "IS_SATISFIED_%s",current->sons->name);
           fprintf(ops, ")\n");
      for ( i_son = current->sons->next; i_son!=NULL; i_son = i_son->next ) {
	   fprintf(ops, "(");
           fprintf(ops, "IS_VIOLATED_%s",i_son->name);
           fprintf(ops, ")\n");
           fprintf(ops, "(");
           fprintf(ops, "IS_SATISFIED_%s",i_son->name);
           fprintf(ops, ")\n");
	
      }
    }
  }
  /*add is-violated-pref for timed initial literal constraints preference
  * in function
  **/
  if(isConstraintsPreference){
      if(gtil_constraints_pref){

           WffNode *n = gtil_constraints_pref;
           PlNode *m = til_constraints_pref;
          if(m->connective==AND){
	       add_is_violated_til_constraints_pref_to_function(n,ops);
            }         
          else{
               add_is_violated_til_constraints_pref_to_function2(n,0,ops);
            }    
      }
  }


  fprintf(ops, ")\n");
  }

  /* actions
   */

    /* for normal actions
    **/
  
   for ( a = gactions; a; a = a->next ) {
       
/*        if (!a->norm_operator && !a->pseudo_action ) */
	    fprint_Action( ops, a );
	
   }

   
  /*for timed initial literal Constraints actions
    **/
    if(isConstraints){
        if(gtil_constraints){
          WffNode *n = gtil_constraints;
          PlNode *m = til_constraints;
          if(m->connective==AND){
	      create_operator_for_gtil_constraints(n,s1, ops);
            }         
          else{
               create_operator_for_gtil_constraints2(n,0,s1, ops);
            }
	}
       }

    /*for timed initial literal Constraints preference actions
    **/

    if(isConstraintsPreference){
        if(gtil_constraints_pref){
          WffNode *n = gtil_constraints_pref;
          PlNode *m = til_constraints_pref;
          if(m->connective==AND){
	       create_operator_for_gtil_constraints(n,s1, ops);
            }         
          else{
               create_operator_for_gtil_constraints2(n,0,s1, ops);
            }
	}
       }

    /* end for timed initial literal constraints and constraints preference */



  fprintf(ops, ")\n");
  fclose( ops);

}











/*******************************************************************
 * help functions*
 *******************************************************************/








void fprint_ParseExpNode( FILE *out, ParseExpNode *n )

{

  if ( !n ) return;

  switch ( n->connective) {
  case AD:
    fprintf(out, "(+ ");
    fprint_ParseExpNode(out,  n->leftson );
    fprint_ParseExpNode(out,  n->rightson );
    fprintf(out, ")");
    break;
  case SU:
    fprintf(out, "(- ");
    fprint_ParseExpNode(out, n->leftson );
    fprint_ParseExpNode(out, n->rightson );
    fprintf(out, ")");
    break;
  case MU:
    fprintf(out, "(* ");
    fprint_ParseExpNode(out, n->leftson );
    fprint_ParseExpNode(out, n->rightson );
    fprintf(out, ")");
    break;
  case DI:
    fprintf(out, "(/ ");
    fprint_ParseExpNode( out, n->leftson );
    fprint_ParseExpNode( out, n->rightson );
    fprintf(out, ")");
    break;
  case MINUS:
    fprintf(out, "(- ");
    fprint_ParseExpNode(out, n->leftson );
    fprintf(out, ")");
    break;
  case NUMBER:
    fprintf(out, "%s", n->atom->item);
    break;
  case FHEAD:
      fprintf(out, "(");
      fprint_hidden_TokenList(out, n->atom, " ");
      fprintf(out, ")");
      break;
  default:
    printf("\n\nprint Parseexpnode: wrong specifier %d",
	   n->connective);
  }

}


void fprint_FinalExpNode( FILE *out, ParseExpNode *n )

{

  if ( !n ) return;

  switch ( n->connective) {
  case AD:
    fprintf(out, "(+ ");
    fprint_FinalExpNode(out,  n->leftson );
    fprint_FinalExpNode(out,  n->rightson );
    fprintf(out, ")");
    break;
  case SU:
    fprintf(out, "(- ");
    fprint_FinalExpNode(out, n->leftson );
    fprint_FinalExpNode(out, n->rightson );
    fprintf(out, ")");
    break;
  case MU:
    fprintf(out, "(* ");
    fprint_FinalExpNode(out, n->leftson );
    fprint_FinalExpNode(out, n->rightson );
    fprintf(out, ")");
    break;
  case DI:
    fprintf(out, "(/ ");
    fprint_FinalExpNode( out, n->leftson );
    fprint_FinalExpNode( out, n->rightson );
    fprintf(out, ")");
    break;
  case MINUS:
    fprintf(out, "(- ");
    fprint_FinalExpNode(out, n->leftson );
    fprintf(out, ")");
    break;
  case NUMBER:
    fprintf(out, "%s", n->atom->item);
    break;
  case FHEAD:
      if (!strcmp(n->atom->item,"TOTAL-TIME")) {
	  fprintf(out, " 0");
      }  else {
	  fprintf(out, "(");
	  fprint_hidden_TokenList(out, n->atom, " ");
	  fprintf(out, ")");
      }
    break;
  default:
    printf("\n\nprint Parseexpnode: wrong specifier %d",
	   n->connective);
  }

}




void print_FinalExpNode( ParseExpNode *n )

{

  if ( !n ) return;

  switch ( n->connective) {
  case AD:
    printf( "(+ ");
    print_FinalExpNode( n->leftson );
    print_FinalExpNode( n->rightson );
    printf( ")");
    break;
  case SU:
    printf( "(- ");
    print_FinalExpNode( n->leftson );
    print_FinalExpNode( n->rightson );
    printf(")");
    break;
  case MU:
    printf( "(* ");
    print_FinalExpNode( n->leftson );
    print_FinalExpNode( n->rightson );
    printf( ")");
    break;
  case DI:
    printf("(/ ");
    print_FinalExpNode( n->leftson );
    print_FinalExpNode( n->rightson );
    printf(")");
    break;
  case MINUS:
    printf( "(- ");
    print_FinalExpNode( n->leftson );
    printf( ")");
    break;
  case NUMBER:
    printf( "%s", n->atom->item);
    break;
  case FHEAD:
      if (!strcmp(n->atom->item,"TOTAL-TIME")) {
	  printf( " 0");
      }  else {
	  printf( "(");
	  print_hidden_TokenList( n->atom, " ");
	  printf( ")");
      }
    break;
  default:
    printf("\n\nprint Parseexpnode: wrong specifier %d",
	   n->connective);
  }

}






void change_ParseExpNode( ParseExpNode *n , ParseExpNode *father)

{

   ParseExpNode *n1;  

  if ( !n ) return;

  switch ( n->connective) {
  case AD:
    n1 = n->leftson;
    if((n->rightson->connective == AD) || (n->rightson->connective == SU))
        change_ParseExpNode(n->rightson, n);
    else
         fluent_have_new_name(n, n->leftson, n->rightson,father );
           
    change_ParseExpNode(n1, n );
    break;
  case SU:
    n1 = n->leftson;
    if((n->rightson->connective == AD) || (n->rightson->connective == SU))
        change_ParseExpNode(n->rightson, n);
    else
       fluent_have_new_name(n, n->leftson, n->rightson, father);
  
    change_ParseExpNode(n1, n );
    break;
  case MU:
  case DI:
  case MINUS:
  case NUMBER:
  case FHEAD:
    break;
  default:
    printf("\n\nprint Parseexpnode: wrong specifier %d",
	   n->connective);
  }

}


void remove_not_found_IsViolated_from_ParseExpNode( ParseExpNode *n , ParseExpNode *father)

{

   ParseExpNode *n1, *n2;  
  if ( !n ) return;

  switch ( n->connective) {
  case AD:
  case SU:
      n1 = n->leftson;
      n2 = n->rightson;
      switch ( n->found ) {
	  case 1:
        	if(father){
                    father->leftson = n1;
                    remove_not_found_IsViolated_from_ParseExpNode(father->leftson, father );
         	}
               else{
                   n->found = n1->found;
                   n->rightson = n1->rightson;
	           n->leftson = n1->leftson;
                   remove_not_found_IsViolated_from_ParseExpNode(n, father );
	        }
	      break;
          case 2:
	     if(father){
                free_ParseExpNode(n);
		father = new_ParseExpNode(NUMBER);
                father->atom = new_TokenList();
                father->atom->item = new_Token(30);
                strcpy(father->atom->item, "0");
	     }
	     else
		n = NULL;
	      break;
          case 3:
              	if(father){
                    father->leftson = n2;
		}
                else{
		    n = n2;
		    }
	      break;
          default:
              remove_not_found_IsViolated_from_ParseExpNode(n1, n );
	      break;
      }
    break;
  case MU:
  case DI:
  case MINUS:
  case NUMBER:
  case FHEAD:
    break;
  default:
    printf("\n\nprint Parseexpnode: wrong specifier %d",
	   n->connective);
  }

}



void  fluent_have_new_name(ParseExpNode *n, ParseExpNode *n1, ParseExpNode *n2, ParseExpNode *father)

{
    char str1[] = "IS-VIOLATED";
    char str2[] = "IS-SATISFIED";
    ParseExpNode *node2 = n;
    ParseExpNode *nn2, *nn1;
    int found = 0;
    int found2 = 0;
    Bool var;
   
    if((!n1) || (!n2))
        return;
    
    nn1 = copy_ParseExpNode(n1 );
    nn2 = copy_ParseExpNode(n2 );
    /*
    **/
    if((n2->connective == MU) || (n2->connective == DI)){
        
     var = FALSE; 

    /* for IS-VIOLATED
     * rightson
    **/
    if(n2->rightson->connective == FHEAD){

    /* IS-VIOLATED
    **/
    if(strncmp(str1,n2->rightson->atom->item, 10) == 0){
     found = 1;
      /* 1.preferences goal
      **/
        if(gpref_head){
          WffNode *m1 = gpref_head;
          PlNode *m = pref_head;

          WffNode *i;
          char s[30];
          char s2[40];
          char s3[40];
                 
           strcpy(s3,n2->rightson->atom->item );
          if(m->connective==AND){
                  sprintf(s2,"IS-VIOLATED-%s",m1->sons->name);
                   if(same_pref_name(s2, s3) == 0){
                      found = 2;
		   }
		  if(same_pref_name(s2, s3) == 1){
                      found = 3;
                      var = TRUE;
		      sprintf(s,"IS-VIOLATED-%s",m1->sons->new_name);
                      n2->rightson->atom = new_TokenList();
                      n2->rightson->atom->item = new_Token(30);
                      strcpy(n2->rightson->atom->item, s);
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-VIOLATED-%s",i->name);
                    if(same_pref_name(s2, s3) == 0){
                        found = 2;
		     }
		    if( same_pref_name(s2, s3) == 1){
                     found = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-VIOLATED-%s",i->new_name);
                         n2->rightson->atom = new_TokenList();
                         n2->rightson->atom->item = new_Token(30);
                         strcpy(n2->rightson->atom->item, s);
		      }
		     else{      
                 
                      ParseExpNode *node = new_ParseExpNode(AD);
                      if(nn2->connective == MU)
                         node->rightson = new_ParseExpNode(MU);
                      if(nn2->connective == DI)
                         node->rightson = new_ParseExpNode(DI);
                      node->rightson->leftson = new_ParseExpNode(nn2->leftson->connective);
                      node->rightson->leftson->atom = copy_TokenList(nn2->leftson->atom);
                      sprintf(s,"IS-VIOLATED-%s",i->new_name);
                      node->rightson->rightson = new_ParseExpNode(nn2->rightson->connective);
                     
                      node->rightson->rightson->atom = new_TokenList();
                      node->rightson->rightson->atom->item = (char *) calloc(30, sizeof(char));
                      strcpy(node->rightson->rightson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;                    
		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-VIOLATED-%s",m1->name);
                  if(same_pref_name(s2, n2->rightson->atom->item) == 0){
                      found = 2;
		  }
		  if(same_pref_name(s2, n2->rightson->atom->item) == 1){
                       found = 3;
		      sprintf(s,"IS-VIOLATED-%s",m1->new_name);
                      strcpy(n2->rightson->atom->item, s);
		   
		  }
            }

          if(found == 2 ||  found == 3)
	      goto spring;

	}  
	/* end preference goal*/
        /* 2.normal constraints preference
	**/
        if(gconstraints_pref){
          WffNode *m1 = gconstraints_pref;
          PlNode *m =  pref_constraints_head;

          WffNode *i;
          char s[30];
          char s2[40];
          char s3[40];
                 
          strcpy(s3,n2->rightson->atom->item );  
          if(m->connective==AND){
                sprintf(s2,"IS-VIOLATED-%s",m1->sons->new_name);
                  if(same_pref_name(s2, s3) == 0){
                      found = 2;
		  }
		  if(same_pref_name(s2, s3) == 1){
                       found = 3;
                      var = TRUE;
		      sprintf(s,"IS-VIOLATED-%s",m1->sons->new_name);
                      n2->rightson->atom = new_TokenList();
                      n2->rightson->atom->item = new_Token(30);
                      strcpy(n2->rightson->atom->item, s);
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
              
                   sprintf(s2,"IS-VIOLATED-%s",i->new_name);
                    if( same_pref_name(s2, s3) == 0){
                        found = 2;
		     }
		    if( same_pref_name(s2, s3) == 1){
                      found = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-VIOLATED-%s",i->new_name);
                         n2->rightson->atom = new_TokenList();
                         n2->rightson->atom->item = new_Token(30);
                         strcpy(n2->rightson->atom->item, s);
		      }
		     else{      
                 
                      ParseExpNode *node = new_ParseExpNode(AD);
                      if(nn2->connective == MU)
                         node->rightson = new_ParseExpNode(MU);
                      if(nn2->connective == DI)
                         node->rightson = new_ParseExpNode(DI);
                      node->rightson->leftson = new_ParseExpNode(nn2->leftson->connective);
                      node->rightson->leftson->atom = copy_TokenList( nn2->leftson->atom);
                      sprintf(s,"IS-VIOLATED-%s",i->new_name);
                      node->rightson->rightson = new_ParseExpNode(nn2->rightson->connective);
                     
                      node->rightson->rightson->atom = new_TokenList();
                      node->rightson->rightson->atom->item = (char *) calloc(30, sizeof(char));
                      strcpy(node->rightson->rightson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;                    
		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-VIOLATED-%s",m1->new_name);
                   if(same_pref_name(s2, n2->rightson->atom->item) == 0){
                      found = 2;
		   }
		  if(same_pref_name(s2, n2->rightson->atom->item) == 1){
                      found = 3;
		      sprintf(s,"IS-VIOLATED-%s",m1->new_name);
                      strcpy(n2->rightson->atom->item, s);
		   
		  }
            }

	  if(found == 2 ||  found == 3)
	       goto spring;

	}

        /*end normal constraints preference*/
        /* 3.within constraints preference
	**/
	if(gwithin_constraints_pref){

          WffNode *m1 = gwithin_constraints_pref;
          PlNode *m =  within_constraints_pref;

          WffNode *i;
          char s[30];
          char s2[40];
          char s3[40];
                 
          strcpy(s3,n2->rightson->atom->item );  
          if(m->connective==AND){
                  sprintf(s2,"IS-VIOLATED-%s",m1->sons->new_name);
                   if(same_pref_name(s2, s3) == 0){
                      found = 2;
		   }
		  if(same_pref_name(s2, s3) == 1){
                      found = 3;
                      var = TRUE;
		      sprintf(s,"IS-VIOLATED-%s",m1->sons->new_name);
                      n2->rightson->atom = new_TokenList();
                      n2->rightson->atom->item = new_Token(30);
                      strcpy(n2->rightson->atom->item, s);
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-VIOLATED-%s",i->new_name);
                     if(same_pref_name(s2, s3) == 0){
                         found = 2;
		     }
		    if( same_pref_name(s2, s3) == 1){
                      found = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-VIOLATED-%s",i->new_name);
                         n2->rightson->atom = new_TokenList();
                         n2->rightson->atom->item = new_Token(30);
                         strcpy(n2->rightson->atom->item, s);
		      }
		     else{      
                 
                      ParseExpNode *node = new_ParseExpNode(AD);
                      if(nn2->connective == MU)
                         node->rightson = new_ParseExpNode(MU);
                      if(nn2->connective == DI)
                         node->rightson = new_ParseExpNode(DI);
                      node->rightson->leftson = new_ParseExpNode(nn2->leftson->connective);
                      node->rightson->leftson->atom = copy_TokenList(nn2->leftson->atom);
                      sprintf(s,"IS-VIOLATED-%s",i->new_name);
                      node->rightson->rightson = new_ParseExpNode(nn2->rightson->connective);
                     
                      node->rightson->rightson->atom = new_TokenList();
                      node->rightson->rightson->atom->item = (char *) calloc(30, sizeof(char));
                      strcpy(node->rightson->rightson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;                    
		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-VIOLATED-%s",m1->new_name);
		  if(same_pref_name(s2, n2->rightson->atom->item) == 0){
                      found = 2;
		  }
                   if(same_pref_name(s2, n2->rightson->atom->item) == 1){
                       found = 3;
		      sprintf(s,"IS-VIOLATED-%s",m1->new_name);
                      strcpy(n2->rightson->atom->item, s);
		   
		  }
            }

	  if(found == 2 ||  found == 3)
               goto spring;

	}
        /*end within constraints preference*/         
        /* 4.til constraints preference
	**/
	if(gtil_constraints_pref){
          WffNode *m1 = gtil_constraints_pref;
          PlNode *m =  til_constraints_pref;

          WffNode *i;
          char s[30];
          char s2[40];
          char s3[40];
                 
          strcpy(s3,n2->rightson->atom->item );  
          if(m->connective==AND){
                  sprintf(s2,"IS-VIOLATED-%s",m1->sons->new_name);
                   if(same_pref_name(s2, s3) == 0){
                      found = 2;
		   }
		  if(same_pref_name(s2, s3) == 1){
                       found = 3;
                      var = TRUE;
		      sprintf(s,"IS-VIOLATED-%s",m1->sons->new_name);
                      n2->rightson->atom = new_TokenList();
                      n2->rightson->atom->item = new_Token(30);
                      strcpy(n2->rightson->atom->item, s);
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-VIOLATED-%s",i->new_name);
                    if( same_pref_name(s2, s3) == 0){
                        found = 2;
		     }
		    if( same_pref_name(s2, s3) == 1){
                      found = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-VIOLATED-%s",i->new_name);
                         n2->rightson->atom = new_TokenList();
                         n2->rightson->atom->item = new_Token(30);
                         strcpy(n2->rightson->atom->item, s);
		      }
		     else{      
                 
                      ParseExpNode *node = new_ParseExpNode(AD);
                      if(nn2->connective == MU)
                         node->rightson = new_ParseExpNode(MU);
                      if(nn2->connective == DI)
                         node->rightson = new_ParseExpNode(DI);
                      node->rightson->leftson = new_ParseExpNode(nn2->leftson->connective);
                      node->rightson->leftson->atom = copy_TokenList(nn2->leftson->atom);
                      sprintf(s,"IS-VIOLATED-%s",i->new_name);
                      node->rightson->rightson = new_ParseExpNode(nn2->rightson->connective);
                     
                      node->rightson->rightson->atom = new_TokenList();
                      node->rightson->rightson->atom->item = (char *) calloc(30, sizeof(char));
                      strcpy(node->rightson->rightson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;                    
		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-VIOLATED-%s",m1->new_name);
                  if(same_pref_name(s2, n2->rightson->atom->item) == 0){
                      found = 2;
		  }
		  if(same_pref_name(s2, n2->rightson->atom->item) == 1){
                       found = 3;
		      sprintf(s,"IS-VIOLATED-%s",m1->new_name);
                      strcpy(n2->rightson->atom->item, s);
		   
		  }
            }

	  if(found == 2 ||  found == 3)
	       goto spring;

	}
        /*end til constraints preference*/         

    }


    /* IS-SATISFIED
    **/
    if(strncmp(str2,n2->rightson->atom->item, 11) == 0){
     found = 1;
      /* 1.preferences goal
      **/
        if(gpref_head){
          WffNode *m1 = gpref_head;
          PlNode *m = pref_head;

          WffNode *i;
          char s[30];
          char s2[40];
          char s3[40];
                 
           strcpy(s3,n2->rightson->atom->item );
          if(m->connective==AND){
                  sprintf(s2,"IS-SATISFIED-%s",m1->sons->name);
                   if(same_pref_name(s2, s3) == 0){
                      found = 2;
		   }
		  if(same_pref_name(s2, s3) == 1){
                      found = 3;
                      var = TRUE;
		      sprintf(s,"IS-SATISFIED-%s",m1->sons->new_name);
                      n2->rightson->atom = new_TokenList();
                      n2->rightson->atom->item = new_Token(30);
                      strcpy(n2->rightson->atom->item, s);
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-SATISFIED-%s",i->name);
                    if(same_pref_name(s2, s3) == 0){
                        found = 2;
		     }
		    if( same_pref_name(s2, s3) == 1){
                     found = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-SATISFIED-%s",i->new_name);
                         n2->rightson->atom = new_TokenList();
                         n2->rightson->atom->item = new_Token(30);
                         strcpy(n2->rightson->atom->item, s);
		      }
		     else{      
                 
                      ParseExpNode *node = new_ParseExpNode(AD);
                      if(nn2->connective == MU)
                         node->rightson = new_ParseExpNode(MU);
                      if(nn2->connective == DI)
                         node->rightson = new_ParseExpNode(DI);
                      node->rightson->leftson = new_ParseExpNode(nn2->leftson->connective);
                      node->rightson->leftson->atom = copy_TokenList(nn2->leftson->atom);
                      sprintf(s,"IS-SATISFIED-%s",i->new_name);
                      node->rightson->rightson = new_ParseExpNode(nn2->rightson->connective);
                     
                      node->rightson->rightson->atom = new_TokenList();
                      node->rightson->rightson->atom->item = (char *) calloc(30, sizeof(char));
                      strcpy(node->rightson->rightson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;                    
		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-SATISFIED-%s",m1->name);
                  if(same_pref_name(s2, n2->rightson->atom->item) == 0){
                      found = 2;
		  }
		  if(same_pref_name(s2, n2->rightson->atom->item) == 1){
                       found = 3;
		      sprintf(s,"IS-SATISFIED-%s",m1->new_name);
                      strcpy(n2->rightson->atom->item, s);
		   
		  }
            }

          if(found == 2 ||  found == 3)
	      goto spring;

	}  
	/* end preference goal*/
        /* 2.normal constraints preference
	**/
        if(gconstraints_pref){
          WffNode *m1 = gconstraints_pref;
          PlNode *m =  pref_constraints_head;

          WffNode *i;
          char s[30];
          char s2[40];
          char s3[40];
                 
          strcpy(s3,n2->rightson->atom->item );  
          if(m->connective==AND){
                sprintf(s2,"IS-SATISFIED-%s",m1->sons->new_name);
                  if(same_pref_name(s2, s3) == 0){
                      found = 2;
		  }
		  if(same_pref_name(s2, s3) == 1){
                       found = 3;
                      var = TRUE;
		      sprintf(s,"IS-SATISFIED-%s",m1->sons->new_name);
                      n2->rightson->atom = new_TokenList();
                      n2->rightson->atom->item = new_Token(30);
                      strcpy(n2->rightson->atom->item, s);
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
              
                   sprintf(s2,"IS-SATISFIED-%s",i->new_name);
                    if( same_pref_name(s2, s3) == 0){
                        found = 2;
		     }
		    if( same_pref_name(s2, s3) == 1){
                      found = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-SATISFIED-%s",i->new_name);
                         n2->rightson->atom = new_TokenList();
                         n2->rightson->atom->item = new_Token(30);
                         strcpy(n2->rightson->atom->item, s);
		      }
		     else{      
                 
                      ParseExpNode *node = new_ParseExpNode(AD);
                      if(nn2->connective == MU)
                         node->rightson = new_ParseExpNode(MU);
                      if(nn2->connective == DI)
                         node->rightson = new_ParseExpNode(DI);
                      node->rightson->leftson = new_ParseExpNode(nn2->leftson->connective);
                      node->rightson->leftson->atom = copy_TokenList( nn2->leftson->atom);
                      sprintf(s,"IS-SATISFIED-%s",i->new_name);
                      node->rightson->rightson = new_ParseExpNode(nn2->rightson->connective);
                     
                      node->rightson->rightson->atom = new_TokenList();
                      node->rightson->rightson->atom->item = (char *) calloc(30, sizeof(char));
                      strcpy(node->rightson->rightson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;                    
		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-SATISFIED-%s",m1->new_name);
                   if(same_pref_name(s2, n2->rightson->atom->item) == 0){
                      found = 2;
		   }
		  if(same_pref_name(s2, n2->rightson->atom->item) == 1){
                      found = 3;
		      sprintf(s,"IS-SATISFIED-%s",m1->new_name);
                      strcpy(n2->rightson->atom->item, s);
		   
		  }
            }

	  if(found == 2 ||  found == 3)
	       goto spring;

	}

        /*end normal constraints preference*/
        /* 3.within constraints preference
	**/
	if(gwithin_constraints_pref){

          WffNode *m1 = gwithin_constraints_pref;
          PlNode *m =  within_constraints_pref;

          WffNode *i;
          char s[30];
          char s2[40];
          char s3[40];
                 
          strcpy(s3,n2->rightson->atom->item );  
          if(m->connective==AND){
                  sprintf(s2,"IS-SATISFIED-%s",m1->sons->new_name);
                   if(same_pref_name(s2, s3) == 0){
                      found = 2;
		   }
		  if(same_pref_name(s2, s3) == 1){
                      found = 3;
                      var = TRUE;
		      sprintf(s,"IS-SATISFIED-%s",m1->sons->new_name);
                      n2->rightson->atom = new_TokenList();
                      n2->rightson->atom->item = new_Token(30);
                      strcpy(n2->rightson->atom->item, s);
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-SATISFIED-%s",i->new_name);
                     if(same_pref_name(s2, s3) == 0){
                         found = 2;
		     }
		    if( same_pref_name(s2, s3) == 1){
                      found = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-SATISFIED-%s",i->new_name);
                         n2->rightson->atom = new_TokenList();
                         n2->rightson->atom->item = new_Token(30);
                         strcpy(n2->rightson->atom->item, s);
		      }
		     else{      
                 
                      ParseExpNode *node = new_ParseExpNode(AD);
                      if(nn2->connective == MU)
                         node->rightson = new_ParseExpNode(MU);
                      if(nn2->connective == DI)
                         node->rightson = new_ParseExpNode(DI);
                      node->rightson->leftson = new_ParseExpNode(nn2->leftson->connective);
                      node->rightson->leftson->atom = copy_TokenList(nn2->leftson->atom);
                      sprintf(s,"IS-SATISFIED-%s",i->new_name);
                      node->rightson->rightson = new_ParseExpNode(nn2->rightson->connective);
                     
                      node->rightson->rightson->atom = new_TokenList();
                      node->rightson->rightson->atom->item = (char *) calloc(30, sizeof(char));
                      strcpy(node->rightson->rightson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;                    
		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-SATISFIED-%s",m1->new_name);
		  if(same_pref_name(s2, n2->rightson->atom->item) == 0){
                      found = 2;
		  }
                   if(same_pref_name(s2, n2->rightson->atom->item) == 1){
                       found = 3;
		      sprintf(s,"IS-SATISFIED-%s",m1->new_name);
                      strcpy(n2->rightson->atom->item, s);
		   
		  }
            }

	  if(found == 2 ||  found == 3)
               goto spring;

	}
        /*end within constraints preference*/         
        /* 4.til constraints preference
	**/
	if(gtil_constraints_pref){
          WffNode *m1 = gtil_constraints_pref;
          PlNode *m =  til_constraints_pref;

          WffNode *i;
          char s[30];
          char s2[40];
          char s3[40];
                 
          strcpy(s3,n2->rightson->atom->item );  
          if(m->connective==AND){
                  sprintf(s2,"IS-SATISFIED-%s",m1->sons->new_name);
                   if(same_pref_name(s2, s3) == 0){
                      found = 2;
		   }
		  if(same_pref_name(s2, s3) == 1){
                       found = 3;
                      var = TRUE;
		      sprintf(s,"IS-SATISFIED-%s",m1->sons->new_name);
                      n2->rightson->atom = new_TokenList();
                      n2->rightson->atom->item = new_Token(30);
                      strcpy(n2->rightson->atom->item, s);
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-SATISFIED-%s",i->new_name);
                    if( same_pref_name(s2, s3) == 0){
                        found = 2;
		     }
		    if( same_pref_name(s2, s3) == 1){
                      found = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-SATISFIED-%s",i->new_name);
                         n2->rightson->atom = new_TokenList();
                         n2->rightson->atom->item = new_Token(30);
                         strcpy(n2->rightson->atom->item, s);
		      }
		     else{      
                 
                      ParseExpNode *node = new_ParseExpNode(AD);
                      if(nn2->connective == MU)
                         node->rightson = new_ParseExpNode(MU);
                      if(nn2->connective == DI)
                         node->rightson = new_ParseExpNode(DI);
                      node->rightson->leftson = new_ParseExpNode(nn2->leftson->connective);
                      node->rightson->leftson->atom = copy_TokenList(nn2->leftson->atom);
                      sprintf(s,"IS-SATISFIED-%s",i->new_name);
                      node->rightson->rightson = new_ParseExpNode(nn2->rightson->connective);
                     
                      node->rightson->rightson->atom = new_TokenList();
                      node->rightson->rightson->atom->item = (char *) calloc(30, sizeof(char));
                      strcpy(node->rightson->rightson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;                    
		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-SATISFIED-%s",m1->new_name);
                  if(same_pref_name(s2, n2->rightson->atom->item) == 0){
                      found = 2;
		  }
		  if(same_pref_name(s2, n2->rightson->atom->item) == 1){
                       found = 3;
		      sprintf(s,"IS-SATISFIED-%s",m1->new_name);
                      strcpy(n2->rightson->atom->item, s);
		   
		  }
            }

	  if(found == 2 ||  found == 3)
	       goto spring;

	}
        /*end til constraints preference*/         

      }

    }



    /* for IS-VIOLATED
     * leftson
     **/
    if(n2->leftson->connective == FHEAD){
      
     /* IS-VIOLATED
     **/
     if(strncmp(str1,n2->leftson->atom->item, 10) == 0){
      found = 1;
      /* 1.preferences goal
      **/
        if(gpref_head){
          WffNode *m1 = gpref_head;
          PlNode *m = pref_head;

          WffNode *i;
          char s[30];
          char s2[40];
          char s3[40];
                 
           strcpy(s3,n2->leftson->atom->item );
          if(m->connective==AND){
                  sprintf(s2,"IS-VIOLATED-%s",m1->sons->name);
                  if(same_pref_name(s2, s3) == 0){
                      found = 2;
		  }
		  if(same_pref_name(s2, s3) == 1){
                       found = 3;
                      var = TRUE;
		      sprintf(s,"IS-VIOLATED-%s",m1->sons->new_name);
                      n2->leftson->atom = new_TokenList();
                      n2->leftson->atom->item = new_Token(30);
                      strcpy(n2->leftson->atom->item, s);
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-VIOLATED-%s",i->name);
                     if( same_pref_name(s2, s3) == 0){
                          found = 2;
		     }
		    if( same_pref_name(s2, s3) == 1){
                      found = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-VIOLATED-%s",i->new_name);
                         n2->leftson->atom = new_TokenList();
                         n2->leftson->atom->item = new_Token(30);
                         strcpy(n2->leftson->atom->item, s);
		      }
		     else{      
                 
                      ParseExpNode *node = new_ParseExpNode(AD);
                      if(nn2->connective == MU)
                         node->rightson = new_ParseExpNode(MU);
                      if(nn2->connective == DI)
                         node->rightson = new_ParseExpNode(DI);
                      node->rightson->rightson = new_ParseExpNode(nn2->rightson->connective);
                      node->rightson->rightson->atom = copy_TokenList(nn2->rightson->atom);
                      sprintf(s,"IS-VIOLATED-%s",i->new_name);
                      node->rightson->leftson = new_ParseExpNode(nn2->leftson->connective);
                     
                      node->rightson->leftson->atom = new_TokenList();
                      node->rightson->leftson->atom->item = (char *) calloc(30, sizeof(char));
                      strcpy(node->rightson->leftson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;                    
		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-VIOLATED-%s",m1->name);
                   if(same_pref_name(s2, n2->leftson->atom->item) == 0){
                      found = 2;
		   }
		  if(same_pref_name(s2, n2->leftson->atom->item) == 1){
                       found = 3;
		      sprintf(s,"IS-VIOLATED-%s",m1->new_name);
                      strcpy(n2->leftson->atom->item, s);
		   
		  }
            }

	  if(found == 2 ||  found == 3)
               goto spring;

	}  
	/* end preference goal*/
        /* 2.normal constraints preference
	**/
        if(gconstraints_pref){
          WffNode *m1 = gconstraints_pref;
          PlNode *m =  pref_constraints_head;

          WffNode *i;
          char s[30];
          char s2[40];
          char s3[40];
                 
          strcpy(s3,n2->leftson->atom->item );  
          if(m->connective==AND){
                sprintf(s2,"IS-VIOLATED-%s",m1->sons->new_name);
                  if(same_pref_name(s2, s3) == 0){
                      found = 2;
		  }
		  if(same_pref_name(s2, s3) == 1){
                       found = 3;
                      var = TRUE;
		      sprintf(s,"IS-VIOLATED-%s",m1->sons->new_name);
                      n2->leftson->atom = new_TokenList();
                      n2->leftson->atom->item = new_Token(30);
                      strcpy(n2->leftson->atom->item, s);
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-VIOLATED-%s",i->new_name);
                    if( same_pref_name(s2, s3) == 0){
                        found = 2;
		    }
		    if( same_pref_name(s2, s3) == 1){
                      found = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-VIOLATED-%s",i->new_name);
                         n2->leftson->atom = new_TokenList();
                         n2->leftson->atom->item = new_Token(30);
                         strcpy(n2->leftson->atom->item, s);
		      }
		     else{      
                 
                      ParseExpNode *node = new_ParseExpNode(AD);
                      if(nn2->connective == MU)
                         node->rightson = new_ParseExpNode(MU);
                      if(nn2->connective == DI)
                         node->rightson = new_ParseExpNode(DI);
                      node->rightson->rightson = new_ParseExpNode(nn2->rightson->connective);
                      node->rightson->rightson->atom = copy_TokenList(nn2->rightson->atom);
                      sprintf(s,"IS-VIOLATED-%s",i->new_name);
                      node->rightson->leftson = new_ParseExpNode(nn2->leftson->connective);
                     
                      node->rightson->leftson->atom = new_TokenList();
                      node->rightson->leftson->atom->item = (char *) calloc(30, sizeof(char));
                      strcpy(node->rightson->leftson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;                    
		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-VIOLATED-%s",m1->new_name);
                   if(same_pref_name(s2, n2->leftson->atom->item) == 0){
                        found = 2;
		   }
		  if(same_pref_name(s2, n2->leftson->atom->item) == 1){
                       found = 3;
		      sprintf(s,"IS-VIOLATED-%s",m1->new_name);
                      strcpy(n2->leftson->atom->item, s);
		   
		  }
            }

	  if(found == 2 ||  found == 3)
	       goto spring;

	}

        /*end normal constraints preference*/
        /* 3.within constraints preference
	**/
	if(gwithin_constraints_pref){

          WffNode *m1 = gwithin_constraints_pref;
          PlNode *m =  within_constraints_pref;

          WffNode *i;
          char s[30];
          char s2[40];
          char s3[40];
                 
          strcpy(s3,n2->leftson->atom->item );  
          if(m->connective==AND){
                  sprintf(s2,"IS-VIOLATED-%s",m1->sons->new_name);
		  if(same_pref_name(s2, s3) == 0){
                      found = 2;
		  }
                   if(same_pref_name(s2, s3) == 1){
                       found = 3;
                      var = TRUE;
		      sprintf(s,"IS-VIOLATED-%s",m1->sons->new_name);
                      n2->leftson->atom = new_TokenList();
                      n2->leftson->atom->item = new_Token(30);
                      strcpy(n2->leftson->atom->item, s);
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-VIOLATED-%s",i->new_name);
                    if( same_pref_name(s2, s3) == 0){
                          found = 2;
		    }
		    if( same_pref_name(s2, s3) == 1){
                      found = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-VIOLATED-%s",i->new_name);
                         n2->leftson->atom = new_TokenList();
                         n2->leftson->atom->item = new_Token(30);
                         strcpy(n2->leftson->atom->item, s);
		      }
		     else{      
                 
                      ParseExpNode *node = new_ParseExpNode(AD);
                      if(nn2->connective == MU)
                         node->rightson = new_ParseExpNode(MU);
                      if(nn2->connective == DI)
                         node->rightson = new_ParseExpNode(DI);
                      node->rightson->rightson = new_ParseExpNode(nn2->rightson->connective);
                      node->rightson->rightson->atom = copy_TokenList(nn2->rightson->atom);
                      sprintf(s,"IS-VIOLATED-%s",i->new_name);
                      node->rightson->leftson = new_ParseExpNode(nn2->leftson->connective);
                     
                      node->rightson->leftson->atom = new_TokenList();
                      node->rightson->leftson->atom->item = (char *) calloc(30, sizeof(char));
                      strcpy(node->rightson->leftson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;                    
		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-VIOLATED-%s",m1->new_name);
                  if(same_pref_name(s2, n2->leftson->atom->item) == 0){
                      found = 2;
		  }
		  if(same_pref_name(s2, n2->leftson->atom->item) == 1){
                       found = 3;
		      sprintf(s,"IS-VIOLATED-%s",m1->new_name);
                      strcpy(n2->leftson->atom->item, s);
		   
		  }
            }

	  if(found == 2 ||  found == 3)
	       goto spring;

	}
        /*end within constraints preference*/         
        /* 4.til constraints preference
	**/
	if(gtil_constraints_pref){
          WffNode *m1 = gtil_constraints_pref;
          PlNode *m =  til_constraints_pref;

          WffNode *i;
          char s[30];
          char s2[40];
          char s3[40];
                 
          strcpy(s3,n2->leftson->atom->item );  
          if(m->connective==AND){
                  sprintf(s2,"IS-VIOLATED-%s",m1->sons->new_name);
                  if(same_pref_name(s2, s3) == 0){
                      found = 2;
		  }
		  if(same_pref_name(s2, s3) == 1){
                       found = 3;
                      var = TRUE;
		      sprintf(s,"IS-VIOLATED-%s",m1->sons->new_name);
                      n2->leftson->atom = new_TokenList();
                      n2->leftson->atom->item = new_Token(30);
                      strcpy(n2->leftson->atom->item, s);
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-VIOLATED-%s",i->new_name);
                    if( same_pref_name(s2, s3) == 0){
                        found = 2;
		     }
		    if( same_pref_name(s2, s3) == 1){
                      found = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-VIOLATED-%s",i->new_name);
                         n2->leftson->atom = new_TokenList();
                         n2->leftson->atom->item = new_Token(30);
                         strcpy(n2->leftson->atom->item, s);
		      }
		     else{      
                 
                      ParseExpNode *node = new_ParseExpNode(AD);
                      if(nn2->connective == MU)
                         node->rightson = new_ParseExpNode(MU);
                      if(nn2->connective == DI)
                         node->rightson = new_ParseExpNode(DI);
                      node->rightson->rightson = new_ParseExpNode(nn2->rightson->connective);
                      node->rightson->rightson->atom = copy_TokenList(nn2->rightson->atom);
                      sprintf(s,"IS-VIOLATED-%s",i->new_name);
                      node->rightson->leftson = new_ParseExpNode(nn2->leftson->connective);
                     
                      node->rightson->leftson->atom = new_TokenList();
                      node->rightson->leftson->atom->item = (char *) calloc(30, sizeof(char));
                      strcpy(node->rightson->leftson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;                    
		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-VIOLATED-%s",m1->new_name);
                  if(same_pref_name(s2, n2->leftson->atom->item) == 0){
                        found = 2;
		   }
		  if(same_pref_name(s2, n2->leftson->atom->item) == 1){
                       found = 3;
		      sprintf(s,"IS-VIOLATED-%s",m1->new_name);
                      strcpy(n2->leftson->atom->item, s);
		   
		  }
            }

	  if(found == 2 ||  found == 3)
	       goto spring;

	}
        /*end til constraints preference*/         

     }

    /* IS-SATISFIED
    **/
     if(strncmp(str2,n2->leftson->atom->item, 11) == 0){

      found = 1;
      
      /* 1.preferences goal
      **/
        if(gpref_head){
          WffNode *m1 = gpref_head;
          PlNode *m = pref_head;

          WffNode *i;
          char s[30];
          char s2[40];
          char s3[40];
                 
           strcpy(s3,n2->leftson->atom->item );
          if(m->connective==AND){
                  sprintf(s2,"IS-SATISFIED-%s",m1->sons->name);
                  if(same_pref_name(s2, s3) == 0){
                      found = 2;
		  }
		  if(same_pref_name(s2, s3) == 1){
                       found = 3;
                      var = TRUE;
		      sprintf(s,"IS-SATISFIED-%s",m1->sons->new_name);
                      n2->leftson->atom = new_TokenList();
                      n2->leftson->atom->item = new_Token(30);
                      strcpy(n2->leftson->atom->item, s);
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-SATISFIED-%s",i->name);
                     if( same_pref_name(s2, s3) == 0){
                          found = 2;
		     }
		    if( same_pref_name(s2, s3) == 1){
                      found = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-SATISFIED-%s",i->new_name);
                         n2->leftson->atom = new_TokenList();
                         n2->leftson->atom->item = new_Token(30);
                         strcpy(n2->leftson->atom->item, s);
		      }
		     else{      
                 
                      ParseExpNode *node = new_ParseExpNode(AD);
                      if(nn2->connective == MU)
                         node->rightson = new_ParseExpNode(MU);
                      if(nn2->connective == DI)
                         node->rightson = new_ParseExpNode(DI);
                      node->rightson->rightson = new_ParseExpNode(nn2->rightson->connective);
                      node->rightson->rightson->atom = copy_TokenList(nn2->rightson->atom);
                      sprintf(s,"IS-SATISFIED-%s",i->new_name);
                      node->rightson->leftson = new_ParseExpNode(nn2->leftson->connective);
                     
                      node->rightson->leftson->atom = new_TokenList();
                      node->rightson->leftson->atom->item = (char *) calloc(30, sizeof(char));
                      strcpy(node->rightson->leftson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;                    
		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-SATISFIED-%s",m1->name);
                   if(same_pref_name(s2, n2->leftson->atom->item) == 0){
                      found = 2;
		   }
		  if(same_pref_name(s2, n2->leftson->atom->item) == 1){
                       found = 3;
		      sprintf(s,"IS-SATISFIED-%s",m1->new_name);
                      strcpy(n2->leftson->atom->item, s);
		   
		  }
            }

	  if(found == 2 ||  found == 3)
               goto spring;

	}  
	/* end preference goal*/
        /* 2.normal constraints preference
	**/
        if(gconstraints_pref){
          WffNode *m1 = gconstraints_pref;
          PlNode *m =  pref_constraints_head;

          WffNode *i;
          char s[30];
          char s2[40];
          char s3[40];
                 
          strcpy(s3,n2->leftson->atom->item );  
          if(m->connective==AND){
                sprintf(s2,"IS-SATISFIED-%s",m1->sons->new_name);
                  if(same_pref_name(s2, s3) == 0){
                      found = 2;
		  }
		  if(same_pref_name(s2, s3) == 1){
                       found = 3;
                      var = TRUE;
		      sprintf(s,"IS-SATISFIED-%s",m1->sons->new_name);
                      n2->leftson->atom = new_TokenList();
                      n2->leftson->atom->item = new_Token(30);
                      strcpy(n2->leftson->atom->item, s);
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-SATISFIED-%s",i->new_name);
                    if( same_pref_name(s2, s3) == 0){
                        found = 2;
		    }
		    if( same_pref_name(s2, s3) == 1){
                      found = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-SATISFIED-%s",i->new_name);
                         n2->leftson->atom = new_TokenList();
                         n2->leftson->atom->item = new_Token(30);
                         strcpy(n2->leftson->atom->item, s);
		      }
		     else{      
                 
                      ParseExpNode *node = new_ParseExpNode(AD);
                      if(nn2->connective == MU)
                         node->rightson = new_ParseExpNode(MU);
                      if(nn2->connective == DI)
                         node->rightson = new_ParseExpNode(DI);
                      node->rightson->rightson = new_ParseExpNode(nn2->rightson->connective);
                      node->rightson->rightson->atom = copy_TokenList(nn2->rightson->atom);
                      sprintf(s,"IS-SATISFIED-%s",i->new_name);
                      node->rightson->leftson = new_ParseExpNode(nn2->leftson->connective);
                     
                      node->rightson->leftson->atom = new_TokenList();
                      node->rightson->leftson->atom->item = (char *) calloc(30, sizeof(char));
                      strcpy(node->rightson->leftson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;                    
		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-SATISFIED-%s",m1->new_name);
                   if(same_pref_name(s2, n2->leftson->atom->item) == 0){
                        found = 2;
		   }
		  if(same_pref_name(s2, n2->leftson->atom->item) == 1){
                       found = 3;
		      sprintf(s,"IS-SATISFIED-%s",m1->new_name);
                      strcpy(n2->leftson->atom->item, s);
		   
		  }
            }

	  if(found == 2 ||  found == 3)
	       goto spring;

	}

        /*end normal constraints preference*/
        /* 3.within constraints preference
	**/
	if(gwithin_constraints_pref){

          WffNode *m1 = gwithin_constraints_pref;
          PlNode *m =  within_constraints_pref;

          WffNode *i;
          char s[30];
          char s2[40];
          char s3[40];
                 
          strcpy(s3,n2->leftson->atom->item );  
          if(m->connective==AND){
                  sprintf(s2,"IS-SATISFIED-%s",m1->sons->new_name);
		  if(same_pref_name(s2, s3) == 0){
                      found = 2;
		  }
                   if(same_pref_name(s2, s3) == 1){
                       found = 3;
                      var = TRUE;
		      sprintf(s,"IS-SATISFIED-%s",m1->sons->new_name);
                      n2->leftson->atom = new_TokenList();
                      n2->leftson->atom->item = new_Token(30);
                      strcpy(n2->leftson->atom->item, s);
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-SATISFIED-%s",i->new_name);
                    if( same_pref_name(s2, s3) == 0){
                          found = 2;
		    }
		    if( same_pref_name(s2, s3) == 1){
                      found = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-SATISFIED-%s",i->new_name);
                         n2->leftson->atom = new_TokenList();
                         n2->leftson->atom->item = new_Token(30);
                         strcpy(n2->leftson->atom->item, s);
		      }
		     else{      
                 
                      ParseExpNode *node = new_ParseExpNode(AD);
                      if(nn2->connective == MU)
                         node->rightson = new_ParseExpNode(MU);
                      if(nn2->connective == DI)
                         node->rightson = new_ParseExpNode(DI);
                      node->rightson->rightson = new_ParseExpNode(nn2->rightson->connective);
                      node->rightson->rightson->atom = copy_TokenList(nn2->rightson->atom);
                      sprintf(s,"IS-SATISFIED-%s",i->new_name);
                      node->rightson->leftson = new_ParseExpNode(nn2->leftson->connective);
                     
                      node->rightson->leftson->atom = new_TokenList();
                      node->rightson->leftson->atom->item = (char *) calloc(30, sizeof(char));
                      strcpy(node->rightson->leftson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;                    
		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-SATISFIED-%s",m1->new_name);
                  if(same_pref_name(s2, n2->leftson->atom->item) == 0){
                      found = 2;
		  }
		  if(same_pref_name(s2, n2->leftson->atom->item) == 1){
                       found = 3;
		      sprintf(s,"IS-SATISFIED-%s",m1->new_name);
                      strcpy(n2->leftson->atom->item, s);
		   
		  }
            }

	  if(found == 2 ||  found == 3)
	       goto spring;

	}
        /*end within constraints preference*/         
        /* 4.til constraints preference
	**/
	if(gtil_constraints_pref){
          WffNode *m1 = gtil_constraints_pref;
          PlNode *m =  til_constraints_pref;

          WffNode *i;
          char s[30];
          char s2[40];
          char s3[40];
                 
          strcpy(s3,n2->leftson->atom->item );  
          if(m->connective==AND){
                  sprintf(s2,"IS-SATISFIED-%s",m1->sons->new_name);
                  if(same_pref_name(s2, s3) == 0){
                      found = 2;
		  }
		  if(same_pref_name(s2, s3) == 1){
                       found = 3;
                      var = TRUE;
		      sprintf(s,"IS-SATISFIED-%s",m1->sons->new_name);
                      n2->leftson->atom = new_TokenList();
                      n2->leftson->atom->item = new_Token(30);
                      strcpy(n2->leftson->atom->item, s);
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-SATISFIED-%s",i->new_name);
                    if( same_pref_name(s2, s3) == 0){
                        found = 2;
		     }
		    if( same_pref_name(s2, s3) == 1){
                      found = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-SATISFIED-%s",i->new_name);
                         n2->leftson->atom = new_TokenList();
                         n2->leftson->atom->item = new_Token(30);
                         strcpy(n2->leftson->atom->item, s);
		      }
		     else{      
                 
                      ParseExpNode *node = new_ParseExpNode(AD);
                      if(nn2->connective == MU)
                         node->rightson = new_ParseExpNode(MU);
                      if(nn2->connective == DI)
                         node->rightson = new_ParseExpNode(DI);
                      node->rightson->rightson = new_ParseExpNode(nn2->rightson->connective);
                      node->rightson->rightson->atom = copy_TokenList(nn2->rightson->atom);
                      sprintf(s,"IS-SATISFIED-%s",i->new_name);
                      node->rightson->leftson = new_ParseExpNode(nn2->leftson->connective);
                     
                      node->rightson->leftson->atom = new_TokenList();
                      node->rightson->leftson->atom->item = (char *) calloc(30, sizeof(char));
                      strcpy(node->rightson->leftson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;                    
		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-SATISFIED-%s",m1->new_name);
                  if(same_pref_name(s2, n2->leftson->atom->item) == 0){
                        found = 2;
		   }
		  if(same_pref_name(s2, n2->leftson->atom->item) == 1){
                       found = 3;
		      sprintf(s,"IS-SATISFIED-%s",m1->new_name);
                      strcpy(n2->leftson->atom->item, s);
		   
		  }
            }

	  if(found == 2 ||  found == 3)
	       goto spring;

	}
        /*end til constraints preference*/         

     }

    }

    /*+++++++++++++++++++++++++++++++*/ 
  }
  else{
      if(n2->connective == FHEAD){
     
      var = FALSE; 

    /* for IS-VIOLATED
    **/
    if(strncmp(str1,n2->atom->item, 10) == 0){
        found = 1;
        /* 1.preferences goal
        **/  
        if(gpref_head){
          WffNode *m1 = gpref_head;
          PlNode *m = pref_head;
          WffNode *i;
          char s[40];
          char s2[40];
          char s3[40];

          strcpy(s3, n2->atom->item);
                   
          if(m->connective==AND){
                  sprintf(s2,"IS-VIOLATED-%s",m1->sons->name);
                  if(same_pref_name(s2, s3) == 0){
                       found = 2;
		   }
		  if(same_pref_name(s2, s3) == 1){
                       found = 3;
                      var = TRUE;
		      sprintf(s,"IS-VIOLATED-%s",m1->sons->new_name);
                      n2->atom = new_TokenList();
                      n2->atom->item = new_Token(30);
                      strcpy(n2->atom->item, s);
		   
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-VIOLATED-%s",i->name);
                     if( same_pref_name(s2, s3) == 0){
                            found = 2;
		     }
		    if( same_pref_name(s2, s3) == 1){
                      found = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-VIOLATED-%s",i->new_name);
                         n2->atom = new_TokenList();
                         n2->atom->item = new_Token(30);
                         strcpy(n2->atom->item, s);
		      }
		     else{      
                      ParseExpNode *node = new_ParseExpNode(AD);
                      sprintf(s,"IS-VIOLATED-%s",i->new_name);
                      node->rightson = new_ParseExpNode(nn2->connective);
                      node->rightson->atom = copy_TokenList(nn2->atom);
                      strcpy(node->rightson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;

		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-VIOLATED-%s",m1->name);
                   if(same_pref_name(s2, n2->atom->item) == 0){
                        found = 2;
		   }
		  if(same_pref_name(s2, n2->atom->item) == 1){
                       found = 3;
		      sprintf(s,"IS-VIOLATED-%s",m1->new_name);
                      strcpy(n2->atom->item, s);		   
		  }
            }

	  if(found == 2 ||  found == 3)
	       goto spring;

	}     
       /* end preference goal*/
        /* 2.normal constraints preference
	**/
        if(gconstraints_pref) {
           WffNode *m1 = gconstraints_pref;
          PlNode *m =  pref_constraints_head;
          WffNode *i;
          char s[40];
          char s2[40];
          char s3[40];

          strcpy(s3, n2->atom->item);
                   
          if(m->connective==AND){
                  sprintf(s2,"IS-VIOLATED-%s",m1->sons->new_name);
                  if(same_pref_name(s2, s3) == 0){
                      found = 2;
		   }
		  if(same_pref_name(s2, s3) == 1){
                       found = 3;
                      var = TRUE;
		      sprintf(s,"IS-VIOLATED-%s",m1->sons->new_name);
                      n2->atom = new_TokenList();
                      n2->atom->item = new_Token(30);
                      strcpy(n2->atom->item, s);
		   
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-VIOLATED-%s",i->new_name);
                    if( same_pref_name(s2, s3) == 1){
                          found = 2;
		     }
		    if( same_pref_name(s2, s3) == 1){
                      found = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-VIOLATED-%s",i->new_name);
                         n2->atom = new_TokenList();
                         n2->atom->item = new_Token(30);
                         strcpy(n2->atom->item, s);
		      }
		     else{      
                      ParseExpNode *node = new_ParseExpNode(AD);
                      sprintf(s,"IS-VIOLATED-%s",i->new_name);
                      node->rightson = new_ParseExpNode(nn2->connective);
                      node->rightson->atom = copy_TokenList(nn2->atom);
                      strcpy(node->rightson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;

		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-VIOLATED-%s",m1->new_name);
                   if(same_pref_name(s2, n2->atom->item) == 0){
                        found = 2;
		   }
		  if(same_pref_name(s2, n2->atom->item) == 1){
                       found = 3;
		      sprintf(s,"IS-VIOLATED-%s",m1->new_name);
                      strcpy(n2->atom->item, s);		   
		  }
            }

	  if(found == 2 ||  found == 3)
	       goto spring;

	}
       /*end normal constraints preference*/
        /* 3.within constraints preference
	**/
	if(gwithin_constraints_pref) {
           WffNode *m1 = gwithin_constraints_pref;
           PlNode *m =  within_constraints_pref;
          WffNode *i;
          char s[40];
          char s2[40];
          char s3[40];

          strcpy(s3, n2->atom->item);
                   
          if(m->connective==AND){
                  sprintf(s2,"IS-VIOLATED-%s",m1->sons->new_name);
                   if(same_pref_name(s2, s3) == 0){
                        found = 2;
		   }
		  if(same_pref_name(s2, s3) == 1){
                       found = 3;
                      var = TRUE;
		      sprintf(s,"IS-VIOLATED-%s",m1->sons->new_name);
                      n2->atom = new_TokenList();
                      n2->atom->item = new_Token(30);
                      strcpy(n2->atom->item, s);
		   
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-VIOLATED-%s",i->new_name);
                     if( same_pref_name(s2, s3) == 0){
                          found = 2;
		     }
		    if( same_pref_name(s2, s3) == 1){
                      found = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-VIOLATED-%s",i->new_name);
                         n2->atom = new_TokenList();
                         n2->atom->item = new_Token(30);
                         strcpy(n2->atom->item, s);
		      }
		     else{      
                      ParseExpNode *node = new_ParseExpNode(AD);
                      sprintf(s,"IS-VIOLATED-%s",i->new_name);
                      node->rightson = new_ParseExpNode(nn2->connective);
                      node->rightson->atom = copy_TokenList(nn2->atom);
                      strcpy(node->rightson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;

		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-VIOLATED-%s",m1->name);
                  if(same_pref_name(s2, n2->atom->item) == 0){
                         found = 2;
		    }
		  if(same_pref_name(s2, n2->atom->item) == 1){
                       found = 3;
		      sprintf(s,"IS-VIOLATED-%s",m1->new_name);
                      strcpy(n2->atom->item, s);		   
		  }
            }

	  if(found == 2 ||  found == 3)
	       goto spring;

	}
        /*end within constraints preference*/       
        /* 4.til constraints preference
	**/
	if(gtil_constraints_pref) {
           WffNode *m1 = gtil_constraints_pref;
           PlNode *m =  til_constraints_pref;
          WffNode *i;
          char s[40];
          char s2[40];
          char s3[40];

          strcpy(s3, n2->atom->item);
                   
          if(m->connective==AND){
                  sprintf(s2,"IS-VIOLATED-%s",m1->sons->new_name);
                   if(same_pref_name(s2, s3) == 0){
                        found = 2;
		   }
		  if(same_pref_name(s2, s3) == 1){
                       found = 3;
                      var = TRUE;
		      sprintf(s,"IS-VIOLATED-%s",m1->sons->new_name);
                      n2->atom = new_TokenList();
                      n2->atom->item = new_Token(30);
                      strcpy(n2->atom->item, s);
		   
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-VIOLATED-%s",i->new_name);
                    if( same_pref_name(s2, s3) == 0){
                         found = 2;
		      }
		    if( same_pref_name(s2, s3) == 1){
                      found = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-VIOLATED-%s",i->new_name);
                         n2->atom = new_TokenList();
                         n2->atom->item = new_Token(30);
                         strcpy(n2->atom->item, s);
		      }
		     else{      
                      ParseExpNode *node = new_ParseExpNode(AD);
                      sprintf(s,"IS-VIOLATED-%s",i->new_name);
                      node->rightson = new_ParseExpNode(nn2->connective);
                      node->rightson->atom = copy_TokenList(nn2->atom);
                      strcpy(node->rightson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;

		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-VIOLATED-%s",m1->name);
                  if(same_pref_name(s2, n2->atom->item) == 0){
                        found = 2;
		    }
		  if(same_pref_name(s2, n2->atom->item) == 1){
                       found = 3;
		      sprintf(s,"IS-VIOLATED-%s",m1->new_name);
                      strcpy(n2->atom->item, s);		   
		  }
            }

	  if(found == 2 ||  found == 3)
	       goto spring;
	}
        /*end til constraints preference*/       

    }

    /* for IS-SATISFIED
    **/
    if(strncmp(str2,n2->atom->item, 11) == 0){
        found = 1;
        /* 1.preferences goal
        **/  
        if(gpref_head){
          WffNode *m1 = gpref_head;
          PlNode *m = pref_head;
          WffNode *i;
          char s[40];
          char s2[40];
          char s3[40];

          strcpy(s3, n2->atom->item);
                   
          if(m->connective==AND){
                  sprintf(s2,"IS-SATISFIED-%s",m1->sons->name);
                  if(same_pref_name(s2, s3) == 0){
                       found = 2;
		   }
		  if(same_pref_name(s2, s3) == 1){
                       found = 3;
                      var = TRUE;
		      sprintf(s,"IS-SATISFIED-%s",m1->sons->new_name);
                      n2->atom = new_TokenList();
                      n2->atom->item = new_Token(30);
                      strcpy(n2->atom->item, s);
		   
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-SATISFIED-%s",i->name);
                     if( same_pref_name(s2, s3) == 0){
                            found = 2;
		     }
		    if( same_pref_name(s2, s3) == 1){
                      found = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-SATISFIED-%s",i->new_name);
                         n2->atom = new_TokenList();
                         n2->atom->item = new_Token(30);
                         strcpy(n2->atom->item, s);
		      }
		     else{      
                      ParseExpNode *node = new_ParseExpNode(AD);
                      sprintf(s,"IS-SATISFIED-%s",i->new_name);
                      node->rightson = new_ParseExpNode(nn2->connective);
                      node->rightson->atom = copy_TokenList(nn2->atom);
                      strcpy(node->rightson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;

		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-SATISFIED-%s",m1->name);
                   if(same_pref_name(s2, n2->atom->item) == 0){
                        found = 2;
		   }
		  if(same_pref_name(s2, n2->atom->item) == 1){
                       found = 3;
		      sprintf(s,"IS-SATISFIED-%s",m1->new_name);
                      strcpy(n2->atom->item, s);		   
		  }
            }

	  if(found == 2 ||  found == 3)
	       goto spring;

	}     
       /* end preference goal*/
        /* 2.normal constraints preference
	**/
        if(gconstraints_pref) {
           WffNode *m1 = gconstraints_pref;
          PlNode *m =  pref_constraints_head;
          WffNode *i;
          char s[40];
          char s2[40];
          char s3[40];

          strcpy(s3, n2->atom->item);
                   
          if(m->connective==AND){
                  sprintf(s2,"IS-SATISFIED-%s",m1->sons->new_name);
                  if(same_pref_name(s2, s3) == 0){
                      found = 2;
		   }
		  if(same_pref_name(s2, s3) == 1){
                       found = 3;
                      var = TRUE;
		      sprintf(s,"IS-SATISFIED-%s",m1->sons->new_name);
                      n2->atom = new_TokenList();
                      n2->atom->item = new_Token(30);
                      strcpy(n2->atom->item, s);
		   
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-SATISFIED-%s",i->new_name);
                    if( same_pref_name(s2, s3) == 1){
                          found = 2;
		     }
		    if( same_pref_name(s2, s3) == 1){
                      found = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-SATISFIED-%s",i->new_name);
                         n2->atom = new_TokenList();
                         n2->atom->item = new_Token(30);
                         strcpy(n2->atom->item, s);
		      }
		     else{      
                      ParseExpNode *node = new_ParseExpNode(AD);
                      sprintf(s,"IS-SATISFIED-%s",i->new_name);
                      node->rightson = new_ParseExpNode(nn2->connective);
                      node->rightson->atom = copy_TokenList(nn2->atom);
                      strcpy(node->rightson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;

		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-SATISFIED-%s",m1->new_name);
                   if(same_pref_name(s2, n2->atom->item) == 0){
                        found = 2;
		   }
		  if(same_pref_name(s2, n2->atom->item) == 1){
                       found = 3;
		      sprintf(s,"IS-SATISFIED-%s",m1->new_name);
                      strcpy(n2->atom->item, s);		   
		  }
            }

	  if(found == 2 ||  found == 3)
	       goto spring;

	}
       /*end normal constraints preference*/
        /* 3.within constraints preference
	**/
	if(gwithin_constraints_pref) {
           WffNode *m1 = gwithin_constraints_pref;
           PlNode *m =  within_constraints_pref;
          WffNode *i;
          char s[40];
          char s2[40];
          char s3[40];

          strcpy(s3, n2->atom->item);
                   
          if(m->connective==AND){
                  sprintf(s2,"IS-SATISFIED-%s",m1->sons->new_name);
                   if(same_pref_name(s2, s3) == 0){
                        found = 2;
		   }
		  if(same_pref_name(s2, s3) == 1){
                       found = 3;
                      var = TRUE;
		      sprintf(s,"IS-SATISFIED-%s",m1->sons->new_name);
                      n2->atom = new_TokenList();
                      n2->atom->item = new_Token(30);
                      strcpy(n2->atom->item, s);
		   
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-SATISFIED-%s",i->new_name);
                     if( same_pref_name(s2, s3) == 0){
                          found = 2;
		     }
		    if( same_pref_name(s2, s3) == 1){
                      found = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-SATISFIED-%s",i->new_name);
                         n2->atom = new_TokenList();
                         n2->atom->item = new_Token(30);
                         strcpy(n2->atom->item, s);
		      }
		     else{      
                      ParseExpNode *node = new_ParseExpNode(AD);
                      sprintf(s,"IS-SATISFIED-%s",i->new_name);
                      node->rightson = new_ParseExpNode(nn2->connective);
                      node->rightson->atom = copy_TokenList(nn2->atom);
                      strcpy(node->rightson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;

		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-SATISFIED-%s",m1->name);
                  if(same_pref_name(s2, n2->atom->item) == 0){
                         found = 2;
		    }
		  if(same_pref_name(s2, n2->atom->item) == 1){
                       found = 3;
		      sprintf(s,"IS-SATISFIED-%s",m1->new_name);
                      strcpy(n2->atom->item, s);		   
		  }
            }

	  if(found == 2 ||  found == 3)
	       goto spring;

	}
        /*end within constraints preference*/       
        /* 4.til constraints preference
	**/
	if(gtil_constraints_pref) {
           WffNode *m1 = gtil_constraints_pref;
           PlNode *m =  til_constraints_pref;
          WffNode *i;
          char s[40];
          char s2[40];
          char s3[40];

          strcpy(s3, n2->atom->item);
                   
          if(m->connective==AND){
                  sprintf(s2,"IS-SATISFIED-%s",m1->sons->new_name);
                   if(same_pref_name(s2, s3) == 0){
                        found = 2;
		   }
		  if(same_pref_name(s2, s3) == 1){
                       found = 3;
                      var = TRUE;
		      sprintf(s,"IS-SATISFIED-%s",m1->sons->new_name);
                      n2->atom = new_TokenList();
                      n2->atom->item = new_Token(30);
                      strcpy(n2->atom->item, s);
		   
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-SATISFIED-%s",i->new_name);
                    if( same_pref_name(s2, s3) == 0){
                         found = 2;
		      }
		    if( same_pref_name(s2, s3) == 1){
                      found = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-SATISFIED-%s",i->new_name);
                         n2->atom = new_TokenList();
                         n2->atom->item = new_Token(30);
                         strcpy(n2->atom->item, s);
		      }
		     else{      
                      ParseExpNode *node = new_ParseExpNode(AD);
                      sprintf(s,"IS-SATISFIED-%s",i->new_name);
                      node->rightson = new_ParseExpNode(nn2->connective);
                      node->rightson->atom = copy_TokenList(nn2->atom);
                      strcpy(node->rightson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;

		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-SATISFIED-%s",m1->name);
                  if(same_pref_name(s2, n2->atom->item) == 0){
                        found = 2;
		    }
		  if(same_pref_name(s2, n2->atom->item) == 1){
                       found = 3;
		      sprintf(s,"IS-SATISFIED-%s",m1->new_name);
                      strcpy(n2->atom->item, s);		   
		  }
            }

	  if(found == 2 ||  found == 3)
	       goto spring;
	}
        /*end til constraints preference*/       

    }

  }
 }
 

    /* Is_VIOLATED not found
    **/
    if(found == 1){

	node2->found = 1;
     }

    /* Mult or div
    **/
    spring:  if((n1->connective == MU) || (n1->connective == DI)){

     var = FALSE; 

    /* for IS-VIOLATED
     * rightson
    **/
    if(n1->rightson->connective == FHEAD){

     /* IS-VIOLATED
     **/
    if(strncmp(str1, n1->rightson->atom->item, 10) == 0){
        found2 = 1;
        /* 1.preferences goal
        **/ 
        if(gpref_head){
          WffNode *m1 = gpref_head;
          PlNode *m = pref_head;
          WffNode *i;
          char s[40];
          char s2[40];
          char s3[40];
                   
          strcpy(s3,n1->rightson->atom->item );
          if(m->connective==AND){
                  sprintf(s2,"IS-VIOLATED-%s",m1->sons->name);
                  if(same_pref_name(s2, s3) == 0){
                      found2 = 2;
		   }
		  if(same_pref_name(s2, s3) == 1){
                       found2 = 3;
                      var = TRUE;
		      sprintf(s,"IS-VIOLATED-%s",m1->sons->new_name);
                      n1->rightson->atom = new_TokenList();
                      n1->rightson->atom->item = new_Token(30);
                      strcpy(n1->rightson->atom->item, s);
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-VIOLATED-%s",i->name);
                    if( same_pref_name(s2, s3) == 0){
                         found2 = 2;
		     }
		    if( same_pref_name(s2, s3) == 1){
                      found2 = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-VIOLATED-%s",i->new_name);
                         n1->rightson->atom = new_TokenList();
                         n1->rightson->atom->item = new_Token(30);
                         strcpy(n1->rightson->atom->item, s);
		      }
		     else{      
                      ParseExpNode *node = new_ParseExpNode(AD);
                      if(nn1->connective == MU)
                         node->rightson = new_ParseExpNode(MU);
                      if(nn1->connective == DI)
                         node->rightson = new_ParseExpNode(DI);
                      node->rightson->leftson = new_ParseExpNode(nn1->leftson->connective);
                      node->rightson->leftson->atom = copy_TokenList(nn1->leftson->atom);
                      sprintf(s,"IS-VIOLATED-%s",i->new_name);
                      node->rightson->rightson = new_ParseExpNode(nn1->rightson->connective);
                      node->rightson->rightson->atom = new_TokenList();
                      node->rightson->rightson->atom->item = (char *) calloc(30, sizeof(char));
                      strcpy(node->rightson->rightson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;

		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-VIOLATED-%s",m1->name);
                  if(same_pref_name(s2, n1->rightson->atom->item) == 0){
                       found2 = 2;
		    }
		  if(same_pref_name(s2, n1->rightson->atom->item) == 1){
                       found2 = 3;
		      sprintf(s,"IS-VIOLATED-%s",m1->new_name);
                      strcpy(n1->rightson->atom->item, s);
		   
		  }
            }

	}       
      
        /* end preference goal*/
        /* 2.normal constraints preference
	**/
        if(gconstraints_pref){
           WffNode *m1 = gconstraints_pref;
          PlNode *m =  pref_constraints_head;
          WffNode *i;
          char s[40];
          char s2[40];
          char s3[40];
                   
          strcpy(s3,n1->rightson->atom->item );
          if(m->connective==AND){
                  sprintf(s2,"IS-VIOLATED-%s",m1->sons->new_name);
                  if(same_pref_name(s2, s3) == 0){
                      found2 = 2;
		    }
		  if(same_pref_name(s2, s3) == 1){
                       found2 = 3;
                      var = TRUE;
		      sprintf(s,"IS-VIOLATED-%s",m1->sons->new_name);
                      n1->rightson->atom = new_TokenList();
                      n1->rightson->atom->item = new_Token(30);
                      strcpy(n1->rightson->atom->item, s);
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-VIOLATED-%s",i->new_name);
                    if( same_pref_name(s2, s3) == 0){
                         found2 = 2;
		     }
		    if( same_pref_name(s2, s3) == 1){
                      found2 = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-VIOLATED-%s",i->new_name);
                         n1->rightson->atom = new_TokenList();
                         n1->rightson->atom->item = new_Token(30);
                         strcpy(n1->rightson->atom->item, s);
		      }
		     else{      
                      ParseExpNode *node = new_ParseExpNode(AD);
                      if(nn1->connective == MU)
                         node->rightson = new_ParseExpNode(MU);
                      if(nn1->connective == DI)
                         node->rightson = new_ParseExpNode(DI);
                      node->rightson->leftson = new_ParseExpNode(nn1->leftson->connective);
                      node->rightson->leftson->atom = copy_TokenList(nn1->leftson->atom);
                      sprintf(s,"IS-VIOLATED-%s",i->new_name);
                      node->rightson->rightson = new_ParseExpNode(nn1->rightson->connective);
                      node->rightson->rightson->atom = new_TokenList();
                        node->rightson->rightson->atom->item = (char *) calloc(30, sizeof(char));
                      strcpy(node->rightson->rightson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;

		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-VIOLATED-%s",m1->new_name);
                  if(same_pref_name(s2, n1->rightson->atom->item) == 0){
                      found2 = 2;
		   }
		  if(same_pref_name(s2, n1->rightson->atom->item) == 1){
                       found2 = 3;
		      sprintf(s,"IS-VIOLATED-%s",m1->new_name);
                      strcpy(n1->rightson->atom->item, s);
		   
		  }
            }

	}
       /*end normal constraints preference*/
        /* 3.within constraints preference
	**/
	if(gwithin_constraints_pref) {
          WffNode *m1 = gwithin_constraints_pref;
          PlNode *m =  within_constraints_pref;
          WffNode *i;
          char s[40];
          char s2[40];
          char s3[40];
                   
          strcpy(s3,n1->rightson->atom->item );
          if(m->connective==AND){
                  sprintf(s2,"IS-VIOLATED-%s",m1->sons->new_name);
                  if(same_pref_name(s2, s3) == 0){
                      found2 = 2;
		    }
		  if(same_pref_name(s2, s3) == 1){
                      found2 = 3;
                      var = TRUE;
		      sprintf(s,"IS-VIOLATED-%s",m1->sons->new_name);
                      n1->rightson->atom = new_TokenList();
                      n1->rightson->atom->item = new_Token(30);
                      strcpy(n1->rightson->atom->item, s);
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-VIOLATED-%s",i->new_name);
                    if( same_pref_name(s2, s3) == 0){
                        found2 = 2;
		     }
		    if( same_pref_name(s2, s3) == 1){
                      found2 = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-VIOLATED-%s",i->new_name);
                         n1->rightson->atom = new_TokenList();
                         n1->rightson->atom->item = new_Token(30);
                         strcpy(n1->rightson->atom->item, s);
		      }
		     else{      
                      ParseExpNode *node = new_ParseExpNode(AD);
                      if(nn1->connective == MU)
                         node->rightson = new_ParseExpNode(MU);
                      if(nn1->connective == DI)
                         node->rightson = new_ParseExpNode(DI);
                      node->rightson->leftson = new_ParseExpNode(nn1->leftson->connective);
                      node->rightson->leftson->atom = copy_TokenList(nn1->leftson->atom);
                      sprintf(s,"IS-VIOLATED-%s",i->new_name);
                      node->rightson->rightson = new_ParseExpNode(nn1->rightson->connective);
                      node->rightson->rightson->atom = new_TokenList();
                        node->rightson->rightson->atom->item = (char *) calloc(30, sizeof(char));
                      strcpy(node->rightson->rightson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;

		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-VIOLATED-%s",m1->new_name);
                  if(same_pref_name(s2, n1->rightson->atom->item) == 0){
                        found2 = 2;
		   }
		  if(same_pref_name(s2, n1->rightson->atom->item) == 1){
                       found2 = 3;
		      sprintf(s,"IS-VIOLATED-%s",m1->new_name);
                      strcpy(n1->rightson->atom->item, s);
		   
		  }
            }
        

	}
        /*end within constraints preference*/       
        /* 4.til constraints preference
	**/
	if(gtil_constraints_pref) {
          WffNode *m1 = gtil_constraints_pref;
          PlNode *m =  til_constraints_pref;
          WffNode *i;
          char s[40];
          char s2[40];
          char s3[40];
                   
          strcpy(s3,n1->rightson->atom->item );
          if(m->connective==AND){
                  sprintf(s2,"IS-VIOLATED-%s",m1->sons->new_name);
                  if(same_pref_name(s2, s3) == 0){
                      found2 = 2;
		   }
		  if(same_pref_name(s2, s3) == 1){
                       found2 = 3;
                      var = TRUE;
		      sprintf(s,"IS-VIOLATED-%s",m1->sons->new_name);
                      n1->rightson->atom = new_TokenList();
                      n1->rightson->atom->item = new_Token(30);
                      strcpy(n1->rightson->atom->item, s);
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-VIOLATED-%s",i->new_name);
                    if( same_pref_name(s2, s3) == 0){
                         found2 = 2;
		     }
		    if( same_pref_name(s2, s3) == 1){
                      found2 = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-VIOLATED-%s",i->new_name);
                         n1->rightson->atom = new_TokenList();
                         n1->rightson->atom->item = new_Token(30);
                         strcpy(n1->rightson->atom->item, s);
		      }
		     else{      
                      ParseExpNode *node = new_ParseExpNode(AD);
                      if(nn1->connective == MU)
                         node->rightson = new_ParseExpNode(MU);
                      if(nn1->connective == DI)
                         node->rightson = new_ParseExpNode(DI);
                      node->rightson->leftson = new_ParseExpNode(nn1->leftson->connective);
                      node->rightson->leftson->atom = copy_TokenList(nn1->leftson->atom);
                      sprintf(s,"IS-VIOLATED-%s",i->new_name);
                      node->rightson->rightson = new_ParseExpNode(nn1->rightson->connective);
                      node->rightson->rightson->atom = new_TokenList();
                        node->rightson->rightson->atom->item = (char *) calloc(30, sizeof(char));
                      strcpy(node->rightson->rightson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;

		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-VIOLATED-%s",m1->new_name);
                   if(same_pref_name(s2, n1->rightson->atom->item) == 0){
                       found2 = 2;
		    }
		  if(same_pref_name(s2, n1->rightson->atom->item) == 1){
                       found2 = 3;
		      sprintf(s,"IS-VIOLATED-%s",m1->new_name);
                      strcpy(n1->rightson->atom->item, s);
		   
		  }
            }

	}
        /*end til constraints preference*/       
     
     }

    /* IS-SATISFIED
    **/    
     if(strncmp(str2, n1->rightson->atom->item, 11) == 0){
        found2 = 1;
        /* 1.preferences goal
        **/ 
        if(gpref_head){
          WffNode *m1 = gpref_head;
          PlNode *m = pref_head;
          WffNode *i;
          char s[40];
          char s2[40];
          char s3[40];
                   
          strcpy(s3,n1->rightson->atom->item );
          if(m->connective==AND){
                  sprintf(s2,"IS-SATISFIED-%s",m1->sons->name);
                  if(same_pref_name(s2, s3) == 0){
                      found2 = 2;
		   }
		  if(same_pref_name(s2, s3) == 1){
                       found2 = 3;
                      var = TRUE;
		      sprintf(s,"IS-SATISFIED-%s",m1->sons->new_name);
                      n1->rightson->atom = new_TokenList();
                      n1->rightson->atom->item = new_Token(30);
                      strcpy(n1->rightson->atom->item, s);
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-SATISFIED-%s",i->name);
                    if( same_pref_name(s2, s3) == 0){
                         found2 = 2;
		     }
		    if( same_pref_name(s2, s3) == 1){
                      found2 = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-SATISFIED-%s",i->new_name);
                         n1->rightson->atom = new_TokenList();
                         n1->rightson->atom->item = new_Token(30);
                         strcpy(n1->rightson->atom->item, s);
		      }
		     else{      
                      ParseExpNode *node = new_ParseExpNode(AD);
                      if(nn1->connective == MU)
                         node->rightson = new_ParseExpNode(MU);
                      if(nn1->connective == DI)
                         node->rightson = new_ParseExpNode(DI);
                      node->rightson->leftson = new_ParseExpNode(nn1->leftson->connective);
                      node->rightson->leftson->atom = copy_TokenList(nn1->leftson->atom);
                      sprintf(s,"IS-SATISFIED-%s",i->new_name);
                      node->rightson->rightson = new_ParseExpNode(nn1->rightson->connective);
                      node->rightson->rightson->atom = new_TokenList();
                      node->rightson->rightson->atom->item = (char *) calloc(30, sizeof(char));
                      strcpy(node->rightson->rightson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;

		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-SATISFIED-%s",m1->name);
                  if(same_pref_name(s2, n1->rightson->atom->item) == 0){
                       found2 = 2;
		    }
		  if(same_pref_name(s2, n1->rightson->atom->item) == 1){
                       found2 = 3;
		      sprintf(s,"IS-SATISFIED-%s",m1->new_name);
                      strcpy(n1->rightson->atom->item, s);
		   
		  }
            }

	}       
      
        /* end preference goal*/
        /* 2.normal constraints preference
	**/
        if(gconstraints_pref){
           WffNode *m1 = gconstraints_pref;
          PlNode *m =  pref_constraints_head;
          WffNode *i;
          char s[40];
          char s2[40];
          char s3[40];
                   
          strcpy(s3,n1->rightson->atom->item );
          if(m->connective==AND){
                  sprintf(s2,"IS-SATISFIED-%s",m1->sons->new_name);
                  if(same_pref_name(s2, s3) == 0){
                      found2 = 2;
		    }
		  if(same_pref_name(s2, s3) == 1){
                       found2 = 3;
                      var = TRUE;
		      sprintf(s,"IS-SATISFIED-%s",m1->sons->new_name);
                      n1->rightson->atom = new_TokenList();
                      n1->rightson->atom->item = new_Token(30);
                      strcpy(n1->rightson->atom->item, s);
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-SATISFIED-%s",i->new_name);
                    if( same_pref_name(s2, s3) == 0){
                         found2 = 2;
		     }
		    if( same_pref_name(s2, s3) == 1){
                      found2 = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-SATISFIED-%s",i->new_name);
                         n1->rightson->atom = new_TokenList();
                         n1->rightson->atom->item = new_Token(30);
                         strcpy(n1->rightson->atom->item, s);
		      }
		     else{      
                      ParseExpNode *node = new_ParseExpNode(AD);
                      if(nn1->connective == MU)
                         node->rightson = new_ParseExpNode(MU);
                      if(nn1->connective == DI)
                         node->rightson = new_ParseExpNode(DI);
                      node->rightson->leftson = new_ParseExpNode(nn1->leftson->connective);
                      node->rightson->leftson->atom = copy_TokenList(nn1->leftson->atom);
                      sprintf(s,"IS-SATISFIED-%s",i->new_name);
                      node->rightson->rightson = new_ParseExpNode(nn1->rightson->connective);
                      node->rightson->rightson->atom = new_TokenList();
                        node->rightson->rightson->atom->item = (char *) calloc(30, sizeof(char));
                      strcpy(node->rightson->rightson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;

		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-SATISFIED-%s",m1->new_name);
                  if(same_pref_name(s2, n1->rightson->atom->item) == 0){
                      found2 = 2;
		   }
		  if(same_pref_name(s2, n1->rightson->atom->item) == 1){
                       found2 = 3;
		      sprintf(s,"IS-SATISFIED-%s",m1->new_name);
                      strcpy(n1->rightson->atom->item, s);
		   
		  }
            }

	}
       /*end normal constraints preference*/
        /* 3.within constraints preference
	**/
	if(gwithin_constraints_pref) {
          WffNode *m1 = gwithin_constraints_pref;
          PlNode *m =  within_constraints_pref;
          WffNode *i;
          char s[40];
          char s2[40];
          char s3[40];
                   
          strcpy(s3,n1->rightson->atom->item );
          if(m->connective==AND){
                  sprintf(s2,"IS-SATISFIED-%s",m1->sons->new_name);
                  if(same_pref_name(s2, s3) == 0){
                      found2 = 2;
		    }
		  if(same_pref_name(s2, s3) == 1){
                      found2 = 3;
                      var = TRUE;
		      sprintf(s,"IS-SATISFIED-%s",m1->sons->new_name);
                      n1->rightson->atom = new_TokenList();
                      n1->rightson->atom->item = new_Token(30);
                      strcpy(n1->rightson->atom->item, s);
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-SATISFIED-%s",i->new_name);
                    if( same_pref_name(s2, s3) == 0){
                        found2 = 2;
		     }
		    if( same_pref_name(s2, s3) == 1){
                      found2 = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-SATISFIED-%s",i->new_name);
                         n1->rightson->atom = new_TokenList();
                         n1->rightson->atom->item = new_Token(30);
                         strcpy(n1->rightson->atom->item, s);
		      }
		     else{      
                      ParseExpNode *node = new_ParseExpNode(AD);
                      if(nn1->connective == MU)
                         node->rightson = new_ParseExpNode(MU);
                      if(nn1->connective == DI)
                         node->rightson = new_ParseExpNode(DI);
                      node->rightson->leftson = new_ParseExpNode(nn1->leftson->connective);
                      node->rightson->leftson->atom = copy_TokenList(nn1->leftson->atom);
                      sprintf(s,"IS-SATISFIED-%s",i->new_name);
                      node->rightson->rightson = new_ParseExpNode(nn1->rightson->connective);
                      node->rightson->rightson->atom = new_TokenList();
                        node->rightson->rightson->atom->item = (char *) calloc(30, sizeof(char));
                      strcpy(node->rightson->rightson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;

		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-SATISFIED-%s",m1->new_name);
                  if(same_pref_name(s2, n1->rightson->atom->item) == 0){
                        found2 = 2;
		   }
		  if(same_pref_name(s2, n1->rightson->atom->item) == 1){
                       found2 = 3;
		      sprintf(s,"IS-SATISFIED-%s",m1->new_name);
                      strcpy(n1->rightson->atom->item, s);
		   
		  }
            }
        

	}
        /*end within constraints preference*/       
        /* 4.til constraints preference
	**/
	if(gtil_constraints_pref) {
          WffNode *m1 = gtil_constraints_pref;
          PlNode *m =  til_constraints_pref;
          WffNode *i;
          char s[40];
          char s2[40];
          char s3[40];
                   
          strcpy(s3,n1->rightson->atom->item );
          if(m->connective==AND){
                  sprintf(s2,"IS-SATISFIED-%s",m1->sons->new_name);
                  if(same_pref_name(s2, s3) == 0){
                      found2 = 2;
		   }
		  if(same_pref_name(s2, s3) == 1){
                       found2 = 3;
                      var = TRUE;
		      sprintf(s,"IS-SATISFIED-%s",m1->sons->new_name);
                      n1->rightson->atom = new_TokenList();
                      n1->rightson->atom->item = new_Token(30);
                      strcpy(n1->rightson->atom->item, s);
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-SATISFIED-%s",i->new_name);
                    if( same_pref_name(s2, s3) == 0){
                         found2 = 2;
		     }
		    if( same_pref_name(s2, s3) == 1){
                      found2 = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-SATISFIED-%s",i->new_name);
                         n1->rightson->atom = new_TokenList();
                         n1->rightson->atom->item = new_Token(30);
                         strcpy(n1->rightson->atom->item, s);
		      }
		     else{      
                      ParseExpNode *node = new_ParseExpNode(AD);
                      if(nn1->connective == MU)
                         node->rightson = new_ParseExpNode(MU);
                      if(nn1->connective == DI)
                         node->rightson = new_ParseExpNode(DI);
                      node->rightson->leftson = new_ParseExpNode(nn1->leftson->connective);
                      node->rightson->leftson->atom = copy_TokenList(nn1->leftson->atom);
                      sprintf(s,"IS-SATISFIED-%s",i->new_name);
                      node->rightson->rightson = new_ParseExpNode(nn1->rightson->connective);
                      node->rightson->rightson->atom = new_TokenList();
                        node->rightson->rightson->atom->item = (char *) calloc(30, sizeof(char));
                      strcpy(node->rightson->rightson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;

		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-SATISFIED-%s",m1->new_name);
                   if(same_pref_name(s2, n1->rightson->atom->item) == 0){
                       found2 = 2;
		    }
		  if(same_pref_name(s2, n1->rightson->atom->item) == 1){
                       found2 = 3;
		      sprintf(s,"IS-SATISFIED-%s",m1->new_name);
                      strcpy(n1->rightson->atom->item, s);
		   
		  }
            }

	}
        /*end til constraints preference*/       
     
     }
    }


    /* for IS-VIOLATED
     * leftson
     **/
    if(n1->leftson->connective == FHEAD){

     /* IS-VIOLATED
     **/
     if(strncmp(str1, n1->leftson->atom->item, 10) == 0){
	 found2 = 1;
        /* 1.preferences goal
        **/ 
        if(gpref_head){
          WffNode *m1 = gpref_head;
          PlNode *m = pref_head;
          WffNode *i;
          char s[40];
          char s2[40];
          char s3[40];
                   
          strcpy(s3,n1->leftson->atom->item );
          if(m->connective==AND){
                  sprintf(s2,"IS-VIOLATED-%s",m1->sons->name);
                   if(same_pref_name(s2, s3) == 0){
                        found2 = 2;
		    }
		  if(same_pref_name(s2, s3) == 1){
                       found2 = 3;
                      var = TRUE;
		      sprintf(s,"IS-VIOLATED-%s",m1->sons->new_name);
                      n1->leftson->atom = new_TokenList();
                      n1->leftson->atom->item = new_Token(30);
                      strcpy(n1->leftson->atom->item, s);
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-VIOLATED-%s",i->name);
                    if( same_pref_name(s2, s3) == 0){
                        found2 = 2;
		    }
		    if( same_pref_name(s2, s3) == 1){
                      found2 = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-VIOLATED-%s",i->new_name);
                         n1->leftson->atom = new_TokenList();
                         n1->leftson->atom->item = new_Token(30);
                         strcpy(n1->leftson->atom->item, s);
		      }
		     else{      
                      ParseExpNode *node = new_ParseExpNode(AD);
                      if(nn1->connective == MU)
                         node->rightson = new_ParseExpNode(MU);
                      if(nn1->connective == DI)
                         node->rightson = new_ParseExpNode(DI);
                      node->rightson->rightson = new_ParseExpNode(nn1->rightson->connective);
                      node->rightson->rightson->atom = copy_TokenList(nn1->rightson->atom);
                      sprintf(s,"IS-VIOLATED-%s",i->new_name);
                      node->rightson->leftson = new_ParseExpNode(nn1->leftson->connective);
                      node->rightson->leftson->atom = new_TokenList();
                        node->rightson->leftson->atom->item = (char *) calloc(30, sizeof(char));
                      strcpy(node->rightson->leftson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;

		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-VIOLATED-%s",m1->name);
                  if(same_pref_name(s2, n1->leftson->atom->item) == 0){
                       found2 = 2;
		   }
		  if(same_pref_name(s2, n1->leftson->atom->item) == 1){
                       found2 = 3;
		      sprintf(s,"IS-VIOLATED-%s",m1->new_name);
                      strcpy(n1->leftson->atom->item, s);
		   
		  }
            }

	}       
      
        /* end preference goal*/
        /* 2.normal constraints preference
	**/
        if(gconstraints_pref){
           WffNode *m1 = gconstraints_pref;
          PlNode *m =  pref_constraints_head;
          WffNode *i;
          char s[40];
          char s2[40];
          char s3[40];
                   
          strcpy(s3,n1->leftson->atom->item );
          if(m->connective==AND){
                  sprintf(s2,"IS-VIOLATED-%s",m1->sons->new_name);
                  if(same_pref_name(s2, s3) == 0){
                      found2 = 2;
		   }
		  if(same_pref_name(s2, s3) == 1){
                      found2 = 3;
                      var = TRUE;
		      sprintf(s,"IS-VIOLATED-%s",m1->sons->new_name);
                      n1->leftson->atom = new_TokenList();
                      n1->leftson->atom->item = new_Token(30);
                      strcpy(n1->leftson->atom->item, s);
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-VIOLATED-%s",i->new_name);
                    if( same_pref_name(s2, s3) == 0){
                         found2 = 2;
		     }
		    if( same_pref_name(s2, s3) == 1){
                      found2 = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-VIOLATED-%s",i->new_name);
                         n1->leftson->atom = new_TokenList();
                         n1->leftson->atom->item = new_Token(30);
                         strcpy(n1->leftson->atom->item, s);
		      }
		     else{      
                      ParseExpNode *node = new_ParseExpNode(AD);
                      if(nn1->connective == MU)
                         node->rightson = new_ParseExpNode(MU);
                      if(nn1->connective == DI)
                         node->rightson = new_ParseExpNode(DI);
                      node->rightson->rightson = new_ParseExpNode(nn1->rightson->connective);
                      node->rightson->rightson->atom = copy_TokenList(nn1->rightson->atom);
                      sprintf(s,"IS-VIOLATED-%s",i->new_name);
                      node->rightson->leftson = new_ParseExpNode(nn1->leftson->connective);
                      node->rightson->leftson->atom = new_TokenList();
                        node->rightson->leftson->atom->item = (char *) calloc(30, sizeof(char));
                      strcpy(node->rightson->leftson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;

		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-VIOLATED-%s",m1->new_name);
                   if(same_pref_name(s2, n1->leftson->atom->item) == 0){
                      found2 = 2;
		   }
		  if(same_pref_name(s2, n1->leftson->atom->item) == 1){
                       found2 = 3;
		      sprintf(s,"IS-VIOLATED-%s",m1->new_name);
                      strcpy(n1->leftson->atom->item, s);
		   
		  }
            }


	}
       /*end normal constraints preference*/
        /* 3.within constraints preference
	**/
	if(gwithin_constraints_pref) {
          WffNode *m1 = gwithin_constraints_pref;
          PlNode *m =  within_constraints_pref;
          WffNode *i;
          char s[40];
          char s2[40];
          char s3[40];
                   
          strcpy(s3,n1->leftson->atom->item );
          if(m->connective==AND){
                  sprintf(s2,"IS-VIOLATED-%s",m1->sons->new_name);
                  if(same_pref_name(s2, s3) == 0){
                        found2 = 2;
		   }
		  if(same_pref_name(s2, s3) == 1){
                       found2 = 3;
                      var = TRUE;
		      sprintf(s,"IS-VIOLATED-%s",m1->sons->new_name);
                      n1->leftson->atom = new_TokenList();
                      n1->leftson->atom->item = new_Token(30);
                      strcpy(n1->leftson->atom->item, s);
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-VIOLATED-%s",i->new_name);
                    if( same_pref_name(s2, s3) == 0){
                        found2 = 2;
		     }
		    if( same_pref_name(s2, s3) == 1){
                      found2 = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-VIOLATED-%s",i->new_name);
                         n1->leftson->atom = new_TokenList();
                         n1->leftson->atom->item = new_Token(30);
                         strcpy(n1->leftson->atom->item, s);
		      }
		     else{      
                      ParseExpNode *node = new_ParseExpNode(AD);
                      if(nn1->connective == MU)
                         node->rightson = new_ParseExpNode(MU);
                      if(nn1->connective == DI)
                         node->rightson = new_ParseExpNode(DI);
                      node->rightson->rightson = new_ParseExpNode(nn1->rightson->connective);
                      node->rightson->rightson->atom = copy_TokenList(nn1->rightson->atom);
                      sprintf(s,"IS-VIOLATED-%s",i->new_name);
                      node->rightson->leftson = new_ParseExpNode(nn1->leftson->connective);
                      node->rightson->leftson->atom = new_TokenList();
                        node->rightson->leftson->atom->item = (char *) calloc(30, sizeof(char));
                      strcpy(node->rightson->leftson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;

		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-VIOLATED-%s",m1->new_name);
                  if(same_pref_name(s2, n1->leftson->atom->item) == 0){
                      found2 = 2;
		   }
		  if(same_pref_name(s2, n1->leftson->atom->item) == 1){
                       found2 = 3;
		      sprintf(s,"IS-VIOLATED-%s",m1->new_name);
                      strcpy(n1->leftson->atom->item, s);
		   
		  }
            }

	}
        /*end within constraints preference*/       
        /* 4.til constraints preference
	**/
	if(gtil_constraints_pref) {
          WffNode *m1 = gtil_constraints_pref;
          PlNode *m =  til_constraints_pref;
          WffNode *i;
          char s[40];
          char s2[40];
          char s3[40];
                   
          strcpy(s3,n1->leftson->atom->item );
          if(m->connective==AND){
                  sprintf(s2,"IS-VIOLATED-%s",m1->sons->new_name);
                  if(same_pref_name(s2, s3) == 0){
                       found2 = 2;
		   }
		  if(same_pref_name(s2, s3) == 1){
                       found2 = 3;
                      var = TRUE;
		      sprintf(s,"IS-VIOLATED-%s",m1->sons->new_name);
                      n1->leftson->atom = new_TokenList();
                      n1->leftson->atom->item = new_Token(30);
                      strcpy(n1->leftson->atom->item, s);
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-VIOLATED-%s",i->new_name);
                    if( same_pref_name(s2, s3) == 0){
                        found2 = 2;
		     }
		    if( same_pref_name(s2, s3) == 1){
                      found2 = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-VIOLATED-%s",i->new_name);
                         n1->leftson->atom = new_TokenList();
                         n1->leftson->atom->item = new_Token(30);
                         strcpy(n1->leftson->atom->item, s);
		      }
		     else{      
                      ParseExpNode *node = new_ParseExpNode(AD);
                      if(nn1->connective == MU)
                         node->rightson = new_ParseExpNode(MU);
                      if(nn1->connective == DI)
                         node->rightson = new_ParseExpNode(DI);
                      node->rightson->rightson = new_ParseExpNode(nn1->rightson->connective);
                      node->rightson->rightson->atom = copy_TokenList(nn1->rightson->atom);
                      sprintf(s,"IS-VIOLATED-%s",i->new_name);
                      node->rightson->leftson = new_ParseExpNode(nn1->leftson->connective);
                      node->rightson->leftson->atom = new_TokenList();
                        node->rightson->leftson->atom->item = (char *) calloc(30, sizeof(char));
                      strcpy(node->rightson->leftson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;

		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-VIOLATED-%s",m1->new_name);
                   if(same_pref_name(s2, n1->leftson->atom->item) == 0){
                      found2 = 2;
		    }
		  if(same_pref_name(s2, n1->leftson->atom->item) == 1){
                       found2 = 3;
		      sprintf(s,"IS-VIOLATED-%s",m1->new_name);
                      strcpy(n1->leftson->atom->item, s);
		   
		  }
            }

	}
        /*end til constraints preference*/       
     
     }

    /* IS-SATISFIED
    **/
    if(strncmp(str2, n1->leftson->atom->item, 11) == 0){
	 found2 = 1;
        /* 1.preferences goal
        **/ 
        if(gpref_head){
          WffNode *m1 = gpref_head;
          PlNode *m = pref_head;
          WffNode *i;
          char s[40];
          char s2[40];
          char s3[40];
                   
          strcpy(s3,n1->leftson->atom->item );
          if(m->connective==AND){
                  sprintf(s2,"IS-SATISFIED-%s",m1->sons->name);
                   if(same_pref_name(s2, s3) == 0){
                        found2 = 2;
		    }
		  if(same_pref_name(s2, s3) == 1){
                       found2 = 3;
                      var = TRUE;
		      sprintf(s,"IS-SATISFIED-%s",m1->sons->new_name);
                      n1->leftson->atom = new_TokenList();
                      n1->leftson->atom->item = new_Token(30);
                      strcpy(n1->leftson->atom->item, s);
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-SATISFIED-%s",i->name);
                    if( same_pref_name(s2, s3) == 0){
                        found2 = 2;
		    }
		    if( same_pref_name(s2, s3) == 1){
                      found2 = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-SATISFIED-%s",i->new_name);
                         n1->leftson->atom = new_TokenList();
                         n1->leftson->atom->item = new_Token(30);
                         strcpy(n1->leftson->atom->item, s);
		      }
		     else{      
                      ParseExpNode *node = new_ParseExpNode(AD);
                      if(nn1->connective == MU)
                         node->rightson = new_ParseExpNode(MU);
                      if(nn1->connective == DI)
                         node->rightson = new_ParseExpNode(DI);
                      node->rightson->rightson = new_ParseExpNode(nn1->rightson->connective);
                      node->rightson->rightson->atom = copy_TokenList(nn1->rightson->atom);
                      sprintf(s,"IS-SATISFIED-%s",i->new_name);
                      node->rightson->leftson = new_ParseExpNode(nn1->leftson->connective);
                      node->rightson->leftson->atom = new_TokenList();
                        node->rightson->leftson->atom->item = (char *) calloc(30, sizeof(char));
                      strcpy(node->rightson->leftson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;

		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-SATISFIED-%s",m1->name);
                  if(same_pref_name(s2, n1->leftson->atom->item) == 0){
                       found2 = 2;
		   }
		  if(same_pref_name(s2, n1->leftson->atom->item) == 1){
                       found2 = 3;
		      sprintf(s,"IS-SATISFIED-%s",m1->new_name);
                      strcpy(n1->leftson->atom->item, s);
		   
		  }
            }

	}       
      
        /* end preference goal*/
        /* 2.normal constraints preference
	**/
        if(gconstraints_pref){
           WffNode *m1 = gconstraints_pref;
          PlNode *m =  pref_constraints_head;
          WffNode *i;
          char s[40];
          char s2[40];
          char s3[40];
                   
          strcpy(s3,n1->leftson->atom->item );
          if(m->connective==AND){
                  sprintf(s2,"IS-SATISFIED-%s",m1->sons->new_name);
                  if(same_pref_name(s2, s3) == 0){
                      found2 = 2;
		   }
		  if(same_pref_name(s2, s3) == 1){
                      found2 = 3;
                      var = TRUE;
		      sprintf(s,"IS-SATISFIED-%s",m1->sons->new_name);
                      n1->leftson->atom = new_TokenList();
                      n1->leftson->atom->item = new_Token(30);
                      strcpy(n1->leftson->atom->item, s);
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-SATISFIED-%s",i->new_name);
                    if( same_pref_name(s2, s3) == 0){
                         found2 = 2;
		     }
		    if( same_pref_name(s2, s3) == 1){
                      found2 = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-SATISFIED-%s",i->new_name);
                         n1->leftson->atom = new_TokenList();
                         n1->leftson->atom->item = new_Token(30);
                         strcpy(n1->leftson->atom->item, s);
		      }
		     else{      
                      ParseExpNode *node = new_ParseExpNode(AD);
                      if(nn1->connective == MU)
                         node->rightson = new_ParseExpNode(MU);
                      if(nn1->connective == DI)
                         node->rightson = new_ParseExpNode(DI);
                      node->rightson->rightson = new_ParseExpNode(nn1->rightson->connective);
                      node->rightson->rightson->atom = copy_TokenList(nn1->rightson->atom);
                      sprintf(s,"IS-SATISFIED-%s",i->new_name);
                      node->rightson->leftson = new_ParseExpNode(nn1->leftson->connective);
                      node->rightson->leftson->atom = new_TokenList();
                        node->rightson->leftson->atom->item = (char *) calloc(30, sizeof(char));
                      strcpy(node->rightson->leftson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;

		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-SATISFIED-%s",m1->new_name);
                   if(same_pref_name(s2, n1->leftson->atom->item) == 0){
                      found2 = 2;
		   }
		  if(same_pref_name(s2, n1->leftson->atom->item) == 1){
                       found2 = 3;
		      sprintf(s,"IS-SATISFIED-%s",m1->new_name);
                      strcpy(n1->leftson->atom->item, s);
		   
		  }
            }


	}
       /*end normal constraints preference*/
        /* 3.within constraints preference
	**/
	if(gwithin_constraints_pref) {
          WffNode *m1 = gwithin_constraints_pref;
          PlNode *m =  within_constraints_pref;
          WffNode *i;
          char s[40];
          char s2[40];
          char s3[40];
                   
          strcpy(s3,n1->leftson->atom->item );
          if(m->connective==AND){
                  sprintf(s2,"IS-SATISFIED-%s",m1->sons->new_name);
                  if(same_pref_name(s2, s3) == 0){
                        found2 = 2;
		   }
		  if(same_pref_name(s2, s3) == 1){
                       found2 = 3;
                      var = TRUE;
		      sprintf(s,"IS-SATISFIED-%s",m1->sons->new_name);
                      n1->leftson->atom = new_TokenList();
                      n1->leftson->atom->item = new_Token(30);
                      strcpy(n1->leftson->atom->item, s);
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-SATISFIED-%s",i->new_name);
                    if( same_pref_name(s2, s3) == 0){
                        found2 = 2;
		     }
		    if( same_pref_name(s2, s3) == 1){
                      found2 = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-SATISFIED-%s",i->new_name);
                         n1->leftson->atom = new_TokenList();
                         n1->leftson->atom->item = new_Token(30);
                         strcpy(n1->leftson->atom->item, s);
		      }
		     else{      
                      ParseExpNode *node = new_ParseExpNode(AD);
                      if(nn1->connective == MU)
                         node->rightson = new_ParseExpNode(MU);
                      if(nn1->connective == DI)
                         node->rightson = new_ParseExpNode(DI);
                      node->rightson->rightson = new_ParseExpNode(nn1->rightson->connective);
                      node->rightson->rightson->atom = copy_TokenList(nn1->rightson->atom);
                      sprintf(s,"IS-SATISFIED-%s",i->new_name);
                      node->rightson->leftson = new_ParseExpNode(nn1->leftson->connective);
                      node->rightson->leftson->atom = new_TokenList();
                        node->rightson->leftson->atom->item = (char *) calloc(30, sizeof(char));
                      strcpy(node->rightson->leftson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;

		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-SATISFIED-%s",m1->new_name);
                  if(same_pref_name(s2, n1->leftson->atom->item) == 0){
                      found2 = 2;
		   }
		  if(same_pref_name(s2, n1->leftson->atom->item) == 1){
                       found2 = 3;
		      sprintf(s,"IS-SATISFIED-%s",m1->new_name);
                      strcpy(n1->leftson->atom->item, s);
		   
		  }
            }

	}
        /*end within constraints preference*/       
        /* 4.til constraints preference
	**/
	if(gtil_constraints_pref) {
          WffNode *m1 = gtil_constraints_pref;
          PlNode *m =  til_constraints_pref;
          WffNode *i;
          char s[40];
          char s2[40];
          char s3[40];
                   
          strcpy(s3,n1->leftson->atom->item );
          if(m->connective==AND){
                  sprintf(s2,"IS-SATISFIED-%s",m1->sons->new_name);
                  if(same_pref_name(s2, s3) == 0){
                       found2 = 2;
		   }
		  if(same_pref_name(s2, s3) == 1){
                       found2 = 3;
                      var = TRUE;
		      sprintf(s,"IS-SATISFIED-%s",m1->sons->new_name);
                      n1->leftson->atom = new_TokenList();
                      n1->leftson->atom->item = new_Token(30);
                      strcpy(n1->leftson->atom->item, s);
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-SATISFIED-%s",i->new_name);
                    if( same_pref_name(s2, s3) == 0){
                        found2 = 2;
		     }
		    if( same_pref_name(s2, s3) == 1){
                      found2 = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-SATISFIED-%s",i->new_name);
                         n1->leftson->atom = new_TokenList();
                         n1->leftson->atom->item = new_Token(30);
                         strcpy(n1->leftson->atom->item, s);
		      }
		     else{      
                      ParseExpNode *node = new_ParseExpNode(AD);
                      if(nn1->connective == MU)
                         node->rightson = new_ParseExpNode(MU);
                      if(nn1->connective == DI)
                         node->rightson = new_ParseExpNode(DI);
                      node->rightson->rightson = new_ParseExpNode(nn1->rightson->connective);
                      node->rightson->rightson->atom = copy_TokenList(nn1->rightson->atom);
                      sprintf(s,"IS-SATISFIED-%s",i->new_name);
                      node->rightson->leftson = new_ParseExpNode(nn1->leftson->connective);
                      node->rightson->leftson->atom = new_TokenList();
                        node->rightson->leftson->atom->item = (char *) calloc(30, sizeof(char));
                      strcpy(node->rightson->leftson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;

		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-SATISFIED-%s",m1->new_name);
                   if(same_pref_name(s2, n1->leftson->atom->item) == 0){
                      found2 = 2;
		    }
		  if(same_pref_name(s2, n1->leftson->atom->item) == 1){
                       found2 = 3;
		      sprintf(s,"IS-SATISFIED-%s",m1->new_name);
                      strcpy(n1->leftson->atom->item, s);
		   
		  }
            }

	}
        /*end til constraints preference*/       
     
     }

    }

    /*++++++++++++++++++++++++++++++++++*/ 

  }
    else{
       if (n1->connective == FHEAD){

     var = FALSE; 

    /* for IS-VIOLATED
    **/
    if(strncmp(str1, n1->atom->item, 10) == 0){
        found2 = 1;
        /* 1.preferences goal
        **/ 
        if(gpref_head){
          WffNode *m1 = gpref_head;
          PlNode *m = pref_head;
          WffNode *i;
          char s[40];
          char s2[40];
          char s3[40];
                   
          strcpy(s3, n1->atom->item);
          if(m->connective==AND){
                  sprintf(s2,"IS-VIOLATED-%s",m1->sons->name);
                   if(same_pref_name(s2, s3) == 0){
                      found2 = 2;
		   }
		  if(same_pref_name(s2, s3) == 1){
                       found2 = 3;
                      var = TRUE;
		      sprintf(s,"IS-VIOLATED-%s",m1->sons->new_name);
                      n1->atom = new_TokenList();
                      n1->atom->item = new_Token(30);
                      strcpy(n1->atom->item, s);
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-VIOLATED-%s",i->name);
                    if( same_pref_name(s2, s3) == 0){
                        found2 = 2;
		     }
		    if( same_pref_name(s2, s3) == 1){
                      found2 = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-VIOLATED-%s",i->new_name);
                         n1->atom = new_TokenList();
                         n1->atom->item = new_Token(30);
                         strcpy(n1->atom->item, s);
		      }
		     else{      
                      ParseExpNode *node = new_ParseExpNode(AD);
                      sprintf(s,"IS-VIOLATED-%s",i->new_name);
                      node->rightson = new_ParseExpNode(nn1->connective);
                      node->rightson->atom = new_TokenList();
                      node->rightson->atom->item = (char *) calloc(30, sizeof(char));
                      strcpy(node->rightson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;

		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-VIOLATED-%s",m1->name);
                   if(same_pref_name(s2, n1->atom->item) == 0){
                     found2 = 2;
		   }
		  if(same_pref_name(s2, n1->atom->item) == 1){
                       found2 = 3;
		      sprintf(s,"IS-VIOLATED-%s",m1->new_name);
                      strcpy(n1->atom->item, s);
		   
		  }
            }

	} 
        /* end preference goal*/
        /* 2.normal constraints preference
	**/
        if(gconstraints_pref){
          WffNode *m1 = gconstraints_pref;
          PlNode *m =  pref_constraints_head;
          WffNode *i;
          char s[40];
          char s2[40];
          char s3[40];
                   
          strcpy(s3, n1->atom->item);
          if(m->connective==AND){
                  sprintf(s2,"IS-VIOLATED-%s",m1->sons->new_name);
                  if(same_pref_name(s2, s3) == 0){
                       found2 = 2;
		    }
		  if(same_pref_name(s2, s3) == 1){
                      found2 = 3;
                      var = TRUE;
		      sprintf(s,"IS-VIOLATED-%s",m1->sons->new_name);
                      n1->atom = new_TokenList();
                      n1->atom->item = new_Token(30);
                      strcpy(n1->atom->item, s);
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-VIOLATED-%s",i->new_name);
                    if( same_pref_name(s2, s3) == 0){
                         found2 = 2;
		     }
		    if( same_pref_name(s2, s3) == 1){
                      found2 = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-VIOLATED-%s",i->new_name);
                         n1->atom = new_TokenList();
                         n1->atom->item = new_Token(30);
                         strcpy(n1->atom->item, s);
		      }
		     else{      
                      ParseExpNode *node = new_ParseExpNode(AD);
                      sprintf(s,"IS-VIOLATED-%s",i->new_name);
                      node->rightson = new_ParseExpNode(nn1->connective);
                      node->rightson->atom = new_TokenList();
                      node->rightson->atom->item = (char *) calloc(30, sizeof(char));
                      strcpy(node->rightson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;

		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-VIOLATED-%s",m1->new_name);
                  if(same_pref_name(s2, n1->atom->item) == 0){
                      found2 = 2;
		    }
		  if(same_pref_name(s2, n1->atom->item) == 1){
                       found2 = 3;
		      sprintf(s,"IS-VIOLATED-%s",m1->new_name);
                      strcpy(n1->atom->item, s);
		   
		  }
            }


	}
       /*end normal constraints preference*/
        /* 3.within constraints preference
	**/
	if(gwithin_constraints_pref){
          WffNode *m1 = gwithin_constraints_pref;
          PlNode *m =  within_constraints_pref;
          WffNode *i;
          char s[40];
          char s2[40];
          char s3[40];
                   
          strcpy(s3, n1->atom->item);
          if(m->connective==AND){
                  sprintf(s2,"IS-VIOLATED-%s",m1->sons->new_name);
                   if(same_pref_name(s2, s3) == 0){
                      found2 = 2;
		   }
		  if(same_pref_name(s2, s3) == 1){
                      found2 = 3;
                      var = TRUE;
		      sprintf(s,"IS-VIOLATED-%s",m1->sons->new_name);
                      n1->atom = new_TokenList();
                      n1->atom->item = new_Token(30);
                      strcpy(n1->atom->item, s);
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-VIOLATED-%s",i->new_name);
                    if( same_pref_name(s2, s3) == 0){
                        found2 = 2;
		     }
		    if( same_pref_name(s2, s3) == 1){
                      found2 = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-VIOLATED-%s",i->new_name);
                         n1->atom = new_TokenList();
                         n1->atom->item = new_Token(30);
                         strcpy(n1->atom->item, s);
		      }
		     else{      
                      ParseExpNode *node = new_ParseExpNode(AD);
                      sprintf(s,"IS-VIOLATED-%s",i->new_name);
                      node->rightson = new_ParseExpNode(nn1->connective);
                      node->rightson->atom = new_TokenList();
                        node->rightson->atom->item = (char *) calloc(30, sizeof(char));
                      strcpy(node->rightson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;

		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-VIOLATED-%s",m1->new_name);
                  if(same_pref_name(s2, n1->atom->item) == 0){
                      found2 = 2;
		    }
		  if(same_pref_name(s2, n1->atom->item) == 1){
                       found2 = 3;
		      sprintf(s,"IS-VIOLATED-%s",m1->new_name);
                      strcpy(n1->atom->item, s);
		   
		  }
            }

	}
        /*end within constraints preference*/    
         /* 4.within constraints preference
	**/
	if(gtil_constraints_pref){
          WffNode *m1 = gtil_constraints_pref;
          PlNode *m =  til_constraints_pref;
          WffNode *i;
          char s[40];
          char s2[40];
          char s3[40];
                   
          strcpy(s3, n1->atom->item);
          if(m->connective==AND){
                  sprintf(s2,"IS-VIOLATED-%s",m1->sons->new_name);
                  if(same_pref_name(s2, s3) == 0){
                        found2 = 2;
		    }
		  if(same_pref_name(s2, s3) == 1){
                       found2 = 3;
                      var = TRUE;
		      sprintf(s,"IS-VIOLATED-%s",m1->sons->new_name);
                      n1->atom = new_TokenList();
                      n1->atom->item = new_Token(30);
                      strcpy(n1->atom->item, s);
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-VIOLATED-%s",i->new_name);
                    if( same_pref_name(s2, s3) == 0){
                       found2 = 2;
		     }
		    if( same_pref_name(s2, s3) == 1){
                      found2 = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-VIOLATED-%s",i->new_name);
                         n1->atom = new_TokenList();
                         n1->atom->item = new_Token(30);
                         strcpy(n1->atom->item, s);
		      }
		     else{      
                      ParseExpNode *node = new_ParseExpNode(AD);
                      sprintf(s,"IS-VIOLATED-%s",i->new_name);
                      node->rightson = new_ParseExpNode(nn1->connective);
                      node->rightson->atom = new_TokenList();
                        node->rightson->atom->item = (char *) calloc(30, sizeof(char));
                      strcpy(node->rightson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;

		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-VIOLATED-%s",m1->new_name);
                  if(same_pref_name(s2, n1->atom->item) == 0){
                      found2 = 2;
		    }
		  if(same_pref_name(s2, n1->atom->item) == 1){
                       found2 = 3;
		      sprintf(s,"IS-VIOLATED-%s",m1->new_name);
                      strcpy(n1->atom->item, s);
		   
		  }
            }
           

	}
        /*end til constraints preference*/           
    }

    /* IS-SATISFIED
    **/
    if(strncmp(str2, n1->atom->item, 11) == 0){
        found2 = 1;
        /* 1.preferences goal
        **/ 
        if(gpref_head){
          WffNode *m1 = gpref_head;
          PlNode *m = pref_head;
          WffNode *i;
          char s[40];
          char s2[40];
          char s3[40];
                   
          strcpy(s3, n1->atom->item);
          if(m->connective==AND){
                  sprintf(s2,"IS-SATISFIED-%s",m1->sons->name);
                   if(same_pref_name(s2, s3) == 0){
                      found2 = 2;
		   }
		  if(same_pref_name(s2, s3) == 1){
                       found2 = 3;
                      var = TRUE;
		      sprintf(s,"IS-SATISFIED-%s",m1->sons->new_name);
                      n1->atom = new_TokenList();
                      n1->atom->item = new_Token(30);
                      strcpy(n1->atom->item, s);
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-SATISFIED-%s",i->name);
                    if( same_pref_name(s2, s3) == 0){
                        found2 = 2;
		     }
		    if( same_pref_name(s2, s3) == 1){
                      found2 = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-SATISFIED-%s",i->new_name);
                         n1->atom = new_TokenList();
                         n1->atom->item = new_Token(30);
                         strcpy(n1->atom->item, s);
		      }
		     else{      
                      ParseExpNode *node = new_ParseExpNode(AD);
                      sprintf(s,"IS-SATISFIED-%s",i->new_name);
                      node->rightson = new_ParseExpNode(nn1->connective);
                      node->rightson->atom = new_TokenList();
                      node->rightson->atom->item = (char *) calloc(30, sizeof(char));
                      strcpy(node->rightson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;

		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-SATISFIED-%s",m1->name);
                   if(same_pref_name(s2, n1->atom->item) == 0){
                     found2 = 2;
		   }
		  if(same_pref_name(s2, n1->atom->item) == 1){
                       found2 = 3;
		      sprintf(s,"IS-SATISFIED-%s",m1->new_name);
                      strcpy(n1->atom->item, s);
		   
		  }
            }

	} 
        /* end preference goal*/
        /* 2.normal constraints preference
	**/
        if(gconstraints_pref){
          WffNode *m1 = gconstraints_pref;
          PlNode *m =  pref_constraints_head;
          WffNode *i;
          char s[40];
          char s2[40];
          char s3[40];
                   
          strcpy(s3, n1->atom->item);
          if(m->connective==AND){
                  sprintf(s2,"IS-SATISFIED-%s",m1->sons->new_name);
                  if(same_pref_name(s2, s3) == 0){
                       found2 = 2;
		    }
		  if(same_pref_name(s2, s3) == 1){
                      found2 = 3;
                      var = TRUE;
		      sprintf(s,"IS-SATISFIED-%s",m1->sons->new_name);
                      n1->atom = new_TokenList();
                      n1->atom->item = new_Token(30);
                      strcpy(n1->atom->item, s);
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-SATISFIED-%s",i->new_name);
                    if( same_pref_name(s2, s3) == 0){
                         found2 = 2;
		     }
		    if( same_pref_name(s2, s3) == 1){
                      found2 = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-SATISFIED-%s",i->new_name);
                         n1->atom = new_TokenList();
                         n1->atom->item = new_Token(30);
                         strcpy(n1->atom->item, s);
		      }
		     else{      
                      ParseExpNode *node = new_ParseExpNode(AD);
                      sprintf(s,"IS-SATISFIED-%s",i->new_name);
                      node->rightson = new_ParseExpNode(nn1->connective);
                      node->rightson->atom = new_TokenList();
                      node->rightson->atom->item = (char *) calloc(30, sizeof(char));
                      strcpy(node->rightson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;

		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-SATISFIED-%s",m1->new_name);
                  if(same_pref_name(s2, n1->atom->item) == 0){
                      found2 = 2;
		    }
		  if(same_pref_name(s2, n1->atom->item) == 1){
                       found2 = 3;
		      sprintf(s,"IS-SATISFIED-%s",m1->new_name);
                      strcpy(n1->atom->item, s);
		   
		  }
            }


	}
       /*end normal constraints preference*/
        /* 3.within constraints preference
	**/
	if(gwithin_constraints_pref){
          WffNode *m1 = gwithin_constraints_pref;
          PlNode *m =  within_constraints_pref;
          WffNode *i;
          char s[40];
          char s2[40];
          char s3[40];
                   
          strcpy(s3, n1->atom->item);
          if(m->connective==AND){
                  sprintf(s2,"IS-SATISFIED-%s",m1->sons->new_name);
                   if(same_pref_name(s2, s3) == 0){
                      found2 = 2;
		   }
		  if(same_pref_name(s2, s3) == 1){
                      found2 = 3;
                      var = TRUE;
		      sprintf(s,"IS-SATISFIED-%s",m1->sons->new_name);
                      n1->atom = new_TokenList();
                      n1->atom->item = new_Token(30);
                      strcpy(n1->atom->item, s);
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-SATISFIED-%s",i->new_name);
                    if( same_pref_name(s2, s3) == 0){
                        found2 = 2;
		     }
		    if( same_pref_name(s2, s3) == 1){
                      found2 = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-SATISFIED-%s",i->new_name);
                         n1->atom = new_TokenList();
                         n1->atom->item = new_Token(30);
                         strcpy(n1->atom->item, s);
		      }
		     else{      
                      ParseExpNode *node = new_ParseExpNode(AD);
                      sprintf(s,"IS-SATISFIED-%s",i->new_name);
                      node->rightson = new_ParseExpNode(nn1->connective);
                      node->rightson->atom = new_TokenList();
                        node->rightson->atom->item = (char *) calloc(30, sizeof(char));
                      strcpy(node->rightson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;

		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-SATISFIED-%s",m1->new_name);
                  if(same_pref_name(s2, n1->atom->item) == 0){
                      found2 = 2;
		    }
		  if(same_pref_name(s2, n1->atom->item) == 1){
                       found2 = 3;
		      sprintf(s,"IS-SATISFIED-%s",m1->new_name);
                      strcpy(n1->atom->item, s);
		   
		  }
            }

	}
        /*end within constraints preference*/    
         /* 4.within constraints preference
	**/
	if(gtil_constraints_pref){
          WffNode *m1 = gtil_constraints_pref;
          PlNode *m =  til_constraints_pref;
          WffNode *i;
          char s[40];
          char s2[40];
          char s3[40];
                   
          strcpy(s3, n1->atom->item);
          if(m->connective==AND){
                  sprintf(s2,"IS-SATISFIED-%s",m1->sons->new_name);
                  if(same_pref_name(s2, s3) == 0){
                        found2 = 2;
		    }
		  if(same_pref_name(s2, s3) == 1){
                       found2 = 3;
                      var = TRUE;
		      sprintf(s,"IS-SATISFIED-%s",m1->sons->new_name);
                      n1->atom = new_TokenList();
                      n1->atom->item = new_Token(30);
                      strcpy(n1->atom->item, s);
		  }
	    if(m1->sons){
	      for(i = m1->sons->next ; i != NULL; i = i->next ){
                   sprintf(s2,"IS-SATISFIED-%s",i->new_name);
                    if( same_pref_name(s2, s3) == 0){
                       found2 = 2;
		     }
		    if( same_pref_name(s2, s3) == 1){
                      found2 = 3;
		     if(!var){
                         var = TRUE;
                         sprintf(s,"IS-SATISFIED-%s",i->new_name);
                         n1->atom = new_TokenList();
                         n1->atom->item = new_Token(30);
                         strcpy(n1->atom->item, s);
		      }
		     else{      
                      ParseExpNode *node = new_ParseExpNode(AD);
                      sprintf(s,"IS-SATISFIED-%s",i->new_name);
                      node->rightson = new_ParseExpNode(nn1->connective);
                      node->rightson->atom = new_TokenList();
                        node->rightson->atom->item = (char *) calloc(30, sizeof(char));
                      strcpy(node->rightson->atom->item, s);
                      
                      node2->leftson = node;
                      node2 = node;

		     }
		  }
	      }

	      node2->leftson = n1;

	    }
          }         
          else{
                  sprintf(s2,"IS-SATISFIED-%s",m1->new_name);
                  if(same_pref_name(s2, n1->atom->item) == 0){
                      found2 = 2;
		    }
		  if(same_pref_name(s2, n1->atom->item) == 1){
                       found2 = 3;
		      sprintf(s,"IS-SATISFIED-%s",m1->new_name);
                      strcpy(n1->atom->item, s);
		   
		  }
            }
           

	}
        /*end til constraints preference*/           
    }

  }
 }

    /* IS_VIOLATED not found at end
    **/
    if(found2 == 1){
	if(found == 1){
	    node2->found = 2;
	}
        else{
            node2->found = 3;
	}
             
    }
  

}




int same_pref_name(char *s, char *item )

{

    if((!s) || (!item))
	return 3;

    if(strcmp(s, item) == 0)
	return 0;
    else{
    if((strncmp(s, item, strlen(item) ) == 0) && (s[strlen(item)] == '_'))
	return 1;
    }

    return 2;
}






void fprint_Fact( FILE *out, Fact *f )

{

  int j;
  char *str, *str2;;

  if ( f->predicate == -3 ) {
    fprintf(out, "GOAL-REACHED");
    return;
  }

  if ( f->predicate == -1 ||
       f->predicate == -2 ) {
    printf("\n\nEQ not implemented here.\n\n");
    exit( 1 );
  }
    
  str = exchange(gpredicates[f->predicate]);

  fprintf(out, "%s", str);
  if(garity[f->predicate])
         fprintf(out, "_");
  for ( j=0; j<garity[f->predicate]; j++ ) {
    if ( f->args[j] >= 0 ) {
      str2 = exchange(gconstants[(f->args)[j]]);
      fprintf(out, "%s", str2);
    } else {
      fprintf(out, "%d", DECODE_VAR( f->args[j] ));
    }
     if ( j < garity[f->predicate] - 1 ) {
      fprintf(out, "_");
      }
  }

}



void fprint_ft_name( FILE *out, int index )

{

  fprint_Fact( out, &(grelevant_facts[index]) );

}




void fprint_LnfExpNode( FILE *out, LnfExpNode *n )

{

  int i;

  if(n->num_pF>=2){
    fprintf(out, "(+ ");
    for ( i = 0; i < n->num_pF; i++ ) {
      fprintf(out, "(* %.2f ", n->pC[i]);
      fprintf(out, "(");
      fprint_fl_name( out, n->pF[i] );
      fprintf(out, "))");
    }
    fprintf(out, ") ");
  }
  else
  {
     for ( i = 0; i < n->num_pF; i++ ) {
      fprintf(out, "(* %.2f ", n->pC[i]);
      fprintf(out, "(");
      fprint_fl_name( out, n->pF[i] );
      fprintf(out, "))");
    }
  }
 
}





void fprint_fl_name( FILE *out, int index )

{

  int i;
  char *str;
  if ( index < 0 ) {
    if ( index != -2 ) {
      fprintf(out, "[UNDEF]");
    } else {
      fprintf(out, "TOTAL-TIME");
    }
    return;
  }

  if ( grelevant_fluents_lnf[index] == NULL ) {
    /* this is a non-artificial "atomic" one
     * (or the mirrored version of one)
     */
      str = exchange(grelevant_fluents_name[index]);
    fprintf(out, "%s", str);
  } else {
    /* this only summarizes a LNF requirement
     */
   
    for ( i = 0; i < grelevant_fluents_lnf[index]->num_pF; i++ ) {
     
      fprint_fl_name( out, grelevant_fluents_lnf[index]->pF[i] );
      if ( i < grelevant_fluents_lnf[index]->num_pF - 1 ) {
	fprintf(out, " + ");
      }
    }
  }

}









void fprint_op_name( FILE *out, Action *a )

{

  int i;
    if ( !a->norm_operator &&
	 !a->pseudo_action ) {
         fprintf(out, "%s", "REACH-GOAL" );
    }
    else{
      fprintf(out, "%s", a->name );
      for ( i = 0; i < a->num_name_vars; i++ ) {
	fprintf(out, ".%s", gconstants[a->name_inst_table[i]]);
      } 
    }

}






void fprint_ExpNode( FILE *out, ExpNode *n )

{

  if ( !n ) return;

  switch ( n->connective) {
  case AD:
    fprintf(out, "(+ ");
    fprint_ExpNode(out,  n->leftson );
    fprint_ExpNode(out,  n->rightson );
    fprintf(out, ")");
    break;
  case SU:
    fprintf(out, "(- ");
    fprint_ExpNode(out,  n->leftson );
    fprint_ExpNode(out,  n->rightson );
    fprintf(out, ")");
    break;
  case MU:
    fprintf(out, "(* ");
    fprint_ExpNode(out,  n->leftson );
    fprint_ExpNode(out,  n->rightson );
    fprintf(out, ")");
    break;
  case DI:
    fprintf(out, "(/ ");
    fprint_ExpNode(out,  n->leftson );
    fprint_ExpNode(out,  n->rightson );
    fprintf(out, ")");
    break;
  case MINUS:
    fprintf(out, "(- ");
    fprint_ExpNode(out,  n->son );
    fprintf(out, ")");
    break;
  case NUMBER:
    fprintf(out, " %.2f", n->value);
    break;
  case FHEAD:
      
    if ( n->fluent ) {
      fprintf(out, " (");
      fprint_Fluent(out,  n->fluent );
      fprintf(out, ")");
    } else {
      if ( n->fl >= 0 ) {
	fprintf(out, " (");
	fprint_fl_name(out,  n->fl );
        fprintf(out, ")");
      } else {
	fprintf(out, " (TOTAL-TIME)");
      }
    }
    
    break;
  default:
    printf("\n\nprint Expnode: wrong specifier %d",
	   n->connective);
  }

}






void fprint_ExpNode2( FILE *out, ExpNode *n )

{

  if ( !n ) return;

  switch ( n->connective) {
  case AD:
    fprintf(out, "(+ ");
    fprint_ExpNode(out,  n->leftson );
    fprint_ExpNode(out,  n->rightson );
    fprintf(out, ")");
    break;
  case SU:
    fprintf(out, "(- ");
    fprint_ExpNode(out,  n->leftson );
    fprint_ExpNode(out,  n->rightson );
    fprintf(out, ")");
    break;
  case MU:
    fprintf(out, "(* ");
    fprint_ExpNode(out,  n->leftson );
    fprint_ExpNode(out,  n->rightson );
    fprintf(out, ")");
    break;
  case DI:
    fprintf(out, "(/ ");
    fprint_ExpNode(out,  n->leftson );
    fprint_ExpNode(out,  n->rightson );
    fprintf(out, ")");
    break;
  case MINUS:
    fprintf(out, "(- ");
    fprint_ExpNode(out,  n->son );
    fprintf(out, ")");
    break;
  case NUMBER:
    fprintf(out, "%.0f", n->value);
    break;
  case FHEAD:
      
    if ( n->fluent ) {
      fprint_Fluent2(out,  n->fluent );
    } else {
      if ( n->fl >= 0 ) {
	fprint_fl_name(out,  n->fl );
      } else {
	fprintf(out, " TOTAL-TIME");
      }
    }
    
    break;
  default:
    printf("\n\nprint Expnode: wrong specifier %d",
	   n->connective);
  }

}







void fprint_Fluent( FILE *out, Fluent *f )

{
  char *str, *str1 = 0, *str2;
  int j, ff = f->function;
  
 if(gfunctions[ff])
       str = exchange(gfunctions[ff]);
  else
      return;

  fprintf(out, "%s", str); 
  for ( j=0; j<gf_arity[ff]; j++ ) {
    fprintf(out, "_");
    if ( f->args[j] >= 0 ) {
      str1 = exchange(gconstants[(f->args)[j]]);
      fprintf(out, "%s", str1);
    } else {
      sprintf(str1,"%d", DECODE_VAR(f->args[j]));
      str2 = exchange(str1);
      fprintf(out, "%s",str2 );
    }
  }
  fprintf(out," ");

}



void fprint_Fluent2( FILE *out, Fluent *f )

{
  char *str, *str1 = 0, *str2 = 0;
  int j, ff = f->function;
  str = exchange(gfunctions[ff]);
  fprintf(out, "%s", str);
  for ( j=0; j<gf_arity[ff]; j++ ) {
    fprintf(out, "_");
    if ( f->args[j] >= 0 ) {
      str1 = exchange(gconstants[(f->args)[j]]);
      fprintf(out, "%s", str1);
    } else {
      sprintf(str1,"%d", DECODE_VAR(f->args[j]));
      str2 = exchange(str1);
      fprintf(out, "%s", str2);
    }
  }

}






void fprint_hidden_TokenList(FILE *out, TokenList *list, char *sep )

{

   TokenList *i_tl;
   char *str;

  i_tl = list;
  if (NULL!=i_tl) {
      if(strcmp("TOTAL-TIME", i_tl->item)){
        str = exchange(i_tl->item);
        fprintf(out, "%s", str);
      }
      else{
         fprintf(out, "%s",i_tl->item );
      }
    i_tl = i_tl->next;
  } else {
    printf("empty");
  }
  
  while (NULL != i_tl) {
    fprintf(out, "%s%s", sep, i_tl->item);
    i_tl = i_tl->next;
  }

}







void fprint_Action(FILE *ops, Action *a)

{

    int i, j;
    ActionEffect *e;

     /* now print pre, add, del
     */
    if(isDurative)
         fprintf(ops, "(:durative-action ");
    else 
         fprintf(ops, "(:action ");
    fprint_op_name( ops, a);
    fprintf(ops, "\n:parameters ()\n");

    if(isDurative){
        fprintf(ops, ":duration (= ?duration ");
        fprint_ExpNode(ops, a->action_duration);
        fprintf(ops, ")\n");
    }
 
    if(isDurative)
        fprintf(ops, ":condition\n");
    else
       fprintf(ops, ":precondition\n");
    fprintf(ops, "(and\n");
        
    /* for sync-ordinary
    **/
    if(gconstraints || gconstraints_pref || gwithin_constraints || gwithin_constraints_pref){
	if(isDurative)
	    fprintf(ops, "(at start ");
               fprintf(ops, "(sync-ordinary)");
	if(isDurative)
	    fprintf(ops, ")");
        fprintf(ops, "\n");
    }
    /* end for sync-ordinary*/    

      for ( i = 0; i < a->num_preconds; i++ ) {
 
        if(isDurative)
        {          
	  switch( a->preconds_t[i]){
          case 1:
             fprintf(ops, "(at start ");
          break;
          case 2:
            fprintf(ops, "(over all ");
          break;
          case 3:
             fprintf(ops, "(at end ");
          break;
          default:
             printf("fprint_Action!");
	  }

         }

        fprintf(ops, "(");
	fprint_ft_name( ops,a->preconds[i] );
        
         if(isDurative)
         {
           fprintf(ops, ")");
    
          }

        fprintf(ops, ")\n");

      }

     if(isTempTimeWindows){
     
      for(i = 0; i < a->norm_operator->ttw_num_preconds; i++){
        if(isDurative)
        {
	    switch( a->tw_pre_t[i]){
            case 1:
               fprintf(ops, "(at start ");
            break;
            case 2:
               fprintf(ops, "(over all ");
            break;
            case 3:
               fprintf(ops, "(at end ");
            break;
            default:
               printf("\nfprint_Action!");
	   }

         }

        fprintf(ops, "(");
	fprint_hidden_TokenList( ops,a->tw_pre[i],"-");
        

         if(isDurative)
         {
           fprintf(ops, ")");
         }

        fprintf(ops, ")\n");

      }

    }
    

    for ( i = 0; i < a->num_numeric_preconds; i++ ) {
    

    if(isDurative)
    {
        
	switch( a->numeric_preconds_lh_t[i]){
        case 1:
           fprintf(ops, "(at start ");
        break;
        case 2:
            fprintf(ops, "(over all ");
        break;
        case 3:
           fprintf(ops, "(at end ");
        break;
        default:
           printf("\nfprint_Action!");
	}
    
    }
    
    fprintf(ops, "(");
    switch ( a->numeric_preconds_comp[i] ) {
    case LE:
      fprintf(ops, "< ");
      break;
    case LEQ:
      fprintf(ops, "<= ");
      break;
    case EQ:
      fprintf(ops, "= ");
      break;
    case GEQ:
      fprintf(ops, ">= ");
      break;
    case GE:
      fprintf(ops, "> ");
      break;
    default:
      printf("\nwrong comparator of Expnodes in actionpre %d\n\n", 
	     a->numeric_preconds_comp[i]);
      exit( 1 );
    }
    

    fprint_ExpNode(ops,  a->numeric_preconds_lh[i] );
    
    fprint_ExpNode(ops,  a->numeric_preconds_rh[i] );

    if(isDurative)
    {
        fprintf(ops, ")");
    
    }
 
     fprintf(ops, ")\n");
    
  }

    fprintf(ops,")\n");
   
    fprintf(ops, ":effect\n");
    fprintf(ops, "(and\n");

    /* for sync-ordinary
    **/
    if(gconstraints || gconstraints_pref || gwithin_constraints || gwithin_constraints_pref){
	if(isDurative)
	    fprintf(ops, "(at start ");
               fprintf(ops, "(not (sync-ordinary))");
	if(isDurative)
	    fprintf(ops, ")");
        fprintf(ops, "\n");
    }
    /* end for sync-ordinary*/  
    /* for first sync-automaton
    **/
    if(gconstraints || gconstraints_pref || gwithin_constraints || gwithin_constraints_pref){
       
	if(isDurative)
	    fprintf(ops, "(at start ");
               fprintf(ops, "(sync-automaton-%s)", automata_name);
	if(isDurative)
	    fprintf(ops, ")");
        fprintf(ops, "\n");
    }
    /* end for first sync-automaton*/  
  

    /* change preference preconds in effect 
    **/
    if(isDomainPreference){  
    if(a->num_pref_preconds > 0){
	int i;
        if(isDurative)
        {
              fprintf(ops, "(at end ");
        }
        fprintf(ops, "(when (not (and ");
	for (i = 0; i < a->num_pref_preconds; i++ ){
            fprintf(ops, "(");
            fprint_Fact(ops,&(grelevant_facts[a->pref_preconds[i]]));
            fprintf(ops, ") ");
	}
          fprintf(ops, "))\n");
          fprintf(ops, "(increase ");
          fprintf(ops, "( IS_VIOLATED_%s", a->norm_operator->operator->pref_preconds->name);
          fprintf(ops, ")  %i", 1);

         if(isDurative)
            {
               fprintf(ops, ")");
            }
         fprintf(ops, "))\n");

    }
  }
    




    for ( j = 0; j < a->num_effects; j++ ) {
    
    e = &(a->effects[j]);
  
    if ( e->illegal ) printf(" ILLEGAL EFFECT!");
   
   
    for ( i = 0; i < e->num_adds; i++ ) {
     

       if(isDurative)
         {
          
  	   switch( e->adds_t[i]){
           case 1:
              fprintf(ops, "(at start ");
           break;
           case 2:
              fprintf(ops, "(over all ");
           break;
           case 3:
              fprintf(ops, "(at end ");
           break;
           default:
              printf("\nfprint_Action!");
	   }
           
          }
      fprintf(ops, "(");
      fprint_ft_name( ops, e->adds[i] );
     
       if(isDurative)
       {
        fprintf(ops, ")");
    
        }

        fprintf(ops, ")\n");
      
    }
   
    for ( i = 0; i < e->num_dels; i++ ) {

      if(isDurative)
         {
           
  	   switch( e->dels_t[i]){
           case 1:
              fprintf(ops, "(at start ");
           break;
           case 2:
              fprintf(ops, "(over all ");
           break;
           case 3:
              fprintf(ops, "(at end ");
           break;
           default:
              printf("\nfprint_Action!");
	   }
         
          }

      fprintf(ops, "(not (");
      fprint_ft_name(ops,  e->dels[i] );
      
       if(isDurative)
       {
        fprintf(ops, ")");
    
        }

      fprintf(ops, "))\n"); 

    }

    
    for ( i = 0; i < e->num_numeric_effects; i++ ) {
	
         if(isDurative)
         {
           
  	   switch(e->numeric_effects_fl_t[i] ){
           case 1:
              fprintf(ops, "(at start ");
           break;
           case 2:
              fprintf(ops, "(over all ");
           break;
           case 3:
              fprintf(ops, "(at end ");
           break;
           default:
              printf("\nfprint_Action!");
	   }
         
          }

      fprintf(ops, "(");
      switch ( e->numeric_effects_neft[i] ) {
      case ASSIGN:
	fprintf(ops, "assign ");
	break;
      case SCALE_UP:
	fprintf(ops, "scale-up ");
	break;
      case SCALE_DOWN:
	fprintf(ops, "scale-down ");
	break;
      case INCREASE:
	fprintf(ops, "increase ");
	break;
      case DECREASE:
	fprintf(ops, "decrease ");
	break;
      default:
	printf("\n\nprint normop: illegal neft %d\n\n", 
	       e->numeric_effects_neft[i]);
	exit( 1 );
      }

      fprintf(ops, "(");
      if ( e->numeric_effects_fl[i] >= 0 ) {

	fprint_fl_name( ops, e->numeric_effects_fl[i] );
      } else {
	fprintf(ops, "[UNDEF]");
      }
      fprintf(ops, ")");

      fprint_ExpNode( ops, e->numeric_effects_rh[i] );

      if(isDurative)
       {
        fprintf(ops, ")");
    
        }
      
        fprintf(ops, ") \n");  
     
    }

    /* for preference goal penalty
    **/
    if(isPreference || gpref_head){
        WffNode *current1 = gpref_head;
        WffNode *i_son;
        WffNode *current2 = NULL;
        int all = 0;
        float value = 0.00;
        int l = 0;

        
     if ( current1->sons ) {
        current2 = current1->sons;
       /*first preference
       **/
       all = is_effect_meet_preference(e,&value, current2, &l);
       switch (current2->connective){
       case ATOM:
       case NOT:
            if(all == 2){
                       if(isDurative)
                       {
                          fprintf(ops, "(at end ");
                       }
                   
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", current2->name);
                       fprintf(ops, ")  %i", 1);
                       if(isDurative)
                       {
                         fprintf(ops, ")");
                       }
                       fprintf(ops, ")\n");

                       if(isDurative)
                       {
                          fprintf(ops, "(at end ");
                       }
                   
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", current2->name);
                       fprintf(ops, ")  %i", 0);
                       if(isDurative)
                       {
                         fprintf(ops, ")");
                       }
                       fprintf(ops, ")\n");
	            }
              if(all == 1){
                       if(isDurative)
                       {
                          fprintf(ops, "(at end ");
                       }
                   
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", current2->name);
                       fprintf(ops, ")  %i", 0);
                       if(isDurative)
                       {
                         fprintf(ops, ")");
                       }
                       fprintf(ops, ")\n");

                        if(isDurative)
                       {
                          fprintf(ops, "(at end ");
                       }
                   
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", current2->name);
                       fprintf(ops, ")  %i", 1);
                       if(isDurative)
                       {
                         fprintf(ops, ")");
                       }
                       fprintf(ops, ")\n");
	            }
         break;
         case TRU:
         case FAL:
	 break;
         case AND:    
            /* (and fac1 fact2 fact3 ...) preference
	       * one or some fact in dels
	      **/
            if(all == 2){
                       if(isDurative)
                       {
                          fprintf(ops, "(at end ");
                       }
                       
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", current2->name);
                       fprintf(ops, ")  %i", 1);
                       if(isDurative)
                       {
                         fprintf(ops, ")");
                       }
                       fprintf(ops, ")\n");

                        if(isDurative)
                       {
                          fprintf(ops, "(at end ");
                       }
                       
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", current2->name);
                       fprintf(ops, ")  %i", 0);
                       if(isDurative)
                       {
                         fprintf(ops, ")");
                       }
                       fprintf(ops, ")\n");
	          }
              /* (and fac1 fact2 fact3 ...) preference
	       * all facts in adds
	      **/
              if(all == 1){
                       if(isDurative)
                       {
                          fprintf(ops, "(at end ");
                       }
                       
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", current2->name);
                       fprintf(ops, ")  %i", 0);
                       if(isDurative)
                       {
                         fprintf(ops, ")");
                       }
                       fprintf(ops, ")\n");


                       if(isDurative)
                       {
                          fprintf(ops, "(at end ");
                       }
                       
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", current2->name);
                       fprintf(ops, ")  %i", 1);
                       if(isDurative)
                       {
                         fprintf(ops, ")");
                       }
                       fprintf(ops, ")\n");
	          }
              /* (and fac1 fact2 fact3 ...) preference
	       * no fact in dels
	       * some fact in adds, but not all
	       * some fact not in adds and not in dels
	      **/
              if(all == 3){
                       if(isDurative)
                       {
                          fprintf(ops, "(at end ");
                       }
                       
                       fprintf(ops, "(when ");
                       /* when l > 1, then node have more than one sons
		        * else without (and)
		       **/
                       if(l > 1)
                           fprintf(ops, "(and ");
                       fprint_Wff_for_and_node(ops, e, current2);
                       if(l > 1)
			   fprintf(ops, ")");
                       fprintf(ops, "(and (assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", current2->name);
                       fprintf(ops, ")  %i)", 0);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", current2->name);
                       fprintf(ops, ")  %i))", 1);

                       if(isDurative)
                       {
                         fprintf(ops, ")");
                       }
                       fprintf(ops, ")\n");
	           
	          }
              /* (and exp1 exp2 exp3 ...) preference
	       **/
              if(all == 4){
                       if(isDurative)
                       {
                          fprintf(ops, "(at end ");
                       }
                       
                       fprintf(ops, "(when ");
                       fprintf(ops, "(and ");
                       fprint_Wff_for_and_expnode(ops, e, current2,&value, TRUE);
		       fprintf(ops, ")");
                       fprintf(ops, "(and (assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", current2->name);
                       fprintf(ops, ")  %i)", 1);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", current2->name);
                       fprintf(ops, ")  %i))", 0);

                       if(isDurative)
                       {
                         fprintf(ops, ")");
                       }
                       fprintf(ops, ")\n");
                   

                        if(isDurative)
                       {
                          fprintf(ops, "(at end ");
                       }
                       
                       fprintf(ops, "(when ");
                       fprintf(ops, "(and ");
                       fprint_Wff_for_and_expnode(ops, e, current2,&value, FALSE);
		       fprintf(ops, ")");
                       fprintf(ops, "(and (assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", current2->name);
                       fprintf(ops, ")  %i)", 0);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", current2->name);
                       fprintf(ops, ")  %i))", 0);
           
           
                       if(isDurative)
                       {
                         fprintf(ops, ")");
                       }
                       fprintf(ops, ")\n");
		       
	        }
       break;  
       case OR:
            /* (or fac1 fact2 fact3 ...) preference
	       * all facts in dels
	      **/
            if(all == 2){
                       if(isDurative)
                       {
                          fprintf(ops, "(at end ");
                       }
                       
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", current2->name);
                       fprintf(ops, ")  %i", 1);

                       if(isDurative)
                       {
                         fprintf(ops, ")");
                       }
                       fprintf(ops, ")\n");

                       if(isDurative)
                       {
                          fprintf(ops, "(at end ");
                       }
                       
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", current2->name);
                       fprintf(ops, ")  %i", 0);

                       if(isDurative)
                       {
                         fprintf(ops, ")");
                       }
                       fprintf(ops, ")\n");
	          }
              /* (or fac1 fact2 fact3 ...) preference
	       * one or some fact in adds
	      **/
              if(all == 1){
                       if(isDurative)
                       {
                          fprintf(ops, "(at end ");
                       }
                       
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", current2->name);
                       fprintf(ops, ")  %i", 0);
                       if(isDurative)
                       {
                         fprintf(ops, ")");
                       }
                       fprintf(ops, ")\n");

                        if(isDurative)
                       {
                          fprintf(ops, "(at end ");
                       }
                       
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", current2->name);
                       fprintf(ops, ")  %i", 1);
                       if(isDurative)
                       {
                         fprintf(ops, ")");
                       }
                       fprintf(ops, ")\n");
	        }
               /* (or fac1 fact2 fact3 ...) preference
	       * no fact in adds
	       * some fact in dels, but not all
	       * some fact not in dels and not in adds
	       **/
               if(all == 3){
                       if(isDurative)
                       {
                          fprintf(ops, "(at end ");
                       }
                       
                       fprintf(ops, "(when (not  ");
                       /* when l > 1, then node have more than one sons
		        * else without (or)
		       **/
                       if(l > 1)
                           fprintf(ops, "(or ");
                       fprint_Wff_for_or_node(ops, e, current2);
                       if(l > 1)
			   fprintf(ops, ")");
                       fprintf(ops, ")");
                       fprintf(ops, "(and (assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", current2->name);
                       fprintf(ops, ")  %i)", 1);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", current2->name);
                       fprintf(ops, ")  %i))", 0);


                       if(isDurative)
                       {
                         fprintf(ops, ")");
                       }
                       fprintf(ops, ")\n");
	          }
               /* (or exp1 exp2 exp3 ...) preference
	       **/
               if(all == 4){
                       if(isDurative)
                       {
                          fprintf(ops, "(at end ");
                       }
                       
                       fprintf(ops, "(when ");
                       fprintf(ops, "(or ");
                       fprint_Wff_for_or_expnode(ops, e, current2,&value, TRUE);
		       fprintf(ops, ")");
                       fprintf(ops, "(and (assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", current2->name);
                       fprintf(ops, ")  %i)", 1);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", current2->name);
                       fprintf(ops, ")  %i))", 0);

                       if(isDurative)
                       {
                         fprintf(ops, ")");
                       }
                       fprintf(ops, ")\n");

		       
                       if(isDurative)
                       {
                          fprintf(ops, "(at end ");
                       }
                       
                       fprintf(ops, "(when ");
                       fprintf(ops, "(or ");
                       fprint_Wff_for_or_expnode(ops, e, current2,&value, FALSE);
		       fprintf(ops, ")");
                       fprintf(ops, "(and (assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", current2->name);
                       fprintf(ops, ")  %i)", 0);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", current2->name);
                       fprintf(ops, ")  %i))", 1);

                       if(isDurative)
                       {
                         fprintf(ops, ")");
                       }
                       fprintf(ops, ")\n");
		       
	           
	          }
       break;
       case COMP:
            if(all == 4){
                       if(isDurative)
                       {
                          fprintf(ops, "(at end ");
                       }
                       fprintf(ops, "(when  ");
                       fprint_Wff_before_action_application1(ops, current2, value);     
                       fprintf(ops, "(and (assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", current2->name);
                       fprintf(ops, ")  %i)", 1);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", current2->name);
                       fprintf(ops, ")  %i))", 0);


                       if(isDurative)
                       {
                         fprintf(ops, ")");
                       }
                       fprintf(ops, ")\n");
                        if(isDurative)
                       {
                          fprintf(ops, "(at end ");
                       }
                       fprintf(ops, "(when  ");
                       fprint_Wff_before_action_application2(ops, current2, value);     
                       fprintf(ops, "(and (assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", current2->name);
                       fprintf(ops, ")  %i)", 0);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", current2->name);
                       fprintf(ops, ")  %i))", 0);

                       if(isDurative)
                       {
                         fprintf(ops, ")");
                       }
                       fprintf(ops, ")\n");

	            }

                if(all == 5){
                       if(isDurative)
                       {
                          fprintf(ops, "(at end ");
                       }
                   
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", current2->name);
                       if(found_violated_or_not(current2, value))
                           fprintf(ops, ")  %i", 0);
                       else
                           fprintf(ops, ")  %i", 1);

                       if(isDurative)
                       {
                         fprintf(ops, ")");
                       }
                       fprintf(ops, ")\n");


                        if(isDurative)
                       {
                          fprintf(ops, "(at end ");
                       }
                   
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", current2->name);
                       if(found_violated_or_not(current2, value))
                           fprintf(ops, ")  %i", 1);
                       else
                           fprintf(ops, ")  %i", 0);

                       if(isDurative)
                       {
                         fprintf(ops, ")");
                       }
                       fprintf(ops, ")\n");

	            }
       break;
       default:
          printf("\n***** ERROR ****");
          printf("\nWrong Node specifier <goal penalty1: %d> in fprint_Action!\n", current2->connective);
          exit(1); 
 
	 }

          /*+++++++++++++++*/


      for ( i_son = current2->next; i_son!=NULL; i_son = i_son->next ) {

	  /*second one preference and so on
	  **/
	  value = 0.00;
          l = 0;

          all = is_effect_meet_preference(e, &value,i_son, &l);
	   switch (i_son->connective){
	   case ATOM:
           case NOT:
		 if(all == 2){
                       if(isDurative)
                       {
                          fprintf(ops, "(at end ");
                       }
                   
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", i_son->name);
                       fprintf(ops, ")  %i", 1);
          
                       if(isDurative)
                       {
                          fprintf(ops, ")");
    
                       }
                       fprintf(ops, ")\n");


                        if(isDurative)
                       {
                          fprintf(ops, "(at end ");
                       }
                   
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", i_son->name);
                       fprintf(ops, ")  %i", 0);
          
                       if(isDurative)
                       {
                          fprintf(ops, ")");
    
                       }
                       fprintf(ops, ")\n");
	           } 
                 if(all == 1){
                       if(isDurative)
                       {
                          fprintf(ops, "(at end ");
                       }
                   
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", i_son->name);
                       fprintf(ops, ")  %i", 0);

                       if(isDurative)
                       {
                          fprintf(ops, ")");
    
                       }
                       fprintf(ops, ")\n");


                        if(isDurative)
                       {
                          fprintf(ops, "(at end ");
                       }
                   
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", i_son->name);
                       fprintf(ops, ")  %i", 1);

                       if(isDurative)
                       {
                          fprintf(ops, ")");
    
                       }
                       fprintf(ops, ")\n");
	           } 
	  break;  
          case TRU:
          case FAL:
	  break;
          case AND:    
            /* (and fac1 fact2 fact3 ...) preference
	       * one or some fact in dels
	      **/
            if(all == 2){
                       if(isDurative)
                       {
                          fprintf(ops, "(at end ");
                       }
                       
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", i_son->name);
                       fprintf(ops, ")  %i", 1);
                     
                       if(isDurative)
                       {
                         fprintf(ops, ")");
                       }
                       fprintf(ops, ")\n");


                        if(isDurative)
                       {
                          fprintf(ops, "(at end ");
                       }
                       
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", i_son->name);
                       fprintf(ops, ")  %i", 0);
                     
                       if(isDurative)
                       {
                         fprintf(ops, ")");
                       }
                       fprintf(ops, ")\n");
	          }
              /* (and fac1 fact2 fact3 ...) preference
	       * all facts in adds
	      **/
              if(all == 1){
                       if(isDurative)
                       {
                          fprintf(ops, "(at end ");
                       }
                       
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", i_son->name);
                       fprintf(ops, ")  %i", 0);

                       if(isDurative)
                       {
                         fprintf(ops, ")");
                       }
                       fprintf(ops, ")\n");

                       
                       if(isDurative)
                       {
                          fprintf(ops, "(at end ");
                       }
                       
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", i_son->name);
                       fprintf(ops, ")  %i", 1);

                       if(isDurative)
                       {
                         fprintf(ops, ")");
                       }
                       fprintf(ops, ")\n");
	          }
              /* (and fac1 fact2 fact3 ...) preference
	       * no fact in dels
	       * some fact in adds, but not all
	       * some fact not in adds and not in dels
	      **/
              if(all == 3){
                       if(isDurative)
                       {
                          fprintf(ops, "(at end ");
                       }
                       
                       fprintf(ops, "(when ");
                       /* when l > 1, then node have more than one sons
		        * else without (and)
		       **/
                       if(l > 1)
                           fprintf(ops, "(and ");
                       fprint_Wff_for_and_node(ops, e, i_son);
                       if(l > 1)
			   fprintf(ops, ")");
                       fprintf(ops, "(and (assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", i_son->name);
                       fprintf(ops, ")  %i)", 0);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", i_son->name);
                       fprintf(ops, ")  %i))", 1);

                       if(isDurative)
                       {
                         fprintf(ops, ")");
                       }
                       fprintf(ops, ")\n");
	          }
                  /* (and exp1 exp2 exp3 ...) preference
	          **/
                  if(all == 4){
                       if(isDurative)
                       {
                          fprintf(ops, "(at end ");
                       }
                       
                       fprintf(ops, "(when ");
                       fprintf(ops, "(and ");
                       fprint_Wff_for_and_expnode(ops, e, i_son, &value, TRUE);
		       fprintf(ops, ")");
                       fprintf(ops, "(and (assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", i_son->name);
                       fprintf(ops, ")  %i)", 1);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", i_son->name);
                       fprintf(ops, ")  %i))", 0);

                       if(isDurative)
                       {
                         fprintf(ops, ")");
                       }
                       fprintf(ops, ")\n");

                       
                       if(isDurative)
                       {
                          fprintf(ops, "(at end ");
                       }
                       
                       fprintf(ops, "(when ");
                       fprintf(ops, "(and ");
                       fprint_Wff_for_and_expnode(ops, e, i_son, &value, FALSE);
		       fprintf(ops, ")");
                       fprintf(ops, "(and (assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", i_son->name);
                       fprintf(ops, ")  %i)", 0);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", i_son->name);
                       fprintf(ops, ")  %i))", 1);

                       if(isDurative)
                       {
                         fprintf(ops, ")");
                       }
                       fprintf(ops, ")\n");
		       
	           
	          }
         break;       
	 case OR:
                 /* (or fac1 fact2 fact3 ...) preference
	         * all facts in dels
	         **/
		 if(all == 2){
                       if(isDurative)
                       {
                          fprintf(ops, "(at end ");
                       }
                  
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", i_son->name);
                       fprintf(ops, ")  %i", 1);

                       if(isDurative)
                       {
                          fprintf(ops, ")");
    
                       }
                       fprintf(ops, ")\n");


                      if(isDurative)
                       {
                          fprintf(ops, "(at end ");
                       }
                  
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", i_son->name);
                       fprintf(ops, ")  %i", 0);

                       if(isDurative)
                       {
                          fprintf(ops, ")");
    
                       }
                       fprintf(ops, ")\n");
		 }
                /* (or fac1 fact2 fact3 ...) preference
	        * one or some fact in adds
	        **/
                if(all == 1){
                       if(isDurative)
                       {
                          fprintf(ops, "(at end ");
                       }
                  
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", i_son->name);
                       fprintf(ops, ")  %i", 0);

                       if(isDurative)
                       {
                          fprintf(ops, ")");
    
                       }
                       fprintf(ops, ")\n");


                        if(isDurative)
                       {
                          fprintf(ops, "(at end ");
                       }
                  
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", i_son->name);
                       fprintf(ops, ")  %i", 1);

                       if(isDurative)
                       {
                          fprintf(ops, ")");
    
                       }
                       fprintf(ops, ")\n");
		 }
               /* (or fac1 fact2 fact3 ...) preference
	       * no fact in adds
	       * some fact in dels, but not all
	       * some fact not in dels and not in adds
	       **/
               if(all == 3){
                       if(isDurative)
                       {
                          fprintf(ops, "(at end ");
                       }
                  
                       fprintf(ops, "(when (not  ");
                       /* when l > 1, then node have more than one sons
		       * else without (or)
		       **/
                       if(l > 1)
			   fprintf(ops, "(or ");
                       fprint_Wff_for_or_node(ops, e, i_son);
                       if(l > 1)
			   fprintf(ops, ")");
                       fprintf(ops, ")"); 
                       fprintf(ops, "(and (assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", i_son->name);
                       fprintf(ops, ")  %i)", 1);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", i_son->name);
                       fprintf(ops, ")  %i))", 0);

                       if(isDurative)
                       {
                          fprintf(ops, ")");
    
                       }
                       fprintf(ops, ")\n");
		   }
                  /* (or exp1 exp2 exp3 ...) preference
	          **/
                  if(all == 4){
                       if(isDurative)
                       {
                          fprintf(ops, "(at end ");
                       }
                       
                       fprintf(ops, "(when ");
                       fprintf(ops, "(or ");
                       fprint_Wff_for_or_expnode(ops, e, i_son, &value, TRUE);
		       fprintf(ops, ")");
                       fprintf(ops, "(and (assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", i_son->name);
                       fprintf(ops, ")  %i)", 1);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", i_son->name);
                       fprintf(ops, ")  %i))", 0);

                       if(isDurative)
                       {
                         fprintf(ops, ")");
                       }
                       fprintf(ops, ")\n");

                       
                        if(isDurative)
                       {
                          fprintf(ops, "(at end ");
                       }
                       
                       fprintf(ops, "(when ");
                       fprintf(ops, "(or ");
                       fprint_Wff_for_or_expnode(ops, e, i_son, &value, FALSE);
		       fprintf(ops, ")");
                       fprintf(ops, "(and (assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", i_son->name);
                       fprintf(ops, ")  %i)", 0);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", i_son->name);
                       fprintf(ops, ")  %i))", 1);

                       if(isDurative)
                       {
                         fprintf(ops, ")");
                       }
                       fprintf(ops, ")\n");
		       
	           
	          }
         break;
         case COMP:
		 if(all == 4){
                       if(isDurative)
                       {
                          fprintf(ops, "(at end ");
                       }
                   
                       fprintf(ops, "(when  ");
                       fprint_Wff_before_action_application1(ops, i_son, value); 
                       fprintf(ops, "(and (assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", i_son->name);
                       fprintf(ops, ")  %i)", 1);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", i_son->name);
                       fprintf(ops, ")  %i))", 0);

                       if(isDurative)
                       {
                          fprintf(ops, ")");
    
                       }
                       fprintf(ops, ")\n");

                       if(isDurative)
                       {
                          fprintf(ops, "(at end ");
                       }
                   
                       fprintf(ops, "(when  ");
                       fprint_Wff_before_action_application2(ops, i_son, value); 
                       fprintf(ops, "(and (assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", i_son->name);
                       fprintf(ops, ")  %i)", 0);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", i_son->name);
                       fprintf(ops, ")  %i))", 1);

                       if(isDurative)
                       {
                          fprintf(ops, ")");
    
                       }
                       fprintf(ops, ")\n");
	           } 

                 if(all == 5){
                       if(isDurative)
                       {
                          fprintf(ops, "(at end ");
                       }
                       
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", i_son->name);
                       if(found_violated_or_not(i_son, value))
                           fprintf(ops, ")  %i", 0);
                       else
                           fprintf(ops, ")  %i", 1);

                       if(isDurative)
                       {
                         fprintf(ops, ")");
                       }
                       fprintf(ops, ")\n");

                
                       if(isDurative)
                       {
                          fprintf(ops, "(at end ");
                       }
                       
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", i_son->name);
                       if(found_violated_or_not(i_son, value))
                           fprintf(ops, ")  %i", 1);
                       else
                           fprintf(ops, ")  %i", 0);

                       if(isDurative)
                       {
                         fprintf(ops, ")");
                       }
                       fprintf(ops, ")\n");

	            }
         break;
         default:
            printf("\n***** ERROR ****");
            printf("\nWrong Node specifier <goal penalty2: %d> in fprint_Action!\n", i_son->connective);
            exit(1);
      }
     } 
    }
   }
 
    fprintf(ops, ")\n");
    fprintf(ops, ")\n");    

}

}



Bool found_violated_or_not(WffNode *n, float value )

{
    Bool right = FALSE;
    
    switch (n->comp){
     case LE:
       if(value < 0)
	  right = TRUE;
      break;
     case EQ:
       if(value == 0)
	  right = TRUE;
      break;
    case LEQ:
       if(value <= 0)
	  right = TRUE;
      break;
    case GEQ:
      if(value >= 0)
	  right = TRUE;
      break;
    case GE:
       if(value > 0)
	  right = TRUE;
      break;
    default:
      printf("\nwrong comparator of Expnodes in WFF %d\n\n", n->comp);
      exit( 1 );
    }
 
  return right;

}



void fprint_Wff(FILE *out, WffNode *n )

{

  WffNode *i;

  if ( !n ) {
    printf("none\n");
    return;
  }
  
 
  switch (n->connective) {
  case ALL: 
   fprintf(out, "ALL x%d (%s): %s\n", n->var, n->var_name,
	    gtype_names[n->var_type]);
    fprintf(out, "(   ");
    fprint_Wff(out, n->son);
    fprintf(out, ")\n");
    break;
  case EX:
    fprintf(out, "EX  x%d (%s) : %s\n",  n->var, n->var_name,
	    gtype_names[n->var_type]);
    fprintf(out, "(   ");
    fprint_Wff(out, n->son);
    fprintf(out, ")\n");
    break;
  case AND: 
    fprintf(out, " (and ");
    fprint_Wff(out, n->sons);
    if ( n->sons ) {
      for ( i = n->sons->next; i!=NULL; i = i->next ) {
	if ( !i->prev ) {
	  printf("\nprev in AND not correctly set!\n\n");
	  exit( 1 );
	}
	fprint_Wff(out, i);
      }
     fprintf(out, ")\n");
    }      
    break;
  case OR:
    fprintf(out, " (or  ");
    fprint_Wff(out, n->sons);
    for ( i = n->sons->next; i!=NULL; i = i->next ) {
      fprint_Wff(out, i);
    }
    fprintf(out, ")\n");
    break;
  case NOT:
    if (ATOM==n->son->connective) {
      fprintf(out, "(NOT  ");
      fprint_Wff(out, n->son);
      fprintf(out, ")\n");
    } else {
      fprintf(out, "(NOT ");
      fprint_Wff(out, n->son);
      fprintf(out, ")\n");
    }
    break;
  case ATOM:
    fprintf(out, "(");
    fprint_Fact(out, n->fact);
    if ( n->NOT_p != -1 ) printf(" - translation NOT");
    fprintf(out, ")");
    break;
  case TRU:
     fprintf(out, "(TRUE)\n");
     break;
  case FAL:
     fprintf(out, "(FALSE)\n");
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
    fprint_ExpNode(out, n->lh );
    fprint_ExpNode(out, n->rh );
    fprintf(out, ")\n");
    break;
    case TEMPORALOP:
      switch (n->temporal_op) {
      case ALWAYS:
        fprintf(out, "(always ");
        fprint_Wff( out, n->son);
      break;
      case SOMETIME:
        fprintf(out, "(sometime ");
        fprint_Wff(out, n->son);
      break;
      case AEND:
        fprintf(out, "(at end ");
        fprint_Wff( out, n->son);
      break;
      case WITHIN:
        fprintf(out, "(within ");
        fprint_ExpNode(out, n->lh );
        fprintf(out, " ");
        fprint_Wff( out, n->son);
      break;
      case AT_MOST_ONCE:
        fprintf(out, "(at-most-once ");
        fprint_Wff(out, n->son);
      break;
      case SOMETIME_AFTER:
      fprintf(out, "(sometime-after ");
      fprint_Wff(out, n->son);
      fprint_Wff(out, n->son->son);
      break;
      case SOMETIME_BEFORE:
        fprintf(out, "(sometime-before ");
        fprint_Wff(out, n->son);
        fprint_Wff(out, n->son->son);
      break;
      case ALWAYS_WITHIN:
        fprintf(out, "(always-within ");
        fprint_ExpNode(out, n->lh );
        fprintf(out, " ");
        fprint_Wff(out, n->son);
        fprint_Wff(out, n->son->son);
      break;
      case HOLD_DURING:
        fprintf(out, "(hold-during ");
        fprint_ExpNode(out, n->lh );
        fprintf(out, " ");
        fprint_ExpNode(out, n->rh );
        fprintf(out, " ");
        fprint_Wff(out, n->son);
      break;
      case HOLD_AFTER:
        fprintf(out, "(hold-after ");
        fprint_ExpNode(out, n->lh );
        fprintf(out, " ");
        fprint_Wff(out, n->son);
      break;
      default:
        printf("\n\nillegal temporal operator in parse tree %d!\n\n", n->temporal_op);
        exit( 1 );
      }
      printf(")\n");
      break;
  default:
    printf("\n***** ERROR ****");
    printf("\nprint_Wff: %d > Wrong Node specifier\n", n->connective);
    exit(1);
  }     

} 








void  generate_gpref_predicate( WffNode *n)
 
{

  WffNode *i;

  if ( !n ) {
    printf("none\n");
    return;
  }  


  switch (n->connective) {
  case AND: 
    generate_gpref_predicate( n->sons);
    if ( n->sons ) {
      for ( i = n->sons->next; i!=NULL; i = i->next ) {
	if ( !i->prev ) {
	  printf("\nprev in AND not correctly set!\n\n");
	  exit( 1 );
	}
         
	generate_gpref_predicate( i);
      }
    }     
    break;
  case OR:
    generate_gpref_predicate(n->sons);
    for ( i = n->sons->next; i!=NULL; i = i->next ) {
      generate_gpref_predicate(i);
    }
    break;
  case NOT:
      generate_gpref_predicate( n->son);
    break;
  case ATOM:
      generate_Fact_for_gpref_predicate( n->fact);
    break;
  case TRU:
     break;
  case FAL:
     break;
  case TEMPORALOP:
        generate_gpref_predicate( n->son);
         if((n->temporal_op==SOMETIME_AFTER) || (n->temporal_op==SOMETIME_BEFORE) || (n->temporal_op==ALWAYS_WITHIN))
       generate_gpref_predicate( n->son->next);
    break;
  case COMP:
    break;
  default:
    printf("\n***** ERROR ****");
    printf("\ngenerate_gpref: %d > Wrong Node specifier\n", n->connective);
    exit(1);
  }     

} 



void generate_Fact_for_gpref_predicate(  Fact *f)

{
    int i, k;
    Bool found = FALSE;
    /*test, if fact is relevant predicate
    **/
      for ( i = 0; i < gnum_relevant_facts; i++ ) {
        Fact *rf = &(grelevant_facts[i]);
            if(same_Fact(f,rf))
		      return;
      }     
    

    /* add preference fact to relevant predicates
    **/
      for (k = 0; k < gnum_gpref_predicate; k++){
	  if(same_Fact(relevant_gpref_predicate[k], f)){
	      found = TRUE;
              break;
	  }
      }
       if(!found)
       relevant_gpref_predicate[gnum_gpref_predicate++] = f;
        
}







TokenList *int_to_TokenList(int index)

{
  int j;
  TokenList *head, *neu_element, *current;
   Fact *f = &(grelevant_facts[index]);
   head = NULL;
   neu_element = ( TokenList * ) calloc( 1, sizeof(TokenList ) );
   neu_element->item = gpredicates[f->predicate];
   neu_element->next = head;
   head = neu_element;
   current = head;

    for ( j=0; j<garity[f->predicate]; j++ ) {
   
          while(current->next != NULL)
		current = current->next;

        neu_element = ( TokenList * ) calloc( 1, sizeof(TokenList ) );
        neu_element->item = gconstants[(f->args)[j]];
        current->next = neu_element;
        neu_element->next = NULL;

  
  }

    return head;

}




Bool isWithinAutomaton(char *s)

{

    char s1[4] = "SKIP"; 
    char s2[4] = "SYNC"; 

    if(!s){
	printf("\n empty str !\n");
        return FALSE;
    }

    if(strstr(s, "WITHIN") && strstr(s, "ACCEPT"))
	return FALSE;

/*    printf("\n str2 %s !\n",s); */

    if(!(strncmp(s, s1, 4)) || (!strncmp(s, s2, 4)))
	return TRUE;

/*    printf("\n str3 %s !\n",s); */

    return FALSE;

}

Bool isAutomaton(char *s)

{

    char s1[] = "SKIP"; 
    char s2[] = "SYNC"; 
    char s3[] = "REACH"; 

    if(!s){
	printf("\n empty str !\n");
        return FALSE;
    }

    if(!(strncmp(s, s3, 5)))
	return TRUE;

    if(!(strncmp(s, s1, 4)) || (!strncmp(s, s2, 4)))
	return TRUE;

    return FALSE;

}






Bool fl_isSame_to_isViolated( int index )

{


  if ( index < 0 ) {
    if ( index != -2 ) {
	return FALSE;
    } else {
        return FALSE;
    }
 
  }

  if ( grelevant_fluents_lnf[index] == NULL ) {
       
      if((!strncmp( "is-violated", grelevant_fluents_name[index], 11)) || 
	 (!strncmp( "IS_VIOLATED", grelevant_fluents_name[index], 11)))
	  return TRUE;
  } 

  return FALSE;

}








Bool same_Fact(Fact *fact1, Fact *fact2)

{
    int i ;

    if((!fact1) || (!fact2)){
	return FALSE;
    }
    
    if(fact1->predicate != fact2->predicate){
	return FALSE;
    }
    
    for(i = 0; i < MAX_ARITY; i++){
	if(fact1->args[i] != fact2->args[i]){
	    return FALSE;
	}
    }

    return TRUE;

}



Bool same_Fluent(Fluent *fl1, Fluent *fl2)

{

    int i ;
    if(fl1->function != fl2->function){
	return FALSE;
    }
    
    for(i = 0; i < MAX_ARITY; i++){
	if(fl1->args[i] != fl2->args[i]){
	    return FALSE;
	}
    }

    return TRUE;

}



void fprint_Wff_before_action_application1(FILE *out, WffNode *n, float value)

{

   if(n->comp == EQ){
       fprintf(out, "(or  ");
       fprintf(out, "(> ");
       fprint_ExpNode(out, n->lh );
       fprintf(out, " %.2f", n->rh->value + value);
       fprintf(out, ")\n");
       fprintf(out, "(< ");
       fprint_ExpNode(out, n->lh );
       fprintf(out, " %.2f", n->rh->value + value);
       fprintf(out, "))\n");

    }
   else{
    switch(n->connective){
    case COMP:
      switch (n->comp) {
      case LE:
        fprintf(out, "(>= ");
        break;
      case LEQ:
        fprintf(out, "(> ");
        break;
      case GEQ:
        fprintf(out, "(< ");
        break;
      case GE:
        fprintf(out, "(<= ");
        break;
      default:
         printf("\nwrong comparator of Expnodes in WFF %d\n\n", n->comp);
         exit( 1 );
      }
    fprint_ExpNode(out, n->lh );
    fprintf(out, " %.2f", n->rh->value + value);
    fprintf(out, ")\n");
    break;
    default:
     printf("\n***** ERROR ****");
      printf("\n %d > Node don't exp-node specifier\n", n->connective);
      exit(1);
  }
 }
        
}


void fprint_Wff_before_action_application2(FILE *out, WffNode *n, float value)

{

  
  switch(n->connective){
   case COMP:
    switch (n->comp) {
    case LE:
      fprintf(out, "(< ");
      break;
     case EQ:
      fprintf(out, "(= ");
      break;
    case LEQ:
      fprintf(out, "(<= ");
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
    fprint_ExpNode(out, n->lh );
    fprintf(out, " %.2f", n->rh->value + value);
    fprintf(out, ")\n");
    break;
    default:
     printf("\n***** ERROR ****");
      printf("\n %d > Node don't exp-node specifier\n", n->connective);
      exit(1);
  }
        
}




/*test, if initial satisfies preference goal
**/


Bool preference_exp_satisfy(WffNode *n)

{

    float value = 0.00;

    switch(n->connective){
    case COMP:
     switch(n->lh->connective){
      case FHEAD:
      case AD:
      case SU:
	 value = preference_exp_satisfy2(n);
       
        switch (n->comp) {
        case LE:
           if(value < n->rh->value ){
              return TRUE;
	   }           
         break;
        case LEQ:
           if(value <= n->rh->value ){
              return TRUE;
	   }  
         break;
        case EQ:
           if(value == n->rh->value ){
             return TRUE;
	    }  
        break;
        case GEQ:
           if(value >= n->rh->value ){
              return TRUE;
	   }  
         break;
        case GE:
           if(value > n->rh->value ){
              return TRUE;
	   }  
        break;
        default:
          printf("\nwrong comparator of Expnodes in WFF %d\n\n", n->comp);
          exit( 1 );
        }
      break;
      default:
       printf("\nwrong  Expnodes in WFF %d in preference_exp_satisfy!\n\n", n->lh->connective);
          exit( 1 );
    }
    break;
    default:
      printf("\n***** ERROR ****");
      printf("\n %d > Node don't exp-node specifier in prefrence_exp_satisfy!\n", n->connective);
      exit(1);
      
    }
     
    return FALSE;

}




Bool preference_exp_satisfy2(WffNode *n)

{

    int i;
    Fluent *f1 = NULL;
    Fluent *f2 = NULL;
    Fluent *f3 = NULL;
    float value = 0.00;

     switch(n->lh->connective){
      case FHEAD:
       f2 = n->lh->fluent;
       for( i = 0; i < gnum_full_fluents_initial; i++){
          f1 = &(gfull_fluents_initial[i].fluent);
          if(same_Fluent( f1, f2)){
                value = gfull_fluents_initial[i].value;
	        break;
           }
        }
       break;
      case AD:
        f2 = n->lh->leftson->fluent;
        f3 = n->lh->rightson->fluent;
       for( i = 0; i < gnum_full_fluents_initial; i++){
          f1 = &(gfull_fluents_initial[i].fluent);
          if(same_Fluent( f1, f2)){
                value = gfull_fluents_initial[i].value;
	        break;
           }
        }
        for( i = 0; i < gnum_full_fluents_initial; i++){
          f1 = &(gfull_fluents_initial[i].fluent);
          if(same_Fluent( f1, f3)){
                value += gfull_fluents_initial[i].value;
	        break;
           }
        }
       
      break;
      case SU:
        f2 = n->lh->fluent;
        f3 = n->rh->fluent;
       for( i = 0; i < gnum_full_fluents_initial; i++){
          f1 = &(gfull_fluents_initial[i].fluent);
          if(same_Fluent( f1, f2)){
                value = gfull_fluents_initial[i].value;
	        break;
           }
        }
        for( i = 0; i < gnum_full_fluents_initial; i++){
          f1 = &(gfull_fluents_initial[i].fluent);
          if(same_Fluent( f1, f3)){
                value -= gfull_fluents_initial[i].value;
	        break;
           }
        }
       
      break;
      default:
       printf("\nwrong  Expnodes in WFF %d in preference_exp_satisfy!\n\n", n->lh->connective);
          exit( 1 );
    }
     
    return value;

}



/**************************************************
 * is_State_meet_preference_expression:
 **************************************************/



Bool is_State_meet_preference_expression(WffNode *current )

{
    WffNode *i_son;
    Bool all = FALSE;
    Fact *fact = NULL;
    int i;
      switch(current->connective){
	 case ATOM:
	     
          for ( i = 0; i < ginitial_state.num_F; i++ ) {
              fact = &(grelevant_facts[ginitial_state.F[i]]);
    
	      if(same_Fact(fact,current->fact ))
	      {
		 all = TRUE;
                 break;
	       }
     
           }
          
	  break;
	  case NOT:
             if (ATOM==current->son->connective) {
                for ( i = 0; i < ginitial_state.num_F; i++ ) {
                   fact = &(grelevant_facts[ginitial_state.F[i]]);
	           if(same_Fact(fact,current->son->fact ))
	           {
		     all = FALSE;
                     break;
	            }
     
                }
          
              if(i==ginitial_state.num_F)          
	       if(!same_Fact( fact, current->son->fact )){
		   all = TRUE;
	         }
              }
	     break;
          case TRU:
              all = TRUE;
	     break;
          case FAL:
	      all = FALSE;
	     break;
	  case AND:
	      all = is_State_meet_preference_expression(current->sons);
              if(all){
                       for ( i_son = current->sons->next; i_son!=NULL; i_son = i_son->next ) {
			 if(all){
                            all = is_State_meet_preference_expression(i_son);
			 }
                         else{
                             break;
			 }
		       }
	           }
	    break;
	    case OR:
              all = is_State_meet_preference_expression(current->sons);
               if(!all){
                       for ( i_son = current->sons->next; i_son!=NULL; i_son = i_son->next ) {
			 if(!all){
                            all = is_State_meet_preference_expression(i_son);
			 }
                         else{
			     break;
			 }
		       }
	           }
            break;
            case COMP:
	      if(preference_exp_satisfy( current )){
		  all = TRUE;
	      }
              else{
                  all = FALSE;
	      }
            break;
	  default:  
             printf("\n***** ERROR ****");
             printf("\nWrong Node specifier <%d> in is_State_meet_preference_expression!\n", current->connective);
             exit(1);        

	 }

      return all;

}








/**************************************************
 * is_State_meet_preference_preconds:
 **************************************************/


Bool is_State_meet_preference_preconds(WffNode *current, Action *a )

{
    WffNode *i_son;
    Bool all = FALSE;
    Fact *fact;
    int i, k, j;
    TokenList *head = NULL;
    TokenList *head2 = NULL;
    TokenList *fact_element;
    TokenList *fact_element2;

      switch(current->connective){
	 case ATOM:
           head = NULL;
           head2 = NULL;
	   fact_element = NULL;
	   fact_element =  ( TokenList * ) calloc( 1, sizeof(TokenList ) );
           fact_element->item = gpredicates[current->fact->predicate];
           fact_element->next = head;
           head = fact_element;
           TokenList *tcurrent = head;
           for(j = 0; j < garity[current->fact->predicate];j++ ){
               while(tcurrent->next != NULL)
	       tcurrent = tcurrent->next;
               fact_element = ( TokenList * ) calloc( 1, sizeof(TokenList ) );         
               if(current->fact->args[j]>=0){
                   fact_element->item = gconstants[(current->fact->args)[j]];
                }
              else {
                   fact_element->item = gconstants[a->inst_table[DECODE_VAR(current->fact->args[j])]];       
               } 
               tcurrent->next = fact_element;
               fact_element->next = NULL;
             }

          for ( i = 0; i < ginitial_state.num_F; i++ ) {
              fact = &(grelevant_facts[ginitial_state.F[i]]);
            fact_element2 = NULL;
            fact_element2 = ( TokenList * ) calloc( 1, sizeof(TokenList ) );
            fact_element2->item = gpredicates[fact->predicate];
            fact_element2->next = head2;
            head2 = fact_element2;
            TokenList *tcurrent2 = head2;
            for(k = 0; k < garity[fact->predicate];k++ ){
               while(tcurrent2->next != NULL)
	       tcurrent2 = tcurrent2->next;
               fact_element2 = ( TokenList * ) calloc( 1, sizeof(TokenList ) );         
               if(fact->args[k]>=0){
                   fact_element2->item = gconstants[(fact->args)[k]];
                }
              else {
                   fact_element2->item = gconstants[a->inst_table[DECODE_VAR(fact->args[k])]];        } 
               tcurrent2->next = fact_element2;
               fact_element2->next = NULL;
             }

	      if(same_TokenList(head,head2))
	      {
		
		 all = TRUE;
                 break;
	       }
           }
	  break;
	  case NOT:
             if (ATOM==current->son->connective) {
	       fact_element =  ( TokenList * ) calloc( 1, sizeof(TokenList ) );
               fact_element->item = gpredicates[current->son->fact->predicate];
               fact_element->next = head;
               head = fact_element;
               TokenList *tcurrent = head;
               for(j = 0; j < garity[current->son->fact->predicate];j++ ){
                 while(tcurrent->next != NULL)
	         tcurrent = tcurrent->next;
                 fact_element = ( TokenList * ) calloc( 1, sizeof(TokenList ) );         
                 if(current->son->fact->args[j]>=0){
                     fact_element->item = gconstants[(current->son->fact->args)[j]];
                  }
                 else {
                     fact_element->item = gconstants[a->inst_table[DECODE_VAR(current->son->fact->args[j])]];        } 
		 tcurrent->next = fact_element;
                   fact_element->next = NULL;
                }

                for ( i = 0; i < ginitial_state.num_F; i++ ) {
                   fact = &(grelevant_facts[ginitial_state.F[i]]);

                    /*++++++++++++++++++++++++++++++++++++++++++++++*/
                  
                  fact_element2 = ( TokenList * ) calloc( 1, sizeof(TokenList ) );
                   fact_element->item = gpredicates[fact->predicate];
                   fact_element2->next = head2;
                   head2 = fact_element2;
                   TokenList *tcurrent2 = head2;
                   for(k = 0; k < garity[fact->predicate];k++ ){
                     while(tcurrent2->next != NULL)
	               tcurrent2 = tcurrent2->next;
                     fact_element2 = ( TokenList * ) calloc( 1, sizeof(TokenList ) );         
                     if(fact->args[k]>=0){
                        fact_element2->item = gconstants[(fact->args)[k]];
                      }
                     else {
                      fact_element2->item = gconstants[a->inst_table[DECODE_VAR(fact->args[k])]];       
                     } 

                    tcurrent2->next = fact_element2;
                    fact_element2->next = NULL;
             }
   

	           if(same_TokenList(head,head2 ))
	           {
		     all = FALSE;
                     break;
	            }
     
                }
          
              if(i==ginitial_state.num_F)          
	       if(!same_TokenList( head, head2 )){
		   all = TRUE;
	         }
              }
	     break;
	  case AND:
	      all = is_State_meet_preference_preconds(current->sons, a);
              if(all){
                       for ( i_son = current->sons->next; i_son!=NULL; i_son = i_son->next ) {
			 if(all){
                            all = is_State_meet_preference_preconds(i_son, a);
			 }
                         else{
                             break;
			 }
		       }
	           }
	    break;
	    case OR:
              all = is_State_meet_preference_preconds(current->sons, a);
               if(!all){
                       for ( i_son = current->sons->next; i_son!=NULL; i_son = i_son->next ) {
			 if(!all){
                            all = is_State_meet_preference_preconds(i_son, a);
			 }
                         else{
			     break;
			 }
		       }
	           }
            break;
            case COMP:
	      if(preference_exp_satisfy( current )){
		  all = TRUE;
	      }
              else{
                  all = FALSE;
	      }
            break;
	  default:  
             printf("\n***** ERROR ****");
             printf("\nWrong Node specifier <%d> in is_State_meet_preference_preconds!\n", current->connective);
             exit(1);        

	 }

      return all;

}








Bool is_effect_meet_preference(ActionEffect *e, float *value, WffNode *current, int *l)

{

      
        WffNode *i_son;
        Fact *fact = NULL;
        int all = 0;
        int help1 = 3;
        int help2 = 3;
        int k, i;
        int all1 = 0;
        int all2 = 0;
        float value1 = 0.00;
        float value2 = 0.00;

       /* test, if adds contains preference goal fact
       **/
       switch(current->connective){
       case ATOM:
        for ( i = 0; i < e->num_adds; i++ ) {
          fact =  &(grelevant_facts[e->adds[i]]);
	  if(same_Fact(fact,current->fact )){
	      all = 1;
            break;
	 }
         
       }
      
        if(i==e->num_adds){
         if(!same_Fact(fact,current->fact )){

         /* adds doesn't contain preference goal
	  * test, if dels contains preference goal
	  **/
          for ( k = 0; k < e->num_dels; k++ ) {
	     fact =  &(grelevant_facts[e->dels[k]]);
	  if(same_Fact(fact,current->fact )){
	           all = 2;
                   break;
	         }
               }            
	    }          
     	}
       break;
       case NOT:
        for ( i = 0; i < e->num_adds; i++ ) {
          fact =  &(grelevant_facts[e->adds[i]]);
	  if(same_Fact(fact,current->son->fact )){
	    all = 2;
            break;
	 }
       }      
     break;
     case TRU:
     case FAL:
     break;
     case AND:
       all = is_effect_meet_preference(e, value, current->sons, l);
       switch(current->sons->connective){
       case OR:
       case AND:
          if(all == 0 || all == 3){
              (*l)++;
           }
       break;
       case COMP:
	   if((all == 4)|| (all == 5))
	   return all;
       break;
       case ATOM:
       case NOT:
           if(all == 0){
              (*l)++;
           }
       break;
       default:
	   printf("is_effect_meet_pref: wrong specifier %d\n", current->sons->connective);
       }

               if((all == 1) || (all == 0)){
                       for ( i_son = current->sons->next; i_son!=NULL; i_son = i_son->next ) {

			 if((all == 1) || (all == 0)){
			    help1 = all;
                            
                            all = is_effect_meet_preference(e, value, i_son, l);
                            if((all != 2) && (all != help1))
                                help2 = 0;
                            
                             switch(i_son->connective){
                             case AND:
			     case OR:
                              if(all == 0 || all == 3){
                                   (*l)++;
                                }
                             break;
			     case COMP:
                                 if((all == 4)|| (all == 5)){
				   return all;
				 }
                             break;  
                             case ATOM:
                             case NOT:
                                if(all == 0){
                                   (*l)++;
                                 }
                            break;
                            default:
	                       printf("is_effect_meet_pref: wrong specifier %d\n", current->sons->connective);
                          }
	
			 }
                         else{
			   break;
			    }
		       }
	           } 

          if((all != 2) && (help2 == 0))
              all = 3;
     break;
     case OR:
       all = is_effect_meet_preference(e, value, current->sons, l);
       switch(current->sons->connective){
       case COMP:
	   if((all == 4)|| (all == 5)){
              return all;
           }
       break;
       case ATOM:
       case NOT:
        if(all == 0){
              (*l)++;
           }
       break;
       case OR:
       case AND:
        if(all == 0 || all == 3){
              (*l)++;
           }
       break;
       default:
	   printf("is_effect_meet_pref: wrong specifier %d\n", current->sons->connective);
       }

               if((all == 2) || (all == 0)){
                       for ( i_son = current->sons->next; i_son!=NULL; i_son = i_son->next ) {
		   
			 if((all == 2) || (all == 0)){
			    help1 = all;
                            all = is_effect_meet_preference(e, value, i_son, l);
                            if((all != 1) && (all != help1))
                                help2 = 0;
                             switch(i_son->connective){
                             case AND:
			     case OR:
                              if(all == 0 || all == 3){
                                 (*l)++;
                               }
                             break;
			     case COMP:
				 if((all == 4)|| (all == 5)){
				    return all;
                                }
                             break;
                             case ATOM:
                             case NOT:
                                if(all == 0){
                                   (*l)++;
                                 }
                            break;
                            default:
	                       printf("is_effect_meet_pref: wrong specifier %d\n", current->sons->connective);
                          }
                          
			 }
                         else{
			   break;
			    }
		       }
	           } 

          if((all != 1) && (help2 == 0))
              all = 3;
     break;
     case COMP:
	switch(current->lh->connective){
	 case FHEAD:
             *value = is_effect_meet_numeric_preference(current->lh->fluent, e,&all1 );
             all = all1;
             if(all == 5)
		 *value -= current->rh->value;
	     break;
         case AD:
            value1 = is_effect_meet_numeric_preference(current->lh->leftson->fluent, e, &all1);
            value2 = is_effect_meet_numeric_preference(current->lh->rightson->fluent, e, &all2);
            *value = value1 + value2;
            if((all1 == 4) || (all2 == 4))
                all = 4;
            if((all1 == 5) || (all2 == 5))
                all = 5;
	     break;
         case SU:
             value1 = is_effect_meet_numeric_preference(current->lh->leftson->fluent, e, &all1);
             value2 = is_effect_meet_numeric_preference(current->lh->rightson->fluent, e, &all2);
            *value = value1 - value2;
            if((all1 == 4) || (all2 == 4))
                all = 4;
            if((all1 == 5) || (all2 == 5))
                all = 5;
	     break;
	 default:
           printf("\nis_effect_meet_preference!\n");
	}
     break;
     default:         
	 printf("\nis_effect_meet_preference!\n");
     }
  
     return all;

}





float is_effect_meet_numeric_preference(Fluent *fl, ActionEffect *e, int *l)

{
  float value = 0.00;
  float b = 0.00;
  int i, j;
  char S1[80];
  char S2[80];     
  Bool assign = FALSE;

   for ( i = 0; i < e->num_numeric_effects; i++ ) {
	
      switch ( e->numeric_effects_neft[i] ) {

      case ASSIGN:
	assign = TRUE;
	b = 1.00;
	break;
      case INCREASE:
	b = -1.00;
	break;
      case DECREASE:
	b = 1.00;
	break;
      default:
	printf("\n\n illegal neft %d\n\n", 
	       e->numeric_effects_neft[i]);
	exit( 1 );
      }

      if ( e->numeric_effects_fl[i] >= 0 ) {

        sprintf(S1,"%s" , grelevant_fluents_name[e->numeric_effects_fl[i]]);
        
             sprintf(S2,"%s", gfunctions[fl->function]);
             for ( j=0; j<gf_arity[fl->function]; j++ ) {
               strcat(S2, "_");
               if ( fl->args[j] >= 0 ) {
                   strcat(S2, gconstants[(fl->args)[j]]);
               } else {
                   sprintf(S2,"%s%d",S2, DECODE_VAR(fl->args[j] ));
              }
             }
	     

      }
       
      if(!strcmp(S1, S2)){
        value = b * e->numeric_effects_rh[i]->value ;
	if(assign)
           *l = 5;
        else
	   *l = 4;
         break;
      }
    }
    

     return value;

}






Bool fprint_Wff_for_and_node(FILE *out, ActionEffect *e, WffNode *n)

{

        WffNode *i_son;
        Fact *fact = NULL;
        int all = 0;     
        int help = 1;
        int k, i;


       /* test, if adds contains preference goal fact
       **/
       switch(n->connective){
       case ATOM:
        for ( i = 0; i < e->num_adds; i++ ) {
          fact =  &(grelevant_facts[e->adds[i]]);
	  if(same_Fact(fact,n->fact )){
	      all = 1;
            break;
	 }
         
       }
      
        if(i==e->num_adds){
         if(!same_Fact(fact,n->fact )){

         /* adds doesn't contain preference goal
	  * test, if dels contains preference goal
	  **/
          for ( k = 0; k < e->num_dels; k++ ) {
	     fact =  &(grelevant_facts[e->dels[k]]);
	  if(same_Fact(fact,n->fact )){
	           all = 2;
                   break;
	         }
               }            
	    }          
     	}
     break;
     case NOT:
        for ( i = 0; i < e->num_adds; i++ ) {
          fact =  &(grelevant_facts[e->adds[i]]);
	  if(same_Fact(fact,n->son->fact )){
	    all = 2;
            break;
	 }
       }
      
     break;
     case OR:
	 all = fprint_Wff_for_and_node(out, e, n->sons);
          if(all == 2){
                  for ( i_son = n->sons->next; i_son!=NULL; i_son = i_son->next ) {
			 if(all == 2 || all == 0){
                            all = fprint_Wff_for_and_node(out, e, i_son);
                            if (all == 0)
				help = 0; 
			 }
                         else{
			     if(all == 1)
                             break;
                             
			 }
		       }
	           }
	 else{
            if(all == 0){
                help = 0;
                for ( i_son = n->sons->next; i_son!=NULL; i_son = i_son->next ) {
			 if(all == 0 || all == 2){
                            all = fprint_Wff_for_and_node(out, e, i_son);
                            if(!i_son->next){
				if(all == 2)
                                    all = 0;
			    }
                               
			 }
                         else{
			     if(all == 1)
                                break;
			 }
		       }
	           }
	 }
       if(all != 1){
	  if(help == 0)         
              all = 0;
       }   
     break;
     case AND:
       all = fprint_Wff_for_and_node(out, e, n->sons);
       if(all == 0)
	   fprint_Wff(out, n->sons);

               if((all == 1) || (all == 0)){
                       for ( i_son = n->sons->next; i_son!=NULL; i_son = i_son->next ) {
			 if((all == 1) || (all == 0)){
                            all = fprint_Wff_for_and_node(out, e, i_son);
                            if(all == 0)
				fprint_Wff(out, i_son);
			 }
                         else{
			   break;
			 }
		       }
	           }  
     break;
     default:         
	 printf("\nfprint_Wff_for_and_node: %d!\n", n->connective);
     }
  
     return all;



}




Bool fprint_Wff_for_or_node(FILE *out, ActionEffect *e, WffNode *n)

{


        WffNode *i_son;
        Fact *fact = NULL;
        int all = 0;     
        int help = 1;
        int k, i;


       /* test, if adds contains preference goal fact
       **/
       switch(n->connective){
       case ATOM:
        for ( i = 0; i < e->num_adds; i++ ) {
          fact =  &(grelevant_facts[e->adds[i]]);
	  if(same_Fact(fact,n->fact )){
	      all = 1;
            break;
	 }
         
       }
      
        if(i==e->num_adds){
         if(!same_Fact(fact,n->fact )){

         /* adds doesn't contain preference goal
	  * test, if dels contains preference goal
	  **/
          for ( k = 0; k < e->num_dels; k++ ) {
	     fact =  &(grelevant_facts[e->dels[k]]);
	  if(same_Fact(fact,n->fact )){
	           all = 2;
                   break;
	         }
               }            
	    }          
     	}
     break;
     case NOT:
        for ( i = 0; i < e->num_adds; i++ ) {
          fact =  &(grelevant_facts[e->adds[i]]);
	  if(same_Fact(fact,n->son->fact )){
	    all = 2;
            break;
	 }
       }
      
     break;
     case AND:
	 all = fprint_Wff_for_or_node(out, e, n->sons);
         if(all == 1){
                  for ( i_son = n->sons->next; i_son!=NULL; i_son = i_son->next ) {
			 if(all == 1 || all == 0){
                            all = fprint_Wff_for_or_node(out, e, i_son);
                            if (all == 0)
				help = 0; 
			 }
                         else{
			     if(all == 2)
                             break;
                             
			 }
		       }
	           }
	 else{
            if(all == 0){
                help = 0;
                for ( i_son = n->sons->next; i_son!=NULL; i_son = i_son->next ) {
			 if(all == 0 || all == 1){
                            all = fprint_Wff_for_or_node(out, e, i_son);
                            if(!i_son->next){
				if(all == 1)
                                    all = 0;
			    }
                               
			 }
                         else{
			     if(all == 2)
                                break;
			 }
		       }
	           }
	 }
       if(all != 2){
	  if(help == 0)         
              all = 0;
       }        
     break;
     case OR:
       all = fprint_Wff_for_or_node(out, e, n->sons);
       if(all == 0)
	   fprint_Wff(out, n->sons);

               if((all == 2) || (all == 0)){
                       for ( i_son = n->sons->next; i_son!=NULL; i_son = i_son->next ) {
			 if((all == 2) || (all == 0)){
                            all = fprint_Wff_for_or_node(out, e, i_son);
                            if(all == 0)
				fprint_Wff(out, i_son);
			 }
                         else{
			   break;
			 }
		       }
	           }  
     break;
     default:         
	 printf("\nfprint_Wff_for_or_node!\n");
     }
  
     return all;



}







Bool fprint_Wff_for_and_expnode(FILE *out, ActionEffect *e, WffNode *n, float *value, Bool bol)

{


      WffNode *i_son;
      int all = 0;
      char S1[80];
      char S2[80];     
      float b = 0.00;
      int i, j;


       /* test, if adds contains preference goal fact
       **/
       switch(n->connective){
       case COMP:
          for ( i = 0; i < e->num_numeric_effects; i++ ) {
            switch ( e->numeric_effects_neft[i] ) {
            case ASSIGN:
	      b = 0.00;
	    break;
            case INCREASE:
	      b = -1.00;
            break;
            case DECREASE:
	    b = 1.00;
	    break;
            default:
	    printf("\n\n illegal neft %d\n\n", 
	       e->numeric_effects_neft[i]);
	       exit( 1 );
           }

          if ( e->numeric_effects_fl[i] >= 0 ) {

           sprintf(S1,"%s" , grelevant_fluents_name[e->numeric_effects_fl[i]]);
           sprintf(S2,"%s", gfunctions[n->lh->fluent->function]);
           for ( j=0; j<gf_arity[n->lh->fluent->function]; j++ ) {
           strcat(S2, "_");
            if ( n->lh->fluent->args[j] >= 0 ) {
               strcat(S2, gconstants[(n->lh->fluent->args)[j]]);
             } else {
                 sprintf(S2,"%s%d",S2, DECODE_VAR(n->lh->fluent->args[j] ));
               }
              }

             }
       
            if(!strcmp(S1, S2)){
             *value = b * e->numeric_effects_rh[i]->value ;
              all = 4;
              break;
             }
           }

       break;
       case OR:
	 all = fprint_Wff_for_and_expnode(out, e, n->sons, value, bol);
          if(all == 0){
                  for ( i_son = n->sons->next; i_son!=NULL; i_son = i_son->next ) {
			 if( all == 0){
                            all = fprint_Wff_for_and_expnode(out, e, i_son, value, bol);
                            if (all == 4)
				break; 
			 }

		       }
	           }   
     break;
     case AND:
       all = fprint_Wff_for_and_expnode(out, e, n->sons, value, bol);
       if(all == 4){
	   if(bol)
	      fprint_Wff_before_action_application1(out, n->sons, *value);
           else
              fprint_Wff_before_action_application2(out, n->sons, *value);
       }
                       for ( i_son = n->sons->next; i_son!=NULL; i_son = i_son->next ) {
                            all = fprint_Wff_for_and_expnode(out, e, i_son, value, bol);
                            if(all == 4){
				if(bol)
				    fprint_Wff_before_action_application1(out, i_son, *value);
				else
                                    fprint_Wff_before_action_application2(out, n->sons, *value);
			    }
		       }  
     break;
     default:         
	 printf("\nfprint_Wff_for_and_expnode!\n");
     }
  
     return all;

}




Bool fprint_Wff_for_or_expnode(FILE *out, ActionEffect *e, WffNode *n, float *value, Bool bol)

{


        WffNode *i_son;
        int all = 0;     
        char S1[80];
        char S2[80];     
        float b = 0.00;
        int i, j;


       /* test, if adds contains preference goal fact
       **/
       switch(n->connective){
       case COMP:
          for ( i = 0; i < e->num_numeric_effects; i++ ) {
            switch ( e->numeric_effects_neft[i] ) {
            case ASSIGN:
	      b = 0.00;
	    break;
            case INCREASE:
	      b = -1.00;
            break;
            case DECREASE:
	    b = 1.00;
	    break;
            default:
	    printf("\n\n illegal neft %d\n\n", 
	       e->numeric_effects_neft[i]);
	       exit( 1 );
           }

          if ( e->numeric_effects_fl[i] >= 0 ) {

           sprintf(S1,"%s" , grelevant_fluents_name[e->numeric_effects_fl[i]]);
           sprintf(S2,"%s", gfunctions[n->lh->fluent->function]);
           for ( j=0; j<gf_arity[n->lh->fluent->function]; j++ ) {
           strcat(S2, "_");
            if ( n->lh->fluent->args[j] >= 0 ) {
               strcat(S2, gconstants[(n->lh->fluent->args)[j]]);
             } else {
                 sprintf(S2, "%s%d",S2, DECODE_VAR(n->lh->fluent->args[j] ));
               }
              }

             }
       
            if(!strcmp(S1, S2)){
             *value = b * e->numeric_effects_rh[i]->value ;
              all = 4;
              break;
             }
           }

       break;
       case AND:
	 all = fprint_Wff_for_or_expnode(out, e, n->sons, value, bol);
         if(all == 4){
                  for ( i_son = n->sons->next; i_son!=NULL; i_son = i_son->next ) {
			 if( all == 4){
                            all = fprint_Wff_for_or_expnode(out, e, i_son, value, bol);
                            if (all == 0)
				break; 
			 }
		       }
	           }        
     break;
     case OR:
       all = fprint_Wff_for_or_expnode(out, e, n->sons, value, bol);
       if(all == 4){
	 if(bol)
	   fprint_Wff_before_action_application1(out, n->sons, *value);
         else
           fprint_Wff_before_action_application2(out, n->sons, *value);
       }
                       for ( i_son = n->sons->next; i_son!=NULL; i_son = i_son->next ) {
                            all = fprint_Wff_for_or_expnode(out, e, i_son, value, bol);
                            if(all == 4){
			      if(bol)
				fprint_Wff_before_action_application1(out, i_son, *value);
                              else
                                fprint_Wff_before_action_application2(out, n->sons, *value);
			    }
		       }  
     break;
     default:         
	 printf("\nfprint_Wff_for_or_expnode!\n");
     }
  
     return all;

}




/**************************************************
 * gorunded simpleadl task*
 **************************************************/








void output_grounded_SIMPLEADL_task( void )

{

  FILE *fts, *ops;
  int i, j, k, ef;
  Action *a;

  if ( (fts = fopen( "groundedproblem.pddl", "w")) == NULL ) {
    printf("\n\nCannot open file facts.pddl.\n\n");
    exit( 1 );
  }
  fprintf(fts, "(define (problem grounded-SIMPLE-ADL-%s)\n", gproblem_name);
  fprintf(fts, "(:domain grounded-SIMPLE-ADL-%s)\n", gdomain_name);
  if ( ginitial_state.num_F > 0 ) {
    fprintf(fts, "(:init\n");
    for ( i = 0; i < ginitial_state.num_F; i++ ) {
      fprintf(fts, "(");
      fprint_ft_name(fts, ginitial_state.F[i]);
      fprintf(fts, ")\n");
    }
    fprintf(fts, ")\n");
  }


  fprintf(fts, "(:goal\n");
  fprintf(fts, "(and\n");
  for ( i = 0; i < gnum_logic_goal; i++ ) {
    fprintf(fts, "(");
    fprint_ft_name(fts,glogic_goal[i] );
    fprintf(fts, ")\n");
  }

  fprintf(fts, ")\n");
  fprintf(fts, ")\n");

  fprintf(fts, ")\n");
  fclose( fts);

  if ( (ops = fopen( "groundeddomain.pddl", "w")) == NULL ) {
    printf("\n\nCannot open file domain.pddl.\n\n");
    exit( 1 );
  }
  fprintf(ops, "(define (domain grounded-SIMPLE-ADL-%s)\n", gdomain_name);
  fprintf(ops, "(:requirements\n");
  fprintf(ops, ":strips\n");


  /*if ( gconditional_effects ) {
    fprintf(ops, ":conditional-effects\n");
  }
  if ( gdps ) {
    fprintf(ops, ":derived-predicates\n");
    }*/


  fprintf(ops, ")\n");
  fprintf(ops, "(:predicates\n");
  for ( i = 0; i < gnum_ft_conn; i++ ) {
    fprintf(ops, "(");
    fprint_ft_name(ops, i);
    fprintf(ops, ")\n");
  }
  fprintf(ops, ")\n");


  /* actions
   */


  for ( i = 0; i < gnum_op_conn; i++ ) {

    a = gop_conn[i].action;    

    fprintf(ops, "(:action ");
    fprint_op_name( ops, a);
    fprintf(ops, "\n");

    fprintf(ops, ":parameters ()\n");

    fprintf(ops, ":precondition\n");
    fprintf(ops, "(and\n");
     for ( j = 0; j < a->num_preconds; j++ ) {
	
        fprintf(ops, "(");
	fprint_ft_name( ops,a->preconds[j] );
        
        fprintf(ops, ")\n");

      }
     fprintf(ops, ")\n");

    fprintf(ops, ":effect\n");
    fprintf(ops, "(and\n");
    for ( j = 0; j < gop_conn[i].num_E; j++ ) {
      ef = gop_conn[i].E[j];
      if ( gef_conn[ef].num_PC > 0 ) {
	fprintf(ops, "(when\n");
	fprintf(ops, "(and\n");
	for ( k = 0; k < gef_conn[ef].num_PC; k++ ) {
	  fprintf(ops, "(");
	  fprint_ft_name(ops, gef_conn[ef].PC[k]);
	  fprintf(ops, ")\n");
	}
	fprintf(ops, ")\n");
	fprintf(ops, "(and\n");
      }
      for ( k = 0; k < gef_conn[ef].num_A; k++ ) {
	fprintf(ops, "(");
	fprint_ft_name(ops, gef_conn[ef].A[k]);
	fprintf(ops, ")\n");
      }
      for ( k = 0; k < gef_conn[ef].num_D; k++ ) {
	fprintf(ops, "(not (");
	fprint_ft_name(ops, gef_conn[ef].D[k]);
	fprintf(ops, "))\n");
      }
      if ( gef_conn[ef].num_PC > 0 ) {
	fprintf(ops, ")\n");
	fprintf(ops, ")\n");
      }
    }
    fprintf(ops, ")\n");
    fprintf(ops, ")\n");
  }



  /* derived rules
   */


  /* for ( i = 0; i < gnum_op_conn; i++ ) {
    if ( !gop_conn[i].axiom ) continue;
    fprintf(ops, "(:derived ");
    ef = gop_conn[i].E[0];
    fprintf(ops, "(");
    fprint_ft_name(ops, gef_conn[ef].A[0]);
    fprintf(ops, ")\n");
    if ( gop_conn[i].num_P == 0 ) {
      printf("\nwarning: derived rule trivially true");
      fprintf(ops, "(foo)\n");
    } else {
      fprintf(ops, "(and\n");
      for ( j = 0; j < gop_conn[i].num_P; j++ ) {
	fprintf(ops, "(");
	fprint_ft_name(ops, gop_conn[i].P[j]);
	fprintf(ops, ")\n");
      }
      fprintf(ops,")\n");
    }
    fprintf(ops,")\n");
    }*/

  fprintf(ops, ")\n");
  fclose( ops);

}






void fprint_ADL_Action(FILE *ops, Action *a)

{

    int i, j;
    ActionEffect *e;

     /* now print pre, add, del
     */
    if(isDurative)
         fprintf(ops, "(:durative-action ");
    else 
         fprintf(ops, "(:action ");
    fprint_op_name( ops, a);
    fprintf(ops, "\n:parameters ()\n");

    if(isDurative){
        fprintf(ops, ":duration (= ?duration ");
        fprint_ExpNode(ops, a->action_duration);
        fprintf(ops, ")\n");
    }
 
    if(isDurative)
        fprintf(ops, ":condition\n");
    else
       fprintf(ops, ":precondition\n");
    fprintf(ops, "(and\n");
        
    /* for sync-ordinary
    **/
    if(gconstraints || gconstraints_pref || gwithin_constraints || gwithin_constraints_pref){
	if(isDurative)
	    fprintf(ops, "(at start ");
               fprintf(ops, "(sync-ordinary)");
	if(isDurative)
	    fprintf(ops, ")");
        fprintf(ops, "\n");
    }
    /* end for sync-ordinary*/    

      for ( i = 0; i < a->num_preconds; i++ ) {
 
        if(isDurative)
        {          
	  switch( a->preconds_t[i]){
          case 1:
             fprintf(ops, "(at start ");
          break;
          case 2:
            fprintf(ops, "(over all ");
          break;
          case 3:
             fprintf(ops, "(at end ");
          break;
          default:
             printf("fprint_ADL_Action!\n");
	  }

         }

        fprintf(ops, "(");
	fprint_ft_name( ops,a->preconds[i] );
        
         if(isDurative)
         {
           fprintf(ops, ")");
    
         }

         fprintf(ops, ")\n");

      }

     if(isTempTimeWindows){
     
      for(i = 0; i < a->norm_operator->ttw_num_preconds; i++){
        if(isDurative)
        {
	    switch( a->tw_pre_t[i]){
            case 1:
               fprintf(ops, "(at start ");
            break;
            case 2:
               fprintf(ops, "(over all ");
            break;
            case 3:
               fprintf(ops, "(at end ");
            break;
            default:
               printf("\nfprint_ADL_Action!");
	   }

         }

        fprintf(ops, "(");
	fprint_hidden_TokenList( ops,a->tw_pre[i],"-");
        

         if(isDurative)
         {
           fprintf(ops, ")");
         }

        fprintf(ops, ")\n");

      }

    }
    

    for ( i = 0; i < a->num_numeric_preconds; i++ ) {    

    if(isDurative)
    {
        
	switch( a->numeric_preconds_lh_t[i]){
        case 1:
           fprintf(ops, "(at start ");
        break;
        case 2:
            fprintf(ops, "(over all ");
        break;
        case 3:
           fprintf(ops, "(at end ");
        break;
        default:
           printf("fprint_ADL_Action!\n");
	}
    
    }
    
    fprintf(ops, "(");
    switch ( a->numeric_preconds_comp[i] ) {
    case LE:
      fprintf(ops, "< ");
      break;
    case LEQ:
      fprintf(ops, "<= ");
      break;
    case EQ:
      fprintf(ops, "= ");
      break;
    case GEQ:
      fprintf(ops, ">= ");
      break;
    case GE:
      fprintf(ops, "> ");
      break;
    default:
      printf("\nwrong comparator of Expnodes in actionpre %d\n\n", 
	     a->numeric_preconds_comp[i]);
      exit( 1 );
    }
    
    fprint_ExpNode(ops,  a->numeric_preconds_lh[i] );
    fprint_ExpNode(ops,  a->numeric_preconds_rh[i] );

    if(isDurative)
    {
        fprintf(ops, ")");
    
    }
 
     fprintf(ops, ")\n");
    
  }

    fprintf(ops,")\n");
   
    fprintf(ops, ":effect\n");
    fprintf(ops, "(and\n");

    /* for sync-ordinary
    **/
    if(gconstraints || gconstraints_pref || gwithin_constraints || gwithin_constraints_pref){
	if(isDurative)
	    fprintf(ops, "(at start ");
               fprintf(ops, "(not (sync-ordinary))");
	if(isDurative)
	    fprintf(ops, ")");
        fprintf(ops, "\n");
    }
    /* end for sync-ordinary*/  
    /* for first sync-automaton
    **/
    if(gconstraints || gconstraints_pref || gwithin_constraints || gwithin_constraints_pref){
       
	if(isDurative)
	    fprintf(ops, "(at start ");
               fprintf(ops, "(sync-automaton-%s)", automata_name);
	if(isDurative)
	    fprintf(ops, ")");
        fprintf(ops, "\n");
    }
    /* end for first sync-automaton*/  
  
    /* change preference preconds in effect 
    **/
    if(isDomainPreference){  
    if(a->pref_preconds > 0){
	int i;
        if(a->norm_operator->operator->pref_preconds->name){
	    /* one pref_preconds*/
         if(isDurative)
          {
  	   switch(a->norm_operator->operator->pref_preconds->fact_t){
           case 1:
              fprintf(ops, "(at start ");
           break;
           case 2:
              fprintf(ops, "(over all ");
           break;
           case 3:
              fprintf(ops, "(at end ");
           break;
           default:
              printf("\nfprint_ADL_Action!\n");
	   }
           
          }            
         
         fprintf(ops, "(when (not (and ");
	for (i = 0; i < a->num_pref_preconds; i++ ){
            fprintf(ops, "(");
            fprint_Fact(ops,&(grelevant_facts[a->pref_preconds[i]]));
            fprintf(ops, ") ");
	}
          fprintf(ops, "))\n");
          fprintf(ops, "(increase ");
          fprintf(ops, "( IS_VIOLATED_%s", a->norm_operator->operator->pref_preconds->name);
          fprintf(ops, ")  %i", 1);

         if(isDurative)
            {
               fprintf(ops, ")");
            }
         fprintf(ops, "))\n");

	    
	}
    }
  }

    
    for ( j = 0; j < a->num_effects; j++ ) {
    
    e = &(a->effects[j]);
  
    if ( e->illegal ) printf(" ILLEGAL EFFECT!");

    if ( e->num_conditions + a->num_preconds + a->num_numeric_preconds > 0 ) {

         if(isDurative)
        {          
	    if(e->conditions){
	     switch( e->conditions_t[0]){
             case 1:
               fprintf(ops, "(at start ");
             break;
             case 2:
               fprintf(ops, "(over all ");
             break;
             case 3:
               fprintf(ops, "(at end ");
             break;
             default:
               printf("fprint_ADL_Action!");
	     }
	    }
            else{
		if(a->preconds){
                   switch( a->preconds_t[0]){
                   case 1:
                     fprintf(ops, "(at start ");
                   break;
                   case 2:
                     fprintf(ops, "(over all ");
                   break;
                   case 3:
                     fprintf(ops, "(at end ");
                   break;
                   default:
                    printf("fprint_ADL_Action!");
	        }
	       }
	      }
            }

	fprintf(ops, "(when\n");
	fprintf(ops, "(and\n");
        for ( i = 0; i < a->num_preconds; i++ ) {

        fprintf(ops, "(");
	fprint_ft_name( ops,a->preconds[i] );

         fprintf(ops, ")\n");

      }

	for ( i = 0; i < e->num_conditions; i++ ) {
	  fprintf(ops, "(");
	  fprint_ft_name(ops, e->conditions[i]);
	  fprintf(ops, ")\n");
	}

      for ( i = 0; i < e->num_numeric_conditions; i++ ) {
      switch ( e->numeric_conditions_comp[i] ) {
      case LE:
	fprintf(ops, "(< ");
	break;
      case LEQ:
	fprintf(ops, "(<= ");
	break;
      case EQ:
	fprintf(ops, "(= ");
	break;
      case GEQ:
	fprintf(ops, "(>= ");
	break;
      case GE:
	fprintf(ops, "(> ");
	break;
      default:
	printf("\nwrong comparator of Expnodes in normeff %d\n\n", 
	       e->numeric_conditions_comp[i]);
	exit( 1 );
      }
      fprint_ExpNode(ops, e->numeric_conditions_lh[i] );
      fprint_ExpNode(ops, e->numeric_conditions_rh[i] );
      fprintf(ops, ")\n");
      }        
	fprintf(ops, ")\n");
	fprintf(ops, "(and\n");
      }
      
   
    for ( i = 0; i < e->num_adds; i++ ) {
     
      fprintf(ops, "(");
      fprint_ft_name( ops, e->adds[i] );

        fprintf(ops, ")\n");
    }
   
    for ( i = 0; i < e->num_dels; i++ ) {

      fprintf(ops, "(not (");
      fprint_ft_name(ops,  e->dels[i] );

      fprintf(ops, "))\n"); 
    }
    
    
     for ( i = 0; i < e->num_numeric_effects; i++ ) {

      fprintf(ops, "(");
      switch ( e->numeric_effects_neft[i] ) {
      case ASSIGN:
	fprintf(ops, "assign ");
	break;
      case SCALE_UP:
	fprintf(ops, "scale-up ");
	break;
      case SCALE_DOWN:
	fprintf(ops, "scale-down ");
	break;
      case INCREASE:
	fprintf(ops, "increase ");
	break;
      case DECREASE:
	fprintf(ops, "decrease ");
	break;
      default:
	printf("\n\nprint normop: illegal neft %d\n\n", 
	       e->numeric_effects_neft[i]);
	exit( 1 );
      }

      fprintf(ops, "(");
      if ( e->numeric_effects_fl[i] >= 0 ) {	fprint_fl_name( ops, e->numeric_effects_fl[i] );
      } else {
	fprintf(ops, "[UNDEF]");
      }
      fprintf(ops, ")");

      fprint_ExpNode( ops, e->numeric_effects_rh[i] );
      
        fprintf(ops, ") \n");  
      }

      if (e->num_conditions + a->num_preconds + a->num_numeric_preconds > 0 ) {
	fprintf(ops, ")\n");
	fprintf(ops, ")\n");
        if(isDurative){
	    fprintf(ops, ")\n");
		}
	}


    /* for when with penalty
     */


    /* for preference goal penalty
    **/
    if(isPreference || gpref_head){
        WffNode *current1 = gpref_head;
        WffNode *i_son;
        WffNode *current2 = NULL;
        int all = 0;
        float value = 0.00;
        int l = 0;
        Bool bol = FALSE;
        Bool bol2 = FALSE;

     
     /* test, if there is conflict with effect
     **/
     if(current1->sons){
        current2 = current1->sons;
       /*first preference
       **/
       all = is_effect_meet_preference(e,&value, current2, &l);
       if(all != 0)  
	   bol = TRUE;
       if(!bol)
         for ( i_son = current2->next; i_son!=NULL; i_son = i_son->next ) {

	  /*second one preference and so on
	  **/
	  value = 0.00;
          l = 0;

          all = is_effect_meet_preference(e, &value,i_son, &l);
          if(all != 0){
              bol = TRUE;
              break;
	  }
	}
     }
     /* test, if there is conflict with numeric effect
     **/
     if(current1->sons){
        current2 = current1->sons;
       /*first preference
       **/
       all = is_effect_meet_preference(e,&value, current2, &l);
       if(all == 4)  
	   bol2 = TRUE;
       if(!bol2)
         for ( i_son = current2->next; i_son!=NULL; i_son = i_son->next ) {

	  /*second one preference and so on
	  **/
	  value = 0.00;
          l = 0;

          all = is_effect_meet_preference(e, &value,i_son, &l);
          if(all == 4){
              bol2 = TRUE;
              break;
	  }
	}
     }

  /* when with penalty 1
  **/

    if(bol){
      
       if ( e->num_conditions + a->num_preconds + a->num_numeric_preconds > 0 ) {

         if(isDurative)
        {          
	    if(e->conditions){
	     switch( e->conditions_t[0]){
             case 1:
               fprintf(ops, "(at start ");
             break;
             case 2:
               fprintf(ops, "(over all ");
             break;
             case 3:
               fprintf(ops, "(at end ");
             break;
             default:
               printf("\nfprint_ADL_Action!");
	     }
	    }
            else{
		if(a->preconds){
                   switch( a->preconds_t[0]){
                   case 1:
                     fprintf(ops, "(at start ");
                   break;
                   case 2:
                     fprintf(ops, "(over all ");
                   break;
                   case 3:
                     fprintf(ops, "(at end ");
                   break;
                   default:
                     printf("\nfprint_ADL_Action!");
	        }
	       }
	      }
            }

	fprintf(ops, "(when\n");
	fprintf(ops, "(and\n");
        for ( i = 0; i < a->num_preconds; i++ ) {

        fprintf(ops, "(");
	fprint_ft_name( ops,a->preconds[i] );

         fprintf(ops, ")\n");

      }

	for ( i = 0; i < e->num_conditions; i++ ) {
	  fprintf(ops, "(");
	  fprint_ft_name(ops, e->conditions[i]);
	  fprintf(ops, ")\n");
	}

      for ( i = 0; i < e->num_numeric_conditions; i++ ) {
      switch ( e->numeric_conditions_comp[i] ) {
      case LE:
	fprintf(ops, "(< ");
	break;
      case LEQ:
	fprintf(ops, "(<= ");
	break;
      case EQ:
	fprintf(ops, "(= ");
	break;
      case GEQ:
	fprintf(ops, "(>= ");
	break;
      case GE:
	fprintf(ops, "(> ");
	break;
      default:
	printf("\nwrong comparator of Expnodes in normeff %d\n\n", 
	       e->numeric_conditions_comp[i]);
	exit( 1 );
      }
      fprint_ExpNode(ops, e->numeric_conditions_lh[i] );
      fprint_ExpNode(ops, e->numeric_conditions_rh[i] );
      fprintf(ops, ")\n");
      }        

      /*+++++++ preconds for when with penalty +++++++++*/

      if(current1->sons){
        current2 = current1->sons;
       /*first preference
       **/
       all = is_effect_meet_preference(e,&value, current2, &l);
       switch (current2->connective){
       case ATOM:
       case NOT:
       break;
       case TRU:
       case FAL:
       break;
       case AND:    
              /* (and fac1 fact2 fact3 ...) preference
	       * no fact in dels
	       * some fact in adds, but not all
	       * some fact not in adds and not in dels
	      **/
              if(all == 3){
                       fprint_Wff_for_and_node(ops, e, current2);
	          }
              /* (and exp1 exp2 exp3 ...) preference
	       **/
              if(all == 4){
                       
                       fprint_Wff_for_and_expnode(ops, e, current2,&value, TRUE);
         
		       /*
                       fprint_Wff_for_and_expnode(ops, e, current2,&value, FALSE);
		       */
	          }
       break;  
       case OR:
               /* (or fac1 fact2 fact3 ...) preference
	       * no fact in adds
	       * some fact in dels, but not all
	       * some fact not in dels and not in adds
	       **/
               if(all == 3){
                       
                       fprintf(ops, "(not  ");
                       fprint_Wff_for_or_node(ops, e, current2);
                       fprintf(ops, ")");
	          }
               /* (or exp1 exp2 exp3 ...) preference
	       **/
               if(all == 4){
                       
                       fprint_Wff_for_or_expnode(ops, e, current2,&value, TRUE);
		                     
		       /* 
                       fprint_Wff_for_or_expnode(ops, e, current2,&value, FALSE);
		       */		       
	           
	          }
       break;
       case COMP:
            if(all == 4){
                   
                       fprint_Wff_before_action_application1(ops, current2, value);     
		       /*
                       fprint_Wff_before_action_application2(ops, current2, value);     
		       */
                       
	            }
       break;
       default:
          printf("\n***** ERROR ****");
          printf("\n %d > Wrong Node specifier in fprint_ADL_Action!\n", current2->connective);
          exit(1); 
 
	 }

          /*+++++++++++++++*/


      for ( i_son = current2->next; i_son!=NULL; i_son = i_son->next ) {

	  /*second one preference and so on
	  **/
	  value = 0.00;
          l = 0;

          all = is_effect_meet_preference(e, &value,i_son, &l);

	   switch (i_son->connective){
           case ATOM:
	   case NOT:
           break;
           case TRU:
           case FAL:
	   break;
           case AND:    
              /* (and fac1 fact2 fact3 ...) preference
	       * no fact in dels
	       * some fact in adds, but not all
	       * some fact not in adds and not in dels
	      **/
              if(all == 3){
                       fprint_Wff_for_and_node(ops, e, i_son);
	          }
               /* (and exp1 exp2 exp3 ...) preference
	       **/
               if(all == 4){
                       
                       fprint_Wff_for_and_expnode(ops, e, i_son, &value, TRUE);
                       /*
                       fprint_Wff_for_and_expnode(ops, e, i_son, &value, FALSE);
		       */
		       
	          }
         break;       
	 case OR:
               /* (or fac1 fact2 fact3 ...) preference
	       * no fact in adds
	       * some fact in dels, but not all
	       * some fact not in dels and not in adds
	       **/
               if(all == 3){
                  
                       fprintf(ops, "(not  ");
                       fprint_Wff_for_or_node(ops, e, i_son);
                       fprintf(ops, ")"); 
		   }
                /* (or exp1 exp2 exp3 ...) preference
	        **/
                if(all == 4){
                       fprint_Wff_for_or_expnode(ops, e, i_son, &value, TRUE);  
                       /*
                       fprint_Wff_for_or_expnode(ops, e, i_son, &value, FALSE);
		       */
	           
	          }
         break;
         case COMP:
		 if(all == 4){
                   
                       fprint_Wff_before_action_application1(ops, i_son, value); 
		       /*
			 fprint_Wff_before_action_application2(ops, i_son, value); */
                       
	           } 
         break;
         default:
            printf("\n***** ERROR ****");
            printf("\n %d > Wrong Node specifier in fprint_ADL_Action!\n", i_son->connective);
            exit(1);
      }
     } 
    }

      /*+++++++ end for preconds for when with penalty +*/

	fprintf(ops, ")\n");
	fprintf(ops, "(and\n");
      }
      

/*hhhh*/
     /*+++++++ effects for when with penalty +*/     
        
     if ( current1->sons ) {
        current2 = current1->sons;
       /*first preference
       **/
       all = is_effect_meet_preference(e,&value, current2, &l);
       switch (current2->connective){
       case ATOM:
       case NOT:
            if(all == 2){
                   
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", current2->name);
                       fprintf(ops, ")  %i)\n", 1);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", current2->name);
                       fprintf(ops, ")  %i)\n", 0);

                     
	            }
              if(all == 1){
                   
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", current2->name);
                       fprintf(ops, ")  %i)\n", 0);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", current2->name);
                       fprintf(ops, ")  %i)\n", 1);

                       
	            }
       break;
       case TRU:
       case FAL:
       break;
       case AND:    
            /* (and fac1 fact2 fact3 ...) preference
	       * one or some fact in dels
	      **/
            if(all == 2){
                       
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", current2->name);
                       fprintf(ops, ")  %i)\n", 1);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", current2->name);
                       fprintf(ops, ")  %i)\n", 0);

                       
	          }
              /* (and fac1 fact2 fact3 ...) preference
	       * all facts in adds
	      **/
              if(all == 1){
                       
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", current2->name);
                       fprintf(ops, ")  %i)\n", 0);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", current2->name);
                       fprintf(ops, ")  %i)\n", 1);

                       
	          }
              /* (and fac1 fact2 fact3 ...) preference
	       * no fact in dels
	       * some fact in adds, but not all
	       * some fact not in adds and not in dels
	      **/
              if(all == 3){
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", current2->name);
                       fprintf(ops, ")  %i)\n", 0);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", current2->name);
                       fprintf(ops, ")  %i)\n", 1);
                       
	           
	          }
              /* (and exp1 exp2 exp3 ...) preference
	       **/
              if(all == 4){
                       
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", current2->name);
                       fprintf(ops, ")  %i)\n", 1);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", current2->name);
                       fprintf(ops, ")  %i)\n", 0);

                       
                      
                       /*
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", current2->name);
                       fprintf(ops, ")  %i", 0);

                       fprintf(ops, ")\n");*/
		       
	          }

       break;  
       case OR:
            /* (or fac1 fact2 fact3 ...) preference
	       * all facts in dels
	      **/
            if(all == 2){
                       
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", current2->name);
                       fprintf(ops, ")  %i)\n", 1);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", current2->name);
                       fprintf(ops, ")  %i)\n", 0);
                       
	          }
              /* (or fac1 fact2 fact3 ...) preference
	       * one or some fact in adds
	      **/
              if(all == 1){
                       
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", current2->name);
                       fprintf(ops, ")  %i)\n", 0);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", current2->name);
                       fprintf(ops, ")  %i)\n", 1);
                       
	          }
               /* (or fac1 fact2 fact3 ...) preference
	       * no fact in adds
	       * some fact in dels, but not all
	       * some fact not in dels and not in adds
	       **/
               if(all == 3){
                       
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", current2->name);
                       fprintf(ops, ")  %i)\n", 1);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", current2->name);
                       fprintf(ops, ")  %i)\n", 0);

                       
	          }
               /* (or exp1 exp2 exp3 ...) preference
	       **/
               if(all == 4){
                       
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", current2->name);
                       fprintf(ops, ")  %i)\n", 1);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", current2->name);
                       fprintf(ops, ")  %i)\n", 0);

                       /*
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", current2->name);
                       fprintf(ops, ")  %i", 0);

                       fprintf(ops, ")\n"); */
	           
	          }
       break;
       case COMP:
            if(all == 4){
                       
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", current2->name);
                       fprintf(ops, ")  %i)\n", 1);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", current2->name);
                       fprintf(ops, ")  %i)\n", 0);

		       /*    
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", current2->name);
                       fprintf(ops, ")  %i)\n", 0);*/

	            }
             if(all == 5){
                       
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", current2->name);
                       if(found_violated_or_not(current2, value))
                           fprintf(ops, ")  %i", 0);
                       else
                           fprintf(ops, ")  %i", 1);
                       fprintf(ops, ")\n");


                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", current2->name);
                       if(found_violated_or_not(current2, value))
                           fprintf(ops, ")  %i", 1);
                       else
                           fprintf(ops, ")  %i", 0);
                       fprintf(ops, ")\n");

	            }
       break;
       default:
          printf("\n***** ERROR ****");
          printf("\n %d > Wrong Node specifier in fprint_ADL_Action!\n", current2->connective);
          exit(1); 
 
	 }

          /*+++++++++++++++*/


      for ( i_son = current2->next; i_son!=NULL; i_son = i_son->next ) {

	  /*second one preference and so on
	  **/
	  value = 0.00;
          l = 0;

          all = is_effect_meet_preference(e, &value,i_son, &l);

	   switch (i_son->connective){
	   case ATOM:
           case NOT:
		 if(all == 2){
                   
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", i_son->name);
                       fprintf(ops, ")  %i)\n", 1);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", i_son->name);
                       fprintf(ops, ")  %i)\n", 0);

                       
	           } 
                 if(all == 1){
                   
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", i_son->name);
                       fprintf(ops, ")  %i)\n", 0);

                        fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", i_son->name);
                       fprintf(ops, ")  %i)\n", 1);

                       
	           } 
	   break;
          case TRU:
          case FAL:
	  break;
          case AND:    
            /* (and fac1 fact2 fact3 ...) preference
	       * one or some fact in dels
	      **/
            if(all == 2){
                       
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", i_son->name);
                       fprintf(ops, ")  %i)\n", 1);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", i_son->name);
                       fprintf(ops, ")  %i)\n", 0);
                       
	          }
              /* (and fac1 fact2 fact3 ...) preference
	       * all facts in adds
	      **/
              if(all == 1){
                       
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", i_son->name);
                       fprintf(ops, ")  %i)\n", 0);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", i_son->name);
                       fprintf(ops, ")  %i)\n", 1);

                       
	          }
              /* (and fac1 fact2 fact3 ...) preference
	       * no fact in dels
	       * some fact in adds, but not all
	       * some fact not in adds and not in dels
	      **/
              if(all == 3){
                       
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", i_son->name);
                       fprintf(ops, ")  %i)\n", 0);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", i_son->name);
                       fprintf(ops, ")  %i)\n", 1);

                       
	          }
                  /* (and exp1 exp2 exp3 ...) preference
	          **/
                  if(all == 4){
                       
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", i_son->name);
                       fprintf(ops, ")  %i)\n", 1);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", i_son->name);
                       fprintf(ops, ")  %i)\n", 0);
                       
                       
                       /*
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", i_son->name);
                       fprintf(ops, ")  %i", 0);

                       fprintf(ops, ")\n");
		       */
	           
	          }
         break;       
	 case OR:
                 /* (or fac1 fact2 fact3 ...) preference
	         * all facts in dels
	         **/
		 if(all == 2){
                  
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", i_son->name);
                       fprintf(ops, ")  %i)\n", 1);

                        fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", i_son->name);
                       fprintf(ops, ")  %i)\n", 0);

                       
		 }
                /* (or fac1 fact2 fact3 ...) preference
	        * one or some fact in adds
	        **/
                if(all == 1){
                  
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", i_son->name);
                       fprintf(ops, ")  %i)\n", 0);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", i_son->name);
                       fprintf(ops, ")  %i)\n", 1);
		 }
               /* (or fac1 fact2 fact3 ...) preference
	       * no fact in adds
	       * some fact in dels, but not all
	       * some fact not in dels and not in adds
	       **/
               if(all == 3){
                  
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", i_son->name);
                       fprintf(ops, ")  %i)\n", 1);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", i_son->name);
                       fprintf(ops, ")  %i)\n", 0);

                       
		   }
               /* (or exp1 exp2 exp3 ...) preference
	       **/
               if(all == 4){
                       
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", i_son->name);
                       fprintf(ops, ")  %i)\n", 1);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", i_son->name);
                       fprintf(ops, ")  %i)\n", 0);
                       
                       
                       /*     
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", i_son->name);
                       fprintf(ops, ")  %i", 0);

                       fprintf(ops, ")\n");
		       */
	           
	          }
         break;
         case COMP:
		 if(all == 4){
                   
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", i_son->name);
                       fprintf(ops, ")  %i)\n", 1);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", i_son->name);
                       fprintf(ops, ")  %i)\n", 0);

		       /*
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", i_son->name);
                       fprintf(ops, ")  %i)\n", 0);*/

	           } 
                  if(all == 5){
                       
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", i_son->name);
                       if(found_violated_or_not(i_son, value))
                           fprintf(ops, ")  %i", 0);
                       else
                           fprintf(ops, ")  %i", 1);

                       fprintf(ops, ")\n");


                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", i_son->name);
                       if(found_violated_or_not(i_son, value))
                           fprintf(ops, ")  %i", 1);
                       else
                           fprintf(ops, ")  %i", 0);

                       fprintf(ops, ")\n");

	            }
         break;
         default:
            printf("\n***** ERROR ****");
            printf("\n %d > Wrong Node specifier in fprint_ADL_Action!\n", i_son->connective);
            exit(1);
      }
     } 
    }
  

    if (e->num_conditions + a->num_preconds + a->num_numeric_preconds > 0 ) {
	fprintf(ops, ")\n");
	fprintf(ops, ")\n");
        if(isDurative){
	    fprintf(ops, ")\n");
		}

	}
      }

     /*+++++++ end for effects for when with penalty 1 +*/ 

    /* when with penalty 0
    **/
    if(bol2) {
      
       if ( e->num_conditions + a->num_preconds + a->num_numeric_preconds > 0 ) {

         if(isDurative)
        {          
	    if(e->conditions){
	     switch( e->conditions_t[0]){
             case 1:
               fprintf(ops, "(at start ");
             break;
             case 2:
               fprintf(ops, "(over all ");
             break;
             case 3:
               fprintf(ops, "(at end ");
             break;
             default:
               printf("\nfprint_ADL_Action!");
	     }
	    }
            else{
		if(a->preconds){
                   switch( a->preconds_t[0]){
                   case 1:
                     fprintf(ops, "(at start ");
                   break;
                   case 2:
                     fprintf(ops, "(over all ");
                   break;
                   case 3:
                     fprintf(ops, "(at end ");
                   break;
                   default:
                     printf("\nfprint_ADL_Action!");
	        }
	       }
	      }
            }

	fprintf(ops, "(when\n");
	fprintf(ops, "(and\n");
        for ( i = 0; i < a->num_preconds; i++ ) {

        fprintf(ops, "(");
	fprint_ft_name( ops,a->preconds[i] );

         fprintf(ops, ")\n");

      }

	for ( i = 0; i < e->num_conditions; i++ ) {
	  fprintf(ops, "(");
	  fprint_ft_name(ops, e->conditions[i]);
	  fprintf(ops, ")\n");
	}

      for ( i = 0; i < e->num_numeric_conditions; i++ ) {
      switch ( e->numeric_conditions_comp[i] ) {
      case LE:
	fprintf(ops, "(< ");
	break;
      case LEQ:
	fprintf(ops, "(<= ");
	break;
      case EQ:
	fprintf(ops, "(= ");
	break;
      case GEQ:
	fprintf(ops, "(>= ");
	break;
      case GE:
	fprintf(ops, "(> ");
	break;
      default:
	printf("\nwrong comparator of Expnodes in normeff %d\n\n", 
	       e->numeric_conditions_comp[i]);
	exit( 1 );
      }
      fprint_ExpNode(ops, e->numeric_conditions_lh[i] );
      fprint_ExpNode(ops, e->numeric_conditions_rh[i] );
      fprintf(ops, ")\n");
      }        

      /*+++++++ preconds for when with penalty +++++++++*/

      if(current1->sons){
        current2 = current1->sons;
       /*first preference
       **/
       all = is_effect_meet_preference(e,&value, current2, &l);
       switch (current2->connective){
       case ATOM:
       case NOT:
       break;
       case TRU:
       case FAL:
       break;
       case AND:    
              /* (and fac1 fact2 fact3 ...) preference
	       * no fact in dels
	       * some fact in adds, but not all
	       * some fact not in adds and not in dels
	      **/
              if(all == 3){
                       fprint_Wff_for_and_node(ops, e, current2);
	          }
              /* (and exp1 exp2 exp3 ...) preference
	       **/
              if(all == 4){
                       
		       
                       fprint_Wff_for_and_expnode(ops, e, current2,&value, FALSE);
	          }
       break;  
       case OR:
               /* (or fac1 fact2 fact3 ...) preference
	       * no fact in adds
	       * some fact in dels, but not all
	       * some fact not in dels and not in adds
	       **/
               if(all == 3){
                       
                       fprintf(ops, "(not  ");
                       fprint_Wff_for_or_node(ops, e, current2);
                       fprintf(ops, ")");
	          }
               /* (or exp1 exp2 exp3 ...) preference
	       **/
               if(all == 4){
                       
                       fprint_Wff_for_or_expnode(ops, e, current2,&value, FALSE);
	           
	          }
       break;
       case COMP:
            if(all == 4){
                       fprint_Wff_before_action_application2(ops, current2, value);     
	            }
       break;
       default:
          printf("\n***** ERROR ****");
          printf("\n %d > Wrong Node specifier in fprint_ADL_Action!\n", current2->connective);
          exit(1); 
 
	 }

          /*+++++++++++++++*/


      for ( i_son = current2->next; i_son!=NULL; i_son = i_son->next ) {

	  /*second one preference and so on
	  **/
	  value = 0.00;
          l = 0;

          all = is_effect_meet_preference(e, &value,i_son, &l);

	   switch (i_son->connective){
           case ATOM:
           case NOT:
           break;
           case TRU:
           case FAL:
	   break;
           case AND:    
              /* (and fac1 fact2 fact3 ...) preference
	       * no fact in dels
	       * some fact in adds, but not all
	       * some fact not in adds and not in dels
	      **/
              if(all == 3){
                       fprint_Wff_for_and_node(ops, e, i_son);
	          }
               /* (and exp1 exp2 exp3 ...) preference
	       **/
               if(all == 4){
                       fprint_Wff_for_and_expnode(ops, e, i_son, &value, FALSE);
		       
	          }
         break;       
	 case OR:
               /* (or fac1 fact2 fact3 ...) preference
	       * no fact in adds
	       * some fact in dels, but not all
	       * some fact not in dels and not in adds
	       **/
               if(all == 3){
                  
                       fprintf(ops, "(not  ");
                       fprint_Wff_for_or_node(ops, e, i_son);
                       fprintf(ops, ")"); 
		   }
                /* (or exp1 exp2 exp3 ...) preference
	        **/
                if(all == 4){
                       fprint_Wff_for_or_expnode(ops, e, i_son, &value, FALSE);
		       
	          }
         break;
         case COMP:
		 if(all == 4){
			 fprint_Wff_before_action_application2(ops, i_son, value); 
	           } 
         break;
         default:
            printf("\n***** ERROR ****");
            printf("\n %d > Wrong Node specifier in fprint_ADL_Action!\n", i_son->connective);
            exit(1);
      }
     } 
    }

      /*+++++++ end for preconds for when with penalty +*/

	fprintf(ops, ")\n");
	fprintf(ops, "(and\n");
      }
      
/*hhhh*/

     /*+++++++ effects for when with penalty +*/     
        
     if ( current1->sons ) {
        current2 = current1->sons;
       /*first preference
       **/
       all = is_effect_meet_preference(e,&value, current2, &l);
       switch (current2->connective){
       case ATOM:
       case NOT:
            if(all == 2){
                   
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", current2->name);
                       fprintf(ops, ")  %i)\n", 1);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", current2->name);
                       fprintf(ops, ")  %i)\n", 0);
                       
	            }
              if(all == 1){
                   
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", current2->name);
                       fprintf(ops, ")  %i)\n", 0);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", current2->name);
                       fprintf(ops, ")  %i)\n", 1);
	            }
       break;
       case TRU:
       case FAL:
       break;
       case AND:    
            /* (and fac1 fact2 fact3 ...) preference
	       * one or some fact in dels
	      **/
            if(all == 2){
                       
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", current2->name);
                       fprintf(ops, ")  %i)\n", 1);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", current2->name);
                       fprintf(ops, ")  %i)\n", 0);
                       
	          }
              /* (and fac1 fact2 fact3 ...) preference
	       * all facts in adds
	      **/
              if(all == 1){
                       
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", current2->name);
                       fprintf(ops, ")  %i)\n", 0);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", current2->name);
                       fprintf(ops, ")  %i)\n", 1);
                       
	          }
              /* (and fac1 fact2 fact3 ...) preference
	       * no fact in dels
	       * some fact in adds, but not all
	       * some fact not in adds and not in dels
	      **/
              if(all == 3){
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", current2->name);
                       fprintf(ops, ")  %i)\n", 0);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", current2->name);
                       fprintf(ops, ")  %i)\n", 1);
	           
	          }
              /* (and exp1 exp2 exp3 ...) preference
	       **/
              if(all == 4){          
                     
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", current2->name);
                       fprintf(ops, ")  %i)\n", 0);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", current2->name);
                       fprintf(ops, ")  %i)\n", 1);
		       
	          }
       break;  
       case OR:
            /* (or fac1 fact2 fact3 ...) preference
	       * all facts in dels
	      **/
            if(all == 2){
                       
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", current2->name);
                       fprintf(ops, ")  %i)\n", 1);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", current2->name);
                       fprintf(ops, ")  %i)\n", 0);
	          }
              /* (or fac1 fact2 fact3 ...) preference
	       * one or some fact in adds
	      **/
              if(all == 1){
                       
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", current2->name);
                       fprintf(ops, ")  %i)\n", 0);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", current2->name);
                       fprintf(ops, ")  %i)\n", 1);
	          }
               /* (or fac1 fact2 fact3 ...) preference
	       * no fact in adds
	       * some fact in dels, but not all
	       * some fact not in dels and not in adds
	       **/
               if(all == 3){
                       
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", current2->name);
                       fprintf(ops, ")  %i)\n", 1);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", current2->name);
                       fprintf(ops, ")  %i)\n", 0);
	          }
               /* (or exp1 exp2 exp3 ...) preference
	       **/
               if(all == 4){
                       
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", current2->name);
                       fprintf(ops, ")  %i)\n", 0);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", current2->name);
                       fprintf(ops, ")  %i)\n", 1);
	           
	          }
       break;
       case COMP:
            if(all == 4){
                      
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", current2->name);
                       fprintf(ops, ")  %i)\n", 0);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", current2->name);
                       fprintf(ops, ")  %i)\n", 1);

	            }
               if(all == 5){
                       
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", current2->name);
                       if(found_violated_or_not(current2, value))
                           fprintf(ops, ")  %i", 0);
                       else
                           fprintf(ops, ")  %i", 1);

                       fprintf(ops, ")\n");


                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", current2->name);
                       if(found_violated_or_not(current2, value))
                           fprintf(ops, ")  %i", 1);
                       else
                           fprintf(ops, ")  %i", 0);

                       fprintf(ops, ")\n");

	            }
       break;
       default:
          printf("\n***** ERROR ****");
          printf("\n %d > Wrong Node specifier in fprint_ADL_Action!\n", current2->connective);
          exit(1); 
 
	 }

          /*+++++++++++++++*/


      for ( i_son = current2->next; i_son!=NULL; i_son = i_son->next ) {

	  /*second one preference and so on
	  **/
	  value = 0.00;
          l = 0;

          all = is_effect_meet_preference(e, &value,i_son, &l);

	   switch (i_son->connective){
	   case ATOM:
           case NOT:
		 if(all == 2){
                   
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", i_son->name);
                       fprintf(ops, ")  %i)\n", 1);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", i_son->name);
                       fprintf(ops, ")  %i)\n", 0);

	           } 
                 if(all == 1){
                   
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", i_son->name);
                       fprintf(ops, ")  %i", 0);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", i_son->name);
                       fprintf(ops, ")  %i", 1);
	           } 
	   break;
           case TRU:
           case FAL:
	   break;
           case AND:    
            /* (and fac1 fact2 fact3 ...) preference
	       * one or some fact in dels
	      **/
            if(all == 2){
                       
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", i_son->name);
                       fprintf(ops, ")  %i)\n", 1);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", i_son->name);
                       fprintf(ops, ")  %i)\n", 0);
	          }
              /* (and fac1 fact2 fact3 ...) preference
	       * all facts in adds
	      **/
              if(all == 1){
                       
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", i_son->name);
                       fprintf(ops, ")  %i)\n", 0);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", i_son->name);
                       fprintf(ops, ")  %i)\n", 1);
	          }
              /* (and fac1 fact2 fact3 ...) preference
	       * no fact in dels
	       * some fact in adds, but not all
	       * some fact not in adds and not in dels
	      **/
              if(all == 3){
                       
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", i_son->name);
                       fprintf(ops, ")  %i)\n", 0);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", i_son->name);
                       fprintf(ops, ")  %i)\n", 1);
	          }
                  /* (and exp1 exp2 exp3 ...) preference
	          **/
                  if(all == 4){
                       
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", i_son->name);
                       fprintf(ops, ")  %i)\n", 0);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", i_son->name);
                       fprintf(ops, ")  %i)\n", 1);
	           
	          }
         break;       
	 case OR:
                 /* (or fac1 fact2 fact3 ...) preference
	         * all facts in dels
	         **/
		 if(all == 2){
                  
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", i_son->name);
                       fprintf(ops, ")  %i)\n", 1);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", i_son->name);
                       fprintf(ops, ")  %i)\n", 0);
		 }
                /* (or fac1 fact2 fact3 ...) preference
	        * one or some fact in adds
	        **/
                if(all == 1){
                  
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", i_son->name);
                       fprintf(ops, ")  %i)\n", 0);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", i_son->name);
                       fprintf(ops, ")  %i)\n", 1);
		 }
               /* (or fac1 fact2 fact3 ...) preference
	       * no fact in adds
	       * some fact in dels, but not all
	       * some fact not in dels and not in adds
	       **/
               if(all == 3){
                  
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", i_son->name);
                       fprintf(ops, ")  %i)\n", 1);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", i_son->name);
                       fprintf(ops, ")  %i)\n", 0);
		   }
               /* (or exp1 exp2 exp3 ...) preference
	       **/
               if(all == 4){
                       
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", i_son->name);
                       fprintf(ops, ")  %i", 0);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", i_son->name);
                       fprintf(ops, ")  %i)\n", 1);
	           
	          }
         break;
         case COMP:
		 if(all == 4){
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", i_son->name);
                       fprintf(ops, ")  %i)\n", 0);

                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", i_son->name);
                       fprintf(ops, ")  %i)\n", 1);

	           } 
                if(all == 5){
                       
                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_VIOLATED_%s", i_son->name);
                       if(found_violated_or_not(i_son, value))
                           fprintf(ops, ")  %i", 0);
                       else
                           fprintf(ops, ")  %i", 1);

                       fprintf(ops, ")\n");


                       fprintf(ops, "(assign ");
                       fprintf(ops, "( IS_SATISFIED_%s", i_son->name);
                       if(found_violated_or_not(i_son, value))
                           fprintf(ops, ")  %i", 1);
                       else
                           fprintf(ops, ")  %i", 0);

                       fprintf(ops, ")\n");

	            }
         break;
         default:
            printf("\n***** ERROR ****");
            printf("\n %d > Wrong Node specifier in fprint_ADL_Action!\n", i_son->connective);
            exit(1);
      }
     } 
    }
  

    if (e->num_conditions + a->num_preconds + a->num_numeric_preconds > 0 ) {
	fprintf(ops, ")\n");
	fprintf(ops, ")\n");
        if(isDurative){
	    fprintf(ops, ")\n");
		}

	}
      }
      
    /*+++++++ end for effects for when with penalty 0 **/

    }

   /*+++++++ end for effects for when with penalty +*/ 
   

   }
 
    fprintf(ops, ")\n");
    fprintf(ops, ")\n");    


}







/**************************************************
 * gorunded adl task*
 **************************************************/








void output_grounded_ADL_task( void )

{

  FILE *fts, *ops;
  int i, j, k;
  Action *a;
  char s1[30];
  char s2[30];
  int k1 = 0;
  int k2 = 0;
  int k3 = 0;
  int k4 = 0;

  if ( (fts = fopen( "groundedproblem.pddl", "w")) == NULL ) {
    printf("\n\nCannot open file facts.pddl.\n\n");
    exit( 1 );
  }
  fprintf(fts, "(define (problem grounded-ADL-%s)\n", gproblem_name);
  fprintf(fts, "(:domain grounded-ADL-%s)\n", gdomain_name);

   fprintf(fts, "(:init\n");
  if ( ginitial_state.num_F > 0 ) {
    for ( i = 0; i < ginitial_state.num_F; i++ ) {
      fprintf(fts, "(");
      fprint_ft_name(fts, ginitial_state.F[i]);
      fprintf(fts, ")\n");
    }

  }



  /*for preferences goal
   **/
   if(isPreference || gpref_head){
        WffNode *current1 = gpref_head;
        WffNode *i_son;
        WffNode *current2 = NULL;
        Bool all = TRUE;

    /* test, if initial  contains preference goal
     *  first preference
     */
       if ( current1->sons ) {
	 current2 = current1->sons;

          all = is_State_meet_preference_expression(current2);
            fprintf(fts,"(=(");
                 fprintf(fts, "IS_VIOLATED_%s)",current2->name);

              	if(all){
                   fprintf(fts, " %i", 0);
                   fprintf(fts, ")\n");
		}
                else{
                   fprintf(fts, " %i", 1);
                   fprintf(fts, ")\n");
		}

               
              fprintf(fts,"(=(");
                 fprintf(fts, "IS_SATISFIED_%s)",current2->name);

              	if(all){
                   fprintf(fts, " %i", 1);
                   fprintf(fts, ")\n");
		}
                else{
                   fprintf(fts, " %i", 0);
                   fprintf(fts, ")\n");
		}

              
           /*second one and so on preferences
	   **/

      for ( i_son = current1->sons->next; i_son!=NULL; i_son = i_son->next ) {

          /* test, if initial  contains preference goal
	  **/
          all = is_State_meet_preference_expression(i_son);
               fprintf(fts,"(=(");
               fprintf(fts, "IS_VIOLATED_%s)",i_son->name);
		if(all){
                   fprintf(fts, " %i", 0);
                   fprintf(fts, ")\n");
		}
                else{
                   fprintf(fts, " %i", 1);
                   fprintf(fts, ")\n");
		}
             


               fprintf(fts,"(=(");
               fprintf(fts, "IS_SATISFIED_%s)",i_son->name);
		if(all){
                   fprintf(fts, " %i", 1);
                   fprintf(fts, ")\n");
		}
                else{
                   fprintf(fts, " %i", 0);
                   fprintf(fts, ")\n");
		}
             
      }
              
  }
 }

  

   for ( i = 0; i < gnum_relevant_fluents; i++ ) {
      fprintf(fts,"(=(");
      fprint_Fluent(fts, &(grelevant_fluents[i]) );
      fprintf(fts, ")");
      if ( ginitial_state.f_D[i] ) {
        fprintf(fts, "%.2f", ginitial_state.f_V[i]);
      } 
      fprintf(fts, ")\n");

  }




  /* value of pref preconds is-violated in initial
  **/
   if(isDomainPreference){
     for ( i = 0; i < gnum_operators; i++ ) {
        WffNode *i_son = NULL;

        if(goperators[i]->pref_preconds){
            if(goperators[i]->pref_preconds->name){
	       /* one pref_preconds*/
               fprintf(fts, "(=(IS_VIOLATED_%s) 0)\n",goperators[i]->pref_preconds->name);
           }
       else{
	   for (i_son = goperators[i]->pref_preconds->sons; i_son != NULL; i_son = i_son->next ){
              fprintf(fts, "(=(IS_VIOLATED_%s) 0)\n", i_son->name);

	   }
         }
      }
    }
   }




    if(isTempTimeWindows){

    Time_Ini_Literal *lauf = NULL;
    if(Timed_Initial_Literals)
     {
           
      for(lauf=Timed_Initial_Literals;lauf!=NULL;lauf=lauf->next)
       { 
         

         TimeWindows *lauf0=lauf->tw;
         while(lauf0)
         {
          fprintf(fts, "(at "); 
          fprintf(fts, " %.2f ",lauf0->mintime);
          fprintf(fts, "(");
          fprint_hidden_TokenList(fts, lauf->pre,"-");
          fprintf(fts, "))\n");

          fprintf(fts, "(at "); 
          fprintf(fts, " %.2f ",lauf0->maxtime);
          fprintf(fts, "(not (");
          fprint_hidden_TokenList(fts, lauf->pre,"-");
          fprintf(fts, ")))\n");
         
          lauf0=lauf0->next;
        }
              
      }
     }
    }

    /*for timed initial literal  Constraints
    **/
    if(isConstraints){

        if(gtil_constraints){
          WffNode *n = gtil_constraints;
          PlNode *m = til_constraints;
          if(m->connective==AND){
	      k1 = read_gtil_constraints(n, fts);
            }         
          else{
               read_gtil_constraints2(n,0, fts);
            }
	}
       }

    /*for timed initial literal Constraints preference
    **/
    if(isConstraintsPreference){

        if(gtil_constraints_pref){
          WffNode *n = gtil_constraints_pref;
          PlNode *m = til_constraints_pref;
          if(m->connective==AND){
	      k2 = read_gtil_constraints(n, fts);
            }         
          else{
               read_gtil_constraints2(n,0, fts);
            }
	}
        
       }

    /*end for timed initial literal constraints and constraints preference*/

    /*create ltl and til-fact for within constraints
    **/
    if(isConstraints){
        if(gwithin_constraints){
          printf("\n...Reading within constraints...\n");
          WffNode *n = gwithin_constraints;
          PlNode *m = within_constraints;
          if(m->connective==AND){
	      k3 = read_gwithin_constraints(n,s2, fts);
            }         
          else{
               read_gwithin_constraints2(n,0,s2, fts, TRUE, NULL);
            }
	}
       }

    /*create ltl and til-fact for within constraints preference
    **/
    if(isConstraintsPreference){
        printf("\n...Reading within constraints preference...\n");
        if(gwithin_constraints_pref){
          WffNode *n = gwithin_constraints_pref;
          PlNode *m = within_constraints_pref;

          if(m->connective==AND){
	      k4 = read_gwithin_constraints(n,s2, fts);
            }         
          else{
               read_gwithin_constraints2(n,0,s2, fts, TRUE, NULL);
            }
	}
        
       }
  

    /* for sync-ordinary
    **/
    if(gconstraints || gconstraints_pref || gwithin_constraints || gwithin_constraints_pref)
               fprintf(fts, "(sync-ordinary)\n");
    /* end for sync-ordinary*/


  fprintf(fts, ")\n");



  fprintf(fts, "(:goal\n");
  fprintf(fts, "(and\n");
  for ( i = 0; i < gnum_logic_goal; i++ ) {
    fprintf(fts, "(");
    fprint_ft_name(fts,glogic_goal[i] );
    fprintf(fts, ")\n");
  }

  for ( i = 0; i < gnum_numeric_goal; i++ ) {
      switch ( gnumeric_goal_comp[i] ) {
      case LE:
	fprintf(fts, "(< ");
	break;
      case LEQ:
	fprintf(fts, "(<= ");
	break;
      case EQ:
	fprintf(fts, "(= ");
	break;
      case GEQ:
	fprintf(fts, "(>= ");
	break;
      case GE:
	fprintf(fts, "(> ");
	break;
      default:
	printf("\nwrong comparator in gnumeric_goal %d\n\n", gnumeric_goal_comp[i]);
	exit( 1 );
      }
      fprint_ExpNode(fts,  gnumeric_goal_lh[i] );
      fprint_ExpNode(fts,  gnumeric_goal_rh[i] );
      fprintf(fts, ")\n");
    }


  fprintf(fts, ")\n");
  fprintf(fts, ")\n");

  
   if ( gparse_metric ) {
      if ( strcmp( gparse_optimization, "MAXIMIZE" ) == SAME )
            fprintf(fts, "(:metric maximize ");
      else
            fprintf(fts, "(:metric minimize ");
      change_ParseExpNode(gparse_metric, NULL);
      remove_not_found_IsViolated_from_ParseExpNode(gparse_metric, NULL);
      fprint_ParseExpNode(fts, gparse_metric);
      fprintf(fts,")\n");
    } 

  fprintf(fts, ")\n");
  fclose( fts);

  if ( (ops = fopen( "groundeddomain.pddl", "w")) == NULL ) {
    printf("\n\nCannot open file domain.pddl.\n\n");
    exit( 1 );
  }
  fprintf(ops, "(define (domain grounded-ADL-%s)\n", gdomain_name);
  fprintf(ops, "(:requirements\n");
  fprintf(ops, ":adl\n");
  


  fprintf(ops, ")\n");
  fprintf(ops, "(:predicates\n");
  
   for ( i = 0; i < gnum_relevant_facts; i++ ) {
      
    fprintf(ops, "(");
    fprint_Fact( ops, &(grelevant_facts[i]) );
    fprintf(ops, ")\n");
  }
 
    /* supplementar relevant fact
    **/

   relevant_gpref_predicate = (Fact_pointer *) calloc(MAX_PREDICATES, sizeof(Fact_pointer));
    gnum_gpref_predicate = 0;
 /* predicates for gpref_head
    **/
    if(gpref_head)
        generate_gpref_predicate(gpref_head);  

   /* predicates for gconstraints_pref
   **/
    if(gconstraints_pref)
        generate_gpref_predicate(gconstraints_pref);

    /* predicates for gwithin_constraints_pref
    **/
    if(gwithin_constraints_pref)
    generate_gpref_predicate(gwithin_constraints_pref);

    /* predicates for gtil_constraints_pref
    **/
    if(gtil_constraints_pref)
        generate_gpref_predicate(gtil_constraints_pref);
    
     for(i = 0; i < gnum_gpref_predicate; i++){
        fprintf(ops, "(");
        fprint_Fact(ops,relevant_gpref_predicate[i] );
	    fprintf(ops, ")\n");
     }    

    /* predicates for pref preconds
    **/
    if(isDomainPreference){
        Bool found1 = FALSE;
        Bool found2 = FALSE;
        for(i = 0; i < gnum_pref_preconds_predicate; i++){
	   /*++++++++++++++++++++*/
             for ( j = 0; j < gnum_relevant_facts; j++ ) {
                 if(same_Fact(&(grelevant_facts[j]),&(grelevant_facts[relevant_pref_preconds_predicate[i]]) )){
                     found1 = TRUE;
                     break;
		 }
	     }
	     if(!found1){
               for(k = 0; k < gnum_gpref_predicate; k++){
		  if(same_Fact(relevant_gpref_predicate[k],&(grelevant_facts[relevant_pref_preconds_predicate[i]]))){
		      found2 = TRUE;
                      break;
		  }
	       }
	     }
	       if((!found1) && (!found2)){
		     fprintf(ops, "(");
                     fprint_Fact(ops,&(grelevant_facts[relevant_pref_preconds_predicate[i]]) );
	             fprintf(ops, ")\n");
		      		
	     }     
           /*++++++++++++++++++++*/
        }  
       }
    /* end of supplementar relevant fact  */
  
    /*predicates for timed initial literal from Constraints
    **/

    if(isConstraints){
        char s[40];
        if(gtil_constraints){
              for ( i = 0; i < k1; i++ ) {
                sprintf(s,"%d", i);
                fprintf(ops, "(fact-%s)\n", s);
                fprintf(ops, "(done-%s)\n", s);
               }
             }
      }

    /*predicates for timed initial literal Constraints preference
    **/
    if(isConstraintsPreference){
        if(gtil_constraints_pref){
           WffNode *n = gtil_constraints_pref;
           PlNode *m = til_constraints_pref;
          if(m->connective==AND){
	       add_til_and_within_constraints_pref_fact_to_predicates(n,s1,ops);
            }         
          else{
               add_til_and_within_constraints_pref_fact_to_predicates2(n,0,s1,ops);
            }    
	}
       }

   
     /* end for timed initial literal constraints and constraints preference */

    /*predicates for within Constraints
    **/
    if(isConstraints){
        char s[40];
        if(gwithin_constraints){
              for ( i = 0; i < k3; i++ ) {
                sprintf(s,"wfact-%d", i);
                fprintf(ops, "(%s)\n", s);
               }
             }
      }

    /*predicates for within  Constraints preference
    **/
    if(isConstraintsPreference){
        if(gwithin_constraints_pref){
           WffNode *n = gwithin_constraints_pref;
           PlNode *m = within_constraints_pref;
          if(m->connective==AND){
	       add_til_and_within_constraints_pref_fact_to_predicates(n,s1,ops);
            }         
          else{
               add_til_and_within_constraints_pref_fact_to_predicates2(n,0,s1,ops);
            }    
	}
       }     
    /* end for within constraints and constraints preference */


    
    if(isTempTimeWindows){

	int m = 0;
        while(twpredicate[m]!=NULL){  
        fprintf(ops, "(");
	fprint_hidden_TokenList(ops, twpredicate[m], "-");
        fprintf(ops, ")\n");
        m++;

	}     
     }



  fprintf(ops, ")\n");


 if(gnum_relevant_fluents || isPreference || isConstraintsPreference) {
    fprintf(ops, "(:functions\n");
  

    for ( i = 0; i < gnum_relevant_fluents; i++ ) {
       
    fprintf(ops, "(");
    fprint_Fluent( ops, &(grelevant_fluents[i]) );
    fprintf(ops, ")\n");
  }

   /* add is_violated preference name from pref_preconds
   **/
   int i;
   for ( i = 0; i < gnum_operators; i++ ) {
    WffNode *i_son = NULL;

    if(goperators[i]->pref_preconds){

        if(goperators[i]->pref_preconds->name){
	    /* one pref_preconds*/
            fprintf(ops, "(IS_VIOLATED_%s)\n",goperators[i]->pref_preconds->name);
	}
    else{
	for (i_son = goperators[i]->pref_preconds->sons; i_son != NULL; i_son = i_son->next ){
            fprintf(ops, "(IS_VIOLATED_%s)\n", i_son->name);

	}
      }
    }
   }


  /*for preferences from goal
  **/
  if(isPreference || gpref_head){
    WffNode *current = gpref_head;
    WffNode *i_son;

      if ( current->sons ) {
           fprintf(ops, "(");
           fprintf(ops, "IS_VIOLATED_%s",current->sons->name);
           fprintf(ops, ")\n");

           fprintf(ops, "(");
           fprintf(ops, "IS_SATISFIED_%s",current->sons->name);
           fprintf(ops, ")\n");

      for ( i_son = current->sons->next; i_son!=NULL; i_son = i_son->next ) {
	   fprintf(ops, "(");
           fprintf(ops, "IS_VIOLATED_%s",i_son->name);
           fprintf(ops, ")\n");

           fprintf(ops, "(");
           fprintf(ops, "IS_SATISFIED_%s",i_son->name);
           fprintf(ops, ")\n");
      }
    }
  }
  /*add is-violated-pref for timed initial literal constraints preference
  * in function
  **/
  if(isConstraintsPreference){
      if(gtil_constraints_pref){

           WffNode *n = gtil_constraints_pref;
           PlNode *m = til_constraints_pref;
          if(m->connective==AND){
	       add_is_violated_til_constraints_pref_to_function(n,ops);
            }         
          else{
               add_is_violated_til_constraints_pref_to_function2(n,0,ops);
            }    
      }
  }


  fprintf(ops, ")\n");
  }



  /* actions
   */
    for ( a = gactions; a; a = a->next ) {
       if (a->norm_operator || a->pseudo_action ){
            fprint_ADL_Action( ops, a );
       }
   }
 

 fprintf(ops, ")\n");
 fclose( ops);

}


