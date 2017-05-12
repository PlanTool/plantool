
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
 * File: backdoors.c
 * Description: functions for cnf output
 *
 * Author: Joerg Hoffmann 2003
 *  Modified: Shane Neph 2004 and 2006
 *********************************************************************/ 










#include "bb.h"

#include "output.h"
#include "memory.h"

#include "instantiateI.h"
#include "instantiateII.h"

#include "graph.h"

#include "cnfout.h"






 




int **lcode;

int **lclause;
int *lclause_size;
int lnum_clauses;

int *lop_to_code;
int *ltime_to_code;
extern const int UNSAT;
extern const int SAT;


void do_cnf_output(int create)

{
    if ( gcmd_line.cnfout == 1 ) {
	print_action_based_encoding( gcmd_line.cnflayer, create );
    return;
    }

    if ( gcmd_line.cnfout == 2 ) {
	print_gpstyle_action_based_encoding( gcmd_line.cnflayer, create );
    return;
    }

    if ( gcmd_line.cnfout == 3 ) {
	print_gp_based_encoding( gcmd_line.cnflayer, create );
    return;
    }

    if ( gcmd_line.cnfout == 4 ) {
	print_thin_gp_based_encoding( gcmd_line.cnflayer, create );
    return;
    }

    printf("\n\nEXIT: cnf out %d not implemented\n\n", gcmd_line.cnfout);
    exit( 1 );
}




















/* action-based
 */
void print_action_based_encoding( int layer, int create )

{
    FILE *CNF, *VARFILE;


    int i, j, t, k, ft, op, prevcl, l;
    int *F, *A;
    int code;
    int tmpNumber;

    int nGclauses = 0, nAclauses = 0, nEclauses = 0;



    lclause = ( int ** ) calloc( MAX_CLAUSES, sizeof( int * ) );
    lclause_size = ( int * ) calloc( MAX_CLAUSES, sizeof( int ) );
    lcode = ( int ** ) calloc( layer, sizeof( int * ) );
    lop_to_code = ( int * ) calloc( MAX_CNF_VARS, sizeof( int ) );
    ltime_to_code = ( int * ) calloc( MAX_CNF_VARS, sizeof( int ) );
    for ( i = 0; i < layer; i++ ) {
	lcode[i] = ( int * ) calloc( gnum_op_conn, sizeof( int ) );
	for ( j = 0; j < gnum_op_conn; j++ ) {
	    lcode[i][j] = -1;
	}
    }


    F  = ( int * ) calloc( gnum_ft_conn, sizeof( int ) );
    for ( i = 0; i < gnum_ft_conn; i++ ) {
	F[i] = -1;
    }
    /* no-ops are included in the gop_conn!
     */
    A  = ( int * ) calloc( gnum_op_conn, sizeof( int ) );
    for ( i = 0; i < gnum_op_conn; i++ ) {
	A[i] = -1;
    }




    /* layer 0
     */
    for ( i = 0; i < ginitial_state.num_F; i++ ) {
	F[ginitial_state.F[i]] = 0;
    }


    printf("\n\ncreating action-based encoding...");

    /* prec constraints, + code assignment.
     */
    lnum_clauses = 0;
    code = 1; /* variables numbered 1 .. n */
    printf("\nbuilding rplan graph layers --> up to layer %d...", layer);
    for ( t = 0; t < layer; t++ ) {	
	for ( i = 0; i < gnum_op_conn; i++ ) {
	    if ( A[i] == -1 ) {
		for ( j = 0; j < gop_conn[i].num_P; j++ ) {
		    if ( F[gop_conn[i].P[j]] == -1 ||
			 F[gop_conn[i].P[j]] > t ) break;
		}
		if ( j < gop_conn[i].num_P ) continue;
		/* now i is an op that comes in here.
		 */
		A[i] = t;
		if ( 0 ) {
		    printf("\nA %d at %d", i, t);
		}
		/* mark its add effects.
		 */
		for ( j = 0; j < gop_conn[i].num_A; j++ ) {
		    ft =  gop_conn[i].A[j];
		    if ( F[ft] != -1 ) continue;
		    F[ft] = t+1;
		}
	    } 

	    /* insert prec clauses, if at t > 0.
	     */
	    if ( t == 0 ) {
		continue;
	    }
	    for ( j = 0; j < gop_conn[i].num_P; j++ ) {
		ft =  gop_conn[i].P[j];

		if ( lnum_clauses == MAX_CLAUSES ) {
		    printf("\n\nEXIT: too many clauses. increase MAX_CLAUSES.\n\n");
		    exit( 1 );
		}
		lclause[lnum_clauses] = ( int * ) calloc( gft_conn[ft].num_A+1, sizeof( int ) );
		/* here the op i is used at t, for the 1st time. assign the code.
		 */
		if ( j == 0 ) {
		    if ( lcode[t][i] != -1 ) {
			printf("\n\nEXIT: op code %d at %d already assigned??\n\n", i, t);
			exit( 1 );
		    }
		    if ( 0 ) {
			printf("\nC %d at %d", i, t);
		    }
		    lcode[t][i] = code++;
		    if ( code == MAX_CNF_VARS + 1 ) {
			printf("\n\nEXIT: too many cnf vars. increase MAX_CNF_VARS.\n\n");
			exit( 1 );
		    }
		    lop_to_code[code-1] = i;
		    ltime_to_code[code-1] = t;
		}

		lclause[lnum_clauses][0] = (-1) * lcode[t][i];
		lclause_size[lnum_clauses] = 1;
		for ( k = 0; k < gft_conn[ft].num_A; k++ ) {
		    op = gft_conn[ft].A[k];
		    if ( A[op] == -1 || A[op] > t-1 ) {
			continue;
		    }
		    if ( lcode[t-1][op] == -1 ) {
			lcode[t-1][op] = code++;
			if ( 0 ) {
			    printf("\npC %d at %d", op, t-1);
			}
			if ( code == MAX_CNF_VARS + 1 ) {
			    printf("\n\nEXIT: too many cnf vars. increase MAX_CNF_VARS.\n\n");
			    exit( 1 );
			}
			lop_to_code[code-1] = op;
			ltime_to_code[code-1] = t-1;
		    }
		    lclause[lnum_clauses][lclause_size[lnum_clauses]++] = lcode[t-1][op];
		}
		if ( lclause_size[lnum_clauses] == 1 ) {
		    printf("\n\nEXIT: no achiever in at t>0??\n\n");
		    exit( 1 );
		}
		nAclauses++;
		lnum_clauses++;
	    } /* pres of i-op */
	} /* i -- op */
    } /* t */


    /* goal constraints
     */
    printf("\ngoal constraints...");
    for ( i = 0; i < ggoal_state.num_F; i++ ) {
	ft = ggoal_state.F[i];

	if ( lnum_clauses == MAX_CLAUSES ) {
	    printf("\n\nEXIT: too many clauses. increase MAX_CLAUSES.\n\n");
	    exit( 1 );
	}
	lclause[lnum_clauses] = ( int * ) calloc( gft_conn[ft].num_A, sizeof( int ) );
	lclause_size[lnum_clauses] = 0;
	for ( k = 0; k < gft_conn[ft].num_A; k++ ) {
	    op = gft_conn[ft].A[k];
	    if ( A[op] == -1 || A[op] > layer-1 ) {
		continue;
	    }
	    if ( lcode[layer-1][op] == -1 ) {
		lcode[layer-1][op] = code++;
		if ( code == MAX_CNF_VARS + 1 ) {
		    printf("\n\nEXIT: too many cnf vars. increase MAX_CNF_VARS.\n\n");
		    exit( 1 );
		}
		lop_to_code[code-1] = op;
		ltime_to_code[code-1] = layer-1;
	    }
	    lclause[lnum_clauses][lclause_size[lnum_clauses]++] = lcode[layer-1][op];
	}
	if ( lclause_size[lnum_clauses] == 0 ) {
	    printf("\n\nno achiever in for goal?? deadline too low!\n\n");
	    exit(UNSAT);
	}
	lnum_clauses++;
	nGclauses++;
    } /* goals */


    /* exclusion constraints. implementation a bit smart, to avoid
     * time-consuming all-op-pairs-check.
     */
	printf("\nbuilding exclusion constraints --> up to layer %d...", layer);
    for ( t = 0; t < layer; t++ ) {
	for ( i = 0; i < gnum_op_conn; i++ ) {
	    if ( A[i] == -1 || A[i] > t ) continue;
	    prevcl = lnum_clauses;
	    for ( j = 0; j < gop_conn[i].num_D; j++ ) {
		ft = gop_conn[i].D[j];
		for ( k = 0; k < gft_conn[ft].num_P; k++ ) {
		    op = gft_conn[ft].P[k];
		    if ( op <= i ) continue; /* only in one of the two dirs we'll get */
		    if ( A[op] == -1 || A[op] > t ) continue;
		    /* did we make op excl of i already?
		     */
		    if ( lcode[t][op] != -1 ) {
			for ( l = prevcl; l < lnum_clauses; l++ ) {
			    if ( lclause[l][1] == (-1) * lcode[t][op] ) break;
			}
			if ( l < lnum_clauses ) continue; /* yes. */
		    }
		    /* record the clause.
		     */
		    if ( lnum_clauses == MAX_CLAUSES ) {
			printf("\n\nEXIT: too many clauses. increase MAX_CLAUSES.\n\n");
			exit( 1 );
		    }
		    lclause[lnum_clauses] = ( int * ) calloc( 2, sizeof( int ) );
		    if ( lcode[t][i] == -1 ) {
			lcode[t][i] = code++;
			if ( code == MAX_CNF_VARS + 1 ) {
			    printf("\n\nEXIT: too many cnf vars. increase MAX_CNF_VARS.\n\n");
			    exit( 1 );
			}
			lop_to_code[code-1] = i;
			ltime_to_code[code-1] = t;
		    }
		    lclause[lnum_clauses][0] = (-1) * lcode[t][i];
		    if ( lcode[t][op] == -1 ) {
			lcode[t][op] = code++;
			if ( code == MAX_CNF_VARS + 1 ) {
			    printf("\n\nEXIT: too many cnf vars. increase MAX_CNF_VARS.\n\n");
			    exit( 1 );
			}
			lop_to_code[code-1] = op;
			ltime_to_code[code-1] = t;
		    }
		    lclause[lnum_clauses][1] = (-1) * lcode[t][op];
		    lclause_size[lnum_clauses] = 2;
		    lnum_clauses++;
		    nEclauses++;
		} /* k: ops that ft is pre of */
		for ( k = 0; k < gft_conn[ft].num_A; k++ ) {
		    op = gft_conn[ft].A[k];
		    if ( op <= i ) continue; /* only in one of the two dirs we'll get */
		    if ( A[op] == -1 || A[op] > t ) continue;
		    /* did we make op excl of i already?
		     */
		    if ( lcode[t][op] != -1 ) {
			for ( l = prevcl; l < lnum_clauses; l++ ) {
			    if ( lclause[l][1] == (-1) * lcode[t][op] ) break;
			}
			if ( l < lnum_clauses ) continue; /* yes. */
		    }
		    /* record the clause.
		     */
		    if ( lnum_clauses == MAX_CLAUSES ) {
			printf("\n\nEXIT: too many clauses. increase MAX_CLAUSES.\n\n");
			exit( 1 );
		    }
		    lclause[lnum_clauses] = ( int * ) calloc( 2, sizeof( int ) );
		    if ( lcode[t][i] == -1 ) {
			lcode[t][i] = code++;
			if ( code == MAX_CNF_VARS + 1 ) {
			    printf("\n\nEXIT: too many cnf vars. increase MAX_CNF_VARS.\n\n");
			    exit( 1 );
			}
			lop_to_code[code-1] = i;
			ltime_to_code[code-1] = t;
		    }
		    lclause[lnum_clauses][0] = (-1) * lcode[t][i];
		    if ( lcode[t][op] == -1 ) {
			lcode[t][op] = code++;
			if ( code == MAX_CNF_VARS + 1 ) {
			    printf("\n\nEXIT: too many cnf vars. increase MAX_CNF_VARS.\n\n");
			    exit( 1 );
			}
			lop_to_code[code-1] = op;
			ltime_to_code[code-1] = t;
		    }
		    lclause[lnum_clauses][1] = (-1) * lcode[t][op];
		    lclause_size[lnum_clauses] = 2;
		    lnum_clauses++;
		    nEclauses++;
		} /* k: ops that ft is added by */
	    } /* j: fts that i dels */


	    for ( j = 0; j < gop_conn[i].num_P; j++ ) {
		ft = gop_conn[i].P[j];
		for ( k = 0; k < gft_conn[ft].num_D; k++ ) {
		    op = gft_conn[ft].D[k];
		    if ( op <= i ) continue; /* only in one of the two dirs we'll get */
		    if ( A[op] == -1 || A[op] > t ) continue;
		    /* did we make op excl of i already?
		     */
		    if ( lcode[t][op] != -1 ) {
			for ( l = prevcl; l < lnum_clauses; l++ ) {
			    if ( lclause[l][1] == (-1) * lcode[t][op] ) break;
			}
			if ( l < lnum_clauses ) continue; /* yes. */
		    }
		    /* record the clause.
		     */
		    if ( lnum_clauses == MAX_CLAUSES ) {
			printf("\n\nEXIT: too many clauses. increase MAX_CLAUSES.\n\n");
			exit( 1 );
		    }
		    lclause[lnum_clauses] = ( int * ) calloc( 2, sizeof( int ) );
		    if ( lcode[t][i] == -1 ) {
			lcode[t][i] = code++;
			if ( code == MAX_CNF_VARS + 1 ) {
			    printf("\n\nEXIT: too many cnf vars. increase MAX_CNF_VARS.\n\n");
			    exit( 1 );
			}
			lop_to_code[code-1] = i;
			ltime_to_code[code-1] = t;
		    }
		    lclause[lnum_clauses][0] = (-1) * lcode[t][i];
		    if ( lcode[t][op] == -1 ) {
			lcode[t][op] = code++;
			if ( code == MAX_CNF_VARS + 1 ) {
			    printf("\n\nEXIT: too many cnf vars. increase MAX_CNF_VARS.\n\n");
			    exit( 1 );
			}
			lop_to_code[code-1] = op;
			ltime_to_code[code-1] = t;
		    }
		    lclause[lnum_clauses][1] = (-1) * lcode[t][op];
		    lclause_size[lnum_clauses] = 2;
		    lnum_clauses++;
		    nEclauses++;
		} /* k: ops that ft is del by */
	    } /* j: fts that i has as prec */

	    for ( j = 0; j < gop_conn[i].num_A; j++ ) {
		ft = gop_conn[i].A[j];
		for ( k = 0; k < gft_conn[ft].num_D; k++ ) {
		    op = gft_conn[ft].D[k];
		    if ( op <= i ) continue; /* only in one of the two dirs we'll get */
		    if ( A[op] == -1 || A[op] > t ) continue;
		    /* did we make op excl of i already?
		     */
		    if ( lcode[t][op] != -1 ) {
			for ( l = prevcl; l < lnum_clauses; l++ ) {
			    if ( lclause[l][1] == (-1) * lcode[t][op] ) break;
			}
			if ( l < lnum_clauses ) continue; /* yes. */
		    }
		    /* record the clause.
		     */
		    if ( lnum_clauses == MAX_CLAUSES ) {
			printf("\n\nEXIT: too many clauses. increase MAX_CLAUSES.\n\n");
			exit( 1 );
		    }
		    lclause[lnum_clauses] = ( int * ) calloc( 2, sizeof( int ) );
		    if ( lcode[t][i] == -1 ) {
			lcode[t][i] = code++;
			if ( code == MAX_CNF_VARS + 1 ) {
			    printf("\n\nEXIT: too many cnf vars. increase MAX_CNF_VARS.\n\n");
			    exit( 1 );
			}
			lop_to_code[code-1] = i;
			ltime_to_code[code-1] = t;
		    }
		    lclause[lnum_clauses][0] = (-1) * lcode[t][i];
		    if ( lcode[t][op] == -1 ) {
			lcode[t][op] = code++;
			if ( code == MAX_CNF_VARS + 1 ) {
			    printf("\n\nEXIT: too many cnf vars. increase MAX_CNF_VARS.\n\n");
			    exit( 1 );
			}
			lop_to_code[code-1] = op;
			ltime_to_code[code-1] = t;
		    }
		    lclause[lnum_clauses][1] = (-1) * lcode[t][op];
		    lclause_size[lnum_clauses] = 2;
		    lnum_clauses++;
		    nEclauses++;
		} /* k: ops that ft is del by */
	    } /* j: fts that i adds */
	} /* i: ops at t */
    } /* t */ 

    /* that's it. print CNF file.
     */
    if ( gcmd_line.debug ) {
	if ( (VARFILE = fopen( gcmd_line.varFileName, "w" )) == NULL ) {
	    printf("\n\nEXIT: can't open VARFILE file.\n\n");
	    exit( 1 );
	}
	fprintf(VARFILE, "\n\nc DECISION LAYER %d, ACTION-BASED ENCODING.", layer);
	fprintf(VARFILE, "\n\nc VARS:");

        
        for ( i = 1; i < code; i++ ) {
            fprintf(VARFILE, "\nc var %2d action (", i);
  	    print_op_nameToFile( lop_to_code[i], VARFILE );
	    fprintf(VARFILE, ") %d", ltime_to_code[i] );
	} 
/*
	fprintf(VARFILE, "\n\nCLAUSES:");
	for ( i = 0; i < lnum_clauses; i++ ) {
   	    for ( j = 0; j < lclause_size[i]; j++ ) {
		if ( lclause[i][j] > 0 ) {
                    fprintf(VARFILE, "\n%6d - ", i);
		    fprintf(VARFILE, "(");
		    print_op_nameToFile( lop_to_code[lclause[i][j]], VARFILE );
		    fprintf(VARFILE, " at %d) VAR = %d", ltime_to_code[lclause[i][j]], lclause[i][j] );
	        } else {
                    fprintf(VARFILE, "\n%6d - ", i);
		    fprintf(VARFILE, "(NOT ");
		    print_op_nameToFile( lop_to_code[(-1)*lclause[i][j]], VARFILE );
		    fprintf(VARFILE, " at %d) VAR = %d", ltime_to_code[(-1)*lclause[i][j]], lclause[i][j] );
		}
	    }
	}
	fprintf(VARFILE, "0\n");
*/
        fclose(VARFILE);
    }

    printf("\n\nDECISION LAYER %d, WRITING ACTION-BASED ENCODING WITH %d VARS, %d CLAUSES.", 
	   layer, code-1, lnum_clauses);
    printf("\n%d G clauses, %d A clauses, %d E clauses.", 
	   nGclauses, nAclauses, nEclauses);
 
    if ( create == 0 ) {
      if ( (CNF = fopen( gcmd_line.cnfFileName, "w" )) == NULL ) {
	  printf("\n\nEXIT: can't open CNF file.\n\n");
	  exit( 1 );
      }

      fprintf(CNF, "c CNF file planning task -p %s, -o %s, -f %s, bound %d, action based encoding\n",
	        gcmd_line.path, gcmd_line.ops_file_name, gcmd_line.fct_file_name, layer);
      printf("\nb-check %d\n", gcmd_line.binary_clause_only);
      tmpNumber = lnum_clauses;
      if ( gcmd_line.binary_clause_only != 0 ) {
          for ( i = 0; i < tmpNumber; ++i ) {
              if ( lclause_size[i] > 2 )
                  --lnum_clauses;
          }
      }
      fprintf(CNF, "p cnf %d %d\n", code-1, lnum_clauses);
      for ( i = 0; i < tmpNumber; i++ ) {
        if ( gcmd_line.binary_clause_only == 0 || lclause_size[i] < 3 ) {
	  for ( j = 0; j < lclause_size[i]; j++ ) {
	    fprintf(CNF, "%d ", lclause[i][j]);
	  }
	  fprintf(CNF, "0\n");
        }
      }
      fclose(CNF);
    }
}

























/* gp-style action-based
 */

void print_gpstyle_action_based_encoding( int layer, int create )
{

    FILE *VARFILE, *CNF;

    int i, j, t, ft, k, op, code;
    IntList *tmp, *i1, *i2;

    lclause = ( int ** ) calloc( MAX_CLAUSES, sizeof( int * ) );
    lclause_size = ( int * ) calloc( MAX_CLAUSES, sizeof( int ) );
    lcode = ( int ** ) calloc( layer, sizeof( int * ) );
    lop_to_code = ( int * ) calloc( MAX_CNF_VARS, sizeof( int ) );
    ltime_to_code = ( int * ) calloc( MAX_CNF_VARS, sizeof( int ) );
    for ( i = 0; i < layer; i++ ) {
	lcode[i] = ( int * ) calloc( gnum_op_conn, sizeof( int ) );
	for ( j = 0; j < gnum_op_conn; j++ ) {
	    lcode[i][j] = -1;
	}
    }


    /* build the graph up to <layer>
     */
    for ( i = 0; i < gnum_op_conn; i++ ) {
	tmp = new_IntList( i );
	if ( gout_ops ) {
	    gout_ops->prev = tmp;
	}
	tmp->next = gout_ops;
	gout_ops = tmp;
    }
    for ( i = 0; i < ginitial_state.num_F; i++ ) {
	ft = ginitial_state.F[i];
	gin_ft_count++;
	gft_conn[ft].first_appearance = 0;
	gft_conn[ft].info_at[0] = new_FtLevelInfo();
	tmp = new_IntList( ft );
	tmp->next = gin_fts;
	gin_fts = tmp;
    }
    gin_fts_at[0] = gin_fts;
    if ( gcmd_line.display_info ) {
	printf("\ntime: %3d, %5d facts and %7d exclusive pairs",
	       0, gin_ft_count, gin_ft_exclusion_count);
	fflush(stdout);
    }
    for( t = 0; t < layer; t++ ) {
	build_graph_evolution_step();
    }

    printf("\n\ncreating gp-style action-based encoding...");

    /* prec constraints, + code assignment.
     */
    lnum_clauses = 0;
    code = 1; /* variables numbered 1 .. n */
    for ( t = 0; t < layer; t++ ) {
	printf("\nplan graph layer %d...", t);
	for ( i = 0; i < gnum_op_conn; i++ ) {
	    if ( gop_conn[i].info_at[t] == NULL /* equiv to not in here */ ) {
		continue;
	    }
	    /* insert prec clauses, if at t > 0.
	     */
	    if ( t == 0 ) {
		continue;
	    }
	    for ( j = 0; j < gop_conn[i].num_P; j++ ) {
		ft =  gop_conn[i].P[j];

		if ( lnum_clauses == MAX_CLAUSES ) {
		    printf("\n\ntoo many clauses. increase MAX_CLAUSES.\n\n");
		    exit( 1 );
		}
		lclause[lnum_clauses] = ( int * ) calloc( gft_conn[ft].num_A+1, sizeof( int ) );
		/* here the op i is used at t, for the 1st time. assign the code.
		 */
		if ( j == 0 ) {
		    if ( lcode[t][i] != -1 ) {
			printf("\n\nop code %d at %d already assigned??\n\n", i, t);
			exit( UNSAT );
		    }
		    if ( 0 ) {
			printf("\nC %d at %d", i, t);
		    }
		    lcode[t][i] = code++;
		    if ( code == MAX_CNF_VARS + 1 ) {
			printf("\n\ntoo many cnf vars. increase MAX_CNF_VARS.\n\n");
			exit( 1 );
		    }
		    lop_to_code[code-1] = i;
		    ltime_to_code[code-1] = t;
		}

		lclause[lnum_clauses][0] = (-1) * lcode[t][i];
		lclause_size[lnum_clauses] = 1;
		for ( k = 0; k < gft_conn[ft].num_A; k++ ) {
		    op = gft_conn[ft].A[k];
		    if ( gop_conn[op].info_at[t-1] == NULL ) {
			continue;
		    }
		    if ( lcode[t-1][op] == -1 ) {
			lcode[t-1][op] = code++;
			if ( 0 ) {
			    printf("\npC %d at %d", op, t-1);
			}
			if ( code == MAX_CNF_VARS + 1 ) {
			    printf("\n\ntoo many cnf vars. increase MAX_CNF_VARS.\n\n");
			    exit( 1 );
			}
			lop_to_code[code-1] = op;
			ltime_to_code[code-1] = t-1;
		    }
		    lclause[lnum_clauses][lclause_size[lnum_clauses]++] = lcode[t-1][op];
		}
		if ( lclause_size[lnum_clauses] == 1 ) {
		    printf("\n\nno achiever in at t>0??\n\n");
		    exit( UNSAT );
		}
		lnum_clauses++;
	    } /* pres of i-op */
	} /* i -- op */
    } /* t */


    /* goal constraints
     */
    printf("\ngoal constraints...");
    for ( i = 0; i < ggoal_state.num_F; i++ ) {
	ft = ggoal_state.F[i];

	if ( lnum_clauses == MAX_CLAUSES ) {
	    printf("\n\ntoo many clauses. increase MAX_CLAUSES.\n\n");
	    exit( 1 );
	}
	lclause[lnum_clauses] = ( int * ) calloc( gft_conn[ft].num_A, sizeof( int ) );
	lclause_size[lnum_clauses] = 0;
	for ( k = 0; k < gft_conn[ft].num_A; k++ ) {
	    op = gft_conn[ft].A[k];
	    if ( gop_conn[op].info_at[layer-1] == NULL ) {
		continue;
	    }
	    if ( lcode[layer-1][op] == -1 ) {
		lcode[layer-1][op] = code++;
		if ( code == MAX_CNF_VARS + 1 ) {
		    printf("\n\ntoo many cnf vars. increase MAX_CNF_VARS.\n\n");
		    exit( 1 );
		}
		lop_to_code[code-1] = op;
		ltime_to_code[code-1] = layer-1;
	    }
	    lclause[lnum_clauses][lclause_size[lnum_clauses]++] = lcode[layer-1][op];
	}
	if ( lclause_size[lnum_clauses] == 0 ) {
	    printf("\n\nno achiever in for goal?? deadline too low!\n\n");
	    exit(UNSAT);
	}
	lnum_clauses++;
    } /* goals */




    /* exclusion constraints.
     */
    for ( t = 0; t < layer; t++ ) {
	printf("\nexclusion constraints layer %d...", t);
	for ( i1 = gin_ops_at[t]; i1 && i1->next; i1 = i1->next ) {
	    i = i1->i1;
	    for ( i2 = i1->next; i2; i2 = i2->next ) {
		j = i2->i1;
		if ( !(gop_conn[i].info_at[t]->bit_exclusives[gop_conn[j].uid_block] &
		       gop_conn[j].uid_mask) ) {
		    continue;
		}
		if ( lnum_clauses == MAX_CLAUSES ) {
		    printf("\n\ntoo many clauses. increase MAX_CLAUSES.\n\n");
		    exit( 1 );
		}
		lclause[lnum_clauses] = ( int * ) calloc( 2, sizeof( int ) );
		if ( lcode[t][i] == -1 ) {
		    lcode[t][i] = code++;
		    if ( code == MAX_CNF_VARS + 1 ) {
			printf("\n\ntoo many cnf vars. increase MAX_CNF_VARS.\n\n");
			exit( 1 );
		    }
		    lop_to_code[code-1] = i;
		    ltime_to_code[code-1] = t;
		}
		lclause[lnum_clauses][0] = (-1) * lcode[t][i];
		if ( lcode[t][j] == -1 ) {
		    lcode[t][j] = code++;
		    if ( code == MAX_CNF_VARS + 1 ) {
			printf("\n\ntoo many cnf vars. increase MAX_CNF_VARS.\n\n");
			exit( 1 );
		    }
		    lop_to_code[code-1] = j;
		    ltime_to_code[code-1] = t;
		}
		lclause[lnum_clauses][1] = (-1) * lcode[t][j];
		lclause_size[lnum_clauses] = 2;
		lnum_clauses++;
	    }
	}
    }


    /* that's it; print CNF file. */
    if ( gcmd_line.debug ) {
        /* debugging: print semantic version of clause set.
         */

        if ( (VARFILE = fopen( gcmd_line.varFileName, "w" )) == NULL ) {
            printf("\n\nEXIT: can't open VARFILE file.\n\n");
            exit( 1 );
        }
        fprintf(VARFILE, "\n\nc DECISION LAYER %d, GP-STYLE ACTION-BASED ENCODING.", layer);
        fprintf(VARFILE, "\n\nc VARS:");

        for ( i = 1; i < code; i++ ) {
            fprintf(VARFILE, "\nc var %6d action (", i);
            print_op_nameToFile( lop_to_code[i], VARFILE );
            fprintf(VARFILE, ") %d", ltime_to_code[i] );
        }

        fclose(VARFILE);
    }

    printf("\n\nDECISION LAYER %d, WRITING GP-STYLE ACTION-BASED ENCODING WITH %d VARS, %d CLAUSES.", 
	   layer, code-1, lnum_clauses);


    if ( create == 0 ) { 
        if ( (CNF = fopen( gcmd_line.cnfFileName, "w" )) == NULL ) {
	    printf("\n\ncan't open CNF file.\n\n");
	    exit( 1 );
        }

        fprintf(CNF, "c CNF file planning task -p %s, -o %s, -f %s, bound %d, gp-style action-based encoding\n",
	        gcmd_line.path, gcmd_line.ops_file_name, gcmd_line.fct_file_name, layer);

        fprintf(CNF, "p cnf %d %d\n", code-1, lnum_clauses);
        for ( i = 0; i < lnum_clauses; i++ ) {
	  for ( j = 0; j < lclause_size[i]; j++ ) {
 	    fprintf(CNF, "%d ", lclause[i][j]);
          }
	  fprintf(CNF, "0\n");
        }
        fclose(CNF);
    }
}


















/* gp-based
 */

void print_gp_based_encoding( int layer, int create )

{

    FILE *VARFILE, *CNF;

    int i, j, t, ft, op, code;
    IntList *tmp, *i1, *i2;

    /* have to separate the ft coding out from the op coding.
     */
    int **fcode;
    int *fop_to_code;
    int *ftime_to_code;



    lclause = ( int ** ) calloc( MAX_CLAUSES, sizeof( int * ) );
    lclause_size = ( int * ) calloc( MAX_CLAUSES, sizeof( int ) );
    lcode = ( int ** ) calloc( layer, sizeof( int * ) );
    lop_to_code = ( int * ) calloc( MAX_CNF_VARS, sizeof( int ) );
    ltime_to_code = ( int * ) calloc( MAX_CNF_VARS, sizeof( int ) );
    for ( i = 0; i < layer; i++ ) {
	lcode[i] = ( int * ) calloc( gnum_op_conn, sizeof( int ) );
	for ( j = 0; j < gnum_op_conn; j++ ) {
	    lcode[i][j] = -1;
	}
    }
    fcode = ( int ** ) calloc( layer + 1, sizeof( int * ) );
    fop_to_code = ( int * ) calloc( MAX_CNF_VARS, sizeof( int ) );
    ftime_to_code = ( int * ) calloc( MAX_CNF_VARS, sizeof( int ) );
    for ( i = 0; i <= layer; i++ ) {
	fcode[i] = ( int * ) calloc( gnum_ft_conn, sizeof( int ) );
	for ( j = 0; j < gnum_ft_conn; j++ ) {
	    fcode[i][j] = -1;
	}
    }




    /* build the graph up to <layer>
     */
    for ( i = 0; i < gnum_op_conn; i++ ) {
	tmp = new_IntList( i );
	if ( gout_ops ) {
	    gout_ops->prev = tmp;
	}
	tmp->next = gout_ops;
	gout_ops = tmp;
    }
    for ( i = 0; i < ginitial_state.num_F; i++ ) {
	ft = ginitial_state.F[i];
	gin_ft_count++;
	gft_conn[ft].first_appearance = 0;
	gft_conn[ft].info_at[0] = new_FtLevelInfo();
	tmp = new_IntList( ft );
	tmp->next = gin_fts;
	gin_fts = tmp;
    }
    gin_fts_at[0] = gin_fts;
    if ( gcmd_line.display_info ) {
	printf("\ntime: %3d, %5d facts and %7d exclusive pairs",
	       0, gin_ft_count, gin_ft_exclusion_count);
	fflush(stdout);
    }
    for( t = 0; t < layer; t++ ) {
	build_graph_evolution_step();
	fflush(stdout);
    }


    printf("\n\ncreating gp-based encoding...");

    /* prec constraints, + code assignment.
     */
    lnum_clauses = 0;
    code = 1; /* variables numbered 1 .. n */
    for ( t = 0; t <= layer; t++ ) {
	printf("\nplan graph layer %d...", t);
	for ( i = 0; i < gnum_ft_conn; i++ ) {
	    if ( gft_conn[i].info_at[t] == NULL /* equiv to not in here */ ) {
		continue;
	    }
	    /* insert adder clause, if at t > 0. else, say it has to be true,
	     * if it's an initial fact.
	     */
	    if ( t == 0 ) {
		for ( j = 0; j < ginitial_state.num_F; j++ ) {
		    if ( ginitial_state.F[j] == i ) break;
		}
		if ( j < ginitial_state.num_F ) {
		    fcode[t][i] = code++;
		    if ( code == MAX_CNF_VARS + 1 ) {
			printf("\n\ntoo many cnf vars. increase MAX_CNF_VARS.\n\n");
			exit( 1 );
		    }
		    fop_to_code[code-1] = i;
		    ftime_to_code[code-1] = t;
		    if ( lnum_clauses == MAX_CLAUSES ) {
			printf("\n\ntoo many clauses. increase MAX_CLAUSES.\n\n");
			exit( 1 );
		    }
		    lclause[lnum_clauses] = ( int * ) calloc( 1, sizeof( int ) );
		    lclause[lnum_clauses][0] = fcode[t][i];
		    lclause_size[lnum_clauses] = 1;
		}
		continue;
	    }
	    if ( fcode[t][i] != -1 ) {
		printf("\n\nft already coded??\n\n");
		exit( UNSAT );
	    }
	    fcode[t][i] = code++;
	    if ( code == MAX_CNF_VARS + 1 ) {
		printf("\n\ntoo many cnf vars. increase MAX_CNF_VARS.\n\n");
		exit( 1 );
	    }
	    fop_to_code[code-1] = i;
	    ftime_to_code[code-1] = t;
	    if ( lnum_clauses == MAX_CLAUSES ) {
		printf("\n\ntoo many clauses. increase MAX_CLAUSES.\n\n");
		exit( 1 );
	    }
	    lclause[lnum_clauses] = ( int * ) calloc( gft_conn[i].num_A+1, sizeof( int ) );
	    lclause[lnum_clauses][0] = (-1) * fcode[t][i];
	    lclause_size[lnum_clauses] = 1;
	    for ( j = 0; j < gft_conn[i].num_A; j++ ) {
		op = gft_conn[i].A[j];
		if ( gop_conn[op].info_at[t-1] == NULL ) {
		    continue;
		}
		if ( lcode[t-1][op] == -1 ) {
		    lcode[t-1][op] = code++;
		    if ( 0 ) {
			printf("\npC %d at %d", op, t-1);
		    }
		    if ( code == MAX_CNF_VARS + 1 ) {
			printf("\n\ntoo many cnf vars. increase MAX_CNF_VARS.\n\n");
			exit( 1 );
		    }
		    lop_to_code[code-1] = op;
		    ltime_to_code[code-1] = t-1;
		}
		lclause[lnum_clauses][lclause_size[lnum_clauses]++] = lcode[t-1][op];
	    }
	    if ( lclause_size[lnum_clauses] == 1 ) {
		printf("\n\nno achiever in at t>0??\n\n");
		exit( UNSAT );
	    }
	    lnum_clauses++;
        } /* i-ft */

	if ( t == layer ) {
	    continue;
	}

	for ( i = 0; i < gnum_op_conn; i++ ) {
	    if ( gop_conn[i].info_at[t] == NULL /* equiv to not in here */ ) {
		continue;
	    }
	    /* insert prec clauses.
	     */
	    for ( j = 0; j < gop_conn[i].num_P; j++ ) {
		ft =  gop_conn[i].P[j];

		if ( lnum_clauses == MAX_CLAUSES ) {
		    printf("\n\ntoo many clauses. increase MAX_CLAUSES.\n\n");
		    exit( 1 );
		}
		lclause[lnum_clauses] = ( int * ) calloc( 2, sizeof( int ) );
		/* here the op i is used at t, for the 1st time. assign the code.
		 */
		if ( j == 0 ) {
		    if ( lcode[t][i] != -1 ) {
			printf("\n\nop code %d at %d already assigned??\n\n", i, t);
			exit( UNSAT );
		    }
		    if ( 0 ) {
			printf("\nC %d at %d", i, t);
		    }
		    lcode[t][i] = code++;
		    if ( code == MAX_CNF_VARS + 1 ) {
			printf("\n\ntoo many cnf vars. increase MAX_CNF_VARS.\n\n");
			exit( 1 );
		    }
		    lop_to_code[code-1] = i;
		    ltime_to_code[code-1] = t;
		}

		lclause[lnum_clauses][0] = (-1) * lcode[t][i];
		if ( fcode[t][ft] == -1 ) {
		    printf("\n\nprec fact not coded??\n\n");
		    exit( 1 );
		}
		lclause[lnum_clauses][1] = fcode[t][ft];
		lclause_size[lnum_clauses] = 2;
		lnum_clauses++;
	    } /* j: pres of i-op */
	} /* i -- op */
    } /* t */


    /* goal constraints
     */
    printf("\ngoal constraints...");
    for ( i = 0; i < ggoal_state.num_F; i++ ) {
	ft = ggoal_state.F[i];

	if ( lnum_clauses == MAX_CLAUSES ) {
	    printf("\n\ntoo many clauses. increase MAX_CLAUSES.\n\n");
	    exit( 1 );
	}
	lclause[lnum_clauses] = ( int * ) calloc( 1, sizeof( int ) );
	if ( fcode[layer][ft] == -1 ) {
	    printf("\n\ngoal fact not coded - level too low??\n\n");
	    exit(UNSAT);
	}
	lclause[lnum_clauses][0] = fcode[layer][ft];
	lclause_size[lnum_clauses] = 1;
	lnum_clauses++;
    } /* goals */




    /* exclusion constraints.
     */
    for ( t = 0; t <= layer; t++ ) {
	printf("\nexclusion constraints layer %d...", t);
	for ( i1 = gin_fts_at[t]; i1 && i1->next; i1 = i1->next ) {
	    i = i1->i1;
	    for ( i2 = i1->next; i2; i2 = i2->next ) {
		j = i2->i1;
		if ( !(gft_conn[i].info_at[t]->bit_exclusives[gft_conn[j].uid_block] &
		       gft_conn[j].uid_mask) ) {
		    continue;
		}
		if ( lnum_clauses == MAX_CLAUSES ) {
		    printf("\n\ntoo many clauses. increase MAX_CLAUSES.\n\n");
		    exit( 1 );
		}
		lclause[lnum_clauses] = ( int * ) calloc( 2, sizeof( int ) );
		if ( fcode[t][i] == -1 ) {
		    fcode[t][i] = code++;
		    if ( code == MAX_CNF_VARS + 1 ) {
			printf("\n\ntoo many cnf vars. increase MAX_CNF_VARS.\n\n");
			exit( 1 );
		    }
		    fop_to_code[code-1] = i;
		    ftime_to_code[code-1] = t;
		}
		lclause[lnum_clauses][0] = (-1) * fcode[t][i];
		if ( fcode[t][j] == -1 ) {
		    fcode[t][j] = code++;
		    if ( code == MAX_CNF_VARS + 1 ) {
			printf("\n\ntoo many cnf vars. increase MAX_CNF_VARS.\n\n");
			exit( 1 );
		    }
		    fop_to_code[code-1] = j;
		    ftime_to_code[code-1] = t;
		}
		lclause[lnum_clauses][1] = (-1) * fcode[t][j];
		lclause_size[lnum_clauses] = 2;
		lnum_clauses++;
	    } /* i2 */
	} /* i1 */

	if ( t == layer ) continue;

	for ( i1 = gin_ops_at[t]; i1 && i1->next; i1 = i1->next ) {
	    i = i1->i1;
	    for ( i2 = i1->next; i2; i2 = i2->next ) {
		j = i2->i1;
		if ( !(gop_conn[i].info_at[t]->bit_exclusives[gop_conn[j].uid_block] &
		       gop_conn[j].uid_mask) ) {
		    continue;
		}
		if ( lnum_clauses == MAX_CLAUSES ) {
		    printf("\n\ntoo many clauses. increase MAX_CLAUSES.\n\n");
		    exit( 1 );
		}
		lclause[lnum_clauses] = ( int * ) calloc( 2, sizeof( int ) );
		if ( lcode[t][i] == -1 ) {
		    lcode[t][i] = code++;
		    if ( code == MAX_CNF_VARS + 1 ) {
			printf("\n\ntoo many cnf vars. increase MAX_CNF_VARS.\n\n");
			exit( 1 );
		    }
		    lop_to_code[code-1] = i;
		    ltime_to_code[code-1] = t;
		}
		lclause[lnum_clauses][0] = (-1) * lcode[t][i];
		if ( lcode[t][j] == -1 ) {
		    lcode[t][j] = code++;
		    if ( code == MAX_CNF_VARS + 1 ) {
			printf("\n\ntoo many cnf vars. increase MAX_CNF_VARS.\n\n");
			exit( 1 );
		    }
		    lop_to_code[code-1] = j;
		    ltime_to_code[code-1] = t;
		}
		lclause[lnum_clauses][1] = (-1) * lcode[t][j];
		lclause_size[lnum_clauses] = 2;
		lnum_clauses++;
	    }
	}
    }

    /* that's it. print CNF file.
     */
    if ( gcmd_line.debug ) {
        /* debugging: print semantic version of clause set.
         */

        if ( (VARFILE = fopen( gcmd_line.varFileName, "w" )) == NULL ) {
            printf("\n\nEXIT: can't open VARFILE file.\n\n");
            exit( 1 );
        }
        fprintf(VARFILE, "\n\nc DECISION LAYER %d, GP-BASED ENCODING.", layer);
        fprintf(VARFILE, "\n\nc VARS:");

        for ( i = 1; i < code; i++ ) {
            fprintf(VARFILE, "\nc var %6d action (", i);
            print_op_nameToFile( lop_to_code[i], VARFILE );
            fprintf(VARFILE, ") %d", ltime_to_code[i] );
        }

        fclose(VARFILE);
    }

    printf("\n\nDECISION LAYER %d, WRITING GP-BASED ENCODING WITH %d VARS, %d CLAUSES.", 
	   layer, code-1, lnum_clauses);
 
    if ( create == 0 ) {
        if ( (CNF = fopen( gcmd_line.cnfFileName, "w" )) == NULL ) {
	    printf("\n\ncan't open CNF file.\n\n");
	    exit( 1 );
        }

        fprintf(CNF, "c CNF file planning task -p %s, -o %s, -f %s, bound %d, gp-based encoding\n",
	        gcmd_line.path, gcmd_line.ops_file_name, gcmd_line.fct_file_name, layer);

         fprintf(CNF, "p cnf %d %d\n", code-1, lnum_clauses);
         for ( i = 0; i < lnum_clauses; i++ ) {
	    for ( j = 0; j < lclause_size[i]; j++ ) {
	        fprintf(CNF, "%d ", lclause[i][j]);
	    }
	    fprintf(CNF, "0\n");
        }
        fclose(CNF);
    }
}

















/* thin gp-based
 */

























void older_print_thin_gp_based_encoding( int layer, int create )

{
    FILE *CNF, *VARFILE;

    int i, j, t, ft, op, code;
    IntList *tmp, *i1, *i2;

    /* have to separate the ft coding out from the op coding.
     */
    int **fcode;
    int *fop_to_code;
    int *ftime_to_code;



    lclause = ( int ** ) calloc( MAX_CLAUSES, sizeof( int * ) );
    lclause_size = ( int * ) calloc( MAX_CLAUSES, sizeof( int ) );
    lcode = ( int ** ) calloc( layer, sizeof( int * ) );
    lop_to_code = ( int * ) calloc( MAX_CNF_VARS, sizeof( int ) );
    ltime_to_code = ( int * ) calloc( MAX_CNF_VARS, sizeof( int ) );
    for ( i = 0; i < layer; i++ ) {
	lcode[i] = ( int * ) calloc( gnum_op_conn, sizeof( int ) );
	for ( j = 0; j < gnum_op_conn; j++ ) {
	    lcode[i][j] = -1;
	}
    }
    fcode = ( int ** ) calloc( layer + 1, sizeof( int * ) );
    fop_to_code = ( int * ) calloc( MAX_CNF_VARS, sizeof( int ) );
    ftime_to_code = ( int * ) calloc( MAX_CNF_VARS, sizeof( int ) );
    for ( i = 0; i <= layer; i++ ) {
	fcode[i] = ( int * ) calloc( gnum_ft_conn, sizeof( int ) );
	for ( j = 0; j < gnum_ft_conn; j++ ) {
	    fcode[i][j] = -1;
	}
    }

    /* build the graph up to <layer>
     */
    for ( i = 0; i < gnum_op_conn; i++ ) {
	tmp = new_IntList( i );
	if ( gout_ops ) {
	    gout_ops->prev = tmp;
	}
	tmp->next = gout_ops;
	gout_ops = tmp;
    }
    for ( i = 0; i < ginitial_state.num_F; i++ ) {
	ft = ginitial_state.F[i];
	gin_ft_count++;
	gft_conn[ft].first_appearance = 0;
	gft_conn[ft].info_at[0] = new_FtLevelInfo();
	tmp = new_IntList( ft );
	tmp->next = gin_fts;
	gin_fts = tmp;
    }
    gin_fts_at[0] = gin_fts;
    if ( gcmd_line.display_info ) {
	printf("\ntime: %3d, %5d facts and %7d exclusive pairs",
	       0, gin_ft_count, gin_ft_exclusion_count);
	fflush(stdout);
    }
    for( t = 0; t < layer; t++ ) {
	build_graph_evolution_step();
    }

    printf("\n\ncreating thin gp-based encoding...");

    /* prec constraints, + code assignment.
     */
    lnum_clauses = 0;
    code = 1; /* variables numbered 1 .. n */
    for ( t = 0; t <= layer; t++ ) {
	printf("\nplan graph layer %d...", t);
	for ( i = 0; i < gnum_ft_conn; i++ ) {
	    if ( gft_conn[i].info_at[t] == NULL /* equiv to not in here */ ) {
		continue;
	    }
	    /* insert adder clause, if at t > 0. else, say it has to be true,
	     * if it's an initial fact.
	     */
	    if ( t == 0 ) {
		for ( j = 0; j < ginitial_state.num_F; j++ ) {
		    if ( ginitial_state.F[j] == i ) break;
		}
		if ( j < ginitial_state.num_F ) {
		    fcode[t][i] = code++;
		    if ( code == MAX_CNF_VARS + 1 ) {
			printf("\n\ntoo many cnf vars. increase MAX_CNF_VARS.\n\n");
			exit( 1 );
		    }
		    fop_to_code[code-1] = i;
		    ftime_to_code[code-1] = t;
		    if ( lnum_clauses == MAX_CLAUSES ) {
			printf("\n\ntoo many clauses. increase MAX_CLAUSES.\n\n");
			exit( 1 );
		    }
		    lclause[lnum_clauses] = ( int * ) calloc( 1, sizeof( int ) );
		    lclause[lnum_clauses][0] = fcode[t][i];
		    lclause_size[lnum_clauses] = 1;
		}
		continue;
	    }
	    if ( fcode[t][i] != -1 ) {
		printf("\n\nft already coded??\n\n");
		exit( UNSAT );
	    }
	    fcode[t][i] = code++;
	    if ( code == MAX_CNF_VARS + 1 ) {
		printf("\n\ntoo many cnf vars. increase MAX_CNF_VARS.\n\n");
		exit( 1 );
	    }
	    fop_to_code[code-1] = i;
	    ftime_to_code[code-1] = t;
	    if ( lnum_clauses == MAX_CLAUSES ) {
		printf("\n\ntoo many clauses. increase MAX_CLAUSES.\n\n");
		exit( 1 );
	    }
	    lclause[lnum_clauses] = ( int * ) calloc( gft_conn[i].num_A+1, sizeof( int ) );
	    lclause[lnum_clauses][0] = (-1) * fcode[t][i];
	    lclause_size[lnum_clauses] = 1;
	    for ( j = 0; j < gft_conn[i].num_A; j++ ) {
		op = gft_conn[i].A[j];
		if ( gop_conn[op].info_at[t-1] == NULL ) {
		    continue;
		}
		if ( lcode[t-1][op] == -1 ) {
		    lcode[t-1][op] = code++;
		    if ( 0 ) {
			printf("\npC %d at %d", op, t-1);
		    }
		    if ( code == MAX_CNF_VARS + 1 ) {
			printf("\n\ntoo many cnf vars. increase MAX_CNF_VARS.\n\n");
			exit( 1 );
		    }
		    lop_to_code[code-1] = op;
		    ltime_to_code[code-1] = t-1;
		}
		lclause[lnum_clauses][lclause_size[lnum_clauses]++] = lcode[t-1][op];
	    }
	    if ( lclause_size[lnum_clauses] == 1 ) {
		printf("\n\nno achiever in at t>0??\n\n");
		exit( UNSAT );
	    }
	    lnum_clauses++;
	} /* i-ft */

	if ( t == layer ) {
	    continue;
	}

	for ( i = 0; i < gnum_op_conn; i++ ) {
	    if ( gop_conn[i].info_at[t] == NULL /* equiv to not in here */ ) {
		continue;
	    }
	    /* insert prec clauses.
	     */
	    for ( j = 0; j < gop_conn[i].num_P; j++ ) {
		ft =  gop_conn[i].P[j];

		if ( lnum_clauses == MAX_CLAUSES ) {
		    printf("\n\ntoo many clauses. increase MAX_CLAUSES.\n\n");
		    exit( 1 );
		}
		lclause[lnum_clauses] = ( int * ) calloc( 2, sizeof( int ) );
		/* here the op i is used at t, for the 1st time. assign the code.
		 */
		if ( j == 0 ) {
		    if ( lcode[t][i] != -1 ) {
			printf("\n\nop code %d at %d already assigned??\n\n", i, t);
			exit( UNSAT );
		    }
		    if ( 0 ) {
			printf("\nC %d at %d", i, t);
		    }
		    lcode[t][i] = code++;
		    if ( code == MAX_CNF_VARS + 1 ) {
			printf("\n\ntoo many cnf vars. increase MAX_CNF_VARS.\n\n");
			exit( 1 );
		    }
		    lop_to_code[code-1] = i;
		    ltime_to_code[code-1] = t;
		}

		lclause[lnum_clauses][0] = (-1) * lcode[t][i];
		if ( fcode[t][ft] == -1 ) {
		    printf("\n\nprec fact not coded??\n\n");
		    exit( 1 );
		}
		lclause[lnum_clauses][1] = fcode[t][ft];
		lclause_size[lnum_clauses] = 2;
		lnum_clauses++;
	    } /* j: pres of i-op */
	} /* i -- op */
    } /* t */


    /* goal constraints
     */
    printf("\ngoal constraints...");
    for ( i = 0; i < ggoal_state.num_F; i++ ) {
	ft = ggoal_state.F[i];

	if ( lnum_clauses == MAX_CLAUSES ) {
	    printf("\n\ntoo many clauses. increase MAX_CLAUSES.\n\n");
	    exit( 1 );
	}
	lclause[lnum_clauses] = ( int * ) calloc( 1, sizeof( int ) );
	if ( fcode[layer][ft] == -1 ) {
	    printf("\n\ngoal fact not coded - level too low??\n\n");
	    exit(UNSAT);
	}
	lclause[lnum_clauses][0] = fcode[layer][ft];
	lclause_size[lnum_clauses] = 1;
	lnum_clauses++;
    } /* goals */




    /* exclusion constraints.
     */
    for ( t = 0; t < layer; t++ ) {
	printf("\nexclusion constraints layer %d...", t);

	for ( i1 = gin_ops_at[t]; i1 && i1->next; i1 = i1->next ) {
	    i = i1->i1;
	    for ( i2 = i1->next; i2; i2 = i2->next ) {
		j = i2->i1;
		if ( !interfere( i, j ) ) {
		    continue;
		}
		if ( lnum_clauses == MAX_CLAUSES ) {
		    printf("\n\ntoo many clauses. increase MAX_CLAUSES.\n\n");
		    exit( 1 );
		}
		lclause[lnum_clauses] = ( int * ) calloc( 2, sizeof( int ) );
		if ( lcode[t][i] == -1 ) {
		    lcode[t][i] = code++;
		    if ( code == MAX_CNF_VARS + 1 ) {
			printf("\n\ntoo many cnf vars. increase MAX_CNF_VARS.\n\n");
			exit( 1 );
		    }
		    lop_to_code[code-1] = i;
		    ltime_to_code[code-1] = t;
		}
		lclause[lnum_clauses][0] = (-1) * lcode[t][i];
		if ( lcode[t][j] == -1 ) {
		    lcode[t][j] = code++;
		    if ( code == MAX_CNF_VARS + 1 ) {
			printf("\n\ntoo many cnf vars. increase MAX_CNF_VARS.\n\n");
			exit( 1 );
		    }
		    lop_to_code[code-1] = j;
		    ltime_to_code[code-1] = t;
		}
		lclause[lnum_clauses][1] = (-1) * lcode[t][j];
		lclause_size[lnum_clauses] = 2;
		lnum_clauses++;
	    }
	}
    }

    /* that's it. print CNF file.
     */
    if ( gcmd_line.debug ) {
	/* debugging: print semantic version of clause set.
	 */

        if ( (VARFILE = fopen( gcmd_line.varFileName, "w" )) == NULL ) {
            printf("\n\nEXIT: can't open VARFILE file.\n\n");
            exit( 1 );
        }
	fprintf(VARFILE, "\n\nc DECISION LAYER %d, THIN GP-BASED ENCODING.", layer);
	fprintf(VARFILE, "\n\nc VARS:");

        for ( i = 1; i < code; i++ ) {
            fprintf(VARFILE, "\nc var %2d action (", i);
  	    print_op_nameToFile( lop_to_code[i], VARFILE );
	    fprintf(VARFILE, ") %d", ltime_to_code[i] );
	} 

        /*
	printf("\n\nCLAUSES:");
	for ( i = 0; i < lnum_clauses; i++ ) {
	    printf("\n%6d - ", i);
	    if ( 1 ) {
		for ( j = 0; j < lclause_size[i]; j++ ) {
		    printf("(%d)", lclause[i][j]);
		    if ( j < lclause_size[i]-1 ) printf(", ");
		} 
		fflush(stdout);
	    }
	    for ( j = 0; j < lclause_size[i]; j++ ) {
		if ( lclause[i][j] > 0 ) {
		    printf("(");
		    print_op_name( lop_to_code[lclause[i][j]] );
		    printf(" at %d)", ltime_to_code[lclause[i][j]] );
		} else {
		    printf("(NOT ");
		    print_op_name( lop_to_code[(-1)*lclause[i][j]] );
		    printf(" at %d)", ltime_to_code[(-1)*lclause[i][j]] );
		}
		if ( j < lclause_size[i]-1 ) printf(", ");
	    }
	}
        */
        fclose(VARFILE);
    }

    printf("\n\nDECISION LAYER %d, WRITING THIN GP-BASED ENCODING WITH %d VARS, %d CLAUSES.", 
	   layer, code-1, lnum_clauses);


    if ( create == 0 ) {
        if ( (CNF = fopen(gcmd_line.cnfFileName, "w")) == NULL ) {
	    printf("\n\ncan't open CNF file.\n\n");
            exit( 1 );
        }

        fprintf(CNF, "c CNF file planning task -p %s, -o %s, -f %s, bound %d, thin gp-based encoding\n",
	        gcmd_line.path, gcmd_line.ops_file_name, gcmd_line.fct_file_name, layer);

        fprintf(CNF, "p cnf %d %d\n", code-1, lnum_clauses);
        for ( i = 0; i < lnum_clauses; i++ ) {
	    for ( j = 0; j < lclause_size[i]; j++ ) {
	        fprintf(CNF, "%d ", lclause[i][j]);
	    }
	    fprintf(CNF, "0\n");
        }
        fclose(CNF);
    }
}


void print_thin_gp_based_encoding( int layer, int create )

{
    FILE *CNF, *VARFILE;

    int i, j, t, ft, op, code;
    IntList *tmp, *i1, *i2;

    /* have to separate the ft coding out from the op coding.
     */
    int **fcode;
    int *fop_to_code;
    int *ftime_to_code;



    lclause = ( int ** ) calloc( MAX_CLAUSES, sizeof( int * ) );
    lclause_size = ( int * ) calloc( MAX_CLAUSES, sizeof( int ) );
    lcode = ( int ** ) calloc( layer, sizeof( int * ) );
    lop_to_code = ( int * ) calloc( MAX_CNF_VARS, sizeof( int ) );
    ltime_to_code = ( int * ) calloc( MAX_CNF_VARS, sizeof( int ) );
    for ( i = 0; i < layer; i++ ) {
	lcode[i] = ( int * ) calloc( gnum_op_conn, sizeof( int ) );
	for ( j = 0; j < gnum_op_conn; j++ ) {
	    lcode[i][j] = -1;
	}
    }
    fcode = ( int ** ) calloc( layer + 1, sizeof( int * ) );
    fop_to_code = ( int * ) calloc( MAX_CNF_VARS, sizeof( int ) );
    ftime_to_code = ( int * ) calloc( MAX_CNF_VARS, sizeof( int ) );
    for ( i = 0; i <= layer; i++ ) {
	fcode[i] = ( int * ) calloc( gnum_ft_conn, sizeof( int ) );
	for ( j = 0; j < gnum_ft_conn; j++ ) {
	    fcode[i][j] = -1;
	}
    }

    /* build the graph up to <layer>
     */
    for ( i = 0; i < gnum_op_conn; i++ ) {
	tmp = new_IntList( i );
	if ( gout_ops ) {
	    gout_ops->prev = tmp;
	}
	tmp->next = gout_ops;
	gout_ops = tmp;
    }
    for ( i = 0; i < ginitial_state.num_F; i++ ) {
	ft = ginitial_state.F[i];
	gin_ft_count++;
	gft_conn[ft].first_appearance = 0;
	gft_conn[ft].info_at[0] = new_FtLevelInfo();
	tmp = new_IntList( ft );
	tmp->next = gin_fts;
	gin_fts = tmp;
    }
    gin_fts_at[0] = gin_fts;
    if ( gcmd_line.display_info ) {
	printf("\ntime: %3d, %5d facts and %7d exclusive pairs",
	       0, gin_ft_count, gin_ft_exclusion_count);
	fflush(stdout);
    }
    for( t = 0; t < layer; t++ ) {
	build_graph_evolution_step();
    }

    printf("\n\ncreating thin gp-based encoding...");

    /* prec constraints, + code assignment.
     */
    lnum_clauses = 0;
    code = 1; /* variables numbered 1 .. n */
    for ( t = 0; t <= layer; t++ ) {
	printf("\nplan graph layer %d...", t);
	for ( i = 0; i < gnum_ft_conn; i++ ) {
	    if ( gft_conn[i].info_at[t] == NULL /* equiv to not in here */ ) {
		continue;
	    }
	    /* insert adder clause, if at t > 0. else, say it has to be true,
	     * if it's an initial fact.
	     */
	    if ( t == 0 ) {
		for ( j = 0; j < ginitial_state.num_F; j++ ) {
		    if ( ginitial_state.F[j] == i ) break;
		}
		if ( j < ginitial_state.num_F ) {
		    fcode[t][i] = code++;
		    if ( code == MAX_CNF_VARS + 1 ) {
			printf("\n\ntoo many cnf vars. increase MAX_CNF_VARS.\n\n");
			exit( 1 );
		    }
		    fop_to_code[code-1] = i;
		    ftime_to_code[code-1] = t;
		    if ( lnum_clauses == MAX_CLAUSES ) {
			printf("\n\ntoo many clauses. increase MAX_CLAUSES.\n\n");
			exit( 1 );
		    }
		    lclause[lnum_clauses] = ( int * ) calloc( 1, sizeof( int ) );
		    lclause[lnum_clauses][0] = fcode[t][i];
		    lclause_size[lnum_clauses] = 1;
		}
		continue;
	    }
	    if ( fcode[t][i] != -1 ) {
		printf("\n\nft already coded??\n\n");
		exit( UNSAT );
	    }
	    fcode[t][i] = code++;
	    if ( code == MAX_CNF_VARS + 1 ) {
		printf("\n\ntoo many cnf vars. increase MAX_CNF_VARS.\n\n");
		exit( 1 );
	    }
	    fop_to_code[code-1] = i;
	    ftime_to_code[code-1] = t;
	    if ( lnum_clauses == MAX_CLAUSES ) {
		printf("\n\ntoo many clauses. increase MAX_CLAUSES.\n\n");
		exit( 1 );
	    }
	    lclause[lnum_clauses] = ( int * ) calloc( gft_conn[i].num_A+1, sizeof( int ) );
	    lclause[lnum_clauses][0] = (-1) * fcode[t][i];
	    lclause_size[lnum_clauses] = 1;
	    for ( j = 0; j < gft_conn[i].num_A; j++ ) {
		op = gft_conn[i].A[j];
		if ( gop_conn[op].info_at[t-1] == NULL ) {
		    continue;
		}
		if ( lcode[t-1][op] == -1 ) {
		    lcode[t-1][op] = code++;
		    if ( 0 ) {
			printf("\npC %d at %d", op, t-1);
		    }
		    if ( code == MAX_CNF_VARS + 1 ) {
			printf("\n\ntoo many cnf vars. increase MAX_CNF_VARS.\n\n");
			exit( 1 );
		    }
		    lop_to_code[code-1] = op;
		    ltime_to_code[code-1] = t-1;
		}
		lclause[lnum_clauses][lclause_size[lnum_clauses]++] = lcode[t-1][op];
	    }
	    if ( lclause_size[lnum_clauses] == 1 ) {
		printf("\n\nno achiever in at t>0??\n\n");
		exit( UNSAT );
	    }
	    lnum_clauses++;
	} /* i-ft */

	if ( t == layer ) {
	    continue;
	}

	for ( i = 0; i < gnum_op_conn; i++ ) {
	    if ( gop_conn[i].info_at[t] == NULL /* equiv to not in here */ ) {
		continue;
	    }
	    /* insert prec clauses.
	     */
	    for ( j = 0; j < gop_conn[i].num_P; j++ ) {
		ft =  gop_conn[i].P[j];

		if ( lnum_clauses == MAX_CLAUSES ) {
		    printf("\n\ntoo many clauses. increase MAX_CLAUSES.\n\n");
		    exit( 1 );
		}
		lclause[lnum_clauses] = ( int * ) calloc( 2, sizeof( int ) );
		/* here the op i is used at t, for the 1st time. assign the code.
		 */
		if ( j == 0 ) {
		    if ( lcode[t][i] != -1 ) {
			printf("\n\nop code %d at %d already assigned??\n\n", i, t);
			exit( UNSAT );
		    }
		    if ( 0 ) {
			printf("\nC %d at %d", i, t);
		    }
		    lcode[t][i] = code++;
		    if ( code == MAX_CNF_VARS + 1 ) {
			printf("\n\ntoo many cnf vars. increase MAX_CNF_VARS.\n\n");
			exit( 1 );
		    }
		    lop_to_code[code-1] = i;
		    ltime_to_code[code-1] = t;
		}

		lclause[lnum_clauses][0] = (-1) * lcode[t][i];
		if ( fcode[t][ft] == -1 ) {
		    printf("\n\nprec fact not coded??\n\n");
		    exit( 1 );
		}
		lclause[lnum_clauses][1] = fcode[t][ft];
		lclause_size[lnum_clauses] = 2;
		lnum_clauses++;
	    } /* j: pres of i-op */
	} /* i -- op */
    } /* t */


    /* goal constraints
     */
    printf("\ngoal constraints...");
    for ( i = 0; i < ggoal_state.num_F; i++ ) {
	ft = ggoal_state.F[i];

	if ( lnum_clauses == MAX_CLAUSES ) {
	    printf("\n\ntoo many clauses. increase MAX_CLAUSES.\n\n");
	    exit( 1 );
	}
	lclause[lnum_clauses] = ( int * ) calloc( 1, sizeof( int ) );
	if ( fcode[layer][ft] == -1 ) {
	    printf("\n\ngoal fact not coded - level too low??\n\n");
	    exit(UNSAT);
	}
	lclause[lnum_clauses][0] = fcode[layer][ft];
	lclause_size[lnum_clauses] = 1;
	lnum_clauses++;
    } /* goals */




    /* exclusion constraints.
     */

    /* START CHANGE */
    for ( t = 0; t <= layer; t++ ) {
	printf("\nexclusion constraints layer %d...", t);

	for ( i1 = gin_fts_at[t]; i1 && i1->next; i1 = i1->next ) {
	    i = i1->i1;
	    for ( i2 = i1->next; i2; i2 = i2->next ) {
		j = i2->i1;
		if ( !(gft_conn[i].info_at[t]->bit_exclusives[gft_conn[j].uid_block] &
		       gft_conn[j].uid_mask) ) {
		    continue;
		}
		if ( lnum_clauses == MAX_CLAUSES ) {
		    printf("\n\ntoo many clauses. increase MAX_CLAUSES.\n\n");
		    exit( 1 );
		}
		lclause[lnum_clauses] = ( int * ) calloc( 2, sizeof( int ) );
		if ( fcode[t][i] == -1 ) {
		    fcode[t][i] = code++;
		    if ( code == MAX_CNF_VARS + 1 ) {
			printf("\n\ntoo many cnf vars. increase MAX_CNF_VARS.\n\n");
			exit( 1 );
		    }
		    fop_to_code[code-1] = i;
		    ftime_to_code[code-1] = t;
		}
		lclause[lnum_clauses][0] = (-1) * fcode[t][i];
		if ( fcode[t][j] == -1 ) {
		    fcode[t][j] = code++;
		    if ( code == MAX_CNF_VARS + 1 ) {
			printf("\n\ntoo many cnf vars. increase MAX_CNF_VARS.\n\n");
			exit( 1 );
		    }
		    fop_to_code[code-1] = j;
		    ftime_to_code[code-1] = t;
		}
		lclause[lnum_clauses][1] = (-1) * fcode[t][j];
		lclause_size[lnum_clauses] = 2;
		lnum_clauses++;
	    } /* i2 */
	} /* i1 */

	if ( t == layer ) continue;
        /* END CHANGE */

	for ( i1 = gin_ops_at[t]; i1 && i1->next; i1 = i1->next ) {
	    i = i1->i1;
	    for ( i2 = i1->next; i2; i2 = i2->next ) {
		j = i2->i1;
		if ( !interfere( i, j ) ) {
		    continue;
		}
		if ( lnum_clauses == MAX_CLAUSES ) {
		    printf("\n\ntoo many clauses. increase MAX_CLAUSES.\n\n");
		    exit( 1 );
		}
		lclause[lnum_clauses] = ( int * ) calloc( 2, sizeof( int ) );
		if ( lcode[t][i] == -1 ) {
		    lcode[t][i] = code++;
		    if ( code == MAX_CNF_VARS + 1 ) {
			printf("\n\ntoo many cnf vars. increase MAX_CNF_VARS.\n\n");
			exit( 1 );
		    }
		    lop_to_code[code-1] = i;
		    ltime_to_code[code-1] = t;
		}
		lclause[lnum_clauses][0] = (-1) * lcode[t][i];
		if ( lcode[t][j] == -1 ) {
		    lcode[t][j] = code++;
		    if ( code == MAX_CNF_VARS + 1 ) {
			printf("\n\ntoo many cnf vars. increase MAX_CNF_VARS.\n\n");
			exit( 1 );
		    }
		    lop_to_code[code-1] = j;
		    ltime_to_code[code-1] = t;
		}
		lclause[lnum_clauses][1] = (-1) * lcode[t][j];
		lclause_size[lnum_clauses] = 2;
		lnum_clauses++;
	    }
	}
    }

    /* that's it. print CNF file.
     */
    if ( gcmd_line.debug ) {
	/* debugging: print semantic version of clause set.
	 */

        if ( (VARFILE = fopen( gcmd_line.varFileName, "w" )) == NULL ) {
            printf("\n\nEXIT: can't open VARFILE file.\n\n");
            exit( 1 );
        }
	fprintf(VARFILE, "\n\nc DECISION LAYER %d, THIN GP-BASED ENCODING.", layer);
	fprintf(VARFILE, "\n\nc VARS:");

        for ( i = 1; i < code; i++ ) {
            fprintf(VARFILE, "\nc var %2d action (", i);
  	    print_op_nameToFile( lop_to_code[i], VARFILE );
	    fprintf(VARFILE, ") %d", ltime_to_code[i] );
	}

        /*
	printf("\n\nCLAUSES:");
	for ( i = 0; i < lnum_clauses; i++ ) {
	    printf("\n%6d - ", i);
	    if ( 1 ) {
		for ( j = 0; j < lclause_size[i]; j++ ) {
		    printf("(%d)", lclause[i][j]);
		    if ( j < lclause_size[i]-1 ) printf(", ");
		}
		fflush(stdout);
	    }
	    for ( j = 0; j < lclause_size[i]; j++ ) {
		if ( lclause[i][j] > 0 ) {
		    printf("(");
		    print_op_name( lop_to_code[lclause[i][j]] );
		    printf(" at %d)", ltime_to_code[lclause[i][j]] );
		} else {
		    printf("(NOT ");
		    print_op_name( lop_to_code[(-1)*lclause[i][j]] );
		    printf(" at %d)", ltime_to_code[(-1)*lclause[i][j]] );
		}
		if ( j < lclause_size[i]-1 ) printf(", ");
	    }
	}
        */
        fclose(VARFILE);
    }

    printf("\n\nDECISION LAYER %d, WRITING THIN GP-BASED ENCODING WITH %d VARS, %d CLAUSES.",
	   layer, code-1, lnum_clauses);


    if ( create == 0 ) {
        if ( (CNF = fopen(gcmd_line.cnfFileName, "w")) == NULL ) {
	    printf("\n\ncan't open CNF file.\n\n");
            exit( 1 );
        }

        fprintf(CNF, "c CNF file planning task -p %s, -o %s, -f %s, bound %d, thin gp-based encoding\n",
	        gcmd_line.path, gcmd_line.ops_file_name, gcmd_line.fct_file_name, layer);

        fprintf(CNF, "p cnf %d %d\n", code-1, lnum_clauses);
        for ( i = 0; i < lnum_clauses; i++ ) {
	    for ( j = 0; j < lclause_size[i]; j++ ) {
	        fprintf(CNF, "%d ", lclause[i][j]);
	    }
	    fprintf(CNF, "0\n");
        }
        fclose(CNF);
    }
}
