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

 * File: dis_subproblem.c 

 * Authors:  Chih-wei Hsu and Benjamin W. Wah

 *

 *********************************************************************/
int solve_subproblem(int h, int b)
{
  int found_plan = 0;   
  dis_gnum_plan_ops = 0;
  if ( h )
  {
    max_hc_iter = h;
    found_plan = dis_do_enforced_hill_climbing();
    
    if ( !found_plan )
    {
      printf("\n\nEnforced Hill-climbing failed !");
      printf("\nswitching to Best-first Search now.\n");
      fflush(stdout);
      if (b)
      {
        max_bfs_iter = b;
        found_plan = dis_do_best_first_search();
      }
    } 
  }   
  else
  {
    if (b)
    {
      max_bfs_iter = b;
      found_plan = dis_do_best_first_search();
    }
  }
  if (!found_plan)
    dis_gnum_plan_ops = 0;
  return found_plan;
}

void dis_back_space_reduction(dis_State *B, int* rsa_facts, int* rsa_actions,
        dis_State *S, dis_State *newS, dis_State *start_state)
{                       
    int i,j, k, f, a, ii;
    int level;
    
    int *rsa_fluents = (int *) malloc(sizeof(int)*dis_gnum_fl_conn);
//  int *rsa_facts = (int *) malloc(sizeof(int)*dis_gnum_ft_conn);
//  int *rsa_effects = (int *) malloc(sizeof(int)*dis_gnum_ef_conn);
    
    for (i = 0; i < dis_gnum_ft_conn; i++) rsa_facts[i] = -1;
    for (i=0;i<dis_gnum_gfl_conn;i++)
      rsa_fluents[i] = -1;
/*    for(i=0; i<start_state->num_F; i++) {
        rsa_facts[start_state->F[i]] = 0;
    }
    for (i=0;i<dis_gnum_fl_conn;i++)
      if (start_state->f_D[i])
        rsa_fluents[i] = 0;*/
    S->num_F = 0;
    for(i=0; i<B->num_F; i++) {
    //    if(rsa_facts[B->F[i]] < 0){
            rsa_facts[B->F[i]] = 0;
            S->F[S->num_F++] = B->F[i];
      //  }
    }
    for (i=0;i<dis_gnum_fl_conn;i++)
      S->f_D[i] = B->f_D[i];
    for (i = 0; i < dis_gnum_ef_conn; i++) {
        //rsa_actions[i] = -1;
        dis_gef_conn[i].red_level = -1;
    }
    
    level = 0;
    while(1)
    {
        newS->num_F = 0;
       
        if(S->num_F == 0) 
        //break;
        {
          for (ii=0;ii<dis_gnum_fl_conn;ii++)
            if (S->f_D[ii])
              break;
          if (ii == dis_gnum_fl_conn)
            break;
        }
        for (i=0;i<dis_gnum_fl_conn;i++)
        {
          if (!S->f_D[i])
            continue;
          for (j=0;j<dis_gfl_conn[i].num_AS;j++)
          {
            a = dis_gfl_conn[i].AS[j];
            if (dis_gef_conn[a].red_level >= 0)
              continue;
            dis_gef_conn[a].red_level = level;
            
                    for (k = 0; k < dis_gef_conn[a].num_PC; k++) {		
                        f = dis_gef_conn[a].PC[k];
                        if(f>=0) {
                            if(rsa_facts[f] < 0) {
                                rsa_facts[f] = level+1;
                                newS->F[newS->num_F++] = f;
                            }     
                        }
                    }       
                    
                    for (k=0;k<dis_gef_conn[a].num_f_PC;k++)
                    {
                      f = dis_gef_conn[a].f_PC_fl[k];
                      if (f >= 0)
                        if (rsa_fluents[f] < 0)
                        {
                          rsa_fluents[f] = level+1;
                          newS->f_D[f] = 1;
                        }
                    }
          }

          for (j=0;j<dis_fl_conn[i].num_IN;j++)
          {
            if (dis_gfl_conn[i].IN_c[j] <= 0 && dis_gfl_conn[i].IN_fl_[j] == -1)
              continue;
            a = dis_gfl_conn[i].IN[j];
            if (dis_gef_conn[a].red_level >= 0)
              continue;
            dis_gef_conn[a].red_level = level;
            
                    for (k = 0; k < dis_gef_conn[a].num_PC; k++) {		
                        f = dis_gef_conn[a].PC[k];
                        if(f>=0) {
                            if(rsa_facts[f] < 0) {
                                rsa_facts[f] = level+1;
                                newS->F[newS->num_F++] = f;
                            }     
                        }
                    }       
                    
                    for (k=0;k<dis_gef_conn[a].num_f_PC;k++)
                    {
                      f = dis_gef_conn[a].f_PC[k];
                      if (f >= 0)
                        if (rsa_fluents[f] < 0)
                        {
                          rsa_fluents[f] = level+1;
                          newS->f_D[f] = 1;
                        }
                    }
          }
        }
        for (ii = 0; ii < S->num_F; ii++) {
            i = S->F[ii];
        
            //printf("level %d", level); print_ft(i);
            for (j = 0; j < dis_gft_conn[i].num_A; j++) {
                a = dis_gft_conn[i].A[j];
                //if(rsa_actions[a]>=0) continue;
                if(dis_gef_conn[a].red_level>=0) continue;
                
                    if(!dis_gef_conn[a].DPop) {    
                    //    printf("level %d", level); print_op(a);
                    }
                    //rsa_actions[a] = level;
                    dis_gef_conn[a].red_level = level;
                        
                    for (k = 0; k < dis_gef_conn[a].num_PC; k++) {
                        f = dis_gef_conn[a].PC[k];
                        if(f>=0) {
                            if(rsa_facts[f] < 0) {
                                rsa_facts[f] = level+1;
                                newS->F[newS->num_F++] = f;
                            }     
                        }
                    }       
                    
                    for (k=0;k<dis_gef_conn[a].num_f_PC;k++)
                    {
                      f = dis_gef_conn[a].f_PC[k];
                      if (f >= 0)
                        if (rsa_fluents[f] < 0)
                        {
                          rsa_fluents[f] = level+1;
                          newS->f_D[f] = 1;
                        }
                    }
            }           
        }     
        
        level++;
//        S->num_F = newS->num_F;
//        for(j=0; j<newS->num_F; j++) S->F[j] = newS->F[j];
        dis_source_to_dest(newS, S);
    }
    
  xfree(rsa_fluents);
//  xfree(rsa_facts);
//  xfree(rsa_effects);
  
}
 