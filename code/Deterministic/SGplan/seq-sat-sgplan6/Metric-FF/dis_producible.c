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

 * File: dis_producible.c 

 * Authors:  Chih-wei Hsu and Benjamin W. Wah

 *

 *********************************************************************/
#include "dis_ff.h"
#include "dis_relax.h"
#include "dis_memory.h"
#include "dis_search.h"
#include "dis_constraints.h"
#include "dis_output.h"
#include "lpg.h"
#include "subspace.h"

short *is_producible_fact, *is_producible_fluent, *is_producible_effect;
unsigned char *is_producible_operator;
int *producible_solution, producible_solution_length, num_level_producible;
extern int *relax_lused_O, relax_lnum_used_O;

void save_goals(int *ng, int **g, int *nf, int **gf, float **vf, dis_Comparator **cf, 
int ng0, int *g0, int nf0, int *gf0, float *vf0, dis_Comparator *cf0)
{
  *g = (int *) malloc(ng0*sizeof(int));
  *gf = (int *) malloc(nf0*sizeof(int));
  *vf = (float *) malloc(nf0*sizeof(float));
  *cf = (dis_Comparator *) malloc(nf0*sizeof(dis_Comparator));

  *ng = ng0;
  memcpy(*g, g0, ng0*sizeof(int));
  *nf = nf0;
  memcpy(*gf, gf0, nf0*sizeof(int));
  memcpy(*vf, vf0, nf0*sizeof(float));
  memcpy(*cf, cf0, nf0*sizeof(dis_Comparator));
}

void restore_goals(int *ng, int *g, int *nf, int *gf, float *vf, dis_Comparator *cf, 
int ng0, int *g0, int nf0, int *gf0, float *vf0, dis_Comparator *cf0)
{
  *ng = ng0;
  memcpy(g, g0, ng0*sizeof(int));
  *nf = nf0;
  memcpy(gf, gf0, nf0*sizeof(int));
  memcpy(vf, vf0, nf0*sizeof(float));
  memcpy(cf, cf0, nf0*sizeof(dis_Comparator));
  
  xfree(g0);
  xfree(gf0);
  xfree(vf0);
  xfree(cf0);
}
                                        
int solve_subproblem(int h, int b, int r, int g_w, int h_w, int o)
{
  int found_plan = 0;
  dis_gnum_plan_ops = 0;
  dis_red_space = r;
  dis_gcmd_line.g_weight = g_w;
  dis_gcmd_line.h_weight = h_w;
  dis_gcmd_line.optimize = o;
  if ( h ) 
  {
    max_hc_iter = h;
    found_plan = dis_do_enforced_hill_climbing();
    
    if ( !found_plan ) 
    {
//      printf("\n\nEnforced Hill-climbing failed !");
//      printf("\nswitching to Best-first Search now.\n");
//      fflush(stdout);
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
  dis_red_space = 0;
  dis_gcmd_line.g_weight = 1;
  dis_gcmd_line.h_weight = 1;
  dis_gcmd_line.optimize = dis_FALSE;
  return found_plan;
}

void relax_producible_variables(dis_State *new, dis_State *old, float *relaxv)
{
  int i;
  unsigned char *fact = (unsigned char *) calloc(dis_gnum_ft_conn, sizeof(unsigned char));
  dis_make_state(old, dis_gnum_ft_conn, dis_gnum_fl_conn);
  dis_source_to_dest(old, new);
  for (i=0;i<old->num_F;i++)
    fact[old->F[i]] = 1;
  for (i=0;i<dis_gnum_ft_conn;i++)
    if (!fact[i] && is_producible_fact[i] > 0)
      new->F[new->num_F++] = i;
  for (i=0;i<dis_gnum_fl_conn;i++)
    if (is_producible_fluent[i] > 0)
      new->f_V[i] = relaxv[i];
  xfree(fact);    
}  

int solve_producible_variables_relax(dis_State *S, int nr_g, int *goal, int nr_f, int *goalf, float *value)
{
  dis_State S1;
  int i, relax_plan_length, h, x, *done, *relax_plan; 
  int saved_gnum_flogic_goal, saved_gnum_fnumeric_goal;
  int *saved_gflogic_goal, *saved_gfnumeric_goal_fl;
  float *saved_gfnumeric_goal_c;
  dis_Comparator *saved_gfnumeric_goal_comp;
  
  save_goals(&saved_gnum_flogic_goal, &saved_gflogic_goal, &saved_gnum_fnumeric_goal, &saved_gfnumeric_goal_fl, 
  &saved_gfnumeric_goal_c, &saved_gfnumeric_goal_comp, dis_gnum_flogic_goal, dis_gflogic_goal, dis_gnum_fnumeric_goal,
  dis_gfnumeric_goal_fl, dis_gfnumeric_goal_c, dis_gfnumeric_goal_comp);
  
  dis_gnum_flogic_goal = nr_g;
  for (i=0;i<nr_g;i++)
    dis_gflogic_goal[i] = goal[i];
  dis_gnum_fnumeric_goal = nr_f;
  for (i=0;i<nr_f;i++)
  {
    dis_gfnumeric_goal_comp[i] = GEQ;
    dis_gfnumeric_goal_fl[i] = goalf[i];
    dis_gfnumeric_goal_c[i] = value[i];
  }
  dis_red_space = 2;
  h = dis_get_1P(S);
  dis_red_space = 0;

  restore_goals(&dis_gnum_flogic_goal, dis_gflogic_goal, &dis_gnum_fnumeric_goal, dis_gfnumeric_goal_fl, 
  dis_gfnumeric_goal_c, dis_gfnumeric_goal_comp, saved_gnum_flogic_goal, saved_gflogic_goal, saved_gnum_fnumeric_goal,
  saved_gfnumeric_goal_fl, saved_gfnumeric_goal_c, saved_gfnumeric_goal_comp);

  if (h == 0)
    return 0;
  if (h == dis_INFINITY)
    return -1;
  dis_make_state(&S1, dis_gnum_ft_conn, dis_gnum_fl_conn);
  relax_plan = (int *) malloc(relax_lnum_used_O*sizeof(int));
  done = (int *) calloc(relax_lnum_used_O, sizeof(int));
  relax_plan_length = relax_lnum_used_O;
  for (i=0;i<relax_plan_length;i++)
    relax_plan[relax_plan_length-i-1] = relax_lused_O[i];
  x = 1;

  while (x)
  {
    x = 0;
    for (i=0;i<relax_plan_length;i++)
      if (!done[i] /*&& priority[relax_plan[i]] >= 0*/)
        if (is_applicable(S, relax_plan[i]))
        {
          done[i] = x = 1;
          dis_result_to_dest(&S1, S, relax_plan[i], -1);
          if (producible_solution_length > dis_MAX_PLAN_LENGTH - 1)
          {
            fprintf(stderr, "LONG\n");
            return -1;
          }
          producible_solution[producible_solution_length++] = relax_plan[i];
          dis_source_to_dest(S, &S1);
        }
  }

  xfree(done);
  xfree(relax_plan);
  dis_destroy_state(&S1);
  return 1;
}

int solve_producible_variables(dis_State *S, int nr_g, int *goal, int nr_f, int *goalf, float *value)
{
  int i, j, nr_g0 = 0, nr_f0 = 0, x = 0;
  int *goal0 = (int *) malloc(nr_g*sizeof(int));
  int *goalf0 = (int *) malloc(nr_f*sizeof(int));
  float *value0 = (float *) malloc(nr_f*sizeof(float));
  dis_State SS;
  int saved_gnum_flogic_goal, saved_gnum_fnumeric_goal;
  int *saved_gflogic_goal, *saved_gfnumeric_goal_fl;
  float *saved_gfnumeric_goal_c;
  dis_Comparator *saved_gfnumeric_goal_comp;
  
  dis_make_state(&SS, dis_gnum_ft_conn, dis_gnum_fl_conn);
  producible_solution_length = 0;
  for (j=0;j<dis_gnum_ef_conn;j++)
    if (is_producible_effect[j] > 0)
      dis_gef_conn[j].red_level = 0;
    else
      dis_gef_conn[j].red_level = -1;
  
  for (i=0;i<nr_g;i++)
    if (is_producible_fact[goal[i]] > 0)
    {
      goal0[nr_g0++] = goal[i];
      save_goals(&saved_gnum_flogic_goal, &saved_gflogic_goal, &saved_gnum_fnumeric_goal, &saved_gfnumeric_goal_fl, 
      &saved_gfnumeric_goal_c, &saved_gfnumeric_goal_comp, dis_gnum_flogic_goal, dis_gflogic_goal, dis_gnum_fnumeric_goal,
      dis_gfnumeric_goal_fl, dis_gfnumeric_goal_c, dis_gfnumeric_goal_comp);
      
      dis_gnum_flogic_goal = nr_g0;
      memcpy(dis_gflogic_goal, goal0, nr_g0*sizeof(int));
      dis_gnum_fnumeric_goal = nr_f0;
      memcpy(dis_gfnumeric_goal_fl, goalf0, nr_f0*sizeof(int));
      memcpy(dis_gfnumeric_goal_c, value0, nr_f0*sizeof(float));
      for (j=0;j<nr_f0;j++)
        dis_gfnumeric_goal_comp[j] = GEQ;

      dis_source_to_dest(&SS, &dis_ginitial_state);
      dis_source_to_dest(&dis_ginitial_state, S);
      solve_subproblem(10, 0, 2, 1, 5, 0);
      dis_source_to_dest(&dis_ginitial_state, &SS);

      restore_goals(&dis_gnum_flogic_goal, dis_gflogic_goal, &dis_gnum_fnumeric_goal, dis_gfnumeric_goal_fl, 
      dis_gfnumeric_goal_c, dis_gfnumeric_goal_comp, saved_gnum_flogic_goal, saved_gflogic_goal, saved_gnum_fnumeric_goal,
      saved_gfnumeric_goal_fl, saved_gfnumeric_goal_c, saved_gfnumeric_goal_comp);
      for (j=0;j<dis_gnum_plan_ops;j++)
        producible_solution[producible_solution_length++] = dis_gplan_ops[j];
      if (dis_gnum_plan_ops > 0)
      {
        dis_source_to_dest(S, &dis_mff_sol);
        continue;
      }
      
      while ((x = solve_producible_variables_relax(S, nr_g0, goal0, nr_f0, goalf0, value0)) == 1)
        ;
      if (x == -1)
      {
        dis_destroy_state(&SS);
        xfree(goal0);
        xfree(goalf0);
        xfree(value0);
        return 0;
      }
    }
  for (i=0;i<nr_f;i++)
    if (is_producible_fluent[goalf[i]] > 0)
    {
      goalf0[nr_f0] = goalf[i];
      value0[nr_f0++] = value[i];
      save_goals(&saved_gnum_flogic_goal, &saved_gflogic_goal, &saved_gnum_fnumeric_goal, &saved_gfnumeric_goal_fl, 
      &saved_gfnumeric_goal_c, &saved_gfnumeric_goal_comp, dis_gnum_flogic_goal, dis_gflogic_goal, dis_gnum_fnumeric_goal,
      dis_gfnumeric_goal_fl, dis_gfnumeric_goal_c, dis_gfnumeric_goal_comp);

      dis_gnum_flogic_goal = nr_g0;
      memcpy(dis_gflogic_goal, goal0, nr_g0*sizeof(int));
      dis_gnum_fnumeric_goal = nr_f0;
      memcpy(dis_gfnumeric_goal_fl, goalf0, nr_f0*sizeof(int));
      memcpy(dis_gfnumeric_goal_c, value0, nr_f0*sizeof(float));
      for (j=0;j<nr_f0;j++)
        dis_gfnumeric_goal_comp[j] = GEQ;

      dis_source_to_dest(&SS, &dis_ginitial_state);
      dis_source_to_dest(&dis_ginitial_state, S);
      solve_subproblem(10, 0, 2, 1, 5, 0);
      dis_source_to_dest(&dis_ginitial_state, &SS);

      restore_goals(&dis_gnum_flogic_goal, dis_gflogic_goal, &dis_gnum_fnumeric_goal, dis_gfnumeric_goal_fl, 
      dis_gfnumeric_goal_c, dis_gfnumeric_goal_comp, saved_gnum_flogic_goal, saved_gflogic_goal, saved_gnum_fnumeric_goal,
      saved_gfnumeric_goal_fl, saved_gfnumeric_goal_c, saved_gfnumeric_goal_comp);
      for (j=0;j<dis_gnum_plan_ops;j++)
        producible_solution[producible_solution_length++] = dis_gplan_ops[j];
      if (dis_gnum_plan_ops > 0)
      {
        dis_source_to_dest(S, &dis_mff_sol);
        continue;
      }

      while ((x = solve_producible_variables_relax(S, nr_g0, goal0, nr_f0, goalf0, value0)) == 1)
        ;
      if (x == -1)
      {
        dis_destroy_state(&SS);
        xfree(goal0);
        xfree(goalf0);
        xfree(value0);
        return 0;
      }
    }

  dis_destroy_state(&SS);
  xfree(goal0);
  xfree(goalf0);
  xfree(value0);
  return 1;
}

void set_bvar_init(dis_State *S)
{
  int j, k;
  dis_State S0;
  
  dis_make_state(&S0, dis_gnum_ft_conn, dis_gnum_fl_conn);
  dis_source_to_dest(&S0, S);
  for (j=0;j<num_bottleneck_var;j++)
  {
    for (k=0;k<S->num_F;k++)
      if (j == group_id(S->F[k]))
        break;
    if (k < S->num_F)
    {
      bvar_init[j] = S->F[k];
      S->F[k] = S->F[--S->num_F];
    }
    else
    {
      num_bottleneck_var = 0;
      dis_source_to_dest(S, &S0);
      break;
//      fprintf(stderr, "State group is not present in the initial state\n");
//	exit(1);
    }
  }
  dis_destroy_state(&S0);
}

int psearch()
{
  int i, j;
  dis_State S0;
  
  dis_make_state(&S0, dis_gnum_ft_conn, dis_gnum_fl_conn);
  dis_source_to_dest(&S0, &dis_ginitial_state);
  set_bvar_init(&dis_ginitial_state);
  
  if (!GpG.is_durative && num_bottleneck_var > 0)
  {
    dis_ginitial_state.F[dis_ginitial_state.num_F++] = bvar_init[0];
    for (i=0;i<saved_dis_gnum_flogic_goal;i++)
    {
      dis_gflogic_goal[0] = saved_dis_gflogic_goal[i];
      dis_gnum_flogic_goal = 1;

      if (!solve_subproblem(0, 100000, 1, 1, 5, 1))
        solve_subproblem(10000, 0, 1, 1, 1, 0);
      if (dis_gnum_plan_ops > 0)
      {
        dis_source_to_dest(&dis_ginitial_state, &dis_mff_sol);
        for (j=0;j<dis_gnum_plan_ops;j++)
          GpG.esp_solution[GpG.num_esp_sol++] = dis_gplan_ops[j];
      }
    }

    for (i=0;i<saved_dis_gnum_fnumeric_goal;i++)
    {
      dis_gfnumeric_goal_fl[0] = saved_dis_gfnumeric_goal_fl[i];
      dis_gfnumeric_goal_c[0] = saved_dis_gfnumeric_goal_c[i];
      dis_gfnumeric_goal_comp[0] = saved_dis_gfnumeric_goal_comp[i];
      dis_gnum_fnumeric_goal = 1;

      if (!solve_subproblem(0, 100000, 1, 1, 5, 1))
        solve_subproblem(10000, 0, 1, 1, 1, 0);
      if (dis_gnum_plan_ops > 0)
      {
        dis_source_to_dest(&dis_ginitial_state, &dis_mff_sol);
        for (j=0;j<dis_gnum_plan_ops;j++)
          GpG.esp_solution[GpG.num_esp_sol++] = dis_gplan_ops[j];
      }
    }
    
    dis_gnum_flogic_goal = saved_dis_gnum_flogic_goal;
    memcpy(dis_gflogic_goal, saved_dis_gflogic_goal, dis_gnum_flogic_goal*sizeof(int));
    dis_gnum_fnumeric_goal = saved_dis_gnum_fnumeric_goal;
    memcpy(dis_gfnumeric_goal_fl, saved_dis_gfnumeric_goal_fl, dis_gnum_fnumeric_goal*sizeof(int));
    memcpy(dis_gfnumeric_goal_c, saved_dis_gfnumeric_goal_c, dis_gnum_fnumeric_goal*sizeof(float));
    memcpy(dis_gfnumeric_goal_comp, saved_dis_gfnumeric_goal_comp, dis_gnum_fnumeric_goal*sizeof(dis_Comparator));
    if (solve_subproblem(100000, 0, 1, 1, 1, 0))
    {
      dis_source_to_dest(&dis_ginitial_state, &dis_mff_sol);
      for (j=0;j<dis_gnum_plan_ops;j++)
        GpG.esp_solution[GpG.num_esp_sol++] = dis_gplan_ops[j];
    }
    else
    {
      GpG.num_esp_sol = 0;
      dis_source_to_dest(&dis_ginitial_state, &S0);
      dis_destroy_state(&S0);
      return 0;
    }
  }

  dis_source_to_dest(&dis_ginitial_state, &S0);
  dis_destroy_state(&S0);
  return 1;
}

int producible_main()
{
  dis_State S1, S;
  float *value = (float *) malloc(dis_gnum_fl_conn*sizeof(float));
  int *goal = (int *) malloc(dis_gnum_ft_conn*sizeof(int));
  int *goalf = (int *) malloc(dis_gnum_fl_conn*sizeof(int));
  int i, j = 0, k, nr_g, nr_f, ef;
  int *tplan = (int *) malloc(dis_MAX_PLAN_LENGTH*sizeof(int)), tl = 0;
  int *splan = (int *) malloc(dis_MAX_PLAN_LENGTH*sizeof(int)), sl = 0;
  char str[256];
  
  for (i=0;i<dis_gnum_fl_conn;i++)
    value[i] = 1000000;
  relax_producible_variables(&dis_ginitial_state, &S, value);
  if (dis_get_1P(&dis_ginitial_state) > 0)
  {
/*    model0();
    sl = GpG.num_esp_sol;
    memcpy(splan, GpG.esp_solution, sl*sizeof(int));*/
    if (solve_subproblem(1000, 0, 1, 1, 5, 1))
    {
      sl = dis_gnum_plan_ops;
      memcpy(splan, dis_gplan_ops, dis_gnum_plan_ops*sizeof(int));
//      my_printplan(sl, splan, NULL);
    }
    else
    {
      xfree(splan);
      xfree(tplan);
      xfree(value);
      xfree(goal);
      xfree(goalf);
      dis_destroy_state(&S);
      return 0;
    }
  }
  producible_solution = (int *) malloc(dis_MAX_PLAN_LENGTH*sizeof(int));
  dis_source_to_dest(&dis_ginitial_state, &S);
  dis_make_state(&S1, dis_gnum_ft_conn, dis_gnum_fl_conn);

  for (i=0;i<sl;i++)
  {
    if (!is_applicable(&S, splan[i]))
    {
      for (j=0;j<dis_gop_conn[splan[i]].num_E;j++)
      {
        ef = dis_gop_conn[splan[i]].E[j];
        nr_g = dis_gef_conn[ef].num_PC;
        for (k=0;k<dis_gef_conn[ef].num_PC;k++)
          goal[k] = dis_gef_conn[ef].PC[k];
        nr_f = dis_gef_conn[ef].num_f_PC;
        for (k=0;k<dis_gef_conn[ef].num_f_PC;k++)
        {
          goalf[k] = dis_gef_conn[ef].f_PC_fl[k];
          value[k] = dis_gef_conn[ef].f_PC_c[k];
          if (dis_gef_conn[ef].f_PC_comp[k] == GE)
            value[k] += 0.001;
        }
        dis_source_to_dest(&S1, &S);
        if (solve_producible_variables(&S1, nr_g, goal, nr_f, goalf, value))
        {
          dis_source_to_dest(&S, &S1);
          for (k=0;k<producible_solution_length;k++)
            tplan[tl++] = producible_solution[k];
          break;
        }
      }
      if (j == dis_gop_conn[splan[i]].num_E)
      {
//        dis_print_op_name(splan[i]);
//        fprintf(stderr, "\nFailed to repaired producible conditions\n");
        xfree(producible_solution);
        xfree(splan);
        xfree(tplan);
        xfree(value);
        xfree(goal);
        xfree(goalf);
        dis_destroy_state(&S);
        dis_destroy_state(&S1);
        return 0;
      }
    }
    
    if (dis_result_to_dest(&S1, &S, splan[i], -1))
    {
      tplan[tl++] = splan[i];
      dis_source_to_dest(&S, &S1);
    }
    else
    {
//      dis_print_op_name(splan[i]);
//      fprintf(stderr, " cannot be executed!!!\n");
//      exit(1);
        xfree(producible_solution);
        xfree(splan);
        xfree(tplan);
        xfree(value);
        xfree(goal);
        xfree(goalf);
        dis_destroy_state(&S);
        dis_destroy_state(&S1);
      return 0;
    }
  }
  dis_destroy_state(&S1);
  xfree(splan);

  nr_f = saved_dis_gnum_fnumeric_goal;
  for (i=0;i<saved_dis_gnum_fnumeric_goal;i++)
  {
    goalf[i] = saved_dis_gfnumeric_goal_fl[i];
    value[i] = saved_dis_gfnumeric_goal_c[i];
    if (saved_dis_gfnumeric_goal_comp[i] == GE)
      value[i] += 0.001;
  }
  nr_g = saved_dis_gnum_flogic_goal;
  for (i=0;i<saved_dis_gnum_flogic_goal;i++)
      goal[i] = saved_dis_gflogic_goal[i];

  if (!solve_producible_variables(&S, nr_g, goal, nr_f, goalf, value))
  {
    xfree(producible_solution);
    xfree(tplan);
    xfree(value);
    xfree(goal);
    xfree(goalf);
    dis_destroy_state(&S);
//    fprintf(stderr, "Stop using producible variables\n");
    return 0;
  }
  for (i=0;i<producible_solution_length;i++)
    tplan[tl++] = producible_solution[i];
  GpG.num_esp_sol = tl;
  memcpy(GpG.esp_solution, tplan, tl*sizeof(int));
  dis_source_to_dest(&dis_mff_sol, &S);

  xfree(producible_solution);
  xfree(tplan);
  xfree(value);
  xfree(goal);
  xfree(goalf);
  return 1;
}

void print_producible_variables()
{
  int i, j;

  for (i=0;i<dis_gnum_operators;i++)
  {
    is_producible_operator[i] = 1;
    for (j=0;j<dis_gnum_ef_conn;j++)
      if (strcmp(dis_goperators[i]->name, dis_gop_conn[dis_gef_conn[j].op].action->name) == 0)
        if (is_producible_effect[j] == 0)
          is_producible_operator[i] = 0;
    if (is_producible_operator[i])
      printf("%s\n", dis_goperators[i]->name);
  }
  for (i=0;i<dis_gnum_ft_conn;i++)
    if (is_producible_fact[i])
    {
      dis_print_ft_name(i);
      printf("\n");
//      printf(" %d\n", is_producible_fact[i]);
    }
  for (i=0;i<dis_gnum_fl_conn;i++)
    if (is_producible_fluent[i])
    {
      dis_print_fl_name(i);
      printf("\n");
//      printf(" %d\n", is_producible_fluent[i]);
    }
  fflush(stdout);
}

void detect_producible_variables()
{
  int i, j, fixpoint = 0, ef = 0, k = 0;
  is_producible_fact = (short *) calloc(dis_gnum_ft_conn, sizeof(short));
  is_producible_fluent = (short *) calloc(dis_gnum_fl_conn, sizeof(short));
  is_producible_effect = (short *) calloc(dis_gnum_ef_conn, sizeof(short));
  is_producible_operator = (unsigned char *) calloc(dis_gnum_operators, sizeof(unsigned char));
  num_level_producible = 0;

  while (!fixpoint)
  {
    fixpoint = 1;
    num_level_producible++;
    for (i=0;i<dis_gnum_fl_conn;i++)
    {
      if (is_producible_fluent[i] > 0)
        continue;
      if (dis_grelevant_fluents_lnf[i])
        for (j=0;j<dis_grelevant_fluents_lnf[i]->num_pF;j++)
          if (dis_grelevant_fluents_lnf[i]->pC[j] > 0 && is_producible_fluent[dis_grelevant_fluents_lnf[i]->pF[j]] > 0)
          {
            fixpoint = 0;
            is_producible_fluent[i] = num_level_producible;
            break;
          }
    }
    for (i=0;i<dis_gnum_ft_conn;i++)
    {
      if (is_producible_fact[i] > 0)
        continue;
      if (dis_gft_conn[i].num_D > 0)
        continue;
      for (j=0;j<dis_gft_conn[i].num_A;j++)
      {
        ef = dis_gft_conn[i].A[j];
        if (is_producible_effect[ef] > 0)
          break;
	if (is_applicable(&dis_ginitial_state, ef) != 1)
        {
          for (k=0;k<dis_gef_conn[ef].num_PC;k++)
            if (is_producible_fact[dis_gef_conn[ef].PC[k]] == 0)
              break;
          if (k < dis_gef_conn[ef].num_PC)
            continue;
          for (k=0;k<dis_gef_conn[ef].num_f_PC;k++)
            if (is_producible_fluent[dis_gef_conn[ef].f_PC_fl[k]] == 0)
              break;
          if (k < dis_gef_conn[ef].num_f_PC)
            continue;
        }
        for (k=0;k<dis_gef_conn[ef].num_D;k++)
          if (is_producible_fact[dis_gef_conn[ef].D[k]] == 0 && dis_gft_conn[dis_gef_conn[ef].D[k]].num_PC > 1)
            break;
        if (k < dis_gef_conn[ef].num_D)
          continue;
        for (k=0;k<dis_gef_conn[ef].num_IN;k++)
          if (is_producible_fluent[dis_gef_conn[ef].IN_fl[k]] == 0 && dis_gef_conn[ef].IN_c[k] <= 0)
            break;
        if (k < dis_gef_conn[ef].num_IN)
          continue;
        is_producible_effect[ef] = num_level_producible;
        break;
      }
      if (j < dis_gft_conn[i].num_A)
      {
        fixpoint = 0;
        is_producible_fact[i] = num_level_producible;
      }
    }
    for (i=0;i<dis_gnum_ef_conn;i++)
    {
      for (j=0;j<dis_gef_conn[i].num_PC;j++)
        if (is_producible_fact[dis_gef_conn[i].PC[j]] == 0)
          break;
      if (j < dis_gef_conn[i].num_PC)
        continue;
      for (j=0;j<dis_gef_conn[i].num_f_PC;j++)
        if (is_producible_fluent[dis_gef_conn[i].f_PC_fl[j]] == 0)
          break;
      if (j < dis_gef_conn[i].num_f_PC)
        continue;
      for (j=0;j<dis_gef_conn[i].num_D;j++)
        if (is_producible_fact[dis_gef_conn[i].D[j]] == 0 && dis_gft_conn[dis_gef_conn[ef].D[k]].num_PC > 0)
          break;          
      if (j < dis_gef_conn[i].num_D)
        continue;
      for (j=0;j<dis_gef_conn[i].num_IN;j++)
        if (is_producible_fluent[dis_gef_conn[i].IN_fl[j]] == 0 && dis_gef_conn[i].IN_c[j] <= 0)
          break;
      if (j < dis_gef_conn[i].num_IN)
        continue;
      is_producible_effect[i] = num_level_producible;
      for (j=0;j<dis_gef_conn[i].num_A;j++)
        if (is_producible_fact[dis_gef_conn[i].A[j]] == 0)
        {
          fixpoint = 0;
          is_producible_fact[dis_gef_conn[i].A[j]] = num_level_producible;
        }
      for (j=0;j<dis_gef_conn[i].num_IN;j++)
        if (dis_gef_conn[i].IN_c[j] > 0 && dis_gef_conn[i].IN_fl_[j] < 0)
          if (is_producible_fluent[dis_gef_conn[i].IN_fl[j]] == 0)
          {
            fixpoint = 0;
            is_producible_fluent[dis_gef_conn[i].IN_fl[j]] = num_level_producible;
          }
    }
  }
  
//  print_producible_variables();  
}
