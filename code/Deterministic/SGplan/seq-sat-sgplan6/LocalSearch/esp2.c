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

 * File: esp2.c 

 * Authors:  Chih-wei Hsu and Benjamin W. Wah

 *

 *********************************************************************/
#include "ff.h"
#include "dis_ff.h"
#include "lpg.h"
#include "mutex.h"
#include "esp1.h"
#include "dis_relax.h"
#define MAX_OPS 128
#include <math.h>

typedef enum _MutexType
{
	NO_MUTEX = 0,
	MUTEX = 1,
	POSS_MUTEX1 = 2,
	POSS_MUTEX2 = 3,
	PCstart_PCstart,
	PCstart_PCend,
	PCstart_PCoverall,
	PCstart_Dstart,
	PCstart_Dend,
	PCoverall_PCstart,
	PCoverall_PCend,
	PCoverall_PCoverall,
	PCoverall_Dstart,
	PCoverall_Dend,
	PCend_PCstart,
	PCend_PCend,
	PCend_PCoverall,
	PCend_Dstart,
	PCend_Dend,
	Astart_Dstart,
	Astart_Dend,
	Aend_Dstart,
	Aend_Dend,
	Dstart_PCstart,
	Dstart_PCend,
	Dstart_PCoverall,
	Dstart_Astart,
	Dstart_Aend,
	Dend_PCstart,
	Dend_PCend,
	Dend_PCoverall,
	Dend_Astart,
	Dend_Aend,	
} MutexType;

State saved_end_state;
int num_subgoal, current_subgoal;
int *num_subplan_ops, **subplan_ops, *sg;
float **scheduled_time, *deadline;
float **lambda, **objs, num_mutex;
int esploop;

float makespan = dis_BIG_INT, max_time = dis_BIG_INT, violation = dis_BIG_INT;
float schedule[1000];
int Nop, dis_ops[1000];

extern void source_to_dest(State *, State *);
extern dis_Bool dis_depend_efs(int, int );
extern int *relax_lused_O, relax_lnum_used_O;
extern void mff_bridge(State *, State *);
extern int find_ef_tif(int , float *, float *);
extern void print_state(State);
extern void dis_print_plan(void);
extern void print_op_name(int);
extern void print_op(int);
extern void print_ft(int);

void print_S(State S)
{
	int i;
	
	for (i=0;i<S.num_F;i++)
		print_ft(S.F[i]);
	printf("------------------------------\n");
}
void print_dis_State(dis_State *S)
{
	int i;
	
	for (i=0;i<S->num_F;i++)
		print_ft(S->F[i]);
	printf("------------------------------\n");
}

void printplan(int n, int *op, float *time)
{
	int i;
	
	for (i=0;i<n;i++)
	{
		printf("%.5f:\t", time[i]);
		print_op(op[i]);
	}
	printf("------------------------------\n");
}
void print_all_plans(void)
{
	int i;
	
	for (i=0;i<num_subgoal;i++)
		printplan(num_subplan_ops[i], subplan_ops[i], scheduled_time[i]);
	printf("\n");
}

float objvalue(void);
void print_obj_lambda(void)
{
	int i, j;
	float o = objvalue();

	printf("obj = %f # of mutex = %.1f\n", o, num_mutex);
	for (i=0;i<num_subgoal;i++)
	{
		for (j=0;j<num_subgoal;j++)
			printf("%f\t", objs[i][j]);
		printf("\n");
	}
	for (i=0;i<num_subgoal;i++)
	{
		for (j=0;j<num_subgoal;j++)
			printf("%f\t", lambda[i][j]);
		printf("\n");
	}
}

MutexType mutex_op(int op1, float s1, int op2, float s2)
{
	int i, j, ef1, ef2;
	float e1, e2;

	if (op1 == op2 && s1 == s2)
		return NO_MUTEX;
	ef1 = gop_conn[op1].E[0];
	ef2 = gop_conn[op2].E[0];
	e1 = s1 + gef_conn[ef1].duration;
	e2 = s2 + gef_conn[ef2].duration;	
	if (!are_mutex_ops(ef1, ef2))
		return NO_MUTEX;
	if (s1 > e2 || s2 > e1)
	{
	if (s1 > e2)
		for (i=0;i<dis_gef_conn[ef1].num_PC;i++)
			for (j=0;j<dis_gef_conn[ef2].num_D;j++)
				if (dis_gef_conn[ef1].PC[i] == dis_gef_conn[ef2].D[j])
					return POSS_MUTEX1;
	if (s2 > e1)
		for (i=0;i<dis_gef_conn[ef2].num_PC;i++)
			for (j=0;j<dis_gef_conn[ef1].num_D;j++)
				if (dis_gef_conn[ef2].PC[i] == dis_gef_conn[ef1].D[j])
					return POSS_MUTEX2;
	return NO_MUTEX;
	}
	else
		return MUTEX;

	for (i=0;i<gef_conn[ef1].num_PC;i++)
	{
		if (gef_conn[ef1].PC[i] < 0)
			continue;

		if (s1 >= e2)
		for (j=0;j<gef_conn[ef2].num_D;j++)
		{
			if (gef_conn[ef2].D[j] < 0)
				continue;
			if (gef_conn[ef1].PC[i] == gef_conn[ef2].D[j])
				return PCstart_Dend;
		}

		if (s1 == s2)
		for (j=0;j<gef_conn[ef2].num_PC;j++)
		{
			if (gef_conn[ef2].PC[j] < 0)
				continue;
			if (GET_BIT (FT_FT_mutex[gef_conn[ef1].PC[i]], gef_conn[ef2].PC[j]))
				return PCstart_PCstart;
		}

		if (gef_conn[ef2].sf)
		{
		if (s1 >= s2)
		for (j=0;j<gef_conn[ef2].sf->num_D_start;j++)
		{
			if (gef_conn[ef2].sf->D_start[j] < 0)
				continue;
			if (gef_conn[ef1].PC[i] == gef_conn[ef2].sf->D_start[j])
				return PCstart_Dstart;
		}

		if (s1 >= s2 && s1 <= e2)
		for (j=0;j<gef_conn[ef2].sf->num_PC_overall;j++)
		{
			if (gef_conn[ef2].sf->PC_overall[j] < 0)
				continue;
			if (GET_BIT (FT_FT_mutex[gef_conn[ef1].PC[i]], gef_conn[ef2].sf->PC_overall[j]))
				return PCstart_PCoverall;
		}

		if (s1 == e2)
		for (j=0;j<gef_conn[ef2].sf->num_PC_end;j++)
		{
			if (gef_conn[ef2].sf->PC_end[j] < 0)
				continue;
			if (GET_BIT (FT_FT_mutex[gef_conn[ef1].PC[i]], gef_conn[ef2].sf->PC_end[j]))
				return PCstart_PCend;
		}
		}
	}

	if (gef_conn[ef1].sf)
	{
	for (i=0;i<gef_conn[ef1].sf->num_PC_overall;i++)
	{
		if (gef_conn[ef1].sf->PC_overall[i] < 0)
			continue;
		
		if (e1 >= e2)
		for (j=0;j<gef_conn[ef2].num_D;j++)
		{
			if (gef_conn[ef2].D[j] < 0)
				continue;
			if (gef_conn[ef1].sf->PC_overall[i] == gef_conn[ef2].D[j])
				return PCoverall_Dend;
		}

		if (s1 <= s2 && e1 >= s2)
		for (j=0;j<gef_conn[ef2].num_PC;j++)
		{
			if (gef_conn[ef2].PC[j] < 0)
				continue;
			if (GET_BIT (FT_FT_mutex[gef_conn[ef1].sf->PC_overall[i]], gef_conn[ef2].PC[j]))
				return PCoverall_PCstart;
		}
	
		if (gef_conn[ef2].sf)
		{
		if (e1 >= s2)
		for (j=0;j<gef_conn[ef2].sf->num_D_start;j++)
		{
			if (gef_conn[ef2].sf->D_start[j] < 0)
				continue;
			if (gef_conn[ef1].sf->PC_overall[i] == gef_conn[ef2].sf->D_start[j])
				return PCoverall_Dstart;
		}

		if (s1 <= e2 && s2 <= e1)
		for (j=0;j<gef_conn[ef2].sf->num_PC_overall;j++)
		{
			if (gef_conn[ef2].sf->PC_overall[j] < 0)
				continue;
			if (GET_BIT (FT_FT_mutex[gef_conn[ef1].sf->PC_overall[i]], gef_conn[ef2].sf->PC_overall[j]))
				return PCoverall_PCoverall;
		}

		if (s1 <= e2 && e1 >= e2)
		for (j=0;j<gef_conn[ef2].sf->num_PC_end;j++)
		{
			if (gef_conn[ef2].sf->PC_end[j] < 0)
				continue;
			if (GET_BIT (FT_FT_mutex[gef_conn[ef1].sf->PC_overall[i]], gef_conn[ef2].sf->PC_end[j]))
				return PCoverall_PCend;
		}
		}		
	}

	for (i=0;i<gef_conn[ef1].sf->num_PC_end;i++)
	{
		if (gef_conn[ef1].sf->PC_end[i] < 0)
			continue;

		if (e1 >= e2)
		for (j=0;j<gef_conn[ef2].num_D;j++)
		{
			if (gef_conn[ef2].D[j] < 0)
				continue;
			if (gef_conn[ef1].sf->PC_end[i] == gef_conn[ef2].D[j])
				return PCend_Dend;
		}

		if (e1 == s2)
		for (j=0;j<gef_conn[ef2].num_PC;j++)
		{
			if (gef_conn[ef2].PC[j] < 0)
				continue;
			if (GET_BIT (FT_FT_mutex[gef_conn[ef1].sf->PC_end[i]], gef_conn[ef2].PC[j]))
				return PCend_PCstart;
		}

		if (gef_conn[ef2].sf)
		{
		if (e1 >= s2)
		for (j=0;j<gef_conn[ef2].sf->num_D_start;j++)
		{
			if (gef_conn[ef2].sf->D_start[j] < 0)
				continue;
			if (gef_conn[ef1].sf->PC_end[i] == gef_conn[ef2].sf->D_start[j])
				return PCend_Dstart;
		}

		if (e1 >= s2 && e1 <= e2)
		for (j=0;j<gef_conn[ef2].sf->num_PC_overall;j++)
		{
			if (gef_conn[ef2].sf->PC_overall[j] < 0)
				continue;
			if (GET_BIT (FT_FT_mutex[gef_conn[ef1].sf->PC_end[i]], gef_conn[ef2].sf->PC_overall[j]))
				return PCend_PCoverall;
		}

		if (e1 == e2)
		for (j=0;j<gef_conn[ef2].sf->num_PC_end;j++)
		{
			if (gef_conn[ef2].sf->PC_end[j] < 0)
				continue;
			if (GET_BIT (FT_FT_mutex[gef_conn[ef1].sf->PC_end[i]], gef_conn[ef2].sf->PC_end[j]))
				return PCend_PCend;
		}
		}
	}
	}

/*	for (i=0;i<gef_conn[ef1].num_A;i++)
	{
		if (gef_conn[ef1].A[i] < 0)
			continue;

		if (e1 == e2)
		for (j=0;j<gef_conn[ef2].num_D;j++)
		{
			if (gef_conn[ef2].D[j] < 0)
				continue;
			if (gef_conn[ef1].A[i] == gef_conn[ef2].D[j])
				return Aend_Dend;
		}

		if (gef_conn[ef2].sf)
		if (e1 >= s2 && e1 <= e2)
		for (j=0;j<gef_conn[ef2].sf->num_D_start;j++)
		{
			if (gef_conn[ef2].sf->D_start[j] < 0)
				continue;
			if (gef_conn[ef1].A[i] == gef_conn[ef2].sf->D_start[j])
				return Aend_Dstart;
		}
	}

	if (gef_conn[ef1].sf)
	{
	for (i=0;i<gef_conn[ef1].sf->num_A_start;i++)
	{
		if (gef_conn[ef1].sf->A_start[i] < 0)
			continue;
		
		if (s1 <= e2 && e1 >= e2)
		for (j=0;j<gef_conn[ef2].num_D;j++)
		{
			if (gef_conn[ef2].D[j] < 0)
				continue;
			if (gef_conn[ef1].sf->A_start[i] == gef_conn[ef2].D[j])
				return Astart_Dend;
		}

		if (gef_conn[ef2].sf)
		if (s1 <= e2 && s2 <= e1)
		for (j=0;j<gef_conn[ef2].sf->num_D_start;j++)
		{
			if (gef_conn[ef2].sf->D_start[j] < 0)
				continue;
			if (gef_conn[ef1].sf->A_start[i] == gef_conn[ef2].sf->D_start[j])
				return Astart_Dstart;
		}
	}
	}*/

	for (i=0;i<gef_conn[ef1].num_D;i++)
	{
		if (gef_conn[ef1].D[i] < 0)
			continue;

		if (e1 <= s2)
		for (j=0;j<gef_conn[ef2].num_PC;j++)
		{
			if (gef_conn[ef2].PC[j] < 0)
				continue;
			if (gef_conn[ef1].D[i] == gef_conn[ef2].PC[j])
				return Dend_PCstart;
		}

/*		if (e1 == e2)
		for (j=0;j<gef_conn[ef2].num_A;j++)
		{
			if (gef_conn[ef2].A[j] < 0)
				continue;
			if (gef_conn[ef1].D[i] == gef_conn[ef2].A[j])
				return Dend_Aend;
		}*/

		if (gef_conn[ef2].sf)
		{
/*		if (e1 >= s2 && e1 <= e2)
		for (j=0;j<gef_conn[ef2].sf->num_A_start;j++)
		{
			if (gef_conn[ef2].sf->A_start[j] < 0)
				continue;
			if (gef_conn[ef1].D[i] == gef_conn[ef2].sf->A_start[j])
				return Dend_Astart;
		}*/

		if (e1 <= e2)
		for (j=0;j<gef_conn[ef2].sf->num_PC_overall;j++)
		{
			if (gef_conn[ef2].sf->PC_overall[j] < 0)
				continue;
			if (gef_conn[ef1].D[i] == gef_conn[ef2].sf->PC_overall[j])
				return Dend_PCoverall;
		}

		if (e1 == e2)
		for (j=0;j<gef_conn[ef2].sf->num_PC_end;j++)
		{
			if (gef_conn[ef2].sf->PC_end[j] < 0)
				continue;
			if (gef_conn[ef1].D[i] == gef_conn[ef2].sf->PC_end[j])
				return Dend_PCend;
		}
		}
	}

	if (gef_conn[ef1].sf)
	for (i=0;i<gef_conn[ef1].sf->num_D_start;i++)
	{
		if (gef_conn[ef1].sf->D_start[i] < 0)
			continue;
		
		if (s1 <= s2)
		for (j=0;j<gef_conn[ef2].num_PC;j++)
		{
			if (gef_conn[ef2].PC[j] < 0)
				continue;
			if (gef_conn[ef1].sf->D_start[i] == gef_conn[ef2].PC[j])
				return Dstart_PCstart;
		}

/*		if (s1 <= e2 && e1 >= e2)
		for (j=0;j<gef_conn[ef2].num_A;j++)
		{
			if (gef_conn[ef2].A[j] < 0)
				continue;
			if (gef_conn[ef1].sf->D_start[i] == gef_conn[ef2].A[j])
				return Dstart_Aend;
		}*/

		if (gef_conn[ef2].sf)
		{
/*		if (s1 <= e2 && s2 <= e1)
		for (j=0;j<gef_conn[ef2].sf->num_A_start;j++)
		{
			if (gef_conn[ef2].sf->A_start[j] < 0)
				continue;
			if (gef_conn[ef1].sf->D_start[i] == gef_conn[ef2].sf->A_start[j])
				return Dstart_Astart;
		}*/

		if (s1 <= e2)
		for (j=0;j<gef_conn[ef2].sf->num_PC_overall;j++)
		{
			if (gef_conn[ef2].sf->PC_overall[j] < 0)
				continue;
			if (gef_conn[ef1].sf->D_start[i] == gef_conn[ef2].sf->PC_overall[j])
				return Dstart_PCoverall;
		}

		if (s1 <= e2)
		for (j=0;j<gef_conn[ef2].sf->num_PC_end;j++)
		{
			if (gef_conn[ef2].sf->PC_end[j] < 0)
				continue;
			if (gef_conn[ef1].sf->D_start[i] == gef_conn[ef2].sf->PC_end[j])
				return Dstart_PCend;
		}
		}
	}

	return NO_MUTEX;
}

int myPERT(int n, int x, int *op, float *time)
{
	int i, j, ef1, ef2, res;
	float begin, end;
	
	for (i=x;i<n;i++)
	{
		ef1 = dis_gop_conn[op[i]].E[0];
		time[i] = 0;	
		for (j=0;j<i;j++)
		{
			ef2 = dis_gop_conn[op[j]].E[0];
			if (dis_depend_efs(ef1, ef2))
				if (time[i] < time[j] + dis_gef_conn[ef2].duration + MIN_DELTA_TIME)
					time[i] = time[j] + dis_gef_conn[ef2].duration + MIN_DELTA_TIME;
		}
		res = find_ef_tif(ef1, &begin, &end);
		time[i] = time[i] < begin ? begin : time[i];
		if (time[i] > end)
			return i;
	}

	return -1;
}
    
void initialize_all(void)
{
	int i, j;

	num_subplan_ops = (int *) malloc(sizeof(int)*num_subgoal);
	subplan_ops = (int **) malloc(sizeof(int *)*num_subgoal);
	for (i=0;i<num_subgoal;i++)
	{
		subplan_ops[i] = (int *) malloc(sizeof(int)*MAX_OPS);
		num_subplan_ops[i] = 0;
	}
	sg = (int *) malloc(sizeof(int)*saved_end_state.num_F);
	for (i=0;i<saved_end_state.num_F;i++)
		sg[i] = i;

	deadline = (float *) malloc(sizeof(float)*saved_end_state.num_F);
	// not general
	max_time = 0;
	for (i=0;i<saved_end_state.num_F;i++)
	{
		deadline[i] = gtils[saved_end_state.num_F-i-1].time;
		if (max_time < deadline[i])
			max_time = deadline[i];
	}
		
	scheduled_time = (float **) malloc(sizeof(float *)*num_subgoal);
	for (i=0;i<num_subgoal;i++)
		scheduled_time[i] = (float *) malloc(sizeof(float)*MAX_OPS);

	lambda = (float **) malloc(sizeof(float *)*num_subgoal);
	objs = (float **) malloc(sizeof(float *)*num_subgoal);
	for (i=0;i<num_subgoal;i++)
	{
		lambda[i] = (float *) malloc(sizeof(float)*num_subgoal);
		objs[i] = (float *) malloc(sizeof(float)*num_subgoal);
		for (j=0;j<num_subgoal;j++)
			lambda[i][j] = 1;
//			lambda[i][j] = 0;
		lambda[i][i] = 0;
	}
}

int copy_subplan(int x)
{
	int i, j, min, op;
	float t;
	
	if (myPERT(dis_gnum_plan_ops, x, dis_gplan_ops, scheduled_time[current_subgoal]) != -1)
		exit(0);
	num_subplan_ops[current_subgoal] = dis_gnum_plan_ops;
	memcpy(subplan_ops[current_subgoal], dis_gplan_ops, sizeof(int)*dis_gnum_plan_ops);
	for (i=0;i<dis_gnum_plan_ops-1;i++)
	{
		min = i;
		for (j=i+1;j<dis_gnum_plan_ops;j++)
			if (scheduled_time[current_subgoal][j] < scheduled_time[current_subgoal][min])
				min = j;
		if (min != i)
		{
			op = subplan_ops[current_subgoal][i];
			subplan_ops[current_subgoal][i] = subplan_ops[current_subgoal][min];
			subplan_ops[current_subgoal][min] = op;
			t = scheduled_time[current_subgoal][i];
			scheduled_time[current_subgoal][i] = scheduled_time[current_subgoal][min];
			scheduled_time[current_subgoal][min] = t;
		}
	}
	return 1;
}
void update_lambda(void)
{
	int i, j;
	
	for (i=0;i<num_subgoal;i++)
		for (j=0;j<num_subgoal;j++)
			lambda[i][j] += 0.1*objs[i][j];
}

void copy_final_plan(void)
{
	int i, j, k;
	float d;
	
	GpG.num_esp_sol = 0;
	for (i=0;i<num_subgoal;i++)
		for (j=0;j<num_subplan_ops[i];j++)
		{
			d = gef_conn[gop_conn[subplan_ops[i][j]].E[0]].duration;
			for (k=0;k<GpG.num_esp_sol;k++)
				if (GpG.esp_solution[k] == subplan_ops[i][j] &&
				fabs(GpG.pert_endtime[k] - scheduled_time[i][j] - d) < 2*MIN_DELTA_TIME)
					break;
			if (k == GpG.num_esp_sol)
			{
				GpG.esp_solution[GpG.num_esp_sol] = subplan_ops[i][j];
				GpG.pert_endtime[GpG.num_esp_sol++] = scheduled_time[i][j] + d;
			}
		}
}

void free_all(void)
{
	int i;
	
	for (i=0;i<num_subgoal;i++)
	{
		free(lambda[i]);
		free(objs[i]);
		free(scheduled_time[i]);
		free(subplan_ops[i]);
	}
	free(lambda);
	free(objs);
	free(scheduled_time);
	free(subplan_ops);
	free(num_subplan_ops);
	free(sg);
	free(deadline);
}

float objvalue(void)
{
	int i, j, k, l, ef, u, v, ef1, ef2, x, y;
	float ret, t, *e;
	MutexType m;
	
	for (i=0;i<num_subgoal;i++)
		for (j=0;j<num_subgoal;j++)
			objs[i][j] = 0;	
	e = (float *) malloc(sizeof(float)*saved_end_state.num_F);
	for (i=0;i<saved_end_state.num_F;i++)
	{
		e[i] = -1;
		for (k=num_subplan_ops[sg[i]]-1;k>=0;k--)
		{
			ef = gop_conn[subplan_ops[sg[i]][k]].E[0];
			for (u=0;u<dis_gef_conn[ef].num_A;u++)
				if (dis_gef_conn[ef].A[u] == saved_end_state.F[i])
				{
					e[i] = scheduled_time[sg[i]][k] + dis_gef_conn[ef].duration;
					k = 0;
					break;
				}
		}
	}
	
	for (i=0;i<num_subgoal;i++)
		for (j=0;j<num_subgoal;j++)
		{
			if (j == i)
				continue;
			for (k=0;k<num_subplan_ops[i];k++)
			{
				ef = gop_conn[subplan_ops[i][k]].E[0];
				for (l=0;l<num_subplan_ops[j];l++)
				{
					ef2 = gop_conn[subplan_ops[j][l]].E[0];
					m = mutex_op(subplan_ops[i][k], scheduled_time[i][k], subplan_ops[j][l], scheduled_time[j][l]);
					if (m != NO_MUTEX)
					{
						if (m == POSS_MUTEX1)
						{
							for (u=0;u<dis_gef_conn[ef].num_PC;u++)
								for (v=0;v<dis_gef_conn[ef2].num_D;v++)
									if (dis_gef_conn[ef2].D[v] == dis_gef_conn[ef].PC[u])
									{
										t = 0;	// precondition is from initial state
										for (x=k-1;x>=0;x--)
										{
											ef1 = gop_conn[subplan_ops[i][x]].E[0];
											for (y=0;y<dis_gef_conn[ef1].num_A;y++)
												if (dis_gef_conn[ef1].A[y] == dis_gef_conn[ef].PC[u])
													break;
											if (y < dis_gef_conn[ef1].num_A)
											{
												t = scheduled_time[i][x];
												break;
											}
										}
										if (scheduled_time[j][l] + gef_conn[ef2].duration > t)
										{
											objs[i][j]++;
											u = dis_gef_conn[ef].num_PC - 1;
											v = dis_gef_conn[ef2].num_D - 1;
										}
									}
						}
						else
						if (m == POSS_MUTEX2)
						{
							for (u=0;u<dis_gef_conn[ef2].num_PC;u++)
								for (v=0;v<dis_gef_conn[ef].num_D;v++)
									if (dis_gef_conn[ef].D[v] == dis_gef_conn[ef2].PC[u])
									{
										t = 0;	// precondition is from initial state
										for (x=l-1;x>=0;x--)
										{
											ef1 = gop_conn[subplan_ops[j][x]].E[0];
											for (y=0;y<dis_gef_conn[ef1].num_A;y++)
												if (dis_gef_conn[ef1].A[y] == dis_gef_conn[ef2].PC[u])
													break;
											if (y < dis_gef_conn[ef1].num_A)
											{
												t = scheduled_time[j][x];
												break;
											}
										}
										if (scheduled_time[i][k] + gef_conn[ef].duration > t)
										{
											objs[i][j]++;
											u = dis_gef_conn[ef2].num_PC - 1;
											v = dis_gef_conn[ef].num_D - 1;
										}
									}
						}
						else
						{
							objs[i][j]++;
						}
					}
				}
			}
		}
	for (i=0;i<num_subgoal;i++)
		for (j=0;j<saved_end_state.num_F;j++)
		{
			if (sg[j] == i)
				continue;
			for (k=0;k<num_subplan_ops[i];k++)
			{
				ef = gop_conn[subplan_ops[i][k]].E[0];
				if (scheduled_time[i][k] >= e[j])					
					for (l=0;l<dis_gef_conn[ef].num_D;l++)
						if (dis_gef_conn[ef].D[l] == saved_end_state.F[j])
						{
							objs[i][sg[j]]++;
							objs[sg[j]][i]++;
						}
			}
		}
		
	ret = num_mutex = 0;
	for (i=0;i<num_subgoal;i++)
		for (j=0;j<num_subgoal;j++)
		{
			ret += lambda[i][j]*objs[i][j];
			num_mutex += objs[i][j]/2;
		}
	free(e);
	return ret;
}

void copy_plan(int *ndst, int *dst, float *time_dst, int nsrc, int *src, float *time_src)
{
	*ndst = nsrc;
	memcpy(dst, src, sizeof(int)*nsrc);
	memcpy(time_dst, time_src, sizeof(float)*nsrc);
}

float make_subproblem(State *sub)
{
	int i;
	float end = dis_INFINITY;

	for (sub->num_F=i=0;i<saved_end_state.num_F;i++)
		if (sg[i] == current_subgoal)
		{
			sub->F[sub->num_F++] = saved_end_state.F[i];
			end = end > deadline[i] ? deadline[i] : end;
		}
	return end;
}

typedef struct _MergeList
{
	float o;
	int m, n;
} MergeList;

void esp1(State *start_state, State *end_state)
{
	int num_old_plan, old_plan[MAX_OPS];
	int i, j, k, changed, l;
	int nsub = 0, m = -1, n = -1;
	float oldobj, o, old_time[MAX_OPS], opt;
	State subgoal;
	MergeList *merge_list, temp;

	source_to_dest(&saved_end_state, end_state);
	num_subgoal = end_state->num_F;
	merge_list = (MergeList *) malloc(num_subgoal*(num_subgoal-1)/2*sizeof(MergeList));
	initialize_all();

	GpG.SearchModal = -103;
	GpG.is_til = 1;
	GpG.num_esp_sol = 0;
	max_bfs_iter = 10000;
	bridge_option = 1;
	mff_bridge(start_state, end_state);
	bridge_option = 0;
	GpG.SearchModal = 3;
	
	for (current_subgoal=0;current_subgoal<num_subgoal;current_subgoal++)
	{
		copy_subplan(0);
		for (i=0;i<GpG.sol_state.num_F;i++)
			if (GpG.sol_state.F[i] == end_state->F[current_subgoal])
				break;
		if (i == GpG.sol_state.num_F)
			num_subplan_ops[current_subgoal] = -num_subplan_ops[current_subgoal];
	}
	
	for (current_subgoal=0;current_subgoal<num_subgoal;current_subgoal++)
	{
		if (num_subplan_ops[current_subgoal] > 0)
			continue;
//		num_subplan_ops[current_subgoal] = -num_subplan_ops[current_subgoal];
		num_subplan_ops[current_subgoal] = 0;
		esploop = 0;
		max_hc_iter = 10000;
subres:
		make_subproblem(&subgoal);
		mff_bridge(start_state, &subgoal);
		nsub++;
			
		if (dis_gnum_plan_ops == -1 || !copy_subplan(0))
		{
			num_subplan_ops[current_subgoal]--;
//			max_hc_iter += 10000;
//			goto subres;
			printf("Get stuck in subgoal %d\n", current_subgoal);
			fflush(stdout);
//			exit(0);
		}
	}

//	print_all_plans();
	oldobj = objvalue();
//	update_lambda();
	printf("obj = %f # of mutex = %.1f # of subproblems solved = %d\n", oldobj, num_mutex, nsub);
	fflush(stdout);

	while ((oldobj = objvalue()) > 0)
	{
		changed = 0;
		
		for (current_subgoal=0;current_subgoal<num_subgoal;current_subgoal++)
		{
			opt = make_subproblem(&subgoal);
			copy_plan(&num_old_plan, old_plan, old_time, num_subplan_ops[current_subgoal], subplan_ops[current_subgoal], scheduled_time[current_subgoal]);
			
			for (i=0;i<num_subgoal;i++)
			{
				for (j=num_subplan_ops[i];j>=0;j--)
				{
					if (j > 0)
						if (scheduled_time[i][j-1] > opt)
							continue;
					if (j == 0 && i != 0)
						continue;
						
					copy_plan(&num_subplan_ops[current_subgoal], subplan_ops[current_subgoal], scheduled_time[current_subgoal], j, subplan_ops[i], scheduled_time[i]);
					esploop = 1;
					max_hc_iter = 1000;
					mff_bridge(start_state, &subgoal);
					nsub++;
					
					if (dis_gnum_plan_ops > j)
					{
						copy_subplan(j);
						if ((o = objvalue()) < oldobj)
						{
//							print_all_plans();
							printf("obj = %f # of mutex = %.1f # of subproblems solved = %d\n", o, num_mutex, nsub);
							fflush(stdout);
							oldobj = o;
							if (oldobj == 0)
								current_subgoal = num_subgoal - 1;
							changed = 1;
							goto out;
						}
					}
					copy_plan(&num_subplan_ops[current_subgoal], subplan_ops[current_subgoal], scheduled_time[current_subgoal], num_old_plan, old_plan, old_time);
				}
			}
		out:
		changed=changed;				
		}
		
		if (changed)
			update_lambda();
		else
		{
//			print_all_plans();
			objvalue();
//			print_obj_lambda();
			fflush(stdout);

			k = l = 0;
			for (i=0;i<num_subgoal-1;i++)
				for (j=i+1;j<num_subgoal;j++)
					if (lambda[i][j]*objs[i][j] > 0)
					{
						merge_list[k].o = lambda[i][j]*objs[i][j];
						merge_list[k].m = i;
						merge_list[k].n = j;
						k++;
					}
re2:
			if (l >= k)
			{
				printf("Cannot solve any merged subproblems\n");
				fflush(stdout);
				exit(0);
			}
			
			opt = 0;
			for (i=j=l;i<k;i++)
				if (merge_list[i].o > merge_list[j].o)
					j = i;
			if (l != j)
			{
				temp = merge_list[j];
				merge_list[j] = merge_list[l];
				merge_list[l] = temp;
			}
			
			m = merge_list[l].m;
			n = merge_list[l].n;
			l++;
			printf("Merge subgoal %d and %d\n", m, n);
			fflush(stdout);
			
			current_subgoal = m;
			sg[n] = m;
			copy_plan(&num_old_plan, old_plan, old_time, num_subplan_ops[m], subplan_ops[m], scheduled_time[m]);
			num_subplan_ops[m] = 0;
			opt = make_subproblem(&subgoal);

			esploop = 0;
			max_hc_iter = 100000;
			mff_bridge(start_state, &subgoal);
			sg[n] = n;
			nsub++;
			
			if (dis_gnum_plan_ops == -1 || !copy_subplan(0))
			{
				printf("Get stuck in merging subgoal %d and %d\n", m, n);
				fflush(stdout);
				copy_plan(&num_subplan_ops[m], subplan_ops[m], scheduled_time[m], num_old_plan, old_plan, old_time);
				goto re2;
			}
			
			copy_plan(&num_subplan_ops[n], subplan_ops[n], scheduled_time[n], num_subplan_ops[m], subplan_ops[m], scheduled_time[m]);
//			print_all_plans();
			oldobj = objvalue();
			printf("obj = %f # of mutex = %.1f # of subproblems solved = %d\n", oldobj, num_mutex, nsub);
			fflush(stdout);
		}
	}
	copy_final_plan();
	free_all();
	free(merge_list);
}

float localobj(int n, int *op, float *time)
{
	int i, j, k, ef;
	float ret = 0, *e;
	
	e = (float *) malloc(sizeof(float)*saved_end_state.num_F);
	for (i=0;i<saved_end_state.num_F;i++)
	{
		e[i] = -1;
		for (k=num_subplan_ops[sg[i]]-1;k>=0;k--)
		{
			ef = gop_conn[subplan_ops[sg[i]][k]].E[0];
			for (j=0;j<dis_gef_conn[ef].num_A;j++)
				if (dis_gef_conn[ef].A[j] == saved_end_state.F[i])
				{
					e[i] = scheduled_time[sg[i]][k] + dis_gef_conn[ef].duration;
					k = 0;
					break;
				}
		}
	}
	
	for (k=0;k<n;k++)
	{
		ef = dis_gop_conn[op[k]].E[0];
		for (i=0;i<num_subgoal;i++)
		{
			if (i == current_subgoal)
				continue;
			for (j=0;j<num_subplan_ops[i];j++)
				if (mutex_op(op[k], time[k], subplan_ops[i][j], scheduled_time[i][j]))
					ret += lambda[current_subgoal][i];
		}
		for (i=0;i<saved_end_state.num_F;i++)
		{
			if (sg[i] == current_subgoal)
				continue;
			if (time[k] >= e[i])
				for (j=0;j<dis_gef_conn[ef].num_D;j++)
					if (dis_gef_conn[ef].D[j] == saved_end_state.F[i])
						ret += lambda[current_subgoal][i];
		}
	}
	for (k=0;k<relax_lnum_used_O;k++)
	{
		ef = dis_gop_conn[relax_lused_O[k]].E[0];
		for (i=0;i<num_subgoal;i++)
		{
			if (i == current_subgoal)
				continue;
			for (j=0;j<num_subplan_ops[i];j++)
				if (are_mutex_ops(ef, dis_gop_conn[subplan_ops[i][j]].E[0]))
					ret += lambda[current_subgoal][i];
		}
	}

	return ret;
}
