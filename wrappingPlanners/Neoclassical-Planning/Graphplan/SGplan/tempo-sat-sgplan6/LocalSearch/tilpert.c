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

 * File: tilpert.c

 * Authors:  Chih-wei Hsu and Benjamin W. Wah

 *

 *********************************************************************/
/*
# Copyright (C) 2006, Board of Trustees of the University of Illinois.
#
# The program is copyrighted by the University of Illinois, and should
# not be distributed without prior approval.  Commercialization of this
# product requires prior licensing from the University of Illinois.
# Commercialization includes the integration of this code in part or
# whole into a product for resale.
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Author: C. W. Hsu, B. W. Wah, R. Y. Huang, and Y. X. Chen  
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
*/
#define BIGFLOAT 1000000000
void dis_print_fl_name(int);
// here we assume tif appears in pairs
// with an add and a del
char *print_Fact_string(Fact *f, char *out_string)
{
	int j;
	char *temp = NULL;
	
  sprintf (out_string, "(%s", gpredicates[f->predicate]);
  for (j = 0; j < garity[f->predicate]; j++)
    {
	strcat (out_string, " ");
      if (f->args[j] >= 0)
	strcat (out_string, gconstants[(f->args)[j]]);
      else
	{
	  sprintf (temp, "x%d", DECODE_VAR (f->args[j]));
	  strcat (out_string, temp);
	}
    }
  strcat (out_string, ")");
  return out_string;
}

int notdis_find_tif(int fact, float *begin, float *end)
{      
	int i; 
	char str[MAX_LENGTH], s2[MAX_LENGTH];    
	Fact* f;    
	Bool f1 = FALSE, f2 = FALSE;
                
	print_ft_name_string(fact, s2);
                                    
    for(i=0; i<gnum_tils; i++) {
        f = &(gtils[i].pred);
        print_Fact_string( f, str);
        if(strcmp(str, s2) == 0) {
            if( gtils[i].negated) {
                (*end) = (float)gtils[i].time;
                f2 = TRUE;
            } else {
                (*begin) = (float)gtils[i].time;
                f1 = TRUE;
            }
        }
    }            

	if(f1 || f2)
		return 1;
	return -1;
}


int find_tif(int fact, float *begin, float *end)
{      
	int i; 
	char str[MAX_LENGTH], s2[MAX_LENGTH];    
	dis_Fact* f;    
	Bool f1 = FALSE, f2 = FALSE;
                
	dis_print_ft_name_string(fact, s2);
                                    
    for(i=0; i<dis_gnum_tils; i++) {
        f = &(dis_gtils[i].pred);
        dis_print_dis_Fact_string( f, str);
        //printf("%s %d %d\n", str, dis_gtils[i].time, dis_gtils[i].negated);
        if(strcmp(str, s2) == 0) {
            if( dis_gtils[i].negated) {
                (*end) = dis_gtils[i].time;
                f2 = TRUE;
            } else {
                (*begin) = dis_gtils[i].time;
                f1 = TRUE;
            }
        }
    }            

    if((!f1) && (!f2)) {
        return -1;
    }
    if(f1 && f2) { 
      return 1;
    }
    
    fprintf(stderr, "two timed literals expected."); exit(1);
}

void notdis_init_timed_initial()
{
    int i;
    TIF x;
    char *s;
    float begin = 0 , end = BIGFLOAT;
    
    for(i=0; i<gnum_ft_conn; i++) {
                x.action = i;
                x.pred = i;
	if (notdis_find_tif(x.pred, &begin, &end) > 0)
	{
                x.begin = begin;
                x.end = end;
                dis_gtif[dis_gnum_tif++] = x; 
                if(dis_gnum_tif > MAX_NUM_TIF) {
                    /*printf("exit 116");*/ exit(1);
                } 
	}
    }           
   
}

void init_timed_initial()
{
    int i;
    char *s;
    TIF x;
    float begin = 0, end = BIGFLOAT;
    
    dis_gnum_tif=0;
    
    for(i=0; i<dis_gnum_ef_conn; i++) {
        s = dis_gop_conn[dis_gef_conn[i].op].action->name;
        if(s&&strncmp(s, "timed-initial-literals", 21)==0) {
        //    dis_print_op_name(i); 
        //    fprintf(stderr, " %d %d\n", dis_gef_conn[i].num_A, dis_gef_conn[i].num_D);
            if(dis_gef_conn[i].num_D>0) {
                x.action = i;
                x.pred = dis_gef_conn[i].D[0];
                find_tif(x.pred, &begin, &end);
                x.begin = begin;
                x.end = end;
                dis_gtif[dis_gnum_tif++] = x; 
                if(dis_gnum_tif > MAX_NUM_TIF) {
                    printf("exit 116"); exit(1);
                } 
            }   
        }        
    }           
   
}

dis_Bool dis_depend_efs(int a, int b) 
{
	int i, j;
	dis_Action *ac1, *ac2;
	
/*	if (GpG.SearchModal == 6)
	{
		ac1 = dis_gop_conn[dis_gef_conn[a].op].action;
		ac2 = dis_gop_conn[dis_gef_conn[a].op].action;
		if (strcmp(ac1->name, "SEND_IMAGE") == 0 && 
		strcmp(ac2->name, "SEND_IMAGE") == 0)
		if (strcmp((char *)dis_gconstants[(ac1)->name_inst_table[1]] 
		,(char *)dis_gconstants[(ac2)->name_inst_table[1]]) == 0)
			return TRUE;
	}*/
	
	for(i=0; i<dis_gef_conn[a].num_PC; i++) 
	{
		for(j=0; j<dis_gef_conn[b].num_A; j++) 
			if(dis_gef_conn[a].PC[i] == dis_gef_conn[b].A[j]) 
				return TRUE;
		for(j=0; j<dis_gef_conn[b].num_D; j++)
			if(dis_gef_conn[a].PC[i] == dis_gef_conn[b].D[j])
				return TRUE;
	}
	for(i=0; i<dis_gef_conn[b].num_PC; i++)
	{
		for(j=0; j<dis_gef_conn[a].num_A; j++) 
			if(dis_gef_conn[b].PC[i] == dis_gef_conn[a].A[j])
				return TRUE;
		for(j=0; j<dis_gef_conn[a].num_D; j++)
			if(dis_gef_conn[b].PC[i] == dis_gef_conn[a].D[j])
				return TRUE;
	} 
	return FALSE; 
}

Bool depend_efs(int a, int b, Bool relax) 
{
	int i, j;
	for(i=0; i<gef_conn[a].num_PC; i++) 
	{
		for(j=0; j<gef_conn[b].num_A; j++) 
			if(gef_conn[a].PC[i] == gef_conn[b].A[j]) 
				return TRUE;
		if (relax)
			continue;
		for(j=0; j<gef_conn[b].num_D; j++)
			if(gef_conn[a].PC[i] == gef_conn[b].D[j])
				return TRUE;
	}
	for(i=0; i<gef_conn[b].num_PC; i++)
	{
		for(j=0; j<gef_conn[a].num_A; j++) 
			if(gef_conn[b].PC[i] == gef_conn[a].A[j])
				return TRUE;
		if (relax)
			continue;
		for(j=0; j<gef_conn[a].num_D; j++)
			if(gef_conn[b].PC[i] == gef_conn[a].D[j])
				return TRUE;
	} 
	return FALSE; 
}

Bool dis_numPC_depend_efs(int a, int b) 
{
	int i, j;
	for(i=0; i<dis_gef_conn[a].num_f_PC; i++)
		for(j=0; j<dis_gef_conn[b].num_f_PC; j++)
			if (dis_gef_conn[a].f_PC_fl[i] == dis_gef_conn[b].f_PC_fl[j]) 
            			return TRUE;
	return FALSE; 
}

int find_ef_tif(int ef, float *begin, float *end)
{                    
	int i, j, f, ret = -1;
	
	*begin = 0;
	*end = BIGFLOAT;
	for(i=0; i<gef_conn[ef].num_PC; i++) 
	{
		f = gef_conn[ef].PC[i];
		for(j=0; j<dis_gnum_tif; j++) 
			if(f == dis_gtif[j].pred) 
			{
				if (*begin < dis_gtif[j].begin)
					(*begin) = dis_gtif[j].begin;
				if (*end > dis_gtif[j].end)
					(*end) = dis_gtif[j].end;
				ret = i;
			}
	}
	if (gef_conn[ef].sf)
	{
	for(i=0; i<gef_conn[ef].sf->num_PC_overall; i++) 
	{
		f = gef_conn[ef].sf->PC_overall[i];
		for(j=0; j<dis_gnum_tif; j++) 
			if(f == dis_gtif[j].pred) 
			{
				if (*begin < dis_gtif[j].begin)
					(*begin) = dis_gtif[j].begin;
				if (*end > dis_gtif[j].end - dis_gef_conn[ef].duration)
					(*end) = dis_gtif[j].end - dis_gef_conn[ef].duration;
				ret = i;
			}
	}
	for(i=0; i<gef_conn[ef].sf->num_PC_end; i++) 
	{
		f = gef_conn[ef].sf->PC_end[i];
		for(j=0; j<dis_gnum_tif; j++) 
			if(f == dis_gtif[j].pred) 
			{
				if (*begin < dis_gtif[j].begin - dis_gef_conn[ef].duration)
					(*begin) = dis_gtif[j].begin - dis_gef_conn[ef].duration;
				if (*end > dis_gtif[j].end - dis_gef_conn[ef].duration)
					(*end) = dis_gtif[j].end - dis_gef_conn[ef].duration;
				ret = i;
			}
	}
	}

	return ret;
}

int dis_find_ef_tif(int ef, float *begin, float *end)
{                    
	int i, j, f, ret = -1;
	
	*begin = 0;
	*end = BIGFLOAT;
	for(i=0; i<dis_gef_conn[ef].num_PC; i++) 
	{
		f = dis_gef_conn[ef].PC[i];
		for(j=0; j<dis_gnum_tif; j++) 
			if(f == dis_gtif[j].pred) 
			{
				if (*begin < dis_gtif[j].begin)
					(*begin) = dis_gtif[j].begin;
				if (*end > dis_gtif[j].end)
					(*end) = dis_gtif[j].end;
				ret = i;
			}
	}
	return ret;
}

void dis_PERT(Bool numeric)
{
	int op1, op2, ef1, ef2;
	Bool dep;

	for (op1=0;op1<dis_gnum_plan_ops;op1++)
	{
		ef1 = dis_gop_conn[dis_gplan_ops[op1]].E[0];
		GpG.pert_endtime[op1] = dis_gef_conn[ef1].duration;
		for(op2=0;op2<op1;op2++)
		{
			ef2 = dis_gop_conn[dis_gplan_ops[op2]].E[0];
			dep = dis_depend_efs(ef1, ef2);
			if (numeric)
				dep = dep || dis_numPC_depend_efs(ef1, ef2);
			if (dep) 
				if (GpG.pert_endtime[op2] + dis_gef_conn[ef1].duration 
				> GpG.pert_endtime[op1]) 
					GpG.pert_endtime[op1] 
					= GpG.pert_endtime[op2] + dis_gef_conn[ef1].duration + MIN_DELTA_TIME; 
		}
	}
} 

Bool PERT(Bool numeric)
{  
	int i, j, ef1, ef2, res;
	Bool dep, ret = TRUE;
	float begin, end;
	Fact *f, *g;

	for (i = 0; i < GpG.num_esp_sol; i++)
	{
		ef1 = gop_conn[GpG.esp_solution[i]].E[0];
		res = find_ef_tif(ef1, &begin, &end);
		GpG.pert_endtime[i] = dis_gef_conn[ef1].duration + begin;
		for(j=0; j<i; j++)
		{
			ef2 = gop_conn[GpG.esp_solution[j]].E[0];
			dep = dis_depend_efs(ef1, ef2);
/*			if (numeric)
				dep ||= numPC_depend_efs(ef1, ef2);*/
			if (dep) 
				if (GpG.pert_endtime[j] + dis_gef_conn[ef1].duration 
				> GpG.pert_endtime[i]) 
					GpG.pert_endtime[i] 
					= GpG.pert_endtime[j] + dis_gef_conn[ef1].duration + MIN_DELTA_TIME; 
		}
		if (GpG.pert_endtime[i] > end + dis_gef_conn[ef1].duration)
			ret = FALSE;
	}
	return ret;
}
struct event
{
	int op;
	float time;
};
struct event *queue;
int num_events = 0;
void insert_event(struct event *t)
{
	int i;
	
	num_events++;
	for (i=num_events-1;i>0;i--)
	{
		if (queue[i-1].time <= t->time)
			break;
		queue[i] = queue[i-1];
	}
	queue[i] = *t;
}
int apply_action(dis_State *S, dis_State *S0, int op, int option)
{
	int out, j, *tempfl;
	float *tempc;
	dis_EfConn temp = dis_gef_conn[op];
	tempc = (float *) malloc(sizeof(float)*temp.num_IN);
	tempfl = (int *) malloc(sizeof(int)*temp.num_IN);
	memcpy(tempc, temp.IN_c, sizeof(float)*temp.num_IN);
	memcpy(tempfl, temp.IN_fl, sizeof(int)*temp.num_IN);
	
    if (option == 1)
    {
      dis_gef_conn[op].num_PC = 0;
      dis_gef_conn[op].num_f_PC = 0;
      for (j=0;j<dis_gef_conn[op].num_IN/2;j++)
      {
        dis_gef_conn[op].IN_fl[j] = dis_gef_conn[op].IN_fl[2*j+1];
        dis_gef_conn[op].IN_c[j] = dis_gef_conn[op].IN_c[2*j+1];
      } 
      dis_gef_conn[op].num_IN /= 2;  
    }
    else
    {
      dis_gef_conn[op].num_A = 0;
      dis_gef_conn[op].num_D = 0;
      for (j=0;j<dis_gef_conn[op].num_IN/2;j++)
      {
        dis_gef_conn[op].IN_fl[j] = dis_gef_conn[op].IN_fl[2*j];
        dis_gef_conn[op].IN_c[j] = dis_gef_conn[op].IN_c[2*j];
      } 
      dis_gef_conn[op].num_IN /= 2;  
    }

	out = dis_result_to_dest(S, S0, op, -1);
	memcpy(temp.IN_c, tempc, sizeof(float)*temp.num_IN);
	memcpy(temp.IN_fl, tempfl, sizeof(int)*temp.num_IN);
	dis_gef_conn[op] = temp;
        free(tempc);
        free(tempfl);
	return out;
}

void dis_fl_pred_PERT()
{
	int i, j, k, a, *done = (int *) calloc(GpG.num_esp_sol, sizeof(int));
	char *s;
	struct event temp;
	float dummy;
	dis_State S, S0;
	queue = (struct event *) malloc(sizeof(struct event)*2*GpG.num_esp_sol);

	for (i=0;i<dis_gnum_ef_conn;i++)
	{
		s = dis_gop_conn[i].action->name;
		if (strncmp(s, "timed-initial-literals", 21) == 0)
		{
			temp.op = i;
			if (dis_gef_conn[i].num_A)
				find_tif(dis_gef_conn[i].A[0], &temp.time, &dummy);
			else
				find_tif(dis_gef_conn[i].D[0], &dummy, &temp.time);
			insert_event(&temp);
		}
	}
	for (i = 0; i < GpG.num_esp_sol; i++)
	{
		a = GpG.esp_solution[i];
		s = dis_gop_conn[a].action->name;
		if (strncmp(s, "timed-initial-literals", 21) == 0) 
		{
//			dis_delete_gplan_ops(i);
			done[i] = 1;
			GpG.esp_solution[i] = -1;
//			fprintf(stderr, "%d %d - ", i, GpG.num_esp_sol);
//			i--;
		}    
	}

	dummy = 0;
	dis_make_state(&S, dis_gnum_ft_conn, dis_gnum_fl_conn);
	dis_make_state(&S0, dis_gnum_ft_conn, dis_gnum_fl_conn);
	dis_source_to_dest(&S, &dis_ginitial_state);
	k = 0;
	for (i=0;;i++)
	{
		a = 0;
		for (j=0;j<GpG.num_esp_sol;j++)
		{
			if (done[j])
				continue;
			if (apply_action(&S0, &S, GpG.esp_solution[j], 0))
			{
				dis_source_to_dest(&S, &S0);
				temp.op = GpG.esp_solution[j];
				temp.time = dummy + dis_gef_conn[GpG.esp_solution[j]].duration;
				insert_event(&temp);
				GpG.pert_endtime[j] = temp.time + 0.001*k;
				k++;
				done[j] = 1;
				a = 1;
			}
		}
		if (i < num_events)
		{
			dummy = queue[i].time;
			apply_action(&S0, &S, queue[i].op, 1);
			dis_source_to_dest(&S, &S0);
			a = 1;
		}
		if (a == 0)
			break;
	}
	
	free(queue);
	free(done);
}
 
void dis_fl_pred_PERT1()
{
	int i, j, a, b, res;
	float begin, end, *bt, *et;
	char *s;
 
	for (i = 0; i < dis_gnum_plan_ops; i++)
	{
		a = dis_gplan_ops[i];
		s = dis_gop_conn[a].action->name;
		if (strncmp(s, "timed-initial-literals", 21) == 0) 
		{
			dis_delete_gplan_ops(i);
			i--;
		}    
	}

	bt = (float *) malloc(sizeof(float)*dis_gnum_plan_ops);
	et = (float *) malloc(sizeof(float)*dis_gnum_plan_ops);
	for (i=0;i<dis_gnum_plan_ops;i++)
	{
		res = dis_find_ef_tif(dis_gplan_ops[i], &begin, &end);
		if (res >= 0)
		{
			bt[i] = dis_gef_conn[dis_gplan_ops[i]].duration + begin;
			et[i] = dis_gef_conn[dis_gplan_ops[i]].duration + end - 0.5;
		}
		else
		{
			bt[i] = dis_gef_conn[dis_gplan_ops[i]].duration;
			et[i] = dis_gef_conn[dis_gplan_ops[i]].duration + BIGFLOAT;
		}	
	}
	
restart:
	for (i = 0; i < dis_gnum_plan_ops; i++)
	{
		if (et[i] < bt[i])
		{
			printf("PERT for tif failed\n");
			exit(0);
		}
		a = dis_gplan_ops[i] ;
		
		GpG.pert_endtime[i] = bt[i];
		for(j=0; j<i; j++) 
		{
			b = dis_gplan_ops[j];
			if (dis_depend_efs(a,b)) 
				if (GpG.pert_endtime[j] + dis_gef_conn[a].duration 
				> GpG.pert_endtime[i]) 
				{
					GpG.pert_endtime[i] = 
					GpG.pert_endtime[j] + dis_gef_conn[a].duration + 10*MIN_DELTA_TIME; 
					if (GpG.pert_endtime[i] > et[i])
					{
						et[j] = et[i] - dis_gef_conn[a].duration;
						goto restart;
					}
				}

			if (!dis_depend_efs(a,b) && dis_numPC_depend_efs(a,b))
				if (GpG.pert_endtime[j] + dis_gef_conn[a].duration 
				> GpG.pert_endtime[i])
				{
					if (GpG.pert_endtime[j] + dis_gef_conn[a].duration 
					<= et[i])
						GpG.pert_endtime[i] 
						= GpG.pert_endtime[j] + dis_gef_conn[a].duration + 10*MIN_DELTA_TIME; 
					else
						GpG.pert_endtime[i] = et[i];
				}
		}
	}
  
	free(bt);
	free(et);
}
