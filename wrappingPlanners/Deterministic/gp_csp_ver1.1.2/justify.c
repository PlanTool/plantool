/* Greedy justification */

#include <sys/types.h>
#include <malloc.h>
#include "graphplan.h"
/* #include "utilities.h" */
/* #include "interface.h" */
#include "justify.h"

extern fact_list the_goals;
/* double elapsed_seconds(void); */	/* function in satz-rand */
int num_pruned_action;


extern hashtable_t *fact_table, *op_table; /* BM: Added */
#define STDMSG stdout
int insert_vertex_at_end(edgelist_t *, vertex_t );

/* remove a single action from the plan                       */
/* return 1: if action was successfully removed; 0: otherwise */
int 
remove_action (edgelist_t *pruned_act, edgelist_t *changed_fact, 
	       vertex_t act, int st_time)
{
  int next_time, flag, ret;
  edgelist_t e_fact, e_act, e_temp;
  vertex_t v_temp, next_fact, next_act;

  next_time = st_time + 1;
  for (e_fact = act->out_edges; e_fact; e_fact = e_fact->next) {
    /* the following checks out if the fact will be removed */
    flag = 0;
    next_fact = e_fact->endpt;
    /* if this is an added fact in previous time, leave it */
    if ((v_temp = next_fact->prev_time) != 0) 
      if (v_temp->needed == ADDED) 
	continue;
    /* check to see if it achieve the goal, if so, cannot be removed! */
    if (e_fact->endpt->needed == NECESSARY) 
      flag = NECESSARY;
     
    for (e_temp = next_fact->in_edges; e_temp; e_temp = e_temp->next) {
      v_temp = e_temp->endpt;
      if (v_temp == act) continue; 
      if (v_temp->used == 1) {	// other actions add the fact
	flag = 1;
	break;
      } 
    }
    if (flag == 1) 
      continue;
    if (flag == NECESSARY) {
      // printf ("   *** sorry, you destory the goal!\n");
      return 0;
    }
    next_fact->needed = PRUNED;	/* really prune fact */
    insert_vertex_at_end(&(changed_fact[next_time]), next_fact);
    /* add actions which will be pruned in next time step */
    for (e_act = next_fact->out_edges; e_act; e_act = e_act->next) {
      next_act = e_act->endpt;
      /* if (next_act->needed == NECESSARY) { 
	// printf ("   ***sorry, an necessary action cannot be removed\n");
	// printf("   %s\n", next_act->name);
	return 0;
	} */
      if (next_act->used == 1) {
	/* fprintf (STDMSG, "%d  %s_%d\n", num_pruned_action, 
	   next_act->name, next_time); */
	ret = insert_vertex_at_end(&(pruned_act[next_time]), next_act);
	if (!IS_NOOP(next_act) && ret)
	  num_pruned_action++;
      }	
    }
  }
  act->used = 0;	   	/* now temporarily remove action */
  for (e_temp = act->del_edges; e_temp; e_temp = e_temp->next) {
    /* add deleted facts if necessary */
    v_temp = e_temp->endpt;
    /* fact was pruned in previous time step, do nothing */
    if (v_temp->prev_time->needed == PRUNED)
      continue;
    v_temp->needed = ADDED;
    insert_vertex_at_end(&(changed_fact[next_time]), v_temp);
  }	
  return 1;
}

/* propagate fact changed in previous steps in the this step */
void 
propagate_fact (edgelist_t *facts, int st_time)
{
  int next_time = st_time + 1;
  vertex_t v_fact;
  edgelist_t e_fact;

  /*
  printf ("%u\n", facts);
  for (e_fact = facts[st_time]; e_fact; e_fact = e_fact->next) {
    printf ("%d %s\n", st_time, e_fact->endpt->name);
  }
  printf ("......\n");
  for (e_fact = facts[next_time]; e_fact;e_fact = e_fact->next) {
    printf ("%d %s\n", next_time, e_fact->endpt->name);
    } 
  */
  for (e_fact = facts[st_time]; e_fact; e_fact = e_fact->next) {
    v_fact = e_fact->endpt->next_time;
    v_fact->needed = e_fact->endpt->needed;
    insert_vertex_at_end(&(facts[next_time]), v_fact);
  }
  /*
  printf ("====\n");
  for (e_fact = facts[st_time]; e_fact; e_fact = e_fact->next) {
    printf ("%d %s\n", st_time, e_fact->endpt->name);
  }
  printf (".....\n");
  for (e_fact = facts[next_time]; e_fact;e_fact = e_fact->next) {
    printf ("%d %s\n", next_time, e_fact->endpt->name);
  }
  */ 
}	

/* restore pruned action */
void 
restore_action (edgelist_t *pruned_act, edgelist_t *changed_fact, int maxtime)
{
  vertex_t v;
  edgelist_t e;
  int i;

  for ( i = 0; i <= maxtime; i++) {
    /* restore pruned actions */
    for (e = pruned_act[i]; e; e = e->next) {
      v = e->endpt;
      // v->needed = NECESSARY;  
      if (!IS_NOOP(v)) {
	num_pruned_action--;
	// printf ("%d %s_%d\n", num_pruned_action, v->name, i);
      }
      v->used = 1;
    }
    /* restore information at facts nodes */
    for (e = changed_fact[i]; e; e = e->next) {
      e->endpt->needed = 1;	
    }
  }
}

/* justify actions */
void 
justify_action (vertex_t sact, int st_time, int maxtime)
{
  int i, flag;
  edgelist_t *pruned_act, *changed_fact;
  edgelist_t e_act;

  pruned_act = (edgelist_t *)calloc(maxtime + 1, sizeof(edgelist_t));
  changed_fact = (edgelist_t *)calloc(maxtime + 1, sizeof(edgelist_t));

  flag = 0;
  num_pruned_action++;
  insert_vertex_at_end(&(pruned_act[st_time]), sact);
  // printf("%d %s_%d\n", num_pruned_action, sact->name, st_time);
  for (i = st_time; i < maxtime; i++) {
    for (e_act = pruned_act[i]; e_act; e_act = e_act->next) {
      // printf("%d %s_%d\n", num_pruned_action, e_act->endpt->name, i);
      if (remove_action (pruned_act, changed_fact, e_act->endpt, i) == 0) {
	flag = 1;
	break;
      }
    }
    if (flag == 1) 
      break;
    
    if (pruned_act[i] != NULL) 
      propagate_fact(changed_fact, i);
    else
      break;
  }
  if (flag == 1) 
    restore_action(pruned_act, changed_fact, maxtime);
  /*
  else {
    for (i = st_time; i < maxtime; i++)
      for (e_act = pruned_act[i]; e_act; e_act = e_act->next) 
	printf ("   %s %d\n", e_act->endpt->name, i + 1); 
    printf ("   %d happy!!!\n", num_pruned_action);
  } 
  */
  for (i = 0; i <= maxtime; i++) {
    free_edgelist(pruned_act[i]);
    free_edgelist(changed_fact[i]);
  }
  free(pruned_act);
  free(changed_fact);
}

/* Mark goal facts as NECESSARY */
void mark_goals_as_necessary (int maxtime)
{
  int flag;
  fact_list temp;
  char str[100];
  edgelist_t e_act;
  vertex_t f, prev_f, a;

  for(temp = the_goals; temp; temp = temp->next) {
    instantiate_into_string(temp->item, NULL, str, 1);
    if ((f = lookup_from_table(fact_table[maxtime], str)) != NULL) { 
      f->needed = NECESSARY;
      /* backward searching the last step achieve the goal */
      for (prev_f = f->prev_time; prev_f; prev_f = prev_f->prev_time) {
	flag = 0;
	for (e_act = prev_f->out_edges; e_act; e_act = e_act->next) {
	  a = e_act->endpt;
	  if (IS_NOOP(a)) {		/* find the NOOP */
	    if (a->used == 1) 
	      prev_f->needed = NECESSARY;
	    else 
	      flag = 1;
	    break;
	  }
	}
	if (flag == 1)
	  break;
      }
    }
  }
}

/* perform greedy plan justification */
void 
justify_plan (int maxtime)
{
  vertex_t v;
  int i, checked = 0;


  fprintf (STDMSG, "Performing plan justification:\n");
  /*  elapsed_seconds(); */
  num_pruned_action = 0;
  mark_goals_as_necessary(maxtime);
  for( i = 0; i < maxtime; i++) {
    get_next(op_table[i],0);
    while((v = get_next(op_table[i], 1)) != NULL) {
      if (v->used == 1 && !IS_NOOP(v)) {
        // fprintf (STDMSG, "   %d - %s\n", i, v->name);
	checked++;
	justify_action (v, i, maxtime);
      }
    }
  }
  /*  fprintf (STDMSG, "   %d actions were pruned in %.2f seconds\n", 
      num_pruned_action, elapsed_seconds()); */
  /* BM: Modified */
  fprintf (STDMSG, " %d actions were checked,  %d actions were pruned\n", 
	   checked, num_pruned_action);
}












