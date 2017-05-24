#include "graph.h"
#include "ff.h"
#include "utility.h"
#include "output.h"

extern int gnum_tsp_vars;
extern TSP_Var_pointer gtsp_vars[];

#define NTP 3

/* shameless hack - unused*/
int real_num_A(int effect) {
  
  int i, real_adds = 0;
  
  for(i = 0; i < gef_conn[effect].num_A; i++) {
    if(gft_conn[gef_conn[effect].A[i]].num_D != 0) {
      real_adds++;
    }
  }
  return real_adds;
}

Bool eligible_action(int effect) {
  return ((gef_conn[effect].num_A == 1) &&
	  (gef_conn[effect].num_D == 1) &&
	  (gef_conn[effect].num_PC == 1));

  /*  return ((real_num_A(effect) == 1) &&
	  (gef_conn[effect].num_D == 1) &&
	  (gef_conn[effect].num_PC == 1));*/
}

void print_TSP_info() {

  int i, j, num_to_print, real_num;
  int *set;
  
  printf("\nFound %d TSP vars:\n", gnum_tsp_vars);

  for(i = 0; i < gnum_tsp_vars; i++) {
    printf("TSP var 1:\n");

    printf("\tAtoms: {");
    real_num = get_as_array(&(gtsp_vars[i]->atoms), &set);
    num_to_print = MIN(NTP, real_num);
    for(j = 0; j < num_to_print; j++) {
      print_ft_name(set[j]); 
      if (j != (num_to_print - 1)) {
	printf(", ");
      }
    }
    if(real_num > NTP) {
      printf(", ...}\n");
    }
    else {
      printf("}\n");
    }
    free(set);

    printf("\tActions: {");
    real_num = get_as_array(&(gtsp_vars[i]->actions), &set);
    num_to_print = MIN(NTP, real_num);
    for(j = 0; j < num_to_print; j++) {
      print_op_name(set[j]); 
      if (j != (num_to_print - 1)) {
	printf(", ");
      }
    }
    if(real_num > NTP) {
      printf(", ...}\n");
    }
    else {
      printf("}\n");
    }
    free(set);
  }
}

void print_path(TSP_Var_pointer tsp_var, int* path, int num_facts) {

  float cost = 0;
  int num_real_actions = 0, i;

  for(i = 0; i < num_facts - 1; i++) {
    /*    printf("Action cost from ");*/
    print_ft_name(tsp_var->atoms_reverse_map[path[i]]);
    printf("- %f -", tsp_var->distance_matrix[path[i]][path[i + 1]]);
    /*printf(" to ");*/
    /*printf(" is ");*/
    /*printf("%f\n", tsp_var->distance_matrix[path[i]][path[i + 1]]);*/
    cost += tsp_var->distance_matrix[path[i]][path[i + 1]];
    num_real_actions += tsp_var->num_actions_matrix[path[i]][path[i + 1]];
  }
  print_ft_name(tsp_var->atoms_reverse_map[path[i]]);
  printf("Total path cost = %f (%d)\n", cost, num_real_actions);
}

/* output info relevant for tsp var detection for effect i */
void print_relevant_info(int i) {
  
  int j;

  printf("operator: "); print_op_name(gef_conn[i].op);
  printf("\n");
  
  printf("pre(%d): ", gef_conn[i].num_PC); 
  for(j = 0; j < gef_conn[i].num_PC; j++) {
    print_ft_name(gef_conn[i].PC[j]); printf(" ");
  }
  printf("\n");
  
  printf("add(%d): ", gef_conn[i].num_A); 
  for(j = 0; j < gef_conn[i].num_A; j++) {
    print_ft_name(gef_conn[i].A[j]); printf(" ");
  }
  printf("\n");
  
  printf("del(%d): ", gef_conn[i].num_D); 
  for(j = 0; j < gef_conn[i].num_D; j++) {
    print_ft_name(gef_conn[i].D[j]); printf(" ");
  }
  printf("eligible: %d", eligible_action(i));
  printf("\n\n");
}

void initialize_TSP() {
  
  int i;

  constructTSPVars();

  for(i = 0; i < gnum_tsp_vars; i++) {
    floyd_warshall(gtsp_vars[i]);
  }
  /*
  for(i = 0; i < gnum_tsp_vars; i++) {
    printf("\n\n");
    printf("TSP var number %d:\n", i);
    print_TSP_by_index(i);
    printf("------------------------------------------\n");
    }*/
}

void constructTSPVars() {
  
  int i, j, effect, fact, factnum, n_elems_temp;
  int *as_array_temp;
  int twofacts[2];
  TSP_Var_pointer current_tsp_var;

  set actions_to_check;
  init_set(&actions_to_check);

  for(i = 0; i < gnum_ef_conn; i++) {
    gef_conn[i].tsp_var = INFINITY;
  }

  for(i = 0; i < gnum_ft_conn; i++) {
    gft_conn[i].tsp_var = INFINITY;
  }
  
  gnum_tsp_vars = 0;

  for(i = 0; i < gnum_ef_conn; i++) {

    if(eligible_action(i) &&
       (gef_conn[i].tsp_var == INFINITY)){

      current_tsp_var = (TSP_Var_pointer) malloc(sizeof(TSP_Var));
      
      init_set(&current_tsp_var->atoms);
      init_set(&current_tsp_var->actions);
      
      set_empty(&actions_to_check);
      add_elem(&actions_to_check, i);
      
      while(remove_one_and_return(&actions_to_check, &effect)) {
	
	gef_conn[effect].tsp_var = gnum_tsp_vars;
      
	add_elem(&current_tsp_var->actions, effect);

	/* consider add effect */
	twofacts[0] = gef_conn[effect].A[0];
	/* and delete effect */
	twofacts[1] = gef_conn[effect].D[0];
	
	for(factnum = 0; factnum < 2; factnum++) {
	  
	  fact = twofacts[factnum];

	  if(gft_conn[fact].tsp_var == INFINITY) {

	    gft_conn[fact].tsp_var = gnum_tsp_vars;
	    add_elem(&current_tsp_var->atoms, fact);
	    
	    /* other adders */
	    for(j = 0; j < gft_conn[fact].num_A; j++) {
	      if(eligible_action(gft_conn[fact].A[j]) &&
		 (gef_conn[gft_conn[fact].A[j]].tsp_var == INFINITY)) {
		gef_conn[gft_conn[fact].A[j]].tsp_var = gnum_tsp_vars;
		add_elem(&actions_to_check, gft_conn[fact].A[j]);
	      }
	    }
	    
	    /* deleters */
	    for(j = 0; j < gft_conn[fact].num_D; j++) {
	      if(eligible_action(gft_conn[fact].D[j]) &&
		 (gef_conn[gft_conn[fact].D[j]].tsp_var == INFINITY)) {
		gef_conn[gft_conn[fact].D[j]].tsp_var = gnum_tsp_vars;
		add_elem(&actions_to_check, gft_conn[fact].D[j]);
	      }
	    }
	  } 
	}
      } /* while(remove_one_and_return ... */
      
      gtsp_vars[gnum_tsp_vars] = current_tsp_var;

      /* fill in information on indexes of facts/effects within the TSP var */

      current_tsp_var->atoms_reverse_map = calloc(num_elems(&current_tsp_var->atoms), sizeof(int));
      n_elems_temp = get_as_array(&current_tsp_var->atoms, &as_array_temp);
      for(j = 0; j < num_elems(&current_tsp_var->atoms); j++) {
	/*gft_conn[current_tsp_var->atoms.elems[j]].tsp_var_index = j;*/
	gft_conn[as_array_temp[j]].tsp_var_index = j;
	/*current_tsp_var->atoms_reverse_map[j] = current_tsp_var->atoms.elems[j];*/
	current_tsp_var->atoms_reverse_map[j] = as_array_temp[j];
      }
      free(as_array_temp);


      current_tsp_var->actions_reverse_map = calloc(num_elems(&current_tsp_var->actions), sizeof(int));
      n_elems_temp = get_as_array(&current_tsp_var->actions, &as_array_temp);
      for(j = 0; j < num_elems(&current_tsp_var->actions); j++) {
	/*gef_conn[current_tsp_var->actions.elems[j]].tsp_var_index = j;*/
	gef_conn[as_array_temp[j]].tsp_var_index = j;
	/*current_tsp_var->actions_reverse_map[j] = current_tsp_var->actions.elems[j];*/
	current_tsp_var->actions_reverse_map[j] = as_array_temp[j];
      }
      free(as_array_temp);

      gnum_tsp_vars++;
    }
  }
}

void print_TSP_variable(TSP_Var_pointer var) {
    
  int i, j;
  int *as_array_temp;

  printf("Number of atoms: %d, number of actions: %d\n", 
	 num_elems(&var->atoms), num_elems(&var->actions));
  
  printf("Atoms: ");

  get_as_array(&var->atoms, &as_array_temp);
  for(i = 0; i < num_elems(&var->atoms); i++) {
    print_ft_name(as_array_temp[i]); printf(", ");
  }
  free(as_array_temp);
  

  printf("\n\nActions: ");

  get_as_array(&var->actions, &as_array_temp);
  for(i = 0; i < num_elems(&var->actions); i++) {
    print_op_name(gef_conn[as_array_temp[i]].op); printf(", ");
  }
  printf("\n\n");
  free(as_array_temp);

  for(i = 0; i < num_elems(&var->atoms); i++) {
    for(j = 0; j < num_elems(&var->atoms); j++) {
      printf("%f(%d)\t", var->distance_matrix[i][j], var->num_actions_matrix[i][j]);
    }
    printf("\n");
  }
  printf("\n\n");
}

void print_TSP_by_index(int index) {
  print_TSP_variable( gtsp_vars[index] );
}

void floyd_warshall( TSP_Var_pointer tsp_v) {

  int i, j, k, effect, from_fact, to_fact, from_fact_index, to_fact_index;
  int n_atoms = num_elems(&tsp_v->atoms);
  float path_cost;
  int *actions_array;

  /* allocate memory for distance matrix, set distance identities */
  tsp_v->distance_matrix = calloc(n_atoms, sizeof(float *));
  tsp_v->num_actions_matrix = calloc(n_atoms, sizeof(int *));;

  tsp_v->next_fact = calloc(n_atoms, sizeof(int *));
  for (i = 0; i < n_atoms; i++) {
    tsp_v->distance_matrix[i] = calloc(n_atoms, sizeof(float));
    tsp_v->num_actions_matrix[i] = calloc(n_atoms, sizeof(int *));;
    tsp_v->next_fact[i] = calloc(n_atoms, sizeof(int));
    for (j = 0; j < n_atoms; j++) {
      tsp_v->distance_matrix[i][j] = ((i == j) ? 0 : INFINITY);
      tsp_v->num_actions_matrix[i][j] = ((i == j) ? 0 : INFINITY);
      tsp_v->next_fact[i][j] = INFINITY;
    }
  }
  
  /* add in actions that already exist */

  get_as_array(&tsp_v->actions, &actions_array);

  for(i = 0; i < num_elems(&tsp_v->actions); i++) {

    effect = actions_array[i];

    from_fact = gef_conn[effect].D[0];
    to_fact = gef_conn[effect].A[0];

    from_fact_index = gft_conn[from_fact].tsp_var_index;
    to_fact_index = gft_conn[to_fact].tsp_var_index;
    
    tsp_v->distance_matrix[from_fact_index][to_fact_index] = get_cost(effect);
    tsp_v->num_actions_matrix[from_fact_index][to_fact_index] = 1;
    tsp_v->next_fact[from_fact_index][to_fact_index] = to_fact_index;
  }
  free(actions_array);
  
  /* main loop for FW */
  for(k = 0; k < n_atoms; k++) {
    for(i = 0; i < n_atoms; i++) {
      for(j = 0; j < n_atoms; j++) {
	path_cost = PLUS(tsp_v->distance_matrix[i][k], tsp_v->distance_matrix[k][j]);
	if(LESS(path_cost, tsp_v->distance_matrix[i][j])) {
	  tsp_v->distance_matrix[i][j] = path_cost;
	  tsp_v->num_actions_matrix[i][j] = tsp_v->num_actions_matrix[i][k] + tsp_v->num_actions_matrix[k][j];
	  tsp_v->next_fact[i][j] = tsp_v->next_fact[i][k];
	}
      }
    }
  }
}

float solve_one_tsp(set_pointer tsp_as, int start_pos, int *num_real_actions, int *first_real_goal) {

  int i, j,start_pos_index, temp;
  float old_distances_sum, old_distances_sum_base, new_distances_sum, pathcost;
  int *path_array, *incoming_array;
  int n_facts = num_elems(tsp_as);
  Bool done = FALSE;
  TSP_Var_pointer current_tsp_var;

  int num_changes = 0;
  
  if (n_facts == 0) {
    *first_real_goal = INFINITY;
    *num_real_actions = 0;
    return 0.0;
  }

  current_tsp_var = gtsp_vars[gft_conn[start_pos].tsp_var];

  path_array = calloc(n_facts + 1, sizeof(int));

  /* get fact indices within the MVV */
  start_pos_index = gft_conn[start_pos].tsp_var_index;
  path_array[0] = start_pos_index;

  get_as_array(tsp_as, &incoming_array);
  for(i = 0; i < n_facts; i++) {
    path_array[i + 1] = gft_conn[incoming_array[i]].tsp_var_index;
  }
  free(incoming_array);
  
  /* treat a single variable as a special case */
  if (n_facts == 1) {
    *first_real_goal = current_tsp_var->next_fact[path_array[0]][path_array[1]];
    *first_real_goal = current_tsp_var->atoms_reverse_map[*first_real_goal];
    *num_real_actions = current_tsp_var->num_actions_matrix[path_array[0]][path_array[1]];
    pathcost = current_tsp_var->distance_matrix[path_array[0]][path_array[1]];
    free(path_array);
    return pathcost;
  }
  /*
  printf("************\n");
  printf("initial: ");
  print_path(current_tsp_var, path_array, n_facts + 1);
  */

  while (!done) {
    done = TRUE;

    for(i = 1; i < n_facts; i++) {

      old_distances_sum_base = current_tsp_var->distance_matrix[path_array[i-1]][path_array[i]];
      old_distances_sum_base += current_tsp_var->distance_matrix[path_array[i]][path_array[i+1]];

      for(j = i + 1; j < n_facts + 1; j++) {
	old_distances_sum = old_distances_sum_base;
	new_distances_sum = current_tsp_var->distance_matrix[path_array[i - 1]][path_array[j]];
	
	if (j != i + 1) {	/* if no overlap */

	  old_distances_sum += current_tsp_var->distance_matrix[path_array[j-1]][path_array[j]];

	  new_distances_sum += current_tsp_var->distance_matrix[path_array[j]][path_array[i + 1]];
	  new_distances_sum += current_tsp_var->distance_matrix[path_array[j-1]][path_array[i]];

	  if (j != (n_facts)) {   /* if j is not last visited point */
	    old_distances_sum += current_tsp_var->distance_matrix[path_array[j]][path_array[j+1]];
	    new_distances_sum += current_tsp_var->distance_matrix[path_array[i]][path_array[j+1]];
	  }
	  else { /* j is end of sequence */

	  }
	}

	else { /* overlap */

	  new_distances_sum += current_tsp_var->distance_matrix[path_array[j]][path_array[i]];

	  if (j != (n_facts)) {   /* if j is not last visited point */
	    old_distances_sum += current_tsp_var->distance_matrix[path_array[j]][path_array[j+1]];
	    new_distances_sum += current_tsp_var->distance_matrix[path_array[i]][path_array[j+1]];
	  }
	  else { /* j is end of sequence */

	  }
	}

	if( old_distances_sum > new_distances_sum ) {

	  temp = path_array[i];
	  path_array[i] = path_array[j];
	  path_array[j] = temp;

	  done = FALSE;
	  num_changes++;
	  break;
	}
      }
    }
  }

  pathcost = 0;
  *num_real_actions = 0;
  for(i = 0; i < n_facts; i++) {
    pathcost += current_tsp_var->distance_matrix[path_array[i]][path_array[i + 1]];
    *num_real_actions += current_tsp_var->num_actions_matrix[path_array[i]][path_array[i + 1]];
  }

  *first_real_goal = current_tsp_var->next_fact[path_array[0]][path_array[1]];
  *first_real_goal = current_tsp_var->atoms_reverse_map[*first_real_goal];

  free(path_array);
  
  return pathcost;
} 
