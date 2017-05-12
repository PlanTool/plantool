
#include "bucket.h"
#include "search.h"
#include <string.h>
#include "file.h"



/*const int PRUNING 10000*/
int total_states;
int compare_int(const void *i, const void * j){
    if (*((int*)i) > *((int*)j))
	return 1;
    else if (*((int*)i) == *((int*)j))
	return 0;
    else
	return -1;
}

 
void insert_in_bucket(Bucket *b, State *S, State *father, int operator){

    if (b->size == b->MAX_SIZE){
	flush_bucket(b);
    }

    if (b->SORT) qsort(S->F, S->num_F, sizeof(int), compare_int);

    /*printf("Duration = %f\n", S->stt);*/
    source_to_dest(&b->states[b->size], S);
    source_to_dest(&b->preds[b->size], father);
    b->states[b->size].pred_index = b->size;
    b->states[b->size].op = operator;
    b->size++;
}

void allocate_states_in_bucket(Bucket *b){
    int i;
    for (i=0; i<b->MAX_SIZE; i++){
	allocate_state(&b->states[i]);
	allocate_state(&b->preds[i]);
#ifdef VERIFY_FLUSH
	allocate_state(&b->backup_states[i]);
	allocate_state(&b->backup_preds[i]);
#endif
    }
}

void allocate_state(State* s){
    s->F = ( int * ) calloc( gnum_ft_conn, sizeof( int ) ); 
    s->f_D = ( Bool * ) calloc( gnum_fl_conn, sizeof( Bool ) ); 
    s->f_V = ( float * ) calloc( gnum_fl_conn, sizeof( float ) );
    
}

void free_state(State* s){
    free(s->F);
    free(s->f_D);
    free(s->f_V);
}
 

void flush_bucket(Bucket *b){
    if(b->size == 0)
	return;
    int duplicates = 0;
    openAppend(b->s_file);
    /*jumpToMiddle(b->s_file, 6 ,1461840192);*/
    if (b->SORT) qsort(b->states, b->size, sizeof(State),
		       compare_states);
    int i=0;
    int comparison = 0;
    EPS eps;
#ifdef VERIFY_FLUSH
    int count = 0;
    int file_part = b->s_file->currentPart;
    int middle_byte = b->s_file->bytesWritten;
#endif
    eps.state = &b->states[0];
    eps.pred = &b->preds[b->states[0].pred_index];
    if ((eps.state->num_F > gnum_ft_conn) || (eps.pred->num_F > gnum_ft_conn) || (eps.state->num_F <= 0) || (eps.pred->num_F <= 0))
	printf("%d-th: Attempt to write invalid states with numF = %d; pred.num_F = %d\n", i, eps.state->num_F, eps.pred->num_F);
    else{
	write_eps_to_file(b->s_file, &eps);
#ifdef VERIFY_FLUSH
	source_to_dest(&b->backup_states[count], &b->states[0]);
	source_to_dest(&b->backup_preds[count++],&b->preds[b->states[0].pred_index] );
#endif
    }
    for (i = 1 ; i < b->size; i++){
	if (b->SORT) comparison = compare_states(&b->states[i-1], &b->states[i]);
	else comparison = -1;
	
	if(comparison < 0){
	    eps.state = &b->states[i];
	    eps.pred = &b->preds[b->states[i].pred_index];
	    if ((eps.state->num_F > gnum_ft_conn) || (eps.pred->num_F > gnum_ft_conn) || (eps.state->num_F <= 0) || (eps.pred->num_F <= 0))
		printf("%d-th: Attempt to write invalid states with numF = %d; pred.num_F = %d\n", i, eps.state->num_F, eps.pred->num_F);
	    else{
		write_eps_to_file(b->s_file, &eps);
#ifdef VERIFY_FLUSH
		source_to_dest(&b->backup_states[count], &b->states[i]);
		source_to_dest(&b->backup_preds[count++],&b->preds[b->states[i].pred_index] );
#endif		
/*
		printf("State in the backup: \n");
		print_F_vector( &b->backup_states[i]);
		printf("original state: \n");
		print_F_vector( &b->states[i]);
		
		printf("Flushed Pred: \n");
		print_F_vector( &b->backup_preds[i]);*/
	    }
	}else if (comparison > 0){
	    printf("\n\n Bucket::flush_bucket *** WARNING!!! Previous element was greater\n");
	    printf("                          Legally it can happen during large merge   \n");
	    printf("                          Ignore it then!                            \n");
	}
	else{
	    duplicates ++ ;
	}
    }
    closeFile(b->s_file);	
    fflush(stdout);
    b->duplicates += duplicates;
    b->inserted += b->size;
    /*    if (b->SORT) 
      printf("Flushed (%d, %d): Inserted = %d; Duplicates = %d; Flushed = %d; Total Written = %u\n", b->g, b->h, b->size, duplicates, 
	     b->size - duplicates, b->inserted - b->duplicates);
    */
#ifdef VERIFY_FLUSH
    verify_buffer_flush(b, count, file_part,  middle_byte);
#endif
    b->size = 0;
}

Bool verify_buffer_flush(Bucket *b, int count, int file_part, int middle_byte){
    exit(1);
    if (b->SORT) printf("Verifying Bucket .. ");
    fflush(stdout);
    /* Since that was a writing bucket, the following initialization will not harm */
    init_bucket_file_only(b);
    /* Now I have to read in the states that I flushed to it. */
    /* .. but only count many states */
    int last_MAX_SIZE = b->MAX_SIZE;
    b->MAX_SIZE = count;
    /* Move the reading pointer to the last place */
    if (openRead(b->s_file) == 0) {
	printf("Could not open the file for reading. \n");
	return 0;
    }
    jumpToMiddle(b->s_file, file_part, middle_byte);
    b->size = 0;
    while(b->size < b->MAX_SIZE && 
	  (read_state_from_file(b->s_file, &b->states[b->size++]))){
	/*print_F_vector( &b->states[b->size-1]);*/
	/*printf("%u State read; Reading pred. \n", b->inserted + b->size);*/
	read_state_from_file(b->s_file, &b->preds[b->size-1]);
	/*print_F_vector( &b->preds[b->size-1]);
	  printf("Pred read: Reading state \n");*/
    }
    b->size--;

    Bool FLAG = FALSE;
    int i;
    for(i=0; i< count; i++){

	if ((compare_states(&b->states[i], &b->backup_states[i]) != 0) || 
	    (compare_states(&b->preds[i], &b->backup_preds[i]) != 0) ){
	       printf("State %d did not match!\n", i);
	    printf("Flushed State: \n");
	    print_F_vector( &b->backup_states[i]);
	    printf("Read State: \n");
	    print_F_vector( &b->states[i]);
    
	    printf("Flushed Pred: \n");
	    print_F_vector( &b->backup_preds[i]);
	    printf("Read Pred: \n");
	    print_F_vector( &b->preds[i]);
	    printf("\n----\n");
	    FLAG = TRUE;
	}
    }
    
    closeFile(b->s_file);
    EPS eps;
    if (FLAG){
	printf("There were invalid states! Writing the whole buffer back again!\n");
	openOverWrite(b->s_file);
	jumpToMiddle(b->s_file, file_part, middle_byte);
	for(i = 0; i< count; i++){
	    eps.state = &b->backup_states[i];
	    eps.pred =  &b->backup_preds[i];
	    if ((eps.state->num_F > gnum_ft_conn) || (eps.pred->num_F > gnum_ft_conn) || (eps.state->num_F <= 0) || (eps.pred->num_F <= 0))
		printf("%d-th: Attempt to write invalid states with numF = %d; pred.num_F = %d\n", i, eps.state->num_F, eps.pred->num_F);
	    else
		write_eps_to_file(b->s_file, &eps);
	}
	closeFile(b->s_file);
    }

    b->MAX_SIZE = last_MAX_SIZE;
    init_bucket_file_only(b);
    if (b->SORT) printf(" .. Verification done!\n");
    return TRUE;
}

float cost(const State* s){
    float lprune = 0.0;
    int i;
/*
    int loop = (glnf_metric.num_pF ) / 4;
    int rest = (glnf_metric.num_pF ) % 4;
	

    for ( i = 0; i < loop; i+=4 ) {
	if ( isDurative == TRUE && glnf_metric.pF[i] == -2 ) {  
		lprune += gtt * s->stt ;
	} else {
		lprune += (glnf_metric.pC[i] * s->f_V[glnf_metric.pF[i]]);   
	}

	if (isDurative == TRUE &&  glnf_metric.pF[i+1] == -2 ) {  
		lprune += gtt * s->stt ;
	} else {
		lprune += (glnf_metric.pC[i+1] * s->f_V[glnf_metric.pF[i+1]]);   
	}

	if (isDurative == TRUE &&  glnf_metric.pF[i+2] == -2 ) {  
		lprune += gtt * s->stt ;
	} else {
		lprune += (glnf_metric.pC[i+2] * s->f_V[glnf_metric.pF[i+2]]);   
	}

	if ( isDurative == TRUE && glnf_metric.pF[i+3] == -2 ) {  
		lprune += gtt * s->stt ;
	} else {
		lprune += (glnf_metric.pC[i+3] * s->f_V[glnf_metric.pF[i+3]]);   
	}
    }
    loop = loop*4;

    for (i=0; i < rest; i++){
	if (isDurative == TRUE &&  glnf_metric.pF[loop+i] == -2 ) {  
		lprune += gtt * s->stt ;
	} else {
		lprune += (glnf_metric.pC[loop+i] * s->f_V[glnf_metric.pF[loop+i]]);   
	}
    }

*/


    for (i=0; i < glnf_metric.num_pF; i++){
	if (isDurative == TRUE &&  glnf_metric.pF[i] == -2 ) {  
		lprune += gtt * s->stt ;
	} else {
		lprune += (glnf_metric.pC[i] * s->f_V[glnf_metric.pF[i]]);   
	}
    }

    return lprune;
}

int compare_states(const void* s1, const void* s2){
    const State *S1 = s1;
    const State *S2 = s2;
    int i;
/*
    if (S1->cost > S2->cost) return 1;
    if (S1->cost < S2->cost) return -1;
*/  
    if (S1->num_F > S2->num_F) return 1;
    if (S1->num_F < S2->num_F) return -1;
    
    int comp_F = memcmp(S1->F, S2->F, S1->num_F * sizeof(int));
    int comp_FD = memcmp(S1->f_D, S2->f_D, gnum_fl_conn * sizeof(Bool));
    int comp_FV;
    /*  = memcmp(S1->f_V, S2->f_V, gnum_fl_conn * sizeof(float)); */
    for ( i = 0; i < gnum_relevant_fluents; i++ ) {
	if (strncmp(grelevant_fluents_name[i],"IS_",3)!=0) {
	    if (S1->f_V[i] > S2->f_V[i]) return 1;
	    if (S1->f_V[i] < S2->f_V[i]) return -1;
	}

    }
    comp_FV = 0;

    if ((comp_F == 0) && 
	(comp_FD == 0) &&
	(comp_FV == 0)){
      if (S1->cost > S2->cost) return 1;
      if (S1->cost < S2->cost) return -1;
      return 0;
    }
    else{

	if ( (comp_F > 0) ||
	     ((comp_F == 0) && (comp_FD > 0)) ||
	     ((comp_F == 0) && (comp_FD == 0) && (comp_FV > 0))){
	
	    return 1;
	}
	else{
	    
	    return -1;
	}
    }
    return 0;
}
int read_in_bucket(Bucket *b){

    b->size = 0;
    b->current = 0;

    if (b->completely_read)
	return 0;

    if (!b->file_opened){
	if (openRead(b->s_file) == 0) 
	    return 0;
	b->file_opened = TRUE;
    }

    int flag = 0;

    while(b->size < b->MAX_SIZE && 
	  (flag = read_state_from_file(b->s_file, &b->states[b->size++]))){
	/*print_F_vector( &b->states[b->size-1]);*/
	/*printf("%u State read; Reading pred. \n", b->inserted + b->size);*/
	read_state_from_file(b->s_file, &b->preds[b->size-1]);
	/*print_F_vector( &b->preds[b->size-1]);
	  printf("Pred read: Reading state \n");*/
	if (gcmd_line.beamsize != UINT_MAX){
	    if((b->inserted + b->size) > gcmd_line.beamsize){
		printf("Reading Prunned!\n");
		flag = 0;
		break;
	    }
	}
    }
    /* All states read */
    if (!flag){
	/* while loop increased the size for even a failed op. */
	b->size -- ;

	/* file has been completely read */
	closeFile(b->s_file); 
	b->file_opened     = FALSE;
	b->completely_read = TRUE;
    }
    /*     if(b->SORT) printf("(%d, %d) R=%d\n", b->g, b->h, b->size);     */
    b->inserted += b->size;
    /*    if (b->SORT) printf("Total states read so far = %d\n", b->inserted); */
    return b->size;
  
}

void initialize_bucket(Bucket *b, int g, int h, Bool SORT){
/*    printf("Initializing the bucket -(%d, %d).\n", g, h);*/
   
    /* UNNECESSARY!!!! */
    /* I assume that num_F = 0 means an invalid state */
    /*for (i = 0 ; i < b->size; i++) 
	b->states->num_F = 0;
    */
    b->g  =  g;  b->h  = h;
    b->size            = 0;
    b->current         = 0;
    b->file_opened     = FALSE;
    b->completely_read = FALSE;
    b->SORT            = SORT;
    b->inserted        = 0;
    b->duplicates      = 0;
    file_init_g_h(b->s_file, g, h, -1);
}

void init_bucket_file_only(Bucket *b){
    b->file_opened     = FALSE;
    b->completely_read = FALSE;
    file_init_g_h(b->s_file, b->g, b->h, -1);
}

void close_bucket(Bucket*b){
   if (b->file_opened) closeFile(b->s_file);
}

State* get_next(Bucket *b){

    if(b->current == b->size)
	read_in_bucket(b);

    if (b->size <= 0)
	return 0;

    /*printf("current = %d, size = %d \n", b->current, b->size);*/
    return &b->states[b->current++];
}

State* get_pred(Bucket *b){
    if (b->current == 0){
	printf("(%d, %d): Warning!! get_pred called without calling get_next", b->g, b->h);
        return &b->preds[b->current];
    }
    return &b->preds[b->current - 1];
}

State* get_previous(Bucket *b){
    if (b->current == 0){
	printf("(%d, %d): Invalid call for get_previous. Zero size bucket! OR Call is for the previous of the first element.\n", b->g, b->h);
	exit(1);
    }
    return &b->states[b->current -1];
}

void allocate_bucket(Bucket *b, int size){
  
  b->states = (State*)calloc(size, sizeof(State) );
  b->preds = (State*)calloc(size, sizeof(State) );
#ifdef VERIFY_FLUSH
  b->backup_states = (State*)calloc(size, sizeof(State) );
  b->backup_preds = (State*)calloc(size, sizeof(State) );
#endif
  b->s_file = (File*)calloc(1, sizeof(File));
  b->MAX_SIZE = size;
  b->s_file->processID = -1;
  
  b->size = 0;
  allocate_states_in_bucket(b);
    
}

void free_bucket(Bucket *b){
    int i;
    for(i=0; i< b->MAX_SIZE; i++){
	free_state(&b->states[i]);
	free_state(&b->preds[i]);
#ifdef VERIFY_FLUSH
	free_state(&b->backup_states[i]);
	free_state(&b->backup_preds[i]);
#endif	
    }
    free(b->states);
    free(b->s_file);
    free(b->preds);
}
