 #include "duplicates_removal.h"
 #define DEBUG_DR 0
/* #define TESTING 0 */
 void init_DuplicatesRemoval(DR *dr){
     /*printf("^^^^^^^^^^Calling Constructor of DuplicatesRemoval\n");*/
     int i;

     for(i=0; i< MAX_IO_BUFFERS; i++){
	 dr->pss[i] = (EPS*)calloc(1, sizeof(EPS));
	 allocate_eps(dr->pss[i]);
	 dr->pss[i]->pred->num_F = 0;
	 dr->bf[i] = (File*)calloc(1, sizeof(File));

	 /*dr->bucketF[i] = (Bucket*) calloc(1, sizeof(Bucket));
	 allocate_bucket(dr->bucketF[i], BUFFER_FILE_SIZE);
	 allocate_states_in_bucket(dr->bucketF[i]);*/
     }
     dr->totalFilePointers = 0;

     dr->bfSB = (File  * ) calloc( 1, sizeof( File ) ); 
     dr->bfOutput= ( File * ) calloc( 1, sizeof( File ) ); 

      dr->bucketOutput = ( Bucket * ) calloc( 1, sizeof( Bucket ) );

     allocate_bucket(dr->bucketOutput, BUFFER_FILE_SIZE);


 }

 void free_DuplicatesRemoval(DR *dr){
     free(dr->bfSB);
     free(dr->bfOutput);
     int i;
     for(i=0; i< MAX_IO_BUFFERS; i++){

	 free_eps(dr->pss[i]);
	 free(dr->pss[i]);
	 free(dr->bf[i]);
	 /*free_bucket(dr->bucketF[i]);
	   free(dr->bucketF[i]);*/
     }
     free_bucket(dr->bucketOutput);
     free(dr->bucketOutput);
 }

 void initializeBufferFiles(DR *dr, int total, int g, int h){
     int i;
     for ( i=0; i< total; i++){
	 file_init_g_h(dr->bf[i], g, h, -1);
     }
 }

 void allocate_eps(EPS *eps){
     eps->state = calloc(1, sizeof(State));
     eps->pred = calloc(1, sizeof(State));
     allocate_state(	eps->state );
     allocate_state(	eps->pred );
 }

 void free_eps(EPS *eps){

     /*printf("Freeing eps\n");*/
     free_state(eps->state);
     free_state(eps->pred);
     free(eps->state);
     free(eps->pred);
 }


 void setInvalid(State *exPS){
     exPS->num_F = USHRT_MAX;
     exPS->cost  = USHRT_MAX;
 }


 int readElementGeneric(DR *dr, int i){
     int result = 0;
     if (0){
	 printf("Reading for Buffer = %d: ", i);
	 printf("\t%d read ", dr->elementsReadFromBuffer[i]); 
	 printf("out of %d;\n", dr->totalElementsInTheBuffer[i]);
     }

     if (dr->elementsReadFromBuffer[i] < dr->totalElementsInTheBuffer[i])
     {

	 /* change */
	 /* we have to use the bucket */
	 /* forget the fuck'in bucket */
	 result += read_eps_from_file(dr->bf[i], dr->pss[i]);
	 /*result += read_state_from_file(dr->bf[i], dr->pss[i]->pred);*/

	 if (dr->pss[i]->state->num_F > USHRT_MAX){
	     printf("Invalid num_F read = %d at pss[%d]\n", dr->pss[i]->state->num_F, i); 
	     exit(1);
	 }
	 if (DEBUG_DR){
	     printf("\nRead in %d-th buffer ; F = ", i);
	     print_F_vector(dr->pss[i]->state);

	     printf("; Bytes read = %d; total bytes read = %d\n", result, dr->bf[i]->bytesRead);
	 }
	 if (result) dr->elementsReadFromBuffer[i]++;
     }else{
	 return 0;
     }
     return result;
 }

 /*
  * returns the min state index number 
  * If we want to use a heap or a balanced tree, we have to stick only to this function and
  * readElement function.
  */
 int delMin(DR *dr, int k){


     int minPos = 0;
     int i;
     /*printf("MinPos with buffers = %d: i = 0; ", k);*/

     for ( i = 1 ; i<k ; i++){
	 if (compare (dr->pss[i]->state, dr->pss[minPos]->state) < 0 ){
	     /*printf("i = %d; ", i);
	       fflush(stdout);*/
	     minPos = i;
	 }
     }
     /*printf("min cost = %f\n", dr->pss[minPos]->state->cost);
       printf("\n");*/
     return minPos;
 }


 /*
  * Given the g,h values of a buffer, SearchBuffers search for all the sorted 
  * buffers in the file and open file pointers to the start of those buffers.
  * The buffers and pointers information is stored in the class variables
  * Returns: The total number of buffers found
  * Assumption: Total buffers found < MAX_IO_BUFFERS     <======== VOID
  */
 Bool SearchBuffers(DR *dr, int g, int h, int *buffers, int procID, int first_call, Bool *file_read){

     /*cout << "Searching buffers in " << g << "-" << h << endl;
     //  printf("Searching buffers in (%d, %d) \n", g, h);*/

     EPS previous;
     allocate_eps(&previous);
     /*setInvalid(previous.state);*/
     previous.state->num_F = -1;
     previous.state->cost = -1;

     EPS current; 
     allocate_eps(&current);
     setInvalid(current.state);

     unsigned int count          = 0;
     unsigned int currentBuffer  = 0;
     unsigned int totalElements  = 0;
    

     char s_err[128];
     Bool openSuccess = FALSE;

     if (!first_call){
	 file_init_g_h(dr->bfSB, g, h, -1);
	 openSuccess = openRead(dr->bfSB);

	 if (!openSuccess) 	
	     return FALSE;
     }
     /* bf->files are already initialized by initializeBufferFiles */
     file_init_g_h(dr->bf[currentBuffer], g, h, -1);
     openRead(dr->bf[currentBuffer]);
     jumpToMiddle(dr->bf[currentBuffer], dr->previousFilePart, 
		  (_file_int)dr->totalBytesReadPrevious);
#ifdef TESTING
     if (1) printf("Starting Buffer-%d from %d-th element lying in %d part starting at the position %d with bytes = %d \n",  
		   currentBuffer, count, dr->currentFilePart, 
		   totalElements, dr->totalBytesReadPrevious);
#endif
     currentBuffer++;

     while((dr->tempTotalBytesRead = read_eps_from_file(dr->bfSB, &current)) != 0){
	 dr->currentFilePart = dr->bfSB->currentPart;

	 if (dr->currentFilePart!=dr->previousFilePart){
	     printf ("New file part started %d -> %d \n", dr->previousFilePart,  dr->currentFilePart);
	     dr->totalBytesReadPrevious = 0;
	     dr->totalBytesReadCurrent = 0;
	 }

	 dr->totalBytesReadCurrent +=  dr->tempTotalBytesRead;
	 if (0) printf("tempTotalBytesRead = %d\n", dr->tempTotalBytesRead);
	 if (0) printf("tempTotalBytesCurrent = %d\n", dr->totalBytesReadCurrent);

	 if (0) printf("DR:Previous read = %u bytes; Read %u bytes; TotalBytesRead so far = %u\n\n", 
			      dr->totalBytesReadPrevious,  dr->tempTotalBytesRead,  dr->totalBytesReadCurrent);
	 totalElements++;
	 count++;

	 int comparison = compare(previous.state, current.state);
	 if (comparison > 0 ){

#ifdef TESTING
	     /* Save the total elements in the last buffer*/
	     if (0) printf("Starting Buffer-%d from %d-th element lying in %d part",
			   currentBuffer, count, dr->currentFilePart);
	     if (0) printf(" starting at the position %d with bytes = %d \n", 
			   totalElements, dr->totalBytesReadPrevious);
	     if (DEBUG_DR) printf("Count at the start of new buffer = %d \n", count);
	     if (DEBUG_DR) printf("Result of comparison was -----  %d \n", comparison);
	     if (DEBUG_DR){
		 printf("previous F = "); print_F_vector(previous.state);
		 printf("current F = ");  print_F_vector(current.state);
		 printf("-=-=-\n\n");
	     }
#endif
	     dr->totalElementsInTheBuffer[currentBuffer - 1] = count-1;
	     if (0) printf("\t\t%d elements read for Buffer-%d\n", count-1, currentBuffer-1);
	     dr->elementsReadFromBuffer[currentBuffer - 1]   = 0;
	     setInvalid(dr->pss[currentBuffer-1]->state);

	     /* Open a new file pointer and take it to the start of the 
		.. new buffer */
	     openSuccess = FALSE;
	     file_init_g_h(dr->bf[currentBuffer], g, h, -1);
	     openSuccess = openRead(dr->bf[currentBuffer]);

	     if (!openSuccess){
		 printf("Cannot allocate more file buffers \n");
		 printf("Total buffers allocated so far = %d. \n", 
			currentBuffer - 1);
		 free_eps(&previous);
		 free_eps(&current);
		 *buffers = currentBuffer;
		 return currentBuffer;
		 perror(s_err);
		 exit(1);
	     }
	     dr->totalFilePointers++;
	     jumpToMiddle(dr->bf[currentBuffer], dr->previousFilePart, 
			  (_file_int)dr->totalBytesReadPrevious);
	     currentBuffer++;
	     /* A new buffer has started */
	     count = 1;
	 }
       
	 if (DEBUG_DR){
	     printf("previous F = "); print_F_vector(previous.state);
	     printf("current F = ");  print_F_vector(current.state);
	     printf("-=-=-\n\n");
	 }
	 copy_eps(&previous, &current);

	 if (currentBuffer >= MAX_IO_BUFFERS){
	     printf("I have already read %d buffers cant read more\n", currentBuffer-1);
	     free_eps(&previous);
	     free_eps(&current);
	     dr->totalBytesReadCurrent -=  dr->tempTotalBytesRead;
	     jumpToMiddle(dr->bfSB, dr->previousFilePart, 
			  (_file_int)dr->totalBytesReadPrevious);
	     /* I have to push back the "current" state */
	     closeFile(dr->bf[currentBuffer-1]);
	     *buffers = currentBuffer-1;
	     return currentBuffer-1;
	 }
	  dr->totalBytesReadPrevious =  dr->totalBytesReadCurrent;
	  dr->previousFilePart =  dr->currentFilePart;
	  dr->tempTotalBytesRead = 0;
	  if (DEBUG_DR) printf("In the end: tempTotalBytesCurrent = %d\n", dr->totalBytesReadCurrent);
     }
     /* Save the information about the last buffer */
     dr->totalElementsInTheBuffer[currentBuffer - 1] = count;
     dr->elementsReadFromBuffer[currentBuffer - 1] = 0;
     setInvalid(dr->pss[currentBuffer-1]->state);
#ifdef TESTING
     if (1) printf("Buffer %d ended at %d \n", currentBuffer-1, (int)totalElements); 
     if (1)  printf("\t\t%d elements read for Buffer-%d\n", count, currentBuffer-1);
     if (DEBUG_DR) printf("SearchBuffers: Total buffers found = %d \n", currentBuffer); 
#endif
     closeFile(dr->bfSB);
     free_eps(&previous);
     free_eps(&current);

     /* The whole file is read */
     *buffers = currentBuffer;
     *file_read = TRUE;
     return TRUE;
 }

 int testAllBuffers(DR *dr, int totalBuffers){
     int i;
     EPS prev;
     allocate_eps(&prev);

     for (i=0; i< totalBuffers; i++){
	 prev.state->num_F = 0;
	 while(dr->elementsReadFromBuffer[i]!=dr->totalElementsInTheBuffer[i]){
	     readElementGeneric(dr, i);
	     if (compare(dr->pss[i]->state, prev.state) < 0){

		 printf("************ ERROR in %d buffer \n", i);
	     }
	     copy_eps(&prev, dr->pss[i]);

	 }
     }
     exit(1);
     return 1;
 }
 /* 
  * This function merge flushed buffers of random sizes.
  */
 int mergeAndRemoveDuplicates(DR *dr, int g, int h, 

			      int totalProcesses,
			      int *duplicatesWithin){

#ifdef TESTING
     printf("\nSearching for the buffers for %d-%d on a single user machine\n",g, h);
#endif
     dr->totalBytesReadPrevious = 0;
     dr->totalBytesReadCurrent  = 0;
     dr->tempTotalBytesRead     = 0;
     dr->previousFilePart       = 0;
     dr->currentFilePart        = 0;

     int totalBuffers =  0;
     remove_file(-1,-1);
     initialize_bucket(dr->bucketOutput, -1, -1, FALSE);
     
     int i;
     int totalElementsWritten = 0;  
     int first_call = 0;
     /**duplicatesWithin = 0;*/
     Bool file_read = FALSE;
     /*printf("File Pointers Before Search Buffers .. \n");
       count_file_buffers();*/
     int duplicates_in_single_merge = 0;
     while(SearchBuffers(dr, g, h, &totalBuffers, 1, first_call, &file_read)){
#ifdef TESTING 
       printf("Merging %d time .. \n", first_call);
#endif
	 first_call++;

#ifdef TESTING 
	 if (totalBuffers > 0) 
	     printf("\nMerge: Total buffers found = %d\n", totalBuffers);
#endif
	 /* There was just one buffer flushed to the file
	  */
	 if (totalBuffers == 1 && totalElementsWritten == 0)
	 {
	     closeFile(dr->bf[0]);
#ifdef TESTING
	     printf("Total elements in that single buffer = %d \n", 
		    (int)dr->totalElementsInTheBuffer[0]);
#endif
	     return(dr->totalElementsInTheBuffer[0]);
	 }

	 /* This is the last buffer that escaped earlier mergings.
	  * This buffer has to be copied to the output file.
	  */
	 if (totalBuffers == 1 && totalElementsWritten > 0){
	     printf("Special Case! One buffer escaped merging \n");
	     printf("Total elements in that single buffer = %d \n", 
		    (int)dr->totalElementsInTheBuffer[0]);
	     while(readElementGeneric(dr,0) != 0){
		 insert_in_bucket(dr->bucketOutput, dr->pss[0]->state, 
				  dr->pss[0]->pred, dr->pss[0]->state->op);
	     }
	     flush_bucket(dr->bucketOutput);
	     /*if (!dr->bucketOutput->completely_read)
	       closeFile(dr->bucketOutput->s_file);*/

	 }else{
	     duplicates_in_single_merge = MergeBuffers(dr, totalBuffers, 
					      &totalElementsWritten);
#ifdef TESTING 
	     printf("(%d, %d): Elements Written = %d; Duplicates = %d\n", 
		    g, h, totalElementsWritten, duplicates_in_single_merge);
#endif
	     *duplicatesWithin += duplicates_in_single_merge;
	 }

	 if (!*duplicatesWithin) printf("No duplicates found!\n");
	 for (i=0; i< totalBuffers; i++)  closeFile(dr->bf[i]);
	 /*printf("File Pointers After Merge Buffers .. \n");
	   count_file_buffers();*/
	 totalBuffers = 0;

	 if (file_read)	     break;
     }
     
     copy_file(-1,-1, g, h);
     
     /*
      * This is the final merge of merged buffers
      */
     if (first_call > 1){
	 initialize_bucket(dr->bucketOutput, -1, -1, FALSE);
	 printf("Doing final merge ... \n");
	 first_call = 0;
	 dr->totalBytesReadPrevious = 0;
	 dr->totalBytesReadCurrent  = 0;
	 dr->tempTotalBytesRead     = 0;
	 dr->previousFilePart       = 0;
	 dr->currentFilePart        = 0;

	 totalBuffers = 0;
	 SearchBuffers(dr, g, h, &totalBuffers, 1, first_call, &file_read);
	 printf("Total buffers found = %d \n", totalBuffers);

	 if (totalBuffers == 1)
	 {
	     closeFile(dr->bf[0]);
	     printf("Total elements in that single buffer = %d \n", 
		    (int)dr->totalElementsInTheBuffer[0]);
	 }else{
	     duplicates_in_single_merge = MergeBuffers(dr, totalBuffers, 
					      &totalElementsWritten);

	     for (i=0; i< totalBuffers; i++) closeFile(dr->bf[i]);
	     printf("(%d, %d): Elements Written = %d; Duplicates = %d\n", 
		    g, h, totalElementsWritten, duplicates_in_single_merge);
	     *duplicatesWithin += duplicates_in_single_merge;

	     printf("Total Duplicates found in External Merge %d\n", *duplicatesWithin);
	     copy_file(-1,-1, g, h);

	 }
	 
     }else{
#ifdef TESTING 
	 printf("No need for a final merge! \n");
#endif
     }


     return totalElementsWritten;
 }

 unsigned int MergeBuffers(DR *dr, int totalBuffers, 
			   int *totalElementsWritten){

     /* initalize previous */
     EPS previous;
     allocate_eps(&previous);
     setInvalid(previous.state);

     *totalElementsWritten = 0;    
     previous.state->num_F = -1;
     previous.state->cost = -1;

     
     /* Read the initial states of all the buffers  */
     int i;
     for (i=0; i<totalBuffers; i++)
     {
	 readElementGeneric(dr, i);
     }

     /* this variable is used for termination of the main loop
	it keeps track of the buffer that are not finished yet. */
     int totalActiveBuffers   = totalBuffers;

#ifdef TESTING 
     printf("Merging the sorted buffers ... \n");
     fflush(stdout);
#endif
     int minState = -1;
     int readSuccess =  -1;
     unsigned int duplicatesWithin = 0;
     int count = 0;
     EPS eps;
     while(totalActiveBuffers > 0){

	 minState = delMin(dr, totalBuffers);        
	 
	 count ++;
	 /*printf("F=%d,op=%d ; prevous:F=%d;op=%d\n", dr->pss[minState]->state->num_F, 
	   dr->pss[minState]->state->op, previous.state->num_F, previous.state->op);*/
	 fflush(stdout);
	 int comparison = compare(dr->pss[minState]->state, previous.state);
	 if (comparison > 0 )
	 {   /* Write the minimum state only if it's unique */
	     eps.state = dr->pss[minState]->state;
	     eps.pred  = dr->pss[minState]->pred;
	     insert_in_bucket(dr->bucketOutput, eps.state, eps.pred, eps.state->op);
	     /*write_eps_to_file(dr->bfOutput, &eps);*/
	     /*printf("result of compare = %d\n", comparison);	    */
	     (*totalElementsWritten) ++;
	     
#ifdef TESTING
	     printf("Total elements written = %d\n", *totalElementsWritten);
#endif
	     /* BOTTLENECK*/
	     copy_eps(&previous, dr->pss[minState]);
	 }
	 else if(comparison < 0){
	     printf("!!! ERROR the previous state was greater\n");
	     printf("\nPrevious State F = ");
	     print_F_vector(previous.state);
	     printf("\nNew Min State F = ");
	     print_F_vector(dr->pss[minState]->state);
	     printf("\nIts Pred: F = ");
	     print_F_vector(dr->pss[minState]->pred);
	     printf("\n********************************************\n");
	     printf("Other states in this buffer .... ");

	     int i, limit = dr->totalElementsInTheBuffer[minState] - 
				 dr->elementsReadFromBuffer[minState]; 
	     printf("Remaining Elements in this buffer = %d \n", limit);
	     exit(1);
	     for(i = 0; i< limit; i++) {
		 /*printf("\ni = %d; F = ", i);*/
		 readElementGeneric(dr, minState);
 /*		print_F_vector(dr->pss[minState]->state);*/
		 printf("Pred: F = ");
		 print_F_vector(dr->pss[minState]->pred);
		 printf("\n");
	     }
	     printf("\n");
	     exit(1);
	 }
	 else{   /* We have a duplicate node. */
 /*	    if (same_state(dr->pss[minState]->state, previous.state))
		 printf("The really are equal!!");
	     else{
		 printf("!!! ERROR:\n");
		 }*/
#ifdef TESTING
	     printf("Count = %d: Duplicates Found for minState = %d\n", count, minState);
#endif
	     duplicatesWithin++;
	 }
	 totalActiveBuffers -- ;

	 readSuccess = readElementGeneric(dr, minState);

	 /*printf("-=-=-=-=-=-=-=-=-=-=-=-=-\n");*/
	 if (readSuccess >= 1)
	 {   /* If an element is read */
	     totalActiveBuffers++;
	 }
	 else
	 {   /* Mark that position as invalid */
#ifdef TESTING
	     printf("Setting invalid for buffer = %d\n", minState);
#endif
	     setInvalid(dr->pss[minState]->state);
	 }
     } /* end of main while loop */
     free_eps(&previous);
     flush_bucket(dr->bucketOutput);
     
#ifdef TESTING 
     if (1) printf("In Merge Buffer: Total read = %d; Elements Written = %d; Duplicates = %d\n", count, *totalElementsWritten, duplicatesWithin);
#endif
     return  duplicatesWithin;
 }



 int compare( State *exps1, State *exps2){

     /* We should not forward such a case to equalold or greaterold functions.
	because then they will call PackedState::equal and it will use
	65535 bytes for comparison :(. This was found out by val-grind.*/
/*     if (exps1->num_F == USHRT_MAX && exps1->num_F == USHRT_MAX)
	 return 0;
*/
     return compare_states(exps1, exps2);

 }

 void copy_eps(EPS *dest, EPS *src){
     source_to_dest(dest->state, src->state);
     source_to_dest(dest->pred, src->pred);
 }

 /*
  * Pre-condition: src is opened
  * Post-condition: file (M-dest_g-dest-h) is created and src
  * is copied into it. The dest file is closed but src is not!
  */

 void copy_file(int src_g, int src_h, int dest_g, int dest_h){
     if (0) printf("Copying to file M-%d-%d \n", dest_g, dest_h);
     File src, dest;
     int written = 0;
     
     remove_file(dest_g, dest_h);
     rename_file(src_g, src_h, dest_g, dest_h);
     return;

     file_init_g_h(&dest, dest_g, dest_h, -1);
     file_init_g_h(&src, src_g, src_h, -1);

     openRead(&src);
     openWrite(&dest);
     EPS eps;
     allocate_eps(&eps);

     while(read_eps_from_file(&src, &eps)){
	 /* Write */
	 write_eps_to_file(&dest, &eps);
	 written ++;
     }
    if (0) printf("\t\tCopied %d elements from M-%d-%d to M-%d-%d\n", written, src_g, src_h, dest_g, dest_h);
     closeFile(&dest);
     closeFile(&src);
     remove_file(src_g, src_h);
     free_eps(&eps);
 }



 int subtractFilesOfAboveLayers(DR *dr, int g, int h, int locality, int *duplicates, int hval_initial_state, int max_depth){


   int elements_written = 0;
   /**duplicates = 0;*/
   int total_duplicates = 0;
   int individual_layer_duplicates = 0;
   Bool openSuccess = FALSE;
#ifdef TESTING
   printf("\t********************* SUBTRACTING *********************\n");
   printf("Subtracting locality = %d files from the Bucket (%d, %d) .. \n", locality, g, h);
#endif
   int loc;

   for (loc = 1; loc <= locality; loc++){

       file_init_g_h(dr->bfSB, g, h, -1);
       openSuccess = openRead(dr->bfSB);

       if (!openSuccess){
	   printf("No such file M-%d-%d!!!", g, h);
	   return 0;
       }

       file_init_g_h(dr->bfOutput,-1,-1, -1);
       openSuccess = openWrite(dr->bfOutput);

       file_init_g_h(dr->bf[0], g-loc, h, -1);
       openSuccess = openRead(dr->bf[0]);
       
       if (!openSuccess){
	 /*printf("No such file M-%d-%d!!!\n", max_depth - loc, h);*/
	 closeFile(dr->bfOutput);
	 closeFile(dr->bfSB);
	 if (g-loc < 0)
	   break;
	 else
	   continue;
       }
#ifdef TESTING
       printf("\t Subtracting (%d, %d) from (%d, %d)\n", g-loc, h, g, h);
#endif
       elements_written = subtract(dr->bf[0], dr->bfSB, dr->bfOutput, &individual_layer_duplicates);
#ifdef TESTING
       printf("\t Subtracted (%d, %d) from (%d, %d): Elements Remaining = %d; Duplicates = %d\n", g-loc, h, g, h, elements_written, individual_layer_duplicates);
#endif
       total_duplicates += individual_layer_duplicates;
       individual_layer_duplicates = 0;

       closeFile(dr->bfSB);
       closeFile(dr->bfOutput);
       closeFile(dr->bf[0]);
       copy_file(-1,-1, g, h);
   }
#ifdef TESTING
   printf("\n-=-=-=-=-=-=-=-=\n");
   printf("Subtraction Summary (%d, %d): Remaining = %d; Duplicates in Previous %d layers = %d\n", 
	  g, h, elements_written, locality, total_duplicates);
   printf("-=-=-=-=-=-=-=-=\n");
#endif
   fflush(stdout);
 /*  printf("\t******************** SUBTRACTION DONE ********************\n");*/
   *duplicates = total_duplicates; /* return value */
   return elements_written;

 }



 int subtract(File* top, File* main_file, File* temp, int *duplicates){  

   EPS *s1 = (EPS*)calloc(1, sizeof(EPS));
   EPS *s2 = (EPS*)calloc(1, sizeof(EPS));
   allocate_eps(s1);
   allocate_eps(s2);
   int elementsInOriginalFile = 0;
   int elements_written = 0;

   /* if files are empty, exit the subtraction routine */
   if (!read_eps_from_file(main_file, s2)){
       free_eps(s1);
       free_eps(s2);
       free(s1);
       free(s2);
       return 0;
   }

   if (!read_eps_from_file(top, s1)){
       free_eps(s1);
       free_eps(s2);
       free(s1);
       free(s2);
       return 0;
   }

   elementsInOriginalFile ++;

   while (1) {
       if (compare(s2->state, s1->state) > 0) {
	   /*printf("- greater advancing pointer in file1\n");*/
	   if (!read_eps_from_file(top, s1)){

	       /* Write the last state before exiting.*/
	       write_eps_to_file(temp, s2);
	       elements_written++;
	       /* Flush the rest of the f2 to temp*/
	       while(read_eps_from_file(main_file, s2)){
		   elementsInOriginalFile++;
		   write_eps_to_file(temp, s2);
		   elements_written++;
	       }
	       break;
	   }
	   continue;
       }
       if (compare(s1->state, s2->state) == 0 ) {
	   (*duplicates)++;
	   /*printf("- equal advancing pointer in file2\n");*/
	   elementsInOriginalFile++;
	   if (!read_eps_from_file(main_file, s2)) break;
	   continue;
       }

       if (compare(s1->state, s2->state) > 0 ) {
	   /*printf("- smaller advancing pointer in file2\n");*/
	   /*if (DEBUG) printf("- writing temporary\n");*/
	   elementsInOriginalFile++;
	   write_eps_to_file(temp, s2);
	   elements_written++;
	   if (!read_eps_from_file(main_file, s2)) break;
	   continue;
       }
   }


   /*printf( "\tTotal elements in the original file were = %d; Duplicates = %d\n", elementsInOriginalFile, *duplicates);*/

   free_eps(s1);
   free_eps(s2);
   free(s1);
   free(s2);
   return elements_written;
 }
