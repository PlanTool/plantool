#include "file.h"
#define         DEBUG_FILE    0
#define DEBUG_JUMP 0
#define MAX_FILE_SIZE 2147400000 /* 2 GB - 1024 bytes */
void file_init(File *file, int nprocessID){
    file->g = -1;    file->h = -1;
    file->processID    = nprocessID;
    file->baseSize     = 0;
    file->currentPart  = 0;
    file->bytesWritten = 0;
    file->bytesRead    = 0;
    file->discarded    = 0;
}

void file_init_g_h(File *file, int ng, int nh, int nprocessID){
    file->g = ng;    file->h = nh;
    file->processID    = nprocessID;
    
    file->currentPart  = 0;
    file->bytesWritten = 0;
    file->bytesRead    = 0;
    file->discarded    = 0;
}

/*
void file_init_temp(File *file, int nprocessID){
   file->g = -1;    file->h = -1;
    file->processID    = nprocessID;
    
    file->currentPart  = 0;
    file->bytesWritten = 0;
    file->bytesRead    = 0;
    file->discarded    = 0;
}
*/
Bool isOpened(File* file){
    return file->opened;
}

void rewindFile(File*file){
    assert(file->filep);
    /*rewind(filep);*/
}

/*Bool openTempRead(File *file){
    strcpy(file->mode, READ_MODE);
    if (file->processID == -1){
	sprintf(file->filename,"%s/%s", PATH, TEMP_FILE_PREFIX);		
	if (DEBUG_FILE) printf("Opening Temp file = %s for Reading\n", file->filename);
    }else{	
	
	sprintf(file->filename,"%s/%s.%d", PATH, TEMP_FILE_PREFIX, file->processID);
	if (DEBUG_FILE) 	printf("Opening Temp file = %s for Reading\n", file->filename);
    }
    return  (file->filep = fopen(file->filename, READ_MODE))!= '\0' ? TRUE : FALSE;
}
*/
/*
Bool openTempWrite(File *file){
    strcpy(file->mode, WRITE_MODE);    
    if (file->processID == -1){
	sprintf(file->filename,"%s/%s", PATH, TEMP_FILE_PREFIX);	
    }else{	
	sprintf(file->filename,"%s/%s.%d", PATH, TEMP_FILE_PREFIX, file->processID);

    }
    return  (file->filep = fopen(file->filename, WRITE_MODE))!= '\0' ? TRUE : FALSE;
    
}
*/
Bool openFile(File *file,const char* nmode){
    if (DEBUG_FILE) printf("\tOpenFile: (%d,%d) in mode = %s\n", file->g, file->h, nmode);
    file->filep = 0;
    strcpy(file->mode, nmode);
    
    if (file->processID == -1)
	sprintf(file->filename,"%s/M-%d-%d",PATH, file->g, file->h);
    else
	sprintf(file->filename,"%s/%d-%d-%d", PATH, file->processID, file->g, file->h);

    file->filep = fopen(file->filename, file->mode);
    /* Has the file opened correctly ?*/
    if (!file->filep)  {
	if(DEBUG_FILE) printf("Opening of file %s failed ... \n", file->filename);
	return FALSE;
    }
    if (DEBUG_FILE) printf("\tFile opened .. \n");
    return TRUE;
}

Bool copyTo(File *file,int copy_g, int copy_h, int copy_processID){
    printf("file::copyTo() Unsafe function!!!!!!!!!!");
    exit(1);
    char command[80];
    
    /* I have to iterate on all the parts.*/
    if (copy_processID == -1){
	sprintf(command, "mv %s %s/M-%d-%d",file->filename, PATH, copy_g, copy_h);
    }else{
	sprintf(command, "mv %s %s/%d-%d-%d",file->filename, PATH, copy_processID, copy_g, copy_h);
    }
    if(DEBUG_FILE) printf("Executing Command: %s\n", command);
    system(command);
    return FALSE;
}

Bool openAppend(File *file){
    if (DEBUG_FILE)     printf("Opening file %d,%d for appending .. \n", file->g, file->h);
    /* Assumption: currentPart = 0*/
    /* Now I have to iterate till the end of the file.*/
    if(DEBUG_FILE) printf("\tTrying to open the first part in reading mode.. \n");
    Bool result = openFile(file, READ_MODE);
    closeFile(file);
    if (result) {
	if (DEBUG_FILE) printf("\tFirst part succedded\n");
	while (result){
	    file->currentPart++;
	    result = openNextPart(file);
	    closeFile(file);
	}
	strcpy(file->mode, APPEND_MODE);
	/* currentPart points to the last index that could not be opened.*/
	file->currentPart--;
	if (file->currentPart == 0)
	    result = openFile(file, APPEND_MODE);
	else	    
	    result = openNextPart(file);
    }else{
	file->currentPart = 0;
	result = openFile(file, APPEND_MODE);
	if (result) 
	    if(DEBUG_FILE) printf("File opened!");
    }
  	
    /* this result is offcourse true;
       Now I have to set the bytesWritten to the size of the file.*/
    file->bytesWritten = fileSize(file);
    return result;
}

Bool openWrite(File *file){
    file->currentPart = 0;
    return openFile(file, WRITE_MODE);
}

Bool openOverWrite(File *file){
    printf("^^^^WARNING: Opening file in Over Writing Mode! Be Careful!\n");
    file->currentPart = 0;
    return openFile(file, OVER_WRITING_MODE);
}

Bool openRead(File *file){
    file->currentPart = 0;
    return openFile(file, "rb");
} 

_file_int fileSize(File *file){

    struct stat	sbuf;

    if(DEBUG_FILE)printf("\tFinding the file size of %s... \n",file->filename);
    if(stat(file->filename,&sbuf) == -1){
	perror("^^^fileSize: stat: could not find file size.");
	exit(1);
    }

    if(DEBUG_FILE)printf("\t\tFile size found = %d.\n", (unsigned int)sbuf.st_size);
    return (_file_int) sbuf.st_size;
}

void closeFile(File *file){

    if (file->filep) fclose(file->filep);
    if (DEBUG_FILE) printf("\tFile M-%d-%d--%d CLOSED!\n", file->g, file->h, file->currentPart);
    file->filep = (FILE*)'\0';
   
}

Bool openNextPart(File *file){

    
    if (file->processID == -1){
	if (file->currentPart == 0){
	    sprintf(file->filename,"%s/M-%d-%d",PATH, file->g,file->h);
	    
	}
	else{
    	    sprintf(file->filename,"%s/M-%d-%d--%d",PATH, 
		    file->g,file->h, file->currentPart);
	}
    }
    else{
	sprintf(file->filename,"%s/%d-%d-%d--%d",PATH, file->processID,file->g,file->h, file->currentPart);
    }
    if (DEBUG_FILE) printf("\tfilename = %s\n", file->filename);
    file->filep = fopen(file->filename, file->mode);
    
    if (file->filep){
	if (DEBUG_FILE) printf("\t%d part of Bucket(%d, %d) opened\n", file->currentPart, file->g, file->h);
    }else{
	if (DEBUG_FILE) printf("\t%d part of Bucket(%d, %d) FAILED. Filename = %s, Mode = %s!\n", file->currentPart, file->g, file->h, file->filename, file->mode);
    }
    if (!file->filep) return FALSE;
    return TRUE;
}
/**
 * ---------------- NOT SUPPORTED ------------------------
 * This function will be used by Distributed HSF-SPIN to jump to the middle
 * of a file and search for the start of the next state.
 * Pre-Condition: File is already opened.
 * @return middleByte: the start of the next state. 
 * This information should be proivded to the bucket.startByte
 */
Bool jumpToMiddle(File *file, int toPart, _file_int middleByte){
    if (DEBUG_JUMP) printf("********* JUMPING TO MIDDLE ******* \n");
    file->currentPart = toPart;
    closeFile(file);
    if (DEBUG_JUMP) printf("Before: MiddleByte = %u of part %d\n", middleByte, toPart);
    
    if(!openNextPart(file)){
	if (DEBUG_JUMP) printf("\tSorry cannot open the requested part %d ..DEBUG ME\n", toPart);
    }else
	if (DEBUG_JUMP) printf("\tPart %d opened! %s opened in %s mode\n", toPart, file->filename, file->mode);
    
    if (!fseek(file->filep, (long)(middleByte), SEEK_SET)){
	if (DEBUG_JUMP) printf("Jump Succeded!\n");
    }else
	if (DEBUG_JUMP) printf("^^^^^ ERROR: Jump not succeeded \n");
    file->bytesRead = middleByte;
    return TRUE;
/*
    // Jumping to the middle
    middleByte = middleByte - strlen(sentinal) ;
    
    if(DEBUG_FILE)printf("Before: MiddleByte = %u\n", middleByte);
    // Search for the next state boundary
    do{  
	fread((void*)garbage, sizeof(char) , strlen(sentinal), filep);
	fseek(filep, (long)++middleByte, SEEK_SET);
	if(DEBUG_FILE)printf("After: MiddleByte = %u, garbage read = %s\n", middleByte, garbage);
    }while(strncmp(garbage, sentinal, strlen(sentinal)) != 0);
  
    // middlebyte points to the sizeof(sentinal) -1 position
    middleByte = middleByte + strlen(sentinal) - 1;
    if(DEBUG_FILE)printf("After: MiddleByte = %u\n", middleByte);
    fseek(filep, (long)middleByte, SEEK_SET);
    return filep;
**/
}

/**
 * @returns number of bytes written for that particular state
 */

int  read_eps_from_file(File *f, EPS *eps){
    
    int bytes = read_state_from_file(f, eps->state);
    int statePart = f->currentPart;
    if ((bytes>0) || (bytes==-1))
	bytes+=read_state_from_file(f, eps->pred);

    if (statePart!=f->currentPart){
	printf("^^^^^ ERROR: state was in %d part while its pred is in %d part\n", statePart, f->currentPart);
	printf("State->num_F = %d; pred->num_F = %d", eps->state->num_F, eps->state->num_F);
	exit(1);
    }
    /*  printf("Part %d: State->num_F = %d; pred->num_F = %d", f->currentPart, eps->state->num_F, eps->state->num_F);
	printf("Total bytes read for this state = %d\n", bytes);*/
    return bytes;
}

int  write_eps_to_file(File *file, EPS *eps){

    if ((eps->state->num_F > gnum_ft_conn) || (eps->pred->num_F > gnum_ft_conn) || (eps->state->num_F <= 0) || (eps->pred->num_F <= 0)){
	printf("Invalid state size while writing: %d; Pred size = %d; gnum_ft_conn = %d\n",eps->state->num_F, eps->pred->num_F, gnum_ft_conn);
	printf("While writing to the file %s with %d bytes already written \n", file->filename, file->bytesWritten);
	return 0;
    }
    
    int state_size = (eps->state->num_F + eps->pred->num_F+2) * sizeof(int) + 
	2 * (sizeof(Bool) * gnum_fl_conn + sizeof(float) * gnum_fl_conn + sizeof(int) + sizeof(int) + sizeof(float));
    
    /* because of solution reconstruction */

    
    /* Perform the checks for the file limit*/
    if (state_size >= (MAX_FILE_SIZE -  file->bytesWritten))
    {
	if(1) printf("Can't write more on the file .. Already %d bytes written.\n",  file->bytesWritten);
	if(1) printf("State size = %d \n", state_size);
	closeFile( file);
	/* open a new file*/
	file->currentPart++;
	if (!openNextPart(file))
	    return 0;
	 file->bytesWritten = 0;
	printf("Next part opened for writing \n");
	fflush(stdout);
    }


    unsigned int bytes = write_state_to_file(file, eps->state);
    if (bytes)
	bytes += write_state_to_file(file, eps->pred);
    return bytes;
}

unsigned int write_state_to_file (File *file, State *state) { 

    unsigned int lbytesWritten = 0;
    if (!file->filep){
	printf("^^Invalid file pointer for writing in %s currentPart = %d with bytesWritten till now = %d\n", 
	       file->filename, file->currentPart, file->bytesWritten);
    } 
	
    if (state->num_F > gnum_ft_conn){
	printf("Invalid state size while writing: %d; gnum_ft_conn = %d\n",state->num_F,gnum_ft_conn);
	printf("While writing to the file %s with %d bytes already written \n", file->filename, file->bytesWritten);
    }
    lbytesWritten += 
      fwrite((const void*)&state->num_F, sizeof(int), 1, file->filep) * sizeof(int); 
    /*printf("num_F: Written = %d\n", lbytesWritten);*/

  lbytesWritten += 
      fwrite((const void*)state->F, sizeof(int), state->num_F, file->filep) * sizeof(int); 
  /*printf("F: Written = %d\n", lbytesWritten);*/
    lbytesWritten += 
	fwrite((const void*)state->f_D, sizeof(Bool), gnum_fl_conn, file->filep)* sizeof(Bool); 
    /* printf("F_D: Written = %d\n", lbytesWritten);*/

    lbytesWritten += 
	fwrite((const void*)state->f_V, sizeof(float), gnum_fl_conn, file->filep) * sizeof(float) ; 
    /*printf("F_V: Written = %d\n", lbytesWritten);    */
    lbytesWritten += 
	fwrite((const void*)&state->op, sizeof(int), 1, file->filep) * sizeof(int);     
    lbytesWritten += 
	fwrite((const void*)&state->cost, sizeof(float), 1, file->filep) * sizeof(float);     

    lbytesWritten += 
	fwrite((const void*)&state->stt, sizeof(float), 1, file->filep) * sizeof(float);     


    /*printf("num_F: Written = %d\n", lbytesWritten);*/
     file->bytesWritten += lbytesWritten;
    
    return lbytesWritten;
}

void file_backup(File *file){
    file->backup_filep = file->filep;
    strcpy(file->backup_filename, file->filename);
}

void file_recover(File *file){
    file->filep =  file->backup_filep;
    strcpy(file->filename,  file->backup_filename);
}

unsigned int read_state_from_file(File *file, State* state) { 

    /*if (DEBUG_FILE) printf("Start of Reading::Bytes Read so far = %d \n", file->bytesRead);*/

    Bool FLAG = FALSE;

    if (!file->filep)
	return 0;

    /* Read the state size */
    unsigned int lbytesRead = 0;
   
    lbytesRead += fread((void*)&state->num_F, sizeof(int), 1, file->filep) * sizeof(int);    
    /*printf("State->num_F = %d \n", state->num_F);*/
    /* If file has ended.*/
    if (lbytesRead <= 0){
        file_backup(file);

	/* open a new file*/
	file->currentPart++;
	if(DEBUG_FILE)	printf("\tTrying to open the next part = %d\n",file->currentPart);
	if (!openNextPart(file)){
	    if(DEBUG_FILE) printf("\tPart %d of g=%d failed!!!!!!\n", file->currentPart, file->g);
	    /* We want filep to contain something. else we will get a Segm. Fault in the next reading.*/
	    file_recover(file);
	    return 0;	
	}else{
	    if(1) printf("\t File %s opened for reading!\n", file->filename);
	    fflush(stdout);
	    /* Since we have a new file, we
	       close the previous filep */
	    file->bytesRead = 0;
	    fclose(file->backup_filep);
	}
	lbytesRead += fread((void*)&state->num_F, sizeof(int), 1, file->filep) * sizeof(int);    
    }

    if (state->num_F > gnum_ft_conn || state->num_F == 0){
	printf("\t\tInvalid state size read: %d; gnum_ft_conn = %d\n",state->num_F,gnum_ft_conn);
	printf("\t\tWhile reading file %s with %d bytes already read \n", file->filename, file->bytesRead);
	printf("\t\tBytes read for that number are %d\n", lbytesRead);
	FLAG = TRUE;
	state->num_F = 35;
     
    }
    lbytesRead += fread((void*)state->F, sizeof(int), state->num_F, file->filep) * sizeof(int); 
    
    lbytesRead += fread((void*)state->f_D, sizeof(Bool), gnum_fl_conn, file->filep) *sizeof(Bool); 
    /*  printf("Read %d bytes .. \n", bytesRead);*/
    lbytesRead += fread((void*)state->f_V, sizeof(float), gnum_fl_conn, file->filep)* sizeof(float); 
    /*  printf("Read %d bytes .. \n", bytesRead);*/
    lbytesRead += fread((void*)&state->op, sizeof(int), 1, file->filep) * sizeof(int);

    lbytesRead += fread((void*)&state->cost, sizeof(float), 1, file->filep) * sizeof(float);

    lbytesRead += fread((void*)&state->stt, sizeof(float), 1, file->filep) * sizeof(float);

/*        printf("************ JUST READ ***********\n");*/
	    /*  print_State(*state);*/


    file->bytesRead += lbytesRead;
    /*if (0)printf("\t %s - Bytes read for this state =%d; Bytes Read so far = %u \n", file->filename, lbytesRead, file->bytesRead);*/
    

    if (FLAG){
	printf("Printing F vector of the error state\n");
	print_F_vector(state);
	if (1) printf("\t %s - Bytes read for this state =%d; Bytes Read so far = %u \n", file->filename, lbytesRead, file->bytesRead);
	if (FLAG){
	    printf("Reading recursively for the next state\n");
	    read_state_from_file(file, state);
	}
	fflush(stdout);
    }
    return lbytesRead;
}

int count_file_buffers(){

    FILE *f[1024];
    int opened = 0;
    int i;
    for ( i=0; i<1020; i++){
	f[i] = 0;
	if ( (f[i] = fopen("/home/mips/MIPS-XXLv3/ff", "rb")) != 0)
	     opened++;
    }
    
    for (i=0; i< opened; i++)
	fclose(f[i]);

    printf("******************************************************\n");
    printf("Total file pointers that can be opened = %d\n", opened);
    printf("******************************************************\n");
    return opened;
}

/*
 * Removes all parts of a file.
 * Equivalent to rm M-g-h*.
 * Executes a remove command of stdio.h on all parts of the files
 */
void remove_file  (int g, int h){
    File *f = (File*)calloc(1, sizeof(File));
    
    file_init_g_h(f, g, h, -1);
    if (openRead(f)){
	closeFile(f);
	if (DEBUG_FILE) {
	    printf("\tRemoving file %s .. ", f->filename);
	    fflush(stdout);
	}
	remove(f->filename);
	if (DEBUG_FILE) printf("done.\n");
    }else{
	goto del_struc;
    }

    f->currentPart++;
    while(openNextPart(f)){
	closeFile(f);
	remove(f->filename);
	f->currentPart++;
    }
 del_struc:
    free(f);
    return;
}

void rename_file(src_g, src_h, dest_g, dest_h){
    File *src = (File*)calloc(1, sizeof(File));
    File *dest = (File*)calloc(1, sizeof(File));
    file_init_g_h(src, src_g, src_h, -1);
    file_init_g_h(dest, dest_g, dest_h, -1);
    Bool opened = FALSE;
    if (openRead(src)){
	closeFile(src);

	/* In this way we will have the name in the filename */
        opened = openRead(dest);
	if (opened) closeFile(dest);
	if (0) {
	    /* This will not print the correct dest->filename */
	    printf("\tRenaming file %s to %s.. ", src->filename, dest->filename);
	      fflush(stdout);
	}

	rename(src->filename, dest->filename);
	/*if (0) printf("done.\n");*/
    }else{
	goto del_struc;
    }

    src->currentPart++;
    dest->currentPart++;
    while(openNextPart(src)){
	closeFile(src);
	opened = openNextPart(dest);
	if (opened) closeFile(dest);
	/*printf("\tRenaming file %s to %s.. ", src->filename, dest->filename);
	  fflush(stdout);*/
	rename(src->filename, dest->filename);
/*	if (1) printf("done.\n");*/
	src->currentPart++;
	dest->currentPart++;
    }
 del_struc:
    free(src);
    free(dest);
    return;
}

void print_F_vector(State *S){
    int j;
    printf("num_F = %d: F = ", S->num_F);
    for(j = 0; j<S->num_F; j++)
	printf("%d ", S->F[j]);
    printf("\n");
    printf("B Vector: ");
    for(j = 0; j<gnum_fl_conn; j++)
	printf("%d ", S->f_D[j]);
    printf("\n");
    printf("V Vector: ");
for(j = 0; j<gnum_fl_conn; j++)
	printf("%f ", S->f_V[j]);
    printf("\n");
}
