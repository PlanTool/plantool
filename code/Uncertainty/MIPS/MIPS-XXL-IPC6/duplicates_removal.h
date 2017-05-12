#ifndef _ExternalMerge_
#define _ExternalMerge_

#include<limits.h>
#include<sys/resource.h>
#include<unistd.h>

#include "file.h"
#include "bucket.h"
#include "ff.h"

#define MAX_IO_BUFFERS 1000
#define BUFFER_FILE_SIZE 500


#define FILE_DOESNOT_EXIST  -1
#define EMPTY_FILE          -2

typedef struct _DR{

    /* It keeps track of the number of elements read from ith buffer. */
    unsigned int elementsReadFromBuffer[MAX_IO_BUFFERS];

    /* Total number of elements read from the ith block. */
    unsigned int totalElementsInTheBuffer[MAX_IO_BUFFERS];

    int totalFilePointers;
    unsigned int totalBytesReadPrevious ;
    unsigned int totalBytesReadCurrent  ;
    unsigned int tempTotalBytesRead     ;

    int previousFilePart ;
    int currentFilePart ;

    
    File *bf[MAX_IO_BUFFERS];

    File *bfSB, *bfOutput;
    
    /* Actually it should be k only*/
    EPS* pss[MAX_IO_BUFFERS];

    /*Bucket* bucketF[MAX_IO_BUFFERS];*/
    Bucket* bucketOutput;

    
}DR;

void init_DuplicatesRemoval(DR *dr);
void free_DuplicatesRemoval(DR *dr);
void initializeBufferFiles(DR *dr, int total, int g, int h);

int readElementGeneric(DR *dr, int i);

void allocate_eps(EPS *eps);
void free_eps(EPS *eps);
/*
 * returns the min state index number 
 * If we want to use a heap or a balanced tree, 
 * we have to stick only to this function and
 * readElement function.
 */
int delMin(DR *dr, int k);
Bool SearchBuffers(DR *dr, int g, int h, int *buffers, int procID, int first_call, Bool *file_read);

/*
 * @return total duplicates found while merging.
 */
unsigned int MergeBuffers(DR *dr, int totalBuffers, 
			  int *totalElementsWritten);

    
void free_DuplicatesRemoval(DR *dr );
int mergeAndRemoveDuplicates(DR *dr, int g, int h, 
			     int totalProcesses, 
			     int *duplicaetsWithin);

int compare( State  *exps1, State *exps2);
    
void setInvalid(State *exPS);
void copy_eps(EPS *dest, EPS *src);

void copy_file(int src_g, int src_h, int dest_g, int dest_h);

int subtractFilesOfAboveLayers(
    DR *dr, int g, int h, int locality, int *duplicates,  int hval_initial_state, int max_depth);

int testAllBuffers(DR *dr, int totalBuffers);

int subtract(File* top, File* main_file, File* temp, int *duplicates); 

#endif
