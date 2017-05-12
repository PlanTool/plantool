#ifndef _BUCKET_H
#define _BUCKET_H

#include <stdlib.h>
#include <limits.h>
#include "ff.h"
#include "memory.h"
#include "search.h"
#include "file.h"

typedef struct _Bucket {
    State *states;
    State *preds;
    State *backup_states;
    State *backup_preds;

    int g;
    int h;
    int size;
    int current;
    int MAX_SIZE;
    Bool completely_read; 
    Bool file_opened;
    File *s_file;
    Bool SORT;
    unsigned int inserted;
    unsigned int duplicates;
    
} Bucket;

void close_bucket(Bucket* b);
void insert_in_bucket(Bucket *b, State *S, State *father, int operator);
int compare_states(const void* s1, const void* s2);
float cost(const State* s);
void flush_bucket(Bucket *b);
void initialize_bucket(Bucket *b, int g, int h, Bool SORT);
void init_bucket_file_only(Bucket *b);

void allocate_states_in_bucket(Bucket *b);

int read_in_bucket(Bucket *b);

void allocate_state(State*);
void free_state(State* s);
float cost(const State* s);
Bool verify_buffer_flush(Bucket *b, int count, int file_part, int middle_byte);
void allocate_bucket(Bucket *b, int size);
void free_bucket(Bucket *b);

State* get_next(Bucket *b);
State* get_pred(Bucket *b);
State* get_previous(Bucket *b);

#endif
