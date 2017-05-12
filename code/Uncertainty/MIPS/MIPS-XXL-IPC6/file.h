/*******************************************************************
 *     File.h
 *     Defines a wrapper on the C-FILE structure.
 *     Motivation: Some linux file systems have a limit on the size 
 *                 of the file.
 *     Functionality: A file that is larger than the maximum allowed
 *                    size can be broken into multiple files.
 *                    This functionality must be transparent to the
 *                    calling program
 ******************************************************************/


#ifndef _FILE_H
#define _FILE_H

#include "ff.h"
#include<sys/stat.h>
#include<stdio.h>
#include <assert.h>
#include <string.h>


typedef struct _EPS{
    State *state;
    State *pred;
}EPS;

typedef unsigned int _file_int;

/*
 * -- USED FOR EXTERNAL PLANNING BY FILE.H
 */

#define         APPEND_MODE        "a+b"
#define         OVER_WRITING_MODE   "r+b"
#define         READ_MODE          "rb"
#define         WRITE_MODE         "wb"
#define         PATH               "."
/*#define         TEMP_FILE_PREFIX  "tempBucket"*/



typedef struct _File{

    /** 
     * The G-value of the File. Useful for opening the next parts. 
     */
    int       g; 
    /** 
     * The H-value of the File. Useful for opening the next parts. 
     */
    int       h;
    int       processID;
    int       currentPart;
    int       discarded;
    _file_int bytesWritten;
    _file_int bytesRead;

    /**
     * In case of distributed SPIN, one is allowed to read only a part of 
     * the file. startByte and endByte defines this limit. Both numbers 
     * are inclusive in the limit.
     */ 
    _file_int startByte; 
    _file_int endByte;

    /**
     * These two variables define the part of the file where start and 
     * end bytes can be found.
     */
    short     startingPart;
    short     endPart;
   
    /*
     * The mode in which current file is opened. 
     * We need to remember it for different parts.
     */
    char mode[5]; 

    FILE* filep; 
    char filename[80];

    /**
     * When we are finished with reading from a file, we need to look for 
     * its further parts. If we do not find any, we have to restore the file
     * pointer values. 
     * These two variables are used by backup() and restore() for this purpose.
     */
    FILE* backup_filep; 
    char backup_filename[80];

    /**
     * Identify if a file is opened or not.
     * Its status can be called by isOpened()
     */
    Bool opened;
    
    int baseSize;
}File;

void file_init(File* file, int nprrocessID);
void file_init_g_h(File* file, int ng, int nh, int nprocessID);

/**
 * File opening functions.
 */
Bool openAppend    (File* file);
Bool openWrite     (File* file);
Bool openRead      (File* file);
Bool openOverWrite(File *file);

Bool jumpToMiddle (File* file, int toPart,  _file_int middleByte);

void closeFile    (File* file);
Bool isOpened     (File* file);
Bool copyTo       (File* file, int copy_g, int copy_h, int nprocessID);
void rewindFile   (File* file);
Bool openNextPart (File* file);

/*
 * Removes all parts of a file.
 * Equivalent to rm M-g-h*.
 * Executes a remove command of stdio.h on all parts of the files
 */
void remove_file  (int g, int h);

void rename_file (int src_g, int src_h, int dest_g, int dest_h);

/**
 * A generic function used by openAppennd(), openWrite() and openRead()
 */
Bool      openFile      (File* file, const char* mode);

/**
 * Used to query the file size to mark the start of a new part.
 */
_file_int fileSize (File* file);

void      file_backup(File* file);
void      file_recover(File* file);

unsigned int write_state_to_file (File* file, State* state);
unsigned int read_state_from_file  (File* file, State* state);
int  read_eps_from_file(File *f, EPS *eps);
int  write_eps_to_file(File *f, EPS *eps);

void print_F_vector(State *S);
int count_file_buffers();

#endif
