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

 * File: tif_parse.c

 * Authors:  Chih-wei Hsu and Benjamin W. Wah

 *

 *********************************************************************/
/*
# Copyright (C) 2004, Board of Trustees of the University of Illinois.
#
# The program is copyrighted by the University of Illinois, and should
# not be distributed without prior approval.  Commercialization of this 
# product requires prior licensing from the University of Illinois.
# Commercialization includes the integration of this code in part or 
# whole into a product for resale. 
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Author: Y. X. Chen, C. W. Hsu, and B. W. Wah, University of Illinois
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>
//#include <time.h>

//#define TEST_OVERFLOW    1

#define MAX_SPECIAL_LINES 2048
#define MAX_LINE_CHARS    1024


int parse_time_file(char *inputFileName, char *outputFileName, float atNumbers[], char *atStrings[]) {

    char matchString[] = "(at";
    char matchStringPrefix[] = "(";
    char matchStringPostfix[] = "))\0";

    int  matchStringSize = strlen(matchString);
    int  matchStringPrefixSize = strlen(matchStringPrefix);
    int  matchStringPostfixSize = strlen(matchStringPostfix);

    char lineStringBuffer[MAX_LINE_CHARS];
    char *pStr;
    char endChar = '\0';

    char targetString[MAX_LINE_CHARS];
    int  targetStringSize;
    int  targetNumber;

    int  atCount = 0;

    FILE *inFile;
    FILE *outFile;

    if ((inFile = fopen(inputFileName, "r")) == NULL || (outFile = fopen(outputFileName, "w")) == NULL) {
         printf("Can not open %s or can not create %s !\n", inputFileName, outputFileName);
         exit(1);
    }

    while (!feof(inFile) && fgets(lineStringBuffer, MAX_LINE_CHARS, inFile)) {
        sscanf(lineStringBuffer, "%s", targetString);

        if (strcmp(targetString, matchString) == 0) {
            pStr = strstr(lineStringBuffer, targetString);
            pStr += matchStringSize;
            strcpy(targetString, pStr);

            sscanf(targetString, "%d", &targetNumber);
            atNumbers[atCount] = targetNumber;

            pStr = strstr(targetString, matchStringPrefix);
            pStr += matchStringPrefixSize;

            targetStringSize = strlen(pStr) - matchStringPostfixSize;
            atStrings[atCount] = (char *)malloc(sizeof(char)*(targetStringSize));
            strncpy(atStrings[atCount], pStr, targetStringSize);
            atStrings[atCount ++][targetStringSize-1] = endChar;

            #ifdef TEST_OVERFLOW
            if (atCount >= MAX_SPECIAL_LINES) {
                printf("Overflow!");
                exit(1);
            }
            #endif
        }
        else {
            fprintf(outFile, lineStringBuffer);
        }
    }

    fclose(inFile);
    fclose(outFile);

    return atCount;
}

/*
int main(int argc, char *argv[]) {

    float atNum[MAX_SPECIAL_LINES];
    char *atStr[MAX_SPECIAL_LINES];
    int  i;
    clock_t startTime, endTime;
    int count;


    if (argc < 3) {
        puts("Usage error!");
        puts("<example>");
        puts("parse  test1.pddl outTest1.pddl");
        exit(1);
    }

    startTime = clock();
    count = parse_time_file(argv[1], argv[2], atNum, atStr);
    endTime = clock();
    printf("#parse %s to %s, used time = %f s\n", argv[1], argv[2], (endTime-startTime)/CLK_TCK);
    for (i = 0; i < count; i ++) {
        printf("%d \"%s\"\n", (int)atNum[i], atStr[i]);
        free(atStr[i]);
    }
    printf("#atCount = %d\n\n", count);

    return 1;
}

*/





