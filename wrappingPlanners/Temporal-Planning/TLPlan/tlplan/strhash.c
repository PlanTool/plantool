#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <stddef.h>


#include "tlplan.h"
#include "btree.h"
#include "formula.h"
#include "tlparse.h"
#include "tl_tab.h"
#include "save.h"
#include "util.h"
#include "zone.h"
#include "strhash.h"

#define MAX_STR_HASH 617 /* should be a prime number to avoid colissions */

typedef struct StrIden {
    char *str;
    struct StrIden *next;
} STRIDEN, *STRIDENP;

static STRIDENP *StrHash;
static int nStrHashSize;
static int nStrHashCollisions;

/* Intializes the hash table */

int StrHashCreate()
{
  StrHash=(STRIDENP *)MemAlloc(sizeof(STRIDENP)*MAX_STR_HASH);
  if (StrHash) StrHashClear();
  nStrHashSize=0;
  nStrHashCollisions=0;
  return (int)StrHash;
}

void StrHashClear() {
  memset(StrHash,0x0,sizeof(STRIDENP)*MAX_STR_HASH);
}



static const int anFibonacci[16]=				/* table of 24-bit fibonacci coefficients */
{
	0x00834271L,						/* 0 */
	0x00915E23L,						/* 1 */
	0x00D85447L,						/* 2 */
	0x00F71B95L,						/* 3 */
	0x00A19325L,						/* 4 */
	0x00A04C01L,						/* 5 */
	0x00E78139L,						/* 6 */
	0x00810A49L,						/* 7 */
	0x00BB1F71L,						/* 8 */
	0x00FE19E3L,						/* 9 */
	0x00C0AB43L,						/* 10 */
	0x0091A8EBL,						/* 11 */
	0x00C23AE9L,						/* 12 */
	0x00BA33E5L,						/* 13 */
	0x00ED3259L,						/* 14 */
	0x00DD103DL							/* 15 */
};


/* static int StrPosIdent(char *str) { */
/*   unsigned int nCRC; */
/*   nCRC=CRC32((unsigned char *)str,strlen(str),0); */
/*   return nCRC%MAX_STR_HASH; */

/* } */


/* Returns the position in the table for a string */

/* static int StrPosIdent */
/* ( */
/* 	char *psKey */
/* ) */
/* { */
/* 	unsigned int nValue;				/\* hash value *\/ */
/* 	unsigned char *ps;					/\* string pointer *\/ */

/* 	ENTER("Hash",TRUE); */
/* 	nValue=0; */
/* 	for(ps=(unsigned char *)psKey;*ps;ps++) */
/* 		nValue+=anFibonacci[(nValue>>12)&0x0000000F]**ps; */
/* 	EXIT("Hash"); */
/* 	return(nValue%MAX_STR_HASH); */
/* } */


static int StrPosIdent(char *str)
{
  char *s;
  int sum;
  sum = 0;
  for (s = str; *s; s++)
    sum += *s;

  return (int) sum%MAX_STR_HASH;
}

int StrHashSearchInsert(char *s,char **result,int *position) 
{ 
  int pos;
  STRIDENP *idptr; 
  ZONEP pz=pzCurrent;
  BOOL bNonEmpty;
  
  pos = StrPosIdent(s); 
  *position = pos;
  
  //  if (!strcmp(s,"crate1")) {
  //  printf("I am SEEING crate1\n");
  //}


  idptr = &StrHash[pos]; 
  
  bNonEmpty=(BOOL)StrHash[pos];

  for (idptr = &StrHash[pos]; *idptr ; idptr = &((*idptr)->next)) {
    if (!strcmp((*idptr)->str,s)) {
      *result = (*idptr)->str; 
      return HASH__SUCCESS;
    }
  }

  SetZone(&zPermanent);
  *idptr = (STRIDENP) MemAlloc(sizeof(STRIDEN));
  if (!*idptr) { 
    SetZone(pz);
    return HASH__NO_MEMORY;
  }
  (*idptr)->str = (char *) MemAlloc(strlen(s)+1); 
  strcpy((*idptr)->str,s);

  SetZone(pz);

  
  if (!(*idptr)->str) {
    return HASH__NO_MEMORY;
  }
    
  (*idptr)->next = NULL; 
  
  *result = (*idptr)->str; 
  
  //  printf("pos=%d\n",*position);
  
  
  nStrHashSize++;
  if (bNonEmpty) nStrHashCollisions++; /* we have detected a collisions */
  
  return HASH__SUCCESS;
} 



void MarkStrHash() {
  
  int i;
  STRIDENP psid;
  
  ZoneMark(StrHash);
  
  for (i=0; i<MAX_STR_HASH; i++)
    for (psid=StrHash[i]; psid; psid=psid->next) { 
      ZoneMark(psid->str);
      ZoneMark(psid);
    }

}


int StrHashGetCount() {
  return nStrHashSize;
}
int StrHashGetCollisions() {
  return nStrHashCollisions;
}
int StrHashGetTableSize() {
  return MAX_STR_HASH;
}

void StrHashDump() {
  STRIDENP psid;
  int i;

  for (i=0; i<MAX_STR_HASH; i++) {
    printf("POSITION %d\n",i);
    for (psid=StrHash[i]; psid; psid=psid->next)
      printf("%s,",psid->str);
    printf("\n");
  }
}
