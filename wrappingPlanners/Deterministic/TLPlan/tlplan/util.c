/* util.c -- utility functions.

Copyright C, 1996 - 99  F. Bacchus

Confidence intervals...

calculate the mean
calculate the unbiased standard deviation (i.e. divide by n-1, not n)
divide by sqrt(n) to get the error of the mean
multiply by the student's t value for n-1 degrees of freedom, at 1-2a
where a is the confidence level 
(i.e. use .975 for 95% confidence, or .995 for 99% confidence)

*/

#include <ctype.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifdef WIN32
#include <windows.h>
#else /* WIN32 */
#include <sys/times.h>					// Linux, Solaris, HPUX, etc.
#include <unistd.h>						// Linux, Solaris
//#include <sys/time.h>					// BSD	
//#include <sys/resource.h>				// BSD
//#include <sys/param.h>				// BSD
//#include <sys/sysctl.h>				// BSD
#endif /* WIN32 */

#include "tlplan.h"
#include "btree.h"
#include "compute.h"
#include "hash.h"
#include "formula.h"
#include "iface.h"
#include "tlparse.h"
#include "tl_tab.h"
#include "util.h"
#include "strhash.h"
#include "zone.h"

/* local data */

static char acErrorBuf[256];			/* error message buffer */

/* fibonacci data */

static int iFib=16;						/* fibonacci lag -1 */
static int jFib=4;						/* fibonacci lag -1 */
static unsigned int nFibSeed=0x404DC89E;	/* congruential seed */

static unsigned int anFibCoef[17]=		/* fibonacci coefficients */
{
	0xC5B191E9,
	0xF2C4A426,
	0xCB46D09C,
	0xB612EADF,
	0x57CE4A07,
	0x3963E451,
	0x47A6C699,
	0x57FA4D15,
	0xBDA044E7,
	0xD0C15F5F,
	0xF37E75A3,
	0x101A61FF,
	0x34A21D1D,
	0xF11874EE,
	0x70FBD4FC,
	0xFE7CA439,
	0xC47087BA
};

static int nMSSeed;



/* Autodin II CRC (CRC-32) */

static unsigned int anCRCTab[256]=
{
	0x00000000, 0x77073096, 0xEE0E612C, 0x990951BA,
	0x076DC419, 0x706AF48F, 0xE963A535, 0x9E6495A3,
	0x0EDB8832, 0x79DCB8A4, 0xE0D5E91E, 0x97D2D988,
	0x09B64C2B, 0x7EB17CBD, 0xE7B82D07, 0x90BF1D91,
	0x1DB71064, 0x6AB020F2, 0xF3B97148, 0x84BE41DE,
	0x1ADAD47D, 0x6DDDE4EB, 0xF4D4B551, 0x83D385C7,
	0x136C9856, 0x646BA8C0, 0xFD62F97A, 0x8A65C9EC,
	0x14015C4F, 0x63066CD9, 0xFA0F3D63, 0x8D080DF5,
	0x3B6E20C8, 0x4C69105E, 0xD56041E4, 0xA2677172,
	0x3C03E4D1, 0x4B04D447, 0xD20D85FD, 0xA50AB56B,
	0x35B5A8FA, 0x42B2986C, 0xDBBBC9D6, 0xACBCF940,
	0x32D86CE3, 0x45DF5C75, 0xDCD60DCF, 0xABD13D59,
	0x26D930AC, 0x51DE003A, 0xC8D75180, 0xBFD06116,
	0x21B4F4B5, 0x56B3C423, 0xCFBA9599, 0xB8BDA50F,
	0x2802B89E, 0x5F058808, 0xC60CD9B2, 0xB10BE924,
	0x2F6F7C87, 0x58684C11, 0xC1611DAB, 0xB6662D3D,
	0x76DC4190, 0x01DB7106, 0x98D220BC, 0xEFD5102A,
	0x71B18589, 0x06B6B51F, 0x9FBFE4A5, 0xE8B8D433,
	0x7807C9A2, 0x0F00F934, 0x9609A88E, 0xE10E9818,
	0x7F6A0DBB, 0x086D3D2D, 0x91646C97, 0xE6635C01,
	0x6B6B51F4, 0x1C6C6162, 0x856530D8, 0xF262004E,
	0x6C0695ED, 0x1B01A57B, 0x8208F4C1, 0xF50FC457,
	0x65B0D9C6, 0x12B7E950, 0x8BBEB8EA, 0xFCB9887C,
	0x62DD1DDF, 0x15DA2D49, 0x8CD37CF3, 0xFBD44C65,
	0x4DB26158, 0x3AB551CE, 0xA3BC0074, 0xD4BB30E2,
	0x4ADFA541, 0x3DD895D7, 0xA4D1C46D, 0xD3D6F4FB,
	0x4369E96A, 0x346ED9FC, 0xAD678846, 0xDA60B8D0,
	0x44042D73, 0x33031DE5, 0xAA0A4C5F, 0xDD0D7CC9,
	0x5005713C, 0x270241AA, 0xBE0B1010, 0xC90C2086,
	0x5768B525, 0x206F85B3, 0xB966D409, 0xCE61E49F,
	0x5EDEF90E, 0x29D9C998, 0xB0D09822, 0xC7D7A8B4,
	0x59B33D17, 0x2EB40D81, 0xB7BD5C3B, 0xC0BA6CAD,
	0xEDB88320, 0x9ABFB3B6, 0x03B6E20C, 0x74B1D29A,
	0xEAD54739, 0x9DD277AF, 0x04DB2615, 0x73DC1683,
	0xE3630B12, 0x94643B84, 0x0D6D6A3E, 0x7A6A5AA8,
	0xE40ECF0B, 0x9309FF9D, 0x0A00AE27, 0x7D079EB1,
	0xF00F9344, 0x8708A3D2, 0x1E01F268, 0x6906C2FE,
	0xF762575D, 0x806567CB, 0x196C3671, 0x6E6B06E7,
	0xFED41B76, 0x89D32BE0, 0x10DA7A5A, 0x67DD4ACC,
	0xF9B9DF6F, 0x8EBEEFF9, 0x17B7BE43, 0x60B08ED5,
	0xD6D6A3E8, 0xA1D1937E, 0x38D8C2C4, 0x4FDFF252,
	0xD1BB67F1, 0xA6BC5767, 0x3FB506DD, 0x48B2364B,
	0xD80D2BDA, 0xAF0A1B4C, 0x36034AF6, 0x41047A60,
	0xDF60EFC3, 0xA867DF55, 0x316E8EEF, 0x4669BE79,
	0xCB61B38C, 0xBC66831A, 0x256FD2A0, 0x5268E236,
	0xCC0C7795, 0xBB0B4703, 0x220216B9, 0x5505262F,
	0xC5BA3BBE, 0xB2BD0B28, 0x2BB45A92, 0x5CB36A04,
	0xC2D7FFA7, 0xB5D0CF31, 0x2CD99E8B, 0x5BDEAE1D,
	0x9B64C2B0, 0xEC63F226, 0x756AA39C, 0x026D930A,
	0x9C0906A9, 0xEB0E363F, 0x72076785, 0x05005713,
	0x95BF4A82, 0xE2B87A14, 0x7BB12BAE, 0x0CB61B38,
	0x92D28E9B, 0xE5D5BE0D, 0x7CDCEFB7, 0x0BDBDF21,
	0x86D3D2D4, 0xF1D4E242, 0x68DDB3F8, 0x1FDA836E,
	0x81BE16CD, 0xF6B9265B, 0x6FB077E1, 0x18B74777,
	0x88085AE6, 0xFF0F6A70, 0x66063BCA, 0x11010B5C,
	0x8F659EFF, 0xF862AE69, 0x616BFFD3, 0x166CCF45,
	0xA00AE278, 0xD70DD2EE, 0x4E048354, 0x3903B3C2,
	0xA7672661, 0xD06016F7, 0x4969474D, 0x3E6E77DB,
	0xAED16A4A, 0xD9D65ADC, 0x40DF0B66, 0x37D83BF0,
	0xA9BCAE53, 0xDEBB9EC5, 0x47B2CF7F, 0x30B5FFE9,
	0xBDBDF21C, 0xCABAC28A, 0x53B39330, 0x24B4A3A6,
	0xBAD03605, 0xCDD70693, 0x54DE5729, 0x23D967BF,
	0xB3667A2E, 0xC4614AB8, 0x5D681B02, 0x2A6F2B94,
	0xB40BBE37, 0xC30C8EA1, 0x5A05DF1B, 0x2D02EF8D
};

/* global data */

FILE *pfError;							/* error message log file stream */
FILE *pfJBCRC;							/* crc32 file*/
int nErrorCount;						/* number of error messages emitted */

/* local function prototypes */

#ifdef WIN32
static double GetWinRunTime(void);
#endif /* WIN32 */

/* MemAlloc

Description:
	Safe memory allocation routine.
	Prints a message and exits if allocation fails.
Note:
	In general, it's cheaper to call CopyAlloc or StrAlloc when copying buffers
	or strings, since MemAlloc clears memory.
*/

DECLSPEC void *MemAlloc
(
	size_t nSize
)
{
	void *pv;

	ENTER("MemAlloc",TRUE);
	if(!nSize)
	{
		ErrorMessage("Allocated 0 bytes of memory\n");
		EXIT("MemAlloc");
		return NULL;
	}
/*	pv=calloc(nSize,1);		*/
	pv=ZoneAlloc(nSize);
	if(!pv)
		ErrorExit("Failed to allocate %d bytes of memory, exiting.\n",nSize);
	memset(pv,0,nSize);
	EXIT("MemAlloc");
	return(pv);
}

/* CopyAlloc

Description:
	Safe memory allocation routine.  Copies the supplied buffer
	Prints a message and exits if allocation fails.
*/

void *CopyAlloc
(
	void *pBuffer,
	size_t nSize
)
{
	void *pv;

	ENTER("CopyAlloc",TRUE);
/*	pv=malloc(nSize);		*/
	pv=ZoneAlloc(nSize);
	if(!pv)
	{
		ErrorMessage("Failed to allocate %d bytes of memory, exiting.\n",nSize);
		exit(1);
	}
	memcpy(pv,pBuffer,nSize);
	EXIT("CopyAlloc");
	return(pv);
}

/* StrAlloc

Description:
	Store a string in the string table. 
	If the string already exists, a pointer to it is returned.
	Otherwise a new hash entry is allocated and a pointer to the newly allocated
	memory is returned.
Notes:
	The string is not converted to lowercase, and there is no limit on length.
*/


DECLSPEC void *StrAllocAndPos(char *psString,int *position) {

  int nStatus;
  char *pps;


  ENTER("StrAllocAndPos",TRUE);

  if(!psString) {
    EXIT("StrAllocAndPos");
    return NULL;
  }

  nStatus=StrHashSearchInsert(psString,&pps,position);

  if (nStatus!=HASH__SUCCESS) {
    ErrorMessage("Failed to store identifier %s\n",psString);
    EXIT("StrAllocAndPos"); 
    return NULL; 
  }

  return pps;
}



/* DECLSPEC void *StrAlloc */
/* ( */
/* 	char *psString */
/* ) */
/* { */
/* 	int nStatus; */
/* 	char **pps; */

/* 	ENTER("StrAlloc",TRUE); */
/* 	if(!psString) */
/* 		psString=""; */

/* 	/\* first attempt to find the string already in the string table *\/ */
	
/* 	nStatus=HashSearch(&htStringTable,psString,(void **)&pps); */
/* 	if(nStatus==HASH__SUCCESS) */
/* 	{ */
/* 		EXIT("StrAlloc"); */
/* 		return *pps; */
/* 	} */
	
/* 	nStatus=HashInsert(&htStringTable,psString,(void **)&pps); */
/* 	if(nStatus!=HASH__SUCCESS) */
/* 	{ */
/* 		ErrorMessage("Failed to store string %s\n",psString); */
/* 		HashError(stderr,nStatus); */
/* 		EXIT("StrAlloc"); */
/* 		return NULL; */
/* 	} */
/* 	*pps=ZoneAlloc(strlen(psString)+1); */
/* 	if(!*pps) */
/* 	{ */
/* 		ErrorMessage("Failed to allocate %d bytes of memory, exiting.\n", */
/* 			strlen(psString)+1); */
/* 		exit(1); */
/* 	} */
/* 	strcpy(*pps,psString); */
/* 	EXIT("StrAlloc"); */
/* 	return(*pps); */
/* } */

/* IdentAlloc

Description:
	Store a string in the string table. 
	If the string already exists, a pointer to it is returned.
	Otherwise a new hash entry is allocated and a pointer to the newly allocated
	memory is returned.
Notes:
	The string is converted to lowercase before it is stored.
	The maximum string length is 255 characters.
*/

/* JB: New version of IdentAlloc, based on the strhash.c code */

DECLSPEC void *IdentAllocAndPos(char *psString,int *position) {

  int nStatus;
  char *pps;
  char buffer[256];
  char *ps1,*ps2;

  ENTER("IdentAlloc",TRUE);

  if(!psString) {
    EXIT("IdentAllocAndPos");
    return NULL;
  }
  
  // we copy & convert string to lowercase
  
  for(ps1=psString,ps2=buffer;*ps1;)
    *ps2++=(char)tolower(*ps1++);
  *ps2=NULL;

  nStatus=StrHashSearchInsert(buffer,&pps,position);

  if (nStatus!=HASH__SUCCESS) {
    ErrorMessage("Failed to store identifier %s\n",psString);
    EXIT("IdentAllocAndPos"); 
    return NULL; 
  }
  
  EXIT("IdentAllocAndPos"); 
  
  return pps;
}




/* DECLSPEC void *IdentAlloc */
/* ( */
/* 	char *psString */
/* ) */
/* { */
/* 	int nStatus; */
/* 	char acString[256];					/\* lowercase string buffer *\/ */
/* 	char *ps1,*ps2; */
/* 	char **pps; */

/* 	ENTER("IdentAlloc",TRUE); */
/* 	if(!psString) */
/* 	{ */
/* 		EXIT("IdentAlloc"); */
/* 		return NULL; */
/* 	} */

/* 	/\* convert the string to lowercase *\/ */

/* 	for(ps1=psString,ps2=acString;*ps1;) */
/* 		*ps2++=(char)tolower(*ps1++); */
/* 	*ps2=NULL; */

/* 	/\* first attempt to find the string already in the string table *\/ */
	
/* 	nStatus=HashSearch(&htStringTable,acString,(void **)&pps); */
/* 	if(nStatus==HASH__SUCCESS) */
/* 	{ */
/* 		EXIT("IdentAlloc"); */
/* 		return *pps; */
/* 	} */
	
/* 	nStatus=HashInsert(&htStringTable,acString,(void **)&pps); */
/* 	if(nStatus!=HASH__SUCCESS) */
/* 	{ */
/* 		ErrorMessage("Failed to store identifier %s\n",acString); */
/* 		HashError(stderr,nStatus); */
/* 		EXIT("IdentAlloc"); */
/* 		return NULL; */
/* 	} */
/* 	*pps=ZoneAlloc(strlen(acString)+1); */
/* 	if(!*pps) */
/* 	{ */
/* 		ErrorMessage("Failed to allocate %d bytes of memory, exiting.\n", */
/* 			strlen(acString)+1); */
/* 		exit(1); */
/* 	} */
/* 	strcpy(*pps,acString); */
/* 	EXIT("IdentAlloc"); */
/* 	return(*pps); */
/* } */

/* StringEqQ

Description:
	Compare two strings to see if they're equal.
Notes:
	We compare pointers first, hoping that saves some time, since most
	strings are identifiers, stored in the hash table.
*/

//BOOL StringEqQ
//(
//	char *ps1,
//	char *ps2
//)
//{
//	if(ps1==ps2)
//		return TRUE;
//	return !stricmp(ps1,ps2);
//}

/* Message

Description:
	Print and log a message.
	For now we send output to stdout.
	A Windows version would write the output to the window instead.
*/

void Message
(
	char *psFormat,
	...
)
{
	va_list pa;							/* argument pointer */

	ENTER("Message",TRUE);
	va_start(pa,psFormat);
	vsprintf(acErrorBuf,psFormat,pa);
	va_end(pa);
	CommandPuts(acErrorBuf,stdout);
	if(pfError!=stderr)
		CommandPuts(acErrorBuf,pfError);
	EXIT("Message");
}

/* ErrorMessage

Description:
	Print and log an error message.
	For now we send output to stderr.
	A Windows version would open a message box instead.
*/

DECLSPEC void ErrorMessage
(
	char *psFormat,
	...
)
{
	va_list pa;							/* argument pointer */

	ENTER("ErrorMessage",TRUE);
	nErrorCount++;
	va_start(pa,psFormat);
	vsprintf(acErrorBuf,psFormat,pa);
	va_end(pa);
	CommandPuts(acErrorBuf,stderr);
	if(pfError!=stderr)
		CommandPuts(acErrorBuf,pfError);
	if(nLineNumber)
	{
		sprintf(acErrorBuf,"Error occurred in %s on or before line %d\n",psCurrentFile,nLineNumber);
		CommandPuts(acErrorBuf,stderr);
		if(pfError!=stderr)
			CommandPuts(acErrorBuf,pfError);
	}

	if(pjbCurrentJump==&jbTLPlannerJump)
		longjmp(*pjbCurrentJump,1);		/* tear down stack */
	EXIT("ErrorMessage");
}

/* ErrorExit

Description:
	Print and log an error message then exit.
	For now we send output to stderr.
	A Windows version would open a message box instead.
*/

void ErrorExit
(
	char *psFormat,
	...
)
{
	va_list pa;							/* argument pointer */

	ENTER("ErrorExit",TRUE);
	va_start(pa,psFormat);
	vsprintf(acErrorBuf,psFormat,pa);
	va_end(pa);
	CommandPuts(acErrorBuf,stderr);
	if(pfError!=stderr)
	CommandPuts(acErrorBuf,pfError);
		EXIT("ErrorExit");
	exit(1);
}

/* DumpBindings

Description:
	Display a bindings list.
*/

void DumpBindings
(
	BINDINGP pbBindings
)
{
	BINDINGP pb;
	char ac[40];						// string buffer

	for(pb=pbBindings;pb;pb=pb->pbNext)
		Message("\t%s=%s\n",GetName(pb->pcVar->pfForm,ac),GetName(pb->pcVal->pfForm,ac));
}

/* GetInternalRunTime

Description:
	Get the time in milliseconds since an OS specific epoch.
	If possible, we provide accounted time.
Notes:
	On non-Windows systems, the default time calculation assumes a Linux, Solaris
	or HPUX system.  Commented out code is also provided for BSD, and if all 
	else fails, there is a call to the ANSI clock routine.  
*/

double GetInternalRunTime(void)
{
	double df;
#ifndef WIN32
// Linux, Solaris, HPUX, etc.
	struct tms usage;
// BSD	
//	struct rusage usage;
#endif /* win32 */

	ENTER("GetInternalRunTime",FALSE);
#ifdef WIN32
	df=GetWinRunTime();
#else /* WIN32 */

#ifndef CLK_TCK
#define CLK_TCK (sysconf(_SC_CLK_TCK))
#endif
	// times() returns accounted time on Linux, Solaris, HPUX etc.
	// on most systems, CLK_TCK is defined as 100, meaning that
	// the time wraps in either 250 or 500 days on a 32 bit processor.

	times(&usage);
	df=((double)usage.tms_utime+(double)usage.tms_stime)/CLK_TCK;

//	// getrusage() returns accounted time (among other things) on BSD
//	
//	getrusage(RUSAGE_SELF,&usage);
//	df=(double)usage.ru_utime.tv_sec+usage.ru_utime.tv_usec/1000000.0+
//		(double)usage.ru_stime.tv_sec+usage.ru_stime.tv_usec/1000000.0;
//
//	// the ANSI clock() routine causes trouble on many systems since
//	// CLOCKS_PER_SEC may be defined as 1000000, causing the time to wrap
//	// in either 35 or 70 minutes on a 32 bit processor.
//	
//	df=(double)clock()/CLOCKS_PER_SEC;
#endif /* WIN32 */
	EXIT("GetInternalRunTime");
	return df;
}

#ifdef WIN32
/* GetWinRunTime

Description:
	Get the current system time in seconds, as a double precision number.
	For Windows 95/98, this routine reports the elapsed time, using 
	QueryPerformanceCounter and QueryPerformanceFrequency to attain a 
	precision of better than 1 microsecond.
	For Windows NT, this routine reports the accounted CPU time using
	GetProcessTimes.
Notes:
	Windows 95/98 reports a clock frequency of 1193180 Hz on a PC.  The actual
	frequency is more likely 1193046.471111 Hz, which causes a 32 bit counter
	to overflow exactly once an hour... but we take their word for it anyways.
	We assume that the clock frequency is under 4294967296 Hz, so we can
	ignore the upper longword of the frequency.
*/

static double GetWinRunTime(void)
{
	double dfTime;

	ENTER("GetWinRunTime",FALSE);
	if(bNT)
	{
		FILETIME ftTemp;
		FILETIME ftKernal;
		FILETIME ftUser;

		GetProcessTimes(GetCurrentProcess(),&ftTemp,&ftTemp,&ftKernal,&ftUser);
		dfTime=(*(__int64 *)&ftKernal+*(__int64 *)&ftUser)/10000000.0;
	}
	else								/* Windows 95/98 */
	{
		unsigned int n[2];
		static double k0,k1;

		if(!k0)
		{
			if(!QueryPerformanceFrequency((LARGE_INTEGER *)n))
			{
				EXIT("GetWinRunTime");
				return 0.0;
			}
			k0=n[0];
			k1=4294967296.0/k0;
		}
		QueryPerformanceCounter((LARGE_INTEGER *)n);
		dfTime=n[1]*k1+n[0]/k0;
	}

	EXIT("GetWinRunTime");
	return dfTime;
}
#endif /* WIN32 */

/* SizeOfMemory

Description:
	Determine the size of physical memory.
*/

unsigned int SizeOfMemory(void)
{
#ifdef WIN32
	MEMORYSTATUS ms;
			
	ms.dwLength=sizeof(MEMORYSTATUS);
	GlobalMemoryStatus(&ms);
	return ms.dwTotalPhys;
#else // WIN32
// Linux, Solaris
	return (unsigned int)sysconf(_SC_PHYS_PAGES)*sysconf(_SC_PAGE_SIZE);
// BSD
//	int mib[2],physmem;
//	size_t len;
//    
//	mib[0]=CTL_HW;
//	mib[1]=HW_PHYSMEM;
//	len=sizeof(physmem);
//	sysctl(mib,2,&physmem,&len,0,0);
//	return (unsigned int)physmem;
#endif // WIN32
}


#ifndef WIN32

/* stricmp

Description:
	Case insensitive strcmp.
*/

int stricmp
(
	const char *s1,
	const char *s2
)
{
	ENTER("stricmp",TRUE);
	for(;tolower(*s1)==tolower(*s2);s1++,s2++)
		if(!*s1)
		{
			EXIT("stricmp");
			return 0;
		}
	EXIT("stricmp");
	return (*(unsigned char *)s1<*(unsigned char *)s2)?-1:1;
}

/* strlwr

Description:
	Convert a string to lower case, in situ.
*/

char *strlwr
(
	char *psString
)
{
	char *ps;

	for(ps=psString;*ps;++ps)
	{
		if('A'<=*ps&&*ps<='Z')
			*ps+='a'-'A';
	}
	return(psString);
}
#endif /* WIN32 */

/* CRC32

Description:
	Calculates the CRC for a buffer.  Uses a table with 256 entries.
*/

unsigned int CRC32
(
	unsigned char *pcBuffer,			/* pointer to buffer */
	unsigned int nSize,					/* buffer size */
	unsigned int nCRC					/* seed or previous crc */
)
{
	unsigned char *pc;
	unsigned int i;

	pc=pcBuffer;
	for(i=0;i<nSize;i++)
	{
		nCRC^=*pc++;
		nCRC=(nCRC>>8)^anCRCTab[nCRC&0x000000FF];
	}
	return(nCRC);
}

/* FormatText

Description:
	Format a string, with scheme/lisp type escapes.

Supported format escapes:

	~A -- print string or symbol name (@,n optional)
	~D -- print decimal integer (no options)
	~F -- print float (d,w optional)
	~S -- print string or symbol name (@,n optional)
	~T -- tab stop (n,r optional)
	~% -- newline
	~~ -- tilde
*/

BOOL FormatText
(
	char *psBuffer,						/* output buffer */
	char *psFormat,						/* format string */
	CELLP pcArgs,						/* argument list */
	LINEARPLANP plpLinearPlan,
	BINDINGP pbBindings
)
{
	char *ps1,*ps2,*ps3,*ps4;			/* string pointers */
	double df;							/* generic float */
	int nOpt1,nOpt2;					/* optional parameters */
	int fOpt1,fOpt2;					/* parameter present flags */
	int fAt;							/* at sign present flag */
	CELLP pc;							/* formula pointer */
	int n;								/* generic integer */
	char ac[40];						/* string buffer */

	/* scan the format buffer copying characters and interpreting escapes */

	ps1=psBuffer;
	ps2=psFormat;
	for(ps2=psFormat;*ps2;)
	{
		if(*ps2=='~')
		{
			ps4=ps2;					/* save for error message */
			ps2++;						/* skip over escape */
			fAt=FALSE;
			if(*ps2=='@')
				fAt=TRUE;
			fOpt1=FALSE;
			fOpt2=FALSE;
			nOpt1=strtol(ps2,&ps3,10);
			nOpt2=0;
			if(ps3!=ps2)
			{
				fOpt1=TRUE;
				ps2=ps3;
			}
			if(*ps2==',')
			{
				ps3++;
				nOpt2=strtol(ps2,&ps3,10);
				if(ps3!=ps2)
				{
					fOpt2=TRUE;
					ps2=ps3;
				}
			}
			switch(toupper(*ps2++))
			{
				case 'A':				/* string */
				case 'S':				/* string */
					if(!pcArgs)
					{
						ErrorMessage("print:  Not enough arguments for format: \n%s\n",
							psFormat);
						*ps1=0;
						return FALSE;
					}
					pc=ComputeTerm(pcArgs,plpLinearPlan,pbBindings);
					if(!pc)
						TermError("eval-print: ~A",pcArgs,pbBindings);
					pcArgs=pcArgs->pcNext;
					if(fOpt1)
					{
						if(fAt)
							sprintf(ps1,"%*s",nOpt1,GetName(pc->pfForm,ac));
						else
							sprintf(ps1,"%*s",-nOpt1,GetName(pc->pfForm,ac));
					}
					else
						strcpy(ps1,GetName(pc->pfForm,ac));
					ps1+=strlen(ps1);
					break;
				case 'D':				/* decimal integer */
					if(!pcArgs)
					{
						ErrorMessage("print:  Not enough arguments for format: \n%s\n",
							psFormat);
						*ps1=0;
						return FALSE;
					}
					if(!FormulaToInteger(pcArgs,plpLinearPlan,pbBindings,&n))
					{
						ErrorMessage("print:  Non-numeric argument to %.*s format specifier.\n%s\n",
							ps2-ps4,ps4,psFormat);
						*ps1=0;
						return FALSE;
					}
					pcArgs=pcArgs->pcNext;
					sprintf(ps1,"%d",n);
					ps1+=strlen(ps1);
					break;
				case 'F':				/* float */
					if(!pcArgs)
					{
						ErrorMessage("print:  Not enough arguments for format: \n%s\n",
							psFormat);
						*ps1=0;
						return FALSE;
					}
					if(!FormulaToDouble(pcArgs,plpLinearPlan,pbBindings,&df))
					{
						ErrorMessage("print:  Non-numeric argument to %.*s format specifier.\n%s\n",
							ps2-ps4,ps4,psFormat);
						*ps1=0;
						return FALSE;
					}
					pcArgs=pcArgs->pcNext;
					if(!fOpt1&&!fOpt2)
						sprintf(ps1,"%f",df);
					else if(!fOpt1&&fOpt2)
						sprintf(ps1,"%.*f",nOpt2,df);
					else if (fOpt1&&!fOpt2)
						sprintf(ps1,"%*f",nOpt1,df);
					else
						sprintf(ps1,"%*.*f",nOpt1,nOpt2,df);
					ps1+=strlen(ps1);
					break;
				case 'T':				/* tab */
					if(!fOpt1)
						nOpt1=1;
					if(!fOpt2)
						nOpt2=1;
					n=ps1-psBuffer;
					if(nOpt1>n)
					{
						ps4=psBuffer+nOpt1;
						while(ps1<ps4)
							*ps1++=' ';
					}
					else if(nOpt2>0)
					{
						ps4=psBuffer+nOpt1+(n-nOpt1+nOpt2)/nOpt2;
						while(ps1<ps4)
							*ps1++=' ';
					}
					break;
				case '~':				/* tilde */
					*ps1++='~';
					break;
				case '%':				/* newline */
					*ps1++='\n';
					break;
				default:
					ErrorMessage("Unsupported format specifier:  %.*s\n%s\n",
						ps2-ps4,ps4,psFormat);
					*ps1=0;
					return FALSE;
			}
		}
		else
			*ps1++=*ps2++;
	}
	*ps1=0;								/* make sure string is terminated */
	return TRUE;
}

/* Random Number Routines ------------------------------------------------------ */

/* Fibonacci random number generator.

These routines implement a combined fibonacci/congruential generator.
This code is based on code written at NBS.

The original algorithm generated numbers in the range 0.0 to 1.0.
The code was converted to generate integers (with the aid of a linear
congruential generator in Knuth, "Seminumerical Algorithms").

The fibonacci generator is of lags 17 and 5, giving a period of about 2**17 * 2**32.
The conguential generator is of period 2**32.
The total period is then 2**81 or about 10**24.

Usage:

Use FibSeed(nSeed) to seed the generator.
Use FibRand() to generate a new random number.
*/

/*	The default fibonacci array has been loaded by first seeding the array with
	FibSeed(305), and then calling FibRand(100000) times.
	The initial seed of the conguential algorithm is the result of
	the 100000th call to FibRand, starting with a seed of 1. */

DECLSPEC int FibRand(void)
{
	unsigned int nValue;

	/* fibonacci generator */

	anFibCoef[iFib]-=anFibCoef[jFib];
	nValue=(int)anFibCoef[iFib];
	if(--iFib<0)
		iFib=16;
	if(--jFib<0)
		jFib=16;

	/* congruential generator */

	nFibSeed=nFibSeed*1664525+1;

	/* combined generator */

	nValue-=nFibSeed;
	return (int)(nValue&0x7FFFFFFF);	/* reduce to 31 bits */
}

/* FibSeed

Description:
	Fill in the fibonacci coefficients.
	Each coefficient is filled in as a random sequence of ones and zeros.
*/

DECLSPEC void FibSeed
(
	int nValue							/* seed value */
)
{
	int nSum;
	int i1,j1,k1,l1,m1;
	int i,j;

	nFibSeed=(unsigned int)nValue*1664525+1;

	/* Convert seed to four smallish positive integers. */

	i1=(nFibSeed%177)+1;
	j1=(nFibSeed%167)+1;
	k1=(nFibSeed%157)+1;
	l1=(nFibSeed%147)+1;

	/* Generate random bit pattern in array based on given seed. */

	for(i=0;i<17;i++)
	{
		nSum=0;
		for(j=0;j<32;j++)		/* for each bit */
		{
			nSum*=2;
			m1=(((i1*j1)%179)*k1)%179;
			i1=j1;
			j1=k1;
			k1=m1;
			l1=(53*l1+1)%169;
			if(((l1*m1)%64)>=32)
				nSum++;
		}
		anFibCoef[i]=nSum;
	}
	iFib=16;
	jFib=4;
}

/* MSRand

Description:
	Generate a random number.
	This algorithm is compatible with Microsoft's rand function.
*/

int MSRand(void)
{
	nMSSeed=nMSSeed*214013+2531011;			/* 0x00034DFD & 0x00269EC3 */
	return (nMSSeed>>16)&0x00007FFF;
}

/* MSSeed

Description:
	Seed the Microsoft compatible random number generator.
*/

void MSSeed
(
	int nSeed
)
{
	nMSSeed=nSeed&0x7FFF;
}

