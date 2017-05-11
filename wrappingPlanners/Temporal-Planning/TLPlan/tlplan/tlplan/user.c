/* user.c -- support for user library routines

Copyright 1997 - 99, Fahiem Bacchus

This module provides support for user written C routines.
	These libraries are loaded on request only, and are unloaded at
	program exit or by a call to (clear-world-symbols).

Windows:
	Within the Windows environment, user routines are placed in domain specific
	DLL's.
Sun OS:
	We have a very similar shared libarary interface.
Unix in General:
	We don't know yet.
*/

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef WIN32
#include <windows.h>
#else
#include <dlfcn.h>
#endif

#include "tlplan.h"
#include "domain.h"
#include "save.h"
#include "tlparse.h"
#include "tl_tab.h"
#include "user.h"
#include "util.h"
#include "zone.h"

/* local macro and structure definitions */

typedef struct library
{
	struct library *plNext;				/* pointer to next */
	char *psPath;						/* library path string */
	int nHandle;						/* library handle (or NULL) */
}LIBRARY, *LIBRARYP;

/* local function prototypes */

/* local data */

LIBRARYP plLibList;						/* list of open libraries */

/* NewLibrary

Description:
	Open a shared library and link its vital information to the library list.
	We don't worry about multple opens... leave that worry to the user.
*/

BOOL NewLibrary
(
	char *psPath,						/* path to library file */
	int *nHandle						/* returned handle */
)
{
	LIBRARYP plLib;

#ifdef WIN32
	char acPath[_MAX_PATH];
	char acDrive[_MAX_DRIVE];
	char acDir[_MAX_DIR];
	char acFname[_MAX_FNAME];
	char acExt[_MAX_EXT];

	/* insert the appropriate directory and file type for windows */

	_splitpath(psPath,acDrive,acDir,acFname,acExt);
#ifdef _DEBUG
	strcpy(acDir+strlen(acDir),"debug/");
#else /* _DEBUG */
	strcpy(acDir+strlen(acDir),"release/");
#endif /* _DEBUG */
	_makepath(acPath,acDrive,acDir,acFname,"dll");	
	
	/* attempt to open the library */

	*nHandle=(int)LoadLibrary(acPath);
	if(*nHandle<HINSTANCE_ERROR)
	{
		char *psMsgBuf;

		ErrorMessage("declare-external-symbols:  Failed to load library %s\n",acPath);
		FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER|FORMAT_MESSAGE_FROM_SYSTEM,
			NULL,GetLastError(),MAKELANGID(LANG_NEUTRAL,SUBLANG_DEFAULT),
			(LPTSTR)&psMsgBuf,0,NULL);
		ErrorMessage(psMsgBuf);
		LocalFree(psMsgBuf);
		EXIT("iDeclareExternalSymbols");
		return FALSE;
	}
#else /* WIN32 */

	/* attempt to open the library */

	*nHandle=(int)dlopen(psPath,RTLD_NOW);
	if(!*nHandle)
	{
		ErrorMessage("declare-external-symbols:  Failed to load library %s\n",psPath);
		ErrorMessage("%s\n",dlerror());
		EXIT("iDeclareExternalSymbols");
		return FALSE;
	}
#endif /* WIN32 */

	/* save path and handle in library list */

	plLib=(LIBRARYP)MemAlloc(sizeof(LIBRARY));
	plLib->psPath=StrAlloc(psPath);
	plLib->nHandle=*nHandle;
	plLib->plNext=plLibList;			/* link to library list */
	plLibList=plLib;
	return TRUE;
}

/* CloseLibraries

Description:
	Close all libraries in the library list.
*/

void CloseLibraries(void)
{
	LIBRARYP pl1,pl2;

	for(pl1=plLibList;pl1;pl1=pl2)
	{
		pl2=pl1->plNext;
#ifdef WIN32
		 FreeLibrary((HINSTANCE)pl1->nHandle);
#else /* WIN32 */
		if(dlclose((void *)pl1->nHandle))
		{
			ErrorMessage("CloseLibrary:  Failed to close library %s\n",pl1->psPath);
			ErrorMessage("%s\n",dlerror());
		}
#endif /* WIN32 */
//		ZoneFree(pl1->psPath);
//		ZoneFree(pl1);
	}
	plLibList=NULL;
}

/* MarkLibraries

Description:
	Mark the library list for garbage collection.
*/

void MarkLibraries(void)
{
	LIBRARYP pl;

	for(pl=plLibList;pl;pl=pl->plNext)
	{
		ZoneMark(pl);
	}
}

/* GetLibSymbol

Description:
	Lookup a symbol in a library.
*/

BOOL GetLibSymbol
(
	int nHandle,						/* library handle */
	char *psName,						/* symbol name */
	void **pvValue						/* symbol value */
)
{
#ifdef WIN32
	*pvValue=(void *)GetProcAddress((HINSTANCE)nHandle,psName);
	if(!*pvValue)
	{
		ErrorMessage("GetLibSymbol:  Failed to load symbol %s\n",psName);
/* add code here to get extended error information */
		return FALSE;
	}
#else /* WIN32 */
	*pvValue=dlsym((void *)nHandle,psName);
	if(!*pvValue)
	{
		ErrorMessage("GetLibSymbol:  Failed to load symbol %s\n",psName);
		ErrorMessage("%s\n",dlerror());
	}
#endif /* WIN32 */
	return TRUE;
}



