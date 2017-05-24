/* tllib.c -- tlplanner user dynalink library

Copyright C, 1997 - 99  F. Bacchus

*/

#include <stdio.h>

//#define _M_IX86 300
//#define _X86_

#include <windows.h>

#include "tlplan.h"

/* global data */

HINSTANCE hInstance;					/* Global instance handle for DLL */

/* DLLEntryPoint --------------------------------------------------------------- */

DECLSPEC BOOL WINAPI DllEntryPoint
(
	HANDLE hModule,
	DWORD dwReason,
	LPVOID lpReserved
)
{
	switch(dwReason)
	{
		case DLL_PROCESS_ATTACH:
			hInstance=(HINSTANCE)hModule;	/* save our instance handle */
			break;
		case DLL_THREAD_ATTACH:
		case DLL_THREAD_DETACH:
		case DLL_PROCESS_DETACH:
			break;
	}
	return 1;
}



