/* userlib.c -- tlplanner generic user dynalink library

Copyright C, 1997  F. Bacchus

*/

#ifdef WIN32
#include <windows.h>

#include "../tlplan.h"

/* global data */

HINSTANCE hInstance;					/* Global instance handle for DLL */

/* DLLMain/LibMain ------------------------------------------------------------- */

DECLSPECX BOOL WINAPI DllMain
(
	HANDLE hModule, 
	DWORD dwReason, 
	LPVOID lpReserved
)
{
	switch(dwReason)
	{
		case DLL_PROCESS_ATTACH:
			hInstance=(HINSTANCE)hModule;
			break;
		case DLL_THREAD_ATTACH:
		case DLL_THREAD_DETACH:
		case DLL_PROCESS_DETACH:
			break;
	}
	return 1;
}

#endif /* WIN32 */


