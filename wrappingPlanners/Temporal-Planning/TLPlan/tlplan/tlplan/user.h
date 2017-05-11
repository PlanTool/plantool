/* user.h */

BOOL NewLibrary
(
	char *psPath,						/* path to library file */
	int *nHandle						/* returned handle */
);
BOOL GetLibSymbol
(
	int nHandle,						/* library handle */
	char *psName,						/* symbol name */
	void **pvValue						/* symbol value */
);
void CloseLibraries(void);
void MarkLibraries(void);

