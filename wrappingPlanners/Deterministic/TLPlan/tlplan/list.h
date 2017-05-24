#ifndef __LIST_H
#define __LIST_H

/* list.h -- list handling functions */

/* Defined functions ---------------------------------------------------------- */

//#define Car(pl) (Nth(0,(pl)))
//#define Cdr(pl) ((pl)->plNext)
//#define Cadr(pl) (Nth(1,(pl)))

/* Structure member functions (to hide ugly structure syntax) */

//#define ListNext(pl)		((pl)->plNext)
//#define ListName(pl)		((pl)->psName)
//#define ListType(pl)		((pl)->nType)
//#define ListInteger(pl)		((pl)->u.nInteger)
//#define ListList(pl)		((pl)->u.plList)
//#define ListTree(pl)		((pl)->u.pbtTree)
//#define ListFunction(pl)	((pl)->u.pfFunction)

/* First, Second, Third... Rest

Description:
	These function like Car, Cadr, Caddr... and Cdr,
	except they can be applied to the empty list.
*/

//#define First(pl)	(Nth(0,(pl)))
//#define Second(pl)	(Nth(1,(pl)))
//#define Third(pl)	(Nth(2,(pl)))
//#define Fourth(pl)	(Nth(3,(pl)))
//#define Rest(pl)	(Nth(1,(pl)))

/* Data Structures ------------------------------------------------------------- */

/* Lists

Lists have essentially all the same fields as symbols do, and an additional
"pointer to next" field.
All list structures except for list pointers have a name.
For strings and literals, the string is the name.
For numbers, the name is a 10 character uppercase hexadecimal representation
of the value.
For functions, the name is the name of the function in the user library.
*/

/* Member functions ------------------------------------------------------------ */

DECLSPEC LISTP NewList					/* create a new list element */
(
	int nType,							/* type of atom */
	char *psName,						/* literal (string) value */
	...									/* value */
);
LISTP CopyAtom							/* Copy an atom (or first atom of a list) */
(
	LISTP plList						/* atom to copy */
);
//BOOL AtomQ
//(
//	LISTP plList
//);
LISTP CopyList							/* copy an entire list */
(
	LISTP plList						/* input list */
);
//void FreeList							/* discard an entire list */
//(
//	LISTP plList						/* input list */
//);
//LISTP CopyCar							/* copy the "value" of first element of a list */
//(
//	LISTP plList						/* input list */
//);
//LISTP CopyCons							/* 	generate a list from two lists or atoms */
//(
//	LISTP lpLeft,						/* input list or atom */
//	LISTP lpRight						/* input list or atom */
//);
//LISTP CopyCdr							/* copy the tail of a list */
//(
//	LISTP plList						/* input list */
//);
LISTP Cons
(
	LISTP plLeft,						/* input list or atom */
	LISTP plRight						/* input list or atom */
);
//LISTP Nth								/* point to the nth element of a list */
//(
//	int n,								/* selector */
//	LISTP plList						/* input list */
//);
//int Length								/* count the number of elements in a list */
//(
//	LISTP plList
//);
//LISTP List
//(
//	LISTP plArg1,						/* first argument */
//	...
//);
void PrintList
(
	FILE *pf,							/* output file stream */
	LISTP plList,						/* list to display */
	int nLevel							/* recursion level (initialize to 0) */
);
//char *ListToString
//(
//	LISTP plList,						/* list to convert */
//	char *psBuffer						/* string buffer */
//);
//int ListStringLength
//(
//	LISTP plList,						/* list to measure */
//	int nCount							/* string length, (initialize to 1) */
//);
//BOOL ListEqQ
//(
//	LISTP plList1,
//	LISTP plList2
//);
//BOOL FirstEqQ
//(
//	LISTP plList1,
//	LISTP plList2
//);
//LISTP CopyListFirst
//(
//	LISTP plList						/* input list */
//);
//LISTP Memq
//(
//	LISTP plObject,						/* object of search */
//	LISTP plList						/* list to search */
//);
void MarkList
(
	LISTP plList						/* input list */
);
void ZoneCopyAtom
(
	LISTP plAtom						/* atom to Copy */
);
void ZoneCopyList
(
	LISTP plList						/* input list */
);
void ZoneRelocAtom
(
	LISTP plAtom						/* atom to Reloc */
);
void ZoneRelocList
(
	LISTP plList						/* input list */
);

#endif /* __LIST_H */
