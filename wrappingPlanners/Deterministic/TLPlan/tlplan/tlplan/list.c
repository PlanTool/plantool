// this stuff is still used in world.c and btree.c...  can we get rid of it?


/* list.c -- basic list class

Copyright C, 1996, 99  F. Bacchus

Notes:
	-This is an implementation of lazy lists.  This implementation aims to
	provide greater efficiency than true lists, by making it possible to
	avoid unnecessary copying and the associated generation of garbage.
	-There are no distinct atom structures; We often call any single list
	structure an atom.
	-There are no car.cdr cells, so Car(pl) simply returns pl, unless the
	first element of pl was itself a list.  That is, Car(pl) typically returns
	a list, even if the first element of pl was not a list.
	-Many operators which expect atoms as operands implicitly ignore the fact
	that the operand may have an extraneous tail.  Where it makes a difference,
	special precautions are necessary, or CopyCar(pl) can be used to single
	out the first element of pl.
	-The null list is represented by the null pointer, i.e. NULL or 0.
	-Unless a copy operation is specified, nothing is ever copied.
	-Copy operations always copy names.
	-Depending on the copy operation, a list pointed to by an atom may or may
	not be copied (see the comments).
	-The operators provided here are not sufficient for fully general
	assembly and disassembly of lists:  In general it is necessary
	to manually manipulate the plNext member of the list structure.
*/

#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef WIN32 
#include <windows.h>
#endif /* WIN32 */

#include "tlplan.h"
#include "btree.h"
#include "formula.h"
#include "list.h"
#include "tlparse.h"
#include "tl_tab.h"
#include "util.h"
#include "zone.h"

/* NewList

Description:
	Allocate and fill in a single list element.
	A passed btree, formula or list is not copied.
*/

DECLSPEC LISTP NewList
(
	int nType,							/* atomic type */
	char *psName,						/* literal (string) value */
	...									/* other value (optional) */
)
{
	char acBuffer[40];					/* format buffer */
	LISTP pl;							/* list pointer */
	SYMBOLP psSymbol;
	va_list pArg;						/* argument pointer */

	ENTER("NewList",TRUE);
	pl=(LISTP)MemAlloc(sizeof(LIST));
	pl->nType=nType;
	if(psName)
	{
		if(nType==ATOM_STRING)
		{
			pl->psName=(char *)MemAlloc(strlen(psName)+1);
			DoEscapes(pl->psName,(unsigned char *)psName);
			pl->psName=StrAlloc(pl->psName);
		}
		else
			pl->psName=IdentAlloc(psName);
	}

	va_start(pArg,psName);
	switch(nType)
	{
		case ATOM_IDENT:
			if(psName)
			{
				SymbolLookup(pl->psName,&psSymbol);
				if(psSymbol)
				{
					pl->uValue=psSymbol->uValue;
					pl->nType=psSymbol->nType;
				}
			}
			break;
		case ATOM_SYMBOLINFOP:
			if(psName)
			{
				SymbolLookup(pl->psName,&psSymbol);
				if(psSymbol)
				{
					pl->uValue=psSymbol->uValue;
					pl->nType=psSymbol->nType;
				}
			}
			break;
		case ATOM_STRING:
			pl->uValue.psString=pl->psName;
			break;
		case ATOM_INTEGER:
			if(!psName)
			{
				sprintf(acBuffer,"%d",va_arg(pArg,int));
				pl->psName=IdentAlloc(acBuffer);
			}
			else
				pl->uValue.nInteger=atoi(psName);
			break;
		case ATOM_FLOAT:
			if(!psName)
			{
				sprintf(acBuffer,"%f",va_arg(pArg,double));
				pl->psName=IdentAlloc(acBuffer);
			}
			else
				pl->uValue.dfFloat=atof(psName);
			break;
		case ATOM_LISTP:
			pl->uValue.plList=va_arg(pArg,LISTP);
			break;
		case ATOM_BTREEP:
			pl->uValue.pbtTree=va_arg(pArg,BTREEP);
			break;
		case ATOM_FUNCTIONP:
			pl->uValue.pfFunction=va_arg(pArg,FUNCTIONP);
			break;
		case ATOM_INFINITY:
			break;
		default:
			ErrorMessage("NewList:  Unsupported atomic type %d\n",nType);
	}
	va_end(pArg);
	if(!pl->psName)
		ErrorMessage("NewList:  List element name is null pointer\n");
	EXIT("NewList");
	return pl;
}

/* AtomQ

Description:
	Determine if a list element is a true atom.
	(A list element is an atom if it is not type ATOM_LISTP.)
*/

//BOOL AtomQ
//(
//	LISTP plList
//)
//{
//	ENTER("AtomQ",TRUE);
//	EXIT("AtomQ");
//	return plList->nType!=ATOM_LISTP;
//}

/* CopyAtom

Description:
	Copy an atom (or first atom of a list).
Notes:
	The name (if there is one) is copied!
*/

LISTP CopyAtom
(
	LISTP plAtom						/* atom to copy */
)
{
	LISTP pl;							/* atom pointer */

	ENTER("CopyAtom",TRUE);
	if(!plAtom)
		pl=NULL;
	else
	{
		pl=(LISTP)MemAlloc(sizeof(LIST));
		*pl=*plAtom;					/* copy atom */
		pl->plNext=NULL;
		if(plAtom->psName)
		{
			if(plAtom->nType==ATOM_STRING)
				pl->uValue.psString=pl->psName=StrAlloc(plAtom->psName);
			else
				pl->uValue.psString=pl->psName=IdentAlloc(plAtom->psName);
		}
	}
	EXIT("CopyAtom");
	return pl;
}

/* CopyList

Description:
	Create a copy of an entire list.
Notes:
	All names are copied!
*/

LISTP CopyList
(
	LISTP plList						/* input list */
)
{
	LISTP plStart;						/* pointer to result */
	LISTP plEnd;						/* pointer end of result */
	LISTP plSource;						/* pointer to input list */

	ENTER("CopyList",TRUE);
	plStart=NULL;						/* initialize list head */
	plEnd=(LISTP)&plStart;

	for(plSource=plList;plSource;plSource=plSource->plNext)
	{
		plEnd=plEnd->plNext=CopyAtom(plSource);
		if(plEnd->nType==ATOM_LISTP)
			plEnd->uValue.plList=CopyList(plEnd->uValue.plList);
	}
	EXIT("CopyList");
	return plStart;
}

/* CopyListFirst

Description:
	Create a copy of the first element of an entire list.
Notes:
	All names are copied!
*/

//LISTP CopyListFirst
//(
//	LISTP plList						/* input list */
//)
//{
//	LISTP plStart;						/* pointer to result */
//	LISTP plEnd;						/* pointer end of result */
//
//	ENTER("CopyListFirst",TRUE);
//	plStart=NULL;						/* initialize list head */
//	plEnd=(LISTP)&plStart;
//
//	if(plList)
//	{
//		plEnd=plEnd->plNext=CopyAtom(plList);
//		if(plEnd->nType==ATOM_LISTP)
//			plEnd->uValue.plList=CopyList(plEnd->uValue.plList);
//	}
//	EXIT("CopyListFirst");
//	return plStart;
//}

/* FreeList

Description:
	Discard an entire list.
*/

//void FreeList
//(
//	LISTP plList						/* input list */
//)
//{
//	LISTP pl1,pl2;						/* pointers to list */
//
//	ENTER("FreeList",TRUE);
//	for(pl1=plList;pl1;pl1=pl2)
//	{
//		pl2=pl1->plNext;
//		if(pl1->nType==ATOM_LISTP)
//			FreeList(pl1->uValue.plList);
//		if(pl1->psName)
//			Free(pl1->psName);
//		Free(pl1);
//	}
//	EXIT("FreeList");
//}

/* MarkList

Description:
	Mark an entire list (for garbage collection).
*/

void MarkList
(
	LISTP plList						/* input list */
)
{
	LISTP pl;							/* pointer to list */

	ENTER("MarkList",FALSE);
	for(pl=plList;pl;pl=pl->plNext)
	{
		if(pl->nType==ATOM_LISTP)
			MarkList(pl->uValue.plList);
		ZoneMark(pl);
	}
	ENTER("MarkList",FALSE);
}

/* CopyCar

Description:
	Copy the first element of a list.
Notes:
	All names are copied!
*/

//LISTP CopyCar
//(
//	LISTP plList						/* input list */
//)
//{
//	LISTP pl;
//
//	ENTER("CopyCar",TRUE);
//	if(!plList)
//		pl=NULL;
//	else if(plList->nType==ATOM_LISTP)
//		pl=CopyList(plList->uValue.plList);
//	else
//		pl=CopyAtom(plList);
//	EXIT("CopyCar");
//	return pl;
//}

/* CopyCdr

Description:
	Copy the tail of a list.
Notes:
	All names are copied!
*/

//LISTP CopyCdr
//(
//	LISTP plList						/* input list */
//)
//{
//	LISTP plStart;						/* pointer to result */
//	LISTP plEnd;						/* pointer end of result */
//	LISTP pl;							/* pointer to input list */
//
//	ENTER("CopyCdr",TRUE);
//	plStart=NULL;						/* initialize list head */
//	plEnd=(LISTP)&plStart;
//	if(plList)
//	{
//		for(pl=plList->plNext;pl;pl=pl->plNext)
//		{
//			plEnd=plEnd->plNext=CopyAtom(pl);
//			if(plEnd->nType==ATOM_LISTP)
//				plEnd->uValue.plList=CopyList(plEnd->uValue.plList);
//		}
//	}
//	EXIT("CopyCdr");
//	return plStart;
//}

/* CopyCons

Description:
	Generate a list from copies of two lists.
Notes:
	All names are copied!
	If the left list is null, the right list is returned.
*/

//LISTP CopyCons
//(
//	LISTP plLeft,						/* input list */
//	LISTP plRight						/* input list */
//)
//{
//	LISTP pl;
//
//	ENTER("CopyCons",TRUE);
//	if(!plLeft)
//		pl=CopyList(plRight);
//	else
//	{
//		pl=CopyList(plLeft);
//		pl->plNext=CopyList(plRight);
//	}
//	EXIT("CopyCons");
//	return pl;
//}

/* Cons

Description:
	Generate a list from two lists or atoms, or a combination thereof.
Notes:
	A single list element "may" be generated by this procedure, to point to
	the left list...  Care must be taken to ensure that it is freed later.
*/

LISTP Cons
(
	LISTP plLeft,						/* input list */
	LISTP plRight						/* input list */
)
{
	LISTP pl;

	ENTER("Cons",TRUE);
	if(!plLeft)
		pl=plRight;
	else
	{
		if(plLeft->plNext)
		{
			pl=NewList(ATOM_LISTP,"",plLeft);
			pl->plNext=plRight;
		}
		else
		{
			plLeft->plNext=plRight;
			pl=plLeft;
		}
	}
	EXIT("Cons");
	return pl;
}

/* Nth

Description:
	Return a pointer to nth element of a list.  If the nth element points
	to a list, return that list instead.
	If the index is out of bounds or the list is empty, return the null pointer.
*/

//LISTP Nth
//(
//	int n,								/* input selector index */
//	LISTP plList						/* input list */
//)
//{
//	int i;								/* loop variable */
//	LISTP pl;							/* list pointer */
//
//	ENTER("Nth",TRUE);
//	for(i=0,pl=plList;i<=n&&pl;i++,pl=pl->plNext);
//	EXIT("Nth");
//	return pl->nType==ATOM_LISTP?pl->uValue.plList:pl;
//}

/* Length

Description:
	Count the number of top level elements in a list.
*/

//int Length
//(
//	LISTP plList
//)
//{
//	int i;
//	LISTP pl;
//
//	ENTER("Length",TRUE);
//	for(i=0,pl=plList;pl;i++,pl=pl->plNext);
//	EXIT("Length");
//	return i;
//}

/* List

Description:
	Create a list from the passed lists.
Notes:
	The argument list must be terminated with a NULL pointer.
*/

//LISTP List
//(
//	LISTP plArg1,						/* first argument */
//	...
//)
//{
//	LISTP plStart,plEnd,pl;
//	va_list pArg;						/* argument pointer */
//
//	ENTER("List",TRUE);
//	plStart=NULL;						/* initialize list head */
//	plEnd=(LISTP)&plStart;
//
//	va_start(pArg,plArg1);
//	for(pl=plArg1;pl;pl=va_arg(pArg,LISTP))
//		plEnd=plEnd->plNext=NewList(ATOM_LISTP,"",pl);
//	EXIT("List");
//	return(plStart);
//}

/* PrintList

Description:
	Print a list in lisp format.
*/

void PrintList
(
	FILE *pf,							/* output file stream */
	LISTP plList,						/* list to display */
	int nLevel							/* recursion level (initialize to 0) */
)
{
	LISTP pl;							/* list pointer */
	int nLength;						/* string length */
	int nCol;							/* current column */
	char acBuffer[64];					/* number buffer */
	char *ps;							/* buffer pointer */
	static char *psBlanks="                                                                                ";

	ENTER("PrintList",TRUE);
	nCol=2*nLevel;
	for(pl=plList;pl;pl=pl->plNext)
	{
		switch(pl->nType)
		{
			case ATOM_STRING:
				if(pl->psName)
				{
					nLength=strlen(pl->psName)+2;
					if(nCol>2*(nLevel+1)&&nCol+nLength>80)
					{
						CommandPrintf(pf,"\n%.*s",2*(nLevel+1),psBlanks);
						nCol=2*(nLevel+1);
					}
					CommandPrintf(pf,"\"%s\"",pl->psName);
					nCol+=nLength;
				}
				break;
			case ATOM_IDENT:
			case ATOM_FUNCTIONP:
			case ATOM_SYMBOLINFOP:
				if(pl->psName)
				{
					nLength=strlen(pl->psName);
					if(nCol>2*(nLevel+1)&&nCol+nLength>80)
					{
						CommandPrintf(pf,"\n%.*s",2*(nLevel+1),psBlanks);
						nCol=2*(nLevel+1);
					}
					CommandPrintf(pf,"%s",pl->psName);
					nCol+=nLength;
				}
				break;
			case ATOM_INTEGER:
				sprintf(acBuffer,"%d",pl->uValue.nInteger);
				nLength=strlen(acBuffer);
				if(nCol>2*(nLevel+1)&&nCol+nLength>80)
				{
					CommandPrintf(pf,"\n%.*s",2*(nLevel+1),psBlanks);
					nCol=2*(nLevel+1);
				}
				CommandPrintf(pf,"%s",acBuffer);
				nCol+=nLength;
				break;
			case ATOM_FLOAT:
				sprintf(acBuffer,"%f",pl->uValue.dfFloat);
				for(ps=acBuffer+strlen(acBuffer);ps>acBuffer&&*ps=='0';--ps);
				ps[1]=0;				/* delete trailing blanks */
				nLength=strlen(acBuffer);
				if(nCol>2*(nLevel+1)&&nCol+nLength>80)
				{
					CommandPrintf(pf,"\n%.*s",2*(nLevel+1),psBlanks);
					nCol=2*(nLevel+1);
				}
				CommandPrintf(pf,"%s",acBuffer);
				nCol+=nLength;
				break;
			case ATOM_LISTP:
				CommandPrintf(pf,"\n%.*s(",2*nLevel,psBlanks);
				PrintList(pf,pl->uValue.plList,nLevel+1);
				CommandPrintf(pf,")");
				break;
			case ATOM_FORMULAP:
				PrintFormula(pf,pl->uValue.pcFormula,nLevel+1);
				break;
			default:
				ErrorMessage("PrintList:  Unsupported atom type %d\n",pl->nType);
		}
		if(pl->plNext)
		{
			CommandPrintf(pf," ");
			nCol++;
		}
	}
	if(!nLevel)
		CommandPrintf(pf,"\n");
	EXIT("PrintList");
}

/* ListToString

Description:
	Convert a list to a string.
Notes:
	The result is unformatted... it contains no embedded tabs or newlines.
Returns:
	A pointer to the end of the string.
*/

//char *ListToString
//(
//	LISTP plList,						/* list to convert */
//	char *psBuffer						/* string buffer */
//)
//{
//	LISTP pl;							/* list pointer */
//
//	ENTER("ListToString",TRUE);
//	for(pl=plList;pl;pl=pl->plNext)
//	{
//		switch(pl->nType)
//		{
//			case ATOM_STRING:
//				sprintf(psBuffer,"\"%s\"",pl->psName);
//				psBuffer+=strlen(psBuffer);
//				break;
//			case ATOM_INTEGER:
//			case ATOM_FUNCTIONP:
//				sprintf(psBuffer,"%s",pl->psName);
//				psBuffer+=strlen(psBuffer);
//				break;
//			case ATOM_LISTP:
//				*psBuffer++='(';
//				psBuffer=ListToString(pl->uValue.plList,psBuffer);
//				*psBuffer++=')';
//				break;
//			default:
//				ErrorMessage("Unsupported atom type %d\n",pl->nType);
//		}
//		if(pl->plNext)
//			*psBuffer++=' ';
//	}
//	EXIT("ListToString");
//	return psBuffer;
//}

/* ListStringLength

Description:
	Count up the number of characters required to convert a list into a string.
*/

//int ListStringLength
//(
//	LISTP plList,						/* list to measure */
//	int nCount							/* string length, (initialize to 1) */
//)
//{
//	LISTP pl;							/* list pointer */
//
//	ENTER("ListStringLength",TRUE);
//	for(pl=plList;pl;pl=pl->plNext)
//	{
//		switch(pl->nType)
//		{
//			case ATOM_STRING:
//				nCount+=strlen(pl->psName)+2;
//				break;
//			case ATOM_INTEGER:
//			case ATOM_FUNCTIONP:
//				nCount+=strlen(pl->psName);
//				break;
//			case ATOM_LISTP:
//				nCount+=ListStringLength(pl->uValue.plList,3);
//				break;
//			default:
//				ErrorMessage("Unsupported atom type %d\n",pl->nType);
//		}
//		if(pl->plNext)
//			nCount++;
//	}
//	EXIT("ListStringLength");
//	return nCount;
//}

/* ListEqQ

Description:
	Return TRUE iff plList1 has the same structure and contents as plList2,
*/

//BOOL ListEqQ
//(
//	LISTP plList1,
//	LISTP plList2
//)
//{
//	LISTP pl1,pl2;						/* list pointers */
//
//	ENTER("ListEqQ",TRUE);
//	for
//	(
//		pl1=plList1,pl2=plList2;
//		pl1&&pl2;
//		pl1=pl1->plNext,pl2=pl2->plNext
//	)
//	{
//		if(pl1->nType!=pl2->nType)
//			return FALSE;
//		switch(pl1->nType)
//		{
//			case ATOM_STRING:
//			case ATOM_FUNCTIONP:
//				if(!StringEqQ(pl1->psName,pl2->psName))
//				{
//					EXIT("ListEqQ");
//					return FALSE;
//				}
//				break;
//			case ATOM_INTEGER:
//				if(pl1->uValue.nInteger!=pl2->uValue.nInteger)
//				{
//					EXIT("ListEqQ");
//					return FALSE;
//				}
//			case ATOM_BTREEP:
//				if(pl1->uValue.pbtTree!=pl2->uValue.pbtTree)
//				{
//					EXIT("ListEqQ");
//					return FALSE;
//				}
//			case ATOM_LISTP:
//				if(!ListEqQ(pl1->uValue.plList,pl2->uValue.plList))
//				{
//					EXIT("ListEqQ");
//					return FALSE;
//				}
//			default:
//				ErrorMessage("Unknown atomic type: %d\n",pl1->nType);
//		}
//	}
//	return !(pl1||pl2);					/* lists are equal if both are exhausted */
//}

/* FirstEqQ

Description:
	Return TRUE iff First(plList1) has the same structure and contents as
	First(plList2),
*/

//BOOL FirstEqQ
//(
//	LISTP plList1,
//	LISTP plList2
//)
//{
//	ENTER("FirstEqQ",TRUE);
//	if(!plList1&&!plList2)
//	{
//		EXIT("FirstEqQ");
//		return TRUE;
//	}
//	if(!plList1||!plList2)
//	{
//		EXIT("FirstEqQ");
//		return FALSE;
//	}
//	if(plList1->nType!=plList2->nType)
//	{
//		EXIT("FirstEqQ");
//		return FALSE;
//	}
//	switch(plList1->nType)
//	{
//		case ATOM_STRING:
//		case ATOM_FUNCTIONP:
//			if(!StringEqQ(plList1->psName,plList2->psName))
//			{
//				EXIT("FirstEqQ");
//				return FALSE;
//			}
//			break;
//		case ATOM_INTEGER:
//			if(plList1->uValue.nInteger!=plList2->uValue.nInteger)
//			{
//				EXIT("FirstEqQ");
//				return FALSE;
//			}
//			break;
//		case ATOM_BTREEP:
//			if(plList1->uValue.pbtTree!=plList2->uValue.pbtTree)
//			{
//				EXIT("FirstEqQ");
//				return FALSE;
//			}
//			break;
//		case ATOM_LISTP:
//			if(!ListEqQ(plList1->uValue.plList,plList2->uValue.plList))
//			{
//				EXIT("FirstEqQ");
//				return FALSE;
//			}
//			break;
//		default:
//			ErrorMessage("Unknown atomic type: %d\n",plList1->nType);
//	}
//	EXIT("FirstEqQ");
//	return TRUE;					/* lists are equal */
//}

/* Memq

Description:
	Check to see if a sublist is a member of another list.  In this implementation,
	we assume that the object of the search is either an atom or the root of
	a list, since we search with FirstEqQ.
*/

//LISTP Memq
//(
//	LISTP plObject,					/* object of search */
//	LISTP plList					/* list to search */
//)
//{
//	LISTP pl;
//
//	ENTER("Memq",TRUE);
//	for(pl=plList;pl;pl=pl->plNext)
//		if(FirstEqQ(plObject,pl))
//			break;
//	EXIT("Memq");
//	return pl;
//}

/* ZoneCopyAtom

Description:
	Copy an atom to the permanent zone.
*/

void ZoneCopyAtom
(
	LISTP plAtom						/* atom to Copy */
)
{
	ENTER("ZoneCopyAtom",TRUE);
	if(plAtom)
	{
		ZoneCopy(plAtom);
		if(plAtom->psName)
			ZoneCopy(plAtom->psName);
	}
	EXIT("ZoneCopyAtom");
}

/* ZoneCopyList

Description:
	Copy an entire list to the permanent zone.
*/

void ZoneCopyList
(
	LISTP plList						/* input list */
)
{
	LISTP pl,pl1;

	ENTER("ZoneCopyList",TRUE);

	for(pl=plList;pl;pl=pl1)
	{
		pl1=pl->plNext;					/* ZoneCopyAtom clobbers plNext */
		ZoneCopyAtom(pl);
		if(pl->nType==ATOM_LISTP)
			ZoneCopyList(pl->uValue.plList);
	}
	EXIT("ZoneCopyList");
}

/* ZoneRelocAtom

Description:
	Relocate an atom (or first atom of a list).
*/

void ZoneRelocAtom
(
	LISTP plAtom						/* atom to Reloc */
)
{
	ENTER("ZoneRelocAtom",TRUE);
	if(plAtom)
	{
		if(plAtom->plNext)
			ZoneReloc((void **)&plAtom->plNext);
		if(plAtom->psName)
			ZoneReloc((void **)&plAtom->psName);
	}
	EXIT("ZoneRelocAtom");
}

/* ZoneRelocList

Description:
	Create a ZoneReloc of an entire list.
Notes:
	All names are copied!
*/

void ZoneRelocList
(
	LISTP plList						/* input list */
)
{
	LISTP pl,pl1;						/* pointer to input list */

	ENTER("ZoneRelocList",TRUE);

	for(pl=plList;pl;pl=pl1)
	{
		pl1=pl->plNext;					/* ZoneRelocAtom clobbers plNext */
		ZoneRelocAtom(pl);
		if(pl->nType==ATOM_LISTP)
			ZoneRelocList(pl->uValue.plList);
	}
	EXIT("ZoneRelocList");
}

