/* bfs

Description:
	Breadth first search generator for a generic graph implemented
	using an "edge" binary described predicate.
Notes:
	This generator enumerates the vertices of the graph in order of
	distance from the starting point.  The starting vertex is not enumerated.
*/

void bfs
(
	struct edge *peArray,				// edge array
	int nEdges,							// number of edges
	char *psStart,						// starting vertex name
	char *ppsDestination,				// returned vertex name
	double *pdfDistance					// returned distance
)
{
	int i;								// vertex index (allocation index)
	int *pnVertexIndices;				// vertex index array (name sorted order)
	char **ppsVertexNames;				// vertex name array (allocation order)
	int *pnDistance;					// distance array (allocation order)
	int *pnQueue;						// queue array (breadth first order)
	int *pnNext;						// pointer to next available element in queue array
	int *pnFree;						// pointer to next free element location in queue array
	int nCount;							// maximum array size required
	int nIndex;							// vertex index (allocation order)
	int nVertex;						// vertex index (name sorted order)
	int nVertices,						// number of vertices
	char *psName;						// name of adjacent vertex

	nCount=nEdges+1;					// at most nEdges+1 vertices
	nIndex=0;							// initial array index
	nVertices=0;						// number of vertices

	// initialize arrays

	pnVertexIndices=(int *)malloc(sizeof(int)*nCount);
	ppsVertexNames=(char *)malloc(sizeof(char *)*nCount);
	pnDistance=(int *)malloc(sizeof(int)*nCount);
	pnQueue=(int *)malloc(sizeof(int)*nCount);
	pnNext=pnQueue-1;
	pnFree=pnQueue;

	// mark the start vertex

	nVertex=InsertVertex(psStart,pnVertexIndices,ppsVertexNames,&nVertices);	// add vertex to vertex array
	nIndex=pnVertexIndices[nVertex];	// get allocation index
	pnDistance[nIndex]=0;				// starting distance
	*pnFree++=nIndex;					// put vertex on the open list
	
	// mark the remaining vertices

	while(pnNext<pnFree)				// search all open vertices
	{
		i=*++pnNext;				// fetch next vertex from open list
;;; "generate" this vertex here
		while((psName=Adjacent(ppsNames[i],peArray))!=0)	// for all adjacent vertices
		{
			nVertex=InsertVertex(psName,pvArray,&nVertices);	// add vertex to vertex array
			if(nVertex==nVertices-1)	// if new vertex
			{
				nIndex=pnVertexIndices[nVertex];	// get allocation index
				pnDistance[nIndex]=pnDistance[i]+1;	// distance from start
				*pnFree++=nIndex;		// put vertex on the open list
			}
		}
	}
}

/* InsertVertex

Description:
	If the new vertex isn't already in the array, it is added.
Returns:
	The index of the vertex in the pnVertexIndices array
Notes:
	This routine uses binary search to maintain a sorted array of unique vertices.
	The array is assumed to be large enough to accept another vertex.
	Uses binary search to find the vertex location, and memmove to shift.
*/

static int InsertVertex
(
	char *psName,						// name of vertex to insert
	int *pnVertexIndices,				// vertex index array (name sorted order)
	int *ppsVertexNames,				// vertex name array (allocation order)
	int *pnCount						// number of vertices in array
)
{
	int nLeft;							// index of lower bound (inclusive)
	int nRight;							// index of upper bound (exclusive)
	int nMiddle;						// index of middle
	int nFlag;							// comparison value

	// search for vertex in array

	nLeft=0;
	nRight=*pnCount;
	while(nLeft<nRight)
	{
		nMiddle=(nLeft+nRight)/2;
		nFlag=strcmp(psName,ppsVertexNames[pnVertexIndices[nMiddle]]);
		if(!nFlag)					// match
			return nMiddle;
		if(nFlag<0)					// less than
			nRight=nMiddle;
		else						// greater than
			nLeft=nMiddle+1;
	}

	// vertex not found, insert new one

	memmove(pnVertexIndices+left+1,pnVertexIndices+left,(*pnCount-left)*sizeof(int));
	ppsVertexNames[*pnCount]=psName;
	pnVertexIndices[left]=*pnCount++;
	return nLeft;
}
