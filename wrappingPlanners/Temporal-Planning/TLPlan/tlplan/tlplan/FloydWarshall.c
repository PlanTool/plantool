/* FloydWarshall

Description:
	Takes an nxn matrix C of edge costs and produces
	an nxn matrix A of lengths of shortest paths and
	an nxn matrix P giving a point in the middle of each shortest path.
*/

void FloydWarshall
(
	int n,								// array dimension
	double C[n][n],						// input edge-cost array
	double A[n][n],						// output total cost array
	int P[n][n],						// output path array
	int P1[n][n]						// output path array
)
{
	int i,j,k;

	for(i=0;i<n;i++)
	{
		for(j=0;j<n;j++)
		{
			A[i][j]=C[i][j];
			P[i][j]=-1;
			P1[i][j]=-1;
		}
	}
	for(i=0;i<n;i++)
		A[i][i]=0;						// no self cycle
	for(k=0;k<n;k++)
	{
		for(i=0;i<n;i++)
		{
			for(j=0;j<n;j++)
			{
				if(A[i][k]+A[k][j]<A[i][j])
				{
					A[i][j]=A[i][k]+A[k][j];
					P[i][j]=k;			// k is included in the shortest path
					P1[i][j]=P1[k][j];	// k is a predecessor
				}
			}
		}
	}
}

/* PrintPath1

Description:
	Print out the shortest path, using P1 array.
*/

void PrintPath1
(
	int i,								// source vertex index
	int j								// destination vertex index
)
{
	int k;
			
	if(i==j)							// must be start of path
		printf("%d ",i);
	else if(P1[i][j]==-1)				// no path
		printf("No path from %d to %d exists",i,j);
	else
	{
		PrintPath1(i,P1[i][j]);
		printf("%d ",j);
	}
}

