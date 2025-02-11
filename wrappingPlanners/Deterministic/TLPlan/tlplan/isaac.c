/* Isaac.c -- cryptographic random number generator by Bob Jenkins 

For details, see:
	http://ourworld.compuserve.com/homepages/bob_jenkins/isaacafa.htm
*/

#include <stdio.h>
#include <stddef.h>

#include "tlplan.h"
#include "isaac.h"
#include "util.h"

#define ISAACSIZL (8)					/* I recommend 8 for crypto, 4 for simulations */
#define ISAACSIZE (1<<ISAACSIZL)

#define ind(mm,x) (*(unsigned int *)((unsigned char *)(mm)+((x)&((ISAACSIZE-1)<<2))))
#define rngstep(mix,a,b,mm,m,m2,r,x) \
{ \
	x=*m; \
	a=(a^(mix))+*(m2++); \
	*(m++)=y=ind(mm,x)+a+b; \
	*(r++)=b=ind(mm,y>>ISAACSIZL)+x; \
}
#define mix(a,b,c,d,e,f,g,h) \
{ \
	a^=b<<11;d+=a;b+=c; \
	b^=c>>2;e+=b;c+=d; \
	c^=d<<8;f+=c;d+=e; \
	d^=e>>16;g+=d;e+=f; \
	e^=f<<10;h+=e;f+=g; \
	f^=g>>4;a+=f;g+=h; \
	g^=h<<8;b+=g;h+=a; \
	h^=a>>9;c+=h;a+=b; \
}

typedef struct IsaacCtx					/* context of random number generator */
{
	unsigned int randcnt;
	unsigned int randrsl[ISAACSIZE];
	unsigned int randmem[ISAACSIZE];
	unsigned int randa;
	unsigned int randb;
	unsigned int randc;
}ISAACCTX, *ISAACCTXP;

/* local data */

static ISAACCTXP picContext;

/* IsaacRand

Description:
	Generate a random number.
	This routine uses the ISAAC algorithm.
*/

unsigned int IsaacRand(void)
{
	register unsigned int a,b,x,y,*m,*mm,*m2,*r,*mend;
	
	if(!picContext)
	{
		picContext=(ISAACCTXP)MemAlloc(sizeof(ISAACCTX));
		IsaacSeed(0);					/* default to a seed of 0 */
	}
	else
	{
		if(picContext->randcnt--)
			return picContext->randrsl[picContext->randcnt];
	}
	
	mm=picContext->randmem; 
	r=picContext->randrsl;
	a=picContext->randa;
	b=picContext->randb+(++picContext->randc);
	for(m=mm,mend=m2=m+(ISAACSIZE/2);m<mend;)
	{
		rngstep(a<<13,a,b,mm,m,m2,r,x);
		rngstep(a>>6,a,b,mm,m,m2,r,x);
		rngstep(a<<2,a,b,mm,m,m2,r,x);
		rngstep(a>>16,a,b,mm,m,m2,r,x);
	}
	for(m2=mm;m2<mend;)
	{
		rngstep(a<<13,a,b,mm,m,m2,r,x);
		rngstep(a>>6,a,b,mm,m,m2,r,x);
		rngstep(a<<2,a,b,mm,m,m2,r,x);
		rngstep(a>>16,a,b,mm,m,m2,r,x);
	}
	picContext->randb=b;
	picContext->randa=a;
	picContext->randcnt=ISAACSIZE-1;
	return picContext->randrsl[picContext->randcnt];
}

/* IsaacSeed

Description:
	Set the seed for the isaac random number generator.
*/

void IsaacSeed
(
	unsigned int uSeed
)
{
	int i;
	unsigned int a,b,c,d,e,f,g,h;
	unsigned int *m,*r;

	for(i=0;i<ISAACSIZE;i++)
		picContext->randrsl[i]=uSeed;

	picContext->randa=picContext->randb=picContext->randc=0;
	m=picContext->randmem;
	r=picContext->randrsl;
	a=b=c=d=e=f=g=h=0x9e3779b9;			/* the golden ratio */

	for(i=0;i<4;++i)					/* scramble it */
		mix(a,b,c,d,e,f,g,h);

	/* initialize using the contents of r[] as the seed */
		
	for(i=0;i<ISAACSIZE;i+=8)
	{
		a+=r[i];b+=r[i+1];c+=r[i+2];d+=r[i+3];
		e+=r[i+4];f+=r[i+5];g+=r[i+6];h+=r[i+7];
		mix(a,b,c,d,e,f,g,h);
		m[i]=a;m[i+1]=b;m[i+2]=c;m[i+3]=d;
		m[i+4]=e;m[i+5]=f;m[i+6]=g;m[i+7]=h;
	}
		
	/* do a second pass to make all of the seed affect all of m */
		
	for(i=0;i<ISAACSIZE;i+=8)
	{
		a+=m[i];b+=m[i+1];c+=m[i+2];d+=m[i+3];
		e+=m[i+4];f+=m[i+5];g+=m[i+6];h+=m[i+7];
		mix(a,b,c,d,e,f,g,h);
		m[i]=a;m[i+1]=b;m[i+2]=c;m[i+3]=d;
		m[i+4]=e;m[i+5]=f;m[i+6]=g;m[i+7]=h;
	}
	picContext->randcnt=0;
	IsaacRand();
}

/* output of first 512 values, given a seed of 0 (reversed):

f650e4c8e448e96d98db2fb4f5fad54f433f1afbedec154ad837048746ca4f9a
5de3743e88381097f1d444eb823cedb66a83e1e04a5f6355c744243325890e2e
7452e31957161df638a824f3002ed71329f5544951c08d83d78cb99ea0cc74f3
8f651659cbc8b7c2f5f71c6912ad6419e5792e1b860536b809b3ce98d45d6d81
f3b2612917e38f8529cf72ce349947b0c998f9ffb5e13dae32ae2a2bf7cf814c
8ebfa303cf22e0640b923200eca4d58aef53cec4d0f7b37d9c411a2affdf8a80
b40e27bcb4d2f97644b89b08f37c71d51a70e7e90bdb9c3060dc5207b3c3f24b
d7386806229749b54e232cd091dabc65a70e11018b87437e5781414fcdbc62e2
8107c9ff69d2e4ae3b18e752b143b6886f4e077295138769943c3c74afc17a97
0fd439636a529b0bd8c58a6aa8bcc22d2db35dfea7a2f4026cb167db538e1f4e
7275e2771d3b8e97ecc5dc9115e3a5b90369661430ab93ecac9fe69d7bc76811
60eda8da28833522d5295ebc5adb60e7f7e1cdd097166d14b67ec13a210f3925
64af0fef0d0286843aea3decb058bafbb8b0ccfcf2b5cc05e3a662d9814bc24c
2364a1aa37c0ed052b36505c451e7ec85d2a542fe43d0fbb91c8d92560d4d5f8
12a0594b9e8a51dacd49ebdb1b0dcdc1cd57c7f7e63444517ded386f2f36fa86
a6d1210133bc405db388d96cdb6dbe96fe29661c13edc0cbcb0eee4a70cc94ae
de11ed340606cf9f3a6ce38923d74f4ea37f63ff917bdec2d73f72d40e7e0e67
3d77d9a213add9228891b3db01a9bd7056a001e3d51f093dcc033ce35ad0d3b0
34105a8c6a123f57bd2e50247364944be89b1a3b21835c4d9f39e2d9d405ded8
294d37e5bccaaeed35a124b56708a2bcb00960ba2a98121a4d8fae820bb3263f
12595a196a1075890809e49421c171ec884d682514c8009bb0b84e7b03fb88f4
28e7cb789388b13bdd2dc1d5848f520a07c28cd168a3935872c9137d127dd430
c613f1578c2f0d55f7d3f39f309bfb788406b13746c0a6f53718d59708607f04
76904b6d04db4e13cd7411a7b510ce0ebfc7f7ccb83f957afdfef62dc35e4580
3ff1e5244112d96c02c9b944d5990dfbe7e265810d9c7e7e826dfa8966f1e0ab
30bcc764eadebeaced35e5ee0c571a7de4f3a26af7f58f7badf6bc235d023e65
1ed3ff4eec46b0b6d2a93b51e75b41c97e315aeb61119a5a53245b7933f6d7b1
cae8deba50fc8194afa92a6dc87c80064188bfcd8bace62e78ffa5685597ec0f
b4415f7d08294766ad56764309c36f903dde9f394a0a283c18080c8e080c79ec
79ae4c10cb9e15637cdd662f62d31911a4ca0cf15cf824cd3b708f991e16614c
b6b9d7665de87abb7229ea81d5b2d75056e6cd21fe1e42d596da2655c2b9aa36
b8f6fd4a6a158d1001913fd3af7d1fb80b5e435f90c107576554abda7a68710f <-start here!

82ac484fd7e1c7be95c85eaa94a302f44d3cfbda786b29081010b27582d53d12
21e2a51c3d1e9150b059261dd0638e1a31860f0581f2864dff4cfc350451516d
bd086f26bc5654c165dfa427a82427f5582e3014b8d2486dc79a17499a1d7745
8766bb541e04a7f73d3dff8ad5ec6bf4dbef7d9f36ec0ea31feb2e4f15cfcc5c
d8c423fbd0ef3cc9eb244925ba5590c8a5f48ac433c5321c613b67b2479c3a22
e21339cc10d210aa931dd7e2ef05ee06b82f2703a385cb2c5d67133c877eb7b4
1e3437f75afb43ae53c078f394d904811d96458908063a85e13222281956b1e5
31860f132e7b022f21182ca396f703ac46819e2e0d28fe523724d4dca0eabe6b
c66699fdc6112fdd19c1e69c04d3658a4b55dd9931907d62f854b5224d678f26
22ae0582eafed133e4a51d2184bd6dd6c1a513753f28ee63fb737b1a70a1660e
8a8dfaa31be79937f7476978513c1764531ac6bf12c06908001cdb951a4b6a53
d067fce512b2cfb69ddb477f740e006639ddf25acc8bfa2df1b20eaf64f2632c
9783cdee63bfd4d80084cfe575f4e9e219b48fd06c48ddd87a36af9371865c4c
9ce0199d867027d72cb7b77f84ef01da72f5972f040f7074df9afa29c921f94e
75c08a3618c1ef9ad649a428c5b719378a30738ad97cd348858129a6239e3b0a
bbb8abc480fac4c2ecfcf20bd9d711f9e2a4ef71b5fe87c0be8b06b2aafef5a7
9c15db3b0aeb81654389a84a253b1d7a19047c797cdc78a2d20adf0356f55a71
3e730fa8fd8650d8959e234eb7546681dad1b22a142a6e858ef4bce668235b9d
85a13f8574096ae7a949bea229322d0dd568385882846526403dae086dd1943a
e1279bff9e7e4f041c3a4524484525e481d4cc5fe24124c0037464c0bf1bd691
26ceb003275ead3ac5bde90826414ff3a30519add7b43abe2ce5d3d588412761
97ca2070e5fbb9c7276df0b4308f751f37a97df6c9cd808cfe4cb3803d469303
aee19096c0d5d42a4e823ad3f5f9cc3b4286619c9ca45e1c66c97340891aec49
45bae606c798f04752649d6cce86fdfc80c6e402d6ec2f2b27c822821fe26ce0
92f57ea7de462f4d07497cae5a48755c721502dd6cbe7935836d80039ead7f70
9ab3a42f4c8652d632e39273e8fa38601da4f25a0cd6ef8102503f7d8854a0a1
9a30c4e88815715305efe29457c4c9252887d96fc1a71e3ce9f841632d0985de
d21e796c6fb5ce5602614abfc3c7be2cb54fed6fa617a083c3142d8f6079e4ce
ceffc1471d0cb81bdc153e5fe36ef5bbd531161a165b10157aa114ed3f7579b3
f7f395f1bc6172c7a86f875e0e6c51b3cdfec2af73c0e762824c2009c5a87748
94d401258aba3ffbd32be0608c17eff021e2547e07cffad905340e15f3310c92
9d8d190886ba527ff943f672ef73fbf046d95ca5c54cd95b9d855e894bb5af29 <- then here!
*/
