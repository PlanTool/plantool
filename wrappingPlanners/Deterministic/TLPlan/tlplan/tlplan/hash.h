#ifndef __HASH_H
#define __HASH_H

/* hash.h */

/* global function prototypes */

int HashCreate
(
	HASHTABP phtOld,					/* hash table structure */
	int nLimit,							/* number of entries needed */
	int nRecLen							/* record length */
);
void HashClear
(
	HASHTABP pht						/* hash table structure */
);
int HashInsert
(
	HASHTABP pht,						/* hash table structure */
	char *psKey,						/* pointer to key string */
	void **ppvRec						/* pointer to returned record pointer */
);
int HashSearch
(
	HASHTABP pht,						/* hash table structure */
	char *psKey,						/* pointer to key string */
	void **ppvRec						/* pointer to returned record pointer */
);
int HashDelete
(
	HASHTABP pht,						/* hash table structure */
	char *psKey							/* pointer to key string */
);
int HashCount
(
	HASHTABP pht						/* hash table structure */
);
void HashScan
(
	HASHTABP pht,						/* hash table structure */
	void (*pfFunc)(void *)				/* function to execute */
);
void HashError
(
	FILE *pf,							/* output file stream */
	int nCode							/* error code */
);

#endif /* __HASH_H */
