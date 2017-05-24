// hashrelax.h

// global function prototypes

int RelCRCHashCreate(void);
int RelCRCHashClear(void);


typedef struct worldsum {
  struct worldsum *pNext;
  unsigned int worldCRC;
  int nGoalsSatisfied;
  int nPreferencesSatisfied;
  int nPreferenceValue;
  double dMetric;
} WORLDSUM, *WORLDSUMP;    // (Relaxed) World Summary

WORLDSUMP RelCRCHashInsert
(
    unsigned int worldCRC,
    int nGoalsSatisfied,
    int nPreferencesSatisfied,
    double dMetric
);

WORLDSUMP RelCRCHashInsert_Qual
(
   unsigned int worldCRC, 
    int nGoalsSatisfied,
    int nPreferenceValue
);

WORLDSUMP RelCRCHashSearch
(
    unsigned int worldCRC			/* CRC of world searched  */
);
//int RelCRCHashDelete
//(
//	unsigned int worldCRC			/* world pointer */
//);
int RelCRCHashCount(void);
void RelMarkCRCHash(void);

