/* save.h */

#ifndef __SAVE_H
#define __SAVE_H

void MarkDomainData(void);
void MarkPlanList
(
	LINEARPLANP plpList				/* list of plans */
);
void MarkParentPlans
(
	LINEARPLANP plpPlan				/* (current) plan */
);
void MarkBindings
(
	BINDINGP pbBindings				/* list of bindings */
);
void CollectGarbage(void);


#endif /* __SAVE_H */
