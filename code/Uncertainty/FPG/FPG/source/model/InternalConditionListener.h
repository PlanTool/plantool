/*
 *  InternalConditionListener.h
 *  
 *
 *  Created by Owen Thomas on 10/05/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */
#include "impl/DOMConditionBase.h"

#ifndef internal_condition_listener
#define internal_condition_listener
class ConditionListener
{
	public:
		virtual ~ConditionListener () { }
		virtual void setNegated (bool value) = 0;
};
class InternalConditionListener : public ConditionListener
{
	public:
		virtual ~InternalConditionListener () { }
		virtual void conditionAdded (DOMCondition*) = 0;
		virtual void conditionRemoved (DOMCondition*) = 0;
};

#endif
