
#ifndef dom_action
#define dom_action
#include "../ActionListener.h"
#include "DOMDomain.h"
#include "DOMEffectSet.h"
#include "DOMRootCondition.h"
#include "DOMProbabilistic.h"

class DOMAction : public DOMWrapper {

	private:
		char* typeName;
		DOMDomain* domain;
		list<DOMProbabilistic*> probabilisticChildren;
		DOMEffectSet* effect;
		DOMEffectSet* atStartEffect;
		DOMRootCondition* precondition;
		DOMRootCondition* overallCondition;
		list<ActionListener*> actionListeners;
		
	protected:
		
		/**
		 * Methods used in the construction process.
		 */
		
		DOMRootCondition* createPrecondition ();
		DOMRootCondition* createOverallCondition ();
		DOMEffectSet* createEffect (XMLCh* effectName);
		void createProbabilisticChildren ();
		
	public:


		/** 
		 * [daa] Shortcut types for lists over DOMConditions. Used by planner.
                 */
		typedef list<DOMAction*> ActionList;
		typedef ActionList::iterator ActionIt;
		typedef ActionList::const_iterator ActionCIt;
	
		/**
		 * Create a new DOMAction instance as a member of domain,
		 * from element.
		 *
		 */
		DOMAction (DOMElement& element, DOMDomain& domain);
		
		/**
		 * Create a new DOMAction instance named name, with the deterministic
		 * effect set effect and as a member of domain. This will create a new
		 * dom element to store the data for this dom action.
		 */
		DOMAction (char* name, DOMDomain& domain);
		
		virtual ~DOMAction ();
		
		/**
		 * Return this action's domain.
		 */
		virtual DOMDomain* getDomain ();
		
		/**
		 * Return all the probabilistic children. This is an immutable
		 * list. Use the add / remove methods of this class
		 * to modify the probabilistic children of this DOMAction.
		 */
		virtual  list<DOMProbabilistic*>& getProbabilisticChildren ();
		
		virtual void addProbabilistic (DOMProbabilistic* probabilistic);
		
		virtual void removeProbabilistic (DOMProbabilistic* probabilistic);
		
		/**
		 * Return the deterministic effects of this DOMAction.
		 * This will never return NULL, every DOMAction has a (possibly empty)
		 * effect set.
		 */ 
		virtual DOMEffectSet* getEffectSet ();

		/**
		 * Return the deterministic effects of *starting* this action
		 * This will never return NULL, every DOMAction has a (possibly empty)
		 * effect set.
		 */ 
		virtual DOMEffectSet* getAtStartEffectSet ();
		
		/**
		 * Return the Precondition of this DOMAction. Every action has a 
		 * (possibly empty) precondition, this method will not return NULL.
		 */
		virtual DOMRootCondition* getPrecondition ();
		
		/**
		 * Return the overall condition of this DOMAction. Every action has a 
		 * (possibly empty) precondition, this method will not return NULL.
		 */
		
		virtual DOMRootCondition* getOverallCondition ();
		
		virtual void addActionListener (ActionListener* listener);
		
		virtual void removeActionListener (ActionListener* listener);
		
		virtual char* getBackgroundColour ();
		
		virtual void setBackgroundColour (char* colour);
		
		virtual char* getName ();
		
		virtual void setName (char* name);
		
		virtual char* getTypeName ();
};

#endif
