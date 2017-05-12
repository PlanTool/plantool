/*
 *  DOMDomain.h
 *  
 *
 *  Created by Owen Thomas on 9/05/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */
#ifndef dom_domain
#define dom_domain

#include <list>
#include <map>
#include <set>
#include <xercesc/dom/DOM.hpp>

#include "DOMPredicate.h"

#include "DOMWrapper.h"
#include "DOMAction.h"
#include "DOMFunction.h"
#include "DOMPropertySet.h"

class DOMProblem;

#include "DOMProbabilistic.h"
#include "../DomainListener.h"

/**
 * A Brazil Domain: A list of predicates and a list of actions.
 */
class DOMDomain : public DOMWrapper 
{
        public:
                /**
                 * Convencience type for accessing <name, function> maps
		 */
                typedef map<const char*, DOMFunction*, cmp_str> FunctionMap;
		typedef FunctionMap::iterator FunctionMapIt;
		typedef FunctionMap::const_iterator FunctionMapCIt;

	private:
		DOMPropertySet* propertySet;

		
		map<const char*, DOMPredicate*, cmp_str> predicates;
		set<DOMPredicate*> predicateSet;
		
		FunctionMap functions;
		set<DOMFunction*> functionSet;
		
		list<DOMAction*> actions;
		
		list<DomainListener*> listeners;
		
		//DOMElements pointing to <predicates>, <functions> and <actions>
		//respectivly.
		DOMElement* predicatesElement;
		DOMElement* functionsElement;
		DOMElement* actionsElement;
		
		DOMProblem* problem;



		//Internal methods for building up the list of predicates
		//and actions.
		void createPredicates ();
		void createFunctions ();
		void createActions ();
		void createProblem ();
		void createPropertySet ();

	public:
		
		/**
		 * Construct a DOMDomain from specified DOMElement.
		 * A Domain is assumed to have a list of <predicate> elements
		 * that can be understood by DOMPredicate and a list of <action>
		 * elements that can be understood by DOMAction.
		 *
		 * The list of predicates are contained under a child <predicates>
		 * element and preceed the list of actions, contained under a child
		 * <actions> element.
		 */
		DOMDomain (DOMElement* element);
		
		virtual ~DOMDomain ();

		/**
		 * [daa] Shortcut types used in planner
		 */
		typedef std::list<DOMAction*> ActionList;
		typedef ActionList::iterator ActionIt;
		typedef ActionList::const_iterator ActionCIt;
		

		DOMPredicate* getPredicate (const char* name)
		{
			if(predicates.find (name) == predicates.end()) {
				return NULL;
			}
			
			else {
				return predicates [name];
			}
		}
		
		/**
		 * Returns a shallow copy of this domain's predicate set.
		 * Manipulation of the returned set will not affect this
		 * domain's set of predicates.
		 */
		set<DOMPredicate*>& getPredicates () 
		{
			return predicateSet;
		}
		void addPredicate (DOMPredicate* predicate);

		list<DOMAction*>& getActions();
				
		void addFunction (DOMFunction* function);
		
		/**
		 * Return the function with name name.
		 *
		 * @param name, the name of the DOMFunction to
		 * return.
		 *
		 * @returns a Resource with name name.
		 */
		DOMFunction* getFunction (const char* name)
		{
			if(functions.find(name) == functions.end()) {
				return NULL;
			}
			return functions [name];
		}
		
		/**
		 * Return all DOMFunction elements in this Domain.
		 *
		 */	
		set<DOMFunction*>& getFunctions()
		{
			return (functionSet);
		}
		
		
		/**
                 * Return all DOMFunction elements in their map form.
		 * Used by StatsWidget
		 * @author daa
		 */
		FunctionMap& getFunctionMap()
		{
		        return functions;
		}

		/**
		 * Adds the specified action to the set of Actions
		 * in the domain. 
		 *
		 * This has no effect if action is already in the
		 * set.
		 *
		 * @param action, the DOMAction to add to the set
		 * of Actions.
		 *
		 */
		void addAction (DOMAction* action);
		
		/**
		 * Removes the specified action from the set
		 * of actions in the domain.
		 *
		 * This has no effect if action is not present in the collection.
		 *
		 * action is not deleted by this method.
		 *
		 * @param action, the DOMAction to remove.
		 *
		 */
		void removeAction (DOMAction* action);
		
		DOMProblem* getProblem () {
			return problem;
		}
				/**
		 * Returns the name of this Domain.
		 */
		char* getName ();
		
		/**
		 * Sets the name of this domain.
		 */ 
		void setName (const char* name);
		
		/**
		 * If not already present, add listener to the set of 
		 * listeners for this Domain. The specified listener 
		 * will be notified of changes to this domain.
		 *
		 * @param listener, an instance of DomainListener to 
		 * be notified of modifications to this domain.
		 */
		void addListener (DomainListener* listener)
		{
			
			list<DomainListener*>::iterator it;
			for(it = listeners.begin (); it != listeners.end(); it++) {
				if(*it == listener) break;
			}
			if(it == listeners.end()) {
				listeners.push_back(listener);
			}
		}
		
		/**
		 * Removes listener from the set of listeners within this 
		 * domain. 
		 *
		 * If listener is not a listener for this domain,
		 * this method does nothing.
		 * 
		 * @param listener, the DomainListener to remove from the
		 * set of listeners.
		 */
		void removeListener (DomainListener* listener)
		{
			list<DomainListener*>::iterator it;
			for(it = listeners.begin (); it != listeners.end(); it++) {
				if(*it == listener) break;
			}
			if(it != listeners.end()) {
				listeners.erase(it);
			}
		}

		
		/**
		 * Get a pointer to the set of available properties for this domain.
		 */
		DOMPropertySet* getProperties() { return propertySet; }
};
#endif
