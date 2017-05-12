/* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is Brazil.
 *
 * The Initial Developer of the Original Code is
 * National ICT Australia.
 * Portions created by the Initial Developer are Copyright (C) 2007
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *	Douglas Aberdeen	(doug.aberdeen@gmail.com)
 *	Owen Thomas		(owen.thomas@nicta.com.au)
 *	Olivier Buffet		(olivier.buffet@loria.fr)
 *
 * ***** END LICENSE BLOCK ***** */
/*
 *  EffectSetEditor.h
 *  
 *
 *  Created by Owen Thomas on 4/12/06.
 *  
 *
 */

class DOMFunction;
class DOMPredicate;
class DOMAtomicEffect;
class AtomicEffectEditor;

#include <QWidget>
#include <QPushButton>
#include <QAction>

#include "../../model/EffectListener.h"
#include "../../model/impl/DOMEffectSet.h"

#include "../../model/impl/DOMDomain.h"

#include <map>
#include <set>

using namespace std;

class EffectSetEditor : public QWidget, public EffectListener, public DomainListener {
	
	Q_OBJECT
	
	private:
		DOMEffectSet* effectSet;
		map<DOMAtomicEffect*, AtomicEffectEditor*> editors;
		DOMDomain* domain;
		QPushButton* deleteButton;
		
		QAction* newFunctionAction;
		QAction* newPredicateAction;
	protected slots:
		
		void handleAtomicEffectSwitch (DOMAtomicEffect* newEffect,
			DOMAtomicEffect* oldEffect);
		
		void handleAtomicEffectSelection (bool selected);
		
		void handleDelete ();
		void handleNewFunctionEffect ();
		void handleNewPredicateEffect ();
		
	public:
		
		EffectSetEditor (DOMEffectSet& effectSet, DOMDomain& domain,
			QWidget* parent = NULL);
		
		
		~EffectSetEditor () { 
			//prevent dud pointers!
			effectSet->removeListener (this);
		}
		
		virtual QSize sizeHint () const {
			return minimumSize ();
		}
		
		//
		//Effect Listener impl.
		//
		
		virtual void effectAdded (DOMAtomicEffect* effect);
		
		virtual void effectRemoved (DOMAtomicEffect* effect);
		
		virtual void durationChanged () { }
		
		//
		//Domain Listener impl.
		//
		
		virtual void functionAdded (DOMFunction*) {
			newFunctionAction->setEnabled (true);
		}
		
		virtual void predicateAdded (DOMPredicate*) {
			newPredicateAction->setEnabled (true);
		}
		
		
		virtual void actionAdded (DOMAction*) { }
		virtual void actionRemoved (DOMAction*) { }
		virtual void nameChanged (const char*) { }
		
};
