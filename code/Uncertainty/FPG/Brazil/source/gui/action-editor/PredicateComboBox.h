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
 *  PredicateComboBox.h
 *  
 *
 *  Created by Owen Thomas on 26/04/06.
 *  
 *
 */

#ifndef predicate_combo_box
#define predicate_combo_box

using namespace std;

#include <set>

#include <QString>
#include <QComboBox>
#include <QFocusEvent>
#include <QRegExpValidator>

#include "../../model/DomainListener.h"
#include "../../model/impl/DOMDomain.h"

#include "BrazilComboBox.h"

/**
 * Allows selection and insertion of a Predicate from and into
 * a DOMDomain list of predicates.
 *
 * When the PredicateComboBox loses focus it emits a focusOut signal.
 * 
 */
class PredicateComboBox : public BrazilComboBox, public DomainListener
{
	Q_OBJECT
	
	private:
		DOMDomain* domain;
		
	protected slots:
		void addPredicate (const QString& predicateName)
		{
			DOMPredicate* predicate = new DOMPredicate (predicateName.toUtf8().data(), domain->getDOMElement()->getOwnerDocument ());
			this->domain->addPredicate (predicate);
		}
	public:
		PredicateComboBox (DOMDomain& domain, QWidget* parent = NULL) : BrazilComboBox (parent)
		{
			this->domain = &domain;
			domain.addListener (this);
			
			//populate combo box.
			set<DOMPredicate*> predicates = domain.getPredicates ();
			set<DOMPredicate*>::iterator it;
			for(it = predicates.begin (); it != predicates.end(); it++) {
				addItem ((*it)->getName());
			}
			
			QRegExp whiteSpace("(\\b([a-zA-Z0-9])+\\b(\\s+))+");
			QValidator* validator = new QRegExpValidator (whiteSpace,this);
						setMinimumWidth (fontMetrics().width ("XXXXXXXXXXXXXXXXXXXXXXX"));

			
			setEditable (true);
			setValidator (validator);
			
		}
		
		~PredicateComboBox ()
		{
			this->domain->removeListener (this);
		}
		
		/**
		 * Returns the predicate mapped to the currentText() property
		 * of this combo box. if no predicate exists in the domain, it
		 * returns NULL.
		 */
		virtual DOMPredicate* getCurrentPredicate () {
			return domain->getPredicate (currentText().toUtf8().data());
		}
		
		virtual void setCurrentPredicate (DOMPredicate* predicate) {
			setCurrentIndex  (findText(predicate->getName()));
		}
		
		virtual void predicateAdded (DOMPredicate* predicate)
		{
			//Add the predicate to my list of strings, if not
			//already present.
			if(-1 == findText (predicate->getName())) {
				addItem (predicate->getName());
			}
		}
		
		virtual void focusOutEvent (QFocusEvent* event)
		{
			if(currentText().trimmed().size() > 0 && -1 == findText (currentText ())) {
				addPredicate (currentText ());
			}
			BrazilComboBox::focusOutEvent (event);
		}
		
		
		//Additional listener methods that are not required.
		virtual void functionAdded (DOMFunction*) {}
		virtual void actionAdded (DOMAction*) { }
		virtual void actionRemoved (DOMAction*) { }
		virtual void nameChanged (const char*) { }
};

#endif

