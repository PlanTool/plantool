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
 *  PredicateList.h
 *  
 *
 *  Created by Owen Thomas on 10/08/06.
 *  
 *
 */

#include "../../model/impl/DOMDomain.h"
#include "../../model/impl/DOMPredicate.h"
#include "../../model/DomainListener.h"

#include <QString>
#include <QWidget>
#include <QListView>
#include <QLineEdit>
#include <QPushButton>
#include <QHBoxLayout>
#include <QVBoxLayout>
#include <QScrollArea>

#include "PredicateListEditor.h"

/**
 * Presents an editable list of Predicates within a Domain. 
 * Allows new Predicates to be defined and predicates added / removed
 * from the initially valid set.
 *
 * To-Do: Allow deletion of existing predicates.
 */
class PredicateListWidget : public QWidget, public DomainListener {

	Q_OBJECT
	
	private:
		
		DOMProblem* problem;
		//QListView* listView;
		QScrollArea* editors;
		
		QLineEdit* editorLineEdit;
		QLayout* editorLayout;
		QWidget* editorsWidget;
	protected slots:
		
		/**
		 * Create a new predicate and add it to the problem's domain.
		 * If a predicate already exists with the specified name then
		 * highlight the line edit text.
		 */
		void newPredicate () {
			const QString& predicateName = editorLineEdit->displayText ().trimmed();
			if(!predicateName.size()) return;
			const char* name = predicateName.toUtf8().data();
			DOMPredicate* existingPredicate = 
				problem->getDomain()->getPredicate (name);
			if(existingPredicate) {
				editorLineEdit->selectAll ();
			} else {
				new DOMPredicate (predicateName.toUtf8().data(),
					problem->getDomain ());
					
				editorLineEdit->clear ();
			}
		}
		
	protected:
		virtual void init (DOMProblem& problem);
	public:
	
		PredicateListWidget (DOMProblem& problem, QWidget* parent = NULL);
		
		virtual ~PredicateListWidget ();
		
		virtual void setProblem (DOMProblem& problem);
	
		//
		//Domain listener methods.
		//
		
		virtual void predicateAdded (DOMPredicate* predicate);
		
		
		virtual void functionAdded (DOMFunction*) {}
		virtual void actionAdded (DOMAction*) { }
		virtual void actionRemoved (DOMAction*) { }
		virtual void nameChanged (const char* ){ }
	
};

