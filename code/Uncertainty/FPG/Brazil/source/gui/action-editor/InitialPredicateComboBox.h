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
 *  InitialPredicateComboBox.h
 *  
 *
 *  Created by Owen Thomas on 4/08/06.
 *  
 *
 */

#ifndef initial_predicate_combo_box
#define initial_predicate_combo_box
#include "PredicateComboBox.h"
class InitialPredicateComboBox : public PredicateComboBox {

	Q_OBJECT
	
	private:
		bool initial;
		
	signals:
		void madeInitialSelection ();
		
	private slots:
		void initialSelectionChange () {
			if(initial && currentIndex () != 0) {
				removeItem (0);
				initial = false;
				QObject::disconnect (this, SIGNAL (currentIndexChanged(int)),
					this, SLOT (initialSelectionChange ()));
				emit madeInitialSelection ();
			}
		}
	public:
		
		InitialPredicateComboBox (DOMDomain& domain, QWidget* parent = NULL)
			:  PredicateComboBox (domain, parent) {
			
			this->initial = true;
			insertItem (0, "Select a condition..");
			setCurrentIndex (0);
			
			QObject::connect (this, SIGNAL (currentIndexChanged(int)),
				this, SLOT (initialSelectionChange ()));
		}
		
		virtual void focusOutEvent (QFocusEvent* event) {
			if(!initial) PredicateComboBox::focusOutEvent (event);
			if(currentText () != "Select a condition.." && currentText ().trimmed() != "") {
				QString newPredicate = currentText ();
				if(-1 == findText (newPredicate)) {
					addPredicate (currentText());
					removeItem (0);
					initial = false;
					setCurrentIndex (findText (newPredicate));
					emit madeInitialSelection ();
				}
			}
		}
};

#endif

