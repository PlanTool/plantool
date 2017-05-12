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
 *  PredicateListEditor.h
 *  
 *
 *  Created by Owen Thomas on 15/08/06.
 *  
 *
 */

#include <QWidget>
#include <QLabel>
#include <QCheckBox>
#include <QHBoxLayout>

#include "../../model/impl/DOMProblem.h"
#include "../../model/impl/DOMPredicate.h"

/**
 * Displays a Predicate within a list of predicates and allows
 * it's initially valid status to be toggled on/off.
 */
class PredicateListEditor : public QWidget {
	Q_OBJECT
	
	protected slots:
		
		void setInitial (bool initial) {
			problem->setInitial (predicate, initial);
		}
	
	private:
		DOMPredicate* predicate;
		DOMProblem* problem;
		
		QCheckBox* checkBox;
		QLabel* label;
		
	public:
		
		/**
		 * Construct a PredicateListEditor as a child widget
		 * of parent, operating over problem and toggling
		 * predicate's initially valid status.
		 *
		 * predicate must be a member of problem's domain's
		 * list of predicates.
		 */
		PredicateListEditor (QWidget* parent, DOMProblem* problem, 
			DOMPredicate* predicate);
			
							
		virtual QSize minimumSize () const {
			return layout()->minimumSize();
		}
		
		virtual int minimumHeight () const {
			return layout()->minimumSize().height();
		}};

