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
 *  FunctionListWidget.h
 *  
 *
 *  Created by Owen Thomas on 24/11/06.
 *  
 *
 */

#include <QWidget>


#include "../../model/DomainListener.h"
#include <QLayout>

using namespace std;
#include <iostream>

class DOMProblem;
#include "../../model/impl/DOMFunction.h"

struct ltFunction {

	bool operator() (DOMFunction* a, DOMFunction* b) {

		return strcmp(a->getName (),b->getName()) < 0;
	}
};

class FunctionListWidget : public QWidget, public DomainListener {

	private:
		
		DOMProblem* problem;
		
		void init (DOMProblem* problem);
		
	public:
		
		FunctionListWidget (DOMProblem& problem, QWidget* parent = NULL);
		
		virtual void setProblem (DOMProblem& problem);
		
	
		virtual void functionAdded (DOMFunction*);
		
		virtual void predicateAdded (DOMPredicate*) { }
		virtual void actionAdded (DOMAction*) { }
		virtual void actionRemoved (DOMAction*) { }
		virtual void nameChanged (const char*) { }

};
