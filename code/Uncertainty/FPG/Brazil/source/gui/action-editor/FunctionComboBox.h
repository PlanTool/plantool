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
 *  FunctionComboBox.h
 *  
 *
 *  Created by Owen Thomas on 28/11/06.
 *  
 *
 */

#include <QWidget>
#include <QString>

#include "BrazilComboBox.h"
#include "../../model/DomainListener.h"

class DOMDomain;
class DOMFunction;
class DOMAction;
class DOMPredicate;

class FunctionComboBox : public BrazilComboBox, DomainListener {

	Q_OBJECT
	
	private:
		DOMDomain* domain;
		
	protected slots:
		
		void addFunction (const QString& name);
	
		void handleIndexChanged (int index);
		
	signals:
		
		void currentFunctionChanged (DOMFunction*);
		
	public:
		
		FunctionComboBox (DOMDomain& domain, QWidget* parent = NULL);
		
		virtual void focusOutEvent (QFocusEvent* event);
		
		virtual DOMFunction* getCurrentFunction ();
		
		virtual void setCurrentFunction (DOMFunction* function);
		
		virtual void setDomain (DOMDomain& domain);
		
		//
		//Domain Listener
		//
		
		virtual void functionAdded (DOMFunction* function);
		
		virtual void predicateAdded (DOMPredicate* ) {}
		virtual void actionAdded (DOMAction* ) {}
		virtual void actionRemoved (DOMAction* ) {}
		virtual void nameChanged (const char* ) { }
	
};
