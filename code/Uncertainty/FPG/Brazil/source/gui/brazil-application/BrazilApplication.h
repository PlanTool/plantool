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
 *  BrazilApplication.h
 *  
 *
 *  Created by Owen Thomas on 6/06/06.
 *  
 *
 */

#ifndef brazil_application
#define brazil_application

#include "ActionListWidget.h"
#include "InitialResultBar.h"
#include "PredicateListWidget.h"
#include "FunctionDefinitionWidget.h"

#include "../BrazilPlanningWrapper.h"

#include "../action-editor/ActionWidget.h"
#include "../action-editor/ActionWidgetFactory.h"

#include <QWidget>
#include <QTabWidget>
#include <QHBoxLayout>
#include <QMainWindow>
class BrazilApplication : public QWidget 
{
	Q_OBJECT
	
	private:
		QMainWindow* actionEditorWindow;
		DOMDomain* domain;
		ActionWidgetFactory* actionWidgetFactory;
		
		//Domain manipulation widgets, contained within a Tab Group.
		ActionListWidget* actionListWidget;
		PredicateListWidget* predicates;
		FunctionDefinitionWidget* functions;
		InternalConditionEditor* goal;
		
		DOMAction* openAction;
				
	protected slots:
	
			
		void openActionEditor (DOMAction*);
		void closeActionEditor (DOMAction*);
	

	public slots:
		
		void setDomain (DOMDomain& domain);
		
	public:
		
		//BrazilApplication (DOMDomain& domain, QWidget* parent = NULL);
		
		BrazilApplication (DOMDomain& domain, 
			QMainWindow* actionEditorWindow, 
			ActionWidgetFactory* actionWidgetFactory, QWidget* parent = NULL);
		
		virtual ~BrazilApplication ();
};

#endif

