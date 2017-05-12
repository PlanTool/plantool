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
 *  ActionListWidget.h
 *  
 *
 *  Created by Owen Thomas on 6/06/06.
 *  
 *
 */

#ifndef action_list_widget
#define action_list_widget

#include <list>

#include <QObject>
#include <QWidget>
#include <QString>
#include <QPushButton>
#include <QLineEdit>
#include <QHBoxLayout>
#include <QVBoxLayout>
#include <QList>
#include <QListWidget>
#include <QListWidgetItem>
#include <QAction>
#include <QMenu>

#include "ActionListWidgetItem.h"

#include "../../model/impl/DOMDomain.h"
#include "../../model/impl/DOMAction.h"

#include "../../model/DomainListener.h"

#define ACTION_DURATION 36000

class DOMPredicate; 

class ActionListWidget : public QWidget, public DomainListener
{
	Q_OBJECT
	private:
		DOMDomain* domain;
		QListWidget* actionNames;
		QLineEdit* newActionName;
		QPushButton* newActionButton;
		
		list<QString> supportedNames;
		QString deleteName;
		
		QAction* deleteAction;
		
		QAction* sortAction;
		QMenu* listWidgetContextMenu;
		
	signals:
		void openActionEditor (DOMAction* action);
		void closeActionEditor (DOMAction* action);
	
	protected slots:
		void actionNameSelected (QListWidgetItem* actionItem);
		void newAction ();
		void sortActions ();
		void deleteSelected ();
		void showListWidgetContextMenu (const QPoint& pos);
		void listWidgetSelectionChanged ();
	
	public slots:
		
		void setDomain (DOMDomain& domain) {
			init (domain);
		}
	
		
	protected:
		void init (DOMDomain& domain);
		
		bool canExecuteDelete ();
		void executeDelete ();
		bool existingActionName (const QString& actionName);
				
	public:
		
		ActionListWidget (DOMDomain& domain, QWidget* parent = NULL);
		
		virtual ~ActionListWidget ()
		{
			this->domain->removeListener (this);
		}
		
		void execute (const QString& name);
		bool canExecute (const QString& name);
		list<QString> getSupportedNames ();
		
		//
		//Domain listener methods
		//
		virtual void actionAdded (DOMAction* action)
		{
			if(!action) return;
			const QString& name = action->getName();
			if(!(actionNames->findItems (name, Qt::MatchExactly)).size()) {
				/*QListWidgetItem* actionItem = */new ActionListWidgetItem (*action,
					action->getName (), actionNames);
			}
		}
		
		virtual void actionRemoved (DOMAction* action)
		{
			if(!action) return;
			
			QList<QListWidgetItem*> removedActions = 
				actionNames->findItems (action->getName(), Qt::MatchExactly);
				
			if(!removedActions.isEmpty()) {
				int deleteRow = actionNames->row (removedActions.front());
				(actionNames->takeItem (deleteRow));
			}
			
		}
		
		//
		//Non-implemented domain listener methods
		//
		virtual void predicateAdded (DOMPredicate*) { }
		virtual void functionAdded (DOMFunction*) { }
		virtual void nameChanged (const char*) { }
		
};

#endif

