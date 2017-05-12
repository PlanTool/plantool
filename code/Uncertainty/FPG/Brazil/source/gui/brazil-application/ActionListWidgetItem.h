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
 *  ActionListWidgetItem.h
 *  
 *
 *  Created by Owen Thomas on 9/06/06.
 *  
 *
 */

#include <QListWidgetItem>
#include <QListWidget>
#include <QString>


#include "../../model/ActionListener.h"
#include "../../model/impl/DOMAction.h"

class DOMProbabilistic;

class ActionListWidgetItem : public QListWidgetItem, public ActionListener
{
	private:
		DOMAction* action;
		
	public:
		
		ActionListWidgetItem (DOMAction& action, QListWidget* parent = NULL, 
			int type = QListWidgetItem::Type): QListWidgetItem (parent, type)
		{
			this->action = &action;
			this->action->addActionListener (this);
		}
		
		ActionListWidgetItem (DOMAction& action, const QString& text, 
			QListWidget* parent = NULL, int type = QListWidgetItem::Type) 
			: QListWidgetItem (text, parent, type)
		{
			this->action = &action;
			this->action->addActionListener (this);
		}
		
		virtual ~ActionListWidgetItem ()
		{
			this->action->removeActionListener (this);
		}
		
		DOMAction* getAction ()
		{
			return action;
		}
		
		//
		//ActionListener methods
		//
		
		virtual void nameChanged ()
		{
			setText (action->getName());
		}
		
		virtual void backgroundColourChanged() { }
		virtual void probabilisticAdded (DOMProbabilistic*) { }
		virtual void probabilisticRemoved (DOMProbabilistic*) { }
		
};

