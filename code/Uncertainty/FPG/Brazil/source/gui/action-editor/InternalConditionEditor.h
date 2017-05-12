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
 *  InternalConditionEditor.h
 *  
 *
 *  Created by Owen Thomas on 29/11/06.
 *  
 *
 */

#ifndef internal_condition_editor
#define internal_condition_editor

#include "BrazilComboBox.h"

#include "ConditionEditor.h"
#include "../../model/impl/DOMConditionVisitor.h"
#include "../../model/InternalConditionListener.h"

#include <set>
#include <map>

#include <QFrame>
#include <QWidget>
#include <QLayout>
#include <QSize>
#include <QLabel>

class DOMDomain;
class EmptyInternalConditionEditorWidget;

class InternalConditionEditor : public ConditionEditor, public DOMConditionVisitor, 
	public InternalConditionListener {

	Q_OBJECT
	
	private:
	
		map<DOMCondition*, ConditionEditor*> mapOfChildren; //the value set = children.
		
		QWidget* operatorBar; //Bar at the top of the editor
		
		//The operator of the condition, contained within
		//operator bar.
		BrazilComboBox* operatorComboBox; 
		
		//widget that displays each child condition editor.
		QFrame* childEditorsFrame;
				
		//The last condition editor to make a request of this
		//internal condition through one of the requestXXX
		//methods..
		ConditionEditor* currentRequester;
		
		EmptyInternalConditionEditorWidget* emptyEditorWidget;
		
		//called by visitor methods during construction.
		void addChildConditionEditor (ConditionEditor* child);
		
		
	protected slots:
		
		//These are requests made on this internal condition editor
		//by child condition editors.
		virtual void handleDeleteCondition (DOMCondition*);
		
		virtual void handleNewInternalCondition (ConditionEditor* requester);
		virtual void handleNewFunctionCondition (ConditionEditor* requester);
		virtual void handleNewPredicateCondition (ConditionEditor* requester);
		
		//request made by operator combo box.
		virtual void handleOperatorComboBoxSelection (int index);
		
	protected:
		
		//
		//Condition Visitor, used to build up this widget with child
		//condition editors.
		//
		virtual bool visitInternal (DOMInternalCondition* condition);
		
		virtual bool visitPredicate (DOMPredicateCondition* condition);
		
		virtual bool visitFunction (DOMFunctionCondition* condition);
		
		//
		//ConditionListener methods.
		//
			
		virtual void conditionAdded (DOMCondition* condition);
		
		virtual void conditionRemoved (DOMCondition* condition);
	
		virtual void setNegated (bool);
		
	public:
		
		InternalConditionEditor (DOMInternalCondition& condition, 
			DOMDomain& domain,
			QWidget* parent = NULL);
			
		virtual void setOperatorBarText (const QString& text);
		
		virtual void setCondition (DOMInternalCondition& condition,
			DOMDomain& domain);
			
		virtual QSize sizeHint () const { return minimumSize(); }
		
		virtual QSize minimumSizeHint () const {
			return minimumSize ();
		}

};

#endif
