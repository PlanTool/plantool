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
 *  FunctionConditionEditor.h
 *  
 *
 *  Created by Owen Thomas on 30/11/06.
 *  
 *
 */
#include "ConditionEditor.h"

class FunctionComboBox;
class FunctionExpressionLineEdit;
class QComboBox;
class DOMFunctionExpression;
class DOMFunctionCondition;
class DOMFunction;
class FunctionConditionEditor : public ConditionEditor {
	
	Q_OBJECT
	
	private:
		
		FunctionComboBox* functionComboBox;
		FunctionExpressionLineEdit* functionExpressionLineEdit;
		QComboBox* operatorComboBox;
	
	protected slots:
		//respond to changes in the function combo box.
		void handleFunctionChanged (DOMFunction* function);
		
		//respond to changes in the operator combo box index.
		void handleIndexChanged (int index);
		
		//respond to changes in the expression line edit.
		void handleExpressionChanged (DOMFunctionExpression* newExpression);
		
		
	public:
		
		FunctionConditionEditor (DOMFunctionCondition& condition,
			DOMDomain& domain,
			FunctionComboBox* functionComboBox,
			FunctionExpressionLineEdit* functionExpressionLineEdit,
			QWidget* parent = NULL);
			
		QSize sizeHint () const {
			return minimumSize ();
		}
};
