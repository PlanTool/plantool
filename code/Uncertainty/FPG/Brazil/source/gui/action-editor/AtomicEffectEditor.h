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
 *  AtomicEffectEditor.h
 *  
 *
 *  Created by Owen Thomas on 4/12/06.
 *  
 *
 */

//Internal class for defining Predicate and Function effects.
class PredicateEditor;
class FunctionEditor;

class DOMAtomicEffect;
class DOMPredicateEffect;
class DOMFunctionEffect;
class DOMDomain;

#include <QComboBox>
#include <QWidget>
#include <QStackedLayout>

#include "../../model/impl/DOMAtomicEffectVisitor.h"
#include <iostream>
using namespace std;

class AtomicEffectEditor : public QWidget, protected DOMAtomicEffectVisitor {

	Q_OBJECT
	
	private:
		
		//An Atomic Effect editor allows definition of
		//a predicate or function effect. 
		
		//It swtiches between which is being defined based on the
		//current entry in the operator combo box.
		DOMPredicateEffect* currentPredicateEffect;
		DOMFunctionEffect* currentFunctionEffect;
		
		DOMAtomicEffect* currentEffect; //The effect currently being displayed.
		
		QComboBox* operatorComboBox;
		
		//Only one of these is visable at a time.
		PredicateEditor* predicateEditor;
		FunctionEditor* functionEditor;
	
		DOMDomain* domain;
		
		QStackedLayout* editorLayout;
		
		bool selected;
		
	private:
		
		DOMPredicateEffect* createNewPredicateEffect ();
		
		DOMFunctionEffect* createNewFunctionEffect ();
		
	protected:
		
		
		//
		//Atomic Effect Visitor - used in initial construction.
		//
		virtual bool visitFunctionEffect (DOMFunctionEffect* effect);
		
		virtual bool visitPredicateEffect (DOMPredicateEffect* effect);
	
	protected slots:
		
		void handleOperatorComboBoxSelectionChanged (int index);
	
		void childFocusIn ();
		
		void childFocusOut ();
		
		void setSelected (bool selected);
		
	signals:
		void atomicEffectChanged (DOMAtomicEffect* newEffect, DOMAtomicEffect* oldEffect);
		
		void selectStatusChanged (bool selected);
		
	public:
		
		AtomicEffectEditor (DOMAtomicEffect& effect, DOMDomain& domain, 
			QWidget* parent = NULL);
		
		virtual ~AtomicEffectEditor () {
			if(selected) {
				emit selectStatusChanged (false);
			}
		}
		
		virtual void focusInEvent (QFocusEvent*);
		
		virtual void focusOutEvent (QFocusEvent*);
		
		virtual bool isSelected ();
		
		virtual DOMAtomicEffect* getCurrentAtomicEffect () {
			return currentEffect;
		}
		
};

class PredicateComboBox;
class PredicateEditor : public QWidget {
	
	Q_OBJECT
	
	private:
		DOMPredicateEffect* effect;
		PredicateComboBox* predicateComboBox;
	
	signals:
		
		void focusIn ();
		
		void focusOut ();
		
	protected slots:
		
		virtual void handleNewPredicate ();
		
		virtual void childFocusIn () {
			emit focusIn ();
		}
		
		virtual void childFocusOut () { 
			emit focusOut ();
		}
	
	public:
		
		PredicateEditor (DOMPredicateEffect& effect, DOMDomain& domain, 
			QWidget* parent = NULL);
			
		virtual QSize sizeHint () const {
			return minimumSize ();
		}
		
		
};

#include "FunctionComboBox.h"
#include "FunctionExpressionLineEdit.h"
class DOMFunctionExpression;

class EffectEditorFunctionExpressionLineEdit : public FunctionExpressionLineEdit {

	Q_OBJECT
	
	signals:
		void focusIn ();
		void focusOut ();
		
	public:
		
		EffectEditorFunctionExpressionLineEdit (QWidget* parent = NULL)
			: FunctionExpressionLineEdit (parent) { 
			
		}
		virtual void focusInEvent (QFocusEvent* event) {
			FunctionExpressionLineEdit::focusInEvent (event);
			emit focusIn ();
		}
		
		virtual void focusOutEvent (QFocusEvent* event) {
			FunctionExpressionLineEdit::focusOutEvent (event);
			emit focusOut ();
		}
};

class FunctionEditor : public QWidget {
	
	Q_OBJECT
	
	private:
		DOMFunctionEffect* effect;
		FunctionComboBox* functionComboBox;
		EffectEditorFunctionExpressionLineEdit* functionExpressionLineEdit;
	signals:
		
		void focusIn ();
		
		void focusOut ();
		
		
	protected slots:
		
		virtual void handleNewExpression (DOMFunctionExpression*);
		virtual void handleNewFunction ();
		
		virtual void childFocusIn () {
			emit focusIn ();
		}
		
		virtual void childFocusOut () { 
			emit focusOut ();
		}
	
	public:
		
		FunctionEditor (DOMFunctionEffect& effect, DOMDomain& domain, QWidget* parent = NULL);
		
		virtual QSize sizeHint () const {
			return minimumSize ();
		}
		
	
		virtual bool childInFocus () {
			return functionComboBox->hasFocus () 
				|| functionExpressionLineEdit->hasFocus();
		}
		
		virtual void focusInEvent (QFocusEvent*) {
			emit focusIn ();
		}
		
		virtual void focusOutEvent (QFocusEvent*) {
			emit focusOut ();
		}
		/**
		 * If the function expression line edit of is displaying an invalid
		 * function expression it is reverted when this widget becomes invisible.
		 */
		virtual void hideEvent (QHideEvent*) {
			if (functionExpressionLineEdit->isDisplayInvalidFunctionExpression ()) {
				functionExpressionLineEdit->revert ();
			}
		}
};
