/*
 *  AtomicEffectEditor.cpp
 *  
 *
 *  Created by Owen Thomas on 4/12/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "AtomicEffectEditor.h"
#include <QHBoxLayout>
#include <QLabel>
#include <QString>
#include <QObject>
#include <QSizePolicy>

#include "../../model/impl/DOMDomain.h"
#include "../../model/impl/DOMAtomicEffect.h"

//Function Editor 

#include "../../model/impl/DOMFunctionEffect.h"
#include "../../model/impl/DOMFunctionExpression.h"
//#include "FunctionComboBox.h"

FunctionEditor::FunctionEditor (DOMFunctionEffect& effect, DOMDomain& domain,
	QWidget* parent) : QWidget (parent) {
	
	this->effect = &effect;
	setFocusPolicy (Qt::ClickFocus);
	functionComboBox = new FunctionComboBox (domain, this);
	functionComboBox->setCurrentFunction (effect.getFunction ());
	functionExpressionLineEdit = new EffectEditorFunctionExpressionLineEdit (this);
	functionExpressionLineEdit->setFunctionExpression
		( * (effect.getExpression()));
	functionExpressionLineEdit->setDomain (domain);
	
	QSizePolicy sizePolicy (QSizePolicy::MinimumExpanding, QSizePolicy::Fixed);
	sizePolicy.setHorizontalStretch (1);
	functionExpressionLineEdit->setSizePolicy (sizePolicy);
	
	QLabel* byLabel = new QLabel (QObject::tr ("by"), this);
	
	QHBoxLayout* hBoxLayout = new QHBoxLayout ();
	setLayout (hBoxLayout);
	hBoxLayout->setMargin (0);
	hBoxLayout->addWidget (functionComboBox);
	hBoxLayout->addWidget (byLabel);
	hBoxLayout->addWidget (functionExpressionLineEdit);
	
	
	QObject::connect (functionComboBox, SIGNAL (currentIndexChanged (int)),
		this, SLOT (handleNewFunction ()));
		
	QObject::connect (functionExpressionLineEdit, SIGNAL (expressionChanged
		(DOMFunctionExpression*)), this, SLOT (handleNewExpression
		(DOMFunctionExpression*)));
		
	
	QObject::connect (functionComboBox, SIGNAL (focusIn()),
		this, SLOT (childFocusIn ()));
	QObject::connect (functionComboBox, SIGNAL (focusOut()),
		this, SLOT (childFocusOut ()));
		
	
	QObject::connect (functionExpressionLineEdit, SIGNAL (focusIn()),
		this, SLOT (childFocusIn ()));
	QObject::connect (functionExpressionLineEdit, SIGNAL (focusOut()),
		this, SLOT (childFocusOut ()));
}

void FunctionEditor::handleNewExpression (DOMFunctionExpression* expression) {
	effect->setExpression (expression);
}

void FunctionEditor::handleNewFunction () {
	effect->setFunction (functionComboBox->getCurrentFunction());
}

//Predicate Editor
#include "PredicateComboBox.h"
#include "../../model/impl/DOMPredicateEffect.h"

PredicateEditor::PredicateEditor (DOMPredicateEffect& effect, DOMDomain& domain, 
	QWidget* parent) : QWidget (parent) {

	this->effect = &effect;
	predicateComboBox = new PredicateComboBox (domain, this);
	predicateComboBox->setCurrentPredicate (effect.getPredicate());
	QHBoxLayout* hBoxLayout = new QHBoxLayout ();
	setLayout (hBoxLayout);
	hBoxLayout->setMargin (0);
	
	hBoxLayout->addWidget (predicateComboBox);
	
	
	QObject::connect (predicateComboBox, SIGNAL (currentIndexChanged (int)),
		this, SLOT (handleNewPredicate ()));
		
	QObject::connect (predicateComboBox, SIGNAL ( focusIn ()),
		this, SLOT (childFocusIn ()));
	
	QObject::connect (predicateComboBox, SIGNAL ( focusOut ()),
		this, SLOT (childFocusOut ()));
		
}

void PredicateEditor::handleNewPredicate () {
	this->effect->setPredicate (predicateComboBox->getCurrentPredicate ());
}

//Atomic Effect Editor

#define SET_IND 0
#define REMOVE_IND 1
#define INCREASE_IND 2
#define DECREASE_IND 3
#define SCALEUP_IND 4
#define SCALEDOWN_IND 5
#define ASSIGN_IND 6

#include "BrazilComboBox.h"
AtomicEffectEditor::AtomicEffectEditor (DOMAtomicEffect& effect, 
	DOMDomain& domain, QWidget* parent) : QWidget (parent) {
	
	this->domain = &domain;
	selected = false;
	setLayout (new QHBoxLayout ());
	
	setSizePolicy (QSizePolicy::MinimumExpanding, QSizePolicy::Fixed);
	setFocusPolicy (Qt::ClickFocus);
	
	operatorComboBox = new BrazilComboBox (this);
	
	operatorComboBox->addItem ("Set");
	operatorComboBox->addItem ("Remove");
	operatorComboBox->addItem ("Increase");
	operatorComboBox->addItem ("Decrease");
	operatorComboBox->addItem ("Scale-up");
	operatorComboBox->addItem ("Scale-down");
	operatorComboBox->addItem ("Assign");

	layout ()->addWidget (operatorComboBox);
	
	editorLayout = new QStackedLayout ();
	((QBoxLayout*)layout())->addLayout (editorLayout);
	
	currentPredicateEffect = NULL;
	currentFunctionEffect = NULL;
	
	currentEffect = NULL;
	
	predicateEditor = NULL;
	functionEditor = NULL;
	
	QObject::connect (operatorComboBox, SIGNAL (currentIndexChanged (int)),
		this, SLOT (handleOperatorComboBoxSelectionChanged (int)));
	
	QObject::connect ((BrazilComboBox*)operatorComboBox, SIGNAL (focusIn()),
		this, SLOT (childFocusIn ()));
		
	QObject::connect ((BrazilComboBox*)operatorComboBox, SIGNAL (focusOut ()),
		this, SLOT (childFocusOut()));
		
	effect.visit (this);
}

bool AtomicEffectEditor::visitFunctionEffect (DOMFunctionEffect* effect) {
	currentFunctionEffect = effect;
	currentEffect = effect;
	
	functionEditor = new FunctionEditor (*effect, *(this->domain), this);
	editorLayout->addWidget (functionEditor);
	
	QObject::connect (functionEditor, SIGNAL (focusIn ()),
		this, SLOT (childFocusIn ()));
		
	QObject::connect (functionEditor, SIGNAL (focusOut()),
		this, SLOT (childFocusOut ()));
		
	//Set combo box to correct value.
	switch(effect->getOperator ()) {
		
		case DOMFunctionEffect::increase:
			operatorComboBox->setCurrentIndex (INCREASE_IND);
		break;
		
		case DOMFunctionEffect::decrease:
			operatorComboBox->setCurrentIndex (DECREASE_IND);
		break;
		
		case DOMFunctionEffect::scaleUp:
			operatorComboBox->setCurrentIndex (SCALEUP_IND);
		break;
		
		case DOMFunctionEffect::scaleDown:
			operatorComboBox->setCurrentIndex (SCALEDOWN_IND);
		break;
		
		case DOMFunctionEffect::assign:
			operatorComboBox->setCurrentIndex (ASSIGN_IND);
		break;
		
	}
	
	return true;
}

bool AtomicEffectEditor::visitPredicateEffect (DOMPredicateEffect* effect) {
	currentPredicateEffect = effect;
	currentEffect = effect;
	
	predicateEditor = new PredicateEditor (*effect, *(this->domain), this);
	editorLayout->addWidget (predicateEditor);
	
	QObject::connect (predicateEditor, SIGNAL (focusIn ()),
		this, SLOT (childFocusIn ()));
		
	QObject::connect (predicateEditor, SIGNAL (focusOut()),
		this, SLOT (childFocusOut ()));
	

	if (effect->isNegated()) operatorComboBox->setCurrentIndex(REMOVE_IND);
	else operatorComboBox->setCurrentIndex(SET_IND);

	return true;

}

void AtomicEffectEditor::handleOperatorComboBoxSelectionChanged (int index) {
	
	switch (index) {
		
		case SET_IND:
			if(predicateEditor == NULL) {
				currentPredicateEffect = createNewPredicateEffect ();
				
				predicateEditor = new PredicateEditor (*currentPredicateEffect, 
					*(this->domain), this);
				editorLayout->addWidget (predicateEditor);
			}
			
			if(currentEffect != currentPredicateEffect) {
				emit atomicEffectChanged (currentPredicateEffect, currentEffect);
				currentEffect = currentPredicateEffect;
			}
			currentPredicateEffect->setNegated (false);
			editorLayout->setCurrentWidget (predicateEditor);
			
		break;
		
		case REMOVE_IND:
			if(predicateEditor == NULL) {
				currentPredicateEffect = createNewPredicateEffect ();
				
				predicateEditor = new PredicateEditor (*currentPredicateEffect, 
					*(this->domain), this);
				editorLayout->addWidget (predicateEditor);
			}
			if(currentEffect != currentPredicateEffect) {
				emit atomicEffectChanged (currentPredicateEffect, currentEffect);
				currentEffect = currentPredicateEffect;
			}

			currentPredicateEffect->setNegated (true);
			cout<<"Setting predicate '"<<currentPredicateEffect->getPredicate()->getName()<<"' to remove (true)"<<endl;
			editorLayout->setCurrentWidget (predicateEditor);

		break;
		
		case INCREASE_IND:
			if(functionEditor == NULL) {
				currentFunctionEffect = createNewFunctionEffect ();
				
				functionEditor = new FunctionEditor (*currentFunctionEffect,
					*(this->domain), this);
				editorLayout->addWidget (functionEditor);
				
				
				QObject::connect (functionEditor, SIGNAL (focusIn ()),
					this, SLOT (childFocusIn ()));
		
				QObject::connect (functionEditor, SIGNAL (focusOut()),
					this, SLOT (childFocusOut ()));

			}
			if(currentEffect != currentFunctionEffect) {
				emit atomicEffectChanged (currentFunctionEffect, currentEffect);
				currentEffect = currentFunctionEffect;
			}

			currentFunctionEffect->setOperator (DOMFunctionEffect::increase);
			editorLayout->setCurrentWidget (functionEditor);

		break;
		
		case DECREASE_IND:
			if(functionEditor == NULL) {
				currentFunctionEffect = createNewFunctionEffect ();
				
				functionEditor = new FunctionEditor (*currentFunctionEffect,
					*(this->domain), this);
				editorLayout->addWidget (functionEditor);
				
				QObject::connect (functionEditor, SIGNAL (focusIn ()),
					this, SLOT (childFocusIn ()));
		
				QObject::connect (functionEditor, SIGNAL (focusOut()),
					this, SLOT (childFocusOut ()));

			}
			if(currentEffect != currentFunctionEffect) {
				emit atomicEffectChanged (currentFunctionEffect, currentEffect);
				currentEffect = currentFunctionEffect;
			}
			currentFunctionEffect->setOperator (DOMFunctionEffect::decrease);
			editorLayout->setCurrentWidget (functionEditor);

	
		break;
		
		case SCALEUP_IND:
			if(functionEditor == NULL) {
				currentFunctionEffect = createNewFunctionEffect ();
				
				functionEditor = new FunctionEditor (*currentFunctionEffect,
					*(this->domain), this);
				editorLayout->addWidget (functionEditor);
				QObject::connect (functionEditor, SIGNAL (focusIn ()),
					this, SLOT (childFocusIn ()));
		
				QObject::connect (functionEditor, SIGNAL (focusOut()),
					this, SLOT (childFocusOut ()));


			}
			if(currentEffect != currentFunctionEffect) {
				emit atomicEffectChanged (currentFunctionEffect, currentEffect);
				currentEffect = currentFunctionEffect;
			}

			currentFunctionEffect->setOperator (DOMFunctionEffect::scaleUp);
			editorLayout->setCurrentWidget (functionEditor);


		break;
		
		case SCALEDOWN_IND:
			if(functionEditor == NULL) {
				currentFunctionEffect = createNewFunctionEffect ();
				
				functionEditor = new FunctionEditor (*currentFunctionEffect,
					*(this->domain), this);
				editorLayout->addWidget (functionEditor);
				
				QObject::connect (functionEditor, SIGNAL (focusIn ()),
					this, SLOT (childFocusIn ()));
		
				QObject::connect (functionEditor, SIGNAL (focusOut()),
					this, SLOT (childFocusOut ()));

			}
			if(currentEffect != currentFunctionEffect) {
				emit atomicEffectChanged (currentFunctionEffect, currentEffect);
				currentEffect = currentFunctionEffect;
			}

			currentFunctionEffect->setOperator (DOMFunctionEffect::scaleDown);
			editorLayout->setCurrentWidget (functionEditor);

			
		break;
		
		case ASSIGN_IND:
			if(functionEditor == NULL) {
				currentFunctionEffect = createNewFunctionEffect ();
				
				functionEditor = new FunctionEditor (*currentFunctionEffect,
					*(this->domain), this);
				editorLayout->addWidget (functionEditor);
				
				QObject::connect (functionEditor, SIGNAL (focusIn ()),
					this, SLOT (childFocusIn ()));
		
				QObject::connect (functionEditor, SIGNAL (focusOut()),
					this, SLOT (childFocusOut ()));

			}
			if(currentEffect != currentFunctionEffect) {
				emit atomicEffectChanged (currentFunctionEffect, currentEffect);
				currentEffect = currentFunctionEffect;
			}

			currentFunctionEffect->setOperator (DOMFunctionEffect::assign);
			editorLayout->setCurrentWidget (functionEditor);
			
			
		break;
	}
}

DOMPredicateEffect* AtomicEffectEditor::createNewPredicateEffect () {
	return new DOMPredicateEffect (**(domain->getPredicates().begin()), *(domain->getDOMElement()->getOwnerDocument()), true);
}

DOMFunctionEffect* AtomicEffectEditor::createNewFunctionEffect () {
	DOMFunctionExpression* expression = new DOMFunctionExpressionValue (*(domain->getDOMElement()->getOwnerDocument()), 0);
	DOMFunctionEffect* returnEffect = 
		new DOMFunctionEffect ( **(domain->getFunctions().begin ()), DOMFunctionEffect::assign, 
		*expression,*(domain->getDOMElement()->getOwnerDocument()));
		
	return returnEffect;
}

void AtomicEffectEditor::childFocusIn () {
	setSelected (true);
}

void AtomicEffectEditor::childFocusOut () {
	setSelected (false);
}

void AtomicEffectEditor::focusInEvent (QFocusEvent*) {
	setSelected (true);
}

void AtomicEffectEditor::focusOutEvent (QFocusEvent*) {
	setSelected (false);
}

void AtomicEffectEditor::setSelected (bool selected) {
	if(selected) {
		setAutoFillBackground(true);
		setBackgroundRole (QPalette::AlternateBase);
	}
	else {
		setAutoFillBackground (false);
	}
	this->selected = selected;
	emit selectStatusChanged (selected);
}

bool AtomicEffectEditor::isSelected () {
	return selected;
}

