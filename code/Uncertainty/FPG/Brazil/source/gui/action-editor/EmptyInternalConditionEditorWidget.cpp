/*
 *  EmptyInternalConditionEditorWidget.cpp
 *  
 *
 *  Created by Owen Thomas on 30/11/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "EmptyInternalConditionEditorWidget.h"

#include <QHBoxLayout>
#include <QVBoxLayout>
#include <QLabel>

#include "../../model/impl/DOMInternalCondition.h"
#include "../../model/impl/DOMPredicateCondition.h"
#include "../../model/impl/DOMFunctionExpression.h"
#include "../../model/impl/DOMFunctionCondition.h"

#include "FunctionComboBox.h"
#include "PredicateComboBox.h"

EmptyInternalConditionEditorWidget::EmptyInternalConditionEditorWidget 
	(DOMInternalCondition& internalCondition, DOMDomain& domain, QWidget* parent)
	: QWidget (parent) {

	this->internalCondition = &internalCondition;
	this->domain = &domain;
	this->domain->addListener (this);
	
	setLayout (new QVBoxLayout ());
	((QBoxLayout*)layout())->setMargin (0);
	((QBoxLayout*)layout())->setSpacing (0);
	
	QWidget* predicateWidget = new QWidget (this);
	predicateWidget->setLayout (new QHBoxLayout ());
	
	QWidget* functionWidget = new QWidget (this);
	functionWidget->setLayout (new QHBoxLayout ());
	
	predicateAddButton = new QPushButton ("add");
	functionAddButton = new QPushButton ("add");
	
	QLabel* predicateLabel = new QLabel (QObject::tr ("Add a Predicate Condition Over: "));
	QLabel* functionLabel = new QLabel (QObject::tr ("Add a Function Condition Over: "));
	
	predicateComboBox = new PredicateComboBox (domain, predicateWidget);
	functionComboBox = new FunctionComboBox (domain, functionWidget);
	
	predicateWidget->layout()->addWidget (predicateLabel);
	predicateWidget->layout()->addWidget (predicateComboBox);
	
	((QBoxLayout*)predicateWidget->layout ())->addStretch (10);
	predicateWidget->layout()->addWidget (predicateAddButton);
	
	functionWidget->layout()->addWidget (functionLabel);
	functionWidget->layout()->addWidget (functionComboBox);
	
	((QBoxLayout*)functionWidget->layout())->addStretch (10);
	functionWidget->layout()->addWidget (functionAddButton);
	
	layout()->addWidget (predicateWidget);
	layout()->addWidget (functionWidget);
	
	if(!domain.getPredicates().size()) predicateAddButton->setEnabled (false);
	if(!domain.getFunctions().size()) functionAddButton->setEnabled (false);
	
	QObject::connect (predicateAddButton, SIGNAL (pressed ()), this,
		SLOT (addPredicateCondition ()));
		
	QObject::connect (functionAddButton, SIGNAL (pressed ()), this,
		SLOT (addFunctionCondition ()));
	
}

void EmptyInternalConditionEditorWidget::addPredicateCondition () {
//Create a new PredicateCondition and add to this->condition.
	DOMPredicateCondition* predicateCondition = 
		new DOMPredicateCondition (predicateComboBox->getCurrentPredicate(),
			domain->getDOMElement()->getOwnerDocument ());
	
	this->internalCondition->addChild (predicateCondition);

}

void EmptyInternalConditionEditorWidget::addFunctionCondition () {
	DOMFunctionExpression* expression = 
		new DOMFunctionExpressionValue (*(domain->getDOMElement()->getOwnerDocument ()),0.0);
	//Must create a new DOMFunctionCondition and add to this->condition.
	DOMFunctionCondition* functionCondition = 
		new DOMFunctionCondition (functionComboBox->getCurrentFunction(),
			DOMFunctionCondition::greaterThan, expression, 
			domain->getDOMElement()->getOwnerDocument ());
	

	this->internalCondition->addChild (functionCondition);
}

