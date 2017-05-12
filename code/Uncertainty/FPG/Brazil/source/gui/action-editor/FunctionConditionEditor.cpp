/*
 *  FunctionConditionEditor.cpp
 *  
 *
 *  Created by Owen Thomas on 30/11/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include <QComboBox>
#include <QHBoxLayout>

#include <QString>
#include <QChar>

#include "FunctionConditionEditor.h"

#include "FunctionComboBox.h"
#include "FunctionExpressionLineEdit.h"
#include "../../model/impl/DOMFunctionCondition.h"
#include "../../model/impl/DOMFunction.h"

#define EQ_IND 0
#define GT_IND 1
#define LT_IND 2
#define GE_IND 3
#define LE_IND 4

FunctionConditionEditor::FunctionConditionEditor (DOMFunctionCondition& condition,
	DOMDomain& domain,
	FunctionComboBox* functionComboBox, 
	FunctionExpressionLineEdit* functionExpressionLineEdit, QWidget* parent) 
	: ConditionEditor (condition, domain, parent) {
	

	this->functionComboBox = functionComboBox;
	this->functionExpressionLineEdit = functionExpressionLineEdit;
	
	functionComboBox->setParent (this);
	functionExpressionLineEdit->setParent (this);
	
	setLayout (new QHBoxLayout ());
	
	operatorComboBox = new QComboBox (this);
	operatorComboBox->addItem ("=");
	operatorComboBox->addItem (">");
	operatorComboBox->addItem ("<");
	
	//Feed in ³ & ² chars.
	QChar leChar (8804);
	QString leString (leChar);
	
	QChar geChar (8805);
	QString geString (geChar);
	
	operatorComboBox->addItem (geString);
	operatorComboBox->addItem (leString);
	
	//Set the correct operator in the combo box.
	
	switch(condition.getOperator ()) {
		case DOMFunctionCondition::equals:
			operatorComboBox->setCurrentIndex (EQ_IND);
		break;
		
		case DOMFunctionCondition::greaterThan:
			operatorComboBox->setCurrentIndex (GT_IND);
		break;
		
		case DOMFunctionCondition::lessThan:
			operatorComboBox->setCurrentIndex (LT_IND);
		break;
		
		case DOMFunctionCondition::greaterThanOrEqualTo:
			operatorComboBox->setCurrentIndex (GE_IND);
		break;
		
		case DOMFunctionCondition::lessThanOrEqualTo:
			operatorComboBox->setCurrentIndex (LE_IND);
		break;
	}
	
	layout()->addWidget (functionComboBox);
	layout()->addWidget (operatorComboBox);
	layout()->addWidget (functionExpressionLineEdit);

	//This will cause the line edit to take up as much space as possible.
	QSizePolicy sizePolicy (QSizePolicy::MinimumExpanding, QSizePolicy::Fixed);
	sizePolicy.setHorizontalStretch (1);
	functionExpressionLineEdit->setSizePolicy (sizePolicy);
	
	layout()->addWidget (deleteButton);
	layout()->addWidget (newButton);
	
	deleteButton->show ();
	newButton->show();
	
	functionComboBox->setCurrentFunction (condition.getFunction());
	
	QObject::connect (functionComboBox, SIGNAL (currentFunctionChanged (DOMFunction*)),
		this, SLOT (handleFunctionChanged (DOMFunction*)));
		
	QObject::connect (operatorComboBox, SIGNAL (currentIndexChanged (int)),
		this, SLOT (handleIndexChanged (int)));
		
	QObject::connect (functionExpressionLineEdit, SIGNAL 
		(expressionChanged (DOMFunctionExpression*)), this,
		SLOT (handleExpressionChanged (DOMFunctionExpression*)));
	
	setMinimumSize (layout()->minimumSize ());
	setSizePolicy (QSizePolicy::MinimumExpanding, QSizePolicy::Fixed);
		
}

void FunctionConditionEditor::handleIndexChanged (int index) {
	switch (index) {
		case EQ_IND:
			((DOMFunctionCondition*)condition)->setOperator
				(DOMFunctionCondition::equals);
		break;
		
		case GT_IND:
			((DOMFunctionCondition*)condition)->setOperator
				(DOMFunctionCondition::greaterThan);
		break;
		
		case LT_IND:
			((DOMFunctionCondition*)condition)->setOperator
				(DOMFunctionCondition::lessThan);
		break;
		
		case GE_IND:
			((DOMFunctionCondition*)condition)->setOperator
				(DOMFunctionCondition::greaterThanOrEqualTo);
		break;
		
		case LE_IND:
			((DOMFunctionCondition*)condition)->setOperator
				(DOMFunctionCondition::lessThanOrEqualTo);
		break;
	}
}

void FunctionConditionEditor::handleExpressionChanged 
	(DOMFunctionExpression* expression) {
	
	((DOMFunctionCondition*)condition)->setExpression (expression);
}

void FunctionConditionEditor::handleFunctionChanged (DOMFunction* function) {
	((DOMFunctionCondition*)condition)->setFunction (function);
}
