/*
 *  FunctionListEditor.cpp
 *  
 *
 *  Created by Owen Thomas on 24/11/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */
#include <QHBoxLayout>
#include <QString>

#include "FunctionListEditor.h"
#include "../../model/impl/DOMFunction.h"
#include "../../model/impl/DOMProblem.h"

FunctionListEditor::FunctionListEditor (QWidget* parent,
	DOMProblem* problem, DOMFunction* function) {
	
	this->function = function;
	this->problem = problem;
	
	
	QHBoxLayout* mainLayout = new QHBoxLayout (this);
	setLayout (mainLayout);
	
	nameLabel = new QLabel (function->getName (), this);
	
	value = new QLineEdit (this);
	if(problem->initialFunctionValues ().count (function) != 0) {
		double dblValue = problem->initialFunctionValues() [function];
		QString valueString;
		valueString = valueString.setNum (dblValue);
		value->setText (valueString);
	}
	else {
		value->setText ("0");
	}
	
	mainLayout->setMargin (0);
	
	mainLayout->addWidget (nameLabel);
	mainLayout->addStretch (10);
	mainLayout->addWidget (value);
	
	setSizePolicy (QSizePolicy::MinimumExpanding, QSizePolicy::Fixed);
	setBackgroundRole (QPalette::Base);
	setAutoFillBackground (true);
	
	QObject::connect (value, SIGNAL (editingFinished ()),
		this, SLOT (setValue ()));
}

void FunctionListEditor::setValue () {
	bool ok = false;
	double doubleValue = this->value->text().toDouble (&ok);
	if(ok) {
		problem->setFunctionValue (this->function, doubleValue);
	}
}

QSize FunctionListEditor::sizeHint () const {
	return QSize (300, 30);
}

