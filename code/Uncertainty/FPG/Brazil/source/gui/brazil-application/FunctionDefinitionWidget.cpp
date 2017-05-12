/*
 *  FunctionDefinitionWidget.cpp
 *  
 *
 *  Created by Owen Thomas on 24/11/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "FunctionDefinitionWidget.h"

#include <QVBoxLayout>

#include <QPushButton>
#include <QScrollArea>

#include "FunctionListWidget.h"

#include "../../model/impl/DOMProblem.h"
using namespace std;
#include <iostream>
FunctionDefinitionWidget::FunctionDefinitionWidget (DOMProblem& problem, QWidget* parent)
	: QWidget (parent) {
	
	this->domain = problem.getDomain ();
	QVBoxLayout* vBoxLayout = new QVBoxLayout ();
	setLayout (vBoxLayout);
	vBoxLayout->setSizeConstraint (QLayout::SetMaximumSize);
	
	QWidget* editorWidget = new QWidget (this);
	QBoxLayout* editorLayout = new QHBoxLayout ();
	
	editorWidget->setLayout (editorLayout);
	
	editorLayout->setSpacing (0);
	editorLayout->setMargin (0);
	
	editorLineEdit = new QLineEdit (editorWidget);
	QPushButton* editorButton = new QPushButton ("New Function", editorWidget);
	
	editorLayout->addWidget (editorLineEdit);
	editorLayout->addWidget (editorButton);
	
	vBoxLayout->addWidget (editorWidget);
	
	QScrollArea* scrollArea = new QScrollArea (this);	
	functionListWidget = new FunctionListWidget (problem, scrollArea);
	
	QSizePolicy functionListWidgetSizePolicy (QSizePolicy::MinimumExpanding, QSizePolicy::MinimumExpanding);
	functionListWidgetSizePolicy.setHorizontalStretch (1);
	functionListWidgetSizePolicy.setVerticalStretch (0);
	functionListWidget->setSizePolicy (functionListWidgetSizePolicy);
	
	scrollArea->setHorizontalScrollBarPolicy (Qt::ScrollBarAlwaysOff);
	scrollArea->setWidget (functionListWidget);
	
	vBoxLayout->addWidget (scrollArea);

	QObject::connect (editorLineEdit, SIGNAL (returnPressed ()),
		this, SLOT (setValue ()));
	QObject::connect (editorButton, SIGNAL (clicked (bool)), this, SLOT (setValue ()));
}

void FunctionDefinitionWidget::setProblem (DOMProblem& problem) {
	functionListWidget->setProblem (problem);
	functionListWidget->setMinimumSize (functionListWidget->sizeHint ());
	functionListWidget->setMaximumSize (functionListWidget->sizeHint ());

	this->domain = problem.getDomain ();
	updateGeometry ();
}


void FunctionDefinitionWidget::setValue () {
	if(editorLineEdit->text().trimmed ().count () == 0) return;
	
	QStringList tokens = editorLineEdit->text().split (" ");
	QString functionName = tokens [0];
	
	for (int i = 1; i < tokens.count (); i++) {
		functionName.append("_");
		functionName.append (tokens [i]);
	}
	
	DOMFunction* function = domain->getFunction (functionName.toUtf8().data());
	
	if(NULL == function)  {
		function = 
			new DOMFunction (functionName.toUtf8().data(), domain->getDOMElement()->getOwnerDocument ());
		domain->addFunction (function);
		
		editorLineEdit->clear();
	}
}

