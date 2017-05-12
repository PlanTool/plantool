/*
 *  FunctionListWidget.cpp
 *  
 *
 *  Created by Owen Thomas on 24/11/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "FunctionListWidget.h"
#include "FunctionListEditor.h"

#include "../../model/impl/DOMDomain.h"
#include "../../model/impl/DOMProblem.h"
#include <QVBoxLayout>
#include <set>
#include "../../model/impl/DOMFunction.h"
#include <QLayoutItem>

FunctionListWidget::FunctionListWidget (DOMProblem& problem, QWidget* parent) : QWidget (parent){
	this->problem = NULL;
	QBoxLayout* listLayout = new QVBoxLayout (this);
	setLayout (listLayout);
	setAutoFillBackground (true);
	setBackgroundRole (QPalette::Base);
	
	init (&problem);
}

void FunctionListWidget::init (DOMProblem* problem) {
	if(this->problem) {
		this->problem->getDomain()->removeListener (this);
	}
	this->problem = problem;
	this->problem->getDomain()->addListener (this);
	
	int numEditors = layout()->count ();
	
	while(numEditors--) {
		QLayoutItem* item = layout()->itemAt (0);
		QWidget* widget = item->widget ();
		layout()->removeItem (item);
		delete item;
		delete widget;
	}
	set<DOMFunction*, ltFunction> sortedFunctions;
	
	set<DOMFunction*> functions = problem->getDomain()->getFunctions ();
	set<DOMFunction*>::iterator it;
	for(it = functions.begin(); it != functions.end(); it++) {
		sortedFunctions.insert (*it);
	}
	
	//A bette approach would be to make a (box)layout subclass that does
	//the sorting..
	set<DOMFunction*, ltFunction>::iterator jt;
	
	for(jt = sortedFunctions.begin (); jt != sortedFunctions.end (); jt ++) {
		FunctionListEditor* editor = new FunctionListEditor (this,
			problem, *jt);
			
		editor->setFocusPolicy (Qt::StrongFocus);
		editor->setAutoFillBackground (true);
		
		layout()->addWidget (editor);
	}
	((QBoxLayout*)layout())->addStretch (100);
	setMinimumSize (layout()->minimumSize());
}

void FunctionListWidget::setProblem (DOMProblem& problem) {
	init (&problem);
}


void FunctionListWidget::functionAdded (DOMFunction* function) {
	FunctionListEditor* editor = new FunctionListEditor (this, problem, function);
	editor->setFocusPolicy (Qt::StrongFocus);
	editor->setAutoFillBackground (true);

	layout ()->addWidget (editor);
	setMinimumSize (layout()->minimumSize());

}


