/*
 *  PredicateListEditor.cpp
 *  
 *
 *  Created by Owen Thomas on 15/08/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "PredicateListEditor.h"
#include <QCheckBox>
#include <QPalette>
#include <QLabel>
#include <QHBoxLayout>
		
PredicateListEditor::PredicateListEditor (QWidget* parent, DOMProblem* problem, 
	DOMPredicate* predicate): QWidget (parent) {
	
	this->predicate = predicate;
	this->problem = problem;
	
	//Create one child widget, a check box, and add it to a layout

	QHBoxLayout* mainLayout = new QHBoxLayout ();
	setLayout (mainLayout);
			
	checkBox = new QCheckBox (predicate->getName(), this);
	mainLayout->setMargin (0);
	mainLayout->setSpacing (0);
			
	mainLayout->addWidget (checkBox);
			
	if(problem->isInitial (predicate)) {
		checkBox->setCheckState (Qt::Checked);
	}
	
	setBackgroundRole (QPalette::Base);
	
	//When the check box is toggled, add/remove predicate
	//from the initial set.
	QObject::connect (checkBox, SIGNAL (toggled (bool)), this,
		SLOT (setInitial(bool)));
	
}

