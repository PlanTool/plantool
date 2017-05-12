/*
 *  PredicateConditionEditor.cpp
 *  
 *
 *  Created by Owen Thomas on 29/11/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "PredicateConditionEditor.h"

#include <QHBoxLayout>

PredicateConditionEditor::PredicateConditionEditor (DOMPredicateCondition& condition,
	DOMDomain& domain,
	PredicateComboBox* predicateComboBox,
	QWidget* parent) : ConditionEditor (condition, domain, parent) {
	
	this->predicateComboBox = predicateComboBox;
	predicateComboBox->setParent (this);
	predicateComboBox->setCurrentPredicate (condition.getPredicate ());
	
	QPushButton* negatedButton = new QPushButton ((QObject::tr ("not")),this);
	negatedButton->setCheckable (true);
	negatedButton->setChecked (condition.isNegated ());
	
	QHBoxLayout* hBoxLayout = new QHBoxLayout ();
	setLayout (hBoxLayout);
	
	hBoxLayout->addWidget (negatedButton);
	hBoxLayout->addWidget (predicateComboBox);
	
	//spacing between predicate combo box and action buttons
	hBoxLayout->addStretch (200);
	
	
	hBoxLayout->addWidget (deleteButton);
	hBoxLayout->addWidget (newButton);
	
	newButton->show ();
	deleteButton->show ();
	
	QObject::connect (negatedButton, SIGNAL (toggled(bool)), 
		this, SLOT (negateCondition (bool)));
		
	QObject::connect (predicateComboBox, SIGNAL (focusOut ()),
		this, SLOT (setPredicate ()));	
	
	setSizePolicy (QSizePolicy::MinimumExpanding, QSizePolicy::Fixed);
	setMinimumSize (layout()->minimumSize());
	//setAutoFillBackground (true);
	
}


