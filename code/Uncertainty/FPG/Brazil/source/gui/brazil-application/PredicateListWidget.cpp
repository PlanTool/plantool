#include "PredicateListWidget.h"

PredicateListWidget::PredicateListWidget (DOMProblem& problem, QWidget* parent) :
	QWidget (parent) {
	
	this->problem = NULL;
		
	
	
	//Create layout
	QBoxLayout* mainLayout = new QVBoxLayout ();
	setLayout (mainLayout);
	
	//Create edit widget
	QWidget* editorWidget = new QWidget (this);
	QBoxLayout* editorLayout = new QHBoxLayout ();
	editorWidget->setLayout (editorLayout);
	
	editorLayout->setSpacing (0);
	editorLayout->setMargin (0);
	
	editorLineEdit = new QLineEdit (editorWidget);
	QPushButton* editorButton = new QPushButton ("New Predicate", editorWidget);
	
	editorLayout->addWidget (editorLineEdit);
	editorLayout->addWidget (editorButton);
	
	mainLayout->addWidget (editorWidget);
	
	//Create scroll area to store individual editors
	editors = new QScrollArea (this);
	editors->setHorizontalScrollBarPolicy (Qt::ScrollBarAlwaysOff);
	
	editorsWidget = new QWidget ();
	QVBoxLayout* vLayout = new QVBoxLayout ();
	this->editorLayout = vLayout;
	vLayout->setSizeConstraint (QLayout::SetMaximumSize);
	editorsWidget->setLayout (vLayout);
	vLayout->setSpacing (0);
	editors->setBackgroundRole (QPalette::Base);
	
	init (problem);
	
	editors->setWidget (editorsWidget);
	mainLayout->addWidget (editors);


	QObject::connect (editorLineEdit, SIGNAL (returnPressed ()),
		this, SLOT (newPredicate()));
	
	QObject::connect (editorButton, SIGNAL (clicked ()), this, SLOT (newPredicate()));
}

void PredicateListWidget::init (DOMProblem& problem) {
	//Remove all editors from list
	QLayout* removeLayout = editorsWidget->layout ();
	int numEditors = removeLayout->count ();
	
	while (numEditors --) {
		QLayoutItem* item = removeLayout->itemAt (0);
		QWidget* widgetForItem = item->widget ();
		removeLayout->removeItem (item);
		delete item;
		delete widgetForItem;
	}
	editorLayout->parentWidget()->setMinimumSize (editorLayout->minimumSize());
	if(this->problem) {
		this->problem->getDomain()->removeListener (this);
	}
	
	//Set problem.
	this->problem = &problem;
	problem.getDomain()->addListener (this);
	
	//Create editors.
	set<DOMPredicate*> predicates = problem.getDomain()->getPredicates ();
	set<DOMPredicate*>::iterator it;
	for(it = predicates.begin (); it != predicates.end(); it++) {
		PredicateListEditor* currentEditor = new PredicateListEditor (editorsWidget, 
			&problem, *it);
		
		currentEditor->setFocusPolicy (Qt::StrongFocus);
		
		editorLayout->addWidget (currentEditor);
		editorLayout->parentWidget()->setMinimumSize (editorLayout->minimumSize());
	}
}

void PredicateListWidget::setProblem (DOMProblem& problem) {
	init (problem);
}

void PredicateListWidget::predicateAdded (DOMPredicate* predicate) {
	//Create a new predicate editor the just added predicate.
	PredicateListEditor* newEditor = new PredicateListEditor (editors, this->problem, predicate);
	newEditor->setFocusPolicy (Qt::StrongFocus);
	newEditor->setParent (editorsWidget);
	editorLayout->addWidget (newEditor);
	editorLayout->parentWidget()->setMinimumSize (editorLayout->minimumSize());
}

 PredicateListWidget::~PredicateListWidget () {
	this->problem->getDomain()->removeListener (this);
}

