/*
 *  GanttNameArea.cpp
 *  
 *
 *  Created by Owen Thomas on 16/05/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "GanttNameArea.h"
#include "GanttBarContainer.h"
void GanttNameArea::addGanttBarContainer (GanttBarContainer& container)
{
	QLabel* containerLabel = new QLabel (container.getLabel ());
	
	//When added to a layout with a fixed size constraint
	//These will have a height of 30. 
	containerLabel->setMaximumHeight (30);
	containerLabel->setMinimumHeight (30);
	
	containerLabel->setParent (this);
	containerLabel->setVisible(true);
	
	containerNames [&container] = containerLabel;
	reverseContainerNames [containerLabel] = &container;
	
	if(layout->count() == 0) {
		layout->addWidget (containerLabel);
		return;
	}
	
	for(int i = 0; i < layout->count(); i++) {
		QWidget* label = layout->itemAt(i)->widget();
		GanttBarContainer& other = *(reverseContainerNames [label]);
		if(container < other) {
			layout->insertWidget (i, containerLabel);
			return;
		}
	}
	
	layout->addWidget (containerLabel);	
	
}

void GanttNameArea::removeGanttBarContainer (GanttBarContainer& container)
{
	for(int i = 0; i < layout->count(); i++) {
		QWidget* label = layout->itemAt(i)->widget ();
		if(reverseContainerNames [label] == &container) {
			layout->removeItem (layout->itemAt(i));
			reverseContainerNames.erase (label);
			containerNames.erase (&container);
			delete label;
			return;
		}
	}
}

