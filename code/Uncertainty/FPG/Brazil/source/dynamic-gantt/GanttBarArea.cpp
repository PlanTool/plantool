/*
 *  GanttBarArea.cpp
 *  
 *
 *  Created by Owen Thomas on 15/05/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "GanttBarArea.h"
#include "GanttBarContainer.h"
void GanttBarArea::addGanttBarContainer (GanttBarContainer& container)
{
	QVBoxLayout* vLayout = (QVBoxLayout*) layout ();
	
	container.setParent (this);
	container.setVisible (true);
	
	/*
	 We have no containers so far.
	 
	 */
	if(vLayout->count() == 0) {
		//even numbered containers use the alternate background
		//colour
		container.setAlternateBackgroundColour ();
		vLayout->addWidget (&container);
		
		//This is the bottom container so it must draw it's bottom
		//line.
		container.setDrawBottomLine (true);
		return;
	}
	
	for(int i = 0; i < vLayout->count(); i++) {
		
		GanttBarContainer& other = 
			*((GanttBarContainer*)vLayout->itemAt(i)->widget());

		if(container < other) {
			vLayout->insertWidget (i, &container);
			
			
			if(i % 2 != 0) {
				container.removeBackgroundColour ();
			}
			else {
				container.setAlternateBackgroundColour();
			} 
			
			for(int j = i + 1; j < vLayout->count(); j++) {
				GanttBarContainer* current = 
					(GanttBarContainer*) vLayout->itemAt(j)->widget();
				if(current->hasBackgroundColour()) 
					current->removeBackgroundColour ();
				else current->setAlternateBackgroundColour ();
			}
			
			return;
		}
	}
	
	//Else we are adding to the bottom of the gantt chart.
	
	//Find the current bottom widget and disable its drawing of bottom
	//line.
	GanttBarContainer* bottom = 
		(GanttBarContainer*)vLayout->itemAt( vLayout->count() - 1)->widget();
	
	bottom->setDrawBottomLine (false);
	
	//Add the new bottom container
	vLayout->addWidget (&container);
	
	//set the background colour properly
	if(vLayout->count() % 2 != 0) container.setAlternateBackgroundColour ();
	else container.removeBackgroundColour ();
	
	//draw the bottom line.
	container.setDrawBottomLine(true);
	
}

void GanttBarArea::removeGanttBarContainer (GanttBarContainer& container)
{

	//
	//I am going to change all this..
	//
	QVBoxLayout* vLayout = (QVBoxLayout*) layout ();
	for(int i = 0; i < vLayout->count(); i++) {
		if(vLayout->itemAt(i)->widget() == &container) {
			vLayout->removeItem (vLayout->itemAt(i));			
			container.setVisible (false);
			return;
		}
	}
	
}


