/*
 *  GanttBar.cpp
 *  
 *
 *  Created by Owen Thomas on 16/11/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "GanttBar.h"
#include "GanttBarContainer.h"

void GanttBar::mousePressEvent (QMouseEvent* event) {
	initMovingPosition = event-> x();
}

void GanttBar::mouseMoveEvent (QMouseEvent* event) {
	if(!editable) return;
	
	barHasMoved = true;
	int delta = initMovingPosition - event->x ();
	
	if(x() - delta > 0) {
		move ( x() - delta, y() );
	
		emit moved (this);
	}
	else {
		move (0, y());
		emit moved (this);
	}
	container->setWidth ();
}


void GanttBar::mouseReleaseEvent (QMouseEvent* event) {

	//Only trigger the event within the container
	//if we have moved the position of the gantt bar.
	if(barHasMoved) {
	
		/**
		 There is a bug in the Gantt Chart that causes an arcane
		 bus error in apple / qt event handling.
		 
		 It has something to do with the fact that the call to 
		 container->commitModification wil lcause this GanttBar
		 to be deleted. 
		 
		 Deleting the event that triggers the ultimate deletion of 
		 this event appears to stop the bus error.
		 
		 I'd like to know why this is happening and why deleting
		 the event stops the problem. I imagine that when the widget
		 is deleted, this events perhaps stays around is accessed by
		 the event handling mechanism. It somehow has a reference to
		 some data that is deleted when this gantt bar is deleted.
		
		 */
		event->accept ();

		// [daa] According to comments above, this should not be commented out.
		// I found it this way, but when I uncommented it, it crashed on bar moving.
		//delete event;
		
		container->commitModification ();
	}
	barHasMoved = false;

}

void GanttBar::mouseDoubleClickEvent (QMouseEvent* ) {
	if(editable)container->nextOutcome ();
}
