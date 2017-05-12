/*
 *  GanttBarResizer.cpp
 *  
 *
 *  Created by Owen Thomas on 16/11/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "GanttBarResizer.h"
#include "GanttBarContainer.h"


void GanttBarResizer::mouseMoveEvent (QMouseEvent* event) {
	resizerMoved = true;
	int delta = initMovingPosition - event->x ();
	
	//Don't allow the resizer to be moved past the min or max position
	if(x()  - delta > minXPosition && x() - delta < maxXPosition) {
		move ( x() - delta, y() );
		emit moved (this);
	}
	
	//If we've moved past an end then set the position to be that end.
	else if (x() - delta <= minXPosition){
		move (minXPosition, y());
		emit moved (this);
	}
	else {
		move (maxXPosition, y());
		emit moved (this);
	}
	event->accept ();
}

void GanttBarResizer::mousePressEvent (QMouseEvent* event) {
	initMovingPosition = event-> x();
	event->accept ();
}

void GanttBarResizer::mouseReleaseEvent (QMouseEvent* event) {
	
	if(resizerMoved) {
		setEnabled (false);
	/**
		 There is a bug in the Gantt Chart that causes an arcane
		 bus error in apple / qt event handling.
		 
		 It has something to do with the fact that the call to 
		 container->commitModification will cause this GanttBar
		 to be deleted. 
		 
		 Deleting the event that triggers the ultimate deletion of 
		 this event appears to stop the bus error.
		 
		 I'd like to know why this is happening and why deleting
		 the event stops the problem. I imagine that when the widget
		 is deleted, this events perhaps stays around is accessed by
		 the event handling mechanism. It somehow has a reference to
		 some data that is deleted when this gantt bar is deleted.
		 
		 Explicitly deleting it here then stops the problem. In the same
		 way that chopping off your feet stops you from having an ingrown toe
		 nail. 
		
		 */
		event->accept ();
		//delete event;
		container->commitModification ();
	}
}
