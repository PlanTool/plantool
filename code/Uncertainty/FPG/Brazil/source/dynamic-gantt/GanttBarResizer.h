/* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is Brazil.
 *
 * The Initial Developer of the Original Code is
 * National ICT Australia.
 * Portions created by the Initial Developer are Copyright (C) 2007
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *	Douglas Aberdeen	(doug.aberdeen@gmail.com)
 *	Owen Thomas		(owen.thomas@nicta.com.au)
 *	Olivier Buffet		(olivier.buffet@loria.fr)
 *
 * ***** END LICENSE BLOCK ***** */
/*
 *  GanttBarResizer.h
 *  
 *
 *  Created by Owen Thomas on 16/11/06.
 *  
 *
 */
#ifndef gantt_bar_resizer
#define gantt_bar_resizer
class GanttBarContainer;

#include <QFrame>
#include <QMouseEvent>
#include <limits>
#include <iostream>


/**
 * A Widget that sits either to the left or the right of
 * a GanttBar, within a GanttBarContainer. Moving the resizer
 * will trigger signals that can be sent to the GanttBar
 * to resize it.
 */
class GanttBarResizer : public QFrame
{
	Q_OBJECT
	
	public:
		//is it to the left or the right of the gantt bar
		enum Side {LEFT, RIGHT};
	
	private:
		
		int initMovingPosition;
		GanttBarContainer* container;
		bool resizerMoved;
		Side side;
		
		//bounds on position of the position the
		//resizer can be moved to.
		int maxXPosition, minXPosition;
		
	signals:
		void moved(GanttBarResizer*);

	protected:
	
		void mouseMoveEvent (QMouseEvent* event);
		
		void mousePressEvent (QMouseEvent* event);
		
		void mouseReleaseEvent (QMouseEvent* );
		
		void enterEvent (QEvent*)
		{
			setFrameStyle(QFrame::Box | QFrame::Plain);
		}
		
		void leaveEvent (QEvent*)
		{
			setFrameStyle (QFrame::NoFrame);
		}
	
	public:
		
		GanttBarResizer (GanttBarContainer* parent, Side side = LEFT,
			int minXPosition = 0, int maxXPosition = std::numeric_limits<int>::max ()) : QFrame ((QWidget*)parent)
		{
			this->minXPosition = minXPosition;
			this->maxXPosition = maxXPosition;
			
			container = parent;
			resizerMoved = false;
			setFrameStyle (QFrame::NoFrame);			
			//Very simple gantt bar resizer, with fixed size.
			resize (25, 25);
			
			if(side == LEFT) this->minXPosition -= width ();
			this->maxXPosition -= width ();
		}
};
#endif
