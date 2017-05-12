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
 *  GanttBar.h
 *  
 *
 *  Created by Owen Thomas on 12/05/06.
 *  
 *
 */

#ifndef gantt_bar
#define gantt_bar

#include <QString>
#include <QFrame>
#include <QColor>
#include <QPalette>
#include <QPainter>
#include <QBrush>

#include <QMouseEvent>
#include <QMoveEvent>
#include <QResizeEvent>
#include <iostream>

class GanttBarContainer;

using namespace std;

/**
 * A single, coloured gantt bar representing a duration in time.
 */
 
//To-do: tool tip when mouse over - show action / outcome naem.

class GanttBar : public QFrame
{
	Q_OBJECT
	
	private:
		//Used to define the background colour
		QPalette newPalette;
		QColor backgroundColour;

		//When the user clicks the gantt bar to move it, this is 
		//the position they first click.
		int initMovingPosition;
		
		//The enclosing container.
		GanttBarContainer* container;

		bool barHasMoved;
		bool editable;
		
	signals:
		//emitted when an enter event occurs
		void enter(GanttBar*);
		
		//emitted when a leave event occurs
		void leave(GanttBar*);
		
		//emitted when a mouse move event occus
		void moved(GanttBar*);
		
		//emitted when a resize event occurs.
		void resized(GanttBar*);
		
	protected:
		
		void mousePressEvent (QMouseEvent* event);
	
		void mouseMoveEvent (QMouseEvent* event);
	
		void mouseReleaseEvent (QMouseEvent* event);
		
		void mouseDoubleClickEvent (QMouseEvent* );		
		
		void enterEvent (QEvent*)
		{
			emit enter(this);
		}
		void leaveEvent (QEvent*)
		{
			emit leave(this);
		}
		void resizeEvent (QResizeEvent*)
		{
			emit resized (this);
		}
		
		
	public:
		GanttBar (QColor& backgroundColour, GanttBarContainer* parent) : QFrame ((QWidget*)parent)
		{
			container = parent;
			newPalette = palette ();
			newPalette.setColor (QPalette::Window, backgroundColour);
			barHasMoved = false;
			setAutoFillBackground (true);
			setPalette (newPalette);
			setBackgroundRole (QPalette::Window);
			setFrameStyle(QFrame::Box | QFrame::Plain);
		}
		
		void setEditable (bool editable) {
			this->editable = editable;
		}
		
		bool isEditable () {
			return editable;
		}
};

#endif

