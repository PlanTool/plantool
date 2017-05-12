/*
 * $Id$
 *
 * This file is part of the Brazil Planner. Copyright NICTA 2006.
 *
 * This file is commercial in confidence. You should no be looking at
 * it unless you are an employee of NICTA, a student who has signed
 * their IP to NICTA, or you have signed an NDA covering the Brazil
 * source code. If you are not one of these people we will poke out
 * your eyes with a gerkhin while forcing you to sing the Brazilian
 * national anthem.
 *
 */


/*
 *  HotAreaWidget.cpp
 *  
 *
 *  Created by Owen Thomas on 27/10/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */



#include <QPainter>
#include <QMouseEvent>

#include "HotAreaWidget.h"

/**
 * Create the generic circle/square interactive widget
 * @param radius how big is it
 * @param color what color is it
 * @param round true for round, false for square 
 * @param locked can it move itself?
 * @param parent parent widget
 */
HotAreaWidget::HotAreaWidget (int radius, QColor color, bool round, bool locked, QWidget* parent)  : QWidget (parent) {

	this->radius = radius;
	this->round = round;
	this->locked = locked;
	
	brush.setColor (color);
	pen.setColor (QColor("black"));
	brush.setStyle (Qt::SolidPattern);
	pen.setStyle(Qt::SolidLine);

	// [daa] What's the +5 for?
	resize (radius * 2 + 5, radius * 2 + 5);
	setFocusPolicy (Qt::ClickFocus);
}





/** 
 * Paint circle or rectangle depending on value of "round"
 */
void HotAreaWidget::paintEvent (QPaintEvent*) {

	QPainter painter (this);
	painter.setRenderHint (QPainter::Antialiasing);
	
	painter.setPen (pen);
	painter.setBrush (brush);
	
	if (round) painter.drawEllipse (0, 0, 2 * radius, 2 * radius);
	else painter.drawRect(0, 0, 2*radius, 2*radius);
}


/**
 * Qt tells this widget it's being dragged.
 * Do the move (if not locked) and tell interested parties about it
 * Used for altering uncertain durations.
 */
void HotAreaWidget::mouseMoveEvent (QMouseEvent* event) {

        if (locked) return; // This widget can't move

	int deltaX = event->x () - radius;
	
	int newX = x () + deltaX;
	if(newX < 0) newX = 0;
	
	
	move (newX, y ());
	
	emit newPosition (x ()); 
	
}


/**
 * Hey, someone has taken an interest in this humble little dot. Let someone know! 
 */
void HotAreaWidget::enterEvent (QEvent* event) {
        emit enter ();
	event->ignore ();
	show ();
}


/**
 * Not interested anymore... leaving :(
 */
void HotAreaWidget::leaveEvent (QEvent* event) {
        emit leave ();
	event->ignore ();
}


/**
 * Tell interested parties about double clicks.
 * Used to change Gantt Chart.
 */
void HotAreaWidget::mouseDoubleClickEvent(QMouseEvent* event) {
        emit doubleClicked(this);
        event->ignore();
}

