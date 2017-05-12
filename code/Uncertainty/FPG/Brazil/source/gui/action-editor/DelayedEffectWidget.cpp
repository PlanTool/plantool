/*
 *  DelayedEffectWidget.cpp
 *  
 *
 *  Created by Owen Thomas on 25/03/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */
#include "DelayedEffectWidget.h"
#include <map>
#include <QPainter>
#include "../../model/impl/DOMPropertySet.h"

using namespace std;

DelayedEffectWidget::DelayedEffectWidget (QWidget* parent, DOMEffectSet* atStartEffect, DOMEffectSet* effect, 
	BrazilController* controller, map<QString, DOMDuration*> defaultDurations, QColor backgroundColour, 
	int height, double scale,
	DOMDomain* domain): QWidget (parent)

{
        DOMPropertySet* props = domain->getProperties();

	if(!effect->isDelayed()) throw std::invalid_argument ("Effect must be delayed");
	this->effect= effect;
	this->effectEditor = NULL;
	
	this->atStartEffect = atStartEffect;
	this->atStartEffectEditor = NULL;
		
	this->controller = controller;
	
	//These can be changed by a client of this class, but
	//we should set them to something somewhat sensible at the start.
	this->hotAreaRadius =5;
	this->dragAreaWidth = 5;
	this->effectAreaWidth = 9;
	
	this->leftMostDropDownPosition = 80;
	this->dropDownMargin = 0;
	
	
	this->mouseInside = false;
	this->movingHotArea = false;
	this->movingDragArea = false;
	

	this->effectAreaBeingClicked = false;
	this->atStartEffectAreaBeingClicked = false;
	
	this->siblingWidgetFocusInCount = 0;
	this->siblingWidgetEnteredCount = 0;
	
	this->domain = domain;
	
	
	//We copy the durations passed in, rather than taking pointers to the
	//same data. Multiple DelayedEffectWidgets can then be constructed with
	//different DOMDurations.
	//Alternativly, we could pull this out of this class and assume that
	//defaultDurations is specific for this class.
	map<QString, DOMDuration*>::iterator it;
	for(it = defaultDurations.begin(); it != defaultDurations.end(); it++)
	{
		durations [(it->first)] = it->second->copy();
	}

	backgroundBrush.setStyle (Qt::SolidPattern);
	durationPathBrush.setStyle(Qt::SolidPattern);
	durationPathBrush.setColor(backgroundColour);
	
	//Create a duration delegate to draw the duration, given the current
	//duration.
	durationDelegate = DurationDelegateFactory::createDurationDelegate
		(effect->getDelay(),delegateHeight,delegateScale);
	this->durationNames = new QComboBox (this);
	
	
	
	//Set the initial position of the drop down.
	updateDurationDropDown ();
	
	effect->addListener(this);
	
	setFocusPolicy (Qt::StrongFocus);
	
	setScale (scale);
	setHeight (height);

	//When duration names loses focus, this widget should
	//receive focus.
	setTabOrder (durationNames, this);
	
	
	
	
	resize (getWidth (), getHeight ());
	
	//When the selection changes in the drop down, I want to update
	//the duration bar with the new duration.
	QObject::connect (durationNames, SIGNAL (currentIndexChanged (const QString&)), 
	this, SLOT (changeDurationText (const QString&)));
	
	

	hotAreaWidget = new HotAreaWidget (hotAreaRadius, 
					   QColor("crimson"),//props->getPropertyString("gui HotAreaWidget colour")), 
					   true, 
					   false, 
					   parent
					   );
	
	if(durationDelegate->hasHotArea ()) {
		hotAreaWidget->raise ();
		hotAreaWidget->setVisible (true);
		hotAreaWidget->move (durationDelegate->getHotAreaPosition () - 5, 19);
	}
	else {
		hotAreaWidget->hide ();
	}
	
	
	QObject::connect (hotAreaWidget, SIGNAL (newPosition (int)), this, SLOT (newHotAreaPosition(int)));
	QObject::connect (hotAreaWidget, SIGNAL (enter ()), this, SLOT (siblingWidgetEntered ()));
	QObject::connect (hotAreaWidget, SIGNAL (leave ()), this, SLOT (siblingWidgetLeft ()));
}

DelayedEffectWidget::~DelayedEffectWidget ()
{
	QObject::disconnect (durationNames, SIGNAL (currentIndexChanged (const QString&)), this, SLOT (changeDurationText (const QString&)));
	delete durationNames;
	
	effect->removeListener (this);
	/*
	if(effectEditor) {
		effect->removeListener(effectEditor);
		delete effectEditor;
	}*/
}

bool DelayedEffectWidget::isChildFocus () 
{
	//Does duration names have focus?
	if(durationNames->hasFocus ()) return true;
	
	//Does any of its children?
	QList<QWidget*> children = durationNames->findChildren<QWidget *>();
	
	for(int i = 0; i < children.size(); i++)
	{
		if(children [i]->hasFocus ()) return true;
	}
	
	//No, so return false.
	return false;
}

bool DelayedEffectWidget::insideHotArea(int x, int y)
{
	if(!durationDelegate->hasHotArea()) return false;
			
	return ((x > durationDelegate->getHotAreaPosition() - hotAreaRadius )
	&& (x < durationDelegate->getHotAreaPosition() +  hotAreaRadius + 2)
	&& (y > (durationDelegate->getHeight () / 2 - hotAreaRadius))
	&& (y < (durationDelegate->getHeight () / 2) +  hotAreaRadius + 2));
}

bool DelayedEffectWidget::insideDragArea (int x, int y)
{
	return (x > durationDelegate->getScaledWidth() + effectAreaWidth)
	&& (x < durationDelegate->getScaledWidth() + effectAreaWidth + dragAreaWidth)
	&& (y < durationDelegate->getHeight());
}

bool DelayedEffectWidget::insideEffectArea (int x, int y)
{
	return (x > durationDelegate->getScaledWidth()) && (x < durationDelegate->getScaledWidth() + effectAreaWidth)
	&& (y < durationDelegate->getHeight());
}

bool DelayedEffectWidget::insideAtStartEffectArea (int x, int y)
{
    return ((x < effectAreaWidth)
	    && (y < durationDelegate->getHeight()));
}

bool DelayedEffectWidget::insideArea (int x, int y)
{
	return (x < durationDelegate->getScaledWidth () + effectAreaWidth + dragAreaWidth)
	&& (y < durationDelegate->getHeight());
}

		
void DelayedEffectWidget::updateDurationDropDown ()
{
	if(showDurationDropDown ())
	{
		durationNames->setVisible(true);
		
		//Check to make sure we actually need to update the position
		if((durationNames->x() + durationNames->width()) != (durationDelegate->getScaledWidth()))
		{
			//If the duration names cannot fit between the end of the duration bar and
			//the left most position allowed, position it just to the left of the
			//left most position.
			if(getDurationWidth () - durationNames->width() <= leftMostDropDownPosition)
			{
				durationNames->move(leftMostDropDownPosition, dropDownMargin);
			}
			else
			{
				durationNames->move(durationDelegate->getScaledWidth() - durationNames->width(), dropDownMargin);
			}
		}
	}
	
	else
	{
		durationNames->setVisible(false);
	}
}

void DelayedEffectWidget::changeDurationText (const QString& durationName)
{
	//Save the old duration into the map, so that if the user
	//comes back to it their previous value will remain
	DOMDuration* oldDuration = effect->getDelay()->copy();
	
	
	this->durations [oldDuration->getName ()] = oldDuration;
	//Find the duration with the selected name and pass
	//it to the delayed effect.
	if (durations.find (durationName) != durations.end()) {
		
		
		effect->setDelay (durations [durationName]);
	}
	else
		throw std::invalid_argument  ("invalid duration name");
	
	resize(getWidth(),getHeight());
	
	emit resizing ();
	
}


void DelayedEffectWidget::setDurationNames (const QStringList& durationNames)
{	
	for(int i = 0; i < this->durationNames->count(); i++)
	{
		this->durationNames->removeItem (i);
	}
	QString oldDurationName = effect->getDelay()->getName();
	//This call will trigger index changing events on the drop down
	//which in turn will trigger the changeDuration slot on this
	//which in turn will change the duration in the DOMDelayedEffect.
	//Hence, above, we store the oldDurationName.
	QObject::disconnect (this->durationNames, SIGNAL (currentIndexChanged (const QString&)), this, SLOT (changeDurationText (const QString&)));
	
	this->durationNames->addItems (durationNames);
	
	//Then set the selection to the old duration name.
	
	//Alternativly during this section we could disconnect the
	//signal.
	this->durationNames->setCurrentIndex
		(this->durationNames->findText 
		(oldDurationName));
		QObject::connect (this->durationNames, SIGNAL (currentIndexChanged (const QString&)), 
	this, SLOT (changeDurationText (const QString&)));
}
 
void DelayedEffectWidget::durationChanged ()
{
	DOMDuration* duration = effect->getDelay();
	this->durationDelegate = 
		DurationDelegateFactory::createDurationDelegate(duration, delegateHeight,delegateScale);
	
	if(durationDelegate->hasHotArea ()) {
		hotAreaWidget->raise ();
		hotAreaWidget->setVisible (true);
		hotAreaWidget->move (durationDelegate->getHotAreaPosition () - 5, 19);
	}
	else {
		hotAreaWidget->hide ();
	}
	updateDurationDropDown();
	resize(getWidth(), getHeight());
	update ();
}

void DelayedEffectWidget::paintEvent (QPaintEvent* event)
{
	QPainter painter(this);
	painter.setRenderHint(QPainter::Antialiasing);
	
	if(showBorder())
	{
		painter.setPen(backgroundPen);
				
		//draw border
		painter.drawRect (0, 0, durationDelegate->getScaledWidth (), durationDelegate->getHeight());
	}
	
	//Paint Duration area.
	painter.setPen (durationPathPen);

	painter.setBrush (durationPathBrush);
			
	//retrieve QPainter path from delegate and draw that.
	painter.drawPath (durationDelegate->getPath (0, 0));
	
	if(showDragArea())
	{
		//If we're selected paint the drag area.
		painter.setPen   (dragAreaPen);
		painter.setBrush (dragAreaBrush);
				
		painter.drawRect (effectAreaWidth + durationDelegate->getScaledWidth (), 0, dragAreaWidth, durationDelegate->getHeight());
	}
	
		
	if(showEffectArea())
	{
		//If we're selected paint the drag area.
		painter.setPen   (effectAreaPen);
		painter.setBrush (effectAreaBrush);
				
		painter.drawRect (durationDelegate->getScaledWidth (), 0, effectAreaWidth, durationDelegate->getHeight());

		// [daa] Added for at start effects
		painter.drawRect (0, 0, effectAreaWidth, durationDelegate->getHeight());
	}
	
	QWidget::paintEvent (event);
}

void DelayedEffectWidget::mousePressEvent (QMouseEvent* event)
{
	if(insideHotArea (event->x(), event->y()))
	{		
		movingHotArea = true;
	}
	else if(insideEffectArea (event->x(), event->y()))
	{
		effectAreaBeingClicked = true;
	}
	else if(insideAtStartEffectArea (event->x(), event->y()))
	{
		atStartEffectAreaBeingClicked = true;
	}
	else if(insideDragArea (event->x(), event->y ()))
	{
		movingDragArea = true;
	}
}

void DelayedEffectWidget::mouseReleaseEvent (QMouseEvent* )
{
	movingHotArea = false;
	movingDragArea = false;
	
	if(effectAreaBeingClicked)  {
		effectAreaBeingClicked = false;
		if(NULL == effectEditor) {
			effectEditor = new EffectSetEditor (*effect, *domain, this);
			effectEditor->setWindowFlags (Qt::Popup);
			effectEditor->show();
		}
		else {
			//Toggle display of effect editor on/off
			effectEditor->setVisible (!effectEditor->isVisible ());
		}
		
		//move the effect editor to be underneath this widget
		if(effectEditor->isVisible ()) {
			effectEditor->move ( mapToGlobal ( 
			QPoint(durationDelegate->getScaledWidth () + effectAreaWidth, 
			y () + height ())));
		}
	}
	// Added by daa
	else if (atStartEffectAreaBeingClicked)  {
		atStartEffectAreaBeingClicked = false;
		if(NULL == atStartEffectEditor) {
			atStartEffectEditor = new EffectSetEditor (*atStartEffect, *domain, this);
			atStartEffectEditor->setWindowFlags (Qt::Popup);
			atStartEffectEditor->show();
		}
		else {
			//Toggle display of effect editor on/off
			atStartEffectEditor->setVisible (!atStartEffectEditor->isVisible ());
			atStartEffectEditor->show();
		}
		
		//move the effect editor to be underneath this widget
		if(atStartEffectEditor->isVisible ()) {
			atStartEffectEditor->move ( mapToGlobal ( 
			QPoint(effectAreaWidth, 
			y () + height ())));
		}
	}

}


void DelayedEffectWidget::mouseMoveEvent (QMouseEvent* event)
{
	if(movingDragArea && event->x() > 0)
	{
		durationDelegate->setScaledWidth(event->x());
		
		resize(getWidth(),getHeight());
		
		if(durationDelegate->hasHotArea ()) {
			hotAreaWidget->move (durationDelegate->getHotAreaPosition () - 5, hotAreaWidget->y ());
		}
		emit resizing();
	}
	
	//Width has potentially changed, so update the
	//position of duration names.
	updateDurationDropDown();
	update();
}

void DelayedEffectWidget::leaveEvent (QEvent*)
{
	mouseInside = false;
	updateDurationDropDown ();
	update ();
	if(!hasFocus ()) hotAreaWidget->hide ();
}

void DelayedEffectWidget::enterEvent (QEvent*)
{
	mouseInside = true;
	updateDurationDropDown ();
	update();
	if(showHotArea ())hotAreaWidget->show ();

}

void DelayedEffectWidget::focusInEvent (QFocusEvent*)
{
	updateDurationDropDown ();
	update();
	emit focusIn ();
	if(showHotArea ())hotAreaWidget->show ();
}

void DelayedEffectWidget::focusOutEvent (QFocusEvent*)
{
	updateDurationDropDown ();
	update();
	emit focusOut ();
	if(!showHotArea ())  hotAreaWidget->hide ();
}


