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
 *  DelayedEffectWidget.h
 *  
 *
 *  Created by Owen Thomas on 25/03/06.
 *  
 *
 */

#ifndef delayed_effect_widget
#define delayed_effect_widget

#include <QEvent>
#include <QSize>
#include <QWidget>
#include <QColor>
#include <QPen>
#include <QPainterPath>
#include <QBrush>
#include <QMouseEvent>
#include <QMenu>
#include <QComboBox>

#include "EffectSetEditor.h"
#include "DurationDelegate.h"
#include "BrazilController.h"
#include "HotAreaWidget.h"

#include "../../model/EffectListener.h"
#include "../../model/impl/DOMEffectSet.h"
#include "../../model/impl/DOMDomain.h"

/**
 * A Delayed Effect Widget presents a durative effect as a solid,
 * coloured bar with an area at the left of the bar for toggling
 * effect display and an area beyond this for lengthning and shortening
 * the bar.
 *
 * Further manipulation of the duration is possible through a hot area which
 * may or may not be present depending on the current duratoin type being
 * displayed.
 *
 * In the top right cordner the DelayedEffetWidget presents a combo box
 * for toggling between different duration types.
 *
 * Drawing of the bar is delgated to an instance of a DurationDelegate.
 */
class DelayedEffectWidget : public QWidget, public EffectListener
{
	Q_OBJECT
	
	private:
		//The effect we are representing / manipulating
		DOMEffectSet* effect;
		DOMEffectSet* atStartEffect;
		
		//The object that draws the bar shape.
		DurationDelegate* durationDelegate;
		
		int delegateHeight;
		double delegateScale;
		
		//Pens draw the border of a shape
		//Brushes the internal colour.
		QPen hotAreaPen;
		QBrush hotAreaBrush;
		
		//The area beneath the duration bar drawn by
		//the duration delegate.
		QPen backgroundPen; /** Essentially border pen */
		QBrush backgroundBrush;
		
		//The sytle of the duration bar.
		QPen durationPathPen;
		QBrush durationPathBrush;
		
		//The style of the area on the far right for dragging
		QPen dragAreaPen;
		QBrush dragAreaBrush; 
		
		//The style of the effect area.
		QPen effectAreaPen;
		QBrush effectAreaBrush;
		
		//Hot areas are drawn as circles and as such have a radius.
		int hotAreaRadius;
		
		int dragAreaWidth;
		int effectAreaWidth;

		//Set my leaveEvent and enterEvent and indicates
		//whether the mouse is inside the DelayedEffectWidget.
		bool mouseInside;
		
		//True if the mouse has been pressed inside either the
		//hot area or drag area, but not released.
		bool movingHotArea;
		bool movingDragArea;
		
		//Because we override mousePress and mouseDown, 
		//the mouseClicked event no longer works correctly.
		//This is set to true when the mouse is pressed inside
		//the effect area and false when it is released.
		bool effectAreaBeingClicked;
		bool atStartEffectAreaBeingClicked;

		//When the effect ara has been 'clicked' an effect
		//editor is opened. This cannot be displayed as a child
		//of this widget so we must signal that we want it to
		//be displayed by another, perhaps parent widget.
		EffectSetEditor* effectEditor;
		EffectSetEditor* atStartEffectEditor;


		
		//The left most position that the combo box can drop down.
		//This is set by parent classes AtomicEffectWidget and OutcomeWidget
		//to indicate the furthest left they want the drop down position to
		//be displayed - otherwise it would override their label text field.
		int leftMostDropDownPosition;
		
		//margin from the top of the widget to the top of the duration
		//combo box.
		int dropDownMargin;
		
		//
		//This is not simple and I think this could be refactored.
		//
		
		//The DelayedEffectWiget is used by ActionEffectWidget and OutcomeWidget
		//which display widgets over the top of it (name / probability text fields)
		//When the mouse enters these widgets the mouse will 'leave' this widget -
		//causing mouseInside to become false. However the mouse is still 'inside' the boundary
		//widget. When the mouse focuses in or enters a widget that ActionEffectWidget
		//or OutcomeWidget regards as a sibling of this, we want the respective count to
		//be incremented. When it leaves the count should be decremented.
		//Then we can check to see if the mouse is inside a sibling widget by checking
		//to see if either count is greater than 0.
		int siblingWidgetEnteredCount;
		int siblingWidgetFocusInCount;
		
		//The top right combo box of duration names.
		QComboBox* durationNames;
		
		//We maintain a map of duration names to duration instances
		//so that when the duration name is changed we can return
		//the duration last used - rather than creating a new one.
		//This is initially populated with default values.
		map<QString, DOMDuration*> durations;
	
	
		map<QString, QString> operatorDisplayNames;
		
		DOMDomain* domain;		
		
		BrazilController* controller;
		
		QMenu* contextMenu;
		
		QWidget* hotAreaWidget;
		
	protected:
		
		/**
		 * Returns true if a child widget or its child has
		 * focus.
		 *
		 * @return true if a child widget or its child widget has
		 * focus.
		 */
		bool isChildFocus ();
		
		/**
		 * Returns true if a sibling widget has focus.
		 *
		 * @return true if a sibling widget has focus.
		 */
		bool isSiblingWidgetInFocus ()
		{
			return siblingWidgetFocusInCount > 0;
		}
		
		/**
		 * Returns true if the mouse is inside a 
		 * sibling widget.
		 *
		 * @return true if the mouse is inside a sibling 
		 * widget.
		 */
		bool isSiblingWidgetEntered ()
		{
			return siblingWidgetEnteredCount > 0;
		}
		
		/**
		 * Return true if the border is to be displayed.
		 *
		 * @return true if the border is to be displayed.
		 */		 
		bool showBorder()
		{
			return hasFocus () || mouseInside || isChildFocus () || isSiblingWidgetInFocus () || isSiblingWidgetEntered ();
		}
		
		/**
		 * Return true if the duration drop down is to be displayed.
		 *
		 * @return true if the duration drop down is to be displayed.
		 */	
		bool showDurationDropDown ()
		{
			return hasFocus () || mouseInside || isChildFocus () || isSiblingWidgetInFocus () || isSiblingWidgetEntered ();
		}
		
		/**
		 * Return true if the effect area is to be displayed.
		 *
		 * @return true if the effect area is to be displayed.
		 */	
		bool showEffectArea ()
		{
			return hasFocus () || mouseInside || isChildFocus () || isSiblingWidgetInFocus () || isSiblingWidgetEntered ();
		}
		
		/**
		 * Return true if the hot area is to be displayed.
		 *
		 * @return true if the hot area is to be displayed.
		 */	
		bool showHotArea ()
		{
			return durationDelegate->hasHotArea () && (hotAreaWidget->hasFocus () || hasFocus () || mouseInside || isChildFocus () || isSiblingWidgetInFocus () || isSiblingWidgetEntered ());
		}
		
		/**
		 * Return true if the drag area is to be displayed.
		 *
		 * @return true if the drag area is to be displayed.
		 */	
		bool showDragArea ()
		{
			return hasFocus () || mouseInside || isChildFocus () || isSiblingWidgetInFocus () || isSiblingWidgetEntered ();
		}
		
		/** 
		 * Returns true if the x, y coordinates are inside the boundary
		 * of the hot area. Returns false if there is no hot area or otherwise.
		 *
		 * @return if x, y are within the boundary of the hot area.
		 */
		bool insideHotArea (int x, int y);
		
		/** 
		 * Returns true if the x, y coordinates are inside the boundary
		 * of the drag area. Returns false if there is no hot area or otherwise.
		 *
		 * @return if x, y are within the boundary of the hot area.
		 */
		bool insideDragArea (int x, int y);
		
		/** 
		 * Returns true if the x, y coordinates are inside the boundary
		 * of the effect area. Returns false if there is no hot area or otherwise.
		 *
		 * @return if x, y are within the boundary of the hot area.
		 */
		bool insideEffectArea (int x, int y);
		bool insideAtStartEffectArea (int x, int y);
		
		/** 
		 * Returns true if the x, y coordinates are inside the boundary
		 * of the entire area, that is the duration bar + drag area and 
		 * effect area. Returns false if there is no hot area or otherwise.
		 *
		 * This is not the same as returning whether the x, y coordinates
		 * are inside this widget. The widget can extend arbitarially 
		 * out to the left and to the bottom. Also the widget will have to 
		 * be larger than this area if the duration names has to extend 
		 * out to the left because the duration bar is to short to accomodate it.
		 *
		 * @return if x, y are within the boundary of the hot area.
		 */
		bool insideArea (int x, int y);
		
		/**
		 * If the drop down is to be displayed it positions the drop down
		 * to the far right of the duration bar area. It will potentially
		 * extend out beyond the duration bar area if the distance between
		 * leftMostDropDownPosition and the right end of the duration bar
		 * cannot accomodate its width.
		 */
		void updateDurationDropDown ();
		
		
	public slots:
	
	    /**
		 * Called when the selection in the duration drop down
		 * has changed and the duration bar needs to be updated.
		 *
		 * @param durationName, the name of the duration to be
		 * displayed.
		 */
		void changeDurationText (const QString& durationName);
		
		
		void siblingWidgetFocusIn () 
		{ 
			siblingWidgetFocusInCount ++; 
			
			//Only update if we have moved from all
			//siblings focused to one being focused.
			if(siblingWidgetFocusInCount == 1) {
				updateDurationDropDown ();
				update(); 
			}
		}
		void siblingWidgetFocusOut () 
		{ 
			siblingWidgetFocusInCount --; 
			
			if(siblingWidgetFocusInCount == 0) {
				updateDurationDropDown ();
				update();  
			}
		}
		void siblingWidgetEntered () 
		{ 
			siblingWidgetEnteredCount ++;
			
			if(siblingWidgetEnteredCount == 1) {
				updateDurationDropDown ();
				update(); 
			}
		}
		void siblingWidgetLeft () 
		{ 
			siblingWidgetEnteredCount --;
			
			if(siblingWidgetEnteredCount == 0) {
				updateDurationDropDown ();
				update();  
			}
		}
		
	protected slots:
		
		void newHotAreaPosition (int x) {
			if(durationDelegate->hasHotArea ()) {
				durationDelegate->setHotAreaPosition (x + 5);
				resize (getWidth (), getHeight ());
				updateDurationDropDown ();
				update ();
				emit resizing();
			}
		}
	signals:
		
		/**
	     * Signals that the duration area has changed width.
		 *
		 */
		void resizing ();
		
		/**
		 * Signals that the Effect editor has been opened at
		 * the x position relative to our position. A parent
		 * widget should position this beneath us at xPosition.
		 *
		 * @param editor, the EffectEditor to be displayed.
		 * @param xPosition, the x position relative to the
		 * left boundary of this widget to display editor.
		 */
		//void effectAreaClicked (EffectEditor* editor, int xPosition);
		
		void focusIn ();
		
		void focusOut ();
		
	public:
		
		/**
		 * Create a DelayedEffectWidget as the child of parent, representing
		 * the DelayedEffect effect, with the initial duration values defaultDurations
		 * and with the initial backgroundColour backgroundColour.
		 */
		DelayedEffectWidget (QWidget* parent,
				     DOMEffectSet* atStartEffect, 
				     DOMEffectSet* effect, 
				     BrazilController* controller,
				     map<QString, DOMDuration*> defaultDurations, 
				     QColor backgroundColour, 
				     int height, 
				     double scale, 
				     DOMDomain* domain);
		
		/**
		 * Delete the DelayedEffectWidget.
		 * DelayedEffectWidget is a listener for its
		 * DelayedEffect - it removes from this as a listener.
		 * It also will delete its EffectEditor if its been
		 * created.
		 */
		~DelayedEffectWidget();
		
		//
		//EffectListener methods
		//
		virtual void effectAdded(DOMAtomicEffect*) { }
		virtual void effectRemoved(DOMAtomicEffect*) { }
		
		
		virtual void durationChanged();
		
		
		void setLeftMostDropDownPosition(int position)
		{
			leftMostDropDownPosition = position;
		}
		
		/**
		 * Returns the total width of the duration bar, plus
		 * effect area and drag area.
		 *
		 * @return the width of the duration bar, plus effect area
		 * and drag area.
		 */
		int getWidth ()
		{
			if(durationDelegate->getScaledWidth () 
				< durationNames->width () + leftMostDropDownPosition) {
				return 2 + dragAreaWidth + effectAreaWidth + durationNames->width () + leftMostDropDownPosition;
			}
			return 2 + dragAreaWidth + effectAreaWidth + durationDelegate->getScaledWidth();
		}
		
		int getHeight()
		{
			return 2 + durationDelegate->getHeight();
		}
		
		void setHeight (int height)
		{
			
			if(height >= 2)  {
				height = height - 2;
				delegateHeight = height;
				durationDelegate->setHeight (height);
			}
		}
		/**
		 * Returns the width of just the duration bar.
		 * 
		 * @return the width of the duration bar.
		 */
		int getDurationWidth ()
		{
			return durationDelegate->getScaledWidth ();
		}
		
		
		DOMEffectSet* getEffect ()
		{
			return effect;
		}
		
		//QWidget methods
		int minimumHeight ()
		{
			return getHeight();
		}
		
		int minimumWidth ()
		{
			return max(getWidth(), leftMostDropDownPosition + durationNames->minimumWidth());
		}
		
		//
		//Lots of setters and getters for properties described
		//at the top of the class!
		//
		//The general theme for the styles - pens and brushes,
		//is that I expose a restricted subset (colours and border
		//thicknesses) - not the pens / brushes themselves. 
		
		
		
		void setDurationNames (const QStringList& durationNames);
		
			int getHotAreaBorderWidth ()
		{
			return hotAreaPen.width ();
		}
		
		void setHotAreaBorderWidth (unsigned int width)
		{
			hotAreaPen.setWidth (width);
		}
		
		int getBorderWidth ()
		{
			return backgroundPen.width ();
		}
		
		void setBorderWidth (unsigned int width)
		{
			backgroundPen.setWidth (width);
		}
		
		int getDragAreaBorderWidth ()
		{
			return dragAreaPen.width ();
		}
		
		void setDragAreaBorderWidth (unsigned int width)
		{
			dragAreaPen.setWidth (width);
		}
		
		QColor getHotAreaBorderColour ()
		{
			return hotAreaPen.color ();
		}
		
		void setHotAreaBorderColour (QColor colour)
		{
			hotAreaPen.setColor (colour);
		}
		
		QColor getBorderColour ()
		{
			return backgroundPen.color();
		}
		
		void setBorderColour (QColor colour)
		{
			backgroundPen.setColor (colour);
		}
		
		QColor getDragAreaBorderColour ()
		{
			return dragAreaPen.color();
		}
		
		void setDragAreaBorderColour (QColor colour)
		{
			dragAreaPen.setColor (colour);
		}
		
		QColor getHotAreaColour ()
		{
			return hotAreaBrush.color();
		}
		
		void setHotAreaColour (QColor colour)
		{
			hotAreaBrush.setColor (colour);
		}
		
		QColor getDurationColour ()
		{
			return durationPathBrush.color ();
		}
		
		void setDurationColour (QColor colour)
		{
			durationPathBrush.setColor (colour);
		}
		
		int getDragAreaWidth ()
		{
			return dragAreaWidth;
		}
		
		void setDragAreaWidth (int width)
		{
			this->dragAreaWidth = width;
		}
		
		int getHotAreaRadius ()
		{	
			return hotAreaRadius;
		}
		
		void setHotAreaRadius (int radius)
		{
			hotAreaRadius = radius;
		}

		int getEffectAreaWidth ()
		{
			return effectAreaWidth;
		}
		
		void setEffectAreaWidth (int width)
		{
			effectAreaWidth = width;
		}
		
		QColor getEffectAreaColour ()
		{	
			return effectAreaBrush.color();
		}
		
		void setEffectAreaColour (QColor colour)
		{	
			effectAreaBrush.setColor(colour);
		}
		
		void setContextMenu (QMenu* menu)
		{
			this->contextMenu = menu;
		}
		
		QMenu* getContextMenu () 
		{
			return contextMenu;
		}
		
		/**
		 * Sets the scale, the number of pixels
		 * occupied by 1 unit of duration.
		 *
		 * @param scale, the number of pixels occupied
		 * by 1 unit of duration.
		 */
		void setScale (double scale)
		{
			delegateScale = scale;
			durationDelegate->setScale (scale);
		}
		
		double getScale ()
		{
			return durationDelegate->getScale();
		}
		
		//
		//QWidget event listener implementations.
		//
		
		/**
		 * Paint the duration bar and if allowed the
		 * hot area, effect area and control area.
		 * 
		 */
		void paintEvent (QPaintEvent*);
		
		void mousePressEvent (QMouseEvent* event);
		void mouseReleaseEvent (QMouseEvent* event);
		void mouseMoveEvent (QMouseEvent* event);
		
		void leaveEvent (QEvent*);
		void enterEvent (QEvent*);
	
		void focusInEvent (QFocusEvent*);
		
		void focusOutEvent (QFocusEvent*);
		
		void contextMenuEvent (QContextMenuEvent* event) 
		{
			if(contextMenu) {
				contextMenu->exec (QCursor::pos());
			} else {
				event->ignore ();
			}
		}
};

#endif

