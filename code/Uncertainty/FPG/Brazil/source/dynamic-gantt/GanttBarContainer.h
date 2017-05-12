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
 *  GanttBarContainer.h
 *  
 *
 *  Created by Owen Thomas on 15/11/06.
 *  
 *
 */

#ifndef gantt_bar_container
#define gantt_bar_container

#include <QWidget>

#include "GanttBar.h"
#include "GanttChart.h"
#include "GanttBarResizer.h"

#include "../planner/BrazilEvent.h"

#include "../model/impl/DOMAction.h"
#include "../model/impl/DOMOutcome.h"

/**
 * A GanttBarContainer contains one or two GanttBars
 * representing the duratin of an action and its
 * outcome (if it has one). It also has two
 * resizer widgets, one at either end of
 * the bar or pair of bars.
 */
class GanttBarContainer : public QWidget {
	
	Q_OBJECT
	
	private:
		//The number of time units corresponding to one pixel.
		int timeScaling;
		
		//Height and vertical internal margin.
		int barHeight;
		int vMargin;
		
		//To indicate which border lines are drawn.
		bool drawTopLine, drawBottomLine, drawLeftLine, drawRightLine;
		
		//can this gantt bar container be modified by the user
		bool editable;
		
		//Events which define the action and outcome gantt bars
		//within this container
		BrazilEvent* startAction;
		BrazilEvent* endAction;
		BrazilEvent* startOutcome;
		BrazilEvent* endOutcome;
		
		//instant / post effects, linked to their
		//start / end events above.
		BrazilEvent* startActionEffect;
		BrazilEvent* endActionEffect;
		BrazilEvent* startOutcomeEffect;
		BrazilEvent* endOutcomeEffect;
		
		set<BrazilEvent*> allEvents; //Contains the above events.
		
		//The GanttBars within this container.
		GanttBar* actionBar;
		GanttBar* outcomeBar;
		
		//GanttBarResizers are widgets that allow the gantt bars
		//to be resized.
		
		GanttBarResizer* actionLeftResizer;
		
		//Null if outcomeBar != NULL
		GanttBarResizer* actionRightResizer;
		
		GanttBarResizer* outcomeRightResizer;
		
		GanttChart* chart; //The chart this container is a part of.
		
		//After a GanttBar or GanttBarResizer has been moved these
		//methods will return whether the time of the events
		//making the ganttbars differs from their geometry.
		
		//e.g. if the start actiontime has been changed in the gantt
		//chart then startActionModified will return true.
		bool startActionModified ();
		bool endActionModified ();
		bool startOutcomeModified ();
		bool endOutcomeModified ();
		
	protected slots:
	
		/**
		 * GanttBar has received a mouseEvent event, set the appropriate
		 * reziers to be framed.
		 */
		void enter (GanttBar* bar);
	
		/**
		 * The opposite of enter () (above), the mouse has left the GanttBar.
		 * set the appropriate resizer to be unframed.
		 */ 
		void leave (GanttBar* bar);
	
	public slots:
		
		/**
		 * Called when the geometry of the passed 
		 * GanttBarResizer has changed. Repositions
		 * the other widgets in the container.
		 */
		void resizerPositionModified (GanttBarResizer* bar);
		
		/**
		 * Called when the geometry of the passed
		 * GanttBar has changed. Repositions the
		 * other widgets in the container.
		 */
		void barPositionModified (GanttBar* bar);

		/**
		 * Compares the geometry of gantt bars with
		 * the underlying set of events and changes
		 * the event properties to match the geometry.
		 * Calls positionChanged on the GanttChart.
		 */
		void commitModification ();
		
		/**
		 * Sets the minimum width of the gantt bar container.
		 */
		void setWidth () {
			if(NULL != outcomeBar) {
				setMinimumWidth (outcomeBar->x() + outcomeBar->width()
					+ outcomeRightResizer->width ());
			}
			else {
				setMinimumWidth (actionBar->x() + actionBar->width ()
					+ actionRightResizer->width ());
			}
		}
		
		void nextOutcome ();
		
	protected:
		
		virtual void paintEvent (QPaintEvent*);
	
	public:
	
		GanttBarContainer (BrazilEvent* startAction, 
			int timeScaling, GanttChart* chart, QWidget* parent = NULL);
		
		/**
		 * Set the end actin event. endAction must have a type
		 * of END_ACTION. This creates and renders a GanttBar
		 * with two resizers.
		 */
		virtual void setEndAction (BrazilEvent* endAction);
		
		/**
		 * Set the start outcome event. startOutcome must be of
		 * type START_OUTCOME.
		 */
		virtual void setStartOutcome (BrazilEvent* startOutcome);
		
		/**
		 * Sets the end outcome event. endu
		 */ 
		virtual void setEndOutcome (BrazilEvent* endOutcome);
		
		virtual void setStartActionEffect (BrazilEvent* startActionEffect);
		
		virtual void setEndActionEffect (BrazilEvent* endActionEffect);
		
		virtual void setStartOutcomeEffect (BrazilEvent* startOutcomeEffect);
		
		virtual void setEndOutcomeEffect (BrazilEvent* endOutcomeEffect);
		
		virtual BrazilEvent* getStartAction () {
			return startAction;
		}
		
		virtual BrazilEvent* getEndAction () {
			return endAction;
		}
		
		virtual BrazilEvent* getStartOutcome () {
			return startOutcome;
		}
		
		virtual BrazilEvent* getEndOutcome () {
			return endOutcome;
		}
		
		/**
		 * Returns the highest time to occur of all
		 * the events in this container.
		 */
		virtual time_t getEndTime ();
		
		virtual bool contains(BrazilEvent* event) {
			return allEvents.count (event) > 0;
		}
		
		void setAlternateBackgroundColour ()
		{
			setAutoFillBackground (true);
			setBackgroundRole (QPalette::AlternateBase);
		}
		
		void removeBackgroundColour () {
			setAutoFillBackground (false);
		}
		
		virtual bool hasBackgroundColour () {
			return autoFillBackground ();
		}
		
		/*
		virtual void setAction (GanttBar& action,
			GanttBarResizer& leftResizer, GanttBarResizer& rightResizer);
			
		virtual void setOutcome (GanttBar& outcome,
			GanttBarResizer& rightResizer);
		*/
			
		bool operator< (const GanttBarContainer& other);
		
		const bool isEditable () {
			return editable;
		}
		
		
		void setEditable (bool editable) {
			this->editable = editable;
			if(NULL != actionBar) actionBar->setEditable (editable);
			if(NULL != outcomeBar) outcomeBar->setEditable (editable);
			
			if(NULL != actionLeftResizer) actionLeftResizer->setVisible (editable);
			
			if(NULL != outcomeBar && NULL != outcomeRightResizer) {
				outcomeRightResizer->setVisible (editable);
			}
			else if (NULL == outcomeBar && NULL != actionRightResizer) {
				actionRightResizer->setVisible (editable);
			}
			
		}
		const QString getLabel ()
		{
			return QString (((DOMAction*)startAction->element)->getName ());
		}
		
		void setDrawTopLine (bool drawLine) {
			this->drawTopLine = drawLine;
		}
		
		bool isDrawTopLine () {
			return drawTopLine;
		}
		void setDrawBottomLine (bool drawLine) {
			this->drawBottomLine = drawLine;
		}
		
		bool isDrawBottomLine () {
			return drawBottomLine;
		}
		void setDrawLeftLine (bool drawLine) {
			this->drawLeftLine = drawLine;
		}
		
		bool isDrawLeftLine () {
			return drawLeftLine;
		}
		void setDrawRightLine (bool drawLine) {
			this->drawRightLine = drawLine;
		}
		
		bool isDrawRightLine () {
			return drawRightLine;
		}
		
		/*
		Overriden QWidget geometry methods.
		*/
		
		virtual QSize minimumSize () {
			if(outcomeBar) 
				return QSize (outcomeBar->x() + outcomeBar->width (),
					barHeight);
			else
				return QSize (actionBar->x() + actionBar->width (), 
					barHeight);
		}	
				
		virtual QSize sizeHint () { 
			return minimumSize ();
		}
		
		virtual QSize maximumSize () {
			return sizeHint ();
		}
};

#endif
