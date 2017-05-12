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
 *  DurationDelegate.h
 *  
 *
 *  Created by Owen Thomas on 16/03/06.
 *  
 *
 */
#ifndef duration_delegate
#define duration_delegate

#include <QObject>
#include <QPainterPath>
#include "../../model/impl/DOMDuration.h"
#include "../../model/impl/DOMNormalDuration.h"
#include "../../model/impl/DOMFixedDuration.h"
#include "../../model/impl/DOMUniformDuration.h"
#include "../../model/impl/DOMExponentialDuration.h"

#include <math.h>

using namespace std;

/**
 * A DurationDelegate draws, by way of returning a QPainterPath,
 * a duration bar.
 *
 * Durations are controlled by setting a scaled width (i.e.
 * the width assuming each one duration value takes scale number
 * of pixels) and the position of a 'hot area' within the duration
 * bar.
 *
 * A DurationDelegate directly manipulates an instance of
 * a DOMDuration. The mapping from hot areas and widths to the
 * duration bar path and the underlying DOMDuration value is 
 * implemented by concrete subclasses. 
 */
class DurationDelegate
{
	protected:
		 int height;	//the height of the duration bar
		 double scale;	//the number of pixels per duration unit
		
	public:
	
		virtual ~DurationDelegate () { }
		virtual  int getScaledWidth () = 0;
	
		virtual void setScaledWidth (int width) = 0;
		
		virtual bool hasHotArea () = 0;
		
		virtual  int getHotAreaPosition () = 0;
	
		virtual void setHotAreaPosition (int position) = 0;
		
		/**
		 * Returns a duration bar, offset horizontally and  vertically
		 * by offsetX and offsetY.
		 *
		 * Each Duration bar draws the inverse cumulative density
		 * function, for the duration. Where the height of the bar
		 * at a certain point indidcates the probability that the
		 * duration will last at least that long.
		 *
		 * @return The duration bar, representing the underlying 
		 * duration. 
		 */
		virtual QPainterPath getPath (int offsetX, int offsetY) = 0;
	
		void setScale ( double scale)
		{
			this->scale = scale;
		}
	
		double getScale ()
		{
			return scale;
		}
		
		void setHeight (int height)
		{
			this->height = height;
		}
		
		int getHeight ()
		{
			return height;
		}
};

class ExponentialDurationDelegate : public DurationDelegate 
{
	private:
		DOMExponentialDuration* duration;
		
	public:
		ExponentialDurationDelegate (DOMExponentialDuration* duration, double scale, int height)
		{
			this->duration = duration;
			setScale(scale);
			setHeight(height);
		}
		
		bool hasHotArea () { return false; }
		
		int getScaledWidth ()
		{
			return (int)(scale * (log (0.1) / (0.0 - duration->getLambda())));
		}
		
		//HELP: Also is this correct?
		void setScaledWidth (int width)
		{
			int unscaledWidth = (int) (width/scale);
			duration->setLambda ( 0.0 - log (0.1) / unscaledWidth);
		}
		
		/**
		 * Exponential Durations do not have Hot Areas.
		 */
		int getHotAreaPosition() {return 0;}
		void setHotAreaPosition (int) { }
		
		/**
		 * Returns a duration bar, offset horizontally and  vertically
		 * by offsetX and offsetY respecitivly.
		 *
		 */ 
		QPainterPath getPath (int offsetX, int offsetY);
		
};

class UniformDurationDelegate : public DurationDelegate
{
	private:
		DOMUniformDuration* duration;
		
	public:
		UniformDurationDelegate (DOMUniformDuration* duration, double scale, int height)
		{
			this->duration = duration;
			setScale(scale);
			setHeight(height);
		}
		
		bool hasHotArea () { return true;}
		
		int getScaledWidth ()
		{
			return (int)( scale * duration->getMaxDuration());
		}
		
		void setScaledWidth (int width);
		
		
		int getHotAreaPosition ()
		{
			return (int)(scale * duration->getMinDuration ());
		}
		
		void setHotAreaPosition (int position);
		
		
		QPainterPath getPath (int offsetX, int offsetY);
};

class FixedDurationDelegate : public DurationDelegate
{
	private:
		DOMFixedDuration* duration;
	
	public:
		FixedDurationDelegate (DOMFixedDuration* duration, double scale, int height)
		{
			this->duration = duration;
			setScale(scale);
			setHeight(height);
		}
		
		bool hasHotArea () { return false;}
		
		int getScaledWidth ()
		{
			return (int)(scale  * duration->getDuration());
		}
		
		void setScaledWidth (int width)
		{
			if(width <= 0) return;
			duration->setDuration ( (int) (width / scale)); 
		}
		
		int getHotAreaPosition () {return 0;}
		void setHotAreaPosition (int) { }
		
		QPainterPath getPath (int offsetX, int offsetY);
		
};
class NormalDurationDelegate : public DurationDelegate
{
	private:
		DOMNormalDuration* duration;
		
	public:
		NormalDurationDelegate (DOMNormalDuration* duration, double scale, int height)
		{
			this->duration = duration;
			setScale (scale);
			setHeight(height);
		}
		
		bool hasHotArea () { return true; }
		
		/**
		 * The width represents the point, beyond the mean, where 3 * standard 
		 * deviation is reached.
		 */
		int getScaledWidth ()
		{
			return (int)(3 * duration->getStandardDeviation() * scale + duration->getMean() * scale);
		}
		
		void setScaledWidth (int width);
		
		
		int getHotAreaPosition ()
		{
			return (int)(duration->getMean() * scale);
		}
		
		void setHotAreaPosition (int position);
		
		
		QPainterPath getPath (int offsetX, int offsetY);
};

/**
 * Provides a factory method for returning a DurationDelegate 
 * for a specific DOMDuration.
 * 
 * Create a new DurationDelegate concrete subclass specific to the
 * name of the passed in duration. This then controls the value of
 * the passed in DOMDuration.
 *
 */
class DurationDelegateFactory
{
	public:
		static DurationDelegate* createDurationDelegate (DOMDuration* duration, int height, double scale)
		{
			if(strcmp(duration->getName(),"Normal") == 0)
				return new NormalDurationDelegate ((DOMNormalDuration*)duration, scale, height);	
			else if(strcmp(duration->getName(),"Fixed") == 0)
				return new FixedDurationDelegate ((DOMFixedDuration*)duration, scale, height);
			else if(strcmp(duration->getName(),"Uniform") == 0)
				return new UniformDurationDelegate ((DOMUniformDuration*)duration, scale, height);
			else if(strcmp(duration->getName(), "Exponential") == 0)
				return new ExponentialDurationDelegate ((DOMExponentialDuration*)duration,scale, height);
			
			throw invalid_argument ("Unknown DOMDuration subclass");
		}
};

#endif

