/*
 *  DurationDelegate.cpp
 *  
 *
 *  Created by Owen Thomas on 5/04/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "DurationDelegate.h"

QPainterPath ExponentialDurationDelegate::getPath (int offsetX, int offsetY)
{
	QPainterPath path;
	
	//Move to top left position.
	path.moveTo (offsetX, offsetY);
	
	//Curve down to Far right bottom position
	path.cubicTo(
				 offsetX,
				 offsetY + getHeight (),
				 (offsetX + getScaledWidth())/2,
				 offsetY + getHeight(),
				 offsetX + getScaledWidth(),
				 offsetX + getHeight());
	
	
	//bottom left
	path.lineTo (offsetX, offsetY + getHeight());
	
	//top left
	path.lineTo (offsetX, offsetY);
	
	return path;
}

/**
 * The scaled width of a UniformDuration represents the 
 * maximum duration value of the uniform duration.
 *
 * When it is altered we need to move the minimum duration
 * down (or up) by the same amount, taking care to ensure that
 * neither falls beneath 0.
 */
void UniformDurationDelegate::setScaledWidth (int width)
{
	if(width <=0 ) return;
	
	int unscaledWidth = (int) (width/scale);
	int delta = duration->getMaxDuration() - unscaledWidth;
	
	//If the new max duration is less than the minimum
	//duration, set both to the new max duration.
	if(unscaledWidth <= duration->getMinDuration())
	{
		duration->setMinDuration (unscaledWidth);
		duration->setMaxDuration(unscaledWidth);
	}
	
	else
	{
		duration->setMaxDuration(unscaledWidth);
		
		//If the minimum duration would be moved to less
		//than zero, set it to 0.
		if(duration->getMinDuration() - delta <= 0)
		{
			duration->setMinDuration(0);
		}
		else
		{
			//Standard case, just move the minimum duration by the same
			//amount as the maximum duration.
			duration->setMinDuration(duration->getMinDuration() - delta);
		}
	}
}

/**
 * Sets the hot area position - the minimum duration value - for
 * the Uniform Duration.
 *
 * If the new minimum duration is greater than or equal to the
 * maximum duration, set both to be the new minimum duration. 
 */
void UniformDurationDelegate::setHotAreaPosition (int position)
{
	if(position <= 0) return;
	int unscaledPosition = (int) (position / scale);
	if(unscaledPosition >= duration->getMaxDuration())
	{
		duration->setMaxDuration (unscaledPosition);
	}
	duration->setMinDuration (unscaledPosition);
}

QPainterPath UniformDurationDelegate::getPath (int offsetX, int offsetY)
{
	QPainterPath path;
	
	//Far left top position
	path.moveTo (offsetX, offsetY);
	
	//Hot area position
	path.lineTo (offsetX + getHotAreaPosition(), offsetY);
	
	//Diagonal line down to the bottom right hand corner
	path.lineTo (offsetX + getScaledWidth(), offsetY + getHeight());
	
	//Far left bottom position
	path.lineTo (offsetX, offsetY + getHeight());
	
	//Back to far left top position
	path.lineTo (offsetX, offsetY);
	
	return path;
}

QPainterPath FixedDurationDelegate::getPath (int offsetX, int offsetY)
{
	QPainterPath path;
	
	//Far left top position
	path.moveTo (offsetX, offsetY);
	
	//Far right top position
	path.lineTo (offsetX + getScaledWidth(), offsetY);
	
	//Far right bottom position
	path.lineTo (offsetX + getScaledWidth (), offsetY + getHeight ());
	
	//Far left bottom position
	path.lineTo (offsetX, offsetY + getHeight());
	
	//Back to Far left top position
	path.lineTo (offsetX,offsetY);
	
	return path;
}

/**
 * Sets the scaled width of a Normal Duration. 
 * Moving the scaled width, the point three standard 
 * deviations from the mean updates the mean of the
 * duration, so that the standard deviation remains the same.
 */
void NormalDurationDelegate::setScaledWidth (int width)
{
	int oldWidth = getScaledWidth ();
	//standard deviation remains unchanged.
	//this sets the mean.
	
	int deltaWidth = width - getScaledWidth ();
	int unScaledDeltaWidth = (int) (deltaWidth / scale);
	
	duration->setMean (duration->getMean() + unScaledDeltaWidth );	
	if(getHotAreaPosition () < 0) {
		setScaledWidth (oldWidth);
		return;
	}
}

/**
 * Sets the Hot Area Position of the Normal Duration.
 *
 * The distance from the Hot Area Position to the end point
 * of the duration bar is three standard deviations. Hence, updating
 * the Hot Area Position updates the standard deviation of the duration.
 */
void NormalDurationDelegate::setHotAreaPosition (int position)
{
	if(position < 0 ) return;
	
	
	//If the hot area has been moved beyond the 
	if(position > getScaledWidth())
	{
		setScaledWidth (position);
	}
	//If Mean increases, standard deviation must decrease
	//If Mean decreases, standard deviation must decrease.
	int deltaPosition = getHotAreaPosition() - position;
	int unScaledDeltaPosition = (int) (deltaPosition / scale);
	duration->setMean (duration->getMean () - unScaledDeltaPosition);
	
	
	int update = duration->getStandardDeviation() + unScaledDeltaPosition / 3;
	
	//Check to make sure the number hasn't wrapped around to a large negative.
	if(update > 0)
		duration->setStandardDeviation (duration->getStandardDeviation() + unScaledDeltaPosition/3);
	
}
		
QPainterPath NormalDurationDelegate::getPath (int offsetX, int offsetY)
{
	//We use a cubic bezier path to draw the duration bar.
	//This draws from the point three standard deviations to the
	//top left of the mean down to three standard deviations to
	//the bottom right of the mean.
	//There are two control points, the first on the top of the bar
	//that pulls the curve to the right, the second at the bottom
	//of the bar that pulls the curve to the left.
	
	QPainterPath path;
	
	//Far left top position.
	path.moveTo (offsetX + scale * (duration->getMean() - 
									3 * duration->getStandardDeviation()),offsetY);
	
	//Curve down to Far right bottom position
	path.cubicTo(
				 offsetX + scale * (duration->getMean () + duration->getStandardDeviation()),
				 offsetY,
				 offsetX + scale * (duration->getMean () - duration->getStandardDeviation()),
				 offsetY + getHeight (),
				 offsetX + getScaledWidth (),
				 offsetY + getHeight ());
	
	//Draw to the bottom left position.
	path.lineTo (offsetX, height + offsetY);
	
	//Back to top left.
	path.lineTo (offsetX, offsetY);
	
	//Antialiasing causes parts of the line border to be shaded out.
	//We have to draw in additional pixels, proportional to the standard 
	//deviation to overcome this. 
	int additionalPixels = (int)((scale * duration->getStandardDeviation()) / 14);
	
	path.lineTo (additionalPixels + offsetX + scale * (duration->getMean() - 
													   3 * duration->getStandardDeviation()), offsetY);
	
	return path;
}


