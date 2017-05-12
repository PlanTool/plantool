/*
 *  DOMNormalDuration.cpp
 *  
 *
 *  Created by Owen Thomas on 5/04/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include <math.h>
#include "DOMNormalDuration.h"

DOMNormalDuration::DOMNormalDuration 
	(time_t mean, time_t stdDev, DOMDocument *document)
{
	XMLCh* delay = XMLString::transcode ("delay");
	this->node = document->createElement (delay);
	XMLString::release (&delay);
	setAttribute ("type", "normal");
	setMean(mean);
	setStandardDeviation(stdDev);
}


/**
 * @author daa
 * Use a trick that originates somewhere in Numerical Recipes in C
 * @return a number sampled from 
 */
time_t DOMNormalDuration::getSample() 
{

    static int iset=0;
    static double gset;
    double fac,r,v1,v2;
    double val = -1.0;

    // Times can't be negative.
    while (val < 0.0) {
	
	if  (iset == 0) {
	    do {
		// This uses a trick which goes along the lines of:
		// If I throw two dice, the distribution of their sum
		// is gaussian. 
		v1=2.0*(random()/(double)RAND_MAX)-1.0;
		v2=2.0*(random()/(double)RAND_MAX)-1.0;
		r=v1*v1+v2*v2;
	    } while (r >= 1.0);
	    fac=sqrt(-2.0*log(r)/r);
	    gset=v1*fac;
	    iset=1;
	    val = v2*fac;
	} else {
	    iset=0;
	    val = gset;
	}
	
	
	val *= getStandardDeviation();
	val += getMean();
    }
    
    return (time_t)val;
}






