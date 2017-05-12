/*
 *  ProbabilityController.cpp
 *  
 *
 *  Created by Owen Thomas on 23/10/06.
 *  Copyright 2006 __MyCompanyName__. All rights reserved.
 *
 */

#include "ProbabilityController.h"

void ProbabilityController::setProbability (DOMOutcome* outcome,
	double probability) {
	
	//Outcome is not contained in probabilistic.
	if(probabilistic->getOutcomes().count (outcome) == 0) 
		throw std::invalid_argument ("outcome not valid.");
	
	//Just stop if there's only one outcome.
	if(probabilistic->getOutcomes().size () == 1) return;
		map<DOMOutcome*, double> newProbabilities = probabilistic->getOutcomes ();
	
	//difference between current probability and new probability.
	//This is the probability 'mass' that needs to be distributed to
	//other outcomes.
	double delta = 
		(newProbabilities [outcome] - probability);;
		
	//The amount each additional outcome should get.
	//double deltaNormalised = delta / (probabilistic->getOutcomes().size () - 1);
	
	//The amount of remaining probaiblity this needs to be distributed 
	//across.
	double remainingProbability = 1.0 - (outcome->getProbability ());
	
	map<DOMOutcome*, double>::iterator it;
	for(it = probabilistic->getOutcomes().begin (); 
		it != probabilistic->getOutcomes().end (); it++) {
		
		if(it->first == outcome) continue;

		//Each outcome recieves an amount of probability based on it's
		//current probability. Outcomes with higher probabilities get
		//bigger adjustments.
		double weight = it->second / remainingProbability;
		
		//The new probability is the current probability, added to the
		//weighting by the amount of change.
		newProbabilities [it->first] = it->second + (weight * delta);
	}
	
	newProbabilities [outcome] = probability;
	
	set<DOMOutcome*> ignoreSet;
	ignoreSet.insert (outcome);
	roundOutcomes (ignoreSet, newProbabilities);
	
	probabilistic->setOutcomes (newProbabilities);
}

/* 
 * Remove outcome and adjust the probabilities of the remaining outcomes up.
 */
void ProbabilityController::removeOutcome (DOMOutcome* outcome) {
	
	map<DOMOutcome*, double> newProbabilities = probabilistic->getOutcomes ();
	
	double delta = probabilistic->getOutcomes () [outcome] / (newProbabilities.size()-1);
	
	newProbabilities.erase (outcome);
	
	//adjust probabilities.
	map<DOMOutcome*, double>::iterator it;
	for(it = probabilistic->getOutcomes ().begin (); 
		it != probabilistic->getOutcomes ().end (); it++) {
		
		if(it->first == outcome) continue;
		
		
		newProbabilities [it->first] = it->second + delta;
		
	}
	
	probabilistic->setOutcomes (newProbabilities);
}

void ProbabilityController::addOutcome (DOMOutcome* outcome) {
	map<DOMOutcome*, double> newProbabilities = probabilistic->getOutcomes ();
	
	double probability = 1.0 / (newProbabilities.size () + 1);
	
		newProbabilities [outcome] = probability;
	
	//adjust probabilities.
	map<DOMOutcome*, double>::iterator it;
	for(it = probabilistic->getOutcomes ().begin (); 
		it != probabilistic->getOutcomes ().end (); it++) {
		
		if(it->first == outcome) continue;
		
		newProbabilities [it->first] = it->second - probability * it->second;
	}
	
	probabilistic->setOutcomes (newProbabilities);
}

/**
 * Temporary until I put in float handling code.
 */
void ProbabilityController::roundOutcomes (set<DOMOutcome*>& ignoreSet,
	map<DOMOutcome*, double>& outcomes) {

	map<DOMOutcome*, int> rounded;
	
	map<DOMOutcome*, double>::iterator it;
	int sum = 0;
	for(it = outcomes.begin (); it != outcomes.end(); it++) {
		
		rounded [it->first] = (int) (100 * it->second);
		
		if(!ignoreSet.count (it->first)) outcomes [it->first] = rounded [it->first] / 100.0;
		
		sum += rounded [it->first];
	}
	
	int remainder = 100 - sum;
	while(remainder > 0) {
		for(it = outcomes.begin (); it != outcomes.end () && remainder > 0; it ++) {
			if(ignoreSet.count (it->first)) continue;
			outcomes [it->first] += 0.01;
			remainder -= 1;
		}
		it = outcomes.begin ();
	}
}
