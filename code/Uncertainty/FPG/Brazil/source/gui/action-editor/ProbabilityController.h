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
 *  ProbabilityController.h
 *  
 *
 *  Created by Owen Thomas on 23/10/06.
 *  
 *
 */

/**
 * Contains rules for updating the probabilities of a pool
 * of outcomes based on changes to a single outcome's 
 * probability. 
 *
 * Used by OutcomeWidget currently.
 */
 
 #ifndef probability_controller
 #define probability_controller
 
 #include "../../model/impl/DOMProbabilistic.h"
class ProbabilityController {

	private:
		DOMProbabilistic* probabilistic;
	
	protected:
		void roundOutcomes (set<DOMOutcome*>& ignoreSet,
			map<DOMOutcome*, double>& outcomes);
		
	public:
		
		ProbabilityController (DOMProbabilistic* probabilistic) {
			this->probabilistic = probabilistic;
		}
		
		void setProbability (DOMOutcome* outcome, double probability);
		
		void addOutcome (DOMOutcome* outcome);
		
		void removeOutcome (DOMOutcome* outcome);
};

#endif
