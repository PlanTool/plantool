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
 * $Id: BrazilState.cpp 80 2006-06-14 05:18:04Z daa $ 
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

#ifndef _PlanningListener__h
#define _PlanningListener__h

/**
 * A collection of messages to pass back to the GUI about events *during* simulatation of many plan trials.
 * At the moment just used to pass the completed State at the end of each trial. Used to render the GUI and
 * gather statistics.
 */
class PlanningListener {

 public:

    /**
     * Stop compiler whinging
     */
    virtual ~PlanningListener() {};

    /**
     * Planner has computed the "median" state for GANTT chart display
     * State will be corrupted after return, so listener must be
     * finished with it by return.
     */
    virtual void medianState(BrazilState* state) = 0;

    /**
     * Planner has computed one stochastic execution of the plan, and
     * passes that plan to the listener for it to compute whatever
     * statistics it desires.  State will be corrupted after return,
     * so listener must take a copy or extract relevant statistics
     * immediately.
     */
    virtual void stochasticState(BrazilState* state) = 0;

    /**
     * This will be called by the planner to occasionally (every few
     * minutes) notify the GUI (or anything else) that it should make
     * a copy of the parameters. This is so that ifa long planning
     * run is interrupted, all the effort is not lost. 
     */
    virtual void parameterBackup(std::vector<double> params) = 0;
    
};
#endif
