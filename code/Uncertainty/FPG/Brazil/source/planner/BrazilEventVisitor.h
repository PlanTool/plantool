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

#include"BrazilEvent.h"
#include"DOMEffectSet.h"
#include"DOMOutcome.h"
#include"DOMAction.h"

class BrazilEventVisitor {

public:
    
    virtual ~BrazilEventVisitor() {};
    
    /**
     * Calling this will invoke a handler for this kind of event.
     */
    void visit (BrazilEvent* event);
    
    
 protected:
		
    /**
     * Each of these visitXXX methods is called by visit up top.
     * They're protected because whatever is giving events to the event
     * visitor shouldn't query the sub-type of event, it should just call
     * visit.
     *
     * Also in each event->element is the same object as the second parameter to the
     * method call. 
     */

	
    virtual void visitStartAction (BrazilEvent* event, DOMAction* action) = 0;
    virtual void visitEndAction (BrazilEvent* event, DOMAction* action) = 0;
    virtual void visitStartOutcome (BrazilEvent* event, DOMOutcome* outcome) = 0;
    virtual void visitEndOutcome (BrazilEvent* event, DOMOutcome* outcome) = 0;
    virtual void visitEffect (BrazilEvent* event, DOMEffectSet* effect) = 0;
};
