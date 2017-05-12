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
 *  SimpleWrapperFactory.h
 *  
 *
 *  Created by Owen Thomas on 4/08/06.
 *  
 *
 */
#ifndef simple_wrapper_factory
#define simple_wrapper_factory

#include "SimpleBrazilPlanningWrapper.h"
#include "../model/impl/DOMDomain.h"
/**
 * A virtual factory class for creating simple
 * planning wrappers. Subclasses will provide
 * implementations.
 */
class SimpleWrapperFactory {

	public:
		virtual SimpleBrazilPlanningWrapper* createSimpleBrazilPlanningWrapper (DOMDomain& domain) = 0;
};

#endif


