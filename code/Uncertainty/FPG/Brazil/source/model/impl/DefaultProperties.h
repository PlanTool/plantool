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


/*
 * This file simply contains just a bunch of defaults that are
 * included into DOMPropertySet.cpp All critical properties should
 * have a hard wired, last resort default here.
 */
d->addProperty("planner discount", 0.95);
d->addProperty("planner step size", 0.001);
d->addProperty("planner epoch steps", 10000);
d->addProperty("planner max makespan", 1000000);
d->addProperty("planner max time", 0);
d->addProperty("planner stochastic notify period", 100);
d->addProperty("planner stochastic evaluation steps", 10000);

d->addProperty("gui stats axis width", 2);
d->addProperty("gui stats axis pad", 0.07);
d->addProperty("gui stats success colour", "green");
d->addProperty("gui stats failure colour", "red");
d->addProperty("gui stats axis y-ticks", 5);

d->addProperty("max states in stats view", 100);

d->addProperty("gui HotAreaWidget colour", "crimson");
d->addProperty("gui HotAreaWidget radius", 7);

