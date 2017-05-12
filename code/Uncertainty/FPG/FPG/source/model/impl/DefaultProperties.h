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


