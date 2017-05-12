There are 4 doors on a continuous corridor. The robot needs to get to
the third door from the left and open it.  The robot does not know its
exact location, but can gather information from 5 observations:
Left-End, Right-End, Door, and Crash.

The movement and the observation is noisy. The step length is 1 plus
some noise ((u - 0.5) * 0.01, where u is a uniform random number in
[0, 1).)

The corridor is on the interval [-21, 21]. When you move past
the interval, you observe Crash. When you inside a radius of 5 from
either end, you observe the corresponding End. When you are inside a
radius of 1 from a door, you observe the corresponding Door.

Opening the correct door (the 1st one on the left) gives a reward of 1 and
the wrong door gives reward of -2. Crashing also gives a reward of -2. There is no movement cost.
