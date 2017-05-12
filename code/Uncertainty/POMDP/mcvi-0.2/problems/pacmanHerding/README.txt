Collaborative Search and Capture: In this problem, a group of
crocodiles had escaped from its enclosure into the environment and two
robotic agents have to collaborate to hunt down and capture the
crocodiles. Both agents are centrally controlled and each agent can
make a one step move in one of the four directions (north, south, east
and west) or stay still at each time instance.

There are twelve crocodiles in the environment. At every time
instance, each crocodile moves to a location furthest from the agent
that is nearest to it with a probability 1 − p (p = 0.05 in the
experiments). With a probability p, the crocodile moves randomly. A
crocodile is captured when it is at the same location as an agent.

The agents do not know the exact location of the crocodiles, but each
agent knows the number of crocodiles in the top left, top right,
bottom left and bottom right quadrants around itself from the noise
made by the crocodiles. Each captured crocodile gives a reward of 10,
while movement is free.

Source:
  "Monte Carlo Value Iteration with Macro-Actions" (Lim et al., NIPS 2011)
