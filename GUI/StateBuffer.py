#coding:utf-8
import numpy as np

class state_buffer:
  """
  While ReplayMemory could have been used for fetching the current state,
  this also means that test time states make their way to training process.
  Having separate state_buffer ensures that test data doesn't leak into training.
  """
  def __init__(self, args):
    self.channel = args.channel
    self.dims = (args.words_num, args.wordvec)
    self.batch_size = args.batch_size
    self.buffer = np.zeros((self.batch_size, self.channel) + self.dims)

  def add(self, observation):#
    assert observation.shape == self.dims
    self.buffer[0, :-1] = self.buffer[0, 1:]
    self.buffer[0, -1] = observation

  def getState(self):#
    return self.buffer[0]

  def getStateMinibatch(self):
    return self.buffer

  def reset(self):#
    self.buffer *= 0

if __name__ == '__main__':
  import argparse
  parser = argparse.ArgumentParser()
  parser.add_argument("--words_num", type=int, default=200, help="")
  parser.add_argument("--wordvec", type=int, default=101, help="")
  parser.add_argument("--channel", type=int, default=4, help="")
  parser.add_argument("--batch_size", type=int, default=32, help="Batch size for neural network.")
  parser.add_argument("--loops", type=int, default=10000, help="Number of loops in testing.")
  args = parser.parse_args()

  import numpy as np
  mem = state_buffer(args)
  for i in xrange(args.loops):
    mem.add(np.zeros((args.wordvec, args.words_num)))
    if i >= args.channel:
      state = mem.getState()
      batch = mem.getStateMinibatch()