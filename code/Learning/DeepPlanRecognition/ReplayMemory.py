#coding:utf-8
import numpy as np
import random

class ReplayMemory:
    def __init__(self, size, args):
        print 'Initializing ReplayMemory...'
        self.size = size
        self.actions = np.empty(self.size, dtype = np.uint8)
        self.rewards = np.empty(self.size, dtype = float)#np.int)
        self.states = np.empty((self.size, args.words_num, args.wordvec))
        self.terminals = np.empty(self.size, dtype = np.bool)
        self.pos_samples = np.zeros(self.size, dtype = np.uint8)
        self.priority = args.priority
        self.positive_rate = args.positive_rate
        self.reward_bound = args.reward_bound
        self.channel = args.channel
        self.dims = (args.words_num, args.wordvec)
        self.batch_size = args.batch_size
        self.count = 0
        self.current = 0

        self.prestates = np.empty((self.batch_size, self.channel) + self.dims)
        self.poststates = np.empty((self.batch_size, self.channel) + self.dims)

        
    def add(self, action, reward, state, terminal):
        assert state.shape == self.dims
        # NB! state is post-state, after action and reward
        self.actions[self.current] = action
        self.rewards[self.current] = reward
        self.states[self.current, ...] = state
        self.terminals[self.current] = terminal
        if reward > self.reward_bound:
            self.pos_samples[self.current] = 1
        else:
            self.pos_samples[self.current] = 0
        #if self.count < self.size: self.count++; else: self.count = self.size
        self.count = max(self.count, self.current + 1)  
        #the pointer circulate in a fixed length array - self.states
        self.current = (self.current + 1) % self.size

    
    def getState(self, index):
        assert self.count > 0, "replay memory is empty, use at least --random_steps 1"
        # normalize index to expected range, allows negative indexes
        index = index % self.count
        # if is not in the beginning of matrix
        if index >= self.channel - 1:
          # use faster slicing
          return self.states[(index - (self.channel - 1)):(index + 1), ...]
        else:
          # otherwise normalize indexes and use slower list based access
          indexes = [(index - i) % self.count for i in reversed(range(self.channel))]
          return self.states[indexes, ...]


    def getMinibatch(self):
        # memory must include poststate, prestate and history
        # sample random indexes
        if self.priority:
            temp_positive = sum(self.pos_samples)
            if int(self.positive_rate*self.batch_size) < temp_positive:
                temp_positive =  int(self.positive_rate*self.batch_size) 

        indexes = []
        count_pos = 0
        count_nag = 0
        count = 0
        max_circles = 1000
        while len(indexes) < self.batch_size:
            # find random index 
            while True:
                # sample one index (ignore states wraping over) 
                index = np.random.randint(self.channel, self.count - 1)
                # if wraps over current pointer, then get new one
                if index >= self.current and index - self.channel < self.current:
                    continue
                # NB! poststate (last state) can be terminal state!
                if self.terminals[(index - self.channel):index].any():
                    continue
                # otherwise use this index
                break
          
            # NB! having index first is fastest in C-order matrices
            if self.priority:
                if count < max_circles:
                    if (count_pos > temp_positive-1) and (self.rewards[index] > 1 or self.rewards[index-1] >1):
                        count += 1
                        continue
                    elif (count_nag > self.batch_size-temp_positive-1) and (self.rewards[index] <= 1 or self.rewards[index-1] <= 1):
                        count += 1
                        continue
                if self.rewards[index] > self.reward_bound:
                    count_pos += 1
                elif self.rewards[index-1] > self.reward_bound:
                    count_pos += 1
                elif self.rewards[index] <= self.reward_bound:
                    count_nag += 1
                elif self.rewards[index-1] <= self.reward_bound:
                    count_nag += 1
            
            
            self.prestates[len(indexes), ...] = self.getState(index - 1)
            self.poststates[len(indexes), ...] = self.getState(index)
            indexes.append(index)

        # copy actions, rewards and terminals with direct slicing
        actions = self.actions[indexes]  #self.actions is a numpy array, indexes is a list
        rewards = self.rewards[indexes]
        terminals = self.terminals[indexes]
        return self.prestates, actions, rewards, self.poststates, terminals
