#coding:utf-8
import time
import random
import numpy as np
from StateBuffer import state_buffer

class Agent:
    def __init__(self, environment, replay_memory, deep_q_network, args):
        print 'Initializing the Agent...'
        self.env = environment
        self.mem = replay_memory
        self.net = deep_q_network
        self.buf = state_buffer(args)
        self.num_actions = args.num_actions
        self.words_num = args.words_num
        self.batch_size = args.batch_size

        self.exploration_rate_start = args.exploration_rate_start
        self.exploration_rate_end = args.exploration_rate_end
        self.exploration_decay_steps = args.exploration_decay_steps
        self.exploration_rate_test = args.exploration_rate_test
        self.total_train_steps = args.start_epoch * args.train_steps

        self.train_frequency = args.train_frequency
        self.train_repeat = args.train_repeat
        self.target_steps = args.target_steps
        self.test_flag = False
        self.test_text_num = args.test_text_num

        self.steps = 0  #use to decrease the reward during time steps 
        self.tag_word_num = 0
        self.action_label = args.action_label
        self.random_play = args.random_play

        
    def _restartRandom(self):
        self.env.restart()
        self.steps = 0


    def _restartRandom_test(self):
        #NB!! Remember to restart the environment and parameters!
        self.env.restart_test()
        self.steps = 0


    def _explorationRate(self):
        # calculate decaying exploration rate
        if self.total_train_steps < self.exploration_decay_steps:
            return self.exploration_rate_start - self.total_train_steps * \
            (self.exploration_rate_start - self.exploration_rate_end) / self.exploration_decay_steps
        else:
            return self.exploration_rate_end
 

    def step(self, exploration_rate):
        # exploration rate determines the probability of random moves
        if random.random() < exploration_rate:
            ind = np.random.randint(2)
            action = 2*self.tag_word_num + ind
            assert action >= 0 and action < self.num_actions
            #print 'take random action',ind

        else:
            # otherwise choose action with highest Q-value
            states = self.buf.getStateMinibatch()
            # for convenience getStateMinibatch() returns minibatch
            # where first item is the current state

            qvalues = self.net.predict(states)
            #print 'qvalues[0] in step():',qvalues[0]
            max_q_index = np.argmax(qvalues[0])
            assert len(qvalues[0]) == 2


            # choose highest Q-value of first state
            action = 2*self.tag_word_num + max_q_index
            assert action >= 0 and action < self.num_actions
            #print 'take max qvalue action',max_q_index

            

        # perform the action  
        reward = self.env.act(action)
        state = self.env.getState()
        terminal = self.env.isTerminal()
        
        self.steps += 1
        #print 'o_reward: %f'%reward
        if not terminal:
            #decrease the reward with time steps
            reward -= abs(reward)*self.steps/(1.5*self.num_actions)  
        else:
            self.steps = 0
            reward += 2   #give a bonus to the terminal actions
        
        # add state to buffer
        self.buf.add(state)

        # restart the game if over
        if terminal and not self.test_flag:
            self._restartRandom()

        return action, reward, state, terminal

 
    def train(self, train_steps, epoch = 0):
        # initialize the environment
        #if epoch == 0:
        #    self.env.train_init()
        # play given number of steps
        for i in xrange(train_steps):
            # perform game step
            print '\n\nepoch: %d  Training step: %d'%(epoch,i+1)
            #print '\nself.steps: %d'%(self.steps+1)
            if self.tag_word_num >= self.words_num:
                self.tag_word_num = 0

            if self.random_play:
                action, reward, state, terminal = self.step(1)
            else:
                action, reward, state, terminal = self.step(self._explorationRate())
                #print 'n_reward:',reward
                #print 'action:',action
                #print 'terminal:',terminal
                self.mem.add(action, reward, state, terminal)

                # Update target network every target_steps steps
                if self.target_steps and i % self.target_steps == 0:
                    #print '333333333333333   Updating target network   333333333333333'
                    self.net.update_target_network()

                # train after every train_frequency steps
                if self.mem.count > self.mem.batch_size and i % self.train_frequency == 0:
                    # train for train_repeat times
                    for j in xrange(self.train_repeat):
                        # sample minibatch
                        minibatch = self.mem.getMinibatch()
                        # train the network
                        self.net.train(minibatch, epoch)
            
            # increase number of training steps for epsilon decay
            self.tag_word_num += 1
            self.total_train_steps += 1
            #time.sleep(0.5)
            #if self.steps >= self.batch_size+3 :
             #   assert 1==0

            
    def test(self, test_steps, num_texts, f1):
        # play given number of steps
        cnt = 0
        ras = 0
        tas = 0
        tta = 0
        total_tag_right = 0  ###
        total_tag_wrong = 0  ###
        accuracy = 0         ###
        twords = 0
        self.tag_word_num = 0
        self.test_flag = True
        self.env.test_init()
        
        for i in xrange(num_texts):
            self._restartRandom_test()
            f1.write('\n\nself.test_text_num: '+str(self.env.text_num))
            for j in xrange(test_steps):
                # perform game step
                print '\nTesting text: %d  step %d' % (i,j)
                if self.tag_word_num >= self.words_num:
                    self.tag_word_num = 0

                if self.random_play:
                    a,r,s,t = self.step(1)
                else:
                    a,r,s,t = self.step(self.exploration_rate_test)
                self.tag_word_num += 1
                #print 'action:',a
                #print 'reward:',r
                #print 'terminal:',t
                if t:
                    text_vec_tags = self.env.text_vec[:,-1]
                    state_tags = self.env.state[:,-1]
                    sum_tags = sum(text_vec_tags)
                    #print "text_vec_tags",text_vec_tags
                    #print 'state_tags',state_tags
                    count = 0
                    right_actions = 0
                    tag_actions = 0
                    total_actions = 0
                    right_tag = 0  ##
                    wrong_tag = 0  ##
                    
                    total_words = self.num_actions/2
                    temp_words = self.env.text_length
                    if temp_words > total_words:
                        temp_words = total_words
                    for t in text_vec_tags:
                        if t == self.action_label:
                            total_actions += 1

                    f1.write('\nText:'+str(i))
                    f1.write('\ntotal words: %d\n'%temp_words)
                    print '\ntotal words: %d\n'%temp_words
                    #f1.write('\ntext_vec:\n')
                    #f1.write(str(self.env.text_vec[:,-1]))
                    #f1.write('\nstates:\n')
                    #f1.write(str(self.env.state[:,-1])) 


                    for s in xrange(temp_words):
                        if state_tags[s] == 0:
                            count += 1
                        elif state_tags[s] == self.action_label:
                            tag_actions += 1
                            if text_vec_tags[s] == state_tags[s]:
                                right_actions += 1
                        if text_vec_tags[s] == state_tags[s]:
                            right_tag += 1
                        else:
                            wrong_tag += 1
                    tag_right_rate = float(right_tag)/temp_words
                    twords += temp_words

                    if total_actions > 0:
                        recall = float(right_actions)/total_actions
                    else:
                        recall = 0
                    if tag_actions > 0:
                        precision = float(right_actions)/tag_actions
                    else:
                        precision = 0
                    rp = recall + precision
                    if rp > 0:
                        F_value = (2.0*recall*precision)/(recall+precision)
                    else:
                        F_value = 0
                    f1.write('\nWords left: %d'%count)
                    f1.write('\nAcions: %d'%total_actions)
                    f1.write('\nRight_actions: %d'%right_actions) 
                    f1.write('\nTag_actions: %d'%tag_actions)   
                    f1.write('\nActions_recall: %f'%recall)
                    f1.write('\nActions_precision: %f'%precision)
                    f1.write('\nF_measure: %f'%F_value)
                    print '\nText: %d'%i
                    print '\nWords left: %d'%count
                    print 'Acions: %d'%total_actions
                    print 'Right_actions: %d'%right_actions    
                    print 'Tag_actions: %d'%tag_actions  
                    print 'Actions_recall: %f'%recall
                    print 'Actions_precision: %f'%precision 
                    print 'F_measure: %f'%F_value
                    f1.write('\nright_tag: %d\nwrong_tag: %d\ntag_right_rate: %f\n'%\
                        (right_tag,wrong_tag,tag_right_rate))
                    print '\nright_tag: %d\nwrong_tag: %d\ntag_right_rate: %f\n'%\
                    (right_tag,wrong_tag,tag_right_rate)

                    break


            total_tag_right += right_tag
            total_tag_wrong += wrong_tag                    
            cnt += count
            ras += right_actions
            tta += tag_actions
            tas += total_actions       


        accuracy = float(total_tag_right)/twords
        if tas > 0:
            average_recall = float(ras)/tas
        else:
            average_recall = 0
        if tta > 0:
            average_precision = float(ras)/tta
        else:
            average_precision = 0
        arp = average_recall + average_precision
        if arp > 0:
            ave_F_value = (2*average_recall*average_precision)/(average_recall+average_precision)
        else:
            ave_F_value = 0
        f1.write('\nTotal words left: %d'%cnt)
        f1.write('\nTotal actions: %d'%tas)
        f1.write('\nTotal right_actions: %d'%ras)
        f1.write('\nTotal tag_actions: %d'%tta)
        f1.write('\nAverage_actions_recall: %f'%average_recall)
        f1.write('\nAverage_actions_precision: %f'%average_precision)
        f1.write('\nAverage_F_measure: %f'%ave_F_value)
        print '\nTotal words left: %d'%cnt
        print 'Total actions: %d'%tas
        print 'Total right_actions: %d'%ras 
        print 'Total tag_actions: %d'%tta
        print 'Average_actions_recall: %f'%average_recall
        print 'Average_actions_precision: %f'%average_precision
        print 'Average_F_measure: %f'%ave_F_value 
        f1.write('\ntotal_tag_right: %d total_tag_wrong: %d  accuracy: %f\n'%\
            (total_tag_right,total_tag_wrong,accuracy))
        print '\ntotal_tag_right: %d total_tag_wrong: %d  accuracy: %f\n'%\
        (total_tag_right,total_tag_wrong,accuracy)
       

    def test_one(self, text_dir):
        print '\nTesting text: %s\n' % text_dir 
        #print '\nTesting text: %s\n' % text_dir
        self.env.test_one_init(text_dir)
        action_sequence = []
        self.tag_word_num = 0
        self.test_flag = True
        
        for i in xrange(self.words_num):
            action, reward, state, terminal = self.step(0)
            self.tag_word_num += 1
        state_tags = self.env.state[:,-1]
        
        for j, tag in enumerate(state_tags):
            if j >= len(self.env.words):
                break
            if int(tag) == int(self.action_label):
                action_sequence.append(self.env.words[j])

        return self.env.words, action_sequence, state_tags


    def test_one_db(self, table, num):
        print '\nTesting table: %s,  text_num: %d\n'%(table, num) 
        #print '\nTesting text: %s\n' % text_dir
        self.env.test_one_db_init(table, num)
        all_words = []
        all_action_sequence = []
        all_state_tags = []
        self.test_flag = True
        
        for k in range(self.env.test_db_texts_num):
            if k != 0:
                self.env.test_one_db_restart()
            action_sequence = []
            self.tag_word_num = 0
            self.steps = 0
            for i in xrange(self.words_num):
                action, reward, state, terminal = self.step(0)
                self.tag_word_num += 1
            state_tags = self.env.state[:,-1]
            all_state_tags.extend(state_tags)
            
            for j, tag in enumerate(state_tags):
                if j >= len(self.env.words):
                    break
                if int(tag) == int(self.action_label):
                    action_sequence.append(self.env.words[j])
            all_action_sequence.extend(action_sequence)
            all_words.extend(self.env.words)

        return all_words, all_action_sequence, all_state_tags