#coding:utf-8
import re
import time
import nltk
import random
import logging
import numpy as np
from StateBuffer import state_buffer
from nltk.parse.stanford import StanfordDependencyParser, StanfordParser
from nltk.tag import StanfordPOSTagger

class Agent:
    def __init__(self, environment, replay_memory, deep_q_network, args):
        logging.debug( 'Initializing the Agent...' )
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
            return self.exploration_rate_start - self.total_train_steps * (self.exploration_rate_start - self.exploration_rate_end) / self.exploration_decay_steps
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
        if epoch == 0:
            self.env.train_init()
        # play given number of steps
        for i in xrange(train_steps):
            # perform game step
            if (i+1)%100 == 0:
                logging.debug( '\n\nepoch: %d  Training step: %d'%(epoch,i+1) )
            if self.tag_word_num >= self.words_num:
                self.tag_word_num = 0

            if self.random_play:
                action, reward, state, terminal = self.step(1)
            else:
                action, reward, state, terminal = self.step(self._explorationRate())
                self.mem.add(action, reward, state, terminal)

                # Update target network every target_steps steps
                if self.target_steps and i % self.target_steps == 0:
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



    def RegexpParser_init(self):
        grammar=r"""VP:{<NN|NNS|PRP|NNP|NNPS><RB>?<MD><RB>?<VB><RB>?<VBD|VBG|VBN>}
        VP:{<NN|NNS|PRP|NNP|NNPS><RB>?<VBP><RB>?<VBN><RB>?<VBD|VBN>}
        VP:{<NN|NNS|PRP|NNP|NNPS><RB>?<VBZ><RB>?<VBD|VBG|VBN>}
        VP:{<VB|VBD|VBG|VBN|VBP><RB>?<PRP\$|DT>?<VBN|VBD|JJ>?<NN|NNS|NNP|NNPS>+}
        VP:{<VB|VBD|VBG|VBN|VBP><RB>?<PRP\$|DT>?<VBN|VBD|JJ>?<PRP>}
        VP:{<VB|VBD|VBG|VBN|VBP><IN><DT>?<NN|NNS>+}
        VP:{<VB|VBD|VBG|VBN|VBP><IN>?<DT>?<CD>?<VBN|VBD|JJ>?<NN|NNS|NNP|NNPS>+}
        VP:{<VB|VBD|VBG|VBN|VBP><IN>?<DT>?<CD>?<VBN|VBD|JJ>?<PRP>}
        VP:{<VB|VBD|VBG|VBN|VBP><IN><VBN|VBD><NN|NNS>+}
        """#被动句中去掉了WDT(Wh-determiner)
        #主动句中只考虑了动词原形的//VP:{<VB><DT>?<VBN|VBD|JJ>?<NN|NNS|NNP|NNPS>+}只有一个代词的时候不重复
        #something can be done
        #something has been done
        #something is/was done
        #do/did something, Set your oven, put the mixture in the oven.
        #Ensure the twists have extended above the plate by
        #Get a ten inch pie plate, add in the hash browns
        #add in something
        #
        self.cp = nltk.RegexpParser(grammar)  #生成语法块
        self.pron_tag = ["it", "they", "them", "this", "that", "these", "those"]
        self.noun_tag = ['NN','NNS','NNP','NNPS','PRP']
        self.verb_tag = ['VB','VBD','VBG','VBN','VBP']


    def regexp_find_vp(self, sent):
        temp_myvp = {}
        obj = []
        result = self.cp.parse(sent)  
        b = re.findall(r'\(VP.+\)',str(result)) 
        for c in b:
            #d = re.findall(r'[\w|\-]+\$?',c)#所有的字母以及符号$
            d = re.findall(r'[A-Za-z-\']+\$?|\d+/\d+',c)#所有的字母以及符号$，don't，20/20这些类型的
            if len(d) % 2 == 0:#去除分错的
                continue
            #if i ==1454:  print d
            #print d[1],d[len(d)-2]
            if d[2] in self.verb_tag and d[3] in ['if']:
                continue
            if d[2] in self.verb_tag:#主动句，第一个单词是动词
                if d[4] == 'NN' and d[3] == 't':#排除don't等缩写的影响
                    pass
                else:
                    j = 4
                    obj.append(d[1])
                    while j < len(d):
                        if d[j] in self.noun_tag:
                            #print 'd[j]: ',d[j]
                            obj.append(d[j-1])
                        j += 2
                    #print 'dobj:',obj
                    temp_myvp[obj[0]] = obj[1]
                    obj = []
            elif d[2] in self.pron_tag:#被动句，第一个单词是名词性主语
                obj = [d[len(d)-2],d[1]]
                #print 'idobj:',obj
                temp_myvp[obj[0]] = obj[1]
                obj = []
        return temp_myvp


    def StanfordParser_init(self):
        pos_tagger_jar = '/home/fengwf/stanford/postagger/stanford-postagger.jar'
        pos_tagger_models = '/home/fengwf/stanford/postagger/models/english-bidirectional-distsim.tagger'
        path_to_jar = '/home/fengwf/stanford/stanford-corenlp-3.7.0.jar'
        path_to_models_jar = '/home/fengwf/stanford/english-models.jar'
        self.dep_parser = StanfordDependencyParser(
            path_to_jar=path_to_jar, path_to_models_jar=path_to_models_jar)
        #self.std_parser = StanfordParser(
        #    path_to_jar=path_to_jar, path_to_models_jar=path_to_models_jar)
        self.pos_tagger = StanfordPOSTagger(
            pos_tagger_models, pos_tagger_jar)


    def stanford_find_vp(self, sent):
        stf_vp = {}
        if len(sent.split()) <= 1:
            return stf_vp
        result = self.dep_parser.raw_parse(sent)
        #pos_sents = self.pos_tagger.tag_sents(sents)
        dep = result.next()
        for t in list(dep.triples()):
            assert  len(t) == 3
            relation = str(t[1])
            if relation in ['dobj', 'nsubjpass']:
                word1 = str(t[0][0])
                tag1 = str(t[0][1])
                word2 = str(t[2][0])
                tag2 = str(t[2][1])
                #f.write('\n%s:  %s ( %s )'%(relation, word1, word2))
                stf_vp[word1] = word2
        return stf_vp

         
    def test_one(self, text_dir):
        #self.RegexpParser_init()
        self.StanfordParser_init()
        logging.debug( '\nTesting text: %s\n' % text_dir )
        #print '\nTesting text: %s\n' % text_dir
        self.env.test_one_init(text_dir)
        action_sequence = []
        self.tag_word_num = 0
        self.test_flag = True
        
        for i in xrange(self.words_num):
            action, reward, state, terminal = self.step(0)
            self.tag_word_num += 1
        state_tags = self.env.state[:,-1]
        
        j = 0
        for l,s in enumerate(self.env.sents):
            svp = self.stanford_find_vp(s)
            #tagged_sent = self.pos_tagger.tag(s)
            #rvp = self.regexp_find_vp(tagged_sent)
            #print svp
            for k in range(len(self.env.w_of_s[l])):
                #print 'j=',j
                if j >= len(self.env.words):
                    break
                if int(state_tags[j]) == int(self.action_label):
                    acn = self.env.words[j]
                    if acn in svp.keys():
                        tmp = acn+' ( '+svp[acn]+' ) '
                    #elif acn in rvp.keys():
                    #    tmp = acn+' ( '+rvp[acn]+' ) '
                    else:
                        tmp = acn+' ( ) '
                    action_sequence.append(tmp)
                j += 1

            if j >= len(self.env.words):
                break

        return self.env.words, action_sequence, state_tags




    def train_test(self, test_steps, num_texts, f1):
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
                if (j+1)%100 == 0:
                    logging.debug( '\nTesting text: %d  step %d' % (i,j) )
                if self.tag_word_num >= self.words_num:
                    self.tag_word_num = 0

                if self.random_play:
                    a,r,s,t = self.step(1)
                else:
                    a,r,s,t = self.step(self.exploration_rate_test)
                self.tag_word_num += 1
                if t:
                    text_vec_tags = self.env.text_vec[:,-1]
                    state_tags = self.env.state[:,-1]
                    sum_tags = sum(text_vec_tags)

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
                    logging.debug( '\ntotal words: %d\n'%temp_words )
                    f1.write('\ntext_vec:\n')
                    f1.write(str(self.env.text_vec[:,-1]))
                    f1.write('\nstates:\n')
                    f1.write(str(self.env.state[:,-1])) 


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
                    logging.debug( '\nText: %d'%i )
                    logging.debug( '\nWords left: %d'%count )
                    logging.debug( 'Acions: %d'%total_actions )
                    logging.debug( 'Right_actions: %d'%right_actions )   
                    logging.debug( 'Tag_actions: %d'%tag_actions ) 
                    logging.debug( 'Actions_recall: %f'%recall )
                    logging.debug( 'Actions_precision: %f'%precision )
                    logging.debug( 'F_measure: %f'%F_value )
                    f1.write('\nright_tag: %d\n  wrong_tag: %d\n  tag_right_rate: %f\n'
                        %(right_tag,wrong_tag,tag_right_rate))
                    logging.debug( '\nright_tag: %d\n  wrong_tag: %d\n  tag_right_rate: %f\n'
                        %(right_tag,wrong_tag,tag_right_rate) )

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
        f1.write('\nTotal acions: %d'%tas)
        f1.write('\nTotal right_acions: %d'%ras)
        f1.write('\nTotal tag_acions: %d'%tta)
        f1.write('\nAverage_actions_recall: %f'%average_recall)
        f1.write('\nAverage_actions_precision: %f'%average_precision)
        f1.write('\nAverage_F_measure: %f'%ave_F_value)
        logging.debug( '\nTotal words left: %d'%cnt )
        logging.debug( 'Total acions: %d'%tas )
        logging.debug( 'Total right_actions: %d'%ras ) 
        logging.debug( 'Total tag_actions: %d'%tta )
        logging.debug( 'Average_actions_recall: %f'%average_recall )
        logging.debug( 'Average_actions_precision: %f'%average_precision )
        logging.debug( 'Average_F_measure: %f'%ave_F_value ) 
        f1.write('\ntotal_tag_right: %d total_tag_wrong: %d  accuracy: %f\n'
            %(total_tag_right,total_tag_wrong,accuracy))
        logging.debug( '\ntotal_tag_right: %d total_tag_wrong: %d  accuracy: %f\n'
            %(total_tag_right,total_tag_wrong,accuracy) )
       
