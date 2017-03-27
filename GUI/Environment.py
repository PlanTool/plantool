#coding:utf-8
import re
import gensim
import logging
import numpy as np
import mysql.connector
from gensim.models.word2vec import Word2Vec

class Environment:
    def __init__(self, args):
        #logging.debug( 'Initializing the Environment...' )
        model_dir = args.model_dir
        vec_model = args.vec_model  
        self.words_num = args.words_num
        self.wordvec = args.wordvec
        self.vec_length = args.vec_length
        self.num_actions = args.num_actions
        self.action_rate = args.action_rate
        self.penal_radix = args.penal_radix
        self.action_label = args.action_label
        self.non_action_label = args.non_action_label
        self.reward_assign = [float(r) for r in args.reward_assign.split()]
        assert len(self.reward_assign) == 4
        self.db = mysql.connector.connect(user='fengwf',password='123',database='test')
        self.cur = self.db.cursor()
        self.actionDB = args.actionDB.split()
        self.max_text_num = [int(i) for i in args.max_text_num.split()] 
        self.test_text_num = args.test_text_num

        self.size = sum(self.max_text_num) - self.test_text_num*len(self.max_text_num)
        #logging.debug( 'self.actionDB',self.actionDB )
        #logging.debug( 'env.save_state.size',self.size )
        self.test_text_name = []
        for i in range(len(self.max_text_num)):
            temp = []
            while(len(temp) < self.test_text_num):
                rand = np.random.randint(1, self.max_text_num[i])
                if rand not in temp:
                    temp.append(rand)
            #temp = np.random.randint(1, self.max_text_num[i], size=self.test_text_num)
            self.test_text_name.append(list(temp))
        #logging.debug( 'self.test_text_name',self.test_text_name )
        assert len(self.test_text_name) == len(self.max_text_num) == len(self.actionDB)
        self.model = Word2Vec.load_word2vec_format(model_dir + vec_model, binary=False)
        
        self.saved_states = np.zeros((self.size,self.words_num,self.wordvec))
        self.saved_text_vec = np.zeros((self.size,self.words_num,self.wordvec))
        self.saved_text_length = [] #np.zeros(self.size,dtype=int)
        self.text_length = 0
        self.total_text = 0
        self.current_text = 0
        self.half_tags = 1
        self.test_index = 0
        

    def create_text_matrix(self, text_dir):
        tags = []
        word_vec = []
        text_vec = np.zeros((self.words_num,self.wordvec))
        raw_text = open(text_dir).read()
        words = re.findall(r'[\w\-\_]+', raw_text)
        self.text_length = len(words)
        self.saved_text_length.append(self.text_length)
        for j in range(self.words_num):
            if j < self.text_length:
                w = words[j]
                if len(self.model[w]):
                    word_vec = self.model[w]
                    #concatenate the word vectors and tags
                    word_vec = np.concatenate((word_vec, np.zeros(self.wordvec-self.vec_length)))
                else:
                    #if a word is not in the word2vec model, make it zeros
                    word_vec = np.zeros(self.vec_length)
                    word_vec = np.concatenate((word_vec, np.zeros(self.wordvec-self.vec_length)))

            else:
                #a text with shorter length will be padding with zeros
                word_vec = np.zeros(self.wordvec)
                
            text_vec[j] = word_vec
            word_vec = []
        
        return words, text_vec


    def _getTaggedtexts(self):
        logging.debug( 'Getting tagged texts in Environment...' )
        get_data = "select * from " + self.actionDB[self.text_num[0]] + " where text_num=" + str(self.text_num[1]) + " order by sent_num"
        self.cur.execute(get_data)
        result = self.cur.fetchall()
        assert len(result) > 0
        
        tags = []
        words = []
        text_vec = np.zeros((self.words_num,self.wordvec))
        word_vec = []
        for i in range(len(result)):
            #get sentences from database
            sent_lower = result[i][2][0].lower() + result[i][2][1:]  
            words_of_sent = re.split(r' ',sent_lower)
            temp_tags_of_sent = re.split(r' ',result[i][3])
            #get the tags from database
            tags_of_sent = []
            for t in temp_tags_of_sent:
                if t == '1':
                    tags_of_sent.append(self.action_label)
                else:
                    tags_of_sent.append(self.non_action_label)

            words.extend(words_of_sent)
            tags.extend(tags_of_sent)
        
        self.text_length = len(words)
        self.saved_text_length.append(self.text_length)
        for j in range(self.words_num):
            if j < self.text_length:
                w = words[j]
                if len(self.model[w]):
                    word_vec = self.model[w]
                    #concatenate the word vectors and tags
                    if self.half_tags:
                        word_vec = np.concatenate((word_vec,[tags[j] for ind in xrange(self.wordvec-self.vec_length)]))
                    else:
                        word_vec = np.concatenate((word_vec,[tags[j]])) 
                else:
                    #if a word is not in the word2vec model, make it zeros
                    if self.half_tags:
                        word_vec = np.zeros(self.vec_length)
                        word_vec = np.concatenate((word_vec,[self.non_action_label for ind in xrange(self.wordvec-self.vec_length)]))
                    else:
                        word_vec = np.zeros(self.wordvec-1)
                        word_vec = np.concatenate((word_vec,[self.non_action_label]))

            else:
                #a text with shorter length will be padding with zeros
                if self.half_tags:
                    word_vec = np.concatenate((np.zeros(self.vec_length),[self.non_action_label for ind in xrange(self.wordvec-self.vec_length)])) 
                else:
                    word_vec = np.concatenate((np.zeros(self.wordvec-1),[self.non_action_label]))
                    #word_vec = np.ones(self.wordvec)
                
            text_vec[j] = word_vec
            word_vec = []
        
        return text_vec  


    def train_init(self):
        self.text_num = [0, 0]  #select the first text of the first table in database
        #get the word vectos of tagged text
        self.text_vec = self._getTaggedtexts()
        self.state = self.text_vec.copy()#!!!!!NB!!!NB!!!!NB!!!!
        if self.half_tags:
            self.state[:,self.vec_length:] = 0
        else:
            self.state[:,-1] = 0


    def test_one_init(self, text_dir):
        print '\ntest_one_init.....\n'
        self.words, self.text_vec = self.create_text_matrix(text_dir)
        self.state = self.text_vec.copy()#!!!!!NB!!!NB!!!!NB!!!!
        self.state[:,self.vec_length:] = 0


    def test_init(self):
        self.text_num[0] = 0
        self.text_num[1] = self.test_text_name[0][0]
        self.test_index += 1
        self.text_vec = self._getTaggedtexts()
        self.state = self.text_vec.copy()#!!!!!NB!!!NB!!!!NB!!!!
        if self.half_tags:
            self.state[:,self.vec_length:] = 0
        else:
            self.state[:,-1] = 0



    def restart_test(self):
        if self.test_index < self.test_text_num -1:
            self.text_num[1] = self.test_text_name[self.text_num[0]][self.test_index]
            self.test_index += 1
        else:
            if self.text_num[0] < len(self.test_text_name) -1:
                self.text_num[0] += 1
                self.text_num[1] = self.test_text_name[self.text_num[0]][0]
                self.test_index = 1
            else:
                self.text_num[0] = 0
                self.text_num[1] = self.test_text_name[0][0]
                self.test_index = 1
        self.text_vec = self._getTaggedtexts()
        self.state = self.text_vec.copy()#!!!!!NB!!!NB!!!!NB!!!!
        if self.half_tags:
            self.state[:,self.vec_length:] = 0
        else:
            self.state[:,-1] = 0


    def restart(self):
        self.saved_states[self.current_text] = self.state
        self.saved_text_vec[self.current_text] = self.text_vec
        self.total_text = max(self.total_text,self.current_text + 1)
        self.current_text = (self.current_text + 1)%self.size
        if self.text_num[1] < self.max_text_num[self.text_num[0]]-self.test_text_num-1:
            self.text_num[1] += 1
        else:
            #choose the first text in the next table
            if self.text_num[0] < len(self.actionDB)-1: 
                self.text_num[0] += 1
                self.text_num[1] = 0
            else:
                self.text_num = [0, 0] #return to the initial text

        while(self.text_num[1] in self.test_text_name[self.text_num[0]]):
            if self.text_num[1] < self.max_text_num[self.text_num[0]]-self.test_text_num-1:
                self.text_num[1] += 1
            else:
                #choose the first text in the next table
                if self.text_num[0] < len(self.actionDB)-1: 
                    self.text_num[0] += 1
                    self.text_num[1] = 0
                else:
                    self.text_num = [0, 0] #return to the initial text

        self.text_vec = self._getTaggedtexts()
        self.state = self.text_vec.copy()#!!!!!NB!!!NB!!!!NB!!!!
        if self.half_tags:
            self.state[:,self.vec_length:] = 0
        else:
            self.state[:,-1] = 0

        
    def act(self, action):
        # Performs action and returns reward
        action = int(action)  
        #even num refers to tagging action, odd num refer to non-action
        if action%2 == 1:  
            if self.half_tags:
                self.state[action/2,self.vec_length:] = self.action_label 
            else:
                self.state[action/2,-1] = self.action_label
            #if action/2 < len(self.words):
            #    print '-----tag an action %s-----'%self.words[action/2]
        else:
            if self.half_tags:
                self.state[action/2,self.vec_length:] = self.non_action_label 
            else:
                self.state[action/2,-1] = self.non_action_label 
        
        t_a_count = 0  #amount of tagged actions 
        for t in self.state[:,-1]:
            if t == self.action_label:
                t_a_count += 1
        t_a_rate = float(t_a_count)/self.words_num

        if self.text_vec[action/2,-1] == self.state[action/2,-1]:
            if self.text_vec[action/2,-1] == self.action_label:
                reward = self.reward_assign[0]
            else:
                reward = self.reward_assign[1]
            if t_a_rate <= self.action_rate:
                reward += self.penal_radix*t_a_rate*t_a_rate #2 1 -1 -2   5*0.15*0.15
            else:
                reward -= self.penal_radix*t_a_rate*t_a_rate 
        else:
            if self.text_vec[action/2,-1] == self.non_action_label:
                reward = self.reward_assign[2]
            else:
                reward = self.reward_assign[3]
            reward -= self.penal_radix*t_a_rate*t_a_rate 
        return reward


    def getState(self):
    # Gets current text state
        return self.state


    def isTerminal(self):
    # Returns if tag_actions is done
    #if all the words of a text have been tagged, then terminate
        if 0 in self.state[:,-1]:  
            return False
        else:
            return True


if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--num_actions", type=int, default=10, help="Total actions of this task.")
    parser.add_argument("--words_num", type=int, default=5, help="")
    parser.add_argument("--wordvec", type=int, default=22, help="")
    parser.add_argument("--vec_length", type=int, default=20, help="Word vector dimension.")
    parser.add_argument("--channel", type=int, default=1, help="")
    parser.add_argument("--batch_size", type=int, default=32, help="Batch size for neural network.")
    parser.add_argument("--loops", type=int, default=10000, help="Number of loops in testing.")
    parser.add_argument("--model_dir", default="/home/fengwf/Documents/", help="The directory of word vector model.")
    parser.add_argument("--vec_model", default='mymodel5-5-20', help="Word vector model name.")
    parser.add_argument("--actionDB", default='tag_actions tag_actions1 tag_actions2 tag_actions4', help="Tables' names in database test.")
    parser.add_argument("--max_text_num", default='35 20 33 111', help="Max text num of database tables.")
    parser.add_argument("--reward_assign", default='2.0 1.0 -1.0 -2.0', help="How the assign the rewards.")
    parser.add_argument("--action_rate", type=float, default=0.15, help="Average actions percentage in a text.")
    parser.add_argument("--action_label", type=int, default=2, help="An integer refer to the label of actions.")
    parser.add_argument("--non_action_label", type=int, default=1, help="An integer refer to the label of non-actions.")
    parser.add_argument("--test_text_num", type=int, default=8, help="How many testing steps after each epoch.")
    parser.add_argument("--penal_radix", type=float, default=5.0, help="Penalty radix according to action rate.")


    args = parser.parse_args()
    env = Environment(args)
    env.test_one_init('./test_inputs/1.txt')
    print env.state
    #ws, tm = env.create_text_matrix('./test_inputs/1.txt')
    #for w in ws:
    #    print w
    #print tm

    '''
    for i in range(200):
        env.restart()
    env.test_init()
    for j in range(40):
        env.restart_test()
    '''
