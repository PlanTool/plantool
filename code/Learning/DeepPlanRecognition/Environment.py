#coding:utf-8
import re
import mysql.connector
import gensim
import numpy as np
from gensim.models.word2vec import Word2Vec


#create table tag_actions5(text_num int, sent_num int, sent varchar(400), tag_sent varchar(400));
class Environment:
    def __init__(self, args):
        print 'Initializing the Environment...'
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
        self.long_text_flag = args.long_text_flag
        self.ten_fold_flag = args.ten_fold_flag
        self.reward_assign = [float(r) for r in args.reward_assign.split()]
        assert len(self.reward_assign) == 4
        self.db = mysql.connector.connect(user='fengwf',password='123',database='test')
        self.cur = self.db.cursor()
        self.actionDB = args.actionDB.split()
        print '\nself.actionDB',self.actionDB
        self.max_text_num = [int(i) for i in args.max_text_num.split()] 
        if args.texts_num != 0:
            self.max_text_num[0] = args.texts_num + args.test_text_num
        print 'self.max_text_num',self.max_text_num
        self.test_text_num = args.test_text_num


        self.text_amount = []
        for ind, tn in enumerate(self.actionDB):
            self.text_amount.append(self.count_texts(tn, self.max_text_num[ind]))
        print 'self.text_amount',self.text_amount

        self.test_text_name = []
        self.test_steps = 0
        for i in range(len(self.max_text_num)):
            temp = []
            if args.texts_num != 0:
                temp = range(self.test_text_num)
                self.test_steps += sum(self.text_amount[i][:self.test_text_num]) * self.words_num
            elif args.ten_fold_flag != 0 and args.fold_test_name:
                temp = args.fold_test_name
                #temp = [int(tn) for tn in args.fold_test_name.split()]
                for tp in temp:
                    self.test_steps += (self.text_amount[i][tp] * self.words_num)
            else:
                while(len(temp) < self.test_text_num):
                    rand = np.random.randint(1, self.max_text_num[i])
                    if rand not in temp:
                        temp.append(rand)
                        self.test_steps += (self.text_amount[i][rand] * self.words_num)
            self.test_text_name.append(list(temp))
        print 'self.test_text_name',self.test_text_name
        print 'self.test_steps',self.test_steps
        self.size = sum([sum(k) for k in self.text_amount])
        self.train_steps = self.size * self.words_num - self.test_steps
        print 'env.save_state.size',self.size

        print 'self.test_text_name',self.test_text_name,'\n'
        assert len(self.test_text_name) == len(self.max_text_num) == len(self.actionDB)
        self.model = Word2Vec.load_word2vec_format(model_dir + vec_model, binary=False)
        
        self.saved_states = np.zeros((self.size,self.words_num,self.wordvec))
        self.saved_text_vec = np.zeros((self.size,self.words_num,self.wordvec))
        self.saved_text_length = [] #np.zeros(self.size,dtype=int)
        self.text_length = 0
        self.total_text = 0
        self.current_text = 0
        self.test_index = 0
        self.long_tags = []
        self.long_words = []
        

    def count_texts(self, table_name, mtn):
        count = []
        for i in range(mtn):
            get_data = "select * from " + table_name + " where text_num = " + \
            str(i) + " order by sent_num"
            #print 'get_data = %s'%get_data
            #print 'text num : %d'%i
            self.cur.execute(get_data)
            temp = self.cur.fetchall()
            num = 1
            words = 0
            for j in range(len(temp)):
                sent = temp[j][2].split()
                ws = len(sent)
                if ws == 1 and sent[0] == '-----LONG_TEXT_SEPARATOR-----':
                    num += 1
                    #print temp[j][2].split()[0]
                else:
                    words += ws
            #print 'num = %d'%num
            #print 'total words: %d\n'%words
            count.append(num)
        #print count
        #print 'count = %d'%sum(count)
        return count


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
        print 'Getting tagged texts in Environment...'
        get_data = "select * from " + self.actionDB[self.text_num[0]] + \
        " where text_num=" + str(self.text_num[1]) + " order by sent_num"
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
        print 'self.text_length',self.text_length
        self.saved_text_length.append(self.text_length)
        #assert 1==0
        for j in range(self.words_num):
            if j < self.text_length:
                w = words[j]
                if len(self.model[w]):
                    word_vec = self.model[w]
                    #concatenate the word vectors and tags
                    word_vec = np.concatenate((word_vec,
                        [tags[j] for ind in xrange(self.wordvec-self.vec_length)]))
                else:
                    #if a word is not in the word2vec model, make it zeros
                    word_vec = np.zeros(self.vec_length)
                    word_vec = np.concatenate((word_vec,
                        [self.non_action_label for ind in xrange(self.wordvec-self.vec_length)]))
            else:
                #a text with shorter length will be padding with zeros
                word_vec = np.concatenate((np.zeros(self.vec_length),
                    [self.non_action_label for ind in xrange(self.wordvec-self.vec_length)])) 
                
            text_vec[j] = word_vec
            word_vec = []
        
        return text_vec  



    def _create_text_matrix_long(self):
        print 'Getting tagged long texts in Environment...'

        if not len(self.long_words):
            get_data = "select * from " + self.actionDB[self.text_num[0]] + \
            " where text_num=" + str(self.text_num[1]) + " order by sent_num"
            self.cur.execute(get_data)
            result = self.cur.fetchall()
            assert len(result) > 0
            
            temp_tags = []
            temp_words = []
            for i in range(len(result)):
                #get sentences from database
                sent = result[i][2].split()
                ws = len(sent)
                if ws == 1 and sent[0] == '-----LONG_TEXT_SEPARATOR-----':
                    print '\nlong text separator: ',result[i][2]
                    print '\nlen(temp_tags)',len(temp_tags)
                    print 'len(temp_words)',len(temp_words)
                    self.long_tags.append(temp_tags)
                    self.long_words.append(temp_words)
                    temp_tags = []
                    temp_words = []
                else:
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

                    temp_words.extend(words_of_sent)
                    temp_tags.extend(tags_of_sent)


            #print '\nlong text separator: -----LONG_TEXT_SEPARATOR-----'
            print '\nlen(temp_tags)',len(temp_tags)
            print 'len(temp_words)',len(temp_words)
            self.long_tags.append(temp_tags)
            self.long_words.append(temp_words)
            self.long_tags.reverse()
            self.long_words.reverse()
            temp_tags = []
            temp_words = []
        print '\nlen(self.long_tags)',len(self.long_tags)
        print 'len(self.long_words)',len(self.long_words)


        words, text_vec = self.get_matrix_from_long_words()
        
        return text_vec 



    def create_text_matrix_test_db(self, table, num):
        #print 'Getting tagged long texts in Environment...'

        get_data = "select * from " + table + " where text_num = " + str(num) + " order by sent_num"
        self.cur.execute(get_data)
        result = self.cur.fetchall()
        assert len(result) > 0
        
        temp_tags = []
        temp_words = []
        for i in range(len(result)):
            sent = result[i][2].split()
            ws = len(sent)
            if ws == 1 and sent[0] == '-----LONG_TEXT_SEPARATOR-----':
                self.long_tags.append(temp_tags)
                self.long_words.append(temp_words)
                temp_tags = []
                temp_words = []
            else:
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

                temp_words.extend(words_of_sent)
                temp_tags.extend(tags_of_sent)


        #print '\nlong text separator: -----LONG_TEXT_SEPARATOR-----'
        #print '\nlen(temp_tags)',len(temp_tags)
        #print 'len(temp_words)',len(temp_words)
        self.long_tags.append(temp_tags)
        self.long_words.append(temp_words)
        self.long_tags.reverse()
        self.long_words.reverse()
        temp_tags = []
        temp_words = []
        #print '\nlen(self.long_tags)',len(self.long_tags)
        #print 'len(self.long_words)',len(self.long_words)


    def get_matrix_from_long_words(self):
        assert len(self.long_words) > 0
        tags = self.long_tags.pop()
        words = self.long_words.pop()
        text_vec = np.zeros((self.words_num,self.wordvec))
        word_vec = []
        self.text_length = len(words)
        #print 'self.text_length',self.text_length
        self.saved_text_length.append(self.text_length)
        #assert 1==0
        for j in range(self.words_num):
            if j < self.text_length:
                w = words[j]
                if len(self.model[w]):
                    word_vec = self.model[w]
                    #concatenate the word vectors and tags
                    word_vec = np.concatenate((word_vec,
                        [tags[j] for ind in xrange(self.wordvec-self.vec_length)]))
                else:
                    #if a word is not in the word2vec model, make it zeros
                    word_vec = np.zeros(self.vec_length)
                    word_vec = np.concatenate((word_vec,
                        [self.non_action_label for ind in xrange(self.wordvec-self.vec_length)]))

            else:
                #a text with shorter length will be padding with zeros
                word_vec = np.concatenate((np.zeros(self.vec_length),
                    [self.non_action_label for ind in xrange(self.wordvec-self.vec_length)])) 
                
            text_vec[j] = word_vec
            word_vec = []
        
        return words, text_vec



    def train_init(self):
        print '\n\ntesting for tag_actions'
        if self.ten_fold_flag:
            tn = 0
            while tn in self.test_text_name:
                tn += 1
            self.text_num = [0, tn]
        else:
            self.text_num = [0, 0]  #select the first text of the first table in database
        print '\n\n\n\n\ntrain_table_num: %d  train_text_num: %d\n\n\n\n\n' \
        %(self.text_num[0],self.text_num[1])
        #get the word vectos of tagged text
        if self.long_text_flag:
            self.text_vec = self._create_text_matrix_long()
        else:
            self.text_vec = self._getTaggedtexts()
        self.state = self.text_vec.copy()#!!!!!NB!!!NB!!!!NB!!!!
        self.state[:,self.vec_length:] = 0


    def test_init(self):
        print '\n\ntesting for tag_actions'
        #text_start = self.max_text_num[0] - self.test_text_num
        #self.text_num = [0, text_start]
        self.long_tags = []
        self.long_words = []
        self.text_num[0] = 0
        self.text_num[1] = self.test_text_name[0][0]
        self.test_index += 1
        print '\n\n\n\n\ntest_table_num: %d  test_text_num: %d\n\n\n\n\n' \
        %(self.text_num[0],self.text_num[1])
        if self.long_text_flag:
            self.text_vec = self._create_text_matrix_long()
        else:
            self.text_vec = self._getTaggedtexts()
        self.state = self.text_vec.copy()#!!!!!NB!!!NB!!!!NB!!!!
        self.state[:,self.vec_length:] = 0


    def test_one_init(self, text_dir):
        print '\ntest_one_init.....\n'
        self.words, self.text_vec = self.create_text_matrix(text_dir)
        self.state = self.text_vec.copy()#!!!!!NB!!!NB!!!!NB!!!!
        self.state[:,self.vec_length:] = 0


    def test_one_db_init(self, table, num):
        #print '\ntest_one_db_init.....\n'
        self.long_tags = []
        self.long_words = []
        self.create_text_matrix_test_db(table, num)
        self.test_db_texts_num = len(self.long_words)
        #print 'self.test_db_texts_num',self.test_db_texts_num
        self.words, self.text_vec = self.get_matrix_from_long_words()
        #assert 1==0
        self.state = self.text_vec.copy()#!!!!!NB!!!NB!!!!NB!!!!
        self.state[:,self.vec_length:] = 0


    def test_one_db_restart(self):
        self.words, self.text_vec = self.get_matrix_from_long_words()
        self.state = self.text_vec.copy()
        self.state[:,self.vec_length:] = 0


    def restart_test(self):
        if not len(self.long_words):
            if self.test_index < self.test_text_num:
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
            print '\ntest_table_num: %d  test_text_num: %d'%(self.text_num[0],self.text_num[1])
        
        if self.long_text_flag:
            self.text_vec = self._create_text_matrix_long()
        else:
            self.text_vec = self._getTaggedtexts()
        self.state = self.text_vec.copy()#!!!!!NB!!!NB!!!!NB!!!!
        self.state[:,self.vec_length:] = 0


    def restart(self):
        self.saved_states[self.current_text] = self.state
        self.saved_text_vec[self.current_text] = self.text_vec
        self.total_text = max(self.total_text, self.current_text + 1)
        self.current_text = (self.current_text + 1)%self.size

        if not len(self.long_words):
            if self.text_num[1] < self.max_text_num[self.text_num[0]] - 1:
                self.text_num[1] += 1
            else:
                #choose the first text in the next table
                if self.text_num[0] < len(self.actionDB)-1: 
                    self.text_num[0] += 1
                    self.text_num[1] = 0
                else:
                    self.text_num = [0, 0] #return to the initial text

            while(self.text_num[1] in self.test_text_name[self.text_num[0]]):
                print '\n\n\n\n\n-----self.text[%d,%d] is reserved for test.-----\n\n\n\n\n' \
                %(self.text_num[0],self.text_num[1])
                if self.text_num[1] < self.max_text_num[self.text_num[0]] - 1:
                    self.text_num[1] += 1
                else:
                    #choose the first text in the next table
                    if self.text_num[0] < len(self.actionDB) - 1: 
                        self.text_num[0] += 1
                        self.text_num[1] = 0
                    else:
                        self.text_num = [0, 0] #return to the initial text

        print '\ntrain_table_num: %d  train_text_num: %d'%(self.text_num[0],self.text_num[1])
        if self.long_text_flag:
            self.text_vec = self._create_text_matrix_long()
        else:
            self.text_vec = self._getTaggedtexts()
        self.state = self.text_vec.copy()#!!!!!NB!!!NB!!!!NB!!!!
        self.state[:,self.vec_length:] = 0

        
    def act(self, action):
        # Performs action and returns reward
        action = int(action)  
        #even num refers to tagging action, odd num refer to non-action
        if action%2 == 1:  
            self.state[action/2,self.vec_length:] = self.action_label  
            #print 'tag an action'
        else:
            self.state[action/2,self.vec_length:] = self.non_action_label 
            #print 'tag a non-action'
        
        t_a_count = 0  #amount of tagged actions 
        for t in self.state[:,-1]:
            if t == self.action_label:
                t_a_count += 1
        #print '----------tagged_actions_amount: %d----------'%t_a_count
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
    parser.add_argument("--num_actions", type=int, default=1000, help="")
    parser.add_argument("--words_num", type=int, default=500, help="")
    parser.add_argument("--wordvec", type=int, default=100, help="")
    parser.add_argument("--channel", type=int, default=1, help="")
    parser.add_argument("--batch_size", type=int, default=8, help="")
    parser.add_argument("--loops", type=int, default=10000, help="")
    parser.add_argument("--model_dir", default="/home/fengwf/Documents/", help="")
    parser.add_argument("--vec_model", default='mymodel5-5-50', help="")
    parser.add_argument("--actionDB", default='tag_actions6', help="")
    parser.add_argument("--max_text_num", default='43', help="")
    parser.add_argument("--reward_assign", default='2.0 1.0 -1.0 -2.0', help="")
    parser.add_argument("--action_rate", type=float, default=0.15, help="")
    parser.add_argument("--action_label", type=int, default=2, help="")
    parser.add_argument("--non_action_label", type=int, default=1, help="")
    parser.add_argument("--test", type=int, default=1, help="")
    parser.add_argument("--test_text_num", type=int, default=6, help="")
    parser.add_argument("--penal_radix", type=float, default=5.0, help="")
    parser.add_argument("--vec_length", type=int, default=50, help="")
    parser.add_argument("--long_text_flag", type=int, default=1, help="")


    args = parser.parse_args()
    env = Environment(args)
    env.train_init()
    a = raw_input('Continue?(y/n)').lower()
    while a != 'n':
        env.restart()
        a = raw_input('Continue?(y/n)').lower()

    env.test_init()
    a = ''
    while a != 'n':
        env.restart_test()
        a = raw_input('Continue?(y/n)').lower()

