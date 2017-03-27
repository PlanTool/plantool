#!/usr/bin/python
import getopt
import random
import logging
import operator
import numpy as np
from gensim import models
from copy import deepcopy
from math import ceil, floor
from itertools import permutations
from numpy import exp, dot, log



class ShallowPlan(object):
    def __init__(self, args):
        self.k = args.k
        self.split_texts = args.split_texts
        self.mask = args.mask
        self.domain = args.domain
        self.domain_dir = args.domain_dir  
        self.train_word2vec = args.train_word2vec   
         
        self.workers = args.workers
        self.min_count = args.min_count
        self.word2vec_iter = args.word2vec_iter

        self.iter_num = args.iter_num
        self.learning_rate = args.learning_rate
        self.blank_percentage = args.blank_percentage
        self.prediction_set_size = args.prediction_set_size
        self.window_size = args.window_size


    def k_fold_generator(self, plans, domain_dir, k):
        subset_size = len(plans) / k
        for i in range(k):
            train = plans[:i*subset_size] + plans[(i+1)*subset_size:]
            test = plans[i*subset_size:][:subset_size]

            f = open(str(domain_dir)+'train'+str(i)+'.txt', 'w')
            for t in train:
                f.write(t+'\n')
            f.close()

            f = open(str(domain_dir)+'test'+str(i)+'.txt', 'w')
            for t in test:
                f.write(t+'\n')
            f.close()


    def remove_random_actions(self, plan):
        blank_count = int(ceil(len(plan) * self.blank_percentage + 0.5))
        incomplete_plan = deepcopy(plan)
        indices = []
        cnt = 0
        while cnt < blank_count:
            missing_action_index = random.randrange(1, len(plan)-1)
            if missing_action_index in indices:
                # making sure that the indices generated are unique
                continue
            else:
                incomplete_plan[ missing_action_index ] = u'###'
                indices.append(missing_action_index)
                cnt += 1
        return blank_count, indices, incomplete_plan


    # p = permutation of actions
    # ip = incomplete plan
    def getTentativePlan(self, p, ip, indices):
        for i in range(len(indices)):
            ip[indices[i]] = p[i]
        return ip


    def min_uncertainty_distance_in_window_size(self, indices):
        # Makes sure that within a window size there is only one missing action
        # Optimized code from http://stackoverflow.com/questions/15606537/finding-minimal-difference
        if len(indices) <= self.window_size:
            return 2
        idx = deepcopy(indices)
        idx = sorted(idx)
        res = [ idx[i+self.window_size]-idx[i] for i in xrange(len(idx)) if i+self.window_size < len(idx) ]
        return min(res)


    def score_sg_pair(self, model, word, word2):
        l1 = model.syn0[word2.index]
        l2a = deepcopy(model.syn1[word.point])  # 2d matrix, codelen x layer1_size
        sgn = -1.0**word.code  # ch function, 0-> 1, 1 -> -1
        lprob = -log(1.0 + exp(-sgn*dot(l1, l2a.T)))
        return sum(lprob)


    def score_sg_grad_b(self, model, word, context_word, b, a):
        l1 = model.syn0[context_word.index] # vector of context word
        l2a = deepcopy(model.syn1[word.point])  # 2d matrix, codelen x layer1_size
        sgn = -1.0**word.code  # ch function, 0-> 1, 1 -> -1
        sigma = 1.0 / (1.0 + exp(-sgn*dot(a * l1, b * l2a.T)))   # p(context_word|word)
        grads = (1.0 - sigma) * dot(a * l1, l2a.T) * sgn  # gradient respect to parameter b
        return sum(grads)


    def compute_gradient(self, model, blank_index, sample_word, target_weight, 
        current_weight, incompelete_plan):
        grad = 0.0
        vocab = model.vocab
        current_word = vocab[sample_word]
        context_words = [ vocab[incompelete_plan[blank_index-1]], vocab[incompelete_plan[blank_index+1]] ]
        for target_word in context_words:
            grad += self.score_sg_grad_b(model, current_word, target_word, current_weight, target_weight)
        return grad


    def test_grad(self, blank_count, model, plan, blank_index):
        tmp_plan =  deepcopy(plan)
        vocab_size = len(model.vocab.keys())
        weights = np.ones(vocab_size * blank_count).reshape(vocab_size, blank_count) / vocab_size
        gradients = np.zeros(vocab_size * blank_count).reshape(vocab_size, blank_count)
        actions = model.vocab.keys()
        grad_dict = {}
        score_dict = {}
        # true_word = plan[blank_index]
        #logging.debug("true_word\tsample_word\tgrad")
        for k in range(vocab_size):
            sample_index = k
            sample_word = actions[sample_index]
            current_weight = weights[sample_index][0]
            grad = self.compute_gradient(model, blank_index, sample_word, 1,
                                    current_weight, plan)
            grad_dict[sample_word] = grad
            tmp_plan[blank_index] = sample_word
            score_dict[sample_word] = model.score([tmp_plan[blank_index-1:blank_index+2]])
            #logging.debug("%s\t%s\t%s", plan[blank_index], sample_word, grad)
            gradients[sample_index][0] += grad

        sorted_x = sorted(grad_dict.items(), key=operator.itemgetter(1), reverse=True)
        order_grad = sorted_x.index([item for item in sorted_x if item[0] == plan[blank_index]][0])

        for order, item in enumerate(sorted_x, start=1):
            if item[0] == plan[blank_index]:
                pass
                #logging.info("***")
            #logging.info("%s\t%f\t%d", item[0], item[1], order)

        sorted_y = sorted(score_dict.items(), key=operator.itemgetter(1), reverse=True)
        # order of true word score
        order_score = sorted_y.index([item for item in sorted_y if item[0] == plan[blank_index]][0])
        for order, item in enumerate(sorted_y, start=1):
            if item[0] == plan[blank_index]:
                pass
                #logging.info("***")
            #logging.info("%s\t%f\t%d", item[0], item[1], order)


    def test_pair_sg(self, model, target_word, current_word, target_weight, current_weight):
        score_dict = {}
        d = model.vocab
        vocab = model.vocab.keys()
        #logging.info("true word:%s", current_word)
        #logging.info("current_word\ttarget_word\tscore")
        for word in vocab:
            score = self.score_sg_grad_b(model, d[target_word], d[word], target_weight, current_weight)
            score_dict[word] = score
            if word == current_word:
                pass
                #logging.info("***")
            #logging.info("%s\t%s\t%f", word, target_word, score)
        sort_x = sorted(score_dict.items(), key=operator.itemgetter(1), reverse=True)
        for order, item in enumerate(sort_x, 1):
            if item[0] == current_word:
                pass
                #logging.info("my score order:%d", order)
                #logging.info("my score:%f", item[1])

        gensim_score_dict = {}
        for word in vocab:
            score = self.score_sg_pair(model, d[target_word], d[word])
            # score = score_sg_grad(model, d[target_word], d[word], target_weight, current_weight)
            gensim_score_dict[word] = score
            if word == current_word:
                pass
                #logging.info("***")
            #logging.info("%s\t%s\t%f", word, target_word, score)
        sort_y = sorted(gensim_score_dict.items(), key=operator.itemgetter(1), reverse=True)
        for order, item in enumerate(sort_y, 1):
            if item[0] == current_word:
                pass
                #logging.info("gensim score order:%d", order)
                #logging.info("gensim score:%f", item[1])


    def train_and_test(self, set_number):
        '''
        The function trains a model on training data and then tests the models accuracy on the testing data.
        Since training is time consuming, we save the model and load it later for further testing
        '''
        print "\n=== Set : %s ===\n" % str(set_number)
        # Train a model based on training data
        if self.train_word2vec:
            sentences = models.word2vec.LineSentence(self.domain_dir+'train'+str(set_number)+'.txt')
            model = models.Word2Vec(sentences=sentences, min_count=self.min_count, sg=1, 
                workers=self.workers, hs=1, window=self.window_size, iter=self.word2vec_iter)
            model.save(self.domain_dir+'wordvec_model'+str(set_number)+'.txt')
            print "Training word vector: COMPLETE!"
        else:
            # OR load a mode
            model = models.Word2Vec.load(self.domain_dir+'wordvec_model'+str(set_number)+'.txt')
            print "Loading saved model: COMPLETE!"
        

        # Evaluate model on test data
        plans = open(self.domain_dir+'test'+str(set_number)+'.txt').read().split("\n")
        list_of_actions = [[unicode(actn, "utf-8") for actn in plan_i.split()] for plan_i in plans]
        actions = model.vocab.keys()
        vocab_size = len(actions)
        correct = 0
        total = 0

        print "Testing : RUNNING . . ."
        list_of_actions = [x for x in list_of_actions if len(x) != 0]

        # test compute gradient
        self.test_grad(1, model, list_of_actions[0], 4)

        for itr in xrange(len(list_of_actions)):
            plan = list_of_actions[itr]
            self.test_a_plan(plan, total, correct, vocab_size, actions, model, itr, list_of_actions)

        print "\r\rTesting : COMPLETE!\n"
        print "\nUnknown actions: %s; Correct predictions: %s" % (str(total), str(correct))
        print "Set Accuracy: %s\n" % str( float(correct*100)/total)
        return total, correct


    def test_a_plan(self, plan, total, correct, vocab_size, actions, model, itr, list_of_actions):
        # This reduces the combinatorial burst as all permutations do not need to be checked
        # This is the logic used for the paper's (http://rakaposhi.eas.asu.edu/aamas16-hankz.pdf) code
        while True:
            blank_count, indices, incomplete_plan = self.remove_random_actions(plan)
            if self.min_uncertainty_distance_in_window_size(indices) > self.window_size:
                # print "min_uncertainty > self.window_size"
                break
        #print "\nblank_count:\t%d"%blank_count
        #print "\nincomplete_plan:\n%s"%str(incomplete_plan)
        #print "\ncomplete_plan:\n%s"%str(plan)
        total += blank_count
        weights = np.ones(vocab_size * blank_count).reshape(vocab_size, blank_count) / vocab_size
        for i in range(self.iter_num):
            gradients = np.zeros(vocab_size * blank_count).reshape(vocab_size, blank_count)
            grad_dict = {}
            for k in range(vocab_size):
                sample_indexs = []
                for blank_order in range(blank_count):
                    index = np.random.choice(np.arange(vocab_size), p=weights[:, blank_order])
                    sample_indexs.append(index)

                # compute gradients
                for blank_order in range(blank_count):
                    blank_index = indices[blank_order]
                    sample_index = sample_indexs[blank_order]
                    # sample_index = k
                    sample_word = actions[sample_index]
                    current_weight = weights[sample_index][blank_order]
                    grad = self.compute_gradient(model, blank_index, sample_word, 1,
                                            current_weight, incomplete_plan)
                    gradients[sample_index][blank_order] += grad

            # update weights
            weights += self.learning_rate * gradients
            # min-max normalize to 0-1
            mins = np.amin(weights, axis=0)
            maxs = np.amax(weights, axis=0)
            weights = (weights - mins) / (maxs - mins)

            # normalize to distribution
            column_sum = weights.sum(axis=0)
            weights = weights / column_sum[np.newaxis, :]

        sorted_weights = np.sort(weights, axis=0)

        best_plan_args = np.argsort(weights, axis=0)[-1*self.prediction_set_size:][:]
        print "\n----------best args----------"
        print "%s"%str(best_plan_args)
        for i in range(blank_count):
            blank_index = indices[i]
            for j in range(self.prediction_set_size):
                pass
                #print "%s\t%f"%(actions[best_plan_args[j][i]], sorted_weights[j+vocab_size-10][i])

        for blank_order in range(blank_count):
            blank_index = indices[blank_order]
            for sample_index in best_plan_args[:][blank_order]:
                if actions[sample_index] == plan[blank_index]:
                    correct += 1
                    print 'Right prediction:\t%s  ---->  %s'%(actions[sample_index], plan[blank_index])
                    break

        # Print at certain time intervals
        if (itr*100)/len(list_of_actions) % 10 == 0:
            print "\rProgress: %s %%" % str( (itr*100)/len(list_of_actions) ) 


    def complete_a_plan(self, incomplete_plan, vocab_size, actions, model, blank_count, indices):
        """
        """
        weights = np.ones(vocab_size * blank_count).reshape(vocab_size, blank_count) / vocab_size
        for i in range(self.iter_num):
            gradients = np.zeros(vocab_size * blank_count).reshape(vocab_size, blank_count)
            grad_dict = {}
            for k in range(vocab_size):
                sample_indexs = []
                for blank_order in range(blank_count):
                    index = np.random.choice(np.arange(vocab_size), p=weights[:, blank_order])
                    sample_indexs.append(index)

                # compute gradients
                for blank_order in range(blank_count):
                    blank_index = indices[blank_order]
                    sample_index = sample_indexs[blank_order]
                    # sample_index = k
                    sample_word = actions[sample_index]
                    current_weight = weights[sample_index][blank_order]
                    grad = self.compute_gradient(model, blank_index, sample_word, 1,
                                            current_weight, incomplete_plan)
                    gradients[sample_index][blank_order] += grad

            # update weights
            weights += self.learning_rate * gradients
            # min-max normalize to 0-1
            mins = np.amin(weights, axis=0)
            maxs = np.amax(weights, axis=0)
            weights = (weights - mins) / (maxs - mins)

            # normalize to distribution
            column_sum = weights.sum(axis=0)
            weights = weights / column_sum[np.newaxis, :]

        sorted_weights = -np.sort(-weights, axis=0)

        #best_plan_args = np.argsort(weights, axis=0)[-1*self.prediction_set_size:][:]
        best_plan_args = np.argsort(-weights, axis=0)[:self.prediction_set_size][:]
        print "\n----------best args----------"
        print "%s"%str(best_plan_args)
        for i in range(blank_count):
            blank_index = indices[i]
            print '\nPredicted actions for index %d:'%blank_index
            for j in range(self.prediction_set_size):
                predict_act = str(actions[best_plan_args[j][i]])
                weight = sorted_weights[j][i]
                #print type(predict_act)
                #if predict_act in self.mis_actions:
                #    print '---Predict right!---'
                print "%s\t%f"%(predict_act, weight)


    def test(self, plan):
        model = models.Word2Vec.load(self.domain_dir+'wordvec_model'+str(1)+'.txt')
        print "\nLoading saved model: COMPLETE!"
        
        # Evaluate model on test data
        actions = model.vocab.keys()
        vocab_size = len(actions)
        #indices = [1, 6, 12, 18]
        #blank_count = 4
        #self.mis_actions = ['PUT-DOWN-B11', 'UNSTACK-B10-B17', 'UNSTACK-B9-B21', 'PICK-UP-B22']

        if not plan:
            plan = """UNSTACK-B11-B4 ### UNSTACK-B4-B22 PUT-DOWN-B4 UNSTACK-B22-B10 \
            PUT-DOWN-B22 ### PUT-DOWN-B10 UNSTACK-B17-B1 PUT-DOWN-B17 \
            UNSTACK-B1-B9 PUT-DOWN-B1 ### PUT-DOWN-B9 UNSTACK-B21-B8 \
            PUT-DOWN-B21 UNSTACK-B8-B20 PUT-DOWN-B8 ### STACK-B22-B8 \
            UNSTACK-B20-B5 PUT-DOWN-B20 UNSTACK-B5-B2 STACK-B5-B22 PICK-UP-B4 \
            STACK-B4-B5 PICK-UP-B13 STACK-B13-B4 UNSTACK-B2-B7 PUT-DOWN-B2 \
            UNSTACK-B7-B6 PUT-DOWN-B7 UNSTACK-B6-B14 PUT-DOWN-B6 UNSTACK-B14-B16 \
            PUT-DOWN-B14 UNSTACK-B16-B18 PUT-DOWN-B16 UNSTACK-B18-B3 PUT-DOWN-B18 \
            UNSTACK-B3-B12 STACK-B3-B10 PICK-UP-B20 STACK-B20-B13 UNSTACK-B12-B15 \
            PUT-DOWN-B12 UNSTACK-B15-B23 STACK-B15-B3 PICK-UP-B9 STACK-B9-B18 \
            PICK-UP-B14 STACK-B14-B20 UNSTACK-B23-B19 STACK-B23-B15 PICK-UP-B1 \
            STACK-B1-B14 PICK-UP-B11 STACK-B11-B9 PICK-UP-B16 STACK-B16-B23 \
            PICK-UP-B17 STACK-B17-B1 PICK-UP-B21 STACK-B21-B16 PICK-UP-B7 \
            STACK-B7-B17 PICK-UP-B19 STACK-B19-B7 PICK-UP-B2 STACK-B2-B19 \
            PICK-UP-B12 STACK-B12-B2"""

        print "Testing : RUNNING . . ."
        #list_of_actions = [unicode(actn, "utf-8") for actn in plan.split()]
        indices = []
        blank_count = 0
        list_of_actions = []   
        for i, a in enumerate(plan.split()):
            list_of_actions.append(unicode(a, "utf-8"))
            if a == self.mask:
                blank_count += 1
                indices.append(i)
        print 'blank_count = %d'%blank_count
        print 'blank indices:\t%s'%str(indices)
        self.complete_a_plan(list_of_actions, vocab_size, actions, model, blank_count, indices)



    def run(self):
        if self.split_texts:
            plans = open(self.domain_dir+self.domain).read().split('\n')
            self.k_fold_generator(plans, self.domain_dir, self.k)

        print "\n=== Domain : %s ===\n" % self.domain_dir
        total_unknown_actions = 0
        total_correct_predictions = 0
        for i in range(self.k):
            ua, cp = self.train_and_test( i )
            total_unknown_actions += ua
            total_correct_predictions += cp

        print "\n==== FINAL STATISTICS ===="
        print "\nTotal unknown actions: %s; Total correct predictions: %s" % (str(total_unknown_actions), str(total_correct_predictions))
        if total_unknown_actions > 0:
            print "ACCURACY: %s\n" % str( float(total_correct_predictions*100)/total_unknown_actions )
        else:
            print "ACCURACY: %f\n" % float( 0 )



if __name__ == '__main__':
    import time
    import argparse
    logging.basicConfig(filename='DUP_debug.log', filemode='w', level=logging.DEBUG)
    start = time.time()
    parser = argparse.ArgumentParser()
    parser.add_argument("--domain_dir", default="./domains/DUP/blocks/", help="")
    parser.add_argument("--domain", default="blocks5000.txt", help="")
    parser.add_argument("--k", type=int, default=10, help="")
    parser.add_argument("--split_texts", type=int, default=0, help="")
    parser.add_argument("--mask", default="###", help="")
    parser.add_argument("--test_only", type=int, default=0, help="")
    parser.add_argument("--test_text", default="./domains/DUP/blocks/test0.txt", help="")
    parser.add_argument("--train_word2vec", type=int, default=0, help="")    
    parser.add_argument("--workers", type=int, default=4, help="")
    parser.add_argument("--min_count", type=int, default=1, help="")
    parser.add_argument("--window_size", type=int, default=1, help="")
    parser.add_argument("--word2vec_iter", type=int, default=20, help="")
    parser.add_argument("--iter_num", type=int, default=1, help="")
    parser.add_argument("--learning_rate", type=float, default=0.01, help="")
    parser.add_argument("--blank_percentage", type=float, default=0.05, help="")
    parser.add_argument("--prediction_set_size", type=int, default=10, help="")

    args = parser.parse_args()
    argsDict = args.__dict__
    for k in argsDict.keys():
        print k,argsDict[k]
    SPlan = ShallowPlan(args)
    if args.test_only:
        plans = open(self.test_text).read().split('\n')
        for i, plan in enumerate(plans):
            print "Testing plan %d"%(i+1)
            SPlan.test(plan)
    #SPlan.run()
    end = time.time()
    print "\nTotal time cost: %ds\n"%(end-start)
