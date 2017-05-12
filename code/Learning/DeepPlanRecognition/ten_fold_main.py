#coding:utf-8
import sys
import time
import argparse
import numpy as np
import tensorflow as tf

from Environment import Environment
from ReplayMemory import ReplayMemory
from EADQN import DeepQLearner
from Agent import Agent

def str2bool(v):
  return v.lower() in ("yes", "true", "t", "1")


def ten_fold_split(tb_num):
    mtn = [64, 52, 33, 54, 111, 35, 43]
    # 64 = 6*6 + 7*4
    # 52 = 5*8 + 6*2
    # 33 = 3*7 + 4*3
    # 54 = 5*6 + 6*4
    # 111 = 11*9 + 12*1
    # 35 = 3*5 + 4*5
    # 43 = 4*7 + 5*3
    fd0 = [6,6,6,6,6,6,7,7,7,7]
    fd1 = [5,5,5,5,5,5,5,5,6,6]
    fd2 = [3,3,3,3,3,3,3,4,4,4]
    fd3 = [5,5,5,5,5,5,6,6,6,6]
    fd4 = [11,11,11,11,11,11,11,11,11,12]
    fd5 = [3,3,3,3,3,4,4,4,4,4]
    fd6 = [4,4,4,4,4,4,4,5,5,5]
    fd = [fd0, fd1, fd2, fd3, fd4, fd5, fd6]
    out = []
    #f = open('10-fold-data-indices.txt', 'w+')
    for i in range(len(mtn)):
        flag = np.zeros(mtn[i])
        tmp = []
        for j in range(len(fd[i])):
            sub_tmp = []
            parts = fd[i][j]
            while len(sub_tmp) < parts:
                a = np.random.randint(mtn[i])
                if flag[a] != 0:
                    continue
                sub_tmp.append(a)
                flag[a] = 1
            tmp.append(sub_tmp)
        out.append(tmp)
    '''    
    for k,o in enumerate(out):
        f.write('Total texts of tb%d: %d\n'%(k, mtn[k]))
        for u in o:
            f.write(str(u)+'\n')
            print u
        print '\n'
        f.write('\n')
    f.close()
    '''
    if tb_num >= 0:
        return out[tb_num]
    else:
        return out


def five_fold_split(tb_num):
    mtn = [64, 52, 33, 54, 111, 35, 43]

    fd0 = [12,13,13,13,13]
    fd1 = [10,10,10,11,11]
    fd2 = [6,6,7,7,7]
    fd3 = [10,11,11,11,11]
    fd4 = [22,22,22,22,23]
    fd5 = [7,7,7,7,7]
    fd6 = [8,8,9,9,9]
    fd = [fd0, fd1, fd2, fd3, fd4, fd5, fd6]
    out = []

    for i in range(len(mtn)):
        flag = np.zeros(mtn[i])
        tmp = []
        for j in range(len(fd[i])):
            sub_tmp = []
            parts = fd[i][j]
            while len(sub_tmp) < parts:
                a = np.random.randint(mtn[i])
                if flag[a] != 0:
                    continue
                sub_tmp.append(a)
                flag[a] = 1
            tmp.append(sub_tmp)
        out.append(tmp)

    if tb_num >= 0:
        return out[tb_num]
    else:
        return out


def args_init():
    parser = argparse.ArgumentParser()

    envarg = parser.add_argument_group('Environment')
    envarg.add_argument("--model_dir", default="/home/fengwf/Documents/", help="The directory of word vector model.")
    envarg.add_argument("--vec_model", default='mymodel5-5-50', help="Word vector model name.")
    envarg.add_argument("--vec_length", type=int, default=50, help="Word vector dimension.")
    envarg.add_argument("--actionDB", default='tag_actions', help="Tables' names in database test.")
    envarg.add_argument("--max_text_num", default='64', help="Max text num of database tables.")
    envarg.add_argument("--reward_assign", default='2.0 1.0 -1.0 -2.0', help="How the assign the rewards.")
    envarg.add_argument("--action_rate", type=float, default=0.15, help="Average actions percentage in a text.")
    envarg.add_argument("--penal_radix", type=float, default=5.0, help="Penalty radix according to action rate.")
    envarg.add_argument("--action_label", type=int, default=2, help="An integer refer to the label of actions.")
    envarg.add_argument("--non_action_label", type=int, default=1, help="An integer refer to the label of non-actions.")
    envarg.add_argument("--long_text_flag", type=int, default=1, help="Whether using long texts as input.")

    memarg = parser.add_argument_group('Replay memory')
    memarg.add_argument("--replay_size", type=int, default=100000, help="Maximum size of replay memory.")
    memarg.add_argument("--channel", type=int, default=1, help="Branches of CNN layers.")
    memarg.add_argument("--positive_rate", type=float, default=0.75, help="Choose how many positive examples per batch.")
    memarg.add_argument("--priority", default=1, help="Use the prioritized experience replay or not.")
    memarg.add_argument("--reward_bound", type=float, default=0, help="The boundary between positive examples and negative ones.")

    netarg = parser.add_argument_group('Deep Q-learning network')
    netarg.add_argument("--num_actions", type=int, default=1000, help="Total actions of this task.")
    netarg.add_argument("--words_num", type=int, default=500, help="Total words of an input text.")
    netarg.add_argument("--wordvec", type=int, default=100, help="Width of a text matrix.")
    netarg.add_argument("--learning_rate", type=float, default=0.0025, help="Learning rate.")
    netarg.add_argument("--momentum", type=float, default=0.1, help="Momentum of RMSPropOptimizer.")
    netarg.add_argument("--epsilon", type=float, default=1e-6, help="Epsilon of RMSPropOptimizer.")
    netarg.add_argument("--decay_rate", type=float, default=0.88, help="Decay rate for RMSPropOptimizer.")
    netarg.add_argument("--discount_rate", type=float, default=0.9, help="Discount rate for future rewards.")
    netarg.add_argument("--batch_size", type=int, default=8, help="Batch size for neural network.")
    netarg.add_argument("--target_output", type=int, default=2, help="Output dimension of DQN.")

    antarg = parser.add_argument_group('Agent')
    antarg.add_argument("--exploration_rate_start", type=float, default=1, help="Exploration rate at the beginning of decay.")
    antarg.add_argument("--exploration_rate_end", type=float, default=0.1, help="Exploration rate at the end of decay.")
    antarg.add_argument("--exploration_decay_steps", type=int, default=1000, help="How many steps to decay the exploration rate.")
    antarg.add_argument("--exploration_rate_test", type=float, default=0.0, help="Exploration rate used during testing.")
    antarg.add_argument("--train_frequency", type=int, default=1, help="Perform training after this many game steps.")
    antarg.add_argument("--train_repeat", type=int, default=1, help="Number of times to sample minibatch during training.")
    antarg.add_argument("--target_steps", type=int, default=5, help="Copy main network to target network after this many game steps.")
    antarg.add_argument("--random_play", default=0, help="Whether to perform random play.")

    mainarg = parser.add_argument_group('Main loop')
    mainarg.add_argument("--result_dir", default="test_result", help="The directory of output results.")
    mainarg.add_argument("--train_steps", type=int, default=0, help="How many training steps per epoch.")
    mainarg.add_argument("--test_one", type=int, default=0, help="Test a text")
    mainarg.add_argument("--text_dir", default='', help="A text for testing")
    mainarg.add_argument("--test", type=int, default=1, help="A flag for test after training or not.")
    mainarg.add_argument("--test_text_num", type=int, default=1, help="How many testing steps after each epoch.")
    mainarg.add_argument("--epochs", type=int, default=2, help="How many epochs to run.")
    mainarg.add_argument("--start_epoch", type=int, default=0, help="Start from this epoch.")
    mainarg.add_argument("--home_dir", default="./", help="The root directory of weights to load or save.")
    mainarg.add_argument("--load_weights", default="", help="Load network from file.")
    mainarg.add_argument("--save_weights_prefix", default="", help="Save network to given file.")
    mainarg.add_argument("--computer_id", type=int, default=1, help="An integer, 0 for CPU only computer, else for GPU computer.")
    mainarg.add_argument("--gpu_rate", type=float, default=0.2, help="How much gpu memory to be use.")
    mainarg.add_argument("--cnn_format", default='NCHW', help="The format of tensorflow input matrixes")
    mainarg.add_argument("--texts_num", type=int, default=0, help="Amount of training texts")
    mainarg.add_argument("--ten_fold_flag", type=int, default=0, help="Use 10-fold-cross-validation or not")
    mainarg.add_argument("--fold_test_name", type=list, default=[], help="Use 10-fold-cross-validation or not")

    args = parser.parse_args()
    return args



def main(args):
    if args.load_weights:
        args.exploration_decay_steps = 10

    start = time.time()
    localtime = time.strftime("%Y-%m-%d %H:%M:%S",time.localtime(time.time()))
    print 'Current time is:',localtime
    print 'Starting at main.py...'

    # use for investigating the influence of tag length
    '''
    f = open(args.home_dir + args.result_dir + "_train.txt",'w')
    f1 = open(args.home_dir + args.result_dir + "_test.txt",'w')
    f.write(str(args)+'\n')
    f.write('\nCurrent time is: %s'%localtime)
    f.write('\nStarting at main.py...')
    '''
    #Initial environment, replay memory, deep q net and agent
    gpu_options = tf.GPUOptions(per_process_gpu_memory_fraction=args.gpu_rate)
    with tf.Session(config=tf.ConfigProto(gpu_options=gpu_options)) as sess:
        net = DeepQLearner(args, sess)
        env = Environment(args)

        temp_size = env.train_steps * args.epochs + env.test_steps
        if temp_size > 100000:
            temp_size = 100000
        args.replay_size = temp_size
        args.train_steps = env.train_steps
        assert args.replay_size > 0

        mem = ReplayMemory(args.replay_size, args)
        agent = Agent(env, mem, net, args)

        print '\n',args,'\n'
        
        if args.load_weights:
            print 'Loading weights from %s...'%args.load_weights
            net.load_weights(args.home_dir + args.load_weights)  #load last trained weights


        if args.test_one and args.load_weights:
            ws, act_seq, st = agent.test_one(args.text_dir)
            #f0.write('\nText_vec: %s'%str(env.text_vec))
            print '\nStates: %s\n'%str(st)
            print '\nWords: %s\n'%str(ws)
            print '\n\nAction_squence: %s\n'%str(act_seq)

        else:
            # loop over epochs
            for epoch in xrange(args.start_epoch, args.epochs):
                #print '\n----------epoch: %d----------'%(epoch+1)
                epoch_start = time.time()
                f = open(args.home_dir + args.result_dir + "_train"+ str(epoch) + ".txt",'w')
                f1 = open(args.home_dir + args.result_dir + "_test"+ str(epoch) + ".txt",'w')

                f.write(str(args)+'\n')
                f.write('\nCurrent time is: %s'%localtime)
                f.write('\nStarting at main.py...')
                #print 'env.train_steps: %d'%env.train_steps
                #print 'env.test_steps: %d'%env.test_steps
                #assert 1==0
                if args.train_steps > 0:
                    #agent.train(args.train_steps, epoch)
                    if epoch == args.start_epoch:
                        env.train_init()
                    agent.train(args.train_steps, epoch)
                    if args.save_weights_prefix:
                        filename = args.home_dir + args.save_weights_prefix + "_%d.prm" % (epoch + 1)
                        net.save_weights(filename)

                    cnt = 0
                    ras = 0
                    tas = 0
                    tta = 0

                    for i in range(env.size):#len(env.saved_text_vec)):
                        text_vec_tags = env.saved_text_vec[i,:,-1]
                        state_tags = env.saved_states[i,:,-1]
                        sum_tags = sum(text_vec_tags)
                        if not sum_tags:
                            break
                        count = 0
                        right_actions = 0
                        tag_actions = 0
                        total_actions = 0
                        total_words = args.num_actions/2
                        temp_words = env.saved_text_length[i]
                        if temp_words > total_words:
                            temp_words = total_words

                        #print "text_vec_tags",text_vec_tags
                        #print 'state_tags',state_tags
                        for t in text_vec_tags:
                            if t == args.action_label:
                                total_actions += 1

                        f.write('\n\nText:'+str(i))
                        f.write('\ntotal words: %d\n'%temp_words)
                        print '\ntotal words: %d\n'%temp_words
                        #f.write('\nsaved_text_vec:\n')
                        #f.write(str(env.saved_text_vec[i,:,-1]))
                        #f.write('\nsaved_states:\n')
                        #f.write(str(env.saved_states[i,:,-1]))

                        for s in xrange(temp_words):
                            if state_tags[s] == 0:
                                count += 1
                            elif state_tags[s] == args.action_label:
                                tag_actions += 1
                                if text_vec_tags[s] == state_tags[s]:
                                    right_actions += 1

                        cnt += count
                        ras += right_actions
                        tta += tag_actions
                        tas += total_actions
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
                        f.write('\nWords left: %d'%count)
                        f.write('\nAcions: %d'%total_actions)
                        f.write('\nRight_actions: %d'%right_actions)
                        f.write('\nTag_actions: %d'%tag_actions)
                        f.write('\nActions_recall: %f'%recall)
                        f.write('\nActions_precision: %f'%precision)
                        f.write('\nF_measure: %f'%F_value)
                        print '\nText: %d'%i
                        print '\nWords left: %d'%count
                        print 'Acions: %d'%total_actions
                        print 'Right_actions: %d'%right_actions
                        print 'Tag_actions: %d'%tag_actions
                        print 'Actions_recall: %f'%recall
                        print 'Actions_precision: %f'%precision
                        print 'F_measure: %f'%F_value

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
                    f.write('\nTotal words left: %d'%cnt)
                    f.write('\nTotal acions: %d'%tas)
                    f.write('\nTotal right_acions: %d'%ras)
                    f.write('\nTotal tag_acions: %d'%tta)
                    f.write('\nAverage_actions_recall: %f'%average_recall)
                    f.write('\nAverage_actions_precision: %f'%average_precision)
                    f.write('\nAverage_F_measure: %f'%ave_F_value)
                    print '\nTotal words left: %d'%cnt
                    print 'Total acions: %d'%tas
                    print 'Total right_actions: %d'%ras
                    print 'Total tag_actions: %d'%tta
                    print 'Average_actions_recall: %f'%average_recall
                    print 'Average_actions_precision: %f'%average_precision
                    print 'Average_F_measure: %f'%ave_F_value


                if args.test:
                    f1.write('test_texts: %s\ttexts_num: %d\n'%(str(env.test_text_name), args.test_text_num))
                    agent.test(args.words_num, env.test_steps/args.words_num, f1)

                epoch_end = time.time()
                print 'Total time cost of epoch %d is: %ds'%(epoch, epoch_end-epoch_start)
                f.write('\nTotal time cost of epoch %d is: %ds\n'%(epoch, epoch_end-epoch_start))
                f1.write('\nTotal time cost of epoch %d is: %ds\n'%(epoch, epoch_end-epoch_start))

                f.close()
                f1.close()

        end = time.time()
        print 'Total time cost: %ds'%(end-start)
        localtime = time.strftime("%Y-%m-%d %H:%M:%S",time.localtime(time.time()))
        print 'Current time is: %s'%localtime



if __name__ == '__main__':
    st = time.time()
    #test_list = ten_fold_split(-1)
    test_list = five_fold_split(-1)
    actionDBs = ['tag_actions','tag_actions1','tag_actions2','tag_actions3','tag_actions4','tag_actions5','tag_actions6']
    mtn = [64, 52, 33, 54, 111, 35, 43]
    for k,tl in enumerate(test_list):
        print '\n\nactionDB: %s\ttotal texts: %d\n'%(actionDBs[k], mtn[k])
        for l in tl:
            print l
    for j in range(len(test_list)):
        for i,tl in enumerate(test_list[j]):
            print '\n\ntl = ',tl,'\n\n'
            #if j == 2:
            #    tl = [0, 1, 2]
            args = args_init()
            args.fold_test_name = tl
            args.max_text_num = str(mtn[j])
            args.test_text_num = len(tl)
            args.ten_fold_flag = 1
            if j <= 3:
                args.epochs = 4
            elif j == 4 or j == 5:
                args.epochs = 2
            else:
                args.epochs =3
            args.actionDB = actionDBs[j]
            args.gpu_rate = 0.34
            args.save_weights_prefix = 'weights/ten_fold/tb'+str(j)+'/five_fold_'+str(i) 
            args.result_dir = 'results/basic_results/ten_fold/tb'+str(j)+'/five_fold_'+str(i)
            #args.result_dir = 'results/tmp/'+actionDBs[j]+'_test_fold_'+str(i)

            #argsDict = args.__dict__
            #for eachArg in argsDict.keys():
            #    print eachArg, argsDict[eachArg]
            #time.sleep(1)
            main(args)
            tf.reset_default_graph()
            print '\n\n----- FOLD %d END -----\n\n'%i
    ed = time.time()
    print 'Total time cost of ten-fold-cross-validation : %ds\n'%(ed-st)






