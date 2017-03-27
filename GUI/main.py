#coding:utf-8
import argparse
import sys
import time
import tensorflow as tf

from Environment import Environment
from ReplayMemory import ReplayMemory
from EADQN import DeepQLearner
from Agent import Agent

def str2bool(v):
  return v.lower() in ("yes", "true", "t", "1")

parser = argparse.ArgumentParser()

envarg = parser.add_argument_group('Environment')
envarg.add_argument("--model_dir", default="./models/word2vec_model/", help="The directory of word vector model.")
envarg.add_argument("--vec_model", default='mymodel5-5-50', help="Word vector model name.")
envarg.add_argument("--vec_length", type=int, default=50, help="Word vector dimension.")
envarg.add_argument("--actionDB", default='tag_actions1', help="Tables' names in database test.")
envarg.add_argument("--max_text_num", default='64', help="Max text num of database tables.")
envarg.add_argument("--reward_assign", default='2.0 1.0 -1.0 -2.0', help="How the assign the rewards.")
envarg.add_argument("--action_rate", type=float, default=0.15, help="Average actions percentage in a text.")
envarg.add_argument("--penal_radix", type=float, default=5.0, help="Penalty radix according to action rate.")
envarg.add_argument("--action_label", type=int, default=2, help="An integer refer to the label of actions.")
envarg.add_argument("--non_action_label", type=int, default=1, help="An integer refer to the label of non-actions.")

memarg = parser.add_argument_group('Replay memory')
memarg.add_argument("--replay_size", type=int, default=10000, help="Maximum size of replay memory.")
memarg.add_argument("--channel", type=int, default=1, help="Branches of CNN layers.")
memarg.add_argument("--positive_rate", type=float, default=0.75, help="Choose how many positive examples per batch.")
memarg.add_argument("--priority", default=1, help="Use the prioritized experience replay or not.")
memarg.add_argument("--reward_bound", type=float, default=0, help="The boundary between positive examples and negative ones.")

netarg = parser.add_argument_group('Deep Q-learning network')
netarg.add_argument("--num_actions", type=int, default=1000, help="Total actions of this task.")
netarg.add_argument("--words_num", type=int, default=500, help="Total words of an input text.")
netarg.add_argument("--wordvec", type=int, default=100, help="Size of word vector.")
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
mainarg.add_argument("--result_dir", default="./outputs/DPR_result", help="The directory of output results.")
mainarg.add_argument("--train_steps", type=int, default=28000, help="How many training steps per epoch.")
mainarg.add_argument("--test_one", type=int, default=1, help="Test a text")
mainarg.add_argument("--text_dir", default='./test_inputs/1.txt', help="A text for testing")
mainarg.add_argument("--train_test", type=int, default=1, help="Test after training.")
mainarg.add_argument("--test_text_num", type=int, default=8, help="How many texts for texting.")
mainarg.add_argument("--epochs", type=int, default=2, help="How many epochs to run.")
mainarg.add_argument("--start_epoch", type=int, default=0, help="Start from this epoch, affects exploration rate and names of saved snapshots.")
mainarg.add_argument("--load_weights", default="./models/DPR_model/tb0_1.prm", help="Load network from file.")
mainarg.add_argument("--save_weights_prefix", default="./models/DPR_model/tb0", help="Save network to given file. Epoch and extension will be appended.")
mainarg.add_argument("--use_gpu", type=int, default=1, help="An integer, 0 for CPU only computer, else for GPU computer.")
mainarg.add_argument("--gpu_rate", type=float, default=0.2, help="How much gpu memory to be use.")
mainarg.add_argument("--cnn_format", default='NHWC', help="The format of tensorflow input matrixes")


args = parser.parse_args()
tables_num = len(args.actionDB.split())


def main(args, tables_num):
    f0 = open(args.result_dir + "_main.txt",'w')
    if args.use_gpu:
        args.cnn_format = 'NCHW'
        temp_size = args.train_steps * args.epochs + args.train_test*args.words_num*args.test_text_num*tables_num
        if temp_size > 100000:  #max size of replay memory
            args.replay_size = 100000
        else:
            args.replay_size = temp_size

    if args.load_weights:
        args.exploration_decay_steps = 10

    if args.words_num != 500:
        args.num_actions = 2*args.words_num
    print args,'\n'
    f0.write(str(args)+'\n')
    #assert 1 == 0


    start = time.time()
    localtime = time.strftime("%Y-%m-%d %H:%M:%S",time.localtime(time.time()))
    print 'Current time is:',localtime
    print 'Starting at main.py...'
    f0.write('\nCurrent time is: %s'%localtime)
    f0.write('\nStarting at main.py...')

    #Initial environment, replay memory, deep q net and agent
    gpu_options = tf.GPUOptions(per_process_gpu_memory_fraction=args.gpu_rate)
    with tf.Session(config=tf.ConfigProto(gpu_options=gpu_options)) as sess:
        net = DeepQLearner(args, sess)
        env = Environment(args)
        mem = ReplayMemory(args.replay_size, args)
        agent = Agent(env, mem, net, args)


        if args.load_weights:
            print 'Loading weights from %s...'%args.load_weights
            f0.write('\nLoading weights from %s...'%args.load_weights)
            net.load_weights(args.load_weights)  #load last trained weights

        if args.test_one:
            ws, act_seq, st = agent.test_one(args.text_dir)
            #f0.write('\nText_vec: %s'%str(env.text_vec))
            f0.write('\nStates: %s\n'%str(st))
            f0.write('\nWords: %s\n'%str(ws))
            f0.write('\n\nAction_squence: %s\n'%str(act_seq))  
     
        else:# loop over epochs
            for epoch in xrange(args.start_epoch, args.epochs):
                #print '\n----------epoch: %d----------'%(epoch+1)
                epoch_start = time.time()
                f = open(args.result_dir + "_train"+ str(epoch) + ".txt",'w')
                f1 = open(args.result_dir + "_test"+ str(epoch) + ".txt",'w')

                f.write(str(args)+'\n')
                f.write('\nCurrent time is: %s'%localtime)
                f.write('\nStarting at main.py...')
                if args.train_steps:
                    agent.train(args.train_steps, epoch)
                    if args.save_weights_prefix:
                        filename = args.save_weights_prefix + "_%d.prm" % (epoch + 1)
                        net.save_weights(filename)

                if args.train_steps > 0:
                    if args.train_steps <= 100:
                        print "**********It's quickly testing the code now.**********"
                        print '\n----------- Environment restart ----------'
                        env.restart()
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

                        print "text_vec_tags",text_vec_tags
                        print 'state_tags',state_tags
                        for t in text_vec_tags:
                            if t == args.action_label:
                                total_actions += 1

                        f.write('\n\nText:'+str(i))
                        f.write('\ntotal words: %d\n'%temp_words)
                        print '\ntotal words: %d\n'%temp_words
                        f.write('\nsaved_text_vec:\n')
                        f.write(str(env.saved_text_vec[i,:,-1]))
                        f.write('\nsaved_states:\n')
                        f.write(str(env.saved_states[i,:,-1]))

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


                if args.train_test:
                    agent.train_test(args.words_num, args.test_text_num*tables_num, f1)

                epoch_end = time.time()
                print 'Total time cost of epoch %d is: %ds'%(epoch, epoch_end-epoch_start)
                f.write('\nTotal time cost of epoch %d is: %ds\n'%(epoch, epoch_end-epoch_start))

                f.close()
                f1.close()

        end = time.time()
        print 'Total time cost: %ds'%(end-start)
        f0.write('\nTotal time cost: %ds'%(end-start))
        localtime = time.strftime("%Y-%m-%d %H:%M:%S",time.localtime(time.time()))
        print 'Current time is: %s'%localtime
        f0.write('\nCurrent time is: %s\n'%localtime)
        f0.close() 


if __name__ == '__main__':
    main(args, tables_num)