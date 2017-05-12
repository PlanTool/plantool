import os
import numpy as np
import tensorflow as tf
from utils import get_time, save_pkl, load_pkl
from tensorflow.contrib.layers.python.layers import initializers

class DeepQLearner:
    def __init__(self, args, sess):
        print 'Initializing the DQN...'
        self.num_actions = args.num_actions
        self.batch_size = args.batch_size
        self.discount_rate = args.discount_rate
        self.learning_rate = args.learning_rate
        self.decay_rate = args.decay_rate
        self.momentum = args.momentum
        self.epsilon = args.epsilon

        self.channel = args.channel
        self.words_num = args.words_num
        self.wordvec = args.wordvec
        self.target_output = args.target_output
        self.cnn_format = args.cnn_format
        self.sess = sess
        self.build_dqn()


    def conv2d(self, x, output_dim, kernel_size, stride, initializer,
        activation_fn=tf.nn.relu, padding='VALID', name='conv2d'):
        with tf.variable_scope(name):
            if self.cnn_format == 'NCHW':
                stride = [1, 1, stride[0], stride[1]]
                kernel_shape = [kernel_size[0], kernel_size[1], x.get_shape()[1], output_dim]
            elif self.cnn_format == 'NHWC':
                stride = [1, stride[0], stride[1], 1]
                kernel_shape = [kernel_size[0], kernel_size[1], x.get_shape()[-1], output_dim]

            w = tf.get_variable('w', kernel_shape, tf.float32, initializer=initializer)
            conv = tf.nn.conv2d(x, w, stride, padding, data_format=self.cnn_format)

            b = tf.get_variable('biases', [output_dim], initializer=tf.constant_initializer(0.0))
            out = tf.nn.bias_add(conv, b, self.cnn_format)
        if activation_fn != None:
            out = activation_fn(out)
        return out, w, b


    def max_pooling(self, x, kernel_size, stride, name='max_pool'):
        with tf.variable_scope(name):
            if self.cnn_format == 'NCHW':
                stride_shape = [1, 1, stride[0], stride[1]]
                kernel_shape = [1, 1, kernel_size[0], kernel_size[1]]
                return tf.nn.max_pool(x, ksize=kernel_shape, strides=stride_shape, padding="VALID")
            elif self.cnn_format == 'NHWC':
                stride_shape = [1, stride[0], stride[1], 1]
                kernel_shape = [1, kernel_size[0], kernel_size[1], 1]
                return tf.nn.max_pool(x, ksize=kernel_shape, strides=stride_shape, padding="VALID")


    def linear(self, input_, output_size, stddev=0.02, bias_start=0.0, activation_fn=None, name='linear'):
        shape = input_.get_shape().as_list()
        with tf.variable_scope(name):
            w = tf.get_variable('Matrix', [shape[1], output_size], tf.float32,
                tf.random_normal_initializer(stddev=stddev))
                #tf.truncated_normal_initializer(0, stddev))
                
            b = tf.get_variable('bias', [output_size],
                initializer=tf.constant_initializer(bias_start))

            out = tf.nn.bias_add(tf.matmul(input_, w), b)

        if activation_fn != None:
            return activation_fn(out), w, b
        else:
            return out, w, b


    def build_dqn(self):
        branch1 = []
        branch2 = []
        branch3 = []
        branch4 = []
        filter_width = self.wordvec - 1
        filter_num = 32
        self.w = {}
        self.t_w = {}

        #initializer = tf.contrib.layers.xavier_initializer()
        initializer = tf.contrib.layers.xavier_initializer_conv2d()
        #initializer = tf.truncated_normal_initializer(0, 0.02)
        activation_fn = tf.nn.relu

        # training network
        with tf.variable_scope('prediction'):
            print 'Initializing main network...'
            if self.cnn_format == 'NHWC':  #CPU only
                self.s_t = tf.placeholder('float32',
                    [None, self.words_num, self.wordvec, self.channel], name='s_t')
            else:
                self.s_t = tf.placeholder('float32',
                    [None, self.channel, self.words_num, self.wordvec], name='s_t')

            self.l1, self.w['l1_w'], self.w['l1_b'] = self.conv2d(self.s_t,
                filter_num, [2, filter_width], [1, 1], initializer, activation_fn, name='l1')
            self.l2 = self.max_pooling(self.l1, kernel_size = [499, 1], stride = [1, 1], name='l2')
            #self.l2 = tf.nn.max_pool(self.l1, ksize=[1, 499, 1, 1], strides=[1, 1, 1, 1], padding="VALID")

            self.l3, self.w['l3_w'], self.w['l3_b'] = self.conv2d(self.s_t,
                filter_num, [3, filter_width], [1, 1], initializer, activation_fn, name='l3')
            self.l4 = self.max_pooling(self.l3, kernel_size = [498, 1], stride = [1, 1], name='l4')

            self.l5, self.w['l5_w'], self.w['l5_b'] = self.conv2d(self.s_t,
                filter_num, [4, filter_width], [1, 1], initializer, activation_fn, name='l5')
            self.l6 = self.max_pooling(self.l5, kernel_size = [497, 1], stride = [1, 1], name='l6')

            self.l7, self.w['l7_w'], self.w['l7_b'] = self.conv2d(self.s_t,
                filter_num, [5, filter_width], [1, 1], initializer, activation_fn, name='l7')
            self.l8 = self.max_pooling(self.l7, kernel_size = [496, 1], stride = [1, 1], name='l8')


            shape1 = self.l1.get_shape().as_list()
            shape2 = self.l2.get_shape().as_list()
            shape3 = self.l3.get_shape().as_list()
            shape4 = self.l4.get_shape().as_list()
            shape5 = self.l5.get_shape().as_list()
            shape6 = self.l6.get_shape().as_list()
            shape7 = self.l7.get_shape().as_list()
            shape8 = self.l8.get_shape().as_list()
            #print 'shape1',shape1
            #print 'shape2',shape2
            #print 'shape3',shape3
            #print 'shape4',shape4
            #print 'shape5',shape5
            #print 'shape6',shape6
            #print 'shape7',shape7
            #print 'shape8',shape8
            assert shape2 == shape4 == shape6 == shape8
            self.l9 = tf.concat(3, [self.l2, self.l4, self.l6, self.l8])
            shape9 = self.l9.get_shape().as_list()
            self.l9_flat = tf.reshape(self.l9, [-1, reduce(lambda x, y: x * y, shape9[1:])])
            #self.l9_flat = tf.contrib.layers.flatten(self.l9)
            

            self.l10, self.w['l10_w'], self.w['l10_b'] = self.linear(
                self.l9_flat, 256, activation_fn=activation_fn, name='l10')
            self.q, self.w['q_w'], self.w['q_b'] = self.linear(
                self.l10, self.target_output, name='q')

            
            shape9_flat = self.l9_flat.get_shape().as_list()
            shape10 = self.l10.get_shape().as_list()
            shape_q = self.q.get_shape().as_list()
            #print 'shape9',shape9
            #print 'shape9_flat',shape9_flat
            #print 'shape10',shape10
            #print 'shape_q',shape_q
            #assert 1==0

        # target network
        with tf.variable_scope('target'):
            print 'Initializing target network...'
            if self.cnn_format == 'NHWC':
                self.target_s_t = tf.placeholder('float32',
                    [None, self.words_num, self.wordvec, self.channel], name='s_t')
            else:
                self.target_s_t = tf.placeholder('float32',
                    [None, self.channel, self.words_num, self.wordvec], name='s_t')

            self.target_l1, self.t_w['l1_w'], self.t_w['l1_b'] = self.conv2d(self.target_s_t,
                filter_num, [2, filter_width], [1, 1], initializer, activation_fn, name='l1')
            self.target_l2 = self.max_pooling(self.target_l1, 
                kernel_size = [499, 1], stride = [1, 1], name='l2')

            self.target_l3, self.t_w['l3_w'], self.t_w['l3_b'] = self.conv2d(self.target_s_t,
                filter_num, [3, filter_width], [1, 1], initializer, activation_fn, name='l3')
            self.target_l4 = self.max_pooling(self.target_l3, 
                kernel_size = [498, 1], stride = [1, 1], name='l4')

            self.target_l5, self.t_w['l5_w'], self.t_w['l5_b'] = self.conv2d(self.target_s_t,
                filter_num, [4, filter_width], [1, 1], initializer, activation_fn, name='l5')
            self.target_l6 = self.max_pooling(self.target_l5, 
                kernel_size = [497, 1], stride = [1, 1], name='l6')

            self.target_l7, self.t_w['l7_w'], self.t_w['l7_b'] = self.conv2d(self.target_s_t,
                filter_num, [5, filter_width], [1, 1], initializer, activation_fn, name='l7')
            self.target_l8 = self.max_pooling(self.target_l7, 
                kernel_size = [496, 1], stride = [1, 1], name='l8')


            shape1 = self.target_l2.get_shape().as_list()
            shape2 = self.target_l4.get_shape().as_list()
            shape3 = self.target_l6.get_shape().as_list()
            shape4 = self.target_l8.get_shape().as_list()
            #print '\ntarget_shape1',shape1
            assert shape1 == shape2 == shape3 == shape4
            self.target_l9 = tf.concat(3, [self.target_l2, self.target_l4, self.target_l6, self.target_l8])
            shape = self.target_l9.get_shape().as_list()
            #print 'target_shape9',shape
            self.target_l9_flat = tf.reshape(self.target_l9, [-1, reduce(lambda x, y: x * y, shape[1:])])

            self.target_l10, self.t_w['l10_w'], self.t_w['l10_b'] = self.linear(
                self.target_l9_flat, 256, activation_fn=activation_fn, name='l10')
            self.target_q, self.t_w['q_w'], self.t_w['q_b'] = self.linear(
                self.target_l10, self.target_output, name='q')

            shape_target_l10 = self.target_l10.get_shape().as_list()
            #print 'shape_target_l10',shape_target_l10
            shape_target_q = self.target_q.get_shape().as_list()
            #print 'shape_target_q',shape_target_q



        with tf.variable_scope('pred_to_target'):
            print 'Initializing pred_to_target...'
            self.t_w_input = {}
            self.t_w_assign_op = {}

            for name in self.w.keys():
                self.t_w_input[name] = tf.placeholder('float32', self.t_w[name].get_shape().as_list(), name=name)
                self.t_w_assign_op[name] = self.t_w[name].assign(self.t_w_input[name])


        # optimizer
        with tf.variable_scope('optimizer'):
            print 'Initializing optimizer...'
            self.target_q_t = tf.placeholder('float32', [self.batch_size, self.target_output], name='target_q_t')
            self.delta = self.target_q_t - self.q
            #self.loss = tf.reduce_mean(tf.square(self.delta), name='loss')
            self.loss = tf.reduce_sum(tf.square(self.delta), name='loss')
            #self.optim = tf.train.GradientDescentOptimizer(self.learning_rate).minimize(self.loss)
            self.optim = tf.train.RMSPropOptimizer(
                self.learning_rate, decay=self.decay_rate, momentum=self.momentum, epsilon=self.epsilon).minimize(self.loss)


        tf.initialize_all_variables().run()
        #assert 1==0


    def update_target_network(self):
        for name in self.w.keys():
            self.t_w_assign_op[name].eval({self.t_w_input[name]: self.w[name].eval()})


    def train(self, minibatch, epoch):
        # expand components of minibatch
        prestates, actions, rewards, poststates, terminals = minibatch
        assert len(prestates.shape) == 4
        assert len(poststates.shape) == 4
        assert len(actions.shape) == 1
        assert len(rewards.shape) == 1
        assert len(terminals.shape) == 1
        assert prestates.shape == poststates.shape
        assert prestates.shape[0] == actions.shape[0] == rewards.shape[0] == poststates.shape[0] == terminals.shape[0]
        
        if self.cnn_format == 'NHWC':
            post_input = np.transpose(poststates, axes=(0, 2, 3, 1))
        else:
            post_input = poststates
        #print 'poststates[0][0][0]',poststates[0][0][0]
        #print '\nnp.max(poststates)',np.max(poststates)
        postq = self.target_q.eval({self.target_s_t: post_input})
        #print '\npostq.shape',postq.shape
        #print 'postq',postq
        assert postq.shape == (self.batch_size, self.target_output)
        
        # calculate max Q-value for each poststate  
        maxpostq = np.max(postq, axis=1)
        #print '\nmaxpostq.shape',maxpostq.shape
        #print 'maxpostq',maxpostq
        assert maxpostq.shape == (self.batch_size,)
        
        if self.cnn_format == "NHWC":
            pre_input = np.transpose(prestates, axes=(0, 2, 3, 1))
        else:
            pre_input = prestates
        #print '\nnp.max(prestates)',np.max(prestates)
        preq = self.q.eval({self.s_t: pre_input})
        #print '\npreq.shape',preq.shape
        #print 'preq',preq
        assert preq.shape == (self.batch_size, self.target_output)
        

        # make copy of prestate Q-values as targets  
        targets = preq.copy()

        # update Q-value targets for actions taken  
        #print 'actions',actions
        #print 'rewards',rewards
        for i, action in enumerate(actions):
            if terminals[i]:  
                targets[i, action%2] = float(rewards[i])
            else:  
                targets[i, action%2] = float(rewards[i]) + self.discount_rate * maxpostq[i]
        #print '\ntargets',targets
        
        

        _, q_t, delta, loss = self.sess.run([self.optim, self.q, self.delta, self.loss], {
            self.target_q_t: targets, self.s_t: pre_input,})
        #print 'delta',delta
        print 'Current loss = ',loss
        #assert 1==0
        
        


    def predict(self, states):
        # minibatch is full size, because Neon doesn't let change the minibatch size
        assert states.shape == (self.batch_size, self.channel, self.words_num, self.wordvec)
        #print '\n33333  predicting  33333\n'

        if self.cnn_format == 'NHWC':
            state_input = np.transpose(states, axes=(0, 2, 3, 1))
        else:
            state_input = states

        qvalues = self.q.eval({self.s_t: state_input})

        return qvalues



    def save_weights(self, weight_dir):
        if not os.path.exists(weight_dir):
            os.makedirs(weight_dir)

        for name in self.w.keys():
            save_pkl(self.w[name].eval(), os.path.join(weight_dir, "%s.pkl" % name))


    def load_weights(self, weight_dir, cpu_mode=False):
        with tf.variable_scope('load_pred_from_pkl'):
            self.w_input = {}
            self.w_assign_op = {}

            for name in self.w.keys():
                self.w_input[name] = tf.placeholder('float32', self.w[name].get_shape().as_list(), name=name)
                self.w_assign_op[name] = self.w[name].assign(self.w_input[name])

        for name in self.w.keys():
            self.w_assign_op[name].eval({self.w_input[name]: load_pkl(os.path.join(weight_dir, "%s.pkl" % name))})

        self.update_target_network()


if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--num_actions", type=int, default=1000, help="Total actions of this task.")
    parser.add_argument("--words_num", type=int, default=500, help="Total words of an input text.")
    parser.add_argument("--wordvec", type=int, default=100, help="Size of word vector.")
    parser.add_argument("--channel", type=int, default=1, help="Channels of CNN layers.")
    parser.add_argument("--learning_rate", type=float, default=0.0025, help="Learning rate.")
    parser.add_argument("--decay_rate", type=float, default=0.95, help="Decay rate for RMSProp and Adadelta algorithms.")
    parser.add_argument("--discount_rate", type=float, default=0.9, help="Discount rate for future rewards.")
    parser.add_argument("--batch_size", type=int, default=8, help="Batch size for neural network.")
    parser.add_argument("--target_output", type=int, default=2, help="Output dimension of DQN.")
    args = parser.parse_args()

    gpu_options = tf.GPUOptions(per_process_gpu_memory_fraction=0.25)
    with tf.Session(config=tf.ConfigProto(gpu_options=gpu_options)) as sess:
        net = DeepQLearner(args, sess)
