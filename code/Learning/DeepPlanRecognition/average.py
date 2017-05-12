#coding:utf-8
import re
import os
import nltk
import time
import mysql.connector as mc


def str2bool(v):
      return v.lower() in ("yes", "true", "t", "1")


def EADQN_main(table, num, weights_dir):#actionDBs, num):
    import argparse
    import sys
    import time
    import tensorflow as tf

    from Environment import Environment
    from ReplayMemory import ReplayMemory
    from EADQN import DeepQLearner
    from Agent import Agent
    parser = argparse.ArgumentParser()

    envarg = parser.add_argument_group('Environment')
    envarg.add_argument("--model_dir", default="/home/fengwf/Documents/", help="")
    envarg.add_argument("--vec_model", default='mymodel5-5-50', help="")
    envarg.add_argument("--vec_length", type=int, default=50, help="")
    envarg.add_argument("--actionDB", default='tag_actions', help="")
    envarg.add_argument("--max_text_num", default='64', help="")
    envarg.add_argument("--reward_assign", default='2.0 1.0 -1.0 -2.0', help="")
    envarg.add_argument("--action_rate", type=float, default=0.15, help="")
    envarg.add_argument("--penal_radix", type=float, default=5.0, help="")
    envarg.add_argument("--action_label", type=int, default=2, help="")
    envarg.add_argument("--non_action_label", type=int, default=1, help="")
    envarg.add_argument("--long_text_flag", type=int, default=1, help="")

    memarg = parser.add_argument_group('Replay memory')
    memarg.add_argument("--replay_size", type=int, default=100000, help="")
    memarg.add_argument("--channel", type=int, default=1, help="")
    memarg.add_argument("--positive_rate", type=float, default=0.75, help="")
    memarg.add_argument("--priority", default=1, help="")
    memarg.add_argument("--reward_bound", type=float, default=0, help="")

    netarg = parser.add_argument_group('Deep Q-learning network')
    netarg.add_argument("--num_actions", type=int, default=1000, help="")
    netarg.add_argument("--words_num", type=int, default=500, help="")
    netarg.add_argument("--wordvec", type=int, default=100, help="")
    netarg.add_argument("--learning_rate", type=float, default=0.0025, help="")
    netarg.add_argument("--momentum", type=float, default=0.1, help="")
    netarg.add_argument("--epsilon", type=float, default=1e-6, help="")
    netarg.add_argument("--decay_rate", type=float, default=0.88, help="")
    netarg.add_argument("--discount_rate", type=float, default=0.9, help="")
    netarg.add_argument("--batch_size", type=int, default=8, help="")
    netarg.add_argument("--target_output", type=int, default=2, help="")

    antarg = parser.add_argument_group('Agent')
    antarg.add_argument("--exploration_rate_start", type=float, default=1, help="")
    antarg.add_argument("--exploration_rate_end", type=float, default=0.1, help="")
    antarg.add_argument("--exploration_decay_steps", type=int, default=1000, help="")
    antarg.add_argument("--exploration_rate_test", type=float, default=0.0, help="")
    antarg.add_argument("--train_frequency", type=int, default=1, help="")
    antarg.add_argument("--train_repeat", type=int, default=1, help="")
    antarg.add_argument("--target_steps", type=int, default=5, help="")
    antarg.add_argument("--random_play", default=0, help="")

    mainarg = parser.add_argument_group('Main loop')
    mainarg.add_argument("--result_dir", default="test_result", help="")
    mainarg.add_argument("--train_steps", type=int, default=0, help="")
    mainarg.add_argument("--test_one", type=int, default=1, help="")
    mainarg.add_argument("--text_dir", default='', help="")
    mainarg.add_argument("--test", type=int, default=1, help="")
    mainarg.add_argument("--test_text_num", type=int, default=8, help="")
    mainarg.add_argument("--epochs", type=int, default=2, help="")
    mainarg.add_argument("--start_epoch", type=int, default=0, help="")
    mainarg.add_argument("--home_dir", default="./", help="")
    mainarg.add_argument("--load_weights", default="", help="")
    mainarg.add_argument("--save_weights_prefix", default="", help="")
    mainarg.add_argument("--computer_id", type=int, default=1, help="")
    mainarg.add_argument("--gpu_rate", type=float, default=0.2, help="")
    mainarg.add_argument("--cnn_format", default='NCHW', help="")

    args = parser.parse_args()
    tables_num = len(args.actionDB.split())
    args.load_weights = weights_dir
    gpu_options = tf.GPUOptions(per_process_gpu_memory_fraction=args.gpu_rate)
    with tf.Session(config=tf.ConfigProto(gpu_options=gpu_options)) as sess:
        net = DeepQLearner(args, sess)
        env = Environment(args)
        mem = ReplayMemory(args.replay_size, args)
        agent = Agent(env, mem, net, args)   
        words = []
        states = []     

        if args.load_weights:
            print 'Loading weights from %s...'%args.load_weights
            net.load_weights(args.home_dir + args.load_weights)  #load last trained weights

        if args.test_one and args.load_weights:
            '''
            for i,ad in enumerate(actionDBs):
                tmp_w = []
                tmp_s = []
                for j in range(num[i]):
                    print 'table = %s,  text_num = %d'%(actionDBs[i],j)
                    ws, act_seq, st = agent.test_one_db(actionDBs[i], j)
                    tmp_w.append(ws)
                    tmp_s.append(st)
                    #print '\nStates: %s\n'%str(st)
                    #print '\nWords: %s\n'%str(ws)
                    #print '\n\nAction_squence: %s\n'%str(act_seq)
                words.append(tmp_w)
                states.append(tmp_s)
            '''
            tmp_w = []
            tmp_s = []
            for j in range(num):
                #print 'table = %s,  text_num = %d'%(table,j)
                ws, act_seq, st = agent.test_one_db(table, j)
                tmp_w.append(ws)
                tmp_s.append(st)
            words = tmp_w
            states = tmp_s    
            print 'len(words) = %d,  len(states) = %d'%(len(words),len(states))
        return words, states


def stanford_find_vp(sent, dep_parser):
    stf_vp = []
    if len(sent.split()) <= 1:
        return stf_vp
    result = dep_parser.raw_parse(sent)
    dep = result.next()
    for t in list(dep.triples()):
        assert  len(t) == 3
        relation = str(t[1])
        if relation in ['dobj', 'nsubjpass']:
            word1 = str(t[0][0])
            tag1 = str(t[0][1])
            word2 = str(t[2][0])
            tag2 = str(t[2][1])
            stf_vp.append(word1)
    return stf_vp


def RegexpParser_init():
    grammar=r"""VP:{<NN|NNS|PRP|NNP|NNPS><RB>?<MD><RB>?<VB><RB>?<VBD|VBG|VBN>}
    VP:{<NN|NNS|PRP|NNP|NNPS><RB>?<VBP><RB>?<VBN><RB>?<VBD|VBN>}
    VP:{<NN|NNS|PRP|NNP|NNPS><RB>?<VBZ><RB>?<VBD|VBG|VBN>}
    VP:{<VB|VBD|VBG|VBN|VBP><RB>?<PRP\$|DT>?<VBN|VBD|JJ>?<NN|NNS|NNP|NNPS>+}
    VP:{<VB|VBD|VBG|VBN|VBP><RB>?<PRP\$|DT>?<VBN|VBD|JJ>?<PRP>}
    VP:{<VB|VBD|VBG|VBN|VBP><IN><DT>?<NN|NNS>+}
    VP:{<VB|VBD|VBG|VBN|VBP><IN>?<DT>?<CD>?<VBN|VBD|JJ>?<NN|NNS|NNP|NNPS>+}
    VP:{<VB|VBD|VBG|VBN|VBP><IN>?<DT>?<CD>?<VBN|VBD|JJ>?<PRP>}
    VP:{<VB|VBD|VBG|VBN|VBP><IN><VBN|VBD><NN|NNS>+}
    """
    cp = nltk.RegexpParser(grammar)
    pron_tag = ["it", "they", "them", "this", "that", "these", "those"]
    noun_tag = ['NN','NNS','NNP','NNPS','PRP']
    verb_tag = ['VB','VBD','VBG','VBN','VBP']
    return cp, pron_tag, noun_tag, verb_tag


def regexp_find_vp(sent, cp, pron_tag, noun_tag, verb_tag):
    temp_myvp = []
    obj = []
    result = cp.parse(sent)  
    b = re.findall(r'\(VP.+\)',str(result)) 
    for c in b:
        #d = re.findall(r'[\w|\-]+\$?',c)#所有的字母以及符号$
        d = re.findall(r'[A-Za-z-\']+\$?|\d+/\d+',c)#所有的字母以及符号$，don't，20/20这些类型的
        if len(d) % 2 == 0:#去除分错的
            continue
        #if i ==1454:  print d
        #print d[1],d[len(d)-2]
        if d[2] in verb_tag and d[3] in ['if']:
            continue
        if d[2] in verb_tag:#主动句，第一个单词是动词
            if d[4] == 'NN' and d[3] == 't':#排除don't等缩写的影响
                pass
            else:
                j = 4
                obj.append(d[1])
                while j < len(d):
                    if d[j] in noun_tag:
                        #print 'd[j]: ',d[j]
                        obj.append(d[j-1])
                    j += 2
                print 'dobj:',obj
                temp_myvp.append(obj[0])
                obj = []
        elif d[2] in pron_tag:#被动句，第一个单词是名词性主语
            obj = [d[len(d)-2],d[1]]
            print 'idobj:',obj
            temp_myvp.append(obj[0])
            obj = []
    return temp_myvp


def multi_meanings(actionDBs, num, wdir, out_name, db, cur):
    import nltk
    from nltk.parse.stanford import StanfordDependencyParser, StanfordParser
    from nltk.tag import StanfordPOSTagger

    import sys
    reload(sys)
    sys.setdefaultencoding('gb18030')

    pos_jar = '/home/fengwf/stanford/postagger/stanford-postagger.jar'
    pos_model = '/home/fengwf/stanford/postagger/models/english-bidirectional-distsim.tagger'
    path_to_jar = '/home/fengwf/stanford/stanford-corenlp-3.7.0.jar'
    models_jar = '/home/fengwf/stanford/english-models.jar'
    dep_parser = StanfordDependencyParser(path_to_jar=path_to_jar, path_to_models_jar=models_jar)
    #std_parser = StanfordParser(path_to_jar=path_to_jar, path_to_models_jar=models_jar)
    pos_tagger = StanfordPOSTagger(pos_model, pos_jar)

    verb_tag = ['VB','VBD','VBG','VBN','VBP']
    #cp, pron_tag, noun_tag, verb_tag = RegexpParser_init()

    all_wrg_act = []
    all_nlp_right = []
    all_nlp_wrong = []
    all_dqn_right = []
    all_dqn_wrong = []
    all_total_act = []
    for i,ad in enumerate(actionDBs):
        ad_start = time.time()
        f = open(out_name, 'w+')
        wrg_act = {}
        nlp_right = 0
        nlp_wrong = 0
        dqn_right = 0
        dqn_wrong = 0
        total_act = 0

        eadqn_words, eadqn_tags = EADQN_main(ad, num[i], wdir)

        for j in range(num[i]):
            if j == 30 and ad == 'tag_actions':
                continue
            #elif (j == 34 or j == 49) and ad == 'tag_actions4':
            #    continue
            get_data = 'select * from ' + ad + ' where text_num = ' + str(j)
            print '\n%s\n'%get_data

            cur.execute(get_data)
            result = cur.fetchall()
            t_ws = sum([len(re.split(r' ',tmp[2])) for tmp in result])
            t_ts = sum([len(re.split(r' ',aaa[3])) for aaa in result])
            if t_ws != len(eadqn_words[j]):
                print '----- t_ws = %d,  len(eadqn_words[%d] = %d) -----'%\
                (t_ws, j, len(eadqn_words[j]))
                continue
            pointer = 0
            sent_num = len(result)
            for l in range(sent_num):
                r = result[l]
                print 'text %d of %d,  sentence %d of %d'%(j, num[i], l, sent_num)
                sent = re.split(r' ',r[2])
                tags = re.split(r' ',r[3])
                tagged_sent = pos_tagger.tag(sent)
                #myvp = regexp_find_vp(tagged_sent, cp, pron_tag, noun_tag, verb_tag)
                stf_vp = stanford_find_vp(r[2], dep_parser)
                for k,t in enumerate(tags):
                    if t == '1':
                        total_act += 1
                        print 'total_act = %d'%total_act
                        if tagged_sent[k][0] in stf_vp:
                            nlp_right += 1
                            stf_vp.remove(tagged_sent[k][0])
                        else:
                            nlp_wrong += 1
                            if tagged_sent[k] not in wrg_act.keys():
                                wrg_act[tagged_sent[k]] = 1
                            else:
                                wrg_act[tagged_sent[k]] += 1
                        print 'j = %d,  pointer = %d'%(j ,pointer)
                        if int(eadqn_tags[j][pointer]) == 2:
                            dqn_right += 1
                        else:
                            dqn_wrong += 1
                    pointer += 1
                    if pointer >= 500:
                        break
                if pointer >= 500:
                    break
            print '\nlen(wrg_act) = %d,  total_act = %d'%(len(wrg_act), total_act)
            print 'nlp_right = %d,  nlp_wrong = %d,  dqn_right = %d,  dqn_wrong = %d'%\
            (nlp_right, nlp_wrong, dqn_right, dqn_wrong)

        f.write('len(wrg_act) = %d,  total_act = %d\n'%(len(wrg_act), total_act))
        f.write('nlp_right = %d,  nlp_wrong = %d,  dqn_right = %d,  dqn_wrong = %d\n'%\
            (nlp_right, nlp_wrong, dqn_right, dqn_wrong))

        sort_wrg_act = sorted(wrg_act.items(), key=lambda a:a[1], reverse=True)
        top_range = 20
        if len(wrg_act) < 20:
            top_range = len(wrg_act)
        f.write('\n\nTop %d items of sort_wrg_act:\n'%top_range)
        for m in range(top_range):
            print sort_wrg_act[m]
            f.write(str(sort_wrg_act[m])+'\n')
        ad_end = time.time()
        f.write('\n\nTotal time cost: %ds\n'%(ad_end - ad_start))
        f.close()
        all_total_act.append(total_act)
        all_nlp_right.append(nlp_right)
        all_nlp_wrong.append(nlp_wrong)
        all_dqn_right.append(dqn_right)
        all_dqn_wrong.append(dqn_wrong)


def compute_pr_results():
    source_dir = './results/pr_results/tb3/'
    tn = '40'
    to = []
    ri = []
    ta = []
    for i in range(2, 20):
        total = 0
        right = 0
        tagged = 0
        for j in range(1, 6):
            text_name = 'pr' + str(i*5) + '_tn' + tn + '_result' + str(j) + '_test1.txt'
            if not os.path.isfile(source_dir+text_name):
                continue
            for line in open(source_dir+text_name):
                if re.search(r' acions', line):
                    temp_total = re.findall(r'\d+', line)
                    if temp_total:
                        print 'temp_total = %s\n'%temp_total[0]
                        total += int(temp_total[0])
                elif re.search(r' right_acions', line):
                    temp_right = re.findall(r'\d+', line)
                    if temp_right:
                        print 'temp_right = %s\n'%temp_right[0]
                        right += int(temp_right[0])
                elif re.search(r' tag_acions', line):
                    temp_tagged = re.findall(r'\d+', line)
                    if temp_tagged:
                        print 'temp_tagged = %s\n'%temp_tagged[0]
                        tagged += int(temp_tagged[0])
        to.append(total)
        ri.append(right)
        ta.append(tagged)
    assert len(to) == len(ri) == len(ta)
    print '\ntotal:',to
    print '\nright:',ri
    print '\ntagged:',ta

    r = [float(ri[k])/to[k] for k in range(len(to))]
    p = [float(ri[k])/ta[k] for k in range(len(to))]
    f = [2*p[k]*r[k]/(p[k]+r[k]) for k in range(len(to))]
    #print '\nrecall:',r
    #print '\nprecision:',p
    #print '\nf-measure:',f
    for i in range(len(to)):
        print 'positive_rate = %d, f-measure = %f'%((i+2)*5, f[i])        


def compute_tag_length_results(table, start, stop):
    source_dir = './results/tag_length_results/new/' + table
    name = ['2', '5', '7', '10', '15', '20', '25', '30',
    '50', '75', '100', '125', '150', '175', '200']
    to = []
    ri = []
    ta = []
    print source_dir
    for i in range(len(name)):
        total = 0
        right = 0
        tagged = 0
        for j in range(start, stop+1):
            text_name = '/50_' + name[i] + '_' + str(j) + '_test1.txt'
            if not os.path.isfile(source_dir+text_name):
                continue
            #print text_name
            for line in open(source_dir+text_name):
                if re.search(r' acions', line):
                    temp_total = re.findall(r'\d+', line)
                    if temp_total:
                        #print 'temp_total = %s\n'%temp_total[0]
                        total += int(temp_total[0])
                elif re.search(r' right_acions', line):
                    temp_right = re.findall(r'\d+', line)
                    if temp_right:
                        #print 'temp_right = %s\n'%temp_right[0]
                        right += int(temp_right[0])
                elif re.search(r' tag_acions', line):
                    temp_tagged = re.findall(r'\d+', line)
                    if temp_tagged:
                        #print 'temp_tagged = %s\n'%temp_tagged[0]
                        tagged += int(temp_tagged[0])
        to.append(total)
        ri.append(right)
        ta.append(tagged)
    assert len(to) == len(ri) == len(ta)
    print '\ntotal:',to
    print '\nright:',ri
    print '\ntagged:',ta

    f = []
    for k in range(len(to)):
        if ta[k] == 0:
            p = 0
        else:
            p = float(ri[k])/ta[k]
        if to[k] == 0:
            r = 0
        else:
            r = float(ri[k])/to[k]
        if p + r == 0:
            f.append(0)
        else:
            f.append(2*r*p/(p+r))
    #r = [float(ri[k])/to[k] for k in range(len(to))]
    #p = [float(ri[k])/ta[k] for k in range(len(to))]
    #f = [2*p[k]*r[k]/(p[k]+r[k]) for k in range(len(to))]
    #print '\nrecall:',r
    #print '\nprecision:',p
    #print '\nf-measure:',f
    for i in range(len(to)):
        #print 'tag_length = %s, f-measure = %f'%(name[i], f[i]) 
        print f[i]


def compute_texts_results(table, start, stop):
    source_dir = './results/texts_results/ep2/' + table
    name = ['2', '5', '10', '20', '30', '40', '50', '60', '70', '80', '90', '100']
    to = []
    ri = []
    ta = []
    ac = []
    print source_dir
    for i in range(len(name)):
        total = 0
        right = 0
        tagged = 0
        count = 0
        acc = 0
        for j in range(start, stop+1):
            text_name0 = '/tn' + name[i] + '-' + str(j) + '_test0.txt'
            text_name1 = '/tn' + name[i] + '-' + str(j) + '_test1.txt'
            #print source_dir+text_name0
            #print source_dir+text_name1
            if not os.path.isfile(source_dir+text_name0):
                continue
            #print text_name
            for line in open(source_dir+text_name0):
                if re.search(r' actions', line):
                    temp_total = re.findall(r'\d+', line)
                    if temp_total:
                        #print 'temp_total = %s\n'%temp_total[0]
                        total += int(temp_total[0])
                elif re.search(r' right_actions', line):
                    temp_right = re.findall(r'\d+', line)
                    if temp_right:
                        #print 'temp_right = %s\n'%temp_right[0]
                        right += int(temp_right[0])
                elif re.search(r' tag_actions', line):
                    temp_tagged = re.findall(r'\d+', line)
                    if temp_tagged:
                        #print 'temp_tagged = %s\n'%temp_tagged[0]
                        tagged += int(temp_tagged[0])
                elif re.search(r' accuracy', line):
                    temp_acc = re.findall(r'\d+', line)
                    if temp_acc:
                        #print 'temp_acc',temp_acc
                        acc += int(temp_acc[-1])
                        count += 1

            if not os.path.isfile(source_dir+text_name1):
                continue
            for line in open(source_dir+text_name1):
                if re.search(r' actions', line):
                    temp_total = re.findall(r'\d+', line)
                    if temp_total:
                        #print 'temp_total = %s\n'%temp_total[0]
                        total += int(temp_total[0])
                elif re.search(r' right_actions', line):
                    temp_right = re.findall(r'\d+', line)
                    if temp_right:
                        #print 'temp_right = %s\n'%temp_right[0]
                        right += int(temp_right[0])
                elif re.search(r' tag_actions', line):
                    temp_tagged = re.findall(r'\d+', line)
                    if temp_tagged:
                        #print 'temp_tagged = %s\n'%temp_tagged[0]
                        tagged += int(temp_tagged[0])
                elif re.search(r' accuracy', line):
                    temp_acc = re.findall(r'\d+', line)
                    if temp_acc:
                        #print 'temp_acc',temp_acc
                        acc += int(temp_acc[-1])
                        count += 1
        
        to.append(total)
        ri.append(right)
        ta.append(tagged)
        ac.append(acc/float(count*10e5))
    assert len(to) == len(ri) == len(ta) == len(ac)
    print '\ntotal:',to
    print '\nright:',ri
    print '\ntagged:',ta
    print '\nacc:',ac

    f = []
    for k in range(len(to)):
        if ta[k] == 0:
            p = 0
        else:
            p = float(ri[k])/ta[k]
        if to[k] == 0:
            r = 0
        else:
            r = float(ri[k])/to[k]
        if p + r == 0:
            f.append(0)
        else:
            f.append(2*r*p/(p+r))
    for i in range(len(to)):
        print 'texts_num = %s\tf-measure = %f\taccuracy = %f'%(name[i], f[i], ac[i]) 
        #print '%f\t%f'%(f[i], ac[i])


def compute_basic_results(table, start, stop):
    source_dir = './results/basic_results/' + table
    total = 0
    right = 0
    tagged = 0
    acc = 0
    count = 0
    print 'table = %s\n'%table
    for j in range(start, stop+1):
        text_name = '/ert0_ep_test' + str(j) + '.txt'
        #print source_dir+text_name
        if not os.path.isfile(source_dir+text_name):
            continue
        #print text_name
        for line in open(source_dir+text_name):
            if re.search(r' acions', line):
                temp_total = re.findall(r'\d+', line)
                if temp_total:
                    #print 'temp_total = %s\n'%temp_total[0]
                    total += int(temp_total[0])
            elif re.search(r' right_acions', line):
                temp_right = re.findall(r'\d+', line)
                if temp_right:
                    #print 'temp_right = %s\n'%temp_right[0]
                    right += int(temp_right[0])
            elif re.search(r' tag_acions', line):
                temp_tagged = re.findall(r'\d+', line)
                if temp_tagged:
                    #print 'temp_tagged = %s\n'%temp_tagged[0]
                    tagged += int(temp_tagged[0])
            elif re.search(r' accuracy', line):
                temp_acc = re.findall(r'\d+', line)
                print 'temp_acc',temp_acc
                if temp_acc:
                    acc += int(temp_acc[-1])
                    count += 1


    if tagged == 0:
        p = 0
    else:
        p = float(right)/tagged
    if total == 0:
        r = 0
    else:
        r = float(right)/total
    if p + r == 0:
        f = 0
    else:
        f = 2*r*p/(p+r)
    if count == 0:
        tacc = 0
    else:
        tacc = acc/float(count)
    print 'f-measure = %f\taccuracy = %d'%(f, tacc) 
        #print f[i]


def top_ten_actions_pct(table, num, db, cur):
    get_data = 'select * from '+table+' where text_num < '+str(num)
    cur.execute(get_data)
    result = cur.fetchall()

    vocab = {}
    actions = 0
    words_num = 0
    from nltk.stem import WordNetLemmatizer
    wnl = WordNetLemmatizer()
    for i in range(len(result)):
        tags = re.split(r' ', result[i][3])
        words = re.split(r' ', result[i][2])
        words_num += len(words)
        if '1' not in tags:
            #print 'tags',tags
            continue
        for j,t in enumerate(tags):
            if t == '1':
                actions += 1
                #w = wnl.lemmatize(words[j], 'v')
                w = words[j]
                if w in vocab.keys():
                    vocab[w] += 1
                else:
                    vocab[w] = 1
                #print 'len(vocab) = %d'%len(vocab)
    sort_vocab = sorted(vocab.items(), key=lambda a:a[1], reverse=True)
    print '\n\ntable name:  %s'%table
    print 'words_num = %d,  actions = %d,  vocab = %d'%\
    (words_num, actions, len(vocab))
    count = 0
    for k in range(10):
        print sort_vocab[k]
        count += sort_vocab[k][1]
    print 'Top ten actions percentage:  %f'%(count/float(actions))


def action_rate(table, mtn, db, cur):
    words = 0
    actions = 0
    rate = 0
    matrix = 0

    for i in range(mtn):
        get_data = 'select * from ' +table+ ' where text_num = '+str(i)
        cur.execute(get_data)
        result = cur.fetchall()
        matrix += 1
        for r in result:
            sent = r[2]
            if re.search(r'-----LONG_TEXT_SEPARATOR-----', sent):
                #print sent
                matrix += 1
            tag = r[3].split()
            #print tag
            a = [int(t) for t in tag]
            actions += sum(a)
            words += len(a)
            #print words
    rate = actions/float(words)
    print 'matrix = %d,  words = %d,  actions = %d,  rate = %f'%\
    (matrix, words, actions, rate)
    #words = 41944,  actions = 3999,  rate = 0.095341


def counter(actionDBs, num, db, cur):
    for i,ad in enumerate(actionDBs):
        long_words = 0
        short_words = 0
        count = 0
        actions = 0
        words = 0
        lt = 0
        st = 0
        for j in range(num[i]):
            get_data = 'select * from ' + ad + ' where text_num = ' + str(j)
            #print get_data
            cur.execute(get_data)
            result = cur.fetchall()

            ws = 0
            count += 1
            lt_flag = False
            for r in result:
                ws += len(re.split(r' ', r[2]))
                tags = [int(t) for t in re.split(r' ', r[3])]
                actions += sum(tags)
                if r[2].find('-----LONG_TEXT_SEPARATOR-----') >= 0:
                    count += 1
                    if not lt_flag:
                        lt_flag = True
            if lt_flag:
                long_words += ws
                lt += 1
            else:
                short_words += ws
                st += 1
            words += ws

        print '\n\ntable: %s\t total matrices: %d'%(ad, count)
        print 'total actions: %d\t action rate: %f'%(actions, actions/float(words))
        print 'long_texts = %d\t\t short_texts = %d'%(lt, st)
        print 'long_words = %d\t\t short_words = %d'%(long_words, short_words)
        if lt > 0 and st > 0:
            print 'long_avg = %f\t short_avg = %f'%\
            (long_words/float(lt), short_words/float(st)) 
        elif lt == 0 and st > 0:
            print 'long_avg = %f\t short_avg = %f'%(0, short_words/float(st))
        elif lt > 0 and st == 0:
            print 'long_avg = %f\t short_avg = %f'%(long_words/float(lt), 0)


def run_EADQN_main():
    actionDBs = ['tag_actions','tag_actions1','tag_actions2',
    'tag_actions3','tag_actions4','tag_actions5']
    num = [64, 52, 33, 54, 111, 35]
    start = time.time()
    for i,ad in enumerate(actionDBs):
        EADQN_main(ad, num[i])
    end = time.time()
    print 'Total time cost: %ds'%(end - start)


def run_multi_meanings():
    start = time.time()
    actionDBs = ['tag_actions5']
    num = [35]
    wdir = 'weights/tb5/ert0_ep_2.prm'
    name = './results/tag_actions5.txt'
    db = mc.connect(user='fengwf', password='123', database='test')
    cur = db.cursor()
    multi_meanings(actionDBs, num, wdir, name, db, cur)
    end = time.time()
    print 'Total time cost: %ds'%(end - start)


def run_compute_tl():
    tables = ['tb0', 'tb1', 'tb2', 'tb3', 'tb4']
    #compute_tag_length_results('tb1')
    for t in tables:
        #compute_tag_length_results(t, 1, 6)
        compute_basic_results(t, 0, 3)
        print '\n\n'


def run_action_rate():
    actionDBs = ['tag_actions','tag_actions1','tag_actions2',
    'tag_actions3','tag_actions4','tag_actions5', 'tag_actions6']
    num = [64, 52, 33, 54, 111, 35, 43]
    db = mc.connect(user='fengwf', password='123', database='test')
    cur = db.cursor()
    for i,ad in enumerate(actionDBs):
        #action_rate(a, num[i], db, cur)
        top_ten_actions_pct(ad, num[i], db, cur)


def run_counter():
    db = mc.connect(user='fengwf', password='123', database='test')
    cur = db.cursor()
    actionDBs = ['tag_actions','tag_actions1','tag_actions2',
    'tag_actions3','tag_actions4','tag_actions5', 'tag_actions6']
    num = [64, 52, 33, 54, 111, 35, 43]
    counter(actionDBs, num, db, cur)


def ten_fold_split():
    import numpy as np
    mtn = [64, 52, 38, 54, 111, 35, 43]
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
    f = open('10-fold-data-indices.txt', 'w+')
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
    for k,o in enumerate(out):
        f.write('Total texts of tb%d: %d\n'%(k, mtn[k]))
        for u in o:
            f.write(str(u)+'\n')
            print u
        print '\n'
        f.write('\n')
    f.close()






if __name__ == '__main__':
    #run_action_rate()
    #run_multi_meanings()
    #run_counter()
    #run_action_rate()
    #run_compute_tl()
    compute_texts_results('tb4', 0, 2)
    #ten_fold_split()