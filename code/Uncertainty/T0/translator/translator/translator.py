#! /usr/bin/env python2.5
# -*- coding: latin-1 -*-

#  Copyright (C) 2006-2008 Universitat Pompeu Fabra
#  Copyright (C) 2009 Universidad Simon Bolivar
#
#  Permission is hereby granted to distribute this software for
#  non-commercial research purposes, provided that this copyright
#  notice is included with any such distribution.
#
#  THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND,
#  EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE
#  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
#  PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE
#  SOFTWARE IS WITH YOU.  SHOULD THE PROGRAM PROVE DEFECTIVE, YOU
#  ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR CORRECTION.
#
# Elaborated by Hector Palacios, hlp@ldc.usb.ve, hectorpal@gmail.com
# Joint work with Hector Geffner.

# call it with the following bash enviroment:
# TRANSLATOR_HOME=<directory>
# ulimit -S -s 1024000 (very high stack size)
# ulimit -S -v MEMORYLIMIT_in_kb

import sys, os, signal, sets, errno
import re, datetime, subprocess, time, resource
import math
import shutil
import timeout, lug, shutil
import nondet2conf
import pddlsfromtar

init_wall_clock = time.time()

def usage():
    print """
TRANSLATOR: a family of translation-based planners for Conformant problems.
Includes t0, based on classical planning.

usage: %s {option}* (<domain.pddl> <problem.pddl> | <bothFiles.{tar|tgz|tbz}>)


general options:
     -l     log prefix. Log files: <prefix>_<problem>.{log,stat,ipc5,time}.
            Prefix default = translator_<strategy>
     -f     force to overwrite previous log file, defaut = no
     -t     global time limit. Overide strategies
     -c     check plan, default = no
     -d     leave generated files for debug, default = no
     -v l   verbosity. default = %d, values = 0, 10, 20, 30
     -s <strategy>

if a tar-file (ending in .tar, .tgz or .tbz) is used,
it should contain only two files: the domain and the problem.

<strategy> is an expression as follows.

strategy ::= abbrevation:time{; strategy} | abbrevation:time
abbrevation ::= t0 | only-t0 | k1 | only-k1 
             |  s0 | fs0 | k0 | t0c | old-t0
             |  sat | mc | satplan
time ::= <int=time in seconds> | inf

(protect strategies from the shell by '' or "")

Strategies are useful for combining different translations
approach. For example: sat + classical.
For reasons of efficiency, classical translations
are themself strategies, as it avoid repeated parsing.

The default strategy is '%s'
Example: 'only-t0:inf; sat:inf'

ABBREVATIONS:

====== Translations fo Classical Planning ======
Look for a conformant plan by transforming the problem into a classical one.
See Palacios & Geffner papers (ICAPS 2007 and JAIR 2009) for more details.
Most of the configurations try a series of classical problems
with FF classical planner (by default).
If an instance is unsolvable, next classical is tried.

==> t0
1st: If width = 1, K1 with uniq merge = satisfying clase,
     else K1 with merges = all clauses
2nd: Ktm with merge for each L, models of clauses relevant to L: C_I(L)
COMPLETE, sometimes polynomic.

==> only-t0
If width = 1, K1 with uniq merge = satisfying clase,
else K1 with merges = all clauses.
INCOMPLETE, polynomic.

==> k1
1st: K1 with merges = all clauses
2nd: Ktm with merge for each L, models of clauses relevant to L: C_I(L)
COMPLETE, sometimes polynomic.

==> only-t0
K1 with merges = all clauses.
INCOMPLETE, polynomic.

==> s0
Ktm with merge for each L, models of clauses relevant to L: C_I(L)
COMPLETE, sometimes polynomic.

==> s0
Ktm with merge for each L, models of Initial situation.
COMPLETE, always exponential.

==> k0
No merges.
INCOMPLETE, polynomic.

==> t0c
like t0, but verify "consistency" of conformant problem.

==> old-t0
Used in IPC5 and IPC6
1st: If width = 1, K1 with uniq merge = satisfying clase
2nd: Ktm with merge for each L, models of clauses relevant to L: C_I(L)
COMPLETE, sometimes polynomic.

==> kp (DISABLED)
AAAI-06 version. Not reimplemented yet.

For further options of translations to classical,
run ./cf2cs
To add a new option when calling cf2cs add '-trans -newoption'
while calling translator.



====== Logic-based Translations ======
Translate conformant problem into a CNF (a la SATPLAN)

==> sat
Look for an OPTIMAL conformant plan until a final horizon.
by transforming the problem to a SAT theory for each horizon
(As published in CAEPIA 2005, Palacios and Geffner, LNCS)

additional options for mc and sat:
     -i n   init horizon. default = %d
     -e n   final horizon. default = %d
     -z     look for serial plans instead of parallel. default = false.
     -enum-s0  print all the possible initial states and exit.
     -qbf   create a .qdimacs for the qbf corresponding to the starting 
            horizon and finish. Use in combination with -i

==> mc
Look for an OPTIMAL conformant plan until a final horizon.
by transforming the problem to d-DNNF and doing MC on each node.
(As published in ICAPS 2005 Palacios, Bonet, Darwiche and Geffner)

addtional options for mc:
     -nsim  no simple prunning
     -nstr  no strong prunning
     -lik   most likely  instead of  select var/value by criticality (Huang ICAPS 2006)

Algorithm "mc" also works with probabilistic version.
On this case, for the init horizon (-i)
a plan with Maximal probability of success is reported.

==> satplan
A naive SATPLAN for classical planning.
addtional options:
     -nsol N    return N solutions to the problem. Useful for learning.

--------------------------------------------

In some cases is better to increase the stack size:
     ulimit -S -s 1024000
and to limit the memory limit.
     ulimit -S -v MEMORYLIMIT_IN_KB

All tools are find in $%s directory
This planner uses 'ff' as classical planner, 'siege_v4' and 'zChaff' as SAT solvers,
'relsat' as a model enumerator, 'verify' for verifying conformant plans,
'validate' for validating classical plans, 'c2d_220' for compiling into d-DNNF
or simplifying CNF theories.
Rights are registered by their respective owners.

Contact Hector Palacios for further information,
or visit http://www.ldc.usb.ve/~hlp
""" % (sys.argv[0], verbosity, sstrategy, init_horiz, end_horiz, Loc_v)
    sys.exit(1)


def get_user_time(str):
    """ For getting seconds from user field of time().
For example: 'user   1m3.183s'
"""
    res = str
    for it2 in str.split(' '):
        if 'user' in it2:
            for it in it2.split():
                if 'm' in it:
                    [m,s] = it.split('m')
                    s = s.split('s')[0]
                else:
                    m = '0'
                    s = it.split('u')[0]
    try:
        res = float(m)*60+float(s)
    except:
        pass
    return res

def weak_unlink(f):
    try:
        os.unlink(f)
    except:
        return

def gen_log(msg, lst = [], time = True, date = False):
    msg2=msg + ' '
    if(time and not date ):
        msg2 += 'at ' + datetime.datetime.now().strftime('%H:%M:%S')
    elif( date ):
        msg2 += 'at ' + datetime.datetime.now().ctime()
    line = ''
    if(lst != []):
        line = '\ncommand: '
        for i in lst:
            line += i + ' '
    return msg2+line

def pr(msg):
    print msg
    log_f.write(msg+'\n')
    log_f.flush()

def log(msg):
    log_f.write(msg+'\n')
    log_f.flush()

# Extrating from result of wait
def the_signal(result):
    return result & 127

def was_core(result):
    return (result & 128)==1

def exit_status(result):
    return result >> 8

tokill=[]
def clean_children():
    for pid in tokill:
        try:
            os.kill(pid,15)
        except OSError, e:
            if e.errno == errno.ESRCH:
                continue
    time.sleep(0.5)
    while 1:
        try:
            pid,res = os.waitpid(0, os.WNOHANG)
            if(pid==0):
                break
            else:
                print 'additional process found %d' % pid
                os.kill(pid,15)
        except OSError, e:
            if e.errno == errno.ESRCH:
                continue
            elif e.errno == errno.ECHILD:
                break
            else:
                raise
    #print 'Collecting zombies...'
    time.sleep(0.5)
    while 1:
        try:
            pid,result = os.wait()
            signal = the_signal(result)
            core = was_core(result)
            status=exit_status(result)
            print '\tfinished pid =', pid, 'signal =', signal, 'core =', core, 'status =', status
        except OSError, e:
            if e.errno == errno.ESRCH:
                continue
            elif e.errno == errno.ECHILD:
                break
            else:
                raise

def killall(signum, frame):
    global cleaning_functions
    if(signum==14):
        print 'Timeout... trying to kill pending children'
    else:
        print 'Interrupted... trying to kill pending children'
        
    print 'collecting zombies... done'
    clean_children()
    print 'cleaning files... done'
    for x in cleaning_functions:
        f = x[0]
        arg = x[1]
        f(arg)
    finish()
    stat_f.write('TIMEOUT\n')
    sys.exit(1)

def calc_num_s0s(init_nf):
    cmd=[Loc+'/relsat','-#c',init_nf] 
    log(gen_log('--------- Calling',cmd))
    relsat=subprocess.Popen(cmd,bufsize=1000, stdout=subprocess.PIPE)
    tokill.append(relsat.pid)
    num_s0 = -1
    for l in relsat.stdout.readlines():
        if(l.startswith('Number of solutions')):
            num_s0 = int(l.split(' ')[3])

    if(num_s0 <= 0):
        pr('Error on response of relsat -#c')
        sys.exit(1)
    res = relsat.wait()
    tokill.pop()
    if(res < 0):
        pr('Error calling relsat -#c: %d ' % res)
        sys.exit(1)
    return num_s0

atom2time = {}
def calc_atom2fluent(atoms_nf):
    atom2fluent = {}
    atoms_f=open(atoms_nf,'r')
    for line in atoms_f.readlines():
        if(line.find(':')> 0):
            pair=line.split(':')
            fluent=pair[1][0:-1].split('*')[0]
            v_atom=pair[0].split(' ')[0]
            t=pair[0].split(' ')[2]
            atom=v_atom[1:len(v_atom)]
            atom2fluent[atom] = fluent
            atom2time[atom] = t
    atoms_f.close()
    return atom2fluent


def is_really_consistent(n_atoms_init, num_s0s, actions_nf, nnf_nf):
    cmd=[Loc+'/plannf','-f',str(n_atoms_init),str(num_s0s),actions_nf,nnf_nf]
    log(gen_log('--------- Calling', cmd))

    # When the nnf is just false or true, plannf violate an assertion
    # the problem is not consistent anyway, so we redirect errors to /dev/null
    devnull=open(os.devnull,'w')
    plannf=subprocess.Popen(cmd, bufsize=-1, stdout=subprocess.PIPE, stderr=devnull )
    tokill.append(plannf.pid)
    answ = False
    for l in plannf.stdout.readlines():
        if(l.startswith('RESULT')):
            answ = l.split(':')[1].strip()
    res = plannf.wait()
    tokill.pop()
    if(res < 0):
        pr('Error calling plannf: %d ' % res)
        sys.exit(1)
    res = ""
    devnull.close()
    if(answ=='yes'):
        return True
    else:
        return False

    
def is_consistent(n_atoms_init,init_nf,cnf_nf):
    cnfs0_nf=init_nf+'.s0'
    erase.add(cnfs0_nf)

    cmd=[Loc+'/relsat',init_nf] # If we add -#a, check every solution
    relsat=subprocess.Popen(cmd,bufsize=-1, stdout=subprocess.PIPE)
    tokill.append(relsat.pid)

    sat=True
    for l in relsat.stdout.readlines():
        if(l.startswith('Solution')):
            # From HERE
            lines = ''
            nlines = 0
            models = set((l.split(':')[1].strip()).split(' '))
            for a in range(1,n_atoms_init+1):
                if(str(a) in models):
                    lines += ' '+str(a)+' 0\n'
                else:
                    lines += ' -'+str(a)+' 0\n'
                nlines += 1
            # to HERE, better to use relsat2model

            cnf_f = open(cnf_nf,'r')
            line = cnf_f.readline().split(' ')

            cnfs0_f=open(cnfs0_nf,'w')
            cnfs0_f.write("p cnf %s %d\n" % (line[2], nlines+int(line[3])) )
            cnfs0_f.write(lines)
            for line in cnf_f.readlines():
                cnfs0_f.write(line)

            cnf_f.close()
            cnfs0_f.close()

            cmd2=[Loc+'/relsat',cnfs0_nf]
            relsat2=subprocess.Popen(cmd2,bufsize=-1, stdout=subprocess.PIPE)
            tokill.append(relsat2.pid)
            for l in relsat2.stdout.readlines():
                if(l.find('UNSAT') >= 0):
                    sat = False
                    break
            res = relsat2.wait()
            tokill.pop()
            if(res < 0):
                pr('Error calling relsat (2): %d' % res)
                sys.exit(1)
            if(not sat): break
    if(not sat):
        try:
            os.kill(relsat.pid,15)
        except OSError, e:
            if e.errno == errno.ESRCH:
                pass
            else:
                raise
            
    relsat.wait() < 0
    tokill.pop()
    return sat

def global_time():
    return resource.getrusage(resource.RUSAGE_SELF)[0]+\
           resource.getrusage(resource.RUSAGE_CHILDREN)[0]

           

def lost_wall_clock():
    return time.time() - init_wall_clock -\
           global_time()

final_time = -1
global_total_time = 0
def finish():
    if(statline != ''):
        stat_f.write(statline+'\n')
    if( final_time == -1 ):
        rt = global_time()
    else:
        rt = final_time    
    if global_total_time == 0:
        t = str(rt)
    else:
        t = str(global_total_time)
    line = 'TOTAL_TIME:' + t
    pr(line)
    stat_f.write(line+'\n')
    time_f = open(time_nf,'w')
    time_f.write(t)
    cleanfiles()

def cleanfiles():
    for f in erase:
        weak_unlink(f)
    if(not do_debug):
        for f in erasedebug:
            weak_unlink(f)

def save_plan(plan,extra=''):
    ipc5_f=open(ipc5_nf+extra,'w')
    ipc5_f.write('0\n')
    ipc5_f.write('%%\n')
    ipc5_f.write(str(len(plan)))
    for act in plan:
        ipc5_f.write(' '+act)
    ipc5_f.write('\n')
    ipc5_f.write('%%\n')
    ipc5_f.write('linear ')
    ipc5_f.write(str(len(plan)))
    for i in range(0,len(plan)):
        ipc5_f.write(' '+str(i))
    ipc5_f.write('\n')
    ipc5_f.close()

def save_flat_plan(plan,nf):
    f=open(nf,'w')
    for act in plan:
        f.write(act+'\n')
    f.close()

go_on=False

def going_on(signum, frame):
    global go_on
    go_on=True
    print 'Continuing.... because got signal',signum

signal.signal(signal.SIGUSR1, going_on)

go_on_others = False
def donothing(signum, frame):
    #print 'Child finish',frame.f_code
    global go_on_others
    go_on_others = True
    pass

# Current distribution de of Lama requieres PDDL to be in
# the same directory where their executables are.
# Set in an enviroment var LAMA
def run_classical_lama(Loc,ndomain_nf,nproblem_nf,is_nondet,to_run_classical):
    copy_to_path = False
    try:
        lama_dir=os.environ['LAMA']
    except KeyError, e:
        pr('LAMA var enviroment not set. Where is it?')
        to_run_classical = False
        return

    if copy_to_path:
        shutil.copy(ndomain_nf,lama_dir)
        shutil.copy(nproblem_nf,lama_dir)
    cur_dir = os.environ['PWD']
    if copy_to_path:
        os.chdir(lama_dir)
    try:
        os.remove('res.1')
    except OSError:
        pass
    if copy_to_path:
        cmd=['./plan',ndomain_nf,nproblem_nf,'res']
    else:
        cmd=[lama_dir+'/plan',ndomain_nf,nproblem_nf,'res']

    pr(gen_log('Solving classical problem'))
    log(gen_log('--------- Calling',cmd))
    log_f.flush()
    log_nf_tmp2=log_nf+'-lama.tmp'
    erasedebug.add(log_nf_tmp2)
    erasedebug.add('res.1')
    erase.add('output')
    erase.add('output.sas')
    erase.add('test.groups')
    log_f_tmp2=open(log_nf_tmp2,'w')
    init_t = resource.getrusage(resource.RUSAGE_CHILDREN)[0]
    go_on_others = False
    classical=subprocess.Popen(cmd, bufsize=-1, stdout=log_f_tmp2, stderr=log_f_tmp2)
    tokill.append(classical.pid)
    res = classical.wait()
    tokill.pop()
    elapsed = resource.getrusage(resource.RUSAGE_CHILDREN)[0] - init_t
    if( res < 0):
        pr('Error calling classical: %d' % res)
        to_run_classical = False

    log_f_tmp2.close()
    log_f_tmp2 = open(log_nf_tmp2,'r')
    for l in log_f_tmp2.readlines():
        log_f.write(l)
        if 'std::bad_alloc' in l or 'MemoryError' in l:
            to_run_classical = False
            pr('solving classical problem: NO MEMORY')
    log_f_tmp2.close()

    full_plan=[]
    for l in file('res.1'):
        full_plan.append(l.strip().replace(' )',')'))

    if copy_to_path:
        shutil.move(log_nf_tmp2,cur_dir)
        os.chdir(cur_dir)
    return [full_plan,to_run_classical,elapsed]

def run_classical_ff(Loc,ndomain_nf,nproblem_nf,is_nondet,to_run_classical):
    path=os.environ['PWD']+'/'
    cmd=[Loc+'/ff','-p',path,'-o',ndomain_nf,'-f',nproblem_nf]
    if False and is_nondet:
        cmd.extend(['-h','1'])

    pr(gen_log('Solving classical problem'))
    log(gen_log('--------- Calling',cmd))
    log_f.flush()
    log_nf_tmp2=log_nf+'-ff.tmp'
    erasedebug.add(log_nf_tmp2)
    log_f_tmp2=open(log_nf_tmp2,'w')
    init_t = resource.getrusage(resource.RUSAGE_CHILDREN)[0]
    go_on_others = False
    classical=subprocess.Popen(cmd, bufsize=-1, stdout=log_f_tmp2, stderr=log_f_tmp2)
    tokill.append(classical.pid)
    res = classical.wait()
    tokill.pop()
    elapsed = resource.getrusage(resource.RUSAGE_CHILDREN)[0] - init_t
    if( res < 0):
        pr('Error calling classical: %d' % res)
        to_run_classical = False

    log_f_tmp2.close()
    exp = re.compile('[0-9]:',re.VERBOSE)
    log_f_tmp2 = open(log_nf_tmp2,'r')
    full_plan=[]
    for l in log_f_tmp2.readlines():
        log_f.write(l)
        if exp.search(l):
            act = '(' + l.split(':')[1].split()[0].strip() + ')'
            full_plan.append(act)
        if l.startswith('NO MEMORY'):
            to_run_classical = False
            pr('solving classical problem: NO MEMORY')

    log_f_tmp2.close()
    return [full_plan,to_run_classical,elapsed]

def run_classical(Loc,ndomain_nf,nproblem_nf,is_nondet,to_run_classical):
    global classical_planner
    """
    Should return [plan,full_plan,to_run_classical,elapsed]
    where:
    - plan: list of conformant actions, original arguments with obj/constants
    - full_plan: list of classical actions (without merge, etc)
    - to_run_classical: true if another run or another planner should be run
    """
    if classical_planner.lower() == 'ff':
        [full_plan,to_run_classical,elapsed] = run_classical_ff(Loc,ndomain_nf,nproblem_nf,is_nondet,to_run_classical)
    elif classical_planner.lower() == 'lama':
        [full_plan,to_run_classical,elapsed] = run_classical_lama(Loc,ndomain_nf,nproblem_nf,is_nondet,to_run_classical)
    else:
        pr('ERROR: classical planner neither FF or LAMA: %s' % classical_planner)
    plan=[]
    for a in full_plan:
        au = a.upper()
        if au.find('MERGE') < 0  and \
                au.find('MAKE_END_DISJ_GOAL') < 0 and\
                au.find('REACH-GOAL') < 0 and \
                '----RESET' not in au:
            act = a.replace('_',' ')
            if is_nondet:
                act_splitted = re.split('COPY----[0-9]+-',act)
                if len(act_splitted) <> 1:
                    act = act_splitted[1]
            plan.append(act)
    return [plan,full_plan,to_run_classical,elapsed]




def cf2cs(exec_f,transf_type):
    global statline, plan_found, erasedebug, erase, extra_options, go_on, global_total_time

    pr('Starting cf2cs (%s): translating from conformant planning to classical planning' %
       (transf_type+'. extra: '+str(extra_options)))

    # Transform non-deterministic probs into deterministics
    prefix_det='conf'
    is_nondet = nondet2conf.nondet2conf(domain_nf, problem_nf, prefix_det)
    tdomain_nf = prefix_det+'-d.pddl'
    tproblem_nf = prefix_det+'-p.pddl'
    erasedebug.add(tdomain_nf)
    erasedebug.add(tproblem_nf)
    if not is_nondet:
        weak_unlink(tdomain_nf)    
        weak_unlink(tproblem_nf)
        tdomain_nf = domain_nf
        tproblem_nf = problem_nf

    calc_models_cnf_f = '.init.cnf'
    calc_models_sols_f = calc_models_cnf_f + '.sols'
    erasedebug.add(calc_models_cnf_f)
    erasedebug.add(calc_models_sols_f)
    erasedebug.add('.c_i.cls')
    erasedebug.add('.c_i.cls.pi')
    erasedebug.add('.clauses.cnf')
    erasedebug.add('.clauses.cnf.nnf')
    erasedebug.add('output-c2d.log')
    erasedebug.add('output-models.log')

    # From conformant into classical
    cmd=[Loc+'/'+exec_f]
    if transf_type != '':
        for i in transf_type.split(' '):
            cmd.extend([i])
    cmd.extend(extra_options)
    prefix='new'
    nproblem_nf = prefix+'-p.pddl'
    ndomain_nf = prefix+'-d.pddl'
    erasedebug.add(nproblem_nf)
    erasedebug.add(ndomain_nf)

    weak_unlink(nproblem_nf)
    weak_unlink(ndomain_nf)    

    cmd.append('-sp') # cf2cs let know this process that is has finished on generating PDDLs
    cmd.extend(['-s',prefix,tdomain_nf,tproblem_nf])
    pr(gen_log('Generating PDDL for classical problem'))
    log(gen_log('--------- Calling',cmd))
    log_f.flush()
    log_nf_tmp=log_nf+'-cf2cs.tmp'
    erasedebug.add(log_nf_tmp)
    log_f_tmp=open(log_nf_tmp,'w')
    init_t = resource.getrusage(resource.RUSAGE_CHILDREN)[0]
    cf2cs=subprocess.Popen(cmd, bufsize=-1, stdout=log_f_tmp, stderr=log_f_tmp)
    tokill.append(cf2cs.pid)
    # Iterar hasta que
    # Cuando encuentra plan, mandar signal a cf2cs.pid para que termine clean
    # cuando no se encuentra, mandar otro signal para que vuelva a generar pddl
    to_run_classical = True
    print "I'm",os.getpid(),'and cf2cs is',cf2cs.pid
    while to_run_classical:
        # Are the files there?
        signal.signal(signal.SIGCHLD, donothing)
        [gotpid,code] = os.waitpid(0,os.WNOHANG)
        if gotpid == 0:
            if not go_on:
                print 'Pausing (waiting for new PDDL).'
            if not go_on:
                signal.pause()
        else:
            print 'Seems that process',gotpid,'already finished. Code:',code
        signal.signal(signal.SIGCHLD, signal.SIG_DFL)
        #print 'Awake. Looking for files and running planner'
        if not os.access(os.path.abspath(ndomain_nf),os.F_OK) or\
           not os.access(os.path.abspath(nproblem_nf),os.F_OK):
            pr('generation of new pddls, FAILED')
            return

        # Run classical planner
        [plan,full_plan,to_run_classical,elapsed] = \
            run_classical(Loc,ndomain_nf,nproblem_nf,is_nondet,to_run_classical)
        if(plan == []):
            pr('solving classical problem: FAILED')
            stat_f.write('classical_planner_failed\n')

            [gotpid,code] = os.waitpid(0,os.WNOHANG)
            if gotpid == 0:
                # Trying with others pddls
                weak_unlink(nproblem_nf)
                weak_unlink(ndomain_nf)    
                if to_run_classical:
                    go_on=False
                    pr('Generating a new PDDL')
                    os.kill(cf2cs.pid,signal.SIGUSR1)
                else:
                    try:
                        os.kill(cf2cs.pid,signal.SIGUSR2)
                    except OSError:
                        pass
            else:
                pr('cf2cs finished. No plan found')
                stat_f.write('cf2cs_finished\n')
                to_run_classical = False
            continue

        pr(gen_log('solution FOUND'))
        global_total_time += elapsed
        stat_f.write('solving_time:'+str(elapsed)+'\n')

        try:
            os.kill(cf2cs.pid,signal.SIGUSR2)
        except OSError:
            pass

        plan_found=True
        to_run_classical = False
        pr('Plan: ')
        for act in plan:
            pr(act)

        msg = 'PLAN_LENGTH:' + str(len(plan)) +'\n'
        pr(msg)
        stat_f.write(msg)
        pr('')
        save_plan(plan)

        if check_plan :
            try:
                # Check also classical plan
                flat_plan_nf = prefix+'.plan'
                erasedebug.add(flat_plan_nf)
                save_flat_plan(full_plan,flat_plan_nf)

                cmd=[Loc+'/validate','-v',ndomain_nf,nproblem_nf,flat_plan_nf]
                log(gen_log('--------- Calling',cmd))
                validate=subprocess.Popen(cmd, bufsize=-1, stdout=subprocess.PIPE, stderr=log_f )
                tokill.append(validate.pid)
                isvalid = False
                unknown_warning = False
                output = ''
                for l in validate.stdout.readlines():
                    output += l
                    if l.startswith('Plan valid'):
                        isvalid = True
                    elif 'WARNING' in l:
                        if 'adds the literal' not in l and 'deletes the literal' not in l:
                            unknown_warning = True
                if unknown_warning:
                    isvalid = False
                if not isvalid:
                    pr(output)
                else:
                    pr('Valid classical plan')
                statline += '\nCLASSICAL-OK:'
                if(isvalid):
                    statline += 'YES'
                else:
                    statline += 'NO'
                res = validate.wait()
                tokill.pop()
                if(res < 0):
                    pr('Error calling validate: %d' % res)        
            except OSError, e:
                pr('failing on call verifier...')

    tokill.pop()
    elapsed = resource.getrusage(resource.RUSAGE_CHILDREN)[0] - init_t
    try:
        res = cf2cs.wait()
    except OSError:
        res = cf2cs.returncode
        if res == None: res = 0
    if( res < 0 ):
        pr('Error calling cf2cs: %d' % int(res) )
        sys.exit(1)
    

    if True or not plan_found:
        log_f_tmp.close()
        pi_time=0
        for i in open(log_nf_tmp,'r'):
            log_f.write(i)
            if(i.find('FATAL ERROR') >= 0):
                stat_f.write('translation_failure')
                plan_found = False
                sys.exit(1)
            if(i.startswith('STAT')):
                s,var,value = i.split(' ')
                stat_f.write(var+' '+value)
            if(' Time elapsed in Prime implicates' in i):
                pi_time=get_user_time(i.split(':')[1].strip())
            if(i.startswith('REGISTER: main()')):
                elapsed = float(i.split()[3])
                elapsed = elapsed+pi_time
                global_total_time += elapsed
                stat_f.write('translation_time:'+str(elapsed)+'\n')                    


def relsat2model(l,n_atoms_init):
    """
    l is a relsat all solutions line (with -#a option). For example:
    Solution 5: 45 123 2 1 12
    """
    models = set((l.split(':')[1].strip()).split())
    model=[]
    for a in range(1,n_atoms_init+1):
        if(str(a) in models):
            model.append(int(a))
        else:
            model.append(-int(a))
    #print 'MODEL',n_atoms_init,model
    return model

def make_bits(num,num_bits,base):
    """
    Return the binary encoding of num
    using num_bits bits.
    Each bit is representing using a numeric variable (cnf sense)
    starting from base+1
    """
    l=[]
    for b in range(0,num_bits):
        #print 2**b, base, base+b+1
        if num & (2**b) == 0:
            l.append(str(-(base+b+1)))
        else:
            l.append(str(base+b+1))
    l.reverse()
    return l

# Print all the possible initial states
def do_enum_s0(n_atoms_init,atoms_nf,init_nf,printit=True):
    atom2fluent = calc_atom2fluent(atoms_nf)
    cmd=[Loc+'/relsat','-#a',init_nf] 
    relsat=subprocess.Popen(cmd,bufsize=1000, stdout=subprocess.PIPE)
    tokill.append(relsat.pid)
    if printit:
        print 'Models:'
    else:
        result=[]
    for l in relsat.stdout.readlines():
        if(l.startswith('Solution ')):
            if printit:
                print 'Solution: ',
                for i in l.split(': ')[1].split():
                    atom = atom2fluent[i]
                    print str(atom),
                print ''
            else:
                result.append(relsat2model(l,n_atoms_init))

    res = relsat.wait()
    tokill.pop()
    if(res < 0):
        pr('Error calling relsat -#c: %d ' % res)
        sys.exit(1)
    if not printit:
        return result

def try_relsat(cnf2_nf, actions_nf, num=1):
    global statline, erase, erasedebug
    
    #raise Exception('relsat', 'file')
    if(not os.access(Loc+'/relsat',os.F_OK)):
        print "relsat executable doesn't exist"
        raise Exception('relsat', 'file')

    if num>1:
        cmd4=[Loc+'/relsat','-#a',cnf2_nf]
    else:
        cmd4=[Loc+'/relsat',cnf2_nf]

    actions_f=open(actions_nf,'r')
    actions=set(actions_f.read().split(' ')[1:-1])
    actions_f.close()

    pr(gen_log('Calling SAT solver'))
    log(gen_log('--------- Calling', cmd4))

    init_t = resource.getrusage(resource.RUSAGE_CHILDREN)[0]
    relsat=subprocess.Popen(cmd4,bufsize=1000, stdout=subprocess.PIPE)
    tokill.append(relsat.pid)
    all_plan_atoms = []
    for l in relsat.stdout:
        plan_atoms = []
        if(l.startswith('UNSAT')):
            return False,[]
        elif(l.startswith('Solution ')):
            for i in l.split(': ')[1].split():
                if(i in actions):
                    plan_atoms.append(i)
            num -= 1
            all_plan_atoms.append(plan_atoms)
        if num == 0:
            break
    os.kill(relsat.pid,15)
    res = relsat.wait()
    tokill.pop()
    elapsed = resource.getrusage(resource.RUSAGE_CHILDREN)[0] - init_t
#     if(res < 0):
#         pr('Error calling relsat -#c: %d ' % res)
#         sys.exit(1)

    statline += str(elapsed)
    return True,all_plan_atoms

def try_siege(cnf2_nf, actions_nf):
    global statline, erase, erasedebug
    
    #raise Exception('siege', 'file')
    if(not os.access(Loc+'/siege_v4',os.F_OK)):
        print "siege_v4 executable doesn't exist"
        raise Exception('siege', 'file')

    cmd4=[Loc+'/siege_v4',cnf2_nf]
    pr(gen_log('Calling SAT solver'))
    log(gen_log('--------- Calling', cmd4))

    results_nf='siege.results'
    erasedebug.add(results_nf)
    weak_unlink(results_nf)
    init_t = resource.getrusage(resource.RUSAGE_CHILDREN)[0]
    siege=subprocess.Popen(cmd4, bufsize=-1, stdout=log_f, stderr=log_f )
    tokill.append(siege.pid)
    res = siege.wait() < 0
    tokill.pop()
    elapsed = resource.getrusage(resource.RUSAGE_CHILDREN)[0] - init_t
    if(res < 0):
        pr('Error calling sieve_v4: %d' % res)
        raise Exception('siege', 'process')

    statline += str(elapsed)

    if(not os.access(os.path.abspath(results_nf),os.F_OK)):
        print 'siege_v4 did not obtain results'
        raise Exception('siege', 'file')
    
    results_f=open(results_nf,'r')
    result=results_f.readline()

    actions_f=open(actions_nf,'r')
    actions=set(actions_f.read().split(' ')[1:-1])
    actions_f.close()

    sat=True
    plan_atoms=[]
    for w in result.split(' '):
        if(w.startswith('unsat')):
            sat=False
            break
        if(w in actions):
            plan_atoms.append(w)
    return sat, plan_atoms

def try_zchaff(cnf2_nf, actions_nf):
    global statline, erase, erasedebug
    
    cmd4=[Loc+'/zchaff',cnf2_nf]
    pr(gen_log('Calling SAT solver'))
    log(gen_log('--------- Calling', cmd4))

    results_nf='zchaff.out'
    erasedebug.add(results_nf)
    weak_unlink(results_nf)
    results_f=open(results_nf, 'w')
    init_t = resource.getrusage(resource.RUSAGE_CHILDREN)[0]
    zchaff=subprocess.Popen(cmd4, bufsize=-1, stdout=results_f, stderr=results_f )
    tokill.append(zchaff.pid)
    res = zchaff.wait() < 0
    tokill.pop()
    elapsed = resource.getrusage(resource.RUSAGE_CHILDREN)[0] - init_t
    if(res < 0):
        pr('Error calling zchaff: %d' % res)
        sys.exit(1)

    statline += str(elapsed)

    results_f=open(results_nf,'r')

    actions_f=open(actions_nf,'r')
    actions=set(actions_f.read().split(' ')[1:-1])
    actions_f.close()

    sat='bad'
    plan_atoms=[]

    for l in results_f.readlines():
        log(l[0:-1])
        line = l.split(' ')
        if line[0].isdigit():
            sat=True
            for w in line:
                if(w == 'Random'):
                    break
                if(w in actions):
                    plan_atoms.append(w)
        elif line[0].startswith('RESULT:'):
            res = line[0].split('\t')
            if(res[1].startswith('SAT')):
                sat = True
            elif (res[1].startswith('UNSAT')):
                sat = False
            else:
                pr('Error processing zchaff output')
                sys.exit(1)
    if sat == 'bad':
        pr('Error processing zchaff output, ac')
        sys.exit(1)
    return sat, plan_atoms

def cf2sat(nnf_nf, n_atoms_init, cnf2_nf, actions_nf ):
    global statline
    cmd3=[Loc+'/2sat',nnf_nf,str(n_atoms_init)]
    pr(gen_log('Generating new CNF'))
    log(gen_log('--------- Calling',cmd3))
    tosat=subprocess.Popen(cmd3, bufsize=-1, stdout=log_f, stderr=log_f )
    tokill.append(tosat.pid)
    res=tosat.wait();
    tokill.pop()
    if(res < 0):
        pr('Error calling 2sat %d: ' % res)
        sys.exit(1)
    cnf2_f = open(cnf2_nf,'r')
    
    line = cnf2_f.readline().split(' ')
    nvars = line[2]
    nclauses = line[3][0:-1]
    statline += nvars + ';' + nclauses + ';'
    cnf2_f.close()
    
    try:
        sat, plan_atoms = try_siege(cnf2_nf, actions_nf)
    except Exception, inst:
        if inst.args == ('siege', 'file'):
            pr('siege_v4 failed. Trying with zchaff')
            sat, plan_atoms = try_zchaff(cnf2_nf, actions_nf)
        else:
            raise
    return sat, plan_atoms

#' nvars2 nclauses2 sat-time\n'
#' nnodes nedgest backtracks nodes searchtime\n'

def read_output_plannf(results_nf):
    global statline, isprob
    found = 'bad'
    backtracks = 'bad'
    nodes = 'bad'
    time = 'bad'
    prob = -1
    plan_atoms = []
    
    results_f=open(results_nf,'r')
    reading_act = False
    for l in results_f.readlines():
        log(l[0:-1])
        line = l.split(':')
        if reading_act:
            if line[0] == 'action':
                plan_atoms.append(line[1].strip())
            else:
                reading_act = False
        else:
            if l.startswith('PLAN RESULT:'):
                if line[1].strip() == 'FOUND':
                    found = True
                elif line[1].strip() == 'NOT-FOUND':
                    found = False
            elif l.startswith('NODES GENERATED:'):
                nodes = line[1].strip()
            elif l.startswith('BACKTRACKS:'):
                backtracks = line[1].strip()
            elif l.startswith('TIME ON SEARCHING:'):
                time = line[1].strip()
            elif l.startswith('un Plan:'):
                reading_act = True
            elif l.startswith('Found plan with probability'):
                prob = float(l.split('=')[1].strip())
                
    statline += backtracks + ';' + nodes + ';' + time 

    return found, prob, plan_atoms

def cf2mc(nnf_nf, n_atoms_init, num_s0s, actions2time_nf, vars2prob_nf ):
    global statline, cleaning_functions, isprob

    nnf_f = open(nnf_nf,'r')
    line = nnf_f.readline().split()
    nnodes = line[1]
    nedges = line[2]
    statline += nnodes + ';' + nedges + ';'
    nnf_f.close()
    
    cmd3=[Loc+'/plannf']
    if not simple_prunning:
        cmd3.append('-nsim')
    if not strong_prunning:
        cmd3.append('-nstr')
    if most_likely_selection:
        cmd3.append('-lik')

    if isprob:
        cmd3.extend(['-p',vars2prob_nf,actions2time_nf,nnf_nf])
        pr(gen_log('Searching over probabilistic plan space using WMC over d-DNNF'))
    else:
        cmd3.extend(['-z',str(n_atoms_init),str(num_s0s),actions2time_nf,nnf_nf])
        pr(gen_log('Searching over plan space using MC over d-DNNF'))

    log(gen_log('--------- Calling',cmd3))

    results_nf='plannf.out'
    erasedebug.add(results_nf)
    weak_unlink(results_nf)
    results_f=open(results_nf, 'w')
    init_t = resource.getrusage(resource.RUSAGE_CHILDREN)[0]
    plannf=subprocess.Popen(cmd3, bufsize=-1, stdout=results_f, stderr=results_f )
    tokill.append(plannf.pid)
    cleaning_functions.append([read_output_plannf,results_nf])    
    res = plannf.wait()
    cleaning_functions.pop()
    tokill.pop()
    elapsed = resource.getrusage(resource.RUSAGE_CHILDREN)[0] - init_t
    if(res < 0):
        pr('Error calling plannf: %d' % res)
        sys.exit(1)
    results_f.close()
    
    found, prob, plan_atoms = read_output_plannf(results_nf)
    #statline += str(elapsed)
    if isprob:
        pr('Probability: %f' % prob)
        statline += '\nPROBABILITY:' + str(prob)
        return True, plan_atoms
    else:
        return found, plan_atoms

def is_prob( isprob_nf ):
    isprob_f = open(isprob_nf,'r')
    line = isprob_f.readline()
    isprob_f.close()
    return line.startswith('yes')

# OJO: Faltan auxvars
class Atoms:
    def atoms_add(self,listmap,time,var,value):
        while time >= len(listmap):
            listmap.append({})
        listmap[time][var] = value
        
    def atoms_add_action(self,time,var,lit):
        self.atoms_add(self.action2str,time,var,lit)
        self.atoms_add(self.str2action,time,lit,var)

    def atoms_add_fluent(self,time,var,lit):
        self.atoms_add(self.fluent2str,time,var,lit)
        self.atoms_add(self.str2fluent,time,lit,var)
    
    def atoms_add_effect(self,time,var,val):
        self.atoms_add(self.effect2stract,time,var,val)
        self.atoms_add(self.stract2effect,time,val,var)
    
    def __init__(self,atoms_nf,auxvars_nf):
        self.fluent2str = []
        self.str2fluent = []
        self.action2str = []
        self.str2action = []
        self.extravars = []
        self.effect2stract = []
        self.stract2effect = []
        self.conditions = set()
        self.last_time = -1
        
        atoms_f = open(atoms_nf,'r')
        for l in atoms_f:
            line = l.split(' ',2)
            var = int(line[0][1:])
            svalue = line[2].split('*')
            if len(svalue) > 1:
                has_star = True
            else:
                has_star = False
            value = svalue[0].strip()
            if value.find(':') > 0:
                var_time = value.split(':')
                time = int(var_time[0])
                self.last_time = max(time,self.last_time)
                lit = var_time[1].lower()
                if has_star:
                    self.atoms_add_action(time,var,lit)
                else:
                    self.atoms_add_fluent(time,var,lit)
            elif value.startswith('(extra-room') > 0:
                self.extravars.append(var)
        auxvars_f = open(auxvars_nf,'r')
        for l in auxvars_f:
            line = l.split(' ',4)
            time = int(line[0])
            auxvar = int(line[1])
            act = int(line[3])
            auxstr = frozenset(lug.condition2tuple(line[4].strip().lower()))
            self.atoms_add_effect(time,auxvar,(act,auxstr))
            for cond in auxstr:
                self.conditions.add(cond)
        #print 'TRANSLATOR:',self.effect2stract
        
def concatenate_cnf(cnf_nf, new_cnf,overwrite=True,recalculate_nvars=False):
    global erasedebug
    if overwrite:
        # rename file
        cnfold_nf = cnf_nf + '.old'
        erasedebug.add(cnfold_nf)
        shutil.move(cnf_nf,cnfold_nf)

        # load header of cnf_nf
        cnfold_f = open(cnfold_nf,'r')

        cnf_f = open(cnf_nf,'w')
    else:
        # newfile file
        cnfnew_nf = cnf_nf + '.new'
        erasedebug.add(cnfnew_nf)

        # load header of cnf_nf
        cnfold_f = open(cnf_nf,'r')

        cnf_f = open(cnfnew_nf,'w')

    line = cnfold_f.readline().split(' ')
    # calculate new number of clauses (same number of vars)
    nvars = int(line[2])
    nclauses = int(line[3][0:-1])
    new_nvars = nvars
    if recalculate_nvars: 
        for clause in new_cnf:
            for lit in clause:
                new_nvars = max(new_nvars,abs(lit))
        
    cnf_f.write("p cnf %d %d\n" % (new_nvars, nclauses+len(new_cnf)))
    # new cnf
    for clause in new_cnf:
        for lit in clause:
            cnf_f.write(str(lit)+' ')
        cnf_f.write('0 \n')

    # old cnf
    for line in cnfold_f.readlines():
        cnf_f.write(line)
    cnf_f.close()

def simplify_cnf(cnf_nf):
    global erasedebug
    # rename file
    cnfold_nf = cnf_nf + '.pre-simplify'
    cnfold_simple_nf = cnfold_nf + '_simplified'
    erasedebug.add(cnfold_nf)
    erasedebug.add(cnfold_simple_nf)
    shutil.move(cnf_nf,cnfold_nf)

    cmd2 = [Loc+'/c2d_220','-in',cnfold_nf,'-simplify']
    pr(gen_log('Simplifying CNF'))
    log(gen_log('--------- Calling',cmd2))
    init_t = resource.getrusage(resource.RUSAGE_CHILDREN)[0]
    c2d=subprocess.Popen(cmd2, bufsize=-1, stdout=log_f, stderr=log_f )
    tokill.append(c2d.pid)
    res = c2d.wait();
    tokill.pop()
    elapsed = resource.getrusage(resource.RUSAGE_CHILDREN)[0] - init_t
    if(res < 0):
        pr('Error calling c2d: %d' % res)
        sys.exit(1)

    shutil.move(cnfold_simple_nf,cnf_nf)
    

def make_qbf(k,n_atoms_init,problem_nf,cnf_nf,atoms_nf,init_nf,actions_nf):
    qbf_nf = problem_nf+'-'+str(k)+'.qdimacs'
    models=do_enum_s0(n_atoms_init,atoms_nf,init_nf,False)
    num_s0 = len(models)
    num_bits = int(math.log(num_s0-1)/math.log(2))+1

    # Opening cnf
    cnf_f = open(cnf_nf,'r')
    line = cnf_f.readline().split(' ')
    old_nvars = int(line[2])
    old_nlines = int(line[3])

    #for saving result
    lines = []

    # Quantifiers
    # actions
    l = ['e']
    actions=set(file(actions_nf).readlines()[0].split()[1:])
    l.extend(actions)
    actions=set(actions)
    l.append('0')
    lines.append(l)
    # bits
    l = ['a']
    for b in range(1,num_bits+1):
        l.append(str(b+old_nvars))
    l.append('0')
    lines.append(l)
    # rest
    l = ['e']
    for v in range(1,old_nvars+1):
        if str(v) not in actions:
            l.append(str(v))
    l.append('0')
    lines.append(l)

    # Bits 2 state
    counter = 0
    new_lines = 0
    for mod in models:
        l = make_bits(counter,num_bits,old_nvars)
        #print 'HLP',l, mod
        for lit in mod:
            lines.append(l[:])
            lines[-1].extend([str(lit),'0'])
            new_lines += 1
        counter += 1

    # Saving file2
    qbf_f=open(qbf_nf,'w')
    qbf_f.write("p cnf %s %d\n" % (num_bits+old_nvars, new_lines+old_nlines) )
    for l in lines:
        #print 'HLP', l
        for a in l:
            qbf_f.write(a+' ')
        qbf_f.write('\n')
    for line in cnf_f.readlines():
        if '%' not in line:
            qbf_f.write(line)

    cnf_f.close()
    qbf_f.close()

def cf2logic(method,dump_qbf=False):
    global statline, plan_found, parallel, erasedebug, erase, isprob, use_lug, use_lug_last_layer
    global init_horiz, end_horiz, extra_room

    if method == 'cf2sat':
        pr('Starting cf2sat: translating from conformant planning to SAT')
    elif method == 'cf2mc':
        pr('Starting cf2mc: translating from conformant planning to Compiling+Model Counting')
    else:
        pr('Error calling cf2logic: wrong method %s' % method)
        sys.exit(1)

    # Translating problem so, it doesn't use oneof
    new_problem_nf=problem_nf+'2'
    erasedebug.add(new_problem_nf)
    os.system(Loc+'/oneof2clause.py '+problem_nf+' '+new_problem_nf)
    
    #cf2sat files
    base='full'
    init_nf=base+'-init.cnf'
    isprob_nf=base+'.isprob'
    cnf_nf=base+'.cnf'
    nnf_nf=cnf_nf+'.nnf'
    partnnf_nf=nnf_nf+'.partcnf'
    cnf2_nf=nnf_nf+'.cnf'
    rfluents_nf=base+'.rfluents'
    ifluents_nf=base+'.ifluents'
    atoms_nf=base+'.atoms'
    actions_nf=base+'.actions'
    actions2time_nf=base+'.actions2time'

    vars2prob_nf=base+'.vars2prob'
    auxvars_nf=base+'.auxvars'
    resolve='resolve_trace'
    
    erase.add(partnnf_nf)
    erase.add(resolve)
    erasedebug.add(isprob_nf)
    erasedebug.add(init_nf)
    erasedebug.add(cnf_nf)
    erasedebug.add(nnf_nf)
    erasedebug.add(cnf2_nf)
    erasedebug.add(rfluents_nf)
    erasedebug.add(ifluents_nf)
    erasedebug.add(atoms_nf)
    erasedebug.add(actions_nf)
    erasedebug.add(actions2time_nf)
    erasedebug.add(vars2prob_nf)
    erasedebug.add(auxvars_nf)
    
    if use_lug:
        lug_cnf_nf='cnf.out'
        lug_dd_nf='dd.out'
        lug_map_nf='mapping.out'
        erasedebug.add(lug_cnf_nf)
        erasedebug.add(lug_dd_nf)
        erasedebug.add(lug_map_nf)
        erasedebug.add(lug.debug_nf)
        
        #$LUGDIR/lug2txt $domain $prob -cnf_out 0 0 -1 0 0  2&>lug2txt.out
        cmd=[Loc+'/lug2txt',domain_nf,new_problem_nf]
        if use_mutex:
            cmd.append('-mutex')
        if use_lug_persist:
            cmd.append('-persist')
        if not use_lug_props:
            cmd.append('-no-props')
        if not use_lug_effs:
            cmd.append('-no-effs')
        if not use_lug_acts:
            cmd.append('-no-acts')
            
        cmd.extend(['-cnf_out','0','0','-1','0','0'])
        pr(gen_log('Generating LUG'))
        log(gen_log('--------- Calling',cmd))
        log_f.flush()
        init_t = resource.getrusage(resource.RUSAGE_CHILDREN)[0]
        devnull = open('/dev/null','w')
        lug2txt=subprocess.Popen(cmd, bufsize=-1, stdout=log_f, stderr=devnull )
        tokill.append(lug2txt.pid)
        res = lug2txt.wait()
        tokill.pop()
        elapsed = resource.getrusage(resource.RUSAGE_CHILDREN)[0] - init_t
        if( res < 0):
            pr('Error calling lug2txt: %d' % res)
            sys.exit(1)
        stat_f.write('lug_time:'+str(elapsed)+'\n')

        # LUG: Create structures for create CNF
        use_lug_last_layer = use_lug_last_layer and use_lug_persist
        mlug = lug.LUG( lug_map_nf, lug_cnf_nf, use_lug_last_layer )

        level_off = mlug.get_level()
        extra_room = mlug.get_n_extra()

        if level_off > end_horiz:
            pr('Level off of LUG: NO solution for horiz <',level_off)
            sys.exit(1)

        if level_off > init_horiz:
            print 'Starting at',level_off,'because LUG level off'
            init_horiz = level_off

        
    consistent=False
    rconsistent=False
    thereisplan=False
    
    n_atoms_init = -1
    num_s0s = -1
    new_cnf = []
    for k in range(init_horiz,end_horiz+1):
        if method == 'cf2sat' and statline == '' :
            statline = 'horizon cconf-time nvars nclauses c2d-time nvars2 nclauses2 sat-time\n'
        elif method == 'cf2mc' and statline == '' :
            statline = 'horizon cconf-time nvars nclauses c2d-time nnodes nedges backtracks nodes searchtime\n'
        else:
            stat_f.write(statline+'\n')
            statline = ''
        statline += str(k) + ';'
    
        pr('===== Horiz %d ==============='  % k)
        cmd=[Loc+'/cconf']
        if(parallel):
            cmd.extend(['-p'])
        if extra_room > 0:
            cmd.extend(['-x',str(extra_room)])
        cmd.extend(['-k',str(k),'-f','-o','full@'+ cnf_nf,'-o','init@'+ init_nf,\
                    '-o','auxvars@'+auxvars_nf,\
                    '-o','rfluents@'+ rfluents_nf,'-o','ifluents@'+ ifluents_nf,'-o','atoms@'+ atoms_nf,\
                    '-o','actions@'+ actions_nf,'-o','actions2time@'+ actions2time_nf,\
                    '-o','isprob@'+ isprob_nf,'-o','vars2prob@'+ vars2prob_nf,\
                    domain_nf,new_problem_nf])
        pr(gen_log('Generating CNF'))
        log(gen_log('--------- Calling',cmd))
        log_f.flush()
        init_t = resource.getrusage(resource.RUSAGE_CHILDREN)[0]
        cconf=subprocess.Popen(cmd, bufsize=-1, stdout=log_f, stderr=log_f, env={'C2D': Loc+'/c2d_220'} )
        tokill.append(cconf.pid)
        res = cconf.wait()
        tokill.pop()
        elapsed = resource.getrusage(resource.RUSAGE_CHILDREN)[0] - init_t
        if( res < 0):
            pr('Error calling cconf: %d' % res)
            sys.exit(1)

        statline += str(elapsed) + ';'

        isprob = is_prob( isprob_nf )
        if isprob:
            print 'Probabilistic domain detected'

        if method != 'cf2mc' and isprob:
            pr('Error method %s doesn\'t support probabilistic confomant planning' % method)
            sys.exit(1)

        if exit_when_cnf:
            pr('Cnf is ready...')
            sys.exit(1)

        if(n_atoms_init < 0):
            init_f = open(init_nf,'r')
            n_atoms_init = int(init_f.readline().split(' ')[2])
            init_f.close()

        if enum_s0: # For printing initial states and exit
            do_enum_s0(n_atoms_init,atoms_nf,init_nf)
            sys.exit(0)

        if new_cnf == []:
            matoms = Atoms(atoms_nf,auxvars_nf)
            if use_lug:
                new_cnf = mlug.get_cnf(matoms,n_atoms_init)
            elif extra_room > 0: # anulate extra vars
                new_cnf = []
                for v in matoms.extravars:
                    new_cnf.append([-v])
        elif use_lug and use_lug_last_again:
            new_cnf_layer = mlug.get_cnf_layer(k)
            new_cnf.extend(new_cnf_layer)
        
        
        if use_lug or extra_room > 0:
            concatenate_cnf(cnf_nf, new_cnf)
            concatenate_cnf(init_nf, new_cnf, False, True)

        if use_lug:
            simplify_cnf(cnf_nf)
        
        cnf_f = open(cnf_nf,'r')
        line = cnf_f.readline().split(' ')
        nvars = line[2]
        nclauses = line[3][0:-1]
        statline += nvars + ';' + nclauses + ';'
        cnf_f.close()
    
        if(num_s0s < 0):
            num_s0s = calc_num_s0s(init_nf)
            stat_f.write('NUM_S0:'+str(num_s0s)+'\n')
    
        if dump_qbf:
            make_qbf(k,n_atoms_init,problem_nf,cnf_nf,atoms_nf,init_nf,actions_nf)
            finish()
            sys.exit(0)

        if(not isprob and not consistent):
            consistent = is_consistent(n_atoms_init, init_nf, cnf_nf)
            if(consistent):
                pr('Problem has at least one plan candidate since horizon ' + str(k))
        if(isprob or consistent):
            # Probably -smooth_all should be prefer, but
            # it seems to make cf2mc failure over some block instance
            cmd2 = [Loc+'/c2d_220','-in',cnf_nf,'-dt_method','3','-force',ifluents_nf,\
                    '-exist', rfluents_nf, '-smooth_all','-reduce','-keep_trivial_cls']
            cmd2.append('-count')
            pr(gen_log('Compiling from CNF to d-DNNF'))
            log(gen_log('--------- Calling',cmd2))
            init_t = resource.getrusage(resource.RUSAGE_CHILDREN)[0]
            c2d=subprocess.Popen(cmd2, bufsize=-1, stdout=log_f, stderr=log_f )
            tokill.append(c2d.pid)
            res = c2d.wait();
            tokill.pop()
            elapsed = resource.getrusage(resource.RUSAGE_CHILDREN)[0] - init_t
            if(res < 0):
                pr('Error calling c2d: %d' % res)
                sys.exit(1)
    
            statline += str(elapsed) + ';'
    
            if(not isprob and not rconsistent):
                rconsistent = is_really_consistent(n_atoms_init, num_s0s, actions_nf, nnf_nf)
                if(rconsistent):
                    pr('Problem has at least one plan candidate for each s0 since horizon ' + str(k))
            if(isprob or rconsistent):
                if method == 'cf2sat':
                    found, plan_atoms = cf2sat(nnf_nf, n_atoms_init, cnf2_nf, actions_nf )
                elif method == 'cf2mc':
                    found, plan_atoms = cf2mc(nnf_nf, n_atoms_init, num_s0s, actions2time_nf,\
                                              vars2prob_nf )

                if not found:
                    pr('solution not found')
                    if not isprob:
                        continue
                else:
                    pr('solution FOUND')
                    pr('LAST_HORIZ='+str(k))
    
                    atom2fluent = calc_atom2fluent(atoms_nf)

                    pr('Plan: ')
                    plan=[]
                    for atom in plan_atoms:
                        action = atom2fluent[atom]
                        pr(str(atom2time[atom])+':'+str(action))
                        plan.append(action)

                    statline += '\nPLAN_LENGTH:' + str(len(plan_atoms))

                    save_plan(plan)
                    plan_found=True
                break
    if(not plan_found):
        pr(gen_log('Plan not found: Horizont limit reached'))


def mysatplan(num_solutions):
    global statline, plan_found, parallel, erasedebug, erase
    global init_horiz, end_horiz

    # Translating problem so, it doesn't use oneof
    new_problem_nf=problem_nf+'2'
    erasedebug.add(new_problem_nf)
    os.system(Loc+'/oneof2clause.py '+problem_nf+' '+new_problem_nf)
    
    #cf2sat files
    base='full'
    init_nf=base+'-init.cnf'
    cnf_nf=base+'.cnf'
    atoms_nf=base+'.atoms'
    actions_nf=base+'.actions'

    vars2prob_nf=base+'.vars2prob'
    auxvars_nf=base+'.auxvars'
    resolve='resolve_trace'
    
    erase.add(resolve)
    erasedebug.add(init_nf)
    erasedebug.add(cnf_nf)
    erasedebug.add(atoms_nf)
    erasedebug.add(actions_nf)
    erasedebug.add(auxvars_nf)

    consistent=False

    n_atoms_init = -1
    num_s0s = -1
    new_cnf = []
    for k in range(init_horiz,end_horiz+1):
        if statline == '' :
            statline = 'horizon cconf-time nvars nclauses c2d-time nvars2 nclauses2 sat-time\n'
        else:
            stat_f.write(statline+'\n')
            statline = ''
        statline += str(k) + ';'
    
        pr('===== Horiz %d ==============='  % k)
        cmd=[Loc+'/cconf']
        if(parallel):
            cmd.extend(['-p'])
            
        # -s for not simplifyng the theory using C2D
        cmd.extend(['-s','-k',str(k),'-f','-o','full@'+ cnf_nf,\
                        '-o','init@'+ init_nf,\
                        '-o','auxvars@'+auxvars_nf,\
                        '-o','atoms@'+ atoms_nf,\
                        '-o','actions@'+ actions_nf,\
                        domain_nf,new_problem_nf])
        pr(gen_log('Generating CNF'))
        log(gen_log('--------- Calling',cmd))
        log_f.flush()
        init_t = resource.getrusage(resource.RUSAGE_CHILDREN)[0]
        cconf=subprocess.Popen(cmd, bufsize=-1, stdout=log_f, stderr=log_f, env={'C2D': Loc+'/c2d_220'} )
        tokill.append(cconf.pid)
        res = cconf.wait()
        tokill.pop()
        elapsed = resource.getrusage(resource.RUSAGE_CHILDREN)[0] - init_t
        if( res < 0):
            pr('Error calling cconf: %d' % res)
            sys.exit(1)

        statline += str(elapsed) + ';'

        if exit_when_cnf:
            pr('Cnf is ready...')
            sys.exit(1)

        if(n_atoms_init < 0):
            init_f = open(init_nf,'r')
            n_atoms_init = int(init_f.readline().split(' ')[2])
            init_f.close()

        if enum_s0: # For printing initial states and exit
            do_enum_s0(n_atoms_init,atoms_nf,init_nf)
            sys.exit(0)

        if new_cnf == []:
            matoms = Atoms(atoms_nf,auxvars_nf)

        cnf_f = open(cnf_nf,'r')
        line = cnf_f.readline().split(' ')
        nvars = line[2]
        nclauses = line[3][0:-1]
        statline += nvars + ';' + nclauses + ';'
        cnf_f.close()
    
        if(num_s0s < 0):
            num_s0s = calc_num_s0s(init_nf)
            stat_f.write('NUM_S0:'+str(num_s0s)+'\n')
    
        if(num_s0s > 1):
            print 'Error: running mysatplan but num of initial states > s0'
            print 'do you want tu run translator as a conformant planner?'
            exit(1)

        if(not consistent):
            consistent = is_consistent(n_atoms_init, init_nf, cnf_nf)
            if(consistent):
                pr('Problem has at least one plan candidate since horizon ' + str(k))
                
        if consistent:
            found, all_plan_atoms = try_relsat(cnf_nf, actions_nf, num_solutions)
            if not found:
                pr('solution not found')
                if not isprob:
                    continue
            else:
                pr('solution FOUND')
                pr('LAST_HORIZ='+str(k))

                atom2fluent = calc_atom2fluent(atoms_nf)
                
                n=1
                for plan_atoms in all_plan_atoms:
                    if num_solutions > 1:
                        pr('Plan ('+str(n)+'): ')
                    else:
                        pr('Plan: ')
                    plan=[]
                    for atom in plan_atoms:
                        action = atom2fluent[atom]
                        pr(str(atom2time[atom])+':'+str(action))
                        plan.append(action)

                    statline += '\nPLAN_LENGTH:' + str(len(plan_atoms))
                    
                    if num_solutions > 1:
                        save_plan(plan,'.'+str(n))
                    else:
                        save_plan(plan)
                    n += 1
                plan_found=True
            break
    if(not plan_found):
        pr(gen_log('Plan not found: Horizont limit reached'))


# Init of main matters

init_horiz=0
end_horiz=100
parallel=True
dump_qbf=False
simple_prunning = True
strong_prunning = True
most_likely_selection = False
check_plan=False
do_debug=False
verbosity=10
extra_room=0
exit_when_cnf=False
use_lug=False
use_mutex=False
use_lug_last_again=True
use_lug_persist=False
use_lug_props = True
use_lug_effs = True
use_lug_acts = True
use_lug_last_layer = True
classical_planner='ff' # or 'lama'
extra_options = []
enum_s0 = False
num_solutions = 1

Loc_v='TRANSLATOR_HOME'
planner='translator'
remove_log=False
statline=''
cleaning_functions=[]
isprob=False
time_limit=0
# Default strategy
sstrategy='t0:1800'
prefix=''
plan_found=False
# files to keep on debug
erasedebug=set()

if len(sys.argv) < 3:
    usage()

i=1
try:
    while i < len(sys.argv):
        if(sys.argv[i] == '-i'):
            init_horiz = int(sys.argv[i+1])
            i += 2
        elif(sys.argv[i] == '-e'):
            end_horiz = int(sys.argv[i+1])
            i += 2
        elif(sys.argv[i] == '-v'):
            verbosity = int(sys.argv[i+1])
            i += 2
        elif(sys.argv[i] == '-s'):
            sstrategy = sys.argv[i+1]
            i += 2
        elif(sys.argv[i] == '-l'):
            prefix = sys.argv[i+1]
            i += 2
        elif(sys.argv[i] == '-t'):
            time_limit = int(sys.argv[i+1])
            i += 2
        elif(sys.argv[i] == '-z'):
            parallel = False
            i += 1
        elif(sys.argv[i] == '-qbf'):
            dump_qbf = True
            i += 1
        elif(sys.argv[i] == '-f'):
            remove_log = True
            i += 1
        elif(sys.argv[i] == '-c'):
            check_plan = True
            i += 1
        elif(sys.argv[i] == '-d'):
            do_debug = True
            i += 1
        elif(sys.argv[i] == '-nsol'):
            num_solutions =  int(sys.argv[i+1])
            i += 2
        elif(sys.argv[i] == '-nsim'):
            simple_prunning = False
            i += 1
        elif(sys.argv[i] == '-nstr'):
            strong_prunning = False
            i += 1
        elif(sys.argv[i] == '-lik'):
            most_likely_selection = True
            i += 1
        elif(sys.argv[i] == '-x'):
            extra_room = int(sys.argv[i+1])
            i += 2
        elif(sys.argv[i] == '-cnf'):
            exit_when_cnf = True
            i += 1

# Lug options
        elif(sys.argv[i] == '-lug'):
            use_lug = True
            i += 1
        elif(sys.argv[i] == '-mut'):
            use_mutex = not use_mutex
            i += 1

        elif(sys.argv[i] == '-lper'):
            use_lug_persist = not use_lug_persist
            i += 1
        elif(sys.argv[i] == '-lnag'):
            use_lug_last_again = not use_lug_last_again
            i += 1
        elif(sys.argv[i] == '-ln_llay'):
            use_lug_last_layer = not use_lug_last_layer
            i += 1

        elif(sys.argv[i] == '-lno_acts'):
            use_lug_acts = not use_lug_acts
            i += 1
        elif(sys.argv[i] == '-lno_props'):
            use_lug_props = not use_lug_props
            i += 1
        elif(sys.argv[i] == '-lno_effs'):
            use_lug_effs = not use_lug_effs
            i += 1
#cf2cs options
        elif(sys.argv[i] == '-cp'):
            classical_planner=sys.argv[i+1]
            i += 2
        elif(sys.argv[i] == '-trans'):
            extra_options.extend(sys.argv[i+1].split(','))
            i += 2
        elif(sys.argv[i] == '-enum-s0'):
            enum_s0 = True
            i += 1
        elif(sys.argv[i].startswith('-')):
            usage()
        else:
            break
except:
    print 'Error processing the options', sys.argv
    sys.exit(1)


# If last file is a tar, assume it contains both files
[has_tar,domain_nf,problem_nf] = pddlsfromtar.get_pddls(sys.argv[i])
if i + 1 == len(sys.argv) and has_tar:
    erasedebug.add(domain_nf)
    erasedebug.add(problem_nf)
elif i + 2 > len(sys.argv):
    usage()
else:
    domain_nf=sys.argv[i]
    problem_nf=sys.argv[i+1]

if(not os.access(os.path.abspath(domain_nf),os.F_OK) ):
    print 'domain file %s does not exist' % domain_nf
    sys.exit(1)

if(not os.access(os.path.abspath(problem_nf),os.F_OK) ):
    print 'problem file %s does not exist' % problem_nf
    sys.exit(1)

# Interchange problem_nf and domain_nf if passed different
for l in file(problem_nf,'r'):
    if '(define' in l and '(domain' in l:
        tmp=problem_nf
        problem_nf=domain_nf
        domain_nf=tmp
        print 'Domain and problem file interchanged. Usually the results are the same anyway\n'
        break

# problem name
probname=problem_nf.split('/')[-1].split('.')[0]
if prefix == '':
    out=planner+'_'+strategy_name+'_'+probname
else:
    out=prefix+'_'+probname

#output files
log_nf=out+'.log'

if(os.access(os.path.abspath(log_nf),os.F_OK)):
    if(remove_log):
        weak_unlink(log_nf)
    else:
        print 'log file exist:', log_nf
        print 'erase it before or use \'-f\' option to overwrite it'
        sys.exit(1)

log_f=open(log_nf,'w')

if use_lug and (not use_lug_acts and not use_lug_props and not use_lug_effs):
    pr('Using lug, at least acts props or effects should be used')
    sys.exit(1)

Loc=''
try:
    Loc=os.environ[Loc_v]
    if(Loc == '' or not os.access(Loc,os.F_OK)):
        pr('Enviroment var',Loc_v,'is not set to an existing directory')
        sys.exit(1)
except:
    pass

if Loc == '':
    Loc='.'
    print 'Assuming than all required files are on current directory\n'

valid_translation=set(['satplan', 'satmemless', 'sat', 'mc', 'k0', 't0', 'only-t0', 'old-t0', 'k1', 'only-k1', 's0', 'fs0', 't0c', 't0n', 't0s', 'kp'])
strategy=[]

strategy_name=''
for step in sstrategy.split(';'):
    pair = step.split(':')
    if(len(pair) != 2):
        pr('Strategy bad formed. Run %s without arguments to obtain help' % planner)
        sys.exit(1)
    translation=pair[0].strip()
    if(not translation in valid_translation):
        pr('Invalid translation: %s. Run %s without arguments to obtain help' % (translation, planner))
        sys.exit(1)
    t=pair[1].strip()
    if(t != 'inf' and not t.isdigit()):
        pr('bad formed time: %s. Run %s without arguments to obtain help' % (t, planner))
        sys.exit(1)
    if(strategy_name == ''):
        strategy_name = translation+'-'+t
    else:
        strategy_name += '_'+translation+'-'+t
    strategy.append((translation, t))


pr('%s: a translation-based conformant planner' % planner.upper())
pr('UPF - 2006')
pr('')
cmdline = 'calling: '
for i in range(0,len(sys.argv)):
    cmdline += sys.argv[i] + ' '
pr(gen_log(cmdline,date=True))

pr('Log file %s' % log_nf)

pr('Strategy -> '+sstrategy)

stat_nf=out+'.stat'
weak_unlink(stat_nf)
stat_f=open(stat_nf,'w')

print >> stat_f, 'Problem file:',problem_nf
print >> stat_f, 'Domain file:',domain_nf
try:
    pwd=os.environ['PWD']
    print >> stat_f, 'Working directory:',pwd
except:
    pass

ipc5_nf=out+'.ipc5'
weak_unlink(ipc5_nf)

time_nf=out+'.time'
weak_unlink(time_nf)

tmpdir=os.environ['PWD']

# Pids of process started
pids=set()
# files to erase
erase=set()

# Set the signal handler and a alarm
signal.signal(signal.SIGINT, killall)
signal.signal(signal.SIGTERM, killall)

def call_translation(translation):
    # Straight forwar sat plan, good for generating many optimal plans for learning purpose
    if(translation=='satplan'):
        mysatplan(num_solutions)

    # Not done yet
    elif(translation=='satmemless'):
        sat_memory_less() 

    # Complete translations using Prop Logic
    elif(translation=='sat'):
        cf2logic('cf2sat',dump_qbf)
    elif(translation=='mc'):
        cf2logic('cf2mc')

    # Complete translations to Classical Planning
    elif(translation=='k0'):
        cf2cs('cf2cs','-k0')
    elif(translation=='t0'):
        cf2cs('cf2cs','-ak1 -static_disj -actcomp -and -s0 -static_disj -actcomp') 
    elif(translation=='only-t0'):
        cf2cs('cf2cs','-ak1 -static_disj -actcomp')
    elif(translation=='old-t0'):
        cf2cs('cf2cs','-t0 -static_disj -actcomp -and -s0 -static_disj -actcomp')
    elif(translation=='k1'):
        cf2cs('cf2cs','-k1 -static_disj -actcomp -and -s0 -static_disj -actcomp')
    elif(translation=='only-k1'):
        cf2cs('cf2cs','-k1 -static_disj -actcomp')
    elif(translation=='s0'):
        cf2cs('cf2cs','-s0 -static_disj -actcomp')
    elif(translation=='fs0'):
        cf2cs('cf2cs','-fs0')

    # For verifying consistency
    elif(translation=='t0c'):
        cf2cs('cf2cs','-pconsistent -ak1 -static_disj -actcomp -and -s0 -static_disj -actcomp')

    # Options for cf2cs old version (in C++)
    elif(translation=='t0n'):
        cf2cs('cf2cs','-k0 -and -ak1 -static_disj -actcomp -and -s0 -static_disj -actcomp')
    elif(translation=='t0s'):
        cf2cs('cf2cs','-t0 -and -s0')
    elif(translation=='kp'):
        cf2cs('cf2cs','-kp -mac')

    else:
        pr('bad translation in strategy: ', translation)
        sys.exit(1)

def do_timeout(translation):
    global statline
    pr('-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-')
    pr('Time out for %s.' % translation)
    clean_children()
    stat_f.write(statline+'\nTIMEOUT\n---------------\n')
    statline=''
    pr('-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-')

lost_used = 0
try:
    for translation, t in strategy:
        if(plan_found): break
        if(t == 'inf'):
            if(time_limit <= 0):
                rtime='inf'
            else:
                rtime = time_limit - global_time()
        elif(t.isdigit()):
            if(time_limit <= 0):
                rtime = int(t)
            else:
                rtime = min(int(t), time_limit - global_time())
        else:
            pr('ill formed time in strategy: ', time)
            sys.exit(1)


        if(rtime <= 0): 
            do_timeout(translation)
            break

        stat_f.write('Translation:'+translation+'\n')
        pr('Calling translation %s during %s' % (translation,rtime))
        if(rtime == 'inf'):
            call_translation(translation)
        else:
            t = int(rtime)
            lost = lost_wall_clock()
            t += lost-lost_used
            lost_used = lost
            pr('Recovering %s second lost' % lost_used)
            try:
                timeout.TimedOutFn(call_translation, int(t), translation)
            except timeout.TimedOutExc:
                do_timeout(translation)
except IOError, e:
    pr('-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-')
    pr('Ending abruptly. Last operation failed. See log file '+log_nf)
    clean_children()


final_time=global_time()
if(plan_found and check_plan):
    cmd=[Loc+'/verify','-plan',ipc5_nf,domain_nf,problem_nf]
    pr(gen_log('--------- Calling',cmd))
    verify=subprocess.Popen(cmd, bufsize=-1, stdout=subprocess.PIPE, stderr=log_f )
    tokill.append(verify.pid)
    isvalid = False
    for l in verify.stdout.readlines():
        pr(l)
        if(l.startswith('valid plan')):
            isvalid=True
    statline += '\nVALID:'
    if(isvalid):
        statline += 'YES'
    else:
        statline += 'NO'
    res = verify.wait()
    tokill.pop()
    if(res < 0):
        pr('Error calling verify: %d' % res)        

statline += '\n'
finish()
sys.exit(0)
