#! /usr/bin/env python2.5
# -*- coding: latin-1 -*-

#  Copyright (C) 2008 Universitat Pompeu Fabra
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

import re
import sys
import subprocess
import os
import parsepddl

###############################
# Save new Problem PDDL
###############################

def rec_save(indent,f,lst):
  if ' ' in lst:
    lst = lst.split()
  print >> f, indent*' ',
  if type(lst) == str:
    print >> f, lst
  elif type(lst) == list:
    if lst != []:
      print >> f, '(',
      for l in lst:
        rec_save(indent+3,f,l)
      print >> f, indent*' ', ')'
  else:
    print 'Error:',lst
    raise SystemExit("Not list or string in pddl.")

def save_pddl(nf,lst):
  f = file(nf,'w')
  rec_save(0,f,lst)
  f.close()


###############################
# Keywords of PDDL
###############################

problem_kwd = 'PROBLEM'
domainp_kwd = ':DOMAIN'
objects_kwd = ':OBJECTS'
init_kwd = ':INIT'
goal_kwd = ':GOAL'
define_kwd = 'DEFINE'
domain_kwd = 'DOMAIN'
requirements_kwd = ':REQUIREMENTS'
types_kwd = ':TYPES'
constants_kwd = ':CONSTANTS'
predicates_kwd = ':PREDICATES'
action_kwd = ':ACTION'
parameters_kwd = ':PARAMETERS'
precondition_kwd = ':PRECONDITION'
effect_kwd = ':EFFECT'
oneof_kwd = 'ONEOF'
when_kwd = 'WHEN'
and_kwd = 'AND'
not_kwd = 'NOT'
or_kwd = 'OR'

###############################
# Converting to conformant
###############################

def create_action(name,parameters,precond,effects):
  res = [action_kwd,name]
  if parameters <> []:
    res.extend([parameters_kwd,parameters])
  if precond <> [] and precond <> [and_kwd]:
    res.extend([precondition_kwd,precond])
  res.extend([effect_kwd,effects])
  return res

def deep_contains(lst,elem,starts = False):
  if type(lst) is not list:
    if starts:
      return lst.startswith(elem)
    else:
      return lst == elem
  else:
    for l in lst:
      if deep_contains(l,elem,starts):
        return True
    return False

# A oneof can occur only once (Dan Bryce dixit)
def deep_replace_effect(e,act_param,params_no_type,new_pred_predix,new_pred_postfix,index,np,ni):
  #print 'act_param',act_param,params_no_type
  if type(e) is not list:
    return [np,ni,e,index]
  else:
    ne=[]
    for l in e:
      if l[0] == oneof_kwd:
        # Do replace
        # For each element, introduce a predicate...
        nne=[and_kwd]
        np_now = []
        for it in l[1:]:
          #print 'it',it
          cond = [new_pred_predix+str(index)+'-'+new_pred_postfix]
          index = index+1
          pred = cond[:]
          cond.extend(params_no_type)
          pred.extend(act_param)
          np_now.append(pred)
          if it != []:
            nne.append([when_kwd,cond,it])
            # Return, meanwhile un instantiated oneof
        nni = [oneof_kwd]
        nni.extend(np_now)
        ni.append(nni)
        np.extend(np_now)
        ne.append(nne)
      else:
        [np,ni,nl,index] = deep_replace_effect(l,act_param,params_no_type,new_pred_predix,new_pred_postfix,index,np,ni)
        ne.append(nl)
  #print 'partial',np,ni,ne,index
  return [np,ni,ne,index]

# Flat 'when'
def collect_flat_when(effect,context,is_new_context,context2effect):
  if type(effect) is not list:
    return [False,effect]
  if effect[0] == when_kwd:
    c2 = context[:] 
    if and_kwd == effect[1][0]:
      c2.extend(effect[1][1:])
    else:
      c2.append(effect[1])
    collect_flat_when(effect[2],c2,True,context2effect)
    return [True,[]]
  else:
    new_e = []
    some_have_when = False
    for e in effect:
      [have_when,collected] = collect_flat_when(e,context,False,context2effect)
      some_have_when = some_have_when or have_when
      if collected != [] and collected <> [and_kwd]:
        #the effect where just (and (when) (when))
        new_e.append(collected)
    if len(new_e) == 2 and new_e[0] == and_kwd:
      #the effect where just (and something (when) (when))
      new_e = new_e[1]
    if is_new_context:
      if new_e <> [and_kwd]: 
        context2effect.append((context,new_e))
      return [some_have_when,[]]
    else:
      return [some_have_when,new_e]
      
# Doesn't eliminate repetitions. Probably not necessary if there weren't in original PDDL
def flat_when(effect):
  context2effect = []
  [have_when,new_e] = collect_flat_when(effect,[],False,context2effect)
  if False:
    print '--------------------'
    print 'context2effect:'
    for x in context2effect:
      print x
    print '--------------------'
    print 'new_e:'
    for x in new_e:
      print x
    print '--------------------'
  if have_when:
    if len(new_e) == 0 or new_e[0] != and_kwd:
      res=[and_kwd]
      res.extend(new_e)
    else:
      res=new_e
    for (c,e) in context2effect:
      new_cond = [and_kwd]
      new_cond.extend(c)
      res.append([when_kwd,new_cond,e])
    return res
  else:
    return effect

def get_oneof_params(effect, sofar = set([]), inoneof=False):
  if type(effect) is not list:
    if inoneof and effect != oneof_kwd and effect != not_kwd:
      try:
        if effect[0] == '?':
          sofar.add(effect)
      except:
        pass
  else:
    if effect[0] == oneof_kwd:
      inoneof=True
    for it in effect[1:]:
      get_oneof_params(it,sofar,inoneof)
  return sofar

def filter_params(effects,parameters):
  debug_it = False
  if debug_it:
    print 'effects', effects
  oneof_params = get_oneof_params(effects)
  if debug_it:
    print 'oneof_params',oneof_params
  tmp=[]
  i = 0
  while i < len(parameters):
    it = parameters[i]
    if it == '-':
      tmp.append(it)
      tmp.append(parameters[i+1])
      i = i+2
    else:
      if it in oneof_params:
        tmp.append(it)
      i = i+1
  if debug_it:
    print 'tmp',tmp
  # clear types without objs
  nparameters=[]
  i = 0
  there_is_obj = False
  while i < len(tmp):
    #print i, nparameters
    it = tmp[i]
    if it == '-':
      if there_is_obj:
        nparameters.append(it)
        nparameters.append(tmp[i+1])
      i = i+2
      there_is_obj = False
    else:
      nparameters.append(it)
      there_is_obj = True                        
      i = i+1
  return nparameters

def get_params_no_type(parameters):
  params_no_type = []
  i = 0
  while i < len(parameters):
    it = parameters[i]
    if it == '-':
      i = i+2
    else:
      params_no_type.append(it)
      i = i+1
  return params_no_type

def get_type(a,obj2type):
  i = len(a)-1
  ctype = ''
  while i >= 0:
    if a[i][0] == ':':
      i = -1 # Stop
    elif a[i-1] == '-':
      ctype = a[i]
      i = i-2
    else:
      if ctype not in obj2type:
        obj2type[ctype] = set([a[i]])
      else:
        obj2type[ctype].add(a[i])
      i = i-1


def add_combination_init(acc,a,type_of_obj,new_init):
  if a == []:
    new_init.append(acc)
  else:
    it = a[0]
    if a[1] == '-':
      rest = a[3:]
    else:
      rest = a[1:]
    for c in type_of_obj[it]:
      acc_new = acc[:]
      acc_new.append(c)
      add_combination_init(acc_new,rest,type_of_obj,new_init)

def calc_combinations(obj2type,lit):
  posible_objs = {}
  pred = lit[0]
  a = lit[1:]
  get_type(a,posible_objs)
  type_of_obj = {}
  for i in posible_objs:
    for j in posible_objs[i]:
      type_of_obj[j] = obj2type[i]
  combinations = []
  add_combination_init([],a,type_of_obj,combinations)
  return combinations

def instantiate_one(obj2type,pred):
  new = []
  combinations = calc_combinations(obj2type,pred)
  for comb in combinations:
    n = [pred[0]]
    n.extend(comb)
    new.append(n)
  return new

def instantiate_init(obj2type,ninit):
  new_init = []
  for o in ninit:
    # by construction, combinations of objects are the same for each lit in 'o'
    # so pick first, and calculate them
    combinations = calc_combinations(obj2type,o[1])
    if False:
      print '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'
      print 'combinations_init'
      for x in combinations:
        print x
    for comb in combinations:
      n = [oneof_kwd]
      for lit in o[1:]:
        term = [lit[0]]
        term.extend(comb)
        n.append(term)
      new_init.append(n)
      
      if False:
        print '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
        print obj2type
        print
        print posible_objs
        print
        print type_of_obj
        print
        print '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
  return new_init

def print_ordered(n,e):
  if type(e) is not list:
    print e,
  else:
    each_one_line = False
    if e[0] == and_kwd or\
          e[0] == when_kwd:
      each_one_line = True
    print '[',
    print_ordered(n+1,e[0])
    if each_one_line:
      print
    for l in e[1:]:
      if each_one_line:
        print '  '*n,
      print_ordered(n+1,l)
    print ']',
    if each_one_line:
      print


def nondet2conf(domain_nf, problem_nf, prefix, method='-a'):
  ###############################
  # Loading Problem PDDL
  ###############################

  problem_pddl=parsepddl.parse_pddl_file('problem',problem_nf)
  #print problem_pddl
  domain_pddl=parsepddl.parse_pddl_file('domain',domain_nf)
  #print domain_pddl

  pname=[]
  pdomain=[]
  pobjects=[]
  pinit=[]
  pgoals=[]  
  for l in problem_pddl:
    first = l[0]
    if first == problem_kwd:
      pname = l
    elif first == domainp_kwd:
      pdomain = l
    elif first == objects_kwd:
      pobjects = l
    elif first == init_kwd:
      pinit = l
    elif first == goal_kwd:
      pgoals = l
    elif l == define_kwd:
      pass
    else:
      print 'ERROR ON PARSING PROBLEM FILE:',problem_nf,'Offender:',l
      sys.exit(0)

  if False:
    print 'Problem:'
    print pname
    print pdomain
    print pobjects
    print pinit
    print pgoals

  #convert Init, removing 'and'
  if len(pinit) == 2 and pinit[1][0] == and_kwd:
    pinit_new = [init_kwd]
    pinit_new.extend(pinit[1][1:])
    pinit = pinit_new
    
  ddomain = []
  drequirements = []
  dtypes = []
  dconstants = []
  dpredicates = []
  dactions = []
  for l in domain_pddl:
    first = l[0]
    if first == domain_kwd:
      ddomain = l
    elif first == requirements_kwd:
      drequirements = l
    elif first == types_kwd:
      dtypes = l
    elif first == constants_kwd:
      dconstants = l
    elif first == predicates_kwd:
      dpredicates = l
    elif first == action_kwd:
      dactions.append(l)
    elif l == define_kwd:
      pass
    else:
      print 'ERROR ON PARSING DOMAIN FILE:',problem_nf,'Offender:',l
      sys.exit(0)

  if False:
    print 'Domain:'
    print ddomain
    print drequirements
    print dtypes
    print dconstants
    print dpredicates
    print dactions

  ###############################
  # Moving complex init into an action
  ###############################
    
  new_precond = []

  def extract_comp_oneof(lst):
    ninit=[]
    comp_oneof=[]
    for l in lst:
      if (l[0] == oneof_kwd and
          deep_contains(l,and_kwd)):
        comp_oneof.append(l)
      else:
        ninit.append(l)
    return [ninit,comp_oneof]

  action_start = 'MERGE_START'

  [ninit,comp_oneof] = extract_comp_oneof(pinit)
  if comp_oneof <> []:
    if False:
      print '******************** Init simple:',ninit
      print '******************** oneof complejo:',comp_oneof

    # Predicates
    pred_do_start = 'DO-START---------------'
    pred_do_rest = 'DO-CONTINUE------------'
    if deep_contains(domain_pddl,pred_do_start):
      print "ERROR. Domain pddl contains a predicate to be added:",pred_do_start
      sys.exit(1)
    if deep_contains(domain_pddl,pred_do_rest):
      print "ERROR. Domain pddl contains a predicate to be added:",pred_do_rest
      sys.exit(1)

    dpredicates.append([pred_do_start])
    dpredicates.append([pred_do_rest])

    # new precond to all actions
    new_precond = [pred_do_rest]


    if method == '-a':
      # Solving complex oneof by adding actions

      # new init
      pinit = ninit
      pinit.append([pred_do_start])

      # new Action
      if deep_contains(domain_pddl,action_start):
        print "ERROR. Domain pddl contains an action to be added:",action_start
        sys.exit(1)

      effects = [[not_kwd,[pred_do_start]],[pred_do_rest]]
      effects.extend(comp_oneof)

      dactions.append(create_action(action_start,[],[pred_do_start],effects))

      # move objects to constants
      dconstants.extend(pobjects[1:])
      pobjects = []
    elif method == '-c':
      # Solving complex oneof by adding new clauses

      pred_prefix = 'ONEOFINIT-'
      if deep_contains(domain_pddl,pred_prefix):
        print "ERROR. Domain pddl contains a predicate to be added:",pred_prefix
        sys.exit(1)

      # new init
      pinit = ninit
      pinit.append([pred_do_start])
      
      new_pred_count = 0
      for o in comp_oneof:
        no = [oneof_kwd]
        new_c = []
        for and_term in o[1:]:
          npred = [pred_prefix+str(new_pred_count)]
          dpredicates.append(npred)
          new_pred_count += 1

          no.append(npred)
          last_c = [or_kwd,npred]
          for l in and_term[1:]:
            last_c.append([not_kwd,l])
            c = [or_kwd,[not_kwd,npred],l]
            new_c.append(c)
          new_c.append(last_c)
        new_c.append(no)
        print 'For oneof:',o
        print 'got:'
        for c in new_c:
          print c
        print '--------------------------------------------------'
        pinit.extend(new_c)
        new_c=[]
    else:
      assert(method <> '-a' and method <> '-c')


    if False:
      print 'new Problem:'
      print pname
      print pdomain
      print pobjects
      print pinit
      print pgoals

    if False:
      print 'new Domain:'
      print ddomain
      print drequirements
      print dtypes
      print dconstants
      print dpredicates
      print dactions

  ###############################
  # Loading constants and objects for instantiation
  ###############################

  obj2type = {}
  
  # Only support simple types
  if '-' in dtypes:
    print 'ERROR: complex types not supported yet (but should be easy to fix)'
    sys.exit(0)
  for a in [dconstants,pobjects]:
    get_type(a,obj2type)
  ctype = ''
  if ctype not in obj2type:
    obj2type[ctype] = set([])
  for x in obj2type:
    for y in obj2type[x]:
      obj2type[ctype].add(y)

  ###############################
  # Processing actions
  ###############################

  npredicates = []
  ninit = []
  nactions = []
  predicate_prefix = "ONEOF----"

  if deep_contains(problem_pddl,predicate_prefix,True):
    print "ERROR. Problem pddl contains the prefix to be added:",predicate_prefix
    sys.exit(1)
  if deep_contains(domain_pddl,predicate_prefix,True):
    print "ERROR. Domain pddl contains the prefix to be added:",predicate_prefix
    sys.exit(1)

  # Process each action
  for act in dactions:
    name = []
    parameters = []
    precond = []
    effects = []
    i = 0
    while i < len(act):
      if act[i] == action_kwd:
        name = act[i+1]
      elif act[i] == parameters_kwd:
        parameters = act[i+1]
      elif act[i] == precondition_kwd:
        precond = act[i+1]
      elif act[i] == effect_kwd:
        effects = act[i+1]
      else:
        print 'ERROR ON PARSING DOMAIN FILE:',problem_nf,'Offender:',l
      i=i+2

    # Action loaded. Does it have a oneof?
    #print 'ACTION:',name,parameters,precond,effects

    # Assume it has oneof
    # It doesn't hurt, and sometimes a new precond has to be added

    new_pred_prefix = predicate_prefix
    new_pred_postfix = name

    if False:
      nparameters = parameters[:]
    else:
      # If use less parameters, just keep
      # parameters inside oneof
      if False:
        print 'Parameters', parameters
      nparameters = filter_params(effects,parameters)
      if False:
        print 'NParameters', nparameters


    # Parameters without type info for replacing
    params_no_type = get_params_no_type(parameters)
    nparams_no_type = get_params_no_type(nparameters)

    debug_replace_and_flat = False
    # Replace each oneof
    index=0
    [np,ni,ne,index] = deep_replace_effect(effects,nparameters,nparams_no_type,new_pred_prefix,new_pred_postfix,index,[],[])
    if debug_replace_and_flat:
      print 'for effect:',
      print_ordered(0,effects)
      print 'with parameters:',parameters
      print 'result np',np
      print 'result ni',ni
      print 'result ne'#,ne
      print_ordered(0,ne)
      print 'fin ne'
    has_oneof = np <> []
    npredicates.extend(np)
    ninit.extend(ni)
    
    # Normalize effects (no when inside when)
    # Idea: remember context of effects, dump new when...
    ne2 = flat_when(ne)
    if debug_replace_and_flat:
      print '===================='
      print 'new ne:'
      print_ordered(0,ne2)
      print '===================='

    def add_precond(precond,new_precond):
      if new_precond <> []:
        if precond == []:
          precond = [and_kwd,new_precond]
        if precond[0] == and_kwd:
          precond.append(new_precond)
        else:
          p = [and_kwd]
          p.append(precond)
          p.append(new_precond)
          precond = p
      return precond
        
    # Add new precond, in case it was set
    if name <> action_start:
      precond = add_precond(precond,new_precond)

    # Allow action to be executed once
    if has_oneof: 
      pred_once = [new_pred_prefix+new_pred_postfix+'-once']
      cond_once = pred_once[:]
      cond_once.extend(nparameters)
      pred_once.extend(nparams_no_type)
      precond = add_precond(precond,pred_once)
      npredicates.append(cond_once)
      cond_once_inst = instantiate_one(obj2type,cond_once)
      pinit.extend(cond_once_inst) #aqui
      ne2.append([not_kwd,pred_once])

    # Create new action
    nactions.append(create_action(name,parameters,precond,ne2))

  if False:
    print '===================='
    print 'npredicates'
    for i in npredicates:
      print i
    print '===================='
    print 'ninit'
    for i in ninit:
      print i
    print '===================='
    print 'nactions'
    for i in nactions:
      print i
    print '===================='

  # Instantiate init: create ground copies for each possible (type-legal) combination
  if False:
    print 'ninit BEFORE instantiated'
    for i in ninit:
      print i
    print '===================='
  ninit = instantiate_init(obj2type,ninit)
  if False:
    print 'ninit instantiated'
    for i in ninit:
      print i
    print '===================='

  # Save problem, including new init, preds, actions
  # In init, all new oneof should be instantiated with
  # all posible objects and constants (using types)

  pinit.extend(ninit)
  problem = [define_kwd,pname,pdomain,pobjects,pinit,pgoals]
  save_pddl(prefix+'-p.pddl',problem)
  dpredicates.extend(npredicates)
  # Fix constants in case it was empty:
  if len(dconstants) > 1 and dconstants[0] <> constants_kwd:
    dconstants_new = [constants_kwd]
    dconstants_new.extend(dconstants)
    dconstants = dconstants_new
  domain = [define_kwd,ddomain,drequirements,dtypes,dconstants,dpredicates]
  domain.extend(nactions)
  save_pddl(prefix+'-d.pddl',domain)
  
  return len(npredicates) <> 0

  
###############################
# Calling it up
###############################

def usage():
    print """
usage: %s <domain.pddl> <problem.pddl> {-c|-a}

Convert a non-deterministic conformant problem
to a conformant one.

* For an action
(:action A
 :paramerts <PARAMETERS>
 :precondition ...
 :effect (and ... (oneof a (and b c) () ) ...))

we save new pair of pddls modifyied as follows:
1. a predicate (oneof-a-<i> <PARAMETERS>) for <i> = num of oneof cases.
2. add at init (oneof oneof-a-1 ... oneof-a-n)
3. add action A as follows:
(:action A
 :paramerts <PARAMETERS>
 :precondition ...
 :effect (and ... (when oneof-a-1 a) (when oneof-a-2 (and b c)) ...))

options -c and -a leads with complex oneof at init.
For example: (oneof (and a b) (and c d))

-a (default) do the following:
Init: (oneof A B)
new action with effect:
A -> (and a b)
B -> (and c d)

-c do the following:
Init:
(oneof A B)
A iff (and a b)
B iff (and c d) 

""" % (sys.argv[0])
    sys.exit(1)

if __name__ == "__main__":
  if len(sys.argv) < 2:
    usage()

  domain_nf = sys.argv[1]
  problem_nf = sys.argv[2]
  if len(sys.argv) == 3:
    method = '-a'
  else:
    assert(sys.argv[3] == '-a' or sys.argv[3] == '-c')
    method = sys.argv[3]
  prefix='conf'

  # Just one copy in principle
  if nondet2conf(domain_nf, problem_nf, prefix, method):
    print 'Problem is non-deterministic'
  else:
    print 'Problem is NOT non-deterministic'
  print 'Done. Conformant (deterministic) PDDL saved in',(prefix+'-d.pddl'),'and',prefix+'-p.pddl'
  sys.exit(0)
