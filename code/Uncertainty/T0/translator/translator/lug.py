#! /usr/bin/env python
# -*- coding: latin-1 -*-

# Elaborated by Hector Palacios, hlp@ldc.usb.ve, hectorpal@gmail.com
# Joint work with Daniel Bryce

import sys
import re
import exceptions


#reg_lits = '\([a-zA-Z0-9\-\ ]+\)'
reg_lits = '\([a-zA-Z0-9\-\ ]+\)|\(not[ ]+\([a-zA-Z0-9\-\ ]+\)\)'
debug_nf = 'cnf-out.cnf'
def extract_condition(symb):
    init = 1 + symb.index(' ')
    i = init+1
    n = 1
    #print symb
    while n > 0:
        #print i, symb[i],'stack:',n
        if symb[i] == ')':
            n -= 1
        elif symb[i] == '(':
            n += 1
        i += 1
    return symb[init:i+1]
    
def condition2tuple(cond):
    if cond.find('(and ') == 0:
        res = []
        for lit in re.findall(reg_lits,cond):
            res.append(lit)
    else:
        res = [cond]
    #Print 'from',symb,'got',tuple(res)
    return tuple(res)

class NormalError(exceptions.Exception):
    "To replace an error that is not: KeyError when key cannot exist with problem, etc"
    def __init__(self,args):
        self.args = args

    
class LUG:
    "Lug Class. Constructor arguments: '<mapping.out>' '<cnf.out>'"

    assert_it = True

    def next_rootid(self, i):
        n = i+1
        while n < len(self.rootids) and self.rootids[n] == -1:
            self.rootid2lit[n] = None
            n = n + 1
        if n == len(self.rootids):
            return -1
        else:
            return n

    def trans(self, txt):
        num=int(txt)
        x=abs(num)
        sign = num/x
        if x > self.max_cnf_prop:
            nx = x - self.max_cnf_prop - 1 + self.n_atoms_init
        else:
            nx = int(x/2)+1
        res = sign*nx
        if self.assert_it:
            self.max_trans = max(self.max_trans,res)
            if nx not in self.trans_pr:
                #print 'Trans',x,'to',nx
                self.trans_pr.add(nx)
            #print self.max_cnf_prop, self.n_atoms_init,
            #print 'txt:',txt, x, nx, self.trans_res, self.trans_res_to_num
            if nx in self.trans_res:
                if self.trans_res_to_num[x] != nx:
                    print 'Error at trans'
                    sys.exit(1)
            else:
                self.trans_res_to_num[x] = nx
                self.trans_res.add(nx)
        return res

    def __init__(self, mapping_nf, cnf_nf, __refer_last_layer):
        "arguments: <mapping> <cnf>"

        self.cnf_nf = cnf_nf
        self.lev = 'error'
        self.p = 'error'
        self.a = 'error'
        self.e = 'error'

        # mapping
        self.propositions = []
        self.propositions2str = {}
        self.str2propositions = {}

        self.actions = []
        self.actions2str = {}
        self.str2actions = {}
        self.actions2effects = {}

        self.effects = set()
        self.effects_symb = set()
        self.effects2stract = {}
        self.stract2effects = {}
        self.conditions = set()
        
        self.prop_lev_size = -1
        self.act_lev_size = -1
        self.eff_lev_size = -1

        # This is the total # of bdds in a time step
        self.lev_size =  -1

        # This is the total number of bdds -- lev-1 time steps have propositions,
        # actions and effect, and the last layer has just propositions
        self.size = -1

        # cnf vars
        # only used during loading and translation
        # afterward uses mapping above
        self.added_cnf_vars = -1
        self.clauses = []
        self.cnf_nvars = -1
        self.cnf_nclauses = -1
        self.cnf_propositions = []
        self.cnf_propositions2str = {}
        self.cnf_propositions_t = []
        self.cnf_propositions2str_t = {}
        self.max_cnf_prop = 0
        self.rootids = 'error'
        self.rootid2lit = {}

        # debugging
        self.trans_pr = set()
        self.trans_res_to_num = {}
        self.trans_res = set()
        self.max_trans = 0

        # mapping from LUG to CF2SAT
        self.lug_init_cache = {}
        self.lug_prop_cache = {}
        self.lug_act_cache = {}
        self.lug_eff_cache = {}
        
        self.refer_last_layer = __refer_last_layer
        # init

        current_action = -1
        loading_p = False
        loading_e = False
        mapping = open(mapping_nf,'r')
        for l in mapping.readlines():
            if l.startswith('#'):
                line = l.split()
                val = int(line[2])
                if line[0] == '#timesteps':
                    self.lev = val-1 # OJO: Dan said last level == level-1, so ignore last.
                elif line[0] == '#propositions':
                    self.p = val
                    loading_p = True
                elif line[0] == '#actions':
                    self.a = val
                    loading_p = False
                elif line[0] == '#effects':
                    self.e = val
                    loading_e = True
            else:
                line = l.split(' ',1)
                if loading_p:
                    prop = int(line[0])
                    self.propositions.append(prop)
                    symb = line[1].strip().lower()
                    self.propositions2str[prop] = symb 
                    self.str2propositions[symb] = prop
                if loading_e:
                    if current_action == -1:
                        current_action = int(line[0])
                        self.actions.append(current_action)
                        symb = line[1].strip().lower()
                        self.actions2str[current_action] = symb
                        self.str2actions[symb] = current_action
                        self.actions2effects[current_action] = set()
                    elif len(l) <= 1:
                        current_action = -1
                    else:
                        if not line[1].startswith('(when ('):
                            print 'Error reading mapping.out: %s' % line[1]
                        neffect = int(line[0])
                        symbtmp = line[1].strip().lower()
                        symb = frozenset(condition2tuple(extract_condition(symbtmp)))
                        self.effects.add(neffect)
                        self.effects_symb.add(symb) 
                        self.effects2stract[neffect] = (current_action,symb)
                        self.stract2effects[(current_action,symb)] = neffect
                        self.actions2effects[current_action].add(neffect)
                        for cond in symb:
                            self.conditions.add(cond)

        if self.lev == 'error' or self.p == 'error' or self.a == 'error' or self.e == 'error':
            print 'error parsing',mapping_nf
            sys.exit(1)

##         if True:
##             for s in self.str2actions:
##                 print s
        if self.assert_it:
            if len(self.effects2stract) != len(self.stract2effects):
                print 'diff on length of effects2stract',len(self.effects2stract),'and',
                print 'stract2effects',len(self.stract2effects)
                #print self.effects2stract
                #print self.stract2effects
                sys.exit(1)                
            
        # These are the sizes for blocks of bdds at each level of the planning graph
        self.prop_lev_size = 2*self.p*((2*self.p)+1)/2
        self.act_lev_size = (self.a*(self.a+1))/2
        self.eff_lev_size = (self.e*(self.e+1))/2

        # This is the total # of bdds in a time step
        self.lev_size =  self.prop_lev_size + self.act_lev_size + self.eff_lev_size

        # This is the total number of bdds -- lev-1 time steps have propositions,
        # actions and effect, and the last layer has just propositions
        self.size = self.lev * self.lev_size + self.prop_lev_size

        cnf_f = open(cnf_nf,'r')

        for l in cnf_f:
            if l.startswith('c .suppvarnames'):
                l_suppvarnames = l
            elif l.startswith('c .cnfids'):
                l_cnfids = l
            elif l.startswith('c .nAddedCnfVar'):
                self.added_cnf_vars = int(l.split()[2])
        cnf_f.close()

        # Load cnf ids
        assert self.cnf_propositions == []
        for it in l_cnfids.split()[2:]:
            num = int(it)
            self.max_cnf_prop = max(num, self.max_cnf_prop)
            self.cnf_propositions.append(num)

        # Processing var names
        all = l_suppvarnames.split(' ',2)
        index = 0
        for lit in re.findall(reg_lits,all[2]):
            self.cnf_propositions2str[self.cnf_propositions[index]] = lit
            index += 1

        assert len(self.cnf_propositions2str) == len(self.cnf_propositions)

        #self.new_cnf_vars = self.added_cnf_vars + len(self.propositions)

    debug_rootid2lit = False
    
    def finish_init(self,n_atoms_init):
        self.n_atoms_init = n_atoms_init

        cnf_f = open(self.cnf_nf,'r')

        debug_save_clause = False
        loading_cnf = False
        nclause = 1
        debug_make_orig_cnf = True
        if debug_make_orig_cnf:
            debug_clauses = []
        # Invariant: prootid = current rootid-cnf being saved
        #            nrootid = -1 or next rootid to be saved
        prootid = 0
        for l in cnf_f:
            if loading_cnf:
                if l.startswith('c # End of Cnf'):
                    if debug_save_clause:
                        print len(self.clauses),'clauses saved'
                    loading_cnf = False
                else:
                    clause = map(self.trans,l.split()[:-1])
                    if debug_save_clause:
                        print 'Clause (',nclause,'):',clause,'from original',l.split()[:-1],
                        print ' prootid=',prootid,' nrootid=',nrootid,
                        print ' rootids[nrootid]=',self.rootids[nrootid]
                    if nrootid < 0 or self.rootids[nrootid]-1 == nclause:
                        if len(clause) > 1:
                            print 'Error, len(clause) > 1'
                            sys.exit(1)
                        self.rootid2lit[prootid] = clause[0]
                        if debug_save_clause:
                            print 'assigning root',prootid,'to lit',clause,
                            print 'new prootid =',nrootid
                        prootid = nrootid
                        nrootid = self.next_rootid(nrootid)
                    else:
                        if debug_make_orig_cnf:
                            debug_clauses.append(l.split())
                        self.clauses.append(clause)
                        if debug_save_clause:
                            print 'saving clause',nclause
                    nclause += 1
            elif l.startswith('c .rootids'):
                self.rootids = map(lambda x:int(x),l.split()[2:])
                assert self.size == len(self.rootids)
                if False: # HLP
                    print 'LUG Level size',self.lev_size,'#lev=',self.lev,'prop size=',self.prop_lev_size,
                    print 'total size =',self.size,'array size =',len(self.rootids)
                if self.assert_it:
                    p = 0
                    for lit in self.rootids:
                        if p != 0 and lit != -1 and lit <= p:
                            print 'Error: rootids is not increasing'
                            sys.exit(1)
                        p = lit
                nrootid = self.next_rootid(prootid) # First assignation of nrootid
                if debug_save_clause:
                    print 'first update to nrootid',nrootid,'from',prootid
                if self.rootids[nrootid] == 1:
                    # If first real rootid (nrootid) points to 1
                    # It means that the first slice starts in nrootid
                    # So, update again
                    # Improve: a general solution for nrootid and prootid is needed.
                    # refactor later.
                    prootid = nrootid
                    nrootid = self.next_rootid(prootid) # First assignation of nrootid
                    if debug_save_clause:
                        print 'new update to nrootid',nrootid,'from',prootid
                if self.refer_last_layer:
                    # First rootid should be stating the last layer, shift by prop_size_layer
                    # or a layer_size backward from the end
                    print 'HLP layer',prootid, '=?=', self.size - self.lev_size, '  ', self.size, self.lev_size
                    #assert prootid == self.size - self.lev_size
            elif l.startswith('p cnf'):
                loading_cnf = True
                line = l.split()
                self.cnf_nvars = int(line[2])
                self.cnf_nclauses = int(line[3])
        cnf_f.close()

        if debug_make_orig_cnf:
            debug_f = open(debug_nf,'w')
            debug_nvars = 0
            for l in debug_clauses:
                for lit in l:
                    debug_nvars = max(debug_nvars,abs(int(lit)))
            for i in range(2,self.max_cnf_prop+3,2):
                debug_clauses.append([-i,0])
            print >> debug_f, 'p cnf',debug_nvars,len(debug_clauses)
            for l in debug_clauses:
                for lit in l:
                    print >> debug_f, lit,
                print >> debug_f
            debug_f.close()
        
        if self.assert_it:
            # Verify trans
            for p in self.cnf_propositions:
                if self.propositions2str[self.trans(p)-1] != \
                   self.cnf_propositions2str[p]:
                    print 'Error on prop',p,'from cnf: ',
                    print self.propositions2str[self.trans(p)-1],'vs',
                    print self.cnf_propositions2str[p]
                    sys.exit(1)
                    
            # Verify added
            if self.added_cnf_vars + self.n_atoms_init != \
               self.max_trans:
                print 'Error on sizes cnf'
                sys.exit(1)

            # Verify rootids
            if self.debug_rootid2lit:
                print 'len of rootid2lit:',len(self.rootid2lit)
                print self.rootid2lit

        for v in self.cnf_propositions:
            nv = self.trans(v)
            self.cnf_propositions_t.append(nv)
            self.cnf_propositions2str_t[nv] = self.cnf_propositions2str[v]

    def printit(self, full=False):
        if full:
            print '#Propositions:',len(self.propositions)
            for pi in self.propositions:
                print 'Proposition', pi,':',self.propositions2str[pi]
            print '#Actions:',len(self.actions)
            for ai in self.actions:
                print 'Action',ai,':',self.actions2str[ai]
                print 'effects',self.actions2effects[ai]
            print '#Effects:',len(self.effects)
            for ei in self.effects:
                print 'Effect',ei,':',self.effects2stract[ei]

        print '#levels =',self.lev,'#props =',self.p,'#actions =',self.a,'#effects = ',self.e

        print 'prop_lev_size =',self.prop_lev_size,'act_lev_size =',
        print self.act_lev_size,'eff_lev_size = ',self.eff_lev_size
        print 'Size of array should be', self.size, 'and is',len(self.rootids)
        
    def get_n_extra(self):
        return self.added_cnf_vars

    def get_level(self):
        return self.lev

    #//These are the indicies of particular pairs of propositions -- currently
    # you'll just need the indicies where i == j
    # (meaning the label for a given proposition, action, or effect.

    # Precondition: i <= j if both pos or neg
    #               or +i, -j
    # pos_i, pos_j: '+' || '-'
    def label_p(self, pos_i, i, pos_j, j, lev, for_prop = True):
        if( ( pos_i and pos_j and i > j ) or
            ( not pos_i and not pos_j and i > j ) or
            ( not pos_i and pos_j ) ):
            print 'error label_p'
            sys.exit(1)

        # Use only the last layer
        if self.refer_last_layer:
            if for_prop:
                assert lev <= self.lev
                lev = self.lev
            else:
                assert lev < self.lev
                lev = self.lev-1

        if( pos_i and pos_j ):
            return lev * self.lev_size + i + (j*(j+1)/2)
        elif( pos_i and not pos_j ):
            return lev * self.lev_size + i + ((j+self.p)*(j+self.p+1)/2)
        elif( not pos_i and not pos_j ):
            return lev * self.lev_size + (i+self.p) + ((j+self.p)*(j+self.p+1)/2)
        else:
            print 'ERROR'
            sys.exit(1)

    def label_a(self,i,j, lev):
        if( i > j ):
            print 'error label_a'
            sys.exit(1)

        return self.label_p( True, i, True, j, lev, False) + self.prop_lev_size


    def label_e(self,i,j, lev):
        if( i > j ):
            print 'error label_e'
            sys.exit(1)

        return self.label_p( True, i, True, j, lev, False) + self.prop_lev_size + self.act_lev_size


    debug_str2str = False
    
    def lug2init(self,lug_init,level):
        v = abs(lug_init)
        if lug_init >= 0:
            s = 1
        else:
            s = -1
        key = (v,level)
        try:
            return s*self.lug_init_cache[key]
        except KeyError:
            if v in self.cnf_propositions_t:
                tmp = self.cnf_propositions2str_t[v]
                res = self.matoms.str2fluent[level][tmp]
                if self.debug_str2str:
                    print 'lug2init maps from',v,tmp,'at level',level,'to',res
            else:
                res = v
            self.lug_init_cache[key] = res
            return s*res

    debug_lug2etc_diff = False
    
    # este mapping no es para traducir literales de los lugs,
    # que van sobre las proposiciones.
    # sino para traducir de mapping.out a los literales de cf2sat.
    # por tanto no deberia usar self.cnf_propositions_t sino otra cosa (self.propositions?)
    def lug2sat(self,v,level,isProp):
        assert v >= 0
        old_level = level
        if isProp:
            cache = self.lug_prop_cache
            lugid2str = self.propositions2str
            str2fluent = self.matoms.str2fluent
            if level > self.matoms.last_time:
                level = self.matoms.last_time
                if self.debug_lug2etc_diff:
                    print 'Diff on level prop',old_level,'to',level
        else:
            cache = self.lug_act_cache
            lugid2str = self.actions2str
            str2fluent = self.matoms.str2action
            if level >= self.matoms.last_time:
                level = self.matoms.last_time-1
                if self.debug_lug2etc_diff:
                    print 'Diff on level act ',old_level,'to',level
        key = (v,level)
        try:
            return cache[key]
        except KeyError:
            try:
                tmp = lugid2str[v]
            except KeyError, inst:
                raise NormalError('var not defined in mapping of LUG: '+str(inst)+" so, don't map")
            try:
                res = str2fluent[level][tmp]
            except KeyError, inst:
                raise NormalError('var not defined in atoms of cf2sat: '+str(inst)+" so, don't map")
            if self.debug_str2str:
                if isProp:
                    print 'lug2prop',
                else:
                    print 'lug2act ',
                print 'maps from',v,tmp,'at level',level,'to',res
            cache[key] = res
            return res

    def lug2prop(self,lug_prop,level):
        return self.lug2sat(lug_prop,level,True)
    
    def lug2act(self,lug_act,level):
        return self.lug2sat(lug_act,level,False)

    debug_lug2eff = False
    def lug2eff(self,lug_eff,level):
        old_level = level
        key = (lug_eff,level)
        if level >= self.matoms.last_time:
            level = self.matoms.last_time-1
            if self.debug_lug2etc_diff:
                print 'Diff on level of eff ',old_level,'to',level

        #raise NormalError('nada')
        try:
            return self.lug_eff_cache[key]
        except KeyError:
            try:
                (act,symb) = self.effects2stract[lug_eff]
            except KeyError, inst:
                raise NormalError('effect not defined in mapping of LUG: '+str(inst)+" so, don't map")
##             print lug_eff,level,act
            acttmp = self.lug2act(act,level)
            if self.debug_lug2eff:
                print '-------------------------'
                print 'lug2eff',lug_eff,level
                print acttmp,symb
                print self.stract2effect_matoms[level]
            try:
                res = self.stract2effect_matoms[level][(acttmp,symb)]
            except KeyError, inst:
                raise NormalError('effect not defined in effects of cf2sat: '+str(inst)+" so, don't map")
            if self.debug_lug2eff or self.debug_str2str:
                print 'lug2eff maps from',lug_eff,act,symb,'at level',level,'to',res
            self.lug_eff_cache[key] = res
            return res
        

    debug_add_implications = False
    
    def add_implication_to_rootid(self,clauses,var1,var2,nrootid):
        #return # OJO: For ignoring LUG, by not adding implications
        vrootid = self.rootid2lit[nrootid]
        if vrootid is not None:
            vrootid = self.lug2init(vrootid,0)
            if self.debug_add_implications:
                print 'Add implications for (',var1,',',var2,') \imply',vrootid,
                print 'corresponding to index at rootid:',nrootid
            if var1 == var2:
                clauses.append([-var1,vrootid])
            else:
                clauses.append([-var1,-var2,vrootid])

    debug_repare_local_mappings = False
    def repare_local_mappings(self, matoms):
        """Transform mappings including conditions, so they depends on common atoms of cond-effects.
        It assumes that both propositional encodings are correct.
        i.e. not common atoms are either static, than can be assume true,
        or preconditions, than are assume true in a cond effect 
        """
        #self.stract2effect_matoms = self.matoms.stract2effect
        #return
        self.conditions_both = self.conditions.intersection(matoms.conditions)
        if self.debug_repare_local_mappings:
            print 'Conditions shared', self.conditions_both
        new = {}
        if self.debug_repare_local_mappings:
            print '==================== self.effects2stract'
        for e in self.effects2stract:
            (current_action,symb) = self.effects2stract[e]
            new[e] = (current_action,symb.intersection(self.conditions_both))
            if self.debug_repare_local_mappings:
                print 'For key',e,'from',(current_action,symb),'to',new[e]
        self.effects2stract = new
        new = {}
        if self.debug_repare_local_mappings:
            print '==================== self.stract2effects'
        for (current_action,symb) in self.stract2effects:
            nsymb = symb.intersection(self.conditions_both)
            new[(current_action, nsymb)] = self.stract2effects[(current_action,symb)]
            if self.debug_repare_local_mappings:
                print 'From key',(current_action,symb),'to key',
                print (current_action,nsymb),'for val',new[(current_action, nsymb)]
        self.stract2effects = new
        new = []
        if self.debug_repare_local_mappings:
            print '==================== self.matoms.stract2effect'
        for time in xrange(0,len(self.matoms.stract2effect)):
            for (current_action,symb) in self.matoms.stract2effect[time]:
                nsymb = symb.intersection(self.conditions_both)
                self.matoms.atoms_add(new,time,(current_action,nsymb),
                                      self.matoms.stract2effect[time][(current_action,symb)])
                if self.debug_repare_local_mappings:
                    print 'From time',time,'key',(current_action,symb),'to key',
                    print (current_action,nsymb),'for val',new[time][(current_action, nsymb)]
        self.stract2effect_matoms = new
        if self.debug_repare_local_mappings:
            print '==================== DONE'


    debug_implications = False
    debug_implications_failures = False
    def get_cnf_lev(self,level,res):
        size = 0
        old_level = level
        if level > self.lev:
            level = self.lev
        if level != old_level:
            print 'get_cnf_lev: Diff on level of eff ',old_level,'to',level

        for sign in [(True,True),(False,False)]:
            if sign[0]:
                s1 = 1
            else:
                s1 = -1
            if sign[1]:
                s2 = 1
            else:
                s2 = -1
            for p1 in xrange(0,self.p):
                for p2 in xrange(p1,self.p):
                    lab = self.label_p(sign[0],p1,sign[1],p2,level)
                    if self.debug_implications:
                        print 'generating pair of props',p1,p2,sign,level,
                        print 'with label = ',lab
                        size += 1
                    self.add_implication_to_rootid( res,
                                                    s1*self.lug2prop(p1,old_level),
                                                    s2*self.lug2prop(p2,old_level),
                                                    lab )

        for p1 in xrange(0,self.p):
            for p2 in xrange(0,self.p):
                lab = self.label_p(True,p1,False,p2,level)
                if self.debug_implications:
                    print 'generating pair of props',p1,p2,True,False,level,
                    print 'with label = ',lab
                    size += 1
                self.add_implication_to_rootid( res,
                                                self.lug2prop(p1,old_level),
                                                -self.lug2prop(p2,old_level),
                                                lab )

        if level >= self.lev:
            level = self.lev-1
        if level != old_level:
            print 'get_cnf_lev: Diff on level of act or eff',old_level,'to',level

        if level < self.lev:
            for a1 in xrange(0,self.a):
                for a2 in xrange(a1,self.a):
                    lab = self.label_a(a1,a2,level)
                    try:
                        self.add_implication_to_rootid( res,
                                                        self.lug2act(a1,old_level),
                                                        -self.lug2act(a2,old_level),
                                                        lab )
                        if self.debug_implications:
                            print 'generating pair of acts',a1,a2,level,
                            print 'with label = ',lab
                            size += 1
                    except NormalError, inst:
                        if self.debug_implications_failures:
                            print "Couldn't generate for action",a1,a2,':',inst

            for e1 in xrange(0,self.e):
                for e2 in xrange(e1,self.e):
                    lab = self.label_e(e1,e2,level)
                    try:
                        self.add_implication_to_rootid( res,
                                                        self.lug2eff(e1,old_level),
                                                        self.lug2eff(e2,old_level),
                                                        lab )
                        if self.debug_implications:
                            print 'generating pair of effects',e1,e2,level,
                            print 'with label = ',lab
                    except NormalError, inst:
                        if self.debug_implications_failures:
                            print "Couldn't generate for effect: ",e1,e2,':',inst
                        size += 1

        return size


    # take mapping from init (fluent[0])
    # to create a function from literals to outside literals
    # then traduce using mapping
    def get_cnf(self, matoms, n_atoms_init):
        self.finish_init(n_atoms_init)

        self.matoms = matoms
        self.repare_local_mappings(matoms)

        res = []
        debug_get_cnf = False
        if debug_get_cnf:
            print 'cp2s_t',self.cnf_propositions2str_t
            print self.clauses
        for clause in self.clauses:
            r = clause[:]
            for index in xrange(0,len(clause)):
                r[index] = self.lug2init(r[index],0)
            res.append(r)
        
        # Create implications
        # For each level, until level off
        #    For each pair of lits on props
        #    For each pair of positive actions
        #    For each pair of positive cond_effects

        # Mappear p,q -> p_cf2sat, q_cf2sat
        # y obtener de rootid, la variable a enlazar

        # Igualmente, a1, a2 -> a1_cf2sat, a2_cf2sat
        # Igualmente, e1, e2 -> e1_cf2sat, e2_cf2sat

        # Quizas: llenar lug2lit_cache explicitamente
        
        size = 0
        for level in range(0,self.lev+1):
            size += self.get_cnf_lev(level,res)

        if self.debug_implications:
            print 'Total number of mutexes',size
        if debug_get_cnf:
            self.printit(True)           
            print 's2i',str2initlit
            print res

        return res
        
    def get_cnf_layer(self, horiz):
        res = []
        self.get_cnf_lev(horiz,res)
        return res

if __name__ == '__main__':
    z=LUG('mapping.out','cnf.out')
    #z.printit(True)
    z.printit()
