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

import subprocess

def which_problem_pddl(nf):
    for l in file(nf):
        if ':domain' in l:
            return True
    return False

def get_pddls(tar_nf):
    if not (tar_nf.endswith('.tar') or
            tar_nf.endswith('.tgz') or
            tar_nf.endswith('.tbz')):
        return [False,0,0]

    if tar_nf.endswith('.tar'):
        cmd = 'tar xvf '+tar_nf
    elif tar_nf.endswith('.tgz'):
        cmd = 'tar xzvf '+tar_nf
    elif tar_nf.endswith('.tbz'):
        cmd = 'tar xjvf '+tar_nf
    else:
        print 'Error: expecting compressed file but unknown extension:', tar_nf
        usage()
        
    tar=subprocess.Popen(cmd, shell = True, stdout=subprocess.PIPE)
    files = []
    for l in tar.stdout.readlines():
        files.append(l.split()[0])
    res = tar.wait()
    if(res < 0):
        print 'Error calling tar: %d ' % res
        sys.exit(1)
    pddl_files = []
    for f in files:
        if 'pddl' in f or 'PDDL' in f:
            pddl_files.append(f)
    # It allows to included something else in the compressed file
    # so it can overwrites cf2cs translator, etc
    if len(pddl_files) != 2:
        print 'Error: tar file should contain exactly two files'
        sys.exit(1)
    if which_problem_pddl(pddl_files[0]):
        problem_nf = pddl_files[0]
        domain_nf = pddl_files[1]
    else:
        domain_nf = pddl_files[0]
        problem_nf = pddl_files[1]
    return [True,domain_nf,problem_nf]
