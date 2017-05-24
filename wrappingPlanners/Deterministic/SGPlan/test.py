import sgplan

factfile = 'fixit_ops.pddl'

operatorfile = 'domain_ops.pddl'

sgplan.oldmain(['sgplan', '-f', factfile, '-o', operatorfile, '-out', 'result.soln2'])

#, '-f', factfile, '-o', operatorfile

print('!!')
