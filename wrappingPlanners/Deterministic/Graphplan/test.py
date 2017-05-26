
import graphplan

operatorfile = 'fixit_ops.pddl'

factfile = 'fixit_facts1.pddl'

graphplan.run(['graphplan', '-f', factfile, '-o', operatorfile, '-i', '0', '-M', '256', '-t', '100', '-O', 'ILSM', '-r', 'test.log'])

print('!!')
