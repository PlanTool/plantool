
import graphplan

operatorfile = 'fixit_ops'

factfile = 'fixit_facts1'

graphplan.run(['graphplan', '-f', factfile, '-o', operatorfile, '-i', '0', '-M', '256', '-t', '100', '-O', 'ILSM', '-r', 'test.log'])

print('!!')
