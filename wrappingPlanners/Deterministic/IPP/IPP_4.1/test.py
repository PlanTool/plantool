
import ipp

problem = '/home/king-kong/my_github/plantool/GUI/pddl_files/ipp_problem.pddl'

fact = '/home/king-kong/my_github/plantool/GUI/pddl_files/ipp_domain.pddl'

ipp.oldmain(['ipp', '-o', problem, '-f', fact])
