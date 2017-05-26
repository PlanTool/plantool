import sys
#import pdb
from translate import *
#from preprocess import *
import preprocess
import search
import argparse
#translate(sys.argv[2],sys.argv[3])
parser = argparse.ArgumentParser()
#pdb.set_trace()

def run(args1,args2,args3,args4):
    #if task_filename is None:
    #if len(sys.argv) not in (4, 5):
    #    raise SystemExit("Error: Need exactly one or two command line arguments.\n"
    #                     "Usage: %s [<domain.pddl>] <task.pddl>" % sys.argv[0])

    #task_filename = sys.argv[2]
    task_filename = args2
    #if len(sys.argv) == 3:
    #    domain_filename = sys.argv[1]
    #domain_filename = sys.argv[1]
    domain_filename = args1

    if not domain_filename:
        dirname, basename = os.path.split(task_filename)
        domain_filename = os.path.join(dirname, "domain.pddl")
        if not os.path.exists(domain_filename) and re.match(r"p[0-9][0-9]\b", basename):
            domain_filename = os.path.join(dirname, basename[:4] + "domain.pddl")
        if not os.path.exists(domain_filename) and re.match(r"p[0-9][0-9]\b", basename):
            domain_filename = os.path.join(dirname, basename[:3] + "-domain.pddl")
        if not os.path.exists(domain_filename) and re.match(r"p[0-9][0-9]\b", basename):
            domain_filename = os.path.join(dirname, "domain_" + basename)
        if not os.path.exists(domain_filename) and basename.endswith("-problem.pddl"):
            domain_filename = os.path.join(dirname, basename[:-13] + "-domain.pddl")
        if not os.path.exists(domain_filename):
            raise SystemExit("Error: Could not find domain file using "
                             "automatic naming rules.")

    #timer = timers.Timer()
    #with timers.timing("Parsing"):
    #    task = pddl.open(task_filename, domain_filename)
    task = pddl.open(task_filename, domain_filename)
    # EXPERIMENTAL!
    # import psyco
    # psyco.full()

    sas_task = pddl_to_sas(task)
    #with timers.timing("Writing output"):
    #    sas_task.output(file("output.sas", "w"))
    sas_task.output(file("output.sas", "w"))
    print "Done!  " 
    #argv=[sys.argv[3],sys.argv[4]]
    argv=[args3,args4]
    #argv=[argv[3],argv[4]]
    preprocess.oldmain()

    search.oldmain(argv)
    


