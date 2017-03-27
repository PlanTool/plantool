import ctypes
import _ctypes

lib = ctypes.CDLL('libs/ipp.so')
argv = ['ipp', '-f', 'pddl_files/ipp_problem.pddl', '-o', 'pddl_files/ipp_domain.pddl']
lib.main(len(argv), (ctypes.c_char_p*len(argv))(*argv))

_ctypes.dlclose(lib._handle)
