;;; Compile and load UCPOP if no defsystem available

" (c) 1990-1995 Copyright (c) University of Washington

  All rights reserved. Use of this software is permitted for non-commercial
  research purposes, and it may be copied only for that use.  All copies must
  include this copyright message.  This software is made available AS IS, and
  neither the authors nor the University of Washington make any warranty about
  the software or its performance.

  When you first acquire this software please send mail to 
  bug-ucpop@cs.washington.edu; the same address should be used for problems."



(in-package 'user)

(unless (find-package 'ucpop) (make-package 'ucpop))
(unless (find-package 'variable) (make-package 'variable))
(unless (find-package 'ptrace) (make-package 'ptrace :nicknames '(vcr pdb)))
(unless (find-package 'choice) (make-package 'choice))
(unless (find-package 'rule-net) (make-package 'rule-net))
(unless (find-package 'sc) (make-package 'sc))
(unless (find-package 'vcr-external) (make-package 'vcr-external :nicknames
						   '(vext)))

(defparameter *ucpop-dir* "/projects/ai/planners/ucpop/")

(defun LOAD-UCPOP ()
  (load (concatenate 'string *ucpop-dir* "variable"))
  (load (concatenate 'string *ucpop-dir* "struct"))
  (load (concatenate 'string *ucpop-dir* "choose"))
  (load (concatenate 'string *ucpop-dir* "rules"))
  (load (concatenate 'string *ucpop-dir* "plan-utils"))
  #+:clim-2 (progn
	      (load (concatenate 'string *ucpop-dir* "pdb/pdb-help"))
	      (load (concatenate 'string *ucpop-dir* "pdb/shell"))
	      
	      (load (concatenate 'string *ucpop-dir* "pdb/recording"))
	      (load (concatenate 'string *ucpop-dir* "pdb/vcr-interface"))
	      (load (concatenate 'string *ucpop-dir* "pdb/structs"))
	      (load (concatenate 'string *ucpop-dir* "pdb/options"))
	      
	      (load (concatenate 'string *ucpop-dir* "pdb/shell-present"))
	      (load (concatenate 'string *ucpop-dir* "pdb/shell-commands"))
	      
	      (load (concatenate 'string *ucpop-dir* "pdb/dialogs"))
	      
	      (load (concatenate 'string *ucpop-dir* "pdb/vcr-present"))
	      (load (concatenate 'string *ucpop-dir* "pdb/vcr-commands"))
	      (load (concatenate 'string *ucpop-dir* "pdb/plan-present"))
	      (load (concatenate 'string *ucpop-dir* "pdb/plan-commands"))
	      (load (concatenate 'string *ucpop-dir* "pdb/browser-present"))
	      (load (concatenate 'string *ucpop-dir* "pdb/browser-commands"))
	      
	      (load (concatenate 'string *ucpop-dir* "pdb/layout"))
	      (load (concatenate 'string *ucpop-dir* "pdb/translation"))
	      (load (concatenate 'string *ucpop-dir* "pdb/compute-tree"))
	      )
  (load (concatenate 'string *ucpop-dir* "ucpop"))
  (load (concatenate 'string *ucpop-dir* "scr"))
  (load (concatenate 'string *ucpop-dir* "zlifo"))
  (load (concatenate 'string *ucpop-dir* "interface"))
  (load (concatenate 'string *ucpop-dir* "safety"))
  (load (concatenate 'string *ucpop-dir* "controllers"))
  (load (concatenate 'string *ucpop-dir* "domains/domains"))
  (load (concatenate 'string *ucpop-dir* "domains/truckworld"))
  (load (concatenate 'string *ucpop-dir* "domains/safety-domain"))
#+:clim-2  (load (concatenate 'string *ucpop-dir* "pdb/vcr-externals"))
  )

(defmacro compile-and-load (file)
  `(progn (compile-file ,file) (load ,file)))
  
(defun COMPILE-UCPOP ()
  (compile-and-load (concatenate 'string *ucpop-dir* "variable"))
  (compile-and-load (concatenate 'string *ucpop-dir* "struct"))
  (compile-and-load (concatenate 'string *ucpop-dir* "choose"))
  (compile-and-load (concatenate 'string *ucpop-dir* "rules"))
  (compile-and-load (concatenate 'string *ucpop-dir* "plan-utils"))
  (compile-and-load (concatenate 'string *ucpop-dir* "scr"))
  #+:clim-2 (progn
	      (compile-and-load (concatenate 'string *ucpop-dir* "pdb/pdb-help"))
	      (compile-and-load (concatenate 'string *ucpop-dir* "pdb/shell"))
	      
	      (compile-and-load (concatenate 'string *ucpop-dir* "pdb/recording"))
	      (compile-and-load (concatenate 'string *ucpop-dir* "pdb/vcr-interface"))
	      (compile-and-load (concatenate 'string *ucpop-dir* "pdb/structs"))
	      (compile-and-load (concatenate 'string *ucpop-dir* "pdb/options"))
	      
	      (compile-and-load (concatenate 'string *ucpop-dir* "pdb/shell-present"))
	      (compile-and-load (concatenate 'string *ucpop-dir* "pdb/shell-commands"))
	      
	      (compile-and-load (concatenate 'string *ucpop-dir* "pdb/dialogs"))
	      
	      (compile-and-load (concatenate 'string *ucpop-dir* "pdb/vcr-present"))
	      (compile-and-load (concatenate 'string *ucpop-dir* "pdb/vcr-commands"))
	      (compile-and-load (concatenate 'string *ucpop-dir* "pdb/plan-present"))
	      (compile-and-load (concatenate 'string *ucpop-dir* "pdb/plan-commands"))
	      (compile-and-load (concatenate 'string *ucpop-dir* "pdb/browser-present"))
	      (compile-and-load (concatenate 'string *ucpop-dir* "pdb/browser-commands"))
	      
	      (compile-and-load (concatenate 'string *ucpop-dir* "pdb/layout"))
	      (compile-and-load (concatenate 'string *ucpop-dir* "pdb/translation"))
	      (compile-and-load (concatenate 'string *ucpop-dir* "pdb/compute-tree"))
	      )
  (compile-and-load (concatenate 'string *ucpop-dir* "ucpop"))
  (compile-and-load (concatenate 'string *ucpop-dir* "zlifo"))
  (compile-and-load (concatenate 'string *ucpop-dir* "interface"))
  (compile-and-load (concatenate 'string *ucpop-dir* "safety"))
  (compile-and-load (concatenate 'string *ucpop-dir* "controllers"))
  (compile-and-load (concatenate 'string *ucpop-dir* "domains/domains"))
  (compile-and-load (concatenate 'string *ucpop-dir* "domains/truckworld"))
  (compile-and-load (concatenate 'string *ucpop-dir* "domains/safety-domain"))
#+:clim-2  (compile-and-load (concatenate 'string *ucpop-dir* "pdb/vcr-externals"))
  )

