(blockA OBJECT)
(blockB OBJECT)
(blockC OBJECT)
(blockD OBJECT)

(preconds
	(on-table blockA)
	(on blockB blockA)
	(on blockC blockB)
	(on blockD blockC)
	(clear blockD)
	(arm-empty))

(effects
	(on blockB blockA)
	(on blockC blockB)
	(on blockA blockD))


