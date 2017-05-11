;;; Missionaries and cannibals

;;; Author: Jeff Dalton

;;; A state has the form {boat_bank m_left c_left m_right c_right}.

;;; Presumably we could omit the counts for one side, since they're
;;; implicit in the counts for the other.

language lisp;

  (defun safe (m c)
    (and (safe1 m c)      ; on one bank
         (safe1 (- 3 m)   ; and the other bank
                (- 3 c))))

  (defun safe1 (m c)      ; on one bank
    (and (>= m 0)
         (>= c 0)
         (or (= m 0)
             (>= m c))))

end_language;


;;; Tasks

task mc_problem;
  nodes 1 start,
        2 finish;
  orderings 1 ---> 2;
  conditions achieve {state} = {right 0 0 3 3} at 2;
  effects {state} = {left 3 3 0 0} at 1,
          {tried ?? ?? ?? ?? ??} = false at 1;
end_task;

 
;;; Schemas that move people to left to right

schema move_2_missionaries_right;
  vars ?ml,
       ?cl,
       ?mr,
       ?cr,
       ?new_ml,
       ?new_mr;
  expands {move 2 m r};
  only_use_for_effects {state} = {right ?new_ml ?cl ?new_mr ?cr};
  ;;; want {left ?new_ml+2 ?cl ?new_mr-2 ?cr}
  ;;; hence ?ml = ?new_ml + 2,
  ;;;       ?mr = ?new_mr - 2
  conditions compute {+ ?new_ml 2} = ?ml,
             compute {- ?new_mr 2} = ?mr,
             compute {safe ?ml ?cl},
             compute {safe ?mr ?cr},
             only_use_if {tried left ?ml ?cl ?mr ?cr} = false,
             achieve {state} = {left ?ml ?cl ?mr ?cr};
  effects {tried left ?ml ?cl ?mr ?cr} = true;
end_schema;

schema move_2_cannibals_right;
  vars ?ml,
       ?cl,
       ?mr,
       ?cr,
       ?new_cl,
       ?new_cr;
  expands {move 2 c r};
  only_use_for_effects {state} = {right ?ml ?new_cl ?mr ?new_cr};
  conditions compute {+ ?new_cl 2} = ?cl,
             compute {- ?new_cr 2} = ?cr,
             compute {safe ?ml ?cl},
             compute {safe ?mr ?cr},
             only_use_if {tried left ?ml ?cl ?mr ?cr} = false,
             achieve {state} = {left ?ml ?cl ?mr ?cr};
  effects {tried left ?ml ?cl ?mr ?cr} = true;
end_schema;


schema move_1_missionary_right;
  vars ?ml,
       ?cl,
       ?mr,
       ?cr,
       ?new_ml,
       ?new_mr;
  expands {move 1 m r};
  only_use_for_effects {state} = {right ?new_ml ?cl ?new_mr ?cr};
  ;;; want {left ?new_ml+1 ?cl ?new_mr-1 ?cr}
  ;;; hence ?ml = ?new_ml + 1,
  ;;;       ?mr = ?new_mr - 1
  conditions compute {+ ?new_ml 1} = ?ml,
             compute {- ?new_mr 1} = ?mr,
             compute {safe ?ml ?cl},
             compute {safe ?mr ?cr},
             only_use_if {tried left ?ml ?cl ?mr ?cr} = false,
             achieve {state} = {left ?ml ?cl ?mr ?cr};
  effects {tried left ?ml ?cl ?mr ?cr} = true;
end_schema;

schema move_1_cannibal_right;
  vars ?ml,
       ?cl,
       ?mr,
       ?cr,
       ?new_cl,
       ?new_cr;
  expands {move 1 c r};
  only_use_for_effects {state} = {right ?ml ?new_cl ?mr ?new_cr};
  conditions compute {+ ?new_cl 1} = ?cl,
             compute {- ?new_cr 1} = ?cr,
             compute {safe ?ml ?cl},
             compute {safe ?mr ?cr},
             only_use_if {tried left ?ml ?cl ?mr ?cr} = false,
             achieve {state} = {left ?ml ?cl ?mr ?cr};
  effects {tried left ?ml ?cl ?mr ?cr} = true;
end_schema;

schema move_1_of_each_right;
  vars ?ml,
       ?cl,
       ?mr,
       ?cr,
       ?new_ml,
       ?new_cl,
       ?new_mr,
       ?new_cr;
  expands {move 1 m 1 c r};
  only_use_for_effects {state} = {right ?new_ml ?new_cl ?new_mr ?new_cr};
  conditions compute {+ ?new_ml 1} = ?ml,
             compute {+ ?new_cl 1} = ?cl,
             compute {- ?new_mr 1} = ?mr,
             compute {- ?new_cr 1} = ?cr,
             compute {safe ?ml ?cl},
             compute {safe ?mr ?cr},
             only_use_if {tried left ?ml ?cl ?mr ?cr} = false,
             achieve {state} = {left ?ml ?cl ?mr ?cr};
  effects {tried left ?ml ?cl ?mr ?cr} = true;
end_schema;


;;; Schemas that move people right to left

schema move_1_missionary_left;
  vars ?ml,
       ?cl,
       ?mr,
       ?cr,
       ?new_ml,
       ?new_mr;
  expands {move 1 m l};
  only_use_for_effects {state} = {left ?new_ml ?cl ?new_mr ?cr};
  ;;; want {right ?new_ml-1 ?cl ?new_mr+1 ?cr}
  ;;; hence ?ml = ?new_ml - 1,
  ;;;       ?mr = ?new_mr + 1
  conditions compute {- ?new_ml 1} = ?ml,
             compute {+ ?new_mr 1} = ?mr,
             compute {safe ?ml ?cl},
             compute {safe ?mr ?cr},
             only_use_if {tried right ?ml ?cl ?mr ?cr} = false,
             achieve {state} = {right ?ml ?cl ?mr ?cr};
  effects {tried right ?ml ?cl ?mr ?cr} = true;
end_schema;

schema move_1_cannibal_left;
  vars ?ml,
       ?cl,
       ?mr,
       ?cr,
       ?new_cl,
       ?new_cr;
  expands {move 1 c l};
  only_use_for_effects {state} = {left ?ml ?new_cl ?mr ?new_cr};
  conditions compute {- ?new_cl 1} = ?cl,
             compute {+ ?new_cr 1} = ?cr,
             compute {safe ?ml ?cl},
             compute {safe ?mr ?cr},
             only_use_if {tried right ?ml ?cl ?mr ?cr} = false,
             achieve {state} = {right ?ml ?cl ?mr ?cr};
  effects {tried right ?ml ?cl ?mr ?cr} = true;
end_schema;

schema move_1_of_each_left;
  vars ?ml,
       ?cl,
       ?mr,
       ?cr,
       ?new_ml,
       ?new_cl,
       ?new_mr,
       ?new_cr;
  expands {move 1 m 1 c l};
  only_use_for_effects {state} = {left ?new_ml ?new_cl ?new_mr ?new_cr};
  conditions compute {- ?new_ml 1} = ?ml,
             compute {- ?new_cl 1} = ?cl,
             compute {+ ?new_mr 1} = ?mr,
             compute {+ ?new_cr 1} = ?cr,
             compute {safe ?ml ?cl},
             compute {safe ?mr ?cr},
             only_use_if {tried right ?ml ?cl ?mr ?cr} = false,
             achieve {state} = {right ?ml ?cl ?mr ?cr};
  effects {tried right ?ml ?cl ?mr ?cr} = true;
end_schema;


schema move_2_missionaries_left;
  vars ?ml,
       ?cl,
       ?mr,
       ?cr,
       ?new_ml,
       ?new_mr;
  expands {move 2 m l};
  only_use_for_effects {state} = {left ?new_ml ?cl ?new_mr ?cr};
  conditions compute {- ?new_ml 2} = ?ml,
             compute {+ ?new_mr 2} = ?mr,
             compute {safe ?ml ?cl},
             compute {safe ?mr ?cr},
             only_use_if {tried right ?ml ?cl ?mr ?cr} = false,
             achieve {state} = {right ?ml ?cl ?mr ?cr};
  effects {tried right ?ml ?cl ?mr ?cr} = true;
end_schema;

schema move_2_cannibals_left;
  vars ?ml,
       ?cl,
       ?mr,
       ?cr,
       ?new_cl,
       ?new_cr;
  expands {move 2 c l};
  only_use_for_effects {state} = {left ?ml ?new_cl ?mr ?new_cr};
  conditions compute {- ?new_cl 2} = ?cl,
             compute {+ ?new_cr 2} = ?cr,
             compute {safe ?ml ?cl},
             compute {safe ?mr ?cr},
             only_use_if {tried right ?ml ?cl ?mr ?cr} = false,
             achieve {state} = {right ?ml ?cl ?mr ?cr};
  effects {tried right ?ml ?cl ?mr ?cr} = true;
end_schema;

;;; End
