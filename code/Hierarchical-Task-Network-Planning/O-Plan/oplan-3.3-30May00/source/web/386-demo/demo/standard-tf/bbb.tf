;;; Blue Badge in Box (BBB) Problem
;;; Defined by Drew McDermott at AAAI Spring Symposium, 1992
;;; BAT 29-Jul-92
;;;
;;; There are two operators putin(container,object) and spray(container,colour)
;;; where a side effect of spraying a container colours the current contents
;;; of the container (if any).  The task is to get a blue badge in a box and
;;; carry out the action of spraying the box red during the plan.
;;;

;;;
;;; defaults give conditions and effects at same end of a node
;;; so they are instantaneous
;;;

defaults condition_node_end = begin_of,
         condition_contributor_node_end = begin_of,
         effect_node_end = begin_of;

types container = (box),
      object = (badge),
      colour = (red blue);
      
task blue_badge_in_red_box;
  nodes 1 start,
        2 finish,
        3 action {spray box red};
  orderings 1 ---> 3, 3 ---> 2;
  conditions achieve {in box} = badge at 2,
             achieve {colour badge} = blue at 2;
;;;          supervised {colour box} = red at 2 from 3;
  effects {colour badge} = blue at 1,
          {in box} = nothing at 1;
end_task;

task red_badge_in_red_box;
  nodes 1 start,
        2 finish,
        3 action {spray box red};
  orderings 1 ---> 3, 3 ---> 2;
  conditions achieve {in box} = badge at 2,
             achieve {colour badge} = red at 2;
;;;          supervised {colour box} = red at 2 from 3;
  effects {colour badge} = blue at 1,
          {in box} = nothing at 1;
end_task;

schema putin;
  vars ?container = ?{type container},
       ?object = ?{type object};
  expands {putin ?container ?object};
  only_use_for_effects {in ?container} = ?object;
end_schema;

;;; next two schemas implement
;;;    if condition {in ?container} = ?x (where ?x /= nothing)
;;;    then extra effect {colour ?x} = ?colour
;;; "if" must be defined "when", could be "at time of schema choice" or
;;; "in final plan" or "if it can be established that"  This last one is
;;; the most general and needs an achieve condition type.

schema spray_only_container;
  vars ?container = ?{type container},
       ?colour = ?{type colour};
  expands {spray ?container ?colour};
  only_use_for_effects {colour ?container} = ?colour;
  conditions achieve {in ?container} = nothing;
end_schema;

schema spray_container_and_contents;
  vars ?container = ?{type container},
       ?colour = ?{type colour},
       ?object = ?{type object};
  expands {spray ?container ?colour};
  only_use_for_effects {colour ?container} = ?colour,
                       {colour ?object} = ?colour;
  conditions achieve {in ?container} = ?object;
end_schema;




