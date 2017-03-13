;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Software System Development Project Management
;;;
;;; Based on the Alvey PLANIT Club Price Waterhouse Demonstrator
;;; with the permission of Price Waterhouse. 15-May-85
;;;
;;; Nonlin TF    : Glen Hopkinson, Systems Designers and Austin Tate, AIAI
;;; O-Plan2 port : Brian Drabble, AIAI. 10-Mar-93
;;; Updated      : Austin Tate, AIAI. 24-Jun-93
;;; Errors fixed : Jeff Dalton, AIAI, 03-Jul-95
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Types required for the domain

types module = (MIM NPL PL CA MR time billing disbursements user_system),
      programming_team = (team_A team_B),
      installation_engineer = (engineer_A engineer_B);

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main task schema of the domain

task Information_System;
  nodes 1 start,
        2 finish,
        3 action {build_common_data_dictionary_system},
    	4 action {master_information_maintenance_system},
    	5 action {nominal_and_private_ledger_system},
    	6 action {time_system},
    	7 action {client_accounting_system},
    	8 action {management_reports_system},
    	9 action {user_system};
  orderings 1 ---> 3, 1 ---> 4, 1 ---> 5, 1 ---> 6, 1 ---> 7, 1 ---> 8, 
	    1 ---> 9, 
            3 ---> 2, 4 ---> 2, 5 ---> 2, 6 ---> 2, 7 ---> 2, 8 ---> 2, 
	    9 ---> 2;
  effects {status programming_team team_A} = unallocated at 1,
          {status programming_team team_B} = unallocated at 1,
          {status programming_team team_C} = unallocated at 1,
	  {status installation_engineer engineer_A} = unallocated at 1,
	  {status installation_engineer engineer_B} = unallocated at 1,
	  {information_system_completed} at 2;
  time_windows  0~09:00 at begin_of 1,   ;;; start day 0 at 09:00
               30~09:00 at end_of 2;     ;;; finish by day 10 at 09:00
end_task;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Schemas to support the main task

schema Master_Information_Maintenance_System;
  vars ?team = ?{type programming_team};
  expands {master_information_maintenance_system}; 
  nodes 1 action {specify_program MIM},
        2 action {carry_out_programming_and_unit_tests_part_1 MIM},
        3 action {carry_out_programming_and_unit_tests_part_2 MIM},
        4 action {integration_tests MIM},
        5 action {carry_out_subsystem_acceptance_and_tests MIM};
  orderings 1 ---> 2, 2 ---> 3, 2 ---> 4,  3 ---> 5;  
  conditions 
    unsupervised {status programming_team ?team} = unallocated at 1,
    supervised 
     {status programming_team ?team} = MIM at end_of 5 from begin_of 1,
    supervised {program_specification MIM} = completed at 2 from 1,
    supervised {programming_and_unit_tests_part_1 MIM} = completed at 4 from 2,
    supervised {programming_and_unit_tests_part_2 MIM} = completed at 5 from 3,
    unsupervised {common_data_dictionary_system} = completed at 2;
  effects {status programming_team ?team} = MIM at begin_of 1,
          {status programming_team ?team} = unallocated at 5,
          {master_information_maintenance_system} = completed at 5;
  time_windows duration self = 10 days .. 10 days;
end_schema;


schema Nominal_and_Private_Ledger_System;
  vars ?team = ?{type programming_team};
  expands {nominal_and_private_ledger_system};
  nodes 1 action {specify_program NPL},
        2 action {carry_out_programming_and_unit_tests_part_1 NPL},
        3 action {carry_out_programming_and_unit_tests_part_2 NPL},
        4 action {integration_tests NPL},
        5 action {carry_out_subsystem_acceptance_and_tests NPL};
  orderings 1 ---> 2,   2 ---> 3,   2 ---> 4,   3 ---> 5;
  conditions 
    unsupervised {status programming_team ?team} = unallocated at 1,
    supervised 
     {status programming_team ?team} = NPL at end_of 5 from begin_of 1,
    supervised {program_specification NPL} = completed at 2 from 1,
    supervised {programming_and_unit_tests_part_1 NPL} = completed at 4 from 2,
    supervised {programming_and_unit_tests_part_2 NPL} = completed at 5 from 3;
  effects {status programming_team ?team} = NPL at begin_of 1,
          {status programming_team ?team} = unallocated at 5,
	  {nominal_and_private_ledger_system} = completed at 5;
  time_windows duration self = 10 days .. 10 days;
end_schema;


schema Time_System;
  vars ?team = ?{type programming_team};
  expands {time_system};
  nodes 1 action {specify_program time},
        2 action {carry_out_programming_and_unit_tests_part_1 time},
        3 action {carry_out_programming_and_unit_tests_part_2 time},
        4 action {integration_tests time},
        5 action {carry_out_subsystem_acceptance_and_tests time};
  orderings 1 ---> 2,   2 ---> 3,   2 ---> 4,   3 ---> 5;
  conditions 
   unsupervised {status programming_team ?team} = unallocated at 1,
   supervised 
     {status programming_team ?team} = time at end_of 5 from begin_of 1,
   supervised {program_specification time} = completed at 2 from 1,
   supervised {programming_and_unit_tests_part_1 time} = completed at 4 from 2,
   supervised {programming_and_unit_tests_part_2 time} = completed at 5 from 3;
  effects {status programming_team ?team} = time at begin_of 1,
          {status programming_team ?team} = unallocated at 5,
          {time_system} = completed at 5;
  time_windows duration self = 10 days .. 10 days;
end_schema;


schema Billing_System;
  vars ?team = ?{type programming_team};
  expands {billing_system};
  nodes 1 action {specify_program billing},
        2 action {carry_out_programming_and_unit_tests_part_1 billing},
        3 action {carry_out_programming_and_unit_tests_part_2 billing},
        4 action {integration_tests billing},
        5 action {carry_out_subsystem_acceptance_and_tests billing};
  orderings 1 ---> 2,   2 ---> 3,   2 ---> 4,   3 ---> 5; 
  conditions 
   unsupervised {status programming_team ?team} = unallocated at 1,
   supervised 
     {status programming_team ?team} = billing at end_of 5 from begin_of 1,
   supervised {program_specification billing} = completed at 2 from 1,
   supervised
     {programming_and_unit_tests_part_1 billing} = completed at 4 from 2,
   supervised
     {programming_and_unit_tests_part_2 biling} = completed at 5 from 3,
   unsupervised {common_data_dictionary_system} = completed at 1;
  effects {status programming_team ?team} = billing at begin_of 1,
          {status programming_team ?team} = unallocated at 5,
          {billing_system} = completed at 5;
  time_windows duration self = 10 days .. 10 days;
end_schema;


schema Purchase_Ledger_System;
  vars ?team = ?{type programming_team};
  expands {purchase_ledger_system};
  nodes 1 action {specify_program PL},
        2 action {carry_out_programming_and_unit_tests_part_1 PL},
        3 action {carry_out_programming_and_unit_tests_part_2 PL},
        4 action {integration_tests PL},
        5 action {carry_out_subsystem_acceptance_and_tests PL};
  orderings 1 ---> 2,   2 ---> 3,   2 ---> 4,   3 ---> 5;
  conditions 
   unsupervised {status programming_team ?team} = unallocated at 1,
   supervised 
    {status programming_team ?team} = PL at end_of 5 from begin_of 1,
   supervised {program_specification PL} = completed at 2 from 1,
   supervised {programming_and_unit_tests_part_1 PL } = completed at 4 from 2,
   supervised {programming_and_unit_tests_part_2 PL} = completed at 5 from 3,
             unsupervised {common_data_dictionary_system} = completed at 1;
  effects {status programming_team ?team} = PL at begin_of 1,
          {status programming_team ?team} = unallocated at 5,
	  {purchase_ledger_system} =  completed at 5;
  time_windows duration self = 10 days .. 10 days;
end_schema;


schema Disbursements_System;
  vars ?team = ?{type programming_team};
  expands {disbursements_system};
  nodes 1 action {specify_program disbursements},
        2 action {carry_out_programming_and_unit_tests_part_1 disbursements},
        3 action {carry_out_programming_and_unit_tests_part_2 disbursements},
        4 action {integration_tests disbursements},
        5 action {carry_out_subsystem_acceptance_and_tests disbursements};
  orderings 1 ---> 2,   2 ---> 3,   2 ---> 4,   3 ---> 5;   
  conditions 
    unsupervised {status programming_team ?team} = unallocated at 1,
    supervised {status programming_team ?team} = disbursements
           at end_of 5 from begin_of 1,
    supervised {program_specification disbursements} = completed at 2 from 1,
    supervised {programming_and_unit_tests_part_1 disbursements} = completed
           at 4 from 2,
    supervised {programming_and_unit_tests_part_2 disbursements} = completed
           at 5 from 3,
    unsupervised {common_data_dictionary_system} = completed at 1; 
  effects {status programming_team ?team} = disbursements at begin_of 1,
          {status programming_team ?team} = unallocated at 5,
	  {disbursements_system} = completed at 5;
  time_windows duration self = 10 days .. 10 days;
end_schema;


schema Client_Accounting;
  vars ?team = ?{type programming_team};
  expands {client_accounting_system};
  nodes 1 action {specify_program CA},
        2 action {carry_out_programming_and_unit_tests_part_1 CA},
        3 action {carry_out_programming_and_unit_tests_part_2 CA},
        4 action {integration_tests CA},
        5 action {carry_out_subsystem_acceptance_and_tests CA};
  orderings 1 ---> 2,   2 ---> 3,   2 ---> 4,   3 ---> 5;
  conditions 
    unsupervised {status programming_team ?team} = unallocated at 1,
    supervised 
     {status programming_team ?team} = CA at end_of 5 from begin_of 1,
    supervised {program_specification CA} = completed at 2 from 1,
    supervised {programming_and_unit_tests_part_1 CA} = completed at 4 from 2,
    supervised {programming_and_unit_tests_part_2 CA} = completed at 5 from 3,
    unsupervised {common_data_dictionary_system} = completed at 1; 
  effects {status programming_team ?team} = CA at begin_of 1,
          {status programming_team ?team} = unallocated at 5,
	  {client_accounting_system} = completed at 5;
  time_windows duration self = 10 days .. 10 days;
end_schema;


schema Management_Reports_System;
  vars ?team = ?{type programming_team};
  expands {management_reports_system};
  nodes 1 action {specify_program MR},
        2 action {carry_out_programming_and_unit_tests_part_1 MR},
        3 action {carry_out_programming_and_unit_tests_part_2 MR},
        4 action {integration_tests MR},
        5 action {carry_out_subsystem_acceptance_and_tests MR};
  orderings 1 ---> 2,   2 ---> 3,   2 ---> 4,   3 ---> 5;
  conditions 
    unsupervised {status programming_team ?team} = unallocated at 1,
    supervised 
     {status programming_team ?team} = MR at end_of 5 from begin_of 1,
    supervised {program_specification MR} = completed at 2 from 1,
    supervised {programming_and_unit_tests_part_1 MR} = completed at 4 from 2,
    supervised {programming_and_unit_tests_part_2 MR} = completed at 5 from 3,
    unsupervised {common_data_dictionary_system} = completed at 1; 
  effects {status programming_team ?team} = MR at begin_of 1,
          {status programming_team ?team} = unallocated at 5,
	  {management_reports_system} = completed;
  time_windows duration self = 10 days .. 10 days;
end_schema;

schema User_System;
  vars ?team = ?{type programming_team};
  expands {user_system};
  nodes 1 action {perform_user_and_clerical_procedures},
        2 action {assemble_training_material},
        3 action {develop_screen_menu_instructions_for_terminal_operators},
        4 action {document_computer_operations_instructions_and_JCL};
  conditions 
    unsupervised {status programming_team ?team} = unallocated at 1,
    supervised 
     {status programming_team ?team} = user_system at end_of 4 from begin_of 1,
    supervised {user_and_clerical_procedures} = completed at 2 from 1,
    supervised {training_material} = completed at 3 from 2,
    supervised {screen_menu_instructions_for_terminal_operators}
                = completed at 4 from 3;
  effects {status programming_team ?team} = user_system at begin_of 1,
          {status programming_team ?team} = unallocated at 4,
	  {user_system} = completed;
  time_windows duration self = 9 days .. 9 days;
end_schema;


schema Hardware_System;
  vars ?engineer = ?{type installation_engineer};
  expands {hardware_system};
  nodes 1 action {prepare_computer_room},
        2 action {install_computer};
  orderings 1 ---> 2;
  conditions 
     unsupervised {status installation_engineer ?engineer} = hardware at 1,
     supervised {preparation_computer_room} = completed at 2 from 1;
  effects  {status installation_engineer ?engineer} = hardware at begin_of 1,
           {status installation_engineer ?engineer} = unallocated at 2;
  time_windows duration self = 0 days .. 10 days;
end_schema;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Primitive Schemas of the domain

schema data_dictionary;
   expands {build_common_data_dictionary_system};
   only_use_for_effects {common_data_dictionary_system} = completed;
   time_windows duration self = 2 days;
end_schema;

schema program_specification;
   vars ?x = ?{type module};
   expands {specify_program ?x};
   only_use_for_effects {program_specification ?x} = completed;
   time_windows duration self = 1 days;
end_schema;

schema program_tests_1;
   vars ?x = ?{type module};
   expands {carry_out_programming_and_unit_tests_part_1 ?x};
   only_use_for_effects {programming_and_unit_tests_part_1 ?x} = completed;
   time_windows duration self = 2 days;
end_schema;

schema program_tests_2;
   vars ?x = ?{type module};
   expands {carry_out_programming_and_unit_tests_part_2 ?x};
   only_use_for_effects {programming_and_unit_tests_part_2 ?x} = completed;
   time_windows duration self = 2 days;
end_schema;

schema integrated_tests;
   vars ?x = ?{type module};
   expands {integration_tests ?x};
   only_use_for_effects {integrated_tests ?x} = completed;
   time_windows duration self = 2 days;
end_schema;

schema subsystem_acceptance;
   vars ?x = ?{type module};
   expands {carry_out_subsystem_acceptance_and_tests ?x};
   only_use_for_effects {subsystem_acceptance_tests ?x} = completed;
   time_windows duration self = 3 days;
end_schema;

schema clerical_procedures;
   expands {perform_user_and_clerical_procedures};
   only_use_for_effects {user_and_clerical_procedures} = completed;
   time_windows duration self = 1 days;
end_schema;

schema training;
   expands {assemble_training_material};
   only_use_for_effects {training_material} = completed;
   time_windows duration self = 3 days;
end_schema;

schema screen_menus;
   expands {develop_screen_menu_instructions_for_terminal_operators};
   only_use_for_effects
       {screen_menu_instructions_for_terminal_operators} = completed;
   time_windows duration self = 3 days;
end_schema;

schema document_operations;
   expands {document_computer_operations_instructions_and_JCL};
   only_use_for_effects 
      {documentation_for_computer_operations_instructions_and_JCL} = completed;
   time_windows duration self = 2 days;
end_schema;

schema prepare_computer_room;
   expands {prepare_computer_room};
   only_use_for_effects {preparation_computer_room} = completed;
   time_windows duration self = 1 days;
end_schema;

schema install_computer;
   expands {install_computer};
   only_use_for_effects {installation_of_computer} = completed;
   time_windows duration self = 1 days;
end_schema;

schema system_test;
   expands {PW_system_testing};
   only_use_for_effects {PW_system_tests} = completed;
   time_windows duration self = 1 days;
end_schema;

schema user_training;
   expands {present_user_training};
   only_use_for_effects {user_training} = completed;
   time_windows duration self = 1 days;
end_schema;

schema final_tests;
   expands {perform_final_acceptance_tests};
   only_use_for_effects {final_acceptance_tests} = completed;
   time_windows duration self = 1 days;
end_schema;

schema specify_conversion_procedures;
   expands {specify_conversion_procedures};
   only_use_for_effects 
		{specification_of_conversion_procedures} = completed;
   time_windows duration self = 1 days;
end_schema;

schema convert_data_files;
   expands {convert_data_files};
   only_use_for_effects {conversion_of_data_files} = completed;
   time_windows duration self = 1 days;
end_schema;

schema parallel_running;
   expands {run_in_parrallel};
   only_use_for_effects {parallel_running} = completed;
   time_windows duration self = 1 days;
end_schema;

schema system_live;
   expands {make_system_live};
   only_use_for_effects {system_live} = completed;
   time_windows duration self = 1 days;
end_schema;

