;;; File: gpdt.tf
;;; Contains: TF for the Pacifica COA matrix demo
;;; Created: John Levine, June 1997
;;; Updated: Mon Sep 29 00:48:42 1997 by Jeff Dalton
;;; Copyright: (c) 1997, AIAI, University of Edinburgh

types city               = (Abyss Barnacle Calypso Delta),
      fast_vehicle       = (GT1 GT2),
      slow_vehicle       = (GT3 GT4),
      fleet_of_trucks    = (GT5),
      helicopter         = (H1 H2),
      repair_team        = (RT1 RT2),
      repair_equipment   = (RE1 RE2),
      medical_team       = (MT1),
      medical_equipment  = (ME1),
      evacuation_team    = (ET1),
      bomb_squad         = (BS1),
      building_team      = (BT1),
      building_equipment = (BE1),
      commander          = (CDR1),
      engineer           = (ENG1),
      house_kit          = (HK1 HK2 HK3 HK4),
      medical_supplies   = (MS1 MS2 MS3 MS4),
      emergency_food     = (EF1 EF2 EF3 EF4),
      flying_weather     = (clear rain);

;;; Level 1 schemas.

;;; Evacuation operations.

schema evacuate_injured_h;
  vars ?wt = ?{type flying_weather},
       ?v1 = ?{type helicopter},
       ?v2 = ?{type helicopter},
       ?mt = ?{type medical_team},
       ?me = ?{type medical_equipment},
       ?at = ?{type city};
  expands {evacuate_injured ?at};
  nodes
    1 action {transport_by_air ?v1 ?mt ?at},
    2 action {transport_by_air ?v2 ?me ?at},
    3 action {deal_with_injured ?mt ?me ?at},
    4 action {load_injured ?mt ?v1 ?at},
    5 action {move_injured_by_air ?v1 ?at Delta};
  orderings
    1 ---> 3, 2 ---> 3, 3 ---> 4, 4 ---> 5;
  conditions
    only_use_if {weather} = ?wt;
end_schema;

schema evacuate_injured_gt;
  vars ?v1 = ?{type slow_vehicle},
       ?v2 = ?{type slow_vehicle},
       ?mt = ?{type medical_team},
       ?me = ?{type medical_equipment},
       ?at = ?{type city};
  expands {evacuate_injured ?at};
  nodes
    1 action {slow_transport ?v1 ?mt ?at},
    2 action {slow_transport ?v2 ?me ?at},
    3 action {deal_with_injured ?mt ?me ?at},
    4 action {load_injured ?mt ?v1 ?at},
    5 action {move_injured_by_road ?v1 ?at Delta};
  orderings
    1 ---> 3, 2 ---> 3, 3 ---> 4, 4 ---> 5;
end_schema;

schema evacuate_population;
  vars ?v1 = ?{type slow_vehicle},
       ?v2 = ?{type fleet_of_trucks},
       ?et = ?{type evacuation_team},
       ?at = ?{type city};
  expands {evacuate_population ?at};
  nodes
    1 action {slow_transport ?v1 ?et ?at},
    2 action {send_fleet_of_trucks ?v2 ?at},
    3 action {load_population ?et ?v2 ?at},
    4 action {move_population ?v2 ?at Delta};
  orderings
    1 ---> 3, 2 ---> 3, 3 ---> 4;
end_schema;

schema evacuate_with_medical_team_h;
  vars ?wt = ?{type flying_weather},
       ?v1 = ?{type helicopter},
       ?v2 = ?{type helicopter},
       ?v3 = ?{type fleet_of_trucks},
       ?mt = ?{type medical_team},
       ?me = ?{type medical_equipment},
       ?at = ?{type city};
  expands {evacuate_with_medical_team ?at};
  nodes 
    1 action {transport_by_air ?v1 ?mt ?at},
    2 action {transport_by_air ?v2 ?me ?at},
    3 action {send_fleet_of_trucks ?v3 ?at},
    4 action {deal_with_injured ?mt ?me ?at},
    5 action {load_injured ?mt ?v1 ?at},
    6 action {load_population ?mt ?v3 ?at},
    7 action {move_injured_by_air ?v1 ?at Delta},
    8 action {move_population ?v3 ?at Delta};
  orderings
    1 ---> 4, 2 ---> 4, 3 ---> 6,
    4 ---> 5, 5 ---> 6,
    5 ---> 7, 6 ---> 8;
  conditions
    only_use_if {weather} = ?wt;
end_schema;

schema evacuate_with_medical_team_gt;
  vars ?v1 = ?{type slow_vehicle},
       ?v2 = ?{type slow_vehicle},
       ?v3 = ?{type fleet_of_trucks},
       ?mt = ?{type medical_team},
       ?me = ?{type medical_equipment},
       ?at = ?{type city};
  expands {evacuate_with_medical_team ?at};
  nodes 
    1 action {slow_transport ?v1 ?mt ?at},
    2 action {slow_transport ?v2 ?me ?at},
    3 action {send_fleet_of_trucks ?v3 ?at},
    4 action {deal_with_injured ?mt ?me ?at},
    5 action {load_injured ?mt ?v1 ?at},
    6 action {load_population ?mt ?v3 ?at},
    7 action {move_injured_by_road ?v1 ?at Delta},
    8 action {move_population ?v3 ?at Delta};
  orderings
    1 ---> 4, 2 ---> 4, 3 ---> 6,
    4 ---> 5, 5 ---> 6,
    5 ---> 7, 6 ---> 8;
end_schema;

;;; Supply operations.

schema send_medical_supplies;
  vars ?fv = ?{type fast_vehicle},
       ?ms = ?{type medical_supplies},
       ?at = ?{type city};
  expands {send_medical_supplies ?at};
  nodes
    1 action {fast_transport ?fv ?ms ?at};
  conditions
    unsupervised {available ?ms} = true;
  effects
    {available ?ms} = false at begin_of self;
;;;  use ?ms across self;
end_schema;

schema send_emergency_food;
  vars ?fv = ?{type fast_vehicle},
       ?ef = ?{type emergency_food},
       ?at = ?{type city};
  expands {send_emergency_food ?at};
  nodes
    1 action {fast_transport ?fv ?ef ?at};
  conditions
    unsupervised {available ?ef} = true;
  effects
    {available ?ef} = false at begin_of self;
;;;  use ?ef across self;
end_schema;

schema send_medical_team_h;
  vars ?wt = ?{type flying_weather},
       ?v1 = ?{type helicopter},
       ?v2 = ?{type helicopter},
       ?mt = ?{type medical_team},
       ?me = ?{type medical_equipment},
       ?at = ?{type city};
  expands {send_medical_team ?at};
  nodes
    1 action {transport_by_air ?v1 ?mt ?at},
    2 action {transport_by_air ?v2 ?me ?at},
    3 action {deal_with_injured ?mt ?me ?at};
  orderings
    1 ---> 3, 2 ---> 3;
  conditions
    only_use_if {weather} = ?wt;
end_schema;

schema send_medical_team_gt;
  vars ?v1 = ?{type slow_vehicle},
       ?v2 = ?{type slow_vehicle},
       ?mt = ?{type medical_team},
       ?me = ?{type medical_equipment},
       ?at = ?{type city};
  expands {send_medical_team ?at};
  nodes
    1 action {slow_transport ?v1 ?mt ?at},
    2 action {slow_transport ?v2 ?me ?at},
    3 action {deal_with_injured ?mt ?me ?at};
  orderings
    1 ---> 3, 2 ---> 3;
end_schema;

;;; Repair operations.

schema repair_gas_leak;
  vars ?v1 = ?{type slow_vehicle},
       ?v2 = ?{type slow_vehicle},
       ?tm = ?{type repair_team},
       ?eq = ?{type repair_equipment},
       ?at = ?{type city};
  expands {repair_gas_leak ?at};
  nodes
    1 action {slow_transport ?v1 ?tm ?at},
    2 action {slow_transport ?v2 ?eq ?at},
    3 action {fix_gas_leak ?tm ?eq ?at},
    4 action {restart_power_station ?tm ?at};
  orderings
    1 ---> 3, 2 ---> 3, 3 ---> 4;
end_schema;

schema defuse_terrorist_bomb;
  vars ?sv = ?{type slow_vehicle},
       ?bs = ?{type bomb_squad},
       ?at = ?{type city};
  expands {defuse_terrorist_bomb ?at};
  nodes
    1 action {slow_transport ?sv ?bs ?at},
    2 action {defuse_bomb ?bs ?at};
  orderings
    1 ---> 2;
end_schema;

schema build_emergency_housing;
  vars ?v1 = ?{type slow_vehicle},
       ?v2 = ?{type slow_vehicle},
       ?v3 = ?{type fleet_of_trucks},
       ?bt = ?{type building_team},
       ?be = ?{type building_equipment},
       ?hk = ?{type house_kit},
       ?at = ?{type city};
  expands {build_emergency_housing ?at};
  nodes
    1 action {slow_transport ?v1 ?bt ?at},
    2 action {slow_transport ?v2 ?be ?at},
    3 action {transport_in_trucks ?v3 ?hk ?at},
    4 action {build_houses ?bt ?be ?hk ?at};
  orderings
    1 ---> 4, 2 ---> 4, 3 ---> 4;
  conditions
    unsupervised {available ?hk} = true;
  effects
    {available ?hk} = false at begin_of self;
end_schema;

schema repair_power_station_turbine;
  vars ?v1 = ?{type slow_vehicle},
       ?v2 = ?{type slow_vehicle},
       ?tm = ?{type repair_team},
       ?eq = ?{type repair_equipment},
       ?at = ?{type city};
  expands {repair_power_station_turbine ?at};
  nodes
    1 action {slow_transport ?v1 ?tm ?at},
    2 action {slow_transport ?v2 ?eq ?at},
    3 action {fix_turbine ?tm ?eq ?at},
    4 action {restart_power_station ?tm ?at};
  orderings
    1 ---> 3, 2 ---> 3, 3 ---> 4;
end_schema;

;;; Emergency operations.

schema provide_immediate_assistance;
  vars ?v = ?{type fast_vehicle},
       ?c = ?{type commander},
       ?a = ?{type city};
  expands {provide_immediate_assistance ?a};
  nodes
    1 action {fast_transport ?v ?c ?a},
    2 action {deal_with_crisis ?c ?a};
  orderings
    1 ---> 2;
end_schema;

schema shut_down_power_station;
  vars ?v = ?{type fast_vehicle},
       ?e = ?{type engineer},
       ?a = ?{type city};
  expands {shut_down_power_station ?a};
  nodes
    1 action {fast_transport ?v ?e ?a},
    2 action {shut_down_power ?e ?a};
  orderings
    1 ---> 2;
end_schema;

;;; Level 2 schemas.

schema fast_transport;
  vars ?v = ?{type fast_vehicle},
       ?i = ??,
       ?a = ?{type city};
  expands {fast_transport ?v ?i ?a};
  nodes
    1 action {fuel_vehicle ?v},
    2 action {check_vehicle ?v},
    3 action {brief_driver ?v},
    4 action {move_to_item ?v ?i},
    5 action {load_item ?v ?i},
    6 action {check_item ?i},
    7 action {move_item ?v ?i ?a},
    8 action {unload_item ?v ?i ?a},
    9 action {check_item ?i};
  orderings
    1 ---> 4, 2 ---> 4, 3 ---> 4,
    4 ---> 5, 4 ---> 6, 5 ---> 7,
    6 ---> 7, 7 ---> 8, 7 ---> 9;
  conditions
    unsupervised {empty_vehicle ?v} = true;
  effects
    {location ?i} = ?a,
    {location ?v} = ?a;
  use ?v across self,
      ?i across self;
  time_windows duration self = 1 hours;
end_schema;

schema slow_transport;
  vars ?v = ?{type slow_vehicle},
       ?i = ??,
       ?a = ?{type city};
  expands {slow_transport ?v ?i ?a};
  nodes
    1 action {fuel_vehicle ?v},
    2 action {check_vehicle ?v},
    3 action {brief_driver ?v},
    4 action {move_to_item ?v ?i},
    5 action {load_item ?v ?i},
    6 action {check_item ?i},
    7 action {move_item ?v ?i ?a},
    8 action {unload_item ?v ?i ?a},
    9 action {check_item ?i};
  orderings
    1 ---> 4, 2 ---> 4, 3 ---> 4,
    4 ---> 5, 4 ---> 6, 5 ---> 7,
    6 ---> 7, 7 ---> 8, 7 ---> 9;
  conditions
    unsupervised {empty_vehicle ?v} = true;
  effects
    {location ?i} = ?a,
    {location ?v} = ?a;
  use ?v across self,
      ?i across self;
  time_windows duration self = 2 hours;
end_schema;

schema transport_in_trucks;
  vars ?v = ?{type fleet_of_trucks},
       ?i = ??,
       ?a = ?{type city};
  expands {transport_in_trucks ?v ?i ?a};
  nodes
    1 action {fuel_vehicle ?v},
    2 action {check_vehicle ?v},
    3 action {brief_drivers ?v},
    4 action {move_to_item ?v ?i},
    5 action {load_item ?v ?i},
    6 action {check_item ?i},
    7 action {move_item ?v ?i ?a},
    8 action {unload_item ?v ?i ?a},
    9 action {check_item ?i};
  orderings
    1 ---> 4, 2 ---> 4, 3 ---> 4,
    4 ---> 5, 4 ---> 6, 5 ---> 7,
    6 ---> 7, 7 ---> 8, 7 ---> 9;
  conditions
    unsupervised {empty_vehicle ?v} = true;
  effects
    {location ?i} = ?a,
    {location ?v} = ?a;
  use ?v across self,
      ?i across self;
  time_windows duration self = 3 hours;
end_schema;

schema transport_by_air;
  vars ?v = ?{type helicopter},
       ?i = ??,
       ?a = ?{type city};
  expands {transport_by_air ?v ?i ?a};
  nodes
    1 action {fuel_vehicle ?v},
    2 action {check_vehicle ?v},
    3 action {brief_pilot ?v},
    4 action {move_to_item ?v ?i},
    5 action {load_item ?v ?i},
    6 action {check_item ?i},
    7 action {move_item ?v ?i ?a},
    8 action {unload_item ?v ?i ?a},
    9 action {check_item ?i};
  orderings
    1 ---> 4, 2 ---> 4, 3 ---> 4,
    4 ---> 5, 4 ---> 6, 5 ---> 7,
    6 ---> 7, 7 ---> 8, 7 ---> 9;
  conditions
    unsupervised {empty_vehicle ?v} = true;
  effects
    {location ?i} = ?a,
    {location ?v} = ?a;
  use ?v across self,
      ?i across self;
  time_windows duration self = 1 hours;
end_schema;

schema send_fleet_of_trucks;
  vars ?v = ?{type fleet_of_trucks},
       ?a = ?{type city};
  expands {send_fleet_of_trucks ?v ?a};
  nodes
    1 action {fuel_vehicle ?v},
    2 action {check_vehicle ?v},
    3 action {brief_drivers ?v},
    4 action {move_vehicle ?v ?a};
  orderings
    1 ---> 4, 2 ---> 4, 3 ---> 4;
  conditions
    unsupervised {empty_vehicle ?v} = true;
  effects
    {location ?v} = ?a;
  use ?v across self;
  time_windows duration self = 3 hours;
end_schema;

schema fix_gas_leak;
  vars ?t = ?{type repair_team},
       ?e = ?{type repair_equipment},
       ?a = ?{type city};
  expands {fix_gas_leak ?t ?e ?a};
  nodes
    1 action {locate_leaks ?t},
    2 action {prepare_sealant ?t},
    3 action {prepare_new_pipe ?t},
    4 action {prepare_site_for_repair ?t},
    5 action {seal_leak_in_pipe ?t},
    6 action {check_seal ?t},
    7 action {pack_equipment ?t ?e};
  orderings
    1 ---> 4, 4 ---> 5, 5 ---> 6,
    1 ---> 3, 3 ---> 5,
    2 ---> 5, 5 ---> 7;
  conditions
    unsupervised {location ?t} = ?a,
    unsupervised {location ?e} = ?a;
  use ?t across self,
      ?e across self;
  time_windows duration self = 3 hours;
end_schema;

schema fix_turbine;
  vars ?t = ?{type repair_team},
       ?e = ?{type repair_equipment},
       ?a = ?{type city};
  expands {fix_turbine ?t ?e ?a};
  nodes
    1 action {locate_turbine_problem ?t},
    2 action {fix_turbine_problem ?t},
    3 action {restart_turbine ?t};
  orderings
    1 ---> 2, 2 ---> 3;
  conditions
    unsupervised {location ?t} = ?a,
    unsupervised {location ?e} = ?a;
  use ?t across self,
      ?e across self;
  time_windows duration self = 3 hours;
end_schema;

schema restart_power_station;
  vars ?t = ?{type repair_team},
       ?a = ?{type city};
  expands {restart_power_station ?t ?a};
  nodes
    1 action {check_gas_system ?t},
    2 action {check_turbines ?t},
    3 action {restart_controller ?t};
  orderings
    1 ---> 3, 2 ---> 3;
  conditions
    unsupervised {location ?t} = ?a;
  use ?t across self;
  time_windows duration self = 1 hours;
end_schema;

schema deal_with_injured;
  vars ?t = ?{type medical_team},
       ?e = ?{type medical_equipment},
       ?a = ?{type city};
  expands {deal_with_injured ?t ?e ?a};
  nodes
    1 action {give_first_aid ?t},
    2 action {prepare_stretchers ?t},
    3 action {check_for_further_casualties ?t};
  conditions
    unsupervised {location ?t} = ?a,
    unsupervised {location ?e} = ?a;
  use ?t across self,
      ?e across self;
  time_windows duration self = 1 hours;
end_schema;

schema load_injured;
  vars ?t = ?{type medical_team},
       ?v = ??,
       ?a = ?{type city};
  expands {load_injured ?t ?v ?a};
  nodes
    1 action {load_injured_onto_stretchers ?t},
    2 action {load_stretchers_into_vehicle ?t ?v},
    3 action {check_injured_are_ok ?t},
    4 action {radio_back_to_base ?t Delta};
  orderings
    1 ---> 2, 2 ---> 4, 3 ---> 4;
  conditions
    unsupervised {location ?t} = ?a,
    unsupervised {location ?v} = ?a,
    unsupervised {empty_vehicle ?v} = true;
  effects
    {empty_vehicle ?v} = false at begin_of self;
  use ?t across self,
      ?v across self;
  time_windows duration self = 1 hours;
end_schema;

schema move_injured_by_air;
  vars ?v = ?{type helicopter},
       ?a = ?{type city},
       ?b = ?{type city};
  expands {move_injured_by_air ?v ?a ?b};
  nodes
    1 action {move_vehicle ?v ?b},
    2 action {check_injured_in_transit ?v};
  conditions
    unsupervised {location ?v} = ?a,
    unsupervised {empty_vehicle ?v} = false;
  effects
    {location ?v} = ?b,
    {empty_vehicle ?v} = true;
  use ?v across self;
  time_windows duration self = 1 hours;
end_schema;

schema move_injured_by_road;
  vars ?v = ?{type slow_vehicle},
       ?a = ?{type city},
       ?b = ?{type city};
  expands {move_injured_by_road ?v ?a ?b};
  nodes
    1 action {move_vehicle ?v ?b},
    2 action {check_injured_in_transit ?v};
  conditions
    unsupervised {location ?v} = ?a,
    unsupervised {empty_vehicle ?v} = false;
  effects
    {location ?v} = ?b,
    {empty_vehicle ?v} = true;
  use ?v across self;
  time_windows duration self = 2 hours;
end_schema;

schema load_population;
  vars ?t = ??,
       ?v = ?{type fleet_of_trucks},
       ?a = ?{type city};
  expands {load_population ?t ?v ?a};
  nodes
    1 action {group_people_for_evacuation ?t},
    2 action {load_groups_onto_trucks ?t ?v},
    3 action {radio_back_to_base ?t Delta};
  orderings
    1 ---> 2, 2 ---> 3;
  conditions
    unsupervised {location ?t} = ?a,
    unsupervised {location ?v} = ?a,
    unsupervised {empty_vehicle ?v} = true;
  effects
    {empty_vehicle ?v} = false at begin_of self;
  use ?t across self,
      ?v across self;
  time_windows duration self = 2 hours;
end_schema;

schema move_population;
  vars ?v = ?{type fleet_of_trucks},
       ?a = ?{type city},
       ?b = ?{type city};
  expands {move_population ?v ?a ?b};
  nodes
    1 action {move_vehicles ?v ?b},
    2 action {check_people_in_transit ?v};
  conditions
    unsupervised {location ?v} = ?a,
    unsupervised {empty_vehicle ?v} = false;
  effects
    {location ?v} = ?b,
    {empty_vehicle ?v} = true;
  use ?v across self;
  time_windows duration self = 3 hours;
end_schema;

schema defuse_bomb;
  vars ?bs = ?{type bomb_squad},
       ?at = ?{type city};
  expands {defuse_bomb ?bs ?at};
  nodes
    1 action {locate_bomb ?bs ?at},
    2 action {remove_primary_detonator ?bs},
    3 action {remove_other_detonators ?bs};
  orderings
    1 ---> 2, 1 ---> 3;
  conditions
    unsupervised {location ?bs} = ?at;
  use ?bs across self;
  time_windows duration self = 1 hours;
end_schema;

schema build_houses;
  vars ?bt = ?{type building_team},
       ?be = ?{type building_equipment},
       ?hk = ?{type house_kit},
       ?at = ?{type city};
  expands {build_houses ?bt ?be ?hk ?at};
  nodes
    1 action {lay_foundations ?bt},
    2 action {prepare_house_kits ?bt ?hk},
    3 action {put_walls_up ?bt},
    4 action {put_roof_on ?bt},
    5 action {put_windows_in ?bt},
    6 action {put_doors_in ?bt};
  orderings
    1 ---> 3, 2 ---> 3,
    3 ---> 4, 3 ---> 5, 3 ---> 6;
  conditions
    unsupervised {location ?bt} = ?at,
    unsupervised {location ?be} = ?at,
    unsupervised {location ?hk} = ?at;
  use ?bt across self,
      ?be across self,
      ?hk across self;
  time_windows duration self = 6 hours;
end_schema;

schema deal_with_crisis;
  vars ?c = ?{type commander},
       ?a = ?{type city};
  expands {deal_with_crisis ?c ?a};
  nodes
    1 action {assess_situation ?c ?a},
    2 action {contact_local_forces ?c ?a},
    3 action {radio_back_to_base ?c Delta};
  conditions
    unsupervised {location ?c} = ?a;
  use ?c across self;
  time_windows duration self = 1 hours;
end_schema;

schema shut_down_power;
  vars ?e = ?{type engineer},
       ?a = ?{type city};
  expands {shut_down_power ?e ?a};
  nodes
    1 action {shut_down_turbines ?e},
    2 action {flush_gas_system ?e},
    3 action {shut_down_controller ?e};
  orderings
    1 ---> 3, 2 ---> 3;
  conditions
    unsupervised {location ?e} = ?a;
  use ?e across self;
  time_windows duration self = 1 hours;
end_schema;

;;; Level 3 schemas.

schema fuel_vehicle;
  vars ?v = ??;
  expands {fuel_vehicle ?v};
end_schema;

schema check_vehicle;
  vars ?v = ??;
  expands {check_vehicle ?v};
end_schema;

schema brief_driver;
  vars ?v = ??;
  expands {brief_driver ?v};
end_schema;

schema brief_drivers;
  vars ?v = ??;
  expands {brief_drivers ?v};
end_schema;

schema brief_pilot;
  vars ?v = ??;
  expands {brief_pilot ?v};
end_schema;

schema move_to_item;
  vars ?v = ??,
       ?i = ??;
  expands {move_to_item ?v ?i};
end_schema;

schema load_item;
  vars ?v = ??,
       ?i = ??;
  expands {load_item ?v ?i};
end_schema;

schema check_item;
  vars ?i = ??;
  expands {check_item ?i};
end_schema;

schema move_item;
  vars ?v = ??,
       ?i = ??,
       ?a = ??;
  expands {move_item ?v ?i ?a};
end_schema;

schema unload_item;
  vars ?v = ??,
       ?i = ??,
       ?a = ??;
  expands {unload_item ?v ?i ?a};
end_schema;

schema move_vehicle;
  vars ?v = ??,
       ?a = ??;
  expands {move_vehicle ?v ?a};
end_schema;

schema locate_leaks;
  vars ?t = ??;
  expands {locate_leaks ?t};
end_schema;

schema prepare_sealant;
  vars ?t = ??;
  expands {prepare_sealant ?t};
end_schema;

schema prepare_new_pipe;
  vars ?t = ??;
  expands {prepare_new_pipe ?t};
end_schema;

schema prepare_site_for_repair;
  vars ?t = ??;
  expands {prepare_site_for_repair ?t};
end_schema;

schema seal_leak_in_pipe;
  vars ?t = ??;
  expands {seal_leak_in_pipe ?t};
end_schema;

schema check_seal;
  vars ?t = ??;
  expands {check_seal ?t};
end_schema;

schema pack_equipment;
  vars ?t = ??,
       ?e = ??;
  expands {pack_equipment ?t ?e};
end_schema;

schema locate_turbine_problem;
  vars ?t = ??;
  expands {locate_turbine_problem ?t};
end_schema;

schema fix_turbine_problem;
  vars ?t = ??;
  expands {fix_turbine_problem ?t};
end_schema;

schema restart_turbine;
  vars ?t = ??;
  expands {restart_turbine ?t};
end_schema;

schema check_gas_system;
  vars ?t = ??;
  expands {check_gas_system ?t};
end_schema;

schema check_turbines;
  vars ?t = ??;
  expands {check_turbines ?t};
end_schema;

schema restart_controller;
  vars ?t = ??;
  expands {restart_controller ?t};
end_schema;

schema give_first_aid;
  vars ?t = ??;
  expands {give_first_aid ?t};
end_schema;

schema prepare_stretchers;
  vars ?t = ??;
  expands {prepare_stretchers ?t};
end_schema;

schema check_for_further_casualties;
  vars ?t = ??;
  expands {check_for_further_casualties ?t};
end_schema;

schema load_injured_onto_stretchers;
  vars ?t = ??;
  expands {load_injured_onto_stretchers ?t};
end_schema;

schema load_stretchers_into_vehicle;
  vars ?t = ??,
       ?v = ??;
  expands {load_stretchers_into_vehicle ?t ?v};
end_schema;

schema check_injured_are_ok;
  vars ?t = ??;
  expands {check_injured_are_ok ?t};
end_schema;

schema radio_back_to_base;
  vars ?t = ??,
       ?a = ?{type city};
  expands {radio_back_to_base ?t ?a};
end_schema;

schema check_injured_in_transit;
  vars ?v = ??;
  expands {check_injured_in_transit ?v};
end_schema;

schema group_people_for_evacuation;
  vars ?t = ??;
  expands {group_people_for_evacuation ?t};
end_schema;

schema load_groups_onto_trucks;
  vars ?t = ??,
       ?v = ??;
  expands {load_groups_onto_trucks ?t ?v};
end_schema;

schema move_vehicles;
  vars ?v = ??,
       ?a = ?{type city};
  expands {move_vehicles ?v ?a};
end_schema;

schema check_people_in_transit;
  vars ?v = ??;
  expands {check_people_in_transit ?v};
end_schema;

schema locate_bomb;
  vars ?t = ??,
       ?a = ?{type city};
  expands {locate_bomb ?t ?a};
end_schema;

schema remove_primary_detonator;
  vars ?t = ??;
  expands {remove_primary_detonator ?t};
end_schema;

schema remove_other_detonators;
  vars ?t = ??;
  expands {remove_other_detonators ?t};
end_schema;

schema lay_foundations;
  vars ?t = ??;
  expands {lay_foundations ?t};
end_schema;

schema prepare_house_kits;
  vars ?t = ??,
       ?k = ??;
  expands {prepare_house_kits ?t ?k};
end_schema;

schema put_walls_up;
  vars ?t = ??;
  expands {put_walls_up ?t};
end_schema;

schema put_roof_on;
  vars ?t = ??;
  expands {put_roof_on ?t};
end_schema;

schema put_windows_in;
  vars ?t = ??;
  expands {put_windows_in ?t};
end_schema;

schema put_doors_in;
  vars ?t = ??;
  expands {put_doors_in ?t};
end_schema;

schema assess_situation;
  vars ?c = ??,
       ?a = ?{type city};
  expands {assess_situation ?c ?a};
end_schema;

schema contact_local_forces;
  vars ?c = ??,
       ?a = ?{type city};
  expands {contact_local_forces ?c ?a};
end_schema;

schema shut_down_turbines;
  vars ?e = ??;
  expands {shut_down_turbines ?e};
end_schema;

schema flush_gas_system;
  vars ?e = ??;
  expands {flush_gas_system ?e};
end_schema;

schema shut_down_controller;
  vars ?e = ??;
  expands {shut_down_controller ?e};
end_schema;

;;; End of file.

