;;; Concept: Glen A. Reece (DAI, Edinburgh) and Austin Tate (AIAI)
;;;          12th December 1992.
;;;
;;; File: pacifica3.tf
;;;
;;; Purpose: PRECiS NEO (Non-combatant Evacuation Operations) Scenario.
;;;          Domain description for transportation logistics problems.
;;;
;;; Created:  Brian Drabble and Jeff Dalton:  20th September 1994
;;;
;;; This file contains the TF which forms the second year demonstration of
;;; the O-Plan project. The aim of the project is to show the benefits of
;;; having a rich model of resources in an activity planner, The type
;;; of resources in the O-Plan resource hierarchy which are handled in the
;;; demonstration are as follows 
;;;
;;;     1. consumable-strictly: 
;;;        Aviation and diesel fuel in the tanks at Delta and the evacuees
;;;        at Abyss, Barnacle and Calypso.
;;;
;;;     2. consumable-producible-by-agent:
;;;        Aviation fuel brought by the KC-10 Tanker aircraft from Honolulu
;;;        when it is there is insufficient fuel at Delta to refuel the
;;;        B707 passenger aircraft prior to departure
;;;
;;;     3. consumable-producible-by-outwith-agent:
;;;        This resource is not handled as expected due to the version 2.2
;;;        not having the required event handling ability
;;;
;;;     4. resusable-nonsharable:
;;;        The ground transports, helicopters, C5, C141, KC-10 and B707.
;;;
;;;     5. resusable-sharable-independently:
;;;        The taxi ways at both Honolulu and Delta airbases.
;;;
;;;     6. reusable-sharable-synchronoulsy:
;;;        The runways at both Honolulu and Delta airbases
;;; 
;;; A City contains a particular numbers of people to be evacuated.
;;; GTs and ATshave a capacity, so more than one trip to a city may be 
;;; needed.The number still to be moved is tracked using techniques 
;;; developed for the Missionaries and Cannibals problem.  The M&C approach
;;; avoids uncertainty about the numbers (i.e. avoids PSVs).

;;;
;;;----------------------------------------------------------------------
;;;                 Domain Fact and Type Definitions

types 
 ;;;
 ;;; Transport Assets
 ;;;
      ground_transport         = (GT1 GT2),
      helicopter               = (AT1),
      air_transporter_cargo    = (C5 C141),
      air_transporter_evacuees = (B707),
      air_fuel_transporter     = (KC10),
      runway_status            = (clear in_use),
      cargo_types              = (passengers ground_transports air_transports),
 ;;;
 ;;; Geograpghic Information
 ;;;
      country          = (Pacifica Hawaii_USA),
      location         = (Abyss Barnacle Calypso Delta Honolulu),
      city             = (Abyss Barnacle Calypso Delta),
      air_base         = (Delta Honolulu),
      transport_use    = (in_transit available Abyss Barnacle Calypso Delta);
 ;;;
 ;;; resource information
 ;;;
 ;;;   resource_units gallons = count;

resource_types
     consumable_strictly {resource aviation_fuel_delta_tank1} = gallons,
     consumable_strictly {resource diesel_fuel_delta_tank2} = gallons,
     consumable_strictly {resource aviation_fuel_KC10_tank1} = gallons;

always {avgas_fuel_required Abyss Delta 140},
       {avgas_fuel_required Barnacle Delta 160},
       {avgas_fuel_required Calypso Delta 180},
       {diesel_fuel_required Abyss Delta 40},
       {diesel_fuel_required Barnacle Delta 60},
       {diesel_fuel_required Calypso Delta 80},

       {country Abyss} = Pacifica,
       {country Barnacle} = Pacifica,
       {country Calypso} = Pacifica,
       {country Delta} = Pacifica,
       {country Honolulu} = Hawaii_USA;

initially
;;;    {evac_status <city>}   = {<# left at city> <# safe at evac pt>}
       {evac_status Abyss}    = { 50 0},
       {evac_status Barnacle} = {100 0},
       {evac_status Calypso}  = { 20 0},
       {evacuate_to Delta};   ;;; the evacuation point in Pacifica

;;;
;;;------------------------------------------------------------------------
;;;                       Task Definitions

;;; Task Operation_Columbus evacuates prople from Abyss, Barnacle and
;;; Calypso to Delta.

task Operation_Columbus;
  nodes sequential 
          1 start,
          parallel
            3 action {transport_ground_transports Honolulu Delta},
            4 action {transport_helicopters Honolulu Delta}
	  end_parallel,
          parallel
            5 action {evacuate Abyss 50},
            6 action {evacuate Barnacle 100},
            7 action {evacuate Calypso 20}
          end_parallel,
          parallel
            8 action {fly_passengers Delta Honolulu},
            9 action {transport_ground_transports Delta Honolulu},
           10 action {transport_helicopters Delta Honolulu}
          end_parallel,
          2 finish
        end_sequential;

  effects {location_gt GT1} = Honolulu at 1,
          {location_gt GT2} = Honolulu at 1,
          {in_use_for GT1} = in_transit at 1,
          {in_use_for GT2} = in_transit at 1,

          {location_at AT1} = Honolulu at 1,
          {in_use_for AT1} = in_transit at 1,

	  {apportioned_forces GT} at 1,
	  {apportioned_forces AT} at 1,

	  {at C141} = Honolulu at 1,
	  {at C5} = Honolulu at 1,
	  {at KC10} = Honolulu at 1,
	  {at B707} = Delta at 1,
	  {runway_status_at Delta} = clear at 1,
	  {runway_status_at Honolulu} = clear at 1,
          {gt_capacity 25} at 1,
          {at_capacity 35} at 1;

  resources
    consumes {resource aviation_fuel_delta_tank1} = 0 .. 15000 gallons overall,
    consumes {resource aviation_fuel_KC10_tank1} = 0 .. 30000 gallons overall,
    consumes {resource diesel_fuel_delta_tank2} = 0 .. 8000 gallons overall;

end_task;

task Operation_Columbus_Mixed_Transports;
  nodes sequential 
          1 start,
          parallel
            3 action {transport_ground_transports Honolulu Delta},
            4 action {transport_helicopters Honolulu Delta}
	  end_parallel,
          parallel
            5 action {evacuate Abyss 50},
            6 action {evacuate Barnacle 100},
            7 action {evacuate Calypso 20}
          end_parallel,
          parallel
            8 action {fly_passengers Delta Honolulu},
            9 action {transport_ground_transports Delta Honolulu},
           10 action {transport_helicopters Delta Honolulu}
          end_parallel,
          2 finish
        end_sequential;

  effects {location_gt GT1} = Honolulu at 1,
          {location_gt GT2} = Honolulu at 1,
          {in_use_for GT1} = in_transit at 1,
          {in_use_for GT2} = in_transit at 1,

          {location_at AT1} = Honolulu at 1,
          {in_use_for AT1} = in_transit at 1,

	  {apportioned_forces GT} at 1,
	  {apportioned_forces AT} at 1,

	  {at C141} = Honolulu at 1,
	  {at C5} = Honolulu at 1,
	  {at KC10} = Honolulu at 1,
	  {at B707} = Delta at 1,
	  {runway_status_at Delta} = clear at 1,
	  {runway_status_at Honolulu} = clear at 1,
          {gt_capacity 25} at 1,
          {at_capacity 35} at 1;

  resources
    consumes {resource aviation_fuel_delta_tank1} = 0 .. 40000 gallons overall,
    consumes {resource aviation_fuel_KC10_tank1} = 0 .. 30000 gallons overall,
    consumes {resource diesel_fuel_delta_tank2} = 0 .. 8000 gallons overall;

end_task;

task Operation_Columbus_Ground_Transports_Only;
  nodes sequential 
          1 start,
          parallel
            3 action {transport_ground_transports Honolulu Delta},
            4 action {transport_helicopters Honolulu Delta}
	  end_parallel,
          parallel
            5 action {evacuate Abyss 50},
            6 action {evacuate Barnacle 100},
            7 action {evacuate Calypso 20}
          end_parallel,
          parallel
            8 action {fly_passengers Delta Honolulu},
            9 action {transport_ground_transports Delta Honolulu},
           10 action {transport_helicopters Delta Honolulu}
          end_parallel,
          2 finish
        end_sequential;

  effects {location_gt GT1} = Honolulu at 1,
          {location_gt GT2} = Honolulu at 1,
          {in_use_for GT1} = in_transit at 1,
          {in_use_for GT2} = in_transit at 1,

          {location_at AT1} = Honolulu at 1,
          {in_use_for AT1} = in_transit at 1,

	  {apportioned_forces GT} at 1,

	  {at C141} = Honolulu at 1,
	  {at C5} = Honolulu at 1,
	  {at KC10} = Honolulu at 1,
	  {at B707} = Delta at 1,
	  {runway_status_at Delta} = clear at 1,
	  {runway_status_at Honolulu} = clear at 1,
          {gt_capacity 25} at 1,
          {at_capacity 35} at 1;

  resources
    consumes {resource aviation_fuel_delta_tank1} = 0 .. 40000 gallons overall,
    consumes {resource aviation_fuel_KC10_tank1} = 0 .. 30000 gallons overall,
    consumes {resource diesel_fuel_delta_tank2} = 0 .. 8000 gallons overall;

end_task;

task Operation_Columbus_Helicopters_Only;
  nodes sequential 
          1 start,
          parallel
            3 action {transport_ground_transports Honolulu Delta},
            4 action {transport_helicopters Honolulu Delta}
	  end_parallel,
          parallel
            5 action {evacuate Abyss 50},
            6 action {evacuate Barnacle 100},
            7 action {evacuate Calypso 20}
          end_parallel,
          parallel
            8 action {fly_passengers Delta Honolulu},
            9 action {transport_ground_transports Delta Honolulu},
           10 action {transport_helicopters Delta Honolulu}
          end_parallel,
          2 finish
        end_sequential;

  effects {location_gt GT1} = Honolulu at 1,
          {location_gt GT2} = Honolulu at 1,
          {in_use_for GT1} = in_transit at 1,
          {in_use_for GT2} = in_transit at 1,

          {location_at AT1} = Honolulu at 1,
          {in_use_for AT1} = in_transit at 1,

	  {apportioned_forces AT} at 1,

	  {at C141} = Honolulu at 1,
	  {at C5} = Honolulu at 1,
	  {at KC10} = Honolulu at 1,
	  {at B707} = Delta at 1,
	  {runway_status_at Delta} = clear at 1,
	  {runway_status_at Honolulu} = clear at 1,
          {gt_capacity 25} at 1,
          {at_capacity 35} at 1;

  resources
    consumes {resource aviation_fuel_delta_tank1} = 0 .. 40000 gallons overall,
    consumes {resource aviation_fuel_KC10_tank1} = 0 .. 30000 gallons overall,
    consumes {resource diesel_fuel_delta_tank2} = 0 .. 8000 gallons overall;

end_task;

;;;
;;;------------------------------------------------------------------------
;;;                 Transportation and Counting Operators

schema evacuate_City;
  vars ?city   = ?{type city},
       ?number = ?{satisfies numberp};
  expands {evacuate ?city ?number};
  conditions achieve {evac_status ?city} = {0 ?number};
end_schema;

;;; Road_transport Air Transport and people-counting

;;; Variables that refer to evac_status values are marked e_ for effect,
;;; c_ for condition.  The schema must be invoked "for effects".
;;; All e_ variables must be bound by the time the schema is selected.
;;; The c_ values are then computed from the e_ values without
;;; introducing any PSVs.  OTOH, ?gt does become a PSV.

schema Road_Transport;
  vars ?from     = ?{type city},
       ?to       = ?{type air_base},
       ?gt       = ?{type ground_transport},
       ?e_left   = ?{satisfies numberp},
       ?e_safe   = ?{satisfies numberp},
       ?c_left   = ?{satisfies numberp},
       ?c_safe   = ?{satisfies numberp},
       ?capacity = ?{satisfies numberp},
       ?take     = ?{satisfies numberp};
  only_use_for_effects {evac_status ?from} = {?e_left ?e_safe};
  nodes 1 action {drive ?take in ?gt from ?from},
        2 dummy;
  conditions only_use_if {apportioned_forces GT},
	     only_use_if {evacuate_to ?to},
             only_use_if {gt_capacity ?capacity},
             compute {transport_step ?capacity ?e_left ?e_safe}
                        = {?c_left ?c_safe},
             compute {- ?e_safe ?c_safe} = ?take,
             achieve {evac_status ?from} = {?c_left ?c_safe} at 2,
             unsupervised {location_gt ?gt} = ?to at begin_of 1,
             unsupervised {in_use_for ?gt} = available at begin_of 1,
               supervised {in_use_for ?gt} = ?from
                          at end_of 1 from begin_of 1;
  effects {in_use_for ?gt} = ?from at begin_of 1,
          {in_use_for ?gt} = available at end_of 1;
end_schema;

;;; Variables that refer to evac_status values are marked e_ for effect,
;;; c_ for condition.  The schema must be invoked "for effects".
;;; All e_ variables must be bound by the time the schema is selected.
;;; The c_ values are then computed from the e_ values without
;;; introducing any PSVs.  OTOH, ?gt does become a PSV.

schema Air_Transport;
  vars ?from     = ?{type city},
       ?to       = ?{type air_base},
       ?at       = ?{type helicopter},
       ?e_left   = ?{satisfies numberp},
       ?e_safe   = ?{satisfies numberp},
       ?c_left   = ?{satisfies numberp},
       ?c_safe   = ?{satisfies numberp},
       ?capacity = ?{satisfies numberp},
       ?take     = ?{satisfies numberp};
  only_use_for_effects {evac_status ?from} = {?e_left ?e_safe};
  nodes 1 action {fly ?take in ?at from ?from},
        2 dummy;
  conditions only_use_if {apportioned_forces AT},
             only_use_if {evacuate_to ?to},
             only_use_if {at_capacity ?capacity},
             compute {transport_step ?capacity ?e_left ?e_safe}
                        = {?c_left ?c_safe},
             compute {- ?e_safe ?c_safe} = ?take,
             achieve {evac_status ?from} = {?c_left ?c_safe} at 2,
             unsupervised {location_at ?at} = ?to at begin_of 1,
             unsupervised {in_use_for ?at} = available at begin_of 1,
               supervised {in_use_for ?at} = ?from
                          at end_of 1 from begin_of 1;
  effects {in_use_for ?at} = ?from at begin_of 1,
          {in_use_for ?at} = available at end_of 1;
end_schema;

;;; Suppose we have oufe {evac_status A} = {0 100}, with capacity = 10.
;;; Well, we can do that if we can get {evac_status A} = {10 90}.  So
;;; we'll post {10 90} as an achieve.  The next road_transport activation
;;; will post {evac_status A} = {20 80} as an achieve, and so on until
;;; we reach {evac_status A} = {100 0} which is true initially.  Each
;;; step from an oufe to the required achieve is computed by the
;;; transport_step function below.

;;;
;;;------------------------------------------------------------------------
;;;            Transport and Passenger Movement Operators

schema transport_ground_transports;
;;;
;;; This schema is used to provide ground transportation from
;;; one air base location to another air base location.  This must take
;;; place by using a cargo plane to fly the nominated transportation
;;; vehicles from the source base to the destination base.
;;;
;;; fly all transport explictly from one base to another. No "forall" yet.
;;;
  vars  ?FROM = ?{type air_base},
        ?TO   = ?{type air_base};

  expands {transport_ground_transports ?FROM ?TO};

  nodes 1 action {load ground_transports},
	2 action {take_off_from ?FROM},
	3 action {fly_to ?TO},
	4 action {land_at ?TO},
	5 action {unload ground_transports};

  orderings 1 ---> 2, 2 ---> 3, 3 ---> 4, 4 ---> 5;

  conditions achieve {at C5} = ?FROM at 1,
             unsupervised {location_gt GT1} = ?FROM at 1,
             unsupervised {location_gt GT2} = ?FROM at 1,
	     unsupervised {runway_status_at ?FROM} = clear at begin_of 2,
	     supervised {runway_status_at ?FROM} = in_use at end_of 2 from
							begin_of 2,
	     unsupervised {runway_status_at ?TO} = clear at begin_of 4,
	     supervised {runway_status_at ?TO} = in_use at end_of 4 from
							begin_of 4;
  effects {at C5} = ?TO at 5,
          {location_gt GT1} = ?TO at 5,
          {location_gt GT2} = ?TO at 5,
          {in_use_for GT1} = available at 5,
          {in_use_for GT2} = available at 5,
	  {runway_status_at ?FROM} = in_use at begin_of 2,
	  {runway_status_at ?FROM} = clear at end_of 2,
	  {runway_status_at ?TO} = in_use at begin_of 4,
	  {runway_status_at ?TO} = clear at end_of 4;
end_schema;

;;;
;;;------------------------------------------------------------------------
;;;

schema transport_extra_fuel;
;;;
;;; This schema is used to provide air transportation of fuel from Honolulu
;;; to Delta. Planes transport explictly from one base to another. 
;;; No "forall" yet.  
;;;
  vars  ?FROM = ?{type air_base},
        ?TO   = ?{type air_base};

  expands {transport_fuel_reserves ?FROM ?TO};

  nodes 1 action {take_off_from ?FROM},
	2 action {fly_to ?TO},
	3 action {land_at ?TO};

  orderings 1 ---> 2, 2 ---> 3;

  conditions achieve {at KC10} = ?FROM at 1,
	     unsupervised {runway_status_at ?FROM} = clear at begin_of 1,
	     supervised {runway_status_at ?FROM} = in_use at end_of 1 from
							begin_of 1,
	     unsupervised {runway_status_at ?TO} = clear at begin_of 3,
	     supervised {runway_status_at ?TO} = in_use at end_of 3 from
							begin_of 3;
  effects {at KC10} = ?TO,
	  {runway_status_at ?FROM} = in_use at begin_of 1,
	  {runway_status_at ?FROM} = clear at end_of 1,
	  {runway_status_at ?TO} = in_use at begin_of 3,
	  {runway_status_at ?TO} = clear at end_of 3;
end_schema;

;;;
;;;------------------------------------------------------------------------
;;;

schema transport_helicopters;
;;;
;;; This schema is used to provide air transportation from
;;; one air base location to another air base location.  This must take
;;; place by using a cargo plane to fly the nominated transportation
;;; helicopters from the source base to the destination base.
;;;
;;; fly all transport explictly from one base to another. No "forall" yet.
;;;
;;;
  vars  ?FROM = ?{type air_base},
        ?TO   = ?{type air_base};

  expands {transport_helicopters ?FROM ?TO};

  nodes 1 action {load air_transports},
	2 action {take_off_from ?FROM},
	3 action {fly_to ?TO},
	4 action {land_at ?TO},
	5 action {unload air_transports};

  orderings 1 ---> 2, 2 ---> 3, 3 ---> 4, 4 ---> 5;

  conditions achieve {at C141} = ?FROM at 1,
             unsupervised {location_at AT1} = ?FROM at 1,
	     unsupervised {runway_status_at ?FROM} = clear at begin_of 2,
	     supervised {runway_status_at ?FROM} = in_use at end_of 2 from
							begin_of 2,
	     unsupervised {runway_status_at ?TO} = clear at begin_of 4,
	     supervised {runway_status_at ?TO} = in_use at end_of 4 from
							begin_of 4;
  effects {at C141} = ?TO,
          {location_at AT1} = ?TO at 5,
          {in_use_for AT1} = available at 5,
	  {runway_status_at ?FROM} = in_use at begin_of 2,
	  {runway_status_at ?FROM} = clear at end_of 2,
	  {runway_status_at ?TO} = in_use at begin_of 4,
	  {runway_status_at ?TO} = clear at end_of 4;
end_schema;

;;;
;;;------------------------------------------------------------------------
;;;

schema fly_passengers;
;;;
;;; This schema transports the evacuees from the airport at Delta to 
;;; Honolulu. The passenger transport needs 20,000 gallons of fuel for the
;;; journey which it obtains from the tanks at Delta. .
;;; The evacuees must be moved by passenger aircraft. 
;;;
  vars ?TO   = ?{type air_base},
       ?FROM = ?{type air_base};

  expands {fly_passengers ?FROM ?TO};

  nodes 1 action {load passengers},
	2 action {take_off_from ?FROM},
	3 action {fly_to ?TO},
	4 action {land_at ?TO},
	5 action {unload passengers};

  orderings 1 ---> 2, 2 ---> 3, 3 ---> 4, 4 ---> 5;

  conditions unsupervised {at B707} = ?FROM at 1,
	     unsupervised {runway_status_at ?FROM} = clear at begin_of 2,
	     supervised {runway_status_at ?FROM} = in_use at end_of 2
						from begin_of 2,
	     unsupervised {runway_status_at ?TO} = clear at begin_of 4,
	     supervised {runway_status_at ?TO} = in_use at end_of 4
						from begin_of 4;
	     
  effects {at B707} = ?TO at 5,
	  {runway_status_at ?FROM} = in_use at begin_of 2,
	  {runway_status_at ?FROM} = clear at end_of 2,
	  {runway_status_at ?TO} = in_use at begin_of 4,
	  {runway_status_at ?TO} = clear at end_of 4,
          {nationals out} = true at 5;

  resources 
    consumes {resource aviation_fuel_delta_tank1} = 20000 gallons;
end_schema;

;;;
;;;------------------------------------------------------------------------
;;;

schema fly_passengers_and_refuel_transporter;
;;;
;;; This schema transports the evacuees from the airport at Delta to 
;;; Honolulu. The passenger transport needs 20,000 gallons of fuel for the
;;; journey which has to be brought from Honolulu in a KC-10 tanker transport.
;;; The evacuees must be moved by passenger aircraft.
;;;
  vars ?TO   = ?{type air_base},
       ?FROM = ?{type air_base};

  expands {fly_passengers ?FROM ?TO};

  nodes 1 action {load passengers},
	2 action {take_off_from ?FROM},
	3 action {fly_to ?TO},
	4 action {land_at ?TO},
	5 action {unload passengers};

  orderings 1 ---> 2, 2 ---> 3, 3 ---> 4, 4 ---> 5;

  conditions achieve {passenger_transporter_refuelled} at 2,
             unsupervised {at B707} = ?FROM at 1,
	     unsupervised {runway_status_at ?FROM} = clear at begin_of 2,
	     supervised {runway_status_at ?FROM} = in_use at end_of 2
						from begin_of 2,
	     unsupervised {runway_status_at ?TO} = clear at begin_of 4,
	     supervised {runway_status_at ?TO} = in_use at end_of 4
						from begin_of 4;

  effects {at B707} = ?TO at 5,
	  {runway_status_at ?FROM} = in_use at begin_of 2,
	  {runway_status_at ?FROM} = clear at end_of 2,
	  {runway_status_at ?TO} = in_use at begin_of 4,
	  {runway_status_at ?TO} = clear at end_of 4,
          {nationals out} = true at 5;

  resources 
    consumes {resource aviation_fuel_KC10_tank1} = 20000 gallons;
end_schema;

;;;
;;;------------------------------------------------------------------------
;;;

schema refuel_transporter;

  only_use_for_effects {passenger_transporter_refuelled};

  nodes 1 action {transport_fuel_reserves Honolulu Delta},
        2 action {refuel_transporter},
        3 action {transport_fuel_reserves Delta Honolulu};

  orderings 1 ---> 2, 2 ---> 3;
end_schema;

;;;
;;;------------------------------------------------------------------------
;;;          Primitive Level Schemas

schema drive;
  vars ?from          = ?{type city},
       ?take          = ?{satisfies numberp},
       ?fuel_required = ?{satisfies numberp},
       ?gt            = ?{type ground_transport};
  expands {drive ?take in ?gt from ?from};

  conditions 
    only_use_if {diesel_fuel_required ?from Delta ?fuel_required};

  resources 
    consumes {resource diesel_fuel_delta_tank2} = ?fuel_required gallons;

  time_windows duration self = 3 hours .. 4 hours;
end_schema;

;;;
;;;------------------------------------------------------------------------
;;;

schema fly;
  vars ?from          = ?{type city},
       ?take          = ?{satisfies numberp},
       ?fuel_required = ?{satisfies numberp},
       ?at            = ?{type helicopter};

  expands {fly ?take in ?at from ?from};

  conditions 
    only_use_if {avgas_fuel_required ?from Delta ?fuel_required};

  resources 
    consumes {resource aviation_fuel_delta_tank1} = ?fuel_required gallons;

  time_windows duration self = 3 hours .. 4 hours;
end_schema;

;;;
;;;------------------------------------------------------------------------
;;;
schema load_cargo;
  vars ?cargo = ?{type cargo_types};

  expands {load ?cargo};

  effects {loaded ?cargo};
end_schema;

;;;
;;;------------------------------------------------------------------------
;;;

schema Add_fuel_to_transporter;

  expands {refuel_transporter};

  effects {transporter_refuelled};
end_schema;
;;;
;;;------------------------------------------------------------------------
;;;

schema take_off_from_airbase;
  vars ?airbase = ?{type air_base};

  expands {take_off_from ?airbase};

  effects {taken_off_from ?airbase};
end_schema;

;;;
;;;------------------------------------------------------------------------
;;;

schema Fly_to_Airbase;
  vars ?airbase = ?{type air_base};

  expands {fly_to ?airbase};

  effects {in_transit_to ?airbase};
end_schema;

;;;
;;;------------------------------------------------------------------------
;;;

schema Land_at_airbase;
  vars ?airbase = ?{type air_base};

  expands {land_at ?airbase};

  effects {cargo_landed_at_airbase ?airbase};
end_schema;

;;;
;;;------------------------------------------------------------------------
;;;

schema Unload_cargo;
  vars ?cargo = ?{type cargo_types};

  expands {unload ?cargo};

  effects {unloaded ?cargo};
end_schema;

;;;
;;;------------------------------------------------------------------------
;;;                       Language Lisp Definitions

language lisp;

  (defun transport_step (capacity e_left e_safe) ; -> (c_left c_safe)
    (let ((take (min e_safe capacity)))
      (list (+ e_left take)
            (- e_safe take))))

end_language;

;;; End

