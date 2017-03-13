;;;; File: ~oplan/development/prerelease/restricted/demo/tf/socap-full.tf
;;;; SCCS Version: @(#)tf-classes       2.1
;;;; Contains: Working file version of the IFD-2 operators and initial facts.
;;;; Author: Brian Drabble
;;;; Created: Tue Jan 26 14:30:30 1993
;;;; Updated: Wed Dec 13 17:15:22 1995 by Jeff Dalton
;;;; Release Version: 2.1
;;;; Copyright: (c) 1992, AIAI, University of Edinburgh
;;;; This material may be reproduced by or for the U.S. Government pursuant
;;;; to the copyright license under the clause at DFARS 252.227-7032
;;;; (June 1975) -- Rights in Technical Data and Computer Software (Foreign).

;;; >>> This version modified for Expect <<<

;;; (But can become the real one once it works.)

;;;
;;;     This file contains the descriptions of the possible plan operators that
;;;     may be employed during the planning process.
;;;
;;;     The operators are arranged in levels that approximate to the different
;;;     levels of the COA/OPLAN development process:
;;;          Level 1:  Select mission type   e.g scale of military operation
;;;          Level 2:  Identify specific threats and locations
;;;          Level 3:  Select employment operations, forces and destinations
;;;          Level 4:  Add deployment actions for all units
;;;          Level 5:  Add ILOCs and compute movement durations
;;;          Level 6:  Add further movements and durations
;;;
;;;     Also functions for computing durations of airlift and sealift movements.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLASSES information needed for this simplified demonstration
;;;

types course_of_action = (coa_1 coa_2 coa_3 coa_4 coa_5),

      territory = (ntunisia stunisia tunisia spain sicily italy france
                   nalgeria algeria),

      sea = (medsea atlantic),

      region = (nwtunisian_border ntunisian_border stunisian_border
                swtunisian_border wtunisian_border ntunisian_coastal
                nwtunisian_coastal netunisian_coastal ntunisian_desert
                swtunisian_desert ntunisian_mountain
                nwtunisian_mountain netunisian_mountain stunisian_plain
                nealgerian_border nalgerian_border salgerian_border
                sealgerian_border ealgerian_border nealgerian_coastal),

      route = (route_A route_B route_C tunis_tabarqah tunis_jundubah
               tunis_bizerte tunis_saminjah),


      location = (tunis tabarqah souk_el_arba ghar_ad_dima jundubah
                  saminjah al_kaf bizerte livorno_supply trapani moron
                  rota_pol ft_bliss_land ft_meade_port gray_aaf_ft_lewis_afb
                  norfolk_shyd shaw_afb_afb dobbins_afb_atlan_afb
		  eglin_afb_afb
                  tunis_port bizerte_port livorno_port palermo_port
                  rota_port norfolk_port jacksonville_port savannah_port
                  charleston_port 
                  bizerte_sidi_ahme_afb tunis_carthage_afb
                  trapani_birgi_afb sigonella_afb livorno_apt
                  rota_naval_statio_afb moron_ab_afb mcdill_afb),

      urban = (tunis tabarqah souk_el_arba ghar_ad_dima jundubah
               saminjah al_kaf bizerte livorno_supply trapani moron
               sigonella palermo rota_pol),

      army = (mechanized armored light_infantry airborne),

      special = (5rg 5sb),

      navy = (marine submarine frigate corvette fast_attack coast_patrol mine_layer),

      airforce = (tfighter tattack sattack sairlift tairlift),

      tfighter = (MIG23_sq MIG25_sq 89b 89c 891sq 892sq),

      sea_port = (tunis_port bizerte_port livorno_port palermo_port
                  rota_port norfolk_port jacksonville_port savannah_port
                  charleston_port tacoma_port beaumont_port wilmington_port),

      sea_sector = (sea_ntunisian sea_nwtunisian sea_netunisian
                    ntunisian_coast netunisian_coast etunisian_coast
                    nwtunisian_coast nealgerian_coast wmedsea emedsea
                    satlantic),

      sea_location  = (sloc_sicily_ntunisia sloc_spain_ntunisia
                       sloc_italy_ntunisia sloc_spain_italy sloc_spain_sicily
                       sloc_conus_spain sloc_conus_italy sloc_conus_sicily
                       sloc_conus_ntunisia sea_route_A sea_route_B),

      air_location = (aloc_sicily_ntunisia aloc_spain_ntunisia
                      aloc_italy_ntunisia aloc_spain_italy aloc_spain_sicily
                      aloc_libya_tunisia aloc_conus_ntunisia aloc_conus_spain
                      aloc_conus_sicily aloc_conus_italy
                      aloc_nealgeria_ntunisia aloc_nalgeria_netunisia
                      aloc_ealgeria_stunisia aloc_salgeria_setunisia
                      air_route_A air_route_B),

      airspace = (air_tunisia),

      air_sector = (air_ntunisia air_stunisia air_wmedsea
                    air_nwtunisian_border air_ntunisian_border
                    air_stunisian_border air_swtunisian_border
                    air_wtunisian_border air_nwtunisian_coastal
                    air_netunisian_coastal air_ntunisian_coastal
                    air_ntunisian_desert air_swtunisian_desert
                    air_ntunisian_mountain air_nwtunisian_mountain
                    air_netunisian_mountain air_stunisian_plain
                    air_nealgerian_border air_nalgerian_border
                    air_salgerian_border air_sealgerian_border
                    air_ealgerian_border air_nealgerian_coastal),

      airfield = (bizerte_sidi_ahme_afb tunis_carthage_afb
                  trapani_birgi_afb sigonella_afb livorno_apt
                  rota_naval_statio_afb moron_ab_afb mcdill_afb
                  gray_aaf_ft_lewis_afb norfolk_nas_afb shaw_afb_afb 
 		  dobbins_afb_atlan_afb eglin_afb_afb biggs_aaf_afb
                  mccord andrews_afb_naf_afb),


      army_threat = (alg_1stArmBde alg_2ndArmBde alg_MIB),

      army_defence = (57th_IMF 3rd_ACR 107th_ACR 116th_ACR),

      navy_threat = (alg_corvette alg_frigate),

      navy_defence = (CVN71_ACN CV66_ACS CV67_ACS BB62_SAG),

      airforce_threat = (alg_1stmig23 alg_mig25),

      airforce_defence = (16th_F15E 27th_F15 42nd_F15 43rd_F15),

      sairlift = (C5),

      ssealift = (fastship),

      tairlift = (563rd_c130 817th_c130 819th_c130 532nd_c130 533rd_c130 357th_c130 75th_c130
                  758th_c130 328th_c130 327th_c130 386th_c130 63rd_c130 63th_c130 817th_c130),

      cargobyair = (3rd_acr_byair 3rd_acr_%byair 107th_acr_byair 107th_acr_%byair
                    116th_acr_byair 116th_acr_%byair 57th_imf_byair 57th_imf_%byair
                    16th_f15e_byair 16th_f15e_%byair 27th_f15_byair 27th_f15_%byair
                    42nd_f15_byair 42nd_f15__%byair 43rd_f15_byair 43rd_f15__%byair),

      cargobysea = (3rd_acr_bysea 3rd_acr_%bysea 107th_acr_bysea 107th_acr_%bysea
                    116th_acr_bysea 116th_acr_%bysea 57th_imf_bysea 57th_imf_%bysea
                    16th_f15e_bysea 16th_f15e_%bysea 27th_f15_bysea 27th_f15_%bysea
                    42nd_f15_bysea 42nd_f15__%bysea 43rd_f15_bysea 43rd_f15__%bysea);
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ALAWYS facts needed for this simplified demonstration

    always 
        {d_day 21},
;;;
;;;
;;; The following data applies to the counter threat operators and deals with the 
;;; alg_1stArmBde, alg_corvette, alg_mig23
;;;

	{immed_threat_enemy alg_1stArmBde route_A coa_1 21},
        {immed_threat_enemy alg_corvette sea_route_A coa_1 15},
        {immed_threat_enemy alg_1stmig23 air_route_A coa_1 25},

        {mobility alg_1stArmBde} = 2,
        {thirdsize_firepower alg_1stArmBde} = 14400,
	{terrain route_A} = 1,
        {size route_A} = 5,
        {terrain tunis_jundubah} = 2,
        {size tunis_jundubah} = 5,
        {near_territory tabarqah route_A},
	{near_territory bizerte route_A},	
	{located_within bizerte ntunisian_coastal},
	{located_within tabarqah nwtunisian_coastal},	
	{located alg_1stArmBde nealgerian_coastal coa_1},
	{adjacent_territory nealgerian_coastal nwtunisian_coastal},

        (sea_firepower alg_corvette) = 3,
        (sea_mobility alg_corvette) = 3,
	{located alg_corvette nealgerian_coast coa_1},
	{adjacent_territory nwtunisian_coast nealgerian_coast},

        (air_firepower alg_1stmig23) = 250,
	{near_territory air_nwtunisian_coastal air_route_A},
	{located alg_1stmig23 air_nealgerian_coastal coa_1},
	{adjacent_territory air_nealgerian_coastal air_nwtunisian_coastal},
;;;
;;;
;;; The following data applies to the counter threat operators and deals with the 
;;; alg_2ndArmBde, alg_MIB, alg_frigate, alg_mig25
;;;

	{threat_enemy alg_2ndArmBde route_B coa_1 34},
        (terrain route_B) = 3,
        (size route_B) = 5,
        {thirdsize_firepower alg_2ndArmBde} = 14400.0,
        {mobility alg_2ndArmBde} = 2,
        {key_terrain jundubah coa_1},
        
        {key_loc tunis_jundubah coa_1},
        {key_base tunis coa_1},
        {base_approval tunis coa_1},
        {air_base_approval tunis_carthage_afb coa_1},
        {key_port tunis_port coa_1},
        {port_approval tunis_port coa_1},
        {key_sloc sloc_sicily_ntunisia coa_1},
        {key_sea_sector ntunisian_coast coa_1},
        {key_airfield tunis_carthage_afb coa_1},
        {key_aloc aloc_sicily_ntunisia coa_1},
        {key_air_sector air_wtunisian_border coa_1},

	{near_territory jundubah route_B},
	{overlaps tunis_jundubah route_B},
	{route_loc tunis jundubah tunis_jundubah},

	{threat_enemy_reserves alg_MIB coa_1 34},

	{threat_enemy alg_frigate sloc_sicily_ntunisia coa_1 21},
        {sea_firepower alg_frigate} = 4,
        {sea_mobility alg_frigate} = 4,
	{route_sloc palermo_port tunis_port sloc_sicily_ntunisia},
	{closest_port palermo_port ntunisia},
	{route_sloc catania_port tunis_port sloc_sicily_ntunisia},
	{near_territory sloc_sicily_ntunisia ntunisian_coast},

	{threat_enemy alg_mig25 air_tunisia coa_1 34},
	{threat_enemy alg_mig25 aloc_sicily_ntunisia coa_1 34},
        {air_firepower alg_mig25} = 180,
        {twice_max_range_to_border tunis_carthage_afb} = 400,

	{located_within tunis_carthage_afb air_tunisia},
	{route_aloc trapani_birgi_afb tunis_carthage_afb aloc_sicily_ntunisia},
	{near_territory air_wtunisian_border aloc_sicily_ntunisia},
;;;
;;;
;;; The following forces are the ones available as a deterrent to the above
;;; forces in this simple example
;;;

 	{terrain_type 57th_IMF} = 4,
 	{type_size 57th_IMF} = 4,
 	{mobility 57th_IMF} = 3, 
 	{firepower 57th_IMF} = 46720,

 	{terrain_type 3rd_ACR} = 4,
 	{type_size 3rd_ACR} = 4,
 	{mobility 3rd_ACR} = 4,
 	{f_ratio 3rd_ACR} = 12,
 	{firepower 3rd_ACR} = 65904,

 	{terrain_type 107th_ACR} = 4,
 	{type_size 107th_ACR} = 4,
 	{mobility 107th_ACR} = 3, 
 	{f_ratio 107th_ACR} = 12,
 	{firepower 107th_ACR} = 65904,

 	{terrain_type 116th_ACR} = 4,
 	{type_size 116th_ACR} = 4,
 	{mobility 116th_ACR} = 3, 
 	{f_ratio 116th_ACR} = 12,
 	{firepower 116th_ACR} = 65904,

 	{sea_mobility CVN71_ACN} = 6,
 	{sea_firepower CVN71_ACN} = 6,

 	{sea_mobility CV66_ACS} = 6,
 	{sea_firepower CV66_ACS} = 6,

 	{sea_mobility CV67_ACS} = 6,
 	{sea_firepower CV67_ACS} = 6,

 	{sea_mobility BB62_SAG} = 5,
 	{sea_firepower BB62_SAG} = 5,

 	{air_firepower 27th_F15} = 360,
 	{a_range 27th_F15} = 500,

 	{air_firepower 42nd_F15} = 360,
 	{a_range 42nd_F15} = 500,

 	{air_firepower 43rd_F15} = 360,
 	{a_range 43rd_F15} = 500,

 	{air_firepower 16th_F15E} = 360,
 	{a_range 16th_F15E} = 500,
;;;
;;; The following data concerns the location of forces and the routes along
;;; which they must travel to their points of employment
;;;
        {near_territory ROTA_NAVAL_STATIO_AFB ROTA_POL},
        {near_territory ROTA_PORT ROTA_POL},
        {near_territory MCCORD GRAY_AAF_FT_LEWIS_AFB},
        {near_territory TACOMA_PORT GRAY_AAF_FT_LEWIS_AFB},
	{near_territory BIGGS_AAF_AFB FT_BLISS_LAND},
        {near_territory BEAUMONT_PORT FT_BLISS_LAND},
        {near_territory ANDREWS_AFB_NAF_AFB FT_MEADE_PORT},
	{near_territory NORFOLK_NAS_AFB NORFOLK_SHYD},
	{near_territory NORFOLK_PORT NORFOLK_SHYD},
        {near_territory WILMINGTON_PORT FT_MEADE_PORT},
        {near_territory SHAW_AFB_AFB SHAW_AFB_AFB},
        {near_territory CHARLESTON_PORT SHAW_AFB_AFB},
        {near_territory EGLIN_AFB_AFB EGLIN_AFB_AFB},
        {near_territory JACKSONVILLE_PORT EGLIN_AFB_AFB},
        {near_territory DOBBINS_AFB_ATLAN_AFB DOBBINS_AFB_ATLAN_AFB},
        {near_territory SAVANNAH_PORT DOBBINS_AFB_ATLAN_AFB},

        {partition_force 57TH_IMF 57TH_IMF_%byair 57TH_IMF_%bysea 57TH_IMF_bysea},
        {partition_force 3RD_ACR 3RD_ACR_%byair 3RD_ACR_%bysea 3RD_ACR_bysea},
        {partition_force 107TH_ACR 107TH_ACR_%byair 107TH_ACR_%bysea 107TH_ACR_bysea},
        {partition_force 116TH_ACR 116TH_ACR_%byair 116TH_ACR_%bysea 116TH_ACR_bysea},
        {partition_force 16TH_F15E 16TH_F15E_byair 16TH_F15E_bysea},
        {partition_force 27TH_F15 27TH_F15_byair 27TH_F15_bysea},
        {partition_force 42ND_F15 42ND_F15_byair 42ND_F15_bysea},
        {partition_force 43RD_F15 43RD_F15_byair 43RD_F15_bysea},

        {transit_approval tunis_port},
        {transit_approval palermo_port},
        {transit_approval tunis_carthage_afb},

        {near_territory tunis_port tunis},
        {near_territory tunis_carthage_afb tunis},
        {near_territory tunis_port tunis_carthage_afb},

        {route_aloc shaw_afb_afb tunis_carthage_afb aloc_conus_ntunisia},
        {route_aloc eglin_afb_afb tunis_carthage_afb aloc_conus_ntunisia},
        {route_aloc dobbins_afb_atlan_afb tunis_carthage_afb aloc_conus_ntunisia},
        {route_aloc rota_naval_statio_afb tunis_carthage_afb aloc_spain_ntunisia},
        {route_aloc biggs_aaf_afb tunis_carthage_afb aloc_conus_ntunisia},
        {route_aloc andrews_afb_naf_afb tunis_carthage_afb aloc_conus_ntunisia},
        {route_aloc mccord tunis_carthage_afb aloc_conus_ntunisia},
        
        {route_sloc norfolk_port palermo_port sloc_conus_sicily},
        {route_sloc savannah_port tunis_port sloc_conus_ntunisia},
        {route_sloc wilmington_port tunis_port sloc_conus_ntunisia},
        {route_sloc beaumont_port tunis_port sloc_conus_ntunisia},
        {route_sloc jacksonville_port tunis_port sloc_conus_ntunisia},
        {route_sloc tacoma_port tunis_port sloc_conus_ntunisia},
        {route_sloc charleston_port tunis_port sloc_conus_ntunisia},
        {route_sloc rota_port tunis_port sloc_spain_ntunisia},
        {route_sloc rota_port palermo_port sloc_spain_sicily},

	{apportioned_forces fastship},
	{apportioned_forces C5},
        {number_berths tunis_port} = 40,
        {number_berths palermo_port} = 40,
	{runway_length tunis_carthage_afb} = 9500,

        {sealift_duration2 fastship 27th_f15_bysea savannah_port tunis_port} = 20,
        {sealift_duration2 fastship 42nd_f15_bysea jacksonville_port tunis_port} = 10,
        {sealift_duration2 fastship 43rd_f15_bysea jacksonville_port tunis_port} = 10,
        {sealift_duration2 fastship 16th_f15e_bysea charleston_port tunis_port} = 15,

        {sealift_duration3 fastship CV67_ACS norfolk_port palermo_port} = 20,
        {sealift_duration3 fastship CV66_ACS rota_port palermo_port} = 20,
        {sealift_duration3 fastship CVN71_ACN rota_port palermo_port} = 20,
        {sealift_duration3 fastship BB62_SAG rota_port palermo_port} = 20,

        {airlift_duration1 C5 27th_f15_byair dobbins_afb_atlan_afb tunis_carthage_afb} = 3,
        {airlift_duration1 C5 42nd_f15_byair eglin_afb_afb tunis_carthage_afb} = 3,
        {airlift_duration1 C5 43rd_f15_byair eglin_afb_afb tunis_carthage_afb} = 3,
        {airlift_duration1 C5 16th_f15e_byair shaw_afb_afb tunis_carthage_afb} = 4,

        {airlift_duration1 C5 57th_imf_%byair rota_naval_statio_afb tunis_carthage_afb} = 3,
        {airlift_duration1 C5 107th_acr_%byair andrews_afb_naf_afb tunis_carthage_afb} = 3,
        {airlift_duration1 C5 3rd_acr_%byair biggs_aaf_afb tunis_carthage_afb} = 3,
        {airlift_duration1 C5 116th_acr_%byair mccord tunis_carthage_afb} = 4,

        {sealift_duration2 fastship 57th_imf_%bysea rota_port tunis_port} = 20,
        {sealift_duration2 fastship 107th_acr_%bysea wilmington_port tunis_port} = 10,
        {sealift_duration2 fastship 3rd_acr_%bysea beaumont_port tunis_port} = 10,        
	{sealift_duration2 fastship 116th_acr_%bysea tacoma_port tunis_port} = 10,

        {sealift_duration2 fastship 57th_imf_bysea rota_port tunis_port} = 30,
        {sealift_duration2 fastship 107th_acr_bysea wilmington_port tunis_port} = 20,
        {sealift_duration2 fastship 3rd_acr_bysea beaumont_port tunis_port} = 20,        
	{sealift_duration2 fastship 116th_acr_bysea tacoma_port tunis_port} = 20,

        {ground_duration dobbins_afb_atlan_afb dobbins_afb_atlan_afb} = 1,
        {ground_duration dobbins_afb_atlan_afb savannah_port} = 10,
	{ground_duration eglin_afb_afb eglin_afb_afb} = 1,
	{ground_duration eglin_afb_afb jacksonville_port} = 15,
	{ground_duration shaw_afb_afb shaw_afb_afb} = 1,
	{ground_duration shaw_afb_afb charleston_port} = 10,
	{ground_duration tunis_port tunis_carthage_afb} = 10,
	{ground_duration rota_pol rota_port} = 2,
	{ground_duration rota_pol rota_naval_statio_afb} = 4,
        {ground_duration ft_meade_port andrews_afb_naf_afb} = 6,
        {ground_duration ft_bliss_land biggs_aaf_afb} = 4,
        {ground_duration gray_aaf_ft_lewis_afb mccord} = 3,
	{ground_duration tunis_carthage_afb tunis} = 3,
	{ground_duration ft_meade_port wilmington_port} = 3,
	{ground_duration ft_bliss_land beaumont_port} = 4,
	{ground_duration gray_aaf_ft_lewis_afb tacoma_port} = 3,
	{ground_duration tunis_port tunis} = 4,
	{ground_duration norfolk_shyd norfolk_port} = 2;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                      TOP LEVEL TASK SCHEMA
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; top level task schema to initiate planning,this action is refined by the 
;;; schemas below

task Deter_Border_Incursions_Only;
  nodes 1 start,
        2 finish,
        3 action {deter_border_incursion coa_1};

  orderings 1 ---> 3, 3 ---> 2;

  effects
        {protected_ti coa_1} at 2,               ;;; overall task requirement
        {border_incursion_deterred coa_1} at 3,
        {located fastship norfolk_port} at 1,
        {located C5 mcdill_afb} at 1,
        {located C130 mcdill_afb} at 1,
        {key_terrain_status jundubah coa_1} = unoccupied at 1,
        {loc_protected_status tunis_jundubah coa_1} = unprotected at 1,
        {harbor_defended_status tunis_port coa_1} = unprotected at 1,
        {sloc_protected_status sloc_sicily_ntunisia coa_1} = unprotected at 1,
        {naval_patrol_status ntunisian_coast coa_1} = unprotected at 1,

        {apportioned_forces_status 57th_IMF} = unallocated at 1,
        {apportioned_forces_status 3rd_ACR} = unallocated at 1,
        {apportioned_forces_status 107th_ACR} = unallocated at 1,
        {apportioned_forces_status 116th_ACR} = unallocated at 1,
        {apportioned_forces_status CVN71_ACN} = unallocated at 1,
        {apportioned_forces_status CV66_ACS} = unallocated at 1,
        {apportioned_forces_status CV67_ACS} = unallocated at 1,
        {apportioned_forces_status BB62_SAG} = unallocated at 1,
        {apportioned_forces_status 16th_F15E} = unallocated at 1,
        {apportioned_forces_status 27th_F15} = unallocated at 1,
        {apportioned_forces_status 42nd_F15} = unallocated at 1,
        {apportioned_forces_status 43rd_F15} = unallocated at 1,

        {located 57TH_IMF ROTA_POL} at 1,
        {located 57TH_IMF_%byair ROTA_POL} at 1,
        {located 57TH_IMF_%bysea ROTA_POL} at 1,
        {located 57TH_IMF_bysea ROTA_POL} at 1,
        {located 3RD_ACR FT_BLISS_LAND} at 1,
        {located 3RD_ACR_%byair FT_BLISS_LAND} at 1,
        {located 3RD_ACR_%bysea FT_BLISS_LAND} at 1,
        {located 3RD_ACR_bysea  FT_BLISS_LAND} at 1,
        {located 107TH_ACR FT_MEADE_PORT} at 1,
        {located 107TH_ACR_%byair FT_MEADE_PORT} at 1,
        {located 107TH_ACR_%bysea FT_MEADE_PORT} at 1,
        {located 107TH_ACR_bysea FT_MEADE_PORT} at 1,
        {located 116TH_ACR GRAY_AAF_FT_LEWIS_AFB} at 1,
        {located 116TH_ACR_%byair GRAY_AAF_FT_LEWIS_AFB} at 1,
        {located 116TH_ACR_%bysea GRAY_AAF_FT_LEWIS_AFB} at 1,
        {located 116TH_ACR_bysea GRAY_AAF_FT_LEWIS_AFB} at 1,
        {located CVN71_ACN ROTA_POL} at 1,
        {located CV66_ACS ROTA_POL} at 1,
        {located BB62_SAG ROTA_POL} at 1,
        {located CV67_ACS NORFOLK_SHYD} at 1,
        {located 16TH_F15E SHAW_AFB_AFB} at 1,        
	{located 16TH_F15E_byair SHAW_AFB_AFB} at 1,
        {located 16TH_F15E_bysea SHAW_AFB_AFB} at 1,
        {located 27TH_F15 DOBBINS_AFB_ATLAN_AFB} at 1,
        {located 27TH_F15_byair DOBBINS_AFB_ATLAN_AFB} at 1,
        {located 27TH_F15_bysea DOBBINS_AFB_ATLAN_AFB} at 1,
        {located 42ND_F15 EGLIN_AFB_AFB} at 1,
        {located 42ND_F15_byair EGLIN_AFB_AFB} at 1,
        {located 42ND_F15_bysea EGLIN_AFB_AFB} at 1,
        {located 43RD_F15 EGLIN_AFB_AFB} at 1,
        {located 43RD_F15_byair EGLIN_AFB_AFB} at 1,
        {located 43RD_F15_bysea EGLIN_AFB_AFB} at 1;
end_task;

task Deter_Border_Incursions_and_Provide_Defence;
  nodes 1 start,
        2 finish,
        3 action {deter_border_incursion coa_1},
        4 action {provide_territorial_defense coa_1};

  orderings 1 ---> 3, 1 ---> 4, 3 ---> 2, 4 ---> 2;

  effects
        {protected_ti coa_1} at 2,                 ;;; overall task requirement
        {border_incursion_deterred coa_1} at 3,    ;;; effect of 1st mission 
        {territory_defense_provided coa_1} at 4,   ;;; effect of 2nd mission 
        {located fastship norfolk_port} at 1,
        {located C5 mcdill_afb} at 1,
        {located C130 mcdill_afb} at 1,
        {key_terrain_status jundubah coa_1} = unoccupied at 1,
        {loc_protected_status tunis_jundubah coa_1} = unprotected at 1,
        {harbor_defended_status tunis_port coa_1} = unprotected at 1,
        {sloc_protected_status sloc_sicily_ntunisia coa_1} = unprotected at 1,
        {naval_patrol_status ntunisian_coast coa_1} = unprotected at 1,

        {apportioned_forces_status 57th_IMF} = unallocated at 1,
        {apportioned_forces_status 3rd_ACR} = unallocated at 1,
        {apportioned_forces_status 107th_ACR} = unallocated at 1,
        {apportioned_forces_status 116th_ACR} = unallocated at 1,
        {apportioned_forces_status CVN71_ACN} = unallocated at 1,
        {apportioned_forces_status CV66_ACS} = unallocated at 1,
        {apportioned_forces_status CV67_ACS} = unallocated at 1,
        {apportioned_forces_status BB62_SAG} = unallocated at 1,
        {apportioned_forces_status 16th_F15E} = unallocated at 1,
        {apportioned_forces_status 27th_F15} = unallocated at 1,
        {apportioned_forces_status 42nd_F15} = unallocated at 1,
        {apportioned_forces_status 43rd_F15} = unallocated at 1,

        {located 57TH_IMF ROTA_POL} at 1,
        {located 57TH_IMF_%byair ROTA_POL} at 1,
        {located 57TH_IMF_%bysea ROTA_POL} at 1,
        {located 57TH_IMF_bysea ROTA_POL} at 1,
        {located 3RD_ACR FT_BLISS_LAND} at 1,
        {located 3RD_ACR_%byair FT_BLISS_LAND} at 1,
        {located 3RD_ACR_%bysea FT_BLISS_LAND} at 1,
        {located 3RD_ACR_bysea  FT_BLISS_LAND} at 1,
        {located 107TH_ACR FT_MEADE_PORT} at 1,
        {located 107TH_ACR_%byair FT_MEADE_PORT} at 1,
        {located 107TH_ACR_%bysea FT_MEADE_PORT} at 1,
        {located 107TH_ACR_bysea FT_MEADE_PORT} at 1,
        {located 116TH_ACR GRAY_AAF_FT_LEWIS_AFB} at 1,
        {located 116TH_ACR_%byair GRAY_AAF_FT_LEWIS_AFB} at 1,
        {located 116TH_ACR_%bysea GRAY_AAF_FT_LEWIS_AFB} at 1,
        {located 116TH_ACR_bysea GRAY_AAF_FT_LEWIS_AFB} at 1,
        {located CVN71_ACN ROTA_POL} at 1,
        {located CV66_ACS ROTA_POL} at 1,
        {located BB62_SAG ROTA_POL} at 1,
        {located CV67_ACS NORFOLK_SHYD} at 1,
        {located 16TH_F15E SHAW_AFB_AFB} at 1,        
	{located 16TH_F15E_byair SHAW_AFB_AFB} at 1,
        {located 16TH_F15E_bysea SHAW_AFB_AFB} at 1,
        {located 27TH_F15 DOBBINS_AFB_ATLAN_AFB} at 1,
        {located 27TH_F15_byair DOBBINS_AFB_ATLAN_AFB} at 1,
        {located 27TH_F15_bysea DOBBINS_AFB_ATLAN_AFB} at 1,
        {located 42ND_F15 EGLIN_AFB_AFB} at 1,
        {located 42ND_F15_byair EGLIN_AFB_AFB} at 1,
        {located 42ND_F15_bysea EGLIN_AFB_AFB} at 1,
        {located 43RD_F15 EGLIN_AFB_AFB} at 1,
        {located 43RD_F15_byair EGLIN_AFB_AFB} at 1,
        {located 43RD_F15_bysea EGLIN_AFB_AFB} at 1;
end_task;

task Provide_Territorial_Defence;
  nodes 1 start,
        2 finish,
        3 action {provide_territorial_defense coa_1};

  orderings 1 ---> 3, 3 ---> 2;

  effects
        {protected_ti coa_1} at 2,                 ;;; overall task requirement
        {territory_defense_provided coa_1} at 3,   ;;; effect of the mission 
        {located fastship norfolk_port} at 1,
        {located C5 mcdill_afb} at 1,
        {located C130 mcdill_afb} at 1,
        {key_terrain_status jundubah coa_1} = unoccupied at 1,
        {loc_protected_status tunis_jundubah coa_1} = unprotected at 1,
        {harbor_defended_status tunis_port coa_1} = unprotected at 1,
        {sloc_protected_status sloc_sicily_ntunisia coa_1} = unprotected at 1,
        {naval_patrol_status ntunisian_coast coa_1} = unprotected at 1,

        {apportioned_forces_status 57th_IMF} = unallocated at 1,
        {apportioned_forces_status 3rd_ACR} = unallocated at 1,
        {apportioned_forces_status 107th_ACR} = unallocated at 1,
        {apportioned_forces_status 116th_ACR} = unallocated at 1,
        {apportioned_forces_status CVN71_ACN} = unallocated at 1,
        {apportioned_forces_status CV66_ACS} = unallocated at 1,
        {apportioned_forces_status CV67_ACS} = unallocated at 1,
        {apportioned_forces_status BB62_SAG} = unallocated at 1,
        {apportioned_forces_status 16th_F15E} = unallocated at 1,
        {apportioned_forces_status 27th_F15} = unallocated at 1,
        {apportioned_forces_status 42nd_F15} = unallocated at 1,
        {apportioned_forces_status 43rd_F15} = unallocated at 1,

        {located 57TH_IMF ROTA_POL} at 1,
        {located 57TH_IMF_%byair ROTA_POL} at 1,
        {located 57TH_IMF_%bysea ROTA_POL} at 1,
        {located 57TH_IMF_bysea ROTA_POL} at 1,
        {located 3RD_ACR FT_BLISS_LAND} at 1,
        {located 3RD_ACR_%byair FT_BLISS_LAND} at 1,
        {located 3RD_ACR_%bysea FT_BLISS_LAND} at 1,
        {located 3RD_ACR_bysea  FT_BLISS_LAND} at 1,
        {located 107TH_ACR FT_MEADE_PORT} at 1,
        {located 107TH_ACR_%byair FT_MEADE_PORT} at 1,
        {located 107TH_ACR_%bysea FT_MEADE_PORT} at 1,
        {located 107TH_ACR_bysea FT_MEADE_PORT} at 1,
        {located 116TH_ACR GRAY_AAF_FT_LEWIS_AFB} at 1,
        {located 116TH_ACR_%byair GRAY_AAF_FT_LEWIS_AFB} at 1,
        {located 116TH_ACR_%bysea GRAY_AAF_FT_LEWIS_AFB} at 1,
        {located 116TH_ACR_bysea GRAY_AAF_FT_LEWIS_AFB} at 1,
        {located CVN71_ACN ROTA_POL} at 1,
        {located CV66_ACS ROTA_POL} at 1,
        {located BB62_SAG ROTA_POL} at 1,
        {located CV67_ACS NORFOLK_SHYD} at 1,
        {located 16TH_F15E SHAW_AFB_AFB} at 1,        
	{located 16TH_F15E_byair SHAW_AFB_AFB} at 1,
        {located 16TH_F15E_bysea SHAW_AFB_AFB} at 1,
        {located 27TH_F15 DOBBINS_AFB_ATLAN_AFB} at 1,
        {located 27TH_F15_byair DOBBINS_AFB_ATLAN_AFB} at 1,
        {located 27TH_F15_bysea DOBBINS_AFB_ATLAN_AFB} at 1,
        {located 42ND_F15 EGLIN_AFB_AFB} at 1,
        {located 42ND_F15_byair EGLIN_AFB_AFB} at 1,
        {located 42ND_F15_bysea EGLIN_AFB_AFB} at 1,
        {located 43RD_F15 EGLIN_AFB_AFB} at 1,
        {located 43RD_F15_byair EGLIN_AFB_AFB} at 1,
        {located 43RD_F15_bysea EGLIN_AFB_AFB} at 1;
end_task;

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compute Conditions Required for Schemas
;;;
;;;
;;;		      REMOVED FOR TEST PURPOSES
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LEVEL 1: MILITARY ACTIONS
;;;
;;; Only two mission types operators have been described. These represent two
;;; levels of military operation at 1, a joint-show-of-force and a joint-ops-defense.
;;; Both achieve the goal of protecting the territorial integrity against some
;;; threat identfied by COA1.
;;; The scale of military operations is best represented by show-of-force at 1,
;;; quarantine at 1, blockade, joint-ops-defense, joint-ops-offense.
;;;
;;;
;;;
;;;		      REMOVED FOR TEST PURPOSES
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LEVEL 2:  Identify specific threats and locations
;;;
;;; Only two operators have been described for identfiying specific threats.
;;;
;;; Deter-border-incursion generates a sub-goal for every immed-threat-enemy
;;; predicate found. The idea is that for every enemy unit that presents an
;;; immediate threat for border incursion an appropriate friendly unit should
;;; be selected to deter it.
;;;
;;; Provide-territorial-defense generates sub-goals for every predicate found
;;; that lies on a predicted enemy avenue-of-approach and is also of the form:
;;;
;;;   key-terrain    - ground locations identified as key from OCOKA analysis
;;;   key-loc        - key ground lines-of-communication
;;;   key-base       - ground bases of operations (eg rear area of battle)
;;;   key-port       - key ports to defend
;;;   key-sloc       - key sea lines-of-communication
;;;   key-sea-sector - key sea-sectors to protect
;;;   key-airfield   - key airfields to defend (eg air-base-of-operations)
;;;   key-aloc       - key air lines-of-communication
;;;   key-air-sector - key air sectors to protect

schema provide_territorial_defense;
  vars ?urban1 = ?{type urban},
       ?urban2 = ?{type urban},
       ?route1 = ?{type route},
       ?seaport1 = ?{type sea_port},
       ?sea_loc1 = ?{type sea_location},
       ?sea_sector1 = ?{type sea_sector},
       ?airfield1 = ?{type airfield},
       ?air_loc1 = ?{type air_location},
       ?coa1 = ?{type course_of_action},
       ?air_sector1 = ?{type air_sector};

  expands {provide_territorial_defense ?coa1};

  nodes 1  dummy,
        2  action {counter_threat ?urban1 ?coa1},
	3  action {counter_threat ?route1 ?coa1},
	4  action {counter_threat ?urban2 ?coa1},

	5  action {counter_threat ?seaport1 ?coa1},
	6  action {counter_threat ?sea_loc1 ?coa1},
	7  action {counter_threat ?sea_sector1 ?coa1},

	8  action {counter_threat ?airfield1 ?coa1},
	9  action {counter_threat ?air_loc1 ?coa1},
	10 action {counter_threat ?air_sector1 ?coa1},
        11 dummy;

  orderings  1 ---> 2,  1 ---> 3,  1 ---> 4,  1 ---> 5,   1 ---> 6, 
             1 ---> 7,  1 ---> 8,  1 ---> 9,  1 ---> 10,
             2 ---> 11, 3 ---> 11, 4 ---> 11, 5 ---> 11,  6 ---> 11,
             7 ---> 11, 8 ---> 11, 9 ---> 11, 10 ---> 11;

  conditions only_use_if {key_terrain ?urban1 ?coa1} at 2,
             only_use_if {key_loc ?route1 ?coa1} at 2,
             only_use_if {key_base ?urban2 ?coa1} at 2,
             only_use_if {key_port ?seaport1 ?coa1} at 2,
             only_use_if {key_sloc ?sea_loc1 ?coa1} at 2,
             only_use_if {key_sea_sector ?sea_sector1 ?coa1} at 2,
             only_use_if {key_airfield ?airfield1 ?coa1} at 2,
             only_use_if {key_aloc ?air_loc1 ?coa1} at 2,
             only_use_if {key_air_sector ?air_sector1 ?coa1} at 2;

end_schema;

schema deter_border_incursion;
  vars ?coa1 = ?{type course_of_action},
       ?army1 = ?{type army_threat},
       ?route1 = ?{type route},
       ?navy1 = ?{type navy_threat},
       ?sea_loc1 = ?{type sea_location},
       ?air1 = ?{type airforce_threat},
       ?air_loc1 = ?{type air_location},
       ?end_time1, ?end_time2, ?end_time3;

  expands {deter_border_incursion ?coa1};

  nodes 1 dummy,
        2 action {deter_threat ?army1 ?coa1 ?end_time1},
        3 action {deter_threat ?navy1 ?coa1 ?end_time2},
        4 action {deter_threat ?air1 ?coa1 ?end_time3},
        5 dummy;

  orderings 1 ---> 2, 1 ---> 3, 1 ---> 4, 2 ---> 5, 3 ---> 5, 4 ---> 5;

  conditions  only_use_if {immed_threat_enemy ?army1 ?route1 ?coa1 ?end_time1} at 2,
              only_use_if {immed_threat_enemy ?navy1 ?sea_loc1 ?coa1 ?end_time2} at 2,
              only_use_if {immed_threat_enemy ?air1 ?air_loc1 ?coa1 ?end_time3} at 2;

  time_windows before 21~00:00 at end_of 2, ;;; deter the Alg1stArmBge by d-day
               before 15~00:00 at end_of 3, ;;; deter the AlgCorvette by d-6
               before 25~00:00 at end_of 4; ;;; deter the Alg1stMig23 by d+4
end_schema;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LEVEL 3:  Employment Planning
;;;
;;; Deterrence operators - deter border incursions by show of force
;;; Defense operators - set up full defense of territory

;;;;;;;;;; DETERRENCE OPERATORS

;;;;;;;;; SETUP Ground patrols near potential border incursion

schema deter_border_incursion_by_ground_patrol_1;

 vars ?army1 = ?{type army_defence},
      ?army2 = ?{type army_threat},
      ?coa1 = ?{type course_of_action},
      ?urban1 = ?{bound},
      ?urban2 = ?{type urban},
      ?region1 = ?{type region},
      ?region2 = ?{type region},
      ?route1 = ?{type route},
      ?end_time1, ?end_time3;

 vars_relations ?urban1 /= ?urban2;

 expands {deter_threat ?army2 ?coa1 ?end_time1};

 nodes 1 action {deploy ?army1 ?urban1 ?end_time1},
       2 action {traverse_terrain ?army1 ?urban1 ?urban2},
       3 action {ground_patrol ?army1 ?region1 ?coa1};

 orderings  1 ---> 2, 2 ---> 3;

 conditions only_use_if {immed_threat_enemy ?army2 ?route1 ?coa1 ?end_time1} at 1,

            unsupervised {apportioned_forces_status ?army1} = unallocated at 1,
	    unsupervised {firepower ?army1} = 65904 at 1,
	    unsupervised {terrain_type ?army1} = 4 at 1,
	    unsupervised {mobility ?army1} = 3 at 1,
	    unsupervised {type_size ?army1} =  4 at 1,

            only_use_if {d_day ?end_time3} at 1,
            only_use_if {located ?army2 ?region2 ?coa1} at 1,
            only_use_if {adjacent_territory ?region2 ?region1} at 1,
            only_use_if {base_approval ?urban1 ?coa1} at 1,
            only_use_if {located_within ?urban2 ?region1} at 1,
            only_use_if {near_territory ?urban2 ?route1} at 1;


 effects {apportioned_forces_status ?army1} = allocated at begin_of 1,
         {ground_moved ?army1 ?urban1 ?urban2} at 2,
         {ground_patrols ?army1 ?region1 ?coa1} at 3;

end_schema;

;;;
;;;;;;;;;; SETUP Naval Patrols near potential naval zone incursion
;;;

schema deter_naval_zone_incursion_by_naval_patrol_1;

 vars ?navy1 = ?(type navy_defence),
      ?navy2 = ?{type navy_threat},
      ?coa1 = ?{type course_of_action},
      ?seaport1 = ?{type sea_port},
      ?sea_sector1 = ?{type sea_sector},
      ?sea_loc1 = ?{type sea_location},
      ?sea_sector2 = ?{type sea_sector},
      ?territory1 = ?{type territory},
      ?end_time1,
      ?end_time3;

 expands {deter_threat ?navy2 ?coa1 ?end_time1};

 nodes 1 action {deploy ?navy1 ?seaport1 ?end_time1},
       2 action {move_sea_sector ?navy1 ?seaport1 ?sea_sector1},
       3 action {naval_patrol ?navy1 ?sea_sector1 ?coa1 ?end_time1};

 orderings  1 ---> 2, 2 ---> 3;

 conditions only_use_if {immed_threat_enemy ?navy2 ?sea_loc1 ?coa1 ?end_time1} at 1,

            unsupervised {apportioned_forces_status ?navy1} = unallocated at 1,
	    unsupervised {sea_firepower ?navy1} = 5 at 1,
	    unsupervised {sea_mobility ?navy1} = 5 at 1,

            only_use_if {d_day ?end_time3} at 1,
            only_use_if {closest_port ?seaport1 ?territory1} at 1,
            only_use_if {located ?navy2 ?sea_sector2 ?coa1} at 1,
            only_use_if {adjacent_territory ?sea_sector1 ?sea_sector2} at 1;

 effects {apportioned_forces_status ?navy1} = allocated at begin_of 1,
         {moved ?navy1 ?seaport1 ?sea_sector1} at 2,
         {naval_patrols ?navy1 ?sea_sector1 ?coa1} at 3;

end_schema;

;;;
;;;;;;;;;; SETUP Combat Air Patrol near potential airspace incursion
;;;

schema deter_airspace_incursion_by_cap_1;

 vars ?tfighter1 = ?(type airforce_defence),
      ?coa1 = ?{type course_of_action},
      ?airfield1 = ?{type airfield},
      ?air_loc1 = ?{type air_location},
      ?air_sector1 = ?{type air_sector},
      ?air_sector2 = ?{type air_sector},
      ?air2 = ?(type airforce_threat),
      ?end_time3, ?end_time1;

 expands {deter_threat ?air2 ?coa1 ?end_time1};

 nodes 1 action {deploy ?tfighter1 ?airfield1 ?end_time1},
       2 action {cap ?tfighter1 ?air_sector1 ?coa1 ?end_time1};

 orderings 1 ---> 2;

 conditions only_use_if {immed_threat_enemy ?air2 ?air_loc1 ?coa1 ?end_time1} at 1,

            unsupervised {apportioned_forces_status ?tfighter1} = unallocated at 1,
            unsupervised {air_firepower ?tfighter1} = 360 at 1,

            only_use_if {d_day ?end_time3} at 1,
            only_use_if {air_base_approval ?airfield1 ?coa1} at 1,
            only_use_if {near_territory ?air_sector1 ?air_loc1} at 1,
            only_use_if {located ?air2 ?air_sector2 ?coa1} at 1,
            only_use_if {adjacent_territory ?air_sector2 ?air_sector1} at 1;
            

 effects {apportioned_forces_status ?tfighter1} = allocated at begin_of 1,
         {cap_provided ?tfighter1 ?air_sector1 ?coa1} at 2;

end_schema;

;;;
;;;;;;;;;; DEFENSE OPERATORS
;;;
;;;
;;;;;;;;; OCCUPY Key Terrain within territory to be protected
;;;

schema provide_defense_by_key_terrain_1;

 vars ?army1 = ?(type army_defence),
      ?urban1 = ?{type urban},
      ?route1 = ?{type route},
      ?route2 = ?{type route},
      ?urban2 = ?{type urban},
      ?army2 = ?{type army_threat},
      ?coa1 = ?{type course_of_action},
      ?end_time1,
      ?end_time3;

 expands {counter_threat ?urban2 ?coa1};

 nodes 1 action {deploy ?army1 ?urban1 ?end_time1},
       2 action {traverse_terrain ?army1 ?urban1 ?urban2},
       3 action {occupy_key_terrain ?army1 ?urban2 ?coa1 ?end_time1},
       4 action {protect_loc ?army1 ?route1 ?coa1 ?end_time1},
       5 dummy;

 orderings  1 ---> 2, 2 ---> 3, 2 ---> 4, 3 ---> 5, 4 ---> 5;

 conditions only_use_if {threat_enemy ?army2 ?route2 ?coa1 ?end_time1} at 1,

            unsupervised {apportioned_forces_status ?army1} = unallocated at 1,
	    unsupervised {firepower ?army1} = 46720 at 1,
	    unsupervised {type_size ?army1} = 4 at 1,	    
            unsupervised {terrain_type ?army1} = 4 at 1,

            only_use_if {d_day ?end_time3} at 1,
            only_use_if {key_terrain ?urban2 ?coa1} at 1,
            only_use_if {near_territory ?urban2 ?route2} at 1,
	    only_use_if {overlaps ?route1 ?route2} at 1,
            only_use_if {base_approval ?urban1 ?coa1} at 1,
            only_use_if {key_terrain_status ?urban2 ?coa1} = unoccupied at 3;

 effects {apportioned_forces_status ?army1} = allocated at begin_of 1,
         {ground_moved ?army1 ?urban1 ?urban2} at 2,
         {key_terrain_status ?urban2 ?coa1} = occupied at begin_of 3,
         {key_terrain_occupied ?army1 ?urban2 ?coa1} at 3,
         {loc_protected ?army1 ?route1 ?coa1} at 4;

end_schema;

;;;
;;;;;;;;; PROTECT LOCs within territory to be protected
;;;

schema provide_defense_by_protect_loc_1;

 vars ?army1 = ?(type army_defence),
      ?urban1 = ?{type urban},
      ?urban2 = ?{type urban},
      ?urban3 = ?{type urban},
      ?route1 = ?{type route},
      ?route2 = ?{type route},
      ?route3 = ?{type route},
      ?army2 = ?{type army_threat},
      ?coa1 = ?{type course_of_action},
      ?end_time1,
      ?end_time3;

 expands {counter_threat ?route1 ?coa1};

 nodes 1 action {deploy ?army1 ?urban1 ?end_time1},
       2 action {traverse_terrain ?army1 ?urban1 ?urban2},
       3 action {protect_loc ?army1 ?route1 ?coa1 ?end_time1};

 orderings  1 ---> 2, 2 ---> 3;

 conditions only_use_if {threat_enemy ?army2 ?route2 ?coa1 ?end_time1} at 1,

            unsupervised {apportioned_forces_status ?army1} = unallocated at 1,
            unsupervised {firepower ?army1} =  65904 at 1,
	    unsupervised {type_size ?army1} = 4 at 1,	    
            unsupervised {terrain_type ?army1} = 4 at 1,

            only_use_if {d_day ?end_time3} at 1,
            only_use_if {key_loc ?route1 ?coa1} at 1,
            only_use_if {overlaps ?route1 ?route2} at 1,
            only_use_if {base_approval ?urban1 ?coa1} at 1,
            only_use_if {route_loc ?urban3 ?urban2 ?route3} at 1,
            only_use_if {loc_protected_status ?route1 ?coa1} = unprotected at 3;

 effects {apportioned_forces_status ?army1} = allocated at begin_of 1,
	 {ground_moved ?army1 ?urban1 ?urban2} at 2,
         {loc_protected_status ?route1 ?coa1} = protected at begin_of 3,
         {loc_protected ?army1 ?route1 ?coa1} at 3;

end_schema;


;;;
;;;;;;;;; SETUP Base within territory to be protected
;;;;;;;;;; POSITION Reserves within territory to be protected
;;;

schema provide_defense_by_base_reserves_1;

 vars ?urban1 = ?{type urban},
      ?army1 = ?{type army_defence},
      ?army2 = ?{type army_threat},
      ?coa1 = ?{type course_of_action},
      ?end_time3,
      ?end_time1;

 expands {counter_threat ?urban1 ?coa1};

 nodes 1 action {deploy ?army1 ?urban1 ?end_time1},
       2 action {setup_base ?army1 ?urban1 ?coa1 ?end_time1},
       3 action {locate_reserves ?army1 ?urban1 ?coa1 ?end_time1},
       4 dummy;

 orderings 1 ---> 2, 1 ---> 3, 2 ---> 4, 3 ---> 4;
 
 conditions only_use_if {d_day ?end_time3} at 1,
            only_use_if {threat_enemy_reserves ?army2 ?coa1 ?end_time1} at 1,
            only_use_if {key_base ?urban1 ?coa1} at 1,
            unsupervised {apportioned_forces_status ?army1} = unallocated at 1;

 effects {apportioned_forces_status ?army1} = allocated at begin_of 1,
         {base_setup ?urban1 ?coa1} at 2,
         {reserves_located ?army1 ?urban1 ?coa1} at 3;

end_schema;

;;;
;;;;;;;; PROVIDE harbor defense within territory to be protected
;;;

schema provide_harbor_defense_1;

 vars ?navy1 = ?(type navy_defence),
      ?seaport1 = ?{type sea_port},
      ?seaport2 = ?{type sea_port},
      ?sea_loc1 = ?{type sea_location},
      ?territory1 = ?{type territory},
      ?navy2 = ?{type navy_threat},
      ?coa1 = ?{type course_of_action},
      ?end_time1,
      ?end_time3;

 expands {counter_threat ?seaport1 ?coa1};

 nodes 1 action {deploy ?navy1 ?seaport2 ?end_time1},
       2 action {defend_harbor ?navy1 ?seaport1 ?coa1 ?end_time1};

 orderings 1 ---> 2;

 conditions only_use_if {threat_enemy ?navy2 ?sea_loc1 ?coa1 ?end_time1} at 1,

	    unsupervised {apportioned_forces_status ?navy1} = unallocated at 1,
            unsupervised {sea_firepower ?navy1} = 6 at 1,

            only_use_if {d_day ?end_time3} at 1,
            only_use_if {key_port ?seaport1 ?coa1} at 1,
            only_use_if {closest_port ?seaport2 ?territory1} at 1,
            only_use_if {route_sloc ?seaport2 ?seaport1 ?sea_loc1} at 1,
            only_use_if {harbor_defended_status ?seaport1 ?coa1} = unprotected at 2;

 effects {apportioned_forces_status ?navy1} = allocated at begin_of 1,
         {harbor_defended ?navy1 ?seaport1 ?coa1} at 2,
         {harbor_defended_status ?seaport1 ?coa1} = protected at begin_of 2;

end_schema;

;;;
;;;;;;;; PROTECT slocs to/from territory to be protected
;;;

schema provide_defense_by_protect_sloc_1;

 vars ?navy1 = ?(type navy_defence),
      ?seaport1 = ?{type sea_port},
      ?seaport2 = ?{type sea_port},
      ?seaport3 = ?{type sea_port},
      ?sea_loc1 = ?{type sea_location},
      ?territory1 = ?{type territory},
      ?navy2 = ?{type navy_threat},
      ?coa1 = ?{type course_of_action},
      ?end_time1,
      ?end_time3;

 expands {counter_threat ?sea_loc1 ?coa1};

 nodes 1 action {deploy ?navy1 ?seaport2 ?end_time1},
       2 action {protect_sloc ?navy1 ?sea_loc1 ?coa1 ?end_time1};

 orderings 1 ---> 2;

 conditions only_use_if {threat_enemy ?navy2 ?sea_loc1 ?coa1 ?end_time1} at 1,

            unsupervised {apportioned_forces_status ?navy1} = unallocated at 1,
            unsupervised {sea_firepower ?navy1} = 6 at 1,

            only_use_if {d_day ?end_time3} at 1,
            only_use_if {key_sloc ?sea_loc1 ?coa1} at 1,
            only_use_if {closest_port ?seaport2 ?territory1} at 1,
            only_use_if {route_sloc ?seaport3 ?seaport1 ?sea_loc1} at 1,
            only_use_if {sloc_protected_status ?sea_loc1 ?coa1} = unprotected at 2;
            

 effects {apportioned_forces_status ?navy1} = allocated at begin_of 1,
         {sloc_protected ?navy1 ?sea_loc1 ?coa1} at 2,
         {sloc_protected_status ?sea_loc1 ?coa1} = protected at begin_of 2;

end_schema;


;;;
;;;;;;;; SETUP naval patrol within naval zones to be protected
;;;

schema provide_defense_by_naval_patrol_1;

 vars ?navy1 = ?(type navy_defence),
      ?seaport1 = ?{type sea_port},
      ?sea_sector1 = ?{type sea_sector},
      ?sea_loc1 = ?{type sea_location},
      ?territory1 = ?{type territory},
      ?navy2 = ?{type navy_threat},
      ?coa1 = ?{type course_of_action},
      ?end_time1,
      ?end_time3;

 expands {counter_threat ?sea_sector1 ?coa1};

 nodes 1 action {deploy ?navy1 ?seaport1 ?end_time1},
       2 action {move_sea_sector ?navy1 ?seaport1 ?sea_sector1},
       3 action {naval_patrol ?navy1 ?sea_sector1 ?coa1 ?end_time1};

 orderings 1 ---> 2, 2 ---> 3;

 conditions only_use_if {threat_enemy ?navy2 ?sea_loc1 ?coa1 ?end_time1} at 1,

            unsupervised {apportioned_forces_status ?navy1} = unallocated at 1,
	    unsupervised {sea_firepower ?navy1} = 6 at 1,
	    unsupervised {sea_mobility ?navy1} = 6 at 1,

            only_use_if {d_day ?end_time3} at 1,
            only_use_if {key_sea_sector ?sea_sector1 ?coa1} at 1,
            only_use_if {closest_port ?seaport1 ?territory1} at 1,
            only_use_if {near_territory ?sea_loc1 ?sea_sector1} at 1,
            only_use_if {naval_patrol_status ?sea_sector1 ?coa1} = unprotected at 3;
            

 effects {apportioned_forces_status ?navy1} = allocated at begin_of 1,
         {moved navy1 ?seaport1 ?sea_sector1} at 2,
         {naval_patrol_status ?sea_sector1 ?coa1} = protected at begin_of 3,
         {naval_patrol ?navy1 ?sea_sector1 ?coa1} at 3;

end_schema;


;;;
;;;;;;;;; PROVIDE dca within airspace to be protected
;;;;;;;;;; SETUP air base within territory to be protected
;;;

schema provide_defense_by_dca_intercept_1;

 vars ?tfighter1 = ?(type airforce_defence),
      ?airfield1 = ?{type airfield},
      ?coa1 = ?{type course_of_action},
      ?airspace1 = ?{type airspace},
      ?air2 = ?{type airforce_threat},
      ?end_time1,
      ?end_time3;

 expands {counter_threat ?airfield1 ?coa1};

 nodes 1 action {deploy ?tfighter1 ?airfield1 ?end_time1},
       2 action {air_intercept ?tfighter1 ?airfield1 ?airspace1 ?coa1 ?end_time1},
       3 action {setup_airbase ?tfighter1 ?airfield1 ?coa1 ?end_time1},
       4 dummy;

 orderings 1 ---> 2, 1 ---> 3, 2 ---> 4, 3 ---> 4;

 conditions only_use_if {threat_enemy ?air2 ?airspace1 ?coa1 ?end_time1} at 1,

            unsupervised {apportioned_forces_status ?tfighter1} = unallocated at 1,
	    unsupervised {air_firepower ?tfighter1} = 360 at 1,
            unsupervised {a_range ?tfighter1} = 500 at 1,
	    
            only_use_if {d_day ?end_time3} at 1,
            only_use_if {key_airfield ?airfield1 ?coa1} at 1,
            only_use_if {located_within ?airfield1 ?airspace1} at 1;

 effects    {apportioned_forces_status ?tfighter1} = allocated at begin_of 1,
            {dca_provided ?tfighter1 ?airfield1 ?airspace1 ?coa1} at 2,
            {base_setup ?airfield1 ?coa1} at 3;

end_schema;


;;;
;;;;;;;;; PROTECT AIR LOCs within territory to be protected
;;;

schema provide_defense_by_protect_aloc_1;

 vars ?tfighter1 = ?(type airforce_defence),
      ?air_loc1 = ?{type air_location},
      ?coa1 = ?{type course_of_action},
      ?airfield1 = ?{type airfield},
      ?airfield2 = ?{type airfield},
      ?air2 = ?{type airforce_threat},
      ?end_time1,
      ?end_time3;

 expands {counter_threat ?air_loc1 ?coa1};

 nodes 1 action {deploy ?tfighter1 ?airfield2 ?end_time1},
       2 action {protect_aloc ?tfighter1 ?air_loc1 ?coa1 ?end_time1};

 orderings 1 ---> 2;

 conditions only_use_if {threat_enemy ?air2 ?air_loc1 ?coa1 ?end_time1} at 1,

            unsupervised {apportioned_forces_status ?tfighter1} = unallocated at 1,
	    unsupervised {air_firepower ?tfighter1} = 360  at 1,

            only_use_if {d_day ?end_time3} at 1,
            only_use_if {key_aloc ?air_loc1 ?coa1} at 1,
            only_use_if {air_base_approval ?airfield2 ?coa1} at 1,
            only_use_if {route_aloc ?airfield1 ?airfield2 ?air_loc1} at 1;

 effects {apportioned_forces_status ?tfighter1} = allocated at begin_of 1,
         {aloc_protected ?tfighter1 ?air_loc1 ?coa1} at 2;

end_schema;


;;;
;;;;;;;; SETUP cap within airspace to be protected
;;;

schema provide_defense_by_cap_1;

 vars ?tfighter1 = ?(type airforce_defence),
      ?air_sector1 = ?{type air_sector},
      ?air_loc1 = ?{type air_location},
      ?coa1 = ?{type course_of_action},
      ?airfield1 = ?{type airfield},
      ?air2 = ?{type airforce_threat},
      ?end_time1,
      ?end_time3;

 expands {counter_threat ?air_sector1 ?coa1};

 nodes 1 action {deploy ?tfighter1 ?airfield1 ?end_time1},
       2 action {cap ?tfighter1 ?air_sector1 ?coa1 ?end_time1};

 orderings 1 ---> 2;

 conditions only_use_if {threat_enemy ?air2 ?air_loc1 ?coa1 ?end_time1} at 1,

            unsupervised {apportioned_forces_status ?tfighter1} = unallocated at 1,
	    unsupervised {air_firepower ?tfighter1} = 360 at 1,

            only_use_if {d_day ?end_time3} at 1,
            only_use_if {key_air_sector ?air_sector1 ?coa1} at 1,
            only_use_if {air_base_approval ?airfield1 ?coa1} at 1,
            only_use_if {near_territory ?air_sector1 ?air_loc1} at 1;


 effects {apportioned_forces_status ?tfighter1} = allocated at begin_of 1,
         {cap_provided ?tfighter1 ?air_sector1 ?coa1} at 2;

end_schema;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LEVEL 4:  Add deployment actions for all units
;;;
;;; Deployment operators for special ops, army, navy, marine and air-force.
;;;
;;; Deployment operator for SOF
;;;      100% non-NAT goes by AIR
;;;      100% NAT     goes by SEA
;;; operator required for RANGER and AIRBORNE division
;;;

schema deploy_sof;
 vars ?special1 = ?{type special},
      ?location2 = ?{type location},
      ?location1 = ?{type location},
      ?airfield1 = ?{type airfield},
      ?airfield2 = ?{type airfield},
      ?cargobyair1  = ?{type cargobyair},
      ?cargobysea1 = ?{type cargobysea},
      ?seaport1 = ?{type sea_port},
      ?seaport2 = ?{type sea_port},
      ?sea_loc1 = ?{type sea_location},
      ?air_loc1 = ?{type air_location},
      ?end_time1;

 vars_relations ?airfield2 /= ?airfield1,
                ?seaport2  /= ?seaport1;

 expands {deploy ?special1 ?location2 ?end_time1};

 nodes 1 action {ground_move ?cargobyair1 ?location1 ?airfield1},
       2 action {move ?cargobyair1 ?airfield1 ?airfield2},
       3 action {ground_move ?cargobyair1 ?airfield2 ?location2},
       4 action {ground_move ?cargobysea1 ?location1 ?seaport1},
       5 action {move ?cargobysea1 ?seaport1 ?seaport2},
       6 action {ground_move ?cargobysea1 ?seaport2 ?location2},
       7 action {mobilize ?special1 ?location1};

 orderings 1 ---> 2, 2 ---> 3,  3 ---> 4,  4 ---> 5,  5 ---> 6,  6 ---> 7;

 conditions only_use_if {located ?special1 ?location1} at 1,
            only_use_if {near_territory ?airfield1 ?location1} at 1,
            only_use_if {near_territory ?seaport1 ?location1} at 1,
            only_use_if {partition_force ?special1 ?cargobyair1 ?cargobysea1} at 1,
            only_use_if {near_territory ?airfield2 ?location2} at 1,
            only_use_if {near_territory ?seaport2 ?location2} at 1,

            only_use_if {transit_approval ?airfield2} at 1,
            only_use_if {transit_approval ?seaport2} at 1,
            only_use_if {route_aloc ?airfield1 ?airfield2 ?air_loc1} at 1,
            only_use_if {route_sloc ?seaport1 ?seaport2 ?sea_loc1} at 1;

end_schema;

;;;
;;;  ARMY  only
;;;      20% non-NAT  goes by AIR
;;;      80% non-NAT  goes by SEA
;;;      100% NAT     goes by SEA
;;;

schema deploy_army;
 vars ?army1 = ?{type army_defence},
      ?location2 = ?{type location},
      ?location1 = ?{type location},
      ?airfield1 = ?{type airfield},
      ?airfield2 = ?{type airfield},
      ?cargobyair1  = ?{type cargobyair},
      ?cargobysea1 = ?{type cargobysea},
      ?cargobysea2 = ?{type cargobysea},
      ?seaport1 = ?{type sea_port},
      ?seaport2 = ?{type sea_port},
      ?sea_loc1 = ?{type sea_location},
      ?air_loc1 = ?{type air_location},
      ?end_time1;

 vars_relations ?airfield2 /= ?airfield1,
                ?seaport2  /= ?seaport1;

 expands {deploy ?army1 ?location2 ?end_time1};

 nodes 1  dummy,
       2  action {ground_move ?cargobyair1 ?location1 ?airfield1} ,
       3  action {move ?cargobyair1 ?airfield1 ?airfield2} ,
       4  action {ground_move ?cargobyair1 ?airfield2 ?location2} ,
       5  action {ground_move ?cargobysea1 ?location1 ?seaport1} ,
       6  action {move ?cargobysea1 ?seaport1 ?seaport2} ,
       7  action {ground_move ?cargobysea1 ?seaport2 ?location2} ,
       8  action {ground_move ?cargobysea2 ?location1 ?seaport1} ,
       9  action {move ?cargobysea2 ?seaport1 ?seaport2} ,
       10 action {ground_move ?cargobysea2 ?seaport2 ?location2},
       11 action {mobilize ?army1 ?location1};

 orderings 1 ---> 2, 2 ---> 3,  3 ---> 4, 4 ---> 11,  
           1 ---> 5, 5 ---> 6,  6 ---> 7, 7 ---> 11,
           1 ---> 8, 8 ---> 9,  9 ---> 10, 10 ---> 11;

 conditions only_use_if {located ?army1 ?location1} at 1,
            only_use_if {near_territory ?airfield1 ?location1} at 1,
            only_use_if {near_territory ?seaport1 ?location1} at 1,
            only_use_if {partition_force ?army1 ?cargobyair1 ?cargobysea1 ?cargobysea2} at 1,
            only_use_if {near_territory ?airfield2 ?location2} at 1,
            only_use_if {near_territory ?seaport2 ?location2} at 1,

            only_use_if {transit_approval ?airfield2} at 1,
            only_use_if {transit_approval ?seaport2} at 1,
            only_use_if {route_aloc ?airfield1 ?airfield2 ?air_loc1} at 1,
            only_use_if {route_sloc ?seaport1 ?seaport2 ?sea_loc1} at 1;

end_schema;

;;;
;;;  AIR FORCE  only
;;;      100% air-transportable carg goes by AIR
;;;      100% NAT                    goes by SEA
;;;

schema deploy_airforce;
 vars ?air1 = ?{type airforce_defence},
      ?location1 = ?{type location},
      ?airfield1 = ?{type airfield},
      ?airfield2 = ?{type airfield},
      ?cargobyair1  = ?{type cargobyair},
      ?cargobysea1 = ?{type cargobysea},
      ?seaport1 = ?{type sea_port},
      ?seaport2 = ?{type sea_port},
      ?sea_loc1 = ?{type sea_location},
      ?air_loc1 = ?{type air_location},
      ?end_time1;

 vars_relations ?airfield2 /= ?airfield1,
                ?seaport2  /= ?seaport1;

 expands {deploy ?air1 ?airfield2 ?end_time1};

 nodes 1 dummy,
       2 action {ground_move ?cargobyair1 ?location1 ?airfield1},
       3 action {move ?cargobyair1 ?airfield1 ?airfield2},
       4 action {ground_move ?cargobysea1 ?location1 ?seaport1},
       5 action {move ?cargobysea1 ?seaport1 ?seaport2},
       6 action {ground_move ?cargobysea1 ?seaport2 ?airfield2},
       7 action {mobilize ?air1 ?location1};

 orderings 1 ---> 2, 2 ---> 3,  3 ---> 7,  
           1 ---> 4, 4 ---> 5,  5 ---> 6, 6 ---> 7;

 conditions only_use_if {located ?air1 ?location1} at 1,
            only_use_if {near_territory ?airfield1 ?location1} at 1,
            only_use_if {near_territory ?seaport1 ?location1} at 1,
            only_use_if {partition_force ?air1 ?cargobyair1 ?cargobysea1} at 1,
            only_use_if {near_territory ?seaport2 ?airfield2} at 1,

            only_use_if {transit_approval ?airfield2} at 1,
            only_use_if {transit_approval ?seaport2} at 1,
            only_use_if {route_aloc ?airfield1 ?airfield2 ?air_loc1} at 1,
            only_use_if {route_sloc ?seaport1 ?seaport2 ?sea_loc1} at 1;

end_schema;

;;;
;;; Deploy Navy and Marines
;;;       ALL cargo goes by sea
;;;       100% non-NAT & NAT goes by sea
;;;

schema deploy_navy_1;
 vars ?navy1 = ?{type navy_defence},
      ?location1 = ?{type location},
      ?seaport1 = ?{type sea_port},
      ?seaport2 = ?{type sea_port},
      ?sea_loc1 = ?{type sea_location},
      ?end_time1;

 vars_relations ?seaport2  /= ?seaport1;

 expands {deploy ?navy1 ?seaport2 ?end_time1};

 nodes 1 action {ground_move ?navy1 ?location1 ?seaport1},
       2 action {move ?navy1 ?seaport1 ?seaport2},
       3 action {mobilize ?navy1 ?location1};

 orderings 1 ---> 2, 2 ---> 3;

 conditions only_use_if {located ?navy1 ?location1} at 1,
            only_use_if {near_territory ?seaport1 ?location1} at 1,
            only_use_if {transit_approval ?seaport2} at 1,
            only_use_if {route_sloc ?seaport1 ?seaport2 ?sea_loc1} at 1;

end_schema;

;;;
;;; Deploy Navy and Marines
;;;       ALL cargo goes by sea
;;;       100% non-NAT & NAT goes by sea
;;;

schema deploy_navy_2;
 vars ?navy1 = ?{type navy_defence},
      ?sea1 = ?{type sea},
      ?sea2 = ?{type sea},
      ?seaport2 = ?{type sea_port},
      ?sea_loc1 = ?{type sea_location},
      ?end_time1;

 expands {deploy ?navy1 ?seaport2 ?end_time1};

 nodes 1 action {ground_move ?navy1 ?sea1 ?sea2},
       2 action {move ?navy1 ?sea2 ?seaport2},
       3 action {mobilize ?navy1 ?sea1};

 orderings 1 ---> 2, 2 ---> 3;

 conditions only_use_if {located ?navy1 ?sea1} at 1,
            only_use_if {near_territory ?sea2 ?sea1} at 1,
            only_use_if {transit_approval ?seaport2} at 1,
            only_use_if {route_sloc ?sea2 ?seaport2 ?sea_loc1} at 1;
end_schema;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LEVEL 5&6:  Complete deployment planning
;;;
;;; Set of operators for moving cargo by land, sea and air.
;;; One operator moves air-transportable cargo by mixture of strategic airlift,
;;; tactical airlift and sealift and also selects an appropriate intermediate
;;; location for troop transit and staging.

schema move_by_sairlift_1;
 vars ?cargobyair1 = ?{type cargobyair},
      ?airfield1 = ?{type airfield},
      ?airfield2 = ?{type airfield},
      ?sairlift1 = ?{type sairlift},
      ?duration1;

 expands {move ?cargobyair1 ?airfield1 ?airfield2};

 nodes 1 action {move_by_sairlift ?cargobyair1 ?airfield1 ?airfield2 ?sairlift1 ?duration1};

 conditions only_use_if {airlift_duration1 ?sairlift1 ?cargobyair1 ?airfield1 ?airfield2} = ?duration1 at 1,
            only_use_if {apportioned_forces ?sairlift1} at 1,
            only_use_if {runway_length ?airfield2} = 9500 at 1;

 effects {moved ?cargobyair1 ?airfield1 ?airfield2};
end_schema;

schema move_by_airlift_and_sealift;
 vars ?cargobyair1 = ?{type cargobyair},
      ?airfield1 = ?{type airfield},
      ?airfield2 = ?{type airfield},
      ?airfield3 = ?{type airfield},
      ?seaport1 = ?{type sea_port},
      ?seaport2 = ?{type sea_port},
      ?location1 = ?{type location},
      ?sairlift1 = ?{type sairlift},
      ?tairlift1 = ?{type tairlift},
      ?ssealift1 = ?{type ssealift},
      ?duration1, ?duration2, ?duration3;

 vars_relations ?airfield2 /= ?airfield1,
                ?airfield2 /= ?airfield3,
                ?seaport1 /= ?seaport2;

 expands {move ?cargobyair1 ?airfield1 ?airfield3};

 nodes 1 action {move_by_sairlift ?cargobyair1 ?airfield1 ?airfield2 ?sairlift1 ?duration1},
       2 action {ground_move ?cargobyair1 ?airfield2 ?location1},
       3 action {ground_move ?cargobyair1 ?location1 ?airfield2},
       4 action {ground_move ?cargobyair1 ?location1 ?seaport1},
       5 action {move_by_tairlift ?cargobyair1 ?airfield2 ?airfield3 ?tairlift1 ?duration2},
       6 action {move_by_sealift ?cargobyair1 ?seaport1 ?seaport2 ?ssealift1 ?duration3},
       7 action {ground_move ?cargobyair1 ?seaport2 ?airfield3};

 orderings 1 ---> 2, 2 ---> 3, 2 ---> 4, 3 ---> 5, 4 ---> 6, 6 ---> 7;

 conditions only_use_if {airlift_duration1 ?sairlift1 ?cargobyair1 ?airfield1 ?airfield2} = ?duration1 at 1,
            only_use_if {airlift_duration2 ?tairlift1 ?cargobyair1 ?airfield2 ?airfield3} = ?duration2 at 1,
            only_use_if {ssealift_duration2 ?ssealift1 ?cargobyair1 ?seaport1 ?seaport2} = ?duration3 at 1,

            only_use_if {transit_approval ?location1} at 1,
            only_use_if {transit_approval ?airfield2} at 1,
            only_use_if {apportioned_forces ?sairlift1} at 1,
            only_use_if {apportioned_forces ?tairlift1} at 1,
            only_use_if {apportioned_forces ?ssealift1} at 1,
            only_use_if {near_territory ?seaport1 ?location1} at 1,
            only_use_if {near_territory ?airfield2 ?location1} at 1,
            only_use_if {near_territory ?seaport2 ?airfield3} at 1,

	    only_use_if {runway_length ?airfield2} = 9500 at 1,
	    only_use_if {runway_length ?airfield3} = 6000 at 1;

 effects {moved ?cargobyair1 ?airfield1 ?airfield2} at 1,
         {moved ?cargobyair1 ?airfield2 ?airfield3} at 2,
         {moved ?cargobyair1 ?seaport1 ?seaport2} at 3;

end_schema;

schema move_by_tairlift_1;
 vars ?cargobyair1 = ?{type cargobyair},
      ?airfield1 = ?{type airfield},
      ?airfield2 = ?{type airfield},
      ?duration1,
      ?tairlift1 = ?{type tairlift};

 expands {move ?cargobyair1 ?airfield1 ?airfield2};

 nodes 1 action {move_by_tairlift ?cargobyair1 ?airfield1 ?airfield2 ?tairlift1 ?duration1};

 conditions only_use_if {airlift_duration2 ?tairlift1 ?cargobyair1 ?airfield1 ?airfield2} = ?duration1 at 1,
	    only_use_if {runway_length ?airfield2} = 6000 at 1,
            only_use_if {apportioned_forces ?tairlift1} at 1;

 effects {moved ?cargobyair1 ?airfield1 ?airfield2} at 1;
end_schema;

schema move_by_sealift_1;
 vars ?cargobysea1 = ?{type cargobysea},
      ?seaport2 = ?{type sea_port},
      ?seaport1 = ?{type sea_port},
      ?duration1,
      ?ssealift1 = ?{type ssealift};

 expands {move ?cargobysea1 ?seaport1 ?seaport2};

 nodes 1 action {move_by_sealift ?cargobysea1 ?seaport1 ?seaport2 ?ssealift1 ?duration1};

 conditions only_use_if {sealift_duration2 ?ssealift1 ?cargobysea1 ?seaport1 ?seaport2} = ?duration1 at 1,
	    only_use_if {number_berths ?seaport2} = 40 at 1,
            only_use_if {apportioned_forces ?ssealift1} at 1;

 effects {moved ?cargobysea1 ?seaport1 ?seaport2} at 1;
end_schema;

schema move_by_sealift_2;
 vars ?cargobyair1 = ?{type cargobyair},
      ?seaport2 = ?{type sea_port},
      ?seaport1 = ?{type sea_port},
      ?duration1,
      ?ssealift1 = ?{type ssealift};

 expands {move ?cargobyair1 ?seaport1 ?seaport2};

 nodes 1 action {move_by_sealift ?cargobyair1 ?seaport1 ?seaport2 ?ssealift1 ?duration1};

 conditions only_use_if {sealift_duration2 ?ssealift1 ?cargobyair1 ?seaport1 ?seaport2} = ?duration1 at 1,
	    only_use_if {number_berths ?seaport2} = 40 at 1,
            only_use_if {apportioned_forces ?ssealift1} at 1;

 effects {moved ?cargobyair1 ?seaport1 ?seaport2};
end_schema;

schema move_by_sealift_3;
 vars ?navy1 = ?{type navy_defence},
      ?seaport2 = ?{type sea_port},
      ?seaport1 = ?{type sea_port},
      ?duration1,
      ?ssealift1 = ?{type ssealift};

 expands {move ?navy1 ?seaport1 ?seaport2};

 nodes 1 action {move_by_sealift ?navy1 ?seaport1 ?seaport2 ?ssealift1 ?duration1};

 conditions only_use_if {sealift_duration3 ?ssealift1 ?navy1 ?seaport1 ?seaport2} = ?duration1 at 1,
	    only_use_if {number_berths ?seaport2} = 40 at 1,
            only_use_if {apportioned_forces ?ssealift1} at 1;

 effects {moved ?navy1 ?seaport1 ?seaport2};
end_schema;

schema move_by_sealift_4;
 vars ?navy1 = ?{type navy},
      ?sea1 = ?{type sea},
      ?duration1,
      ?seaport2 = ?{type sea_port};

 expands {move ?navy1 ?sea1 ?seaport2};

 nodes 1 action {move_sea ?navy1 ?sea1 ?seaport2 ?duration1};

 conditions only_use_if {sea_duration ?sea1 ?seaport2} = ?duration1 at 1,
	    only_use_if {number_berths ?seaport2} = 40 at 1;

 effects {moved ?navy1 ?sea1 ?seaport2};
end_schema;

schema move_ground_1;
 vars ?force1,
      ?duration1,
      ?territory1,
      ?territory2;

 expands {ground_move ?force1 ?territory1 ?territory2};

 nodes 1 action {move_by_ground ?force1 ?territory1 ?territory2 ?duration1};

 conditions only_use_if {ground_duration ?territory1 ?territory2} = ?duration1 at 1;
            ;;;only_use_if {located ?force1 ?territory1};
 effects {ground_moved ?force1 ?territory1 ?territory2};
end_schema;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Primitive Schemas used in the SOCAP Domain.
;;;
;;; The primitive schemas have no sub-nodes and their expands field is
;;; linked to the requirement from higher level schemas. Each has an
;;; only_use_for_effects field which could be used for satisfying 
;;; ACHIEVE conditions but at present these are not used. 


schema move_forces1;
 vars ?force1,
      ?duration1,
      ?territory1,
      ?territory2;
 expands {move_by_ground ?force1 ?territory1 ?territory2 ?duration1};
 only_use_for_effects {moved_by_ground ?force1 ?territory1 ?territory2 ?duration1};
 conditions only_use_if {located ?force1 ?territory1};
 effects {located ?force1 ?territory2};
 time_windows duration self = 2 days;
end_schema;

schema move_forces2;
 vars ?cargobyair1,
      ?airfield1,
      ?airfield2,
      ?sairlift1,
      ?duration1;
 expands {move_by_sairlift ?cargobyair1 ?airfield1 ?airfield2  ?sairlift1 ?duration1};
 only_use_for_effects {moved_by_sairlift ?cargobyair1 ?airfield1 ?airfield2  ?sairlift1 ?duration1};
 effects {located ?cargobyair1 ?airfield2};
 time_windows duration self = 3 days;
end_schema;

schema move_forces3;
 vars ?cargobyair1,
      ?airfield1,
      ?airfield2,
      ?tairlift1,
      ?duration1;
 expands {move_by_tairlift ?cargobyair1 ?airfield1 ?airfield2  ?tairlift1 ?duration1};
 only_use_for_effects {moved_by_tairlift ?cargobyair1 ?airfield1 ?airfield2  ?tairlift1 ?duration1};
 effects {located ?cargobyair1 ?airfield2};
 time_windows duration self = 3 days;
end_schema;

schema move_forces4;
 vars ?cargobysea1,
      ?seaport2,
      ?seaport1,
      ?duration1,
      ?ssealift1;
 expands {move_by_sealift ?cargobysea1 ?seaport1 ?seaport2  ?ssealift1 ?duration1};
 only_use_for_effects {moved_by_ssealift ?cargobysea1 ?seaport1 ?seaport2  ?ssealift1 ?duration1};
 effects {located ?cargobysea1 ?seaport2};
 time_windows duration self = 6 days;
end_schema;

schema move_forces5;
 vars ?navy1,
      ?sea1,
      ?duration1,
      ?seaport2;
 expands {move_sea ?navy1 ?sea1 ?seaport2 ?duration1};
 only_use_for_effects {moved_from_sea ?navy1 ?sea1 ?seaport2};
 effects {located ?navy1 ?seaport2};
end_schema;

schema mobilize_force;
 vars ?force,
      ?location;
 expands {mobilize ?force ?location};
 only_use_for_effects {mobilized ?force ?location};
 time_windows duration self = 3 days;
end_schema;

schema traverse_terrain;
 vars ?army1,
      ?urban1,
      ?urban2;
  expands {traverse_terrain ?army1 ?urban1 ?urban2};
  only_use_for_effects {terrain_traversed ?army1 ?urban1 ?urban2};
  time_windows duration self = 2 days;
end_schema;

schema ground_patrol; 
 vars ?army1, 
      ?region1, 
      ?coa1;
 expands {ground_patrol ?army1 ?region1 ?coa1};
 only_use_for_effects {ground_patrolled ?army1 ?region1 ?coa1};
end_schema;

schema move_sea_sector; 
 vars 	?navy1, 
	?seaport1, 
	?sea_sector1;
 expands {move_sea_sector ?navy1 ?seaport1 ?sea_sector1};
 only_use_for_effects {moved_to_sea_sector ?navy1 ?seaport1 ?sea_sector1};
 time_windows duration self = 3 days;
end_schema;

schema naval_patrol; 
 vars 	?navy1, 
	?sea_sector1, 
	?coa1, 
	?end_time1;
 expands {naval_patrol ?navy1 ?sea_sector1 ?coa1 ?end_time1};
 only_use_for_effects {naval_patrolled ?navy1 ?sea_sector1 ?coa1 ?end_time1};
end_schema;

schema cap; 
 vars 	?tfighter1, 
	?air_sector1, 
	?coa1, 
	?end_time1;
 expands {cap ?tfighter1 ?air_sector1 ?coa1 ?end_time1};
 only_use_for_effects {cap_provided ?air_sector1 ?coa1 ?end_time1};
end_schema;

schema occupy_key_terrain;
 vars 	?army1, 
	?urban2, 
	?coa1, 
	?end_time1;
 expands {occupy_key_terrain ?army1 ?urban2 ?coa1 ?end_time1};
 only_use_for_effects {key_terrain_occupied ?army1 ?urban2 ?coa1 ?end_time1};
 time_windows duration self = 2 days;
end_schema;

schema protect_loc;
 vars 	?army1,
 	?route1,
 	?coa1,
 	?end_time1;
 expands {protect_loc ?army1 ?route1 ?coa1 ?end_time1};
 only_use_for_effects {loc_protected ?army1 ?route1 ?coa1 ?end_time1};
end_schema;

schema setup_base; 
 vars	?army1,
        ?urban1, 
	?coa1, 
	?end_time1;
 expands {setup_base ?army1 ?urban1 ?coa1 ?end_time1};
 only_use_for_effects {base_setup ?urban1 ?coa1 ?end_time1};
 time_windows duration self = 4 days;
end_schema;

schema locate_reserves;
 vars	?army1,
 	?urban1,
 	?coa1,
	?end_time1;
 expands {locate_reserves ?army1 ?urban1 ?coa1 ?end_time1};
 only_use_for_effects {reserved_located ?army1 ?urban1 ?coa1 ?end_time1};
 time_windows duration self = 3 days;
end_schema;

schema defend_harbor;
 vars 	?navy1,
	?seaport1,
 	?coa1,
	?end_time1;
 expands {defend_harbor ?navy1 ?seaport1 ?coa1 ?end_time1};
 only_use_for_effects {harbor_defended ?navy1 ?seaport1 ?coa1 ?end_time1};
end_schema;

schema protect_sloc; 
 vars 	?navy1,
	?sea_loc1, 
	?coa1,
	?end_time1;
 expands {protect_sloc ?navy1 ?sea_loc1 ?coa1 ?end_time1};
 only_use_for_effects {sloc_protected ?navy1 ?sea_loc1 ?coa1 ?end_time1};
end_schema;

schema air_intercept;
 vars 	?tfighter1, 
	?airfield1,
	?airspace1, 
	?coa1,
	?end_time1;
 expands {air_intercept ?tfighter1 ?airfield1 ?airspace1 ?coa1 ?end_time1};
 only_use_for_effects {air_intercept_provided ?tfighter1 ?airfield1 ?airspace1 ?coa1 ?end_time1};
end_schema;

schema setup_airbase;
 vars	?tfighter1,
        ?airfield1, 
	?coa1, 
	?end_time1;
 expands {setup_airbase ?tfighter1 ?airfield1 ?coa1 ?end_time1};
 only_use_for_effects {airbase_provided ?airfield1 ?coa1 ?end_time1};
 time_windows duration self = 3 days;
end_schema;

schema protect_aloc;
 vars  	?tfighter1,
	?air_loc1,
	?coa1,
	?end_time1;
 expands {protect_aloc ?tfighter1 ?air_loc1 ?coa1 ?end_time1};
 only_use_for_effects {aloc_protected ?tfighter1 ?air_loc1 ?coa1 ?end_time1};
end_schema;


