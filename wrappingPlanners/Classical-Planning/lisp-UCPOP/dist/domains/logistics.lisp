(in-package "UCPOP")

(define (DOMAIN LOGISTICS-OLD)
    (:OPERATOR LOAD-TRUCK :PARAMETERS ((OBJECT ?OBJ) (TRUCK ?TRUCK) (LOCATION ?LOC)) :PRECONDITION
	       (AND (AT ?TRUCK ?LOC) (AT ?OBJ ?LOC)) :EFFECT
	       (and (IN ?OBJ ?TRUCK)))
  (:OPERATOR LOAD-AIRPLANE :PARAMETERS ((OBJECT ?OBJ) (AIRPLANE ?AIRPLANE) (LOCATION ?LOC)) :PRECONDITION
	     (AND (AT ?OBJ ?LOC) (AT ?AIRPLANE ?LOC)) :EFFECT
	     (and (IN ?OBJ ?AIRPLANE)))
  (:OPERATOR UNLOAD-TRUCK :PARAMETERS ((OBJECT ?OBJ) (TRUCK ?TRUCK) (LOCATION ?LOC)) :PRECONDITION
	     (AND (AT ?TRUCK ?LOC) (IN ?OBJ ?TRUCK)) :EFFECT
	     (and (AT ?OBJ ?LOC) (:NOT (IN ?OBJ ?TRUCK))))
  (:OPERATOR UNLOAD-AIRPLANE :PARAMETERS ((OBJECT ?OBJ) (AIRPLANE ?AIRPLANE) (LOCATION ?LOC)) :PRECONDITION
	     (AND (IN ?OBJ ?AIRPLANE) (AT ?AIRPLANE ?LOC)) :EFFECT
	     (and (AT ?OBJ ?LOC) (:NOT (IN ?OBJ AIRPLANE))))
  (:OPERATOR DRIVE-TRUCK :PARAMETERS ((TRUCK ?TRUCK) (LOCATION ?LOC-FROM) (LOCATION ?LOC-TO) (CITY ?CITY))
	     :PRECONDITION
	     (AND (AT ?TRUCK ?LOC-FROM) (LOC-AT ?LOC-FROM ?CITY) (LOC-AT ?LOC-TO ?CITY))
	     :EFFECT
	     (and (AT ?TRUCK ?LOC-TO) (:NOT (AT ?TRUCK ?LOC-FROM))))
  (:OPERATOR FLY-AIRPLANE :PARAMETERS ((AIRPLANE ?AIRPLANE) (LOCATION ?LOC-FROM) (LOCATION ?LOC-TO))
	     :PRECONDITION (AND (AT ?AIRPLANE ?LOC-FROM)) :EFFECT (and (AT ?AIRPLANE ?LOC-TO)
								       (:NOT (AT ?AIRPLANE ?LOC-FROM)))))

(define (DOMAIN LOGISTICS-DUMB)
  (:axiom is-vehicle
	  :context (or (truck ?vehicle)
		       (airplane ?vehicle))
	  :implies (vehicle ?vehicle))
  (:OPERATOR LOAD :PARAMETERS ((OBJECT ?OBJ) (VEHICLE ?AIRPLANE) (LOCATION ?LOC)) :PRECONDITION
	     (AND (AT ?OBJ ?LOC) (AT ?AIRPLANE ?LOC)) :EFFECT
	     (and (IN ?OBJ ?AIRPLANE)))
  (:OPERATOR UNLOAD :PARAMETERS ((OBJECT ?OBJ) (VEHICLE ?AIRPLANE) (LOCATION ?LOC)) :PRECONDITION
	     (AND (IN ?OBJ ?AIRPLANE) (AT ?AIRPLANE ?LOC)) :EFFECT
	     (and (AT ?OBJ ?LOC) (:NOT (IN ?OBJ AIRPLANE))))
  (:OPERATOR GO :PARAMETERS ((VEHICLE ?TRUCK) (LOCATION ?LOC-FROM) (LOCATION ?LOC-TO) (CITY ?CITY))
	     :PRECONDITION
	     (AND (AT ?TRUCK ?LOC-FROM) (LOC-AT ?LOC-FROM ?CITY) (LOC-AT ?LOC-TO ?CITY))
	     :EFFECT
	     (and (AT ?TRUCK ?LOC-TO) (:NOT (AT ?TRUCK ?LOC-FROM)))))

(define (DOMAIN LOGISTICS)
  (:axiom is-vehicle
	  :context (or (truck ?vehicle)
		       (airplane ?vehicle))
	  :implies (vehicle ?vehicle))
  (:OPERATOR LOAD :PARAMETERS ((OBJECT ?OBJ) (VEHICLE ?AIRPLANE) (LOCATION ?LOC)) :PRECONDITION
	     (AND (AT ?OBJ ?LOC) (AT ?AIRPLANE ?LOC)) :EFFECT
	     (and (IN ?OBJ ?AIRPLANE)))
  (:OPERATOR UNLOAD :PARAMETERS ((OBJECT ?OBJ) (VEHICLE ?AIRPLANE) (LOCATION ?LOC)) :PRECONDITION
	     (AND (IN ?OBJ ?AIRPLANE) (AT ?AIRPLANE ?LOC)) :EFFECT
	     (and (:NOT (IN ?OBJ AIRPLANE))))
  (:OPERATOR GO :PARAMETERS ((VEHICLE ?TRUCK) (LOCATION ?LOC-FROM) (LOCATION ?LOC-TO) (CITY ?CITY))
	     :PRECONDITION
	     (AND (AT ?TRUCK ?LOC-FROM) (LOC-AT ?LOC-FROM ?CITY) (LOC-AT ?LOC-TO ?CITY))
	     :EFFECT
	     (and (AT ?TRUCK ?LOC-TO) (:NOT (AT ?TRUCK ?LOC-FROM))
		  (forall (?x)
			  (when (and (object ?x) (in ?x ?truck))
			    (and (:not (at ?x ?loc-from)) (at ?x ?loc-to)))))))

(define (problem logistics1)
    :domain 'logistics
    :inits (
(OBJECT PACKAGE1) (OBJECT PACKAGE2) (OBJECT PACKAGE3) (OBJECT PACKAGE4) (OBJECT PACKAGE5) (OBJECT PACKAGE6)
 (TRUCK PGH-TRUCK) (TRUCK BOS-TRUCK) (TRUCK LA-TRUCK) (AIRPLANE AIRPLANE1) (AIRPLANE AIRPLANE2) (LOCATION BOS-PO)
 (LOCATION LA-PO) (LOCATION PGH-PO) (AIRPORT BOS-AIRPORT) (LOCATION BOS-AIRPORT) (AIRPORT PGH-AIRPORT)
 (LOCATION PGH-AIRPORT) (AIRPORT LA-AIRPORT) (LOCATION LA-AIRPORT) (CITY PGH) (CITY BOS) (CITY LA)

 (at package1 pgh-po)
 (at package2 pgh-po)
 (at package3 pgh-po)
 (at package4 pgh-po)
 (at package5 pgh-po)
 (at package6 la-po)
 (at airplane1 pgh-airport)
 (at airplane2 pgh-airport)
 (at bos-truck bos-po)
 (at pgh-truck pgh-po)
 (at la-truck la-po)
 (loc-at pgh-po pgh)
 (loc-at pgh-airport pgh)
 (loc-at bos-po bos)
 (loc-at bos-airport bos)
 (loc-at la-po la)
 (loc-at la-airport la))
    :goal (and
 (at package1 bos-po)
 (at package2 la-po)
 (at package3 la-po)
 (at package4 la-airport)
 (at package5 bos-po)
 (at package6 pgh-po)
))


(define (problem logistics2)
    :domain 'logistics
    :inits (
(OBJECT PACKAGE1) (OBJECT PACKAGE2) (OBJECT PACKAGE3) (OBJECT PACKAGE5) (OBJECT PACKAGE7) (TRUCK PGH-TRUCK)
 (TRUCK BOS-TRUCK) (TRUCK LA-TRUCK) (TRUCK NY-TRUCK) (AIRPLANE AIRPLANE1) (AIRPLANE AIRPLANE2) (LOCATION BOS-PO)
 (LOCATION LA-PO) (LOCATION PGH-PO) (LOCATION NY-PO) (AIRPORT BOS-AIRPORT) (LOCATION BOS-AIRPORT) (AIRPORT PGH-AIRPORT)
 (LOCATION PGH-AIRPORT) (AIRPORT LA-AIRPORT) (LOCATION LA-AIRPORT) (AIRPORT NY-AIRPORT) (LOCATION NY-AIRPORT) (CITY PGH)
 (CITY BOS) (CITY LA) (CITY NY)
 (at package1 pgh-po)
 (at package2 pgh-po)
 (at package3 pgh-po)
 (at package5 bos-po)
 (at package7 ny-po)
 (at airplane1 pgh-airport)
 (at airplane2 pgh-airport)
 (at bos-truck bos-po)
 (at pgh-truck pgh-po)
 (at la-truck la-po)
 (at ny-truck ny-po)
 (loc-at pgh-po pgh)
 (loc-at pgh-airport pgh)
 (loc-at bos-po bos)
 (loc-at bos-airport bos)
 (loc-at la-po la)
 (loc-at la-airport la)
 (loc-at ny-po ny)
 (loc-at ny-airport ny))
    :goal (and
 (at package1 bos-po)
 (at package2 ny-po)
 (at package3 la-po)
 (at package5 pgh-po)
 (at package7 pgh-po))
)

(define (problem logistics3)
    :domain 'logistics
    :inits (
(OBJECT PACKAGE1) (OBJECT PACKAGE2) (OBJECT PACKAGE3) (OBJECT PACKAGE4) (OBJECT PACKAGE5) (OBJECT PACKAGE6)
 (OBJECT PACKAGE7) (TRUCK PGH-TRUCK) (TRUCK BOS-TRUCK) (TRUCK LA-TRUCK) (TRUCK NY-TRUCK) (AIRPLANE AIRPLANE1)
 (AIRPLANE AIRPLANE2) (LOCATION BOS-PO) (LOCATION LA-PO) (LOCATION PGH-PO) (LOCATION NY-PO) (AIRPORT BOS-AIRPORT)
 (LOCATION BOS-AIRPORT) (AIRPORT PGH-AIRPORT) (LOCATION PGH-AIRPORT) (AIRPORT LA-AIRPORT) (LOCATION LA-AIRPORT)
 (AIRPORT NY-AIRPORT) (LOCATION NY-AIRPORT) (CITY PGH) (CITY BOS) (CITY LA) (CITY NY)
 (at package1 pgh-po)
 (at package2 pgh-po)
 (at package3 pgh-po)
 (at package4 ny-po)
 (at package5 bos-po)
 (at package6 bos-po)
 (at package7 ny-po)
 (at airplane1 pgh-airport)
 (at airplane2 pgh-airport)
 (at bos-truck bos-po)
 (at pgh-truck pgh-po)
 (at la-truck la-po)
 (at ny-truck ny-po)
 (loc-at pgh-po pgh)
 (loc-at pgh-airport pgh)
 (loc-at bos-po bos)
 (loc-at bos-airport bos)
 (loc-at la-po la)
 (loc-at la-airport la)
 (loc-at ny-po ny)
 (loc-at ny-airport ny))
    :goal (and
 (at package1 bos-po)
 (at package2 ny-po)
 (at package3 la-po)
 (at package4 la-airport)
 (at package5 pgh-po)
 (at package6 ny-airport)
 (at package7 pgh-po))
)

