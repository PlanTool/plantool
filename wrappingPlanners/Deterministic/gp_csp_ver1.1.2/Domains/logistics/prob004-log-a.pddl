;; original name logistics.a
;; extended version of logistics_facts7h
;; (:length (:parallel 11))
;; optimal
;; #actions 54 #states 10^11
;;
;; note: by going to a non-typed representation
;;       of the problems, the instances become (somewhat)
;;       harder to solve.
;;       (larger propositional representation)
;;

(define (problem log004)
    (:domain logistics-typed)
    (:objects
        package1 - PACKAGE
        package2 - PACKAGE
        package3 - PACKAGE
        package4 - PACKAGE
        package5 - PACKAGE
        package6 - PACKAGE
        package7 - PACKAGE
        package8 - PACKAGE

        airplane1 - AIRPLANE
        airplane2 - AIRPLANE

        pgh - CITY
        bos - CITY
        la - CITY

        pgh-truck - TRUCK
        bos-truck - TRUCK
        la-truck - TRUCK

        pgh-po - LOCATION
        bos-po - LOCATION
        la-po - LOCATION

        pgh-airport - (either LOCATION AIRPORT)
        bos-airport - (either LOCATION AIRPORT)
        la-airport - (either LOCATION AIRPORT)
    )
    (:init
        (in-city pgh-po pgh)
        (in-city pgh-airport pgh)

        (in-city bos-po bos)
        (in-city bos-airport bos)

        (in-city la-po la)
        (in-city la-airport la)

        (at package1 pgh-po)
        (at package2 pgh-po)
        (at package3 pgh-po)
        (at package4 pgh-po)
        (at package5 bos-po)
        (at package6 bos-po)
        (at package7 bos-po)
        (at package8 la-po)

        (at airplane1 pgh-airport)
        (at airplane2 pgh-airport)
       
        (at bos-truck bos-po)
        (at pgh-truck pgh-po)
        (at la-truck la-po)
    )
    (:goal (and
        (at package1 bos-po)
        (at package2 bos-airport)
        (at package3 la-po)
        (at package4 la-airport)
        (at package5 pgh-po)
        (at package6 pgh-airport)
        (at package7 pgh-po)
        (at package8 pgh-po)
    ))
)
