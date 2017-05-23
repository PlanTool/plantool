;; original name logistics.easy
;; (:length (:parallel 9))
;; optimal
;;

(define (problem log001)
    (:domain logistics-typed)
    (:objects
        package1 - PACKAGE
        package2 - PACKAGE
        package3 - PACKAGE

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

        pgh-central - LOCATION
        bos-central - LOCATION
        la-central - LOCATION

        pgh-airport - (either AIRPORT LOCATION)
        bos-airport - (either AIRPORT LOCATION)
        la-airport - (either AIRPORT LOCATION))
    (:init
        (in-city pgh-po pgh)
        (in-city pgh-airport pgh)
        (in-city pgh-central pgh)

        (in-city bos-po bos)
        (in-city bos-airport bos)
        (in-city bos-central bos)

        (in-city la-po la)
        (in-city la-airport la)
        (in-city la-central la)

        (at package1 pgh-po)
        (at package2 pgh-po)
        (at package3 pgh-po)

        (at airplane1 pgh-airport)
        (at airplane2 pgh-airport)

        (at bos-truck bos-po)
        (at pgh-truck pgh-po)
        (at la-truck la-po)

    )
    (:goal (and
        (at package1 bos-po)
        (at package2 la-po)
        (at package3 bos-po)
    ))
)

