;; original name logistics.d
;; (:length (:parallel 14))
;; optimal
;;

(define (problem log007)
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
        package9 - PACKAGE

        airplane1 - AIRPLANE
        airplane2 - AIRPLANE

        pgh - CITY
        bos - CITY
        la - CITY
        ny - CITY
        sf - CITY

        pgh-truck - TRUCK
        bos-truck - TRUCK
        la-truck - TRUCK
        ny-truck - TRUCK
        sf-truck - TRUCK

        pgh-po - LOCATION
        bos-po - LOCATION
        la-po - LOCATION
        ny-po - LOCATION
        sf-po - LOCATION

        pgh-central - LOCATION
        bos-central - LOCATION
        la-central - LOCATION
        ny-central - LOCATION
        sf-central - LOCATION

        pgh-airport - (either LOCATION AIRPORT)
        bos-airport - (either LOCATION AIRPORT)
        la-airport - (either LOCATION AIRPORT)
        ny-airport - (either LOCATION AIRPORT)
        sf-airport - (either LOCATION AIRPORT)
    )
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

        (in-city ny-po ny)
        (in-city ny-airport ny)
        (in-city ny-central ny)

        (in-city sf-po sf)
        (in-city sf-airport sf)
        (in-city sf-central sf)

        (at package1 pgh-po)
        (at package2 pgh-central)
        (at package3 pgh-central)
        (at package4 ny-po)
        (at package5 bos-po)
        (at package6 bos-po)
        (at package7 ny-po)
        (at package8 sf-airport)
        (at package9 sf-central)

        (at airplane1 pgh-airport)
        (at airplane2 pgh-airport)

        (at bos-truck bos-po)
        (at pgh-truck pgh-airport)
        (at la-truck la-po)
        (at ny-truck ny-central)
        (at sf-truck sf-airport)
    )
    (:goal (and
       (at package1 bos-po)
       (at package2 ny-po)
       (at package3 la-central)
       (at package4 la-airport)
       (at package5 pgh-po)
       (at package6 ny-central)
       (at package7 pgh-po)
       (at package8 ny-central)
       (at package9 sf-po)
    ))
)
