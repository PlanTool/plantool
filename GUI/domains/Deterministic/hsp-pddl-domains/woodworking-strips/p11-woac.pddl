; woodworking task with 3 parts and 120% wood
; Machines:
;   1 grinder
;   1 glazer
;   1 immersion-varnisher
;   1 planer
;   1 highspeed-saw
;   1 spray-varnisher
;   1 saw
; random seed: 578239

(define (problem wood-prob-s11)
  (:domain woodworking)
  (:objects
    grinder0 - grinder
    glazer0 - glazer
    immersion-varnisher0 - immersion-varnisher
    planer0 - planer
    highspeed-saw0 - highspeed-saw
    spray-varnisher0 - spray-varnisher
    saw0 - saw
    blue mauve - acolour
    beech mahogany - awood
    p0 p1 p2 - part
     - board
    s0 - aboardsize
  )
  (:init
    (grind-treatment-change varnished colourfragments)
    (grind-treatment-change glazed untreated)
    (grind-treatment-change untreated untreated)
    (grind-treatment-change colourfragments untreated)
    (is-smooth smooth)
    (is-smooth verysmooth)
    
    (has-colour glazer0 natural)
    (has-colour immersion-varnisher0 blue)
    (empty highspeed-saw0)
    (has-colour spray-varnisher0 mauve)
    (available p0)
    (colour p0 blue)
    (wood p0 mahogany)
    (surface-condition p0 verysmooth)
    (treatment p0 colourfragments)
    (goalsize p0 small)
    
    
    
    
    (available p1)
    (colour p1 natural)
    (wood p1 mahogany)
    (surface-condition p1 smooth)
    (treatment p1 colourfragments)
    (goalsize p1 small)
    
    
    
    
    (available p2)
    (colour p2 mauve)
    (wood p2 beech)
    (surface-condition p2 verysmooth)
    (treatment p2 colourfragments)
    (goalsize p2 small)
    
    
    
    
  )
  (:goal
    (and
      (available p0)
      (wood p0 mahogany)
      (surface-condition p0 smooth)
      (available p1)
      (surface-condition p1 smooth)
      (treatment p1 glazed)
      (available p2)
      (colour p2 natural)
      (treatment p2 glazed)
    )
  )
  
)
