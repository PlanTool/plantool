; woodworking task with 15 parts and 120% wood
; Machines:
;   1 grinder
;   1 glazer
;   1 immersion-varnisher
;   1 planer
;   1 highspeed-saw
;   1 spray-varnisher
;   1 saw
; random seed: 370706

(define (problem wood-prob-s15)
  (:domain woodworking)
  (:objects
    grinder0 - grinder
    glazer0 - glazer
    immersion-varnisher0 - immersion-varnisher
    planer0 - planer
    highspeed-saw0 - highspeed-saw
    spray-varnisher0 - spray-varnisher
    saw0 - saw
    mauve white blue black red green - acolour
    oak mahogany teak beech - awood
    p0 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 - part
    b0 b1 b2 b3 b4 - board
    s0 s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 - aboardsize
  )
  (:init
    (grind-treatment-change varnished colourfragments)
    (grind-treatment-change glazed untreated)
    (grind-treatment-change untreated untreated)
    (grind-treatment-change colourfragments untreated)
    (is-smooth smooth)
    (is-smooth verysmooth)
    
    (boardsize-successor s0 s1)
    (boardsize-successor s1 s2)
    (boardsize-successor s2 s3)
    (boardsize-successor s3 s4)
    (boardsize-successor s4 s5)
    (boardsize-successor s5 s6)
    (boardsize-successor s6 s7)
    (boardsize-successor s7 s8)
    (boardsize-successor s8 s9)
    (boardsize-successor s9 s10)
    (boardsize-successor s10 s11)
    (has-colour glazer0 blue)
    (has-colour glazer0 white)
    (has-colour glazer0 red)
    (has-colour immersion-varnisher0 blue)
    (has-colour immersion-varnisher0 white)
    (empty highspeed-saw0)
    (has-colour spray-varnisher0 blue)
    (has-colour spray-varnisher0 white)
    (unused p0)
    (goalsize p0 large)
    
    
    
    
    (unused p1)
    (goalsize p1 large)
    
    
    
    
    (unused p2)
    (goalsize p2 small)
    
    
    
    
    (available p3)
    (colour p3 green)
    (wood p3 beech)
    (surface-condition p3 verysmooth)
    (treatment p3 varnished)
    (goalsize p3 large)
    
    
    
    
    (unused p4)
    (goalsize p4 medium)
    
    
    
    
    (unused p5)
    (goalsize p5 large)
    
    
    
    
    (unused p6)
    (goalsize p6 large)
    
    
    
    
    (unused p7)
    (goalsize p7 large)
    
    
    
    
    (unused p8)
    (goalsize p8 large)
    
    
    
    
    (unused p9)
    (goalsize p9 medium)
    
    
    
    
    (unused p10)
    (goalsize p10 large)
    
    
    
    
    (unused p11)
    (goalsize p11 small)
    
    
    
    
    (unused p12)
    (goalsize p12 large)
    
    
    
    
    (available p13)
    (colour p13 red)
    (wood p13 beech)
    (surface-condition p13 rough)
    (treatment p13 glazed)
    (goalsize p13 medium)
    
    
    
    
    (available p14)
    (colour p14 green)
    (wood p14 teak)
    (surface-condition p14 rough)
    (treatment p14 colourfragments)
    (goalsize p14 small)
    
    
    
    
    (boardsize b0 s10)
    (wood b0 beech)
    (surface-condition b0 rough)
    (available b0)
    (boardsize b1 s5)
    (wood b1 teak)
    (surface-condition b1 rough)
    (available b1)
    (boardsize b2 s9)
    (wood b2 mahogany)
    (surface-condition b2 rough)
    (available b2)
    (boardsize b3 s11)
    (wood b3 oak)
    (surface-condition b3 rough)
    (available b3)
    (boardsize b4 s3)
    (wood b4 oak)
    (surface-condition b4 rough)
    (available b4)
  )
  (:goal
    (and
      (available p0)
      (colour p0 white)
      (treatment p0 glazed)
      (available p1)
      (wood p1 mahogany)
      (surface-condition p1 verysmooth)
      (available p2)
      (surface-condition p2 smooth)
      (treatment p2 varnished)
      (available p3)
      (colour p3 red)
      (treatment p3 glazed)
      (available p4)
      (colour p4 blue)
      (surface-condition p4 verysmooth)
      (treatment p4 varnished)
      (available p5)
      (surface-condition p5 smooth)
      (treatment p5 varnished)
      (available p6)
      (surface-condition p6 smooth)
      (treatment p6 glazed)
      (available p7)
      (wood p7 mahogany)
      (treatment p7 varnished)
      (available p8)
      (colour p8 red)
      (wood p8 beech)
      (surface-condition p8 smooth)
      (treatment p8 glazed)
      (available p9)
      (wood p9 oak)
      (surface-condition p9 smooth)
      (available p10)
      (wood p10 oak)
      (surface-condition p10 verysmooth)
      (available p11)
      (colour p11 white)
      (wood p11 mahogany)
      (surface-condition p11 smooth)
      (treatment p11 varnished)
      (available p12)
      (wood p12 oak)
      (surface-condition p12 smooth)
      (available p13)
      (colour p13 blue)
      (wood p13 beech)
      (treatment p13 glazed)
      (available p14)
      (surface-condition p14 smooth)
      (treatment p14 glazed)
    )
  )
  
)
