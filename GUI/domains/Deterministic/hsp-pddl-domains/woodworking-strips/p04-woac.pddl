; woodworking task with 12 parts and 140% wood
; Machines:
;   1 grinder
;   1 glazer
;   1 immersion-varnisher
;   1 planer
;   1 highspeed-saw
;   1 spray-varnisher
;   1 saw
; random seed: 686037

(define (problem wood-prob-s04)
  (:domain woodworking)
  (:objects
    grinder0 - grinder
    glazer0 - glazer
    immersion-varnisher0 - immersion-varnisher
    planer0 - planer
    highspeed-saw0 - highspeed-saw
    spray-varnisher0 - spray-varnisher
    saw0 - saw
    mauve green white red blue black - acolour
    beech oak pine - awood
    p0 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 - part
    b0 b1 b2 b3 - board
    s0 s1 s2 s3 s4 s5 s6 s7 - aboardsize
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
    (has-colour glazer0 white)
    (has-colour glazer0 red)
    (has-colour immersion-varnisher0 mauve)
    (has-colour immersion-varnisher0 white)
    (has-colour immersion-varnisher0 natural)
    (has-colour immersion-varnisher0 red)
    (empty highspeed-saw0)
    (has-colour spray-varnisher0 mauve)
    (has-colour spray-varnisher0 white)
    (has-colour spray-varnisher0 natural)
    (has-colour spray-varnisher0 red)
    (unused p0)
    (goalsize p0 medium)
    
    
    
    
    (unused p1)
    (goalsize p1 medium)
    
    
    
    
    (unused p2)
    (goalsize p2 small)
    
    
    
    
    (unused p3)
    (goalsize p3 small)
    
    
    
    
    (unused p4)
    (goalsize p4 large)
    
    
    
    
    (unused p5)
    (goalsize p5 large)
    
    
    
    
    (unused p6)
    (goalsize p6 medium)
    
    
    
    
    (available p7)
    (colour p7 blue)
    (wood p7 oak)
    (surface-condition p7 rough)
    (treatment p7 varnished)
    (goalsize p7 medium)
    
    
    
    
    (available p8)
    (colour p8 blue)
    (wood p8 beech)
    (surface-condition p8 smooth)
    (treatment p8 glazed)
    (goalsize p8 large)
    
    
    
    
    (unused p9)
    (goalsize p9 small)
    
    
    
    
    (unused p10)
    (goalsize p10 medium)
    
    
    
    
    (unused p11)
    (goalsize p11 small)
    
    
    
    
    (boardsize b0 s6)
    (wood b0 beech)
    (surface-condition b0 rough)
    (available b0)
    (boardsize b1 s7)
    (wood b1 beech)
    (surface-condition b1 rough)
    (available b1)
    (boardsize b2 s6)
    (wood b2 oak)
    (surface-condition b2 rough)
    (available b2)
    (boardsize b3 s7)
    (wood b3 pine)
    (surface-condition b3 rough)
    (available b3)
  )
  (:goal
    (and
      (available p0)
      (colour p0 red)
      (wood p0 beech)
      (surface-condition p0 verysmooth)
      (treatment p0 glazed)
      (available p1)
      (wood p1 pine)
      (surface-condition p1 verysmooth)
      (available p2)
      (surface-condition p2 verysmooth)
      (treatment p2 glazed)
      (available p3)
      (colour p3 natural)
      (wood p3 beech)
      (surface-condition p3 verysmooth)
      (treatment p3 varnished)
      (available p4)
      (colour p4 natural)
      (wood p4 oak)
      (surface-condition p4 smooth)
      (treatment p4 varnished)
      (available p5)
      (colour p5 red)
      (surface-condition p5 smooth)
      (available p6)
      (colour p6 natural)
      (wood p6 beech)
      (surface-condition p6 verysmooth)
      (treatment p6 varnished)
      (available p7)
      (colour p7 mauve)
      (surface-condition p7 smooth)
      (treatment p7 varnished)
      (available p8)
      (colour p8 white)
      (wood p8 beech)
      (surface-condition p8 verysmooth)
      (available p9)
      (surface-condition p9 verysmooth)
      (treatment p9 varnished)
      (available p10)
      (wood p10 pine)
      (surface-condition p10 verysmooth)
      (treatment p10 glazed)
      (available p11)
      (colour p11 red)
      (wood p11 oak)
      (surface-condition p11 verysmooth)
      (treatment p11 glazed)
    )
  )
  
)
