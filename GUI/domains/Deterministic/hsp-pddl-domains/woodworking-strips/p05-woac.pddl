; woodworking task with 15 parts and 140% wood
; Machines:
;   1 grinder
;   1 glazer
;   1 immersion-varnisher
;   1 planer
;   1 highspeed-saw
;   1 spray-varnisher
;   1 saw
; random seed: 627360

(define (problem wood-prob-s05)
  (:domain woodworking)
  (:objects
    grinder0 - grinder
    glazer0 - glazer
    immersion-varnisher0 - immersion-varnisher
    planer0 - planer
    highspeed-saw0 - highspeed-saw
    spray-varnisher0 - spray-varnisher
    saw0 - saw
    red black mauve blue green white - acolour
    teak cherry walnut pine - awood
    p0 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 - part
    b0 b1 b2 b3 b4 b5 - board
    s0 s1 s2 s3 s4 s5 s6 s7 s8 s9 - aboardsize
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
    (has-colour glazer0 green)
    (has-colour glazer0 red)
    (has-colour immersion-varnisher0 mauve)
    (has-colour immersion-varnisher0 white)
    (has-colour immersion-varnisher0 green)
    (has-colour immersion-varnisher0 red)
    (empty highspeed-saw0)
    (has-colour spray-varnisher0 mauve)
    (has-colour spray-varnisher0 white)
    (has-colour spray-varnisher0 green)
    (has-colour spray-varnisher0 red)
    (unused p0)
    (goalsize p0 medium)
    
    
    
    
    (unused p1)
    (goalsize p1 medium)
    
    
    
    
    (unused p2)
    (goalsize p2 large)
    
    
    
    
    (unused p3)
    (goalsize p3 small)
    
    
    
    
    (unused p4)
    (goalsize p4 small)
    
    
    
    
    (unused p5)
    (goalsize p5 medium)
    
    
    
    
    (unused p6)
    (goalsize p6 small)
    
    
    
    
    (unused p7)
    (goalsize p7 small)
    
    
    
    
    (unused p8)
    (goalsize p8 large)
    
    
    
    
    (available p9)
    (colour p9 natural)
    (wood p9 pine)
    (surface-condition p9 smooth)
    (treatment p9 glazed)
    (goalsize p9 medium)
    
    
    
    
    (unused p10)
    (goalsize p10 small)
    
    
    
    
    (unused p11)
    (goalsize p11 small)
    
    
    
    
    (available p12)
    (colour p12 mauve)
    (wood p12 teak)
    (surface-condition p12 verysmooth)
    (treatment p12 colourfragments)
    (goalsize p12 small)
    
    
    
    
    (unused p13)
    (goalsize p13 large)
    
    
    
    
    (unused p14)
    (goalsize p14 small)
    
    
    
    
    (boardsize b0 s6)
    (wood b0 cherry)
    (surface-condition b0 rough)
    (available b0)
    (boardsize b1 s1)
    (wood b1 cherry)
    (surface-condition b1 smooth)
    (available b1)
    (boardsize b2 s2)
    (wood b2 walnut)
    (surface-condition b2 rough)
    (available b2)
    (boardsize b3 s8)
    (wood b3 pine)
    (surface-condition b3 rough)
    (available b3)
    (boardsize b4 s6)
    (wood b4 pine)
    (surface-condition b4 smooth)
    (available b4)
    (boardsize b5 s9)
    (wood b5 teak)
    (surface-condition b5 rough)
    (available b5)
  )
  (:goal
    (and
      (available p0)
      (colour p0 green)
      (wood p0 pine)
      (surface-condition p0 smooth)
      (treatment p0 glazed)
      (available p1)
      (colour p1 red)
      (surface-condition p1 smooth)
      (available p2)
      (colour p2 red)
      (wood p2 pine)
      (surface-condition p2 smooth)
      (available p3)
      (wood p3 pine)
      (treatment p3 varnished)
      (available p4)
      (colour p4 white)
      (wood p4 cherry)
      (surface-condition p4 verysmooth)
      (treatment p4 varnished)
      (available p5)
      (colour p5 mauve)
      (wood p5 teak)
      (surface-condition p5 smooth)
      (treatment p5 varnished)
      (available p6)
      (surface-condition p6 verysmooth)
      (treatment p6 glazed)
      (available p7)
      (wood p7 cherry)
      (treatment p7 glazed)
      (available p8)
      (colour p8 red)
      (surface-condition p8 verysmooth)
      (available p9)
      (colour p9 red)
      (wood p9 pine)
      (surface-condition p9 verysmooth)
      (treatment p9 varnished)
      (available p10)
      (wood p10 walnut)
      (surface-condition p10 verysmooth)
      (treatment p10 varnished)
      (available p11)
      (colour p11 green)
      (wood p11 pine)
      (surface-condition p11 verysmooth)
      (treatment p11 varnished)
      (available p12)
      (colour p12 green)
      (wood p12 teak)
      (surface-condition p12 smooth)
      (treatment p12 glazed)
      (available p13)
      (wood p13 pine)
      (surface-condition p13 smooth)
      (available p14)
      (surface-condition p14 smooth)
      (treatment p14 varnished)
    )
  )
  
)
