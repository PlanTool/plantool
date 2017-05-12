; woodworking task with 15 parts and 100% wood
; Machines:
;   1 grinder
;   1 glazer
;   1 immersion-varnisher
;   1 planer
;   1 highspeed-saw
;   1 spray-varnisher
;   1 saw
; random seed: 224661

(define (problem wood-prob-s25)
  (:domain woodworking)
  (:objects
    grinder0 - grinder
    glazer0 - glazer
    immersion-varnisher0 - immersion-varnisher
    planer0 - planer
    highspeed-saw0 - highspeed-saw
    spray-varnisher0 - spray-varnisher
    saw0 - saw
    green blue white red mauve black - acolour
    oak cherry walnut teak - awood
    p0 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 - part
    b0 b1 b2 b3 b4 - board
    s0 s1 s2 s3 s4 s5 s6 s7 s8 - aboardsize
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
    (has-colour glazer0 green)
    (has-colour glazer0 white)
    (has-colour glazer0 natural)
    (has-colour glazer0 black)
    (has-colour immersion-varnisher0 blue)
    (has-colour immersion-varnisher0 mauve)
    (has-colour immersion-varnisher0 white)
    (has-colour immersion-varnisher0 natural)
    (has-colour immersion-varnisher0 black)
    (empty highspeed-saw0)
    (has-colour spray-varnisher0 blue)
    (has-colour spray-varnisher0 mauve)
    (has-colour spray-varnisher0 white)
    (has-colour spray-varnisher0 natural)
    (has-colour spray-varnisher0 black)
    (unused p0)
    (goalsize p0 large)
    
    
    
    
    (unused p1)
    (goalsize p1 medium)
    
    
    
    
    (unused p2)
    (goalsize p2 small)
    
    
    
    
    (available p3)
    (colour p3 green)
    (wood p3 teak)
    (surface-condition p3 rough)
    (treatment p3 varnished)
    (goalsize p3 large)
    
    
    
    
    (available p4)
    (colour p4 blue)
    (wood p4 teak)
    (surface-condition p4 rough)
    (treatment p4 colourfragments)
    (goalsize p4 small)
    
    
    
    
    (unused p5)
    (goalsize p5 small)
    
    
    
    
    (unused p6)
    (goalsize p6 medium)
    
    
    
    
    (unused p7)
    (goalsize p7 large)
    
    
    
    
    (unused p8)
    (goalsize p8 large)
    
    
    
    
    (unused p9)
    (goalsize p9 small)
    
    
    
    
    (available p10)
    (colour p10 white)
    (wood p10 walnut)
    (surface-condition p10 smooth)
    (treatment p10 colourfragments)
    (goalsize p10 small)
    
    
    
    
    (unused p11)
    (goalsize p11 large)
    
    
    
    
    (unused p12)
    (goalsize p12 medium)
    
    
    
    
    (unused p13)
    (goalsize p13 large)
    
    
    
    
    (unused p14)
    (goalsize p14 medium)
    
    
    
    
    (boardsize b0 s5)
    (wood b0 cherry)
    (surface-condition b0 rough)
    (available b0)
    (boardsize b1 s3)
    (wood b1 cherry)
    (surface-condition b1 rough)
    (available b1)
    (boardsize b2 s7)
    (wood b2 oak)
    (surface-condition b2 smooth)
    (available b2)
    (boardsize b3 s3)
    (wood b3 oak)
    (surface-condition b3 rough)
    (available b3)
    (boardsize b4 s8)
    (wood b4 teak)
    (surface-condition b4 rough)
    (available b4)
  )
  (:goal
    (and
      (available p0)
      (colour p0 mauve)
      (wood p0 oak)
      (surface-condition p0 smooth)
      (treatment p0 varnished)
      (available p1)
      (surface-condition p1 smooth)
      (treatment p1 glazed)
      (available p2)
      (wood p2 cherry)
      (surface-condition p2 smooth)
      (treatment p2 glazed)
      (available p3)
      (wood p3 teak)
      (treatment p3 glazed)
      (available p4)
      (colour p4 white)
      (wood p4 teak)
      (surface-condition p4 smooth)
      (available p5)
      (colour p5 natural)
      (wood p5 cherry)
      (surface-condition p5 smooth)
      (treatment p5 varnished)
      (available p6)
      (colour p6 green)
      (wood p6 oak)
      (surface-condition p6 smooth)
      (treatment p6 glazed)
      (available p7)
      (colour p7 white)
      (wood p7 teak)
      (treatment p7 varnished)
      (available p8)
      (colour p8 green)
      (surface-condition p8 smooth)
      (treatment p8 glazed)
      (available p9)
      (colour p9 white)
      (treatment p9 varnished)
      (available p10)
      (colour p10 natural)
      (wood p10 walnut)
      (surface-condition p10 smooth)
      (treatment p10 glazed)
      (available p11)
      (colour p11 black)
      (wood p11 teak)
      (surface-condition p11 smooth)
      (available p12)
      (colour p12 blue)
      (wood p12 teak)
      (surface-condition p12 verysmooth)
      (treatment p12 varnished)
      (available p13)
      (colour p13 white)
      (wood p13 cherry)
      (available p14)
      (colour p14 natural)
      (wood p14 oak)
      (surface-condition p14 smooth)
      (treatment p14 glazed)
    )
  )
  
)
