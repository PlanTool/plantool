; woodworking task with 27 parts and 100% wood
; Machines:
;   1 grinder
;   1 glazer
;   1 immersion-varnisher
;   1 planer
;   1 highspeed-saw
;   1 spray-varnisher
;   1 saw
; random seed: 484690

(define (problem wood-prob-s29)
  (:domain woodworking)
  (:objects
    grinder0 - grinder
    glazer0 - glazer
    immersion-varnisher0 - immersion-varnisher
    planer0 - planer
    highspeed-saw0 - highspeed-saw
    spray-varnisher0 - spray-varnisher
    saw0 - saw
    white green blue black mauve red - acolour
    beech teak walnut mahogany pine oak cherry - awood
    p0 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 - part
    b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 - board
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
    (has-colour glazer0 blue)
    (has-colour glazer0 natural)
    (has-colour glazer0 mauve)
    (has-colour glazer0 green)
    (has-colour glazer0 black)
    (has-colour glazer0 white)
    (has-colour immersion-varnisher0 blue)
    (has-colour immersion-varnisher0 natural)
    (has-colour immersion-varnisher0 mauve)
    (has-colour immersion-varnisher0 green)
    (has-colour immersion-varnisher0 white)
    (has-colour immersion-varnisher0 red)
    (empty highspeed-saw0)
    (has-colour spray-varnisher0 blue)
    (has-colour spray-varnisher0 natural)
    (has-colour spray-varnisher0 mauve)
    (has-colour spray-varnisher0 green)
    (has-colour spray-varnisher0 white)
    (has-colour spray-varnisher0 red)
    (unused p0)
    (goalsize p0 small)
    
    
    
    
    (available p1)
    (colour p1 green)
    (wood p1 oak)
    (surface-condition p1 smooth)
    (treatment p1 varnished)
    (goalsize p1 large)
    
    
    
    
    (unused p2)
    (goalsize p2 medium)
    
    
    
    
    (unused p3)
    (goalsize p3 small)
    
    
    
    
    (unused p4)
    (goalsize p4 small)
    
    
    
    
    (unused p5)
    (goalsize p5 medium)
    
    
    
    
    (available p6)
    (colour p6 white)
    (wood p6 beech)
    (surface-condition p6 rough)
    (treatment p6 colourfragments)
    (goalsize p6 large)
    
    
    
    
    (available p7)
    (colour p7 natural)
    (wood p7 oak)
    (surface-condition p7 verysmooth)
    (treatment p7 colourfragments)
    (goalsize p7 medium)
    
    
    
    
    (unused p8)
    (goalsize p8 medium)
    
    
    
    
    (unused p9)
    (goalsize p9 small)
    
    
    
    
    (unused p10)
    (goalsize p10 medium)
    
    
    
    
    (unused p11)
    (goalsize p11 small)
    
    
    
    
    (unused p12)
    (goalsize p12 small)
    
    
    
    
    (available p13)
    (colour p13 black)
    (wood p13 walnut)
    (surface-condition p13 smooth)
    (treatment p13 glazed)
    (goalsize p13 large)
    
    
    
    
    (unused p14)
    (goalsize p14 medium)
    
    
    
    
    (unused p15)
    (goalsize p15 small)
    
    
    
    
    (unused p16)
    (goalsize p16 large)
    
    
    
    
    (unused p17)
    (goalsize p17 small)
    
    
    
    
    (unused p18)
    (goalsize p18 small)
    
    
    
    
    (unused p19)
    (goalsize p19 large)
    
    
    
    
    (available p20)
    (colour p20 mauve)
    (wood p20 walnut)
    (surface-condition p20 smooth)
    (treatment p20 glazed)
    (goalsize p20 medium)
    
    
    
    
    (unused p21)
    (goalsize p21 small)
    
    
    
    
    (unused p22)
    (goalsize p22 small)
    
    
    
    
    (unused p23)
    (goalsize p23 medium)
    
    
    
    
    (unused p24)
    (goalsize p24 medium)
    
    
    
    
    (unused p25)
    (goalsize p25 small)
    
    
    
    
    (unused p26)
    (goalsize p26 medium)
    
    
    
    
    (boardsize b0 s4)
    (wood b0 cherry)
    (surface-condition b0 rough)
    (available b0)
    (boardsize b1 s2)
    (wood b1 cherry)
    (surface-condition b1 rough)
    (available b1)
    (boardsize b2 s4)
    (wood b2 mahogany)
    (surface-condition b2 rough)
    (available b2)
    (boardsize b3 s2)
    (wood b3 mahogany)
    (surface-condition b3 rough)
    (available b3)
    (boardsize b4 s5)
    (wood b4 oak)
    (surface-condition b4 rough)
    (available b4)
    (boardsize b5 s4)
    (wood b5 oak)
    (surface-condition b5 rough)
    (available b5)
    (boardsize b6 s2)
    (wood b6 pine)
    (surface-condition b6 smooth)
    (available b6)
    (boardsize b7 s2)
    (wood b7 walnut)
    (surface-condition b7 rough)
    (available b7)
    (boardsize b8 s8)
    (wood b8 teak)
    (surface-condition b8 rough)
    (available b8)
    (boardsize b9 s1)
    (wood b9 beech)
    (surface-condition b9 rough)
    (available b9)
  )
  (:goal
    (and
      (available p0)
      (wood p0 walnut)
      (treatment p0 varnished)
      (available p1)
      (colour p1 white)
      (surface-condition p1 verysmooth)
      (treatment p1 glazed)
      (available p2)
      (colour p2 white)
      (wood p2 oak)
      (treatment p2 varnished)
      (available p3)
      (colour p3 blue)
      (wood p3 walnut)
      (surface-condition p3 smooth)
      (available p4)
      (colour p4 natural)
      (wood p4 mahogany)
      (available p5)
      (colour p5 blue)
      (treatment p5 varnished)
      (available p6)
      (wood p6 beech)
      (surface-condition p6 smooth)
      (available p7)
      (colour p7 blue)
      (treatment p7 glazed)
      (available p8)
      (colour p8 green)
      (surface-condition p8 verysmooth)
      (treatment p8 varnished)
      (available p9)
      (colour p9 natural)
      (wood p9 beech)
      (surface-condition p9 verysmooth)
      (available p10)
      (colour p10 natural)
      (surface-condition p10 smooth)
      (treatment p10 varnished)
      (available p11)
      (colour p11 natural)
      (wood p11 cherry)
      (surface-condition p11 verysmooth)
      (treatment p11 glazed)
      (available p12)
      (colour p12 mauve)
      (surface-condition p12 smooth)
      (available p13)
      (colour p13 blue)
      (treatment p13 varnished)
      (available p14)
      (wood p14 teak)
      (surface-condition p14 smooth)
      (treatment p14 glazed)
      (available p15)
      (colour p15 green)
      (treatment p15 glazed)
      (available p16)
      (colour p16 blue)
      (wood p16 teak)
      (surface-condition p16 smooth)
      (treatment p16 varnished)
      (available p17)
      (colour p17 black)
      (wood p17 pine)
      (treatment p17 glazed)
      (available p18)
      (colour p18 natural)
      (wood p18 oak)
      (surface-condition p18 verysmooth)
      (treatment p18 varnished)
      (available p19)
      (colour p19 mauve)
      (wood p19 teak)
      (surface-condition p19 smooth)
      (treatment p19 varnished)
      (available p20)
      (surface-condition p20 smooth)
      (treatment p20 varnished)
      (available p21)
      (colour p21 natural)
      (wood p21 oak)
      (surface-condition p21 verysmooth)
      (treatment p21 glazed)
      (available p22)
      (wood p22 cherry)
      (surface-condition p22 smooth)
      (available p23)
      (wood p23 cherry)
      (surface-condition p23 smooth)
      (treatment p23 glazed)
      (available p24)
      (colour p24 white)
      (surface-condition p24 smooth)
      (available p25)
      (colour p25 red)
      (surface-condition p25 verysmooth)
      (treatment p25 varnished)
      (available p26)
      (colour p26 white)
      (wood p26 oak)
      (surface-condition p26 smooth)
    )
  )
  
)
