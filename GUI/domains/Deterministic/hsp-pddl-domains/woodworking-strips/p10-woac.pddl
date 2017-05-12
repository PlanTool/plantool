; woodworking task with 30 parts and 140% wood
; Machines:
;   1 grinder
;   1 glazer
;   1 immersion-varnisher
;   1 planer
;   1 highspeed-saw
;   1 spray-varnisher
;   1 saw
; random seed: 747708

(define (problem wood-prob-s10)
  (:domain woodworking)
  (:objects
    grinder0 - grinder
    glazer0 - glazer
    immersion-varnisher0 - immersion-varnisher
    planer0 - planer
    highspeed-saw0 - highspeed-saw
    spray-varnisher0 - spray-varnisher
    saw0 - saw
    black mauve white green blue red - acolour
    teak beech cherry walnut pine mahogany oak - awood
    p0 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 p27 p28 p29 - part
    b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 - board
    s0 s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 - aboardsize
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
    (has-colour glazer0 blue)
    (has-colour glazer0 natural)
    (has-colour glazer0 mauve)
    (has-colour glazer0 green)
    (has-colour glazer0 black)
    (has-colour glazer0 white)
    (has-colour glazer0 red)
    (has-colour immersion-varnisher0 blue)
    (has-colour immersion-varnisher0 natural)
    (has-colour immersion-varnisher0 mauve)
    (has-colour immersion-varnisher0 black)
    (has-colour immersion-varnisher0 white)
    (has-colour immersion-varnisher0 red)
    (empty highspeed-saw0)
    (has-colour spray-varnisher0 blue)
    (has-colour spray-varnisher0 natural)
    (has-colour spray-varnisher0 mauve)
    (has-colour spray-varnisher0 black)
    (has-colour spray-varnisher0 white)
    (has-colour spray-varnisher0 red)
    (unused p0)
    (goalsize p0 large)
    
    
    
    
    (unused p1)
    (goalsize p1 medium)
    
    
    
    
    (unused p2)
    (goalsize p2 small)
    
    
    
    
    (unused p3)
    (goalsize p3 large)
    
    
    
    
    (available p4)
    (colour p4 blue)
    (wood p4 oak)
    (surface-condition p4 smooth)
    (treatment p4 colourfragments)
    (goalsize p4 large)
    
    
    
    
    (unused p5)
    (goalsize p5 small)
    
    
    
    
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
    (goalsize p12 medium)
    
    
    
    
    (unused p13)
    (goalsize p13 large)
    
    
    
    
    (unused p14)
    (goalsize p14 medium)
    
    
    
    
    (unused p15)
    (goalsize p15 medium)
    
    
    
    
    (available p16)
    (colour p16 green)
    (wood p16 beech)
    (surface-condition p16 verysmooth)
    (treatment p16 varnished)
    (goalsize p16 small)
    
    
    
    
    (unused p17)
    (goalsize p17 medium)
    
    
    
    
    (available p18)
    (colour p18 natural)
    (wood p18 mahogany)
    (surface-condition p18 smooth)
    (treatment p18 glazed)
    (goalsize p18 small)
    
    
    
    
    (available p19)
    (colour p19 red)
    (wood p19 walnut)
    (surface-condition p19 verysmooth)
    (treatment p19 varnished)
    (goalsize p19 small)
    
    
    
    
    (unused p20)
    (goalsize p20 medium)
    
    
    
    
    (unused p21)
    (goalsize p21 small)
    
    
    
    
    (unused p22)
    (goalsize p22 small)
    
    
    
    
    (unused p23)
    (goalsize p23 small)
    
    
    
    
    (unused p24)
    (goalsize p24 small)
    
    
    
    
    (available p25)
    (colour p25 mauve)
    (wood p25 cherry)
    (surface-condition p25 rough)
    (treatment p25 varnished)
    (goalsize p25 small)
    
    
    
    
    (unused p26)
    (goalsize p26 large)
    
    
    
    
    (unused p27)
    (goalsize p27 medium)
    
    
    
    
    (unused p28)
    (goalsize p28 small)
    
    
    
    
    (unused p29)
    (goalsize p29 medium)
    
    
    
    
    (boardsize b0 s10)
    (wood b0 cherry)
    (surface-condition b0 rough)
    (available b0)
    (boardsize b1 s4)
    (wood b1 cherry)
    (surface-condition b1 rough)
    (available b1)
    (boardsize b2 s7)
    (wood b2 mahogany)
    (surface-condition b2 rough)
    (available b2)
    (boardsize b3 s6)
    (wood b3 oak)
    (surface-condition b3 rough)
    (available b3)
    (boardsize b4 s8)
    (wood b4 oak)
    (surface-condition b4 rough)
    (available b4)
    (boardsize b5 s6)
    (wood b5 oak)
    (surface-condition b5 smooth)
    (available b5)
    (boardsize b6 s9)
    (wood b6 pine)
    (surface-condition b6 smooth)
    (available b6)
    (boardsize b7 s3)
    (wood b7 teak)
    (surface-condition b7 smooth)
    (available b7)
    (boardsize b8 s10)
    (wood b8 beech)
    (surface-condition b8 rough)
    (available b8)
    (boardsize b9 s8)
    (wood b9 beech)
    (surface-condition b9 rough)
    (available b9)
    (boardsize b10 s1)
    (wood b10 beech)
    (surface-condition b10 rough)
    (available b10)
  )
  (:goal
    (and
      (available p0)
      (colour p0 black)
      (treatment p0 glazed)
      (available p1)
      (colour p1 mauve)
      (wood p1 oak)
      (treatment p1 glazed)
      (available p2)
      (surface-condition p2 verysmooth)
      (treatment p2 glazed)
      (available p3)
      (colour p3 white)
      (surface-condition p3 verysmooth)
      (available p4)
      (wood p4 oak)
      (surface-condition p4 verysmooth)
      (treatment p4 glazed)
      (available p5)
      (colour p5 green)
      (wood p5 oak)
      (surface-condition p5 verysmooth)
      (treatment p5 glazed)
      (available p6)
      (colour p6 black)
      (wood p6 cherry)
      (surface-condition p6 smooth)
      (treatment p6 varnished)
      (available p7)
      (colour p7 natural)
      (wood p7 mahogany)
      (surface-condition p7 smooth)
      (treatment p7 varnished)
      (available p8)
      (colour p8 natural)
      (wood p8 beech)
      (available p9)
      (surface-condition p9 smooth)
      (treatment p9 varnished)
      (available p10)
      (surface-condition p10 verysmooth)
      (treatment p10 varnished)
      (available p11)
      (colour p11 mauve)
      (surface-condition p11 smooth)
      (available p12)
      (colour p12 blue)
      (wood p12 oak)
      (surface-condition p12 smooth)
      (treatment p12 varnished)
      (available p13)
      (colour p13 black)
      (surface-condition p13 verysmooth)
      (available p14)
      (colour p14 natural)
      (treatment p14 varnished)
      (available p15)
      (wood p15 beech)
      (treatment p15 varnished)
      (available p16)
      (colour p16 red)
      (surface-condition p16 smooth)
      (available p17)
      (colour p17 black)
      (wood p17 oak)
      (surface-condition p17 smooth)
      (available p18)
      (wood p18 mahogany)
      (surface-condition p18 smooth)
      (treatment p18 varnished)
      (available p19)
      (colour p19 natural)
      (surface-condition p19 verysmooth)
      (treatment p19 glazed)
      (available p20)
      (wood p20 pine)
      (treatment p20 glazed)
      (available p21)
      (colour p21 red)
      (wood p21 beech)
      (surface-condition p21 verysmooth)
      (treatment p21 glazed)
      (available p22)
      (colour p22 blue)
      (surface-condition p22 smooth)
      (treatment p22 glazed)
      (available p23)
      (colour p23 black)
      (wood p23 pine)
      (surface-condition p23 verysmooth)
      (available p24)
      (colour p24 black)
      (treatment p24 glazed)
      (available p25)
      (colour p25 green)
      (treatment p25 glazed)
      (available p26)
      (colour p26 natural)
      (treatment p26 glazed)
      (available p27)
      (colour p27 green)
      (treatment p27 glazed)
      (available p28)
      (colour p28 mauve)
      (surface-condition p28 verysmooth)
      (treatment p28 varnished)
      (available p29)
      (colour p29 black)
      (wood p29 cherry)
      (surface-condition p29 verysmooth)
      (treatment p29 glazed)
    )
  )
  
)
