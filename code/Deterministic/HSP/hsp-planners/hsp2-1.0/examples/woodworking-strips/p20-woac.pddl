; woodworking task with 30 parts and 120% wood
; Machines:
;   1 grinder
;   1 glazer
;   1 immersion-varnisher
;   1 planer
;   1 highspeed-saw
;   1 spray-varnisher
;   1 saw
; random seed: 702790

(define (problem wood-prob-s20)
  (:domain woodworking)
  (:objects
    grinder0 - grinder
    glazer0 - glazer
    immersion-varnisher0 - immersion-varnisher
    planer0 - planer
    highspeed-saw0 - highspeed-saw
    spray-varnisher0 - spray-varnisher
    saw0 - saw
    red green blue white black mauve - acolour
    mahogany teak walnut beech oak cherry pine - awood
    p0 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 p27 p28 p29 - part
    b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 - board
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
    (has-colour immersion-varnisher0 green)
    (has-colour immersion-varnisher0 white)
    (has-colour immersion-varnisher0 red)
    (empty highspeed-saw0)
    (has-colour spray-varnisher0 blue)
    (has-colour spray-varnisher0 natural)
    (has-colour spray-varnisher0 mauve)
    (has-colour spray-varnisher0 black)
    (has-colour spray-varnisher0 green)
    (has-colour spray-varnisher0 white)
    (has-colour spray-varnisher0 red)
    (unused p0)
    (goalsize p0 medium)
    
    
    
    
    (unused p1)
    (goalsize p1 small)
    
    
    
    
    (unused p2)
    (goalsize p2 small)
    
    
    
    
    (available p3)
    (colour p3 blue)
    (wood p3 pine)
    (surface-condition p3 smooth)
    (treatment p3 glazed)
    (goalsize p3 small)
    
    
    
    
    (unused p4)
    (goalsize p4 large)
    
    
    
    
    (unused p5)
    (goalsize p5 medium)
    
    
    
    
    (unused p6)
    (goalsize p6 medium)
    
    
    
    
    (unused p7)
    (goalsize p7 small)
    
    
    
    
    (unused p8)
    (goalsize p8 medium)
    
    
    
    
    (unused p9)
    (goalsize p9 small)
    
    
    
    
    (unused p10)
    (goalsize p10 medium)
    
    
    
    
    (unused p11)
    (goalsize p11 large)
    
    
    
    
    (unused p12)
    (goalsize p12 medium)
    
    
    
    
    (unused p13)
    (goalsize p13 small)
    
    
    
    
    (unused p14)
    (goalsize p14 medium)
    
    
    
    
    (unused p15)
    (goalsize p15 small)
    
    
    
    
    (unused p16)
    (goalsize p16 large)
    
    
    
    
    (unused p17)
    (goalsize p17 large)
    
    
    
    
    (unused p18)
    (goalsize p18 large)
    
    
    
    
    (available p19)
    (colour p19 white)
    (wood p19 mahogany)
    (surface-condition p19 rough)
    (treatment p19 colourfragments)
    (goalsize p19 medium)
    
    
    
    
    (unused p20)
    (goalsize p20 large)
    
    
    
    
    (unused p21)
    (goalsize p21 large)
    
    
    
    
    (unused p22)
    (goalsize p22 large)
    
    
    
    
    (unused p23)
    (goalsize p23 small)
    
    
    
    
    (available p24)
    (colour p24 black)
    (wood p24 teak)
    (surface-condition p24 verysmooth)
    (treatment p24 glazed)
    (goalsize p24 large)
    
    
    
    
    (unused p25)
    (goalsize p25 small)
    
    
    
    
    (unused p26)
    (goalsize p26 small)
    
    
    
    
    (unused p27)
    (goalsize p27 small)
    
    
    
    
    (unused p28)
    (goalsize p28 medium)
    
    
    
    
    (unused p29)
    (goalsize p29 large)
    
    
    
    
    (boardsize b0 s9)
    (wood b0 cherry)
    (surface-condition b0 rough)
    (available b0)
    (boardsize b1 s2)
    (wood b1 cherry)
    (surface-condition b1 rough)
    (available b1)
    (boardsize b2 s9)
    (wood b2 mahogany)
    (surface-condition b2 rough)
    (available b2)
    (boardsize b3 s5)
    (wood b3 mahogany)
    (surface-condition b3 smooth)
    (available b3)
    (boardsize b4 s9)
    (wood b4 oak)
    (surface-condition b4 rough)
    (available b4)
    (boardsize b5 s6)
    (wood b5 pine)
    (surface-condition b5 rough)
    (available b5)
    (boardsize b6 s5)
    (wood b6 walnut)
    (surface-condition b6 rough)
    (available b6)
    (boardsize b7 s11)
    (wood b7 teak)
    (surface-condition b7 rough)
    (available b7)
    (boardsize b8 s4)
    (wood b8 teak)
    (surface-condition b8 rough)
    (available b8)
    (boardsize b9 s5)
    (wood b9 beech)
    (surface-condition b9 rough)
    (available b9)
    (boardsize b10 s1)
    (wood b10 beech)
    (surface-condition b10 smooth)
    (available b10)
  )
  (:goal
    (and
      (available p0)
      (colour p0 blue)
      (wood p0 beech)
      (surface-condition p0 smooth)
      (available p1)
      (colour p1 mauve)
      (wood p1 walnut)
      (surface-condition p1 verysmooth)
      (available p2)
      (wood p2 mahogany)
      (surface-condition p2 smooth)
      (available p3)
      (colour p3 black)
      (surface-condition p3 verysmooth)
      (available p4)
      (colour p4 black)
      (wood p4 walnut)
      (surface-condition p4 smooth)
      (treatment p4 varnished)
      (available p5)
      (surface-condition p5 verysmooth)
      (treatment p5 glazed)
      (available p6)
      (colour p6 mauve)
      (wood p6 cherry)
      (surface-condition p6 verysmooth)
      (available p7)
      (colour p7 black)
      (wood p7 beech)
      (surface-condition p7 smooth)
      (treatment p7 varnished)
      (available p8)
      (colour p8 natural)
      (surface-condition p8 smooth)
      (available p9)
      (colour p9 green)
      (wood p9 pine)
      (surface-condition p9 verysmooth)
      (available p10)
      (colour p10 mauve)
      (treatment p10 glazed)
      (available p11)
      (colour p11 green)
      (wood p11 pine)
      (surface-condition p11 verysmooth)
      (available p12)
      (colour p12 red)
      (wood p12 teak)
      (available p13)
      (colour p13 green)
      (wood p13 mahogany)
      (surface-condition p13 smooth)
      (treatment p13 glazed)
      (available p14)
      (colour p14 white)
      (surface-condition p14 verysmooth)
      (available p15)
      (wood p15 beech)
      (treatment p15 varnished)
      (available p16)
      (colour p16 white)
      (wood p16 cherry)
      (surface-condition p16 smooth)
      (treatment p16 glazed)
      (available p17)
      (wood p17 teak)
      (treatment p17 varnished)
      (available p18)
      (colour p18 natural)
      (surface-condition p18 verysmooth)
      (available p19)
      (colour p19 black)
      (treatment p19 glazed)
      (available p20)
      (surface-condition p20 smooth)
      (treatment p20 glazed)
      (available p21)
      (wood p21 oak)
      (surface-condition p21 smooth)
      (available p22)
      (colour p22 black)
      (wood p22 teak)
      (surface-condition p22 smooth)
      (available p23)
      (colour p23 natural)
      (wood p23 pine)
      (surface-condition p23 verysmooth)
      (treatment p23 glazed)
      (available p24)
      (colour p24 red)
      (wood p24 teak)
      (surface-condition p24 smooth)
      (treatment p24 varnished)
      (available p25)
      (colour p25 red)
      (surface-condition p25 smooth)
      (available p26)
      (wood p26 beech)
      (treatment p26 glazed)
      (available p27)
      (colour p27 blue)
      (wood p27 mahogany)
      (available p28)
      (surface-condition p28 smooth)
      (treatment p28 varnished)
      (available p29)
      (colour p29 black)
      (wood p29 mahogany)
      (surface-condition p29 smooth)
      (treatment p29 varnished)
    )
  )
  
)
