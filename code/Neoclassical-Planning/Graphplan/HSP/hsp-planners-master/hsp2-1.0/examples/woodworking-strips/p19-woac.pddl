; woodworking task with 27 parts and 120% wood
; Machines:
;   1 grinder
;   1 glazer
;   1 immersion-varnisher
;   1 planer
;   1 highspeed-saw
;   1 spray-varnisher
;   1 saw
; random seed: 331662

(define (problem wood-prob-s19)
  (:domain woodworking)
  (:objects
    grinder0 - grinder
    glazer0 - glazer
    immersion-varnisher0 - immersion-varnisher
    planer0 - planer
    highspeed-saw0 - highspeed-saw
    spray-varnisher0 - spray-varnisher
    saw0 - saw
    black green white red mauve blue - acolour
    oak mahogany walnut cherry teak pine beech - awood
    p0 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 - part
    b0 b1 b2 b3 b4 b5 b6 b7 b8 - board
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
    (has-colour glazer0 blue)
    (has-colour glazer0 natural)
    (has-colour glazer0 white)
    (has-colour glazer0 black)
    (has-colour immersion-varnisher0 blue)
    (has-colour immersion-varnisher0 natural)
    (has-colour immersion-varnisher0 mauve)
    (has-colour immersion-varnisher0 green)
    (has-colour immersion-varnisher0 black)
    (has-colour immersion-varnisher0 white)
    (has-colour immersion-varnisher0 red)
    (empty highspeed-saw0)
    (has-colour spray-varnisher0 blue)
    (has-colour spray-varnisher0 natural)
    (has-colour spray-varnisher0 mauve)
    (has-colour spray-varnisher0 green)
    (has-colour spray-varnisher0 black)
    (has-colour spray-varnisher0 white)
    (has-colour spray-varnisher0 red)
    (available p0)
    (colour p0 mauve)
    (wood p0 pine)
    (surface-condition p0 smooth)
    (treatment p0 glazed)
    (goalsize p0 large)
    
    
    
    
    (unused p1)
    (goalsize p1 large)
    
    
    
    
    (available p2)
    (colour p2 red)
    (wood p2 teak)
    (surface-condition p2 smooth)
    (treatment p2 glazed)
    (goalsize p2 large)
    
    
    
    
    (available p3)
    (colour p3 natural)
    (wood p3 cherry)
    (surface-condition p3 verysmooth)
    (treatment p3 glazed)
    (goalsize p3 small)
    
    
    
    
    (unused p4)
    (goalsize p4 large)
    
    
    
    
    (unused p5)
    (goalsize p5 large)
    
    
    
    
    (unused p6)
    (goalsize p6 small)
    
    
    
    
    (unused p7)
    (goalsize p7 small)
    
    
    
    
    (unused p8)
    (goalsize p8 large)
    
    
    
    
    (unused p9)
    (goalsize p9 medium)
    
    
    
    
    (unused p10)
    (goalsize p10 small)
    
    
    
    
    (unused p11)
    (goalsize p11 small)
    
    
    
    
    (unused p12)
    (goalsize p12 large)
    
    
    
    
    (unused p13)
    (goalsize p13 small)
    
    
    
    
    (unused p14)
    (goalsize p14 small)
    
    
    
    
    (available p15)
    (colour p15 red)
    (wood p15 beech)
    (surface-condition p15 verysmooth)
    (treatment p15 varnished)
    (goalsize p15 medium)
    
    
    
    
    (unused p16)
    (goalsize p16 large)
    
    
    
    
    (unused p17)
    (goalsize p17 medium)
    
    
    
    
    (unused p18)
    (goalsize p18 medium)
    
    
    
    
    (available p19)
    (colour p19 mauve)
    (wood p19 cherry)
    (surface-condition p19 smooth)
    (treatment p19 colourfragments)
    (goalsize p19 medium)
    
    
    
    
    (unused p20)
    (goalsize p20 medium)
    
    
    
    
    (unused p21)
    (goalsize p21 large)
    
    
    
    
    (available p22)
    (colour p22 blue)
    (wood p22 teak)
    (surface-condition p22 rough)
    (treatment p22 colourfragments)
    (goalsize p22 small)
    
    
    
    
    (unused p23)
    (goalsize p23 medium)
    
    
    
    
    (available p24)
    (colour p24 natural)
    (wood p24 pine)
    (surface-condition p24 verysmooth)
    (treatment p24 colourfragments)
    (goalsize p24 small)
    
    
    
    
    (unused p25)
    (goalsize p25 large)
    
    
    
    
    (available p26)
    (colour p26 mauve)
    (wood p26 oak)
    (surface-condition p26 verysmooth)
    (treatment p26 colourfragments)
    (goalsize p26 small)
    
    
    
    
    (boardsize b0 s5)
    (wood b0 cherry)
    (surface-condition b0 smooth)
    (available b0)
    (boardsize b1 s8)
    (wood b1 mahogany)
    (surface-condition b1 rough)
    (available b1)
    (boardsize b2 s5)
    (wood b2 oak)
    (surface-condition b2 rough)
    (available b2)
    (boardsize b3 s8)
    (wood b3 pine)
    (surface-condition b3 rough)
    (available b3)
    (boardsize b4 s9)
    (wood b4 walnut)
    (surface-condition b4 rough)
    (available b4)
    (boardsize b5 s3)
    (wood b5 walnut)
    (surface-condition b5 rough)
    (available b5)
    (boardsize b6 s2)
    (wood b6 teak)
    (surface-condition b6 rough)
    (available b6)
    (boardsize b7 s8)
    (wood b7 beech)
    (surface-condition b7 rough)
    (available b7)
    (boardsize b8 s3)
    (wood b8 beech)
    (surface-condition b8 rough)
    (available b8)
  )
  (:goal
    (and
      (available p0)
      (colour p0 black)
      (wood p0 pine)
      (surface-condition p0 smooth)
      (treatment p0 varnished)
      (available p1)
      (wood p1 beech)
      (treatment p1 varnished)
      (available p2)
      (surface-condition p2 smooth)
      (treatment p2 varnished)
      (available p3)
      (colour p3 white)
      (surface-condition p3 smooth)
      (available p4)
      (colour p4 mauve)
      (wood p4 walnut)
      (surface-condition p4 smooth)
      (treatment p4 varnished)
      (available p5)
      (colour p5 natural)
      (wood p5 walnut)
      (surface-condition p5 smooth)
      (treatment p5 glazed)
      (available p6)
      (colour p6 natural)
      (wood p6 teak)
      (available p7)
      (wood p7 cherry)
      (treatment p7 varnished)
      (available p8)
      (colour p8 blue)
      (wood p8 pine)
      (available p9)
      (surface-condition p9 verysmooth)
      (treatment p9 varnished)
      (available p10)
      (colour p10 blue)
      (wood p10 pine)
      (treatment p10 varnished)
      (available p11)
      (colour p11 mauve)
      (surface-condition p11 verysmooth)
      (treatment p11 varnished)
      (available p12)
      (wood p12 cherry)
      (treatment p12 glazed)
      (available p13)
      (colour p13 white)
      (treatment p13 glazed)
      (available p14)
      (wood p14 beech)
      (surface-condition p14 verysmooth)
      (available p15)
      (wood p15 beech)
      (surface-condition p15 smooth)
      (available p16)
      (wood p16 mahogany)
      (treatment p16 varnished)
      (available p17)
      (colour p17 natural)
      (treatment p17 glazed)
      (available p18)
      (colour p18 green)
      (wood p18 pine)
      (surface-condition p18 smooth)
      (treatment p18 varnished)
      (available p19)
      (colour p19 natural)
      (wood p19 cherry)
      (surface-condition p19 verysmooth)
      (treatment p19 varnished)
      (available p20)
      (colour p20 blue)
      (wood p20 oak)
      (available p21)
      (colour p21 black)
      (wood p21 beech)
      (surface-condition p21 verysmooth)
      (treatment p21 glazed)
      (available p22)
      (wood p22 teak)
      (treatment p22 varnished)
      (available p23)
      (wood p23 walnut)
      (surface-condition p23 smooth)
      (treatment p23 glazed)
      (available p24)
      (colour p24 blue)
      (wood p24 pine)
      (surface-condition p24 smooth)
      (treatment p24 varnished)
      (available p25)
      (colour p25 blue)
      (wood p25 mahogany)
      (available p26)
      (colour p26 red)
      (wood p26 oak)
      (surface-condition p26 smooth)
      (treatment p26 varnished)
    )
  )
  
)
