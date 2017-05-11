
(define (problem meet)
    (:domain meeting)
  (:objects  pioneer1
             person100 person101 person102 person103 person104 person105
             corridor2 corridor7 corridor11 corridor3 corridor5 corridor9
             corridor12 corridor13
             room450 room449 room448 room447 room446 room4405 room4404 
             room4401 room4402 room4403 room442 room441 room407 room422
             door4503 door449450 door4493 door449448 door4483 door4473 
             door4463 door44053 door44043 door40711 door4413 door4423 
             door44013 door44023 door44032 door35 door912 door42213)


(:init


(robby pioneer1)

(person person100)
(person person101)
(person person102)
(person person103) 
(person person104) 
(person person105)

(corridor corridor2)  (corridor corridor7) (corridor corridor11)
(corridor corridor3)  (corridor corridor5) (corridor corridor9)
(corridor corridor12) (corridor corridor13)

(room room450)   (room room449)  (room room448)   (room room447)
(room room446)   (room room4405) (room room4404)  (room room4401) 
(room room4402)  (room room4403) (room room442)   (room room441) 
(room room407)   (room room422)  (room corridor3) (room corridor5)
(room corridor9) (room corridor12)

(door door4503)  (door door449450) (door door4493) (door door449448)
(door door4483)  (door door4473)   (door door4463) (door  door44053)
(door door44043) (door door40711)  (door door4413) (door door4423) 
(door door44013) (door door44023)  (door door44032)
(door door35)    (door door912)    (door door42213)


(at pioneer1 corridor3) 
(at person100 room4403) (at person101 room450) (at person102 room422) 
(at person103 room441) (at person104 room441) (at person105 room441)
(meetingroom room441) 
(free room441)
(connected corridor2 corridor3)   (connected corridor3 corridor2)
(connected corridor11 corridor5)  (connected corridor5 corridor11)
(connected corridor5 corridor7)   (connected corridor7 corridor5)
(connected corridor7 corridor9)   (connected corridor9 corridor7)
(connected corridor11 corridor9)  (connected corridor9 corridor11)
(connected corridor12 corridor13) (connected corridor13 corridor12)

(adjacent door912 corridor9)
(adjacent door912 corridor12)
(adjacent door35 corridor3)
(adjacent door35 corridor5)
(adjacent door42213 corridor13)
(adjacent door42213 room422)
(adjacent door449450 room450)
(adjacent door449450 room449)
(adjacent door4493 corridor3)
(adjacent door4493 room449)
(adjacent door449448 room449)
(adjacent door449448 room448)
(adjacent door4483 room448)
(adjacent door4483 corridor3)
(adjacent door4473 corridor3)
(adjacent door4473 room447)  
(adjacent door4463 corridor3)
(adjacent door4463 room446)                
(adjacent door44053 corridor3)
(adjacent door44053 room4450)
(adjacent door44043 corridor3)
(adjacent door44043 room4404)
(adjacent door40711 room407)
(adjacent door40711 corridor11)
(adjacent door4423 corridor3)
(adjacent door4423 room442)         
(adjacent door4413 corridor3)
(adjacent door4413 room441)
(adjacent door44032 corridor2)
(adjacent door44032 room4403)         
(adjacent door44023 corridor3)
(adjacent door44023 room4402)        
(adjacent door44013 corridor3)
(adjacent door44013 room4401))
         

(:goal (and (informed person100) (informed person101) (informed person102)
      (informed person103) (informed person104) (informed person105))))
