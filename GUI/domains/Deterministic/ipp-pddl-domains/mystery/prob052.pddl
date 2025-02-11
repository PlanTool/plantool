(DEFINE (PROBLEM STRIPS-MYSTY-11)
   (:DOMAIN MYSTERY-STRIPS)
   (:OBJECTS HAMBURGER HAM SHRIMP MUFFIN YOGURT SCALLOP TUNA OKRA
             CUCUMBER MARZIPAN TOMATO MELON FLOUNDER KALE SNICKERS LUBRICITY
             HANGOVER BOILS DEPRESSION GRIEF PROSTATITIS JEALOUSY SCIATICA
             DREAD ABRASION ANXIETY ANGER LONELINESS ANGINA LACERATION
             JEALOUSY-1 GRIEF-2 LACERATION-8 PROSTATITIS-3 ANXIETY-4
             DEPRESSION-7 ANGINA-16 SCIATICA-5 HANGOVER-6 LONELINESS-15
             ABRASION-13 DREAD-14 MANITOBA PLUTO MARS)
   (:INIT (FOOD HAMBURGER)
          (FOOD HAM)
          (FOOD SHRIMP)
          (FOOD MUFFIN)
          (FOOD YOGURT)
          (FOOD SCALLOP)
          (FOOD TUNA)
          (FOOD OKRA)
          (FOOD CUCUMBER)
          (FOOD MARZIPAN)
          (FOOD TOMATO)
          (FOOD MELON)
          (FOOD FLOUNDER)
          (FOOD KALE)
          (FOOD SNICKERS)
          (PLEASURE LUBRICITY)
          (PAIN HANGOVER)
          (PAIN BOILS)
          (PAIN DEPRESSION)
          (PAIN GRIEF)
          (PAIN PROSTATITIS)
          (PAIN JEALOUSY)
          (PAIN SCIATICA)
          (PAIN DREAD)
          (PAIN ABRASION)
          (PAIN ANXIETY)
          (PAIN ANGER)
          (PAIN LONELINESS)
          (PAIN ANGINA)
          (PAIN LACERATION)
          (PAIN JEALOUSY-1)
          (PAIN GRIEF-2)
          (PAIN LACERATION-8)
          (PAIN PROSTATITIS-3)
          (PAIN ANXIETY-4)
          (PAIN DEPRESSION-7)
          (PAIN ANGINA-16)
          (PAIN SCIATICA-5)
          (PAIN HANGOVER-6)
          (PAIN LONELINESS-15)
          (PAIN ABRASION-13)
          (PAIN DREAD-14)
          (PROVINCE MANITOBA)
          (PLANET PLUTO)
          (PLANET MARS)
          (EATS TOMATO KALE)
          (EATS TOMATO SNICKERS)
          (CRAVES DEPRESSION HAM)
          (EATS YOGURT OKRA)
          (EATS TUNA OKRA)
          (EATS MUFFIN YOGURT)
          (EATS SNICKERS MELON)
          (EATS MUFFIN SHRIMP)
          (LOCALE MARZIPAN MANITOBA)
          (LOCALE OKRA MANITOBA)
          (LOCALE YOGURT MANITOBA)
          (CRAVES LUBRICITY CUCUMBER)
          (EATS MELON MARZIPAN)
          (CRAVES DREAD YOGURT)
          (LOCALE MELON MANITOBA)
          (LOCALE TUNA MANITOBA)
          (CRAVES LONELINESS OKRA)
          (CRAVES LACERATION-8 TOMATO)
          (EATS CUCUMBER FLOUNDER)
          (EATS OKRA YOGURT)
          (CRAVES SCIATICA-5 FLOUNDER)
          (LOCALE MUFFIN MANITOBA)
          (CRAVES GRIEF HAM)
          (CRAVES SCIATICA MUFFIN)
          (EATS MARZIPAN CUCUMBER)
          (EATS CUCUMBER MARZIPAN)
          (EATS YOGURT SCALLOP)
          (LOCALE SCALLOP MANITOBA)
          (EATS FLOUNDER KALE)
          (CRAVES GRIEF-2 MARZIPAN)
          (EATS OKRA FLOUNDER)
          (EATS KALE TOMATO)
          (CRAVES DEPRESSION-7 MELON)
          (CRAVES ANXIETY TUNA)
          (EATS FLOUNDER CUCUMBER)
          (CRAVES HANGOVER-6 FLOUNDER)
          (EATS YOGURT MUFFIN)
          (CRAVES HANGOVER HAMBURGER)
          (CRAVES ANGINA-16 FLOUNDER)
          (EATS MELON SNICKERS)
          (ORBITS PLUTO MARS)
          (EATS TUNA SCALLOP)
          (CRAVES JEALOUSY SHRIMP)
          (CRAVES JEALOUSY-1 MARZIPAN)
          (EATS FLOUNDER OKRA)
          (EATS HAMBURGER HAM)
          (CRAVES PROSTATITIS-3 TOMATO)
          (EATS SNICKERS TOMATO)
          (EATS SHRIMP HAM)
          (CRAVES ANGINA CUCUMBER)
          (EATS SCALLOP YOGURT)
          (CRAVES BOILS HAM)
          (EATS MARZIPAN MELON)
          (CRAVES LONELINESS-15 KALE)
          (EATS HAM SHRIMP)
          (LOCALE HAMBURGER MANITOBA)
          (LOCALE HAM MANITOBA)
          (EATS FLOUNDER TOMATO)
          (CRAVES DREAD-14 SNICKERS)
          (CRAVES LACERATION CUCUMBER)
          (LOCALE KALE MANITOBA)
          (LOCALE TOMATO MANITOBA)
          (EATS HAM HAMBURGER)
          (LOCALE CUCUMBER MANITOBA)
          (LOCALE FLOUNDER MANITOBA)
          (CRAVES ABRASION SCALLOP)
          (EATS SHRIMP MUFFIN)
          (LOCALE SNICKERS MANITOBA)
          (LOCALE SHRIMP MANITOBA)
          (CRAVES ANXIETY-4 TOMATO)
          (EATS OKRA TUNA)
          (EATS MUFFIN HAMBURGER)
          (CRAVES ANGER OKRA)
          (EATS HAMBURGER MUFFIN)
          (EATS SCALLOP TUNA)
          (EATS KALE FLOUNDER)
          (HARMONY LUBRICITY MARS)
          (EATS TOMATO FLOUNDER)
          (CRAVES ABRASION-13 SNICKERS)
          (CRAVES PROSTATITIS SHRIMP))
   (:GOAL (AND (CRAVES GRIEF-2 SNICKERS))))