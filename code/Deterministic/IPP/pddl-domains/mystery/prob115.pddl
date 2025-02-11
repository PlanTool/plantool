(DEFINE (PROBLEM STRIPS-MYSTY-A-3)
   (:DOMAIN MYSTERY-STRIPS)
   (:OBJECTS SCALLOP FLOUNDER ARUGULA KALE TOFU SCALLION YOGURT
             PAPAYA OKRA ORANGE HAM POTATO LEMON WONDERBREAD MARZIPAN
             SWEETROLL LOBSTER TUNA EXPECTATION AESTHETICS LOVE STIMULATION
             SATIETY CURIOSITY EMPATHY INTOXICATION TRIUMPH LUBRICITY
             SCIATICA LACERATION ARIZONA BAVARIA MORAVIA SURREY OREGON
             JUPITER EARTH URANUS MARS)
   (:INIT (FOOD SCALLOP)
          (FOOD FLOUNDER)
          (FOOD ARUGULA)
          (FOOD KALE)
          (FOOD TOFU)
          (FOOD SCALLION)
          (FOOD YOGURT)
          (FOOD PAPAYA)
          (FOOD OKRA)
          (FOOD ORANGE)
          (FOOD HAM)
          (FOOD POTATO)
          (FOOD LEMON)
          (FOOD WONDERBREAD)
          (FOOD MARZIPAN)
          (FOOD SWEETROLL)
          (FOOD LOBSTER)
          (FOOD TUNA)
          (PLEASURE EXPECTATION)
          (PLEASURE AESTHETICS)
          (PLEASURE LOVE)
          (PLEASURE STIMULATION)
          (PLEASURE SATIETY)
          (PLEASURE CURIOSITY)
          (PLEASURE EMPATHY)
          (PLEASURE INTOXICATION)
          (PLEASURE TRIUMPH)
          (PLEASURE LUBRICITY)
          (PAIN SCIATICA)
          (PAIN LACERATION)
          (PROVINCE ARIZONA)
          (PROVINCE BAVARIA)
          (PROVINCE MORAVIA)
          (PROVINCE SURREY)
          (PROVINCE OREGON)
          (PLANET JUPITER)
          (PLANET EARTH)
          (PLANET URANUS)
          (PLANET MARS)
          (EATS FLOUNDER ARUGULA)
          (HARMONY SATIETY EARTH)
          (CRAVES LUBRICITY WONDERBREAD)
          (CRAVES EMPATHY ORANGE)
          (EATS SCALLOP KALE)
          (ORBITS URANUS MARS)
          (HARMONY LUBRICITY URANUS)
          (EATS MARZIPAN YOGURT)
          (EATS YOGURT MARZIPAN)
          (EATS HAM LOBSTER)
          (EATS FLOUNDER SCALLOP)
          (LOCALE PAPAYA BAVARIA)
          (LOCALE OKRA ARIZONA)
          (EATS KALE SCALLOP)
          (EATS WONDERBREAD HAM)
          (EATS SWEETROLL MARZIPAN)
          (LOCALE SCALLOP MORAVIA)
          (ORBITS EARTH URANUS)
          (EATS SCALLION OKRA)
          (LOCALE WONDERBREAD ARIZONA)
          (EATS LEMON POTATO)
          (EATS WONDERBREAD TUNA)
          (LOCALE HAM BAVARIA)
          (CRAVES TRIUMPH POTATO)
          (EATS SCALLION TOFU)
          (EATS LOBSTER SWEETROLL)
          (LOCALE YOGURT MORAVIA)
          (EATS LEMON SWEETROLL)
          (CRAVES INTOXICATION HAM)
          (EATS SWEETROLL POTATO)
          (EATS TOFU SCALLION)
          (EATS YOGURT OKRA)
          (CRAVES EXPECTATION FLOUNDER)
          (LOCALE POTATO OREGON)
          (CRAVES CURIOSITY OKRA)
          (EATS PAPAYA ARUGULA)
          (EATS OKRA PAPAYA)
          (EATS TUNA MARZIPAN)
          (LOCALE ARUGULA OREGON)
          (HARMONY EMPATHY MARS)
          (HARMONY INTOXICATION MARS)
          (EATS TOFU ORANGE)
          (ATTACKS BAVARIA MORAVIA)
          (CRAVES AESTHETICS ARUGULA)
          (LOCALE ORANGE SURREY)
          (LOCALE MARZIPAN OREGON)
          (EATS MARZIPAN SWEETROLL)
          (LOCALE FLOUNDER MORAVIA)
          (LOCALE KALE SURREY)
          (EATS ARUGULA PAPAYA)
          (ATTACKS ARIZONA BAVARIA)
          (EATS ORANGE YOGURT)
          (HARMONY AESTHETICS URANUS)
          (LOCALE LOBSTER SURREY)
          (EATS PAPAYA YOGURT)
          (LOCALE TUNA BAVARIA)
          (EATS YOGURT SCALLION)
          (HARMONY STIMULATION URANUS)
          (EATS PAPAYA OKRA)
          (EATS LOBSTER LEMON)
          (CRAVES STIMULATION SCALLION)
          (EATS LEMON TUNA)
          (EATS YOGURT PAPAYA)
          (HARMONY LOVE URANUS)
          (EATS TUNA WONDERBREAD)
          (CRAVES SATIETY YOGURT)
          (CRAVES SCIATICA POTATO)
          (EATS SWEETROLL LOBSTER)
          (CRAVES LOVE TOFU)
          (EATS YOGURT ORANGE)
          (LOCALE SWEETROLL SURREY)
          (ORBITS JUPITER EARTH)
          (CRAVES LACERATION LEMON)
          (LOCALE LEMON ARIZONA)
          (EATS MARZIPAN TUNA)
          (EATS OKRA SCALLION)
          (HARMONY EXPECTATION MARS)
          (ATTACKS SURREY OREGON)
          (HARMONY TRIUMPH URANUS)
          (EATS POTATO LEMON)
          (EATS OKRA YOGURT)
          (EATS LEMON LOBSTER)
          (EATS ARUGULA FLOUNDER)
          (LOCALE SCALLION ARIZONA)
          (EATS SCALLOP FLOUNDER)
          (EATS ORANGE TOFU)
          (EATS KALE ARUGULA)
          (EATS SWEETROLL LEMON)
          (EATS SCALLION YOGURT)
          (EATS ARUGULA KALE)
          (EATS POTATO MARZIPAN)
          (EATS TUNA LEMON)
          (HARMONY CURIOSITY EARTH)
          (EATS MARZIPAN POTATO)
          (ATTACKS MORAVIA SURREY)
          (EATS POTATO SWEETROLL)
          (EATS HAM WONDERBREAD)
          (EATS LOBSTER HAM)
          (LOCALE TOFU SURREY))
   (:GOAL (AND (CRAVES SCIATICA PAPAYA))))