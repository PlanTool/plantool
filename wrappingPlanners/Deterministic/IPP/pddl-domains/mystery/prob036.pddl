(DEFINE (PROBLEM MYSTY-36)
   (:DOMAIN MYSTERY-TYPED)
   (:OBJECTS PAPAYA-15 LAMB-16 TOFU-5 SNICKERS-6 MUFFIN-7 HAM-8
             TUNA-3 SHRIMP-4 WONDERBREAD-1 BACON-2 POTATO YOGURT
             GRAPEFRUIT KALE BROCCOLI TUNA BACON FLOUNDER PISTACHIO OKRA LEMON
             MARZIPAN BEEF HAMBURGER SCALLOP LAMB TOFU CANTELOPE LOBSTER
             HOTDOG SWEETROLL WONDERBREAD BAGUETTE ORANGE HAM PEPPER
             SCALLION MELON COD PEA APPLE CHERRY TOMATO PAPAYA TURKEY CUCUMBER
             WURST ARUGULA ONION RICE GUAVA POPOVER PEAR CHOCOLATE LETTUCE
             MUTTON SHRIMP ENDIVE SNICKERS CHICKEN MUFFIN PORK HAROSET
             - FOOD
             EMPATHY UNDERSTANDING REST SATISFACTION STIMULATION
             EXPECTATION INTOXICATION TRIUMPH LEARNING EXCITEMENT SATIETY
             CURIOSITY LOVE LUBRICITY AESTHETICS ACHIEVEMENT ENTERTAINMENT
             TRIUMPH-14 LUBRICITY-13 LOVE-12 SATISFACTION-11 CURIOSITY-10
             STIMULATION-9 EXPECTATION-32 ENTERTAINMENT-31 AESTHETICS-30
             INTOXICATION-29 UNDERSTANDING-28 ACHIEVEMENT-27 SATIETY-26
             EMPATHY-25 LEARNING-24 REST-23 EXCITEMENT-22 AESTHETICS-21
             EXCITEMENT-20 REST-19 STIMULATION-18 LEARNING-17 TRIUMPH-64
             - PLEASURE
             SCIATICA GRIEF DREAD ABRASION LACERATION ANXIETY DEPRESSION
             LONELINESS ANGINA PROSTATITIS ANGER HANGOVER BOILS GRIEF-63
             JEALOUSY ANGER-60 HANGOVER-61 DEPRESSION-62 PROSTATITIS-57
             DREAD-58 JEALOUSY-59 SCIATICA-53 LONELINESS-54 BOILS-55
             ABRASION-56 ANXIETY-52 LACERATION-50 ANGINA-51 ANGINA-49
             JEALOUSY-45 DREAD-46 HANGOVER-47 LACERATION-48 ANXIETY-41
             GRIEF-42 ANGER-43 PROSTATITIS-44 BOILS-40 DEPRESSION-39
             ABRASION-37 LONELINESS-38 HANGOVER-35 SCIATICA-36
             PROSTATITIS-127 ABRASION-128 ANGER-33 ANGINA-34 JEALOUSY-124
             DEPRESSION-125 GRIEF-126 LACERATION-122 BOILS-123 LONELINESS-119
             DREAD-120 ANXIETY-121 SCIATICA-118 DREAD-116 GRIEF-117
             LACERATION-115 ANGER-113 DEPRESSION-114 ANGINA-112 PROSTATITIS-108
             ANXIETY-109 HANGOVER-110 BOILS-111 SCIATICA-105 ABRASION-106
             JEALOUSY-107 DREAD-101 ABRASION-102 PROSTATITIS-103
             LONELINESS-104 LONELINESS-98 BOILS-99 ANGINA-100 SCIATICA-94
             HANGOVER-95 DEPRESSION-96 GRIEF-97 LACERATION-91 JEALOUSY-92
             ANGER-93 LACERATION-89 ANXIETY-90 DEPRESSION-85 ANXIETY-86
             LONELINESS-87 ANGER-88 ANGINA-82 BOILS-83 DREAD-84 JEALOUSY-80
             GRIEF-81 PROSTATITIS-77 ABRASION-78 HANGOVER-79 ANGINA-73
             BOILS-74 PROSTATITIS-75 SCIATICA-76 ABRASION-71 SCIATICA-72
             DEPRESSION-67 ANGER-68 DREAD-69 LONELINESS-70 - PAIN
             KENTUCKY QUEBEC - PROVINCE
             MERCURY SATURN MARS JUPITER - PLANET)
   (:INIT (EATS LETTUCE ARUGULA)
          (CRAVES DREAD LAMB-16)
          (CRAVES SATIETY FLOUNDER)
          (CRAVES ACHIEVEMENT LAMB)
          (EATS HOTDOG POPOVER)
          (CRAVES EMPATHY PAPAYA-15)
          (CRAVES ANGINA-82 LETTUCE)
          (EATS BACON HAM)
          (CRAVES GRIEF-117 COD)
          (HARMONY LUBRICITY JUPITER)
          (LOCALE SCALLION KENTUCKY)
          (EATS ONION OKRA)
          (CRAVES SATISFACTION-11 ORANGE)
          (EATS CUCUMBER SNICKERS)
          (EATS TOFU MUTTON)
          (CRAVES CURIOSITY-10 HAM)
          (EATS ORANGE CANTELOPE)
          (EATS SNICKERS-6 KALE)
          (EATS SNICKERS CUCUMBER)
          (EATS CHOCOLATE PEA)
          (ORBITS MERCURY SATURN)
          (LOCALE TOFU-5 KENTUCKY)
          (CRAVES HANGOVER-47 FLOUNDER)
          (HARMONY EXPECTATION-32 MARS)
          (EATS TOFU-5 GRAPEFRUIT)
          (CRAVES DEPRESSION-96 ONION)
          (EATS POTATO KALE)
          (CRAVES HANGOVER-61 SHRIMP-4)
          (CRAVES ANXIETY-90 POPOVER)
          (CRAVES ANGER-113 CHERRY)
          (EATS POTATO PAPAYA-15)
          (HARMONY ACHIEVEMENT SATURN)
          (EATS MELON LOBSTER)
          (EATS BROCCOLI SNICKERS-6)
          (EATS OKRA PAPAYA)
          (EATS GRAPEFRUIT PAPAYA-15)
          (EATS CHOCOLATE TOMATO)
          (LOCALE HAM QUEBEC)
          (EATS PEA ENDIVE)
          (EATS POPOVER LETTUCE)
          (LOCALE LAMB-16 QUEBEC)
          (EATS ENDIVE PEA)
          (EATS SNICKERS SCALLOP)
          (HARMONY INTOXICATION SATURN)
          (LOCALE OKRA KENTUCKY)
          (LOCALE PEAR QUEBEC)
          (EATS RICE SHRIMP)
          (HARMONY STIMULATION-9 JUPITER)
          (EATS POPOVER LAMB)
          (EATS MUFFIN-7 LAMB-16)
          (CRAVES UNDERSTANDING-28 TURKEY)
          (EATS KALE SHRIMP-4)
          (EATS GRAPEFRUIT TOFU-5)
          (LOCALE POPOVER KENTUCKY)
          (CRAVES AESTHETICS SCALLOP)
          (CRAVES PROSTATITIS-127 WONDERBREAD)
          (EATS TUNA-3 YOGURT)
          (CRAVES UNDERSTANDING SNICKERS-6)
          (LOCALE MELON QUEBEC)
          (CRAVES ANXIETY-109 PAPAYA)
          (EATS YOGURT TUNA-3)
          (HARMONY LEARNING-24 SATURN)
          (CRAVES DEPRESSION-125 BAGUETTE)
          (CRAVES ANGER-68 HAROSET)
          (CRAVES PROSTATITIS-103 CUCUMBER)
          (EATS TURKEY MUFFIN)
          (EATS SNICKERS-6 BROCCOLI)
          (CRAVES EXPECTATION POTATO)
          (CRAVES ANGINA-34 WONDERBREAD)
          (LOCALE ORANGE QUEBEC)
          (CRAVES BOILS-123 PEPPER)
          (EATS YOGURT BACON-2)
          (LOCALE KALE KENTUCKY)
          (CRAVES SCIATICA-94 ONION)
          (LOCALE ENDIVE KENTUCKY)
          (CRAVES ABRASION-128 WONDERBREAD)
          (CRAVES LONELINESS-119 SCALLION)
          (EATS COD GUAVA)
          (HARMONY LEARNING JUPITER)
          (CRAVES LONELINESS TOFU-5)
          (EATS ARUGULA LETTUCE)
          (EATS POPOVER LOBSTER)
          (CRAVES ANXIETY TOFU-5)
          (CRAVES ANXIETY-86 PEAR)
          (EATS TOFU-5 BACON-2)
          (HARMONY UNDERSTANDING MARS)
          (EATS FLOUNDER ENDIVE)
          (EATS PEAR WURST)
          (EATS MUTTON PEPPER)
          (EATS LETTUCE HAROSET)
          (CRAVES JEALOUSY-107 TURKEY)
          (EATS PEAR MUTTON)
          (EATS SCALLION BAGUETTE)
          (CRAVES ABRASION-71 PORK)
          (LOCALE PORK KENTUCKY)
          (CRAVES STIMULATION BACON-2)
          (CRAVES DREAD-58 WONDERBREAD-1)
          (EATS WONDERBREAD-1 BACON-2)
          (EATS ORANGE PEAR)
          (EATS PAPAYA CUCUMBER)
          (CRAVES EMPATHY-25 GUAVA)
          (EATS WONDERBREAD LETTUCE)
          (CRAVES ANGINA-73 MUFFIN)
          (CRAVES LEARNING-24 PEAR)
          (LOCALE TOMATO KENTUCKY)
          (HARMONY CURIOSITY-10 SATURN)
          (CRAVES DREAD-46 FLOUNDER)
          (EATS HAM BACON)
          (EATS BACON-2 WONDERBREAD-1)
          (HARMONY REST-19 SATURN)
          (LOCALE PEPPER KENTUCKY)
          (LOCALE SNICKERS-6 KENTUCKY)
          (EATS LAMB POPOVER)
          (EATS WURST SNICKERS)
          (CRAVES PROSTATITIS-108 PAPAYA)
          (EATS ENDIVE FLOUNDER)
          (LOCALE GUAVA KENTUCKY)
          (EATS TURKEY CHICKEN)
          (CRAVES JEALOUSY TUNA-3)
          (EATS LOBSTER POPOVER)
          (LOCALE PEA KENTUCKY)
          (EATS TURKEY COD)
          (EATS POPOVER HOTDOG)
          (EATS PISTACHIO TURKEY)
          (EATS ARUGULA CHICKEN)
          (LOCALE PAPAYA-15 KENTUCKY)
          (LOCALE CANTELOPE QUEBEC)
          (EATS HAM-8 BROCCOLI)
          (EATS SNICKERS WURST)
          (EATS BROCCOLI HAM-8)
          (EATS PEAR COD)
          (EATS LAMB MUTTON)
          (EATS CANTELOPE HAM)
          (CRAVES LONELINESS-54 YOGURT)
          (HARMONY LEARNING-17 JUPITER)
          (EATS GUAVA ARUGULA)
          (LOCALE SNICKERS KENTUCKY)
          (EATS WONDERBREAD LOBSTER)
          (EATS MUTTON LAMB)
          (CRAVES DREAD-120 SCALLION)
          (EATS MUTTON TOFU)
          (EATS MELON WONDERBREAD)
          (EATS TUNA BACON-2)
          (HARMONY LOVE-12 JUPITER)
          (LOCALE WONDERBREAD-1 KENTUCKY)
          (CRAVES STIMULATION-18 SNICKERS)
          (LOCALE MUTTON KENTUCKY)
          (LOCALE MARZIPAN KENTUCKY)
          (EATS TOFU WURST)
          (HARMONY EMPATHY-25 MARS)
          (EATS SCALLOP SNICKERS)
          (EATS PEA BEEF)
          (EATS ARUGULA POPOVER)
          (EATS HAM CANTELOPE)
          (EATS MARZIPAN LETTUCE)
          (EATS BACON-2 TOFU-5)
          (CRAVES JEALOUSY-92 GUAVA)
          (ORBITS MARS JUPITER)
          (EATS HAROSET BAGUETTE)
          (CRAVES ANGER-93 GUAVA)
          (LOCALE POTATO KENTUCKY)
          (EATS BEEF TOMATO)
          (CRAVES SCIATICA-76 MUFFIN)
          (EATS CHERRY CHICKEN)
          (EATS HAMBURGER SNICKERS)
          (CRAVES ABRASION-106 TURKEY)
          (CRAVES REST-19 ENDIVE)
          (LOCALE LEMON KENTUCKY)
          (EATS SHRIMP ENDIVE)
          (CRAVES GRIEF-63 TUNA-3)
          (CRAVES DREAD-84 LETTUCE)
          (LOCALE PAPAYA KENTUCKY)
          (CRAVES EXCITEMENT-20 SHRIMP)
          (EATS MELON SHRIMP)
          (CRAVES DREAD-69 HAROSET)
          (CRAVES EXCITEMENT-22 LETTUCE)
          (LOCALE COD QUEBEC)
          (HARMONY SATISFACTION-11 SATURN)
          (LOCALE CHERRY KENTUCKY)
          (EATS LEMON CHOCOLATE)
          (EATS SNICKERS HAMBURGER)
          (CRAVES LEARNING-17 CHICKEN)
          (CRAVES AESTHETICS-21 MUTTON)
          (CRAVES EXCITEMENT BACON)
          (HARMONY ENTERTAINMENT MARS)
          (EATS COD CHERRY)
          (EATS CHOCOLATE BEEF)
          (EATS PORK SNICKERS)
          (HARMONY AESTHETICS-30 SATURN)
          (HARMONY TRIUMPH SATURN)
          (CRAVES BOILS-83 LETTUCE)
          (EATS GUAVA CHICKEN)
          (CRAVES LACERATION-122 PEPPER)
          (CRAVES PROSTATITIS-75 MUFFIN)
          (EATS TURKEY PISTACHIO)
          (CRAVES JEALOUSY-80 ENDIVE)
          (LOCALE HAM-8 QUEBEC)
          (CRAVES LOVE-12 WONDERBREAD)
          (EATS PEPPER GUAVA)
          (EATS MUTTON PEAR)
          (EATS GUAVA PEAR)
          (LOCALE MUFFIN KENTUCKY)
          (LOCALE BAGUETTE KENTUCKY)
          (CRAVES GRIEF PAPAYA-15)
          (CRAVES JEALOUSY-124 BAGUETTE)
          (HARMONY STIMULATION SATURN)
          (EATS WURST LEMON)
          (EATS SWEETROLL APPLE)
          (CRAVES INTOXICATION KALE)
          (LOCALE SWEETROLL KENTUCKY)
          (EATS COD PEAR)
          (CRAVES LUBRICITY HAMBURGER)
          (CRAVES HANGOVER SNICKERS-6)
          (CRAVES GRIEF-42 LAMB)
          (LOCALE SHRIMP-4 QUEBEC)
          (EATS CHERRY COD)
          (CRAVES SATIETY-26 RICE)
          (LOCALE HAROSET KENTUCKY)
          (HARMONY ENTERTAINMENT-31 MARS)
          (EATS HOTDOG GUAVA)
          (EATS ONION PORK)
          (LOCALE PISTACHIO KENTUCKY)
          (EATS CHOCOLATE SNICKERS)
          (CRAVES PROSTATITIS SNICKERS-6)
          (EATS LAMB-16 MUFFIN-7)
          (EATS HAMBURGER MUFFIN)
          (CRAVES SCIATICA-72 PORK)
          (EATS PEA CHOCOLATE)
          (EATS LETTUCE WONDERBREAD)
          (EATS TOMATO CHOCOLATE)
          (EATS CHOCOLATE LEMON)
          (HARMONY LUBRICITY-13 SATURN)
          (LOCALE LETTUCE QUEBEC)
          (CRAVES PROSTATITIS-44 LAMB)
          (CRAVES GRIEF-97 ONION)
          (EATS SNICKERS PORK)
          (HARMONY AESTHETICS-21 SATURN)
          (CRAVES ANXIETY-121 SCALLION)
          (EATS HAM PISTACHIO)
          (HARMONY EXPECTATION MARS)
          (HARMONY INTOXICATION-29 MARS)
          (LOCALE CUCUMBER KENTUCKY)
          (CRAVES TRIUMPH BROCCOLI)
          (EATS CHICKEN GUAVA)
          (CRAVES REST MUFFIN-7)
          (EATS BEEF PEA)
          (EATS BACON-2 TUNA)
          (LOCALE WURST QUEBEC)
          (LOCALE ARUGULA QUEBEC)
          (CRAVES ACHIEVEMENT-27 CUCUMBER)
          (EATS HOTDOG YOGURT)
          (LOCALE TUNA-3 QUEBEC)
          (LOCALE HOTDOG KENTUCKY)
          (EATS OKRA ONION)
          (CRAVES AESTHETICS-30 PEA)
          (EATS SCALLION MARZIPAN)
          (CRAVES JEALOUSY-59 WONDERBREAD-1)
          (LOCALE BROCCOLI KENTUCKY)
          (CRAVES ANGINA-112 TOMATO)
          (EATS LETTUCE POPOVER)
          (LOCALE HAMBURGER KENTUCKY)
          (EATS ONION CHOCOLATE)
          (CRAVES EXPECTATION-32 MELON)
          (CRAVES LACERATION-48 FLOUNDER)
          (EATS TOMATO BEEF)
          (LOCALE YOGURT KENTUCKY)
          (CRAVES ANGINA-49 TUNA)
          (EATS PEAR GUAVA)
          (EATS SHRIMP RICE)
          (CRAVES JEALOUSY-45 FLOUNDER)
          (EATS WONDERBREAD-1 HAM-8)
          (CRAVES ENTERTAINMENT-31 COD)
          (CRAVES ABRASION LAMB-16)
          (CRAVES REST-23 CHOCOLATE)
          (LOCALE SHRIMP KENTUCKY)
          (EATS BACON MUFFIN)
          (EATS PAPAYA-15 ORANGE)
          (EATS GRAPEFRUIT TUNA)
          (LOCALE RICE KENTUCKY)
          (CRAVES CURIOSITY LEMON)
          (EATS RICE SCALLION)
          (HARMONY SATISFACTION MARS)
          (EATS BACON-2 YOGURT)
          (LOCALE LOBSTER KENTUCKY)
          (HARMONY EXCITEMENT-22 SATURN)
          (CRAVES DEPRESSION-85 PEAR)
          (LOCALE CHOCOLATE KENTUCKY)
          (LOCALE FLOUNDER KENTUCKY)
          (LOCALE APPLE KENTUCKY)
          (EATS WONDERBREAD-1 MUFFIN-7)
          (EATS LETTUCE CHICKEN)
          (EATS FLOUNDER MUTTON)
          (EATS YOGURT SHRIMP-4)
          (CRAVES ABRASION-37 HOTDOG)
          (CRAVES ANGINA-51 BROCCOLI)
          (EATS TUNA LAMB-16)
          (EATS LOBSTER MELON)
          (CRAVES SCIATICA-105 TURKEY)
          (CRAVES BOILS-55 YOGURT)
          (EATS PAPAYA-15 GRAPEFRUIT)
          (EATS LAMB-16 TUNA)
          (EATS TUNA GRAPEFRUIT)
          (EATS CHICKEN ARUGULA)
          (HARMONY STIMULATION-18 JUPITER)
          (EATS ARUGULA GUAVA)
          (EATS POPOVER APPLE)
          (EATS GUAVA COD)
          (LOCALE TOFU KENTUCKY)
          (CRAVES PROSTATITIS-57 WONDERBREAD-1)
          (CRAVES DEPRESSION-114 CHERRY)
          (CRAVES LONELINESS-70 HAROSET)
          (CRAVES LACERATION-115 APPLE)
          (EATS SNICKERS CHOCOLATE)
          (EATS WONDERBREAD MELON)
          (CRAVES SCIATICA-118 MELON)
          (EATS KALE SNICKERS-6)
          (EATS CHICKEN LETTUCE)
          (EATS MUTTON WURST)
          (ORBITS SATURN MARS)
          (LOCALE ONION QUEBEC)
          (EATS WURST PEAR)
          (CRAVES LACERATION-50 BROCCOLI)
          (EATS APPLE MELON)
          (CRAVES PROSTATITIS-77 CHICKEN)
          (EATS POPOVER ARUGULA)
          (CRAVES ANXIETY-41 LAMB)
          (LOCALE GRAPEFRUIT QUEBEC)
          (LOCALE SCALLOP QUEBEC)
          (LOCALE BACON QUEBEC)
          (EATS KALE POTATO)
          (CRAVES LONELINESS-87 PEAR)
          (LOCALE LAMB KENTUCKY)
          (HARMONY EMPATHY SATURN)
          (CRAVES SATISFACTION HAM-8)
          (EATS PAPAYA OKRA)
          (EATS MUFFIN HAMBURGER)
          (EATS MUTTON FLOUNDER)
          (EATS HAM-8 WONDERBREAD-1)
          (CRAVES BOILS HAM-8)
          (HARMONY CURIOSITY JUPITER)
          (CRAVES ABRASION-56 YOGURT)
          (CRAVES ABRASION-78 CHICKEN)
          (EATS PISTACHIO HAM)
          (CRAVES DREAD-101 CUCUMBER)
          (CRAVES BOILS-40 CANTELOPE)
          (EATS PORK TOMATO)
          (EATS ORANGE PAPAYA-15)
          (EATS PORK ONION)
          (EATS SHRIMP-4 KALE)
          (EATS MUFFIN BACON)
          (EATS APPLE POPOVER)
          (EATS MELON APPLE)
          (CRAVES ENTERTAINMENT TOFU)
          (HARMONY ACHIEVEMENT-27 JUPITER)
          (LOCALE CHICKEN KENTUCKY)
          (CRAVES LONELINESS-104 CUCUMBER)
          (LOCALE WONDERBREAD KENTUCKY)
          (CRAVES DEPRESSION TOFU-5)
          (EATS COD TURKEY)
          (EATS SCALLOP PEAR)
          (EATS PEPPER MUTTON)
          (EATS BEEF CHOCOLATE)
          (EATS CHICKEN TURKEY)
          (CRAVES HANGOVER-35 SWEETROLL)
          (CRAVES SCIATICA-36 SWEETROLL)
          (LOCALE TURKEY KENTUCKY)
          (EATS ENDIVE CHOCOLATE)
          (EATS PAPAYA-15 POTATO)
          (HARMONY AESTHETICS SATURN)
          (CRAVES HANGOVER-95 ONION)
          (EATS CANTELOPE ORANGE)
          (CRAVES GRIEF-81 ENDIVE)
          (EATS ENDIVE SHRIMP)
          (CRAVES ANGER-43 LAMB)
          (HARMONY REST SATURN)
          (EATS LETTUCE MARZIPAN)
          (CRAVES LOVE BEEF)
          (CRAVES BOILS-111 PAPAYA)
          (EATS GUAVA HOTDOG)
          (EATS CHOCOLATE ONION)
          (HARMONY LOVE MARS)
          (HARMONY REST-23 SATURN)
          (EATS CHOCOLATE ENDIVE)
          (EATS APPLE SWEETROLL)
          (CRAVES ANXIETY-52 GRAPEFRUIT)
          (EATS SHRIMP SWEETROLL)
          (EATS BAGUETTE HAROSET)
          (EATS WURST TOFU)
          (ATTACKS KENTUCKY QUEBEC)
          (HARMONY SATIETY-26 SATURN)
          (EATS LEMON WURST)
          (EATS CUCUMBER PAPAYA)
          (HARMONY EXCITEMENT-20 JUPITER)
          (LOCALE MUFFIN-7 KENTUCKY)
          (EATS CHOCOLATE TOFU)
          (HARMONY SATIETY MARS)
          (CRAVES TRIUMPH-64 PORK)
          (CRAVES LACERATION-89 POPOVER)
          (EATS SWEETROLL SHRIMP)
          (CRAVES DEPRESSION-62 SHRIMP-4)
          (CRAVES ANGER-33 WONDERBREAD)
          (EATS MUFFIN TURKEY)
          (CRAVES LUBRICITY-13 HOTDOG)
          (HARMONY UNDERSTANDING-28 JUPITER)
          (CRAVES BOILS-99 WURST)
          (EATS SHRIMP-4 YOGURT)
          (EATS BAGUETTE SCALLION)
          (CRAVES DEPRESSION-67 HAROSET)
          (EATS PEAR ORANGE)
          (CRAVES ANGINA SNICKERS-6)
          (CRAVES LEARNING TUNA)
          (EATS CHICKEN CHERRY)
          (CRAVES LACERATION-91 GUAVA)
          (EATS HAROSET LETTUCE)
          (HARMONY TRIUMPH-14 SATURN)
          (CRAVES ANGER-60 SHRIMP-4)
          (CRAVES ANGER-88 PEAR)
          (EATS LOBSTER WONDERBREAD)
          (EATS WURST MUTTON)
          (CRAVES LONELINESS-98 WURST)
          (CRAVES DREAD-116 COD)
          (CRAVES HANGOVER-110 PAPAYA)
          (LOCALE BACON-2 QUEBEC)
          (CRAVES DEPRESSION-39 LOBSTER)
          (EATS GUAVA PEPPER)
          (CRAVES SCIATICA-53 YOGURT)
          (CRAVES BOILS-74 MUFFIN)
          (EATS PEAR SCALLOP)
          (CRAVES SCIATICA PAPAYA-15)
          (EATS MUFFIN-7 WONDERBREAD-1)
          (EATS BROCCOLI TUNA-3)
          (CRAVES GRIEF-126 BAGUETTE)
          (EATS SCALLION RICE)
          (LOCALE TUNA KENTUCKY)
          (CRAVES HANGOVER-79 CHICKEN)
          (CRAVES STIMULATION-9 PEPPER)
          (CRAVES INTOXICATION-29 CHERRY)
          (EATS YOGURT HOTDOG)
          (EATS MARZIPAN SCALLION)
          (EATS TOMATO PORK)
          (EATS SHRIMP MELON)
          (CRAVES ANGER SNICKERS-6)
          (LOCALE BEEF KENTUCKY)
          (HARMONY TRIUMPH-64 MARS)
          (CRAVES LACERATION LAMB-16)
          (CRAVES TRIUMPH-14 LOBSTER)
          (EATS TOFU CHOCOLATE)
          (CRAVES ANGINA-100 WURST)
          (CRAVES LONELINESS-38 HOTDOG)
          (CRAVES ABRASION-102 CUCUMBER)
          (EATS TUNA-3 BROCCOLI)
          (HARMONY EXCITEMENT SATURN))
   (:GOAL (AND (CRAVES JEALOUSY CANTELOPE)
               (CRAVES GRIEF-117 CANTELOPE)
               (CRAVES ANGINA-73 ORANGE)
               (CRAVES PROSTATITIS HOTDOG)
               (CRAVES ANGINA HOTDOG)
               (CRAVES BOILS-123 BEEF)
               (CRAVES ABRASION-102 GRAPEFRUIT))))