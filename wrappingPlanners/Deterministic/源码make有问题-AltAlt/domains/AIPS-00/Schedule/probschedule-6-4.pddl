(define (problem schedule-6-4)
(:domain schedule)
(:objects
    F0
    E0
    D0
    C0
    CIRCULAR
    TWO
    THREE
    BLUE
    YELLOW
    BACK
    RED
    B0
    FRONT
    ONE
    BLACK
    OBLONG
    A0
)
(:init
    (idle punch) (idle drill-press) (idle lathe) (idle roller) (idle polisher)
    (idle immersion-painter) (idle spray-painter) (idle grinder) (ru unwantedargs)
    (SHAPE A0 CIRCULAR)
    (SURFACE-CONDITION A0 ROUGH)
    (PAINTED A0 BLUE)
    (HAS-HOLE A0 TWO BACK) (lasthole A0 TWO BACK) (linked A0 nowidth noorient TWO BACK)
    (TEMPERATURE A0 COLD)
    (SHAPE B0 OBLONG)
    (SURFACE-CONDITION B0 POLISHED)
    (PAINTED B0 RED)
    (HAS-HOLE B0 ONE FRONT) (lasthole B0 ONE FRONT) (linked B0 nowidth noorient ONE FRONT)
    (TEMPERATURE B0 COLD)
    (SHAPE C0 OBLONG)
    (SURFACE-CONDITION C0 SMOOTH)
    (PAINTED C0 YELLOW)
    (HAS-HOLE C0 TWO BACK) (lasthole C0 TWO BACK) (linked C0 nowidth noorient TWO BACK)
    (TEMPERATURE C0 COLD)
    (SHAPE D0 CIRCULAR)
    (SURFACE-CONDITION D0 POLISHED)
    (PAINTED D0 BLACK)
    (HAS-HOLE D0 TWO BACK) (lasthole D0 TWO BACK) (linked D0 nowidth noorient TWO BACK)
    (TEMPERATURE D0 COLD)
    (SHAPE E0 OBLONG)
    (SURFACE-CONDITION E0 POLISHED)
    (PAINTED E0 RED)
    (HAS-HOLE E0 ONE FRONT) (lasthole E0 ONE FRONT) (linked E0 nowidth noorient ONE FRONT)
    (TEMPERATURE E0 COLD)
    (SHAPE F0 CYLINDRICAL)
    (SURFACE-CONDITION F0 SMOOTH)
    (PAINTED F0 BLUE)
    (HAS-HOLE F0 THREE BACK) (lasthole F0 THREE BACK) (linked F0 nowidth noorient THREE BACK)
    (TEMPERATURE F0 COLD)
    (CAN-ORIENT DRILL-PRESS BACK)
    (CAN-ORIENT PUNCH BACK)
    (CAN-ORIENT DRILL-PRESS FRONT)
    (CAN-ORIENT PUNCH FRONT)
    (HAS-PAINT IMMERSION-PAINTER YELLOW)
    (HAS-PAINT SPRAY-PAINTER YELLOW)
    (HAS-PAINT IMMERSION-PAINTER BLUE)
    (HAS-PAINT SPRAY-PAINTER BLUE)
    (HAS-PAINT IMMERSION-PAINTER BLACK)
    (HAS-PAINT SPRAY-PAINTER BLACK)
    (HAS-PAINT IMMERSION-PAINTER RED)
    (HAS-PAINT SPRAY-PAINTER RED)
    (HAS-BIT DRILL-PRESS THREE)
    (HAS-BIT PUNCH THREE)
    (HAS-BIT DRILL-PRESS TWO)
    (HAS-BIT PUNCH TWO)
    (HAS-BIT DRILL-PRESS ONE)
    (HAS-BIT PUNCH ONE)
    (PART F0) (unscheduled F0)
    (PART E0) (unscheduled E0)
    (PART D0) (unscheduled D0)
    (PART C0) (unscheduled C0)
    (PART B0) (unscheduled B0)
    (PART A0) (unscheduled A0)
)
(:goal (and
    (SHAPE B0 CYLINDRICAL)
    (PAINTED C0 BLUE)
    (PAINTED B0 BLACK)
    (SURFACE-CONDITION A0 POLISHED)
    (SURFACE-CONDITION F0 ROUGH)
    (PAINTED E0 BLACK)
)))
