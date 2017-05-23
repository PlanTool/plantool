Hi Fahiem,

I have just checked through everything and I am pretty sure it is all
OK. The domain is the same as the old one with an extra operator to tidy
up holes if you want to roll. Holes are stacked up so that it is
possible to implement the removal of them all before rolling.
do-time-step is tied to a machine and job pair, so it is possible to
release several machines with separate steps - the script that tidies up
afterwards compresses adjacent do-time-steps into a single step and it
also removes the hole-removal steps. I have replaced negative
preconditions with positive complements, and adjusted the scheduled
predicate to refer to two arguments - the job and the machine it is
scheduled with (makes the release easier afterwards). There is a dummy
argument added to each operator to mark where the unwanted arguments
from the STRIPS versions start and the script strips these arguments out
afterwards.

So, for example, STAN produces the following for the first problem:

schedule-2-0,0.25,3,(do-lathe b0 unwantedargs oblong red
smooth),(delete-strip-holes a0 one front nowidth noorient),(do-roll a0
unwantedargs smooth cold oblong black)

and this gets tidied up to be:

schedule-2-0,0.25,2,(do-lathe b0),(do-roll a0)

Time is for the STAN plan production only - the tidy up script is
negligible cost, anyway.

To execute: treatproblem takes the problem files as command line inputs
(eg treatproblem prob*pddl) and generates files with the same names, in
the same directory, but with the extension "strips" for "pddl". The tidy
up script is "adjustplan" and it takes the input file in the format you
specified, off the standard input, and writes the modified version to
standard output.

NOTE: The tidy up script expects its input to be in lower case (to spot
the key "unwantedargs", "delete-strip-holes" and
"do-time-step" phrases). People can adjust the script to suit their
output, or else pass it through an appropriate "tr" filter first, if
they produce upper case output. More significant, I expect comma
separated fields, as you suggested in your spec. I noticed some people
used space separated output - it won't work for that, so these people
will need to adjust their output, or else modify the script. The domain
is untyped and that can cause problems for people who instantiate
operators up front - some of the operators now have a lot of arguments.
(TIM infers the types, so we are OK, but I tried another planner on this
encoding and it died - I don't know how well others will cope). Anyway,
I think it is worth a try to see if we can learn something about the ADL
vs STRIPS issue.

To summarise, we use:

cd Schedule; treatproblem prob*pddl; cd; stanscript
Schedule/newdomain.pddl Schedule/prob*strips | adjustplan > resultsfile

Let me know if there are problems that I might be able to fix.

Cheers

Derek
