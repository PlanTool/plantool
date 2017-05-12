#!/usr/bin/perl

#=================================================================================================
# siege_v4 produces a file siege.results.  Run siege_v4, extract sat or unsat info, print results
# to generalsolver.stats per requirements and remove siege.results
#=================================================================================================

$num_args = $#ARGV+1;
if ( $num_args >= 3 ) {

  $file = $ARGV[0];
  $directory = $ARGV[1];
  $outfile = $ARGV[2];
  if ( !(-e $file && -f $file) ) {
      print $file . " is not a valid file name\n\n";
      exit 2;
  }

  $str = $directory . "siege_v4";
  for ( $count = 3; $count <= $num_args; $count++ ) {
    $str = $str . " " . shift;
  }

  $rtn = system($str);
  if ( $rtn >= 0 ) {
    open(INPUT, "siege.results") or die "\nError: cannot open siege.results to extract sat or unsat info\n $!";
    open(RESULTS, ">" . $outfile) or die "\nError: cannot open " . $outfile . " for writing\n $!";
    $done = 0;

    OUTER:
    while (<INPUT>) {
        if ( /unsatisfiable/ ) { 
            $done = 1; 
            print "UNSATISFIABLE\n"; 
            print RESULTS "UNSATISFIABLE\n";
            last OUTER;
        }
    }

    if ( $done eq 0 ) {
        print RESULTS "SATISFIABLE\n";
        close(INPUT);
        open(INPUT, "siege.results") or die "\nError: cannot re-open siege.results\n $!";
        $issat = 0;
        while ( <INPUT> ) {
            if ( /\s*\(.*,\s*\{([0-9-\s]*)\}\)/ ) {
                $issat = 1;
                print RESULTS $1;
                print RESULTS "\n";
            }
        }
        if ( $issat eq 1 ) {
            print "SATISFIABLE\n";
        } else {
            close(RESULTS);
            close(INPUT);
            open(NOGOOD, ">" . $outfile) or die "\nError: cannot open " . $outfile . " to print ERROR\n $!";
            print NOGOOD "ERROR";
            print "SOLVER ERROR";
            close(NOGOOD) or die "\nError: cannot close " . $outfile . "\n $!"; 
            exit 2;
        }
    }

    close(INPUT) or die "\nError: cannot close siege.results\n $!";
    close(RESULTS) or die "\nError: cannot close " . $outfile . "\n $!";
    unlink("siege.results");
  } else {
    open(NOGOOD, ">" . $outfile) or die "\nError: cannot open " . $outfile . " to print ERROR\n $!";
    print NOGOOD "ERROR";
    print "SOLVER ERROR";
    close(NOGOOD) or die "\nError: cannot close " . $outfile . "\n $!";  
    exit 2;
  }
} else {
  print "< 2 arguments sent to SiegeWrapper.pl\n";
  exit 2;
}
