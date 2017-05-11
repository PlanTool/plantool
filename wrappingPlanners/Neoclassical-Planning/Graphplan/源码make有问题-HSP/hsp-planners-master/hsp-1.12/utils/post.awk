BEGIN			{ timing = "start"; flag = 0; }
$0 == "END PLAN"	{ flag = 0; }
/^[0-9]+\ [0-9]+$/	{ sec[timing] = $1; usec[timing] = $2;
			  if( timing == "start" )
			    timing = "end";
			  else
			    {
			      timing = "start";
			      if( sec["start"] == sec["end"] )
				printf "%.0f\n", (usec["end"] - usec["start"]) / 1000.0;
			      else
				printf "%.0f\n", (1000.0-(usec["start"]/1000.0)) + 1000.0*(sec["end"]-sec["start"]-1) + (usec["end"]/1000.0);
			    }
			}
			{ if( flag ) print $0; }
$0 == "BEGIN PLAN"	{ flag = 1; }

