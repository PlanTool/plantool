#!/bin/sh

PLANNER=../../source/planner/standAlone/brazilplan

failures=0

function produceOutput {

	for file in *.xml 
	do
		echo "$file..."
		$PLANNER $file | ./extractRegressionOutput.perl > $file.$1
		if [ $# -gt 1 ] 
		then 
			if [ ! -f $file.$2 ] 
                        then 
				echo "...no baseline"
			else
			    	diff $file.$1 $file.$2
				if [ $? -ne 0 ] 
				then
					echo "...differences"
					failures=`expr $failures + 1`
				fi
			fi
		fi
	done	

}

if [ "$1" = "recompile" ] 
then
	produceOutput baseline
else
	produceOutput output baseline
	echo "Tests failed=$failures"
fi
