#!/usr/bin/perl

$state=0;

while (<>) {

    if ($state==0 && /^Regression output:/) {
	$state=1;
    }

    if ($state==1) {
        print $_;
    }

}
