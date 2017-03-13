BEGIN	{ FS = "[\.\ ]"; }
	{ c = sprintf( "%s/utils/gettime >> solutions.temp", ENVIRON["HSPHOME"] );
	  system( c );
	  c = sprintf( "make PROBLEM=%s.pddl DOMAIN=%s.pddl", $1, $3 );
	  system( c );
	  if( ENVIRON["HSPSEED"] != "" )
	    c = sprintf( "./%s -N 1000 -r %s %s >> solutions.temp", $1, ENVIRON["HSPSEED"], ENVIRON["HSPFLAGS"] );
	  else
	    c = sprintf( "./%s -N 1000 %s >> solutions.temp", $1, ENVIRON["HSPFLAGS"] );
	  system( c );
	  c = sprintf( "%s/utils/gettime >> solutions.temp", ENVIRON["HSPHOME"] );
	  system( c );
	}

