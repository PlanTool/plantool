BEGIN	{ FS = "[\.\ ]"; }
        {
	  c = sprintf( "./%s %s 2>> solutions.all", $1, ENVIRON["HSPRFLAGS"] );
	  system( c );
	}
