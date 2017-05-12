#include <stdio.h>
#include <sys/time.h>

main()
{
  struct timeval tv;

  gettimeofday( &tv, NULL );
  fprintf( stdout, "%lu %lu\n", tv.tv_sec, tv.tv_usec );
}


