// ********************************************************************
// * CPT, version 1.0 sep. 2004                                       *
// * file: static-conflicts.cl                                        *
// * Copyright (©) Vincent Vidal, 2003-2004                           *
// ********************************************************************

(system.verbose := 0)

[make_simple_bklist(x:any, bkl:any, size:integer, t:type, e:any) : void => 
  let l := make_list(size, t, e) in (
      shrink(l, 0),
      put(bkl, x, l))]
