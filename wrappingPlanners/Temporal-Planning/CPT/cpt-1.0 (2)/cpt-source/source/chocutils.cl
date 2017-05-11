// ********************************************************************
// * CHOCO, version 1.330 sept. 9th 2002                              *
// * file: utils.cl                                                   *
// *    common utilities & data structures                            *
// * Copyright (©) F. Laburthe, 1999-2002, see readme.txt             *
// ********************************************************************
(system.verbose := 0)
// ------------------  File Overview  ---------------------------------
// *   Part 1: Global parameters                                      *
// *   Part 2: simple utilities (min,max,etc.)                        *
// *   Part 3: data structure utilities                               *
// *   Part 3: The generic object hierarchy                           *
// *   Part 4: Variables                                              *
// *   Part 5: Propagation events                                     *
// *   Part 6: Constraints (objects, creation, generic methods)       *
// *   Part 7: compiler optimization                                  *
// *   Part 8: Problems & constraint networks                         *
// --------------------------------------------------------------------

// ********************************************************************
// *   Part 1: simple utilities (min,max,etc.)                        *
// ********************************************************************


// the largest bound for integers are [-1073741823, 1073741823],
// but we use a slightly restricted range, with easier to recognize values
// 0.34: MAXINT - MININT no longer produces an overflow
claire/MAXINT :: 99999999
claire/MININT :: -99999999

// utils specific for claire3 port
[claire/max(x:integer, y:integer) : integer
 => if (x >= y) x else y]
[claire/min(x:integer, y:integer) : integer
 => if (x >= y) y else x]

// Utils : maximum and minimum of a collection of integers
// v1.02 lowercase those function names
[claire/max(x:(list[integer] U set[integer])) : integer
 => let s := MININT in (for y in x s :max y, s)]

[claire/min(x:(list[integer] U set[integer])) : integer
 => let s := MAXINT in (for y in x s :min y, s)]

[claire/sum(x:(list[integer] U set[integer])) : integer
 => let s := 0 in (for y in x s :+ y, s)]

[claire/product(x:(list[integer] U set[integer])) : integer
 => let p := 1 in (for y in x p :* y, p)]

[claire/count(S:any) : integer
 => let s := 0 in
      (for %x in S s :+ 1, s)]


[claire/random(l:list) : any
 -> let n := length(l) in l[1 + random(n)] ]

// 0.26 casts for improved compilation
// claire3 port: module System has disappeared
[claire/random(I:Interval) : integer
 -> I.arg1 + random(1 + I.arg2 - I.arg1) ]

[claire/random(a:integer, b:integer) : integer
 -> assert(a <= b),
    a + random(b - a + 1) ]

claire/UNKNOWNINT :: -100000000
[knownInt(x:integer) : boolean => (x != UNKNOWNINT)]

// ********************************************************************
// *   Part 2: backtrackable integers                                 *
// ********************************************************************

// root class for all generic data structure utilities
Util <: ephemeral_object()

// utility: storing a backtrackable integer with a time stamp, so that
// only one update is recorded per world
// (cf Claire documentation, end of Part 2, page 15)
// new in v0.37
claire/StoredInt <: Util(
    claire/latestValue:integer = 0,   // current (latest) value
    claire/latestUpdate:integer = 0)  // index of the latest world in which the StoredInt was modified
[self_print(x:StoredInt) : void -> princ(x.latestValue)]

[write(x:StoredInt, y:integer) : void
 -> if (y != x.latestValue)      // v1.010 <ega>
      let currentWorld := world?() in
       (if (currentWorld > x.latestUpdate)
           (put_store(latestValue,x,y,true),
            put_store(latestUpdate,x,currentWorld,true))
        else x.latestValue := y)]

[read(x:StoredInt) : integer => x.latestValue]

[make_simple_bklist(x:any, bkl:any, size:integer, t:type, e:any) : void => 
  let l := make_list(size, t, e) in (
      shrink(l, 0),
      put(bkl, x, l))]
