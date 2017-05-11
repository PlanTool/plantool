// ********************************************************************
// * CHOCO, version 1.330 sept. 9th 2002                              *
// * file: dom.cl                                                     *
// *    encoding variable domains                                     *
// * Copyright (©) F. Laburthe, 1999-2002, see readme.txt             *
// ********************************************************************

// ------------------  File Overview  ---------------------------------
// *   Part 1: abstract IntVar domains                                *
// *   Part 2: implementing IntVar domains by enumerations of values  *
// *   Part 3: abstract SetVar domains                                *
// *   Part 4: implementing SetVar domains by bitvectors              *
// *   Part 5: implementing SetVar domains by bitvector lists         *
// --------------------------------------------------------------------

// Abstract class for domain implementations, no interface methods
AbstractDomain <: collection() // v1.011: required in order to be able to iterate the object
(ephemeral(AbstractDomain))

// Abstract class for domain implementations of integer variables
AbstractIntDomain <: AbstractDomain()
// Interface of AbstractIntDomain documented in file iprop.cl
containsValInDomain :: property()
remove :: property()
restrict :: property()

AbstractSetDomain <: AbstractDomain

// ********************************************************************
// *   Part 1: abstract IntVar domains                                *
// ********************************************************************

// Interface of AbstractIntDomain:
//   the following methods should be defined on subclasses XXXIntDomain of AbstractIntDomain:
//     getDomainCard(d:XXXIntDomain) : integer
//     restrict(d:XXXIntDomain,s:set[integer]):void
//     restrict(d:XXXIntDomain,x:integer):void
//     removeDomainVal(d:XXXIntDomain,x:integer):void
//     updateDomainInf(d:XXXIntDomain,x:integer):void
//     updateDomainSup(d:XXXIntDomain,x:integer):void
//     getDomainInf(d:XXXIntDomain):integer
//     getDomainSup(d:XXXIntDomain):integer
//     containsValInDomain(d:XXXIntDomain,x:integer):boolean

getDomainCard :: property(range = integer)
getNextValue :: property(range = integer)
getPrevValue :: property(range = integer)
removeDomainVal  :: property(range = boolean)
domainSequence :: property(range = list<integer>)
domainSet :: property(range = set<integer>)

// v1.04
[domainSequence(d:AbstractIntDomain) : list<integer>
-> error("the domainSequence method has not been implemented on ~S",d),
   list<integer>()]
[domainSet(d:AbstractIntDomain) : set<integer>
-> error("the domainSequence method has not been implemented on ~S",d),
   set<integer>()]
[getDomainInf(d:AbstractIntDomain) : integer
 -> error("the getDomainInf method has not been implemented on ~S",d), 0]
[getDomainSup(d:AbstractIntDomain) : integer
 -> error("the getDomainSup method has not been implemented on ~S",d), 0]
[updateDomainInf(d:AbstractIntDomain,x:integer) : integer
 -> error("the updateDomainInf method has not been implemented on ~S",d), 0]
[updateDomainSup(d:AbstractIntDomain,x:integer) : integer
 -> error("the updateDomainSup method has not been implemented on ~S",d), 0]
[containsValInDomain(d:AbstractIntDomain, x:integer) : boolean
 -> error("the containsValInDomain method has not been implemented on ~S",d), true]
[removeDomainVal(d:AbstractIntDomain,x:integer) : boolean
 -> error("the removeDomainVal method has not been implemented on ~S",d), true]
[restrict(d:AbstractIntDomain,x:integer) : void
 -> error("the restrict method has not been implemented on ~S",d)]
[getDomainCard(d:AbstractIntDomain) : integer
 -> error("the getDomainCard method has not been implemented on ~S",d), 1]
[getNextValue(d:AbstractIntDomain, x:integer) : integer
 -> error("the getNextValue method has not been implemented on ~S",d), 0]
// v1.016 from franck:
[getPrevValue(d:AbstractIntDomain, x:integer) : integer
 -> error("the getPrevValue method has not been implemented on ~S",d), 0]
(interface(domainSequence), interface(domainSet),
 interface(updateDomainInf),interface(updateDomainSup),
 interface(getDomainInf), interface(getDomainSup), interface(containsValInDomain),
 interface(removeDomainVal), interface(restrict),
 interface(getDomainCard), 
 interface(getNextValue),
 interface(getPrevValue)
)
             
// ********************************************************************
// *   Part 2: implementing IntVar domains by enumerations of values  *
// ********************************************************************

// Interface of domains
claire/DINF :: -1000000
claire/DSUP ::  1000000


// An encoding of enumerated domains as linked lists
// we use an array of pointer indices: contents
//   contents[i] = i      <=> i is a possible value (OK)
//   contents[i] = j > i  <=> i, i+1, ..., j-1 non-OK, j probably OK
//   contents[i]  = MAXINT  <=> i, ..., dim non-OK
//
LinkedListIntDomain <: AbstractIntDomain(
       offset:integer = 0,    // v0.9907 no longer private fields
       bucketSize:integer = 0,
       contents:list[integer]) // VV
store(bucketSize)
// v1.02 <ebo>, <fxj>, v1.08 <fl> lighter printing
[self_print(x:LinkedListIntDomain) : void
 -> printf("[~S]~I",x.bucketSize,
           (let s := domainSet(x), si := size(s) in
              (if (si <= 4) print(s)
               else printf("{~S,~S...~S,~S}",s[1],s[2],s[si - 1],s[si])))) ]

// automatically called when
;                           contents = array!(list<integer>{i | i in (1 .. n)} add MAXINT)) ]  // v1.0, now an array
[makeLinkedListIntDomain(a:integer, b:integer) : LinkedListIntDomain
 -> assert(a <= b),
    assert(b <= a + 10000),  // we refuse such domains with too large a span (consumes too much memory)
    let n := b - a + 1 in
    let d := LinkedListIntDomain(bucketSize = n,
				 offset = a - 1,
				 contents = make_list(n + 1, integer, MAXINT)) 
    in (for i in (1 .. n) d.contents[i] := i, d)]

// claire3 port use array only on strongly typed lists

// a first utility
[random(d:LinkedListIntDomain) : integer
 -> let l := d.contents, i := 1 + random(length(l)) in  // fix v0.27 length vs. size
      (while (l[i] != i)
          i := 1 + random(length(l)),   // fix v0.27 length vs. size
       i + d.offset) ]

// Implementing the interface from AbstractDomain: primitives for iteration
// v1.04
[domainSet(d:LinkedListIntDomain) : set<integer>
 -> let s := set<integer>(), l := d.contents, i := l[1] in
       (while (i != MAXINT)
         (s :add (i + d.offset), i := l[i + 1]),
        s)]
// this is necessarliy sorted by increasing order of values
// v1.04
[domainSequence(d:LinkedListIntDomain) : list<integer>
 -> let s := list<integer>(), l := d.contents, i := l[1] in
       (while (i != MAXINT)
         (s :add (i + d.offset), i := l[i + 1]),
        s)]

;// used for iteration in compiled code: do not built the intentional set (does NOT allocate)
;[iterate(d:LinkedListIntDomain,v:Variable,e:any)
; => let delta := d.offset, i := d.contents[1] in
;       while (i != MAXINT)
;          (if (d.contents[i] = i)
;             let v := i + delta in e,      // we test that the value is indeed present
;	   i := d.contents[i + 1])]	   // (the bucket may be modified by e !!)

// Implementing the interface from AbstractIntDomain
// Accessors
[getDomainInf(d:LinkedListIntDomain) : integer
 => let l := d.contents in
      (assert(l[1] != MAXINT),    // check: non empty domain
       l[1] + d.offset)]

[getDomainSup(d:LinkedListIntDomain) : integer
 => let l := d.contents, i := length(l) - 1 in // v0.28: size vs. length
      (assert(l[1] != MAXINT),    // check: non empty domain
       while (l[i] = MAXINT) i :- 1,
       assert(l[i] = i),        // check that the last pointer is a feasible index
       l[i] + d.offset) ]

// v0.9901: submethod of domainIncludedIn
// supposed that [v.inf,v.sup] is included in [l[1],last(l)]     
// v0.9907 new name
[isIncludedIn(b:LinkedListIntDomain,l:list[integer]) : boolean
 -> forall(x in b | x % l)]

// v0.9907
[getDomainCard(d:LinkedListIntDomain) : integer -> d.bucketSize]

[containsValInDomain(d:LinkedListIntDomain, x:integer) : boolean
 -> let l := d.contents, i := x - d.offset in   // i is the index corresponding to x
      (assert(i >= 1 & i < length(l)),          // safety check: valid index (fix v0.27 length vs. size)
       l[i] = i)]

// v1.013
[getNextValue(d:LinkedListIntDomain, x:integer) : integer
 -> let l := d.contents, o := d.offset, i := x - o in
       (assert(i >= 1 & i < length(l)),          // safety check: valid index
        l[i + 1] + o)]

// v1.016 from franck:
[getPrevValue(d:LinkedListIntDomain, x:integer) : integer
 -> let l := d.contents, o := d.offset, i := x - o in
       (assert(i >= 1 & i < length(l)),          // safety check: valid index
        i :- 1,
        while (l[i] > i) i :- 1,
        assert(l[i] = i),        // check that the last pointer is a feasible index
        l[i] + o)]

// Modifiers (update functions)
[removeDomainVal(d:LinkedListIntDomain,x:integer) : boolean
 -> let l := d.contents, i := x - d.offset in   // i is the index corresponding to x
      (assert(i >= 1 & i < length(l)),          // safety check: valid index (fix v0.27 length vs. size)
       if (l[i] = i)                            // if i was present in the domain
         let k := l[i + 1], j := i - 1 in
           (d.bucketSize :- 1,                        //    decrease cardinal
            store(l,i,k,true),                  //    i points to the next feasible value k
            while (j >= 1 & l[j] = i)           //    all previous infeasible indices
                  (store(l,j,k,true), j :- 1),  //    pointing on i now point on k
            //[DDEBUG] after removal(~S) => bucket:~S // x,d,
            true)
       else false)]

[restrict(d:LinkedListIntDomain,x:integer) : void
 -> let l := d.contents, i := x - d.offset in   // i is the index corresponding to x
      (assert(i >= 1 & i < length(l)),          // safety check: valid index (fix v0.27 length vs. size)
       assert(l[i] = i),                        // i is already present in the domain
       for j in (1 .. i - 1)
           store(l,j,i,true),
       for j in (i + 1 .. length(l) - 1)    // fix v0.27 length vs. size
           store(l,j,MAXINT,true),
       d.bucketSize := 1, // v0.36 <michel>
       //[DDEBUG] after restrict(~S) => bucket:~S // x,d
       )]

// returns the new value of the lower bound (at least x)
[updateDomainInf(d:LinkedListIntDomain,x:integer) : integer
 -> let l := d.contents, i := x - d.offset in   // i is the index corresponding to x
      (assert(i >= 1 & i < length(l)),          // safety check: valid index (fix v0.27 length vs. size)
       assert(l[1] < MAXINT),     // check : d is non empty
       assert(i > l[1]),          // check : x is indeed an improved lower bound
       let i0 := l[i] in
          (assert(i0 != MAXINT),  // check: the new bound does not empty the domain v1.010
           let j := 1 in
              (while (l[j] != i0)               // for all indices j that point to less than i0
                  (if (l[j] = j) d.bucketSize :- 1, // if they were feasible, remove one value
                   store(l,j,i0,true),          // anoyhow, have them point to i0
                   j :+ 1)),
           //[DDEBUG] after setInf(~S) => bucket:~S // x,d,
           i0 + d.offset))]

// returns the new value of the upper bound (at most x)
[updateDomainSup(d:LinkedListIntDomain,x:integer) : integer
 -> let l := d.contents, i := x - d.offset in   // i is the index corresponding to x
      (assert(i >= 1 & i < length(l)),          // safety check: valid index (fix v0.27 length vs. size)
       assert(l[1] < MAXINT),   // check : d is non empty  v1.010
       assert(l[i] < MAXINT),   // check : x is indeed an improved upper bound
       assert(l[1] <= i),       // check: the new bound does not empty the domain v1.010
       let j := i + 1 in
          (while (l[j] != MAXINT)                // for all indices j after i pointing to some value
             (if (l[j] = j) d.bucketSize :- 1,   // if they were feasible, remove one value
              store(l,j,MAXINT,true),            // anyhow, have them point to END
              j :+ 1)),
       let j := i in
          (while (l[j] != j)                    // for all infeasible indices j around i
             (store(l,j,MAXINT,true),             // have them point to END
              j :- 1),
           //[DDEBUG] after setSup(~S) => bucket:~S // x,d,
           j + d.offset))]

