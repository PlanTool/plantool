// ********************************************************************
// * CHOCO, version 1.330 sept. 9th 2002                              *
// * file: var.cl                                                     *
// *    modelling domain variables                                    *
// * Copyright (©) F. Laburthe, 1999-2002, see readme.txt             *
// ********************************************************************

// ------------------  File Overview  ---------------------------------
// *   Part 1: AbstractVar                                            *
// *   Part 2: IntVar                                                 *
// *   Part 3: Generating events from IntVars                         *
// *   Part 4: SetVar                                                 *
// *   Part 5: Generating events from SetVars                         *
// --------------------------------------------------------------------

// ********************************************************************
// *   Part 1: AbstractVar                                            *
// ********************************************************************

// the abstract class for all types of domain variables
AbstractVar <: Ephemeral(
    name:string = "",
    problem:Problem,        // a variable is related to a single problem
    nbViolatedConstraints:integer = 0,
    nbConstraints:integer = 0,
    constraints:list[AbstractConstraint], // VV
    indices:list[integer]) // VV

(store(nbConstraints)) // VV


isInstantiated :: property(range = boolean)
[isInstantiated(v:AbstractVar) : boolean -> false]

(interface(isInstantiated))

// v1.0
[getConstraint(v:AbstractVar, i:integer) : AbstractConstraint => v.constraints[i]]
// v1.010: accessing the degree of a variable
[getDegree(v:AbstractVar) : integer -> length(v.constraints)]

// ********************************************************************
// *   Part 2: IntVar                                                 *
// ********************************************************************

// the class for all integer variables
IntVar <: AbstractVar(
    inf:StoredInt,
    sup:StoredInt,
    value:integer = UNKNOWNINT,
    private/savedAssignment:integer = unknown,
    bucket:AbstractIntDomain,
    updtInfEvt:IncInf,    // v0.91: representing propagation events as objects
    updtSupEvt:DecSup,
    instantiateEvt:InstInt,
    remValEvt:ValueRemovals)
;(store(inf,sup)) // needs to be declared if inf/sup slots are not reified
(reify(inf,sup))  // v0.37 <fl> more efficient storage of inf/sup slots
(store(value))  // v1.02 inf and sup are no longer stored since they are StoredInt !!!!!

[self_print(v:IntVar) : void
 -> if known?(get(name,v)) printf("~A:",v.name) else printf("<IntVar>"),
    if knownInt(v.value) printf("~S",v.value)
    else if known?(bucket,v) printf("~S",v.bucket)
    else printf("[~S.~S]",v.inf,v.sup) ]

// v0.9903:add one parameter (number of removal to react to, one by one)
[closeIntVar(v:IntVar,i:integer,j:integer,p:integer) : void
 => put(inf,v,StoredInt(latestValue = i)),
    put(sup,v,StoredInt(latestValue = j)),
    if (i = j) v.value := i,
    make_simple_bklist(v, constraints, v.problem.nbConstVar, AbstractConstraint, AbstractConstraint()), // VV
    make_simple_bklist(v, indices, v.problem.nbConstVar, integer, 0), // VV
    v.updtInfEvt := IncInf(modifiedVar = v),
    make_simple_bklist(v.updtInfEvt, nextConst, v.problem.nbConstVar, integer, 0), // VV
    v.updtSupEvt := DecSup(modifiedVar = v),
    make_simple_bklist(v.updtSupEvt, nextConst, v.problem.nbConstVar, integer, 0), // VV
    v.instantiateEvt := InstInt(modifiedVar = v),
    make_simple_bklist(v.instantiateEvt, nextConst, v.problem.nbConstVar, integer, 0), // VV
    let e := ValueRemovals(modifiedVar = v, maxVals = p) in
       (// claire3 port
        e.valueStack := make_list(e.maxVals,integer,0),
        e.causeStack := make_list(e.maxVals,integer,0),
        make_simple_bklist(e, nextConst, v.problem.nbConstVar, integer, 0), // VV
        v.remValEvt := e)]


// Basic interface of IntVar for performing events on variable domains

[updateInf(v:IntVar, x:integer) : boolean
 => (if (x > v.inf)
        (if (x > v.sup)
            (if (x > MAXINT) error("Finite domain overflow on var:~S, ~S > ~S (MAXINT)",v,x,MAXINT) // <thb> v0.36
             else raiseContradiction(v))
         else if known?(bucket,v) v.inf := updateDomainInf(v.bucket, x)
         else v.inf := x,
         true)
    else false) ]

[updateSup(v:IntVar, x:integer) : boolean
 => (if (x < v.sup)
       (if (x < v.inf)
           (if (x < MININT) error("Finite domain overflow on var:~S, ~S < ~S (MININT)",v,x,MININT) // <thb> v0.36
            else raiseContradiction(v))
        else if known?(bucket,v) v.sup := updateDomainSup(v.bucket, x)
        else v.sup := x,
        true)
     else false) ]

[instantiate(v:IntVar, x:integer) : boolean
 -> if knownInt(v.value)
      (if (v.value != x) 
          (raiseContradiction(v), true)   // v1.322 fake "true" return value
       else false)
    else (if (x < v.inf | x > v.sup) raiseContradiction(v)
          else (when dom := get(bucket,v) in
                  (if containsValInDomain(dom,x) restrict(dom,x)   // v1.010
                   else raiseContradiction(v))),
          v.sup := x, v.inf := x, v.value := x,
          true) ]

[removeVal(v:IntVar, x:integer) : boolean
 -> (if (v.inf <= x & x <= v.sup)
        (if (x = v.inf)
            (updateInf(v,x + 1),
             if (v.inf = v.sup) instantiate(v,v.inf),
             true)  // v0.12
         else if (x = v.sup)
            (updateSup(v,x - 1),
            if (v.inf = v.sup) instantiate(v,v.inf),
            true)  // v0.12
         else if known?(bucket,v) removeDomainVal(v.bucket, x)
         else false)
     else false)]
     
// Implementing the interface of AbstractVar :
//   1. iterating the values in the current domain of a variable
// claire3 port: Intervals no longer need to be typed, change range to subtype[int]
// v1.04: use the domainSequence new API method
[domain(x:IntVar) : subtype[integer]
 -> if known?(bucket,x) domainSequence(x.bucket)
    else (x.inf .. x.sup)]

//   2. iterating the values in the current domain of a variable
// 1.322: reshaped to avoid the definition of iterators on domains.

[Iterate(x:domain[tuple(IntVar)],v:Variable,e:any)
 => let thevar:IntVar := (eval(nth(args(x),1)) as IntVar) in
      (when dom := get(bucket,thevar) in 
           (case dom 
             (LinkedListIntDomain 
                let v := thevar.inf in 
                  (while (v <= thevar.sup)
                     (e, v := getNextValue(dom,v))),
              AbstractIntDomain
                for v in domainSequence(dom) e))
;                let v := getInf(thevar) in 
;                  (while (v <= getSup(thevar))
;                     (e, v := getNextValue(dom,v))) ))
       else let v := thevar.inf in
               (while (v <= thevar.sup)
                 (e, v := max(v + 1,thevar.inf)) ) ) ]

// Basic interface of IntVar for testing domains
getInf :: property(range = integer)
getSup :: property(range = integer)
isInstantiatedTo :: operation(precedence = precedence(%), range = boolean)
canBeInstantiatedTo :: operation(precedence = precedence(%), range = boolean)
canBeEqualTo :: operation(precedence = precedence(%), range = boolean) // <thb> v0.93
domainIncludedIn :: operation(precedence = precedence(%), range = boolean) // <ega> v0.9901
canBeInstantiatedIn :: operation(precedence = precedence(%), range = boolean) // <thb> v1.02

[isInstantiatedTo(v:IntVar, x:integer) : boolean
 => (v.value = x)] //  knownInt(v.value)
[canBeInstantiatedTo(v:IntVar, x:integer) : boolean
; -> (v.inf <= x & v.sup >= x & (unknown?(bucket,v) | containsValInDomain(v.bucket,x)))]
 => (v.inf <= x & v.sup >= x & containsValInDomain(v.bucket,x))]
// returns true iff 2 IntVar have intersecting domains
[canBeEqualTo(v:IntVar, x:IntVar) : boolean
 -> (v.sup >= x.inf) & (v.inf <= x.sup) &
    ((not(known?(bucket,v)) | not(known?(bucket,x))) | // VV
      exists(val in domain(x) | containsValInDomain(v.bucket,val)))]

//  <ega> v0.9901: checks if all values in v are included in l
// v0.9907
[domainIncludedIn(v:IntVar, sortedList:list[integer]) : boolean
 => (v.inf >= sortedList[1] & v.sup <= last(sortedList) &
     (unknown?(bucket,v) | isIncludedIn(v.bucket,sortedList)))]
[canBeInstantiatedIn(v:IntVar, sortedList:list[integer]) : boolean
 -> v.inf <= last(sortedList) & v.sup >= sortedList[1] &
    (unknown?(bucket,v) | exists(val in v.bucket | val % sortedList))]
// v0.9907 <ega>
[hasExactDomain(v:IntVar) : boolean => known?(bucket,v)]
[random(v:IntVar) : integer // v1.02 silly boolean return type
 -> when dom := get(bucket,v) in random(dom) 
    else random(v.inf, v.sup)]
// v1.013: a new method for accessing the domain of an IntVar
// v1.016: <franck> add a test comparison to the domain lower bound
[getNextDomainValue(v:IntVar, i:integer) : integer
 -> if (i < v.inf) v.inf
    else (assert(i < v.sup),
          when d := get(bucket,v) in getNextValue(d,i)
          else i + 1)]
// v1.016 <franck>
[getPrevDomainValue(v:IntVar, i:integer) : integer
 -> if (i > v.sup) v.sup
    else (assert(i > v.inf),
          when d := get(bucket,v) in getPrevValue(d,i)
          else i - 1)]

// retrieving bound information on the variables
[getInf(v:IntVar) : integer => v.inf]
[getSup(v:IntVar) : integer => v.sup]
[isInstantiated(v:IntVar) : boolean => knownInt(v.value)]
[getValue(v:IntVar) : integer => assert(isInstantiated(v)), v.value]

// retrieving bound information on the variables
// V0.9907
[getDomainSize(v:IntVar) : integer
 -> when dom := get(bucket,v) in getDomainCard(dom)
    else (v.sup - v.inf + 1)]
          
(;interface(updateInf), interface(updateSup), interface(removeVal),// non uniform because two restrictions with arity 2&3
 ;interface(instantiate),
 // below mysterious bug: not placed in dispatcher of IntVar
 ; interface(isInstantiatedTo), interface(canBeInstantiatedTo), interface(canBeEqualTo), interface(canBeInstantiatedIn), 
 interface(domainIncludedIn), interface(hasExactDomain),
 interface(getInf), interface(getSup)
 )
(interface(IntVar,;updateInf,
                  ;updateSup,
                  ;removeVal,
                  ;instantiate,
                  // below mysterious bug: not placed in dispatcher of IntVar (because they are operators ?)
                  ;isInstantiatedTo, canBeInstantiatedTo,   canBeEqualTo,  canBeInstantiatedIn,                  
                  domainIncludedIn,
                  hasExactDomain,
                  getInf,
                  getSup
                  ))

// ********************************************************************
// *   Part 3: Generating events from IntVars                         *
// ********************************************************************

// v0.9907: now returns a Boolean indicating whether the call indeed added new information

[updateInf(v:IntVar, x:integer, idx:integer) : boolean
 -> //[PDEBUG] ~S.inf: ~S -> ~S // v,v.inf,x,
    if updateInf(v,x)
      let cause := (if (v.inf = x) idx else 0) in
         (if (v.sup = v.inf) instantiate(v,v.inf,cause)
          else postUpdateInf(v.problem.propagationEngine,v,cause),
          true)
    else false]

// v0.9907: now returns a Boolean indicating whether the call indeed added new information

[updateSup(v:IntVar, x:integer, idx:integer) : boolean
 -> //[PDEBUG] ~S.sup: ~S -> ~S // v,v.sup,x,
    if updateSup(v,x)
      let cause := (if (v.sup = x) idx else 0) in
        (if (v.sup = v.inf) instantiate(v,v.sup,cause)
         else postUpdateSup(v.problem.propagationEngine,v,cause),
         true)
    else false]


// v0.9907: now returns a Boolean indicating whether the call indeed added new information
// Note: In case v<>x => v>=x+1, then, maybe we can keep the idx of the
//          event generating constraint (and avoid telling that constraint
//          that the event created not a hole, but improved a bound).
//          Maybe, but we no longer take that risk (v0.90)
//       otherwise, for instance, if v<>x => v>=x+2 (because there was already
//          a hole at x+1 in the domain of v), then, probably, we should
//          forget about the index of the event generating constraint.
//          Indeed the propagated event is stronger than the initial one that
//          was generated; thus the generating constraint should be informed
//          about such a new event.
[removeVal(v:IntVar, x:integer, idx:integer) : boolean
 -> if removeVal(v,x)
      (//[PDEBUG] ~S removeVal ~S // v,x,
       if (v.inf = v.sup) postInstInt(v.problem.propagationEngine,v,0)  // v0.28: conforms to the note above
       else if (x < v.inf) postUpdateInf(v.problem.propagationEngine,v,0)
       else if (x > v.sup) postUpdateSup(v.problem.propagationEngine,v,0)
       else postRemoveVal(v.problem.propagationEngine,v,x,idx),
       true)
    else false]

// v0.9907: now returns a Boolean indicating whether the call indeed added new information
[instantiate(v:IntVar, x:integer, idx:integer) : boolean
 -> if instantiate(v,x)
      (//[PDEBUG] instantiate ~S to ~S // v,x,
       postInstInt(v.problem.propagationEngine,v,idx),
       true)
    else false]

// v0.9907: now returns a Boolean indicating whether the call indeed added new information
[removeInterval(v:IntVar,a:integer,b:integer,idx:integer) : boolean
 ->  //[PDEBUG]  ~S remove interval  (~S .. ~S) // v,a,b,
     if (a <= v.inf) updateInf(v,b + 1,idx)
     else if (b >= v.sup) updateSup(v,a - 1,idx)
     else if known?(bucket,v)  //v0.93: <ega> this test can be done once here instead of b - a times in removeVal(integer,integer)
             let anyChange := false in 
                (for i in (a .. b)
                    (if removeVal(v,i,idx) anyChange := true),
                 anyChange)
     else false]

// <ega> 0.9901
// incomplete propagation if
// 1- v is not an enumerated variable and
// 2- values in x are not "contiguous" 
[restrictTo(v:IntVar, sortedList:list[integer], idx:integer) : void
-> let n := length(sortedList) in
   ( if (sortedList[1] > v.inf) updateInf(v,sortedList[1],idx),
     if (sortedList[n] < v.sup) updateSup(v,sortedList[n],idx),
     if known?(bucket,v)
      let lastVal := sortedList[1] in
        for i in (2 .. n)
          for j in (lastVal + 1 .. (sortedList[i] - 1))
             ( removeVal(v,j,idx),
               lastVal := sortedList[i]))]
                  
