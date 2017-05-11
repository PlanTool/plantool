// ********************************************************************
// * CHOCO, version 1.330 sept. 9th 2002                              *
// * file: const.cl                                                   *
// *    modelling constraints                                         *
// * Copyright (©) F. Laburthe, 1999-2002, see readme.txt             *
// ********************************************************************

// ------------------  File Overview  ---------------------------------
// --------------------------------------------------------------------

// ********************************************************************
// *   Part 1: AbstractConstraint                                     *
// ********************************************************************


// Abstract class for constraints, no interface methods
// Such a constraint may have IntVar as well as other types of variables
AbstractConstraint <: Ephemeral(
   // v0.30: abstract class for constraints whose propagation is delayed (events are not handled immediately)
   constAwakeEvent:ConstAwakeEvent  // v0.9906, used to be on DelayedConstraint
   )

// abstract classes for small constraints on integer valued variables (IntVar)
//
IntConstraint <: AbstractConstraint(
   cste:integer = 0)

UnIntConstraint <: IntConstraint(
   v1:IntVar,        // the only integer variable involved in the constraint
   idx1:integer = 0)     // constraint c is the (c.idx1)th constraint of its variable c.v1
                     // i.e.    c.v1.constraints[c.idx1] = c
BinIntConstraint <: IntConstraint(
   v1:IntVar, idx1:integer = 0,  // c.v1.constraints[c.idx1] = c
   v2:IntVar, idx2:integer = 0)  // c.v2.constraints[c.idx2] = c

TernIntConstraint <: IntConstraint(
   v1:IntVar, idx1:integer = 0,  // c.v1.constraints[c.idx1] = c
   v2:IntVar, idx2:integer = 0,  // c.v2.constraints[c.idx2] = c
   v3:IntVar, idx3:integer = 0)  // c.v3.constraints[c.idx3] = c
(known!(v1,v2,v3))

LargeIntConstraint <: IntConstraint(
   vars:list<IntVar>,
   indices:list<integer>,
   nbVars:integer = 0)

[closeLargeIntConstraint(c:LargeIntConstraint) : void
 => c.nbVars := length(c.vars),
    c.indices := make_list(c.nbVars,integer,0)]


// abstract classes for compoosite constraints (constraints made out of
// several (simpler) constraints).
// we provide with a generic mechanism for assigning a unique index to each
// variable in the composite constraint.

// Abstract class for constraint made of several sub-constraints, no interface
// note (0.18): CompositeConstraint could feature a field "indices"
CompositeConstraint <: AbstractConstraint()

BinCompositeConstraint <: CompositeConstraint(
     const1:AbstractConstraint,
     const2:AbstractConstraint,
     offset:integer = 0)
known!(const1,const2)

// claire3 port: change slots to strongly typed lists
LargeCompositeConstraint <: CompositeConstraint(
     lconst:list<AbstractConstraint>,
     loffset:list<integer>,
     nbConst:integer = 0,
     additionalVars:list<IntVar>,      // v1.02: variables that are not in the subconstraints
     additionalIndices:list<integer>)  // v1.02: corresponding constraint indices

getNbVars :: property(range = integer)

// v0.37 public for extensions to CHOCO
[closeCompositeConstraint(c:BinCompositeConstraint) : void
 -> c.offset := getNbVars(c.const1)]
// v1.02 fill the nbConst slot + take the additionalVars slot into account
// claire3 port: strongly typed lists
[closeCompositeConstraint(c:LargeCompositeConstraint) : void
 -> c.nbConst := length(c.lconst), // v1.02
    let nbvars := 0, 
        l := make_list(c.nbConst,integer,0) in
      (for i in (1 .. c.nbConst)
           (nbvars :+ (getNbVars(c.lconst[i]) as integer),
            l[i] := nbvars),
       c.loffset := l,
       c.additionalIndices := make_list(length(c.additionalVars),integer,0),
       assert(nbvars + length(c.additionalVars) = getNbVars(c)))]
// v1.02: new access functions: retrieving the number of variables coming from subconstraints
[getNbVarsFromSubConst(c:LargeCompositeConstraint) : integer
 => last(c.loffset)]

// Methods for connecting constraints

// <fl> 0.36: open-coded method for compatibility with other libraries (eg igloo)
connect :: property(range = void)

// private lower-level connection methods
// connecting a constraint to one of its variables, activate connections for bound events, or do both
connectIntVar :: property(range = void)
connectIntVarEvents :: property(range = void)


// v1.03 needs to be abstract (redefined for "global" constraint inheriting from IntConstraint)
setConstraintIndex :: property(range = void)

// Kind of prefix numbering of all variables in the subconstraints of a composite constraint
//
// In a complex root composite constraint (a syntactic tree where the nodes
// are CompositeConstraint and the leaves are simple AbstractConstraint
// such as UnIntConstraint/BinIntConstraint)
// The assignIndices function associates to each variable involved in a leaf constraint
// a unique index.
// Each internal node of the tree contains an "offset" field which contains the
// largest index among all variables in the left subtree.
//
// v0.30: the second argument of assignIndices can now either be a CompositeConstraint or a Delayer
// v1.02 raise an error + return an integer (for type inference)
[assignIndices(c1:AbstractConstraint, root:(CompositeConstraint), i:integer) : integer
 -> error("the root definition of assignIndices should not be called (~S,~S,~S)",c1,root,i),1]

[assignIndices(c1:UnIntConstraint, root:(CompositeConstraint), i:integer) : integer
 -> let j := i in
       (j :+ 1, connectIntVar(root,c1.v1,j),
        setConstraintIndex(c1,1,length(c1.v1.constraints)), // v1.03
        j)]

[assignIndices(c1:BinIntConstraint, root:(CompositeConstraint), i:integer) : integer
 -> let j := i in
       (j :+ 1, connectIntVar(root,c1.v1,j),
        setConstraintIndex(c1,1,length(c1.v1.constraints)), // v1.03
        j :+ 1, connectIntVar(root,c1.v2,j),
        setConstraintIndex(c1,2,length(c1.v2.constraints)), // v1.03
        j)]

// v0.34
[assignIndices(c1:TernIntConstraint, root:(CompositeConstraint), i:integer) : integer
 -> let j := i in
       (j :+ 1, connectIntVar(root,c1.v1,j),
        setConstraintIndex(c1,1,length(c1.v1.constraints)), // v1.03
        j :+ 1, connectIntVar(root,c1.v2,j),
        setConstraintIndex(c1,2,length(c1.v2.constraints)), // v1.03
        j :+ 1, connectIntVar(root,c1.v3,j),
        setConstraintIndex(c1,3,length(c1.v3.constraints)), // v1.03
        j)]

// v0.15
[assignIndices(c1:LargeIntConstraint, root:(CompositeConstraint), i:integer) : integer
 -> let j := i in
       (for k in (1 .. c1.nbVars)
           (j :+ 1, connectIntVar(root,c1.vars[k],j),
            setConstraintIndex(c1,k,length(c1.vars[k].constraints))), // v1.03
        j)]

[assignIndices(c1:BinCompositeConstraint, root:(CompositeConstraint), i:integer) : integer
 -> let j := i in
       (j := assignIndices(c1.const1, root, j) as integer,
        assert(c1.offset = j - i), // v0.37 (c1.offset is now filled earlier by closeCompositeConstraint)
        j := assignIndices(c1.const2, root, j) as integer,
        j)]

[assignIndices(c1:LargeCompositeConstraint, root:(CompositeConstraint), i:integer) : integer
 -> let j := i in
       (for constIdx in (1 .. c1.nbConst)
          let subc := c1.lconst[constIdx] in
             (j := (assignIndices(subc,root,j) as integer),  // <hha> 0.20
              assert(c1.loffset[constIdx] = j - i)), // v0.37 (c1.offset is now filled earlier by closeCompositeConstraint)
        assert(length(c1.additionalVars) = length(c1.additionalIndices)),
        for k in (1 .. length(c1.additionalVars))    // v1.02: handle the new additional* slots
          (j :+ 1, 
           connectIntVar(root,c1.additionalVars[k],j), 
           c1.additionalIndices[k] := length(c1.additionalVars[k].constraints)),
        j)]

// v0.32 this new function is required since the introduction of indexInOpposite.
// v1.02 raise an error
[setConstraintIndex(c:AbstractConstraint, i:integer, val:integer) : void
 -> error("the root definition of setConstraintIndex (~S,~S,~S) should not be called",c,i,val)]
// let v be the i-th var of c, records that c is the n-th constraint involving v
[setConstraintIndex(c:UnIntConstraint, i:integer, val:integer) : void
 -> if (i = 1) c.idx1 := val
    else error("impossible to copy index ~S on ~S as ~S",i,c,val)]
[setConstraintIndex(c:BinIntConstraint, i:integer, val:integer) : void
 -> if (i = 1) c.idx1 := val
    else if (i = 2) c.idx2 := val
    else error("impossible to copy index ~S on ~S as ~S",i,c,val)]
[setConstraintIndex(c:TernIntConstraint, i:integer, val:integer) : void // v0.34
 -> if (i = 1) c.idx1 := val
    else if (i = 2) c.idx2 := val
    else if (i = 3) c.idx3 := val
    else error("impossible to copy index ~S on ~S as ~S",i,c,val)]
[setConstraintIndex(c:LargeIntConstraint, i:integer, val:integer) : void
 -> if (i <= length(c.indices)) c.indices[i] := val
    else error("impossible to copy index ~S on ~S as ~S",i,c,val)]
[setConstraintIndex(c:BinCompositeConstraint, i:integer, val:integer) : void
 -> if (i <= c.offset) setConstraintIndex(c.const1,i,val)
    else setConstraintIndex(c.const2,i - c.offset,val)]
// v1.02 raise an error when the index is too large
[setConstraintIndex(c:LargeCompositeConstraint, i:integer, val:integer) : void
 -> when constIdx := some(i2 in (1 .. c.nbConst) | c.loffset[i2] >= i) in
      let realIdx := (if (constIdx = 1) i else i - c.loffset[constIdx - 1]) in
          setConstraintIndex(c.lconst[constIdx],realIdx,val)
    else let realIdx := i - last(c.loffset) in // v1.02 handle the new additional* slots
      (if (realIdx <= length(c.additionalVars))
          c.additionalIndices[realIdx] := val
       else error("index ~S above largest var-index in constraint ~S",i,c))]

// v0.32 among all constraints linked to the idx-th variable of c,
// find the index of constraint c
// v1.02 raises an error + return an int (for type inference)
[getConstraintIdx(c:AbstractConstraint, idx:integer) : integer
 -> error("the root definition of getConstraintIdx (~S,~S) should not be called",c,idx),0]
[getConstraintIdx(c:UnIntConstraint, idx:integer) : integer
 -> if (idx = 1) c.idx1
    else (error("impossible to get ~S-th index of ~S",idx,c), 0)]
[getConstraintIdx(c:BinIntConstraint, idx:integer) : integer
 -> if (idx = 1) c.idx1
    else if (idx = 2) c.idx2
    else (error("impossible to get ~S-th index of ~S",idx,c), 0)]
[getConstraintIdx(c:TernIntConstraint, idx:integer) : integer // v0.34
 -> if (idx = 1) c.idx1
    else if (idx = 2) c.idx2
    else if (idx = 3) c.idx3
    else (error("impossible to get ~S-th index of ~S",idx,c), 0)]
[getConstraintIdx(c:LargeIntConstraint, idx:integer) : integer
 -> if (idx <= length(c.indices)) c.indices[idx]
    else (error("impossible to get ~S-th index of ~S",idx,c), 0)]
[getConstraintIdx(c:BinCompositeConstraint, idx:integer) : integer
 -> if (idx <= c.offset) getConstraintIdx(c.const1,idx)
    else getConstraintIdx(c.const2,idx - c.offset)]
// v1.02 raise an error when the index is too large
[getConstraintIdx(c:LargeCompositeConstraint, idx:integer) : integer
 -> when constIdx := some(i in (1 .. c.nbConst) | c.loffset[i] >= idx) in
      let realIdx := (if (constIdx = 1) idx else idx - c.loffset[constIdx - 1]) in
          getConstraintIdx(c.lconst[constIdx],realIdx)
    else let realIdx := idx - last(c.loffset) in // v1.02 handle the new additional* slots
      (if (realIdx <= length(c.additionalVars))
          c.additionalIndices[realIdx]
       else (error("index ~S above largest var-index in constraint ~S",idx,c), 0))]
// v1.02 <naren>

// Constraint interface

// The public methods on the constraints are the following
doPropagate :: property(range = void) // v1.02
doAwake :: property(range = void)
doAwakeOnVar :: property(range = void)
askIfTrue :: property(range = (boolean U {unknown}))
doAwakeOnInf :: property(range = void)
doAwakeOnSup :: property(range = void)
doAwakeOnInst :: property(range = void)
doAwakeOnRem :: property(range = void)
testIfTrue :: property(range = boolean)
testIfInstantiated :: property(range = boolean)
getVar :: property(range = AbstractVar) // v1.04

propagate :: property(range = void) // v1.04
awake :: property(range = void)
awakeOnInf :: property(range = void)
awakeOnSup :: property(range = void)
awakeOnInst :: property(range = void)
awakeOnRem :: property(range = void)
awakeOnVar :: property(range = void)
askIfEntailed :: property(range = (boolean U {unknown}))
testIfSatisfied :: property(range = boolean)
testIfCompletelyInstantiated :: property(range = boolean)

// doAwake(c:AbstractConstraint) : void
//    implements a fast call to awake(c)
// doAwakeOnInf(c:AbstractConstraint, idx:integer) : void
//    implements a fast call to awakeOnInf(c,idx)
// doAwakeOnSup(c:AbstractConstraint, idx:integer) : void
//    implements a fast call to awakeOnSup(c,idx)
// doAwakeOnInst(c:AbstractConstraint, idx:integer) : void
//    implements a fast call to awakeOnInst(c,idx)
// doAwakeOnRem(c:AbstractConstraint, idx:integer, x:integer) : void
//    implements a fast call to awakeOnRem(c,idx, x)
// doAwakeOnVar(c:AbstractConstraint,idx) : void
//    implements a fast call to awakeOnVar(c,idx)
// askIfTrue(c:AbstractConstraint) : (boolean U {unknown})
//    implements a fast call to askIfEntailed(c)
// testIfTrue(c:AbstractConstraint) : boolean
//    implements a fast call to testIfSatisfied(c)
// testIfInstantiated(c:AbstractConstraint) : boolean
//    implements a fast call to testIfCompletelyInstantiated(c)

// for each constraint the following method must be defined
// v0.9907: accessing the priority of a constraint

// v1.04
[testIfCompletelyInstantiated(c:IntConstraint) : boolean
 -> let n := getNbVars(c) in
       forall(i in (1 .. n) | isInstantiated(getVar(c,i)))]
[testIfCompletelyInstantiated(c:UnIntConstraint) : boolean
 -> isInstantiated(c.v1)]
[testIfCompletelyInstantiated(c:BinIntConstraint) : boolean
 -> (isInstantiated(c.v1) & isInstantiated(c.v2))]
[testIfCompletelyInstantiated(c:TernIntConstraint) : boolean  // v0.34
 -> (isInstantiated(c.v1) & isInstantiated(c.v2) & isInstantiated(c.v3))]
[testIfCompletelyInstantiated(c:LargeIntConstraint) : boolean  // v0.15
 -> forall(v in c.vars | isInstantiated(v))]
[testIfCompletelyInstantiated(c:BinCompositeConstraint) : boolean
 -> (testIfInstantiated(c.const1) & testIfInstantiated(c.const2) )]
[testIfCompletelyInstantiated(c:LargeCompositeConstraint) : boolean
 -> (forall(subc in c.lconst | testIfInstantiated(subc)) &
     forall(v in c.additionalVars | isInstantiated(v)) )]  // v1.02

// v0.29: counting the number of variables involved in a constraint
[getNbVars(c:AbstractConstraint) : integer -> error("getNbVars has not yet been defined on ~S, it should be !",c),0]
[getNbVars(c:UnIntConstraint) : integer -> 1]
[getNbVars(c:BinIntConstraint) : integer -> 2]
[getNbVars(c:TernIntConstraint) : integer -> 3]
[getNbVars(c:LargeIntConstraint) : integer
 -> assert(c.nbVars = length(c.vars)), // <thb> v0.36
    c.nbVars]
[getNbVars(c:LargeCompositeConstraint) : integer
 -> if (c.nbConst = 0) 0 // v1.011 <thb>
    else (assert(c.loffset[c.nbConst] = sum(list{getNbVars(subc) | subc in c.lconst})),   // v1.02 sum vs. Sum
          c.loffset[c.nbConst] + length(c.additionalVars))] // v1.02
[getNbVars(c:BinCompositeConstraint) : integer
 -> assert(c.offset + getNbVars(c.const2) = getNbVars(c.const1) + getNbVars(c.const2)),
    c.offset + getNbVars(c.const2)]

// accessing the ith variable of a constraint
[getVar(c:AbstractConstraint, i:integer) : AbstractVar
 -> error("getVar has not yet been defined on ~S, it should be !",c), IntVar()]
[getVar(c:UnIntConstraint, i:integer) : AbstractVar
 -> if (i = 1) c.v1 else error("wrong var index (~S) for ~S",i,c), c.v1]
[getVar(c:BinIntConstraint, i:integer) : AbstractVar
 -> if (i = 1) c.v1
    else if (i = 2) c.v2
    else (error("wrong var index (~S) for ~S",i,c), c.v1)]
[getVar(c:TernIntConstraint, i:integer) : AbstractVar
 -> if (i = 1) c.v1
    else if (i = 2) c.v2
    else if (i = 3) c.v3
    else (error("wrong var index (~S) for ~S",i,c), c.v1)]
[getVar(c:LargeIntConstraint, i:integer) : AbstractVar
 -> if (i % (1 .. c.nbVars)) c.vars[i]
    else (error("wrong var index (~S) for ~S",i,c), c.vars[1])]
[getVar(c:LargeCompositeConstraint, i:integer) : AbstractVar
 -> when constIdx := some(i0 in (1 .. c.nbConst) | c.loffset[i0] >= i) in
      let realIdx := (if (constIdx = 1) i else i - c.loffset[constIdx - 1]) in
         (getVar(c.lconst[constIdx],realIdx) as IntVar)
    else let realIdx := i - last(c.loffset) in // v1.02 handle the new additional* slots
      (if (realIdx <= length(c.additionalVars))
          (c.additionalVars[realIdx] as IntVar)
       else (error("wrong var index (~S) for ~S",i,c), getVar(c.lconst[1],1) as IntVar))]
[getVar(c:BinCompositeConstraint, i:integer) : AbstractVar
 -> if (i <= c.offset) (getVar(c.const1,i) as IntVar)
    else (getVar(c.const2, i - c.offset) as IntVar)]

// for each constraint C that involves IntVar (therefore, all IntConstraint,
// some GlobalConstraint and some CompositeConstraint), we implement
// propagation by defining
//   - a method awakeOnInf(C,i) saying how C reacts
//              to an increase of the inf of its ith variable
//   - a method awakeOnSup(C,i) saying how C reacts
//              to a decrease of the sup of its ith variable
//   - a method awakeOnInst(C,i) saying how C reacts
//              when its ith variable is assigned a value
//   - a method awakeOnRem(C,i,x) saying how C reacts
//              when its ith variable has lost the value x
//   - a method askIfEntailed(C) saying whether one can infer
//              that C is true, false or not.
//   - a method testIfSatisfied(C) testing whether C is true or not
//              This is called only when all variables are instantiated
// The constraint may in addition refine the following definitions:
//   - a method testIfCompletelyInstantiated(C) testing
//              whether all variables in C are instantiated
//   - a method getNbVars(C) indicating the number of variables involved in constraint
//   - a method onePropagation(C) performing one pass of propagation on C.
//     However, unlike awake, it may not reach saturation (the fix-point of complete propagation)
//     Therefore, it returns a boolean indicating whether additional calls are needed
//     to reach it.

// a few default definitions
[connect(c:AbstractConstraint) : void -> error("connect has not yet been defined on ~S, it should be !",c)]

[opposite(c:AbstractConstraint) : AbstractConstraint
 -> error("opposite has not yet been defined on ~S, it should be !",c), c]
[askIfEntailed(c:AbstractConstraint) : (boolean U {unknown})
-> (if testIfCompletelyInstantiated(c) testIfTrue(c) else unknown)] //v0.93
// v1.04: this is the function that must be implemented on all constraint classes
// TODO: note that this definition is on Ephemeral, while it should be on AbstractConstraint. 
// This is because of the definition of propagate@Problem. This should be fixed (rename propagate/awake into awake/awakeAtFirst)
[propagate(c:Ephemeral) : void
 -> error("the propagate method has not been implemented on ~S",c.constAwakeEvent)]
// v1.05 <brg> <thb> new default definitions:
// forget the variable on which the event (domain update) has occurred and call the basic propagation function
[awakeOnInf(c:AbstractConstraint,idx:integer) : void -> doPropagate(c)]
[awakeOnSup(c:AbstractConstraint,idx:integer) : void -> doPropagate(c)]
[awakeOnInst(c:AbstractConstraint,idx:integer) : void -> doPropagate(c)]
[awakeOnRem(c:AbstractConstraint, idx:integer, x:integer) : void -> doPropagate(c)]
[awakeOnVar(c:AbstractConstraint, idx:integer) : void -> doPropagate(c)]
[awake(c:AbstractConstraint) : void -> doAwake(c)] // VV instead of doPropagate(c)

[testIfSatisfied(c:AbstractConstraint) : boolean
 -> error("the feasibility test has not been implemented on ~S",c), false]

[testIfCompletelyInstantiated(c:AbstractConstraint) : boolean
 -> error("the instantiation test has not been implemented on ~S",c), false]

(interface(getNbVars),
; interface(assignIndices), BUG (because of a union as second argument ?)
 interface(getConstraintIdx),
 interface(askIfEntailed),
 interface(testIfSatisfied), interface(testIfCompletelyInstantiated),
 interface(propagate),
 interface(opposite),
 interface(awakeOnVar), interface(awakeOnInf),
 interface(awakeOnSup), interface(awakeOnInst), interface(awakeOnRem))

// Note: the three next definitions are usually not optimal (redundant propagation)
// and should therefore preferably be redefined for subclasses
[propagate(c:UnIntConstraint) : void
 -> doAwakeOnInf(c,1), doAwakeOnSup(c,1)]

[propagate(c:BinIntConstraint) : void
 -> doAwakeOnInf(c,1), doAwakeOnInf(c,2),
    doAwakeOnSup(c,1), doAwakeOnSup(c,2)]

[propagate(c:LargeIntConstraint) : void  // v0.15
 -> for i in (1 .. c.nbVars)
     (doAwakeOnInf(c,i), doAwakeOnSup(c,i))]

// Note: Within the layered propagation framework, this function must return a Boolean
// indicating whether some events have been popped for this constraint or not.
//  - Whenever it returns no, a fixpoint has been reached
//  - Otherwise, one propagation pass was performed and the function should be called
//    a second time to check whether some new events have been produced.
// V0.30, 0.33 <thb>

// ********************************************************************
// *   Part 7: compiler optimization                                  *
// ********************************************************************

// Dispatching the virtual methods
//  (trying to improve over the std Claire dispatch)

// claire3 port: remove DispatchIndexValue / function arrays
// claire3 port: remove that ugly ptach for fast dispatch of the main methods
// (no more register)
;doPropagate :: propagate
;doAwake :: awake
;doAwakeOnInf :: awakeOnInf
;doAwakeOnSup :: awakeOnSup
;doAwakeOnInst :: awakeOnInst
;doAwakeOnRem :: awakeOnRem
;doAwakeOnVar :: awakeOnVar

[doAwake(c:AbstractConstraint) : void
 => awake(c)]
[doPropagate(c:AbstractConstraint) : void
 => propagate(c)]
[doAwakeOnInf(c:AbstractConstraint, idx:integer) : void
 => awakeOnInf(c,idx) ]
[doAwakeOnSup(c:AbstractConstraint, idx:integer) : void
 => awakeOnSup(c,idx) ]
[doAwakeOnInst(c:AbstractConstraint, idx:integer) : void
 => awakeOnInst(c,idx) ]
[doAwakeOnRem(c:AbstractConstraint, idx:integer, x:integer) : void
 => awakeOnRem(c,idx, x) ]
[doAwakeOnVar(c:AbstractConstraint, idx:integer) : void
 => awakeOnVar(c,idx) ]
[askIfTrue(c:AbstractConstraint) : (boolean U {unknown})
 => askIfEntailed(c) ]
[testIfTrue(c:AbstractConstraint) : boolean
 => testIfSatisfied(c) ]
[testIfInstantiated(c:AbstractConstraint) : boolean
 => testIfCompletelyInstantiated(c) ]

// v0.34
// claire3 port: remove that ugly ptach for fast dispatch of the main methods (no more register)
;(#if (compiler.active? & compiler.loading?) register(Delayer))
// ********************************************************************
// *   Part 10: constraint networks                                   *
// ********************************************************************

// Building the constraint network
// 1. Adding an integer variable
[addIntVar(p:Problem,v:IntVar) : void
 => p.vars :add v,
    v.problem := p,   // v0.93
    if (length(p.vars) = (p.propagationEngine.maxSize + 1))  // v0.28: size vs. length
       printf("Watchout: the problem size is too small: risk of event queue overflows") ]

// 2. connecting constraints and variables

// *** Part 10a: connecting a constraint ******************************

// This function connects a constraint with its variables in several ways.
// Note that connect and connect may only be called while a constraint
// has been fully created and is being posted to a problem !
// Note that it should be called only once per constraint !
// (Note: these functions are redefined on GlobalConstraint (LinComb, Matching, ...)
// v1.0: inform the hook of the connection (useful architecture for Palm explanation)
// v1.03 definition on the abstract class <ega>
[connect(c:IntConstraint) : void
 => let n := getNbVars(c) in
      (for i in (1 .. n)
         connectIntVar(c,getVar(c,i) as IntVar,i))]
[connect(c:UnIntConstraint) : void
 => connectIntVar(c,c.v1,1)]
[connect(c:BinIntConstraint) : void
 => connectIntVar(c,c.v1,1), connectIntVar(c,c.v2,2)]
[connect(c:TernIntConstraint) : void
 => connectIntVar(c,c.v1,1), connectIntVar(c,c.v2,2), connectIntVar(c,c.v3,3)]
[connect(c:LargeIntConstraint) : void
 => for i in (1 .. c.nbVars) connectIntVar(c,c.vars[i],i)]
[connect(c:CompositeConstraint) : void
 => assignIndices(c,c,0)]
// v0.30: a Delayer is a sort of "UnCompositeConstraint" wrt constraint connection
// v1.02 <naren> call connectHook on target subconstraint

// These connections will be used by the event scheduler.
//   1. for all the concerned variables v, the constraint c is stored in the list
//      of v.constraints (the list of constraints involving v). This allows us to
//      inform all such constraints that an event has occurred on v. Thus, we only
//      store a queue of events associated to a variable rather than a pair (v,c) => shorter queue
//   2. In order for such a list to avoid re-informing an event-generating constraint
//      about its own occurrence, we store a redundant index telling how to retrieve
//      a constraint c in all lists v.constraints for its variables v.
//      (ie, if wake(c1) => event(v) => wake(c) for c in (v.constraints BUT c1)
//      This supports a propagation of c on v by cyclic iteration on (v.constraints but c)
//   3. In order to avoid wakening constraints that are fully satisfied, there is
//      a linked list iteration mechanism on the list v.constraints.
//      Note here, that although the
//      "nextConst" list is a backtrackable list of indices (the index next active constraint
//      in this chained list), unbacktrackable updates are performed on it. Indeed, constraint
//      posting is not backtrackable !

[connectIntVar(cont:AbstractConstraint, u:IntVar, i:integer) : void ->
  //[5] connectIntVar ~S ~S ~S // cont,u,i,
 let n := u.nbConstraints + 1 in (
     store(u.constraints, cont), store(u.indices, i), // VV
     u.nbConstraints := n,
     setConstraintIndex(cont,i,n),
     //[5] connectIntVarEvents for last constraint of ~S // u,
     connectEvent(u.updtInfEvt, n),
     connectEvent(u.updtSupEvt, n),
     connectEvent(u.instantiateEvt, n),
     connectEvent(u.remValEvt, n))]

[connectEvent(e:VarEvent, nbconst:integer) : void
 -> if (nbconst > 1)
      let lnext := e.nextConst, j:integer := lnext[nbconst - 1], k := nbconst - 1 in
         (if (j = 0)   // <naren> v0.25: no constraint on u is connected to the constraint network
             store(lnext, nbconst) // VV //               it will be the only one
          else store(lnext, j),    // VV // standard case
            while (k > 0 & k >= j & lnext[k] = j) // <naren> v0.25 don't loop with k<j1
                   (store(lnext, k, nbconst), // VV           //   (was the case when j1 was the only active constraint)
                    k :- 1)  ; , // VV
          ;e.nextConst := lnext // VV
	  ) 
    // claire3 port typed lists
    else store(e.nextConst, 1)]      // on the variable and it is connected.


