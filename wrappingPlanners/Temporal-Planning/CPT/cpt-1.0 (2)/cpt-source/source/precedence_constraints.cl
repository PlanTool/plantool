// ********************************************************************
// * CPT, version 1.0 sep. 2004                                       *
// * file: static-conflicts.cl                                        *
// * Copyright (©) Vincent Vidal, 2003-2004                           *
// ********************************************************************

// ------------------  File Overview  ---------------------------------
// *   Part 1: Precedence Constraint between 2 actions                *
// *   Part 2: Precedence constraints between action and causal link  *
// --------------------------------------------------------------------

// ********************************************************************
// *   Part 1: Precedence Constraint between 2 actions                *
// ********************************************************************

PrecedenceConstraint <: BinIntConstraint(v1:Action, v2:Action)

[self_print(c:PrecedenceConstraint) : void ->  
  let distance := getDistance(c.v1, c.v2) in (
      if (distance = 0) (printf("(~S << ~S)", c.v1, c.v2))
      else (printf("(~S + ~S << ~S)", c.v1, distance, c.v2)))]


[<<(a:Action, a':Action) : PrecedenceConstraint => 
  PrecedenceConstraint(v1 = a, v2 = a')]


/******** Propagations ********/

[propagate(c:PrecedenceConstraint) : void -> nil]

[awake(c:PrecedenceConstraint) : void -> 
  tracePrecedenceAwake(c),
  awakeOnInf(c, 1), 
  awakeOnSup(c, 2)]

[awakeOnInf(c:PrecedenceConstraint, idx:integer) : void -> 
  if (idx = 1 & isUsed(c.v1)) updateInfA(c.v2, firstEnd(c.v1) + getDistance(c.v1, c.v2))]

[awakeOnSup(c:PrecedenceConstraint, idx:integer) : void ->
  if (idx = 2 & isUsed(c.v2)) updateSupA(c.v1, lastStart(c.v2) - getDuration(c.v1) - getDistance(c.v1, c.v2))]

[awakeOnInst(c:PrecedenceConstraint, idx:integer) : void ->
  if (idx = 1) awakeOnInf(c, 1)
  else if (idx = 2) awakeOnSup(c, 2)]

[askIfEntailed(c:PrecedenceConstraint) : (boolean U {unknown}) ->
  if (c.v1 alwaysPrecede c.v2) true
  else if (isUsed(c.v2) & c.v1 cannotPrecede c.v2) false
  else unknown]


// ********************************************************************
// *   Part 2: Precedence constraints between action and causal link  *
// ********************************************************************

/******** Constraint ********/

CausalPrecedenceConstraint <: TernIntConstraint(v1:Action, v2:IntVar, v3:CausalLink)

[self_print(c:CausalPrecedenceConstraint) : void -> 
  let 
    a := (when prod := getProducer(c.v3) in prod else c.v3),
    distance := getDistance(c.v1, c.v3)
  in (
      if (distance = 0) printf("(~S << ~S)", c.v1, a)
      else printf("(~S + ~S << ~S)", c.v1, distance, a))]

[<<(a:Action, c:CausalLink) : CausalPrecedenceConstraint => 
   CausalPrecedenceConstraint(v1 = a, v2 = c.init, v3 = c)]


/******** Propagations ********/

[awake(c:CausalPrecedenceConstraint) : void ->
  tracePrecedenceAwake(c),
  awakeOnInf(c, 1), 
  awakeOnSup(c, 2)]

[awakeOnInf(c:CausalPrecedenceConstraint, idx:integer) : void -> 
  if (idx = 1 & isUsed(c.v1)) (
    if canProduce(c.v1, c.v3) updateInfC(c.v3, firstStart(c.v1))
    else updateInfC(c.v3, firstEnd(c.v1) + getDistance(c.v1, c.v3)))]

[awakeOnSup(c:CausalPrecedenceConstraint, idx:integer) : void ->
  if (idx = 2) (
    if canProduce(c.v1, c.v3) updateSupA(c.v1, lastStart(c.v3))
    else updateSupA(c.v1, lastStart(c.v3) - getDuration(c.v1) - getDistance(c.v1, c.v3)))]

[awakeOnInst(c:CausalPrecedenceConstraint, idx:integer) : void ->
  if (idx = 1) awakeOnInf(c, 1)
  else if (idx = 2) awakeOnSup(c, 2)]

[awakeOnRem(c:CausalPrecedenceConstraint, idx:integer, v:integer) : void ->
  awakeOnInf(c, 1), 
  awakeOnSup(c, 2)]

[awakeOnVar(c:CausalPrecedenceConstraint, idx:integer) : void ->
  awakeOnInf(c, 1), 
  awakeOnSup(c, 2)]

[askIfEntailed(c:CausalPrecedenceConstraint) : (boolean U {unknown}) ->
  if (c.v1 alwaysPrecede c.v3) true
  else if (c.v1 cannotPrecede c.v3) false
  else unknown]


