// ********************************************************************
// * CPT, version 1.0 sep. 2004                                       *
// * file: static-conflicts.cl                                        *
// * Copyright (©) Vincent Vidal, 2003-2004                           *
// ********************************************************************

// ********************************************************************
// *   Part 1: Symbols                                                *
// ********************************************************************

Symbol <: object(name:string)

[self_print(s:Symbol) : void -> printf("~A", s.name)]

Predicate <: Symbol

Predicate <: Symbol(
  arity:integer = 0, 
  typing:boolean = true, 
  upTypes:set<Predicate>)

Term <: Symbol(isvar:boolean = false)

Constant <: Term()

Variabl <: Term

TransitionRule <: object

AttributeSpace :: tuple(Predicate, integer)
AttributeSpaceSet :: set<AttributeSpace>

[self_print(att:AttributeSpace) : void ->
  printf("<~S,~S>", att[1], att[2])]

[self_print(s:AttributeSpaceSet) : void ->
  printf("~A", s)]

attribute_spaces:AttributeSpaceSet :: set<AttributeSpace>()

[createAttributeSpace(p:Predicate, n:integer) : AttributeSpace -> ;tuple(p, n)]
  let a := tuple(p, n) in (
      add(attribute_spaces, a),
      a)]

TransitionRule <: object(
  enablers:AttributeSpaceSet, 
  consequences:AttributeSpaceSet)

[self_print(t:TransitionRule) : void ->
  printf("~S ---> ~S", t.enablers, t.consequences)]

attribute_spaces_table[x:AttributeSpace] : list<TransitionRule> := list<TransitionRule>()
constants_table[x:AttributeSpace] : set<Constant> := set<Constant>()

[addConstant(p:Predicate, n:integer, c:Constant) : void ->
  let a := createAttributeSpace(p, n) in (
      if not(c % constants_table[a]) (
	add(constants_table[a], c),
	for r in attribute_spaces_table[a] (
	  if forall(a' in r.enablers | c % constants_table[a']) (
	    for conseq in r.consequences (
	      addConstant(conseq[1], conseq[2], c)))),
	for t in p.upTypes addConstant(t, 1, c)))]

Variabl <: Term(
  index:integer,
  inequalities:list<Variabl>,
  attPrec:AttributeSpaceSet,
  attEffect:AttributeSpaceSet,
  domaine:set[Constant])

cpt_var:integer :: 0
  
predicate_table[s:string] : (Predicate U {unknown}) := unknown
variable_table[s:string] : (Variabl U {unknown}) := unknown
constant_table[s:string] : (Constant U {unknown}) := unknown
		  
[createType(s:string) : Predicate -> createPredicate(s, 1)]
[createPredicate(s:string) : Predicate -> createPredicate(s, 0)]
[createPredicate(s:string, i:integer) : Predicate ->
  when p := predicate_table[s] in (p as Predicate)
  else (
    let p := Predicate(name = s, arity = i) in (
	predicate_table[s] := p,
	p))]

[createVariable(rs:string) : Variabl ->
  let s := rs /+ string!(cpt_var) in (
  when v := variable_table[s] in (v as Variabl)
    else (
      let v := Variabl(name = s, isvar = true) in (
	  variable_table[s] := v,
	  v)))]

[createConstant(s:string) : Constant ->
  when c := constant_table[s] in (c as Constant)
  else (
    let c := Constant(name = s) in (
	constant_table[s] := c,
	c))]

[createTerm(s:string) : Term ->
  if (s[1] = '?') createVariable(s)
  else createConstant(s)]

						
// ********************************************************************
// *   Part 2: Atoms                                                  *
// ********************************************************************

Atom <: ephemeral_object(
  pred:Predicate,
  terms:list<Term>,
  tinitRat:(Rational U {unknown}) = unknown)
PositiveAtom <: Atom()
NegativeAtom <: Atom()

[self_print(a:PositiveAtom) : void ->
  printf("(~S", a.pred),
  for t in a.terms printf(" ~S", t),
  printf(")")]

[self_print(a:NegativeAtom) : void ->
  printf("(not (~S", a.pred),
  for t in a.terms printf(" ~S", t),
  printf("))")]

[createAtom(n:string) : Atom =>
  PositiveAtom(pred = Predicate(name = n, typing = false), terms = list<Term>())]

[createAtom(p:Predicate, t:list<Term>, sign:boolean, init:(Rational U {unknown})) : Atom => 
  p.arity := length(t),
  if sign PositiveAtom(pred = p, terms = t, tinitRat = init)
  else NegativeAtom(pred = p, terms = t, tinitRat = init)]

// ********************************************************************
// *   Part 3: Operators                                              *
// ********************************************************************

Operator <: object(
  name:string,
  parameters:list<Term>,
  precondition:list<Atom>,
  addEffect:list<Atom>,
  delEffect:list<Atom>,
  constraints:list<list>,
  durationExpr:Expression = Rational(num = 1, den = 1),
  duration:integer)

[self_print(o:Operator) : void ->
  printf("OPERATOR ~A(~A) :\n  Duration : ~S\n  Prec : ~A\n  Adds : ~A\n  Dels : ~A\n",
	 o.name,	 o.parameters,
	 o.durationExpr,
	 o.precondition,
	 o.addEffect,
	 o.delEffect)]

[createOperator(s:string) : Operator -> Operator(name = s)]

// ********************************************************************
// *   Part 4: Domains                                                *
// ********************************************************************

typed_constants:list<tuple> :: list<tuple>()

Domain <: object(
  name:string,
  durative:boolean = false,
  equality:boolean = false,
  operators:list<Operator>)

[self_print(d:Domain) : void ->
  printf("DOMAIN ~A\n", d.name),
  for o in d.operators printf("~S\n", o)]


// ********************************************************************
// *   Part 4: Problem                                                *
// ********************************************************************

Instance <: object(
  name:string,
  domain:Domain,
  initState:list<Atom>,
  timedLitterals:list<Atom>,
  actions:list<Action>,
  events:list<Action>,
  fluents:list<Fluent>,
  goal:list<Atom>,
  pgcd:integer,
  ppcm:integer)

[self_print(p:Instance) : void ->
  printf("PROBLEM ~A\nINIT STATE : ~A\nGOAL : ~A\n", 
	 p.name, p.initState, p.goal)]


// ********************************************************************
// *   Part 5: Parser                                                 *
// ********************************************************************


claire/buffer :: make_string(256, ' ')

claire/current_char:char :: ' '
claire/current_word:string :: ""
claire/next_word:string :: ""
claire/parsed_file:port :: unknown

;claire/endl :: char!(10)
claire/endl2 :: char!(13)
claire/lparen :: '('
claire/rparen :: ')'
claire/comt :: ';'
claire/space :: ' '
claire/tabul :: '	'
claire/diff_upcase :: (integer!('a') - integer!('A'))

[openFile(s:string) ->
   current_char := ' ',
   current_word := "",
   next_word := "",
   parsed_file := fopen(s, "r"),
   readNext()]

[closeFile() -> fclose(parsed_file)]

[parseError() : void -> printf("Error !\n"), exit(1)]

[readChar() : void => 
  current_char := getc(parsed_file),
  if (current_char >= 'A' & current_char < 'Z') current_char := char!(integer!(current_char) + diff_upcase)]

eof :: 'ÿ'

[readNext() : string ->
  let n := 0 in (
      current_word := next_word,
      while (current_char = space | current_char = comt | current_char = endl | current_char = endl2 | current_char = tabul) (
	if (current_char = comt) while (current_char != endl) current_char := getc(parsed_file),
	readChar()),
      buffer[n :+ 1] := current_char, 
      readChar(),
      if not(buffer[1] % {lparen, rparen, eof}) (
	while  (current_char != space & 
		current_char != comt & 
		current_char != endl & 
		current_char != endl2 & 
		current_char != tabul & 
		current_char != '?' & 
		current_char != lparen & 
		current_char != rparen &
		current_char != eof
	       ) (
	  buffer[n :+ 1] := current_char,
	  readChar())), 
      next_word := substring(buffer, 1, n),
      current_word)]

[currentWord() : string => current_word]
[nextWord() : string => next_word]

[checkWord(s:string, w:string) : void => if (s != w) parseError()]
[checkNext(s:string) : void => checkWord(s, readNext())] 
[testNext(s:string) : boolean => if (s = nextWord()) (readNext(), true) else false]

[readBegin() : void -> if (readNext() != "(") parseError()]
[readEnd() : void => if (readNext() != ")") parseError()]
[testBegin() : boolean => testNext("(")]
[testEnd() : boolean => testNext(")")]
		       

[readDomain() : Domain ->
  readBegin(),
  checkNext("define"),
  readBegin(),
  checkNext("domain"),
  let d := Domain(name = readNext()) in (
      readEnd(),
      while testBegin() (
	readNext(),
	case currentWord() (
	  {":requirements"} readRequirements(d),
	  {":types"} readTypes(d),
	  {":constants"} readConstants(),
	  {":predicates"} readPredicates(),
	  {":functions"} readFunctions(),
	  {":action", ":durative-action"} (cpt_var :+ 1, add(d.operators, readOperator(d))))),
      readEnd(),
      d)]

[readInstance(d:Domain) : Instance ->
  readBegin(),
  checkNext("define"),
  readBegin(),
  checkNext("problem"),
  let pb := Instance(name = readNext(), domain = d) in (
      readEnd(),
      while testBegin() (
	readNext(),
	case currentWord() (
	  {":domain"} readDomainInstance(),
	  {":objects"} readObjects(),
	  {":init"} readInitState(pb),
	  {":goal"} readGoal(pb))),
      readEnd(),
      pb)]

[readDomainInstance() : void -> 
  while not(testEnd()) readNext()]

[readObjects() : void ->
  readConstants()]

[readInitState(pb:Instance) : void ->
  let (pos, neg) := readAtomList(pb.domain.durative) in (
      for a in list{a in (pos /+ neg) | known?(a.tinitRat)} add(pb.timedLitterals, a),
      pb.initState := list<Atom>{a in pos | unknown?(a.tinitRat)})]

[readGoal(pb:Instance) : void ->
  readBegin(),
  pb.goal := readConjunction(false)[1]]


[readRequirements(d:Domain) : void ->
  while not(testEnd()) (
    readNext(),
    case currentWord() (
      {":equality"} d.equality := true,
      {":durative-actions"} d.durative := true))]

[readTypes(d:Domain) : void ->
  let typed := list<Predicate>() in (
      while not(testEnd()) (
	add(typed, createType(readNext())),
	if testNext("-") (
	  let type := createType(readNext()) in (
	      for t in typed add(t.upTypes, type),
	      shrink(typed, 0)))))]

[readConstants() : void ->
  let constants := readTermList() in (
      nil)]

[readPredicates() : void ->
  while not(testEnd()) (
    while not(testEnd()) readNext())]

[readFunctions() : void ->
  while not(testEnd()) (
    while not(testEnd()) readNext())]

[readOperator(d:Domain) : Operator ->
   let o := createOperator(readNext()) in (
       while not(testEnd()) (
	 readNext(),
	 case currentWord() (
	   {":parameters"} readParameters(o),
	   {":duration"} readDuration(o),
	   {":precondition", ":condition"} readPrecondition(d, o),
	   {":effect"} readEffect(d, o))),
       o)]

[readParameters(o:Operator) : void ->
  readBegin(),
  o.parameters := readTermList(),
  for i in (1 .. length(o.parameters)) (
    o.parameters[i].index := i)]

functions_table[p:Predicate, l:list<Term>] : (Rational U {unknown}) := unknown

[readDuration(o:Operator) : void ->
  if testBegin() (
    checkNext("="),
    checkNext("?duration"),
    o.durationExpr := readExpression(),
    readEnd())
  else
    o.durationExpr := readExpression()]

[readPrecondition(d:Domain, o:Operator)  : void ->
  readBegin(),
  let (pos, neg) := readConjunction(d.durative) in (
    o.precondition := pos,
    for a in neg (
      if (a.pred.name = "=") (
	if (a.terms[1].index < a.terms[2].index) add(a.terms[2].inequalities, a.terms[1] as Variabl)
	else if (a.terms[1].index > a.terms[2].index) add(a.terms[1].inequalities, a.terms[2] as Variabl)
	else parseError())))]
    
[readEffect(d:Domain, o:Operator)  : void ->
  readBegin(),
  let (pos, neg) := readConjunction(d.durative) in (
      for a in (pos /+ neg) a.pred.typing := false,
      o.addEffect := pos,
      o.delEffect := neg)]

[readConjunction(timed:boolean) : tuple(list<Atom>, list<Atom>) ->
  if testNext("and") readAtomList(timed)
  else if testEnd() tuple(list<Atom>(), list<Atom>())
  else (   
    let a := readAtom(timed), pos := list<Atom>(), neg := list<Atom>() in (
	addAtomToList(a, pos, neg),
	tuple(pos, neg)))]

[readAtomList(timed:boolean) : tuple(list<Atom>, list<Atom>) ->
  let pos := list<Atom>(), neg := list<Atom>() in (
      while testBegin() (
	when a := readAtom(timed) 
	in addAtomToList(a, pos, neg)),
      readEnd(),
      tuple(pos, neg))]

[addAtomToList(a:Atom, l:list<Atom>) : void => 
  if (known?(a.tinitRat) | forall(a' in l | (a.pred != a'.pred | a.terms != a'.terms)))
    add(l, a)]
[addAtomToList(a:PositiveAtom, pos:list<Atom>, neg:list<Atom>) : void => addAtomToList(a, pos)]
[addAtomToList(a:NegativeAtom, pos:list<Atom>, neg:list<Atom>) : void => addAtomToList(a, neg)]

[readAtom(timed:boolean) : (Atom U {unknown}) ->
  let sign := true, time := unknown in (
      if timed (
        if (testNext("over") & testNext("all")) readBegin()
        else if testNext("at") (
          if testNext("start") readBegin()
          else if testNext("end") readBegin()
          else (time := readRational(), readBegin()))
        else timed := false),
      if testNext("not") (sign := false, readBegin()),
      let name := readNext() in (
	  if (name = "=" & testBegin()) (
	    let func := readAtom(false) in (
		let n := readRational() in (
		    functions_table[func.pred, func.terms] := n,
		    readEnd(), 
		    unknown)))
	  else (
	    let t := readTermList() in (
		if not(sign) readEnd(), 
		if timed readEnd(),
		if (time = zero) time := unknown,
		createAtom(createPredicate(name), t, sign, time)))))]
      
[readRational() : Rational ->
  stringToRational(readNext())]

[stringToRational(s:string) : Rational ->
  let rat := false, n := 0, d := 1 in (
      for i in (1 .. length(s)) (
	if (s[i] = '.') rat := true
	else (
	  n := n * 10 + integer!(s[i]) - integer!('0'),
	  if rat (d :* 10))),
      Rational(num = n, den = d))]

[readExpression() : Expression ->
  if testBegin() (
    case nextWord() (
      {"+"} Addition(terms = readOperationTerms()),
      {"-"} Substraction(terms = readOperationTerms()),
      {"*"} Multiplication(terms = readOperationTerms()),
      {"/"} Division(terms = readOperationTerms()),
      any (readAtom(false) as Atom)))
  else readRational()]

[readOperationTerms() : list<Expression> ->
  readNext(),
  let terms := list<Expression>() in (
      while not(testEnd()) (
	add(terms, readExpression())),
      terms)]

      
[readTermList() : list<Term> ->
  let res := list<Term>(), current := list<Term>() in (
      while not(testEnd()) (
	let term := createTerm(readNext()) in (
	    add(res, term),
	    add(current, term),
	    if testNext("-") (
	      let 
		type := createType(readNext()), 
		a := createAttributeSpace(type, 1)
	      in (
		  for term in current (
		    if term.isvar add(term.attPrec, a)
		    else add(typed_constants, tuple(type, term))),
		  shrink(current, 0))))),
      res)]

[createAttributeSpaces(d:Domain) : void ->
  for o in d.operators (
    for a in o.precondition (
      for i in (1 .. length(a.terms)) (
	let v := a.terms[i] in (
	    if v.isvar 
	      add(v.attPrec, createAttributeSpace(a.pred, i))))),
    for a in o.addEffect (
      for i in (1 .. length(a.terms)) (
	let v := a.terms[i], a' := createAttributeSpace(a.pred, i) in (
	    if v.isvar (if not(a' % v.attPrec) add(v.attEffect, a'))
	    else addConstant(a.pred, i, v)))),
    for v in list{v in o.parameters | v.attEffect } (
      let tr := some(tr in TransitionRule | v.attPrec = tr.enablers) in (
	  if known?(tr) tr.consequences := tr.consequences U v.attEffect
	  else let tr := TransitionRule(enablers = v.attPrec, consequences = v.attEffect) in (
	    for a in v.attPrec (
	      add(attribute_spaces_table[a], tr))))))]


fluent_table[p:Predicate, l:list<Term>] : (Fluent U {unknown}) := unknown
constraints_table[p:Predicate, l:list<Term>] : boolean := false


[createFluent(pb:Instance, a:Atom) : Fluent =>
  createFluent(pb, a.pred, a.terms)]

[createFluent(pb:Instance, a:Atom, c:list<Term>) : Fluent =>
  createFluent(pb, a.pred, list<Term>{(if v.isvar c[v.index] else v) | v in a.terms})]

[createFluent(pb:Instance, p:Predicate, t:list<Term>) : Fluent ->
  when f := fluent_table[p, t] in f
  else let f := createFluent(p, t) in (
    fluent_table[p, t] := f,
    add(pb.fluents, f),
    f)]

[createFluent(p:Predicate, c:list<Term>) : Fluent =>
  Fluent(name = (print_in_string(), printf("(~S", p), for t in c printf(" ~S", t), princ(")"), end_of_string()))]

[instantiateOperator(pb:Instance, o:Operator, i:integer, params:list<Term>) : void ->
  if (i <= length(o.parameters)) (
    for c in (if pb.domain.equality list{c in o.parameters[i].domaine | forall(v in o.parameters[i].inequalities | params[v.index] != c)}
	      else list{c in o.parameters[i].domaine | forall(j in (1 .. i - 1) | params[j] != c)}) (
      params[i] := c,
      if forall(a in o.constraints[i] | constraints_table[a.pred, list{(if v.isvar params[v.index] else v) | v in a.terms}])
	instantiateOperator(pb, o, i + 1, params)))
 else 
   let a := Action(name = o.name, parameters = copy(params))
   in (
       try (
	 a.durationRat := evaluateExpr(o.durationExpr, params),
	 simplify(a.durationRat),
	 a.prec := list<Fluent>{createFluent(pb, a', params) | a' in o.precondition},
	 a.add := list<Fluent>{createFluent(pb, a', params) | a' in o.addEffect},
	 a.del := list<Fluent>{createFluent(pb, a', params) | a' in o.delEffect},
	 a.edel := copy(a.del),
	 add(pb.actions, a))
       catch any nil)]

[instantiateOperators(pb:Instance) : void ->
   let ini := list<Fluent>(), timed := list<Action>() in (
       for a in list{a in pb.initState | not(a.pred.typing)} (
	 let f := createFluent(pb, a) in (
	     if not(f % ini) add(ini, f))),
       add(pb.actions, Action(name = "start", add = ini)),
       ini := list<Fluent>(), 
       for a in pb.goal (
	 if a.pred.typing (
	   if not(constraints_table[a.pred,a.terms]) (
	     printf("done : goal ~S not reachable.\n", a), exit(0)))
	 else (
	   let f := createFluent(pb, a) in (
	       if not(f % ini) add(ini, f)))),
       add(pb.actions, Action(name = "end", prec = ini)),
       for a in pb.timedLitterals addEvent(pb, a),
       for a in pb.events add(pb.actions, a)),
   for o in pb.domain.operators (
     instantiateOperator(pb, o, 1, copy(o.parameters)))]

[addEvent(pb:Instance, a:NegativeAtom) : void ->
  let a' := some(a' in pb.events | a.tinitRat = a'.durationRat), f := createFluent(pb, a) in (
      if unknown?(a') (a' := Action(name = "event", isevent = true, durationRat = a.tinitRat), add(pb.events, a')),
      add(a'.del, f), 
      add(a'.edel, f))]

[addEvent(pb:Instance, a:PositiveAtom) : void ->
  let a' := some(a' in pb.events | a.tinitRat = a'.durationRat), f := createFluent(pb, a) in (
      if unknown?(a') (a' := Action(name = "event", isevent = true, durationRat = a.tinitRat), add(pb.events, a')),
      add(a'.add, f))]

    
[addEdels(a:Action, f:Fluent, fluents:list<Fluent>) : void ->
  for f' in fluents (
    if (not(deletes(a, f')) & not(produces(a, f')) & getPairCost(f, f') = MAXINT) (
      setDeletes(a, f'),
      add(f'.deleters, a),
      add(a.edel, f')))]

[computeReachability(a:Action) : void ->
  if not(a.reachable)  (
    a.reachable := true,
    for f in list{f in a.add | f.enabled & not(f.reachable)} (
      f.reachable := true,
      for a' in f.consumers (
	a'.reachedPrec :+ 1,
	if (a'.reachedPrec = length(a'.prec))
	  computeReachability(a'))))]

[computeReachability(f:Fluent) : void ->
  if not(f.reachable) (
  f.reachable := true,
  for a' in f.consumers (
    a'.reachedPrec :+ 1,
    if (a'.reachedPrec = length(a'.prec))
      computeReachability(a')))]

[computeDurations(pb:Instance) : void ->
  let p := 1, g := 0 in (
      for a in list{a in pb.actions | a.durationRat != zero} (
	p := ppcm(p, a.durationRat.den)),
      for a in list{a in pb.actions | a.durationRat != zero} (
	a.duration := a.durationRat.num * (p / a.durationRat.den)),
      for a in list{a in pb.actions | a.durationRat != zero} (
	g := pgcd(g, a.durationRat.num)),
      for a in list{a in pb.actions | a.durationRat != zero} (
	a.duration := a.duration / g),
      pb.ppcm := p,
      pb.pgcd := g)]

[readProblem(s:Session) : Instance ->
  openFile(s.operators),
  beginMonitor("Parsing domain"),
  let d := readDomain() in (
      closeFile(),
      openFile(s.facts),
      restartMonitor("Parsing problem"), 
      let pb := readInstance(d), n := 0 in (
	  closeFile(),

	  restartMonitor("Computing variable domains"), 

	  ;printf("~S\n", d),
	  ;printf("~S\n", p),
	  
	  createAttributeSpaces(d),
	 
	  for a in (pb.initState /+ pb.timedLitterals) (
	    for i in (1 .. length(a.terms)) (
	      addConstant(a.pred, i, a.terms[i]))),
	  
	  for c in typed_constants (
	    addConstant(c[1], 1, c[2])),

	  for v in Variabl (
	    v.domaine := set<Constant>{x in Constant | forall(a in v.attPrec| x % constants_table[a])}),
	  
	  #if PRETTY (
	    if s.print_domains (
	      princ("\n\n================= Domain operators =================\n\n"),
	      for o in d.operators printf("~S\n\n", o),
	      princ("\n\n================= TimedLitterals =================\n\n"),
	      for a in pb.timedLitterals printf("~S ~S\n", a, a.tinitRat),
	      princ("\n================= Transition rules =================\n\n"),
	      for tr in TransitionRule printf("~S\n", tr),
	      princ("\n================== Attribute spaces ==================\n\n"),
	      for a in attribute_spaces printf("~S : ~A\n", a, constants_table[a]),
	      princ("\n================== Variable domains ==================\n\n"),
	      for o in d.operators (
		printf("Operator ~A :\n", o.name),
		for v in o.parameters (
		  printf("  ~S : ~A\n", v, v.domaine))),
	      princ("\n"))),
	  
	  for a in pb.timedLitterals a.pred.typing := false,
	  for a in list{a in pb.initState | a.pred.typing} constraints_table[a.pred, a.terms] := true,

	  
	let ops := d.operators, good := true in (
	    d.operators := list<Operator>(),
	    for o in ops (
	      good := true,
	      o.constraints := make_list(length(o.parameters), list, list<list>()),
	      for i in (1 .. length(o.constraints)) o.constraints[i] := list<Atom>(),
	      for a in list{a in o.precondition | a.pred.typing} (
	        let rep := exists(t in a.terms | t.isvar) in ( // BUG OF CLAIRE !!!
		  if rep (if (a.pred.arity > 1) add(o.constraints[max(list{(if v.isvar v.index else 0) | v in a.terms})], a))
		  else if not(constraints_table[a.pred, a.terms]) (good := false, break()))),
	      if good (
		o.precondition := list{a in o.precondition | not(a.pred.typing)},
		add(d.operators, o)))),
	  
	  restartMonitor("Instantiating actions"), 
	  
	  instantiateOperators(pb),

	  restartMonitor("Reachability analysis"), 
	  
	  for a in pb.actions (for f in a.prec add(f.consumers, a)),
	  for a in {a in pb.actions | not(a.prec)} computeReachability(a),
	  computeReachability(pb.actions[1]),
	  when fail := some(f in pb.actions[2].prec | not(f.reachable)) 
	  in (printf("done : goal ~S not reachable.\n", fail), exit(0)),
	  
 	  restartMonitor("Computing h2 bound"), 
	  
	  pb.fluents := list<Fluent>{f in pb.fluents | f.reachable},
	  n := 0,
	  for f in pb.fluents (
	    f.num := (n :+ 1), 
	    f.consumers := list<Action>()),
	  
	  pb.actions := list<Action>{a in pb.actions | a.reachable},
	  n := 0,
	  for a in pb.actions (
	    a.num := (n :+ 1),
	    a.numinit := a.num,
	    for f in a.prec add(f.consumers, a),
	    for f in a.add add(f.producers, a),
	    for f in a.edel add(f.deleters, a)),
	  
	  for a in pb.actions closeActionParser(pb.actions, pb.fluents, a),
	  for f in pb.fluents closeFluentParser(pb.fluents, f),

	computeDurations(pb),
	let p := pgcd(pb.ppcm, s.initial_bound.den) in (
	    for e in pb.events (
	      e.tinitEvent := e.duration,
	      e.durationRat := zero,
	      e.duration := 0),

	    case s.init_heuristic (
	      {H0} computeInitH0Cost(pb.actions, pb.fluents),
	      {H1} computeInitH1Cost(pb.actions, pb.fluents),
	      {H2} computeInitH2Cost(pb.actions, pb.fluents)),
	    
	    s.initial_bound.num := (s.initial_bound.num * (pb.ppcm / p)) / (s.initial_bound.den / p) / pb.pgcd,
	    s.initial_bound.den := 1,
	    
	    restartMonitor("Cleaning structures"), 
	  
	    pb.fluents := list<Fluent>{f in pb.fluents | getPairCost(f, f) != MAXINT},
	    for f in pb.fluents (
	      f.producers := list<Action>(),
	      f.consumers := list<Action>(),
	      f.deleters := list<Action>()),
	    
	    pb.actions := list<Action>{a in pb.actions | a.tinit != MAXINT},
	    n := 0,
	    for a in pb.actions (
	      a.num := (n :+ 1),
	      for f in a.prec add(f.consumers, a),
	      for f in a.add add(f.producers, a),
	      for f in a.del add(f.deleters, a)),
	    
	    for a in list{a in pb.actions | a.num > 2} (
	      for f in a.prec addEdels(a, f, pb.fluents),
	      for f in a.add addEdels(a, f, pb.fluents)),

	    n := 0,
	    for f in pb.fluents (f.num := (n :+ 1)),

	    endMonitor(),
	    
	  #if PRETTY (
	    if s.print_actions (
	      princ("\n\n================== Ground actions ==================\n\n"),
	      for a in pb.actions complete_print(a),
	      for a in pb.fluents complete_print(a),
	      princ("\n"))),

	  pb)))]

