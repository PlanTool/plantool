// ********************************************************************
// * CPT, version 1.0 sep. 2004                                       *
// * file: static-conflicts.cl                                        *
// * Copyright (©) Vincent Vidal, 2003-2004                           *
// ********************************************************************

Atom <: ephemeral_object
Symbol <: object
Term <: Symbol

Rational <: ephemeral_object(num:integer, den:integer)

zero :: Rational(num = 0, den = 1)

[self_print(r:Rational) : void -> 
  printf("~S", r.num),
  if (r.den != 1)
    printf("/~S", r.den)]

OperationTerms <: ephemeral_object

Addition <: OperationTerms()
Substraction <: OperationTerms()
Multiplication <: OperationTerms()
Division <: OperationTerms()

Expression :: (Addition U Substraction U Multiplication U Division U Rational U Atom)

OperationTerms <: ephemeral_object(terms:list<Expression>)

[self_print(terms:list<Expression>) : void -> for t in terms printf(" ~S", t)]
[self_print(a:Addition) : void -> printf("(+~S)", a.terms)]
[self_print(s:Substraction) : void -> printf("(-~S)", s.terms)]
[self_print(m:Multiplication) : void -> printf("(*~S)", m.terms)]
[self_print(d:Division) : void -> printf("(/~S)", d.terms)]


[evaluateTerms(o:OperationTerms, p:list<Term>) : list[Rational] =>
  list{evaluateExpr(t, p) | t in o.terms}]

[evaluateExpr(r:Rational, p:list<Term>) : Rational -> r]

// a is an Atom!!
[evaluateExpr(a:any, p:list<Term>) : Rational ->
  let par := list<Term>{(if v.isvar p[v.index] else v) | v in a.terms} in (
      when r := functions_table[a.pred, par] 
      in r else (contradiction!(), Rational()))]

[evaluateExpr(a:Addition, p:list<Term>) : Rational ->
  let r := Rational(), prem := true in (
      for t in evaluateTerms(a, p) (
	addRational(r, t, prem),
	prem := false),
      r)]

[evaluateExpr(s:Substraction, p:list<Term>) : Rational ->
  let r := Rational(), prem := true in (
      for t in evaluateTerms(s, p) (
	subRational(r, t, prem),
	prem := false),
      r)]

[evaluateExpr(m:Multiplication, p:list<Term>) : Rational ->
  let r := Rational(), prem := true in (
      for t in evaluateTerms(m, p) (
	multRational(r, t, prem),
	prem := false),
      r)]

[evaluateExpr(d:Division, p:list<Term>) : Rational ->
  let r := Rational(), prem := true in (
      for t in evaluateTerms(d, p) (
	divRational(r, t, prem),
	prem := false),
      r)]

[addRational(r:Rational, t:Rational, prem:boolean) : void ->
  if prem (r.num := t.num, r.den := t.den)
  else 
    let n := r.num * t.den + r.den * t.num in (
	r.den := r.den * t.den,
	r.num := n)]

[subRational(r:Rational, t:Rational, prem:boolean) : void ->
  if prem (r.num := t.num, r.den := t.den)
  else 
    let n := r.num * t.den - r.den * t.num in (
	r.den := r.den * t.den,
	r.num := n)]

[multRational(r:Rational, t:Rational, prem:boolean) : void ->
  if prem (r.num := t.num, r.den := t.den)
  else (
    r.num := r.num * t.num,
    r.den := r.den * t.den)]

[divRational(r:Rational, t:Rational, prem:boolean) : void ->
  if prem (r.num := t.num, r.den := t.den)
  else (
    r.num := r.num * t.den,
    r.den := r.den * t.num )]

[simplify(r:Rational) : void ->
  let d := pgcd(r.num, r.den) in (
      r.num := r.num / d,
      r.den := r.den / d)]

[=(r1:Rational, r2:Rational) : boolean =>
  r1.num = r2.num & r1.den = r2.den]

[!=(r1:Rational, r2:Rational) : boolean =>
  r1.num != r2.num | r1.den != r2.den]

[pgcd(a:integer, b:integer) : integer ->
  let rest := a mod b in (
      while (rest != 0) (
	a := b,
	b := rest,
	rest := a mod b),
      b)]

[ppcm(a:integer, b:integer) : integer ->
  a * b / pgcd(a,b)]
