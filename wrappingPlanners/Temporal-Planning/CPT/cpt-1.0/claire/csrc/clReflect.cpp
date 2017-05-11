/***********************************************************************/
/**   microCLAIRE                                       Yves Caseau    */
/**   clReflect.cpp                                                    */
/**  Copyright (C) 1998-2003 Yves Caseau. All Rights Reserved.         */
/**  cf claire.h                                                       */
/***********************************************************************/

#include <claire.h>
#include <Kernel.h>
#include <marie.h>

// ----------------------------------------------------------------------------
// this file contains the methods for reflective objects (and classes)
// it uses a set of external functions that are defined in System (claire code)
// ----------------------------------------------------------------------------

/*********************************************************************/
/** Contents                                                         */
/**    1. Objects                                                    */
/**    2. Class                                                      */
/**    3. Properties                                                 */
/**    4. Tables                                                     */
/*********************************************************************/


/*********************************************************************/
/**    1. Objects                                                    */
/*********************************************************************/

// v3.2.24: was awfully slow -> introduce a direct link to member? method
ClaireBoolean *ClaireCollection::contains(OID oself)
{ if (isa == Kernel._class)
     return (INHERIT(OWNER(oself),((ClaireClass *) this)) ? CTRUE : CFALSE);
  else if (isa == Kernel._list) return contain_ask_list((list *) this, oself);
  else if (isa == Kernel._set) return contain_ask_set((set *) this, oself);
  else {method  *m = OBJECT(method, (*Kernel.member_ask->definition)[1]);
        return ( (ClaireBoolean*)
                 ( ((fptr2) (m->functional->value))(oself, (int) this)));  }}

// ---------------------- API functions ----------------------------------------

// copy an object (or an item) */
ClaireObject *copy_object(ClaireObject *x)
{int i,m = *SLOTADR(x,0);
 ClaireObject *y = (ClaireObject *) ClAlloc->makeAny(m);
 for (i = 1; i<= m; i++) *SLOTADR(y,i) = *SLOTADR(x,i);
 return y;}

// logical equality
ClaireBoolean *equal(OID n, OID m)
{  if (n == m) return CTRUE;
   else if (IDENTIFIED(n) || IDENTIFIED(m)) return CFALSE;
   else {ClaireAny *x = OBJECT(ClaireAny,n);
         ClaireAny *y = OBJECT(ClaireAny,m);
         if (x->isa != y->isa) return CFALSE;
         else if (x->isa == Kernel._list || x->isa == Kernel._tuple)
            return((list *) x)->equalList((list *) y);
         else if (x->isa == Kernel._set) return((set *) x)->equalSet((set *) y);
         else if (x->isa == Kernel._string) return equal_string(string_v(n),string_v(m));
         else if (x->isa == Kernel._float)
             {if (((ClaireFloat *) x)->value == ((ClaireFloat *) y)->value) return CTRUE;
              else return CFALSE;}
         else {if (((ClaireImport *) x)->value == ((ClaireImport *) y)->value) return CTRUE;
               else return CFALSE;}}}

// reading a value in a slot
// new in v2.5: floats
OID slot_get_object(ClaireObject *x, int y, ClaireClass *s)
{if (s == Kernel._float)
    {return _float_(*( (double *) SLOTADR(x,y) )); }
 else {int z = *SLOTADR(x,y);
        // if (ClEnv->verbose > 10) printf("read slot[%d] of %d -> %d\n",y,x,z);
        return ((z == 0) ? (((s == Kernel._integer) | (s == Kernel._any)) ? 0 : CNULL) :
                 ((z == CNULL) ? CNULL : CLAIREOID(z,s)));}}

// membership for any collection
ClaireBoolean *belong_to(OID oself, OID ens)
{ ClaireClass *c = OWNER(ens);
   if (c == Kernel._integer)
       return ((oself >= 1 && oself <= 29 && BCONTAIN(ens,oself)) ? CTRUE : CFALSE);
   else if (c == Kernel._array) return contain_ask_array(array_v(ens),oself);
   else return OBJECT(ClaireCollection,ens)->contains(oself);}

// hashing for a generic list of length 2^n - 2
int hash_list(list *l, OID x)
{int i = ClRes->hashOid(l->content[0] - 1,x);
   if (i < l->length) return (i + 1);
   else return (1 + (i % l->length));}

//convert anything into a Boolean
ClaireBoolean *boolean_I_ClaireAny(ClaireAny *x)
 {if (x->isa == Kernel._boolean) return (ClaireBoolean *) x;
  else if (x->isa == Kernel._set || x->isa == Kernel._list)
      return ( (((list *) x)->length != 0) ? CTRUE : CFALSE);
  else if (x == CFALSE) return CFALSE;     // to remove later on
  else return CTRUE;}

ClaireBoolean *boolean_I_any(OID n)
 { if ((INT_MASK & n) != OBJ_CODE) return CTRUE;
   else return boolean_I_ClaireAny(OBJECT(ClaireAny,n)); }


// ---- three debugging functions which are quite useful ------------------------------------
int CL_Address(OID x) {return ADR(x);}

char *CL_Oid(int x)
{char *s = make_string_integer(15,ClRes->ascii[32]);
  sprintf(s,"%d",x);  return s;}

OID CL_Oid_inv(char *s)
{OID x;
   sscanf(s,"%d",&x); return x;}

/*********************************************************************/
/**    2. Class                                                      */
/*********************************************************************/

// new in v3.1.16: float alignment parity is an option
// for 99% of C++ compilers, float slots need to be aligned (even)
// so the C++ compiler wastes memory if needed
// #ifdef CLFLPAR
// #define CL_FLOAT_PARITY 666      // disable ! needed for LINUX-Intel
// #else
// #define CL_FLOAT_PARITY 0        // ensure alignment of float slots (default)
// #endif

// NEW in v3.3.36 : Sylvain proposed a better, more portable solution for float:
// we create two dumb classes that will be used to see how float alignment both
// generally and within an object (as a fied) is handled
//<sb> some architecture may need structure alignment and/or field alignment
class align_struct : ClaireObject {double d;};
class align_field : ClaireObject {int pad1; int pad2; double d;};

#define ALIGN_STRUCT (sizeof(class align_struct) != sizeof(double) + sizeof(int))
#define ALIGN_FIELD (sizeof(class align_field) != sizeof(double) + 3 * sizeof(int))

// simple instantiation for objects with OID slots (exceptions)
ClaireObject *ClaireClass::operator() (OID arg1)
{int n = prototype->length;
 ClaireObject *o = instantiate(n);
 int rep = ADR(_oid_(o)) + 2;
 Cmemory[rep++] = arg1;
 return o;}

ClaireObject *ClaireClass::operator() (OID arg1, OID arg2)
{int n = prototype->length;
 ClaireObject *o = instantiate(n);
 int rep = ADR(_oid_(o)) + 2;
 Cmemory[rep++] = arg1;
 Cmemory[rep++] = arg2;
 return o;}


ClaireObject *ClaireClass::operator() (OID arg1, OID arg2, OID arg3)
{int n = prototype->length;
 ClaireObject *o = instantiate(n);
 int rep = ADR(_oid_(o)) + 2;
 Cmemory[rep++] = arg1;
 Cmemory[rep++] = arg2;
 Cmemory[rep++] = arg3;
 return o;}


// this first constructor works with no environment and is used to create the
// first classes
ClaireClass *ClaireClass::make(ClaireAny *x)
{ClaireClass *c = (ClaireClass *) x;
   // if (ClEnv->verbose > 11) printf("=== class::make allocates a class @ %x \n",(int) c);
   c->ancestors = list::empty();
   c->slots = list::empty();
   c->instances = list::empty();
   c->subclass = set::empty();
   c->isa = Kernel._class;
   c->evaluate = (ClaireFunction *) NULL;        // will be setup later
   c->open = 2;
   c->params = list::empty();
   c->dictionary = list::empty();
   c->dispatcher = list::empty();
   c->code = 0;
   c->ident_ask = CTRUE;
   return c;
   }

// same with a name
ClaireClass *ClaireClass::make(char *n)
{ClaireClass *c = make( ClAlloc->makeStatic(18)) ;
 symbol *s = symbol::make(n,claire.it,ClEnv->module_I);
 s->value = _oid_(c);
 c->name = s;
 c->comment = n;
 Kernel._class->instances->addFast(_oid_(c));
 return c;}

// this is the regular constructor
// open(p) = 0 <=> read only, 1 <=> compiled (cannot be overwritten)
//           2 <=> default (unknown), 3 <=> open (extensible)
//           -1 : system property that may be overwritten  (v3.2.24)
// V3.2 PATCH : final(p) => open(p) = 0 because we allow overwriting open=1 properties !
// open(c) = -2 <=> system_set, -1 <=> no changes !
//           0  <=> no more instance, 1 <=> no new subclasses
//           2 <=> default, 3 <=>open
//           4 <=> ephemeral
ClaireClass *ClaireClass::make(char *name, ClaireClass *c2, module *def)
{ClaireClass *c = (ClaireClass *) Kernel._class->instantiate(name,def);
//  if (ClEnv->verbose > 10)
//      see(">>> ClaireClass::make prototype = ",_oid_(Kernel._class->prototype));
   c->comment = name;
   c->superclass = c2;
   c->slots = (list *) copy_bag(c2->slots);
   if (c2->open == ClEnv->ephemeral) c->open = ClEnv->ephemeral;
   c->prototype = (list *) copy_bag(c2->prototype),
   c->evaluate = c2->evaluate;
   c->genealogy();
   if (c->code == 0) c->encode();
   c->dictionary = (list *) copy_bag(c2->dictionary);
   c->dispatcher = (list *) copy_bag(c2->dispatcher);
   return c;}

// instantiation methods from simple to more complex
ClaireObject *ClaireClass::instantiate(int n)
{ClaireObject *o = (ClaireObject *) ClAlloc->makeAny(n);
 o->isa = this;
 return o;}

// here we use the prototype list as the pattern
// note: the prototype only contains things that can be copied directly !!!
// in 2.9 we extend this to floats (need to skip the index count: +2)
//     copied_defaults = object (for object) + float (for float) + integer (for anything)
//                       + NULL for objects
// IMPORTANT: do not put a float or an object in the prototype if the range is ANY !!!
//            because an object in prototype means a slot of srange object
//            CNULL means please put 0 (object representation)
//            Nodefault means please put CNULL (OID representation)
// v3.2.52: there is no alighnment problem here IF (1) instantiate respects the parity of the
// object allocation (2) addSlot respects the parity of the position of the float default value
// in the prototype
ClaireObject *ClaireClass::instantiate()
{int i, n = prototype->length;
 ClaireObject *o = instantiate(n);
 ClAlloc->currentNew = o;                           // protect the object but NOT the content !
 int u = _oid_(o), rep = ADR(_oid_(o)) + 2;
 // <debug for alignment> printf("alloc object size %d -> rep = %d @ %d\n",n,rep, &o);
 for (i= 2; i<=n ; i++)
    {int look = rep - ADR(_oid_(o));
     OID v = (*prototype)[i];                          // value in the prototype
     ClaireClass *c = OWNER(v);                        // owner(v)
     if (c == Kernel._float)                           // implies a range float !
         { // <debug for alignment> printf("rep = %d, i = %d, & = %x v = %g\n",rep,i,&Cmemory[rep],float_v(v));
           *((double *) &Cmemory[rep]) = float_v(v);
           rep += 2; i += 1;}                      // v3.0.68 ! i changes => v changes
     else if ((c == Kernel._set) || (c == Kernel._list))
             {// printf("--- put a copy of %x into C[%d]\n",v,rep + 1);
              Cmemory[rep++] = (int) copy_bag(OBJECT(bag,v)); }       // copy may cause GC!
     else if (CTAG(v) == OBJ_CODE && v != CNULL)
         {if (v == _oid_(Kernel.NoDefault)) Cmemory[rep++] = CNULL;  // NEW in V3.0.41
          else  Cmemory[rep++] = (int) OBJECT(ClaireAny,v);}
     else Cmemory[rep++] = v;}
#ifdef CLDEBUG
 if (ClEnv->verbose > 11) printf("<<< instantiate returns %x at adress %d [-> %d]\n",o,getADR(o),rep - 1);
 checkOID(u); // debug !
#endif
 ClAlloc->currentNew = NULL;
 return o;}


// instantiation (for named object) m is the module for the symbol, the definition module
// is taken as the current module
thing *ClaireClass::instantiate(char *n, module *m)
{symbol *s = symbol::make(n,m,ClEnv->module_I);
   if (s->value != CNULL && CTAG(s->value) == OBJ_CODE)
      {thing *o = OBJECT(thing,s->value);
        if (o->isa != this)
            { // ClEnv->verbose = 11;
              // see("--- try to make a ",_oid_(this));
              // printf("--- with name %s\n", n);
              // see("--- and found a ",_oid_(o->isa));
              Cerror(18,_oid_(s),_oid_(o->isa));}
        // printf("reuse address %d for object named %s \n",getADR(o),n);
        return o;}
    else {thing *o = (thing *) instantiate();
           o->name = s;
           s->value = _oid_(o);
           if (open != 4) instances->addFast(_oid_(o));
           // if (ClEnv->verbose > 10) printf("create a new |%s| @ adr %d\n",n,getADR(o));
           return o;}}

// creates the genealogy for a class
void ClaireClass::genealogy()
{OID self = _oid_(this);
 ClaireClass *c = superclass;
 ITERATE(y);
   descendents = set::alloc(Kernel._class,1,self);
   for (START(c->ancestors); NEXT(y);)
      { OBJECT(ClaireClass,y)->descendents->addFast(self);}
   c->subclass->addFast(self);
   ancestors = (list *) copy_bag(c->ancestors);
   ancestors->addFast(self);}

extern void print_any(OID x);   
  
// how to create a new slot
// v3.2: this method is modified so that it may receive ix = the position of 
// the slot (obtained through a macro) in the C++ object [warning: ix may
// also be provided directly by the interpreter, thus the safety check on parity]
slot *ClaireClass::addSlot(property *p,ClaireType *t,OID def,int ix)
{slot *s = (slot *) instantiate(9);
 ClAlloc->currentNew = s;                // v3.3.34 - protection proposed by Sylvain
 ClaireClass *c1 = class_I_type(t);
 ClaireClass *s1 = sort_I_class(c1);     // sort: range for the slot
 int i = slots->length;                  // v3.2: number of existing slots
    s->isa = Kernel._slot;
    Kernel._slot->instances->addFast(_oid_(s));
    s->domain = list::alloc(Kernel._type,1,_oid_(this));
    s->selector = p;
    if (c1 == Kernel._list || c1 == Kernel._set)
       {if (p->restrictions->length == 0)
          if (c1 == Kernel._set) p->multivalued_ask = CTRUE;
          else p->multivalued_ask = Kernel._list;
        if (def == CNULL)
          if (c1 == Kernel._set) def = _oid_(set::empty(of_extract_type(t)));
          else def = _oid_(list::empty(of_extract_type(t))); }
    else if (c1 == Kernel._float && def == CNULL) def = _float_(0.0); //NOTE: test on c !!
    s->DEFAULT = def;
    if ((s1 != Kernel._integer) &&
              (s1 != Kernel._any) &&
              (def == CNULL)) def = 0;      // NULL
    if ((p->multivalued_ask == CTRUE && c1 != Kernel._set) ||
        (p->multivalued_ask == Kernel._list && c1 != Kernel._list))
       Cerror(28,_oid_(p),_oid_(s));        // v3.1.08
    p->restrictions->addFast(_oid_(s));
    if (c1 == Kernel._float && ix % 2 == 0) {  // some architecture requires even float indexes
       if (ALIGN_FIELD ||                      // v3.3.36 -> new solution from Sylvain
             (ALIGN_STRUCT &&
                //<sb> test that the new slot is the first slot of its domain
                // note : i is always positive due to isa @ object !
                OBJECT(ClaireClass,(*(OBJECT(slot,(*slots)[i])->domain))[1]) != this))
          ix++;}
    // was previously : if (c1 == Kernel._float && ix % 2 == CL_FLOAT_PARITY) ix++;
    // change the default representation that will be stored in the prototype
    //  copied_default = object (for object) + float (for float) + integer (for all) + NULL for objects
    if (s1 == Kernel._object || OWNER(def) == Kernel._integer || c1 == Kernel._float) ; // nothing (store def)
    else if (s1 == Kernel._any || s1 == Kernel._integer) 
       {if (def != CNULL) def = _oid_(Kernel.NoDefault);}    // this is the case that wa cannot handle
    else def = 0;
    // new in v3.2 : allow overiding of existing slots
    while (i > 1 & OBJECT(slot,(*slots)[i])->selector != p)  i--;
    if (i <= 1) 
        {slots->addFast(_oid_(s));     // a new slot
         // compute the index for slot s (needed for the interpreter :-( )
         if (ix > 1) 
            {slot * sprev = OBJECT(slot,slots->content[slots->length - 1]);
             int i = sprev->index + ((sprev->srange == Kernel._float) ? 2 : 1);
             if (i != ix)                 // alignment constraint: ix was add +1 !
                prototype->addFast(0);}   // maintain length(proto) = size(object)
          s->index = ix;}                 // ix is given by the interpreter or the C++ compiler !
    else {s->index = OBJECT(slot,(*slots)[i])->index;
          (*slots)[i] = _oid_(s);
          (*prototype)[s->index] = def;}  
    //  copied_default = object (for object) + float (for float) + integer (for all) + NULL for objects
    if (i <= 1)                                // new slot ! 
     {prototype->addFast(def);
      if (c1 == Kernel._float) prototype->addFast(0);}  // maintain length consistency
    s->srange = ((c1 == Kernel._float) ? c1 : s1);      // sort = float allowed only for slots
    s->module_I = ClEnv->module_I;
    s->range = t;
    insert_definition_property(p,s);           // defined in system (unless CLKTEST is defined)
    ClAlloc->currentNew = NULL;                // v3.3.38
    return s;}                 
    
    
// number of bits that are necessary to encode n children
int ClaireClass::nBits(int n)
{ return ((n < 4) ? n : ((n < 7) ? 4 : ((n < 11) ? 5 : ((n < 21) ? 6 : ((n < 36) ? 7 :
          (Cerror(101,n,0), 1))))));}


// returns the number of bits used for coding the class c
int  ClaireClass::totalBits()
{ int  n = 0;
  // see("enter totalbit",_oid_(this),0);
  // see("ancestor is ",_oid_(ancestors),1);
  ITERATE(c2);
  for (START(ancestors); NEXT(c2);)
     {// see("look at c2", c2, 0);
      if (c2 != _oid_(this)) n = n + nBits((OBJECT(ClaireClass,c2)->subclass->length));}
  return n;}

// incremental encoding
void ClaireClass::encode()
{ClaireClass *c2 = superclass;
#ifdef CLSMALL
 int  n = c2->totalBits(), i = c2->subclass->length, m = nBits(i);
 if (this == Kernel._void) ;
 else if (m == nBits(i - 1))  code = (c2->code + ClRes->makeCode(n,i,m));
 else c2->recode(n);
#else
code = 0;  // ??
#endif
}

// new partial encoding: recode all children of this class
void ClaireClass::recode(int n)
{int i = 1, m = nBits(subclass->length);
 ITERATE(c2);
 for (START(subclass); NEXT(c2);)
    OBJECT(ClaireClass,c2)->nodeCode(code,n,m,i++);}

// gives the new code for this node (a child of c)
void ClaireClass::nodeCode(int cx,int n,int m,int i)
{ if ((n + m) > 29) Cerror(102,0,0);
  code = cx + ClRes->makeCode(n,i,m);
  // printf("--- nodeCode %s -> %x\n",comment,code);
  if TRUEP(subclass) recode(n + m);}

// --------------------- API method -------------------------------------------

// create a slot
//void add_slot_class(ClaireClass *c, property *p, ClaireType *t, OID def) 
//;  {c->addSlot(p,t,def);}
void add_slot_class(ClaireClass *c, property *p, ClaireType *t, OID def, int ix) 
  {c->addSlot(p,t,def,ix);}

//void add_slot_classNew(ClaireClass *c, property *p, ClaireType *t, OID def, int ix) 
//  {c->addSlotNew(p,t,def,ix);}

// create an instance
ClaireObject *new_object_class(ClaireClass *c) {return c->instantiate();}
thing *new_thing_class(ClaireClass *c, symbol *s)
   {return c->instantiate(s->name,s->module_I);}

// create a class
ClaireClass *class_I_symbol(symbol *s, ClaireClass *c2)
{return ClaireClass::make(s->name,c2,s->module_I);}

// class to sort mapping
// note that the sort float does not exist as such (mapped to any for the methods's sake)
// float-specific tests must use the range.
ClaireClass *sort_I_class(ClaireClass *c)
{ if (c == Kernel._array || c == Kernel._any || c == Kernel._port ||
     c == Kernel._integer || c == Kernel._string || c == Kernel._void)  return c;
  else if (c == Kernel._float || c == Kernel._primitive) return Kernel._any;
  else if (c == Kernel._class) return Kernel._object;
  else if (c->superclass == Kernel._cl_import) return c;   // v3.3.22
  else return Kernel._object;}


/*********************************************************************/
/**    3. Property                                                   */
/*********************************************************************/

// create a property
property *property::make(char *name, module *m)
{property *ob = (property *) Kernel._property->instantiate(name,m);
  ob->comment = name;               // for debug
  ob->range = Kernel._any;
  ob->open = 1;
  // if (ClEnv->verbose > 10) printf("--- create property %s @ %d\n",ob->comment,getADR(ob));
  return ob;}

// same with a status
property *property::make(char *name, int op, module *m)
{property *ob = make(name,m);
  ob->open = op;
  return ob;}

// temporaty
property *property::make(char *name, int op, module *m, int i)
{property *ob = make(name,m);
  ob->open = op;
  ob->dispatcher = i;
  return ob;}

// new (v3.1) same with a dispatcher index !
property *property::make(char *name, int op, module *m,ClaireClass *c, int i)
{property *ob = make(name,m);
  ob->open = op;
  ob->dispatcher = i;
  ob->domain = c;
  return ob;}

// constructor for operations
operation *operation::make(char *name, module *m, int p)
{operation *ob = (operation *) Kernel._operation->instantiate(name,m);
  ob->comment = name;               // for debug
  ob->range = Kernel._any;
  ob->open = 1;
  ob->precedence = p;
  return ob;}
  
// constructor for operations that supports an  open status 
operation *operation::make(char *name, int op, module *m, int p)
{operation *ob = (operation *) Kernel._operation->instantiate(name,m);
  ob->comment = name;               // for debug
  ob->range = Kernel._any;
  ob->open = op;
  ob->precedence = p;
  return ob;}
  
// dynamic binding for a message (call) - We got rid of variable argument and use C++
// polymorphism
OID property::operator() (OID arg1)
{PUSH(arg1);
 return stack_apply(1);}
   
OID property::operator() (OID arg1, OID arg2)
{   PUSH(arg1); PUSH(arg2); return stack_apply(2); }

OID property::operator() (OID arg1, OID arg2, OID arg3)
{PUSH(arg1); PUSH(arg2); PUSH(arg3); return stack_apply(3); }

OID property::operator() (OID arg1, OID arg2, OID arg3, OID arg4)
{PUSH(arg1); PUSH(arg2); PUSH(arg3); PUSH(arg4); return stack_apply(4); }

OID property::operator() (OID arg1, OID arg2, OID arg3, OID arg4, OID arg5)
{PUSH(arg1); PUSH(arg2); PUSH(arg3); PUSH(arg4); PUSH(arg5);
 return stack_apply(5); }

OID property::operator() (OID arg1, OID arg2, OID arg3, OID arg4, OID arg5, OID arg6)
{PUSH(arg1); PUSH(arg2); PUSH(arg3); PUSH(arg4); PUSH(arg5);PUSH(arg6);
 return stack_apply(6); }

OID property::operator() (OID arg1, OID arg2, OID arg3, OID arg4, OID arg5,
                          OID arg6, OID arg7)
{PUSH(arg1); PUSH(arg2); PUSH(arg3); PUSH(arg4); PUSH(arg5);PUSH(arg6);PUSH(arg7);
 return stack_apply(7); }

OID property::operator() (OID arg1, OID arg2, OID arg3, OID arg4, OID arg5,
                          OID arg6, OID arg7, OID arg8)
{PUSH(arg1); PUSH(arg2); PUSH(arg3); PUSH(arg4); PUSH(arg5);PUSH(arg6);
 PUSH(arg7);PUSH(arg8);
 return stack_apply(8); }

OID property::operator() (OID arg1, OID arg2, OID arg3, OID arg4, OID arg5,
                          OID arg6, OID arg7, OID arg8, OID arg9)
{PUSH(arg1); PUSH(arg2); PUSH(arg3); PUSH(arg4); PUSH(arg5);PUSH(arg6);
 PUSH(arg7);PUSH(arg8); PUSH(arg9);
 return stack_apply(9); }

OID property::operator() (OID arg1, OID arg2, OID arg3, OID arg4, OID arg5,
                          OID arg6, OID arg7, OID arg8, OID arg9, OID arg10)
{PUSH(arg1); PUSH(arg2); PUSH(arg3); PUSH(arg4); PUSH(arg5);PUSH(arg6);
 PUSH(arg7);PUSH(arg8); PUSH(arg9); PUSH(arg10);
 return stack_apply(10); }

// v3.2.54
OID property::operator() (OID arg1, OID arg2, OID arg3, OID arg4, OID arg5,
                          OID arg6, OID arg7, OID arg8, OID arg9, OID arg10, OID arg11)
{PUSH(arg1); PUSH(arg2); PUSH(arg3); PUSH(arg4); PUSH(arg5);PUSH(arg6);
 PUSH(arg7);PUSH(arg8); PUSH(arg9); PUSH(arg10); PUSH(arg11);
 return stack_apply(11);}
   
OID property::operator() (OID arg1, OID arg2, OID arg3, OID arg4, OID arg5,
                          OID arg6, OID arg7, OID arg8, OID arg9, OID arg10,OID arg11, OID arg12)
{PUSH(arg1); PUSH(arg2); PUSH(arg3); PUSH(arg4); PUSH(arg5);PUSH(arg6);PUSH(arg7);PUSH(arg8);
 PUSH(arg9); PUSH(arg10); PUSH(arg11); PUSH(arg12);
    return stack_apply(12); }

OID property::operator() (OID arg1, OID arg2, OID arg3, OID arg4, OID arg5,
                          OID arg6, OID arg7, OID arg8, OID arg9, OID arg10,
                          OID arg11, OID arg12, OID arg13)
{PUSH(arg1); PUSH(arg2); PUSH(arg3); PUSH(arg4); PUSH(arg5);PUSH(arg6); PUSH(arg7);PUSH(arg8);
 PUSH(arg9); PUSH(arg10);PUSH(arg11); PUSH(arg12); PUSH(arg13);
 return stack_apply(13); }

OID property::operator() (OID arg1, OID arg2, OID arg3, OID arg4, OID arg5,
                          OID arg6, OID arg7, OID arg8, OID arg9, OID arg10,
                          OID arg11, OID arg12, OID arg13, OID arg14)
{PUSH(arg1); PUSH(arg2); PUSH(arg3); PUSH(arg4); PUSH(arg5);PUSH(arg6);PUSH(arg7);PUSH(arg8);
 PUSH(arg9); PUSH(arg10); PUSH(arg11); PUSH(arg12); PUSH(arg13); PUSH(arg14);
 return stack_apply(14); }

OID property::operator() (OID arg1, OID arg2, OID arg3, OID arg4, OID arg5,
                          OID arg6, OID arg7, OID arg8, OID arg9, OID arg10,
                          OID arg11, OID arg12, OID arg13, OID arg14, OID arg15)
{PUSH(arg1); PUSH(arg2); PUSH(arg3); PUSH(arg4); PUSH(arg5);PUSH(arg6);PUSH(arg7);PUSH(arg8);
 PUSH(arg9); PUSH(arg10); PUSH(arg11); PUSH(arg12); PUSH(arg13); PUSH(arg14); PUSH(arg15);
 return stack_apply(15); }

OID property::operator() (OID arg1, OID arg2, OID arg3, OID arg4, OID arg5,
                          OID arg6, OID arg7, OID arg8, OID arg9, OID arg10,
                          OID arg11, OID arg12, OID arg13, OID arg14, OID arg15, OID arg16)
{PUSH(arg1); PUSH(arg2); PUSH(arg3); PUSH(arg4); PUSH(arg5);PUSH(arg6);PUSH(arg7);PUSH(arg8);
 PUSH(arg9); PUSH(arg10);PUSH(arg11); PUSH(arg12); PUSH(arg13); PUSH(arg14); PUSH(arg15);PUSH(arg16);
 return stack_apply(16); }

OID property::operator() (OID arg1, OID arg2, OID arg3, OID arg4, OID arg5,
                          OID arg6, OID arg7, OID arg8, OID arg9, OID arg10,
                          OID arg11, OID arg12, OID arg13, OID arg14, OID arg15,
                          OID arg16, OID arg17)
{PUSH(arg1); PUSH(arg2); PUSH(arg3); PUSH(arg4); PUSH(arg5);PUSH(arg6);PUSH(arg7);PUSH(arg8);
 PUSH(arg9); PUSH(arg10);PUSH(arg11); PUSH(arg12); PUSH(arg13); PUSH(arg14); PUSH(arg15);PUSH(arg16);
 PUSH(arg17);
 return stack_apply(17); }

OID property::operator() (OID arg1, OID arg2, OID arg3, OID arg4, OID arg5,
                          OID arg6, OID arg7, OID arg8, OID arg9, OID arg10,
                          OID arg11, OID arg12, OID arg13, OID arg14, OID arg15,
                          OID arg16, OID arg17, OID arg18)
{PUSH(arg1); PUSH(arg2); PUSH(arg3); PUSH(arg4); PUSH(arg5);PUSH(arg6);PUSH(arg7);PUSH(arg8);
 PUSH(arg9); PUSH(arg10);PUSH(arg11); PUSH(arg12); PUSH(arg13); PUSH(arg14); PUSH(arg15);PUSH(arg16);
 PUSH(arg17);PUSH(arg8);
 return stack_apply(18); }

OID property::operator() (OID arg1, OID arg2, OID arg3, OID arg4, OID arg5,
                          OID arg6, OID arg7, OID arg8, OID arg9, OID arg10,
                          OID arg11, OID arg12, OID arg13, OID arg14, OID arg15,
                          OID arg16, OID arg17, OID arg18, OID arg19)
{PUSH(arg1); PUSH(arg2); PUSH(arg3); PUSH(arg4); PUSH(arg5);PUSH(arg6);PUSH(arg7);PUSH(arg8);
 PUSH(arg9); PUSH(arg10);PUSH(arg11); PUSH(arg12); PUSH(arg13); PUSH(arg14); PUSH(arg15);PUSH(arg16);
 PUSH(arg17);PUSH(arg18); PUSH(arg19);
 return stack_apply(19); }

OID property::operator() (OID arg1, OID arg2, OID arg3, OID arg4, OID arg5,
                          OID arg6, OID arg7, OID arg8, OID arg9, OID arg10,
                          OID arg11, OID arg12, OID arg13, OID arg14, OID arg15,
                          OID arg16, OID arg17, OID arg18, OID arg19, OID arg20)
{PUSH(arg1); PUSH(arg2); PUSH(arg3); PUSH(arg4); PUSH(arg5);PUSH(arg6);PUSH(arg7);PUSH(arg8);
 PUSH(arg9); PUSH(arg10);PUSH(arg11); PUSH(arg12); PUSH(arg13); PUSH(arg14); PUSH(arg15);PUSH(arg16);
 PUSH(arg17);PUSH(arg18); PUSH(arg19); PUSH(arg20);
 return stack_apply(20); }

// this is used by the compiler
OID property::super(ClaireClass *c,int size)
{int j = ClEnv->index, i = j - size;
 return eval_message_property(this, find_which_class(c,definition,i,j),i,CFALSE);}


OID property::stack_apply(int size)
{int i = ClEnv->index - size;
 return eval_message_property(this,
              find_which_property(this,i,OWNER(ClEnv->stack[i])), i, CFALSE);}


// how to create a new method
method *property::addMethod(list *dom, ClaireType *ran, int sta, ClaireFunction *f)
{method *m = (method *) Kernel._method->instantiate();
   Kernel._method->instances->addFast(_oid_(m));
   m->selector = this;
   restrictions->addFast(_oid_(m));
   m->domain = dom;
   m->range = ran;
   m->module_I = ClEnv->module_I;
   m->functional = f;
   m->evaluate = f;
   m->status = sta;
   // computes the list of sorts
   {list* l = list::empty(Kernel._class);
    ITERATE(z);
    for (START(dom); NEXT(z);)
         l->addFast(_oid_(sort_I_class(class_I_type(OBJECT(ClaireType,z)))));
    l->addFast(_oid_(sort_I_class(class_I_type(ran))));
    m->srange = l;}
   insert_definition_property(this,m);
   return m;}


// special case for a float method
method *property::addFloatMethod(list *dom, ClaireType *ran, int sta, ClaireFunction *f,
                                 ClaireFunction *f2)
{method *m = addMethod(dom,ran,sta,f);
  m->evaluate = f2;
  return m;}


// add an inlining definition
method *method::inlineDef(char *def)
{return inlineok_ask_method(this,def); }

// --------------------- API method -------------------------------------------

method *add_method_property(property *p, list *dom, ClaireType *r,
                   int status, OID f)
 { return p->addMethod(dom,r,status,
                       ((f == CNULL) ? NULL : OBJECT(ClaireFunction,f)));}

 
/*********************************************************************/
/**    4. Tables                                                     */
/*********************************************************************/


// expand the alist (hash table) and then creates the item
// the size is 2^n - 4 (even)
int table::expand(OID x)
{list *old = (list *) graph;
 int i, j = old->content[0];             // j = chunk size
 list *NEW = make_list_integer(2 * (*old)[0] - 4,CNULL);
   for (i=1; i < j - 4; i = i+2)
       if ((*old)[i] != CNULL) insertHash(NEW,(*old)[i],(*old)[i + 1]);
   graph = NEW;
   if (ClRes->cWorld != 0 && store_ask == CTRUE)      // new from 2.5
     STOREOBJ(((int *) &graph),(ClaireObject *) old);
   return index_table(this,x);}

// insert a new bucket in a alist without problem. This only works for
// buckets !
int table::insertHash (list *l, OID x, OID val)
{int i,chsize = l->content[0],end = chsize - 4,
     mask = (chsize >> 1) - 1, entry = (ClRes->hashOid(mask,x) << 1) + 1;
     for (i = entry; i < end; i = i + 2)
        if ((*l)[i] == CNULL) {(*l)[i] = x; (*l)[i+1] = val; return 1;}
     for (i = 1; i < entry; i = i + 2)
        if ((*l)[i] == CNULL) {(*l)[i] = x; (*l)[i + 1] = val; return 1;}
     return 1;}

// -------------------- API functions -----------------------------------

// add an item in an association list (of size 2^n - 3, full of unknown)
// uses a hash insertion in a list (use the cardinal to find is a new list
// must be allocated). Returns the position in the list
int index_table(table *a, OID x)
{list *l = (list *) a->graph;
 int i, chsize = (*l)[0], end = chsize - 4,
     mask = (chsize >> 1) - 1, entry = (ClRes->hashOid(mask,x) << 1) + 1;
   for (i = entry; i < end; i = i + 2)
        if (equal((*l)[i],x) == CTRUE) return i + 1;
        else if ((*l)[i] == CNULL)
          {ClaireClass *c = OWNER(a->DEFAULT);
           if ((i - entry) > (chsize >> 2)) return a->expand(x);
           (*l)[i] = x;
           if ((c == Kernel._set) || (c == Kernel._list))
                (*l)[i+1] = _oid_(copy_bag(OBJECT(bag,a->DEFAULT)));
           else (*l)[i+1] = a->DEFAULT;
           return (i + 1);}
   if ((i - entry) > (chsize >> 2)) return a->expand(x);
   if (entry > end) entry = end;    // <yc> 9/98 do not cross the bound
   for (i = 1; i < entry; i = i + 2)
       if (equal((*l)[i],x) == CTRUE) return i + 1;
       else if ((*l)[i] == CNULL)
          {ClaireClass *c = OWNER(a->DEFAULT);
           (*l)[i] = x;
           if ((c == Kernel._set) || (c == Kernel._list))   // v3.0.29 : copy
                (*l)[i+1] = _oid_(copy_bag(OBJECT(bag,a->DEFAULT)));
           else (*l)[i+1] = a->DEFAULT;
           return (i + 1);}
   return a->expand(x);}


// a nice version that takes two args and does not allocate the tuple.
// WARNING: this does not return the index but the value !!!!
//          it is thus simpler, because it is only used for reading
// v3.2.16: was completely false !!!!
int index_table2(table *a, OID x, OID y)
{list *l = (list *) a->graph;
 int i,chsize = (*l)[0], end = chsize - 4,
     mask = (chsize >> 1) - 1,
     hash2 = (ClRes->hashOid(mask,x) + ClRes->hashOid(mask,y)) & mask,
     entry = (hash2 << 1) + 1;
   for (i = entry; i < end; i = i + 2)
      {if ((*l)[i] == CNULL) return a->DEFAULT;
       else if (equal((*(OBJECT(list,(*l)[i])))[1],x) == CTRUE &&
                equal((*OBJECT(list,(*l)[i]))[2],y) == CTRUE) return (*l)[i + 1];}
   if (entry > end) entry = end;    // <yc> 9/98 do not cross the bound
   for (i = 1; i < entry; i = i + 2)
     {if ((*l)[i] == CNULL) return a->DEFAULT;
      else if (equal((*OBJECT(list,(*l)[i]))[1],x) == CTRUE &&
               equal((*OBJECT(list,(*l)[i]))[2],y) == CTRUE) return (*l)[i + 1];}
   return a->DEFAULT;}



