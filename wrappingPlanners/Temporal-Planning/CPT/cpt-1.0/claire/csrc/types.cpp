/***** CLAIRE Compilation of file c:\claire\v3.3\src\meta\types.cl 
         [version 3.3.4 / safety 5] Sat Oct 16 06:53:25 2004 *****/

#include <claire.h>
#include <Kernel.h>
#include <Core.h>

//+-------------------------------------------------------------+
//| CLAIRE                                                      |
//| types.cl                                                    |
//| Copyright (C) 1994 - 2003 Yves Caseau. All Rights Reserved  |
//| cf. copyright info in file object.cl: about()               |
//+-------------------------------------------------------------+
// --------------------------------------------------------------------
// This file contains the definition of the CLAIRE type system (a true lattice).
// that is used both at compile- and at run-time.
// --------------------------------------------------------------------
// ******************************************************************
// *  Table of contents                                             *
// *                                                                *
// *    Part 1: Common Set Methods                                  *
// *    Part 2: definition of the type operators                    *
// *    Part 3: Interface methods                                   *
// *    Part 4: Lattice methods                                     *
// *    Part 5: Type methods                                        *
// *                                                                *
// ******************************************************************
// *********************************************************************
// *   Part 1: Common Set Methods                                      *
// *********************************************************************
// ----------------------- useful methods ------------------------------
/* The c++ function for: finite?(self:type) [NEW_ALLOC] */
ClaireBoolean * finite_ask_type(ClaireType *self)
{ { ClaireBoolean *Result ;
    if (Kernel._set == self->isa)
     Result = CTRUE;
    else if (INHERIT(self->isa,Kernel._list))
     { OID  g0098UU;
      { ITERATE(t);
        g0098UU= _oid_(CFALSE);
        for (START(((bag *) self)); NEXT(t);)
        if ((*Core.finite_ask)(t) != Kernel.ctrue)
         { g0098UU = Kernel.ctrue;
          break;} 
        else ;} 
      Result = not_any(g0098UU);
      } 
    else if (INHERIT(self->isa,Kernel._class))
     { int  n = CLREAD(ClaireClass,self,open);
      Result = ((0 <= n) ? ((n <= 2) ? ((self != Kernel._any) ? CTRUE: CFALSE): CFALSE): CFALSE);
      } 
    else Result = CFALSE;
      return (Result);} 
  } 


// making a bag from an abstract_set
/* The c++ function for: enumerate(self:any) [NEW_ALLOC+RETURN_ARG] */
bag * enumerate_any(OID self)
{ { bag *Result ;
    { ClaireObject *V_CC ;
      if (INHERIT(OWNER(self),Kernel._bag))
       V_CC = OBJECT(bag,self);
      else if (Kernel._array == OWNER(self))
       V_CC = list_I_array(array_v(self));
      else if (INHERIT(OWNER(self),Kernel._integer))
       V_CC = make_set_integer(self);
      else if (INHERIT(OWNER(self),Kernel._collection))
       V_CC = OBJECT(bag,(*Kernel.set_I)(self));
      else close_exception(((general_error *) (*Core._general_error)(_string_("[178] cannot enumerate ~S"),
          _oid_(list::alloc(1,self)))));
        Result= (bag *) V_CC;} 
    return (Result);} 
  } 


// =type? is an operation (equality on types)
/* The c++ function for: =type?(self:type,ens:type) [NEW_ALLOC] */
ClaireBoolean * _equaltype_ask_any(ClaireType *self,ClaireType *ens)
{ return (((_inf_equal_type(self,ens) == CTRUE) ? ((_inf_equal_type(ens,self) == CTRUE) ? CTRUE: CFALSE): CFALSE));} 


// finds the sort associated to a type
/* The c++ function for: sort!(x:type) [NEW_ALLOC+RETURN_ARG] */
ClaireClass * sort_I_type(ClaireType *x)
{ { ClaireClass *Result ;
    Result = ((INHERIT(x->isa,Kernel._class)) ?
      sort_I_class(((ClaireClass *) x)) :
      sort_I_class(class_I_type(x)) );
    return (Result);} 
  } 


// the membership membership for lattice_sets
/* The c++ function for: %(self:any,ens:class) [0] */
ClaireBoolean * _Z_any1(OID self,ClaireClass *ens)
{ { ClaireBoolean *Result ;
    Result = ((INHERIT(OWNER(self),ens)) ?
      CTRUE :
      CFALSE );
    return (Result);} 
  } 


// an extension for %
/* The c++ function for: mClaire/%type(x:any,y:any) [NEW_ALLOC] */
ClaireBoolean * Ztype_any(OID x,OID y)
{ { ClaireBoolean *Result ;
    if ((INHERIT(OWNER(x),Kernel._type)) && 
        (Kernel._set == OWNER(y)))
     { OID  g0099UU;
      { ITERATE(z);
        g0099UU= _oid_(CFALSE);
        for (START(OBJECT(set,y)); NEXT(z);)
        if (_equaltype_ask_any(OBJECT(ClaireType,x),OBJECT(ClaireType,z)) == CTRUE)
         { g0099UU = Kernel.ctrue;
          break;} 
        } 
      Result = boolean_I_any(g0099UU);
      } 
    else Result = belong_to(x,y);
      return (Result);} 
  } 


// ****************************************************************
// *         Part 2: definition of the type operators             *
// ****************************************************************
// union of two types ---------------------------------------------
// Disjonctive Union Axiom (DU): Each union (A U B) is stricly disjunctive:
//       (1) A ^B = 0
//       (2) x < A U B <=> x < A or x < B
// Producing disjunction union is a form of normalization (the previous notion
// of diustributivity was a lousy bug)
// DU Axiom is necessary to make <= and ^ easier to define
// This is achieved in the U method
/* The c++ function for: self_print(self:Union) [NEW_ALLOC] */
void  self_print_Union_Core(Union *self)
{ GC_BIND;
  princ_string("(");
  print_any(GC_OID(_oid_(self->t1)));
  princ_string(" U ");
  print_any(GC_OID(_oid_(self->t2)));
  princ_string(")");
  GC_UNBIND;} 


/* The c++ function for: finite?(self:Union) [NEW_ALLOC] */
ClaireBoolean * finite_ask_Union(Union *self)
{ GC_BIND;
  { ClaireBoolean *Result ;
    Result = (((OBJECT(ClaireBoolean,(*Core.finite_ask)(GC_OID(_oid_(self->t1))))) == CTRUE) ? (((OBJECT(ClaireBoolean,(*Core.finite_ask)(GC_OID(_oid_(self->t2))))) == CTRUE) ? CTRUE: CFALSE): CFALSE);
    GC_UNBIND; return (Result);} 
  } 


// Intervals of integers ----------
/* The c++ function for: self_print(self:Interval) [NEW_ALLOC] */
void  self_print_Interval_Core(Interval *self)
{ princ_string("(");
  print_any(self->arg1);
  princ_string(" .. ");
  print_any(self->arg2);
  princ_string(")");
  } 


/* The c++ function for: finite?(self:Interval) [0] */
ClaireBoolean * finite_ask_Interval(Interval *self)
{ return (CTRUE);} 


// true constructor
/* The c++ function for: --(x:integer,y:integer) [NEW_ALLOC] */
Interval * _dash_dash_integer(int x,int y)
{ { Interval *Result ;
    { ClaireObject *V_CC ;
      if (x <= y)
       V_CC = _dot_dot_integer(x,y);
      else close_exception(((general_error *) (*Core._general_error)(_string_("[182] the interval (~S -- ~S) is empty"),
          _oid_(list::alloc(2,x,y)))));
        Result= (Interval *) V_CC;} 
    return (Result);} 
  } 


// Parameterized class. -------------------------------------------
/* The c++ function for: self_print(self:Param) [NEW_ALLOC+SLOT_UPDATE] */
void  self_print_Param_Core(Param *self)
{ GC_BIND;
  if ((self->params->length == 1) && 
      (((*(self->params))[1] == _oid_(Kernel.of)) && 
        (Kernel._set == OWNER((*(self->args))[1]))))
   { print_any(_oid_(self->arg));
    princ_string("<");
    print_any((*(OBJECT(set,(*(self->args))[1])))[1]);
    princ_string(">");
    } 
  else { print_any(_oid_(self->arg));
      princ_string("[");
      { int  i = 1;
        int  g0100 = self->args->length;
        { OID gc_local;
          while ((i <= g0100))
          { GC_LOOP;
            if (i != 1)
             princ_string(", ");
            print_any((*(self->params))[i]);
            princ_string(":(");
            print_any((*(self->args))[i]);
            princ_string(")");
            ++i;
            GC_UNLOOP;} 
          } 
        } 
      princ_string("]");
      } 
    GC_UNBIND;} 


/* The c++ function for: finite?(self:Param) [NEW_ALLOC] */
ClaireBoolean * finite_ask_Param(Param *self)
{ return (finite_ask_type(self->arg));} 


// subtype[X] ----------------------------------------------
// subtype[X] = {u in type | u <= t}
// for closure purposes, we add an arg Y -> Y inter st[X]
// Y can be any type class, but we forbid parametrisation on such classes !
// thus we can ensure that Y is a class
/* The c++ function for: self_print(self:subtype) [NEW_ALLOC] */
void  self_print_subtype_Core(subtype *self)
{ GC_BIND;
  if (self->arg == Kernel._type)
   { princ_string("subtype[");
    print_any(GC_OID(_oid_(self->t1)));
    princ_string("]");
    } 
  else { print_any(_oid_(self->arg));
      princ_string("[");
      print_any(GC_OID(_oid_(self->t1)));
      princ_string("]");
      } 
    GC_UNBIND;} 


// v3.2
/* The c++ function for: finite?(self:subtype) [NEW_ALLOC] */
ClaireBoolean * finite_ask_subtype(subtype *self)
{ GC_BIND;
  { ClaireBoolean *Result ;
    Result = ((self->arg == Kernel._set) ? (((OBJECT(ClaireBoolean,(*Core.finite_ask)(GC_OID(_oid_(self->t1))))) == CTRUE) ? CTRUE: CFALSE): CFALSE);
    GC_UNBIND; return (Result);} 
  } 


// creates a subtype, with some normalization
// v3.2 list[t] -> subtype 
/* The c++ function for: nth(self:class,x:type) [NEW_ALLOC] */
ClaireType * nth_class1(ClaireClass *self,ClaireType *x)
{ GC_BIND;
  { ClaireType *Result ;
    { ClaireObject *V_CC ;
      if ((self == Kernel._set) || 
          (self == Kernel._list))
       { subtype * _CL_obj = ((subtype *) GC_OBJECT(subtype,new_object_class(Core._subtype)));
        (_CL_obj->arg = self);
        (_CL_obj->t1 = x);
        V_CC = _CL_obj;
        } 
      else if (inherit_ask_class(self,Kernel._type) != CTRUE)
       close_exception(((general_error *) (*Core._general_error)(_string_("[177] subtyping of ~S not allowed"),
        _oid_(list::alloc(1,_oid_(self))))));
      else { subtype * _CL_obj = ((subtype *) GC_OBJECT(subtype,new_object_class(Core._subtype)));
          (_CL_obj->arg = ((self == Core._subtype) ?
            Kernel._type :
            self ));
          (_CL_obj->t1 = x);
          V_CC = _CL_obj;
          } 
        Result= (ClaireType *) V_CC;} 
    GC_UNBIND; return (Result);} 
  } 


// create a Param with a list of parameters (constant properties) l1 and a list
// of types l2
/* The c++ function for: nth(self:class,l1:list,l2:list) [NEW_ALLOC+SLOT_UPDATE+RETURN_ARG] */
ClaireType * nth_class2(ClaireClass *self,list *l1,list *l2)
{ GC_BIND;
  { ClaireType *Result ;
    { ClaireObject *V_CC ;
      if (((self == Kernel._list) || 
            (self == Kernel._set)) && 
          (INHERIT(OWNER((*(l2))[1]),Core._subtype)))
       V_CC = nth_class1(self,GC_OBJECT(ClaireType,OBJECT(subtype,(*(l2))[1])->t1));
      else if (((self == Kernel._list) || 
            (self == Kernel._set)) && 
          ((*(l1))[1] != _oid_(Kernel.of)))
       close_exception(((general_error *) (*Core._general_error)(_string_("[177] the subtyping expression ~S[~A] is not allowed"),
        _oid_(list::alloc(2,_oid_(self),_oid_(l1))))));
      else { Param * _CL_obj = ((Param *) GC_OBJECT(Param,new_object_class(Core._Param)));
          (_CL_obj->arg = self);
          (_CL_obj->params = l1);
          (_CL_obj->args = l2);
          V_CC = _CL_obj;
          } 
        Result= (ClaireType *) V_CC;} 
    GC_UNBIND; return (Result);} 
  } 


// create a Param of the stack[X] kind
/* The c++ function for: param!(self:class,tx:type) [NEW_ALLOC] */
ClaireType * param_I_class(ClaireClass *self,ClaireType *tx)
{ GC_BIND;
  { ClaireType *Result ;
    { Param * _CL_obj = ((Param *) GC_OBJECT(Param,new_object_class(Core._Param)));
      (_CL_obj->arg = self);
      (_CL_obj->params = list::alloc(1,_oid_(Kernel.of)));
      (_CL_obj->args = list::alloc(1,_oid_(set::alloc(1,_oid_(tx)))));
      Result = _CL_obj;
      } 
    GC_UNBIND; return (Result);} 
  } 


// create the t[] param
/* The c++ function for: nth(self:type) [NEW_ALLOC] */
ClaireType * nth_type(ClaireType *self)
{ GC_BIND;
  { ClaireType *Result ;
    { Param * _CL_obj = ((Param *) GC_OBJECT(Param,new_object_class(Core._Param)));
      (_CL_obj->arg = Kernel._array);
      (_CL_obj->params = list::alloc(1,_oid_(Kernel.of)));
      (_CL_obj->args = list::alloc(1,_oid_(set::alloc(1,_oid_(self)))));
      Result = _CL_obj;
      } 
    GC_UNBIND; return (Result);} 
  } 


// tuple are types
/* The c++ function for: finite?(self:tuple) [NEW_ALLOC] */
ClaireBoolean * finite_ask_tuple(tuple *self)
{ { ClaireBoolean *Result ;
    { OID  g0101UU;
      { ITERATE(x);
        g0101UU= _oid_(CFALSE);
        for (START(self); NEXT(x);)
        if ((*Core.finite_ask)(x) != Kernel.ctrue)
         { g0101UU = Kernel.ctrue;
          break;} 
        } 
      Result = not_any(g0101UU);
      } 
    return (Result);} 
  } 


// reference to a previous variable, not a type but a pattern -------
// index is the position of the stack of the referred type
// args is a list representing the path (a sequence of properties (parameters))
// a property is applied to the referred type
// if arg = true, the reference is the singleton containing the ref. value
// TODO check that arg is still used !
/* The c++ function for: self_print(self:Reference) [NEW_ALLOC] */
void  self_print_Reference_Core(Reference *self)
{ GC_BIND;
  princ_string("<ref:");
  print_any(GC_OID(_oid_(self->args)));
  princ_string("(ltype[");
  princ_integer(self->index);
  princ_string("])>");
  GC_UNBIND;} 


/* The c++ function for: get(self:Reference,y:any) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
OID  get_Reference(Reference *self,OID y)
{ GC_RESERVE(1);  // HOHO v3.0.55 optim !
  { OID Result = 0;
    { list * l = GC_OBJECT(list,self->args);
      { int  i = 1;
        int  g0102 = l->length;
        { OID gc_local;
          while ((i <= g0102))
          { GC_LOOP;
            GC__OID(y = funcall_property(OBJECT(property,(*(l))[i]),y), 1);
            ++i;
            GC_UNLOOP;} 
          } 
        } 
      Result = y;
      } 
    GC_UNBIND; return (Result);} 
  } 


// apply a reference to a type (l is args(self), passed for disambiguation)
/* The c++ function for: @(self:Reference,l:list,y:any) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
OID  _at_Reference(Reference *self,list *l,OID y)
{ GC_RESERVE(1);  // HOHO v3.0.55 optim !
  ;{ int  i = 1;
    int  g0103 = l->length;
    { OID gc_local;
      while ((i <= g0103))
      { GC_LOOP;
        GC__OID(y = _oid_(_at_type(OBJECT(ClaireType,y),OBJECT(property,(*(l))[i]))), 1);
        ++i;
        GC_UNLOOP;} 
      } 
    } 
  { OID Result = 0;
    Result = y;
    GC_UNBIND; return (Result);} 
  } 


// type to set coercion  -------------------------------------------------
// new in v3.0.5 = use an interface method for type enumeration
// the default strategy is extensible: we look if there exists
// a proper definition that could be interpreted !
/* The c++ function for: set!(x:collection) [NEW_ALLOC] */
set * set_I_collection(ClaireCollection *x)
{ GC_BIND;
  { set *Result ;
    { ClaireObject *V_CC ;
      { OID  m = GC_OID(_oid_(_at_property1(Kernel.set_I,x->isa)));
        if (domain_I_restriction(OBJECT(restriction,m)) != Kernel._collection)
         V_CC = OBJECT(set,(*Kernel.funcall)(m,
          _oid_(x)));
        else close_exception(((general_error *) (*Core._general_error)(_string_("[178] cannot enumerate ~S"),
            _oid_(list::alloc(1,_oid_(x))))));
          } 
      Result= (set *) V_CC;} 
    GC_UNBIND; return (Result);} 
  } 


/* The c++ function for: size(x:collection) [NEW_ALLOC] */
int  size_collection(ClaireCollection *x)
{ GC_BIND;
  { int Result = 0;
    { OID  m = GC_OID(_oid_(_at_property1(Core.size,x->isa)));
      Result = ((domain_I_restriction(OBJECT(restriction,m)) != Kernel._collection) ?
        (*Kernel.funcall)(m,
          _oid_(x)) :
        length_bag(OBJECT(bag,(*Kernel.set_I)(_oid_(x)))) );
      } 
    GC_UNBIND; return (Result);} 
  } 


// v3.2.34  -> makes the API simpler
// set is needed for recursive def
/* The c++ function for: set!(x:set) [RETURN_ARG] */
set * set_I_set(set *x)
{ return (x);} 


/* The c++ function for: size(x:set) [0] */
int  size_set(set *x)
{ return (x->length);} 


// set is needed for recursive def
/* The c++ function for: size(x:list) [NEW_ALLOC] */
int  size_list2_Core(list *x)
{ return (length_bag(set_I_bag(x)));} 


// class  -> return a read-only list  (v3.2)
/* The c++ function for: set!(x:class) [NEW_ALLOC+RETURN_ARG] */
set * set_I_class(ClaireClass *x)
{ GC_RESERVE(1);  // HOHO v3.0.55 optim !
  { set *Result ;
    { list * rep = list::empty();
      { OID gc_local;
        ITERATE(c);
        for (START(x->descendents); NEXT(c);)
        { GC_LOOP;
          if ((INHERIT(OBJECT(ClaireClass,c),Kernel._primitive)) && 
              (c != _oid_(Kernel._boolean)))
           close_exception(((general_error *) (*Core._general_error)(_string_("[178] cannot enumerate ~S"),
            _oid_(list::alloc(1,c)))));
          else GC__ANY(rep = append_list(rep,OBJECT(ClaireClass,c)->instances), 1);
            GC_UNLOOP;} 
        } 
      Result = set_I_bag(rep);
      } 
    GC_UNBIND; return (Result);} 
  } 


/* The c++ function for: size(self:class) [SAFE_RESULT] */
int  size_class(ClaireClass *self)
{ { int Result = 0;
    { int  n = 0;
      { ITERATE(x);
        for (START(self->descendents); NEXT(x);)
        n= (n+OBJECT(ClaireClass,x)->instances->length);
        } 
      Result = n;
      } 
    return (Result);} 
  } 


// Union
/* The c++ function for: set!(x:Union) [NEW_ALLOC] */
set * set_I_Union(Union *x)
{ GC_BIND;
  { set *Result ;
    Result = append_set(GC_OBJECT(set,OBJECT(set,(*Kernel.set_I)(GC_OID(_oid_(x->t1))))),GC_OBJECT(set,OBJECT(set,(*Kernel.set_I)(GC_OID(_oid_(x->t2))))));
    GC_UNBIND; return (Result);} 
  } 


/* The c++ function for: size(x:Union) [NEW_ALLOC] */
int  size_Union(Union *x)
{ GC_BIND;
  { int Result = 0;
    Result = (((INHERIT(x->t1->isa,Core._Interval)) || 
        (Kernel._set == x->t1->isa)) ?
      ((Core.size->fcall(((int) OBJECT(ClaireObject,GC_OID(_oid_(x->t1))))))+(Core.size->fcall(((int) OBJECT(ClaireObject,GC_OID(_oid_(x->t2))))))) :
      length_bag(set_I_Union(x)) );
    GC_UNBIND; return (Result);} 
  } 


// interval
/* The c++ function for: set!(x:Interval) [NEW_ALLOC] */
set * set_I_Interval(Interval *x)
{ return (sequence_integer(x->arg1,x->arg2));} 


/* The c++ function for: size(self:Interval) [0] */
int  size_Interval(Interval *self)
{ return (((self->arg2+1)-self->arg1));} 


// param
/* The c++ function for: set!(x:Param) [NEW_ALLOC] */
set * set_I_Param(Param *x)
{ GC_BIND;
  { set *Result ;
    { bag * y_in = set_I_class(x->arg);
      set * y_out = ((set *) empty_bag(y_in));
      { ITERATE(y);
        for (START(y_in); NEXT(y);)
        if (belong_to(y,_oid_(x)) == CTRUE)
         y_out->addFast(y);
        } 
      Result = GC_OBJECT(set,y_out);
      } 
    GC_UNBIND; return (Result);} 
  } 


/* The c++ function for: size(x:Param) [NEW_ALLOC] */
int  size_Param(Param *x)
{ return (length_bag(set_I_Param(x)));} 


// subtype
/* The c++ function for: set!(x:subtype) [NEW_ALLOC+BAG_UPDATE] */
set * set_I_subtype(subtype *x)
{ GC_BIND;
  { set *Result ;
    { ClaireObject *V_CC ;
      if (x->arg == Kernel._set)
       V_CC = build_powerset_list(GC_OBJECT(list,list_I_set(GC_OBJECT(set,OBJECT(set,(*Kernel.set_I)(GC_OID(_oid_(x->t1))))))));
      else close_exception(((general_error *) (*Core._general_error)(_string_("[178] cannot enumerate ~S"),
          _oid_(list::alloc(1,_oid_(x))))));
        Result= (set *) V_CC;} 
    GC_UNBIND; return (Result);} 
  } 


/* The c++ function for: size(x:subtype) [NEW_ALLOC] */
int  size_subtype(subtype *x)
{ GC_BIND;
  { int Result = 0;
    if (x->arg == Kernel._set)
     Result = exp2_integer(Core.size->fcall(((int) OBJECT(ClaireObject,GC_OID(_oid_(x->t1))))));
    else close_exception(((general_error *) (*Core._general_error)(_string_("[178] cannot enumerate ~S"),
        _oid_(list::alloc(1,_oid_(x))))));
      GC_UNBIND; return (Result);} 
  } 


// tuple
/* The c++ function for: set!(x:tuple) [NEW_ALLOC] */
set * set_I_tuple(tuple *x)
{ GC_RESERVE(10);  // v3.0.55 optim !
  { set *Result ;
    { list * l = ((list *) x);
      if (boolean_I_any(_oid_(l)) != CTRUE)
       Result = set::alloc(Kernel.emptySet,1,_oid_(Kernel.emptySet));
      else { set * l1;
          { { set * y_bag = set::empty(Kernel.emptySet);
              { OID gc_local;
                ITERATE(y);
                bag *y_support;
                y_support = GC_OBJECT(set,OBJECT(bag,(*Kernel.set_I)((*(l))[1])));
                for (START(y_support); NEXT(y);)
                y_bag->addFast(_oid_(list::alloc(1,y)));
                } 
              l1 = GC_OBJECT(set,y_bag);
              } 
            GC_OBJECT(set,l1);} 
          { int  n = 2;
            int  g0104 = l->length;
            { OID gc_local;
              while ((n <= g0104))
              { GC_LOOP;
                { set * l2 = GC_OBJECT(set,set::empty(Kernel._any));
                  { OID gc_local;
                    ITERATE(z);
                    bag *z_support;
                    z_support = GC_OBJECT(set,OBJECT(bag,(*Kernel.set_I)((*(l))[n])));
                    for (START(z_support); NEXT(z);)
                    { OID gc_local;
                      ITERATE(l3);
                      for (START(l1); NEXT(l3);)
                      l2= l2->addFast(_oid_(((list *) copy_bag(OBJECT(bag,l3)))->addFast(z)));
                      } 
                    } 
                  GC__ANY(l1 = l2, 4);
                  } 
                ++n;
                GC_UNLOOP;} 
              } 
            } 
          Result = l1;
          } 
        } 
    GC_UNBIND; return (Result);} 
  } 


/* The c++ function for: size(l:tuple) [NEW_ALLOC] */
int  size_tuple(tuple *l)
{ { int Result = 0;
    if (boolean_I_any(_oid_(l)) != CTRUE)
     Result = 1;
    else { int  m = (*Core.size)((*(l))[1]);
        { int  n = 2;
          int  g0105 = l->length;
          { OID gc_local;
            while ((n <= g0105))
            { m= (m*((*Core.size)((*(l))[n])));
              ++n;
              } 
            } 
          } 
        Result = m;
        } 
      return (Result);} 
  } 


// generic collection membership
// v3.2.24: this is extensible through the redefinition of %
/* The c++ function for: member?(x:any,y:type) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
ClaireBoolean * member_ask_any(OID x,ClaireType *y)
{ GC_BIND;
  { ClaireBoolean *Result ;
    { ClaireObject *V_CC ;
      if (INHERIT(y->isa,Core._Union))
       V_CC = ((member_ask_any(x,GC_OBJECT(ClaireType,CLREAD(Union,y,t1))) == CTRUE) ? CTRUE : ((member_ask_any(x,GC_OBJECT(ClaireType,CLREAD(Union,y,t2))) == CTRUE) ? CTRUE : CFALSE));
      else if (INHERIT(y->isa,Core._Interval))
       { V_CC = ((INHERIT(OWNER(x),Kernel._integer)) ?
          ((CLREAD(Interval,y,arg1) <= x) ? ((x <= CLREAD(Interval,y,arg2)) ? CTRUE: CFALSE): CFALSE) :
          CFALSE );
        } 
      else if (INHERIT(y->isa,Core._Param))
       { int  n = 1;
        list * l = GC_OBJECT(list,CLREAD(Param,y,args));
        { ClaireBoolean *v_and;
          { v_and = _Z_any1(x,CLREAD(Param,y,arg));
            if (v_and == CFALSE) V_CC =CFALSE; 
            else { { OID  g0107UU;
                { ITERATE(p);
                  g0107UU= _oid_(CFALSE);
                  bag *p_support;
                  p_support = GC_OBJECT(list,CLREAD(Param,y,params));
                  for (START(p_support); NEXT(p);)
                  if (Ztype_any(GC_OID(funcall_property(OBJECT(property,p),x)),(*(l))[n]) != CTRUE)
                   { g0107UU = Kernel.ctrue;
                    break;} 
                  else ++n;
                    } 
                v_and = not_any(g0107UU);
                } 
              if (v_and == CFALSE) V_CC =CFALSE; 
              else V_CC = CTRUE;} 
            } 
          } 
        } 
      else if (INHERIT(y->isa,Core._subtype))
       V_CC = (((CLREAD(subtype,y,arg) == Core._subtype) ? (INHERIT(OWNER(x),Kernel._type)) : (_Z_any1(x,CLREAD(subtype,y,arg)) == CTRUE)) ? (((OBJECT(ClaireBoolean,(*Core._inf_equalt)(x,
        GC_OID(_oid_(CLREAD(subtype,y,t1)))))) == CTRUE) ? CTRUE: CFALSE): CFALSE);
      else if (INHERIT(y->isa,Kernel._tuple))
       { int  n = ((bag *) y)->length;
        if (INHERIT(OWNER(x),Kernel._tuple))
         { ClaireBoolean *v_and;
          { v_and = ((OBJECT(bag,x)->length == n) ? CTRUE : CFALSE);
            if (v_and == CFALSE) V_CC =CFALSE; 
            else { { OID  g0108UU;
                { int  i = 1;
                  int  g0106 = n;
                  { g0108UU= _oid_(CFALSE);
                    while ((i <= g0106))
                    { if (belong_to((*(OBJECT(bag,x)))[i],(*(((bag *) y)))[i]) != CTRUE)
                       { g0108UU = Kernel.ctrue;
                        break;} 
                      ++i;
                      } 
                    } 
                  } 
                v_and = not_any(g0108UU);
                } 
              if (v_and == CFALSE) V_CC =CFALSE; 
              else V_CC = CTRUE;} 
            } 
          } 
        else V_CC = CFALSE;
          } 
      else if (INHERIT(y->isa,Kernel._bag))
       V_CC = belong_to(x,_oid_(y));
      else if (INHERIT(y->isa,Kernel._class))
       V_CC = _Z_any1(x,((ClaireClass *) y));
      else if (INHERIT(y->isa,Core._Reference))
       V_CC = CTRUE;
      else { int  start = ClEnv->index;
          PUSH(x);
          PUSH(_oid_(y));
          { ClaireObject * m = find_which_property(Kernel._Z,start,OWNER(x));
            if ((Kernel._method == m->isa) && ((CLREAD(restriction,m,domain)->length == 2) && 
                ((*(CLREAD(restriction,m,domain)))[2] != _oid_(Kernel._any))))
             V_CC = OBJECT(ClaireBoolean,eval_message_property(Kernel._Z,m,start,CTRUE));
            else close_exception(((general_error *) (*Core._general_error)(_string_("[179] (~S % ~S): not implemented!"),
                _oid_(list::alloc(2,x,_oid_(y))))));
              } 
          } 
        Result= (ClaireBoolean *) V_CC;} 
    GC_UNBIND; return (Result);} 
  } 


// the best class approximation
/* The c++ function for: class!(x:type) [NEW_ALLOC+RETURN_ARG] */
ClaireClass * class_I_type(ClaireType *x)
{ GC_BIND;
  { ClaireClass *Result ;
    { ClaireType *V_CC ;
      if (INHERIT(x->isa,Kernel._class))
       V_CC = x;
      else if (Kernel._set == x->isa)
       { if (((bag *) x)->length == 0)
         V_CC = Kernel._void;
        else { ClaireClass * rep = OWNER((*(((bag *) x)))[1]);
            { ITERATE(y);
              for (START(((bag *) x)); NEXT(y);)
              rep= meet_class(rep,OWNER(y));
              } 
            V_CC = rep;
            } 
          } 
      else if (INHERIT(x->isa,Core._Union))
       V_CC = meet_class(class_I_type(CLREAD(Union,x,t1)),class_I_type(CLREAD(Union,x,t2)));
      else if (INHERIT(x->isa,Core._Interval))
       V_CC = Kernel._integer;
      else V_CC = ((INHERIT(x->isa,Core._subtype)) ?
        ((CLREAD(subtype,x,arg) == Core._subtype) ?
          Kernel._any :
          CLREAD(subtype,x,arg) ) :
        ((INHERIT(x->isa,Core._Param)) ?
          CLREAD(Param,x,arg) :
          ((INHERIT(x->isa,Core._Reference)) ?
            Kernel._any :
            ((INHERIT(x->isa,Kernel._tuple)) ?
              Kernel._tuple :
              Kernel._any ) ) ) );
      Result= (ClaireClass *) V_CC;} 
    GC_UNBIND; return (Result);} 
  } 


// declarations
// ********************************************************************
// *                Part 3: Interface Methods                         *
// ********************************************************************
// there is a special restriction for + to specify the way the inheritance
// conflict should be solved
//U(self:set,ens:type) : type -> (case ens (set self /+ ens, any ens U self))
// the union makes a partial reduction to the normal form. The complete
// reduction is done by enumeration if needed during the type subsumption
// union is left-associative: A U B U C is represented by (A U B) U C  => never(t2(x:Union) % union)
// a union of intervals is ALWAYS disjoint
/* The c++ function for: U(x:type,y:type) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
ClaireType * U_type(ClaireType *x,ClaireType *y)
{ GC_BIND;
  { ClaireType *Result ;
    if (Kernel._set == x->isa)
     { if (Kernel._set == y->isa)
       Result = append_set(((set *) x),((set *) y));
      else Result = U_type(y,x);
        } 
    else if ((OBJECT(ClaireBoolean,_oid_((ClaireObject *) Core._inf_equalt->fcall(((int) y),((int) x))))) == CTRUE)
     Result = x;
    else if ((OBJECT(ClaireBoolean,_oid_((ClaireObject *) Core._inf_equalt->fcall(((int) x),((int) y))))) == CTRUE)
     Result = y;
    else if (INHERIT(y->isa,Core._Union))
     Result = U_type(GC_OBJECT(ClaireType,U_type(x,GC_OBJECT(ClaireType,OBJECT(ClaireType,(*Core.t1)(_oid_(y)))))),GC_OBJECT(ClaireType,CLREAD(Union,y,t2)));
    else if ((INHERIT(x->isa,Core._Interval)) && (INHERIT(y->isa,Core._Interval)))
     { if (((CLREAD(Interval,y,arg1)-1) <= CLREAD(Interval,x,arg2)) && 
          (CLREAD(Interval,x,arg1) <= CLREAD(Interval,y,arg1)))
       Result = _dot_dot_integer(CLREAD(Interval,x,arg1),CLREAD(Interval,y,arg2));
      else if (((CLREAD(Interval,x,arg1)-1) <= CLREAD(Interval,y,arg2)) && 
          (CLREAD(Interval,y,arg1) <= CLREAD(Interval,x,arg1)))
       Result = _dot_dot_integer(CLREAD(Interval,y,arg1),CLREAD(Interval,x,arg2));
      else { Union * _CL_obj = ((Union *) GC_OBJECT(Union,new_object_class(Core._Union)));
          (_CL_obj->t1 = x);
          (_CL_obj->t2 = y);
          Result = _CL_obj;
          } 
        } 
    else if ((INHERIT(x->isa,Core._Union)) && (INHERIT(y->isa,Core._Interval)))
     { ClaireType * z = GC_OBJECT(ClaireType,U_type(GC_OBJECT(ClaireType,CLREAD(Union,x,t2)),y));
      if (INHERIT(z->isa,Core._Union))
       { Union * _CL_obj = ((Union *) GC_OBJECT(Union,new_object_class(Core._Union)));
        (_CL_obj->t1 = U_type(GC_OBJECT(ClaireType,OBJECT(ClaireType,(*Core.t1)(_oid_(x)))),y));
        (_CL_obj->t2 = CLREAD(Union,x,t2));
        Result = _CL_obj;
        } 
      else Result = U_type(GC_OBJECT(ClaireType,OBJECT(ClaireType,(*Core.t1)(_oid_(x)))),z);
        } 
    else if ((INHERIT(x->isa,Core._Interval)) && ((Kernel._set == y->isa) && 
        ((belong_to((CLREAD(Interval,x,arg1)-1),_oid_(y)) == CTRUE) || 
            (belong_to((CLREAD(Interval,x,arg2)+1),_oid_(y)) == CTRUE))))
     { int  a = CLREAD(Interval,x,arg1);
      int  b = CLREAD(Interval,x,arg2);
      if (belong_to((a-1),_oid_(y)) == CTRUE)
       a= (a-1);
      if (belong_to((b+1),_oid_(y)) == CTRUE)
       ++b;
      Result = U_type(GC_OBJECT(ClaireType,_dot_dot_integer(a,b)),y);
      } 
    else { if (Kernel._set == y->isa)
         { { set * z_out = set::empty(Kernel.emptySet);
            { OID gc_local;
              ITERATE(z);
              bag *z_support;
              z_support = GC_OBJECT(bag,enumerate_any(_oid_(y)));
              for (START(z_support); NEXT(z);)
              if (belong_to(z,_oid_(x)) != CTRUE)
               z_out->addFast(z);
              } 
            y = GC_OBJECT(set,z_out);
            } 
          GC_OBJECT(set,y);} 
        { Union * _CL_obj = ((Union *) GC_OBJECT(Union,new_object_class(Core._Union)));
          (_CL_obj->t1 = x);
          (_CL_obj->t2 = y);
          Result = _CL_obj;
          } 
        } 
      GC_UNBIND; return (Result);} 
  } 


// the Interval construction method has a smart second-order type  - fix on v3.1.06
/* The c++ function for: ..(x:integer,y:integer) [NEW_ALLOC] */
ClaireType * _dot_dot_integer(int x,int y)
{ { ClaireType *Result ;
    if (x <= y)
     Result = ((Interval *) (*Core._Interval)(x,
      y));
    else Result = Kernel.emptySet;
      return (Result);} 
  } 


/* The c++ function for: .._integer_type */
ClaireType * _dot_dot_integer_type(ClaireType *x,ClaireType *y)
{ GC_BIND;
  { ClaireType *Result ;
    if ((unique_ask_type(x) == CTRUE) && 
        ((unique_ask_type(y) == CTRUE) && 
          ((OBJECT(ClaireBoolean,(*Kernel._inf_equal)(GC_OID(the_type(x)),
            GC_OID(the_type(y))))) == CTRUE)))
     Result = set::alloc(1,GC_OID(_oid_(_dot_dot_integer(the_type(x),the_type(y)))));
    else Result = nth_class1(Core._subtype,Kernel._integer);
      GC_UNBIND; return (Result);} 
  } 


// exception
/* The c++ function for: but(s:any,x:any) [NEW_ALLOC] */
bag * but_any(OID s,OID x)
{ GC_BIND;
  { bag *Result ;
    if (INHERIT(OWNER(s),Kernel._list))
     { bag * y_in = OBJECT(bag,s);
      list * y_out = ((list *) empty_bag(y_in));
      { ITERATE(y);
        for (START(y_in); NEXT(y);)
        if (equal(y,x) != CTRUE)
         y_out->addFast(y);
        } 
      Result = GC_OBJECT(list,y_out);
      } 
    else Result = ((Kernel._set == OWNER(s)) ?
      delete_bag(copy_bag(OBJECT(bag,s)),x) :
      delete_bag(OBJECT(bag,(*Kernel.set_I)(s)),x) );
    GC_UNBIND; return (Result);} 
  } 


/* The c++ function for: but_any_type */
ClaireType * but_any_type(ClaireType *s,ClaireType *x)
{ GC_BIND;
  { ClaireType *Result ;
    Result = nth_class1(Kernel._bag,GC_OBJECT(ClaireType,member_type(s)));
    GC_UNBIND; return (Result);} 
  } 


/* The c++ function for: \(x:type,y:type) [NEW_ALLOC] */
set * _backslash_type(ClaireType *x,ClaireType *y)
{ GC_BIND;
  { set *Result ;
    { set * z_out = set::empty(Kernel.emptySet);
      { OID gc_local;
        ITERATE(z);
        bag *z_support;
        z_support = GC_OBJECT(bag,enumerate_any(_oid_(x)));
        for (START(z_support); NEXT(z);)
        if (belong_to(z,_oid_(y)) != CTRUE)
         z_out->addFast(z);
        } 
      Result = GC_OBJECT(set,z_out);
      } 
    GC_UNBIND; return (Result);} 
  } 


// ******************************************************************
// *    Part 4: Lattice methods                                     *
// ******************************************************************
// glb operation ---------------------------------------------------
// should use type
// new in v3.0.60: we reintroduce a glb method
/* The c++ function for: glb(x:set,y:type) [NEW_ALLOC] */
set * glb_set(set *x,ClaireType *y)
{ GC_BIND;
  { set *Result ;
    { bag * z_in = x;
      set * z_out = ((set *) empty_bag(z_in));
      { ITERATE(z);
        for (START(z_in); NEXT(z);)
        if (belong_to(z,_oid_(y)) == CTRUE)
         z_out->addFast(z);
        } 
      Result = GC_OBJECT(set,z_out);
      } 
    GC_UNBIND; return (Result);} 
  } 


/* The c++ function for: glb(x:Union,y:type) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
ClaireType * glb_Union(Union *x,ClaireType *y)
{ GC_BIND;
  { ClaireType *Result ;
    Result = U_type(GC_OBJECT(ClaireType,OBJECT(ClaireType,(*Core.glb)(GC_OID(_oid_(x->t1)),
      _oid_(y)))),GC_OBJECT(ClaireType,OBJECT(ClaireType,(*Core.glb)(GC_OID(_oid_(x->t2)),
      _oid_(y)))));
    GC_UNBIND; return (Result);} 
  } 


/* The c++ function for: glb(x:Interval,y:type) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
ClaireType * glb_Interval(Interval *x,ClaireType *y)
{ GC_BIND;
  { ClaireType *Result ;
    if (INHERIT(y->isa,Kernel._class))
     { if (_inf_equalt_class(Kernel._integer,y) == CTRUE)
       Result = x;
      else Result = Kernel.emptySet;
        } 
    else if (Kernel._set == y->isa)
     Result = glb_set(((set *) y),x);
    else if (INHERIT(y->isa,Core._Interval))
     { if (x->arg1 <= CLREAD(Interval,y,arg1))
       { if (CLREAD(Interval,y,arg1) <= x->arg2)
         { Result = ((x->arg2 <= CLREAD(Interval,y,arg2)) ?
            _dot_dot_integer(CLREAD(Interval,y,arg1),x->arg2) :
            y );
          } 
        else Result = Kernel.emptySet;
          } 
      else Result = glb_Interval(((Interval *) y),x);
        } 
    else if (INHERIT(y->isa,Core._Union))
     Result = U_type(GC_OBJECT(ClaireType,glb_Interval(x,GC_OBJECT(ClaireType,CLREAD(Union,y,t1)))),GC_OBJECT(ClaireType,glb_Interval(x,GC_OBJECT(ClaireType,CLREAD(Union,y,t2)))));
    else Result = Kernel.emptySet;
      GC_UNBIND; return (Result);} 
  } 


/* The c++ function for: glb(x:class,y:type) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
ClaireType * glb_class(ClaireClass *x,ClaireType *y)
{ GC_BIND;
  { ClaireType *Result ;
    if ((x->open == ClEnv->ABSTRACT) && 
        (boolean_I_any(_oid_(x->subclass)) != CTRUE))
     { set * z_out = set::empty(Kernel.emptySet);
      { OID gc_local;
        ITERATE(z);
        bag *z_support;
        z_support = GC_OBJECT(bag,enumerate_any(_oid_(x)));
        for (START(z_support); NEXT(z);)
        if (belong_to(z,_oid_(y)) == CTRUE)
         z_out->addFast(z);
        } 
      Result = GC_OBJECT(set,z_out);
      } 
    else if ((x->open == ClEnv->ABSTRACT) && 
        (boolean_I_any(_oid_(x->instances)) != CTRUE))
     { list * g0109UU;
      { { bag *v_list; OID v_val;
          OID z,CLcount;
          v_list = x->subclass;
           g0109UU = v_list->clone();
          for (CLcount= 1; CLcount <= v_list->length; CLcount++)
          { z = (*(v_list))[CLcount];
            v_val = (*Core.glb)(z,
              _oid_(y));
            
            (*((list *) g0109UU))[CLcount] = v_val;} 
          } 
        GC_OBJECT(list,g0109UU);} 
      Result = Uall_list(g0109UU);
      } 
    else Result = ((INHERIT(y->isa,Kernel._class)) ?
      join_class(x,((ClaireClass *) y)) :
      OBJECT(ClaireType,(*Core.glb)(_oid_(y),
        _oid_(x))) );
    GC_UNBIND; return (Result);} 
  } 


/* The c++ function for: glb(x:Param,y:type) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
ClaireType * glb_Param(Param *x,ClaireType *y)
{ GC_RESERVE(9);  // v3.0.55 optim !
  { ClaireType *Result ;
    if (INHERIT(y->isa,Core._Param))
     { ClaireType * c = join_class(x->arg,CLREAD(Param,y,arg));
      list * lp = GC_OBJECT(list,list_I_set(GC_OBJECT(set,set_I_bag(GC_OBJECT(list,append_list(GC_OBJECT(list,x->params),GC_OBJECT(list,CLREAD(Param,y,params))))))));
      list * l = list::empty(Kernel._any);
      { OID gc_local;
        ITERATE(p);
        for (START(lp); NEXT(p);)
        { GC_LOOP;
          { ClaireType * t = GC_OBJECT(ClaireType,OBJECT(ClaireType,(*Core.glb)(GC_OID(_oid_(_at_type(x,OBJECT(property,p)))),
              GC_OID(_oid_(_at_type(y,OBJECT(property,p)))))));
            if (equal(_oid_(t),_oid_(Kernel.emptySet)) != CTRUE)
             GC__ANY(l = l->addFast(_oid_(t)), 6);
            else { c= Kernel.emptySet;
                { ;break;} 
                } 
              } 
          GC_UNLOOP;} 
        } 
      if (equal(_oid_(c),_oid_(Kernel.emptySet)) != CTRUE)
       { Param * _CL_obj = ((Param *) GC_OBJECT(Param,new_object_class(Core._Param)));
        (_CL_obj->arg = ((ClaireClass *) c));
        (_CL_obj->params = lp);
        (_CL_obj->args = l);
        Result = _CL_obj;
        } 
      else Result = Kernel.emptySet;
        } 
    else if (INHERIT(y->isa,Kernel._class))
     { ClaireType * c = join_class(x->arg,((ClaireClass *) y));
      if (equal(_oid_(c),_oid_(Kernel.emptySet)) != CTRUE)
       { Param * _CL_obj = ((Param *) GC_OBJECT(Param,new_object_class(Core._Param)));
        (_CL_obj->arg = ((ClaireClass *) c));
        (_CL_obj->params = x->params);
        (_CL_obj->args = x->args);
        Result = _CL_obj;
        } 
      else Result = Kernel.emptySet;
        } 
    else Result = OBJECT(ClaireType,(*Core.glb)(_oid_(y),
        _oid_(x)));
      GC_UNBIND; return (Result);} 
  } 


// notice that a param whose class is a type must use of (only parameter allowed!)
// the result is a subtype
/* The c++ function for: glb(x:subtype,y:type) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
ClaireType * glb_subtype(subtype *x,ClaireType *y)
{ GC_BIND;
  { ClaireType *Result ;
    if (INHERIT(y->isa,Kernel._class))
     { if (equal(_oid_(join_class(x->arg,((ClaireClass *) y))),_oid_(Kernel.emptySet)) != CTRUE)
       Result = nth_class1(((ClaireClass *) join_class(x->arg,((ClaireClass *) y))),GC_OBJECT(ClaireType,x->t1));
      else Result = Kernel.emptySet;
        } 
    else if (INHERIT(y->isa,Core._Param))
     { if (equal(_oid_(join_class(x->arg,CLREAD(Param,y,arg))),_oid_(Kernel.emptySet)) != CTRUE)
       Result = param_I_class(((ClaireClass *) join_class(x->arg,CLREAD(Param,y,arg))),GC_OBJECT(ClaireType,OBJECT(ClaireType,(*Core.glb)(GC_OID(_oid_(member_type(x))),
        GC_OID(_oid_(member_type(y)))))));
      else Result = Kernel.emptySet;
        } 
    else if (INHERIT(y->isa,Core._subtype))
     { if (equal(_oid_(join_class(x->arg,CLREAD(subtype,y,arg))),_oid_(Kernel.emptySet)) != CTRUE)
       { OID  t = GC_OID((*Core.glb)(GC_OID(_oid_(x->t1)),
          GC_OID(_oid_(CLREAD(subtype,y,t1)))));
        if (equal(t,_oid_(Kernel.emptySet)) != CTRUE)
         Result = nth_class1(((ClaireClass *) join_class(x->arg,CLREAD(subtype,y,arg))),OBJECT(ClaireType,t));
        else Result = Kernel.emptySet;
          } 
      else Result = Kernel.emptySet;
        } 
    else Result = OBJECT(ClaireType,(*Core.glb)(_oid_(y),
        _oid_(x)));
      GC_UNBIND; return (Result);} 
  } 


// set, Interval, list
/* The c++ function for: glb(x:tuple,y:type) [NEW_ALLOC+RETURN_ARG] */
ClaireType * glb_tuple(tuple *x,ClaireType *y)
{ GC_BIND;
  { ClaireType *Result ;
    if (INHERIT(y->isa,Kernel._class))
     { if (INHERIT(Kernel._tuple,((ClaireClass *) y)))
       Result = x;
      else Result = Kernel.emptySet;
        } 
    else if (INHERIT(y->isa,Core._Param))
     Result = Kernel.emptySet;
    else if (INHERIT(y->isa,Kernel._tuple))
     Result = tuple_I_list(GC_OBJECT(list,_exp_list(((list *) x),((list *) y))));
    else if (INHERIT(y->isa,Core._subtype))
     { if (CLREAD(subtype,y,arg) == Kernel._tuple)
       { list * g0110UU;
        { { bag *v_list; OID v_val;
            OID z,CLcount;
            v_list = x;
             g0110UU = v_list->clone();
            for (CLcount= 1; CLcount <= v_list->length; CLcount++)
            { z = (*(v_list))[CLcount];
              v_val = (*Core.glb)(z,
                GC_OID(_oid_(CLREAD(subtype,y,t1))));
              
              (*((list *) g0110UU))[CLcount] = v_val;} 
            } 
          GC_OBJECT(list,g0110UU);} 
        Result = tuple_I_list(g0110UU);
        } 
      else Result = Kernel.emptySet;
        } 
    else Result = OBJECT(ClaireType,(*Core.glb)(_oid_(y),
        _oid_(x)));
      GC_UNBIND; return (Result);} 
  } 


// a reference is seen as "any"
/* The c++ function for: glb(x:Reference,y:type) [RETURN_ARG] */
ClaireType * glb_Reference(Reference *x,ClaireType *y)
{ return (y);} 


// this will be greatly simplified in a few minutes !
/* The c++ function for: ^(x:type,y:type) [NEW_ALLOC+RETURN_ARG] */
ClaireType * _exp_type(ClaireType *x,ClaireType *y)
{ return (OBJECT(ClaireType,(*Core.glb)(_oid_(x),
    _oid_(y))));} 


// the old lattice_glb
/* The c++ function for: join(x:class,y:class) [0] */
ClaireType * join_class(ClaireClass *x,ClaireClass *y)
{ { ClaireType *Result ;
    { list * l1 = x->ancestors;
      int  n1 = l1->length;
      list * l2 = y->ancestors;
      int  n2 = l2->length;
      if (n1 < n2)
       { if ((*(l2))[n1] == _oid_(x))
         Result = y;
        else Result = Kernel.emptySet;
          } 
      else if ((*(l1))[n2] == _oid_(y))
       Result = x;
      else Result = Kernel.emptySet;
        } 
    return (Result);} 
  } 


// for lists
/* The c++ function for: ^(x:list,y:list) [NEW_ALLOC] */
list * _exp_list(list *x,list *y)
{ GC_RESERVE(1);  // HOHO v3.0.55 optim !
  { list *Result ;
    { int  n = x->length;
      list * r = list::empty();
      if (n == y->length)
       { int  i = 1;
        int  g0111 = n;
        { OID gc_local;
          while ((i <= g0111))
          { GC_LOOP;
            { ClaireType * z = GC_OBJECT(ClaireType,OBJECT(ClaireType,(*Core.glb)((*(x))[i],
                (*(y))[i])));
              if (equal(_oid_(z),_oid_(Kernel.emptySet)) != CTRUE)
               r= r->addFast(_oid_(z));
              else { r= Kernel.nil;
                  { ;break;} 
                  } 
                } 
            ++i;
            GC_UNLOOP;} 
          } 
        } 
      Result = r;
      } 
    GC_UNBIND; return (Result);} 
  } 


// a combined union
/* The c++ function for: Uall(l:list) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
ClaireType * Uall_list(list *l)
{ GC_RESERVE(1);  // HOHO v3.0.55 optim !
  { ClaireType *Result ;
    { set * rep = Kernel.emptySet;
      { OID gc_local;
        ITERATE(x);
        for (START(l); NEXT(x);)
        { GC_LOOP;
          GC__ANY(rep = ((set *) U_type(rep,OBJECT(ClaireType,x))), 1);
          GC_UNLOOP;} 
        } 
      Result = rep;
      } 
    GC_UNBIND; return (Result);} 
  } 


// ------------------- The inclusion operation ------------------------
// hand-made
// v3.2: extend from set to bags
/* The c++ function for: <=t(s:bag,y:type) [NEW_ALLOC] */
ClaireBoolean * _inf_equalt_bag2(bag *s,ClaireType *y)
{ { ClaireBoolean *Result ;
    { ClaireType * z = of_bag(s);
      if (equal(_oid_(z),_oid_(Kernel.emptySet)) != CTRUE)
       Result = OBJECT(ClaireBoolean,_oid_((ClaireObject *) Core._inf_equalt->fcall(((int) z),((int) y))));
      else { OID  g0112UU;
          { ITERATE(x);
            g0112UU= _oid_(CFALSE);
            for (START(s); NEXT(x);)
            if (Ztype_any(x,_oid_(y)) != CTRUE)
             { g0112UU = Kernel.ctrue;
              break;} 
            } 
          Result = not_any(g0112UU);
          } 
        } 
    return (Result);} 
  } 


// class
/* The c++ function for: <=t(x:class,y:type) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
ClaireBoolean * _inf_equalt_class(ClaireClass *x,ClaireType *y)
{ GC_BIND;
  { ClaireBoolean *Result ;
    if (INHERIT(y->isa,Kernel._class))
     Result = contain_ask_list(x->ancestors,_oid_(y));
    else if (INHERIT(y->isa,Kernel._tuple))
     Result = CFALSE;
    else if (INHERIT(y->isa,Core._Union))
     { if ((x->open == 0) && 
          (boolean_I_any(_oid_(x->instances)) != CTRUE))
       { OID  g0113UU;
        { OID gc_local;
          ITERATE(c);
          g0113UU= _oid_(CFALSE);
          for (START(x->subclass); NEXT(c);)
          if (_inf_equalt_class(OBJECT(ClaireClass,c),y) != CTRUE)
           { g0113UU = Kernel.ctrue;
            break;} 
          else ;} 
        Result = not_any(g0113UU);
        } 
      else Result = ((_inf_equalt_class(x,GC_OBJECT(ClaireType,CLREAD(Union,y,t1))) == CTRUE) ? CTRUE : ((_inf_equalt_class(x,GC_OBJECT(ClaireType,CLREAD(Union,y,t2))) == CTRUE) ? CTRUE : CFALSE));
        } 
    else if (Kernel._set == y->isa)
     { if ((x->open == 0) && 
          (boolean_I_any(_oid_(x->subclass)) != CTRUE))
       { OID  g0114UU;
        { OID gc_local;
          ITERATE(u);
          g0114UU= _oid_(CFALSE);
          bag *u_support;
          u_support = GC_OBJECT(bag,enumerate_any(_oid_(x)));
          for (START(u_support); NEXT(u);)
          if (contain_ask_set(((set *) y),u) != CTRUE)
           { g0114UU = Kernel.ctrue;
            break;} 
          } 
        Result = not_any(g0114UU);
        } 
      else Result = CFALSE;
        } 
    else Result = _inf_equalt_type(x,y);
      GC_UNBIND; return (Result);} 
  } 


// Union
/* The c++ function for: <=t(x:Union,y:type) [NEW_ALLOC] */
ClaireBoolean * _inf_equalt_Union(Union *x,ClaireType *y)
{ GC_BIND;
  { ClaireBoolean *Result ;
    Result = (((OBJECT(ClaireBoolean,_oid_((ClaireObject *) Core._inf_equalt->fcall(((int) OBJECT(ClaireObject,GC_OID(_oid_(x->t1)))),((int) y))))) == CTRUE) ? (((OBJECT(ClaireBoolean,_oid_((ClaireObject *) Core._inf_equalt->fcall(((int) OBJECT(ClaireObject,GC_OID(_oid_(x->t2)))),((int) y))))) == CTRUE) ? CTRUE: CFALSE): CFALSE);
    GC_UNBIND; return (Result);} 
  } 


// Interval
/* The c++ function for: <=t(x:Interval,y:type) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
ClaireBoolean * _inf_equalt_Interval(Interval *x,ClaireType *y)
{ GC_BIND;
  { ClaireBoolean *Result ;
    Result = ((INHERIT(y->isa,Core._Interval)) ?
      ((CLREAD(Interval,y,arg1) <= x->arg1) ? ((x->arg2 <= CLREAD(Interval,y,arg2)) ? CTRUE: CFALSE): CFALSE) :
      ((Kernel._set == y->isa) ?
        OBJECT(ClaireBoolean,_oid_((ClaireObject *) Core._inf_equalt->fcall(((int) OBJECT(ClaireObject,GC_OID(_oid_(set_I_Interval(x))))),((int) y)))) :
        ((INHERIT(y->isa,Core._Union)) ?
          ((_inf_equalt_Interval(x,GC_OBJECT(ClaireType,CLREAD(Union,y,t1))) == CTRUE) ? CTRUE : ((_inf_equalt_Interval(x,GC_OBJECT(ClaireType,CLREAD(Union,y,t2))) == CTRUE) ? CTRUE : CFALSE)) :
          ((INHERIT(y->isa,Kernel._class)) ?
            inherit_ask_class(Kernel._integer,((ClaireClass *) y)) :
            _inf_equalt_type(x,y) ) ) ) );
    GC_UNBIND; return (Result);} 
  } 


// subtype
/* The c++ function for: <=t(x:subtype,y:type) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
ClaireBoolean * _inf_equalt_subtype(subtype *x,ClaireType *y)
{ GC_BIND;
  { ClaireBoolean *Result ;
    Result = ((INHERIT(y->isa,Core._Param)) ?
      _inf_equalt_type(x,y) :
      ((INHERIT(y->isa,Core._subtype)) ?
        _inf_equalt_type(x,y) :
        ((INHERIT(y->isa,Core._Union)) ?
          ((_inf_equalt_subtype(x,GC_OBJECT(ClaireType,CLREAD(Union,y,t1))) == CTRUE) ? CTRUE : ((_inf_equalt_subtype(x,GC_OBJECT(ClaireType,CLREAD(Union,y,t2))) == CTRUE) ? CTRUE : CFALSE)) :
          _inf_equalt_class(x->arg,y) ) ) );
    GC_UNBIND; return (Result);} 
  } 


// Param is similar !
/* The c++ function for: <=t(x:Param,y:type) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
ClaireBoolean * _inf_equalt_Param(Param *x,ClaireType *y)
{ GC_BIND;
  { ClaireBoolean *Result ;
    Result = ((INHERIT(y->isa,Core._Param)) ?
      _inf_equalt_type(x,y) :
      ((INHERIT(y->isa,Core._subtype)) ?
        _inf_equalt_type(x,y) :
        ((INHERIT(y->isa,Core._Union)) ?
          ((_inf_equalt_Param(x,GC_OBJECT(ClaireType,CLREAD(Union,y,t1))) == CTRUE) ? CTRUE : ((_inf_equalt_Param(x,GC_OBJECT(ClaireType,CLREAD(Union,y,t2))) == CTRUE) ? CTRUE : CFALSE)) :
          _inf_equalt_class(x->arg,y) ) ) );
    GC_UNBIND; return (Result);} 
  } 


// Reference
/* The c++ function for: <=t(x:Reference,y:type) [0] */
ClaireBoolean * _inf_equalt_Reference(Reference *x,ClaireType *y)
{ return (CTRUE);} 


// tuple : the only subtlety is the de-normalization of U within a tuple type
/* The c++ function for: <=t(x:tuple,y:type) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
ClaireBoolean * _inf_equalt_tuple(tuple *x,ClaireType *y)
{ GC_BIND;
  { ClaireBoolean *Result ;
    { OID  itest;
      { { OID  j_some = CNULL;
          { int  j = 1;
            int  g0116 = x->length;
            { while ((j <= g0116))
              { if (INHERIT(OWNER((*(x))[j]),Core._Union))
                 { j_some= j;
                  break;} 
                ++j;
                } 
              } 
            } 
          itest = j_some;
          } 
        GC_OID(itest);} 
      if (itest != CNULL)
       { int  i = itest;
        Union * ui = OBJECT(Union,(*(x))[i]);
        tuple * x1 = ((tuple *) copy_bag(x));
        tuple * x2 = ((tuple *) copy_bag(x));
        ((*(((list *) x1)))[i]=_oid_(ui->t1));
        ((*(((list *) x2)))[i]=_oid_(ui->t2));
        Result = ((_inf_equalt_tuple(x1,y) == CTRUE) ? ((_inf_equalt_tuple(x2,y) == CTRUE) ? CTRUE: CFALSE): CFALSE);
        } 
      else if (INHERIT(y->isa,Kernel._tuple))
       { ClaireBoolean *v_and;
        { v_and = ((x->length == ((bag *) y)->length) ? CTRUE : CFALSE);
          if (v_and == CFALSE) Result =CFALSE; 
          else { { OID  g0117UU;
              { int  i = 1;
                int  g0115 = x->length;
                { g0117UU= _oid_(CFALSE);
                  while ((i <= g0115))
                  { if ((*Core._inf_equalt)((*(x))[i],
                      (*(((bag *) y)))[i]) != Kernel.ctrue)
                     { g0117UU = Kernel.ctrue;
                      break;} 
                    ++i;
                    } 
                  } 
                } 
              v_and = not_any(g0117UU);
              } 
            if (v_and == CFALSE) Result =CFALSE; 
            else Result = CTRUE;} 
          } 
        } 
      else Result = ((INHERIT(y->isa,Core._Union)) ?
        ((_inf_equalt_tuple(x,GC_OBJECT(ClaireType,CLREAD(Union,y,t1))) == CTRUE) ? CTRUE : ((_inf_equalt_tuple(x,GC_OBJECT(ClaireType,CLREAD(Union,y,t2))) == CTRUE) ? CTRUE : CFALSE)) :
        _inf_equalt_class(Kernel._tuple,y) );
      } 
    GC_UNBIND; return (Result);} 
  } 


// this is a generic ordering when y is a type Interval, a subtype or a Param
// x <= one such type is actually easy
/* The c++ function for: <=t(x:type,y:type) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
ClaireBoolean * _inf_equalt_type(ClaireType *x,ClaireType *y)
{ GC_BIND;
  { ClaireBoolean *Result ;
    if (INHERIT(y->isa,Core._Param))
     { ClaireBoolean *v_and;
      { v_and = OBJECT(ClaireBoolean,_oid_((ClaireObject *) Core._inf_equalt->fcall(((int) x),((int) CLREAD(Param,y,arg)))));
        if (v_and == CFALSE) Result =CFALSE; 
        else { { OID  g0119UU;
            { int  n = 1;
              int  g0118 = CLREAD(Param,y,params)->length;
              { OID gc_local;
                g0119UU= _oid_(CFALSE);
                while ((n <= g0118))
                { GC_LOOP;
                  if ((*Core._inf_equalt)(GC_OID(_oid_(_at_type(x,OBJECT(property,(*(CLREAD(Param,y,params)))[n])))),
                    GC_OID((*(CLREAD(Param,y,args)))[n])) != Kernel.ctrue)
                   { g0119UU = Kernel.ctrue;
                    break;} 
                  ++n;
                  GC_UNLOOP;} 
                } 
              } 
            v_and = not_any(g0119UU);
            } 
          if (v_and == CFALSE) Result =CFALSE; 
          else Result = CTRUE;} 
        } 
      } 
    else Result = ((INHERIT(y->isa,Core._Reference)) ?
      CTRUE :
      ((INHERIT(y->isa,Core._subtype)) ?
        (((OBJECT(ClaireBoolean,_oid_((ClaireObject *) Core._inf_equalt->fcall(((int) x),((int) CLREAD(subtype,y,arg)))))) == CTRUE) ? (((OBJECT(ClaireBoolean,_oid_((ClaireObject *) Core._inf_equalt->fcall(((int) OBJECT(ClaireObject,GC_OID(_oid_(member_type(x))))),((int) OBJECT(ClaireObject,GC_OID(_oid_(CLREAD(subtype,y,t1))))))))) == CTRUE) ? CTRUE: CFALSE): CFALSE) :
        ((INHERIT(y->isa,Core._Interval)) ?
          CFALSE :
          OBJECT(ClaireBoolean,(*Core.less_ask)(_oid_(x),
            _oid_(y))) ) ) );
    GC_UNBIND; return (Result);} 
  } 


// for extensibility !
// default order for types
/* The c++ function for: <=(x:type,y:type) [NEW_ALLOC] */
ClaireBoolean * _inf_equal_type(ClaireType *x,ClaireType *y)
{ return (OBJECT(ClaireBoolean,_oid_((ClaireObject *) Core._inf_equalt->fcall(((int) x),((int) y)))));} 


// ******************************************************************
// *    Part 5: type methods                                        *
// ******************************************************************
// --------------------- extract tuple type information -------------
// extract a member type, that is a valid type for all members (z) of instances of
// the type x.This is much simpler in v3.0
/* The c++ function for: member(x:type) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
ClaireType * member_type(ClaireType *x)
{ GC_BIND;
  { ClaireType *Result ;
    if (INHERIT(x->isa,Kernel._class))
     { Result = ((x == Core._Interval) ?
        Kernel._integer :
        Kernel._any );
      } 
    else if (INHERIT(x->isa,Core._Union))
     Result = U_type(GC_OBJECT(ClaireType,member_type(GC_OBJECT(ClaireType,CLREAD(Union,x,t1)))),GC_OBJECT(ClaireType,member_type(GC_OBJECT(ClaireType,CLREAD(Union,x,t2)))));
    else if (INHERIT(x->isa,Core._Interval))
     Result = Kernel.emptySet;
    else if (INHERIT(x->isa,Core._Param))
     Result = member_type(GC_OBJECT(ClaireType,_at_type(x,Kernel.of)));
    else if (INHERIT(x->isa,Kernel._tuple))
     Result = Uall_list(((list *) x));
    else if (INHERIT(x->isa,Core._subtype))
     Result = CLREAD(subtype,x,t1);
    else if (Kernel._set == x->isa)
     { list * g0120UU;
      { { bag *v_list; OID v_val;
          OID y,CLcount;
          v_list = ((bag *) x);
           g0120UU = v_list->clone();
          for (CLcount= 1; CLcount <= v_list->length; CLcount++)
          { y = (*(v_list))[CLcount];
            if (INHERIT(OWNER(y),Kernel._list))
             v_val = _oid_(set_I_bag(OBJECT(list,y)));
            else if (INHERIT(OWNER(y),Kernel._type))
             v_val = y;
            else v_val = _oid_(Kernel.emptySet);
              
            (*((list *) g0120UU))[CLcount] = v_val;} 
          } 
        GC_OBJECT(list,g0120UU);} 
      Result = Uall_list(g0120UU);
      } 
    else Result = Kernel.emptySet;
      GC_UNBIND; return (Result);} 
  } 


// a simpler version (projection on bag subtypes)
// dumb code because it is used early in the bootstrap
/* The c++ function for: of_extract(x:type) [RETURN_ARG] */
ClaireType * of_extract_type(ClaireType *x)
{ { ClaireType *Result ;
    { ClaireClass * c = x->isa;
      if (c == Core._subtype)
       Result = CLREAD(subtype,x,t1);
      else if (c == Core._Param)
       { if ((*(CLREAD(Param,x,params)))[1] == _oid_(Kernel.of))
         { ClaireType * y = OBJECT(ClaireType,(*(CLREAD(Param,x,args)))[1]);
          if (Kernel._set == y->isa)
           Result = OBJECT(ClaireType,(*(((bag *) y)))[1]);
          else if (INHERIT(y->isa,Core._subtype))
           Result = CLREAD(subtype,y,t1);
          else Result = Kernel._any;
            } 
        else Result = Kernel._any;
          } 
      else Result = Kernel._any;
        } 
    return (Result);} 
  } 


// --------------------- extract range information ------------------
// the method @ is used to extract the range information contained
// in a type. This method returns a type and is crucial for compiling !
/* The c++ function for: @(x:type,p:property) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
ClaireType * _at_type(ClaireType *x,property *p)
{ GC_BIND;
  { ClaireType *Result ;
    if (INHERIT(x->isa,Kernel._class))
     { ClaireObject * r = GC_OBJECT(ClaireObject,_at_property1(p,((ClaireClass *) x)));
      if (r != CFALSE)
       Result = CLREAD(restriction,r,range);
      else Result = Kernel._any;
        } 
    else if (INHERIT(x->isa,Core._Param))
     { int  i = index_list(CLREAD(Param,x,params),_oid_(p));
      Result = ((i > 0) ?
        OBJECT(ClaireType,(*(CLREAD(Param,x,args)))[i]) :
        _at_type(CLREAD(Param,x,arg),p) );
      } 
    else if (INHERIT(x->isa,Core._Union))
     Result = U_type(GC_OBJECT(ClaireType,_at_type(GC_OBJECT(ClaireType,CLREAD(Union,x,t1)),p)),GC_OBJECT(ClaireType,_at_type(GC_OBJECT(ClaireType,CLREAD(Union,x,t2)),p)));
    else if (Kernel._set == x->isa)
     { list * g0121UU;
      { { bag *v_list; OID v_val;
          OID y,CLcount;
          v_list = ((bag *) x);
           g0121UU = v_list->clone();
          for (CLcount= 1; CLcount <= v_list->length; CLcount++)
          { y = (*(v_list))[CLcount];
            v_val = _oid_(set::alloc(1,GC_OID(funcall_property(p,y))));
            
            (*((list *) g0121UU))[CLcount] = v_val;} 
          } 
        GC_OBJECT(list,g0121UU);} 
      Result = Uall_list(g0121UU);
      } 
    else Result = _at_type(class_I_type(x),p);
      GC_UNBIND; return (Result);} 
  } 


// useful type functions for the compiler
/* The c++ function for: unique?(x:type) [0] */
ClaireBoolean * unique_ask_type(ClaireType *x)
{ { ClaireBoolean *Result ;
    Result = ((Kernel._set == x->isa) ?
      equal(size_set(((set *) x)),1) :
      ((INHERIT(x->isa,Kernel._class)) ?
        ((CLREAD(ClaireClass,x,open) == 0) ? ((size_class(((ClaireClass *) x)) == 1) ? CTRUE: CFALSE): CFALSE) :
        CFALSE ) );
    return (Result);} 
  } 


// returns the unique element of the type
/* The c++ function for: the(x:type) [NEW_ALLOC+RETURN_ARG] */
OID  the_type(ClaireType *x)
{ return ((*(OBJECT(bag,(*Kernel.set_I)(_oid_(x)))))[1]);} 


// bitvector made easy
// v0.01: should not use set[0 .. 29] => burden on caller is too heavy
/* The c++ function for: integer!(s:set[integer]) [NEW_ALLOC] */
int  integer_I_set(set *s)
{ { int Result = 0;
    { int  n = 0;
      { ITERATE(y);
        for (START(s); NEXT(y);)
        if ((y <= 29) && 
            (0 <= y))
         n= (n+exp2_integer(y));
        } 
      Result = n;
      } 
    return (Result);} 
  } 


/* The c++ function for: make_set(x:integer) [NEW_ALLOC] */
set * make_set_integer(int x)
{ GC_BIND;
  { set *Result ;
    { set * i_out = set::empty(Kernel.emptySet);
      { int  i = 0;
        int  g0122 = 29;
        { OID gc_local;
          while ((i <= g0122))
          { if (BCONTAIN(x,i))
             i_out->addFast(i);
            ++i;
            } 
          } 
        } 
      Result = GC_OBJECT(set,i_out);
      } 
    GC_UNBIND; return (Result);} 
  } 


// asbtract coercion of a set into an interval
/* The c++ function for: abstract_type(xt1:set) [NEW_ALLOC] */
ClaireType * abstract_type_set(set *xt1)
{ { ClaireType *Result ;
    { int  m1 = 1;
      int  m2 = 0;
      { ITERATE(x);
        for (START(xt1); NEXT(x);)
        if (INHERIT(OWNER(x),Kernel._integer))
         { if (m1 > m2)
           { m1= x;
            m2= x;
            } 
          else if (x > m2)
           m2= x;
          else if (x < m1)
           m1= x;
          } 
        else { m1= 1;
            m2= 0;
            { ;break;} 
            } 
          } 
      Result = _dot_dot_integer(m1,m2);
      } 
    return (Result);} 
  } 


// abstract interpretation of integer arithmetique
/* The c++ function for: abstract_type(p:operation,xt1:type,xt2:type) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
ClaireType * abstract_type_operation(operation *p,ClaireType *xt1,ClaireType *xt2)
{ GC_BIND;
  { ClaireType *Result ;
    if (Kernel._set == xt1->isa)
     { Result = ((equal(_oid_(xt1),_oid_(Kernel.emptySet)) != CTRUE) ?
        abstract_type_operation(p,GC_OBJECT(ClaireType,abstract_type_set(((set *) xt1))),xt2) :
        xt1 );
      } 
    else if (INHERIT(xt1->isa,Core._Interval))
     { if (INHERIT(xt2->isa,Core._Interval))
       { if (p == Core._plus)
         Result = _dot_dot_integer((CLREAD(Interval,xt1,arg1)+CLREAD(Interval,xt2,arg1)),(CLREAD(Interval,xt1,arg2)+CLREAD(Interval,xt2,arg2)));
        else if (p == Kernel._dash)
         Result = _dot_dot_integer((CLREAD(Interval,xt1,arg1)-CLREAD(Interval,xt2,arg2)),(CLREAD(Interval,xt1,arg2)-CLREAD(Interval,xt2,arg1)));
        else Result = Kernel._integer;
          } 
      else if (Kernel._set == xt2->isa)
       { Result = ((equal(_oid_(xt2),_oid_(Kernel.emptySet)) != CTRUE) ?
          abstract_type_operation(p,xt1,GC_OBJECT(ClaireType,abstract_type_set(((set *) xt2)))) :
          xt2 );
        } 
      else if (INHERIT(xt2->isa,Core._Union))
       Result = U_type(GC_OBJECT(ClaireType,abstract_type_operation(p,xt1,GC_OBJECT(ClaireType,CLREAD(Union,xt2,t1)))),GC_OBJECT(ClaireType,abstract_type_operation(p,xt1,GC_OBJECT(ClaireType,CLREAD(Union,xt2,t2)))));
      else Result = Kernel._integer;
        } 
    else if (INHERIT(xt1->isa,Core._Union))
     Result = U_type(GC_OBJECT(ClaireType,abstract_type_operation(p,GC_OBJECT(ClaireType,CLREAD(Union,xt1,t1)),xt2)),GC_OBJECT(ClaireType,abstract_type_operation(p,GC_OBJECT(ClaireType,CLREAD(Union,xt1,t2)),xt2)));
    else Result = Kernel._integer;
      GC_UNBIND; return (Result);} 
  } 


// we create some types that we need
// a useful second ortder type
/* The c++ function for: first_arg_type(x:type,y:type) [RETURN_ARG] */
ClaireType * first_arg_type_type(ClaireType *x,ClaireType *y)
{ return (x);} 


/* The c++ function for: first_arg_type(x:type,y:type,z:type) [RETURN_ARG] */
ClaireType * first_arg_type_type2(ClaireType *x,ClaireType *y,ClaireType *z)
{ return (x);} 


/* The c++ function for: second_arg_type(x:type,y:type) [RETURN_ARG] */
ClaireType * second_arg_type_type(ClaireType *x,ClaireType *y)
{ return (y);} 


/* The c++ function for: meet_arg_types(x:type,y:type) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
ClaireType * meet_arg_types_type(ClaireType *x,ClaireType *y)
{ return (U_type(x,y));} 


/* The c++ function for: first_member_type(x:type,y:type) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
ClaireType * first_member_type_type(ClaireType *x,ClaireType *y)
{ return (member_type(x));} 


// v3.3.10
// we place here all methods that require second order types !!!!
//nth_get(a:array,n:integer) : type[member(a)] -> function!(nth_get_array)
/* The c++ function for: nth(self:array,x:integer) [RETURN_ARG] */
OID  nth_array(OID *self,int x)
{ { OID Result = 0;
    if ((x > 0) && 
        (x <= self[0]))
     Result = nth_get_array(self,x);
    else { OID  V_CL0123;close_exception(((general_error *) (*Core._general_error)(_string_("[180] nth[~S] out of scope for ~S"),
          _oid_(list::alloc(2,x,_array_(self))))));
        
        Result=_void_(V_CL0123);} 
      return (Result);} 
  } 


/* The c++ function for: nth_array_type */
ClaireType * nth_array_type(ClaireType *self,ClaireType *x)
{ return (member_type(self));} 


/* The c++ function for: make_array_integer_type */
ClaireType * make_array_integer_type(ClaireType *i,ClaireType *t,ClaireType *v)
{ GC_BIND;
  { ClaireType *Result ;
    if (unique_ask_type(t) == CTRUE)
     Result = nth_type(GC_OBJECT(ClaireType,OBJECT(ClaireType,the_type(t))));
    else Result = Kernel._array;
      GC_UNBIND; return (Result);} 
  } 


/* The c++ function for: make_list(n:integer,t:type,x:any) [NEW_ALLOC] */
list * make_list_integer2(int n,ClaireType *t,OID x)
{ return (((list *) cast_I_bag(make_list_integer(n,x),t)));} 


/* The c++ function for: make_list_integer2_type */
ClaireType * make_list_integer2_type(ClaireType *n,ClaireType *t,ClaireType *x)
{ GC_BIND;
  { ClaireType *Result ;
    if (unique_ask_type(t) == CTRUE)
     Result = nth_class1(Kernel._list,GC_OBJECT(ClaireType,OBJECT(ClaireType,the_type(t))));
    else Result = Kernel._list;
      GC_UNBIND; return (Result);} 
  } 


/* The c++ function for: make_set(self:array[of:(any)]) [NEW_ALLOC+RETURN_ARG] */
set * make_set_array(OID *self)
{ GC_BIND;
  { set *Result ;
    Result = set_I_bag(GC_OBJECT(list,list_I_array(self)));
    GC_UNBIND; return (Result);} 
  } 


/* The c++ function for: make_set_array_type */
ClaireType * make_set_array_type(ClaireType *self)
{ if (member_type(GC_OBJECT(ClaireType,_at_type(self,Kernel.of))) == Kernel._any) 
  { { ClaireType *Result ;
      Result = Kernel._set;
      return (Result);} 
     } 
  else{ GC_BIND;
    { ClaireType *Result ;
      Result = nth_class2(Kernel._set,list::alloc(Kernel._any,1,_oid_(Kernel.of)),list::alloc(1,_oid_(set::alloc(1,GC_OID(_oid_(member_type(GC_OBJECT(ClaireType,_at_type(self,Kernel.of)))))))));
      GC_UNBIND; return (Result);} 
    } 
  } 


/* The c++ function for: list_I_array_type */
ClaireType * list_I_array_type(ClaireType *a)
{ if (member_type(GC_OBJECT(ClaireType,_at_type(a,Kernel.of))) == Kernel._any) 
  { { ClaireType *Result ;
      Result = Kernel._list;
      return (Result);} 
     } 
  else{ GC_BIND;
    { ClaireType *Result ;
      Result = nth_class2(Kernel._list,list::alloc(Kernel._any,1,_oid_(Kernel.of)),list::alloc(1,_oid_(set::alloc(1,GC_OID(_oid_(member_type(GC_OBJECT(ClaireType,_at_type(a,Kernel.of)))))))));
      GC_UNBIND; return (Result);} 
    } 
  } 


/* The c++ function for: array_I_list_type */
ClaireType * array_I_list_type(ClaireType *a)
{ if (member_type(GC_OBJECT(ClaireType,_at_type(a,Kernel.of))) == Kernel._any) 
  { { ClaireType *Result ;
      Result = Kernel._array;
      return (Result);} 
     } 
  else{ GC_BIND;
    { ClaireType *Result ;
      Result = nth_class2(Kernel._array,list::alloc(Kernel._any,1,_oid_(Kernel.of)),list::alloc(1,_oid_(set::alloc(1,GC_OID(_oid_(member_type(GC_OBJECT(ClaireType,_at_type(a,Kernel.of)))))))));
      GC_UNBIND; return (Result);} 
    } 
  } 


// v3.0.72
/* The c++ function for: set_I_bag_type */
ClaireType * set_I_bag_type(ClaireType *l)
{ if (member_type(GC_OBJECT(ClaireType,_at_type(l,Kernel.of))) == Kernel._any) 
  { { ClaireType *Result ;
      Result = Kernel._set;
      return (Result);} 
     } 
  else{ GC_BIND;
    { ClaireType *Result ;
      Result = nth_class2(Kernel._set,list::alloc(Kernel._any,1,_oid_(Kernel.of)),list::alloc(1,_oid_(set::alloc(1,GC_OID(_oid_(member_type(GC_OBJECT(ClaireType,_at_type(l,Kernel.of)))))))));
      GC_UNBIND; return (Result);} 
    } 
  } 


/* The c++ function for: list_I_set_type */
ClaireType * list_I_set_type(ClaireType *l)
{ if (member_type(GC_OBJECT(ClaireType,_at_type(l,Kernel.of))) == Kernel._any) 
  { { ClaireType *Result ;
      Result = Kernel._list;
      return (Result);} 
     } 
  else{ GC_BIND;
    { ClaireType *Result ;
      Result = nth_class2(Kernel._list,list::alloc(Kernel._any,1,_oid_(Kernel.of)),list::alloc(1,_oid_(set::alloc(1,GC_OID(_oid_(member_type(GC_OBJECT(ClaireType,_at_type(l,Kernel.of)))))))));
      GC_UNBIND; return (Result);} 
    } 
  } 


// new in v3.0.60 : second-order type for copy