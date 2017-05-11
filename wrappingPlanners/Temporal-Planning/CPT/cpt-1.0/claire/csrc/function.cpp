/***** CLAIRE Compilation of file c:\claire\v3.3\src\meta\function.cl 
         [version 3.3.4 / safety 5] Sat Oct 16 06:53:25 2004 *****/

#include <claire.h>
#include <Kernel.h>
#include <Core.h>

//+-------------------------------------------------------------+
//| CLAIRE                                                      |
//| function.cl                                                 |
//| Copyright (C) 1994 - 2003 Yves Caseau. All Rights Reserved  |
//| cf. copyright info in file object.cl: about()               |
//+-------------------------------------------------------------+
// --------------------------------------------------------------------
// This file hold the methods that are defined by an external function
// and those that apply to a primitive type
// --------------------------------------------------------------------
// *********************************************************************
// *  Table of contents                                                *
// *   Part 1: Basics of pretty printing                               *
// *   Part 2: Methods for CLAIRE objects                              *
// *   Part 3: System Methods                                          *
// *   Part 4: Methods for Native entities                             *
// *********************************************************************
// we find here what is necessary for the minimal kernel of CLAIRE
// ==============================================================
// *********************************************************************
// *   Part 1: Basics of pretty printing                               *
// *********************************************************************
// we use a nice object
// support reccursive print-in-string 
// buffered print
// new in v3.3.26: unbounded recursion is supported :-)
/* The c++ function for: print_in_string(_CL_obj:void) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
void  print_in_string_void()
{ GC_BIND;
  { int  n = (Core.pretty->cprevious+1);
    ClairePort * p1 = ((n < Core.pretty->cpstack->length) ?
      EXPORT((ClairePort *),(*(Core.pretty->cpstack))[(n+1)]) :
      port_I_void() );
    ClairePort * p2 = use_as_output_port(p1);
    (Core.pretty->cprevious = n);
    (Core.pretty->cpretty = p1);
    if (equal(_oid_(Core.pretty->cpstack),Core.nil->value) == CTRUE)
     (Core.pretty->cpstack = list::alloc(Kernel._port,2,GC_OID(ClAlloc->import(Kernel._port,(int *) p2)),GC_OID(ClAlloc->import(Kernel._port,(int *) p1))));
    else { ((*(Core.pretty->cpstack))[n]=ClAlloc->import(Kernel._port,(int *) p2));
        if (n == Core.pretty->cpstack->length)
         GC_OBJECT(list,Core.pretty->cpstack)->addFast(GC_OID(ClAlloc->import(Kernel._port,(int *) p1)));
        } 
      } 
  GC_UNBIND;} 


/* The c++ function for: end_of_string(_CL_obj:void) [SLOT_UPDATE] */
char * end_of_print_void()
{ if (Core.pretty->cprevious == 0)
   close_exception(((general_error *) (*Core._general_error)(_string_("[123] unbalanced use of print-in-string"),
    _oid_(Kernel.nil))));
  { char *Result ;
    { int  n = Core.pretty->cprevious;
      char * s = string_I_port(Core.pretty->cpretty);
      ClairePort * p = EXPORT((ClairePort *),(*(Core.pretty->cpstack))[n]);
      set_length_port(Core.pretty->cpretty,0);
      use_as_output_port(p);
      (Core.pretty->cpretty = p);
      (Core.pretty->cprevious = (Core.pretty->cprevious-1));
      Result = s;
      } 
    return (Result);} 
  } 


/* The c++ function for: mClaire/buffer_length(_CL_obj:void) [0] */
int  buffer_length_void()
{ return (length_port(Core.pretty->cpretty));} 


/* The c++ function for: mClaire/buffer_set_length(i:integer) [0] */
void  buffer_set_length_integer(int i)
{ set_length_port(Core.pretty->cpretty,i);
  } 


// a method for calling the printer without issuing a message
// here we assume that self_print is always defined as a function
/* The c++ function for: apply_self_print(self:any) [NEW_ALLOC+SLOT_UPDATE+RETURN_ARG] */
void  apply_self_print_any(OID self)
{ if (INHERIT(OWNER(self),Kernel._thing))
   princ_symbol(OBJECT(thing,self)->name);
  else if (INHERIT(OWNER(self),Kernel._class))
   princ_symbol(OBJECT(ClaireClass,self)->name);
  else if (INHERIT(OWNER(self),Kernel._integer))
   princ_integer(self);
  else if (Kernel._string == OWNER(self))
   (*Kernel.self_print)(self);
  else { method * _Zprop = ((method *) _at_property1(Kernel.self_print,OWNER(self)));
      if ((boolean_I_any(_oid_(_Zprop)) == CTRUE) && 
          (((_Zprop->functional == (NULL)) ? CTRUE : CFALSE) != CTRUE))
       { list * l = _Zprop->srange;
        fcall1(_Zprop->evaluate,OBJECT(ClaireClass,(*(l))[1]),self,OBJECT(ClaireClass,(*(l))[2]));
        } 
      else { ClaireHandler c_handle = ClaireHandler();
          if ERROR_IN 
          { (*Kernel.self_print)(self);
            ClEnv->cHandle--;} 
          else if (belong_to(_oid_(ClEnv->exception_I),_oid_(Kernel._any)) == CTRUE)
          { c_handle.catchIt();{ princ_string("<unprintable:");
              print_any(_oid_(OWNER(self)));
              princ_string(">");
              } 
            } 
          else PREVIOUS_HANDLER;} 
        } 
    } 


// some basic definitions
/* The c++ function for: self_print(self:any) [NEW_ALLOC+SLOT_UPDATE] */
void  self_print_any_Core(OID self)
{ if (self == CNULL)
   princ_string("unknown");
  else { princ_string("<");
      print_any(_oid_(OWNER(self)));
      princ_string(">");
      } 
    } 


/* The c++ function for: self_print(self:boolean) [0] */
void  self_print_boolean_Core(ClaireBoolean *self)
{ if (self == CTRUE)
   princ_string("true");
  else princ_string("false");
    } 


/* The c++ function for: self_print(self:function) [0] */
void  self_print_function_Core(ClaireFunction *self)
{ princ_string("#'");
  princ_string(string_I_function(self));
  princ_string("");
  } 


// prints the name of a restriction. If we have a close property and if a
// short-cut is possible, we use it.
/* The c++ function for: self_print(self:restriction) [NEW_ALLOC] */
void  self_print_restriction_Core(restriction *self)
{ if ((not_any(_oid_(((self->selector == (NULL)) ? CTRUE : CFALSE))) != CTRUE) || 
      (self->domain == (NULL)))
   { princ_string("<");
    print_any(_oid_(OWNER(_oid_(self))));
    princ_string(">");
    } 
  else { property * p = self->selector;
      int  n = 0;
      ClaireClass * c = domain_I_restriction(self);
      { ITERATE(r);
        for (START(p->restrictions); NEXT(r);)
        if (domain_I_restriction(OBJECT(restriction,r)) == c)
         ++n;
        } 
      princ_symbol(p->name);
      princ_string(" @ ");
      { OID  g0060UU;
        if (n == 1)
         g0060UU = _oid_(c);
        else g0060UU = _oid_(self->domain);
          print_any(g0060UU);
        } 
      princ_string("");
      } 
    } 


// we are too far
/* The c++ function for: print(x:any) [NEW_ALLOC+SLOT_UPDATE] */
void  print_any(OID x)
{ if ((Core.pretty->pbreak == CTRUE) && 
      (Core.pretty->pprint == CTRUE))
   { int  b_index = buffer_length_void();
    ClaireBoolean * missed = CFALSE;
    int  _Zl = Core.pretty->index;
    if (short_enough_integer((b_index+10)) != CTRUE)
     { (Core.pretty->pprint = CFALSE);
      (Core.pretty->pbreak = CFALSE);
      print_any(x);
      (Core.pretty->pprint = CTRUE);
      } 
    else { { ClaireHandler c_handle = ClaireHandler();
          if ERROR_IN 
          { { (Core.pretty->pbreak = CFALSE);
              apply_self_print_any(x);
              (Core.pretty->pbreak = CTRUE);
              } 
            ClEnv->cHandle--;} 
          else if (belong_to(_oid_(ClEnv->exception_I),_oid_(Core._much_too_far)) == CTRUE)
          { c_handle.catchIt();missed= CTRUE;
            } 
          else PREVIOUS_HANDLER;} 
        if (missed == CTRUE)
         { (Core.pretty->pprint = CTRUE);
          (Core.pretty->pbreak = CTRUE);
          buffer_set_length_integer(b_index);
          (Core.pretty->index = _Zl);
          apply_self_print_any(x);
          } 
        } 
      } 
  else apply_self_print_any(x);
    ;} 


/* The c++ function for: short_enough(self:integer) [0] */
ClaireBoolean * short_enough_integer(int self)
{ return (_inf_integer(self,Core.pretty->width));} 


// *********************************************************************
// *   Part 2: Methods for CLAIRE objects                              *
// *********************************************************************
// the instantiation body is a sequence of words from which the initialization
// of the object must be built.
//  copied_def = object (for object) + float (for float) + integer (for all)
//               + NULL for objects
// v3.2.12: use a condition that is coherent with ClReflect.cl : a slot defaut value must be placed
// unless it is a copied_def
/* The c++ function for: mClaire/complete!(self:object) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
ClaireObject * complete_I_object(ClaireObject *self)
{ GC_RESERVE(1);  // HOHO v3.0.55 optim !
  { OID gc_local;
    ITERATE(s);
    for (START(self->isa->slots); NEXT(s);)
    { GC_LOOP;
      { property * p = OBJECT(restriction,s)->selector;
        ClaireClass * s2 = OBJECT(slot,s)->srange;
        OID  d = GC_OID(OBJECT(slot,s)->DEFAULT);
        if (d != CNULL)
         { OID  v = slot_get_object(self,OBJECT(slot,s)->index,OBJECT(slot,s)->srange);
          if ((v == CNULL) && 
              (((s2 == Kernel._object) ? CTRUE : ((INHERIT(OWNER(d),Kernel._integer)) ? CTRUE : ((s2 == Kernel._float) ? CTRUE : CFALSE))) != CTRUE))
           update_property(p,
            self,
            OBJECT(slot,s)->index,
            OBJECT(slot,s)->srange,
            d);
          else if (equal(d,v) == CTRUE)
           { if (p->multivalued_ask != CFALSE)
             { OID gc_local;
              ITERATE(y);
              bag *y_support;
              y_support = GC_OBJECT(bag,enumerate_any(d));
              for (START(y_support); NEXT(y);)
              update_plus_relation(p,_oid_(self),y);
              } 
            else update_plus_relation(p,_oid_(self),d);
              } 
          } 
        } 
      GC_UNLOOP;} 
    } 
  { ClaireObject *Result ;
    { OID  m = GC_OID(_oid_(_at_property1(Kernel.close,OWNER(_oid_(self)))));
      Result = ((Kernel._method == OWNER(m)) ?
        OBJECT(ClaireObject,funcall_method1(OBJECT(method,m),_oid_(self))) :
        self );
      } 
    GC_UNBIND; return (Result);} 
  } 


// v3.0.41  obviously
//-------------------------- ENTITY   --------------------------------------
/* The c++ function for: not(self:any) [0] */
ClaireBoolean * not_any(OID self)
{ { ClaireBoolean *Result ;
    Result = ((self == Kernel.ctrue) ?
      CFALSE :
      ((self == Kernel.cfalse) ?
        CTRUE :
        ((boolean_I_any(self) != CTRUE) ?
          CTRUE :
          CFALSE ) ) );
    return (Result);} 
  } 


/* The c++ function for: !=(self:any,x:any) [0] */
ClaireBoolean * _I_equal_any(OID self,OID x)
{ { ClaireBoolean *Result ;
    Result = ((equal(self,x) == CTRUE) ?
      CFALSE :
      CTRUE );
    return (Result);} 
  } 


// gives the type of any object. This is open_coded.
/* The c++ function for: owner(self:any) [0] */
ClaireClass * owner_any(OID self)
{ return (OWNER(self));} 


// some useful methods
/* The c++ function for: known?(self:any) [0] */
ClaireBoolean * known_ask_any(OID self)
{ return (_I_equal_any(CNULL,self));} 


/* The c++ function for: unknown?(self:any) [0] */
ClaireBoolean * unknown_ask_any(OID self)
{ return (equal(CNULL,self));} 


// needed by the compiled code
/* The c++ function for: check_in(self:any,y:type) [RETURN_ARG] */
OID  check_in_any(OID self,ClaireType *y)
{ { OID Result = 0;
    if (belong_to(self,_oid_(y)) == CTRUE)
     Result = self;
    else { OID  V_CL0061;close_exception(((general_error *) (*Core._general_error)(_string_("[124] the value ~S does not belong to the range ~S"),
          _oid_(list::alloc(2,self,_oid_(y))))));
        
        Result=_void_(V_CL0061);} 
      return (Result);} 
  } 


/* The c++ function for: check_in(self:bag,c:class,y:type) [0] */
bag * check_in_bag(bag *self,ClaireClass *c,ClaireType *y)
{ { bag *Result ;
    { ClaireObject *V_CC ;
      { ClaireBoolean * g0062I;
        { OID  g0063UU;
          { ITERATE(z);
            g0063UU= _oid_(CFALSE);
            for (START(self); NEXT(z);)
            if (belong_to(z,_oid_(y)) != CTRUE)
             { g0063UU = Kernel.ctrue;
              break;} 
            } 
          g0062I = not_any(g0063UU);
          } 
        
        if (g0062I == CTRUE) V_CC = cast_I_bag(self,y);
          else close_exception(((general_error *) (*Core._general_error)(_string_("[124] the value ~S does not belong to subtype[~S²]"),
          _oid_(list::alloc(2,_oid_(self),_oid_(y))))));
        } 
      Result= (bag *) V_CC;} 
    return (Result);} 
  } 


// new in v3.00.48
/* The c++ function for: <(self:any,x:any) [NEW_ALLOC] */
ClaireBoolean * _inf_any(OID self,OID x)
{ { ClaireBoolean *Result ;
    Result = ((equal(self,x) == CTRUE) ?
      CFALSE :
      OBJECT(ClaireBoolean,(*Kernel._inf_equal)(self,
        x)) );
    return (Result);} 
  } 


/* The c++ function for: >(self:any,x:any) [NEW_ALLOC] */
ClaireBoolean * _sup_any(OID self,OID x)
{ { ClaireBoolean *Result ;
    Result = ((equal(self,x) == CTRUE) ?
      CFALSE :
      OBJECT(ClaireBoolean,(*Kernel._inf_equal)(x,
        self)) );
    return (Result);} 
  } 


// >= is defined as a macro in file.cl
// ----------------------- CLASS ---------------------------------------------
// declares a class as ephemeral: the member set is not maintained
// v3.2.14 recusively applies to subclasses
/* The c++ function for: ephemeral(self:class) [SLOT_UPDATE+RETURN_ARG] */
OID  ephemeral_class(ClaireClass *self)
{ { OID Result = 0;
    { ITERATE(c);
      Result= _oid_(CFALSE);
      for (START(self->descendents); NEXT(c);)
      if ((OBJECT(ClaireClass,c)->instances->length != 0) || 
          (OBJECT(ClaireClass,c)->open <= 1))
       close_exception(((general_error *) (*Core._general_error)(_string_("[187] cannot declare ~S as ephemeral because of ~S"),
        _oid_(list::alloc(2,_oid_(self),c)))));
      else (OBJECT(ClaireClass,c)->open = ClEnv->ephemeral);
        } 
    return (Result);} 
  } 


// declares a class as an abtract class (without instances)
/* The c++ function for: abstract(c:class) [NEW_ALLOC+SLOT_UPDATE+SAFE_RESULT] */
OID  abstract_class(ClaireClass *c)
{ { OID Result = 0;
    { int  n = c->open;
      if (n == ClEnv->ephemeral)
       close_exception(((general_error *) (*Core._general_error)(_string_("[125] ephemeral classes cannot be abstract"),
        _oid_(Kernel.nil))));
      else { if (c->instances->length != 0)
           ;if (n == 2)
           (c->open = 0);
          else if (n == 1)
           (c->open = -1);
          } 
        Result = _oid_(c);
      } 
    return (Result);} 
  } 


// declares a class as totally defined in the hierarchy: no new subclasses can be added.
/* The c++ function for: final(c:class) [SLOT_UPDATE+SAFE_RESULT] */
OID  final_class(ClaireClass *c)
{ { OID Result = 0;
    { int  n = c->open;
      if (n == 3)
       close_exception(((general_error *) (*Core._general_error)(_string_("[126] ephemeral classes cannot be set as final"),
        _oid_(Kernel.nil))));
      else if (n == 2)
       (c->open = 1);
      else if (n == 0)
       (c->open = -1);
      Result = _oid_(c);
      } 
    return (Result);} 
  } 


//instantiation with and without a name
/* The c++ function for: new(self:class) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
ClaireObject * new_class1(ClaireClass *self)
{ GC_BIND;
  { ClaireObject *Result ;
    { ClaireObject * o;
      { { if (self->open <= 0)
           close_exception(((general_error *) (*Core._general_error)(_string_("[105] cannot instantiate ~S"),
            _oid_(list::alloc(1,_oid_(self))))));
          o = new_object_class(self);
          } 
        GC_OBJECT(ClaireObject,o);} 
      if (self->open != ClEnv->ephemeral)
       self->instances->addFast(_oid_(o));
      Result = complete_I_object(o);
      } 
    GC_UNBIND; return (Result);} 
  } 


/* The c++ function for: new_class1_type */
ClaireType * new_class1_type(ClaireType *self)
{ GC_BIND;
  { ClaireType *Result ;
    Result = glb_class(Kernel._object,GC_OBJECT(ClaireType,member_type(self)));
    GC_UNBIND; return (Result);} 
  } 


// v3.2.26
/* The c++ function for: new(self:class,%nom:symbol) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
thing * new_class2(ClaireClass *self,symbol *_Znom)
{ { thing *Result ;
    { ClaireObject *V_CC ;
      { thing * o;
        { if (self->open <= 0)
           close_exception(((general_error *) (*Core._general_error)(_string_("[105] cannot instantiate ~S"),
            _oid_(list::alloc(1,_oid_(self))))));
          o = new_thing_class(self,_Znom);
          } 
        V_CC = complete_I_object(o);
        } 
      Result= (thing *) V_CC;} 
    return (Result);} 
  } 


/* The c++ function for: new_class2_type */
ClaireType * new_class2_type(ClaireType *self,ClaireType *_Znom)
{ GC_BIND;
  { ClaireType *Result ;
    Result = glb_class(Kernel._thing,GC_OBJECT(ClaireType,member_type(self)));
    GC_UNBIND; return (Result);} 
  } 


// v3.2.26
// internal version
/* The c++ function for: new_object_class_type */
ClaireType * new_object_class_type(ClaireType *self)
{ GC_BIND;
  { ClaireType *Result ;
    Result = glb_class(Kernel._object,GC_OBJECT(ClaireType,member_type(self)));
    GC_UNBIND; return (Result);} 
  } 


/* The c++ function for: new_thing_class_type */
ClaireType * new_thing_class_type(ClaireType *self,ClaireType *_Znom)
{ GC_BIND;
  { ClaireType *Result ;
    Result = glb_class(Kernel._thing,GC_OBJECT(ClaireType,member_type(self)));
    GC_UNBIND; return (Result);} 
  } 


// the smallest super_set of two sets
// there is always any, so it always returns a class
/* The c++ function for: meet(self:class,ens:class) [RETURN_ARG] */
ClaireClass * meet_class(ClaireClass *self,ClaireClass *ens)
{ { ClaireClass *Result ;
    { list * l1 = self->ancestors;
      list * l2 = ens->ancestors;
      int  m = ((l1->length < l2->length) ?
        l1->length :
        l2->length );
      { while ((equal((*(l1))[m],(*(l2))[m]) != CTRUE))
        { m= (m-1);
          } 
        } 
      Result = OBJECT(ClaireClass,(*(l1))[m]);
      } 
    return (Result);} 
  } 


// fast inclusion method for lattice_sets (lattice order). The argument is
// either a lattice_set or {}
/* The c++ function for: inherit?(self:class,ens:class) [0] */
ClaireBoolean * inherit_ask_class(ClaireClass *self,ClaireClass *ens)
{ { ClaireBoolean *Result ;
    { list * l = self->ancestors;
      int  n = ens->ancestors->length;
      Result = ((n <= l->length) ? (((*(l))[n] == _oid_(ens)) ? CTRUE: CFALSE): CFALSE);
      } 
    return (Result);} 
  } 


//------------- PROPERTY ---------------------------------------------------
// the two methods to access open(r)
// an abstract property is extensible and can receive new restrictions
/* The c++ function for: abstract(p:property) [SLOT_UPDATE+SAFE_RESULT] */
OID  abstract_property(property *p)
{ { OID Result = 0;
    { int  n = p->open;
      if (n < 2)
       close_exception(((general_error *) (*Core._general_error)(_string_("[127] ~S can no longer become abstract"),
        _oid_(list::alloc(1,_oid_(p))))));
      else (p->open = 3);
        Result = _oid_(p);
      } 
    return (Result);} 
  } 


// a final property is completely defined and cannot receive a new restriction
// v3.2.04: the new value 4 will be used to represent (compiled but open)
/* The c++ function for: final(r:relation) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
void  final_relation(ClaireRelation *r)
{ GC_BIND;
  if (INHERIT(r->isa,Kernel._property))
   { if (r->open <= 2)
     { (r->open = 1);
      { ClaireRelation * g0064 = r; 
        ClaireType * g0065;
        { list * g0066UU;
          { { bag *v_list; OID v_val;
              OID x,CLcount;
              v_list = CLREAD(property,r,restrictions);
               g0066UU = v_list->clone();
              for (CLcount= 1; CLcount <= v_list->length; CLcount++)
              { x = (*(v_list))[CLcount];
                v_val = (*(OBJECT(restriction,x)->domain))[1];
                
                (*((list *) g0066UU))[CLcount] = v_val;} 
              } 
            GC_OBJECT(list,g0066UU);} 
          g0065 = Uall_list(g0066UU);
          } 
        (g0064->domain = g0065);} 
      { ClaireRelation * g0067 = r; 
        ClaireType * g0068;
        { list * g0069UU;
          { { bag *v_list; OID v_val;
              OID x,CLcount;
              v_list = CLREAD(property,r,restrictions);
               g0069UU = v_list->clone();
              for (CLcount= 1; CLcount <= v_list->length; CLcount++)
              { x = (*(v_list))[CLcount];
                v_val = _oid_(OBJECT(restriction,x)->range);
                
                (*((list *) g0069UU))[CLcount] = v_val;} 
              } 
            GC_OBJECT(list,g0069UU);} 
          g0068 = Uall_list(g0069UU);
          } 
        (g0067->range = g0068);} 
      } 
    } 
  GC_UNBIND;} 


//------------- MODULES   --------------------------------------------------
// the close function gives its right value to the *internal* slot that
// is the order of the module in the system. The name is supposed to be
// read in the system module.
/* The c++ function for: close(self:module) [NEW_ALLOC+SLOT_UPDATE+SAFE_RESULT] */
module * close_module(module *self)
{ GC_BIND;
  if (self != claire.it)
   { if (((self->part_of == (NULL)) ? CTRUE : CFALSE) != CTRUE)
     { module * sup = self->part_of;
      GC_OBJECT(list,sup->parts)->addFast(_oid_(self));
      { OID gc_local;
        ITERATE(x);
        bag *x_support;
        x_support = GC_OBJECT(list,sup->uses);
        for (START(x_support); NEXT(x);)
        { GC_LOOP;
          if ((contain_ask_list(self->uses,x) != CTRUE) && 
              (INHERIT(OWNER(x),Kernel._module)))
           GC_OBJECT(list,self->uses)->addFast(x);
          GC_UNLOOP;} 
        } 
      } 
    } 
  { module *Result ;
    Result = self;
    GC_UNBIND; return (Result);} 
  } 


// note: dynamic modules are no longer supported
/* The c++ function for: get_symbol(self:string) [0] */
OID  get_symbol_string(char *self)
{ return (get_symbol_module(claire.it,self));} 


// *********************************************************************
// *   Part 3: System Methods                                          *
// *********************************************************************
// all these methods will be open-coded by the compiler
//get_stack(self:integer) : any -> get_stack(self)
//put_stack(self:integer,x:any) : any -> put_stack(self, x)
//push!(self:meta_system,x:any) : void -> push!(self, x)
//stat() : void -> function!(claire_stat)
/* The c++ function for: gensym(self:void) [0] */
symbol * gensym_void()
{ return (gensym_string("g"));} 


// world management
/* The c++ function for: store(l:list,n:integer,y:any) [BAG_UPDATE+RETURN_ARG] */
OID  store_list4(list *l,int n,OID y)
{ return (STOREI((*l)[n],y));} 


/* The c++ function for: store(l:array,n:integer,y:any) [RETURN_ARG] */
OID  store_array1(OID *l,int n,OID y)
{ return (store_array(l,n,y,CTRUE));} 


/* The c++ function for: commit(n:integer) [0] */
void  commit_integer(int n)
{ { while ((n < world_number()))
    { world_remove();
      } 
    } 
  } 


/* The c++ function for: backtrack(n:integer) [0] */
void  backtrack_integer(int n)
{ { while ((n < world_number()))
    { world_pop();
      } 
    } 
  } 


// allows to change the storage class
/* The c++ function for: store(l:listargs) [SLOT_UPDATE+RETURN_ARG] */
OID  store_listargs(listargs *l)
{ { OID Result = 0;
    { ITERATE(r);
      Result= _oid_(CFALSE);
      for (START(l); NEXT(r);)
      if (INHERIT(OWNER(r),Kernel._relation))
       (OBJECT(ClaireRelation,r)->store_ask = CTRUE);
      else if (Kernel._string == OWNER(r))
       { OID  v = value_string(string_v(r));
        if (INHERIT(OWNER(v),Core._global_variable))
         (OBJECT(global_variable,v)->store_ask = CTRUE);
        } 
      } 
    return (Result);} 
  } 


// *********************************************************************
// *   Part 4: Methods for Native entities                             *
// *********************************************************************
//------------------- STRING -----------------------------------------------
/* The c++ function for: symbol!(self:string) [0] */
symbol * symbol_I_string2(char *self)
{ return (symbol_I_string(self,claire.it));} 


/* The c++ function for: externC(s:string) [SAFE_RESULT] */
void  externC_string(char *s)
{ close_exception(((general_error *) (*Core._general_error)(_string_("cannot execute C code: ~A"),
    _oid_(list::alloc(1,_string_(s))))));
  } 


/* The c++ function for: externC(s:string,c:class) [SAFE_RESULT] */
OID  externC_string2(char *s,ClaireClass *c)
{ { OID Result = 0;
    { OID  V_CL0072;close_exception(((general_error *) (*Core._general_error)(_string_("cannot execute ~A"),
        _oid_(list::alloc(1,_string_(s))))));
      
      Result=_void_(V_CL0072);} 
    return (Result);} 
  } 


/* The c++ function for: externC_string2_type */
ClaireType * externC_string2_type(ClaireType *s,ClaireType *c)
{ return (member_type(c));} 


/* The c++ function for: nth_get(s:string,n:integer,max:integer) [RETURN_ARG] */
ClaireChar * nth_get_string(char *s,int n,int max)
{ { ClaireChar *Result ;
    { ClaireObject *V_CC ;
      if (n <= max)
       V_CC = _char_(s[n - 1]);
      else close_exception(((general_error *) (*Core._general_error)(_string_("Buffer string access"),
          _oid_(Kernel.nil))));
        Result= (ClaireChar *) V_CC;} 
    return (Result);} 
  } 


/* The c++ function for: nth_put(s:string,n:integer,c:char,max:integer) [BAG_UPDATE+RETURN_ARG] */
void  nth_put_string(char *s,int n,ClaireChar *c,int max)
{ if (n <= max)
   (s[n - 1] = (char) c->ascii);
  else close_exception(((general_error *) (*Core._general_error)(_string_("Buffer string access"),
      _oid_(Kernel.nil))));
    } 


//  v3.2.14
//------------------- SYMBOL -----------------------------------------------
/* The c++ function for: make_string(self:symbol) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
char * make_string_symbol(symbol *self)
{ print_in_string_void();
  princ_symbol(self);
  return (end_of_print_void());} 


//princ(self:symbol) : any -> function!(princ_symbol)
/* The c++ function for: self_print(self:symbol) [NEW_ALLOC+SLOT_UPDATE] */
void  self_print_symbol_Core(symbol *self)
{ princ_symbol(self->module_I->name);
  princ_string("/");
  print_any(_string_(string_I_symbol(self)));
  princ_string("");
  } 


//c_princ(self:symbol) : any -> function!(c_princ_symbol)
//gensym(self:string) : symbol -> function!(gensym_string, NEW_ALLOC)
//--------------------- INTEGER -----------------------------------------
/* The c++ function for: +(self:integer,x:integer) [0] */
int  _plus_integer(int self,int x)
{ return (_integer_(self+x));} 


/* The c++ function for: +_integer_type */
ClaireType * _plus_integer_type(ClaireType *self,ClaireType *x)
{ return (abstract_type_operation(Core._plus,self,x));} 


/* The c++ function for: -(self:integer,x:integer) [0] */
int  _dash_integer1(int self,int x)
{ return ((self-x));} 


/* The c++ function for: -_integer1_type */
ClaireType * _dash_integer1_type(ClaireType *self,ClaireType *x)
{ return (abstract_type_operation(Kernel._dash,self,x));} 


//-(self:integer) : integer -> function!(ch_sign)
//float!(self:integer) : float -> function!(to_float)
//mod(self:integer,x:integer) : integer -> function!(mod_integer)
//less_code(n:integer,i:integer) : boolean -> function!(less_code_integer)
/* The c++ function for: <<(x:integer,y:integer) [0] */
int  _inf_inf_integer(int x,int y)
{ return ((x << y));} 


/* The c++ function for: >>(x:integer,y:integer) [0] */
int  _sup_sup_integer(int x,int y)
{ return ((x >> y));} 


/* The c++ function for: and(x:integer,y:integer) [0] */
int  and_integer(int x,int y)
{ return ((x & y));} 


/* The c++ function for: or(x:integer,y:integer) [0] */
int  or_integer(int x,int y)
{ return ((x | y));} 


// open-coded
/* The c++ function for: <(self:integer,x:integer) [0] */
ClaireBoolean * _inf_integer(int self,int x)
{ { ClaireBoolean *Result ;
    Result = ((self < x) ?
      CTRUE :
      CFALSE );
    return (Result);} 
  } 


/* The c++ function for: <=(self:integer,x:integer) [0] */
ClaireBoolean * _inf_equal_integer(int self,int x)
{ { ClaireBoolean *Result ;
    Result = ((self <= x) ?
      CTRUE :
      CFALSE );
    return (Result);} 
  } 


/* The c++ function for: >(self:integer,x:integer) [0] */
ClaireBoolean * _sup_integer(int self,int x)
{ { ClaireBoolean *Result ;
    Result = ((self > x) ?
      CTRUE :
      CFALSE );
    return (Result);} 
  } 


/* The c++ function for: nth(self:integer,y:integer) [0] */
ClaireBoolean * nth_integer(int self,int y)
{ { ClaireBoolean *Result ;
    Result = ((BCONTAIN(self,y)) ?
      CTRUE :
      CFALSE );
    return (Result);} 
  } 


// used by the logic
/* The c++ function for: factor?(x:integer,y:integer) [0] */
ClaireBoolean * factor_ask_integer(int x,int y)
{ return (equal(mod_integer(x,y),0));} 


/* The c++ function for: divide?(x:integer,y:integer) [0] */
ClaireBoolean * divide_ask_integer(int x,int y)
{ return (equal(mod_integer(y,x),0));} 


/* The c++ function for: Id(x:any) [RETURN_ARG] */
OID  Id_any(OID x)
{ return (x);} 


/* The c++ function for: Id_any_type */
ClaireType * Id_any_type(ClaireType *x)
{ return (x);} 


/* The c++ function for: pair(x:any,y:any) [NEW_ALLOC] */
list * pair_any(OID x,OID y)
{ return (list::alloc(2,x,y));} 


/* The c++ function for: pair_1(x:list) [RETURN_ARG] */
OID  pair_1_list(list *x)
{ return ((*(x))[1]);} 


/* The c++ function for: pair_1_list_type */
ClaireType * pair_1_list_type(ClaireType *x)
{ return (member_type(x));} 


/* The c++ function for: pair_2(x:list) [RETURN_ARG] */
OID  pair_2_list(list *x)
{ return ((*(x))[2]);} 


/* The c++ function for: pair_2_list_type */
ClaireType * pair_2_list_type(ClaireType *x)
{ return (member_type(x));} 


//------------------------ FLOAT ---------------------------------------------
/* The c++ function for: +(g0073:any,g0074:any) [0] */
OID  _plus_float_(OID g0073,OID g0074)
{ return _float_(_plus_float(float_v(g0073),float_v(g0074)));} 


/* The c++ function for: +(self:float,x:float) [0] */
double  _plus_float(double self,double x)
{ { double Result =0.0;
    { double  y = (self+x);
      Result = y;
      } 
    return (Result);} 
  } 


/* The c++ function for: -(g0075:any,g0076:any) [0] */
OID  _dash_float_(OID g0075,OID g0076)
{ return _float_(_dash_float(float_v(g0075),float_v(g0076)));} 


/* The c++ function for: -(self:float,x:float) [0] */
double  _dash_float(double self,double x)
{ { double Result =0.0;
    { double  y = (self-x);
      Result = y;
      } 
    return (Result);} 
  } 


/* The c++ function for: *(g0077:any,g0078:any) [0] */
OID  _star_float_(OID g0077,OID g0078)
{ return _float_(_star_float(float_v(g0077),float_v(g0078)));} 


/* The c++ function for: *(self:float,x:float) [0] */
double  _star_float(double self,double x)
{ { double Result =0.0;
    { double  y = (self*x);
      Result = y;
      } 
    return (Result);} 
  } 


/* The c++ function for: /(g0079:any,g0080:any) [0] */
OID  _7_float_(OID g0079,OID g0080)
{ return _float_(_7_float(float_v(g0079),float_v(g0080)));} 


/* The c++ function for: /(self:float,x:float) [0] */
double  _7_float(double self,double x)
{ { double Result =0.0;
    { double  y = (self/x);
      Result = y;
      } 
    return (Result);} 
  } 


/* The c++ function for: -(g0081:any) [NEW_ALLOC] */
OID  _dash_float2_(OID g0081)
{ return _float_(_dash_float2(float_v(g0081)));} 


/* The c++ function for: -(self:float) [NEW_ALLOC] */
double  _dash_float2(double self)
{ return (((-1.0)*self));} 


/* The c++ function for: sqrt(g0082:any) [0] */
OID  sqrt_float_(OID g0082)
{ return _float_(sqrt_float(float_v(g0082)));} 


/* The c++ function for: sqrt(self:float) [0] */
double  sqrt_float(double self)
{ { double Result =0.0;
    { double  y = sqrt(self);
      Result = y;
      } 
    return (Result);} 
  } 


/* The c++ function for: ^(g0083:any,g0084:any) [NEW_ALLOC] */
OID  _exp_float_(OID g0083,OID g0084)
{ return _float_(_exp_float(float_v(g0083),float_v(g0084)));} 


/* The c++ function for: ^(self:float,x:float) [NEW_ALLOC] */
double  _exp_float(double self,double x)
{ { double Result =0.0;
    { double  y = 0.0;
      y = pow(self,x);
      Result = y;
      } 
    return (Result);} 
  } 


/* The c++ function for: log(g0085:any) [NEW_ALLOC] */
OID  log_float_(OID g0085)
{ return _float_(log_float(float_v(g0085)));} 


/* The c++ function for: log(self:float) [NEW_ALLOC] */
double  log_float(double self)
{ { double Result =0.0;
    { double  y = 0.0;
      y = log(self);
      Result = y;
      } 
    return (Result);} 
  } 


/* The c++ function for: mClaire/atan(g0086:any) [NEW_ALLOC] */
OID  atan_float_(OID g0086)
{ return _float_(atan_float(float_v(g0086)));} 


/* The c++ function for: mClaire/atan(self:float) [NEW_ALLOC] */
double  atan_float(double self)
{ { double Result =0.0;
    { double  y = 0.0;
      y = atan(self);
      Result = y;
      } 
    return (Result);} 
  } 


/* The c++ function for: string!(g0087:any) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
char * string_I_float_(OID g0087)
{ return string_I_float(float_v(g0087));} 


/* The c++ function for: string!(self:float) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
char * string_I_float(double self)
{ print_in_string_void();
  princ_float(self);
  return (end_of_print_void());} 


//--------- BAG --------------------------------------------------------
/* The c++ function for: length(self:bag) [0] */
int  length_bag(bag *self)
{ return (self->length);} 


/* The c++ function for: nth(self:bag,x:integer) [RETURN_ARG] */
OID  nth_bag(bag *self,int x)
{ { OID Result = 0;
    if ((x > 0) && 
        (x <= self->length))
     Result = (*(self))[x];
    else { OID  V_CL0088;close_exception(((general_error *) (*Core._general_error)(_string_("[41] nth[~S] out of scope for ~S"),
          _oid_(list::alloc(2,x,_oid_(self))))));
        
        Result=_void_(V_CL0088);} 
      return (Result);} 
  } 


/* The c++ function for: nth_bag_type */
ClaireType * nth_bag_type(ClaireType *self,ClaireType *x)
{ GC_BIND;
  { ClaireType *Result ;
    Result = (((INHERIT(self->isa,Kernel._tuple)) && 
        (unique_ask_type(x) == CTRUE)) ?
      OBJECT(ClaireType,(*Kernel.nth)(_oid_(self),
        GC_OID(the_type(x)))) :
      member_type(self) );
    GC_UNBIND; return (Result);} 
  } 


/* The c++ function for: nth_get(self:bag,x:integer) [RETURN_ARG] */
OID  nth_get_bag(bag *self,int x)
{ return ((*(self))[x]);} 


/* The c++ function for: min(f:method,self:bag) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
OID  min_method(method *f,bag *self)
{ { OID Result = 0;
    if (self->length != 0)
     { OID  x = (*(self))[1];
      { ITERATE(y);
        for (START(self); NEXT(y);)
        if ((OBJECT(ClaireBoolean,funcall_method2(f,y,x))) == CTRUE)
         x= y;
        } 
      Result = x;
      } 
    else { OID  V_CL0089;close_exception(((general_error *) (*Core._general_error)(_string_("[183] min of empty set is undefined"),
          _oid_(Kernel.nil))));
        
        Result=_void_(V_CL0089);} 
      return (Result);} 
  } 


/* The c++ function for: min_method_type */
ClaireType * min_method_type(ClaireType *f,ClaireType *self)
{ return (member_type(self));} 


/* The c++ function for: max(f:method,self:bag) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
OID  max_method(method *f,bag *self)
{ { OID Result = 0;
    if (self->length != 0)
     { OID  x = (*(self))[1];
      { ITERATE(y);
        for (START(self); NEXT(y);)
        if (boolean_I_any(funcall_method2(f,y,x)) != CTRUE)
         x= y;
        } 
      Result = x;
      } 
    else { OID  V_CL0090;close_exception(((general_error *) (*Core._general_error)(_string_("[183] max of empty set is undefined"),
          _oid_(Kernel.nil))));
        
        Result=_void_(V_CL0090);} 
      return (Result);} 
  } 


/* The c++ function for: max_method_type */
ClaireType * max_method_type(ClaireType *f,ClaireType *self)
{ return (member_type(self));} 


// there seems to be a difficulty with providing this method with the proper type ..
/* The c++ function for: /+(x:bag,y:bag) [NEW_ALLOC] */
list * _7_plus_bag(bag *x,bag *y)
{ GC_RESERVE(1);  // HOHO v3.0.55 optim !
  { list *Result ;
    { list * l;
      { { ClaireObject *V_CC ;
          if (INHERIT(x->isa,Kernel._list))
           V_CC = copy_bag(x);
          else if (Kernel._set == x->isa)
           V_CC = list_I_set(((set *) x));
          else if (INHERIT(x->isa,Kernel._tuple))
           V_CC = list_I_tuple(((tuple *) x));
          else V_CC = CFALSE;
            l= (list *) V_CC;} 
        GC_OBJECT(list,l);} 
      if (INHERIT(y->isa,Kernel._list))
       l= GC_OBJECT(list,append_list(l,((list *) y)));
      else { OID gc_local;
          ITERATE(z);
          for (START(y); NEXT(z);)
          { GC_LOOP;
            GC__ANY(l = l->addFast(z), 1);
            GC_UNLOOP;} 
          } 
        Result = l;
      } 
    GC_UNBIND; return (Result);} 
  } 


//--------- LIST --------------------------------------------------------
// last element of a list
/* The c++ function for: last(self:list) [RETURN_ARG] */
OID  last_list(list *self)
{ { OID Result = 0;
    if (self->length > 0)
     Result = (*(self))[self->length];
    else { OID  V_CL0091;close_exception(((general_error *) (*Core._general_error)(_string_("[41] car(nil) is undefined"),
          _oid_(Kernel.nil))));
        
        Result=_void_(V_CL0091);} 
      return (Result);} 
  } 


/* The c++ function for: last_list_type */
ClaireType * last_list_type(ClaireType *self)
{ return (member_type(self));} 


// remove the last element
/* The c++ function for: rmlast(self:list) [RETURN_ARG] */
list * rmlast_list(list *self)
{ delete_at_list(self,self->length);
  return (self);} 


/* The c++ function for: nth=(self:list,x:integer,y:any) [BAG_UPDATE+RETURN_ARG] */
OID  nth_set_list(list *self,int x,OID y)
{ { OID Result = 0;
    if ((x <= 0) || 
        (x > self->length))
     { OID  V_CL0092;close_exception(((general_error *) (*Core._general_error)(_string_("[41] nth[~S] out of scope for ~S"),
        _oid_(list::alloc(2,x,_oid_(self))))));
      
      Result=_void_(V_CL0092);} 
    else if (belong_to(y,_oid_(of_bag(self))) == CTRUE)
     Result = ((*(self))[x]=y);
    else { OID  V_CL0093;close_exception(((system_error *) (*Kernel._system_error)(17,
          y,
          _oid_(self))));
        
        Result=_void_(V_CL0093);} 
      return (Result);} 
  } 


// v3.2.00
// the old LISP method
/* The c++ function for: car(self:list) [RETURN_ARG] */
OID  car_list(list *self)
{ { OID Result = 0;
    if (self->length > 0)
     Result = (*(self))[1];
    else { OID  V_CL0094;close_exception(((general_error *) (*Core._general_error)(_string_("[41] car(nil) is undefined"),
          _oid_(Kernel.nil))));
        
        Result=_void_(V_CL0094);} 
      return (Result);} 
  } 


// hashtable basics
/* The c++ function for: hashlist(n:integer) [NEW_ALLOC] */
list * hashlist_integer(int n)
{ GC_BIND;
  { list *Result ;
    { list * l = GC_OBJECT(list,make_list_integer(n,CNULL));
      int  u = (((*(l))[0])-3);
      { int  i = (n+1);
        int  g0095 = u;
        { OID gc_local;
          while ((i <= g0095))
          { l->addFast(CNULL);
            ++i;
            } 
          } 
        } 
      Result = l;
      } 
    GC_UNBIND; return (Result);} 
  } 


/* The c++ function for: hashsize(l:list) [0] */
int  hashsize_list(list *l)
{ { int Result = 0;
    { int  x = 0;
      { ITERATE(y);
        for (START(l); NEXT(y);)
        if (y != CNULL)
         ++x;
        } 
      Result = x;
      } 
    return (Result);} 
  } 


// this method sorts a list according to an order
/* The c++ function for: sort(f:method,self:list) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
list * sort_method(method *f,list *self)
{ quicksort_list(self,f,1,self->length);
  return (self);} 


// v3.0.38: upgrade the quicksort algorithm with a better pivot selection cf.bag.cpp
// this is also proposed as a macro: cf. file.cl
/* The c++ function for: quicksort(self:list,f:method,n:integer,m:integer) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
void  quicksort_list(list *self,method *f,int n,int m)
{ if (m > n)
   { OID  x = (*(self))[n];
    if (m == (n+1))
     { if ((OBJECT(ClaireBoolean,funcall_method2(f,(*(self))[m],x))) == CTRUE)
       { ((*(self))[n]=(*(self))[m]);
        ((*(self))[m]=x);
        } 
      } 
    else { int  p = ((m+n)>>1);
        int  q = n;
        x= (*(self))[p];
        if (p != n)
         ((*(self))[p]=(*(self))[n]);
        { int  p = (n+1);
          int  g0096 = m;
          { OID gc_local;
            while ((p <= g0096))
            { if ((OBJECT(ClaireBoolean,funcall_method2(f,(*(self))[p],x))) == CTRUE)
               { ((*(self))[n]=(*(self))[p]);
                ++n;
                if (p > n)
                 ((*(self))[p]=(*(self))[n]);
                } 
              ++p;
              } 
            } 
          } 
        ((*(self))[n]=x);
        quicksort_list(self,f,q,(n-1));
        quicksort_list(self,f,(n+1),m);
        } 
      } 
  } 


// destructive method that build the powerset
/* The c++ function for: build_powerset(self:list) [NEW_ALLOC+BAG_UPDATE] */
set * build_powerset_list(list *self)
{ GC_RESERVE(1);  // HOHO v3.0.55 optim !
  { set *Result ;
    if (self->length != 0)
     { OID  x = (*(self))[1];
      set * l1 = GC_OBJECT(set,build_powerset_list(skip_list(self,1)));
      set * l2 = l1;
      { OID gc_local;
        ITERATE(y);
        for (START(l1); NEXT(y);)
        { GC_LOOP;
          GC__ANY(l2 = l2->addFast(GC_OID(_oid_(append_set(set::alloc(1,x),OBJECT(set,y))))), 1);
          GC_UNLOOP;} 
        } 
      Result = l2;
      } 
    else Result = set::alloc(Kernel.emptySet,1,_oid_(Kernel.emptySet));
      GC_UNBIND; return (Result);} 
  } 


// <<(x:list,y:integer) : list -> function!(skip_list)
// new and useful (v3.1.06)
/* The c++ function for: make_copy_list(n:integer,d:any) [NEW_ALLOC+BAG_UPDATE] */
list * make_copy_list_integer(int n,OID d)
{ GC_BIND;
  { list *Result ;
    { list * l = GC_OBJECT(list,make_list_integer(n,d));
      if (INHERIT(OWNER(d),Kernel._bag))
       { int  i = 1;
        int  g0097 = n;
        { OID gc_local;
          while ((i <= g0097))
          { ((*(l))[i]=_oid_(copy_bag(OBJECT(bag,d))));
            ++i;
            } 
          } 
        } 
      Result = l;
      } 
    GC_UNBIND; return (Result);} 
  } 


//----------------------  SET  ---------------------------------------------
/* The c++ function for: difference(self:set,x:set) [NEW_ALLOC] */
set * difference_set(set *self,set *x)
{ GC_BIND;
  { set *Result ;
    { bag * y_in = self;
      set * y_out = ((set *) empty_bag(y_in));
      { ITERATE(y);
        for (START(y_in); NEXT(y);)
        if (contain_ask_set(x,y) != CTRUE)
         y_out->addFast(y);
        } 
      Result = GC_OBJECT(set,y_out);
      } 
    GC_UNBIND; return (Result);} 
  } 


//--------- ARRAY --------------------------------------------------------
/* The c++ function for: nth=(self:array,x:integer,y:any) [RETURN_ARG] */
void  nth_equal_array(OID *self,int x,OID y)
{ if (belong_to(y,_oid_(of_array(self))) != CTRUE)
   close_exception(((general_error *) (*Core._general_error)(_string_("type mismatch for array update ~S, ~S"),
    _oid_(list::alloc(2,y,_array_(self))))));
  else if ((x > 0) && 
      (x <= self[0]))
   nth_put_array(self,x,y);
  else close_exception(((general_error *) (*Core._general_error)(_string_("nth[~S] out of scope for ~S"),
      _oid_(list::alloc(2,x,_array_(self))))));
    } 


/* The c++ function for: self_print(self:array) [NEW_ALLOC] */
void  self_print_array_Core(OID *self)
{ princ_string("array<");
  print_any(_oid_(of_array(self)));
  princ_string(">[");
  princ_integer(self[0]);
  princ_string("]");
  } 


//---------------------- CHAR --------------------------------------------
/* The c++ function for: self_print(self:char) [0] */
void  self_print_char_Core(ClaireChar *self)
{ princ_string("'");
  princ_char(self);
  princ_string("'");
  } 


/* The c++ function for: <=(c1:char,c2:char) [0] */
ClaireBoolean * _inf_equal_char(ClaireChar *c1,ClaireChar *c2)
{ return (_inf_equal_integer(c1->ascii,c2->ascii));} 


// three methods that are useful for debugging !