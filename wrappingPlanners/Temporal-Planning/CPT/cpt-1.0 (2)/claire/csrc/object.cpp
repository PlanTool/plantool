/***** CLAIRE Compilation of file c:\claire\v3.3\src\meta\object.cl 
         [version 3.3.4 / safety 5] Sat Oct 16 06:53:25 2004 *****/

#include <claire.h>
#include <Kernel.h>
#include <Core.h>

//+-------------------------------------------------------------+
//| CLAIRE                                                      |
//| object.cl                                                   |
//| Copyright (C) 1994 - 2003 Yves Caseau. All Rights Reserved  |
//| cf. copyright info in about()                               |
//+-------------------------------------------------------------+
// ---------------------------------------------------------------------
// This file contains the definition of the objects that implement the
// core features of the microCLAIRE library: traceable & debug-able calls,
// tables, demons and exceptions
// ---------------------------------------------------------------------
// *********************************************************************
// *  Table of contents                                                *
// *   Part 1: Ask, debug & trace                                      *
// *   Part 2: Tables                                                  *
// *   Part 3: Demons & relations for the logic modules                *
// *   Part 4: Basics of Exceptions                                    *
// *********************************************************************
// release() should produce a version number
/* The c++ function for: release(_CL_obj:void) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
OID  release_void()
{ GC_BIND;
  { OID Result = 0;
    Result = _string_(append_string("3.",GC_STRING(string_I_float(ClEnv->version))));
    GC_UNBIND; return (Result);} 
  } 


// the about method produces the legal warning, according to the GNU software
// recommendation
/* The c++ function for: about(_CL_obj:void) [0] */
OID  about_void()
{ princ_string("CLAIRE v3.");
  princ_float(ClEnv->version);
  princ_string(" Copyright (C) 1994-2003 Yves Caseau. All Rights Reserved.\n");
  princ_string("use in binary forms is permitted\n");
  princ_string("redistribution or resale in any form are not permitted without the\n");
  princ_string("explicit agreement of Yves Caseau\n");
  princ_string("THIS SOFTWARE IS PROVIDED AS IS AND WITHOUT ANY WARRANTY, INCLUDING,\n");
  princ_string("WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF MERCHANTABILTY AND FITNESS\n");
  princ_string("FOR A PARTICULAR PURPOSE\n");
  return (Kernel.ctrue);} 


// *********************************************************************
// *   Part 1: Ask, debug & trace                                      *
// *********************************************************************
// create the list of arguments if needed : allocate on the stack
/* The c++ function for: mClaire/get_args(i:integer) [NEW_ALLOC] */
list * get_args_integer(int i)
{ GC_RESERVE(1);  // HOHO v3.0.55 optim !
  { list *Result ;
    { list * liste = list::empty(Kernel._any);
      { OID gc_local;
        while ((i < ClEnv->index))
        { GC_LOOP;
          GC__ANY(liste = liste->addFast(GC_OID(ClEnv->stack[i])), 1);
          ++i;
          GC_UNLOOP;} 
        } 
      Result = liste;
      } 
    GC_UNBIND; return (Result);} 
  } 


// evaluation of a message without the message structure, with a list
// of arguments. This method must be garbage-protected, because it is
// used as an entry point.
// to remove !!!!
// a simple method for a direct call with no argument
/* The c++ function for: funcall(self:method,x:any) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
OID  funcall_method1(method *self,OID x)
{ { OID Result = 0;
    { int  start = ClEnv->index;
      PUSH(x);
      Result = execute_method(self,start,CFALSE);
      } 
    return (Result);} 
  } 


// this is a simple method for calling directly a method with one argument
/* The c++ function for: funcall(self:method,x:any,y:any) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
OID  funcall_method2(method *self,OID x,OID y)
{ { OID Result = 0;
    { int  start = ClEnv->index;
      PUSH(x);
      PUSH(y);
      Result = execute_method(self,start,CFALSE);
      } 
    return (Result);} 
  } 


// this is a simple method for calling directly a method with two arguments
/* The c++ function for: funcall(self:method,x:any,y:any,z:any) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
OID  funcall_method3(method *self,OID x,OID y,OID z)
{ { OID Result = 0;
    { int  start = ClEnv->index;
      PUSH(x);
      PUSH(y);
      PUSH(z);
      Result = execute_method(self,start,CFALSE);
      } 
    return (Result);} 
  } 


// how to apply a function to a list
/* The c++ function for: apply(self:function,ls:list,l:list) [0] */
OID  apply_function(ClaireFunction *self,list *ls,list *l)
{ { OID Result = 0;
    { int  start = ClEnv->index;
      { ITERATE(x);
        for (START(l); NEXT(x);)
        PUSH(x);
        } 
      Result = stack_apply_function(self,ls,start,ClEnv->index);
      } 
    return (Result);} 
  } 


/* The c++ function for: call(p:property,l:listargs) [NEW_ALLOC+SLOT_UPDATE] */
OID  call_property(property *p,listargs *l)
{ return (apply_property(p,l));} 


/* The c++ function for: apply(p:property,l:list) [NEW_ALLOC+SLOT_UPDATE] */
OID  apply_property(property *p,list *l)
{ { OID Result = 0;
    { int  start = ClEnv->index;
      { ITERATE(x);
        for (START(l); NEXT(x);)
        PUSH(x);
        } 
      Result = eval_message_property(p,find_which_property(p,start,OWNER((*(l))[1])),start,CTRUE);
      } 
    return (Result);} 
  } 


/* The c++ function for: apply(m:method,l:list) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
OID  apply_method(method *m,list *l)
{ { OID Result = 0;
    { int  start = ClEnv->index;
      { ITERATE(x);
        for (START(l); NEXT(x);)
        PUSH(x);
        } 
      Result = execute_method(m,start,CFALSE);
      } 
    return (Result);} 
  } 


// push and pop debug info on the stack
// this method also does the tracing and the steppping
// NOTE: self should be either a property or a restriction
/* The c++ function for: push_debug(prop:property,arity:integer,start:integer) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
void  push_debug_property(property *prop,int arity,int start)
{ GC_BIND;
  { int  i = ClEnv->index;
    int  n = ClEnv->trace_I;
    if ((n > 0) && 
        (((prop->trace_I+ClEnv->verbose) > 4) || 
            (n == ClEnv->step_I)))
     { ClairePort * p = use_as_output_port(ClEnv->ctrace);
      (ClEnv->trace_I = 0);
      tr_indent_boolean(CFALSE,n);
      princ_string(" ");
      print_any(_oid_(prop));
      princ_string("(");
      print_any(GC_OID(ClEnv->stack[start]));
      princ_string("");
      { int  j = (start+1);
        { OID gc_local;
          while ((j < (start+arity)))
          { GC_LOOP;
            princ_string(",");
            print_any(GC_OID(ClEnv->stack[j]));
            princ_string("");
            ++j;
            GC_UNLOOP;} 
          } 
        } 
      if (1000 <= prop->trace_I)
       (ClEnv->step_I = n);
      if (0 <= ClEnv->count_call)
       { (ClEnv->count_call = (ClEnv->count_call+1));
        princ_string(" [");
        princ_integer(ClEnv->count_call);
        princ_string("]");
        if (ClEnv->count_call == ClEnv->count_level)
         { if (ClEnv->count_trigger == _oid_(Core.call_step))
           (ClEnv->step_I = n);
          else if (ClEnv->count_trigger == _oid_(Core.spy))
           update_property(Kernel.spy_I,
            ClEnv,
            18,
            Kernel._object,
            GC_OID(_oid_(_at_property1(Core.spy,Kernel._void))));
          else (ClEnv->verbose = ClEnv->count_trigger);
            } 
        } 
      if (n == ClEnv->step_I)
       (*Core.call_step)(_oid_(prop));
      else princ_string(")\n");
        (ClEnv->trace_I = (n+1));
      use_as_output_port(p);
      } 
    else if ((n > 0) && 
        (ClEnv->step_I > 0))
     { if (0 <= ClEnv->count_call)
       (ClEnv->count_call = (ClEnv->count_call+1));
      (ClEnv->trace_I = (n+1));
      } 
    if (get_table(Core.StopProperty,_oid_(prop)) != CNULL)
     { { ClaireBoolean * g0046I;
        { ClaireBoolean *v_or;
          { v_or = ((equal(nth_table1(Core.StopProperty,_oid_(prop)),Core.nil->value) == CTRUE) ? CTRUE : CFALSE);
            if (v_or == CTRUE) g0046I =CTRUE; 
            else { { OID  g0047UU;
                { OID gc_local;
                  ITERATE(l2);
                  g0047UU= _oid_(CFALSE);
                  bag *l2_support;
                  l2_support = OBJECT(bag,nth_table1(Core.StopProperty,_oid_(prop)));
                  for (START(l2_support); NEXT(l2);)
                  { GC_LOOP;
                    { ClaireBoolean * g0048I;
                      { OID  g0049UU;
                        { int  j = 1;
                          int  g0045 = (*Kernel.length)(l2);
                          { OID gc_local;
                            g0049UU= _oid_(CFALSE);
                            while ((j <= g0045))
                            { GC_LOOP;
                              if ((((j+start) <= i) ? ((equal(GC_OID((*Kernel.nth)(l2,
                                j)),GC_OID(ClEnv->stack[((start+j)-1)])) == CTRUE) ? CTRUE: CFALSE): CFALSE) != CTRUE)
                               { g0049UU = Kernel.ctrue;
                                break;} 
                              ++j;
                              GC_UNLOOP;} 
                            } 
                          } 
                        g0048I = not_any(g0049UU);
                        } 
                      
                      if (g0048I == CTRUE) { g0047UU = Kernel.ctrue;
                          break;} 
                        } 
                    GC_UNLOOP;} 
                  } 
                v_or = boolean_I_any(g0047UU);
                } 
              if (v_or == CTRUE) g0046I =CTRUE; 
              else g0046I = CFALSE;} 
            } 
          } 
        
        if (g0046I == CTRUE) close_exception(((general_error *) (*Core._general_error)(_string_("stop as required in ~S(~A)"),
            _oid_(list::alloc(2,_oid_(prop),GC_OID(_oid_(get_args_integer(start))))))));
          } 
      } 
    PUSH(ClEnv->debug_I);
    PUSH(_oid_(prop));
    PUSH(arity);
    PUSH(start);
    (ClEnv->debug_I = i);
    } 
  GC_UNBIND;} 


// value of the previous debug
// n is 0 for interpreted code and 1 for compiled code
/* The c++ function for: pop_debug(self:property,n:integer,val:any) [0] */
void  pop_debug_property(property *self,int n,OID val)
{ GC_BIND;
  { int  v = ClEnv->debug_I;
    if (v > 0)
     { if (n != 0)
       (ClEnv->index= ClEnv->stack[(v+3)]);
      (ClEnv->debug_I = ClEnv->stack[ClEnv->debug_I]);
      if (self->if_write == CNULL)
       { ClaireObject * m = GC_OBJECT(ClaireObject,ClEnv->spy_I);
        if (m != NULL)
         { (ClEnv->spy_I = NULL);
          funcall_method1(((method *) m),_oid_(ClEnv));
          (ClEnv->spy_I = m);
          } 
        } 
      if ((ClEnv->trace_I > 1) && 
          (((self->trace_I+ClEnv->verbose) > 4) || 
              (ClEnv->step_I != 0)))
       { int  i = ClEnv->trace_I;
        (ClEnv->trace_I = 0);
        if ((self->trace_I+ClEnv->verbose) > 4)
         { ClairePort * p = use_as_output_port(ClEnv->ctrace);
          tr_indent_boolean(CTRUE,(i-1));
          princ_string(" ");
          print_any(val);
          princ_string("\n");
          use_as_output_port(p);
          } 
        if (i <= ClEnv->step_I)
         (ClEnv->step_I = (i-1));
        (ClEnv->trace_I = (i-1));
        if (ClEnv->trace_I == 1)
         (ClEnv->step_I = 0);
        } 
      } 
    } 
  GC_UNBIND;} 


// print a nice indented mark
/* The c++ function for: tr_indent(return?:boolean,n:integer) [0] */
void  tr_indent_boolean(ClaireBoolean *return_ask,int n)
{ if (return_ask == CTRUE)
   { princ_string("[");
    princ_integer(n);
    princ_string("]");
    } 
  else { princ_integer(n);
      princ_string(":=");
      } 
    { while ((n > 9))
    { princ_string("=");
      n= (n-10);
      } 
    } 
  { while ((n > 0))
    { princ_string(">");
      n= (n-1);
      } 
    } 
  } 


// *********************************************************************
// *   Part 2: Tables                                                  *
// *********************************************************************
// finds if objects are identified
/* The c++ function for: identified?(self:class) [0] */
ClaireBoolean * identified_ask_class(ClaireClass *self)
{ return (((self == Kernel._integer) ? CTRUE : ((INHERIT(self,Kernel._object)) ? CTRUE : ((self == Kernel._symbol) ? CTRUE : ((self == Kernel._boolean) ? CTRUE : ((self == Kernel._char) ? CTRUE : CFALSE))))));} 


/* The c++ function for: identical?(x:any,y:any) [0] */
ClaireBoolean * identical_ask_any(OID x,OID y)
{ return (((ClaireBoolean *) ((x == y) ? CTRUE : CFALSE)));} 


//  let x1: (if ((x as boolean) = (y as boolean)) true else false)
// writing a single value into a slot but does NOT trigger the rules !
// equivalent to is! of LAURE
// this definition should not be placed in the method.cl file
// (it requires some inheritance conflict processing)
/* The c++ function for: put(self:property,x:object,y:any) [NEW_ALLOC+SLOT_UPDATE+RETURN_ARG] */
OID  put_property2(property *self,ClaireObject *x,OID y)
{ GC_BIND;
  { OID Result = 0;
    { ClaireObject * s = GC_OBJECT(ClaireObject,_at_property1(self,OWNER(_oid_(x))));
      if (Kernel._slot == s->isa)
       Result = store_object(x,
        CLREAD(slot,s,index),
        CLREAD(slot,s,srange),
        y,
        self->store_ask);
      else { OID  V_CL0050;close_exception(((selector_error *) (*Core._selector_error)(_oid_(self),
            _oid_(list::alloc(1,_oid_(x))))));
          
          Result=_void_(V_CL0050);} 
        } 
    GC_UNBIND; return (Result);} 
  } 


// v3.2 : same but multi valued
/* The c++ function for: add_value(self:property,x:object,y:any) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
void  add_value_property3(property *self,ClaireObject *x,OID y)
{ GC_BIND;
  { ClaireObject * s = GC_OBJECT(ClaireObject,_at_property1(self,OWNER(_oid_(x))));
    if (boolean_I_any(_oid_(s)) != CTRUE)
     close_exception(((selector_error *) (*Core._selector_error)(_oid_(self),
      _oid_(list::alloc(1,_oid_(x))))));
    else if (multi_ask_any(_oid_(self)) != CTRUE)
     close_exception(((general_error *) (*Core._general_error)(_string_("[134] Cannot apply add to ~S"),
      _oid_(list::alloc(1,_oid_(self))))));
    else { int  n = CLREAD(slot,s,index);
        bag * l1 = OBJECT(bag,slot_get_object(x,n,Kernel._object));
        add_value_property(self,
          x,
          n,
          l1,
          y);
        } 
      } 
  GC_UNBIND;} 


// access
/* The c++ function for: nth(a:table,x:any) [0] */
OID  nth_table1(table *a,OID x)
{ { OID Result = 0;
    { OID  p = a->params;
      if (belong_to(x,_oid_(a->domain)) != CTRUE)
       close_exception(((general_error *) (*Core._general_error)(_string_("[135] ~S does not belong to the domain of ~S"),
        _oid_(list::alloc(2,x,_oid_(a))))));
      { OID  v;
        if (INHERIT(OWNER(p),Kernel._integer))
         v = (*(a->graph))[((x)-p)];
        else if (INHERIT(OWNER(p),Kernel._list))
         v = (*(a->graph))[get_index_table2(a,(*(OBJECT(list,x)))[1],(*(OBJECT(list,x)))[2])];
        else { int  i = index_table(a,x);
            v = (*(a->graph))[i];
            } 
          if ((v != CNULL) || 
            (belong_to(v,_oid_(a->range)) == CTRUE))
         Result = v;
        else { OID  V_CL0051;close_exception(((general_error *) (*Core._general_error)(_string_("[138] the value ~S(~S) is unknown !"),
              _oid_(list::alloc(2,_oid_(a),x)))));
            
            Result=_void_(V_CL0051);} 
          } 
      } 
    return (Result);} 
  } 


/* The c++ function for: nth_table1_type */
ClaireType * nth_table1_type(ClaireType *a,ClaireType *x)
{ GC_BIND;
  { ClaireType *Result ;
    if (unique_ask_type(a) == CTRUE)
     Result = OBJECT(ClaireType,(*Kernel.range)(GC_OID(the_type(a))));
    else Result = Kernel._any;
      GC_UNBIND; return (Result);} 
  } 


/* The c++ function for: get(a:table,x:any) [RETURN_ARG] */
OID  get_table(table *a,OID x)
{ { OID Result = 0;
    { OID  p = a->params;
      if (belong_to(x,_oid_(a->domain)) != CTRUE)
       Result = CNULL;
      else { int  i = get_index_table1(a,x);
          Result = (*(a->graph))[i];
          } 
        } 
    return (Result);} 
  } 


/* The c++ function for: get_table_type */
ClaireType * get_table_type(ClaireType *a,ClaireType *x)
{ GC_BIND;
  { ClaireType *Result ;
    if (unique_ask_type(a) == CTRUE)
     Result = U_type(GC_OBJECT(ClaireType,OBJECT(ClaireType,(*Kernel.range)(GC_OID(the_type(a))))),set::alloc(Kernel.emptySet,1,CNULL));
    else Result = Kernel._any;
      GC_UNBIND; return (Result);} 
  } 


// interface update method for a[x] := y
/* The c++ function for: nth=(a:table,x:any,y:any) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
void  nth_equal_table1(table *a,OID x,OID y)
{ if (belong_to(x,_oid_(a->domain)) != CTRUE)
   close_exception(((general_error *) (*Core._general_error)(_string_("[135] ~S does not belong to the domain of ~S"),
    _oid_(list::alloc(2,x,_oid_(a))))));
  if (belong_to(y,_oid_(a->range)) != CTRUE)
   close_exception(((range_error *) (*Core._range_error)(_oid_(a),
    y,
    _oid_(a->range))));
  nth_put_table(a,x,y);
  } 


// internal form without checks
// equivalent of update = put + put_inverse
/* The c++ function for: nth_put(a:table,x:any,y:any) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
void  nth_put_table(table *a,OID x,OID y)
{ GC_BIND;
  if ((((a->if_write == CNULL) ? CTRUE : CFALSE) != CTRUE) && 
      (multi_ask_any(_oid_(a)) != CTRUE))
   fastcall_relation2(a,x,y);
  else if (multi_ask_any(_oid_(a)) == CTRUE)
   { OID  r = GC_OID(get_property(Kernel.inverse,a));
    OID  old = get_table(a,x);
    { OID  g0052UU;
      if (OBJECT(set,y)->length == 0)
       g0052UU = y;
      else if (a->multivalued_ask == Kernel._list)
       g0052UU = _oid_(make_list_integer2(0,of_extract_type(a->range),0));
      else g0052UU = _oid_(cast_I_bag(set::empty(),of_extract_type(a->range)));
        ((*(((list *) a->graph)))[get_index_table1(a,x)]=g0052UU);
      } 
    if ((old != CNULL) && 
        (r != CNULL))
     { OID gc_local;
      ITERATE(z);
      bag *z_support;
      z_support = GC_OBJECT(bag,enumerate_any(old));
      for (START(z_support); NEXT(z);)
      update_dash_relation(OBJECT(ClaireRelation,r),z,x);
      } 
    { OID gc_local;
      ITERATE(z);
      for (START(OBJECT(set,y)); NEXT(z);)
      add_I_table(a,x,z);
      } 
    } 
  else { OID  r = GC_OID(get_property(Kernel.inverse,a));
      OID  z = get_table(a,x);
      if (equal(z,y) != CTRUE)
       { if (r != CNULL)
         { OID  z = get_table(a,x);
          if ((z != CNULL) && 
              ((r != _oid_(a)) || 
                  (equal(x,z) != CTRUE)))
           update_dash_relation(OBJECT(ClaireRelation,r),z,x);
          } 
        put_table(a,x,y);
        update_plus_relation(a,x,y);
        } 
      } 
    GC_UNBIND;} 


// put does NOT update the inverse
/* The c++ function for: put(a:table,x:any,y:any) [BAG_UPDATE] */
void  put_table(table *a,OID x,OID y)
{ { OID  p = a->params;
    OID  z = get_table(a,x);
    if (equal(z,y) != CTRUE)
     { if (INHERIT(OWNER(p),Kernel._integer))
       store_list(((list *) a->graph),((x)-p),y,a->store_ask);
      else if (INHERIT(OWNER(p),Kernel._list))
       store_list(((list *) a->graph),get_index_table2(a,(*(OBJECT(list,x)))[1],(*(OBJECT(list,x)))[2]),y,a->store_ask);
      else { int  i = index_table(a,x);
          store_list(((list *) a->graph),i,y,a->store_ask);
          } 
        ;} 
    } 
  } 


// adds a value to a multi-valued table: interface method
/* The c++ function for: add(a:table,x:any,y:any) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
void  add_table(table *a,OID x,OID y)
{ if (belong_to(x,_oid_(a->domain)) != CTRUE)
   close_exception(((general_error *) (*Core._general_error)(_string_("[135] ~S does not belong to the domain of ~S"),
    _oid_(list::alloc(2,x,_oid_(a))))));
  if (belong_to(y,_oid_(member_type(a->range))) != CTRUE)
   close_exception(((range_error *) (*Core._range_error)(_oid_(a),
    y,
    _oid_(a->range))));
  add_I_table(a,x,y);
  } 


// adds a value to a multi-valued table: internal version without type checks
/* The c++ function for: add!(a:table,x:any,y:any) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
void  add_I_table(table *a,OID x,OID y)
{ if (((a->if_write == CNULL) ? CTRUE : CFALSE) != CTRUE)
   fastcall_relation2(a,x,y);
  else { OID  p = a->params;
      int  i = get_index_table1(a,x);
      bag * l = OBJECT(bag,(*(a->graph))[i]);
      if (add_value_array(a,i,l,y) == CTRUE)
       update_plus_relation(a,x,y);
      } 
    } 


// this methods adds a value to a multi-slot (used by the compiler)
/* The c++ function for: add_value(self:table,n:integer,l:bag,y:any) [NEW_ALLOC+BAG_UPDATE] */
ClaireBoolean * add_value_array(table *self,int n,bag *l,OID y)
{ GC_BIND;
  { ClaireBoolean *Result ;
    if (self->multivalued_ask == CTRUE)
     { if (belong_to(y,_oid_(l)) != CTRUE)
       { set * l1 = GC_OBJECT(set,((set *) ((self->store_ask == CTRUE) ?
          copy_bag(l) :
          l ))->addFast(y));
        store_list(((list *) self->graph),n,_oid_(l1),self->store_ask);
        Result = CTRUE;
        } 
      else Result = CFALSE;
        } 
    else { list * l1 = GC_OBJECT(list,((self->store_ask == CTRUE) ?
          store_add(((list *) l),y) :
          ((list *) l)->addFast(y) ));
        store_list(((list *) self->graph),n,_oid_(l1),self->store_ask);
        Result = CTRUE;
        } 
      GC_UNBIND; return (Result);} 
  } 


// a direct version (v3.2) that can be used in lieu of add!
/* The c++ function for: add_value(self:table,x:any,y:any) [NEW_ALLOC+BAG_UPDATE] */
void  add_value_table3(table *self,OID x,OID y)
{ { OID  p = self->params;
    int  i = get_index_table1(self,x);
    bag * l = OBJECT(bag,(*(self->graph))[i]);
    add_value_array(self,i,l,y);
    } 
  } 


// removes a value from an table
/* The c++ function for: delete(a:table,x:any,y:any) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
OID  delete_table(table *a,OID x,OID y)
{ { OID Result = 0;
    { OID  p = a->params;
      int  i = get_index_table1(a,x);
      set * l1 = OBJECT(set,(*(a->graph))[i]);
      set * l;
      { bag *V_CC ;
        { bag * g0053UU;
          if (a->store_ask == CTRUE)
           g0053UU = copy_bag(l1);
          else g0053UU = l1;
            V_CC = delete_bag(g0053UU,y);
          } 
        l= (set *) V_CC;} 
      store_list(((list *) a->graph),i,_oid_(l),a->store_ask);
      { ClaireRelation * r = a->inverse;
        if (r != NULL)
         update_dash_relation(r,y,x);
        } 
      Result = _oid_(l);
      } 
    return (Result);} 
  } 


// direct access to 2-dim tables
/* The c++ function for: nth(a:table,x:any,y:any) [0] */
OID  nth_table2(table *a,OID x,OID y)
{ { OID Result = 0;
    { OID  p = a->params;
      OID  v;
      if (INHERIT(OWNER(p),Kernel._list))
       { if (((belong_to(x,(*(((bag *) a->domain)))[1]) == CTRUE) ? ((belong_to(y,(*(((bag *) a->domain)))[2]) == CTRUE) ? CTRUE: CFALSE): CFALSE) != CTRUE)
         close_exception(((general_error *) (*Core._general_error)(_string_("[135] ~S does not belong to the domain of ~S"),
          _oid_(list::alloc(2,x,_oid_(a))))));
        v = (*(a->graph))[get_index_table2(a,x,y)];
        } 
      else v = index_table2(a,x,y);
        if ((v != CNULL) || 
          (belong_to(v,_oid_(a->range)) == CTRUE))
       Result = v;
      else { OID  V_CL0054;close_exception(((general_error *) (*Core._general_error)(_string_("~S(~S) is unknown !"),
            _oid_(list::alloc(2,_oid_(a),x)))));
          
          Result=_void_(V_CL0054);} 
        } 
    return (Result);} 
  } 


/* The c++ function for: nth_table2_type */
ClaireType * nth_table2_type(ClaireType *a,ClaireType *x,ClaireType *y)
{ GC_BIND;
  { ClaireType *Result ;
    if (unique_ask_type(a) == CTRUE)
     Result = OBJECT(ClaireType,(*Kernel.range)(GC_OID(the_type(a))));
    else Result = Kernel._any;
      GC_UNBIND; return (Result);} 
  } 


// sets a value in a 2-dim table
/* The c++ function for: nth=(a:table,x:any,y:any,z:any) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
void  nth_equal_table2(table *a,OID x,OID y,OID z)
{ { OID  p = a->params;
    if (INHERIT(OWNER(p),Kernel._list))
     { if (((belong_to(x,(*(((bag *) a->domain)))[1]) == CTRUE) ? ((belong_to(y,(*(((bag *) a->domain)))[2]) == CTRUE) ? CTRUE: CFALSE): CFALSE) != CTRUE)
       close_exception(((general_error *) (*Core._general_error)(_string_("[135] ~S does not belong to the domain of ~S"),
        _oid_(list::alloc(2,_oid_(list::alloc(2,x,y)),_oid_(a))))));
      if (belong_to(z,_oid_(a->range)) != CTRUE)
       close_exception(((range_error *) (*Core._range_error)(_oid_(a),
        z,
        _oid_(a->range))));
      if ((((a->inverse == (NULL)) ? CTRUE : CFALSE) != CTRUE) || 
          (((a->if_write == CNULL) ? CTRUE : CFALSE) != CTRUE))
       nth_put_table(a,_oid_(list::alloc(2,x,y)),z);
      else store_list(((list *) a->graph),get_index_table2(a,x,y),z,a->store_ask);
        } 
    else nth_equal_table1(a,_oid_(tuple::alloc(2,x,y)),z);
      } 
  } 


// v3.2.16 tuple(a,b) is not list(a,b) !
/* The c++ function for: get_index(a:table,x:any) [RETURN_ARG] */
int  get_index_table1(table *a,OID x)
{ { int Result = 0;
    { OID  p = a->params;
      Result = ((INHERIT(OWNER(p),Kernel._integer)) ?
        ((x)-p) :
        ((INHERIT(OWNER(p),Kernel._list)) ?
          get_index_table2(a,(*(OBJECT(list,x)))[1],(*(OBJECT(list,x)))[2]) :
          index_table(a,x) ) );
      } 
    return (Result);} 
  } 


/* The c++ function for: get_index(a:table,x:integer,y:integer) [0] */
int  get_index_table2(table *a,int x,int y)
{ { int Result = 0;
    { list * p = OBJECT(list,a->params);
      Result = (((((*(p))[1])*x)+y)-((*(p))[2]));
      } 
    return (Result);} 
  } 


// erase an table means to clean its graph so that it becomes empty.
/* The c++ function for: erase(a:table) [NEW_ALLOC+BAG_UPDATE+RETURN_ARG] */
void  erase_table(table *a)
{ GC_BIND;
  { OID  p = a->params;
    if (INHERIT(OWNER(p),Kernel._integer))
     { OID gc_local;
      ITERATE(i);
      bag *i_support;
      i_support = GC_OBJECT(bag,enumerate_any(_oid_(a->domain)));
      for (START(i_support); NEXT(i);)
      ((*(((list *) a->graph)))[get_index_table1(a,i)]=a->DEFAULT);
      } 
    else if (INHERIT(OWNER(p),Kernel._list))
     { OID gc_local;
      ITERATE(l);
      bag *l_support;
      l_support = GC_OBJECT(bag,enumerate_any(_oid_(a->domain)));
      for (START(l_support); NEXT(l);)
      ((*(((list *) a->graph)))[get_index_table2(a,(*(OBJECT(bag,l)))[1],(*(OBJECT(bag,l)))[2])]=a->DEFAULT);
      } 
    else { int  i = 1;
        int  g0055 = a->graph->length;
        { OID gc_local;
          while ((i <= g0055))
          { ((*(((list *) a->graph)))[i]=CNULL);
            ++i;
            } 
          } 
        } 
      } 
  GC_UNBIND;} 


// new in v3.2.50 a constructor for building a table dynamically
/* The c++ function for: make_table(%domain:type,%range:type,%default:any) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
table * make_table_type(ClaireType *_Zdomain,ClaireType *_Zrange,OID _Zdefault)
{ { table *Result ;
    { table * t = ((table *) new_object_class(Kernel._table));
      (t->range = _Zrange);
      add_I_property(Kernel.instances,Kernel._table,11,_oid_(t));
      (t->domain = _Zdomain);
      (t->DEFAULT = _Zdefault);
      (t->params = _oid_(Kernel._any));
      (t->graph = make_list_integer(29,CNULL));
      Result = t;
      } 
    return (Result);} 
  } 


// Our first table: a debuging tool which stores a list of stopping values
// *********************************************************************
//   Part 3: Demons & relations for the logic modules                  *
// *********************************************************************
// applying a lambda to one argument
/* The c++ function for: funcall(self:lambda,x:any) [NEW_ALLOC] */
OID  funcall_lambda1(lambda *self,OID x)
{ GC_BIND;
  { OID Result = 0;
    { int  start = ClEnv->index;
      int  retour = ClEnv->base;
      (ClEnv->base= start);
      PUSH(x);
      stack_add(self->dimension);
      { OID  val = GC_OID(OPT_EVAL(self->body));
        (ClEnv->base= retour);
        (ClEnv->index= start);
        Result = val;
        } 
      } 
    GC_UNBIND; return (Result);} 
  } 


// applying a lambda to two argument
/* The c++ function for: funcall(self:lambda,x:any,y:any) [NEW_ALLOC] */
OID  funcall_lambda2(lambda *self,OID x,OID y)
{ GC_BIND;
  { OID Result = 0;
    { int  start = ClEnv->index;
      int  retour = ClEnv->base;
      (ClEnv->base= start);
      PUSH(x);
      PUSH(y);
      stack_add(self->dimension);
      { OID  val = GC_OID(OPT_EVAL(self->body));
        (ClEnv->base= retour);
        (ClEnv->index= start);
        Result = val;
        } 
      } 
    GC_UNBIND; return (Result);} 
  } 


// applying a lambda to two argument
/* The c++ function for: funcall(self:lambda,x:any,y:any,z:any) [NEW_ALLOC] */
OID  funcall_lambda3(lambda *self,OID x,OID y,OID z)
{ GC_BIND;
  { OID Result = 0;
    { int  start = ClEnv->index;
      int  retour = ClEnv->base;
      (ClEnv->base= start);
      PUSH(x);
      PUSH(y);
      PUSH(z);
      stack_add(self->dimension);
      { OID  val = GC_OID(OPT_EVAL(self->body));
        (ClEnv->base= retour);
        (ClEnv->index= start);
        Result = val;
        } 
      } 
    GC_UNBIND; return (Result);} 
  } 


// for historical reasons
// dealing with inverse
/* The c++ function for: check_inverse(%r1:any,%r2:any) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
void  check_inverse_any(OID _Zr1,OID _Zr2)
{ GC_BIND;
  { ClaireRelation * r1 = OBJECT(ClaireRelation,_Zr1);
    ClaireRelation * r2 = OBJECT(ClaireRelation,_Zr2);
    (r1->inverse = r2);
    (r2->inverse = r1);
    final_relation(r1);
    final_relation(r2);
    if ((_inf_equal_type(r1->domain,GC_OBJECT(ClaireType,((multi_ask_any(_oid_(r2)) == CTRUE) ?
        member_type(r2->range) :
        r2->range ))) != CTRUE) || 
        (_inf_equal_type(r2->domain,GC_OBJECT(ClaireType,((multi_ask_any(_oid_(r1)) == CTRUE) ?
          member_type(r1->range) :
          r1->range ))) != CTRUE))
     close_exception(((general_error *) (*Core._general_error)(_string_("[137] ~S and ~S cannot be inverses for one another"),
      _oid_(list::alloc(2,_oid_(r1),_oid_(r2))))));
    } 
  GC_UNBIND;} 


// very useful
/* The c++ function for: invert(r:relation,x:any) [NEW_ALLOC] */
bag * invert_relation(ClaireRelation *r,OID x)
{ GC_BIND;
  { bag *Result ;
    { ClaireObject *V_CC ;
      { OID  r2 = GC_OID(get_property(Kernel.inverse,r));
        if (INHERIT(OWNER(r2),Kernel._table))
         { OID  v = nth_table1(OBJECT(table,r2),x);
          if (OBJECT(ClaireRelation,r2)->multivalued_ask != CFALSE)
           V_CC = OBJECT(bag,v);
          else V_CC = set::alloc(1,v);
            } 
        else if (INHERIT(OWNER(r2),Kernel._property))
         { OID  v = GC_OID(get_property(OBJECT(property,r2),OBJECT(ClaireObject,x)));
          if (OBJECT(ClaireRelation,r2)->multivalued_ask != CFALSE)
           V_CC = OBJECT(bag,v);
          else V_CC = set::alloc(1,v);
            } 
        else if (INHERIT(r->isa,Kernel._property))
         { if (r->multivalued_ask != CFALSE)
           { set * z_out = set::empty(Kernel.emptySet);
            { OID gc_local;
              ITERATE(z);
              bag *z_support;
              z_support = GC_OBJECT(bag,enumerate_any(_oid_(r->domain)));
              for (START(z_support); NEXT(z);)
              if (belong_to(x,get_property(((property *) r),OBJECT(ClaireObject,z))) == CTRUE)
               z_out->addFast(z);
              } 
            V_CC = GC_OBJECT(set,z_out);
            } 
          else { set * z_out = set::empty(Kernel.emptySet);
              { OID gc_local;
                ITERATE(z);
                bag *z_support;
                z_support = GC_OBJECT(bag,enumerate_any(_oid_(r->domain)));
                for (START(z_support); NEXT(z);)
                if (equal(get_property(((property *) r),OBJECT(ClaireObject,z)),x) == CTRUE)
                 z_out->addFast(z);
                } 
              V_CC = GC_OBJECT(set,z_out);
              } 
            } 
        else if (INHERIT(r->isa,Kernel._table))
         { if (r->multivalued_ask != CFALSE)
           { set * z_out = set::empty(Kernel.emptySet);
            { OID gc_local;
              ITERATE(z);
              bag *z_support;
              z_support = GC_OBJECT(bag,enumerate_any(_oid_(r->domain)));
              for (START(z_support); NEXT(z);)
              if (belong_to(x,nth_table1(((table *) r),z)) == CTRUE)
               z_out->addFast(z);
              } 
            V_CC = GC_OBJECT(set,z_out);
            } 
          else { set * z_out = set::empty(Kernel.emptySet);
              { OID gc_local;
                ITERATE(z);
                bag *z_support;
                z_support = GC_OBJECT(bag,enumerate_any(_oid_(r->domain)));
                for (START(z_support); NEXT(z);)
                if (equal(nth_table1(((table *) r),z),x) == CTRUE)
                 z_out->addFast(z);
                } 
              V_CC = GC_OBJECT(set,z_out);
              } 
            } 
        else V_CC = CFALSE;
          } 
      Result= (bag *) V_CC;} 
    GC_UNBIND; return (Result);} 
  } 


// same: two useful methods that are used often
/* The c++ function for: domain!(x:restriction) [NEW_ALLOC+RETURN_ARG] */
ClaireClass * domain_I_restriction(restriction *x)
{ return (class_I_type(OBJECT(ClaireType,(*(x->domain))[1])));} 


/* The c++ function for: methods(d:class,r:class) [NEW_ALLOC] */
set * methods_class(ClaireClass *d,ClaireClass *r)
{ GC_BIND;
  { set *Result ;
    { set * m_out = set::empty(Kernel.emptySet);
      { ITERATE(m);
        bag *m_support;
        m_support = Kernel._method->instances;
        for (START(m_support); NEXT(m);)
        if ((_inf_equal_type(OBJECT(ClaireType,(*(OBJECT(restriction,m)->domain))[1]),d) == CTRUE) && 
            (_inf_equal_type(OBJECT(restriction,m)->range,r) == CTRUE))
         m_out->addFast(m);
        } 
      Result = GC_OBJECT(set,m_out);
      } 
    GC_UNBIND; return (Result);} 
  } 


// sets the reified flag
/* The c++ function for: reify(l:listargs) [SLOT_UPDATE+RETURN_ARG] */
void  reify_listargs(listargs *l)
{ { ITERATE(p);
    for (START(l); NEXT(p);)
    if (INHERIT(OWNER(p),Kernel._property))
     (OBJECT(property,p)->reified = CTRUE);
    } 
  } 


// *********************************************************************
// *   Part 4: Basics of Exceptions                                    *
// *********************************************************************
// a generic error that is produced by the error(" ....") instruction
/* The c++ function for: self_print(self:general_error) [NEW_ALLOC+SLOT_UPDATE] */
void  self_print_general_error_Core(general_error *self)
{ GC_BIND;
  princ_string("**** An error has occurred.\n");
  format_string(GC_STRING(string_v(self->cause)),GC_OBJECT(list,OBJECT(list,self->arg)));
  princ_string("\n");
  GC_UNBIND;} 


// a read_slot error is produced when an unknown value is found
/* The c++ function for: self_print(self:read_slot_error) [NEW_ALLOC] */
void  self_print_read_slot_error_Core(read_slot_error *self)
{ GC_BIND;
  princ_string("****[138] The value of ");
  print_any(GC_OID(self->wrong));
  princ_string("(");
  print_any(GC_OID(self->arg));
  princ_string(") is unknown");
  GC_UNBIND;} 


// range errors
/* The c++ function for: self_print(self:range_error) [NEW_ALLOC] */
void  self_print_range_error_Core(range_error *self)
{ GC_BIND;
  princ_string("****[139] ");
  print_any(GC_OID(self->cause));
  princ_string(": range error, ");
  print_any(GC_OID(self->arg));
  princ_string(" does not belong? to ");
  print_any(GC_OID(self->wrong));
  princ_string(".\n");
  GC_UNBIND;} 


// selector errors
/* The c++ function for: self_print(self:selector_error) [NEW_ALLOC] */
void  self_print_selector_error_Core(selector_error *self)
{ GC_BIND;
  { OID  p = GC_OID(self->selector);
    if (boolean_I_any(_oid_(OBJECT(property,p)->restrictions)) != CTRUE)
     { princ_string("[140] The property ");
      print_any(p);
      princ_string(" is not defined (was applied to ");
      print_any(GC_OID(self->arg));
      princ_string(").\n");
      } 
    else { princ_string("****[141] ");
        print_any(GC_OID(self->arg));
        princ_string(" is a wrong arg list for ");
        print_any(p);
        princ_string(".\n");
        } 
      } 
  GC_UNBIND;} 


// produced by a return (usually trapped)
/* The c++ function for: self_print(self:return_error) [0] */
void  self_print_return_error_Core(return_error *self)
{ princ_string("****[142] return called outside of a loop (for or while).");
  } 


// interpretation of all the error codes
/* The c++ function for: self_print(self:system_error) [NEW_ALLOC+SLOT_UPDATE] */
void  self_print_system_error_Core(system_error *self)
{ GC_BIND;
  { int  n = self->index;
    princ_string("**** An internal error [");
    princ_integer(n);
    princ_string("] has occured:\n");
    { char * g0057UU;
      if (n == 1)
       g0057UU = "dynamic allocation, item is too big (~S)";
      else if (n == 2)
       g0057UU = "dynamic allocation, too large for available memory (~S)";
      else if (n == 3)
       g0057UU = "object allocation, too large for available memory (~S)";
      else if (n == 5)
       g0057UU = "nth[~S] outside of scope for ~S";
      else if (n == 7)
       g0057UU = "Skip applied on ~S with a negative argument ~S";
      else if (n == 8)
       g0057UU = "List operation: cdr(()) is undefined";
      else if (n == 9)
       g0057UU = "String buffer is full: ~S";
      else if (n == 10)
       g0057UU = "Cannot create an imported entity from NULL reference";
      else if (n == 11)
       g0057UU = "nth_string[~S]: string too short~S";
      else if (n == 12)
       g0057UU = "Symbol Table table full";
      else if (n == 13)
       g0057UU = "Cannot create a subclass for ~S [~A]";
      else if (n == 16)
       g0057UU = "Temporary output string buffer too small";
      else if (n == 17)
       g0057UU = "Bag Type Error: ~S cannot be added to ~S";
      else if (n == 18)
       g0057UU = "definition of ~S is in conflict with an object from ~S";
      else if (n == 19)
       g0057UU = "Integer overflow";
      else if (n == 20)
       g0057UU = "Integer arithmetic: division/modulo of ~A by 0";
      else if (n == 21)
       g0057UU = "Integer to character: ~S is a wrong value";
      else if (n == 22)
       g0057UU = "Cannote create a string with negative length ~S";
      else if (n == 23)
       g0057UU = "Not enough memory to instal claire";
      else if (n == 24)
       g0057UU = "execution stack is full [~A]";
      else if (n == 26)
       g0057UU = "Wrong usage of time counter [~A]";
      else if (n == 27)
       g0057UU = "internal garbage protection stack overflow";
      else if (n == 28)
       g0057UU = "the multivalued status of ~S is not compatible with ~S";
      else if (n == 29)
       g0057UU = "There is no module ~S";
      else if (n == 30)
       g0057UU = "Attempt to read a private symbol ~S";
      else if (n == 31)
       g0057UU = "External function not compiled yet";
      else if (n == 32)
       g0057UU = "Too many arguments (~S) for function ~S";
      else if (n == 33)
       g0057UU = "Exception handling: stack overflow";
      else if (n == 34)
       g0057UU = "User interrupt: EXECUTION ABORTED";
      else if (n == 35)
       g0057UU = "reading char '~S': wrong char: ~S";
      else if (n == 36)
       g0057UU = "cannot open file ~A";
      else if (n == 37)
       g0057UU = "world stack is full";
      else if (n == 38)
       g0057UU = "Undefined access to ~S";
      else if (n == 39)
       g0057UU = "cannot convert ~S to an integer";
      else if (n == 40)
       g0057UU = "integer multiplication overflow with ~S and ~S";
      else if (n == 41)
       g0057UU = "wrong NTH access on ~S and ~S";
      else if (n == 42)
       g0057UU = "Wrong array[~S] init value: ~S";
      else if (n == 43)
       g0057UU = "Defeasible addition on list ~S requires pre-allocation (size ~S)";
      else if (n == 50)
       g0057UU = "C++ imported error (~S) : ~S";
      else { (self->value = n);
          g0057UU = "What the hell is this ! [code: ~S^]";
          } 
        format_string(g0057UU,list::alloc(2,GC_OID(self->value),GC_OID(self->arg)));
      } 
    } 
  GC_UNBIND;} 


// contradictions are nice exceptions
/* The c++ function for: self_print(x:contradiction) [0] */
void  self_print_contradiction_Core(contradiction *x)
{ princ_string("A contradiction has occured.");
  } 


// the format method is used to print error messages (similar to a printf)
/* The c++ function for: format(self:string,larg:list) [NEW_ALLOC+SLOT_UPDATE] */
void  format_string(char *self,list *larg)
{ GC_RESERVE(1);  // HOHO v3.0.55 optim !
  { char * s = self;
    int  n = get_string(s,_char_('~'));
    list * l = ((list *) copy_bag(larg));
    { OID gc_local;
      while ((equal(n,0) != CTRUE))
      { GC_LOOP;
        { ClaireChar * m = _char_(s[(n+1) - 1]);
          if (n > 1)
           princ_string(substring_string(s,1,(n-1)));
          if ('A' == ((char) m->ascii))
           (*Kernel.princ)(car_list(l));
          else if ('S' == ((char) m->ascii))
           print_any(car_list(l));
          else if ('I' == ((char) m->ascii))
           close_exception(((general_error *) (*Core._general_error)(_string_("[143] ~I not allowed in format"),
            _oid_(list::alloc(1,CNULL)))));
          if (((char) m->ascii) != '%')
           l= skip_list(l,1);
          GC__STRING(s = substring_string(s,(n+2),1000), 1);
          n= get_string(s,_char_('~'));
          } 
        GC_UNLOOP;} 
      } 
    if (strlen(s) > 0)
     princ_string(s);
    } 
  GC_UNBIND;} 


// special version that prints in the trace port
/* The c++ function for: tformat(self:string,i:integer,l:list) [NEW_ALLOC+SLOT_UPDATE] */
OID  tformat_string(char *self,int i,list *l)
{ { OID Result = 0;
    if (i <= ClEnv->verbose)
     { ClairePort * p = use_as_output_port(ClEnv->ctrace);
      format_string(self,l);
      Result = ClAlloc->import(Kernel._port,(int *) use_as_output_port(p));
      } 
    else Result = Kernel.cfalse;
      return (Result);} 
  } 


// printing a bag without ( )
/* The c++ function for: princ(s:bag) [NEW_ALLOC+SLOT_UPDATE] */
void  princ_bag(bag *s)
{ { ClaireBoolean * f = CTRUE;
    { ITERATE(x);
      for (START(s); NEXT(x);)
      { if (f == CTRUE)
         f= CFALSE;
        else princ_string(",");
          print_any(x);
        } 
      } 
    } 
  } 


// a global variable is a named object with a special evaluation
// NOTE: we need to refine the scheme for global constants !
// GV are defeasible
/* The c++ function for: close(self:global_variable) [SAFE_RESULT] */
global_variable * close_global_variable(global_variable *self)
{ if (((self->value == CNULL) ? CTRUE : (((Kernel._set == self->range->isa) || 
      (belong_to(self->value,_oid_(self->range)) == CTRUE)) ? CTRUE : CFALSE)) != CTRUE)
   close_exception(((range_error *) (*Core._range_error)(self->value,
    _oid_(self),
    _oid_(self->range))));
  return (self);} 


// we create a spcial contraidiction that we shall reuse
// how to use it
/* The c++ function for: contradiction!(_CL_obj:void) [RETURN_ARG] */
void  contradiction_I_void()
{ close_exception(OBJECT(ClaireException,Core.contradiction_occurs->value));
  } 


// v0.01
// end of file