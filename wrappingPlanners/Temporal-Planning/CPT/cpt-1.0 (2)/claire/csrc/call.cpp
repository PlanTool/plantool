/***** CLAIRE Compilation of file c:\claire\v3.3\src\meta\call.cl 
         [version 3.3.4 / safety 5] Sat Oct 16 06:53:29 2004 *****/

#include <claire.h>
#include <Kernel.h>
#include <Core.h>
#include <Language.h>

//+-------------------------------------------------------------+
//| CLAIRE                                                      |
//| call.cl                                                     |
//| Copyright (C) 1994 - 2003 Yves Caseau. All Rights Reserved  |
//| cf. copyright info in file object.cl: about()               |
//+-------------------------------------------------------------+
// -----------------------------------------------------------------
// This file holds the definition of functional calls in CLAIRE
// -----------------------------------------------------------------`
// *********************************************************************
// * Contents                                                          *
// *      Part 1: the basic object messages                            *
// *      Part 2: Basic structures                                     *
// *      Part 3: Specialized structures                               *
// *      Part 4: Functions on instructions                            *
// *********************************************************************
// *********************************************************************
// *      Part 1: the basic object messages                            *
// *********************************************************************
// contains the last message that was evaluated
// messages in CLAIRE are called calls --------------------------------
//
/* The c++ function for: self_print(self:Call) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
void  self_print_Call_Language(Call *self)
{ GC_BIND;
  { int  _Zl = Core.pretty->index;
    property * _Zs = self->selector;
    list * _Za = GC_OBJECT(list,self->args);
    if ((INHERIT(_Zs->isa,Kernel._operation)) && 
        (_Za->length == 2))
     { (Core.pretty->index = (Core.pretty->index+2));
      printe_any((*(_Za))[1],_Zs);
      princ_string(" ");
      print_any(_oid_(_Zs));
      princ_string(" ");
      lbreak_void();
      printe_any((*(_Za))[2],_Zs);
      princ_string("");
      } 
    else if (_Zs == Kernel.nth)
     { if (_Za->length == 3)
       { printexp_any((*(_Za))[1],CFALSE);
        princ_string("[");
        print_any((*(_Za))[2]);
        princ_string(",");
        print_any((*(_Za))[3]);
        princ_string("]");
        } 
      else if (_Za->length == 1)
       { printexp_any((*(_Za))[1],CFALSE);
        princ_string("[]");
        } 
      else { printexp_any((*(_Za))[1],CFALSE);
          princ_string("[");
          if (_Za->length == 2)
           print_any((*(_Za))[2]);
          princ_string("]");
          } 
        } 
    else if ((_Zs == Kernel.nth_equal) && 
        (3 <= _Za->length))
     { OID  a = (*(_Za))[3];
      OID  o;
      if (INHERIT(OWNER(a),Language._Call))
       o = _oid_(OBJECT(Call,a)->selector);
      else o = Kernel.cfalse;
        if (_Za->length == 4)
       { printexp_any((*(_Za))[1],CFALSE);
        princ_string("[");
        print_any((*(_Za))[2]);
        princ_string(",");
        print_any(a);
        princ_string("] := ");
        lbreak_integer(2);
        print_any((*(_Za))[4]);
        princ_string("");
        } 
      else { ClaireBoolean * g0014I;
        { OID  g0015UU;
          if (INHERIT(OWNER(a),Language._Call))
           g0015UU = (*(OBJECT(Call,a)->args))[1];
          else g0015UU = Kernel.cfalse;
            g0014I = sugar_ask_any((*(_Za))[1],(*(_Za))[2],o,g0015UU);
          } 
        
        if (g0014I == CTRUE) { print_any((*(_Za))[1]);
            princ_string("[");
            print_any((*(_Za))[2]);
            princ_string("] :");
            print_any(o);
            princ_string(" ");
            lbreak_integer(2);
            print_any(GC_OID((*(OBJECT(bag,(*Core.args)(a))))[2]));
            princ_string("");
            } 
          else { print_any((*(_Za))[1]);
          princ_string("[");
          print_any((*(_Za))[2]);
          princ_string("] := ");
          lbreak_integer(2);
          print_any(a);
          princ_string("");
          } 
        } 
      } 
    else if ((_Zs == Language.assign) && 
        (INHERIT(OWNER((*(_Za))[1]),Kernel._property)))
     { OID  a = (*(_Za))[3];
      OID  o;
      if (INHERIT(OWNER(a),Language._Call))
       o = _oid_(OBJECT(Call,a)->selector);
      else o = Kernel.cfalse;
        { ClaireBoolean * g0016I;
        { OID  g0017UU;
          if (INHERIT(OWNER(a),Language._Call))
           g0017UU = (*(OBJECT(Call,a)->args))[1];
          else g0017UU = Kernel.cfalse;
            g0016I = sugar_ask_any((*(_Za))[1],(*(_Za))[2],o,g0017UU);
          } 
        
        if (g0016I == CTRUE) { print_any((*(_Za))[1]);
            princ_string("(");
            print_any((*(_Za))[2]);
            princ_string(") :");
            print_any(o);
            princ_string(" ");
            lbreak_integer(2);
            print_any(GC_OID((*(OBJECT(bag,(*Core.args)(a))))[2]));
            princ_string("");
            } 
          else { print_any((*(_Za))[1]);
          princ_string("(");
          print_any((*(_Za))[2]);
          princ_string(") := ");
          lbreak_integer(2);
          print_any((*(_Za))[3]);
          princ_string("");
          } 
        } 
      } 
    else if ((_Zs == Kernel.add) && 
        (INHERIT(OWNER((*(_Za))[1]),Kernel._property)))
     { print_any((*(_Za))[1]);
      princ_string("(");
      print_any((*(_Za))[2]);
      princ_string(") :add ");
      lbreak_integer(2);
      print_any((*(_Za))[3]);
      princ_string("");
      } 
    else if ((_Zs == Kernel._delete) && 
        (INHERIT(OWNER((*(_Za))[1]),Kernel._property)))
     { print_any((*(_Za))[1]);
      princ_string("(");
      print_any((*(_Za))[2]);
      princ_string(") :delete ");
      lbreak_integer(2);
      print_any((*(_Za))[3]);
      princ_string("");
      } 
    else if (((*(_Za))[1] == _oid_(ClEnv)) && 
        (_Za->length == 1))
     { print_any(_oid_(_Zs));
      princ_string("()");
      } 
    else { print_any(_oid_(_Zs));
        princ_string("(");
        set_level_void();
        printbox_bag2(_Za);
        princ_string(")");
        } 
      (Core.pretty->index = _Zl);
    } 
  GC_UNBIND;} 


/* The c++ function for: self_print(self:Call+) [NEW_ALLOC+SLOT_UPDATE] */
OID  self_print_Call_plus_Language(Call_plus *self)
{ GC_BIND;
  printexp_any((*(self->args))[1],CTRUE);
  princ_string(".");
  print_any(_oid_(self->selector));
  { OID Result = 0;
    princ_string("");
    GC_UNBIND; return (Result);} 
  } 


/* The c++ function for: self_eval(self:Call) [SAFE_GC] */
OID  self_eval_Call(Call *self)
{ { OID Result = 0;
    { int  start = ClEnv->index;
      property * p = self->selector;
      if (0 <= ClEnv->debug_I)
       (Language.LastCall->value= _oid_(self));
      { ITERATE(x);
        bag *x_support;
        x_support = self->args;
        for (START(x_support); NEXT(x);)
        PUSH(OPT_EVAL(x));
        } 
      { OID  rx = eval_message_property(p,find_which_property(p,start,OWNER(ClEnv->stack[start])),start,CTRUE);
        if (0 <= ClEnv->debug_I)
         (Language.LastCall->value= _oid_(self));
        Result = rx;
        } 
      } 
    return (Result);} 
  } 


/* The c++ function for: self_eval(self:Call+) [NEW_ALLOC+SLOT_UPDATE] */
OID  self_eval_Call_plus(Call_plus *self)
{ GC_BIND;
  { OID Result = 0;
    { property * p = self->selector;
      OID  x = GC_OID(OPT_EVAL((*(self->args))[1]));
      ClaireObject * s = GC_OBJECT(ClaireObject,_at_property1(p,OWNER(x)));
      if (equal(_oid_(OWNER(_oid_(s))),_oid_(Kernel._slot)) != CTRUE)
       { OID  V_CL0018;close_exception(((selector_error *) (*Core._selector_error)(_oid_(p),
          _oid_(list::alloc(1,x)))));
        
        Result=_void_(V_CL0018);} 
      else { OID  z = slot_get_object(OBJECT(ClaireObject,x),CLREAD(slot,s,index),CLREAD(slot,s,srange));
          if ((z != CNULL) || 
              (belong_to(z,(*Kernel.range)(_oid_(s))) == CTRUE))
           { int  n = ClEnv->trace_I;
            if ((n > 0) && 
                (((p->trace_I+ClEnv->verbose) > 4) || 
                    (n == ClEnv->step_I)))
             { (ClEnv->trace_I = 0);
              princ_string("read: ");
              print_any(_oid_(p));
              princ_string("(");
              print_any(x);
              princ_string(") = ");
              print_any(z);
              princ_string("\n");
              (ClEnv->trace_I = n);
              } 
            Result = z;
            } 
          else { OID  V_CL0019;close_exception(((read_slot_error *) (*Core._read_slot_error)(x,
                _oid_(p))));
              
              Result=_void_(V_CL0019);} 
            } 
        } 
    GC_UNBIND; return (Result);} 
  } 


// recursive printing of bicall
//
/* The c++ function for: printe(self:any,s:property) [NEW_ALLOC+SLOT_UPDATE+RETURN_ARG] */
void  printe_any(OID self,property *s)
{ if ((INHERIT(OWNER(self),Language._Call)) && ((INHERIT(OBJECT(Call,self)->selector->isa,Kernel._operation)) && 
      (OBJECT(Call,self)->args->length == 2)))
   { if (CTRUE == CTRUE)
     { princ_string("(");
      print_any(self);
      princ_string(")");
      } 
    else printexp_any(self,CTRUE);
      } 
  else printexp_any(self,CTRUE);
    } 


// tells if the sugar :op can be used
//
/* The c++ function for: sugar?(x:any,x2:any,o:any,a:any) [0] */
ClaireBoolean * sugar_ask_any(OID x,OID x2,OID o,OID a)
{ { ClaireBoolean *Result ;
    Result = ((INHERIT(OWNER(o),Kernel._operation)) ?
      ((INHERIT(OWNER(x),Kernel._property)) ?
        ((INHERIT(OWNER(a),Language._Call)) ?
          ((x == _oid_(OBJECT(Call,a)->selector)) ? ((equal((*(OBJECT(Call,a)->args))[1],x2) == CTRUE) ? CTRUE: CFALSE): CFALSE) :
          CFALSE ) :
        CFALSE ) :
      (((INHERIT(OWNER(o),Language._Variable)) || 
          (INHERIT(OWNER(o),Core._global_variable))) ?
        equal(x,a) :
        ((INHERIT(OWNER(a),Language._Call)) ?
          ((OBJECT(Call,a)->selector == Kernel.nth) ? ((equal((*(OBJECT(Call,a)->args))[1],x) == CTRUE) ? ((equal((*(OBJECT(Call,a)->args))[2],x2) == CTRUE) ? CTRUE: CFALSE): CFALSE): CFALSE) :
          CFALSE ) ) );
    return (Result);} 
  } 


// *********************************************************************
// *      Part 2: Basic structures                                     *
// *********************************************************************
// ------------------ assignment ---------------------------------------
// <-(var V, arg E) where V is a variable (and therefore NOT a global_variable)
//
// the var slot is filled with a real variable later.
/* The c++ function for: self_print(self:Assign) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
void  self_print_Assign_Language(Assign *self)
{ GC_BIND;
  { OID  a = GC_OID(self->arg);
    OID  o;
    if (INHERIT(OWNER(a),Language._Call))
     o = _oid_(OBJECT(Call,a)->selector);
    else o = Kernel.cfalse;
      { ClaireBoolean * g0021I;
      { OID  g0022UU;
        if (INHERIT(OWNER(a),Language._Call))
         g0022UU = (*(OBJECT(Call,a)->args))[1];
        else g0022UU = Kernel.cfalse;
          g0021I = sugar_ask_any(self->var,_oid_(Kernel.emptySet),o,g0022UU);
        } 
      
      if (g0021I == CTRUE) { print_any(GC_OID(self->var));
          princ_string(" :");
          print_any(o);
          princ_string(" ");
          lbreak_integer(2);
          printexp_any(GC_OID((*(OBJECT(bag,(*Core.args)(a))))[2]),CTRUE);
          princ_string("");
          } 
        else { print_any(GC_OID(self->var));
        princ_string(" := ");
        lbreak_integer(2);
        printexp_any(a,CTRUE);
        princ_string("");
        } 
      } 
    (Core.pretty->index = (Core.pretty->index-2));
    } 
  GC_UNBIND;} 


/* The c++ function for: self_eval(self:Assign) [NEW_ALLOC+RETURN_ARG] */
OID  self_eval_Assign(Assign *self)
{ GC_BIND;
  { OID Result = 0;
    if (INHERIT(OWNER(self->var),Language._Variable))
     Result = write_value_Variable(GC_OBJECT(Variable,OBJECT(Variable,self->var)),GC_OID(OPT_EVAL(self->arg)));
    else { OID  V_CL0023;close_exception(((general_error *) (*Core._general_error)(_string_("[101] ~S is not a variable"),
          _oid_(list::alloc(1,self->var)))));
        
        Result=_void_(V_CL0023);} 
      GC_UNBIND; return (Result);} 
  } 


// global variables
//
/* The c++ function for: self_print(self:Gassign) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
void  self_print_Gassign_Language(Gassign *self)
{ GC_BIND;
  { OID  a = GC_OID(self->arg);
    OID  o;
    if (INHERIT(OWNER(a),Language._Call))
     o = _oid_(OBJECT(Call,a)->selector);
    else o = Kernel.cfalse;
      { ClaireBoolean * g0025I;
      { OID  g0026UU;
        if (INHERIT(OWNER(a),Language._Call))
         g0026UU = (*(OBJECT(Call,a)->args))[1];
        else g0026UU = Kernel.cfalse;
          g0025I = sugar_ask_any(_oid_(self->var),_oid_(Kernel.emptySet),o,g0026UU);
        } 
      
      if (g0025I == CTRUE) { print_any(_oid_(self->var));
          princ_string(" :");
          print_any(o);
          princ_string(" ");
          lbreak_integer(2);
          print_any(GC_OID((*(OBJECT(bag,(*Core.args)(a))))[2]));
          princ_string("");
          } 
        else { print_any(_oid_(self->var));
        princ_string(" := ");
        lbreak_integer(2);
        print_any(a);
        princ_string("");
        } 
      } 
    (Core.pretty->index = (Core.pretty->index-2));
    } 
  GC_UNBIND;} 


/* The c++ function for: self_eval(self:Gassign) [NEW_ALLOC+SLOT_UPDATE+RETURN_ARG] */
OID  self_eval_Gassign(Gassign *self)
{ GC_BIND;
  { OID Result = 0;
    { global_variable * v = self->var;
      Result = write_value_global_variable(v,GC_OID(eval_any(GC_OID(self->arg))));
      } 
    GC_UNBIND; return (Result);} 
  } 


//--------------- BOOLEAN OPERATIONS ---------------------------------
// "and" is strictly boolean and is based on short-circuit evaluation.
//
/* The c++ function for: self_print(self:And) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
void  self_print_And_Language(And *self)
{ GC_BIND;
  princ_string("(");
  printbox_bag3(GC_OBJECT(list,self->args)," & ");
  princ_string(")");
  GC_UNBIND;} 


/* The c++ function for: self_eval(self:And) [NEW_ALLOC] */
OID  self_eval_And(And *self)
{ GC_BIND;
  { OID Result = 0;
    { ClaireBoolean * V_CL0027;{ OID  g0028UU;
        { OID gc_local;
          ITERATE(x);
          g0028UU= _oid_(CFALSE);
          for (START(self->args); NEXT(x);)
          if (boolean_I_any(OPT_EVAL(x)) != CTRUE)
           { g0028UU = Kernel.ctrue;
            break;} 
          } 
        V_CL0027 = not_any(g0028UU);
        } 
      
      Result=_oid_(V_CL0027);} 
    GC_UNBIND; return (Result);} 
  } 


// or expression
//
/* The c++ function for: self_print(self:Or) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
void  self_print_Or_Language(Or *self)
{ GC_BIND;
  princ_string("(");
  printbox_bag3(GC_OBJECT(list,self->args)," | ");
  princ_string(")");
  GC_UNBIND;} 


/* The c++ function for: self_eval(self:Or) [NEW_ALLOC] */
OID  self_eval_Or(Or *self)
{ GC_BIND;
  { OID Result = 0;
    { ClaireBoolean * g0029I;
      { OID V_C;{ OID gc_local;
          ITERATE(x);
          V_C= _oid_(CFALSE);
          for (START(self->args); NEXT(x);)
          if (boolean_I_any(OPT_EVAL(x)) == CTRUE)
           { V_C = Kernel.ctrue;
            break;} 
          } 
        
        g0029I=OBJECT(ClaireBoolean,V_C);} 
      
      if (g0029I == CTRUE) Result = Kernel.ctrue;
        else Result = Kernel.cfalse;
      } 
    GC_UNBIND; return (Result);} 
  } 


// ----------------- an anti-evaluator ---------------------------------
//
/* The c++ function for: self_print(self:Quote) [NEW_ALLOC] */
void  self_print_Quote_Language(Quote *self)
{ GC_BIND;
  princ_string("quote(");
  print_any(GC_OID(self->arg));
  princ_string(")");
  GC_UNBIND;} 


/* The c++ function for: self_eval(self:Quote) [RETURN_ARG] */
OID  self_eval_Quote(Quote *self)
{ return (self->arg);} 


// *********************************************************************
// *      Part 3: Specialized structures                               *
// *********************************************************************
// optimized_instruction is the set of optimized messages.
// These are the forms produced by the optimizer. They correspond to basic
// kinds of evaluation.
//
// This is how a call to a compiled method can be compiled.
// We use the C external function
//
/* The c++ function for: self_print(self:Call_method) [NEW_ALLOC+SLOT_UPDATE] */
void  self_print_Call_method_Language(Call_method *self)
{ GC_BIND;
  print_any(_oid_(self->arg));
  princ_string("(");
  princ_bag(GC_OBJECT(list,self->args));
  princ_string(")");
  GC_UNBIND;} 


/* The c++ function for: self_eval(self:Call_method) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
OID  self_eval_Call_method(Call_method *self)
{ GC_BIND;
  { OID Result = 0;
    { int  start = ClEnv->index;
      method * Cprop = self->arg;
      { OID gc_local;
        ITERATE(x);
        bag *x_support;
        x_support = GC_OBJECT(list,self->args);
        for (START(x_support); NEXT(x);)
        { GC_LOOP;
          PUSH(GC_OID(OPT_EVAL(x)));
          GC_UNLOOP;} 
        } 
      Result = execute_method(Cprop,start,CTRUE);
      } 
    GC_UNBIND; return (Result);} 
  } 


// same thing with one only argument: we do not use the stack
//
/* The c++ function for: self_eval(self:Call_method1) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
OID  self_eval_Call_method1(Call_method1 *self)
{ GC_BIND;
  { OID Result = 0;
    { method * f = self->arg;
      list * l = GC_OBJECT(list,self->args);
      Result = funcall_method1(f,GC_OID(OPT_EVAL((*(l))[1])));
      } 
    GC_UNBIND; return (Result);} 
  } 


// same thing with two arguments
//
/* The c++ function for: self_eval(self:Call_method2) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
OID  self_eval_Call_method2(Call_method2 *self)
{ GC_BIND;
  { OID Result = 0;
    { method * f = self->arg;
      list * l = GC_OBJECT(list,self->args);
      Result = funcall_method2(f,GC_OID(OPT_EVAL((*(l))[1])),GC_OID(OPT_EVAL((*(l))[2])));
      } 
    GC_UNBIND; return (Result);} 
  } 


// an instruction to read a slot
//
/* The c++ function for: self_print(self:Call_slot) [NEW_ALLOC] */
void  self_print_Call_slot_Language(Call_slot *self)
{ GC_BIND;
  print_any(_oid_(self->selector));
  princ_string("(");
  print_any(GC_OID(self->arg));
  princ_string(")");
  GC_UNBIND;} 


/* The c++ function for: self_eval(self:Call_slot) [NEW_ALLOC+RETURN_ARG] */
OID  self_eval_Call_slot(Call_slot *self)
{ GC_BIND;
  { OID Result = 0;
    Result = get_slot(self->selector,OBJECT(ClaireObject,OPT_EVAL(self->arg)));
    GC_UNBIND; return (Result);} 
  } 


// an instruction to read an array
// selector is an exp with type array, arg is an exp with type integer, and test
// contains the inferred member_type of the array
//
/* The c++ function for: self_print(self:Call_array) [NEW_ALLOC] */
void  self_print_Call_array_Language(Call_array *self)
{ GC_BIND;
  print_any(GC_OID(self->selector));
  princ_string("[");
  print_any(GC_OID(self->arg));
  princ_string("]");
  GC_UNBIND;} 


/* The c++ function for: self_eval(self:Call_array) [NEW_ALLOC+RETURN_ARG] */
OID  self_eval_Call_array(Call_array *self)
{ GC_BIND;
  { OID Result = 0;
    Result = nth_array(array_v(OPT_EVAL(self->selector)),OPT_EVAL(self->arg));
    GC_UNBIND; return (Result);} 
  } 


// an instruction to read a table
//
/* The c++ function for: self_print(self:Call_table) [NEW_ALLOC] */
void  self_print_Call_table_Language(Call_table *self)
{ GC_BIND;
  print_any(_oid_(self->selector));
  princ_string("[");
  print_any(GC_OID(self->arg));
  princ_string("]");
  GC_UNBIND;} 


/* The c++ function for: self_eval(self:Call_table) [NEW_ALLOC+RETURN_ARG] */
OID  self_eval_Call_table(Call_table *self)
{ GC_BIND;
  { OID Result = 0;
    if (self->test == CTRUE)
     Result = nth_table1(self->selector,OPT_EVAL(self->arg));
    else Result = get_table(self->selector,OPT_EVAL(self->arg));
      GC_UNBIND; return (Result);} 
  } 


// an instruction to write a slot
// the structure is complex: see ocall.cl
//
/* The c++ function for: self_print(self:Update) [NEW_ALLOC] */
void  self_print_Update_Language(Update *self)
{ GC_BIND;
  print_any(GC_OID(self->selector));
  princ_string("(");
  print_any(GC_OID((*Kernel.arg)(self->var)));
  princ_string(") := ");
  print_any(GC_OID(self->value));
  princ_string("");
  GC_UNBIND;} 


/* The c++ function for: self_eval(self:Update) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
OID  self_eval_Update(Update *self)
{ GC_BIND;
  { OID Result = 0;
    { OID  s = GC_OID(self->selector);
      if (INHERIT(OWNER(s),Kernel._property))
       put_property2(OBJECT(property,s),GC_OBJECT(ClaireObject,OBJECT(ClaireObject,eval_any(GC_OID((*Kernel.arg)(self->var))))),GC_OID(eval_any(GC_OID(self->value))));
      else if (INHERIT(OWNER(s),Kernel._table))
       nth_equal_table1(OBJECT(table,s),GC_OID(eval_any(GC_OID((*Kernel.arg)(self->var)))),GC_OID(eval_any(GC_OID(self->value))));
      Result = CNULL;
      } 
    GC_UNBIND; return (Result);} 
  } 


// ------------------ SUPER: a jump in the set lattice ---------------
// A "super" allows one to execute a message as if the type of the receiver
// was a given abstract_class.
// However we require that the receiver be in the specified abstract_class.
// The form of the super is: SELECTOR@ABSTRACT_CLASS(RECEIVER , ...)
//
/* The c++ function for: self_print(self:Super) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
void  self_print_Super_Language(Super *self)
{ GC_BIND;
  { int  _Zl = Core.pretty->index;
    property * _Zs = self->selector;
    list * _Za = GC_OBJECT(list,self->args);
    print_any(_oid_(self->selector));
    princ_string("@");
    print_any(GC_OID(_oid_(self->cast_to)));
    princ_string("(");
    set_level_void();
    printbox_bag2(_Za);
    princ_string(")");
    (Core.pretty->index = _Zl);
    } 
  GC_UNBIND;} 


/* The c++ function for: self_eval(self:Super) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
OID  self_eval_Super(Super *self)
{ GC_BIND;
  { OID Result = 0;
    { int  start = ClEnv->index;
      ClaireType * t = GC_OBJECT(ClaireType,self->cast_to);
      ClaireClass * c = class_I_type(t);
      property * p = self->selector;
      { OID gc_local;
        ITERATE(x);
        bag *x_support;
        x_support = GC_OBJECT(list,self->args);
        for (START(x_support); NEXT(x);)
        { GC_LOOP;
          PUSH(GC_OID(OPT_EVAL(x)));
          GC_UNLOOP;} 
        } 
      Result = eval_message_property(p,find_which_class(c,p->definition,start,(ClEnv->index)),start,CTRUE);
      } 
    GC_UNBIND; return (Result);} 
  } 


//--------------- comments ------------------------------------------
// the cast is the new form of simple super
//
/* The c++ function for: self_print(x:Cast) [NEW_ALLOC+SLOT_UPDATE] */
void  self_print_Cast_Language(Cast *x)
{ GC_BIND;
  printexp_any(GC_OID(x->arg),CFALSE);
  princ_string(" as ");
  printexp_any(GC_OID(_oid_(x->set_arg)),CFALSE);
  princ_string("");
  GC_UNBIND;} 


/* The c++ function for: self_eval(self:Cast) [NEW_ALLOC+RETURN_ARG] */
OID  self_eval_Cast(Cast *self)
{ GC_BIND;
  { OID Result = 0;
    { OID  x = GC_OID(OPT_EVAL(self->arg));
      ClaireType * y = self->set_arg;
      if ((INHERIT(y->isa,Core._Param)) && (((CLREAD(Param,y,arg) == Kernel._list) || 
            (CLREAD(Param,y,arg) == Kernel._set)) && 
          (Kernel._set == OWNER((*(CLREAD(Param,y,args)))[1]))))
       Result = _oid_(check_in_bag(OBJECT(bag,x),Kernel._bag,OBJECT(ClaireType,(*(OBJECT(set,(*(CLREAD(Param,y,args)))[1])))[1])));
      else Result = check_in_any(x,y);
        } 
    GC_UNBIND; return (Result);} 
  } 


// v3.3.16
// ----------------- return from a loop --------------------------------
//
// return_error is an exception that is handled by the "for" family
// of structures
//
/* The c++ function for: self_print(self:Return) [NEW_ALLOC+SLOT_UPDATE] */
void  self_print_Return_Language(Return *self)
{ GC_BIND;
  princ_string("break(");
  (Core.pretty->index = (Core.pretty->index+2));
  print_any(GC_OID(self->arg));
  (Core.pretty->index = (Core.pretty->index-2));
  princ_string(")");
  GC_UNBIND;} 


/* The c++ function for: self_eval(self:Return) [NEW_ALLOC] */
OID  self_eval_Return(Return *self)
{ GC_BIND;
  { OID Result = 0;
    { OID  V_CL0032;close_exception(((return_error *) (*Core._return_error)(GC_OID(OPT_EVAL(self->arg)))));
      
      Result=_void_(V_CL0032);} 
    GC_UNBIND; return (Result);} 
  } 


// ****************************************************************
// *       Part 4: Miscellaneous on instructions                  *
// ****************************************************************
// substitute any variable with same name as x with the value val
/* The c++ function for: substitution(self:any,x:Variable,val:any) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
OID  substitution_any(OID self,Variable *x,OID val)
{ { OID Result = 0;
    if (INHERIT(OWNER(self),Language._Variable))
     { if (OBJECT(Variable,self)->pname == x->pname)
       Result = val;
      else Result = self;
        } 
    else if (INHERIT(OWNER(self),Kernel._bag))
     { { int  i = 1;
        int  g0033 = OBJECT(bag,self)->length;
        { OID gc_local;
          while ((i <= g0033))
          { if ((INHERIT(OWNER((*(OBJECT(bag,self)))[i]),Language._Variable)) || 
                (INHERIT(OWNER((*(OBJECT(bag,self)))[i]),Kernel._unbound_symbol)))
             ((*(OBJECT(list,self)))[i]=substitution_any((*(OBJECT(bag,self)))[i],x,val));
            else substitution_any((*(OBJECT(bag,self)))[i],x,val);
              ++i;
            } 
          } 
        } 
      Result = self;
      } 
    else if (INHERIT(OWNER(self),Kernel._unbound_symbol))
     { if (OBJECT(unbound_symbol,self)->name == x->pname)
       Result = val;
      else Result = self;
        } 
    else if (INHERIT(OWNER(self),Language._Instruction))
     { { OID gc_local;
        ITERATE(s);
        bag *s_support;
        s_support = OWNER(self)->slots;
        for (START(s_support); NEXT(s);)
        { OID  y = get_slot(OBJECT(slot,s),OBJECT(ClaireObject,self));
          if ((INHERIT(OWNER(y),Language._Variable)) || 
              (INHERIT(OWNER(y),Kernel._unbound_symbol)))
           put_slot(OBJECT(slot,s),OBJECT(ClaireObject,self),substitution_any(y,x,val));
          else substitution_any(y,x,val);
            } 
        } 
      Result = self;
      } 
    else Result = self;
      return (Result);} 
  } 


// count the number of occurrences of x
/* The c++ function for: occurrence(self:any,x:Variable) [NEW_ALLOC] */
int  occurrence_any(OID self,Variable *x)
{ { int Result = 0;
    if (INHERIT(OWNER(self),Language._Variable))
     { Result = ((OBJECT(Variable,self)->pname == x->pname) ?
        1 :
        0 );
      } 
    else if (INHERIT(OWNER(self),Kernel._bag))
     { int  n = 0;
      { int  i = 1;
        int  g0034 = OBJECT(bag,self)->length;
        { OID gc_local;
          while ((i <= g0034))
          { n= (n+occurrence_any((*(OBJECT(bag,self)))[i],x));
            ++i;
            } 
          } 
        } 
      Result = n;
      } 
    else if (INHERIT(OWNER(self),Kernel._unbound_symbol))
     { Result = ((OBJECT(unbound_symbol,self)->name == x->pname) ?
        1 :
        0 );
      } 
    else if (INHERIT(OWNER(self),Language._Instruction))
     { int  n = 0;
      { OID gc_local;
        ITERATE(s);
        bag *s_support;
        s_support = OWNER(self)->slots;
        for (START(s_support); NEXT(s);)
        n= (n+occurrence_any(get_slot(OBJECT(slot,s),OBJECT(ClaireObject,self)),x));
        } 
      Result = n;
      } 
    else Result = 0;
      return (Result);} 
  } 


// makes a (deep) copy of the instruction self
//
/* The c++ function for: instruction_copy(self:any) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
OID  instruction_copy_any(OID self)
{ GC_BIND;
  { OID Result = 0;
    if (INHERIT(OWNER(self),Kernel._bag))
     { bag * l = copy_bag(OBJECT(bag,self));
      { int  i = 1;
        int  g0035 = OBJECT(bag,self)->length;
        { OID gc_local;
          while ((i <= g0035))
          { ((*(((list *) l)))[i]=instruction_copy_any((*(OBJECT(bag,self)))[i]));
            ++i;
            } 
          } 
        } 
      Result = _oid_(l);
      } 
    else if (INHERIT(OWNER(self),Language._Variable))
     Result = self;
    else if (INHERIT(OWNER(self),Language._Instruction))
     { Instruction * o = GC_OBJECT(Instruction,((Instruction *) copy_object(OBJECT(ClaireObject,self))));
      { OID gc_local;
        ITERATE(s);
        bag *s_support;
        s_support = OWNER(self)->slots;
        for (START(s_support); NEXT(s);)
        put_slot(OBJECT(slot,s),o,instruction_copy_any(get_slot(OBJECT(slot,s),OBJECT(ClaireObject,self))));
        } 
      Result = _oid_(o);
      } 
    else Result = self;
      GC_UNBIND; return (Result);} 
  } 

