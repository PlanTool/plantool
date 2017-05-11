/***** CLAIRE Compilation of file c:\claire\v3.3\src\meta\control.cl 
         [version 3.3.4 / safety 5] Sat Oct 16 06:53:29 2004 *****/

#include <claire.h>
#include <Kernel.h>
#include <Core.h>
#include <Language.h>

//+-------------------------------------------------------------+
//| CLAIRE                                                      |
//| control.cl                                                  |
//| Copyright (C) 1994 - 2003 Yves Caseau. All Rights Reserved  |
//| cf. copyright info in file object.cl: about()               |
//+-------------------------------------------------------------+
// *********************************************************************
// *     Part 1: If, Do, Let                                           *
// *     Part 2: set control structures                                *
// *     Part 3: other control structures                              *
// *     Part 4: the constructs                                        *
// *********************************************************************
// *********************************************************************
// *     Part 1: If, Do, Let                                           *
// *********************************************************************
//--------------- the IF --------------------------------------------
/* The c++ function for: self_print(self:If) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
void  self_print_If_Language(If *self)
{ princ_string("(");
  (Core.pretty->index = (Core.pretty->index+1));
  printstat_If(self);
  (Core.pretty->index = (Core.pretty->index-1));
  princ_string(")");
  } 


/* The c++ function for: printstat(self:If) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
void  printstat_If(If *self)
{ GC_BIND;
  princ_string("if ");
  printexp_any(GC_OID(self->test),CFALSE);
  checkfar_void();
  princ_string(" ");
  printif_any(GC_OID(self->arg));
  (Core.pretty->index = (Core.pretty->index-3));
  printelse_If(self);
  princ_string("");
  GC_UNBIND;} 


/* The c++ function for: printif(self:any) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
void  printif_any(OID self)
{ (Core.pretty->index = (Core.pretty->index+3));
  if (Core.pretty->pbreak == CTRUE)
   { int  b_index = buffer_length_void();
    int  _Zl = Core.pretty->index;
    (Core.pretty->pbreak = CFALSE);
    { ClaireHandler c_handle = ClaireHandler();
      if ERROR_IN 
      { print_any(self);
        ClEnv->cHandle--;} 
      else if (belong_to(_oid_(ClEnv->exception_I),_oid_(Core._much_too_far)) == CTRUE)
      { c_handle.catchIt();;} 
      else PREVIOUS_HANDLER;} 
    (Core.pretty->pbreak = CTRUE);
    if (short_enough_integer(buffer_length_void()) == CTRUE)
     ;else { buffer_set_length_integer(b_index);
        (Core.pretty->index = _Zl);
        lbreak_void();
        print_any(self);
        } 
      } 
  else print_any(self);
    } 


/* The c++ function for: printelse(self:If) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
void  printelse_If(If *self)
{ GC_BIND;
  { OID  e = GC_OID(self->other);
    if (INHERIT(OWNER(e),Language._If))
     { princ_string(" ");
      lbreak_void();
      princ_string("else if ");
      printexp_any(GC_OID(OBJECT(If,e)->test),CFALSE);
      princ_string(" ");
      printif_any(GC_OID(OBJECT(If,e)->arg));
      (Core.pretty->index = (Core.pretty->index-3));
      printelse_If(OBJECT(If,e));
      princ_string("");
      } 
    else if (equal(e,Core.nil->value) != CTRUE)
     { int  _Zl = Core.pretty->index;
      princ_string(" ");
      lbreak_void();
      princ_string("else ");
      set_level_integer(1);
      print_any(e);
      princ_string("");
      (Core.pretty->index = _Zl);
      } 
    } 
  GC_UNBIND;} 


// notice that the eval(test) is not a boolean thus the compiler will add
// something
// TODO: check that is is not too slow (may use a constant for _oid_(true))
/* The c++ function for: self_eval(self:If) [SAFE_GC] */
OID  self_eval_If(If *self)
{ { OID Result = 0;
    { OID  x = OPT_EVAL(self->test);
      if (x == Kernel.ctrue)
       Result = OPT_EVAL(self->arg);
      else if (x == Kernel.cfalse)
       Result = OPT_EVAL(self->other);
      else if (boolean_I_any(x) == CTRUE)
       Result = OPT_EVAL(self->arg);
      else Result = OPT_EVAL(self->other);
        } 
    return (Result);} 
  } 


//--------------------- block structure------------------------------
/* The c++ function for: self_print(self:Do) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
void  self_print_Do_Language(Do *self)
{ GC_BIND;
  { int  _Zl = Core.pretty->index;
    princ_string("(");
    set_level_integer(1);
    printdo_list(GC_OBJECT(list,self->args),CTRUE);
    (Core.pretty->index = _Zl);
    } 
  GC_UNBIND;} 


/* The c++ function for: printdo(l:list,clo:boolean) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
void  printdo_list(list *l,ClaireBoolean *clo)
{ { int  n = l->length;
    { ITERATE(x);
      for (START(l); NEXT(x);)
      { if (INHERIT(OWNER(x),Language._If))
         printstat_If(OBJECT(If,x));
        else print_any(x);
          n= (n-1);
        if (n == 0)
         { if (clo == CTRUE)
           princ_string(")");
          } 
        else { princ_string(", ");
            lbreak_void();
            } 
          } 
      } 
    } 
  } 


/* The c++ function for: printblock(x:any) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
void  printblock_any(OID x)
{ GC_BIND;
  if (INHERIT(OWNER(x),Language._Do))
   printdo_list(GC_OBJECT(list,OBJECT(Do,x)->args),CFALSE);
  else if (INHERIT(OWNER(x),Language._If))
   printstat_If(OBJECT(If,x));
  else print_any(x);
    GC_UNBIND;} 


/* The c++ function for: self_eval(self:Do) [SAFE_GC] */
OID  self_eval_Do(Do *self)
{ { OID Result = 0;
    { OID  res = _oid_(Kernel.emptySet);
      { ITERATE(_Zx);
        for (START(self->args); NEXT(_Zx);)
        res= OPT_EVAL(_Zx);
        } 
      Result = res;
      } 
    return (Result);} 
  } 


// ----------------- lexical variable definition -----------------------
/* The c++ function for: self_print(self:Let) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
void  self_print_Let_Language(Let *self)
{ GC_BIND;
  { int  _Zl = Core.pretty->index;
    set_level_integer(1);
    princ_string("let ");
    ppvariable_Variable(GC_OBJECT(Variable,self->var));
    princ_string(" := ");
    printexp_any(GC_OID(self->value),CFALSE);
    printbody_Let(self);
    princ_string("");
    (Core.pretty->index = _Zl);
    } 
  GC_UNBIND;} 


/* The c++ function for: printbody(self:Let) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
void  printbody_Let(Let *self)
{ GC_BIND;
  { OID  a = GC_OID(self->arg);
    if (INHERIT(OWNER(a),Language._Let))
     { princ_string(",");
      lbreak_integer(4);
      ppvariable_Variable(GC_OBJECT(Variable,OBJECT(Instruction_with_var,a)->var));
      princ_string(" := ");
      printexp_any(GC_OID(OBJECT(Let,a)->value),CFALSE);
      (Core.pretty->index = (Core.pretty->index-4));
      printbody_Let(OBJECT(Let,a));
      princ_string("");
      } 
    else { princ_string(" in ");
        lbreak_integer(2);
        print_any(a);
        princ_string("");
        } 
      } 
  GC_UNBIND;} 


/* The c++ function for: self_eval(self:Let) [NEW_ALLOC+SLOT_UPDATE] */
OID  self_eval_Let(Let *self)
{ GC_BIND;
  { OID Result = 0;
    { OID  val = GC_OID(eval_any(GC_OID(self->value)));
      int  n = ClEnv->trace_I;
      if ((n > 0) && 
          (n == ClEnv->step_I))
       { (ClEnv->trace_I = 0);
        princ_string("start: let ");
        print_any(GC_OID(_oid_(self->var)));
        princ_string(" := ");
        print_any(val);
        princ_string(" ...\n");
        (ClEnv->trace_I = n);
        } 
      write_value_Variable(self->var,val);
      Result = eval_any(GC_OID(self->arg));
      } 
    GC_UNBIND; return (Result);} 
  } 


// a when is a special Let that filters out the unknown value !
//
/* The c++ function for: self_print(self:When) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
void  self_print_When_Language(When *self)
{ GC_BIND;
  { int  _Zl = Core.pretty->index;
    set_level_integer(1);
    princ_string("when ");
    ppvariable_Variable(GC_OBJECT(Variable,self->var));
    princ_string(" := ");
    printexp_any(GC_OID(self->value),CFALSE);
    princ_string(" in ");
    lbreak_integer(2);
    print_any(GC_OID(self->arg));
    princ_string("");
    if (((self->other == CNULL) ? CTRUE : CFALSE) != CTRUE)
     { princ_string(" ");
      lbreak_void();
      princ_string("else ");
      set_level_integer(1);
      print_any(GC_OID(self->other));
      princ_string("");
      } 
    (Core.pretty->index = _Zl);
    } 
  GC_UNBIND;} 


/* The c++ function for: self_eval(self:When) [NEW_ALLOC+SLOT_UPDATE] */
OID  self_eval_When(When *self)
{ GC_BIND;
  { OID Result = 0;
    { OID  val = GC_OID(eval_any(GC_OID(self->value)));
      int  n = ClEnv->trace_I;
      if ((n > 0) && 
          (n == ClEnv->step_I))
       { (ClEnv->trace_I = 0);
        princ_string("start: when ");
        print_any(GC_OID(_oid_(self->var)));
        princ_string(" := ");
        print_any(val);
        princ_string(" ...\n");
        (ClEnv->trace_I = n);
        } 
      if (val != CNULL)
       { write_value_Variable(self->var,val);
        Result = eval_any(GC_OID(self->arg));
        } 
      else Result = eval_any(GC_OID(self->other));
        } 
    GC_UNBIND; return (Result);} 
  } 


// two special forms of Let:
// Let+(v,r(x),(r(x) := y),Let(v2,e,(r(x) := v,v2)))    <=>  let r(x) = y in e
// Let*(v,f(),Let(v1,v[1],...(Let(vn,v[n],e))   <=> let (v1,v2,...vn) := f() in e
//
//note: the Let* is also used for multi-assignments
// Let*(v,f(),(v1 := v[1], v2 := v[2], ...))   <=>  (v1,v2,...vn) := f()
//
/* The c++ function for: self_print(self:Let+) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
void  self_print_Let_plus_Language(Let_plus *self)
{ GC_BIND;
  { int  _Zl = Core.pretty->index;
    list * l = GC_OBJECT(list,OBJECT(Do,self->arg)->args);
    set_level_integer(1);
    princ_string("let ");
    printexp_any(GC_OID(self->value),CFALSE);
    princ_string(" := ");
    printexp_any(GC_OID((*(OBJECT(bag,(*Core.args)((*(l))[1]))))[3]),CFALSE);
    princ_string(" in ");
    lbreak_integer(2);
    print_any(GC_OID(OBJECT(Let,(*(l))[2])->value));
    princ_string("");
    (Core.pretty->index = _Zl);
    } 
  GC_UNBIND;} 


/* The c++ function for: self_print(self:Let*) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
void  self_print_Let_star_Language(Let_star *self)
{ GC_RESERVE(4);  // v3.0.55 optim !
  { int  _Zl = Core.pretty->index;
    OID  l = GC_OID(self->arg);
    set_level_integer(1);
    if (INHERIT(OWNER(l),Language._Let))
     { princ_string("let (");
      { OID gc_local;
        while ((CTRUE == CTRUE))
        { GC_LOOP;
          (*Language.ppvariable)(GC_OID((*Language.var)(l)));
          { OID  lnext = GC_OID((*Kernel.arg)(l));
            if ((INHERIT(OWNER(lnext),Language._Let)) && ((INHERIT(OWNER(OBJECT(Let,lnext)->value),Language._Call)) && 
                (equal(GC_OID((*(OBJECT(bag,(*Core.args)(GC_OID(OBJECT(Let,lnext)->value)))))[1]),GC_OID(_oid_(self->var))) == CTRUE)))
             { princ_string(",");
              GC__OID(l = lnext, 3);
              } 
            else { ;break;} 
              } 
          GC_UNLOOP;} 
        } 
      princ_string(") := ");
      printexp_any(GC_OID(self->value),CFALSE);
      printbody_Let(OBJECT(Let,l));
      princ_string("");
      } 
    else { princ_string("(");
        { ClaireBoolean * _Zf = CTRUE;
          { OID gc_local;
            ITERATE(_Za);
            bag *_Za_support;
            _Za_support = GC_OBJECT(list,OBJECT(bag,(*Core.args)(l)));
            for (START(_Za_support); NEXT(_Za);)
            { GC_LOOP;
              { if (_Zf == CTRUE)
                 _Zf= CFALSE;
                else princ_string(",");
                  (*Language.ppvariable)(GC_OID((*Language.var)(_Za)));
                } 
              GC_UNLOOP;} 
            } 
          } 
        princ_string(") := ");
        printexp_any(GC_OID(self->value),CFALSE);
        princ_string("");
        } 
      (Core.pretty->index = _Zl);
    } 
  GC_UNBIND;} 


// *********************************************************************
// *     Part 2: set control structures                                *
// *********************************************************************
// for is the simplest evaluation loop
//
/* The c++ function for: self_print(self:For) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
void  self_print_For_Language(For *self)
{ GC_BIND;
  princ_string("for ");
  ppvariable_Variable(GC_OBJECT(Variable,self->var));
  princ_string(" in ");
  { int  _Zl = Core.pretty->index;
    set_level_void();
    printexp_any(GC_OID(self->set_arg),CFALSE);
    (Core.pretty->index = _Zl);
    } 
  princ_string(" ");
  (Core.pretty->index = (Core.pretty->index+2));
  lbreak_void();
  print_any(GC_OID(self->arg));
  (Core.pretty->index = (Core.pretty->index-2));
  princ_string("");
  GC_UNBIND;} 


/* The c++ function for: self_eval(self:For) [NEW_ALLOC+SLOT_UPDATE+RETURN_ARG] */
OID  self_eval_For(For *self)
{ GC_BIND;
  { OID Result = 0;
    { OID  x = GC_OID(eval_any(GC_OID(self->set_arg)));
      int  n = ClEnv->trace_I;
      if ((n > 0) && 
          (n == ClEnv->step_I))
       { (ClEnv->trace_I = 0);
        princ_string("start: for ");
        print_any(GC_OID(_oid_(self->var)));
        princ_string(" in ");
        print_any(x);
        princ_string(" ...\n");
        (ClEnv->trace_I = n);
        } 
      { ClaireHandler c_handle = ClaireHandler();
        if ERROR_IN 
        { if (INHERIT(OWNER(x),Kernel._class))
           { OID gc_local;
            ITERATE(y);
            Result= _oid_(CFALSE);
            for (START(OBJECT(ClaireClass,x)->descendents); NEXT(y);)
            { GC_LOOP;
              { OID gc_local;
                ITERATE(z);
                for (START(OBJECT(ClaireClass,y)->instances); NEXT(z);)
                { GC_LOOP;
                  { write_value_Variable(self->var,z);
                    eval_any(GC_OID(self->arg));
                    } 
                  GC_UNLOOP;} 
                } 
              GC_UNLOOP;} 
            } 
          else if (Kernel._array == OWNER(x))
           { int  n = array_v(x)[0];
            int  g0044 = 1;
            int  g0045 = n;
            { OID gc_local;
              Result= _oid_(CFALSE);
              while ((g0044 <= g0045))
              { GC_LOOP;
                { OID  z = nth_get_array(array_v(x),g0044);
                  write_value_Variable(self->var,z);
                  eval_any(GC_OID(self->arg));
                  } 
                ++g0044;
                GC_UNLOOP;} 
              } 
            } 
          else if (INHERIT(OWNER(x),Core._Interval))
           { int  y = OBJECT(Interval,x)->arg1;
            int  g0046 = OBJECT(Interval,x)->arg2;
            { OID gc_local;
              Result= _oid_(CFALSE);
              while ((y <= g0046))
              { GC_LOOP;
                write_value_Variable(self->var,y);
                eval_any(GC_OID(self->arg));
                ++y;
                GC_UNLOOP;} 
              } 
            } 
          else if (INHERIT(OWNER(x),Kernel._collection))
           { OID gc_local;
            ITERATE(y);
            Result= _oid_(CFALSE);
            bag *y_support;
            y_support = GC_OBJECT(bag,enumerate_any(x));
            for (START(y_support); NEXT(y);)
            { GC_LOOP;
              { write_value_Variable(self->var,y);
                eval_any(GC_OID(self->arg));
                } 
              GC_UNLOOP;} 
            } 
          else { OID  V_CL0047;close_exception(((general_error *) (*Core._general_error)(_string_("[136] ~S is not a collection !"),
                _oid_(list::alloc(1,x)))));
              
              Result=_void_(V_CL0047);} 
            ClEnv->cHandle--;} 
        else if (belong_to(_oid_(ClEnv->exception_I),_oid_(Core._return_error)) == CTRUE)
        { c_handle.catchIt();Result = (*Kernel.arg)(GC_OID(_oid_(ClEnv->exception_I)));
          } 
        else PREVIOUS_HANDLER;} 
      } 
    GC_UNBIND; return (Result);} 
  } 


// [collect VAR in SET_EXPR, ...] is the same as a "for", but returns the list of values
//
/* The c++ function for: self_print(self:Collect) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
void  self_print_Collect_Language(Collect *self)
{ GC_BIND;
  princ_string("list{ ");
  (Core.pretty->index = (Core.pretty->index+2));
  printexp_any(GC_OID(self->arg),CFALSE);
  princ_string(" | ");
  lbreak_void();
  ppvariable_Variable(GC_OBJECT(Variable,self->var));
  princ_string(" in ");
  { int  _Zl = Core.pretty->index;
    set_level_void();
    printexp_any(GC_OID(self->set_arg),CFALSE);
    (Core.pretty->index = (_Zl-2));
    } 
  princ_string("}");
  GC_UNBIND;} 


/* The c++ function for: self_eval(self:Collect) [NEW_ALLOC] */
OID  self_eval_Collect(Collect *self)
{ GC_BIND;
  { OID Result = 0;
    { OID  x = GC_OID(OPT_EVAL(self->set_arg));
      list * res = list::empty();
      if (INHERIT(OWNER(x),Kernel._class))
       { ITERATE(y);
        for (START(OBJECT(ClaireClass,x)->descendents); NEXT(y);)
        { ITERATE(z);
          for (START(OBJECT(ClaireClass,y)->instances); NEXT(z);)
          { write_value_Variable(self->var,z);
            res= GC_OBJECT(list,res->addFast(GC_OID(OPT_EVAL(self->arg))));
            } 
          } 
        } 
      else { ITERATE(y);
          bag *y_support;
          y_support = GC_OBJECT(bag,enumerate_any(x));
          for (START(y_support); NEXT(y);)
          { write_value_Variable(self->var,y);
            res= res->addFast(GC_OID(OPT_EVAL(self->arg)));
            } 
          } 
        if (((self->of == (NULL)) ? CTRUE : CFALSE) != CTRUE)
       { { OID  x;
          { { OID  x_some = CNULL;
              { ITERATE(x);
                for (START(res); NEXT(x);)
                if (belong_to(x,_oid_(self->of)) != CTRUE)
                 { x_some= x;
                  break;} 
                } 
              x = x_some;
              } 
            GC_OID(x);} 
          if (x != CNULL)
           close_exception(((range_error *) (*Core._range_error)(_oid_(self),
            x,
            GC_OID(_oid_(self->of)))));
          else ;} 
        cast_I_bag(res,self->of);
        } 
      Result = _oid_(res);
      } 
    GC_UNBIND; return (Result);} 
  } 


// this is a set image version, that produces a set
//
/* The c++ function for: self_print(self:Image) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
void  self_print_Image_Language(Image *self)
{ GC_BIND;
  princ_string("{ ");
  (Core.pretty->index = (Core.pretty->index+2));
  printexp_any(GC_OID(self->arg),CFALSE);
  princ_string(" | ");
  lbreak_void();
  ppvariable_Variable(GC_OBJECT(Variable,self->var));
  princ_string(" in ");
  { int  _Zl = Core.pretty->index;
    set_level_void();
    printexp_any(GC_OID(self->set_arg),CFALSE);
    (Core.pretty->index = (_Zl-2));
    } 
  princ_string("}");
  GC_UNBIND;} 


/* The c++ function for: self_eval(self:Image) [NEW_ALLOC] */
OID  self_eval_Image(Image *self)
{ GC_BIND;
  { OID Result = 0;
    { OID  x = GC_OID(OPT_EVAL(self->set_arg));
      set * res = set::empty();
      { ITERATE(y);
        bag *y_support;
        y_support = GC_OBJECT(bag,enumerate_any(x));
        for (START(y_support); NEXT(y);)
        { write_value_Variable(self->var,y);
          res= GC_OBJECT(set,res->addFast(GC_OID(OPT_EVAL(self->arg))));
          } 
        } 
      if (((self->of == (NULL)) ? CTRUE : CFALSE) != CTRUE)
       { { OID  x;
          { { OID  x_some = CNULL;
              { ITERATE(x);
                for (START(res); NEXT(x);)
                if (belong_to(x,_oid_(self->of)) != CTRUE)
                 { x_some= x;
                  break;} 
                } 
              x = x_some;
              } 
            GC_OID(x);} 
          if (x != CNULL)
           close_exception(((range_error *) (*Core._range_error)(_oid_(self),
            x,
            GC_OID(_oid_(self->of)))));
          else ;} 
        cast_I_bag(res,self->of);
        } 
      Result = _oid_(res);
      } 
    GC_UNBIND; return (Result);} 
  } 


// [select VAR in SET_EXPR, ...] is the same as a "for" but returns the subset of
//  members that produce a true value
//
/* The c++ function for: self_print(self:Select) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
void  self_print_Select_Language(Select *self)
{ GC_BIND;
  princ_string("{ ");
  ppvariable_Variable(GC_OBJECT(Variable,self->var));
  princ_string(" in ");
  { int  _Zl = Core.pretty->index;
    set_level_void();
    printexp_any(GC_OID(self->set_arg),CFALSE);
    (Core.pretty->index = _Zl);
    } 
  princ_string(" | ");
  lbreak_integer(2);
  print_any(GC_OID(self->arg));
  (Core.pretty->index = (Core.pretty->index-2));
  princ_string("}");
  GC_UNBIND;} 


/* The c++ function for: self_eval(self:Select) [NEW_ALLOC] */
OID  self_eval_Select(Select *self)
{ GC_BIND;
  { OID Result = 0;
    { OID  x = GC_OID(OPT_EVAL(self->set_arg));
      set * res;
      { bag *V_CC ;
        if (Kernel._set == OWNER(x))
         V_CC = empty_bag(OBJECT(bag,x));
        else V_CC = set::empty();
          res= (set *) V_CC;} 
      if (INHERIT(OWNER(x),Kernel._class))
       { ITERATE(y);
        for (START(OBJECT(ClaireClass,x)->descendents); NEXT(y);)
        { ITERATE(z);
          for (START(OBJECT(ClaireClass,y)->instances); NEXT(z);)
          { write_value_Variable(self->var,z);
            if (OPT_EVAL(self->arg) != Kernel.cfalse)
             res= GC_OBJECT(set,res->addFast(z));
            } 
          } 
        } 
      else { ITERATE(y);
          bag *y_support;
          y_support = GC_OBJECT(bag,enumerate_any(x));
          for (START(y_support); NEXT(y);)
          { write_value_Variable(self->var,y);
            if (OPT_EVAL(self->arg) != Kernel.cfalse)
             res= res->addFast(y);
            } 
          } 
        if (((self->of == (NULL)) ? CTRUE : CFALSE) != CTRUE)
       { { OID  x;
          { { OID  x_some = CNULL;
              { ITERATE(x);
                for (START(res); NEXT(x);)
                if (belong_to(x,_oid_(self->of)) != CTRUE)
                 { x_some= x;
                  break;} 
                } 
              x = x_some;
              } 
            GC_OID(x);} 
          if (x != CNULL)
           close_exception(((range_error *) (*Core._range_error)(_oid_(self),
            x,
            GC_OID(_oid_(self->of)))));
          else ;} 
        cast_I_bag(res,self->of);
        } 
      Result = _oid_(res);
      } 
    GC_UNBIND; return (Result);} 
  } 


// [select VAR in SET_EXPR, ...] is the same as a "for" but returns the subset of
//  members that produce a true value
//
/* The c++ function for: self_print(self:Lselect) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
void  self_print_Lselect_Language(Lselect *self)
{ GC_BIND;
  princ_string("list{ ");
  ppvariable_Variable(GC_OBJECT(Variable,self->var));
  princ_string(" in ");
  { int  _Zl = Core.pretty->index;
    set_level_void();
    printexp_any(GC_OID(self->set_arg),CFALSE);
    (Core.pretty->index = _Zl);
    } 
  princ_string(" | ");
  lbreak_integer(2);
  print_any(GC_OID(self->arg));
  (Core.pretty->index = (Core.pretty->index-2));
  princ_string("}");
  GC_UNBIND;} 


/* The c++ function for: self_eval(self:Lselect) [NEW_ALLOC] */
OID  self_eval_Lselect(Lselect *self)
{ GC_BIND;
  { OID Result = 0;
    { OID  x = GC_OID(OPT_EVAL(self->set_arg));
      list * res;
      { bag *V_CC ;
        if (INHERIT(OWNER(x),Kernel._list))
         V_CC = empty_bag(OBJECT(bag,x));
        else V_CC = list::empty();
          res= (list *) V_CC;} 
      if (INHERIT(OWNER(x),Kernel._class))
       { ITERATE(y);
        for (START(OBJECT(ClaireClass,x)->descendents); NEXT(y);)
        { ITERATE(z);
          for (START(OBJECT(ClaireClass,y)->instances); NEXT(z);)
          { write_value_Variable(self->var,z);
            if (OPT_EVAL(self->arg) != Kernel.cfalse)
             res= GC_OBJECT(list,res->addFast(z));
            } 
          } 
        } 
      else { ITERATE(y);
          bag *y_support;
          y_support = GC_OBJECT(bag,enumerate_any(x));
          for (START(y_support); NEXT(y);)
          { write_value_Variable(self->var,y);
            if (OPT_EVAL(self->arg) != Kernel.cfalse)
             res= res->addFast(y);
            } 
          } 
        if (((self->of == (NULL)) ? CTRUE : CFALSE) != CTRUE)
       { { OID  x;
          { { OID  x_some = CNULL;
              { ITERATE(x);
                for (START(res); NEXT(x);)
                if (belong_to(x,_oid_(self->of)) != CTRUE)
                 { x_some= x;
                  break;} 
                } 
              x = x_some;
              } 
            GC_OID(x);} 
          if (x != CNULL)
           close_exception(((range_error *) (*Core._range_error)(_oid_(self),
            x,
            GC_OID(_oid_(self->of)))));
          else ;} 
        cast_I_bag(res,self->of);
        } 
      Result = _oid_(res);
      } 
    GC_UNBIND; return (Result);} 
  } 


// Exists is an iteration that checks a condition
// other = true => forall,  other = false => exists, other = unknown => some
/* The c++ function for: self_print(self:Exists) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
void  self_print_Exists_Language(Exists *self)
{ GC_BIND;
  if (self->other == Kernel.ctrue)
   princ_string("forall");
  else if (self->other == Kernel.cfalse)
   princ_string("exists");
  else princ_string("some");
    if (self->set_arg == _oid_(Kernel._any))
   { princ_string("(");
    ppvariable_Variable(GC_OBJECT(Variable,self->var));
    princ_string(",");
    print_any(GC_OID(self->arg));
    princ_string(")");
    } 
  else { princ_string("(");
      ppvariable_Variable(GC_OBJECT(Variable,self->var));
      princ_string(" in ");
      { int  _Zl = Core.pretty->index;
        set_level_void();
        printexp_any(GC_OID(self->set_arg),CFALSE);
        (Core.pretty->index = _Zl);
        } 
      princ_string(" | ");
      lbreak_integer(2);
      print_any(GC_OID(self->arg));
      (Core.pretty->index = (Core.pretty->index-2));
      princ_string(")");
      } 
    GC_UNBIND;} 


/* The c++ function for: self_eval(self:Exists) [NEW_ALLOC] */
OID  self_eval_Exists(Exists *self)
{ GC_BIND;
  { OID Result = 0;
    { OID  x = GC_OID(OPT_EVAL(self->set_arg));
      OID  b = self->other;
      OID  res = b;
      if (INHERIT(OWNER(x),Kernel._class))
       { OID gc_local;
        ITERATE(y);
        for (START(OBJECT(ClaireClass,x)->descendents); NEXT(y);)
        { GC_LOOP;
          { OID gc_local;
            ITERATE(z);
            for (START(OBJECT(ClaireClass,y)->instances); NEXT(z);)
            { GC_LOOP;
              { write_value_Variable(self->var,z);
                if (OPT_EVAL(self->arg) != Kernel.cfalse)
                 { if (b != Kernel.ctrue)
                   { if (boolean_I_any(b) == CTRUE)
                     res = z;
                    else res = Kernel.ctrue;
                      break;} 
                  } 
                else if (b == Kernel.ctrue)
                 { res= Kernel.cfalse;
                  break;} 
                } 
              GC_UNLOOP;} 
            } 
          GC_UNLOOP;} 
        } 
      else { OID gc_local;
          ITERATE(y);
          bag *y_support;
          y_support = GC_OBJECT(bag,enumerate_any(x));
          for (START(y_support); NEXT(y);)
          { GC_LOOP;
            { write_value_Variable(self->var,y);
              if (OPT_EVAL(self->arg) != Kernel.cfalse)
               { if (b != Kernel.ctrue)
                 { if (boolean_I_any(b) == CTRUE)
                   res = y;
                  else res = Kernel.ctrue;
                    break;} 
                } 
              else if (b == Kernel.ctrue)
               { res= Kernel.cfalse;
                break;} 
              } 
            GC_UNLOOP;} 
          } 
        Result = res;
      } 
    GC_UNBIND; return (Result);} 
  } 


// *********************************************************************
// *     Part 3: other control structures                              *
// *********************************************************************
// ----------------- case  --------------------------------------
/* The c++ function for: self_print(self:Case) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
void  self_print_Case_Language(Case *self)
{ GC_BIND;
  princ_string("case ");
  print_any(GC_OID(self->var));
  princ_string(" ");
  lbreak_integer(1);
  princ_string("(");
  { int  n = 1;
    int  m = self->args->length;
    (Core.pretty->index = (Core.pretty->index+1));
    { OID gc_local;
      while ((n <= m))
      { GC_LOOP;
        { int  _Zl = Core.pretty->index;
          printexp_any(GC_OID((*(self->args))[n]),CFALSE);
          princ_string(" ");
          if (buffer_length_void() > (Core.pretty->width-50))
           lbreak_integer(2);
          else set_level_void();
            print_any(GC_OID((*(self->args))[(n+1)]));
          (Core.pretty->index = _Zl);
          if ((n+1) != m)
           { princ_string(", ");
            lbreak_void();
            princ_string("");
            } 
          princ_string("");
          n= (n+2);
          } 
        GC_UNLOOP;} 
      } 
    princ_string(")");
    (Core.pretty->index = (Core.pretty->index-2));
    } 
  GC_UNBIND;} 


/* The c++ function for: self_eval(self:Case) [NEW_ALLOC] */
OID  self_eval_Case(Case *self)
{ GC_RESERVE(1);  // HOHO v3.0.55 optim !
  { OID Result = 0;
    { OID  truc = GC_OID(OPT_EVAL(self->var));
      ClaireBoolean * flip = CTRUE;
      OID  previous = Kernel.cfalse;
      { ClaireBoolean * g0055I;
        { OID V_C;{ OID gc_local;
            ITERATE(x);
            V_C= _oid_(CFALSE);
            for (START(self->args); NEXT(x);)
            { GC_LOOP;
              if (flip == CTRUE)
               { flip= CFALSE;
                GC__OID(previous = OPT_EVAL(x), 1);
                } 
              else if (belong_to(truc,previous) == CTRUE)
               { GC__OID(previous = OPT_EVAL(x), 1);
                { V_C = Kernel.ctrue;
                  break;} 
                } 
              else flip= CTRUE;
                GC_UNLOOP;} 
            } 
          
          g0055I=OBJECT(ClaireBoolean,V_C);} 
        
        if (g0055I == CTRUE) Result = previous;
          else Result = Kernel.cfalse;
        } 
      } 
    GC_UNBIND; return (Result);} 
  } 


// ------------------ WHILE  and UNTIL  -----------------------------
// the "other" while is until, where the first test is skipped
/* The c++ function for: self_print(self:While) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
void  self_print_While_Language(While *self)
{ GC_BIND;
  princ_string(((self->other == CTRUE) ?
    "until" :
    "while" ));
  princ_string(" ");
  printexp_any(GC_OID(self->test),CFALSE);
  princ_string(" ");
  lbreak_integer(2);
  print_any(GC_OID(self->arg));
  princ_string("");
  (Core.pretty->index = (Core.pretty->index-2));
  GC_UNBIND;} 


/* The c++ function for: self_eval(self:While) [NEW_ALLOC+RETURN_ARG] */
OID  self_eval_While(While *self)
{ GC_BIND;
  { OID Result = 0;
    { ClaireBoolean * a = self->other;
      ClaireBoolean * b = a;
      { ClaireHandler c_handle = ClaireHandler();
        if ERROR_IN 
        { { OID gc_local;
            ClaireBoolean *v_while;
            Result= _oid_(CFALSE);
            v_while = ((b == CTRUE) ? CTRUE : ((not_any(OPT_EVAL(self->test)) == a) ? CTRUE : CFALSE));
            
            while (v_while == CTRUE)
            { GC_LOOP;
              b= CFALSE;
              OPT_EVAL(self->arg);
              v_while = ((b == CTRUE) ? CTRUE : ((not_any(OPT_EVAL(self->test)) == a) ? CTRUE : CFALSE));
              GC_UNLOOP;} 
            } 
          ClEnv->cHandle--;} 
        else if (belong_to(_oid_(ClEnv->exception_I),_oid_(Core._return_error)) == CTRUE)
        { c_handle.catchIt();Result = (*Kernel.arg)(GC_OID(_oid_(ClEnv->exception_I)));
          } 
        else PREVIOUS_HANDLER;} 
      } 
    GC_UNBIND; return (Result);} 
  } 


//-------------- handling errors -----------------------------------
// This is the control structure associated with these errors. Its real
// semantics is defined in the C compiler file
//
/* The c++ function for: self_print(self:Handle) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
void  self_print_Handle_Language(ClaireHandle *self)
{ GC_BIND;
  princ_string("try ");
  print_any(GC_OID(self->arg));
  princ_string(" ");
  lbreak_integer(0);
  princ_string("catch ");
  print_any(GC_OID(self->test));
  princ_string(" ");
  print_any(GC_OID(self->other));
  princ_string("");
  (Core.pretty->index = (Core.pretty->index-2));
  GC_UNBIND;} 


/* The c++ function for: self_eval(self:Handle) [NEW_ALLOC+RETURN_ARG] */
OID  self_eval_Handle(ClaireHandle *self)
{ GC_BIND;
  { OID Result = 0;
    { OID  x = GC_OID(OPT_EVAL(self->test));
      { ClaireHandler c_handle = ClaireHandler();
        if ERROR_IN 
        { Result = OPT_EVAL(self->arg);
          ClEnv->cHandle--;} 
        else if (belong_to(_oid_(ClEnv->exception_I),x) == CTRUE)
        { c_handle.catchIt();if (INHERIT(ClEnv->exception_I->isa,Core._return_error))
           { OID  V_CL0058;close_exception(ClEnv->exception_I);
            
            Result=_void_(V_CL0058);} 
          else Result = OPT_EVAL(self->other);
            } 
        else PREVIOUS_HANDLER;} 
      } 
    GC_UNBIND; return (Result);} 
  } 


// <yc> 6/98
// *********************************************************************
// *     Part 4: the constructs                                         *
// *********************************************************************
// v3.2.16   constructor for arrays
/* The c++ function for: self_print(self:Construct) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
void  self_print_Construct_Language(Construct *self)
{ GC_BIND;
  { int  _Zl = Core.pretty->index;
    { char * g0059UU;
      if (INHERIT(self->isa,Language._List))
       g0059UU = "list";
      else if (INHERIT(self->isa,Language._Set))
       g0059UU = "set";
      else if (INHERIT(self->isa,Language._Tuple))
       g0059UU = "tuple";
      else if (INHERIT(self->isa,Language._Printf))
       g0059UU = "printf";
      else g0059UU = ((INHERIT(self->isa,Language._Error)) ?
        "error" :
        ((INHERIT(self->isa,Language._Trace)) ?
          "trace" :
          ((INHERIT(self->isa,Language._Assert)) ?
            "assert" :
            ((INHERIT(self->isa,Language._Branch)) ?
              "branch" :
              string_I_symbol(self->isa->name) ) ) ) );
      princ_string(g0059UU);
      } 
    if ((INHERIT(self->isa,Language._List)) || 
        (INHERIT(self->isa,Language._Set)))
     { OID  _Zt = GC_OID(get_property(Kernel.of,self));
      if (_Zt != CNULL)
       { if (equal(_Zt,_oid_(Kernel.emptySet)) != CTRUE)
         { princ_string("<");
          print_any(_Zt);
          princ_string(">");
          } 
        } 
      else ;} 
    princ_string("(");
    set_level_void();
    printbox_bag2(GC_OBJECT(list,self->args));
    princ_string(")");
    (Core.pretty->index = _Zl);
    } 
  GC_UNBIND;} 


// constructors: how to create a list, a set, a tuple or an array
// note that the constructor is typed
/* The c++ function for: self_eval(self:List) [NEW_ALLOC] */
OID  self_eval_List(List *self)
{ GC_BIND;
  { OID Result = 0;
    { list * l;
      { { bag *v_list; OID v_val;
          OID x,CLcount;
          v_list = self->args;
           l = v_list->clone();
          for (CLcount= 1; CLcount <= v_list->length; CLcount++)
          { x = (*(v_list))[CLcount];
            v_val = OPT_EVAL(x);
            
            (*((list *) l))[CLcount] = v_val;} 
          } 
        GC_OBJECT(list,l);} 
      if (((self->of == (NULL)) ? CTRUE : CFALSE) != CTRUE)
       { { OID  x;
          { { OID  x_some = CNULL;
              { ITERATE(x);
                for (START(l); NEXT(x);)
                if (belong_to(x,_oid_(self->of)) != CTRUE)
                 { x_some= x;
                  break;} 
                } 
              x = x_some;
              } 
            GC_OID(x);} 
          if (x != CNULL)
           close_exception(((range_error *) (*Core._range_error)(_oid_(self),
            x,
            GC_OID(_oid_(self->of)))));
          else ;} 
        Result = _oid_(cast_I_bag(l,self->of));
        } 
      else Result = _oid_(cast_I_bag(l,Kernel.emptySet));
        } 
    GC_UNBIND; return (Result);} 
  } 


// v3.2
/* The c++ function for: self_eval(self:Set) [NEW_ALLOC] */
OID  self_eval_Set(Set *self)
{ GC_BIND;
  { OID Result = 0;
    { set * s;
      { { list * g0060UU;
          { { bag *v_list; OID v_val;
              OID x,CLcount;
              v_list = self->args;
               g0060UU = v_list->clone();
              for (CLcount= 1; CLcount <= v_list->length; CLcount++)
              { x = (*(v_list))[CLcount];
                v_val = OPT_EVAL(x);
                
                (*((list *) g0060UU))[CLcount] = v_val;} 
              } 
            GC_OBJECT(list,g0060UU);} 
          s = set_I_bag(g0060UU);
          } 
        GC_OBJECT(set,s);} 
      if (((self->of == (NULL)) ? CTRUE : CFALSE) != CTRUE)
       { { OID  x;
          { { OID  x_some = CNULL;
              { ITERATE(x);
                for (START(s); NEXT(x);)
                if (belong_to(x,_oid_(self->of)) != CTRUE)
                 { x_some= x;
                  break;} 
                } 
              x = x_some;
              } 
            GC_OID(x);} 
          if (x != CNULL)
           close_exception(((range_error *) (*Core._range_error)(_oid_(self),
            x,
            GC_OID(_oid_(self->of)))));
          else ;} 
        Result = _oid_(cast_I_bag(s,self->of));
        } 
      else Result = _oid_(cast_I_bag(s,Kernel.emptySet));
        } 
    GC_UNBIND; return (Result);} 
  } 


/* The c++ function for: self_eval(self:Tuple) [NEW_ALLOC] */
OID  self_eval_Tuple(Tuple *self)
{ GC_BIND;
  { OID Result = 0;
    { tuple * V_CL0061;{ list * g0062UU;
        { { bag *v_list; OID v_val;
            OID x,CLcount;
            v_list = self->args;
             g0062UU = v_list->clone();
            for (CLcount= 1; CLcount <= v_list->length; CLcount++)
            { x = (*(v_list))[CLcount];
              v_val = OPT_EVAL(x);
              
              (*((list *) g0062UU))[CLcount] = v_val;} 
            } 
          GC_OBJECT(list,g0062UU);} 
        V_CL0061 = tuple_I_list(g0062UU);
        } 
      
      Result=_oid_(V_CL0061);} 
    GC_UNBIND; return (Result);} 
  } 


// v3.2.16 - introduce a nicer way to create an array
/* The c++ function for: self_eval(self:Array) [NEW_ALLOC] */
OID  self_eval_Array2(Array *self)
{ GC_BIND;
  { OID Result = 0;
    { list * l;
      { { bag *v_list; OID v_val;
          OID x,CLcount;
          v_list = self->args;
           l = v_list->clone();
          for (CLcount= 1; CLcount <= v_list->length; CLcount++)
          { x = (*(v_list))[CLcount];
            v_val = OPT_EVAL(x);
            
            (*((list *) l))[CLcount] = v_val;} 
          } 
        GC_OBJECT(list,l);} 
      if (((self->of == (NULL)) ? CTRUE : CFALSE) != CTRUE)
       { { OID  x;
          { { OID  x_some = CNULL;
              { ITERATE(x);
                for (START(l); NEXT(x);)
                if (belong_to(x,_oid_(self->of)) != CTRUE)
                 { x_some= x;
                  break;} 
                } 
              x = x_some;
              } 
            GC_OID(x);} 
          if (x != CNULL)
           close_exception(((range_error *) (*Core._range_error)(_oid_(self),
            x,
            GC_OID(_oid_(self->of)))));
          else ;} 
        cast_I_bag(l,self->of);
        } 
      else cast_I_bag(l,Kernel._any);
        Result = _array_(array_I_list(l));
      } 
    GC_UNBIND; return (Result);} 
  } 


// Macros are a nice but undocumented feature of CLAIRE. This is deliberate
//
/* The c++ function for: self_eval(self:Macro) [NEW_ALLOC] */
OID  self_eval_Macro2(Macro *self)
{ GC_BIND;
  { OID Result = 0;
    Result = eval_any(GC_OID((*Language.macroexpand)(_oid_(self))));
    GC_UNBIND; return (Result);} 
  } 


// error produces an exception of type general_error
/* The c++ function for: self_eval(self:Error) [NEW_ALLOC+SLOT_UPDATE+RETURN_ARG] */
void  self_eval_Error(Error *self)
{ GC_BIND;
  if ((boolean_I_any(_oid_(self->args)) != CTRUE) || 
      (equal(_oid_(Kernel._string),_oid_(OWNER((*(self->args))[1]))) != CTRUE))
   close_exception(((general_error *) (*Core._general_error)(_string_("Syntax error: ~S"),
    _oid_(list::alloc(1,_oid_(self))))));
  { general_error * x = GC_OBJECT(general_error,((general_error *) new_object_class(Core._general_error)));
    (x->cause = car_list(self->args));
    { general_error * g0063 = x; 
      OID  g0064;
      { list * V_CL0065;{ bag *v_list; OID v_val;
          OID x,CLcount;
          v_list = GC_OBJECT(list,cdr_list(GC_OBJECT(list,self->args)));
           V_CL0065 = v_list->clone();
          for (CLcount= 1; CLcount <= v_list->length; CLcount++)
          { x = (*(v_list))[CLcount];
            v_val = OPT_EVAL(x);
            
            (*((list *) V_CL0065))[CLcount] = v_val;} 
          } 
        
        g0064=_oid_(V_CL0065);} 
      (g0063->arg = g0064);} 
    close_exception(x);
    } 
  GC_UNBIND;} 


// this is the basic tool for printing in CLAIRE. A complex statement
// is macroexpanded into basic printing instructions
//
/* The c++ function for: self_eval(self:Printf) [NEW_ALLOC+SLOT_UPDATE] */
OID  self_eval_Printf(Printf *self)
{ GC_RESERVE(6);  // v3.0.55 optim !
  { OID Result = 0;
    { list * l = GC_OBJECT(list,self->args);
      OID  s = (*(l))[1];
      if (equal(_oid_(Kernel._string),_oid_(OWNER(s))) != CTRUE)
       close_exception(((general_error *) (*Core._general_error)(_string_("[102] the first argument in ~S must be a string"),
        _oid_(list::alloc(1,_oid_(self))))));
      else { int  i = 2;
          OID  n = GC_OID((*Kernel.get)(s,
            _oid_(_char_('~'))));
          { OID gc_local;
            while ((equal(n,0) != CTRUE))
            { GC_LOOP;
              { OID  m = GC_OID((*Kernel.nth)(s,
                  ((n)+1)));
                if (i > l->length)
                 close_exception(((general_error *) (*Core._general_error)(_string_("[103] not enough arguments in ~S"),
                  _oid_(list::alloc(1,_oid_(self))))));
                if ((OBJECT(ClaireBoolean,(*Kernel._sup)(n,
                  1))) == CTRUE)
                 princ_string(substring_string(string_v(s),1,((n)-1)));
                if (_oid_(_char_('A')) == m)
                 (*Kernel.princ)(GC_OID(OPT_EVAL((*(l))[i])));
                else if (_oid_(_char_('S')) == m)
                 print_any(GC_OID(OPT_EVAL((*(l))[i])));
                else if (_oid_(_char_('I')) == m)
                 OPT_EVAL((*(l))[i]);
                ++i;
                GC__OID(s = _string_(substring_string(string_v(s),((n)+2),1000)), 3);
                GC__OID(n = (*Kernel.get)(s,
                  _oid_(_char_('~'))), 5);
                } 
              GC_UNLOOP;} 
            } 
          if (boolean_I_any(s) == CTRUE)
           (*Kernel.princ)(s);
          } 
        Result = CNULL;
      } 
    GC_UNBIND; return (Result);} 
  } 


// trace is refined in inspect.cl
// If trace_output() is known, use it, else use current output.
//
/* The c++ function for: self_eval(self:Trace) [NEW_ALLOC+SLOT_UPDATE] */
OID  self_eval_Trace(Trace *self)
{ GC_BIND;
  { OID Result = 0;
    { list * a = GC_OBJECT(list,self->args);
      list * l;
      { { bag *v_list; OID v_val;
          OID x,CLcount;
          v_list = a;
           l = v_list->clone();
          for (CLcount= 1; CLcount <= v_list->length; CLcount++)
          { x = (*(v_list))[CLcount];
            v_val = OPT_EVAL(x);
            
            (*((list *) l))[CLcount] = v_val;} 
          } 
        GC_OBJECT(list,l);} 
      OID  i = (*(l))[1];
      OID  a2;
      if (a->length > 1)
       a2 = (*(a))[2];
      else a2 = Kernel.cfalse;
        if ((Kernel._string == OWNER(a2)) && 
          ((INHERIT(OWNER(i),Kernel._integer)) && (i <= ClEnv->verbose)))
       { OID  p = GC_OID(get_property(Kernel.ctrace,ClEnv));
        if (p != CNULL)
         p= GC_OID(ClAlloc->import(Kernel._port,(int *) use_as_output_port(EXPORT((ClairePort *),p))));
        format_string(string_v(a2),skip_list(l,2));
        if (p != CNULL)
         use_as_output_port(EXPORT((ClairePort *),p));
        Result = _oid_(Kernel.emptySet);
        } 
      else Result = Kernel.cfalse;
        } 
    GC_UNBIND; return (Result);} 
  } 


// assert is refined in trace.la
//
/* The c++ function for: self_eval(self:Assert) [NEW_ALLOC+SLOT_UPDATE] */
OID  self_eval_Assert(Assert *self)
{ GC_BIND;
  { OID Result = 0;
    { list * a = GC_OBJECT(list,self->args);
      if ((a->length > 0) && 
          ((get_property(Kernel.ctrace,ClEnv) != CNULL) && 
            (boolean_I_any(OPT_EVAL((*(a))[1])) != CTRUE)))
       { ClairePort * p = use_as_output_port(ClEnv->ctrace);
        print_any(GC_OID(_string_(self->external)));
        princ_string(",line=");
        princ_integer(self->index);
        princ_string(": (ASSERT) ");
        print_any((*(a))[1]);
        princ_string("\n");
        use_as_output_port(p);
        if (0 <= ClEnv->debug_I)
         close_exception(((general_error *) (*Core._general_error)(_string_("Assertion Violation"),
          _oid_(Kernel.nil))));
        Result = _oid_(Kernel.emptySet);
        } 
      else Result = Kernel.cfalse;
        } 
    GC_UNBIND; return (Result);} 
  } 


/* The c++ function for: self_eval(self:Branch) [NEW_ALLOC] */
OID  self_eval_Branch(Branch *self)
{ GC_BIND;
  if (self->args->length != 1)
   close_exception(((general_error *) (*Core._general_error)(_string_("[104] Syntax error with ~S (one arg. expected)"),
    _oid_(list::alloc(1,_oid_(self))))));
  { OID Result = 0;
    { ClaireHandler c_handle = ClaireHandler();
      if ERROR_IN 
      { { world_push();
          if (OPT_EVAL((*(self->args))[1]) != Kernel.cfalse)
           Result = Kernel.ctrue;
          else { world_pop();
              Result = Kernel.cfalse;
              } 
            } 
        ClEnv->cHandle--;} 
      else if (belong_to(_oid_(ClEnv->exception_I),_oid_(Core._contradiction)) == CTRUE)
      { c_handle.catchIt();{ world_pop();
          Result = Kernel.cfalse;
          } 
        } 
      else PREVIOUS_HANDLER;} 
    GC_UNBIND; return (Result);} 
  } 


// end of file