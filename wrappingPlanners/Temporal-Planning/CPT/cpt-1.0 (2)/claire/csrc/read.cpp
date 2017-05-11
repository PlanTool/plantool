/***** CLAIRE Compilation of file c:\claire\v3.3\src\meta\read.cl 
         [version 3.3.4 / safety 5] Sat Oct 16 06:53:32 2004 *****/

#include <claire.h>
#include <Kernel.h>
#include <Core.h>
#include <Language.h>
#include <Reader.h>

//+-------------------------------------------------------------+
//| CLAIRE                                                      |
//| reader.cl                                                   |
//| Copyright (C) 1994 - 2003 Yves Caseau. All Rights Reserved  |
//| cf. copyright info in file object.cl: about()               |
//+-------------------------------------------------------------+
// ---------------------------------------------------------------------
// Contents:
//   Part 1: the reader object
//   Part 2: lexical analysis
//   Part 3: reading structures	
//   Part 4: Variable Handling
// ---------------------------------------------------------------------
// **********************************************************************
// *   Part 1: The reader object                                        *
// **********************************************************************
// global definitions
// *arrow*:boolean :: false
// here we define the basic keywords
/* The c++ function for: keyword?(x:any) [0] */
ClaireBoolean * keyword_ask_any(OID x)
{ return (inherit_ask_class(OWNER(x),Reader._reserved_keyword));} 


// the meta_class of the reader --------------------------------------
// The key values are placed in indexed so that they can be changed (eof ...).
// The slot *internal* is used to give addresses to lexical variables.
// The function next reads a character and places it in the slot first.
//
// closing brace as a CLAIRE entity
/* The c++ function for: next(r:meta_reader) [0] */
int  next_meta_reader(meta_reader *r)
{ return (((int) r->fromp->getNext()));} 


/* The c++ function for: firstc(r:meta_reader) [0] */
int  firstc_meta_reader(meta_reader *r)
{ return (r->fromp->firstc);} 


/* The c++ function for: stop?(n:integer) [0] */
OID  stop_ask_integer(int n)
{ return (_oid_(((n == 44) ? CTRUE : ((n == 41) ? CTRUE : ((n == 93) ? CTRUE : ((n == 125) ? CTRUE : CFALSE))))));} 


// sugar in file
// this is used to keep comments when translating CLAIRE to another language
// read the next unit (definition, block or expression)
//
/* The c++ function for: nextunit(r:meta_reader) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
OID  nextunit_meta_reader(meta_reader *r)
{ GC_BIND;
  { OID Result = 0;
    { OID  n = skipc_meta_reader(r);
      if (n == r->eof)
       { next_meta_reader(r);
        Result = _oid_(Reader.eof);
        } 
      else if (n == 91)
       { OID  z = GC_OID(nexte_meta_reader(cnext_meta_reader(r)));
        Result = nextdefinition_meta_reader(r,z,GC_OID(nexte_meta_reader(r)),CTRUE);
        } 
      else if (n == 40)
       { if (r->toplevel == CTRUE)
         Result = nexts_meta_reader(r,Reader.none);
        else Result = readblock_meta_reader(r,GC_OID(nexte_meta_reader(cnext_meta_reader(r))),41);
          } 
      else if (n == 96)
       { Quote * _CL_obj = ((Quote *) GC_OBJECT(Quote,new_object_class(Language._Quote)));
        (_CL_obj->arg = nextunit_meta_reader(cnext_meta_reader(r)));
        add_I_property(Kernel.instances,Language._Quote,11,_oid_(_CL_obj));
        Result = _oid_(_CL_obj);
        } 
      else if (n == 59)
       { { OID gc_local;
          while (((firstc_meta_reader(r) != r->eof) && 
              (firstc_meta_reader(r) != 10)))
          { next_meta_reader(r);
            } 
          } 
        if (firstc_meta_reader(r) == r->eof)
         Result = _oid_(Reader.eof);
        else { (r->nb_line = (r->nb_line+1));
            next_meta_reader(r);
            Result = nextunit_meta_reader(r);
            } 
          } 
      else { OID  x;
          { if (r->toplevel == CTRUE)
             x = nexts_meta_reader(r,Reader.none);
            else x = nextexp_meta_reader(r,CTRUE);
              GC_OID(x);} 
          if ((r->toplevel == CTRUE) && 
              ((INHERIT(OWNER(x),Language._Assign)) && (INHERIT(OWNER(OBJECT(Assign,x)->var),Language._Vardef))))
           { Defobj * _CL_obj = ((Defobj *) GC_OBJECT(Defobj,new_object_class(Language._Defobj)));
            (_CL_obj->ident = OBJECT(Variable,(*Language.var)(x))->pname);
            (_CL_obj->arg = Core._global_variable);
            { Definition * g0003 = _CL_obj; 
              list * g0004;
              { OID v_bag;
                GC_ANY(g0004= list::empty(Kernel.emptySet));
                { { Call * _CL_obj = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                    (_CL_obj->selector = Kernel._equal);
                    (_CL_obj->args = list::alloc(2,_oid_(Kernel.range),GC_OID(_oid_(extract_type_any(GC_OID((*Kernel.range)(GC_OID((*Language.var)(x)))))))));
                    add_I_property(Kernel.instances,Language._Call,11,_oid_(_CL_obj));
                    v_bag = _oid_(_CL_obj);
                    } 
                  GC_OID(v_bag);} 
                ((list *) g0004)->addFast(v_bag);
                { { Call * _CL_obj = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                    (_CL_obj->selector = Kernel._equal);
                    (_CL_obj->args = list::alloc(2,_oid_(Kernel.value),GC_OID((*Kernel.arg)(x))));
                    add_I_property(Kernel.instances,Language._Call,11,_oid_(_CL_obj));
                    v_bag = _oid_(_CL_obj);
                    } 
                  GC_OID(v_bag);} 
                ((list *) g0004)->addFast(v_bag);} 
              (g0003->args = g0004);} 
            add_I_property(Kernel.instances,Language._Defobj,11,_oid_(_CL_obj));
            Result = _oid_(_CL_obj);
            } 
          else if (Kernel._string == OWNER(x))
           Result = x;
          else { ClaireBoolean * g0005I;
            if (INHERIT(OWNER(x),Language._Call))
             { ClaireBoolean *v_and;
              { v_and = contain_ask_set(r->s_properties,_oid_(OBJECT(Call,x)->selector));
                if (v_and == CFALSE) g0005I =CFALSE; 
                else { { OID  g0006UU;
                    { OID gc_local;
                      ITERATE(y);
                      g0006UU= _oid_(CFALSE);
                      bag *y_support;
                      y_support = GC_OBJECT(list,OBJECT(Call,x)->args);
                      for (START(y_support); NEXT(y);)
                      if (not_any(_oid_(inherit_ask_class(OWNER(y),Language._Vardef))) != CTRUE)
                       { g0006UU = Kernel.ctrue;
                        break;} 
                      } 
                    v_and = not_any(g0006UU);
                    } 
                  if (v_and == CFALSE) g0005I =CFALSE; 
                  else g0005I = CTRUE;} 
                } 
              } 
            else g0005I = CFALSE;
              
            if (g0005I == CTRUE) { Call * z = OBJECT(Call,x);
                OID  a = GC_OID((*(z->args))[1]);
                if ((z->selector == Kernel.begin) && 
                    (INHERIT(OWNER(a),Kernel._unbound_symbol)))
                 ((*(z->args))[1]=_string_(string_I_symbol(extract_symbol_any(a))));
                if ((z->selector == Kernel.end) && 
                    (INHERIT(OWNER(a),Kernel._module)))
                 ((*(z->args))[1]=_oid_(claire.it));
                Result = x;
                } 
              else if (((r->toplevel == CTRUE) ? CTRUE : ((INHERIT(OWNER(x),Language._Assert)) ? CTRUE : CFALSE)) != CTRUE)
             Result = nextdefinition_meta_reader(r,x,GC_OID(nexte_meta_reader(r)),CFALSE);
            else Result = x;
              } 
          } 
        } 
    GC_UNBIND; return (Result);} 
  } 


// read the next statement & stops at the keyword e or at a delimiter
// the keyword has been read but not the delimiter, so we know which case
// by testing stop?(first(r))
// Note: it actually reads a fragment
//
/* The c++ function for: nexts(r:meta_reader,e:keyword) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
OID  nexts_meta_reader(meta_reader *r,keyword *e)
{ GC_BIND;
  { OID Result = 0;
    { OID  n = skipc_meta_reader(r);
      if (n == r->eof)
       { next_meta_reader(r);
        Result = _oid_(Reader.eof);
        } 
      else if (n == 91)
       { OID  z = GC_OID(nexte_meta_reader(cnext_meta_reader(r)));
        Result = nextdefinition_meta_reader(r,z,GC_OID(nexte_meta_reader(r)),CTRUE);
        } 
      else if (e == Reader.None)
       Result = nexte_meta_reader(r);
      else { OID  x = GC_OID(nexte_meta_reader(r));
          if (keyword_ask_any(x) == CTRUE)
           Result = nextstruct_meta_reader(r,OBJECT(keyword,x),e);
          else if ((e == Reader.none) && 
              ((INHERIT(OWNER(x),Language._Call)) && (contain_ask_set(r->s_properties,_oid_(OBJECT(Call,x)->selector)) == CTRUE)))
           Result = x;
          else Result = loopexp_meta_reader(r,x,e,CFALSE);
            } 
        } 
    GC_UNBIND; return (Result);} 
  } 


// loops until the right expression is built (ends with e ',', '}' or ')')
//
/* The c++ function for: loopexp(r:meta_reader,x:any,e:keyword,loop:boolean) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
OID  loopexp_meta_reader(meta_reader *r,OID x,keyword *e,ClaireBoolean *loop)
{ if ((r->toplevel == CTRUE) && 
      ((e == Reader.none) && 
        (findeol_meta_reader(r) == CTRUE))) 
  { { OID Result = 0;
      Result = x;
      return (Result);} 
     } 
  else{ GC_BIND;
    { OID Result = 0;
      if (x == _oid_(Reader._ask))
       { Call * _CL_obj = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
        (_CL_obj->selector = Reader.inspect);
        (_CL_obj->args = list::alloc(1,GC_OID(nexte_meta_reader(r))));
        add_I_property(Kernel.instances,Language._Call,11,_oid_(_CL_obj));
        Result = _oid_(_CL_obj);
        } 
      else if (skipc_meta_reader(r) == 58)
       { OID  y = GC_OID(nexte_meta_reader(cnext_meta_reader(r)));
        if (y == _oid_(Kernel._equal))
         Result = loopexp_meta_reader(r,GC_OID(combine_any(x,_oid_(Reader.L__equal),GC_OID(nexte_meta_reader(r)))),e,CTRUE);
        else if (y == _oid_(Reader.L_))
         Result = nextinst_meta_reader(r,x);
        else if (operation_ask_any(y) == CTRUE)
         Result = extended_operator_property(OBJECT(property,y),x,GC_OID(loopexp_meta_reader(r,GC_OID(nexte_meta_reader(r)),e,CFALSE)));
        else if (INHERIT(OWNER(x),Language._Call))
         { OID  w = GC_OID(nexte_meta_reader(r));
          if (w == _oid_(Reader._equal_sup))
           (r->last_arrow = CTRUE);
          else if (((equal(w,Reader.arrow->value) == CTRUE) ? CTRUE : ((w == _oid_(Reader.L__equal)) ? CTRUE : CFALSE)) != CTRUE)
           Serror_string("[149] wrong keyword (~S) after ~S",list::alloc(2,w,y));
          Result = nextmethod_meta_reader(r,
            x,
            y,
            equal(w,_oid_(Reader.L__equal)),
            CFALSE,
            equal(w,_oid_(Reader._equal_sup)));
          } 
        else Serror_string("[150] Illegal use of :~S after ~S",list::alloc(2,y,x));
          } 
      else { OID  y = GC_OID(nexte_meta_reader(r));
          if ((y == _oid_(e)) || 
              ((y == _oid_(Reader._equal_sup)) && 
                  (_oid_(e) == Reader.arrow->value)))
           { if (y != _oid_(e))
             (r->last_arrow = CTRUE);
            if (boolean_I_any(stop_ask_integer(firstc_meta_reader(r))) == CTRUE)
             Serror_string("[151] ~S not allowed after ~S\n",list::alloc(2,_oid_(char_I_integer(firstc_meta_reader(r))),_oid_(e)));
            else Result = x;
              } 
          else if ((equal(y,Reader.triangle->value) == CTRUE) || 
              ((equal(y,Reader.arrow->value) == CTRUE) || 
                ((y == _oid_(Reader.L_)) || 
                  ((y == _oid_(Reader.L_L_)) || 
                    (y == _oid_(Reader._equal_sup))))))
           Result = nextdefinition_meta_reader(r,x,y,CFALSE);
          else if ((INHERIT(OWNER(y),Reader._delimiter)) && 
              (boolean_I_any(stop_ask_integer(firstc_meta_reader(r))) == CTRUE))
           Result = x;
          else if (operation_ask_any(y) == CTRUE)
           { if (loop == CTRUE)
             Result = loopexp_meta_reader(r,GC_OID(combine_any(x,y,GC_OID(nexte_meta_reader(r)))),e,CTRUE);
            else Result = loopexp_meta_reader(r,GC_OID(combine_I_any(x,y,GC_OID(nexte_meta_reader(r)))),e,CTRUE);
              } 
          else Serror_string("[152] Separation missing between ~S \nand ~S [~S?]",list::alloc(3,x,
              y,
              _oid_(e)));
            } 
        GC_UNBIND; return (Result);} 
    } 
  } 


// this is the special form for x :op y - new in v3.3.32
/* The c++ function for: extended_operator(p:property,x:any,y:any) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
OID  extended_operator_property(property *p,OID x,OID y)
{ GC_BIND;
  { OID Result = 0;
    if (INHERIT(OWNER(x),Language._Call))
     { OID  r;
      { if (OBJECT(Call,x)->selector == Kernel.nth)
         r = (*(OBJECT(Call,x)->args))[2];
        else r = (*(OBJECT(Call,x)->args))[1];
          GC_OID(r);} 
      Variable * v;
      { { Variable * _CL_obj = ((Variable *) GC_OBJECT(Variable,new_object_class(Language._Variable)));
          (_CL_obj->pname = gensym_void());
          add_I_property(Kernel.instances,Language._Variable,11,_oid_(_CL_obj));
          v = _CL_obj;
          } 
        GC_OBJECT(Variable,v);} 
      Call * x2;
      if (OBJECT(Call,x)->selector == Kernel.nth)
       { Call * _CL_obj = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
        (_CL_obj->selector = Kernel.nth);
        (_CL_obj->args = list::alloc(2,GC_OID((*(OBJECT(Call,x)->args))[1]),_oid_(v)));
        add_I_property(Kernel.instances,Language._Call,11,_oid_(_CL_obj));
        x2 = _CL_obj;
        } 
      else { Call * _CL_obj = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
          (_CL_obj->selector = OBJECT(Call,x)->selector);
          (_CL_obj->args = list::alloc(1,_oid_(v)));
          add_I_property(Kernel.instances,Language._Call,11,_oid_(_CL_obj));
          x2 = _CL_obj;
          } 
        if (INHERIT(OWNER(r),Language._Call))
       { Let * _CL_obj = ((Let *) GC_OBJECT(Let,new_object_class(Language._Let)));
        (_CL_obj->var = v);
        (_CL_obj->value = r);
        (_CL_obj->arg = combine_any(_oid_(x2),_oid_(Reader.L__equal),GC_OID(combine_any(_oid_(x2),_oid_(p),y))));
        add_I_property(Kernel.instances,Language._Let,11,_oid_(_CL_obj));
        Result = _oid_(_CL_obj);
        } 
      else Result = combine_any(x,_oid_(Reader.L__equal),GC_OID(combine_any(x,_oid_(p),y)));
        } 
    else Result = combine_any(x,_oid_(Reader.L__equal),GC_OID(combine_any(x,_oid_(p),y)));
      GC_UNBIND; return (Result);} 
  } 


// reading the next compact expression - comments are ignored but they can
// be attached to the last read expression
//
/* The c++ function for: nexte(r:meta_reader) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
OID  nexte_meta_reader(meta_reader *r)
{ GC_BIND;
  { OID Result = 0;
    { OID  x = GC_OID(nextexp_meta_reader(r,CFALSE));
      if (INHERIT(OWNER(x),Language._Instruction))
       (r->last_form = x);
      Result = x;
      } 
    GC_UNBIND; return (Result);} 
  } 


// v3.3
// reading the next compact expression/ same
//
/* The c++ function for: nextexp(r:meta_reader,str:boolean) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
OID  nextexp_meta_reader(meta_reader *r,ClaireBoolean *str)
{ GC_RESERVE(10);  // v3.0.55 optim !
  { OID Result = 0;
    { int  n = skipc_meta_reader(r);
      if (n == 41)
       Result = r->paren;
      else if (n == 125)
       Result = r->curly;
      else if (n == 93)
       Result = r->bracket;
      else if (n == 124)
       { next_meta_reader(r);
        Result = Reader.OR->value;
        } 
      else if (n == 44)
       Result = r->comma;
      else if (n == r->eof)
       Serror_string("[153] eof inside an expression",Kernel.nil);
      else if (n == 59)
       { { OID gc_local;
          while (((firstc_meta_reader(r) != r->eof) && 
              (firstc_meta_reader(r) != 10)))
          { next_meta_reader(r);
            } 
          } 
        if (firstc_meta_reader(r) == r->eof)
         Result = _oid_(Reader.eof);
        else { (r->nb_line = (r->nb_line+1));
            next_meta_reader(r);
            Result = nexte_meta_reader(r);
            } 
          } 
      else if (n == 35)
       Result = read_escape_meta_reader(r);
      else if (n == 96)
       { Quote * _CL_obj = ((Quote *) GC_OBJECT(Quote,new_object_class(Language._Quote)));
        (_CL_obj->arg = nexte_meta_reader(cnext_meta_reader(r)));
        add_I_property(Kernel.instances,Language._Quote,11,_oid_(_CL_obj));
        Result = _oid_(_CL_obj);
        } 
      else { OID  y = CNULL;
          OID  x;
          { if (n == 34)
             x = _string_(read_string_port(cnext_meta_reader(r)->fromp));
            else if (n == 40)
             x = readblock_meta_reader(r,GC_OID(nexte_meta_reader(cnext_meta_reader(r))),41);
            else if ((48 <= n) && 
                (n <= 57))
             x = read_number_port(r->fromp);
            else if (n == 123)
             x = readset_meta_reader(r,GC_OID(nexte_meta_reader(cnext_meta_reader(r))));
            else { y= read_ident_port(r->fromp);
                if (Kernel._string == OWNER(y))
                 x = y;
                else x = nexti_meta_reader(r,y);
                  } 
              GC_OID(x);} 
          if (Kernel._string == OWNER(y))
           { if (extended_comment_ask_meta_reader(r,string_v(y)) == CTRUE)
             Result = extended_comment_I_meta_reader(r,string_v(y));
            else if (str == CTRUE)
             Result = y;
            else Result = nexte_meta_reader(r);
              } 
          else { { OID gc_local;
                while (((firstc_meta_reader(r) == 91) || 
                    ((firstc_meta_reader(r) == 46) || 
                      (firstc_meta_reader(r) == 60))))
                { GC_LOOP;
                  if (firstc_meta_reader(r) == 60)
                   { OID  y = GC_OID(nexte_meta_reader(cnext_meta_reader(r)));
                    if ((INHERIT(OWNER(x),Kernel._class)) && 
                        (firstc_meta_reader(r) == 62))
                     { cnext_meta_reader(r);
                      { { ClaireObject * V_CL0008;{ list * g0009UU;
                            { OID v_bag;
                              GC_ANY(g0009UU= list::empty(Kernel.emptySet));
                              { { Call * _CL_obj = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                                  (_CL_obj->selector = Kernel._equal);
                                  (_CL_obj->args = list::alloc(2,_oid_(Kernel.of),y));
                                  add_I_property(Kernel.instances,Language._Call,11,_oid_(_CL_obj));
                                  v_bag = _oid_(_CL_obj);
                                  } 
                                GC_OID(v_bag);} 
                              ((list *) g0009UU)->addFast(v_bag);} 
                            V_CL0008 = extract_class_call_class(OBJECT(ClaireClass,x),g0009UU);
                            } 
                          
                          x=_oid_(V_CL0008);} 
                         GC__OID(x, 6);} 
                      GC__OID(x = nexti_meta_reader(r,x), 6);
                      } 
                    else Serror_string("[154] ~S<~S not allowed",list::alloc(2,x,y));
                      } 
                  else if (firstc_meta_reader(r) == 91)
                   { OID  l = GC_OID(nextseq_meta_reader(cnext_meta_reader(r),93));
                    { if ((INHERIT(OWNER(x),Kernel._class)) && 
                          ((x != _oid_(Kernel._type)) && 
                            (boolean_I_any(l) == CTRUE)))
                       x = _oid_(extract_class_call_class(OBJECT(ClaireClass,x),OBJECT(list,l)));
                      else x = _oid_(Call_I_property(Kernel.nth,GC_OBJECT(list,cons_any(x,OBJECT(list,l)))));
                         GC__OID(x, 6);} 
                    } 
                  else { OID  y = read_ident_port(cnext_meta_reader(r)->fromp);
                      property * p = make_a_property_any(y);
                      { { Call_plus * _CL_obj = ((Call_plus *) GC_OBJECT(Call_plus,new_object_class(Language._Call_plus)));
                          (_CL_obj->selector = p);
                          (_CL_obj->args = list::alloc(1,x));
                          add_I_property(Kernel.instances,Language._Call_plus,11,_oid_(_CL_obj));
                          x = _oid_(_CL_obj);
                          } 
                         GC__OID(x, 6);} 
                      if (p->reified == CTRUE)
                       { { Call * _CL_obj = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                          (_CL_obj->selector = Core.read);
                          (_CL_obj->args = list::alloc(1,x));
                          add_I_property(Kernel.instances,Language._Call,11,_oid_(_CL_obj));
                          x = _oid_(_CL_obj);
                          } 
                         GC__OID(x, 6);} 
                      } 
                    GC_UNLOOP;} 
                } 
              Result = x;
              } 
            } 
        } 
    GC_UNBIND; return (Result);} 
  } 


// reads a compact expression that starts with an ident
//
/* The c++ function for: nexti(r:meta_reader,val:any) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
OID  nexti_meta_reader(meta_reader *r,OID val)
{ GC_BIND;
  { OID Result = 0;
    if (firstc_meta_reader(r) == 40)
     { if ((val == _oid_(Reader.forall)) || 
          ((val == _oid_(Reader.exists)) || 
            (val == _oid_(Reader.some))))
       { Variable * v = GC_OBJECT(Variable,extract_variable_any(GC_OID(nexte_meta_reader(cnext_meta_reader(r)))));
        OID  _Za2 = GC_OID(nexte_meta_reader(r));
        OID  _Za3 = _oid_(Kernel._any);
        if (_Za2 == _oid_(Reader.in))
         { _Za3= GC_OID(nexte_meta_reader(r));
          if (equal(GC_OID(nexte_meta_reader(r)),GC_OID(Reader.OR->value)) != CTRUE)
           Serror_string("[155] missing | in exists / forall",Kernel.nil);
          } 
        else if (equal(_Za2,r->comma) == CTRUE)
         cnext_meta_reader(r);
        else Serror_string("[156] wrong use of exists(~S ~S ...",list::alloc(2,_oid_(v),_Za2));
          { Exists * _CL_obj = ((Exists *) GC_OBJECT(Exists,new_object_class(Language._Exists)));
          (_CL_obj->var = v);
          (_CL_obj->set_arg = _Za3);
          { Iteration * g0010 = _CL_obj; 
            OID  g0011;
            { list * _Zbind = GC_OBJECT(list,bind_I_meta_reader(r,v));
              OID  x = GC_OID(nexts_I_meta_reader2(r,41));
              unbind_I_meta_reader(r,_Zbind);
              g0011 = x;
              } 
            (g0010->arg = g0011);} 
          { Exists * g0012 = _CL_obj; 
            OID  g0013;
            if (val == _oid_(Reader.forall))
             g0013 = Kernel.ctrue;
            else if (val == _oid_(Reader.exists))
             g0013 = Kernel.cfalse;
            else g0013 = CNULL;
              (g0012->other = g0013);} 
          add_I_property(Kernel.instances,Language._Exists,11,_oid_(_CL_obj));
          Result = _oid_(_CL_obj);
          } 
        } 
      else if (val == _oid_(Reader.rule))
       { cnext_meta_reader(r);
        Result = val;
        } 
      else Result = readcall_meta_reader(r,val,CNULL);
        } 
    else if ((val == _oid_(Kernel._list)) && 
        (firstc_meta_reader(r) == 123))
     { OID  s = GC_OID(readset_meta_reader(r,GC_OID(nexte_meta_reader(cnext_meta_reader(r)))));
      if (INHERIT(OWNER(s),Language._Image))
       { (OBJECT(ClaireObject,s)->isa = Language._Collect);
        Result = s;
        } 
      else if (INHERIT(OWNER(s),Language._Select))
       { (OBJECT(ClaireObject,s)->isa = Language._Lselect);
        Result = s;
        } 
      else Serror_string("[157] ~S cannot follow list{",list::alloc(1,s));
        } 
    else if (((INHERIT(OWNER(val),Language._Call)) && ((OBJECT(Call,val)->selector == Kernel.nth) && 
          ((*(OBJECT(Call,val)->args))[1] == _oid_(Kernel._list)))) && 
        (firstc_meta_reader(r) == 123))
     { OID  s = GC_OID(readset_meta_reader(r,GC_OID(nexte_meta_reader(cnext_meta_reader(r)))));
      ClaireType * x = GC_OBJECT(ClaireType,extract_of_type_Call(OBJECT(Call,val)));
      if (INHERIT(OWNER(s),Language._Image))
       { (OBJECT(ClaireObject,s)->isa = Language._Collect);
        (OBJECT(Image,s)->of = x);
        Result = s;
        } 
      else if (INHERIT(OWNER(s),Language._Select))
       { (OBJECT(ClaireObject,s)->isa = Language._Lselect);
        (OBJECT(Select,s)->of = x);
        Result = s;
        } 
      else Serror_string("[157] ~S cannot follow list{",list::alloc(1,s));
        } 
    else if (((INHERIT(OWNER(val),Language._Call)) && ((OBJECT(Call,val)->selector == Kernel.nth) && 
          ((*(OBJECT(Call,val)->args))[1] == _oid_(Kernel._set)))) && 
        (firstc_meta_reader(r) == 123))
     { OID  s = GC_OID(readset_meta_reader(r,GC_OID(nexte_meta_reader(cnext_meta_reader(r)))));
      ClaireType * x = GC_OBJECT(ClaireType,extract_of_type_Call(OBJECT(Call,val)));
      if (INHERIT(OWNER(s),Language._Image))
       { (OBJECT(Image,s)->of = x);
        Result = s;
        } 
      else if (INHERIT(OWNER(s),Language._Select))
       { (OBJECT(Select,s)->of = x);
        Result = s;
        } 
      else Serror_string("[157] ~S cannot follow list{",list::alloc(1,s));
        } 
    else if (firstc_meta_reader(r) == 58)
     Result = nextvariable_meta_reader(r,val);
    else if (firstc_meta_reader(r) == 64)
     { OID  _Za1 = read_ident_port(cnext_meta_reader(r)->fromp);
      if (inherit_ask_class(OWNER(_Za1),Kernel._class) != CTRUE)
       Serror_string("[158] wrong type in call ~S@~S",list::alloc(2,val,_Za1));
      if (firstc_meta_reader(r) == 40)
       Result = readcall_meta_reader(r,val,_Za1);
      else Serror_string("[159] missing ( after ~S@~S",list::alloc(2,val,_Za1));
        } 
    else Result = val;
      GC_UNBIND; return (Result);} 
  } 


// we have read the escape character #
//
/* The c++ function for: read_escape(r:meta_reader) [0] */
OID  read_escape_meta_reader(meta_reader *r)
{ { OID Result = 0;
    if (firstc_meta_reader(cnext_meta_reader(r)) == 47)
     { int  val = firstc_meta_reader(cnext_meta_reader(r));
      next_meta_reader(r);
      Result = val;
      } 
    else if (firstc_meta_reader(r) == 39)
     Result = _oid_(make_function_string(string_I_symbol(extract_symbol_any(read_ident_port(cnext_meta_reader(r)->fromp)))));
    else if ((firstc_meta_reader(r) == 105) && 
        (firstc_meta_reader(cnext_meta_reader(r)) == 102))
     { next_meta_reader(r);
      Result = _oid_(Reader._cl_Zif);
      } 
    else Serror_string("[160] wrong use of special char #",Kernel.nil);
      return (Result);} 
  } 


/* The c++ function for: nextvariable(r:meta_reader,val:any) [NEW_ALLOC+SLOT_UPDATE] */
OID  nextvariable_meta_reader(meta_reader *r,OID val)
{ GC_BIND;
  { OID Result = 0;
    if (val == _oid_(Kernel._inf))
     { skipc_meta_reader(r);
      Result = Reader.triangle->value;
      } 
    else { Vardef * _CL_obj = ((Vardef *) GC_OBJECT(Vardef,new_object_class(Language._Vardef)));
        (_CL_obj->pname = extract_symbol_any(val));
        update_property(Kernel.range,
          _CL_obj,
          3,
          Kernel._object,
          GC_OID(nexte_meta_reader(cnext_meta_reader(r))));
        add_I_property(Kernel.instances,Language._Vardef,11,_oid_(_CL_obj));
        Result = _oid_(_CL_obj);
        } 
      GC_UNBIND; return (Result);} 
  } 


// reads an expression, then the exact keyword e
//
/* The c++ function for: nexts!(r:meta_reader,e:keyword) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
OID  nexts_I_meta_reader1(meta_reader *r,keyword *e)
{ GC_BIND;
  { OID Result = 0;
    { OID  x = GC_OID(nexts_meta_reader(r,e));
      if (boolean_I_any(stop_ask_integer(firstc_meta_reader(r))) != CTRUE)
       Result = x;
      else Serror_string("[161] Missing keyword ~S after ~S",list::alloc(2,_oid_(e),x));
        } 
    GC_UNBIND; return (Result);} 
  } 


// reads an expression, then the exact keyword e
//
/* The c++ function for: nexte!(r:meta_reader,e:keyword) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
OID  nexte_I_meta_reader(meta_reader *r,keyword *e)
{ GC_BIND;
  { OID Result = 0;
    { OID  x = GC_OID(nexte_meta_reader(r));
      if (nexte_meta_reader(r) == _oid_(e))
       Result = x;
      else Serror_string("[161] Missing keyword ~S after ~S",list::alloc(2,_oid_(e),x));
        } 
    GC_UNBIND; return (Result);} 
  } 


// ... exact separator
/* The c++ function for: nexts!(r:meta_reader,e:integer) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
OID  nexts_I_meta_reader2(meta_reader *r,int e)
{ GC_BIND;
  { OID Result = 0;
    { OID  x = GC_OID(nexts_meta_reader(r,Reader.none));
      if (firstc_meta_reader(r) == e)
       { cnext_meta_reader(r);
        Result = x;
        } 
      else Serror_string("[162] Missing separator ~S after ~S",list::alloc(2,_oid_(char_I_integer(e)),x));
        } 
    GC_UNBIND; return (Result);} 
  } 


// ... keyword e or separator n. DOES NOT SKIP the last character
//
/* The c++ function for: nexts!(r:meta_reader,e:keyword,n:integer) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
OID  nexts_I_meta_reader3(meta_reader *r,keyword *e,int n)
{ GC_BIND;
  { OID Result = 0;
    { OID  x = GC_OID(nexts_meta_reader(r,e));
      if ((firstc_meta_reader(r) == n) || 
          (boolean_I_any(stop_ask_integer(firstc_meta_reader(r))) != CTRUE))
       Result = x;
      else Serror_string("[163] wrong separator ~S after ~S",list::alloc(2,_oid_(char_I_integer(firstc_meta_reader(r))),x));
        } 
    GC_UNBIND; return (Result);} 
  } 


// checks if s is an extended comment
//
/* The c++ function for: extended_comment?(r:meta_reader,s:string) [0] */
ClaireBoolean * extended_comment_ask_meta_reader(meta_reader *r,char *s)
{ { ClaireBoolean *Result ;
    { int  n = get_string(s,_char_(']'));
      if ((_oid_(_char_(s[1 - 1])) == Language.EOS->value) || 
          ((s[1 - 1] != '[') || 
            (n == 0)))
       Result = CFALSE;
      else { OID  g0015UU;
          { int  i = 2;
            int  g0014 = n;
            { g0015UU= _oid_(CFALSE);
              while ((i <= g0014))
              { if (_I_equal_any(_oid_(_char_(s[n - 1])),_oid_(_char_('['))) != CTRUE)
                 { g0015UU = Kernel.ctrue;
                  break;} 
                ++i;
                } 
              } 
            } 
          Result = not_any(g0015UU);
          } 
        } 
    return (Result);} 
  } 


// produce the equivalent extended comment
//
/* The c++ function for: extended_comment!(r:meta_reader,s:string) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
OID  extended_comment_I_meta_reader(meta_reader *r,char *s)
{ GC_BIND;
  { OID Result = 0;
    { int  i = get_string(s,_char_(']'));
      int  k = included_string(s,"//",CTRUE);
      int  m = strlen(s);
      int  cx = firstc_meta_reader(r);
      print_in_string_void();
      { OID gc_local;
        while ((useless_c_integer(integer_I_char(_char_(s[m - 1]))) == CTRUE))
        { m= (m-1);
          } 
        } 
      if (s[m - 1] == ',')
       { cx= 44;
        m= (m-1);
        } 
      if (k == 0)
       k= (m+1);
      if ((i == 3) && 
          (s[i - 1] == '\?'))
       { princ_string("assert(");
        { int  j = (i+2);
          int  g0017 = m;
          { OID gc_local;
            while ((j <= g0017))
            { princ_char(_char_(s[j - 1]));
              ++j;
              } 
            } 
          } 
        princ_string(")");
        } 
      else { princ_string("trace(");
          { int  j = 2;
            int  g0018 = (i-1);
            { OID gc_local;
              while ((j <= g0018))
              { princ_char(_char_(s[j - 1]));
                ++j;
                } 
              } 
            } 
          princ_string(",\"");
          { int  j = (i+2);
            int  g0019 = (k-1);
            { OID gc_local;
              while ((j <= g0019))
              { princ_char(_char_(s[j - 1]));
                ++j;
                } 
              } 
            } 
          princ_string("\\n\"");
          if ((k+3) <= m)
           { princ_string(",");
            { int  j = (k+3);
              int  g0020 = m;
              { OID gc_local;
                while ((j <= g0020))
                { princ_char(_char_(s[j - 1]));
                  ++j;
                  } 
                } 
              } 
            } 
          princ_string(")");
          } 
        { OID  s2 = GC_OID(read_string(end_of_print_void()));
        (Reader.reader->nb_line = (Reader.reader->nb_line+1));
        pushback_port(Reader.reader->fromp,cx);
        Result = s2;
        } 
      } 
    GC_UNBIND; return (Result);} 
  } 

