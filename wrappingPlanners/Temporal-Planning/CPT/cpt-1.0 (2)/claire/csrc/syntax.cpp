/***** CLAIRE Compilation of file c:\claire\v3.3\src\meta\syntax.cl 
         [version 3.3.4 / safety 5] Sat Oct 16 06:53:32 2004 *****/

#include <claire.h>
#include <Kernel.h>
#include <Core.h>
#include <Language.h>
#include <Reader.h>

//+-------------------------------------------------------------+
//| CLAIRE                                                      |
//| syntax.cl                                                   |
//| Copyright (C) 1994 - 2003 Yves Caseau. All Rights Reserved  |
//| cf. copyright info in file object.cl: about()               |
//+-------------------------------------------------------------+
// store the line number in debug mode
/* The c++ function for: DBregister(c:Call) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
Call * DBregister_Call(Call *c)
{ GC_BIND;
  if (0 <= ClEnv->debug_I)
   { (Language.LastCall->value= _oid_(c));
    put_table(Reader.DBline,_oid_(c),Reader.reader->nb_line);
    } 
  if ((c->selector == Kernel.store) && 
      (c->args->length == 1))
   { list * l = GC_OBJECT(list,c->args);
    if (INHERIT(OWNER((*(l))[1]),Core._global_variable))
     ((*(l))[1]=_string_(make_string_symbol(OBJECT(symbol,(*Kernel.name)((*(l))[1])))));
    } 
  { Call *Result ;
    Result = c;
    GC_UNBIND; return (Result);} 
  } 


/* The c++ function for: Call!(p:property,l:list) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
Call * Call_I_property(property *p,list *l)
{ GC_BIND;
  { Call *Result ;
    { Call * g0021UU;
      { Call * _CL_obj = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
        (_CL_obj->selector = p);
        (_CL_obj->args = l);
        add_I_property(Kernel.instances,Language._Call,11,_oid_(_CL_obj));
        g0021UU = _CL_obj;
        } 
      Result = DBregister_Call(g0021UU);
      } 
    GC_UNBIND; return (Result);} 
  } 


// who is an operation?
//
/* The c++ function for: operation?(y:any) [0] */
ClaireBoolean * operation_ask_any(OID y)
{ return (((y == _oid_(Reader.as)) ? CTRUE : 
  ((y == _oid_(Reader.L__equal)) ? CTRUE : 
  ((INHERIT(OWNER(y),Kernel._operation)) ? CTRUE : 
  ((equal(y,Reader.OR->value) == CTRUE) ? CTRUE : 
  ((y == _oid_(Kernel._Z)) ? CTRUE : 
  ((y == _oid_(Kernel.add)) ? CTRUE : 
  CFALSE)))))));} 


// produce an expression from an operation
//
/* The c++ function for: combine(x:any,y:any,z:any) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
OID  combine_any(OID x,OID y,OID z)
{ GC_BIND;
  { OID Result = 0;
    { OID  p = operation_I_any(x);
      if ((boolean_I_any(p) == CTRUE) && 
          (precedence_I_any(y) < precedence_I_any(p)))
       Result = combine_I_any(GC_OID(operand_I_any(x,1)),p,GC_OID(combine_any(GC_OID(operand_I_any(x,2)),y,z)));
      else Result = combine_I_any(x,y,z);
        } 
    GC_UNBIND; return (Result);} 
  } 


/* The c++ function for: combine!(x:any,y:any,z:any) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
OID  combine_I_any(OID x,OID y,OID z)
{ GC_BIND;
  { OID Result = 0;
    if (y == _oid_(Reader.as))
     { Cast * _CL_obj = ((Cast *) GC_OBJECT(Cast,new_object_class(Language._Cast)));
      (_CL_obj->arg = x);
      (_CL_obj->set_arg = extract_type_any(z));
      add_I_property(Kernel.instances,Language._Cast,11,_oid_(_CL_obj));
      Result = _oid_(_CL_obj);
      } 
    else if (y == _oid_(Reader.L__equal))
     { if (INHERIT(OWNER(x),Core._global_variable))
       { Gassign * _CL_obj = ((Gassign *) GC_OBJECT(Gassign,new_object_class(Language._Gassign)));
        (_CL_obj->var = OBJECT(global_variable,x));
        (_CL_obj->arg = z);
        add_I_property(Kernel.instances,Language._Gassign,11,_oid_(_CL_obj));
        Result = _oid_(_CL_obj);
        } 
      else if (INHERIT(OWNER(x),Language._Call))
       { if ((INHERIT(OWNER(z),Language._Call)) && (((OBJECT(Call,z)->selector == Kernel.add) || 
              (OBJECT(Call,z)->selector == Kernel._delete)) && 
            ((equal((*(OBJECT(Call,z)->args))[1],x) == CTRUE) && 
              ((OBJECT(Call,x)->args->length == 1) ? (multi_ask_any(_oid_(OBJECT(Call,x)->selector)) == CTRUE) : (((OBJECT(Call,x)->selector == Kernel.nth) && 
                  (OBJECT(Call,x)->args->length == 2)) && (multi_ask_any((*(OBJECT(Call,x)->args))[1]) == CTRUE))))))
         { Call * V_CL0023;{ list * g0024UU;
            { OID v_bag;
              GC_ANY(g0024UU= list::empty(Kernel.emptySet));
              { if (OBJECT(Call,x)->args->length == 1)
                 v_bag = _oid_(OBJECT(Call,x)->selector);
                else v_bag = (*(OBJECT(Call,x)->args))[1];
                  GC_OID(v_bag);} 
              ((list *) g0024UU)->addFast(v_bag);
              { if (OBJECT(Call,x)->args->length == 1)
                 v_bag = (*(OBJECT(Call,x)->args))[1];
                else v_bag = (*(OBJECT(Call,x)->args))[2];
                  GC_OID(v_bag);} 
              ((list *) g0024UU)->addFast(v_bag);
              ((list *) g0024UU)->addFast(GC_OID((*(OBJECT(bag,(*Core.args)(z))))[2]));} 
            V_CL0023 = Call_I_property(OBJECT(property,(*Kernel.selector)(z)),g0024UU);
            } 
          
          Result=_oid_(V_CL0023);} 
        else if (OBJECT(Call,x)->selector == Kernel.nth)
         Result = _oid_(Call_I_property(Kernel.nth_equal,GC_OBJECT(list,((list *) copy_bag(OBJECT(Call,x)->args))->addFast(z))));
        else if (OBJECT(Call,x)->args->length == 1)
         { property * p = make_a_property_any(_oid_(OBJECT(Call,x)->selector));
          OID  y = GC_OID((*(OBJECT(Call,x)->args))[1]);
          if ((p == Core.read) && 
              ((INHERIT(OWNER(y),Language._Call_plus)) && (OBJECT(Call,y)->selector->reified == CTRUE)))
           Result = _oid_(Call_I_property(Core.write,list::alloc(2,y,z)));
          else Result = _oid_(Call_I_property(Core.write,list::alloc(3,_oid_(p),
              y,
              z)));
            } 
        else Serror_string("[164] ~S cannot be assigned with :=",list::alloc(1,x));
          } 
      else if (INHERIT(OWNER(x),Language._Do))
       { list * l = GC_OBJECT(list,OBJECT(Do,x)->args);
        int  m = l->length;
        Variable * v;
        { { Variable * _CL_obj = ((Variable *) GC_OBJECT(Variable,new_object_class(Language._Variable)));
            (_CL_obj->pname = gensym_void());
            add_I_property(Kernel.instances,Language._Variable,11,_oid_(_CL_obj));
            v = _CL_obj;
            } 
          GC_OBJECT(Variable,v);} 
        Let_star * _CL_obj = ((Let_star *) GC_OBJECT(Let_star,new_object_class(Language._Let_star)));
        (_CL_obj->var = v);
        (_CL_obj->value = z);
        { Let * g0025 = _CL_obj; 
          OID  g0026;
          { Do * _CL_obj = ((Do *) GC_OBJECT(Do,new_object_class(Language._Do)));
            { Do * g0027 = _CL_obj; 
              list * g0028;
              { list * i_bag = list::empty(Kernel._any);
                { int  i = 1;
                  int  g0022 = m;
                  { OID gc_local;
                    while ((i <= g0022))
                    { GC_LOOP;
                      { OID  g0029UU;
                        { Assign * _CL_obj = ((Assign *) GC_OBJECT(Assign,new_object_class(Language._Assign)));
                          (_CL_obj->var = (*(l))[i]);
                          (_CL_obj->arg = _oid_(Call_I_property(Kernel.nth,GC_OBJECT(list,list::alloc(2,_oid_(v),i)))));
                          add_I_property(Kernel.instances,Language._Assign,11,_oid_(_CL_obj));
                          g0029UU = _oid_(_CL_obj);
                          } 
                        i_bag->addFast(g0029UU);
                        } 
                      ++i;
                      GC_UNLOOP;} 
                    } 
                  } 
                g0028 = GC_OBJECT(list,i_bag);
                } 
              (g0027->args = g0028);} 
            add_I_property(Kernel.instances,Language._Do,11,_oid_(_CL_obj));
            g0026 = _oid_(_CL_obj);
            } 
          (g0025->arg = g0026);} 
        add_I_property(Kernel.instances,Language._Let_star,11,_oid_(_CL_obj));
        Result = _oid_(_CL_obj);
        } 
      else { Assign * _CL_obj = ((Assign *) GC_OBJECT(Assign,new_object_class(Language._Assign)));
          (_CL_obj->var = x);
          (_CL_obj->arg = z);
          add_I_property(Kernel.instances,Language._Assign,11,_oid_(_CL_obj));
          Result = _oid_(_CL_obj);
          } 
        } 
    else if (equal(y,Reader.OR->value) == CTRUE)
     { if (INHERIT(OWNER(x),Language._Or))
       { GC_OBJECT(list,OBJECT(Or,x)->args)->addFast(z);
        Result = x;
        } 
      else { Or * _CL_obj = ((Or *) GC_OBJECT(Or,new_object_class(Language._Or)));
          (_CL_obj->args = list::alloc(2,x,z));
          add_I_property(Kernel.instances,Language._Or,11,_oid_(_CL_obj));
          Result = _oid_(_CL_obj);
          } 
        } 
    else if (equal(y,Reader.AND->value) == CTRUE)
     { if (INHERIT(OWNER(x),Language._And))
       { GC_OBJECT(list,OBJECT(And,x)->args)->addFast(z);
        Result = x;
        } 
      else { And * _CL_obj = ((And *) GC_OBJECT(And,new_object_class(Language._And)));
          (_CL_obj->args = list::alloc(2,x,z));
          add_I_property(Kernel.instances,Language._And,11,_oid_(_CL_obj));
          Result = _oid_(_CL_obj);
          } 
        } 
    else if (y == _oid_(Kernel._Z))
     Result = _oid_(Call_I_property(Kernel._Z,GC_OBJECT(list,list::alloc(2,x,z))));
    else { Call * V_CL0030;{ Call_star * g0031UU;
          { Call_star * _CL_obj = ((Call_star *) GC_OBJECT(Call_star,new_object_class(Language._Call_star)));
            update_property(Kernel.selector,
              _CL_obj,
              2,
              Kernel._object,
              y);
            (_CL_obj->args = list::alloc(2,x,z));
            add_I_property(Kernel.instances,Language._Call_star,11,_oid_(_CL_obj));
            g0031UU = _CL_obj;
            } 
          V_CL0030 = DBregister_Call(g0031UU);
          } 
        
        Result=_oid_(V_CL0030);} 
      GC_UNBIND; return (Result);} 
  } 


// allows to treats Calls, Assigns, Gassign in an homogeneous way
//
/* The c++ function for: operation!(x:any) [0] */
OID  operation_I_any(OID x)
{ { OID Result = 0;
    if (INHERIT(OWNER(x),Language._Or))
     Result = Reader.OR->value;
    else if (INHERIT(OWNER(x),Language._And))
     Result = Reader.AND->value;
    else if (INHERIT(OWNER(x),Language._Assign))
     Result = _oid_(Reader.L__equal);
    else if (INHERIT(OWNER(x),Language._Gassign))
     Result = _oid_(Reader.L__equal);
    else if (INHERIT(OWNER(x),Language._Call))
     { property * p = OBJECT(Call,x)->selector;
      if ((INHERIT(OBJECT(ClaireObject,x)->isa,Language._Call_star)) && 
          (operation_ask_any(_oid_(p)) == CTRUE))
       Result = _oid_(p);
      else if (p == Kernel.nth_equal)
       Result = _oid_(Reader.L__equal);
      else if (p == Core.write)
       Result = _oid_(Reader.L__equal);
      else Result = Kernel.cfalse;
        } 
    else Result = Kernel.cfalse;
      return (Result);} 
  } 


/* The c++ function for: operand!(x:any,n:integer) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
OID  operand_I_any(OID x,int n)
{ GC_BIND;
  { OID Result = 0;
    if (INHERIT(OWNER(x),Language._Or))
     { if (n == 1)
       { Or * _CL_obj = ((Or *) GC_OBJECT(Or,new_object_class(Language._Or)));
        (_CL_obj->args = rmlast_list(((list *) copy_bag(OBJECT(Or,x)->args))));
        add_I_property(Kernel.instances,Language._Or,11,_oid_(_CL_obj));
        Result = _oid_(_CL_obj);
        } 
      else Result = last_list(OBJECT(Or,x)->args);
        } 
    else if (INHERIT(OWNER(x),Language._And))
     { if (n == 1)
       { And * _CL_obj = ((And *) GC_OBJECT(And,new_object_class(Language._And)));
        (_CL_obj->args = rmlast_list(((list *) copy_bag(OBJECT(And,x)->args))));
        add_I_property(Kernel.instances,Language._And,11,_oid_(_CL_obj));
        Result = _oid_(_CL_obj);
        } 
      else Result = last_list(OBJECT(And,x)->args);
        } 
    else if (INHERIT(OWNER(x),Language._Assign))
     { if (n == 1)
       Result = OBJECT(Assign,x)->var;
      else Result = OBJECT(Assign,x)->arg;
        } 
    else if (INHERIT(OWNER(x),Language._Gassign))
     { if (n == 1)
       Result = _oid_(OBJECT(Gassign,x)->var);
      else Result = OBJECT(Gassign,x)->arg;
        } 
    else if (INHERIT(OWNER(x),Language._Call))
     { if (OBJECT(Call,x)->selector == Core.write)
       { if (n == 2)
         Result = (*(OBJECT(Call,x)->args))[3];
        else Result = _oid_(Call_I_property(OBJECT(property,(*(OBJECT(Call,x)->args))[1]),list::alloc(1,GC_OID((*(OBJECT(Call,x)->args))[2]))));
          } 
      else if (OBJECT(Call,x)->selector == Kernel.nth_equal)
       { if (n == 2)
         Result = last_list(OBJECT(Call,x)->args);
        else Result = _oid_(Call_I_property(Kernel.nth,rmlast_list(((list *) copy_bag(OBJECT(Call,x)->args)))));
          } 
      else Result = (*(OBJECT(Call,x)->args))[n];
        } 
    else Result = Kernel.cfalse;
      GC_UNBIND; return (Result);} 
  } 


// precedence
//
/* The c++ function for: precedence!(y:any) [RETURN_ARG] */
int  precedence_I_any(OID y)
{ { int Result = 0;
    Result = ((y == _oid_(Reader.as)) ?
      0 :
      ((y == _oid_(Reader.L__equal)) ?
        100 :
        ((equal(y,Reader.AND->value) == CTRUE) ?
          1000 :
          ((equal(y,Reader.OR->value) == CTRUE) ?
            1010 :
            OBJECT(operation,y)->precedence ) ) ) );
    return (Result);} 
  } 


// *********************************************************************
// *      Part 3: Syntactical Analysis                                 *
// *********************************************************************
/* The c++ function for: nextstruct(r:meta_reader,%first:keyword,e:keyword) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
OID  nextstruct_meta_reader(meta_reader *r,keyword *_Zfirst,keyword *e)
{ GC_BIND;
  { OID Result = 0;
    if (_Zfirst == Reader._cl_let)
     Result = readlet_meta_reader(r,e);
    else if (_Zfirst == Reader._cl_when)
     Result = readwhen_meta_reader(r,e);
    else if (_Zfirst == Reader._cl_case)
     Result = readcase_meta_reader(r,e);
    else if (_Zfirst == Reader._cl_for)
     { Variable * _Zvar = GC_OBJECT(Variable,extract_variable_any(GC_OID(nexts_I_meta_reader1(r,Reader.in))));
      OID  _Zset = GC_OID(nexte_meta_reader(r));
      list * _Zbind = GC_OBJECT(list,bind_I_meta_reader(r,_Zvar));
      For * x;
      { if (firstc_meta_reader(r) == 44)
         next_meta_reader(r);
        { For * _CL_obj = ((For *) GC_OBJECT(For,new_object_class(Language._For)));
          (_CL_obj->var = _Zvar);
          (_CL_obj->set_arg = _Zset);
          (_CL_obj->arg = nexts_meta_reader(r,e));
          add_I_property(Kernel.instances,Language._For,11,_oid_(_CL_obj));
          x = _CL_obj;
          } 
        } 
      unbind_I_meta_reader(r,_Zbind);
      Result = _oid_(x);
      } 
    else if (_Zfirst == Reader._cl_while)
     { While * _CL_obj = ((While *) GC_OBJECT(While,new_object_class(Language._While)));
      (_CL_obj->test = nexte_meta_reader(r));
      (_CL_obj->arg = nexts_meta_reader(r,e));
      (_CL_obj->other = CFALSE);
      add_I_property(Kernel.instances,Language._While,11,_oid_(_CL_obj));
      Result = _oid_(_CL_obj);
      } 
    else if (_Zfirst == Reader._cl_until)
     { While * _CL_obj = ((While *) GC_OBJECT(While,new_object_class(Language._While)));
      (_CL_obj->test = nexte_meta_reader(r));
      (_CL_obj->arg = nexts_meta_reader(r,e));
      (_CL_obj->other = CTRUE);
      add_I_property(Kernel.instances,Language._While,11,_oid_(_CL_obj));
      Result = _oid_(_CL_obj);
      } 
    else if (_Zfirst == Reader._cl_try)
     { OID  _Za = GC_OID(nexts_I_meta_reader1(r,Reader.CATCH));
      OID  _Zt = GC_OID(nexte_meta_reader(r));
      ClaireHandle * _CL_obj = ((ClaireHandle *) GC_OBJECT(ClaireHandle,new_object_class(Language._Handle)));
      (_CL_obj->test = _Zt);
      (_CL_obj->arg = _Za);
      (_CL_obj->other = nexts_meta_reader(r,e));
      add_I_property(Kernel.instances,Language._Handle,11,_oid_(_CL_obj));
      Result = _oid_(_CL_obj);
      } 
    else Result = _oid_(_Zfirst);
      GC_UNBIND; return (Result);} 
  } 


// reads a let expression
//
/* The c++ function for: readlet(r:meta_reader,e:keyword) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
OID  readlet_meta_reader(meta_reader *r,keyword *e)
{ GC_BIND;
  { OID Result = 0;
    { OID  _Zdef = GC_OID(nexts_I_meta_reader3(r,Reader.in,44));
      if (INHERIT(OWNER(_Zdef),Language._Assign))
       { Variable * v = GC_OBJECT(Variable,extract_variable_any(GC_OID(OBJECT(Assign,_Zdef)->var)));
        list * _Zbind = GC_OBJECT(list,bind_I_meta_reader(r,v));
        Let * x;
        { { Let * _CL_obj = ((Let *) GC_OBJECT(Let,new_object_class(Language._Let)));
            (_CL_obj->var = v);
            (_CL_obj->value = OBJECT(Assign,_Zdef)->arg);
            { Let * g0032 = _CL_obj; 
              OID  g0033;
              if (firstc_meta_reader(r) == 44)
               g0033 = readlet_meta_reader(cnext_meta_reader(r),e);
              else g0033 = nexts_meta_reader(r,e);
                (g0032->arg = g0033);} 
            add_I_property(Kernel.instances,Language._Let,11,_oid_(_CL_obj));
            x = _CL_obj;
            } 
          GC_OBJECT(Let,x);} 
        unbind_I_meta_reader(r,_Zbind);
        Result = _oid_(x);
        } 
      else if (INHERIT(OWNER(_Zdef),Language._Let_star))
       { (OBJECT(Let,_Zdef)->arg = readlet_star_meta_reader(r,GC_OBJECT(list,OBJECT(Do,OBJECT(Let,_Zdef)->arg)->args),1,e));
        Result = _Zdef;
        } 
      else if ((OBJECT(ClaireBoolean,_oid_(((INHERIT(OWNER(_Zdef),Language._Call)) ? (((*Kernel.selector)(_Zdef) == _oid_(Core.write)) ? CTRUE: CFALSE): CFALSE)))) == CTRUE)
       { Variable * v1;
        { { Variable * _CL_obj = ((Variable *) GC_OBJECT(Variable,new_object_class(Language._Variable)));
            (_CL_obj->pname = gensym_void());
            (_CL_obj->range = Kernel._any);
            add_I_property(Kernel.instances,Language._Variable,11,_oid_(_CL_obj));
            v1 = _CL_obj;
            } 
          GC_OBJECT(Variable,v1);} 
        Variable * v2;
        { { Variable * _CL_obj = ((Variable *) GC_OBJECT(Variable,new_object_class(Language._Variable)));
            (_CL_obj->pname = gensym_void());
            (_CL_obj->range = Kernel._any);
            add_I_property(Kernel.instances,Language._Variable,11,_oid_(_CL_obj));
            v2 = _CL_obj;
            } 
          GC_OBJECT(Variable,v2);} 
        list * _Za = GC_OBJECT(list,OBJECT(Call,_Zdef)->args);
        OID  _Ze = GC_OID(nexts_meta_reader(r,e));
        Let_plus * _CL_obj = ((Let_plus *) GC_OBJECT(Let_plus,new_object_class(Language._Let_plus)));
        (_CL_obj->var = v1);
        { Let * g0034 = _CL_obj; 
          OID  g0035;
          { Call_plus * _CL_obj = ((Call_plus *) GC_OBJECT(Call_plus,new_object_class(Language._Call_plus)));
            update_property(Kernel.selector,
              _CL_obj,
              2,
              Kernel._object,
              (*(_Za))[1]);
            (_CL_obj->args = list::alloc(1,(*(_Za))[2]));
            add_I_property(Kernel.instances,Language._Call_plus,11,_oid_(_CL_obj));
            g0035 = _oid_(_CL_obj);
            } 
          (g0034->value = g0035);} 
        { Let * g0036 = _CL_obj; 
          OID  g0037;
          { Do * _CL_obj = ((Do *) GC_OBJECT(Do,new_object_class(Language._Do)));
            { Do * g0038 = _CL_obj; 
              list * g0039;
              { OID v_bag;
                GC_ANY(g0039= list::empty(Kernel._any));
                ((list *) g0039)->addFast(_Zdef);
                { { Let * _CL_obj = ((Let *) GC_OBJECT(Let,new_object_class(Language._Let)));
                    (_CL_obj->var = v2);
                    (_CL_obj->value = _Ze);
                    { Let * g0040 = _CL_obj; 
                      OID  g0041;
                      { Do * _CL_obj = ((Do *) GC_OBJECT(Do,new_object_class(Language._Do)));
                        (_CL_obj->args = list::alloc(Kernel._any,2,GC_OID(_oid_(Call_I_property(Core.write,list::alloc(3,(*(_Za))[1],
                          (*(_Za))[2],
                          _oid_(v1))))),_oid_(v2)));
                        add_I_property(Kernel.instances,Language._Do,11,_oid_(_CL_obj));
                        g0041 = _oid_(_CL_obj);
                        } 
                      (g0040->arg = g0041);} 
                    add_I_property(Kernel.instances,Language._Let,11,_oid_(_CL_obj));
                    v_bag = _oid_(_CL_obj);
                    } 
                  GC_OID(v_bag);} 
                ((list *) g0039)->addFast(v_bag);} 
              (g0038->args = g0039);} 
            add_I_property(Kernel.instances,Language._Do,11,_oid_(_CL_obj));
            g0037 = _oid_(_CL_obj);
            } 
          (g0036->arg = g0037);} 
        add_I_property(Kernel.instances,Language._Let_plus,11,_oid_(_CL_obj));
        Result = _oid_(_CL_obj);
        } 
      else Serror_string("[165] ~S is illegal after a let",list::alloc(1,_Zdef));
        } 
    GC_UNBIND; return (Result);} 
  } 


// recursive construction of the tail of a Let*
//
/* The c++ function for: readlet*(r:meta_reader,l:list,n:integer,e:keyword) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
OID  readlet_star_meta_reader(meta_reader *r,list *l,int n,keyword *e)
{ GC_BIND;
  { OID Result = 0;
    if (n > l->length)
     Result = nexts_meta_reader(r,e);
    else { Variable * v = GC_OBJECT(Variable,extract_variable_any(GC_OID((*Language.var)((*(l))[n]))));
        list * _Zbind = GC_OBJECT(list,bind_I_meta_reader(r,v));
        Let * x;
        { { Let * _CL_obj = ((Let *) GC_OBJECT(Let,new_object_class(Language._Let)));
            (_CL_obj->var = v);
            (_CL_obj->value = (*Kernel.arg)((*(l))[n]));
            (_CL_obj->arg = readlet_star_meta_reader(r,l,(n+1),e));
            add_I_property(Kernel.instances,Language._Let,11,_oid_(_CL_obj));
            x = _CL_obj;
            } 
          GC_OBJECT(Let,x);} 
        unbind_I_meta_reader(r,_Zbind);
        Result = _oid_(x);
        } 
      GC_UNBIND; return (Result);} 
  } 


// reads a when expression
//
/* The c++ function for: readwhen(r:meta_reader,e:keyword) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
OID  readwhen_meta_reader(meta_reader *r,keyword *e)
{ GC_BIND;
  { OID Result = 0;
    { OID  _Zdef = GC_OID(nexts_I_meta_reader3(r,Reader.in,44));
      if (INHERIT(OWNER(_Zdef),Language._Assign))
       { Variable * v = GC_OBJECT(Variable,extract_variable_any(GC_OID(OBJECT(Assign,_Zdef)->var)));
        list * _Zbind = GC_OBJECT(list,bind_I_meta_reader(r,v));
        OID  _Za = GC_OID(nexts_meta_reader(r,Reader._cl_else));
        When * x;
        { { When * _CL_obj = ((When *) GC_OBJECT(When,new_object_class(Language._When)));
            (_CL_obj->var = v);
            (_CL_obj->value = OBJECT(Assign,_Zdef)->arg);
            (_CL_obj->arg = _Za);
            { When * g0042 = _CL_obj; 
              OID  g0043;
              if (boolean_I_any(stop_ask_integer(firstc_meta_reader(r))) == CTRUE)
               g0043 = CNULL;
              else g0043 = nexts_meta_reader(r,e);
                (g0042->other = g0043);} 
            add_I_property(Kernel.instances,Language._When,11,_oid_(_CL_obj));
            x = _CL_obj;
            } 
          GC_OBJECT(When,x);} 
        unbind_I_meta_reader(r,_Zbind);
        Result = _oid_(x);
        } 
      else Serror_string("[165] ~S is illegal after a when",list::alloc(1,_Zdef));
        } 
    GC_UNBIND; return (Result);} 
  } 


// read an if
//
/* The c++ function for: readif(r:meta_reader,e:integer) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
OID  readif_meta_reader(meta_reader *r,int e)
{ GC_BIND;
  { OID Result = 0;
    { OID  _Za1 = GC_OID(nexte_meta_reader(r));
      OID  _Za2 = GC_OID(nexts_meta_reader(r,Reader._cl_else));
      If * _CL_obj = ((If *) GC_OBJECT(If,new_object_class(Language._If)));
      (_CL_obj->test = _Za1);
      (_CL_obj->arg = _Za2);
      { If * g0044 = _CL_obj; 
        OID  g0045;
        if ((firstc_meta_reader(r) == 44) || 
            (firstc_meta_reader(r) == e))
         g0045 = Kernel.cfalse;
        else { OID  x = GC_OID(nexte_meta_reader(r));
            if (x == _oid_(Reader._cl_if))
             g0045 = readif_meta_reader(r,e);
            else if (keyword_ask_any(x) == CTRUE)
             g0045 = nextstruct_meta_reader(r,OBJECT(keyword,x),Reader.none);
            else g0045 = loopexp_meta_reader(r,x,Reader.none,CFALSE);
              } 
          (g0044->other = g0045);} 
      add_I_property(Kernel.instances,Language._If,11,_oid_(_CL_obj));
      Result = _oid_(_CL_obj);
      } 
    GC_UNBIND; return (Result);} 
  } 


// reads a member_of
//
/* The c++ function for: readcase(r:meta_reader,e:keyword) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
OID  readcase_meta_reader(meta_reader *r,keyword *e)
{ GC_RESERVE(1);  // HOHO v3.0.55 optim !
  { OID Result = 0;
    { OID  _Zv = GC_OID(nexte_meta_reader(r));
      if (skipc_I_meta_reader(r) != 40)
       Serror_string("[166] Missing ( after case ~S",list::alloc(1,_Zv));
      { Case * _Zx;
        { { Case * _CL_obj = ((Case *) GC_OBJECT(Case,new_object_class(Language._Case)));
            (_CL_obj->var = _Zv);
            (_CL_obj->args = list::empty());
            add_I_property(Kernel.instances,Language._Case,11,_oid_(_CL_obj));
            _Zx = _CL_obj;
            } 
          GC_OBJECT(Case,_Zx);} 
        OID  _Zt = _oid_(Kernel._any);
        { OID gc_local;
          while ((firstc_meta_reader(r) != 41))
          { GC_LOOP;
            next_meta_reader(r);
            GC__OID(_Zt = _oid_(extract_type_any(GC_OID(nexte_meta_reader(r)))), 6);
            (_Zx->args = GC_OBJECT(list,GC_OBJECT(list,_Zx->args)->addFast(_Zt))->addFast(GC_OID(nexts_meta_reader(r,Reader.none))));
            if ((boolean_I_any(stop_ask_integer(firstc_meta_reader(r))) != CTRUE) && 
                (boolean_I_any(stop_ask_integer(skipc_meta_reader(r))) != CTRUE))
             Serror_string("[167] missing ) or , after ~S",list::alloc(1,_oid_(_Zx)));
            GC_UNLOOP;} 
          } 
        next_meta_reader(r);
        if ((e != Reader.none) && 
            ((boolean_I_any(stop_ask_integer(skipc_meta_reader(r))) != CTRUE) && 
              (nexte_meta_reader(r) != _oid_(e))))
         Serror_string("[161] missing ~S after ~S",list::alloc(2,_oid_(e),_oid_(_Zx)));
        Result = _oid_(_Zx);
        } 
      } 
    GC_UNBIND; return (Result);} 
  } 


// if the expression begins with "{"
//
/* The c++ function for: readset(r:meta_reader,%a1:any) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
OID  readset_meta_reader(meta_reader *r,OID _Za1)
{ GC_BIND;
  { OID Result = 0;
    if (equal(_Za1,r->curly) == CTRUE)
     { next_meta_reader(r);
      Result = _oid_(Kernel.emptySet);
      } 
    else { if (keyword_ask_any(_Za1) == CTRUE)
         _Za1= GC_OID(nextstruct_meta_reader(r,OBJECT(keyword,_Za1),Reader.none));
        { OID  _Za2 = GC_OID(nexte_meta_reader(r));
          if (equal(_Za2,r->comma) == CTRUE)
           { bag * V_CL0046;{ set * g0047UU;
              { set * u_bag = set::empty(Kernel.emptySet);
                { OID gc_local;
                  ITERATE(u);
                  bag *u_support;
                  u_support = GC_OBJECT(list,cons_any(_Za1,GC_OBJECT(list,OBJECT(list,nextseq_meta_reader(cnext_meta_reader(r),125)))));
                  for (START(u_support); NEXT(u);)
                  { GC_LOOP;
                    u_bag->addFast(GC_OID(dereference_any(u)));
                    GC_UNLOOP;} 
                  } 
                g0047UU = GC_OBJECT(set,u_bag);
                } 
              V_CL0046 = cast_I_bag(g0047UU,Kernel.emptySet);
              } 
            
            Result=_oid_(V_CL0046);} 
          else if (equal(_Za2,r->curly) == CTRUE)
           { next_meta_reader(r);
            Result = _oid_(cast_I_bag(set::alloc(1,GC_OID(dereference_any(_Za1))),Kernel.emptySet));
            } 
          else if (_Za2 == _oid_(Reader.in))
           { Variable * v = GC_OBJECT(Variable,extract_variable_any(_Za1));
            Select * _CL_obj = ((Select *) GC_OBJECT(Select,new_object_class(Language._Select)));
            (_CL_obj->var = v);
            (_CL_obj->set_arg = nexte_meta_reader(r));
            { Iteration * g0048 = _CL_obj; 
              OID  g0049;
              { list * _Zbind = GC_OBJECT(list,bind_I_meta_reader(r,v));
                OID  x;
                { if (equal(GC_OID(nexte_meta_reader(r)),GC_OID(Reader.OR->value)) != CTRUE)
                   Serror_string("[168] missing | in selection",Kernel.nil);
                  else x = nexts_I_meta_reader2(r,125);
                    GC_OID(x);} 
                unbind_I_meta_reader(r,_Zbind);
                g0049 = x;
                } 
              (g0048->arg = g0049);} 
            add_I_property(Kernel.instances,Language._Select,11,_oid_(_CL_obj));
            Result = _oid_(_CL_obj);
            } 
          else if (equal(_Za2,Reader.OR->value) == CTRUE)
           { Variable * v = GC_OBJECT(Variable,extract_variable_any(GC_OID(nexts_I_meta_reader1(r,Reader.in))));
            { OID  g0050UU;
              { Image * _CL_obj = ((Image *) GC_OBJECT(Image,new_object_class(Language._Image)));
                (_CL_obj->var = v);
                (_CL_obj->set_arg = nexts_I_meta_reader2(r,125));
                (_CL_obj->arg = substitution_any(_Za1,v,_oid_(v)));
                add_I_property(Kernel.instances,Language._Image,11,_oid_(_CL_obj));
                g0050UU = _oid_(_CL_obj);
                } 
              Result = lexical_build_any(g0050UU,GC_OBJECT(list,list::alloc(1,_oid_(v))),0);
              } 
            } 
          else if (operation_ask_any(_Za2) == CTRUE)
           Result = readset_meta_reader(r,GC_OID(loopexp_meta_reader(r,GC_OID(combine_any(_Za1,_Za2,GC_OID(nexte_meta_reader(r)))),Reader.none,CFALSE)));
          else Serror_string("[169] missing separation between ~S and ~S",list::alloc(2,_Za1,_Za2));
            } 
        } 
      GC_UNBIND; return (Result);} 
  } 


/* The c++ function for: dereference(x:any) [NEW_ALLOC] */
OID  dereference_any(OID x)
{ { OID Result = 0;
    if (INHERIT(OWNER(x),Kernel._unbound_symbol))
     { OID  V_CL0051;close_exception(((general_error *) (*Core._general_error)(_string_("[170] cannot use ~S in a set constant"),
        _oid_(list::alloc(1,x)))));
      
      Result=_void_(V_CL0051);} 
    else if (INHERIT(OWNER(x),Language._Variable))
     { OID  V_CL0052;close_exception(((general_error *) (*Core._general_error)(_string_("[170] cannot use a variable (~S) in a set constant"),
        _oid_(list::alloc(1,x)))));
      
      Result=_void_(V_CL0052);} 
    else Result = OPT_EVAL(x);
      return (Result);} 
  } 


// reads a sequence of exp. Must end with a e = ) | ] | }
//
/* The c++ function for: nextseq(r:meta_reader,e:integer) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
OID  nextseq_meta_reader(meta_reader *r,int e)
{ GC_BIND;
  { OID Result = 0;
    if (firstc_meta_reader(r) == e)
     { next_meta_reader(r);
      Result = _oid_(list::empty());
      } 
    else { OID  x = GC_OID(nexts_meta_reader(r,Reader.none));
        if ((firstc_meta_reader(r) == 10) && 
            (r->toplevel == CTRUE))
         skipc_meta_reader(r);
        if (firstc_meta_reader(r) == e)
         { next_meta_reader(r);
          Result = _oid_(list::alloc(1,x));
          } 
        else if (firstc_meta_reader(r) == 44)
         Result = _oid_(cons_any(x,GC_OBJECT(list,OBJECT(list,nextseq_meta_reader(cnext_meta_reader(r),e)))));
        else Serror_string("[171] Read the character ~S inside a sequence",list::alloc(1,_oid_(char_I_integer(firstc_meta_reader(r)))));
          } 
      GC_UNBIND; return (Result);} 
  } 


// read the next block: a sequence of exp. Must end with a e = ) | ] | }
//
/* The c++ function for: readblock(r:meta_reader,x:any,e:integer) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
OID  readblock_meta_reader(meta_reader *r,OID x,int e)
{ GC_BIND;
  skipc_meta_reader(r);
  { OID Result = 0;
    if (equal(x,r->paren) == CTRUE)
     Result = _oid_(list::empty());
    else if (firstc_meta_reader(r) == 44)
     Result = Do_I_any(x,GC_OID(readblock_meta_reader(r,GC_OID(nexte_meta_reader(cnext_meta_reader(r))),e)));
    else if (firstc_meta_reader(r) == e)
     { cnext_meta_reader(r);
      Result = x;
      } 
    else if (boolean_I_any(stop_ask_integer(firstc_meta_reader(r))) == CTRUE)
     Serror_string("[172] the sequence ...~S must end with ~A",list::alloc(2,x,_oid_(char_I_integer(e))));
    else if (x == _oid_(Reader._cl_if))
     Result = readblock_meta_reader(r,GC_OID(readif_meta_reader(r,e)),e);
    else if (x == _oid_(Reader._cl_Zif))
     { If * _Zi = GC_OBJECT(If,OBJECT(If,readif_meta_reader(r,e)));
      { OID  g0053UU;
        { if (boolean_I_any(eval_any(GC_OID(_Zi->test))) == CTRUE)
           g0053UU = _Zi->arg;
          else g0053UU = _Zi->other;
            GC_OID(g0053UU);} 
        Result = readblock_meta_reader(r,g0053UU,e);
        } 
      } 
    else if (x == _oid_(Reader._cl_else))
     Serror_string("[173] Expression starting with else",Kernel.nil);
    else if (keyword_ask_any(x) == CTRUE)
     Result = readblock_meta_reader(r,GC_OID(nextstruct_meta_reader(r,OBJECT(keyword,x),Reader.none)),e);
    else { OID  y = GC_OID(loopexp_meta_reader(r,x,Reader.none,CFALSE));
        if (INHERIT(OWNER(y),Language._Call_star))
         (OBJECT(ClaireObject,y)->isa = Language._Call);
        Result = readblock_meta_reader(r,y,e);
        } 
      GC_UNBIND; return (Result);} 
  } 


/* The c++ function for: Do!(x:any,y:any) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
OID  Do_I_any(OID x,OID y)
{ GC_BIND;
  { OID Result = 0;
    if (INHERIT(OWNER(y),Language._Do))
     { (OBJECT(Do,y)->args = add_at_list(GC_OBJECT(list,OBJECT(Do,y)->args),1,x));
      Result = y;
      } 
    else { Do * _CL_obj = ((Do *) GC_OBJECT(Do,new_object_class(Language._Do)));
        (_CL_obj->args = list::alloc(Kernel._any,2,x,y));
        add_I_property(Kernel.instances,Language._Do,11,_oid_(_CL_obj));
        Result = _oid_(_CL_obj);
        } 
      GC_UNBIND; return (Result);} 
  } 


// extract the type from a list<X> expression
/* The c++ function for: extract_of_type(x:Call) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
ClaireType * extract_of_type_Call(Call *x)
{ GC_BIND;
  { ClaireType *Result ;
    { list * l = GC_OBJECT(list,x->args);
      if (l->length > 2)
       { OID  y = (*(l))[3];
        if (INHERIT(OWNER(y),Language._List))
         { OID  z = GC_OID((*(OBJECT(Construct,y)->args))[1]);
          if (INHERIT(OWNER(z),Language._Set))
           Result = extract_type_any(GC_OID((*(OBJECT(Construct,z)->args))[1]));
          else Result = Kernel._any;
            } 
        else Result = Kernel._any;
          } 
      else Result = Kernel._any;
        } 
    GC_UNBIND; return (Result);} 
  } 


// if the expression is a call -------------------------------------------
//
/* The c++ function for: readcall(r:meta_reader,x:any,t:any) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
OID  readcall_meta_reader(meta_reader *r,OID x,OID t)
{ GC_BIND;
  { OID Result = 0;
    { OID  l = GC_OID(nextseq_meta_reader(cnext_meta_reader(r),41));
      if (x == _oid_(Reader.PRINTF))
       { Printf * _CL_obj = ((Printf *) GC_OBJECT(Printf,new_object_class(Language._Printf)));
        store_object(_CL_obj,
          2,
          Kernel._object,
          l,
          CFALSE);
        add_I_property(Kernel.instances,Language._Printf,11,_oid_(_CL_obj));
        Result = _oid_(_CL_obj);
        } 
      else if (x == _oid_(Kernel._error))
       { Error * _CL_obj = ((Error *) GC_OBJECT(Error,new_object_class(Language._Error)));
        store_object(_CL_obj,
          2,
          Kernel._object,
          l,
          CFALSE);
        add_I_property(Kernel.instances,Language._Error,11,_oid_(_CL_obj));
        Result = _oid_(_CL_obj);
        } 
      else if (x == _oid_(Reader.assert))
       { Assert * _CL_obj = ((Assert *) GC_OBJECT(Assert,new_object_class(Language._Assert)));
        store_object(_CL_obj,
          2,
          Kernel._object,
          l,
          CFALSE);
        (_CL_obj->index = r->nb_line);
        (_CL_obj->external = r->external);
        add_I_property(Kernel.instances,Language._Assert,11,_oid_(_CL_obj));
        Result = _oid_(_CL_obj);
        } 
      else if (x == _oid_(Reader.trace))
       { Trace * _CL_obj = ((Trace *) GC_OBJECT(Trace,new_object_class(Language._Trace)));
        store_object(_CL_obj,
          2,
          Kernel._object,
          l,
          CFALSE);
        add_I_property(Kernel.instances,Language._Trace,11,_oid_(_CL_obj));
        Result = _oid_(_CL_obj);
        } 
      else if (x == _oid_(Reader._cl_branch))
       { Branch * _CL_obj = ((Branch *) GC_OBJECT(Branch,new_object_class(Language._Branch)));
        store_object(_CL_obj,
          2,
          Kernel._object,
          l,
          CFALSE);
        add_I_property(Kernel.instances,Language._Branch,11,_oid_(_CL_obj));
        Result = _oid_(_CL_obj);
        } 
      else if (x == _oid_(Reader.quote))
       { Quote * _CL_obj = ((Quote *) GC_OBJECT(Quote,new_object_class(Language._Quote)));
        { Quote * g0055 = _CL_obj; 
          OID  g0056;
          if (boolean_I_any(l) == CTRUE)
           g0056 = (*Kernel.nth)(l,
            1);
          else g0056 = Kernel.cfalse;
            (g0055->arg = g0056);} 
        add_I_property(Kernel.instances,Language._Quote,11,_oid_(_CL_obj));
        Result = _oid_(_CL_obj);
        } 
      else if (x == _oid_(Kernel._tuple))
       { Tuple * _CL_obj = ((Tuple *) GC_OBJECT(Tuple,new_object_class(Language._Tuple)));
        store_object(_CL_obj,
          2,
          Kernel._object,
          l,
          CFALSE);
        add_I_property(Kernel.instances,Language._Tuple,11,_oid_(_CL_obj));
        Result = _oid_(_CL_obj);
        } 
      else if (x == _oid_(Kernel._list))
       { List * _CL_obj = ((List *) GC_OBJECT(List,new_object_class(Language._List)));
        store_object(_CL_obj,
          2,
          Kernel._object,
          l,
          CFALSE);
        add_I_property(Kernel.instances,Language._List,11,_oid_(_CL_obj));
        Result = _oid_(_CL_obj);
        } 
      else if ((INHERIT(OWNER(x),Language._Call)) && ((*(OBJECT(Call,x)->args))[1] == _oid_(Kernel._list)))
       { List * _CL_obj = ((List *) GC_OBJECT(List,new_object_class(Language._List)));
        (_CL_obj->of = extract_of_type_Call(OBJECT(Call,x)));
        store_object(_CL_obj,
          2,
          Kernel._object,
          l,
          CFALSE);
        add_I_property(Kernel.instances,Language._List,11,_oid_(_CL_obj));
        Result = _oid_(_CL_obj);
        } 
      else if ((INHERIT(OWNER(x),Language._Call)) && ((*(OBJECT(Call,x)->args))[1] == _oid_(Kernel._array)))
       { Array * _CL_obj = ((Array *) GC_OBJECT(Array,new_object_class(Language._Array)));
        (_CL_obj->of = extract_of_type_Call(OBJECT(Call,x)));
        store_object(_CL_obj,
          2,
          Kernel._object,
          l,
          CFALSE);
        add_I_property(Kernel.instances,Language._Array,11,_oid_(_CL_obj));
        Result = _oid_(_CL_obj);
        } 
      else if ((INHERIT(OWNER(x),Language._Call)) && ((*(OBJECT(Call,x)->args))[1] == _oid_(Kernel._set)))
       { Set * _CL_obj = ((Set *) GC_OBJECT(Set,new_object_class(Language._Set)));
        (_CL_obj->of = extract_of_type_Call(OBJECT(Call,x)));
        store_object(_CL_obj,
          2,
          Kernel._object,
          l,
          CFALSE);
        add_I_property(Kernel.instances,Language._Set,11,_oid_(_CL_obj));
        Result = _oid_(_CL_obj);
        } 
      else if ((INHERIT(OWNER(x),Kernel._class)) && 
          (INHERIT(OBJECT(ClaireClass,x),Language._Macro)))
       { ClaireObject * o = GC_OBJECT(ClaireObject,new_object_class(OBJECT(ClaireClass,x)));
        put_property2(Core.args,o,l);
        Result = _oid_(o);
        } 
      else if (x == _oid_(Kernel._set))
       { Set * _CL_obj = ((Set *) GC_OBJECT(Set,new_object_class(Language._Set)));
        store_object(_CL_obj,
          2,
          Kernel._object,
          l,
          CFALSE);
        add_I_property(Kernel.instances,Language._Set,11,_oid_(_CL_obj));
        Result = _oid_(_CL_obj);
        } 
      else if ((x == _oid_(Reader.RETURN)) || 
          (x == _oid_(Reader.BREAK)))
       { Return * _CL_obj = ((Return *) GC_OBJECT(Return,new_object_class(Language._Return)));
        { Return * g0057 = _CL_obj; 
          OID  g0058;
          if (boolean_I_any(l) == CTRUE)
           g0058 = (*Kernel.nth)(l,
            1);
          else g0058 = Kernel.ctrue;
            (g0057->arg = g0058);} 
        add_I_property(Kernel.instances,Language._Return,11,_oid_(_CL_obj));
        Result = _oid_(_CL_obj);
        } 
      else if (INHERIT(OWNER(x),Kernel._class))
       { { ClaireBoolean * g0059I;
          { OID  g0060UU;
            { ClaireBoolean * V_CL0061;{ OID  g0062UU;
                { OID gc_local;
                  ITERATE(y);
                  g0062UU= _oid_(CFALSE);
                  bag *y_support;
                  y_support = GC_OBJECT(bag,enumerate_any(l));
                  for (START(y_support); NEXT(y);)
                  { GC_LOOP;
                    { ClaireBoolean * g0063I;
                      { OID  g0064UU;
                        if (INHERIT(OWNER(y),Language._Call))
                         { if (OBJECT(Call,y)->selector == Kernel._equal)
                           { ((*(OBJECT(Call,y)->args))[1]=_oid_(make_a_property_any(GC_OID((*(OBJECT(Call,y)->args))[1]))));
                            g0064UU = Kernel.ctrue;
                            } 
                          else g0064UU = Kernel.cfalse;
                            } 
                        else g0064UU = Kernel.cfalse;
                          g0063I = not_any(g0064UU);
                        } 
                      
                      if (g0063I == CTRUE) { g0062UU = Kernel.ctrue;
                          break;} 
                        } 
                    GC_UNLOOP;} 
                  } 
                V_CL0061 = not_any(g0062UU);
                } 
              
              g0060UU=_oid_(V_CL0061);} 
            g0059I = not_any(g0060UU);
            } 
          
          if (g0059I == CTRUE) { list * l2 = OBJECT(ClaireClass,x)->params;
              int  n = ((INHERIT(l2->isa,Kernel._list)) ?
                l2->length :
                0 );
              if ((*Kernel.length)(l) == n)
               { list * V_CL0065;{ list * i_bag = list::empty(Kernel.emptySet);
                  { int  i = 1;
                    int  g0054 = n;
                    { OID gc_local;
                      while ((i <= g0054))
                      { GC_LOOP;
                        { OID  g0066UU;
                          { Call * _CL_obj = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                            (_CL_obj->selector = Kernel._equal);
                            (_CL_obj->args = list::alloc(2,(*(l2))[i],GC_OID((*Kernel.nth)(l,
                              i))));
                            add_I_property(Kernel.instances,Language._Call,11,_oid_(_CL_obj));
                            g0066UU = _oid_(_CL_obj);
                            } 
                          i_bag->addFast(g0066UU);
                          } 
                        ++i;
                        GC_UNLOOP;} 
                      } 
                    } 
                  V_CL0065 = GC_OBJECT(list,i_bag);
                  } 
                
                l=_oid_(V_CL0065);} 
              else Serror_string("[174] Wrong instantiation list ~S(~S...",list::alloc(2,x,GC_OID(_oid_(list::alloc(1,l)))));
                } 
            } 
        { Definition * _CL_obj = ((Definition *) GC_OBJECT(Definition,new_object_class(Language._Definition)));
          update_property(Kernel.arg,
            _CL_obj,
            2,
            Kernel._object,
            x);
          store_object(_CL_obj,
            3,
            Kernel._object,
            l,
            CFALSE);
          add_I_property(Kernel.instances,Language._Definition,11,_oid_(_CL_obj));
          Result = _oid_(_CL_obj);
          } 
        } 
      else { ClaireBoolean * g0067I;
        { ClaireBoolean *v_or;
          { v_or = inherit_ask_class(OWNER(x),Language._Variable);
            if (v_or == CTRUE) g0067I =CTRUE; 
            else { { OID  g0068UU;
                if (INHERIT(OWNER(x),Core._global_variable))
                 g0068UU = _oid_(OBJECT(global_variable,x)->range);
                else g0068UU = Kernel.cfalse;
                  v_or = boolean_I_any(g0068UU);
                } 
              if (v_or == CTRUE) g0067I =CTRUE; 
              else g0067I = CFALSE;} 
            } 
          } 
        
        if (g0067I == CTRUE) Result = _oid_(Call_I_property(Core.call,cons_any(x,GC_OBJECT(list,((boolean_I_any(l) == CTRUE) ?
            OBJECT(list,l) :
            list::alloc(1,_oid_(ClEnv)) )))));
          else { property * p = make_a_property_any(x);
          OID  l2;
          { if (boolean_I_any(l) == CTRUE)
             l2 = l;
            else l2 = _oid_(list::alloc(1,_oid_(ClEnv)));
              GC_OID(l2);} 
          if (t != CNULL)
           { Super * _CL_obj = ((Super *) GC_OBJECT(Super,new_object_class(Language._Super)));
            (_CL_obj->selector = p);
            update_property(Language.cast_to,
              _CL_obj,
              3,
              Kernel._object,
              t);
            store_object(_CL_obj,
              4,
              Kernel._object,
              l2,
              CFALSE);
            add_I_property(Kernel.instances,Language._Super,11,_oid_(_CL_obj));
            Result = _oid_(_CL_obj);
            } 
          else Result = _oid_(Call_I_property(p,OBJECT(list,l2)));
            } 
        } 
      } 
    GC_UNBIND; return (Result);} 
  } 


// reads a definition (CLAIRE2 syntax)   - x and y are two expressions that have been read
//
/* The c++ function for: nextdefinition(r:meta_reader,x:any,y:any,old?:boolean) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
OID  nextdefinition_meta_reader(meta_reader *r,OID x,OID y,ClaireBoolean *old_ask)
{ GC_BIND;
  (r->last_arrow = CFALSE);
  { OID Result = 0;
    if (equal(y,Reader.triangle->value) == CTRUE)
     Result = _oid_(nextDefclass_meta_reader(cnext_meta_reader(r),x,old_ask));
    else if (y == _oid_(Reader.L_))
     { ClaireBoolean * table_ask = ((INHERIT(OWNER(x),Language._Call)) ?
        ((OBJECT(Call,x)->selector == Kernel.nth) ? (((INHERIT(OWNER((*(OBJECT(Call,x)->args))[1]),Kernel._unbound_symbol)) || 
            (INHERIT(OWNER((*(OBJECT(Call,x)->args))[1]),Kernel._table))) ? CTRUE: CFALSE): CFALSE) :
        CFALSE );
      OID  z = GC_OID(nexte_meta_reader(r));
      OID  w = GC_OID(nexte_meta_reader(r));
      if ((table_ask == CTRUE) ? (w == _oid_(Reader.L__equal)) : ((equal(w,Reader.arrow->value) == CTRUE) || 
          (w == _oid_(Reader._equal_sup))))
       ;else Serror_string("[149] wrong keyword (~S) after ~S",list::alloc(2,w,z));
        Result = nextmethod_meta_reader(r,
        x,
        z,
        table_ask,
        old_ask,
        equal(w,_oid_(Reader._equal_sup)));
      } 
    else if (y == _oid_(Reader.L_L_))
     { if (INHERIT(OWNER(x),Language._Call))
       { OID  ru = GC_OID(nexte_meta_reader(r));
        OID  z = GC_OID(nexts_meta_reader(r,Reader._equal_sup));
        Defrule * _CL_obj = ((Defrule *) GC_OBJECT(Defrule,new_object_class(Language._Defrule)));
        (_CL_obj->ident = OBJECT(Call,x)->selector->name);
        (_CL_obj->args = OBJECT(Call,x)->args);
        (_CL_obj->arg = z);
        { Defrule * g0069 = _CL_obj; 
          OID  g0070;
          if (firstc_meta_reader(r) == 41)
           { next_meta_reader(r);
            g0070 = Core.nil->value;
            } 
          else g0070 = readblock_meta_reader(r,GC_OID(nexte_meta_reader(r)),41);
            (g0069->body = g0070);} 
        add_I_property(Kernel.instances,Language._Defrule,11,_oid_(_CL_obj));
        Result = _oid_(_CL_obj);
        } 
      else Result = nextinst_meta_reader(r,x);
        } 
    else if ((equal(y,Reader.arrow->value) == CTRUE) || 
        (y == _oid_(Reader._equal_sup)))
     { (r->last_arrow = equal(y,_oid_(Reader._equal_sup)));
      Result = nextmethod_meta_reader(r,
        x,
        _oid_(Kernel._void),
        CFALSE,
        old_ask,
        equal(y,_oid_(Reader._equal_sup)));
      } 
    else if ((y == _oid_(Reader.L__equal)) && 
        (INHERIT(OWNER(x),Language._Vardef)))
     { Defobj * _CL_obj = ((Defobj *) GC_OBJECT(Defobj,new_object_class(Language._Defobj)));
      (_CL_obj->ident = OBJECT(Variable,x)->pname);
      (_CL_obj->arg = Core._global_variable);
      (_CL_obj->args = list::alloc(2,GC_OID(_oid_(Call_I_property(Kernel._equal,list::alloc(2,_oid_(Kernel.range),GC_OID(_oid_(extract_type_any(GC_OID((*Kernel.range)(x))))))))),GC_OID(_oid_(Call_I_property(Kernel._equal,list::alloc(2,_oid_(Kernel.value),GC_OID(nexte_meta_reader(r))))))));
      add_I_property(Kernel.instances,Language._Defobj,11,_oid_(_CL_obj));
      Result = _oid_(_CL_obj);
      } 
    else { Do * _CL_obj = ((Do *) GC_OBJECT(Do,new_object_class(Language._Do)));
        (_CL_obj->args = list::alloc(Kernel._any,2,x,y));
        add_I_property(Kernel.instances,Language._Do,11,_oid_(_CL_obj));
        Result = _oid_(_CL_obj);
        } 
      GC_UNBIND; return (Result);} 
  } 


/* The c++ function for: nextmethod(r:meta_reader,x:any,y:any,table?:boolean,old?:boolean,inl?:boolean) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
OID  nextmethod_meta_reader(meta_reader *r,OID x,OID y,ClaireBoolean *table_ask,ClaireBoolean *old_ask,ClaireBoolean *inl_ask)
{ GC_BIND;
  { OID Result = 0;
    { OID  n = skipc_meta_reader(r);
      OID  z;
      { if (old_ask == CTRUE)
         z = readblock_meta_reader(r,GC_OID(nexte_meta_reader(r)),93);
        else if (n == 40)
         { if (r->toplevel == CTRUE)
           z = nexts_meta_reader(r,Reader.none);
          else z = readblock_meta_reader(r,GC_OID(nexte_meta_reader(cnext_meta_reader(r))),41);
            } 
        else z = nexte_meta_reader(r);
          GC_OID(z);} 
      Defmethod * rs;
      { { Defmethod * _CL_obj = ((Defmethod *) GC_OBJECT(Defmethod,new_object_class(Language._Defmethod)));
          update_property(Kernel.arg,
            _CL_obj,
            2,
            Kernel._object,
            x);
          (_CL_obj->set_arg = y);
          { Defmethod * g0071 = _CL_obj; 
            OID  g0072;
            if (z == _oid_(Reader._cl_let))
             g0072 = readlet_meta_reader(r,Reader.None);
            else g0072 = z;
              (g0071->body = g0072);} 
          (_CL_obj->inline_ask = _oid_(inl_ask));
          add_I_property(Kernel.instances,Language._Defmethod,11,_oid_(_CL_obj));
          rs = _CL_obj;
          } 
        GC_OBJECT(Defmethod,rs);} 
      if (table_ask == CTRUE)
       (rs->isa = Language._Defarray);
      Result = _oid_(rs);
      } 
    GC_UNBIND; return (Result);} 
  } 


// reads an instantiation
//
/* The c++ function for: nextinst(r:meta_reader,x:any) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
OID  nextinst_meta_reader(meta_reader *r,OID x)
{ GC_BIND;
  { OID Result = 0;
    if (INHERIT(OWNER(x),Language._Variable))
     { Defobj * _CL_obj = ((Defobj *) GC_OBJECT(Defobj,new_object_class(Language._Defobj)));
      (_CL_obj->ident = OBJECT(Variable,x)->pname);
      (_CL_obj->arg = Core._global_variable);
      { Definition * g0073 = _CL_obj; 
        list * g0074;
        { OID v_bag;
          GC_ANY(g0074= list::empty(Kernel.emptySet));
          { { Call * _CL_obj = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
              (_CL_obj->selector = Kernel._equal);
              (_CL_obj->args = list::alloc(2,_oid_(Kernel.range),GC_OID(_oid_(extract_type_any(GC_OID(_oid_(OBJECT(Variable,x)->range)))))));
              add_I_property(Kernel.instances,Language._Call,11,_oid_(_CL_obj));
              v_bag = _oid_(_CL_obj);
              } 
            GC_OID(v_bag);} 
          ((list *) g0074)->addFast(v_bag);
          { { Call * _CL_obj = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
              (_CL_obj->selector = Kernel._equal);
              (_CL_obj->args = list::alloc(2,_oid_(Kernel.value),GC_OID(nexte_meta_reader(r))));
              add_I_property(Kernel.instances,Language._Call,11,_oid_(_CL_obj));
              v_bag = _oid_(_CL_obj);
              } 
            GC_OID(v_bag);} 
          ((list *) g0074)->addFast(v_bag);} 
        (g0073->args = g0074);} 
      add_I_property(Kernel.instances,Language._Defobj,11,_oid_(_CL_obj));
      Result = _oid_(_CL_obj);
      } 
    else if (INHERIT(OWNER(x),Language._Call))
     { OID  ru = GC_OID(nexte_meta_reader(r));
      OID  z = GC_OID(nexts_meta_reader(r,Reader._equal_sup));
      Defrule * _CL_obj = ((Defrule *) GC_OBJECT(Defrule,new_object_class(Language._Defrule)));
      (_CL_obj->ident = OBJECT(Call,x)->selector->name);
      (_CL_obj->args = OBJECT(Call,x)->args);
      (_CL_obj->arg = z);
      { Defrule * g0075 = _CL_obj; 
        OID  g0076;
        if (firstc_meta_reader(r) == 41)
         { next_meta_reader(r);
          g0076 = Core.nil->value;
          } 
        else g0076 = readblock_meta_reader(r,GC_OID(nexte_meta_reader(r)),41);
          (g0075->body = g0076);} 
      add_I_property(Kernel.instances,Language._Defrule,11,_oid_(_CL_obj));
      Result = _oid_(_CL_obj);
      } 
    else { OID  y = GC_OID(nexte_meta_reader(r));
        if ((INHERIT(OWNER(x),Core._global_variable)) && (y == CNULL))
         Result = y;
        else if ((INHERIT(OWNER(y),Language._Definition)) && (INHERIT(OBJECT(Definition,y)->arg,Kernel._thing)))
         { Defobj * _CL_obj = ((Defobj *) GC_OBJECT(Defobj,new_object_class(Language._Defobj)));
          (_CL_obj->ident = extract_symbol_any(x));
          update_property(Kernel.arg,
            _CL_obj,
            2,
            Kernel._object,
            GC_OID((*Kernel.arg)(y)));
          store_object(_CL_obj,
            3,
            Kernel._object,
            (*Core.args)(y),
            CFALSE);
          add_I_property(Kernel.instances,Language._Defobj,11,_oid_(_CL_obj));
          Result = _oid_(_CL_obj);
          } 
        else { Defobj * _CL_obj = ((Defobj *) GC_OBJECT(Defobj,new_object_class(Language._Defobj)));
            (_CL_obj->ident = extract_symbol_any(x));
            (_CL_obj->arg = Core._global_variable);
            { Definition * g0077 = _CL_obj; 
              list * g0078;
              { OID v_bag;
                GC_ANY(g0078= list::empty(Kernel.emptySet));
                { { Call * _CL_obj = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                    (_CL_obj->selector = Kernel._equal);
                    (_CL_obj->args = list::alloc(2,_oid_(Kernel.range),_oid_(Kernel.emptySet)));
                    add_I_property(Kernel.instances,Language._Call,11,_oid_(_CL_obj));
                    v_bag = _oid_(_CL_obj);
                    } 
                  GC_OID(v_bag);} 
                ((list *) g0078)->addFast(v_bag);
                { { Call * _CL_obj = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                    (_CL_obj->selector = Kernel._equal);
                    (_CL_obj->args = list::alloc(2,_oid_(Kernel.value),y));
                    add_I_property(Kernel.instances,Language._Call,11,_oid_(_CL_obj));
                    v_bag = _oid_(_CL_obj);
                    } 
                  GC_OID(v_bag);} 
                ((list *) g0078)->addFast(v_bag);} 
              (g0077->args = g0078);} 
            add_I_property(Kernel.instances,Language._Defobj,11,_oid_(_CL_obj));
            Result = _oid_(_CL_obj);
            } 
          } 
      GC_UNBIND; return (Result);} 
  } 


// reads a class Definition of the form C(p:t | p:t = v *)
// new in v2.5
/* The c++ function for: nextDefclass(r:meta_reader,x:any,old?:boolean) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
Defclass * nextDefclass_meta_reader(meta_reader *r,OID x,ClaireBoolean *old_ask)
{ GC_BIND;
  skipc_meta_reader(r);
  { Defclass *Result ;
    { OID  c = GC_OID(verify_any(_oid_(Kernel._class),read_ident_port(r->fromp),_oid_(Language._Defclass)));
      Defclass * y;
      if (firstc_meta_reader(r) != 40)
       { Defclass * _CL_obj = ((Defclass *) GC_OBJECT(Defclass,new_object_class(Language._Defclass)));
        update_property(Kernel.arg,
          _CL_obj,
          2,
          Kernel._object,
          c);
        (_CL_obj->args = Kernel.nil);
        (_CL_obj->forward_ask = CTRUE);
        add_I_property(Kernel.instances,Language._Defclass,11,_oid_(_CL_obj));
        y = _CL_obj;
        } 
      else { OID  l = GC_OID(nextseq_meta_reader(cnext_meta_reader(r),41));
          { OID gc_local;
            ITERATE(y1);
            bag *y1_support;
            y1_support = GC_OBJECT(bag,enumerate_any(l));
            for (START(y1_support); NEXT(y1);)
            { ClaireBoolean * g0079I;
              { OID  g0080UU;
                if (INHERIT(OWNER(y1),Language._Call))
                 g0080UU = _oid_(((OBJECT(Call,y1)->selector == Kernel._equal) ? ((INHERIT(OWNER((*(OBJECT(Call,y1)->args))[1]),Language._Vardef)) ? CTRUE: CFALSE): CFALSE));
                else if (INHERIT(OWNER(y1),Language._Vardef))
                 g0080UU = Kernel.ctrue;
                else g0080UU = Kernel.cfalse;
                  g0079I = not_any(g0080UU);
                } 
              
              if (g0079I == CTRUE) Serror_string("[175] Wrong form ~S in ~S(~S)",list::alloc(3,y1,
                  c,
                  l));
                } 
            } 
          { Defclass * _CL_obj = ((Defclass *) GC_OBJECT(Defclass,new_object_class(Language._Defclass)));
            update_property(Kernel.arg,
              _CL_obj,
              2,
              Kernel._object,
              c);
            store_object(_CL_obj,
              3,
              Kernel._object,
              l,
              CFALSE);
            (_CL_obj->forward_ask = CFALSE);
            add_I_property(Kernel.instances,Language._Defclass,11,_oid_(_CL_obj));
            y = _CL_obj;
            } 
          } 
        list * lp = Kernel.nil;
      symbol * idt;
      if ((INHERIT(OWNER(x),Language._Call)) && (OBJECT(Call,x)->selector == Kernel.nth))
       { list * l = GC_OBJECT(list,OBJECT(Call,x)->args);
        if (INHERIT(OWNER((*(l))[1]),Kernel._class))
         lp= OBJECT(list,(*(l))[2]);
        else { { bag *v_list; OID v_val;
              OID y2,CLcount;
              v_list = cdr_list(l);
               lp = v_list->clone();
              for (CLcount= 1; CLcount <= v_list->length; CLcount++)
              { y2 = (*(v_list))[CLcount];
                v_val = _oid_(make_a_property_any(y2));
                
                (*((list *) lp))[CLcount] = v_val;} 
              } 
            GC_OBJECT(list,lp);} 
          idt = extract_symbol_any((*(l))[1]);
        } 
      else idt = extract_symbol_any(x);
        if ((old_ask == CTRUE) && 
          (skipc_meta_reader(r) != 93))
       Serror_string("[176] Missing ] after ~S ",list::alloc(1,_oid_(y)));
      else if (old_ask == CTRUE)
       next_meta_reader(r);
      (y->ident = idt);
      (y->params = lp);
      Result = y;
      } 
    GC_UNBIND; return (Result);} 
  } 


// end of file