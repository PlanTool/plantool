/***** CLAIRE Compilation of file c:\claire\v3.3\src\compile\copt.cl 
         [version 3.3.4 / safety 5] Sat Oct 16 06:53:39 2004 *****/

#include <claire.h>
#include <Kernel.h>
#include <Core.h>
#include <Language.h>
#include <Reader.h>
#include <Optimize.h>
#include <Generate.h>
void  inline_exp_c_producer1(Generate_c_producer *v7227,Call_method1 *v1140,OID v15308)
{ GC_BIND;
  { method * v7237 = v1140->arg;
    property * v7240 = v7237->selector;
    OID  v11032 = GC_OID(car_list(v1140->args));
    if ((v7240 == Kernel._dash) && 
        ((domain_I_restriction(v7237) == Kernel._integer) || 
            (domain_I_restriction(v7237) == Kernel._float)))
     { princ_string("(-");
      bexpression_any(v11032,v15308);
      princ_string(")");
      } 
    else if ((v7240 == Core.owner) && 
        (designated_ask_any(v11032) == CTRUE))
     { princ_string("OWNER(");
      (*Generate.expression)(v11032,
        v15308);
      princ_string(")");
      } 
    else if ((v7240 == Core.sqrt) && 
        (domain_I_restriction(v7237) == Kernel._float))
     { princ_string("sqrt(");
      (*Generate.expression)(v11032,
        v15308);
      princ_string(")");
      } 
    else if ((v7240 == Core.eval) && 
        (designated_ask_any(v11032) == CTRUE))
     { princ_string("OPT_EVAL(");
      (*Generate.expression)(v11032,
        v15308);
      princ_string(")");
      } 
    else if (v7240 == Core.externC)
     (*Kernel.princ)(v11032);
    else if ((_oid_(v7237) == Generate._starlength_bag_star->value) && 
        (designated_ask_any(v11032) == CTRUE))
     { (*Generate.expression)(v11032,
        v15308);
      princ_string("->length");
      } 
    else if ((v7240 == Kernel.integer_I) && 
        ((domain_I_restriction(v7237) == Kernel._char) && 
          (designated_ask_any(v11032) == CTRUE)))
     { (*Generate.expression)(v11032,
        v15308);
      princ_string("->ascii");
      } 
    else if (_oid_(v7237) == Generate._starlength_array_star->value)
     { (*Generate.expression)(v11032,
        v15308);
      princ_string("[0]");
      } 
    else if ((_oid_(v7237) == Generate._starnot_star->value) && 
        (_inf_equal_type(GC_OBJECT(ClaireType,OBJECT(ClaireType,(*Optimize.c_type)(v11032))),Kernel._boolean) == CTRUE))
     { princ_string("((");
      (*Generate.expression)(v11032,
        v15308);
      princ_string(" == Kernel.ctrue) ? CFALSE : CTRUE)");
      } 
    else print_external_call_c_producer(v7227,v1140,v15308);
      } 
  GC_UNBIND;} 

void  inline_exp_c_producer2(Generate_c_producer *v7227,Call_method2 *v1140,OID v15308)
{ GC_BIND;
  { method * v7237 = v1140->arg;
    property * v7240 = v7237->selector;
    OID  v11032 = GC_OID((*(v1140->args))[1]);
    OID  v11033 = GC_OID((*(v1140->args))[2]);
    if ((_oid_(v7237) == Generate._starmin_integer_star->value) && 
        ((designated_ask_any(v11032) == CTRUE) && 
          (designated_ask_any(v11033) == CTRUE)))
     { princ_string("((");
      (*Generate.expression)(v11032,
        v15308);
      princ_string(" <= ");
      (*Generate.expression)(v11033,
        v15308);
      princ_string(") ? ");
      (*Generate.expression)(v11032,
        v15308);
      princ_string(" : ");
      (*Generate.expression)(v11033,
        v15308);
      princ_string(")");
      } 
    else if ((_oid_(v7237) == Generate._starmax_integer_star->value) && 
        ((designated_ask_any(v11032) == CTRUE) && 
          (designated_ask_any(v11033) == CTRUE)))
     { princ_string("((");
      (*Generate.expression)(v11032,
        v15308);
      princ_string(" <= ");
      (*Generate.expression)(v11033,
        v15308);
      princ_string(") ? ");
      (*Generate.expression)(v11033,
        v15308);
      princ_string(" : ");
      (*Generate.expression)(v11032,
        v15308);
      princ_string(")");
      } 
    else if ((v7240 == Core.class_I) && 
        (INHERIT(OWNER(v11032),Kernel._symbol)))
     { princ_string("(");
      ident_thing(defined_symbol(OBJECT(symbol,v11032)));
      princ_string("._");
      c_princ_string(string_v((*Kernel.string_I)(v11032)));
      princ_string(" = ClaireClass::make(");
      print_any(GC_OID((*Kernel.string_I)(v11032)));
      princ_string(",");
      (*Generate.expression)(v11033,
        v15308);
      princ_string(",");
      ident_thing(OBJECT(thing,(*Kernel.module_I)(v11032)));
      princ_string(".it))");
      } 
    else if ((v7227->open_operators->memq(_oid_(v7240)) == CTRUE) && 
        (((v7240 != Kernel._7) || 
            (5 <= Optimize.compiler->safety)) && 
          ((equal((*(v7237->domain))[1],(*(v7237->domain))[2]) == CTRUE) && 
            ((_inf_equal_type(GC_OBJECT(ClaireType,OBJECT(ClaireType,(*Optimize.c_type)(v11032))),Kernel._integer) == CTRUE) || 
                (_inf_equal_type(GC_OBJECT(ClaireType,OBJECT(ClaireType,(*Optimize.c_type)(v11032))),Kernel._float) == CTRUE)))))
     { if ((Optimize.compiler->safety < 2) && 
          (_inf_equal_type(GC_OBJECT(ClaireType,OBJECT(ClaireType,(*Optimize.c_type)(v11032))),Kernel._integer) == CTRUE))
       princ_string("_integer_");
      princ_string("(");
      bexpression_any(v11032,v15308);
      princ_string(string_I_symbol(v7240->name));
      bexpression_any(v11033,v15308);
      princ_string(")");
      } 
    else if ((_oid_(v7237) == Generate._starcontain_star->value) && 
        (identifiable_ask_any(v11033) == CTRUE))
     { (*Generate.expression)(v11032,
        v15308);
      princ_string("->memq(");
      (*Generate.expression)(v11033,
        v15308);
      princ_string(")");
      } 
    else if (((_oid_(v7237) == Generate._starnth_list_star->value) && 
          (2 <= Optimize.compiler->safety)) || 
        (_oid_(v7237) == Generate._starnth_1_bag_star->value))
     { princ_string("(*(");
      (*Generate.expression)(v11032,
        v15308);
      princ_string("))[");
      (*Generate.expression)(v11033,
        v15308);
      princ_string("]");
      } 
    else if ((v7240 == Kernel.add_I) && 
        (_inf_equal_type(domain_I_restriction(v7237),Kernel._bag) == CTRUE))
     { (*Generate.expression)(v11032,
        v15308);
      princ_string("->addFast(");
      (*Generate.expression)(v11033,
        v15308);
      princ_string(")");
      } 
    else if ((_oid_(v7237) == Generate._starnth_string_star->value) && 
        (2 <= Optimize.compiler->safety))
     { princ_string("_char_(");
      (*Generate.expression)(v11032,
        v15308);
      princ_string("[");
      (*Generate.expression)(v11033,
        v15308);
      princ_string(" - 1])");
      } 
    else if (v7237->selector == Core.identical_ask)
     { princ_string("(");
      (*Optimize.bool_exp)(_oid_(v1140),
        Kernel.ctrue,
        v15308);
      princ_string(" ? CTRUE : CFALSE)");
      } 
    else if (v7237->selector == Core.externC)
     (*Kernel.princ)(v11032);
    else if ((v7240 == Core.inlineok_ask) && 
        (Kernel._string == OWNER(v11033)))
     { (*Generate.expression)(v11032,
        v15308);
      princ_string("->inlineDef(");
      print_any(v11033);
      princ_string(")");
      } 
    else print_external_call_c_producer(v7227,v1140,v15308);
      } 
  GC_UNBIND;} 

void  inline_exp_c_producer3(Generate_c_producer *v7227,Call_method *v1140,OID v15308)
{ GC_BIND;
  { method * v7237 = v1140->arg;
    ClaireBoolean * v7226 = Optimize.OPT->alloc_stack;
    OID  v11032 = GC_OID((*(v1140->args))[1]);
    OID  v11033 = GC_OID((*(v1140->args))[2]);
    OID  v11034 = GC_OID((*(v1140->args))[3]);
    (Optimize.OPT->alloc_stack = CFALSE);
    if ((_oid_(v7237) == Generate._starnth_equal_list_star->value) && 
        (3 <= Optimize.compiler->safety))
     { princ_string("((*(");
      (*Generate.expression)(v11032,
        v15308);
      princ_string("))[");
      (*Generate.expression)(v11033,
        v15308);
      princ_string("]=");
      (*Generate.expression)(v11034,
        v15308);
      princ_string(")");
      } 
    else if ((domain_I_restriction(v7237) == Kernel._string) && 
        (((v7237->selector == Kernel.nth_equal) && 
              (2 <= Optimize.compiler->safety)) || 
            (v7237->selector == Kernel.nth_put)))
     { princ_string("(");
      (*Generate.expression)(v11032,
        v15308);
      princ_string("[");
      (*Generate.expression)(v11033,
        v15308);
      princ_string(" - 1] = (char) ");
      (*Generate.expression)(v11034,
        v15308);
      princ_string("->ascii)");
      } 
    else if (_oid_(v7237) == Generate._starnth_1_string_star->value)
     { princ_string("_char_(");
      (*Generate.expression)(v11032,
        v15308);
      princ_string("[");
      (*Generate.expression)(v11033,
        v15308);
      princ_string(" - 1])");
      } 
    else if ((v7237->selector == Kernel.store) && 
        ((_inf_equal_type(GC_OBJECT(ClaireType,OBJECT(ClaireType,(*Optimize.c_type)(v11032))),Kernel._list) == CTRUE) && 
          (((v1140->args->length == 4) && 
                ((*(v1140->args))[4] == Kernel.ctrue)) || 
              (v1140->args->length == 3))))
     { princ_string("STOREI((*");
      (*Generate.expression)(v11032,
        v15308);
      princ_string(")[");
      (*Generate.expression)(v11033,
        v15308);
      princ_string("],");
      (*Generate.expression)(v11034,
        v15308);
      princ_string(")");
      } 
    else if ((v7237->selector == Kernel.store) && 
        ((_inf_equal_type(GC_OBJECT(ClaireType,OBJECT(ClaireType,(*Optimize.c_type)(v11032))),Kernel._array) == CTRUE) && 
          ((equal(_oid_(_exp_type(GC_OBJECT(ClaireType,member_type(GC_OBJECT(ClaireType,OBJECT(ClaireType,(*Optimize.c_type)(v11032))))),Kernel._float)),_oid_(Kernel.emptySet)) == CTRUE) && 
            (((v1140->args->length == 4) && 
                  ((*(v1140->args))[4] == Kernel.ctrue)) || 
                (v1140->args->length == 3)))))
     { princ_string("STOREI(");
      (*Generate.expression)(v11032,
        v15308);
      princ_string("[");
      (*Generate.expression)(v11033,
        v15308);
      princ_string("],");
      (*Generate.expression)(v11034,
        v15308);
      princ_string(")");
      } 
    else if ((v7237->selector == Kernel.add_slot) && 
        (INHERIT(owner_any(getC_any(v11032)),Kernel._class)))
     { princ_string("CL_ADD_SLOT(");
      (*Generate.expression)(v11032,
        v15308);
      princ_string(",");
      class_princ_class(OBJECT(ClaireClass,getC_any(v11032)));
      princ_string(",");
      (*Generate.expression)(v11033,
        v15308);
      princ_string(",");
      ident_symbol(OBJECT(symbol,(*Kernel.name)(GC_OID(getC_any(v11033)))));
      princ_string(",");
      (*Generate.expression)(GC_OID(getC_any(v11034)),
        v15308);
      princ_string(",");
      (*Generate.expression)(GC_OID((*(v1140->args))[4]),
        v15308);
      princ_string(")");
      } 
    else if (v7237->selector == Kernel.add_method)
     { if (INHERIT(OWNER(v11032),Kernel._property))
       { (*Generate.expression)(v11032,
          v15308);
        princ_string("->add");
        if (v1140->args->length > 5)
         princ_string("Float");
        princ_string("Method(");
        signature_I_c_producer(v7227,v11033);
        princ_string(",");
        (*Generate.expression)(v11034,
          v15308);
        princ_string(",");
        breakline_void();
        princ_string("\t");
        bitvector_I_c_producer(v7227,GC_OID((*(v1140->args))[4]));
        princ_string(",");
        (*Generate.expression)(GC_OID((*(v1140->args))[5]),
          v15308);
        if (v1140->args->length > 5)
         { princ_string(",");
          (*Generate.expression)(GC_OID((*(v1140->args))[6]),
            v15308);
          } 
        princ_string(")");
        } 
      else { princ_string("add_method_property(");
          args_list_bag(GC_OBJECT(list,v1140->args),v15308,CTRUE);
          princ_string(")");
          } 
        } 
    else { (Optimize.OPT->alloc_stack = v7226);
        print_external_call_c_producer(v7227,v1140,v15308);
        } 
      (Optimize.OPT->alloc_stack = v7226);
    } 
  GC_UNBIND;} 

void  print_external_call_c_producer(Generate_c_producer *v7227,Call_method *v1140,OID v15308)
{ GC_BIND;
  { method * v7237 = v1140->arg;
    list * v7236 = GC_OBJECT(list,v1140->args);
    int  v7239 = 1;
    list * v11424 = v7237->domain;
    c_princ_function(OBJECT(ClaireFunction,(*Optimize.functional_I)(_oid_(v7237))));
    princ_string("(");
    if (v7236->length > 4)
     (Optimize.OPT->level = (Optimize.OPT->level+1));
    if ((v7236->length == 1) && 
        (domain_I_restriction(v7237) == Kernel._void))
     v7236= Kernel.nil;
    { ITERATE(v7248);
      for (START(v7236); NEXT(v7248);)
      { if (v7239 != 1)
         { princ_string(",");
          if (v7236->length > 4)
           breakline_void();
          } 
        { ClaireClass * v11590 = psort_any((*(v11424))[v7239]);
          ClaireClass * v11592 = stupid_t_any1(v7248);
          if ((_inf_equalt_class(v11590,Kernel._object) == CTRUE) && 
              (_inf_equalt_class(v11592,v11590) != CTRUE))
           { princ_string("(");
            class_princ_class(v11590);
            princ_string(" *) ");
            } 
          } 
        (*Generate.expression)(v7248,
          v15308);
        ++v7239;
        } 
      } 
    princ_string(")");
    if ((INHERIT(v7237->range->isa,Kernel._tuple)) && 
        (Optimize.OPT->alloc_stack != CTRUE))
     princ_string("->copyIfNeeded()");
    if (v7236->length > 4)
     (Optimize.OPT->level = (Optimize.OPT->level-1));
    } 
  GC_UNBIND;} 

void  inline_exp_c_producer5(Generate_c_producer *v7227,Call *v1140,OID v15308)
{ GC_BIND;
  { property * v7240 = v1140->selector;
    OID  v11032 = GC_OID(car_list(v1140->args));
    int  v7239 = v1140->args->length;
    if (v7240 == Core.get_stack)
     { princ_string("ClEnv->stack[");
      (*Generate.expression)(v11032,
        v15308);
      princ_string("]");
      } 
    else if (v7240 == Optimize.safe)
     { int  v7249 = Optimize.compiler->safety;
      (Optimize.compiler->safety = 1);
      (*Generate.expression)((*(v1140->args))[1],
        v15308);
      (Optimize.compiler->safety = v7249);
      } 
    else if (v7240 == Core.base_I)
     princ_string("ClEnv->base");
    else if ((v7240 == Core.index_I) && 
        (v7239 == 1))
     princ_string("ClEnv->index");
    else if ((v7240 == Core.push_I) && 
        (v7239 == 1))
     { princ_string("PUSH(");
      (*Generate.expression)(v11032,
        v15308);
      princ_string(")");
      } 
    else if (v7240 == Core.put_stack)
     { princ_string("(ClEnv->stack[");
      (*Generate.expression)(v11032,
        v15308);
      princ_string("]=");
      (*Generate.expression)((*(v1140->args))[2],
        v15308);
      princ_string(")");
      } 
    else if (v7240 == Core.set_base)
     { princ_string("(ClEnv->base= ");
      (*Generate.expression)(v11032,
        v15308);
      princ_string(")");
      } 
    else if (v7240 == Core.set_index)
     { princ_string("(ClEnv->index= ");
      (*Generate.expression)(v11032,
        v15308);
      princ_string(")");
      } 
    else if (v7240 == Optimize.object_I)
     { OID  v11033 = (*(v1140->args))[2];
      princ_string("(");
      ident_thing(defined_symbol(OBJECT(symbol,v11032)));
      princ_string(".");
      if (_inf_equal_type(OBJECT(ClaireType,v11033),Reader._reserved_keyword) == CTRUE)
       { princ_string("_cl_");
        c_princ_string(string_v((*Kernel.string_I)(v11032)));
        } 
      else (*Language.ident)(v11032);
        princ_string(" = ");
      if ((v11033 == _oid_(Kernel._property)) && 
          (INHERIT(owner_any((*Kernel.value)(v11032)),Kernel._property)))
       { princ_string("property::make(");
        print_any(GC_OID((*Kernel.string_I)(v11032)));
        princ_string(",");
        princ_integer((*Kernel.open)(GC_OID((*Kernel.value)(v11032))));
        princ_string(",");
        expression_thing(OBJECT(thing,(*Kernel.module_I)(v11032)),Core.nil->value);
        princ_string(",");
        (*Generate.expression)(GC_OID((*Kernel.domain)(GC_OID((*Kernel.value)(v11032)))),
          Core.nil->value);
        princ_string(",");
        (*Kernel.princ)(GC_OID((*Kernel.dispatcher)(GC_OID((*Kernel.value)(v11032)))));
        princ_string(")");
        } 
      else { princ_string("(");
          class_princ_class(OBJECT(ClaireClass,v11033));
          princ_string(" *) ");
          (*Generate.expression)(v11033,
            v15308);
          princ_string("->instantiate(");
          print_any(GC_OID((*Kernel.string_I)(v11032)));
          princ_string(",");
          expression_thing(OBJECT(thing,(*Kernel.module_I)(v11032)),Core.nil->value);
          princ_string(")");
          } 
        princ_string(")");
      } 
    else if (v7240 == Optimize.anyObject_I)
     { princ_string("((");
      class_princ_class(OBJECT(ClaireClass,v11032));
      princ_string(" *) (*");
      (*Generate.expression)(v11032,
        v15308);
      princ_string(")(");
      args_list_bag(GC_OBJECT(list,cdr_list(GC_OBJECT(list,v1140->args))),v15308,_sup_integer(v1140->args->length,2));
      princ_string("))");
      } 
    else if ((OBJECT(ClaireBoolean,(*Generate.fcall_ask)(_oid_(v1140)))) == CTRUE)
     (*Generate.fcall_exp)(_oid_(v1140),
      v15308);
    else if (v1140->args->length > 20)
     { if (v1140->selector == Kernel.store)
       { list * v7236 = GC_OBJECT(list,v1140->args);
        int  v7239 = v7236->length;
        int  v7237 = (v7239/10);
        princ_string("(");
        { int  v7233 = 0;
          int  v6816 = v7237;
          { OID gc_local;
            while ((v7233 <= v6816))
            { GC_LOOP;
              princ_string("(*");
              expression_thing(Kernel.store,v15308);
              princ_string(")(");
              { list * v5015;
                { list * v5771 = list::empty(Kernel.emptySet);
                  { int  v7234 = ((v7233*10)+1);
                    int  v6817 = ((v7233 == v7237) ?
                      v7239 :
                      ((v7233*10)+10) );
                    { OID gc_local;
                      while ((v7234 <= v6817))
                      { v5771->addFast((*(v7236))[v7234]);
                        ++v7234;
                        } 
                      } 
                    } 
                  v5015 = GC_OBJECT(list,v5771);
                  } 
                args_list_bag(v5015,v15308,CTRUE);
                } 
              princ_string(")");
              if (v7233 != v7237)
               princ_string(",");
              ++v7233;
              GC_UNLOOP;} 
            } 
          } 
        princ_string(")");
        } 
      else close_exception(((general_error *) (*Core._general_error)(_string_("[216] ~S has more than 10 parameters"),
          _oid_(list::alloc(1,_oid_(v1140))))));
        } 
    else { princ_string("(*");
        expression_thing(v1140->selector,v15308);
        princ_string(")(");
        args_list_bag(GC_OBJECT(list,v1140->args),v15308,_sup_integer(v1140->args->length,1));
        princ_string(")");
        } 
      } 
  GC_UNBIND;} 

ClaireBoolean * fcall_ask_Call2_Generate(Call *v7248)
{ GC_RESERVE(1);  // HOHO v3.0.55 optim !
  { ClaireBoolean *Result ;
    { property * v7240 = v7248->selector;
      OID  v11032 = GC_OID((*(v7248->args))[1]);
      int  v6058;
      { list * v5976;
        { bag * v10195 = v7248->args;
          list * v10848 = ((list *) empty_bag(v10195));
          { OID gc_local;
            ITERATE(v7249);
            for (START(v10195); NEXT(v7249);)
            if ((OBJECT(ClaireBoolean,(*Optimize.c_gc_ask)(v7249))) == CTRUE)
             v10848->addFast(v7249);
            } 
          v5976 = GC_OBJECT(list,v10848);
          } 
        v6058 = v5976->length;
        } 
      { ClaireBoolean *v_and;
        { v_and = ((designated_ask_any(v11032) == CTRUE) ? CTRUE : ((v7240->dispatcher > 0) ? CTRUE : CFALSE));
          if (v_and == CFALSE) Result =CFALSE; 
          else { { list * v7236 = GC_OBJECT(list,cdr_list(GC_OBJECT(list,v7248->args)));
              ClaireType * v9399 = Kernel.emptySet;
              list * v11440;
              { { bag *v_list; OID v_val;
                  OID v7249,CLcount;
                  v_list = GC_OBJECT(list,v7248->args);
                   v11440 = v_list->clone();
                  for (CLcount= 1; CLcount <= v_list->length; CLcount++)
                  { v7249 = (*(v_list))[CLcount];
                    v_val = _oid_(ptype_type(OBJECT(ClaireType,(*Optimize.c_type)(v7249))));
                    
                    (*((list *) v11440))[CLcount] = v_val;} 
                  } 
                GC_OBJECT(list,v11440);} 
              list * v10626 = GC_OBJECT(list,cdr_list(v11440));
              list * v11438 = GC_OBJECT(list,get_restrictions_Call2(v7248,v11440));
              ClaireBoolean * v1934;
              { ClaireBoolean *v_and;
                { v_and = ((v11438->length > 0) ? CTRUE : CFALSE);
                  if (v_and == CFALSE) v1934 =CFALSE; 
                  else { { ClaireBoolean *v_or;
                      { v_or = ((v6058 == 0) ? CTRUE : CFALSE);
                        if (v_or == CTRUE) v_and =CTRUE; 
                        else { { OID  v6937;
                            { OID gc_local;
                              ITERATE(v7237);
                              v6937= _oid_(CFALSE);
                              for (START(v11438); NEXT(v7237);)
                              if (not_any(_oid_(nth_integer(status_I_restriction(OBJECT(restriction,v7237)),1))) != CTRUE)
                               { v6937 = Kernel.ctrue;
                                break;} 
                              } 
                            v_or = not_any(v6937);
                            } 
                          if (v_or == CTRUE) v_and =CTRUE; 
                          else v_and = CFALSE;} 
                        } 
                      } 
                    if (v_and == CFALSE) v1934 =CFALSE; 
                    else { v_and = (((v7240->dispatcher > 0) && 
                          (v7236->length <= 4)) ? CTRUE : ((v11438->length < 3) ? CTRUE : CFALSE));
                      if (v_and == CFALSE) v1934 =CFALSE; 
                      else v1934 = CTRUE;} 
                    } 
                  } 
                } 
              ClaireClass * v7243 = ((v11438->length > 0) ?
                c_srange_method(OBJECT(method,(*(v11438))[1])) :
                Kernel._void );
              ClaireBoolean * v778;
              { OID  v7898;
                { OID gc_local;
                  ITERATE(v7242);
                  v7898= _oid_(CFALSE);
                  for (START(v11438); NEXT(v7242);)
                  { GC_LOOP;
                    { ClaireBoolean * g0084I;
                      { OID  v9820;
                        { GC__ANY(v9399 = U_type(v9399,domain_I_restriction(OBJECT(restriction,v7242))), 1);
                          v9820 = _oid_(((INHERIT(OWNER((*(OBJECT(restriction,v7242)->domain))[1]),Kernel._class)) ? ((last_list(OBJECT(restriction,v7242)->domain) != _oid_(Kernel._listargs)) ? ((tmatch_ask_list(v10626,GC_OBJECT(list,cdr_list(OBJECT(restriction,v7242)->domain))) == CTRUE) ? ((c_srange_method(OBJECT(method,v7242)) == v7243) ? CTRUE: CFALSE): CFALSE): CFALSE): CFALSE));
                          } 
                        g0084I = not_any(v9820);
                        } 
                      
                      if (g0084I == CTRUE) { v7898 = Kernel.ctrue;
                          break;} 
                        } 
                    GC_UNLOOP;} 
                  } 
                v778 = not_any(v7898);
                } 
              ClaireBoolean * v12258 = (((v7243 == Kernel._integer) || 
                  ((v7243 == Kernel._object) || 
                    ((v7243 == Kernel._any) || 
                      (v7243 == Kernel._void)))) ? ((_inf_equal_type(v9399,Kernel._object) == CTRUE) ? ((_inf_equal_type(GC_OBJECT(ClaireType,ptype_type(OBJECT(ClaireType,(*Optimize.c_type)(v11032)))),v9399) == CTRUE) ? CTRUE: CFALSE): CFALSE): CFALSE);
              if (((OBJECT(ClaireBoolean,Generate.FCALLSTINKS->value)) == CTRUE) && 
                  ((((v1934 == CTRUE) ? ((v778 == CTRUE) ? ((v12258 == CTRUE) ? CTRUE: CFALSE): CFALSE): CFALSE) != CTRUE) && 
                    (v7240->dispatcher > 0)))
               tformat_string("****> fcall(~S) fails (selectorOK = ~S , lrOK = ~S, callOK = ~S)\n",0,GC_OBJECT(list,list::alloc(4,_oid_(v7248),
                _oid_(v1934),
                _oid_(v778),
                _oid_(v12258))));
              v_and = ((v1934 == CTRUE) ? ((v778 == CTRUE) ? ((v12258 == CTRUE) ? CTRUE: CFALSE): CFALSE): CFALSE);
              } 
            if (v_and == CFALSE) Result =CFALSE; 
            else Result = CTRUE;} 
          } 
        } 
      } 
    GC_UNBIND; return (Result);} 
  } 

list * get_restrictions_Call2(Call *v7248,list *v11440)
{ GC_RESERVE(1);  // HOHO v3.0.55 optim !
  { list *Result ;
    { property * v7240 = v7248->selector;
      ClaireBoolean * v460 = ((class_I_type(OBJECT(ClaireType,(*(v11440))[1]))->open == ClEnv->open) ? CTRUE : CFALSE);
      list * v11438 = list::empty(Kernel._method);
      if ((v7240->dispatcher == 0) && 
          ((v460 == CTRUE) || 
              (v7240->open == ClEnv->open)))
       ;else { OID gc_local;
          ITERATE(v7242);
          for (START(v7240->restrictions); NEXT(v7242);)
          { GC_LOOP;
            if (length_bag(_exp_list(OBJECT(restriction,v7242)->domain,v11440)) != 0)
             { if (Kernel._method == OBJECT(ClaireObject,v7242)->isa)
               GC__ANY(v11438 = v11438->addFast(v7242), 1);
              else { shrink_list(v11438,0);
                  break;} 
                } 
            GC_UNLOOP;} 
          } 
        Result = v11438;
      } 
    GC_UNBIND; return (Result);} 
  } 

void  fcall_exp_Call2_Generate(Call *v7248,OID v15308)
{ GC_RESERVE(1);  // HOHO v3.0.55 optim !
  { property * v7240 = v7248->selector;
    OID  v11032 = (*(v7248->args))[1];
    list * v11440;
    { { bag *v_list; OID v_val;
        OID v7249,CLcount;
        v_list = v7248->args;
         v11440 = v_list->clone();
        for (CLcount= 1; CLcount <= v_list->length; CLcount++)
        { v7249 = (*(v_list))[CLcount];
          v_val = _oid_(ptype_type(OBJECT(ClaireType,(*Optimize.c_type)(v7249))));
          
          (*((list *) v11440))[CLcount] = v_val;} 
        } 
      GC_OBJECT(list,v11440);} 
    list * v11438 = GC_OBJECT(list,get_restrictions_Call2(v7248,v11440));
    method * v7237 = OBJECT(method,(*(v11438))[1]);
    ClaireClass * v7243 = c_srange_method(v7237);
    if (v7243 == Kernel._void)
     princ_string("_void_(");
    else if (v7243 == Kernel._object)
     princ_string("_oid_(");
    else if ((v7243 == Kernel._integer) && 
        (Optimize.compiler->safety <= 2))
     princ_string("_integer_(");
    if ((v7240->dispatcher > 0) && 
        (v11440->length <= 4))
     { if (v7243 == Kernel._object)
       princ_string("(ClaireObject *) ");
      expression_thing(v7240,v15308);
      princ_string("->fcall(");
      c_sorted_args_Call(v7248,v7237->srange,v15308,CTRUE);
      princ_string(")");
      } 
    else { list * v10564;
        { { bag * v13172 = v7240->definition;
            list * v4832 = ((list *) empty_bag(v13172));
            { OID gc_local;
              ITERATE(v7248);
              for (START(v13172); NEXT(v7248);)
              if (v11438->memq(v7248) == CTRUE)
               v4832->addFast(v7248);
              } 
            v10564 = GC_OBJECT(list,v4832);
            } 
          GC_OBJECT(list,v10564);} 
        { OID gc_local;
          ITERATE(v7237);
          for (START(v10564); NEXT(v7237);)
          { GC_LOOP;
            { list * v3814;
              { { bag *v_list; OID v_val;
                  OID v7248,CLcount;
                  v_list = OBJECT(restriction,v7237)->domain;
                   v3814 = v_list->clone();
                  for (CLcount= 1; CLcount <= v_list->length; CLcount++)
                  { v7248 = (*(v_list))[CLcount];
                    v_val = _oid_(psort_any(v7248));
                    
                    (*((list *) v3814))[CLcount] = v_val;} 
                  } 
                GC_OBJECT(list,v3814);} 
              if (v7237 != last_list(v10564))
               { princ_string("(INHERIT(");
                if (INHERIT(OWNER(v11032),Optimize._to_CL))
                 { (*Generate.expression)(GC_OID((*Kernel.arg)(v11032)),
                    v15308);
                  princ_string("->isa");
                  } 
                else { princ_string("OWNER(");
                    (*Generate.expression)(v11032,
                      v15308);
                    princ_string(")");
                    } 
                  princ_string(",");
                expression_any(_oid_(domain_I_restriction(OBJECT(restriction,v7237))),v15308);
                princ_string(") ? ");
                if (v7243 == Kernel._object)
                 princ_string("(ClaireObject *)");
                princ_string(" ");
                c_princ_function(OBJECT(ClaireFunction,(*Optimize.functional_I)(v7237)));
                princ_string("((");
                class_princ_class(domain_I_restriction(OBJECT(restriction,v7237)));
                princ_string(" *) ");
                c_sorted_args_Call(v7248,v3814,v15308,CFALSE);
                princ_string(") : ");
                ;princ_string(" ");
                } 
              else { if (v7243 == Kernel._object)
                   princ_string("(ClaireObject *) ");
                  princ_string(" ");
                  c_princ_function(OBJECT(ClaireFunction,(*Optimize.functional_I)(v7237)));
                  princ_string("((");
                  (*Generate.interface_I)(Generate.PRODUCER->value,
                    _oid_(domain_I_restriction(OBJECT(restriction,v7237))));
                  princ_string(") ");
                  c_sorted_args_Call(v7248,v3814,v15308,CFALSE);
                  princ_string(")");
                  { int  v7233 = 1;
                    int  v6845 = (v10564->length-1);
                    { OID gc_local;
                      while ((v7233 <= v6845))
                      { princ_string(")");
                        ++v7233;
                        } 
                      } 
                    } 
                  } 
                } 
            GC_UNLOOP;} 
          } 
        } 
      if ((v7243 == Kernel._void) || 
        ((v7243 == Kernel._object) || 
          ((v7243 == Kernel._float) || 
            ((v7243 == Kernel._integer) && 
                (Optimize.compiler->safety <= 2)))))
     princ_string(")");
    } 
  GC_UNBIND;} 

void  c_sorted_arg_any(OID v7248,ClaireClass *v7243,OID v15308,ClaireBoolean *v4284)
{ GC_BIND;
  if (v4284 == CTRUE)
   princ_string("((int) ");
  if ((INHERIT(OWNER(v7248),Optimize._to_CL)) && (osort_any(_oid_(OBJECT(Compile_to_CL,v7248)->set_arg)) == v7243))
   (*Generate.expression)(GC_OID((*Kernel.arg)(v7248)),
    v15308);
  else if (v7243 == Kernel._any)
   (*Generate.expression)(v7248,
    v15308);
  else (*Generate.to_c)(Generate.PRODUCER->value,
      v7248,
      _oid_(v7243),
      v15308);
    if (v4284 == CTRUE)
   princ_string(")");
  GC_UNBIND;} 

void  c_sorted_args_Call(Call *v7248,list *v11439,OID v15308,ClaireBoolean *v4284)
{ GC_BIND;
  { int  v7233 = 0;
    { OID gc_local;
      ITERATE(v7249);
      for (START(v7248->args); NEXT(v7249);)
      { if (v7233 != 0)
         princ_string(",");
        ++v7233;
        c_sorted_arg_any(v7249,OBJECT(ClaireClass,(*(v11439))[v7233]),v15308,v4284);
        } 
      } 
    } 
  GC_UNBIND;} 

void  bitvector_I_c_producer(Generate_c_producer *v7227,OID v7248)
{ GC_BIND;
  if (INHERIT(OWNER(v7248),Kernel._integer))
   bitvectorSum_integer(v7248);
  else if (INHERIT(OWNER(v7248),Optimize._to_CL))
   bitvector_I_c_producer(v7227,GC_OID(OBJECT(Compile_to_CL,v7248)->arg));
  else if (INHERIT(OWNER(v7248),Optimize._to_protect))
   bitvector_I_c_producer(v7227,GC_OID(OBJECT(Compile_to_protect,v7248)->arg));
  else (*Generate.expression)(v7248,
      Core.nil->value);
    GC_UNBIND;} 

void  bitvectorSum_integer(int v7248)
{ if (v7248 == 0)
   princ_string("0");
  else { ClaireBoolean * v7226 = CFALSE;
      int  v7233 = 1;
      int  v6846 = 6;
      { while ((v7233 <= v6846))
        { if (BCONTAIN(v7248,v7233))
           { if (v7226 == CTRUE)
             princ_string("+");
            else v7226= CTRUE;
              princ_string(string_v((*(OBJECT(bag,Generate.bitvectorList->value)))[v7233]));
            } 
          ++v7233;
          } 
        } 
      } 
    } 

void  signature_I_c_producer(Generate_c_producer *v7227,OID v7248)
{ GC_BIND;
  if (INHERIT(OWNER(v7248),Kernel._list))
   { princ_string("list::domain(");
    princ_integer(OBJECT(bag,v7248)->length);
    princ_string(",");
    { list * v12703;
      { { bag *v_list; OID v_val;
          OID v7249,CLcount;
          v_list = OBJECT(bag,v7248);
           v12703 = v_list->clone();
          for (CLcount= 1; CLcount <= v_list->length; CLcount++)
          { v7249 = (*(v_list))[CLcount];
            v_val = getC_any(v7249);
            
            (*((list *) v12703))[CLcount] = v_val;} 
          } 
        GC_OBJECT(list,v12703);} 
      args_list_bag(v12703,Core.nil->value,_sup_integer(OBJECT(bag,v7248)->length,3));
      } 
    princ_string(")");
    } 
  else if (INHERIT(OWNER(v7248),Optimize._to_C))
   signature_I_c_producer(v7227,GC_OID(OBJECT(Compile_to_C,v7248)->arg));
  else if (INHERIT(OWNER(v7248),Optimize._to_protect))
   signature_I_c_producer(v7227,GC_OID(OBJECT(Compile_to_protect,v7248)->arg));
  else if (INHERIT(OWNER(v7248),Language._List))
   signature_I_c_producer(v7227,GC_OID(_oid_(OBJECT(Construct,v7248)->args)));
  else if (INHERIT(OWNER(v7248),Language._Variable))
   expression_Variable(OBJECT(Variable,v7248),Core.nil->value);
  else { princ_string("<fucking ");
      print_any(_oid_(OWNER(v7248)));
      princ_string(":");
      print_any(v7248);
      princ_string(">");
      } 
    GC_UNBIND;} 

OID  getC_any(OID v7248)
{ GC_BIND;
  { OID Result = 0;
    if (INHERIT(OWNER(v7248),Optimize._to_CL))
     Result = getC_any(GC_OID(OBJECT(Compile_to_CL,v7248)->arg));
    else if (INHERIT(OWNER(v7248),Optimize._to_protect))
     Result = getC_any(GC_OID(OBJECT(Compile_to_protect,v7248)->arg));
    else if (INHERIT(OWNER(v7248),Core._global_variable))
     { if (nativeVar_ask_global_variable(OBJECT(global_variable,v7248)) == CTRUE)
       Result = v7248;
      else { Compile_to_C * v2072 = ((Compile_to_C *) GC_OBJECT(Compile_to_C,new_object_class(Optimize._to_C)));
          (v2072->arg = v7248);
          (v2072->set_arg = Kernel._type);
          add_I_property(Kernel.instances,Optimize._to_C,11,_oid_(v2072));
          Result = _oid_(v2072);
          } 
        } 
    else Result = v7248;
      GC_UNBIND; return (Result);} 
  } 

void  gassign_c_producer(Generate_c_producer *v7227,Gassign *v1140,OID v15308)
{ GC_BIND;
  if (v1140->var->store_ask == CTRUE)
   { princ_string("(STOREI(");
    expression_global_variable(v1140->var,v15308);
    princ_string(",");
    (*Generate.expression)(v1140->arg,
      v15308);
    princ_string("))");
    } 
  else { princ_string("(");
      expression_global_variable(v1140->var,v15308);
      princ_string("= ");
      (*Generate.expression)(v1140->arg,
        v15308);
      princ_string(")");
      } 
    GC_UNBIND;} 

void  call_slot_c_producer(Generate_c_producer *v7227,Call_slot *v1140,OID v15308)
{ GC_BIND;
  if (v1140->test == CTRUE)
   { princ_string((((INHERIT(v1140->selector->srange,Kernel._cl_import)) || 
        ((INHERIT(v1140->selector->srange,Kernel._string)) || 
          ((INHERIT(v1140->selector->srange,Kernel._array)) || 
            (INHERIT(v1140->selector->srange,Kernel._object))))) ?
      "NOTNULL" :
      "KNOWN" ));
    princ_string("(");
    expression_thing(v1140->selector->selector,v15308);
    princ_string(",");
    } 
  c_member_c_producer(v7227,
    GC_OID(v1140->arg),
    psort_any(_oid_(domain_I_restriction(v1140->selector))),
    v1140->selector->selector,
    v15308);
  if (v1140->test == CTRUE)
   princ_string(")");
  GC_UNBIND;} 

void  call_table_c_producer(Generate_c_producer *v7227,Call_table *v1140,OID v15308)
{ GC_BIND;
  { table * v7225 = v1140->selector;
    OID  v7240 = v7225->params;
    OID  v7236 = v1140->arg;
    if (v1140->test == CTRUE)
     { princ_string("KNOWN(");
      expression_thing(v7225,v15308);
      princ_string(",");
      } 
    princ_string("(*(");
    expression_thing(v7225,v15308);
    princ_string("->graph))[");
    if (INHERIT(OWNER(v7240),Kernel._integer))
     { (*Generate.expression)(v7236,
        v15308);
      princ_string(" - ");
      princ_integer(v7240);
      princ_string("");
      } 
    else if (INHERIT(OWNER(v7240),Kernel._list))
     { (*Generate.expression)((*(OBJECT(bag,(*Core.args)(v7236))))[1],
        v15308);
      princ_string(" * ");
      (*Kernel.princ)((*(OBJECT(bag,v7240)))[1]);
      princ_string(" + ");
      (*Generate.expression)((*(OBJECT(bag,(*Core.args)(v7236))))[2],
        v15308);
      princ_string(" - ");
      (*Kernel.princ)((*(OBJECT(bag,v7240)))[2]);
      princ_string("");
      } 
    princ_string("]");
    if (v1140->test == CTRUE)
     princ_string(")");
    } 
  GC_UNBIND;} 

void  call_array_c_producer(Generate_c_producer *v7227,Call_array *v1140,OID v15308)
{ GC_BIND;
  if (v1140->test == _oid_(Kernel._float))
   { princ_string("((double *) ");
    (*Generate.expression)(v1140->selector,
      v15308);
    princ_string(")[");
    (*Generate.expression)(v1140->arg,
      v15308);
    princ_string("]");
    } 
  else { princ_string("((OID *) ");
      (*Generate.expression)(v1140->selector,
        v15308);
      princ_string(")[");
      (*Generate.expression)(v1140->arg,
        v15308);
      princ_string("]");
      } 
    GC_UNBIND;} 

void  update_c_producer(Generate_c_producer *v7227,Update *v1140,OID v15308)
{ GC_BIND;
  { OID  v7240 = v1140->selector;
    OID  v7225 = v1140->arg;
    OID  v7247 = v1140->value;
    OID  v7248 = v1140->var;
    ClaireClass * v7243 = OBJECT(ClaireClass,(*Optimize.c_sort)(v7247));
    if ((INHERIT(OWNER(v7240),Kernel._relation)) && ((OBJECT(ClaireRelation,v7240)->if_write != CNULL) && 
        ((v7225 != _oid_(Kernel.put)) && 
          (v7225 != _oid_(Core.put_store)))))
     { c_princ_string(string_I_symbol(OBJECT(symbol,(*Kernel.name)(v7240))));
      princ_string("_write(");
      (*Generate.expression)(GC_OID((*Kernel.arg)(v7248)),
        v15308);
      princ_string(",");
      (*Generate.expression)(v7247,
        v15308);
      princ_string(")");
      } 
    else if ((INHERIT(OWNER(v7240),Kernel._relation)) && ((OBJECT(ClaireRelation,v7240)->store_ask == CTRUE) || 
        (v7225 == _oid_(Core.put_store))))
     { princ_string("STORE");
      princ_string((((v7243 == Kernel._any) || 
          (v7243 == Kernel._integer)) ?
        "I" :
        ((v7243 == Kernel._float) ?
          "F" :
          "O" ) ));
      princ_string("(");
      (*Generate.expression)(v7248,
        v15308);
      princ_string(",");
      (*Generate.expression)(v7247,
        v15308);
      princ_string(")");
      } 
    else { princ_string("(");
        (*Generate.expression)(v7248,
          v15308);
        princ_string(" = ");
        (*Generate.expression)(v7247,
          v15308);
        princ_string(")");
        } 
      } 
  GC_UNBIND;} 

void  object_test_c_producer(Generate_c_producer *v7227,OID v11032,ClaireBoolean *v3475,OID v15308)
{ princ_string("(CTAG(");
  (*Generate.expression)(v11032,
    v15308);
  princ_string(") ");
  sign_equal_boolean(v3475);
  princ_string(" OBJ_CODE)");
  } 

void  exp_to_protect_c_producer(Generate_c_producer *v7227,Compile_to_protect *v1140,OID v15308)
{ GC_BIND;
  { OID  v11657 = GC_OID((*Optimize.c_type)(v1140->arg));
    if ((Optimize.OPT->protection == CTRUE) && 
        ((need_protect_any(v1140->arg) == CTRUE) && 
          (((Optimize.OPT->alloc_stack == CTRUE) ? ((_inf_equal_type(OBJECT(ClaireType,v11657),Kernel._tuple) == CTRUE) ? CTRUE: CFALSE): CFALSE) != CTRUE)))
     { OID  v7248 = v1140->arg;
      ClaireClass * v7243 = OBJECT(ClaireClass,(*Optimize.c_sort)(v7248));
      princ_string(gc_protect_class(v7243));
      princ_string("(");
      if (INHERIT(v7243,Kernel._object))
       { class_princ_class(psort_any(GC_OID((*Core.glb)(GC_OID((*Optimize.c_type)(v7248)),
          _oid_(v7243)))));
        princ_string(",");
        (*Generate.expression)(v1140->arg,
          v15308);
        princ_string("");
        } 
      else (*Generate.expression)(v1140->arg,
          v15308);
        princ_string(")");
      } 
    else (*Generate.expression)(v1140->arg,
        v15308);
      } 
  GC_UNBIND;} 

void  macro_c_producer(Generate_c_producer *v7227)
{ ;} 

void  init_var_c_producer(Generate_c_producer *v7227,ClaireClass *v7243)
{ ;} 

void  any_interface_c_producer(Generate_c_producer *v7227)
{ princ_string("OID");
  } 

void  pointer_cast_c_producer(Generate_c_producer *v7227,ClaireClass *v7243)
{ princ_string("(");
  class_princ_c_producer(v7227,v7243);
  princ_string(" *)");
  } 

void  exp_Assign_c_producer(Generate_c_producer *v7227,Assign *v1140,OID v15308)
{ GC_BIND;
  { OID  v7248 = v1140->arg;
    OID  v7247 = v1140->var;
    (*Language.ident)(Generate.PRODUCER->value,
      v7247);
    princ_string("= ");
    (*Generate.expression)(v7248,
      v15308);
    princ_string("");
    } 
  GC_UNBIND;} 

void  stat_handle_c_producer(Generate_c_producer *v7227,ClaireHandle *v1140,OID v7243,OID v15308)
{ GC_BIND;
  new_block_void();
  princ_string("ClaireHandler c_handle = ClaireHandler();");
  breakline_void();
  princ_string("if ERROR_IN ");
  breakline_void();
  new_block_void();
  statement_any(GC_OID(v1140->arg),v7243,v15308);
  princ_string("ClEnv->cHandle--;");
  close_block_void();
  princ_string("else if (belong_to(_oid_(ClEnv->exception_I),");
  (*Generate.expression)(GC_OID((*Optimize.c_code)(GC_OID(v1140->test),
      _oid_(Kernel._any))),
    _oid_(Kernel.emptySet));
  princ_string(") == CTRUE)");
  breakline_void();
  new_block_void();
  princ_string("c_handle.catchIt();");
  statement_any(GC_OID(v1140->other),v7243,v15308);
  close_block_void();
  princ_string("else PREVIOUS_HANDLER;");
  close_block_void();
  GC_UNBIND;} 

void  stat_construct_c_producer(Generate_c_producer *v7227,Construct *v1140,OID v7243,OID v15308)
{ GC_BIND;
  if (boolean_I_any(v7243) != CTRUE)
   close_exception(((general_error *) (*Core._general_error)(_string_("[202] A do should have been used for ~S"),
    _oid_(list::alloc(1,_oid_(v1140))))));
  { char * v7247 = GC_STRING(check_var_string("v_bag",v7243,v15308));
    bag * v921;
    { ClaireObject *V_CC ;
      if (INHERIT(v1140->isa,Language._List))
       V_CC = list::empty();
      else if (INHERIT(v1140->isa,Language._Set))
       V_CC = set::empty();
      else if (INHERIT(v1140->isa,Language._Tuple))
       V_CC = tuple::empty();
      else close_exception(((general_error *) (*Core._general_error)(_string_("BUG: ~S"),
          _oid_(list::alloc(1,_oid_(v1140))))));
        v921= (bag *) V_CC;} 
    new_block_void();
    if (get_property(Kernel.of,v1140) != CNULL)
     cast_I_bag(v921,OBJECT(ClaireType,(*Kernel.of)(_oid_(v1140))));
    { ClaireBoolean * g0090I;
      { OID  v2999;
        { OID gc_local;
          ITERATE(v7248);
          v2999= _oid_(CFALSE);
          bag *v7248_support;
          v7248_support = GC_OBJECT(list,v1140->args);
          for (START(v7248_support); NEXT(v7248);)
          if (c_func_any(v7248) != CTRUE)
           { v2999 = Kernel.ctrue;
            break;} 
          } 
        g0090I = boolean_I_any(v2999);
        } 
      
      if (g0090I == CTRUE) { princ_string("OID ");
          princ_string(v7247);
          princ_string(";");
          breakline_void();
          princ_string("");
          } 
        } 
    if (Optimize.OPT->protection == CTRUE)
     { (v7227->stat = (v7227->stat+1));
      princ_string("GC_ANY(");
      } 
    (*Kernel.c_princ)(v7243);
    princ_string("= ");
    (*Generate.bag_expression)(Generate.PRODUCER->value,
      _oid_(v921->isa),
      _oid_(v921),
      _oid_(of_bag(v921)),
      v15308);
    if (Optimize.OPT->protection == CTRUE)
     princ_string(")");
    princ_string(";");
    { OID gc_local;
      ITERATE(v7248);
      bag *v7248_support;
      v7248_support = GC_OBJECT(list,v1140->args);
      for (START(v7248_support); NEXT(v7248);)
      { ClaireBoolean * v7230 = c_func_any(v7248);
        breakline_void();
        if (v7230 != CTRUE)
         statement_any(v7248,_string_(v7247),v15308);
        princ_string("((");
        { OID  v3960;
          if (INHERIT(v1140->isa,Language._List))
           v3960 = _oid_(Kernel._list);
          else if (INHERIT(v1140->isa,Language._Set))
           v3960 = _oid_(Kernel._set);
          else v3960 = _oid_(Kernel._tuple);
            print_any(v3960);
          } 
        princ_string(" *) ");
        (*Kernel.c_princ)(v7243);
        princ_string(")");
        addFast_c_producer(v7227);
        princ_string("(");
        if (v7230 == CTRUE)
         (*Generate.expression)(v7248,
          v15308);
        else c_princ_string(v7247);
          princ_string(");");
        } 
      } 
    close_block_void();
    } 
  GC_UNBIND;} 

void  stat_while_c_producer(Generate_c_producer *v7227,While *v1140,OID v7243,OID v15308)
{ GC_BIND;
  new_block_void();
  { ClaireBoolean * v11201 = ((c_func_any(GC_OID(v1140->test)) == CTRUE) ? ((v1140->other != CTRUE) ? ((gc_usage_any(GC_OID(v1140->test),OBJECT(ClaireBoolean,v15308)) == Kernel.cfalse) ? CTRUE: CFALSE): CFALSE): CFALSE);
    char * v7247 = GC_STRING(check_var_string("v_while",v7243,v15308));
    if (Optimize.OPT->loop_gc == CTRUE)
     { princ_string("OID gc_local;");
      breakline_void();
      princ_string("");
      } 
    if (v11201 != CTRUE)
     { interface_I_class(Kernel._boolean);
      princ_string(v7247);
      princ_string(";");
      breakline_void();
      princ_string("");
      } 
    if (Kernel._string == OWNER(v7243))
     { (*Kernel.c_princ)(v7243);
      princ_string("= _oid_(");
      expression_boolean(CFALSE,v15308);
      princ_string(");");
      breakline_void();
      } 
    if (v11201 == CTRUE)
     { princ_string("while (");
      (*Optimize.bool_exp)(GC_OID(v1140->test),
        _oid_(not_any(_oid_(v1140->other))),
        v15308);
      princ_string(")");
      } 
    else { { OID  v4922;
          { if (v1140->other == CTRUE)
             v4922 = Kernel.cfalse;
            else v4922 = v1140->test;
              GC_OID(v4922);} 
          statement_any(v4922,_string_(v7247),Kernel.ctrue);
          } 
        breakline_void();
        princ_string("while (");
        princ_string(v7247);
        princ_string(" ");
        if (v1140->other == CTRUE)
         princ_string("!=");
        else princ_string("==");
          princ_string(" CTRUE)");
        } 
      breakline_void();
    new_block_void();
    { ClaireBoolean * v13790 = ((Optimize.OPT->loop_gc == CTRUE) ? ((gc_usage_any(GC_OID(v1140->arg),CTRUE) != Kernel.cfalse) ? CTRUE: CFALSE): CFALSE);
      if (v13790 == CTRUE)
       { princ_string("GC_LOOP;");
        breakline_void();
        } 
      { OID  v5882;
        if (Kernel._string == OWNER(v7243))
         v5882 = v7243;
        else v5882 = CNULL;
          inner_statement_any(GC_OID(v1140->arg),_oid_(Kernel.emptySet),v5882);
        } 
      if (v11201 != CTRUE)
       statement_any(GC_OID(v1140->test),_string_(v7247),Kernel.ctrue);
      if (v13790 == CTRUE)
       princ_string("GC_UNLOOP;");
      } 
    close_block_void();
    } 
  close_block_void();
  GC_UNBIND;} 

void  stat_gassign_c_producer(Generate_c_producer *v7227,Gassign *v1140,OID v7243,OID v15308)
{ GC_BIND;
  new_block_void();
  interface_I_c_producer(v7227,((nativeVar_ask_global_variable(v1140->var) == CTRUE) ?
    getRange_global_variable(v1140->var) :
    Kernel._any ));
  princ_string(" truc;");
  statement_any(GC_OID(v1140->arg),_string_("truc"),v15308);
  princ_string("");
  breakline_void();
  if (v1140->var->store_ask == CTRUE)
   { princ_string("STOREI(");
    expression_global_variable(v1140->var,v15308);
    princ_string(",truc);");
    } 
  else { princ_string("(");
      expression_global_variable(v1140->var,v15308);
      princ_string(" = truc);");
      } 
    close_block_void();
  GC_UNBIND;} 

void  stat_for_c_producer(Generate_c_producer *v7227,For *v1140,OID v7243,OID v15308)
{ GC_BIND;
  { char * v7247 = GC_STRING(c_string_c_producer1(v7227,GC_OBJECT(Variable,v1140->var)));
    new_block_void();
    if (Optimize.OPT->loop_gc == CTRUE)
     { princ_string("OID gc_local;");
      breakline_void();
      princ_string("");
      } 
    princ_string("ITERATE(");
    c_princ_string(v7247);
    princ_string(");");
    if (Kernel._string == OWNER(v7243))
     { breakline_void();
      (*Kernel.c_princ)(v7243);
      princ_string("= _oid_(CFALSE);");
      } 
    breakline_void();
    if ((c_func_any(GC_OID(v1140->set_arg)) == CTRUE) && 
        (designated_ask_any(GC_OID(v1140->set_arg)) == CTRUE))
     { princ_string("for (START(");
      (*Generate.expression)(GC_OID(v1140->set_arg),
        v15308);
      princ_string("); NEXT(");
      c_princ_string(v7247);
      princ_string(");)");
      } 
    else { char * v11684 = GC_STRING(append_string(v7247,"_support"));
        princ_string("bag *");
        c_princ_string(v11684);
        princ_string(";");
        breakline_void();
        princ_string("");
        statement_any(GC_OID(v1140->set_arg),_string_(v11684),v15308);
        princ_string("for (START(");
        c_princ_string(v11684);
        princ_string("); NEXT(");
        c_princ_string(v7247);
        princ_string(");)");
        } 
      breakline_void();
    { ClaireBoolean * v13790 = ((Optimize.OPT->loop_gc == CTRUE) ? ((gc_usage_any(GC_OID(v1140->arg),CTRUE) != Kernel.cfalse) ? CTRUE: CFALSE): CFALSE);
      if (v13790 == CTRUE)
       { new_block_void();
        princ_string("GC_LOOP;");
        breakline_void();
        } 
      if ((Optimize.OPT->profile_ask == CTRUE) && 
          (_Z_any1(Optimize.OPT->in_method,Kernel._object) == CTRUE))
       { if (Optimize.OPT->loop_gc != CTRUE)
         new_block_void();
        princ_string("PRloop(PR_x);");
        breakline_void();
        } 
      { OID  v6849;
        if (Kernel._string == OWNER(v7243))
         v6849 = v7243;
        else v6849 = CNULL;
          statement_any(GC_OID(v1140->arg),_oid_(Kernel.emptySet),v6849);
        } 
      if (v13790 == CTRUE)
       { princ_string("GC_UNLOOP;");
        close_block_void();
        } 
      if ((Optimize.OPT->profile_ask == CTRUE) && 
          ((_Z_any1(Optimize.OPT->in_method,Kernel._object) == CTRUE) && 
            (Optimize.OPT->loop_gc != CTRUE)))
       close_block_void();
      close_block_void();
      } 
    } 
  GC_UNBIND;} 

void  stat_iteration_c_producer(Generate_c_producer *v7227,Iteration *v1140,OID v7243,OID v15308)
{ GC_BIND;
  if (boolean_I_any(v7243) != CTRUE)
   close_exception(((general_error *) (*Core._general_error)(_string_("[203] you should have used a FOR here:~S"),
    _oid_(list::alloc(1,_oid_(v1140))))));
  { char * v7247 = GC_STRING(c_string_c_producer1(v7227,GC_OBJECT(Variable,v1140->var)));
    char * v3907 = GC_STRING(check_var_string("v_val",v7243,v15308));
    char * v4959 = GC_STRING(check_var_string("v_list",v7243,v15308));
    new_block_void();
    princ_string("bag *");
    princ_string(v4959);
    princ_string("; OID ");
    princ_string(v3907);
    princ_string(";");
    breakline_void();
    princ_string("");
    princ_string("OID ");
    c_princ_string(v7247);
    princ_string(",CLcount;");
    breakline_void();
    princ_string("");
    statement_any(GC_OID(v1140->set_arg),_string_(v4959),v15308);
    princ_string(" ");
    (*Kernel.c_princ)(v7243);
    princ_string(" = ");
    princ_string(v4959);
    princ_string("->clone(");
    if (get_property(Kernel.of,v1140) != CNULL)
     (*Generate.expression)(GC_OID((*Optimize.c_code)(GC_OID((*Kernel.of)(_oid_(v1140))),
        _oid_(Kernel._type))),
      Core.nil->value);
    princ_string(")");
    princ_string(";");
    breakline_void();
    princ_string("for (CLcount= 1; CLcount <= ");
    princ_string(v4959);
    princ_string("->length; CLcount++)");
    breakline_void();
    new_block_void();
    c_princ_string(v7247);
    princ_string(" = (*(");
    princ_string(v4959);
    princ_string("))[CLcount];");
    breakline_void();
    statement_any(GC_OID(v1140->arg),_string_(v3907),Kernel.ctrue);
    breakline_void();
    princ_string("(*((list *) ");
    (*Kernel.princ)(v7243);
    princ_string("))[CLcount] = ");
    princ_string(v3907);
    princ_string(";");
    close_block_void();
    close_block_void();
    } 
  GC_UNBIND;} 

void  stat_super_c_producer(Generate_c_producer *v7227,Super *v1140,OID v7243,OID v15308)
{ GC_BIND;
  { char * v15977 = GC_STRING(check_var_string("v_rec",v7243,v15308));
    new_block_void();
    princ_string("OID ");
    princ_string(v15977);
    princ_string(";");
    breakline_void();
    { OID gc_local;
      ITERATE(v7248);
      bag *v7248_support;
      v7248_support = GC_OBJECT(list,v1140->args);
      for (START(v7248_support); NEXT(v7248);)
      { statement_any(v7248,_string_(v15977),v15308);
        princ_string("PUSH(");
        princ_string(v15977);
        princ_string(");");
        breakline_void();
        } 
      } 
    if (Kernel._string == OWNER(v7243))
     { c_princ_string(string_v(v7243));
      princ_string("=");
      } 
    expression_thing(v1140->selector,v15308);
    princ_string("->super(");
    (*Generate.expression)(GC_OID(_oid_(v1140->cast_to)),
      v15308);
    princ_string(",");
    princ_integer(v1140->args->length);
    princ_string(");");
    close_block_void();
    } 
  GC_UNBIND;} 

void  stat_let_c_producer(Generate_c_producer *v7227,Let *v1140,OID v7243,OID v15308)
{ GC_RESERVE(8);  // v3.0.55 optim !
  { char * v11501 = GC_STRING(string_v((*Generate.c_string)(Generate.PRODUCER->value,
      _oid_(v1140->var->pname))));
    if ((v11501[1 - 1] == 'C') && 
        (v11501[2 - 1] == '%'))
     (v1140->var->pname = gensym_void());
    } 
  { OID  v7247 = GC_OID((*Generate.c_string)(Generate.PRODUCER->value,
      GC_OID(_oid_(v1140->var))));
    ClaireBoolean * v7226 = Optimize.OPT->alloc_stack;
    OID  v7248 = GC_OID(v1140->value);
    ClaireBoolean * v7230 = CTRUE;
    new_block_void();
    (Optimize.OPT->alloc_stack = CFALSE);
    { OID gc_local;
      while ((v7230 == CTRUE))
      { GC_LOOP;
        if (INHERIT(v1140->isa,Language._Let_star))
         (Optimize.OPT->alloc_stack = CTRUE);
        interface_I_class(sort_Variable(GC_OBJECT(Variable,v1140->var)));
        princ_string(" ");
        c_princ_string(string_v(v7247));
        princ_string("");
        if (c_func_any(v7248) == CTRUE)
         { princ_string(" = ");
          if (bool_exp_ask_any(v7248) == CTRUE)
           (*Generate.bool_exp_I)(Generate.PRODUCER->value,
            v7248,
            v15308);
          else (*Generate.expression)(v7248,
              v15308);
            princ_string(";");
          breakline_void();
          princ_string("");
          } 
        else { (*Generate.init_var)(Generate.PRODUCER->value,
              _oid_(sort_Variable(GC_OBJECT(Variable,v1140->var))));
            princ_string(";");
            breakline_void();
            statement_any(v7248,v7247,v15308);
            princ_string("");
            } 
          if (INHERIT(v1140->isa,Language._Let_star))
         (Optimize.OPT->alloc_stack = CFALSE);
        if (INHERIT(OWNER(v1140->arg),Language._Let))
         { GC__ANY(v1140 = OBJECT(Let,v1140->arg), 2);
          GC__OID(v7247 = (*Generate.c_string)(Generate.PRODUCER->value,
            GC_OID(_oid_(v1140->var))), 5);
          GC__OID(v7248 = v1140->value, 7);
          } 
        else v7230= CFALSE;
          GC_UNLOOP;} 
      } 
    (Optimize.OPT->alloc_stack = v7226);
    inner_statement_any(GC_OID(v1140->arg),v7243,v15308);
    princ_string("");
    close_block_void();
    } 
  GC_UNBIND;} 

