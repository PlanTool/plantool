/***** CLAIRE Compilation of file c:\claire\v3.3\src\compile\odefine.cl 
         [version 3.3.4 / safety 5] Sat Oct 16 06:53:36 2004 *****/

#include <claire.h>
#include <Kernel.h>
#include <Core.h>
#include <Language.h>
#include <Reader.h>
#include <Optimize.h>
ClaireType * c_type_List_Optimize(List *v9268)
{ GC_RESERVE(1);  // HOHO v3.0.55 optim !
  ;{ ClaireType *Result ;
    if (((v9268->of == (NULL)) ? CTRUE : CFALSE) != CTRUE)
     Result = param_I_class(Kernel._list,v9268->of);
    else { OID  v8003 = _oid_(Kernel.emptySet);
        { OID gc_local;
          ITERATE(v13275);
          for (START(v9268->args); NEXT(v13275);)
          { GC_LOOP;
            if (boolean_I_any(v8003) == CTRUE)
             GC__OID(v8003 = _oid_(meet_class(OBJECT(ClaireClass,v8003),class_I_type(GC_OBJECT(ClaireType,ptype_type(OBJECT(ClaireType,(*Optimize.c_type)(v13275))))))), 1);
            else GC__OID(v8003 = _oid_(class_I_type(GC_OBJECT(ClaireType,ptype_type(OBJECT(ClaireType,(*Optimize.c_type)(v13275)))))), 1);
              GC_UNLOOP;} 
          } 
        Result = nth_class1(Kernel._list,OBJECT(ClaireType,v8003));
        } 
      GC_UNBIND; return (Result);} 
  } 

OID  c_code_List_Optimize(List *v9268)
{ GC_BIND;
  (Optimize.OPT->allocation = CTRUE);
  { OID Result = 0;
    { List * v5264;
      { { List * v2072 = ((List *) GC_OBJECT(List,new_object_class(Language._List)));
          { Construct * v9477 = v2072; 
            list * v9478;
            { bag *v_list; OID v_val;
              OID v13275,CLcount;
              v_list = GC_OBJECT(list,v9268->args);
               v9478 = v_list->clone();
              for (CLcount= 1; CLcount <= v_list->length; CLcount++)
              { v13275 = (*(v_list))[CLcount];
                v_val = c_gc_I_any2(GC_OID((*Optimize.c_code)(v13275,
                  _oid_(Kernel._any))),GC_OBJECT(ClaireType,OBJECT(ClaireType,(*Optimize.c_type)(v13275))));
                
                (*((list *) v9478))[CLcount] = v_val;} 
              } 
            (v9477->args = v9478);} 
          add_I_property(Kernel.instances,Language._List,11,_oid_(v2072));
          v5264 = v2072;
          } 
        GC_OBJECT(List,v5264);} 
      if (((v9268->of == (NULL)) ? CTRUE : CFALSE) != CTRUE)
       { { ClaireBoolean * g0305I;
          { ClaireBoolean *v_or;
            { v_or = ((Optimize.compiler->safety > 4) ? CTRUE : CFALSE);
              if (v_or == CTRUE) g0305I =CTRUE; 
              else { v_or = ((equal(_oid_(v9268->of),_oid_(Kernel.emptySet)) == CTRUE) ? CTRUE : CFALSE);
                if (v_or == CTRUE) g0305I =CTRUE; 
                else { { OID  v3496;
                    { OID gc_local;
                      ITERATE(v13275);
                      v3496= _oid_(CFALSE);
                      bag *v13275_support;
                      v13275_support = GC_OBJECT(list,v9268->args);
                      for (START(v13275_support); NEXT(v13275);)
                      { GC_LOOP;
                        if (_inf_equal_type(GC_OBJECT(ClaireType,OBJECT(ClaireType,(*Optimize.c_type)(v13275))),GC_OBJECT(ClaireType,v9268->of)) != CTRUE)
                         { v3496 = Kernel.ctrue;
                          break;} 
                        GC_UNLOOP;} 
                      } 
                    v_or = not_any(v3496);
                    } 
                  if (v_or == CTRUE) g0305I =CTRUE; 
                  else g0305I = CFALSE;} 
                } 
              } 
            } 
          
          if (g0305I == CTRUE) { (v5264->of = v9268->of);
              Result = _oid_(v5264);
              } 
            else { warn_void();
            { list * v4457;
              { { OID v_bag;
                  GC_ANY(v4457= list::empty(Kernel.emptySet));
                  { { list * v11109;{ bag *v_list; OID v_val;
                        OID v13275,CLcount;
                        v_list = GC_OBJECT(list,v9268->args);
                         v11109 = v_list->clone();
                        for (CLcount= 1; CLcount <= v_list->length; CLcount++)
                        { v13275 = (*(v_list))[CLcount];
                          v_val = (*Optimize.c_type)(v13275);
                          
                          (*((list *) v11109))[CLcount] = v_val;} 
                        } 
                      
                      v_bag=_oid_(v11109);} 
                    GC_OID(v_bag);} 
                  ((list *) v4457)->addFast(v_bag);
                  ((list *) v4457)->addFast(GC_OID(_oid_(v9268->of)));} 
                GC_OBJECT(list,v4457);} 
              tformat_string("unsafe typed list: ~S not in ~S [262]\n",2,v4457);
              } 
            { OID  v6379;
              { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                (v2072->selector = Core.check_in);
                (v2072->args = list::alloc(3,_oid_(v5264),
                  _oid_(Kernel._list),
                  GC_OID(_oid_(v9268->of))));
                add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                v6379 = _oid_(v2072);
                } 
              Result = (*Optimize.c_code)(v6379,
                _oid_(Kernel._list));
              } 
            } 
          } 
        } 
      else Result = _oid_(v5264);
        } 
    GC_UNBIND; return (Result);} 
  } 

ClaireType * c_type_Set_Optimize(Set *v9268)
{ GC_RESERVE(1);  // HOHO v3.0.55 optim !
  ;{ ClaireType *Result ;
    if (((v9268->of == (NULL)) ? CTRUE : CFALSE) != CTRUE)
     Result = param_I_class(Kernel._set,v9268->of);
    else { OID  v8003 = _oid_(Kernel.emptySet);
        { OID gc_local;
          ITERATE(v13275);
          for (START(v9268->args); NEXT(v13275);)
          { GC_LOOP;
            if (boolean_I_any(v8003) == CTRUE)
             GC__OID(v8003 = _oid_(meet_class(OBJECT(ClaireClass,v8003),class_I_type(GC_OBJECT(ClaireType,OBJECT(ClaireType,(*Optimize.c_type)(v13275)))))), 1);
            else GC__OID(v8003 = _oid_(class_I_type(GC_OBJECT(ClaireType,OBJECT(ClaireType,(*Optimize.c_type)(v13275))))), 1);
              GC_UNLOOP;} 
          } 
        Result = nth_class1(Kernel._set,OBJECT(ClaireType,v8003));
        } 
      GC_UNBIND; return (Result);} 
  } 

OID  c_code_Set_Optimize(Set *v9268)
{ GC_BIND;
  (Optimize.OPT->allocation = CTRUE);
  { OID Result = 0;
    { Set * v5264;
      { { Set * v2072 = ((Set *) GC_OBJECT(Set,new_object_class(Language._Set)));
          { Construct * v9505 = v2072; 
            list * v9506;
            { bag *v_list; OID v_val;
              OID v13275,CLcount;
              v_list = GC_OBJECT(list,v9268->args);
               v9506 = v_list->clone();
              for (CLcount= 1; CLcount <= v_list->length; CLcount++)
              { v13275 = (*(v_list))[CLcount];
                v_val = c_gc_I_any2(GC_OID((*Optimize.c_code)(v13275,
                  _oid_(Kernel._any))),GC_OBJECT(ClaireType,OBJECT(ClaireType,(*Optimize.c_type)(v13275))));
                
                (*((list *) v9506))[CLcount] = v_val;} 
              } 
            (v9505->args = v9506);} 
          add_I_property(Kernel.instances,Language._Set,11,_oid_(v2072));
          v5264 = v2072;
          } 
        GC_OBJECT(Set,v5264);} 
      if (((v9268->of == (NULL)) ? CTRUE : CFALSE) != CTRUE)
       { { ClaireBoolean * g0312I;
          { ClaireBoolean *v_or;
            { v_or = ((Optimize.compiler->safety > 4) ? CTRUE : CFALSE);
              if (v_or == CTRUE) g0312I =CTRUE; 
              else { v_or = ((equal(_oid_(v9268->of),_oid_(Kernel.emptySet)) == CTRUE) ? CTRUE : CFALSE);
                if (v_or == CTRUE) g0312I =CTRUE; 
                else { { OID  v14020;
                    { OID gc_local;
                      ITERATE(v13275);
                      v14020= _oid_(CFALSE);
                      bag *v13275_support;
                      v13275_support = GC_OBJECT(list,v9268->args);
                      for (START(v13275_support); NEXT(v13275);)
                      { GC_LOOP;
                        if (_inf_equal_type(GC_OBJECT(ClaireType,OBJECT(ClaireType,(*Optimize.c_type)(v13275))),GC_OBJECT(ClaireType,v9268->of)) != CTRUE)
                         { v14020 = Kernel.ctrue;
                          break;} 
                        GC_UNLOOP;} 
                      } 
                    v_or = not_any(v14020);
                    } 
                  if (v_or == CTRUE) g0312I =CTRUE; 
                  else g0312I = CFALSE;} 
                } 
              } 
            } 
          
          if (g0312I == CTRUE) { (v5264->of = v9268->of);
              Result = _oid_(v5264);
              } 
            else { warn_void();
            { list * v14982;
              { { OID v_bag;
                  GC_ANY(v14982= list::empty(Kernel.emptySet));
                  { { list * v11137;{ bag *v_list; OID v_val;
                        OID v13275,CLcount;
                        v_list = GC_OBJECT(list,v9268->args);
                         v11137 = v_list->clone();
                        for (CLcount= 1; CLcount <= v_list->length; CLcount++)
                        { v13275 = (*(v_list))[CLcount];
                          v_val = (*Optimize.c_type)(v13275);
                          
                          (*((list *) v11137))[CLcount] = v_val;} 
                        } 
                      
                      v_bag=_oid_(v11137);} 
                    GC_OID(v_bag);} 
                  ((list *) v14982)->addFast(v_bag);
                  ((list *) v14982)->addFast(GC_OID(_oid_(v9268->of)));} 
                GC_OBJECT(list,v14982);} 
              tformat_string("unsafe typed set: ~S not in ~S [262]\n",2,v14982);
              } 
            { OID  v519;
              { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                (v2072->selector = Core.check_in);
                (v2072->args = list::alloc(3,_oid_(v5264),
                  _oid_(Kernel._set),
                  GC_OID(_oid_(v9268->of))));
                add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                v519 = _oid_(v2072);
                } 
              Result = (*Optimize.c_code)(v519,
                _oid_(Kernel._set));
              } 
            } 
          } 
        } 
      else Result = _oid_(v5264);
        } 
    GC_UNBIND; return (Result);} 
  } 

ClaireType * c_type_Tuple_Optimize(Tuple *v9268)
{ GC_BIND;
  { ClaireType *Result ;
    { list * v1480;
      { { bag *v_list; OID v_val;
          OID v5264,CLcount;
          v_list = v9268->args;
           v1480 = v_list->clone();
          for (CLcount= 1; CLcount <= v_list->length; CLcount++)
          { v5264 = (*(v_list))[CLcount];
            v_val = (*Optimize.c_type)(v5264);
            
            (*((list *) v1480))[CLcount] = v_val;} 
          } 
        GC_OBJECT(list,v1480);} 
      Result = tuple_I_list(v1480);
      } 
    GC_UNBIND; return (Result);} 
  } 

OID  c_code_Tuple_Optimize(Tuple *v9268)
{ GC_BIND;
  (Optimize.OPT->allocation = CTRUE);
  { OID Result = 0;
    { Tuple * v2072 = ((Tuple *) GC_OBJECT(Tuple,new_object_class(Language._Tuple)));
      { Construct * v9513 = v2072; 
        list * v9514;
        { bag *v_list; OID v_val;
          OID v13275,CLcount;
          v_list = GC_OBJECT(list,v9268->args);
           v9514 = v_list->clone();
          for (CLcount= 1; CLcount <= v_list->length; CLcount++)
          { v13275 = (*(v_list))[CLcount];
            v_val = c_gc_I_any2(GC_OID((*Optimize.c_code)(v13275,
              _oid_(Kernel._any))),GC_OBJECT(ClaireType,OBJECT(ClaireType,(*Optimize.c_type)(v13275))));
            
            (*((list *) v9514))[CLcount] = v_val;} 
          } 
        (v9513->args = v9514);} 
      add_I_property(Kernel.instances,Language._Tuple,11,_oid_(v2072));
      Result = _oid_(v2072);
      } 
    GC_UNBIND; return (Result);} 
  } 

ClaireType * c_type_Definition_Optimize(Definition *v9268)
{ { ClaireType *Result ;
    if (_inf_equalt_class(v9268->arg,Kernel._exception) == CTRUE)
     Result = Kernel.emptySet;
    else Result = v9268->arg;
      return (Result);} 
  } 

OID  c_code_Definition_Optimize(Definition *v9268,ClaireClass *v5259)
{ GC_BIND;
  { OID Result = 0;
    { ClaireClass * v13254 = v9268->arg;
      Variable * v13274;
      { { int  v9121;
          { (Optimize.OPT->max_vars = (Optimize.OPT->max_vars+1));
            v9121 = 0;
            } 
          v13274 = Variable_I_symbol(OBJECT(symbol,Optimize._starname_star->value),v9121,v13254);
          } 
        GC_OBJECT(Variable,v13274);} 
      OID  v13275 = GC_OID(total_ask_class(v13254,GC_OBJECT(list,v9268->args)));
      if (v13254->open <= 0)
       close_exception(((general_error *) (*Core._general_error)(_string_("[105] cannot instantiate ~S"),
        _oid_(list::alloc(1,_oid_(v13254))))));
      if (boolean_I_any(v13275) == CTRUE)
       Result = (*Optimize.c_code)(v13275,
        _oid_(v5259));
      else { OID  v10082;
          { Let * v2072 = ((Let *) GC_OBJECT(Let,new_object_class(Language._Let)));
            (v2072->var = v13274);
            { Let * v9539 = v2072; 
              OID  v9540;
              { Cast * v2072 = ((Cast *) GC_OBJECT(Cast,new_object_class(Language._Cast)));
                { Cast * v9541 = v2072; 
                  OID  v9543;
                  { OID  v14887;
                    { { OID  v15848;
                        { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                          (v2072->selector = Core.new_I);
                          (v2072->args = list::alloc(1,_oid_(v13254)));
                          add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                          v15848 = _oid_(v2072);
                          } 
                        v14887 = (*Optimize.c_code)(v15848,
                          _oid_(Kernel._object));
                        } 
                      GC_OID(v14887);} 
                    v9543 = c_gc_I_any1(v14887);
                    } 
                  (v9541->arg = v9543);} 
                (v2072->set_arg = v13254);
                add_I_property(Kernel.instances,Language._Cast,11,_oid_(v2072));
                v9540 = _oid_(v2072);
                } 
              (v9539->value = v9540);} 
            { Let * v9546 = v2072; 
              OID  v9567;
              { Do * v2072 = ((Do *) GC_OBJECT(Do,new_object_class(Language._Do)));
                store_object(v2072,
                  2,
                  Kernel._object,
                  analyze_I_class(v13254,_oid_(v13274),GC_OBJECT(list,v9268->args),list::empty()),
                  CFALSE);
                add_I_property(Kernel.instances,Language._Do,11,_oid_(v2072));
                v9567 = _oid_(v2072);
                } 
              (v9546->arg = v9567);} 
            add_I_property(Kernel.instances,Language._Let,11,_oid_(v2072));
            v10082 = _oid_(v2072);
            } 
          Result = (*Optimize.c_code)(v10082,
            _oid_(v5259));
          } 
        } 
    GC_UNBIND; return (Result);} 
  } 

OID  total_ask_class(ClaireClass *v9268,list *v5252)
{ GC_BIND;
  { OID Result = 0;
    { list * v15468 = GC_OBJECT(list,OBJECT(list,(*Optimize.get_indexed)(_oid_(v9268))));
      int  v5254 = v15468->length;
      { ClaireBoolean * g0332I;
        { ClaireBoolean *v_and;
          { v_and = not_any(_oid_(Optimize.compiler->diet_ask));
            if (v_and == CFALSE) g0332I =CFALSE; 
            else { v_and = ((v5252->length == (v5254-1)) ? CTRUE : CFALSE);
              if (v_and == CFALSE) g0332I =CFALSE; 
              else { v_and = ((v9268->open == ClEnv->ephemeral) ? CTRUE : ((_inf_equalt_class(v9268,Kernel._exception) == CTRUE) ? CTRUE : CFALSE));
                if (v_and == CFALSE) g0332I =CFALSE; 
                else { v_and = ((v5254 <= 4) ? CTRUE : CFALSE);
                  if (v_and == CFALSE) g0332I =CFALSE; 
                  else { { OID  v8066;
                      { int  v5249 = 2;
                        int  v9568 = v5254;
                        { v8066= _oid_(CFALSE);
                          while ((v5249 <= v9568))
                          { if ((((*Kernel.srange)((*(v15468))[v5249]) == _oid_(Kernel._integer)) ? CTRUE : (((*Kernel.srange)((*(v15468))[v5249]) == _oid_(Kernel._any)) ? CTRUE : CFALSE)) != CTRUE)
                             { v8066 = Kernel.ctrue;
                              break;} 
                            ++v5249;
                            } 
                          } 
                        } 
                      v_and = not_any(v8066);
                      } 
                    if (v_and == CFALSE) g0332I =CFALSE; 
                    else g0332I = CTRUE;} 
                  } 
                } 
              } 
            } 
          } 
        
        if (g0332I == CTRUE) { OID  v13254;
            { { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                (v2072->selector = ((v5252->length == 0) ?
                  Core.new_I :
                  Optimize.anyObject_I ));
                { Call * v9571 = v2072; 
                  list * v9572;
                  { list * v10949;
                    { { bag *v_list; OID v_val;
                        OID v5264,CLcount;
                        v_list = v5252;
                         v10949 = v_list->clone();
                        for (CLcount= 1; CLcount <= v_list->length; CLcount++)
                        { v5264 = (*(v_list))[CLcount];
                          v_val = c_gc_I_any1(GC_OID((*Optimize.c_code)(GC_OID((*(OBJECT(bag,(*Core.args)(v5264))))[2]),
                            _oid_(Kernel._any))));
                          
                          (*((list *) v10949))[CLcount] = v_val;} 
                        } 
                      GC_OBJECT(list,v10949);} 
                    v9572 = cons_any(_oid_(v9268),v10949);
                    } 
                  (v9571->args = v9572);} 
                add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                v13254 = _oid_(v2072);
                } 
              GC_OID(v13254);} 
            OID  v5253 = GC_OID(_oid_(_at_property1(Kernel.close,v9268)));
            if (_inf_equal_type(v9268,Kernel._exception) != CTRUE)
             (Optimize.OPT->allocation = CTRUE);
            if (v5252->length == 0)
             v13254= GC_OID((*Optimize.c_code)(v13254));
            if (boolean_I_any(v5253) == CTRUE)
             { Call_method1 * v2072 = ((Call_method1 *) GC_OBJECT(Call_method1,new_object_class(Language._Call_method1)));
              update_property(Kernel.arg,
                v2072,
                2,
                Kernel._object,
                v5253);
              (v2072->args = list::alloc(1,v13254));
              add_I_property(Kernel.instances,Language._Call_method1,11,_oid_(v2072));
              Result = _oid_(v2072);
              } 
            else Result = v13254;
              } 
          else Result = Kernel.cfalse;
        } 
      } 
    GC_UNBIND; return (Result);} 
  } 

OID  analyze_I_class(ClaireClass *v5243,OID v9268,list *v13263,list *v15468)
{ GC_RESERVE(15);  // v3.0.55 optim !
  { OID Result = 0;
    { ClaireBoolean * v15097 = ((v5243->open != 4) ? ((boolean_I_any(_oid_(v15468)) != CTRUE) ? ((Optimize.compiler->class2file_ask != CTRUE) ? CTRUE: CFALSE): CFALSE): CFALSE);
      list * v5258;
      { { bag *v_list; OID v_val;
          OID v5264,CLcount;
          v_list = v13263;
           v5258 = v_list->clone(Kernel._any);
          for (CLcount= 1; CLcount <= v_list->length; CLcount++)
          { v5264 = (*(v_list))[CLcount];
            { OID  v5256 = GC_OID((*(OBJECT(Call,v5264)->args))[1]);
              OID  v5265 = GC_OID((*(OBJECT(Call,v5264)->args))[2]);
              ClaireObject * v5259 = GC_OBJECT(ClaireObject,_at_property1(OBJECT(property,v5256),v5243));
              ClaireBoolean * v14222 = (((*Kernel.open)(v5256) == 0) ? ((Kernel._slot == v5259->isa) ? CTRUE: CFALSE): CFALSE);
              GC__ANY(v15468 = v15468->addFast(v5256), 4);
              { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                (v2072->selector = ((v14222 == CTRUE) ?
                  Kernel.put :
                  Core.write ));
                { Call * v9574 = v2072; 
                  list * v9576;
                  { OID v_bag;
                    GC_ANY(v9576= list::empty(Kernel.emptySet));
                    ((list *) v9576)->addFast(v5256);
                    ((list *) v9576)->addFast(v9268);
                    { if ((v14222 != CTRUE) || 
                          (_inf_equal_type(GC_OBJECT(ClaireType,OBJECT(ClaireType,(*Optimize.c_type)(v5265))),GC_OBJECT(ClaireType,OBJECT(ClaireType,(*Kernel.range)(_oid_(v5259))))) == CTRUE))
                       v_bag = v5265;
                      else v_bag = c_check_any(GC_OID((*Optimize.c_code)(v5265,
                          _oid_(Kernel._any))),GC_OID((*Optimize.c_code)(GC_OID((*Kernel.range)(_oid_(v5259))),
                          _oid_(Kernel._type))));
                        GC_OID(v_bag);} 
                    ((list *) v9576)->addFast(v_bag);} 
                  (v9574->args = v9576);} 
                add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                v_val = _oid_(v2072);
                } 
              } 
            
            (*((list *) v5258))[CLcount] = v_val;} 
          } 
        GC_OBJECT(list,v5258);} 
      if (v15097 == CTRUE)
       { { OID  v13832;
          { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
            (v2072->selector = Kernel.add);
            (v2072->args = list::alloc(3,_oid_(Kernel.instances),
              _oid_(v5243),
              v9268));
            add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
            v13832 = _oid_(v2072);
            } 
          v5258 = v5258->addFast(v13832);
          } 
        GC_OBJECT(list,v5258);} 
      if (Optimize.compiler->class2file_ask != CTRUE)
       { OID gc_local;
        ITERATE(v5259);
        bag *v5259_support;
        v5259_support = GC_OBJECT(list,OBJECT(bag,(*Optimize.get_indexed)(_oid_(v5243))));
        for (START(v5259_support); NEXT(v5259);)
        { GC_LOOP;
          { property * v5256 = OBJECT(restriction,v5259)->selector;
            OID  v5263 = GC_OID(OBJECT(slot,v5259)->DEFAULT);
            { ClaireBoolean * g0340I;
              { ClaireBoolean *v_and;
                { v_and = known_ask_any(v5263);
                  if (v_and == CFALSE) g0340I =CFALSE; 
                  else { { OID  v3167;
                      if (multi_ask_any(_oid_(v5256)) == CTRUE)
                       v3167 = v5263;
                      else v3167 = Kernel.ctrue;
                        v_and = boolean_I_any(v3167);
                      } 
                    if (v_and == CFALSE) g0340I =CFALSE; 
                    else { v_and = not_any(_oid_(v15468->memq(_oid_(v5256))));
                      if (v_and == CFALSE) g0340I =CFALSE; 
                      else { v_and = ((((v5256->inverse == (NULL)) ? CTRUE : CFALSE) != CTRUE) ? CTRUE : ((((v5256->if_write == CNULL) ? CTRUE : CFALSE) != CTRUE) ? CTRUE : (((OBJECT(slot,v5259)->srange != Kernel._object) && 
                            ((OBJECT(slot,v5259)->srange != Kernel._float) && 
                              (inherit_ask_class(OWNER(v5263),Kernel._integer) != CTRUE))) ? CTRUE : CFALSE)));
                        if (v_and == CFALSE) g0340I =CFALSE; 
                        else g0340I = CTRUE;} 
                      } 
                    } 
                  } 
                } 
              
              if (g0340I == CTRUE) { OID  v3776;
                  if (designated_ask_any(v5263) == CTRUE)
                   v3776 = v5263;
                  else { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                      (v2072->selector = Kernel.DEFAULT);
                      { Call * v9600 = v2072; 
                        list * v9602;
                        { OID v_bag;
                          GC_ANY(v9602= list::empty(Kernel.emptySet));
                          { { Cast * v2072 = ((Cast *) GC_OBJECT(Cast,new_object_class(Language._Cast)));
                              { Cast * v9603 = v2072; 
                                OID  v9604;
                                { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                                  (v2072->selector = Core._at);
                                  (v2072->args = list::alloc(2,_oid_(v5256),_oid_(v5243)));
                                  add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                                  v9604 = _oid_(v2072);
                                  } 
                                (v9603->arg = v9604);} 
                              (v2072->set_arg = Kernel._slot);
                              add_I_property(Kernel.instances,Language._Cast,11,_oid_(v2072));
                              v_bag = _oid_(v2072);
                              } 
                            GC_OID(v_bag);} 
                          ((list *) v9602)->addFast(v_bag);} 
                        (v9600->args = v9602);} 
                      add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                      v3776 = _oid_(v2072);
                      } 
                    { { OID  v7972;
                      { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                        (v2072->selector = Core.write);
                        (v2072->args = list::alloc(3,_oid_(v5256),
                          v9268,
                          v3776));
                        add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                        v7972 = _oid_(v2072);
                        } 
                      v5258 = v5258->addFast(v7972);
                      } 
                     GC__ANY(v5258, 7);} 
                  } 
                } 
            } 
          GC_UNLOOP;} 
        } 
      { OID  v5253 = GC_OID(_oid_(_at_property1(Kernel.close,v5243)));
        { OID  v8933;
          if (boolean_I_any(v5253) == CTRUE)
           { Call_method1 * v2072 = ((Call_method1 *) GC_OBJECT(Call_method1,new_object_class(Language._Call_method1)));
            update_property(Kernel.arg,
              v2072,
              2,
              Kernel._object,
              v5253);
            (v2072->args = list::alloc(1,v9268));
            add_I_property(Kernel.instances,Language._Call_method1,11,_oid_(v2072));
            v8933 = _oid_(v2072);
            } 
          else v8933 = v9268;
            v5258 = v5258->addFast(v8933);
          } 
        } 
      Result = _oid_(v5258);
      } 
    GC_UNBIND; return (Result);} 
  } 

OID  c_code_Defobj_Optimize(Defobj *v9268,ClaireClass *v5259)
{ GC_BIND;
  { OID Result = 0;
    { ClaireBoolean * v13253 = Optimize.OPT->allocation;
      ClaireClass * v13254 = v9268->arg;
      OID  v5255 = get_symbol(v9268->ident);
      OID  v13274;
      { if ((v5255 != CNULL) && 
            (inherit_ask_class(OWNER(v5255),Core._global_variable) != CTRUE))
         v13274 = v5255;
        else { Variable * v11256;{ int  v190;
              { (Optimize.OPT->max_vars = (Optimize.OPT->max_vars+1));
                v190 = 0;
                } 
              v11256 = Variable_I_symbol(OBJECT(symbol,Optimize._starname_star->value),v190,v13254);
              } 
            
            v13274=_oid_(v11256);} 
          GC_OID(v13274);} 
      Call * v2005;
      { { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
          (v2072->selector = Optimize.object_I);
          (v2072->args = list::alloc(2,_oid_(v9268->ident),_oid_(v13254)));
          add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
          v2005 = v2072;
          } 
        GC_OBJECT(Call,v2005);} 
      OID  v2006 = GC_OID(analyze_I_class(v13254,v13274,GC_OBJECT(list,v9268->args),list::alloc(1,_oid_(Kernel.name))));
      OID  v13275;
      if (inherit_ask_class(OWNER(v13274),Language._Variable) != CTRUE)
       { Do * v2072 = ((Do *) GC_OBJECT(Do,new_object_class(Language._Do)));
        store_object(v2072,
          2,
          Kernel._object,
          (*Kernel.cons)(_oid_(v2005),
            v2006),
          CFALSE);
        add_I_property(Kernel.instances,Language._Do,11,_oid_(v2072));
        v13275 = _oid_(v2072);
        } 
      else { Let * v2072 = ((Let *) GC_OBJECT(Let,new_object_class(Language._Let)));
          store_object(v2072,
            2,
            Kernel._object,
            v13274,
            CFALSE);
          (v2072->value = _oid_(v2005));
          { Let * v9631 = v2072; 
            OID  v9632;
            { Do * v2072 = ((Do *) GC_OBJECT(Do,new_object_class(Language._Do)));
              store_object(v2072,
                2,
                Kernel._object,
                v2006,
                CFALSE);
              add_I_property(Kernel.instances,Language._Do,11,_oid_(v2072));
              v9632 = _oid_(v2072);
              } 
            (v9631->arg = v9632);} 
          add_I_property(Kernel.instances,Language._Let,11,_oid_(v2072));
          v13275 = _oid_(v2072);
          } 
        if (v13254->open <= 0)
       close_exception(((general_error *) (*Core._general_error)(_string_("[105] cannot instantiate ~S"),
        _oid_(list::alloc(1,_oid_(v13254))))));
      if (v5255 != CNULL)
       { if (contain_ask_list(Optimize.OPT->objects,v5255) != CTRUE)
         { GC_OBJECT(list,Optimize.OPT->objects)->addFast(v5255);
          (*Optimize.c_register)(v5255);
          } 
        } 
      else { warn_void();
          tformat_string("~S is unknown [265]\n",2,list::alloc(1,_oid_(v9268->ident)));
          } 
        v13275= GC_OID((*Optimize.c_code)(v13275,
        _oid_(v5259)));
      if (_inf_equal_type(v9268->arg,Kernel._exception) == CTRUE)
       (Optimize.OPT->allocation = v13253);
      Result = v13275;
      } 
    GC_UNBIND; return (Result);} 
  } 

OID  c_code_Defclass_Optimize(Defclass *v9268,ClaireClass *v5259)
{ GC_BIND;
  { OID Result = 0;
    { symbol * v10312 = v9268->ident;
      OID  v5255 = get_symbol(v10312);
      Call * v4985;
      { ClaireObject *V_CC ;
        if (v5255 != CNULL)
         { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
          (v2072->selector = Core.class_I);
          (v2072->args = list::alloc(2,_oid_(v10312),_oid_(v9268->arg)));
          add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
          V_CC = v2072;
          } 
        else close_exception(((general_error *) (*Core._general_error)(_string_("[internal] cannot compile unknown class ~S"),
            _oid_(list::alloc(1,_oid_(v10312))))));
          v4985= (Call *) V_CC;} 
      Do * v13275;
      { { Do * v2072 = ((Do *) GC_OBJECT(Do,new_object_class(Language._Do)));
          { Do * v9634 = v2072; 
            list * v9635;
            { list * v5956;
              { { list * v6917;
                  { { bag *v_list; OID v_val;
                      OID v5264,CLcount;
                      v_list = GC_OBJECT(list,v9268->args);
                       v6917 = v_list->clone();
                      for (CLcount= 1; CLcount <= v_list->length; CLcount++)
                      { v5264 = (*(v_list))[CLcount];
                        { OID  v5263 = CNULL;
                          if (INHERIT(OWNER(v5264),Language._Call))
                           { v5263= GC_OID((*(OBJECT(Call,v5264)->args))[2]);
                            v5264= GC_OID((*(OBJECT(Call,v5264)->args))[1]);
                            } 
                          { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                            (v2072->selector = Kernel.add_slot);
                            (v2072->args = list::alloc(5,v5255,
                              _oid_(make_a_property_any(_oid_(OBJECT(Variable,v5264)->pname))),
                              GC_OID((*Kernel.range)(v5264)),
                              v5263,
                              0));
                            add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                            v_val = _oid_(v2072);
                            } 
                          } 
                        
                        (*((list *) v6917))[CLcount] = v_val;} 
                      } 
                    GC_OBJECT(list,v6917);} 
                  list * v7878;
                  if (v9268->params->length != 0)
                   { OID v_bag;
                    GC_ANY(v7878= list::empty(Kernel.emptySet));
                    { { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                        (v2072->selector = Kernel.put);
                        (v2072->args = list::alloc(3,_oid_(Kernel.params),
                          v5255,
                          GC_OID(_oid_(v9268->params))));
                        add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                        v_bag = _oid_(v2072);
                        } 
                      GC_OID(v_bag);} 
                    ((list *) v7878)->addFast(v_bag);} 
                  else v7878 = list::empty();
                    v5956 = append_list(v6917,v7878);
                  } 
                GC_OBJECT(list,v5956);} 
              v9635 = cons_any(_oid_(v4985),v5956);
              } 
            (v9634->args = v9635);} 
          add_I_property(Kernel.instances,Language._Do,11,_oid_(v2072));
          v13275 = v2072;
          } 
        GC_OBJECT(Do,v13275);} 
      if (contain_ask_list(Optimize.OPT->objects,v5255) != CTRUE)
       { GC_OBJECT(list,Optimize.OPT->objects)->addFast(v5255);
        (*Optimize.c_register)(v5255);
        } 
      Result = (*Optimize.c_code)(_oid_(v13275),
        _oid_(v5259));
      } 
    GC_UNBIND; return (Result);} 
  } 

ClaireType * c_type_Defmethod_Optimize(Defmethod *v9268)
{ return (Kernel._any);} 

OID  c_code_Defmethod_Optimize(Defmethod *v9268)
{ GC_BIND;
  { OID Result = 0;
    { property * v15600 = v9268->arg->selector;
      list * v5252 = GC_OBJECT(list,v9268->arg->args);
      list * v15474 = (((v5252->length == 1) && 
          ((*(v5252))[1] == _oid_(ClEnv))) ?
        list::alloc(1,GC_OID(_oid_(Variable_I_symbol(OBJECT(symbol,Optimize._starname_star->value),0,Kernel._void)))) :
        v5252 );
      list * v15471 = GC_OBJECT(list,extract_signature_I_list(v15474));
      list * v5945 = GC_OBJECT(list,extract_range_any(GC_OID(v9268->set_arg),v15474,GC_OBJECT(list,OBJECT(list,Language.LDEF->value))));
      OID  v8090;
      if ((boolean_I_any(v9268->inline_ask) == CTRUE) && 
          (Optimize.compiler->inline_ask == CTRUE))
       { print_in_string_void();
        princ_string("lambda[(");
        ppvariable_list(v15474);
        princ_string("),");
        print_any(GC_OID(v9268->body));
        princ_string("]");
        v8090 = _string_(end_of_print_void());
        } 
      else v8090 = Kernel.cfalse;
        list * v11302 = GC_OBJECT(list,extract_status_new_any(GC_OID(v9268->body)));
      OID  v12479 = GC_OID((*Core._at)(_oid_(v15600),
        (*(v15471))[2]));
      method * v5253;
      { ClaireObject *V_CC ;
        if (Kernel._method == OBJECT(ClaireObject,v12479)->isa)
         V_CC = OBJECT(method,v12479);
        else close_exception(((general_error *) (*Core._general_error)(_string_("[internal] the method ~S @ ~S is not known"),
            _oid_(list::alloc(2,_oid_(v15600),(*(v15471))[2])))));
          v5253= (method *) V_CC;} 
      OID  v11284 = v5253->status;
      ((*(v11302))[2]=get_property(Kernel.functional,v5253));
      if ((Optimize.compiler->inline_ask != CTRUE) && 
          ((v15600 == Language.Iterate) || 
              (v15600 == Language.iterate)))
       Result = Core.nil->value;
      else if (((*(v5945))[1] == _oid_(Kernel._void)) && 
          (sort_pattern_ask_list(v15474,GC_OID(v9268->body)) == CTRUE))
       Result = sort_code_Defmethod(v9268,v15474);
      else { if ((*(v11302))[3] != _oid_(Kernel.body))
           { char * v15515 = GC_STRING(string_v((*Optimize.function_name)(_oid_(v15600),
              (*(v15471))[2],
              (*(v11302))[2])));
            lambda * v15453 = GC_OBJECT(lambda,lambda_I_list(v15474,(*(v11302))[3]));
            int  v8123 = ((Optimize.OPT->recompute == CTRUE) ?
              c_status_any(GC_OID(v15453->body),GC_OBJECT(list,v15453->vars)) :
              status_I_restriction(v5253) );
            compile_lambda_string(v15515,v15453,_oid_(v5253));
            if (((*(v11302))[1] == CNULL) || 
                (Optimize.OPT->recompute == CTRUE))
             { if ((Optimize.OPT->use_nth_equal != CTRUE) && 
                  (BCONTAIN(v8123,2)))
               v8123= (v8123-exp2_integer(2));
              if ((Optimize.OPT->use_update != CTRUE) && 
                  (BCONTAIN(v8123,3)))
               v8123= (v8123-exp2_integer(3));
              if ((Optimize.OPT->allocation != CTRUE) && 
                  (BCONTAIN(v8123,1)))
               v8123= (v8123-exp2_integer(1));
              ((*(v11302))[1]=v8123);
              } 
            ((*(v11302))[2]=_oid_(make_function_string(v15515)));
            } 
          if (INHERIT(OWNER(v9268->set_arg),Core._global_variable))
           ((*(v5945))[1]=v9268->set_arg);
          else if ((INHERIT(v5253->range->isa,Kernel._class)) && 
              (inherit_ask_class(OWNER((*(v5945))[1]),Kernel._class) != CTRUE))
           ((*(v5945))[1]=_oid_(v5253->range));
          { OID  v13264 = GC_OID(add_method_I_method(v5253,
              OBJECT(list,(*(v15471))[1]),
              (*(v5945))[1],
              (*(v11302))[1],
              OBJECT(ClaireFunction,(*(v11302))[2])));
            { OID  v12636;
              if ((boolean_I_any(v9268->inline_ask) == CTRUE) && 
                  ((Optimize.compiler->inline_ask == CTRUE) && 
                    (Optimize.compiler->diet_ask != CTRUE)))
               { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                (v2072->selector = Core.inlineok_ask);
                (v2072->args = list::alloc(2,v13264,v8090));
                add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                v12636 = _oid_(v2072);
                } 
              else if ((boolean_I_any((*(v5945))[2]) == CTRUE) && 
                  (Optimize.compiler->diet_ask != CTRUE))
               { char * v15515 = GC_STRING(append_string(GC_STRING(string_v((*Optimize.function_name)(_oid_(v15600),
                  (*(v15471))[2],
                  (*(v11302))[2]))),"_type"));
                compile_lambda_string(v15515,OBJECT(lambda,(*(v5945))[2]),_oid_(Kernel._type));
                { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                  (v2072->selector = Core.write);
                  (v2072->args = list::alloc(3,Language.typing->value,
                    v13264,
                    _oid_(make_function_string(v15515))));
                  add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                  v12636 = _oid_(v2072);
                  } 
                } 
              else v12636 = v13264;
                Result = (*Optimize.c_code)(v12636);
              } 
            } 
          } 
        } 
    GC_UNBIND; return (Result);} 
  } 

ClaireBoolean * sort_pattern_ask_list(list *v15474,OID v10079)
{ GC_BIND;
  { ClaireBoolean *Result ;
    { ClaireBoolean *v_and;
      { v_and = ((v15474->length == 1) ? CTRUE : CFALSE);
        if (v_and == CFALSE) Result =CFALSE; 
        else { if (INHERIT(OWNER(v10079),Language._Call))
           { ClaireBoolean *v_and1;
            { v_and1 = ((OBJECT(Call,v10079)->selector == Core.sort) ? CTRUE : CFALSE);
              if (v_and1 == CFALSE) v_and =CFALSE; 
              else { { OID  v15065 = GC_OID((*(OBJECT(Call,v10079)->args))[1]);
                  v_and1 = ((INHERIT(OWNER(v15065),Language._Call)) ?
                    ((OBJECT(Call,v15065)->selector == Core._at) ? ((INHERIT(OWNER((*(OBJECT(Call,v15065)->args))[1]),Kernel._property)) ? CTRUE: CFALSE): CFALSE) :
                    CFALSE );
                  } 
                if (v_and1 == CFALSE) v_and =CFALSE; 
                else { v_and1 = ((equal(lexical_build_any(GC_OID((*(OBJECT(Call,v10079)->args))[2]),v15474,0),(*(v15474))[1]) == CTRUE) ? CTRUE : CFALSE);
                  if (v_and1 == CFALSE) v_and =CFALSE; 
                  else v_and = CTRUE;} 
                } 
              } 
            } 
          else v_and = CFALSE;
            if (v_and == CFALSE) Result =CFALSE; 
          else Result = CTRUE;} 
        } 
      } 
    GC_UNBIND; return (Result);} 
  } 

OID  sort_code_Defmethod(Defmethod *v9268,list *v15474)
{ GC_BIND;
  { OID Result = 0;
    { OID  v5252 = (*(v15474))[1];
      OID  v5246 = GC_OID((*(OBJECT(bag,(*Core.args)(GC_OID((*(OBJECT(bag,(*Core.args)(GC_OID(v9268->body)))))[1])))))[1]);
      Variable * v5253 = GC_OBJECT(Variable,Variable_I_symbol(symbol_I_string2("m"),0,Kernel._integer));
      Variable * v5254 = GC_OBJECT(Variable,Variable_I_symbol(symbol_I_string2("n"),0,Kernel._integer));
      Variable * v5264 = GC_OBJECT(Variable,Variable_I_symbol(symbol_I_string2("x"),0,GC_OBJECT(ClaireType,member_type(GC_OBJECT(ClaireType,OBJECT(ClaireType,(*Kernel.range)(v5252)))))));
      Variable * v5256 = GC_OBJECT(Variable,Variable_I_symbol(symbol_I_string2("p"),0,Kernel._integer));
      Variable * v1353 = GC_OBJECT(Variable,Variable_I_symbol(symbol_I_string2("q"),0,Kernel._integer));
      Defmethod * v4532;
      { { Defmethod * v2072 = ((Defmethod *) GC_OBJECT(Defmethod,new_object_class(Language._Defmethod)));
          (v2072->arg = v9268->arg);
          (v2072->inline_ask = Kernel.cfalse);
          (v2072->set_arg = v9268->set_arg);
          { Defmethod * v9662 = v2072; 
            OID  v9663;
            { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
              (v2072->selector = v9268->arg->selector);
              { Call * v9664 = v2072; 
                list * v9665;
                { OID v_bag;
                  GC_ANY(v9665= list::empty(Kernel.emptySet));
                  ((list *) v9665)->addFast(1);
                  { { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                      (v2072->selector = Kernel.length);
                      (v2072->args = list::alloc(1,(*(v15474))[1]));
                      add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                      v_bag = _oid_(v2072);
                      } 
                    GC_OID(v_bag);} 
                  ((list *) v9665)->addFast(v_bag);
                  ((list *) v9665)->addFast(v5252);} 
                (v9664->args = v9665);} 
              add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
              v9663 = _oid_(v2072);
              } 
            (v9662->body = v9663);} 
          add_I_property(Kernel.instances,Language._Defmethod,11,_oid_(v2072));
          v4532 = v2072;
          } 
        GC_OBJECT(Defmethod,v4532);} 
      If * v1343;
      { { If * v2072 = ((If *) GC_OBJECT(If,new_object_class(Language._If)));
          { If * v9666 = v2072; 
            OID  v9667;
            { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
              (v2072->selector = Kernel._sup);
              (v2072->args = list::alloc(2,_oid_(v5253),_oid_(v5254)));
              add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
              v9667 = _oid_(v2072);
              } 
            (v9666->test = v9667);} 
          { If * v9668 = v2072; 
            OID  v9669;
            { Let * v2072 = ((Let *) GC_OBJECT(Let,new_object_class(Language._Let)));
              (v2072->var = v5264);
              { Let * v9670 = v2072; 
                OID  v9691;
                { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                  (v2072->selector = Kernel.nth);
                  (v2072->args = list::alloc(2,v5252,_oid_(v5254)));
                  add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                  v9691 = _oid_(v2072);
                  } 
                (v9670->value = v9691);} 
              { Let * v9692 = v2072; 
                OID  v9693;
                { If * v2072 = ((If *) GC_OBJECT(If,new_object_class(Language._If)));
                  { If * v9694 = v2072; 
                    OID  v9696;
                    { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                      (v2072->selector = Kernel._equal);
                      { Call * v9697 = v2072; 
                        list * v9698;
                        { OID v_bag;
                          GC_ANY(v9698= list::empty(Kernel.emptySet));
                          ((list *) v9698)->addFast(_oid_(v5253));
                          { { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                              (v2072->selector = Core._plus);
                              (v2072->args = list::alloc(2,_oid_(v5254),1));
                              add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                              v_bag = _oid_(v2072);
                              } 
                            GC_OID(v_bag);} 
                          ((list *) v9698)->addFast(v_bag);} 
                        (v9697->args = v9698);} 
                      add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                      v9696 = _oid_(v2072);
                      } 
                    (v9694->test = v9696);} 
                  { If * v9699 = v2072; 
                    OID  v9700;
                    { If * v2072 = ((If *) GC_OBJECT(If,new_object_class(Language._If)));
                      { If * v9701 = v2072; 
                        OID  v9722;
                        { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                          update_property(Kernel.selector,
                            v2072,
                            2,
                            Kernel._object,
                            v5246);
                          { Call * v9723 = v2072; 
                            list * v9724;
                            { OID v_bag;
                              GC_ANY(v9724= list::empty(Kernel.emptySet));
                              { { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                                  (v2072->selector = Kernel.nth);
                                  (v2072->args = list::alloc(2,v5252,_oid_(v5253)));
                                  add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                                  v_bag = _oid_(v2072);
                                  } 
                                GC_OID(v_bag);} 
                              ((list *) v9724)->addFast(v_bag);
                              ((list *) v9724)->addFast(_oid_(v5264));} 
                            (v9723->args = v9724);} 
                          add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                          v9722 = _oid_(v2072);
                          } 
                        (v9701->test = v9722);} 
                      { If * v9725 = v2072; 
                        OID  v9726;
                        { Do * v2072 = ((Do *) GC_OBJECT(Do,new_object_class(Language._Do)));
                          { Do * v9727 = v2072; 
                            list * v9728;
                            { OID v_bag;
                              GC_ANY(v9728= list::empty(Kernel.emptySet));
                              { { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                                  (v2072->selector = Kernel.nth_equal);
                                  { Call * v9729 = v2072; 
                                    list * v9730;
                                    { OID v_bag;
                                      GC_ANY(v9730= list::empty(Kernel.emptySet));
                                      ((list *) v9730)->addFast(v5252);
                                      ((list *) v9730)->addFast(_oid_(v5254));
                                      { { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                                          (v2072->selector = Kernel.nth);
                                          (v2072->args = list::alloc(2,v5252,_oid_(v5253)));
                                          add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                                          v_bag = _oid_(v2072);
                                          } 
                                        GC_OID(v_bag);} 
                                      ((list *) v9730)->addFast(v_bag);} 
                                    (v9729->args = v9730);} 
                                  add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                                  v_bag = _oid_(v2072);
                                  } 
                                GC_OID(v_bag);} 
                              ((list *) v9728)->addFast(v_bag);
                              { { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                                  (v2072->selector = Kernel.nth_equal);
                                  (v2072->args = list::alloc(3,v5252,
                                    _oid_(v5253),
                                    _oid_(v5264)));
                                  add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                                  v_bag = _oid_(v2072);
                                  } 
                                GC_OID(v_bag);} 
                              ((list *) v9728)->addFast(v_bag);} 
                            (v9727->args = v9728);} 
                          add_I_property(Kernel.instances,Language._Do,11,_oid_(v2072));
                          v9726 = _oid_(v2072);
                          } 
                        (v9725->arg = v9726);} 
                      add_I_property(Kernel.instances,Language._If,11,_oid_(v2072));
                      (v2072->other = Kernel.cfalse);
                      v9700 = _oid_(v2072);
                      } 
                    (v9699->arg = v9700);} 
                  { If * v9731 = v2072; 
                    OID  v9753;
                    { Let * v2072 = ((Let *) GC_OBJECT(Let,new_object_class(Language._Let)));
                      (v2072->var = v5256);
                      { Let * v9755 = v2072; 
                        OID  v9756;
                        { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                          (v2072->selector = Core._sup_sup);
                          { Call * v9757 = v2072; 
                            list * v9758;
                            { OID v_bag;
                              GC_ANY(v9758= list::empty(Kernel.emptySet));
                              { { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                                  (v2072->selector = Core._plus);
                                  (v2072->args = list::alloc(2,_oid_(v5254),_oid_(v5253)));
                                  add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                                  v_bag = _oid_(v2072);
                                  } 
                                GC_OID(v_bag);} 
                              ((list *) v9758)->addFast(v_bag);
                              ((list *) v9758)->addFast(1);} 
                            (v9757->args = v9758);} 
                          add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                          v9756 = _oid_(v2072);
                          } 
                        (v9755->value = v9756);} 
                      { Let * v9759 = v2072; 
                        OID  v9760;
                        { Let * v2072 = ((Let *) GC_OBJECT(Let,new_object_class(Language._Let)));
                          (v2072->var = v1353);
                          (v2072->value = _oid_(v5254));
                          { Let * v9761 = v2072; 
                            OID  v9762;
                            { Do * v2072 = ((Do *) GC_OBJECT(Do,new_object_class(Language._Do)));
                              { Do * v9763 = v2072; 
                                list * v10438;
                                { OID v_bag;
                                  GC_ANY(v10438= list::empty(Kernel.emptySet));
                                  { { Assign * v2072 = ((Assign *) GC_OBJECT(Assign,new_object_class(Language._Assign)));
                                      (v2072->var = _oid_(v5264));
                                      { Assign * v10439 = v2072; 
                                        OID  v10440;
                                        { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                                          (v2072->selector = Kernel.nth);
                                          (v2072->args = list::alloc(2,v5252,_oid_(v5256)));
                                          add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                                          v10440 = _oid_(v2072);
                                          } 
                                        (v10439->arg = v10440);} 
                                      add_I_property(Kernel.instances,Language._Assign,11,_oid_(v2072));
                                      v_bag = _oid_(v2072);
                                      } 
                                    GC_OID(v_bag);} 
                                  ((list *) v10438)->addFast(v_bag);
                                  { { If * v2072 = ((If *) GC_OBJECT(If,new_object_class(Language._If)));
                                      { If * v10441 = v2072; 
                                        OID  v10442;
                                        { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                                          (v2072->selector = Core._I_equal);
                                          (v2072->args = list::alloc(2,_oid_(v5256),_oid_(v5254)));
                                          add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                                          v10442 = _oid_(v2072);
                                          } 
                                        (v10441->test = v10442);} 
                                      { If * v10443 = v2072; 
                                        OID  v10444;
                                        { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                                          (v2072->selector = Kernel.nth_equal);
                                          { Call * v10445 = v2072; 
                                            list * v10446;
                                            { OID v_bag;
                                              GC_ANY(v10446= list::empty(Kernel.emptySet));
                                              ((list *) v10446)->addFast(v5252);
                                              ((list *) v10446)->addFast(_oid_(v5256));
                                              { { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                                                  (v2072->selector = Kernel.nth);
                                                  (v2072->args = list::alloc(2,v5252,_oid_(v5254)));
                                                  add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                                                  v_bag = _oid_(v2072);
                                                  } 
                                                GC_OID(v_bag);} 
                                              ((list *) v10446)->addFast(v_bag);} 
                                            (v10445->args = v10446);} 
                                          add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                                          v10444 = _oid_(v2072);
                                          } 
                                        (v10443->arg = v10444);} 
                                      add_I_property(Kernel.instances,Language._If,11,_oid_(v2072));
                                      (v2072->other = Kernel.cfalse);
                                      v_bag = _oid_(v2072);
                                      } 
                                    GC_OID(v_bag);} 
                                  ((list *) v10438)->addFast(v_bag);
                                  { { For * v2072 = ((For *) GC_OBJECT(For,new_object_class(Language._For)));
                                      (v2072->var = v5256);
                                      { Iteration * v10447 = v2072; 
                                        OID  v10466;
                                        { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                                          (v2072->selector = Kernel._dot_dot);
                                          { Call * v10467 = v2072; 
                                            list * v10468;
                                            { OID v_bag;
                                              GC_ANY(v10468= list::empty(Kernel.emptySet));
                                              { { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                                                  (v2072->selector = Core._plus);
                                                  (v2072->args = list::alloc(2,_oid_(v5254),1));
                                                  add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                                                  v_bag = _oid_(v2072);
                                                  } 
                                                GC_OID(v_bag);} 
                                              ((list *) v10468)->addFast(v_bag);
                                              ((list *) v10468)->addFast(_oid_(v5253));} 
                                            (v10467->args = v10468);} 
                                          add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                                          v10466 = _oid_(v2072);
                                          } 
                                        (v10447->set_arg = v10466);} 
                                      { Iteration * v10469 = v2072; 
                                        OID  v10470;
                                        { If * v2072 = ((If *) GC_OBJECT(If,new_object_class(Language._If)));
                                          { If * v10471 = v2072; 
                                            OID  v10472;
                                            { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                                              update_property(Kernel.selector,
                                                v2072,
                                                2,
                                                Kernel._object,
                                                v5246);
                                              { Call * v10473 = v2072; 
                                                list * v10474;
                                                { OID v_bag;
                                                  GC_ANY(v10474= list::empty(Kernel.emptySet));
                                                  { { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                                                      (v2072->selector = Kernel.nth);
                                                      (v2072->args = list::alloc(2,v5252,_oid_(v5256)));
                                                      add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                                                      v_bag = _oid_(v2072);
                                                      } 
                                                    GC_OID(v_bag);} 
                                                  ((list *) v10474)->addFast(v_bag);
                                                  ((list *) v10474)->addFast(_oid_(v5264));} 
                                                (v10473->args = v10474);} 
                                              add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                                              v10472 = _oid_(v2072);
                                              } 
                                            (v10471->test = v10472);} 
                                          { If * v10475 = v2072; 
                                            OID  v10497;
                                            { Do * v2072 = ((Do *) GC_OBJECT(Do,new_object_class(Language._Do)));
                                              { Do * v10498 = v2072; 
                                                list * v10499;
                                                { OID v_bag;
                                                  GC_ANY(v10499= list::empty(Kernel.emptySet));
                                                  { { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                                                      (v2072->selector = Kernel.nth_equal);
                                                      { Call * v10500 = v2072; 
                                                        list * v10501;
                                                        { OID v_bag;
                                                          GC_ANY(v10501= list::empty(Kernel.emptySet));
                                                          ((list *) v10501)->addFast(v5252);
                                                          ((list *) v10501)->addFast(_oid_(v5254));
                                                          { { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                                                              (v2072->selector = Kernel.nth);
                                                              (v2072->args = list::alloc(2,v5252,_oid_(v5256)));
                                                              add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                                                              v_bag = _oid_(v2072);
                                                              } 
                                                            GC_OID(v_bag);} 
                                                          ((list *) v10501)->addFast(v_bag);} 
                                                        (v10500->args = v10501);} 
                                                      add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                                                      v_bag = _oid_(v2072);
                                                      } 
                                                    GC_OID(v_bag);} 
                                                  ((list *) v10499)->addFast(v_bag);
                                                  { { Assign * v2072 = ((Assign *) GC_OBJECT(Assign,new_object_class(Language._Assign)));
                                                      (v2072->var = _oid_(v5254));
                                                      { Assign * v10502 = v2072; 
                                                        OID  v10504;
                                                        { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                                                          (v2072->selector = Core._plus);
                                                          (v2072->args = list::alloc(2,_oid_(v5254),1));
                                                          add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                                                          v10504 = _oid_(v2072);
                                                          } 
                                                        (v10502->arg = v10504);} 
                                                      add_I_property(Kernel.instances,Language._Assign,11,_oid_(v2072));
                                                      v_bag = _oid_(v2072);
                                                      } 
                                                    GC_OID(v_bag);} 
                                                  ((list *) v10499)->addFast(v_bag);
                                                  { { If * v2072 = ((If *) GC_OBJECT(If,new_object_class(Language._If)));
                                                      { If * v10505 = v2072; 
                                                        OID  v10506;
                                                        { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                                                          (v2072->selector = Kernel._sup);
                                                          (v2072->args = list::alloc(2,_oid_(v5256),_oid_(v5254)));
                                                          add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                                                          v10506 = _oid_(v2072);
                                                          } 
                                                        (v10505->test = v10506);} 
                                                      { If * v10507 = v2072; 
                                                        OID  v10528;
                                                        { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                                                          (v2072->selector = Kernel.nth_equal);
                                                          { Call * v10529 = v2072; 
                                                            list * v10530;
                                                            { OID v_bag;
                                                              GC_ANY(v10530= list::empty(Kernel.emptySet));
                                                              ((list *) v10530)->addFast(v5252);
                                                              ((list *) v10530)->addFast(_oid_(v5256));
                                                              { { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                                                                  (v2072->selector = Kernel.nth);
                                                                  (v2072->args = list::alloc(2,v5252,_oid_(v5254)));
                                                                  add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                                                                  v_bag = _oid_(v2072);
                                                                  } 
                                                                GC_OID(v_bag);} 
                                                              ((list *) v10530)->addFast(v_bag);} 
                                                            (v10529->args = v10530);} 
                                                          add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                                                          v10528 = _oid_(v2072);
                                                          } 
                                                        (v10507->arg = v10528);} 
                                                      add_I_property(Kernel.instances,Language._If,11,_oid_(v2072));
                                                      (v2072->other = Kernel.cfalse);
                                                      v_bag = _oid_(v2072);
                                                      } 
                                                    GC_OID(v_bag);} 
                                                  ((list *) v10499)->addFast(v_bag);} 
                                                (v10498->args = v10499);} 
                                              add_I_property(Kernel.instances,Language._Do,11,_oid_(v2072));
                                              v10497 = _oid_(v2072);
                                              } 
                                            (v10475->arg = v10497);} 
                                          add_I_property(Kernel.instances,Language._If,11,_oid_(v2072));
                                          (v2072->other = Kernel.cfalse);
                                          v10470 = _oid_(v2072);
                                          } 
                                        (v10469->arg = v10470);} 
                                      add_I_property(Kernel.instances,Language._For,11,_oid_(v2072));
                                      v_bag = _oid_(v2072);
                                      } 
                                    GC_OID(v_bag);} 
                                  ((list *) v10438)->addFast(v_bag);
                                  { { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                                      (v2072->selector = Kernel.nth_equal);
                                      (v2072->args = list::alloc(3,v5252,
                                        _oid_(v5254),
                                        _oid_(v5264)));
                                      add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                                      v_bag = _oid_(v2072);
                                      } 
                                    GC_OID(v_bag);} 
                                  ((list *) v10438)->addFast(v_bag);
                                  { { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                                      (v2072->selector = v9268->arg->selector);
                                      { Call * v10531 = v2072; 
                                        list * v10532;
                                        { OID v_bag;
                                          GC_ANY(v10532= list::empty(Kernel.emptySet));
                                          ((list *) v10532)->addFast(_oid_(v1353));
                                          { { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                                              (v2072->selector = Kernel._dash);
                                              (v2072->args = list::alloc(2,_oid_(v5254),1));
                                              add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                                              v_bag = _oid_(v2072);
                                              } 
                                            GC_OID(v_bag);} 
                                          ((list *) v10532)->addFast(v_bag);
                                          ((list *) v10532)->addFast(v5252);} 
                                        (v10531->args = v10532);} 
                                      add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                                      v_bag = _oid_(v2072);
                                      } 
                                    GC_OID(v_bag);} 
                                  ((list *) v10438)->addFast(v_bag);
                                  { { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                                      (v2072->selector = v9268->arg->selector);
                                      { Call * v10533 = v2072; 
                                        list * v10534;
                                        { OID v_bag;
                                          GC_ANY(v10534= list::empty(Kernel.emptySet));
                                          { { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                                              (v2072->selector = Core._plus);
                                              (v2072->args = list::alloc(2,_oid_(v5254),1));
                                              add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                                              v_bag = _oid_(v2072);
                                              } 
                                            GC_OID(v_bag);} 
                                          ((list *) v10534)->addFast(v_bag);
                                          ((list *) v10534)->addFast(_oid_(v5253));
                                          ((list *) v10534)->addFast(v5252);} 
                                        (v10533->args = v10534);} 
                                      add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                                      v_bag = _oid_(v2072);
                                      } 
                                    GC_OID(v_bag);} 
                                  ((list *) v10438)->addFast(v_bag);} 
                                (v9763->args = v10438);} 
                              add_I_property(Kernel.instances,Language._Do,11,_oid_(v2072));
                              v9762 = _oid_(v2072);
                              } 
                            (v9761->arg = v9762);} 
                          add_I_property(Kernel.instances,Language._Let,11,_oid_(v2072));
                          v9760 = _oid_(v2072);
                          } 
                        (v9759->arg = v9760);} 
                      add_I_property(Kernel.instances,Language._Let,11,_oid_(v2072));
                      v9753 = _oid_(v2072);
                      } 
                    (v9731->other = v9753);} 
                  add_I_property(Kernel.instances,Language._If,11,_oid_(v2072));
                  v9693 = _oid_(v2072);
                  } 
                (v9692->arg = v9693);} 
              add_I_property(Kernel.instances,Language._Let,11,_oid_(v2072));
              v9669 = _oid_(v2072);
              } 
            (v9668->arg = v9669);} 
          add_I_property(Kernel.instances,Language._If,11,_oid_(v2072));
          (v2072->other = Kernel.cfalse);
          v1343 = v2072;
          } 
        GC_OBJECT(If,v1343);} 
      Defmethod * v4533;
      { { Defmethod * v2072 = ((Defmethod *) GC_OBJECT(Defmethod,new_object_class(Language._Defmethod)));
          { Defmethod * v10535 = v2072; 
            Call * v10536;
            { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
              (v2072->selector = v9268->arg->selector);
              (v2072->args = list::alloc(3,_oid_(v5254),
                _oid_(v5253),
                v5252));
              add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
              v10536 = v2072;
              } 
            (v10535->arg = v10536);} 
          (v2072->inline_ask = Kernel.cfalse);
          (v2072->set_arg = v9268->set_arg);
          (v2072->body = _oid_(v1343));
          add_I_property(Kernel.instances,Language._Defmethod,11,_oid_(v2072));
          v4533 = v2072;
          } 
        GC_OBJECT(Defmethod,v4533);} 
      tformat_string("---- note: quick sort optimisation for ~S ---- \n",2,list::alloc(1,_oid_(v9268->arg->selector)));
      OPT_EVAL(_oid_(v4533));
      { Do * v2072 = ((Do *) GC_OBJECT(Do,new_object_class(Language._Do)));
        (v2072->args = list::alloc(2,GC_OID((*Optimize.c_code)(_oid_(v4532))),GC_OID((*Optimize.c_code)(_oid_(v4533)))));
        add_I_property(Kernel.instances,Language._Do,11,_oid_(v2072));
        Result = _oid_(v2072);
        } 
      } 
    GC_UNBIND; return (Result);} 
  } 

method * add_method_property2(property *v5256,list *v15471,ClaireType *v15645,int v15689,ClaireFunction *v15219,ClaireFunction *v15220)
{ return (add_method_property(v5256,
    v15471,
    v15645,
    v15689,
    _oid_(v15219)));} 

OID  add_method_I_method(method *v5253,list *v15471,OID v15645,OID v12924,ClaireFunction *v15287)
{ GC_BIND;
  { OID Result = 0;
    { Call_method * v13254;
      { { Call_method * v2072 = ((Call_method *) GC_OBJECT(Call_method,new_object_class(Language._Call_method)));
          (v2072->arg = ((method *) _at_property1(Kernel.add_method,Kernel._property)));
          (v2072->args = list::alloc(5,GC_OID((*Optimize.c_code)(_oid_(v5253->selector),
              _oid_(Kernel._property))),
            GC_OID((*Optimize.c_code)(_oid_(v15471),
              _oid_(Kernel._list))),
            GC_OID((*Optimize.c_code)(v15645,
              _oid_(Kernel._type))),
            v12924,
            _oid_(v15287)));
          add_I_property(Kernel.instances,Language._Call_method,11,_oid_(v2072));
          v13254 = v2072;
          } 
        GC_OBJECT(Call_method,v13254);} 
      if ((v5253->range == Kernel._float) || 
          ((v5253->domain->memq(_oid_(Kernel._float)) == CTRUE) || 
            (INHERIT(v5253->range->isa,Kernel._tuple))))
       GC_OBJECT(list,v13254->args)->addFast(_oid_(make_function_string(append_string(string_I_function(v15287),"_"))));
      Result = _oid_(v13254);
      } 
    GC_UNBIND; return (Result);} 
  } 

list * extract_status_new_any(OID v5264)
{ GC_BIND;
  { list *Result ;
    { OID  v5259 = CNULL;
      OID  v5246;
      if ((INHERIT(OWNER(v5264),Language._Call)) && (OBJECT(Call,v5264)->selector == Language.function_I))
       v5246 = v5264;
      else v5246 = CNULL;
        if (INHERIT(OWNER(v5264),Language._And))
       { OID  v5265 = (*(OBJECT(And,v5264)->args))[1];
        if ((INHERIT(OWNER(v5265),Language._Call)) && (OBJECT(Call,v5265)->selector == Language.function_I))
         { v5246= v5265;
          v5264= (*(OBJECT(And,v5264)->args))[2];
          } 
        } 
      else if (INHERIT(OWNER(v5264),Language._Call))
       { if (OBJECT(Call,v5264)->selector == Language.function_I)
         v5264= _oid_(Kernel.body);
        } 
      if (v5246 != CNULL)
       { v5264= _oid_(Kernel.body);
        if (length_bag(OBJECT(bag,(*Core.args)(v5246))) > 1)
         { ClaireHandler c_handle = ClaireHandler();
          if ERROR_IN 
          { { int  v12186;{ set * v9184;
                { set * v12630 = set::empty(Kernel.emptySet);
                  { OID gc_local;
                    ITERATE(v5261);
                    bag *v5261_support;
                    v5261_support = GC_OBJECT(list,cdr_list(GC_OBJECT(list,OBJECT(list,(*Core.args)(v5246)))));
                    for (START(v5261_support); NEXT(v5261);)
                    { GC_LOOP;
                      v12630->addFast(GC_OID(OPT_EVAL(v5261)));
                      GC_UNLOOP;} 
                    } 
                  v9184 = GC_OBJECT(set,v12630);
                  } 
                v12186 = integer_I_set(v9184);
                } 
              
              v5259=v12186;} 
            ClEnv->cHandle--;} 
          else if (belong_to(_oid_(ClEnv->exception_I),_oid_(Kernel._any)) == CTRUE)
          { c_handle.catchIt();{ warn_void();
              (Optimize.SHIT->value= _oid_(cdr_list(GC_OBJECT(list,OBJECT(list,(*Core.args)(v5246))))));
              tformat_string("wrong status ~S -> ~S [266]\n",2,GC_OBJECT(list,list::alloc(2,v5246,_oid_(set_I_bag(cdr_list(GC_OBJECT(list,OBJECT(list,(*Core.args)(v5246)))))))));
              v5259= 0;
              } 
            } 
          else PREVIOUS_HANDLER;} 
        else v5259= 0;
          v5246= _oid_(make_function_string(string_I_symbol(extract_symbol_any((*(OBJECT(bag,(*Core.args)(v5246))))[1]))));
        } 
      Result = list::alloc(3,v5259,
        v5246,
        v5264);
      } 
    GC_UNBIND; return (Result);} 
  } 

list * extract_signature_I_list(list *v5252)
{ GC_BIND;
  (Language.LDEF->value= _oid_(list::empty(Kernel._any)));
  { list *Result ;
    { int  v5254 = 0;
      list * v15405 = list::empty(Kernel._type);
      list * v15406;
      { { bag *v_list; OID v_val;
          OID v5263,CLcount;
          v_list = v5252;
           v15406 = v_list->clone(Kernel._any);
          for (CLcount= 1; CLcount <= v_list->length; CLcount++)
          { v5263 = (*(v_list))[CLcount];
            { OID  v5256 = GC_OID(extract_pattern_any(GC_OID(_oid_(OBJECT(Variable,v5263)->range)),list::alloc(1,v5254)));
              ++v5254;
              { { OID  v10145;
                  { if (INHERIT(OBJECT(Variable,v5263)->range->isa,Core._global_variable))
                     v10145 = _oid_(OBJECT(Variable,v5263)->range);
                    else v10145 = v5256;
                      GC_OID(v10145);} 
                  v15405 = v15405->addFast(v10145);
                  } 
                GC_OBJECT(list,v15405);} 
              (OBJECT(Variable,v5263)->range = type_I_any(v5256));
              v_val = v5256;
              } 
            
            (*((list *) v15406))[CLcount] = v_val;} 
          } 
        GC_OBJECT(list,v15406);} 
      Result = list::alloc(2,_oid_(v15405),_oid_(v15406));
      } 
    GC_UNBIND; return (Result);} 
  } 

ClaireBoolean * _equalsig_ask_list(list *v5264,list *v5265)
{ return (((tmatch_ask_list(v5264,v5265) == CTRUE) ? ((tmatch_ask_list(v5265,v5264) == CTRUE) ? CTRUE: CFALSE): CFALSE));} 

char * function_name_property_Optimize(property *v5256,list *v5252,OID v5264)
{ GC_BIND;
  { char *Result ;
    if (INHERIT(OWNER(v5264),Kernel._function))
     Result = string_I_function(OBJECT(ClaireFunction,v5264));
    else { int  v5254 = 0;
        int  v5253 = 0;
        module * v15488 = v5256->name->module_I;
        ClaireClass * v5243 = class_I_type(OBJECT(ClaireType,(*(v5252))[1]));
        char * v5258 = GC_STRING(append_string(GC_STRING(append_string(string_I_symbol(v5256->name),"_")),string_I_symbol(v5243->name)));
        if ((Optimize.compiler->naming == 0) && 
            (v5256 != Core.main))
         v5258= GC_STRING(append_string(GC_STRING(append_string(string_I_symbol(v15488->name),"_")),v5258));
        { ITERATE(v5258);
          for (START(v5256->restrictions); NEXT(v5258);)
          { if (v5243 == domain_I_restriction(OBJECT(restriction,v5258)))
             ++v5254;
            if (_equalsig_ask_list(v5252,OBJECT(restriction,v5258)->domain) == CTRUE)
             v5253= v5254;
            } 
          } 
        v5258= GC_STRING(((v5254 <= 1) ?
          v5258 :
          append_string(v5258,GC_STRING(string_I_integer (v5253))) ));
        Result = (((stable_ask_relation(v5256) == CTRUE) || 
            (v5256 == Core.main)) ?
          v5258 :
          append_string(GC_STRING(append_string(v5258,"_")),string_I_symbol(ClEnv->module_I->name)) );
        } 
      GC_UNBIND; return (Result);} 
  } 

OID  compile_lambda_string(char *v9268,lambda *v5252,OID v5253)
{ { OID Result = 0;
    { int  v5264 = Optimize.compiler->safety;
      lambda * v5265 = v5252;
      if (Kernel._method == OWNER(v5253))
       (Optimize.OPT->in_method = v5253);
      (Optimize.OPT->protection = CFALSE);
      (Optimize.OPT->allocation = CFALSE);
      if (Optimize.OPT->loop_index > 0)
       (Optimize.OPT->loop_index = 0);
      (Optimize.OPT->loop_gc = CFALSE);
      (Optimize.OPT->use_update = CFALSE);
      (Optimize.OPT->use_nth_equal = CFALSE);
      (Optimize.OPT->max_vars = 0);
      if (contain_ask_list(Optimize.OPT->unsure,v5253) == CTRUE)
       (Optimize.compiler->safety = 1);
      (*Optimize.make_c_function)(_oid_(v5252),
        _string_(v9268),
        v5253);
      (Optimize.OPT->in_method = CNULL);
      (Optimize.compiler->safety = v5264);
      Result = Kernel.ctrue;
      } 
    return (Result);} 
  } 

OID  c_code_Defarray_Optimize(Defarray *v9268)
{ GC_BIND;
  { OID Result = 0;
    { list * v5241 = GC_OBJECT(list,v9268->arg->args);
      OID  v13253 = get_symbol(extract_symbol_any((*(v5241))[1]));
      table * v13274;
      { ClaireObject *V_CC ;
        if (INHERIT(OWNER(v13253),Kernel._table))
         V_CC = OBJECT(table,v13253);
        else close_exception(((general_error *) (*Core._general_error)(_string_("[internal] the table ~S is unknown"),
            _oid_(list::alloc(1,(*(v5241))[1])))));
          v13274= (table *) V_CC;} 
      OID  v5259 = GC_OID((*Kernel.domain)(v13253));
      OID  v5245;
      { { list * v5252 = GC_OBJECT(list,cdr_list(v5241));
          OID  v5242 = GC_OID(lexical_build_any(GC_OID(v9268->body),v5252,0));
          { ClaireBoolean * g0444I;
            { OID  v13028;
              { ITERATE(v15763);
                v13028= _oid_(CFALSE);
                for (START(v5252); NEXT(v15763);)
                if (occurrence_any(v5242,OBJECT(Variable,v15763)) > 0)
                 { v13028 = Kernel.ctrue;
                  break;} 
                } 
              g0444I = boolean_I_any(v13028);
              } 
            
            if (g0444I == CTRUE) v5245 = _oid_(lambda_I_list(v5252,v5242));
              else v5245 = v9268->body;
            } 
          } 
        GC_OID(v5245);} 
      OID  v5244;
      { if (INHERIT(OWNER(v5245),Core._lambda))
         v5244 = CNULL;
        else v5244 = v9268->body;
          GC_OID(v5244);} 
      list * v1602;
      if (boolean_I_any(_oid_(OBJECT(ClaireRelation,v13253)->multivalued_ask)) == CTRUE)
       { OID v_bag;
        GC_ANY(v1602= list::empty(Kernel._any));
        { { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
            (v2072->selector = Kernel.put);
            (v2072->args = list::alloc(3,_oid_(Kernel.multivalued_ask),
              _oid_(v13274),
              GC_OID(_oid_(OBJECT(ClaireRelation,v13253)->multivalued_ask))));
            add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
            v_bag = _oid_(v2072);
            } 
          GC_OID(v_bag);} 
        ((list *) v1602)->addFast(v_bag);} 
      else v1602 = list::empty(Kernel._any);
        list * v1603;
      { OID v_bag;
        GC_ANY(v1603= list::empty(Kernel._any));
        { { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
            (v2072->selector = Kernel.put);
            (v2072->args = list::alloc(3,_oid_(Kernel.range),
              _oid_(v13274),
              GC_OID((*Kernel.range)(v13253))));
            add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
            v_bag = _oid_(v2072);
            } 
          GC_OID(v_bag);} 
        ((list *) v1603)->addFast(v_bag);
        { { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
            (v2072->selector = Kernel.put);
            (v2072->args = list::alloc(3,_oid_(Kernel.params),
              _oid_(v13274),
              GC_OID((*Kernel.params)(v13253))));
            add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
            v_bag = _oid_(v2072);
            } 
          GC_OID(v_bag);} 
        ((list *) v1603)->addFast(v_bag);
        { { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
            (v2072->selector = Kernel.put);
            (v2072->args = list::alloc(3,_oid_(Kernel.domain),
              _oid_(v13274),
              v5259));
            add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
            v_bag = _oid_(v2072);
            } 
          GC_OID(v_bag);} 
        ((list *) v1603)->addFast(v_bag);} 
      (OBJECT(Variable,(*(v5241))[2])->range = extract_type_any(GC_OID(_oid_(OBJECT(Variable,(*(v5241))[2])->range))));
      if (v5241->length == 2)
       { { { OID  v13989;
            { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
              (v2072->selector = Kernel.put);
              { Call * v10566 = v2072; 
                list * v10569;
                { OID v_bag;
                  GC_ANY(v10569= list::empty(Kernel.emptySet));
                  ((list *) v10569)->addFast(_oid_(Kernel.graph));
                  ((list *) v10569)->addFast(_oid_(v13274));
                  if (INHERIT(OBJECT(ClaireObject,v5259)->isa,Core._Interval))
                   { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                    (v2072->selector = Core.make_copy_list);
                    (v2072->args = list::alloc(2,size_Interval(OBJECT(Interval,v5259)),v5244));
                    add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                    v_bag = _oid_(v2072);
                    } 
                  else { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                      (v2072->selector = Kernel.make_list);
                      (v2072->args = list::alloc(2,29,CNULL));
                      add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                      v_bag = _oid_(v2072);
                      } 
                    ((list *) v10569)->addFast(v_bag);} 
                (v10566->args = v10569);} 
              add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
              v13989 = _oid_(v2072);
              } 
            v1603 = v1603->addFast(v13989);
            } 
          GC_OBJECT(list,v1603);} 
        { { OID  v488;
            if (INHERIT(OWNER(v5245),Core._lambda))
             { For * v2072 = ((For *) GC_OBJECT(For,new_object_class(Language._For)));
              store_object(v2072,
                2,
                Kernel._object,
                (*(v5241))[2],
                CFALSE);
              (v2072->set_arg = v5259);
              { Iteration * v10590 = v2072; 
                OID  v10591;
                { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                  (v2072->selector = Kernel.nth_equal);
                  (v2072->args = list::alloc(3,_oid_(v13274),
                    (*(v5241))[2],
                    GC_OID(OBJECT(lambda,v5245)->body)));
                  add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                  v10591 = _oid_(v2072);
                  } 
                (v10590->arg = v10591);} 
              add_I_property(Kernel.instances,Language._For,11,_oid_(v2072));
              v488 = _oid_(v2072);
              } 
            else { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                (v2072->selector = Kernel.put);
                (v2072->args = list::alloc(3,_oid_(Kernel.DEFAULT),
                  _oid_(v13274),
                  v5244));
                add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                v488 = _oid_(v2072);
                } 
              v1603 = v1603->addFast(v488);
            } 
          GC_OBJECT(list,v1603);} 
        } 
      else { ClaireType * v15623 = GC_OBJECT(ClaireType,extract_type_any(GC_OID(_oid_(OBJECT(Variable,(*(v5241))[3])->range))));
          (OBJECT(Variable,(*(v5241))[3])->range = v15623);
          { { OID  v7168;
              { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                (v2072->selector = Kernel.put);
                { Call * v10593 = v2072; 
                  list * v10594;
                  { OID v_bag;
                    GC_ANY(v10594= list::empty(Kernel.emptySet));
                    ((list *) v10594)->addFast(_oid_(Kernel.graph));
                    ((list *) v10594)->addFast(_oid_(v13274));
                    { { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                        (v2072->selector = Core.make_copy_list);
                        { Call * v10595 = v2072; 
                          list * v10596;
                          { OID v_bag;
                            GC_ANY(v10596= list::empty(Kernel.emptySet));
                            ((list *) v10596)->addFast(OBJECT(table,v13253)->graph->length);
                            { if ((*Kernel.params)(v13253) == _oid_(Kernel._any))
                               v_bag = CNULL;
                              else v_bag = (*Kernel.DEFAULT)(v13253);
                                GC_OID(v_bag);} 
                            ((list *) v10596)->addFast(v_bag);} 
                          (v10595->args = v10596);} 
                        add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                        v_bag = _oid_(v2072);
                        } 
                      GC_OID(v_bag);} 
                    ((list *) v10594)->addFast(v_bag);} 
                  (v10593->args = v10594);} 
                add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                v7168 = _oid_(v2072);
                } 
              v1603 = v1603->addFast(v7168);
              } 
            GC_OBJECT(list,v1603);} 
          { { OID  v11973;
              if (INHERIT(OWNER(v5245),Core._lambda))
               { For * v2072 = ((For *) GC_OBJECT(For,new_object_class(Language._For)));
                store_object(v2072,
                  2,
                  Kernel._object,
                  (*(v5241))[2],
                  CFALSE);
                (v2072->set_arg = (*(OBJECT(bag,v5259)))[1]);
                { Iteration * v10598 = v2072; 
                  OID  v10599;
                  { For * v2072 = ((For *) GC_OBJECT(For,new_object_class(Language._For)));
                    store_object(v2072,
                      2,
                      Kernel._object,
                      (*(v5241))[3],
                      CFALSE);
                    (v2072->set_arg = _oid_(v15623));
                    { Iteration * v10621 = v2072; 
                      OID  v10622;
                      { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                        (v2072->selector = Kernel.nth_equal);
                        (v2072->args = list::alloc(4,_oid_(v13274),
                          (*(v5241))[2],
                          (*(v5241))[3],
                          GC_OID(OBJECT(lambda,v5245)->body)));
                        add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                        v10622 = _oid_(v2072);
                        } 
                      (v10621->arg = v10622);} 
                    add_I_property(Kernel.instances,Language._For,11,_oid_(v2072));
                    v10599 = _oid_(v2072);
                    } 
                  (v10598->arg = v10599);} 
                add_I_property(Kernel.instances,Language._For,11,_oid_(v2072));
                v11973 = _oid_(v2072);
                } 
              else { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                  (v2072->selector = Kernel.put);
                  (v2072->args = list::alloc(3,_oid_(Kernel.DEFAULT),
                    _oid_(v13274),
                    v5244));
                  add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                  v11973 = _oid_(v2072);
                  } 
                v1603 = v1603->addFast(v11973);
              } 
            GC_OBJECT(list,v1603);} 
          } 
        GC_OBJECT(list,Optimize.OPT->objects)->addFast(v13253);
      (*Optimize.c_register)(v13253);
      { OID  v4191;
        { Do * v2072 = ((Do *) GC_OBJECT(Do,new_object_class(Language._Do)));
          { Do * v10624 = v2072; 
            list * v10625;
            { OID  v7074;
              { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                (v2072->selector = Optimize.object_I);
                (v2072->args = list::alloc(2,GC_OID((*Kernel.name)(v13253)),_oid_(Kernel._table)));
                add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                v7074 = _oid_(v2072);
                } 
              v10625 = cons_any(v7074,GC_OBJECT(list,add_star_list(v1602,v1603)));
              } 
            (v10624->args = v10625);} 
          add_I_property(Kernel.instances,Language._Do,11,_oid_(v2072));
          v4191 = _oid_(v2072);
          } 
        Result = (*Optimize.c_code)(v4191,
          _oid_(Kernel._any));
        } 
      } 
    GC_UNBIND; return (Result);} 
  } 

void  compute_if_write_inverse_relation2(ClaireRelation *v5226)
{ GC_BIND;
  { Variable * v5264;
    { { Variable * v2072 = ((Variable *) GC_OBJECT(Variable,new_object_class(Language._Variable)));
        (v2072->pname = symbol_I_string2("XX"));
        (v2072->range = v5226->domain);
        add_I_property(Kernel.instances,Language._Variable,11,_oid_(v2072));
        v5264 = v2072;
        } 
      GC_OBJECT(Variable,v5264);} 
    Variable * v5265;
    { { Variable * v2072 = ((Variable *) GC_OBJECT(Variable,new_object_class(Language._Variable)));
        (v2072->pname = symbol_I_string2("YY"));
        (v2072->range = ((multi_ask_any(_oid_(v5226)) == CTRUE) ?
          member_type(v5226->range) :
          v5226->range ));
        add_I_property(Kernel.instances,Language._Variable,11,_oid_(v2072));
        v5265 = v2072;
        } 
      GC_OBJECT(Variable,v5265);} 
    Variable * v5266;
    { { Variable * v2072 = ((Variable *) GC_OBJECT(Variable,new_object_class(Language._Variable)));
        (v2072->pname = symbol_I_string2("ZZ"));
        (v2072->range = v5226->range);
        add_I_property(Kernel.instances,Language._Variable,11,_oid_(v2072));
        v5266 = v2072;
        } 
      GC_OBJECT(Variable,v5266);} 
    list * v15405 = list::empty(Kernel._any);
    if (multi_ask_any(_oid_(v5226)) == CTRUE)
     { v15405= list::alloc(Kernel._any,1,GC_OID((INHERIT(v5226->isa,Kernel._property) ?  Produce_put_property2((property *) OBJECT(property,_oid_(v5226)),OBJECT(Variable,_oid_(v5264)),_oid_(v5265)) :   Produce_put_table2((table *) OBJECT(table,_oid_(v5226)),OBJECT(Variable,_oid_(v5264)),_oid_(v5265)))));
      if (((v5226->inverse == (NULL)) ? CTRUE : CFALSE) != CTRUE)
       v15405= GC_OBJECT(list,v15405->addFast(GC_OID((INHERIT(v5226->inverse->isa,Kernel._property) ?  Produce_put_property2((property *) OBJECT(property,_oid_(v5226->inverse)),OBJECT(Variable,_oid_(v5265)),_oid_(v5264)) :   Produce_put_table2((table *) OBJECT(table,_oid_(v5226->inverse)),OBJECT(Variable,_oid_(v5265)),_oid_(v5264))))));
      { ClaireRelation * v10627 = v5226; 
        OID  v10628;
        { lambda * v12256;{ OID  v10918;
            { If * v2072 = ((If *) GC_OBJECT(If,new_object_class(Language._If)));
              { If * v10652 = v2072; 
                OID  v10653;
                { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                  (v2072->selector = Core.NOT);
                  { Call * v10654 = v2072; 
                    list * v10655;
                    { OID v_bag;
                      GC_ANY(v10655= list::empty(Kernel.emptySet));
                      { { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                          (v2072->selector = Kernel._Z);
                          (v2072->args = list::alloc(2,_oid_(v5265),GC_OID(Produce_get_relation2(v5226,v5264))));
                          add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                          v_bag = _oid_(v2072);
                          } 
                        GC_OID(v_bag);} 
                      ((list *) v10655)->addFast(v_bag);} 
                    (v10654->args = v10655);} 
                  add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                  v10653 = _oid_(v2072);
                  } 
                (v10652->test = v10653);} 
              { If * v10656 = v2072; 
                OID  v10658;
                { Do * v2072 = ((Do *) GC_OBJECT(Do,new_object_class(Language._Do)));
                  (v2072->args = v15405);
                  add_I_property(Kernel.instances,Language._Do,11,_oid_(v2072));
                  v10658 = _oid_(v2072);
                  } 
                (v10656->arg = v10658);} 
              add_I_property(Kernel.instances,Language._If,11,_oid_(v2072));
              (v2072->other = Kernel.cfalse);
              v10918 = _oid_(v2072);
              } 
            v12256 = lambda_I_list(list::alloc(2,_oid_(v5264),_oid_(v5265)),v10918);
            } 
          
          v10628=_oid_(v12256);} 
        (v10627->if_write = v10628);} 
      } 
    else { v15405= list::alloc(Kernel._any,1,GC_OID((INHERIT(v5226->isa,Kernel._property) ?  Produce_put_property2((property *) OBJECT(property,_oid_(v5226)),OBJECT(Variable,_oid_(v5264)),_oid_(v5265)) :   Produce_put_table2((table *) OBJECT(table,_oid_(v5226)),OBJECT(Variable,_oid_(v5264)),_oid_(v5265)))));
        if (((v5226->inverse == (NULL)) ? CTRUE : CFALSE) != CTRUE)
         { { { OID  v5058;
              { If * v2072 = ((If *) GC_OBJECT(If,new_object_class(Language._If)));
                { If * v10660 = v2072; 
                  OID  v10661;
                  { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                    (v2072->selector = Core.known_ask);
                    (v2072->args = list::alloc(1,_oid_(v5266)));
                    add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                    v10661 = _oid_(v2072);
                    } 
                  (v10660->test = v10661);} 
                (v2072->arg = (INHERIT(v5226->inverse->isa,Kernel._property) ?  Produce_remove_property2((property *) OBJECT(property,_oid_(v5226->inverse)),OBJECT(Variable,_oid_(v5266)),_oid_(v5264)) :   Produce_remove_table2((table *) OBJECT(table,_oid_(v5226->inverse)),OBJECT(Variable,_oid_(v5266)),_oid_(v5264))));
                add_I_property(Kernel.instances,Language._If,11,_oid_(v2072));
                (v2072->other = Kernel.cfalse);
                v5058 = _oid_(v2072);
                } 
              v15405 = v15405->addFast(v5058);
              } 
            GC_OBJECT(list,v15405);} 
          v15405= GC_OBJECT(list,v15405->addFast(GC_OID((INHERIT(v5226->inverse->isa,Kernel._property) ?  Produce_put_property2((property *) OBJECT(property,_oid_(v5226->inverse)),OBJECT(Variable,_oid_(v5265)),_oid_(v5264)) :   Produce_put_table2((table *) OBJECT(table,_oid_(v5226->inverse)),OBJECT(Variable,_oid_(v5265)),_oid_(v5264))))));
          } 
        { ClaireRelation * v10662 = v5226; 
          OID  v10683;
          { lambda * v12311;{ OID  v14621;
              { Let * v2072 = ((Let *) GC_OBJECT(Let,new_object_class(Language._Let)));
                (v2072->var = v5266);
                (v2072->value = Produce_get_relation2(v5226,v5264));
                { Let * v10686 = v2072; 
                  OID  v10687;
                  { If * v2072 = ((If *) GC_OBJECT(If,new_object_class(Language._If)));
                    { If * v10688 = v2072; 
                      OID  v10689;
                      { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                        (v2072->selector = Core._I_equal);
                        (v2072->args = list::alloc(2,_oid_(v5265),_oid_(v5266)));
                        add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                        v10689 = _oid_(v2072);
                        } 
                      (v10688->test = v10689);} 
                    { If * v10690 = v2072; 
                      OID  v10691;
                      { Do * v2072 = ((Do *) GC_OBJECT(Do,new_object_class(Language._Do)));
                        (v2072->args = v15405);
                        add_I_property(Kernel.instances,Language._Do,11,_oid_(v2072));
                        v10691 = _oid_(v2072);
                        } 
                      (v10690->arg = v10691);} 
                    add_I_property(Kernel.instances,Language._If,11,_oid_(v2072));
                    (v2072->other = Kernel.cfalse);
                    v10687 = _oid_(v2072);
                    } 
                  (v10686->arg = v10687);} 
                add_I_property(Kernel.instances,Language._Let,11,_oid_(v2072));
                v14621 = _oid_(v2072);
                } 
              v12311 = lambda_I_list(list::alloc(2,_oid_(v5264),_oid_(v5265)),v14621);
              } 
            
            v10683=_oid_(v12311);} 
          (v10662->if_write = v10683);} 
        } 
      { char * v8882 = GC_STRING(append_string(string_I_symbol(v5226->name),"_write"));
      compile_lambda_string(v8882,GC_OBJECT(lambda,OBJECT(lambda,v5226->if_write)),_oid_(Kernel._void));
      } 
    } 
  GC_UNBIND;} 

OID  compute_set_write_relation2(ClaireRelation *v5226)
{ GC_BIND;
  { OID Result = 0;
    { Variable * v5264;
      { { Variable * v2072 = ((Variable *) GC_OBJECT(Variable,new_object_class(Language._Variable)));
          (v2072->pname = symbol_I_string2("XX"));
          (v2072->range = v5226->domain);
          add_I_property(Kernel.instances,Language._Variable,11,_oid_(v2072));
          v5264 = v2072;
          } 
        GC_OBJECT(Variable,v5264);} 
      Variable * v5265;
      { { Variable * v2072 = ((Variable *) GC_OBJECT(Variable,new_object_class(Language._Variable)));
          (v2072->pname = symbol_I_string2("YY"));
          (v2072->range = Kernel._bag);
          add_I_property(Kernel.instances,Language._Variable,11,_oid_(v2072));
          v5265 = v2072;
          } 
        GC_OBJECT(Variable,v5265);} 
      Variable * v5266;
      { { Variable * v2072 = ((Variable *) GC_OBJECT(Variable,new_object_class(Language._Variable)));
          (v2072->pname = symbol_I_string2("ZZ"));
          (v2072->range = member_type(v5226->range));
          add_I_property(Kernel.instances,Language._Variable,11,_oid_(v2072));
          v5266 = v2072;
          } 
        GC_OBJECT(Variable,v5266);} 
      list * v15405 = list::empty(Kernel._any);
      tformat_string("compute set_write for ~S \n",0,list::alloc(1,_oid_(v5226)));
      if (((v5226->inverse == (NULL)) ? CTRUE : CFALSE) != CTRUE)
       { { OID  v4964;
          { For * v2072 = ((For *) GC_OBJECT(For,new_object_class(Language._For)));
            (v2072->var = v5266);
            (v2072->set_arg = Produce_get_relation2(v5226,v5264));
            (v2072->arg = (INHERIT(v5226->inverse->isa,Kernel._property) ?  Produce_remove_property2((property *) OBJECT(property,_oid_(v5226->inverse)),OBJECT(Variable,_oid_(v5266)),_oid_(v5264)) :   Produce_remove_table2((table *) OBJECT(table,_oid_(v5226->inverse)),OBJECT(Variable,_oid_(v5266)),_oid_(v5264))));
            add_I_property(Kernel.instances,Language._For,11,_oid_(v2072));
            v4964 = _oid_(v2072);
            } 
          v15405 = v15405->addFast(v4964);
          } 
        GC_OBJECT(list,v15405);} 
      v15405= GC_OBJECT(list,v15405->addFast(GC_OID(Produce_erase_property2(((property *) v5226),v5264))));
      { { OID  v9732;
          { For * v2072 = ((For *) GC_OBJECT(For,new_object_class(Language._For)));
            (v2072->var = v5266);
            (v2072->set_arg = _oid_(v5265));
            (v2072->arg = (INHERIT(v5226->isa,Kernel._property) ?  Produce_put_property2((property *) OBJECT(property,_oid_(v5226)),OBJECT(Variable,_oid_(v5264)),_oid_(v5266)) :   Produce_put_table2((table *) OBJECT(table,_oid_(v5226)),OBJECT(Variable,_oid_(v5264)),_oid_(v5266))));
            add_I_property(Kernel.instances,Language._For,11,_oid_(v2072));
            v9732 = _oid_(v2072);
            } 
          v15405 = v15405->addFast(v9732);
          } 
        GC_OBJECT(list,v15405);} 
      { char * v8882 = GC_STRING(append_string(string_I_symbol(v5226->name),"_set_write"));
        { lambda * v10684;
          { { OID  v11644;
              { Do * v2072 = ((Do *) GC_OBJECT(Do,new_object_class(Language._Do)));
                (v2072->args = v15405);
                add_I_property(Kernel.instances,Language._Do,11,_oid_(v2072));
                v11644 = _oid_(v2072);
                } 
              v10684 = lambda_I_list(list::alloc(2,_oid_(v5264),_oid_(v5265)),v11644);
              } 
            GC_OBJECT(lambda,v10684);} 
          Result = compile_lambda_string(v8882,v10684,_oid_(Kernel._void));
          } 
        } 
      } 
    GC_UNBIND; return (Result);} 
  } 

OID  Produce_put_property2(property *v5258,Variable *v5264,OID v5265)
{ GC_BIND;
  { OID Result = 0;
    { list * v5252 = list::empty(Kernel._any);
      { OID gc_local;
        ITERATE(v15843);
        for (START(v5258->restrictions); NEXT(v15843);)
        { GC_LOOP;
          if ((Kernel._slot == OBJECT(ClaireObject,v15843)->isa) && 
              (boolean_I_any(_oid_(_exp_type(GC_OBJECT(ClaireType,ptype_type(v5264->range)),domain_I_restriction(OBJECT(restriction,v15843))))) == CTRUE))
           { list * v12605;
            { { OID v_bag;
                GC_ANY(v12605= list::empty(Kernel.emptySet));
                ((list *) v12605)->addFast(_oid_(domain_I_restriction(OBJECT(restriction,v15843))));
                if (boolean_I_any(_oid_(v5258->multivalued_ask)) == CTRUE)
                 { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                  (v2072->selector = Kernel.add_I);
                  { Call * v10718 = v2072; 
                    list * v10719;
                    { OID v_bag;
                      GC_ANY(v10719= list::empty(Kernel.emptySet));
                      { { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                          (v2072->selector = v5258);
                          { Call * v10720 = v2072; 
                            list * v10721;
                            { OID v_bag;
                              GC_ANY(v10721= list::empty(Kernel.emptySet));
                              { { Cast * v2072 = ((Cast *) GC_OBJECT(Cast,new_object_class(Language._Cast)));
                                  (v2072->arg = _oid_(v5264));
                                  (v2072->set_arg = domain_I_restriction(OBJECT(restriction,v15843)));
                                  add_I_property(Kernel.instances,Language._Cast,11,_oid_(v2072));
                                  v_bag = _oid_(v2072);
                                  } 
                                GC_OID(v_bag);} 
                              ((list *) v10721)->addFast(v_bag);} 
                            (v10720->args = v10721);} 
                          add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                          v_bag = _oid_(v2072);
                          } 
                        GC_OID(v_bag);} 
                      ((list *) v10719)->addFast(v_bag);
                      ((list *) v10719)->addFast(v5265);} 
                    (v10718->args = v10719);} 
                  add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                  v_bag = _oid_(v2072);
                  } 
                else { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                    (v2072->selector = Kernel.put);
                    { Call * v10722 = v2072; 
                      list * v10723;
                      { OID v_bag;
                        GC_ANY(v10723= list::empty(Kernel.emptySet));
                        ((list *) v10723)->addFast(_oid_(v5258));
                        { { Cast * v2072 = ((Cast *) GC_OBJECT(Cast,new_object_class(Language._Cast)));
                            (v2072->arg = _oid_(v5264));
                            (v2072->set_arg = domain_I_restriction(OBJECT(restriction,v15843)));
                            add_I_property(Kernel.instances,Language._Cast,11,_oid_(v2072));
                            v_bag = _oid_(v2072);
                            } 
                          GC_OID(v_bag);} 
                        ((list *) v10723)->addFast(v_bag);
                        ((list *) v10723)->addFast(v5265);} 
                      (v10722->args = v10723);} 
                    add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                    v_bag = _oid_(v2072);
                    } 
                  ((list *) v12605)->addFast(v_bag);} 
              GC_OBJECT(list,v12605);} 
            v5252 = add_star_list(v5252,v12605);
            } 
          GC_UNLOOP;} 
        } 
      if (v5252->length == 2)
       Result = (*(v5252))[2];
      else { Case * v2072 = ((Case *) GC_OBJECT(Case,new_object_class(Language._Case)));
          (v2072->var = _oid_(v5264));
          (v2072->args = v5252);
          add_I_property(Kernel.instances,Language._Case,11,_oid_(v2072));
          Result = _oid_(v2072);
          } 
        } 
    GC_UNBIND; return (Result);} 
  } 

OID  Produce_erase_property2(property *v5258,Variable *v5264)
{ GC_BIND;
  { OID Result = 0;
    { list * v5252 = list::empty(Kernel._any);
      bag * v13625;
      if (v5258->multivalued_ask == Kernel._list)
       v13625 = list::empty(Kernel._any);
      else v13625 = set::empty(Kernel._any);
        cast_I_bag(v13625,member_type(v5258->range));
      { OID gc_local;
        ITERATE(v15843);
        for (START(v5258->restrictions); NEXT(v15843);)
        { GC_LOOP;
          if ((Kernel._slot == OBJECT(ClaireObject,v15843)->isa) && 
              (boolean_I_any(_oid_(_exp_type(GC_OBJECT(ClaireType,ptype_type(v5264->range)),domain_I_restriction(OBJECT(restriction,v15843))))) == CTRUE))
           { list * v9764;
            { { OID v_bag;
                GC_ANY(v9764= list::empty(Kernel.emptySet));
                ((list *) v9764)->addFast(_oid_(domain_I_restriction(OBJECT(restriction,v15843))));
                { { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                    (v2072->selector = Kernel.put);
                    { Call * v11397 = v2072; 
                      list * v11398;
                      { OID v_bag;
                        GC_ANY(v11398= list::empty(Kernel.emptySet));
                        ((list *) v11398)->addFast(_oid_(v5258));
                        { { Cast * v2072 = ((Cast *) GC_OBJECT(Cast,new_object_class(Language._Cast)));
                            (v2072->arg = _oid_(v5264));
                            (v2072->set_arg = domain_I_restriction(OBJECT(restriction,v15843)));
                            add_I_property(Kernel.instances,Language._Cast,11,_oid_(v2072));
                            v_bag = _oid_(v2072);
                            } 
                          GC_OID(v_bag);} 
                        ((list *) v11398)->addFast(v_bag);
                        { if (boolean_I_any(_oid_(v5258->multivalued_ask)) == CTRUE)
                           v_bag = _oid_(v13625);
                          else v_bag = OBJECT(slot,v15843)->DEFAULT;
                            GC_OID(v_bag);} 
                        ((list *) v11398)->addFast(v_bag);} 
                      (v11397->args = v11398);} 
                    add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                    v_bag = _oid_(v2072);
                    } 
                  GC_OID(v_bag);} 
                ((list *) v9764)->addFast(v_bag);} 
              GC_OBJECT(list,v9764);} 
            v5252 = add_star_list(v5252,v9764);
            } 
          GC_UNLOOP;} 
        } 
      if (v5252->length == 2)
       Result = (*(v5252))[2];
      else { Case * v2072 = ((Case *) GC_OBJECT(Case,new_object_class(Language._Case)));
          (v2072->var = _oid_(v5264));
          (v2072->args = v5252);
          add_I_property(Kernel.instances,Language._Case,11,_oid_(v2072));
          Result = _oid_(v2072);
          } 
        } 
    GC_UNBIND; return (Result);} 
  } 

OID  Produce_put_table2(table *v5258,Variable *v5264,OID v5265)
{ GC_BIND;
  { OID Result = 0;
    { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
      (v2072->selector = Kernel.put);
      { Call * v11399 = v2072; 
        list * v11400;
        { OID v_bag;
          GC_ANY(v11400= list::empty(Kernel.emptySet));
          ((list *) v11400)->addFast(_oid_(v5258));
          ((list *) v11400)->addFast(_oid_(v5264));
          if (boolean_I_any(_oid_(v5258->multivalued_ask)) == CTRUE)
           { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
            (v2072->selector = Kernel.add);
            (v2072->args = list::alloc(2,_oid_(list::alloc(2,_oid_(Kernel.nth),_oid_(list::alloc(2,_oid_(v5258),_oid_(v5264))))),v5265));
            add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
            v_bag = _oid_(v2072);
            } 
          else v_bag = v5265;
            ((list *) v11400)->addFast(v_bag);} 
        (v11399->args = v11400);} 
      add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
      Result = _oid_(v2072);
      } 
    GC_UNBIND; return (Result);} 
  } 

OID  Produce_get_relation2(ClaireRelation *v5258,Variable *v5264)
{ GC_BIND;
  { OID Result = 0;
    if (INHERIT(v5258->isa,Kernel._table))
     { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
      (v2072->selector = Kernel.nth);
      (v2072->args = list::alloc(2,_oid_(v5258),_oid_(v5264)));
      add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
      Result = _oid_(v2072);
      } 
    else if (INHERIT(v5258->isa,Kernel._property))
     { list * v5252 = list::empty(Kernel._any);
      { OID gc_local;
        ITERATE(v15843);
        for (START(CLREAD(property,v5258,restrictions)); NEXT(v15843);)
        { GC_LOOP;
          if ((Kernel._slot == OBJECT(ClaireObject,v15843)->isa) && 
              (boolean_I_any(_oid_(_exp_type(GC_OBJECT(ClaireType,ptype_type(v5264->range)),domain_I_restriction(OBJECT(restriction,v15843))))) == CTRUE))
           { list * v14569;
            { { OID v_bag;
                GC_ANY(v14569= list::empty(Kernel.emptySet));
                ((list *) v14569)->addFast(_oid_(domain_I_restriction(OBJECT(restriction,v15843))));
                { { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                    (v2072->selector = ((property *) v5258));
                    { Call * v11402 = v2072; 
                      list * v11403;
                      { OID v_bag;
                        GC_ANY(v11403= list::empty(Kernel.emptySet));
                        { { Cast * v2072 = ((Cast *) GC_OBJECT(Cast,new_object_class(Language._Cast)));
                            (v2072->arg = _oid_(v5264));
                            (v2072->set_arg = domain_I_restriction(OBJECT(restriction,v15843)));
                            add_I_property(Kernel.instances,Language._Cast,11,_oid_(v2072));
                            v_bag = _oid_(v2072);
                            } 
                          GC_OID(v_bag);} 
                        ((list *) v11403)->addFast(v_bag);} 
                      (v11402->args = v11403);} 
                    add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                    v_bag = _oid_(v2072);
                    } 
                  GC_OID(v_bag);} 
                ((list *) v14569)->addFast(v_bag);} 
              GC_OBJECT(list,v14569);} 
            v5252 = add_star_list(v5252,v14569);
            } 
          GC_UNLOOP;} 
        } 
      if (v5252->length == 2)
       Result = (*(v5252))[2];
      else { Case * v2072 = ((Case *) GC_OBJECT(Case,new_object_class(Language._Case)));
          (v2072->var = _oid_(v5264));
          (v2072->args = v5252);
          add_I_property(Kernel.instances,Language._Case,11,_oid_(v2072));
          Result = _oid_(v2072);
          } 
        } 
    else Result = Kernel.cfalse;
      GC_UNBIND; return (Result);} 
  } 

OID  Produce_remove_property2(property *v5258,Variable *v5264,OID v5265)
{ GC_BIND;
  { OID Result = 0;
    { list * v5252 = list::empty(Kernel._any);
      { OID gc_local;
        ITERATE(v15843);
        for (START(v5258->restrictions); NEXT(v15843);)
        { GC_LOOP;
          if (Kernel._slot == OBJECT(ClaireObject,v15843)->isa)
           { list * v1068;
            { { OID v_bag;
                GC_ANY(v1068= list::empty(Kernel.emptySet));
                ((list *) v1068)->addFast(_oid_(domain_I_restriction(OBJECT(restriction,v15843))));
                if (boolean_I_any(_oid_(v5258->multivalued_ask)) == CTRUE)
                 { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                  (v2072->selector = Kernel._delete);
                  { Call * v11405 = v2072; 
                    list * v11427;
                    { OID v_bag;
                      GC_ANY(v11427= list::empty(Kernel.emptySet));
                      { { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                          (v2072->selector = v5258);
                          (v2072->args = list::alloc(1,_oid_(v5264)));
                          add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                          v_bag = _oid_(v2072);
                          } 
                        GC_OID(v_bag);} 
                      ((list *) v11427)->addFast(v_bag);
                      ((list *) v11427)->addFast(v5265);} 
                    (v11405->args = v11427);} 
                  add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                  v_bag = _oid_(v2072);
                  } 
                else { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                    (v2072->selector = Kernel.put);
                    (v2072->args = list::alloc(3,_oid_(v5258),
                      _oid_(v5264),
                      CNULL));
                    add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                    v_bag = _oid_(v2072);
                    } 
                  ((list *) v1068)->addFast(v_bag);} 
              GC_OBJECT(list,v1068);} 
            v5252 = add_star_list(v5252,v1068);
            } 
          GC_UNLOOP;} 
        } 
      if (v5252->length == 2)
       Result = (*(v5252))[2];
      else { Case * v2072 = ((Case *) GC_OBJECT(Case,new_object_class(Language._Case)));
          (v2072->var = _oid_(v5264));
          (v2072->args = v5252);
          add_I_property(Kernel.instances,Language._Case,11,_oid_(v2072));
          Result = _oid_(v2072);
          } 
        } 
    GC_UNBIND; return (Result);} 
  } 

OID  Produce_remove_table2(table *v5258,Variable *v5264,OID v5265)
{ GC_BIND;
  { OID Result = 0;
    { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
      (v2072->selector = Kernel.put);
      { Call * v11428 = v2072; 
        list * v11429;
        { OID v_bag;
          GC_ANY(v11429= list::empty(Kernel.emptySet));
          ((list *) v11429)->addFast(_oid_(v5258));
          ((list *) v11429)->addFast(_oid_(v5264));
          if (boolean_I_any(_oid_(v5258->multivalued_ask)) == CTRUE)
           { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
            (v2072->selector = Kernel._delete);
            (v2072->args = list::alloc(2,_oid_(list::alloc(2,_oid_(Kernel.nth),_oid_(list::alloc(2,_oid_(v5258),_oid_(v5264))))),v5265));
            add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
            v_bag = _oid_(v2072);
            } 
          else v_bag = CNULL;
            ((list *) v11429)->addFast(v_bag);} 
        (v11428->args = v11429);} 
      add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
      Result = _oid_(v2072);
      } 
    GC_UNBIND; return (Result);} 
  } 

void  Tighten_relation2(ClaireRelation *v5258)
{ GC_RESERVE(6);  // v3.0.55 optim !
  if (INHERIT(v5258->isa,Kernel._property))
   { ClaireType * v15115 = set::empty();
    ClaireType * v15129 = set::empty();
    { OID gc_local;
      ITERATE(v5259);
      for (START(CLREAD(property,v5258,restrictions)); NEXT(v5259);)
      { GC_LOOP;
        if (Kernel._slot == OBJECT(ClaireObject,v5259)->isa)
         { GC__ANY(v15115 = U_type(v15115,domain_I_restriction(OBJECT(restriction,v5259))), 3);
          GC__ANY(v15129 = U_type(v15129,GC_OBJECT(ClaireType,((multi_ask_any(_oid_(v5258)) == CTRUE) ?
            member_type(OBJECT(restriction,v5259)->range) :
            OBJECT(restriction,v5259)->range ))), 4);
          } 
        GC_UNLOOP;} 
      } 
    (v5258->open = 1);
    (v5258->domain = class_I_type(v15115));
    (v5258->range = ((v5258->multivalued_ask == Kernel._list) ?
      param_I_class(Kernel._list,class_I_type(v15129)) :
      ((v5258->multivalued_ask == Kernel._set) ?
        param_I_class(Kernel._set,class_I_type(v15129)) :
        v15129 ) ));
    ;} 
  GC_UNBIND;} 

void  lexical_num_any2(OID v9268,int v5254)
{ GC_BIND;
  if (INHERIT(OWNER(v9268),Language._Call))
   lexical_num_any2(GC_OID(_oid_(OBJECT(Call,v9268)->args)),v5254);
  else if (INHERIT(OWNER(v9268),Language._Instruction))
   { ClaireClass * v15607 = OBJECT(ClaireObject,v9268)->isa;
    if (contain_ask_set(Language._Instruction_with_var->descendents,_oid_(v15607)) == CTRUE)
     { put_property2(Kernel.index,GC_OBJECT(ClaireObject,OBJECT(ClaireObject,(*Language.var)(v9268))),v5254);
      ++v5254;
      if (v5254 > Language._starvariable_index_star->value)
       (Language._starvariable_index_star->value= v5254);
      } 
    { ITERATE(v5259);
      for (START(v15607->slots); NEXT(v5259);)
      lexical_num_any2(get_slot(OBJECT(slot,v5259),OBJECT(ClaireObject,v9268)),v5254);
      } 
    } 
  else if (INHERIT(OWNER(v9268),Kernel._bag))
   { ITERATE(v5264);
    for (START(OBJECT(bag,v9268)); NEXT(v5264);)
    lexical_num_any2(v5264,v5254);
    } 
  else ;GC_UNBIND;} 

ClaireType * c_type_Defrule2_Optimize(Defrule *v9268)
{ return (Kernel._any);} 

OID  c_code_Defrule_Optimize(Defrule *v9268,ClaireClass *v5259)
{ GC_RESERVE(11);  // v3.0.55 optim !
  { OID Result = 0;
    { OID  v15659 = get_symbol(v9268->ident);
      list * v5252 = list::empty(Kernel._any);
      tformat_string("compile a rule ~S \n",0,list::alloc(1,v15659));
      { OID gc_local;
        ITERATE(v5258);
        bag *v5258_support;
        v5258_support = OBJECT(bag,nth_table1(Language.relations,v15659));
        for (START(v5258_support); NEXT(v5258);)
        if (eventMethod_ask_relation2(OBJECT(ClaireRelation,v5258)) != CTRUE)
         Tighten_relation2(OBJECT(ClaireRelation,v5258));
        } 
      { OID gc_local;
        ITERATE(v5258);
        bag *v5258_support;
        v5258_support = OBJECT(bag,nth_table1(Language.relations,v15659));
        for (START(v5258_support); NEXT(v5258);)
        { GC_LOOP;
          { if ((*Kernel.open)(v5258) < 2)
             { { OID  v9671;
                { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                  (v2072->selector = Kernel.FINAL);
                  (v2072->args = list::alloc(1,v5258));
                  add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                  v9671 = _oid_(v2072);
                  } 
                v5252 = v5252->addFast(v9671);
                } 
               GC__ANY(v5252, 5);} 
            compile_if_write_relation(OBJECT(ClaireRelation,v5258));
            { OID  v8882 = GC_OID((*Kernel._7_plus)(GC_OID((*Kernel.name)(v5258)),
                _string_("_write")));
              char * v5259 = string_I_symbol(OBJECT(symbol,v8882));
              OID  v15454 = GC_OID((*Kernel.if_write)(v5258));
              compile_lambda_string(v5259,OBJECT(lambda,v15454),_oid_(Kernel._void));
              { OID  v10631;
                { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                  (v2072->selector = Kernel.put);
                  (v2072->args = list::alloc(3,Optimize.if_write->value,
                    v5258,
                    _oid_(make_function_string(v5259))));
                  add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                  v10631 = _oid_(v2072);
                  } 
                v5252->addFast(v10631);
                } 
              } 
            } 
          GC_UNLOOP;} 
        } 
      { OID gc_local;
        ITERATE(v5258);
        bag *v5258_support;
        v5258_support = OBJECT(bag,nth_table1(Language.relations,v15659));
        for (START(v5258_support); NEXT(v5258);)
        { GC_LOOP;
          if (eventMethod_ask_relation2(OBJECT(ClaireRelation,v5258)) == CTRUE)
           v5252= v5252->addFast(GC_OID(compileEventMethod_property(OBJECT(property,v5258))));
          GC_UNLOOP;} 
        } 
      { OID  v11592;
        { Do * v2072 = ((Do *) GC_OBJECT(Do,new_object_class(Language._Do)));
          (v2072->args = v5252);
          add_I_property(Kernel.instances,Language._Do,11,_oid_(v2072));
          v11592 = _oid_(v2072);
          } 
        Result = (*Optimize.c_code)(v11592,
          _oid_(v5259));
        } 
      } 
    GC_UNBIND; return (Result);} 
  } 

void  compile_if_write_relation(ClaireRelation *v5226)
{ GC_BIND;
  { OID  v5252 = nth_table1(Language.demons,_oid_(v5226));
    list * v13347 = GC_OBJECT(list,OBJECT(Language_demon,(*(OBJECT(bag,v5252)))[1])->formula->vars);
    list * v15405 = list::alloc(Kernel._any,1,GC_OID((*Optimize.Produce_put)(_oid_(v5226),
      (*(v13347))[1],
      (*(v13347))[2])));
    list * v15406;
    { { bag *v_list; OID v_val;
        OID v5264,CLcount;
        v_list = OBJECT(bag,v5252);
         v15406 = v_list->clone(Kernel._any);
        for (CLcount= 1; CLcount <= v_list->length; CLcount++)
        { v5264 = (*(v_list))[CLcount];
          v_val = substitution_any(GC_OID(substitution_any(GC_OID(substitution_any(GC_OID(OBJECT(Language_demon,v5264)->formula->body),GC_OBJECT(Variable,OBJECT(Variable,(*(OBJECT(Language_demon,v5264)->formula->vars))[3])),(*(v13347))[3])),GC_OBJECT(Variable,OBJECT(Variable,(*(OBJECT(Language_demon,v5264)->formula->vars))[1])),(*(v13347))[1])),GC_OBJECT(Variable,OBJECT(Variable,(*(OBJECT(Language_demon,v5264)->formula->vars))[2])),(*(v13347))[2]);
          
          (*((list *) v15406))[CLcount] = v_val;} 
        } 
      GC_OBJECT(list,v15406);} 
    put_property2(Kernel.range,OBJECT(ClaireObject,(*(v13347))[1]),_oid_(v5226->domain));
    put_property2(Kernel.range,OBJECT(ClaireObject,(*(v13347))[2]),_oid_(v5226->range));
    { OID gc_local;
      ITERATE(v5263);
      for (START(v13347); NEXT(v5263);)
      { GC_LOOP;
        put_property2(Kernel.range,OBJECT(ClaireObject,v5263),_oid_(class_I_type(GC_OBJECT(ClaireType,OBJECT(ClaireType,(*Kernel.range)(v5263))))));
        GC_UNLOOP;} 
      } 
    if ((INHERIT(OWNER((*(v15406))[1]),Language._If)) && 
        (eventMethod_ask_relation2(v5226) != CTRUE))
     { if (INHERIT(OWNER(OBJECT(If,(*(v15406))[1])->test),Language._And))
       { If * v11433 = OBJECT(If,(*(v15406))[1]); 
        OID  v11434;
        { And * v2072 = ((And *) GC_OBJECT(And,new_object_class(Language._And)));
          (v2072->args = cdr_list(GC_OBJECT(list,OBJECT(list,(*Core.args)(GC_OID(OBJECT(If,(*(v15406))[1])->test))))));
          add_I_property(Kernel.instances,Language._And,11,_oid_(v2072));
          v11434 = _oid_(v2072);
          } 
        (v11433->test = v11434);} 
      else ((*(v15406))[1]=OBJECT(If,(*(v15406))[1])->arg);
        } 
    if (((v5226->inverse == (NULL)) ? CTRUE : CFALSE) != CTRUE)
     { if (multi_ask_any(_oid_(v5226)) != CTRUE)
       v15405= v15405->addFast(GC_OID((*Optimize.Produce_remove)(_oid_(v5226->inverse),
        (*(v13347))[3],
        (*(v13347))[1])));
      v15405= v15405->addFast(GC_OID((*Optimize.Produce_put)(_oid_(v5226->inverse),
        (*(v13347))[2],
        (*(v13347))[1])));
      } 
    { ClaireRelation * v11435 = v5226; 
      OID  v11436;
      { lambda * v13085;{ OID  v4771;
          if (eventMethod_ask_relation2(v5226) == CTRUE)
           { Do * v2072 = ((Do *) GC_OBJECT(Do,new_object_class(Language._Do)));
            (v2072->args = v15406);
            add_I_property(Kernel.instances,Language._Do,11,_oid_(v2072));
            v4771 = _oid_(v2072);
            } 
          else if (multi_ask_any(_oid_(v5226)) == CTRUE)
           { If * v2072 = ((If *) GC_OBJECT(If,new_object_class(Language._If)));
            { If * v11460 = v2072; 
              OID  v11461;
              { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                (v2072->selector = Core.NOT);
                { Call * v11462 = v2072; 
                  list * v11463;
                  { OID v_bag;
                    GC_ANY(v11463= list::empty(Kernel.emptySet));
                    { { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                        (v2072->selector = Kernel._Z);
                        (v2072->args = list::alloc(2,(*(v13347))[2],GC_OID(_oid_(readCall_relation(v5226,(*(v13347))[1])))));
                        add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                        v_bag = _oid_(v2072);
                        } 
                      GC_OID(v_bag);} 
                    ((list *) v11463)->addFast(v_bag);} 
                  (v11462->args = v11463);} 
                add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                v11461 = _oid_(v2072);
                } 
              (v11460->test = v11461);} 
            { If * v11464 = v2072; 
              OID  v11465;
              { Do * v2072 = ((Do *) GC_OBJECT(Do,new_object_class(Language._Do)));
                (v2072->args = append_list(v15405,v15406));
                add_I_property(Kernel.instances,Language._Do,11,_oid_(v2072));
                v11465 = _oid_(v2072);
                } 
              (v11464->arg = v11465);} 
            add_I_property(Kernel.instances,Language._If,11,_oid_(v2072));
            (v2072->other = Kernel.cfalse);
            v4771 = _oid_(v2072);
            } 
          else { Let * v2072 = ((Let *) GC_OBJECT(Let,new_object_class(Language._Let)));
              store_object(v2072,
                2,
                Kernel._object,
                (*(v13347))[3],
                CFALSE);
              (v2072->value = _oid_(readCall_relation(v5226,(*(v13347))[1])));
              { Let * v11466 = v2072; 
                OID  v11467;
                { If * v2072 = ((If *) GC_OBJECT(If,new_object_class(Language._If)));
                  { If * v11489 = v2072; 
                    OID  v11490;
                    { Call * v2072 = ((Call *) GC_OBJECT(Call,new_object_class(Language._Call)));
                      (v2072->selector = Core._I_equal);
                      (v2072->args = list::alloc(2,(*(v13347))[2],(*(v13347))[3]));
                      add_I_property(Kernel.instances,Language._Call,11,_oid_(v2072));
                      v11490 = _oid_(v2072);
                      } 
                    (v11489->test = v11490);} 
                  { If * v11491 = v2072; 
                    OID  v11492;
                    { Do * v2072 = ((Do *) GC_OBJECT(Do,new_object_class(Language._Do)));
                      (v2072->args = append_list(v15405,v15406));
                      add_I_property(Kernel.instances,Language._Do,11,_oid_(v2072));
                      v11492 = _oid_(v2072);
                      } 
                    (v11491->arg = v11492);} 
                  add_I_property(Kernel.instances,Language._If,11,_oid_(v2072));
                  (v2072->other = Kernel.cfalse);
                  v11467 = _oid_(v2072);
                  } 
                (v11466->arg = v11467);} 
              add_I_property(Kernel.instances,Language._Let,11,_oid_(v2072));
              v4771 = _oid_(v2072);
              } 
            v13085 = lambda_I_list(GC_OBJECT(list,list::alloc(2,(*(v13347))[1],(*(v13347))[2])),v4771);
          } 
        
        v11436=_oid_(v13085);} 
      (v11435->if_write = v11436);} 
    } 
  GC_UNBIND;} 

OID  compileEventMethod_property(property *v5256)
{ GC_BIND;
  { OID Result = 0;
    { method * v5253 = OBJECT(method,(*(v5256->restrictions))[1]);
      char * v15515 = GC_STRING(append_string(string_I_symbol(v5256->name),"_write"));
      Result = add_method_I_method(v5253,
        list::alloc(2,_oid_(v5256->domain),_oid_(v5256->range)),
        _oid_(Kernel._void),
        0,
        make_function_string(v15515));
      } 
    GC_UNBIND; return (Result);} 
  } 

