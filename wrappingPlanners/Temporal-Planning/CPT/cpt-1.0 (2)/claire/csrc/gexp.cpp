/***** CLAIRE Compilation of file c:\claire\v3.3\src\compile\gexp.cl 
         [version 3.3.4 / safety 5] Sat Oct 16 06:53:38 2004 *****/

#include <claire.h>
#include <Kernel.h>
#include <Core.h>
#include <Language.h>
#include <Reader.h>
#include <Optimize.h>
#include <Generate.h>
ClaireBoolean * c_func_any(OID v1140)
{ GC_RESERVE(1);  // HOHO v3.0.55 optim !
  { ClaireBoolean *Result ;
    if (INHERIT(OWNER(v1140),Kernel._bag))
     { OID  v4712;
      { OID gc_local;
        ITERATE(v7248);
        v4712= _oid_(CFALSE);
        for (START(OBJECT(bag,v1140)); NEXT(v7248);)
        if (c_func_any(v7248) != CTRUE)
         { v4712 = Kernel.ctrue;
          break;} 
        else ;} 
      Result = not_any(v4712);
      } 
    else if (INHERIT(OWNER(v1140),Language._Construct))
     { if (((INHERIT(OBJECT(ClaireObject,v1140)->isa,Language._Set)) || 
            (INHERIT(OBJECT(ClaireObject,v1140)->isa,Language._List))) || 
          (INHERIT(OBJECT(ClaireObject,v1140)->isa,Language._Tuple)))
       { ClaireBoolean *v_and;
        { v_and = ((OBJECT(Construct,v1140)->args->length < 15) ? CTRUE : CFALSE);
          if (v_and == CFALSE) Result =CFALSE; 
          else { { OID  v9470;
              { ClaireBoolean * v8250;{ OID  v11392;
                  { set * v8252;{ set * v4832 = set::empty(Kernel.emptySet);
                      { OID gc_local;
                        ITERATE(v7248);
                        bag *v7248_support;
                        v7248_support = GC_OBJECT(list,OBJECT(Construct,v1140)->args);
                        for (START(v7248_support); NEXT(v7248);)
                        if (c_func_any(v7248) != CTRUE)
                         v4832->addFast(v7248);
                        } 
                      v8252 = GC_OBJECT(set,v4832);
                      } 
                    
                    v11392=_oid_(v8252);} 
                  v8250 = boolean_I_any(v11392);
                  } 
                
                v9470=_oid_(v8250);} 
              v_and = ((v9470 != Kernel.ctrue) ? CTRUE : CFALSE);
              } 
            if (v_and == CFALSE) Result =CFALSE; 
            else Result = CTRUE;} 
          } 
        } 
      else Result = CFALSE;
        } 
    else if (INHERIT(OWNER(v1140),Language._If))
     { ClaireBoolean *v_and;
      { v_and = c_func_any(GC_OID(OBJECT(If,v1140)->test));
        if (v_and == CFALSE) Result =CFALSE; 
        else { v_and = c_func_any(GC_OID(OBJECT(If,v1140)->arg));
          if (v_and == CFALSE) Result =CFALSE; 
          else { v_and = c_func_any(GC_OID(OBJECT(If,v1140)->other));
            if (v_and == CFALSE) Result =CFALSE; 
            else { v_and = stupid_t_any2(GC_OID(OBJECT(If,v1140)->arg),GC_OID(OBJECT(If,v1140)->other));
              if (v_and == CFALSE) Result =CFALSE; 
              else { { int  v7239 = 4;
                  OID  v7248 = GC_OID(OBJECT(If,v1140)->other);
                  { OID v15730;{ OID gc_local;
                      v15730= _oid_(CFALSE);
                      while ((v7239 > 0))
                      { GC_LOOP;
                        if (INHERIT(OWNER(v7248),Language._If))
                         { GC__OID(v7248 = OBJECT(If,v7248)->other, 1);
                          v7239= (v7239-1);
                          } 
                        else { v15730 = Kernel.ctrue;
                            break;} 
                          GC_UNLOOP;} 
                      } 
                    
                    v_and=OBJECT(ClaireBoolean,v15730);} 
                  } 
                if (v_and == CFALSE) Result =CFALSE; 
                else Result = CTRUE;} 
              } 
            } 
          } 
        } 
      } 
    else if (INHERIT(OWNER(v1140),Language._Assign))
     Result = c_func_any(GC_OID(OBJECT(Assign,v1140)->arg));
    else if (INHERIT(OWNER(v1140),Language._Gassign))
     Result = c_func_any(GC_OID(OBJECT(Gassign,v1140)->arg));
    else if (INHERIT(OWNER(v1140),Optimize._to_protect))
     Result = c_func_any(GC_OID(OBJECT(Compile_to_protect,v1140)->arg));
    else if (INHERIT(OWNER(v1140),Language._And))
     Result = c_func_any(GC_OID(_oid_(OBJECT(And,v1140)->args)));
    else if (INHERIT(OWNER(v1140),Language._Or))
     Result = c_func_any(GC_OID(_oid_(OBJECT(Or,v1140)->args)));
    else if (INHERIT(OWNER(v1140),Language._Call))
     Result = c_func_any(GC_OID(_oid_(OBJECT(Call,v1140)->args)));
    else if (INHERIT(OWNER(v1140),Language._Call_method))
     Result = ((c_func_any(GC_OID(_oid_(OBJECT(Call_method,v1140)->args))) == CTRUE) ? ((_oid_(OBJECT(Call_method,v1140)->arg) != Generate._starclose_exception_star->value) ? CTRUE: CFALSE): CFALSE);
    else if (INHERIT(OWNER(v1140),Language._Call_slot))
     Result = c_func_any(GC_OID(OBJECT(Call_slot,v1140)->arg));
    else if (INHERIT(OWNER(v1140),Language._Call_table))
     Result = c_func_any(GC_OID(OBJECT(Call_table,v1140)->arg));
    else if (INHERIT(OWNER(v1140),Language._Call_array))
     Result = c_func_any(GC_OID(OBJECT(Call_array,v1140)->arg));
    else Result = ((INHERIT(OWNER(v1140),Language._Update)) ?
      ((c_func_any(GC_OID(OBJECT(Update,v1140)->value)) == CTRUE) ? ((c_func_any(GC_OID(OBJECT(Update,v1140)->arg)) == CTRUE) ? CTRUE: CFALSE): CFALSE) :
      ((INHERIT(OWNER(v1140),Language._Cast)) ?
        c_func_any(GC_OID(OBJECT(Cast,v1140)->arg)) :
        ((INHERIT(OWNER(v1140),Optimize._to_C)) ?
          c_func_any(GC_OID(OBJECT(Compile_to_C,v1140)->arg)) :
          ((INHERIT(OWNER(v1140),Optimize._to_CL)) ?
            c_func_any(GC_OID(OBJECT(Compile_to_CL,v1140)->arg)) :
            ((INHERIT(OWNER(v1140),Kernel._thing)) ? CTRUE : 
            ((INHERIT(OWNER(v1140),Kernel._integer)) ? CTRUE : 
            ((Kernel._string == OWNER(v1140)) ? CTRUE : 
            ((INHERIT(OWNER(v1140),Kernel._char)) ? CTRUE : 
            ((Kernel._float == OWNER(v1140)) ? CTRUE : 
            ((INHERIT(OWNER(v1140),Language._Variable)) ? CTRUE : 
            ((INHERIT(OWNER(v1140),Core._global_variable)) ? CTRUE : 
            ((INHERIT(OWNER(v1140),Kernel._function)) ? CTRUE : 
            ((INHERIT(OWNER(v1140),Kernel._symbol)) ? CTRUE : 
            ((v1140 == CNULL) ? CTRUE : 
            ((Kernel._boolean == OWNER(v1140)) ? CTRUE : 
            ((INHERIT(OWNER(v1140),Kernel._class)) ? CTRUE : 
            ((INHERIT(OWNER(v1140),Kernel._environment)) ? CTRUE : 
            CFALSE))))))))))))) ) ) ) );
    GC_UNBIND; return (Result);} 
  } 

void  expression_thing(thing *v1140,OID v15308)
{ (*Generate.produce)(Generate.PRODUCER->value,
    _oid_(v1140));
  } 

void  expression_integer(int v1140,OID v15308)
{ princ_integer(v1140);
  } 

void  expression_any(OID v1140,OID v15308)
{ (*Generate.produce)(Generate.PRODUCER->value,
    v1140);
  } 

void  expression_string(char *v1140,OID v15308)
{ (*Generate.produce)(Generate.PRODUCER->value,
    _string_(v1140));
  } 

void  expression_float_(OID v6626,OID v6627)
{ expression_float(float_v(v6626),v6627);} 

void  expression_float(double v1140,OID v15308)
{ princ_float(v1140);
  } 

void  expression_boolean(ClaireBoolean *v1140,OID v15308)
{ (*Generate.produce)(Generate.PRODUCER->value,
    _oid_(v1140));
  } 

void  expression_environment(ClaireEnvironment *v1140,OID v15308)
{ (*Generate.produce)(Generate.PRODUCER->value,
    _oid_(v1140));
  } 

void  expression_Variable(Variable *v1140,OID v15308)
{ (*Language.ident)(Generate.PRODUCER->value,
    _oid_(v1140));
  } 

void  expression_global_variable(global_variable *v1140,OID v15308)
{ (*Generate.produce)(Generate.PRODUCER->value,
    _oid_(v1140));
  } 

void  expression_Set(Set *v1140,OID v15308)
{ GC_BIND;
  { OID  v15236;
    { if (((v1140->of == (NULL)) ? CTRUE : CFALSE) != CTRUE)
       v15236 = _oid_(v1140->of);
      else v15236 = _oid_(Kernel._void);
        GC_OID(v15236);} 
    (*Generate.bag_expression)(Generate.PRODUCER->value,
      _oid_(Kernel._set),
      GC_OID(_oid_(v1140->args)),
      v15236,
      v15308);
    } 
  GC_UNBIND;} 

void  expression_set2(set *v1140,OID v15308)
{ if ((v1140->length == 0) && 
      (equal(_oid_(of_bag(v1140)),_oid_(Kernel.emptySet)) == CTRUE))
   princ_string("Kernel.emptySet");
  else (*Generate.bag_expression)(Generate.PRODUCER->value,
      _oid_(Kernel._set),
      _oid_(v1140),
      _oid_(of_bag(v1140)),
      v15308);
    } 

void  expression_Tuple(Tuple *v1140,OID v15308)
{ GC_BIND;
  (*Generate.bag_expression)(Generate.PRODUCER->value,
    _oid_(Kernel._tuple),
    GC_OID(_oid_(v1140->args)),
    _oid_(Kernel._void),
    v15308);
  GC_UNBIND;} 

void  expression_tuple(tuple *v1140,OID v15308)
{ (*Generate.bag_expression)(Generate.PRODUCER->value,
    _oid_(Kernel._tuple),
    _oid_(v1140),
    _oid_(Kernel._void),
    v15308);
  } 

void  expression_List(List *v1140,OID v15308)
{ GC_BIND;
  { OID  v16197;
    { if (((v1140->of == (NULL)) ? CTRUE : CFALSE) != CTRUE)
       v16197 = _oid_(v1140->of);
      else v16197 = _oid_(Kernel._void);
        GC_OID(v16197);} 
    (*Generate.bag_expression)(Generate.PRODUCER->value,
      _oid_(Kernel._list),
      GC_OID(_oid_(v1140->args)),
      v16197,
      v15308);
    } 
  GC_UNBIND;} 

void  expression_list(list *v1140,OID v15308)
{ if ((v1140->length == 0) && 
      (equal(_oid_(of_bag(v1140)),_oid_(Kernel.emptySet)) == CTRUE))
   princ_string("Kernel.nil");
  else (*Generate.bag_expression)(Generate.PRODUCER->value,
      _oid_(Kernel._list),
      _oid_(v1140),
      _oid_(of_bag(v1140)),
      v15308);
    } 

void  expression_Call2(Call *v1140,OID v15308)
{ (*Generate.inline_exp)(Generate.PRODUCER->value,
    _oid_(v1140),
    v15308);
  } 

void  expression_Call_method12(Call_method1 *v1140,OID v15308)
{ (*Generate.inline_exp)(Generate.PRODUCER->value,
    _oid_(v1140),
    v15308);
  } 

void  expression_Call_method22(Call_method2 *v1140,OID v15308)
{ (*Generate.inline_exp)(Generate.PRODUCER->value,
    _oid_(v1140),
    v15308);
  } 

void  expression_Call_method2(Call_method *v1140,OID v15308)
{ (*Generate.inline_exp)(Generate.PRODUCER->value,
    _oid_(v1140),
    v15308);
  } 

void  bexpression_any(OID v1140,OID v15308)
{ if (INHERIT(OWNER(v1140),Language._Assign))
   { princ_string("(");
    expression_Assign(OBJECT(Assign,v1140),v15308);
    princ_string(")");
    } 
  else if (INHERIT(OWNER(v1140),Optimize._to_C))
   { princ_string("(");
    (*Generate.expression)(v1140,
      v15308);
    princ_string(")");
    } 
  else if (INHERIT(OWNER(v1140),Kernel._integer))
   { if (v1140 < 0)
     { princ_string("(");
      expression_integer(v1140,v15308);
      princ_string(")");
      } 
    else expression_integer(v1140,v15308);
      } 
  else if (Kernel._float == OWNER(v1140))
   { if (float_v(v1140) < 0.0)
     { princ_string("(");
      expression_float(float_v(v1140),v15308);
      princ_string(")");
      } 
    else expression_float(float_v(v1140),v15308);
      } 
  else (*Generate.expression)(v1140,
      v15308);
    } 

void  expression_If(If *v1140,OID v15308)
{ GC_BIND;
  princ_string("(");
  (*Optimize.bool_exp)(GC_OID(v1140->test),
    Kernel.ctrue,
    v15308);
  princ_string(" ?");
  (Optimize.OPT->level = (Optimize.OPT->level+1));
  breakline_void();
  (*Generate.expression)(GC_OID(v1140->arg),
    v15308);
  princ_string(" :");
  breakline_void();
  (*Generate.expression)(GC_OID(v1140->other),
    v15308);
  princ_string(" )");
  (Optimize.OPT->level = (Optimize.OPT->level-1));
  GC_UNBIND;} 

void  expression_Assign(Assign *v1140,OID v15308)
{ GC_BIND;
  { OID  v7248 = v1140->arg;
    OID  v7247 = v1140->var;
    if ((INHERIT(OWNER(v7248),Language._Call_method2)) && ((_oid_(OBJECT(Call_method,v7248)->arg) == Generate._star_plus_integer_star->value) && 
        ((equal((*(OBJECT(Call_method,v7248)->args))[1],v1140->var) == CTRUE) && 
          ((*(OBJECT(Call_method,v7248)->args))[2] == 1))))
     { princ_string("++");
      (*Generate.expression)(v7247,
        v15308);
      princ_string("");
      } 
    else if ((boolean_I_any(v15308) == CTRUE) && 
        ((Optimize.OPT->loop_gc == CTRUE) && 
          (inner2outer_ask_any(v7248) == CTRUE)))
     { OID  v6493;
      { if (INHERIT(OWNER(v7248),Optimize._to_protect))
         v6493 = OBJECT(Compile_to_protect,v7248)->arg;
        else v6493 = v7248;
          GC_OID(v6493);} 
      (*Generate.gc_protection_exp)(Generate.PRODUCER->value,
        v7247,
        Kernel.ctrue,
        v6493,
        v15308);
      } 
    else (*Generate.exp_Assign)(Generate.PRODUCER->value,
        _oid_(v1140),
        v15308);
      } 
  GC_UNBIND;} 

void  expression_to_protect(Compile_to_protect *v1140,OID v15308)
{ (*Generate.exp_to_protect)(Generate.PRODUCER->value,
    _oid_(v1140),
    v15308);
  } 

char * gc_protect_class(ClaireClass *v7227)
{ { char *Result ;
    Result = ((v7227 == Kernel._any) ?
      "GC_OID" :
      ((v7227 == Kernel._string) ?
        "GC_STRING" :
        ((v7227 == Kernel._array) ?
          "GC_ARRAY" :
          ((INHERIT(v7227,Kernel._object)) ?
            "GC_OBJECT" :
            "" ) ) ) );
    return (Result);} 
  } 

void  expression_Gassign(Gassign *v1140,OID v15308)
{ (*Generate.gassign)(Generate.PRODUCER->value,
    _oid_(v1140),
    v15308);
  } 

void  expression_And(And *v1140,OID v15308)
{ GC_BIND;
  { ClaireBoolean * v7226 = ((v1140->args->length > 5) ? CTRUE : CFALSE);
    { OID gc_local;
      ITERATE(v7248);
      for (START(v1140->args); NEXT(v7248);)
      { princ_string("(");
        (*Optimize.bool_exp)(v7248,
          Kernel.ctrue,
          v15308);
        princ_string(" ? ");
        if (v7226 == CTRUE)
         breakline_void();
        } 
      } 
    expression_boolean(CTRUE,v15308);
    { OID gc_local;
      ITERATE(v7248);
      for (START(v1140->args); NEXT(v7248);)
      { princ_string(": ");
        expression_boolean(CFALSE,v15308);
        princ_string(")");
        } 
      } 
    } 
  GC_UNBIND;} 

void  expression_Or(Or *v1140,OID v15308)
{ GC_BIND;
  { ClaireBoolean * v7226 = ((v1140->args->length > 5) ? CTRUE : CFALSE);
    { OID gc_local;
      ITERATE(v7248);
      for (START(v1140->args); NEXT(v7248);)
      { princ_string("(");
        (*Optimize.bool_exp)(v7248,
          Kernel.ctrue,
          v15308);
        princ_string(" ? ");
        expression_boolean(CTRUE,v15308);
        princ_string(" : ");
        if (v7226 == CTRUE)
         breakline_void();
        } 
      } 
    expression_boolean(CFALSE,v15308);
    { OID gc_local;
      ITERATE(v7248);
      for (START(v1140->args); NEXT(v7248);)
      princ_string(")");
      } 
    } 
  GC_UNBIND;} 

void  expression_to_CL(Compile_to_CL *v1140,OID v15308)
{ GC_BIND;
  (*Generate.to_cl)(Generate.PRODUCER->value,
    v1140->arg,
    _oid_(v1140->set_arg),
    v15308);
  GC_UNBIND;} 

void  expression_to_C(Compile_to_C *v1140,OID v15308)
{ GC_BIND;
  (*Generate.to_c)(Generate.PRODUCER->value,
    v1140->arg,
    _oid_(v1140->set_arg),
    v15308);
  GC_UNBIND;} 

void  expression_C_cast(Compile_C_cast *v1140,OID v15308)
{ (*Kernel.cast_I)(Generate.PRODUCER->value,
    _oid_(v1140),
    v15308);
  } 

void  expression_Call_slot(Call_slot *v1140,OID v15308)
{ (*Generate.call_slot)(Generate.PRODUCER->value,
    _oid_(v1140),
    v15308);
  } 

void  expression_Call_table(Call_table *v1140,OID v15308)
{ (*Generate.call_table)(Generate.PRODUCER->value,
    _oid_(v1140),
    v15308);
  } 

void  expression_Call_array(Call_array *v1140,OID v15308)
{ (*Generate.call_array)(Generate.PRODUCER->value,
    _oid_(v1140),
    v15308);
  } 

void  expression_Update(Update *v1140,OID v15308)
{ (*Generate.update)(Generate.PRODUCER->value,
    _oid_(v1140),
    v15308);
  } 

void  sign_equal_boolean(ClaireBoolean *v1140)
{ if (v1140 == CTRUE)
   princ_string("==");
  else princ_string("!=");
    } 

void  sign_or_boolean(ClaireBoolean *v1140)
{ if (v1140 == CTRUE)
   princ_string("||");
  else princ_string("&&");
    } 

void  bool_exp_any_Generate(OID v1140,ClaireBoolean *v3475,OID v15308)
{ any_bool_exp_any(v1140,v3475,v15308,not_any(_oid_(((INHERIT(OWNER(v1140),Core._global_variable)) ? ((nativeVar_ask_global_variable(OBJECT(global_variable,v1140)) != CTRUE) ? CTRUE: CFALSE): CFALSE))));
  } 

void  any_bool_exp_any(OID v1140,ClaireBoolean *v3475,OID v15308,ClaireBoolean *v12390)
{ princ_string("(");
  bexpression_any(v1140,v15308);
  princ_string(" ");
  if (v3475 != CTRUE)
   princ_string("!=");
  else princ_string("==");
    princ_string(" ");
  if (v12390 != CTRUE)
   (*Generate.to_cl)(Generate.PRODUCER->value,
    Kernel.ctrue,
    _oid_(Kernel._boolean),
    v15308);
  else expression_boolean(CTRUE,v15308);
    princ_string(")");
  } 

void  bool_exp_to_CL_Generate(Compile_to_CL *v1140,ClaireBoolean *v3475,OID v15308)
{ GC_BIND;
  (*Optimize.bool_exp)(v1140->arg,
    _oid_(v3475),
    v15308);
  GC_UNBIND;} 

void  bool_exp_If_Generate(If *v1140,ClaireBoolean *v3475,OID v15308)
{ GC_BIND;
  if (boolean_I_any(v1140->other) == CTRUE)
   { princ_string("(");
    (*Optimize.bool_exp)(v1140->test,
      Kernel.ctrue,
      v15308);
    princ_string(" ? ");
    (*Optimize.bool_exp)(v1140->arg,
      _oid_(v3475),
      v15308);
    princ_string(" : ");
    (*Optimize.bool_exp)(v1140->other,
      _oid_(v3475),
      v15308);
    princ_string(")");
    } 
  else { princ_string("(");
      (*Optimize.bool_exp)(v1140->test,
        _oid_(v3475),
        v15308);
      princ_string(" ");
      sign_or_boolean(not_any(_oid_(v3475)));
      princ_string(" ");
      (*Optimize.bool_exp)(v1140->arg,
        _oid_(v3475),
        v15308);
      princ_string(")");
      } 
    GC_UNBIND;} 

void  bool_exp_And_Generate(And *v1140,ClaireBoolean *v3475,OID v15308)
{ GC_BIND;
  { list * v7236 = GC_OBJECT(list,v1140->args);
    int  v7237 = v7236->length;
    int  v7239 = 0;
    int  v9231 = Optimize.OPT->level;
    (Optimize.OPT->level = (Optimize.OPT->level+1));
    { OID gc_local;
      ITERATE(v7248);
      for (START(v7236); NEXT(v7248);)
      { ++v7239;
        if (v7239 == v7237)
         (*Optimize.bool_exp)(v7248,
          _oid_(v3475),
          v15308);
        else { princ_string("(");
            (*Optimize.bool_exp)(v7248,
              _oid_(v3475),
              v15308);
            princ_string(" ");
            sign_or_boolean(not_any(_oid_(v3475)));
            princ_string(" ");
            (Optimize.OPT->level = (Optimize.OPT->level+1));
            breakline_void();
            } 
          } 
      } 
    { int  v7248 = 2;
      int  v6657 = v7237;
      { OID gc_local;
        while ((v7248 <= v6657))
        { princ_string(")");
          ++v7248;
          } 
        } 
      } 
    (Optimize.OPT->level = v9231);
    } 
  GC_UNBIND;} 

void  bool_exp_Or_Generate(Or *v1140,ClaireBoolean *v3475,OID v15308)
{ GC_BIND;
  { list * v7236 = GC_OBJECT(list,v1140->args);
    int  v7237 = v7236->length;
    int  v7239 = 0;
    int  v9231 = Optimize.OPT->level;
    (Optimize.OPT->level = (Optimize.OPT->level+1));
    { OID gc_local;
      ITERATE(v7248);
      for (START(v7236); NEXT(v7248);)
      { ++v7239;
        if (v7239 == v7237)
         (*Optimize.bool_exp)(v7248,
          _oid_(v3475),
          v15308);
        else { princ_string("(");
            (*Optimize.bool_exp)(v7248,
              _oid_(v3475),
              v15308);
            princ_string(" ");
            sign_or_boolean(v3475);
            princ_string(" ");
            (Optimize.OPT->level = (Optimize.OPT->level+1));
            breakline_void();
            } 
          } 
      } 
    { int  v7248 = 2;
      int  v6660 = v7237;
      { OID gc_local;
        while ((v7248 <= v6660))
        { princ_string(")");
          ++v7248;
          } 
        } 
      } 
    (Optimize.OPT->level = v9231);
    } 
  GC_UNBIND;} 

void  bool_exp_Call_method1_Generate(Call_method1 *v1140,ClaireBoolean *v3475,OID v15308)
{ GC_BIND;
  { method * v7237 = v1140->arg;
    OID  v11032 = (*(v1140->args))[1];
    if (_oid_(v7237) == Generate._starnot_star->value)
     { if (INHERIT(OWNER(v11032),Optimize._to_CL))
       any_bool_exp_any(GC_OID((*Kernel.arg)(v11032)),not_any(_oid_(v3475)),v15308,CTRUE);
      else any_bool_exp_any(v11032,not_any(_oid_(v3475)),v15308,CFALSE);
        } 
    else if (_oid_(v7237) == Generate._starknown_star->value)
     (*Generate.equal_exp)(Generate.PRODUCER->value,
      v11032,
      _oid_(not_any(_oid_(v3475))),
      CNULL,
      v15308);
    else if (_oid_(v7237) == Generate._starunknown_star->value)
     (*Generate.equal_exp)(Generate.PRODUCER->value,
      v11032,
      _oid_(v3475),
      CNULL,
      v15308);
    else if (_inf_equal_type(v7237->range,Kernel._boolean) == CTRUE)
     { princ_string("(");
      expression_Call_method12(v1140,v15308);
      princ_string(" ");
      sign_equal_boolean(v3475);
      princ_string(" ");
      expression_boolean(CTRUE,v15308);
      princ_string(")");
      } 
    else { OID v_rec;
        v_rec = _oid_(v1140);
        PUSH(v_rec);
        v_rec = _oid_(v3475);
        PUSH(v_rec);
        v_rec = v15308;
        PUSH(v_rec);
        Optimize.bool_exp->super(Kernel._any,3);} 
      } 
  GC_UNBIND;} 

void  bool_exp_Call_method2_Generate(Call_method2 *v1140,ClaireBoolean *v3475,OID v15308)
{ GC_BIND;
  { method * v7237 = v1140->arg;
    property * v7240 = v7237->selector;
    list * v10533 = GC_OBJECT(list,OBJECT(Generate_producer,Generate.PRODUCER->value)->open_comparators);
    OID  v11032 = (*(v1140->args))[1];
    OID  v11033 = (*(v1140->args))[2];
    if (v7240 == Core._I_equal)
     (*Generate.equal_exp)(Generate.PRODUCER->value,
      v11032,
      _oid_(not_any(_oid_(v3475))),
      v11033,
      Core.nil->value);
    else if (v7240 == Core.identical_ask)
     (*Generate.equal_exp)(Generate.PRODUCER->value,
      v11032,
      _oid_(v3475),
      v11033,
      Kernel.ctrue);
    else if (v7240 == Kernel._equal)
     (*Generate.equal_exp)(Generate.PRODUCER->value,
      v11032,
      _oid_(v3475),
      v11033,
      Core.nil->value);
    else if ((v10533->memq(_oid_(v7240)) == CTRUE) && 
        ((domain_I_restriction(v7237) == Kernel._integer) || 
            (domain_I_restriction(v7237) == Kernel._float)))
     { princ_string("(");
      (*Generate.expression)(v11032,
        v15308);
      princ_string(" ");
      if (v3475 == CTRUE)
       print_any(_oid_(v7240));
      else print_any((*(v10533))[(mod_integer((index_list(v10533,_oid_(v7240))+1),4)+1)]);
        princ_string(" ");
      (*Generate.expression)(v11033,
        v15308);
      princ_string(")");
      } 
    else if (_oid_(v7237) == Generate._starnth_integer_star->value)
     { princ_string("(");
      if (v3475 != CTRUE)
       princ_string("!");
      (*Generate.bitvector_exp)(Generate.PRODUCER->value,
        v11032,
        v11033,
        v15308);
      princ_string(")");
      } 
    else if ((v7240 == Core.inherit_ask) && 
        (domain_I_restriction(v7237) == Kernel._class))
     { princ_string("(");
      if (v3475 != CTRUE)
       princ_string("!");
      (*Generate.inherit_exp)(Generate.PRODUCER->value,
        v11032,
        v11033,
        v15308);
      princ_string(")");
      } 
    else if (_inf_equal_type(v7237->range,Kernel._boolean) == CTRUE)
     { princ_string("(");
      expression_Call_method22(v1140,v15308);
      princ_string(" ");
      sign_equal_boolean(v3475);
      princ_string(" ");
      expression_boolean(CTRUE,v15308);
      princ_string(")");
      } 
    else { OID v_rec;
        v_rec = _oid_(v1140);
        PUSH(v_rec);
        v_rec = _oid_(v3475);
        PUSH(v_rec);
        v_rec = v15308;
        PUSH(v_rec);
        Optimize.bool_exp->super(Kernel._any,3);} 
      } 
  GC_UNBIND;} 

ClaireBoolean * bool_exp_ask_any(OID v7248)
{ GC_BIND;
  { ClaireBoolean *Result ;
    if (INHERIT(OWNER(v7248),Optimize._to_CL))
     Result = bool_exp_ask_any(OBJECT(Compile_to_CL,v7248)->arg);
    else if (INHERIT(OWNER(v7248),Language._Call_method))
     { property * v7240 = OBJECT(Call_method,v7248)->arg->selector;
      Result = (((OBJECT(Call_method,v7248)->arg->range == Kernel._boolean) && 
          ((v7240 == Kernel._equal) || 
              ((v7240 == Core._I_equal) || 
                ((v7240 == Kernel._sup) || 
                  ((v7240 == Kernel._sup_equal) || 
                    ((v7240 == Kernel._inf) || 
                      (v7240 == Kernel._inf_equal))))))) ? CTRUE : (((v7240 == Core.NOT) && 
          (bool_exp_ask_any((*(OBJECT(Call_method,v7248)->args))[1]) == CTRUE)) ? CTRUE : CFALSE));
      } 
    else Result = CFALSE;
      GC_UNBIND; return (Result);} 
  } 

void  args_list_bag(bag *v1140,OID v15308,ClaireBoolean *v750)
{ { ClaireBoolean * v11475 = CTRUE;
    if (v750 == CTRUE)
     (Optimize.OPT->level = (Optimize.OPT->level+1));
    { ITERATE(v7248);
      for (START(v1140); NEXT(v7248);)
      if (v11475 == CTRUE)
       { (*Generate.expression)(v7248,
          v15308);
        v11475= CFALSE;
        } 
      else { princ_string(",");
          if (v750 == CTRUE)
           breakline_void();
          (*Generate.expression)(v7248,
            v15308);
          princ_string("");
          } 
        } 
    if (v750 == CTRUE)
     (Optimize.OPT->level = (Optimize.OPT->level-1));
    } 
  } 

char * check_var_string(char *v1140,OID v7243,OID v15308)
{ { char *Result ;
    Result = (((equal(_string_(v1140),v7243) == CTRUE) || 
        (equal(_string_(v1140),v15308) == CTRUE)) ?
      append_string(v1140,"1") :
      v1140 );
    return (Result);} 
  } 

Variable * build_Variable_string(char *v7243,OID v7244)
{ return (Variable_I_symbol(symbol_I_string2(v7243),0,OBJECT(ClaireType,v7244)));} 

