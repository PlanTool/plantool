/***** CLAIRE Compilation of file c:\claire\v3.3\src\compile\cgen.cl 
         [version 3.3.4 / safety 5] Sat Oct 16 06:53:38 2004 *****/

#include <claire.h>
#include <Kernel.h>
#include <Core.h>
#include <Language.h>
#include <Reader.h>
#include <Optimize.h>
#include <Generate.h>
char * c_string_c_producer1(Generate_c_producer *v7227,Variable *v1140)
{ GC_BIND;
  { char *Result ;
    if (Optimize.compiler->naming == 2)
     Result = append_string("v",GC_STRING(string_I_integer (integer_I_symbol(v1140->pname))));
    else { print_in_string_void();
        ident_c_producer(v7227,v1140->pname);
        Result = end_of_print_void();
        } 
      GC_UNBIND; return (Result);} 
  } 

char * c_string_c_producer2(Generate_c_producer *v7227,symbol *v1140)
{ print_in_string_void();
  ident_c_producer(v7227,v1140);
  return (end_of_print_void());} 

char * string_I_c_producer1(Generate_c_producer *v7227,Variable *v1140)
{ GC_BIND;
  { char *Result ;
    if (Optimize.compiler->naming == 2)
     Result = append_string("v",GC_STRING(string_I_integer (integer_I_symbol(v1140->pname))));
    else { print_in_string_void();
        ident_c_producer(v7227,v1140->pname);
        Result = end_of_print_void();
        } 
      GC_UNBIND; return (Result);} 
  } 

char * string_I_c_producer2(Generate_c_producer *v7227,symbol *v1140)
{ print_in_string_void();
  ident_c_producer(v7227,v1140);
  return (end_of_print_void());} 

void  ident_c_producer3(Generate_c_producer *v7227,Variable *v7247)
{ GC_BIND;
  if (Optimize.compiler->naming == 2)
   princ_string(append_string("v",GC_STRING(string_I_integer (integer_I_symbol(v7247->pname)))));
  else { symbol * v7243 = v7247->pname;
      int  v7239 = index_list(v7227->bad_names,_oid_(v7243));
      if (v7239 == 0)
       c_princ_string(string_I_symbol(v7243));
      else c_princ_symbol(OBJECT(symbol,(*(v7227->good_names))[v7239]));
        } 
    GC_UNBIND;} 

void  ident_c_producer(Generate_c_producer *v7227,symbol *v7243)
{ { int  v7239 = index_list(v7227->bad_names,_oid_(v7243));
    if (v7239 == 0)
     c_princ_string(string_I_symbol(v7243));
    else c_princ_symbol(OBJECT(symbol,(*(v7227->good_names))[v7239]));
      } 
  } 

void  class_princ_c_producer(Generate_c_producer *v7227,ClaireClass *v1140)
{ if (v1140->name->module_I != claire.it)
   { ident_c_producer(v7227,v1140->name->module_I->name);
    princ_string("_");
    } 
  ident_c_producer(v7227,v1140->name);
  } 

void  produce_c_producer2(Generate_c_producer *v7227,OID v7248)
{ if (INHERIT(OWNER(v7248),Kernel._function))
   { princ_string("_function_(");
    c_princ_function(OBJECT(ClaireFunction,v7248));
    princ_string(",");
    print_any(_string_(string_I_function(OBJECT(ClaireFunction,v7248))));
    princ_string(")");
    } 
  else if (INHERIT(OWNER(v7248),Kernel._char))
   { princ_string("_char_('");
    if ((v7248 == _oid_(_char_('\"'))) || 
        ((v7248 == _oid_(_char_('\''))) || 
          ((v7248 == _oid_(_char_('\?'))) || 
            (v7248 == _oid_(_char_('\\'))))))
     princ_string("\\");
    princ_char(OBJECT(ClaireChar,v7248));
    princ_string("')");
    } 
  else if (INHERIT(OWNER(v7248),Kernel._environment))
   princ_string("ClEnv");
  else if (Kernel._string == OWNER(v7248))
   print_any(v7248);
  else if (INHERIT(OWNER(v7248),Core._global_variable))
   globalVar_c_producer(v7227,OBJECT(global_variable,v7248));
  else if (Kernel._boolean == OWNER(v7248))
   { if ((OBJECT(ClaireBoolean,v7248)) == CTRUE)
     princ_string("CTRUE");
    else princ_string("CFALSE");
      } 
  else if (INHERIT(OWNER(v7248),Kernel._symbol))
   { princ_string("symbol::make(");
    print_any(_string_(string_I_symbol(OBJECT(symbol,v7248))));
    princ_string(",");
    ident_symbol(OBJECT(symbol,v7248)->module_I->name);
    princ_string(",");
    ident_symbol(defined_symbol(OBJECT(symbol,v7248))->name);
    princ_string(")");
    } 
  else if (INHERIT(OWNER(v7248),Kernel._class))
   { if (v7248 == _oid_(Kernel._cl_import))
     princ_string("Kernel._cl_import");
    else { ident_c_producer(v7227,defined_symbol(OBJECT(ClaireClass,v7248)->name)->name);
        princ_string("._");
        c_princ_string(string_I_symbol(OBJECT(ClaireClass,v7248)->name));
        princ_string("");
        } 
      } 
  else if (INHERIT(OWNER(v7248),Reader._reserved_keyword))
   { ident_c_producer(v7227,defined_symbol(OBJECT(thing,v7248)->name)->name);
    princ_string("._cl_");
    c_princ_string(string_I_symbol(OBJECT(thing,v7248)->name));
    princ_string("");
    } 
  else if (INHERIT(OWNER(v7248),Kernel._thing))
   { if (INHERIT(OBJECT(ClaireObject,v7248)->isa,Kernel._module))
     { ident_c_producer(v7227,OBJECT(thing,v7248)->name);
      princ_string(".it");
      } 
    else { ident_c_producer(v7227,defined_symbol(OBJECT(thing,v7248)->name)->name);
        princ_string(".");
        ident_symbol(OBJECT(thing,v7248)->name);
        princ_string("");
        } 
      } 
  else princ_string("CNULL");
    } 

void  globalVar_c_producer(Generate_c_producer *v7227,global_variable *v7248)
{ GC_BIND;
  if ((equal(_oid_(v7248->range),_oid_(Kernel.emptySet)) == CTRUE) && 
      (INHERIT(OWNER(v7248->value),Kernel._integer)))
   (*Kernel.princ)(v7248->value);
  else { ident_c_producer(v7227,defined_symbol(v7248->name)->name);
      princ_string(".");
      ident_symbol(v7248->name);
      if (nativeVar_ask_global_variable(v7248) != CTRUE)
       princ_string("->value");
      princ_string("");
      } 
    GC_UNBIND;} 

void  stat_exp_c_producer(Generate_c_producer *v7227,OID v1140,OID v15308)
{ GC_BIND;
  if (INHERIT(OWNER(v1140),Optimize._to_C))
   stat_exp_c_producer(v7227,GC_OID(OBJECT(Compile_to_C,v1140)->arg),v15308);
  else if (INHERIT(OWNER(v1140),Optimize._to_CL))
   stat_exp_c_producer(v7227,GC_OID(OBJECT(Compile_to_CL,v1140)->arg),v15308);
  else if (designated_ask_any(v1140) == CTRUE)
   princ_string(";");
  else { (*Generate.expression)(v1140,
        v15308);
      princ_string(";");
      breakline_void();
      princ_string("");
      } 
    GC_UNBIND;} 

void  namespace_I_c_producer(Generate_c_producer *v7227,module *v7237)
{ princ_string("\n\n// namespace class for ");
  print_any(_oid_(v7237));
  princ_string(" \n");
  { princ_string("class ");
    ident_c_producer(v7227,v7237->name);
    princ_string("Class: public NameSpace {\npublic:\n");
    } 
  } 

void  module_I_c_producer(Generate_c_producer *v7227,module *v7237)
{ princ_string("\n\n// module definition \n");
  princ_string(" void metaLoad();};\n\n");
  princ_string("extern ");
  ident_c_producer(v7227,v7237->name);
  princ_string("Class ");
  ident_c_producer(v7227,v7237->name);
  princ_string(";\n");
  { module * v11405 = v7237->part_of;
    { while (((boolean_I_any(_oid_(v11405->made_of)) != CTRUE) && 
          (v11405 != claire.it)))
      { princ_string("extern NameSpace ");
        ident_c_producer(v7227,v11405->name);
        princ_string(";\n");
        v11405= v11405->part_of;
        } 
      } 
    } 
  } 

void  declare_c_producer(Generate_c_producer *v7227,property *v7240)
{ breakline_void();
  { expression_thing(v7240,Core.nil->value);
    princ_string(" = ");
    princ_string(((INHERIT(v7240->isa,Kernel._operation)) ?
      "operation" :
      "property" ));
    princ_string("::make(");
    print_any(_string_(string_I_symbol(v7240->name)));
    princ_string(",");
    if ((v7240->open != 1) || 
        (v7240->dispatcher > 0))
     { expression_integer(v7240->open,_oid_(Kernel.emptySet));
      princ_string(",");
      } 
    expression_thing(v7240->name->module_I,Core.nil->value);
    if (INHERIT(v7240->isa,Kernel._operation))
     { princ_string(",");
      princ_integer(CLREAD(operation,v7240,precedence));
      princ_string("");
      } 
    else if (v7240->dispatcher > 0)
     { princ_string(",");
      (*Generate.expression)(_oid_(v7240->domain),
        Core.nil->value);
      princ_string(",");
      princ_integer(v7240->dispatcher);
      princ_string("");
      } 
    princ_string(");");
    } 
  } 

void  start_module_interface_c_producer(Generate_c_producer *v7227,module *v1140)
{ GC_BIND;
  { char * v15836 = GC_STRING(append_string(GC_STRING(Optimize.compiler->headers_dir),GC_STRING(append_string(GC_STRING(append_string(GC_STRING(string_v(Reader._starfs_star->value)),string_I_symbol(v1140->name))),".h"))));
    (Optimize.OPT->cinterface = fopen_string(v15836,"w"));
    use_as_output_port(Optimize.OPT->cinterface);
    princ_string("// interface defination for module ");
    print_any(_oid_(v1140));
    princ_string(", ");
    princ_string(date_I_integer(1));
    princ_string("");
    princ_string("#ifndef CLAIREH_");
    ident_thing(v1140);
    princ_string("\n#define CLAIREH_");
    ident_thing(v1140);
    princ_string("\n");
    { OID gc_local;
      ITERATE(v7248);
      bag *v7248_support;
      v7248_support = GC_OBJECT(list,Optimize.compiler->headers);
      for (START(v7248_support); NEXT(v7248);)
      { princ_string("#include \"");
        princ_string(string_v(v7248));
        princ_string(".h\"\n");
        } 
      } 
    use_as_output_port(EXPORT((ClairePort *),Reader.STDOUT->value));
    } 
  GC_UNBIND;} 

void  end_module_interface_c_producer(Generate_c_producer *v7227,module *v1140)
{ princ_string("\n#endif\n");
  } 

void  generate_end_file_c_producer(Generate_c_producer *v7234,module *v7237)
{ GC_BIND;
  close_block_void();
  fclose_port(Optimize.OPT->outfile);
  { OID gc_local;
    ITERATE(v7227);
    bag *v7227_support;
    v7227_support = GC_OBJECT(list,Optimize.OPT->objects);
    for (START(v7227_support); NEXT(v7227);)
    { GC_LOOP;
      if (INHERIT(OWNER(v7227),Kernel._class))
       { ClairePort * v7240 = fopen_string(append_string(GC_STRING(append_string(GC_STRING(append_string(GC_STRING(Optimize.compiler->source),GC_STRING(string_v(Reader._starfs_star->value)))),GC_STRING(c_string_c_producer2(v7234,OBJECT(symbol,(*Kernel.name)(v7227)))))),GC_STRING(v7234->extension)),"a");
        use_as_output_port(v7240);
        close_block_void();
        fclose_port(v7240);
        } 
      GC_UNLOOP;} 
    } 
  GC_UNBIND;} 

void  generate_classes_c_producer(Generate_c_producer *v7234,char *v7243,module *v7237)
{ princ_string("\n\n");
  princ_string(v7243);
  princ_string("Class ");
  princ_string(v7243);
  princ_string(";\n\n");
  { module * v11404 = v7237;
    module * v11405 = v11404->part_of;
    { while (((boolean_I_any(_oid_(v11405->made_of)) != CTRUE) && 
          ((v11405 != claire.it) && 
            ((*(v11405->parts))[1] == _oid_(v11404)))))
      { princ_string("NameSpace ");
        (*Language.ident)(Generate.PRODUCER->value,
          _oid_(v11405->name));
        princ_string(";\n");
        v11405= v11405->part_of;
        v11404= v11405;
        } 
      } 
    } 
  } 

void  generate_start_file_c_producer(Generate_c_producer *v7227,module *v7237)
{ GC_BIND;
  tformat_string("++++ Creating the file ~A.cpp \n",2,list::alloc(1,GC_OID(_string_(c_string_c_producer2(v7227,v7237->name)))));
  start_file_string(string_I_symbol(v7237->name),v7237);
  (Optimize.OPT->level = 0);
  princ_string("/* class file for module ");
  print_any(_oid_(v7237));
  princ_string(" */");
  breakline_void();
  breakline_void();
  princ_string("public class ");
  ident_c_producer(v7227,v7237->name);
  princ_string(" extends NameSpace");
  new_block_void();
  GC_UNBIND;} 

void  generate_meta_load_c_producer(Generate_c_producer *v7227,module *v7237)
{ princ_string("void ");
  ident_c_producer(v7227,v7237->name);
  princ_string("Class::metaLoad() ");
  new_block_void();
  breakline_void();
  { princ_string("GC_BIND;");
    breakline_void();
    princ_string("ClEnv->module_I = it;\n");
    } 
  } 

OID  start_file_string(char *v7243,module *v7237)
{ GC_BIND;
  use_as_output_port(Optimize.OPT->outfile);
  princ_string("/***** CLAIRE Compilation of file ");
  princ_string(v7243);
  princ_string(".cl \n         [version ");
  (*Kernel.princ)(GC_OID(release_void()));
  princ_string(" / safety ");
  print_any(Optimize.compiler->safety);
  princ_string("] ");
  princ_string(substring_string(date_I_integer(1),1,24));
  princ_string(" *****/\n\n");
  princ_string("#include <claire.h>\n");
  princ_string("#include <Kernel.h>\n");
  { OID gc_local;
    ITERATE(v7248);
    bag *v7248_support;
    v7248_support = GC_OBJECT(list,add_modules_list(list::alloc(1,_oid_(v7237))));
    for (START(v7248_support); NEXT(v7248);)
    { GC_LOOP;
      { { OID gc_local;
          ITERATE(v7251);
          bag *v7251_support;
          v7251_support = GC_OBJECT(list,OBJECT(module,v7248)->uses);
          for (START(v7251_support); NEXT(v7251);)
          if (Kernel._string == OWNER(v7251))
           { princ_string("#include <");
            (*Kernel.princ)(v7251);
            princ_string(".h>\n");
            } 
          } 
        if (OBJECT(module,v7248)->made_of->length != 0)
         { princ_string("#include <");
          ident_symbol(OBJECT(symbol,(*Kernel.name)(v7248)));
          princ_string(".h>\n");
          } 
        } 
      GC_UNLOOP;} 
    } 
  if (boolean_I_any(Optimize.OPT->cfile) == CTRUE)
   { princ_string("#include <");
    (*Kernel.princ)(GC_OID(Optimize.OPT->cfile));
    princ_string(".h>\n");
    } 
  { OID Result = 0;
    Result = ClAlloc->import(Kernel._port,(int *) use_as_output_port(EXPORT((ClairePort *),Reader.STDOUT->value)));
    GC_UNBIND; return (Result);} 
  } 

void  define_variable_c_producer2(Generate_c_producer *v7227,ClaireClass *v7244,char *v7247)
{ interface_I_class(v7244);
  princ_string(v7247);
  princ_string(" ");
  if ((v7244 == Kernel._integer) || 
      (v7244 == Kernel._any))
   princ_string("= 0");
  else if (v7244 == Kernel._float)
   princ_string("=0.0");
  princ_string(";");
  } 

void  generate_profile_c_producer(Generate_c_producer *v7227,OID v7237)
{ if (Kernel._method == OWNER(v7237))
   get_dependents_method(OBJECT(method,v7237));
  { princ_string("   PRcount *PR_x = PRstart(PRget_property(");
    expression_thing(((Kernel._method == OWNER(v7237)) ?
      OBJECT(restriction,v7237)->selector :
      Kernel.fastcall ),Core.nil->value);
    princ_string("));");
    breakline_void();
    princ_string("");
    } 
  } 

void  generate_interface_c_producer(Generate_c_producer *v7227,module *v1140)
{ GC_RESERVE(8);  // v3.0.55 optim !
  { OID gc_local;
    ITERATE(v7248);
    bag *v7248_support;
    v7248_support = GC_OBJECT(list,Optimize.OPT->objects);
    for (START(v7248_support); NEXT(v7248);)
    if (INHERIT(OWNER(v7248),Kernel._class))
     { princ_string("\nclass ");
      class_princ_c_producer(v7227,OBJECT(ClaireClass,v7248));
      princ_string(";");
      } 
    } 
  { OID gc_local;
    ITERATE(v7248);
    bag *v7248_support;
    v7248_support = GC_OBJECT(list,Optimize.OPT->objects);
    for (START(v7248_support); NEXT(v7248);)
    { GC_LOOP;
      if (INHERIT(OWNER(v7248),Kernel._class))
       { princ_string("\n\nclass ");
        class_princ_c_producer(v7227,OBJECT(ClaireClass,v7248));
        princ_string(": public ");
        class_princ_c_producer(v7227,OBJECT(ClaireClass,v7248)->superclass);
        new_block_void();
        breakline_void();
        princ_string("public:");
        { OID gc_local;
          ITERATE(v7249);
          bag *v7249_support;
          v7249_support = GC_OBJECT(list,OBJECT(bag,(*Optimize.get_indexed)(v7248)));
          for (START(v7249_support); NEXT(v7249);)
          { ClaireBoolean * g0060I;
            { OID  v11930;
              { OID gc_local;
                ITERATE(v11592);
                v11930= _oid_(CFALSE);
                for (START(OBJECT(ClaireClass,v7248)->superclass->slots); NEXT(v11592);)
                if (_I_equal_any(_oid_(OBJECT(restriction,v11592)->selector),_oid_(OBJECT(restriction,v7249)->selector)) != CTRUE)
                 { v11930 = Kernel.ctrue;
                  break;} 
                } 
              g0060I = not_any(v11930);
              } 
            
            if (g0060I == CTRUE) { breakline_void();
                princ_string("   ");
                (*Generate.interface_I)(Generate.PRODUCER->value,
                  _oid_(psort_any(_oid_(OBJECT(restriction,v7249)->range))));
                ident_symbol(OBJECT(restriction,v7249)->selector->name);
                princ_string(";");
                } 
              } 
          } 
        (*Generate.methods_interface)(Generate.PRODUCER->value,
          v7248);
        close_block_void();
        princ_string(";");
        } 
      GC_UNLOOP;} 
    } 
  { OID gc_local;
    ITERATE(v7236);
    bag *v7236_support;
    v7236_support = GC_OBJECT(list,Optimize.OPT->functions);
    for (START(v7236_support); NEXT(v7236);)
    { GC_LOOP;
      { OID  v9226 = GC_OID((*Kernel.nth)(v7236,
          1));
        OID  v13225 = GC_OID((*Kernel.nth)(v7236,
          2));
        OID  v7243 = GC_OID((*Kernel.nth)(v7236,
          3));
        princ_string("\nextern ");
        (*Generate.interface_I)(Generate.PRODUCER->value,
          v7243);
        princ_string(" ");
        (*Kernel.c_princ)(v9226);
        princ_string("(");
        typed_args_list_list(OBJECT(list,v13225));
        princ_string(");");
        } 
      GC_UNLOOP;} 
    } 
  GC_UNBIND;} 

void  global_var_def_I_c_producer(Generate_c_producer *v7227,module *v1140,Let *v7248)
{ GC_BIND;
  { global_variable * v11997 = OBJECT(global_variable,get_symbol(OBJECT(symbol,(*(OBJECT(bag,(*Core.args)(GC_OID(v7248->value)))))[1])));
    ClaireClass * v7243 = getRange_global_variable(v11997);
    OID  v8966 = GC_OID((*(OBJECT(Do,v7248->arg)->args))[2]);
    OID  v2249;
    { if (INHERIT(OWNER(v8966),Language._Update))
       v2249 = OBJECT(Update,v8966)->value;
      else if ((INHERIT(OWNER(v8966),Language._Call)) || 
          (INHERIT(OWNER(v8966),Language._Call_method)))
       v2249 = (*(OBJECT(bag,(*Core.args)(v8966))))[5];
      else { OID  v8406;close_exception(((general_error *) (*Core._general_error)(_string_("Design bug: make ~S a public global var !"),
            _oid_(list::alloc(1,_oid_(v11997))))));
          
          v2249=_void_(v8406);} 
        GC_OID(v2249);} 
    ClaireBoolean * v7226 = c_func_any(v2249);
    Variable * v4936 = GC_OBJECT(Variable,build_Variable_string("V_C",_oid_(Kernel._any)));
    if (nativeVar_ask_global_variable(v11997) != CTRUE)
     statement_any(_oid_(v7248),Core.nil->value,Core.nil->value);
    else { if (v7226 != CTRUE)
         { new_block_void();
          princ_string("OID ");
          ident_c_producer3(v7227,v4936);
          princ_string(";");
          breakline_void();
          statement_any(v2249,GC_OID((*Generate.c_string)(Generate.PRODUCER->value,
            _oid_(v4936))),Core.nil->value);
          princ_string("");
          } 
        ident_c_producer(v7227,v1140->name);
        princ_string(".");
        ident_c_producer(v7227,v11997->name);
        princ_string(" = ");
        if (v7226 == CTRUE)
         to_c_c_producer1(v7227,v2249,v7243,Core.nil->value);
        else to_c_c_producer1(v7227,_oid_(v4936),v7243,Core.nil->value);
          princ_string(";");
        if (v7226 != CTRUE)
         close_block_void();
        breakline_void();
        princ_string("");
        } 
      } 
  GC_UNBIND;} 

void  gc_introduction_c_producer(Generate_c_producer *v7227,OID v9221)
{ GC_BIND;
  if (Optimize.OPT->protection == CTRUE)
   { OID  v7250 = GC_OID(gc_usage_any(v9221,CFALSE));
    if ((Optimize.OPT->loop_gc == CTRUE) && 
        (INHERIT(OWNER(v7250),Kernel._integer)))
     { if ((OBJECT(ClaireBoolean,(*Kernel._inf)(v7250,
        100))) == CTRUE)
       { (Optimize.OPT->loop_index = 0);
        princ_string("GC_RESERVE(1);  // HOHO v3.0.55 optim !");
        } 
      else { princ_string("GC_RESERVE(");
          princ_integer((Optimize.OPT->loop_index+1));
          princ_string(");  // v3.0.55 optim !");
          } 
        } 
    else princ_string("GC_BIND;");
      breakline_void();
    } 
  GC_UNBIND;} 

OID  gc_usage_any(OID v1140,ClaireBoolean *v15308)
{ GC_BIND;
  { OID Result = 0;
    if (INHERIT(OWNER(v1140),Language._Instruction))
     { if (INHERIT(OBJECT(ClaireObject,v1140)->isa,Language._Construct))
       Result = gc_usage_star_list(GC_OBJECT(list,OBJECT(Construct,v1140)->args),v15308);
      else if (INHERIT(OBJECT(ClaireObject,v1140)->isa,Language._Definition))
       Result = Kernel.ctrue;
      else if (INHERIT(OBJECT(ClaireObject,v1140)->isa,Language._Call_method))
       Result = gc_usage_star_list(GC_OBJECT(list,OBJECT(Call_method,v1140)->args),v15308);
      else if (INHERIT(OBJECT(ClaireObject,v1140)->isa,Language._Call_slot))
       Result = gc_usage_any(GC_OID(OBJECT(Call_slot,v1140)->arg),v15308);
      else if (INHERIT(OBJECT(ClaireObject,v1140)->isa,Language._Call_table))
       Result = gc_usage_any(GC_OID(OBJECT(Call_table,v1140)->arg),v15308);
      else if (INHERIT(OBJECT(ClaireObject,v1140)->isa,Language._Call_array))
       Result = gc_usage_any(GC_OID(OBJECT(Call_array,v1140)->arg),v15308);
      else if (INHERIT(OBJECT(ClaireObject,v1140)->isa,Optimize._to_C))
       Result = gc_usage_any(GC_OID(OBJECT(Compile_to_C,v1140)->arg),v15308);
      else if (INHERIT(OBJECT(ClaireObject,v1140)->isa,Optimize._to_CL))
       Result = gc_usage_any(GC_OID(OBJECT(Compile_to_CL,v1140)->arg),v15308);
      else if (INHERIT(OBJECT(ClaireObject,v1140)->isa,Optimize._to_protect))
       Result = Kernel.ctrue;
      else if (INHERIT(OBJECT(ClaireObject,v1140)->isa,Language._Update))
       Result = gc_usage_any(GC_OID(OBJECT(Update,v1140)->value),v15308);
      else if (INHERIT(OBJECT(ClaireObject,v1140)->isa,Language._Call))
       Result = gc_usage_star_list(GC_OBJECT(list,OBJECT(Call,v1140)->args),v15308);
      else if (INHERIT(OBJECT(ClaireObject,v1140)->isa,Language._If))
       Result = gc_or_any(gc_usage_any(GC_OID(OBJECT(If,v1140)->test),v15308),gc_or_any(GC_OID(gc_usage_any(GC_OID(OBJECT(If,v1140)->arg),v15308)),GC_OID(gc_usage_any(GC_OID(OBJECT(If,v1140)->other),v15308))));
      else if (INHERIT(OBJECT(ClaireObject,v1140)->isa,Language._Do))
       Result = gc_usage_star_list(GC_OBJECT(list,OBJECT(Do,v1140)->args),v15308);
      else if (INHERIT(OBJECT(ClaireObject,v1140)->isa,Language._Iteration))
       Result = gc_or_any(GC_OID(gc_usage_any(GC_OID(OBJECT(Iteration,v1140)->set_arg),v15308)),GC_OID(gc_usage_any(GC_OID(OBJECT(Iteration,v1140)->arg),CTRUE)));
      else if (INHERIT(OBJECT(ClaireObject,v1140)->isa,Language._Let))
       { OID  v7248 = GC_OID(OBJECT(Let,v1140)->value);
        { OID  v13852;
          if ((v15308 == CTRUE) && 
              (INHERIT(OWNER(v7248),Optimize._to_protect)))
           v13852 = OBJECT(Instruction_with_var,v1140)->var->index;
          else v13852 = Kernel.cfalse;
            Result = gc_or_any(v13852,gc_or_any(GC_OID(gc_usage_any(v7248,v15308)),GC_OID(gc_usage_any(GC_OID(OBJECT(Let,v1140)->arg),v15308))));
          } 
        } 
      else if (INHERIT(OBJECT(ClaireObject,v1140)->isa,Language._Assign))
       { OID  v7248 = GC_OID(OBJECT(Assign,v1140)->arg);
        { OID  v14813;
          { if ((v15308 == CTRUE) && 
                (inner2outer_ask_any(v7248) == CTRUE))
             v14813 = (*Kernel.index)(GC_OID(OBJECT(Assign,v1140)->var));
            else v14813 = Kernel.cfalse;
              GC_OID(v14813);} 
          Result = gc_or_any(v14813,GC_OID(gc_usage_any(v7248,v15308)));
          } 
        } 
      else if (INHERIT(OBJECT(ClaireObject,v1140)->isa,Optimize._to_protect))
       Result = Kernel.ctrue;
      else if (INHERIT(OBJECT(ClaireObject,v1140)->isa,Language._Gassign))
       Result = gc_usage_any(GC_OID(OBJECT(Gassign,v1140)->arg),v15308);
      else if (INHERIT(OBJECT(ClaireObject,v1140)->isa,Language._And))
       Result = gc_usage_star_list(GC_OBJECT(list,OBJECT(And,v1140)->args),v15308);
      else if (INHERIT(OBJECT(ClaireObject,v1140)->isa,Language._Or))
       Result = gc_usage_star_list(GC_OBJECT(list,OBJECT(Or,v1140)->args),v15308);
      else if (INHERIT(OBJECT(ClaireObject,v1140)->isa,Language._Cast))
       Result = gc_usage_any(GC_OID(OBJECT(Cast,v1140)->arg),v15308);
      else if (INHERIT(OBJECT(ClaireObject,v1140)->isa,Language._Super))
       Result = gc_usage_star_list(GC_OBJECT(list,OBJECT(Super,v1140)->args),v15308);
      else if (INHERIT(OBJECT(ClaireObject,v1140)->isa,Language._Case))
       Result = gc_usage_star_list(GC_OBJECT(list,OBJECT(Case,v1140)->args),v15308);
      else if (INHERIT(OBJECT(ClaireObject,v1140)->isa,Language._While))
       Result = gc_or_any(GC_OID(gc_usage_any(GC_OID(OBJECT(While,v1140)->arg),CTRUE)),GC_OID(gc_usage_any(GC_OID(OBJECT(While,v1140)->test),v15308)));
      else if (INHERIT(OBJECT(ClaireObject,v1140)->isa,Language._Return))
       Result = gc_usage_any(GC_OID(OBJECT(Return,v1140)->arg),v15308);
      else if (INHERIT(OBJECT(ClaireObject,v1140)->isa,Language._Handle))
       Result = gc_or_any(gc_or_any(GC_OID(gc_usage_any(GC_OID(OBJECT(ClaireHandle,v1140)->arg),v15308)),GC_OID(gc_usage_any(GC_OID(OBJECT(ClaireHandle,v1140)->other),v15308))),gc_usage_any(GC_OID(OBJECT(ClaireHandle,v1140)->test),v15308));
      else Result = Kernel.cfalse;
        } 
    else Result = Kernel.cfalse;
      GC_UNBIND; return (Result);} 
  } 

OID  gc_or_any(OID v7248,OID v7249)
{ { OID Result = 0;
    if (v7248 == Kernel.cfalse)
     Result = v7249;
    else if (v7248 == Kernel.ctrue)
     { if (v7249 == Kernel.cfalse)
       Result = v7248;
      else Result = v7249;
        } 
    else if (INHERIT(OWNER(v7249),Kernel._integer))
     { if (equal(v7248,v7249) == CTRUE)
       Result = v7249;
      else Result = 1000;
        } 
    else Result = v7248;
      return (Result);} 
  } 

OID  gc_usage_star_list(list *v7236,ClaireBoolean *v15308)
{ GC_RESERVE(1);  // HOHO v3.0.55 optim !
  { OID Result = 0;
    { OID  v7248 = Kernel.cfalse;
      { OID gc_local;
        ITERATE(v7249);
        for (START(v7236); NEXT(v7249);)
        { GC_LOOP;
          GC__OID(v7248 = gc_or_any(v7248,gc_usage_any(v7249,v15308)), 1);
          GC_UNLOOP;} 
        } 
      Result = v7248;
      } 
    GC_UNBIND; return (Result);} 
  } 

void  debug_intro_c_producer(Generate_c_producer *v7227,lambda *v1140,method *v7248)
{ GC_BIND;
  { module * v7237;
    { ClaireObject *V_CC ;
      if (Kernel._method == v7248->isa)
       V_CC = v7248->module_I;
      else V_CC = CFALSE;
        v7237= (module *) V_CC;} 
    int  v7239 = 1;
    princ_string("DB_BIND(");
    ident_thing(v7237);
    princ_string(",");
    expression_thing(v7248->selector,_oid_(Kernel.emptySet));
    princ_string(",");
    princ_integer(v1140->vars->length);
    princ_string(",{");
    if ((v1140->vars->length == 1) && 
        ((*Kernel.range)((*(v1140->vars))[1]) == _oid_(Kernel._void)))
     princ_string("PUSH(_oid_(ClEnv));");
    else { OID gc_local;
        ITERATE(v7247);
        for (START(v1140->vars); NEXT(v7247);)
        { princ_string("PUSH(");
          { OID  v7243 = (*(v7248->srange))[v7239];
            if (((v7243 == _oid_(Kernel._any)) && 
                  ((*Kernel.range)(v7247) != _oid_(Kernel._float))) || 
                (v7243 == _oid_(Kernel._integer)))
             (*Generate.expression)(v7247,
              Kernel.cfalse);
            else to_cl_c_producer(v7227,v7247,(((*Kernel.range)(v7247) == _oid_(Kernel._float)) ?
                Kernel._float :
                OBJECT(ClaireClass,v7243) ),Kernel.cfalse);
              } 
          princ_string(");");
          ++v7239;
          } 
        } 
      princ_string("});");
    breakline_void();
    princ_string("");
    } 
  GC_UNBIND;} 

char * protect_result_c_producer(Generate_c_producer *v7227,ClaireClass *v7243,ClaireBoolean *v13756,OID v7248)
{ GC_BIND;
  if (Optimize.compiler->safety > 5)
   { (Optimize.OPT->protection = CFALSE);
    v13756= CFALSE;
    (Optimize.OPT->loop_gc = CFALSE);
    } 
  { char *Result ;
    { char * v11590 = ((v7243 != Kernel._void) ?
        ((v13756 == CTRUE) ?
          "GC_UNBIND; return " :
          "return " ) :
        ((v13756 == CTRUE) ?
          "GC_UNBIND;" :
          "" ) );
      ClaireObject * v7237;
      if (need_debug_ask_any(v7248) == CTRUE)
       v7237 = OBJECT(method,v7248)->module_I;
      else v7237 = CFALSE;
        if (INHERIT(v7237->isa,Kernel._module))
       { print_in_string_void();
        princ_string("DB_UNBIND(");
        ident_thing(((thing *) v7237));
        princ_string(",");
        (*Generate.expression)(GC_OID((*Kernel.selector)(v7248)),
          _oid_(Kernel.emptySet));
        princ_string(",");
        if (v7243 == Kernel._void)
         princ_string("CNULL");
        else if ((v7243 == Kernel._any) || 
            (v7243 == Kernel._integer))
         princ_string("Result");
        else to_cl_c_producer(v7227,GC_OID(_oid_(build_Variable_string("Result",_oid_(v7243)))),v7243,Core.nil->value);
          princ_string(");");
        breakline_void();
        princ_string("  ");
        princ_string(v11590);
        princ_string("");
        Result = end_of_print_void();
        } 
      else Result = (((Optimize.OPT->profile_ask == CTRUE) && 
          ((Kernel._method == OWNER(v7248)) || 
              (v7248 == CNULL))) ?
        append_string("PRend(PR_x);",v11590) :
        v11590 );
      } 
    GC_UNBIND; return (Result);} 
  } 

void  generate_function_start_c_producer(Generate_c_producer *v7227,lambda *v1140,ClaireClass *v7243,OID v7237,char *v12719)
{ GC_BIND;
  { OID  v3109;
    { if (v1140->vars->length != 0)
       v3109 = (*Kernel.range)(GC_OID((*(v1140->vars))[1]));
      else v3109 = _oid_(Kernel._any);
        GC_OID(v3109);} 
    ClaireFunction * v9226 = make_function_string(v12719);
    list * v11442 = GC_OBJECT(list,(((v1140->vars->length == 1) && 
        ((v3109 == _oid_(Kernel._environment)) || 
            (v3109 == _oid_(Kernel._void)))) ?
      Kernel.nil :
      v1140->vars ));
    update_function_entry_c_producer(v7227,v9226,v11442,v7243);
    use_as_output_port(Optimize.OPT->outfile);
    if (Kernel._method == OWNER(v7237))
     { if ((OBJECT(restriction,v7237)->range == Kernel._float) || 
          (OBJECT(restriction,v7237)->domain->memq(_oid_(Kernel._float)) == CTRUE))
       generate_float_function_c_producer(v7227,OBJECT(method,v7237),string_I_function(v9226));
      else if (INHERIT(OBJECT(restriction,v7237)->range->isa,Kernel._tuple))
       generate_tuple_function_c_producer(v7227,OBJECT(method,v7237),string_I_function(v9226));
      } 
    generate_regular_function_c_producer(v7227,
      v1140,
      v9226,
      v7243,
      v7237,
      v11442);
    } 
  GC_UNBIND;} 

void  generate_regular_function_c_producer(Generate_c_producer *v7227,lambda *v1140,ClaireFunction *v9226,ClaireClass *v7243,OID v7237,list *v11442)
{ GC_BIND;
  if (Optimize.compiler->naming != 2)
   { princ_string("\n/* The c++ function for: ");
    if (Kernel._method == OWNER(v7237))
     { print_any(_oid_(OBJECT(restriction,v7237)->selector));
      princ_string("(");
      ppvariable_list(GC_OBJECT(list,v1140->vars));
      princ_string(") [");
      if (Optimize.compiler->naming == 1)
       bitvectorSum_integer(status_I_restriction(OBJECT(restriction,v7237)));
      princ_string("]");
      } 
    else princ_string(string_I_function(v9226));
      princ_string(" */\n");
    } 
  { interface_I_c_producer(v7227,v7243);
    princ_string(" ");
    c_princ_function(v9226);
    princ_string("(");
    typed_args_list_list(v11442);
    princ_string(")\n");
    } 
  GC_UNBIND;} 

void  generate_float_function_c_producer(Generate_c_producer *v7227,method *v7237,char *v12719)
{ GC_BIND;
  { list * v11424 = (((v7237->domain->length == 1) && 
        (domain_I_restriction(v7237) == Kernel._void)) ?
      Kernel.nil :
      v7237->domain );
    int  v7239 = v11424->length;
    list * v11442;
    { { bag *v_list; OID v_val;
        OID v7248,CLcount;
        v_list = v11424;
         v11442 = v_list->clone();
        for (CLcount= 1; CLcount <= v_list->length; CLcount++)
        { v7248 = (*(v_list))[CLcount];
          { Variable * v2072 = ((Variable *) GC_OBJECT(Variable,new_object_class(Language._Variable)));
            (v2072->pname = gensym_void());
            { Variable * v6785 = v2072; 
              ClaireType * v6786;
              if (v7248 == _oid_(Kernel._float))
               v6786 = Kernel._any;
              else v6786 = OBJECT(ClaireType,v7248);
                (v6785->range = v6786);} 
            add_I_property(Kernel.instances,Language._Variable,11,_oid_(v2072));
            v_val = _oid_(v2072);
            } 
          
          (*((list *) v11442))[CLcount] = v_val;} 
        } 
      GC_OBJECT(list,v11442);} 
    char * v1168 = GC_STRING(append_string(v12719,"_"));
    ClaireFunction * v9226 = make_function_string(v1168);
    lambda * v15876 = GC_OBJECT(lambda,lambda_I_list(v11442,Core.nil->value));
    ClaireClass * v7243 = ((v7237->range == Kernel._float) ?
      Kernel._any :
      psort_any(_oid_(v7237->range)) );
    generate_regular_function_c_producer(v7227,
      v15876,
      v9226,
      v7243,
      _oid_(v7237),
      v11442);
    GC_OBJECT(list,Optimize.OPT->functions)->addFast(_oid_(list::alloc(3,_oid_(v9226),
      _oid_(v11442),
      _oid_(v7243))));
    new_block_void();
    if (v7243 != Kernel._void)
     princ_string("return ");
    if (v7237->range == Kernel._float)
     princ_string("_float_(");
    c_princ_function(OBJECT(ClaireFunction,(*Optimize.functional_I)(_oid_(v7237))));
    princ_string("(");
    { int  v7233 = 1;
      int  v6784 = v7239;
      { OID gc_local;
        while ((v7233 <= v6784))
        { if (v7233 != 1)
           princ_string(",");
          if ((*(v7237->domain))[v7233] == _oid_(Kernel._float))
           { princ_string("float_v(");
            expression_Variable(OBJECT(Variable,(*(v11442))[v7233]),Core.nil->value);
            princ_string(")");
            } 
          else expression_Variable(OBJECT(Variable,(*(v11442))[v7233]),Core.nil->value);
            ++v7233;
          } 
        } 
      } 
    princ_string(")");
    if (v7237->range == Kernel._float)
     princ_string(")");
    princ_string(";");
    close_block_void();
    princ_string("\n");
    } 
  GC_UNBIND;} 

void  at_c_producer(Generate_c_producer *v7227)
{ princ_string("->");
  } 

void  generate_tuple_function_c_producer(Generate_c_producer *v7227,method *v7237,char *v12719)
{ GC_BIND;
  { list * v11424 = (((v7237->domain->length == 1) && 
        (domain_I_restriction(v7237) == Kernel._void)) ?
      Kernel.nil :
      v7237->domain );
    int  v7239 = v11424->length;
    list * v11442;
    { { bag *v_list; OID v_val;
        OID v7248,CLcount;
        v_list = v11424;
         v11442 = v_list->clone();
        for (CLcount= 1; CLcount <= v_list->length; CLcount++)
        { v7248 = (*(v_list))[CLcount];
          { Variable * v2072 = ((Variable *) GC_OBJECT(Variable,new_object_class(Language._Variable)));
            (v2072->pname = gensym_void());
            (v2072->range = OBJECT(ClaireType,v7248));
            add_I_property(Kernel.instances,Language._Variable,11,_oid_(v2072));
            v_val = _oid_(v2072);
            } 
          
          (*((list *) v11442))[CLcount] = v_val;} 
        } 
      GC_OBJECT(list,v11442);} 
    char * v1168 = GC_STRING(append_string(v12719,"_"));
    ClaireFunction * v9226 = make_function_string(v1168);
    lambda * v15876 = GC_OBJECT(lambda,lambda_I_list(v11442,Core.nil->value));
    ClaireClass * v7243 = Kernel._tuple;
    generate_regular_function_c_producer(v7227,
      v15876,
      v9226,
      v7243,
      _oid_(v7237),
      v11442);
    GC_OBJECT(list,Optimize.OPT->functions)->addFast(_oid_(list::alloc(3,_oid_(v9226),
      _oid_(v11442),
      _oid_(v7243))));
    new_block_void();
    if (v7243 != Kernel._void)
     princ_string("return ");
    c_princ_function(OBJECT(ClaireFunction,(*Optimize.functional_I)(_oid_(v7237))));
    princ_string("(");
    { int  v7233 = 1;
      int  v6808 = v7239;
      { OID gc_local;
        while ((v7233 <= v6808))
        { if (v7233 != 1)
           princ_string(",");
          expression_Variable(OBJECT(Variable,(*(v11442))[v7233]),Core.nil->value);
          ++v7233;
          } 
        } 
      } 
    princ_string(")");
    princ_string("->copyIfNeeded()");
    princ_string(";");
    close_block_void();
    princ_string("\n");
    } 
  GC_UNBIND;} 

OID  create_function_entry_c_producer(Generate_c_producer *v7227,lambda *v11422,char *v9226,OID v7237)
{ return (Core.nil->value);} 

OID  update_function_entry_c_producer(Generate_c_producer *v7227,ClaireFunction *v9226,list *v11442,ClaireClass *v7243)
{ GC_BIND;
  GC_OBJECT(list,Optimize.OPT->functions)->addFast(_oid_(list::alloc(3,_oid_(v9226),
    _oid_(v11442),
    _oid_(v7243))));
  { OID Result = 0;
    Result = Kernel.cfalse;
    GC_UNBIND; return (Result);} 
  } 

char * c_interface_class1_Generate(ClaireClass *v1140)
{ { char *Result ;
    { int  v7239 = index_list(OBJECT(Generate_producer,Generate.PRODUCER->value)->interfaces,_oid_(v1140));
      Result = ((v7239 == 0) ?
        "OID *" :
        string_v((*(OBJECT(Generate_producer,Generate.PRODUCER->value)->interfaces))[(v7239+1)]) );
      } 
    return (Result);} 
  } 

void  c_interface_class2_Generate(ClaireClass *v1140,char *v7243)
{ GC_BIND;
  { int  v7239 = index_list(OBJECT(Generate_producer,Generate.PRODUCER->value)->interfaces,_oid_(v1140));
    if (v7239 == 0)
     (OBJECT(Generate_producer,Generate.PRODUCER->value)->interfaces = GC_OBJECT(list,GC_OBJECT(list,OBJECT(Generate_producer,Generate.PRODUCER->value)->interfaces)->addFast(_oid_(v1140)))->addFast(_string_(v7243)));
    else ((*(OBJECT(Generate_producer,Generate.PRODUCER->value)->interfaces))[(v7239+1)]=_string_(v7243));
      } 
  GC_UNBIND;} 

void  c_interface_method_Generate(method *v1140)
{ c_princ_string(string_v((*Optimize.function_name)(_oid_(v1140->selector),
    _oid_(v1140->domain),
    _oid_(v1140->functional))));
  } 

void  interface_I_c_producer(Generate_c_producer *v7227,ClaireClass *v1140)
{ if (v1140 == Kernel._void)
   princ_string("void ");
  else if (v1140 == Kernel._integer)
   princ_string("int ");
  else if ((v1140 == Kernel._function) || 
      ((v1140 == Kernel._char) || 
        (v1140 == Kernel._boolean)))
   { ident_c_producer(v7227,v1140->name);
    princ_string(" *");
    } 
  else if ((INHERIT(v1140,Kernel._cl_import)) || 
      ((v1140 == Kernel._string) || 
        (v1140 == Kernel._array)))
   princ_string(string_v((*Optimize.c_interface)(_oid_(v1140))));
  else if (INHERIT(v1140,Kernel._object))
   { class_princ_c_producer(v7227,v1140);
    princ_string(" *");
    } 
  else if (INHERIT(v1140,Kernel._bag))
   { ident_c_producer(v7227,v1140->name);
    princ_string(" *");
    } 
  else if (v1140 == Kernel._float)
   princ_string("double ");
  else princ_string("OID ");
    } 

void  to_cl_c_producer(Generate_c_producer *v7227,OID v7248,ClaireClass *v7243,OID v15308)
{ if (v7243 == Kernel._void)
   { princ_string("_void_(");
    (*Generate.expression)(v7248,
      v15308);
    princ_string(")");
    } 
  else if (INHERIT(v7243,Kernel._object))
   { if (v7248 == Kernel.ctrue)
     princ_string("Kernel.ctrue");
    else if (v7248 == Kernel.cfalse)
     princ_string("Kernel.cfalse");
    else { princ_string("_oid_(");
        (*Generate.expression)(v7248,
          v15308);
        princ_string(")");
        } 
      } 
  else if (v7243 == Kernel._integer)
   { if (Optimize.compiler->safety > 1)
     (*Generate.expression)(v7248,
      v15308);
    else { princ_string("_integer_(");
        (*Generate.expression)(v7248,
          v15308);
        princ_string(")");
        } 
      } 
  else if (v7243 == Kernel._char)
   { princ_string("_oid_(");
    (*Generate.expression)(v7248,
      v15308);
    princ_string(")");
    } 
  else if (v7243 == Kernel._string)
   { princ_string("_string_(");
    (*Generate.expression)(v7248,
      v15308);
    princ_string(")");
    } 
  else if (v7243 == Kernel._array)
   { princ_string("_array_(");
    (*Generate.expression)(v7248,
      v15308);
    princ_string(")");
    } 
  else if (v7243 == Kernel._float)
   { princ_string("_float_(");
    (*Generate.expression)(v7248,
      v15308);
    princ_string(")");
    } 
  else if (INHERIT(v7243,Kernel._cl_import))
   { princ_string("ClAlloc->import(");
    expression_any(_oid_(v7243),_oid_(Kernel.emptySet));
    princ_string(",(int *) ");
    (*Generate.expression)(v7248,
      v15308);
    princ_string(")");
    } 
  else if (v7243 == Kernel._any)
   (*Generate.expression)(v7248,
    v15308);
  else close_exception(((general_error *) (*Core._general_error)(_string_("[internal] to_cl for a ~S is not implemented"),
      _oid_(list::alloc(1,_oid_(v7243))))));
    } 

void  to_c_c_producer1(Generate_c_producer *v7227,OID v7248,ClaireClass *v7243,OID v15308)
{ if (v7243 == Kernel._integer)
   (*Generate.expression)(v7248,
    v15308);
  else if (v7248 == CNULL)
   princ_string("NULL");
  else if ((INHERIT(OWNER(v7248),Core._global_variable)) && 
      ((equal((*Kernel.range)(v7248),_oid_(Kernel.emptySet)) == CTRUE) && 
        (equal((*Kernel.value)(v7248),Core.nil->value) == CTRUE)))
   princ_string("Kernel.nil");
  else { to_c_c_producer2(v7227,v7243);
      (*Generate.expression)(v7248,
        v15308);
      princ_string(")");
      } 
    } 

void  to_c_c_producer2(Generate_c_producer *v7227,ClaireClass *v7243)
{ if (INHERIT(v7243,Kernel._object))
   { princ_string("OBJECT(");
    class_princ_class(v7243);
    princ_string(",");
    } 
  else if (v7243 == Kernel._float)
   princ_string("float_v(");
  else if (v7243 == Kernel._char)
   princ_string("char_v(");
  else if (v7243 == Kernel._string)
   princ_string("string_v(");
  else if (v7243 == Kernel._array)
   princ_string("array_v(");
  else if (INHERIT(v7243,Kernel._cl_import))
   { princ_string("EXPORT((");
    interface_I_class(v7243);
    princ_string("),");
    } 
  else close_exception(((general_error *) (*Core._general_error)(_string_("[internal] to_c for a ~S is not implemented"),
      _oid_(list::alloc(1,_oid_(v7243))))));
    } 

void  public_static_c_producer(Generate_c_producer *v7227)
{ ;} 

void  bool_exp_I_c_producer(Generate_c_producer *v7227,OID v1140,OID v15308)
{ if (INHERIT(OWNER(v1140),Optimize._to_CL))
   { princ_string("(");
    (*Optimize.bool_exp)(v1140,
      Kernel.ctrue,
      v15308);
    princ_string(" ? Kernel.ctrue : Kernel.cfalse)");
    } 
  else { princ_string("(");
      (*Optimize.bool_exp)(v1140,
        Kernel.ctrue,
        v15308);
      princ_string(" ? CTRUE : CFALSE)");
      } 
    } 

void  inherit_exp_c_producer(Generate_c_producer *v7227,OID v11032,OID v11033,OID v15308)
{ princ_string("INHERIT(");
  (*Generate.expression)(v11032,
    v15308);
  princ_string(",");
  (*Generate.expression)(v11033,
    v15308);
  princ_string(")");
  } 

void  bitvector_exp_c_producer(Generate_c_producer *v7227,OID v11032,OID v11033,OID v15308)
{ princ_string("BCONTAIN(");
  (*Generate.expression)(v11032,
    v15308);
  princ_string(",");
  (*Generate.expression)(v11033,
    v15308);
  princ_string(")");
  } 

void  equal_exp_c_producer(Generate_c_producer *v7227,OID v11032,ClaireBoolean *v3475,OID v11033,OID v7260)
{ GC_BIND;
  if ((INHERIT(OWNER(v11032),Optimize._to_CL)) && 
      ((INHERIT(OWNER(v11033),Optimize._to_CL)) && 
        ((osort_any(GC_OID((*Language.set_arg)(v11032))) == osort_any(GC_OID((*Language.set_arg)(v11033)))) && 
          ((identifiable_ask_any(GC_OID((*Kernel.arg)(v11032))) == CTRUE) || 
              ((identifiable_ask_any(GC_OID((*Kernel.arg)(v11033))) == CTRUE) || 
                (((*Language.set_arg)(v11032) == _oid_(Kernel._string)) || 
                  ((*Language.set_arg)(v11032) == _oid_(Kernel._float))))))))
   equal_exp_c_producer(v7227,
    GC_OID((*Kernel.arg)(v11032)),
    v3475,
    GC_OID((*Kernel.arg)(v11033)),
    Kernel.ctrue);
  else if ((INHERIT(OWNER(v11032),Optimize._to_protect)) && 
      ((INHERIT(owner_any((*Kernel.arg)(v11032)),Optimize._to_CL)) && 
        (((*Optimize.c_gc_ask)(GC_OID((*Kernel.arg)(GC_OID((*Kernel.arg)(v11032))))) != Kernel.ctrue) && 
          ((INHERIT(OWNER(v11033),Optimize._to_protect)) && 
            ((INHERIT(owner_any((*Kernel.arg)(v11033)),Optimize._to_CL)) && 
              ((*Optimize.c_gc_ask)(GC_OID((*Kernel.arg)(GC_OID((*Kernel.arg)(v11033))))) != Kernel.ctrue))))))
   equal_exp_c_producer(v7227,
    GC_OID((*Kernel.arg)(v11032)),
    v3475,
    GC_OID((*Kernel.arg)(v11033)),
    v7260);
  else { ClaireBoolean * g0072I;
    { ClaireBoolean *v_and;
      { v_and = (((*Optimize.c_sort)(v11032) == _oid_(Kernel._string)) ? CTRUE : CFALSE);
        if (v_and == CFALSE) g0072I =CFALSE; 
        else { v_and = (((*Optimize.c_sort)(v11033) == _oid_(Kernel._string)) ? CTRUE : CFALSE);
          if (v_and == CFALSE) g0072I =CFALSE; 
          else { { OID  v10875;
              if (INHERIT(OWNER(v11033),Optimize._to_C))
               v10875 = ((OBJECT(Compile_to_C,v11033)->arg == CNULL) ? Kernel.ctrue : Kernel.cfalse);
              else v10875 = Kernel.cfalse;
                v_and = not_any(v10875);
              } 
            if (v_and == CFALSE) g0072I =CFALSE; 
            else g0072I = CTRUE;} 
          } 
        } 
      } 
    
    if (g0072I == CTRUE) { princ_string("(equal_string(");
        (*Generate.expression)(v11032,
          Core.nil->value);
        princ_string(",");
        (*Generate.expression)(v11033,
          Core.nil->value);
        princ_string(") ");
        sign_equal_boolean(v3475);
        princ_string(" CTRUE)");
        } 
      else if ((INHERIT(OWNER(v11032),Optimize._to_CL)) && 
        (((*Language.set_arg)(v11032) != _oid_(Kernel._integer)) && 
          (v11033 == CNULL)))
     { princ_string("(");
      (*Generate.expression)(GC_OID((*Kernel.arg)(v11032)),
        Core.nil->value);
      princ_string(" ");
      sign_equal_boolean(v3475);
      princ_string(" NULL)");
      } 
    else if ((char_exp_ask_c_producer2(v7227,v11032) == CTRUE) || 
        (char_exp_ask_c_producer2(v7227,v11033) == CTRUE))
     { princ_string("(");
      char_exp_c_producer2(v7227,v11032,Core.nil->value);
      princ_string(" ");
      sign_equal_boolean(v3475);
      princ_string(" ");
      char_exp_c_producer2(v7227,v11033,Core.nil->value);
      princ_string(")");
      } 
    else if ((boolean_I_any(v7260) == CTRUE) || 
        ((identifiable_ask_any(v11032) == CTRUE) || 
          ((identifiable_ask_any(v11033) == CTRUE) || 
            ((*Optimize.c_sort)(v11032) == _oid_(Kernel._float)))))
     { if (equal(_oid_(glb_class(stupid_t_any1(v11032),stupid_t_any1(v11033))),_oid_(Kernel.emptySet)) == CTRUE)
       { warn_void();
        tformat_string("~S = ~S will fail ! [263]",2,list::alloc(2,v11032,v11033));
        } 
      princ_string("(");
      bexpression_any(v11032,Core.nil->value);
      princ_string(" ");
      sign_equal_boolean(v3475);
      princ_string(" ");
      bexpression_any(v11033,Core.nil->value);
      princ_string(")");
      } 
    else { princ_string("(equal(");
        (*Generate.expression)(v11032,
          Core.nil->value);
        princ_string(",");
        (*Generate.expression)(v11033,
          Core.nil->value);
        princ_string(") ");
        sign_equal_boolean(v3475);
        princ_string(" CTRUE)");
        } 
      } 
  GC_UNBIND;} 

ClaireBoolean * char_exp_ask_c_producer2(Generate_c_producer *v7227,OID v7248)
{ { ClaireBoolean *Result ;
    if (INHERIT(OWNER(v7248),Kernel._char))
     Result = CTRUE;
    else if (INHERIT(OWNER(v7248),Language._Call_method))
     { method * v7237 = OBJECT(Call_method,v7248)->arg;
      Result = ((_oid_(v7237) == Generate._starnth_1_string_star->value) ? CTRUE : (((_oid_(v7237) == Generate._starnth_string_star->value) && 
          (2 <= Optimize.compiler->safety)) ? CTRUE : CFALSE));
      } 
    else Result = CFALSE;
      return (Result);} 
  } 

void  char_exp_c_producer2(Generate_c_producer *v7227,OID v7248,OID v15308)
{ GC_BIND;
  if (INHERIT(OWNER(v7248),Kernel._char))
   { princ_string("'");
    if ((v7248 == _oid_(_char_('\"'))) || 
        ((v7248 == _oid_(_char_('\''))) || 
          ((v7248 == _oid_(_char_('\?'))) || 
            (v7248 == _oid_(_char_('\\'))))))
     princ_string("\\");
    princ_char(OBJECT(ClaireChar,v7248));
    princ_string("'");
    } 
  else if (INHERIT(OWNER(v7248),Language._Call_method))
   { method * v7237 = OBJECT(Call_method,v7248)->arg;
    if ((_oid_(v7237) == Generate._starnth_1_string_star->value) || 
        (_oid_(v7237) == Generate._starnth_string_star->value))
     { OID  v11032 = (*(OBJECT(Call_method,v7248)->args))[1];
      OID  v11033 = (*(OBJECT(Call_method,v7248)->args))[2];
      (*Generate.expression)(v11032,
        v15308);
      princ_string("[");
      (*Generate.expression)(v11033,
        v15308);
      princ_string(" - 1]");
      } 
    else { princ_string("((char) ");
        (*Generate.expression)(v7248,
          v15308);
        princ_string("->ascii)");
        } 
      } 
  else { princ_string("((char) ");
      (*Generate.expression)(v7248,
        v15308);
      princ_string("->ascii)");
      } 
    GC_UNBIND;} 

void  c_member_c_producer(Generate_c_producer *v7227,OID v1140,ClaireClass *v7243,property *v7248,OID v15308)
{ if (INHERIT(stupid_t_any1(v1140),v7243))
   { (*Generate.expression)(v1140,
      v15308);
    princ_string("->");
    ident_symbol(v7248->name);
    princ_string("");
    } 
  else { princ_string("CLREAD(");
      class_princ_class(v7243);
      princ_string(",");
      (*Generate.expression)(v1140,
        v15308);
      princ_string(",");
      ident_symbol(v7248->name);
      princ_string(")");
      } 
    } 

void  addFast_c_producer(Generate_c_producer *v7227)
{ princ_string("->addFast");
  } 

void  cast_I_c_producer(Generate_c_producer *v7227,Compile_C_cast *v1140,OID v15308)
{ GC_BIND;
  princ_string("((");
  class_princ_class(v1140->set_arg);
  princ_string(" *) ");
  (*Generate.expression)(v1140->arg,
    v15308);
  princ_string(")");
  GC_UNBIND;} 

void  gc_protection_exp_c_producer(Generate_c_producer *v7227,Variable *v7247,ClaireBoolean *v12010,OID v7245,OID v15308)
{ GC_BIND;
  (v7227->stat = (v7227->stat+1));
  if ((osort_any(GC_OID(_oid_(v7247->range))) == Kernel._float) || 
      (osort_any(GC_OID(_oid_(v7247->range))) == Kernel._integer))
   { princ_string("(");
    ident_c_producer3(v7227,v7247);
    princ_string("=");
    if (v12010 == CTRUE)
     (*Generate.expression)(v7245,
      v15308);
    else c_princ_string(string_v(v7245));
      princ_string(")");
    } 
  else { princ_string("GC__");
      princ_string(((osort_any(GC_OID(_oid_(v7247->range))) == Kernel._any) ?
        "OID" :
        ((osort_any(GC_OID(_oid_(v7247->range))) == Kernel._string) ?
          "STRING" :
          "ANY" ) ));
      princ_string("(");
      ident_c_producer3(v7227,v7247);
      if (v7245 != _oid_(v7247))
       { princ_string(" = ");
        if (v12010 == CTRUE)
         (*Generate.expression)(v7245,
          v15308);
        else c_princ_string(string_v(v7245));
          } 
      princ_string(", ");
      princ_integer(((Optimize.OPT->loop_index == 0) ?
        1 :
        (v7247->index+1) ));
      princ_string(")");
      } 
    GC_UNBIND;} 

void  bag_expression_c_producer(Generate_c_producer *v11165,ClaireClass *v7227,bag *v7236,ClaireType *v7244,OID v15308)
{ GC_BIND;
  if (v7236->length == 0)
   { print_any(_oid_(v7227));
    princ_string("::empty(");
    if (v7244 != Kernel._void)
     { (*Generate.expression)(GC_OID((*Optimize.c_code)(_oid_(v7244),
          _oid_(Kernel._object))),
        v15308);
      } 
    princ_string(")");
    } 
  else { print_any(_oid_(v7227));
      princ_string("::alloc");
      if ((v7227 == Kernel._tuple) && 
          (Optimize.OPT->alloc_stack == CTRUE))
       princ_string("Stack");
      princ_string("(");
      if (v7244 != Kernel._void)
       { (*Generate.expression)(GC_OID((*Optimize.c_code)(_oid_(v7244),
            _oid_(Kernel._object))),
          v15308);
        princ_string(",");
        } 
      princ_integer(v7236->length);
      princ_string(",");
      args_list_bag(v7236,v15308,_sup_integer(v7236->length,2));
      princ_string(")");
      } 
    GC_UNBIND;} 

void  generate_s_file_string(char *v1140,list *v7236,OID v7237)
{ GC_BIND;
  { ClairePort * v7240 = fopen_string(append_string(GC_STRING(append_string(GC_STRING(_7_string(GC_STRING(Optimize.compiler->source),v1140)),"-s")),GC_STRING(OBJECT(Generate_producer,Generate.PRODUCER->value)->extension)),"w");
    list * v4824 = GC_OBJECT(list,add_modules_list(v7236));
    list * v10516 = GC_OBJECT(list,parents_list(v7236));
    (Optimize.OPT->cinterface = v7240);
    (Optimize.OPT->properties = set::empty(Kernel._property));
    (Optimize.OPT->objects = list::empty(Kernel._any));
    (Optimize.OPT->functions = list::empty(Kernel._any));
    (Optimize.OPT->need_to_close = set::empty(Kernel._any));
    (Optimize.OPT->legal_modules = set_I_bag(v10516));
    use_as_output_port(v7240);
    princ_string("// --- System configuration file for ");
    print_any(_string_(v1140));
    princ_string(" , [");
    print_any(_string_(date_I_integer(1)));
    princ_string("] ---\n\n");
    princ_string("#include <claire.h>\n");
    princ_string("#include <Kernel.h>\n");
    { ITERATE(v7248);
      for (START(v4824); NEXT(v7248);)
      if (OBJECT(module,v7248)->made_of->length != 0)
       { princ_string("#include <");
        ident_symbol(OBJECT(symbol,(*Kernel.name)(v7248)));
        princ_string(".h>\n");
        } 
      } 
    create_load_modules_string(v1140,v7240,v10516,v7237);
    if ((boolean_I_any(_oid_(_at_property1(Core.main,Kernel._list))) == CTRUE) && 
        (domain_I_restriction(GC_OBJECT(restriction,((restriction *) _at_property1(Core.main,Kernel._list)))) == Kernel._list))
     { char * v7243 = "main_list";
      princ_string("\nextern void ");
      princ_string(v7243);
      princ_string("(list *l);\n");
      princ_string("void call_main() {");
      princ_string(v7243);
      princ_string("(ClEnv->params);}\n");
      } 
    else princ_string("\nvoid call_main() {default_main();}\n");
      fclose_port(v7240);
    } 
  GC_UNBIND;} 

void  create_load_modules_string(char *v1140,ClairePort *v7240,list *v10516,OID v7237)
{ GC_BIND;
  breakline_void();
  { OID gc_local;
    ITERATE(v7248);
    for (START(v10516); NEXT(v7248);)
    if ((*Kernel.status)(v7248) == 5)
     { princ_string("void load_");
      ident_symbol(OBJECT(symbol,(*Kernel.name)(v7248)));
      princ_string("() {");
      ident_symbol(OBJECT(symbol,(*Kernel.name)(v7248)));
      princ_string(".metaLoad();}\n");
      } 
    } 
  princ_string("\n\nvoid loadModules() \n");
  new_block_void();
  princ_string("//module definitions ");
  breakline_void();
  princ_string("");
  { OID gc_local;
    ITERATE(v7248);
    bag *v7248_support;
    v7248_support = GC_OBJECT(set,_backslash_type(v10516,GC_OBJECT(set,set::alloc(Kernel.emptySet,3,_oid_(claire.it),
      _oid_(mClaire.it),
      _oid_(Kernel.it)))));
    for (START(v7248_support); NEXT(v7248);)
    { GC_LOOP;
      { ident_symbol(OBJECT(symbol,(*Kernel.name)(v7248)));
        princ_string(".initModule(");
        print_any(_string_(string_I_symbol(OBJECT(symbol,(*Kernel.name)(v7248)))));
        princ_string(",");
        expression_thing(OBJECT(module,v7248)->part_of,Core.nil->value);
        princ_string(",");
        (*Generate.expression)(GC_OID((*Optimize.c_code)(GC_OID(_oid_(OBJECT(module,v7248)->uses)),
            _oid_(Kernel._list))),
          Core.nil->value);
        princ_string(",");
        breakline_void();
        print_any(GC_OID((*Kernel.source)(v7248)));
        princ_string(",");
        (*Generate.expression)(GC_OID((*Optimize.c_code)(GC_OID(_oid_(OBJECT(module,v7248)->made_of)),
            _oid_(Kernel._list))),
          Core.nil->value);
        princ_string(");");
        breakline_void();
        princ_string("");
        } 
      GC_UNLOOP;} 
    } 
  princ_string("//module load ");
  breakline_void();
  princ_string("");
  { OID gc_local;
    ITERATE(v7248);
    for (START(v10516); NEXT(v7248);)
    if ((OBJECT(module,v7248)->made_of->length != 0) && 
        ((*Kernel.status)(v7248) != 5))
     { ident_symbol(OBJECT(symbol,(*Kernel.name)(v7248)));
      princ_string(".metaLoad();");
      breakline_void();
      princ_string("");
      } 
    } 
  { OID gc_local;
    ITERATE(v7248);
    for (START(v10516); NEXT(v7248);)
    if ((*Kernel.status)(v7248) == 5)
     { ident_symbol(OBJECT(symbol,(*Kernel.name)(v7248)));
      princ_string(".it->evaluate = ");
      expression_any(_oid_(make_function_string(append_string("load_",string_I_symbol(OBJECT(symbol,(*Kernel.name)(v7248)))))),Kernel.cfalse);
      breakline_void();
      princ_string("");
      ident_symbol(OBJECT(symbol,(*Kernel.name)(v7248)));
      princ_string(".it->status = 2;");
      breakline_void();
      princ_string("");
      } 
    } 
  princ_string("ClEnv->module_I = ");
  { OID  v12797;
    if (INHERIT(OWNER(v7237),Kernel._module))
     v12797 = v7237;
    else v12797 = _oid_(claire.it);
      (*Generate.expression)(v12797,
      Core.nil->value);
    } 
  princ_string("; ");
  breakline_void();
  princ_string("");
  if (Optimize.compiler->safety > 5)
   princ_string("ClAlloc->statusGC = 2;\n");
  if (Optimize.OPT->profile_ask == CTRUE)
   { OID gc_local;
    ITERATE(v7240);
    for (START(Reader.PRdependent->graph); NEXT(v7240);)
    if (INHERIT(OWNER(v7240),Kernel._property))
     { OID gc_local;
      ITERATE(v11498);
      bag *v11498_support;
      v11498_support = OBJECT(bag,nth_table1(Reader.PRdependent,v7240));
      for (START(v11498_support); NEXT(v11498);)
      if ((contain_ask_set(Optimize.OPT->to_remove,v11498) != CTRUE) && 
          (OBJECT(thing,v7240)->name->definition == OBJECT(thing,v11498)->name->definition))
       { princ_string("PRdepends_property(");
        expression_thing(OBJECT(thing,v7240),Core.nil->value);
        princ_string(",");
        expression_thing(OBJECT(thing,v11498),Core.nil->value);
        princ_string(");\n");
        } 
      } 
    } 
  close_block_void();
  GC_UNBIND;} 

void  methods_interface_c_producer(Generate_c_producer *v7227,ClaireClass *v7248)
{ GC_RESERVE(6);  // v3.0.55 optim !
  { OID gc_local;
    ITERATE(v7240);
    bag *v7240_support;
    v7240_support = OBJECT(bag,nth_table1(Language.InterfaceList,_oid_(v7248)));
    for (START(v7240_support); NEXT(v7240);)
    { GC_LOOP;
      { ClaireObject * v7237 = GC_OBJECT(ClaireObject,_at_property1(OBJECT(property,v7240),v7248));
        if ((Kernel._method == v7237->isa) && 
            (get_property(Kernel.formula,v7237) != CNULL))
         { ClaireClass * v7243 = class_I_type(GC_OBJECT(ClaireType,OBJECT(ClaireType,(*Kernel.range)(_oid_(v7237)))));
          list * v13225 = GC_OBJECT(list,cdr_list(OBJECT(lambda,(*Kernel.formula)(_oid_(v7237)))->vars));
          breakline_void();
          interface_I_c_producer(v7227,v7243);
          princ_string(" ");
          (*Kernel.c_princ)(GC_OID((*Kernel.name)(v7240)));
          princ_string("(");
          typed_args_list_list(v13225);
          princ_string(");  ");
          } 
        } 
      GC_UNLOOP;} 
    } 
  GC_UNBIND;} 

void  methods_bodies_c_producer(Generate_c_producer *v7227,ClaireClass *v7248)
{ GC_RESERVE(6);  // v3.0.55 optim !
  { OID gc_local;
    ITERATE(v7240);
    bag *v7240_support;
    v7240_support = OBJECT(bag,nth_table1(Language.InterfaceList,_oid_(v7248)));
    for (START(v7240_support); NEXT(v7240);)
    { GC_LOOP;
      { ClaireObject * v7237 = GC_OBJECT(ClaireObject,_at_property1(OBJECT(property,v7240),v7248));
        if ((Kernel._method == v7237->isa) && 
            (get_property(Kernel.formula,v7237) != CNULL))
         { ClaireClass * v7243 = class_I_type(GC_OBJECT(ClaireType,OBJECT(ClaireType,(*Kernel.range)(_oid_(v7237)))));
          list * v13225 = GC_OBJECT(list,cdr_list(OBJECT(lambda,(*Kernel.formula)(_oid_(v7237)))->vars));
          breakline_void();
          princ_string("// interface method ");
          breakline_void();
          princ_string("");
          breakline_void();
          interface_I_c_producer(v7227,v7243);
          princ_string(" ");
          class_princ_class(v7248);
          princ_string("::");
          (*Kernel.c_princ)(GC_OID((*Kernel.name)(v7240)));
          princ_string("(");
          typed_args_list_list(v13225);
          princ_string(")");
          breakline_void();
          princ_string("  ");
          princ_string("\t{ ");
          if (v7243 != Kernel._void)
           { princ_string("return (");
            interface_I_c_producer(v7227,v7243);
            princ_string(") ");
            } 
          if ((OBJECT(ClaireBoolean,(*Kernel._sup)(GC_OID((*Kernel.dispatcher)(v7240)),
            0))) == CTRUE)
           { (*Generate.expression)(v7240,
              Core.nil->value);
            princ_string("->fcall((int) this");
            { OID gc_local;
              ITERATE(v7247);
              for (START(v13225); NEXT(v7247);)
              { princ_string(",(int) ");
                (*Generate.expression)(v7247,
                  Core.nil->value);
                } 
              } 
            princ_string(")");
            } 
          else { c_princ_function(OBJECT(ClaireFunction,(*Optimize.functional_I)(_oid_(v7237))));
              princ_string("(this");
              { OID gc_local;
                ITERATE(v7247);
                for (START(v13225); NEXT(v7247);)
                { princ_string(",");
                  (*Generate.expression)(v7247,
                    Core.nil->value);
                  } 
                } 
              princ_string(")");
              } 
            princ_string(";}\n");
          breakline_void();
          princ_string("");
          } 
        } 
      GC_UNLOOP;} 
    } 
  GC_UNBIND;} 

