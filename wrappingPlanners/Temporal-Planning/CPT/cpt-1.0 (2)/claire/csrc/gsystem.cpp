/***** CLAIRE Compilation of file c:\claire\v3.3\src\compile\gsystem.cl 
         [version 3.3.4 / safety 5] Sat Oct 16 06:53:38 2004 *****/

#include <claire.h>
#include <Kernel.h>
#include <Core.h>
#include <Language.h>
#include <Reader.h>
#include <Optimize.h>
#include <Generate.h>
void  ident_symbol(symbol *v1140)
{ (*Language.ident)(Generate.PRODUCER->value,
    _oid_(v1140));
  } 

void  ident_thing(thing *v1140)
{ (*Language.ident)(Generate.PRODUCER->value,
    _oid_(v1140->name));
  } 

void  ident_class(ClaireClass *v1140)
{ (*Language.ident)(Generate.PRODUCER->value,
    _oid_(v1140->name));
  } 

void  interface_I_class(ClaireClass *v7227)
{ (*Generate.interface_I)(Generate.PRODUCER->value,
    _oid_(v7227));
  } 

void  class_princ_class(ClaireClass *v7227)
{ (*Generate.class_princ)(Generate.PRODUCER->value,
    _oid_(v7227));
  } 

OID  indent_c_void()
{ { OID Result = 0;
    { int  v7248 = Optimize.OPT->level;
      { Result= _oid_(CFALSE);
        while ((v7248 > 0))
        { princ_string("  ");
          v7248= (v7248-1);
          } 
        } 
      } 
    return (Result);} 
  } 

OID  breakline_void()
{ princ_string("\n");
  return (indent_c_void());} 

void  new_block_void()
{ (Optimize.OPT->level = (Optimize.OPT->level+1));
  princ_string("{ ");
  } 

void  close_block_void()
{ (Optimize.OPT->level = (Optimize.OPT->level-1));
  princ_string("} ");
  breakline_void();
  } 

void  c_test_any(OID v7248)
{ GC_BIND;
  { OID  v7244 = GC_OID((*Optimize.c_type)(v7248));
    ClaireClass * v7243 = osort_any(v7244);
    OID  v7245 = GC_OID((*Optimize.c_code)(v7248,
      _oid_(v7243)));
    ClaireBoolean * v7230 = c_func_any(v7245);
    princ_string("type -> ");
    print_any(v7244);
    princ_string(" [sort ");
    print_any(_oid_(v7243));
    princ_string("]\n");
    princ_string("opt[");
    print_any(_oid_(OWNER(v7245)));
    princ_string("] -> ");
    print_any(v7245);
    princ_string(" \n");
    if (v7230 == CTRUE)
     { princ_string("exp  -> ");
      (*Generate.expression)(v7245,
        Core.nil->value);
      princ_string("\n");
      } 
    else { princ_string("stat -> ");
        statement_any(v7245,_string_("result"),Core.nil->value);
        princ_string("\n");
        } 
      } 
  GC_UNBIND;} 

void  c_test_method(method *v7237)
{ GC_BIND;
  { lambda * v7236 = GC_OBJECT(lambda,v7237->formula);
    if (v7236 == (NULL))
     ;else { tformat_string("---- Compiling ~S ---- \n",0,list::alloc(1,_oid_(v7237)));
        (Optimize.OPT->in_method = _oid_(v7237));
        (Optimize.OPT->protection = CFALSE);
        (Optimize.OPT->allocation = CFALSE);
        if (Optimize.OPT->loop_index > 0)
         (Optimize.OPT->loop_index = 0);
        (Optimize.OPT->loop_gc = CFALSE);
        (Optimize.OPT->use_update = CFALSE);
        (Optimize.OPT->use_nth_equal = CFALSE);
        (Optimize.OPT->max_vars = 0);
        (Optimize.OPT->online_ask = CFALSE);
        (Optimize.OPT->legal_modules = ((set *) Kernel._module->instances));
        (OBJECT(Generate_producer,Generate.PRODUCER->value)->body = v7236->body);
        { ClaireClass * v7243 = Kernel._void;
          OID  v3871;
          { { v7243= check_sort_method(v7237);
              v3871 = OBJECT(Generate_producer,Generate.PRODUCER->value)->body;
              } 
            GC_OID(v3871);} 
          char * v4031 = GC_STRING(string_v((*Generate.protect_result)(Generate.PRODUCER->value,
            _oid_(v7243),
            _oid_(Optimize.OPT->protection),
            _oid_(v7237))));
          princ_string("Opt => ");
          print_any(v3871);
          princ_string(" \n\n");
          new_block_void();
          (*Generate.print_body)(v3871,
            _string_(v4031),
            _oid_(v7243),
            GC_OID(v7236->body),
            Kernel.ctrue);
          } 
        (Optimize.OPT->in_method = CNULL);
        } 
      } 
  GC_UNBIND;} 

void  compile_module(module *v1140)
{ GC_BIND;
  (Optimize.OPT->need_modules = Kernel.emptySet);
  { bag * v11373 = GC_OBJECT(list,parents_list(GC_OBJECT(list,add_modules_list(list::alloc(1,_oid_(v1140))))));
    update_property(Optimize.legal_modules,
      Optimize.OPT,
      9,
      Kernel._object,
      GC_OID((*Kernel.set_I)(_oid_(v11373))));
    if (Optimize.compiler->class2file_ask == CTRUE)
     { tformat_string("**** Compiling the module ~A [v. 3.~A - verbose:~A opt:~S] \n",2,list::alloc(4,_string_(string_I_symbol(v1140->name)),
        GC_OID(Optimize.compiler->version),
        ClEnv->verbose,
        _oid_(Optimize.compiler->optimize_ask)));
      write_property(Generate.outmodule,Optimize.OPT,GC_OID(ClAlloc->import(Kernel._port,(int *) fopen_string(append_string(GC_STRING(append_string(GC_STRING(append_string(GC_STRING(Optimize.compiler->source),GC_STRING(string_v(Reader._starfs_star->value)))),GC_STRING(string_v((*Generate.c_string)(Generate.PRODUCER->value,
        _oid_(v1140->name)))))),GC_STRING(OBJECT(Generate_producer,Generate.PRODUCER->value)->extension)),"w"))));
      generate_files_module(v1140);
      begin_module(v1140);
      generate_classes_module(v1140);
      generate_c2f_module(v1140);
      end_module(v1140);
      } 
    else { generate_files_module(v1140);
        generate_f2f_module(v1140);
        generate_interface_module(v1140,GC_OBJECT(set,Optimize.OPT->legal_modules));
        } 
      v11373= GC_OBJECT(set,difference_set(GC_OBJECT(set,set_I_set(Optimize.OPT->need_modules)),GC_OBJECT(set,Optimize.OPT->legal_modules)));
    tformat_string("++++ v3.3.32 info: ~A GC protection inserted. \n",1,list::alloc(1,OBJECT(Generate_producer,Generate.PRODUCER->value)->stat));
    if (v11373->length != 0)
     tformat_string("---- WARNING: ~S should be declared for ~S\n",2,list::alloc(2,_oid_(v11373),_oid_(v1140)));
    } 
  GC_UNBIND;} 

void  generate_files_module(module *v1140)
{ GC_BIND;
  tformat_string("==== Generate ~A files for module ~S [verbose = ~A, Opt? = ~S] \n",0,list::alloc(4,GC_OID(_string_(OBJECT(Generate_producer,Generate.PRODUCER->value)->comment)),
    _oid_(v1140),
    ClEnv->verbose,
    _oid_(Optimize.compiler->optimize_ask)));
  (Optimize.OPT->instructions = list::empty(Kernel._any));
  (Optimize.OPT->properties = set::empty(Kernel._property));
  (Optimize.OPT->objects = list::empty(Kernel._object));
  (Optimize.OPT->functions = list::empty(Kernel._any));
  (Optimize.OPT->need_to_close = set::empty(Kernel._any));
  (*Generate.start_module_interface)(Generate.PRODUCER->value,
    _oid_(v1140));
  begin_module(v1140);
  { OID gc_local;
    ITERATE(v7248);
    bag *v7248_support;
    v7248_support = GC_OBJECT(list,v1140->made_of);
    for (START(v7248_support); NEXT(v7248);)
    { GC_LOOP;
      { tformat_string("++++ Compiling the file ~A.cl [v. 3.~A - safety:~A] \n",2,list::alloc(3,v7248,
          GC_OID(Optimize.compiler->version),
          Optimize.compiler->safety));
        if (equal(v7248,_string_(string_I_symbol(v1140->name))) == CTRUE)
         (*Optimize.Cerror)(_string_("[211]  ~S cannot be used both as a file and module name"),
          v7248);
        generate_file_string2(GC_STRING(append_string(GC_STRING(append_string(GC_STRING(v1140->source),GC_STRING(string_v(Reader._starfs_star->value)))),string_v(v7248))),GC_STRING(append_string(GC_STRING(append_string(GC_STRING(Optimize.compiler->source),GC_STRING(string_v(Reader._starfs_star->value)))),string_v(v7248))));
        if (CFALSE == CTRUE)
         claire_gc();
        } 
      GC_UNLOOP;} 
    } 
  end_module(v1140);
  GC_UNBIND;} 

void  generate_f2f_module(module *v7237)
{ GC_BIND;
  { ClairePort * v7240 = fopen_string(append_string(GC_STRING(append_string(GC_STRING(append_string(GC_STRING(Optimize.compiler->source),GC_STRING(string_v(Reader._starfs_star->value)))),string_I_symbol(v7237->name))),GC_STRING(OBJECT(Generate_producer,Generate.PRODUCER->value)->extension)),"w");
    tformat_string("==== generate file for module ~S ==== \n",0,list::alloc(1,_oid_(v7237)));
    (Optimize.OPT->outfile = v7240);
    generate_file_string1(string_I_symbol(v7237->name),v7237);
    } 
  GC_UNBIND;} 

void  generate_file_string1(char *v7243,module *v7237)
{ GC_BIND;
  start_file_string(v7243,v7237);
  use_as_output_port(Optimize.OPT->outfile);
  (*Generate.generate_classes)(Generate.PRODUCER->value,
    _string_(v7243),
    _oid_(v7237));
  { OID gc_local;
    ITERATE(v7227);
    bag *v7227_support;
    v7227_support = GC_OBJECT(list,Optimize.OPT->objects);
    for (START(v7227_support); NEXT(v7227);)
    if (INHERIT(OWNER(v7227),Kernel._class))
     (*Generate.methods_bodies)(Generate.PRODUCER->value,
      v7227);
    } 
  generate_meta_load_module(v7237);
  breakline_void();
  princ_string("GC_UNBIND;");
  close_block_void();
  breakline_void();
  fclose_port(Optimize.OPT->outfile);
  GC_UNBIND;} 

void  generate_classes_module(module *v1140)
{ GC_BIND;
  erase_table(Generate.classFile);
  { OID gc_local;
    ITERATE(v7227);
    bag *v7227_support;
    v7227_support = GC_OBJECT(list,Optimize.OPT->objects);
    for (START(v7227_support); NEXT(v7227);)
    { GC_LOOP;
      if (INHERIT(OWNER(v7227),Kernel._class))
       { put_table(Generate.classFile,v7227,ClAlloc->import(Kernel._port,(int *) fopen_string(append_string(GC_STRING(append_string(GC_STRING(append_string(GC_STRING(Optimize.compiler->source),GC_STRING(string_v(Reader._starfs_star->value)))),GC_STRING(string_v((*Generate.c_string)(Generate.PRODUCER->value,
          GC_OID((*Kernel.name)(v7227))))))),GC_STRING(OBJECT(Generate_producer,Generate.PRODUCER->value)->extension)),"w")));
        use_as_output_port(EXPORT((ClairePort *),nth_table1(Generate.classFile,v7227)));
        tformat_string("++++ Creating the file ~A.java\n",2,GC_OBJECT(list,list::alloc(1,GC_OID((*Generate.c_string)(Generate.PRODUCER->value,
          GC_OID((*Kernel.name)(v7227)))))));
        (Optimize.OPT->level = 0);
        princ_string("// class file for ");
        print_any(v7227);
        princ_string(" in module ");
        print_any(_oid_(v1140));
        princ_string(" ");
        breakline_void();
        breakline_void();
        breakline_void();
        princ_string("public class ");
        ident_symbol(OBJECT(symbol,(*Kernel.name)(v7227)));
        princ_string(" extends ");
        ident_symbol(OBJECT(ClaireClass,v7227)->superclass->name);
        new_block_void();
        breakline_void();
        breakline_void();
        princ_string("/* empty constructor by default */");
        breakline_void();
        princ_string("public ");
        ident_symbol(OBJECT(symbol,(*Kernel.name)(v7227)));
        princ_string("() {}");
        breakline_void();
        breakline_void();
        breakline_void();
        princ_string("/* declaration of fields */");
        { OID gc_local;
          ITERATE(v7249);
          bag *v7249_support;
          v7249_support = GC_OBJECT(list,OBJECT(bag,(*Optimize.get_indexed)(v7227)));
          for (START(v7249_support); NEXT(v7249);)
          if (_oid_(domain_I_restriction(OBJECT(restriction,v7249))) == v7227)
           { breakline_void();
            princ_string("public  ");
            interface_I_class(psort_any(_oid_(OBJECT(restriction,v7249)->range)));
            princ_string(" ");
            ident_symbol(OBJECT(restriction,v7249)->selector->name);
            princ_string(";");
            } 
          } 
        breakline_void();
        princ_string("");
        fclose_port(EXPORT((ClairePort *),nth_table1(Generate.classFile,v7227)));
        } 
      GC_UNLOOP;} 
    } 
  GC_UNBIND;} 

void  generate_c2f_module(module *v1140)
{ GC_BIND;
  { ClairePort * v5173 = fopen_string(append_string(GC_STRING(append_string(GC_STRING(append_string(GC_STRING(Optimize.compiler->source),GC_STRING(string_v(Reader._starfs_star->value)))),GC_STRING(string_v((*Generate.c_string)(Generate.PRODUCER->value,
      _oid_(v1140->name)))))),GC_STRING(OBJECT(Generate_producer,Generate.PRODUCER->value)->extension)),"w");
    (Optimize.OPT->outfile = v5173);
    use_as_output_port(v5173);
    (*Generate.generate_start_file)(Generate.PRODUCER->value,
      _oid_(v1140));
    fclose_port(v5173);
    claire_gc();
    generate_functions_module(v1140);
    claire_gc();
    v5173= fopen_string(append_string(GC_STRING(append_string(GC_STRING(append_string(GC_STRING(Optimize.compiler->source),GC_STRING(string_v(Reader._starfs_star->value)))),GC_STRING(string_v((*Generate.c_string)(Generate.PRODUCER->value,
      _oid_(v1140->name)))))),GC_STRING(OBJECT(Generate_producer,Generate.PRODUCER->value)->extension)),"a");
    use_as_output_port(v5173);
    (Optimize.OPT->outfile = v5173);
    (Optimize.OPT->level = 2);
    generate_objects_module(v1140);
    use_as_output_port(EXPORT((ClairePort *),Reader.STDOUT->value));
    claire_gc();
    use_as_output_port(v5173);
    generate_meta_load_module(v1140);
    (*Generate.generate_end_file)(Generate.PRODUCER->value,
      _oid_(v1140));
    } 
  GC_UNBIND;} 

void  generate_interface_module(module *v1140,bag *v7236)
{ tformat_string("==== Generate inteface (.h) files for module ~S \n",0,list::alloc(1,_oid_(v1140)));
  use_as_output_port(Optimize.OPT->cinterface);
  breakline_void();
  (*Generate.generate_interface)(Generate.PRODUCER->value,
    _oid_(v1140));
  (*Generate.namespace_I)(Generate.PRODUCER->value,
    _oid_(v1140));
  generate_objects_module(v1140);
  (*Kernel.module_I)(Generate.PRODUCER->value,
    _oid_(v1140));
  (*Generate.end_module_interface)(Generate.PRODUCER->value,
    _oid_(v1140));
  fclose_port(Optimize.OPT->cinterface);
  use_as_output_port(EXPORT((ClairePort *),Reader.STDOUT->value));
  } 

void  generate_objects_module(module *v1140)
{ GC_RESERVE(1);  // HOHO v3.0.55 optim !
  { OID gc_local;
    ITERATE(v7248);
    bag *v7248_support;
    v7248_support = GC_OBJECT(list,Optimize.OPT->objects);
    for (START(v7248_support); NEXT(v7248);)
    { breakline_void();
      (*Generate.public_static)(Generate.PRODUCER->value);
      if (INHERIT(OWNER(v7248),Core._global_variable))
       { { OID  v16291;
          if (nativeVar_ask_global_variable(OBJECT(global_variable,v7248)) == CTRUE)
           v16291 = _oid_(getRange_global_variable(OBJECT(global_variable,v7248)));
          else v16291 = _oid_(Core._global_variable);
            (*Generate.interface_I)(Generate.PRODUCER->value,
            v16291);
          } 
        princ_string(" ");
        ident_symbol(OBJECT(thing,v7248)->name);
        princ_string(";");
        } 
      else { (*Generate.interface_I)(Generate.PRODUCER->value,
            _oid_(psort_any(_oid_(OWNER(v7248)))));
          princ_string(" ");
          if (INHERIT(OWNER(v7248),Kernel._class))
           { princ_string("_");
            c_princ_string(string_I_symbol(OBJECT(symbol,(*Kernel.name)(v7248))));
            } 
          else if (INHERIT(OWNER(v7248),Reader._reserved_keyword))
           { princ_string("_cl_");
            c_princ_string(string_I_symbol(OBJECT(symbol,(*Kernel.name)(v7248))));
            } 
          else ident_symbol(OBJECT(symbol,(*Kernel.name)(v7248)));
            princ_string(";");
          } 
        } 
    } 
  { OID gc_local;
    ITERATE(v7248);
    bag *v7248_support;
    v7248_support = GC_OBJECT(set,Optimize.OPT->properties);
    for (START(v7248_support); NEXT(v7248);)
    { GC_LOOP;
      if (Optimize.OPT->objects->memq(v7248) != CTRUE)
       { { OID  v11388;
          { { OID  v809 = CNULL;
              { OID gc_local;
                ITERATE(v11498);
                bag *v11498_support;
                v11498_support = GC_OBJECT(set,Optimize.OPT->properties);
                for (START(v11498_support); NEXT(v11498);)
                { GC_LOOP;
                  if (v11498 != v7248)
                   { if (equal_string(string_I_symbol(OBJECT(thing,v11498)->name),string_I_symbol(OBJECT(thing,v7248)->name)) == CTRUE)
                     { v809= v11498;
                      break;} 
                    } 
                  GC_UNLOOP;} 
                } 
              v11388 = v809;
              } 
            GC_OID(v11388);} 
          if (v11388 != CNULL)
           { property * v11498 = OBJECT(property,v11388);
            close_exception(((general_error *) (*Core._general_error)(_string_("[217] ~S and ~S cannot be defined in the same module"),
              _oid_(list::alloc(2,_oid_(v11498),v7248)))));
            } 
          else ;} 
        princ_string("\n");
        (*Generate.public_static)(Generate.PRODUCER->value);
        (*Generate.interface_I)(Generate.PRODUCER->value,
          _oid_(psort_any(_oid_(OWNER(v7248)))));
        princ_string(" ");
        ident_symbol(OBJECT(thing,v7248)->name);
        princ_string(";// ");
        print_any(_oid_(OBJECT(thing,v7248)->name));
        princ_string("");
        } 
      GC_UNLOOP;} 
    } 
  GC_UNBIND;} 

void  generate_meta_load_module(module *v1140)
{ GC_RESERVE(1);  // HOHO v3.0.55 optim !
  princ_string("// definition of the meta-model for ");
  print_any(_oid_(v1140));
  princ_string(" \n");
  breakline_void();
  princ_string("");
  (*Generate.generate_meta_load)(Generate.PRODUCER->value,
    _oid_(v1140));
  princ_string("// definition of the properties ");
  { OID gc_local;
    ITERATE(v7248);
    bag *v7248_support;
    v7248_support = GC_OBJECT(set,Optimize.OPT->properties);
    for (START(v7248_support); NEXT(v7248);)
    if (Optimize.OPT->objects->memq(v7248) != CTRUE)
     (*Generate.declare)(Generate.PRODUCER->value,
      v7248);
    } 
  breakline_void();
  breakline_void();
  princ_string("// instructions from module sources");
  { OID  v7234 = CNULL;
    { OID gc_local;
      ITERATE(v7233);
      bag *v7233_support;
      v7233_support = GC_OBJECT(list,Optimize.OPT->instructions);
      for (START(v7233_support); NEXT(v7233);)
      { GC_LOOP;
        { breakline_void();
          if (Kernel._string == OWNER(v7233))
           { if (equal(_oid_(Kernel._string),_oid_(OWNER(v7234))) != CTRUE)
             breakline_void();
            princ_string("// ");
            (*Kernel.princ)(v7233);
            princ_string("");
            } 
          else if (global_var_def_ask_any(v7233) == CTRUE)
           (*Generate.global_var_def_I)(Generate.PRODUCER->value,
            _oid_(v1140),
            v7233);
          else statement_any(v7233,_oid_(Kernel.emptySet),_oid_(Kernel.emptySet));
            GC__OID(v7234 = v7233, 1);
          } 
        GC_UNLOOP;} 
      } 
    } 
  GC_UNBIND;} 

ClaireBoolean * global_var_def_ask_any(OID v7248)
{ GC_BIND;
  { ClaireBoolean *Result ;
    Result = ((INHERIT(OWNER(v7248),Language._Let)) ?
      ((INHERIT(OWNER(OBJECT(Let,v7248)->value),Language._Call)) ? (((*Kernel.selector)(OBJECT(Let,v7248)->value) == _oid_(Optimize.object_I)) ? (((*(OBJECT(bag,(*Core.args)(OBJECT(Let,v7248)->value))))[2] == _oid_(Core._global_variable)) ? CTRUE: CFALSE): CFALSE): CFALSE) :
      CFALSE );
    GC_UNBIND; return (Result);} 
  } 

ClaireClass * getRange_global_variable(global_variable *v7248)
{ { ClaireClass *Result ;
    Result = ((equal(_oid_(v7248->range),_oid_(Kernel.emptySet)) == CTRUE) ?
      OWNER(v7248->value) :
      class_I_type(v7248->range) );
    return (Result);} 
  } 

void  generate_functions_module(module *v1140)
{ GC_BIND;
  princ_string("// definition of class methods ");
  { ClairePort * v7240 = (Optimize.OPT->outfile);
    { OID gc_local;
      ITERATE(v7230);
      bag *v7230_support;
      v7230_support = GC_OBJECT(list,Optimize.OPT->functions);
      for (START(v7230_support); NEXT(v7230);)
      { GC_LOOP;
        { (Optimize.OPT->level = 2);
          (Optimize.OPT->outfile = v7240);
          print_c_function_lambda2(GC_OBJECT(lambda,OBJECT(lambda,(*Kernel.nth)(v7230,
            1))),GC_STRING(string_v((*Kernel.nth)(v7230,
            2))),GC_OID((*Kernel.nth)(v7230,
            3)));
          } 
        GC_UNLOOP;} 
      } 
    } 
  GC_UNBIND;} 

list * parents_module(module *v1140,list *v7236)
{ if (v7236->memq(_oid_(v1140)) == CTRUE) 
  { { list *Result ;
      Result = v7236;
      return (Result);} 
     } 
  else{ GC_BIND;
    if (((v1140->part_of == (NULL)) ? CTRUE : CFALSE) != CTRUE)
     v7236= GC_OBJECT(list,parents_module(v1140->part_of,v7236));
    v7236= GC_OBJECT(list,v7236->addFast(_oid_(v1140)));
    { list *Result ;
      Result = v7236;
      GC_UNBIND; return (Result);} 
    } 
  } 

list * parents_list(list *v1140)
{ GC_RESERVE(1);  // HOHO v3.0.55 optim !
  { list *Result ;
    { list * v7236 = list::empty(Kernel._module);
      { OID gc_local;
        ITERATE(v7248);
        for (START(v1140); NEXT(v7248);)
        { GC_LOOP;
          GC__ANY(v7236 = parents_module(OBJECT(module,v7248),v7236), 1);
          GC_UNLOOP;} 
        } 
      Result = v7236;
      } 
    GC_UNBIND; return (Result);} 
  } 

void  get_module2(module *v7237)
{ load_module(v7237);
  begin_module(v7237);
  } 

void  generate_file_string2(char *v1140,char *v11188)
{ GC_RESERVE(1);  // HOHO v3.0.55 optim !
  { ClairePort * v11497 = fopen_string(append_string(v1140,".cl"),"r");
    ClaireBoolean * v7226 = Reader.reader->toplevel;
    ClairePort * v11496 = (Reader.reader->fromp);
    if (Optimize.compiler->class2file_ask != CTRUE)
     (Optimize.OPT->outfile = fopen_string(append_string(v11188,GC_STRING(OBJECT(Generate_producer,Generate.PRODUCER->value)->extension)),"w"));
    (Reader.reader->toplevel = CFALSE);
    (Optimize.compiler->loading_ask = CTRUE);
    (Reader.reader->nb_line = 1);
    (Reader.reader->external = v1140);
    (Reader.reader->fromp = v11497);
    if (Optimize.compiler->class2file_ask != CTRUE)
     start_file_string(v1140,ClEnv->module_I);
    { OID  v8305 = GC_OID(readblock_port(v11497));
      { OID gc_local;
        while ((equal(v8305,_oid_(Reader.eof)) != CTRUE))
        { GC_LOOP;
          if (ClEnv->verbose > 5)
           { princ_string("[");
            princ_integer(Reader.reader->nb_line);
            princ_string("] ");
            print_any(_oid_(OWNER(v8305)));
            princ_string(" (");
            print_any(GC_OID(_oid_(Optimize.OPT->need_modules)));
            princ_string("%)\n");
            } 
          if ((Kernel._string == OWNER(v8305)) && 
              (Optimize.compiler->class2file_ask != CTRUE))
           { if (Optimize.compiler->naming < 2)
             { ClairePort * v11560 = use_as_output_port(Optimize.OPT->outfile);
              princ_string("\n//");
              (*Kernel.princ)(v8305);
              princ_string("");
              use_as_output_port(v11560);
              } 
            } 
          else GC_OBJECT(list,Optimize.OPT->instructions)->addFast(GC_OID((*Optimize.c_code)(v8305,
              _oid_(Kernel._void))));
            GC__OID(v8305 = readblock_port(v11497), 1);
          GC_UNLOOP;} 
        } 
      } 
    fclose_port(v11497);
    (Optimize.compiler->loading_ask = CFALSE);
    (Reader.reader->toplevel = v7226);
    (Reader.reader->external = "toplevel");
    (Reader.reader->fromp = v11496);
    if (Optimize.compiler->class2file_ask != CTRUE)
     fclose_port(Optimize.OPT->outfile);
    } 
  GC_UNBIND;} 

OID  make_c_function_lambda_Generate(lambda *v1140,char *v12719,OID v7237)
{ { OID Result = 0;
    if (Optimize.compiler->class2file_ask == CTRUE)
     Result = (*Generate.create_function_entry)(Generate.PRODUCER->value,
      _oid_(v1140),
      _string_(v12719),
      v7237);
    else Result = print_c_function_lambda2(v1140,v12719,v7237);
      return (Result);} 
  } 

OID  print_c_function_lambda2(lambda *v1140,char *v12719,OID v7237)
{ GC_BIND;
  (OBJECT(Generate_producer,Generate.PRODUCER->value)->body = v1140->body);
  { OID Result = 0;
    { ClaireClass * v7243 = Kernel._void;
      OID  v3871;
      { if (boolean_I_any(v7237) != CTRUE)
         v3871 = (*Optimize.c_code)(GC_OID(v1140->body),
          _oid_(Kernel._void));
        else if (Kernel._method == OWNER(v7237))
         { v7243= check_sort_method(OBJECT(method,v7237));
          v3871 = OBJECT(Generate_producer,Generate.PRODUCER->value)->body;
          } 
        else if (INHERIT(OWNER(v7237),Kernel._class))
         { v7243= OBJECT(ClaireClass,v7237);
          v3871 = (*Optimize.c_code)(GC_OID(v1140->body),
            v7237);
          } 
        else { v7243= Kernel._any;
            v3871 = (*Optimize.c_code)(GC_OID(v1140->body),
              _oid_(Kernel._any));
            } 
          GC_OID(v3871);} 
      OID  v4031 = GC_OID((*Generate.protect_result)(Generate.PRODUCER->value,
        _oid_(v7243),
        _oid_(Optimize.OPT->protection),
        v7237));
      ClaireBoolean * v12406 = need_debug_ask_any(v7237);
      ClaireBoolean * v9997 = ((Optimize.OPT->profile_ask == CTRUE) ? (((Kernel._method == OWNER(v7237)) || 
          (v7237 == CNULL)) ? CTRUE: CFALSE): CFALSE);
      (*Generate.generate_function_start)(Generate.PRODUCER->value,
        _oid_(v1140),
        _oid_(v7243),
        v7237,
        _string_(v12719));
      if (Optimize.OPT->max_vars > 0)
       { (Language._starvariable_index_star->value= 0);
        lexical_num_any2(v3871,(v1140->vars->length+1));
        (Optimize.OPT->loop_index = Language._starvariable_index_star->value);
        } 
      new_block_void();
      if (v9997 == CTRUE)
       (*Generate.generate_profile)(Generate.PRODUCER->value,
        v7237);
      if (v12406 == CTRUE)
       (*Generate.debug_intro)(Generate.PRODUCER->value,
        _oid_(v1140),
        v7237);
      (*Generate.print_body)(v3871,
        v4031,
        _oid_(v7243),
        GC_OID(v1140->body),
        _oid_(not_any(_oid_(((Optimize.OPT->protection == CTRUE) ? CTRUE : ((v9997 == CTRUE) ? CTRUE : ((v12406 == CTRUE) ? CTRUE : ((Optimize.OPT->alloc_stack == CTRUE) ? CTRUE : CFALSE))))))));
      close_block_void();
      breakline_void();
      (Optimize.OPT->alloc_stack = CFALSE);
      Result = ClAlloc->import(Kernel._port,(int *) use_as_output_port(EXPORT((ClairePort *),Reader.STDOUT->value)));
      } 
    GC_UNBIND; return (Result);} 
  } 

void  print_body_If(If *v3871,char *v4031,ClaireClass *v7243,If *v6350,ClaireBoolean *v14719)
{ GC_BIND;
  if ((c_func_any(GC_OID(v3871->test)) == CTRUE) && 
      ((Optimize.OPT->protection == CTRUE) && 
        ((c_safe_any(GC_OID(v6350->arg)) == CTRUE) && 
          (Optimize.compiler->debug_ask->memq(_oid_(ClEnv->module_I)) != CTRUE))))
   { char * v10292 = ((v7243 != Kernel._void) ?
      "return " :
      "" );
    princ_string("if ");
    (*Optimize.bool_exp)(GC_OID(v3871->test),
      Kernel.ctrue,
      Core.nil->value);
    princ_string(" ");
    breakline_void();
    new_block_void();
    outer_statement_any(GC_OID(v3871->arg),v10292,v7243,v14719);
    princ_string(" ");
    close_block_void();
    princ_string("else");
    new_block_void();
    (*Generate.print_body)(GC_OID(v3871->other),
      _string_(v4031),
      _oid_(v7243),
      GC_OID(v6350->other),
      _oid_(v14719));
    close_block_void();
    } 
  else print_body_any(_oid_(v3871),
      v4031,
      v7243,
      _oid_(v6350),
      v14719);
    GC_UNBIND;} 

void  print_body_any(OID v3871,char *v4031,ClaireClass *v7243,OID v6350,ClaireBoolean *v14719)
{ (*Generate.gc_introduction)(Generate.PRODUCER->value,
    v3871);
  outer_statement_any(v3871,v4031,v7243,v14719);
  } 

OID  print_body_Do(Do *v3871,char *v4031,ClaireClass *v7243,OID v6350,ClaireBoolean *v14719)
{ GC_BIND;
  { OID Result = 0;
    { list * v7236 = GC_OBJECT(list,v3871->args);
      int  v9507 = v7236->length;
      int  v7237 = 0;
      (*Generate.gc_introduction)(Generate.PRODUCER->value,
        _oid_(v3871));
      { ITERATE(v7248);
        Result= _oid_(CFALSE);
        for (START(v7236); NEXT(v7248);)
        { ++v7237;
          if (v7237 == v9507)
           outer_statement_any(v7248,v4031,v7243,v14719);
          else inner_statement_any(v7248,_oid_(Kernel.emptySet),_oid_(Kernel.emptySet));
            } 
        } 
      } 
    GC_UNBIND; return (Result);} 
  } 

void  outer_statement_any(OID v3871,char *v4031,ClaireClass *v7243,ClaireBoolean *v14719)
{ if ((c_func_any(v3871) == CTRUE) && 
      ((v14719 == CTRUE) && 
        ((v7243 != Kernel._void) && 
          (inherit_ask_class(OWNER(v3871),Language._If) != CTRUE))))
   { if (equal_string(v4031,"") != CTRUE)
     { princ_string(v4031);
      princ_string("(");
      (*Generate.expression)(v3871,
        Core.nil->value);
      princ_string(");");
      } 
    else { (*Generate.expression)(v3871,
          Core.nil->value);
        princ_string(";");
        } 
      } 
  else if (v7243 != Kernel._void)
   { new_block_void();
    (*Generate.define_variable)(Generate.PRODUCER->value,
      _oid_(v7243),
      _string_("Result"));
    breakline_void();
    statement_any(v3871,_string_("Result"),_oid_(Kernel.emptySet));
    if (equal_string(v4031,"") != CTRUE)
     { princ_string(v4031);
      princ_string("(Result);");
      close_block_void();
      princ_string("");
      } 
    } 
  else { statement_any(v3871,_oid_(Kernel.emptySet),_oid_(Kernel.emptySet));
      princ_string(v4031);
      } 
    } 

ClaireBoolean * c_safe_any(OID v7248)
{ return (not_any(_oid_(((Optimize.OPT->allocation == CTRUE) ? ((BCONTAIN(c_status_any(v7248,Kernel.nil),1)) ? CTRUE: CFALSE): CFALSE))));} 

ClaireClass * check_sort_method(method *v1140)
{ GC_BIND;
  { ClaireClass *Result ;
    { ClaireType * v11590 = v1140->range;
      OID  v7226 = GC_OID(OBJECT(Generate_producer,Generate.PRODUCER->value)->body);
      ClaireType * v11592 = GC_OBJECT(ClaireType,ptype_type(OBJECT(ClaireType,(*Optimize.c_type)(v7226))));
      ClaireClass * v7243 = psort_any(_oid_(v11590));
      (OBJECT(Generate_producer,Generate.PRODUCER->value)->body = c_strict_code_any(v7226,v7243));
      if (_inf_equal_type(v11592,v11590) != CTRUE)
       { tformat_string("---- note: ~S's range was found to be ~S (vs. ~S) \n",2,list::alloc(3,_oid_(v1140),
          _oid_(v11592),
          _oid_(v11590)));
        if (Generate.WrongMethod->value == 0)
         { (Generate.WrongMethod->value= v7226);
          close_exception(((general_error *) (*Core._general_error)(_string_("You can look at WrongMethod"),
            _oid_(Kernel.nil))));
          } 
        if ((Optimize.compiler->safety < 2) || 
            ((Optimize.compiler->safety < 4) && 
                (boolean_I_any(sort_equal_class(osort_any(_oid_(v11592)),v7243)) != CTRUE)))
         (OBJECT(Generate_producer,Generate.PRODUCER->value)->body = c_strict_code_any(GC_OID(c_check_any(GC_OID((*Optimize.c_code)(v7226,
          _oid_(Kernel._any))),GC_OID((*Optimize.c_code)(_oid_(v11590),
          _oid_(Kernel._type))))),v7243));
        if (boolean_I_any(sort_equal_class(osort_any(_oid_(v11592)),v7243)) != CTRUE)
         { if ((v7243 != Kernel._void) && 
              ((_oid_((INHERIT(v11592->isa,Kernel._class) ? (ClaireObject *) sort_I_class((ClaireClass *) OBJECT(ClaireClass,_oid_(v11592))) :  (ClaireObject *)  sort_I_type((ClaireType *) OBJECT(ClaireType,_oid_(v11592))))) == _oid_(Kernel._void)) || 
                  (inherit_ask_class(v7243,OBJECT(ClaireClass,_oid_((INHERIT(v11592->isa,Kernel._class) ? (ClaireObject *) sort_I_class((ClaireClass *) OBJECT(ClaireClass,_oid_(v11592))) :  (ClaireObject *)  sort_I_type((ClaireType *) OBJECT(ClaireType,_oid_(v11592))))))) != CTRUE)))
           (*Optimize.Cerror)(_string_("[218] Sort error: Cannot compile ~S."),
            _oid_(v1140));
          } 
        } 
      { ClaireType * v11593 = GC_OBJECT(ClaireType,OBJECT(ClaireType,(*Kernel._exp)(_oid_(v11590),
          _oid_(v11592))));
        if ((Optimize.compiler->safety > 1) && 
            (osort_any(_oid_(v11593)) == v7243))
         { (OBJECT(Generate_producer,Generate.PRODUCER->value)->body = (*Optimize.c_strict_check)(GC_OID(OBJECT(Generate_producer,Generate.PRODUCER->value)->body),
            _oid_(class_I_type(v11593))));
          ;} 
        } 
      if (INHERIT(v11590->isa,Kernel._tuple))
       { (Optimize.OPT->alloc_stack = CTRUE);
        } 
      if ((Optimize.OPT->allocation == CTRUE) && 
          (nth_integer(status_I_restriction(v1140),6) != CTRUE))
       { if (nth_integer(status_I_restriction(v1140),1) != CTRUE)
         ;} 
      else { (Optimize.OPT->loop_gc = CFALSE);
          (Optimize.OPT->protection = CFALSE);
          } 
        Result = psort_any(_oid_(v1140->range));
      } 
    GC_UNBIND; return (Result);} 
  } 

OID  typed_args_list_list(list *v1140)
{ { OID Result = 0;
    { ClaireBoolean * v4961 = CTRUE;
      { ITERATE(v7248);
        Result= _oid_(CFALSE);
        for (START(v1140); NEXT(v7248);)
        { if (v4961 == CTRUE)
           v4961= CFALSE;
          else princ_string(",");
            (*Generate.interface_I)(Generate.PRODUCER->value,
            _oid_(sort_Variable(OBJECT(Variable,v7248))));
          (*Language.ident)(Generate.PRODUCER->value,
            v7248);
          } 
        } 
      } 
    return (Result);} 
  } 

ClaireBoolean * need_debug_ask_any(OID v7237)
{ { ClaireBoolean *Result ;
    if (Kernel._method == OWNER(v7237))
     { property * v7240 = OBJECT(restriction,v7237)->selector;
      Result = ((Optimize.compiler->debug_ask->memq(_oid_(OBJECT(restriction,v7237)->module_I)) == CTRUE) ? 
      ((domain_I_restriction(OBJECT(restriction,v7237)) != Kernel._environment) ? 
      ((OBJECT(restriction,v7237)->module_I != claire.it) ? 
      ((v7240 != Core.self_eval) ? 
      ((v7240 != Core.execute) ? 
      ((v7240 != Core.eval_message) ? 
      ((v7240 != Core.push_debug) ? 
      ((v7240 != Core.pop_debug) ? 
      ((v7240 != Core.tr_indent) ? 
      ((v7240 != Core.find_which) ? 
      ((v7240 != Core.matching_ask) ? 
      ((v7240 != Core.vmatch_ask) ? 
      CTRUE: CFALSE): CFALSE): CFALSE): CFALSE): CFALSE): CFALSE): CFALSE): CFALSE): CFALSE): CFALSE): CFALSE): CFALSE);
      } 
    else Result = CFALSE;
      return (Result);} 
  } 

void  get_dependents_method(method *v7237)
{ GC_BIND;
  { OID gc_local;
    ITERATE(v7240);
    bag *v7240_support;
    v7240_support = GC_OBJECT(set,dependents_method(v7237));
    for (START(v7240_support); NEXT(v7240);)
    { property * v6598 = v7237->selector;
      add_table(Reader.PRdependent,_oid_(v6598),v7240);
      } 
    } 
  GC_UNBIND;} 

void  c_princ_function(ClaireFunction *v1140)
{ c_princ_string(string_I_function(v1140));
  } 

void  set_outfile_lambda(lambda *v1140)
{ GC_BIND;
  { module * v7237 = ClEnv->module_I;
    char * v11153 = string_I_symbol(v7237->name);
    ClairePort * v7240 = (Optimize.OPT->outfile);
    OID  v3109;
    { if (v1140->vars->length != 0)
       v3109 = (*Kernel.range)((*(v1140->vars))[1]);
      else v3109 = CNULL;
        GC_OID(v3109);} 
    if (v3109 != CNULL)
     { { ClaireBoolean * g0007I;
        { OID  v3751;
          { OID gc_local;
            ITERATE(v7227);
            v3751= _oid_(CFALSE);
            bag *v7227_support;
            v7227_support = GC_OBJECT(list,Optimize.OPT->objects);
            for (START(v7227_support); NEXT(v7227);)
            if ((INHERIT(OWNER(v7227),Kernel._class)) && 
                (equal(v7227,v3109) == CTRUE))
             { v3751 = Kernel.ctrue;
              break;} 
            } 
          g0007I = boolean_I_any(v3751);
          } 
        
        if (g0007I == CTRUE) v11153= GC_STRING(string_v((*Generate.c_string)(Generate.PRODUCER->value,
            _oid_(OBJECT(ClaireClass,v3109)->name))));
          } 
      if (equal_string(v11153,string_I_symbol(ClEnv->module_I->name)) != CTRUE)
       v7240= fopen_string(append_string(GC_STRING(append_string(GC_STRING(append_string(GC_STRING(Optimize.compiler->source),GC_STRING(string_v(Reader._starfs_star->value)))),v11153)),GC_STRING(OBJECT(Generate_producer,Generate.PRODUCER->value)->extension)),"a");
      (Optimize.OPT->outfile = v7240);
      } 
    ;} 
  GC_UNBIND;} 

