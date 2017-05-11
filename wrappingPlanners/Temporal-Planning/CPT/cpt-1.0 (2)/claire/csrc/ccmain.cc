/***** CLAIRE Compilation of file ccmain.cl 
         [version 3.0.0 / safety 5] Mon Nov  8 16:37:05 2004 *****/

#include <claire.h>
#include <Kernel.h>
#include <Core.h>
#include <Language.h>
#include <Reader.h>
#include <Optimize.h>
#include <Generate.h>
#include <ccmain.h>

//+-------------------------------------------------------------+
//| CLAIRE                                                      |
//| ccmain.cl                                                   |
//| Copyright (C) 1994 - 2003 Yves Caseau. All Rights Reserved  |
//| cf. copyright info in file object.cl: about()               |
//+-------------------------------------------------------------+
// ----------------------------------------------------------------------
// this file contains the claire definition of the main for the cclaire
// executable. The four envt are supported ntw, ntv, unix, win32v
// ----------------------------------------------------------------------
// -------------------------------------------------------------------
// Contents
//      Part 1: definition of system variables
//      Part 2: definition of the main function
//      Part 3: Generating makefiles
// -------------------------------------------------------------------
// *******************************************************************
// *       Part 1: definition of the system variables                *
// *******************************************************************
// dumb utility
/* The c++ function for: external!(m:module) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG] */
char * external_I_module(module *m)
{ { char *Result ;
    Result = ((((m->external == (NULL)) ? CTRUE : CFALSE) != CTRUE) ?
      m->external :
      string_I_symbol(m->name) );
    return (Result);} 
  } 


/* The c++ function for: string2module(s:string) [NEW_ALLOC] */
module * string2module_string(char *s)
{ { module *Result ;
    { ClaireObject *V_CC ;
      { OID  m = value_string(s);
        if (INHERIT(OWNER(m),Kernel._module))
         V_CC = OBJECT(module,m);
        else close_exception(((general_error *) (*Core._general_error)(_string_("~A is not a module"),
            _oid_(list::alloc(1,_string_(s))))));
          } 
      Result= (module *) V_CC;} 
    return (Result);} 
  } 


// *******************************************************************
// *       Part 2: definition of the main function
// *******************************************************************
// help file
/* The c++ function for: printHelp(_CL_obj:void) [NEW_ALLOC] */
void  printHelp_void()
{ princ_string("------------- CLAIRE: The Art of Elegant Programming -----------\n\n");
  about_void();
  princ_string("\noptions -s <int> <int> : set memory allocation size  \n");
  princ_string("        -f <filename>  : load <filename>             \n");
  princ_string("        -env <sys> : compile for a different OS target \n");
  princ_string("        -n : do not load the init file               \n");
  princ_string("        -m <module> : load <module>                  \n");
  princ_string("        -v <int> : upgrade the verbosity level       \n");
  princ_string("        -S <flag> : sets the global variable <flag> to true  \n");
  princ_string("        -o <name> : sets the name of the executable  \n");
  princ_string("        -ld <name> : sets the library directory  \n");
  princ_string("        -od <name> : sets the output directory  \n");
  princ_string("        -p : profiling mode                          \n");
  princ_string("        -D : debug mode                              \n");
  princ_string("        -safe : safe mode                            \n");
  princ_string("        -O : optimizing mode                         \n");
  princ_string("        -os <int> : sets the optimizer savety level          \n");
  princ_string("        -l <lib> : adds <lib> to the list of needed libs     \n");
  princ_string("        -cm <module>: compiles a module -> executable        \n");
  princ_string("        -cc <module>: compiles a module -> target files      \n");
  princ_string("        -cl <module>: compiles a module -> library           \n");
  princ_string("        -cx <main file> : generates an executable from a file\n");
  CL_exit(0);
  } 


//Claire's main
/* The c++ function for: main(lp:list[string]) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
void  main_list(list *lp)
{ GC_RESERVE(13);  // v3.0.55 optim !
  { ClaireBoolean * rCode = CTRUE;
    char * _Zcm = "";
    char * _Zcf = "";
    int  dblevel = 1;
    char * _Zout = "";
    char * _Zcj = "";
    int  slevel = 0;
    int  clevel = 1;
    ClaireBoolean * _Zinit_ask = CTRUE;
    int  vlevel = 2;
    list * l = ((list *) copy_bag(lp));
    { ClaireHandler c_handle = ClaireHandler();
      if ERROR_IN 
      { { (Reader._starfs_star->value= _string_("/"));
          (OBJECT(Generate_producer,Generate.PRODUCER->value)->extension = ".cc");
          update_property(Optimize.libraries_dir,
            Optimize.compiler,
            17,
            Kernel._object,
            _oid_(list::alloc(Kernel._any,3,_string_("/home/vince/Programmes/cpt-1.0/claire/bin/public/unix"),
              _string_("/home/vince/Programmes/cpt-1.0/claire/bin/debug/unix"),
              _string_("/home/vince/Programmes/cpt-1.0/claire/bin/public/unix"))));
          (Optimize.compiler->headers_dir = "/home/vince/Programmes/cpt-1.0/claire/include");
          update_property(Optimize.options,
            Optimize.compiler,
            19,
            Kernel._object,
            _oid_(list::alloc(Kernel._any,3,_string_("-c -fwritable-strings -O2"),
              _string_("-c -fwritable-strings"),
              _string_("-c -fwritable-strings"))));
          (Optimize.compiler->env = "unix");
          (Optimize.claire_lib->value= _string_(""));
          { OID gc_local;
            while ((l->length != 0))
            { GC_LOOP;
              if ((equal((*(l))[1],_string_("?")) == CTRUE) || 
                  (equal((*(l))[1],_string_("-help")) == CTRUE))
               printHelp_void();
              else if (equal((*(l))[1],_string_("-s")) == CTRUE)
               { if (3 <= l->length)
                 l= skip_list(l,3);
                else close_exception(((general_error *) (*Core._general_error)(_string_("option: -s <s1> <s2>"),
                    _oid_(Kernel.nil))));
                  } 
              else if (equal((*(l))[1],_string_("-f")) == CTRUE)
               { if (2 <= l->length)
                 { load_string(string_v((*(l))[2]));
                  l= skip_list(l,2);
                  } 
                else close_exception(((general_error *) (*Core._general_error)(_string_("option: -f <filename>"),
                    _oid_(Kernel.nil))));
                  } 
              else if (equal((*(l))[1],_string_("-env")) == CTRUE)
               { if (2 <= l->length)
                 { (Optimize.compiler->env = string_v((*(l))[2]));
                  l= skip_list(l,2);
                  } 
                else close_exception(((general_error *) (*Core._general_error)(_string_("option: -env <OS name>"),
                    _oid_(Kernel.nil))));
                  } 
              else if (equal((*(l))[1],_string_("-m")) == CTRUE)
               { if (2 <= l->length)
                 { if (_Zinit_ask == CTRUE)
                   { load_string("init");
                    _Zinit_ask= CFALSE;
                    } 
                  { module * m = string2module_string(string_v((*(l))[2]));
                    load_module(m);
                    begin_module(m);
                    l= skip_list(l,2);
                    (Optimize.claire_modules->value= _oid_(GC_OBJECT(list,OBJECT(list,Optimize.claire_modules->value))->addFast(_oid_(m))));
                    } 
                  } 
                else close_exception(((general_error *) (*Core._general_error)(_string_("option: -m <module>"),
                    _oid_(Kernel.nil))));
                  } 
              else if (equal((*(l))[1],_string_("-v")) == CTRUE)
               { if (2 <= l->length)
                 { vlevel= (vlevel+integer_I_string(string_v((*(l))[2])));
                  l= skip_list(l,2);
                  } 
                else close_exception(((general_error *) (*Core._general_error)(_string_("option: -v <integer>"),
                    _oid_(Kernel.nil))));
                  } 
              else if (equal((*(l))[1],_string_("-ld")) == CTRUE)
               { if (2 <= l->length)
                 { (Optimize.claire_lib->value= (*(l))[2]);
                  l= skip_list(l,2);
                  } 
                else close_exception(((general_error *) (*Core._general_error)(_string_("option: -od <directory>"),
                    _oid_(Kernel.nil))));
                  } 
              else if (equal((*(l))[1],_string_("-od")) == CTRUE)
               { if (2 <= l->length)
                 { (Optimize.compiler->source = string_v((*(l))[2]));
                  l= skip_list(l,2);
                  } 
                else close_exception(((general_error *) (*Core._general_error)(_string_("option: -od <directory>"),
                    _oid_(Kernel.nil))));
                  } 
              else if (equal((*(l))[1],_string_("-os")) == CTRUE)
               { if (2 <= l->length)
                 { slevel= integer_I_string(string_v((*(l))[2]));
                  l= skip_list(l,2);
                  } 
                else close_exception(((general_error *) (*Core._general_error)(_string_("option: -ol <int>"),
                    _oid_(Kernel.nil))));
                  } 
              else if (equal((*(l))[1],_string_("-S")) == CTRUE)
               { if (2 <= l->length)
                 { (CLREAD(global_variable,new_class2(Core._global_variable,symbol_I_string2(string_v((*(l))[2]))),value) = Kernel.ctrue);
                  l= skip_list(l,2);
                  } 
                else close_exception(((general_error *) (*Core._general_error)(_string_("option: -S <FLAG>"),
                    _oid_(Kernel.nil))));
                  } 
              else if (equal((*(l))[1],_string_("-o")) == CTRUE)
               { if (2 <= l->length)
                 { GC__STRING(_Zout = string_v((*(l))[2]), 6);
                  l= skip_list(l,2);
                  } 
                else close_exception(((general_error *) (*Core._general_error)(_string_("option: -o <name>"),
                    _oid_(Kernel.nil))));
                  } 
              else if (equal((*(l))[1],_string_("-p")) == CTRUE)
               { (Optimize.OPT->profile_ask = CTRUE);
                dblevel= ((dblevel <= 1) ?
                  1 :
                  dblevel );
                l= skip_list(l,1);
                } 
              else if (equal((*(l))[1],_string_("-D")) == CTRUE)
               { dblevel= 0;
                l= skip_list(l,1);
                } 
              else if (equal((*(l))[1],_string_("-safe")) == CTRUE)
               { (Optimize.compiler->safety = ((dblevel == 0) ?
                  0 :
                  1 ));
                (Optimize.claire_lib->value= (*(Optimize.compiler->libraries_dir))[2]);
                (Optimize.claire_options->value= (*(Optimize.compiler->options))[2]);
                l= skip_list(l,1);
                } 
              else if (equal((*(l))[1],_string_("-O")) == CTRUE)
               { (Optimize.compiler->optimize_ask = CTRUE);
                dblevel= 2;
                l= skip_list(l,1);
                } 
              else if (equal((*(l))[1],_string_("-l")) == CTRUE)
               { if (2 <= l->length)
                 { GC_OBJECT(list,Optimize.compiler->libraries)->addFast((*(l))[2]);
                  l= skip_list(l,2);
                  } 
                else close_exception(((general_error *) (*Core._general_error)(_string_("option: -l <library>"),
                    _oid_(Kernel.nil))));
                  } 
              else if (equal((*(l))[1],_string_("-cl")) == CTRUE)
               { if (2 <= l->length)
                 { GC__STRING(_Zcm = string_v((*(l))[2]), 3);
                  l= skip_list(l,2);
                  } 
                else close_exception(((general_error *) (*Core._general_error)(_string_("option: -cm <module>"),
                    _oid_(Kernel.nil))));
                  } 
              else if (equal((*(l))[1],_string_("-cc")) == CTRUE)
               { if (2 <= l->length)
                 { clevel= 0;
                  GC__STRING(_Zcm = string_v((*(l))[2]), 3);
                  l= skip_list(l,2);
                  } 
                else close_exception(((general_error *) (*Core._general_error)(_string_("option: -cc <module>"),
                    _oid_(Kernel.nil))));
                  } 
              else if (equal((*(l))[1],_string_("-cm")) == CTRUE)
               { if (2 <= l->length)
                 { clevel= 2;
                  GC__STRING(_Zcm = string_v((*(l))[2]), 3);
                  l= skip_list(l,2);
                  } 
                else close_exception(((general_error *) (*Core._general_error)(_string_("option: -cl <module>"),
                    _oid_(Kernel.nil))));
                  } 
              else if (equal((*(l))[1],_string_("-cj")) == CTRUE)
               { if (2 <= l->length)
                 { GC__STRING(_Zcj = string_v((*(l))[2]), 7);
                  l= skip_list(l,2);
                  } 
                } 
              else if (equal((*(l))[1],_string_("-cjx")) == CTRUE)
               { if (2 <= l->length)
                 { GC__STRING(_Zcj = string_v((*(l))[2]), 7);
                  clevel= 0;
                  l= skip_list(l,2);
                  } 
                } 
              else if (equal((*(l))[1],_string_("-cx")) == CTRUE)
               { if (2 <= l->length)
                 { GC__STRING(_Zcf = string_v((*(l))[2]), 4);
                  l= skip_list(l,2);
                  clevel= 2;
                  } 
                else close_exception(((general_error *) (*Core._general_error)(_string_("option: -cx <filename>"),
                    _oid_(Kernel.nil))));
                  } 
              else if (equal((*(l))[1],_string_("-n")) == CTRUE)
               { _Zinit_ask= CFALSE;
                l= skip_list(l,1);
                } 
              else { if (string_v((*(l))[1])[1 - 1] == '-')
                   { print_any((*(l))[1]);
                    princ_string(" is an unvalid option\n");
                    printHelp_void();
                    } 
                  rCode= CFALSE;
                  GC__ANY(l = list::empty(Kernel._string), 12);
                  } 
                GC_UNLOOP;} 
            } 
          if (equal_string(_Zout,"") == CTRUE)
           { if (equal_string(_Zcm,"") != CTRUE)
             _Zout= _Zcm;
            else if (equal_string(_Zcf,"") != CTRUE)
             _Zout= _Zcf;
            } 
          if (_Zinit_ask == CTRUE)
           load_string("init");
          (Optimize.claire_options->value= (*(Optimize.compiler->options))[((dblevel == 0) ?
            2 :
            ((dblevel == 2) ?
              1 :
              3 ) )]);
          if (equal(Optimize.claire_lib->value,_string_("")) == CTRUE)
           (Optimize.claire_lib->value= (*(Optimize.compiler->libraries_dir))[((dblevel == 0) ?
            2 :
            ((dblevel == 2) ?
              1 :
              3 ) )]);
          (ClEnv->verbose = vlevel);
          if (slevel > 0)
           (Optimize.compiler->safety = slevel);
          if (equal_string(_Zcm,"") != CTRUE)
           { module * m = string2module_string(_Zcm);
            (Optimize.compiler->active_ask = CTRUE);
            if (equal(GC_OID(_oid_(m->uses)),GC_OID(_oid_(list::alloc(1,GC_OID((*(OBJECT(bag,Optimize.claire_modules->value)))[2]))))) == CTRUE)
             { (Optimize.claire_modules->value= _oid_(shrink_list(OBJECT(bag,Optimize.claire_modules->value),2)));
              tformat_string("=== Light Module ~S:~S -> use ~S=== ",0,GC_OBJECT(list,list::alloc(3,_oid_(m),
                GC_OID(_oid_(m->uses)),
                GC_OID(Optimize.claire_modules->value))));
              } 
            (Optimize.claire_modules->value= _oid_(GC_OBJECT(list,OBJECT(list,Optimize.claire_modules->value))->addFast(_oid_(m))));
            (*Reader.load)(value_string("Compile"));
            if (equal_string(_Zout,"") != CTRUE)
             (m->external = _Zout);
            load_module(m);
            if (dblevel < 1)
             { (Optimize.compiler->safety = ((Optimize.compiler->safety <= 4) ?
                Optimize.compiler->safety :
                4 ));
              GC_OBJECT(list,Optimize.compiler->debug_ask)->addFast(_oid_(m));
              } 
            compile_module(m);
            if (clevel == 1)
             { if (equal_string(_Zout,"") != CTRUE)
               (m->external = _Zout);
              cmakefile_any(_oid_(m),"");
              } 
            else if (clevel == 2)
             cmakefile_any(_oid_(m),_Zout);
            CL_exit(0);
            } 
          else if (equal_string(_Zcj,"") != CTRUE)
           (*Core.call)(value_string("jcmakefile"),
            _oid_(string2module_string(_Zcj)),
            _string_(_Zout),
            _oid_(equal(clevel,0)));
          else if (equal_string(_Zcf,"") != CTRUE)
           { (Optimize.compiler->active_ask = CTRUE);
            (*Reader.load)(value_string("Compile"));
            load_string(_Zcf);
            function_compile_string(_Zcf,_Zcf);
            cmakefile_any(_string_(_Zcf),_Zout);
            CL_exit(0);
            } 
          } 
        ClEnv->cHandle--;} 
      else if (belong_to(_oid_(ClEnv->exception_I),_oid_(Kernel._any)) == CTRUE)
      { c_handle.catchIt();{ restore_state_meta_reader(Reader.reader);
          debug_if_possible_void();
          } 
        } 
      else PREVIOUS_HANDLER;} 
    } 
  GC_UNBIND;} 


// *******************************************************************
// *       Part 3: single file compiling                             *
// *******************************************************************
// compile a single command file: can only generate functions
// since there is no associated module
// we assume that the file only contains function definitions
//
/* The c++ function for: function_compile(self:string,fullname:string) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
void  function_compile_string(char *self,char *fullname)
{ GC_RESERVE(7);  // v3.0.55 optim !
  (Optimize.OPT->need_modules = set::empty());
  { char * _Zinterface = GC_STRING(append_string(GC_STRING(_7_string(GC_STRING(Optimize.compiler->headers_dir),self)),".h"));
    (Optimize.OPT->legal_modules = set_I_class(Kernel._module));
    (Optimize.OPT->properties = set::empty(Kernel._property));
    (Optimize.OPT->objects = ((list *) set::empty(Kernel._object)));
    (Optimize.OPT->functions = list::empty());
    (Optimize.OPT->cinterface = fopen_string(_Zinterface,"w"));
    (Optimize.OPT->cfile = _string_(self));
    generate_file_string2(fullname,GC_STRING(_7_string(GC_STRING(Optimize.compiler->source),self)));
    use_as_output_port(Optimize.OPT->cinterface);
    breakline_void();
    { OID gc_local;
      ITERATE(x);
      bag *x_support;
      x_support = GC_OBJECT(set,Optimize.OPT->need_modules);
      for (START(x_support); NEXT(x);)
      if ((OBJECT(module,x)->made_of->length != 0) && 
          (contain_ask_list(OBJECT(list,Optimize.claire_modules->value),x) != CTRUE))
       { princ_string("#include <");
        ident_symbol(OBJECT(symbol,(*Kernel.name)(x)));
        princ_string(".h>\n");
        } 
      } 
    { OID gc_local;
      ITERATE(l);
      bag *l_support;
      l_support = GC_OBJECT(list,Optimize.OPT->functions);
      for (START(l_support); NEXT(l);)
      { GC_LOOP;
        { OID  _Zf = GC_OID((*Kernel.nth)(l,
            1));
          OID  _Zvars = GC_OID((*Kernel.nth)(l,
            2));
          OID  s = GC_OID((*Kernel.nth)(l,
            3));
          princ_string("\nextern ");
          (*Generate.interface_I)(Generate.PRODUCER->value,
            s);
          princ_string(" ");
          (*Kernel.c_princ)(_Zf);
          princ_string("(");
          typed_args_list_list(OBJECT(list,_Zvars));
          princ_string(");");
          } 
        GC_UNLOOP;} 
      } 
    fclose_port(Optimize.OPT->cinterface);
    } 
  GC_UNBIND;} 


// *******************************************************************
// *       Part 4: Generating makefiles                              *
// *******************************************************************
// prints the list of lib files that are needed for m
/* The c++ function for: lib!(m:any,l:list) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
void  lib_I_any(OID m,list *l)
{ GC_BIND;
  { char * s_end = ".lib";
    char * _Zenv = GC_STRING(Optimize.compiler->env);
    ClaireBoolean * first = CTRUE;
    char * s_sep = GC_STRING((((equal_string(_Zenv,"unix") == CTRUE) || 
        ((equal_string(_Zenv,"win32v") == CTRUE) || 
          (equal_string(_Zenv,"ntv") == CTRUE))) ?
      append_string(" $Z",GC_STRING(string_v(Reader._starfs_star->value))) :
      "," ));
    if (equal_string(_Zenv,"ntw") == CTRUE)
     princ_string("LIBP $Z L ");
    else if (equal_string(_Zenv,"unix") == CTRUE)
     princ_string("$Z/");
    else if ((equal_string(_Zenv,"win32v") == CTRUE) || 
        (equal_string(_Zenv,"ntv") == CTRUE))
     princ_string("$Z\\");
    { OID gc_local;
      ITERATE(m2);
      for (START(l); NEXT(m2);)
      { GC_LOOP;
        if (OBJECT(module,m2)->made_of->length != 0)
         { if (first == CTRUE)
           first= CFALSE;
          else princ_string(s_sep);
            princ_string(external_I_module(OBJECT(module,m2)));
          princ_string(s_end);
          { OID gc_local;
            ITERATE(s);
            bag *s_support;
            s_support = GC_OBJECT(list,OBJECT(module,m2)->uses);
            for (START(s_support); NEXT(s);)
            if (Kernel._string == OWNER(s))
             { princ_string(s_sep);
              (*Kernel.princ)(s);
              princ_string(s_end);
              princ_string("");
              } 
            } 
          princ_string("");
          } 
        GC_UNLOOP;} 
      } 
    { OID gc_local;
      ITERATE(m2);
      bag *m2_support;
      m2_support = GC_OBJECT(list,Optimize.compiler->libraries);
      for (START(m2_support); NEXT(m2);)
      { princ_string(s_sep);
        princ_string(string_v(m2));
        princ_string(s_end);
        princ_string("");
        } 
      } 
    } 
  GC_UNBIND;} 


// prints the necesssary files for the two compilation modes
// if link? is true we need a comma-separated list
// if m is a module, we compile fi*.cpp + m.cpp -> m.lib
// if m=f is a file, we compile f-s.cpp (system), f.cpp (functions) -> exe
/* The c++ function for: files!(m:any,link?:boolean,%out:string) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
void  files_I_any(OID m,ClaireBoolean *link_ask,char *_Zout)
{ GC_BIND;
  { char * _Zbef = GC_STRING(((link_ask == CTRUE) ?
      "" :
      append_string("$T",GC_STRING(string_v(Reader._starfs_star->value))) ));
    char * _Zend = ((equal_string(Optimize.compiler->env,"unix") == CTRUE) ?
      "o" :
      "obj" );
    char * _Zsep = ((link_ask == CTRUE) ?
      "," :
      " " );
    princ_string(_Zbef);
    { OID  g0009UU;
      { if ((link_ask == CTRUE) || 
            (Kernel._string == OWNER(m)))
         g0009UU = _string_(append_string(_Zout,"-s"));
        else g0009UU = (*Kernel.name)(m);
          GC_OID(g0009UU);} 
      (*Kernel.princ)(g0009UU);
      } 
    princ_string(".");
    princ_string(_Zend);
    if (INHERIT(OWNER(m),Kernel._module))
     { if (link_ask != CTRUE)
       { OID gc_local;
        ITERATE(ff);
        for (START(OBJECT(module,m)->made_of); NEXT(ff);)
        { princ_string(_Zsep);
          princ_string(_Zbef);
          princ_string(string_v(ff));
          princ_string(".");
          princ_string(_Zend);
          princ_string(" ");
          } 
        } 
      } 
    else if (Kernel._string == OWNER(m))
     { princ_string(_Zsep);
      princ_string(_Zbef);
      princ_string(string_v(m));
      princ_string(".");
      princ_string(_Zend);
      princ_string("");
      } 
    princ_string("");
    } 
  GC_UNBIND;} 


// module linker - dispatch according to hardware and OS
/* The c++ function for: cmakefile(m:any,out:string) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
void  cmakefile_any(OID m,char *out)
{ GC_BIND;
  { list * l = GC_OBJECT(list,add_modules_list(GC_OBJECT(list,OBJECT(list,Optimize.claire_modules->value))));
    char * f = ((INHERIT(OWNER(m),Kernel._module)) ?
      string_I_symbol(OBJECT(thing,m)->name) :
      out );
    char * _Zos = GC_STRING(Optimize.compiler->env);
    if (equal_string(out,"") != CTRUE)
     generate_s_file_string(out,l,m);
    if (equal_string(_Zos,"win32v") == CTRUE)
     GC_OBJECT(list,Optimize.compiler->libraries)->addFast(_string_("gui"));
    else if (OBJECT(bag,Optimize.claire_modules->value)->length == 3)
     GC_OBJECT(list,Optimize.compiler->libraries)->addFast(_string_("noConsole"));
    else GC_OBJECT(list,Optimize.compiler->libraries)->addFast(_string_("Console"));
      if ((equal_string(_Zos,"ntw") == CTRUE) || 
        ((equal_string(_Zos,"ntv") == CTRUE) || 
          (equal_string(_Zos,"win32v") == CTRUE)))
     create_makefile_nt_any(m,out,l);
    else if (equal_string(_Zos,"unix") == CTRUE)
     create_makefile_unix_any(m,out,l);
    else close_exception(((general_error *) (*Core._general_error)(_string_("Unknown environment, should be one of :'ntv','ntw','win32v','unix'\n"),
        _oid_(Kernel.nil))));
      print_in_string_void();
    if (equal_string(_Zos,"unix") == CTRUE)
     { princ_string("make -f ");
      princ_string(f);
      princ_string(".mk");
      } 
    else { princ_string("nmake /c /f ");
        princ_string(f);
        princ_string(".mk");
        } 
      claire_shell(end_of_print_void());
    } 
  GC_UNBIND;} 


// creates the nt makefile for a module or a string
// m is a module or nothing
// out is a string or nothing
// l is a list of library modules
// ... with the help of Arnaud Linz
/* The c++ function for: create_makefile_nt(m:any,out:string,l:list) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
void  create_makefile_nt_any(OID m,char *out,list *l)
{ GC_BIND;
  { char * f = ((INHERIT(OWNER(m),Kernel._module)) ?
      string_I_symbol(OBJECT(thing,m)->name) :
      out );
    ClairePort * p = fopen_string(append_string(f,".mk"),"w");
    char * _Zenv = GC_STRING(Optimize.compiler->env);
    char * _ZI = ((equal_string(_Zenv,"ntw") == CTRUE) ?
      "/i=" :
      "/I" );
    char * _ZO = ((equal_string(_Zenv,"ntw") == CTRUE) ?
      "/fo=" :
      "/Fo" );
    char * sis = ((equal_string(_Zenv,"ntw") == CTRUE) ?
      "nt" :
      _Zenv );
    use_as_output_port(p);
    princ_string("option = ");
    princ_string(_ZI);
    princ_string(Optimize.compiler->headers_dir);
    princ_string(" ");
    princ_string(string_v(Optimize.claire_options->value));
    princ_string(" /DCLPC /DCLWIN\n");
    princ_string(".SUFFIXES : .exe .obj .cpp\n\n");
    princ_string("Z = ");
    princ_string(string_v(Optimize.claire_lib->value));
    princ_string("\nT = ");
    princ_string(Optimize.compiler->source);
    princ_string("\n");
    princ_string("CC = ");
    princ_string(((equal_string(Optimize.compiler->env,"ntw") == CTRUE) ?
      "wpp386" :
      "cl" ));
    princ_string("\n");
    princ_string("FILES = ");
    files_I_any(m,CFALSE,out);
    princ_string("\n");
    princ_string("{$T}.cpp{$T}.obj:\n");
    princ_string("\t$(CC) $(option) ");
    princ_string(_ZO);
    princ_string("$T\\$(<B).obj ");
    if (equal_string(_Zenv,"ntw") != CTRUE)
     princ_string("/Tp ");
    princ_string("$<\n");
    princ_string("all: ");
    if (equal_string(out,"") != CTRUE)
     { princ_string(out);
      princ_string(".exe");
      } 
    else { princ_string("$Z\\");
        print_any(m);
        princ_string(".lib");
        } 
      princ_string("\n");
    if (INHERIT(OWNER(m),Kernel._module))
     { princ_string("$Z\\");
      princ_string(external_I_module(OBJECT(module,m)));
      princ_string(".lib: $(FILES)\n");
      if (equal_string(_Zenv,"ntw") == CTRUE)
       { princ_string("\t!wlib /q /c / b $Z\\");
        princ_string(external_I_module(OBJECT(module,m)));
        princ_string(".lib +-$?\n");
        } 
      else princ_string("\tlib /NOLOGO /OUT:$@ $(FILES)\n");
        } 
    if (equal_string(out,"") != CTRUE)
     { if (equal_string(_Zenv,"ntw") != CTRUE)
       princ_string("JUNK = /NOLOGO /DEBUG /MAP /STACK:1600000 user32.lib gdi32.lib shell32.lib comdlg32.lib\n");
      princ_string(out);
      princ_string(".exe: ");
      if (INHERIT(OWNER(m),Kernel._module))
       { princ_string("$Z\\");
        princ_string(external_I_module(OBJECT(module,m)));
        princ_string(".lib $T\\");
        princ_string(out);
        princ_string("-s.obj");
        } 
      else princ_string("$(FILES)");
        princ_string("\n");
      if (equal_string(_Zenv,"ntw") == CTRUE)
       { princ_string("\twlink sys ");
        princ_string(sis);
        princ_string(" N ");
        princ_string(out);
        princ_string(" d all option ");
        princ_string("q,d,ST=600K,c,mang");
        princ_string(" P $T F ");
        files_I_any(m,CTRUE,out);
        princ_string(" ");
        lib_I_any(m,l);
        princ_string("\n\n");
        } 
      else { princ_string("\tlink /subsystem:");
          princ_string(((equal_string(_Zenv,"win32v") == CTRUE) ?
            "windows" :
            "console" ));
          princ_string(" $(JUNK) ");
          lib_I_any(m,l);
          princ_string(" /OUT:");
          princ_string(out);
          princ_string(".exe ");
          if (INHERIT(OWNER(m),Kernel._module))
           { princ_string("$T\\");
            princ_string(out);
            princ_string("-s.obj");
            } 
          else princ_string("$(FILES)");
            princ_string("\n\n");
          } 
        } 
    fclose_port(p);
    } 
  GC_UNBIND;} 


// creates the unix makefile for a module or a string (cf. nt makefile)
// this was copied from v2.4.28
// with the help of Francois Laburthe !
/* The c++ function for: create_makefile_unix(m:any,out:string,l:list) [NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE] */
void  create_makefile_unix_any(OID m,char *out,list *l)
{ GC_BIND;
  { char * f = ((INHERIT(OWNER(m),Kernel._module)) ?
      string_I_symbol(OBJECT(thing,m)->name) :
      out );
    ClairePort * p = fopen_string(append_string(f,".mk"),"w");
    use_as_output_port(p);
    princ_string("# --- unix makefile --- version ");
    (*Kernel.princ)(GC_OID(release_void()));
    princ_string(" ----\n");
    princ_string("Z = ");
    princ_string(string_v(Optimize.claire_lib->value));
    princ_string("\n");
    princ_string("T = ");
    princ_string(Optimize.compiler->source);
    princ_string("\n\n");
    princ_string("FILES = ");
    files_I_any(m,CFALSE,out);
    princ_string("\n");
    princ_string("CXX = g++\n");
    princ_string("LINK = ld -r\n");
    princ_string("CXXFLAGS = -I");
    princ_string(Optimize.compiler->headers_dir);
    princ_string(" -DCLUNIX ");
    princ_string(string_v(Optimize.claire_options->value));
    princ_string("\n\n");
    princ_string("$T%.o:\t$T%.cc\n\t$(CXX) $(CXXFLAGS) -c $(@:.o=.cc) -o $@\n");
    princ_string("all: ");
    if (equal_string(out,"") != CTRUE)
     { princ_string(out);
      princ_string("");
      } 
    else { princ_string("$Z");
        princ_string(string_v(Reader._starfs_star->value));
        print_any(m);
        princ_string(".lib");
        } 
      princ_string("\n");
    if (INHERIT(OWNER(m),Kernel._module))
     { princ_string("$Z");
      princ_string(string_v(Reader._starfs_star->value));
      princ_string(external_I_module(OBJECT(module,m)));
      princ_string(".lib: $(FILES)\n");
      princ_string("\t$(LINK) -o $Z");
      princ_string(string_v(Reader._starfs_star->value));
      princ_string(external_I_module(OBJECT(module,m)));
      princ_string(".lib $(FILES)\n");
      } 
    if (equal_string(out,"") != CTRUE)
     { princ_string(out);
      princ_string(": ");
      if (INHERIT(OWNER(m),Kernel._module))
       { princ_string("$Z");
        princ_string(string_v(Reader._starfs_star->value));
        princ_string(external_I_module(OBJECT(module,m)));
        princ_string(".lib $T");
        princ_string(string_v(Reader._starfs_star->value));
        princ_string(out);
        princ_string("-s.o");
        } 
      else princ_string("$(FILES)");
        princ_string("\n");
      princ_string("\t$(CXX) -o ");
      princ_string(out);
      princ_string(" ");
      lib_I_any(m,l);
      princ_string(" ");
      if (INHERIT(OWNER(m),Kernel._module))
       { princ_string("$T");
        princ_string(string_v(Reader._starfs_star->value));
        princ_string(out);
        princ_string("-s.o");
        } 
      else princ_string("$(FILES)");
        princ_string("\n\n");
      } 
    fclose_port(p);
    } 
  GC_UNBIND;} 


// ---------------------------------------------------------------