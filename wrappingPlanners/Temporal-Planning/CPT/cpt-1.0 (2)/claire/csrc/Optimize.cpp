/***** CLAIRE Compilation of file Optimize.cl 
         [version 3.3.4 / safety 5] Sat Oct 16 06:53:36 2004 *****/

#include <claire.h>
#include <Kernel.h>
#include <Core.h>
#include <Language.h>
#include <Reader.h>
#include <Optimize.h>


OptimizeClass Optimize;

NameSpace Compile;
// definition of the meta-model for Optimize 

void OptimizeClass::metaLoad() { 
  GC_BIND;
  ClEnv->module_I = it;
// definition of the properties 
  Optimize.home = property::make("home",claire.it);
  Optimize.instructions = property::make("instructions",2,Compile.it);
  Optimize.objects = property::make("objects",Compile.it);
  Optimize.properties = property::make("properties",Compile.it);
  Optimize.functions = property::make("functions",2,Compile.it);
  Optimize.need_to_close = property::make("need_to_close",2,Compile.it);
  Optimize.need_modules = property::make("need_modules",2,Compile.it);
  Optimize.legal_modules = property::make("legal_modules",Compile.it);
  Optimize.allocation = property::make("allocation",Compile.it);
  Optimize.protection = property::make("protection",Compile.it);
  Optimize.alloc_stack = property::make("alloc_stack",2,Compile.it);
  Optimize.ignore = property::make("ignore",Compile.it);
  Optimize.to_remove = property::make("to_remove",Compile.it);
  Optimize.cinterface = property::make("cinterface",2,Compile.it);
  Optimize.outfile = property::make("outfile",2,Compile.it);
  Optimize.max_vars = property::make("max_vars",Compile.it);
  Optimize.loop_gc = property::make("loop_gc",Compile.it);
  Optimize.loop_index = property::make("loop_index",Compile.it);
  Optimize.level = property::make("level",2,Compile.it);
  Optimize.in_method = property::make("in_method",Compile.it);
  Optimize.profile_ask = property::make("profile?",2,Compile.it);
  Optimize.cfile = property::make("cfile",2,Compile.it);
  Optimize.use_update = property::make("use_update",Optimize.it);
  Optimize.use_nth_equal = property::make("use_nth=",Optimize.it);
  Optimize.online_ask = property::make("online?",Optimize.it);
  Optimize.recompute = property::make("recompute",Optimize.it);
  Optimize.unsure = property::make("unsure",Optimize.it);
  Optimize.knowns = property::make("knowns",Optimize.it);
  Optimize.headers = property::make("headers",2,claire.it);
  Optimize.debug_ask = property::make("debug?",claire.it);
  Optimize.active_ask = property::make("active?",2,claire.it);
  Optimize.safety = property::make("safety",claire.it);
  Optimize.env = property::make("env",2,claire.it);
  Optimize.naming = property::make("naming",claire.it);
  Optimize.libraries = property::make("libraries",2,claire.it);
  Optimize.loading_ask = property::make("loading?",2,claire.it);
  Optimize.class2file_ask = property::make("class2file?",claire.it);
  Optimize.greedy_ask = property::make("greedy?",2,claire.it);
  Optimize.libraries_dir = property::make("libraries_dir",2,claire.it);
  Optimize.headers_dir = property::make("headers_dir",2,claire.it);
  Optimize.options = property::make("options",2,claire.it);
  Optimize.ptype = property::make("ptype",Optimize.it);
  Optimize.c_code_call = property::make("c_code_call",Optimize.it);
  Optimize.c_register = property::make("c_register",Optimize.it);
  Optimize.c_gc_ask = property::make("c_gc?",Compile.it);
  Optimize.gcsafe_ask = property::make("gcsafe?",Optimize.it);
  Optimize.selector_psort = property::make("selector_psort",Optimize.it);
  Optimize.c_status = property::make("c_status",Optimize.it);
  Optimize.c_or = property::make("c_or",Optimize.it);
  Optimize.c_return = property::make("c_return",Optimize.it);
  Optimize.status_I = property::make("status!",Compile.it);
  Optimize.stable_ask = property::make("stable?",Optimize.it);
  Optimize.legal_ask = property::make("legal?",Optimize.it);
  Optimize.selector_register = property::make("selector_register",Optimize.it);
  Optimize.allocate_ask = property::make("allocate?",Optimize.it);
  Optimize.oload = property::make("oload",Optimize.it);
  Optimize.turbo = property::make("turbo",Optimize.it);
  Optimize.stats = property::make("stats",Optimize.it);
  Optimize.sort_abstract_I = property::make("sort_abstract!",Optimize.it);
  Optimize.warn = property::make("warn",Compile.it);
  Optimize.c_warn = property::make("c_warn",Optimize.it);
  Optimize.open_message = property::make("open_message",Optimize.it);
  Optimize.sort_abstract_ask = property::make("sort_abstract?",Optimize.it);
  Optimize.extended_ask = property::make("extended?",Optimize.it);
  Optimize.extends = property::make("extends",Optimize.it);
  Optimize.enumerate_code = property::make("enumerate_code",Optimize.it);
  Optimize.c_code_method = property::make("c_code_method",Optimize.it);
  Optimize.range_infers_for = property::make("range_infers_for",Optimize.it);
  Optimize.range_infers = property::make("range_infers",Optimize.it);
  Optimize.gc_register = property::make("gc_register",Optimize.it);
  Optimize.range_infer_case = property::make("range_infer_case",Optimize.it);
  Optimize.c_gc_I = property::make("c_gc!",Optimize.it);
  Optimize.range_sets = property::make("range_sets",Optimize.it);
  Optimize.Variable_I = property::make("Variable!",Compile.it);
  Optimize.identifiable_ask = property::make("identifiable?",Compile.it);
  Optimize.c_inline = property::make("c_inline",Optimize.it);
  Optimize.bound_variables = property::make("bound_variables",Optimize.it);
  Optimize.c_substitution = property::make("c_substitution",Optimize.it);
  Optimize.c_inline_arg_ask = property::make("c_inline_arg?",Optimize.it);
  Optimize.restriction_I = property::make("restriction!",Optimize.it);
  Optimize.c_inline_ask = property::make("c_inline?",Optimize.it);
  Optimize.c_boolean = property::make("c_boolean",Optimize.it);
  Optimize.use_range = property::make("use_range",Optimize.it);
  Optimize.inline_optimize_ask = property::make("inline_optimize?",Optimize.it);
  Optimize.daccess = property::make("daccess",Optimize.it);
  Optimize.c_code_write = property::make("c_code_write",Optimize.it);
  Optimize.c_code_hold = property::make("c_code_hold",Optimize.it);
  Optimize.c_code_add = property::make("c_code_add",Optimize.it);
  Optimize.c_code_delete = property::make("c_code_delete",Optimize.it);
  Optimize.c_code_table = property::make("c_code_table",Optimize.it);
  Optimize.c_code_array = property::make("c_code_array",Optimize.it);
  Optimize.c_code_nth = property::make("c_code_nth",Optimize.it);
  Optimize.c_code_belong = property::make("c_code_belong",Optimize.it);
  Optimize.c_code_not = property::make("c_code_not",Optimize.it);
  Optimize.Update_ask = property::make("Update?",Optimize.it);
  Optimize.Call_method_I = property::make("Call_method!",Optimize.it);
  Optimize.nth_type_check = property::make("nth_type_check",Optimize.it);
  Optimize.extendedTest_ask = property::make("extendedTest?",Optimize.it);
  Optimize.Iterate_I = property::make("Iterate!",Optimize.it);
  Optimize.total_ask = property::make("total?",Optimize.it);
  Optimize.analyze_I = property::make("analyze!",Optimize.it);
  Optimize.extract_signature_I = property::make("extract_signature!",Optimize.it);
  Optimize.extract_status_new = property::make("extract_status_new",Optimize.it);
  Optimize.add_method_I = property::make("add_method!",Optimize.it);
  Optimize.showstatus = property::make("showstatus",claire.it);
  Optimize.safe = property::make("safe",claire.it);
  Optimize.overflow_ask = property::make("overflow?",claire.it);
  Optimize.c_code_multiple = property::make("c_code_multiple",Optimize.it);
  Optimize.Produce_put = property::make("Produce_put",Compile.it);
  Optimize.Produce_get = property::make("Produce_get",Compile.it);
  Optimize.Produce_remove = property::make("Produce_remove",Compile.it);
  Optimize.compute_if_write_inverse = property::make("compute_if_write_inverse",Compile.it);
  Optimize.compute_set_write = property::make("compute_set_write",Compile.it);
  Optimize.Tighten = property::make("Tighten",Compile.it);
  Optimize.c_code_add_bag = property::make("c_code_add_bag",Optimize.it);
  Optimize.diet_ask = property::make("diet?",claire.it);
  Optimize.Produce_erase = property::make("Produce_erase",Compile.it);
  Optimize.lexical_num = property::make("lexical_num",Compile.it);
  Optimize.inner2outer_ask = property::make("inner2outer?",Compile.it);
  Optimize.pmember = property::make("pmember",Optimize.it);
  Optimize.c_code_select = property::make("c_code_select",Optimize.it);
  Optimize.inner_select = property::make("inner_select",Optimize.it);
  Optimize.compile_if_write = property::make("compile_if_write",Optimize.it);
  Optimize.compileEventMethod = property::make("compileEventMethod",Optimize.it);
  Optimize.s_test = property::make("s_test",claire.it);
  Optimize.c_srange = property::make("c_srange",Compile.it);
  Optimize.optimize_ask = property::make("optimize?",claire.it);
  Optimize.nativeVar_ask = property::make("nativeVar?",Compile.it);
  Optimize.simple_operations = property::make("simple_operations",Compile.it);
  Optimize.non_identifiable_set = property::make("non_identifiable_set",Compile.it);
  Optimize.sort_pattern_ask = property::make("sort_pattern?",Optimize.it);
  Optimize.sort_code = property::make("sort_code",Optimize.it);
  Optimize.return_type = property::make("return_type",Compile.it);
  Optimize.notice = property::make("notice",Compile.it);
  Optimize.infers_from = property::make("infers_from",Optimize.it);
  
  // instructions from module sources
  { global_variable * v2072 = (Optimize.srange = (global_variable *) Core._global_variable->instantiate("srange",Compile.it));
    (v2072->range = Kernel.emptySet);
    (v2072->value = _oid_(Kernel.srange));
    close_global_variable(v2072);
    } 
  
  { global_variable * v2072 = (Language.typing = (global_variable *) Core._global_variable->instantiate("typing",iClaire.it));
    (v2072->range = Kernel.emptySet);
    (v2072->value = _oid_(Kernel.typing));
    close_global_variable(v2072);
    } 
  
  { global_variable * v2072 = (Optimize.status = (global_variable *) Core._global_variable->instantiate("status",Compile.it));
    (v2072->range = Kernel.emptySet);
    (v2072->value = _oid_(Kernel.status));
    close_global_variable(v2072);
    } 
  
  { global_variable * v2072 = (Optimize.tmatch_ask = (global_variable *) Core._global_variable->instantiate("tmatch?",Compile.it));
    (v2072->range = Kernel.emptySet);
    (v2072->value = _oid_(Core.tmatch_ask));
    close_global_variable(v2072);
    } 
  
  { global_variable * v2072 = (Optimize.pname = (global_variable *) Core._global_variable->instantiate("pname",Compile.it));
    (v2072->range = Kernel.emptySet);
    (v2072->value = _oid_(Core.pname));
    close_global_variable(v2072);
    } 
  
  { global_variable * v2072 = (Optimize.if_write = (global_variable *) Core._global_variable->instantiate("if_write",Compile.it));
    (v2072->range = Kernel.emptySet);
    (v2072->value = _oid_(Kernel.if_write));
    close_global_variable(v2072);
    } 
  
  Optimize.home->addMethod(list::domain(1,Kernel._void),Kernel._string,
  	0,_function_(home_void,"home_void"));
  
  { (Optimize._meta_OPT = ClaireClass::make("meta_OPT",Kernel._thing,Optimize.it));
    CL_ADD_SLOT(Optimize._meta_OPT,Optimize_meta_OPT,Optimize.instructions,instructions,Kernel._list,CNULL);
    CL_ADD_SLOT(Optimize._meta_OPT,Optimize_meta_OPT,Optimize.objects,objects,Kernel._list,CNULL);
    CL_ADD_SLOT(Optimize._meta_OPT,Optimize_meta_OPT,Optimize.properties,properties,nth_class2(Kernel._set,list::alloc(Kernel._any,1,_oid_(Kernel.of)),list::alloc(1,_oid_(set::alloc(1,_oid_(Kernel._property))))),CNULL);
    CL_ADD_SLOT(Optimize._meta_OPT,Optimize_meta_OPT,Optimize.functions,functions,Kernel._list,CNULL);
    CL_ADD_SLOT(Optimize._meta_OPT,Optimize_meta_OPT,Optimize.need_to_close,need_to_close,Kernel._set,CNULL);
    CL_ADD_SLOT(Optimize._meta_OPT,Optimize_meta_OPT,Optimize.need_modules,need_modules,Kernel._set,CNULL);
    CL_ADD_SLOT(Optimize._meta_OPT,Optimize_meta_OPT,Optimize.legal_modules,legal_modules,Kernel._set,CNULL);
    CL_ADD_SLOT(Optimize._meta_OPT,Optimize_meta_OPT,Optimize.allocation,allocation,Kernel._boolean,Kernel.cfalse);
    CL_ADD_SLOT(Optimize._meta_OPT,Optimize_meta_OPT,Optimize.protection,protection,Kernel._boolean,Kernel.cfalse);
    CL_ADD_SLOT(Optimize._meta_OPT,Optimize_meta_OPT,Optimize.alloc_stack,alloc_stack,Kernel._boolean,Kernel.cfalse);
    CL_ADD_SLOT(Optimize._meta_OPT,Optimize_meta_OPT,Optimize.ignore,ignore,Kernel._set,CNULL);
    CL_ADD_SLOT(Optimize._meta_OPT,Optimize_meta_OPT,Optimize.to_remove,to_remove,Kernel._set,CNULL);
    CL_ADD_SLOT(Optimize._meta_OPT,Optimize_meta_OPT,Optimize.cinterface,cinterface,Kernel._port,CNULL);
    CL_ADD_SLOT(Optimize._meta_OPT,Optimize_meta_OPT,Optimize.outfile,outfile,Kernel._port,CNULL);
    CL_ADD_SLOT(Optimize._meta_OPT,Optimize_meta_OPT,Optimize.max_vars,max_vars,Kernel._integer,0);
    CL_ADD_SLOT(Optimize._meta_OPT,Optimize_meta_OPT,Optimize.loop_gc,loop_gc,Kernel._boolean,Kernel.cfalse);
    CL_ADD_SLOT(Optimize._meta_OPT,Optimize_meta_OPT,Optimize.loop_index,loop_index,Kernel._integer,0);
    CL_ADD_SLOT(Optimize._meta_OPT,Optimize_meta_OPT,Optimize.level,level,Kernel._integer,0);
    CL_ADD_SLOT(Optimize._meta_OPT,Optimize_meta_OPT,Optimize.in_method,in_method,Kernel._any,CNULL);
    CL_ADD_SLOT(Optimize._meta_OPT,Optimize_meta_OPT,Optimize.profile_ask,profile_ask,Kernel._boolean,Kernel.cfalse);
    CL_ADD_SLOT(Optimize._meta_OPT,Optimize_meta_OPT,Optimize.cfile,cfile,Kernel._any,Kernel.cfalse);
    CL_ADD_SLOT(Optimize._meta_OPT,Optimize_meta_OPT,Optimize.use_update,use_update,Kernel._boolean,Kernel.cfalse);
    CL_ADD_SLOT(Optimize._meta_OPT,Optimize_meta_OPT,Optimize.use_nth_equal,use_nth_equal,Kernel._boolean,Kernel.cfalse);
    CL_ADD_SLOT(Optimize._meta_OPT,Optimize_meta_OPT,Optimize.online_ask,online_ask,Kernel._boolean,Kernel.cfalse);
    CL_ADD_SLOT(Optimize._meta_OPT,Optimize_meta_OPT,Optimize.recompute,recompute,Kernel._boolean,Kernel.ctrue);
    CL_ADD_SLOT(Optimize._meta_OPT,Optimize_meta_OPT,Optimize.unsure,unsure,Kernel._list,Core.nil->value);
    CL_ADD_SLOT(Optimize._meta_OPT,Optimize_meta_OPT,Optimize.knowns,knowns,nth_class2(Kernel._set,list::alloc(Kernel._any,1,_oid_(Kernel.of)),list::alloc(1,_oid_(set::alloc(1,_oid_(Kernel._relation))))),CNULL);
    CL_ADD_SLOT(Optimize._meta_OPT,Optimize_meta_OPT,Optimize.simple_operations,simple_operations,nth_class2(Kernel._set,list::alloc(Kernel._any,1,_oid_(Kernel.of)),list::alloc(1,_oid_(set::alloc(1,_oid_(Kernel._property))))),CNULL);
    CL_ADD_SLOT(Optimize._meta_OPT,Optimize_meta_OPT,Optimize.non_identifiable_set,non_identifiable_set,nth_class2(Kernel._set,list::alloc(Kernel._any,1,_oid_(Kernel.of)),list::alloc(1,_oid_(set::alloc(1,_oid_(Kernel._class))))),CNULL);
    } 
  
  { (Optimize._meta_compiler = ClaireClass::make("meta_compiler",Kernel._thing,Optimize.it));
    CL_ADD_SLOT(Optimize._meta_compiler,Optimize_meta_compiler,Kernel.external,external,Kernel._string,CNULL);
    CL_ADD_SLOT(Optimize._meta_compiler,Optimize_meta_compiler,Optimize.headers,headers,nth_class2(Kernel._list,list::alloc(Kernel._any,1,_oid_(Kernel.of)),list::alloc(1,_oid_(set::alloc(1,_oid_(Kernel._string))))),CNULL);
    CL_ADD_SLOT(Optimize._meta_compiler,Optimize_meta_compiler,Kernel.source,source,Kernel._string,CNULL);
    CL_ADD_SLOT(Optimize._meta_compiler,Optimize_meta_compiler,Optimize.debug_ask,debug_ask,nth_class2(Kernel._list,list::alloc(Kernel._any,1,_oid_(Kernel.of)),list::alloc(1,_oid_(set::alloc(1,_oid_(Kernel._module))))),CNULL);
    CL_ADD_SLOT(Optimize._meta_compiler,Optimize_meta_compiler,Kernel.version,version,Kernel._any,CNULL);
    CL_ADD_SLOT(Optimize._meta_compiler,Optimize_meta_compiler,Optimize.active_ask,active_ask,Kernel._boolean,Kernel.cfalse);
    CL_ADD_SLOT(Optimize._meta_compiler,Optimize_meta_compiler,Optimize.safety,safety,Kernel._integer,1);
    CL_ADD_SLOT(Optimize._meta_compiler,Optimize_meta_compiler,Optimize.env,env,Kernel._string,_string_("ntw"));
    CL_ADD_SLOT(Optimize._meta_compiler,Optimize_meta_compiler,Optimize.naming,naming,Kernel._integer,0);
    CL_ADD_SLOT(Optimize._meta_compiler,Optimize_meta_compiler,Optimize.libraries,libraries,nth_class2(Kernel._list,list::alloc(Kernel._any,1,_oid_(Kernel.of)),list::alloc(1,_oid_(set::alloc(1,_oid_(Kernel._string))))),CNULL);
    CL_ADD_SLOT(Optimize._meta_compiler,Optimize_meta_compiler,Kernel.inline_ask,inline_ask,Kernel._boolean,Kernel.cfalse);
    CL_ADD_SLOT(Optimize._meta_compiler,Optimize_meta_compiler,Optimize.loading_ask,loading_ask,Kernel._boolean,Kernel.cfalse);
    CL_ADD_SLOT(Optimize._meta_compiler,Optimize_meta_compiler,Optimize.class2file_ask,class2file_ask,Kernel._boolean,Kernel.cfalse);
    CL_ADD_SLOT(Optimize._meta_compiler,Optimize_meta_compiler,Optimize.greedy_ask,greedy_ask,Kernel._boolean,Kernel.cfalse);
    CL_ADD_SLOT(Optimize._meta_compiler,Optimize_meta_compiler,Optimize.libraries_dir,libraries_dir,Kernel._list,CNULL);
    CL_ADD_SLOT(Optimize._meta_compiler,Optimize_meta_compiler,Optimize.headers_dir,headers_dir,Kernel._string,CNULL);
    CL_ADD_SLOT(Optimize._meta_compiler,Optimize_meta_compiler,Optimize.options,options,Kernel._list,CNULL);
    CL_ADD_SLOT(Optimize._meta_compiler,Optimize_meta_compiler,Optimize.overflow_ask,overflow_ask,Kernel._boolean,Kernel.cfalse);
    CL_ADD_SLOT(Optimize._meta_compiler,Optimize_meta_compiler,Optimize.diet_ask,diet_ask,Kernel._boolean,Kernel.cfalse);
    CL_ADD_SLOT(Optimize._meta_compiler,Optimize_meta_compiler,Optimize.optimize_ask,optimize_ask,Kernel._boolean,Kernel.cfalse);
    } 
  
  { global_variable * v2072 = (Optimize.claire_options = (global_variable *) Core._global_variable->instantiate("claire_options",claire.it));
    (v2072->range = Kernel._string);
    (v2072->value = _string_("/w0 /zq"));
    close_global_variable(v2072);
    } 
  
  { global_variable * v2072 = (Optimize.claire_lib = (global_variable *) Core._global_variable->instantiate("claire_lib",claire.it));
    (v2072->range = Kernel._string);
    (v2072->value = _string_(""));
    close_global_variable(v2072);
    } 
  
  { global_variable * v2072 = (Optimize.claire_modules = (global_variable *) Core._global_variable->instantiate("claire_modules",claire.it));
    (v2072->range = Kernel._list);
    { global_variable * v11494 = v2072; 
      OID  v11495;
      { list * v13122;{ bag *v_list; OID v_val;
          OID v5264,CLcount;
          v_list = list::alloc(4,_string_("Kernel"),
            _string_("Core"),
            _string_("Language"),
            _string_("Reader"));
           v13122 = v_list->clone();
          for (CLcount= 1; CLcount <= v_list->length; CLcount++)
          { v5264 = (*(v_list))[CLcount];
            v_val = value_string(string_v(v5264));
            
            (*((list *) v13122))[CLcount] = v_val;} 
          } 
        
        v11495=_oid_(v13122);} 
      (v11494->value = v11495);} 
    close_global_variable(v2072);
    } 
  
  { (Optimize.compiler = (Optimize_meta_compiler *) Optimize._meta_compiler->instantiate("compiler",claire.it));
    (Optimize.compiler->external = "MS VC++");
    (Optimize.compiler->env = "ntv");
    (Optimize.compiler->version = _float_(3.4));
    (Optimize.compiler->source = "");
    (Optimize.compiler->headers_dir = "");
    (Optimize.compiler->libraries = list::alloc(Kernel._string,1,_string_("Kernel")));
    update_property(Optimize.options,
      Optimize.compiler,
      19,
      Kernel._object,
      _oid_(list::alloc(Kernel._any,3,_string_("-c /Ox /G6"),
        _string_("-c /Zi"),
        _string_("-c /Zi"))));
    ;} 
  
  { (Optimize.c_type = property::make("c_type",3,claire.it,Kernel._any,0));
    (Optimize.c_type->open = 3);
    (Optimize.c_type->range = Kernel._type);
    ;} 
  
  { (Optimize.c_code = property::make("c_code",3,claire.it,Kernel._any,0));
    (Optimize.c_code->open = 3);
    ;} 
  
  { (Optimize.c_gc = property::make("c_gc",3,claire.it,Kernel._any,0));
    (Optimize.c_gc->open = 3);
    ;} 
  
  { (Optimize.get_index = property::make("get_index",3,Compile.it,Kernel._any,0));
    (Optimize.get_index->range = Kernel._integer);
    (Optimize.get_index->open = 3);
    ;} 
  
  { (Optimize.get_indexed = property::make("get_indexed",3,Compile.it,Kernel._any,0));
    (Optimize.get_indexed->range = Kernel._bag);
    (Optimize.get_indexed->open = 3);
    ;} 
  
  { (Optimize.make_c_function = property::make("make_c_function",3,Compile.it,Kernel._any,0));
    (Optimize.make_c_function->open = 3);
    ;} 
  
  { (Optimize.make_float_function = property::make("make_float_function",3,Compile.it,Kernel._any,0));
    (Optimize.make_float_function->open = 3);
    ;} 
  
  { (Optimize.c_expression = property::make("c_expression",3,Compile.it,Kernel._any,0));
    (Optimize.c_expression->open = 3);
    ;} 
  
  { (Optimize.bool_exp = property::make("bool_exp",3,Compile.it,Kernel._any,0));
    (Optimize.bool_exp->open = 3);
    ;} 
  
  { (Optimize.c_statement = property::make("c_statement",2,Compile.it,Kernel._any,0));
    ;} 
  
  { (Optimize.c_interface = property::make("c_interface",3,claire.it,Kernel._any,0));
    (Optimize.c_interface->open = 3);
    ;} 
  
  { (Optimize.c_sort = property::make("c_sort",3,Compile.it,Kernel._any,0));
    (Optimize.c_sort->open = 3);
    ;} 
  
  { (Optimize.designated_ask = property::make("designated?",1,Compile.it,Kernel._any,0));
    ;} 
  
  { (Optimize.sort_equal = property::make("sort=",1,Compile.it,Kernel._any,0));
    ;} 
  
  { (Optimize.psort = property::make("psort",1,Compile.it,Kernel._any,0));
    ;} 
  
  { (Optimize.osort = property::make("osort",1,Compile.it,Kernel._any,0));
    ;} 
  
  { (Optimize.compile_lambda = property::make("compile_lambda",1,Compile.it,Kernel._any,0));
    ;} 
  
  { (Optimize.c_check = property::make("c_check",1,Compile.it,Kernel._any,0));
    ;} 
  
  { (Optimize.need_protect = property::make("need_protect",1,Compile.it,Kernel._any,0));
    ;} 
  
  { (Optimize.member_code = property::make("member_code",1,Optimize.it,Kernel._any,0));
    ;} 
  
  { (Optimize.c_strict_code = property::make("c_strict_code",1,Compile.it,Kernel._any,0));
    ;} 
  
  { (Optimize.c_strict_check = property::make("c_strict_check",3,Compile.it,Kernel._any,0));
    (Optimize.c_strict_check->open = 3);
    ;} 
  
  { (Optimize.stupid_t = property::make("stupid_t",1,Compile.it,Kernel._any,0));
    ;} 
  
  { (Optimize.object_I = property::make("object!",1,Compile.it,Kernel._any,0));
    ;} 
  
  { (Optimize.anyObject_I = property::make("anyObject!",2,Compile.it,Kernel._any,0));
    (Optimize.anyObject_I->range = Kernel._object);
    ;} 
  
  { (Optimize.Cerror = property::make("Cerror",1,Compile.it,Kernel._any,0));
    ;} 
  
  { (Optimize.self_code = property::make("self_code",1,Compile.it,Kernel._any,0));
    ;} 
  
  { (Optimize.get_module = property::make("get_module",1,Compile.it,Kernel._any,0));
    ;} 
  
  { (Core.main = property::make("main",2,claire.it,Kernel._any,0));
    ;} 
  
  { (Optimize._to_protect = ClaireClass::make("to_protect",Language._Optimized_instruction,Compile.it));
    CL_ADD_SLOT(Optimize._to_protect,Compile_to_protect,Kernel.arg,arg,Kernel._any,CNULL);
    } 
  
  { (Optimize._to_CL = ClaireClass::make("to_CL",Language._Optimized_instruction,Compile.it));
    CL_ADD_SLOT(Optimize._to_CL,Compile_to_CL,Kernel.arg,arg,Kernel._any,CNULL);
    CL_ADD_SLOT(Optimize._to_CL,Compile_to_CL,Language.set_arg,set_arg,Kernel._class,CNULL);
    } 
  
  { (Optimize._to_C = ClaireClass::make("to_C",Language._Optimized_instruction,Compile.it));
    CL_ADD_SLOT(Optimize._to_C,Compile_to_C,Kernel.arg,arg,Kernel._any,CNULL);
    CL_ADD_SLOT(Optimize._to_C,Compile_to_C,Language.set_arg,set_arg,Kernel._class,CNULL);
    } 
  
  (Optimize._C_cast = ClaireClass::make("C_cast",Optimize._to_C,Compile.it));
  
  { (Optimize._Pattern = ClaireClass::make("Pattern",Core._Type,Optimize.it));
    CL_ADD_SLOT(Optimize._Pattern,Optimize_ClairePattern,Kernel.selector,selector,Kernel._property,CNULL);
    CL_ADD_SLOT(Optimize._Pattern,Optimize_ClairePattern,Kernel.arg,arg,Kernel._list,CNULL);
    } 
  
  { (Optimize.OPT = (Optimize_meta_OPT *) Optimize._meta_OPT->instantiate("OPT",claire.it));
    (Optimize.OPT->outfile = EXPORT((ClairePort *),Reader.STDIN->value));
    (Optimize.OPT->cinterface = EXPORT((ClairePort *),Reader.STDIN->value));
    (Optimize.OPT->ignore = set::alloc(9,_oid_(Core.index_I),
      _oid_(Core.set_index),
      _oid_(Optimize.object_I),
      _oid_(Core.base_I),
      _oid_(Core.set_base),
      _oid_(Core.push_I),
      _oid_(Optimize.anyObject_I),
      _oid_(Core.get_stack),
      _oid_(Core.put_stack)));
    (Optimize.OPT->to_remove = set::alloc(1,_oid_(Language.ClaireInterface)));
    (Optimize.OPT->knowns = set::alloc(Kernel._relation,2,_oid_(Core.arg1),_oid_(Core.arg2)));
    (Optimize.OPT->unsure = list::alloc(3,_oid_(_at_property1(Core._plus,Kernel._integer)),
      _oid_(_at_property1(Kernel._star,Kernel._integer)),
      _oid_(_at_property1(Kernel._dash,Kernel._integer))));
    (Optimize.OPT->simple_operations = set::alloc(Kernel.emptySet,4,_oid_(Kernel._dash),
      _oid_(Kernel._7),
      _oid_(Kernel._star),
      _oid_(Core._plus)));
    update_property(Optimize.non_identifiable_set,
      Optimize.OPT,
      31,
      Kernel._object,
      _oid_(set::alloc(Kernel._class,14,_oid_(Kernel._object),
        _oid_(Kernel._primitive),
        _oid_(Kernel._cl_import),
        _oid_(Kernel._string),
        _oid_(Kernel._port),
        _oid_(Kernel._collection),
        _oid_(Kernel._bag),
        _oid_(Kernel._tuple),
        _oid_(Kernel._set),
        _oid_(Kernel._list),
        _oid_(Kernel._any),
        _oid_(Kernel._type),
        _oid_(Kernel._float),
        _oid_(Kernel._void))));
    (Optimize.OPT->cfile = Kernel.cfalse);
    ;} 
  
  (Optimize.safe->addMethod(list::domain(1,Kernel._any),Kernel._any,
  	RETURN_ARG,_function_(safe_any2,"safe_any2"))->typing = _oid_(_function_(safe_any2_type,"safe_any2_type")));
  
  Optimize.c_type->addMethod(list::domain(1,Kernel._any),Kernel._type,
  	NEW_ALLOC+RETURN_ARG,_function_(c_type_any_Optimize,"c_type_any_Optimize"));
  
  Optimize.c_strict_code->addMethod(list::domain(2,Kernel._any,Kernel._class),Kernel._any,
  	NEW_ALLOC+RETURN_ARG,_function_(c_strict_code_any,"c_strict_code_any"));
  
  Optimize.c_strict_check->addMethod(list::domain(2,Kernel._any,Kernel._class),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_strict_check_any_Optimize,"c_strict_check_any_Optimize"));
  
  Optimize.c_code->addMethod(list::domain(2,Kernel._any,Kernel._class),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_code_any1_Optimize,"c_code_any1_Optimize"));
  
  Optimize.c_code->addMethod(list::domain(1,Kernel._any),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_code_any2_Optimize,"c_code_any2_Optimize"));
  
  Optimize.c_gc_ask->addMethod(list::domain(1,Kernel._any),Kernel._boolean,
  	NEW_ALLOC,_function_(c_gc_ask_any,"c_gc?_any"));
  
  Optimize.c_sort->addMethod(list::domain(1,Kernel._any),Kernel._class,
  	NEW_ALLOC+RETURN_ARG,_function_(c_sort_any_Optimize,"c_sort_any_Optimize"));
  
  Optimize.selector_psort->addMethod(list::domain(1,Language._Call),Kernel._class,
  	RETURN_ARG,_function_(selector_psort_Call,"selector_psort_Call"));
  
  Optimize.c_status->addMethod(list::domain(2,Kernel._any,Kernel._list),Kernel._integer,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(c_status_any,"c_status_any"));
  
  Optimize.c_or->addMethod(list::domain(2,Kernel._integer,Kernel._integer),Kernel._integer,
  	0,_function_(c_or_integer,"c_or_integer"));
  
  Optimize.c_or->addMethod(list::domain(1,Kernel._list),Kernel._integer,
  	0,_function_(c_or_list,"c_or_list"))->inlineDef("lambda[(l:list),let d := 0 in (for x in l d := c_or(d, x), d)]");
  
  Optimize.status_I->addMethod(list::domain(1,Kernel._restriction),Kernel._integer,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(status_I_restriction,"status!_restriction"));
  
  Optimize.c_return->addMethod(list::domain(2,Kernel._integer,Kernel._integer),Kernel._integer,
  	0,_function_(c_return_integer,"c_return_integer"));
  
  Optimize.c_status->addMethod(list::domain(1,Kernel._property),Kernel._integer,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(c_status_property,"c_status_property"));
  
  { (CLREAD(method,_at_property1(Core.vmatch_ask,Kernel._any),status) = 64);
    (CLREAD(method,_at_property1(Core.pop_debug,Kernel._property),status) = 0);
    (CLREAD(method,_at_property1(Core.matching_ask,Kernel._list),status) = 64);
    (CLREAD(method,_at_property1(Core.eval_message,Kernel._property),status) = (*Language.bit_vector)(1,
      3,
      6));
    (CLREAD(method,_at_property1(Kernel.nth,Kernel._bag),status) = 16);
    (CLREAD(method,_at_property1(Core.eval,Kernel._any),status) = 2);
    (CLREAD(method,_at_property1(Core.self_eval,Language._Call),status) = 64);
    (CLREAD(method,_at_property1(Core.self_eval,Language._If),status) = 64);
    (CLREAD(method,_at_property1(Core.self_eval,Language._Do),status) = 64);
    } 
  
  Optimize.showstatus->addMethod(list::domain(1,Kernel._method),Kernel._any,
  	NEW_ALLOC,_function_(showstatus_method2,"showstatus_method2"));
  
  Optimize.s_test->addMethod(list::domain(1,Kernel._method),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(s_test_method2,"s_test_method2"));
  
  Optimize.legal_ask->addMethod(list::domain(2,Kernel._module,Kernel._any),Kernel._boolean,
  	NEW_ALLOC+SLOT_UPDATE,_function_(legal_ask_module,"legal?_module"));
  
  Optimize.legal_ask->addMethod(list::domain(2,Kernel._environment,Kernel._any),Kernel._any,
  	0,_function_(legal_ask_environment,"legal?_environment"));
  
  Optimize.c_register->addMethod(list::domain(1,U_type(Kernel._thing,Kernel._class)),Kernel._any,
  	NEW_ALLOC,_function_(c_register_object,"c_register_object"));
  
  Optimize.c_register->addMethod(list::domain(1,Kernel._property),Kernel._any,
  	NEW_ALLOC+SLOT_UPDATE,_function_(c_register_property,"c_register_property"));
  
  Optimize.selector_register->addMethod(list::domain(1,Kernel._property),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+SAFE_RESULT,_function_(selector_register_property,"selector_register_property"));
  
  Optimize.allocate_ask->addMethod(list::domain(1,Kernel._property),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(allocate_ask_property,"allocate?_property"));
  
  Optimize.stable_ask->addMethod(list::domain(1,Kernel._relation),Kernel._boolean,
  	NEW_ALLOC+SLOT_UPDATE,_function_(stable_ask_relation,"stable?_relation"));
  
  Optimize.get_module->addMethod(list::domain(1,U_type(Kernel._thing,Kernel._class)),Kernel._any,
  	NEW_ALLOC,_function_(get_module_object,"get_module_object"));
  
  Reader.known_I->addMethod(list::domain(1,Kernel._listargs),Kernel._any,
  	NEW_ALLOC+SLOT_UPDATE+RETURN_ARG,_function_(known_I_listargs,"known!_listargs"));
  
  Optimize.oload->addMethod(list::domain(1,Kernel._module),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(oload_module,"oload_module"));
  
  Optimize.oload->addMethod(list::domain(1,Kernel._string),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(oload_string,"oload_string"));
  
  Optimize.turbo->addMethod(list::domain(1,Kernel._module),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(turbo_module,"turbo_module"));
  
  Optimize.stats->addMethod(list::domain(1,Optimize._meta_OPT),Kernel._void,
  	0,_function_(stats_meta_OPT,"stats_meta_OPT"));
  
  Kernel.self_print->addMethod(list::domain(1,Optimize._to_protect),Kernel._void,
  	NEW_ALLOC,_function_(self_print_to_protect,"self_print_to_protect"));
  
  Core.self_eval->addMethod(list::domain(1,Optimize._to_protect),Kernel._any,
  	NEW_ALLOC,_function_(self_eval_to_protect,"self_eval_to_protect"));
  
  Kernel.self_print->addMethod(list::domain(1,Optimize._to_CL),Kernel._void,
  	NEW_ALLOC,_function_(self_print_to_CL,"self_print_to_CL"));
  
  Optimize.c_type->addMethod(list::domain(1,Optimize._to_CL),Kernel._type,
  	NEW_ALLOC+RETURN_ARG,_function_(c_type_to_CL_Optimize,"c_type_to_CL_Optimize"));
  
  Optimize.c_gc_ask->addMethod(list::domain(1,Optimize._to_CL),Kernel._boolean,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(c_gc_ask_to_CL,"c_gc?_to_CL"));
  
  Kernel.self_print->addMethod(list::domain(1,Optimize._to_C),Kernel._void,
  	NEW_ALLOC,_function_(self_print_to_C,"self_print_to_C"));
  
  Optimize.c_gc_ask->addMethod(list::domain(1,Optimize._to_C),Kernel._boolean,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(c_gc_ask_to_C,"c_gc?_to_C"));
  
  Optimize.c_type->addMethod(list::domain(1,Optimize._to_C),Kernel._type,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_type_to_C_Optimize,"c_type_to_C_Optimize"));
  
  Kernel.self_print->addMethod(list::domain(1,Optimize._C_cast),Kernel._void,
  	NEW_ALLOC,_function_(self_print_C_cast,"self_print_C_cast"));
  
  Optimize.c_gc_ask->addMethod(list::domain(1,Optimize._C_cast),Kernel._boolean,
  	NEW_ALLOC,_function_(c_gc_ask_C_cast,"c_gc?_C_cast"));
  
  Optimize.c_type->addMethod(list::domain(1,Optimize._C_cast),Kernel._type,
  	RETURN_ARG,_function_(c_type_C_cast_Optimize,"c_type_C_cast_Optimize"));
  
  Optimize.c_code->addMethod(list::domain(2,Optimize._C_cast,Kernel._class),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_code_C_cast_Optimize,"c_code_C_cast_Optimize"));
  
  Kernel.self_print->addMethod(list::domain(1,Optimize._Pattern),Kernel._void,
  	NEW_ALLOC,_function_(self_print_Pattern,"self_print_Pattern"));
  
  Kernel._Z->addMethod(list::domain(2,Kernel._any,Optimize._Pattern),Kernel._boolean,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(_Z_any3,"%_any3"));
  
  Core.glb->addMethod(list::domain(2,Optimize._Pattern,Kernel._type),Kernel._type,
  	0,_function_(glb_Pattern,"glb_Pattern"));
  
  Core.less_ask->addMethod(list::domain(2,Optimize._Pattern,Kernel._any),Kernel._boolean,
  	NEW_ALLOC,_function_(less_ask_Pattern,"less?_Pattern"));
  
  Core.less_ask->addMethod(list::domain(2,Kernel._any,Optimize._Pattern),Kernel._boolean,
  	NEW_ALLOC,_function_(less_ask_any,"less?_any"));
  
  Kernel.nth->addMethod(list::domain(2,Kernel._property,Kernel._tuple),Optimize._Pattern,
  	NEW_ALLOC,_function_(nth_property,"nth_property"));
  
  (Optimize._optUnion = ClaireClass::make("optUnion",Core._Union,Optimize.it));
  
  Optimize.warn->addMethod(list::domain(1,Kernel._void),Kernel._void,
  	NEW_ALLOC,_function_(warn_void,"warn_void"));
  
  Optimize.Cerror->addMethod(list::domain(2,Kernel._string,Kernel._listargs),Kernel.emptySet,
  	NEW_ALLOC,_function_(Cerror_string,"Cerror_string"));
  
  Optimize.notice->addMethod(list::domain(1,Kernel._void),Kernel._void,
  	0,_function_(notice_void,"notice_void"));
  
  Optimize.c_warn->addMethod(list::domain(2,Language._Call,Kernel._any),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(c_warn_Call,"c_warn_Call"));
  
  Optimize.c_warn->addMethod(list::domain(2,Language._Super,Kernel._any),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(c_warn_Super,"c_warn_Super"));
  
  Optimize.c_warn->addMethod(list::domain(3,Kernel._property,Kernel._list,Kernel._list),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(c_warn_property,"c_warn_property"));
  
  Optimize.c_warn->addMethod(list::domain(3,Language._Variable,Kernel._any,Kernel._type),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_warn_Variable,"c_warn_Variable"));
  
  Optimize.sort_equal->addMethod(list::domain(2,Kernel._class,Kernel._class),Kernel._any,
  	0,_function_(sort_equal_class,"sort=_class"));
  
  Optimize.psort->addMethod(list::domain(1,Kernel._any),Kernel._class,
  	NEW_ALLOC+RETURN_ARG,_function_(psort_any,"psort_any"));
  
  Optimize.osort->addMethod(list::domain(1,Kernel._any),Kernel._class,
  	NEW_ALLOC+RETURN_ARG,_function_(osort_any,"osort_any"));
  
  Core.sort->addMethod(list::domain(1,Language._Variable),Kernel._class,
  	NEW_ALLOC+RETURN_ARG,_function_(sort_Variable,"sort_Variable"));
  
  Optimize.stupid_t->addMethod(list::domain(1,Kernel._any),Kernel._class,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(stupid_t_any1,"stupid_t_any1"));
  
  Optimize.stupid_t->addMethod(list::domain(2,Kernel._any,Kernel._any),Kernel._boolean,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(stupid_t_any2,"stupid_t_any2"));
  
  Optimize.extended_ask->addMethod(list::domain(1,Kernel._type),Kernel._boolean,
  	0,_function_(extended_ask_type,"extended?_type"));
  
  Optimize.extends->addMethod(list::domain(1,Kernel._type),Kernel._type,
  	NEW_ALLOC,_function_(extends_type,"extends_type"));
  
  Optimize.sort_abstract_I->addMethod(list::domain(1,Kernel._type),Kernel._type,
  	NEW_ALLOC+RETURN_ARG,_function_(sort_abstract_I_type,"sort_abstract!_type"));
  
  Optimize.sort_abstract_ask->addMethod(list::domain(1,Kernel._type),Kernel._boolean,
  	0,_function_(sort_abstract_ask_type,"sort_abstract?_type"));
  
  Optimize.ptype->addMethod(list::domain(1,Kernel._type),Kernel._type,
  	RETURN_ARG,_function_(ptype_type,"ptype_type"));
  
  Optimize.pmember->addMethod(list::domain(1,Kernel._type),Kernel._type,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(pmember_type,"pmember_type"));
  
  Optimize.enumerate_code->addMethod(list::domain(2,Kernel._any,Kernel._type),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(enumerate_code_any,"enumerate_code_any"));
  
  Optimize.range_infers_for->addMethod(list::domain(3,Language._Variable,Kernel._type,Kernel._type),Kernel._any,
  	NEW_ALLOC+SLOT_UPDATE+RETURN_ARG,_function_(range_infers_for_Variable,"range_infers_for_Variable"));
  
  Optimize.range_infers->addMethod(list::domain(2,Language._Variable,Kernel._type),Kernel._any,
  	NEW_ALLOC+SLOT_UPDATE+RETURN_ARG,_function_(range_infers_Variable,"range_infers_Variable"));
  
  Optimize.range_infer_case->addMethod(list::domain(2,Kernel._any,Kernel._type),Kernel._void,
  	NEW_ALLOC+SLOT_UPDATE+RETURN_ARG,_function_(range_infer_case_any,"range_infer_case_any"));
  
  Optimize.c_check->addMethod(list::domain(2,Kernel._any,Kernel._any),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_check_any,"c_check_any"));
  
  Optimize.range_sets->addMethod(list::domain(2,Kernel._any,Kernel._type),Kernel._void,
  	NEW_ALLOC+SLOT_UPDATE+RETURN_ARG,_function_(range_sets_any,"range_sets_any"));
  
  Optimize.c_srange->addMethod(list::domain(1,Kernel._method),Kernel._class,
  	RETURN_ARG,_function_(c_srange_method,"c_srange_method"));
  
  Optimize.nativeVar_ask->addMethod(list::domain(1,Core._global_variable),Kernel._boolean,
  	NEW_ALLOC,_function_(nativeVar_ask_global_variable,"nativeVar?_global_variable"));
  
  Optimize.return_type->addMethod(list::domain(1,Kernel._any),Kernel._type,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(return_type_any,"return_type_any"));
  
  Optimize.c_code->addMethod(list::domain(2,Core._Type,Kernel._class),Kernel._any,
  	NEW_ALLOC+RETURN_ARG,_function_(c_code_Type_Optimize,"c_code_Type_Optimize"));
  
  Optimize.self_code->addMethod(list::domain(1,Core._subtype),Kernel._any,
  	NEW_ALLOC,_function_(self_code_subtype,"self_code_subtype"));
  
  Optimize.self_code->addMethod(list::domain(1,Core._Param),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(self_code_Param,"self_code_Param"));
  
  Optimize.self_code->addMethod(list::domain(1,Core._Union),Kernel._any,
  	NEW_ALLOC,_function_(self_code_Union,"self_code_Union"));
  
  Optimize.self_code->addMethod(list::domain(1,Core._Interval),Kernel._any,
  	NEW_ALLOC,_function_(self_code_Interval,"self_code_Interval"));
  
  Optimize.self_code->addMethod(list::domain(1,Core._Reference),Kernel._any,
  	NEW_ALLOC,_function_(self_code_Reference,"self_code_Reference"));
  
  Optimize.self_code->addMethod(list::domain(1,Optimize._Pattern),Kernel._any,
  	NEW_ALLOC,_function_(self_code_Pattern,"self_code_Pattern"));
  
  Optimize.member_code->addMethod(list::domain(2,Kernel._class,Kernel._any),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(member_code_class2,"member_code_class2"));
  
  Optimize.member_code->addMethod(list::domain(2,Core._Type,Kernel._any),Kernel._any,
  	NEW_ALLOC,_function_(member_code_Type,"member_code_Type"));
  
  Optimize.member_code->addMethod(list::domain(2,Core._Union,Kernel._any),Kernel._any,
  	NEW_ALLOC,_function_(member_code_Union,"member_code_Union"));
  
  Optimize.member_code->addMethod(list::domain(2,Core._Interval,Kernel._any),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(member_code_Interval,"member_code_Interval"));
  
  Optimize.member_code->addMethod(list::domain(2,Core._Param,Kernel._any),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(member_code_Param,"member_code_Param"));
  
  Optimize.member_code->addMethod(list::domain(2,Kernel._tuple,Kernel._any),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(member_code_tuple,"member_code_tuple"));
  
  Optimize.member_code->addMethod(list::domain(2,Kernel._any,Kernel._any),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(member_code_any,"member_code_any"));
  
  Kernel._Z->addMethod(list::domain(2,Kernel._any,nth_property(Kernel._dot_dot,tuple::alloc(2,_oid_(Kernel._any),_oid_(Kernel._any)))),Kernel._boolean,
  	NEW_ALLOC,_function_(_Z_any4,"%_any4"))->inlineDef("lambda[(x:any,y:..[tuple(any,any)]),(x <= eval(y.args[2]) & eval(y.args[1]) <= x)]");
  
  Kernel._Z->addMethod(list::domain(2,Kernel._any,nth_property(Core.but,tuple::alloc(2,_oid_(Kernel._any),_oid_(Kernel._any)))),Kernel._boolean,
  	NEW_ALLOC,_function_(_Z_any5,"%_any5"))->inlineDef("lambda[(x:any,y:but[tuple(any,any)]),(x % eval(y.args[1]) & x != eval(y.args[2]))]");
  
  Optimize.gcsafe_ask->addMethod(list::domain(1,Kernel._class),Kernel._boolean,
  	0,_function_(gcsafe_ask_class,"gcsafe?_class"));
  
  Optimize.gcsafe_ask->addMethod(list::domain(1,Kernel._type),Kernel._boolean,
  	NEW_ALLOC,_function_(gcsafe_ask_type,"gcsafe?_type"));
  
  Optimize.gcsafe_ask->addMethod(list::domain(1,Kernel._property),Kernel._boolean,
  	NEW_ALLOC,_function_(gcsafe_ask_property,"gcsafe?_property"));
  
  Optimize.c_gc_I->addMethod(list::domain(1,Kernel._any),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_gc_I_any1,"c_gc!_any1"));
  
  Optimize.c_gc_I->addMethod(list::domain(2,Kernel._any,Kernel._type),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_gc_I_any2,"c_gc!_any2"));
  
  Optimize.need_protect->addMethod(list::domain(1,Kernel._any),Kernel._boolean,
  	NEW_ALLOC+RETURN_ARG,_function_(need_protect_any,"need_protect_any"));
  
  Optimize.Variable_I->addMethod(list::domain(3,Kernel._symbol,Kernel._integer,Kernel._type),Language._Variable,
  	NEW_ALLOC,_function_(Variable_I_symbol,"Variable!_symbol"));
  
  Optimize.get_indexed->addMethod(list::domain(1,Kernel._class),Kernel._list,
  	RETURN_ARG,_function_(get_indexed_class_Optimize,"get_indexed_class_Optimize"));
  
  Optimize.designated_ask->addMethod(list::domain(1,Kernel._any),Kernel._boolean,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(designated_ask_any,"designated?_any"));
  
  Optimize.gc_register->addMethod(list::domain(1,Language._Variable),Kernel._any,
  	SLOT_UPDATE,_function_(gc_register_Variable,"gc_register_Variable"));
  
  Optimize.gc_register->addMethod(list::domain(2,Language._Variable,Kernel._any),Kernel._any,
  	NEW_ALLOC+SLOT_UPDATE,_function_(gc_register_Variable2,"gc_register_Variable2"));
  
  Optimize.inner2outer_ask->addMethod(list::domain(1,Kernel._any),Kernel._boolean,
  	NEW_ALLOC,_function_(inner2outer_ask_any,"inner2outer?_any"));
  
  Optimize.identifiable_ask->addMethod(list::domain(1,Kernel._any),Kernel._boolean,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(identifiable_ask_any,"identifiable?_any"));
  
  Optimize.c_inline->addMethod(list::domain(3,Kernel._method,Kernel._list,Kernel._class),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_inline_method1,"c_inline_method1"));
  
  Optimize.c_inline->addMethod(list::domain(2,Kernel._method,Kernel._list),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_inline_method2,"c_inline_method2"));
  
  Optimize.c_inline_arg_ask->addMethod(list::domain(1,Kernel._any),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_inline_arg_ask_any,"c_inline_arg?_any"));
  
  Optimize.c_substitution->addMethod(list::domain(4,Kernel._any,
    nth_class1(Kernel._list,Language._Variable),
    Kernel._list,
    Kernel._boolean),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_substitution_any,"c_substitution_any"));
  
  Core.eval->addMethod(list::domain(2,Kernel._any,Kernel._class),Kernel._any,
  	NEW_ALLOC,_function_(eval_any2,"eval_any2"));
  
  Optimize.bound_variables->addMethod(list::domain(1,Kernel._any),Kernel._list,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(bound_variables_any,"bound_variables_any"));
  
  Optimize.c_boolean->addMethod(list::domain(1,Kernel._any),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_boolean_any,"c_boolean_any"));
  
  { (Optimize.ambiguous = (keyword *) Kernel._keyword->instantiate("ambiguous",Optimize.it));
    ;} 
  
  Optimize.restriction_I->addMethod(list::domain(3,Kernel._property,Kernel._list,Kernel._boolean),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(restriction_I_property,"restriction!_property"));
  
  Optimize.restriction_I->addMethod(list::domain(3,Kernel._list,Kernel._list,Kernel._boolean),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(restriction_I_list,"restriction!_list"));
  
  Optimize.restriction_I->addMethod(list::domain(3,Kernel._class,Kernel._list,Kernel._list),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(restriction_I_class,"restriction!_class"));
  
  Optimize.use_range->addMethod(list::domain(2,Kernel._method,Kernel._list),Kernel._type,
  	NEW_ALLOC+SLOT_UPDATE,_function_(use_range_method,"use_range_method"));
  
  Optimize.c_type->addMethod(list::domain(1,Language._Call),Kernel._type,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_type_Call_Optimize,"c_type_Call_Optimize"));
  
  Optimize.c_code->addMethod(list::domain(1,Language._Call),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_code_Call_Optimize,"c_code_Call_Optimize"));
  
  Optimize.c_code_call->addMethod(list::domain(2,Language._Call,Kernel._class),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_code_call_Call,"c_code_call_Call"));
  
  Optimize.open_message->addMethod(list::domain(2,Kernel._property,Kernel._list),Language._Call,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(open_message_property,"open_message_property"));
  
  Optimize.c_gc_ask->addMethod(list::domain(1,Language._Call),Kernel._boolean,
  	NEW_ALLOC+SLOT_UPDATE,_function_(c_gc_ask_Call,"c_gc?_Call"));
  
  Optimize.daccess->addMethod(list::domain(2,Kernel._any,Kernel._boolean),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(daccess_any,"daccess_any"));
  
  Optimize.c_gc_ask->addMethod(list::domain(1,Language._Call_slot),Kernel._boolean,
  	NEW_ALLOC,_function_(c_gc_ask_Call_slot,"c_gc?_Call_slot"));
  
  Optimize.c_type->addMethod(list::domain(1,Language._Call_slot),Kernel._type,
  	RETURN_ARG,_function_(c_type_Call_slot_Optimize,"c_type_Call_slot_Optimize"));
  
  Optimize.c_type->addMethod(list::domain(1,Language._Call_table),Kernel._type,
  	RETURN_ARG,_function_(c_type_Call_table_Optimize,"c_type_Call_table_Optimize"));
  
  Optimize.c_type->addMethod(list::domain(1,Language._Call_array),Kernel._type,
  	RETURN_ARG,_function_(c_type_Call_array_Optimize,"c_type_Call_array_Optimize"));
  
  Optimize.c_code_write->addMethod(list::domain(1,Language._Call),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_code_write_Call,"c_code_write_Call"));
  
  Optimize.c_code_hold->addMethod(list::domain(4,Kernel._property,
    Kernel._any,
    Kernel._any,
    Kernel._boolean),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_code_hold_property,"c_code_hold_property"));
  
  Optimize.c_code_add->addMethod(list::domain(1,Language._Call),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_code_add_Call,"c_code_add_Call"));
  
  Optimize.c_code_add_bag->addMethod(list::domain(1,Language._Call),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_code_add_bag_Call,"c_code_add_bag_Call"));
  
  Optimize.c_code_delete->addMethod(list::domain(1,Language._Call),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_code_delete_Call,"c_code_delete_Call"));
  
  Optimize.c_code_not->addMethod(list::domain(1,Language._Select),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_code_not_Select,"c_code_not_Select"));
  
  Optimize.c_code_belong->addMethod(list::domain(1,Language._Call),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_code_belong_Call,"c_code_belong_Call"));
  
  Optimize.c_code_nth->addMethod(list::domain(1,Language._Call),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_code_nth_Call,"c_code_nth_Call"));
  
  Optimize.c_code_table->addMethod(list::domain(1,Language._Call),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_code_table_Call,"c_code_table_Call"));
  
  Optimize.c_code_array->addMethod(list::domain(1,Language._Call),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_code_array_Call,"c_code_array_Call"));
  
  Optimize.Update_ask->addMethod(list::domain(3,Kernel._relation,Kernel._any,Kernel._any),Kernel._boolean,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(Update_ask_relation,"Update?_relation"));
  
  Optimize.Update_ask->addMethod(list::domain(2,Kernel._relation,Kernel._relation),Kernel._boolean,
  	0,_function_(Update_ask_relation2,"Update?_relation2"));
  
  Optimize.c_type->addMethod(list::domain(1,Language._Update),Kernel._type,
  	0,_function_(c_type_Update_Optimize,"c_type_Update_Optimize"));
  
  Optimize.c_code_method->addMethod(list::domain(3,Kernel._method,Kernel._list,Kernel._list),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_code_method_method1,"c_code_method_method1"));
  
  Optimize.c_code_method->addMethod(list::domain(4,Kernel._method,
    Kernel._list,
    Kernel._list,
    Kernel._class),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_code_method_method2,"c_code_method_method2"));
  
  Optimize.Call_method_I->addMethod(list::domain(2,Kernel._method,Kernel._list),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(Call_method_I_method,"Call_method!_method"));
  
  Optimize.c_type->addMethod(list::domain(1,Language._Call_method),Kernel._type,
  	NEW_ALLOC+SLOT_UPDATE,_function_(c_type_Call_method_Optimize,"c_type_Call_method_Optimize"));
  
  Optimize.c_code->addMethod(list::domain(1,Language._Call_method),Kernel._any,
  	RETURN_ARG,_function_(c_code_Call_method_Optimize,"c_code_Call_method_Optimize"));
  
  Optimize.c_gc_ask->addMethod(list::domain(1,Language._Call_method),Kernel._boolean,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(c_gc_ask_Call_method,"c_gc?_Call_method"));
  
  { (Optimize.functional_I = property::make("functional!",3,Compile.it,Kernel._any,0));
    (Optimize.functional_I->open = 3);
    ;} 
  
  Optimize.functional_I->addMethod(list::domain(1,Kernel._method),Kernel._function,
  	NEW_ALLOC,_function_(functional_I_method_Optimize,"functional!_method_Optimize"));
  
  Optimize.nth_type_check->addMethod(list::domain(3,Kernel._type,Kernel._type,Kernel._type),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(nth_type_check_type,"nth_type_check_type"))->inlineDef("lambda[(tl:type,ti:type,tx:type),(if not(tx <= member(tl)) (Compile/warn(), trace(2, \"unsafe update on bag: type ~S into ~S [252]\\n\", tx, tl)) else false, tx)]");
  
  (CLREAD(method,_at_property1(Kernel.nth_equal,Kernel._list),typing) = _oid_(Optimize.nth_type_check));
  
  Optimize.c_inline_ask->addMethod(list::domain(2,Kernel._method,Kernel._list),Kernel._boolean,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(c_inline_ask_method,"c_inline?_method"));
  
  Optimize.inline_optimize_ask->addMethod(list::domain(1,Language._Call),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(inline_optimize_ask_Call,"inline_optimize?_Call"));
  
  Optimize.c_type->addMethod(list::domain(1,Language._Assign),Kernel._type,
  	NEW_ALLOC+RETURN_ARG,_function_(c_type_Assign_Optimize,"c_type_Assign_Optimize"));
  
  Optimize.c_code->addMethod(list::domain(1,Language._Assign),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(c_code_Assign_Optimize,"c_code_Assign_Optimize"));
  
  Optimize.c_gc_ask->addMethod(list::domain(1,Language._Assign),Kernel._boolean,
  	NEW_ALLOC,_function_(c_gc_ask_Assign,"c_gc?_Assign"));
  
  Optimize.c_type->addMethod(list::domain(1,Language._Gassign),Kernel._type,
  	NEW_ALLOC+RETURN_ARG,_function_(c_type_Gassign_Optimize,"c_type_Gassign_Optimize"));
  
  Optimize.c_code->addMethod(list::domain(1,Language._Gassign),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(c_code_Gassign_Optimize,"c_code_Gassign_Optimize"));
  
  Optimize.c_gc_ask->addMethod(list::domain(1,Language._Gassign),Kernel._boolean,
  	NEW_ALLOC,_function_(c_gc_ask_Gassign,"c_gc?_Gassign"));
  
  Optimize.c_type->addMethod(list::domain(1,Language._And),Kernel._type,
  	0,_function_(c_type_And_Optimize,"c_type_And_Optimize"));
  
  Optimize.c_code->addMethod(list::domain(1,Language._And),Kernel._any,
  	NEW_ALLOC,_function_(c_code_And_Optimize,"c_code_And_Optimize"));
  
  Optimize.c_type->addMethod(list::domain(1,Language._Or),Kernel._type,
  	0,_function_(c_type_Or_Optimize,"c_type_Or_Optimize"));
  
  Optimize.c_code->addMethod(list::domain(1,Language._Or),Kernel._any,
  	NEW_ALLOC,_function_(c_code_Or_Optimize,"c_code_Or_Optimize"));
  
  Optimize.c_type->addMethod(list::domain(1,Language._Quote),Kernel._type,
  	0,_function_(c_type_Quote_Optimize,"c_type_Quote_Optimize"));
  
  Optimize.c_code->addMethod(list::domain(1,Language._Quote),Kernel.emptySet,
  	NEW_ALLOC,_function_(c_code_Quote_Optimize,"c_code_Quote_Optimize"));
  
  Optimize.c_type->addMethod(list::domain(1,Optimize._to_protect),Kernel._type,
  	NEW_ALLOC+RETURN_ARG,_function_(c_type_to_protect_Optimize,"c_type_to_protect_Optimize"));
  
  Optimize.c_code->addMethod(list::domain(1,Optimize._to_protect),Kernel._any,
  	SAFE_RESULT,_function_(c_code_to_protect_Optimize,"c_code_to_protect_Optimize"));
  
  Optimize.c_gc_ask->addMethod(list::domain(1,Optimize._to_protect),Kernel._boolean,
  	0,_function_(c_gc_ask_to_protect,"c_gc?_to_protect"));
  
  Optimize.c_type->addMethod(list::domain(1,Language._Return),Kernel._type,
  	0,_function_(c_type_Return_Optimize,"c_type_Return_Optimize"));
  
  Optimize.c_code->addMethod(list::domain(1,Language._Return),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(c_code_Return_Optimize,"c_code_Return_Optimize"));
  
  Optimize.c_type->addMethod(list::domain(1,Language._Handle),Kernel._type,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_type_Handle_Optimize,"c_type_Handle_Optimize"));
  
  Optimize.c_code->addMethod(list::domain(2,Language._Handle,Kernel._class),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(c_code_Handle_Optimize,"c_code_Handle_Optimize"));
  
  Optimize.c_gc_ask->addMethod(list::domain(1,Language._Handle),Kernel._boolean,
  	NEW_ALLOC,_function_(c_gc_ask_Handle,"c_gc?_Handle"));
  
  Optimize.c_type->addMethod(list::domain(1,Language._Cast),Kernel._type,
  	RETURN_ARG,_function_(c_type_Cast_Optimize,"c_type_Cast_Optimize"));
  
  Optimize.c_code->addMethod(list::domain(1,Language._Cast),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_code_Cast_Optimize,"c_code_Cast_Optimize"));
  
  Optimize.c_type->addMethod(list::domain(1,Language._Super),Kernel._type,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_type_Super_Optimize,"c_type_Super_Optimize"));
  
  Optimize.c_code->addMethod(list::domain(1,Language._Super),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_code_Super_Optimize,"c_code_Super_Optimize"));
  
  { (Optimize._Call_function2 = ClaireClass::make("Call_function2",Language._Optimized_instruction,Optimize.it));
    CL_ADD_SLOT(Optimize._Call_function2,Optimize_Call_function2,Kernel.arg,arg,Kernel._function,CNULL);
    CL_ADD_SLOT(Optimize._Call_function2,Optimize_Call_function2,Core.args,args,Kernel._list,CNULL);
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Optimize._Call_function2),Kernel._void,
  	NEW_ALLOC+SLOT_UPDATE,_function_(self_print_Call_function2,"self_print_Call_function2"));
  
  Optimize.c_type->addMethod(list::domain(1,Optimize._Call_function2),Kernel._type,
  	0,_function_(c_type_Call_function2_Optimize,"c_type_Call_function2_Optimize"));
  
  Optimize.c_code->addMethod(list::domain(1,Optimize._Call_function2),Kernel._any,
  	NEW_ALLOC,_function_(c_code_Call_function2_Optimize,"c_code_Call_function2_Optimize"));
  
  Optimize.c_code->addMethod(list::domain(1,Language._Assert),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_code_Assert_Optimize,"c_code_Assert_Optimize"));
  
  Optimize.c_code->addMethod(list::domain(1,Language._Trace),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_code_Trace_Optimize,"c_code_Trace_Optimize"));
  
  Optimize.c_type->addMethod(list::domain(1,Language._Assert),Kernel._type,
  	0,_function_(c_type_Assert_Optimize,"c_type_Assert_Optimize"));
  
  Optimize.c_type->addMethod(list::domain(1,Language._Trace),Kernel._type,
  	0,_function_(c_type_Trace_Optimize,"c_type_Trace_Optimize"));
  
  Optimize.c_type->addMethod(list::domain(1,Language._Branch),Kernel._type,
  	0,_function_(c_type_Branch_Optimize,"c_type_Branch_Optimize"));
  
  Optimize.c_code->addMethod(list::domain(1,Language._Branch),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_code_Branch_Optimize,"c_code_Branch_Optimize"));
  
  Optimize.c_code->addMethod(list::domain(2,Language._Macro,Kernel._class),Kernel._any,
  	NEW_ALLOC+RETURN_ARG,_function_(c_code_Macro_Optimize,"c_code_Macro_Optimize"));
  
  Optimize.c_type->addMethod(list::domain(1,Language._Macro),Kernel._type,
  	NEW_ALLOC+RETURN_ARG,_function_(c_type_Macro_Optimize,"c_type_Macro_Optimize"));
  
  Optimize.c_type->addMethod(list::domain(1,Language._Printf),Kernel._type,
  	0,_function_(c_type_Printf_Optimize,"c_type_Printf_Optimize"));
  
  Optimize.c_code->addMethod(list::domain(1,Language._Printf),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_code_Printf_Optimize,"c_code_Printf_Optimize"));
  
  Optimize.c_type->addMethod(list::domain(1,Language._Error),Kernel._type,
  	0,_function_(c_type_Error_Optimize,"c_type_Error_Optimize"));
  
  Optimize.c_code->addMethod(list::domain(1,Language._Error),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(c_code_Error_Optimize,"c_code_Error_Optimize"));
  
  Optimize.extendedTest_ask->addMethod(list::domain(1,Language._If),Kernel._type,
  	NEW_ALLOC+RETURN_ARG,_function_(extendedTest_ask_If,"extendedTest?_If"));
  
  Optimize.c_type->addMethod(list::domain(1,Language._If),Kernel._type,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(c_type_If_Optimize,"c_type_If_Optimize"));
  
  { global_variable * v2072 = (Optimize.PENIBLE = (global_variable *) Core._global_variable->instantiate("PENIBLE",claire.it));
    (v2072->range = Kernel._boolean);
    (v2072->value = Kernel.cfalse);
    close_global_variable(v2072);
    } 
  
  Optimize.c_code->addMethod(list::domain(2,Language._If,Kernel._class),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(c_code_If_Optimize,"c_code_If_Optimize"));
  
  Optimize.c_gc_ask->addMethod(list::domain(1,Language._If),Kernel._boolean,
  	NEW_ALLOC,_function_(c_gc_ask_If,"c_gc?_If"));
  
  Optimize.c_type->addMethod(list::domain(1,Language._Case),Kernel._type,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(c_type_Case_Optimize,"c_type_Case_Optimize"));
  
  Optimize.c_code->addMethod(list::domain(2,Language._Case,Kernel._class),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(c_code_Case_Optimize,"c_code_Case_Optimize"));
  
  Optimize.c_type->addMethod(list::domain(1,Language._Do),Kernel._type,
  	NEW_ALLOC+RETURN_ARG,_function_(c_type_Do_Optimize,"c_type_Do_Optimize"));
  
  Optimize.c_code->addMethod(list::domain(2,Language._Do,Kernel._class),Kernel._any,
  	NEW_ALLOC,_function_(c_code_Do_Optimize,"c_code_Do_Optimize"));
  
  Optimize.c_gc_ask->addMethod(list::domain(1,Language._Do),Kernel._boolean,
  	NEW_ALLOC,_function_(c_gc_ask_Do,"c_gc?_Do"));
  
  Optimize.c_type->addMethod(list::domain(1,Language._Let),Kernel._type,
  	NEW_ALLOC+SLOT_UPDATE+RETURN_ARG,_function_(c_type_Let_Optimize,"c_type_Let_Optimize"));
  
  Optimize.c_code->addMethod(list::domain(2,Language._Let,Kernel._class),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(c_code_Let_Optimize,"c_code_Let_Optimize"));
  
  Optimize.c_type->addMethod(list::domain(1,Language._When),Kernel._type,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_type_When_Optimize,"c_type_When_Optimize"));
  
  Optimize.c_code->addMethod(list::domain(2,Language._When,Kernel._class),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_code_When_Optimize,"c_code_When_Optimize"));
  
  Optimize.c_gc_ask->addMethod(list::domain(1,Language._Let),Kernel._boolean,
  	NEW_ALLOC,_function_(c_gc_ask_Let,"c_gc?_Let"));
  
  Optimize.c_type->addMethod(list::domain(1,Language._For),Kernel._type,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_type_For_Optimize,"c_type_For_Optimize"));
  
  Optimize.infers_from->addMethod(list::domain(2,Kernel._type,Kernel._any),Kernel._type,
  	NEW_ALLOC+RETURN_ARG,_function_(infers_from_type2,"infers_from_type2"));
  
  Optimize.c_code->addMethod(list::domain(2,Language._For,Kernel._class),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(c_code_For_Optimize,"c_code_For_Optimize"));
  
  Optimize.c_code_multiple->addMethod(list::domain(3,Language._For,Kernel._type,Kernel._class),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_code_multiple_For,"c_code_multiple_For"));
  
  Optimize.c_gc_ask->addMethod(list::domain(1,Language._Iteration),Kernel._boolean,
  	0,_function_(c_gc_ask_Iteration,"c_gc?_Iteration"));
  
  Optimize.c_type->addMethod(list::domain(1,Language._Iteration),Kernel._type,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_type_Iteration_Optimize,"c_type_Iteration_Optimize"));
  
  Optimize.c_code->addMethod(list::domain(1,Language._Iteration),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_code_Iteration_Optimize,"c_code_Iteration_Optimize"));
  
  Optimize.c_code->addMethod(list::domain(1,Language._Select),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(c_code_Select_Optimize,"c_code_Select_Optimize"));
  
  Optimize.c_code->addMethod(list::domain(1,Language._Lselect),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(c_code_Lselect_Optimize,"c_code_Lselect_Optimize"));
  
  Optimize.c_code_select->addMethod(list::domain(2,Language._Iteration,Kernel._class),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(c_code_select_Iteration,"c_code_select_Iteration"));
  
  Optimize.inner_select->addMethod(list::domain(4,Language._Iteration,
    Kernel._any,
    Kernel._any,
    Kernel._any),Kernel._any,
  	NEW_ALLOC,_function_(inner_select_Iteration,"inner_select_Iteration"));
  
  Optimize.c_type->addMethod(list::domain(1,Language._Exists),Kernel._type,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(c_type_Exists_Optimize,"c_type_Exists_Optimize"));
  
  Optimize.c_code->addMethod(list::domain(2,Language._Exists,Kernel._class),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_code_Exists_Optimize,"c_code_Exists_Optimize"));
  
  Optimize.c_type->addMethod(list::domain(1,Language._Image),Kernel._type,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_type_Image_Optimize,"c_type_Image_Optimize"));
  
  Optimize.c_type->addMethod(list::domain(1,Language._Select),Kernel._type,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_type_Select_Optimize,"c_type_Select_Optimize"));
  
  Optimize.c_type->addMethod(list::domain(1,Language._Lselect),Kernel._type,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_type_Lselect_Optimize,"c_type_Lselect_Optimize"));
  
  Optimize.c_type->addMethod(list::domain(1,Language._While),Kernel._type,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_type_While_Optimize,"c_type_While_Optimize"));
  
  Optimize.c_code->addMethod(list::domain(2,Language._While,Kernel._class),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(c_code_While_Optimize,"c_code_While_Optimize"));
  
  Optimize.Iterate_I->addMethod(list::domain(1,Language._Iteration),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(Iterate_I_Iteration,"Iterate!_Iteration"));
  
  Language.iterate->addMethod(list::domain(3,Core._Interval,nth_class2(Language._Variable,list::alloc(Kernel._any,1,_oid_(Kernel.range)),list::alloc(Kernel.emptySet,1,GC_OID(_oid_(nth_class1(Kernel._type,Kernel._integer))))),Kernel._any),Kernel._any,
  	NEW_ALLOC,_function_(iterate_Interval,"iterate_Interval"))->inlineDef("lambda[(x:Interval,v:Variable[range:(subtype[integer])],e:any),let v := eval(x.arg1, Interval),%max:integer := eval(x.arg2, Interval) in while (v <= %max) (e, v := v + 1)]");
  
  Language.iterate->addMethod(list::domain(3,Kernel._array,Language._Variable,Kernel._any),Kernel._any,
  	0,_function_(iterate_array,"iterate_array"))->inlineDef("lambda[(x:array,v:Variable,e:any),let %i := 1,%a := x,%max := length(%a) in while (%i <= %max) let v := %a[%i] in (e, %i := %i + 1)]");
  
  Language.Iterate->addMethod(list::domain(3,Kernel._class,Language._Variable,Kernel._any),Kernel._any,
  	0,_function_(Iterate_class,"Iterate_class"))->inlineDef("lambda[(x:class,v:Variable,e:any),for %v_1 in x.descendents let %v_2 := (for v in %v_1.instances e) in (if %v_2 break(%v_2) else false)]");
  
  Language.Iterate->addMethod(list::domain(3,nth_property(Kernel._dot_dot,tuple::alloc(2,_oid_(Kernel._integer),_oid_(Kernel._integer))),Language._Variable,Kernel._any),Kernel._any,
  	NEW_ALLOC,_function_(Iterate_any1,"Iterate_any1"))->inlineDef("lambda[(x:..[tuple(integer,integer)],v:Variable,e:any),let v := eval(x.args[1]),%max := eval(x.args[2]) in while (v <= %max) (e, v := v + 1)]");
  
  Language.Iterate->addMethod(list::domain(3,Language._Lselect,Language._Variable,Kernel._any),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(Iterate_Lselect,"Iterate_Lselect"))->inlineDef("lambda[(x:Lselect,v:Variable,e:any),for v in eval(x.iClaire/set_arg) (if eval(substitution(x.arg, x.var, v)) e else false)]");
  
  Language.Iterate->addMethod(list::domain(3,Language._Select,Language._Variable,Kernel._any),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(Iterate_Select,"Iterate_Select"))->inlineDef("lambda[(x:Select,v:Variable,e:any),for v in eval(x.iClaire/set_arg) (if eval(substitution(x.arg, x.var, v)) e else false)]");
  
  Language.Iterate->addMethod(list::domain(3,Language._Collect,Language._Variable,Kernel._any),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(Iterate_Collect,"Iterate_Collect"))->inlineDef("lambda[(x:Collect,v:Variable,e:any),for C%v in eval(x.iClaire/set_arg) let v := eval(substitution(x.arg, x.var, C%v)) in e]");
  
  Language.Iterate->addMethod(list::domain(3,nth_property(Core.but,tuple::alloc(2,_oid_(Kernel._any),_oid_(Kernel._any))),Language._Variable,Kernel._any),Kernel._any,
  	NEW_ALLOC+RETURN_ARG,_function_(Iterate_any2,"Iterate_any2"))->inlineDef("lambda[(x:but[tuple(any,any)],v:Variable,e:any),for v in eval(x.args[1]) (if (v != eval(x.args[2])) e else false)]");
  
  Language.Iterate->addMethod(list::domain(3,nth_property(Kernel._7_plus,tuple::alloc(2,_oid_(Kernel._any),_oid_(Kernel._any))),Language._Variable,Kernel._any),Kernel._any,
  	NEW_ALLOC+RETURN_ARG,_function_(Iterate_any3,"Iterate_any3"))->inlineDef("lambda[(x:/+[tuple(any,any)],v:Variable,e:any),(for v in eval(x.args[1]) e, for v in eval(x.args[2]) e)]");
  
  Optimize.c_type->addMethod(list::domain(1,Language._List),Kernel._type,
  	NEW_ALLOC+RETURN_ARG,_function_(c_type_List_Optimize,"c_type_List_Optimize"));
  
  Optimize.c_code->addMethod(list::domain(1,Language._List),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_code_List_Optimize,"c_code_List_Optimize"));
  
  Optimize.c_type->addMethod(list::domain(1,Language._Set),Kernel._type,
  	NEW_ALLOC+RETURN_ARG,_function_(c_type_Set_Optimize,"c_type_Set_Optimize"));
  
  Optimize.c_code->addMethod(list::domain(1,Language._Set),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_code_Set_Optimize,"c_code_Set_Optimize"));
  
  Optimize.c_type->addMethod(list::domain(1,Language._Tuple),Kernel._type,
  	NEW_ALLOC,_function_(c_type_Tuple_Optimize,"c_type_Tuple_Optimize"));
  
  Optimize.c_code->addMethod(list::domain(1,Language._Tuple),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(c_code_Tuple_Optimize,"c_code_Tuple_Optimize"));
  
  Optimize.c_type->addMethod(list::domain(1,Language._Definition),Kernel._type,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_type_Definition_Optimize,"c_type_Definition_Optimize"));
  
  { global_variable * v2072 = (Optimize._starname_star = (global_variable *) Core._global_variable->instantiate("*name*",Compile.it));
    (v2072->range = Kernel._symbol);
    (v2072->value = _oid_(symbol_I_string2("_CL_obj")));
    close_global_variable(v2072);
    } 
  
  Optimize.c_code->addMethod(list::domain(2,Language._Definition,Kernel._class),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_code_Definition_Optimize,"c_code_Definition_Optimize"));
  
  Optimize.total_ask->addMethod(list::domain(2,Kernel._class,Kernel._list),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(total_ask_class,"total?_class"));
  
  Optimize.analyze_I->addMethod(list::domain(4,Kernel._class,
    Kernel._any,
    Kernel._list,
    Kernel._list),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(analyze_I_class,"analyze!_class"));
  
  Optimize.c_code->addMethod(list::domain(2,Language._Defobj,Kernel._class),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(c_code_Defobj_Optimize,"c_code_Defobj_Optimize"));
  
  Optimize.c_code->addMethod(list::domain(2,Language._Defclass,Kernel._class),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_code_Defclass_Optimize,"c_code_Defclass_Optimize"));
  
  { global_variable * v2072 = (Optimize.SHIT = (global_variable *) Core._global_variable->instantiate("SHIT",claire.it));
    (v2072->range = Kernel._any);
    (v2072->value = 1);
    close_global_variable(v2072);
    } 
  
  Optimize.c_type->addMethod(list::domain(1,Language._Defmethod),Kernel._type,
  	0,_function_(c_type_Defmethod_Optimize,"c_type_Defmethod_Optimize"));
  
  Optimize.c_code->addMethod(list::domain(1,Language._Defmethod),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_code_Defmethod_Optimize,"c_code_Defmethod_Optimize"));
  
  Optimize.sort_pattern_ask->addMethod(list::domain(2,Kernel._list,Kernel._any),Kernel._boolean,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(sort_pattern_ask_list,"sort_pattern?_list"));
  
  Optimize.sort_code->addMethod(list::domain(2,Language._Defmethod,Kernel._list),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(sort_code_Defmethod,"sort_code_Defmethod"));
  
  Kernel.add_method->addMethod(list::domain(6,Kernel._property,
    Kernel._list,
    Kernel._type,
    Kernel._integer,
    Kernel._function,
    Kernel._function),Kernel._method,
  	0,_function_(add_method_property2,"add_method_property2"));
  
  Optimize.add_method_I->addMethod(list::domain(5,Kernel._method,
    Kernel._list,
    Kernel._any,
    Kernel._any,
    Kernel._function),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(add_method_I_method,"add_method!_method"));
  
  Optimize.extract_status_new->addMethod(list::domain(1,Kernel._any),Kernel._list,
  	NEW_ALLOC+SLOT_UPDATE,_function_(extract_status_new_any,"extract_status_new_any"));
  
  Optimize.extract_signature_I->addMethod(list::domain(1,Kernel._list),Kernel._list,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(extract_signature_I_list,"extract_signature!_list"));
  
  { (Optimize._equalsig_ask = (operation *) Kernel._operation->instantiate("=sig?",Optimize.it));
    ;} 
  
  Optimize._equalsig_ask->addMethod(list::domain(2,Kernel._list,Kernel._list),Kernel._boolean,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(_equalsig_ask_list,"=sig?_list"));
  
  { (Optimize.function_name = property::make("function_name",3,Optimize.it,Kernel._any,0));
    (Optimize.function_name->open = 3);
    ;} 
  
  Optimize.function_name->addMethod(list::domain(3,Kernel._property,Kernel._list,Kernel._any),Kernel._string,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(function_name_property_Optimize,"function_name_property_Optimize"));
  
  Optimize.compile_lambda->addMethod(list::domain(3,Kernel._string,Core._lambda,Kernel._any),Kernel._any,
  	NEW_ALLOC+SLOT_UPDATE,_function_(compile_lambda_string,"compile_lambda_string"));
  
  Optimize.c_code->addMethod(list::domain(1,Language._Defarray),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_code_Defarray_Optimize,"c_code_Defarray_Optimize"));
  
  Optimize.compute_if_write_inverse->addMethod(list::domain(1,Kernel._relation),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(compute_if_write_inverse_relation2,"compute_if_write_inverse_relation2"));
  
  Optimize.compute_set_write->addMethod(list::domain(1,Kernel._relation),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(compute_set_write_relation2,"compute_set_write_relation2"));
  
  Optimize.Produce_put->addMethod(list::domain(3,Kernel._property,Language._Variable,Kernel._any),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(Produce_put_property2,"Produce_put_property2"));
  
  Optimize.Produce_erase->addMethod(list::domain(2,Kernel._property,Language._Variable),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(Produce_erase_property2,"Produce_erase_property2"));
  
  Optimize.Produce_put->addMethod(list::domain(3,Kernel._table,Language._Variable,Kernel._any),Kernel._any,
  	NEW_ALLOC,_function_(Produce_put_table2,"Produce_put_table2"));
  
  Optimize.Produce_get->addMethod(list::domain(2,Kernel._relation,Language._Variable),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(Produce_get_relation2,"Produce_get_relation2"));
  
  Optimize.Produce_remove->addMethod(list::domain(3,Kernel._property,Language._Variable,Kernel._any),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(Produce_remove_property2,"Produce_remove_property2"));
  
  Optimize.Produce_remove->addMethod(list::domain(3,Kernel._table,Language._Variable,Kernel._any),Kernel._any,
  	NEW_ALLOC,_function_(Produce_remove_table2,"Produce_remove_table2"));
  
  Optimize.Tighten->addMethod(list::domain(1,Kernel._relation),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(Tighten_relation2,"Tighten_relation2"));
  
  Optimize.lexical_num->addMethod(list::domain(2,Kernel._any,Kernel._integer),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(lexical_num_any2,"lexical_num_any2"));
  
  Optimize.c_type->addMethod(list::domain(1,Language._Defrule),Kernel._type,
  	0,_function_(c_type_Defrule2_Optimize,"c_type_Defrule2_Optimize"));
  
  Optimize.c_code->addMethod(list::domain(2,Language._Defrule,Kernel._class),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(c_code_Defrule_Optimize,"c_code_Defrule_Optimize"));
  
  Optimize.compile_if_write->addMethod(list::domain(1,Kernel._relation),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(compile_if_write_relation,"compile_if_write_relation"));
  
  Optimize.compileEventMethod->addMethod(list::domain(1,Kernel._property),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(compileEventMethod_property,"compileEventMethod_property"));
  
  GC_UNBIND;} 

