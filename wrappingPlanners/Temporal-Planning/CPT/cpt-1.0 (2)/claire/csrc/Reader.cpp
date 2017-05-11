/***** CLAIRE Compilation of file Reader.cl 
         [version 3.3.4 / safety 5] Sat Oct 16 06:53:33 2004 *****/

#include <claire.h>
#include <Kernel.h>
#include <Core.h>
#include <Language.h>
#include <Reader.h>


ReaderClass Reader;

// definition of the meta-model for Reader 

void ReaderClass::metaLoad() { 
  GC_BIND;
  ClEnv->module_I = it;
// definition of the properties 
  Reader.s_index = property::make("s_index",Reader.it);
  Reader.fromp = property::make("fromp",Reader.it);
  Reader.nb_line = property::make("nb_line",Reader.it);
  Reader.firstc = property::make("firstc",Reader.it);
  Reader.last_form = property::make("last_form",Reader.it);
  Reader.maxstack = property::make("maxstack",Reader.it);
  Reader.toplevel = property::make("toplevel",Reader.it);
  Reader.eof = property::make("eof",Reader.it);
  Reader.space = property::make("space",Reader.it);
  Reader.tab = property::make("tab",Reader.it);
  Reader.bracket = property::make("bracket",Reader.it);
  Reader.paren = property::make("paren",Reader.it);
  Reader.comma = property::make("comma",Reader.it);
  Reader.curly = property::make("curly",Reader.it);
  Reader.next = property::make("next",Reader.it);
  Reader.keyword_ask = property::make("keyword?",Reader.it);
  Reader.stop_ask = property::make("stop?",Reader.it);
  Reader.nextunit = property::make("nextunit",Reader.it);
  Reader.nexts = property::make("nexts",Reader.it);
  Reader.loopexp = property::make("loopexp",Reader.it);
  Reader.nexte = property::make("nexte",Reader.it);
  Reader.nextexp = property::make("nextexp",Reader.it);
  Reader.nexti = property::make("nexti",Reader.it);
  Reader.read_escape = property::make("read_escape",Reader.it);
  Reader.nextvariable = property::make("nextvariable",Reader.it);
  Reader.nexts_I = property::make("nexts!",Reader.it);
  Reader.nexte_I = property::make("nexte!",Reader.it);
  Reader.extended_comment_ask = property::make("extended_comment?",Reader.it);
  Reader.extended_comment_I = property::make("extended_comment!",Reader.it);
  Reader.DBregister = property::make("DBregister",Reader.it);
  Reader.Call_I = property::make("Call!",Reader.it);
  Reader.operation_ask = property::make("operation?",Reader.it);
  Reader.combine = property::make("combine",Reader.it);
  Reader.combine_I = property::make("combine!",Reader.it);
  Reader.operation_I = property::make("operation!",Reader.it);
  Reader.operand_I = property::make("operand!",Reader.it);
  Reader.precedence_I = property::make("precedence!",Reader.it);
  Reader.nextstruct = property::make("nextstruct",Reader.it);
  Reader.readlet = property::make("readlet",Reader.it);
  Reader.readlet_star = property::make("readlet*",Reader.it);
  Reader.readwhen = property::make("readwhen",Reader.it);
  Reader.show = property::make("show",claire.it);
  Reader.readif = property::make("readif",Reader.it);
  Reader.readcase = property::make("readcase",Reader.it);
  Reader.readset = property::make("readset",Reader.it);
  Reader.dereference = property::make("dereference",Reader.it);
  Reader.nextseq = property::make("nextseq",Reader.it);
  Reader.readblock = property::make("readblock",claire.it);
  Reader.Do_I = property::make("Do!",Reader.it);
  Reader.readcall = property::make("readcall",Reader.it);
  Reader.nextdefinition = property::make("nextdefinition",Reader.it);
  Reader.nextmethod = property::make("nextmethod",Reader.it);
  Reader.nextinst = property::make("nextinst",Reader.it);
  Reader.nextDefclass = property::make("nextDefclass",Reader.it);
  Reader.useless_c = property::make("useless_c",Reader.it);
  Reader.skipc = property::make("skipc",Reader.it);
  Reader.skipc_I = property::make("skipc!",Reader.it);
  Reader.cnext = property::make("cnext",Reader.it);
  Reader.findeol = property::make("findeol",Reader.it);
  Reader.checkno = property::make("checkno",Reader.it);
  Reader.verify = property::make("verify",Reader.it);
  Reader.Serror = property::make("Serror",Reader.it);
  Reader.extract_variable = property::make("extract_variable",Reader.it);
  Reader.bind_I = property::make("bind!",Reader.it);
  Reader.unbind_I = property::make("unbind!",Reader.it);
  Reader.load_file = property::make("load_file",Reader.it);
  Reader.load = property::make("load",claire.it);
  Reader.sload = property::make("sload",claire.it);
  Reader.add_modules = property::make("add_modules",Reader.it);
  Reader.eload = property::make("eload",claire.it);
  Reader.debug_if_possible = property::make("debug_if_possible",Reader.it);
  Reader.print_exception = property::make("print_exception",Reader.it);
  Reader.kill = property::make("kill",claire.it);
  Reader.hashgrow = property::make("hashgrow",mClaire.it);
  Reader.inspect_system = property::make("inspect_system",Reader.it);
  Reader.debug_system = property::make("debug_system",Reader.it);
  Reader.step_system = property::make("step_system",Reader.it);
  Reader.CommandLoop = property::make("CommandLoop",Reader.it);
  Reader.trace_on = property::make("trace_on",Reader.it);
  Reader.untrace = property::make("untrace",Reader.it);
  Reader.self_trace = property::make("self_trace",Reader.it);
  Reader.trace_rule = property::make("trace_rule",Reader.it);
  Reader.stop = property::make("stop",Reader.it);
  Reader.breakpoint = property::make("breakpoint",Reader.it);
  Reader.print_debug_info = property::make("print_debug_info",Reader.it);
  Reader.Show = property::make("Show",Reader.it);
  Reader.block = property::make("block",claire.it);
  Reader.closure_build = property::make("closure_build",Reader.it);
  Reader.step = property::make("step",claire.it);
  Reader.rtime = property::make("rtime",Reader.it);
  Reader.rdepth = property::make("rdepth",2,Reader.it);
  Reader.rnum = property::make("rnum",Reader.it);
  Reader.rloop = property::make("rloop",2,Reader.it);
  Reader.rstart = property::make("rstart",2,Reader.it);
  Reader.PRget = property::make("PRget",claire.it);
  Reader.PRlook = property::make("PRlook",claire.it);
  Reader.dependents = property::make("dependents",Reader.it);
  Reader.extract_of_type = property::make("extract_of_type",Reader.it);
  Reader.PRdepends = property::make("PRdepends",claire.it);
  Reader.call_count = property::make("call_count",2,Kernel.it);
  Reader.PRshow = property::make("PRshow",claire.it);
  Reader.PRtime = property::make("PRtime",claire.it);
  Reader.PRcounter = property::make("PRcounter",claire.it);
  Reader.last_arrow = property::make("last_arrow",2,Reader.it);
  Reader.s_properties = property::make("s_properties",Reader.it);
  Reader.extended_operator = property::make("extended_operator",Reader.it);
  
  // instructions from module sources
  (Reader._delimiter = ClaireClass::make("delimiter",Core._global_variable,claire.it));
  
  { global_variable * _CL_obj = (Reader.arrow = (global_variable *) Core._global_variable->instantiate("arrow",claire.it));
    (_CL_obj->range = Kernel._any);
    { global_variable * g0107 = _CL_obj; 
      OID  g0108;
      { keyword * _CL_obj = ((keyword *) new_object_class(Kernel._keyword));
        (_CL_obj->name = symbol_I_string2("->"));
        add_I_property(Kernel.instances,Kernel._keyword,11,_oid_(_CL_obj));
        g0108 = _oid_(_CL_obj);
        } 
      (g0107->value = g0108);} 
    close_global_variable(_CL_obj);
    } 
  
  put_symbol(OBJECT(symbol,(*Kernel.name)(Reader.arrow->value)),Reader.arrow->value);
  
  { global_variable * _CL_obj = (Reader.triangle = (global_variable *) Core._global_variable->instantiate("triangle",claire.it));
    (_CL_obj->range = Kernel._any);
    { global_variable * g0109 = _CL_obj; 
      OID  g0110;
      { keyword * _CL_obj = ((keyword *) new_object_class(Kernel._keyword));
        (_CL_obj->name = symbol_I_string2("<:"));
        add_I_property(Kernel.instances,Kernel._keyword,11,_oid_(_CL_obj));
        g0110 = _oid_(_CL_obj);
        } 
      (g0109->value = g0110);} 
    close_global_variable(_CL_obj);
    } 
  
  (Reader._reserved_keyword = ClaireClass::make("reserved_keyword",Kernel._keyword,claire.it));
  
  { (Reader._cl_else = (reserved_keyword *) Reader._reserved_keyword->instantiate("else",claire.it));
    ;} 
  
  { (Reader._cl_for = (reserved_keyword *) Reader._reserved_keyword->instantiate("for",claire.it));
    ;} 
  
  { (Reader._cl_case = (reserved_keyword *) Reader._reserved_keyword->instantiate("case",claire.it));
    ;} 
  
  { (Reader._cl_while = (reserved_keyword *) Reader._reserved_keyword->instantiate("while",claire.it));
    ;} 
  
  { (Reader._cl_until = (reserved_keyword *) Reader._reserved_keyword->instantiate("until",claire.it));
    ;} 
  
  { (Reader._cl_let = (reserved_keyword *) Reader._reserved_keyword->instantiate("let",claire.it));
    ;} 
  
  { (Reader._cl_when = (reserved_keyword *) Reader._reserved_keyword->instantiate("when",claire.it));
    ;} 
  
  { (Reader._cl_try = (reserved_keyword *) Reader._reserved_keyword->instantiate("try",claire.it));
    ;} 
  
  { (Reader._cl_if = (reserved_keyword *) Reader._reserved_keyword->instantiate("if",claire.it));
    ;} 
  
  { (Reader._cl_Zif = (reserved_keyword *) Reader._reserved_keyword->instantiate("Zif",claire.it));
    ;} 
  
  { (Reader._cl_branch = (reserved_keyword *) Reader._reserved_keyword->instantiate("branch",claire.it));
    ;} 
  
  Reader.keyword_ask->addMethod(list::domain(1,Kernel._any),Kernel._boolean,
  	0,_function_(keyword_ask_any,"keyword?_any"));
  
  { (Reader.forall = (keyword *) Kernel._keyword->instantiate("forall",claire.it));
    ;} 
  
  { (Reader.none = (keyword *) Kernel._keyword->instantiate("none",claire.it));
    ;} 
  
  { (Reader.None = (keyword *) Kernel._keyword->instantiate("None",claire.it));
    ;} 
  
  { (Reader.L__equal = (keyword *) Kernel._keyword->instantiate(":=",claire.it));
    ;} 
  
  { (Reader.L_ = (keyword *) Kernel._keyword->instantiate(":",claire.it));
    ;} 
  
  { (Reader.CATCH = (keyword *) Kernel._keyword->instantiate("catch",claire.it));
    ;} 
  
  { (Reader.in = (keyword *) Kernel._keyword->instantiate("in",claire.it));
    ;} 
  
  { (Reader.as = (keyword *) Kernel._keyword->instantiate("as",claire.it));
    ;} 
  
  { (Reader.L_L_ = (keyword *) Kernel._keyword->instantiate("::",claire.it));
    ;} 
  
  { (Reader.PRINTF = (keyword *) Kernel._keyword->instantiate("printf",claire.it));
    ;} 
  
  { (Reader.assert = (keyword *) Kernel._keyword->instantiate("assert",claire.it));
    ;} 
  
  { (Reader.RETURN = (keyword *) Kernel._keyword->instantiate("return",claire.it));
    ;} 
  
  { (Reader.BREAK = (keyword *) Kernel._keyword->instantiate("break",claire.it));
    ;} 
  
  { (Reader.trace = (keyword *) Kernel._keyword->instantiate("trace",claire.it));
    ;} 
  
  { (Reader.exists = (keyword *) Kernel._keyword->instantiate("exists",claire.it));
    ;} 
  
  { (Reader.some = (keyword *) Kernel._keyword->instantiate("some",claire.it));
    ;} 
  
  { (Reader._equal_sup = (keyword *) Kernel._keyword->instantiate("=>",claire.it));
    ;} 
  
  { (Reader._ask = (keyword *) Kernel._keyword->instantiate("?",claire.it));
    ;} 
  
  { (Reader.rule = (keyword *) Kernel._keyword->instantiate("rule",claire.it));
    ;} 
  
  { (Reader.quote = (keyword *) Kernel._keyword->instantiate("quote",claire.it));
    ;} 
  
  { (Reader.inspect = property::make("inspect",1,claire.it,Kernel._any,0));
    ;} 
  
  { (Reader.known_I = property::make("known!",2,claire.it,Kernel._any,0));
    ;} 
  
  { (Reader._meta_reader = ClaireClass::make("meta_reader",Kernel._thing,claire.it));
    CL_ADD_SLOT(Reader._meta_reader,meta_reader,Kernel.source,source,Kernel._string,CNULL);
    CL_ADD_SLOT(Reader._meta_reader,meta_reader,Reader.s_index,s_index,Kernel._integer,0);
    CL_ADD_SLOT(Reader._meta_reader,meta_reader,Reader.fromp,fromp,Kernel._port,CNULL);
    CL_ADD_SLOT(Reader._meta_reader,meta_reader,Reader.nb_line,nb_line,Kernel._integer,CNULL);
    CL_ADD_SLOT(Reader._meta_reader,meta_reader,Kernel.external,external,Kernel._string,_string_("toplevel"));
    CL_ADD_SLOT(Reader._meta_reader,meta_reader,Kernel.index,index,Kernel._integer,CNULL);
    CL_ADD_SLOT(Reader._meta_reader,meta_reader,Reader.last_form,last_form,Kernel._any,CNULL);
    CL_ADD_SLOT(Reader._meta_reader,meta_reader,Reader.maxstack,maxstack,Kernel._integer,CNULL);
    CL_ADD_SLOT(Reader._meta_reader,meta_reader,Reader.toplevel,toplevel,Kernel._boolean,CNULL);
    CL_ADD_SLOT(Reader._meta_reader,meta_reader,Reader.eof,eof,Kernel._integer,CNULL);
    CL_ADD_SLOT(Reader._meta_reader,meta_reader,Reader.space,space,Kernel._integer,CNULL);
    CL_ADD_SLOT(Reader._meta_reader,meta_reader,Reader.tab,tab,Kernel._integer,CNULL);
    CL_ADD_SLOT(Reader._meta_reader,meta_reader,Reader.bracket,bracket,Kernel._any,CNULL);
    CL_ADD_SLOT(Reader._meta_reader,meta_reader,Reader.paren,paren,Kernel._any,CNULL);
    CL_ADD_SLOT(Reader._meta_reader,meta_reader,Reader.comma,comma,Kernel._any,CNULL);
    CL_ADD_SLOT(Reader._meta_reader,meta_reader,Reader.curly,curly,Kernel._any,CNULL);
    CL_ADD_SLOT(Reader._meta_reader,meta_reader,Reader.last_arrow,last_arrow,Kernel._boolean,Kernel.cfalse);
    CL_ADD_SLOT(Reader._meta_reader,meta_reader,Reader.s_properties,s_properties,nth_class1(Kernel._set,Kernel._property),_oid_(set::alloc(Kernel._property,9,_oid_(Kernel.ABSTRACT),
      _oid_(Kernel.FINAL),
      _oid_(Kernel.ephemeral),
      _oid_(Kernel.begin),
      _oid_(Kernel.end),
      _oid_(Kernel.store),
      _oid_(Core.reify),
      _oid_(Language.ClaireInterface),
      _oid_(Reader.known_I))));
    } 
  
  Reader.next->addMethod(list::domain(1,Reader._meta_reader),Kernel._integer,
  	0,_function_(next_meta_reader,"next_meta_reader"));
  
  Reader.firstc->addMethod(list::domain(1,Reader._meta_reader),Kernel._integer,
  	0,_function_(firstc_meta_reader,"firstc_meta_reader"));
  
  Reader.stop_ask->addMethod(list::domain(1,Kernel._integer),Kernel._any,
  	0,_function_(stop_ask_integer,"stop?_integer"));
  
  { global_variable * _CL_obj = (Reader.AND = (global_variable *) Core._global_variable->instantiate("AND",claire.it));
    (_CL_obj->range = Kernel._any);
    (_CL_obj->value = _oid_(Core._and));
    close_global_variable(_CL_obj);
    } 
  
  { global_variable * _CL_obj = (Reader.OR = (global_variable *) Core._global_variable->instantiate("OR",claire.it));
    (_CL_obj->range = Kernel._any);
    (_CL_obj->value = _oid_(new_thing_class(Reader._delimiter,symbol_I_string("|",claire.it))));
    close_global_variable(_CL_obj);
    } 
  
  Reader.nextunit->addMethod(list::domain(1,Reader._meta_reader),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(nextunit_meta_reader,"nextunit_meta_reader"));
  
  Reader.nexts->addMethod(list::domain(2,Reader._meta_reader,Kernel._keyword),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(nexts_meta_reader,"nexts_meta_reader"));
  
  Reader.loopexp->addMethod(list::domain(4,Reader._meta_reader,
    Kernel._any,
    Kernel._keyword,
    Kernel._boolean),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(loopexp_meta_reader,"loopexp_meta_reader"));
  
  Reader.extended_operator->addMethod(list::domain(3,Kernel._property,Kernel._any,Kernel._any),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(extended_operator_property,"extended_operator_property"));
  
  Reader.nexte->addMethod(list::domain(1,Reader._meta_reader),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(nexte_meta_reader,"nexte_meta_reader"));
  
  Reader.nextexp->addMethod(list::domain(2,Reader._meta_reader,Kernel._boolean),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(nextexp_meta_reader,"nextexp_meta_reader"));
  
  Reader.nexti->addMethod(list::domain(2,Reader._meta_reader,Kernel._any),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(nexti_meta_reader,"nexti_meta_reader"));
  
  Reader.read_escape->addMethod(list::domain(1,Reader._meta_reader),Kernel._any,
  	0,_function_(read_escape_meta_reader,"read_escape_meta_reader"));
  
  Reader.nextvariable->addMethod(list::domain(2,Reader._meta_reader,Kernel._any),Kernel._any,
  	NEW_ALLOC+SLOT_UPDATE,_function_(nextvariable_meta_reader,"nextvariable_meta_reader"));
  
  Reader.nexts_I->addMethod(list::domain(2,Reader._meta_reader,Kernel._keyword),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(nexts_I_meta_reader1,"nexts!_meta_reader1"));
  
  Reader.nexte_I->addMethod(list::domain(2,Reader._meta_reader,Kernel._keyword),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(nexte_I_meta_reader,"nexte!_meta_reader"));
  
  Reader.nexts_I->addMethod(list::domain(2,Reader._meta_reader,Kernel._integer),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(nexts_I_meta_reader2,"nexts!_meta_reader2"));
  
  Reader.nexts_I->addMethod(list::domain(3,Reader._meta_reader,Kernel._keyword,Kernel._integer),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(nexts_I_meta_reader3,"nexts!_meta_reader3"));
  
  Reader.extended_comment_ask->addMethod(list::domain(2,Reader._meta_reader,Kernel._string),Kernel._boolean,
  	0,_function_(extended_comment_ask_meta_reader,"extended_comment?_meta_reader"));
  
  Reader.extended_comment_I->addMethod(list::domain(2,Reader._meta_reader,Kernel._string),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(extended_comment_I_meta_reader,"extended_comment!_meta_reader"));
  
  { (Reader.DBline = (table *) Kernel._table->instantiate("DBline",Reader.it));
    (Reader.DBline->range = Kernel._integer);
    (Reader.DBline->params = _oid_(Kernel._any));
    (Reader.DBline->domain = Language._Call);
    (Reader.DBline->graph = make_list_integer(29,CNULL));
    (Reader.DBline->DEFAULT = 0);
    } 
  
  Reader.DBregister->addMethod(list::domain(1,Language._Call),Language._Call,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(DBregister_Call,"DBregister_Call"));
  
  Reader.Call_I->addMethod(list::domain(2,Kernel._property,Kernel._list),Language._Call,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(Call_I_property,"Call!_property"));
  
  Reader.operation_ask->addMethod(list::domain(1,Kernel._any),Kernel._boolean,
  	0,_function_(operation_ask_any,"operation?_any"));
  
  Reader.combine->addMethod(list::domain(3,Kernel._any,Kernel._any,Kernel._any),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(combine_any,"combine_any"));
  
  Reader.combine_I->addMethod(list::domain(3,Kernel._any,Kernel._any,Kernel._any),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(combine_I_any,"combine!_any"));
  
  Reader.operation_I->addMethod(list::domain(1,Kernel._any),Kernel._any,
  	0,_function_(operation_I_any,"operation!_any"));
  
  Reader.operand_I->addMethod(list::domain(2,Kernel._any,Kernel._integer),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(operand_I_any,"operand!_any"));
  
  Reader.precedence_I->addMethod(list::domain(1,Kernel._any),Kernel._integer,
  	RETURN_ARG,_function_(precedence_I_any,"precedence!_any"));
  
  Reader.nextstruct->addMethod(list::domain(3,Reader._meta_reader,Kernel._keyword,Kernel._keyword),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(nextstruct_meta_reader,"nextstruct_meta_reader"));
  
  Reader.readlet->addMethod(list::domain(2,Reader._meta_reader,Kernel._keyword),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(readlet_meta_reader,"readlet_meta_reader"));
  
  Reader.readlet_star->addMethod(list::domain(4,Reader._meta_reader,
    Kernel._list,
    Kernel._integer,
    Kernel._keyword),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(readlet_star_meta_reader,"readlet*_meta_reader"));
  
  Reader.readwhen->addMethod(list::domain(2,Reader._meta_reader,Kernel._keyword),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(readwhen_meta_reader,"readwhen_meta_reader"));
  
  Reader.readif->addMethod(list::domain(2,Reader._meta_reader,Kernel._integer),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(readif_meta_reader,"readif_meta_reader"));
  
  Reader.readcase->addMethod(list::domain(2,Reader._meta_reader,Kernel._keyword),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(readcase_meta_reader,"readcase_meta_reader"));
  
  Reader.readset->addMethod(list::domain(2,Reader._meta_reader,Kernel._any),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(readset_meta_reader,"readset_meta_reader"));
  
  Reader.dereference->addMethod(list::domain(1,Kernel._any),Kernel._any,
  	NEW_ALLOC,_function_(dereference_any,"dereference_any"));
  
  Reader.nextseq->addMethod(list::domain(2,Reader._meta_reader,Kernel._integer),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(nextseq_meta_reader,"nextseq_meta_reader"));
  
  Reader.readblock->addMethod(list::domain(3,Reader._meta_reader,Kernel._any,Kernel._integer),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(readblock_meta_reader,"readblock_meta_reader"));
  
  Reader.Do_I->addMethod(list::domain(2,Kernel._any,Kernel._any),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(Do_I_any,"Do!_any"));
  
  Reader.extract_of_type->addMethod(list::domain(1,Language._Call),Kernel._type,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(extract_of_type_Call,"extract_of_type_Call"));
  
  Reader.readcall->addMethod(list::domain(3,Reader._meta_reader,Kernel._any,Kernel._any),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(readcall_meta_reader,"readcall_meta_reader"));
  
  Reader.nextdefinition->addMethod(list::domain(4,Reader._meta_reader,
    Kernel._any,
    Kernel._any,
    Kernel._boolean),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(nextdefinition_meta_reader,"nextdefinition_meta_reader"));
  
  Reader.nextmethod->addMethod(list::domain(6,Reader._meta_reader,
    Kernel._any,
    Kernel._any,
    Kernel._boolean,
    Kernel._boolean,
    Kernel._boolean),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(nextmethod_meta_reader,"nextmethod_meta_reader"));
  
  Reader.nextinst->addMethod(list::domain(2,Reader._meta_reader,Kernel._any),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(nextinst_meta_reader,"nextinst_meta_reader"));
  
  Reader.nextDefclass->addMethod(list::domain(3,Reader._meta_reader,Kernel._any,Kernel._boolean),Language._Defclass,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(nextDefclass_meta_reader,"nextDefclass_meta_reader"));
  
  Core.self_eval->addMethod(list::domain(1,Reader._delimiter),Kernel._any,
  	SAFE_RESULT,_function_(self_eval_delimiter,"self_eval_delimiter"));
  
  (Reader._delimiter->evaluate = CLREAD(method,_at_property1(Core.self_eval,Reader._delimiter),functional));
  
  Reader.useless_c->addMethod(list::domain(1,Kernel._integer),Kernel._boolean,
  	SLOT_UPDATE,_function_(useless_c_integer,"useless_c_integer"));
  
  Reader.skipc->addMethod(list::domain(1,Reader._meta_reader),Kernel._any,
  	SLOT_UPDATE,_function_(skipc_meta_reader,"skipc_meta_reader"));
  
  Reader.skipc_I->addMethod(list::domain(1,Reader._meta_reader),Kernel._any,
  	NEW_ALLOC+SLOT_UPDATE,_function_(skipc_I_meta_reader,"skipc!_meta_reader"));
  
  Reader.cnext->addMethod(list::domain(1,Reader._meta_reader),Reader._meta_reader,
  	SAFE_RESULT,_function_(cnext_meta_reader,"cnext_meta_reader"));
  
  Reader.findeol->addMethod(list::domain(1,Reader._meta_reader),Kernel._boolean,
  	SLOT_UPDATE,_function_(findeol_meta_reader,"findeol_meta_reader"));
  
  Reader.checkno->addMethod(list::domain(3,Reader._meta_reader,Kernel._integer,Kernel._any),Kernel._any,
  	NEW_ALLOC,_function_(checkno_meta_reader,"checkno_meta_reader"));
  
  Reader.verify->addMethod(list::domain(3,Kernel._any,Kernel._any,Kernel._any),Kernel._any,
  	NEW_ALLOC+RETURN_ARG,_function_(verify_any,"verify_any"));
  
  Reader.Serror->addMethod(list::domain(2,Kernel._string,Kernel._list),Kernel.emptySet,
  	0,_function_(Serror_string,"Serror_string"));
  
  { (Reader.reader = (meta_reader *) Reader._meta_reader->instantiate("reader",claire.it));
    (Reader.reader->space = 202);
    (Reader.reader->eof = ((int) EOF));
    (Reader.reader->tab = 9);
    (Reader.reader->index = 1);
    (Reader.reader->external = "toplevel");
    (Reader.reader->bracket = _oid_(new_thing_class(Reader._delimiter,symbol_I_string2("]"))));
    (Reader.reader->paren = _oid_(new_thing_class(Reader._delimiter,symbol_I_string2(")"))));
    (Reader.reader->comma = _oid_(new_thing_class(Reader._delimiter,symbol_I_string2(","))));
    (Reader.reader->curly = _oid_(new_thing_class(Reader._delimiter,symbol_I_string2("}"))));
    ;} 
  
  Reader.extract_variable->addMethod(list::domain(1,Kernel._any),Language._Variable,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(extract_variable_any,"extract_variable_any"));
  
  Reader.bind_I->addMethod(list::domain(2,Reader._meta_reader,Language._Variable),Kernel._list,
  	NEW_ALLOC+SLOT_UPDATE,_function_(bind_I_meta_reader,"bind!_meta_reader"));
  
  Reader.unbind_I->addMethod(list::domain(2,Reader._meta_reader,Kernel._list),Kernel._any,
  	SLOT_UPDATE+RETURN_ARG,_function_(unbind_I_meta_reader,"unbind!_meta_reader"));
  
  { global_variable * _CL_obj = (Reader.STDOUT = (global_variable *) Core._global_variable->instantiate("stdout",claire.it));
    (_CL_obj->range = Kernel._port);
    (_CL_obj->value = get_symbol(symbol_I_string("STDOUT",claire.it)));
    close_global_variable(_CL_obj);
    } 
  
  (ClEnv->ctrace = EXPORT((ClairePort *),Reader.STDOUT->value));
  
  { global_variable * _CL_obj = (Reader.STDIN = (global_variable *) Core._global_variable->instantiate("stdin",claire.it));
    (_CL_obj->range = Kernel._port);
    (_CL_obj->value = get_symbol(symbol_I_string("STDIN",claire.it)));
    close_global_variable(_CL_obj);
    } 
  
  { global_variable * _CL_obj = (Reader._starfs_star = (global_variable *) Core._global_variable->instantiate("*fs*",claire.it));
    (_CL_obj->range = Kernel._string);
    (_CL_obj->value = _string_("\\"));
    close_global_variable(_CL_obj);
    } 
  
  Kernel._7->addMethod(list::domain(2,Kernel._string,Kernel._string),Kernel._string,
  	NEW_ALLOC,_function_(_7_string,"/_string"));
  
  Kernel.restore_state->addMethod(list::domain(1,Reader._meta_reader),Kernel._void,
  	SLOT_UPDATE,_function_(restore_state_meta_reader,"restore_state_meta_reader"));
  
  Reader.load_file->addMethod(list::domain(2,Kernel._string,Kernel._boolean),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(load_file_string,"load_file_string"));
  
  Reader.load->addMethod(list::domain(1,Kernel._string),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(load_string,"load_string"));
  
  Reader.sload->addMethod(list::domain(1,Kernel._string),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(sload_string,"sload_string"));
  
  Reader.load_file->addMethod(list::domain(2,Kernel._module,Kernel._boolean),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(load_file_module,"load_file_module"));
  
  Reader.load->addMethod(list::domain(1,Kernel._module),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE,_function_(load_module,"load_module"));
  
  Reader.sload->addMethod(list::domain(1,Kernel._module),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE,_function_(sload_module,"sload_module"));
  
  Reader.add_modules->addMethod(list::domain(3,Kernel._module,Kernel._set,Kernel._list),Kernel._list,
  	NEW_ALLOC+BAG_UPDATE+RETURN_ARG,_function_(add_modules_module,"add_modules_module"));
  
  Reader.add_modules->addMethod(list::domain(1,Kernel._list),Kernel._list,
  	NEW_ALLOC+BAG_UPDATE,_function_(add_modules_list,"add_modules_list"));
  
  Reader.eload->addMethod(list::domain(1,Kernel._string),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(eload_string,"eload_string"));
  
  Reader.readblock->addMethod(list::domain(1,Kernel._port),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(readblock_port,"readblock_port"));
  
  Core.read->addMethod(list::domain(1,Kernel._port),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(read_port,"read_port"));
  
  Core.read->addMethod(list::domain(1,Kernel._string),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(read_string,"read_string"));
  
  { (Reader.q = (keyword *) Kernel._keyword->instantiate("q",claire.it));
    ;} 
  
  { (Reader.call_debug = property::make("call_debug",2,claire.it,Kernel._any,0));
    ;} 
  
  { (Reader.EVAL = (table *) Kernel._table->instantiate("EVAL",claire.it));
    (Reader.EVAL->range = Kernel._any);
    (Reader.EVAL->params = -1);
    (Reader.EVAL->domain = _dot_dot_integer(0,99));
    (Reader.EVAL->graph = make_copy_list_integer(100,CNULL));
    (Reader.EVAL->DEFAULT = CNULL);
    } 
  
  Reader.debug_if_possible->addMethod(list::domain(1,Kernel._void),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(debug_if_possible_void,"debug_if_possible_void"));
  
  Reader.print_exception->addMethod(list::domain(1,Kernel._void),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(print_exception_void,"print_exception_void"));
  
  { (Reader.pretty_show = property::make("pretty_show",3,claire.it,Kernel._any,0));
    (Reader.pretty_show->open = 3);
    ;} 
  
  Reader.show->addMethod(list::domain(1,Kernel._any),Kernel._any,
  	NEW_ALLOC+SLOT_UPDATE,_function_(show_any,"show_any"));
  
  Reader.kill->addMethod(list::domain(1,Kernel._object),Kernel._any,
  	SLOT_UPDATE,_function_(kill_object,"kill_object"));
  
  Reader.kill->addMethod(list::domain(1,Kernel._class),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(kill_class,"kill_class"));
  
  Core.min->addMethod(list::domain(2,Kernel._integer,Kernel._integer),Kernel._integer,
  	SAFE_RESULT,_function_(min_integer,"min_integer"))->inlineDef("lambda[(x:integer,y:integer),(if (x <= y) x else y)]");
  
  Core.max->addMethod(list::domain(2,Kernel._integer,Kernel._integer),Kernel._integer,
  	SAFE_RESULT,_function_(max_integer,"max_integer"))->inlineDef("lambda[(x:integer,y:integer),(if (x <= y) y else x)]");
  
  Core.min->addFloatMethod(list::domain(2,Kernel._float,Kernel._float),Kernel._float,
  	RETURN_ARG,_function_(min_float,"min_float"),_function_(min_float_,"min_float_"))->inlineDef("lambda[(x:float,y:float),(if (x <= y) x else y)]");
  
  Core.max->addFloatMethod(list::domain(2,Kernel._float,Kernel._float),Kernel._float,
  	RETURN_ARG,_function_(max_float,"max_float"),_function_(max_float_,"max_float_"))->inlineDef("lambda[(x:float,y:float),(if (x <= y) y else x)]");
  
  Core.min->addMethod(list::domain(2,Kernel._any,Kernel._any),Kernel._any,
  	NEW_ALLOC+RETURN_ARG,_function_(min_any,"min_any"))->inlineDef("lambda[(x:any,y:any),(if (x <= y) x else y)]");
  
  Core.max->addMethod(list::domain(2,Kernel._any,Kernel._any),Kernel._any,
  	NEW_ALLOC+RETURN_ARG,_function_(max_any,"max_any"))->inlineDef("lambda[(x:any,y:any),(if (x <= y) y else x)]");
  
  Reader.hashgrow->addMethod(list::domain(2,Kernel._list,Kernel._property),Kernel._list,
  	NEW_ALLOC,_function_(hashgrow_list,"hashgrow_list"))->inlineDef("lambda[(l:list,hi:property),let l1 := l,l2 := make_list(nth_get(l1, 0) * 2, unknown) in (for x in l1 (if known?(x) hi(l2, x) else false), l2)]");
  
  Core.known_ask->addMethod(list::domain(2,Kernel._table,Kernel._any),Kernel._boolean,
  	0,_function_(known_ask_table,"known?_table"))->inlineDef("lambda[(a:table,x:any),get(a, x) != unknown]");
  
  Core.unknown_ask->addMethod(list::domain(2,Kernel._table,Kernel._any),Kernel._boolean,
  	0,_function_(unknown_ask_table,"unknown?_table"))->inlineDef("lambda[(a:table,x:any),get(a, x) = unknown]");
  
  Kernel.float_I->addFloatMethod(list::domain(1,Kernel._string),Kernel._float,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(float_I_string,"float!_string"),_function_(float_I_string_,"float!_string_"));
  
  Kernel._sup_equal->addMethod(list::domain(2,Kernel._any,Kernel._any),Kernel._boolean,
  	NEW_ALLOC,_function_(_sup_equal_any,">=_any"))->inlineDef("lambda[(self:any,x:any),x <= self]");
  
  { (Reader.execute_do = property::make("execute_do",2,claire.it,Kernel._any,0));
    ;} 
  
  { (Reader.execute_bk = property::make("execute_bk",2,claire.it,Kernel._any,0));
    ;} 
  
  { (Reader.inspect_loop = property::make("inspect_loop",2,claire.it,Kernel._any,0));
    ;} 
  
  { (Reader.get_from_integer = property::make("get_from_integer",1,claire.it,Kernel._any,0));
    ;} 
  
  { (Reader.top_debugger = property::make("top_debugger",2,claire.it,Kernel._any,0));
    ;} 
  
  { global_variable * _CL_obj = (Reader._starlast_star = (global_variable *) Core._global_variable->instantiate("*last*",claire.it));
    (_CL_obj->range = Kernel._any);
    (_CL_obj->value = CNULL);
    close_global_variable(_CL_obj);
    } 
  
  { global_variable * _CL_obj = (Reader._starindex_star = (global_variable *) Core._global_variable->instantiate("*index*",claire.it));
    (_CL_obj->range = Kernel._integer);
    (_CL_obj->value = CNULL);
    close_global_variable(_CL_obj);
    } 
  
  { global_variable * _CL_obj = (Reader._starmaxd_star = (global_variable *) Core._global_variable->instantiate("*maxd*",claire.it));
    (_CL_obj->range = Kernel._integer);
    (_CL_obj->value = CNULL);
    close_global_variable(_CL_obj);
    } 
  
  { global_variable * _CL_obj = (Reader._starcurd_star = (global_variable *) Core._global_variable->instantiate("*curd*",claire.it));
    (_CL_obj->range = Kernel._integer);
    (_CL_obj->value = 0);
    close_global_variable(_CL_obj);
    } 
  
  { global_variable * _CL_obj = (Reader._starshowall_star = (global_variable *) Core._global_variable->instantiate("*showall*",claire.it));
    (_CL_obj->range = Kernel._boolean);
    (_CL_obj->value = Kernel.ctrue);
    close_global_variable(_CL_obj);
    } 
  
  Reader.inspect_system->addMethod(list::domain(1,Kernel._list),Kernel._any,
  	0,_function_(InspectLoop,"InspectLoop"));
  
  Reader.debug_system->addMethod(list::domain(1,Kernel._void),Kernel._any,
  	0,_function_(DebugLoop,"DebugLoop"));
  
  Reader.step_system->addMethod(list::domain(1,Kernel._void),Kernel._integer,
  	0,_function_(StepLoop,"StepLoop"));
  
  Reader.CommandLoop->addMethod(list::domain(1,Kernel._void),Kernel._string,
  	0,_function_(CommandLoopVoid,"CommandLoopVoid"));
  
  Reader.inspect->addMethod(list::domain(1,Kernel._any),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(inspect_any,"inspect_any"));
  
  Reader.inspect_loop->addMethod(list::domain(2,Kernel._any,Kernel._list),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(inspect_loop_any,"inspect_loop_any"));
  
  Reader.get_from_integer->addMethod(list::domain(2,Kernel._any,Kernel._integer),Kernel._any,
  	NEW_ALLOC+SLOT_UPDATE+RETURN_ARG,_function_(get_from_integer_any,"get_from_integer_any"));
  
  Reader.trace_on->addMethod(list::domain(1,Kernel._any),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(trace_on_any,"trace_on_any"));
  
  Reader.untrace->addMethod(list::domain(1,Kernel._any),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(untrace_any,"untrace_any"));
  
  Core.spy->addMethod(list::domain(1,Kernel._listargs),Kernel._void,
  	NEW_ALLOC+SLOT_UPDATE+RETURN_ARG,_function_(spy_listargs2_Reader,"spy_listargs2_Reader"));
  
  Reader.self_trace->addMethod(list::domain(1,Language._Trace),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(self_trace_Trace,"self_trace_Trace"));
  
  { OID  x = get_property(Kernel.functional,_at_property1(Reader.self_trace,Language._Trace));
    if (x != CNULL)
     (Language._Trace->evaluate = OBJECT(ClaireFunction,x));
    } 
  
  Reader.trace_rule->addMethod(list::domain(6,Kernel._relation,
    Kernel._string,
    Kernel._any,
    Kernel._any,
    Kernel._any,
    Kernel._any),Kernel._void,
  	NEW_ALLOC+SLOT_UPDATE,_function_(trace_rule_relation,"trace_rule_relation"));
  
  Reader.stop->addMethod(list::domain(2,Kernel._property,Kernel._listargs),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(stop_property,"stop_property"));
  
  Core.debug->addMethod(list::domain(1,Kernel._void),Kernel._void,
  	SLOT_UPDATE,_function_(debug_void,"debug_void"));
  
  Reader.call_debug->addMethod(list::domain(1,Kernel._void),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(call_debug_void,"call_debug_void"));
  
  Reader.breakpoint->addMethod(list::domain(1,Kernel._void),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(breakpoint_void,"breakpoint_void"));
  
  { (Reader.up = property::make("up",2,claire.it,Kernel._any,0));
    ;} 
  
  { (Reader.dn = property::make("dn",2,claire.it,Kernel._any,0));
    ;} 
  
  { (Reader.where = property::make("where",2,claire.it,Kernel._any,0));
    ;} 
  
  Reader.dn->addMethod(list::domain(1,Kernel._integer),Kernel._void,
  	NEW_ALLOC,_function_(dn_integer,"dn_integer"));
  
  Reader.up->addMethod(list::domain(1,Kernel._integer),Kernel._void,
  	NEW_ALLOC,_function_(up_integer,"up_integer"));
  
  Reader.where->addMethod(list::domain(1,Kernel._integer),Kernel._void,
  	NEW_ALLOC,_function_(where_integer,"where_integer"));
  
  Reader.print_debug_info->addMethod(list::domain(3,Kernel._integer,Kernel._integer,Kernel._integer),Kernel._void,
  	NEW_ALLOC,_function_(print_debug_info_integer,"print_debug_info_integer"));
  
  Reader.Show->addMethod(list::domain(1,Kernel._integer),Kernel._any,
  	NEW_ALLOC,_function_(Show_integer,"Show_integer"));
  
  Reader.block->addMethod(list::domain(1,Kernel._integer),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(block_integer,"block_integer"));
  
  Reader.closure_build->addMethod(list::domain(1,Core._lambda),Kernel._list,
  	NEW_ALLOC+BAG_UPDATE,_function_(closure_build_lambda,"closure_build_lambda"));
  
  Reader.closure_build->addMethod(list::domain(2,Kernel._any,Kernel._list),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+RETURN_ARG,_function_(closure_build_any,"closure_build_any"));
  
  Core.call_step->addMethod(list::domain(1,Kernel._property),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(call_step_property_Reader,"call_step_property_Reader"));
  
  Reader.step->addMethod(list::domain(1,Kernel._any),Kernel._void,
  	SLOT_UPDATE+RETURN_ARG,_function_(step_any,"step_any"));
  
  Kernel.mem->addMethod(list::domain(1,Kernel._class),Kernel._integer,
  	NEW_ALLOC,_function_(mem_class,"mem_class"));
  
  { (Reader._PRcount = ClaireClass::make("PRcount",Kernel._object,claire.it));
    CL_ADD_SLOT(Reader._PRcount,PRcount,Reader.rtime,rtime,Kernel._integer,0);
    CL_ADD_SLOT(Reader._PRcount,PRcount,Reader.rdepth,rdepth,Kernel._integer,0);
    CL_ADD_SLOT(Reader._PRcount,PRcount,Reader.rnum,rnum,Kernel._integer,0);
    CL_ADD_SLOT(Reader._PRcount,PRcount,Reader.rloop,rloop,Kernel._integer,0);
    CL_ADD_SLOT(Reader._PRcount,PRcount,Reader.rstart,rstart,Kernel._integer,0);
    } 
  
  Reader.PRget->addMethod(list::domain(1,Kernel._property),Reader._PRcount,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(PRget_property,"PRget_property"));
  
  Reader.PRlook->addMethod(list::domain(1,Kernel._property),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(PRlook_property2,"PRlook_property2"));
  
  Reader.PRshow->addMethod(list::domain(1,Kernel._property),Kernel._void,
  	NEW_ALLOC,_function_(PRshow_property,"PRshow_property"));
  
  Reader.PRtime->addMethod(list::domain(1,Kernel._property),Kernel._integer,
  	RETURN_ARG,_function_(PRtime_property,"PRtime_property"));
  
  Reader.PRcounter->addMethod(list::domain(1,Kernel._property),Kernel._integer,
  	RETURN_ARG,_function_(PRcounter_property,"PRcounter_property"));
  
  Reader.PRshow->addMethod(list::domain(1,Kernel._void),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE,_function_(PRshow_void,"PRshow_void"));
  
  { (Reader.PRdependent = (table *) Kernel._table->instantiate("PRdependent",Reader.it));
    (Reader.PRdependent->multivalued_ask = CTRUE);
    (Reader.PRdependent->range = nth_class1(Kernel._set,Kernel._property));
    (Reader.PRdependent->params = _oid_(Kernel._any));
    (Reader.PRdependent->domain = Kernel._property);
    (Reader.PRdependent->graph = make_list_integer(29,CNULL));
    (Reader.PRdependent->DEFAULT = _oid_(set::empty(Kernel._property)));
    } 
  
  { (Reader.PRdependentOf = (table *) Kernel._table->instantiate("PRdependentOf",Reader.it));
    (Reader.PRdependentOf->multivalued_ask = CTRUE);
    (Reader.PRdependentOf->range = nth_class1(Kernel._set,Kernel._property));
    (Reader.PRdependentOf->params = _oid_(Kernel._any));
    (Reader.PRdependentOf->domain = Kernel._property);
    (Reader.PRdependentOf->graph = make_list_integer(29,CNULL));
    (Reader.PRdependentOf->DEFAULT = _oid_(set::empty(Kernel._property)));
    } 
  
  Reader.dependents->addMethod(list::domain(1,Kernel._method),nth_class1(Kernel._set,Kernel._property),
  	NEW_ALLOC,_function_(dependents_method,"dependents_method"));
  
  Reader.dependents->addMethod(list::domain(1,Kernel._any),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(dependents_any,"dependents_any"));
  
  update_property(Kernel.inverse,
    Reader.PRdependent,
    8,
    Kernel._object,
    _oid_(Reader.PRdependentOf));
  
  Reader.PRdepends->addMethod(list::domain(2,Kernel._property,Kernel._property),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(PRdepends_property,"PRdepends_property"));
  
  GC_UNBIND;} 

