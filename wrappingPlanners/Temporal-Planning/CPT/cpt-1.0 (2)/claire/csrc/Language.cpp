/***** CLAIRE Compilation of file Language.cl 
         [version 3.3.4 / safety 5] Sat Oct 16 06:53:30 2004 *****/

#include <claire.h>
#include <Kernel.h>
#include <Core.h>
#include <Language.h>


LanguageClass Language;

NameSpace iClaire;
// definition of the meta-model for Language 

void LanguageClass::metaLoad() { 
  GC_BIND;
  ClEnv->module_I = it;
// definition of the properties 
  Language.no_eval = property::make("no_eval",Language.it);
  Language.ppvariable = property::make("ppvariable",Language.it);
  Language.write_value = property::make("write_value",Language.it);
  Language.var = property::make("var",0,claire.it);
  Language.lambda_I = property::make("lambda!",iClaire.it);
  Language.lexical_build = property::make("lexical_build",iClaire.it);
  Language.lexical_change = property::make("lexical_change",iClaire.it);
  Language.extract_symbol = property::make("extract_symbol",iClaire.it);
  Language.make_a_property = property::make("make_a_property",iClaire.it);
  Language.lbreak = property::make("lbreak",Language.it);
  Language.put_buffer = property::make("put_buffer",Language.it);
  Language.checkfar = property::make("checkfar",Language.it);
  Language.indent = property::make("indent",Language.it);
  Language.set_level = property::make("set_level",Language.it);
  Language.printbox = property::make("printbox",Language.it);
  Language.printexp = property::make("printexp",Language.it);
  Language.pretty_print = property::make("pretty_print",claire.it);
  Language.assign = property::make("assign",Language.it);
  Language.printe = property::make("printe",Language.it);
  Language.sugar_ask = property::make("sugar?",Language.it);
  Language.cast_to = property::make("cast_to",iClaire.it);
  Language.set_arg = property::make("set_arg",0,iClaire.it);
  Language.substitution = property::make("substitution",claire.it);
  Language.occurrence = property::make("occurrence",Language.it);
  Language.instruction_copy = property::make("instruction_copy",Language.it);
  Language.other = property::make("other",iClaire.it);
  Language.test = property::make("test",iClaire.it);
  Language.printstat = property::make("printstat",Language.it);
  Language.printif = property::make("printif",Language.it);
  Language.printelse = property::make("printelse",Language.it);
  Language.printdo = property::make("printdo",Language.it);
  Language.printblock = property::make("printblock",Language.it);
  Language.printbody = property::make("printbody",Language.it);
  Language.ident = property::make("ident",iClaire.it);
  Language.attach_comment = property::make("attach_comment",Language.it);
  Language.extract_signature = property::make("extract_signature",iClaire.it);
  Language.extract_pattern = property::make("extract_pattern",iClaire.it);
  Language.extract_type = property::make("extract_type",iClaire.it);
  Language.extract_pattern_nth = property::make("extract_pattern_nth",Language.it);
  Language.extract_class_call = property::make("extract_class_call",iClaire.it);
  Language.extract_range = property::make("extract_range",iClaire.it);
  Language.extract_status = property::make("extract_status",iClaire.it);
  Language.type_I = property::make("type!",iClaire.it);
  Language.forward_ask = property::make("forward?",iClaire.it);
  Language.pname = property::make("pname",Language.it);
  Language.priority = property::make("priority",2,Language.it);
  Language.make_filter = property::make("make_filter",Language.it);
  Language.make_demon = property::make("make_demon",Language.it);
  Language.eval_if_write = property::make("eval_if_write",Language.it);
  Language.readCall = property::make("readCall",Language.it);
  Language.eventMethod = property::make("eventMethod",Language.it);
  Language.eventMethod_ask = property::make("eventMethod?",Language.it);
  Language.putCall = property::make("putCall",Language.it);
  Language.safeRange = property::make("safeRange",Language.it);
  
  // instructions from module sources
  (Language._Instruction = ClaireClass::make("Instruction",Kernel._system_object,claire.it));
  
  (Language._Basic_instruction = ClaireClass::make("Basic_instruction",Language._Instruction,claire.it));
  
  Language.no_eval->addMethod(list::domain(1,Language._Instruction),Kernel._any,
  	SAFE_RESULT,_function_(no_eval_Instruction,"no_eval_Instruction"));
  
  { global_variable * _CL_obj = (Language.typing = (global_variable *) Core._global_variable->instantiate("typing",iClaire.it));
    (_CL_obj->range = Kernel.emptySet);
    (_CL_obj->value = _oid_(Kernel.typing));
    close_global_variable(_CL_obj);
    } 
  
  { global_variable * _CL_obj = (Language.index = (global_variable *) Core._global_variable->instantiate("index",iClaire.it));
    (_CL_obj->range = Kernel.emptySet);
    (_CL_obj->value = _oid_(Kernel.index));
    close_global_variable(_CL_obj);
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Kernel._unbound_symbol),Kernel._void,
  	0,_function_(self_print_unbound_symbol_Language,"self_print_unbound_symbol_Language"));
  
  Core.self_eval->addMethod(list::domain(1,Kernel._unbound_symbol),Kernel._any,
  	NEW_ALLOC,_function_(self_eval_unbound_symbol,"self_eval_unbound_symbol"));
  
  { (Language._Variable = ClaireClass::make("Variable",Language._Basic_instruction,claire.it));
    CL_ADD_SLOT(Language._Variable,Variable,Core.pname,pname,Kernel._symbol,CNULL);
    CL_ADD_SLOT(Language._Variable,Variable,Kernel.range,range,Kernel._type,CNULL);
    CL_ADD_SLOT(Language._Variable,Variable,Kernel.index,index,Kernel._integer,CNULL);
    (Language._Variable->params = list::alloc(Kernel._any,2,_oid_(Core.pname),_oid_(Kernel.range)));
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Language._Variable),Kernel._void,
  	0,_function_(self_print_Variable_Language,"self_print_Variable_Language"));
  
  Language.ppvariable->addMethod(list::domain(1,Language._Variable),Kernel._void,
  	NEW_ALLOC+SLOT_UPDATE,_function_(ppvariable_Variable,"ppvariable_Variable"));
  
  Language.ppvariable->addMethod(list::domain(1,Kernel._list),Kernel._void,
  	NEW_ALLOC+SLOT_UPDATE,_function_(ppvariable_list,"ppvariable_list"));
  
  Core.self_eval->addMethod(list::domain(1,Language._Variable),Kernel._any,
  	0,_function_(self_eval_Variable,"self_eval_Variable"));
  
  Language.write_value->addMethod(list::domain(2,Language._Variable,Kernel._any),Kernel._any,
  	0,_function_(write_value_Variable,"write_value_Variable"));
  
  (Language._Vardef = ClaireClass::make("Vardef",Language._Variable,claire.it));
  
  Core.self_eval->addMethod(list::domain(1,Language._Vardef),Kernel._any,
  	0,_function_(self_eval_Vardef,"self_eval_Vardef"));
  
  (Language._Complex_instruction = ClaireClass::make("Complex_instruction",Language._Instruction,claire.it));
  
  { (Language._Instruction_with_var = ClaireClass::make("Instruction_with_var",Language._Complex_instruction,claire.it));
    CL_ADD_SLOT(Language._Instruction_with_var,Instruction_with_var,Language.var,var,Language._Variable,CNULL);
    } 
  
  (Language._Control_structure = ClaireClass::make("Control_structure",Language._Complex_instruction,claire.it));
  
  Core.self_eval->addMethod(list::domain(1,Core._global_variable),Kernel._any,
  	RETURN_ARG,_function_(self_eval_global_variable,"self_eval_global_variable"));
  
  Language.write_value->addMethod(list::domain(2,Core._global_variable,Kernel._any),Kernel._any,
  	NEW_ALLOC+SLOT_UPDATE+RETURN_ARG,_function_(write_value_global_variable,"write_value_global_variable"));
  
  { (Core._global_variable->evaluate = ((ClaireFunction *) _function_(self_eval_global_variable,"self_eval_global_variable")));
    (Kernel._unbound_symbol->evaluate = ((ClaireFunction *) _function_(self_eval_unbound_symbol,"self_eval_unbound_symbol")));
    } 
  
  { global_variable * _CL_obj = (Language._eof = (global_variable *) Core._global_variable->instantiate("EOF",claire.it));
    (_CL_obj->range = Kernel._char);
    (_CL_obj->value = _oid_(char_I_integer(((int) EOF))));
    close_global_variable(_CL_obj);
    } 
  
  { global_variable * _CL_obj = (Language.EOS = (global_variable *) Core._global_variable->instantiate("EOS",claire.it));
    (_CL_obj->range = Kernel._char);
    (_CL_obj->value = _oid_(char_I_integer(0)));
    close_global_variable(_CL_obj);
    } 
  
  { global_variable * _CL_obj = (Language.MAX_INTEGER = (global_variable *) Core._global_variable->instantiate("MAX_INTEGER",claire.it));
    (_CL_obj->range = Kernel.emptySet);
    (_CL_obj->value = 1073741822);
    close_global_variable(_CL_obj);
    } 
  
  Core.apply->addMethod(list::domain(2,Core._lambda,Kernel._list),Kernel._any,
  	NEW_ALLOC,_function_(apply_lambda,"apply_lambda"));
  
  Core.call->addMethod(list::domain(2,Core._lambda,Kernel._listargs),Kernel._any,
  	NEW_ALLOC,_function_(call_lambda2,"call_lambda2"));
  
  Kernel.self_print->addMethod(list::domain(1,Core._lambda),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(self_print_lambda_Language,"self_print_lambda_Language"));
  
  { global_variable * _CL_obj = (Language._starvariable_index_star = (global_variable *) Core._global_variable->instantiate("*variable_index*",claire.it));
    (_CL_obj->range = Kernel._integer);
    (_CL_obj->value = 0);
    close_global_variable(_CL_obj);
    } 
  
  Language.lambda_I->addMethod(list::domain(2,Kernel._list,Kernel._any),Core._lambda,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(lambda_I_list,"lambda!_list"));
  
  Language.lexical_build->addMethod(list::domain(3,Kernel._any,Kernel._list,Kernel._integer),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(lexical_build_any,"lexical_build_any"));
  
  Language.lexical_change->addMethod(list::domain(2,Kernel._any,Kernel._list),Kernel._any,
  	0,_function_(lexical_change_any,"lexical_change_any"));
  
  Kernel.close->addMethod(list::domain(1,Kernel._class),Kernel._class,
  	SAFE_RESULT,_function_(close_class,"close_class"));
  
  Language.extract_symbol->addMethod(list::domain(1,Kernel._any),Kernel._symbol,
  	RETURN_ARG,_function_(extract_symbol_any,"extract_symbol_any"));
  
  Language.make_a_property->addMethod(list::domain(1,Kernel._any),Kernel._property,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(make_a_property_any,"make_a_property_any"));
  
  { (Language.printl = property::make("printl",1,claire.it,Kernel._any,0));
    ;} 
  
  Language.lbreak->addMethod(list::domain(1,Kernel._void),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(lbreak_void,"lbreak_void"));
  
  Language.put_buffer->addMethod(list::domain(1,Kernel._void),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(put_buffer_void,"put_buffer_void"));
  
  Language.checkfar->addMethod(list::domain(1,Kernel._void),Kernel._any,
  	NEW_ALLOC,_function_(checkfar_void,"checkfar_void"));
  
  Language.lbreak->addMethod(list::domain(1,Kernel._integer),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(lbreak_integer,"lbreak_integer"));
  
  Language.indent->addMethod(list::domain(1,Kernel._integer),Kernel._any,
  	0,_function_(indent_integer,"indent_integer"));
  
  Language.set_level->addMethod(list::domain(1,Kernel._void),Kernel._void,
  	SLOT_UPDATE+RETURN_ARG,_function_(set_level_void,"set_level_void"));
  
  Language.set_level->addMethod(list::domain(1,Kernel._integer),Kernel._void,
  	SLOT_UPDATE+RETURN_ARG,_function_(set_level_integer,"set_level_integer"));
  
  Language.printbox->addMethod(list::domain(4,Kernel._bag,
    Kernel._integer,
    Kernel._integer,
    Kernel._string),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(printbox_bag1,"printbox_bag1"));
  
  Language.printbox->addMethod(list::domain(1,Kernel._bag),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(printbox_bag2,"printbox_bag2"));
  
  Language.printbox->addMethod(list::domain(2,Kernel._bag,Kernel._string),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(printbox_bag3,"printbox_bag3"));
  
  Language.printl->addMethod(list::domain(2,Kernel._bag,Kernel._string),Kernel._void,
  	NEW_ALLOC+SLOT_UPDATE+RETURN_ARG,_function_(printl_bag,"printl_bag"));
  
  Language.printexp->addMethod(list::domain(2,Kernel._any,Kernel._boolean),Kernel._void,
  	NEW_ALLOC+SLOT_UPDATE+RETURN_ARG,_function_(printexp_any,"printexp_any"));
  
  Language.pretty_print->addMethod(list::domain(1,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(pretty_print_any,"pretty_print_any"));
  
  Kernel.self_print->addMethod(list::domain(1,Kernel._list),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(self_print_list_Language,"self_print_list_Language"));
  
  Kernel.self_print->addMethod(list::domain(1,Kernel._set),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(self_print_set_Language,"self_print_set_Language"));
  
  Kernel.self_print->addMethod(list::domain(1,Kernel._tuple),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(self_print_tuple_Language,"self_print_tuple_Language"));
  
  { global_variable * _CL_obj = (Language.LastCall = (global_variable *) Core._global_variable->instantiate("LastCall",iClaire.it));
    (_CL_obj->range = Kernel._any);
    (_CL_obj->value = CNULL);
    close_global_variable(_CL_obj);
    } 
  
  { (Language._Call = ClaireClass::make("Call",Language._Control_structure,claire.it));
    CL_ADD_SLOT(Language._Call,Call,Kernel.selector,selector,Kernel._property,CNULL);
    CL_ADD_SLOT(Language._Call,Call,Core.args,args,Kernel._list,CNULL);
    (Language._Call->params = list::alloc(Kernel._any,2,_oid_(Kernel.selector),_oid_(Core.args)));
    } 
  
  { (Language._Call_star = ClaireClass::make("Call*",Language._Call,claire.it));
    (Language._Call_star->params = list::alloc(Kernel._any,2,_oid_(Kernel.selector),_oid_(Core.args)));
    } 
  
  { (Language._Call_plus = ClaireClass::make("Call+",Language._Call,claire.it));
    (Language._Call_plus->params = list::alloc(Kernel._any,2,_oid_(Kernel.selector),_oid_(Core.args)));
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Language._Call),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(self_print_Call_Language,"self_print_Call_Language"));
  
  Kernel.self_print->addMethod(list::domain(1,Language._Call_plus),Kernel._any,
  	NEW_ALLOC+SLOT_UPDATE,_function_(self_print_Call_plus_Language,"self_print_Call+_Language"));
  
  Core.self_eval->addMethod(list::domain(1,Language._Call),Kernel._any,
  	NEW_ALLOC+SLOT_UPDATE,_function_(self_eval_Call,"self_eval_Call"));
  
  Core.self_eval->addMethod(list::domain(1,Language._Call_plus),Kernel._any,
  	NEW_ALLOC+SLOT_UPDATE,_function_(self_eval_Call_plus,"self_eval_Call+"));
  
  Language.printe->addMethod(list::domain(2,Kernel._any,Kernel._property),Kernel._void,
  	NEW_ALLOC+SLOT_UPDATE+RETURN_ARG,_function_(printe_any,"printe_any"));
  
  Language.sugar_ask->addMethod(list::domain(4,Kernel._any,
    Kernel._any,
    Kernel._any,
    Kernel._any),Kernel._boolean,
  	0,_function_(sugar_ask_any,"sugar?_any"));
  
  { (Language._Assign = ClaireClass::make("Assign",Language._Basic_instruction,claire.it));
    CL_ADD_SLOT(Language._Assign,Assign,Language.var,var,Kernel._any,CNULL);
    CL_ADD_SLOT(Language._Assign,Assign,Kernel.arg,arg,Kernel._any,CNULL);
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Language._Assign),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(self_print_Assign_Language,"self_print_Assign_Language"));
  
  Core.self_eval->addMethod(list::domain(1,Language._Assign),Kernel._any,
  	NEW_ALLOC+RETURN_ARG,_function_(self_eval_Assign,"self_eval_Assign"));
  
  { (Language._Gassign = ClaireClass::make("Gassign",Language._Basic_instruction,claire.it));
    CL_ADD_SLOT(Language._Gassign,Gassign,Language.var,var,Core._global_variable,CNULL);
    CL_ADD_SLOT(Language._Gassign,Gassign,Kernel.arg,arg,Kernel._any,CNULL);
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Language._Gassign),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(self_print_Gassign_Language,"self_print_Gassign_Language"));
  
  Core.self_eval->addMethod(list::domain(1,Language._Gassign),Kernel._any,
  	NEW_ALLOC+SLOT_UPDATE+RETURN_ARG,_function_(self_eval_Gassign,"self_eval_Gassign"));
  
  { (Language._And = ClaireClass::make("And",Language._Control_structure,claire.it));
    CL_ADD_SLOT(Language._And,And,Core.args,args,Kernel._list,CNULL);
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Language._And),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(self_print_And_Language,"self_print_And_Language"));
  
  Core.self_eval->addMethod(list::domain(1,Language._And),Kernel._any,
  	NEW_ALLOC,_function_(self_eval_And,"self_eval_And"));
  
  { (Language._Or = ClaireClass::make("Or",Language._Control_structure,claire.it));
    CL_ADD_SLOT(Language._Or,Or,Core.args,args,Kernel._list,CNULL);
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Language._Or),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(self_print_Or_Language,"self_print_Or_Language"));
  
  Core.self_eval->addMethod(list::domain(1,Language._Or),Kernel._any,
  	NEW_ALLOC,_function_(self_eval_Or,"self_eval_Or"));
  
  { (Language._Quote = ClaireClass::make("Quote",Language._Basic_instruction,claire.it));
    CL_ADD_SLOT(Language._Quote,Quote,Kernel.arg,arg,Kernel._any,CNULL);
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Language._Quote),Kernel._void,
  	NEW_ALLOC,_function_(self_print_Quote_Language,"self_print_Quote_Language"));
  
  Core.self_eval->addMethod(list::domain(1,Language._Quote),Kernel._any,
  	RETURN_ARG,_function_(self_eval_Quote,"self_eval_Quote"));
  
  (Language._Optimized_instruction = ClaireClass::make("Optimized_instruction",Language._Complex_instruction,claire.it));
  
  { (Language._Call_method = ClaireClass::make("Call_method",Language._Optimized_instruction,claire.it));
    CL_ADD_SLOT(Language._Call_method,Call_method,Kernel.arg,arg,Kernel._method,CNULL);
    CL_ADD_SLOT(Language._Call_method,Call_method,Core.args,args,Kernel._list,CNULL);
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Language._Call_method),Kernel._void,
  	NEW_ALLOC+SLOT_UPDATE,_function_(self_print_Call_method_Language,"self_print_Call_method_Language"));
  
  Core.self_eval->addMethod(list::domain(1,Language._Call_method),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(self_eval_Call_method,"self_eval_Call_method"));
  
  { (Language._Call_method1 = ClaireClass::make("Call_method1",Language._Call_method,claire.it));
    Core.self_eval->addMethod(list::domain(1,Language._Call_method1),Kernel._any,
    	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(self_eval_Call_method1,"self_eval_Call_method1"));
    } 
  
  { (Language._Call_method2 = ClaireClass::make("Call_method2",Language._Call_method,claire.it));
    Core.self_eval->addMethod(list::domain(1,Language._Call_method2),Kernel._any,
    	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(self_eval_Call_method2,"self_eval_Call_method2"));
    } 
  
  { (Language._Call_slot = ClaireClass::make("Call_slot",Language._Optimized_instruction,claire.it));
    CL_ADD_SLOT(Language._Call_slot,Call_slot,Kernel.selector,selector,Kernel._slot,CNULL);
    CL_ADD_SLOT(Language._Call_slot,Call_slot,Kernel.arg,arg,Kernel._any,CNULL);
    CL_ADD_SLOT(Language._Call_slot,Call_slot,Language.test,test,Kernel._boolean,CNULL);
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Language._Call_slot),Kernel._void,
  	NEW_ALLOC,_function_(self_print_Call_slot_Language,"self_print_Call_slot_Language"));
  
  Core.self_eval->addMethod(list::domain(1,Language._Call_slot),Kernel._any,
  	NEW_ALLOC+RETURN_ARG,_function_(self_eval_Call_slot,"self_eval_Call_slot"));
  
  { (Language._Call_array = ClaireClass::make("Call_array",Language._Optimized_instruction,claire.it));
    CL_ADD_SLOT(Language._Call_array,Call_array,Kernel.selector,selector,Kernel._any,CNULL);
    CL_ADD_SLOT(Language._Call_array,Call_array,Kernel.arg,arg,Kernel._any,CNULL);
    CL_ADD_SLOT(Language._Call_array,Call_array,Language.test,test,Kernel._any,CNULL);
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Language._Call_array),Kernel._void,
  	NEW_ALLOC,_function_(self_print_Call_array_Language,"self_print_Call_array_Language"));
  
  Core.self_eval->addMethod(list::domain(1,Language._Call_array),Kernel._any,
  	NEW_ALLOC+RETURN_ARG,_function_(self_eval_Call_array,"self_eval_Call_array"));
  
  { (Language._Call_table = ClaireClass::make("Call_table",Language._Optimized_instruction,claire.it));
    CL_ADD_SLOT(Language._Call_table,Call_table,Kernel.selector,selector,Kernel._table,CNULL);
    CL_ADD_SLOT(Language._Call_table,Call_table,Kernel.arg,arg,Kernel._any,CNULL);
    CL_ADD_SLOT(Language._Call_table,Call_table,Language.test,test,Kernel._boolean,CNULL);
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Language._Call_table),Kernel._void,
  	NEW_ALLOC,_function_(self_print_Call_table_Language,"self_print_Call_table_Language"));
  
  Core.self_eval->addMethod(list::domain(1,Language._Call_table),Kernel._any,
  	NEW_ALLOC+RETURN_ARG,_function_(self_eval_Call_table,"self_eval_Call_table"));
  
  { (Language._Update = ClaireClass::make("Update",Language._Optimized_instruction,claire.it));
    CL_ADD_SLOT(Language._Update,Update,Kernel.selector,selector,Kernel._any,CNULL);
    CL_ADD_SLOT(Language._Update,Update,Kernel.arg,arg,Kernel._any,CNULL);
    CL_ADD_SLOT(Language._Update,Update,Kernel.value,value,Kernel._any,CNULL);
    CL_ADD_SLOT(Language._Update,Update,Language.var,var,Kernel._any,CNULL);
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Language._Update),Kernel._void,
  	NEW_ALLOC,_function_(self_print_Update_Language,"self_print_Update_Language"));
  
  Core.self_eval->addMethod(list::domain(1,Language._Update),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(self_eval_Update,"self_eval_Update"));
  
  { (Language._Super = ClaireClass::make("Super",Language._Control_structure,claire.it));
    CL_ADD_SLOT(Language._Super,Super,Kernel.selector,selector,Kernel._property,CNULL);
    CL_ADD_SLOT(Language._Super,Super,Language.cast_to,cast_to,Kernel._type,CNULL);
    CL_ADD_SLOT(Language._Super,Super,Core.args,args,Kernel._list,CNULL);
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Language._Super),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(self_print_Super_Language,"self_print_Super_Language"));
  
  Core.self_eval->addMethod(list::domain(1,Language._Super),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(self_eval_Super,"self_eval_Super"));
  
  { (Language._Cast = ClaireClass::make("Cast",Language._Basic_instruction,claire.it));
    CL_ADD_SLOT(Language._Cast,Cast,Kernel.arg,arg,Kernel._any,CNULL);
    CL_ADD_SLOT(Language._Cast,Cast,Language.set_arg,set_arg,Kernel._type,CNULL);
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Language._Cast),Kernel._void,
  	NEW_ALLOC+SLOT_UPDATE,_function_(self_print_Cast_Language,"self_print_Cast_Language"));
  
  Core.self_eval->addMethod(list::domain(1,Language._Cast),Kernel._any,
  	NEW_ALLOC+RETURN_ARG,_function_(self_eval_Cast,"self_eval_Cast"));
  
  { (Language._Return = ClaireClass::make("Return",Language._Basic_instruction,claire.it));
    CL_ADD_SLOT(Language._Return,Return,Kernel.arg,arg,Kernel._any,CNULL);
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Language._Return),Kernel._void,
  	NEW_ALLOC+SLOT_UPDATE,_function_(self_print_Return_Language,"self_print_Return_Language"));
  
  Core.self_eval->addMethod(list::domain(1,Language._Return),Kernel._any,
  	NEW_ALLOC,_function_(self_eval_Return,"self_eval_Return"));
  
  Language.substitution->addMethod(list::domain(3,Kernel._any,Language._Variable,Kernel._any),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(substitution_any,"substitution_any"));
  
  Language.occurrence->addMethod(list::domain(2,Kernel._any,Language._Variable),Kernel._integer,
  	NEW_ALLOC,_function_(occurrence_any,"occurrence_any"));
  
  Language.instruction_copy->addMethod(list::domain(1,Kernel._any),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(instruction_copy_any,"instruction_copy_any"));
  
  { (Language._If = ClaireClass::make("If",Language._Control_structure,claire.it));
    CL_ADD_SLOT(Language._If,If,Language.test,test,Kernel._any,CNULL);
    CL_ADD_SLOT(Language._If,If,Kernel.arg,arg,Kernel._any,CNULL);
    CL_ADD_SLOT(Language._If,If,Language.other,other,Kernel._any,Kernel.cfalse);
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Language._If),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(self_print_If_Language,"self_print_If_Language"));
  
  Language.printstat->addMethod(list::domain(1,Language._If),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(printstat_If,"printstat_If"));
  
  Language.printif->addMethod(list::domain(1,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(printif_any,"printif_any"));
  
  Language.printelse->addMethod(list::domain(1,Language._If),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(printelse_If,"printelse_If"));
  
  Core.self_eval->addMethod(list::domain(1,Language._If),Kernel._any,
  	NEW_ALLOC,_function_(self_eval_If,"self_eval_If"));
  
  { (Language._Do = ClaireClass::make("Do",Language._Control_structure,claire.it));
    CL_ADD_SLOT(Language._Do,Do,Core.args,args,Kernel._list,CNULL);
    (Language._Do->params = list::alloc(Kernel._any,1,_oid_(Core.args)));
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Language._Do),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(self_print_Do_Language,"self_print_Do_Language"));
  
  Language.printdo->addMethod(list::domain(2,Kernel._list,Kernel._boolean),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(printdo_list,"printdo_list"));
  
  Language.printblock->addMethod(list::domain(1,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(printblock_any,"printblock_any"));
  
  Core.self_eval->addMethod(list::domain(1,Language._Do),Kernel._any,
  	NEW_ALLOC,_function_(self_eval_Do,"self_eval_Do"));
  
  { (Language._Let = ClaireClass::make("Let",Language._Instruction_with_var,claire.it));
    CL_ADD_SLOT(Language._Let,Let,Kernel.value,value,Kernel._any,CNULL);
    CL_ADD_SLOT(Language._Let,Let,Kernel.arg,arg,Kernel._any,CNULL);
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Language._Let),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(self_print_Let_Language,"self_print_Let_Language"));
  
  Language.printbody->addMethod(list::domain(1,Language._Let),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(printbody_Let,"printbody_Let"));
  
  Core.self_eval->addMethod(list::domain(1,Language._Let),Kernel._any,
  	NEW_ALLOC+SLOT_UPDATE,_function_(self_eval_Let,"self_eval_Let"));
  
  { (Language._When = ClaireClass::make("When",Language._Let,claire.it));
    CL_ADD_SLOT(Language._When,When,Language.other,other,Kernel._any,CNULL);
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Language._When),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(self_print_When_Language,"self_print_When_Language"));
  
  Core.self_eval->addMethod(list::domain(1,Language._When),Kernel._any,
  	NEW_ALLOC+SLOT_UPDATE,_function_(self_eval_When,"self_eval_When"));
  
  (Language._Let_plus = ClaireClass::make("Let+",Language._Let,claire.it));
  
  (Language._Let_star = ClaireClass::make("Let*",Language._Let,claire.it));
  
  Kernel.self_print->addMethod(list::domain(1,Language._Let_plus),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(self_print_Let_plus_Language,"self_print_Let+_Language"));
  
  Kernel.self_print->addMethod(list::domain(1,Language._Let_star),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(self_print_Let_star_Language,"self_print_Let*_Language"));
  
  { (Language._Iteration = ClaireClass::make("Iteration",Language._Instruction_with_var,claire.it));
    CL_ADD_SLOT(Language._Iteration,Iteration,Language.set_arg,set_arg,Kernel._any,CNULL);
    CL_ADD_SLOT(Language._Iteration,Iteration,Kernel.arg,arg,Kernel._any,CNULL);
    } 
  
  { (Language.iterate = property::make("iterate",2,claire.it,Kernel._any,0));
    ;} 
  
  { (Language.Iterate = property::make("Iterate",2,claire.it,Kernel._any,0));
    ;} 
  
  (Language._For = ClaireClass::make("For",Language._Iteration,claire.it));
  
  Kernel.self_print->addMethod(list::domain(1,Language._For),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(self_print_For_Language,"self_print_For_Language"));
  
  Core.self_eval->addMethod(list::domain(1,Language._For),Kernel._any,
  	NEW_ALLOC+SLOT_UPDATE+RETURN_ARG,_function_(self_eval_For,"self_eval_For"));
  
  { (Language._Collect = ClaireClass::make("Collect",Language._Iteration,claire.it));
    CL_ADD_SLOT(Language._Collect,Collect,Kernel.of,of,Kernel._type,CNULL);
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Language._Collect),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(self_print_Collect_Language,"self_print_Collect_Language"));
  
  Core.self_eval->addMethod(list::domain(1,Language._Collect),Kernel._any,
  	NEW_ALLOC,_function_(self_eval_Collect,"self_eval_Collect"));
  
  { (Language._Image = ClaireClass::make("Image",Language._Iteration,claire.it));
    CL_ADD_SLOT(Language._Image,Image,Kernel.of,of,Kernel._type,CNULL);
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Language._Image),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(self_print_Image_Language,"self_print_Image_Language"));
  
  Core.self_eval->addMethod(list::domain(1,Language._Image),Kernel._any,
  	NEW_ALLOC,_function_(self_eval_Image,"self_eval_Image"));
  
  { (Language._Select = ClaireClass::make("Select",Language._Iteration,claire.it));
    CL_ADD_SLOT(Language._Select,Select,Kernel.of,of,Kernel._type,CNULL);
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Language._Select),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(self_print_Select_Language,"self_print_Select_Language"));
  
  Core.self_eval->addMethod(list::domain(1,Language._Select),Kernel._any,
  	NEW_ALLOC,_function_(self_eval_Select,"self_eval_Select"));
  
  { (Language._Lselect = ClaireClass::make("Lselect",Language._Iteration,claire.it));
    CL_ADD_SLOT(Language._Lselect,Lselect,Kernel.of,of,Kernel._type,CNULL);
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Language._Lselect),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(self_print_Lselect_Language,"self_print_Lselect_Language"));
  
  Core.self_eval->addMethod(list::domain(1,Language._Lselect),Kernel._any,
  	NEW_ALLOC,_function_(self_eval_Lselect,"self_eval_Lselect"));
  
  { (Language._Exists = ClaireClass::make("Exists",Language._Iteration,claire.it));
    CL_ADD_SLOT(Language._Exists,Exists,Language.other,other,Kernel._any,Kernel.cfalse);
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Language._Exists),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(self_print_Exists_Language,"self_print_Exists_Language"));
  
  Core.self_eval->addMethod(list::domain(1,Language._Exists),Kernel._any,
  	NEW_ALLOC,_function_(self_eval_Exists,"self_eval_Exists"));
  
  { (Language._Case = ClaireClass::make("Case",Language._Control_structure,claire.it));
    CL_ADD_SLOT(Language._Case,Case,Language.var,var,Kernel._any,CNULL);
    CL_ADD_SLOT(Language._Case,Case,Core.args,args,Kernel._list,CNULL);
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Language._Case),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(self_print_Case_Language,"self_print_Case_Language"));
  
  Core.self_eval->addMethod(list::domain(1,Language._Case),Kernel._any,
  	NEW_ALLOC,_function_(self_eval_Case,"self_eval_Case"));
  
  { (Language._While = ClaireClass::make("While",Language._Control_structure,claire.it));
    CL_ADD_SLOT(Language._While,While,Language.test,test,Kernel._any,CNULL);
    CL_ADD_SLOT(Language._While,While,Kernel.arg,arg,Kernel._any,CNULL);
    CL_ADD_SLOT(Language._While,While,Language.other,other,Kernel._boolean,Kernel.cfalse);
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Language._While),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(self_print_While_Language,"self_print_While_Language"));
  
  Core.self_eval->addMethod(list::domain(1,Language._While),Kernel._any,
  	NEW_ALLOC+RETURN_ARG,_function_(self_eval_While,"self_eval_While"));
  
  { (Language._Handle = ClaireClass::make("Handle",Language._Control_structure,claire.it));
    CL_ADD_SLOT(Language._Handle,ClaireHandle,Language.test,test,Kernel._any,CNULL);
    CL_ADD_SLOT(Language._Handle,ClaireHandle,Kernel.arg,arg,Kernel._any,CNULL);
    CL_ADD_SLOT(Language._Handle,ClaireHandle,Language.other,other,Kernel._any,CNULL);
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Language._Handle),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(self_print_Handle_Language,"self_print_Handle_Language"));
  
  Core.self_eval->addMethod(list::domain(1,Language._Handle),Kernel._any,
  	NEW_ALLOC+RETURN_ARG,_function_(self_eval_Handle,"self_eval_Handle"));
  
  { (Language._Construct = ClaireClass::make("Construct",Language._Complex_instruction,claire.it));
    CL_ADD_SLOT(Language._Construct,Construct,Core.args,args,Kernel._list,CNULL);
    } 
  
  { (Language._List = ClaireClass::make("List",Language._Construct,claire.it));
    CL_ADD_SLOT(Language._List,List,Kernel.of,of,Kernel._type,CNULL);
    } 
  
  (Language._Tuple = ClaireClass::make("Tuple",Language._Construct,claire.it));
  
  { (Language._Set = ClaireClass::make("Set",Language._Construct,claire.it));
    CL_ADD_SLOT(Language._Set,Set,Kernel.of,of,Kernel._type,CNULL);
    } 
  
  { (Language._Array = ClaireClass::make("Array",Language._Construct,claire.it));
    CL_ADD_SLOT(Language._Array,Array,Kernel.of,of,Kernel._type,CNULL);
    } 
  
  (Language._Printf = ClaireClass::make("Printf",Language._Construct,claire.it));
  
  (Language._Error = ClaireClass::make("Error",Language._Construct,claire.it));
  
  (Language._Branch = ClaireClass::make("Branch",Language._Construct,claire.it));
  
  Kernel.self_print->addMethod(list::domain(1,Language._Construct),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(self_print_Construct_Language,"self_print_Construct_Language"));
  
  Core.self_eval->addMethod(list::domain(1,Language._List),Kernel._any,
  	NEW_ALLOC,_function_(self_eval_List,"self_eval_List"));
  
  Core.self_eval->addMethod(list::domain(1,Language._Set),Kernel._any,
  	NEW_ALLOC,_function_(self_eval_Set,"self_eval_Set"));
  
  Core.self_eval->addMethod(list::domain(1,Language._Tuple),Kernel._any,
  	NEW_ALLOC,_function_(self_eval_Tuple,"self_eval_Tuple"));
  
  Core.self_eval->addMethod(list::domain(1,Language._Array),Kernel._any,
  	NEW_ALLOC,_function_(self_eval_Array2,"self_eval_Array2"));
  
  (Language._Macro = ClaireClass::make("Macro",Language._Construct,claire.it));
  
  { (Language.macroexpand = property::make("macroexpand",3,claire.it,Kernel._any,0));
    (Language.macroexpand->open = 3);
    ;} 
  
  Core.self_eval->addMethod(list::domain(1,Language._Macro),Kernel._any,
  	NEW_ALLOC,_function_(self_eval_Macro2,"self_eval_Macro2"));
  
  Core.self_eval->addMethod(list::domain(1,Language._Error),Kernel._void,
  	NEW_ALLOC+SLOT_UPDATE+RETURN_ARG,_function_(self_eval_Error,"self_eval_Error"));
  
  Core.self_eval->addMethod(list::domain(1,Language._Printf),Kernel._any,
  	NEW_ALLOC+SLOT_UPDATE,_function_(self_eval_Printf,"self_eval_Printf"));
  
  (Language._Trace = ClaireClass::make("Trace",Language._Construct,claire.it));
  
  Core.self_eval->addMethod(list::domain(1,Language._Trace),Kernel._any,
  	NEW_ALLOC+SLOT_UPDATE,_function_(self_eval_Trace,"self_eval_Trace"));
  
  { (Language._Assert = ClaireClass::make("Assert",Language._Construct,claire.it));
    CL_ADD_SLOT(Language._Assert,Assert,Kernel.index,index,Kernel._integer,CNULL);
    CL_ADD_SLOT(Language._Assert,Assert,Kernel.external,external,Kernel._string,CNULL);
    } 
  
  Core.self_eval->addMethod(list::domain(1,Language._Assert),Kernel._any,
  	NEW_ALLOC+SLOT_UPDATE,_function_(self_eval_Assert,"self_eval_Assert"));
  
  Core.self_eval->addMethod(list::domain(1,Language._Branch),Kernel._any,
  	NEW_ALLOC,_function_(self_eval_Branch,"self_eval_Branch"));
  
  { (Language.extract_item = property::make("extract_item",1,claire.it,Kernel._any,0));
    ;} 
  
  { (Language.function_I = property::make("function!",2,claire.it,Kernel._any,0));
    ;} 
  
  { global_variable * _CL_obj = (Language.LastComment = (global_variable *) Core._global_variable->instantiate("LastComment",iClaire.it));
    (_CL_obj->range = Kernel._any);
    (_CL_obj->value = CNULL);
    close_global_variable(_CL_obj);
    } 
  
  { global_variable * _CL_obj = (Language.NeedComment = (global_variable *) Core._global_variable->instantiate("NeedComment",claire.it));
    (_CL_obj->range = Kernel._boolean);
    (_CL_obj->value = Kernel.cfalse);
    close_global_variable(_CL_obj);
    } 
  
  (Language._Defclaire = ClaireClass::make("Defclaire",Language._Complex_instruction,claire.it));
  
  { (Language._Definition = ClaireClass::make("Definition",Language._Defclaire,claire.it));
    CL_ADD_SLOT(Language._Definition,Definition,Kernel.arg,arg,Kernel._class,CNULL);
    CL_ADD_SLOT(Language._Definition,Definition,Core.args,args,Kernel._list,CNULL);
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Language._Definition),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(self_print_Definition_Language,"self_print_Definition_Language"));
  
  { (Language._Defobj = ClaireClass::make("Defobj",Language._Definition,claire.it));
    CL_ADD_SLOT(Language._Defobj,Defobj,Language.ident,ident,Kernel._symbol,CNULL);
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Language._Defobj),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(self_print_Defobj_Language,"self_print_Defobj_Language"));
  
  { (Language._Defclass = ClaireClass::make("Defclass",Language._Defobj,claire.it));
    CL_ADD_SLOT(Language._Defclass,Defclass,Kernel.params,params,Kernel._list,CNULL);
    CL_ADD_SLOT(Language._Defclass,Defclass,Language.forward_ask,forward_ask,Kernel._boolean,CNULL);
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Language._Defclass),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(self_print_Defclass_Language,"self_print_Defclass_Language"));
  
  { (Language._Defmethod = ClaireClass::make("Defmethod",Language._Defclaire,claire.it));
    CL_ADD_SLOT(Language._Defmethod,Defmethod,Kernel.arg,arg,Language._Call,CNULL);
    CL_ADD_SLOT(Language._Defmethod,Defmethod,Language.set_arg,set_arg,Kernel._any,CNULL);
    CL_ADD_SLOT(Language._Defmethod,Defmethod,Kernel.body,body,Kernel._any,CNULL);
    CL_ADD_SLOT(Language._Defmethod,Defmethod,Kernel.inline_ask,inline_ask,Kernel._any,CNULL);
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Language._Defmethod),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(self_print_Defmethod_Language,"self_print_Defmethod_Language"));
  
  (Language._Defarray = ClaireClass::make("Defarray",Language._Defmethod,claire.it));
  
  Kernel.self_print->addMethod(list::domain(1,Language._Defarray),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(self_print_Defarray_Language,"self_print_Defarray_Language"));
  
  { (Language._Defrule = ClaireClass::make("Defrule",Language._Defclaire,claire.it));
    CL_ADD_SLOT(Language._Defrule,Defrule,Language.ident,ident,Kernel._symbol,CNULL);
    CL_ADD_SLOT(Language._Defrule,Defrule,Core.args,args,Kernel._list,CNULL);
    CL_ADD_SLOT(Language._Defrule,Defrule,Kernel.arg,arg,Kernel._any,CNULL);
    CL_ADD_SLOT(Language._Defrule,Defrule,Kernel.body,body,Kernel._any,CNULL);
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Language._Defrule),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(self_print_Defrule_Language,"self_print_Defrule_Language"));
  
  { (Language._Defvar = ClaireClass::make("Defvar",Language._Defclaire,claire.it));
    CL_ADD_SLOT(Language._Defvar,Defvar,Language.ident,ident,Language._Variable,CNULL);
    CL_ADD_SLOT(Language._Defvar,Defvar,Kernel.arg,arg,Kernel._any,CNULL);
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Language._Defvar),Kernel._void,
  	NEW_ALLOC+SLOT_UPDATE,_function_(self_print_Defvar_Language,"self_print_Defvar_Language"));
  
  Core.self_eval->addMethod(list::domain(1,Language._Definition),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(self_eval_Definition,"self_eval_Definition"));
  
  { (Language.complete = property::make("complete",1,claire.it,Kernel._any,0));
    ;} 
  
  Language.complete->addMethod(list::domain(2,Kernel._object,Kernel._list),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(complete_object,"complete_object"));
  
  Core.self_eval->addMethod(list::domain(1,Language._Defobj),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(self_eval_Defobj,"self_eval_Defobj"));
  
  Core.self_eval->addMethod(list::domain(1,Language._Defclass),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(self_eval_Defclass,"self_eval_Defclass"));
  
  { global_variable * _CL_obj = (Language.LDEF = (global_variable *) Core._global_variable->instantiate("LDEF",claire.it));
    (_CL_obj->range = Kernel._any);
    (_CL_obj->value = _oid_(list::empty()));
    close_global_variable(_CL_obj);
    } 
  
  Core.self_eval->addMethod(list::domain(1,Language._Defmethod),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+SAFE_RESULT,_function_(self_eval_Defmethod,"self_eval_Defmethod"));
  
  { (Kernel._Z->open = -1);
    (Kernel._sup_equal->open = -1);
    (Kernel._equal->open = -1);
    } 
  
  Language.attach_comment->addMethod(list::domain(1,Kernel._any),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(attach_comment_any,"attach_comment_any"));
  
  Language.extract_signature->addMethod(list::domain(1,Kernel._list),Kernel._list,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(extract_signature_list,"extract_signature_list"));
  
  Language.extract_pattern->addMethod(list::domain(2,Kernel._any,Kernel._list),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(extract_pattern_any,"extract_pattern_any"));
  
  Language.extract_type->addMethod(list::domain(1,Kernel._any),Kernel._type,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(extract_type_any,"extract_type_any"));
  
  Language.extract_item->addMethod(list::domain(2,Kernel._any,Kernel._any),Kernel._any,
  	NEW_ALLOC+RETURN_ARG,_function_(extract_item_any,"extract_item_any"));
  
  Language.extract_pattern_nth->addMethod(list::domain(2,Kernel._list,Kernel._list),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(extract_pattern_nth_list,"extract_pattern_nth_list"));
  
  Language.extract_class_call->addMethod(list::domain(2,Kernel._class,Kernel._list),Kernel._object,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(extract_class_call_class,"extract_class_call_class"));
  
  Language.extract_range->addMethod(list::domain(3,Kernel._any,Kernel._list,Kernel._list),Kernel._list,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(extract_range_any,"extract_range_any"));
  
  { global_variable * _CL_obj = (Language._NEW_ALLOC = (global_variable *) Core._global_variable->instantiate("NEW_ALLOC",claire.it));
    (_CL_obj->range = Kernel.emptySet);
    (_CL_obj->value = 1);
    close_global_variable(_CL_obj);
    } 
  
  { global_variable * _CL_obj = (Language._BAG_UPDATE = (global_variable *) Core._global_variable->instantiate("BAG_UPDATE",claire.it));
    (_CL_obj->range = Kernel.emptySet);
    (_CL_obj->value = 2);
    close_global_variable(_CL_obj);
    } 
  
  { global_variable * _CL_obj = (Language._SLOT_UPDATE = (global_variable *) Core._global_variable->instantiate("SLOT_UPDATE",claire.it));
    (_CL_obj->range = Kernel.emptySet);
    (_CL_obj->value = 3);
    close_global_variable(_CL_obj);
    } 
  
  { global_variable * _CL_obj = (Language._RETURN_ARG = (global_variable *) Core._global_variable->instantiate("RETURN_ARG",claire.it));
    (_CL_obj->range = Kernel.emptySet);
    (_CL_obj->value = 4);
    close_global_variable(_CL_obj);
    } 
  
  { global_variable * _CL_obj = (Language._SAFE_RESULT = (global_variable *) Core._global_variable->instantiate("SAFE_RESULT",claire.it));
    (_CL_obj->range = Kernel.emptySet);
    (_CL_obj->value = 5);
    close_global_variable(_CL_obj);
    } 
  
  { global_variable * _CL_obj = (Language._SAFE_GC = (global_variable *) Core._global_variable->instantiate("SAFE_GC",claire.it));
    (_CL_obj->range = Kernel.emptySet);
    (_CL_obj->value = 6);
    close_global_variable(_CL_obj);
    } 
  
  { (Language.bit_vector = property::make("bit_vector",2,claire.it,Kernel._any,0));
    ;} 
  
  Language.bit_vector->addMethod(list::domain(1,Kernel._listargs),Kernel._integer,
  	0,_function_(bit_vector_listargs2,"bit_vector_listargs2"));
  
  Language.extract_status->addMethod(list::domain(1,Kernel._any),Kernel._list,
  	NEW_ALLOC,_function_(extract_status_any,"extract_status_any"));
  
  Language.type_I->addMethod(list::domain(1,Kernel._any),Kernel._type,
  	NEW_ALLOC+RETURN_ARG,_function_(type_I_any,"type!_any"));
  
  Core.self_eval->addMethod(list::domain(1,Language._Defarray),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+SAFE_RESULT,_function_(self_eval_Defarray,"self_eval_Defarray"));
  
  { (Language._demon = ClaireClass::make("demon",Core._lambda,Language.it));
    CL_ADD_SLOT(Language._demon,Language_demon,Language.pname,pname,Kernel._symbol,_oid_(symbol_I_string2("unamed")));
    CL_ADD_SLOT(Language._demon,Language_demon,Language.priority,priority,Kernel._integer,0);
    CL_ADD_SLOT(Language._demon,Language_demon,Kernel.formula,formula,Core._lambda,CNULL);
    } 
  
  Kernel.self_print->addMethod(list::domain(1,Language._demon),Kernel._void,
  	0,_function_(self_print_demon,"self_print_demon"));
  
  Kernel.funcall->addMethod(list::domain(3,Language._demon,Kernel._any,Kernel._any),Kernel._any,
  	NEW_ALLOC,_function_(funcall_demon1,"funcall_demon1"));
  
  Kernel.funcall->addMethod(list::domain(4,Language._demon,
    Kernel._any,
    Kernel._any,
    Kernel._any),Kernel._any,
  	NEW_ALLOC,_function_(funcall_demon2,"funcall_demon2"));
  
  { (Language.demons = (table *) Kernel._table->instantiate("demons",claire.it));
    (Language.demons->multivalued_ask = Kernel._list);
    (Language.demons->range = param_I_class(Kernel._list,Language._demon));
    (Language.demons->params = _oid_(Kernel._any));
    (Language.demons->domain = Kernel._relation);
    (Language.demons->graph = make_list_integer(29,CNULL));
    (Language.demons->DEFAULT = _oid_(list::empty(Language._demon)));
    } 
  
  { (Language._inf_dash = (operation *) Kernel._operation->instantiate("<-",claire.it));
    ;} 
  
  (Language._rule_object = ClaireClass::make("rule_object",Kernel._property,Language.it));
  
  { (Language.relations = (table *) Kernel._table->instantiate("relations",Language.it));
    (Language.relations->multivalued_ask = CTRUE);
    (Language.relations->range = Kernel._set);
    (Language.relations->params = _oid_(Kernel._any));
    (Language.relations->domain = Language._rule_object);
    (Language.relations->graph = make_list_integer(29,CNULL));
    (Language.relations->DEFAULT = _oid_(Kernel.emptySet));
    } 
  
  { (Language.last_rule = (table *) Kernel._table->instantiate("last_rule",Language.it));
    (Language.last_rule->range = Language._rule_object);
    (Language.last_rule->params = _oid_(Kernel._any));
    (Language.last_rule->domain = Kernel._relation);
    (Language.last_rule->graph = make_list_integer(29,CNULL));
    (Language.last_rule->DEFAULT = CNULL);
    } 
  
  update_property(Kernel.inverse,
    Language.relations,
    8,
    Kernel._object,
    _oid_(Language.last_rule));
  
  { (Language.eval_rule = property::make("eval_rule",3,claire.it,Kernel._any,0));
    (Language.eval_rule->open = 3);
    ;} 
  
  Core.self_eval->addMethod(list::domain(1,Language._Defrule),Kernel._any,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(self_eval_Defrule,"self_eval_Defrule"));
  
  Language.eventMethod_ask->addMethod(list::domain(1,Kernel._relation),Kernel._boolean,
  	0,_function_(eventMethod_ask_relation2,"eventMethod?_relation2"));
  
  Language.make_filter->addFloatMethod(list::domain(1,Kernel._any),tuple::alloc(2,_oid_(Kernel._relation),GC_OID(_oid_(nth_class1(Kernel._list,Language._Variable)))),
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(make_filter_any,"make_filter_any"),_function_(make_filter_any_,"make_filter_any_"));
  
  Language.make_demon->addMethod(list::domain(5,Kernel._relation,
    Kernel._symbol,
    nth_class1(Kernel._list,Language._Variable),
    Kernel._any,
    Kernel._any),Language._demon,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(make_demon_relation,"make_demon_relation"));
  
  Language.readCall->addMethod(list::domain(2,Kernel._relation,Kernel._any),Language._Call,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE,_function_(readCall_relation,"readCall_relation"));
  
  Language.putCall->addMethod(list::domain(3,Kernel._relation,Kernel._any,Kernel._any),Language._Call,
  	NEW_ALLOC,_function_(putCall_relation2,"putCall_relation2"));
  
  Language.safeRange->addMethod(list::domain(1,Kernel._relation),Kernel._type,
  	RETURN_ARG,_function_(safeRange_relation,"safeRange_relation"));
  
  Language.eval_if_write->addMethod(list::domain(1,Kernel._relation),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE+SLOT_UPDATE+RETURN_ARG,_function_(eval_if_write_relation,"eval_if_write_relation"));
  
  Language.eventMethod->addMethod(list::domain(1,Kernel._property),Kernel._void,
  	NEW_ALLOC+SLOT_UPDATE+RETURN_ARG,_function_(eventMethod_property,"eventMethod_property"));
  
  { (Language.InterfaceList = (table *) Kernel._table->instantiate("InterfaceList",Language.it));
    (Language.InterfaceList->multivalued_ask = Kernel._list);
    (Language.InterfaceList->range = Kernel._list);
    (Language.InterfaceList->params = _oid_(Kernel._any));
    (Language.InterfaceList->domain = Kernel._class);
    (Language.InterfaceList->graph = make_list_integer(29,CNULL));
    (Language.InterfaceList->DEFAULT = Core.nil->value);
    } 
  
  { (Language.ClaireInterface = property::make("interface",1,claire.it,Kernel._any,0));
    ;} 
  
  Language.ClaireInterface->addMethod(list::domain(1,Kernel._property),Kernel._void,
  	NEW_ALLOC+SLOT_UPDATE+RETURN_ARG,_function_(interface_property,"interface_property"));
  
  Language.ClaireInterface->addMethod(list::domain(2,Kernel._class,Kernel._listargs),Kernel._void,
  	NEW_ALLOC+BAG_UPDATE,_function_(interface_class,"interface_class"));
  
  { (Kernel._table->open = 0);
    (Kernel._class->open = 0);
    { OID gc_local;
      ITERATE(x);
      bag *x_support;
      x_support = Language._Instruction->descendents;
      for (START(x_support); NEXT(x);)
      { GC_LOOP;
        { OID  m = GC_OID(_oid_(_at_property1(Core.self_eval,OBJECT(ClaireClass,x))));
          (OBJECT(ClaireClass,x)->open = 3);
          if (Kernel._method == OWNER(m))
           (OBJECT(ClaireClass,x)->evaluate = OBJECT(method,m)->functional);
          } 
        GC_UNLOOP;} 
      } 
    } 
  
  GC_UNBIND;} 

