// --- System configuration file for "cclaire" , ["Mon Nov  8 16:37:05 2004\n"] ---

#include <claire.h>
#include <Kernel.h>
#include <Core.h>
#include <Language.h>
#include <Reader.h>
#include <Optimize.h>
#include <Generate.h>



void loadModules() 
{ //module definitions 
  Core.initModule("Core",mClaire.it,list::alloc(Kernel.emptySet,1,_oid_(Kernel.it)),
  "c:\\claire\\v3.3\\src\\meta",list::alloc(Kernel.emptySet,4,_string_("method"),
    _string_("object"),
    _string_("function"),
    _string_("types")));
  iClaire.initModule("iClaire",claire.it,list::alloc(Kernel.emptySet,1,_oid_(mClaire.it)),
  "",Kernel.nil);
  Language.initModule("Language",iClaire.it,list::alloc(Kernel.emptySet,2,_oid_(Kernel.it),_oid_(Core.it)),
  "c:\\claire\\v3.3\\src\\meta",list::alloc(Kernel.emptySet,4,_string_("pretty"),
    _string_("call"),
    _string_("control"),
    _string_("define")));
  Reader.initModule("Reader",iClaire.it,list::alloc(Kernel.emptySet,3,_oid_(Kernel.it),
    _oid_(Core.it),
    _oid_(Language.it)),
  "c:\\claire\\v3.3\\src\\meta",list::alloc(Kernel.emptySet,4,_string_("read"),
    _string_("syntax"),
    _string_("file"),
    _string_("inspect")));
  Compile.initModule("Compile",iClaire.it,list::alloc(Kernel._module,1,_oid_(mClaire.it)),
  "",list::empty(Kernel._string));
  Optimize.initModule("Optimize",Compile.it,list::alloc(Kernel._module,2,_oid_(Reader.it),_oid_(mClaire.it)),
  "c:\\claire\\v3.3\\src\\compile",list::alloc(Kernel._string,5,_string_("osystem"),
    _string_("otool"),
    _string_("ocall"),
    _string_("ocontrol"),
    _string_("odefine")));
  Generate.initModule("Generate",Compile.it,list::alloc(Kernel._module,2,_oid_(Optimize.it),_oid_(mClaire.it)),
  "c:\\claire\\v3.3\\src\\compile",list::alloc(Kernel._string,5,_string_("gsystem"),
    _string_("gexp"),
    _string_("gstat"),
    _string_("cgen"),
    _string_("copt")));
  //module load 
  Core.metaLoad();
  Language.metaLoad();
  Reader.metaLoad();
  Optimize.metaLoad();
  Generate.metaLoad();
  ClEnv->module_I = claire.it; 
  } 

extern void main_list(list *l);
void call_main() {main_list(ClEnv->params);}
