

extern char * external_I_module(module *m);
extern module * string2module_string(char *s);
extern void  printHelp_void();
extern void  main_list(list *lp);
extern void  function_compile_string(char *self,char *fullname);
extern void  lib_I_any(OID m,list *l);
extern void  files_I_any(OID m,ClaireBoolean *link_ask,char *_Zout);
extern void  cmakefile_any(OID m,char *out);
extern void  create_makefile_nt_any(OID m,char *out,list *l);
extern void  create_makefile_unix_any(OID m,char *out,list *l);