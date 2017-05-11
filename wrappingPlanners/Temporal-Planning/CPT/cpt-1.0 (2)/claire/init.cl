// +----------------------------------------------------------------------+
// | init.cl  
// +----------------------------------------------------------------------+

// note the environment must be set-up through the -env declaration
// this file works for most UNIX and NT setups, you may need to edit it
// though

(printf("this is a init file for cclaire  for OS: ~A -------------------\n", compiler.env))

// this init file is needed to generate a claire.exe from the local sclaire.exe

ROOT:string :: getenv("CLAIRE3_HOME")

(compiler.inline? := true,      // do not change
 OPT.Optimize/recompute := false,
 compiler.naming := 1,
 if (compiler.env = "unix")
    (*fs* := "/",
     PRODUCER.Generate/extension := ".cc",
     // compiler options: optimized, debug, default
     compiler.options := list("-c -fwritable-strings -O2", 
                              "-c -fwritable-strings", 
                              "-c -fwritable-strings"))
 else if (compiler.env = "ntv")
    (*fs* := "\\",
     // compiler options: optimized, debug, default
      compiler.options := list("-c /O2", 
                               "-c /Zi", 
                               "-c")),
 compiler.libraries_dir := list(ROOT / "bin" / "public" / compiler.env,
                                ROOT / "bin" / "debug" / compiler.env,
                                ROOT / "bin" / "public" / compiler.env),
 verbose() := 2,              // not significant
 compiler.safety := 5,        // do not change
 compiler.headers_dir := ROOT / "include",
 compiler.source := ROOT / "csrc")

// ---------------------------------------------------------------


// when changing to a platform that requires a new makefile generator,
// one cannot use the existing old file to compile the new one. The following
// method may be used to cross-compile the new (revised) file once it is
// loaded
// THIS METHOD IS RESERVED FOR EXPERT USE ...
[ccmain() : void
  -> compiler.active? := true,
     begin(claire),
     Generate/*cf* := "ccmain",
     compiler.headers_dir := home() / "include",
     claire_lib := home() / "bin" / "debug" / compiler.env,
     compile("ccmain"),
     cmakefile("ccmain","cclaire") ]
       
(printf("init.cl is ok -------------------------------------------------\n"))




