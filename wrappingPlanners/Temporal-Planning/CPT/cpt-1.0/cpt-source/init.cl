(system.verbose := 0,
 compiler.safety := 5)

choco_files :: list("chocutils","model","dom","prop","var","const","search","chocapi","compil")
cptlib_files :: list("rationals", "utils", "structures", "parse")
cptshort_files :: list("static_conflicts", "precedence_constraints", "branching", "trace", "main")
cpt_files :: list("rationals", "utils", "structures", "parse", "static_conflicts", "precedence_constraints", "branching", "trace", "main")
chome :: getenv("CLAIRE3_HOME")
build :: getenv("CPT_BUILD")

(#if (build = "one") (
   cptall :: module(source = "source", uses = list(Core), made_of = choco_files /+ cpt_files)))

(#if (build != "one") (
   choco :: module(source = "source", uses = list(Core, Optimize), made_of = choco_files)))

(#if (build = "cptlib" | build = "cptshort") (
   cptlib :: module(source = "source", uses = list(choco), part_of = choco, made_of = cptlib_files)))

(#if (build = "cptshort") (
   cptshort :: module(source = "source", uses = list(cptlib), part_of = cptlib, made_of = cptshort_files)))

(#if (build = "cpt") (
   cpt :: module(source = "source", uses = list(choco), part_of = choco, made_of = cpt_files)))


(compiler.source := "csrc",
 compiler.libraries_dir := list(
   chome / "bin" / "public" / compiler.env,
   chome / "bin" / "debug" / compiler.env,
   chome / "bin" / "public" / compiler.env),
 compiler.headers_dir := chome / "include")
