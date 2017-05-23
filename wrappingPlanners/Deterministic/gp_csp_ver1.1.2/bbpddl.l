%{
/*****************************************************************/ 
/*  Strict Strips PDDL lexical analyzer for BlackBox             */
/*****************************************************************/
#include "graphplan.h"
#include "bbpddl.tab.h"

%}

ID [a-zA-Z0-9_=-]+ 
VAR "?"{ID}
comment ;.*$

%option caseless
%option yylineno

%%
"(" {return '(';}
")" {return ')';}
"define" { return DEFINE; }
"domain" { return DOMAIN; }
"requirements" { return REQUIREMENTS; }
"constants" { return CONSTANTS; }
"types" { return TYPES; }
"predicates" { return PREDICATES; }
"action" { return ACTION; }
"parameters" { return PARAMETERS; }
"precondition" { return PRECONDITION; }
"effect" { return EFFECT; }
"and" { return AND; }
"not" { return NOT; }
"problem" { return PROBLEM; }
"objects" { return OBJECTS; }
"init" { return INIT; }
"goal" { return GOAL; }
"length" { return (LENGTH);}
"serial" { return (SERIAL);}
"parallel" {return (PARALLEL);}
"exists" { return EXISTS; }
"either" { return EITHER; }
"fluent" { return FLUENT; }

"set" { return SET; }
"test" { return TEST; }
"influence" { return INFLUENCE; }

"-" { return '-'; }
":" { return ':'; }

{ID} { return ID; }
{VAR} {return VAR; }

[ \t]+ ;
\n ;
{comment} ;

%%

