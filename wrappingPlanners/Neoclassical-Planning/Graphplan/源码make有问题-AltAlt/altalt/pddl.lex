char [a-zA-Z0-9_-]
string {char}+
whitespace [ \t]+
nl \n
comment ;.*$


%%
"(" {return (OPEN_BRAC);}
")" {return (CLOSE_BRAC);}
":requirements" {return (REQS);}
":equality" {return (EQ);}
":EQUALITY" {return (EQ);}
":strips" {return (STRIPS);}
":constants" {return (CONSTANTS);}
":predicates" {return (PREDS);}
"define" {return (DEFINE);}
"domain" {return (DOMAIN);}
":timeless" {return (STATICS);}
":action" {return (ACTION);}
":parameters" {return (ARGS);}
":precondition" {return (PRE);}
":effect" {return (EFFECTS);}
"and" {return (AND);}
"not" {return (DEL);}
"situation" {return (SITUATION);}
"problem" {return (PROBLEM);}
":vars" {return (VARS);}
":VARS" {return (VARS);}
":domain" {return (FORDOMAIN);}
":situation" {return (SITUATION);}
":objects" {return (CONSTANTS);}
":init" {return (INITIALLY);}
":goal" {return (GOALS);}
":REQUIREMENTS" {return (REQS);}
":STRIPS" {return (STRIPS);}
":CONSTANTS" {return (CONSTANTS);}
":PREDICATES" {return (PREDS);}
"DEFINE" {return (DEFINE);}
"DOMAIN" {return (DOMAIN);}
":TIMELESS" {return (STATICS);}
":ACTION" {return (ACTION);}
":PARAMETERS" {return (ARGS);}
":PRECONDITION" {return (PRE);}
":EFFECT" {return (EFFECTS);}
"AND" {return (AND);}
"NOT" {return (DEL);}
"SITUATION" {return (SITUATION);}
"PROBLEM" {return (PROBLEM);}
":DOMAIN" {return (FORDOMAIN);}
":SITUATION" {return (SITUATION);}
":OBJECTS" {return (CONSTANTS);}
":INIT" {return (INITIALLY);}
":GOAL" {return (GOALS);}
"=" {return (EQ);}
":length" {return (LENGTH);}
":LENGTH" {return (LENGTH);}
":serial" {return (SERIAL);}
":SERIAL" {return (SERIAL);}
":parallel" {return (PARALLEL);}
":PARALLEL" {return (PARALLEL);}

"?" {return (Q);}

{string} {int i; yylval.cp = malloc(strlen(yytext)+1);
			strcpy(yylval.cp,yytext);
			for(i = 0;i<strlen(yylval.cp);i++)
				yylval.cp[i] = tolower(yylval.cp[i]);
			return (NAME);}
{whitespace} ;
{comment} ;
{nl} {lineno++;};
