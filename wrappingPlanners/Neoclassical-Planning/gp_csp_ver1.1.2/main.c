
#include "graphplan.h"
#include "utilities.h"

#ifndef FALSE
#define FALSE 0
#endif

/* Tracing messages go to STDMSG */
#define STDMSG stdout
/* wffs, solutions, models go to STDDATA */
#define STDDATA stdout
int yyparse();
extern FILE *yyin;

int input_type;
char *domain_name, *problem_name;
int max_time;
op_list ops;
fact_list initial_facts, the_goals, constants, predicates, the_types;
token_list objects;

main(int argc, char *argv[])
{
    int i;
    char opfile[100], factfile[100];
    opfile[0] = factfile[0] = '\0';

    if (argc <= 1) {
	printf("Expecting domain and problem file names.\n");
	exit(1);
    }

    for(i = 1; i < argc; ++i) {
	if (argv[i][0] != '-') continue;

	if (argv[i][1] == 'o') strcpy(opfile, argv[i+1]);
	else if (argv[i][1] == 'f') strcpy(factfile,argv[i+1]);
    }

    if(opfile[0] == '\0' || factfile[0] == '\0') {
	printf("Both the domain and problem file should be given.\n");
	exit(1);
    }

    if ((yyin = fopen(opfile,"r")) == NULL)
	do_error("cannot load operator file");
    input_type = DOMAIN_INPUT;
    fprintf(STDMSG, "Loading domain file: %s\n", opfile);
    yyparse();
    
    if ((yyin = fopen(factfile,"r")) == NULL)
	do_error("cannot load facts file");
    input_type = PROBLEM_INPUT;
    fprintf(STDMSG, "Loading fact file: %s\n", factfile);
    yyparse();
    
    printf(" \n ** Parsing completed. Dumping output: ** \n\n");
    fprintf(STDMSG, "Problem name: %s\n", problem_name);
    fprintf (STDMSG, "** Operators: \n");
     print_actions(ops);
    fprintf (STDMSG, "** Types: \n");
    print_fact_list (the_types);
    fprintf (STDMSG, "\n");
    fprintf (STDMSG, "** Init: \n");
    print_fact_list (initial_facts);
    fprintf (STDMSG, "\n");      
    fprintf (STDMSG, "** Goal: \n");
    print_fact_list (the_goals);
    fprintf (STDMSG, "\n");
}

/****** newly added ******/
hashtable_t *fact_table, *op_table;  /* the arrays of hash tables */

int really_is_var(char *str)
{
  return 1;
  /* return (str[strlen(str)-1] == '?'); */
}

/****have instantiation list associated to op.  Set all the var-parts.*****/
/** Changing this to allow the parameters to have some variables mentioned
 ** more than once.  Just makes a list of all of the different ones.
 ** This goes in reverse order. (Gets re-reversed when op-string is created)
 **/
void set_insts(op_ptr op)
{
  char junk[100];
  param_list pptr;
  token_list tptr;
  instantiation_list iptr;
  op->insts = NULL;
  for(pptr = op->params; pptr != NULL; pptr = pptr->next) {
    for(tptr = pptr->item; tptr; tptr = tptr->next) {
      if (!is_var(tptr->item)) continue;
      if (!really_is_var(tptr->item)) {
        sprintf(junk,"Badly-formed op: %s",op->name);
        do_error(junk);
      }
      /* it's a variable.  Check to make sure it's a new one */
      for(iptr = op->insts; iptr; iptr = iptr->next) {
        if (strcmp(iptr->var_part, tptr->item) == SAME) break;
      }
      if (iptr != NULL) continue;  /* did break above: not new */

      op->insts = insert_inst(tptr->item, NULL, op->insts);
    }
  }
}

/***given a string, makes a new one with NOOP in front***
  Returns new storage.
*/
char *make_noop_string(char *str)
{
  char *newp;
  newp = (char *) calloc(strlen(str) + strlen(NOOP) + 2, sizeof(char));
  sprintf(newp,"%s_%s",NOOP,str);
  return newp;
}

edgelist_t insert_edge(edgelist_t e, vertex_t v)
{
  edgelist_t newedge = (edgelist_t) calloc(1,sizeof(edgelist_s));
  if(!newedge)
    do_error("not enough memory");
  newedge->next = e;
  newedge->endpt = v;
  return newedge;
}

void delete_edge(edgelist_t e)
{
  edgelist_t tmp;
  
  if(e) {
      tmp=e->next;
      delete e;
      delete_edge(tmp);
  }
}

/* like above, but insert at end of list */
edgelist_t insert_edge_at_end(edgelist_t e, vertex_t v)
{
  edgelist_t newedge;
  if (e == NULL) {
    newedge = (edgelist_t) calloc(1,sizeof(edgelist_s));
    if(!newedge)
      do_error("not enough memory");
    newedge->endpt = v;
    return newedge;
  } else {
    e->next = insert_edge_at_end(e->next, v);
    return e;
  }
}

void completely_free_fact_list(fact_list l)
{
  fact_list temp;
  while(l) { temp = l->next; free_token_list(l->item); free(l); l = temp;}
}

void free_token_list(token_list l)
{
  token_list temp;
  while(l) {   temp = l->next; free(l->item); free(l); l = temp;  }
}

token_list token_list_from_string(char str[])
{
  char *p = str, *q, temp[50];
  token_list_elt dummy, *current;
  current = &dummy;
  while (*p) {
    current->next = (token_list) malloc(sizeof(token_list_elt));
    current = current->next;
    for(q = temp; (*p != '\0') && (*p != CONNECTOR); *q++ = *p++);
    if (*p == CONNECTOR) ++p;
    *q = '\0';
    current->item = (char *) calloc(1 + strlen(temp),sizeof(char));
    strcpy(current->item, temp);
  }
  current->next = NULL;
  return( dummy.next );
}



/* insert into instantiation list */
instantiation_list insert_inst(char *v, char *c, instantiation_list i)
{
  instantiation_list temp=(instantiation_list) malloc(sizeof(instantiation_s));
  if(!temp)
    do_error("not enough memory");
  temp->var_part = v;
  temp->const_part = c;
  temp->next = i;
  return( temp );
}


/* Returns instantiate token list in a string (last argument) Uses
   CONNECTOR to connect the tokens together. 
   if "failflag" is 0 then Return 1 if ok, 0 if failure.
   Otherwise, exit on failure.
   */
int instantiate_into_string(token_list l, instantiation_list inst,char str[],
			    int failflag)
{
  instantiation_list i;
  char *p;
  token_list t = l;

  for(p = str; t; t = t->next) {
    if (is_const(t->item))  /* just copy over */
      { strcpy(p,t->item); p+=strlen(t->item); *p++ = CONNECTOR; }
    else {
      for(i=inst; i; i = i->next)
        if (strcmp(i->var_part, t->item) == SAME) {
	  if (i->const_part == NULL) {i = NULL; break;}
          strcpy(p,i->const_part);p+=strlen(i->const_part);*p++ = CONNECTOR;
          break;
        }
      if (!i) {
	if (failflag) do_error("instantiation failed");
	return 0;
      }
    }
  }
  *(--p) = '\0';
  return 1;
}

/* "whitespace" is blank or paren */
int is_stopper(int c)
{
  return (isspace(c) || (c == ')') || (c == '('));
}

/* read the next "item".  An item is either a string of non-parentesis,
   non-blank characters, or a parenthesis.  In the former case, put into
   str and return OK. In the latter case, return LEFT_PAREN or RIGHT_PAREN
   appropriately.  Return EOF on end-of-file.

   if character is CONNECTOR, change it to REPLACEMENT
 */
#define REPLACEMENT '.'
int read_item(FILE *fp, char *str)
{
  char *s = str;
  int letter, var_flag;
  /* first read in any blankspace and check for EOF */
  while(isspace(letter = getc(fp)));
  if (letter == EOF) return EOF;

  /* check if parenthesis */
  if (letter == '(') return LEFT_PAREN;
  if (letter == ')') return RIGHT_PAREN;
  /*  if (letter == ':') return SEMI_COLON; */
  if (letter == ';') {
    while (getc(fp) != '\n');
    return read_item(fp, str);
  }
  var_flag = 0;
  if (letter == '?') {
    var_flag = 1;
    letter = '<';
  }
  ungetc((char) letter, fp);
  while(!is_stopper(*s++ = tolower(getc(fp))))    /* read word in */
    if (*(s-1) == CONNECTOR) *(s-1) = REPLACEMENT;  /* change '_'to'.'*/
  ungetc(*--s, fp);                      /* went one too far */
  if (var_flag)
    *s++ = '>';
  *s = '\0';                             /* end it nicely */
  if (strcmp(str, "not") == 0) {	 /* "not" -> "del" */
    strcpy(str, "del");
  }
  if (strcmp(str, "and") == 0) {	 /* handle "and" */
    return CAND;
  }
  return OK;
}

/* string to describe an instantiated operator: re-reverse order */
void rec_make_op_string(op_ptr op,instantiation_list insts, char *str)
{
  if (insts == NULL) return;
  rec_make_op_string(op, insts->next, str);
  if (insts->const_part == NULL) 
    {fprintf(STDMSG,"op %s, ",op->name); do_error("bad instantiation");}
  sprintf(str + strlen(str), "_%s", insts->const_part);
}
/* string to describe an instantiated operator */
void make_op_string(op_ptr op, char *str)
{
  sprintf(str, "%s",op->name);
  rec_make_op_string(op, op->insts, str + strlen(str));
}

void do_error(char *s)
{
  fprintf(STDMSG,"%s\n",s); exit( 1 );
}

int equal_facts(token_list f1, token_list f2)
{
  for(; f1 && f2; f1 = f1->next, f2 = f2->next)
    if (!equal_tokens(f1->item,f2->item)) return 0;  /* different tokens*/
  if (f1 || f2) return 0;  /* different lengths */
  return 1;
}

/************ ROUTINES FOR PRINTING OUT INFORMATION TO USER**************/
void print_info_piece(hashtable_t t, int flag)
{
}

/* for printing out info to user */
void print_info(int len)
{
  int i;
fprintf(STDMSG, "Printing graph. For clarity, ignoring parts unreachable by planner\n");

  for(i=0; i < len; ++i) {
    fprintf(STDMSG, "\nTime: %d\n",i+1);
  }
}


/************ more random utilities ***************/
void read_initial_comments(FILE *fp)
{
  int c;
  while((c = getc(fp)) != '(');  /* read up to first left paren */
}

int allocs_in_use;

void my_free(void * p) {
    allocs_in_use--;
    free(p);
}
void * my_alloc(int size) {
    void * p = malloc(size);
    allocs_in_use++;
    if (p == NULL) {
        fprintf(STDMSG, "Ran out of space.  Requested size=%d.\n", size);
        exit(1);
    }
    return p;
}

void print_alloc(void)
{ fprintf(STDMSG, "allocs - frees = %d\n",allocs_in_use); }

/*
void yyerror(char *x)
{
	fprintf(STDMSG, "%s\n",x);
}
*/

fact_list fact_list_append(fact_list f1, fact_list f2)
{
  fact_list f;
  
  if (f1 == NULL) return f2;
  for (f = f1; f->next; f = f->next);
  f->next = f2;
  return f1;
}


token_list token_list_append(token_list f1, token_list f2)
{
	if (f2 == NULL) return f1;

	if (f1 == NULL) return f2;

	f1->next = token_list_append(f1->next, f2);

	return f1;
}

/* simply printout no plan if necessary */
void print_noplan (void)
{
}

void no_solution(char * msg, int time)
{
    fprintf(STDMSG, "\n----------------------------------------------------\n");    
    fprintf(STDMSG, "NO SOLUTION\n");
    fprintf(STDMSG, "Problem not solvable: %s\n", msg);
    fprintf(STDMSG, "----------------------------------------------------\n");    
    print_noplan();
}

/*
 * routines added for blackbox 
 */

/* lookup the constant part in the instantiation */
char* lookup_instantiation (instantiation_list insts, char *name)
{
  for (; insts; insts = insts->next) {
    if (strcmp(insts->var_part, name) == 0)
      return insts->const_part;
  }
  return NULL;
}

/* free instantiation list; assume var_part & const_part are shared */
void free_instantiation (instantiation_list insts)
{
  instantiation_list temp;

  while(insts) {
    temp = insts->next;
    free(insts);
    insts = temp;
  }
}

/* make a fact from a token */
fact_list token2fact (token_list tlist)
{
  fact_list f;
  f = (fact_list) malloc(sizeof(fact_list_elt));
  f->item = tlist;
  f->next = NULL;
  return f;
}

/* make a token from a string */
token_list str2token (char *str)
{
  token_list tlist;
  
  tlist = (token_list)malloc(sizeof(token_list_elt));
  tlist->item = str;
  tlist->next = NULL;
  return tlist;
}

/* duplicate a string and from a token */
token_list strdup2token (char *str)
{
  token_list tlist;
  
  tlist = (token_list)malloc(sizeof(token_list_elt));
  tlist->item = bbstrdup(str);
  tlist->next = NULL;
  return tlist;
}

/* duplicate a token list */
token_list dup_token_list (token_list tlist)
{
  token_list thead, tcur, tnew;
  
  thead = tcur = NULL;
  while (tlist != NULL) {
    tnew = strdup2token(tlist->item);
    if (thead == NULL)
      thead = tcur= tnew;
    else {
      tcur->next = tnew;
      tcur = tnew;
    }
    tlist = tlist->next;
  }
  return thead;
}

/* print token list */
void print_token_list (token_list t)
{
  fprintf (STDMSG, "( ");
  while (t != NULL) {
    fprintf (STDMSG, "%s ", t->item);
   t = t->next; 
  }
  fputc (')', STDMSG);
}

/* print fact list */
void print_fact_list (fact_list f)
{
  while (f != NULL) {
    print_token_list(f->item);
    f = f->next;
  }
  fprintf (STDMSG, "\n");
}

/* print acitons for debugging */
void print_actions (op_list op)
{
  while (op != NULL) {
    fprintf (STDMSG, "action %s:\n", op->name);
    fprintf (STDMSG, "parameters => ");
    print_fact_list (op->params);
    fprintf (STDMSG, "precondsions => ");
    print_fact_list (op->preconds);
    fprintf (STDMSG,"effects => ");
    print_fact_list (op->effects);
    fprintf (STDMSG, "\n");
    op = op->next;
  }
}

/* same as strdup, except all characters are converted to lower case */
char* bbstrdup (char* s1)
{
  char *str, *p;  

  str = p = strdup(s1);
  while (*p) 
    *p++ = tolower(*p);
  return str;
}
