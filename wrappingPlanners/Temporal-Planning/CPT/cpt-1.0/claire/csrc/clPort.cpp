/***********************************************************************/
/**   microCLAIRE                                       Yves Caseau    */
/**   clPort.cpp                                                       */
/**  Copyright (C) 1998-2003 Yves Caseau. All Rights Reserved.         */
/**  cf claire.h                                                       */
/***********************************************************************/

#include <claire.h>
#include <Kernel.h>
#include <marie.h>
#ifdef CLWIN
#include <conio.h>
#endif


// this file contains the I/O objects and methods

/*********************************************************************/
/** Contents                                                         */
/**    1: Claire Ports                                               */
/**    3. Reader functions                                           */
/**    3. Character                                                  */
/**    4. System interface (miscellaneous)                           */
/*********************************************************************/

// new: use the bounds to print the float properly !
extern double CLMAXFLOAT;
extern double CLMINFLOAT;

// ***************************************************************************
// * PART 1: Claire Ports                                                    *
// ***************************************************************************

// local useful macro (v3.2.26) is a float close to nice decimal number
// v3.2.50 try to make it stricter
#define DECIMAL(x) ((x * 1e5 - floor(x * 1e5) < 1e-6) ||  (ceil(x * 1e5) - x * 1e5 < 1e-6))

// we use an internal hierarchy of classes (only valid in this file) to represent
// the various types of port (a link to the C or C++ library)
// this is totally extensible (we can introduce new kinds of ports)

// read/write into a file (may rely on C++ streams in the future)
class CPFile: public ClairePort {
    public:
    FILE *value;
    virtual char get()
     {return getc(value);};
    virtual void put(char c)
      {  putc(c, value);};
    virtual void put(int n) {fprintf(value,"%d",n);};
    virtual void put(double x) {fprintf(value,"%#0.20g",x);}          // v3.2.54 : regular
    virtual void prettyp(double x)                                    // v3.2.54 : pretty
     {double y = floor(x);
         if (x > CLMINFLOAT & x < CLMAXFLOAT & y == x) fprintf(value,"%#.1f",x); // v3.2.54 thanks to B Martin
         else if (y == 0.0 || ( (x - y) / y > 1e-5 && DECIMAL(x)))         // v3.2.54
             fprintf(value,"%g",x);               // easy print is OK
        else fprintf(value,"%#0.10g",x);}         // v3.2.24
    virtual void flush() {fflush(value);};
    virtual void pclose()
        {fclose(value);};
};

// reading from a string   -> to check for correctnedd
// we could use an istrstream if the documentation was readily available
class CPStringIn: public ClairePort {
    public:
    char *buffer;
    int index;
    virtual char get() {char c = buffer[index++];
                        if (c == '\0') return EOF; else return c;};
};

// output to a string : we implement our own for better control but it could change ...
class CPStringOut: public ClairePort {
   public:
    char *buffer;
    int index;
    virtual void put(char c) {
         buffer[index++] = c;
         if (index > MAXBUF) Cerror(16,0,0);};
    virtual void put(int n)
       { sprintf(&buffer[index],"%d",n);
         for ( ;buffer[index] != '\0'; index++) ;
         if (index > MAXBUF)  Cerror(16,0,0);};
    virtual void put(double x)
       {sprintf(&buffer[index],"%#0.20g",x);
        for ( ;buffer[index] != '\0'; index++) ;
        if (index > MAXBUF)  Cerror(16,0,0);};             // v3.2.26
    virtual void prettyp(double x)                         // v3.2.54   pretty-print
       {double y = floor(x);
         if (x > CLMINFLOAT & x < CLMAXFLOAT & y == x)
            sprintf(&buffer[index],"%#.1f",x);
         else if (y == 0.0 || ( (x - y) / y > 1e-5 && DECIMAL(x)))
             sprintf(&buffer[index],"%g",x);               // easy print is OK
        else sprintf(&buffer[index],"%#0.10g",x);          // v3.2.54
        for ( ;buffer[index] != '\0'; index++) ;
        if (index > MAXBUF)  Cerror(16,0,0);};             // v3.2.26
 };

/* interface to the GUI
class CPGUI: public ClairePort {
    public:
    virtual void put(char c) {CL_put(c);}
    virtual char get() {return CL_get();}
    virtual void flush() {Cl_flush();}
    virtual void put(int n) {CL_int(n);}
    virtual void put(double x) {Cl_float(x);}
    virtual void prettyp(double x) {Cl_float(x);}
}*/

// default definition
char ClairePort::get() {return EOF;}
void ClairePort::put(char c) {}
void ClairePort::put(int n) {}
void ClairePort::put(double x) {}
void ClairePort::prettyp(double x) {}
void ClairePort::flush() {}
void ClairePort::pclose() {}

// this is the buffered read mode,
int ClairePort::getNext()
  {firstc = (int) get();
   return firstc;}

// this is an internal version -> uses new !
ClairePort *ClairePort::make(FILE *x)
{CPFile *p = new CPFile();
  p->value = x;
  p->status = 1;
  p->firstc = ' ';
  return p;}

// -------------- API functions --------------------------------------------

CPFile *MyMark = NULL;

// declare a port to be used as an output
ClairePort *use_as_output_port(ClairePort *p)
{ MyMark = (CPFile *) p;
  if (p != ClEnv->cout)
     {ClairePort *x = ClEnv->cout;
      ClEnv->cout = p;
      return x;}
  else return p;}

// close a file
void fclose_port(ClairePort *p)
 {if (ClEnv->cout == p) ClEnv->cout = ClAlloc->stdOut;
  p->pclose();}

// could free the memory ... introduced in v3.2.40
void free_I_port(ClairePort *p)  {printf("// ---- We could free a port :-) \n");}

// write/read to a port
void putc_char(ClaireChar *c,ClairePort *p)  {p->put((char) (c->ascii));}

ClaireChar * getc_port(ClairePort *p)
  {int n = (int) p->get();
   return _char_(n); }  // v3.1.06   -> _char_ does a AND: EOF -> 511

// flush a port
void flush_port(ClairePort *p) {p->flush();}

// link with old-style files
ClairePort *fopen_string(char *s, char *m)
{CPFile *p = new CPFile(); // TRY (CPFile *) ClAlloc->makeStatic(sizeof(CPFile) / 4 + 2);
 FILE *f = fopen(s,m);
  if (MyMark == NULL) MyMark = p;
  if (f == NULL) Cerror(36,_string_(s),0);
  p->value = f;
  p->status = 1;
  p->firstc = 32;
  return p;}

// create a port (string stream)
// string ports are persistent system objects => the space is allocated by C++ and not
// returned.
ClairePort *port_I_void()
{CPStringOut *p = new CPStringOut(); // (CPStringOut *) ClAlloc->makeStatic(sizeof(CPStringOut) / 4 + 2);
  p->buffer = new char[MAXBUF];
  p->status = 2;
  p->index = 0;
  p->firstc = 32;
  return p;}

// create a port from an input string
ClairePort *port_I_string(char *s)
{CPStringIn *p =  new CPStringIn(); // TRY (CPStringIn *) ClAlloc->makeStatic(sizeof(CPStringIn) / 4 + 2);
  p->status = 3;
  p->buffer = s;
  p->index = 0;
  p->firstc = 32;
  return p;}

void pushback_port(ClairePort *p, int n) {p->firstc = n;}

// returns the string associated to the port
char *string_I_port(ClairePort *p)
{if (p->status == 2) {char *s = ((CPStringOut *) p)->buffer;
                     int i =  ((CPStringOut *) p)->index;
                     s[i] = '\0';
                     // printf("---- C: string!(port) -> %s ------------\n",s);
                     return copy_string(s);}
 else return "";}

// this function returns the number of chars in the buffer 
OID length_port(ClairePort *p)
{ if (p->status == 2) return ((CPStringOut *) p)->index;
  else return 0;}

// sets the buffer length to a certain number
// this is crucial to reuse a string port for multiple use !
void set_length_port(ClairePort *p, int m)
{ if (p->status == 2 && (m >= 0) && (m <=  ((CPStringOut *) p)->index ))
     ((CPStringOut *) p)->index = m;}


/*********************************************************************/
/**    2. Reader general functions                                   */
/*********************************************************************/


// check that a char is not a special char 
ClaireBoolean *alpha_char (ClaireChar *cx)
{char c = (char) cx->ascii;
 if       ( (c == EOF) || (c == ((char) 255)) || (c == '\n') ||
             (c == '}') || (c == ')')  || (c == ']') ||
             (c == '{') || (c == '(')  || (c == '[') ||
             (c == 9  ) || (c == ';')  || (c == '|') ||
             (c == ',') || (c == '/')  || (c == ':') ||
             (c == '@') || (c == '.')  || (c == '\r') ||
             (c == '<') || (c == '>')  || (c == ' ') )
           //  (c == '+') || (c == '-')  || (c == '*') )
 return CTRUE;
 else return CFALSE;}


// reading a string in a port - assumes that " was read and that the string will end with "
char *read_string_port(ClairePort *p)
{char cur = p->firstc;
 ClEnv->bufferStart();
 while (cur != '\"')
    {if (cur == EOF) break;
     if (cur == '\\') {p->getNext();
                       cur = p->firstc;
                       if (cur == 't') cur = '\t';
                       else if (cur == 'n') cur = '\n';}
     ClEnv->pushChar(cur);
     cur = p->getNext();}
 p->getNext();
 return ClEnv->bufferCopy();}

// reading an ident, which is either a symbol, a number or a special case
OID read_ident_port(ClairePort *p)
{int cur = p->firstc;
 p->getNext();
 if ((cur == '-') && (('0' <= p->firstc) && ('9' >= p->firstc)))
    {OID value = read_number_port(p);
      if (INTEGERP(value)) return (- value);
      else return _float_( -(float_v(value))); }
 else if (cur == '\'')
   { cur = p->firstc;
     p->getNext();
     if ('\'' != p->firstc) {Cerror(35,cur,p->firstc); return 1;}
     else {p->getNext();
           return _oid_(char_I_integer(cur));}}
 else return  read_thing_port(p,ClEnv->module_I,char_I_integer(cur),ClEnv->module_I); }


// read a number, either a float or an integer
// changed in v3.0.70 to read long floats
OID read_number_port(ClairePort *p)
{double res = (double) (p->firstc - '0');
 p->getNext();
 while ((p->firstc >= '0') && (p->firstc <= '9'))
     {res = (res * 10.0) + (double) (p->firstc - '0');
      p->getNext();}
 if ((p->firstc != '.') && (p->firstc != 'e'))
    {if (res >= CLMINFLOAT && res <= CLMAXFLOAT) return ((int) res);     // rean an int
     else return _float_(res);}                                         // overflow -> float (v3.0.70)
 else {double possible = res;                  // read a float (saw a e or a .)
         if (p->firstc == '.')                 // read the decimal part
           {res = 10.0;
            p->getNext();
            while ((p->firstc >= '0') && (p->firstc <= '9'))
               {possible = possible + (((double) (p->firstc - '0')) / res);
                res = res * 10.0;
                p->getNext();}}
         if (p->firstc == 'e')                 // read the exponent part
            {char signe = '+';
             res = 0.0;
             p->getNext();
             if (p->firstc == '-') {signe = '-'; p->getNext();}
             if (p->firstc == '+') p->getNext();
             while ((p->firstc >= '0') && (p->firstc <= '9'))
                {res = (res * 10.0) + (double) (p->firstc - '0');
                 p->getNext();}
             if (signe == '-') possible = possible / pow(10.0,  res);
             else  possible = possible * pow(10.0,  res);}
          return _float_(possible);}
}

// reading a true identifier (symbol or object)
//   app is the module in which the stream is read, cur is the current
//   character and n is the reader object
// def = NULL means that we read a private name
OID read_thing_port(ClairePort *p, module *app, ClaireChar *cx, module *def)
{char cur = (char) cx->ascii;
 if (cur == '"')
    return _oid_(symbol::make(read_string_port(p),app,def));      // strings
 if ((cur == '/') && (p->firstc == '*'))                                           // C-style comments
    {while ((cur != '*') || (p->firstc != '/')) {cur = p->firstc; p->getNext();}
     p->getNext();
     return _string_("");}
 ClEnv->bufferStart();
 if ((cur == '/') && (p->firstc == '/'))                                           // C++ comment
    {p->getNext();
     while (((cur = p->firstc) != '\n') && (cur != EOF))
         {ClEnv->pushChar(cur); p->getNext();}
     ClEnv->pushChar('\0');
     return _string_(ClEnv->bufferCopy());}
 ClEnv->pushChar(cur);
 if ((cur == ':') && (p->firstc == ':')) {ClEnv->pushChar(cur);p->getNext();}       // :: trap
 else if ((cur == '.') && (p->firstc == '.')) {ClEnv->pushChar(cur);p->getNext();}       // .. trap
 else if ((cur == '<') && (p->firstc == '<')) {ClEnv->pushChar(cur);p->getNext();}       // .. trap
 else if ((cur == '-' || cur == '=' || cur == '>') && (p->firstc == '>'))
       {ClEnv->pushChar('>');p->getNext();}       // -> trap for *>
 cur = p->firstc;
 while (alpha_char(char_I_integer(cur)) == CFALSE)
       {ClEnv->pushChar((char) cur); cur = p->getNext();}
 ClEnv->pushChar('\0');
 if (cur == '/')                                                                   // read a qualified ident
    {OID s = (app->getSymbol(ClEnv->buffer,claire.it))->getValue();
     ClaireChar *cx = char_I_integer(p->getNext());
       p->getNext();
       if (s == _oid_(Kernel.PRIVATE)) return read_thing_port(p,app,cx,NULL);
       else {if (OWNER(s) !=  Kernel._module)
                Cerror(29,s,0);
             return read_thing_port(p,OBJECT(module,s),cx,def);}}
 else {symbol *s = app->getSymbol(ClEnv->buffer,def);                               // create the symbol
         if ((app == ClEnv->module_I) || (s->definition != NULL))
           return s->getValue();
         else {Cerror(30,_oid_(s),0); return 1;}}}

/**********************************************************************/
/**    3. Characters                                                  */
/**********************************************************************/

// initialization: create all chars (this is called once ClaireChar is properly defined)
void ClaireChar::init()
{int i;
  for (i = 0; i < 512; i++)
    {ClaireChar *c = (ClaireChar *) ClAlloc->makeAny(4);
       c->isa = Kernel._char;
       #ifdef CLWIN
       c->ascii = ((i <= 256) ? i : -(512 - i));     // v3.2.44  encoding on (-255 256)
       #else
       c->ascii = i;
       #endif
       ClRes->ascii[i] = c;
       }}

// princ a char / int / float
void princ_char(ClaireChar *cx) {ClEnv->cout->put((char) cx->ascii);}

// special consversion (language dependent)
void c_princ_c(ClaireChar *cx)
{ char c = (char) cx->ascii;
  switch (c) {
       case '.':  princ_string("_dot");break;
       case '/':  princ_string("_7");break;
       case '\\': princ_string("_backslash");break;
       case '&':  princ_string("_and");break;
       case '-':  princ_string("_dash");break;
       case '+':  princ_string("_plus");break;
       case '%':  princ_string("_Z");break;
       case '*':  princ_string("_star");break;
       case '?':  princ_string("_ask");break;
       case '!':  princ_string("_I");break;
       case '<':  princ_string("_inf");break;
       case '>':  princ_string("_sup");break;
       case '=':  princ_string("_equal");break;
       case ',':  princ_string("_comma");break;
       case '^':  princ_string("_exp");break;
       case '@':  princ_string("_at");break;
       case '~':  princ_string("_tilda");break;
       case ']':  princ_string("_brack");break;
       case ':':  princ_string("L_");break;
       case '\'': princ_string("_prime");break;
       case '$': princ_string("_dollar");break;       // v3.2.14
       case '²': princ_string("_two");break;
       default: ClEnv->cout->put(c);break;
       }}

// print the name of an object as a C identifier
void c_princ_string(char *ss)
{ int i;
  for (i=0; ss[i] != '\0'; i++) c_princ_c(char_I_integer(ss[i]));}

// print a symbol (the name of an object) as a C identifier */
void c_princ_symbol(symbol *s)
{  if (s->module_I != claire.it)
      {c_princ_symbol(s->module_I->name);
       ClEnv->cout->put('_');}
   c_princ_string(s->name);}

int integer_I_char(ClaireChar *c)
{return c->ascii;}

/*********************************************************************/
/**    4. System interface (miscellaneous)                           */
/*********************************************************************/

// set the time counter
void time_set_void()
{ if (++ClEnv->tIndex > 9) Cerror(26,ClEnv->tIndex,0);
  msec(ClEnv->tStack[ClEnv->tIndex]);}

// shows the elaped time
int time_get_void()
{int now;
   msec(now);
   if (ClEnv->tIndex <= 0) Cerror(26,ClEnv->tIndex,0);
   return (now - ClEnv->tStack[ClEnv->tIndex--]);}

int time_read_void()
{int now;
   msec(now);
   if (ClEnv->tIndex <= 0) Cerror(26,ClEnv->tIndex,0);
   return (now - ClEnv->tStack[ClEnv->tIndex]);}

void time_show_void()
{OID now;
   msec(now);
   if (ClEnv->tIndex <= 0) Cerror(26,ClEnv->tIndex,0);
   princ_string("Counter["); princ_integer(ClEnv->tIndex);
   princ_string("] Elapsed time: "); princ_integer(now - ClEnv->tStack[ClEnv->tIndex--]);
   princ_string("ms. \n");}

// pass a command string to the host system
void claire_shell(char *s)
{
#ifdef CLDEBUG
 ClEnv->verbose = 1;
 ClEnv->abortOnError = 1;
#endif
 CL_system(s); }


// profiler methods (ClaireProfile -> KernelProfile)
PRcount *PRstart(PRcount *p)
{int x = 0;
  if (p->rdepth == 0) {p->rstart = clock();}  // v3.2.58 : more precision ....
  p->rdepth++; p->rnum++;
  return p;}

// fixed bugs in v3.0.53 thanks to FXJ
void PRend(PRcount *p)
{int x = 0;
  p->rdepth--;
  if (p->rdepth == 0) {p->rtime += (clock() - p->rstart);}}

void PRloop(PRcount *p)
{p->rloop++;}


