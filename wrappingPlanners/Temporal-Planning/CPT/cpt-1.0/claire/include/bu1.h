// interface defination for module bu1, Sun Nov 19 20:32:55 2000
#ifndef CLAIREH_bu1
#define CLAIREH_bu1


class GUI2;
class GUI;
class screen;
class C;

class GUI2: public thing{ 
  public:
     int x;
     int y;
     int w;} 
;

class GUI: public thing{ 
  public:
     int x;
     int y;
     int w;} 
;

class screen: public ClaireObject{ 
  public:
     int day;
     int time;} 
;

class C: public thing{ 
  public:
     list *st;} 
;
extern OID  bu1_check_string(char *tag,OID x);
extern OID  bu1_record_string(char *tag,OID x);
extern void  bu1_testOK_void();
extern OID  bu1_startTest_void();
extern OID  bu1_s_integer(int d,int t);class bu1Class: public NameSpace {
public:

global_variable * CompiledTest;
global_variable * ClaireVersion;
global_variable * TestPort;
global_variable * ModuleName;
ClaireClass * _GUI2;
ClaireClass * _GUI;
ClaireClass * _screen;
ClaireClass * _C;
C * c;
property * check;// bu1/"check"
property * record;// bu1/"record"
property * testOK;// bu1/"testOK"
property * startTest;// bu1/"startTest"
property * x;// bu1/"x"
property * y;// bu1/"y"
property * w;// bu1/"w"
property * day;// bu1/"day"
property * time;// bu1/"time"
property * s;// bu1/"s"
property * st;// bu1/"st"

// module definition 
 void metaLoad();};

extern bu1Class bu1;

#endif
