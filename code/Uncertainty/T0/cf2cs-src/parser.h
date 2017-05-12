#ifndef __PARSER_h
#define __PARSER_h

#define ATOMIC_FORMULA       1
#define AND_FORMULA          2
#define OR_FORMULA           3
#define NOT_FORMULA          4
#define FORALL_FORMULA       5
#define FORALL_EFFECT        6
#define LIST_FORMULA         7
#define EMPTY_FORMULA        8
#define CERTAINTY_FORMULA    9
#define ADD_FORMULA         10
#define DEFINED_FORMULA     11
#define SET_FORMULA         12
#define WHEN_EFFECT         13
#define KNOW_FORMULA        14
#define UNKNOWN_FORMULA     15
#define ONEOF_FORMULA       16

class idList_t
{
public:
  int id;
  char *name;
  idList_t *type;
  idList_t *next;
};

class formula_t
{
public:
  int type;
  union
  {
    struct
    {
      int id;
      int neg;
      int equal;
      char *name;
      idList_t *args;
    } atomic;
    formula_t *formula;
    struct
    {
      idList_t *vars;
      formula_t *formula;
    } lambda;
    struct
    {
      formula_t *formula;
      formula_t *next;
    } flist;
    struct
    {
      formula_t *literal;
      formula_t *formula;
    } fset;
    struct
    {
      formula_t *formula;
      formula_t *effect;
    } when;
  } u;
};

class schema_t
{
public:
  bool is_observation;
  int id;
  int cost;
  char *name;
  idList_t *vars;
  formula_t *prec;
  formula_t *effect;
  formula_t *obs;
  schema_t *next;
};

class body_t
{
public:
  int type;
  union
  {
    int cost;
    idList_t *parameters;
    formula_t *formula;
  } u;
  body_t *next;
};

class instantiation_t
{
public:
  idList_t *var, *val;
  instantiation_t *next;
};

struct ltstr
{
  bool operator()(const char* s1, const char* s2) const { return( strcasecmp( s1, s2 ) < 0 ); }
};

typedef union
{
  float              real;
  int                integer;
  char *             ident;
  idList_t *         idlist;
  formula_t *        formula;
  schema_t *         schema;
  body_t *           body;
} YYSTYPE_T;

#define YYSTYPE  YYSTYPE_T

#endif
