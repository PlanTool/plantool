/*
**
** Universidad Simon Bolivar, 1999, 2000 (c)
** Blai Bonet and Hector Geffner. 1999, 2000.
**
*/

struct idList_s
{
  int id;
  char *name;
  struct idList_s *type;
  struct idList_s *next;
} idlist_s;
typedef struct idList_s idList_t;


struct atomicFormula_s
{
  int id;
  int neg;
  int equal;
  char *name;
  idList_t *args;
} atomicFormula_s;
typedef struct atomicFormula_s atomicFormula_t;

#define ATOMIC_FORMULA       1
#define WHEN_FORMULA         2
#define FORALL_FORMULA       3
#define FORALL_GOAL_FORMULA  4

struct formula_s
{
  int type;
  union
  {
    atomicFormula_t *atomic;
    struct 
    {
      struct formula_s *condition;
      struct formula_s *addEffect;
      struct formula_s *delEffect;
    } when;
    struct
    {
      idList_t *vars;
      struct formula_s *addEffect;
      struct formula_s *delEffect;
      struct formula_s *whenEffect;
    } forall;
    struct
    {
      idList_t *vars;
      struct formula_s *formula;
    } forallGoal;
  } u;
  struct formula_s *next;
} formula_s;
typedef struct formula_s formula_t;


struct schema_s
{
  int id;
  char *name;
  idList_t *vars;
  formula_t *prec;
  formula_t *add;
  formula_t *del;
  formula_t *when;
  formula_t *forall;
  struct schema_s *next;
} schema_s;
typedef struct schema_s schema_t;


struct body_s
{
  int type;
  union
  {
    idList_t *parameters;
    void *precondition;
    void *effect;
  } data;
  struct body_s *next;
} body_s;
typedef struct body_s body_t;


struct instantiation_s
{
  idList_t *var, *val;
  struct instantiation_s *next;
} instantiation_s;
typedef struct instantiation_s instantiation_t;


typedef union
{
  int                integer;
  char *             ident;
  idList_t *         idlist;
  formula_t *        formula;
  atomicFormula_t *  atomicFormula;
  schema_t *         schema;
  body_t *           body;
} YYSTYPE_T;


#define YYSTYPE  YYSTYPE_T
#define YYSTYPE_IS_DECLARED 1
