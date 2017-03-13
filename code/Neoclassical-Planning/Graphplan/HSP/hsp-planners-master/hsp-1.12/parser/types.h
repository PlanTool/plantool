struct idlist_s {
  int id;
  char *name;
  struct idlist_s *type;
  struct idlist_s *next;
} idlist_s;

struct aformula_s {
  int id;
  int neg;
  int type;
  int equal;
  char *func;
  struct idlist_s *args;
  struct idlist_s *domain;
  struct aformula_s *next;
} aformula_s;

struct action_s {
  int id;
  char *name;
  struct idlist_s *vars;
  void *prec;
  void *add;
  void *del;
  struct action_s *next;
} operator_s;

struct body_s {
  int type;
  union {
    struct idlist_s *parameters;
    void *precondition;
    void *effect;
  } data;
  struct body_s *next;
} body_s;

struct instantiation_s {
  char *var, *val;
  struct instantiation_s *next;
} instantiation_s;

struct hashentry_s {
  char *name;
  int number;
  struct hashentry_s *next;
} hashentry_s;


typedef union {
  int integer;
  char *ident;
  struct idlist_s *idlist;
  struct aformula_s *aformula;
  struct operator_s *operator;
  struct body_s *body;
} YYSTYPE_T;

#define YYSTYPE  YYSTYPE_T
