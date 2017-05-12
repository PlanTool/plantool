typedef union {
  int integer;
  char *ident;
  struct idlist_s *idlist;
  struct predicate_s *predicate;
  struct operator_s *operator;
} YYSTYPE_T;
