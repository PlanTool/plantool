
/***********************************************************************/
/**   microCLAIRE                                       Yves Caseau    */
/**   clBag.cpp                                                        */
/**  Copyright (C) 1998-2003 Yves Caseau. All Rights Reserved.         */
/**  cf claire.h                                                       */
/***********************************************************************/

#include <claire.h>
#include <Kernel.h>
#include <marie.h>

// This file contains the definition of the bag, list, set and array classes.

// ***************************************************************************
// * Table of content:                                                       *
// * PART 1: Bags                                                            *
// * PART 2: Lists                                                           *
// * PART 3: Sets                                                            *
// * PART 4: Tuples                                                          *
// * PART 5: Arrays                                                          *
// ***************************************************************************


// ***************************************************************************
// * PART 1: Bags                                                            *
// ***************************************************************************

// member methods ------------------------------------------------------------

// read the i-th element of a list
/*inline OID &bag::operator[](int i)  {
#ifdef CLDEBUG
   if (ClEnv->verbose > 12) printf("BAG:~%x [%d] -> %x (%d)\n",this,i,content[i],getADR(content) + i);
   return ((i <= length) ? content[i] : (Cerror(41,_oid_(this),i), content[1]));
#else
    return this->content[i];
#endif
} */

#define hash_order(x) ClRes->hashOid(0xFFFFFFFF,x)


// internal static functions that operate on Cmemory
// sort using the hash code order
// should be made available in CLAIRE !
void bag::quickSort(OID *a, int n, int m)
{int x,y;
 if (m > n)                                     // sort memory zone from l + n to m
     {x = a[n]; y = hash_order(x);          // use x as pivot
      if (m == (n + 1)) {if (hash_order(a[m]) < y)                // two pieces
                            {a[n] = a[m]; a[m] = x;}} // swap
      else {int q = n, p = (m + n) >> 1;        // new in v3.0.20
            x = a[p]; y = hash_order(x);
            if (p != n) a[p] = a[n];            // move pivot to first place
            for (p = n + 1; p <= m; p++)        // sort around pivot
              {if (hash_order(a[p]) < y)
                  {a[q++] = a[p];
                   if (p > q) a[p] = a[q];}}
            a[q] = x;
            quickSort(a,n,q-1);
            quickSort(a,q+1,m);}}}


// remove duplicates and return cardinal (assumed that a[] was sorted)
// note that equal(x,y) => hash(x) = hash(y)
// at any point a[1 .. j] is cleaned and we have examined a[1 .. i]
// u is the index of the last strict increase in hash_order
int bag::quickClean(OID *a, int m)
{int i = 1, j = 1, x = a[1], u = 1, y = hash_order(x);
  while (i + 1 <= m)
     {OID x2 = a[i + 1], y2 = hash_order(x2), keep = 1, k;       // do we keep x2 ?
      // printf("hash de %d -> %d [%d,%d]\n",x2,y2,x,y);
      if (y2 != y) u = j ;                                       // y2 != y => y2 > y => yes
      else if IDENTIFIED(x2)
         for (k = u; k <= j; k++) if (a[k] == x2) {keep = 0; break;}
      else 
         for (k = u; k <= j; k++) if (equal(a[k],x2) == CTRUE) {keep = 0; break;}
      // printf("look at %d in position %d -> keep = %d [%d]\n",x2,i + 1,keep,j);
      if (keep == 1) {if (j < i) a[j + 1] = x2; j++; x = x2; y = y2;}
      i++; }
  return j; }
  
  
// v3.2 new: create a list of same size with no type or a given type
list *bag::clone() 
 {return clone(Kernel.emptySet);}
 
list *bag::clone(ClaireType *t)
{  if (length == 0) return list::empty(t);        // v3.2.38 -> moved up thanks to FXJ
   list *obj = list::make();
   int i;
   OID *x = ClAlloc->makeContent(length);
   for (i = 1; i <= length; i++) x[i] = CNULL;    // for GC protection
   obj->content = x;
   obj->length = length;
   obj->of = t;
  if (ClAlloc->statusGC != 2) GC_PUSH(obj);                       // v3.2.20 : clone is protected too late ...
  return obj;}

// API functions ------------------------------------------------------------
// copy a bag - in v0.01, we cannot opy nil or {} we return a generic empty
bag *copy_bag(bag *l)
{int i;
 if (Kernel.nil == l) return list::empty();
 else if (l == Kernel.emptySet) return set::empty();
 else {
 bag *obj = (bag *) ClAlloc->makeAny(4);
 obj->isa = l->isa;
 obj->content = NULL;
 obj->of = l->of;                           // v3.1.08
 obj->length = l->length;
 if (ClAlloc->statusGC != 2)  GC_PUSH(obj);  // v3.2.30
 OID *x = ClAlloc->makeContent(l->length);
 // for (i = 1; i <= l->length ; i++) x[i] = (*l)[i];
 if (l->length) memcpy(x+1,l->content+1,sizeof(OID) * l->length);  //<sb> v3.3.33
 obj->content = x;
 return obj;}}

// new in v3.1.16: create an empty copy  
bag *empty_bag(bag *l)
{int i;
 bag *obj = (bag *) ClAlloc->makeAny(4);
 obj->isa = l->isa;
 obj->content = NULL;
 obj->of = l->of;                           // v3.1.08
 obj->length = 0;
 if (ClAlloc->statusGC != 2) GC_PUSH(obj);     // v3.2.30
 obj->content = ClAlloc->makeContent(l->length);
 obj->content[1] = 0;                       // v3.1.18 erase the NOTHING !
 return obj;}
  
// removes the first occurrence of val in l; works for bags !
// we could still do better with a set if desired ...
bag *delete_bag(bag *l, OID val)
{int i,j, m = l->length;
 OID *x = l->content;
 if ( m == 0 ) return(l);                   // v3.2.22
 if IDENTIFIED(val)
  for (i= 1;;)  {OID j = x[i++];
                  if (j == val) break;
                  if (i > m) return(l);}
  else for (i= 1;;) {OID j = x[i++];
                      if (equal(j,val) == CTRUE) break;
                      if (i > m) return(l);}
  for (j= i - 1; j < m; j++) x[j] = x[j + 1];
  l->length = m - 1;
  return l;}

// returns a true type
ClaireType * of_bag(bag *l)
{if (l->of == NULL) return Kernel._void; else return l->of;}

// used by the reader to change this information
bag * cast_I_bag(bag *l, ClaireType *x)  { l->of = x; return l;}

// ***************************************************************************
// * PART 2: Lists                                                           *
// ***************************************************************************

// constructors --------------------------------------------------------

// create a list skeleton  - every method uses this one and
// it performs the GC_PUSH
inline list *list::make()
{list *obj = (list *) ClAlloc->makeAny(4);
  if (ClAlloc->statusGC != 2)  GC_PUSH(obj);    // v3.2.30
  obj->isa = Kernel._list;
  obj->of = NULL;
  obj->content = NULL;                       // v3.2 make is the only one to use NULL ...
  return obj;}                               // ... and it MUST be replaced by something

// create an empty list
list *list::empty()
{list *obj = list::make();
 OID *x = ClAlloc->makeContent(1);
   obj->length = 0;
   obj->content = x;
   obj->of = Kernel.emptySet;                 // v3.2 emptySet => do not touch
   return obj;}

// create a typed empty list
list *list::empty(ClaireType *t)
{ClAlloc->currentNew = t;                              // v3.3.34: a method to avoid protecting the type
 list *obj = list::make();
 OID *x = ClAlloc->makeContent(1);
   obj->length = 0;
   obj->content = x;
   obj->of = t;
   return obj;}

// create a list with one element
list *list::make(OID val)
{list *obj = list::make();
 OID *x = ClAlloc->makeContent(1);
   x[1] = val;
   obj->length = 1;
   obj->content = x;
   return obj;}

// create a list with two members
list *list::make(OID val1, OID val2)
{list *obj = list::make();
 OID *x = ClAlloc->makeContent(2);
   x[1] = val1; x[2] = val2;
   obj->length = 2;
   obj->content = x;
   return obj;}

/* create a list with two members on the eval stack
list *list::makeStack(OID val1, OID val2)
{OID *adr = &(ClEnv->stack[ClEnv->index]);
 list *obj = (list *) adr;
   obj->isa = Kernel._list;
   obj->of = NULL;
   obj->length = 2;
   obj->content = &adr[4];
   (*obj)[0] = 4; (*obj)[1] = val1; (*obj)[2] = val2;
   ClEnv->index += 7;
   if (ClEnv->index >= ClAlloc->maxStack) Cerror(27,ClAlloc->index,0);
   return obj; }     */

// creates a list
list *list::alloc(int n,...)
{va_list ap;
 int i;
 list *obj = list::make();
 obj->of = Kernel.emptySet;                 // v3.2
 OID *x = ClAlloc->makeContent(n);
   va_start(ap,n);
   for (i = 1; i <= n; i++) x[i] = va_arg(ap, OID);
   obj->length = n;
   obj->content = x;
   va_end(ap);
   return obj;}

// creates a typed list
list *list::alloc(ClaireType *t, int n,...)
{va_list ap;
 int i;
 ClAlloc->currentNew = t;                     // v3.3.34: a method to avoid protecting the type
 list *obj = list::make();                    // recall that make() protects the result
 OID *x = ClAlloc->makeContent(n);
   obj->of = t;                               // moved so that t is protected with obj
   va_start(ap,n);
   for (i = 1; i <= n; i++) x[i] = va_arg(ap, OID);
   obj->length = n;
   obj->content = x;
   va_end(ap);
   ClAlloc->currentNew = NULL;                 // v3.3.38 close the use of currentNew
   return obj;}

// this is pure sugar but nice
list *list::domain(int n, ...)
{va_list ap;
 int i;
 list *obj = list::make();
 OID *x = ClAlloc->makeContent(n);
   va_start(ap,n);
   for (i = 1; i <= n; i++)
     {int z = va_arg(ap, int);
      x[i] = _oid_(((ClaireAny *) z));}
   obj->length = n;
   obj->content = x;
   obj->of = Kernel._type;
   va_end(ap);
   return obj;}


// member methods ------------------------------------------------------

// add a new element to a list (without checking the type)
list *list::addFast(OID x)
{int i, m = length;
 if (m + 1 == (*this)[0])    // the memory zone is full
    {OID *y = ClAlloc->makeContent(m + 1);
        if (length) memcpy(y+1, content+1, sizeof(OID) * length); //<sb> v3.3.33
        content = y;}
 length = m + 1;
 (*this)[m + 1] = x;        // add the element
 return this;}


// equality on lists
ClaireBoolean *list::equalList(list *l2)
{int i, m = length;
  if ( l2->length != m ) return(CFALSE);
  else {for ( i = 1; i <= m ; i++)
            if (equal((*this)[i],(*l2)[i]) == CFALSE) return(CFALSE);
        return(CTRUE);}}

// fast membership (we know that val is identified)
ClaireBoolean *list::memq(OID val)
{ITERATE(j);
    for ( START(this); NEXT(j); ) if (j == val) return(CTRUE);
    return(CFALSE);}

// API functions ----------------------------------------------------------------


// generic contains function
ClaireBoolean *contain_ask_list(list *l,OID val)
{if IDENTIFIED(val) return l->memq(val);
 else {ITERATE(j);
       for ( START(l); NEXT(j) ; ) if ( equal(j,val) == CTRUE) return(CTRUE);
        return(CFALSE);}}

// API version of add, does the type checking
list *add_list(list *l, OID val)
{if (l->of == NULL || l->of->contains(val) == CFALSE ) // v3.3.24
     Cerror(17,val,_oid_(l));         // v3.2
 return l->addFast(val);}

list *add_I_list(list *l, OID val)
{ return l->addFast(val);}

/* create a list with car = val and cdr = l ; l must be a list 999 */
list *cons_any(OID val, list *l)
{  list *obj = list::make();
   int i;
   OID *x = ClAlloc->makeContent(l->length + 1);
     x[1] = val;
     // for (i = 1; i <= l->length; i++) x[i + 1] = (*l)[i];
     if(l->length) memcpy(x+2,l->content+1,sizeof(OID) * l->length);  //<sb> v3.3.33
     obj->length = l->length + 1;
     obj->content = x;
     if (l->of == NULL || l->of->contains(val) == CFALSE ) // v3.3.24
        obj->of = Kernel._any;
     else obj->of = l->of;                 // v3.2.36
     return obj;}

// allocate a list with n members equal to m
list *make_list_integer(int n, OID m)
{  if ( n < 0 ) n = 0;
   list *obj = list::make();
   int i;
   if (n > 0)
      {OID *x = ClAlloc->makeContent(n);
       for (i = 1; i <= n; i++) x[i] = m;
       obj->content = x;}
   else obj->content =  ClAlloc->makeContent(1);   // v3.1.16
   obj->length = n;
   obj->of = Kernel._any;
   return obj;}

// returns the position of val in the list l (use fast iteration)
int index_list (list *l, OID val)
{int i = 1;
 ITERATE(j);
  for (START(l); NEXT(j); i++)  if (equal(j,val) == CTRUE) return(i);
  return(0);}

// add a second list into a first (destructive - equivalent of LISP nconc)
list *add_star_list(list *l1, list *l2)
{int i = l1->length + 1,k;
 ITERATE(j);
 if (l1->of != NULL)
    for (START(l2) ;NEXT(j) ;)
       if (l1->of->contains(j) == CFALSE) Cerror(17,j,_oid_(l1));  // v3.2
 if (i + l2->length > l1->content[0])
    {OID *x = ClAlloc->makeContent(l1->length + l2->length);
       // for ( i =1 ; i <= l1->length ; i++) x[i] = (*l1)[i];
       if (l1->length) memcpy(x+1,l1->content + 1, sizeof(OID) * l1->length);  //<sb> v3.3.33
       l1->content = x;}
 l1->length += l2->length;
 for (START(l2); NEXT(k);) (*l1)[i++] = k;
 return l1;}

// append: non destructive
list *append_list(list *l1, list *l2)
{ITERATE(k);
 if (l1->length == 0) return l2;
 else if (l2->length == 0) return l1;
 else {list *obj = list::make();
       int n =l1->length + l2->length,j = 1;
       OID *x = ClAlloc->makeContent(n);
         //for (START(l1); NEXT(k);)  x[j++] = k;
         //for (START(l2); NEXT(k);) x[j++] = k;
         if (l1->length) memcpy(x+1, l1->content+1,sizeof(OID) * l1->length);  //<sb> v3.3.33
         if (l2->length) memcpy(x+l1->length+1,l2->content+1,sizeof(OID) * l2->length);  //<sb> v3.3.33
         obj->of = ((l1->of == l2->of) ? l1->of : NULL);
         obj->length = n;
         obj->content = x;
         return obj;}}


// insert after a member, works only for a list
list *add_at_list(list *l, int n, OID val)
{int i,j,m = l->length;
 if (l->of == NULL || l->of->contains(val) == CFALSE )        // v3.3.24
     Cerror(17,val,_oid_(l));
 if (n <= 0 || n > m + 1) Cerror(5,n,_oid_(l));                                 // v3.2.24 !
 if (m + 1 == (*l)[0])
    {OID *x = ClAlloc->makeContent(m + 1);
     // for (i = 1; i <= m; i++) x[i] = (*l)[i];
     if (m) memcpy(x+1,l->content+1,sizeof(OID) * m);  //<sb> v3.3.33
     l->content = x;}
 l->length = m + 1;
 // for ( j = m; j >= n ; j-- ) (*l)[j + 1] = (*l)[j];
 if ((m + 1 - n) > 0) memmove(l->content + n + 1, l->content + n, sizeof(OID) * (m + 1 - n));  //<sb> v3.3.33
 (*l)[n] = val;
 return l;}

// removes the nth member of a list
// TODO write rmlast using delete_at
list *delete_at_list (list *l, int n)
{int j, m = l->length;
  if ((n < 1) || (n > m)) Cerror(5,n,_oid_(l));    // v3.2.44 : same error as 2.5
  // for (j= n; j < m; j++) (*l)[j] = (*l)[j+1];
  if (m - n) memmove(l->content+n, l->content + n + 1, sizeof(OID) * (m - n));  //<sb> v3.3.33
  l->length = (m - 1);
  return l;}

 // remove the n first elements of a list
list *skip_list(list *l, int n)
{int i, m = l->length;
    if (n < 0) Cerror(7,_oid_(l),n);
    if (m <= n) l->length = 0;
    else {// for (i = n + 1; i <= m; i++) (*l)[i-n] = (*l)[i];
    	  int len = m - n;
          if (len) memmove(l->content + 1, l->content + 1 + n, sizeof(OID) * len);  //<sb> v3.3.33
          l->length = m - n;}
   return l; }

// old LISP cdr ....
list *cdr_list(list *l)
{int i,m = l->length;
  if (m == 0) {Cerror(8,0,0); return NULL;}
  else { list *obj = list::make();
         OID *x = ClAlloc->makeContent(m - 1);
          //for (i = 2; i <= m; i++) x[i - 1] = (*l)[i];
          if (m - 1) memcpy(x+1,l->content + 2, sizeof(OID) * (m - 1));  //<sb> v3.3.33
          obj->length = m - 1;
          obj->content = x;
          obj->of = l->of;
          return obj;}}

// shrinks a list (keep the same allocation)
// v3.2.20 : also works for a bag !
bag *shrink_list (bag *l, int n)
{if (n < 0) Cerror(7,_oid_(l),n);
 if (n < l->length) l->length = n;
 return l;}


// ***************************************************************************
// * PART 3: Sets                                                            *
// ***************************************************************************


// constructors --------------------------------------------------------

// create an empty list (used for non-constant sets)
set *set::empty()
{set *obj = set::make();
 OID *x = ClAlloc->makeContent(1);
   obj->length = 0;
   obj->content = x;
   obj->of = Kernel.emptySet;                 // v3.2.08 emptySet => do not touch
   return obj;}

// create a typed empty list
set *set::empty(ClaireType *t)
{ClAlloc->currentNew = t;                              // v3.3.34: a method to avoid protecting the type
 set *obj = set::make();
 OID *x = ClAlloc->makeContent(1);
   obj->length = 0;
   obj->content = x;
   obj->of = t;
   ClAlloc->currentNew = NULL;                          // v3.3.36: close
   return obj;}

// create a list skeleton
inline set *set::make()
{set *obj = (set *) ClAlloc->makeAny(4);
  if (ClAlloc->statusGC != 2)  GC_PUSH(obj);    // v3.2.30
  obj->isa = Kernel._set;
  obj->of = NULL;
  obj->content = NULL;
  obj->length = 0;
  return obj;}

// creates a set
set *set::alloc(int n,...)
{va_list ap;
 int i;
 set *obj = set::empty();
 obj->of = Kernel.emptySet;                 // v3.2
   va_start(ap,n);
   for (i = 1; i <= n; i++) obj->addFast(va_arg(ap, OID));
   va_end(ap);
   return obj;}

// creates a typed set
set *set::alloc(ClaireType *t, int n,...)
{va_list ap;
 int i;
 set *obj = set::empty(t);
   va_start(ap,n);
   for (i = 1; i <= n; i++) obj->addFast(va_arg(ap, OID));
   va_end(ap);
   return obj;}



// member methods ------------------------------------------------------

// this methods insert val into the set
set *set::addFast(OID val)
{ if (IDENTIFIED(val) || (contain_ask_set(this,val) == CFALSE))    // otherwise nothing to do !
  {OID *x = content;                // array= chunk
   int m = length, i = 1, j = m,NEW;
   while ((i + 1) < j)              // dichotomic research for the insertion point [i,j]
     {NEW = ((i + j) >> 1);         // NEW is neither l or j
      if (x[NEW] == val) return this;
       else if (x[NEW] < val) i = NEW;
       else j = NEW;}
  if (m == 0) i = 0;
  else if (x[i] == val || x[j] == val) return this;
  else if (x[i] > val) i = i - 1;
  else if (x[j] < val) i = j;
  length = m + 1;
  if (x[0] == m + 1)                // current chunk is full
     { // if (ClEnv->verbose > 10) printf("addFast: complex insertion of OID %x at %d [new length %d]\n",val,j,m + 1);
       OID *y = ClAlloc->makeContent(m + 1);
       //for (j = 1; j <= i; j++) y[j] = x[j];
       //y[j] = val;
       //for (j++; j <= m + 1; j++) y[j] = x[j - 1];
       if (i) memcpy(y+1,x+1,sizeof(OID) * i);  //<sb> v3.3.33
       y[i+1] = val;
       if (m - i > 0) memcpy(y+2+i, x+i+1, sizeof(OID) * (m - i));  //<sb> v3.3.33
       content = y;
       return this;}
  else                             // simply insert val between i and i+1
    { //for (j = m ; j >= i; j--) x[j+1] = x[j];
      if(m - i + 1 > 0) memmove(x+i+1, x+i, sizeof(OID) * (m - i + 1));  //<sb> v3.3.33
      x[i+1] = val;
      return this;}}
 else return this;}


// redefinition of generic equal for sets
// *ClaireBoolean set::isEqual(OID x2)
//  {if (OWNER(x2) == Kernel._set) return equalSet(OBJECT(set,x2));
//   else return(CFALSE);}

// equality on lists
ClaireBoolean *set::equalSet(set *l2)
{int i,direct = 0;                            // direct = 0 <=> fast mode since all members are identified (=> same position)
  if (length != l2->length) return CFALSE;
  for (i=1; i <= length ; i++)
        {OID x = (*this)[i]; 
          if (!IDENTIFIED(x)) direct = 1;
          if (direct != 0) {if (contain_ask_set(l2,x) == CFALSE) return(CFALSE);}
          else if (equal(x,(*l2)[i]) == CFALSE) return(CFALSE);}
  return(CTRUE);}


// API functions ----------------------------------------------------------------

// generic contains function
ClaireBoolean *contain_ask_set(set *s,OID val)
 {OID *x = s->content;                // array= chunk
  int j, m = s->length;
  if (m == 0) return CFALSE;
  if IDENTIFIED(val)
    {int i = 1, j = m, k;
     while ((i + 1) < j)            // dichotomic search
      {k = ((i + j) >> 1);           // k is neither l or j
       if (x[k] == val) return CTRUE;
       else if (x[k] < val) i = k;
       else j = k;}
     if (x[i] == val || x[j] == val) return CTRUE;
     else  return CFALSE;}
  else  { for (j = 1; j <= m; j++) if (equal(x[j],val) == CTRUE) return CTRUE;
          return CFALSE;}}
          
/* insert a value at the end of a list */
set *add_set(set *l, OID val)
{ if (l->of == NULL || l->of->contains(val) == CFALSE ) Cerror(17,val,_oid_(l)); // v3.3.24
  return l->addFast(val);}

set *add_I_set(set *l, OID val)
{ return l->addFast(val);}

// intersection of two sets
set *_exp_set(set *l1, set *l2)
{set *s = set::empty();
 int m1 = l1->length, m2 = l2->length, newl = m1,i1,i2,j = 1;
 if (newl > m2) newl = m2;                    // min of 2 length (if needed)
 for ((i1 = 1,i2 = 1); ((i1 <= m1) && (i2 <= m2));)
      {OID x1 = (*l1)[i1], x2 = (*l2)[i2];
       if IDENTIFIED(x1)
         {if (x1 == x2) {if (j == 1) s->content = ClAlloc->makeContent(newl);
                         s->content[j++] = x1; i1++; i2++;}
          else if (x1 < x2) i1++;
          else i2++;}
       else if (contain_ask_set(l2,x1) == CTRUE)
         {if (j == 1) s->content = ClAlloc->makeContent(newl);
          s->content[j++] = x1; i1++;}
       else i1++;}
  if (i2 == m2)
     for ( ;(i1 <= m1); i1++)
        {OID x1 = (*l1)[i1];
          if (!IDENTIFIED(x1) && contain_ask_set(l2,x1) == CTRUE)
             s->content[j++] = x1;}
  s->length = j - 1;
  return s;}

// union of two sets: merge of sorted lists (sort_of) */
set *append_set (set *l1, set *l2)
{int m1 = l1->length, m2 = l2->length;
  if (m1 == 0) return (set *) copy_bag(l2);
  if (m2 == 0) return (set *) copy_bag(l1);
  set *s = set::make();
  OID *x = ClAlloc->makeContent(m1 + m2);
  int i1,i2,k,j = 1;
  for ((i1 =1, i2 = 1); (i1 <= m1 && i2 <= m2);)       // merge of l1 and l2
    {OID x1 = (*l1)[i1], x2 = (*l2)[i2];
      {if (x1 == x2) {i1++; i2++; x[j++] = x1;}
       else if (x1 < x2)
          {i1++;
           if (!IDENTIFIED(x1))
              {for (k = i2; (k <= m2 && (equal(x1,(*l2)[k]) == CFALSE));k++) ;
               if (k > m2) x[j++] = x1;}              // was not found => add
           else x[j++] = x1;}
       else {i2++;
             if (!IDENTIFIED(x2))
              {for (k = i1; (k <= m1 && (equal(x2,(*l1)[k]) == CFALSE));k++) ;
               if (k > m1) x[j++] = x2; }             // v3.2.44 fixed !
              else x[j++] = x2;}}}
  if (i1 > m1) {//for (;(i2 <= m2); i2++)  x[j++] = (*l2)[i2];     // adds what remain in l2..
               memcpy(x+j, l2->content + i2, sizeof(OID) * (m2 - i2 + 1)); } //<sb> v3.3.33
  else {//for (;(i1 <= m1); i1++)  x[j++] = (*l1)[i1];     // .. or l1
  	memcpy(x+j,l1->content + i1, sizeof(OID) * (m1 - i1 + 1)); }  //<sb> v3.3.33
  if (i1 > m1) for (;(i2 <= m2); i2++)  x[j++] = (*l2)[i2];     // adds what remain in l2..
  else          for (;(i1 <= m1); i1++)  x[j++] = (*l1)[i1];     // .. or l1
  s->content = x;
  s->of = ((l1->of == l2->of) ? l1->of : NULL);
  s->length = (j - 1);
  return s;}



/* remove duplicates: very useful ; works on bags*/
set *set_I_bag (bag *l)
{ if (l->length == 0)
    {if (l->of == NULL) return set::empty(); else return set::empty(l->of);}
  else if (l->isa == Kernel._set) return (set *) l;
  else {set *obj = (set *) copy_bag(l);
        obj->isa = Kernel._set;
        bag::quickSort(obj->content,1,obj->length);     // <yc> sort by hash code */
        // see("------------------------  after quickSort ",_oid_(obj));
        obj->length = bag::quickClean(obj->content,obj->length);
        return obj;}}

/* create a list from an enumeration */
list *list_I_set (set *l)
{list *obj = (list *) copy_bag(l);
   obj->isa = Kernel._list;
   return obj;}

// returns a nice sequence of consecutive numbers */
set *sequence_integer(int n, int m)
{ if (m < n) return set::empty();
  else {if (n == m) return set::alloc(Kernel._integer,1,n);
        else {int i;
              set *x = set::make();
              x->content = ClAlloc->makeContent(m-n+3);
              x->isa = Kernel._set;
              x->of = Kernel._integer;                    // v3.2.01
              x->length = (m -n + 1);
              for (i = n; i <= m; i++) x->content[1 + i - n] = i;
              return x;}}}


// ***************************************************************************
// * PART 4: Tuples                                                          *
// ***************************************************************************

// in Claire 3.0, tuples are bags (without the ability to modify) without
// parameter types (they are values)
// tuples are also types: tuple(1,2) % tuple(integer,integer)

// create a tuple skeleton
inline tuple *tuple::make()
{tuple *obj = (tuple *) ClAlloc->makeAny(4);
  if (ClAlloc->statusGC != 2)  GC_PUSH(obj);                   // v3.2.30
  obj->isa = Kernel._tuple;
  obj->of = NULL;
  obj->content = NULL;
  obj->length = 0;
  return obj;}

// create an empty tuple
tuple *tuple::empty()
{tuple *obj = tuple::make();
 OID *x = ClAlloc->makeContent(1);
   obj->length = 0;
   obj->content = x;
   return obj;}
   
// creates a tuple
tuple *tuple::alloc(int n,...)
{va_list ap;
 int i;
 tuple *obj = tuple::make();
 OID *x = ClAlloc->makeContent(n);
   va_start(ap,n);
   for (i = 1; i <= n; i++) x[i] = va_arg(ap, OID);
   va_end(ap);
   obj->content = x;
   obj->length = n;
   return obj;}

// creates a (really !) temporary list on the stack
// v3.2.26 - stack allocation is only for tuples
// v3.2.58 - since -> copyIfNeeded is used all the time in the interpreted mode we do not
// need to protect the zone from another eval-push :-)
tuple *tuple::allocStack(int n,...)
{va_list ap;
 int i;
 OID *adr = &(ClEnv->stack[ClEnv->index]);
 tuple *obj = (tuple *) adr;
   va_start(ap,n);
   obj->isa = Kernel._tuple;
   obj->of = NULL;
   obj->length = n;
   obj->content = &adr[4];
   (*obj)[0] = ClaireAllocation::log2up(n);        // v3.2.38
   // ClEnv->index += (n + 3);                        v3.2.58
   if (ClEnv->index + n + 3 >= ClAlloc->maxStack) Cerror(27,ClEnv->index,0);
   for (i = 1; i <= n ; i++) (*obj)[i] = va_arg(ap,OID);
   va_end(ap);
   return obj; }

// create a tuple from a list
tuple * tuple_I_list(list *l)
{tuple *x = (tuple *) copy_bag(l);
  x->isa = Kernel._tuple;
  x->of = NULL;
  return x;}
  
// create a list from a tuple
list * list_I_tuple(tuple *l)
{list *x = (list *) copy_bag(l);
  x->isa = Kernel._list;
  x->of = NULL;
  return x;}

// add is reused from list.
tuple *tuple::addFast(OID x)
{return (tuple *) ((list *) this)->addFast(x);}

// copy a tuple from the stack if needed
tuple *tuple::copyIfNeeded()
{ if CLMEM(this) return(this);
  else return (tuple *) copy_bag(this);
}


// ***************************************************************************
// * PART 5: Arrays                                                          *
// ***************************************************************************

// arrays cannot be bags because they are imported - a cute trick that allows the
// generation of really nice (and fast) code
// as opposed to other imported, the wrapper and the wrapped are part of the same
//

// API functions -----------------------------------------------------------------
// copy an array onto another
// note that a[-2] = EOL if step = 1 and a[-2] = size if step = 2
OID *copy_array(OID *a)
{int i,m = ARRAYLENGTH(a);
 ClaireType *t = ARRAYTYPE(a);
 OID *b = ClAlloc->makeArray(m,t);
 int n = ((t == Kernel._float) ? (2 * m) + 1 : m);     // v3.2.44
   for (i = 1; i <= n ; i++) b[i] = a[i];
   return b;}

// returns the length of the array (to be removed later)
// the length is always at *a[0]
int length_array(OID *a) {return (int) a[0];}

// returns the type the array
ClaireType  *of_array(OID *a) {return  ARRAYTYPE(a);}

// creates a new array
OID *make_array_integer(int n, ClaireType *t, OID v)
  { if (t->contains(v) == CFALSE) Cerror(42,_oid_(t),v);
    int i;
    OID *a = ClAlloc->makeArray(n,t);
    if (t != Kernel._float)
       {for (i = 1; i <= n ; i++) a[i] = v;}
    else {double val = OBJECT(ClaireFloat,v)->value;
          for (i = 1; i <= n; i++) ((double *) a)[i] = val;}
    return a;}

// return the nth element of the array
OID nth_get_array(OID *a, int n)
{ // return  (ARRAYFLOAT(a) ? _float_( ((double *) a)[n]) : a[n]); }
  if ARRAYFLOAT(a)
      return _float_( ((double *) a)[n]);
  else return a[n];}

// sets the nth element of the array
void nth_put_array(OID *a, int n, OID y)
   {if ARRAYFLOAT(a) ((double *) a)[n] = OBJECT(ClaireFloat,y)->value;
    else a[n] = y;}

// creates a list copy of the array
list *list_I_array(OID *a)
{ int i,n = ARRAYLENGTH(a);
  list *obj = list::make();
  OID *x = ClAlloc->makeContent(n);
   obj->of = ARRAYTYPE(a);                 // v3.2.8
   obj->length = n;
   obj->content = x;
   if (ClAlloc->statusGC != 2) GC_PUSH(obj);                       // v3.2.54 : protected because of _float_
   if (ARRAYTYPE(a) != Kernel._float)
     {// for (i = 1; i <= n ; i++) x[i] = a[i];
      if (n) memcpy(x+1,a+1,sizeof(OID) * n); }  //<sb> v3.3.33
   else for (i = 1; i <= n; i++) x[i] = _float_( ((double *) a)[i]);
   return obj;}

// creates an array from a list and a type
OID *array_I_list(list *l)
{ int i,n = l->length;
  OID *a = ClAlloc->makeArray(n,((l->of == NULL) ? Kernel._any : l->of));
  ARRAYLENGTH(a) = n;
  for (i = 1; i <= n ; i++)
   {OID v = (*l)[i];
    if ARRAYFLOAT(a)  ((double *) a)[i] = OBJECT(ClaireFloat,v)->value;
    else a[i] = v;}
  return a;}

ClaireBoolean *contain_ask_array(OID *a,OID val)
{ int i,n = ARRAYLENGTH(a);
  if (ARRAYTYPE(a) != Kernel._float)
     {for (i = 1; i <= n ; i++) if (equal(val, a[i]) == CTRUE) return CTRUE;}
  else for (i = 1; i <= n; i++)         // MOST DISGRACEFUL, TRULY ABJECT
     if (equal(val,_float_( ((double *) a)[i])) == CTRUE) return CTRUE;
  return(CFALSE);}

