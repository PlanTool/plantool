// Macro Guard
#ifndef TYPIFY_CSE473_H
#define TYPIFY_CSE473_H


/*============================================================================//
    Typify.h: header file for Int2Type<int> and Type2Type<Type> UDT's

    Purpose is to allow user to quickly typify an integer (Int2Type<>) or to
     re-type a type.  These can be useful for function overloading purposes
     --> very lightweight.  Located in TypeDefine namespace.

    Implementation: Shane J. Neph, June 2004, University of Washington
//============================================================================*/

namespace TypeDefine {

//===============
// Int2Type<int>
//===============
template <int Val>
struct Int2Type {
	enum { val = Val };
};

//=================
// Type2Type<Type>
//=================
template <typename T>
struct Type2Type {
    typedef T OriginalType;
};

} // namespace TypeDefine

#endif // TYPIFY_CSE473_H
