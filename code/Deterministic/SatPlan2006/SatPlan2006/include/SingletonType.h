// Macro Guard
#ifndef CSE473_SINGLETON_H
#define CSE473_SINGLETON_H


/*============================================================================//
    SingletonType.h: header file for generic singletons

    Purpose is to allow user to create singleton object of arbitrary type.

    Implementation: Shane J. Neph, June 2004, University of Washington
//============================================================================*/


template <typename T>
class SingletonType {
    ~SingletonType() { /* */ }
public:
    static T* Instance() {
        static T val;
        return(&val);
    }
};

#endif // CSE473_SINGLETON_H
