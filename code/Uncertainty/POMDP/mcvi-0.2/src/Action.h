#ifndef __ACTION_H
#define __ACTION_H

#include "Model.h"

enum actType{Initial, Macro, Act, None}; // types of actions

class Action
{
  public:
    // actNum is for internal use (arrange as InitActs, MacroActs,
    // NormalActs), no need for type.

    // actNumUser is for the user (type + index of the action with
    // this type)
    long actNum, actNumUser;
    actType type;
    static Model const* model;

    explicit Action(long actNum);
    Action(actType type, long actNumUser);
    virtual ~Action() {}

    bool operator==(const Action& action) const;

    static int compare(const Action& a, const Action& b);

    /**
       Set actNum, compute the type and actNumUser
       @param [in] actNum
    */
    void setActNum(long actNum);

    /**
       Set type, actNumUser, compute actNum
       @param [in] type
       @param [in] actNumUser
    */
    void setActNumUser(actType type, long actNumUser);

    /**
       Get the type of action, computed using actNum
    */
    actType getActType();

    /**
       Get actNumUser
    */
    long getActNumUser() const;

    /**
       Compute the type based on actNum
    */
    void computeType();

    /**
       Given the type \a aType and actNumUser number \a aNum, return
       the actNum.
       @param [in] model
       @param [in] aType : the type
       @param [in] aNum  : actNumUser number
       @return actNum
    */
    static long getBeliefAct(Model const& model, actType aType, long aNum);

    /**
       Set the model for querying number of InitActs, MacroActs
       @param [in] model
    */
    static void initStatic(Model const* model);
};

#endif
