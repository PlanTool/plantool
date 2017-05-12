#include <cstdio>
#include <cstdlib>
#include "Action.h"
using namespace std;

Model const* Action::model;

void Action::initStatic(Model const* model)
{
    Action::model = model;
}

Action::Action(long actNum): actNum(actNum)
{
    computeType();
}

Action::Action(actType type, long actNumUser)
{
    actNum = Action::getBeliefAct(*model,type,actNumUser);
    // be consistent, don't belief the user
    computeType();
}

bool Action::operator==(const Action& action) const
{
    if (action.actNum != actNum) return false;
    return true;
}

int Action::compare(const Action& a, const Action& b)
{
    if (a.type < b.type) return 1;
    else if (a.type > b.type) return -1;

    if (a.actNum < b.actNum) return 1;
    else if (a.actNum > b.actNum) return -1;

    return 0;
}

void Action::setActNum(long actNum)
{
    this->actNum = actNum;

    computeType();
}

void Action::setActNumUser(actType type, long actNumUser)
{
    this->type = type;
    this->actNumUser = actNumUser;
    actNum = Action::getBeliefAct(*model,type,actNumUser);
}

actType Action::getActType()
{
    if (type != None)
        return type;

    computeType();
    return type;
}

long Action::getActNumUser() const {
    return actNumUser;
}

void Action::computeType()
{
    if (model == NULL) {
        cerr<<"Action::model has not been initialized\n";
        exit(1);
    }

    actNumUser = actNum;
    if (actNum - model->getNumInitPolicies() - model->getNumMacroActs() >=0) {
        type = Act; // simple action
        actNumUser -= (model->getNumInitPolicies() + model->getNumMacroActs());
    }
    else if (actNum - model->getNumInitPolicies() >= 0) {
        type = Macro; // macro action
        actNumUser -= model->getNumInitPolicies();
    }
    else type = Initial; // initial action
}

long Action::getBeliefAct(Model const& model, actType aType, long aNum)
{
    if (aType == Act)
        return aNum + model.getNumInitPolicies() + model.getNumMacroActs();
    else if (aType == Macro)
        return aNum + model.getNumInitPolicies();
    else return aNum;
}
