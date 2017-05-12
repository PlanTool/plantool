#include "Herding.h"
#include "Include.h"
#include <cstdlib>
#include <deque>

using namespace std;

Herding::Herding(HerdingProblem const& problem, bool useMacro):
        Model(problem.numStateVars, problem.numObsVars, NumActs, NumMacroActs, NumInitPolicies, problem.discount),
        useMacro(useMacro),
        xSize(problem.xSize), ySize(problem.ySize),
        numRegionPerAgent(problem.numRegionPerAgent),
        numGhosts(problem.numGhosts),
        numStateVars(problem.numStateVars),
        probRandom(problem.probRandom),
        grid(problem.grid)
{
   // Figure out the topology - which region is connected to which
    connectivity.resize(numRegionPerAgent);
    vector<bool> done(numRegionPerAgent);
    for (long i = 0; i< numRegionPerAgent; i++){
        connectivity[i].resize(NumActsPerAgent); // where does it go next
        done[i] = false;
    }
    rType.resize(numRegionPerAgent); // junction or not

    for (long i = 0; i< xSize; i++){
        for (long j = 0; j < ySize; j++){
            if (grid[i][j] >= 0) // wall is negative
                if (!done[grid[i][j]]){ // region already processed
                    long k;
                    for (k = 1; grid[i][j] == grid[i+k][j]; k++) ; // east
                    connectivity[grid[i][j]][0] = grid[i+k][j];
                    for (k = 1; grid[i][j] == grid[i][j-k]; k++) ; // south
                    connectivity[grid[i][j]][1] = grid[i][j-k];
                    for (k = 1; grid[i][j] == grid[i-k][j]; k++) ; // west
                    connectivity[grid[i][j]][2] = grid[i-k][j];
                    for (k = 1; grid[i][j] == grid[i][j+k]; k++) ; // north
                    connectivity[grid[i][j]][3] = grid[i][j+k];

                    connectivity[grid[i][j]][4] = grid[i][j]; // unchanged

                    // Check if it is a junction
                    if ((grid[i][j] != grid[i+1][j]) && (grid[i][j] != grid[i-1][j]) && (grid[i][j] != grid[i][j+1]) && (grid[i][j] != grid[i][j-1])){
                        rType[grid[i][j]] = junction;
                    }
                    else{
                        rType[grid[i][j]] = nonjunction;
                    }

                    done[grid[i][j]] = true;
                }
        }
    }

    // Calculate shortest path matrix
    calcShortestPathMatrix();
    discountedReward.push_back(Caught);
    for (long i = 1; i < numAccessibleLocs; i++){
        discountedReward.push_back(discount*discountedReward[i-1]);
    }

    // init variables used for computing macrostate groups from states
    int tempCt = GhostCtLevels;
    for (long i = 0; i < 3; i++) tempCt *= tempCt;
    ghostMStateMap.resize(tempCt,-1);
    ghostMStateCt = 0;
    agentMStateCt = numRegionPerAgent * numRegionPerAgent;

    // init variables used for allowable actions from each macro state grp
    constraints.resize(agentMStateCt);
    findConstraints(&constraints);
}

bool Herding::allowableAct(Belief const& belief, Action const& action)
{
    actType type = action.type;
    if (useMacro && (type != Macro)) return false;
    if (!useMacro && (type == Macro)) return false;

    bool ok = false;
    long temp = belief.beliefNode->obs.obs[1] % agentMStateCt;
    for (int i=0; i < int(constraints[temp].size()); i++){
        if (constraints[temp][i] == action.getActNumUser()){
            ok = true;
            break;
        }
    }
    return ok;
}

void Herding::readProblem(std::string filename, HerdingProblem* problem)
{
    ifstream fp;
    fp.open(filename.c_str(), ios::in);
    if (!fp.is_open()){
        cerr << "Fail to open " << filename << "\n";
        exit(EXIT_FAILURE);
    }
    fp >> problem->numObsVars;
    fp >> problem->probRandom;
    fp >> problem->numGhosts;

    fp >> problem->xSize;
    fp >> problem->ySize;

    problem->numStateVars = 2*(problem->numGhosts + 2);
    problem->initState.resize(problem->numStateVars);
    fp >> problem->initState[a1x];
    fp >> problem->initState[a1y];
    fp >> problem->initState[a2x];
    fp >> problem->initState[a2y];
    for (long i=0; i< problem->numGhosts; i++){
        fp >> problem->initState[2*i + gx];
        fp >> problem->initState[2*i + gy];
    }

    long num = 0, tmp;
    problem->grid.resize(problem->xSize);
    for (long i = 0; i< problem->xSize; i++)
        problem->grid[i].resize(problem->ySize);

    for (long i = 0; i< problem->ySize; i++){
        for (long j = 0; j < problem->xSize; j++){
            fp >> tmp;
            problem->grid[j][problem->ySize-i-1] = tmp-1;
            if (tmp > num)
                num = tmp;
        }
    }

    problem->numRegionPerAgent = num;

    fp.close();
}

double Herding::sample(State const& currState, Action const& action, State* nextState, Obs* obs, RandStream* randStream )
{
    long act = action.getActNumUser();

    if (currState[0] == TermState){ // terminal state
        obs->obs[0] = TermObs;
        (*nextState) = currState;
        return 0;
    }

    (*nextState).resize(numStateVars,0);
    double tempReward = 0;
    bool ghostRemain = false;
    for (long i = 0; i < numGhosts; i++){
        // ghost caught if either agent is at the same location as ghost
        if (((currState[a1x] == currState[gx + 2*i]) && (currState[a1y] == currState[gy + 2*i]))||
            ((currState[a2x] == currState[gx + 2*i]) && (currState[a2y] == currState[gy + 2*i]))){
            tempReward += Caught;
            (*nextState)[gx + 2*i] = -1;
            (*nextState)[gy + 2*i] = -1;
        } else
            if (currState[gx + 2*i] != -1)
                ghostRemain = true;
            else{
                (*nextState)[gx + 2*i] = -1;
                (*nextState)[gy + 2*i] = -1;
            }
    }
    if (ghostRemain == false){
        for (long i=0; i < numStateVars; i++)
            (*nextState)[i] = TermState; // terminal state
        obs->obs[0] = TermObs;
        obs->obs[1] = 0;
        if (this->getNumObsVar()==3)
            obs->obs[2] = 0;
        return tempReward;
    }


    // action of each agent
    long act1 = act/NumActsPerAgent;
    long act2 = act % NumActsPerAgent;

    // change agent 1 position if possible
    if (grid[currState[a1x] + RelativeDirX[act1]][currState[a1y] + RelativeDirY[act1]] >= 0){
        (*nextState)[a1x] = currState[a1x] + RelativeDirX[act1];
        (*nextState)[a1y] = currState[a1y] + RelativeDirY[act1];
    }else{
        (*nextState)[a1x] = currState[a1x];
        (*nextState)[a1y] = currState[a1y];
    }
    // change agent 2 position if possible
    if (grid[currState[a2x] + RelativeDirX[act2]][currState[a2y] + RelativeDirY[act2]] >= 0){
        (*nextState)[a2x] = currState[a2x] + RelativeDirX[act2];
        (*nextState)[a2y] = currState[a2y] + RelativeDirY[act2];
    }else{
        (*nextState)[a2x] = currState[a2x];
        (*nextState)[a2y] = currState[a2y];
    }

    for (long k = 0; k < numGhosts; k++){
        if ((*nextState)[gx + 2*k] != -1){
            // Store legal ghost actions
            // cannot move into walls, cannot move to position currently occupied by agent
            // Compute the ghost move that moves furthest from the closest agent
            long numLegalDir = 0;
            long legalDirs[NumActsPerAgent];
            long costs[NumActsPerAgent];
            long bestCost = -1;
            for (long i = east; i<= unchanged; i++){
                if ((grid[currState[gx + 2*k] + RelativeDirX[i]][currState[gy + 2*k] + RelativeDirY[i]] >= 0) &&
                    ((currState[gx + 2*k] + RelativeDirX[i] != currState[a1x])||(currState[gy + 2*k] + RelativeDirY[i] != currState[a1y])) &&
                    ((currState[gx + 2*k] + RelativeDirX[i] != currState[a2x])||(currState[gy + 2*k] + RelativeDirY[i] != currState[a2y]))){

                    long a1Node = gridNodeLabel[currState[a1x]][currState[a1y]];
                    long a2Node = gridNodeLabel[currState[a2x]][currState[a2y]];

                    long gNode = gridNodeLabel[currState[gx + 2*k]][currState[gy + 2*k]];
                    long gNodeNext = gridNodeLabel[currState[gx + 2*k] + RelativeDirX[i]][currState[gy + 2*k] + RelativeDirY[i]];
                    long cost = (shortestPath[a1Node][gNode] < shortestPath[a2Node][gNode]) ? (shortestPath[a1Node][gNodeNext]*gridNodeLabel.size() + shortestPath[a2Node][gNodeNext]) : (shortestPath[a2Node][gNodeNext]*gridNodeLabel.size() + shortestPath[a1Node][gNodeNext]);

                    if (cost > bestCost)
                        bestCost = cost;

                    legalDirs[numLegalDir] = i;
                    costs[numLegalDir] = cost;
                    numLegalDir++;
                }
            }

            long numBestCost = 0;
            for (long i = 0; i< numLegalDir; i++){
                if (costs[i] == bestCost)
                    numBestCost++;
            }

            double probs[NumActsPerAgent];
            for (long i = 0; i< numLegalDir; i++){
                if (costs[i] == bestCost)
                    probs[i] = (1-probRandom)/numBestCost;
                else
                    probs[i] = probRandom/(numLegalDir-numBestCost);
            }
            double randReal = (double) (randStream->get() % 10000)/10000;
            long ghostDir = 0;
            double currSum = probs[0];
            for (long i=1;i<numLegalDir;i++){
                if (randReal < currSum)
                    break;
                else{
                    ghostDir++;
                    currSum += probs[i];
                }
            }

            (*nextState)[gx + 2*k] = currState[gx + 2*k] + RelativeDirX[legalDirs[ghostDir]];
            (*nextState)[gy + 2*k] = currState[gy + 2*k] + RelativeDirY[legalDirs[ghostDir]];
        }
    }

    obs->obs[0] = OtherObs;
    obs->obs[1] = getObsGrpFromState((*nextState));
    if (this->getNumObsVar()==3)
        obs->obs[2] = computeGhostIndex((*nextState));

    return tempReward + MovementCost;
}

double Herding::sample(State const& currState, Action const& macroAction, long controllerState, State* nextState, long* nextControllerState, Obs* obs, RandStream* randStream )
{
    long macroAct = macroAction.getActNumUser();
    if (currState[0] < 0){
        obs->obs[0] = TermObs;
        (*nextState) = currState;
        return 0;
    }

    nextControllerState = 0;
    double rwd = sample(currState, Action(Act, macroAct), nextState, obs, randStream);
    if (obs->obs[0] == TermObs)
        return rwd;
    else{
        bool a1EnterJunc = ((currState[a1x] != (*nextState)[a1x]) || (currState[a1y] != (*nextState)[a1y])) && (rType[grid[(*nextState)[a1x]][(*nextState)[a1y]]] == junction);
        bool a2EnterJunc = ((currState[a2x] != (*nextState)[a2x]) || (currState[a2y] != (*nextState)[a2y])) && (rType[grid[(*nextState)[a2x]][(*nextState)[a2y]]] == junction);
        if (!(a1EnterJunc || a2EnterJunc)){
            obs->obs.resize(this->getNumObsVar(),0);
            obs->obs[0] = LoopObs;
        }
    }
    return rwd;
}

long Herding::computeGhostIndex(State const& state)
{
    long a1NEnum = 0, a1SEnum = 0, a1SWnum = 0, a1NWnum = 0;
    long a2NEnum = 0, a2SEnum = 0, a2SWnum = 0, a2NWnum = 0;
    int totalGhosts = 0;
    for (long i = 0; i < numGhosts; i++){
        if (state[gx + 2*i] != -1){
            totalGhosts++;
            if (state[gx+2*i] > state[a1x]){
                if (state[gy + 2*i] > state[a1y]){
                    a1NEnum++;
                }else{
                    a1NWnum++;
                }
            }else{
                if (state[gy + 2*i] > state[a1y]){
                    a1SEnum++;
                }else{
                    a1SWnum++;
                }
            }

            if (state[gx+2*i] > state[a2x]){
                if (state[gy + 2*i] > state[a2y]){
                    a2NEnum++;
                }else{
                    a2NWnum++;
                }
            }else{
                if (state[gy + 2*i] > state[a2y]){
                    a2SEnum++;
                }else{
                    a2SWnum++;
                }
            }
        }
    }

    long index = 0;
    if (a1NEnum != 0) a1NEnum = (a1NEnum == totalGhosts) ? GhostCtQuant : (a1NEnum * GhostCtQuant)/totalGhosts + 1;
    index = (index + a1NEnum) * GhostCtLevels;
    if (a1NWnum != 0) a1NWnum = (a1NWnum == totalGhosts) ? GhostCtQuant :(a1NWnum * GhostCtQuant)/totalGhosts + 1;
    index = (index + a1NWnum) * GhostCtLevels;
    if (a1SEnum != 0) a1SEnum = (a1SEnum == totalGhosts) ? GhostCtQuant :(a1SEnum * GhostCtQuant)/totalGhosts + 1;
    index = (index + a1SEnum) * GhostCtLevels;
    if (a1SWnum != 0) a1SWnum = (a1SWnum == totalGhosts) ? GhostCtQuant :(a1SWnum * GhostCtQuant)/totalGhosts + 1;
    index = (index + a1SWnum) * GhostCtLevels;
    if (a2NEnum != 0) a2NEnum = (a2NEnum == totalGhosts) ? GhostCtQuant :(a2NEnum * GhostCtQuant)/totalGhosts + 1;
    index = (index + a2NEnum) * GhostCtLevels;
    if (a2NWnum != 0) a2NWnum = (a2NWnum == totalGhosts) ? GhostCtQuant :(a2NWnum * GhostCtQuant)/totalGhosts + 1;
    index = (index + a2NWnum) * GhostCtLevels;
    if (a2SEnum != 0) a2SEnum = (a2SEnum == totalGhosts) ? GhostCtQuant :(a2SEnum * GhostCtQuant)/totalGhosts + 1;
    index = (index + a2SEnum) * GhostCtLevels;
    if (a2SWnum != 0) a2SWnum = (a2SWnum == totalGhosts) ? GhostCtQuant :(a2SWnum * GhostCtQuant)/totalGhosts + 1;
    index = index + a2SWnum;

    int ghostIndex;
    if (ghostMStateMap[index] == -1){
        ghostIndex = ghostMStateCt;
        ghostMStateMap[index] = ghostIndex;
        ghostMStateCt++;
    }else
        ghostIndex = ghostMStateMap[index];

    return ghostIndex;
}

long Herding::getObsGrpFromState(State const& state)
{
    long a1Region = grid[state[a1x]][state[a1y]];
    long a2Region = grid[state[a2x]][state[a2y]];
    if (this->getNumObsVar()==2){
        long ghostIndex = computeGhostIndex(state);
        return ghostIndex * agentMStateCt + (a1Region * numRegionPerAgent) + a2Region;
    }else{
        return (a1Region * numRegionPerAgent) + a2Region;
    }
}

void Herding::findConstraints(std::vector<std::vector<long> >* constraints)
{
    for (long i = 0; i< agentMStateCt; i++){ // go through all macrostate grps
        long a1Region = i/numRegionPerAgent; // agent 1's region
        long a2Region = i % numRegionPerAgent; // agent 2's region
        for (long j = 0; j< NumActsPerAgent; j++){
            for (long k = 0; k< NumActsPerAgent; k++){
                if ((connectivity[a1Region][j] >= 0) && (connectivity[a2Region][k] >=0)){ // not wall
                    bool ok = true;
                    // allowed to stay only at junctions
                    if ((j == unchanged) && (rType[a1Region] != junction)) ok = false;
                    if ((k == unchanged) && (rType[a2Region] != junction)) ok = false;
                    // at least one agent must move
                    if ((j == unchanged) && (k == unchanged)) ok = false;
                    if (ok){
                        (*constraints)[i].push_back(j*NumActsPerAgent+k);
                    }
                }
            }
        }
    }
}

double Herding::initPolicy(State const& currState, Action const& initAction, long controllerState, State* nextState, long* nextControllerState, Obs* obs, RandStream* randStream )
{
    //long policyIndex = initAction.getActNumUser();
    if (currState[0] == TermState){
        (*nextState) = currState;
        return 0;
    }

    long act1, act2, act;
    long a1NEnum = 0, a1SEnum = 0, a1SWnum = 0, a1NWnum = 0;
    long a2NEnum = 0, a2SEnum = 0, a2SWnum = 0, a2NWnum = 0;
    int totalGhosts = 0;
    for (long i = 0; i < numGhosts; i++){
        if (currState[gx + 2*i] != -1){
            totalGhosts++;
            if (currState[gx+2*i] > currState[a1x]){
                if (currState[gy + 2*i] > currState[a1y]){
                    a1NEnum++;
                }else{
                    a1SEnum++;
                }
            }else{
                if (currState[gy + 2*i] > currState[a1y]){
                    a1NWnum++;
                }else{
                    a1SWnum++;
                }
            }

            if (currState[gx+2*i] > currState[a2x]){
                if (currState[gy + 2*i] > currState[a2y]){
                    a2NEnum++;
                }else{
                    a2SEnum++;
                }
            }else{
                if (currState[gy + 2*i] > currState[a2y]){
                    a2NWnum++;
                }else{
                    a2SWnum++;
                }
            }
        }
    }

    //long index = 0;
    if (a1NEnum != 0) a1NEnum = (a1NEnum == totalGhosts) ? GhostCtQuant : (a1NEnum * GhostCtQuant)/totalGhosts + 1;
    if (a1NWnum != 0) a1NWnum = (a1NWnum == totalGhosts) ? GhostCtQuant :(a1NWnum * GhostCtQuant)/totalGhosts + 1;
    if (a1SEnum != 0) a1SEnum = (a1SEnum == totalGhosts) ? GhostCtQuant :(a1SEnum * GhostCtQuant)/totalGhosts + 1;
    if (a1SWnum != 0) a1SWnum = (a1SWnum == totalGhosts) ? GhostCtQuant :(a1SWnum * GhostCtQuant)/totalGhosts + 1;
    if (a2NEnum != 0) a2NEnum = (a2NEnum == totalGhosts) ? GhostCtQuant :(a2NEnum * GhostCtQuant)/totalGhosts + 1;
    if (a2NWnum != 0) a2NWnum = (a2NWnum == totalGhosts) ? GhostCtQuant :(a2NWnum * GhostCtQuant)/totalGhosts + 1;
    if (a2SEnum != 0) a2SEnum = (a2SEnum == totalGhosts) ? GhostCtQuant :(a2SEnum * GhostCtQuant)/totalGhosts + 1;
    if (a2SWnum != 0) a2SWnum = (a2SWnum == totalGhosts) ? GhostCtQuant :(a2SWnum * GhostCtQuant)/totalGhosts + 1;

    vector<long> temp(4,0);
    temp[0] = a1SEnum + a1NEnum;
    temp[1] = a1SEnum + a1SWnum;
    temp[2] = a1NWnum + a1SWnum;
    temp[3] = a1NEnum + a1NWnum;
    long maxNum = -1;
    act1 = 4;
    for (long i=0; i<4; ++i) {
        // cout << temp[i] << " ";
        if(temp[i] > maxNum && (grid[currState[a1x] + RelativeDirX[i]][currState[a1y] + RelativeDirY[i]] >= 0)){
            maxNum = temp[i];
            act1 = i;
        }
    }
    // cout << act1 << endl;

    temp[0] = a2SEnum + a2NEnum;
    temp[1] = a2SEnum + a2SWnum;
    temp[2] = a2NWnum + a2SWnum;
    temp[3] = a2NEnum + a2NWnum;

    maxNum = -1;
    act2 = 4;
    for (long i=0; i<4; ++i) {
        //cout << temp[i] << " ";
        if(temp[i] > maxNum && (grid[currState[a2x] + RelativeDirX[i]][currState[a2y] + RelativeDirY[i]] >= 0)){
            maxNum = temp[i];
            act2 = i;
        }
    }
    // cout << act2 << endl;

    act = act1 * NumActsPerAgent + act2;

    return sample(currState, Action(Act,act), nextState, obs, randStream);
}

inline double Herding::upperBound(State const& state)
{
    long a1Node = gridNodeLabel[state[a1x]][state[a1y]];
    long a2Node = gridNodeLabel[state[a2x]][state[a2y]];
    double curr = 0;
    for (long i = 0; i < numGhosts; i++){
        if (state[gx+2*i] != -1){
            long gNode = gridNodeLabel[state[gx + 2*i]][state[gy + 2*i]];
            long a1_g_Dist = shortestPath[a1Node][gNode];
            long a2_g_Dist = shortestPath[a2Node][gNode];
            long minSteps = (a1_g_Dist < a2_g_Dist) ? (a1_g_Dist+1)/2: (a2_g_Dist+1)/2;
            curr += discountedReward[minSteps];
        }
    }
    return curr;
}

void Herding::calcShortestPathMatrix()
{
    // Generate node labels. Every accessible position is a node in the graph.
    numAccessibleLocs = 0;
    gridNodeLabel.resize(xSize);
    for (long i = 0; i< xSize; i++){
        gridNodeLabel[i].resize(ySize);
        for (long j = 0; j < ySize; j++){
            if (grid[i][j] < 0)
                gridNodeLabel[i][j] = -1;
            else{
                gridNodeLabel[i][j] = numAccessibleLocs;
                numAccessibleLocs++;
            }
        }
    }


    // alloc memory for all pair shortest path matrix
    shortestPath.resize(numAccessibleLocs);
    for (long i = 0; i < numAccessibleLocs; i++){
        shortestPath[i].resize(numAccessibleLocs);
    }

    // fill in shortest path matrix
    found.resize(numAccessibleLocs);
    for (long i = 0; i< xSize; i++){
        for (long j = 0; j < ySize; j++){
            if (grid[i][j] >= 0)
                calcShortestPath(i,j);
        }
    }
}

// Breadth first search to calculate shortest path from (i,j)
void Herding::calcShortestPath(long i, long j)
{
    deque<long> fifo_i, fifo_j, dist;

    long sourceNode = gridNodeLabel[i][j];

    // init found vector
    for (long k = 0; k < numAccessibleLocs; k++)
        found[k] = false;

    fifo_i.push_back(i); fifo_j.push_back(j); dist.push_back(0);
    found[sourceNode] = true;

    while (!fifo_i.empty()){
        long curr_i = fifo_i.front(); fifo_i.pop_front();
        long curr_j = fifo_j.front(); fifo_j.pop_front();
        long currNode = gridNodeLabel[curr_i][curr_j];
        long currDist = dist.front(); dist.pop_front();
        shortestPath[sourceNode][currNode] = currDist;
        for (long k = 0; k < 4; k++){
            if (gridNodeLabel[curr_i+RelativeDirX[k]][curr_j+RelativeDirY[k]] >= 0){
                long neighbour = gridNodeLabel[curr_i+RelativeDirX[k]][curr_j + RelativeDirY[k]];
                if (!found[neighbour]){
                    found[neighbour] = true;
                    fifo_i.push_back(curr_i+RelativeDirX[k]);
                    fifo_j.push_back(curr_j+RelativeDirY[k]);
                    dist.push_back(currDist+1);
                }
            }
        }
    }
}

void Herding::displayState(State state, long type)
{
    if (type != -1)
        cout << "\n";
    for (long i = 0; i< ySize; i++){
        for (long j = 0; j < xSize; j++){
            if (grid[j][ySize-i-1] == -1)
                if (type != -1)
                    cout << "o";
                else
                    cout << "0";
            else if ((j==state[a1x]) && ((ySize-i-1) == state[a1y]))
                if (type != -1)
                    cout << "@";
                else
                    cout << "1";
            else if ((j==state[a2x]) && ((ySize-i-1) == state[a2y]))
                if (type != -1)
                    cout << "@";
                else
                    cout << "1";
            else{
                bool ghostHere = false;
                for (long k = 0; k < numGhosts; k++){
                    if ((j==state[gx+2*k]) && ((ySize-i-1) == state[gy+2*k])){
                        ghostHere = true;
                        break;
                    }
                }
                if (ghostHere)
                    if (type != -1)
                        cout << "$";
                    else
                        cout << "2";
                else
                    if (type != -1)
                        cout << " ";
                    else
                        cout << "3";
            }
            if ((type == -1) && (j != xSize-1))
                cout << " , ";
    }
    cout << "\n";
}
if (type != -1)
    cout << "\n";
}

void Herding::writeMapping(std::string filename)
{
    ofstream fp;
    fp.open(filename.c_str());
    if (!fp.is_open()){
        cerr << "Fail to open " << filename << "\n";
        exit(EXIT_FAILURE);
    }
    fp << ghostMStateCt << "\n";
    int tempCt = GhostCtLevels;
    for (long i = 0; i < 3; i++) tempCt *= tempCt;

    for (long i=0; i < tempCt; i++){
        if (ghostMStateMap[i] != -1)
            fp << i << " " << ghostMStateMap[i] << "\n";
    }
    fp.close();
}

void Herding::readMapping(std::string filename)
{
    ifstream fp;
    fp.open(filename.c_str(), ios::in);
    if (!fp.is_open()){
        cerr << "Fail to open " << filename << "\n";
        exit(EXIT_FAILURE);
    }
    int tempCt = GhostCtLevels;
    for (long i = 0; i < 3; i++) tempCt *= tempCt;
    ghostMStateMap.resize(tempCt,-1);
    ghostMStateCt = 0;

    fp >> ghostMStateCt;

    for (long i=0; i< ghostMStateCt; i++){
        long index;
        fp >> index;
        fp >> ghostMStateMap[index];
    }
    fp.close();
}

inline double Herding::getObsProb(Action const& action, State const& nextState, Obs const& obs){
    if(nextState[0] == TermState) return obs.obs[0] == TermObs;
    else if(obs.obs[0] == LoopObs) { return 0.0; cerr << "shouldn't end with LoopObs" <<endl;}
    if(obs.obs[1] != getObsGrpFromState(nextState))
        return 0.0;
    if(obs.obs[2] != computeGhostIndex(nextState))
        return 0.0;
    else
        return 1.0;
}

/*
  void Herding::symmetrize(State* state)
  {
  bool swap = false;
  if (state[a1x] > state[a2x])
  swap = true;
  else if ((state[a1x] == state[a2x]) && (state[a1y] > state[a2y]))
  swap = true;
  if (swap){
  long temp = state[a1x];
  state[a1x] = state[a2x];
  state[a2x] = temp;
  temp = state[a1y];
  state[a1y] = state[a2y];
  state[a2y] = temp;
  }
  }
*/
