#include <cstdlib>
#include <cstdio>
#include <cassert>
#include <iostream>
#include <algorithm>
#include "ParticlesBelief.h"
#include "Action.h"
#include "ActNode.h"
#include "Obs.h"
#include "ObsEdge.h"
#include "Model.h"
#include "BeliefNode.h"
#include "RandSource.h"
using namespace std;
typedef Belief::const_iterator const_iterator;

// define static members
Model* BeliefNode::model;
RandSource* ParticlesBelief::randSource;
long ParticlesBelief::numRandStreams;
long ParticlesBelief::maxMacroActLength;
double ParticlesBelief::ESSthreshold = 0.5;
double ParticlesBelief::approxSample = 0.99;

ParticlesBelief::~ParticlesBelief() {
}

void ParticlesBelief::initStatic(RandSource* randSource, long numRandStreams, long maxMacroActLength)
{
    ParticlesBelief::randSource = randSource;
    ParticlesBelief::numRandStreams = numRandStreams;
    ParticlesBelief::maxMacroActLength = maxMacroActLength;
}

State ParticlesBelief::average() const
{
    State avg(BeliefNode::model->getNumStateVar(),0);
    std::vector<Particle>::const_iterator it = belief.begin();
    long n = belief.size();
    for(;it!=belief.end(); it++) {
        for(int i = 0; i < BeliefNode::model->getNumStateVar(); i++) {
            avg[i] += it->state[i];
        }
    }
    for(int i = 0; i < BeliefNode::model->getNumStateVar(); i++) {
        avg[i] /= n;
    }
    return avg;
}

Belief* ParticlesBelief::beliefFromState(const State& st, const Obs& obs, long pathLength)
{
    ParticlesBelief *pb = new ParticlesBelief(new BeliefNode(obs));
    Particle temp(st,pathLength,1.0);
    pb->belief.push_back(temp);
    return pb;
}

Belief* ParticlesBelief::beliefFromStateSet(const vector<State>& st, const Obs& obs, const vector<long>& pathLength)
{
    ParticlesBelief *pb = new ParticlesBelief(new BeliefNode(obs));
    double w = 1.0/st.size();
    for (long i=0; i < (long)st.size(); i++){
        Particle temp(st[i],pathLength[i], w);
        pb->belief.push_back(temp);
    }
    return pb;
}

void print(vector<long> a) {
    for (int i=0; i < (long)a.size(); i++) {
        cerr<<a[i]<<" ";
    }
    cerr<<"\n";
}

void print(vector<double> a) {
    for (int i=0; i < (long)a.size(); i++) {
        cerr<<a[i]<<" ";
    }
    cerr<<"\n";
}

/*
  Generating next belief.
*/
Belief* ParticlesBelief::nextBelief(const Action& action, const Obs& obs, bool useSameRandSeed) const
{
    bool debug = false;
    ParticlesBelief *nxt = NULL;

    if (!this->beliefNode->actNodes.empty())
        nxt = safe_cast<ParticlesBelief*>(this->beliefNode->actNodes[action.actNum]->obsChildren[obs].nextBelief);

    if (nxt != NULL)
        return nxt;

    if (debug) {
        cout<<"ParticlesBelief::nextBelief\n";
    }

    //long obsGrp = BeliefNode::model->getObsGrpFromObs(obs);
    nxt = new ParticlesBelief(new BeliefNode(obs));

    double weight_sum = 0.0;

    #ifdef BDEBUG
    const bool DEBUG = true;
    #else
    const bool DEBUG = false;
    #endif

    if(DEBUG) {
        cout << "=================================================="<<endl;
        cout << this << " " << nxt << endl;
        cout << action.actNum << endl;
        for(long kk = 0; kk < (long)obs.obs.size(); kk++) cout << obs.obs[kk] << " ";
        cout<<endl;
    }

    RandStream randStream;
    if (useSameRandSeed) {
        randStream.initseed(
            beliefNode->actNodes[action.actNum]->randSeed);
    } else {
        randStream.initseed(randSource->get());
    }
    // Use the same seed as in generate(Macro)ObsPartition

    vector<Particle> particles;
    vector<unsigned> seeds;
    for (Belief::const_iterator it = this->begin(numRandStreams, randStream);
         it != this->end(); ++it) {
        particles.push_back(*it);
        seeds.push_back(randStream.get());
    }

    #pragma omp parallel for schedule(guided) reduction(+:weight_sum)
    for (unsigned int i = 0; i < particles.size(); ++i) {
        Particle const& currParticle = particles[i];
        RandStream rStream;
        rStream.initseed(seeds[i]);
        State currState = currParticle.state;

        State nextState(BeliefNode::model->getNumStateVar(),0);
        Obs currObs(vector<long>(BeliefNode::model->getNumObsVar(),0));

        long currPathLength = currParticle.pathLength;

        if (action.type == Act){
            BeliefNode::model->sample(currState,
                                      action,
                                      &nextState,
                                      &currObs,
                                      &rStream);
            ++currPathLength;
        } else if (action.type == Macro || action.type == Initial) {
            double (Model::*sample)(State const&, Action const&, long, State*, long*, Obs*, RandStream*) = &Model::sample;
            if (action.type == Initial) {
                sample = &Model::initPolicy;
            }

            long currMacroActState = InitMacroActState;
            long nextMacroActState;

            for (long k=0; k < maxMacroActLength; k++) {
                if (BeliefNode::model->isTermState(currState)) {
                    BeliefNode::model->setObsType(&currObs, TermObs);
                    break;
                }
                (BeliefNode::model->*sample)(currState, action,
                                             currMacroActState,
                                             &nextState,
                                             &nextMacroActState,
                                             &currObs, &rStream);

                currState = nextState;
                currMacroActState = nextMacroActState;
                ++currPathLength;

                if (BeliefNode::model->getObsType(currObs) != LoopObs)
                    break;
            }
        } else {
            cerr << "Illegal actType in ParticlesBeliefSet::nextBelief. actType = " << action.type << "\n";
            exit(EXIT_FAILURE);
        }

        double obsProb = BeliefNode::model->getObsProb(action,
                                                       nextState,
                                                       obs);

        if (obsProb != 0.0) {
            if (debug) {
                cout<<currState[1]<<" "<<action.actNum<<" "<<nextState[1]<<" "<<currObs.obs[0]<<"\n";
            }

            double new_weight = currParticle.weight * obsProb;

            if (debug) {
                cout<<"currParticle.weight = "<<currParticle.weight<<"\n";
                cout<<"obsProb = "<<obsProb<<"\n";
                cout<<"new_weight = "<<new_weight<<"\n";
            }

            weight_sum += new_weight;
            Particle tmp(nextState,
                         currPathLength,
                         new_weight);

            #pragma omp critical
            {
                nxt->belief.push_back(tmp);
            }
        }
    }

    if (weight_sum == 0.0) {
        if (debug) {
            cerr<<"action = "<<action.actNum<<" "<<action.type<<" / obs\n";
            print(obs.obs);
            for (long i = 0; i < numRandStreams; i++){
                cerr<<"currState ";
                print((this->getParticle(i)).state);
            }
        }

        delete nxt;
        cerr << "\nWarning: no next belief" << endl;
        return NULL;
    }

    // normalize
    for (unsigned i=0; i < nxt->belief.size(); i++)
        nxt->belief[i].weight /= weight_sum;

    double ess = ESS(nxt->belief);
    if (ess <= ESSthreshold * nxt->belief.size()) {
        cout<<"Resampling\n";
        // resampling
        vector<Particle>belief_tmp = nxt->belief;
        nxt->belief.clear();

        int cum_idx = 0;
        double cum_weight = belief_tmp[cum_idx].weight;
        double weight_interval = 1.0 / numRandStreams;
        double sample_weight = weight_interval * ((double) rand()/((double)RAND_MAX+1));
        for(int i = 0; i < numRandStreams; i++) {
            while (cum_weight < sample_weight) {
                cum_idx++;
                cum_weight += belief_tmp[cum_idx].weight;
            }

            Particle newParticle = belief_tmp[cum_idx];
            newParticle.weight = weight_interval;
            nxt->belief.push_back(newParticle);

            sample_weight += weight_interval;
        }

        nxt->cum_sum.clear();
        // } else if (ess >= approxSample * nxt->belief.size()) {
        //     nxt->cum_sum.clear();
    } else {
        nxt->compute_cum_sum();
    }

    if (nxt->belief.size() == 0){
        delete nxt;
        return NULL;
    }

    if (debug) {
        cout<<"Leaving ParticlesBelief::nextBelief";
    }

    return nxt;
}

void ParticlesBelief::compute_cum_sum()
{
    cum_sum.clear();
    cum_sum.push_back(belief[0].weight);
    for (unsigned i=1; i < belief.size(); i++)
        cum_sum.push_back(cum_sum[i-1] + belief[i].weight);

    // the total sum is 1
    cum_sum.back() = 1;
}

double ParticlesBelief::ESS(vector<Particle>& sample)
{
    // double cv2 = 0.0;
    // double M = sample.size();
    // for (vector<Particle>::iterator it=sample.begin();
    //      it != sample.end();
    //      ++it)
    // {
    //     cv2  +=  (M * it->weight - 1) * (M * it->weight - 1);
    // }

    // return M*M / ( M + cv2);

    double sum = 0.0;
    for (vector<Particle>::iterator it=sample.begin();
         it != sample.end();
         ++it)
    {
        sum +=  it->weight * it->weight;
    }

    return 1.0 / sum;

}

Particle ParticlesBelief::sample(RandStream& randStream) const
{
    if (cum_sum.empty())
        return belief[randStream.getf() * belief.size()];

    double cum = randStream.getf();
    vector<double>::const_iterator low =
            lower_bound(cum_sum.begin(), cum_sum.end(), cum);
    unsigned int index = low - cum_sum.begin();

    assert(index <= belief.size());
    return belief[index];
}

ParticlesBelief::ParticlesBeliefIterator::ParticlesBeliefIterator(vector<Particle> const* belief, int num_particles, RandStream& randStream)
        : belief_(belief), num_particles_(num_particles),
          current_particle_(0),
          cum_index_(-1),
          cum_weight_(0.0),
          weight_interval_(1.0 / num_particles)
{
    sample_weight_=(weight_interval_ * randStream.getf());
}

ParticlesBelief::ParticlesBeliefIterator::ParticlesBeliefIterator(vector<Particle> const* belief, int num_particles)
        : belief_(belief), num_particles_(num_particles),
          current_particle_(0),
          cum_index_(-1),
          cum_weight_(0.0),
          weight_interval_(1.0 / num_particles),
          sample_weight_(weight_interval_ * (rand() / ((double)RAND_MAX + 1)))
{}

ParticlesBelief::ParticlesBeliefIterator::ParticlesBeliefIterator(ParticlesBelief::ParticlesBeliefIterator const& other)
        : belief_(other.belief_), num_particles_(other.num_particles_),
          current_particle_(other.current_particle_),
          cum_index_(other.cum_index_),
          cum_weight_(other.cum_weight_),
          weight_interval_(other.weight_interval_),
          sample_weight_(other.sample_weight_),
          new_particle_(other.new_particle_)
{}

ParticlesBelief::ParticlesBeliefIterator* ParticlesBelief::ParticlesBeliefIterator::clone()
{
    assert(current_particle_ <= num_particles_);
    ParticlesBelief::ParticlesBeliefIterator* new_iterator = new ParticlesBelief::ParticlesBeliefIterator(*this);

    return new_iterator;
}

void ParticlesBelief::ParticlesBeliefIterator::operator++()
{
    assert(current_particle_<= num_particles_);
    if (current_particle_ == num_particles_) {
        ParticlesBelief::ParticlesBeliefIterator* temp = new ParticlesBelief::ParticlesBeliefIterator(NULL,-1);
        swap(*this,*temp);
        return;
    }

    while (cum_weight_ < sample_weight_) {
        ++cum_index_;
        cum_weight_ += (*belief_)[cum_index_].weight;
    }
    sample_weight_ += weight_interval_;
    if (sample_weight_ > 1) sample_weight_ = 1;
    current_particle_++;
}

bool ParticlesBelief::ParticlesBeliefIterator::operator!=(Belief::BeliefItImpl const& right)
{
    ParticlesBelief::ParticlesBeliefIterator const* temp = safe_cast<ParticlesBelief::ParticlesBeliefIterator const*>(&right);
    return (belief_ != temp->belief_ ||
            num_particles_ != temp->num_particles_ ||
            current_particle_ != temp->current_particle_ ||
            cum_index_ != temp->cum_index_);
    // other attributes are double and too prone to error but the
    // first sample_weight_ may be needed, but not for this iterator
}

Particle const& ParticlesBelief::ParticlesBeliefIterator::operator*()
{
    assert(current_particle_ <= num_particles_);
    if (cum_index_ < 0)
        throw 20;
    assert(cum_index_ < int(belief_->size()));

    new_particle_ = (*belief_)[cum_index_];
    new_particle_.weight = weight_interval_;

    return new_particle_;
}

Particle const* ParticlesBelief::ParticlesBeliefIterator::getPointer()
{
    assert(current_particle_ <= num_particles_);
    assert(cum_index_ < int(belief_->size()));

    new_particle_ = (*belief_)[cum_index_];
    new_particle_.weight = weight_interval_;

    return &new_particle_;
}

const_iterator ParticlesBelief::begin(int num_particles, RandStream& randStream) const
{
    ParticlesBelief::ParticlesBeliefIterator* temp = new ParticlesBeliefIterator(&belief, num_particles, randStream);
    ++(*temp);
    return const_iterator(temp);
}

const_iterator ParticlesBelief::begin(int num_particles) const
{
    ParticlesBelief::ParticlesBeliefIterator* temp = new ParticlesBeliefIterator(&belief, num_particles);
    ++(*temp);
    return const_iterator(temp);
}

const_iterator ParticlesBelief::end() const
{
    return const_iterator(new ParticlesBeliefIterator(NULL,-1));
}
