#ifndef CLIENT_H
#define CLIENT_H
#include "strxml.h"
#include <iostream>

class Atom;
class Application;
class action_t;
class planner_t;
class problem_t;
class state_t;

#if HAVE_SSTREAM
#include <sstream>
#else
#include <strstream>
namespace std {
typedef std::ostrstream ostringstream;
}
#endif


/*******************************************************************************
 *
 * XML client
 *
 ******************************************************************************/

/*
  This class is responsible for communicating the policy produced by the planner
  to the server. The server sends to the client the descriptions of states for
  which it wants to know an action to execute, and the client sends back the
  descriptions of actions recommended for these states by the planner's policy. 
*/

class XMLClient_t
{
  const problem_t *problem_;
  planner_t *planner_;

  /*
    Based on the description of a state received from the server, constructs 
    that state in the planner's representation.
  */
  const state_t* getState( const XMLNode* stateNode );

  /*
    Based on the description of an atom (i.e., a ground predicate) received 
    from the server, finds that atom in the planner's internal representation 
    table.
  */
  const Atom* getAtom( const XMLNode* atomNode );

public:
  XMLClient_t( planner_t *planner, const problem_t *problem );
  ~XMLClient_t() { } 

  /*
    Carries out a problem-solving session with the server.
  */
  void do_session(std::string name, int socket);

  /*
    Serializes the specified ground action into a description in the form of 
    a string and sends this description string to the server.
  */
  void sendAction( const action_t *a, int socket ) const;
};

#endif // CLIENT_H


