#include "global.h"
#include "client.h"
#include "domains.h"
#include "exceptions.h"
#include "hash.h"
#include "planners.h"
#include "strxml.h"
#include "states.h"
#include <assert.h>
#include <cstring>


/*
  Extracts the number of policy execution attempts (rounds) and the amount of time allowed
  by the server for each attempt.
*/

static int 
sessionRequestInfo(const XMLNode* node, std::ostream &os,
		    size_t& rounds, long& time)
{
  std::string s;
  if( !node->dissect( "num-rounds", s ) ) return( 0 );
  rounds = (size_t)atoi( s.c_str() );

  if( !node->dissect("time-allowed", s ) ) return( 0 );
  time = atol( s.c_str() );

  if( gpt::verbosity > 0 )
    {
      os << "<client>: session start: total number of rounds = " << rounds << std::endl;
      os << "<client>: session start: total allowed time = " << time << std::endl;
    }

  return( 1 );
}


/*******************************************************************************
 *
 * XML client
 *
 ******************************************************************************/

XMLClient_t::XMLClient_t( planner_t *planner, const problem_t *problem)
  : problem_(problem), planner_(planner)
{ }


void
XMLClient_t::do_session(std::string name, int socket)
{
  const action_t *action;
  std::ostringstream oss;
  
  /*
    Initiate session by sending the name of the client and the name of the
    problem to the server. Presumably, the server already has that problem
    loaded.
  */

  oss.str("");
  oss << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" << "<session-request>"
      <<  "<problem-name>" << problem_->name()  << "</problem-name>"
      <<  "<client-name>" << name << "</client-name>"
      <<  "<no-header/>"
      << "</session-request>"
      << '\0';
#if !HAVE_SSTREAM
  oss << '\0';
#endif
  write(socket, oss.str().c_str(), oss.str().length());
  
  /*
    Read session parameters -- the number of rounds to execute a policy
    for the requested problem on the server and the amount of time
    allowed for each round
  */
  const XMLNode* sessionInitNode = read_node(socket);
  long round_time = 0;

  if( !sessionRequestInfo( sessionInitNode, std::cout,
			   gpt::num_rounds, round_time) )
    {     
      if (sessionInitNode != 0)
	{
	  std::cout << "<client>: ERROR: could not extract session info from the server's response to the session request. The server's response was: " << sessionInitNode << std::endl;
	  delete sessionInitNode;
	}
      else
	{
	  std::cout << "<client>: ERROR: could not parse the server's response to the session request."<< std::endl;
	}

      return;
    }

  // execute the specified number of rounds; in this implementation, we ignore round_time 
  for( int rounds_left = gpt::num_rounds; rounds_left > 0; --rounds_left )
    {
      std::cout << "***********************************************" << std::endl;
      std::cout << ">>> ROUND " << gpt::num_rounds - rounds_left + 1 << " OUT OF " << gpt::num_rounds << std::endl;
      std::cout << "***********************************************" << std::endl;  
      
      // initiate round
      oss.str("");
      oss << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" <<  "<round-request/>" << '\0';
#if !HAVE_SSTREAM
      oss << '\0';
#endif
      write(socket, oss.str().c_str(), oss.str().length());

      // read round parameters
      const XMLNode* roundInitNode = read_node(socket); 
      if( !roundInitNode || (roundInitNode->getName() != "round-init") )
	{
	  if (roundInitNode != 0)
	    {
	      std::cout << "<client>: ERROR: could not extract session info from the server's response to the session request. The server's response was: "  << roundInitNode << std::endl;
	      delete roundInitNode;
	    }
	  else
	    {
	      std::cout << "<client>: ERROR: could not parse the server's response to the session request." << std::endl;
	    }

	  return;
	}

      // let the planner know a new round is starting
      planner_->initRound();

      /*
	Start executing the policy. The server should send the client states for
	which the server wants to know a policy, and the client should send
	actions to execute in these states in response.
      */
      const XMLNode *response = NULL;
      const state_t *state = NULL;
      bool bSessionEnd = false;
      
      while( true )
	{
	  // read the server's message
	  response = read_node(socket);
	  
	  if( !response )
	    {
	      std::cout << "<client>: ERROR: no state response!! "<< std::endl;
	      return;	    }

	  // check if the message is a notification about the end of a 
	  // round/session
	  if( response->getName() == "round-end" ) 
	    {
	      /*
	       If it's the end of a round, the server tells the client how much
	       reward the xeceution of the client's policy yielded.
	      */
	      std::string s;
	      if (response->dissect("round-reward", s)) 
		{  
		  float reward = atof(s.c_str());  
		  std::cout << "***********************************************" << std::endl;  
		  std::cout << ">>> END OF ROUND -- REWARD RECEIVED: " << reward << std::endl;  
		  std::cout << "***********************************************\n\n" << std::endl;  
		  planner_->endRound();
		}

	      delete response;
	      response = NULL;
	      break;
	    }
	  else if (response->getName() == "session-end")
	    {
	      bSessionEnd = true;
	      delete response;
	      response = NULL;
	      break;
	    }

	  /*
	   If the message isn't about the end of a round/session, it must contain the state
	   for which the server wants an action.
	  */
	  if( !(state = getState( response )) )
	    {
	      std::cout << "<client>: ERROR: invalid state response from the server! The response was: " << response << std::endl;
	      return;
	    }

	  // ask the planner to provide an action for this state
	  action = planner_->decideAction( *state );
	    
	  if( action == NULL )
	    {
	      std::cout<<"<client>: ERROR: the planner doesn't know what to do in this state! The state is: "; 
	      state->full_print( std::cout, problem_ );
	      std::cout << std::endl;
	      if (state != NULL)
		{
		  delete state;
		}
	      return;
	    }
	  else
	    {
	      // send the action back to the server
	      sendAction( action, socket );
	    }

	  if (state != NULL)
	    {
	      delete state;
	      state = NULL;
	    }

	  if (response != NULL) 
	    {  
	      delete response;
	      response = NULL;
	    }  
	}

      if (bSessionEnd)
	{
	  break;
	}
    }
    
  // at the end of a session, read the total reward yielded by all policy execution rounds
  const XMLNode* endSessionNode = read_node(socket);
  if( endSessionNode )
    {
      std::string s;  
      if (endSessionNode->dissect("total-reward", s)) 
	{  
	  float reward = atof(s.c_str());
	  std::cout << "***********************************************" 
		    << std::endl;  
	  std::cout << ">>> END OF SESSION -- OVERALL REWARD: " << reward 
		    << std::endl;  
	  std::cout << "***********************************************\n" 
		    << std::endl;  
	  gpt::previous_solution_attempt_reward = reward / gpt::num_rounds;
	}  
      delete endSessionNode;
    }  
}


const state_t* 
XMLClient_t::getState( const XMLNode* stateNode )
{
  if( !stateNode || (stateNode->getName() != "turn") )
    return( NULL );
  
  if (stateNode->size() == 2 
      && stateNode->getChild(1)->getName() == "no-observed-fluents") 
    {     
      std::cout << "No state/observations received.\n" << std::endl;   
      return NULL;   
    } 
  else 
    {  
      state_t *s = new state_t;

      // construct the state based on the description of its atoms 
      for (int i = 0; i < stateNode->size(); i++) 
	{        
	  const XMLNode* cn = stateNode->getChild(i);        
	  if (cn->getName() == "observed-fluent") 
	    {     
	      std::string fluent;       
	      const Atom *a = getAtom( cn ); 
	      if (a != NULL)
		{      
		  s->add( *a );
		  StateFormula::unregister_use( a );
		}    
	    }       
	}  

      problem_->complete_state( *s );
      s->make_digest();

      // remove the following lines if you don't want to see every state 
      // received by the client
      std::cout<<"State to act in: ";
      s->full_print(std::cout, problem_);
      return( s );
    }
}




const Atom* 
XMLClient_t::getAtom( const XMLNode* atomNode )
{
  std::ostringstream os;
  os.str("");
  if( !atomNode || (atomNode->getName() != "observed-fluent") )
    {
      std::cout<<"<client> ERROR: There is something wrong with the atom node"<<std::endl;
      return NULL;
    }

  // get predicate name
  std::string predicate_name;
  if (!atomNode->dissect("fluent-name", predicate_name)) 
    {
      std::cout<<"<client> ERROR: There is something wrong with the fluent's name"<<std::endl;
      return NULL;
    }

  // to convert the predicate name to the planner's internal representation, 
  // replace a;; '-' with '_'.
  for (size_t i = 0; i < predicate_name.size(); i++)
    {
      if (predicate_name[i] == '-')
	{
	  predicate_name[i] = '_';
	}
    }

  
  // In the planner's naming convention, an atom is characterized by a string 
  // name__arg1_arg2, where "name" is the name of the predicate, and arg1, arg2,
  // etc. are the names of the constants to which the predicate is applied. 
  // The code below takes an XML message containing the name of a predicate and
  // its arguments, and puts them into the above form.
  os << predicate_name;
  const Domain& domain = problem_->domain();
  bool value = false;
  bool bFirstArg = true;
  for( int i = 0; i < atomNode->size(); ++i )
    {
      const XMLNode* termNode = atomNode->getChild( i );
      if( !termNode )
	{
	  continue;
	}
      else if (termNode->getName() == "fluent-arg")
	{
	  std::string term_name = termNode->getText();

	  if (bFirstArg)
	    {
	      bFirstArg = false;
	      os << "__";    
	    }
	  else
	    {
	      os << "_";
	    }

	  os << term_name;
	}
      else if (termNode != 0 && termNode->getName() == "fluent-value")
	{
	  value = (termNode->getText() == "true");
	}
    }

  // using the atom's description, find the atom's identifier
  std::pair<Predicate,bool> p = domain.predicates().find_predicate( os.str() );
  if( !p.second ) return( NULL );
  Predicate predicate = p.first;

  if (value)
    {
      TermList terms;
      const Atom& a = Atom::make_atom( predicate, terms );
      return( &a );
    }
  else 
    {
      return NULL;
    }
}


void 
XMLClient_t::sendAction( const action_t *a, int socket ) const
{
  std::cout << "--> Action taken: "; 
  a->print(std::cout);
  std::cout<<std::endl;
  std::cout<<std::endl;
  
  // action_t is the planner's internal representation of a ground action, i.e.,
  // of an action application. An action application consists of the name of an
  // action and of the constants to which this action is applied. In what 
  // follows, we essentially extract these names from the planner's internal 
  // representation and serialize them into an XML string.
  //
  // For info on the format of the string the server expects, see 
  //  http://code.google.com/p/rddlsim/source/browse/trunk/cclient/README.txt
  std::string app_string = std::string(a->name()).c_str();

  // An action application has an identifier of the form (act1__x_y___act2__z),
  // where act1, act2, etc. are the actions being applied in parallel, and x,y,
  // z, etc. are the constants these actions are applied to. We essentially 
  // need to parse out the action and constant names out of this identifier, 
  // and put them into an appropriate XML message. The code below dows exactly 
  // that.

  // first, get rid of the parentheses around the action application name.
  app_string = app_string.substr(1, app_string.length() - 2);

  // formulate the header of the XML message
  std::ostringstream oss;
  oss.str("");
  oss << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"; 
  oss  << "<actions>";   

  std::string::size_type prev_pos = 0, pos = 0, prev_act_pos = 0, act_pos = 0; 

  do
    {
      pos = 0;
      prev_pos = 0;

      // figure out the name of the next action being applied in parallel
      act_pos = app_string.find("___", prev_act_pos);

      std::string app( app_string.substr(prev_act_pos, act_pos-prev_act_pos) );
      prev_act_pos = act_pos;

      // figure out the constants to which it is applied
      pos = app.find("__", pos);
      std::string substring( app.substr(prev_pos, pos-prev_pos) );

      // if it's the noop action, we don't need to do much more
      if (strcasecmp(substring.c_str(), "noop") != 0)
	{
	  //wrap the name of the action and its arguments into XML
	  oss  << "<action>" << "<action-name>"<< substring <<"</action-name>"; 
	
	  if (pos != std::string::npos)
	    {
	      while (app.find('_', pos) == pos && pos != std::string::npos )
		{
		  pos++;
		  prev_pos = pos;
		}

	      while( prev_pos != std::string::npos)   
		{      
		  pos = app.find('_', prev_pos);
		  substring = std::string( app.substr(prev_pos, pos-prev_pos) );
		  prev_pos = pos;
		  oss << "<action-arg>" << substring << "</action-arg>";

		  while (app.find('_', prev_pos) == pos && pos != std::string::npos)
		    {
		      pos++;
		      prev_pos = pos;
		    }
		}
	    }    
	  oss << "<action-value>true</action-value>";
	  oss  << "</action>";
	}


      while (app_string.find('_', act_pos) == act_pos && act_pos != std::string::npos )
	{
	  act_pos++;
	  prev_act_pos = act_pos;
	} 
    }
  while (act_pos != std::string::npos);

  oss << "</actions>" << '\0';

#if !HAVE_SSTREAM
  oss << '\0';
#endif

  // send off the action application description!
  write(socket, oss.str().c_str(), oss.str().length());
}
