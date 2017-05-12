#include "strxml.h"
#include <iostream>
#include <stdlib.h>
#include <sstream>

namespace XML
{

typedef std::vector<std::string> str_vec;

void 
Parent::print( std::ostream& os ) const
{
  os << "<" << name_;
  for( str_str_map::const_iterator itr = params_.begin(); itr != params_.end(); ++itr )
    os << " " << itr->first << "=\"" << itr->second << "\"";
  os << ">";
  for( size_t i = 0; i < children_.size(); ++i )
    children_[i]->print( os );
  os << "</" << name_ << ">";
}

const Node*
Parent::get_child( std::string s ) const
{
  for( size_t i = 0; i < children_.size(); ++i )
    if( children_[i]->type() == 2 )
      {
        Parent *p = (Parent*)children_[i];
        if( p->name() == s ) return( p );
      }
  return( 0 );
}

std::string 
next_token( std::istream &is )
{
  static char last_char = 0;
  std::string res;
  if( last_char )
    res += last_char;

  if( last_char == '<' )
    {
      last_char = 0;
      return( res );
    }
  else if( last_char == '>' )
    {
      last_char = 0;
      return( res );
    }

  int next_char;
  while( 1 )
    {
      next_char = is.get();
      if( isspace( next_char ) )
	continue;
      else if( (next_char == 1) || (next_char == -1) || (next_char == 0) )
	{
	  return( std::string( "" ) );
	}
      else if( (next_char == '>') || (next_char == '<') )
	{
	  if( res.length() == 0 )
	    {
	      res += next_char;
	      last_char = 0;
	      return( res );
	    }
	  last_char = next_char;
	  break;
	}
      res += next_char;
    }
  return( res );
}

int 
token_type( char c )
{
  if( c == '=' )
    return( 2 );
  else if( c == '"' )
    return( 3 );
  else if( c == '/' )
    return( 4 );
  else if( isspace( c ) )
    return( 0 );
  else return( 1 );
}

str_vec 
tokenize_string( std::string str )
{
  str_vec v;
  std::string last;
  int t_type = 0;
  for( const char *s = str.c_str(); *s; ++s )
    {
      int n_type = token_type( *s );
      if( (t_type != n_type) && (last.length() != 0) )
	{
	  if( t_type )
	    v.push_back( last );
	  last.erase();
	}
      last += *s;
      t_type = n_type;
    }
  if( last.length() )
    v.push_back( last );
  return( v );
}

int 
do_node( std::string token, PSink& ps )
{
  str_vec node_tokens = tokenize_string( token );

  if( node_tokens.size() == 0 )
    return( -2 );
    
  if( node_tokens[0] == "/" )
    {
      ps.pop_node( node_tokens[1] );
      return( -1 );
    }

  std::string name = node_tokens[0];
  str_pair_vec v;
  for( size_t i = 1; i < node_tokens.size(); i += 5 )
    {
      if( node_tokens[i] == "/" )
	{
	  ps.push_node( name, v );
	  ps.pop_node( name );
	  return( 0 );
	}
      if( (i+5 > node_tokens.size()) ||
	  (node_tokens[i+1] != "=") ||
	  (node_tokens[i+2] != "\"") ||
	  (node_tokens[i+4] != "\"") )
	return( -2 );
      str_pair p( node_tokens[i], node_tokens[i+3] );
      v.push_back( p );
    }
  ps.push_node( name, v );
  return( 1 );
}

int 
parse_stream( std::istream &is, PSink &ps )
{
  int depth = 0;
  std::string token = next_token( is );
  while( token.length() != 0 )
    {
      if( token == "<" )
	{
	  int delta = do_node( next_token( is ), ps );
	  if( delta == -2 )
	    {
	      ps.format_error();
	      return( 1 );
	    }
	  depth += delta;
	  token = next_token( is );
	  if( token != ">" )
	    {
	      ps.format_error();
	      return( 1 );
	    }
	  if( depth == 0 )
	    return( 0 );
	}
      else
	ps.push_text( token );
      token = next_token( is );
    }
  ps.stream_error();
  return( 1 );
}

}; // namespace XML
