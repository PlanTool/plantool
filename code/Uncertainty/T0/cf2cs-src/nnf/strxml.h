#ifndef XML_H
#define XML_H

#include <vector>
#include <string>
#include <map>
#include <sstream>
#include <stack>

namespace XML
{
  class Node;
  typedef std::pair<std::string,std::string> str_pair;
  typedef std::vector<str_pair> str_pair_vec;
  typedef std::map<std::string,std::string> str_str_map;

  class Node
    {
      int type_;

    public:
      Node( int type ) : type_(type) { }
      virtual ~Node() { }
      virtual const Node* get_child( int i ) const{ return( 0 ); }
      virtual const Node* get_child( std::string s ) const { return( 0 ); }
      virtual int size( void ) const { return( 0 ); }
      virtual void print( std::ostream &os ) const = 0;
      virtual std::string get_text( void ) const = 0;
      virtual std::string get_name( void ) const = 0;
      virtual std::string get_param( std::string s ) const = 0;
      int type( void ) const { return( type_ ); }
  };

  class Text : public Node
  {
    std::string text_;

  public:
    Text( int type = 2 ) : Node(type) { }
    virtual ~Text() { }
    virtual void print( std::ostream &os ) const { os << text_; }
    virtual std::string get_text( void ) const { return( text_ ); }
    virtual std::string get_name( void ) const { return( text_ ); }
    virtual std::string get_param( std::string s ) const { return( "" ); }
    void set_text( std::string &text ) { text_ = text; }
  };

  class Parent : public Node
  {
  protected:
    std::string name_;
    str_str_map params_;
    std::vector<Node*> children_;

  public:
    Parent() : Node(2) { }
    virtual ~Parent()
    {
      for( size_t i = 0; i < children_.size(); ++i )
	delete children_[i];
    }
    virtual void print( std::ostream &os ) const;
    virtual int size( void ) const { return( children_.size() ); }
    virtual std::string get_text( void ) const
    {
      std::ostringstream os;
      for( size_t i = 0; i < children_.size(); ++i )
	children_[i]->print( os );
      std::string s = os.str();
      return( s );
    }
    virtual std::string get_name( void ) const { return( name_ ); }
    virtual std::string get_param( std::string s ) const
    {
      str_str_map::const_iterator it = params_.find( s );
      return( (*it).second );
    }
    virtual const Node* get_child( int i ) const { return( i < (int)children_.size() ? children_[i] : 0 ); }
    virtual const Node* get_child( std::string s ) const;

    const std::string& name( void ) const { return( name_ ); }
    void set_name( std::string name ) { name_ = name; }

    friend class PSink;
  };

  class PSink
  {
  protected:
    std::stack<Node*> s_;
    Node *top_;
    int error_;

  public:
    PSink() : top_(0), error_(0) { }
    ~PSink() { }
    void push_node( std::string name, str_pair_vec params )
    {
      Parent *p = new Parent;
      p->name_ = name;
      for( size_t i = 0; i < params.size(); ++i )
	p->params_[params[i].first] = params[i].second;
      if( s_.size() == 0 )
	{
	  if( top_ ) delete top_;
	  top_ = p;
	}
      else
	((Parent*)(s_.top()))->children_.push_back( p );
      s_.push( p );
    }
    void pop_node( std::string name ) { s_.pop(); }
    void push_text( std::string text )
    {
      Text *t = new Text(1);
      t->set_text( text );
      if( s_.size() == 0 )
	{
	  if( top_ ) delete top_;
	  top_ = t;
	}
      else
	((Parent*)(s_.top()))->children_.push_back( t );
    }
    void format_error( void ) { error_ = 1; }
    void stream_error( void ) { error_ = 2; }
    int error( void ) const { return( error_ ); }

    friend Node* get_node_from_stream( std::istream &is );
  };

  inline int dissect_node( Node *p, std::string child, std::string &destination )
  {
    if( !p ) return( 0 );
    const Node *c = p->get_child( child );
    if( !c ) return( 0 );
    destination = c->get_text();
    return( 1 );
  }
  int parse_stream( std::istream &is, PSink &ps );
  inline Node* get_node_from_stream( std::istream &is )
  {
    PSink ps;
    if( parse_stream( is, ps ) )
      {
	delete ps.top_;
	return( 0 );
      }
    else
      return( ps.top_ );
  }

}; // namespace XML

inline std::ostream& operator<<( std::ostream &os, const XML::Node &node )
{
  node.print( os );
  return( os );
}

inline std::istream& operator>>( std::istream &is, XML::Node* &node )
{
  if( node ) delete node;
  node = XML::get_node_from_stream( is );
  return( is );
}

#endif // XML_H
