# 
# RDF.py - Redland Python RDF module
#
# $Id$
#
# Copyright (C) 2000-2002 David Beckett - http://purl.org/net/dajobe/
# Institute for Learning and Research Technology - http://www.ilrt.org/
# University of Bristol - http://www.bristol.ac.uk/
# 
# This package is Free Software or Open Source available under the
# following licenses (these are alternatives):
#   1. GNU Lesser General Public License (LGPL)
#   2. GNU General Public License (GPL)
#   3. Mozilla Public License (MPL)
# 
# See LICENSE.html or LICENSE.txt at the top of this package for the
# full license terms.
# 
# 
#

# TODO:
# * express failure conditions with Exceptions -- create a class hierarchy
#   of exceptions for Redland
# * rename modules: Redland as _Redland, RDF as Redland


"""Redland Python API

More documentation needed here.

See the perl API documentation for inspiration :)

"""

import sys
import string

__all__ = [ "World",
            "Node",
            "Statement",
            "Model",
            "Iterator",
            "Stream",
            "Storage",
            "Uri",
            "Parser"]

__version__ = "0.6"

# Package variables [globals]
#   Python style says to use _ to prevent exporting
#   Use two underscores "(class-private names,
#     enforced by Python 1.4) in those cases where it is important
#     that only the current class accesses an attribute"
#      -- http://python.sourceforge.net/peps/pep-0008.html

_debug = 0
_world = None

_node_types={
    'NODE_TYPE_UNKNOWN'   : 0,
    'NODE_TYPE_RESOURCE'  : 1,
    'NODE_TYPE_PROPERTY'  : 1,
    'NODE_TYPE_LITERAL'   : 2,
    'NODE_TYPE_STATEMENT' : 3,
    'NODE_TYPE_LI'        : 4,
    'NODE_TYPE_BLANK'     : 5,
    'NODE_TYPE_LAST'      : 5}

import Redland;

class RedlandError(Exception):
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return `self.value`

class NodeTypeError(RedlandError):
    pass

def node_type(name):
    if _node_types.has_key(name):
        return _node_types[name]
    else:
        raise NodeTypeError('Unknown node type %s' % name)

def node_type_name(num):
    for n in _node_types.keys():
        if num==_node_types[n]:
            return n
    raise NodeTypeError('Unknown node type number %d' % num)

class World:
  """Core RDF class"""

  def __init__(self,digest_name="",uri_hash=None):
    """Create new RDF World object (constructor)"""
    self._world=Redland.librdf_new_world()
    Redland.librdf_world_open(self._world)
    Redland.librdf_python_world_init(self._world)
    import Redland_python;
    Redland_python.set_callback(World.message)

  def message (type, message):
    if type == 0:
      raise "Redland error - ",message
    else:
      raise "Redland warning - ",message

  def __del__(self):
    """Destroy RDF World object (destructor)."""
    global _debug    
    if _debug:
      print "Destroying RDF.World"
    Redland.librdf_free_world(self._world)

# end class World

def debug(value=-1):
  global _debug
  if value >= 0:
    _debug=value
  else:
    return _debug

class Node:

  def __init__(self, **args):
    """Create an RDF Node (constructor)."""
    global _world
    global _debug
    if _debug:
      print "Creating RDF.Node args=",args
    self._node=None
    self._free_me=1

    if args.has_key('uri_string'):
      self._node=Redland.librdf_new_node_from_uri_string(_world._world,
        args['uri_string'])

    elif args.has_key('uri'):
      # no need to copy the underlying uri as the redland C api does
      # this on node construction
      self._node=Redland.librdf_new_node_from_uri(args['uri']._reduri)

    elif args.has_key('literal'):
      if args.has_key('xml_language'):
        xml_language=args['xml_language']
      else:
        xml_language=""
      if args.has_key('is_wf_xml'):
        is_wf_xml=args['is_wf_xml']
      else:
        is_wf_xml=0
      self._node=Redland.librdf_new_node_from_literal(_world._world,
        args['literal'], xml_language, is_wf_xml)

    elif args.has_key('node'):
      self._node=Redland.librdf_new_node_from_node(args['node']._node)

    elif args.has_key('from_object'):
      # internal constructor to build an object from a node created
      # by librdf e.g. from the result of a iterator->next operation
      # this is always shared (at present) so should not be freed
      self._node=args['from_object']
      self._free_me=args['free_node']
    else:
      self._node=Redland.librdf_new_node(_world._world)

  def __del__(self):
    """Free an RDF Node (destructor)."""
    global _debug    
    if _debug:
      print "Destroying RDF.Node"
    if self._node and self._free_me:
      if _debug:
        print "Deleting Redland node object"
      Redland.librdf_free_node(self._node)

  def __getattr__(self, name):
    if name=='uri':
        return self._get_uri()
    elif name=='type':
        return self._get_type()
    else:
        return self.__dict__[name]

  def __setattr__(self, name, value):
    if name=='uri':
        self._set_uri(value)
    elif name=='type':
        return self._set_type()
    else:
        self.__dict__[name]=value

  def _get_uri(self):
    # we return a copy of our internal uri
    if self._get_type()==node_type('NODE_TYPE_RESOURCE'):
        return Uri(from_object=Redland.librdf_node_get_uri(self._node))
    else:
        raise NodeTypeError("Can't get URI for node type %s (%d)" % \
            (node_type_name(self._get_type()), self._get_type()))

  def _set_uri(self, uri):
    # when we set the uri, the node takes ownership of it
    # also it frees the existing one.  this means that
    # we need to take a copy of the underlying redland URI
    # passed in, so as not to cause the URI to disappear
    # when this node is deconstructed
    Redland.librdf_node_set_uri(self._node,
        Redland.librdf_uri_from_uri(uri._reduri))

  def _get_type(self):
    return Redland.librdf_node_get_type(self._node)

  def _set_type(self, type):
    Redland.librdf_node_set_type(self._node, type)

  def get_literal_value (self):
    """Get a dictionary containing the value of the node literal"""
    val={
        'string': Redland.librdf_node_get_literal_value(self._node),
        'language': Redland.librdf_node_get_literal_value_language(self._node),
        'is_wf_xml': Redland.librdf_node_get_literal_value_is_wf_xml(self._node)
        }
    return val

  def set_literal_value (self,string,xml_language,is_wf_xml):
    """Set a literal node string value."""
    Redland.librdf_node_set_literal_value(self._node, string,
        xml_language, is_wf_xml)

  def __str__(self):
    """Get a string representation of an RDF Node."""
    if self._node == None:
        return ""
    else:
        return Redland.librdf_node_to_string(self._node)

  def __eq__ (self,other):
    """Equality of an RDF Node compared to another RDF Node."""
    if type(other)==type(None):
        if type(self)==type(None):
            return 1
        else:
            return 0
    return (Redland.librdf_node_equals(self._node, other._node) != 0)

  def __ne__ (self,other):
    """Inequality of an RDF Node compared to another RDF Node."""
    if type(other)==type(None):
        if type(self)==type(None):
            return 0
        else:
            return 1
    return (Redland.librdf_node_equals(self._node, other._node) == 0)

  def __hash__(self):
    return hash(str(self))

  def is_resource(self):
    return self.type==_node_types['NODE_TYPE_RESOURCE']

  def is_literal(self):
    return self.type==_node_types['NODE_TYPE_LITERAL']
  
# end class Node


class Statement:

  def __init__(self, **args):
    """Create an RDF Statement (constructor)."""
    global _world
    global _debug    
    if _debug:
      print "Creating RDF.Statement object args",args
    self._statement=None
    self._free_me=1

    if args.has_key('statement'):
        self._statement=Redland.librdf_new_statement_from_statement(
            args['statement']._statement)

    elif args.has_key('subject'):
        subject=args['subject']
        predicate=args['predicate']
        object=args['object']

        if subject==None:
            s=None
        else:
            s=Redland.librdf_new_node_from_node(subject._node)

        if predicate==None:
            p=None
        else:
            p=Redland.librdf_new_node_from_node(predicate._node)

        if object==None:
            o=None
        else:
            o=Redland.librdf_new_node_from_node(object._node)

        self._statement=Redland.librdf_new_statement_from_nodes(
            _world._world, s, p, o)

    elif args.has_key('from_object') and args.has_key('free_statements'): 
      # internal constructor to build an object from a statement created
      # by librdf e.g. from the result of a stream.next operation
      self._statement=args['from_object']
      self._free_me=args['free_statements']
      
    else:
      self._statement=Redland.librdf_new_statement(_world._world)

  def __del__(self):
    global _debug    
    if _debug:
      print "Destroying RDF.Statement"
    if self._statement and self._free_me:
      if _debug:
        print "Deleting Redland statement object"
      Redland.librdf_free_statement(self._statement)

  def __getattr__(self, name):
    if name=="subject":
        return self._get_subject()
    elif name=="predicate":
        return self._get_predicate()
    elif name=="object":
        return self._get_object()
    else:
        return self.__dict__[name]

  def __setattr__(self, name, value):
    if name=="subject":
        self._set_subject(value)
    elif name=="predicate":
        self._set_predicate(value)
    elif name=="object":
        self._set_object(value)
    else:
        self.__dict__[name]=value

  def _wrap_node(self, rednode):
    return Node(free_node=1, from_object=
        Redland.librdf_new_node_from_node(rednode))

  def _get_subject(self):
    return self._wrap_node(
        Redland.librdf_statement_get_subject(self._statement))

  def _get_object(self):
    return self._wrap_node(
        Redland.librdf_statement_get_object(self._statement))

  def _get_predicate(self):
    return self._wrap_node(
        Redland.librdf_statement_get_predicate(self._statement))

  def _set_subject(self, value):
    if value==None:
        Redland.librdf_statement_set_subject(self._statement, None)
    else:
        Redland.librdf_statement_set_subject(self._statement,
            Redland.librdf_new_node_from_node(value._node))

  def _set_object(self, value):
    if value==None:
        Redland.librdf_statement_set_object(self._statement, None)
    else:
        Redland.librdf_statement_set_object(self._statement,
             Redland.librdf_new_node_from_node(value._node))

  def _set_predicate(self, value):
    if value==None:
        Redland.librdf_statement_set_predicate(self._statement, None)
    else:
        Redland.librdf_statement_set_predicate(self._statement,
            Redland.librdf_new_node_from_node(value._node))

  def __str__ (self):
    if self._statement == None:
        return ""
    else:
        return Redland.librdf_statement_to_string(self._statement)

# end class Statement

class Model:

  def __init__(self, storage, **args):
    """Create an RDF Model (constructor)."""
    global _world
    global _debug    
    if _debug:
      print "Creating RDF.Model args=",args
    self._model=None
    self._storage=None

    if args.has_key('options_string'):
      self._model=Redland.librdf_new_model(_world._world, storage._storage,
        args['options_string'])
    elif args.has_key('options_hash'):
      self._model=Redland.librdf_new_model_with_options( _world._world,
        storage._storage, args['options_hash'].hash)
    elif args.has_key('model'):
      self._model=Redland.librdf_new_model_from_model(storage._storage,
                                                     args['model']._model)
    else:
      self._model=Redland.librdf_new_model(_world._world, storage._storage, "")

    if self._model == "NULL" or self._model == None:
      self._model=None
      raise "new RDF.Model failed"
    else:
      # keep a reference around so storage object is destroyed after this
      self._storage=storage

  def __del__(self):
    global _debug    
    if _debug:
      print "Destroying RDF.Model"
    if self._model:
      Redland.librdf_free_model(self._model)

  def size(self):
    return Redland.librdf_model_size(self._model)

  def add(self,subject,predicate,object):
    return Redland.librdf_model_add(self._model, 
        Redland.librdf_new_node_from_node(subject._node),
        Redland.librdf_new_node_from_node(predicate._node),
        Redland.librdf_new_node_from_node(object._node));

  def add_string_literal_statement (self,subject,predicate,
                                    string,xml_language,is_wf_xml):
    return Redland.librdf_model_add_string_literal_statement( self._model,
        subject._node, predicate._node, string,
        xml_language, 0, is_wf_xml)

  def add_statement (self,statement):
    # adding a statement means it gets *copied* into the model
    # we should be free to re-use the statement after adding it
    Redland.librdf_model_add_statement(self._model, 
        statement._statement)

  def add_statements (self,statement_stream):
    return Redland.librdf_model_add_statements(self._model,
        statement_stream.stream)

  def remove_statement (self,statement):
    return Redland.librdf_model_remove_statement(self._model,
        statement._statement)

  def contains_statement (self,statement):
    return Redland.librdf_model_contains_statement(self._model,
        statement._statement)

  def serialize(self):
    return self.serialise()

  def serialise (self):
    my_stream=Redland.librdf_model_serialise(self._model)
    return Stream(my_stream,self,1)

  def find_statements (self,statement):
    my_stream=Redland.librdf_model_find_statements(self._model,
        statement._statement)
    return Stream(my_stream,self,0)

  def sources (self,arc,target):
    my_iterator=Redland.librdf_model_get_sources(self._model, arc._node,
        target._node)
    if my_iterator is None:
      return []

    user_iterator=Iterator(my_iterator,self,arc,target)
    if user_iterator is None:
      return []

    results=[]
    while not user_iterator.end():
      results.append(user_iterator.get())
      user_iterator.next()

    user_iterator=None
    return results

  def arcs (self,source,target):
    my_iterator=Redland.librdf_model_get_arcs(self._model, source._node,
        target._node)
    if my_iterator is None:
      return []

    user_iterator=Iterator(my_iterator,self,source,target)
    if user_iterator is None:
      return []

    results=[]
    while not user_iterator.end():
      results.append(user_iterator.get())
      user_iterator.next()

    user_iterator=None
    return results

  def targets (self,source,arc):
    my_iterator=Redland.librdf_model_get_targets(self._model, source._node,
        arc._node)
    if my_iterator is None:
      return []

    user_iterator=Iterator(my_iterator,self,source,arc)
    if user_iterator is None:
      return []

    results=[]
    while not user_iterator.end():
      results.append(user_iterator.get())
      user_iterator.next()

    user_iterator=None
    return results

  def get_sources_iterator (self,arc,target):
    my_iterator=Redland.librdf_model_get_sources(self._model, arc._node,
        target._node)
    return Iterator(my_iterator,self,arc,target)

  def get_arcs_iterator (self,source,target):
    my_iterator=Redland.librdf_model_get_arcs(self._model, source._node,
        target._node)
    return Iterator(my_iterator,self,source,target)

  def get_targets_iterator (self,source,arc):
    my_iterator=Redland.librdf_model_get_targets(self._model, source._node,
        arc._node)
    return Iterator(my_iterator,self,source,arc)

  def get_source (self,arc,target):
    my_node=Redland.librdf_model_get_source(self._model, arc._node,
        target._node)
    if not my_node:
      return None
    else:
      return Node(from_object=my_node, free_node=1)

  def get_arc (self,source,target):
    my_node=Redland.librdf_model_get_arc(self._model, source._node,
        target._node)
    if not my_node:
      return None
    else:
      return Node(from_object=my_node, free_node=1)

  def get_target (self,source,arc):
    my_node=Redland.librdf_model_get_target(self._model, source._node,
        arc._node)
    if not my_node:
      return None
    else:
      return Node(from_object=my_node, free_node=1)

# end class Model


class Iterator:

  def __init__(self,object,creator1=None,creator2=None,creator3=None):
    """Create an RDF Iterator (constructor)."""
    global _debug    
    if _debug:
      print "Creating RDF.Iterator object=",object,"creator=",creator1
    self._iterator=object
    # keep references to the things we're iterating over so they
    # don't get destroyed before we're done with them.
    self._creator1=creator1
    self._creator2=creator2
    self._creator3=creator3

  def __del__(self):
    global _debug    
    if _debug:
      print "Destroying RDF.Iterator"
    Redland.librdf_free_iterator(self._iterator)

  def end (self):
    return Redland.librdf_iterator_end(self._iterator)

  def have_elements (self):
    print """RDF.Iterator method have_elements is deprecated,
please use 'not iterator.end' instead."""
    return Redland.librdf_iterator_have_elements(self._iterator)

  def get (self):
    my_node=Redland.librdf_iterator_get_object(self._iterator)
    if my_node == "NULL" or my_node == None:
      return None

    # return a new (1) node (2)owned by the librdf iterator object
    # Reasons: (1) at the user API level the iterator only returns nodes
    #          (2) the node returned is shared with the iterator
    node=Node(from_object=my_node, free_node=0)
    return node

  def next (self):
    Redland.librdf_iterator_next(self._iterator)

# end class Iterator


class Stream:

  def __init__(self, object, creator, free_statements):
    """Create an RDF Stream (constructor)."""
    global _debug    
    if _debug:
      print "Creating RDF.Stream for object",object, \
        "creator",creator,"free_statements",free_statements

    self.stream=object;

    # Keep around a reference to the object that created the stream
    # so that Python does not destroy them before us.
    self.creator=creator;

    # should the resulting statements be freed?
    self.free_statements=free_statements;

  def __del__(self):
    global _debug    
    if _debug:
      print "Destroying RDF.Stream"
    Redland.librdf_free_stream(self.stream)

  def end (self):
    if not self.stream:
      return 1
    return Redland.librdf_stream_end(self.stream)

  def next (self):
    if not self.stream:
      return None

    # return a new statement created by the librdf stream object
    my_statement=Redland.librdf_stream_next(self.stream)
    if my_statement == "NULL" or my_statement == None:
      return None
    return Statement(from_object=my_statement,
                     free_statements=self.free_statements)

# end class Stream


class Storage:

  def __init__(self, **args):
    """Create an RDF Storage (constructor)."""
    global _world
    global _debug    
    if _debug:
      print "Creating RDF.Storage args=",args
    self._storage=None

    if (args.has_key('storage_name') and
        args.has_key('name') and 
        args.has_key('options_string')):
      self._storage=Redland.librdf_new_storage(_world._world,
          args['storage_name'], args['name'], args['options_string']);
    elif args.has_key('storage'):
      self._storage=Redland.librdf_new_storage_from_storage(
          args['storage']._storage);
    else:
      raise "new RDF.Storage failed - illegal arguments"

    if self._storage == "NULL" or self._storage == None:
      self._storage=None
      raise "new RDF.Storage failed"

  def __del__(self):
    global _debug    
    if _debug:
      print "Destroying RDF.Storage"
    if self._storage:
      Redland.librdf_free_storage(self._storage)

# end class Storage


class Uri:

  def __init__(self, **args):
    """Create an RDF URI (constructor)."""
    global _world
    global _debug    
    if _debug:
      print "Creating RDF.Uri args=",args
    self._reduri=None

    if args.has_key('string') and args['string']!=None:
        self._reduri=Redland.librdf_new_uri(_world._world, args['string'])
    elif args.has_key('uri'):
        self._reduri=Redland.librdf_new_uri_from_uri(args['uri']._reduri)
    elif args.has_key('from_object'):
        if args['from_object']!=None:
            self._reduri=Redland.librdf_new_uri_from_uri(args['from_object'])
        else:
            raise 'RDF.py: attempt to create new URI from null URI'

  def __del__(self):
    global _debug    
    if _debug:
      print "Destroying RDF.Uri"
    if self._reduri:
      if _debug:
        print "Deleting Redland uri object"
      Redland.librdf_free_uri(self._reduri)

  def __str__(self):
    """Get a string representation of an RDF URI."""
    return Redland.librdf_uri_to_string(self._reduri)

  def __hash__(self):
    return hash(self._reduri)
  
  def __eq__(self,other):
    """Equality of RDF URI to another RDF URI."""
    if type(other)==type(None):
        if type(self)==type(None):
            return 1
        else:
            return 0
    return (Redland.librdf_uri_equals(self._reduri, other._reduri) != 0)

  def __ne__(self,other):
    """Inequality of RDF URI to another RDF URI."""
    if type(other)==type(None):
        if type(self)==type(None):
            return 0
        else:
            return 1
    return (Redland.librdf_uri_equals(self._reduri, other._reduri) == 0)

# end class Uri


class Parser:

  def __init__(self, name="", mime_type="application/rdf+xml", uri=None):
    """Create an RDF Parser (constructor)."""
    global _world
    global _debug    
    if _debug:
      print "Creating RDF.Parser name=",name,"mime_type=",mime_type, \
        "uri=",uri

    if uri!=None:
        uri=uri._reduri

    self._parser=Redland.librdf_new_parser(_world._world, name,
        mime_type, uri)

  def __del__(self):
    global _debug    
    if _debug:
      print "Destroying RDF.Parser"
    if self._parser:
      Redland.librdf_free_parser(self._parser)

  def parse_as_stream (self, uri, base_uri=None):
    if base_uri==None:
        base_uri=uri
    my_stream=Redland.librdf_parser_parse_as_stream(self._parser,
        uri._reduri, base_uri._reduri)
    return Stream(my_stream, self, 1)

  def parse_into_model (self, model, uri, base_uri=None):
    if base_uri==None:
        base_uri=uri
    return Redland.librdf_parser_parse_into_model(self._parser,
      uri._reduri, base_uri._reduri, model._model)

  def get_feature(self, uri):
    if not isinstance(uri, Uri):
      uri=Uri(string=uri)
    return Redland.librdf_parser_get_feature(self._parser,uri._reduri)

  def set_feature(self, uri, value):
    if not isinstance(uri, Uri):
      uri=Uri(string=uri)
    Redland.librdf_parser_set_feature(self._parser,uri._reduri,value)

# end class Parser

# global init, create our world.

_world=World()
