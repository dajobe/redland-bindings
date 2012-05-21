# 
# RDF.py - Redland Python RDF module
#
# Copyright (C) 2000-2011 David Beckett - http://www.dajobe.org/
# Copyright (C) 2000-2005 University of Bristol - http://www.bristol.ac.uk/
# 
# This package is Free Software and part of Redland http://librdf.org/
# 
# It is licensed under the following three licenses as alternatives:
#   1. GNU Lesser General Public License (LGPL) V2.1 or any newer version
#   2. GNU General Public License (GPL) V2 or any newer version
#   3. Apache License, V2.0 or any newer version
#
# This one file RDF.py is also available under the following license:
#
#   4. BSD License without advertising (aka MIT license)
#      from http://www.opensource.org/licenses/mit-license.html
#
#      -------------------------------------------------------------------
#      Permission is hereby granted, free of charge, to any person
#      obtaining a copy of this software and associated documentation
#      files (the "Software"), to deal in the Software without
#      restriction, including without limitation the rights to use, copy,
#      modify, merge, publish, distribute, sublicense, and/or sell copies
#      of the Software, and to permit persons to whom the Software is
#      furnished to do so, subject to the following conditions:
#      
#      The above copyright notice and this permission notice shall be
#      included in all copies or substantial portions of the Software.
#      
#      THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
#      EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
#      MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
#      NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
#      BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
#      ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
#      CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
#      SOFTWARE.
#     --------------------------------------------------------------------
#
# You may not use this file except in compliance with at least one of
# the above four licenses.
# 
# See LICENSE.html or LICENSE.txt at the top of this package for the
# full license terms.
#
#

from __future__ import generators

# TODO:
# * express failure conditions with Exceptions -- create a class hierarchy
#   of exceptions for Redland
# * rename modules: Redland as _Redland, RDF as Redland


"""Redland Python API

  import RDF

  storage=RDF.Storage(...)
  model=RDF.Model(storage)

  ... do stuff

The Python interface to the Redland RDF library.  See
  http://librdf.org/
for full details.

The main class that is used is Model which represents the RDF graph
formed from triples or Statements.  These statements consist of
three Node objects for resource or literals and can be stored in
a Storage (persistent or in-memory) as well as serialized to/from
syntaxes via the Serializer or Parser classes.  The Query class
provides a way to use a query language with a Model, returning
answers in QueryResults objects.

"""

import sys
import string

__all__ = [ "Node",
            "Statement",
            "Model",
            "Iterator",
            "Serializer",
            "NTriplesSerializer",
            "RDFXMLSerializer",
            "Stream",
            "Storage",
            "MemoryStorage",
            "HashStorage",
            "Uri",
            "Parser",
            "TurtleParser",
            "NTriplesParser",
            "NS",
            "Query",
            "RDQLQuery",
            "SPARQLQuery",
            "debug"]

__version__ = "1.1"

# For pydoc
__date__ = '$Date$'
__author__ = 'Dave Beckett - http://www.dajobe.org/, Edd Dumbill <edd@usefulinc.com> and Matt Biddulph <mb@picdiary.com>'

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
    'NODE_TYPE_LITERAL'   : 2,
    'NODE_TYPE_BLANK'     : 4}

import Redland


class RedlandError(Exception):
  """Redland Runtime errors"""
  def __init__(self, value):
    self.value = value

  def __str__(self):
    return `self.value`

class NodeTypeError(RedlandError):
  pass

class RedlandWarning(Warning):
  pass

def node_type(name):
  """Return the Redland node type of a node name"""
  if _node_types.has_key(name):
    return _node_types[name]
  else:
    raise NodeTypeError('Unknown node type %s' % name)

def node_type_name(num):
  """Return the name of a Redland node type"""
  for n in _node_types.keys():
    if num==_node_types[n]:
      return n
  raise NodeTypeError('Unknown node type number %d' % num)

def message_handler (type, message):
  raise RedlandError("Obsolete method")

def set_message_handler(handler):
  raise RedlandError("Obsolete method")

class World(object):
  """Redland Initialisation class.

  There are no user methods (can only be constructed).

  """

  def __init__(self,digest_name="",uri_hash=None):
    """Create new RDF World object (constructor)"""
    #trick to ensure function exists during module cleanup
    self._cleanup = Redland.librdf_free_world

    self._world = None

    self._world = Redland.librdf_new_world()
    Redland.librdf_world_open(self._world)

    Redland.librdf_python_world_init(self._world)

  def __del__(self):
    """Destroy RDF World object (destructor)."""
    global _debug    
    if self._world:
      if _debug:
        print "Destroying RDF.World"
      self._cleanup(self._world)

# end class World


def debug(value = None):
  """Get/set Redland debugging output status.

  RDF.debug (1)   # enable debugging
  if RDF.debug(): # test for debug mode
  """
  global _debug
  if value is not None:
      _debug = value
  else:
      return _debug


class Node(object):
  """Redland Node (RDF Resource, Property, Literal) Class

    import RDF

    node1=RDF.Node()

    node2=RDF.Node(RDF.Uri("http://example.com/"))
    node3=RDF.Node("Hello, World!")

    node4=RDF.Node(uri_string="http://example.com/")
    node5=RDF.Node(literal="<tag>content</tag>", is_wf_xml=1)
    node6=RDF.Node(blank="abc")
    node7=RDF.Node(node5)
  ...

    print node2
    if node7.is_resource():
      print "Resource with URI", node7.uri

    if node5.is_blank():
      print "Resource with blank node name ", node5.blank_identifier

  """

  def __init__(self, arg = None, **args):
    """Create an RDF Node (constructor).

    Resource or Property node creation:

      n1 = Node(Uri("http://example.com/foo"))

    String literal node creation (see below for more complex
    ways of building literals.)

      n2 = Node("foo")

    Node copying:

      n3 = Node(n1)

    Or create a new RDF Node using the following named parameters:

      uri_string  - create a resource node from a string URI
      uri         - create a resource node from a URI object
      literal     - create a literal node from a literal string   
        datatype     - the datatype URI
        is_wf_xml    - the literal is XML (alternative to datatype)
        language     - the literal XML language
      blank       - create a resource node from with a blank node identiifer
      node        - copy a node
    """

    global _world
    global _debug
    if _debug:
      print "Creating RDF.Node args=",args
    self._node=None

    if arg is not None:
      if isinstance(arg, str):
        args['literal'] = arg
      elif isinstance(arg, unicode):
        import Redland_python
        args['literal'] = Redland_python.unicode_to_bytes(arg)
      elif isinstance(arg, Uri):
        args['uri'] = arg
      elif isinstance(arg, Node):
        args['node'] = arg

    if args.has_key('literal') and isinstance(args['literal'], unicode):
        import Redland_python
        args['literal'] = Redland_python.unicode_to_bytes(args['literal'])

    if args.has_key('uri_string'):
      self._node=Redland.librdf_new_node_from_uri_string(_world._world,
        args['uri_string'])

    elif args.has_key('uri'):
      # no need to copy the underlying uri as the redland C api does
      # this on node construction
      self._node = Redland.librdf_new_node_from_uri(_world._world,
              args['uri']._reduri)

    elif args.has_key('literal'):
      if args.has_key('xml_language'):
        xml_language=args['xml_language']
      elif args.has_key('language'):
        xml_language=args['language']
      else:
        xml_language=None
      if args.has_key('is_wf_xml'):
        is_wf_xml=args['is_wf_xml']
      else:
        is_wf_xml=0
      if args.has_key('datatype'):
        datatype=args['datatype']
        self._node=Redland.librdf_new_node_from_typed_literal(_world._world,
          args['literal'], xml_language, datatype._reduri)
      else:
        self._node=Redland.librdf_new_node_from_literal(_world._world,
          args['literal'], xml_language, is_wf_xml)

    elif args.has_key('node'):
      self._node=Redland.librdf_new_node_from_node(args['node']._node)

    elif args.has_key('blank'):
      self._node=Redland.librdf_new_node_from_blank_identifier(_world._world, args['blank'])

    elif args.has_key('from_object'):
      if args.has_key('do_not_copy'):
        self._node=args['from_object']
      else:
        self._node=Redland.librdf_new_node_from_node(args['from_object'])
    else:
      self._node=Redland.librdf_new_node(_world._world)

    if self._node is None:
        raise RedlandError("Node construction failed")

  def __del__(self):
    """Free an RDF Node (destructor)."""
    global _debug    
    if _debug:
      print "Destroying RDF.Node"
    if self._node:
      if _debug:
        print "Deleting Redland node object"
      Redland.librdf_free_node(self._node)

  def _get_uri(self):
    # we return a copy of our internal uri
    if self.is_resource():
      return Uri(from_object=Redland.librdf_node_get_uri(self._node))
    else:
      raise NodeTypeError("Can't get URI for node type %s (%d)" % \
                          (node_type_name(self.type), self.type))

  def _get_type(self):
    return Redland.librdf_node_get_type(self._node)

  uri = property(_get_uri, doc = "The URI of a resource node")
  type = property(_get_type, doc = "The node type, an integer")

  def _get_literal (self):
    if not self.is_literal():
      raise NodeTypeError("Can't get literal value for node type %s (%d)" % \
            (node_type_name(self.type), self.type))

    dt_uri = Redland.librdf_node_get_literal_value_datatype_uri(self._node)

    if dt_uri:
      dt_uri = Uri(string=Redland.librdf_uri_to_string(dt_uri))

    return (unicode(Redland.librdf_node_get_literal_value(self._node), 'utf-8'),
            Redland.librdf_node_get_literal_value_language(self._node),
            dt_uri)

  def _get_literal_value (self):
    if not self.is_literal():
      raise NodeTypeError("Can't get literal value for node type %s (%d)" % \
            (node_type_name(self.type), self.type))

    dt_uri = Redland.librdf_node_get_literal_value_datatype_uri(self._node)

    if dt_uri:
      dt_uri = Uri(string=Redland.librdf_uri_to_string(dt_uri))

    val={
        'string': unicode(Redland.librdf_node_get_literal_value(self._node), 'utf-8'),
        'language': Redland.librdf_node_get_literal_value_language(self._node),
        'datatype': dt_uri
        }
    return val

  literal = property(_get_literal,
          doc = "A tuple of the string, language and datatype values of the node")

  literal_value = property(_get_literal_value,
          doc = "A dictionary containing the value of the node literal with keys string, language and datatype")

  def _get_blank_identifier(self):
    if not self.is_blank():
      raise NodeTypeError("Can't get blank identifier for node type %s (%d)" % \
            (node_type_name(self.type), self.type))
    else:
      return unicode(Redland.librdf_node_get_blank_identifier(self._node), 'utf-8')

  blank = property(_get_blank_identifier, 
          doc = "The blank node identifier")

  blank_identifier = property(_get_blank_identifier, 
          doc = "The node identifier of a blank node")

  def __str__(self):
    """Get a string representation of an RDF Node."""
    return unicode(self).encode('utf-8')

  def __unicode__(self):
    """Get a Unicode string representation of an RDF Node."""
    if self._node is None:
      raise RedlandError("Node is invalid") 
    elif self.is_literal():
      return unicode(Redland.librdf_node_get_literal_value(self._node), 'utf-8')
    elif self.is_blank():
      return self._get_blank_identifier()
    else:
      return unicode(self.uri)

  def __eq__(self,other):
    """Equality of an RDF Node compared to another RDF Node."""
    if not isinstance(other, self.__class__):
      return False
    return (Redland.librdf_node_equals(self._node, other._node) != 0)

  def __ne__(self,other):
    """Inequality of an RDF Node compared to another RDF Node."""
    return not self == other

  def __hash__(self):
    return hash(str(self))

  def is_resource(self):
    """Return true if node is a resource  with a URI"""   
    return (Redland.librdf_node_is_resource(self._node) != 0)

  def is_literal(self):
    """Return true if node is a literal"""
    return (Redland.librdf_node_is_literal(self._node) != 0)
  
  def is_blank(self):
    """Return true if node is a blank node"""   
    return (Redland.librdf_node_is_blank(self._node) != 0)
  
# end class Node


class Statement(object):
  """Redland Statement (triple) class.  The main means of manipulating
  statements is by the subject, predicate and object properties.

    import RDF
    statement1 = RDF.Statement(node1, node2, node3)
    statement2 = RDF.Statement(statement = statement1)

    if statement2.subject.is_resource():
      print "statement2 subject is URI ",statement2.subject.uri

    statement.object = Node("hello, world")
  """

  def __init__(self, subject = None, predicate = None,
          object = None, **args):
    """Constructor for Statement.

    Create a Statement from three Node objects.

        s1 = RDF.Statement(subjnode, prednode, objnode)

    A Node argument can be replaced with Uri or string to
    shortcut Node creation.

        s2 = RDF.Statement(Uri("http://foo"), Uri("http://bar"), "baz")

    Copy an existing Statement s1.

        s3 = RDF.Statement(statement=s1)
    """
    global _world
    global _debug    
    if _debug:
      print "Creating RDF.Statement subject=",subject,"predicate=",predicate,"object=",object,"args=",args

    self._statement = None

    if args.has_key('statement'):
      self._statement=Redland.librdf_new_statement_from_statement(
          args['statement']._statement)

    elif args.has_key('from_object'):
      self._statement = Redland.librdf_new_statement_from_statement(
          args['from_object'])

    else:
      if subject is None:
        s = None
      else:
        if isinstance(subject, Uri) or isinstance(subject, str):
          subject = Node(subject)
        elif isinstance(subject, unicode):
          import Redland_python
          subject = Node(Redland_python.unicode_to_bytes(subject))
        s = Redland.librdf_new_node_from_node(subject._node)

      if predicate is None:
        p = None
      else:
        if isinstance(predicate, Uri) or isinstance(predicate, str):
          predicate = Node(predicate)
        elif isinstance(predicate, unicode):
          import Redland_python
          predicate = Node(Redland_python.unicode_to_bytes(predicate))
        p = Redland.librdf_new_node_from_node(predicate._node)

      if object is None:
        o = None
      else:
        if isinstance(object, Uri) or isinstance(object, str):
          object = Node(object)
        elif isinstance(object, unicode):
          import Redland_python
          object = Node(Redland_python.unicode_to_bytes(object))
        o = Redland.librdf_new_node_from_node(object._node)

      self._statement=Redland.librdf_new_statement_from_nodes(
          _world._world, s, p, o)

    if self._statement is None:
        raise RedlandError("Statement construction failed")

  def __del__(self):
    global _debug    
    if _debug:
      print "Destroying RDF.Statement"
    if self._statement:
      if _debug:
        print "Deleting Redland statement object"
      Redland.librdf_free_statement(self._statement)

  def _wrap_node(self, rednode):
    if rednode is None:
      return None
    return Node(from_object=rednode)

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
    if value is None:
      Redland.librdf_statement_set_subject(self._statement, None)
    else:
      Redland.librdf_statement_set_subject(self._statement,
          Redland.librdf_new_node_from_node(value._node))

  def _set_object(self, value):
    if value is None:
      Redland.librdf_statement_set_object(self._statement, None)
    else:
      Redland.librdf_statement_set_object(self._statement,
          Redland.librdf_new_node_from_node(value._node))

  def _set_predicate(self, value):
    if value is None:
      Redland.librdf_statement_set_predicate(self._statement, None)
    else:
      Redland.librdf_statement_set_predicate(self._statement,
          Redland.librdf_new_node_from_node(value._node))

  object = property(_get_object, _set_object, 
          doc = "The object node of the statement.")
  subject = property(_get_subject, _set_subject, 
          doc = "The subject node of the statement.")
  predicate = property(_get_predicate, _set_predicate,
          doc = "The predicate node of the statement.")
  
  def __str__(self):
    if self._statement is None:
      raise RedlandError("Statement is invalid")
    else:
      return Redland.librdf_statement_to_string(self._statement)

  def __unicode__(self):
    if self._statement is None:
      raise RedlandError("Statement is invalid")
    else:
      return unicode(Redland.librdf_statement_to_string(self._statement), 'utf-8')

  def __eq__(self,other):
    """Equality of an RDF Statement compared to another RDF Statement."""
    if not isinstance(other, self.__class__):
      return False
    return (Redland.librdf_statement_equals(self._statement, other._statement) != 0)

  def __ne__(self,other):
    """Inequality of an RDF Statement compared to another RDF Statement."""
    return not self == other

  def matches(self,other):
    """Comparison of this potentially incomplete RDF Statement compared to another RDF Statement."""
    if other is None:
      return (self is None)
    return (Redland.librdf_statement_match(other._statement, self._statement) != 0)
 
# end class Statement


class Model(object):
  """Redland Graph class

    import RDF
    model = RDF.Model(storage)

  The main interface to the Redland RDF graph (formed from triples, or
  RDF statements).  There are many methods for adding, removing, querying
  statements and serializing them to/from syntaxes using the Serializer
  or Parser classes.

  Models can also be used as Python sequences to give every triple in the
  model:

    for statement in model:
      print statement

  Models have other aspects of sequence types.  The following also works:

    if statement in model:            # do whatever
    if (statement, context) in model: # do whatever

    del model[statement]              # remove statement from model
    del model[statement, context]     # ditto for context-aware model

    model.append(statement)           # append a statement
    model.append(statement, context)  # append statement with context

    num_items = len(model) # get number of statements in the model
                           # works only with countable storages
    

  """

  def __init__(self, storage = None, **args):
    """Create an RDF Model (constructor).

Create a Model from an existing Storage (most common use).  

Optional parameters:

  options_string - A string of options for the Model
  options_hash   - A Hash of options for the Model

  m1 = RDF.Model(s1)
  m1 = RDF.Model(storage = s1)

Copy an existing model m1, copying the underlying Storage of m1

  m2 = RDF.Model(model = m1)

Create a model using an in-memory storage.

  m3 = RDF.Model()
    """
    global _world
    global _debug    
    if _debug:
      print "Creating RDF.Model args=",args
    self._model=None
    self._storage=None

    if storage is None:
      storage = MemoryStorage()

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

    if self._model is None or self._model == "NULL":
      self._model=None
      raise RedlandError("Creating new Model failed")
    else:
      # keep a reference around so storage object is destroyed after this
      self._storage=storage

  def __iter__(self):
    return self.as_stream().__iter__()

  def __del__(self):
    global _debug    
    if _debug:
      print "Destroying RDF.Model"
    if self._model:
      Redland.librdf_free_model(self._model)

  def __len__(self):
    s = self.size()
    if s < 0:
      raise RedlandError("Attempt to get size when using non-countable storage.")
    return s

  def size(self):
    """Return the size of the Model in number of statements.
       Returns a value < 0 if number of statements not countable."""
    return Redland.librdf_model_size(self._model)

  def add(self,subject,predicate,object):
    """Add the statement (subject,predicate,object) to the model.
    DEPRECATED. Use Model.append(Statement(s,p,o)) instead."""
    # the reason to deprecate this method is that the Node constructors
    # will do a lot more checking.  This wanton calling of underlying
    # C methods is a recipe for unexplained core dumps if any of the
    # nodes are null or invalid.
    return Redland.librdf_model_add(self._model, 
        Redland.librdf_new_node_from_node(subject._node),
        Redland.librdf_new_node_from_node(predicate._node),
        Redland.librdf_new_node_from_node(object._node))

  def add_typed_literal_statement(self,subject,predicate,
                                  string,xml_language=None,datatype=None):
    """Add the Statement (subject,predicate, typed literal) to the Model
       where the typed literal is constructed from the
       literal string, optional XML language and optional datatype URI.
       DEPRECATED. Use Model.append(Statement(s,p,o)) instead."""
    # the reason to deprecate this method is that the Node constructors
    # will do a lot more checking.  This wanton calling of underlying
    # C methods is a recipe for unexplained core dumps if any of the
    # nodes are null or invalid.
    if datatype is not None:
      rdatatype = datatype._reduri
    else:
      rdatatype = None
    subject_copy = Redland.librdf_new_node_from_node(subject._node)
    predicate_copy = Redland.librdf_new_node_from_node(predicate._node)
    return Redland.librdf_model_add_typed_literal_statement(self._model,
        subject_copy, predicate_copy, string, xml_language, rdatatype)

  def add_statement(self,statement,context=None):
    """Add the Statement to the Model with optional context Node.
    For Python idiom you should use Model.append() instead, which does
    the same thing."""
    # adding a statement means it gets *copied* into the model
    # we are free to re-use the statement after adding it
    if context is not None:
      rc = Redland.librdf_model_context_add_statement(self._model,
              context._node, statement._statement)
    else:
      rc = Redland.librdf_model_add_statement(self._model,
                                                statement._statement)
    if rc != 0:
      raise RedlandError("Adding statement failed")

  def add_statements(self,statement_stream,context=None):
    """Add the Stream of Statements to the Model with the optional
    context Node"""
    if context is not None:
      return Redland.librdf_model_context_add_statements(self._model,
              context._node, statement_stream._stream)
    else:
      return Redland.librdf_model_add_statements(self._model,
                                                 statement_stream._stream)

  def append(self, statement, context = None):
      """Append a Statement to the Model, with optional context Node.
      
        model.append(Statement(s, p, o)"""
      self.add_statement(statement, context)

  def remove_statement(self, statement, context = None):
    """Remove the Statement from the Model with the optional context Node.
    This is used by the __delitem__ method.  Preferred way of removing a
    Statement is:
        
        del model[statement]
        del model[statement, context]
    """
    if context is not None:
      return Redland.librdf_model_context_remove_statement(self._model,
              context._node, statement._statement)
    else:
      return Redland.librdf_model_remove_statement(self._model,
                                                   statement._statement)

  def __delitem__(self, arg):
    if isinstance(arg, tuple):
      try:
        (s, c) = arg
        self.remove_statement(s, c)
      except ValueError:
        raise TypeError("can only del statement or (statement,context) tuple")
    else:
        self.remove_statement(arg)

  def remove_statements_with_context(self, context):
    """Remove all Statements from the Model with the given context Node"""
    return Redland.librdf_model_context_remove_statements(self._model,
                                                          context._node)

  context_remove_statements = remove_statements_with_context

  def contains_statement(self, statement):
    """Return true if the Statement is in the Model"""
    return Redland.librdf_model_contains_statement(self._model,
        statement._statement)

  def contains_statement_context(self, statement, context):
    """Return true if the Statement is in the Model with the specified
    context.  Note that the implementation is pretty inefficient."""
    for (s, c) in self.find_statements_context(statement):
      if c == context:
        return 1
    return 0

  def __contains__(self, arg):
    # provided here for efficiency, otherwise Python
    # would iterate through the contents of the model
    if isinstance(arg, tuple):
      try:
        (s, c) = arg
        return self.contains_statement_context(s, c)
      except ValueError:
        raise TypeError("requires statement or (statement,context) tuple")
    else:
      return self.contains_statement(arg)


  def as_stream(self, context = None):
    """Return the Model as a Stream of Statements.  No need to use
    this explicitly, instead do:
        
        for statement in model:
            # process statement
    """
    if context is None:
      my_stream = Redland.librdf_model_as_stream(self._model)
    else:
      my_stream = Redland.librdf_model_context_as_stream(self._model,
                                                         context._node)
    return Stream(my_stream, self)

  serialise = as_stream

  def as_stream_context(self, context = None):
    """Return the Model as a Stream of (statement, context) tuples.
        
        for (s, c) in model.as_stream_context():
            # do whatever

       Specify the optional argument context if you want to hardwire
       the stream's context.
    """
    return StreamWithContextIter(self.as_stream(context))

  def find_statements(self,statement, context = None):
    """Return a Stream of Statements matching the given Statement --
       any nodes with value None of the statement match any Node in
       the Model.
       
       Specify the optional argument context if you want to search
       only in one context.

       qs = RDF.Statement(subject = None,
           predicate = RDF.Node(uri_string = "http://example.com/pred"),
           object = None)
       for statement in model.find_statements(qs):
           # do whatever
       
    """
    if context is None:
      my_stream = Redland.librdf_model_find_statements(self._model,
                                                     statement._statement)
    else:
      my_stream = Redland.librdf_model_find_statements_in_context(self._model,
                                                                  statement._statement,
                                                                  context._node)
    return Stream(my_stream, self)

  def find_statements_context(self,statement):
    """Return a Stream of Statements with context, matching the given
       Statement -- any nodes with value None of the statement match
       any Node in the Model.
       
       qs = RDF.Statement(subject = None,
           predicate = RDF.Node(uri_string = "http://example.com/pred"),
           object = None)
       for (statement, context) in model.find_statements_context(qs):
           # do whatever
     """
    return StreamWithContextIter(self.find_statements(statement))

  def get_sources(self, predicate, target):
    """Return a sequence of Node s that are the source
       of Statements in the Model matching (?, predicate, target).

       Instead of specifying a Node for predicate, you can shortcut with
       a Uri, and with a Uri or string for target.

       e.g.
         model.get_sources(Uri("http://example.com/name"), "Fred")
    """
    if isinstance(predicate, Uri):
      predicate = Node(predicate)
    if isinstance(target, Uri) or isinstance(target, str):
      target = Node(target)
    elif isinstance(target, unicode):
      import Redland_python
      target = Node(Redland_python.unicode_to_bytes(target))

    my_iterator = Redland.librdf_model_get_sources(self._model,
                                                   predicate._node,
                                                   target._node)
    if my_iterator is None:
      raise RedlandError("Unable to create iterator")

    return Iterator(my_iterator, self, predicate, target)

  sources = get_sources

  def get_sources_context(self, predicate, target):
    """As for Model.get_sources but returns a list of 
    (statement, context) tuples."""
    return IteratorWithContextIter(self.get_sources(predicate, target))

  def get_predicates(self,source,target):
    """Return a sequence of Nodes that are the predicates
       of Statements in the Model matching (source, ?, target).
       
       Instead of specifying a Node for source, you can shortcut with
       a Uri, and with a Uri or string for target.
       
       e.g.
         model.get_predicates(Uri("http://example.com/me"), "Fred")
    """
    if isinstance(source, Uri):
      source = Node(source)
    if isinstance(target, Uri) or isinstance(target, str):
      target = Node(target)
    elif isinstance(target, unicode):
      import Redland_python
      target = Node(Redland_python.unicode_to_bytes(target))

    my_iterator=Redland.librdf_model_get_arcs(self._model, source._node,
                                              target._node)
    if my_iterator is None:
      raise RedlandError("Unable to create iterator")

    return Iterator(my_iterator,self,source,target)

  arcs = get_predicates
  get_arcs = get_predicates
  predicates = get_predicates

  def get_predicates_context(self, source, target):
    """As for Model.get_predicates but returns a list of 
    (statement, context) tuples."""
    return IteratorWithContextIter(self.get_predicates(source, target))

  def get_targets(self, source, predicate):
    """Return a sequence of Nodes that are the targets
       of Statements in the Model matching (source, predicate, ?).
       
       Instead of specifying a Node for source or predicate, you
       can shortcut with a Uri.
       
       e.g.

         model.get_targets(Uri("http://example.com/me"), prednode)
    """
    if isinstance(source, Uri):
      source = Node(source)
    if isinstance(predicate, Uri):
      predicate = Node(predicate)

    my_iterator = Redland.librdf_model_get_targets(self._model, source._node,
                                                   predicate._node)
    if my_iterator is None:
      raise RedlandError("Unable to create iterator")

    return Iterator(my_iterator,self,source,predicate)

  targets = get_targets

  def get_targets_context(self, source, predicate):
    """As for Model.get_targets but returns a list of 
    (statement, context) tuples."""
    return IteratorWithContextIter(self.get_targets(source, predicate))

  def get_source(self, predicate, target):
    """Return one Node in the Model matching (?, predicate, target).
    The predicate can be a Node or Uri, the target a Node, Uri or string."""
    if isinstance(predicate, Uri):
      predicate = Node(predicate)
    if isinstance(target, Uri) or isinstance(target, str):
      target = Node(target)
    elif isinstance(target, unicode):
      import Redland_python
      target = Node(Redland_python.unicode_to_bytes(target))

    my_node=Redland.librdf_model_get_source(self._model, predicate._node,
                                            target._node)
    if not my_node:
      return None
    else:
      return Node(from_object=my_node, do_not_copy=1)

  def get_predicate(self,source,target):
    """Return one Node in the Model matching (source, ?, target).
    The source can be a Node or Uri, the target a Node, Uri or string."""
    if isinstance(source, Uri):
      source = Node(source)
    if isinstance(target, Uri) or isinstance(target, str):
      target = Node(target)
    elif isinstance(target, unicode):
      import Redland_python
      target = Node(Redland_python.unicode_to_bytes(target))

    my_node=Redland.librdf_model_get_arc(self._model, source._node,
                                         target._node)
    if not my_node:
      return None
    else:
      return Node(from_object=my_node, do_not_copy=1)

  get_arc = get_predicate

  def get_target(self, source, predicate):
    """Return one Node in the Model matching (source, predicate, ?).
    The source and predicate can be a Node or Uri."""
    if isinstance(source, Uri):
      source = Node(source)
    if isinstance(predicate, Uri):
      predicate = Node(predicate)

    my_node=Redland.librdf_model_get_target(self._model, source._node,
                                            predicate._node)
    if not my_node:
      return None
    else:
      return Node(from_object=my_node, do_not_copy=1)

  def sync(self):
    """Synchronise the Model with the underlying Storage."""
    Redland.librdf_model_sync(self._model)

  def get_contexts(self):
    """Return a sequence of context Nodes in the Model."""

    my_iterator = Redland.librdf_model_get_contexts(self._model)
    if my_iterator is None:
      raise RedlandError("Unable to create iterator")

    return Iterator(my_iterator,self)

  def get_feature(self, uri):
    """Return the Node value of Model feature URI uri"""
    if not isinstance(uri, Uri):
      uri=Uri(string=uri)

    value=Redland.librdf_model_get_feature(self._model,uri._reduri)
    if value == "NULL" or value == None:
      return None
    return Node(from_object=value)

  def set_feature(self, uri, value):
    """Set the Node value of Model feature URI uri."""
    if not isinstance(uri, Uri):
      uri=Uri(string=uri)
    if not isinstance(value, Node):
      value=Node(literal=value)
    Redland.librdf_model_set_feature(self._model,uri._reduri,value._node)

  def load(self, uri, name="", mime_type="", type_uri=None, handler=None):
    """Load triples into the Model from a URI in a syntax.
       Returns a boolean success or failure.

       If no parser name is given, the parser to use is guessed.

       If handler is given, an error handler with the signature
         def handler(code, level, facility, message, line, column, byte, file, uri)
       is called.
   """
    if isinstance(uri, str):
      uri = Uri(string=uri)
    elif isinstance(uri, unicode):
      import Redland_python
      uri = Uri(string=Redland_python.unicode_to_bytes(uri))
    if uri is None:
      raise TypeError("uri must be a string or RDF.Uri")
    ruri = uri._reduri

    if isinstance(type_uri, str):
      type_uri = Uri(string=type_uri)
    elif isinstance(type_uri, unicode):
      import Redland_python
      type_uri = Uri(string=Redland_python.unicode_to_bytes(type_uri))
    if type_uri is not None:
      rtype_uri = type_uri._reduri
    else:
      rtype_uri = None

    if handler is not None:
      import Redland_python
      Redland_python.set_callback(handler)

    rc=Redland.librdf_model_load(self._model, ruri, name, mime_type, rtype_uri)

    if handler is not None:
      import Redland_python
      Redland_python.reset_callback()

    return (rc != None)

  def to_string(self, base_uri=None, name="", mime_type="", type_uri=None):
    """Serialize the Model to a syntax.

       print model.to_string(base_uri="http://example.org/base")

       If no serializer name is given, the default serializer RDF/XML is used.
   """
    if isinstance(base_uri, str):
      base_uri = Uri(string=base_uri)
    elif isinstance(base_uri, unicode):
      import Redland_python
      base_uri = Uri(string=Redland_python.unicode_to_bytes(base_uri))
    if base_uri is not None:
      rbase_uri = base_uri._reduri
    else:
      rbase_uri = None
    if isinstance(type_uri, str):
      type_uri = Uri(string=type_uri)
    elif isinstance(type_uri, unicode):
      import Redland_python
      type_uri = Uri(string=Redland_python.unicode_to_bytes(type_uri))
    if type_uri is not None:
      rtype_uri = type_uri._reduri
    else:
      rtype_uri = None
    return Redland.librdf_model_to_string(self._model, rbase_uri, name, mime_type, rtype_uri)

  def __str__(self):
    return self.to_string()

  def __unicode__(self):
    return unicode(self.to_string(), 'utf-8')

  def execute(self,query):
    results = Redland.librdf_model_query_execute(self._model,query._query)
    if results is not None:
      return QueryResults(query._query,results)
    else:
      return None

  def run_as_statements(self,query):
    results = Redland.librdf_model_query_execute(self._model,query._query)
    if results is not None:
      s=Redland.librdf_query_results_as_stream(results)
      return Stream(s,results)
    else:
      return None


# end class Model


class Iterator(object):
  """Redland Node Iterator class

     A class for iterating over a sequence of Node s such as
     those returned from a Model query.  Some methods return
     Iterator s or Python sequences.  If this is used, it works
     as follows:

       iterator=model.get_targets_iterator(source, arc)
       while not iterator.end():
         # get the current Node
         node=iterator.current()
         # do something with it
         # (it is shared; you must copy it you want to keep it)
         ...
         iterator.next()
       iterator=None

  """

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

  def __iter__(self):
    return IteratorIter(self)  

  def __del__(self):
    global _debug    
    if _debug:
      print "Destroying RDF.Iterator"
    if self._iterator:
      Redland.librdf_free_iterator(self._iterator)

  def end(self):
    """Return true if the iterator is exhausted"""
    return Redland.librdf_iterator_end(self._iterator)

  def have_elements(self):
    print """RDF.Iterator method have_elements is deprecated,
please use 'not iterator.end' instead."""
    return Redland.librdf_iterator_have_elements(self._iterator)

  def current(self):
    """Return the current object on the Iterator"""
    my_node=Redland.librdf_iterator_get_object(self._iterator)
    if my_node == "NULL" or my_node == None:
      return None

    return Node(from_object=my_node)

  def next(self):
    """Move to the next object on the Iterator"""
    Redland.librdf_iterator_next(self._iterator)

  def context(self):
    """Return the context Node of the current object on the Iterator"""
    my_node=Redland.librdf_iterator_get_context(self._iterator)
    if my_node == "NULL" or my_node == None:
      return None

    return Node(from_object=my_node)

# end class Iterator

class StreamWithContextIter(object):
  def __init__(self,stream):
    global _debug
    if _debug:
      print "Creating StreamWithContextIter for Stream "+repr(stream)  
    self.stream = stream
    self.first = 1

  def __iter__(self):
    return self

  def next(self):
    if self.first:
      self.first = 0
    else:
      self.stream.next()
    if self.stream is None or self.stream.end():
      raise StopIteration
    return (self.stream.current(), self.stream.context())

class IteratorWithContextIter(object):
  def __init__(self,iterator):
    global _debug
    if _debug:
      print "Creating IteratorWithContextIter for Iterator "+repr(iterator)  
    self.iterator = iterator
    self.first = 1

  def __iter__(self):
    return self

  def next(self):
    if self.first:
      self.first = 0
    else:
      self.iterator.next()
    if self.iterator is None or self.iterator.end():
      raise StopIteration
    try:
      return (self.iterator.current(), self.iterator.context())
    except AttributeError:
      return (self.iterator.current(), None)

class IteratorIter(object):
  def __init__(self,iterator):
    global _debug
    if _debug:
      print "Creating IteratorIter for Iterator "+repr(iterator)  
    self.iterator = iterator
    self.first = 1

  def __iter__(self):
    return self

  def next(self):
    if self.first:
      self.first = 0
    else:
      self.iterator.next()
    if self.iterator is None or self.iterator.end():
      raise StopIteration
    return self.iterator.current()

class StreamIter:
  def __init__(self,stream):
    global _debug
    if _debug:
      print "Creating StreamIter for Stream "+repr(stream)  
    self.stream = stream
    self.first = 1

  def __iter__(self):
    return self

  def next(self):
    if self.first:
      self.first = 0
    else:
      self.stream.next()
    if self.stream is None or self.stream.end():
      raise StopIteration
    return self.stream.current()

class Stream(object):
  """Redland Statement Stream class

     A class encapsulating a sequence of Statements, such as
     those returned from a Model query.  Can be used as a Python
     sequence.
     
     stream = model.find_statements(query_statement)
     for statement in stream:
        # do whatever with 'statement'
        # note it is shared and will go out of scope, so you must
        # copy it if you want it to stay around

     You should not normally find yourself needing to use this
     class explicitly.
   """

  def __init__(self, object, creator):
    """Create an RDF Stream (constructor)."""
    global _debug    
    if _debug:
      print "Creating RDF.Stream for object",object, "creator",creator

    self._stream=object

    # Keep around a reference to the object that created the stream
    # so that Python does not destroy them before us.
    self.creator=creator

  def context_iter(self):
    """Return an iterator over this stream that
       returns (stream, context) tuples each time it is iterated.
       DEPRECATED.  Instead use the context-aware method appropriate,
       e.g.  Model.find_statements_context() or Model.as_stream_context()
       """
    return StreamWithContextIter(self)

  def __iter__(self):
    return StreamIter(self)

  def __del__(self):
    global _debug    
    if _debug:
      print "Destroying RDF.Stream"
    if self._stream:
      Redland.librdf_free_stream(self._stream)

  def end(self):
    """Return true if the stream is exhausted"""
    if not self._stream:
      return 1
    return Redland.librdf_stream_end(self._stream)

  def current(self):
    """Return the current Statement on the Stream"""
    if not self._stream:
      return None

    my_statement=Redland.librdf_stream_get_object(self._stream)
    if my_statement == "NULL" or my_statement == None:
      return None
    return Statement(from_object=my_statement)

  def next(self):
    """Move to the next Statement on the Stream"""
    if not self._stream:
      return 1

    return Redland.librdf_stream_next(self._stream)

  def context(self):
    """Return the context Node of the current object on the Stream"""
    if not self._stream:
      return 1

    my_node=Redland.librdf_stream_get_context(self._stream)
    if my_node == "NULL" or my_node == None:
      return None

    return Node(from_object=my_node)

# end class Stream


class Storage(object):

  """Redland Statement Storage class

     import RDF
     storage=RDF.Storage(storage_name="memory")

  The Redland abstraction for storing RDF graphs as Statements.

  There are no user methods (can only be constructed).

  You should normally use a specialized class such as MemoryStorage or
  HashStorage in preference to this class.

  """

  def __init__(self, **args):
    """Create an RDF Storage (constructor).

    Create a new RDF Storage using any of these forms

  s1=RDF.Storage(storage_name="name",options_string="")

Create a Storage of the given type.  Currently the built in storage
names that are always present are 'memory', 'hashes', 'file' and
'uri'.  'bdb' is available when Sleepycat / BerkeleyDB is compiled
in, 'mysql' when MySQL is compiled in, and 'sqlite' when SQLite is
compiled in.  If storage_name is omitted, it defaults to 'memory'.

The argument 'name' can be used when the storage needs a name
to operate, such as used for a filename or URI:

  s1=RDF.Storage(storage_name="file", name='/filename',options_string="")

  s2=RDF.Storage(storage_name="uri", name='http://rdf.example.org/',options_string="")


The required argument options_string allows additional store-specific
options to be given, some of which are required by certain stores.
This uses the following form:

  s3=RDF.Storage(storage_name="name", name='abc',
                 options_string="key1='value1', key2='value2', ...")

for multiple key/value option pairs, option values are always
surrounded by single quotes.

The common options are:
  new - optional and takes a boolean value (default false)
    If true, it deletes any existing store and creates a new one
    otherwise if false (default) open an existing store.

  write - optional and takes a boolean value (default true)
    If true (default) the Storage is opened read-write otherwise
    if false the storage is opened read-only and for file-based
    Storages or those with locks, may be opened with shared-readers.

Some storage types have additional options:

storage_name 'hashes' has options:
  hash-type -  the name of any hash type supported.
    'memory' (default), 'file' hash types are always present. 'bdb' is
    available when BerkeleyDB is compiled in,
  dir - the directory name to create the files in (default '.')
  mode - the file creation mode (default 0644 octal)

storage_name 'mysql' has options:
  host - required MySQL database hostname
  port - optional MySQL database port (defaults to 3306)
  database - required MySQL database name
  user - required MySQL database user
  password - required MySQL database password

stoage name 'sqlite' has options:
  synchronous - optional value off, normal or full
      
The other form is:
  s4=RDF.Storage(storage=s1)
Copy an existing Storage s1.


Note: there are convience classes to create a memory storage
  s5=RDF.MemoryStorage()

and Hash storage:
  # memory hash
  s6=RDF.HashStorage('abc')

  # specified bdb hash stored in files named 'def'*
  s7=RDF.HashStorage('def', options="hash-type='bdb'")

    """
    global _world
    global _debug    
    if _debug:
      print "Creating RDF.Storage args=",args
    self._storage=None

    if (args.has_key('storage_name') and
        args.has_key('name') and 
        args.has_key('options_string')):
      self._storage=Redland.librdf_new_storage(_world._world,
          args['storage_name'], args['name'], args['options_string'])
    elif args.has_key('storage'):
      self._storage=Redland.librdf_new_storage_from_storage(
          args['storage']._storage)
    else:
      raise RedlandError("Creating Storage failed - missing storage_name, name or options_string arguments")

    if self._storage == "NULL" or self._storage == None:
      self._storage=None
      raise RedlandError("Creating Storage failed")

  def __del__(self):
    global _debug    
    if _debug:
      print "Destroying RDF.Storage"
    if self._storage:
      Redland.librdf_free_storage(self._storage)

# end class Storage


class HashStorage(Storage):
  """Redland Hashed Storage class

     import RDF
     h1=RDF.HashStorage("abc", options="hash-type='memory'")

     # Creating a storage with contexts enabled
     s=RDF.HashStorage("def", options="contexts='yes'")

  Class of hashed Storage for a particular type of hash (typically
  hash-type is "memory" or "bdb") and any other options.
  """
  def __init__(self, hash_name, options=""):
    Storage.__init__(self, name = hash_name, storage_name = "hashes",
                     options_string = options)


class MemoryStorage(Storage):
  """Redland memory Storage class

     import RDF
     h1=RDF.MemoryStorage()
     h1=RDF.MemoryStorage("abc")
     h2=RDF.MemoryStorage("abc", "write='no'")

     # Creating a storage with contexts enabled
     s = RDF.MemoryStorage(options_string="contexts='yes'")

  Class of memory Storage with optional name, additional options.
  """
  def __init__(self, mem_name = "", options_string = ""):
    Storage.__init__(self, name = mem_name, storage_name = "memory",
                     options_string = options_string)


class FileStorage(Storage):
  """Redland file Storage class

     import RDF
     s=RDF.FileStorage("abc")

  Class of file Storage with required name, additional options.
  """
  def __init__(self, mem_name, options_string = ""):
    Storage.__init__(self, name = mem_name, storage_name = "file",
                     options_string = options_string)


class Uri(object):
  """Redland URI Class

  import RDF
  uri1 = RDF.Uri("http://example.com/")
  uri2 = RDF.Uri(uri1)

  """
  
  def __init__(self, arg = None, **args):
    """Create an RDF URI (constructor).

Creates a new RDF URI from either of the following forms:

  uri1 = RDF.Uri("http://example.com/")
  
Create a URI from the given string.

  uri2 = RDF.Uri(uri1)

Copy an existing URI uri1.
    """
    global _world
    global _debug    
    if _debug:
      print "Creating RDF.Uri arg,args=",arg,args
    self._reduri=None

    if arg is not None:
      if isinstance(arg, str):
        args['string'] = arg
      elif isinstance(arg, unicode):
        import Redland_python
        args['string'] = Redland_python.unicode_to_bytes(arg)
      elif isinstance(arg, Uri):
        args['uri'] = arg

    if args.has_key('string') and args['string'] is not None:
      self._reduri=Redland.librdf_new_uri(_world._world, args['string'])
    elif args.has_key('uri'):
      self._reduri=Redland.librdf_new_uri_from_uri(args['uri']._reduri)
    elif args.has_key('from_object'):
      if args['from_object']!=None:
        self._reduri=Redland.librdf_new_uri_from_uri(args['from_object'])
      else:
        raise RedlandError("Attempt to create new URI from null URI")

    if self._reduri is None:
      raise RedlandError("Uri construction failed")

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

  def __unicode__(self):
    """Get a Unicode string representation of an RDF URI."""
    return unicode(Redland.librdf_uri_to_string(self._reduri), 'utf-8')

  def __hash__(self):
    return hash(str(self))
  
  def __eq__(self,other):
    """Equality of RDF URI to another RDF URI."""
    if not isinstance(other, self.__class__):
      return False
    return (Redland.librdf_uri_equals(self._reduri, other._reduri) != 0)

  def __ne__(self,other):
    """Inequality of RDF URI to another RDF URI."""
    return not self == other

# end class Uri

class Parser(object):
  """Redland Syntax Parser Class

  import RDF
  parser1=RDF.Parser()
  parser2=RDF.Parser(name="rdfxml")
  parser3=RDF.Parser(mime_type="application/rdf+xml")

  stream=parser2.parse_as_stream("file://dir/file.rdf")
  parser3.parse_into_model(model, "file://dir/file.rdf", "http://example.org/")

  The default parser type if not given explicitly is raptor,
  for the RDF/XML syntax.
  """
  
  def __init__(self, name=None, mime_type=None, uri=None):
    """Create an RDF Parser (constructor).

Create a new RDF Parser for a particular syntax.  The parser is
chosen by the fields given to the constructor, all of which are
optional.  When any are given, they must all match.  The default
parser is chosen if none are given, which is RDF/XML in the
standard configuration of Raptor.

  name      - parser syntax name
  mime_type - syntax mime type
  uri       - URI identifying the syntax
    """
    #"
    global _world
    global _debug    
    if _debug:
      print "Creating RDF.Parser name=",name,"mime_type=",mime_type, "uri=",uri

    self._parser = None

    if uri is not None:
      ruri=uri._reduri
    else:
      ruri=None

    self._parser=Redland.librdf_new_parser(_world._world, name, mime_type, ruri)
    if self._parser is None:
      raise RedlandError("Parser construction failed")

  def __del__(self):
    global _debug    
    if _debug:
      print "Destroying RDF.Parser"
    if self._parser:
      Redland.librdf_free_parser(self._parser)

  def parse_as_stream(self, uri, base_uri=None):
    """Return a Stream of Statements from parsing the content at
        a URI for the optional base URI or None if the parsing fails.

        (This depends on what URI support raptor provides to redland)
        
          for statement in parser.parse_as_stream("http://localhost/r.rdf"):
              print statement
    """
    if isinstance(uri, str):
      uri = Uri(string=uri)
    elif isinstance(uri, unicode):
      import Redland_python
      uri = Uri(string=Redland_python.unicode_to_bytes(uri))
    if base_uri is None:
      base_uri=uri
    my_stream=Redland.librdf_parser_parse_as_stream(self._parser,
        uri._reduri, base_uri._reduri)
    if my_stream is None:
      return None
    return Stream(my_stream, self)

  def parse_string_as_stream(self, string, base_uri):
    """Return a Stream of Statements from parsing the content in
        string with the required base URI or None if the parsing fails.

          for statement in parser.parse_string_as_stream(rdfstring, base_uri):
              print statement
    """
    if isinstance(base_uri, str):
      base_uri = Uri(string=base_uri)
    elif isinstance(base_uri, unicode):
      import Redland_python
      base_uri = Uri(string=Redland_python.unicode_to_bytes(base_uri))
    if base_uri is None:
      raise RedlandError("A base URI is required when parsing a string")

    if isinstance(string, unicode):
      import Redland_python
      string=Redland_python.unicode_to_bytes(string)

    my_stream=Redland.librdf_parser_parse_string_as_stream(self._parser,
      string, base_uri._reduri)
    if my_stream==None:
      return None
    return Stream(my_stream, self)

  def parse_into_model(self, model, uri, base_uri=None, handler=None):
    """Parse into the Model model from the content at the URI, for
       the optional base URI.  Returns a boolean success or failure.
        
       If handler is given, an error handler with the signature
         def handler(code, level, facility, message, line, column, byte, file, uri)
       is called.

       parser.parse_into_model(model, "file:./foo.rdf",
                               "http://example.com/foo.rdf")
    """

    if isinstance(uri, str):
      uri = Uri(string = uri)
    elif isinstance(uri, unicode):
      import Redland_python
      uri = Uri(string=Redland_python.unicode_to_bytes(uri))
    if isinstance(base_uri, str):
      base_uri = Uri(string = base_uri)
    elif isinstance(base_uri, unicode):
      import Redland_python
      base_uri = Uri(string=Redland_python.unicode_to_bytes(base_uri))
    if base_uri is None:
      base_uri = uri

    if handler is not None:
      import Redland_python
      Redland_python.set_callback(handler)

    try:
      rc = Redland.librdf_parser_parse_into_model(self._parser,
        uri._reduri, base_uri._reduri, model._model)
    except RedlandError, err:
      print "Caught error",err
      raise err

    if handler is not None:
      import Redland_python
      Redland_python.reset_callback()

    return (rc != None)

  def parse_string_into_model(self, model, string, base_uri, handler = None):
    """Parse into the Model model from the content string
        with the required base URI.  Returns a boolean success or failure.

       If handler is given, an error handler with the signature
         def handler(code, level, facility, message, line, column, byte, file, uri)
       is called.

    """
    if isinstance(base_uri, str):
      base_uri = Uri(string = base_uri)
    elif isinstance(base_uri, unicode):
      import Redland_python
      base_uri = Uri(string=Redland_python.unicode_to_bytes(base_uri))
    if base_uri is None:
      raise RedlandError("A base URI is required when parsing a string")

    if handler is not None:
      import Redland_python
      Redland_python.set_callback(handler)

    if isinstance(string, unicode):
      import Redland_python
      string=Redland_python.unicode_to_bytes(string)

    rc=Redland.librdf_parser_parse_string_into_model(self._parser, string,
                                           base_uri._reduri, model._model)

    if handler is not None:
      import Redland_python
      Redland_python.reset_callback()

    return (rc != None)

  def get_feature(self, uri):
    """Return the Node value of Parser feature URI uri"""
    if not isinstance(uri, Uri):
      uri=Uri(string=uri)

    value=Redland.librdf_parser_get_feature(self._parser,uri._reduri)
    if value == "NULL" or value == None:
      return None
    return Node(from_object=value)

  def set_feature(self, uri, value):
    """Set the Node value of Parser feature URI uri."""
    if not isinstance(uri, Uri):
      uri=Uri(string=uri)
    if not isinstance(value, Node):
      value=Node(literal=value)
    Redland.librdf_parser_set_feature(self._parser,uri._reduri,value._node)

  def set_uri_filter(self, filter):
    """Set a function for rejecting retrieval of URIs seen during parsing.

   Return False or 0 to not reject the URI

   def reject(uri):
     if "bad" == uri:
       return True
     return False

   parser.set_uri_filter(reject)
"""
    import Redland_python
    Redland_python.set_parser_uri_filter(self._parser, filter)

  def namespaces_seen(self):
    """Get a dictionary of prefix/URI pairs for namespaces seen during parsing.
"""
    count = Redland.librdf_parser_get_namespaces_seen_count(self._parser)
    nspaces={}
    for index in range(0, count-1):
      prefix=Redland.librdf_parser_get_namespaces_seen_prefix(self._parser, index)
      uri_obj=Redland.librdf_parser_get_namespaces_seen_uri(self._parser, index)
      if uri_obj is None:
        uri=None
      else:
        uri=Uri(from_object=uri_obj)
      nspaces[prefix]=uri
    return nspaces

# end class Parser

class RDFXMLParser(Parser):
  """Redland RDF/XML Parser class

     import RDF
     parser=RDF.RDFXMLParser()
  """
  def __init__(self):
    Parser.__init__(self, name = "rdfxml")

class NTriplesParser(Parser):
  """Redland N-Triples Parser class

     import RDF
     parser=RDF.NTriplesParser()
  """
  def __init__(self):
    Parser.__init__(self, name = "ntriples")

class TurtleParser(Parser):
  """Redland Turtle Parser class

     import RDF
     parser=RDF.TurtleParser()
  """
  def __init__(self):
    Parser.__init__(self, name = "turtle")

class RSSTagSoupParser(Parser):
  """Redland RSS Tag Soup Parser class

     import RDF
     parser=RDF.RSSTagSoupParser()
  """
  def __init__(self):
    Parser.__init__(self, name = "rss-tag-soup")

class GRDDLParser(Parser):
  """Redland GRDDL Parser class

     import RDF
     parser=RDF.GRDDLParser()
  """
  def __init__(self):
    Parser.__init__(self, name = "grddl")


class Query(object):
  """Redland Query interface class

  import RDF

  q1 = RDF.Query("SELECT ?a ?c WHERE (?a dc:title ?c) USING dc FOR <http://purl.org/dc/elements/1.1/>")
  q2 = RDF.Query("- - -", query_language="triples")
  q3 = RDF.Query("select $a where ...", query_language="sparql")

  results=q1.execute(model)
  for result in results:
    print result['a']
    print result['c']

  for statement in q2.execute().as_stream(model):
    print statement

  """
  def __init__(self, querystring, base_uri=None, query_language="sparql", query_uri=None):
    self._query = None

    if querystring is None:
      raise RedlandError("No query string given")

    if isinstance(querystring, unicode): 
      querystring = querystring.encode('utf-8')

    if query_uri is not None:
      ruri = query_uri._reduri
    else:
      ruri = None

    if base_uri is not None:
      rbase_uri = base_uri._reduri
    else:
      rbase_uri = None

    global _world
    global _debug
    if _debug:
      print "Creating query for language '"+query_language+"', base '"+str(rbase_uri)+"': "+querystring

    self._query = Redland.librdf_new_query(_world._world, query_language, ruri, querystring, rbase_uri)
    self.result_stream = None

    if self._query is None:
      raise RedlandError("Query construction failed")

  def __del__(self):
    global _debug    
    if _debug:
      print "Destroying RDF.Query"
    if self._query:
      Redland.librdf_free_query(self._query)

  def execute(self,model):
    results = Redland.librdf_query_execute(self._query,model._model)
    if results is not None:
      return QueryResults(self._query,results)
    else:
      return None

# end class Query


class RDQLQuery(Query):
  """Redland RDQL Query class"""
  def __init__(self, querystring, base_uri=None):
    Query.__init__(self, querystring = querystring, base_uri = base_uri,
                   query_language = "rdql")

class SPARQLQuery(Query):
  """Redland SPARQL Query class"""
  def __init__(self, querystring, base_uri=None):
    Query.__init__(self, querystring = querystring, base_uri = base_uri,
                   query_language = "sparql")


class QueryResults(object):
  """Redland Query results class


  """
  def __init__(self,query,results):
    global _debug
    if _debug:
      print "Creating QueryResults"
    self._query = query
    self._results = results
    self.first = True

  def is_bindings(self):
    """Test if the query results format is variable bindings"""
    return (Redland.librdf_query_results_is_bindings(self._results) != 0)

  def is_boolean(self):
    """Test if the query results format is a boolean"""
    return (Redland.librdf_query_results_is_boolean(self._results) != 0)

  def is_graph(self):
    """Test if the query results format is an RDF graph"""
    return (Redland.librdf_query_results_is_graph(self._results) != 0)

  def __iter__(self):
    return self

  def __len__(self):
    """Returns an exception since len() of an iterable is undefined."""
    raise ValueError("Cannot take the length of iterable query results")

  # Iterator method
  def next(self):
    """Get the next variable binding result"""
    if not self.is_bindings():
      raise RedlandError("Query result is not in variable bindings format")
    if self.first:
      self.first = False
    else:
      Redland.librdf_query_results_next(self._results)
    if Redland.librdf_query_results_finished(self._results):
      raise StopIteration
    return self.make_results_hash()

  def make_results_hash(self):
    results = {}
    c = Redland.librdf_query_results_get_bindings_count(self._results)
    for i in range(c):
      n = Redland.librdf_query_results_get_binding_name(self._results,i)
      v = Redland.librdf_query_results_get_binding_value(self._results,i)
      if v is None:
        results[n] = None
      else:
        results[n] = Node(from_object=v)

    return results

  def finished(self):
    """Test if reached the last variable binding result"""
    if not self.is_bindings():
      raise RedlandError("Query result is not in variable bindings format")
    return (Redland.librdf_query_results_finished(self._results) != 0)

  def as_stream(self):
    """Return the query results as a stream of triples (RDF.Statement)"""
    if not self.is_graph():
      raise RedlandError("Query result is not in RDF graph format")
    s=Redland.librdf_query_results_as_stream(self._results)
    if s is not None:
      return Stream(s, self)
    else:
      return None

  def get_boolean(self):
    """Get the boolean query result"""
    if not self.is_boolean():
      raise RedlandError("Query result is not in boolean format")
    return (Redland.librdf_query_results_get_boolean(self._results) != 0)

  def get_binding_value(self, offset):
    """Get the value of a variable binding by offset"""
    if not self.is_bindings():
      raise RedlandError("Query result is not in variable bindings format")
    n=Redland.librdf_query_results_get_binding_value(self._results, offset)
    if n is None:
      return None
    else:
      return Node(from_object=n, do_not_copy=1)

  def get_binding_name(self, offset):
    """Get the name of a variable binding by offset"""
    if not self.is_bindings():
      raise RedlandError("Query result is not in variable bindings format")
    return Redland.librdf_query_results_get_binding_name(self._results, offset)

  def get_binding_value_by_name(self, name):
    """Get the value of a variable binding by variable name"""
    if not self.is_bindings():
      raise RedlandError("Query result is not in variable bindings format")
    n=Redland.librdf_query_results_get_binding_value_by_name(self._results, name)
    if n is None:
      return None;
    else:
      return Node(from_object=n, do_not_copy=1)

  def get_bindings_count(self):
    """Get the number of variable bindings in the query result"""
    if not self.is_bindings():
      raise RedlandError("Query result is not in variable bindings format")
    return Redland.librdf_query_results_get_bindings_count(self._results)

  def __del__(self):
    global _debug    
    if _debug:
      print "Destroying RDF.QueryResults"""
    if self._results:
      Redland.librdf_free_query_results(self._results)

  def to_file(self, name, format_uri=None, base_uri=None):
    """Serialize to filename name in syntax format_uri using the optional base URI."""
    if isinstance(format_uri, str):
      format_uri = Uri(string = format_uri)
    elif isinstance(format_uri, unicode):
      import Redland_python
      format_uri = Uri(string=Redland_python.unicode_to_bytes(format_uri))
    else:
      format_uri = None

    if format_uri is not None:
      rformat_uri = format_uri._reduri
    else:
      rformat_uri = None

    if isinstance(base_uri, str):
      base_uri = Uri(string = base_uri)
    elif isinstance(base_uri, unicode):
      import Redland_python
      base_uri = Uri(string=Redland_python.unicode_to_bytes(base_uri))
    if base_uri is not None:
      rbase_uri = base_uri._reduri
    else:
      rbase_uri = None
    return Redland.librdf_query_results_to_file(self._results, name, rformat_uri, rbase_uri)

  def to_string(self, format_uri=None, base_uri=None):
    """Serialize to a string syntax format_uri using the optional base URI."""
    if self.is_graph():
      tmpmodel = Model(MemoryStorage())
      tmpmodel.add_statements(self.as_stream())
      serializer = Serializer()
      return serializer.serialize_model_to_string(tmpmodel, base_uri)

    if self.is_boolean():
      return str(self.get_boolean())

    if not self.is_bindings():
      raise RedlandError("Unknown query result format cannot be written as a string")
    
    if isinstance(format_uri, str):
      format_uri = Uri(string = format_uri)
    elif isinstance(format_uri, unicode):
      import Redland_python
      format_uri = Uri(string=Redland_python.unicode_to_bytes(format_uri))
    else:
      format_uri = None

    if format_uri is not None:
      rformat_uri = format_uri._reduri
    else:
      rformat_uri = None
    if isinstance(base_uri, str):
      base_uri = Uri(string = base_uri)
    elif isinstance(base_uri, unicode):
      import Redland_python
      base_uri = Uri(string=Redland_python.unicode_to_bytes(base_uri))
    if base_uri is not None:
      rbase_uri = base_uri._reduri
    else:
      rbase_uri = None

    return Redland.librdf_query_results_to_string(self._results,
                                                  rformat_uri, rbase_uri)

  def __str__(self):
    """Serialize to string syntax format."""
    return self.to_string()


# end class QueryResults


class Serializer(object):
  """ Redland Syntax Serializer Class

  import RDF
  ser1=RDF.Serializer()

  ser2=RDF.Serializer(mime_type="application/rdf+xml")

  ser3=RDF.Serializer(name="ntriples")

  A class for turning a Model into a syntax serialization (at present
  only to local files).
  """
  
  def __init__(self, name=None, mime_type=None, uri=None):
    """Create an RDF Serializer (constructor).

    The arguments name, mime_type and uri are all optional and
    when omitted the default serialization syntax is used.  If
    any arguments are given, they must all match for an appropriate
    syntax to be chosen.  For example, RDF/XML has a MIME type of
    'application/rdf+xml' so this can be given with the mime_type
    argument, however the N-Triples has none, so the 'ntriples' name
    must be used.  Most syntaxes have URIs.
    """
    global _world
    global _debug    
    if _debug:
      print "Creating RDF.Serializer name=",name,"mime_type=",mime_type, \
        "uri=",uri

    self._serializer = None

    if uri is not None:
      ruri = uri._reduri
    else:
      ruri = None

    self._serializer=Redland.librdf_new_serializer(_world._world, name,
      mime_type, ruri)
    if self._serializer is None:
      raise RedlandError("Serializer construction failed")

  def __del__(self):
    global _debug    
    if _debug:
      print "Destroying RDF.Serializer"
    if self._serializer:
      Redland.librdf_free_serializer(self._serializer)

  def serialize_model_to_file(self, name, model, base_uri=None):
    """Serialize to filename name the Model model using the
       optional base URI."""
    if isinstance(base_uri, str):
      base_uri = Uri(string = base_uri)
    elif isinstance(base_uri, unicode):
      import Redland_python
      base_uri = Uri(string=Redland_python.unicode_to_bytes(base_uri))
    if base_uri is not None:
      rbase_uri = base_uri._reduri
    else:
      rbase_uri = None
    return Redland.librdf_serializer_serialize_model_to_file(self._serializer,
      name, rbase_uri, model._model)

  def serialize_model_to_string(self, model, base_uri=None):
    """Serialize to a string using the optional base URI."""
    if isinstance(base_uri, str):
      base_uri = Uri(string = base_uri)
    elif isinstance(base_uri, unicode):
      import Redland_python
      base_uri = Uri(string=Redland_python.unicode_to_bytes(base_uri))
    if base_uri is not None:
      rbase_uri = base_uri._reduri
    else:
      rbase_uri = None
    return Redland.librdf_serializer_serialize_model_to_string(self._serializer, rbase_uri, model._model)
  
  def serialize_stream_to_string(self, stream, base_uri=None):
    """Serialize a stream to a string using the optional base URI."""
    if isinstance(base_uri, str):
      base_uri = Uri(string = base_uri)
    elif isinstance(base_uri, unicode):
      import Redland_python
      base_uri = Uri(string=Redland_python.unicode_to_bytes(base_uri))
    if base_uri is not None:
      rbase_uri = base_uri._reduri
    else:
      rbase_uri = None
    return Redland.librdf_serializer_serialize_stream_to_string(self._serializer, rbase_uri, stream._stream)
  
  # TODO: features could usefully be implemented as a collection

  def get_feature(self, uri):
    """Return the value of Serializer feature URI uri"""
    if not isinstance(uri, Uri):
      if isinstance(uri, str):
        uri = Uri(string=uri)
      elif isinstance(uri, unicode):
        import Redland_python
        uri = Uri(string=Redland_python.unicode_to_bytes(uri))
      else:
        raise TypeError("uri must be string or RDF.Uri")

    return Redland.librdf_serializer_get_feature(self._serializer,uri._reduri)

  def set_feature(self, uri, value):
    """Set the value of Serializer feature URI uri."""
    if not isinstance(uri, Uri):
      if isinstance(uri, str):
        uri = Uri(string=uri)
      elif isinstance(uri, unicode):
        import Redland_python
        uri = Uri(string=Redland_python.unicode_to_bytes(uri))
      else:
        raise TypeError("uri must be string or RDF.Uri")
    Redland.librdf_serializer_set_feature(self._serializer,uri._reduri,value)

  def set_namespace(self, prefix, uri):
    """Set a namespace prefix and URI for the Serializer to use."""
    if not isinstance(uri, Uri):
      if isinstance(uri, str):
        uri = Uri(string=uri)
      elif isinstance(uri, unicode):
        import Redland_python
        uri = Uri(string=Redland_python.unicode_to_bytes(uri))
      else:
        raise TypeError("uri must be string or RDF.Uri")
    Redland.librdf_serializer_set_namespace(self._serializer, uri._reduri, prefix)


# end class Serializer


class NTriplesSerializer(Serializer):
  """Redland N-Triples Serializer class

     import RDF
     ser=RDF.NTriplesSerializer()
  """
  def __init__(self):
    Serializer.__init__(self, name = "ntriples", mime_type = "", uri = None)


class RDFXMLSerializer(Serializer):
  """Redland RDF/XML Serializer class

     import RDF
     ser=RDF.RDFXMLSerializer()
  """
  def __init__(self):
    Serializer.__init__(self, name = "rdfxml")

class RDFXMLAbbrevSerializer(Serializer):
  """Redland RDF/XML with abbreviations Serializer class

     import RDF
     ser=RDF.RDFXMLAbbrevSerializer()
  """
  def __init__(self):
    Serializer.__init__(self, name = "rdfxml-abbrev")

class RSS10Serializer(Serializer):
  """Redland RSS 1.0 Serializer class

     import RDF
     ser=RDF.RSS10Serializer()
  """
  def __init__(self):
    Serializer.__init__(self, name = "rss-1.0")

class TurtleSerializer(Serializer):
  """Redland Turtle Serializer class

     import RDF
     ser=RDF.TurtleSerializer()
  """
  def __init__(self):
    Serializer.__init__(self, name = "turtle")


class NS(object):
  """ Redland Namespace Utility Class

  import RDF
  nspace = RDF.NS("http://example.com/foo#")

  # creates an RDF Node for http://example.com/foo#blah   
  node1 = nspace.blah

  # creates an RDF Node for http://example.com/foo#blah   
  node2 = nspace['blah']

  A class for generating RDF Nodes with URIs from the same vocabulary
  (such as XML Namespace) varying only in the appended name in
  the vocabulary.  Each node returned is a pointer to a shared copy.

  """

  def __init__(self,prefix):
    self._prefix = prefix
    self._nodecache = {}

  def _node(self,localName):
    if localName not in self._nodecache:
      self._nodecache[localName] = Redland.librdf_new_node_from_uri_string(_world._world, self._prefix+localName)
    return Node(from_object=self._nodecache[localName])

  def __getitem__(self,localName):
    return self._node(localName)

  def __getattr__(self,localName):
    return self._node(localName)


# global init, create our world.

_world=World()
