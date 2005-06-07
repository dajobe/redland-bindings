require 'rdf/redland'
require 'rdf/redland/uri'

module Redland
  
  class Node
    attr_reader :node
    
    @@node_types = {0 =>'NODE_TYPE_UNKNOWN',
                    1 =>'NODE_TYPE_RESOURCE',
                    2 =>'NODE_TYPE_LITERAL',
                    4 =>'NODE_TYPE_BLANK'}

    # Create an RDF Node
    # 
    # Resource or property node creation
    #   n1 = Node.new(Uri.new("http://example.com/foo"))
    #
    # String literal node creation
    #   n2 = Node.new("foo")
    def initialize(arg=nil)
      if arg.class == String
        temp = arg
        arg = {}
        arg[:literal]= temp
        @node = self.node_from_hash(arg)
      elsif arg.class == Hash
        @node = node_from_hash(arg)
      else
        @node = Node.ensure(arg)
      end
      raise RedlandError.new("Node construction failed") if not @node
      ObjectSpace.define_finalizer(self,Node.create_finalizer(@node))
    end

    # sets this node's value using a blank or URI extracted from a Hash
    # (?? confirm)
    def node_from_hash(hash)
      h = {:blank,:uri_string}
      if hash.key?(:blank)
        node = Redland.librdf_new_node_from_blank_identifier($world.world,hash[:blank])
      end
      if hash.key?(:from_object)
        if hash.key?(:do_not_copy)
          node = hash[:from_object]
        else
          node = Redland.librdf_new_node_from_node(hash[:from_object])
        end
      end
      
      if hash.key?(:uri_string)
        node = Redland.librdf_new_node_from_uri_string($world.world,hash[:uri_string])
      end
      if hash.key?(:literal)
        node = node_from_literal(hash)
      end
      return node
    end

    # sets this node's value using a literal value extracted from a Hash
    # (?? confirm)
    def node_from_literal(hash)
      xml_language = hash.key?(:xml_language) ? hash[:xml_language] : ""
      wf_xml = hash.key?(:wf_xml?) ? 1 : 0
      if hash.key?(:datatype)
        datatype = hash[:datatype]
        node = Redland.librdf_new_node_from_typed_literal($world.world,hash[:literal],xml_language,datatype.uri)
      else
        node = Redland.librdf_new_node_from_literal($world.world,hash[:literal],xml_language,wf_xml)
      end
      return node
    end

    # Sets this node's value to a blank node with the specified ID (?? confirm)
    def Node.bnode(id=nil)
      @node = Redland.librdf_new_node_from_blank_identifier($world.world,id)
    end

    # You shouldn't use this. Used internally for cleanup.
    def Node.create_finalizer(node)
      proc {|id| Redland::librdf_free_node(node) }
    end

    # Creates a new blank node with the specified ID (?? confirm)
    def Node.anon(id)
      self.initialize()
      super(id)
      @node = Redland.librdf_new_node_from_blank_identifier($world.world,id)
    end

    # Return true if node is a literal
    def literal?
      return (Redland.librdf_node_is_literal(self.node) !=0)
    end

    # Return true if node is a resource with a URI
    def resource?
      return (Redland.librdf_node_is_resource(self.node) !=0)
    end

    # Return true if node is a blank node
    def blank?
      return (Redland.librdf_node_is_blank(self.node) !=0)
    end

    # Equivalency. Only works for comparing two Nodes
    def ==(other)
      return (Redland.librdf_node_equals(self.node,other.node) != 0)
    end

    # Convert this to a string
    def to_s
      return Redland.librdf_node_to_string(self.node)
    end

    # Get the type of this node as a string (Node.node_types)
    def node_type()
      return @@node_types[Redland.librdf_node_get_type(self.node)]
    end
    
    # return a copy of the internal uri
    def uri
      if self.resource?
        return Uri.new(Redland.librdf_node_get_uri(self.node))
      else
        raise NodeTypeError.new("Can't get URI for node type %s" % self.node_type() )
      end
    end

    # returns a Literal if the node is a literal type 
    def literal
      unless self.literal?
        raise NodeTypeError.new("Can't get literal value for node type %s" % self.node_type)
      end
      return Literal.from_node(self.node)
    end

    # returns the blank identifier for this node if the internal model is
    # a blank node
    def blank_identifier()
      unless self.blank?
        raise NodeTypeError.new("Can't get blank identifier for node type %s" % self.node_type)
      else
        return Redland.librdf_node_get_blank_identifier(self.node)
      end
    end

    # Ensures that the argument is a node by constructing one or returning nil
    # (?? confirm)
    def Node.ensure(node)
      case node
      when Node
        my_node = Redland.librdf_new_node_from_node(node.node)
      when SWIG::TYPE_p_librdf_node_s
        my_node = Redland.librdf_new_node_from_node(node)
      when Uri
        my_node = Redland.librdf_new_node_from_uri($world.world,node.uri)
      when URI
        my_node = Redland.librdf_new_node_from_uri_string($world.world,node.to_s)
      else
        my_node = nil
      end
      return my_node
    end

  end

  # A class to make Blank Node creation easier
  #  model = Model.new
  #  dom = BNode.new('dom') # create with optional id
  #
  # I will probably depreciate this and just use Resource.new()
  # to generate blank nodes.
  class BNode < Node

    def initialize(id=nil)
      @node = Redland.librdf_new_node_from_blank_identifier($world.world,id)
      if not @node then raise RedlandError.new("Node construction failed")end
      ObjectSpace.define_finalizer(self,Node.create_finalizer(@node))
    end
    
  end

  # A helper class for namespace.  Caches the nodes for a given namespaces
  #  foaf = Namespace.new('http://xmlns.com/0.1/foaf/')
  #  model.find(nil,foaf['name'],'Dominic')
  #  puts foaf['name'] => http://xmlns.com/0.1/foaf/name
  class Namespace < Node

    def initialize(namespace)
      super(:uri_string=>namespace)
      @nodecache = {}
    end

    def [](item)
      return the_node(item)
    end

    def the_node(localName)
      if not @nodecache.member? localName
        @nodecache[localName] = Node.new(:uri_string=>self.uri.to_s + localName)
      end
      return @nodecache[localName]
    end

    private :the_node
    
  end

  # A literal node
  class Literal < Node
    include Redland
    
    # the string value of the literal
    attr_reader :value
    
    # the language of the literal
    attr_reader :language

    # create a new Literal node
    #  literal = Literal.new('Dominic')
    #  label = Literal.new('Name','en')
    def initialize(str,lang=nil,uri=nil,is_xml=false)
      @value = str
      @language = lang
      is_xml = is_xml==true ? 1 : 0
      if uri
        @node = Redland.librdf_new_node_from_typed_literal($world.world,value,lang,uri.uri)
      else
        @node = Redland.librdf_new_node_from_literal($world.world,value,lang,is_xml)
      end

      raise RedlandError.new("Node construction failed") if not @node
      ObjectSpace.define_finalizer(self,Node.create_finalizer(@node))
    end

    # create a literal from another Node
    def Literal.from_node(node)
      lang = Redland.librdf_node_get_literal_value_language(node) if Redland.librdf_node_get_literal_value_language(node)
      str = Redland.librdf_node_get_literal_value(node)
      hash_uri = Redland.librdf_node_get_literal_value_datatype_uri(node)
      hash_uri = Uri(Redland.librdf_uri_to_string(hash_uri)) if hash_uri
      return Literal.new(str,lang,hash_uri)
    end

    # create a literal from and xml string
    #  literal = Literal.from_xml('<em>This is emphasized</em>')
    def Literal.from_xml(str,lang=nil)
      return Literal.new(str,lang,nil,true)
    end
  end
end #Redland

