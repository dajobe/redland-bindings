require 'rdf/redland'

module Redland

# Redland Statement (triple class).  The main means of manipulating
# statements is by the subject,predicate,and object properties
#
#   require 'rdf'
#   Statement creation from nodes
#   statement1 = Statement.new(node1,node2,node3)
#   statement2 = Statement.new(:statement=>statement)
#
#   if statement2.resource?
#     puts "statement2.subject has uri #{statement2.subject.uri}"
#
  class Statement
    include Util

    # the internal librdf statement
    attr_reader :statement

    # create a new Statement with subject,predicate,and object
    # the subject, predicate, and objects can be of class String or
    # Uri.
    def initialize(subject=nil,predicate=nil,object=nil,model=nil)
      @model = model
      
      if subject.class == Hash
        if subject.key?(:from_object)
          @statement = Redland.librdf_new_statement_from_statement(subject[:from_object])
          @model = subject[:model]
        end
      else
        
        unless subject
          s = nil
        else
          if (subject.class == Uri)|| (subject.class == String)
            subject = Node.new(subject)
          end
          s = Redland.librdf_new_node_from_node(subject.node)
        end
        
        unless predicate
          p = nil
        else    
          if (predicate.class == Uri) || (predicate.class == String)
            predicate = Node.new(predicate)
          end
          p = Redland.librdf_new_node_from_node(predicate.node)
        end

        unless object
          o = nil
        else    
          if (object.class == Uri) || (object.class == String)
            object = Node.new(object)
          end
          o = Redland.librdf_new_node_from_node(object.node)
        end
        @statement = Redland.librdf_new_statement_from_nodes($world.world,s,p,o)
      end

      raise RedlandError.new("Statement construction failed") if !@statement
      ObjectSpace.define_finalizer(self,Statement.create_finalizer(@statement))

    end

    # You shouldn't use this. Used internally for cleanup.
    def Statement.create_finalizer(statement)
      proc {|id| Redland::librdf_free_statement(statement) }
    end

    def wrap_node(rednode)
      return Node.new(:from_object=>rednode)
    end

    private :wrap_node

    # return the subject of the statement triple. The subject is of class
    # Resource
    def subject()
      return Resource.new(Redland.librdf_statement_get_subject(self.statement),@model)
    end

    # return the predicate of the statement triple.
    def predicate()
      return wrap_node(Redland.librdf_statement_get_predicate(self.statement))
    end

    # return the object of the statement triple. The return value is either
    # a Literal or Resource depending if object returned from the store
    # is literal or not
    def object()
      object_node = Redland.librdf_statement_get_object(self.statement)
      return Literal.from_node(object_node) if is_literal?(object_node)
      return Resource.new(object_node,@model)
    end

    # set the subject of the statement
    def subject=(value)
      if value
        Redland.librdf_statement_set_subject(self.statement,
                                             Redland.librdf_new_node_from_node(value.node))
      else
        Redland.librdf_statement_set_subject(self.statement, nil)
      end
    end

    # set the object of the statement
    def object=(value)
      if value
        Redland.librdf_statement_set_object(self.statement,
                                            Redland.librdf_new_node_from_node(value.node))
      else
        Redland.librdf_statement_set_object(self.statement, nil)
      end
    end

    # set the predicate of the statement
    def predicate=(value)
      if value
        Redland.librdf_statement_set_predicate(self.statement,
                                               Redland.librdf_new_node_from_node(value.node))
      else
        Redland.librdf_statement_set_predicate(self.statement, nil)
      end
    end

    # Convert this Statement to a string
    def to_s()
      unless self.statement
        raise RedlandError.new("Statement is invalid")
      else
        return Redland.librdf_statement_to_string(self.statement)
      end
    end
  end

end #module Redland
